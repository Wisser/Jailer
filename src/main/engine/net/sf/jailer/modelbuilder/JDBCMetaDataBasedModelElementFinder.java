/*
 * Copyright 2007 - 2023 Ralf Wisser.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.sf.jailer.modelbuilder;

import java.io.File;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.Session;
import net.sf.jailer.database.Session.ResultSetReader;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Cardinality;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.PrimaryKey;
import net.sf.jailer.datamodel.PrimaryKeyFactory;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.CancellationHandler;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.CsvFile.Line;
import net.sf.jailer.util.JSqlParserUtil;
import net.sf.jailer.util.LogUtil;
import net.sf.jailer.util.Pair;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlUtil;
import net.sf.jsqlparser.expression.Expression;
import net.sf.jsqlparser.statement.StatementVisitorAdapter;
import net.sf.jsqlparser.statement.select.AllColumns;
import net.sf.jsqlparser.statement.select.AllTableColumns;
import net.sf.jsqlparser.statement.select.FromItemVisitor;
import net.sf.jsqlparser.statement.select.LateralSubSelect;
import net.sf.jsqlparser.statement.select.ParenthesisFromItem;
import net.sf.jsqlparser.statement.select.PlainSelect;
import net.sf.jsqlparser.statement.select.Select;
import net.sf.jsqlparser.statement.select.SelectExpressionItem;
import net.sf.jsqlparser.statement.select.SelectItem;
import net.sf.jsqlparser.statement.select.SelectItemVisitor;
import net.sf.jsqlparser.statement.select.SelectVisitorAdapter;
import net.sf.jsqlparser.statement.select.SetOperationList;
import net.sf.jsqlparser.statement.select.SubJoin;
import net.sf.jsqlparser.statement.select.SubSelect;
import net.sf.jsqlparser.statement.select.TableFunction;
import net.sf.jsqlparser.statement.select.ValuesList;
import net.sf.jsqlparser.statement.select.WithItem;
import net.sf.jsqlparser.statement.upsert.Upsert;

/**
 * Finds associations and tables by analyzing the JDBC meta data.
 *
 * @author Ralf Wisser
 */
public class JDBCMetaDataBasedModelElementFinder implements ModelElementFinder {

	/**
	 * The logger.
	 */
	private static final Logger _log = LoggerFactory.getLogger(JDBCMetaDataBasedModelElementFinder.class);

	/**
	 * Set of sql types (uppercase) not listed in {@link Types} which needs a length argument.
	 */
	public static final Set<String> TYPES_WITH_LENGTH = new HashSet<String>();
	static {
		TYPES_WITH_LENGTH.add("NVARCHAR2");
		TYPES_WITH_LENGTH.add("NVARCHAR");
		TYPES_WITH_LENGTH.add("NCHAR");
		TYPES_WITH_LENGTH.add("RAW");
	}

	/**
	 * Set of sql types (uppercase) not listed in {@link Types} which don't accept a length argument.
	 */
	public static final Set<String> TYPES_WITHOUT_LENGTH = new HashSet<String>();
	static {
		TYPES_WITHOUT_LENGTH.add("HIERARCHYID"); // MSSQL
	}

	/**
	 * Set of the names of user defined types.
	 */
	private Set<String> userDefinedTypes = null;
	
	/**
	 * Map from element (table, column) to the comment.
	 */
	private Map<Pair<Table, Column>, String> comments = new HashMap<>();

	/**
	 * Get of the names of user defined types.
	 *
	 * @param session to retrieve the UDT names
	 * @return names of user defined types
	 */
	private Set<String> getUserDefinedTypes(Session session) {
		if (userDefinedTypes == null) {
			userDefinedTypes = new HashSet<String>();
			if (session.dbms.getUserDefinedColumnsQuery() != null) {
				try {
					String query = String.format(Locale.ENGLISH, session.dbms.getUserDefinedColumnsQuery(), Quoting.staticUnquote(session.getSchema()));
					session.executeQuery(query, new ResultSetReader() {

						@Override
						public void readCurrentRow(ResultSet resultSet) throws SQLException {
							userDefinedTypes.add(resultSet.getString(1));
						}

						@Override
						public void close() throws SQLException {
						}
					});
				} catch (SQLException e) {
					// ignore
				}
			}
		}
		return userDefinedTypes;
	}

	/**
	 * Finds associations by reading the databases meta-data.
	 *
	 * @param session the statement executor for executing SQL-statements
	 * @param dataModel model containing already known elements
	 * @param namingSuggestion to put naming suggestions for associations into
	 * @return found associations
	 */
	@Override
	public Collection<Association> findAssociations(DataModel dataModel, Map<Association, String[]> namingSuggestion, Session session, ExecutionContext executionContext) throws Exception {
		Collection<Association> associations = new ArrayList<Association>();
		Quoting quoting = Quoting.getQuoting(session);
		ResultSet resultSet;
		String defaultSchema = getDefaultSchema(session, session.getSchema());
		Set<Association> toRemove = new HashSet<Association>();

		for (Table viewOrTable: dataModel.getTables()) {
			Table table;
			Table underlyingTable = null;
			UnderlyingTableInfo uti = null;
			uti = underlyingTableInfos.get(viewOrTable.getName());
			if (uti != null) {
				underlyingTable = uti.underlyingTable;
			}
			table = viewOrTable;
			_log.info("find associations with " + table.getName());
			try {
				Table child = underlyingTable != null? underlyingTable : table;
				String ikSchema = quoting.unquote(child.getOriginalSchema(quoting.quote(defaultSchema)));
				Session ikSession = session;
				if (sessionWithPermissionToReadSchema.containsKey(Quoting.normalizeIdentifier(ikSchema))) {
					ikSession = sessionWithPermissionToReadSchema.get(Quoting.normalizeIdentifier(ikSchema));
				}
				resultSet = getImportedKeys(ikSession, ikSchema, quoting.unquote(child.getUnqualifiedName()), true);
			} catch (Exception e) {
				_log.info("failed. " + e.getMessage());
				continue;
			}
			Map<String, Association> fkMap = new HashMap<String, Association>();
			Map<String, Integer> unknownFKCounter = new HashMap<String, Integer>();
			while (resultSet.next()) {
				String qualifiedPKTableName = toQualifiedTableName(quoting.quote(defaultSchema), quoting.quote(resultSet.getString(DBMS.MySQL.equals(session.dbms)? 1 : 2)), quoting.quote(resultSet.getString(3)));
//				String qualifiedFKTableName = toQualifiedTableName(quoting.quote(defaultSchema), quoting.quote(resultSet.getString(DBMS.MySQL.equals(session.dbms)? 5 : 6)), quoting.quote(resultSet.getString(7)));
				Table defaultPkTable = dataModel.getTable(qualifiedPKTableName);

				Table fkTable = table;

				String uqFK = resultSet.getString(8);
				String fkColumn = quoting.quote(uqFK);
				if (uti != null) {
					fkColumn = uti.origColumnName.get(quoting.normalizeCase(uqFK));
					if (fkColumn == null) {
						fkColumn = uti.columnMapping.get(quoting.normalizeCase(uqFK));
					}
					if (fkColumn != null) {
						fkColumn = quoting.quote(fkColumn);
					}
				}

				// collect all PKTables
				Map<Table, UnderlyingTableInfo> infos = new LinkedHashMap<Table, UnderlyingTableInfo>();
				if (defaultPkTable != null) {
					infos.put(defaultPkTable, null);
				}
				for (Entry<String, UnderlyingTableInfo> e: underlyingTableInfos.entrySet()) {
					Table view = dataModel.getTable(e.getKey());
					if (view != null && qualifiedPKTableName.equals(e.getValue().underlyingTable.getName())) {
						infos.put(view, e.getValue());
					}
				}

				for (Entry<Table, UnderlyingTableInfo> e: infos.entrySet()) {
					Table pkTable = e.getKey();
					String pkColumn = quoting.quote(resultSet.getString(4));
					String foreignKey = resultSet.getString(12);

					UnderlyingTableInfo info = e.getValue();
					if (info != null) {
						pkColumn = info.columnMapping.get(quoting.normalizeCase(Quoting.staticUnquote(pkColumn)));
					}

					if (pkTable != null) {
						if (foreignKey == null || foreignKey.trim().length() == 0) {
							foreignKey = pkTable.getName();
							if (info != null) {
								foreignKey += "." + quoting.unquote(pkTable.getUnqualifiedName());
							}
							int seq = resultSet.getInt(9);
							String fkKey = pkTable.getName() + "." + foreignKey;
							if (seq == 1) {
								Integer count = unknownFKCounter.get(fkKey);
								if (count == null) {
									count = 1;
								} else {
									count++;
								}
								unknownFKCounter.put(fkKey, count);
							}
							foreignKey += "." + unknownFKCounter.get(fkKey);
						} else {
							if (info != null) {
								foreignKey += "." + quoting.unquote(pkTable.getUnqualifiedName());
							}
						}
						String fkName = pkTable.getName() + "." + foreignKey;

						if (foreignKey != null && fkMap.containsKey(fkName)) {
							if (fkColumn == null || pkColumn == null) {
								toRemove.add(fkMap.get(fkName));
							} else {
								fkMap.get(fkName).appendCondition("A." + fkColumn + "=B." + pkColumn);
							}
						} else {
							Association association = new Association(fkTable, pkTable, false, true, "A." + fkColumn + "=B." + pkColumn, dataModel, false, Cardinality.MANY_TO_ONE);
							association.setAuthor(session.getMetaData().getDriverName());
							associations.add(association);
							fkMap.put(fkName, association);
							if (foreignKey != null) {
								namingSuggestion.put(association, new String[] { foreignKey, fkTable.getUnqualifiedName() + "." + foreignKey });
							}
							if (fkColumn == null || pkColumn == null) {
								toRemove.add(association);
							}
						}
					}
				}
			}
			resultSet.close();
			CancellationHandler.checkForCancellation(null);
		}
		associations.removeAll(toRemove);
		shutDownSessionsWithPermissionToReadSchema();
		return associations;
	}

	public static ResultSet getImportedKeys(Session session, String schema, String table, boolean withCaching) throws SQLException {
		if (withCaching) {
			final String NAME = "getImportedKeys " + schema;
			MetaDataCache metaDataCache = (MetaDataCache) session.getSessionProperty(JDBCMetaDataBasedModelElementFinder.class, NAME);
			if (metaDataCache == null) {
				metaDataCache = MetaDataCache.readImportedKeys(session, schema);
				session.setSessionProperty(JDBCMetaDataBasedModelElementFinder.class, NAME, metaDataCache);
			}
			ResultSet resultSet = metaDataCache.forTable(table);
			if (resultSet != null) {
				return resultSet;
			}
		}
		if (DBMS.MySQL.equals(session.dbms)) {
			return session.getMetaData().getImportedKeys(schema, null, table);
		}
		return session.getMetaData().getImportedKeys(null, schema, table);
	}

	public static ResultSet getExportedKeys(Session session, String schema, String table) throws SQLException {
		if (DBMS.MySQL.equals(session.dbms)) {
			return session.getMetaData().getExportedKeys(schema, null, table);
		}
		return session.getMetaData().getExportedKeys(null, schema, table);
	}

	/**
	 * Gets qualified table name.
	 *
	 * @param defaultSchema default schema
	 * @param schema schema
	 * @param table table
	 * @return qualified table name
	 */
	private String toQualifiedTableName(String defaultSchema, String schema, String table) {
		if (schema != null && schema.trim().length() > 0 && !schema.trim().equals(defaultSchema)) {
			return schema.trim() + "." + table;
		}
		return table;
	}

	private class UnderlyingTableInfo {
		Table underlyingTable;
		Map<String, String> columnMapping = new LinkedHashMap<String, String>();
		Map<String, String> origColumnName = new LinkedHashMap<String, String>();

		@Override
		public String toString() {
			return "UnderlyingTableInfo [underlyingTable=" + underlyingTable + ", columnMapping=" + columnMapping + "]";
		}

		public void join() {
			UnderlyingTableInfo u2TableInfo = underlyingTableInfos.get(underlyingTable.getName());
			if (u2TableInfo != null) {
				Map<String, String> newColumnMapping = new HashMap<String, String>();
				for (Entry<String, String> e: u2TableInfo.columnMapping.entrySet()) {
					String cMapped = columnMapping.get(e.getValue());
					if (cMapped != null) {
						newColumnMapping.put(e.getKey(), cMapped);
						origColumnName.put(e.getKey(), u2TableInfo.origColumnName.get(e.getKey()));
					}
				}
				columnMapping = newColumnMapping;
				underlyingTable = u2TableInfo.underlyingTable;
			}
		}
	}

	/**
	 * Finds all tables in DB schema.
	 *
	 * @param session the statement executor for executing SQL-statements
	 */
	@Override
	public Set<Table> findTables(Session session, ExecutionContext executionContext) throws Exception {
		String introspectionSchema = session.getIntrospectionSchema();
		String tableNamePattern = "%";

		return findTables(session, executionContext, introspectionSchema, tableNamePattern, 0);
	}

	private Map<String, UnderlyingTableInfo> underlyingTableInfos = new LinkedHashMap<String, UnderlyingTableInfo>();
	private Map<String, String> tableTypes = new HashMap<String, String>();

	/**
	 * Finds all tables in DB schema.
	 *
	 * @param session the statement executor for executing SQL-statements
	 * @param introspectionSchema the schema
	 * @param tableNamePattern table name pattern
	 */
	private Set<Table> findTables(Session session, ExecutionContext executionContext, String introspectionSchema,
			String tableNamePattern, int depth) throws SQLException {
		final int MAX_DEPTH = 100;
		if (depth > MAX_DEPTH) {
			return new HashSet<Table>();
		}
		PrimaryKeyFactory primaryKeyFactory = new PrimaryKeyFactory(executionContext);

		Set<Table> tables = new HashSet<Table>();
		Quoting quoting = Quoting.getQuoting(session);
		List<String> types = getTypes(executionContext);
		ResultSet resultSet;
		List<String> tableNames = new ArrayList<String>();
		Map<String, String> commentPerTableName = new HashMap<>();
		for (String type: types) {
			resultSet = getTables(session, introspectionSchema, tableNamePattern, new String[] { type });
			while (resultSet.next()) {
//				1.TABLE_CAT String => table catalog (may be null) 
//				2.TABLE_SCHEM String => table schema (may be null) 
//				3.TABLE_NAME String => table name 
				if (!Quoting.equalsWROSearchPattern(introspectionSchema, resultSet.getString(1), resultSet.getString(2))) {
					continue;
				}
				String tableName = resultSet.getString(3);
				if (resultSet.getString(4) != null) {
					if (isValidName(tableName, session)) {
						tableName = quoting.quote(tableName);
						if (executionContext.getQualifyNames() || (depth > 0 && introspectionSchema != null && !introspectionSchema.equals(session.getIntrospectionSchema()))) {
							String schemaName = resultSet.getString(DBMS.MySQL.equals(session.dbms)? 1 : 2);
							if (schemaName != null) {
								schemaName = quoting.quote(schemaName.trim());
								if (schemaName.length() > 0) {
									tableName = schemaName + "." + tableName;
								}
							}
						}
						tableNames.add(tableName);
						tableTypes.put(tableName, resultSet.getString(4).toUpperCase(Locale.ENGLISH));
						String comment = resultSet.getString(5);
						if (comment != null) {
							comment = comment.trim();
							if (!comment.isEmpty()) {
								commentPerTableName.put(tableName, comment);
							}
						}
						_log.info("found table " + tableName);
					} else {
						_log.info("skip table " + tableName);
					}
				}
				CancellationHandler.checkForCancellation(null);
			}
			resultSet.close();
		}
		Map<String, Map<Integer, Column>> pkColumns = new HashMap<String, Map<Integer, Column>>();
		
		Pattern nonPKPattern = null;
		if (executionContext.getQualifiedDatamodelFolder() != null) {
			File nonPKFile = new File(executionContext.getQualifiedDatamodelFolder(), "nonPKColumns.csv");
			if (nonPKFile.exists()) {
				try {
					List<Line> lines = new CsvFile(nonPKFile).getLines();
					if (!lines.isEmpty() && !lines.get(0).cells.get(0).isEmpty()) {
						nonPKPattern = Pattern.compile(lines.get(0).cells.get(0));
					}
				} catch (Exception e) {
					// ignore
				}
			}
		}
		
		for (String tableName: tableNames) {
			Table tmp = new Table(tableName, null, false, false);
			resultSet = null;
			try {
				resultSet = getPrimaryKeys(session, quoting.unquote(tmp.getOriginalSchema(quoting.quote(introspectionSchema))), quoting.unquote(tmp.getUnqualifiedName()), true);
			} catch (Exception e) {
				_log.warn("can't get PK for " + tableName, e);
			}
			Map<Integer, Column> pk = pkColumns.get(tableName);
			if (pk == null) {
				pk = new HashMap<Integer, Column>();
				pkColumns.put(tableName, pk);
			}
			boolean hasPK = false;
			int nextKeySeq = 0;
			while (resultSet != null && resultSet.next()) {
				String cName = resultSet.getString(4);
				if (nonPKPattern != null && nonPKPattern.matcher(cName).matches()) {
					continue;
				}
				hasPK = true;
				int keySeq = resultSet.getInt(5);
				if (DBMS.SQLITE.equals(session.dbms)) {
					// SQlite driver doesn't return the keySeq
					keySeq = nextKeySeq++;
				}
				pk.put(keySeq, new Column(quoting.quote(cName), "", 0, -1));
			}
			if (!hasPK) {
				_log.info("find unique index of table " + tableName);
				hasPK = findUniqueIndexBasedKey(quoting, session, tmp, pk, tableTypes.get(tableName));
			}
			_log.info((hasPK? "" : "no ") + "primary key found for table " + tableName);
			if (resultSet != null) {
				resultSet.close();
			}
			CancellationHandler.checkForCancellation(null);
		}
		for (String tableName: tableNames) {
			Table tmp = new Table(tableName, null, false, false);
			_log.info("getting columns for " + quoting.unquote(tmp.getOriginalSchema(quoting.quote(introspectionSchema))) + "." + quoting.unquote(tmp.getUnqualifiedName()));
			resultSet = getColumns(session, quoting.unquote(tmp.getOriginalSchema(quoting.quote(introspectionSchema))), quoting.unquote(tmp.getUnqualifiedName()), "%", true, false, tableTypes.get(tableName));
			_log.info("done");
			Map<Integer, Column> pk = pkColumns.get(tableName);
			while (resultSet.next()) {
//				1.TABLE_CAT String => table catalog (may be null) 
//				2.TABLE_SCHEM String => table schema (may be null) 
//				3.TABLE_NAME String => table name 
				if (!Quoting.equalsWROSearchPattern(quoting.unquote(tmp.getOriginalSchema(quoting.quote(introspectionSchema))), resultSet.getString(1), resultSet.getString(2))) {
					continue;
				}
				if (!Quoting.equalsWROSearchPattern(quoting.unquote(tmp.getUnqualifiedName()), resultSet.getString(3))) {
					continue;
				}
				String colName = quoting.quote(resultSet.getString(4));
				int type = resultSet.getInt(5);
				int length = 0;
				int precision = -1;
				String sqlType = toSqlType(resultSet.getString(6), session.dbms);
				if (sqlType == null || sqlType.trim().length() == 0 || resultSet.wasNull()) {
					sqlType = SqlUtil.SQL_TYPE.get(type);
					if (sqlType == null) {
						throw new RuntimeException("unknown SQL type: " + type);
					}
				}
				if (TYPES_WITH_LENGTH.contains(sqlType.toUpperCase(Locale.ENGLISH)) || type == Types.NUMERIC || type == Types.DECIMAL || type == Types.VARCHAR || type == Types.CHAR || type == Types.BINARY || type == Types.VARBINARY) {
					if (!TYPES_WITHOUT_LENGTH.contains(sqlType.toUpperCase(Locale.ENGLISH))) {
						length = resultSet.getInt(7);
					}
				}
				if (DBMS.MSSQL.equals(session.dbms) && sqlType != null && sqlType.equalsIgnoreCase("timestamp")) {
					length = 0;
				}
				if (sqlType != null && sqlType.equalsIgnoreCase("uniqueidentifier")) {
					length = 0;
				}
				if (type == Types.NUMERIC || type == Types.DECIMAL || type == Types.VARCHAR || type == Types.CHAR) {
					precision = resultSet.getInt(9);
					if (resultSet.wasNull() || precision == 0) {
						precision = -1;
					}
				}
				if (type == Types.DISTINCT || getUserDefinedTypes(session).contains(sqlType)) {
					length = 0;
					precision = -1;
				}
				Column column = new Column(colName, filterType(sqlType, length, resultSet.getString(6), type, session.dbms, resultSet.getInt(7)), filterLength(length, precision, resultSet.getString(6), type, session.dbms, resultSet.getInt(7)), filterPrecision(length, precision, resultSet.getString(6), type, session.dbms, resultSet.getInt(7)));
				for (int i: pk.keySet()) {
					if (pk.get(i).name.equals(column.name)) {
						pk.put(i, column);
					}
				}
			}
			resultSet.close();
			_log.info("read primary key type for table " + tableName);

			List<Integer> keySeqs = new ArrayList<Integer>(pk.keySet());
			Collections.sort(keySeqs);
			List<Column> columns = new ArrayList<Column>();
			for (int i: keySeqs) {
				Column column = pk.get(i);
				if (column.type != null && column.type.trim().length() > 0) {
					columns.add(column);
				}
			}
			PrimaryKey primaryKey = primaryKeyFactory.createPrimaryKey(columns, tableName);
			Table table = new Table(tableName, primaryKey, false, false);
			table.setAuthor(session.getMetaData().getDriverName());
			tables.add(table);
			String comment = commentPerTableName.get(tableName);
			if (comment != null) {
				comments.put(new Pair<Table, Column>(table, null), comment.trim());
			}
			CancellationHandler.checkForCancellation(null);
		}

		for (Table table: tables) {
			String viewText = null;
			String viewTextOrDDLQuery = session.dbms.getViewTextOrDDLQuery();
			if ("VIEW".equals(tableTypes.get(table.getName())) && viewTextOrDDLQuery != null) {
				String viewTextQuery = String.format(Locale.ENGLISH, viewTextOrDDLQuery, introspectionSchema, table.getUnqualifiedName());
				final String[] viewTextContainer = new String[1];
				try {
					session.executeQuery(viewTextQuery, new Session.AbstractResultSetReader() {
						@Override
						public void readCurrentRow(ResultSet resultSet) throws SQLException {
							viewTextContainer[0] = resultSet.getString(1);
						}
					});
					viewText = viewTextContainer[0];
					if (viewText != null) {
						viewText = viewText.trim();
						viewText = viewText.replaceFirst("(?is)^create.*as\\b(\\W*select\\b)(.*)$", "$1$2").trim();
					}
				} catch (Exception e) {
					_log.info("can't get view text: " + viewTextQuery);
					_log.info(e.getMessage());
				}
			} else if ("SYNONYM".equals(tableTypes.get(table.getName())) || "ALIAS".equals(tableTypes.get(table.getName()))) {
				if (session.dbms.getSynonymTableQuery() != null) {
					String synonymTableQuery = String.format(Locale.ENGLISH, session.dbms.getSynonymTableQuery(), introspectionSchema, table.getUnqualifiedName());
					final String[] synonymTableQueryContainer = new String[1];
					try {
						session.executeQuery(synonymTableQuery, new Session.AbstractResultSetReader() {
							@Override
							public void readCurrentRow(ResultSet resultSet) throws SQLException {
								synonymTableQueryContainer[0] = "Select * from " + resultSet.getString(1);
							}
						});
						viewText = synonymTableQueryContainer[0];
					} catch (Exception e) {
						_log.info("can't get synonym table: " + synonymTableQuery);
						_log.info(e.getMessage());
					}
				}
			} else {
				continue;
			}

			if (viewText != null) {
				UnderlyingTableInfo uti = parseViewText(session, executionContext, quoting, depth, viewText, introspectionSchema, table);
				if (uti != null) {
					List<Column> columns = findColumns(table, session, executionContext);
					uti.join();
					if (table.primaryKey.getColumns().isEmpty()) {
						if (uti.underlyingTable.primaryKey != null && !uti.underlyingTable.primaryKey.getColumns().isEmpty()) {
							List<Column> newPK = new ArrayList<Column>();
							for (Column pkc: uti.underlyingTable.primaryKey.getColumns()) {
								String newPkc = uti.columnMapping.get(quoting.normalizeCase(Quoting.staticUnquote(pkc.name)));
								if (newPkc != null) {
									Column newC = null;
									for (Column c: columns) {
										if (Quoting.equalsIgnoreQuotingAndCase(c.name, newPkc)) {
											newC = c;
											break;
										}
									}
									if (newC != null) {
										newPK.add(newC);
									} else {
										newPK = null;
										break;
									}
								} else {
									newPK = null;
									break;
								}
							}
							if (newPK != null) {
								table.primaryKey.assign(newPK);
							}
						}
					}
				}
			}
		}

		return tables;
	}

	private UnderlyingTableInfo parseViewText(final Session session, final ExecutionContext executionContext, final Quoting quoting, final int depth, String viewText, final String defaultSchema, final Table view) {
		net.sf.jsqlparser.statement.Statement st;
		try {
			_log.info("analyzing view \"" + view.getName() + "\": " + viewText.replaceAll("\\s+", " "));
			final List<Column> columns = findColumns(view, session, executionContext);
			final boolean[] isValid = new boolean[] { true };
			final boolean[] selectExists = new boolean[] { false };
			final UnderlyingTableInfo underlyingTableInfo = new UnderlyingTableInfo();
			st = JSqlParserUtil.parse(SqlUtil.removeNonMeaningfulFragments(viewText), 5);
			st.accept(new StatementVisitorAdapter() {
				@Override
				public void visit(Upsert arg0) {
				}
				@Override
				public void visit(Select select) {
					select.getSelectBody().accept(new SelectVisitorAdapter() {
						@Override
						public void visit(WithItem withItem) {
							isValid[0] = false;
						}
						@Override
						public void visit(SetOperationList setOpList) {
							isValid[0] = false;
						}
						@Override
						public void visit(PlainSelect plainSelect) {
							if (plainSelect.getFromItem() != null && plainSelect.getJoins() == null) {
								selectExists[0] = true;
								for (SelectItem sItem: plainSelect.getSelectItems()) {
									sItem.accept(new SelectItemVisitor() {
										@Override
										public void visit(SelectExpressionItem eItem) {
											Expression expression = eItem.getExpression();
											if (expression instanceof net.sf.jsqlparser.schema.Column) {
												String column = ((net.sf.jsqlparser.schema.Column) expression).getColumnName();
												column = Quoting.staticUnquote(column);
												String alias = column;
												if (eItem.getAlias() != null) {
													if (eItem.getAlias().getName() != null) {
														alias = Quoting.staticUnquote(eItem.getAlias().getName());
													}
												}
												underlyingTableInfo.columnMapping.put(quoting.normalizeCase(Quoting.staticUnquote(column)), quoting.normalizeCase(Quoting.staticUnquote(alias)));
												underlyingTableInfo.origColumnName.put(quoting.normalizeCase(Quoting.staticUnquote(column)), Quoting.staticUnquote(alias));
											}
										}
										@Override
										public void visit(AllTableColumns arg0) {
											for (Column column: columns) {
												underlyingTableInfo.columnMapping.put(quoting.normalizeCase(Quoting.staticUnquote(column.name)), quoting.normalizeCase(Quoting.staticUnquote(column.name)));
												underlyingTableInfo.origColumnName.put(quoting.normalizeCase(Quoting.staticUnquote(column.name)), Quoting.staticUnquote(column.name));
											}
										}
										@Override
										public void visit(AllColumns arg0) {
											for (Column column: columns) {
												underlyingTableInfo.columnMapping.put(quoting.normalizeCase(Quoting.staticUnquote(column.name)), quoting.normalizeCase(Quoting.staticUnquote(column.name)));
												underlyingTableInfo.origColumnName.put(quoting.normalizeCase(Quoting.staticUnquote(column.name)), Quoting.staticUnquote(column.name));
											}
										}
									});
								}
								FromItemVisitor fromItemVisitor = new FromItemVisitor() {
									private void unknownTable() {
										isValid[0] = false;
										selectExists[0] = false;
									}
									@Override
									public void visit(TableFunction tableFunction) {
										unknownTable();
									}

									@Override
									public void visit(ValuesList valuesList) {
										unknownTable();
									}

									@Override
									public void visit(LateralSubSelect lateralSubSelect) {
										unknownTable();
									}

									@Override
									public void visit(SubJoin subjoin) {
										unknownTable();
									}

									@Override
									public void visit(SubSelect subSelect) {
										unknownTable();
									}

									@Override
									public void visit(net.sf.jsqlparser.schema.Table tableName) {
										String schema = Quoting.staticUnquote(tableName.getSchemaName());
										if (schema == null) {
											schema = defaultSchema;
										}
										String name = Quoting.staticUnquote(tableName.getName());
										if (tableName.getPivot() != null) {
											unknownTable();
										} else {
											try {
												Session theSession = session;
												while (theSession != null) {
													Quoting quoting = Quoting.getQuoting(theSession);
													Set<Table> tables = findTables(theSession, executionContext, quoting.normalizeCase(schema), quoting.normalizeCase(name), depth + 1);
													if (tables.size() < 1) {
														tables = findTables(theSession, executionContext, schema, name, depth + 1);
													}
													if (tables.size() >= 1) {
														underlyingTableInfo.underlyingTable = tables.iterator().next();
														break;
													} else {
														if (session == theSession && Quoting.equalsIgnoreQuotingAndCase(theSession.getIntrospectionSchema(), schema)
															||
															checkReadPermission(theSession, schema)
															|| checkReadPermission(theSession, quoting.normalizeCase(schema))) {
																_log.warn("View or Synonym \"" + view.getName() + "\": can't find underlying table \"" + schema + "." + name + "\"");
																break;
														} else {
															Session newSession = sessionWithPermissionToReadSchema.get(Quoting.normalizeIdentifier(schema));
															if (newSession != null && newSession != theSession) {
																theSession = newSession;
															} else {
																theSession = askForSessionWithPermissionToReadSchema(session, view, Quoting.normalizeIdentifier(schema), quoting.normalizeCase(schema), quoting.normalizeCase(name), executionContext);
																if (theSession == null) {
																	_log.warn("Insufficient privileges to analyze table \"" + schema + "." + name + "\"");
																}
															}
														}
													}
												}
											} catch (SQLException e) {
												_log.error("paring failed", e);
											}
										}
									}
									@Override
									public void visit(ParenthesisFromItem aThis) {
										if (aThis.getFromItem() != null) {
											aThis.getFromItem().accept(this);
										}
									}
								};
								plainSelect.getFromItem().accept(fromItemVisitor);
							}
						}
					});
				}
			});
			if (isValid[0] && selectExists[0] && underlyingTableInfo.underlyingTable != null) {
				underlyingTableInfos.put(view.getName(), underlyingTableInfo);
				return underlyingTableInfo;
			}
		} catch (Exception e) {
			_log.info("can't parse view definition");
			_log.info(e.getMessage());
		}
		return null;
	}

	private List<String> getTypes(ExecutionContext executionContext) {
		ArrayList<String> result = new ArrayList<String>();
		result.add("TABLE");
		if (executionContext.getAnalyseAlias()) {
			result.add("ALIAS");
		}
		if (executionContext.getAnalyseSynonym()) {
			result.add("SYNONYM");
		}
		if (executionContext.getAnalyseView()) {
			result.add("VIEW");
		}
		return result;
	}

	public static ResultSet getPrimaryKeys(Session session, String schema, String table, boolean withCaching) throws SQLException {
		if (withCaching) {
			final String NAME = "getPrimaryKeys " + schema;
			MetaDataCache metaDataCache = (MetaDataCache) session.getSessionProperty(JDBCMetaDataBasedModelElementFinder.class, NAME);
			if (metaDataCache == null) {
				metaDataCache = MetaDataCache.readPrimaryKeys(session, schema);
				session.setSessionProperty(JDBCMetaDataBasedModelElementFinder.class, NAME, metaDataCache);
			}
			ResultSet resultSet = metaDataCache.forTable(table);
			if (resultSet != null) {
				return resultSet;
			}
		}
		if (DBMS.MySQL.equals(session.dbms)) {
			return session.getMetaData().getPrimaryKeys(schema, null, table);
		}
		return session.getMetaData().getPrimaryKeys(null, schema, table);
	}

	public static ResultSet getTables(Session session, String schemaPattern, String tableNamePattern, String[] types) throws SQLException {
		if (DBMS.MySQL.equals(session.dbms)) {
			 return session.getMetaData().getTables(schemaPattern, null, tableNamePattern, types);
		}
		return session.getMetaData().getTables(null, schemaPattern, tableNamePattern, types);
	}

	/**
	 * Finds comments for all model elements.
	 * 
	 * @return map from element (table, column) to the comment
	 */
	@Override
	public Map<Pair<Table, Column>, String> getComments() {
		return comments;
	}

	/**
	 * Find a key of a table based on an unique index on non-nullable columns.
	 */
	private boolean findUniqueIndexBasedKey(Quoting quoting, Session session, Table tmp, Map<Integer, Column> pk, String tableType) {
		try {
			ResultSet resultSet = getColumns(session, quoting.unquote(tmp.getOriginalSchema(quoting.quote(session.getIntrospectionSchema()))), quoting.unquote(tmp.getUnqualifiedName()), "%", true, false, tableType);

			List<String> nonNullColumns = new ArrayList<String>();
			while (resultSet.next()) {
//				1.TABLE_CAT String => table catalog (may be null) 
//				2.TABLE_SCHEM String => table schema (may be null) 
//				3.TABLE_NAME String => table name 
				if (!Quoting.equalsWROSearchPattern(quoting.unquote(tmp.getOriginalSchema(quoting.quote(session.getIntrospectionSchema()))), resultSet.getString(1), resultSet.getString(2))) {
					continue;
				}
				if (!Quoting.equalsWROSearchPattern(quoting.unquote(tmp.getUnqualifiedName()), resultSet.getString(3))) {
					continue;
				}
				int type = resultSet.getInt(5);
				if (resultSet.getInt(11) == DatabaseMetaData.columnNoNulls) {
					nonNullColumns.add(resultSet.getString(4));
					if (!(
							type == Types.BIGINT ||
							type == Types.BOOLEAN ||
							type == Types.CHAR ||
							type == Types.DATE ||
							type == Types.DECIMAL ||
							type == Types.DOUBLE ||
							type == Types.FLOAT ||
							type == Types.INTEGER ||
							type == Types.NCHAR ||
							type == Types.NVARCHAR ||
							type == Types.REAL ||
							type == Types.SMALLINT ||
							type == Types.TIME ||
							type == Types.TIMESTAMP ||
							type == Types.TINYINT ||
							type == Types.VARCHAR
						)) {
					}
				}
			}
			resultSet.close();

			if (nonNullColumns.isEmpty()) {
				return false;
			}

			resultSet = getIndexInfo(session, quoting.unquote(tmp.getOriginalSchema(quoting.quote(session.getIntrospectionSchema()))), quoting.unquote(tmp.getUnqualifiedName()), true, true);
			Map<String, List<String>> indexes = new TreeMap<String, List<String>>();
			while (resultSet.next()) {
				String indexName = resultSet.getString(6);
				if (indexName == null || resultSet.getBoolean(4)) {
					continue;
				}
				List<String> indexColumns = indexes.get(indexName);
				if (indexColumns == null) {
					indexColumns = new ArrayList<String>();
					indexes.put(indexName, indexColumns);
				}
				indexColumns.add(resultSet.getString(9));
			}
			resultSet.close();

			for (String index: indexes.keySet()) {
				List<Column> columns = new ArrayList<Column>();
				boolean isNullable = false;
				for (String column: indexes.get(index)) {
					if (column == null || !nonNullColumns.contains(column)) {
						isNullable = true;
						break;
					}
					columns.add(new Column(quoting.quote(column), "", 0, -1));
				}
				if (!isNullable && !columns.isEmpty()) {
					for (int i = 1; i <= columns.size(); ++i) {
						pk.put(i, columns.get(i - 1));
					}
					return true;
				}
			}
			return false;
		} catch (Exception e) {
			_log.error(e.getMessage(), e);
			return false;
		}
	}

	private ResultSet getIndexInfo(Session session, String schema, String table, boolean unique, boolean approximate) throws SQLException {
		final String NAME = "getIndexInfo " + schema;
		MetaDataCache metaDataCache = (MetaDataCache) session.getSessionProperty(JDBCMetaDataBasedModelElementFinder.class, NAME);
		if (metaDataCache == null) {
			metaDataCache = MetaDataCache.readIndexInfo(session, schema);
			session.setSessionProperty(JDBCMetaDataBasedModelElementFinder.class, NAME, metaDataCache);
		}
		ResultSet resultSet = metaDataCache.forTable(table);
		if (resultSet != null) {
			return resultSet;
		}
		if (DBMS.MySQL.equals(session.dbms)) {
			return session.getMetaData().getIndexInfo(schema, null, table, unique, approximate);
		}
		return session.getMetaData().getIndexInfo(null, schema, table, unique, approximate);
	}

	/**
	 * Calls {@link DatabaseMetaData#getIndexInfo(String, String, String, boolean, boolean). Uses schemaPattern as catalogPattern on MySQL.
	 */
	public static ResultSet getIndexes(Session session, String schemaPattern, String tableNamePattern) throws SQLException {
		if (DBMS.MySQL.equals(session.dbms)) {
			return session.getMetaData().getIndexInfo(schemaPattern, null, tableNamePattern, false, true);
		}
		return session.getMetaData().getIndexInfo(null, schemaPattern, tableNamePattern, false, true);
	}
	
	/**
	 * Calls {@link DatabaseMetaData#getColumns(String, String, String, String)}. Uses schemaPattern as catalogPattern on MySQL.
	 * @param withCaching
	 */
	public static ResultSet getColumns(Session session, String schemaPattern, String tableNamePattern, String columnNamePattern, boolean withCaching, boolean onlyIfCached, String tableType) throws SQLException {
		boolean includeSynonym = false;
		if (DBMS.ORACLE.equals(session.dbms)) {
			if ("ALIAS".equalsIgnoreCase(tableType) || "SYNONYM".equalsIgnoreCase(tableType)) {
				includeSynonym = true;
			}
			includeSynonym = setIncludeSynonyms(includeSynonym, session);
		}
		try {
			final String NAME = "getColumns " + includeSynonym + " " + schemaPattern;

			Object mdc = session.getSessionProperty(JDBCMetaDataBasedModelElementFinder.class, NAME);
			if (withCaching && onlyIfCached) {
				if (mdc == null) {
					withCaching = false;
				}

			}
			if (withCaching) {
				synchronized (session.MD_GETCOLUMNS_LOCK) {
					MetaDataCache metaDataCache = (MetaDataCache) session.getSessionProperty(JDBCMetaDataBasedModelElementFinder.class, NAME);
					if (metaDataCache == null) {
						metaDataCache = MetaDataCache.readColumns(session, schemaPattern);
						session.setSessionProperty(JDBCMetaDataBasedModelElementFinder.class, NAME, metaDataCache);
					}
					ResultSet resultSet = metaDataCache.forTable(tableNamePattern);
					if (resultSet != null) {
						return resultSet;
					}
				}
			}
			if (DBMS.MySQL.equals(session.dbms)) {
				return session.getMetaData().getColumns(schemaPattern, null, tableNamePattern, columnNamePattern);
			}
			return session.getMetaData().getColumns(null, schemaPattern, tableNamePattern, columnNamePattern);
		} finally {
			if (DBMS.ORACLE.equals(session.dbms)) {
				if (includeSynonym) {
					setIncludeSynonyms(false, session);
				}
			}
		}
	}

	private static boolean setIncludeSynonyms(boolean includeSynonyms, Session session) {
		try {
			Connection con = session.getConnection();
			con.getClass().getClassLoader().loadClass("oracle.jdbc.OracleConnection").getMethod("setIncludeSynonyms", new Class[] { boolean.class }).invoke(con, includeSynonyms);
			return includeSynonyms;
		} catch (Throwable t) {
			return false;
		}
	}

	/**
	 * Calls {@link DatabaseMetaData#getProcedures(String, String, String)}. Uses schemaPattern as catalogPattern on MySQL.
	 */
	public static ResultSet getProcedures(Session session, String schemaPattern, String functionPattern) throws SQLException {
		if (DBMS.MySQL.equals(session.dbms)) {
			return session.getMetaData().getProcedures(schemaPattern, null, functionPattern);
		}
		return session.getMetaData().getProcedures(null, schemaPattern, functionPattern);
	}

	/**
	 * Checks syntactical correctness of names.
	 *
	 * @param name a table or column name
	 * @return <code>true</code> if name is syntactically correct
	 */
	private boolean isValidName(String name, Session session) {
		return name != null && (!DBMS.ORACLE.equals(session.dbms) || !name.startsWith("BIN$"));
	}

	/**
	 * Finds all schemas in DB.
	 *
	 * @param session the statement executor for executing SQL-statements
	 * @param userName schema with this name may be empty
	 */
	public static List<String> getSchemas(Session session, String userName) {
		List<String> schemas = new ArrayList<String>();
		try {
			DatabaseMetaData metaData = session.getMetaData();
			ResultSet rs = DBMS.MySQL.equals(session.dbms)? metaData.getCatalogs() : metaData.getSchemas();
			if (rs != null) {
				while (rs.next()) {
					String schema = rs.getString(DBMS.MySQL.equals(session.dbms)? "TABLE_CAT" : "TABLE_SCHEM").trim();
					if (schema != null) {
						if (DBMS.POSTGRESQL.equals(session.dbms) && schema.startsWith("pg_toast_temp")) {
							continue;
						}
						schemas.add(schema);
					}
				}
				rs.close();
			}
		} catch (SQLException e) {
			e.printStackTrace();
			if (userName != null) {
				schemas.add(userName);
			}
		}
		Collections.sort(schemas);
		return schemas;
	}

	/**
	 * Finds all catalogs with all schemas in DB.
	 *
	 * @param session the statement executor for executing SQL-statements
	 */
	public static List<String> getCatalogsWithSchemas(Session session) {
		if (DBMS.MySQL.equals(session.dbms)) {
			return Collections.emptyList(); // no catalogs here
		}
		if (!DBMS.MSSQL.equals(session.dbms)) {
			return Collections.emptyList(); // DBMS not (yet) supported
		}

		List<String> schemas = new ArrayList<String>();
		try {
			DatabaseMetaData metaData = session.getMetaData();
			ResultSet rsCatalog = metaData.getCatalogs();
			while (rsCatalog.next()) {
				String catalog = rsCatalog.getString("TABLE_CAT");
				if (catalog != null) {
					catalog = catalog.trim();
					if (!catalog.isEmpty()) {
						ResultSet rs = DBMS.MySQL.equals(session.dbms)? metaData.getCatalogs() : metaData.getSchemas();
						if (rs != null) {
							while (rs.next()) {
								String schema = rs.getString(DBMS.MySQL.equals(session.dbms)? "TABLE_CAT" : "TABLE_SCHEM").trim();
								if (schema != null) {
									if (DBMS.POSTGRESQL.equals(session.dbms) && schema.startsWith("pg_toast_temp")) {
										continue;
									}
									schemas.add(catalog + "." + schema);
								}
							}
							rs.close();
						}
					}
				}
			}
			rsCatalog.close();
		} catch (SQLException e) {
			return Collections.emptyList();
		}
		Collections.sort(schemas);
		return schemas;
	}

	/**
	 * Gets default schema of DB.
	 *
	 * @param session the statement executor for executing SQL-statements
	 * @param userName schema with this name may be empty
	 */
	public static String getDefaultSchema(Session session, String userName) {
		return getDefaultSchema(session, userName, null);
	}

	/**
	 * Gets default schema of DB.
	 *
	 * @param session the statement executor for executing SQL-statements
	 * @param userName schema with this name may be empty
	 */
	public static String getDefaultSchema(Session session, String userName, List<String> schemaNames) {
		if (session.dbms.getDefaultSchemaQuery() != null) {
			try {
				final String[] database = new String[1];
				session.executeQuery(session.dbms.getDefaultSchemaQuery(), new Session.AbstractResultSetReader() {
					@Override
					public void readCurrentRow(ResultSet resultSet) throws SQLException {
						database[0] = resultSet.getString(1);
					}
				});
				if (database[0] != null) {
					return database[0];
				}
			} catch (SQLException e) {
				if (!session.isDown()) {
					e.printStackTrace();
				}
			}
		}
		List<String> schemas = new ArrayList<String>();
		try {
			DatabaseMetaData metaData = session.getMetaData();
			boolean isPostgreSQL = DBMS.POSTGRESQL.equals(session.dbms);
			boolean isH2Sql = DBMS.H2.equals(session.dbms);
			if (schemaNames != null) {
				schemas.addAll(schemaNames);
			} else {
				ResultSet rs = DBMS.MySQL.equals(session.dbms)? metaData.getCatalogs() : metaData.getSchemas();
				if (rs != null) {
					while (rs.next()) {
						schemas.add(rs.getString(DBMS.MySQL.equals(session.dbms)? "TABLE_CAT" : "TABLE_SCHEM"));
					}
					rs.close();
				}
			}
			String userSchema = null;
			for (Iterator<String> i = schemas.iterator(); i.hasNext(); ) {
				String schema = i.next().trim();
				if ((isPostgreSQL || isH2Sql) && "public".equalsIgnoreCase(schema)) {
					return schema;
				}
				if (schema.equalsIgnoreCase(userName.trim())) {
					if (userSchema == null || !userSchema.equals(userName.trim())) {
						userSchema = schema;
					}
				}
			}
			if (userSchema != null) {
				return userSchema;
			}
			return userName;
		} catch (SQLException e) {
			if (!session.isDown()) {
				e.printStackTrace();
			}
			return userName;
		}
	}

	private Session forDefaultSchema = null;
	private String defaultSchema = null;

	/**
	 * Finds the {@link Column}s of a given {@link Table}.
	 *
	 * @param table the table
	 * @param session the statement executor for executing SQL-statements
	 *
	 * @throws Exception on each error
	 */
	@Override
	public List<Column> findColumns(Table table, Session session, ExecutionContext executionContext) throws SQLException {
		List<Column> columns = new ArrayList<Column>();
		Quoting quoting = Quoting.getQuoting(session);
		if (forDefaultSchema != session) {
			forDefaultSchema = session;
			_log.info("getting default schema...");
			defaultSchema = getDefaultSchema(session, session.getSchema());
			_log.info("default schema is '" + defaultSchema + "'");
		}
		UnderlyingTableInfo uti = underlyingTableInfos.get(table.getName());
		String schemaName = quoting.unquote(table.getOriginalSchema(defaultSchema));
		String tableName = quoting.unquote(table.getUnqualifiedName());
		_log.info("getting columns for " + table.getOriginalSchema(defaultSchema) + "." + tableName);
		ResultSet resultSet = getColumns(session, schemaName, tableName, "%", true, false, tableTypes.get(table.getName()));
		_log.info("done");
		while (resultSet.next()) {
//			1.TABLE_CAT String => table catalog (may be null) 
//			2.TABLE_SCHEM String => table schema (may be null) 
//			3.TABLE_NAME String => table name 
			if (!Quoting.equalsWROSearchPattern(schemaName, resultSet.getString(1), resultSet.getString(2))) {
				continue;
			}
			if (!Quoting.equalsWROSearchPattern(tableName, resultSet.getString(3))) {
				continue;
			}
			String colName = quoting.quote(resultSet.getString(4));
			int type = resultSet.getInt(5);
			int length = 0;
			int precision = -1;
			if (type == Types.NUMERIC || type == Types.DECIMAL || type == Types.VARCHAR || type == Types.CHAR) {
				length = resultSet.getInt(7);
			}
			if (type == Types.NUMERIC || type == Types.DECIMAL || type == Types.VARCHAR || type == Types.CHAR) {
				precision = resultSet.getInt(9);
				if (resultSet.wasNull() || precision == 0) {
					precision = -1;
				}
			}
			String sqlType = toSqlType(resultSet.getString(6), session.dbms);
			if (sqlType == null || sqlType.trim().length() == 0 || resultSet.wasNull()) {
				sqlType = SqlUtil.SQL_TYPE.get(type);
				if (sqlType == null) {
					continue;
					// throw new RuntimeException("unknown SQL type: " + type);
				}
			}
			if (TYPES_WITH_LENGTH.contains(sqlType.toUpperCase(Locale.ENGLISH)) || type == Types.NUMERIC || type == Types.DECIMAL || type == Types.VARCHAR || type == Types.CHAR || type == Types.BINARY || type == Types.VARBINARY) {
				length = resultSet.getInt(7);
				if (type == Types.VARCHAR) {
					if (session.dbms.getVarcharLengthLimit() != null) {
						length = Math.min(length, session.dbms.getVarcharLengthLimit());
					}
				}
			}
			if (sqlType != null && sqlType.equalsIgnoreCase("uniqueidentifier")) {
				length = 0;
			}
			if (DBMS.MSSQL.equals(session.dbms) && sqlType != null && sqlType.equalsIgnoreCase("timestamp")) {
				length = 0;
			}
			if (type == Types.NUMERIC || type == Types.DECIMAL || type == Types.VARCHAR || type == Types.CHAR) {
				precision = resultSet.getInt(9);
				if (resultSet.wasNull() || precision == 0) {
					precision = -1;
				}
			}
			if (type == Types.DISTINCT || getUserDefinedTypes(session).contains(sqlType)) {
				length = 0;
				precision = -1;
			}
			_log.debug("column info: '" + colName + "' '" + sqlType + "' " + type + " '" + resultSet.getString(6) + "'");
			Column column = new Column(colName, filterType(sqlType, length, resultSet.getString(6), type, session.dbms, resultSet.getInt(7)), filterLength(length, precision, resultSet.getString(6), type, session.dbms, resultSet.getInt(7)), filterPrecision(length, precision, resultSet.getString(6), type, session.dbms, resultSet.getInt(7)));
			column.isNullable = resultSet.getInt(11) == DatabaseMetaData.columnNullable;
			String comment = resultSet.getString(12);
			if (comment != null) {
				comment = comment.trim();
				if (!comment.isEmpty()) {
					comments.put(new Pair<Table, Column>(table, column), comment);
				}
			}
			Boolean isVirtual = null;
			if (session.dbms.getExportBlocks().contains(sqlType)) {
				isVirtual = true;
			}
			if (uti != null && !uti.columnMapping.containsKey(quoting.normalizeCase(Quoting.staticUnquote(colName)))) {
				isVirtual = true;
			}
			if (isVirtual == null) {
				String virtualColumnsQuery = session.dbms.getVirtualColumnsQuery();
				if (virtualColumnsQuery != null) {
					@SuppressWarnings("unchecked")
					Set<Pair<String, String>> virtualColumns = (Set<Pair<String, String>>) session.getSessionProperty(getClass(), "virtualColumns" + schemaName);
					if (virtualColumns == null) {
						virtualColumns = new HashSet<Pair<String,String>>();
							try {
								session.setSilent(true);
								final Set<Pair<String, String>> finalVirtualColumns = virtualColumns;
								session.executeQuery(virtualColumnsQuery.replace("${SCHEMA}", schemaName), new Session.AbstractResultSetReader() {
									@Override
									public void readCurrentRow(ResultSet resultSet) throws SQLException {
										finalVirtualColumns.add(new Pair<String, String>(resultSet.getString(1), resultSet.getString(2)));
									}
								});
							} catch (Exception e) {
								// ignore
							} finally {
								session.setSilent(false);
							}
							session.setSessionProperty(getClass(), "virtualColumns" + schemaName, virtualColumns);
						}
					isVirtual = virtualColumns.contains(new Pair<String, String>(tableName, resultSet.getString(4)));
				}
			}
			if (isVirtual == null) {
				if (!Boolean.FALSE.equals(session.getSessionProperty(getClass(), "JDBC4Supported"))) {
					try {
						String virtual = resultSet.getString(24);
						if (virtual != null) {
							isVirtual = "YES".equalsIgnoreCase(virtual);
						}
					} catch (Exception e) {
						session.setSessionProperty(getClass(), "JDBC4Supported", false);
					}
				}
			}
			if (isVirtual != null) {
				column.isVirtual = isVirtual;
			}
			
			try {
				String identityColumnsQuery = session.dbms.getIdentityColumnsQuery();
				if (identityColumnsQuery != null) {
					@SuppressWarnings("unchecked")
					Set<Pair<String, String>> identityColumns = (Set<Pair<String, String>>) session.getSessionProperty(getClass(), "identityColumns" + schemaName);
					if (identityColumns == null) {
						identityColumns = new HashSet<Pair<String,String>>();
							try {
								session.setSilent(true);
								final Set<Pair<String, String>> finalidentityColumns = identityColumns;
								session.executeQuery(identityColumnsQuery.replace("${SCHEMA}", schemaName), new Session.AbstractResultSetReader() {
									@Override
									public void readCurrentRow(ResultSet resultSet) throws SQLException {
										finalidentityColumns.add(new Pair<String, String>(resultSet.getString(1), resultSet.getString(2)));
									}
								});
							} catch (Exception e) {
								// ignore
							} finally {
								session.setSilent(false);
							}
							session.setSessionProperty(getClass(), "identityColumns" + schemaName, identityColumns);
						}
					if (identityColumns.contains(new Pair<String, String>(tableName, resultSet.getString(4)))) {
						column.isIdentityColumn = true;
					}
				}
			} catch (Exception e) {
				LogUtil.warn(e);
			}
		
			columns.add(column);
		}
		resultSet.close();
		_log.info("found columns for table " + table.getName());
		return columns;
	}

	/**
	 * Filters the length attribute of a column in a DBMS specific way.
	 *
	 * @param length the length as given from driver
	 * @param precision the precision as given from driver
	 * @param type the sql type
	 * @param dbms the DBMS
	 *
	 * @return filtered length
	 */
	public static int filterLength(int length, int precision, String typeName, int type, DBMS dbms, int origLength) {
		if (length > 0) {
			if (length == 65535 && precision == 32767 && "DECIMAL".equals(typeName) && DBMS.H2.equals(dbms)) {
				return 0;
			}
			if (DBMS.POSTGRESQL.equals(dbms)) {
				if (type == Types.VARCHAR && length >= 10485760) {
					length = 0;
				} else if (type == Types.NUMERIC && length > 1000) {
					length = 0;
				} else if ("bytea".equalsIgnoreCase(typeName)) {
					length = 0;
				}
				if (length > 10485760 && "bpchar".equalsIgnoreCase(typeName)) {
					length = 0;
				}
			} else if (DBMS.SQLITE.equals(dbms)) {
				return 0;
			}
		} else {
			if (DBMS.POSTGRESQL.equals(dbms)) {
				if ("bit".equalsIgnoreCase(typeName)) {
					length = origLength;
				}
			}
		}
		return length;
	}

	/**
	 * Filters the precision attribute of a column in a DBMS specific way.
	 *
	 * @param length the length as given from driver
	 * @param precision
	 * @param type the sql type
	 * @param dbms the DBMS
	 *
	 * @return filtered length
	 */
	public static int filterPrecision(int length, int precision, String typeName, int type, DBMS dbms, int origLength) {
		if (length == 65535 && precision == 32767 && "DECIMAL".equals(typeName) && DBMS.H2.equals(dbms)) {
			return -1;
		}
		return precision;
	}

	/**
	 * Filters the type attribute of a column in a DBMS specific way.
	 *
	 * @param length the length as given from driver
	 * @param type the sql type
	 * @param dbms the DBMS
	 *
	 * @return filtered length
	 */
	public static String filterType(String sqlType, int length, String typeName, int type, DBMS dbms, int origLength) {
		if (length > 10485760) {
			if (DBMS.POSTGRESQL.equals(dbms)) {
				if ("bpchar".equalsIgnoreCase(typeName)) {
					return "varchar";
				}
			}
		}
		return sqlType;
	}

	/**
	 * Converts result from {@link DatabaseMetaData#getColumns(String, String, String, String)}
	 * into the type name.
	 */
	private static String toSqlType(String sqlType, DBMS dbms) {
		if (sqlType == null) {
			return null;
		}
		sqlType = sqlType.trim();
		
		if (sqlType.equalsIgnoreCase("CHARACTER VARYING")) {
			sqlType = "VARCHAR";
		}

		if (DBMS.MySQL.equals(dbms)) {
			if (sqlType.equalsIgnoreCase("SET") || sqlType.equalsIgnoreCase("ENUM")) {
				return "VARCHAR";
			}
		}
		if (!sqlType.toLowerCase(Locale.ENGLISH).endsWith(" identity")) {
			// Some drivers (MS SQL Server driver for example) prepends the type with some options,
			// so we ignore everything after the first space.
			int i = sqlType.indexOf(' ');
			if (i > 0) {
				sqlType = sqlType.substring(0, i);
			}
			i = sqlType.indexOf('(');
			if (i > 0) {
				sqlType = sqlType.substring(0, i);
			}
		}
		return sqlType;
	}

	public static Column toColumn(ResultSetMetaData metaData, int i, Session session) throws SQLException {
		Quoting quoting = Quoting.getQuoting(session);
		String colName = quoting.quote(metaData.getColumnLabel(i));
		int type = metaData.getColumnType(i);
		int length = 0;
		int precision = -1;
		if (type == Types.NUMERIC || type == Types.DECIMAL || type == Types.VARCHAR || type == Types.CHAR) {
			length = metaData.getPrecision(i);
		}
		if (type == Types.NUMERIC || type == Types.DECIMAL || type == Types.VARCHAR || type == Types.CHAR) {
			precision = metaData.getScale(i);
			if (precision == 0) {
				precision = -1;
			}
		}
		String sqlType = toSqlType(metaData.getColumnTypeName(i), session.dbms);
		if (sqlType == null || sqlType.trim().length() == 0) {
			sqlType = SqlUtil.SQL_TYPE.get(type);
			if (sqlType == null) {
				sqlType = "(?unknown type)";
			}
		}
		if (TYPES_WITH_LENGTH.contains(sqlType.toUpperCase(Locale.ENGLISH)) || type == Types.NUMERIC || type == Types.DECIMAL || type == Types.VARCHAR || type == Types.CHAR || type == Types.BINARY || type == Types.VARBINARY) {
			length = metaData.getPrecision(i);
			if (type == Types.VARCHAR) {
				if (session.dbms.getVarcharLengthLimit() != null) {
					length = Math.min(length, session.dbms.getVarcharLengthLimit());
				}
			}
		}
		if (sqlType != null && sqlType.equalsIgnoreCase("uniqueidentifier")) {
			length = 0;
		}
		if (DBMS.MSSQL.equals(session.dbms) && sqlType != null && sqlType.equalsIgnoreCase("timestamp")) {
			length = 0;
		}
		if (type == Types.NUMERIC || type == Types.DECIMAL || type == Types.VARCHAR || type == Types.CHAR) {
			precision = metaData.getScale(i);
			if (precision == 0) {
				precision = -1;
			}
		}
		if (type == Types.DISTINCT) {
			length = 0;
			precision = -1;
		}
		Column column = new Column(colName, filterType(sqlType, length, metaData.getColumnTypeName(i), type, session.dbms, metaData.getPrecision(i)), filterLength(length, precision, metaData.getColumnTypeName(i), type, session.dbms, metaData.getPrecision(i)), filterPrecision(length, precision, metaData.getColumnTypeName(i), type, session.dbms, metaData.getPrecision(i)));
		column.isNullable = metaData.isNullable(i) != ResultSetMetaData.columnNoNulls;
		return column;
	}

	public static void resetCaches(Session session) {
		session.removeSessionProperties(JDBCMetaDataBasedModelElementFinder.class);
	}

	private Map<String, Session> sessionWithPermissionToReadSchema = new HashMap<String, Session>();

	private void shutDownSessionsWithPermissionToReadSchema() {
		for (Session s: sessionWithPermissionToReadSchema.values()) {
			try {
				s.shutDown();
			} catch (Exception e) {
				_log.warn(e.getMessage());
			}
		}
		sessionWithPermissionToReadSchema.clear();
	}

	public static interface PrivilegedSessionProvider {
		Session askForSessionWithPermissionToReadSchema(Session session, Table view, String schema, String tableName, ExecutionContext executionContext);
	}

	public static PrivilegedSessionProvider privilegedSessionProvider;

	private Session askForSessionWithPermissionToReadSchema(Session session, Table view, String schemaID, String schema, String tableName, ExecutionContext executionContext) {
		if (privilegedSessionProvider == null) {
			return null;
		}

		if (Quoting.equalsIgnoreQuotingAndCase(session.getIntrospectionSchema(), schema)) {
			return null;
		}

		Session newSession = privilegedSessionProvider.askForSessionWithPermissionToReadSchema(session, view, schema, tableName, executionContext);

		if (newSession != null) {
			Session s = sessionWithPermissionToReadSchema.get(schemaID);
			if (s != null) {
				try {
					s.shutDown();
				} catch (Exception e) {
					// ignore
				}
				sessionWithPermissionToReadSchema.remove(schemaID);
			}
			sessionWithPermissionToReadSchema.put(schemaID, newSession);
		}

		return newSession;
	}

	private boolean checkReadPermission(Session theSession, String schema) {
		return false;
	}

	/**
	 * Gets description.
	 */
	@Override
	public String toString() {
		return "JDBC based model element finder";
	}

}

