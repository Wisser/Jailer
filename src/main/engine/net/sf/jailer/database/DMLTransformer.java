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

package net.sf.jailer.database;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.SQLXML;
import java.sql.Types;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.Session.AbstractResultSetReader;
import net.sf.jailer.database.Session.ResultSetReader;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Filter;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.subsetting.TransformerFactory;
import net.sf.jailer.util.Base64;
import net.sf.jailer.util.CellContentConverter;
import net.sf.jailer.util.PrintUtil;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlScriptExecutor;
import net.sf.jailer.util.SqlUtil;

/**
 * A {@link ResultSetReader} that writes the read rows as DML-statements
 * into the export-script.
 *
 * @author Ralf Wisser
 */
public class DMLTransformer extends AbstractResultSetReader {

	/**
	 * The table to read from.
	 */
	private final Table table;

	/**
	 * The file to write to.
	 */
	private final OutputStreamWriter scriptFileWriter;

	/**
	 * Number of columns.
	 */
	private int columnCount;

	/**
	 * Labels of columns.
	 */
	private String[] columnLabel = null;

	/**
	 * Lob columns.
	 */
	private List<String> lobColumns = null;
	private boolean[] isLobColumn;

	/**
	 * Literals for empty lob values.
	 */
	private String[] emptyLobValue = null;

	/**
	 * Whether or not the table has columns of type CLOB or BLOB.
	 */
	private boolean tableHasLobs = false;

	/**
	 * Lob columns indexes.
	 */
	private List<Integer> lobColumnIndexes = null;

	/**
	 * Labels of columns as comma separated list.
	 */
	private String labelCSL;

	/**
	 * For building compact insert-statements.
	 */
	private final StatementBuilder insertStatementBuilder;

	/**
	 * For building compact update-statements.
	 */
	private final StatementBuilder updateStatementBuilder;

	/**
	 * Terminator of current {@link #updateStatementBuilder}
	 */
	private String updateStatementBuilderTerminator;

	/**
	 * For building compact insert-parts of upsert-statements.
	 */
	private Map<String, StatementBuilder> upsertInsertStatementBuilder = new HashMap<String, StatementBuilder>();

	/**
	 * Whether to create INSERTs or UPSERTS for all entities.
	 */
	private final boolean upsertOnly;

	/**
	 * Maximum length of SQL values list (for generated inserts).
	 */
	private final int maxBodySize;

	/**
	 * Counts the exported LOBs. (GUI support)
	 */
	public static final AtomicLong numberOfExportedLOBs = new AtomicLong();

	/**
	 * For quoting of column names.
	 */
	protected final Quoting quoting;

	/**
	 * If table has identity column (MSSQL/Sybase/Postgres)
	 */
	private boolean tableHasIdentityColumn;

	/**
	 * Current session;
	 */
	private final Session session;

	/**
	 * Configuration of the target DBMS.
	 */
	private final DBMS targetDBMSConfiguration;

	/**
	 * SQL Dialect.
	 */
	private final SQLDialect currentDialect;

	/**
	 * The execution context.
	 */
	private final ExecutionContext executionContext;

	/**
	 * Transforms {@link Filter} into SQL-expressions.
	 */
	private final ImportFilterTransformer importFilterTransformer;

	private final Set<String> primaryKeyColumnNames;
	private final Set<String> nullableColumnNames;

	/**
	 * Factory.
	 */
	public static class Factory implements TransformerFactory {

		private final int maxBodySize;
		private final boolean upsertOnly;
		private final OutputStreamWriter scriptFileWriter;
		private final Session session;
		private final DBMS targetDBMSConfiguration;
		private ImportFilterTransformer importFilterTransformer;
		private final AtomicReference<Table> identityInsertTable = new AtomicReference<Table>();

		/**
		 * The execution context.
		 */
		private final ExecutionContext executionContext;

		/**
		 * Constructor.
		 *
		 * @param scriptFileWriter the file to write to
		 * @param maxBodySize maximum length of SQL values list (for generated inserts)
		 * @param upsertOnly use 'upsert' statements for all entities
		 */
		public Factory(OutputStreamWriter scriptFileWriter, boolean upsertOnly, int maxBodySize, Session session, DBMS targetDBMSConfiguration, ExecutionContext executionContext) {
			this.maxBodySize = maxBodySize;
			this.upsertOnly = upsertOnly;
			this.scriptFileWriter = scriptFileWriter;
			this.session = session;
			this.targetDBMSConfiguration = targetDBMSConfiguration;
			this.executionContext = executionContext;
		}

		/**
		 * Creates transformer (as {@link ResultSetReader} which
		 * transforms rows of a given table into an external representation.
		 *
		 * @param table the table
		 * @return a transformer
		 */
		@Override
		public ResultSetReader create(Table table) throws SQLException {
			return new DMLTransformer(table, scriptFileWriter, upsertOnly, maxBodySize, session, targetDBMSConfiguration, importFilterTransformer, identityInsertTable, executionContext);
		}

		/**
		 * Sets the {@link ImportFilterTransformer}.
		 */
		public void setImportFilterTransformer(ImportFilterTransformer importFilterManager) {
			this.importFilterTransformer = importFilterManager;
		}

	}

	private final List<Column> selectionClause;

	/**
	 * Constructor.
	 *
	 * @param table the table to read from
	 * @param scriptFileWriter the file to write to
	 * @param maxBodySize maximum length of SQL values list (for generated inserts)
	 * @param upsertOnly use 'upsert' statements for all entities
	 * @param session the session
	 * @param targetDBMSConfiguration configuration of the target DBMS
	 * @param executionContext
	 */
	protected DMLTransformer(Table table, OutputStreamWriter scriptFileWriter, boolean upsertOnly, int maxBodySize, Session session, DBMS targetDBMSConfiguration, ImportFilterTransformer importFilterTransformer, AtomicReference<Table> identityInsertTable, ExecutionContext executionContext) throws SQLException {
		this.executionContext = executionContext;
		this.targetDBMSConfiguration = targetDBMSConfiguration;
		this.maxBodySize = maxBodySize;
		this.upsertOnly = upsertOnly;
		this.table = table;
		this.scriptFileWriter = scriptFileWriter;
		this.currentDialect = targetDBMSConfiguration.getSqlDialect();
		this.insertStatementBuilder = new StatementBuilder(currentDialect.isSupportsMultiRowInserts() || DBMS.ORACLE.equals(targetDBMSConfiguration) || DBMS.SQLITE.equals(targetDBMSConfiguration)? maxBodySize : 1);
		this.updateStatementBuilder = new StatementBuilder(maxBodySize);
		this.quoting = createQuoting(session);
		this.importFilterTransformer = importFilterTransformer;
		if (targetDBMSConfiguration != session.dbms) {
			if (targetDBMSConfiguration.getIdentifierQuoteString() != null) {
				this.quoting.setIdentifierQuoteString(targetDBMSConfiguration.getIdentifierQuoteString());
			}
		}
		this.identityInsertTable = identityInsertTable;
		this.session = session;
		this.selectionClause = table.getSelectionClause();
		tableHasIdentityColumn = false;
		for (Column c: table.getColumns()) {
			if (c.isIdentityColumn) {
				tableHasIdentityColumn = true;
				break;
			}
		}
		this.primaryKeyColumnNames = new HashSet<String>();
		this.nullableColumnNames = new HashSet<String>();
		for (Column c: table.getNonVirtualPKColumns(session)) {
			this.primaryKeyColumnNames.add(c.name.toUpperCase(Locale.ENGLISH));
		}
		for (Column c: table.getColumns()) {
			if (c.isNullable) {
				this.nullableColumnNames.add(c.name.toUpperCase(Locale.ENGLISH));
			}
		}
	}

	protected Quoting createQuoting(Session session) throws SQLException {
		return Quoting.getQuoting(session);
	}

	private Map<Integer, String> columnTypeFromDatamodel = new HashMap<Integer, String>();

	/**
	 * Reads result-set and writes into export-script.
	 */
	@Override
	public void readCurrentRow(ResultSet resultSet) throws SQLException {
		if (columnLabel == null) {
			columnCount = getMetaData(resultSet).getColumnCount();
			columnLabel = new String[columnCount + 1];
			lobColumns = new ArrayList<String>();
			isLobColumn = new boolean[columnCount + 1];
			emptyLobValue = new String[columnCount + 1];
			lobColumnIndexes = new ArrayList<Integer>();
			labelCSL = "";
			tableHasLobs = false;
			for (int i = 1; i <= columnCount; ++i) {
				// TODO get rid of #getColumnLabel(int). Pass selection-clause as list of columns (for all Transformers).
				String mdColumnLabel = SqlUtil.columnLabel(quoting, session, targetDBMSConfiguration, table, getMetaData(resultSet).getColumnLabel(i));
				// int mdColumnType = getMetaData(resultSet).getColumnType(i);
				int mdColumnType = SqlUtil.getColumnType(session.dbms, resultSet, getMetaData(resultSet), i, null);
				if ((mdColumnType == Types.BLOB || mdColumnType == Types.CLOB || mdColumnType == Types.NCLOB || mdColumnType == Types.SQLXML) && !DBMS.SQLITE.equals(targetDBMSConfiguration)) {
					tableHasLobs = true;
					isLobColumn[i] = true;
					lobColumnIndexes.add(i);
					lobColumns.add(mdColumnLabel);
					if (mdColumnType == Types.SQLXML) {
						emptyLobValue[i] = null;
					} else {
						DBMS c = targetDBMSConfiguration;
						emptyLobValue[i] = mdColumnType == Types.BLOB? c.getEmptyBLOBValue() : mdColumnType == Types.CLOB? c.getEmptyCLOBValue() : c.getEmptyNCLOBValue();
					}
					if (emptyLobValue[i] == null) {
						continue;
					}
				}
				columnLabel[i] = mdColumnLabel;
				if (labelCSL.length() > 0) {
					labelCSL += ", ";
				}
				labelCSL += columnLabel[i];
			}
		}
		try {
			StringBuffer valueList = new StringBuffer("");
			StringBuffer namedValues = new StringBuffer("");
			boolean f = true;
			Map<Integer, String> smallLobsPerIndex = new HashMap<Integer, String>();
			CellContentConverter cellContentConverter = getCellContentConverter(resultSet, session, targetDBMSConfiguration);
			for (int i = 1; i <= columnCount; ++i) {
				Object content = null;
				if (columnLabel[i] == null) {
					continue;
				}
				boolean isSmallLob = false;
				if (isLobColumn[i]) {
					content = cellContentConverter.getSmallLob(resultSet, i);
					if (content != null) {
						isSmallLob = true;
						smallLobsPerIndex.put(i, (String) content);
					}
				}
				if (content == null) {
					content = cellContentConverter.getObject(resultSet, i);
					if (resultSet.wasNull()) {
						content = null;
					}
				}
				if (!f) {
					namedValues.append(", ");
					valueList.append(", ");
				}
				f = false;
				String cVal = isSmallLob? (String) content :
					 convertToSql(cellContentConverter, resultSet, i, content, 0, null);
				if (!isSmallLob && content != null && emptyLobValue[i] != null) {
					cVal = emptyLobValue[i];
				}
				valueList.append(cVal);
				namedValues.append(cVal + " " + columnLabel[i]);
			}
			String identityColumnInsertClause = tableHasIdentityColumn && session.dbms != null && session.dbms.getIdentityColumnInsertClause() != null? session.dbms.getIdentityColumnInsertClause() + " ": "";
			if (table.getUpsert() || upsertOnly) {
				if (table.getNonVirtualPKColumns(session).isEmpty()) {
					throw new DataModel.NoPrimaryKeyException(table, "has no " +
							(table.primaryKey != null && table.primaryKey.getColumns() != null && !table.primaryKey.getColumns().isEmpty()? "non-virtual " : "") +
							"primary key. Upsert statement can not be generated.");
				}

				Map<String, String> val = new HashMap<String, String>();
				Map<String, Boolean> valIsNull = new HashMap<String, Boolean>();
				StringBuffer valuesWONull = new StringBuffer("");
				StringBuffer namedValuesWONull = new StringBuffer("");
				StringBuffer columnsWONull = new StringBuffer("");
				f = true;
				boolean generateUpsertStatementsWithoutNulls = Configuration.getInstance().isGenerateUpsertStatementsWithoutNulls();
				for (int i = 1; i <= columnCount; ++i) {
					if (columnLabel[i] == null) {
						continue;
					}
					Object content = cellContentConverter.getObject(resultSet, i);
					if (resultSet.wasNull()) {
						content = null;
					}
					String suffix = null;
					if (DBMS.POSTGRESQL.equals(targetDBMSConfiguration)) {
						int mdColumnType = getMetaData(resultSet).getColumnType(i);
						if (mdColumnType == Types.TIME || (content == null && ((currentDialect.getUpdateMode() == UPDATE_MODE.PG || !generateUpsertStatementsWithoutNulls)))) {
							// explicit cast needed
							if (mdColumnType == Types.OTHER) {
								String type = null;
								if (!columnTypeFromDatamodel.containsKey(i)) {
									for (Column c: table.getColumns()) {
										if (c.name != null && c.name.equalsIgnoreCase(columnLabel[i])) {
											type = c.type;
											break;
										}
									}
									columnTypeFromDatamodel.put(i, type);
								} else {
									type = columnTypeFromDatamodel.get(i);
								}
								if (type != null) {
									suffix = "::" + type;
								}
							} else {
								suffix = "::" + getMetaData(resultSet).getColumnTypeName(i);
							}
						}
					}
					String cVal = convertToSql(cellContentConverter, resultSet, i, content, 1, suffix);
					if (content != null && emptyLobValue[i] != null) {
						cVal = smallLobsPerIndex.get(i);
						if (cVal == null) {
							cVal = emptyLobValue[i];
						}
					}
					valIsNull.put(columnLabel[i], content == null);
					val.put(columnLabel[i], cVal);
					if (content != null || !generateUpsertStatementsWithoutNulls) {
						if (!f) {
							valuesWONull.append(", ");
							namedValuesWONull.append(", ");
							columnsWONull.append(", ");
						}
						f = false;
						valuesWONull.append(cVal);
						namedValuesWONull.append(cVal);
						namedValuesWONull.append(" ");
						namedValuesWONull.append(columnLabel[i]);
						columnsWONull.append(columnLabel[i]);
					}
				}

				String insertHead = "Insert into " + qualifiedTableName(table) + "(" + columnsWONull + ") " + identityColumnInsertClause;
				f = true;
				StringBuffer whereForTerminator = new StringBuffer("");
				StringBuffer whereForTerminatorWONull = new StringBuffer("");
				StringBuffer where = new StringBuffer("");
				StringBuffer whereWOAlias = new StringBuffer("");

				// assemble 'where' for sub-select and update
				for (Column pk: table.getNonVirtualPKColumns(session)) {
					if (!f) {
						whereForTerminator.append(" and ");
						whereForTerminatorWONull.append(" and ");
						where.append(" and ");
						whereWOAlias.append(" and ");
					}
					f = false;
					String value;
					Boolean isNull;
					String name = quoting.unquote(pk.name);
					if (val.containsKey(name)) {
						value = val.get(name);
						isNull = valIsNull.get(name);
					} else if (val.containsKey(name.toLowerCase(Locale.ENGLISH))) {
						value = val.get(name.toLowerCase(Locale.ENGLISH));
						isNull = valIsNull.get(name.toLowerCase(Locale.ENGLISH));
					} else {
						value = val.get(name.toUpperCase(Locale.ENGLISH));
						isNull = valIsNull.get(name.toUpperCase(Locale.ENGLISH));
					}
					String op = Boolean.TRUE.equals(isNull)? " is null" : ("=" + value);
					where.append("T." + quoting.requote(pk.name) + op);
					whereWOAlias.append(quoting.requote(pk.name) + op);
					if (pk.isNullable) {
						whereForTerminator.append("(T." + quoting.requote(pk.name) + "=Q." + quoting.requote(pk.name));
						whereForTerminator.append(" or (T." + quoting.requote(pk.name) + " is null and Q." + quoting.requote(pk.name) + " is null))");
					} else {
						whereForTerminator.append("T." + quoting.requote(pk.name) + "=Q." + quoting.requote(pk.name));
					}
					whereForTerminatorWONull.append("T." + quoting.requote(pk.name) + (Boolean.TRUE.equals(isNull)? " is null" : ("=Q." + quoting.requote(pk.name))));
				}

				if (currentDialect.getUpsertMode() == UPSERT_MODE.MERGE && !tableHasLobs) {
					// MERGE INTO JL_TMP T USING (SELECT 1 c1, 2 c2 from dual) incoming
					// ON (T.c1 = incoming.c1)
					// WHEN MATCHED THEN UPDATE SET T.c2 = incoming.c2
					// WHEN NOT MATCHED THEN INSERT (T.c1, T.c2) VALUES (incoming.c1, incoming.c2)
					insertHead = "MERGE INTO " + qualifiedTableName(table) + " T USING(";
					StringBuffer terminator = new StringBuffer(") Q ON(" + whereForTerminator + ") ");

					StringBuffer sets = new StringBuffer();
					StringBuffer tSchema = new StringBuffer();
					StringBuffer iSchema = new StringBuffer();
					for (int i = 1; i <= columnCount; ++i) {
						if (columnLabel[i] == null) {
							continue;
						}
						if (!isPrimaryKeyColumn(columnLabel[i])) {
							if (sets.length() > 0) {
								sets.append(", ");
							}
							sets.append("T." + columnLabel[i] + "=Q." + columnLabel[i]);
						}
						if (tSchema.length() > 0) {
							tSchema.append(", ");
						}
						tSchema.append("T." + columnLabel[i]);
						if (iSchema.length() > 0) {
							iSchema.append(", ");
						}
						iSchema.append("Q." + columnLabel[i]);
					}
					if (sets.length() > 0) {
						terminator.append("WHEN MATCHED THEN UPDATE SET " + sets + " ");
					}
					terminator.append("WHEN NOT MATCHED THEN INSERT (" + tSchema + ") VALUES(" + iSchema + ");" + PrintUtil.LINE_SEPARATOR);

					StatementBuilder sb = upsertInsertStatementBuilder.get(insertHead);
					if (sb == null) {
						sb = new StatementBuilder(maxBodySize);
						upsertInsertStatementBuilder.put(insertHead, sb);
					}

					String item = "Select " + valueList + " from dual";
					if (!sb.isAppendable(insertHead)) {
						writeToScriptFile(sb.build(), true);
					}
					if (sb.isEmpty()) {
						item = "Select " + namedValues + " from dual";
					}
					sb.append(insertHead, item, " UNION ALL ", terminator.toString());
				} else if (currentDialect.getUpsertMode() == UPSERT_MODE.DB2) {
					insertHead += "Select * From (values ";
					StringBuffer terminator = new StringBuffer(") as Q(" + columnsWONull + ") Where not exists (Select * from " + qualifiedTableName(table) + " T "
							+ "Where ");
					terminator.append(whereForTerminatorWONull + ");" + PrintUtil.LINE_SEPARATOR);

					StatementBuilder sb = upsertInsertStatementBuilder.get(insertHead);
					if (sb == null) {
						sb = new StatementBuilder(maxBodySize);
						upsertInsertStatementBuilder.put(insertHead, sb);
					}

					String item = (maxBodySize > 1? PrintUtil.LINE_SEPARATOR + " " : "") + "(" + valuesWONull + ")";
					if (!sb.isAppendable(insertHead)) {
						writeToScriptFile(sb.build(), true);
					}
					sb.append(insertHead, item, ", ", terminator.toString());
				} else if (currentDialect.getUpsertMode() == UPSERT_MODE.UNION_ALL) {
					insertHead += "Select * From (" + PrintUtil.LINE_SEPARATOR + " Select ";
					StringBuffer terminator = new StringBuffer(") as Q " + PrintUtil.LINE_SEPARATOR + "Where not exists (Select * from " + qualifiedTableName(table) + " T "
							+ "Where ");
					terminator.append(whereForTerminatorWONull + ");" + PrintUtil.LINE_SEPARATOR);

					StatementBuilder sb = upsertInsertStatementBuilder.get(insertHead);
					if (sb == null) {
						sb = new StatementBuilder(maxBodySize);
						upsertInsertStatementBuilder.put(insertHead, sb);
					}

					String item;
					item = valuesWONull.toString();
					if (!sb.isAppendable(insertHead)) {
						writeToScriptFile(sb.build(), true);
					}
					if (sb.isEmpty()) {
						item = namedValuesWONull.toString();
					}
					sb.append(insertHead, item, " union all " + PrintUtil.LINE_SEPARATOR + " Select ", terminator.toString());
				} else {
					String item = "Select " + valuesWONull + " From " +
						(currentDialect.getUpsertMode() == UPSERT_MODE.FROM_DUAL ||
						 currentDialect.getUpsertMode() == UPSERT_MODE.MERGE? // oracle table with lobs
								 "dual" : currentDialect.getUpsertMode() == UPSERT_MODE.FROM_SYSDUMMY1? "sysibm.sysdummy1" : SQLDialect.DUAL_TABLE);
					StringBuffer terminator = new StringBuffer(" Where not exists (Select * from " + qualifiedTableName(table) + " T "
							+ "Where ");
					terminator.append(where + ");" + PrintUtil.LINE_SEPARATOR);

					StatementBuilder sb = upsertInsertStatementBuilder.get(insertHead);
					if (sb == null) {
						sb = new StatementBuilder(1 /* insertStatementBuilder.getMaxBodySize() */);
						upsertInsertStatementBuilder.put(insertHead, sb);
					}

					if (!sb.isAppendable(insertHead)) {
						writeToScriptFile(sb.build(), true);
					}
					sb.append(insertHead, item, ", ", terminator.toString());
				}

				// TODO refactoring, method is too long

				if (currentDialect.getUpsertMode() != UPSERT_MODE.MERGE || tableHasLobs) {
					if (currentDialect.getUpdateMode() == UPDATE_MODE.PG && DBMS.POSTGRESQL.equals(session.dbms)) {
						StringBuilder item = new StringBuilder(" (");
						StringBuilder terminator = new StringBuilder(") Q(");
						StringBuilder head = new StringBuilder("Update " + qualifiedTableName(table) + " T set ");
						StringBuilder set = new StringBuilder();
						f = true;
						for (int i = 1; i <= columnCount; ++i) {
							if (columnLabel[i] == null || (emptyLobValue[i] != null && !valIsNull.get(columnLabel[i]))) {
								continue;
							}
							if (!isPrimaryKeyColumn(columnLabel[i])) {
								if (set.length() > 0) {
									set.append(", ");
								}
								set.append(columnLabel[i] + "=Q." + columnLabel[i]);
							}
							if (!f) {
								terminator.append(", ");
								item.append(", ");
							}
							f = false;
							terminator.append(columnLabel[i]);
							item.append(val.get(columnLabel[i]));
						}
						head.append(set).append(PrintUtil.LINE_SEPARATOR + "From (values" + PrintUtil.LINE_SEPARATOR);
						item.append(")");
						if (set.length() > 0) {
							String headAsString = head.toString();
							String itemAsString = item.toString();
							if (!updateStatementBuilder.isAppendable(headAsString)) {
								writeToScriptFile(updateStatementBuilder.build(), true);
							}
							updateStatementBuilder.append(
									headAsString,
									itemAsString,
									", " + PrintUtil.LINE_SEPARATOR,
									terminator + ")" + PrintUtil.LINE_SEPARATOR + "Where " + whereForTerminator + ";" + PrintUtil.LINE_SEPARATOR);
						}
					} else if (currentDialect.getUpdateMode() == UPDATE_MODE.MYSQL || currentDialect.getUpdateMode() == UPDATE_MODE.MSSQL) {
						boolean ms = currentDialect.getUpdateMode() == UPDATE_MODE.MSSQL;
						StringBuilder item = new StringBuilder(" Select ");
						StringBuilder terminator = new StringBuilder();
						StringBuilder columns = new StringBuilder();
						StringBuilder set = new StringBuilder();
						StringBuilder head = new StringBuilder(
								ms? ("Update T ")
								  : ("Update " + qualifiedTableName(table) + " T join (" + PrintUtil.LINE_SEPARATOR));
						f = true;
						boolean tf = true;
						boolean withLabel = !updateStatementBuilder.isAppendable(head.toString());
						for (int i = 1; i <= columnCount; ++i) {
							if (columnLabel[i] == null || (emptyLobValue[i] != null && !valIsNull.get(columnLabel[i]))) {
								continue;
							}
							if (!f) {
								item.append(", ");
								columns.append(", ");
							}
							f = false;
							if (isPrimaryKeyColumn(columnLabel[i])) {
								if (!tf) {
									terminator.append(" and ");
								}
								tf = false;
								if (isNullableColumn(columnLabel[i])) {
									terminator.append("(T." + columnLabel[i] + "=Q." + columnLabel[i]);
									terminator.append(" or (T." + columnLabel[i] + " is null and Q." + columnLabel[i] + " is null))");
								} else {
									terminator.append("T." + columnLabel[i] + "=Q." + columnLabel[i]);
								}
							} else {
								if (set.length() > 0) {
									set.append(", ");
								}
								set.append("T." + columnLabel[i] + "=Q." + columnLabel[i]);
							}
							item.append(val.get(columnLabel[i]));
							if (!ms && updateStatementBuilder.isEmpty() || withLabel) {
								item.append(" " + columnLabel[i]);
							}
							if (ms) {
								columns.append(columnLabel[i]);
							}
						}
						if (set.length() > 0) {
							if (ms) {
								head.append("set " + set).append(PrintUtil.LINE_SEPARATOR).append("from " + qualifiedTableName(table) + " T join (").append(PrintUtil.LINE_SEPARATOR);
							}
							String headAsString = head.toString();
							String itemAsString = item.toString();
							if (!ms) {
								terminator.append(PrintUtil.LINE_SEPARATOR);
								terminator.append("set " + set);
							}
							terminator.append(";").append(PrintUtil.LINE_SEPARATOR);
							String terminatorAsString = (ms? (") Q(" + columns + ") on ") : ") Q on ") + terminator.toString();
							if (!terminatorAsString.equals(updateStatementBuilderTerminator) || !updateStatementBuilder.isAppendable(headAsString)) {
								writeToScriptFile(updateStatementBuilder.build(), true);
							}
							updateStatementBuilderTerminator = terminatorAsString;
							updateStatementBuilder.append(
									headAsString,
									itemAsString,
									" union all " + PrintUtil.LINE_SEPARATOR,
									terminatorAsString);
						}
					} else {
						StringBuffer update = new StringBuffer("");
						update.append("Update " + qualifiedTableName(table) + " set ");
						f = true;
						for (int i = 1; i <= columnCount; ++i) {
							if (columnLabel[i] == null || (emptyLobValue[i] != null && !valIsNull.get(columnLabel[i]))) {
								continue;
							}
							if (isPrimaryKeyColumn(columnLabel[i])) {
								continue;
							}
							if (!f) {
								update.append(", ");
							}
							f = false;
							update.append(columnLabel[i] + "=" + val.get(columnLabel[i]));
						}
						if (!f) {
							update.append(" Where " + whereWOAlias + ";" + PrintUtil.LINE_SEPARATOR);
							writeToScriptFile(update.toString(), true);
						}
					}
				}
			} else {
				if (DBMS.DB2_ZOS.equals(targetDBMSConfiguration) && maxBodySize > 1) {
					String insertSchema = "Insert into " + qualifiedTableName(table) + "(" + labelCSL + ") " + identityColumnInsertClause;
					String item = PrintUtil.LINE_SEPARATOR + " Select " + valueList + " From sysibm.sysdummy1";
					if (!insertStatementBuilder.isAppendable(insertSchema)) {
						writeToScriptFile(insertStatementBuilder.build(), true);
					}
					insertStatementBuilder.append(insertSchema, item, " Union all ", ";" + PrintUtil.LINE_SEPARATOR);
				} else if (DBMS.ORACLE.equals(targetDBMSConfiguration) && maxBodySize > 1) {
					String insertSchema = "Insert into " + qualifiedTableName(table) + "(" + labelCSL + ") " + identityColumnInsertClause;
					if (!insertStatementBuilder.isAppendable(insertSchema)) {
						writeToScriptFile(insertStatementBuilder.build(), true);
					}
					String item;
					if (insertStatementBuilder.isEmpty()) {
						item = PrintUtil.LINE_SEPARATOR + " Select " + namedValues + " From DUAL";
					} else {
						item = PrintUtil.LINE_SEPARATOR + " Select " + valueList + " From DUAL";
					}
					insertStatementBuilder.append(insertSchema, item, " Union all ", ";" + PrintUtil.LINE_SEPARATOR);
				} else if (DBMS.SQLITE.equals(targetDBMSConfiguration) && maxBodySize > 1) {
					String insertSchema = "Insert into " + qualifiedTableName(table) + "(" + labelCSL + ") " + identityColumnInsertClause;
					String item = PrintUtil.LINE_SEPARATOR + " Select " + valueList + " ";
					if (!insertStatementBuilder.isAppendable(insertSchema)) {
						writeToScriptFile(insertStatementBuilder.build(), true);
					}
					insertStatementBuilder.append(insertSchema, item, " Union all ", ";" + PrintUtil.LINE_SEPARATOR);
				} else {
					String insertSchema = "Insert into " + qualifiedTableName(table) + "(" + labelCSL + ") " + identityColumnInsertClause + "values ";
					String item = (maxBodySize > 1? PrintUtil.LINE_SEPARATOR + " " : "") + "(" + valueList + ")";
					if (!insertStatementBuilder.isAppendable(insertSchema)) {
						writeToScriptFile(insertStatementBuilder.build(), true);
					}
					insertStatementBuilder.append(insertSchema, item, ", ", ";" + PrintUtil.LINE_SEPARATOR);
				}
			}

			exportLobs(table, resultSet, smallLobsPerIndex.keySet());
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * Converts cell content to SQL literals.
	 *
	 * @param cellContentConverter converter
	 * @param resultSet points to current row
	 * @param i current result set index
	 * @param content cell content
	 * @return SQL literal
	 */
	protected String convertToSql(CellContentConverter cellContentConverter, ResultSet resultSet, int i, Object content, int callerId, String suffix) throws SQLException {
		String cVal = cellContentConverter.toSql(content);
		if (i > selectionClause.size()) {
			throw new IllegalStateException("Table \"" + table.getName() + "\": Too many columns in the result set. Check the filter definitions.");
		}
		Column column = selectionClause.get(i - 1);
		Filter filter = column.getFilter();

		if (filter != null && importFilterTransformer != null) {
			if (!filter.isApplyAtExport()) {
				return importFilterTransformer.transform(column, cVal);
			}
		}

		if (content != null && filter != null && filter.getExpression().trim().startsWith(Filter.LITERAL_PREFIX)) {
			return content.toString();
		}

		if (suffix != null) {
			cVal += suffix;
		}

		if (filter != null && filter.getReason() != null) {
			if (!seen.containsKey(callerId)) {
				seen.put(callerId, new IdentityHashMap<Filter, Filter>());
			}
			if (!seen.get(callerId).containsKey(filter)) {
				seen.get(callerId).put(filter, filter);
				return cVal + " /*" + filter.getReason() + "*/";
			}
		}

		return cVal;
	}

	private Map<Integer, IdentityHashMap<Filter, Filter>> seen = new HashMap<Integer, IdentityHashMap<Filter,Filter>>();

	/**
	 * Gets qualified table name.
	 *
	 * @param t the table
	 * @return qualified name of t
	 */
	protected String qualifiedTableName(Table t) {
		String schema = t.getOriginalSchema("");
		String mappedSchema = executionContext.getSchemaMapping().get(schema);
		if (mappedSchema != null) {
			schema = mappedSchema;
		}
		if (schema.length() == 0) {
			return quoting.requote(t.getUnqualifiedName());
		}
		return quoting.requote(schema) + "." + quoting.requote(t.getUnqualifiedName());
	}

	/**
	 * Checks if columns is part of primary key.
	 *
	 * @param column the column
	 * @return <code>true</code> if column is part of primary key
	 */
	private boolean isPrimaryKeyColumn(String column) {
		return primaryKeyColumnNames.contains(column.toUpperCase(Locale.ENGLISH));
	}

	private boolean isNullableColumn(String column) {
		return nullableColumnNames.contains(column.toUpperCase(Locale.ENGLISH));
	}

	/**
	 * Exports the (c|b)lob content.
	 *
	 * @param resultSet export current row
	 */
	private void exportLobs(Table table, ResultSet resultSet, Set<Integer> smallLobsIndexes) throws IOException, SQLException {
		synchronized (scriptFileWriter) {
			CellContentConverter cellContentConverter = getCellContentConverter(resultSet, session, targetDBMSConfiguration);
			for (int i = 0; i < lobColumnIndexes.size(); ++i) {
				if (smallLobsIndexes.contains(lobColumnIndexes.get(i))) {
					continue;
				}
				Object lob = cellContentConverter.getObject(resultSet, lobColumnIndexes.get(i));
				Map<String, String> val = new HashMap<String, String>();
				for (int j = 1; j <= columnCount; ++j) {
					if (columnLabel[j] == null) {
						continue;
					}
					Object content = cellContentConverter.getObject(resultSet, j);
					if (resultSet.wasNull()) {
						content = null;
					}
					String cVal = cellContentConverter.toSql(content);
					val.put(columnLabel[j], cVal);
				}
				boolean f = true;
				StringBuffer where = new StringBuffer("");
				if (table.getNonVirtualPKColumns(session).isEmpty()) {
					throw new DataModel.NoPrimaryKeyException(table, "has no " +
							(table.primaryKey != null && table.primaryKey.getColumns() != null && !table.primaryKey.getColumns().isEmpty()? "non-virtual " : "") +
							"primary key. Update statement to import CLOB/BLOB/XML can not be generated.");
				}
				for (Column pk: table.getNonVirtualPKColumns(session)) {
					if (!f) {
						where.append(" and ");
					}
					f = false;
					where.append(quoting.requote(pk.name) + "=" + val.get(pk.name));
				}
				if (lob instanceof SQLXML) {
					numberOfExportedLOBs.incrementAndGet();
					flush();
					SQLXML xml = (SQLXML) lob;
					writeToScriptFile(SqlScriptExecutor.UNFINISHED_MULTILINE_COMMENT + "XML " + qualifiedTableName(table) + ", " + lobColumns.get(i) + ", " + where + PrintUtil.LINE_SEPARATOR, false);
					Reader in = xml.getCharacterStream();
					int c;
					StringBuffer line = new StringBuffer(SqlScriptExecutor.UNFINISHED_MULTILINE_COMMENT);
					while ((c = in.read()) != -1) {
						if ((char) c == '\n') {
							writeToScriptFile(line.toString() + "\\n" + PrintUtil.LINE_SEPARATOR, false);
							line = new StringBuffer(SqlScriptExecutor.UNFINISHED_MULTILINE_COMMENT);
						} else {
							if ((char) c == '\r') {
								line.append("\\r");
							} else {
								line.append((char) c);
								if ((char) c == '\\') {
									line.append((char) c);
								}
							}
						}
						if (line.length() >= 200) {
							writeToScriptFile(line.toString() + PrintUtil.LINE_SEPARATOR, false);
							line = new StringBuffer(SqlScriptExecutor.UNFINISHED_MULTILINE_COMMENT);
						}
					}
					in.close();
					writeToScriptFile(line.toString() + PrintUtil.LINE_SEPARATOR + SqlScriptExecutor.FINISHED_MULTILINE_COMMENT + "" + PrintUtil.LINE_SEPARATOR + "", false);
				}
				if (lob instanceof Clob) {
					numberOfExportedLOBs.getAndIncrement();
					flush();
					Clob clob = (Clob) lob;
					writeToScriptFile(SqlScriptExecutor.UNFINISHED_MULTILINE_COMMENT + "CLOB " + qualifiedTableName(table) + ", " + lobColumns.get(i) + ", " + where + "" + PrintUtil.LINE_SEPARATOR + "", false);
					Reader in = clob.getCharacterStream();
					int c;
					StringBuffer line = new StringBuffer(SqlScriptExecutor.UNFINISHED_MULTILINE_COMMENT);
					while ((c = in.read()) != -1) {
						if ((char) c == '\n') {
							writeToScriptFile(line.toString() + "\\n" + PrintUtil.LINE_SEPARATOR, false);
							line = new StringBuffer(SqlScriptExecutor.UNFINISHED_MULTILINE_COMMENT);
						} else {
							if ((char) c == '\r') {
								line.append("\\r");
							} else {
								line.append((char) c);
								if ((char) c == '\\') {
									line.append((char) c);
								}
							}
						}
						if (line.length() >= 200) {
							writeToScriptFile(line.toString() + PrintUtil.LINE_SEPARATOR, false);
							line = new StringBuffer(SqlScriptExecutor.UNFINISHED_MULTILINE_COMMENT);
						}
					}
					in.close();
					writeToScriptFile(line.toString() + PrintUtil.LINE_SEPARATOR + SqlScriptExecutor.FINISHED_MULTILINE_COMMENT + "" + PrintUtil.LINE_SEPARATOR + "", false);
				}
				if (lob instanceof Blob) {
					numberOfExportedLOBs.getAndIncrement();
					flush();
					Blob blob = (Blob) lob;
					writeToScriptFile(SqlScriptExecutor.UNFINISHED_MULTILINE_COMMENT + "BLOB " + qualifiedTableName(table) + ", " + lobColumns.get(i) + ", " + where + "" + PrintUtil.LINE_SEPARATOR + "", false);
					InputStream in = blob.getBinaryStream();
					int b;
					StringBuffer line = new StringBuffer(SqlScriptExecutor.UNFINISHED_MULTILINE_COMMENT);
					byte[] buffer = new byte[64];
					int size = 0;
					while ((b = in.read()) != -1) {
						buffer[size++] = (byte) b;
						if (size == buffer.length) {
							writeToScriptFile(line.toString() + Base64.encodeBytes(buffer, Base64.DONT_BREAK_LINES) + PrintUtil.LINE_SEPARATOR, false);
							line = new StringBuffer(SqlScriptExecutor.UNFINISHED_MULTILINE_COMMENT);
							size = 0;
						}
					}
					in.close();
					writeToScriptFile(line.toString() + Base64.encodeBytes(buffer, 0, size, Base64.DONT_BREAK_LINES) + PrintUtil.LINE_SEPARATOR + SqlScriptExecutor.FINISHED_MULTILINE_COMMENT + "" + PrintUtil.LINE_SEPARATOR + "", false);
				}
			}
		}
	}

	/**
	 * Flushes the export-reader.
	 */
	public void flush() {
		try {
			writeToScriptFile(insertStatementBuilder.build(), true);
			writeToScriptFile(updateStatementBuilder.build(), true);
			for (StatementBuilder sb: upsertInsertStatementBuilder.values()) {
				writeToScriptFile(sb.build(), true);
			}
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * Flushes the export-reader.
	 */
	@Override
	public void close() {
		flush();
		synchronized (scriptFileWriter) {
			if (identityInsertTable.get() != null) {
				try {
					scriptFileWriter.write("SET IDENTITY_INSERT " + qualifiedTableName(identityInsertTable.get()) + " OFF;" + PrintUtil.LINE_SEPARATOR);
				} catch (IOException e) {
					throw new RuntimeException(e);
				}
				identityInsertTable.set(null);
			}
		}
	}

	/**
	 * Table which is currently enabled for identity-inserts.
	 */
	private final AtomicReference<Table> identityInsertTable;

	/**
	 * Writes into script.
	 */
	private void writeToScriptFile(String content, boolean wrap) throws IOException {
		synchronized (scriptFileWriter) {
			if (targetDBMSConfiguration.isIdentityInserts() && tableHasIdentityColumn) {
				if (identityInsertTable.get() != table) {
					if (identityInsertTable.get() != null) {
						scriptFileWriter.write("SET IDENTITY_INSERT " + qualifiedTableName(identityInsertTable.get()) + " OFF;" + PrintUtil.LINE_SEPARATOR);
						identityInsertTable.set(null);
					}
					scriptFileWriter.write("SET IDENTITY_INSERT " + qualifiedTableName(table) + " ON;" + PrintUtil.LINE_SEPARATOR);
					identityInsertTable.set(table);
				}
			}
			if (wrap && DBMS.ORACLE.equals(targetDBMSConfiguration)) {
				   scriptFileWriter.write(SqlUtil.splitDMLStatement(content, 2400));
			} else {
				scriptFileWriter.write(content);
			}
		}
	}

}
