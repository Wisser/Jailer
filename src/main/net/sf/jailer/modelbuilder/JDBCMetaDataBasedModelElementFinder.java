/*
 * Copyright 2007 - 2016 the original author or authors.
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

import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import net.sf.jailer.CommandLineParser;
import net.sf.jailer.Configuration;
import net.sf.jailer.database.DBMS;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Cardinality;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.PrimaryKey;
import net.sf.jailer.datamodel.PrimaryKeyFactory;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.CancellationHandler;
import net.sf.jailer.util.Pair;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlUtil;

import org.apache.log4j.Logger;

/**
 * Finds associations and tables by analyzing the JDBC meta data.
 * 
 * @author Ralf Wisser
 */
public class JDBCMetaDataBasedModelElementFinder implements ModelElementFinder {

    /**
     * The logger.
     */
    private static final Logger _log = Logger.getLogger(JDBCMetaDataBasedModelElementFinder.class);

    /**
     * Set of sql types (uppercase) not listed in {@link Types} which needs a length argument.
     */
    private final Set<String> typesWithLength = new HashSet<String>();
    {
    	typesWithLength.add("NVARCHAR2");
    	typesWithLength.add("NVARCHAR");
    	typesWithLength.add("NCHAR");
    	typesWithLength.add("RAW");
    }
    
    /**
     * Finds associations by reading the databases meta-data.
     * 
     * @param session the statement executor for executing SQL-statements 
     * @param dataModel model containing already known elements
     * @param namingSuggestion to put naming suggestions for associations into
     * @return found associations
     */
    public Collection<Association> findAssociations(DataModel dataModel, Map<Association, String[]> namingSuggestion, Session session) throws Exception {
        Collection<Association> associations = new ArrayList<Association>();
        DatabaseMetaData metaData = session.getMetaData();
    	Quoting quoting = new Quoting(session, true);
    	ResultSet resultSet;
        String defaultSchema = getDefaultSchema(session, session.dbUser);
         
        for (Table table: dataModel.getTables()) {
            _log.info("find associations with " + table.getName());
        	try {
        		resultSet = getImportedKeys(session, metaData, quoting.unquote(table.getOriginalSchema(quoting.quote(defaultSchema))), quoting.unquote(table.getUnqualifiedName()));
        	} catch (Exception e) {
        		_log.info("failed. " + e.getMessage());
            	continue;
        	}
            Map<String, Association> fkMap = new HashMap<String, Association>();
            while (resultSet.next()) {
                Table pkTable = dataModel.getTable(toQualifiedTableName(quoting.quote(defaultSchema), quoting.quote(resultSet.getString(session.dbms == DBMS.MySQL? 1 : 2)), quoting.quote(resultSet.getString(3))));
                String pkColumn = quoting.quote(resultSet.getString(4));
                Table fkTable = dataModel.getTable(toQualifiedTableName(quoting.quote(defaultSchema), quoting.quote(resultSet.getString(6)), quoting.quote(resultSet.getString(7))));
                String fkColumn = quoting.quote(resultSet.getString(8));
                String foreignKey = resultSet.getString(12);
                if (fkTable != null) {
	                String fkName = fkTable.getName() + "." + foreignKey;
	                if (foreignKey != null && fkMap.containsKey(fkName)) {
	                	fkMap.get(fkName).appendCondition("A." + fkColumn + "=B." + pkColumn);
	                } else {
		                if (pkTable != null && fkTable != null) {
		                    Association association = new Association(fkTable, pkTable, false, true, "A." + fkColumn + "=B." + pkColumn, dataModel, false, Cardinality.MANY_TO_ONE);
		                    association.setAuthor(metaData.getDriverName());
		                    associations.add(association);
		                    fkMap.put(fkName, association);
		                    if (foreignKey != null) {
		                    	namingSuggestion.put(association, new String[] { foreignKey, fkTable.getUnqualifiedName() + "." + foreignKey });
		                    }
		                }
	                }
                }
            }
            resultSet.close();
            CancellationHandler.checkForCancellation(null);
        }
        return associations;
    }

    private ResultSet getImportedKeys(Session session, DatabaseMetaData metaData, String schema, String table) throws SQLException {
		if (session.dbms == DBMS.MySQL) {
	    	return metaData.getImportedKeys(schema, null, table);
		}
    	return metaData.getImportedKeys(null, schema, table);
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
    
    /**
     * Finds all tables in DB schema.
     * 
     * @param session the statement executor for executing SQL-statements 
     */
    public Set<Table> findTables(Session session) throws Exception {
        PrimaryKeyFactory primaryKeyFactory = new PrimaryKeyFactory();
        
        Set<Table> tables = new HashSet<Table>();
        DatabaseMetaData metaData = session.getMetaData();
        Quoting quoting = new Quoting(session, true);
        ResultSet resultSet;
        resultSet = getTables(session, metaData, session.getIntrospectionSchema(), "%", new String[] { "TABLE" });
        List<String> tableNames = new ArrayList<String>();
        while (resultSet.next()) {
            String tableName = resultSet.getString(3);
            if ("TABLE".equalsIgnoreCase(resultSet.getString(4))) {
                if (isValidName(tableName)) {
                	tableName = quoting.quote(tableName);
                	if (CommandLineParser.getInstance().qualifyNames) {
                		String schemaName = resultSet.getString(session.dbms == DBMS.MySQL? 1 : 2);
                		if (schemaName != null) {
                			schemaName = quoting.quote(schemaName.trim());
                			if (schemaName.length() > 0) {
                				tableName = schemaName + "." + tableName;
                			}
                		}
                	}
                	tableNames.add(tableName);
                	_log.info("found table " + tableName);
                } else {
                	_log.info("skip table " + tableName);
                }
            }
            CancellationHandler.checkForCancellation(null);
        }
        resultSet.close();
        Map<String, Map<Integer, Column>> pkColumns = new HashMap<String, Map<Integer, Column>>();
        for (String tableName: tableNames) {
        	Table tmp = new Table(tableName, null, false);
            resultSet = getPrimaryKeys(session, metaData, quoting.unquote(tmp.getOriginalSchema(quoting.quote(session.getIntrospectionSchema()))), quoting.unquote(tmp.getUnqualifiedName()));
            Map<Integer, Column> pk = pkColumns.get(tableName);
            if (pk == null) {
                pk = new HashMap<Integer, Column>();
                pkColumns.put(tableName, pk);
            }
            boolean hasPK = false;
            int nextKeySeq = 0;
            while (resultSet.next()) {
            	hasPK = true;
            	int keySeq = resultSet.getInt(5);
            	if (session.dbms == DBMS.SQLITE) {
            		// SQlite driver doesn't return the keySeq
            		keySeq = nextKeySeq++;
            	}
                pk.put(keySeq, new Column(quoting.quote(resultSet.getString(4)), "", 0, -1));
            }
            if (!hasPK) {
            	_log.info("find unique index of table " + tableName);
                hasPK = findUniqueIndexBasedKey(metaData, quoting, session, tmp, pk);
            }
            if (!hasPK) {
            	_log.info("find unique index of table " + tableName);
                hasPK = findUniqueIndexBasedKey(metaData, quoting, session, tmp, pk);
            }
            _log.info((hasPK? "" : "no ") + "primary key found for table " + tableName);
            resultSet.close();
            CancellationHandler.checkForCancellation(null);
        }
        for (String tableName: tableNames) {
        	Table tmp = new Table(tableName, null, false);
        	_log.info("getting columns for " + quoting.unquote(tmp.getOriginalSchema(quoting.quote(session.getIntrospectionSchema()))) + "." + quoting.unquote(tmp.getUnqualifiedName()));
        	resultSet = getColumns(session, metaData, quoting.unquote(tmp.getOriginalSchema(quoting.quote(session.getIntrospectionSchema()))), quoting.unquote(tmp.getUnqualifiedName()), "%");
        	_log.info("done");
        	Map<Integer, Column> pk = pkColumns.get(tableName);
            while (resultSet.next()) {
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
                if (typesWithLength.contains(sqlType.toUpperCase()) || type == Types.NUMERIC || type == Types.DECIMAL || type == Types.VARCHAR || type == Types.CHAR || type == Types.BINARY || type == Types.VARBINARY) {
                    length = resultSet.getInt(7);
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
                if (type == Types.DISTINCT) {
                	length = 0;
                	precision = -1;
                }
                Column column = new Column(colName, sqlType, filterLength(length, resultSet.getString(6), type, session.dbms, resultSet.getInt(7)), precision);
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
            PrimaryKey primaryKey = primaryKeyFactory.createPrimaryKey(columns);
            Table table = new Table(tableName, primaryKey, false);
            table.setAuthor(metaData.getDriverName());
            tables.add(table);
            CancellationHandler.checkForCancellation(null);
        }

        return tables;
    }

    private ResultSet getPrimaryKeys(Session session, DatabaseMetaData metaData, String schema, String table) throws SQLException {
    	if (session.dbms == DBMS.MySQL) {
        	return metaData.getPrimaryKeys(schema, null, table);
    	}
    	return metaData.getPrimaryKeys(null, schema, table);
	}

	private ResultSet getTables(Session session, DatabaseMetaData metaData, String schemaPattern, String tableNamePattern, String[] types) throws SQLException {
    	if (session.dbms == DBMS.MySQL) {
    		 return metaData.getTables(schemaPattern, null, tableNamePattern, types);
    	}
    	return metaData.getTables(null, schemaPattern, tableNamePattern, types);
	}

	/**
     * Find a key of a table based on an unique index on non-nullable columns.
     */
    private boolean findUniqueIndexBasedKey(DatabaseMetaData metaData, Quoting quoting, Session session, Table tmp, Map<Integer, Column> pk) {
    	try {
    		ResultSet resultSet = getColumns(session, metaData, quoting.unquote(tmp.getOriginalSchema(quoting.quote(session.getIntrospectionSchema()))), quoting.unquote(tmp.getUnqualifiedName()), "%");
        	
    		List<String> nonNullColumns = new ArrayList<String>();
    		boolean hasNullable = false;
    		while (resultSet.next()) {
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
    					hasNullable = true;
    				}
    			} else {
    				hasNullable = true;
    			}
    		}
    		resultSet.close();
    		
    		if (nonNullColumns.isEmpty()) {
    			return false;
    		}
    		
    		resultSet = getIndexInfo(session, metaData, quoting.unquote(tmp.getOriginalSchema(quoting.quote(session.getIntrospectionSchema()))), quoting.unquote(tmp.getUnqualifiedName()), true, true);
	        Map<String, List<String>> indexes = new TreeMap<String, List<String>>();
	        while (resultSet.next()) {
	        	String indexName = resultSet.getString(6);
	        	if (indexName == null) {
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
	        
	        if (!hasNullable) {
	        	if (nonNullColumns.size() <= 6) {
	        		for (int i = 1; i <= nonNullColumns.size(); ++i) {
	        			pk.put(i, new Column(quoting.quote(nonNullColumns.get(i - 1)), "", 0, -1));
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
    
	private ResultSet getIndexInfo(Session session, DatabaseMetaData metaData, String schema, String table, boolean unique, boolean approximate) throws SQLException {
		if (session.dbms == DBMS.MySQL) {
			return metaData.getIndexInfo(schema, null, table, unique, approximate);
		}
		return metaData.getIndexInfo(null, schema, table, unique, approximate);
	}

	/**
	 * Calls {@link DatabaseMetaData#getColumns(String, String, String, String)}. Uses schemaPattern as catalogPattern on MySQL.
	 */
	public static ResultSet getColumns(Session session, DatabaseMetaData metaData, String schemaPattern, String tableNamePattern, String columnNamePattern) throws SQLException {
		if (session.dbms == DBMS.MySQL) {
			return metaData.getColumns(schemaPattern, null, tableNamePattern, columnNamePattern);
		}
		try {
			return metaData.getColumns(null, schemaPattern, tableNamePattern, columnNamePattern);
		} catch (Exception e) {
			throw new RuntimeException("Error in getColumns(): schemaPattern=" + schemaPattern + ", tableNamePattern=" + tableNamePattern + ", columnNamePattern=" + columnNamePattern, e);
		}
	}

	/**
     * Checks syntactical correctness of names.
     * 
     * @param name a table or column name
     * @return <code>true</code> if name is syntactically correct
     */
    private boolean isValidName(String name) {
		return name != null && !name.contains("$") && !name.contains("/") && !name.contains("=");
	}

	/**
     * Finds all non-empty schemas in DB.
     * 
     * @param session the statement executor for executing SQL-statements
     * @param userName schema with this name may be empty
     */ 
    public static List<String> getSchemas(Session session, String userName) {
    	List<String> schemas = new ArrayList<String>();
		try {
			DatabaseMetaData metaData = session.getMetaData();
			ResultSet rs = session.dbms == DBMS.MySQL? metaData.getCatalogs() : metaData.getSchemas();
			while (rs.next()) {
				String schema = rs.getString(session.dbms == DBMS.MySQL? "TABLE_CAT" : "TABLE_SCHEM").trim();
				if (schema != null) {
					if (session.dbms == DBMS.POSTGRESQL && schema.startsWith("pg_toast_temp")) {
						continue;
					}
					schemas.add(schema);
				}
			}
			rs.close();
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
     * Gets default schema of DB.
     * 
     * @param session the statement executor for executing SQL-statements
     * @param userName schema with this name may be empty
     */ 
    public static String getDefaultSchema(Session session, String userName) {
    	if (session.dbms == DBMS.MySQL) {
    		try {
    			final String[] database = new String[1];
    			session.executeQuery("Select DATABASE()", new Session.AbstractResultSetReader() {
					@Override
					public void readCurrentRow(ResultSet resultSet) throws SQLException {
						database[0] = resultSet.getString(1);
					}
				});
        		if (database[0] != null) {
        			return database[0];
        		}
    		} catch (SQLException e) {
    			e.printStackTrace();
    			// ignore
    		}
    	}
    	List<String> schemas = new ArrayList<String>();
		try {
			DatabaseMetaData metaData = session.getMetaData();
			String dbName = metaData.getDatabaseProductName();
			boolean isPostgreSQL = dbName != null && dbName.toLowerCase().contains("PostgreSQL".toLowerCase());
			boolean isH2Sql = dbName != null && dbName.toLowerCase().startsWith("H2".toLowerCase());
			ResultSet rs = session.dbms == DBMS.MySQL? metaData.getCatalogs() : metaData.getSchemas();
			while (rs.next()) {
				schemas.add(rs.getString(session.dbms == DBMS.MySQL? "TABLE_CAT" : "TABLE_SCHEM"));
			}
			rs.close();
			String userSchema = null;
			for (Iterator<String> i = schemas.iterator(); i.hasNext(); ) {
				String schema = i.next().trim();
				if ((isPostgreSQL || isH2Sql) && "public".equalsIgnoreCase(schema)) {
					return schema;
				}
				if (schema.equalsIgnoreCase(userName.trim())) {
					userSchema = schema;
				}
			}
			if (userSchema != null) {
				return userSchema;
			}
			return userName;
		} catch (SQLException e) {
			e.printStackTrace();
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
    public List<Column> findColumns(Table table, Session session) throws Exception {
    	List<Column> columns = new ArrayList<Column>();
    	DatabaseMetaData metaData = session.getMetaData();
    	Quoting quoting = new Quoting(session, true);
    	if (forDefaultSchema != session) {
    		forDefaultSchema = session;
    		_log.info("getting default schema...");
    		defaultSchema = getDefaultSchema(session, session.dbUser);
    		_log.info("default schema is '" + defaultSchema + "'");
    	}
    	String schemaName = quoting.unquote(table.getOriginalSchema(defaultSchema));
    	String tableName = quoting.unquote(table.getUnqualifiedName());
		_log.info("getting columns for " + table.getOriginalSchema(defaultSchema) + "." + tableName);
		ResultSet resultSet = getColumns(session, metaData, schemaName, tableName, "%");
    	_log.info("done");
    	while (resultSet.next()) {
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
            if (typesWithLength.contains(sqlType.toUpperCase()) || type == Types.NUMERIC || type == Types.DECIMAL || type == Types.VARCHAR || type == Types.CHAR || type == Types.BINARY || type == Types.VARBINARY) {
                length = resultSet.getInt(7);
                if (type == Types.VARCHAR) {
                	if (Configuration.forDbms(session).getVarcharLengthLimit() != null) {
                		length = Math.min(length, Configuration.forDbms(session).getVarcharLengthLimit());
                	}
                }
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
            if (type == Types.DISTINCT) {
            	length = 0;
            	precision = -1;
            }
            _log.debug("column info: '" + colName + "' '" + sqlType + "' " + type + " '" + resultSet.getString(6) + "'");
            Column column = new Column(colName, sqlType, filterLength(length, resultSet.getString(6), type, session.dbms, resultSet.getInt(7)), precision);
			Boolean isVirtual = null;
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
            if (isVirtual == null) {
            	@SuppressWarnings("unchecked")
				Set<Pair<String, String>> virtualColumns = (Set<Pair<String, String>>) session.getSessionProperty(getClass(), "virtualColumns" + schemaName);
            	if (virtualColumns == null) {
            		virtualColumns = new HashSet<Pair<String,String>>();
            		String virtualColumnsQuery = Configuration.forDbms(session).virtualColumnsQuery;
            		if (virtualColumnsQuery != null) {
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
            		}
            		session.setSessionProperty(getClass(), "virtualColumns" + schemaName, virtualColumns);
            	}
        		isVirtual = virtualColumns.contains(new Pair<String, String>(tableName, resultSet.getString(4)));
            }
            if (isVirtual != null) {
            	column.isVirtual = isVirtual;
            }
            columns.add(column);
        }
        resultSet.close();
        _log.info("found columns for table " + table.getName());
        return columns;
    }

    /**
     * Filter the length attribute of a column in a DBMS specific way.
     * 
     * @param length the length as given from driver
     * @param the type name
     * @param type the sql type
     * @param dbms the DBMS
     * @return filtered length
     */
    private int filterLength(int length, String typeName, int type, DBMS dbms, int origLength) {
    	if (length > 0) {
    		if (dbms == DBMS.POSTGRESQL) {
    			if (type == Types.VARCHAR && length >= 10485760) {
    				length = 0;
    			} else if (type == Types.NUMERIC && length > 1000) {
    				length = 0;
    			} else if ("bytea".equalsIgnoreCase(typeName)) {
    				length = 0;
    			}
    		} else if (dbms == DBMS.SQLITE) {
    			return 0;
    		}
    	} else {
    		if (dbms == DBMS.POSTGRESQL) {
    			if ("bit".equalsIgnoreCase(typeName)) {
    				length = origLength;
    			}
    		}
    	}
		return length;
	}

	/**
     * Converts result from {@link DatabaseMetaData#getColumns(String, String, String, String)}
     * into the type name.
     */
    private String toSqlType(String sqlType, DBMS dbms) {
    	if (sqlType == null) {
    		return null;
    	}
    	sqlType = sqlType.trim();

    	if (dbms == DBMS.MySQL) {
    		if (sqlType.equalsIgnoreCase("SET") || sqlType.equalsIgnoreCase("ENUM")) {
    			return "VARCHAR";
    		}
    	}
    	if (!sqlType.toLowerCase().endsWith(" identity")) {
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

	/**
     * Gets description.
     */
    public String toString() {
    	return "JDBC based model element finder";
    }
    
}
