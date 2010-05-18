/*
 * Copyright 2007 the original author or authors.
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

import net.sf.jailer.CommandLineParser;
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
    	Quoting quoting = new Quoting(metaData);
    	ResultSet resultSet;
        String defaultSchema = getDefaultSchema(session, session.dbUser);
         
        for (Table table: dataModel.getTables()) {
            _log.info("find associations with " + table.getName());
        	try {
        		resultSet = metaData.getImportedKeys(null, quoting.unquote(table.getOriginalSchema(quoting.quote(defaultSchema))), quoting.unquote(table.getUnqualifiedName()));
        	} catch (Exception e) {
        		_log.info("failed. " + e.getMessage());
            	continue;
        	}
            Map<String, Association> fkMap = new HashMap<String, Association>();
            while (resultSet.next()) {
                Table pkTable = dataModel.getTable(toQualifiedTableName(quoting.quote(defaultSchema), quoting.quote(resultSet.getString(2)), quoting.quote(resultSet.getString(3))));
                String pkColumn = quoting.quote(resultSet.getString(4));
                Table fkTable = dataModel.getTable(toQualifiedTableName(quoting.quote(defaultSchema), quoting.quote(resultSet.getString(6)), quoting.quote(resultSet.getString(7))));
                String fkColumn = quoting.quote(resultSet.getString(8));
                String foreignKey = resultSet.getString(12);
                String fkName = fkTable + "." + foreignKey;
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
            resultSet.close();
            CancellationHandler.checkForCancellation();
        }
        return associations;
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
        Quoting quoting = new Quoting(metaData);
        ResultSet resultSet;
        resultSet = metaData.getTables(null, session.getIntrospectionSchema(), "%", new String[] { "TABLE" });
        List<String> tableNames = new ArrayList<String>();
        while (resultSet.next()) {
            String tableName = resultSet.getString(3);
            if ("TABLE".equalsIgnoreCase(resultSet.getString(4))) {
                if (isValidName(tableName)) {
                	tableName = quoting.quote(tableName);
                	if (CommandLineParser.getInstance().qualifyNames) {
                		String schemaName = resultSet.getString(2);
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
            CancellationHandler.checkForCancellation();
        }
        resultSet.close();
        Map<String, Map<Integer, Column>> pkColumns = new HashMap<String, Map<Integer, Column>>();
        for (String tableName: tableNames) {
        	Table tmp = new Table(tableName, null, false);
            resultSet = metaData.getPrimaryKeys(null, quoting.unquote(tmp.getOriginalSchema(quoting.quote(session.getIntrospectionSchema()))), quoting.unquote(tmp.getUnqualifiedName()));
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
            		// SQlite driver does'nt return the keySeq
            		keySeq = nextKeySeq++;
            	}
                pk.put(keySeq, new Column(quoting.quote(resultSet.getString(4)), "", 0, -1));
            }
            _log.info((hasPK? "" : "no ") + "primary key found for table " + tableName);
            resultSet.close();
            CancellationHandler.checkForCancellation();
        }
        for (String tableName: tableNames) {
        	Table tmp = new Table(tableName, null, false);
        	_log.info("getting columns for " + quoting.unquote(tmp.getOriginalSchema(quoting.quote(session.getIntrospectionSchema()))) + "." + quoting.unquote(tmp.getUnqualifiedName()));
        	resultSet = metaData.getColumns(null, quoting.unquote(tmp.getOriginalSchema(quoting.quote(session.getIntrospectionSchema()))), quoting.unquote(tmp.getUnqualifiedName()), "%");
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
            CancellationHandler.checkForCancellation();
        }

        return tables;
    }

    /**
     * Checks syntactical correctness of names.
     * 
     * @param name a table or column name
     * @return <code>true</code> if name is syntactically correct
     */
    private boolean isValidName(String name) {
		return name != null && !name.contains("$");
	}

	/**
     * Finds all non-empty schemas in DB.
     * 
     * @param session the statement executor for executing SQL-statements
     * @param userName schema with this name may be empty
     */ 
    public static List<String> getSchemas(Session session, String userName) throws Exception {
    	List<String> schemas = new ArrayList<String>();
		try {
			DatabaseMetaData metaData = session.getMetaData();
			ResultSet rs = metaData.getSchemas();
			while (rs.next()) {
				String schema = rs.getString("TABLE_SCHEM").trim();
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
		return schemas;
    }

	/**
     * Gets default schema of DB.
     * 
     * @param session the statement executor for executing SQL-statements
     * @param userName schema with this name may be empty
     */ 
    public static String getDefaultSchema(Session session, String userName) throws Exception {
    	List<String> schemas = new ArrayList<String>();
		try {
			DatabaseMetaData metaData = session.getMetaData();
			String dbName = metaData.getDatabaseProductName();
			boolean isPostgreSQL = dbName != null && dbName.toLowerCase().contains("PostgreSQL".toLowerCase());
			ResultSet rs = metaData.getSchemas();
			while (rs.next()) {
				schemas.add(rs.getString("TABLE_SCHEM"));
			}
			rs.close();
			String userSchema = null;
			for (Iterator<String> i = schemas.iterator(); i.hasNext(); ) {
				String schema = i.next().trim();
				if (isPostgreSQL && "public".equalsIgnoreCase(schema)) {
					return schema;
				}
				if (!schema.equalsIgnoreCase(userName.trim())) {
					rs = metaData.getTables(null, schema, "%", new String[] { "TABLE" });
					if (!rs.next()) {
						i.remove();
					}
					rs.close();
				} else {
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
    	Quoting quoting = new Quoting(metaData);
    	if (forDefaultSchema != session) {
    		forDefaultSchema = session;
    		_log.info("getting default schema...");
    		defaultSchema = getDefaultSchema(session, session.dbUser);
    		_log.info("default schema is '" + defaultSchema + "'");
    	}
    	_log.info("getting columns for " + table.getOriginalSchema(defaultSchema) + "." + quoting.unquote(table.getUnqualifiedName()));
    	ResultSet resultSet = metaData.getColumns(null, quoting.unquote(table.getOriginalSchema(defaultSchema)), quoting.unquote(table.getUnqualifiedName()), "%");
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
            columns.add(new Column(colName, sqlType, filterLength(length, resultSet.getString(6), type, session.dbms, resultSet.getInt(7)), precision));
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
