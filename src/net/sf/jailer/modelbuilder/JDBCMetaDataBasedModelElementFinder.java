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
import net.sf.jailer.database.StatementExecutor;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Cardinality;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.PrimaryKey;
import net.sf.jailer.datamodel.PrimaryKeyFactory;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.SqlUtil;

import org.apache.log4j.Logger;

/**
 * Finds associations and tables by introspection of the JDBC meta data.
 * 
 * @author Ralf Wisser
 */
public class JDBCMetaDataBasedModelElementFinder implements ModelElementFinder {

    /**
     * The logger.
     */
    private static final Logger _log = Logger.getLogger(JDBCMetaDataBasedModelElementFinder.class);

    /**
     * Finds associations by reading the databases meta-data.
     * 
     * @param statementExecutor the statement executor for executing SQL-statements 
     * @param dataModel model containing already known elements
     * @param namingSuggestion to put naming suggestions for associations into
     * @return found associations
     */
    public Collection<Association> findAssociations(DataModel dataModel, Map<Association, String[]> namingSuggestion, StatementExecutor statementExecutor) throws Exception {
        Collection<Association> associations = new ArrayList<Association>();
        DatabaseMetaData metaData = statementExecutor.getMetaData();
        ResultSet resultSet;
        String defaultSchema = getDefaultSchema(statementExecutor, statementExecutor.dbUser);
        
        for (Table table: dataModel.getTables()) {
        	resultSet = metaData.getExportedKeys(null, table.getSchema(defaultSchema), table.getUnqualifiedName());
            _log.info("find associations with " + table.getName());
            Map<String, Association> fkMap = new HashMap<String, Association>();
            while (resultSet.next()) {
                Table pkTable = dataModel.getTable(toQualifiedTableName(defaultSchema, resultSet.getString(2), resultSet.getString(3)));
                String pkColumn = resultSet.getString(4);
                Table fkTable = dataModel.getTable(toQualifiedTableName(defaultSchema, resultSet.getString(6), resultSet.getString(7)));
                String fkColumn = resultSet.getString(8);
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
     * @param statementExecutor the statement executor for executing SQL-statements 
     */
    public Set<Table> findTables(StatementExecutor statementExecutor) throws Exception {
        PrimaryKeyFactory primaryKeyFactory = new PrimaryKeyFactory();
        
        Set<Table> tables = new HashSet<Table>();
        DatabaseMetaData metaData = statementExecutor.getMetaData();
        ResultSet resultSet;
        resultSet = metaData.getTables(null, statementExecutor.getIntrospectionSchema(), "%", new String[] { "TABLE" });
        List<String> tableNames = new ArrayList<String>();
        while (resultSet.next()) {
            String tableName = resultSet.getString(3);
            if ("TABLE".equalsIgnoreCase(resultSet.getString(4))) {
                if (isValidName(tableName)) {
                	if (CommandLineParser.getInstance().qualifyNames) {
                		String schemaName = resultSet.getString(2);
                		if (schemaName != null) {
                			schemaName = schemaName.trim();
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
        }
        resultSet.close();
        Map<String, Map<Integer, Column>> pkColumns = new HashMap<String, Map<Integer, Column>>();
        for (String tableName: tableNames) {
        	Table tmp = new Table(tableName, null, false);
            resultSet = metaData.getPrimaryKeys(null, tmp.getSchema(statementExecutor.getIntrospectionSchema()), tmp.getUnqualifiedName());
            Map<Integer, Column> pk = pkColumns.get(tableName);
            if (pk == null) {
                pk = new HashMap<Integer, Column>();
                pkColumns.put(tableName, pk);
            }
            while (resultSet.next()) {
                pk.put(resultSet.getInt(5), new Column(resultSet.getString(4), "", 0, -1));
            }    
            _log.info("found primary key for table " + tableName);
            resultSet.close();
        }
        for (String tableName: tableNames) {
        	Table tmp = new Table(tableName, null, false);
            resultSet = metaData.getColumns(null, tmp.getSchema(statementExecutor.getIntrospectionSchema()), tmp.getUnqualifiedName(), null);
            Map<Integer, Column> pk = pkColumns.get(tableName);
            while (resultSet.next()) {
                String colName = resultSet.getString(4);
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
                String sqlType = SqlUtil.SQL_TYPE.get(type);
                if (sqlType == null) {
                	sqlType = resultSet.getString(6);
                    // throw new RuntimeException("unknown SQL type: " + type);
                }
                Column column = new Column(colName, sqlType, length, precision);
                for (int i: pk.keySet()) {
                    if (pk.get(i).name.equals(column.name)) {
                        pk.put(i, column);
                    }
                }
            }
            resultSet.close();
            _log.info("found primary key for table " + tableName);
            
            List<Integer> keySeqs = new ArrayList<Integer>(pk.keySet());
            Collections.sort(keySeqs);
            List<Column> columns = new ArrayList<Column>();
            for (int i: keySeqs) {
                columns.add(pk.get(i));
            }
            PrimaryKey primaryKey = primaryKeyFactory.createPrimaryKey(columns);
            Table table = new Table(tableName, primaryKey, false);
            table.setAuthor(metaData.getDriverName());
            tables.add(table);
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
     * @param statementExecutor the statement executor for executing SQL-statements
     * @param userName schema with this name may be empty
     */ 
    public static List<String> getSchemas(StatementExecutor statementExecutor, String userName) throws Exception {
    	List<String> schemas = new ArrayList<String>();
		try {
			DatabaseMetaData metaData = statementExecutor.getMetaData();
			ResultSet rs = metaData.getSchemas();
			while (rs.next()) {
				schemas.add(rs.getString("TABLE_SCHEM").trim());
			}
			rs.close();
			
			// don't remove empty schemas 
//			for (Iterator<String> i = schemas.iterator(); i.hasNext(); ) {
//				String schema = i.next();
//				if (!schema.equalsIgnoreCase(userName.trim())) {
//					rs = metaData.getTables(null, schema, "%", new String[] { "TABLE" });
//					if (!rs.next()) {
//						i.remove();
//					}
//					rs.close();
//				}
//			}
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
     * @param statementExecutor the statement executor for executing SQL-statements
     * @param userName schema with this name may be empty
     */ 
    public static String getDefaultSchema(StatementExecutor statementExecutor, String userName) throws Exception {
    	List<String> schemas = new ArrayList<String>();
		try {
			DatabaseMetaData metaData = statementExecutor.getMetaData();
			String dbName = metaData.getDatabaseProductName();
			boolean isPostgreSQL = "PostgreSQL".equals(dbName);
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

    /**
     * Finds the {@link Column}s of a given {@link Table}.
     *
     * @param table the table
     * @param statementExecutor the statement executor for executing SQL-statements 
     * 
     * @throws Exception on each error
     */
    public List<Column> findColumns(Table table, StatementExecutor statementExecutor) throws Exception {
    	List<Column> columns = new ArrayList<Column>();
    	DatabaseMetaData metaData = statementExecutor.getMetaData();
    	String defaultSchema = getDefaultSchema(statementExecutor, statementExecutor.dbUser);
        ResultSet resultSet = metaData.getColumns(null, table.getSchema(defaultSchema), table.getUnqualifiedName(), null);
        while (resultSet.next()) {
            String colName = resultSet.getString(4);
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
            String sqlType = SqlUtil.SQL_TYPE.get(type);
            // if (sqlType == null) {
            	sqlType = resultSet.getString(6);
            // }
            columns.add(new Column(colName, sqlType, length, precision));
        }
        resultSet.close();
        _log.info("found columns for table " + table.getName());
        return columns;
    }

}
