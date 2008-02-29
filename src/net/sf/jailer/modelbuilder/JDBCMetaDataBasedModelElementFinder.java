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
 * Finds associations and tables by inspecting the JDBC meta data.
 * 
 * @author Wisser
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
        for (Table table: dataModel.getTables()) {
        	resultSet = metaData.getExportedKeys(null, statementExecutor.getIntrospectionSchema(), table.getName());
            _log.info("find associations with " + table.getName());
            Map<String, Association> fkMap = new HashMap<String, Association>();
            while (resultSet.next()) {
                Table pkTable = dataModel.getTable(resultSet.getString(3));
                String pkColumn = resultSet.getString(4);
                Table fkTable = dataModel.getTable(resultSet.getString(7));
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
	                    	namingSuggestion.put(association, new String[] { foreignKey, fkTable.getName() + "." + foreignKey });
	                    }
	                }
                }
            }
            resultSet.close();
        }
        return associations;
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
            resultSet = metaData.getPrimaryKeys(null, statementExecutor.getIntrospectionSchema(), tableName);
            Map<Integer, Column> pk = pkColumns.get(tableName);
            if (pk == null) {
                pk = new HashMap<Integer, Column>();
                pkColumns.put(tableName, pk);
            }
            while (resultSet.next()) {
                pk.put(resultSet.getInt(5), new Column(resultSet.getString(4), "", 0));
            }    
            _log.info("found primary key for table " + tableName);
            resultSet.close();
        }
        for (String tableName: tableNames) {
            resultSet = metaData.getColumns(null, statementExecutor.getIntrospectionSchema(), tableName, null);
            Map<Integer, Column> pk = pkColumns.get(tableName);
            while (resultSet.next()) {
                String colName = resultSet.getString(4);
                int type = resultSet.getInt(5);
                int length = 0;
                if (type == Types.NUMERIC || type == Types.DECIMAL || type == Types.VARCHAR || type == Types.CHAR) {
                    length = resultSet.getInt(7);
                }
                String sqlType = SqlUtil.SQL_TYPE.get(type);
                if (sqlType == null) {
                	sqlType = "-unknown-";
                    // throw new RuntimeException("unknown SQL type: " + type);
                }
                Column column = new Column(colName, sqlType, length);
                for (int i: pk.keySet()) {
                    if (pk.get(i).name.equals(column.name)) {
                        pk.put(i, column);
                    }
                }
            }
            resultSet.close();
            _log.info("found columns for table " + tableName);
            
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
				schemas.add(rs.getString("TABLE_SCHEM"));
			}
			rs.close();
			for (Iterator<String> i = schemas.iterator(); i.hasNext(); ) {
				String schema = i.next();
				if (!schema.equalsIgnoreCase(userName.trim())) {
					rs = metaData.getTables(null, schema, "%", new String[] { "TABLE" });
					if (!rs.next()) {
						i.remove();
					}
					rs.close();
				}
			}
		} catch (SQLException e) {
			e.printStackTrace();
			if (userName != null) {
				schemas.add(userName);
			}
		}
		return schemas;
    }

}
