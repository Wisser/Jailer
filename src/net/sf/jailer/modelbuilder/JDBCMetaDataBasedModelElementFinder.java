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
import java.sql.Types;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
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
     * @return found associations
     */
    public Collection<Association> findAssociations(DataModel dataModel, StatementExecutor statementExecutor) throws Exception {
        Collection<Association> associations = new ArrayList<Association>();
        DatabaseMetaData metaData = statementExecutor.getMetaData();
        ResultSet resultSet;
        for (Table table: dataModel.getTables()) {
            resultSet = metaData.getExportedKeys(null, null, table.getName());
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
        resultSet = metaData.getTables(null, null, "%", null);
        List<String> tableNames = new ArrayList<String>();
        while (resultSet.next()) {
            String tableName = resultSet.getString(3);
            if ("TABLE".equalsIgnoreCase(resultSet.getString(4))) {
                tableNames.add(tableName);
            }
        }
        resultSet.close();
        Map<String, Map<Integer, Column>> pkColumns = new HashMap<String, Map<Integer, Column>>();
        for (String tableName: tableNames) {
            resultSet = metaData.getPrimaryKeys(null, null, tableName);
            Map<Integer, Column> pk = pkColumns.get(tableName);
            if (pk == null) {
                pk = new HashMap<Integer, Column>();
                pkColumns.put(tableName, pk);
            }
            while (resultSet.next()) {
                pk.put(resultSet.getInt(5), new Column(resultSet.getString(4), "", 0));
            }    
            resultSet.close();
        }
        for (String tableName: tableNames) {
            resultSet = metaData.getColumns(null, null, tableName, null);
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
                    throw new RuntimeException("unknown SQL type: " + type);
                }
                Column column = new Column(colName, sqlType, length);
                for (int i: pk.keySet()) {
                    if (pk.get(i).name.equals(column.name)) {
                        pk.put(i, column);
                    }
                }
            }
            resultSet.close();
            
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

}
