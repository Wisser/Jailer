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

package org.jailer.util;

import java.sql.Types;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.jailer.datamodel.DataModel;
import org.jailer.datamodel.Table;


/**
 * Some utility methods.
 * 
 * @author Wisser
 */
public class SqlUtil {
    
    /**
     * Change alias A to B and B to A in a SQL-condition.
     * 
     * @param condition the condition
     * @return condition with revered aliases
     */
    public static String reversRestrictionCondition(String condition) {
        final String chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_";
        StringBuffer reversed = new StringBuffer("");
        for (int i = 0; i < condition.length(); ++i) {
            char c = condition.charAt(i);
            if (c == 'A' || c == 'B') {
                if (i + 1 < condition.length() && condition.charAt(i + 1) == '.') {
                    if (i == 0 || chars.indexOf(condition.charAt(i - 1)) < 0) {
                        reversed.append(c == 'A'? 'B' : 'A');
                        continue;
                    }
                }
            }
            reversed.append(c);
        }
        return reversed.toString();
    }
    
    /**
     * Replaces the aliases A and B with given aliases  ina SQL-condition.
     * 
     * @param condition the condition
     * @param aliasA alias for A
     * @param aliasB alias for B
     * @return condition with replaced aliases
     */
    public static String replaceAliases(String condition, String aliasA, String aliasB) {
        final String chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_";
        StringBuffer result = new StringBuffer("");
        for (int i = 0; i < condition.length(); ++i) {
            char c = condition.charAt(i);
            if (c == 'A' || c == 'B') {
                if (i + 1 < condition.length() && condition.charAt(i + 1) == '.') {
                    if (i == 0 || chars.indexOf(condition.charAt(i - 1)) < 0) {
                        result.append(c == 'A'? aliasA : aliasB);
                        continue;
                    }
                }
            }
            result.append(c);
        }
        return result.toString();
    }
    
    /**
     * Reads a table-list from CSV-file.
     * 
     * @param dataModel to get tables from
     * @param tableFile the file containing the list
     * @return set of tables, empty list if file contains no tables
     */
    public static Set<Table> readTableList(CsvFile tableFile, DataModel dataModel) {
        Set<Table> tabuTables = new HashSet<Table>();
        
        if (tableFile != null) {
            for (CsvFile.Line line: tableFile.getLines()) {
                Table table = dataModel.getTable(line.cells.get(0));
                if (table == null) {
                    throw new RuntimeException(line.location + ": unknown table: '" + line.cells.get(0) + "'");
                }
                tabuTables.add(table);
            }
        }
        return tabuTables;
    }
    
    /**
     * Converts a cell-content to valid SQL-literal.
     * 
     * @param object the content
     * @return the SQL-literal
     */
    public static String toSql(Object content) {
        if (content == null) {
            return "null";
        }

        if (content instanceof java.sql.Date) {
            return "'" + content + "'";
        }
        if (content instanceof java.sql.Timestamp) {
            return "'" + content + "'";
        }
        if (content instanceof String) {
            String qvalue = (String) content;
            if (qvalue.indexOf('\'') > -1) {
                StringBuffer escapeBuffer = new StringBuffer(qvalue);
                int pos = 0;
                int lastpos = 0;

                do {
                    pos = escapeBuffer.indexOf("'", lastpos);
                    if (pos > -1) {
                        escapeBuffer.insert(pos, '\'');
                        lastpos = pos + 2;
                    }
                } while (pos > -1);
                qvalue = escapeBuffer.toString();
            }
            return "'" + qvalue + "'";
        }
        return content.toString();
    }
    
    public final static Map<Integer, String> SQL_TYPE;
    
    static {
        SQL_TYPE = new HashMap<Integer, String>();
        SQL_TYPE.put(Types.BIGINT, "BIGINT");
        SQL_TYPE.put(Types.BINARY, "BINARY");
        SQL_TYPE.put(Types.BIT, "BIT");
        SQL_TYPE.put(Types.CHAR, "CHAR");
        SQL_TYPE.put(Types.DATE, "DATE");
        SQL_TYPE.put(Types.DECIMAL, "DECIMAL");
        SQL_TYPE.put(Types.DOUBLE, "DOUBLE");
        SQL_TYPE.put(Types.FLOAT, "FLOAT");
        SQL_TYPE.put(Types.INTEGER, "INTEGER");
        SQL_TYPE.put(Types.NUMERIC, "NUMERIC");
        SQL_TYPE.put(Types.TIME, "TIME");
        SQL_TYPE.put(Types.TIMESTAMP, "TIMESTAMP");
        SQL_TYPE.put(Types.TINYINT, "TINYINT");
        SQL_TYPE.put(Types.VARCHAR, "VARCHAR");
        SQL_TYPE.put(Types.SMALLINT, "SMALLINT");
    };
    
}
