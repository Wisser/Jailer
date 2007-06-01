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

package org.jailer.datamodel;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Primary-key description of a {@link Table}.
 * 
 * @author Wisser
 */
public class PrimaryKey {
    
    /**
     * The primary-key columns.
     */
    private final List<Column> columns;
    
    /**
     * Constructor.
     * 
     * @param primaryKeyColumns the primary-key columns
     */
    PrimaryKey(List<Column> columns) {
        this.columns = columns;
    }
    
    /**
     * Gets the primary-key columns.
     * 
     * @return the primary-key columns
     */
    public List<Column> getColumns() {
        return columns;
    }

    /**
     * Matches the columns with the columns of <code>primaryKey</code>
     * s.t. each column of <code>primaryKey</code> is assigned to a
     * column of this PK with same type.
     * 
     * @param primaryKey to match
     * @return a match of all columns of <code>primaryKey</code>
     */
    public Map<Column, Column> match(PrimaryKey primaryKey) {
        Map<Column, Column> match = new HashMap<Column, Column>();
        int i = 0;
        for (Column column: getColumns()) {
            Column otherColumn = primaryKey.getColumns().get(i);
            if (column.type.equals(otherColumn.type)) {
                match.put(column, otherColumn);
                ++i;
                if (i >= primaryKey.columns.size()) {
                    break;
                }
            }
        }
        return match;
    }
    
    /**
     * Creates a comma-separated list of column names.
     * 
     * @param columnPrefix an optional prefix for each PK-column
     */
    public String columnList(String prefix) {
        String list = "";
        for (Column column: getColumns()) {
            if (list.length() > 0) {
                list += ", ";
            }
            if (prefix != null) {
                list += prefix;
            }
            list += column.name;
        }
        return list;
    }
    

    /**
     * Returns the primary key in SQL syntax.
     * 
     * @param columnPrefix an optional prefix for each PK-column
     */
    public String toSQL(String columnPrefix) {
        return toSQL(columnPrefix, true);
    }
    
    /**
     * Returns the primary key in SQL syntax.
     * 
     * @param columnPrefix an optional prefix for each PK-column
     */
    public String toSQL(String columnPrefix, boolean withContraints) {
        StringBuffer sb = new StringBuffer();
        for (int i = 0; i < columns.size(); ++i) {
            if (i > 0) {
                sb.append(", ");
            }
            sb.append(columns.get(i).toSQL(columnPrefix) + (withContraints? " NOT NULL" : ""));
        }
        return sb.toString();
    }
    
    /**
     * Returns a string representation of the primary key.
     */ 
    public String toString() {
        return toSQL(null);
    }
    
}
