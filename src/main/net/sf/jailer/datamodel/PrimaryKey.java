/*
 * Copyright 2007 - 2012 the original author or authors.
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
package net.sf.jailer.datamodel;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.sf.jailer.Configuration;

/**
 * Primary-key of a {@link Table}.
 * 
 * @author Ralf Wisser
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
    	if (Configuration.getDoMinimizeUPK()) {
			Set<Integer> assignedUPKColumns = new HashSet<Integer>();
	        Map<Column, Column> match = new HashMap<Column, Column>();
	        for (Column column: getColumns()) {
	            for (int i = 0; i < primaryKey.getColumns().size(); ++i) {
	            	if (assignedUPKColumns.contains(i)) {
	            		continue;
	            	}
		        	Column otherColumn = primaryKey.getColumns().get(i);
		            if (isAssignable(column, otherColumn)) {
		                match.put(column, otherColumn);
		                assignedUPKColumns.add(i);
	                    break;
		            }
	            }
	        }
	        return match;
    	} else {
	        Map<Column, Column> match = new HashMap<Column, Column>();
	        int i = 0;
	        for (Column column: getColumns()) {
	            Column otherColumn = primaryKey.getColumns().get(i);
	            if (isAssignable(column, otherColumn)) {
	                match.put(column, otherColumn);
	                ++i;
	                if (i >= primaryKey.columns.size()) {
	                    break;
	                }
	            }
	        }
	        return match;
    	}
    }

	private boolean isAssignable(Column uPKColumn, Column entityColumn) {
		if (!uPKColumn.type.equals(entityColumn.type)) {
			return false;
		}
		if (uPKColumn.length == 0 && entityColumn.length != 0) {
			return false;
		}
		if (uPKColumn.length != 0 && entityColumn.length == 0) {
			return false;
		}
		if (uPKColumn.length < entityColumn.length) {
			return false;
		}
		if (uPKColumn.precision < 0 && entityColumn.precision >= 0) {
			return false;
		}
		if (uPKColumn.precision >= 0 && entityColumn.precision < 0) {
			return false;
		}
		if (uPKColumn.precision < entityColumn.precision) {
			return false;
		}
		return true;
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
     * Returns the primary key in SQL syntax.
     * 
     * @param columnPrefix an optional prefix for each PK-column
     * @param typeReplacement column types replacements
     */
    public String toSQL(String columnPrefix, String contraint, Map<String, String> typeReplacement) {
        StringBuffer sb = new StringBuffer();
        for (int i = 0; i < columns.size(); ++i) {
            if (i > 0) {
                sb.append(", ");
            }
            sb.append(columns.get(i).toSQL(columnPrefix, typeReplacement) + " " + contraint);
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
