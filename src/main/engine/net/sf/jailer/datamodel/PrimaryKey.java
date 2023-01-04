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
package net.sf.jailer.datamodel;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.util.Quoting;

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
	 * <code>true</code> if match with UPK must be ordered.
	 */
	private final boolean needsOrderedMatch;

	public int numberOfIndexedPKColumns;

	/**
	 * Constructor.
	 * 
	 * @param columns the primary-key columns
	 * @param needsOrderedMatch <code>true</code> if match with UPK must be ordered
	 */
	public PrimaryKey(List<Column> columns, boolean needsOrderedMatch) {
		this.columns = columns;
		this.needsOrderedMatch = needsOrderedMatch;
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
		boolean minimize = Configuration.getInstance().getDoMinimizeUPK() || !primaryKey.needsOrderedMatch;
		if (minimize) {
			Set<Integer> assignedUPKColumns = new HashSet<Integer>();
			for (Column column: getColumns()) {
				for (int i = 0; i < primaryKey.getColumns().size(); ++i) {
					if (assignedUPKColumns.contains(i)) {
						continue;
					}
					if (i >= primaryKey.getColumns().size()) {
						break;
					}
					Column otherColumn = primaryKey.getColumns().get(i);
					if (isAssignable(column, otherColumn)) {
						match.put(column, otherColumn);
						assignedUPKColumns.add(i);
						break;
					}
				}
			}
		} else {
			int i = 0;
			for (Column column: getColumns()) {
				if (i >= primaryKey.getColumns().size()) {
					break;
				}
				Column otherColumn = primaryKey.getColumns().get(i);
				if (isAssignable(column, otherColumn)) {
					match.put(column, otherColumn);
					++i;
					if (i >= primaryKey.columns.size()) {
						break;
					}
				}
			}
		}
		
		if (match.size() != primaryKey.columns.size()) {
			throw new IllegalStateException("incomplete PK-UPK-match (" + minimize + ")\n"
					+ "PK: " + primaryKey.toSQL(null) + "\n"
					+ "UPK: " + toSQL(null) + "\n"
					+ "Match: " + match + "\n");
		}
		
		return match;
	}

	public static boolean  isAssignable(Column uPKColumn, Column entityColumn) {
		if (!uPKColumn.type.equalsIgnoreCase(entityColumn.type)) {
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
	 * @param prefix an optional prefix for each PK-column
	 */
	public String columnList(String prefix) {
		return columnList(prefix, null);
	}
	
	/**
	 * Creates a comma-separated list of column names.
	 * 
	 * @param prefix an optional prefix for each PK-column
	 */
	public String columnList(String prefix, Quoting quoting) {
		String list = "";
		for (Column column: getColumns()) {
			if (list.length() > 0) {
				list += ", ";
			}
			if (prefix != null) {
				list += prefix;
			}
			list += quoting != null? quoting.requote(column.name) : column.name;
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
	@Override
	public String toString() {
		return toSQL(null);
	}

	public void assign(List<Column> primaryKeyColumns) {
		columns.clear();
		columns.addAll(primaryKeyColumns);
	}

	public static boolean isIncreasable(Column uPKColumn, Column column) {
		if(!uPKColumn.type.equals(column.type)) {
			return false;
		}

		if((uPKColumn.precision < 0) && (column.precision >=0) ) {
			return false;
		}

		if((uPKColumn.precision >=0) && (column.precision < 0)) {
			return false;
		}

		if(uPKColumn.length == 0 && column.length > 0) {
			return false;
		}

		if(uPKColumn.length < column.length) {
			return true;
		}

		if(uPKColumn.precision < column.precision) {
			return true;
		}

		// never should get THIS far !
		return false;
	}

}
