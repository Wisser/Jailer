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
package net.sf.jailer.util;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Time;
import java.sql.Timestamp;
import java.sql.Types;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.sf.jailer.database.DBMS;
import net.sf.jailer.database.SQLDialect;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.entitygraph.EntityGraph;

/**
 * Some utility methods.
 * 
 * @author Ralf Wisser
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
     * Replaces the aliases A and B with given aliases in a SQL-condition.
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
     * List of all jailer tables (upper case).
     */
    public static final List<String> JAILER_TABLES;
    static {
    	JAILER_TABLES = new ArrayList<String>();
    	JAILER_TABLES.add(EntityGraph.ENTITY_GRAPH_);
    	JAILER_TABLES.add(EntityGraph.ENTITY_SET_ELEMENT_);
    	JAILER_TABLES.add(EntityGraph.ENTITY_);
    	JAILER_TABLES.add(EntityGraph.DEPENDENCY_);
    	JAILER_TABLES.add(SQLDialect.CONFIG_TABLE_);
    	JAILER_TABLES.add(SQLDialect.DUAL_TABLE);
    	JAILER_TABLES.add(SQLDialect.TMP_TABLE_);
    }
    
    /**
     * To be used for date formatting.
     */
    public static DateFormat dateFormat = null;
    
    /**
     * To be used for time stamp formatting.
     */
    public static DateFormat timestampFormat = null;

    /**
     * Default time stamp format (for 'to_timestamp' function).
     */
    public static DateFormat defaultTimestampFormat = new SimpleDateFormat("yyyy-MM-dd HH.mm.ss");

    /**
     * <code>true</code> if 'to_timestamp' function is used for writing out timestamps instead of formatting them.
     */
	public static boolean useToTimestampFunction = false;

    public static boolean appendNanosToTimestamp = true;
    public static boolean appendMillisToTimestamp = false;
	public static char nanoSep = '.';

	public static DBMS dbms;
    
	/**
	 * All hex digits.
	 */
	private static final char[] hexChar = new char[] { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' };
	
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
        	if (dateFormat != null) {
        		synchronized(dateFormat) {
        			return "'" + dateFormat.format((Date) content) + "'";
        		}
        	}
            return "'" + content + "'";
        }
        if (content instanceof java.sql.Timestamp) {
        	if (useToTimestampFunction) {
        		String format;
        		synchronized(defaultTimestampFormat) {
	        		format = defaultTimestampFormat.format((Date) content);
	       			format += getNanoString((Timestamp) content, appendNanosToTimestamp);
        		}
				return "to_timestamp('" + format + "', 'YYYY-MM-DD HH24:MI:SS.FF')";
        	} else if (timestampFormat != null) {
        		String format;
        		synchronized(timestampFormat) {
	        		format = timestampFormat.format((Date) content);
	        		if (appendMillisToTimestamp) {
	        			format += getNanoString((Timestamp) content, appendNanosToTimestamp);
	        		}
        		}
				return "'" + format + "'";
        	}
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
        if (content instanceof byte[]) {
        	byte[] data = (byte[]) content;
        	StringBuilder hex = new StringBuilder((data.length + 1) * 2);
        	for (byte b: data) {
        		hex.append(hexChar[(b >> 4) & 15]);
        		hex.append(hexChar[b & 15]);
        	}
        	return SQLDialect.binaryPattern.replace("%s", hex);
        }
        if (content instanceof Time) {
        	return "'" + content + "'";
        }
        if (dbms == DBMS.POSTGRESQL) {
        	if (content.getClass().getName().endsWith(".PGobject")) {
        		// PostgreSQL bit values
        		return "B'" + content + "'";
        	}
        }
        return content.toString();
    }
    
    /**
     * Gets nano string suffix of a timestamp.
     * 
     * @param timestamp the timestamp
     */
    private static String getNanoString(Timestamp timestamp, boolean full) {
    	String zeros = "000000000";
    	int nanos = timestamp.getNanos();
    	String nanosString = Integer.toString(nanos);
    	
    	// Add leading zeros
    	nanosString = zeros.substring(0, (9-nanosString.length())) + nanosString;
    	
    	// Truncate trailing zeros
    	char[] nanosChar = new char[nanosString.length()];
    	nanosString.getChars(0, nanosString.length(), nanosChar, 0);
    	int truncIndex = 8;
    	while (truncIndex > 0 && nanosChar[truncIndex] == '0') {
    		truncIndex--;
    	}
    
    	nanosString = nanoSep + new String(nanosChar, 0, truncIndex + 1);
    	
    	if (!full) {
    		if (nanosString.length() > 4) {
    			return nanosString.substring(0, 4);
    		}
    	}
    	return nanosString;
    }

    /**
     * Gets object from result-set.
     * 
     * @param resultSet result-set
     * @param i column index
     * @param typeCache for caching types
     * @return object
     */
	public static Object getObject(ResultSet resultSet, int i, Map<Integer, Integer> typeCache) throws SQLException {
		Integer type = typeCache.get(i);
		if (type == null) {
			try {
				type = resultSet.getMetaData().getColumnType(i);
				if (SQLDialect.treatDateAsTimestamp) {
					if (type == Types.DATE) {
						type = Types.TIMESTAMP;
					}
				}
			} catch (Exception e) {
				type = Types.OTHER;
			}
			typeCache.put(i, type);
		}
		if (type == Types.TIMESTAMP) {
			return resultSet.getTimestamp(i);
		}
		if (type == Types.DATE) {
			Date date = resultSet.getDate(i);
			return date;
		}
		Object object = resultSet.getObject(i);
		if (dbms == DBMS.POSTGRESQL) {
			if (object instanceof Boolean) {
				String typeName = resultSet.getMetaData().getColumnTypeName(i);
				if (typeName != null && typeName.toLowerCase().equals("bit")) {
					final String value = Boolean.TRUE.equals(object)? "B'1'" : "B'0'";
					return new Object() {
						public String toString() {
							return value;
						}
					};
				}
			}
		}
		return object;
	};
	
    /**
     * Gets type of column from result-set.
     * 
     * @param resultSet result-set
     * @param i column index
     * @param typeCache for caching types
     * @return type according to {@link Types}
     */
	public static int getColumnType(ResultSet resultSet, int i, Map<Integer, Integer> typeCache) throws SQLException {
		Integer type = typeCache.get(i);
		if (type == null) {
			try {
				type = resultSet.getMetaData().getColumnType(i);
			} catch (Exception e) {
				type = Types.OTHER;
			}
			typeCache.put(i, type);
		}
		return type;
	};
	
    /**
     * Gets type of column from result-set.
     * 
     * @param resultSet result-set
     * @param columnName column name
     * @param typeCache for caching types
     * @return object
     */
	public static int getColumnType(ResultSet resultSet, String columnName, Map<String, Integer> typeCache) throws SQLException {
		Integer type = typeCache.get(columnName);
		if (type == null) {
			try {
				type = Types.OTHER;
				for (int i = resultSet.getMetaData().getColumnCount(); i > 0; --i) {
					if (columnName.equalsIgnoreCase(resultSet.getMetaData().getColumnName(i))) {
						type = resultSet.getMetaData().getColumnType(i);
						break;
					}
				}
			} catch (Exception e) {
			}
			typeCache.put(columnName, type);
		}
		return type;
	}
	
    /**
     * Gets object from result-set.
     * 
     * @param resultSet result-set
     * @param columnName column name
     * @param typeCache for caching types
     * @return object
     */
	public static Object getObject(ResultSet resultSet, String columnName, Map<String, Integer> typeCache) throws SQLException {
		Integer type = typeCache.get(columnName);
		if (type == null) {
			try {
				type = Types.OTHER;
				for (int i = resultSet.getMetaData().getColumnCount(); i > 0; --i) {
					if (columnName.equalsIgnoreCase(resultSet.getMetaData().getColumnName(i))) {
						type = resultSet.getMetaData().getColumnType(i);
						break;
					}
				}
			} catch (Exception e) {
			}
			typeCache.put(columnName, type);
		}
		if (type == Types.TIMESTAMP) {
			return resultSet.getTimestamp(columnName);
		}
		if (type == Types.DATE) {
			Date date = resultSet.getDate(columnName);
			return date;
		}
		return resultSet.getObject(columnName);
	};

	public static final String LETTERS_AND_DIGITS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789";

	/**
	 * Maps SQL types from {@link java.sql.Types} to clear text types.
	 */
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
        SQL_TYPE.put(Types.CLOB, "CLOB");
        SQL_TYPE.put(Types.BLOB, "BLOB");
    }
    
}
