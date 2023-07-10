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
package net.sf.jailer.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.lang.reflect.Method;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.NClob;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Time;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.Session;

/**
 * Converts a cell-content to valid SQL-literal.
 *
 * @author Ralf Wisser
 */
public class CellContentConverter {

	private final ResultSetMetaData resultSetMetaData;
	private final Map<Integer, Integer> typeCache = new HashMap<Integer, Integer>();
	private final Map<Integer, String> typenameCache = new HashMap<Integer, String>();
	private final Map<Integer, String> typenameWithLengthCache = new HashMap<Integer, String>();
	private final Map<String, Integer> columnIndex = new HashMap<String, Integer>();
	private final Map<Class<?>, Boolean> isPGObjectClass = new HashMap<Class<?>, Boolean>();
	private final Session session;
	private final DBMS configuration;
	private final DBMS targetConfiguration;
	private Method pgObjectGetType;

	/**
	 * Constructor.
	 *
	 * @param resultSetMetaData meta data of the result set to read from
	 * @param session database session
	 * @param targetConfiguration configuration of the target DBMS
	 */
	public CellContentConverter(ResultSetMetaData resultSetMetaData, Session session, DBMS targetConfiguration) {
		this.resultSetMetaData = resultSetMetaData;
		this.session = session;
		this.targetConfiguration = targetConfiguration;
		this.configuration = this.session.dbms;
	}

	public static final int TIMESTAMP_WITH_NANO = -30201;

	/**
	 * Converts a cell-content to valid SQL-literal.
	 *
	 * @param content the content
	 * 
	 * @return the SQL-literal
	 */
	public String toSql(Object content) {
		if (content == null) {
			return "null";
		}

		if (content instanceof SQLExpressionWrapper) {
			return ((SQLExpressionWrapper) content).getExpression();
		}
		if (content instanceof java.sql.Date) {
			if (targetConfiguration.getDatePattern() != null) {
				return targetConfiguration.createDateFormat().format((Date) content);
			}
			return "'" + content + "'";
		}
		if (content instanceof java.sql.Timestamp) {
			String nano = getNanoString((Timestamp) content, true);
			if (content instanceof TimestampWithNano && targetConfiguration.getTimestampWithNanoPattern() != null) {
				return targetConfiguration.createTimestampWithNanoFormat()
						.format(content)
						.replace("${NANOFORMAT}", "FF" + (nano.length()))
						.replace("${NANO}", nano);
			} else if (targetConfiguration.getTimestampPattern() != null) {
				return targetConfiguration.createTimestampFormat()
						.format(content)
						.replace("${NANOFORMAT}", "FF" + (nano.length()))
						.replace("${NANO}", nano);
			}
			return "'" + content + "'";
		}
		if (content instanceof NCharWrapper) {
			String ncharPrefix = targetConfiguration.getNcharPrefix();
			String value = content.toString();
			String literal = (ncharPrefix != null? ncharPrefix : "") + "'" + targetConfiguration.convertToStringLiteral(value, ncharPrefix) + "'";
			literal = targetConfiguration.postProcessStringLiteral(literal, value, ncharPrefix);
			return literal;
		}
		if (content instanceof String) {
			return targetConfiguration.postProcessStringLiteral("'" + targetConfiguration.convertToStringLiteral((String) content) + "'", (String) content, null);
		}
		if (content instanceof PObjectWrapper) {
			if (((PObjectWrapper) content).getValue() == null) {
				return "null";
			}
			return "'" + targetConfiguration.convertToStringLiteral(content.toString()) + "'::" + ((PObjectWrapper) content).getType();
		}
		if (content instanceof byte[]) {
			byte[] data = (byte[]) content;
			String binaryPattern = targetConfiguration.getBinaryPattern();
			int i = binaryPattern.indexOf("%s");
			if (i < 0) {
				return binaryPattern;
			}
			StringBuilder hex = new StringBuilder((data.length + 1) * 2 + binaryPattern.length());
			hex.append(binaryPattern.substring(0, i));
			for (byte b: data) {
				hex.append(hexChar[(b >> 4) & 15]);
				hex.append(hexChar[b & 15]);
			}
			data = null; // gc
			hex.append(binaryPattern.substring(i + 2));
			return hex.toString();
		}
		if (content instanceof Time) {
			return "'" + content + "'";
		}
		if (DBMS.POSTGRESQL.equals(configuration)) {
			Boolean isPGObject = isPGObjectClass.get(content.getClass());
			if (isPGObject == null) {
				isPGObject = content.getClass().getSimpleName().equals("PGobject");
				isPGObjectClass.put(content.getClass(), isPGObject);
			}
			if (isPGObject) {
				try {
					if (pgObjectGetType == null) {
						pgObjectGetType = content.getClass().getMethod("getType");
					}
					if ("varbit".equalsIgnoreCase((String) pgObjectGetType.invoke(content))) {
						// PostgreSQL bit values
						return "B'" + content + "'";
					}
					String val = content.toString();
					return targetConfiguration.postProcessStringLiteral("'" + targetConfiguration.convertToStringLiteral(val) + "'", val, null);
				} catch (Exception e) {
					throw new RuntimeException(e);
				}
			}
		}
		if (content instanceof UUID) {
			if (DBMS.POSTGRESQL.equals(targetConfiguration)) {
				return "'" + content + "'::uuid";
			}
			return "'" + content + "'";
		}
		if (DBMS.MSSQL.equals(targetConfiguration) || DBMS.SYBASE.equals(targetConfiguration) || DBMS.SQLITE.equals(targetConfiguration)) {
			// Boolean mapping for MSSQL/Sybase/SQLite
			if (content instanceof Boolean) {
				content = Boolean.TRUE.equals(content)? "1" : "0";
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

		nanosString = new String(nanosChar, 0, truncIndex + 1);

		if (!full) {
			if (nanosString.length() > 4) {
				return nanosString.substring(0, 4);
			}
		}
		return nanosString;
	}

	private static final int TYPE_POBJECT = 10500;
	private static Set<String> OBJECT_TYPES = new HashSet<String>();
	static {
		OBJECT_TYPES.addAll(Arrays.asList("uuid", "hstore", "ghstore", "json", "jsonb", "_hstore", "_json", "_jsonb", "_ghstore"));
	}

	public class SQLExpressionWrapper implements Comparable<SQLExpressionWrapper> {
		private final Object value;
		private final String type;
		private final String pattern;
		public SQLExpressionWrapper(Object value, String type, String pattern) {
			this.value = value;
			this.type = type;
			this.pattern = pattern;
		}
		public String getExpression() {
			String expression;
			if (pattern.contains("'$1'")) {
				expression = pattern.replace("$1", value == null? "null" : targetConfiguration.convertToStringLiteral(String.valueOf(value)));
			} else {
				expression = pattern.replace("$1", value == null? "null" : String.valueOf(value));
			}
			expression = expression.replace("$2", type);
			return expression;
		}
		@Override
		public String toString() {
			return String.valueOf(value);
		}
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + ((type == null) ? 0 : type.hashCode());
			result = prime * result + ((value == null) ? 0 : value.hashCode());
			return result;
		}
		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			SQLExpressionWrapper other = (SQLExpressionWrapper) obj;
			if (type == null) {
				if (other.type != null)
					return false;
			} else if (!type.equals(other.type))
				return false;
			if (value == null) {
				if (other.value != null)
					return false;
			} else if (!value.equals(other.value))
				return false;
			return true;
		}
		@Override
		public int compareTo(SQLExpressionWrapper o) {
			return toString().compareTo(o.toString());
		}
	}

	public static class PObjectWrapper implements Comparable<PObjectWrapper> {
		private final String value;
		private final String type;
		public PObjectWrapper(String value, String type) {
			this.value = value;
			this.type = type;
		}
		public String getValue() {
			return value;
		}
		public String getType() {
			return type;
		}
		@Override
		public String toString() {
			return String.valueOf(value);
		}
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + ((type == null) ? 0 : type.hashCode());
			result = prime * result + ((value == null) ? 0 : value.hashCode());
			return result;
		}
		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			PObjectWrapper other = (PObjectWrapper) obj;
			if (type == null) {
				if (other.type != null)
					return false;
			} else if (!type.equals(other.type))
				return false;
			if (value == null) {
				if (other.value != null)
					return false;
			} else if (!value.equals(other.value))
				return false;
			return true;
		}
		@Override
		public int compareTo(PObjectWrapper o) {
			return toString().compareTo(o.toString());
		}
	}

	public static class NCharWrapper implements Comparable<NCharWrapper> {
		private final String value;
		public NCharWrapper(String value) {
			this.value = value;
		}
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + ((value == null) ? 0 : value.hashCode());
			return result;
		}
		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			NCharWrapper other = (NCharWrapper) obj;
			if (value == null) {
				if (other.value != null)
					return false;
			} else if (!value.equals(other.value))
				return false;
			return true;
		}
		@Override
		public String toString() {
			return String.valueOf(value);
		}
		@Override
		public int compareTo(NCharWrapper o) {
			return toString().compareToIgnoreCase(o.toString());
		}
	}

	@SuppressWarnings("serial")
	public static class TimestampWithNano extends Timestamp {
		public TimestampWithNano(long time) {
			super(time);
		}
	}

	/**
	 * Gets object from result-set.
	 *
	 * @param resultSet result-set
	 * @param i column index
	 * @return object
	 */
	public Object getObject(ResultSet resultSet, int i) throws SQLException {
		Integer type = typeCache.get(i);
		String columnTypeName = typenameCache.get(i);
		String columnTypeNameWithLength = typenameWithLengthCache.get(i);
		if (type == null) {
			try {
				type = resultSetMetaData.getColumnType(i);
				columnTypeName = resultSetMetaData.getColumnTypeName(i);
				int columnDisplaySize = resultSetMetaData.getColumnDisplaySize(i);
				columnTypeNameWithLength = resultSetMetaData.getColumnTypeName(i) + "(" + (columnDisplaySize == Integer.MAX_VALUE? "max" : Integer.toString(columnDisplaySize)) + ")";
				if (configuration.getTimestampWithNanoTypeName() != null && configuration.getTimestampWithNanoTypeName().equalsIgnoreCase(columnTypeName)) {
					type = TIMESTAMP_WITH_NANO;
				}
				if (DBMS.ORACLE.equals(configuration)) {
					if (type == Types.DATE || type == -102 || type == -101 /* TIMESTAMPTZ */) {
						type = Types.TIMESTAMP;
					}
				 }
				if (DBMS.POSTGRESQL.equals(configuration)) {
					String typeName = columnTypeName;
					if (isPostgresObjectType(typeName) || type == Types.ARRAY) {
						type = TYPE_POBJECT;
					}
				 }
				 // workaround for JDTS bug
				 if (type == Types.VARCHAR) {
					 if ("nvarchar".equalsIgnoreCase(columnTypeName)) {
						 type = Types.NVARCHAR;
					 }
				 }
				 // workaround for JDTS bug
				 if (DBMS.MSSQL.equals(configuration)) {
					 if ("datetimeoffset".equalsIgnoreCase(columnTypeName)) {
						 type = Types.TIMESTAMP;
					 }
				 }
				 if (type == Types.CHAR) {
					 if ("nchar".equalsIgnoreCase(columnTypeName)) {
						 type = Types.NCHAR;
					 }
				 }
				 if (type == Types.OTHER) {
					 if ("rowid".equalsIgnoreCase(columnTypeName)) {
						 type = Types.ROWID;
					 }
				 }
			} catch (Exception e) {
				type = Types.OTHER;
			}
			typeCache.put(i, type);
			typenameCache.put(i, columnTypeName);
			typenameWithLengthCache.put(i, columnTypeNameWithLength);
		}
		try {
			if (type == Types.ROWID) {
				return resultSet.getString(i);
			}
			if (type == Types.ARRAY) {
				return resultSet.getString(i);
			}
			if (type == Types.TIMESTAMP || type == 2014 /* Types.TIMESTAMP_WITH_TIMEZONE */) {
				return resultSet.getTimestamp(i);
			}
			if (type == Types.DATE) {
				if (DBMS.MySQL.equals(configuration)) {
					// YEAR
					String typeName = resultSetMetaData.getColumnTypeName(i);
					if (typeName != null && typeName.toUpperCase(Locale.ENGLISH).equals("YEAR")) {
						int result = resultSet.getInt(i);
						if (resultSet.wasNull()) {
							return null;
						}
						return result;
					}
				}
				Date date = resultSet.getDate(i);
				return date;
			}
		} catch (SQLException e) {
			return resultSet.getString(i);
		}

		Object object = null;
		try {
			if (configuration.isClobType(columnTypeNameWithLength)) {
				object = resultSet.getClob(i);
			} else if (configuration.isNClobType(columnTypeNameWithLength)) {
				object = resultSet.getNClob(i);
			} else if (configuration.isBlobType(columnTypeNameWithLength)) {
				object = resultSet.getBlob(i);
			}
		} catch (Exception e) {
			object = null;
		}

		if (object == null) {
			if (DBMS.POSTGRESQL.equals(configuration) && "money".equals(columnTypeName)) {
				// workaround for https://github.com/pgjdbc/pgjdbc/issues/100
				object = resultSet.getString(i);
			} else {
				object = resultSet.getObject(i);
			}
		}

		if (type == TIMESTAMP_WITH_NANO && object instanceof Timestamp) {
			long t = ((Timestamp) object).getTime();
			int nano = ((Timestamp) object).getNanos();
			Timestamp ts = new TimestampWithNano(t);
			ts.setNanos(nano);
			object = ts;
		}

		if (type == Types.NCHAR || type == Types.NVARCHAR || type == Types.LONGNVARCHAR) {
			if (object instanceof String) {
				object = new NCharWrapper((String) object);
			}
		}
		if (DBMS.POSTGRESQL.equals(configuration)) {
			if (type == TYPE_POBJECT) {
				return new PObjectWrapper(resultSet.getString(i), resultSetMetaData.getColumnTypeName(i));
			} else if (object instanceof Double && Double.isNaN((double) object)) {
				return "NaN";
			} else if (object instanceof Boolean) {
				String typeName = resultSetMetaData.getColumnTypeName(i);
				if (typeName != null && typeName.toLowerCase(Locale.ENGLISH).equals("bit")) {
					final String value = Boolean.TRUE.equals(object)? "B'1'" : "B'0'";
					return new Object() {
						@Override
						public String toString() {
							return value;
						}
					};
				}
			}
		}

		if (configuration != null && configuration.equals(targetConfiguration)) {
			if (!configuration.getSqlExpressionRule().isEmpty() && columnTypeName != null && object != null) {
				String expr = configuration.getSqlExpressionRule().get(columnTypeName.toLowerCase());
				if (expr != null) {
					return new SQLExpressionWrapper(object, columnTypeName, expr);
				}
				if (DBMS.POSTGRESQL.equals(configuration) && DBMS.POSTGRESQL.equals(targetConfiguration)) {
					expr = "'$1'::$2";
					if (!columnTypeName.startsWith("\"") && !columnTypeName.endsWith("\"")) {
						columnTypeName = "\"" + columnTypeName + "\"";
					}
					if ((type == TYPE_POBJECT || type == Types.ARRAY || object instanceof String)) {
						if (type == Types.VARCHAR) {
							if (!"text".equals(columnTypeName)) {
								int columnDisplaySize = resultSetMetaData.getColumnDisplaySize(i);
								if (columnDisplaySize == Integer.MAX_VALUE) {
									return new SQLExpressionWrapper(object, columnTypeName, expr);
								}
							}
						} else {
							return new SQLExpressionWrapper(object, columnTypeName, expr);
						} 
					} else {
						Boolean isPGObject = isPGObjectClass.get(object.getClass());
						if (isPGObject == null) {
							isPGObject = object.getClass().getSimpleName().equals("PGobject");
							isPGObjectClass.put(object.getClass(), isPGObject);
						}
						if (isPGObject) {
							return new SQLExpressionWrapper(object, columnTypeName, expr);
						}
					}
				}
			}
		}
		
		return object;
	}

	public static boolean isPostgresObjectType(String columnTypeName) {
		if (columnTypeName == null) {
			return false;
		}
		int i = columnTypeName.lastIndexOf('.');
		if (i >= 0) {
			columnTypeName = columnTypeName.substring(i + 1);
		}
		return OBJECT_TYPES.contains(Quoting.staticUnquote(columnTypeName.toLowerCase(Locale.ENGLISH)));
	}

	/**
	 * Gets object from result-set.
	 *
	 * @param resultSet result-set
	 * @param columnName column name
	 * 
	 * @return the object
	 */
	public Object getObject(ResultSet resultSet, String columnName) throws SQLException {
		Integer index = columnIndex.get(columnName);
		if (index == null) {
			for (int i = resultSetMetaData.getColumnCount(); i > 0; --i) {
				if (columnName.equalsIgnoreCase(resultSetMetaData.getColumnLabel(i))) {
					index = i;
					break;
				}
			}
			columnIndex.put(columnName, index);
		}
		if (index == null) {
			// this should never happen
			return resultSet.getObject(columnName);
		}
		return getObject(resultSet, index);
	}

	/**
	 * Gets SQL expression for a C/BLOB for small LOBS.
	 *
	 * @param resultSet the result set
	 * @param i index of LOB column
	 * @return SQL expression for a C/BLOB for small LOBS
	 */
	public String getSmallLob(ResultSet resultSet, int i) {
		try {
			String columnTypeNameWithLength = typenameWithLengthCache.get(i);
			if (columnTypeNameWithLength == null) {
				try {
					int columnDisplaySize = resultSetMetaData.getColumnDisplaySize(i);
					columnTypeNameWithLength = resultSetMetaData.getColumnTypeName(i) + "(" + (columnDisplaySize == Integer.MAX_VALUE? "max" : Integer.toString(columnDisplaySize)) + ")";
				} catch (Exception e) {
					// ignore
				}
				typenameWithLengthCache.put(i, columnTypeNameWithLength);
			}
			Object object = null;
			try {
				if (configuration.isClobType(columnTypeNameWithLength)) {
					object = resultSet.getClob(i);
				} else if (configuration.isNClobType(columnTypeNameWithLength)) {
					object = resultSet.getNClob(i);
				} else if (configuration.isBlobType(columnTypeNameWithLength)) {
					object = resultSet.getBlob(i);
				}
			} catch (Exception e) {
				object = null;
			}
			Object lob = object != null? object : resultSet.getObject(i);
			return getSmallLob(lob, targetConfiguration, null, null);
		}
		catch (SQLException e) {
			return null;
		}
	}

	/**
	 * Gets SQL expression for a C/BLOB for small LOBS.
	 *
	 * @param lob the lob
	 * 
	 * @return SQL expression for a C/BLOB for small LOBS
	 */
	public static String getSmallLob(Object lob, DBMS targetConfiguration, Integer maxBlobLength, Integer maxClobLength) {
		try {
			int embeddedLobSizeLimit = targetConfiguration.getEmbeddedLobSizeLimit();
			if (lob instanceof Clob) {
				Clob clob = (Clob) lob;
				String toClob;
				if (lob instanceof NClob) {
					toClob = targetConfiguration.getToNClob();
				} else {
					toClob = targetConfiguration.getToClob();
				}
				if (toClob == null || clob.length() > Math.min(embeddedLobSizeLimit, maxClobLength != null? maxClobLength : Integer.MAX_VALUE)) {
					return null;
				}
				Reader in = clob.getCharacterStream();
				int c;
				StringBuilder line = new StringBuilder();
				while ((c = in.read()) != -1) {
					line.append((char) c);
					if (line.length() > embeddedLobSizeLimit) {
						in.close();
						return null;
					}
				}
				in.close();
				if (lob instanceof NClob) {
					if (line.length() == 0 && targetConfiguration.getEmptyNCLOBValue() != null) {
						return targetConfiguration.getEmptyNCLOBValue();
					}
				} else {
					if (line.length() == 0 && targetConfiguration.getEmptyCLOBValue() != null) {
						return targetConfiguration.getEmptyCLOBValue();
					}
				}
				if (maxClobLength != null && line.length() > maxClobLength) {
					return null;
				}
				if (lob instanceof NClob) {
					return toClob.replace("%s",targetConfiguration.convertToStringLiteral(line.toString(), targetConfiguration.getNcharPrefix()));
				} else {
					return toClob.replace("%s",targetConfiguration.convertToStringLiteral(line.toString()));
				}
			}
			if (lob instanceof Blob) {
				Blob blob = (Blob) lob;
				if (targetConfiguration.getToBlob() == null || 2 * blob.length() > Math.min(embeddedLobSizeLimit, maxBlobLength != null? maxBlobLength : Integer.MAX_VALUE)) {
					return null;
				}

				InputStream in = blob.getBinaryStream();
				int b;
				StringBuilder hex = new StringBuilder();
				while ((b = in.read()) != -1) {
					hex.append(hexChar[(b >> 4) & 15]);
					hex.append(hexChar[b & 15]);
					if (hex.length() > embeddedLobSizeLimit) {
						in.close();
						return null;
					}
				}
				in.close();
				if (hex.length() == 0 && targetConfiguration.getEmptyBLOBValue() != null) {
					return targetConfiguration.getEmptyBLOBValue();
				}
				if (maxBlobLength != null && hex.length() > maxBlobLength) {
					return null;
				}
				return targetConfiguration.getToBlob().replace("%s", targetConfiguration.convertToStringLiteral(hex.toString()));
			}
		} catch (SQLException | IOException e) {
			return null;
		}

		return null;
	}

	/**
	 * All hex digits.
	 */
	public static final char[] hexChar = new char[] { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' };

}
