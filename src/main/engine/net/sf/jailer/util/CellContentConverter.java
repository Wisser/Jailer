/*
 * Copyright 2007 - 2017 the original author or authors.
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

	/**
	 * All hex digits.
	 */
	private static final char[] hexChar = new char[] { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' };
	
	private final ResultSetMetaData resultSetMetaData;
	private final Map<Integer, Integer> typeCache = new HashMap<Integer, Integer>();
	private final Map<String, Integer> columnIndex = new HashMap<String, Integer>();
	private final Session session;
	private final DBMS configuration;
	private final DBMS targetConfiguration;
	private Method pgObjectGetType;
	
	/**
	 * Constructor.
	 * 
	 * @param resultSetMetaData meta data of the result set to read from
	 * @param session database session
	 * @param targetDBMSConfiguration configuration of the target DBMS
	 */
	public CellContentConverter(ResultSetMetaData resultSetMetaData, Session session, DBMS targetConfiguration) {
		this.resultSetMetaData = resultSetMetaData;
		this.session = session;
		this.targetConfiguration = targetConfiguration;
		this.configuration = this.session.dbms;
	}

	/**
	 * Converts a cell-content to valid SQL-literal.
	 * 
	 * @param object the content
	 * @return the SQL-literal
	 */
	public String toSql(Object content) {
		if (content == null) {
			return "null";
		}

		if (content instanceof java.sql.Date) {
			if (targetConfiguration.getDatePattern() != null) {
				synchronized (targetConfiguration.getDatePattern()) {
					return targetConfiguration.createDateFormat()
							.format((Date) content);
				}
			}
			return "'" + content + "'";
		}
		if (content instanceof java.sql.Timestamp) {
			String nano = getNanoString((Timestamp) content, true);
			if (targetConfiguration.getTimestampPattern() != null) {
				synchronized (targetConfiguration.getTimestampPattern()) {
					return targetConfiguration.createTimestampFormat()
							.format(content)
							.replace("${NANOFORMAT}", "FF" + (nano.length()))
							.replace("${NANO}", nano);
				}
			}
			return "'" + content + "'";
		}
		if (content instanceof NCharWrapper) {
			String prefix = targetConfiguration.getNcharPrefix();
			if (prefix == null) {
				prefix = "";
			}
			return prefix + "'" + targetConfiguration.convertToStringLiteral(content.toString()) + "'";
		}
		if (content instanceof String) {
			return "'" + targetConfiguration.convertToStringLiteral((String) content) + "'";
		}
		if (content instanceof PObjectWrapper) {
			if (((PObjectWrapper) content).getValue() == null) {
				return "null";
			}
			return "'" + targetConfiguration.convertToStringLiteral(content.toString()) + "'::" + ((PObjectWrapper) content).getType();
		}
		if (content instanceof byte[]) {
			byte[] data = (byte[]) content;
			StringBuilder hex = new StringBuilder((data.length + 1) * 2);
			for (byte b: data) {
				hex.append(hexChar[(b >> 4) & 15]);
				hex.append(hexChar[b & 15]);
			}
			return targetConfiguration.getBinaryPattern().replace("%s", hex);
		}
		if (content instanceof Time) {
			return "'" + content + "'";
		}
		if (content.getClass().getSimpleName().equals("PGobject")) {
			try {
				if (pgObjectGetType == null) {
					pgObjectGetType = content.getClass().getMethod("getType");
				}
				if ("varbit".equalsIgnoreCase((String) pgObjectGetType.invoke(content))) {
					// PostgreSQL bit values
					return "B'" + content + "'";
				}
				return "'" + targetConfiguration.convertToStringLiteral(content.toString()) + "'";
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		}
		if (content instanceof UUID) {
			if (DBMS.POSTGRESQL.equals(targetConfiguration)) {
				return "'" + content + "'::uuid";
			}
			return "'" + content + "'";
		}
		if (targetConfiguration.isIdentityInserts()) {
			// Boolean mapping for MSSQL/Sybase
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
	 * @param nanoSep 
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
	private static Set<String> POSTGRES_EXTENSIONS = new HashSet<String>();
	static {
		POSTGRES_EXTENSIONS.addAll(Arrays.asList("hstore", "ghstore", "json", "jsonb", "_hstore", "_json", "_jsonb", "_ghstore"));
	}
	
	public static class PObjectWrapper {
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
		public String toString() {
			return value;
		}
	}

	static class NCharWrapper {
		private final String value;
		public NCharWrapper(String value) {
			this.value = value;
		}
		public String toString() {
			return value;
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
		if (type == null) {
			try {
				type = resultSetMetaData.getColumnType(i);
				if (DBMS.ORACLE.equals(configuration)) {
					if (type == Types.DATE || type == -102 || type == -101 /* TIMESTAMPTZ */) {
						type = Types.TIMESTAMP;
					}
				 }
				 if (DBMS.POSTGRESQL.equals(configuration)) {
					String typeName = resultSetMetaData.getColumnTypeName(i);
					if (isPostgresObjectType(typeName) || type == Types.ARRAY) {
						type = TYPE_POBJECT;
					}
				 }
				 // workaround for JDTS bug
				 if (type == Types.VARCHAR) {
					 if ("nvarchar".equalsIgnoreCase(resultSetMetaData.getColumnTypeName(i))) {
						 type = Types.NVARCHAR;
					 }
				 }
				 if (type == Types.CHAR) {
					 if ("nchar".equalsIgnoreCase(resultSetMetaData.getColumnTypeName(i))) {
						 type = Types.NCHAR;
					 }
				 }
				 if (type == Types.OTHER) {
					 if ("rowid".equalsIgnoreCase(resultSetMetaData.getColumnTypeName(i))) {
						 type = Types.ROWID;
					 }
				 }
			} catch (Exception e) {
				type = Types.OTHER;
			}
			typeCache.put(i, type);
		}
		try {
			if (type == Types.ROWID) {
				return resultSet.getString(i);
			}
			if (type == Types.ARRAY) {
				return resultSet.getString(i);
			}
			if (type == Types.TIMESTAMP) {
				return resultSet.getTimestamp(i);
			}
			if (type == Types.DATE) {
				if (DBMS.MySQL.equals(configuration)) {
					// YEAR
					String typeName = resultSetMetaData.getColumnTypeName(i);
					if (typeName != null && typeName.toUpperCase().equals("YEAR")) {
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
		Object object = resultSet.getObject(i);
		if (type == Types.NCHAR || type == Types.NVARCHAR) {
			if (object instanceof String) {
				object = new NCharWrapper((String) object);
			}
		}
		if (DBMS.POSTGRESQL.equals(configuration)) {
			if (type == TYPE_POBJECT) {
				return new PObjectWrapper(resultSet.getString(i), resultSetMetaData.getColumnTypeName(i));
			} else if (object instanceof Boolean) {
				String typeName = resultSetMetaData.getColumnTypeName(i);
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
	
	private boolean isPostgresObjectType(String columnTypeName) {
		if (columnTypeName == null) {
			return false;
		}
		int i = columnTypeName.lastIndexOf('.');
		if (i >= 0) {
			columnTypeName = columnTypeName.substring(i + 1);
		}
		return POSTGRES_EXTENSIONS.contains(Quoting.staticUnquote(columnTypeName.toLowerCase(Locale.ENGLISH)));
	}

	/**
	 * Gets object from result-set.
	 * 
	 * @param resultSet result-set
	 * @param columnName column name
	 * @param typeCache for caching types
	 * @return object
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
	public String getSmallLob(ResultSet resultSet, int i) throws SQLException, IOException {
		try {
			Object lob = resultSet.getObject(i);
			
			if (lob instanceof Clob) {
				Clob clob = (Clob) lob;
				String toClob;
				if (lob instanceof NClob) {
					toClob = targetConfiguration.getToNClob();
				} else {
					toClob = targetConfiguration.getToClob();
				}
				if (toClob == null || clob.length() > targetConfiguration.getEmbeddedLobSizeLimit()) {
					return null;
				}
				Reader in = clob.getCharacterStream();
				int c;
				StringBuilder line = new StringBuilder();
				while ((c = in.read()) != -1) {
					line.append((char) c);
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
				return toClob.replace("%s",targetConfiguration.convertToStringLiteral(line.toString()));
			}
			if (lob instanceof Blob) {
				Blob blob = (Blob) lob;
				if (targetConfiguration.getToBlob() == null || 2 * blob.length() > targetConfiguration.getEmbeddedLobSizeLimit()) {
					return null;
				}
	
				InputStream in = blob.getBinaryStream();
				int b;
				StringBuilder hex = new StringBuilder();
				while ((b = in.read()) != -1) {
					hex.append(hexChar[(b >> 4) & 15]);
					hex.append(hexChar[b & 15]);
				}
				in.close();
				if (hex.length() == 0 && targetConfiguration.getEmptyBLOBValue() != null) {
					return targetConfiguration.getEmptyBLOBValue();
				}
				return targetConfiguration.getToBlob().replace("%s", targetConfiguration.convertToStringLiteral(hex.toString()));
			}
		} catch (SQLException e) {
			return null;
		}
		
		return null;
	}

}
