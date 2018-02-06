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
package net.sf.jailer.modelbuilder;

import java.io.InputStream;
import java.io.Reader;
import java.math.BigDecimal;
import java.net.URL;
import java.sql.Array;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.DatabaseMetaData;
import java.sql.Date;
import java.sql.NClob;
import java.sql.Ref;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.RowId;
import java.sql.SQLException;
import java.sql.SQLWarning;
import java.sql.SQLXML;
import java.sql.Statement;
import java.sql.Time;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.log4j.Logger;

import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.Session;
import net.sf.jailer.util.CancellationHandler;
import net.sf.jailer.util.CellContentConverter;

/**
 * Reads database meta data directly from meta data views.
 * 
 * @author Wisser
 */
public class MetaDataCache {

	/**
	 * The logger.
	 */
	private static final Logger _log = Logger.getLogger(JDBCMetaDataBasedModelElementFinder.class);

	/**
	 * The cached rows.
	 */
	private Map<String, List<Object[]>> cache;

	/**
	 * Meta data of chached row set.
	 */
	private MDCResultSetMetaData resultSetMetaData;
	
	/**
	 * Reads primary keys.
	 * 
	 * @param session
	 *            the session
	 * @param schema
	 *            name of the schema
	 * @return cache
	 */
	public static MetaDataCache readPrimaryKeys(Session session, String schema) {
		String primaryKeysQuery = session.dbms.getPrimaryKeysQuery();
		if (primaryKeysQuery == null) {
			return new MetaDataCache();
		}

		_log.info("reading primary keys...");

		MetaDataCache metaDataCache = new MetaDataCache();
		try {
			Set<Integer> intIndex = new HashSet<Integer>(Arrays.asList(5));
			readMetaData(metaDataCache, session, primaryKeysQuery.replace("${SCHEMA}", schema), intIndex);
			return metaDataCache;
		} catch (Exception e) {
			_log.info(e.getMessage());
			return new MetaDataCache();
		}
	}

	/**
	 * Reads index infos.
	 * 
	 * @param session
	 *            the session
	 * @param schema
	 *            name of the schema
	 * @return cache
	 */
	public static MetaDataCache readIndexInfo(Session session, String schema) {
		String indexInfoQuery = session.dbms.getIndexInfoQuery();
		if (indexInfoQuery == null) {
			return new MetaDataCache();
		}

		_log.info("reading index info...");

		MetaDataCache metaDataCache = new MetaDataCache();
		try {
			Set<Integer> intIndex = new HashSet<Integer>(Arrays.asList(4));
			readMetaData(metaDataCache, session, indexInfoQuery.replace("${SCHEMA}", schema), intIndex);
			return metaDataCache;
		} catch (Exception e) {
			_log.info(e.getMessage());
			return new MetaDataCache();
		}
	}

	/**
	 * Reads imported keys (FKs).
	 * 
	 * @param session
	 *            the session
	 * @param schema
	 *            name of the schema
	 * @return cache
	 */
	public static MetaDataCache readImportedKeys(Session session, String schema) {
		String importedKeysQuery = session.dbms.getImportedKeysQuery();
		if (importedKeysQuery == null) {
			return new MetaDataCache();
		}

		_log.info("reading imported keys...");

		MetaDataCache metaDataCache = new MetaDataCache();
		try {
			Set<Integer> intIndex = new HashSet<Integer>(Arrays.asList(9, 10, 11, 14));
			readMetaData(metaDataCache, session, importedKeysQuery.replace("${SCHEMA}", schema), intIndex);
			return metaDataCache;
		} catch (Exception e) {
			_log.info(e.getMessage());
			return new MetaDataCache();
		}
	}

	/**
	 * Reads column infos.
	 * 
	 * @param session
	 *            the session
	 * @param schema
	 *            name of the schema
	 * @return cache
	 */
	public static MetaDataCache readColumns(Session session, DatabaseMetaData metaData, String schema) {
		_log.info("reading columns...");

		MetaDataCache metaDataCache = new MetaDataCache();
		ResultSet rs;
		try {
			if (DBMS.MySQL.equals(session.dbms)) {
				rs = metaData.getColumns(schema, null, "%", "%");
			} else {
				rs = metaData.getColumns(null, schema, "%", "%");
			}
			Set<Integer> intIndex = new HashSet<Integer>(Arrays.asList(5, 7, 9, 10, 11, 14, 15, 16, 17, 22));

			metaDataCache.cache = new HashMap<String, List<Object[]>>();
			ResultSetMetaData rsMetaData = rs.getMetaData();
			int numCol = rsMetaData.getColumnCount();
			String[] names = new String[numCol];
			int[] types = new int[numCol];
			for (int i = 0; i < numCol; ++i) {
				names[i] = rsMetaData.getColumnName(i + 1);
				types[i] = rsMetaData.getColumnType(i + 1);
			}
			
			while (rs.next()) {
				Object[] row = new Object[numCol];
				for (int i = 1; i < numCol; ++i) {
					if (i >= 22 && DBMS.MSSQL.equals(session.dbms)) {
						row[i - 1] = null;
					} else {
						if (intIndex.contains(i)) {
							row[i - 1] = rs.getInt(i);
						} else {
							row[i - 1] = rs.getString(i);
						}
					}
				}
				String table = (String) row[2];
				
				List<Object[]> rowList = metaDataCache.cache.get(table);
				if (rowList == null) {
					rowList = new LinkedList<Object[]>();
					metaDataCache.cache.put(table, rowList);
				}
				rowList.add(row);
			}
			metaDataCache.resultSetMetaData = new MDCResultSetMetaData(rsMetaData.getColumnCount(), names, types); 
			rs.close();

			if (metaDataCache.cache.isEmpty()) {
				metaDataCache.cache = null;
			}
			return metaDataCache;
		} catch (SQLException e) {
			_log.info(e.getMessage());
			return new MetaDataCache();
		}
	}

	public ResultSet forTable(String tableName) {
		if (cache == null) {
			return null;
		}

		List<Object[]> rowList = cache.get(tableName);
		if (rowList == null) {
			rowList = new ArrayList<Object[]>();
		}

		CachedResultSet result = new CachedResultSet(rowList);
		result.resultSetMetaData = resultSetMetaData;
		return result;
	}

	/**
	 * Reads meta data.
	 */
	private static void readMetaData(final MetaDataCache metaDataCache, Session session, String query,
			final Set<Integer> intIndex) throws SQLException {
		metaDataCache.cache = new HashMap<String, List<Object[]>>();
		boolean wasSilent = session.getSilent();
		session.setSilent(true);
		try {
			long rc = session.executeQuery(query, new Session.AbstractResultSetReader() {
				@Override
				public void readCurrentRow(ResultSet resultSet) throws SQLException {
					int numCol = getMetaData(resultSet).getColumnCount();
					Object[] row = new Object[numCol];
					for (int i = 1; i < numCol; ++i) {
						if (intIndex.contains(i)) {
							row[i - 1] = resultSet.getInt(i);
						} else {
							row[i - 1] = resultSet.getString(i);
						}
					}
					String table = (String) row[2];
					List<Object[]> rowList = metaDataCache.cache.get(table);
					if (rowList == null) {
						rowList = new LinkedList<Object[]>();
						metaDataCache.cache.put(table, rowList);
					}
					rowList.add(row);
				}
			});
			if (rc == 0 && session.dbms != DBMS.ORACLE) {
				throw new SQLException("Nothing found. Fall back to JDBC meta data.");
			}
			_log.info(rc + " rows read");
		} finally {
			session.setSilent(wasSilent);
		}
	}

	public static class MDCResultSetMetaData implements ResultSetMetaData {
		private final int numCol;
		private final String[] names;
		public final int[] types;

		public MDCResultSetMetaData(int numCol, String[] names, int[] types) {
			this.numCol = numCol;
			this.names = names;
			this.types = types;
		}

		@Override
		public <T> T unwrap(Class<T> iface) throws SQLException {
			return null;
		}

		@Override
		public boolean isWrapperFor(Class<?> iface) throws SQLException {
			return false;
		}

		@Override
		public boolean isWritable(int column) throws SQLException {
			return false;
		}

		@Override
		public boolean isSigned(int column) throws SQLException {
			return false;
		}

		@Override
		public boolean isSearchable(int column) throws SQLException {
			return false;
		}

		@Override
		public boolean isReadOnly(int column) throws SQLException {
			return true;
		}

		@Override
		public int isNullable(int column) throws SQLException {
			return 0;
		}

		@Override
		public boolean isDefinitelyWritable(int column) throws SQLException {
			return false;
		}

		@Override
		public boolean isCurrency(int column) throws SQLException {
			return false;
		}

		@Override
		public boolean isCaseSensitive(int column) throws SQLException {
			return false;
		}

		@Override
		public boolean isAutoIncrement(int column) throws SQLException {
			return false;
		}

		@Override
		public String getTableName(int column) throws SQLException {
			return null;
		}

		@Override
		public String getSchemaName(int column) throws SQLException {
			return null;
		}

		@Override
		public int getScale(int column) throws SQLException {
			return 0;
		}

		@Override
		public int getPrecision(int column) throws SQLException {
			return 0;
		}

		@Override
		public String getColumnTypeName(int column) throws SQLException {
			return null;
		}

		@Override
		public int getColumnType(int column) throws SQLException {
			return 0;
		}

		@Override
		public String getColumnName(int column) throws SQLException {
			return names[column - 1];
		}

		@Override
		public String getColumnLabel(int column) throws SQLException {
			return names[column - 1];
		}

		@Override
		public int getColumnDisplaySize(int column) throws SQLException {
			return 0;
		}

		@Override
		public int getColumnCount() throws SQLException {
			return numCol;
		}

		@Override
		public String getColumnClassName(int column) throws SQLException {
			return null;
		}

		@Override
		public String getCatalogName(int column) throws SQLException {
			return null;
		}
	}

	/**
	 * Result set of meta data.
	 */
	public static class CachedResultSet implements ResultSet {

		private final List<Object[]> rowList;
		private int index = -1;
		private boolean wasNull;
		private ResultSetMetaData resultSetMetaData;

		public CachedResultSet(List<Object[]> rowList) {
			this.rowList = rowList;
		}

		public CachedResultSet(List<Object[]> rowList, int numCol, String[] names, int[] types) {
			this.rowList = rowList;
			this.resultSetMetaData = new MDCResultSetMetaData(numCol, names, types);
		}

		public CachedResultSet(List<Object[]> rowList, ResultSetMetaData resultSetMetaData) {
			this.rowList = rowList;
			this.resultSetMetaData = resultSetMetaData;
		}

		public int getSize() {
			return rowList.size();
		}

		public List<Object[]> getRowList() {
			return rowList;
		}

		public CachedResultSet(ResultSet resultSet, Integer limit, Session session, Object cancellationContext)
				throws SQLException {
			this(resultSet, limit, session, cancellationContext, null, null);
		}

		public CachedResultSet(ResultSet resultSet, Integer limit, Session session, Object cancellationContext, int[] projection, String[] columnNames)
				throws SQLException {
			this.rowList = new ArrayList<Object[]>();
			ResultSetMetaData rmd = resultSet.getMetaData();
			CellContentConverter cellContentConverter = new CellContentConverter(rmd, session, session.dbms);
			final int numCol = projection == null? rmd.getColumnCount() : projection.length;
			while (resultSet.next()) {
				Object[] row = new Object[numCol];
				for (int i = 1; i <= numCol; ++i) {
					row[i - 1] = cellContentConverter.getObject(resultSet, projection == null? i - 1 : projection[i - 1]);
				}
				rowList.add(row);
				if (limit != null && rowList.size() > limit) {
					break;
				}
				if (cancellationContext != null) {
					CancellationHandler.checkForCancellation(cancellationContext);
				}
			}

			final String[] names = new String[numCol];
			final int[] types = new int[numCol];
			for (int i = 1; i <= numCol; ++i) {
				names[i - 1] = columnNames == null? rmd.getColumnName(projection == null? i - 1 : projection[i - 1]) : columnNames[i - 1];
				types[i - 1] = rmd.getColumnType(projection == null? i - 1 : projection[i - 1]);
			}
			resultSetMetaData = new MDCResultSetMetaData(numCol, names, types);
		}

		@Override
		public Object getObject(int columnIndex) throws SQLException {
			Object[] row = rowList.get(index);
			Object c;
			if (columnIndex > row.length) {
				c = null;
			}
			c = row[columnIndex - 1];
			wasNull = c == null;
			return c;
		}

		@Override
		public String getString(int columnIndex) throws SQLException {
			return (String) getObject(columnIndex);
		}

		@Override
		public int getInt(int columnIndex) throws SQLException {
			Integer c = (Integer) getObject(columnIndex);
			if (c == null) {
				return 0;
			}
			return c;
		}

		@Override
		public boolean getBoolean(int columnIndex) throws SQLException {
			int i = getInt(columnIndex);
			return i != 0 && !wasNull;
		}

		@Override
		public boolean wasNull() throws SQLException {
			return wasNull;
		}

		@Override
		public boolean next() throws SQLException {
			++index;
			return index < rowList.size();
		}

		@Override
		public void close() throws SQLException {
		}

		@Override
		public boolean isWrapperFor(Class<?> iface) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public <T> T unwrap(Class<T> iface) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public boolean absolute(int row) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void afterLast() throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void beforeFirst() throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void cancelRowUpdates() throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void clearWarnings() throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void deleteRow() throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public int findColumn(String columnLabel) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public boolean first() throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public Array getArray(int columnIndex) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public Array getArray(String columnLabel) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public InputStream getAsciiStream(int columnIndex) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public InputStream getAsciiStream(String columnLabel) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public BigDecimal getBigDecimal(int columnIndex) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public BigDecimal getBigDecimal(String columnLabel) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public BigDecimal getBigDecimal(int columnIndex, int scale) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public BigDecimal getBigDecimal(String columnLabel, int scale) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public InputStream getBinaryStream(int columnIndex) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public InputStream getBinaryStream(String columnLabel) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public Blob getBlob(int columnIndex) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public Blob getBlob(String columnLabel) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public boolean getBoolean(String columnLabel) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public byte getByte(int columnIndex) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public byte getByte(String columnLabel) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public byte[] getBytes(int columnIndex) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public byte[] getBytes(String columnLabel) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public Reader getCharacterStream(int columnIndex) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public Reader getCharacterStream(String columnLabel) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public Clob getClob(int columnIndex) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public Clob getClob(String columnLabel) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public int getConcurrency() throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public String getCursorName() throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public Date getDate(int columnIndex) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public Date getDate(String columnLabel) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public Date getDate(int columnIndex, Calendar cal) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public Date getDate(String columnLabel, Calendar cal) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public double getDouble(int columnIndex) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public double getDouble(String columnLabel) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public int getFetchDirection() throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public int getFetchSize() throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public float getFloat(int columnIndex) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public float getFloat(String columnLabel) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public int getHoldability() throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public int getInt(String columnLabel) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public long getLong(int columnIndex) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public long getLong(String columnLabel) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public ResultSetMetaData getMetaData() throws SQLException {
			if (resultSetMetaData != null) {
				return resultSetMetaData;
			}
			throw new UnsupportedOperationException();
		}

		@Override
		public Reader getNCharacterStream(int columnIndex) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public Reader getNCharacterStream(String columnLabel) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public NClob getNClob(int columnIndex) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public NClob getNClob(String columnLabel) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public String getNString(int columnIndex) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public String getNString(String columnLabel) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public Object getObject(String columnLabel) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public Object getObject(int columnIndex, Map<String, Class<?>> map) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public Object getObject(String columnLabel, Map<String, Class<?>> map) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public <T> T getObject(int columnIndex, Class<T> type) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public <T> T getObject(String columnLabel, Class<T> type) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public Ref getRef(int columnIndex) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public Ref getRef(String columnLabel) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public int getRow() throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public RowId getRowId(int columnIndex) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public RowId getRowId(String columnLabel) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public SQLXML getSQLXML(int columnIndex) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public SQLXML getSQLXML(String columnLabel) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public short getShort(int columnIndex) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public short getShort(String columnLabel) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public Statement getStatement() throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public String getString(String columnLabel) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public Time getTime(int columnIndex) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public Time getTime(String columnLabel) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public Time getTime(int columnIndex, Calendar cal) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public Time getTime(String columnLabel, Calendar cal) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public Timestamp getTimestamp(int columnIndex) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public Timestamp getTimestamp(String columnLabel) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public Timestamp getTimestamp(int columnIndex, Calendar cal) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public Timestamp getTimestamp(String columnLabel, Calendar cal) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public int getType() throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public URL getURL(int columnIndex) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public URL getURL(String columnLabel) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public InputStream getUnicodeStream(int columnIndex) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public InputStream getUnicodeStream(String columnLabel) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public SQLWarning getWarnings() throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void insertRow() throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public boolean isAfterLast() throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public boolean isBeforeFirst() throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public boolean isClosed() throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public boolean isFirst() throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public boolean isLast() throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public boolean last() throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void moveToCurrentRow() throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void moveToInsertRow() throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public boolean previous() throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void refreshRow() throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public boolean relative(int rows) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public boolean rowDeleted() throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public boolean rowInserted() throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public boolean rowUpdated() throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void setFetchDirection(int direction) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void setFetchSize(int rows) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateArray(int columnIndex, Array x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateArray(String columnLabel, Array x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateAsciiStream(int columnIndex, InputStream x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateAsciiStream(String columnLabel, InputStream x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateAsciiStream(int columnIndex, InputStream x, int length) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateAsciiStream(String columnLabel, InputStream x, int length) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateAsciiStream(int columnIndex, InputStream x, long length) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateAsciiStream(String columnLabel, InputStream x, long length) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateBigDecimal(int columnIndex, BigDecimal x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateBigDecimal(String columnLabel, BigDecimal x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateBinaryStream(int columnIndex, InputStream x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateBinaryStream(String columnLabel, InputStream x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateBinaryStream(int columnIndex, InputStream x, int length) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateBinaryStream(String columnLabel, InputStream x, int length) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateBinaryStream(int columnIndex, InputStream x, long length) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateBinaryStream(String columnLabel, InputStream x, long length) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateBlob(int columnIndex, Blob x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateBlob(String columnLabel, Blob x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateBlob(int columnIndex, InputStream inputStream) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateBlob(String columnLabel, InputStream inputStream) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateBlob(int columnIndex, InputStream inputStream, long length) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateBlob(String columnLabel, InputStream inputStream, long length) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateBoolean(int columnIndex, boolean x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateBoolean(String columnLabel, boolean x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateByte(int columnIndex, byte x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateByte(String columnLabel, byte x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateBytes(int columnIndex, byte[] x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateBytes(String columnLabel, byte[] x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateCharacterStream(int columnIndex, Reader x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateCharacterStream(String columnLabel, Reader reader) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateCharacterStream(int columnIndex, Reader x, int length) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateCharacterStream(String columnLabel, Reader reader, int length) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateCharacterStream(int columnIndex, Reader x, long length) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateCharacterStream(String columnLabel, Reader reader, long length) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateClob(int columnIndex, Clob x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateClob(String columnLabel, Clob x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateClob(int columnIndex, Reader reader) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateClob(String columnLabel, Reader reader) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateClob(int columnIndex, Reader reader, long length) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateClob(String columnLabel, Reader reader, long length) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateDate(int columnIndex, Date x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateDate(String columnLabel, Date x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateDouble(int columnIndex, double x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateDouble(String columnLabel, double x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateFloat(int columnIndex, float x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateFloat(String columnLabel, float x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateInt(int columnIndex, int x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateInt(String columnLabel, int x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateLong(int columnIndex, long x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateLong(String columnLabel, long x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateNCharacterStream(int columnIndex, Reader x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateNCharacterStream(String columnLabel, Reader reader) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateNCharacterStream(int columnIndex, Reader x, long length) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateNCharacterStream(String columnLabel, Reader reader, long length) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateNClob(int columnIndex, NClob nClob) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateNClob(String columnLabel, NClob nClob) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateNClob(int columnIndex, Reader reader) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateNClob(String columnLabel, Reader reader) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateNClob(int columnIndex, Reader reader, long length) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateNClob(String columnLabel, Reader reader, long length) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateNString(int columnIndex, String nString) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateNString(String columnLabel, String nString) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateNull(int columnIndex) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateNull(String columnLabel) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateObject(int columnIndex, Object x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateObject(String columnLabel, Object x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateObject(int columnIndex, Object x, int scaleOrLength) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateObject(String columnLabel, Object x, int scaleOrLength) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateRef(int columnIndex, Ref x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateRef(String columnLabel, Ref x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateRow() throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateRowId(int columnIndex, RowId x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateRowId(String columnLabel, RowId x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateSQLXML(int columnIndex, SQLXML xmlObject) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateSQLXML(String columnLabel, SQLXML xmlObject) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateShort(int columnIndex, short x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateShort(String columnLabel, short x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateString(int columnIndex, String x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateString(String columnLabel, String x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateTime(int columnIndex, Time x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateTime(String columnLabel, Time x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateTimestamp(int columnIndex, Timestamp x) throws SQLException {
			throw new UnsupportedOperationException();

		}

		@Override
		public void updateTimestamp(String columnLabel, Timestamp x) throws SQLException {
			throw new UnsupportedOperationException();

		}
	}

}
