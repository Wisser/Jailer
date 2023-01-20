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
package net.sf.jailer.modelbuilder;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.Session;
import net.sf.jailer.modelbuilder.MemorizedResultSet.MemorizedResultSetMetaData;
import net.sf.jailer.util.LogUtil;
import net.sf.jailer.util.Quoting;

/**
 * Reads database meta data directly from meta data views.
 *
 * @author Wisser
 */
public class MetaDataCache {

	/**
	 * The logger.
	 */
	private static final Logger _log = LoggerFactory.getLogger(MetaDataCache.class);

	/**
	 * The cached rows.
	 */
	private Map<String, List<Object[]>> cache;

	/**
	 * Meta data of cached row set.
	 */
	private MemorizedResultSetMetaData resultSetMetaData;

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

		_log.info("reading primary keys (may take some time)...");

		MetaDataCache metaDataCache = new MetaDataCache();
		try {
			Set<Integer> intIndex = new HashSet<Integer>(Arrays.asList(5));
			readMetaData(metaDataCache, session, primaryKeysQuery.replace("${SCHEMA}", schema), intIndex, 2);
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

		_log.info("reading index info (may take some time)...");

		MetaDataCache metaDataCache = new MetaDataCache();
		try {
			Set<Integer> intIndex = new HashSet<Integer>(Arrays.asList(4));
			readMetaData(metaDataCache, session, indexInfoQuery.replace("${SCHEMA}", schema), intIndex, 2);
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

		_log.info("reading imported keys (may take some time)...");

		MetaDataCache metaDataCache = new MetaDataCache();
		try {
			Set<Integer> intIndex = new HashSet<Integer>(Arrays.asList(9, 10, 11, 14));
			readMetaData(metaDataCache, session, importedKeysQuery.replace("${SCHEMA}", schema), intIndex, 6);
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
	public static MetaDataCache readColumns(Session session, String schema) {
		_log.info("reading columns (may take some time)...");
		
		MetaDataCache metaDataCache = new MetaDataCache();
		ResultSet rs;
		try {
			if (DBMS.MySQL.equals(session.dbms)) {
				rs = session.getMetaData().getColumns(schema, null, "%", "%");
			} else {
				rs = session.getMetaData().getColumns(null, schema, "%", "%");
			}
			Set<Integer> intIndex = new HashSet<Integer>(Arrays.asList(5, 7, 9, 10, 11, 14, 15, 16, 17, 22));

			metaDataCache.cache = new HashMap<String, List<Object[]>>();
			ResultSetMetaData rsMetaData = rs.getMetaData();
			int numCol = rsMetaData.getColumnCount();
			String[] names = new String[numCol];
			String[] typeNames = new String[numCol];
			int[] types = new int[numCol];
			for (int i = 0; i < numCol; ++i) {
				names[i] = rsMetaData.getColumnName(i + 1);
				types[i] = rsMetaData.getColumnType(i + 1);
				typeNames[i] = ""; // not needed
			}

			while (rs.next()) {
//				1.TABLE_CAT String => table catalog (may be null) 
//				2.TABLE_SCHEM String => table schema (may be null) 
//				3.TABLE_NAME String => table name 
//				4.COLUMN_NAME String => column name 
				if (!Quoting.equalsWROSearchPattern(schema, rs.getString(1), rs.getString(2))) {
					continue;
				}
				
				Object[] row = new Object[numCol];
				for (int i = 1; i <= numCol; ++i) {
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
			metaDataCache.resultSetMetaData = new MemorizedResultSetMetaData(numCol, names, types, typeNames);
			rs.close();
			if (metaDataCache.cache.isEmpty()) {
				metaDataCache.cache = null;
			}
			return metaDataCache;
		} catch (SQLException e) {
			if (!session.isDown()) {
				_log.info(e.getMessage());
				LogUtil.warn(e);
				session.reconnect();
			}
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

		MemorizedResultSet result = new MemorizedResultSet(rowList);
		result.resultSetMetaData = resultSetMetaData;
		return result;
	}

	/**
	 * Reads meta data.
	 */
	private static void readMetaData(final MetaDataCache metaDataCache, Session session, String query,
			final Set<Integer> intIndex, final int tableIndex) throws SQLException {
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
					String table = (String) row[tableIndex];
					List<Object[]> rowList = metaDataCache.cache.get(table);
					if (rowList == null) {
						rowList = new LinkedList<Object[]>();
						metaDataCache.cache.put(table, rowList);
					}
					rowList.add(row);
				}
			});
			if (rc == 0 && !DBMS.ORACLE.equals(session.dbms)) {
				throw new SQLException("Nothing found. Fall back to JDBC meta data.");
			}
			_log.info(rc + " rows read");
		} finally {
			session.setSilent(wasSilent);
		}
	}
	
	public String info(String subject) {
		try {
			Set<String> schema = new HashSet<String>();
			cache.forEach((k, v) -> v.forEach(c-> schema.add(c[0] + "." + c[1])));
			return "MetaDataCache [cache=" + cache.entrySet().stream().map(e -> e.getValue().size()).collect(Collectors.summarizingInt(n -> n)) + "/" + schema + "]" +
				cache.get(subject);
		} catch (Throwable t) {
			return t.getMessage();
		}
	}
}
