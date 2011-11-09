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
package net.sf.jailer.database;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import net.sf.jailer.CommandLineParser;
import net.sf.jailer.Configuration;
import net.sf.jailer.Jailer;
import net.sf.jailer.datamodel.PrimaryKey;
import net.sf.jailer.util.SqlUtil;

import org.apache.log4j.Logger;

/**
 * Description of the DBMS's SQL dialect.
 * 
 * @author Ralf Wisser
 */
public class SQLDialect {

	/**
	 * Name of config table.
	 */
	public static final String CONFIG_TABLE_ = "JAILER_CONFIG";
	
	/**
	 * Name of temp table.
	 */
	public static final String TMP_TABLE_ = "JAILER_TMP";
	
	/**
	 * Name of dual table.
	 */
	public static final String DUAL_TABLE = "JAILER_DUAL";
	
	/**
	 * The logger.
	 */
	private static final Logger _log = Logger.getLogger(SQLDialect.class);

	/**
	 * The sql logger.
	 */
	private static final Logger _sqllog = Logger.getLogger("sql");;

	/**
	 * Dialect name.
	 */
	private final String name;
	
	/**
	 * 'DELETE FROM T where PK IN (values ...)'
	 * or
	 * 'DELETE FROM T where PK IN (...)'
	 */
	public final boolean needsValuesKeywordForDeletes;
	public final boolean supportsInClauseForDeletes;
	public final boolean supportsMultiRowInserts;
	
    /**
     * Gets table reference for DML statements for a given working table.
     * 
     * @param tableName the working table
     * @param session holds connection to DBMS
     * @return table reference for the working table
     */
    public static String dmlTableReference(String tableName, Session session) {
    	TemporaryTableManager tableManager = null;
    	TemporaryTableScope temporaryTableScope = CommandLineParser.getInstance().getTemporaryTableScope();
		if (temporaryTableScope == TemporaryTableScope.SESSION_LOCAL) {
			tableManager = Configuration.forDbms(session).sessionTemporaryTableManager;
		}
		if (temporaryTableScope == TemporaryTableScope.TRANSACTION_LOCAL) {
			tableManager = Configuration.forDbms(session).transactionTemporaryTableManager;
		}
		if (tableManager != null) {
			return tableManager.getDmlTableReference(tableName);
		}
		return tableName;
    }

	/**
	 * Statements for upserts (overwrites).
	 */
	public static enum UPSERT_MODE { 
		DB2("Select * From (values (1, 2), (3, 4)) as Q(c1, c2) Where not exists (Select * from $ T Where T.c1=Q.c1)"), 
		FROM_DUAL("Select 1, 2 From dual where not exists(Select * from $ T where T.c1=1)"),
		FROM_JL_DUAL("Select 1, 2 From $ where not exists(Select * from $ T where T.c1=1)"),
		ORACLE("MERGE INTO $ T " +
                              "USING (SELECT 1 c1, 2 c2 from dual) incoming " +
                              "ON (T.c1 = incoming.c1) " +
                              "WHEN MATCHED THEN " +
                              "UPDATE SET T.c2 = incoming.c2 " +
                              "WHEN NOT MATCHED THEN " +
                              "INSERT (T.c1, T.c2) " +
                              "VALUES (incoming.c1, incoming.c2)");
		
		public final String testSQL_;
		UPSERT_MODE(String testSQL) {
			this.testSQL_ = testSQL;
		}
	};
	
	/**
	 * Upsert mode.
	 */
	public final UPSERT_MODE upsertMode;
	
	/**
	 * On oracle, treat DATA as TIMESTAMP.
	 */
	public static boolean treatDateAsTimestamp = false;
	
	/**
	 * Binary data literal pattern.
	 */
//	public static String binaryPattern = "x'%s'";
	
	/**
	 * Empty CLOB as SQL literal, <code>null</code> if DBMS does not support CLOB literals.
	 * For instance: "empty_clob()"
	 */
//	public static String emptyCLOBValue = null;

	/**
	 * Empty BLOB as SQL literal, <code>null</code> if DBMS does not support BLOB literals.
	 * For instance: "empty_blob()"
	 */
//	public static String emptyBLOBValue = null;

	/**
	 * Current dialect.
	 */
	public static SQLDialect currentDialect;

	/**
	 * Constructor.
	 * 
	 * @param name dialect name
	 * @param needsValuesKeywordForDeletes see {@link #needsValuesKeywordForDeletes}
	 * @param upsertMode see {@link #upsertMode}
	 */
	private SQLDialect(String name, boolean supportsMultiRowInserts, boolean needsValuesKeywordForDeletes, boolean supportsInClauseForDeletes, UPSERT_MODE upsertMode) {
		this.name = name;
		this.supportsMultiRowInserts = supportsMultiRowInserts;
		this.needsValuesKeywordForDeletes = needsValuesKeywordForDeletes;
		this.supportsInClauseForDeletes = supportsInClauseForDeletes;
		this.upsertMode = upsertMode;
	}
	
	/**
	 * Named dialects.
	 */
	private static List<SQLDialect> sqlDialects = new ArrayList<SQLDialect>();

	static {
		sqlDialects.add(new SQLDialect("DB2", true, true, true, UPSERT_MODE.DB2));
		sqlDialects.add(new SQLDialect("ORACLE_10", false, false, true, UPSERT_MODE.ORACLE));
		sqlDialects.add(new SQLDialect("D7", true, false, true, UPSERT_MODE.ORACLE));
		sqlDialects.add(new SQLDialect("ORACLE", false, false, true, UPSERT_MODE.FROM_DUAL));
		sqlDialects.add(new SQLDialect("MYSQL", true, false, true, UPSERT_MODE.FROM_DUAL));
		sqlDialects.add(new SQLDialect("POSTGRESQL", true, true, true, UPSERT_MODE.DB2));

		sqlDialects.add(new SQLDialect("D1", true, true, true, UPSERT_MODE.ORACLE));
		sqlDialects.add(new SQLDialect("D2", true, false, true, UPSERT_MODE.DB2));
		sqlDialects.add(new SQLDialect("D3", true, true, true, UPSERT_MODE.FROM_DUAL));

		sqlDialects.add(new SQLDialect("D6", false, true, true, UPSERT_MODE.DB2));
		sqlDialects.add(new SQLDialect("D8", true, false, true, UPSERT_MODE.FROM_DUAL));
		sqlDialects.add(new SQLDialect("D9", false, false, true, UPSERT_MODE.FROM_DUAL));
		sqlDialects.add(new SQLDialect("D10", false, true, true, UPSERT_MODE.DB2));

		sqlDialects.add(new SQLDialect("D11", false, true, true, UPSERT_MODE.ORACLE));
		sqlDialects.add(new SQLDialect("D12", false, false, true, UPSERT_MODE.DB2));
		sqlDialects.add(new SQLDialect("D13", false, true, true, UPSERT_MODE.FROM_DUAL));

		sqlDialects.add(new SQLDialect("D16", true, true, true, UPSERT_MODE.DB2));
		
		sqlDialects.add(new SQLDialect("D4", true, false, true, UPSERT_MODE.FROM_JL_DUAL));
		sqlDialects.add(new SQLDialect("D5", true, true, true, UPSERT_MODE.FROM_JL_DUAL));
		sqlDialects.add(new SQLDialect("D14", false, false, true, UPSERT_MODE.FROM_JL_DUAL));
		sqlDialects.add(new SQLDialect("D15", false, true, true, UPSERT_MODE.FROM_JL_DUAL));

		sqlDialects.add(new SQLDialect("D17", true, false, false, UPSERT_MODE.DB2));
		sqlDialects.add(new SQLDialect("D19", true, false, false, UPSERT_MODE.FROM_DUAL));
		sqlDialects.add(new SQLDialect("D20", true, false, false, UPSERT_MODE.ORACLE));
		sqlDialects.add(new SQLDialect("D21", false, false, false, UPSERT_MODE.FROM_DUAL));
		sqlDialects.add(new SQLDialect("D22", false, false, false, UPSERT_MODE.DB2));
		sqlDialects.add(new SQLDialect("D24", false, false, false, UPSERT_MODE.ORACLE));
		sqlDialects.add(new SQLDialect("D18", true, false, false, UPSERT_MODE.FROM_JL_DUAL));
		sqlDialects.add(new SQLDialect("D23", false, false, false, UPSERT_MODE.FROM_JL_DUAL));

		currentDialect = sqlDialects.get(0);
	}
	
	/**
	 * Guesses SQL dialect and dummy-values for each column of a primary key.
	 * 
	 * @param primaryKey
	 *            the primary key
	 */
	public static void guessDialect(PrimaryKey primaryKey,
				Session session) {

		treatDateAsTimestamp = false;
		
		if (session.dbms == DBMS.ORACLE) {
			treatDateAsTimestamp = true;
			_log.info("DATE is treated as TIMESTAMP");
		}
		
		session.setSilent(true);
		String dialectName = readConfigValue("sqldialect", session);
		SQLDialect dialect = null;
		if (dialectName != null) {
			for (SQLDialect sqlDialect: sqlDialects) {
				if (sqlDialect.name.equals(dialectName)) {
					dialect = sqlDialect;
					break;
				}
			}	
		}
		
		if (dialect != null) {
			currentDialect = dialect;
			log("SQL dialect is " + dialect.name);
		} else {
			log("begin guessing SQL dialect");
			
			boolean canDeleteWithValuesKeyword = false;
			boolean canDeleteWithoutValuesKeyword = false;
			try {
				String values = "values ";
				session.execute("DELETE FROM " + SQLDialect.dmlTableReference(TMP_TABLE_, session) + " where (c1, c2) IN (" + values + "(1,2), (3,4))");
				canDeleteWithValuesKeyword = true;
			} catch (Exception e) {
				_sqllog.info(e.getMessage());
			}
			try {
				String values = "";
				session.execute("DELETE FROM " + SQLDialect.dmlTableReference(TMP_TABLE_, session) + " where (c1, c2) IN (" + values + "(1,2), (3,4))");
				canDeleteWithoutValuesKeyword = true;
			} catch (Exception e) {
				_sqllog.info(e.getMessage());
			}
			
			for (SQLDialect sqlDialect: sqlDialects) {
				boolean ok = true;
				if (session.dbms != DBMS.SYBASE) {
					if (sqlDialect.supportsInClauseForDeletes) {
						if ((!canDeleteWithoutValuesKeyword) && (!canDeleteWithValuesKeyword)) {
							ok = false;
						} else if (sqlDialect.needsValuesKeywordForDeletes && !canDeleteWithValuesKeyword) {
							ok = false;
						} else if ((!sqlDialect.needsValuesKeywordForDeletes) && !canDeleteWithoutValuesKeyword) {
							ok = false;
						}
					} else {
						if (canDeleteWithoutValuesKeyword || canDeleteWithValuesKeyword) {
							ok = false;
						}
					}
				}
				if (!ok) {
					continue;
				}
				boolean multiRow;
				try {
					session.execute("INSERT INTO " + SQLDialect.dmlTableReference(TMP_TABLE_, session) + "(c1, c2) values (1,2), (3,4)");
					multiRow = true;
				} catch (Exception e) {
					multiRow = false;
					_sqllog.info(e.getMessage());
				}
				if (multiRow != sqlDialect.supportsMultiRowInserts) {
					ok = false;
				}
				if (!ok) {
					continue;
				}
				try {
					session.executeQuery(sqlDialect.upsertMode.testSQL_.replaceAll("\\$", SQLDialect.dmlTableReference(TMP_TABLE_, session)), new Session.AbstractResultSetReader() {
						@Override
						public void readCurrentRow(ResultSet resultSet) throws SQLException {
						}
					});
				} catch (Exception e) {
					ok = false;
					_sqllog.info(e.getMessage());
				}
				if (!ok) {
					continue;
				}
				
				currentDialect = sqlDialect;
				setConfigValue("sqldialect", sqlDialect.name, session);
				log("SQL dialect is " + sqlDialect.name);
				break;
			}
		
			log("end guessing SQL dialect");
		}
		
		session.setSilent(false);
		
//		Configuration c = Configuration.forDbms(session);
//		SqlUtil.dateFormat = c.dateFormat;
//		SqlUtil.nanoSep = c.nanoSep;
//		SqlUtil.appendNanosToTimestamp = c.appendNanosToTimestamp;
//		SqlUtil.appendMillisToTimestamp = c.appendMillisToTimestamp;
//		SqlUtil.useToTimestampFunction = c.useToTimestampFunction;
//		SqlUtil.timestampFormat = c.timestampFormat;
//		emptyCLOBValue = c.emptyCLOBValue;
//		emptyBLOBValue = c.emptyBLOBValue;
//		binaryPattern = c.binaryPattern;	
	}

	/**
	 * Reads value from " + CONFIG_TABLE + " table.
	 * 
	 * @param key key for value lookup
	 * @param session for executing sql statements
	 * @return value for given key or <code>null</code> if no value for given key can be found
	 */
	private static String readConfigValue(String key, Session session) {
		try {
			final String[] value = new String[] { null };
			session.executeQuery("Select jvalue from " + SQLDialect.dmlTableReference(CONFIG_TABLE_, session) + " where jversion='" + Jailer.VERSION + "' and jkey='" + key + "'", new Session.ResultSetReader() {
				public void readCurrentRow(ResultSet resultSet) throws SQLException {
					value[0] = resultSet.getString(1);
				}
				public void close() {
				}
			});
			return value[0];
		} catch (Exception e) {
			return null;
		}
	}
	
	/**
	 * Sets value from " + CONFIG_TABLE + " table.
	 * 
	 * @param key key for value
	 * @param value for given key or <code>null</code> if no value for given key can be found
	 * @param session for executing sql statements
	 */
	private static void setConfigValue(String key, String value, Session session) {
		try {
			session.executeUpdate("Delete from " + SQLDialect.dmlTableReference(CONFIG_TABLE_, session) + " where jversion='" + Jailer.VERSION + "' and jkey=" + SqlUtil.toSql(key, session));
		} catch (Exception e) {
		}
		try {
			session.executeUpdate("Insert into " + SQLDialect.dmlTableReference(CONFIG_TABLE_, session) + "(jversion, jkey, jvalue) values ('" + Jailer.VERSION + "', " + SqlUtil.toSql(key, session) + ", " + SqlUtil.toSql(value, session) + ")");
		} catch (Exception e) {
		}
	}
	
	/**
	 * Logs a message.
	 * 
	 * @param message
	 *            the message
	 */
	private static void log(String message) {
		_log.info(message);
		_sqllog.info(message);
	}

}
