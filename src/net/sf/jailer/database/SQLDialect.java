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

import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.Date;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;

import net.sf.jailer.ExplainTool;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.PrimaryKey;
import net.sf.jailer.util.SqlUtil;

import org.apache.log4j.Logger;

/**
 * Description of the DBMS's SQL dialect.
 * 
 * @author Wisser
 */
public class SQLDialect {

	/**
	 * The logger.
	 */
	private static final Logger _log = Logger.getLogger(ExplainTool.class);;

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
	public final boolean supportsMultiRowInserts;
	
	public static enum UPSERT_MODE { 
		DB2("Select * From (values (1, 2), (3, 4)) as Q(c1, c2) Where not exists (Select * from JL_TMP T Where T.c1=Q.c1)"), 
		FROM_DUAL("Select 1, 2 From dual where not exists(Select * from JL_TMP T where T.c1=1)"),
		FROM_JL_DUAL("Select 1, 2 From JL_DUAL where not exists(Select * from JL_TMP T where T.c1=1)"),
		ORACLE("MERGE INTO JL_TMP T " +
                              "USING (SELECT 1 c1, 2 c2 from dual) incoming " +
                              "ON (T.c1 = incoming.c1) " +
                              "WHEN MATCHED THEN " +
                              "UPDATE SET T.c2 = incoming.c2 " +
                              "WHEN NOT MATCHED THEN " +
                              "INSERT (T.c1, T.c2) " +
                              "VALUES (incoming.c1, incoming.c2)");
		
		public final String testSQL;
		UPSERT_MODE(String testSQL) {
			this.testSQL = testSQL;
		}
	};
	
	/**
	 * Upsert mode.
	 */
	public final UPSERT_MODE upsertMode;
	
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
	private SQLDialect(String name, boolean supportsMultiRowInserts, boolean needsValuesKeywordForDeletes, UPSERT_MODE upsertMode) {
		this.name = name;
		this.supportsMultiRowInserts = supportsMultiRowInserts;
		this.needsValuesKeywordForDeletes = needsValuesKeywordForDeletes;
		this.upsertMode = upsertMode;
	}
	
	/**
	 * Named dialects.
	 */
	private static List<SQLDialect> sqlDialects = new ArrayList<SQLDialect>();
	static {
		sqlDialects.add(new SQLDialect("DB2", true, true, UPSERT_MODE.DB2));
		sqlDialects.add(new SQLDialect("ORACLE_10", false, false, UPSERT_MODE.ORACLE));
		sqlDialects.add(new SQLDialect("D7", true, false, UPSERT_MODE.ORACLE));
		sqlDialects.add(new SQLDialect("ORACLE", false, false, UPSERT_MODE.FROM_DUAL));
		sqlDialects.add(new SQLDialect("MYSQL", true, false, UPSERT_MODE.FROM_DUAL));
		sqlDialects.add(new SQLDialect("POSTGRESQL", true, true, UPSERT_MODE.DB2));

		sqlDialects.add(new SQLDialect("D1", true, true, UPSERT_MODE.ORACLE));
		sqlDialects.add(new SQLDialect("D2", true, false, UPSERT_MODE.DB2));
		sqlDialects.add(new SQLDialect("D3", true, true, UPSERT_MODE.FROM_DUAL));
		sqlDialects.add(new SQLDialect("D4", true, false, UPSERT_MODE.FROM_JL_DUAL));
		sqlDialects.add(new SQLDialect("D5", true, true, UPSERT_MODE.FROM_JL_DUAL));

		sqlDialects.add(new SQLDialect("D6", false, true, UPSERT_MODE.DB2));
		sqlDialects.add(new SQLDialect("D8", true, false, UPSERT_MODE.FROM_DUAL));
		sqlDialects.add(new SQLDialect("D9", false, false, UPSERT_MODE.FROM_DUAL));
		sqlDialects.add(new SQLDialect("D10", false, true, UPSERT_MODE.DB2));

		sqlDialects.add(new SQLDialect("D11", false, true, UPSERT_MODE.ORACLE));
		sqlDialects.add(new SQLDialect("D12", false, false, UPSERT_MODE.DB2));
		sqlDialects.add(new SQLDialect("D13", false, true, UPSERT_MODE.FROM_DUAL));
		sqlDialects.add(new SQLDialect("D14", false, false, UPSERT_MODE.FROM_JL_DUAL));
		sqlDialects.add(new SQLDialect("D15", false, true, UPSERT_MODE.FROM_JL_DUAL));

		currentDialect = sqlDialects.get(0);
	}
	
	/**
	 * Guesses SQL dialect and dummy-values for each column of a primary key.
	 * 
	 * @param primaryKey
	 *            the primary key
	 */
	public static void guessDialect(PrimaryKey primaryKey,
			StatementExecutor statementExecutor) {
		log("begin guessing SQL dialect");
		
		String drop = "DROP TABLE JL_TMP";
		String create = "CREATE TABLE JL_TMP(c1 INTEGER, c2 INTEGER)";
		try {
			statementExecutor.execute(drop);
		} catch (Exception e) {
			_sqllog.info(e.getMessage());
		}
		try {
			statementExecutor.execute(create);
		} catch (Exception e) {
			_sqllog.info(e.getMessage());
		}
		try {
			statementExecutor.execute("DROP TABLE JL_DUAL");
		} catch (Exception e) {
			_sqllog.info(e.getMessage());
		}
		try {
			statementExecutor.execute("CREATE TABLE JL_DUAL(D INTEGER)");
		} catch (Exception e) {
			_sqllog.info(e.getMessage());
		}
		
		for (SQLDialect sqlDialect: sqlDialects) {
			boolean ok = true;
			try {
				String values = sqlDialect.needsValuesKeywordForDeletes? "values " : "";
				statementExecutor.execute("DELETE FROM JL_TMP where (c1, c2) IN (" + values + "(1,2), (3,4))");
			} catch (Exception e) {
				ok = false;
				_sqllog.info(e.getMessage());
			}
			if (!ok) {
				continue;
			}
			boolean multiRow;
			try {
				statementExecutor.execute("INSERT INTO JL_TMP(c1, c2) values (1,2), (3,4)");
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
				statementExecutor.execute(sqlDialect.upsertMode.testSQL);
			} catch (Exception e) {
				ok = false;
				_sqllog.info(e.getMessage());
			}
			if (!ok) {
				continue;
			}
			
			currentDialect = sqlDialect;
			log("SQL dialect is " + sqlDialect.name);
			break;
		}
		try {
			statementExecutor.execute("DROP TABLE JL_DUAL");
		} catch (Exception e) {
			_sqllog.info(e.getMessage());
		}
		try {
			statementExecutor.execute(drop);
		} catch (Exception e) {
			_sqllog.info(e.getMessage());
		}
		log("end guessing SQL dialect");
		
		log("begin guessing dummy-values");
		for (Column column : primaryKey.getColumns()) {
			guessDummyValues(column, statementExecutor);
		}
		// force date format guessing
		guessDummyValues(new Column("C", "DATE", 0), statementExecutor);
		guessDummyValues(new Column("C", "TIMESTAMP", 0), statementExecutor);
		log("end guessing dummy values");
	}

	/**
	 * Guesses dummy-values for each column of a primary key.
	 * 
	 * @param primaryKey
	 *            the primary key
	 */
	private static void guessDummyValues(Column column,
			StatementExecutor statementExecutor) {
		Calendar cal = Calendar.getInstance();
		cal.set(2000, 0, 30, 9, 59, 30);
		cal.set(Calendar.MILLISECOND, 456);
		long time = cal.getTimeInMillis();
		final Timestamp timestamp = new Timestamp(time);
		timestamp.setNanos(123456789);
		Object[] potNulls = new Object[] { "0", new Integer(0), new Character('0'),
				timestamp, new Date(time), new Byte((byte) 0),
				BigInteger.ZERO, new BigDecimal(0), new Double(0.0),
				new Float(0.0f) };

		String drop = "DROP TABLE JL_TMP";
		String create = "CREATE TABLE JL_TMP(" + column + ")";
		try {
			statementExecutor.execute(drop);
		} catch (Exception e) {
			_sqllog.info(e.getMessage());
		}
		try {
			statementExecutor.execute(create);
		} catch (Exception e) {
			_sqllog.info(e.getMessage());
		}
		for (final Object potNull : potNulls) {
			try {
				final Object[] result = new Object[1];
				statementExecutor.executeUpdate("DELETE FROM JL_TMP");
				statementExecutor
						.executeUpdate("INSERT INTO JL_TMP(" + column.name
								+ ") VALUES(?)", new Object[] { potNull });
				statementExecutor.executeQuery("SELECT " + column.name
						+ " FROM JL_TMP",
						new StatementExecutor.ResultSetReader() {
							public void readCurrentRow(ResultSet resultSet)
									throws SQLException {
								result[0] = SqlUtil.getObject(resultSet, 1, new HashMap<Integer, Integer>());
							}

							public void close() {
							}
						});
				if (result[0] != null) {
					if (result[0] instanceof Date) {
						for (String pattern : datePattern) {
							try {
								SqlUtil.dateFormat = pattern == null ? null
										: new SimpleDateFormat(pattern);
								insertNullValue(column, result[0],
										statementExecutor);
								break;
							} catch (Exception e) {
								_sqllog.info(e.getMessage());
							}
						}
					} else if (result[0] instanceof Timestamp) {
						for (int i = 0; i < 3; ++i) {
							for (char nanoSep: new char[] { '.', ',', '-', ':' }) {
								SqlUtil.nanoSep = nanoSep;
								for (String pattern : timePattern) {
									try {
										SqlUtil.appendNanosToTimestamp = i != 2;
										SqlUtil.appendMillisToTimestamp = i != 0;
										SqlUtil.timestampFormat = pattern == null ? null
												: new SimpleDateFormat(pattern);
										insertNullValue(column, result[0],
												statementExecutor);
										break;
									} catch (Exception e) {
										_sqllog.info(e.getMessage());
									}
								}
								if (column.nullValue != null) {
									break;
								}
							}
							if (column.nullValue != null) {
								break;
							}
						}
					} else {
						try {
							insertNullValue(column, result[0], statementExecutor);
						} catch (Exception e) {
							_sqllog.info(e.getMessage());
						}
					}
					if (column.nullValue != null) {
						log("dummy value for " + column + " is " + column.nullValue);
						break;
					}
				}
			} catch (Exception e) {
				_sqllog.info(e.getMessage());
			}
		}
		try {
			statementExecutor.execute(drop);
		} catch (Exception e) {
			_sqllog.info(e.getMessage());
		}
	}

	/**
	 * Inserts a null value into JL_TMP.
	 * 
	 * @param column
	 *            the column to insert into
	 * @param nullValue
	 *            the null value
	 */
	private static void insertNullValue(Column column, Object nullValue,
			StatementExecutor statementExecutor) throws SQLException {
		String nv = SqlUtil.toSql(nullValue);
		statementExecutor.executeUpdate("DELETE FROM JL_TMP");
		statementExecutor.executeUpdate("INSERT INTO JL_TMP(" + column.name
				+ ") VALUES(" + nv + ")");
		column.nullValue = nv;
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

	private static String[] datePattern = new String[] { null, "dd-MM-yyyy",
			"dd/MM/yyyy", "dd.MM.yyyy", "dd.MMM.yyyy", "dd MMMM yyyy",
			"MM-dd-yyyy", "MM/dd/yyyy", "MMM d, yyyy", "yyyy.d.M",
			"dd MMM yyyy", "dd-MMM-yyyy", "dd.MM.yyyy.", "d MMM yyyy",
			"d MMM, yyyy", "d-MMM-yyyy", "d/MMM/yyyy", "d/MM/yyyy",
			"d.MM.yyyy", "d.M.yyyy", "Gy.MM.dd", "yyyy-M-d", "yyyy/M/d",
			"yyyy. M. d", "yyyy.M.d", "yyyy'?'M'?'d'?'", "yyyy-MM-dd",
			"yyyy/MM/dd", "yyyy.MM.dd", "yyyy.MM.dd.", "yyyy MMM d",
			"yyyy-MMM-dd", "dd MMM yy", "dd-MMM-yy", "MM d, yy", null };

	private static String[] timePattern = new String[] { null,
			"dd/MM/yyyy HH:mm:ss", "dd.MM.yyyy HH:mm:ss",
			"dd.MM.yyyy. HH.mm.ss", 
			"dd.MM.yyyy H:mm:ss",
			"yyyy-MM-dd HH:mm:ss",
			"yyyy.MM.dd HH:mm:ss", "yyyy/MM/dd H:mm:ss", "yyyy.MM.dd. H:mm:ss",
			"yyyy-MMM-dd HH:mm:ss",

			"dd/MM/yyyy HH.mm.ss", "dd.MM.yyyy HH.mm.ss",
			"dd.MM.yyyy. HH.mm.ss", "dd.MM.yyyy H.mm.ss",
			"yyyy-MM-dd HH.mm.ss",
			"yyyy.MM.dd HH.mm.ss", "yyyy/MM/dd H.mm.ss", "yyyy.MM.dd. H.mm.ss",
			"yyyy-MMM-dd HH.mm.ss",

			"dd/MM/yyyy-HH.mm.ss", "dd.MM.yyyy-HH.mm.ss",
			"dd.MM.yyyy.-HH.mm.ss", 
			"dd.MM.yyyy-H.mm.ss",
			"yyyy-MM-dd-HH.mm.ss",
			"yyyy.MM.dd-HH.mm.ss", "yyyy/MM/dd-H.mm.ss", "yyyy.MM.dd.-H.mm.ss",
			"yyyy-MMM-dd-HH.mm.ss",

			"dd MMM yy H:mm:ss",
			"dd MMM yyyy HH:mm:ss", "dd-MMM-yyyy HH:mm:ss",
			"dd.MMM.yyyy HH:mm:ss", "dd-MMM-yyyy H:mm:ss",
			"dd-MM-yyyy HH:mm:ss",
			"d MMM yyyy HH:mm:ss", "d-MMM-yyyy HH:mm:ss", "d MMM yyyy H:mm:ss",
			"d MMM yyyy, H:mm:ss", "d-MMM-yyyy H:mm:ss", "d-MMM-yyyy H.mm.ss",
			"d/MMM/yyyy H:mm:ss",
			"d/MM/yyyy HH:mm:ss", "d.MM.yyyy H:mm:ss",
			"d.M.yyyy HH:mm:", "d.M.yyyy HH:mm:ss", "d.M.yyyy H:mm:ss",
			"d.M.yyyy H.mm.ss", "Gy.MM.dd H:mm:ss", "HH:mm:ss dd-MM-yyyy",
			"HH:mm:ss dd/MM/yyyy",
			"yyyy.d.M HH:mm:ss",
			"yyyy.M.d HH.mm.ss",
			"yyyy MMM d HH:mm:ss", null };

}
