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

package net.sf.jailer.datamodel;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.Date;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

import org.apache.log4j.Logger;

import net.sf.jailer.ExplainTool;
import net.sf.jailer.database.StatementExecutor;
import net.sf.jailer.util.SqlUtil;

/**
 * Factory for {@link PrimaryKey}s. Builds the universal primary key as a
 * super-set of all created primary key.
 * 
 * @author Wisser
 */
public class PrimaryKeyFactory {

	/**
	 * The logger.
	 */
	private static final Logger _log = Logger.getLogger(ExplainTool.class);;

	/**
	 * The sql logger.
	 */
	private static final Logger _sqllog = Logger.getLogger("sql");;

	/**
	 * {@link #getUniversalPrimaryKey()} closes the factory, no further creation
	 * of PKs is allowed then.
	 */
	private boolean closed = false;

	/**
	 * A super-set of all columns of created primary-keys.
	 */
	private PrimaryKey universalPrimaryKey = new PrimaryKey(
			new ArrayList<Column>());

	/**
	 * Constructs a new primary-key.
	 * 
	 * @return a newly created primary-key
	 * 
	 * @exception IllegalStateException
	 *                if factory is closed
	 */
	public PrimaryKey createPrimaryKey(List<Column> columns) {
		if (closed) {
			throw new IllegalStateException("factory is closed");
		}
		PrimaryKey primaryKey = new PrimaryKey(columns);

		int n = 0;
		if (!columns.isEmpty()) {
			for (int i = 0; i < universalPrimaryKey.getColumns().size(); ++i) {
				Column uPKColumn = universalPrimaryKey.getColumns().get(i);
				Column column = columns.get(n);
				if (uPKColumn.type.equals(column.type)) {
					if (column.length > 0 && column.length > uPKColumn.length) {
						// increase size
						universalPrimaryKey.getColumns().set(
								i,
								new Column(uPKColumn.name, uPKColumn.type,
										column.length));
					}
					++n;
					if (n >= columns.size()) {
						break;
					}
				}
			}
		}
		// add new columns to universal primary key
		for (; n < columns.size(); ++n) {
			Column column = columns.get(n);
			universalPrimaryKey.getColumns().add(
					new Column(createUniqueUPKName(), column.type,
							column.length));
		}
		return primaryKey;
	}

	/**
	 * Creates a unique name for a new universal primary key column.
	 * 
	 * @return a unique name for a new universal primary key column
	 */
	private String createUniqueUPKName() {
		return "PK" + universalPrimaryKey.getColumns().size();
	}

	/**
	 * Gets the primary-key to be used for the entity-table and closes the
	 * factory.
	 * 
	 * @param statementExecutor
	 *            for guessing null-values of columns
	 * @return the universal primary key
	 */
	public PrimaryKey getUniversalPrimaryKey(StatementExecutor statementExecutor) {
		closed = true;
		if (statementExecutor != null) {
			guessDummyValues(universalPrimaryKey, statementExecutor);
		}
		return universalPrimaryKey;
	}

	/**
	 * Guesses dummy-values for each column of a primary key.
	 * 
	 * @param primaryKey
	 *            the primary key
	 */
	private void guessDummyValues(PrimaryKey primaryKey,
			StatementExecutor statementExecutor) {
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
	private void guessDummyValues(Column column,
			StatementExecutor statementExecutor) {
		Calendar cal = Calendar.getInstance();
		cal.set(2000, 0, 30, 9, 59, 30);
		cal.set(Calendar.MILLISECOND, 456);
		long time = cal.getTimeInMillis();
		Object[] potNulls = new Object[] { "0", new Integer(0), new Character('0'),
				new Timestamp(time), new Date(time), new Byte((byte) 0),
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
		for (Object potNull : potNulls) {
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
								result[0] = resultSet.getObject(1);
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
						for (String pattern : timePattern) {
							try {
								SqlUtil.timestampFormat = pattern == null ? null
										: new SimpleDateFormat(pattern + ".SSS");
								insertNullValue(column, result[0],
										statementExecutor);
								break;
							} catch (Exception e) {
								_sqllog.info(e.getMessage());
							}
						}
						if (column.nullValue == null) {
							for (String pattern : timePattern) {
								try {
									SqlUtil.timestampFormat = pattern == null ? null
											: new SimpleDateFormat(pattern);
									insertNullValue(column, result[0],
											statementExecutor);
									break;
								} catch (Exception e) {
									_sqllog.info(e.getMessage());
								}
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
	private void insertNullValue(Column column, Object nullValue,
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
	private void log(String message) {
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
