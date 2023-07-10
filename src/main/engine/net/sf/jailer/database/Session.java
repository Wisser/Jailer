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
package net.sf.jailer.database;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.Semaphore;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.sql.DataSource;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.util.CancellationException;
import net.sf.jailer.util.CancellationHandler;
import net.sf.jailer.util.CellContentConverter;
import net.sf.jailer.util.LogUtil;


/**
 * Manages database sessions on a 'per thread' basis.
 * Executes SQL-Statements in the context of a session.
 *
 * @author Ralf Wisser
 */
public class Session {

	/**
	 * Hold a connection for each thread.
	 */
	protected ThreadLocal<Connection> connection = new ThreadLocal<Connection>();

	/**
	 * Holds all connections.
	 */
	private final List<Connection> connections = Collections.synchronizedList(new ArrayList<Connection>());

	/**
	 * The session in which temporary tables lives, if any.
	 */
	private Connection temporaryTableSession = null;

	/**
	 * Shared scope of temporary tables.
	 */
	private WorkingTableScope temporaryTableScope;

	/**
	 * Scope of temporary tables.
	 */
	public final WorkingTableScope scope;

	/**
	 * No SQL-Exceptions will be logged in silent mode.
	 */
	private boolean silent = false;
	
	/**
	 * Last known activity time per connection.
	 */
	private Map<Connection, Long> lastConnectionActiviyTimeStamp = Collections.synchronizedMap(new HashMap<Connection, Long>());

	private final boolean transactional;
	private Map<Long, Integer> connectionCount = new HashMap<Long, Integer>();
	
	/**
	 * Reads a JDBC-result-set.
	 */
	public interface ResultSetReader {

		/**
		 * Reads current row of a result-set.
		 *
		 * @param resultSet the result-set
		 */
		void readCurrentRow(ResultSet resultSet) throws SQLException;

		/**
		 * Finalizes reading.
		 */
		void close() throws SQLException;
	}

	/**
	 * Reads a JDBC-result-set.
	 * Caches a {@link ResultSetMetaData}.
	 */
	public static abstract class AbstractResultSetReader implements ResultSetReader {

		private ResultSet owner;
		private ResultSetMetaData metaData;

		private ResultSet cccOwner;
		private Session cccSession;
		private CellContentConverter cellContentConverter;

		/**
		 * Gets and cache meta data of a result set.
		 *
		 * @param resultSet
		 * @return meta data of resultSet
		 * @throws SQLException
		 */
		protected ResultSetMetaData getMetaData(ResultSet resultSet) throws SQLException {
			if (owner == resultSet) {
				return metaData;
			}
			owner = resultSet;
			metaData = resultSet.getMetaData();
			return metaData;
		}

		/**
		 * Gets and cache CellContentConverter for the result set.
		 *
		 * @param resultSet
		 * @return meta data of resultSet
		 * @throws SQLException
		 */
		protected CellContentConverter getCellContentConverter(ResultSet resultSet, Session session, DBMS targetDBMSConfiguration) throws SQLException {
			if (cccOwner == resultSet && cccSession == session) {
				return cellContentConverter;
			}
			cccOwner = resultSet;
			cccSession = session;
			cellContentConverter = new CellContentConverter(getMetaData(resultSet), session, targetDBMSConfiguration);
			return cellContentConverter;
		}

		/**
		 * Does nothing.
		 * @throws SQLException
		 */
		@Override
		public void close() throws SQLException {
		}

		/**
		 * Initializes the reader.
		 *
		 * @param resultSet the result set to read.
		 * @throws SQLException
		 */
		public void init(ResultSet resultSet) throws SQLException {
		}

	}

	/**
	 * The logger.
	 */
	public static final Logger _log = LoggerFactory.getLogger("sql");

	/**
	 * Connection factory.
	 */
	public interface ConnectionFactory {
		Connection getConnection() throws SQLException;
		Connection createNewConnection() throws SQLException;
	}

	/**
	 * Connection factory.
	 */
	protected final ConnectionFactory connectionFactory;

	/**
	 * The DB schema name (empty string if unknown).
	 */
	private final String schema;

	/**
	 * Optional schema for database analysis.
	 */
	private String introspectionSchema;

	/**
	 * The DBMS.
	 */
	public final DBMS dbms;

	/**
	 * The DBMS.
	 */
	public final String driverClassName;

	/**
	 * The dbUrl (<code>null</code> if unknown)
	 */
	public final String dbUrl;

	/**
	 * Constructor.
	 *
	 * @param dataSource the data source
	 * @param dbms the DBMS
	 */
	public Session(DataSource dataSource, DBMS dbms, Integer isolationLevel) throws SQLException {
		this(dataSource, dbms, isolationLevel, null, false);
	}

	/**
	 * Constructor.
	 *
	 * @param dataSource the data source
	 * @param dbms the DBMS
	 */
	public Session(DataSource dataSource, DBMS dbms, Integer isolationLevel, final WorkingTableScope scope, boolean transactional) throws SQLException {
		this(dataSource, dbms, isolationLevel, scope, transactional, false);
	}

	/**
	 * Constructor.
	 *
	 * @param dataSource the data source
	 * @param dbms the DBMS
	 * @param local <code>true</code> for the local entity-graph database
	 */
	public Session(final DataSource dataSource, DBMS dbms, final Integer isolationLevel, final WorkingTableScope scope, boolean transactional, final boolean local) throws SQLException {
		this.transactional = transactional;
		this.scope = scope;
		this.dbms = dbms;
		this.driverClassName = (dataSource instanceof BasicDataSource)? ((BasicDataSource) dataSource).driverClassName : null;
		this.dbUrl = (dataSource instanceof BasicDataSource)? ((BasicDataSource) dataSource).dbUrl : null;
		this.schema = (dataSource instanceof BasicDataSource)? ((BasicDataSource) dataSource).dbUser : "";
		this.temporaryTableScope = scope;

		connectionFactory = new ConnectionFactory() {
			private Random random = new Random();
			@Override
			public Connection getConnection() throws SQLException {
				long databaseConnectionInteractiveTimeout = Configuration.getInstance().getDatabaseConnectionInteractiveTimeout() * 1000L;
				Connection con = getConnectionIfExist();
				if (con != null) {
					Long ts = lastConnectionActiviyTimeStamp.get(con);
					if (ts != null && System.currentTimeMillis() - ts < databaseConnectionInteractiveTimeout) {
						// prevent synchronization
						releaseConnection(con); // that's ok
						return con;
					}
				}
				synchronized (this) {
					con = getConnection0();
					Long ts = lastConnectionActiviyTimeStamp.get(con);
					releaseConnection(con);
					boolean isInvalid = false;
					boolean currentAutoCommit = true;
					try {
						if (con != null) {
							currentAutoCommit = con.getAutoCommit();
						}
					} catch (Throwable t) {
						isInvalid = true;
					}
					if (ts != null && con != null && con == connection.get() && currentAutoCommit && !Session.this.transactional && !isDown()) {
						long idleTime = System.currentTimeMillis() - ts;
						
						if (isInvalid || idleTime >= databaseConnectionInteractiveTimeout) {
							boolean valid;
							try {
								valid = con.isValid(4);
							} catch (Throwable t) {
								valid = true;
							}
							if (!valid) {
								// LogUtil.warn(new RuntimeException("invalid connection, reconnecting (" + idleTime + ")"));
								reconnect();
								return getConnection0();
							}
						}
					}
					return con;
				}
			}
			private Connection getConnectionIfExist() throws SQLException {
				Connection con = local? connection.get() : temporaryTableSession == null? connection.get() : temporaryTableSession;

				if (con == null && Boolean.TRUE.equals(sharesConnection.get())) {
					con = defaultConnection;
				}
				return con;
			}
			private Connection getConnection0() throws SQLException {
				Connection con = getConnectionIfExist();
				if (isDown()) {
					return defaultConnection;
				}
				if (con == null) {
					try {
						con = dataSource.getConnection();
						defaultConnection = con;
					} catch (SQLException e) {
						if (connections.size() > 1) {
							con = connections.get(random.nextInt(connections.size()));
						} else if (defaultConnection != null) {
							// fall back to default connection
							con = defaultConnection;
						} else if (globalFallbackConnection != null) {
							// fall back to global default connection
							con = globalFallbackConnection;
						} else {
							throw e;
						}
					}
					boolean ac = scope == null || scope != WorkingTableScope.TRANSACTION_LOCAL;
					if (Session.this.transactional) {
						ac = false;
					}
					_log.info(logPrefix + "set auto commit to " + ac);
					con.setAutoCommit(ac);
					try {
						if (isolationLevel != null) {
							_log.info(logPrefix + "set isolation level to " + isolationLevel);
							con.setTransactionIsolation(isolationLevel);
							_log.info(logPrefix + "isolation level is " + con.getTransactionIsolation());
						}
					} catch (SQLException e) {
						_log.warn(logPrefix + "can't set isolation level to UR. Reason: " + e.getMessage());
					}
					if ((Session.this.transactional && !local) || scope == WorkingTableScope.SESSION_LOCAL || scope == WorkingTableScope.TRANSACTION_LOCAL) {
						temporaryTableSession = con;
					} else {
						setConnection(con);
						boolean addCon = true;
						if (con != globalFallbackConnection) {
							synchronized (connections) {
								for (Connection c: connections) {
									if (c == con) {
										addCon = false;
										break;
									}
								}
								if (addCon) {
									connections.add(con);
								}
							}
						}
					}
				}
				return con;
			}
			@Override
			public Connection createNewConnection() throws SQLException {
				return dataSource.getConnection();
			}
		};
		// fail fast
		init();
	}

	protected synchronized void setConnection(Connection con) {
		connection.set(con);
		Long tid = Thread.currentThread().getId();
		if (con == null) {
			Integer count = connectionCount.get(tid);
			if (count != null) {
				connectionCount.put(tid, count - 1);
			}
		} else {
			Integer count = connectionCount.get(tid);
			if (count != null) {
				connectionCount.put(tid, count + 1);
			} else {
				connectionCount.put(tid, 1);
			}
		}
	}

	/**
	 * Releases a connection get from {@link #getConnection()}.
	 * Indicated that the connection is no longer in use for the time being.
	 * 
	 * @param con the connection
	 */
	private void releaseConnection(Connection con) {
		lastConnectionActiviyTimeStamp.put(con, System.currentTimeMillis());
	}

	/**
	 * Marks a connection as potentially invalid.
	 * Forces to re-validate the connection in {@link #getConnection()}.
	 * 
	 * @param con the connection
	 */
	public void markConnectionAsPotentiallyInvalid(Connection con) {
		lastConnectionActiviyTimeStamp.put(con, System.currentTimeMillis() - 1000L * 60 * 24 * 31);
	}

	protected void init() throws SQLException {
		Connection connection = connectionFactory.getConnection();
		logDriverInfo(connection);
	}

	/**
	 * Closes current connection and opens a new one.
	 */
	public void reconnect() {
		Connection con = connection.get();
		if (con != null) {
			if (temporaryTableScope == WorkingTableScope.TRANSACTION_LOCAL) {
				try {
					con.commit();
				} catch (SQLException e) {
					// ignore
				}
			}
			try {
				con.close();
			// catch all because ucanaccess throws non-SQLException
			} catch (Throwable e) { // SQLException e) {
				// ignore
			}
			setConnection(null);
			if (con == temporaryTableSession) {
				temporaryTableSession = null;
				return;
			}
		}
		if (temporaryTableSession != null) {
			if (temporaryTableScope == WorkingTableScope.TRANSACTION_LOCAL) {
				try {
					temporaryTableSession.commit();
				} catch (SQLException e) {
					// ignore
				}
			}
			try {
				temporaryTableSession.close();
				// catch all because ucanaccess throws non-SQLException
			} catch (Throwable e) { // SQLException e) {
				// ignore
			}
			temporaryTableSession = null;
		}
	}

	private Object silentLock = new Object();

	/**
	 * No SQL-Exceptions will be logged in silent mode.
	 *
	 * @param silent <code>true</code> for silence
	 */
	public void setSilent(boolean silent) {
		synchronized (silentLock) {
			this.silent = silent;
		}
	}

	/**
	 * No SQL-Exceptions will be logged in silent mode.
	 *
	 * @return silent <code>true</code> for silence
	 */
	public boolean getSilent() {
		synchronized (silentLock) {
			return silent;
		}
	}

	private static final ThreadLocal<Boolean> logStatements = new ThreadLocal<Boolean>();

	/**
	 * Log statements?
	 */
	public void setLogStatements(boolean logStatements) {
		Session.logStatements.set(logStatements);
	}

	/**
	 * Log statements?
	 */
	public boolean getLogStatements() {
		return !Boolean.FALSE.equals(logStatements.get());
	}

	/**
	 * Logs driver info
	 *
	 * @param connection connection to DB
	 * @return the DBMS
	 */
	private void logDriverInfo(Connection connection) {
		try {
			DatabaseMetaData meta = connection.getMetaData();
			_log.info(logPrefix + "driver name:    " + meta.getDriverName());
			_log.info(logPrefix + "driver version: " + meta.getDriverVersion());
			_log.info(logPrefix + "DB name:        " + meta.getDatabaseProductName() + (dbms.getDisplayName() != null? " (" + dbms.getDisplayName() + ")" : ""));
			_log.info(logPrefix + "DB version:     " + meta.getDatabaseProductVersion());
		} catch (Exception e) {
			// ignore exceptions
		}
	}

	/**
	 * Gets DB schema name.
	 *
	 * @return DB schema name (empty string if unknown)
	 */
	public String getSchema() {
		return schema;
	}

	/**
	 * Executes a SQL-Query (SELECT).
	 *
	 * @param sqlQuery the query in SQL
	 * @param reader the reader for the result
	 * @param withExplicitCommit if <code>true</code>, switch of autocommit and commit explicitly
	 */
	public long executeQuery(String sqlQuery, ResultSetReader reader, boolean withExplicitCommit) throws SQLException {
		return executeQuery(sqlQuery, reader, null, null, 0, withExplicitCommit);
	}

	/**
	 * Executes a SQL-Query (SELECT).
	 *
	 * @param sqlQuery the query in SQL
	 * @param reader the reader for the result
	 */
	public long executeQuery(String sqlQuery, ResultSetReader reader) throws SQLException {
		return executeQuery(sqlQuery, reader, null, null, 0, false);
	}

	/**
	 * Executes a SQL-Query (SELECT).
	 *
	 * @param sqlQuery the query in SQL
	 * @param reader the reader for the result
	 * @param alternativeSQL query to be executed if sqlQuery fails
	 * @param context cancellation context
	 * @param limit row limit, 0 for unlimited
	 * @param withExplicitCommit if <code>true</code>, switch of autocommit and commit explicitly
	 */
	public long executeQuery(String sqlQuery, ResultSetReader reader, String alternativeSQL, Object context, long limit, boolean withExplicitCommit) throws SQLException {
		return executeQuery(sqlQuery, reader, alternativeSQL, context, limit, 0, withExplicitCommit);
	}

	/**
	 * Executes a SQL-Query (SELECT).
	 *
	 * @param sqlQuery the query in SQL
	 * @param reader the reader for the result
	 * @param alternativeSQL query to be executed if sqlQuery fails
	 * @param context cancellation context
	 * @param limit row limit, 0 for unlimited
	 */
	public long executeQuery(String sqlQuery, ResultSetReader reader, String alternativeSQL, Object context, long limit) throws SQLException {
		return executeQuery(sqlQuery, reader, alternativeSQL, context, limit, 0, false);
	}

	/**
	 * Executes a SQL-Query (SELECT) with timeout.
	 *
	 * @param theConnection connection to use
	 * @param sqlQuery the query in SQL
	 * @param reader the reader for the result
	 * @param alternativeSQL query to be executed if sqlQuery fails
	 * @param context cancellation context
	 * @param limit row limit, 0 for unlimited
	 * @param timeout the timeout in sec
	 * @param withExplicitCommit if <code>true</code>, switch of autocommit and commit explicitly
	 */
	private long executeQuery(Connection theConnection, String sqlQuery, ResultSetReader reader, String alternativeSQL, Object context, long limit, int timeout, boolean withExplicitCommit) throws SQLException {
		if (!transactional || DBMS.MySQL.equals(dbms)) {
			synchronized (theConnection) {
				return executeQuery0(theConnection, sqlQuery, reader, alternativeSQL, context, limit, timeout, withExplicitCommit);
			}
		} else {
			return executeQuery0(theConnection, sqlQuery, reader, alternativeSQL, context, limit, timeout, withExplicitCommit);
		}
	}

	/**
	 * Executes a SQL-Query (SELECT) with timeout.
	 *
	 * @param theConnection connection to use
	 * @param sqlQuery the query in SQL
	 * @param reader the reader for the result
	 * @param alternativeSQL query to be executed if sqlQuery fails
	 * @param context cancellation context
	 * @param limit row limit, 0 for unlimited
	 * @param timeout the timeout in sec
	 * @param withExplicitCommit if <code>true</code>, switch of autocommit and commit explicitly
	 */
	private long executeQuery0(Connection theConnection, String sqlQuery, ResultSetReader reader, String alternativeSQL, Object context, long limit, int timeout, boolean withExplicitCommit) throws SQLException {
		if (withExplicitCommit) {
			synchronized (theConnection) {
				if (theConnection.getAutoCommit()) {
					try {
						theConnection.setAutoCommit(false);
						return executeQuery(theConnection, sqlQuery, reader, alternativeSQL, context, limit, timeout, false);
					} finally {
						try {
							theConnection.commit();
						} catch (SQLException e) {
							_log.warn(logPrefix + "commit failed", e);
						}
						theConnection.setAutoCommit(true);
					}
				}
			}
		}
		long rc = 0;
		CancellationHandler.checkForCancellation(context);
		long startTime = System.currentTimeMillis();
		Statement statement = null;
		try {
			final String woSuffix = " /*!*/";
			boolean wo = sqlQuery.endsWith(woSuffix);
			if (wo) {
				sqlQuery = sqlQuery.substring(0, sqlQuery.length() - woSuffix.length());
			}
			statement = theConnection.createStatement();
			if (dbms != null) {
				if (!wo || !DBMS.MySQL.equals(dbms)) {
					try {
						statement.setFetchSize(dbms.getLimitedFetchSize(limit));
					} catch (Throwable t) {
						LogUtil.warn(t);
					}
				}
			}
			begin(statement, context);
			ResultSet resultSet;
			try {
				if (limit > 0 && limit < Integer.MAX_VALUE - 1) {
					statement.setMaxRows((int) limit + 1);
				}
			} catch (Exception e) {
				// ignore
			}
			try {
				if (timeout > 0) {
					statement.setQueryTimeout(timeout);
				}
				resultSet = statement.executeQuery(sqlQuery);
			} catch (SQLException e) {
				checkKilled();
				CancellationHandler.checkForCancellation(context);

				if (alternativeSQL != null) {
					_log.warn(logPrefix + "query failed, using alternative query. Reason: " + e.getMessage());
					_log.info(logPrefix + alternativeSQL);
					resultSet = statement.executeQuery(alternativeSQL);
				} else {
					throw e;
				}
			}
			if (reader instanceof AbstractResultSetReader) {
				((AbstractResultSetReader) reader).init(resultSet);
			}
			while (resultSet.next()) {
				reader.readCurrentRow(resultSet);
				++rc;
				if (rc % 100 == 0) {
					checkKilled();
					CancellationHandler.checkForCancellation(context);
				}
				if (limit > 0 && rc >= limit) {
					break;
				}
			}
			reader.close();
			resultSet.close();
		} finally {
			if (statement != null) {
				try {
					statement.close();
				} catch (SQLException e) {
					// ignore
				}
				end(statement, context);
			}
		}
		if (getLogStatements()) {
			_log.info(logPrefix + rc + " row(s) in " + (System.currentTimeMillis() - startTime) + " ms");
		}
		return rc;
	}

	/**
	 * Executes a SQL-Query (SELECT) with timeout.
	 *
	 * @param sqlQuery the query in SQL
	 * @param reader the reader for the result
	 * @param alternativeSQL query to be executed if sqlQuery fails
	 * @param limit row limit, 0 for unlimited
	 * @param context cancellation context
	 * @param timeout the timeout in sec
	 * @param withExplicitCommit if <code>true</code>, switch of autocommit and commit explicitly
	 */
	public long executeQuery(String sqlQuery, ResultSetReader reader, String alternativeSQL, Object context, long limit, int timeout, boolean withExplicitCommit) throws SQLException {
		if (getLogStatements()) {
			_log.info(logPrefix + sqlQuery);
		}
		Connection con = null;
		try {
			con = connectionFactory.getConnection();
			long result = executeQuery(con, sqlQuery, reader, alternativeSQL, context, limit, timeout, withExplicitCommit);
			releaseConnection(con);
			return result;
		} catch (SQLException e) {
			if (con != null) {
				markConnectionAsPotentiallyInvalid(con);
			}
			CancellationHandler.checkForCancellation(context);
			if (!silent) {
				_log.error(logPrefix + "Error executing query", e);
			}
			if (e instanceof SqlException) {
				throw e;
			}
			final String woSuffix = " /*!*/";
			if (sqlQuery != null && sqlQuery.endsWith(woSuffix)) {
				sqlQuery = sqlQuery.substring(0, sqlQuery.length() - woSuffix.length());
			}
			throw new SqlException("\"" + e.getMessage() + "\" in statement \"" + sqlQuery + "\"", sqlQuery, e);
		}
	}

	/**
	 * Executes a SQL-Query (SELECT).
	 *
	 * @param sqlFile file containing a query in SQL
	 * @param reader the reader for the result
	 * @param withExplicitCommit if <code>true</code>, switch of autocommit and commit explicitly
	 */
	public void executeQuery(File sqlFile, ResultSetReader reader, boolean withExplicitCommit) throws SQLException {
		StringBuffer result = new StringBuffer();
		try {
			BufferedReader in = new BufferedReader(new FileReader(sqlFile));

			String line;
			while ((line = in.readLine()) != null) {
				result.append(line);
				result.append(System.getProperty("line.separator", "\n"));
			}
			in.close();
		} catch (IOException e) {
			throw new RuntimeException("Failed to load content of file", e);
		}

		executeQuery(result.toString(), reader, withExplicitCommit);
	}

	/**
	 * Prevention of livelocks.
	 */
	private static final int PERMITS = Integer.MAX_VALUE / 4;
	private Semaphore semaphore = new Semaphore(PERMITS);

	private static final int MAXIMUM_NUMBER_OF_FAILURES = 100;

	/**
	 * Executes a SQL-Update (INSERT, DELETE or UPDATE).
	 *
	 * @param sqlUpdate the update in SQL
	 *
	 * @return update-count
	 */
	public int executeUpdate(String sqlUpdate) throws SQLException {
		if (getLogStatements()) {
			_log.info(logPrefix + sqlUpdate);
		}
		CancellationHandler.checkForCancellation(null);
		try {
			int rowCount = 0;
			int failures = 0;
			boolean ok = false;
			boolean serializeAccess = false;

			while (!ok) {
				long startTime = System.currentTimeMillis();
				Statement statement = null;
				Connection con = null;
				try {
					con = connectionFactory.getConnection();
					statement = con.createStatement();
					begin(statement, null);
					if (serializeAccess) {
						boolean acquired;
						try {
							semaphore.acquire(PERMITS);
							acquired = true;
						} catch (InterruptedException e) {
							acquired = false;
						}

						try {
							rowCount = statement.executeUpdate(sqlUpdate);
						} finally {
							if (acquired) {
								semaphore.release(PERMITS);
							}
						}
					} else {
						boolean acquired;
						try {
							semaphore.acquire(1);
							acquired = true;
						} catch (InterruptedException e) {
							acquired = false;
						}

						try {
							rowCount = statement.executeUpdate(sqlUpdate);
						} finally {
							if (acquired) {
								semaphore.release(1);
							}
						}
					}

					end(statement, null);
					releaseConnection(con);
					ok = true;
					if (getLogStatements()) {
						_log.info(logPrefix + "" + rowCount + " row(s) in " + (System.currentTimeMillis() - startTime) + " ms");
					}
				} catch (SQLException e) {
					if (con != null) {
						markConnectionAsPotentiallyInvalid(con);
					}
					checkKilled();
					CancellationHandler.checkForCancellation(null);
					end(statement, null);

					boolean isRetrieable = isRetrieable(e);
					if (++failures > MAXIMUM_NUMBER_OF_FAILURES || !isRetrieable) {
						throw new SqlException("\"" + e.getMessage() + "\" in statement \"" + sqlUpdate + "\"", sqlUpdate, e);
					}
					// deadlock
					serializeAccess = true;
					_log.info(logPrefix + "Deadlock! Try again...");
					try {
						Thread.sleep(140);
					} catch (InterruptedException e1) {
						// ignore
					}
				} finally {
					if (statement != null) {
						try { statement.close(); } catch (SQLException e) { }
					}
				}
			}
			return rowCount;
		} catch (SQLException e) {
			CancellationHandler.checkForCancellation(null);
			if (!silent) {
				_log.error(logPrefix + "Error executing statement", e);
			} else {
				String message = e.getMessage();
				if (e instanceof SqlException) {
					message = e.getCause().getMessage();
				}
				_log.info(logPrefix + "\"" + message + "\"");
			}
			throw e;
		}
	}

	/**
	 * Executes a SQL-Update (INSERT, DELETE or UPDATE) with parameters.
	 *
	 * @param sqlUpdate the update in SQL
	 * @param parameter the parameters
	 *
	 * @return update-count
	 */
	public int executeUpdate(String sqlUpdate, Object[] parameter) throws SQLException {
		if (getLogStatements()) {
			_log.info(logPrefix + sqlUpdate);
		}
		PreparedStatement statement = null;
		Connection con = null;
		try {
			CancellationHandler.checkForCancellation(null);
			int rowCount = 0;
			long startTime = System.currentTimeMillis();
			try {
				con = connectionFactory.getConnection();
				statement = con.prepareStatement(sqlUpdate);
				begin(statement, null);
				int i = 1;
				for (Object p: parameter) {
					statement.setObject(i++, p);
				}
				rowCount = statement.executeUpdate();
				end(statement, null);
				releaseConnection(con);
				if (getLogStatements()) {
					_log.info(logPrefix + "" + rowCount + " row(s) in " + (System.currentTimeMillis() - startTime) + " ms");
				}
			} finally {
				if (statement != null) {
					try { statement.close(); } catch (SQLException e) { }
				}
			}
			return rowCount;
		} catch (SQLException e) {
			if (con != null) {
				markConnectionAsPotentiallyInvalid(con);
			}
			checkKilled();
			CancellationHandler.checkForCancellation(null);
			if (!silent) {
				_log.error(logPrefix + "Error executing statement", e);
			}
			throw new SqlException("\"" + e.getMessage() + "\" in statement \"" + sqlUpdate + "\"", sqlUpdate, e);
		}
	}

	/**
	 * Inserts a CLob.
	 */
	public void insertClob(String table, String column, String where, File lobFile, long length) throws SQLException, IOException {
		String sqlUpdate = "Update " + table + " set " + column + "=? where " + where;
		if (getLogStatements()) {
			_log.info(logPrefix + sqlUpdate);
		}
		PreparedStatement statement = null;
		Connection con = null;
		try {
			con = connectionFactory.getConnection();
			statement = con.prepareStatement(sqlUpdate);
			begin(statement, null);
			InputStreamReader inputStreamReader = new InputStreamReader(new FileInputStream(lobFile), "UTF-8"); // lgtm [java/input-resource-leak]
			statement.setCharacterStream(1, inputStreamReader, (int) length);
			statement.execute();
			inputStreamReader.close();
		} catch (SQLException e) {
			if (con != null) {
				markConnectionAsPotentiallyInvalid(con);
			}
			checkKilled();
			CancellationHandler.checkForCancellation(null);
			throw e;
		} finally {
			if (statement != null) {
				try {
					statement.close();
					end(statement, null);
				} catch (SQLException e) {
				}
			}
		}
	}

	/**
	 * Inserts a SQL-XML.
	 */
	public void insertSQLXML(String table, String column, String where, File lobFile, long length) throws SQLException, IOException {
		String sqlUpdate = "Update " + table + " set " + column + "=? where " + where;
		_log.info(logPrefix + sqlUpdate);
		PreparedStatement statement = null;
		Connection con = null;
		try {
			con = connectionFactory.getConnection();
			statement = con.prepareStatement(sqlUpdate);
			begin(statement, null);
			InputStreamReader inputStreamReader = new InputStreamReader(new FileInputStream(lobFile), "UTF-8"); // lgtm [java/input-resource-leak]
			statement.setCharacterStream(1, inputStreamReader, (int) length);
			statement.execute();
			inputStreamReader.close();
		} catch (SQLException e) {
			if (con != null) {
				markConnectionAsPotentiallyInvalid(con);
			}
			checkKilled();
			CancellationHandler.checkForCancellation(null);
			throw e;
		} finally {
			if (statement != null) {
				try {
					statement.close();
					end(statement, null);
				} catch (SQLException e) {
				}
			}
		}
	}

	/**
	 * Inserts a BLob.
	 */
	public void insertBlob(String table, String column, String where, File lobFile) throws SQLException, IOException {
		String sqlUpdate = "Update " + table + " set " + column + "=? where " + where;
		_log.info(logPrefix + sqlUpdate);
		PreparedStatement statement = null;
		Connection con = null;
		try {
			con = connectionFactory.getConnection();
			statement = con.prepareStatement(sqlUpdate);
			begin(statement, null);
			FileInputStream fileInputStream = new FileInputStream(lobFile);
			statement.setBinaryStream(1, fileInputStream, (int) lobFile.length());
			statement.execute();
			fileInputStream.close();
		} catch (SQLException e) {
			if (con != null) {
				markConnectionAsPotentiallyInvalid(con);
			}
			checkKilled();
			CancellationHandler.checkForCancellation(null);
			throw e;
		} finally {
			if (statement != null) {
				try {
					statement.close();
					end(statement, null);
				} catch (SQLException e) {
				}
			}
		}
	}

	/**
	 * Executes a SQL-Statement without returning any result.
	 *
	 * @param sql the SQL-Statement
	 */
	public long execute(String sql) throws SQLException {
		return execute(sql, null, false);
	}

	/**
	 * Executes a SQL-Statement without returning any result.
	 *
	 * @param sql the SQL-Statement
	 */
	public long execute(String sql, Object cancellationContext, boolean acceptQueries) throws SQLException {
		if (getLogStatements()) {
			_log.info(logPrefix + sql);
		}
		CancellationHandler.checkForCancellation(null);
		try {
			int rowCount = 0;
			int failures = 0;
			boolean ok = false;
			boolean serializeAccess = false;

			while (!ok) {
				long startTime = System.currentTimeMillis();
				Statement statement = null;
				Connection con = null;
				try {
					con = connectionFactory.getConnection();
					statement = con.createStatement();
					begin(statement, null);
					if (serializeAccess) {
						boolean acquired;
						try {
							semaphore.acquire(PERMITS);
							acquired = true;
						} catch (InterruptedException e) {
							acquired = false;
						}

						try {
							if (acceptQueries) {
								if (statement.execute(sql)) {
									statement.getResultSet().close();
								} else {
									rowCount = statement.getUpdateCount();
									lastUpdateTS = System.currentTimeMillis();
								}
							} else {
								rowCount = statement.executeUpdate(sql);
								lastUpdateTS = System.currentTimeMillis();
							}
						} finally {
							if (acquired) {
								semaphore.release(PERMITS);
							}
						}
					} else {
						boolean acquired;
						try {
							semaphore.acquire(1);
							acquired = true;
						} catch (InterruptedException e) {
							acquired = false;
						}

						try {
							if (acceptQueries) {
								if (statement.execute(sql)) {
									statement.getResultSet().close();
								} else {
									rowCount = statement.getUpdateCount();
									lastUpdateTS = System.currentTimeMillis();
								}
							} else {
								rowCount = statement.executeUpdate(sql);
								lastUpdateTS = System.currentTimeMillis();
							}
						} finally {
							if (acquired) {
								semaphore.release(1);
							}
						}
					}

					end(statement, null);
					releaseConnection(con);
					ok = true;
					if (getLogStatements()) {
						_log.info(logPrefix + "" + rowCount + " row(s) in " + (System.currentTimeMillis() - startTime) + " ms");
					}
				} catch (SQLException e) {
					if (con != null) {
						markConnectionAsPotentiallyInvalid(con);
					}
					checkKilled();
					CancellationHandler.checkForCancellation(null);
					end(statement, null);

					boolean isRetrieable = isRetrieable(e);
					if (++failures > MAXIMUM_NUMBER_OF_FAILURES || !isRetrieable) {
						throw new SqlException("\"" + e.getMessage() + "\" in statement \"" + sql + "\"", sql, e);
					}
					// deadlock
					serializeAccess = true;
					_log.info(logPrefix + "Deadlock! Trying again...");
					try {
						Thread.sleep(140);
					} catch (InterruptedException e1) {
						// ignore
					}
				} finally {
					if (statement != null) {
						try { statement.close(); } catch (SQLException e) { }
					}
				}
			}
			return rowCount;
		} catch (SQLException e) {
			CancellationHandler.checkForCancellation(null);
			if (!silent) {
				_log.error(logPrefix + "Error executing statement", e);
			} else {
				String message = e.getMessage();
				if (e instanceof SqlException) {
					message = e.getCause().getMessage();
				}
				_log.info(logPrefix + "\"" + message + "\"");
			}
			throw e;
		}
	}

	private boolean isRetrieable(SQLException e) {
		String sqlState = e.getSQLState();
		boolean deadlock = sqlState != null && sqlState.matches("40.01"); // "serialization failure", see https://en.wikipedia.org/wiki/SQLSTATE
		boolean crf = DBMS.ORACLE.equals(dbms) && e.getErrorCode() == 8176; // ORA-08176: consistent read failure; rollback data not available

		boolean isRetrieable = deadlock || crf;
		return isRetrieable;
	}

	/**
	 * Cached Database Meta Data.
	 */
	private Map<Connection, DatabaseMetaData> metaData = Collections.synchronizedMap(new IdentityHashMap<Connection, DatabaseMetaData>());

	private boolean checkMetaData = true;

	public void disableMetaDataChecking() {
		checkMetaData = false;
	}

	/**
	 * Gets DB meta data.
	 *
	 * @return DB meta data
	 */
	public DatabaseMetaData getMetaData() throws SQLException {
		Connection con = connectionFactory.getConnection();
		DatabaseMetaData mData = metaData.get(con);
		if (mData != null && checkMetaData) {
			try {
				// is meta data still valid?
				mData.getIdentifierQuoteString();
			} catch (SQLException e) {
				mData = null;
			}
		}
		if (mData == null) {
			mData = con.getMetaData();
			metaData.put(con, mData);
		}
		return mData;
	}

	protected AtomicBoolean down = new AtomicBoolean(false);

	/**
	 * Closes all connections.
	 */
	public void shutDown() {
		down.set(true);
		_log.info(logPrefix + "closing connections... (" + connections.size() + ")");
		for (;;) {
			Connection con = null;
			synchronized (connections) {
				if (!connections.isEmpty()) {
					con = connections.remove(0);
				}
			}
			if (con == null) {
				break;
			}
			try {
				con.close();
			} catch (Exception e) {
				// ignore
			}
		}
		closeTemporaryTableSession();
		_log.info(logPrefix + "connection closed");
	}

	public boolean isDown() {
		return down.get();
	}

	/**
	 * Cancels all currently running statements.
	 */
	public synchronized void killRunningStatements() {
		final IdentityHashMap<Statement, Statement> toBeCanceled = new IdentityHashMap<Statement, Statement>(runningStatements);
		runningStatements.clear();
		++currentVersion;
		for (final IdentityHashMap.Entry<Statement, Statement> statement: toBeCanceled.entrySet()) {
			Thread thread = new Thread(new Runnable() {
				@Override
				public void run() {
					try {
						statement.getKey().cancel();
					} catch (Exception e) {
						// ignore
					}
				}
			});
			thread.setDaemon(true);
			thread.start();
		}
	}

	private long currentVersion = 0;
	private static ThreadLocal<Long> runningVersion = new ThreadLocal<Long>();
	private Map<Statement, Statement> runningStatements = new IdentityHashMap<Statement, Statement>();

	private synchronized void begin(Statement statement, Object context) {
		CancellationHandler.begin(statement, context);
		runningStatements.put(statement, statement);
		runningVersion.set(currentVersion);
	}

	private synchronized void end(Statement statement, Object context) {
		CancellationHandler.end(statement, context);
		runningStatements.remove(statement);
		runningVersion.set(null);
	}

	private synchronized void checkKilled() {
		Long v = runningVersion.get();
		if (v != null && v != currentVersion) {
			throw new CancellationException();
		}
	}

	/**
	 * Rolls back and closes all connections.
	 */
	public void rollbackAll() throws SQLException {
		for (Connection con: connections) {
			try {
				con.rollback();
			} catch(SQLException e) {
				_log.warn(logPrefix + e.getMessage());
			}
			try {
				con.close();
			} catch(SQLException e) {
				_log.warn(logPrefix + e.getMessage());
			}
		}
		if (temporaryTableSession != null) {
			try {
				temporaryTableSession.rollback();
			} catch(SQLException e) {
				_log.warn(logPrefix + e.getMessage());
			}
		 }
		connection = new ThreadLocal<Connection>();
		synchronized (this) {
			connectionCount = new HashMap<Long, Integer>();
		}
	}

	/**
	 * Commits all connections.
	 */
	public void commitAll() throws SQLException {
		for (Connection con: connections) {
			try {
				con.commit();
			} catch(SQLException e) {
				_log.warn(logPrefix + e.getMessage());
			}
		}
		if (temporaryTableSession != null) {
			try {
				temporaryTableSession.commit();
			} catch(SQLException e) {
				_log.warn(logPrefix + e.getMessage());
			}
		 }
	}

	/**
	 * Gets optional schema for database analysis.
	 *
	 * @return optional schema for database analysis
	 */
	public String getIntrospectionSchema() {
		return introspectionSchema;
	}

	/**
	 * Sets optional schema for database analysis.
	 *
	 * @param introspectionSchema optional schema for database analysis
	 */
	public void setIntrospectionSchema(String introspectionSchema) {
		this.introspectionSchema = introspectionSchema;
	}

	/**
	 * Closes the session in which temporary tables lives, if any.
	 */
	private void closeTemporaryTableSession() {
		try {
			if (temporaryTableSession != null) {
				if (temporaryTableScope == WorkingTableScope.TRANSACTION_LOCAL) {
					temporaryTableSession.commit();
				}
				temporaryTableSession.close();
			}
		} catch(SQLException e) {
			_log.warn(logPrefix + "can't close connection", e);
		}
		temporaryTableSession = null;
	}

	/**
	 * CLI connection arguments (UI support)
	 */
	private List<String> cliArguments;

	/**
	 * Connection password (UI support)
	 */
	private String password;

	private final Object CLI_LOCK = new String("CLI_LOCK");

	/**
	 * Gets connection password (UI support)
	 */
	public String getPassword() {
		synchronized (CLI_LOCK) {
			return password;
		}
	}

	/**
	 * Sets connection password (UI support)
	 */
	public void setPassword(String password) {
		synchronized (CLI_LOCK) {
			this.password = password;
		}
	}

	/**
	 * Sets CLI connection arguments (UI support)
	 */
	public void setCliArguments(List<String> args) {
		synchronized (CLI_LOCK) {
			this.cliArguments = args;
		}
	}

	/**
	 * Gets CLI connection arguments (UI support)
	 */
	public List<String> getCliArguments() {
		synchronized (CLI_LOCK) {
			return cliArguments;
		}
	}

	/**
	 * Gets the connection for the current thread.
	 *
	 * @return the connection for the current thread
	 */
	public Connection getConnection() throws SQLException {
		return connectionFactory.getConnection();
	}

	/**
	 * Create a new connection. Must be closed from caller.
	 *
	 * @return a new connection
	 */
	public Connection createNewConnection() throws SQLException {
		return connectionFactory.createNewConnection();
	}

	private InlineViewStyle inlineViewStyle;
	private boolean noInlineViewStyleFound = false;
	private final Object IVS_LOCK = new Object();

	/**
	 * Returns a suitable {@link InlineViewStyle} for this session.
	 *
	 * @return a suitable {@link InlineViewStyle} for this session or <code>null</code>, if no style is found
	 */
	public InlineViewStyle getInlineViewStyle() {
		synchronized (IVS_LOCK) {
			if (inlineViewStyle == null && !noInlineViewStyleFound) {
				try {
					inlineViewStyle = InlineViewStyle.forSession(this);
				} catch (Exception e) {
					LogUtil.warn(e);
					// no suitable style found
					noInlineViewStyleFound = true;
				}
			}
		}
		return inlineViewStyle;
	}

	private Map<String, Object> sessionProperty = Collections.synchronizedMap(new HashMap<String, Object>());

	/**
	 * Sets a session property.
	 *
	 * @param owner the class that owns the property
	 * @param name name of the property
	 * @param property value of the property
	 */
	public void setSessionProperty(Class<?> owner, String name, Object property) {
		sessionProperty.put(owner.getName() + "." + name, property);
	}

	/**
	 * Gets a session property.
	 *
	 * @param owner the class that owns the property
	 * @param name name of the property
	 * @return value of the property
	 */
	public Object getSessionProperty(Class<?> owner, String name) {
		return sessionProperty.get(owner.getName() + "." + name);
	}

	/**
	 * Removes all session properties.
	 *
	 * @param owner the class that owns the properties
	 */
	public void removeSessionProperties(Class<?> owner) {
		Iterator<Map.Entry<String, Object>> i = sessionProperty.entrySet().iterator();
		while (i.hasNext()) {
			if (i.next().getKey().startsWith(owner.getName() + ".")) {
				i.remove();
			}
		}
	}

	/**
	 * Checks SQL query.
	 *
	 * @param sql
	 * @return <code>true</code> iff sql is executable without errors
	 */
	public boolean checkQuery(String sql) {
		try {
			executeQuery(sql, new AbstractResultSetReader() {
				@Override
				public void readCurrentRow(ResultSet resultSet) throws SQLException {
					// nothing to do
				}
			});
		} catch (Exception e) {
			return false;
		}
		return true;
	}

	private final static ThreadLocal<Boolean> sharesConnection = new ThreadLocal<Boolean>();

	public final Object MD_GETCOLUMNS_LOCK = new String("MD_GETCOLUMNS_LOCK");

	public static void setThreadSharesConnection() {
		sharesConnection.set(true);
	}

	private String logPrefix = "";
	
	public void setLogPrefix(String logPrefix) {
		this.logPrefix = logPrefix;
	}

	private static volatile Connection globalFallbackConnection = null;
	private Connection defaultConnection = null;
	public static volatile long lastUpdateTS;
	
	public void setGlobalFallbackConnection() {
		Session.globalFallbackConnection = defaultConnection;
	}

	public static void resetGlobalFallbackConnection() {
		Session.globalFallbackConnection = null;
	}

	public synchronized boolean isConnectionExclusive() {
		Long tid = Thread.currentThread().getId();
		Integer count = connectionCount.get(tid);
		return count != null && count <= 1;
	}

	public synchronized String getConnectionStats() {
		return "ConStats:" + connections.size() + "/" + connectionCount.size() + "/" + connectionCount.keySet().size();
	}

}
