/*
 * Copyright 2007 - 2019 the original author or authors.
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

import javax.sql.DataSource;

import org.apache.log4j.Logger;

import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.util.CancellationHandler;
import net.sf.jailer.util.CellContentConverter;

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
	
	private final boolean transactional;
	public final boolean local;
	
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
	public static final Logger _log  = Logger.getLogger("sql");

	/**
	 * Connection factory.
	 */
	public interface ConnectionFactory {
		Connection getConnection() throws SQLException;
	};
	
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
		this.local = local;
		this.scope = scope;
		this.dbms = dbms;
		this.driverClassName = (dataSource instanceof BasicDataSource)? ((BasicDataSource) dataSource).driverClassName : null;
		this.dbUrl = (dataSource instanceof BasicDataSource)? ((BasicDataSource) dataSource).dbUrl : null;
		this.schema = (dataSource instanceof BasicDataSource)? ((BasicDataSource) dataSource).dbUser : "";
		this.temporaryTableScope = scope;
		
		connectionFactory = new ConnectionFactory() {
			private Connection defaultConnection = null;
			private Random random = new Random();
			@Override
			public synchronized Connection getConnection() throws SQLException {
				@SuppressWarnings("resource")
				Connection con = local? connection.get() : temporaryTableSession == null? connection.get() : temporaryTableSession;
				
				if (con == null) {
					try {
						con = dataSource.getConnection();
						defaultConnection = con;
					} catch (SQLException e) {
						if (connections != null && connections.size() > 1) {
							con = connections.get(random.nextInt(connections.size()));
						} else if (defaultConnection != null) {
							// fall back to default connection
							con = defaultConnection;
						} else {
							throw e;
						}
					}
					boolean ac = scope == null || scope != WorkingTableScope.TRANSACTION_LOCAL;
					if (Session.this.transactional) {
						ac = false;
					}
					_log.info("set auto commit to " + ac);
					con.setAutoCommit(ac);
					try {
						if (isolationLevel != null) {
							_log.info("set isolation level to " + isolationLevel);
							con.setTransactionIsolation(isolationLevel);
							_log.info("isolation level is " + con.getTransactionIsolation());
						}
					} catch (SQLException e) {
						_log.warn("can't set isolation level to UR. Reason: " + e.getMessage());
					}
					if ((Session.this.transactional && !local) || scope == WorkingTableScope.SESSION_LOCAL || scope == WorkingTableScope.TRANSACTION_LOCAL) {
						temporaryTableSession = con;
					} else {
						connection.set(con);
						boolean addCon = true;
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
				return con;
			}
		};
		// fail fast
		init();
	}

	protected void init() throws SQLException {
		Connection connection = connectionFactory.getConnection();
		logDriverInfo(connection);
	}

	/**
	 * Closes current connection and opens a new one.
	 */
	public void reconnect() throws SQLException {
		Connection con = connection.get();
		if (con != null) {
			if (temporaryTableScope == WorkingTableScope.TRANSACTION_LOCAL) {
				con.commit();
			}
			con.close();
			connection.set(null);
			if (con == temporaryTableSession) {
				temporaryTableSession = null;
				return;
			}
		}
		if (temporaryTableSession != null) {
			if (temporaryTableScope == WorkingTableScope.TRANSACTION_LOCAL) {
				temporaryTableSession.commit();
			}
			temporaryTableSession.close();
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
			_log.info("driver name:    " + meta.getDriverName());
			_log.info("driver version: " + meta.getDriverVersion());
			_log.info("DB name:        " + meta.getDatabaseProductName() + (dbms.getDisplayName() != null? " (" + dbms.getDisplayName() + ")" : ""));
			_log.info("DB version:     " + meta.getDatabaseProductVersion());
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
	 */
	public long executeQuery(String sqlQuery, ResultSetReader reader) throws SQLException {
		return executeQuery(sqlQuery, reader, null, null, 0);
	}
	
	/**
	 * Executes a SQL-Query (SELECT).
	 * 
	 * @param sqlQuery the query in SQL
	 * @param reader the reader for the result
	 * @param alternativeSQL query to be executed if sqlQuery fails
	 * @param limit row limit, 0 for unlimited
	 * @param context cancellation context
	 */
	public long executeQuery(String sqlQuery, ResultSetReader reader, String alternativeSQL, Object context, int limit) throws SQLException {
		return executeQuery(sqlQuery, reader, alternativeSQL, context, limit, 0);
	}

	/**
	 * Executes a SQL-Query (SELECT) with timeout.
	 * 
	 * @param theConnection connection to use
	 * @param sqlQuery the query in SQL
	 * @param reader the reader for the result
	 * @param alternativeSQL query to be executed if sqlQuery fails
	 * @param limit row limit, 0 for unlimited
	 * @param context cancellation context
	 * @param timeout the timeout in sec
	 */
	private long executeQuery(Connection theConnection, String sqlQuery, ResultSetReader reader, String alternativeSQL, Object context, int limit, int timeout) throws SQLException {
		long rc = 0;
		CancellationHandler.checkForCancellation(context);
		Statement statement = null;
		try {
			statement = theConnection.createStatement();
			if (dbms != null) {
				if (dbms.getFetchSize() != null) {
					statement.setFetchSize(dbms.getFetchSize());
				}
			}
			CancellationHandler.begin(statement, context);
			ResultSet resultSet;
			try {
				if (timeout > 0) {
					statement.setQueryTimeout(timeout);
				}
				resultSet = statement.executeQuery(sqlQuery);
			} catch (SQLException e) {
				if (alternativeSQL != null) {
					_log.warn("query failed, using alternative query. Reason: " + e.getMessage());
					_log.info(alternativeSQL);
					CancellationHandler.checkForCancellation(context);
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
				CancellationHandler.end(statement, context);
			}
		}
		if (getLogStatements()) {
			_log.info(rc + " row(s)");
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
	 */
	public long executeQuery(String sqlQuery, ResultSetReader reader, String alternativeSQL, Object context, int limit, int timeout) throws SQLException {
		if (getLogStatements()) {
			_log.info(sqlQuery);
		}
		try {
			return executeQuery(connectionFactory.getConnection(), sqlQuery, reader, alternativeSQL, context, limit, timeout);
		} catch (SQLException e) {
			CancellationHandler.checkForCancellation(context);
			if (!silent) {
				_log.error("Error executing query", e);
			}
			if (e instanceof SqlException) {
				throw e;
			}
			throw new SqlException("\"" + e.getMessage() + "\" in statement \"" + sqlQuery + "\"", sqlQuery, e);
		}
	}

	/**
	 * Executes a SQL-Query (SELECT).
	 * 
	 * @param sqlFile file containing a query in SQL
	 * @param reader the reader for the result
	 */
	public void executeQuery(File sqlFile, ResultSetReader reader) throws SQLException {
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

		executeQuery(result.toString(), reader);
	}

	/**
	 * Prevention of livelocks.
	 */
	private final int PERMITS = Integer.MAX_VALUE / 4;
	private Semaphore semaphore = new Semaphore(PERMITS);

	/**
	 * Executes a SQL-Update (INSERT, DELETE or UPDATE).
	 * 
	 * @param sqlUpdate the update in SQL
	 * 
	 * @return update-count
	 */
	public int executeUpdate(String sqlUpdate) throws SQLException {
		if (getLogStatements()) {
			_log.info(sqlUpdate);
		}
		CancellationHandler.checkForCancellation(null);
		final int maximumNumberOfFailures = 10;
		try {
			int rowCount = 0;
			int failures = 0;
			boolean ok = false;
			boolean serializeAccess = false;

			while (!ok) {
				Statement statement = null;
				try {
					statement = connectionFactory.getConnection().createStatement();
					CancellationHandler.begin(statement, null);
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

					CancellationHandler.end(statement, null);
					ok = true;
					if (getLogStatements()) {
						_log.info("" + rowCount + " row(s)");
					}
				} catch (SQLException e) {
					CancellationHandler.checkForCancellation(null);
					CancellationHandler.end(statement, null);

					boolean deadlock = "40001".equals(e.getSQLState()); // "serialization failure", see https://en.wikipedia.org/wiki/SQLSTATE
					boolean crf = DBMS.ORACLE.equals(dbms) && e.getErrorCode() == 8176; // ORA-08176: consistent read failure; rollback data not available
					
					if (++failures > maximumNumberOfFailures || !(deadlock || crf)) {
						throw new SqlException("\"" + e.getMessage() + "\" in statement \"" + sqlUpdate + "\"", sqlUpdate, e);
					}
					// deadlock
					serializeAccess = true;
					_log.info("Deadlock! Try again...");
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
				_log.error("Error executing statement", e);
			} else {
				String message = e.getMessage();
				if (e instanceof SqlException) {
					message = e.getCause().getMessage();
				}
				_log.info("\"" + message + "\"");
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
			_log.info(sqlUpdate);
		}
		try {
			CancellationHandler.checkForCancellation(null);
			int rowCount = 0;
			PreparedStatement statement = null;
			try {
				statement = connectionFactory.getConnection().prepareStatement(sqlUpdate);
				CancellationHandler.begin(statement, null);
				int i = 1;
				for (Object p: parameter) {
					statement.setObject(i++, p);
				}
				rowCount = statement.executeUpdate();
				CancellationHandler.end(statement, null);
				if (getLogStatements()) {
					_log.info("" + rowCount + " row(s)");
				}
			} finally {
				if (statement != null) {
					try { statement.close(); } catch (SQLException e) { }
				}
			}
			return rowCount;
		} catch (SQLException e) {
			CancellationHandler.checkForCancellation(null);
			if (!silent) {
				_log.error("Error executing statement", e);
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
			_log.info(sqlUpdate);
		}
		PreparedStatement statement = null;
		try {
			statement = connectionFactory.getConnection().prepareStatement(sqlUpdate);
			CancellationHandler.begin(statement, null);
			InputStreamReader inputStreamReader = new InputStreamReader(new FileInputStream(lobFile), "UTF-8");
			statement.setCharacterStream(1, inputStreamReader, (int) length);
			statement.execute();
			inputStreamReader.close();
		} catch (SQLException e) {
			CancellationHandler.checkForCancellation(null);
			throw e;
		} finally {
			if (statement != null) {
				try {
					statement.close();
					CancellationHandler.end(statement, null);
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
		_log.info(sqlUpdate);
		PreparedStatement statement = null;
		try {
			statement = connectionFactory.getConnection().prepareStatement(sqlUpdate);
			CancellationHandler.begin(statement, null);
			InputStreamReader inputStreamReader = new InputStreamReader(new FileInputStream(lobFile), "UTF-8");
			statement.setCharacterStream(1, inputStreamReader, (int) length);
			statement.execute();
			inputStreamReader.close();
		} catch (SQLException e) {
			CancellationHandler.checkForCancellation(null);
			throw e;
		} finally {
			if (statement != null) {
				try {
					statement.close();
					CancellationHandler.end(statement, null);
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
		_log.info(sqlUpdate);
		PreparedStatement statement = null;
		try {
			statement = connectionFactory.getConnection().prepareStatement(sqlUpdate);
			CancellationHandler.begin(statement, null);
			FileInputStream fileInputStream = new FileInputStream(lobFile);
			statement.setBinaryStream(1, fileInputStream, (int) lobFile.length());
			statement.execute();
			fileInputStream.close();
		} catch (SQLException e) {
			CancellationHandler.checkForCancellation(null);
			throw e;
		} finally {
			if (statement != null) {
				try {
					statement.close();
					CancellationHandler.end(statement, null);
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
		return execute(sql, null);
	}

	/**
	 * Executes a SQL-Statement without returning any result.
	 * 
	 * @param sql the SQL-Statement
	 */
	public long execute(String sql, Object cancellationContext) throws SQLException {
		if (getLogStatements()) {
			_log.info(sql);
		}
		long rc = 0;
		Statement statement = null;
		try {
			CancellationHandler.checkForCancellation(cancellationContext);
			statement = connectionFactory.getConnection().createStatement();
			CancellationHandler.begin(statement, cancellationContext);
			rc = statement.executeUpdate(sql);
			if (getLogStatements()) {
				_log.info("" + rc + " row(s)");
			}
		} catch (SQLException e) {
			CancellationHandler.checkForCancellation(cancellationContext);
			if (!silent) {
				_log.error("Error executing statement", e);
			}
			throw new SqlException("\"" + e.getMessage() + "\" in statement \"" + sql + "\"", sql, e);
		} finally {
			if (statement != null) {
				try {
					statement.close();
				} catch (SQLException e) {
					// ignore
				}
				CancellationHandler.end(statement, cancellationContext);
			}
		}
		return rc;
	}
	
	/**
	 * Cached Database Meta Data.
	 */
	private Map<Connection, DatabaseMetaData> metaData = Collections.synchronizedMap(new IdentityHashMap<Connection, DatabaseMetaData>());
	
	/**
	 * Gets DB meta data.
	 * 
	 * @return DB meta data
	 */
	public DatabaseMetaData getMetaData() throws SQLException {
		Connection con = connectionFactory.getConnection();
		DatabaseMetaData mData = metaData.get(con);
		if (mData != null) {
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

	protected boolean down = false;
	
	/**
	 * Closes all connections.
	 */
	public void shutDown() throws SQLException {
		synchronized (this) {
			down = true;
		}
		_log.info("closing connection...");
		for (Connection con: connections) {
			con.close();
		}
		closeTemporaryTableSession();
		_log.info("connection closed");
	}

	public synchronized boolean isDown() {
		return down;
	}

	/**
	 * Rolls back and closes all connections.
	 */
	public void rollbackAll() throws SQLException {
		for (Connection con: connections) {
			try {
				con.rollback();
			} catch(SQLException e) {
				_log.warn(e.getMessage());
			}
			try {
				con.close();
			} catch(SQLException e) {
				_log.warn(e.getMessage());
			}
		}
		if (temporaryTableSession != null) {
			try {
				temporaryTableSession.rollback();
			} catch(SQLException e) {
				_log.warn(e.getMessage());
			}
		 }
		connection = new ThreadLocal<Connection>();
	}
	
	/**
	 * Commits all connections.
	 */
	public void commitAll() throws SQLException {
		for (Connection con: connections) {
			try {
				con.commit();
			} catch(SQLException e) {
				_log.warn(e.getMessage());
			}
		}
		if (temporaryTableSession != null) {
			try {
				temporaryTableSession.commit();
			} catch(SQLException e) {
				_log.warn(e.getMessage());
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
			_log.error("can't close connection", e);
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
	
	/**
	 * Gets connection password (UI support)
	 */
	public synchronized String getPassword() {
		return password;
	}

	/**
	 * Sets connection password (UI support)
	 */
	public synchronized void setPassword(String password) {
		this.password = password;
	}
	
	/**
	 * Sets CLI connection arguments (UI support)
	 */
	public synchronized void setCliArguments(List<String> args) {
		this.cliArguments = args;
	}
	
	/**
	 * Gets CLI connection arguments (UI support)
	 */
	public synchronized List<String> getCliArguments() {
		return cliArguments;
	}

	/**
	 * Gets the connection for the current thread.
	 * 
	 * @return the connection for the current thread
	 */
	public Connection getConnection() throws SQLException {
		return connectionFactory.getConnection();
	}
	
	private InlineViewStyle inlineViewStyle;
	private boolean noInlineViewStyleFound = false;
	
	/**
	 * Returns a suitable {@link InlineViewStyle} for this session.
	 * 
	 * @return a suitable {@link InlineViewStyle} for this session or <code>null</code>, if no style is found
	 */
	public synchronized InlineViewStyle getInlineViewStyle() {
		if (inlineViewStyle == null && !noInlineViewStyleFound) {
			try {
				inlineViewStyle = InlineViewStyle.forSession(this);
			} catch (Exception e) {
				// no suitable style found
				noInlineViewStyleFound = true;
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

}
