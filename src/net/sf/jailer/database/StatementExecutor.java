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

import net.sf.jailer.aliases.DatabaseAlias;
import org.apache.log4j.Logger;

import java.io.*;
import java.sql.*;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

/**
 * Executes SQL-Statements.
 * Holds connections to the database on a 'per thread' basis.
 * 
 * @author Wisser
 * @author Vladimir "Dair T'arg" Bekutov
 */
public class StatementExecutor {

    /**
     * Hold a connection for each thread.
     */
    private final ThreadLocal<Connection> myConnection = new ThreadLocal<Connection>();
    
    /**
     * Holds all connections.
     */
    private final Collection<Connection> myConnections = Collections.synchronizedCollection(new ArrayList<Connection>());
    
    /**
     * Reads a JDBC-result-set.
     */
    public interface ResultSetReader {
    
        /**
         * Reads current row of a result-set.
         * 
         * @param resultSet the result-set
		 * @throws SQLException If the reading has been executed with errors.
         */
        void readCurrentRow(ResultSet resultSet) throws SQLException;

        /**
         * Finalizes reading.
         */
        void close();
    }
    
    /**
     * Reads a JDBC-result-set.
     */
    public static abstract class AbstractResultSetReader implements ResultSetReader {
    
        /**
         * Does nothing.
         */
        public void close() {
        }
    }
    
    /**
     * The logger.
     */
    private static final Logger myLog = Logger.getLogger("sql");

	/**
     * The DB schema name.
     */
    private final String schemaName;

	/**
     * Optional schema for introspection.
     */
    private String introspectionSchema;
    
    /**
     * Constructs {@code StatementExecutor} for executing statements at
	 * specified DB-server with specified DB-driver and authorizes using
	 * specified DB-username and DB-password.
     * 
     * @param driverClass name of driver class
     * @param host The database host URL
     * @param user The DB-user
     * @param password The DB-password
	 *
	 * @throws Exception
     */
    public StatementExecutor(String driverClass, final String host, final String user, final String password)
	throws Exception {
		// todo: replace with locale string
		myLog.info("connecting to db-server at " + host + " with user " + user + ";");
		myDatabaseAlias = new DatabaseAlias(host, user, password);
        this.schemaName = user;
        Connection connection = getConnection();
	    logDriverInfo(connection);
	}
	
	/**
	 * Logs driver info
	 * 
	 * @param connection connection to DB
	 */
	private void logDriverInfo(Connection connection) {
		try {
			DatabaseMetaData meta = connection.getMetaData();
			myLog.info("driver name:    " + meta.getDriverName());
			myLog.info("driver version: " + meta.getDriverVersion());
			myLog.info("DB name:        " + meta.getDatabaseProductName());
			myLog.info("DB version:     " + meta.getDatabaseProductVersion());
		} catch (Exception e) {
			// ignore exceptions
		}
	}

	///////////////////////////
	// Default DB server url //
	///////////////////////////

    /**
     * The database alias.
     */
    private final DatabaseAlias myDatabaseAlias;
    
	//private String myHost;

	/**
	 * Returns a sql-server host.
	 *
	 * @return A sql-server host.
	 */
	public String getHost() {
		return myDatabaseAlias.getURL();
	}

	//////////////////////
	// Default username //
	//////////////////////

	//private String myUsername;

	/**
	 * Returns a username.
	 *
	 * @return A username.
	 */
	public String getUsername() {
		return myDatabaseAlias.getUser();
	}

	//////////////////////
	// Default password //
	//////////////////////

	//private String myPassword;

	/**
	 * Returns a password.
	 *
	 * @return A password.
	 */
	public String getPassword() {
		return myDatabaseAlias.getPassword();
	}

    /**
     * Gets DB schema name.
     * 
     * @return DB schema name
     */
    public Object getSchemaName() {
        return schemaName;
    }

    /**
     * Executes a SQL-Query (SELECT).
     * 
     * @param sqlQuery the query in SQL
     * @param reader the reader for the result
     */
    public void executeQuery(String sqlQuery, ResultSetReader reader) throws SQLException {
        myLog.debug(sqlQuery);
        Statement statement = getConnection().createStatement();
        ResultSet resultSet = statement.executeQuery(sqlQuery);
        while (resultSet.next()) {
            reader.readCurrentRow(resultSet);
        }
        reader.close();
        resultSet.close();
        statement.close();
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
                result.append(System.getProperty("line.separator"));
            }
            in.close();
        } catch (IOException e) {
            throw new RuntimeException("Failed to load content of file", e);
        }

        executeQuery(result.toString(), reader);
    }

    /**
     * Lock for prevention of livelocks.
     */
    private static final Object DB_LOCK = "DB_LOCK";
    
    /**
     * Executes a SQL-Update (INSERT, DELETE or UPDATE).
     * 
     * @param sqlUpdate the update in SQL
     * @return update-count
	 * @throws SQLException
     */
    public int executeUpdate(String sqlUpdate) throws SQLException {
        myLog.debug(sqlUpdate);
        int rowCount = 0;
        int failures = 0;
        boolean ok = false;
        boolean serializeAccess = false;
        while (!ok) {
            Statement statement = null;
            try {
                statement = getConnection().createStatement();
                if (serializeAccess) {
                    synchronized (DB_LOCK) {
                        rowCount = statement.executeUpdate(sqlUpdate);
                    }
                } else {
                    rowCount = statement.executeUpdate(sqlUpdate);
                }
                ok = true;
                myLog.debug(MessageFormat.format("{0} row(s) affected", rowCount));
            } catch (SQLException e) {
				// todo: replace '-911' sql-error with an appropriate constant.
				if (++failures > myMaxFailures || e.getErrorCode() != -911) {
                    throw e;
                }
                // deadlock
                serializeAccess = true;
                myLog.info("Deadlock! Try again.");
            } finally {
                if (statement != null) {
                    statement.close();
                }
            }
        }
        return rowCount;
    }
    
    /**
     * Inserts a CLob.
	 *
	 * @param table
	 * @param column
	 * @param where
	 * @param lobFile
	 */
    public void insertClob(String table, String column, String where, File lobFile) throws SQLException, FileNotFoundException {
    	String sqlUpdate = "Update " + table + " set " + column + "=? where " + where;
        myLog.debug(sqlUpdate);
        PreparedStatement statement = null;
        statement = getConnection().prepareStatement(sqlUpdate);
        statement.setCharacterStream(1, new InputStreamReader(new FileInputStream(lobFile)), (int) lobFile.length());
        statement.execute();
        statement.close();
    }

    /**
     * Inserts a BLob.
	 *
	 * @param table
	 * @param column
	 * @param where
	 * @param lobFile
     */
    public void insertBlob(String table, String column, String where, File lobFile) throws SQLException, FileNotFoundException {
    	String sqlUpdate = "Update " + table + " set " + column + "=? where " + where;
        myLog.debug(sqlUpdate);
        PreparedStatement statement = null;
        statement = getConnection().prepareStatement(sqlUpdate);
        statement.setBinaryStream(1, new FileInputStream(lobFile), (int) lobFile.length());
        statement.execute();
        statement.close();
    }

    /**
     * Executes a SQL-Statement without returning any result.
     * 
     * @param sql the SQL-Statement
     */
    public void execute(String sql) throws SQLException {
        myLog.debug(sql);
        Statement statement = getConnection().createStatement();
        statement.execute(sql);
        statement.close();
    }
    
    /**
     * Gets DB meta data.
     * 
     * @return DB meta data
     */
    public DatabaseMetaData getMetaData() throws SQLException {
        Connection connection = getConnection();
        return connection.getMetaData();
    }

    /**
     * Closes all connections.
	 * @throws java.sql.SQLException
	 */
    public void shutDown() throws SQLException {
        for (Connection con: myConnections) {
            con.close();
        }
    }
    
    /**
     * Gets optional schema for introspection.
     * 
     * @return optional schema for introspection
     */
    public String getIntrospectionSchema() {
    	return introspectionSchema;
    }
    
    /**
     * Sets optional schema for introspection.
     * 
     * @param introspectionSchema optional schema for introspection
     */
    public void setIntrospectionSchema(String introspectionSchema) {
    	this.introspectionSchema = introspectionSchema;
    }

	//////////////////////////
	// Max Filures settings //
	//////////////////////////

	/**
	 * A maximum number of failures that could occurs during exeution of one
	 * query before an exception will be thrown out.
	 *
	 * The default value is 10.
	 *
	 * @see #getMaxFailures()
	 * @see #setMaxFailures(int)
	 */
	protected int myMaxFailures = 10;

	/**
	 * Returns a maximum number of failures during query execution.
	 *
	 * @return A maximum number of failures during query execution.
	 *
	 * @see #setMaxFailures(int)
	 */
	public int getMaxFailures() {
		return myMaxFailures;
	}

	/**
	 * Sets a maximum number of failures during query execution.
	 *
	 * @param maxFailures A new maximum number of failures during query
	 * execution.
	 * @throws IllegalArgumentException If a received value is negative.
	 *
	 * @see #setMaxFailures(int)
	 */
	public void setMaxFailures(int maxFailures)
	throws IllegalArgumentException {
		if (maxFailures < 0) {
			throw new IllegalArgumentException("The maximum number of failures should be positive value");
		}
		myMaxFailures = maxFailures;
	}

	/**
	 * Returns a connection to a database server.
	 *
	 * @return A connection to a database server.
	 * 
	 * @throws SQLException If some SQL errors occured.
	 */
	public Connection getConnection()
	throws SQLException {
		Connection connection = myConnection.get();
		if (connection == null) {
			connection = myDatabaseAlias.getConnection();
			connection.setAutoCommit(true);
			try {
				connection.setTransactionIsolation(Connection.TRANSACTION_READ_UNCOMMITTED);
			} catch (SQLException e) {
				myLog.info("can't set isolation level to UR. Reason: " + e.getMessage());
			}
			myConnection.set(connection);
			myConnections.add(connection);
		}
		return connection;
	}
}
