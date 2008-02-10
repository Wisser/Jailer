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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.sql.*;
import java.util.*;
import java.text.MessageFormat;

import org.apache.log4j.Logger;
import net.sf.jailer.drivermanager.JDBCDriverManager;

/**
 * Executes SQL-Statements.
 * Holds connections to the database on a 'per thread' basis.
 * 
 * @author Wisser
 */
public class StatementExecutor {

    /**
     * Hold a connection for each thread.
     */
    private final ThreadLocal<Connection> connection = new ThreadLocal<Connection>();
    
    /**
     * Holds all connections.
     */
    private final Collection<Connection> connections = Collections.synchronizedCollection(new ArrayList<Connection>());
    
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
     * Connection factory.
     */
    private interface ConnectionFactory {
        Connection getConnection() throws SQLException;
    }
    
    /**
     * Connection factory.
     */
    private final ConnectionFactory connectionFactory;

    /**
     * The DB schema name.
     */
    private final String schemaName;

    /**
     * The DB URL.
     */
    public final String dbUrl;
    
    /**
     * The DB user.
     */
    public final String dbUser;
    
    /**
     * The DB password.
     */
    public final String dbPassword;

    /**
     * Optional schema for introspection.
     */
    private String introspectionSchema;
    
    /**
     * Classloader to load Jdbc-Driver with.
     */
    //public static ClassLoader myJDBCDriverLoader = null;
    
    /**
     * Wraps a Jdbc-Driver.
     */
    public static class DriverShim implements Driver {
        private Driver driver;
        public DriverShim(Driver d) {
            this.driver = d;
        }
        public boolean acceptsURL(String u) throws SQLException {
            return this.driver.acceptsURL(u);
        }
        public Connection connect(String u, Properties p) throws SQLException {
            return this.driver.connect(u, p);
        }
        public int getMajorVersion() {
            return this.driver.getMajorVersion();
        }
        public int getMinorVersion() {
            return this.driver.getMinorVersion();
        }
        public DriverPropertyInfo[] getPropertyInfo(String u, Properties p) throws SQLException {
            return this.driver.getPropertyInfo(u, p);
        }
        public boolean jdbcCompliant() {
            return this.driver.jdbcCompliant();
        }
    }

    /**
     * Constructs {@code StatementExecutor} for executing statements at
	 * specified DB-server with specified DB-driver and authorizes using
	 * specified DB-username and DB-password.
     * 
     * @param driverClassName Name of JDBC-driver class
     * @param host The database host URL
     * @param user The DB-user
     * @param password The DB-password
     */
    public StatementExecutor(String driverClassName, final String host, final String user, final String password)
	throws Exception {
		// todo: replace with locale string
		myLog.info("connecting to db-server at " + host + " with user " + user + ";");
		// Loading and registering a driver.
		DriverManager.registerDriver(JDBCDriverManager.getDriver(driverClassName));
        this.schemaName = user;
        this.dbUrl = host;
        this.dbUser = user;
        this.dbPassword = password;
        connectionFactory = new ConnectionFactory() {
            public Connection getConnection() throws SQLException {
                Connection con = connection.get();
                
                if (con == null) {
                    con = DriverManager.getConnection(host, user, password);
                    con.setAutoCommit(true);
                    try {
                        con.setTransactionIsolation(Connection.TRANSACTION_READ_UNCOMMITTED);
                    } catch (SQLException e) {
                        myLog.info("can't set isolation level to UR. Reason: " + e.getMessage());
                    }
                    connection.set(con);
                    connections.add(con);
                }
                return con;
            }
        };
        // fail fast
        connectionFactory.getConnection();
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
        Statement statement = connectionFactory.getConnection().createStatement();
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
                statement = connectionFactory.getConnection().createStatement();
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
     */
    public void insertClob(String table, String column, String where, File lobFile) throws SQLException, FileNotFoundException {
    	String sqlUpdate = "Update " + table + " set " + column + "=? where " + where;
        myLog.debug(sqlUpdate);
        PreparedStatement statement = null;
        statement = connectionFactory.getConnection().prepareStatement(sqlUpdate);
        statement.setCharacterStream(1, new InputStreamReader(new FileInputStream(lobFile)), (int) lobFile.length());
        statement.execute();
        statement.close();
    }

    /**
     * Inserts a BLob.
     */
    public void insertBlob(String table, String column, String where, File lobFile) throws SQLException, FileNotFoundException {
    	String sqlUpdate = "Update " + table + " set " + column + "=? where " + where;
        myLog.debug(sqlUpdate);
        PreparedStatement statement = null;
        statement = connectionFactory.getConnection().prepareStatement(sqlUpdate);
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
        Statement statement = connectionFactory.getConnection().createStatement();
        statement.execute(sql);
        statement.close();
    }
    
    /**
     * Gets DB meta data.
     * 
     * @return DB meta data
     */
    public DatabaseMetaData getMetaData() throws SQLException {
        Connection connection = connectionFactory.getConnection();
        return connection.getMetaData();
    }

    /**
     * Closes all connections.
     */
    public void shutDown() throws SQLException {
        for (Connection con: connections) {
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
    
}
