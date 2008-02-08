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
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.Driver;
import java.sql.DriverManager;
import java.sql.DriverPropertyInfo;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Properties;

import org.apache.log4j.Logger;

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
    private static final Logger _log = Logger.getLogger("sql");
 
    /**
     * Connection factory.
     */
    private interface ConnectionFactory {
        Connection getConnection() throws SQLException;
    };
    
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
    public static ClassLoader classLoaderForJdbcDriver = null;
    
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
     * Constructor.
     * 
     * @param driverClassName name of JDBC-driver class
     * @param dbUrl the database URL
     * @param user the DB-user
     * @param password the DB-password
     */
    public StatementExecutor(String driverClassName, final String dbUrl, final String user, final String password) throws Exception {
        _log.info("connect to user " + user + " at "+ dbUrl);
        if (classLoaderForJdbcDriver != null) {
            Driver d = (Driver)Class.forName(driverClassName, true, classLoaderForJdbcDriver).newInstance();
            DriverManager.registerDriver(new DriverShim(d));
        } else {
            Class.forName(driverClassName);
        }
        this.schemaName = user;
        this.dbUrl = dbUrl;
        this.dbUser = user;
        this.dbPassword = password;
        connectionFactory = new ConnectionFactory() {
            public Connection getConnection() throws SQLException {
                Connection con = connection.get();
                
                if (con == null) {
                    con = DriverManager.getConnection(dbUrl, user, password);
                    con.setAutoCommit(true);
                    try {
                        con.setTransactionIsolation(Connection.TRANSACTION_READ_UNCOMMITTED);
                    } catch (SQLException e) {
                        _log.info("can't set isolation level to UR. Reason: " + e.getMessage());
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
        _log.debug(sqlQuery);
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
     * 
     * @return update-count
     */
    public int executeUpdate(String sqlUpdate) throws SQLException {
        _log.debug(sqlUpdate);
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
                _log.debug("" + rowCount + " row(s)");
            } catch (SQLException e) {
                if (++failures > 10 || e.getErrorCode() != -911) {
                    throw e;
                }
                // deadlock
                serializeAccess = true;
                _log.info("Deadlock! Try again.");
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
        _log.debug(sqlUpdate);
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
        _log.debug(sqlUpdate);
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
        _log.debug(sql);
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
     * Sets Classloader to load Jdbc-Driver with.
     * 
     * @param classLoader Classloader to load Jdbc-Driver with
     */
    public static void setClassLoaderForJdbcDriver(ClassLoader classLoader) {
        classLoaderForJdbcDriver = classLoader;
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
    
}
