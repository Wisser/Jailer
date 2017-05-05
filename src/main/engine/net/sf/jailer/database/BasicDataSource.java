package net.sf.jailer.database;

import java.io.File;
import java.io.PrintWriter;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.sql.Connection;
import java.sql.Driver;
import java.sql.DriverManager;
import java.sql.DriverPropertyInfo;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.SQLFeatureNotSupportedException;
import java.sql.Statement;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.TreeSet;
import java.util.regex.Pattern;

import javax.sql.DataSource;

import org.apache.log4j.Logger;

import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.configuration.DBMS;

/**
 * Basic implementation of {@link DataSource}. Uses {@link DriverManager} to create connections.
 * 
 * @author Ralf Wisser
 */
public class BasicDataSource implements DataSource {

	/**
     * The logger.
     */
    private static final Logger _log = Logger.getLogger(BasicDataSource.class);
    
    /**
     * Name of JDBC-driver class.
     */
    private final String driverClassName;
    
	/**
     * The DB URL.
     */
    final String dbUrl;
    
    /**
     * The DB user.
     */
    final String dbUser;
    
    /**
     * The DB password.
     */
    private final String dbPassword;

    /**
     * The DBMS.
     */
    public final DBMS dbms;

    /**
     * Maximum number of pooled connections.
     */
    private final int maxPoolSize;

    /**
     * Constructor. Derives DBMS from URL.
     * 
     * @param driverClassName name of JDBC-driver class
     * @param dbUrl the URL
     * @param dbUser the user
     * @param dbPassword the password
     * @param maxPoolSize maximum number of pooled connections
     * @param jdbcDriver driver jar file
     */
	public BasicDataSource(String driverClassName, String dbUrl, String dbUser, String dbPassword, int maxPoolSize, File jdbcDriver) {
		this.driverClassName = driverClassName;
		this.dbUrl = dbUrl;
		this.dbUser = dbUser;
		this.dbPassword = dbPassword;
		this.maxPoolSize = maxPoolSize;
		try {
			loadDriver(jdbcDriver == null? null : new URL[] { jdbcDriver.toURI().toURL() });
		} catch (MalformedURLException e) {
			throw new RuntimeException(e);
		}
		this.dbms = findDBMS();	
	}


    /**
     * Constructor. Derives DBMS from URL.
     * 
     * @param driverClassName name of JDBC-driver class
     * @param dbUrl the URL
     * @param dbUser the user
     * @param dbPassword the password
     * @param maxPoolSize maximum number of pooled connections
     * @param jdbcDriverURL URL of driver jar file
     */
	public BasicDataSource(String driverClassName, String dbUrl, String dbUser, String dbPassword, int maxPoolSize, URL... jdbcDriverURL) {
		this.driverClassName = driverClassName;
		this.dbUrl = dbUrl;
		this.dbUser = dbUser;
		this.dbPassword = dbPassword;
		this.maxPoolSize = maxPoolSize;
		loadDriver(jdbcDriverURL);
		this.dbms = findDBMS();	
	}

	/**
     * Constructor.
     * 
     * @param driverClassName name of JDBC-driver class
     * @param dbUrl the URL
     * @param dbUser the user
     * @param dbPassword the password
     * @param maxPoolSize maximum number of pooled connections
     * @param dbms the DBMS 
     */
	public BasicDataSource(String driverClassName, String dbUrl, String dbUser, String dbPassword, DBMS dbms, int maxPoolSize, URL... jdbcDriverURL) {
		this.driverClassName = driverClassName;
		this.dbUrl = dbUrl;
		this.dbUser = dbUser;
		this.dbPassword = dbPassword;
		this.maxPoolSize = maxPoolSize;
		loadDriver(jdbcDriverURL);
		this.dbms = dbms;
	}

	/**
	 * Closes all pooled connections.
	 */
	public void close() {
		synchronized (pool) {
			for (Connection connection: pool) {
				try {
					connection.close();
				} catch (SQLException e) {
					// ignore
				}
			}
			pool.clear();
		}
	}
	
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
        public java.util.logging.Logger getParentLogger() throws SQLFeatureNotSupportedException {
        	throw new SQLFeatureNotSupportedException();
        }
    }

    private void loadDriver(URL[] jdbcDriverURL) {
    	ClassLoader classLoaderForJdbcDriver = addJarToClasspath(jdbcDriverURL);
    	try {
	    	if (classLoaderForJdbcDriver != null) {
	            Driver d;
				try {
					d = (Driver) Class.forName(driverClassName, true, classLoaderForJdbcDriver).newInstance();
		            DriverManager.registerDriver(new DriverShim(d));
				} catch (InstantiationException e) {
					throw new RuntimeException(e);
				} catch (IllegalAccessException e) {
					throw new RuntimeException(e);
				}
	        } else {
	            Class.forName(driverClassName);
	        }
    	} catch (SQLException e) {
    		throw new RuntimeException(e);
    	} catch (ClassNotFoundException e) {
    		throw new RuntimeException(e);
		}
	}

	private DBMS findDBMS() {
		if (perUrl.containsKey(dbUrl)) {
			return perUrl.get(dbUrl);
		}
        DBMS defaultDBMS = DBMS.forDBMS(null);
        List<DBMS> cs = Configuration.getInstance().getDBMS();  
        for (DBMS c: cs) {
        	if (Pattern.matches(c.getUrlPattern(), dbUrl)) {
        		boolean ok = true;
        		if (c.getTestQuery() != null) {
        			Connection connection = null;
        			try {
        				connection = getConnection(defaultDBMS, false);
        				Statement st = connection.createStatement();
        				ResultSet rs = st.executeQuery(c.getTestQuery());
        				while (rs.next()) {
        				}
        				rs.close();
        				st.close();
					} catch (SQLException e) {
						ok = false;
					} finally {
						if (connection != null) {
							try {
								connection.close();
							} catch (SQLException e) {
							}
						}
					}
        		}
        		if (ok) {
        			perUrl.put(dbUrl, c);
        			return c;
        		}
        	}
        }
		perUrl.put(dbUrl, defaultDBMS);
        return defaultDBMS;
	}

	private final List<Connection> pool = Collections.synchronizedList(new LinkedList<Connection>());
	
	/**
	 * Creates a new connection.
	 * 
	 * @param theDbms the DBMS to use
	 * @return new connection
	 */
    private Connection getConnection(DBMS theDbms, boolean usePool) throws SQLException {
		Connection con = null;

		if (usePool) {
    		synchronized (pool) {
				if (pool.size() > 0) {
					con = pool.remove(0);
				}
			}
    	}
		
		Map<String, String> jdbcProperties = theDbms.getJdbcProperties();
		if (con == null && jdbcProperties != null) {
			try {
				 java.util.Properties info = new java.util.Properties();
				 if (dbUser != null) {
					 info.put("user", dbUser);
				 }
				 if (dbPassword != null) {
					 info.put("password", dbPassword);
				 }
				 for (Map.Entry<String, String> entry: jdbcProperties.entrySet()) {
					 info.put(entry.getKey(), entry.getValue());
				 }
				 con = DriverManager.getConnection(dbUrl, info);
			} catch (SQLException e2) {
				// ignore
			}
		}
		
		if (con == null) {
			con = DriverManager.getConnection(dbUrl, dbUser, dbPassword);
		}
		
		if (maxPoolSize == 0) {
			return con;
		}
		
		final Connection finalCon = con;
		return (Connection) Proxy.newProxyInstance(con.getClass().getClassLoader(), con.getClass().getInterfaces(), new InvocationHandler() {
			private volatile boolean valid = true;
			@Override
			public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
				if ("close".equals(method.getName())) {
					if (valid && pool.size() < maxPoolSize) {
						try {
							finalCon.rollback();
							pool.add(finalCon);
							return null;
						} catch (SQLException e) {
							// ignore
						}
					}
				}
				try {
					return method.invoke(finalCon, args);
				} catch (InvocationTargetException e) {
					if (e.getCause() instanceof SQLException) {
						try {
							valid = finalCon.isValid(10);
						} catch (SQLException e2) {
							valid = false;
						} catch (Throwable t) {
							valid = true;
						}
					}
					throw e;
				}
			}
		});
	}

	/**
	 * Holds all class-loader in order to prevent loading a jar twice.
	 */
	private static Map<String, URLClassLoader> classloaders = new HashMap<String, URLClassLoader>();

	/**
	 * Adds jars to classpath.
	 */
	private static synchronized ClassLoader addJarToClasspath(URL[] jdbcDriverURL) {
		if (jdbcDriverURL.length == 0) {
			return null;
		}
		TreeSet<String> s = new TreeSet<String>();
		for (URL url: jdbcDriverURL) {
			s.add(url.toString());
		}
		String mapKey = s.toString();
		if (classloaders.containsKey(mapKey)) {
			return classloaders.get(mapKey);
		}
		_log.info("added '" + mapKey + "' to classpath");
		URLClassLoader urlLoader = new URLClassLoader(jdbcDriverURL);
		classloaders.put(mapKey, urlLoader);
		return urlLoader;
	}

	/**
     * Holds configurations.
     */
    private static Map<String, DBMS> perUrl = new HashMap<String, DBMS>();

	@Override
	public Connection getConnection() throws SQLException {
		return getConnection(dbms, true);
	}

	@Override
	public PrintWriter getLogWriter() throws SQLException {
		throw new UnsupportedOperationException();
	}

	@Override
	public int getLoginTimeout() throws SQLException {
		throw new UnsupportedOperationException();
	}

	@Override
	public java.util.logging.Logger getParentLogger() throws SQLFeatureNotSupportedException {
		throw new UnsupportedOperationException();
	}

	@Override
	public void setLogWriter(PrintWriter arg0) throws SQLException {
		throw new UnsupportedOperationException();
		
	}

	@Override
	public void setLoginTimeout(int arg0) throws SQLException {
		throw new UnsupportedOperationException();
		
	}

	@Override
	public boolean isWrapperFor(Class<?> arg0) throws SQLException {
		return false;
	}

	@Override
	public <T> T unwrap(Class<T> arg0) throws SQLException {
		throw new UnsupportedOperationException();
	}

	@Override
	public Connection getConnection(String username, String password) throws SQLException {
		throw new UnsupportedOperationException();
	}

}
