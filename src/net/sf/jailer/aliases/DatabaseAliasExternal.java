package net.sf.jailer.aliases;

import java.sql.Connection;
import java.sql.SQLException;

/**
 * @author Vladimir "Dair T'arg" Berkutov
 * @date: 01.03.2008
 * @time: 21:59:05
 */
public class DatabaseAliasExternal extends DatabaseAlias {

	public DatabaseAliasExternal(String url, String user, String password, String jars, String className)
	throws DriverNotFoundException {
		myUrl = url;
		myUser = user;
		myPassword = password;
		myJars = jars;
		myClassName = className;
	}

	public DatabaseAliasExternal(String url, String user, String password, String jars[], String className)
	throws DriverNotFoundException {
		this(url, user, password, JDBCUtil.implode(jars, ":"), className);
	}

	private String myJars;

	public String getJars() {
		return myJars;
	}

	private String myClassName;
	
	public String getClassName() {
		return myClassName;
	}

	/**
	 * Return a connection to the database with this {@code DatabaseAlias}
	 * properties.
	 *
	 * @return A connection to the database.
	 *
	 * @throws SQLException If the connection to the database could not be
	 * established with current data.
	 *
	 * @see net.sf.jailer.aliases.DatabaseAlias#getConnection(String)
	 */
	public Connection getConnection()
	throws SQLException {
		if (myPassword == null) {
			throw new SQLException("Paranoya mode is on - DatabaseAlias contains no password");
		}
		return getConnection(getPassword());
	}

	/**
	 * Return a connection to the database with this {@code DatabaseAlias}
	 * properties and enforced password.
	 *
	 * @param password A password for database access.
	 * @return A connection to the database.
	 *
	 * @throws SQLException If the connection to the database could not be
	 * established with current data.
	 *
	 * @see net.sf.jailer.aliases.DatabaseAlias#getConnection(String, String)
	 */
	public Connection getConnection(String password)
	throws SQLException {
		return getConnection(getUser(), password);
	}

	public Connection getConnection(String user, String password)
	throws SQLException {
		try {
			setDriver(JDBCDriverManager.createDriverInstance(myJars, myClassName));
		} catch (Exception exception) {
			throw new RuntimeException("Could not load database server driver", exception);
		}
		return super.getConnection(user, password);
	}

}
