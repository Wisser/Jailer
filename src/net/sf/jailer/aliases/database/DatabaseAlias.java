package net.sf.jailer.aliases.database;

import net.sf.jailer.aliases.JDBCDriverManager;
import net.sf.jailer.aliases.driver.DriverAlias;
import net.sf.jailer.aliases.driver.DriverNotFoundException;

import java.io.OutputStream;
import java.io.PrintStream;
import java.sql.Connection;
import java.sql.Driver;
import java.sql.SQLException;
import java.util.List;
import java.util.Properties;

/**
 * @author Vladimir "Dair T'arg" Berkutov
 * @date: 12.02.2008
 * @time: 0:05:42
 */
public class DatabaseAlias {
	
	//////////////////
	// Constructors //
	//////////////////

	/**
	 * Constructs a {@code DatabaseAlias} for a database at the specified
	 * {@code url} with a specified {@code user}.
	 *
	 * Note, that this constructor does not set a password to the database, so
	 * a password should be passed during each of connection establishings.
	 *
	 * @param url An url string of the database.
	 * @param user A user of the database.
	 *
	 * @throws net.sf.jailer.aliases.driver.DriverNotFoundException If there were no drivers found at the
	 * drivers repository.
	 *
	 * @see DatabaseAlias#DatabaseAlias(String, String, java.sql.Driver)
	 */
	public DatabaseAlias(String url, String user)
	throws DriverNotFoundException {
		this(url, user, JDBCDriverManager.getDriverForURL(url));
	}

	/**
	 * Constructs a DatabaseAlias for the specified database at {@code url}
	 * which could be accessed with {@code user} name and {@code password}.
	 *
	 * @param url A database url ({@code jdbc:mysql://localhost:3306/} for
	 * example).
	 * @param user A database user.
	 * @param password A password for the {@code user}.
	 * 
	 * @throws DriverNotFoundException If no {@link java.sql.Driver}s for the
	 * specified subprotocol has been found.
	 *
	 * @see DatabaseAlias#DatabaseAlias(String, String, String, java.sql.Driver)
	 */
	public DatabaseAlias(String url, String user, String password)
	throws DriverNotFoundException {
		this(url, user, password, JDBCDriverManager.getDriverForURL(url));
	}

	/**
	 * Constructs a {@code DatabaseAlias} for a database at the specified
	 * {@code url} with the specified {@code user} and {@link java.sql.Driver}
	 * for connections.
	 *
	 * @param url A database url ({@code jdbc:mysql://localhost:3306/} for
	 * example).
	 * @param user A database user.
	 * @param driver A driver for all connections with the database.
	 *
	 * @throws DriverNotFoundException If the specified url string is
	 * unacceptible by the specified driver.
	 *
	 * @see DatabaseAlias#DatabaseAlias(String, String, String, java.sql.Driver)
	 */
	public DatabaseAlias(String url, String user, Driver driver)
	throws DriverNotFoundException {
		this(url, user, null, driver);
	}

	/**
	 * Constructs a DatabaseAlias for the specified database at {@code url}
	 * which could be accessed with {@code user} name and {@code password} using
	 * the specified {@link java.sql.Driver}.
	 *
	 * @param url A database url ({@code jdbc:mysql://localhost:3306/} for
	 * example).
	 * @param user A database user.
	 * @param password A password for the {@code user}.
	 * @param driver A driver for all connections with the database.
	 *
	 * @throws DriverNotFoundException If the specified url string is
	 * unacceptible by the specified driver or a test connection has failed.
	 */
	public DatabaseAlias(String url, String user, String password, Driver driver)
	throws DriverNotFoundException {
		super();
		// Auth properties.
		myUrl = url;
		myUser = user;
		myPassword = password;
		// Driver
		setDriver(driver);
	}

	public DatabaseAlias(String name, String url, String user, String password) {
		setName(name);
		myUrl = url;
		myUser = user;
		myPassword = password;
	}

	protected DatabaseAlias() {}

	protected void setDriver(Driver driver)
	throws DriverNotFoundException {
		if (driver == null) {
			myDriver = null;
		} else {
			try {
				if (driver.acceptsURL(myUrl)) {
					myDriver = driver;
				} else {
					throw new SQLException("The url string is not acceptible by the specified driver");
				}
			} catch (SQLException exception) {
				throw new DriverNotFoundException("Unacceptible url string", exception);
			}
		}
	}

	//////////
	// Name //
	//////////

	/**
	 * The alias name.
	 */
	protected String myName;

	/**
	 * Returns the alias name.
	 * <p>
	 * If the alias name is not set then the jdbc url will be returned.
	 * </p>
	 *
	 * @return The alias name. If the alias name is not set then the jdbc url
	 * will be returned.
	 */
	public String getName() {
		if (myName == null || myName.equals("")) {
			return myUrl;
		}
		return myName;
	}

	/**
	 * Sets the alias name.
	 *
	 * @param name Any string.
	 */
	public void setName(String name) {
		if (name.equals("")) {
			myName = null;
		} else {
			myName = name;
		}
	}
	
	////////////
	// Driver //
	////////////

	protected Driver myDriver;

	/**
	 * Return a connection to the database with this {@code DatabaseAlias}
	 * properties.
	 *
	 * @return A connection to the database.
	 *
	 * @throws SQLException If the connection to the database could not be
	 * established with current data.
	 *
	 * @see DatabaseAlias#getConnection(String)
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
	 * @see DatabaseAlias#getConnection(String, String)
	 */
	public Connection getConnection(String password)
	throws SQLException {
		return getConnection(getUser(), password);
	}

	/**
	 * Return a connection to the database with this {@code DatabaseAlias}
	 * properties and enforced password.
	 *
	 * @param user A user of the database.
	 * @param password A password for database access.
	 * @return A connection to the database.
	 *
	 * @throws SQLException If the connection to the database could not be
	 * established with current data.
	 *
	 * @see java.sql.Driver#connect(String, java.util.Properties)
	 */
	public Connection getConnection(String user, String password)
	throws SQLException {
		if (myDriver == null) {
			try {
				myDriver = JDBCDriverManager.getDriverForURL(myUrl);
			} catch (DriverNotFoundException exception) {
				throw new SQLException("No driver has been found to establish connection");
			}
		}
		Properties properties = new Properties();
		properties.setProperty("user", user);
		properties.setProperty("password", password);
		return myDriver.connect(getURL(), properties);
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
	 * @deprecated Use #getConnection() instead.
	 */
	@Deprecated
	public Connection connect()
	throws SQLException {
		return getConnection();
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
	 * @deprecated Use #getConnection(java.lang.String) instead.
	 */
	@Deprecated
	public Connection connect(String password)
	throws SQLException {
		return getConnection(password);
	}

	/**
	 * Return a connection to the database with this {@code DatabaseAlias}
	 * properties and enforced password.
	 *
	 * @param user A user of the database.
	 * @param password A password for database access.
	 * @return A connection to the database.
	 *
	 * @throws SQLException If the connection to the database could not be
	 * established with current data.
	 *
	 * @deprecated Use #getConnection(java.lang.String) instead.
	 */
	@Deprecated
	public Connection connect(String user, String password)
	throws SQLException {
		return getConnection(user, password);
	}

	/////////
	// URL //
	/////////

	protected String myUrl;

	public String getURL() {
		return myUrl;
	}

	public boolean isSameDatabase(String url) {
		return getURL().equals(url);
	}

	public boolean isSameDatabase(DatabaseAlias alias) {
		return getURL().equals(alias.getURL());
	}

	//////////
	// User //
	//////////

	protected String myUser;

	public String getUser() {
		return myUser;
	}

	//////////////
	// Password //
	//////////////

	protected String myPassword = null;

	/**
	 * Returns a password for this database.
	 *
	 * Note, that if the password has not been set then null will be returned
	 * rather empty string.
	 *
	 * @return A password for this database or {@code null} if it is not set.
	 */
	public String getPassword() {
		return myPassword;
	}

	//////////////
	// Overload //
	//////////////

	public boolean equals(Object object) {
		return (object instanceof DatabaseAlias)
				&& isSameDatabase((DatabaseAlias)object)
				&& getUser().equals(((DatabaseAlias)object).getUser());
	}

	public String toString() {
		return "DatabaseAlias [url=" + getURL() + "; user=" + getUser() + "]";
	}

	@Deprecated
	public void addDbArgs(List<String> list) {
		list.add(JDBCDriverManager.getDriverClassNameForURL(myUrl));
		list.add(myUrl);
		list.add(myUser);
		list.add(myPassword);
	}

	/**
	 * Returns if the last test connection has been successful or not.
	 *
	 * @return {@code true} if the last test connection has been successful and
	 * {@code false} otherways.
	 */
	public boolean isValid() {
		return false;
	}

	DriverAlias driverAlias;

	/**
	 * Prints this alias to the specified {@link java.io.OutputStream}.
	 *
	 * @param output An {@link java.io.OutputStream} to which this alias will be
	 * printed.
	 */
	public void printTo(OutputStream output) {
		PrintStream printStream = new PrintStream(output);
		printStream.println("<alias name=\"" + getName() + "\">");
		printStream.println("<database url=\"" + getURL() + "\"/>");
		printStream.println("<user name=\"" + getUser() + "\" password=\"" + getPassword() + "\"/>");
		if (driverAlias != null) {
			driverAlias.printTo(output);
		}
		printStream.println("</alias>");
	}

}
