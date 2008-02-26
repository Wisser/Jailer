package net.sf.jailer.aliases;

import java.sql.Connection;
import java.sql.Driver;
import java.sql.SQLException;
import java.util.Properties;

/**
 * @author Vladimir "Dair T'arg" Berkutov
 * @date: 12.02.2008
 * @time: 0:05:42
 */
public class DatabaseAlias {

	public DatabaseAlias(String url, String user)
	throws DriverNotFoundException {
		myUrl = url;
		myUser = user;
		myDriver = JDBCDriverManager.getDriverForURL(url);
	}

	/**
	 * Constructs a DatabaseAlias for the specified database at {@code url}
	 * which could be accessed with {@code user} name and {@code password}.
	 *
	 * @param url A database url ({@code jdbc:mysql://localhost:3306/} for
	 * example).
	 * @param user A database user.
	 * @param password A password for the {@code user}.
	 * @throws DriverNotFoundException If no {@link java.sql.Driver}s for the
	 * specified subprotocol has been found.
	 * @throws SQLException If the error occured during the test connection.
	 */
	public DatabaseAlias(String url, String user, String password)
	throws DriverNotFoundException, SQLException {
		this(url, user);
		myPassword = password;
		Connection connection = getConnection();
		connection.close();
	}

	////////////
	// Driver //
	////////////

	private Driver myDriver;

	public Connection getConnection()
	throws SQLException, NullPointerException {
		if (myPassword == null) {
			throw new NullPointerException("Paranoya mode is on - DatabaseAlias contains no password");
		}
		return getConnection(myPassword);
	}

	/* Use in paranoya mode - no password is stored. */
	public Connection getConnection(String password)
	throws SQLException {
		Properties properties = new Properties();
		properties.setProperty("user", getUser());
		properties.setProperty("password", password);
		return myDriver.connect(getURL(), properties);
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

}
