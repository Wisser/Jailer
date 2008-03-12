package net.sf.jailer.aliases.parser;

import net.sf.jailer.aliases.database.DatabaseAlias;

/**
 * @author Vladimir "Dair T'arg" Berkutov
 * @date: 06.03.2008
 * @time: 19:27:38
 */
public class DatabaseAliasBuilder {

	/////////////////
	// Constructor //
	/////////////////

	/**
	 * Constructs an instance of the {@code DatabaseAliasBuilder}.
	 */
	public DatabaseAliasBuilder() {
		this(null);
	}

	public DatabaseAliasBuilder(String name) {
		setName(name);
	}

	///////////////////
	// DatabaseAlias //
	///////////////////

	/**
	 * Returns a builded {@link DatabaseAlias}.
	 *
	 * @return Builded {@link DatabaseAlias} or the {@code null} if the alias
	 * could no be built.
	 */
	public DatabaseAlias getDatabaseAlias() {
		if (myName == null) {
			return null;
		}
		if (myURL == null) {
			return null;
		}
		if (myUser == null) {
			return null;
		}
		return new DatabaseAlias(getName(), getURL(), getUser(), getPassword());
	}

	//////////
	// Name //
	//////////

	/**
	 * The name of the alias.
	 *
	 * @see #getName()
	 * @see #setName(String)
	 */
	protected String myName;

	/**
	 * Returns the alias name.
	 *
	 * @return A name of the alias.
	 *
	 * @see #setName(String)
	 */
	public String getName() {
		return myName;
	}

	/**
	 * Sets the alias name.
	 *
	 * @param name A new name of the alias.
	 *
	 * @see #getName()
	 */
	public void setName(String name) {
		myName = name;
	}

	//////////////
	// JDBC URL //
	//////////////

	/**
	 * The url of the database.
	 *
	 * @see #getURL()
	 * @see #setURL(String)
	 */
	protected String myURL;

	/**
	 * Returns an url.
	 *
	 * @return An url.
	 *
	 * @see #setURL(String)
	 */
	public String getURL() {
		return myURL;
	}

	/**
	 * Sets an url.
	 *
	 * @param url An url.
	 *
	 * @see #getURL()
	 */
	public void setURL(String url) {
		myURL = url;
	}

	//////////
	// User //
	//////////

	/**
	 * The name of the user who will access database through building
	 * {@link net.sf.jailer.aliases.database.DatabaseAlias}
	 *
	 * @see #getUser()
	 * @see #setUser(String)
	 */
	protected String myUser;

	/**
	 * Returns the username.
	 *
	 * @return A username.
	 *
	 * @see #setUser(String)
	 */
	public String getUser() {
		return myUser;
	}

	/**
	 * Sets a username.
	 *
	 * @param user A new username.
	 *
	 * @see #getUser()
	 */
	public void setUser(String user) {
		myUser = user;
	}

	//////////////
	// Password //
	//////////////

	/**
	 * The password for the user who will access database through building
	 * {@link net.sf.jailer.aliases.database.DatabaseAlias}
	 *
	 * @see #getPassword()
	 * @see #setPassword(String)
	 */
	protected String myPassword;

	/**
	 * Returns the password.
	 *
	 * @return A password.
	 *
	 * @see #setPassword(String)
	 */
	public String getPassword() {
		return myPassword;
	}

	/**
	 * Sets a password.
	 *
	 * @param user A new password.
	 *
	 * @see #getPassword()
	 */
	public void setPassword(String user) {
		myPassword = user;
	}

}
