package net.sf.jailer.drivermanager;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.List;

/**
 * Manages {@link DatabaseAlias}es
 * <ul>
 *   <li>Persistency
 *   <li>Establishment of database connections
 * </ul>
 */
public final class DatabaseAliasManager {
	
	/**
	 * Registers a {@link DatabaseAlias} under a given name.
	 * Removes any alias previously registered under same name.
	 * 
	 * @param name the alias name
	 * @param alias the alias to register
	 */
	public static void registerAlias(String name, DatabaseAlias alias) {
//		...
	}

	/**
	 * Gets a registered {@link DatabaseAlias}.
	 * 
	 * @param name the name under which the alias has been registered
	 * @return the alias or null, if no alias has been registered under given name
	 */
	public static DatabaseAlias getAlias(String name) {
//		...
		return null;
	}

	/**
	 * Gets the names of all registered {@link DatabaseAlias}es.
	 * 
	 * @return a list of the names of all registered {@link DatabaseAlias}es
	 */
	public static List<String> getAliases() {
//		...
		return null;
	}

	/**
	 * Removes a {@link DatabaseAlias}.
	 * 
	 * @param name the name under which the alias to be removed has been registered
	 */
	public static void removeAlias(String name) {
//		...
	}

	/**
	 * Connect to a database schema.
	 * 
	 * @param alias alias of database schema
	 * @param password password for authentification
	 * @return a connection to the database
	 * 
	 * @throws SQLException if connection cannot be established
	 */
	public static Connection connect(DatabaseAlias alias, String password) throws SQLException {
//		...
		return null;
	}
	
}
