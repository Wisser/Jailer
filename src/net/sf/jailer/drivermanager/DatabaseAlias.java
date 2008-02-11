package net.sf.jailer.drivermanager;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Identifies a database schema.
 * 
 * @author Wisser
 */
public class DatabaseAlias {

	/**
	 * Host name (DB URL).
	 */
	public final String host;

	/**
	 * Database user.
	 */
	public final String user;

	/**
	 * List of driver-jars.
	 */
	public final List<String> driverJars;
	
	/**
	 * Constructor.
	 * 
	 * @param host host name (DB URL)
	 * @param user database user
	 * @param driverJars list of driver-jars. null if no jars are needed.
	 */
	public DatabaseAlias(String host, String user, List<String> driverJars) {
		this.host = host;
		this.user = user;
		this.driverJars = driverJars;
	}
	
	/**
	 * Internalizes a database alias.
	 * 
	 * @param externalizedAlias externalized alias, see {{@link #externalize(DatabaseAlias)} 
	 * @return the alias
	 */
	public static DatabaseAlias internalize(String externalizedAlias) {
		String[] splits = externalizedAlias.split("#");
		List<String> driverJars = new ArrayList<String>();
		driverJars.addAll(Arrays.asList(splits[0].split(":")));
		return new DatabaseAlias(splits[1], splits[2], driverJars);
	}

	/**
	 * Externalizes the database alias.
	 * 
	 * @return externalized form of alias
	 */
	public String externalize() {
		StringBuffer result = new StringBuffer();
		for (String jar: driverJars) {
			if (result.length() > 0) {
				result.append(":");
			}
			result.append(jar);
		}
		result.append("#" + host + "#" + user);
		return result.toString();
	}
	
}
