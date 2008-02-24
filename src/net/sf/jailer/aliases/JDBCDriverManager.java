package net.sf.jailer.aliases;

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.sql.Connection;
import java.sql.Driver;
import java.sql.DriverPropertyInfo;
import java.sql.SQLException;
import java.util.Properties;

/**
 * This object manages the list of databases drivers. It performs loading of
 * appropriate drivers during connection to a database server.
 *
 * Also, this object registers and unregisters a drivers.
 *
 * @author Vladimir "Dair T'arg" Berkutov
 * @date: 08.02.2008
 * @time: 14:49:29
 */
public final class JDBCDriverManager {

	/**
	 * A default configuration file path.
	 */
	public final static String DEFAULT_DRIVERS_LIST_FILE = "config/drivers.list";

	/**
	 * A default drivers directory.
	 */
	public final static String DEFAULT_DRIVERS_DIRECTORY = "drivers/";

	/**
	 * Initializes the {@code JDBCDriverManager} engine.
	 *
	 * @param configurationFile A configuration xml file with a list of
	 * installed drivers.
	 * @param driversDirectory A directory with drivers.
	 *
	 * @throws RuntimeException If configuration file could not be loaded.
	 */
	public static void initialize(String configurationFile, String driversDirectory)
	throws RuntimeException {
		myConfigurationFile = configurationFile;
		myDriversDirectory = driversDirectory;
		initialized = true;
		try {
			loadDriverList(configurationFile);
		} catch (IOException exception) {
			throw new RuntimeException("Driver list could not be loaded", exception);
		}
	}

	/////////////////////////
	// Initialization flag //
	/////////////////////////

	/**
	 * The flag of the {@code JDBCDriverManager} initialization. If the
	 * {@code JDBCDriverManager} is initialized then this flag is true, otherways
	 * this flag is false.
	 *
	 * @see #isInitialized()
	 */
	private static boolean initialized = false;

	/**
	 * Checks whether {@code JDBCDriverManager} engine is initialized or not.
	 *
	 * @return {@code true} if {@code JDBCDriverManager} is initialized and
	 * {@code false} if {@code JDBCDriverManager} is not initialized.
	 */
	public static boolean isInitialized() {
		return initialized;
	}

	////////////////////////
	// Configuration File //
	////////////////////////

	private static String myConfigurationFile;

	/**
	 * Returns a path to the configuration file with a drivers list.
	 *
	 * @return A path to the configuration file with a drivers list.
	 */
	public static String getConfigurationFile() {
		return myConfigurationFile;
	}

	///////////////////////
	// Drivers Directory //
	///////////////////////

	/**
	 * A path to a directory with drivers libraries.
	 *
	 * @see #getDriversDirectory()
	 */
	private static String myDriversDirectory = null;

	/**
	 * Returns a path to a directory with drivers libraries.
	 *
	 * @return A path to a directory with drivers libraries.
	 */
	public static String getDriversDirectory() {
		if (!initialized) {
			return "";
		}
		if (myDriversDirectory == null) {
			return "";
		}
		return myDriversDirectory;
	}

	//////////////////
	// Drivers List //
	//////////////////

	private final static Properties myDriversList = new Properties();

	private static void loadDriverList(String configurationFile)
	throws IOException {
		myDriversList.load(new FileReader(configurationFile));
	}

	/////////
	// API //
	/////////

	/**
	 * Returns a {@link java.sql.Driver} instance for requested server type.
	 *
	 * @param serverName A database server type.
	 * @return A {@link java.sql.Driver} instance for requested server type.
	 *
	 * @throws RuntimeException If the {@code JDBCDriverManager} engine has not been
	 * initialized or if a driver instance could not be created.
	 * @throws DriverNotFoundException If no {@link java.sql.Driver}s has been
	 * found for the specified serverName.
	 */
	public static Driver getDriver(String serverName)
	throws RuntimeException, DriverNotFoundException {
		if (!initialized) {
			initialize(DEFAULT_DRIVERS_LIST_FILE, DEFAULT_DRIVERS_DIRECTORY);
		}
		if (!myDriversList.containsKey(serverName)) {
			throw new DriverNotFoundException(serverName);
		}
		String fullName = myDriversList.getProperty(serverName);
		String libraryName = myDriversDirectory + getLibraryName(fullName);
		String className = getClassName(fullName);
		try {
			return createDriverInstance(libraryName, className);
		} catch (Exception exception) {
			System.err.println(exception);
			throw new RuntimeException("Could not load database server driver", exception);
		}
	}

	public static Driver getDriverForURL(String url)
	throws DriverNotFoundException {
		return getDriver(getSubprotocol(url));
	}

	private static String getLibraryName(String fullName) {
		return fullName.substring(0, fullName.indexOf('#'));
	}

	private static String getClassName(String fullName) {
		return fullName.substring(fullName.indexOf("#") + 1);
	}

	private static Driver createDriverInstance(String libraryFileName, String className)
	throws MalformedURLException, ClassNotFoundException {
		URL libraryURL = new File(libraryFileName).toURI().toURL();
		// Adding libraryFileName to a system ClassLoader.
		ClassLoader loader = new URLClassLoader(new URL[] {libraryURL},
					ClassLoader.getSystemClassLoader());
		try {
			return new DriverProxy((Driver)loader.loadClass(className).newInstance());
		} catch (Exception exception) {
			System.err.println(exception);
			throw new ClassNotFoundException("Specified class is not an extension of java.sql.Driver", exception);
		}
	}

	/**
	 * Sets a driver for the server type. This method just adds a record to a
	 * drivers list and flushes it to a file.
	 *
	 * @param serverType A type of server - just a key for a driver.
	 * @param libraryName A name of .jar file with a library.
	 * @param className A name of class from a library which implements
	 * {@link java.sql.Driver}.
	 *
	 * @throws RuntimeException If there were some problems with writing drivers
	 * list to a file.
	 */
	public static void setDriver(String serverType, String libraryName, String className)
	throws RuntimeException {
		if (!initialized) {
			initialize(DEFAULT_DRIVERS_LIST_FILE, DEFAULT_DRIVERS_DIRECTORY);
		}
		if (!isDriverExists(serverType)) {
			return;
		}
		myDriversList.setProperty(serverType, libraryName + "#" + className);
		try {
			myDriversList.store(new FileWriter(myConfigurationFile), "");
		} catch (IOException exception) {
			throw new RuntimeException("Driver list could not be written", exception);
		}
	}

	/**
	 * Checks whether driver is set for the specified server type or not.
	 * No library existing or file check is made - only a check for a list
	 * record
	 *
	 * @param serverType A server type.
	 * @return {@code true} If a default driver for the specified server type
	 * is set and {@code false} otherways.
	 */
	public static boolean isDriverExists(String serverType) {
		if (!initialized) {
			initialize(DEFAULT_DRIVERS_LIST_FILE, DEFAULT_DRIVERS_DIRECTORY);
		}
		return myDriversList.containsKey(serverType)
				&& myDriversList.getProperty(serverType).length() != 0;
	}

	public static Connection getConnection(String url, String name, String password)
	throws SQLException, DriverNotFoundException {
		Driver driver = getDriver(getSubprotocol(url));
		Properties properties = new Properties();
		properties.setProperty("user", name);
		properties.setProperty("password", password);
		return driver.connect(url, properties);
	}

	private static String getSubprotocol(String url)
	throws IllegalArgumentException {
		int start = url.indexOf(':');
		if (start == -1) {
			throw new IllegalArgumentException("Invalid url string");
		}
		int finish = url.indexOf(':', start + 1);
		if (finish == -1) {
			throw new IllegalArgumentException("Invalid url string");
		}
		return url.substring(start + 1, finish);
	}

	//////////////////
	// Driver Proxy //
	//////////////////

	/**
	 * A proxy for dynamicly loaded JDBC Driver.
	 *
	 * @author Wisser
	 */
	public static class DriverProxy implements Driver {
		private final Driver driver;
		private DriverProxy(Driver driver) {
			this.driver = driver;
		}
		public boolean acceptsURL(String u) throws SQLException {
			return driver.acceptsURL(u);
		}
		public Connection connect(String u, Properties p) throws SQLException {
			return driver.connect(u, p);
		}
		public int getMajorVersion() {
			return driver.getMajorVersion();
		}
		public int getMinorVersion() {
			return driver.getMinorVersion();
		}
		public DriverPropertyInfo[] getPropertyInfo(String u, Properties p) throws SQLException {
			return driver.getPropertyInfo(u, p);
		}
		public boolean jdbcCompliant() {
			return driver.jdbcCompliant();
		}
	}


}
