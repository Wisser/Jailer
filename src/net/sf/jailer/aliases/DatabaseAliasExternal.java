package net.sf.jailer.aliases;

/**
 * @author Vladimir "Dair T'arg" Berkutov
 * @date: 01.03.2008
 * @time: 21:59:05
 */
public class DatabaseAliasExternal extends DatabaseAlias {

	public DatabaseAliasExternal(String url, String user, String password, String jars, String className)
	throws DriverNotFoundException {
		super(url, user, password, JDBCDriverManager.getDriverForURL(url));
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

}
