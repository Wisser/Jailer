package net.sf.jailer.aliases.driver;

import java.io.OutputStream;
import java.io.PrintStream;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

/**
 * Includes all information about one JDBC Driver.
 *
 * Since the driver could accept more than one type of the jdbc subprotocol, there is no jdbc subprotocol field in the
 * {@code DriverAlias}. Whether driver could accept some JDBC URL or not could be checked by
 * {@link net.sf.jailer.aliases.driver.DriverAlias#isAcceptibleURL(String)} method.
 *
 * @author Vladimir "Dair T'arg" Berkutov
 * @date: 06.03.2008
 * @time: 22:42:59
 */
public class DriverAlias {

	public DriverAlias(Collection<String> urls)
	throws SQLException {
		this(urls.toArray(new String[urls.size()]));
	}
	
	public DriverAlias(String urls[])
	throws SQLException {
		libraries = new ArrayList<String>();
		for (int i = 0; i < urls.length; i++) {
//			if (acceptURL(urls[i])) {
				libraries.add(urls[i]);
//			}
		}
	}

	protected String myClassName;
	public String getClassName() {
		return myClassName;
	}

	public boolean isValid() {
		return false;
	}
	
	protected ArrayList<String> libraries;

	public void printTo(OutputStream output) {
		PrintStream printStream = new PrintStream(output);
		printStream.println("<driver class=\"" + getClassName() + "\">");
		printStream.println("<libraries>");
		Iterator<String> librariesIterator = libraries.iterator();
		//noinspection WhileLoopReplaceableByForEach
		while (librariesIterator.hasNext()) {
			printStream.println("<library url=\"" + librariesIterator.next() + "\"/>");
		}
		printStream.println("</libraries>");
		printStream.println("</driver>");
	}

	/**
	 * Alias to {@link net.sf.jailer.aliases.driver.DriverAlias#acceptURL(String)}.
	 * 
	 * Retrieves whether the driver pointed by this {@link net.sf.jailer.aliases.driver.DriverAlias} thinks that it
	 * could accept the specified JDBC {@code url} of a database or not.
	 *
	 * @param url The url of a database.
	 * @return {@code true} if the driver understands the subprotocol and {@code false} otherwise.
	 *
	 * @throws SQLException If a database access error occurs.
	 */
	public boolean isAcceptibleURL(String url)
	throws SQLException {
		return acceptURL(url);
	}

	/**
	 * Retrieves whether the driver pointed by this {@link net.sf.jailer.aliases.driver.DriverAlias} thinks that it
	 * could accept the specified JDBC {@code url} of a database or not.
	 * 
	 * @param url The url of a database.
	 * @return {@code true} if the driver understands the subprotocol and {@code false} otherwise.
	 *
	 * @throws SQLException If a database access error occurs.
	 */
	public boolean acceptURL(String url)
	throws SQLException {
		throw new RuntimeException("Not implemented yet");
	}
}
