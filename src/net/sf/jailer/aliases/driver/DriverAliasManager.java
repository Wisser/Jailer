package net.sf.jailer.aliases.driver;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;

import java.io.*;
import java.util.Iterator;
import java.util.Properties;
import java.util.Vector;

/**
 * @author Vladimir "Dair T'arg" Berkutov
 * @date: 07.03.2008
 * @time: 21:48:19
 */
public final class DriverAliasManager {

	private DriverAliasManager() {}

	/**
	 * A lias of all aliases.
	 */
	private static Vector<DriverAlias> driverAliases = new Vector<DriverAlias>();

	/**
	 * Retunrs a full list of {@link DriverAlias}es which stored in
	 * configuration file.
	 *
	 * @return A full list of {@link DriverAlias}es which are avaliable.
	 */
	public static DriverAlias[] getAllDriverAliases() {
		int size = driverAliases.size();
		return driverAliases.toArray(new DriverAlias[size]);
	}
	
	/**
	 * Adds the specified {@link DriverAlias} to the driver aliases list.
	 * If the specified alias is {@code null} then nothing will be added.
	 *
	 * @param alias A {@link DriverAlias} to be added to the driver aliases
	 * list.
	 * @return {@code true} if the alias has been added (it is not {@code null}
	 * pointer) and {@code false} if the alias has not been added (it is
	 * {@code null} pointer).
	 */
	public static boolean addDriverAlias(DriverAlias alias) {
		if (alias == null) {
			return false;
		}
		driverAliases.addElement(alias);
		return true;
	}

	/**
	 * Removes the first occurence of the specified {@link DriverAlias}
	 * from the driver aliases list.
	 * If the alias is {@code null} then nothing will be removed.
	 *
	 * @param alias A {@link DriverAlias} to be removed from the driver
	 * aliases list.
	 * @return {@code true} if the alias has been truly removed (at list one
	 * pointer to the specified alias occurs) and {@code false} otherways.
	 * In other words, this method returns whether the driver aliases list size
	 * was changed or not.
	 */
	public static boolean removeDriverAlias(DriverAlias alias) {
		return (alias != null) && driverAliases.removeElement(alias);
	}

	/**
	 * Removes all {@link DriverAlias}es from the driver aliases list.
	 *
	 * @return {@code true} if any aliases has really been removed from the list
	 * or not. In other words, this method returns if the size of the driver
	 * aliases list has changed after calling this method or not.
	 */
	public static boolean removeAllDriverAliases() {
		int size = driverAliases.size();
		driverAliases.removeAllElements();
		return size == 0;
	}

	/**
	 * Stores {@link DriverAlias}es list to the specified
	 * {@link java.io.File}.
	 *
	 * @param file The {@link java.io.File} to which the
	 * {@link net.sf.jailer.aliases.driver.DriverAlias}es list will be stored.
	 *
	 * @throws java.io.IOException If the list has not been stored properly during some
	 * output problems.
	 */
	public static void store(File file)
	throws IOException {
		FileOutputStream outputStream = new FileOutputStream(file, false);
		store(outputStream);
		outputStream.close();
	}

	/**
	 * Stores {@link DriverAlias}es list to the specified
	 * {@link java.io.OutputStream}.
	 *
	 * @param output The {@link java.io.OutputStream} to which the
	 * {@link net.sf.jailer.aliases.driver.DriverAlias}es list will be stored.
	 */
	public static void store(OutputStream output) {
		PrintStream printStream = new PrintStream(output);
		printStream.println("<aliases>");
		Iterator<DriverAlias> aliasesIterator = driverAliases.iterator();
		//noinspection WhileLoopReplaceableByForEach
		while (aliasesIterator.hasNext()) {
			aliasesIterator.next().printTo(printStream);
		}
		printStream.println("</aliases>");
	}

	/**
	 * Loads the {@link DriverAlias}es list from the specified
	 * {@link java.io.File}
	 *
	 * @param file A file with stored
	 * {@link net.sf.jailer.aliases.driver.DriverAlias}es.
	 *
	 * @throws IOException If the list has not been loaded properly during some
	 * input problems.
	 */
	public static void load(File file)
	throws IOException, SAXException {
		FileInputStream inputStream = new FileInputStream(file);
		load(inputStream);
		inputStream.close();
	}

	/**
	 * Loads {@link net.sf.jailer.aliases.driver.DriverAlias}es list from the specified input stream.
	 * The {@link net.sf.jailer.aliases.driver.DriverAlias}es list is stored as XML.
	 *
	 * @param input An {@link java.io.InputStream} from which the {@link net.sf.jailer.aliases.driver.DriverAlias}es
	 * list will be loaded.
	 * @throws IOException
	 * @throws SAXException If there is no default SAX handler found.
	 */
	public static void load(InputStream input)
	throws IOException, SAXException {
		XMLReader reader = XMLReaderFactory.createXMLReader();
		DriverAliasSAXHandler handler = new DriverAliasSAXHandler();
		reader.setContentHandler(handler);
		reader.parse(new InputSource(input));
		/* SAX XML parsing... */
	}

	/////////////////////////////
	// DriverAliasSAXHandler //
	/////////////////////////////

	private static final class DriverAliasSAXHandler extends DefaultHandler {
		public DriverAliasSAXHandler() {}

		private boolean loaded = false;

		/**
		 * Returns whether or not the {@link net.sf.jailer.aliases.driver.DriverAlias}es list is loaded.
		 *
		 * @return {@code true} if the {@link net.sf.jailer.aliases.driver.DriverAlias}es list is loaded and
		 * {@code false} otherwise.
		 */
		public boolean isLoaded() {
			return loaded;
		}

		private Vector<DriverAlias> loadedAliases;

		private static DriverAlias buildAlias(Properties properties) {
			return null;
		}

		private Properties properties;

		///////////////
		// Listeners //
		///////////////

		public void startDocument()
		throws SAXException {
			if (isLoaded()) {
				return;
			}
			loadedAliases = new Vector<DriverAlias>();
		}

		public void startElement (String uri, String localName, String qName, Attributes attributes)
		throws SAXException {
			if (isLoaded()) {
				return;
			}
			if (localName.equals("alias")) {
				properties = new Properties();
			}
		}

		public void endElement (String uri, String localName, String qName)
		throws SAXException {
			if (isLoaded()) {
				return;
			}
			if (localName.equals("aliases")) {
				loaded = true;
			} else if (localName.equals("alias")) {
				
			}
		}
	}
}
