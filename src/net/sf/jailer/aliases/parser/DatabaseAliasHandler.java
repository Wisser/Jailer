package net.sf.jailer.aliases.parser;

import net.sf.jailer.aliases.database.DatabaseAlias;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import java.util.LinkedList;

/**
 * @author Vladimir "Dair T'arg" Berkutov
 * @date: 06.03.2008
 * @time: 19:09:42
 */
public class DatabaseAliasHandler extends DefaultHandler {

	/**
	 * Constructs {@code DatabaseAliasHandler}.
	 */
	public DatabaseAliasHandler() {
		super();
	}

	public void startDocument()
	throws SAXException {

	}

    public void endDocument()
	throws SAXException {
	}

	public void startElement(String uri, String localName, String qName, Attributes attributes)
	throws SAXException {
		if (localName.equals("aliases")) {
			return;
		}
		if (localName.equals("alias")) {
			onAliasStarts(attributes.getValue(uri, "name"));
			return;
		}
		if (localName.equals("database")) {
			onDatabase(attributes.getValue(uri, "url"));
			return;
		}
		if (localName.equals("user")) {
			onUser(attributes.getValue(uri, "name"), attributes.getValue(uri, "password"));
		}
	}

    public void endElement(String uri, String localName, String qName)
	throws SAXException {
    }

	/////////////
	// Aliases //
	/////////////

	protected LinkedList<DatabaseAlias> aliases = new LinkedList<DatabaseAlias>();

	public DatabaseAlias[] getAliases() {
		if (myBuildingAlias == null) {
			return null;
		}
		return aliases.toArray(new DatabaseAlias[aliases.size()]);
	}

	///////////////
	// Listening //
	///////////////

	/**
	 * A currently building alias.
	 */
	protected DatabaseAliasBuilder myBuildingAlias = null;

	/**
	 * Flushes the currently building alias to the list of loaded aliases.
	 */
	public void flushBuilder() {
		if (myBuildingAlias == null) {
			return;
		}
		DatabaseAlias alias = myBuildingAlias.getDatabaseAlias();
		myBuildingAlias = null;
		if (alias == null) {
			return;
		}
		aliases.push(alias);
	}

	public void onAliasStarts(String name) {
		flushBuilder();
		myBuildingAlias = new DatabaseAliasBuilder(name);
	}

	public void onAliasEnds() {
		flushBuilder();
	}

	public void onDatabase(String url) {
		myBuildingAlias.setURL(url);
	}

	public void onUser(String user, String password) {
		myBuildingAlias.setUser(user);
		myBuildingAlias.setPassword(password);
	}


}
