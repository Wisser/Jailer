package net.sf.jailer.aliases.database;

import java.io.*;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Vector;

/**
 * @author Vladimir "Dair T'arg" Berkutov
 * @date: 29.02.2008
 * @time: 21:27:25
 */
public final class DatabaseAliasManager {

	private DatabaseAliasManager() {}

	/**
	 * A lias of all aliases.
	 */
	private static Vector<DatabaseAlias> databaseAliases = new Vector<DatabaseAlias>();

	/**
	 * Retunrs a full list of {@link DatabaseAlias}es which stored in
	 * configuration file.
	 *
	 * @return A full list of {@link DatabaseAlias}es which are avaliable.
	 */
	public static DatabaseAlias[] getAllDatabaseAliases() {
		int size = databaseAliases.size();
		return databaseAliases.toArray(new DatabaseAlias[size]);
	}

	/**
	 * Returns a list of the valid {@link DatabaseAlias}es. Valid
	 * {@link DatabaseAlias} is an alias using which the connection with a
	 * database could be established.
	 *
	 * @return A list of the valid {@link DatabaseAlias}es.
	 */
	public static DatabaseAlias[] getActiveDatabaseAliases() {
		Iterator<DatabaseAlias> aliasesIterator = databaseAliases.iterator();
		List<DatabaseAlias> validAliases = new LinkedList<DatabaseAlias>();
		while (aliasesIterator.hasNext()) {
			DatabaseAlias alias = aliasesIterator.next();
			if (alias.isValid()) {
				validAliases.add(alias);
			}
		}
		int size = validAliases.size();
		return validAliases.toArray(new DatabaseAlias[size]);
	}

	/**
	 * Adds the specified {@link DatabaseAlias} to the database aliases list.
	 * If the specified alias is {@code null} then nothing will be added.
	 *
	 * @param alias A {@link DatabaseAlias} to be added to the database aliases
	 * list.
	 * @return {@code true} if the alias has been added (it is not {@code null}
	 * pointer) and {@code false} if the alias has not been added (it is
	 * {@code null} pointer).
	 */
	public static boolean addDatabaseAlias(DatabaseAlias alias) {
		if (alias == null) {
			return false;
		}
		databaseAliases.addElement(alias);
		return true;
	}

	/**
	 * Removes the first occurence of the specified {@link DatabaseAlias}
	 * from the database aliases list.
	 * If the alias is {@code null} then nothing will be removed.
	 *
	 * @param alias A {@link DatabaseAlias} to be removed from the database
	 * aliases list.
	 * @return {@code true} if the alias has been truly removed (at list one
	 * pointer to the specified alias occurs) and {@code false} otherways.
	 * In other words, this method returns whether the database aliases list size
	 * was changed or not.
	 */
	public static boolean removeDatabaseAlias(DatabaseAlias alias) {
		return (alias != null) && databaseAliases.removeElement(alias);
	}

	/**
	 * Removes all {@link DatabaseAlias}es from the database aliases list.
	 *
	 * @return {@code true} if any aliases has really been removed from the list
	 * or not. In other words, this method returns if the size of the database
	 * aliases list has changed after calling this method or not.
	 */
	public static boolean removeAllDatabaseAliases() {
		int size = databaseAliases.size();
		databaseAliases.removeAllElements();
		return size == 0;
	}

	/**
	 * Stores {@link DatabaseAlias}es list to the specified
	 * {@link java.io.File}.
	 *
	 * @param file The {@link java.io.File} to which the
	 * {@link net.sf.jailer.aliases.database.DatabaseAlias}es list will be stored.
	 *
	 * @throws IOException If the list has not been stored properly during some
	 * output problems.
	 */
	public static void store(File file)
	throws IOException {
		FileOutputStream outputStream = new FileOutputStream(file, false);
		store(outputStream);
		outputStream.close();
	}

	/**
	 * Stores {@link DatabaseAlias}es list to the specified
	 * {@link java.io.OutputStream}.
	 *
	 * @param output The {@link java.io.OutputStream} to which the
	 * {@link net.sf.jailer.aliases.database.DatabaseAlias}es list will be stored.
	 */
	public static void store(OutputStream output) {
		PrintStream printStream = new PrintStream(output);
		printStream.println("<aliases>");
		Iterator<DatabaseAlias> aliasesIterator = databaseAliases.iterator();
		//noinspection WhileLoopReplaceableByForEach
		while (aliasesIterator.hasNext()) {
			aliasesIterator.next().printTo(printStream);
		}
		printStream.println("</aliases>");
	}

	/**
	 * Loads the {@link DatabaseAlias}es list from the specified
	 * {@link java.io.File}
	 *
	 * @param file A file with stored
	 * {@link net.sf.jailer.aliases.database.DatabaseAlias}es.
	 *
	 * @throws IOException If the list has not been loaded properly during some
	 * input problems.
	 */
	public static void load(File file)
	throws IOException {
		FileInputStream inputStream = new FileInputStream(file);
		load(inputStream);
		inputStream.close();
	}

	public static void load(InputStream input) {
		/* SAX XML parsing... */
	}

}
