package net.sf.jailer.aliases.driver;

import java.io.OutputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Iterator;

/**
 * @author Vladimir "Dair T'arg" Berkutov
 * @date: 06.03.2008
 * @time: 22:42:59
 */
public class DriverAlias {

	protected String myClassName;
	public String getClassName() {
		return myClassName;
	}

	public boolean isValid() {
		return false;
	}
	
	protected ArrayList<String> myLibraries;

	public void printTo(OutputStream output) {
		PrintStream printStream = new PrintStream(output);
		printStream.println("<driver class=\"" + getClassName() + "\">");
		printStream.println("	</libraries>");
		Iterator<String> librariesIterator = myLibraries.iterator();
		//noinspection WhileLoopReplaceableByForEach
		while (librariesIterator.hasNext()) {
			printStream.println("		<library url=\"" + librariesIterator.next() + "\"/>");
		}
		printStream.println("	</libraries>");
		printStream.println("</driver>");
	}
}
