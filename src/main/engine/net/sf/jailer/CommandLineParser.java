/*
 * Copyright 2007 - 2019 Ralf Wisser.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.sf.jailer;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;

import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;

/**
 * Parser for {@link CommandLine}.
 * 
 * @author Ralf Wisser
 */
public class CommandLineParser {
	
	/**
	 * Parses arguments and initializes the parser.
	 * 
	 * @param args the arguments
	 * @param silent if <code>true</code>, no error messages will be written
	 */
	public static CommandLine parse(String[] args, boolean silent) throws Exception {
		CommandLine commandLine = new CommandLine();
		try {
			List<String> theArgs = new ArrayList<String>();
			
			final String ESC_PREFIX = "((!JAILER_MINUS_ESC!!)";

			int i = 0;
			while (i < args.length) {
				String arg = args[i];
				if ("-".equals(arg) && i < args.length - 1) {
					theArgs.add(ESC_PREFIX + args[++i]);
				} else {
					theArgs.add(arg);
				}
				++i;
			}

			CmdLineParser cmdLineParser = new CmdLineParser(commandLine);
			cmdLineParser.parseArgument(theArgs.toArray(new String[0]));
			List<String> escapedWords = new ArrayList<String>();
			for (String arg: commandLine.arguments) {
				if (arg.startsWith(ESC_PREFIX)) {
					escapedWords.add(arg.substring(ESC_PREFIX.length()));
				} else {
					escapedWords.add(arg);
				}
			}
			commandLine.arguments = escapedWords;
			if (commandLine.arguments.isEmpty()) {
				if (!silent) {
					printUsage(args);
					return null;
				}
			}
			return commandLine;
		} catch (CmdLineException e) {
			System.out.println(e.getMessage());
			printUsage(args);
			throw e;
		}
	}

	/**
	 * Prints out usage.
	 */
	public static void printUsage(String[] args) {
		System.out.println("usage:");
		System.out.println("  jailer export [options] <extraction-model> <jdbc-driver-class> <db-URL> <db-user> <db-password>");
		System.out.println("    extracts data (see option '-e') and optionally creates a delete-script (see option '-d')");
		System.out.println("    -where subject condition. Optional, overrides condition in extraction-model");
		System.out.println("    -format [SQL, XML, DBUNIT_FLAT_XML or LIQUIBASE_XML]");
		System.out.println("    -xml-root root tag of XML export file");
		System.out.println("    -xml-date pattern for dates in XML and LIQUIBASE_XML export file");
		System.out.println("    -xml-time pattern for times in XML and LIQUIBASE_XML export file");
		System.out.println("    -xml-timestamp pattern for time-stamps in XML and LIQUIBASE_XML export file");
		System.out.println("    -t prevents deletion of entities from 'tabu'-tables");
		System.out.println();
		System.out.println("  jailer import <sql-script> <jdbc-driver-class> <db-URL> <db-user> <db-password>");
		System.out.println("    imports data (with C|BLOB support)");
		System.out.println();
		System.out.println("  jailer delete [options] <extraction-model> <jdbc-driver-class> <db-URL> <db-user> <db-password>");
		System.out.println("    Like export, but skips the export and creates a delete-script (see option '-d')");
		System.out.println("    -where subject condition. Optional, overrides condition in extraction-model");
		System.out.println("    -t prevents deletion of entities from 'tabu'-tables");
		System.out.println();
		System.out.println("  jailer create-ddl [-datamodel VAL] [-target-dbms <DBMS>] [-working-table-schema VAL] [<extraction-model> -independent-working-tables] [-no-rowid]");
		System.out.println("    creates the DDL for the working-tables and prints it to stdout");
		System.out.println();
		System.out.println("  jailer create-ddl <jdbc-driver-class> <db-URL> <db-user> <db-password> [<extraction-model> -independent-working-tables] [-no-rowid]");
		System.out.println("    creates the DDL for the working-tables and executes it");
		System.out.println();
		System.out.println("  jailer build-model [-schema <schema>] <jdbc-driver-class> <db-URL> <db-user> <db-password>");
		System.out.println("    automatically retrieves datamodel elements using the 'model-finder' beans");
		System.out.println("    reduces JDBC-Introspection to schema <schema>");
		System.out.println();
		System.out.println("  jailer print-datamodel [options] {<restriction-model>}*");
		System.out.println("    prints restricted data-model");
		System.out.println("    -c with closures ");
		System.out.println();
		System.out.println("  jailer render-datamodel [options] {<restriction-model>}* ");
		System.out.println("    generates a HTML render of the restricted data-model into directory 'render'");
		System.out.println();
		System.out.println("  jailer find-association [options] <source-table> <destination-table> {<restriction-model>}*");
		System.out.println("    finds the shortest path of associations between two tables");
		System.out.println("    -u considers associations as un-directed");
		System.out.println();
		System.out.println("options:");
		CmdLineParser cmdLineParser = new CmdLineParser(new CommandLine());
		cmdLineParser.setUsageWidth(120);
		cmdLineParser.printUsage(System.out);
		System.out.println();
		printAruments(System.out, args, null);
	}

	public static void printAruments(PrintStream out, String[] args, String password) {
		if (args.length > 0) {
			out.println();
			out.print("Arguments: ");
			int i = 0;
			while (i < args.length) {
				String arg = args[i];
				if (arg.equals(password)) {
					arg = "?";
				}
				if (i > 0) {
					out.print(", ");
				}
				out.print(" " + i + ": {" + arg + "}");
				++i;
			}
			out.println();
			out.println();
		}
	}

}
