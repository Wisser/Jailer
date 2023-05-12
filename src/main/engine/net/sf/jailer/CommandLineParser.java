/*
 * Copyright 2007 - 2023 Ralf Wisser.
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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

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
	 * @param cliArgs the arguments
	 * @param silent if <code>true</code>, no error messages will be written
	 */
	public static CommandLine parse(String[] cliArgs, boolean silent) throws Exception {
		CommandLine commandLine = new CommandLine();
		try {
			String[] args = preprocessFileLookup(cliArgs);
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
			printUsage(cliArgs);
			throw e;
		}
	}

	public static String[] preprocessFileLookup(String[] cArgs) throws IOException {
		List<String> result = new ArrayList<String>();
		for (int i = 0; i < cArgs.length; ++i) {
			if ("-file-lookup".equals(cArgs[i]) && i < cArgs.length - 1) {
				BufferedReader in = new BufferedReader(new FileReader(new File(cArgs[i + 1])));
				String line = in.readLine();
				in.close();
				if (line == null) {
					throw new RuntimeException("File \"" + cArgs[i + 1] + "\" is empty");
				}
				if (i > 0 && !"-".equals(cArgs[i - 1])) {
					result.add("-");
				}
				result.add(line);
				++i;
			} else {
				result.add(cArgs[i]);
			}
		}
		return result.toArray(new String[0]);
	}

	/**
	 * Prints out usage.
	 */
	public static void printUsage(String[] args) {
		String cmd = "sh jailer.sh";
		if (System.getProperty("os.name", "").toLowerCase(Locale.ENGLISH).startsWith("windows")) {
			cmd = "jailer.bat";
		}
		System.out.println("usage:");
		System.out.println("  " + cmd + " export [options] <extraction-model> <jdbc-driver-class> <db-URL> <db-user> <db-password> -jdbcjar <JDBC driver jar file>");
		System.out.println("    extracts data (see option '-e') and optionally creates a delete-script (see option '-d')");
		System.out.println("    -where subject condition. Optional, overrides condition in extraction-model");
		System.out.println("    -format [SQL, XML, DBUNIT_FLAT_XML or LIQUIBASE_XML]");
		System.out.println("    -xml-root <root tag of XML export file>");
		System.out.println("    -xml-date <pattern for dates in XML export file>");
		System.out.println("    -xml-time <pattern for times in XML  file>");
		System.out.println("    -xml-timestamp <pattern for time-stamps in XML export file>");
		System.out.println();
		System.out.println("  " + cmd + " import <sql-script> <jdbc-driver-class> <db-URL> <db-user> <db-password> -jdbcjar <JDBC driver jar file>");
		System.out.println("    imports data (with CLOB/BLOB/XML support)");
		System.out.println();
		System.out.println("  " + cmd + " delete [options] <extraction-model> <jdbc-driver-class> <db-URL> <db-user> <db-password> -jdbcjar <JDBC driver jar file>");
		System.out.println("    Like export, but skips the export and creates a delete-script (see option '-d')");
		System.out.println("    -where <subject condition>: optional, overrides condition in extraction-model");
		System.out.println("    -jdbcjar <JDBC driver jar file>");
		System.out.println();
		System.out.println("  " + cmd + " create-ddl [-datamodel VAL] [-target-dbms <DBMS>] [-working-table-schema VAL] [<extraction-model> -independent-working-tables] [-use-rowid] [-use-rowid-if-needed]");
		System.out.println("    creates the DDL for the working-tables and prints it to stdout");
		System.out.println();
		System.out.println("  " + cmd + " create-ddl <jdbc-driver-class> <db-URL> <db-user> <db-password> -jdbcjar <JDBC driver jar file> [<extraction-model> -independent-working-tables] [-use-rowid] [-use-rowid-if-needed]");
		System.out.println("    creates the working-tables");
		System.out.println();
		System.out.println("  " + cmd + " build-model [-schema <schema>] <jdbc-driver-class> <db-URL> <db-user> <db-password> -jdbcjar <JDBC driver jar file>");
		System.out.println("    determines table and relationship information through database analysis");
		System.out.println("    -datamodel <folder>: folder holding the data model. Defaults to './datamodel'");
		System.out.println("    -schema <schema>: limits analysis to the schema <schema>");
		System.out.println();
		System.out.println("  " + cmd + " render-datamodel [-datamodel VAL] [<extraction-model>] ");
		System.out.println("    generates a HTML render of the (restricted) data model into directory 'render'");
		System.out.println();
		System.out.println("  " + cmd + " print-closure <extraction-model> [<separator>] [-datamodel VAL]");
		System.out.println("    prints a list of all tables that are directly or transitively associated with a subject table,");
		System.out.println("    taking into account the restrictions on the associations (the so-called \"Closure\")");
		System.out.println("    <separator>: optional separator between table names in the output");
		System.out.println();
		System.out.println("options:");
		CmdLineParser cmdLineParser = new CmdLineParser(new CommandLine());
		cmdLineParser.setUsageWidth(160);
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
					arg = "<password> (not shown)";
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
