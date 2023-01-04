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
package net.sf.jailer.ui.commandline;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.kohsuke.args4j.CmdLineParser;

import net.sf.jailer.CommandLineParser;
import net.sf.jailer.ui.UIUtil;

/**
 * Parser for {@link UICommandLine}.
 * 
 * @author Ralf Wisser
 */
public class UICommandLineParser {
	
	/**
	 * Parses arguments and initializes the parser.
	 * 
	 * @param args the arguments
	 * @param silent if <code>true</code>, no error messages will be written
	 */
	public static UICommandLine parse(String[] args, boolean silent) throws Exception {
		UICommandLine commandLine = new UICommandLine();
		try {
			args = CommandLineParser.preprocessFileLookup(args);
			List<String> theArgs = new ArrayList<String>();
			StringBuilder allArgs = new StringBuilder(UIUtil.LINE_SEPARATOR + "Arguments: ");
			
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
			if (args.length > 0) {
				i = 0;
				while (i < args.length) {
					String arg = args[i];
					if (i > 0) {
						allArgs.append(", ");
					}
					allArgs.append(" " + i + ": {" + arg + "}");
					++i;
				}
				allArgs.append(UIUtil.LINE_SEPARATOR);
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
			if (commandLine.arguments.size() > 1) {
				throw new RuntimeException("Illegal arguments " + commandLine.arguments + allArgs);
			}
			if (commandLine.arguments.size() == 1 && !commandLine.arguments.get(0).toLowerCase(Locale.ENGLISH).endsWith(".jm")) {
				throw new RuntimeException("'" + commandLine.arguments.get(0) + "' is not a valid extraction model file." + allArgs);
			}
			if (commandLine.datamodelFolder == null && commandLine.arguments.isEmpty()) {
				commandLine.url = null;
			}
			return commandLine;
		} catch (Exception e) {
			ByteArrayOutputStream out = new ByteArrayOutputStream();
			PrintStream ps = new PrintStream(out);
			printUsage(ps);
			ps.close();
			throw new RuntimeException(e.getMessage() + "\n" + out.toString());
		}
	}

	/**
	 * Prints out usage.
	 */
	public static void printUsage(PrintStream out) {
		String cmd = "sh jailerGUI.sh";
		if (System.getProperty("os.name", "").toLowerCase(Locale.ENGLISH).startsWith("windows")) {
			cmd = "jailerGUI.bat";
		}
		out.println("usage:");
		out.println("  " + cmd + " <extraction model (*.jm)> [-datamodel <data model>] [-jdbcjar <jdbc-jar>] [-driver driver class name] [-url <jdbc-url>] [-user <db-user>] [-password <db-password>]");
		out.println("    Starts the Extraction Model Editor with default database connection and/or data model.\n" + 
						   "    Loads the possibly specified extraction model.");
		out.println();
		out.println("  " + cmd + " -datamodel <data model> [-jdbcjar <jdbc-jar>] [-driver driver class name] [-url <jdbc-url>] [-user <db-user>] [-password <db-password>] [-schemamapping <mapping>] [-bookmark <name>]");
		out.println("    Starts the Data Browser with default database connection and/or data model.\n");
		out.println("    Opens a bookmark (If the -bookmark option is specified).\n");
		out.println();
		out.println("options:");
		CmdLineParser cmdLineParser = new CmdLineParser(new UICommandLine());
		cmdLineParser.setUsageWidth(120);
		cmdLineParser.printUsage(out);
		out.println();
	}

}
