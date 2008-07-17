/*
 * Copyright 2007 the original author or authors.
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

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.SqlUtil;

import org.kohsuke.args4j.Argument;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;

/**
 * Commandline-parser for {@link Jailer}.
 * 
 * @author Ralf Wisser
 */
public class CommandLineParser {

    /**
     * The singleton.
     */
    private static CommandLineParser commandLineParser = null;
    
    /**
     * Gets the singleton.
     * 
     * @return the singleton
     */
    public static CommandLineParser getInstance() {
        return commandLineParser;
    }
    
    /**
     * Parses arguments and initializes the parser.
     * 
     * @param args the arguments
     */
    public static void parse(String[] args) throws Exception {
        commandLineParser = new CommandLineParser();
        cmdLineParser = null;
        try {
            cmdLineParser = new CmdLineParser(commandLineParser);
            cmdLineParser.parseArgument(args);
            if (commandLineParser.arguments.isEmpty()) {
                printUsage();
                System.exit(0);
            }
            if (commandLineParser.tabuFileName != null) {
                commandLineParser.loadTabuTables(commandLineParser.tabuFileName);
            }
        } catch( CmdLineException e ) {
            System.out.println(e.getMessage());
            printUsage();
            System.exit(0);
        }
    }

    /**
     * Prints out usage.
     */
    public static void printUsage() {
        System.out.println("usage:");
        System.out.println("  jailer print-datamodel [options] {<restriction-model>}*");
        System.out.println("    prints restricted data-model");
        System.out.println("    -c with closures ");
        System.out.println("    -t excludes 'tabu'-tables from component-analysis");
        System.out.println();
        System.out.println("  jailer render-datamodel [options] {<restriction-model>}* ");
        System.out.println("    generates a HTML render of the restricted data-model into directory 'render'");
        System.out.println();
        System.out.println("  jailer find-association [options] <source-table> <destination-table> {<restriction-model>}*");
        System.out.println("    finds the shortest path of associations between two tables");
        System.out.println("    -u considers associations as un-directed");
        System.out.println("    -t ignores 'tabu'-tables");
        System.out.println();
        System.out.println("  jailer export [options] <extraction-model> <jdbc-driver-class> <db-URL> <db-user> <db-password>");
        System.out.println("    extracts data (see option '-e') and optionally creates a delete-script (see option '-d')");
        System.out.println("    -where subject condition. Optional, overrides condition in extraction-model");
        System.out.println("    -xml export entities into XML file (instead of SQL)");
        System.out.println("    -xml-root root tag of XML export file");
        System.out.println("    -xml-date pattern for dates in XML export file");
        System.out.println("    -xml-timestamp pattern for time-stamps in XML export file");
        System.out.println("    -t prevents deletion of entities from 'tabu'-tables");
        System.out.println();
        System.out.println("  jailer create-ddl");
        System.out.println("    creates the DDL for the working-tables");
        System.out.println();
        System.out.println("  jailer build-model [-schema <schema>] <jdbc-driver-class> <db-URL> <db-user> <db-password>");
        System.out.println("    automatically retrieves datamodel elements using the 'model-finder' beans");
        System.out.println("    reduces JDBC-Introspection to schema <schema>");
        System.out.println();
        System.out.println("  jailer check-domainmodel [options] {<restriction-model>}*");
        System.out.println("    checks the domain model (directory 'domainmodel')");
        System.out.println();
        System.out.println("  jailer import <sql-script> <jdbc-driver-class> <db-URL> <db-user> <db-password>");
        System.out.println("    imports data (with C|BLOB support)");
        System.out.println();
        System.out.println("options:");
        cmdLineParser.setUsageWidth(120);
        cmdLineParser.printUsage(System.out);
        System.out.println();
    }

    /**
     * Gets 'tabu' tables.
     * 
     * @param dataModel to get tables from
     * @return set of 'tabu' tables or <code>null</code>, empty list if no tabu-file is given
     */
    public Set<Table> getTabuTables(DataModel dataModel) {
        return SqlUtil.readTableList(tabuTableNames, dataModel);
    }
    
    /**
     * Loads 'tabu' tables file.
     */
    private void loadTabuTables(String fileName) throws Exception {
        File file = new File(fileName);
        tabuTableNames = new CsvFile(file);
    }

    private Map<String, String> schemaMapping;

    public Map<String, String> getSchemaMapping() {
		if (schemaMapping == null) {
			schemaMapping = new HashMap<String, String>();
			if (rawschemamapping != null) {
				for (String item: rawschemamapping.split(",")) {
					String[] fromTo = (" " + item + " ").split("=");
					if (fromTo.length == 2) {
						schemaMapping.put(fromTo[0].trim(), fromTo[1].trim());
					}
				}
			}
		}
		return schemaMapping;
	}
    
    @Option(name="-xml",usage="export entities into XML file (instead of SQL)")
    public boolean asXml = false;
    
    @Option(name="-xml-root",usage="root tag of XML export file",metaVar="tag-name")
    public String xmlRootTag = "entities";

    @Option(name="-xml-date",usage="pattern for dates in XML export file",metaVar="pattern")
    public String xmlDatePattern = "yyyy-MM-dd";

    @Option(name="-xml-timestamp",usage="pattern for time-stamps in XML export file",metaVar="pattern")
    public String xmlTimeStampPattern = "yyyy-MM-dd-HH.mm.ss";
    
    @Option(name="-c",usage="print restricted data-model with closures")
    public boolean withClosures = false;
    
    @Option(name="-explain",usage="write export-explanation ('explain.log')")
    public boolean explain = false;

    @Option(name="-u",usage="consider associations as un-directed")
    public boolean undirected = false;
    
    @Option(name="-m",usage="abort export if number of entities is greater than this limit", metaVar="max-number-of-entities")
    public int maxNumberOfEntities = 0;
    
    @Option(name="-e",usage="name of the export-script file (compressed if it ends with '.zip' or '.gz')", metaVar="export-script")
    public String exportScriptFileName = null;

    @Option(name="-schema",usage="schema to reduce JDBC-Introspection to", metaVar="export-script")
    public String schema = null;

    @Option(name="-qualifyNames",usage="add schema prefix to table names after DB-introspection", metaVar="export-script")
    public boolean qualifyNames = false;

    @Option(name="-d",usage="name of the delete-script file (compressed if it ends with '.zip' or '.gz')", metaVar="delete-script")
    public String deleteScriptFileName = null;

    @Option(name="-t",usage="name of the 'tabu' file (for 'print-datamodel or 'find-association')", metaVar="'tabu'-file")
    public String tabuFileName = null;

    @Option(name="-where",usage="subject condition", metaVar="SQL-expression")
    public String where = null;
    
    @Option(name="-schemamapping",usage="schema map", metaVar="<from>=<to>[','<from>=<to>]*")
    public String rawschemamapping = null;
    
    @Option(name="-threads",usage="number of threads (default is 10)", metaVar="#threads")
    public int numberOfThreads = 10;
    
    @Option(name="-entities",usage="maximum number of entities per insert-statement (in export-file, default is 10)", metaVar="#entities")
    public int numberOfEntities = 10;
    
    @Option(name="-upsert-only",usage="generate 'upsert'-statements for all entities (in export-file)")
    public boolean upsertOnly = false;
    
    @Argument
    public List<String> arguments = new ArrayList<String>();

    /**
     * Names of 'tabu' tables.
     */
    private CsvFile tabuTableNames = null;
    
    /**
     * The singleton.
     */
    private static CmdLineParser cmdLineParser;
}