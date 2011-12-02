/*
 * Copyright 2007 - 2012 the original author or authors.
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
import java.util.TreeMap;

import net.sf.jailer.database.TemporaryTableScope;
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
     * The singleton (per thread).
     */
    private static InheritableThreadLocal<CommandLineParser> commandLineParser = new InheritableThreadLocal<CommandLineParser>();
    
    /**
     * Gets the singleton.
     * 
     * @return the singleton
     */
    public static CommandLineParser getInstance() {
        return commandLineParser.get();
    }
    
    /**
     * Parses arguments and initializes the parser.
     * 
     * @param args the arguments
     * @param silent if <code>true</code>, no error messages will be written
     */
    public static boolean parse(String[] args, boolean silent) throws Exception {
    	CommandLineParser p = new CommandLineParser();
        commandLineParser.set(p);
        p.cmdLineParser = null;
        try {
        	p.cmdLineParser = new CmdLineParser(p);
        	p.cmdLineParser.parseArgument(args);
            if (p.arguments.isEmpty()) {
            	if (!silent) {
            		printUsage();
            		return false;
            	}
            }
            if (p.tabuFileName != null) {
                p.loadTabuTables(p.tabuFileName);
            }
            return true;
        } catch (CmdLineException e) {
            System.out.println(e.getMessage());
            printUsage();
            throw e;
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
        System.out.println("    creates the DDL for the working-tables and prints it to stdout");
        System.out.println();
        System.out.println("  jailer create-ddl <jdbc-driver-class> <db-URL> <db-user> <db-password>");
        System.out.println("    creates the DDL for the working-tables and executes it");
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
        getInstance().cmdLineParser.setUsageWidth(120);
        getInstance().cmdLineParser.printUsage(System.out);
        System.out.println();
    }

    /**
     * Gets 'tabu' tables.
     * 
     * @param dataModel to get tables from
     * @return set of 'tabu' tables or <code>null</code>, empty list if no tabu-file is given
     */
    public Set<Table> getTabuTables(DataModel dataModel, Map<String, String> sourceSchemaMapping) {
        return SqlUtil.readTableList(tabuTableNames, dataModel, sourceSchemaMapping);
    }
    
    /**
     * Loads 'tabu' tables file.
     */
    private void loadTabuTables(String fileName) throws Exception {
        File file = newFile(fileName);
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
    
    private Map<String, String> sourceSchemaMapping;

    public Map<String, String> getSourceSchemaMapping() {
		if (sourceSchemaMapping == null) {
			sourceSchemaMapping = new HashMap<String, String>();
			if (rawsourceschemamapping != null) {
				for (String item: rawsourceschemamapping.split(",")) {
					String[] fromTo = (" " + item + " ").split("=");
					if (fromTo.length == 2) {
						sourceSchemaMapping.put(fromTo[0].trim(), fromTo[1].trim());
					}
				}
			}
		}
		return sourceSchemaMapping;
	}
    
    /**
     * Gets the script format.
     * 
     * @return the script format
     */
    public ScriptFormat getScriptFormat() {
    	if (_asXml) {
    		return ScriptFormat.XML;
    	}
    	return ScriptFormat.valueOf(format);
    }
    
    @Option(name="-format",usage="export file format: SQL, XML or DBUNIT_FLAT_XML")
    public String format = "SQL";
    
    @Option(name="-xml",usage="export entities into XML file (deprecated, use -format XML instead)")
    public boolean _asXml = false;
    
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
    
    @Option(name="-schemamapping",usage="target schema map", metaVar="<from>=<to>[','<from>=<to>]*")
    public String rawschemamapping = null;
    
    @Option(name="-source-schemamapping",usage="source schema map", metaVar="<from>=<to>[','<from>=<to>]*")
    public String rawsourceschemamapping = null;
    
    @Option(name="-parameters",usage="parameters", metaVar="<parameter>=<value>[';'<parameter>=<value>]*")
    public String parameters = null;
    
    @Option(name="-threads",usage="number of threads (default is 1)", metaVar="#threads")
    public int numberOfThreads = 1;
    
    @Option(name="-entities",usage="maximum number of entities per insert-statement (in export-file, default is 10)", metaVar="#entities")
    public int numberOfEntities = 10;
    
    @Option(name="-upsert-only",usage="generate 'upsert'-statements for all entities (in export-file)")
    public boolean upsertOnly = false;
    
    @Option(name="-scope",usage="scope of working tables, GLOBAL, SESSION_LOCAL or TRANSACTION_LOCAL")
    public String scope = null;

	@Option(name="-datamodel", usage="folder holding the data model. Defaults to './datamodel'")
    public String datamodelFolder = "datamodel";

	@Option(name="-working-folder", usage="the working folder. Defaults to '.'")
    public String workingFolder = null;

	@Option(name="-script-enhancer", usage="folder holding the script-enhancer templates 'prolog' and 'epilog'. Defaults to '.'")
    public String enhancerFolder = ".";

	@Option(name="-jdbcjar", usage="JDBC driver's jar file")
    public String jdbcjar = null;

	@Option(name="-jdbcjar2", usage="JDBC driver's secondary jar file")
    public String jdbcjar2 = null;

	@Option(name="-no-sorting", usage="if set, the exported rows will not be sorted according to foreign key constraints")
    public boolean noSorting = false;
	
	@Option(name="-transactional", usage="if set, import rows transactional")
    public boolean transactional = false;
	
	@Argument
    public List<String> arguments = new ArrayList<String>();

    /**
     * Gets working-folder option.
     * 
     * @return working folder option, or <code>null</code> if no working-folder option is given
     */
    public File getWorkingfolder() {
    	if (workingFolder == null) {
    		return null;
    	}
    	return new File(workingFolder);
    }

    /**
     * Gets {@link File} from a file name relative to the working-folder.
     * 
     * @param filename the file name
     * @return {@link File} from a file name relative to the working-folder
     */
    public File newFile(String filename) {
    	File wf = getWorkingfolder();
    	if (wf == null) {
    		return new File(filename);
    	}
    	File f = new File(filename);
    	if (f.isAbsolute()) {
    		return f;
    	}
    	return new File(wf, filename);
    }
    
    public TemporaryTableScope getTemporaryTableScope() {
    	if (scope == null) {
    		return TemporaryTableScope.GLOBAL;
    	}
    	try {
    		return TemporaryTableScope.valueOf(scope);
    	} catch (Exception e) {
    		return TemporaryTableScope.GLOBAL;
    	}
    }
 
    public Map<String, String> getParameters() {
    	Map<String, String> map = new TreeMap<String, String>();
    	
    	if (parameters != null) {
    		for (String pv: CsvFile.decodeLine(parameters)) {
    			int i = pv.indexOf('=');
    			if (i >= 0) {
    				map.put(pv.substring(0, i), pv.substring(i + 1));
    			}
    		}
    	}
    	
    	return map;
    }
    
    /**
     * Folder of current data model.
     */
    private String currentModelSubfolder = null;
    
    /**
     * Sets folder of current data model.
     * 
     * @param modelFolder the folder, <code>null</code> for default model
     */
	public void setCurrentModelSubfolder(String modelFolder) {
		currentModelSubfolder = modelFolder;
	}

	/**
     * Gets folder of current data model.
     * 
     * @return modelFolder the folder, <code>null</code> for default model
     */
	public String getCurrentModelSubfolder() {
		return currentModelSubfolder;
	}

	/**
     * Gets fully qualified folder name of current data model.
     */
	public String getDataModelFolder() {
		if (currentModelSubfolder == null) {
			return datamodelFolder;
		}
		return datamodelFolder + File.separator + currentModelSubfolder;
	}
	
	/**
     * Names of 'tabu' tables.
     */
    private CsvFile tabuTableNames = null;
    
    /**
     * The parser.
     */
    private CmdLineParser cmdLineParser;

}