/*
 * Copyright 2007 - 2017 the original author or authors.
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

import org.kohsuke.args4j.Argument;
import org.kohsuke.args4j.Option;

import net.sf.jailer.database.TemporaryTableScope;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.SqlUtil;

/**
 * Holds command-line arguments.
 * 
 * @author Ralf Wisser
 */
public class CommandLine {

    @Option(name="-UTF8",usage="use UTF-8 encoding")
    public boolean uTF8 = false;
    
    @Option(name="-format",usage="export file format: SQL, XML, DBUNIT_FLAT_XML, INTRA_DATABASE or LIQUIBASE_XML")
    public String format = "SQL";
    
    @Option(name="-target-dbms", usage="target-DBMS: ORACLE, MSSQL, DB2, MySQL, POSTGRESQL, SYBASE, SQLITE, HSQL or H2", metaVar="<DBMS>")
    public String targetDBMS = null;
    
    @Option(name="-xml",usage="export entities into XML file (deprecated, use -format XML instead)")
    public boolean _asXml = false;
    
    @Option(name="-xml-root",usage="root tag of XML export file",metaVar="tag-name")
    public String xmlRootTag = "entities";

    @Option(name="-xml-date",usage="pattern for dates in XML and LIQUIBASE_XML export file",metaVar="pattern")
    public String xmlDatePattern = "yyyy-MM-dd";
    
    @Option(name="-xml-time",usage="pattern for times in XML and LIQUIBASE_XML export file",metaVar="pattern")
    public String xmlTimePattern = "HH.mm.ss";

    @Option(name="-xml-timestamp",usage="pattern for time-stamps in XML and LIQUIBASE_XML export file",metaVar="pattern")
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

    @Option(name="-schema",usage="schema to reduce JDBC-Introspection to", metaVar="schema")
    public String schema = null;

    @Option(name="-qualifyNames",usage="add schema prefix to table names after analysing the DB", metaVar="export-script")
    public boolean qualifyNames = false;
    
    @Option(name="-analyse-alias",usage="look for aliases while analysing the DB")
    public boolean analyseAlias = false;
    
    @Option(name="-analyse-synonym",usage="look for synonyms while analysing the DB")
    public boolean analyseSynonym = false;
    
    @Option(name="-analyse-view",usage="look for views while analysing the DB")
    public boolean analyseView = false;
    
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
    
    @Option(name="-scope",usage="scope of working tables, GLOBAL, SESSION_LOCAL or LOCAL_DATABASE")
    public String scope = null;

	@Option(name="-working-table-schema", usage="schema in which the working tables will be created")
    public String workingTableSchema = null;

	@Option(name="-datamodel", usage="folder holding the data model. Defaults to './datamodel'")
    public String datamodelFolder = "datamodel";

	@Option(name="-working-folder", usage="the working folder. Defaults to '.'")
    public String workingFolder = null;

	@Option(name="-jdbcjar", usage="JDBC driver's jar file")
    public String jdbcjar = null;

	@Option(name="-jdbcjar2", usage="JDBC driver's secondary jar file")
    public String jdbcjar2 = null;

	@Option(name="-no-sorting", usage="the exported rows will not be sorted according to foreign key constraints")
    public boolean noSorting = false;
	
	@Option(name="-transactional", usage="import rows in a single transaction")
    public boolean transactional = false;
	
	@Option(name="-no-rowid", usage="use primary keys to determine row identity (instead of rowid-column)")
    public boolean noRowid = false;
	
	@Option(name="-import-filter-mapping-table-schema", usage="schema in which the import-filter mapping tables will be created")
    public String importFilterMappingTableSchema = "";
	
	@Option(name="-script-enhancer", usage="no longer used")
    public String scriptEnhancer = "";
	
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
    void loadTabuTables(String fileName) throws Exception {
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

}