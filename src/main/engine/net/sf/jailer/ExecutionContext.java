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
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;
import java.util.UUID;

import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.TemporaryTableScope;
import net.sf.jailer.subsetting.ScriptFormat;
import net.sf.jailer.util.CsvFile;

/**
 * Execution context of import-/export commands.
 * 
 * @author Ralf Wisser
 */
public class ExecutionContext {
	
	/**
	 * Default constructor.
	 */
	public ExecutionContext() {
	}

	/**
	 * Creates new context with attributes taken from {@link ExecutionContext}.
	 * 
	 * @param executionContext the command line
	 */
	public ExecutionContext(CommandLine commandLine) throws Exception {
		copyCommandLineFields(commandLine);
	}

	/**
	 * Creates a temporary file
	 * 
	 * @return a temporary file
	 */
	public File createTempFile() {
		String file;
		String ts = UUID.randomUUID().toString();
		File newFile;
		for (int i = 1; ; ++i) {
			file = getTempFileFolder();
			newFile = newFile(file);
			newFile.mkdirs();
			file += File.separator + "up" + "-" + ts + (i > 1? "-" + Integer.toString(i) : "");
			newFile = newFile(file);
			if (!newFile.exists()) {
				break;
			}
		}
		return new File(file);
	}

	/**
	 * If <code>true</code>, Use UTF-8 encoding
	 *
	 * @return <code>true</code> if Use UTF-8 encoding
	 */
	public boolean getUTF8() {
		return uTF8;
	}

	/**
	 * If <code>true</code>, Use UTF-8 encoding
	 *
	 * @param uTF8
	 *            <code>true</code> if Use UTF-8 encoding
	 */
	public void setUTF8(boolean uTF8) {
		this.uTF8 = uTF8;
	}

	/**
	 * Gets target-DBMS: ORACLE, MSSQL, DB2, MySQL, POSTGRESQL, SYBASE, SQLITE,
	 * HSQL or H2
	 *
	 * @return target-DBMS: ORACLE, MSSQL, DB2, MySQL, POSTGRESQL, SYBASE,
	 *         SQLITE, HSQL or H2
	 */
	public DBMS getTargetDBMS() {
		return targetDBMS;
	}

	/**
	 * Sets target-DBMS: ORACLE, MSSQL, DB2, MySQL, POSTGRESQL, SYBASE, SQLITE,
	 * HSQL or H2
	 *
	 * @param targetDBMS
	 *            target-DBMS: ORACLE, MSSQL, DB2, MySQL, POSTGRESQL, SYBASE,
	 *            SQLITE, HSQL or H2
	 */
	public void setTargetDBMS(DBMS targetDBMS) {
		this.targetDBMS = targetDBMS;
	}

	/**
	 * Gets root tag of XML export file
	 *
	 * @return root tag of XML export file
	 */
	public String getXmlRootTag() {
		return xmlRootTag;
	}

	/**
	 * Sets root tag of XML export file
	 *
	 * @param xmlRootTag
	 *            root tag of XML export file
	 */
	public void setXmlRootTag(String xmlRootTag) {
		this.xmlRootTag = xmlRootTag;
	}

	/**
	 * Gets pattern for dates in XML and LIQUIBASE_XML export file
	 *
	 * @return pattern for dates in XML and LIQUIBASE_XML export file
	 */
	public String getXmlDatePattern() {
		return xmlDatePattern;
	}

	/**
	 * Sets pattern for dates in XML and LIQUIBASE_XML export file
	 *
	 * @param xmlDatePattern
	 *            pattern for dates in XML and LIQUIBASE_XML export file
	 */
	public void setXmlDatePattern(String xmlDatePattern) {
		this.xmlDatePattern = xmlDatePattern;
	}

	/**
	 * Gets pattern for times in XML and LIQUIBASE_XML export file
	 *
	 * @return pattern for times in XML and LIQUIBASE_XML export file
	 */
	public String getXmlTimePattern() {
		return xmlTimePattern;
	}

	/**
	 * Sets pattern for times in XML and LIQUIBASE_XML export file
	 *
	 * @param xmlTimePattern
	 *            pattern for times in XML and LIQUIBASE_XML export file
	 */
	public void setXmlTimePattern(String xmlTimePattern) {
		this.xmlTimePattern = xmlTimePattern;
	}

	/**
	 * Gets pattern for time-stamps in XML and LIQUIBASE_XML export file
	 *
	 * @return pattern for time-stamps in XML and LIQUIBASE_XML export file
	 */
	public String getXmlTimeStampPattern() {
		return xmlTimeStampPattern;
	}

	/**
	 * Sets pattern for time-stamps in XML and LIQUIBASE_XML export file
	 *
	 * @param xmlTimeStampPattern
	 *            pattern for time-stamps in XML and LIQUIBASE_XML export file
	 */
	public void setXmlTimeStampPattern(String xmlTimeStampPattern) {
		this.xmlTimeStampPattern = xmlTimeStampPattern;
	}

	/**
	 * Gets name of the export-script file (compressed if it ends with '.zip' or
	 * '.gz')
	 *
	 * @return name of the export-script file (compressed if it ends with '.zip'
	 *         or '.gz')
	 */
	public String getExportScriptFileName() {
		return exportScriptFileName;
	}

	/**
	 * Sets name of the export-script file (compressed if it ends with '.zip' or
	 * '.gz')
	 *
	 * @param exportScriptFileName
	 *            name of the export-script file (compressed if it ends with
	 *            '.zip' or '.gz')
	 */
	public void setExportScriptFileName(String exportScriptFileName) {
		this.exportScriptFileName = exportScriptFileName;
	}

	/**
	 * If <code>true</code>, Add schema prefix to table names after analysing
	 * the DB
	 *
	 * @return <code>true</code> if Add schema prefix to table names after
	 *         analysing the DB
	 */
	public boolean getQualifyNames() {
		return qualifyNames;
	}

	/**
	 * If <code>true</code>, Add schema prefix to table names after analysing
	 * the DB
	 *
	 * @param qualifyNames
	 *            <code>true</code> if Add schema prefix to table names after
	 *            analysing the DB
	 */
	public void setQualifyNames(boolean qualifyNames) {
		this.qualifyNames = qualifyNames;
	}

	/**
	 * If <code>true</code>, Look for aliases while analysing the DB
	 *
	 * @return <code>true</code> if Look for aliases while analysing the DB
	 */
	public boolean getAnalyseAlias() {
		return analyseAlias;
	}

	/**
	 * If <code>true</code>, Look for aliases while analysing the DB
	 *
	 * @param analyseAlias
	 *            <code>true</code> if Look for aliases while analysing the DB
	 */
	public void setAnalyseAlias(boolean analyseAlias) {
		this.analyseAlias = analyseAlias;
	}

	/**
	 * If <code>true</code>, Look for synonyms while analysing the DB
	 *
	 * @return <code>true</code> if Look for synonyms while analysing the DB
	 */
	public boolean getAnalyseSynonym() {
		return analyseSynonym;
	}

	/**
	 * If <code>true</code>, Look for synonyms while analysing the DB
	 *
	 * @param analyseSynonym
	 *            <code>true</code> if Look for synonyms while analysing the DB
	 */
	public void setAnalyseSynonym(boolean analyseSynonym) {
		this.analyseSynonym = analyseSynonym;
	}

	/**
	 * If <code>true</code>, Look for views while analysing the DB
	 *
	 * @return <code>true</code> if Look for views while analysing the DB
	 */
	public boolean getAnalyseView() {
		return analyseView;
	}

	/**
	 * If <code>true</code>, Look for views while analysing the DB
	 *
	 * @param analyseView
	 *            <code>true</code> if Look for views while analysing the DB
	 */
	public void setAnalyseView(boolean analyseView) {
		this.analyseView = analyseView;
	}

	/**
	 * Gets subject condition
	 *
	 * @return subject condition
	 */
	public String getWhere() {
		return where;
	}

	/**
	 * Sets subject condition
	 *
	 * @param where
	 *            subject condition
	 */
	public void setWhere(String where) {
		this.where = where;
	}

	/**
	 * Gets number of threads (default is 1)
	 *
	 * @return number of threads (default is 1)
	 */
	public int getNumberOfThreads() {
		return numberOfThreads;
	}

	/**
	 * Sets number of threads (default is 1)
	 *
	 * @param numberOfThreads
	 *            number of threads (default is 1)
	 */
	public void setNumberOfThreads(int numberOfThreads) {
		this.numberOfThreads = numberOfThreads;
	}

	/**
	 * Gets maximum number of entities per insert-statement (in export-file,
	 * default is 10)
	 *
	 * @return maximum number of entities per insert-statement (in export-file,
	 *         default is 10)
	 */
	public int getNumberOfEntities() {
		return numberOfEntities;
	}

	/**
	 * Sets maximum number of entities per insert-statement (in export-file,
	 * default is 10)
	 *
	 * @param numberOfEntities
	 *            maximum number of entities per insert-statement (in
	 *            export-file, default is 10)
	 */
	public void setNumberOfEntities(int numberOfEntities) {
		this.numberOfEntities = numberOfEntities;
	}

	/**
	 * If <code>true</code>, Generate 'upsert'-statements for all entities (in
	 * export-file)
	 *
	 * @return <code>true</code> if Generate 'upsert'-statements for all
	 *         entities (in export-file)
	 */
	public boolean getUpsertOnly() {
		return upsertOnly;
	}

	/**
	 * If <code>true</code>, Generate 'upsert'-statements for all entities (in
	 * export-file)
	 *
	 * @param upsertOnly
	 *            <code>true</code> if Generate 'upsert'-statements for all
	 *            entities (in export-file)
	 */
	public void setUpsertOnly(boolean upsertOnly) {
		this.upsertOnly = upsertOnly;
	}

	/**
	 * Gets scope of working tables, GLOBAL, SESSION_LOCAL or LOCAL_DATABASE
	 *
	 * @return scope of working tables, GLOBAL, SESSION_LOCAL or LOCAL_DATABASE
	 */
	public TemporaryTableScope getScope() {
		return scope;
	}

	/**
	 * Sets scope of working tables, GLOBAL, SESSION_LOCAL or LOCAL_DATABASE
	 *
	 * @param scope
	 *            scope of working tables, GLOBAL, SESSION_LOCAL or
	 *            LOCAL_DATABASE
	 */
	public void setScope(TemporaryTableScope scope) {
		this.scope = scope;
	}

	/**
	 * Gets schema in which the working tables will be created
	 *
	 * @return schema in which the working tables will be created
	 */
	public String getWorkingTableSchema() {
		return workingTableSchema;
	}

	/**
	 * Sets schema in which the working tables will be created
	 *
	 * @param workingTableSchema
	 *            schema in which the working tables will be created
	 */
	public void setWorkingTableSchema(String workingTableSchema) {
		this.workingTableSchema = workingTableSchema;
	}

	/**
	 * Gets folder holding the data model. Defaults to './datamodel'
	 *
	 * @return folder holding the data model. Defaults to './datamodel'
	 */
	public String getDatamodelFolder() {
		return datamodelFolder;
	}

	/**
	 * Sets folder holding the data model. Defaults to './datamodel'
	 *
	 * @param datamodelFolder
	 *            folder holding the data model. Defaults to './datamodel'
	 */
	public void setDatamodelFolder(String datamodelFolder) {
		this.datamodelFolder = datamodelFolder;
	}

	/**
	 * Gets the working folder. Defaults to '.'
	 *
	 * @return the working folder. Defaults to '.'
	 */
	public String getWorkingFolder() {
		return workingFolder;
	}

	/**
     * Gets fully qualified folder name of current data model.
     */
	public String getQualifiedDatamodelFolder() {
		if (currentModelSubfolder == null) {
			return datamodelFolder;
		}
		return datamodelFolder + File.separator + currentModelSubfolder;
	}

	/**
	 * Gets the temporary files folder. Defaults to 'tmp'.
	 * 
	 * @return the tempFileFolder absolute or relative to {@link #getWorkingFolder()}
	 */
	public String getTempFileFolder() {
		return tempFileFolder;
	}

	/**
	 * Sets the temporary files folder. Defaults to 'tmp'.
	 * 
	 * @param tempFileFolder absolute or relative to {@link #getWorkingFolder()}
	 */
	public void setTempFileFolder(String tempFileFolder) {
		this.tempFileFolder = tempFileFolder;
	}


	/**
	 * Sets the working folder. Defaults to '.'
	 *
	 * @param workingFolder
	 *            the working folder. Defaults to '.'
	 */
	public void setWorkingFolder(String workingFolder) {
		this.workingFolder = workingFolder;
	}

	/**
	 * If <code>true</code>, The exported rows will not be sorted according to
	 * foreign key constraints
	 *
	 * @return <code>true</code> if The exported rows will not be sorted
	 *         according to foreign key constraints
	 */
	public boolean getNoSorting() {
		return noSorting;
	}

	/**
	 * If <code>true</code>, The exported rows will not be sorted according to
	 * foreign key constraints
	 *
	 * @param noSorting
	 *            <code>true</code> if The exported rows will not be sorted
	 *            according to foreign key constraints
	 */
	public void setNoSorting(boolean noSorting) {
		this.noSorting = noSorting;
	}

	/**
	 * If <code>true</code>, Import rows in a single transaction
	 *
	 * @return <code>true</code> if Import rows in a single transaction
	 */
	public boolean getTransactional() {
		return transactional;
	}

	/**
	 * If <code>true</code>, Import rows in a single transaction
	 *
	 * @param transactional
	 *            <code>true</code> if Import rows in a single transaction
	 */
	public void setTransactional(boolean transactional) {
		this.transactional = transactional;
	}

	/**
	 * If <code>true</code>, Use primary keys to determine row identity (instead
	 * of rowid-column)
	 *
	 * @return <code>true</code> if Use primary keys to determine row identity
	 *         (instead of rowid-column)
	 */
	public boolean getNoRowid() {
		return noRowid;
	}

	/**
	 * If <code>true</code>, Use primary keys to determine row identity (instead
	 * of rowid-column)
	 *
	 * @param noRowid
	 *            <code>true</code> if Use primary keys to determine row
	 *            identity (instead of rowid-column)
	 */
	public void setNoRowid(boolean noRowid) {
		this.noRowid = noRowid;
	}

	/**
	 * Gets schema in which the import-filter mapping tables will be created
	 *
	 * @return schema in which the import-filter mapping tables will be created
	 */
	public String getImportFilterMappingTableSchema() {
		return importFilterMappingTableSchema;
	}

	/**
	 * Sets schema in which the import-filter mapping tables will be created
	 *
	 * @param importFilterMappingTableSchema
	 *            schema in which the import-filter mapping tables will be
	 *            created
	 */
	public void setImportFilterMappingTableSchema(String importFilterMappingTableSchema) {
		this.importFilterMappingTableSchema = importFilterMappingTableSchema;
	}

	/**
     * Gets {@link File} from a file name relative to the working-folder.
     * 
     * @param filename the file name
     * @return {@link File} from a file name relative to the working-folder
     */
    public File newFile(String filename) {
    	File wf;
    	if (workingFolder == null) {
    		wf = null;
    	} else {
    		wf = new File(workingFolder);
    	}
    	if (wf == null) {
    		return new File(filename);
    	}
    	File f = new File(filename);
    	if (f.isAbsolute()) {
    		return f;
    	}
    	return new File(wf, filename);
    }
    
	/**
	 * Gets parameters
	 *
	 * @return parameters
	 */
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

	/**
	 * Sets source schema map
	 *
	 * @param rawsourceschemamapping
	 *            source schema map
	 */
	public void setSchemaMapping(Map<String, String> schemaMapping) {
		this.schemaMapping = schemaMapping;
	}

    private Map<String, String> sourceSchemaMapping;

    /**
	 * @param sourceSchemaMapping the sourceSchemaMapping to set
	 */
	public void setSourceSchemaMapping(Map<String, String> sourceSchemaMapping) {
		this.sourceSchemaMapping = sourceSchemaMapping;
	}

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
    
    private ScriptFormat scriptFormat;
    
    /**
     * Gets the script format.
     * 
     * @return the script format
     */
    public ScriptFormat getScriptFormat() {
    	if (scriptFormat == null) {
	    	if (_asXml) {
	    		scriptFormat = ScriptFormat.XML;
	    	} else if (format != null) {
	    		scriptFormat = ScriptFormat.valueOf(format);
	    	}
    	}
    	return scriptFormat;
    }
    
    /**
     * Sets the script format.
     * 
     * @return the script format
     */
    public void setScriptFormat(ScriptFormat scriptFormat) {
    	this.scriptFormat = scriptFormat;
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

	private URL datamodelURL;
	
	/**
     * Gets URL of the current data model (the datamodels base folder)
     */
	public URL getDataModelURL() {
		if (datamodelURL == null) {
			String fn;
			if (currentModelSubfolder == null) {
				fn = datamodelFolder;
			} else {
				fn = datamodelFolder + File.separator + currentModelSubfolder;
			}
			try {
				datamodelURL = newFile(fn).toURI().toURL();
			} catch (MalformedURLException e) {
				throw new RuntimeException(e);
			}
		}
		return datamodelURL;
	}
	
	/**
     * Sets URL of the current data model (the datamodels base folder)
     */
	public void setDataModelURL(URL datamodelURL) {
		this.datamodelURL = datamodelURL;
	}
	
	// use UTF-8 encoding
	private boolean uTF8 = false;

	// export file format: SQL, XML, DBUNIT_FLAT_XML, INTRA_DATABASE or
	// LIQUIBASE_XML
	private String format = "SQL";

	// target-DBMS: ORACLE, MSSQL, DB2, MySQL, POSTGRESQL, SYBASE, SQLITE, HSQL
	// or H2
	private DBMS targetDBMS = null;

	// export entities into XML file (deprecated, use -format XML instead)
	private boolean _asXml = false;

	// root tag of XML export file
	private String xmlRootTag = "entities";

	// pattern for dates in XML and LIQUIBASE_XML export file
	private String xmlDatePattern = "yyyy-MM-dd";

	// pattern for times in XML and LIQUIBASE_XML export file
	private String xmlTimePattern = "HH.mm.ss";

	// pattern for time-stamps in XML and LIQUIBASE_XML export file
	private String xmlTimeStampPattern = "yyyy-MM-dd-HH.mm.ss";

	// name of the export-script file (compressed if it ends with '.zip' or
	// '.gz')
	private String exportScriptFileName = null;

	// add schema prefix to table names after analysing the DB
	private boolean qualifyNames = false;

	// look for aliases while analysing the DB
	private boolean analyseAlias = false;

	// look for synonyms while analysing the DB
	private boolean analyseSynonym = false;

	// look for views while analysing the DB
	private boolean analyseView = false;

	// subject condition
	private String where = null;

	// target schema map
	private String rawschemamapping = null;

	// source schema map
	private String rawsourceschemamapping = null;

	// parameters
	private String parameters = null;

	// number of threads (default is 1)
	private int numberOfThreads = 1;

	// maximum number of entities per insert-statement (in export-file, default
	// is 10)
	private int numberOfEntities = 10;

	// generate 'upsert'-statements for all entities (in export-file)
	private boolean upsertOnly = false;

	// schema in which the working tables will be created
	private String workingTableSchema = null;

	// folder holding the data model. Defaults to './datamodel'
	private String datamodelFolder = "datamodel";

	// the working folder. Defaults to '.'
	private String workingFolder = null;

	// the temporary files folder. Defaults to 'tmp'
	private String tempFileFolder = "tmp";

	// the exported rows will not be sorted according to foreign key constraints
	private boolean noSorting = false;

	// import rows in a single transaction
	private boolean transactional = false;

	// use primary keys to determine row identity (instead of rowid-column)
	private boolean noRowid = false;

	// schema in which the import-filter mapping tables will be created
	private String importFilterMappingTableSchema = "";

	private TemporaryTableScope scope;

	private void copyCommandLineFields(CommandLine commandLine) {
		uTF8 = commandLine.uTF8;
		format = commandLine.format;
		targetDBMS = commandLine.targetDBMS == null? null : DBMS.forDBMS(commandLine.targetDBMS);
		_asXml = commandLine._asXml;
		xmlRootTag = commandLine.xmlRootTag;
		xmlDatePattern = commandLine.xmlDatePattern;
		xmlTimePattern = commandLine.xmlTimePattern;
		xmlTimeStampPattern = commandLine.xmlTimeStampPattern;
		exportScriptFileName = commandLine.exportScriptFileName;
		qualifyNames = commandLine.qualifyNames;
		analyseAlias = commandLine.analyseAlias;
		analyseSynonym = commandLine.analyseSynonym;
		analyseView = commandLine.analyseView;
		where = commandLine.where;
		rawschemamapping = commandLine.rawschemamapping;
		rawsourceschemamapping = commandLine.rawsourceschemamapping;
		parameters = commandLine.parameters;
		numberOfThreads = commandLine.numberOfThreads;
		numberOfEntities = commandLine.numberOfEntities;
		upsertOnly = commandLine.upsertOnly;
		if (commandLine.scope == null) {
    		scope = TemporaryTableScope.GLOBAL;
    	}
    	try {
    		scope = TemporaryTableScope.valueOf(commandLine.scope);
    	} catch (Exception e) {
    		scope = TemporaryTableScope.GLOBAL;
    	}
    	workingTableSchema = commandLine.workingTableSchema;
		datamodelFolder = commandLine.datamodelFolder;
		workingFolder = commandLine.workingFolder;
		noSorting = commandLine.noSorting;
		transactional = commandLine.transactional;
		noRowid = commandLine.noRowid;
		importFilterMappingTableSchema = commandLine.importFilterMappingTableSchema;
	}

}