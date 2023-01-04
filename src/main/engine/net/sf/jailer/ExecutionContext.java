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

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.sql.Connection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import net.sf.jailer.api.Subsetter;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.WorkingTableScope;
import net.sf.jailer.progress.ProgressListenerRegistry;
import net.sf.jailer.subsetting.InconsistentSubsettingResultException;
import net.sf.jailer.subsetting.ScriptFormat;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.LayoutStorage;

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
	 * Copy constructor.
	 */
	public ExecutionContext(ExecutionContext other) {
		this.schemaMapping = copy(other.schemaMapping);
		this.sourceSchemaMapping = copy(other.sourceSchemaMapping);
		this.deletionSchemaMapping = copy(other.deletionSchemaMapping);
		this.scriptFormat = other.scriptFormat;
		this.currentModelSubfolder = other.currentModelSubfolder;
		this.datamodelURL = other.datamodelURL;
		this.uTF8 = other.uTF8;
		this.format = other.format;
		this.targetDBMS = other.targetDBMS;
		this._asXml = other._asXml;
		this.xmlRootTag = other.xmlRootTag;
		this.xmlDatePattern = other.xmlDatePattern;
		this.xmlTimePattern = other.xmlTimePattern;
		this.xmlTimeStampPattern = other.xmlTimeStampPattern;
		this.qualifyNames = other.qualifyNames;
		this.analyseAlias = other.analyseAlias;
		this.analyseSynonym = other.analyseSynonym;
		this.analyseView = other.analyseView;
		this.rawschemamapping = other.rawschemamapping;
		this.rawsourceschemamapping = other.rawsourceschemamapping;
		this.rawdeletionschemamapping = other.rawdeletionschemamapping;
		this.parameters = copy(other.parameters);
		this.numberOfThreads = other.numberOfThreads;
		this.numberOfEntities = other.numberOfEntities;
		this.upsertOnly = other.upsertOnly;
		this.workingTableSchema = other.workingTableSchema;
		this.datamodelFolder = other.datamodelFolder;
		this.noSorting = other.noSorting;
		this.orderByPK = other.orderByPK;
		this.transactional = other.transactional;
		this.isolationLevel = other.isolationLevel;
		this.useRowid = other.useRowid;
		this.useRowIdsOnlyForTablesWithoutPK = other.useRowIdsOnlyForTablesWithoutPK;
		this.importFilterMappingTableSchema = other.importFilterMappingTableSchema;
		this.scope = other.scope;
		this.rawparameters = other.rawparameters;
		this.embedded = other.embedded;
		this.insertIncrementally = other.insertIncrementally;
		this.abortInCaseOfInconsistency = other.abortInCaseOfInconsistency;
		this.independentWorkingTables = other.independentWorkingTables;
		this.upkDomain = other.upkDomain;
		this.currentConnectionAlias = other.currentConnectionAlias;
		this.limit = other.limit;
// don't share progressListenerRegistry, was: this.progressListenerRegistry = other.progressListenerRegistry;
	}

	/**
	 * Creates new context with attributes taken from {@link ExecutionContext}.
	 *
	 * @param commandLine the command line
	 */
	public ExecutionContext(CommandLine commandLine) {
		copyCommandLineFields(commandLine);
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
	public WorkingTableScope getScope() {
		return scope;
	}

	/**
	 * Sets scope of working tables, GLOBAL, SESSION_LOCAL or LOCAL_DATABASE
	 *
	 * @param scope
	 *            scope of working tables, GLOBAL, SESSION_LOCAL or
	 *            LOCAL_DATABASE
	 */
	public void setScope(WorkingTableScope scope) {
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
	 * Gets fully qualified folder name of current data model.
	 */
	public String getQualifiedDatamodelFolder() {
		if (currentModelSubfolder == null) {
			return datamodelFolder;
		}
		return datamodelFolder + File.separator + currentModelSubfolder;
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
	 * If <code>true</code>, the exported rows will be ordered according to the primary key.
	 */
	public boolean getOrderByPK() {
		return orderByPK;
	}

	/**
	 * @param orderByPK if <code>true</code>, the exported rows will be ordered according to the primary key
	 */
	public void setOrderByPK(boolean orderByPK) {
		this.orderByPK = orderByPK;
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
	 * Gets IsolationLevel.
	 *
	 * @see Connection#setTransactionIsolation(int)
	 */
	public Integer getIsolationLevel() {
		return isolationLevel;
	}

	/**
	 * Sets IsolationLevel.
	 *
	 * @see Connection#setTransactionIsolation(int)
	 */
	public void setIsolationLevel(Integer isolationLevel) {
		this.isolationLevel = isolationLevel;
	}

	/**
	 * If <code>true</code>, use rowid/ctid-column to determine row identity (instead
	 * of primary keys)
	 *
	 * @return if <code>true</code> use rowid/ctid-column to determine row identity
	 *         (instead of primary keys)
	 */
	public boolean getUseRowid() {
		return useRowid;
	}

	/**
	 * If <code>true</code>, use rowid/ctid-column to determine row identity (instead
	 * of primary keys)
	 *
	 * @param useRowid
	 *            if <code>true</code> use rowid/ctid-column to determine row
	 *            identity (instead of primary keys)
	 */
	public void setUseRowid(boolean useRowid) {
		this.useRowid = useRowid;
	}

	/**
	 * If <code>true</code>, use rowid/ctid-column only for tables without primary key.
	 *
	 * @return if <code>true</code> use rowid/ctid-column only for tables without primary key
	 */
	public boolean getUseRowIdsOnlyForTablesWithoutPK() {
		return useRowIdsOnlyForTablesWithoutPK;
	}

	/**
	 * If <code>true</code>, use rowid/ctid-column only for tables without primary key.
	 *
	 * @param useRowIdsOnlyForTablesWithoutPK
	 *            if <code>true</code> use rowid/ctid-column only for tables without primary key
	 */
	public void setUseRowIdsOnlyForTablesWithoutPK(boolean useRowIdsOnlyForTablesWithoutPK) {
		this.useRowIdsOnlyForTablesWithoutPK = useRowIdsOnlyForTablesWithoutPK;
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
	 * If <code>true</code>, collects the rows using multiple insert operations with a limited number of rows per operation. <br>
	 * Use this option if otherwise the transactions become too big.
	 */
	public boolean isInsertIncrementally() {
		return insertIncrementally;
	}

	/**
	 * If <code>true</code>, collects the rows using multiple insert operations with a limited number of rows per operation. <br>
	 * Use this option if otherwise the transactions become too big.
	 */
	public void setInsertIncrementally(boolean insertIncrementally) {
		this.insertIncrementally = insertIncrementally;
	}

	/**
	 * @return if <code>true</code>, {@link Subsetter#execute(String, File)} throws an
	 *         {@link InconsistentSubsettingResultException} if the result is inconsistent
	 *         due to insufficient transaction isolation
	 */
	public boolean isAbortInCaseOfInconsistency() {
		return abortInCaseOfInconsistency;
	}

	/**
	 * @param abortInCaseOfInconsitency if <code>true</code>, {@link Subsetter#execute(String, File)} throws an
	 *         {@link InconsistentSubsettingResultException} if the result is inconsistent
	 *         due to insufficient transaction isolation
	 */
	public void setAbortInCaseOfInconsistency(boolean abortInCaseOfInconsitency) {
		this.abortInCaseOfInconsistency = abortInCaseOfInconsitency;
	}

	/**
	 * Is the subsetter embedded into an application?
	 */
	public boolean isEmbedded() {
		return embedded;
	}

	/**
	 * @param embedded is the subsetter embedded into an application?
	 */
	public void setEmbedded(boolean embedded) {
		this.embedded = embedded;
	}

	/**
	 * Gets parameters
	 *
	 * @return parameters
	 */
	public Map<String, String> getParameters() {
		if (parameters == null) {
			Map<String, String> map = new TreeMap<String, String>();

			if (rawparameters != null) {
				for (String pv: CsvFile.decodeLine(rawparameters)) {
					int i = pv.indexOf('=');
					if (i >= 0) {
						map.put(pv.substring(0, i), pv.substring(i + 1));
					}
				}
			}
			parameters = map;
		}
		return parameters;
	}

	/**
	 * Sets a parameter.
	 *
	 * @param name parameter name
	 * @param value value
	 */
	public void setParameter(String name, String value) {
		getParameters().put(name, value);
	}

	private Map<String, String> schemaMapping;

	public static Map<String, String> getSchemaMapping(String rawschemamapping) {
		Map<String, String> mapping = new HashMap<String, String>();
		if (rawschemamapping != null) {
			for (String item: rawschemamapping.split(",")) {
				String[] fromTo = (" " + item + " ").split("=");
				if (fromTo.length == 2) {
					mapping.put(fromTo[0].trim(), fromTo[1].trim());
				}
			}
		}
		return mapping;
	}

	public Map<String, String> getSchemaMapping() {
		if (schemaMapping == null) {
			schemaMapping = getSchemaMapping(rawschemamapping);
		}
		return schemaMapping;
	}

	/**
	 * Sets source schema map
	 *
	 * @param schemaMapping
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

	private Map<String, String> deletionSchemaMapping;

	/**
	 * @param deletionSchemaMapping the sourceSchemaMapping to set
	 */
	public void setDeletionSchemaMapping(Map<String, String> deletionSchemaMapping) {
		this.deletionSchemaMapping = deletionSchemaMapping;
	}

	public Map<String, String> getDeletionSchemaMapping() {
		if (deletionSchemaMapping == null) {
			deletionSchemaMapping = new HashMap<String, String>();
			String rawmapping = rawdeletionschemamapping;
			if (rawmapping == null) {
				rawmapping = rawsourceschemamapping;
			}
			if (rawmapping != null) {
				for (String item: rawmapping.split(",")) {
					String[] fromTo = (" " + item + " ").split("=");
					if (fromTo.length == 2) {
						deletionSchemaMapping.put(fromTo[0].trim(), fromTo[1].trim());
					}
				}
			}
		}
		return deletionSchemaMapping;
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
				return new File(fn).toURI().toURL();
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
		if (!datamodelURL.toExternalForm().endsWith("/")) {
			try {
				this.datamodelURL = new URL(datamodelURL.toExternalForm() + "/");
			} catch (MalformedURLException e) {
				throw new RuntimeException(e);
			}
		} else {
			this.datamodelURL = datamodelURL;
		}
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

	// add schema prefix to table names after analysing the DB
	private boolean qualifyNames = false;

	// look for aliases while analysing the DB
	private boolean analyseAlias = false;

	// look for synonyms while analysing the DB
	private boolean analyseSynonym = false;

	// look for views while analysing the DB
	private boolean analyseView = false;

	// target schema map
	private String rawschemamapping = null;

	// source schema map
	private String rawsourceschemamapping = null;

	// source schema map
	private String rawdeletionschemamapping = null;

	// parameters
	private Map<String, String> parameters = null;

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
	private String datamodelFolder = defaultDatamodelFolder;

	public static String defaultDatamodelFolder = "datamodel";

	// the exported rows will not be sorted according to foreign key constraints
	private boolean noSorting = false;

	// orders the exported rows according to the primary key
	private boolean orderByPK = false;

	// import rows in a single transaction
	private boolean transactional = false;

	// isolation level
	private Integer isolationLevel = null;

	// use rowid-column to determine row identity (instead of primary keys)
	private boolean useRowid = false;

	// use rowid-column only for tables without primary key
	private boolean useRowIdsOnlyForTablesWithoutPK = false;

	// collects the rows using multiple insert operations with a limited number of rows per operation
	private boolean insertIncrementally = false;

	// abort the process if the result is inconsistent due to insufficient transaction isolation
	private boolean abortInCaseOfInconsistency = false;

	// schema in which the import-filter mapping tables will be created
	private String importFilterMappingTableSchema = "";

	// create working tables that are independent of the extraction model. (Potentially less efficient)
	private boolean independentWorkingTables = false;

	// maximum allowed number of exported rows. If this limit is exceeded, the export aborts with an error.
	private Long limit;
	
	// the folder where the local database will be stored. Default temp folder is used if this is not specified.
	private String localDatabaseStorage = null;

	private WorkingTableScope scope = WorkingTableScope.GLOBAL;

	private String rawparameters;

	private boolean embedded = false;
	private Set<String> upkDomain;

	private String currentConnectionAlias;

	private ProgressListenerRegistry progressListenerRegistry = new ProgressListenerRegistry();

	/**
	 * Gets the {@link ProgressListenerRegistry}.
	 *
	 * @return the {@link ProgressListenerRegistry}
	 */
	public ProgressListenerRegistry getProgressListenerRegistry() {
		return progressListenerRegistry;
	}

	/**
	 * Sets the {@link ProgressListenerRegistry}.
	 *
	 * @param progressListenerRegistry the {@link ProgressListenerRegistry}
	 */
	public void setProgressListenerRegistry(ProgressListenerRegistry progressListenerRegistry) {
		this.progressListenerRegistry = progressListenerRegistry;
	}

	private LayoutStorage layoutStorage = new LayoutStorage();

	public LayoutStorage getLayoutStorage() {
		return layoutStorage;
	}

	public void setLayoutStorage(LayoutStorage layoutStorage) {
		this.layoutStorage = layoutStorage;
	}

	/**
	 * @return maximum allowed number of exported rows. If this limit is exceeded, the export aborts with an error.
	 */
	public Long getLimit() {
		return limit;
	}

	/**
	 * @param limit maximum allowed number of exported rows. If this limit is exceeded, the export aborts with an error.
	 */
	public void setLimit(Long limit) {
		this.limit = limit;
	}

	/**
	 * Create working tables that are independent of the extraction model. (Potentially less efficient)
	 */
	public boolean isIndependentWorkingTables() {
		return independentWorkingTables;
	}

	/**
	 * Create working tables that are independent of the extraction model. (Potentially less efficient)
	 */
	public void setIndependentWorkingTables(boolean independentWorkingTables) {
		this.independentWorkingTables = independentWorkingTables;
	}

	public Set<String> getUpkDomain() {
		return upkDomain;
	}

	public void setUpkDomain(Set<String> upkDomain) {
		this.upkDomain = upkDomain;
	}

	public String getCurrentConnectionAlias() {
		return currentConnectionAlias;
	}

	public void setCurrentConnectionAlias(String currentConnectionAlias) {
		this.currentConnectionAlias = currentConnectionAlias;
	}

	/**
	 * @return the folder where the local database will be stored. If <code>null</code>, default temp folder is used.
	 */
	public String getLocalDatabaseStorage() {
		return localDatabaseStorage;
	}

	/**
	 * Sets the folder where the local database will be stored.
	 * 
	 * @param localDatabaseStorage the folder where the local database will be stored. If <code>null</code>, default temp folder is used.
	 */
	public void setLocalDatabaseStorage(String localDatabaseStorage) {
		this.localDatabaseStorage = localDatabaseStorage;
	}

	private void copyCommandLineFields(CommandLine commandLine) {
		uTF8 = commandLine.uTF8;
		format = commandLine.format;
		targetDBMS = commandLine.targetDBMS == null? null : DBMS.forDBMS(commandLine.targetDBMS);
		_asXml = commandLine._asXml;
		xmlRootTag = commandLine.xmlRootTag;
		xmlDatePattern = commandLine.xmlDatePattern;
		xmlTimePattern = commandLine.xmlTimePattern;
		xmlTimeStampPattern = commandLine.xmlTimeStampPattern;
		qualifyNames = commandLine.qualifyNames;
		analyseAlias = commandLine.analyseAlias;
		analyseSynonym = commandLine.analyseSynonym;
		analyseView = commandLine.analyseView;
		rawschemamapping = commandLine.rawschemamapping;
		rawsourceschemamapping = commandLine.rawsourceschemamapping;
		rawdeletionschemamapping = commandLine.rawdeletionschemamapping;
		rawparameters = commandLine.parameters;
		numberOfThreads = commandLine.numberOfThreads;
		numberOfEntities = commandLine.numberOfEntities;
		upsertOnly = commandLine.upsertOnly;
		if (commandLine.scope == null) {
			scope = WorkingTableScope.GLOBAL;
		} else {
			try {
				scope = WorkingTableScope.valueOf(commandLine.scope);
			} catch (Exception e) {
				scope = WorkingTableScope.GLOBAL;
			}
		}
		workingTableSchema = commandLine.workingTableSchema;
		datamodelFolder = commandLine.datamodelFolder;
		noSorting = commandLine.noSorting;
		orderByPK = commandLine.orderByPK;
		independentWorkingTables = commandLine.independentWorkingTables;
		transactional = commandLine.transactional;
		isolationLevel = commandLine.isolationLevel;
		useRowid = commandLine.useRowid;
		useRowIdsOnlyForTablesWithoutPK = commandLine.useRowIdsOnlyForTablesWithoutPK;
		importFilterMappingTableSchema = commandLine.importFilterMappingTableSchema;
		insertIncrementally = commandLine.insertIncrementally;
		abortInCaseOfInconsistency = commandLine.abortInCaseOfInconsistency;
		limit = null;
		if (commandLine.limit != null) {
			String limitStr = commandLine.limit.trim();
			if (!limitStr.isEmpty()) {
				limit = Long.parseLong(commandLine.limit);
			}
		}
		localDatabaseStorage = commandLine.localDatabaseStorage;
	}

	private Map<String, String> copy(Map<String, String> map) {
		return map == null? null : new HashMap<String, String>(map);
	}

}