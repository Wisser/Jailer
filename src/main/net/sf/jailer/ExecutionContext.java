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

/**
 * Execution context of import-/export commands.
 * 
 * @author Ralf Wisser
 */
public class ExecutionContext {
	
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
	 * Gets export file format: SQL, XML, DBUNIT_FLAT_XML, INTRA_DATABASE or
	 * LIQUIBASE_XML
	 *
	 * @return export file format: SQL, XML, DBUNIT_FLAT_XML, INTRA_DATABASE or
	 *         LIQUIBASE_XML
	 */
	public String getFormat() {
		return format;
	}

	/**
	 * Sets export file format: SQL, XML, DBUNIT_FLAT_XML, INTRA_DATABASE or
	 * LIQUIBASE_XML
	 *
	 * @param format
	 *            export file format: SQL, XML, DBUNIT_FLAT_XML, INTRA_DATABASE
	 *            or LIQUIBASE_XML
	 */
	public void setFormat(String format) {
		this.format = format;
	}

	/**
	 * Gets target-DBMS: ORACLE, MSSQL, DB2, MySQL, POSTGRESQL, SYBASE, SQLITE,
	 * HSQL or H2
	 *
	 * @return target-DBMS: ORACLE, MSSQL, DB2, MySQL, POSTGRESQL, SYBASE,
	 *         SQLITE, HSQL or H2
	 */
	public String getTargetDBMS() {
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
	public void setTargetDBMS(String targetDBMS) {
		this.targetDBMS = targetDBMS;
	}

	/**
	 * If <code>true</code>, Export entities into XML file (deprecated, use
	 * -format XML instead)
	 *
	 * @return <code>true</code> if Export entities into XML file (deprecated,
	 *         use -format XML instead)
	 */
	public boolean get_asXml() {
		return _asXml;
	}

	/**
	 * If <code>true</code>, Export entities into XML file (deprecated, use
	 * -format XML instead)
	 *
	 * @param _asXml
	 *            <code>true</code> if Export entities into XML file
	 *            (deprecated, use -format XML instead)
	 */
	public void set_asXml(boolean _asXml) {
		this._asXml = _asXml;
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
	 * If <code>true</code>, Print restricted data-model with closures
	 *
	 * @return <code>true</code> if Print restricted data-model with closures
	 */
	public boolean getWithClosures() {
		return withClosures;
	}

	/**
	 * If <code>true</code>, Print restricted data-model with closures
	 *
	 * @param withClosures
	 *            <code>true</code> if Print restricted data-model with closures
	 */
	public void setWithClosures(boolean withClosures) {
		this.withClosures = withClosures;
	}

	/**
	 * If <code>true</code>, Write export-explanation ('explain.log')
	 *
	 * @return <code>true</code> if Write export-explanation ('explain.log')
	 */
	public boolean getExplain() {
		return explain;
	}

	/**
	 * If <code>true</code>, Write export-explanation ('explain.log')
	 *
	 * @param explain
	 *            <code>true</code> if Write export-explanation ('explain.log')
	 */
	public void setExplain(boolean explain) {
		this.explain = explain;
	}

	/**
	 * If <code>true</code>, Consider associations as un-directed
	 *
	 * @return <code>true</code> if Consider associations as un-directed
	 */
	public boolean getUndirected() {
		return undirected;
	}

	/**
	 * If <code>true</code>, Consider associations as un-directed
	 *
	 * @param undirected
	 *            <code>true</code> if Consider associations as un-directed
	 */
	public void setUndirected(boolean undirected) {
		this.undirected = undirected;
	}

	/**
	 * Gets abort export if number of entities is greater than this limit
	 *
	 * @return abort export if number of entities is greater than this limit
	 */
	public int getMaxNumberOfEntities() {
		return maxNumberOfEntities;
	}

	/**
	 * Sets abort export if number of entities is greater than this limit
	 *
	 * @param maxNumberOfEntities
	 *            abort export if number of entities is greater than this limit
	 */
	public void setMaxNumberOfEntities(int maxNumberOfEntities) {
		this.maxNumberOfEntities = maxNumberOfEntities;
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
	 * Gets schema to reduce JDBC-Introspection to
	 *
	 * @return schema to reduce JDBC-Introspection to
	 */
	public String getSchema() {
		return schema;
	}

	/**
	 * Sets schema to reduce JDBC-Introspection to
	 *
	 * @param schema
	 *            schema to reduce JDBC-Introspection to
	 */
	public void setSchema(String schema) {
		this.schema = schema;
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
	 * Gets name of the delete-script file (compressed if it ends with '.zip' or
	 * '.gz')
	 *
	 * @return name of the delete-script file (compressed if it ends with '.zip'
	 *         or '.gz')
	 */
	public String getDeleteScriptFileName() {
		return deleteScriptFileName;
	}

	/**
	 * Sets name of the delete-script file (compressed if it ends with '.zip' or
	 * '.gz')
	 *
	 * @param deleteScriptFileName
	 *            name of the delete-script file (compressed if it ends with
	 *            '.zip' or '.gz')
	 */
	public void setDeleteScriptFileName(String deleteScriptFileName) {
		this.deleteScriptFileName = deleteScriptFileName;
	}

	/**
	 * Gets name of the 'tabu' file (for 'print-datamodel or 'find-association')
	 *
	 * @return name of the 'tabu' file (for 'print-datamodel or
	 *         'find-association')
	 */
	public String getTabuFileName() {
		return tabuFileName;
	}

	/**
	 * Sets name of the 'tabu' file (for 'print-datamodel or 'find-association')
	 *
	 * @param tabuFileName
	 *            name of the 'tabu' file (for 'print-datamodel or
	 *            'find-association')
	 */
	public void setTabuFileName(String tabuFileName) {
		this.tabuFileName = tabuFileName;
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
	 * Gets target schema map
	 *
	 * @return target schema map
	 */
	public String getRawschemamapping() {
		return rawschemamapping;
	}

	/**
	 * Sets target schema map
	 *
	 * @param rawschemamapping
	 *            target schema map
	 */
	public void setRawschemamapping(String rawschemamapping) {
		this.rawschemamapping = rawschemamapping;
	}

	/**
	 * Gets source schema map
	 *
	 * @return source schema map
	 */
	public String getRawsourceschemamapping() {
		return rawsourceschemamapping;
	}

	/**
	 * Sets source schema map
	 *
	 * @param rawsourceschemamapping
	 *            source schema map
	 */
	public void setRawsourceschemamapping(String rawsourceschemamapping) {
		this.rawsourceschemamapping = rawsourceschemamapping;
	}

	/**
	 * Gets parameters
	 *
	 * @return parameters
	 */
	public String getParameters() {
		return parameters;
	}

	/**
	 * Sets parameters
	 *
	 * @param parameters
	 *            parameters
	 */
	public void setParameters(String parameters) {
		this.parameters = parameters;
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
	public String getScope() {
		return scope;
	}

	/**
	 * Sets scope of working tables, GLOBAL, SESSION_LOCAL or LOCAL_DATABASE
	 *
	 * @param scope
	 *            scope of working tables, GLOBAL, SESSION_LOCAL or
	 *            LOCAL_DATABASE
	 */
	public void setScope(String scope) {
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
	 * Sets the working folder. Defaults to '.'
	 *
	 * @param workingFolder
	 *            the working folder. Defaults to '.'
	 */
	public void setWorkingFolder(String workingFolder) {
		this.workingFolder = workingFolder;
	}

	/**
	 * Gets JDBC driver's jar file
	 *
	 * @return JDBC driver's jar file
	 */
	public String getJdbcjar() {
		return jdbcjar;
	}

	/**
	 * Sets JDBC driver's jar file
	 *
	 * @param jdbcjar
	 *            JDBC driver's jar file
	 */
	public void setJdbcjar(String jdbcjar) {
		this.jdbcjar = jdbcjar;
	}

	/**
	 * Gets JDBC driver's secondary jar file
	 *
	 * @return JDBC driver's secondary jar file
	 */
	public String getJdbcjar2() {
		return jdbcjar2;
	}

	/**
	 * Sets JDBC driver's secondary jar file
	 *
	 * @param jdbcjar2
	 *            JDBC driver's secondary jar file
	 */
	public void setJdbcjar2(String jdbcjar2) {
		this.jdbcjar2 = jdbcjar2;
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
	 * Gets no longer used
	 *
	 * @return no longer used
	 */
	public String getScriptEnhancer() {
		return scriptEnhancer;
	}

	/**
	 * Sets no longer used
	 *
	 * @param scriptEnhancer
	 *            no longer used
	 */
	public void setScriptEnhancer(String scriptEnhancer) {
		this.scriptEnhancer = scriptEnhancer;
	}

	// use UTF-8 encoding
	private boolean uTF8 = false;

	// export file format: SQL, XML, DBUNIT_FLAT_XML, INTRA_DATABASE or
	// LIQUIBASE_XML
	private String format = "SQL";

	// target-DBMS: ORACLE, MSSQL, DB2, MySQL, POSTGRESQL, SYBASE, SQLITE, HSQL
	// or H2
	private String targetDBMS = "null";

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

	// print restricted data-model with closures
	private boolean withClosures = false;

	// write export-explanation ('explain.log')
	private boolean explain = false;

	// consider associations as un-directed
	private boolean undirected = false;

	// abort export if number of entities is greater than this limit
	private int maxNumberOfEntities = 0;

	// name of the export-script file (compressed if it ends with '.zip' or
	// '.gz')
	private String exportScriptFileName = "null";

	// schema to reduce JDBC-Introspection to
	private String schema = "null";

	// add schema prefix to table names after analysing the DB
	private boolean qualifyNames = false;

	// look for aliases while analysing the DB
	private boolean analyseAlias = false;

	// look for synonyms while analysing the DB
	private boolean analyseSynonym = false;

	// look for views while analysing the DB
	private boolean analyseView = false;

	// name of the delete-script file (compressed if it ends with '.zip' or
	// '.gz')
	private String deleteScriptFileName = "null";

	// name of the 'tabu' file (for 'print-datamodel or 'find-association')
	private String tabuFileName = "null";

	// subject condition
	private String where = "null";

	// target schema map
	private String rawschemamapping = "null";

	// source schema map
	private String rawsourceschemamapping = "null";

	// parameters
	private String parameters = "null";

	// number of threads (default is 1)
	private int numberOfThreads = 1;

	// maximum number of entities per insert-statement (in export-file, default
	// is 10)
	private int numberOfEntities = 10;

	// generate 'upsert'-statements for all entities (in export-file)
	private boolean upsertOnly = false;

	// scope of working tables, GLOBAL, SESSION_LOCAL or LOCAL_DATABASE
	private String scope = "null";

	// schema in which the working tables will be created
	private String workingTableSchema = "null";

	// folder holding the data model. Defaults to './datamodel'
	private String datamodelFolder = "datamodel";

	// the working folder. Defaults to '.'
	private String workingFolder = "null";

	// JDBC driver's jar file
	private String jdbcjar = "null";

	// JDBC driver's secondary jar file
	private String jdbcjar2 = "null";

	// the exported rows will not be sorted according to foreign key constraints
	private boolean noSorting = false;

	// import rows in a single transaction
	private boolean transactional = false;

	// use primary keys to determine row identity (instead of rowid-column)
	private boolean noRowid = false;

	// schema in which the import-filter mapping tables will be created
	private String importFilterMappingTableSchema = "";

	// no longer used
	private String scriptEnhancer = "";

	private void copyCommandLineFields(CommandLine commandLine) {
		uTF8 = commandLine.uTF8;
		format = commandLine.format;
		targetDBMS = commandLine.targetDBMS;
		_asXml = commandLine._asXml;
		xmlRootTag = commandLine.xmlRootTag;
		xmlDatePattern = commandLine.xmlDatePattern;
		xmlTimePattern = commandLine.xmlTimePattern;
		xmlTimeStampPattern = commandLine.xmlTimeStampPattern;
		withClosures = commandLine.withClosures;
		explain = commandLine.explain;
		undirected = commandLine.undirected;
		maxNumberOfEntities = commandLine.maxNumberOfEntities;
		exportScriptFileName = commandLine.exportScriptFileName;
		schema = commandLine.schema;
		qualifyNames = commandLine.qualifyNames;
		analyseAlias = commandLine.analyseAlias;
		analyseSynonym = commandLine.analyseSynonym;
		analyseView = commandLine.analyseView;
		deleteScriptFileName = commandLine.deleteScriptFileName;
		tabuFileName = commandLine.tabuFileName;
		where = commandLine.where;
		rawschemamapping = commandLine.rawschemamapping;
		rawsourceschemamapping = commandLine.rawsourceschemamapping;
		parameters = commandLine.parameters;
		numberOfThreads = commandLine.numberOfThreads;
		numberOfEntities = commandLine.numberOfEntities;
		upsertOnly = commandLine.upsertOnly;
		scope = commandLine.scope;
		workingTableSchema = commandLine.workingTableSchema;
		datamodelFolder = commandLine.datamodelFolder;
		workingFolder = commandLine.workingFolder;
		jdbcjar = commandLine.jdbcjar;
		jdbcjar2 = commandLine.jdbcjar2;
		noSorting = commandLine.noSorting;
		transactional = commandLine.transactional;
		noRowid = commandLine.noRowid;
		importFilterMappingTableSchema = commandLine.importFilterMappingTableSchema;
		scriptEnhancer = commandLine.scriptEnhancer;
	}

}