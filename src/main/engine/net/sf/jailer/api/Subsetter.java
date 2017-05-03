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
package net.sf.jailer.api;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.sql.SQLException;
import java.util.Map;

import javax.sql.DataSource;

import org.xml.sax.SAXException;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.BasicDataSource;
import net.sf.jailer.database.TemporaryTableScope;
import net.sf.jailer.subsetting.ScriptFormat;
import net.sf.jailer.subsetting.SubsettingEngine;

/**
 * Generates a subset of a relational database that respects foreign key constraints. <br>
 * 
 * @author Ralf Wisser
 */
public class Subsetter {
	
	/**
	 * Generates the export- and/or delete-script.
	 */
	public void execute() throws SQLException, IOException {
		File exportScriptFile = getExportScriptFile();
		File deleteScriptFile = getDeleteScriptFile();
		try {
			if (getDataModelURL() == null) {
				throw new IllegalStateException("missing DataModelURL");
			}
			if (getExtractionModelURL() == null) {
				throw new IllegalStateException("missing ExtractionModelURL");
			}
			if (getDataSource() == null) {
				throw new IllegalStateException("missing DataSource");
			}
			if (getScriptFormat() == null) {
				throw new IllegalStateException("missing ScriptFormat");
			}
			DBMS sourceDBMS = getDbms();
			if (sourceDBMS == null) {
				if (getDataSource() instanceof BasicDataSource) {
					sourceDBMS = ((BasicDataSource) getDataSource()).dbms;
				} else {
					throw new IllegalStateException("no DBMS set but data-source is not net.sf.jailer.database.BasicDataSource");
				}
			}
			new SubsettingEngine(executionContext).export(
					getExtractionModelURL(), 
					exportScriptFile == null? null : exportScriptFile.getAbsolutePath(),
					deleteScriptFile == null? null : deleteScriptFile.getAbsolutePath(),
					getDataSource(), sourceDBMS, false, getScriptFormat());
		} catch (SAXException e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * Gets the data-source to connect with the database.
	 * 
	 * @return the data-source to connect with the database
	 */
	public DataSource getDataSource() {
		return dataSource;
	}

	/**
	 * Sets the data-source to connect with the database.
	 * 
	 * @param dataSource the data-source to connect with the database
	 */
	public void setDataSource(DataSource dataSource) {
		this.dataSource = dataSource;
	}

	/**
	 * Gets the DBMS of the database.
	 * 
	 * @return the DBMS
	 */
	public DBMS getDbms() {
		return dbms;
	}

	/**
	 * Sets the DBMS of the database. <br>
	 * Note that it's not necessary to set the DBMS if {@link BasicDataSource} is used.
	 * 
	 * @param dbms the DBMS
	 */
	public void setDbms(DBMS dbms) {
		this.dbms = dbms;
	}

	/**
     * Gets URL of the current data model (the datamodel's base folder)
     */
	public URL getDataModelURL() {
		return executionContext.getDataModelURL();
	}
	
	/**
     * Sets URL of the current data model (the datamodel's base folder)
     */
	public void setDataModelURL(URL datamodelURL) {
		executionContext.setDataModelURL(datamodelURL);
	}

	/**
	 * Gets the URL of the extraction model.
	 * 
	 * @return the URL of the extraction model
	 */
	public URL getExtractionModelURL() {
		return extractionModelURL;
	}

	/**
	 * Sets the URL of the extraction model.
	 * 
	 * @param extractionModelURL the URL of the extraction model
	 */
	public void setExtractionModelURL(URL extractionModelURL) {
		this.extractionModelURL = extractionModelURL;
	}

    /**
     * Gets the script format.
     * 
     * @return the script format
     */
    public ScriptFormat getScriptFormat() {
    	return executionContext.getScriptFormat();
    }
    
    /**
     * Sets the script format.
     * 
     * @return the script format
     */
    public void setScriptFormat(ScriptFormat scriptFormat) {
    	executionContext.setScriptFormat(scriptFormat);
    }
    
	/**
	 * If <code>true</code>, Use uTF-8 encoding
	 *
	 * @return <code>true</code> if use UTF-8 encoding
	 */
	public boolean getUTF8() {
		return executionContext.getUTF8();
	}

	/**
	 * If <code>true</code>, use UTF-8 encoding
	 *
	 * @param uTF8
	 *            <code>true</code> if use UTF-8 encoding
	 */
	public void setUTF8(boolean uTF8) {
		executionContext.setUTF8(uTF8);
	}

	/**
	 * Gets target-DBMS: ORACLE, MSSQL, DB2, MySQL, POSTGRESQL, SYBASE, SQLITE,
	 * HSQL or H2
	 *
	 * @return target-DBMS: ORACLE, MSSQL, DB2, MySQL, POSTGRESQL, SYBASE,
	 *         SQLITE, HSQL or H2
	 */
	public DBMS getTargetDBMS() {
		return executionContext.getTargetDBMS();
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
		executionContext.setTargetDBMS(targetDBMS);
	}

	/**
	 * Gets root tag of XML export file
	 *
	 * @return root tag of XML export file
	 */
	public String getXmlRootTag() {
		return executionContext.getXmlRootTag();
	}

	/**
	 * Sets root tag of XML export file
	 *
	 * @param xmlRootTag
	 *            root tag of XML export file
	 */
	public void setXmlRootTag(String xmlRootTag) {
		executionContext.setXmlRootTag(xmlRootTag);
	}

	/**
	 * Gets pattern for dates in XML and LIQUIBASE_XML export file
	 *
	 * @return pattern for dates in XML and LIQUIBASE_XML export file
	 */
	public String getXmlDatePattern() {
		return executionContext.getXmlDatePattern();
	}

	/**
	 * Sets pattern for dates in XML and LIQUIBASE_XML export file
	 *
	 * @param xmlDatePattern
	 *            pattern for dates in XML and LIQUIBASE_XML export file
	 */
	public void setXmlDatePattern(String xmlDatePattern) {
		executionContext.setXmlDatePattern(xmlDatePattern);
	}

	/**
	 * Gets pattern for times in XML and LIQUIBASE_XML export file
	 *
	 * @return pattern for times in XML and LIQUIBASE_XML export file
	 */
	public String getXmlTimePattern() {
		return executionContext.getXmlTimePattern();
	}

	/**
	 * Sets pattern for times in XML and LIQUIBASE_XML export file
	 *
	 * @param xmlTimePattern
	 *            pattern for times in XML and LIQUIBASE_XML export file
	 */
	public void setXmlTimePattern(String xmlTimePattern) {
		executionContext.setXmlTimePattern(xmlTimePattern);
	}

	/**
	 * Gets pattern for time-stamps in XML and LIQUIBASE_XML export file
	 *
	 * @return pattern for time-stamps in XML and LIQUIBASE_XML export file
	 */
	public String getXmlTimeStampPattern() {
		return executionContext.getXmlTimeStampPattern();
	}

	/**
	 * Sets pattern for time-stamps in XML and LIQUIBASE_XML export file
	 *
	 * @param xmlTimeStampPattern
	 *            pattern for time-stamps in XML and LIQUIBASE_XML export file
	 */
	public void setXmlTimeStampPattern(String xmlTimeStampPattern) {
		executionContext.setXmlTimeStampPattern(xmlTimeStampPattern);
	}

	/**
	 * Gets the export-script file (compressed if it ends with '.zip' or
	 * '.gz')
	 *
	 * @return the export-script file (compressed if it ends with '.zip'
	 *         or '.gz')
	 */
	public File getExportScriptFile() {
		String name = executionContext.getExportScriptFileName();
		if (name == null) {
			return null;
		}
		return new File(name);
	}

	/**
	 * Sets the export-script file (compressed if it ends with '.zip' or
	 * '.gz')
	 *
	 * @param exportScriptFileName
	 *            the export-script file (compressed if it ends with
	 *            '.zip' or '.gz')
	 */
	public void setExportScriptFile(File exportScriptFile) {
		executionContext.setExportScriptFileName(exportScriptFile == null? null : exportScriptFile.getPath());
	}

	/**
	 * Gets the delete-script file (compressed if it ends with '.zip' or
	 * '.gz')
	 *
	 * @return the delete-script file (compressed if it ends with '.zip'
	 *         or '.gz')
	 */
	public File getDeleteScriptFile() {
		String name = executionContext.getDeleteScriptFileName();
		if (name == null) {
			return null;
		}
		return new File(name);
	}

	/**
	 * Sets the delete-script file (compressed if it ends with '.zip' or
	 * '.gz')
	 *
	 * @param deleteScriptFileName
	 *            the delete-script file (compressed if it ends with
	 *            '.zip' or '.gz')
	 */
	public void setDeleteScriptFile(File deleteScriptFile) {
		executionContext.setDeleteScriptFileName(deleteScriptFile == null? null : deleteScriptFile.getPath());
	}

	/**
	 * If <code>true</code>, Add schema prefix to table names after analysing
	 * the DB
	 *
	 * @return <code>true</code> if Add schema prefix to table names after
	 *         analysing the DB
	 */
	public boolean getQualifyNames() {
		return executionContext.getQualifyNames();
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
		executionContext.setQualifyNames(qualifyNames);
	}

	/**
	 * If <code>true</code>, Look for aliases while analysing the DB
	 *
	 * @return <code>true</code> if Look for aliases while analysing the DB
	 */
	public boolean getAnalyseAlias() {
		return executionContext.getAnalyseAlias();
	}

	/**
	 * If <code>true</code>, Look for aliases while analysing the DB
	 *
	 * @param analyseAlias
	 *            <code>true</code> if Look for aliases while analysing the DB
	 */
	public void setAnalyseAlias(boolean analyseAlias) {
		executionContext.setAnalyseAlias(analyseAlias);
	}

	/**
	 * If <code>true</code>, Look for synonyms while analysing the DB
	 *
	 * @return <code>true</code> if Look for synonyms while analysing the DB
	 */
	public boolean getAnalyseSynonym() {
		return executionContext.getAnalyseSynonym();
	}

	/**
	 * If <code>true</code>, Look for synonyms while analysing the DB
	 *
	 * @param analyseSynonym
	 *            <code>true</code> if Look for synonyms while analysing the DB
	 */
	public void setAnalyseSynonym(boolean analyseSynonym) {
		executionContext.setAnalyseSynonym(analyseSynonym);
	}

	/**
	 * If <code>true</code>, Look for views while analysing the DB
	 *
	 * @return <code>true</code> if Look for views while analysing the DB
	 */
	public boolean getAnalyseView() {
		return executionContext.getAnalyseView();
	}

	/**
	 * If <code>true</code>, Look for views while analysing the DB
	 *
	 * @param analyseView
	 *            <code>true</code> if Look for views while analysing the DB
	 */
	public void setAnalyseView(boolean analyseView) {
		executionContext.setAnalyseView(analyseView);
	}

	/**
	 * Gets subject condition
	 *
	 * @return subject condition
	 */
	public String getWhere() {
		return executionContext.getWhere();
	}

	/**
	 * Sets subject condition
	 *
	 * @param where
	 *            subject condition
	 */
	public void setWhere(String where) {
		executionContext.setWhere(where);
	}

	/**
	 * Gets number of threads (default is 1)
	 *
	 * @return number of threads (default is 1)
	 */
	public int getNumberOfThreads() {
		return executionContext.getNumberOfThreads();
	}

	/**
	 * Sets number of threads (default is 1)
	 *
	 * @param numberOfThreads
	 *            number of threads (default is 1)
	 */
	public void setNumberOfThreads(int numberOfThreads) {
		executionContext.setNumberOfThreads(numberOfThreads);
	}

	/**
	 * Gets maximum number of entities per insert-statement (in export-file,
	 * default is 10)
	 *
	 * @return maximum number of entities per insert-statement (in export-file,
	 *         default is 10)
	 */
	public int getNumberOfEntities() {
		return executionContext.getNumberOfEntities();
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
		executionContext.setNumberOfEntities(numberOfEntities);
	}

	/**
	 * If <code>true</code>, Generate 'upsert'-statements for all entities (in
	 * export-file)
	 *
	 * @return <code>true</code> if Generate 'upsert'-statements for all
	 *         entities (in export-file)
	 */
	public boolean getUpsertOnly() {
		return executionContext.getUpsertOnly();
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
		executionContext.setUpsertOnly(upsertOnly);
	}

	/**
	 * Gets scope of working tables, GLOBAL, SESSION_LOCAL or LOCAL_DATABASE
	 *
	 * @return scope of working tables, GLOBAL, SESSION_LOCAL or LOCAL_DATABASE
	 */
	public TemporaryTableScope getScope() {
		return executionContext.getScope();
	}

	/**
	 * Sets scope of working tables, GLOBAL, SESSION_LOCAL or LOCAL_DATABASE
	 *
	 * @param scope
	 *            scope of working tables, GLOBAL, SESSION_LOCAL or
	 *            LOCAL_DATABASE
	 */
	public void setScope(TemporaryTableScope scope) {
		executionContext.setScope(scope);
	}

	/**
	 * Gets schema in which the working tables will be created
	 *
	 * @return schema in which the working tables will be created
	 */
	public String getWorkingTableSchema() {
		return executionContext.getWorkingTableSchema();
	}

	/**
	 * Sets schema in which the working tables will be created
	 *
	 * @param workingTableSchema
	 *            schema in which the working tables will be created
	 */
	public void setWorkingTableSchema(String workingTableSchema) {
		executionContext.setWorkingTableSchema(workingTableSchema);
	}

	/**
     * Sets the current data model base folder.
     * 
     * @param datamodelBaseFolder represents the folder. Will be converted to an URL an set as datamodel URL
     * 
     * @see #setDataModelURL(URL)
     */
	public void setDataModelBaseFolder(File datamodelBaseFolder) {
		try {
			setDataModelURL(datamodelBaseFolder.toURI().toURL());
		} catch (MalformedURLException e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * Gets the working folder. Defaults to '.'
	 *
	 * @return the working folder. Defaults to '.'
	 */
	public String getWorkingFolder() {
		return executionContext.getWorkingFolder();
	}

	/**
	 * Sets the working folder. Defaults to '.'
	 *
	 * @param workingFolder
	 *            the working folder. Defaults to '.'
	 */
	public void setWorkingFolder(String workingFolder) {
		executionContext.setWorkingFolder(workingFolder);
	}

	/**
	 * Gets the temporary files folder. Defaults to 'tmp'.
	 * 
	 * @return the tempFileFolder absolute or relative to {@link #getWorkingFolder()}
	 */
	public String getTempFileFolder() {
		return executionContext.getTempFileFolder();
	}

	/**
	 * Sets the temporary files folder. Defaults to 'tmp'.
	 * 
	 * @param tempFileFolder absolute or relative to {@link #getWorkingFolder()}
	 */
	public void setTempFileFolder(String tempFileFolder) {
		executionContext.setTempFileFolder(tempFileFolder);
	}

	/**
	 * If <code>true</code>, The exported rows will not be sorted according to
	 * foreign key constraints
	 *
	 * @return <code>true</code> if The exported rows will not be sorted
	 *         according to foreign key constraints
	 */
	public boolean getNoSorting() {
		return executionContext.getNoSorting();
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
		executionContext.setNoSorting(noSorting);
	}

	/**
	 * If <code>true</code>, Import rows in a single transaction
	 *
	 * @return <code>true</code> if Import rows in a single transaction
	 */
	public boolean getTransactional() {
		return executionContext.getTransactional();
	}

	/**
	 * If <code>true</code>, Import rows in a single transaction
	 *
	 * @param transactional
	 *            <code>true</code> if Import rows in a single transaction
	 */
	public void setTransactional(boolean transactional) {
		executionContext.setTransactional(transactional);
	}

	/**
	 * If <code>true</code>, Use primary keys to determine row identity (instead
	 * of rowid-column)
	 *
	 * @return <code>true</code> if Use primary keys to determine row identity
	 *         (instead of rowid-column)
	 */
	public boolean getNoRowid() {
		return executionContext.getNoRowid();
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
		executionContext.setNoRowid(noRowid);
	}

	/**
	 * Gets schema in which the import-filter mapping tables will be created
	 *
	 * @return schema in which the import-filter mapping tables will be created
	 */
	public String getImportFilterMappingTableSchema() {
		return executionContext.getImportFilterMappingTableSchema();
	}

	/**
	 * Sets schema in which the import-filter mapping tables will be created
	 *
	 * @param importFilterMappingTableSchema
	 *            schema in which the import-filter mapping tables will be
	 *            created
	 */
	public void setImportFilterMappingTableSchema(String importFilterMappingTableSchema) {
		executionContext.setImportFilterMappingTableSchema(importFilterMappingTableSchema);
	}

	/**
	 * Gets parameters
	 *
	 * @return parameters
	 */
    public Map<String, String> getParameters() {
    	return executionContext.getParameters();
    }

    public Map<String, String> getSchemaMapping() {
    	return executionContext.getSchemaMapping();
    }

	/**
	 * Sets source schema map
	 *
	 * @param rawsourceschemamapping
	 *            source schema map
	 */
	public void setSchemaMapping(Map<String, String> schemaMapping) {
		executionContext.setSchemaMapping(schemaMapping);
	}

    /**
	 * @param sourceSchemaMapping the sourceSchemaMapping to set
	 */
	public void setSourceSchemaMapping(Map<String, String> sourceSchemaMapping) {
		executionContext.setSourceSchemaMapping(sourceSchemaMapping);
	}

	public Map<String, String> getSourceSchemaMapping() {
		return executionContext.getSourceSchemaMapping();
	}
    
    /**
     * Sets folder of current data model.
     * 
     * @param modelFolder the folder, <code>null</code> for default model
     */
	public void setCurrentModelSubfolder(String modelFolder) {
		executionContext.setCurrentModelSubfolder(modelFolder);
	}

	/**
     * Gets folder of current data model.
     * 
     * @return modelFolder the folder, <code>null</code> for default model
     */
	public String getCurrentModelSubfolder() {
		return executionContext.getCurrentModelSubfolder();
	}

	private final ExecutionContext executionContext = new ExecutionContext();
	
	private URL extractionModelURL;
	private DataSource dataSource;
	private DBMS dbms;

}
