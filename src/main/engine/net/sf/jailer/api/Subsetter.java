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
package net.sf.jailer.api;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.Map;

import javax.sql.DataSource;

import org.xml.sax.SAXException;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.BasicDataSource;
import net.sf.jailer.database.WorkingTableScope;
import net.sf.jailer.subsetting.ExportStatistic;
import net.sf.jailer.subsetting.InconsistentSubsettingResultException;
import net.sf.jailer.subsetting.ScriptFormat;
import net.sf.jailer.subsetting.SubsettingEngine;

/**
 * Generates a subset of a relational database that respects foreign key constraints.
 *
 * @author Ralf Wisser
 */
public class Subsetter {

	/**
	 * Default constructor.
	 */
	public Subsetter() {
		executionContext = new ExecutionContext();
	}

	/**
	 * Copy constructor.
	 */
	public Subsetter(Subsetter other) {
		this.dataSource = other.dataSource;
		this.modelPoolSize = other.modelPoolSize;
		this.dbms = other.dbms;
		this.executionContext = new ExecutionContext(other.executionContext);
		this.extractionModelURL = other.extractionModelURL;
	}

	/**
	 * Creates a new Subsetter with all mandatory attributes.
	 *
	 * @param dataSource the data-source to connect with the source database
	 * @param dbms the DBMS of the source database
	 * @param dataModel URL of the current data model (the datamodel's base folder)
	 * @param extractionModel URL of the extraction model
	 * @param scriptFormat script format
	 */
	public Subsetter(
			DataSource dataSource,
			DBMS dbms,
			URL dataModel,
			URL extractionModel,
			ScriptFormat scriptFormat) {
		executionContext = new ExecutionContext();
		executionContext.setEmbedded(true);
		setDataSource(dataSource);
		setDbms(dbms);
		setDataModelURL(dataModel);
		setExtractionModelURL(extractionModel);
		setScriptFormat(scriptFormat);
	}

	/**
	 * Creates a new Subsetter with all mandatory attributes.
	 *
	 * @param dataSource the data-source to connect with the source database
	 * @param dbms the DBMS of the source database
	 * @param dataModel the current data model (the datamodel's base folder)
	 * @param extractionModel the extraction model
	 * @param scriptFormat script format
	 */
	public Subsetter(
			DataSource dataSource,
			DBMS dbms,
			File dataModel,
			File extractionModel,
			ScriptFormat scriptFormat) {
		executionContext = new ExecutionContext();
		executionContext.setEmbedded(true);
		setDataSource(dataSource);
		setDbms(dbms);
		try {
			setDataModelURL(dataModel.toURI().toURL());
			setExtractionModelURL(extractionModel.toURI().toURL());
		} catch (MalformedURLException e) {
			throw new RuntimeException(e);
		}
		setScriptFormat(scriptFormat);
	}

	/**
	 * Generates the export-script.
	 * @param whereClause if not <code>null</code>, overrides the extraction model's subject condition
	 * @param exportScriptFile the export-script file (compressed if it ends with '.zip' or '.gz')
	 *
	 * @return export statistic
	 *
	 * @throws InconsistentSubsettingResultException if {@link ExecutionContext#isAbortInCaseOfInconsistency()} and the number of exported rows differs from that of the collected ones
	 */
	public ExportStatistic execute(String whereClause, File exportScriptFile) throws SQLException, IOException {
		return execute(whereClause, exportScriptFile, null);
	}

	/**
	 * Generates the export- and/or delete-script.
	 *
	 * @param whereClause if not <code>null</code>, overrides the extraction model's subject condition
	 * @param exportScriptFile the export-script file (compressed if it ends with '.zip' or '.gz'), optional
	 * @param deleteScriptFile the delete-script file (compressed if it ends with '.zip' or '.gz'), optional
	 *
	 * @return export statistic
	 *
	 * @throws InconsistentSubsettingResultException if {@link ExecutionContext#isAbortInCaseOfInconsistency()} and the number of exported rows differs from that of the collected ones
	 */
	public ExportStatistic execute(String whereClause, File exportScriptFile, File deleteScriptFile) throws SQLException, IOException {
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
			return new SubsettingEngine(executionContext).export(
					whereClause,
					getExtractionModelURL(),
					exportScriptFile == null? null : exportScriptFile.getAbsolutePath(),
					deleteScriptFile == null? null : deleteScriptFile.getAbsolutePath(),
					getDataSource(), sourceDBMS, getScriptFormat(), getModelPoolSize());
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
	 * Sets the current data model as {@link File}
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
	 * Gets target-DBMS.
	 *
	 * @return target-DBMS
	 */
	public DBMS getTargetDBMS() {
		return executionContext.getTargetDBMS();
	}

	/**
	 * Sets target-DBMS.
	 *
	 * @param targetDBMS target-DBMS. If it's <code>null</code>, target DBMS is assumed to be the same as {@link #getDbms()}
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
	 * Gets number of parallel threads (default is 1) to be used.
	 *
	 * @return number of threads (default is 1)
	 */
	public int getNumberOfThreads() {
		return executionContext.getNumberOfThreads();
	}

	/**
	 * Sets number of parallel threads (default is 1) to be used
	 *
	 * @param numberOfThreads
	 *            number of threads
	 */
	public void setNumberOfThreads(int numberOfThreads) {
		executionContext.setNumberOfThreads(numberOfThreads);
	}

	/**
	 * If <code>true</code>, export rows in a single transaction. (default is false)
	 *
	 * @return <code>true</code> if Import rows in a single transaction
	 */
	public boolean getTransactional() {
		return executionContext.getTransactional();
	}

	/**
	 * If <code>true</code>, export rows in a single transaction. (default is false)
	 *
	 * @param transactional
	 *            <code>true</code> if import rows in a single transaction
	 */
	public void setTransactional(boolean transactional) {
		executionContext.setTransactional(transactional);
	}

	/**
	 * Gets IsolationLevel.
	 *
	 * @see Connection#setTransactionIsolation(int)
	 */
	public Integer getIsolationLevel() {
		return executionContext.getIsolationLevel();
	}

	/**
	 * Sets IsolationLevel.
	 *
	 * @see Connection#setTransactionIsolation(int)
	 */
	public void setIsolationLevel(Integer isolationLevel) {
		executionContext.setIsolationLevel(isolationLevel);
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
	 * Gets scope of working tables, GLOBAL, SESSION_LOCAL or LOCAL_DATABASE <br>
	 * Default is GLOBAL.
	 *
	 * @return scope of working tables, GLOBAL, SESSION_LOCAL or LOCAL_DATABASE
	 */
	public WorkingTableScope getScope() {
		return executionContext.getScope();
	}

	/**
	 * Sets scope of working tables, GLOBAL, SESSION_LOCAL or LOCAL_DATABASE
	 *
	 * @param scope
	 *            scope of working tables, GLOBAL, SESSION_LOCAL or
	 *            LOCAL_DATABASE
	 */
	public void setScope(WorkingTableScope scope) {
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
	 * @deprecated use {@link #getUseRowid()}
	 */
	public boolean getNoRowid() {
		return !getUseRowid();
	}

	/**
	 * @deprecated use {@link #setUseRowid(boolean)}
	 */
	public void setNoRowid(boolean noRowid) {
		setUseRowid(!noRowid);
	}

	/**
	 * If <code>true</code>, use rowid/ctid-column to determine row identity (instead
	 * of primary keys)
	 *
	 * @return if <code>true</code> use rowid/ctid-column to determine row identity
	 *         (instead of primary keys)
	 */
	public boolean getUseRowid() {
		return executionContext.getUseRowid();
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
		executionContext.setUseRowid(useRowid);
	}

	/**
	 * If <code>true</code>, use rowid/ctid-column only for tables without primary key.
	 *
	 * @return <code>true</code> if use rowid/ctid-column only for tables without primary key
	 *
	 * @see #getUseRowIdsOnlyForTablesWithoutPK()
	 */
	public boolean getUseRowIdsOnlyForTablesWithoutPK() {
		return executionContext.getUseRowIdsOnlyForTablesWithoutPK();
	}

	/**
	 * If <code>true</code>, use rowid/ctid-column only for tables without primary key.
	 *
	 * @param useRowIdsOnlyForTablesWithoutPK
	 *            <code>true</code> if use rowid/ctid-column only for tables without primary key
	 *
	 * @see #setUseRowid(boolean)
	 */
	public void setUseRowIdsOnlyForTablesWithoutPK(boolean useRowIdsOnlyForTablesWithoutPK) {
		executionContext.setUseRowIdsOnlyForTablesWithoutPK(useRowIdsOnlyForTablesWithoutPK);;
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
	 * Gets parameters.
	 *
	 * @return parameters
	 */
	public Map<String, String> getParameters() {
		return executionContext.getParameters();
	}

	/**
	 * Sets a parameter.
	 *
	 * @param name parameter name
	 * @param value value
	 */
	public void setParameter(String name, String value) {
		executionContext.setParameter(name, value);
	}

	/**
	 * Gets schema map for destination database.
	 *
	 * @return schema map
	 */
	public Map<String, String> getSchemaMapping() {
		return executionContext.getSchemaMapping();
	}

	/**
	 * Sets schema map for destination database.
	 *
	 * @param schemaMapping schema map
	 */
	public void setSchemaMapping(Map<String, String> schemaMapping) {
		executionContext.setSchemaMapping(schemaMapping);
	}

	/**
	 * Sets schema map for source database.
	 *
	 * @param sourceSchemaMapping the sourceSchemaMapping to set
	 */
	public void setSourceSchemaMapping(Map<String, String> sourceSchemaMapping) {
		executionContext.setSourceSchemaMapping(sourceSchemaMapping);
	}

	/**
	 * Gets schema map for source database.
	 *
	 * @return the sourceSchemaMapping to set
	 */
	public Map<String, String> getSourceSchemaMapping() {
		return executionContext.getSourceSchemaMapping();
	}

	/**
	 * @return if <code>true</code>, the Subsetter throws an exception if the result is inconsistent due to insufficient transaction isolation
	 */
	public boolean isAbortInCaseOfInconsistency() {
		return executionContext.isAbortInCaseOfInconsistency();
	}

	/**
	 * @param abortInCaseOfInconsitency if <code>true</code>, abort the process if the result is inconsistent due to insufficient transaction isolation
	 */
	public void setAbortInCaseOfInconsistency(boolean abortInCaseOfInconsitency) {
		executionContext.setAbortInCaseOfInconsistency(abortInCaseOfInconsitency);
	}

	/**
	 * @return the size of extraction-model pool (default is 10)
	 */
	public int getModelPoolSize() {
		return modelPoolSize;
	}

	/**
	 * @param modelPoolSize size of extraction-model pool (default is 10)
	 */
	public void setModelPoolSize(int modelPoolSize) {
		this.modelPoolSize = modelPoolSize;
	}

	/**
	 * @return the folder where the local database will be stored. If <code>null</code>, default temp folder is used.
	 */
	public String getLocalDatabaseStorage() {
		return executionContext.getLocalDatabaseStorage();
	}

	/**
	 * Sets the folder where the local database will be stored.
	 * 
	 * @param localDatabaseStorage the folder where the local database will be stored. If <code>null</code>, default temp folder is used.
	 */
	public void setLocalDatabaseStorage(File localDatabaseStorage) {
		executionContext.setLocalDatabaseStorage(localDatabaseStorage != null? localDatabaseStorage.getPath() : null);
	}

	/**
	 * Sets the folder where the local database will be stored.
	 * 
	 * @param localDatabaseStorage the folder where the local database will be stored. If <code>null</code>, default temp folder is used.
	 */
	public void setLocalDatabaseStorage(String localDatabaseStorage) {
		executionContext.setLocalDatabaseStorage(localDatabaseStorage);
	}

	/**
	 * Gets the {@link ExecutionContext}. <br>
	 * Use this to set parameters that are not accessible via this facade.
	 *
	 * @return the {@link ExecutionContext}
	 */
	public ExecutionContext getExecutionContext() {
		return executionContext;
	}

	private final ExecutionContext executionContext;

	private int modelPoolSize = 10;
	private URL extractionModelURL;
	private DataSource dataSource;
	private DBMS dbms;

}
