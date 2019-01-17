/*
 * Copyright 2007 - 2019 the original author or authors.
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

import java.util.ArrayList;
import java.util.List;

import org.kohsuke.args4j.Argument;
import org.kohsuke.args4j.Option;

import net.sf.jailer.subsetting.ScriptFormat;

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
	
	@Option(name="-m", usage="no longer used")
	public int maxNumberOfEntities = 0;
	
	@Option(name="-script-enhancer", usage="no longer used")
	public String scriptEnhancer = "";

	@Option(name="-t", usage="no longer used")
	public String tabu = "";

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

	@Option(name="-jdbcjar", usage="JDBC driver's jar file")
	public String jdbcjar = null;

	@Option(name="-jdbcjar2", usage="JDBC driver's additional jar file")
	public String jdbcjar2 = null;

	@Option(name="-jdbcjar3", usage="JDBC driver's 2. additional jar file")
	public String jdbcjar3 = null;

	@Option(name="-jdbcjar4", usage="JDBC driver's 3. additional jar file")
	public String jdbcjar4 = null;

	@Option(name="-no-sorting", usage="the exported rows will not be sorted according to foreign key constraints")
	public boolean noSorting = false;

	@Option(name="-order-by-pk", usage="Orders the exported rows according to the primary key.")
	public boolean orderByPK = false;

	@Option(name="-independent-working-tables", usage="create working tables that are independent of the extraction model. (Potentially less efficient)")
	public boolean independentWorkingTables = false;
	
	@Option(name="-transactional", usage="import rows in a single transaction")
	public boolean transactional = false;
	
	@Option(name="-isolation-level", usage="isolation level (optional), 1=READ_UNCOMMITTED, 2=READ_COMMITTED, 4=REPEATABLE_READ, 8=SERIALIZABLE")
	public Integer isolationLevel = null;
	
	@Option(name="-no-rowid", usage="use primary keys to determine row identity (instead of rowid-column)")
	public boolean noRowid = false;
	
	@Option(name="-import-filter-mapping-table-schema", usage="schema in which the import-filter mapping tables will be created")
	public String importFilterMappingTableSchema = "";
	
	@Option(name="-check-primary-keys", usage="should the PKs be checked for validity?")
	boolean checkPrimaryKeys = false;

	@Option(name="-limit-transaction-size", usage="collects the rows using multiple insert operations with a limited number of rows per operation")
	boolean insertIncrementally = false;
	
	@Argument
	public List<String> arguments = new ArrayList<String>();

	/**
	 * Gets the script format.
	 * 
	 * @return the script format
	 */
	public ScriptFormat getScriptFormat() {
		if (_asXml) {
			return ScriptFormat.XML;
		} else if (format != null) {
			return ScriptFormat.valueOf(format);
		}
		return null;
	}
	
}
