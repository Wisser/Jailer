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

	@Option(name="-schemamapping",usage="schema mapping (Default schema is empty string)", metaVar="schema-in-model=schema-in-db[','x=y]*")
	public String rawschemamapping = null;

	@Option(name="-source-schemamapping",usage="source schema mapping (Default schema is empty string)", metaVar="<from>=<to>[','<from>=<to>]*")
	public String rawsourceschemamapping = null;

	@Option(name="-deletion-schemamapping",usage="deletion schema mapping (Default schema is empty string)", metaVar="<from>=<to>[','<from>=<to>]*")
	public String rawdeletionschemamapping = null;

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

	@Option(name="-jdbcjar", usage="JDBC driver jar file")
	public String jdbcjar = null;

	@Option(name="-jdbcjar2", usage="JDBC driver additional jar file")
	public String jdbcjar2 = null;

	@Option(name="-jdbcjar3", usage="JDBC driver 2. additional jar file")
	public String jdbcjar3 = null;

	@Option(name="-jdbcjar4", usage="JDBC driver 3. additional jar file")
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

	@Option(name="-no-rowid", usage="(no longer used, see \"-use-rowid\")")
	public boolean noRowidNoLongerUsed = false;

	@Option(name="-use-rowid", usage="use rowid/ctid-column to identify rows (instead of primary keys, see \"-use-rowid-if-needed\")")
	public boolean useRowid = false;

	@Option(name="-use-rowid-if-needed", usage="use rowid/ctid-column only for tables without primary key (see \"-use-rowid\")")
	public boolean useRowIdsOnlyForTablesWithoutPK = false;

	@Option(name="-import-filter-mapping-table-schema", usage="schema in which the import-filter mapping tables will be created")
	public String importFilterMappingTableSchema = "";

	@Option(name="-check-primary-keys", usage="(no longer used)")
	boolean checkPrimaryKeysNoLongerUsed = false;

	@Option(name="-limit-transaction-size", usage="collects the rows using multiple insert operations with a limited number of rows per operation")
	boolean insertIncrementally = false;

	@Option(name="-abortInCaseOfInconsistency", usage="abort the process if the result is inconsistent due to insufficient transaction isolation")
	boolean abortInCaseOfInconsistency = false;

	@Option(name="-row-limit", usage="maximum allowed number of exported rows. If this limit is exceeded, the export aborts with an error.")
	public String limit = null;

	@Option(name="-local-database-storage", usage="the folder where the local database will be stored. \nDefault temp folder is used if this is not specified.")
	public String localDatabaseStorage = null;

	@Option(name="-", usage="do not interpret the next word as an option, even if it begins with a '-'. E.g. if the username is: \"-abc\", use: \"- -abc\".")
	public List<String> escapedWords = new ArrayList<String>();

	@Option(name="-file-lookup", usage="read the next parameter from the (1st line of the) file named VAL. \n(This is especially useful for not making passwords visible by querying the command line parameters)")
	public List<String> parameterFile = new ArrayList<String>();
	
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
