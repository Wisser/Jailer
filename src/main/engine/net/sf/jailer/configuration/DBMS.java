/*
 * Copyright 2007 - 2026 Ralf Wisser.
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
package net.sf.jailer.configuration;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import com.fasterxml.jackson.annotation.JsonPropertyOrder;

import net.sf.jailer.database.DefaultTemporaryTableManager;
import net.sf.jailer.database.SQLDialect;
import net.sf.jailer.database.SqlScriptBasedStatisticRenovator;

/**
 * Describes a specific DBMS.
 *
 * @author Ralf Wisser
 */
@JsonPropertyOrder({
	"id",
	"displayName",
	"urlPattern",
	"sqlDialect",
	"statisticRenovator",
	"jdbcProperties",
	"stringLiteralEscapeSequences",
	"sqlLimitSuffix",
	"tableProperties",
	"sessionTemporaryTableManager",
	"ncharPrefix",
	"exportBlocks",
	"typeReplacement",
	"clobTypesRE",
	"nClobTypesRE",
	"blobTypesRE",
	"identityInserts",
	"timestampPattern",
	"timestampWithNanoTypeName",
	"timestampWithNanoPattern",
	"emptyBLOBValue",
	"emptyCLOBValue",
	"emptyNCLOBValue",
	"toBlob",
	"toClob",
	"toNClob",
	"embeddedLobSizeLimit",
	"binaryPattern",
	"avoidLeftJoin",
	"supportsSchemasInIndexDefinitions",
	"useInlineViewsInDataBrowser",
	"limitTransactionSize",
	"estimatedRowCountQuery",
	"defaultSchemaQuery",
	"viewTextOrDDLQuery",
	"virtualColumnsQuery",
	"userDefinedColumnsQuery",
	"nullableContraint",
	"identifierQuoteString",
	"explainPrepare",
	"explainQuery",
	"explainCleanup",
	"constraintsQuery"
})
public class DBMS {

	// predefined DBMSes
	public static final DBMS ORACLE;
	public static final DBMS MSSQL;
	public static final DBMS DB2;
	public static final DBMS DB2_ZOS;
	public static final DBMS MySQL;
	public static final DBMS POSTGRESQL;
	public static final DBMS SQLITE;
	public static final DBMS HSQL;
	public static final DBMS H2;
	public static final DBMS SYBASE;
	public static final DBMS INFORMIX;
	public static final DBMS CLOADSCAPE;
	public static final DBMS FIREBIRD;
	public static final DBMS DERBY;
	public static final DBMS CLICKHOUSE;

	/**
	 * Gets all DBMSes.
	 *
	 * @return array of all DBMSes
	 */
	public static DBMS[] values() {
		return Configuration.getInstance().getDBMS().toArray(new DBMS[0]);
	}

	/**
	 * Default constructor.
	 */
	public DBMS() {
	}

	/**
	 * Gets DBMS specific configuration.
	 *
	 * @param dbmsId the DBMS id
	 * @return the DBMS with given id, or the default DBMS if id is <code>null</code>
	 */
	public static synchronized DBMS forDBMS(String dbmsId) {
		if (dbmsId == null) {
			return defaultDBMS;
		}
		if (perDBMS.containsKey(dbmsId)) {
			return perDBMS.get(dbmsId);
		}
		List<DBMS> cs = Configuration.getInstance().getDBMS();
		for (DBMS c: cs) {
			if (dbmsId.equals(c.getId())) {
				perDBMS.put(dbmsId, c);
				return c;
			}
		}
		throw new RuntimeException("Unknown DBMS: \"" + dbmsId + "\"");
	}

	/**
	 * Holds configurations.
	 */
	private static Map<String, DBMS> perDBMS = new HashMap<String, DBMS>();

	/**
	 * Default configuration for unknown DBMS.
	 */
	private static final DBMS defaultDBMS = new DBMS();

	static {
		ORACLE = forDBMS("ORACLE");
		MSSQL = forDBMS("MSSQL");
		DB2 = forDBMS("DB2");
		DB2_ZOS = forDBMS("DB2_ZOS");
		MySQL = forDBMS("MySQL");
		POSTGRESQL = forDBMS("POSTGRESQL");
		SQLITE = forDBMS("SQLITE");
		HSQL = forDBMS("HSQL");
		H2 = forDBMS("H2");
		SYBASE = forDBMS("SYBASE");
		INFORMIX = forDBMS("INFORMIX");
		CLOADSCAPE = forDBMS("CLOADSCAPE");
		FIREBIRD = forDBMS("FIREBIRD");
		DERBY = forDBMS("DERBY");
		CLICKHOUSE = forDBMS("ClickHouse");
	}

	private String id;
	private String familyId;
	private String displayName;
	private String icon;

	/**
	 * DB-URL pattern of DBMS for which this holds the configuration.
	 */
	private String urlPattern;

	/**
	 * Test-query for the DBMS for which this holds the configuration.
	 */
	private String testQuery;

	/**
	 * The {@link SqlScriptBasedStatisticRenovator}.
	 */
	private SqlScriptBasedStatisticRenovator statisticRenovator;

	/**
	 * Replacement map for column types used for DDL generation.
	 */
	private Map<String, String> typeReplacement = new HashMap<String, String>();

	private Map<String, String> sqlExpressionRule = new HashMap<String, String>();

	private String liquibaseProductName;
	
	/**
	 * Gets the Liquibase product name for this DBMS.
	 *
	 * @return the Liquibase product name
	 */
	public String getLiquibaseProductName() {
		return liquibaseProductName;
	}

	/**
	 * Sets the Liquibase product name for this DBMS.
	 *
	 * @param liquibaseProductName the Liquibase product name to set
	 */
	public void setLiquibaseProductName(String liquibaseProductName) {
		this.liquibaseProductName = liquibaseProductName;
	}

	/**
	 * Replacement map for column types used for DDL generation. Only used if DBMS accept it.
	 */
	private Map<String, String> experimentalTypeReplacement = new HashMap<String, String>();

	/**
	 * Replacement map for special characters in string literals.
	 */
	private Map<String, String> stringLiteralEscapeSequences;

	/**
	 * Suffix of SQL-Select statement to limit number of rows.
	 */
	private String sqlLimitSuffix;

	private Integer varcharLengthLimit = null;

	private String tableProperties = "";

	/**
	 * DB-Query to get DDL of a table
	 */
	private String ddlQuery;

	/**
	 * DB-Call to get DDL of a table
	 */
	private String ddlCall;

	/**
	 * Query to get constraints
	 */
	private String constraintsQuery;

	/**
	 * Gets the DB query to retrieve the DDL of a table.
	 *
	 * @return the DDL query
	 */
	public String getDdlQuery() {
		return ddlQuery;
	}

	/**
	 * Sets the DB query to retrieve the DDL of a table.
	 *
	 * @param ddlQuery the DDL query to set
	 */
	public void setDdlQuery(String ddlQuery) {
		this.ddlQuery = ddlQuery;
	}

	/**
	 * Gets the DB call to retrieve the DDL of a table.
	 *
	 * @return the DDL call
	 */
	public String getDdlCall() {
		return ddlCall;
	}

	/**
	 * Sets the DB call to retrieve the DDL of a table.
	 *
	 * @param ddlCall the DDL call to set
	 */
	public void setDdlCall(String ddlCall) {
		this.ddlCall = ddlCall;
	}

	/**
	 * Maps characters to escape sequences according to {@link #stringLiteralEscapeSequences}.
	 */
	private Map<Character, String> charToEscapeSequence = new HashMap<Character, String>();
	{ charToEscapeSequence.put('\'', "''"); }
	private char[] keysOfCharToEscapeSequence = new char[] { '\'' };
	private String ncharPrefix = null;

	/**
	 * Set of type names for which no data must be exported.
	 */
	private Set<String> exportBlocks = new HashSet<String>();

	/**
	 * <code>true</code> if DBMS supports identity-type (MS-SQL)
	 */
	private boolean identityInserts = false;

	private String emptyCLOBValue = null;
	private String emptyNCLOBValue = null;
	private String emptyBLOBValue = null;
	private String toBlob;
	private String toClob;
	private String toNClob;
	private int embeddedLobSizeLimit = 3980;
	private String binaryPattern = "x'%s'";
	private boolean avoidLeftJoin = false;
	private String timestampPattern = null;
	private String timestampWithNanoTypeName = null;
	private String timestampWithNanoPattern = null;
	private String datePattern = null;
	private ThreadLocal<SimpleDateFormat> timestampFormat = new ThreadLocal<SimpleDateFormat>();
	private ThreadLocal<SimpleDateFormat> timestampWithNanoFormat = new ThreadLocal<SimpleDateFormat>();
	private ThreadLocal<SimpleDateFormat> dateFormat = new ThreadLocal<SimpleDateFormat>();
	private SQLDialect sqlDialect = new SQLDialect();
	private String rowidName = null;
	private Boolean supportsSchemasInIndexDefinitions = null;
	private boolean useInlineViewsInDataBrowser = true;
	private String virtualColumnsQuery = null;
	private String identityColumnsQuery = null;
	private String partitionsQuery = null;
	private String synonymTableQuery;
	private String viewTextOrDDLQuery = "SELECT VIEW_DEFINITION FROM INFORMATION_SCHEMA.VIEWS WHERE TABLE_SCHEMA = '%1$s' and TABLE_NAME = '%2$s'";
	private String estimatedRowCountQuery = null;
	private String userDefinedColumnsQuery = null;
	private String importedKeysQuery = null;
	private String primaryKeysQuery = null;
	private String indexInfoQuery = null;
	private String identifierQuoteString = "\"";

	private String explainCreateExplainTable = null;
	private String explainPrepare = null;
	private String explainQuery = null;
	private String explainCleanup = null;

	private String functionSourceQuery;
	private String procedureSourceQuery;
	private String packageSourceQuery;
	private String packageNamesQuery;
	private String defaultSchemaQuery;

	private Integer fetchSize = null;

	private List<DatabaseObjectRenderingDescription> objectRenderers = new ArrayList<DatabaseObjectRenderingDescription>();
	private boolean procedureDetailNeedsSpecificName = false;

	private LimitTransactionSizeInfo limitTransactionSize = new LimitTransactionSizeInfo();

	private String clobTypesRE;
	private String nClobTypesRE;
	private String blobTypesRE;
	private Pattern clobTypesPattern;
	private Pattern nClobTypesPattern;
	private Pattern blobTypesPattern;
	
	private boolean usesCatalog;
	
	/**
	 * Returns whether this DBMS uses catalogs.
	 *
	 * @return <code>true</code> if catalogs are used
	 */
	public boolean isUsesCatalog() {
		return usesCatalog;
	}

	/**
	 * Sets whether this DBMS uses catalogs.
	 *
	 * @param usesCatalog <code>true</code> if catalogs are used
	 */
	public void setUsesCatalog(boolean usesCatalog) {
		this.usesCatalog = usesCatalog;
	}
	
	private boolean supportsGlobalWorkingtables = true;
	
	/**
	 * Returns whether this DBMS supports global working tables.
	 *
	 * @return <code>true</code> if global working tables are supported
	 */
	public boolean isSupportsGlobalWorkingtables() {
		return supportsGlobalWorkingtables;
	}

	/**
	 * Sets whether this DBMS supports global working tables.
	 *
	 * @param supportsGlobalWorkingtables <code>true</code> if global working tables are supported
	 */
	public void setSupportsGlobalWorkingtables(boolean supportsGlobalWorkingtables) {
		this.supportsGlobalWorkingtables = supportsGlobalWorkingtables;
	}

	private boolean supported = true;
	
	/**
	 * Returns whether this DBMS is supported.
	 *
	 * @return <code>true</code> if this DBMS is supported
	 */
	public boolean isSupported() {
		return supported;
	}

	/**
	 * Sets whether this DBMS is supported.
	 *
	 * @param supported <code>true</code> if this DBMS is supported
	 */
	public void setSupported(boolean supported) {
		this.supported = supported;
	}

	/**
	 * Returns whether the given type name matches the CLOB type pattern.
	 *
	 * @param typeWithLength the SQL type name (possibly including length)
	 * @return <code>true</code> if the type is a CLOB type
	 */
	public boolean isClobType(String typeWithLength) {
		if (clobTypesRE == null) {
			return false;
		}
		if (clobTypesPattern == null) {
			clobTypesPattern = Pattern.compile(clobTypesRE, Pattern.CASE_INSENSITIVE);
		}
		return clobTypesPattern.matcher(typeWithLength).matches();
	}

	/**
	 * Returns whether the given type name matches the NCLOB type pattern.
	 *
	 * @param typeWithLength the SQL type name (possibly including length)
	 * @return <code>true</code> if the type is an NCLOB type
	 */
	public boolean isNClobType(String typeWithLength) {
		if (nClobTypesRE == null) {
			return false;
		}
		if (nClobTypesPattern == null) {
			nClobTypesPattern = Pattern.compile(nClobTypesRE, Pattern.CASE_INSENSITIVE);
		}
		return nClobTypesPattern.matcher(typeWithLength).matches();
	}

	/**
	 * Returns whether the given type name matches the BLOB type pattern.
	 *
	 * @param typeWithLength the SQL type name (possibly including length)
	 * @return <code>true</code> if the type is a BLOB type
	 */
	public boolean isBlobType(String typeWithLength) {
		if (blobTypesRE == null) {
			return false;
		}
		if (blobTypesPattern == null) {
			blobTypesPattern = Pattern.compile(blobTypesRE, Pattern.CASE_INSENSITIVE);
		}
		return blobTypesPattern.matcher(typeWithLength).matches();
	}

	/**
	 * Gets the regular expression pattern for CLOB type names.
	 *
	 * @return the CLOB types regular expression
	 */
	public String getClobTypesRE() {
		return clobTypesRE;
	}

	/**
	 * Sets the regular expression pattern for CLOB type names.
	 *
	 * @param clobTypesRE the CLOB types regular expression to set
	 */
	public void setClobTypesRE(String clobTypesRE) {
		this.clobTypesRE = clobTypesRE;
	}

	/**
	 * Gets the regular expression pattern for NCLOB type names.
	 *
	 * @return the NCLOB types regular expression
	 */
	public String getnClobTypesRE() {
		return nClobTypesRE;
	}

	/**
	 * Sets the regular expression pattern for NCLOB type names.
	 *
	 * @param nClobTypesRE the NCLOB types regular expression to set
	 */
	public void setnClobTypesRE(String nClobTypesRE) {
		this.nClobTypesRE = nClobTypesRE;
	}

	/**
	 * Gets the regular expression pattern for BLOB type names.
	 *
	 * @return the BLOB types regular expression
	 */
	public String getBlobTypesRE() {
		return blobTypesRE;
	}

	/**
	 * Sets the regular expression pattern for BLOB type names.
	 *
	 * @param blobTypesRE the BLOB types regular expression to set
	 */
	public void setBlobTypesRE(String blobTypesRE) {
		this.blobTypesRE = blobTypesRE;
	}

	/**
	 * Gets the query to retrieve virtual columns.
	 *
	 * @return the virtual columns query
	 */
	public String getVirtualColumnsQuery() {
		return virtualColumnsQuery;
	}

	/**
	 * Sets the query to retrieve virtual columns.
	 *
	 * @param virtualColumnsQuery the virtual columns query to set
	 */
	public void setVirtualColumnsQuery(String virtualColumnsQuery) {
		this.virtualColumnsQuery = virtualColumnsQuery;
	}

	/**
	 * Returns whether inline views should be used in the Data Browser for this DBMS.
	 *
	 * @return <code>true</code> if inline views should be used in the Data Browser
	 */
	public boolean isUseInlineViewsInDataBrowser() {
		return useInlineViewsInDataBrowser;
	}

	/**
	 * Sets whether inline views should be used in the Data Browser for this DBMS.
	 *
	 * @param useInlineViewsInDataBrowser <code>true</code> to use inline views in the Data Browser
	 */
	public void setUseInlineViewsInDataBrowser(boolean useInlineViewsInDataBrowser) {
		this.useInlineViewsInDataBrowser = useInlineViewsInDataBrowser;
	}

	/**
	 * Gets the maximum size (in bytes) for LOB values to be embedded inline in SQL statements.
	 *
	 * @return the embedded LOB size limit
	 */
	public int getEmbeddedLobSizeLimit() {
		return embeddedLobSizeLimit;
	}

	/**
	 * Sets the maximum size (in bytes) for LOB values to be embedded inline in SQL statements.
	 *
	 * @param embeddedLobSizeLimit the embedded LOB size limit to set
	 */
	public void setEmbeddedLobSizeLimit(int embeddedLobSizeLimit) {
		this.embeddedLobSizeLimit = embeddedLobSizeLimit;
	}

	/**
	 * Returns whether this DBMS supports schema qualifiers in index definitions.
	 *
	 * @return <code>true</code> if schema qualifiers are supported in index definitions, or {@code null} if unknown
	 */
	public Boolean getSupportsSchemasInIndexDefinitions() {
		return supportsSchemasInIndexDefinitions;
	}

	/**
	 * Sets whether this DBMS supports schema qualifiers in index definitions.
	 *
	 * @param supportsSchemasInIndexDefinitions <code>true</code> if schema qualifiers are supported in index definitions
	 */
	public void setSupportsSchemasInIndexDefinitions(
			Boolean supportsSchemasInIndexDefinitions) {
		this.supportsSchemasInIndexDefinitions = supportsSchemasInIndexDefinitions;
	}

	/**
	 * Gets the name of the rowid/ctid pseudo-column for this DBMS.
	 *
	 * @return the rowid column name, or {@code null} if not applicable
	 */
	public String getRowidName() {
		return rowidName;
	}

	/**
	 * Sets the name of the rowid/ctid pseudo-column for this DBMS.
	 *
	 * @param rowidName the rowid column name to set
	 */
	public void setRowidName(String rowidName) {
		this.rowidName = rowidName != null && rowidName.trim().length() == 0? null : rowidName;
	}

	/**
	 * Gets the SQL type of the rowid/ctid pseudo-column for this DBMS.
	 *
	 * @return the rowid column type, or {@code null} if not applicable
	 */
	public String getRowidType() {
		return rowidType;
	}

	/**
	 * Sets the SQL type of the rowid/ctid pseudo-column for this DBMS.
	 *
	 * @param rowidType the rowid column type to set
	 */
	public void setRowidType(String rowidType) {
		this.rowidType = rowidType;
	}

	private String rowidType = null;

	/**
	 * Gets the SQL dialect configuration for this DBMS.
	 *
	 * @return the SQL dialect
	 */
	public SQLDialect getSqlDialect() {
		return sqlDialect;
	}

	/**
	 * Sets the SQL dialect.
	 *
	 * @param sqlDialect the SQL dialect to set
	 */
	public void setSqlDialect(SQLDialect sqlDialect) {
		this.sqlDialect = sqlDialect;
	}

	/**
	 * Manages session local temporary tables.
	 */
	private DefaultTemporaryTableManager sessionTemporaryTableManager = null;

	/**
	 * Manages transaction local temporary tables.
	 */
	private DefaultTemporaryTableManager transactionTemporaryTableManager = null;

	/**
	 * Optional JDBC properties.
	 */
	private Map<String, String> jdbcProperties = null;

	/**
	 * Gets the DB-URL pattern for this DBMS.
	 *
	 * @return the URL pattern
	 */
	public String getUrlPattern() {
		return urlPattern;
	}

	/**
	 * Sets DB-URL pattern of DBMS for which this holds the configuration.
	 *
	 * @param urlPattern the URL pattern to set
	 */
	public void setUrlPattern(String urlPattern) {
		this.urlPattern = urlPattern;
	}

	/**
	 * Gets the set of type names for which no data must be exported.
	 *
	 * @return the set of blocked export type names
	 */
	public Set<String> getExportBlocks() {
		return exportBlocks;
	}

	/**
	 * Sets the set of type names for which no data must be exported.
	 *
	 * @param exportBlocks the set of blocked export type names to set
	 */
	public void setExportBlocks(Set<String> exportBlocks) {
		this.exportBlocks = exportBlocks;
	}

	/**
	 * Gets the {@link SqlScriptBasedStatisticRenovator}.
	 *
	 * @return the {@link SqlScriptBasedStatisticRenovator}
	 */
	public SqlScriptBasedStatisticRenovator getStatisticRenovator() {
		return statisticRenovator;
	}

	/**
	 * Sets the {@link SqlScriptBasedStatisticRenovator}.
	 *
	 * @param statisticRenovator the {@link SqlScriptBasedStatisticRenovator}
	 */
	public void setStatisticRenovator(SqlScriptBasedStatisticRenovator statisticRenovator) {
		this.statisticRenovator = statisticRenovator;
	}

	/**
	 * Sets the value used to represent an empty CLOB.
	 *
	 * @param emptyCLOBValue the empty CLOB value to set
	 */
	public void setEmptyCLOBValue(String emptyCLOBValue) {
		this.emptyCLOBValue = emptyCLOBValue;
	}

	/**
	 * Sets the value used to represent an empty BLOB.
	 *
	 * @param emptyBLOBValue the empty BLOB value to set
	 */
	public void setEmptyBLOBValue(String emptyBLOBValue) {
		this.emptyBLOBValue = emptyBLOBValue;
	}

	/**
	 * Sets the pattern used to format binary data.
	 *
	 * @param binaryPattern the binary pattern to set
	 */
	public void setBinaryPattern(String binaryPattern) {
		this.binaryPattern = binaryPattern;
	}

	/**
	 * Sets replacement map for column types used for DDL generation.
	 *
	 * @param tr the type replacement map to set
	 */
	public void setTypeReplacement(Map<String, String> tr) {
		typeReplacement = tr;
	}

	/**
	 * Sets replacement map for column types used for DDL generation. Only used if the DBMS accepts it.
	 *
	 * @param tr the experimental type replacement map to set
	 */
	public void setExperimentalTypeReplacement(Map<String, String> tr) {
		experimentalTypeReplacement = tr;
	}

	/**
	 * Gets the SQL expression template used to convert a value to a BLOB.
	 *
	 * @return the to-BLOB conversion expression
	 */
	public String getToBlob() {
		return toBlob;
	}

	/**
	 * Sets the SQL expression template used to convert a value to a BLOB.
	 *
	 * @param toBlob the to-BLOB conversion expression to set
	 */
	public void setToBlob(String toBlob) {
		this.toBlob = toBlob;
	}

	/**
	 * Gets the SQL expression template used to convert a value to a CLOB.
	 *
	 * @return the to-CLOB conversion expression
	 */
	public String getToClob() {
		return toClob;
	}

	/**
	 * Sets the SQL expression template used to convert a value to a CLOB.
	 *
	 * @param toClob the to-CLOB conversion expression to set
	 */
	public void setToClob(String toClob) {
		this.toClob = toClob;
	}

	/**
	 * Gets the replacement map for column types used for DDL generation.
	 *
	 * @return the type replacement map
	 */
	public Map<String, String> getTypeReplacement() {
		if (!this.equals(DBMS.ORACLE)) {
			if (typeReplacement == null || !typeReplacement.containsKey("VARCHAR2")) {
				if (typeReplacement == null) {
					typeReplacement = new HashMap<String, String>();
				} else {
					typeReplacement = new HashMap<String, String>(typeReplacement);
				}
				typeReplacement.put("VARCHAR2", "VARCHAR");
			}
		}
		return typeReplacement;
	}

	/**
	 * Gets replacement map for column types used for DDL generation. Only used if DBMS accepts it.
	 *
	 * @return the experimental type replacement map
	 */
	public Map<String, String> getExperimentalTypeReplacement() {
		return experimentalTypeReplacement;
	}

	/**
	 * Sets the manager for session-local temporary tables.
	 *
	 * @param tableManager the session temporary table manager to set
	 */
	public void setSessionTemporaryTableManager(DefaultTemporaryTableManager tableManager) {
		sessionTemporaryTableManager = tableManager;
	}

	/**
	 * Sets the manager for transaction-local temporary tables.
	 *
	 * @param tableManager the transaction temporary table manager to set
	 */
	public void setTransactionTemporaryTableManager(DefaultTemporaryTableManager tableManager) {
		transactionTemporaryTableManager = tableManager;
	}

	/**
	 * Returns whether the DBMS supports identity-type columns (MS SQL Server).
	 *
	 * @return <code>true</code> if identity inserts are supported
	 */
	public boolean isIdentityInserts() {
		return identityInserts;
	}

	/**
	 * Sets whether the DBMS supports identity-type columns (MS SQL Server).
	 *
	 * @param identityInserts <code>true</code> if identity inserts are supported
	 */
	public void setIdentityInserts(boolean identityInserts) {
		this.identityInserts = identityInserts;
	}

	/**
	 * Indicates that E'x\ny' syntax is supported.
	 */
	private boolean supportsCStyleBackslashEscapes = false;
	
	/**
	 * Indicates that E'x\ny' syntax is supported.
	 * 
	 * @return <code>true</code> iff E'x\ny' syntax is supported
	 */
	public boolean isSupportsCStyleBackslashEscapes() {
		return supportsCStyleBackslashEscapes;
	}

	/**
	 * Indicates that E'x\ny' syntax is supported.
	 * 
	 * @param supportsCStyleBackslashEscapes <code>true</code> iff E'x\ny' syntax is supported
	 */
	public void setSupportsCStyleBackslashEscapes(boolean supportsCStyleBackslashEscapes) {
		this.supportsCStyleBackslashEscapes = supportsCStyleBackslashEscapes;
	}

	/**
	 * Sets the replacement map for special characters in string literals.
	 *
	 * @param stringLiteralEscapeSequences the string literal escape sequences map to set
	 */
	public void setStringLiteralEscapeSequences(Map<String, String> stringLiteralEscapeSequences) {
		if (stringLiteralEscapeSequences == null) {
			stringLiteralEscapeSequences = new HashMap<String, String>();
		}
		this.stringLiteralEscapeSequences = stringLiteralEscapeSequences;
		try {
			for (Map.Entry<String, String> e: stringLiteralEscapeSequences.entrySet()) {
				if (e.getKey().startsWith("\\")) {
					char c;
					char c2 = e.getKey().charAt(1);
					if (Character.isDigit(c2)) {
						c = (char) Integer.parseInt(e.getKey().substring(1));
					} else if (c2 == '\\') {
						c = '\\';
					} else if (c2 == 'b') {
						c = '\b';
					} else if (c2 == 'n') {
						c = '\n';
					} else if (c2 == 't') {
						c = '\t';
					} else if (c2 == 'r') {
						c = '\r';
					} else {
						throw new RuntimeException("illegal escape sequence: " + e.getKey());
					}
					charToEscapeSequence.put(c, e.getValue());
				} else {
					charToEscapeSequence.put(e.getKey().charAt(0), e.getValue());
				}
			}
			keysOfCharToEscapeSequence = new char[charToEscapeSequence.keySet().size()];
			int i = 0;
			for (char c: charToEscapeSequence.keySet()) {
				keysOfCharToEscapeSequence[i++] = c;
			}
		} catch (Exception e) {
			throw new RuntimeException("cannot recognize key of stringLiteralEscapeSequences-entry", e);
		}
	}

	/**
	 * Gets the replacement map for special characters in string literals.
	 *
	 * @return the string literal escape sequences map
	 */
	public Map<String, String> getStringLiteralEscapeSequences() {
		return stringLiteralEscapeSequences;
	}

	/**
	 * Converts a string to a string literal according to the {@link #getStringLiteralEscapeSequences()}.
	 *
	 * @param string the string to convert
	 * @param mustBeParenthesized 1-element array. Contains <code>true</code> after call iff result is complex expression.
	 * @return the string literal
	 */
	public String convertToStringLiteral(String string, boolean[] mustBeParenthesized) {
		return convertToStringLiteral(string, null, mustBeParenthesized);
	}

	/**
	 * Converts a string to a string literal according to the {@link #getStringLiteralEscapeSequences()}.
	 *
	 * @param string the string to convert
	 * @param prefix literal prefix (optional)
	 * @param mustBeParenthesized 1-element array. Contains <code>true</code> after call iff result is complex expression.
	 * @return the string literal
	 */
	public String convertToStringLiteral(String string, String prefix, boolean[] mustBeParenthesized) {
		boolean esc = false;
		for (char c: keysOfCharToEscapeSequence) {
			if (string.indexOf(c) >= 0) {
				esc = true;
				break;
			}
		}
		if (!esc) {
			return string;
		}

		StringBuilder qvalue = new StringBuilder();
		int l = string.length();
		boolean resultIsComplex = false;

		for (int i = 0; i < l; ++i) {
			char c = string.charAt(i);
			String es = charToEscapeSequence.get(c);
			if (es != null) {
				boolean isComplex = es.startsWith("'") && es.endsWith("'") && es.length() > 2;
				if (isComplex) {
					resultIsComplex = true;
				}
				if (prefix != null && isComplex) {
					es = es.substring(0, es.length() - 1) + prefix + "'";
				}
				qvalue.append(es);
			} else {
				qvalue.append(c);
			}
		}
		if (mustBeParenthesized != null && mustBeParenthesized.length > 0) {
			if (resultIsComplex && ("POSTGRESQL".equals(id) || "POSTGRESQL".equals(familyId))) {
				mustBeParenthesized[0] = true;
			} else {
				mustBeParenthesized[0] = false;
			}
		}
		return qvalue.toString();
	}
	
	/**
	 * Eventually applies standard escape syntax.
	 *
	 * @param literal the literal
	 * @param value original string
	 * @param prefix literal prefix (optional)
	 * @return the post-processed literal
	 */
	public String postProcessStringLiteral(String literal, String value, String prefix) {
		// TODO 1 check. this doesn't make sense. (f.e. what about the "'"?)
		if (supportsCStyleBackslashEscapes && prefix == null && literal.startsWith("'") && literal.endsWith("'")) {
			if (value.contains("\n") || value.contains("\r") || value.contains("\t")) {
				return "E'" + convertToStringLiteral(value.replace("\\", "\\\\").replace("\r", "\\r").replace("\n", "\\n").replace("\t", "\\t"), new boolean[1]) + "'";
			}
		}
		return literal;
	}
	
	/**
	 * Returns whether left joins should be avoided for this DBMS.
	 *
	 * @return <code>true</code> if left joins should be avoided
	 */
	public boolean isAvoidLeftJoin() {
		return avoidLeftJoin;
	}

	/**
	 * Sets whether left joins should be avoided for this DBMS.
	 *
	 * @param avoidLeftJoin <code>true</code> to avoid left joins
	 */
	public void setAvoidLeftJoin(boolean avoidLeftJoin) {
		this.avoidLeftJoin = avoidLeftJoin;
	}

	/**
	 * Sets the suffix appended to a SQL SELECT statement to limit the number of rows returned.
	 *
	 * @param sqlLimitSuffix the SQL limit suffix to set
	 */
	public void setSqlLimitSuffix(String sqlLimitSuffix) {
		this.sqlLimitSuffix = sqlLimitSuffix;
	}

	/**
	 * Gets the suffix appended to a SQL SELECT statement to limit the number of rows returned.
	 *
	 * @return the SQL limit suffix
	 */
	public String getSqlLimitSuffix() {
		return sqlLimitSuffix;
	}

	/**
	 * Gets the maximum length for VARCHAR columns, or {@code null} if unlimited.
	 *
	 * @return the VARCHAR length limit, or {@code null}
	 */
	public Integer getVarcharLengthLimit() {
		return varcharLengthLimit;
	}

	/**
	 * Sets the maximum length for VARCHAR columns.
	 *
	 * @param varcharLengthLimit the VARCHAR length limit to set, or {@code null} for unlimited
	 */
	public void setVarcharLengthLimit(Integer varcharLengthLimit) {
		this.varcharLengthLimit = varcharLengthLimit;
	}

	/**
	 * Gets the pattern used to format timestamp values.
	 *
	 * @return the timestamp pattern
	 */
	public String getTimestampPattern() {
		return timestampPattern;
	}

	/**
	 * Sets the pattern used to format timestamp values.
	 *
	 * @param timestampPattern the timestamp pattern to set
	 */
	public void setTimestampPattern(String timestampPattern) {
		this.timestampPattern = timestampPattern;
	}

	/**
	 * Gets the name of the timestamp type with nanosecond precision, if any.
	 *
	 * @return the name of the timestamp type with nano-precision, or {@code null} if not applicable
	 */
	public String getTimestampWithNanoTypeName() {
		return timestampWithNanoTypeName;
	}

	/**
	 * Sets the name of the timestamp type with nanosecond precision.
	 *
	 * @param timestampWithNanoTypeName the name of the timestamp type with nano-precision, if any
	 */
	public void setTimestampWithNanoTypeName(String timestampWithNanoTypeName) {
		this.timestampWithNanoTypeName = timestampWithNanoTypeName;
	}

	/**
	 * Gets the pattern used to format timestamp values with nanosecond precision.
	 *
	 * @return the timestamp-with-nano pattern
	 */
	public String getTimestampWithNanoPattern() {
		return timestampWithNanoPattern;
	}

	/**
	 * Sets the pattern used to format timestamp values with nanosecond precision.
	 *
	 * @param timestampWithNanoPattern the timestamp-with-nano pattern to set
	 */
	public void setTimestampWithNanoPattern(String timestampWithNanoPattern) {
		this.timestampWithNanoPattern = timestampWithNanoPattern;
	}

	/**
	 * Creates a {@link SimpleDateFormat} from the {@link #getTimestampWithNanoPattern()}.
	 *
	 * @return the timestamp-with-nano format, or {@code null} if no pattern is configured
	 */
	public SimpleDateFormat createTimestampWithNanoFormat() {
		if (timestampWithNanoPattern == null) {
			return null;
		}
		SimpleDateFormat format = timestampWithNanoFormat.get();
		if (format == null) {
			format = new SimpleDateFormat(timestampWithNanoPattern, Locale.ENGLISH);
			timestampWithNanoFormat.set(format);
		}
		return format;
	}

	/**
	 * Creates a {@link SimpleDateFormat} from the {@link #getTimestampPattern()}.
	 *
	 * @return the timestamp format
	 */
	public SimpleDateFormat createTimestampFormat() {
		SimpleDateFormat format = timestampFormat.get();
		if (format == null) {
			format = new SimpleDateFormat(timestampPattern, Locale.ENGLISH);
			timestampFormat.set(format);
		}
		return format;
	}

	/**
	 * Gets the pattern used to format date values.
	 *
	 * @return the date pattern
	 */
	public String getDatePattern() {
		return datePattern;
	}

	/**
	 * Sets the pattern used to format date values.
	 *
	 * @param datePattern the date pattern to set
	 */
	public void setDatePattern(String datePattern) {
		this.datePattern = datePattern;
	}

	/**
	 * Creates a {@link SimpleDateFormat} from the {@link #getDatePattern()}.
	 *
	 * @return the date format
	 */
	public SimpleDateFormat createDateFormat() {
		SimpleDateFormat format = dateFormat.get();
		if (format == null) {
			format = new SimpleDateFormat(datePattern, Locale.ENGLISH);
			dateFormat.set(format);
		}
		return format;
	}

	/**
	 * Gets the prefix used for national character (NCHAR) string literals.
	 *
	 * @return the NCHAR prefix
	 */
	public String getNcharPrefix() {
		return ncharPrefix;
	}

	/**
	 * Sets the prefix used for national character (NCHAR) string literals.
	 *
	 * @param ncharPrefix the NCHAR prefix to set
	 */
	public void setNcharPrefix(String ncharPrefix) {
		this.ncharPrefix = ncharPrefix;
	}

	/**
	 * Gets the additional table properties clause appended to CREATE TABLE statements.
	 *
	 * @return the table properties clause
	 */
	public String getTableProperties() {
		return tableProperties;
	}

	/**
	 * Sets the additional table properties clause appended to CREATE TABLE statements.
	 *
	 * @param tableProperties the table properties clause to set
	 */
	public void setTableProperties(String tableProperties) {
		this.tableProperties = tableProperties;
	}

	/**
	 * Gets the JDBC properties.
	 *
	 * @return the jdbcProperties
	 */
	public Map<String, String> getJdbcProperties() {
		return jdbcProperties;
	}

	/**
	 * Sets the JDBC properties.
	 *
	 * @param jdbcProperties the jdbcProperties to set
	 */
	public void setJdbcProperties(Map<String, String> jdbcProperties) {
		this.jdbcProperties = jdbcProperties;
	}

	/**
	 * Gets the string used to quote SQL identifiers.
	 *
	 * @return the identifier quote string
	 */
	public String getIdentifierQuoteString() {
		return identifierQuoteString;
	}

	/**
	 * Sets the string used to quote SQL identifiers.
	 *
	 * @param identifierQuoteString the identifier quote string to set
	 */
	public void setIdentifierQuoteString(String identifierQuoteString) {
		this.identifierQuoteString = identifierQuoteString;
	}

	/**
	 * Gets the test query for this DBMS.
	 *
	 * @return the test query
	 */
	public String getTestQuery() {
		return testQuery;
	}

	/**
	 * Sets the test query for this DBMS.
	 *
	 * @param testQuery the test query to set
	 */
	public void setTestQuery(String testQuery) {
		this.testQuery = testQuery;
	}

	/**
	 * Gets the query to retrieve user-defined columns.
	 *
	 * @return the user-defined columns query
	 */
	public String getUserDefinedColumnsQuery() {
		return userDefinedColumnsQuery;
	}

	/**
	 * Sets the query to retrieve user-defined columns.
	 *
	 * @param userDefinedColumnsQuery the user-defined columns query to set
	 */
	public void setUserDefinedColumnsQuery(String userDefinedColumnsQuery) {
		this.userDefinedColumnsQuery = userDefinedColumnsQuery;
	}

	/**
	 * Gets the query to retrieve identity columns.
	 *
	 * @return the identity columns query
	 */
	public String getIdentityColumnsQuery() {
		return identityColumnsQuery;
	}

	/**
	 * Sets the query to retrieve identity columns.
	 *
	 * @param identityColumnsQuery the identity columns query to set
	 */
	public void setIdentityColumnsQuery(String identityColumnsQuery) {
		this.identityColumnsQuery = identityColumnsQuery;
	}

	/**
	 * Gets the query to retrieve partition information.
	 *
	 * @return the partitions query
	 */
	public String getPartitionsQuery() {
		return partitionsQuery;
	}

	/**
	 * Sets the query to retrieve partition information.
	 *
	 * @param partitionsQuery the partitions query to set
	 */
	public void setPartitionsQuery(String partitionsQuery) {
		this.partitionsQuery = partitionsQuery;
	}

	private String identityColumnInsertClause;

	/**
	 * Gets the SQL clause used in INSERT statements for identity columns.
	 *
	 * @return the identity column insert clause
	 */
	public String getIdentityColumnInsertClause() {
		return identityColumnInsertClause;
	}

	/**
	 * Sets the SQL clause used in INSERT statements for identity columns.
	 *
	 * @param identityColumnInsertClause the identity column insert clause to set
	 */
	public void setIdentityColumnInsertClause(String identityColumnInsertClause) {
		this.identityColumnInsertClause = identityColumnInsertClause;
	}

	private String nullableContraint = null;

	/**
	 * Gets the SQL constraint clause used to declare nullable columns.
	 *
	 * @return the nullable constraint clause
	 */
	public String getNullableContraint() {
		return nullableContraint;
	}

	/**
	 * Sets the SQL constraint clause used to declare nullable columns.
	 *
	 * @param nullableContraint the nullable constraint clause to set
	 */
	public void setNullableContraint(String nullableContraint) {
		this.nullableContraint = nullableContraint;
	}

	/**
	 * Gets the value used to represent an empty NCLOB.
	 *
	 * @return the empty NCLOB value
	 */
	public String getEmptyNCLOBValue() {
		return emptyNCLOBValue;
	}

	/**
	 * Sets the value used to represent an empty NCLOB.
	 *
	 * @param emptyNCLOBValue the empty NCLOB value to set
	 */
	public void setEmptyNCLOBValue(String emptyNCLOBValue) {
		this.emptyNCLOBValue = emptyNCLOBValue;
	}

	/**
	 * Gets the query to retrieve imported foreign keys.
	 *
	 * @return the imported keys query
	 */
	public String getImportedKeysQuery() {
		return importedKeysQuery;
	}

	/**
	 * Sets the query to retrieve imported foreign keys.
	 *
	 * @param importedKeysQuery the imported keys query to set
	 */
	public void setImportedKeysQuery(String importedKeysQuery) {
		this.importedKeysQuery = importedKeysQuery;
	}

	/**
	 * Gets the query to retrieve primary keys.
	 *
	 * @return the primary keys query
	 */
	public String getPrimaryKeysQuery() {
		return primaryKeysQuery;
	}

	/**
	 * Sets the query to retrieve primary keys.
	 *
	 * @param primaryKeysQuery the primary keys query to set
	 */
	public void setPrimaryKeysQuery(String primaryKeysQuery) {
		this.primaryKeysQuery = primaryKeysQuery;
	}

	/**
	 * Gets the query to retrieve index information.
	 *
	 * @return the index info query
	 */
	public String getIndexInfoQuery() {
		return indexInfoQuery;
	}

	/**
	 * Sets the query to retrieve index information.
	 *
	 * @param indexInfoQuery the index info query to set
	 */
	public void setIndexInfoQuery(String indexInfoQuery) {
		this.indexInfoQuery = indexInfoQuery;
	}

	/**
	 * Gets the SQL expression template used to convert a value to an NCLOB.
	 *
	 * @return the to-NCLOB conversion expression
	 */
	public String getToNClob() {
		return toNClob;
	}

	/**
	 * Sets the SQL expression template used to convert a value to an NCLOB.
	 *
	 * @param toNClob the to-NCLOB conversion expression to set
	 */
	public void setToNClob(String toNClob) {
		this.toNClob = toNClob;
	}

	/**
	 * Gets the value used to represent an empty CLOB.
	 *
	 * @return the empty CLOB value
	 */
	public String getEmptyCLOBValue() {
		return emptyCLOBValue;
	}

	/**
	 * Gets the value used to represent an empty BLOB.
	 *
	 * @return the empty BLOB value
	 */
	public String getEmptyBLOBValue() {
		return emptyBLOBValue;
	}

	/**
	 * Gets the pattern used to format binary data as a SQL literal.
	 *
	 * @return the binary pattern
	 */
	public String getBinaryPattern() {
		return binaryPattern;
	}

	/**
	 * Gets the manager for session-local temporary tables.
	 *
	 * @return the session temporary table manager
	 */
	public DefaultTemporaryTableManager getSessionTemporaryTableManager() {
		return sessionTemporaryTableManager;
	}

	/**
	 * Gets the manager for transaction-local temporary tables.
	 *
	 * @return the transaction temporary table manager
	 */
	public DefaultTemporaryTableManager getTransactionTemporaryTableManager() {
		return transactionTemporaryTableManager;
	}

	/**
	 * Gets the DBMS identifier.
	 *
	 * @return the DBMS id
	 */
	public String getId() {
		return id;
	}

	/**
	 * Sets the DBMS identifier.
	 *
	 * @param id the DBMS id to set
	 */
	public void setId(String id) {
		this.id = id;
	}

	/**
	 * Gets the DBMS family identifier.
	 *
	 * @return the family id
	 */
	public String getFamilyId() {
		return familyId;
	}

	/**
	 * Sets the DBMS family identifier.
	 *
	 * @param familyId the family id to set
	 */
	public void setFamilyId(String familyId) {
		this.familyId = familyId;
	}

	/**
	 * Gets the display name of this DBMS.
	 *
	 * @return the display name
	 */
	public String getDisplayName() {
		return displayName;
	}

	/**
	 * Sets the display name of this DBMS.
	 *
	 * @param displayName the display name to set
	 */
	public void setDisplayName(String displayName) {
		this.displayName = displayName;
	}

	/**
	 * Gets the icon resource name for this DBMS.
	 *
	 * @return the icon resource name
	 */
	public String getIcon() {
		return icon;
	}

	/**
	 * Sets the icon resource name for this DBMS.
	 *
	 * @param icon the icon resource name to set
	 */
	public void setIcon(String icon) {
		this.icon = icon;
	}

	/**
	 * Gets the SQL statement to prepare for query explain.
	 *
	 * @return the explain prepare statement
	 */
	public String getExplainPrepare() {
		return explainPrepare;
	}

	/**
	 * Sets the SQL statement to prepare for query explain.
	 *
	 * @param explainPrepare the explain prepare statement to set
	 */
	public void setExplainPrepare(String explainPrepare) {
		this.explainPrepare = explainPrepare;
	}

	/**
	 * Gets the SQL statement to create the explain table.
	 *
	 * @return the explain table creation statement
	 */
	public String getExplainCreateExplainTable() {
		return explainCreateExplainTable;
	}

	/**
	 * Sets the SQL statement to create the explain table.
	 *
	 * @param explainCreateExplainTable the explain table creation statement to set
	 */
	public void setExplainCreateExplainTable(String explainCreateExplainTable) {
		this.explainCreateExplainTable = explainCreateExplainTable;
	}

	/**
	 * Gets the query to retrieve the explain plan.
	 *
	 * @return the explain query
	 */
	public String getExplainQuery() {
		return explainQuery;
	}

	/**
	 * Sets the query to retrieve the explain plan.
	 *
	 * @param explainQuery the explain query to set
	 */
	public void setExplainQuery(String explainQuery) {
		this.explainQuery = explainQuery;
	}

	/**
	 * Gets the SQL statement to clean up after query explain.
	 *
	 * @return the explain cleanup statement
	 */
	public String getExplainCleanup() {
		return explainCleanup;
	}

	/**
	 * Sets the SQL statement to clean up after query explain.
	 *
	 * @param explainCleanup the explain cleanup statement to set
	 */
	public void setExplainCleanup(String explainCleanup) {
		this.explainCleanup = explainCleanup;
	}

	/**
	 * Gets the query to retrieve the source of a function.
	 *
	 * @return the function source query
	 */
	public String getFunctionSourceQuery() {
		return functionSourceQuery;
	}

	/**
	 * Sets the query to retrieve the source of a function.
	 *
	 * @param functionSourceQuery the function source query to set
	 */
	public void setFunctionSourceQuery(String functionSourceQuery) {
		this.functionSourceQuery = functionSourceQuery;
	}

	/**
	 * Gets the query to retrieve the source of a stored procedure.
	 *
	 * @return the procedure source query
	 */
	public String getProcedureSourceQuery() {
		return procedureSourceQuery;
	}

	/**
	 * Sets the query to retrieve the source of a stored procedure.
	 *
	 * @param procedureSourceQuery the procedure source query to set
	 */
	public void setProcedureSourceQuery(String procedureSourceQuery) {
		this.procedureSourceQuery = procedureSourceQuery;
	}

	/**
	 * Gets the query to retrieve the source of a package.
	 *
	 * @return the package source query
	 */
	public String getPackageSourceQuery() {
		return packageSourceQuery;
	}

	/**
	 * Sets the query to retrieve the source of a package.
	 *
	 * @param packageSourceQuery the package source query to set
	 */
	public void setPackageSourceQuery(String packageSourceQuery) {
		this.packageSourceQuery = packageSourceQuery;
	}

	/**
	 * Gets the query to retrieve the names of all packages.
	 *
	 * @return the package names query
	 */
	public String getPackageNamesQuery() {
		return packageNamesQuery;
	}

	/**
	 * Sets the query to retrieve the names of all packages.
	 *
	 * @param packageNamesQuery the package names query to set
	 */
	public void setPackageNamesQuery(String packageNamesQuery) {
		this.packageNamesQuery = packageNamesQuery;
	}

	/**
	 * Gets the list of database object renderer descriptions.
	 *
	 * @return the object renderers
	 */
	public List<DatabaseObjectRenderingDescription> getObjectRenderers() {
		return objectRenderers;
	}

	/**
	 * Sets the list of database object renderer descriptions.
	 *
	 * @param objectRenderers the object renderers to set
	 */
	public void setObjectRenderers(List<DatabaseObjectRenderingDescription> objectRenderers) {
		this.objectRenderers = objectRenderers;
	}

	/**
	 * Returns whether retrieving procedure details requires the specific (overloaded) name.
	 *
	 * @return <code>true</code> if a specific name is required for procedure detail retrieval
	 */
	public boolean isProcedureDetailNeedsSpecificName() {
		return procedureDetailNeedsSpecificName;
	}

	/**
	 * Sets whether retrieving procedure details requires the specific (overloaded) name.
	 *
	 * @param procedureDetailNeedsSpecificName <code>true</code> if a specific name is required for procedure detail retrieval
	 */
	public void setProcedureDetailNeedsSpecificName(boolean procedureDetailNeedsSpecificName) {
		this.procedureDetailNeedsSpecificName = procedureDetailNeedsSpecificName;
	}

	/**
	 * Gets the query to retrieve constraint information.
	 *
	 * @return the constraints query
	 */
	public String getConstraintsQuery() {
		return constraintsQuery;
	}

	/**
	 * Sets the query to retrieve constraint information.
	 *
	 * @param constraintsQuery the constraints query to set
	 */
	public void setConstraintsQuery(String constraintsQuery) {
		this.constraintsQuery = constraintsQuery;
	}

	/**
	 * Gets the query to retrieve an estimated row count for a table.
	 *
	 * @return the estimated row count query
	 */
	public String getEstimatedRowCountQuery() {
		return estimatedRowCountQuery;
	}

	/**
	 * Sets the query to retrieve an estimated row count for a table.
	 *
	 * @param estimatedRowCountQuery the estimated row count query to set
	 */
	public void setEstimatedRowCountQuery(String estimatedRowCountQuery) {
		this.estimatedRowCountQuery = estimatedRowCountQuery;
	}

	/**
	 * Gets the query to retrieve the text or DDL of a view.
	 *
	 * @return the view text or DDL query
	 */
	public String getViewTextOrDDLQuery() {
		return viewTextOrDDLQuery;
	}

	/**
	 * Sets the query to get view text or DDL.
	 *
	 * @param viewTextOrDDLQuery the query to set
	 */
	public void setViewTextOrDDLQuery(String viewTextOrDDLQuery) {
		this.viewTextOrDDLQuery = viewTextOrDDLQuery;
	}

	/**
	 * Gets the query to retrieve the underlying table of a synonym.
	 *
	 * @return the synonym table query
	 */
	public String getSynonymTableQuery() {
		return synonymTableQuery;
	}

	/**
	 * Sets the query to retrieve the underlying table of a synonym.
	 *
	 * @param synonymTableQuery the synonym table query to set
	 */
	public void setSynonymTableQuery(String synonymTableQuery) {
		this.synonymTableQuery = synonymTableQuery;
	}

	/**
	 * Gets the query to retrieve the default schema of the current user.
	 *
	 * @return the default schema query
	 */
	public String getDefaultSchemaQuery() {
		return defaultSchemaQuery;
	}

	/**
	 * Sets the query to retrieve the default schema of the current user.
	 *
	 * @param defaultSchemaQuery the default schema query to set
	 */
	public void setDefaultSchemaQuery(String defaultSchemaQuery) {
		this.defaultSchemaQuery = defaultSchemaQuery;
	}

	/**
	 * Gets information about how to limit transaction size (never <code>null</code>).
	 *
	 * @return the limit transaction size info, never <code>null</code>
	 */
	public LimitTransactionSizeInfo getLimitTransactionSize() {
		if (limitTransactionSize == null) {
			 limitTransactionSize = new LimitTransactionSizeInfo();
		}
		return limitTransactionSize;
	}

	/**
	 * Sets information about how to limit transaction size.
	 *
	 * @param incremenalInsertInfo the limit transaction size info to set
	 */
	public void setLimitTransactionSize(LimitTransactionSizeInfo incremenalInsertInfo) {
		this.limitTransactionSize = incremenalInsertInfo;
	}

	/**
	 * Gets fetch size.
	 *
	 * @return fetch size
	 */
	public Integer getFetchSize() {
		return fetchSize;
	}

	/**
	 * Sets fetch size.
	 *
	 * @param fetchSize fetch size
	 */
	public void setFetchSize(Integer fetchSize) {
		this.fetchSize = fetchSize;
	}

	/**
	 * Gets the SQL expression rewrite rules for this DBMS.
	 *
	 * @return the SQL expression rule map
	 */
	public Map<String, String> getSqlExpressionRule() {
		return sqlExpressionRule;
	}

	/**
	 * Sets the SQL expression rewrite rules for this DBMS.
	 *
	 * @param sqlExpressionRule the SQL expression rule map to set
	 */
	public void setSqlExpressionRule(Map<String, String> sqlExpressionRule) {
		this.sqlExpressionRule = sqlExpressionRule;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return id == null ? 0 : id.hashCode();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		DBMS other = (DBMS) obj;
		String fid = familyId != null? familyId : id;
		String otherFid = other.familyId != null? other.familyId : other.id;
		if (fid == null) {
			if (otherFid != null) {
				return false;
			}
		} else if (!fid.equals(otherFid)) {
			return false;
		}
		return true;
	}

	private static final ThreadLocal<Integer> tmpFetchSize = new ThreadLocal<>();
	
	/**
	 * Sets a temporary override for the fetch size, used during limited-row queries.
	 *
	 * @param tmpFetchSize the temporary fetch size to set, or {@code null} to clear
	 */
	public static void setTmpFetchSize(Integer tmpFetchSize) {
		DBMS.tmpFetchSize.set(tmpFetchSize);
	}

	/**
	 * Gets the effective fetch size to use when fetching at most {@code limit} rows.
	 *
	 * @param limit the maximum number of rows to fetch, or 0 if unlimited
	 * @return the fetch size to use
	 */
	public int getLimitedFetchSize(long limit) {
		Integer fSize = getFetchSize();
		if (fSize != null) {
			return fSize;
		}

		final int DEFAULT_FETCH_SIZE = 5010;
		if (fSize == null) {
			fSize = DEFAULT_FETCH_SIZE;
		}
		
		Integer dfs = tmpFetchSize.get();
		if (dfs == null) {
			dfs = fSize;
		}
		if (limit > 0 && limit <= dfs) {
			return (int) (limit + 1 + 1);
		} else {
			return dfs;
		}
	}

}
