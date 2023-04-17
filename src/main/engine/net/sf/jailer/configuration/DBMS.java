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

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlTransient;

import net.sf.jailer.database.DefaultTemporaryTableManager;
import net.sf.jailer.database.SQLDialect;
import net.sf.jailer.database.SqlScriptBasedStatisticRenovator;

/**
 * Describes a specific DBMS.
 *
 * @author Ralf Wisser
 */
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
	 * Copy constructor.
	 */
	public DBMS(DBMS other) {
		this.id = other.id;
		this.familyId = other.familyId;
		this.displayName = other.displayName;
		this.urlPattern = other.urlPattern;
		this.testQuery = other.testQuery;
		this.statisticRenovator = other.statisticRenovator;
		this.typeReplacement = other.typeReplacement;
		this.experimentalTypeReplacement = other.experimentalTypeReplacement;
		this.stringLiteralEscapeSequences = other.stringLiteralEscapeSequences;
		this.sqlLimitSuffix = other.sqlLimitSuffix;
		this.varcharLengthLimit = other.varcharLengthLimit;
		this.tableProperties = other.tableProperties;
		this.charToEscapeSequence = other.charToEscapeSequence;
		this.keysOfCharToEscapeSequence = other.keysOfCharToEscapeSequence;
		this.ncharPrefix = other.ncharPrefix;
		this.exportBlocks = other.exportBlocks;
		this.identityInserts = other.identityInserts;
		this.emptyCLOBValue = other.emptyCLOBValue;
		this.emptyNCLOBValue = other.emptyNCLOBValue;
		this.emptyBLOBValue = other.emptyBLOBValue;
		this.toBlob = other.toBlob;
		this.toClob = other.toClob;
		this.toNClob = other.toNClob;
		this.embeddedLobSizeLimit = other.embeddedLobSizeLimit;
		this.binaryPattern = other.binaryPattern;
		this.avoidLeftJoin = other.avoidLeftJoin;
		this.timestampPattern = other.timestampPattern;
		this.timestampWithNanoTypeName = other.timestampWithNanoTypeName;
		this.timestampWithNanoPattern = other.timestampWithNanoPattern;
		this.datePattern = other.datePattern;
		this.sqlDialect = other.sqlDialect;
		this.rowidName = other.rowidName;
		this.supportsSchemasInIndexDefinitions = other.supportsSchemasInIndexDefinitions;
		this.useInlineViewsInDataBrowser = other.useInlineViewsInDataBrowser;
		this.viewTextOrDDLQuery = other.viewTextOrDDLQuery;
		this.synonymTableQuery = other.synonymTableQuery;
		this.estimatedRowCountQuery = other.estimatedRowCountQuery;
		this.virtualColumnsQuery = other.virtualColumnsQuery;
		this.userDefinedColumnsQuery = other.userDefinedColumnsQuery;
		this.importedKeysQuery = other.importedKeysQuery;
		this.primaryKeysQuery = other.primaryKeysQuery;
		this.indexInfoQuery = other.indexInfoQuery;
		this.identifierQuoteString = other.identifierQuoteString;
		this.rowidType = other.rowidType;
		this.sessionTemporaryTableManager = other.sessionTemporaryTableManager;
		this.transactionTemporaryTableManager = other.transactionTemporaryTableManager;
		this.jdbcProperties = other.jdbcProperties;
		this.nullableContraint = other.nullableContraint;
		this.ddlQuery = other.ddlQuery;
		this.ddlCall = other.ddlCall;
		this.functionSourceQuery = other.functionSourceQuery;
		this.procedureSourceQuery = other.procedureSourceQuery;
		this.packageNamesQuery = other.packageNamesQuery;
		this.objectRenderers = other.objectRenderers;
		this.procedureDetailNeedsSpecificName = other.procedureDetailNeedsSpecificName;
		this.limitTransactionSize = other.limitTransactionSize;
		this.defaultSchemaQuery = other.defaultSchemaQuery;
		this.fetchSize = other.fetchSize;
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
	}

	private String id;
	private String familyId;
	private String displayName;

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
	 * DB-Query to get DDL of a table
	 */
	public String getDdlQuery() {
		return ddlQuery;
	}

	/**
	 * DB-Query to get DDL of a table
	 */
	public void setDdlQuery(String ddlQuery) {
		this.ddlQuery = ddlQuery;
	}

	/**
	 * DB-Call to get DDL of a table
	 */
	public String getDdlCall() {
		return ddlCall;
	}

	/**
	 * DB-Call to get DDL of a table
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
	@XmlTransient
	private ThreadLocal<SimpleDateFormat> timestampFormat = new ThreadLocal<SimpleDateFormat>();
	private ThreadLocal<SimpleDateFormat> timestampWithNanoFormat = new ThreadLocal<SimpleDateFormat>();
	private ThreadLocal<SimpleDateFormat> dateFormat = new ThreadLocal<SimpleDateFormat>();
	private SQLDialect sqlDialect = new SQLDialect();
	private String rowidName = null;
	private Boolean supportsSchemasInIndexDefinitions = null;
	private boolean useInlineViewsInDataBrowser = true;
	private String virtualColumnsQuery = null;
	private String identityColumnsQuery = null;
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

	public boolean isClobType(String typeWithLength) {
		if (clobTypesRE == null) {
			return false;
		}
		if (clobTypesPattern == null) {
			clobTypesPattern = Pattern.compile(clobTypesRE, Pattern.CASE_INSENSITIVE);
		}
		return clobTypesPattern.matcher(typeWithLength).matches();
	}

	public boolean isNClobType(String typeWithLength) {
		if (nClobTypesRE == null) {
			return false;
		}
		if (nClobTypesPattern == null) {
			nClobTypesPattern = Pattern.compile(nClobTypesRE, Pattern.CASE_INSENSITIVE);
		}
		return nClobTypesPattern.matcher(typeWithLength).matches();
	}

	public boolean isBlobType(String typeWithLength) {
		if (blobTypesRE == null) {
			return false;
		}
		if (blobTypesPattern == null) {
			blobTypesPattern = Pattern.compile(blobTypesRE, Pattern.CASE_INSENSITIVE);
		}
		return blobTypesPattern.matcher(typeWithLength).matches();
	}

	public String getClobTypesRE() {
		return clobTypesRE;
	}

	public void setClobTypesRE(String clobTypesRE) {
		this.clobTypesRE = clobTypesRE;
	}

	public String getnClobTypesRE() {
		return nClobTypesRE;
	}

	public void setnClobTypesRE(String nClobTypesRE) {
		this.nClobTypesRE = nClobTypesRE;
	}

	public String getBlobTypesRE() {
		return blobTypesRE;
	}

	public void setBlobTypesRE(String blobTypesRE) {
		this.blobTypesRE = blobTypesRE;
	}

	/**
	 * @return the virtualColumnsQuery
	 */
	public String getVirtualColumnsQuery() {
		return virtualColumnsQuery;
	}

	/**
	 * @param virtualColumnsQuery the virtualColumnsQuery to set
	 */
	public void setVirtualColumnsQuery(String virtualColumnsQuery) {
		this.virtualColumnsQuery = virtualColumnsQuery;
	}

	/**
	 * @return the useInlineViewsInDataBrowser
	 */
	public boolean isUseInlineViewsInDataBrowser() {
		return useInlineViewsInDataBrowser;
	}

	/**
	 * @param useInlineViewsInDataBrowser the useInlineViewsInDataBrowser to set
	 */
	public void setUseInlineViewsInDataBrowser(boolean useInlineViewsInDataBrowser) {
		this.useInlineViewsInDataBrowser = useInlineViewsInDataBrowser;
	}

	/**
	 * @return the embeddedLobSizeLimit
	 */
	public int getEmbeddedLobSizeLimit() {
		return embeddedLobSizeLimit;
	}

	/**
	 * @param embeddedLobSizeLimit the embeddedLobSizeLimit to set
	 */
	public void setEmbeddedLobSizeLimit(int embeddedLobSizeLimit) {
		this.embeddedLobSizeLimit = embeddedLobSizeLimit;
	}

	/**
	 * @return the supportsSchemasInIndexDefinitions
	 */
	public Boolean getSupportsSchemasInIndexDefinitions() {
		return supportsSchemasInIndexDefinitions;
	}

	/**
	 * @param supportsSchemasInIndexDefinitions the supportsSchemasInIndexDefinitions to set
	 */
	public void setSupportsSchemasInIndexDefinitions(
			Boolean supportsSchemasInIndexDefinitions) {
		this.supportsSchemasInIndexDefinitions = supportsSchemasInIndexDefinitions;
	}

	/**
	 * @return the rowidName
	 */
	public String getRowidName() {
		return rowidName;
	}

	/**
	 * @param rowidName the rowidName to set
	 */
	public void setRowidName(String rowidName) {
		this.rowidName = rowidName != null && rowidName.trim().length() == 0? null : rowidName;
	}

	/**
	 * @return the rowidType
	 */
	public String getRowidType() {
		return rowidType;
	}

	/**
	 * @param rowidType the rowidType to set
	 */
	public void setRowidType(String rowidType) {
		this.rowidType = rowidType;
	}

	private String rowidType = null;

	/**
	 * @return the sqlDialect
	 */
	public SQLDialect getSqlDialect() {
		return sqlDialect;
	}

	/**
	 * @return the sqlDialect
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
	 * @return the urlPattern
	 */
	public String getUrlPattern() {
		return urlPattern;
	}

	/**
	 * Sets DB-URL pattern of DBMS for which this holds the configuration.
	 */
	public void setUrlPattern(String urlPattern) {
		this.urlPattern = urlPattern;
	}

	public Set<String> getExportBlocks() {
		return exportBlocks;
	}

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

	public void setEmptyCLOBValue(String emptyCLOBValue) {
		this.emptyCLOBValue = emptyCLOBValue;
	}

	public void setEmptyBLOBValue(String emptyBLOBValue) {
		this.emptyBLOBValue = emptyBLOBValue;
	}

	public void setBinaryPattern(String binaryPattern) {
		this.binaryPattern = binaryPattern;
	}

	/**
	 * Sets replacement map for column types used for DDL generation.
	 */
	public void setTypeReplacement(Map<String, String> tr) {
		typeReplacement = tr;
	}

	/**
	 * Sets replacement map for column types used for DDL generation. Only used if DBMS accept it.
	 */
	public void setExperimentalTypeReplacement(Map<String, String> tr) {
		experimentalTypeReplacement = tr;
	}

	/**
	 * @return the toBlob
	 */
	public String getToBlob() {
		return toBlob;
	}

	/**
	 * @param toBlob the toBlob to set
	 */
	public void setToBlob(String toBlob) {
		this.toBlob = toBlob;
	}

	/**
	 * @return the toClob
	 */
	public String getToClob() {
		return toClob;
	}

	/**
	 * @param toClob the toClob to set
	 */
	public void setToClob(String toClob) {
		this.toClob = toClob;
	}

	/**
	 * Gets replacement map for column types used for DDL generation.
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
	 * Sets replacement map for column types used for DDL generation. Only used if DBMS accept it.
	 */
	public Map<String, String> getExperimentalTypeReplacement() {
		return experimentalTypeReplacement;
	}

	/**
	 * Sets manager for session local temporary tables.
	 */
	public void setSessionTemporaryTableManager(DefaultTemporaryTableManager tableManager) {
		sessionTemporaryTableManager = tableManager;
	}

	/**
	 * Sets manager for transaction local temporary tables.
	 */
	public void setTransactionTemporaryTableManager(DefaultTemporaryTableManager tableManager) {
		transactionTemporaryTableManager = tableManager;
	}

	public boolean isIdentityInserts() {
		return identityInserts;
	}

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
	 * Sets replacement map for special characters in string literals.
	 */
	public void setStringLiteralEscapeSequences(Map<String, String> stringLiteralEscapeSequences) {
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
	 * Gets replacement map for special characters in string literals.
	 */
	public Map<String, String> getStringLiteralEscapeSequences() {
		return stringLiteralEscapeSequences;
	}

	/**
	 * Converts a string to a string literal according to the {@link #getStringLiteralEscapeSequences()}.
	 *
	 * @param string the string to convert
	 * @return the string literal
	 */
	public String convertToStringLiteral(String string) {
		return convertToStringLiteral(string, null);
	}
	
	/**
	 * Converts a string to a string literal according to the {@link #getStringLiteralEscapeSequences()}.
	 *
	 * @param string the string to convert
	 * @param prefix literal prefix (optional)
	 * @return the string literal
	 */
	public String convertToStringLiteral(String string, String prefix) {
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

		for (int i = 0; i < l; ++i) {
			char c = string.charAt(i);
			String es = charToEscapeSequence.get(c);
			if (es != null) {
				if (prefix != null && es.startsWith("'") && es.endsWith("'") && es.length() > 2) {
					es = es.substring(0, es.length() - 1) + prefix + "'";
				}
				qvalue.append(es);
			} else {
				qvalue.append(c);
			}
		}
		return qvalue.toString();
	}
	
	/**
	 * Evt. applies standart escape syntax.
	 * 
	 * @param literal the litaral
	 * @param value original string
	 * @return literal
	 */
	public String postProcessStringLiteral(String literal, String value, String prefix) {
		if (supportsCStyleBackslashEscapes && prefix == null && literal.startsWith("'") && literal.endsWith("'")) {
			if (value.contains("\n") || value.contains("\r") || value.contains("\t")) {
				return "E'" + convertToStringLiteral(value.replace("\\", "\\\\").replace("\r", "\\r").replace("\n", "\\n").replace("\t", "\\t")) + "'";
			}
		}
		return literal;
	}
	
	public boolean isAvoidLeftJoin() {
		return avoidLeftJoin;
	}

	public void setAvoidLeftJoin(boolean avoidLeftJoin) {
		this.avoidLeftJoin = avoidLeftJoin;
	}

	/**
	 * @param sqlLimitSuffix the sqlLimitSuffix to set
	 */
	public void setSqlLimitSuffix(String sqlLimitSuffix) {
		this.sqlLimitSuffix = sqlLimitSuffix;
	}

	/**
	 * @return the sqlLimitSuffix
	 */
	public String getSqlLimitSuffix() {
		return sqlLimitSuffix;
	}

	public Integer getVarcharLengthLimit() {
		return varcharLengthLimit;
	}

	public void setVarcharLengthLimit(Integer varcharLengthLimit) {
		this.varcharLengthLimit = varcharLengthLimit;
	}

	/**
	 * @return the timestampPattern
	 */
	public String getTimestampPattern() {
		return timestampPattern;
	}

	/**
	 * @param timestampPattern the timestampPattern to set
	 */
	public void setTimestampPattern(String timestampPattern) {
		this.timestampPattern = timestampPattern;
	}

	/**
	 * @return name of timestamp type with nano-precision, if any
	 */
	public String getTimestampWithNanoTypeName() {
		return timestampWithNanoTypeName;
	}

	/**
	 * @param timestampWithNanoTypeName name of timestamp type with nano-precision, if any
	 */
	public void setTimestampWithNanoTypeName(String timestampWithNanoTypeName) {
		this.timestampWithNanoTypeName = timestampWithNanoTypeName;
	}

	/**
	 * @return timestampPattern the timestampPattern to set
	 */
	public String getTimestampWithNanoPattern() {
		return timestampWithNanoPattern;
	}

	/**
	 * @param timestampWithNanoPattern the timestampPattern to set
	 */
	public void setTimestampWithNanoPattern(String timestampWithNanoPattern) {
		this.timestampWithNanoPattern = timestampWithNanoPattern;
	}

	/**
	 * @return the {@link #getTimestampWithNanoPattern()} as {@link SimpleDateFormat}.
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
	 * @return the {@link #getTimestampPattern()} as {@link SimpleDateFormat}.
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
	 * @return the datePattern
	 */
	public String getDatePattern() {
		return datePattern;
	}

	/**
	 * @param datePattern the datePattern to set
	 */
	public void setDatePattern(String datePattern) {
		this.datePattern = datePattern;
	}

	/**
	 * @return the {@link #getDatePattern()} as {@link SimpleDateFormat}.
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
	 * @return the ncharPrefix
	 */
	public String getNcharPrefix() {
		return ncharPrefix;
	}

	/**
	 * @param ncharPrefix the ncharPrefix to set
	 */
	public void setNcharPrefix(String ncharPrefix) {
		this.ncharPrefix = ncharPrefix;
	}

	/**
	 * @return the tableProperties
	 */
	public String getTableProperties() {
		return tableProperties;
	}

	/**
	 * @param tableProperties the tableProperties to set
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
	 * @return the identifierQuoteString
	 */
	public String getIdentifierQuoteString() {
		return identifierQuoteString;
	}

	/**
	 * @param identifierQuoteString the identifierQuoteString to set
	 */
	public void setIdentifierQuoteString(String identifierQuoteString) {
		this.identifierQuoteString = identifierQuoteString;
	}

	public String getTestQuery() {
		return testQuery;
	}

	public void setTestQuery(String testQuery) {
		this.testQuery = testQuery;
	}

	public String getUserDefinedColumnsQuery() {
		return userDefinedColumnsQuery;
	}

	public void setUserDefinedColumnsQuery(String userDefinedColumnsQuery) {
		this.userDefinedColumnsQuery = userDefinedColumnsQuery;
	}

	public String getIdentityColumnsQuery() {
		return identityColumnsQuery;
	}

	public void setIdentityColumnsQuery(String identityColumnsQuery) {
		this.identityColumnsQuery = identityColumnsQuery;
	}
	
	private String identityColumnInsertClause;

	public String getIdentityColumnInsertClause() {
		return identityColumnInsertClause;
	}

	public void setIdentityColumnInsertClause(String identityColumnInsertClause) {
		this.identityColumnInsertClause = identityColumnInsertClause;
	}

	private String nullableContraint = null;

	/**
	 * @return the nullableContraint
	 */
	public String getNullableContraint() {
		return nullableContraint;
	}

	/**
	 * @param nullableContraint the nullableContraint to set
	 */
	public void setNullableContraint(String nullableContraint) {
		this.nullableContraint = nullableContraint;
	}

	/**
	 * @return the emptyNCLOBValue
	 */
	public String getEmptyNCLOBValue() {
		return emptyNCLOBValue;
	}

	/**
	 * @param emptyNCLOBValue the emptyNCLOBValue to set
	 */
	public void setEmptyNCLOBValue(String emptyNCLOBValue) {
		this.emptyNCLOBValue = emptyNCLOBValue;
	}

	/**
	 * @return the importedKeysQuery
	 */
	public String getImportedKeysQuery() {
		return importedKeysQuery;
	}

	/**
	 * @param importedKeysQuery the importedKeysQuery to set
	 */
	public void setImportedKeysQuery(String importedKeysQuery) {
		this.importedKeysQuery = importedKeysQuery;
	}

	/**
	 * @return the primaryKeysQuery
	 */
	public String getPrimaryKeysQuery() {
		return primaryKeysQuery;
	}

	/**
	 * @param primaryKeysQuery the primaryKeysQuery to set
	 */
	public void setPrimaryKeysQuery(String primaryKeysQuery) {
		this.primaryKeysQuery = primaryKeysQuery;
	}

	/**
	 * @return the indexInfoQuery
	 */
	public String getIndexInfoQuery() {
		return indexInfoQuery;
	}

	/**
	 * @param indexInfoQuery the indexInfoQuery to set
	 */
	public void setIndexInfoQuery(String indexInfoQuery) {
		this.indexInfoQuery = indexInfoQuery;
	}

	/**
	 * @return the toNClob
	 */
	public String getToNClob() {
		return toNClob;
	}

	/**
	 * @param toNClob the toNClob to set
	 */
	public void setToNClob(String toNClob) {
		this.toNClob = toNClob;
	}

	/**
	 * @return the emptyCLOBValue
	 */
	public String getEmptyCLOBValue() {
		return emptyCLOBValue;
	}

	/**
	 * @return the emptyBLOBValue
	 */
	public String getEmptyBLOBValue() {
		return emptyBLOBValue;
	}

	/**
	 * @return the binaryPattern
	 */
	public String getBinaryPattern() {
		return binaryPattern;
	}

	/**
	 * @return the sessionTemporaryTableManager
	 */
	public DefaultTemporaryTableManager getSessionTemporaryTableManager() {
		return sessionTemporaryTableManager;
	}

	/**
	 * @return the transactionTemporaryTableManager
	 */
	public DefaultTemporaryTableManager getTransactionTemporaryTableManager() {
		return transactionTemporaryTableManager;
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getFamilyId() {
		return familyId;
	}

	public void setFamilyId(String familyId) {
		this.familyId = familyId;
	}

	public String getDisplayName() {
		return displayName;
	}

	public void setDisplayName(String displayName) {
		this.displayName = displayName;
	}

	public String getExplainPrepare() {
		return explainPrepare;
	}

	public void setExplainPrepare(String explainPrepare) {
		this.explainPrepare = explainPrepare;
	}

	public String getExplainCreateExplainTable() {
		return explainCreateExplainTable;
	}

	public void setExplainCreateExplainTable(String explainCreateExplainTable) {
		this.explainCreateExplainTable = explainCreateExplainTable;
	}

	public String getExplainQuery() {
		return explainQuery;
	}

	public void setExplainQuery(String explainQuery) {
		this.explainQuery = explainQuery;
	}

	public String getExplainCleanup() {
		return explainCleanup;
	}

	public void setExplainCleanup(String explainCleanup) {
		this.explainCleanup = explainCleanup;
	}

	public String getFunctionSourceQuery() {
		return functionSourceQuery;
	}

	public void setFunctionSourceQuery(String functionSourceQuery) {
		this.functionSourceQuery = functionSourceQuery;
	}

	public String getProcedureSourceQuery() {
		return procedureSourceQuery;
	}

	public void setProcedureSourceQuery(String procedureSourceQuery) {
		this.procedureSourceQuery = procedureSourceQuery;
	}

	public String getPackageSourceQuery() {
		return packageSourceQuery;
	}

	public void setPackageSourceQuery(String packageSourceQuery) {
		this.packageSourceQuery = packageSourceQuery;
	}

	public String getPackageNamesQuery() {
		return packageNamesQuery;
	}

	public void setPackageNamesQuery(String packageNamesQuery) {
		this.packageNamesQuery = packageNamesQuery;
	}

	@XmlElement(name = "objectRenderers")
	public List<DatabaseObjectRenderingDescription> getObjectRenderers() {
		return objectRenderers;
	}

	public void setObjectRenderers(List<DatabaseObjectRenderingDescription> objectRenderers) {
		this.objectRenderers = objectRenderers;
	}

	public boolean isProcedureDetailNeedsSpecificName() {
		return procedureDetailNeedsSpecificName;
	}

	public void setProcedureDetailNeedsSpecificName(boolean procedureDetailNeedsSpecificName) {
		this.procedureDetailNeedsSpecificName = procedureDetailNeedsSpecificName;
	}

	/**
	 * Gets query to get constraints
	 */
	public String getConstraintsQuery() {
		return constraintsQuery;
	}

	/**
	 * Sets query to get constraints
	 */
	public void setConstraintsQuery(String constraintsQuery) {
		this.constraintsQuery = constraintsQuery;
	}

	/**
	 * Gets query to get row count.
	 */
	public String getEstimatedRowCountQuery() {
		return estimatedRowCountQuery;
	}

	/**
	 * Sets query to get row count.
	 */
	public void setEstimatedRowCountQuery(String estimatedRowCountQuery) {
		this.estimatedRowCountQuery = estimatedRowCountQuery;
	}

	/**
	 * Gets query to get view text.
	 */
	public String getViewTextOrDDLQuery() {
		return viewTextOrDDLQuery;
	}

	public void setViewTextOrDDLQuery(String viewTextOrDDLQuery) {
		this.viewTextOrDDLQuery = viewTextOrDDLQuery;
	}

	/**
	 * Gets query to get underlying table of a synonym.
	 */
	public String getSynonymTableQuery() {
		return synonymTableQuery;
	}

	/**
	 * Sets query to get underlying table of a synonym.
	 */
	public void setSynonymTableQuery(String synonymTableQuery) {
		this.synonymTableQuery = synonymTableQuery;
	}

	/**
	 * Gets query to get default schema of the user.
	 */
	public String getDefaultSchemaQuery() {
		return defaultSchemaQuery;
	}

	/**
	 * Sets query to get default schema of the user.
	 */
	public void setDefaultSchemaQuery(String defaultSchemaQuery) {
		this.defaultSchemaQuery = defaultSchemaQuery;
	}

	/**
	 * Information about how to to limit transaction size (never <code>null</code>).
	 */
	@XmlElement
	public LimitTransactionSizeInfo getLimitTransactionSize() {
		if (limitTransactionSize == null) {
			limitTransactionSize = new LimitTransactionSizeInfo();
		}
		return limitTransactionSize;
	}

	/**
	 * Information about how to limit transaction size.
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

	public Map<String, String> getSqlExpressionRule() {
		return sqlExpressionRule;
	}

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
	
	public static void setTmpFetchSize(Integer tmpFetchSize) {
		DBMS.tmpFetchSize.set(tmpFetchSize);
	}
	
	public int getLimitedFetchSize(long limit) {
		Integer fSize = getFetchSize();
		if (fSize != null) {
			if (fSize < 0) {
				return fSize;
			}
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
