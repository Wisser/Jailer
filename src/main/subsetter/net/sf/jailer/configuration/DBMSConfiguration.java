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
package net.sf.jailer.configuration;

import java.text.DateFormat;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.xml.bind.annotation.XmlType;

import net.sf.jailer.database.DBMS;
import net.sf.jailer.database.DefaultTemporaryTableManager;
import net.sf.jailer.database.SQLDialect;
import net.sf.jailer.database.SqlScriptBasedStatisticRenovator;

/**
 * Reads and holds configuration file <code>jailer.xml</code>.
 * 
 * @author Ralf Wisser
 */
@XmlType(propOrder = {
		"dbms",
		"urlPattern",
		"testQuery",
		"sqlDialect",
		"jdbcProperties",
		"statisticRenovator",
		"typeReplacement",
		"stringLiteralEscapeSequences",
		"sqlLimitSuffix",
		"varcharLengthLimit",
		"tableProperties",
		"ncharPrefix",
		"sessionTemporaryTableManager",
		"transactionTemporaryTableManager",
		"exportBlocks",
		"identityInserts",
		"appendNanosToTimestamp",
		"appendMillisToTimestamp",
		"useToTimestampFunction",
		"timestampPattern",
		"timestampFormat",
		"emptyBLOBValue",
		"emptyCLOBValue",
		"emptyNCLOBValue",
		"toBlob",
		"toClob",
		"toNClob",
		"embeddedLobSizeLimit",
		"binaryPattern",
		"avoidLeftJoin",
		"rowidName",
		"rowidType",
		"supportsSchemasInIndexDefinitions",
		"useInlineViewsInDataBrowser",
		"virtualColumnsQuery",
		"userDefinedColumnsQuery",
		"importedKeysQuery",
		"primaryKeysQuery",
		"indexInfoQuery",
		"nullableContraint",
		"identifierQuoteString" })

public class DBMSConfiguration {

	private DBMS dbms = DBMS.UNKNOWN;
    
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
    private Map<String, String> typeReplacement;
    
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
    
	private boolean appendNanosToTimestamp = true;
	private boolean appendMillisToTimestamp = false;
	private boolean useToTimestampFunction = false;
	private DateFormat timestampFormat = null;
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
	private SQLDialect sqlDialect = new SQLDialect();
	private String rowidName = null;
	private Boolean supportsSchemasInIndexDefinitions = null;
	private boolean useInlineViewsInDataBrowser = true;
	private String virtualColumnsQuery = null;
	private String userDefinedColumnsQuery = null;
	
	private String importedKeysQuery = 
			  "SELECT null, PKCU.TABLE_SCHEMA, PKCU.TABLE_NAME, PKCU.COLUMN_NAME,"
			+ "       null, KCU.TABLE_SCHEMA, KCU.TABLE_NAME, KCU.COLUMN_NAME, KCU.ORDINAL_POSITION,"
			+ "       null, null, RC.CONSTRAINT_NAME, RC.UNIQUE_CONSTRAINT_NAME, null"
			+ " FROM INFORMATION_SCHEMA.REFERENTIAL_CONSTRAINTS RC"
			+ " JOIN INFORMATION_SCHEMA.KEY_COLUMN_USAGE KCU"
			+ "   ON KCU.CONSTRAINT_CATALOG = RC.CONSTRAINT_CATALOG"
			+ "      AND KCU.CONSTRAINT_SCHEMA = RC.CONSTRAINT_SCHEMA"
			+ "      AND KCU.CONSTRAINT_NAME = RC.CONSTRAINT_NAME"
			+ " JOIN INFORMATION_SCHEMA.KEY_COLUMN_USAGE PKCU"
			+ "   ON PKCU.CONSTRAINT_CATALOG = RC.UNIQUE_CONSTRAINT_CATALOG"
			+ "      AND PKCU.CONSTRAINT_SCHEMA = RC.UNIQUE_CONSTRAINT_SCHEMA"
			+ "      AND PKCU.CONSTRAINT_NAME = RC.UNIQUE_CONSTRAINT_NAME"
			+ "      AND PKCU.ORDINAL_POSITION = KCU.ORDINAL_POSITION"
			+ " WHERE PKCU.TABLE_SCHEMA = '${SCHEMA}'"
			+ " ORDER BY KCU.ORDINAL_POSITION";
	
	private String primaryKeysQuery = 
			  "SELECT null, KCU.TABLE_SCHEMA, KCU.TABLE_NAME, KCU.COLUMN_NAME, KCU.ORDINAL_POSITION, C.CONSTRAINT_NAME"
			+ "  FROM"
			+ "  INFORMATION_SCHEMA.TABLE_CONSTRAINTS C"
			+ "  JOIN INFORMATION_SCHEMA.KEY_COLUMN_USAGE KCU"
			+ "    ON KCU.CONSTRAINT_CATALOG = C.CONSTRAINT_CATALOG"
			+ "      AND KCU.CONSTRAINT_SCHEMA = C.CONSTRAINT_SCHEMA"
			+ "      AND KCU.CONSTRAINT_NAME = C.CONSTRAINT_NAME"
			+ "  WHERE C.CONSTRAINT_TYPE = 'PRIMARY KEY'"
			+ "      AND  KCU.TABLE_SCHEMA = '${SCHEMA}'"
			+ "  ORDER BY KCU.ORDINAL_POSITION";
	
	private String indexInfoQuery = null;
	private String identifierQuoteString = "\"";
	/**
	 * @return the dbms
	 */
	public DBMS getDbms() {
		return dbms;
	}

	/**
	 * @param dbmsEnum the dbmsEnum to set
	 */
	public void setDbms(DBMS dbms) {
		this.dbms = dbms;
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

	public void setAppendNanosToTimestamp(boolean appendNanosToTimestamp) {
		this.appendNanosToTimestamp = appendNanosToTimestamp;
	}

	public void setAppendMillisToTimestamp(boolean appendMillisToTimestamp) {
		this.appendMillisToTimestamp = appendMillisToTimestamp;
	}

	public void setUseToTimestampFunction(boolean useToTimestampFunction) {
		this.useToTimestampFunction = useToTimestampFunction;
	}

	public void setTimestampFormat(DateFormat timestampFormat) {
		this.timestampFormat = timestampFormat;
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
    	return typeReplacement;
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
        		qvalue.append(es);
        	} else {
        		qvalue.append(c);
        	}
        }
        return qvalue.toString();
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
	 * @return the appendNanosToTimestamp
	 */
	public boolean isAppendNanosToTimestamp() {
		return appendNanosToTimestamp;
	}

	/**
	 * @return the appendMillisToTimestamp
	 */
	public boolean isAppendMillisToTimestamp() {
		return appendMillisToTimestamp;
	}

	/**
	 * @return the useToTimestampFunction
	 */
	public boolean isUseToTimestampFunction() {
		return useToTimestampFunction;
	}

	/**
	 * @return the timestampFormat
	 */
	public DateFormat getTimestampFormat() {
		return timestampFormat;
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

}
