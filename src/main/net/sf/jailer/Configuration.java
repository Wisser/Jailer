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
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import net.sf.jailer.database.DBMS;
import net.sf.jailer.database.SQLDialect;
import net.sf.jailer.database.Session;
import net.sf.jailer.database.StatisticRenovator;
import net.sf.jailer.database.TemporaryTableManager;
import net.sf.jailer.enhancer.ScriptEnhancer;
import net.sf.jailer.modelbuilder.ModelElementFinder;
import net.sf.jailer.render.DataModelRenderer;

import org.springframework.context.support.AbstractXmlApplicationContext;
import org.springframework.context.support.FileSystemXmlApplicationContext;

/**
 * Reads and holds configuration file <code>jailer.xml</code>.
 * 
 * @author Ralf Wisser
 */
public class Configuration {
    
	/**
     * The scipt-enhancer.
     */
    private static List<ScriptEnhancer> theScriptEnhancer;
    
    /**
     * The renderer.
     */
    private static DataModelRenderer theRenderer;

    /**
     * DB-URL pattern of DBMS for which this holds the configuration.
     */
	private String urlPattern;
	
	/**
     * Test-query for the DBMS for which this holds the configuration.
     */
	private String testQuery;
	
	/**
     * The {@link StatisticRenovator}.
     */
    private StatisticRenovator statisticRenovator;
    
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
    
    public SimpleDateFormat dateFormat = null;

	public char nanoSep = '.';
	public boolean appendNanosToTimestamp = true;
	public boolean appendMillisToTimestamp = false;
	public boolean useToTimestampFunction = false;
	public DateFormat timestampFormat = null;
	public String emptyCLOBValue = null;
	public String emptyNCLOBValue = null;
	public String emptyBLOBValue = null;
	public String toBlob;
	public String toClob;
	public String toNClob;
	public int embeddedLobSizeLimit = 3980;
	public String binaryPattern = "x'%s'";
	public boolean avoidLeftJoin = false;
	public String timestampPattern = null;
	public SQLDialect sqlDialect = new SQLDialect();
	public String rowidName = null;
	public Boolean supportsSchemasInIndexDefinitions = null;
	public boolean useInlineViewsInDataBrowser = true;
	public String virtualColumnsQuery = null;
	public String userDefinedColumnsQuery = null;
	public String identifierQuoteString = "\"";
	public DBMS dbms = DBMS.UNKNOWN;
	
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

	public String rowidType = null;
	
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
	public TemporaryTableManager sessionTemporaryTableManager = null;
	
	/**
	 * Manages transaction local temporary tables.
	 */
	public TemporaryTableManager transactionTemporaryTableManager = null;

	/**
	 * Optional JDBC properties.
	 */
	private Map<String, String> jdbcProperties = null;

	/**
     * Default configuration for unknown DBMS.
     */
	private static final Configuration defaultConfiguration = new Configuration();
    
	/**
	 * If <code>true</code>, the UPK don't preserve order. This minimizes the size of the UPK.
	 */
	private static boolean doMinimizeUPK = false;

	public static Object localEntityGraphConfiguration;
	
	/**
	 * Replacement for null in DBUnit datasets.
	 */
	private static String nullColumnPlaceholder = null;

	private static int columnsPerIFMTable = 8;

	/**
	 * Gets replacement for null in DBUnit datasets.
	 * 
	 * @return replacement for null in DBUnit datasets
	 */
	public static String getNullColumnPlaceholder() {
		return nullColumnPlaceholder;
	}

	/**
	 * Returns <code>true</code>, the UPK don't preserve order. This minimizes the size of the UPK.
	 */
	public static boolean getDoMinimizeUPK() {
		getContext();
		return doMinimizeUPK;
	}
	
    /**
     * Gets all {@link ModelElementFinder}.
     * 
     * @return all {@link ModelElementFinder}
     */
    @SuppressWarnings("unchecked")
	public static List<ModelElementFinder> getModelElementFinder() throws Exception {
        List<ModelElementFinder> modelElementFinder = (List<ModelElementFinder>) getContext().getBean("model-finder");
        return modelElementFinder;
    }
    
	/**
     * The configuration.
     */
    private static AbstractXmlApplicationContext theApplicationContext = null;

	private static synchronized AbstractXmlApplicationContext getContext() {
		loadConfigurationFile();
    	return theApplicationContext;
    }
    
	/**
	 * Loads the configuration file.
	 * 
	 * @param executionContext the command line arguments
	 */
    @SuppressWarnings("unchecked")
	private static synchronized void loadConfigurationFile() {
    	if (theApplicationContext == null) {
    		String configFile = "jailer.xml";
    		if (getConfigurationFolder() != null) {
    			configFile = "file:" + new File(getConfigurationFolder(), configFile).getAbsolutePath();
    		}
	    	theApplicationContext = new FileSystemXmlApplicationContext(configFile);
	    	doMinimizeUPK = Boolean.TRUE.equals(theApplicationContext.getBean("minimize-UPK"));
	        theScriptEnhancer = (List<ScriptEnhancer>) theApplicationContext.getBean("script-enhancer");
	        theRenderer = (DataModelRenderer) theApplicationContext.getBean("renderer");
	        if (theApplicationContext.containsBean("null-column-placeholder")) {
	        	nullColumnPlaceholder = (String) theApplicationContext.getBean("null-column-placeholder");
	        }
	        if (theApplicationContext.containsBean("local-entity-graph")) {
		        localEntityGraphConfiguration = theApplicationContext.getBean("local-entity-graph");
	        }
	        if (theApplicationContext.containsBean("columns-per-import-filter-mapping-table")) {
	        	columnsPerIFMTable = (Integer) theApplicationContext.getBean("columns-per-import-filter-mapping-table");
	        }
    	}
    }
    
    private static File configurationFolder = new File(".");
    
    /**
	 * @return the configurationFolder
	 */
	public static File getConfigurationFolder() {
		return configurationFolder;
	}

	/**
	 * @param configurationFolder the configurationFolder to set
	 */
	public static void setConfigurationFolder(File configurationFolder) {
		Configuration.configurationFolder = configurationFolder;
	}

	/**
     * Holds configurations.
     */
    private static Map<String, Configuration> perUrl = new HashMap<String, Configuration>();
    
    /**
     * Holds configurations.
     */
    private static Map<DBMS, Configuration> perDBMS = new HashMap<DBMS, Configuration>();

	/**
     * Gets DBMS specific configuration.
     * 
     * @param session connected to the DBMS
     * @return configuration for the DBMS to which the {@link Session} is connected to
     */
	@SuppressWarnings("unchecked")
	public static synchronized Configuration forDbms(Session session) {
		if (session == null) {
			return defaultConfiguration;
		}
		if (perUrl.containsKey(session.dbUrl)) {
			return perUrl.get(session.dbUrl);
		}
        if (getContext().containsBean("dbms-configuration")) {
            List<Configuration> cs = (List<Configuration>) getContext().getBean("dbms-configuration");  
            for (Configuration c: cs) {
            	if (Pattern.matches(c.urlPattern, session.dbUrl)) {
            		boolean ok = true;
            		if (c.getTestQuery() != null) {
            			boolean wasSilent = session.getSilent();
            			session.setSilent(true);
            			try {
							session.executeQuery(c.getTestQuery(), new Session.AbstractResultSetReader() {
								@Override
								public void readCurrentRow(ResultSet resultSet) throws SQLException {
								}
							});
						} catch (SQLException e) {
							ok = false;
						}
            			session.setSilent(wasSilent);
            		}
            		if (ok) {
            			perUrl.put(session.dbUrl, c);
            			return c;
            		}
            	}
            }
        }
        perUrl.put(session.dbUrl, defaultConfiguration);
        return defaultConfiguration;
	}

    /**
     * Gets DBMS specific configuration.
     * 
     * @param dbUrl URL
     * @return configuration for the DBMS with given URL
     */
	@SuppressWarnings("unchecked")
	public static Configuration forDbms(String dbUrl) {
		if (dbUrl == null) {
			return defaultConfiguration;
		}
        if (getContext().containsBean("dbms-configuration")) {
            List<Configuration> cs = (List<Configuration>) getContext().getBean("dbms-configuration");  
            for (Configuration c: cs) {
            	if (Pattern.matches(c.urlPattern, dbUrl)) {
        			return c;
        		}
            }
        }
        return defaultConfiguration;
	}
	
	/**
     * Gets DBMS specific configuration.
     * 
     * @param dbms the DBMS
     * @return configuration for the DBMS
     */
	@SuppressWarnings("unchecked")
	public static synchronized Configuration forDbms(DBMS dbms) {
		if (perDBMS.containsKey(dbms)) {
			return perDBMS.get(dbms);
		}
        if (getContext().containsBean("dbms-configuration")) {
            List<Configuration> cs = (List<Configuration>) getContext().getBean("dbms-configuration");  
            for (Configuration c: cs) {
            	if (c.dbms == dbms) {
            		perDBMS.put(dbms, c);
	                return c;
            	}
            }
        }
        perDBMS.put(dbms, defaultConfiguration);
        return defaultConfiguration;
	}

	public Set<String> getExportBlocks() {
		return exportBlocks;
	}
	
	public void setExportBlocks(Set<String> exportBlocks) {
		this.exportBlocks = exportBlocks;
	}
    
    /**
     * Sets DB-URL pattern of DBMS for which this holds the configuration.
     */
	public void setUrlPattern(String urlPattern) {
		this.urlPattern = urlPattern;
	}

	/**
	 * Gets the {@link StatisticRenovator}.
	 * 
	 * @return the {@link StatisticRenovator}
	 */
	public StatisticRenovator getStatisticRenovator() {
		return statisticRenovator;
	}
	
	/**
	 * Sets the {@link StatisticRenovator}.
	 * 
	 * @param statisticRenovator the {@link StatisticRenovator}
	 */
	public void setStatisticRenovator(StatisticRenovator statisticRenovator) {
		this.statisticRenovator = statisticRenovator;
	}

	public void setDateFormat(SimpleDateFormat dateFormat) {
		this.dateFormat = dateFormat;
	}

	public void setNanoSep(char nanoSep) {
		this.nanoSep = nanoSep;
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
	public void setSessionTemporaryTableManager(TemporaryTableManager tableManager) {
		sessionTemporaryTableManager = tableManager;
	}
	
	/**
	 * Sets manager for transaction local temporary tables.
	 */
	public void setTransactionTemporaryTableManager(TemporaryTableManager tableManager) {
		transactionTemporaryTableManager = tableManager;
	}
	
	/**
     * Gets the scipt-enhancer.
     */
    public static List<ScriptEnhancer> getScriptEnhancer() {
    	getContext();
    	return theScriptEnhancer;
    }
    
    /**
     * Gets the renderer.
     */
    public static DataModelRenderer getRenderer() {
    	getContext();
    	return theRenderer;
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

	public String nullableContraint = null;
    
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

	public static int getColumnsPerIFMTable() {
		return columnsPerIFMTable;
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

}
