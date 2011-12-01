/*
 * Copyright 2007 the original author or authors.
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

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

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
    
    /**
     * Maps characters to escape sequences according to {@link #stringLiteralEscapeSequences}.
     */
    private Map<Character, String> charToEscapeSequence = new HashMap<Character, String>();
    { charToEscapeSequence.put('\'', "''"); }
    private char[] keysOfCharToEscapeSequence = new char[] { '\'' };
    
    /**
     * Set of type names for which no data must be exported.
     */
    public Set<String> exportBlocks = new HashSet<String>();
    
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
	public String emptyBLOBValue = null;
	public String binaryPattern = "x'%s'";
	public boolean avoidLeftJoin = false;
	
	/**
	 * Manages session local temporary tables.
	 */
	public TemporaryTableManager sessionTemporaryTableManager = null;
	
	/**
	 * Manages transaction local temporary tables.
	 */
	public TemporaryTableManager transactionTemporaryTableManager = null;
	
    /**
     * Default configuration for unknown DBMS.
     */
	private static final Configuration defaultConfiguration = new Configuration();
    
	/**
	 * If <code>true</code>, the UPK don't preserve order. This minimizes the size of the UPK.
	 */
	private static boolean doMinimizeUPK = false;

	/**
	 * Replacement for null in DBUnit datasets.
	 */
	private static String nullColumnPlaceholder = null;

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
    @SuppressWarnings("unchecked")
	private static synchronized AbstractXmlApplicationContext getContext() {
    	if (theApplicationContext == null) {
    		String configFile = "jailer.xml";
    		if (CommandLineParser.getInstance().getWorkingfolder() != null) {
    			configFile = "file:" + CommandLineParser.getInstance().newFile(configFile).getAbsolutePath();
    		}
	    	theApplicationContext = new FileSystemXmlApplicationContext(configFile);
	    	doMinimizeUPK = Boolean.TRUE.equals(theApplicationContext.getBean("minimize-UPK"));
	        theScriptEnhancer = (List<ScriptEnhancer>) theApplicationContext.getBean("script-enhancer");
	        theRenderer = (DataModelRenderer) theApplicationContext.getBean("renderer");
	        if (theApplicationContext.containsBean("null-column-placeholder")) {
	        	nullColumnPlaceholder = (String) theApplicationContext.getBean("null-column-placeholder");
	        }
    	}
    	return theApplicationContext;
    }

    /**
     * Holds configurations.
     */
    private static Map<String, Configuration> perUrl = new HashMap<String, Configuration>();
    
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
            		perUrl.put(session.dbUrl, c);
	                return c;
            	}
            }
        }
        perUrl.put(session.dbUrl, defaultConfiguration);
        return defaultConfiguration;
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
    
}
