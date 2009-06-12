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
import java.util.List;
import java.util.Map;
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
    
    private boolean identityInserts = false;
    
    public SimpleDateFormat dateFormat = null;

	public char nanoSep = '.';
	public boolean appendNanosToTimestamp = false;
	public boolean appendMillisToTimestamp = false;
	public boolean useToTimestampFunction = false;
	public DateFormat timestampFormat = null;
	public String emptyCLOBValue = null;
	public String emptyBLOBValue = null;
	public String binaryPattern = "x'%s'";
	
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
    public static List<ModelElementFinder> getModelElementFinder() throws Exception {
        List<ModelElementFinder> modelElementFinder = (List<ModelElementFinder>) getContext().getBean("model-finder");
        return modelElementFinder;
    }
    
	/**
     * The configuration.
     */
    private static AbstractXmlApplicationContext theApplicationContext = null;
    private static AbstractXmlApplicationContext getContext() {
    	if (theApplicationContext == null) {
    		String configFile = "jailer.xml";
	    	theApplicationContext = new FileSystemXmlApplicationContext(configFile);
	    	doMinimizeUPK = Boolean.TRUE.equals(theApplicationContext.getBean("minimize-UPK"));
	        theScriptEnhancer = (List<ScriptEnhancer>) theApplicationContext.getBean("script-enhancer");
	        theRenderer = (DataModelRenderer) theApplicationContext.getBean("renderer");
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
     * @param statementExecutor connected to the DBMS
     * @return configuration for the DBMS to which the {@link Session} is connected to
     */
	public static Configuration forDbms(Session statementExecutor) {
		if (statementExecutor == null) {
			return defaultConfiguration;
		}
		if (perUrl.containsKey(statementExecutor.dbUrl)) {
			return perUrl.get(statementExecutor.dbUrl);
		}
        if (getContext().containsBean("dbms-configuration")) {
            List<Configuration> cs = (List<Configuration>) getContext().getBean("dbms-configuration");  
            for (Configuration c: cs) {
            	if (Pattern.matches(c.urlPattern, statementExecutor.dbUrl)) {
            		perUrl.put(statementExecutor.dbUrl, c);
	                return c;
            	}
            }
        }
        perUrl.put(statementExecutor.dbUrl, defaultConfiguration);
        return defaultConfiguration;
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
    
}
