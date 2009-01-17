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

import net.sf.jailer.database.StatementExecutor;
import net.sf.jailer.database.StatisticRenovator;
import net.sf.jailer.datamodel.PrimaryKeyFactory;
import net.sf.jailer.enhancer.ScriptEnhancer;
import net.sf.jailer.render.DataModelRenderer;

import org.springframework.context.support.AbstractXmlApplicationContext;
import org.springframework.context.support.FileSystemXmlApplicationContext;

/**
 * Reads and holds configuration file <code>config/config.xml</code>.
 * 
 * @author Ralf Wisser
 */
public class Configuration {
    
	/**
     * The scipt-enhancer.
     */
    public static final List<ScriptEnhancer> scriptEnhancer;
    
    /**
     * The renderer.
     */
    public static final DataModelRenderer renderer;

    /**
     * DB-URL pattern of DBMS for which this holds the configuration.
     */
	private String urlPattern;
	
	/**
     * The {@link StatisticRenovator}.
     */
    private StatisticRenovator statisticRenovator;
    
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
     * Default configuration for unknown DBMS.
     */
	private static final Configuration defaultConfiguration = new Configuration();
    
	/**
     * The configuration.
     */
    private static AbstractXmlApplicationContext applicationContext = new FileSystemXmlApplicationContext("config/config.xml");
    static {
		PrimaryKeyFactory.minimizeUPK = Boolean.TRUE.equals(applicationContext.getBean("minimize-UPK"));
        scriptEnhancer = (List<ScriptEnhancer>) applicationContext.getBean("script-enhancer");
        renderer = (DataModelRenderer) applicationContext.getBean("renderer");
	}

    /**
     * Holds configurations.
     */
    private static Map<String, Configuration> perUrl = new HashMap<String, Configuration>();
    
    /**
     * Gets DBMS specific configuration.
     * 
     * @param statementExecutor connected to the DBMS
     * @return configuration for the DBMS to which the {@link StatementExecutor} is connected to
     */
	public static Configuration forDbms(StatementExecutor statementExecutor) {
		if (perUrl.containsKey(statementExecutor.dbUrl)) {
			return perUrl.get(statementExecutor.dbUrl);
		}
        if (applicationContext.containsBean("dbms-configuration")) {
            List<Configuration> cs = (List<Configuration>) applicationContext.getBean("dbms-configuration");  
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
	
}
