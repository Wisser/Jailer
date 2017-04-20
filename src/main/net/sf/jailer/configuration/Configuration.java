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

import java.io.File;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import net.sf.jailer.database.DBMS;
import net.sf.jailer.database.DefaultTemporaryTableManager;
import net.sf.jailer.database.Session;
import net.sf.jailer.enhancer.FileBasedScriptEnhancer;
import net.sf.jailer.enhancer.HelperTableEnhancer;
import net.sf.jailer.enhancer.ScriptEnhancer;
import net.sf.jailer.render.HtmlDataModelRenderer;

/**
 * Reads and holds configuration file <code>jailer.xml</code>.
 * 
 * @author Ralf Wisser
 */
@XmlRootElement
public class Configuration {
    
	/**
     * The renderer.
     */
    private HtmlDataModelRenderer renderer;

    /**
     * Default configuration for unknown DBMS.
     */
	private final DBMSConfiguration defaultConfiguration = new DBMSConfiguration();
    
	/**
	 * All {@link DBMSConfiguration}s.
	 */
	private List<DBMSConfiguration> dBMSConfigurations = new ArrayList<DBMSConfiguration>();

	/**
	 * If <code>true</code>, the UPK don't preserve order. This minimizes the size of the UPK.
	 */
	private boolean doMinimizeUPK = false;

	@XmlElement(name = "localDatabase")
	public LocalDatabaseConfiguration localEntityGraphConfiguration;
	
	/**
	 * Replacement for null in DBUnit datasets.
	 */
	private String nullColumnPlaceholder = null;

	private int columnsPerIFMTable = 8;

	/**
	 * Gets replacement for null in DBUnit datasets.
	 * 
	 * @return replacement for null in DBUnit datasets
	 */
	public String getNullColumnPlaceholder() {
		return nullColumnPlaceholder;
	}

	/**
	 * Returns <code>true</code>, the UPK don't preserve order. This minimizes the size of the UPK.
	 */
	public boolean getDoMinimizeUPK() {
		return doMinimizeUPK;
	}
	
    /**
     * The configuration.
     */
    private static Configuration theConfiguration = null;

	/**
     * Gets the configuration.
     */
	public synchronized static Configuration getInstance() {
		loadConfigurationFile();
    	return theConfiguration;
    }
	
	/**
     * Gets the scipt-enhancer.
     */
    public static List<ScriptEnhancer> getScriptEnhancer() {
    	ArrayList<ScriptEnhancer> enhancer = new ArrayList<ScriptEnhancer>();
    	enhancer.add(new FileBasedScriptEnhancer());
    	enhancer.add(new HelperTableEnhancer());
    	return enhancer;
    }
    
	/**
	 * Loads the configuration file.
	 */
	private synchronized static void loadConfigurationFile() {
    	if (theConfiguration == null) {
    		File configFile = new File("jailer.xml");
    		if (getConfigurationFolder() != null) {
    			configFile = new File(getConfigurationFolder(), configFile.getName());
    		}
    		try {
    			JAXBContext jc = JAXBContext.newInstance(Configuration.class, DBMSConfiguration.class, DefaultTemporaryTableManager.class);
    			Unmarshaller um = jc.createUnmarshaller();
    			theConfiguration = (Configuration) um.unmarshal(configFile);
    		} catch (JAXBException e) {
    			throw new RuntimeException(e);
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
	public static void setConfigurationFolder(File theConfigurationFolder) {
		configurationFolder = theConfigurationFolder;
	}

	/**
     * Holds configurations.
     */
    private Map<String, DBMSConfiguration> perUrl = new HashMap<String, DBMSConfiguration>();
    
    /**
     * Holds configurations.
     */
    private Map<DBMS, DBMSConfiguration> perDBMS = new HashMap<DBMS, DBMSConfiguration>();

	/**
     * Gets DBMS specific configuration.
     * 
     * @param session connected to the DBMS
     * @return configuration for the DBMS to which the {@link Session} is connected to
     */
	public synchronized DBMSConfiguration forDbms(Session session) {
		if (session == null) {
			return defaultConfiguration;
		}
		if (perUrl.containsKey(session.dbUrl)) {
			return perUrl.get(session.dbUrl);
		}
        List<DBMSConfiguration> cs = (List<DBMSConfiguration>) getInstance().dBMSConfigurations;  
        for (DBMSConfiguration c: cs) {
        	if (Pattern.matches(c.getUrlPattern(), session.dbUrl)) {
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
        perUrl.put(session.dbUrl, defaultConfiguration);
        return defaultConfiguration;
	}

	/**
     * Gets DBMS specific configuration.
     * 
     * @param dbUrl URL
     * @return configuration for the DBMS with given URL
     */
	public DBMSConfiguration forDbms(String dbUrl) {
		if (dbUrl == null) {
			return defaultConfiguration;
		}
        List<DBMSConfiguration> cs = dBMSConfigurations;  
        for (DBMSConfiguration c: cs) {
        	if (Pattern.matches(c.getUrlPattern(), dbUrl)) {
    			return c;
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
	public synchronized DBMSConfiguration forDbms(DBMS dbms) {
		if (perDBMS.containsKey(dbms)) {
			return perDBMS.get(dbms);
		}
        List<DBMSConfiguration> cs = dBMSConfigurations;  
        for (DBMSConfiguration c: cs) {
        	if (c.getDbms() == dbms) {
        		perDBMS.put(dbms, c);
                return c;
        	}
        }
        perDBMS.put(dbms, defaultConfiguration);
        return defaultConfiguration;
	}

    /**
	 * @return the columnsPerIFMTable
	 */
	public int getColumnsPerIFMTable() {
		return columnsPerIFMTable;
	}

	/**
	 * @return the theRenderer
	 */
	public HtmlDataModelRenderer getRenderer() {
		return renderer;
	}

	/**
	 * @param theRenderer the theRenderer to set
	 */
	public void setRenderer(HtmlDataModelRenderer theRenderer) {
		this.renderer = theRenderer;
	}

	/**
	 * @return the dBMSConfigurations
	 */
	@XmlElement(name = "dbmsConfiguration")
	public List<DBMSConfiguration> getdBMSConfigurations() {
		return dBMSConfigurations;
	}

	/**
	 * @param dBMSConfigurations the dBMSConfigurations to set
	 */
	public void setdBMSConfigurations(List<DBMSConfiguration> dBMSConfigurations) {
		this.dBMSConfigurations = dBMSConfigurations;
	}

	/**
	 * @return the defaultConfiguration
	 */
	public DBMSConfiguration getDefaultConfiguration() {
		return defaultConfiguration;
	}

	/**
	 * @param doMinimizeUPK the doMinimizeUPK to set
	 */
	public void setDoMinimizeUPK(boolean doMinimizeUPK) {
		this.doMinimizeUPK = doMinimizeUPK;
	}

	/**
	 * @param nullColumnPlaceholder the nullColumnPlaceholder to set
	 */
	public void setNullColumnPlaceholder(String nullColumnPlaceholder) {
		this.nullColumnPlaceholder = nullColumnPlaceholder;
	}

	/**
	 * @param columnsPerIFMTable the columnsPerIFMTable to set
	 */
	public void setColumnsPerIFMTable(int columnsPerIFMTable) {
		this.columnsPerIFMTable = columnsPerIFMTable;
	}
	
	
	// TODO

/*
	public static void main(String[] args) throws Exception {
		
		Configuration dest = new Configuration();
		net.sf.jailer.DBMSConfiguration.getScriptEnhancer(); 
		
		for (Field f: net.sf.jailer.DBMSConfiguration.class.getDeclaredFields()) {
			if ("defaultConfiguration".equals(f.getName())) {
				continue;
			}
			if (Modifier.isStatic(f.getModifiers())) {
				f.setAccessible(true);
				try {
					Field declaredField = dest.getClass().getDeclaredField(f.getName());
					declaredField.set(dest, f.get(null));
				} catch (NoSuchFieldException e) {
					
				}
			}
		}
		List<net.sf.jailer.DBMSConfiguration> cs = (List<net.sf.jailer.DBMSConfiguration>) net.sf.jailer.DBMSConfiguration.getContext().getBean("dbms-configuration");  
        
		for (net.sf.jailer.DBMSConfiguration c: cs) {
			net.sf.jailer.configuration.DBMSConfiguration d = new net.sf.jailer.configuration.DBMSConfiguration();
			dest.dBMSConfigurations.add(d);
			for (Field f: net.sf.jailer.DBMSConfiguration.class.getDeclaredFields()) {
				if ("".equals(f.getName())) {
					continue;
				}
				if (!Modifier.isStatic(f.getModifiers())) {
					f.setAccessible(true);
					try {
						Field declaredField = d.getClass().getDeclaredField(f.getName());
						declaredField.setAccessible(true);
						declaredField.set(d, f.get(c));
					} catch (NoSuchFieldException e) {
						
					}
				}
			}
		}
		
		dest.setRenderer((HtmlDataModelRenderer) net.sf.jailer.DBMSConfiguration.getRenderer());
		JAXBContext jc = JAXBContext.newInstance(Configuration.class, DBMSConfiguration.class, DefaultTemporaryTableManager.class);
		Marshaller m = jc.createMarshaller();
		m.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
	    m.marshal(dest, new File("_jailer.xml"));
	}
*/
	
}
