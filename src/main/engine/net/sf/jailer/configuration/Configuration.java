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
package net.sf.jailer.configuration;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import net.sf.jailer.database.DefaultTemporaryTableManager;
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
	 * Gets the temporary files folder. Defaults to 'tmp'.
	 * 
	 * @return the tempFileFolder absolute or relative to {@link #getWorkingFolder()}
	 */
	public String getTempFileFolder() {
		return tempFileFolder;
	}

	/**
	 * Sets the temporary files folder. Defaults to 'tmp'.
	 * 
	 * @param tempFileFolder absolute or relative to {@link #getWorkingFolder()}
	 */
	public void setTempFileFolder(String tempFileFolder) {
		this.tempFileFolder = tempFileFolder;
	}

	/**
	 * Creates a temporary file
	 * 
	 * @return a temporary file
	 */
	public File createTempFile() {
		String file;
		String ts = UUID.randomUUID().toString();
		File newFile;
		for (int i = 1; ; ++i) {
			file = getTempFileFolder();
			newFile = new File(file);
			newFile.mkdirs();
			file += File.separator + "up" + "-" + ts + (i > 1? "-" + Integer.toString(i) : "");
			newFile = new File(file);
			if (!newFile.exists()) {
				break;
			}
		}
		return new File(file);
	}
	
	/**
	 * The temporary files folder. Defaults to 'tmp'.
	 */
	private String tempFileFolder = "tmp";
	
	/**
	 * The renderer.
	 */
	private HtmlDataModelRenderer renderer;
	
	/**
	 * All {@link DBMS}s.
	 */
	private List<DBMS> dBMSConfigurations = new ArrayList<DBMS>();

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


	private String additionalSQLKeywords;

	public String getAdditionalSQLKeywords() {
		return additionalSQLKeywords;
	}

	public void setAdditionalSQLKeywords(String additionalSQLKeywords) {
		this.additionalSQLKeywords = additionalSQLKeywords;
	}

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
			String name = "jailer.xml";
			try {
				InputStream res;
				File configFile = new File(name);
				if (!configFile.exists()) {
					res = Configuration.class.getResourceAsStream(name);
				} else {
					res = new FileInputStream(configFile);
				}
				JAXBContext jc = JAXBContext.newInstance(Configuration.class, DBMS.class, DefaultTemporaryTableManager.class);
				Unmarshaller um = jc.createUnmarshaller();
				theConfiguration = (Configuration) um.unmarshal(res);
				res.close();
			} catch (JAXBException e) {
				throw new RuntimeException(e);
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
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
	@XmlElement(name = "dbms")
	public List<DBMS> getDBMS() {
		return dBMSConfigurations;
	}

	/**
	 * @param dBMSConfigurations the dBMSConfigurations to set
	 */
	public void setdBMSConfigurations(List<DBMS> dBMSConfigurations) {
		this.dBMSConfigurations = dBMSConfigurations;
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

	static {
		DBMS.values(); // trigger static init
	}
}
