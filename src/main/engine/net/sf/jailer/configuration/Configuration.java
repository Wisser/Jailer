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

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import com.fasterxml.jackson.databind.ObjectMapper;

import net.sf.jailer.CommandLine;
import net.sf.jailer.enhancer.FileBasedScriptEnhancer;
import net.sf.jailer.enhancer.HelperTableEnhancer;
import net.sf.jailer.enhancer.ScriptEnhancer;
import net.sf.jailer.render.HtmlDataModelRenderer;

/**
 * Reads and holds configuration file <code>jailer.json</code>.
 * 
 * @author Ralf Wisser
 */
@JsonPropertyOrder({
	"localEntityGraphConfiguration",
	"nullColumnPlaceholder",
	"doMinimizeUPK",
	"renderer",
	"additionalSQLKeywords",
	"urlRewriteRule",
	"databaseConnectionInteractiveTimeout",
	"generateUpsertStatementsWithoutNulls"
})
@JsonIgnoreProperties({
	"commentNullColumnPlaceholder",
	"commentDoMinimizeUPK",
	"commentRenderer",
	"commentAdditionalSQLKeywords",
	"commentDatabaseConnectionInteractiveTimeout",
	"commentColumnsPerIFMTable"
})
public class Configuration {
	
	/**
	 * Gets the temporary files folder. Defaults to 'tmp'.
	 * 
	 * @return the tempFileFolder
	 */
	public String getTempFileFolder() {
		return tempFileFolder;
	}

	/**
	 * Sets the temporary files folder. Defaults to 'tmp'.
	 * 
	 * @param tempFileFolder the tempFileFolder to set
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
	 * Checks whether the given file is a temporary file managed by this configuration.
	 *
	 * @param file the file to check
	 * @return <code>true</code> if the file is a managed temporary file
	 */
	public boolean isTempFile(File file) {
		try {
			return new File(getTempFileFolder()).equals(file.getParentFile()) && file.getName().startsWith("up-");
		} catch (Throwable t) {
			return false;
		}
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
	 * URL rewrite rule.
	 */
	@JsonIgnoreProperties({"comment"})
	public static class UrlRewriteRule {
		private String pattern;
		private String replacement;

		/**
		 * Gets the URL pattern to match.
		 *
		 * @return the URL pattern
		 */
		public String getPattern() {
			return pattern;
		}

		/**
		 * Sets the URL pattern to match.
		 *
		 * @param pattern the URL pattern to set
		 */
		public void setPattern(String pattern) {
			this.pattern = pattern;
		}

		/**
		 * Gets the replacement string.
		 *
		 * @return the replacement string
		 */
		public String getReplacement() {
			return replacement;
		}

		/**
		 * Sets the replacement string.
		 *
		 * @param replacement the replacement string to set
		 */
		public void setReplacement(String replacement) {
			this.replacement = replacement;
		}
	}

	/**
	 * All URL rewrite rules.
	 */
	private List<UrlRewriteRule> urlRewriteRules = new ArrayList<UrlRewriteRule>();

	/**
	 * All {@link DBMS}s.
	 */
	private List<DBMS> dBMSConfigurations = new ArrayList<DBMS>();
	
	/**
	 * If <code>true</code>, the UPK don't preserve order. This minimizes the size of the UPK.
	 */
	private boolean doMinimizeUPK = false;

	public LocalDatabaseConfiguration localEntityGraphConfiguration;
	
	/**
	 * Replacement for null in DBUnit datasets.
	 */
	private String nullColumnPlaceholder = null;

	/**
	 * Generate upsert statements without nulls?
	 */
	private boolean generateUpsertStatementsWithoutNulls = true;

	private int columnsPerIFMTable = 8;

	/**
	 * Time in seconds waiting for an idle database connection to be tested if it is still valid.
	 */
	private int databaseConnectionInteractiveTimeout = 240;

	private String additionalSQLKeywords;

	/**
	 * Gets additional SQL keywords.
	 *
	 * @return the additional SQL keywords
	 */
	public String getAdditionalSQLKeywords() {
		return additionalSQLKeywords;
	}

	/**
	 * Sets additional SQL keywords.
	 *
	 * @param additionalSQLKeywords the additional SQL keywords to set
	 */
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
	 * Returns <code>true</code> if the UPK doesn't preserve order. This minimizes the size of the UPK.
	 *
	 * @return <code>true</code> if the UPK doesn't preserve order
	 */
	public boolean getDoMinimizeUPK() {
		return doMinimizeUPK;
	}
	
	/**
	 * Gets time in seconds waiting for an idle database connection to be tested if it is still valid.
	 *
	 * @return the timeout in seconds
	 */
	public int getDatabaseConnectionInteractiveTimeout() {
		return databaseConnectionInteractiveTimeout;
	}

	/**
	 * Sets time in seconds waiting for an idle database connection to be tested if it is still valid.
	 *
	 * @param databaseConnectionInteractiveTimeout the timeout in seconds
	 */
	public void setDatabaseConnectionInteractiveTimeout(int databaseConnectionInteractiveTimeout) {
		this.databaseConnectionInteractiveTimeout = databaseConnectionInteractiveTimeout;
	}

	/**
	 * The configuration.
	 */
	private static Configuration theConfiguration = null;

	/**
	 * Gets the configuration.
	 *
	 * @return the singleton {@link Configuration} instance
	 */
	public synchronized static Configuration getInstance() {
		loadConfigurationFile();
		return theConfiguration;
	}
	
	/**
	 * Gets the script-enhancer.
	 *
	 * @return list of {@link ScriptEnhancer}s
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
			String name = "jailer.json";
			try {
				InputStream res;
				File configFile = null;
				if (configFile == null) {
					if (CommandLine.applicationBase == null) {
						configFile = new File(name);
					} else {
						configFile = new File(CommandLine.applicationBase, name);
					}
				}
				if (!configFile.exists()) {
					res = Configuration.class.getResourceAsStream(name);
				} else {
					res = new FileInputStream(configFile);
				}
				theConfiguration = new ObjectMapper().readValue(res, Configuration.class);
				res.close();
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		}
	}

	/**
	 * Gets the number of columns per import-filter mapping table.
	 *
	 * @return the number of columns per import-filter mapping table
	 */
	public int getColumnsPerIFMTable() {
		return columnsPerIFMTable;
	}

	/**
	 * Gets the HTML data model renderer.
	 *
	 * @return the renderer
	 */
	public HtmlDataModelRenderer getRenderer() {
		return renderer;
	}

	/**
	 * Sets the HTML data model renderer.
	 *
	 * @param theRenderer the renderer to set
	 */
	public void setRenderer(HtmlDataModelRenderer theRenderer) {
		this.renderer = theRenderer;
	}

	/**
	 * Gets all URL rewrite rules.
	 *
	 * @return the URL rewrite rules
	 */
	public List<UrlRewriteRule> getUrlRewriteRules() {
		return urlRewriteRules;
	}

	/**
	 * Sets all URL rewrite rules.
	 *
	 * @param urlRewriteRules the URL rewrite rules to set
	 */
	public void setUrlRewriteRules(List<UrlRewriteRule> urlRewriteRules) {
		this.urlRewriteRules = urlRewriteRules;
	}

	/**
	 * Gets all DBMS configurations.
	 *
	 * @return the list of DBMS configurations
	 */
	public List<DBMS> getDBMS() {
		return dBMSConfigurations;
	}

	/**
	 * Sets all DBMS configurations.
	 *
	 * @param dBMSConfigurations the list of DBMS configurations to set
	 */
	public void setdBMSConfigurations(List<DBMS> dBMSConfigurations) {
		this.dBMSConfigurations = dBMSConfigurations;
	}

	/**
	 * Sets whether the UPK should not preserve order in order to minimize its size.
	 *
	 * @param doMinimizeUPK <code>true</code> to minimize the UPK size
	 */
	public void setDoMinimizeUPK(boolean doMinimizeUPK) {
		this.doMinimizeUPK = doMinimizeUPK;
	}

	/**
	 * Sets the replacement for null in DBUnit datasets.
	 *
	 * @param nullColumnPlaceholder the replacement string for null values
	 */
	public void setNullColumnPlaceholder(String nullColumnPlaceholder) {
		this.nullColumnPlaceholder = nullColumnPlaceholder;
	}

	/**
	 * Sets the number of columns per import-filter mapping table.
	 *
	 * @param columnsPerIFMTable the number of columns per import-filter mapping table
	 */
	public void setColumnsPerIFMTable(int columnsPerIFMTable) {
		this.columnsPerIFMTable = columnsPerIFMTable;
	}

	/**
	 * Returns whether upsert statements are generated without null values.
	 *
	 * @return <code>true</code> if upsert statements are generated without nulls
	 */
	public boolean isGenerateUpsertStatementsWithoutNulls() {
		return generateUpsertStatementsWithoutNulls;
	}

	/**
	 * Sets whether upsert statements should be generated without null values.
	 *
	 * @param generateUpsertStatementsWithoutNulls <code>true</code> to generate upsert statements without nulls
	 */
	public void setGenerateUpsertStatementsWithoutNulls(boolean generateUpsertStatementsWithoutNulls) {
		this.generateUpsertStatementsWithoutNulls = generateUpsertStatementsWithoutNulls;
	}
	
	static {
		DBMS.values(); // trigger static init
	}

}
