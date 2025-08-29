/*
 * Copyright 2007 - 2025 Ralf Wisser.
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
package net.sf.jailer.subsetting;

/**
 * Enumeration of output formats.
 * 
 * @author Ralf Wisser
 */
public enum ScriptFormat {

	/**
	 * SQL DML.
	 */
	SQL("SQL", "SQL Export", ".sql", false, false, false),

	/**
	 * Intra database export.
	 */
	INTRA_DATABASE("Schema in same database", "Intra Database Export - Receipt File", ".txt", false, true, false),
	
	/**
	 * Template based JSON.
	 */
	JSON("JSON", "JSON Export", ".json", true, false, true),

	/**
	 * Template based YAML.
	 */
	YAML("YAML", "YAML Export", ".yaml", true, false, true),

	/**
	 * Template based XML.
	 */
	XML("XML", "XML Export", ".xml", true, true, false),

	/**
	 * DbUnit's FlatXmlDataSet format.
	 */
	DBUNIT_FLAT_XML("DbUnit flat dataset", "DbUnit flat dataset Export", ".xml", false, false, false),
	
	
	LIQUIBASE_XML("Liquibase", "Liquibase Export", ".xml", false, false, false);

	/**
	 * Constructor.
	 * 
	 * @param displayName human readable name
	 */
	private ScriptFormat(String displayName, String fileChooserTitle, String fileExtension, boolean isObjectNotation, boolean separatorFollowed, boolean usesUTF8) {
		this.displayName = displayName;
		this.fileChooserTitle = fileChooserTitle;
		this.fileExtension = fileExtension;
		this.isObjectNotation = isObjectNotation;
		this.separatorFollowed = separatorFollowed;
		this.usesUTF8 = usesUTF8;
	}

	/**
	 * Gets human readable name.
	 */
	public String getDisplayName() {
		return displayName;
	}
	
	/**
	 * Human readable name.
	 */
	private final String displayName;

	/**
	 * File chooser title.
	 */
	private final String fileChooserTitle;

	/**
	 * File extension.
	 */
	private final String fileExtension;
	
	/**
	 * Is object notation format? (XML, jSON, YAML, ...)
	 */
	private final boolean isObjectNotation;
	
	/**
	 * Uses UTF-8 charset?
	 */
	private final boolean usesUTF8;
	
	/**
	 * Is followed by separator in drop-down-list?
	 */
	public final boolean separatorFollowed;
	
	/**
	 * Is object notation format) (XML, jSON, YAML, ...)
	 */
	public boolean isObjectNotation() {
		return isObjectNotation;
	}

	/**
	 * Gets file chooser title.
	 * 
	 * @return file chooser title
	 */
	public String getFileChooserTitle() {
		return fileChooserTitle;
	}
	
	/**
	 * Gets file extension.
	 * 
	 * @return file extension
	 */
	public String getFileExtension() {
		return fileExtension;
	}
	
	/**
	 * Uses UTF-8 charset?
	 */
	public boolean isUsesUTF8() {
		return usesUTF8;
	}

}
