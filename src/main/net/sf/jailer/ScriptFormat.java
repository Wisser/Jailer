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

/**
 * Enumeration of output formats.
 * 
 * @author Ralf Wisser
 */
public enum ScriptFormat {

	/**
	 * SQL DML.
	 */
	SQL("SQL", "SQL Export", ".sql"),

	/**
	 * Template based XML.
	 */
	XML("XML", "XML Export", ".xml"),
	
	/**
	 * DbUnit's FlatXmlDataSet format.
	 */
	DBUNIT_FLAT_XML("DbUnit flat dataset", "DbUnit flat dataset Export", ".xml");

	/**
	 * Constructor.
	 * 
	 * @param displayName human readable name
	 */
	private ScriptFormat(String displayName, String fileChooserTitle, String fileExtension) {
		this.displayName = displayName;
		this.fileChooserTitle = fileChooserTitle;
		this.fileExtension = fileExtension;
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
	
}
