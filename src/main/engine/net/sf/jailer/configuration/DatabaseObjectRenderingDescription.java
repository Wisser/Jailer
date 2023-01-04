/*
 * Copyright 2007 - 2023 Ralf Wisser.
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

/**
 * Describes how generic database objects are rendered (Functions, Procedures etc).
 * 
 * @author Ralf Wisser
 */
public class DatabaseObjectRenderingDescription {
	
	/**
	 * Query for a list of all objects.
	 */
	private String listQuery;
	
	/**
	 * Query for a textual representation of all objects.
	 */
	private String textQuery;
	
	/**
	 * Icon.
	 */
	private String iconURL;

	/**
	 * Name.
	 */
	private String name;

	private boolean expensive = false;
	
	/**
	 * Description of a list item.
	 */
	private DatabaseObjectRenderingDescription itemDescription;

	/**
	 * Constructor.
	 */
	public DatabaseObjectRenderingDescription() {
	}

	/**
	 * Copy constructor.
	 */
	public DatabaseObjectRenderingDescription(DatabaseObjectRenderingDescription other) {
		iconURL = other.iconURL;
		itemDescription = other.itemDescription;
		listQuery = other.listQuery;
		textQuery = other.textQuery;
		name = other.name;
	}

	/**
	 * Gets query for a list of all objects.
	 * 
	 * @return query for a list of all objects
	 */
	public String getListQuery() {
		return listQuery;
	}

	/**
	 * Sets query for a list of all objects.
	 * 
	 * @param listQuery query for a list of all objects
	 */
	public void setListQuery(String listQuery) {
		this.listQuery = listQuery;
	}
	
	/**
	 * Gets description of a list item.
	 * 
	 * @return description of a list item
	 */
	public DatabaseObjectRenderingDescription getItemDescription() {
		return itemDescription;
	}

	/**
	 * Sets description of a list item.
	 * 
	 * @param itemDescription description of a list item
	 */
	public void setItemDescription(DatabaseObjectRenderingDescription itemDescription) {
		this.itemDescription = itemDescription;
	}

	/**
	 * Gets query for a textual representation of all objects.
	 * 
	 * @return query for a textual representation of all objects
	 */
	public String getTextQuery() {
		return textQuery;
	}

	/**
	 * Sets query for a textual representation of all objects.
	 * 
	 * @param textQuery query for a textual representation of all objects
	 */
	public void setTextQuery(String textQuery) {
		this.textQuery = textQuery;
	}

	/**
	 * Gets the name.
	 * 
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * Sets the name.
	 * 
	 * @param name the name
	 */
	public void setName(String name) {
		this.name = name;
	}

	public String getIconURL() {
		return iconURL;
	}

	public void setIconURL(String iconURL) {
		this.iconURL = iconURL;
	}

	public boolean isCheap() {
		return !expensive;
	}

	public void setCheap(boolean cheap) {
		expensive = !cheap;
	}

}
