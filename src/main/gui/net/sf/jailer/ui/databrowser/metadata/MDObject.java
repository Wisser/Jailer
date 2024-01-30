/*
 * Copyright 2007 - 2024 Ralf Wisser.
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
package net.sf.jailer.ui.databrowser.metadata;

import net.sf.jailer.util.Quoting;

/**
 * Information about a database object.
 * 
 * @author Ralf Wisser
 */
public class MDObject {

	private final String name;
	private final MetaDataSource metaDataSource;

	/**
	 * Constructor.
	 * 
	 * @param name the object name
	 */
	public MDObject(String name, MetaDataSource metaDataSource) {
		this.name = name;
		this.metaDataSource = metaDataSource;
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
	 * Gets the unquoted name.
	 * 
	 * @return the unquoted name
	 */
	public String getUnquotedName() {
		return Quoting.staticUnquote(name);
	}

	/**
	 * Gets the source.
	 * 
	 * @return the source
	 */
	public MetaDataSource getMetaDataSource() {
		return metaDataSource;
	}
	
	@Override
	public String toString() {
		return getUnquotedName();
	}
	
}
