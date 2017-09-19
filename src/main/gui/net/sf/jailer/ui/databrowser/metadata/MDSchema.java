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
package net.sf.jailer.ui.databrowser.metadata;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/**
 * Information about a database schema.
 * 
 * @author Ralf Wisser
 */
public class MDSchema extends MDObject {

	final boolean isDefaultSchema;
	private List<MDTable> tables;
	
	/**
	 * Constructor.
	 * 
	 * @param name schema name
	 * @param isDefaultSchema <code>true</code> iff it's the users default schema
	 * @param metaDataSource the source
	 */
	public MDSchema(String name, boolean isDefaultSchema, MetaDataSource metaDataSource) {
		super(name, metaDataSource);
		this.isDefaultSchema = isDefaultSchema;
	}
	
	/**
	 * Gets tables of schema
	 * 
	 * @return tables of schema
	 */
	public synchronized List<MDTable> getTables() throws SQLException {
		if (tables == null) {
			tables = new ArrayList<MDTable>();
			ResultSet rs = getMetaDataSource().readTables(getName());
			while (rs.next()) {
				String tableName = rs.getString(3);
				tables.add(new MDTable(tableName, this));
			}
			rs.close();
			Collections.sort(tables, new Comparator<MDTable>() {
				@Override
				public int compare(MDTable o1, MDTable o2) {
					return o1.getName().compareTo(o2.getName());
				}
			});
		}
		return tables;
	}
}
