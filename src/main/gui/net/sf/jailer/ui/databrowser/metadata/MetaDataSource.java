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
import java.util.List;

import net.sf.jailer.database.Session;
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;

/**
 * Meta Data Source.
 *
 * @author Ralf Wisser
 */
public class MetaDataSource {

	/**
	 * The database session.
	 */
	private final Session session;
	
	/**
	 * Name of the data source.
	 */
	final String dataSourceName;
	
	/**
	 * Schemas.
	 */
	private List<MDSchema> schemas = new ArrayList<MDSchema>();
	
	/**
	 * Constructor.
	 * 
	 * @param session the database session
	 * @param dataSourceName name of the data source 
	 */
	public MetaDataSource(Session session, String dataSourceName) {
		this.session = session;
		this.dataSourceName = dataSourceName;
		readSchemas();
	}

	private void readSchemas() {
		List<String> sList = JDBCMetaDataBasedModelElementFinder.getSchemas(session, session.getSchema());
		String defaultSchema = JDBCMetaDataBasedModelElementFinder.getDefaultSchema(session, session.getSchema());
		for (String s: sList) {
			schemas.add(new MDSchema(s, s.equals(defaultSchema), this));
		}
	}

	ResultSet readTables(String schemaPattern) throws SQLException {
		return JDBCMetaDataBasedModelElementFinder.getTables(session, session.getMetaData(), schemaPattern, "%", new String[] { "TABLE" });
	}
	
	/**
	 * @return the schemas
	 */
	public List<MDSchema> getSchemas() {
		return schemas;
	}

	/**
	 * Gets the users default schema.
	 */
	public MDSchema getDefaultSchema() {
		for (MDSchema schema: getSchemas()) {
			if (schema.isDefaultSchema) {
				return schema;
			}
		}
		return null;
	}
	
	/**
	 * Removes all chached data.
	 */
	public void clear() {
		schemas.clear();
		readSchemas();
	}

}
