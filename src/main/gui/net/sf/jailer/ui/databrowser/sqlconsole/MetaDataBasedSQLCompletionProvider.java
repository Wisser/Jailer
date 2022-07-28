/*
 * Copyright 2007 - 2022 Ralf Wisser.
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
package net.sf.jailer.ui.databrowser.sqlconsole;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;

import javax.swing.JComponent;

import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.databrowser.metadata.MDSchema;
import net.sf.jailer.ui.databrowser.metadata.MDTable;
import net.sf.jailer.ui.databrowser.metadata.MetaDataSource;
import net.sf.jailer.ui.syntaxtextarea.SQLCompletionProvider;

/**
 * A {@link SQLCompletionProvider} based on {@link SQLCompletionProvider}.
 * 
 * @author Ralf Wisser
 */
public class MetaDataBasedSQLCompletionProvider extends SQLCompletionProvider<MetaDataSource, MDSchema, MDTable> {

	public MetaDataBasedSQLCompletionProvider(Session session, MetaDataSource metaDataSource) throws SQLException {
		super(session, metaDataSource);
	}

	@Override
	protected List<String> getColumns(MDTable table, long timeOut, JComponent waitCursorSubject) {
		try {
			return table.getColumns(timeOut, waitCursorSubject);
		} catch (SQLException e) {
			return Collections.emptyList();
		}
	}

	@Override
	protected MDSchema getDefaultSchema(MetaDataSource metaDataSource) {
		return metaDataSource.isInitialized()? metaDataSource.getDefaultSchema() : null;
	}

	@Override
	protected MDSchema findSchema(MetaDataSource metaDataSource, String name) {
		return metaDataSource.isInitialized()?  metaDataSource.find(name) : null;
	}

	@Override
	protected MDTable findTable(MDSchema schema, String name) {
		if (!schema.isLoaded()) {
			getTables(schema);
		}
		if (!schema.isLoaded()) {
			return null;
    	}
		return schema.find(name);
	}

	@Override
	protected String getTableName(MDTable table) {
		return table.getName();
	}

	private Map<MDSchema, MDSchema> triedToLoad = new WeakHashMap<MDSchema, MDSchema>();
	
	@Override
	protected List<MDTable> getTables(MDSchema schema) {
		if (!schema.isLoaded()) {
			if (!triedToLoad.containsKey(schema)) {
				triedToLoad.put(schema, schema);
				schema.loadTables(true, null, null, null);
				for (int i = 0; i < 10; ++i) {
					if (schema.isLoaded()) {
						return schema.getTables();
					}
					try {
						Thread.sleep(100);
					} catch (InterruptedException e) {
						// ignore
					}
				}
			}
			return Collections.emptyList();
    	}
		return schema.getTables();
	}

	@Override
	protected String getSchemaName(MDSchema schema) {
		return schema.getName();
	}

	@Override
	protected List<MDSchema> getSchemas(MetaDataSource metaDataSource) {
		return metaDataSource.getSchemas();
	}
	
	@Override
	protected List<Association> getAssociations(MDTable mdSource, MDTable mdDestination) {
		List<Association> result = new ArrayList<Association>();

		Table source = null;
		Table destination = null;
		
		if (mdSource != null) {
			source = metaDataSource.toTable(mdSource);
			if (source == null) {
				return result;
			}
		}
		if (mdDestination != null) {
			destination = metaDataSource.toTable(mdDestination);
			if (destination == null) {
				return result;
			}
		}
		
		if (source != null) {
			for (Association association: source.associations) {
				if (association.destination.equals(destination) || destination == null) {
					result.add(association);
				}
			}
		}
		return result;
	}

	@Override
	protected boolean isInitialized() {
		return metaDataSource.isInitialized() && (metaDataSource.getDefaultSchema() == null || metaDataSource.getDefaultSchema().isLoaded());
	}

	@Override
	protected MDTable createDummyTable(MDSchema schema, String name) {
		return new MDTable(name, schema, false, false);
	}

}