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

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JComponent;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.DatabaseObjectRenderingDescription;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.modelbuilder.MetaDataCache.CachedResultSet;

/**
 * Generic database object description based on DatabaseObjectRenderingDescription.
 * 
 * @author Ralf Wisser
 */
public class MDDescriptionBasedGeneric extends MDGeneric {

	private final DatabaseObjectRenderingDescription databaseObjectRenderingDescription;
	private final MDSchema schema;
	private final DataModel dataModel;

	/**
	 * Constructor.
	 * 
	 * @param name the object name
	 */
	public MDDescriptionBasedGeneric(String name, MetaDataSource metaDataSource, MDSchema schema, DataModel dataModel, DatabaseObjectRenderingDescription databaseObjectRenderingDescription) {
		super(name, metaDataSource);
		this.databaseObjectRenderingDescription = databaseObjectRenderingDescription;
		this.schema = schema;
		this.dataModel = dataModel;
	}

	/**
	 * Gets a list of descriptions of the details.
	 * 
	 * @return list of descriptions of the details
	 */
	public List<MDDescriptionBasedGeneric> getDetails() {
		try {
			final CachedResultSet theList = retrieveList(getMetaDataSource().getSession());
			ArrayList<MDDescriptionBasedGeneric> result = new ArrayList<MDDescriptionBasedGeneric>();
			for (final Object[] row: theList.getRowList()) {
				DatabaseObjectRenderingDescription detailDesc = new DatabaseObjectRenderingDescription() {
					public CachedResultSet retrieveList(Session session, String schema) throws SQLException {
						return new CachedResultSet(Collections.singletonList(row), theList.getMetaData());
					}
				};
				detailDesc.setListQuery(databaseObjectRenderingDescription.getListQuery());
				detailDesc.setIconURL(databaseObjectRenderingDescription.getDetailsIconURL());
				result.add(new MDDescriptionBasedGeneric(String.valueOf(row[0]), getMetaDataSource(), schema, dataModel, detailDesc));
			}
			return result;
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * creates the render of the database object.
	 * 
	 * @return render of the database object
	 */
	public JComponent createRender(Session session, ExecutionContext executionContext) throws Exception {
		return new ResultSetRenderer(retrieveList(session), getName(), dataModel, session, executionContext);
	}

	private CachedResultSet list;
	
	private CachedResultSet retrieveList(Session session) throws SQLException {
		if (list == null) {
			list = databaseObjectRenderingDescription.retrieveList(session, schema.getName());
		}
		return list;
	}

	public ImageIcon getIcon() {
		return databaseObjectRenderingDescription.getIcon();
	}

}
