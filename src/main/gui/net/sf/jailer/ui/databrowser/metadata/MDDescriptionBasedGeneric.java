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

import java.awt.GridBagConstraints;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;

import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;

import org.apache.log4j.Logger;
import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.fife.ui.rsyntaxtextarea.SyntaxConstants;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.configuration.DatabaseObjectRenderingDescription;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.modelbuilder.MetaDataCache;
import net.sf.jailer.modelbuilder.MetaDataCache.CachedResultSet;
import net.sf.jailer.util.Quoting;

/**
 * Generic database object description based on DatabaseObjectRenderingDescription.
 * 
 * @author Ralf Wisser
 */
public class MDDescriptionBasedGeneric extends MDGeneric {

	/**
	 * The logger.
	 */
	private static final Logger logger = Logger.getLogger(MetaDataDetailsPanel.class);

	protected final DatabaseObjectRenderingDescription databaseObjectRenderingDescription;
	protected final MDSchema schema;
	protected final DataModel dataModel;
	protected String detailName;

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
		this.detailName = name;
	}

	/**
	 * Gets a list of descriptions of the details.
	 * 
	 * @return list of descriptions of the details
	 */
	public List<MDDescriptionBasedGeneric> getDetails() {
		ArrayList<MDDescriptionBasedGeneric> result = new ArrayList<MDDescriptionBasedGeneric>();
		try {
			CachedResultSet theList = retrieveList(getMetaDataSource().getSession());
			for (final Object[] row: theList.getRowList()) {
				CachedResultSet detailRS = new CachedResultSet(Collections.singletonList(row), theList.getMetaData());
				DatabaseObjectRenderingDescription detailDesc = itemDescription(detailRS);
				if (detailDesc != null) {
					MDDescriptionBasedGeneric mdDetails = new MDDescriptionBasedGeneric(String.valueOf(row[0]), getMetaDataSource(), schema, dataModel, detailDesc);
					mdDetails.detailName = String.valueOf(row[getDetailIDIndex()]);
					if (detailDesc.getListQuery() == null) {
						mdDetails.list = detailRS;
					}
					result.add(mdDetails);
				}
			}
		} catch (Throwable t) {
			logger.info("error", t);
		}
		return result;
	}

	protected int getDetailIDIndex() {
		return 0;
	}

	/**
	 * Gets description of an item.
	 * @param itemContent content of the item
	 * 
	 * @return description of an item
	 */
	protected DatabaseObjectRenderingDescription itemDescription(CachedResultSet itemContent) {
		return databaseObjectRenderingDescription.getItemDescription();
	}

	/**
	 * creates the render of the database object.
	 * 
	 * @return render of the database object
	 */
	public JComponent createRender(Session session, ExecutionContext executionContext) throws Exception {
		ResultSetRenderer details = new ResultSetRenderer(distinct(retrieveList(session)), getName(), dataModel, session, executionContext);
		if (databaseObjectRenderingDescription.getTextQuery() != null) {
			int textIndex = 1;
			if (DBMS.MySQL.equals(session.dbms) && databaseObjectRenderingDescription.getTextQuery().matches("\\s*SHOW\\s+CREATE\\b.*")) {
				textIndex = 2;
			}
			CachedResultSet text = retrieveList(session, databaseObjectRenderingDescription.getTextQuery(), Quoting.staticUnquote(schema.getName()), Quoting.staticUnquote(detailName));
			LinkedHashMap<String, StringBuilder> rows = new LinkedHashMap<String, StringBuilder>();
			String nl = System.getProperty("line.separator", "\n");
			for (Object[] row: text.getRowList()) {
				StringBuilder sb = rows.get(row[0]);
				if (sb == null) {
					sb = new StringBuilder();
					rows.put((String) row[0], sb);
				}
				String line = (String) row[textIndex];
				if (line != null) {
					sb.append(line);
					if (!line.endsWith("\n")) {
						sb.append(nl);
					}
				}
			}
			if (!rows.isEmpty()) {
				JTabbedPane tabbedPane = new JTabbedPane();
				for (Entry<String, StringBuilder> e: rows.entrySet()) {
					RSyntaxTextArea textPane = new RSyntaxTextArea(e.getValue().toString());
					textPane.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_SQL);
					textPane.setEditable(false);
					textPane.setCaretPosition(0);
					
					JScrollPane jScrollPane = new JScrollPane();
					jScrollPane.setViewportView(textPane);
					GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
					gridBagConstraints.gridx = 1;
					gridBagConstraints.gridy = 20;
					gridBagConstraints.gridwidth = 2;
					gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
					gridBagConstraints.weightx = 1.0;
					gridBagConstraints.weighty = 1.0;
					gridBagConstraints.insets = new java.awt.Insets(4, 4, 0, 4);
					jScrollPane.setViewportView(textPane);
					
					tabbedPane.addTab(e.getKey(), jScrollPane);
				}
				tabbedPane.addTab("Details", details);
				return tabbedPane;
			}
		}
		return details;
	}

	private CachedResultSet distinct(CachedResultSet list) throws SQLException {
		List<Object[]> rowList = new ArrayList<Object[]>();
		Set<Object> seen = new HashSet<Object>();
		for (Object[] row: list.getRowList()) {
			if (row[0] == null || !seen.contains(row[0])) {
				rowList.add(row);
				seen.add(row[getDetailIDIndex()]);
			}
		}
		return new CachedResultSet(rowList, list.getMetaData());
	}

	private CachedResultSet list;
	
	protected CachedResultSet retrieveList(Session session) throws SQLException {
		if (list == null) {
			list = distinct(retrieveList(session, databaseObjectRenderingDescription.getListQuery(), schema.getName(), null));
		}
		return list;
	}

	/**
	 * Retrieves list of all objects.
	 * 
	 * @return list of all objects 
	 */
	protected CachedResultSet retrieveList(Session session, String query, String schema, String parentName) throws SQLException {
		Statement cStmt = null;
        try {
            Connection connection = session.getConnection();
            cStmt = connection.createStatement();
            if (schema != null) {
            	schema = Quoting.staticUnquote(schema);
            }
            if (parentName != null) {
            	parentName = Quoting.staticUnquote(parentName);
            }
            ResultSet rs = cStmt.executeQuery(String.format(query, schema, parentName));
            CachedResultSet result = new MetaDataCache.CachedResultSet(rs, null, session, schema);
            rs.close();
            return result;
        } catch (Exception e) {
        	logger.info("error", e);
            return null;
        } finally {
            if (cStmt != null) {
                try {
                    cStmt.close();
                } catch (SQLException e) {
                }
            }
        }
	}

	public ImageIcon getIcon() {
		return databaseObjectRenderingDescription.getIcon();
	}

}
