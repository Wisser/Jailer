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
package net.sf.jailer.ui.databrowser.metadata;

import java.awt.GridBagConstraints;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;

import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;

import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.fife.ui.rsyntaxtextarea.SyntaxConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.configuration.DatabaseObjectRenderingDescription;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.modelbuilder.MemorizedResultSet;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.syntaxtextarea.RSyntaxTextAreaWithTheme;
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
	private static final Logger logger = LoggerFactory.getLogger(MetaDataDetailsPanel.class);

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
			MemorizedResultSet theList = retrieveList(getMetaDataSource().getSession());
			for (final Object[] row: theList.getRowList()) {
				MemorizedResultSet detailRS = new MemorizedResultSet(Collections.singletonList(row), theList.getMetaData());
				DatabaseObjectRenderingDescription detailDesc = itemDescription(detailRS);
				if (detailDesc != null) {
					MDDescriptionBasedGeneric mdDetails = createDetailDescription(row, detailDesc);
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

	protected MDDescriptionBasedGeneric createDetailDescription(final Object[] row, DatabaseObjectRenderingDescription detailDesc) {
		return new MDDescriptionBasedGeneric(String.valueOf(row[0]), getMetaDataSource(), schema, dataModel, detailDesc);
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
	protected DatabaseObjectRenderingDescription itemDescription(MemorizedResultSet itemContent) {
		return databaseObjectRenderingDescription.getItemDescription();
	}

	/**
	 * creates the render of the database object.
	 * 
	 * @return render of the database object
	 */
	@Override
	public JComponent createRender(final Session session, final ExecutionContext executionContext) throws Exception {
		final AtomicReference<MemorizedResultSet> resultSet = new AtomicReference<MemorizedResultSet>();
		final AtomicReference<MemorizedResultSet> text = new AtomicReference<MemorizedResultSet>();
		int textIndexNF = 1;
		if (databaseObjectRenderingDescription.getTextQuery() != null) {
			if (DBMS.MySQL.equals(session.dbms) && databaseObjectRenderingDescription.getTextQuery().matches("\\s*SHOW\\s+CREATE\\b.*")) {
				textIndexNF = 2;
			}
		}
		final int textIndex = textIndexNF;
		final JPanel panel = new JPanel();
		panel.setLayout(new BoxLayout(panel, javax.swing.BoxLayout.LINE_AXIS));
		panel.add(new JLabel("  loading..."));
		synchronized (this) {
			MDSchema.loadMetaData(new Runnable() {
				@Override
				public void run() {
					try {
						resultSet.set(distinct(retrieveList(session)));
						if (databaseObjectRenderingDescription.getTextQuery() != null) {
							text.set(retrieveList(session, databaseObjectRenderingDescription.getTextQuery(), Quoting.staticUnquote(schema.getName()), Quoting.staticUnquote(detailName)));
						}
					} catch (SQLException e) {
						final String message = e.getMessage();
						UIUtil.invokeLater(new Runnable() {
							@Override
							public void run() {
								panel.removeAll();
								panel.add(new JLabel("Error: " + message));
								panel.repaint();
								panel.revalidate();
							}
						});
						return;
					}
					UIUtil.invokeLater(new Runnable() {
						@Override
						public void run() {
							ResultSetRenderer details;
							try {
								details = new ResultSetRenderer(resultSet.get(), getName(), dataModel, session, executionContext);
							} catch (SQLException e) {
								return;
							}
							if (text.get() != null) {
								LinkedHashMap<String, StringBuilder> rows = new LinkedHashMap<String, StringBuilder>();
								String nl = System.getProperty("line.separator", "\n");
								List<Object[]> rowList = text.get().getRowList();
								for (Object[] row: rowList) {
									StringBuilder sb = rows.get(row[0]);
									if (sb == null) {
										sb = new StringBuilder();
										rows.put(row[0] == null? null : row[0].toString(), sb);
									}
									String line = row[textIndex] == null? null : row[textIndex].toString();
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
										RSyntaxTextArea textPane = new RSyntaxTextAreaWithTheme(e.getValue().toString());
										textPane.setBracketMatchingEnabled(false);
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
									panel.removeAll();
									panel.add(tabbedPane);
									panel.repaint();
									return;
								}
							}
							panel.removeAll();
							panel.add(details);
							panel.repaint();
						}
					});
				}
			}, isCheap()? 0 : 1);
		}
		return panel;
	}

	protected MemorizedResultSet distinct(MemorizedResultSet list) throws SQLException {
		List<Object[]> rowList = new ArrayList<Object[]>();
		Set<Object> seen = new HashSet<Object>();
		for (Object[] row: list.getRowList()) {
			if (row[0] == null || !seen.contains(row[0])) {
				rowList.add(row);
				seen.add(row[getDetailIDIndex()]);
			}
		}
		return new MemorizedResultSet(rowList, list.getMetaData());
	}

	protected MemorizedResultSet list;
	
	protected synchronized MemorizedResultSet retrieveList(Session session) throws SQLException {
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
	protected MemorizedResultSet retrieveList(Session session, String query, String schema, String parentName) throws SQLException {
		Statement cStmt = null;
		Connection connection = null;
		try {
            connection = session.getConnection();
            cStmt = connection.createStatement();
            if (schema != null) {
            	schema = Quoting.staticUnquote(schema);
            }
            if (parentName != null) {
            	parentName = Quoting.staticUnquote(parentName);
            }
            ResultSet rs = cStmt.executeQuery(String.format(Locale.ENGLISH, query, schema, parentName));
            MemorizedResultSet result = new MemorizedResultSet(rs, null, session, schema);
            rs.close();
            return result;
		} catch (SQLException e) {
			if (connection != null) {
        		session.markConnectionAsPotentiallyInvalid(connection);
        	}
			throw e;
		} catch (Exception e) {
			if (connection != null) {
        		session.markConnectionAsPotentiallyInvalid(connection);
        	}
        	logger.info("error", e);
            return new MemorizedResultSet(new ArrayList<Object[]>(), 1, new String[] { "?" }, new int[] { Types.VARCHAR });
        } finally {
            if (cStmt != null) {
                try {
                    cStmt.close();
                } catch (SQLException e) {
                }
            }
        }
	}

	private static Map<String, ImageIcon> icons = Collections.synchronizedMap(new HashMap<String, ImageIcon>());
	
	public ImageIcon getIcon() {
		if (databaseObjectRenderingDescription.getIconURL() != null) {
			ImageIcon icon = icons.get(databaseObjectRenderingDescription.getIconURL());
			if (icon == null) {
				try {
		            icon = UIUtil.readImage(databaseObjectRenderingDescription.getIconURL().replaceFirst(".*(/[^/]*)$", "$1"));
		        } catch (Exception e) {
		        }
			}
			icons.put(databaseObjectRenderingDescription.getIconURL(), icon);
			return icon;
		}
		return null;
	}

	public boolean isCheap() {
		return databaseObjectRenderingDescription.isCheap();
	}

	public boolean hasDetails() {
		return true;
	}

}
