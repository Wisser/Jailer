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

import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.PriorityBlockingQueue;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextArea;

import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.fife.ui.rsyntaxtextarea.SyntaxConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.PrimaryKey;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.modelbuilder.MemorizedResultSet;
import net.sf.jailer.modelbuilder.ModelBuilder;
import net.sf.jailer.ui.DbConnectionDialog;
import net.sf.jailer.ui.QueryBuilderDialog;
import net.sf.jailer.ui.QueryBuilderDialog.Relationship;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.BrowserContentPane;
import net.sf.jailer.ui.databrowser.BrowserContentPane.LoadJob;
import net.sf.jailer.ui.databrowser.Desktop.FindClosureContext;
import net.sf.jailer.ui.databrowser.Desktop.RowBrowser;
import net.sf.jailer.ui.databrowser.Desktop.RunnableWithPriority;
import net.sf.jailer.ui.databrowser.Reference;
import net.sf.jailer.ui.databrowser.Row;
import net.sf.jailer.ui.databrowser.sqlconsole.SQLConsole;
import net.sf.jailer.ui.syntaxtextarea.RSyntaxTextAreaWithTheme;
import net.sf.jailer.util.Pair;

/**
 * Meta Data Details View.
 *
 * @author Ralf Wisser
 */
public abstract class MetaDataDetailsPanel extends javax.swing.JPanel {

	/**
	 * The logger.
	 */
	private static final Logger logger = LoggerFactory.getLogger(MetaDataDetailsPanel.class);

	private final Reference<DataModel> datamodel;
	private final Session session;
	private final ExecutionContext executionContext;
	private final JFrame owner;
	private static final List<BlockingQueue<Runnable>> queues = new ArrayList<BlockingQueue<Runnable>>();
	private final Map<MetaDataDetails, JPanel> detailsPanels = new HashMap<MetaDataDetails, JPanel>();
	private final JPanel ddlPanel;
	private final JPanel constraintsPanel;
	private final Map<Pair<MetaDataDetails, MDTable>, JComponent> detailsViews = new HashMap<Pair<MetaDataDetails, MDTable>, JComponent>();
	private final Map<Table, TableDetailsView> tableDetailsViews = new HashMap<Table, TableDetailsView>();
	private final Map<Table, TableDetailsView> tableDetailsViewsAll = new HashMap<Table, TableDetailsView>();

    /**
     * Creates new form MetaDataDetailsPanell 
     */
    public MetaDataDetailsPanel(Reference<DataModel> datamodel, Session session, JFrame owner, ExecutionContext executionContext) {
    	this.datamodel = datamodel;
    	this.session = session;
    	this.owner = owner;
    	this.executionContext = executionContext;
        initComponents(); UIUtil.initComponents(this);
        
        if (session != null && session.dbms.getConstraintsQuery() != null) {
        	constraintsPanel = new JPanel(new BorderLayout());
        } else {
        	constraintsPanel = null;
        }
    	ddlPanel = new JPanel(new BorderLayout());
        for (MetaDataDetails mdd: MetaDataDetails.values()) {
        	JPanel panel = new JPanel(new BorderLayout());
        	detailsPanels.put(mdd, panel);
        	tabbedPane.addTab(mdd.name, panel);
        	if (tabbedPane.getTabCount() == 3) {
        		tabbedPane.addTab("DDL", ddlPanel);
        	}
        	if (tabbedPane.getTabCount() == 4) {
        		if (constraintsPanel != null) {
        			tabbedPane.addTab("Constraints", constraintsPanel);
        		}
        	}
        }
        ((CardLayout) getLayout()).show(this, "other");
    }
    
    static {
        for (int i = 0; i < 3; ++i) {
        	final LinkedBlockingQueue<Runnable> queue = new LinkedBlockingQueue<Runnable>();
	        queues.add(queue);
        	Thread thread = new Thread(new Runnable() {
				@Override
				public void run() {
					for (;;) {
						try {
							queue.take().run();
						} catch (Throwable t) {
							// ignore, was: logger.info("error", t);
						}
					}
				}
			}, "Metadata-LoadDetails-" + (i + 1));
	        thread.setDaemon(true);
	        thread.start();
        }
    }

	public void clear() {
    	setVisible(false);
	}

	public void reset() {
		detailsViews.clear();
		tableDetailsViews.clear();
		tableDetailsViewsAll.clear();
	}

	public void showMetaDataDetails(MDGeneric mdOther, ExecutionContext executionContext) {
		setVisible(true);
        ((CardLayout) getLayout()).show(this, "other");
    	otherPanel.removeAll();
    	JComponent render = mdOther.getRender(session, executionContext);
		otherPanel.add(render);
		render.revalidate();
    	otherPanel.repaint();
	}

    public void showMetaDataDetails(final MDTable mdTable, Table table, Row row, boolean onlyTable, DataModel dataModel) {
    	TableDetailsView v = table == null? null : tableDetailsViewsAll.get(table);
    	Point vpos = v == null? null : v.getViewPosition();
    	setVisible(true);
        ((CardLayout) getLayout()).show(this, "table");
    	tableDetailsPanel.removeAll();
    	if (table != null) {
    		TableDetailsView view = tableDetailsViews.get(table);
    		if (row != null || view == null) {
    			TableDetailsView tdv = new TableDetailsView(table, mdTable, this, row, dataModel, (TableDetailsView) view);
    			view = tdv;
    			if (row == null && tdv.isCacheable()) {
    				tableDetailsViews.put(table, view);
    			}
    		}
    		if (vpos != null) {
    			view.setViewPosition(vpos);
    		}
    		tableDetailsViewsAll.put(table, view);
    		
    		tableDetailsPanel.add(view);
    	} else if (mdTable != null) {
    		if (!ModelBuilder.isJailerTable(mdTable.getUnquotedName())) {
    			JButton analyseButton = new JButton("Analyse schema \"" + mdTable.getSchema().getUnquotedName() + "\"");
    			JPanel panel = new JPanel(new GridBagLayout());
	    		if (!mdTable.isSystem()) {
    				analyseButton.setIcon(MetaDataPanel.getScaledIcon(this, MetaDataPanel.warnIcon, false));
    				analyseButton.addActionListener(new ActionListener() {
    					@Override
    					public void actionPerformed(ActionEvent e) {
    						analyseSchema(mdTable.getSchema().getName(), mdTable.isView(), mdTable.isSynonym());
    					}
    				});
		    		GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
			        gridBagConstraints.gridx = 1;
			        gridBagConstraints.gridy = 1;
			        gridBagConstraints.gridwidth = 1;
			        gridBagConstraints.fill = java.awt.GridBagConstraints.NONE;
			        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
		    		panel.add(new JLabel("  Table \"" + mdTable.getUnquotedName() + "\""), gridBagConstraints);
		    		gridBagConstraints = new java.awt.GridBagConstraints();
			        gridBagConstraints.gridx = 1;
			        gridBagConstraints.gridy = 2;
			        gridBagConstraints.gridwidth = 1;
			        gridBagConstraints.fill = java.awt.GridBagConstraints.NONE;
			        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
		    		panel.add(new JLabel("  is not part of the data model."), gridBagConstraints);
    			}
	    		GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
		        gridBagConstraints.gridx = 1;
		        gridBagConstraints.gridy = 4;
		        gridBagConstraints.gridwidth = 1;
		        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
		        gridBagConstraints.weightx = 1;
		        gridBagConstraints.weighty = 1;
		        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
	    		if (mdTable.isLoaded()) {
	    			List<Column> pkColumns = new ArrayList<Column>();
					List<Column> columns = new ArrayList<Column>();
		    		for (Column c: mdTable.getColumnTypes()) {
		    			columns.add(c);
		    			try {
							for (String pk: mdTable.getPrimaryKeyColumns()) {
								if (pk.equals(c.name)) {
									pkColumns.add(c);
									break;
								}
							}
						} catch (SQLException e1) {
							// ignore
						}
		    		}
		    		PrimaryKey pks = new PrimaryKey(pkColumns, false);
					Table tTable = new Table(mdTable.getName(), pks, false, false);
					tTable.setIsArtifical(true);
		    		tTable.setColumns(columns);
					panel.add(new TableDetailsView(tTable, mdTable, this, null, dataModel, null), gridBagConstraints);
	    		}
	    		gridBagConstraints = new java.awt.GridBagConstraints();
		        gridBagConstraints.gridx = 1;
		        gridBagConstraints.gridy = 3;
		        gridBagConstraints.gridwidth = 1;
		        gridBagConstraints.fill = java.awt.GridBagConstraints.NONE;
		        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
		        if (!mdTable.isSystem()) {
	    			panel.add(analyseButton, gridBagConstraints);
		        }
	    		tableDetailsPanel.add(panel);
	   		} else {
				JPanel panel = new JPanel(new GridBagLayout());
	    		GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
		        gridBagConstraints.gridx = 1;
		        gridBagConstraints.gridy = 1;
		        gridBagConstraints.gridwidth = 1;
		        gridBagConstraints.fill = java.awt.GridBagConstraints.NONE;
		        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
	    		panel.add(new JLabel("Jailer subsetting tool working table"), gridBagConstraints);
	    		tableDetailsPanel.add(panel);
    		}
    	}
		tabbedPane.repaint();
		if (onlyTable) {
			return;
		}
    	for (BlockingQueue<Runnable> queue: queues) {
    		queue.clear();
    	}
    	for (final MetaDataDetails mdd: MetaDataDetails.values()) {
	    	final JPanel panel = detailsPanels.get(mdd);
	    	panel.removeAll();
	    	final Pair<MetaDataDetails, MDTable> cacheKey = new Pair<MetaDataDetails, MDTable>(mdd, mdTable);
	    	if (detailsViews.containsKey(cacheKey)) {
	    		JComponent comp = detailsViews.get(cacheKey);
				panel.add(comp);
				tabbedPane.repaint();
				continue;
	    	}
	    	panel.add(new JLabel(" loading..."));
	    	tabbedPane.repaint();
	    	try {
		    	final int tableNameColumnIndex = 3;
		    	final Set<String> pkNames = Collections.synchronizedSet(new HashSet<String>());
		    	final BrowserContentPane rb = new BrowserContentPane(datamodel.get(), null, "", session, null,
						null, null, new BrowserContentPane.RowsClosure(), false, false, executionContext) {
		    		{
		    			noSingleRowDetailsView = true;
		    			rowsTableScrollPane.setWheelScrollingEnabled(true);
		    			sortColumnsCheckBox.setVisible(false);
		    			sortColumnsPanel.setVisible(false);
		                statusPanel.setVisible(false);
		    		}
		    		protected double getAnimationFactor() {
		    			return 0f;
		    		}
		    		@Override
		    		protected int getReloadLimit() {
		    			return Integer.MAX_VALUE;
		    		}

		    		@Override
					protected void setReloadLimit(int limit) {
					}

		    		@Override
					protected void unhide() {
					}
					@Override
					protected void showInNewWindow() {
					}
					@Override
					protected void reloadDataModel() throws Exception {
					}
					@Override
					protected void openSchemaMappingDialog() {
					}
					@Override
					protected void openSchemaAnalyzer() {
					}
					@Override
					protected void onRedraw() {
						tabbedPane.repaint();
					}
					@Override
					protected void onHide() {
					}
					@Override
					protected void onContentChange(List<Row> rows, boolean reloadChildren) {
					}
					@Override
					protected RowBrowser navigateTo(Association association, List<Row> pRows) {
						return null;
					}
					@Override
					protected List<RowBrowser> getTableBrowser() {
						return null;
					}
					@Override
					protected PriorityBlockingQueue<RunnableWithPriority> getRunnableQueue() {
						return null;
					}
					@Override
					protected QueryBuilderDialog getQueryBuilderDialog() {
						return null;
					}
					@Override
					protected RowBrowser getParentBrowser() {
						return null;
					}
					@Override
					protected JFrame getOwner() {
						return owner;
					}
					@Override
					protected double getLayoutFactor() {
						return 0;
					}
					@Override
					protected DbConnectionDialog getDbConnectionDialog() {
						return null;
					}
					@Override
					protected List<RowBrowser> getChildBrowsers() {
						return new ArrayList<RowBrowser>();
					}
					@Override
					protected void findClosure(Row row, Set<Pair<BrowserContentPane, Row>> closure, boolean forward, FindClosureContext findClosureContext) {
					}
					@Override
					protected void findClosure(Row row) {
					}
					@Override
					protected Relationship createQBRelations(boolean withParents) {
						return null;
					}
					@Override
					protected List<Relationship> createQBChildrenRelations(RowBrowser tabu, boolean all) {
						return null;
					}
					@Override
					protected void collectPositions(Map<String, Map<String, double[]>> positions) {
					}
					@Override
					protected void close() {
					}
					@Override
					protected void beforeReload() {
					}
					@Override
					protected void appendLayout() {
					}
					@Override
					protected void adjustClosure(BrowserContentPane tabu, BrowserContentPane thisOne) {
					}
					@Override
					protected void addRowToRowLink(Row pRow, Row exRow) {
					}
					@Override
					protected boolean renderRowAsPK(Row theRow) {
						if (tableNameColumnIndex >= 0 && tableNameColumnIndex < theRow.values.length) { // lgtm [java/constant-comparison]
							return pkNames.contains(String.valueOf(theRow.values[tableNameColumnIndex]));
						}
						return false;
					}
					@Override
					protected MetaDataSource getMetaDataSource() {
						return null;
					}
					@Override
					protected SQLConsole getSqlConsole(boolean switchToConsole) {
						return null;
					}
					@Override
					protected void deselectChildrenIfNeededWithoutReload() {
					}
					@Override
					protected void forceRepaint() {
					}
				};
		    	
				final MemorizedResultSet[] metaDataDetails = new MemorizedResultSet[1];
				
				queues.get(mdd.queueIndex).put(new Runnable() {
					@Override
					public void run() {
						try {
							pkNames.addAll(mdTable.getPrimaryKeyColumns(false));
						} catch (SQLException e1) {
							if (!session.isDown()) {
								logger.info("error", e1);
							}
						}
				    	try {
				    		synchronized (session.getMetaData()) {
					    		ResultSet rs = mdd.readMetaDataDetails(session, mdTable);
					    		metaDataDetails[0] = new MemorizedResultSet(rs, null, session, null);
					    		rs.close();
				    		}
						} catch (SQLException e) {
							// ignore
						}
				    	if (metaDataDetails[0] != null) {
				    		UIUtil.invokeLater(new Runnable() {
								@Override
								public void run() {
									LoadJob loadJob = rb.newLoadJob(metaDataDetails[0], null);
						    		loadJob.run();
						        	JComponent rTabContainer = rb.getRowsTableContainer();
							    	detailsViews.put(cacheKey, rTabContainer);
									final JTable rTab = rb.getRowsTable();
									UIUtil.invokeLater(new Runnable() {
										@Override
										public void run() {
											mdd.adjustRowsTable(rTab);
											panel.removeAll();
								        	JComponent rTabContainer = rb.getRowsTableContainer();
											panel.add(rTabContainer);
											rb.resetRowsTableContainer();
											tabbedPane.repaint();
										}
									});
								}
							});
				    	}
					}
				});
			} catch (InterruptedException e) {
				// ignore
			}
    	}

    	// DDL
    	final JPanel panel = ddlPanel;
		final Runnable doRun = new Runnable() {
			@Override
			public void run() {
				final String ddl = mdTable.getDDL();
		    	panel.removeAll();
		    	if (ddl == null) {
		    		panel.add(new JLabel(" DDL not available"));
		    	} else {
		    		JScrollPane tab = new JScrollPane();
		    		RSyntaxTextArea area = new RSyntaxTextAreaWithTheme();
					area.setBracketMatchingEnabled(false);

		    		area.setEditable(false);
		    		tab.setViewportView(area);
		    		area.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_SQL);
		    		area.setText(ddl);
		    		area.setCaretPosition(0);
		    		panel.add(tab);
		    	}
		    	tabbedPane.repaint();
			}
		};
    	panel.removeAll();
    	if (mdTable.isDDLLoaded()) { // lgtm [java/dereferenced-value-may-be-null]
    		doRun.run();
    	} else {
	    	panel.add(new JLabel(" loading..."));
	    	tabbedPane.repaint();
	    	try {
				queues.get(2).put(new Runnable() {
					@Override
					public void run() {
						mdTable.getDDL();
						UIUtil.invokeLater(doRun);
					}
				});
			} catch (InterruptedException e) {
				// ignore
			}
    	}
    	
    	if (constraintsPanel != null) {
        	// Constraints
        	final Runnable doRunGetConstraints = new Runnable() {
    			@Override
    			public void run() {
    				final MemorizedResultSet constraints;
    				try {
    					constraints = mdTable.getSchema().getConstraints(mdTable, dataModel);
    				} catch (SQLException e) {
    					constraintsPanel.removeAll();
    					JTextArea ta = new JTextArea(" Error: " + e.getMessage());
                        constraintsPanel.add(new JScrollPane(ta));
        		    	tabbedPane.repaint();
        		    	return;
    				}
    				constraintsPanel.removeAll();
    		    	if (constraints == null) {
    		    		constraintsPanel.add(new JLabel(" Constraints not available"));
    		    	} else {
    		    		ResultSetRenderer area;
						try {
							area = new ResultSetRenderer(constraints, "Constraints", datamodel.get(), session, executionContext);
						} catch (SQLException e) {
							return;
						}
    		    		constraintsPanel.add(area);
    		    	}
    		    	tabbedPane.repaint();
    			}
    		};
    		constraintsPanel.removeAll();
        	if (mdTable.getSchema().isConstraintsLoaded()) {
        		doRunGetConstraints.run();
        	} else {
        		constraintsPanel.add(new JLabel(" loading..."));
    	    	tabbedPane.repaint();
    	    	MDSchema.loadMetaData(new Runnable() {
    	    		@Override
    	    		public void run() {
    	    			try {
							mdTable.getSchema().getConstraints(null, dataModel);
	    	    			if (mdTable.getSchema().isConstraintsLoaded()) {
	    	    	        	UIUtil.invokeLater(doRunGetConstraints);
	    	    			}
						} catch (final Throwable t) {
							UIUtil.invokeLater(new Runnable() {
								@Override
								public void run() {
									constraintsPanel.removeAll();
			    					JTextArea ta = new JTextArea(" Error: " + t.getMessage());
			    					constraintsPanel.add(new JScrollPane(ta));
			        		    	tabbedPane.repaint();
								}
							});
						}
    	    		}
    	    	}, 1);
    		}
    	}
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jPanel1 = new javax.swing.JPanel();
        tabbedPane = new javax.swing.JTabbedPane();
        tableDetailsPanel = new javax.swing.JPanel();
        otherPanel = new javax.swing.JPanel();

        setLayout(new java.awt.CardLayout());

        jPanel1.setLayout(new javax.swing.BoxLayout(jPanel1, javax.swing.BoxLayout.LINE_AXIS));

        tabbedPane.setTabLayoutPolicy(javax.swing.JTabbedPane.SCROLL_TAB_LAYOUT);

        tableDetailsPanel.setLayout(new java.awt.BorderLayout());
        tabbedPane.addTab("Details", tableDetailsPanel);

        jPanel1.add(tabbedPane);

        add(jPanel1, "table");

        otherPanel.setLayout(new javax.swing.BoxLayout(otherPanel, javax.swing.BoxLayout.LINE_AXIS));
        add(otherPanel, "other");
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel otherPanel;
    public javax.swing.JTabbedPane tabbedPane;
    private javax.swing.JPanel tableDetailsPanel;
    // End of variables declaration//GEN-END:variables

    protected abstract void analyseSchema(String schemaName, boolean withViews, boolean withSynonyms);
   
}
