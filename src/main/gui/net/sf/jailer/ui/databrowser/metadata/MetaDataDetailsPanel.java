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

import java.awt.BorderLayout;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.PriorityBlockingQueue;

import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.DbConnectionDialog;
import net.sf.jailer.ui.QueryBuilderDialog;
import net.sf.jailer.ui.QueryBuilderDialog.Relationship;
import net.sf.jailer.ui.databrowser.BrowserContentPane;
import net.sf.jailer.ui.databrowser.BrowserContentPane.LoadJob;
import net.sf.jailer.ui.databrowser.Desktop.RowBrowser;
import net.sf.jailer.ui.databrowser.QueryBuilderPathSelector;
import net.sf.jailer.ui.databrowser.Reference;
import net.sf.jailer.ui.databrowser.Row;
import net.sf.jailer.util.Pair;

/**
 * Meta Data Details View.
 *
 * @author Ralf Wisser
 */
public abstract class MetaDataDetailsPanel extends javax.swing.JPanel {

	private final Reference<DataModel> datamodel;
	private final Session session;
	private final ExecutionContext executionContext;
	private final JFrame owner;
	private final BlockingQueue<Runnable> queue = new LinkedBlockingQueue<Runnable>();
	private final Map<MetaDataDetails, JPanel> detailsPanels = new HashMap<MetaDataDetails, JPanel>();
	private final Map<Pair<MetaDataDetails, MDTable>, JComponent> detailsViews = new HashMap<Pair<MetaDataDetails, MDTable>, JComponent>();
	private final Map<Table, JComponent> tableDetailsViews = new HashMap<Table, JComponent>();
	
    /**
     * Creates new form MetaDataDetailsPanell 
     */
    public MetaDataDetailsPanel(Reference<DataModel> datamodel, Session session, JFrame owner, ExecutionContext executionContext) {
    	this.datamodel = datamodel;
    	this.session = session;
    	this.owner = owner;
    	this.executionContext = executionContext;
        initComponents();
        
        for (MetaDataDetails mdd: MetaDataDetails.values()) {
        	JPanel panel = new JPanel(new BorderLayout());
        	detailsPanels.put(mdd, panel);
        	tabbedPane.addTab(mdd.name, panel);
        }
        
        Thread thread = new Thread(new Runnable() {
			@Override
			public void run() {
				for (;;) {
					try {
						queue.take().run();
					} catch (Throwable t) {
						t.printStackTrace();
					}
				}
			}
		});
        thread.setDaemon(true);
        thread.start();
    }

    public void showMetaDataDetails(final MDTable mdTable, Table table, DataModel dataModel) {
    	tableDetailsPanel.removeAll();
    	if (table != null) {
    		JComponent view = tableDetailsViews.get(table);
    		if (view == null) {
    			view = new TableDetailsView(table, mdTable, this, dataModel);
    			tableDetailsViews.put(table, view);
    		}
    		tableDetailsPanel.add(view);
    	} else {
    		tableDetailsPanel.add(new JLabel("  Table \"" + mdTable.getName() + "\" is not part of the data model."));
    	}
		tabbedPane.repaint();
    	queue.clear();
    	for (final MetaDataDetails mdd: MetaDataDetails.values()) {
	    	final JPanel panel = detailsPanels.get(mdd);
	    	panel.removeAll();
	    	final Pair<MetaDataDetails, MDTable> cacheKey = new Pair<MetaDataDetails, MDTable>(mdd, mdTable);
	    	if (detailsViews.containsKey(cacheKey)) {
	    		panel.add(detailsViews.get(cacheKey));
		    	tabbedPane.repaint();
	    		continue;
	    	}
	    	panel.add(new JLabel(" loading..."));
	    	tabbedPane.repaint();
	    	try {
				queue.put(new Runnable() {
					@Override
					public void run() {
				    	try {
					    	final int tableNameColumnIndex = 3;
					    	final Set<String> pkNames = new HashSet<String>(mdTable.getPrimaryKeyColumns());
					    	final BrowserContentPane rb = new BrowserContentPane(datamodel.get(), null, "", session, null, null,
									null, null, new HashSet<Pair<BrowserContentPane, Row>>(), new HashSet<Pair<BrowserContentPane, String>>(), 0, false, false, executionContext) {
					    		{
					    			noSingleRowDetailsView = true;
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
								protected void navigateTo(Association association, int rowIndex, Row row) {
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
								protected QueryBuilderPathSelector getQueryBuilderPathSelector() {
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
								protected void findClosure(Row row, Set<Pair<BrowserContentPane, Row>> closure, boolean forward) {
									Pair<BrowserContentPane, Row> thisRow = new Pair<BrowserContentPane, Row>(this, row);
									if (!closure.contains(thisRow)) {
										closure.add(thisRow);
									}
								}
								@Override
								protected void findClosure(Row row) {
									Set<Pair<BrowserContentPane, Row>> rows = new HashSet<Pair<BrowserContentPane, Row>>();
									findClosure(row, rows, false);
									currentClosure.addAll(rows);
									rows = new HashSet<Pair<BrowserContentPane, Row>>();
									findClosure(row, rows, true);
									currentClosure.addAll(rows);
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
								protected void adjustClosure(BrowserContentPane tabu) {
								}
								@Override
								protected void addRowToRowLink(Row pRow, Row exRow) {
								}
								@Override
								protected boolean renderRowAsPK(Row theRow) {
									if (tableNameColumnIndex >= 0 && tableNameColumnIndex < theRow.values.length) {
										return pkNames.contains(theRow.values[tableNameColumnIndex]);
									}
									return false;
								}
							};
					    	
				    		LoadJob loadJob = rb.newLoadJob(mdd.readMetaDataDetails(session, mdTable));
				    		loadJob.run();
				    		SwingUtilities.invokeLater(new Runnable() {
								@Override
								public void run() {
									panel.removeAll();
									JComponent rTab = rb.getRowsTable();
						        	panel.add(rTab);
						        	tabbedPane.repaint();
							    	detailsViews.put(cacheKey, rTab);
								}
							});
						} catch (SQLException e) {
							e.printStackTrace();
						}
					}
				});
			} catch (InterruptedException e) {
				// ignore
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

        setLayout(new javax.swing.BoxLayout(this, javax.swing.BoxLayout.LINE_AXIS));

        jPanel1.setLayout(new javax.swing.BoxLayout(jPanel1, javax.swing.BoxLayout.LINE_AXIS));

        tableDetailsPanel.setLayout(new java.awt.BorderLayout());
        tabbedPane.addTab("Table", tableDetailsPanel);

        jPanel1.add(tabbedPane);

        add(jPanel1);
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JPanel jPanel1;
    private javax.swing.JTabbedPane tabbedPane;
    private javax.swing.JPanel tableDetailsPanel;
    // End of variables declaration//GEN-END:variables

    protected abstract void analyseSchema(String schemaName);
}
