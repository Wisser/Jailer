/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package net.sf.jailer.ui.databrowser.sqlconsole;

import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.SwingUtilities;

import org.fife.ui.autocomplete.AutoCompletion;
import org.fife.ui.rtextarea.RTextScrollPane;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.modelbuilder.MetaDataCache.CachedResultSet;
import net.sf.jailer.ui.DbConnectionDialog;
import net.sf.jailer.ui.QueryBuilderDialog;
import net.sf.jailer.ui.QueryBuilderDialog.Relationship;
import net.sf.jailer.ui.databrowser.BrowserContentPane;
import net.sf.jailer.ui.databrowser.BrowserContentPane.LoadJob;
import net.sf.jailer.ui.databrowser.DataBrowser;
import net.sf.jailer.ui.databrowser.Desktop.RowBrowser;
import net.sf.jailer.ui.databrowser.QueryBuilderPathSelector;
import net.sf.jailer.ui.databrowser.Reference;
import net.sf.jailer.ui.databrowser.Row;
import net.sf.jailer.ui.databrowser.metadata.MetaDataSource;
import net.sf.jailer.ui.syntaxtextarea.RSyntaxTextAreaWithSQLSyntaxStyle;
import net.sf.jailer.util.CancellationException;
import net.sf.jailer.util.CancellationHandler;
import net.sf.jailer.util.Pair;
import util.SmallButton;

/**
 * SQL Console.
 *
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public abstract class SQLConsole extends javax.swing.JPanel {

	private static final int MAX_TAB_COUNT = 10;
	
	private Session session;
	MetaDataSource metaDataSource;
	private RSyntaxTextAreaWithSQLSyntaxStyle editorPane;
	private final MetaDataBasedSQLCompletionProvider provider;
	private final BlockingQueue<Runnable> queue = new LinkedBlockingQueue<Runnable>();
	private final Reference<DataModel> datamodel;
	private final ExecutionContext executionContext;
	
	/**
	 * Creates new form SQLConsole
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	public SQLConsole(Session session, MetaDataSource metaDataSource, Reference<DataModel> datamodel, ExecutionContext executionContext) throws SQLException {
		this.session = session;
		this.metaDataSource = metaDataSource;
		this.datamodel = datamodel;
		this.executionContext = executionContext;
		initComponents();

		this.editorPane = new RSyntaxTextAreaWithSQLSyntaxStyle() {
			@Override
			protected void actionPerformed() {
				executeSelectedStatements();
			}
		};
		provider = new MetaDataBasedSQLCompletionProvider(session, metaDataSource);
		AutoCompletion ac = new AutoCompletion(provider);
		ac.install(editorPane);
		RTextScrollPane jScrollPane = new RTextScrollPane();
		jScrollPane.setViewportView(editorPane);
		consoleContainerPanel.add(jScrollPane);
		jScrollPane.setLineNumbersEnabled(true);
		
		limitComboBox.setModel(new DefaultComboBoxModel(DataBrowser.ROW_LIMITS));
		limitComboBox.setSelectedItem(1000);
		
		resetStatus();

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

	private void resetStatus() {
		statusLabel.setVisible(false);
		statusScrollPane.setVisible(false);
		cancelButton.setEnabled(false);
	}

	/**
	 * Executes a block of SQL statements (each statement separated by a ';' at the end of the line).
	 * 
	 * @param sqlBlock the sql block
	 * @param location location of the block in the console
	 */
	protected void executeSQLBlock(final String sqlBlock, Pair<Integer, Integer> location) {
		queue.add(new Runnable() {
			@Override
			public void run() {
				Status status = new Status();
				Pattern pattern = Pattern.compile("(.*?)(;\\s*(\\n\\r?|$))", Pattern.DOTALL);
				Matcher matcher = pattern.matcher(sqlBlock);
				boolean result = matcher.find();
				StringBuffer sb = new StringBuffer();
				if (result) {
					do {
						String sql = matcher.group(1);
						if (sql.trim().length() > 0) {
							executeSQL(sql, status);
							if (status.failed) {
								break;
							}
						}
						matcher.appendReplacement(sb, "");
						result = matcher.find();
					} while (result);
				}
				if (!status.failed) {
					matcher.appendTail(sb);
					String sql = sb.toString();
					if (sql.trim().length() > 0) {
						executeSQL(sql, status);
					}
				}
			}
		});
	}

	/**
	 * Executes a single SQL statment.
	 * 
	 * @param sql the statement
	 * @param status the status to update
	 */
	private void executeSQL(final String sql, Status status) {
		Statement statement = null;
		ResultSet resultSet = null;
		final Status localStatus = new Status();
		try {
			status.running = true;
			status.numStatements++;
			localStatus.numStatements++;
			status.updateView();
			statement = session.getConnection().createStatement();
			CancellationHandler.begin(statement, SQLConsole.this);
			long startTime = System.currentTimeMillis();
			if (statement.execute(sql)) {
				resultSet = statement.getResultSet();
				Integer limit = (Integer) limitComboBox.getSelectedItem();
				final BrowserContentPane rb = new ResultContentPane(datamodel.get(), null, "", session, null, null,
						null, null, new HashSet<Pair<BrowserContentPane, Row>>(), new HashSet<Pair<BrowserContentPane, String>>(), limit, false, false, executionContext);
				final CachedResultSet metaDataDetails = new CachedResultSet(resultSet, limit, SQLConsole.this);
		    	resultSet.close();
				long now = System.currentTimeMillis();
		    	status.timeInMS += (now - startTime);
		    	localStatus.timeInMS += (now - startTime);
		    	status.numRowsRead += metaDataDetails.getSize();
		    	localStatus.numRowsRead += metaDataDetails.getSize();
		    	if (metaDataDetails.getSize() >= limit) {
		    		status.limitExceeded = true;
		    		localStatus.limitExceeded = true;
		    	}
				status.running = false;
				status.updateView();
	    		SwingUtilities.invokeLater(new Runnable() {
					@Override
					public void run() {
						LoadJob loadJob = rb.newLoadJob(metaDataDetails);
			    		loadJob.run();
			        	JComponent rTabContainer = rb.getRowsTableContainer();
			        	final TabContentPanel tabContentPanel = new TabContentPanel();
			        	tabContentPanel.contentPanel.add(rTabContainer);
			        	tabContentPanel.statusLabel.setText(localStatus.getText());
			        	if (localStatus.limitExceeded) {
			        		tabContentPanel.statusLabel.setForeground(Color.red);
			        	}
			        	rb.sortColumnsCheckBox.setVisible(true);
			        	tabContentPanel.controlsPanel1.add(rb.sortColumnsCheckBox);
			        	String stmt = sql.trim();
			        	if (stmt.length() > 200) {
			        		stmt = stmt.substring(0, 200) + "...";
			        	}
						tabContentPanel.statementLabel.setText(stmt);
			        	tabContentPanel.statementLabel.setToolTipText(stmt);
			        	tabContentPanel.reloadButton.addActionListener(new ActionListener() {
							@Override
							public void actionPerformed(ActionEvent e) {
								jTabbedPane1.remove(tabContentPanel);
								executeSQLBlock(sql, null);
							}
						});
			        	rTabContainer = tabContentPanel;
				    	String title = sql.trim();
				    	final int MAXLENGTH = 30;
				    	if (title.length() > MAXLENGTH) {
				    		title = title.replaceFirst("^(?is)(\\bselect\\b).*?(\\bfrom\\b)(.*)$", "$1 ... $2$3");
				    	}
				    	if (title.length() > MAXLENGTH) {
							title = title.substring(0, MAXLENGTH) + "...";
				    	}
						jTabbedPane1.add(rTabContainer);
						jTabbedPane1.setTabComponentAt(jTabbedPane1.indexOfComponent(rTabContainer), getTitlePanel(jTabbedPane1, rTabContainer, title));

						if (jTabbedPane1.getTabCount() > MAX_TAB_COUNT) {
							jTabbedPane1.remove(0);
						}
						jTabbedPane1.setSelectedIndex(jTabbedPane1.getTabCount() - 1);
				    	jTabbedPane1.repaint();
					}
				});
			} else {
		    	status.timeInMS += (System.currentTimeMillis() - startTime);
		    	int updateCount = statement.getUpdateCount();
				status.numRowsUpdated += updateCount;
				status.running = false;
		    	status.updateView();
		    	if (updateCount != 0) {
		    		setDataHasChanged(true);
		    	}
		    	if (isDDLStatement(sql)) {
		    		refreshMetaData();
		    	}
			}
			CancellationHandler.end(statement, SQLConsole.this);
			statement.close();
		} catch (Throwable error) {
			if (statement != null) {
				try {
					statement.close();
				} catch (SQLException e) {
				}
			}
			if (resultSet != null) {
				try {
					resultSet.close();
				} catch (SQLException e) {
				}
			}
			if (error instanceof CancellationException) {
				CancellationHandler.reset(SQLConsole.this);
				queue.clear();
			}
			status.failed = true;
			status.error = error;
			status.updateView();
		}
	}

	private boolean isDDLStatement(String sql) {
		return sql.trim().matches("^(?is)\\b(drop|create|alter|rename)\\b.*");
	}

	protected abstract void refreshMetaData();

	private boolean dataHasChanged = false;
	
	public synchronized void setDataHasChanged(boolean b) {
		dataHasChanged = b;		
	}

	public synchronized boolean getDataHasChanged() {
		return dataHasChanged;		
	}

	/**
	 * Resets the console.
	 * 
	 * @param session
	 *            new session
	 * @param metaDataSource
	 *            new meta data source
	 */
	public void reset(Session session, MetaDataSource metaDataSource) throws SQLException {
		this.session = session;
		this.metaDataSource = metaDataSource;
		provider.reset(session, metaDataSource);
	}

	private class Status {
		boolean failed;
		boolean running;
		boolean limitExceeded;
		int numRowsRead;
		int numRowsUpdated;
		int numStatements;
		long timeInMS;
		Throwable error;

		private void updateView() {
			SwingUtilities.invokeLater(new Runnable() {
				@Override
				public void run() {
					statusLabel.setVisible(false);
					cancelButton.setEnabled(false);
					statusScrollPane.setVisible(false);
					if (!failed) {
						cancelButton.setEnabled(running);
						statusLabel.setVisible(true);
						statusLabel.setForeground(running? new Color(0, 100, 0) : Color.BLACK);
						statusLabel.setText(getText());
					} else {
						statusScrollPane.setVisible(true);
						if (error instanceof CancellationException) {
							statusTextPane.setText("Cancelled");
						} else {
							statusTextPane.setText(error.getMessage());
						}
						statusTextPane.setCaretPosition(0);
					}
					jPanel4.revalidate();
					jPanel2.repaint();
				}
			});
		}

		private String getText() {
			String text = "";
			if (numStatements > 1) {
				text += numStatements + " Statements. ";
			}
			if (numRowsRead > 0 || numRowsUpdated == 0) {
				text += numRowsRead + " rows read";
				if (limitExceeded) {
					text += " (limit exceeded)";
				}
				text += ". ";
			}
			if (numRowsUpdated > 0 || numRowsRead == 0) {
				text += numRowsUpdated + " rows updated. ";
			}
			return text + "Elapsed time: " + (timeInMS / 1000.0) + " sec";
		}
	}
	
	/**
	 * This method is called from within the constructor to initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is always
	 * regenerated by the Form Editor.
	 */
	// <editor-fold defaultstate="collapsed" desc="Generated
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jPanel1 = new javax.swing.JPanel();
        jSplitPane1 = new javax.swing.JSplitPane();
        jPanel2 = new javax.swing.JPanel();
        consoleContainerPanel = new javax.swing.JPanel();
        statusLabel = new javax.swing.JLabel();
        jPanel4 = new javax.swing.JPanel();
        statusScrollPane = new javax.swing.JScrollPane();
        statusTextPane = new javax.swing.JTextPane();
        jPanel5 = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        limitComboBox = new javax.swing.JComboBox();
        cancelButton = new javax.swing.JButton();
        jLabel2 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        jLabel6 = new javax.swing.JLabel();
        jLabel7 = new javax.swing.JLabel();
        jPanel3 = new javax.swing.JPanel();
        jTabbedPane1 = new javax.swing.JTabbedPane();

        setLayout(new java.awt.GridBagLayout());

        jPanel1.setLayout(new java.awt.GridBagLayout());

        jSplitPane1.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        jSplitPane1.setResizeWeight(0.5);

        jPanel2.setLayout(new java.awt.GridBagLayout());

        consoleContainerPanel.setLayout(new java.awt.BorderLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel2.add(consoleContainerPanel, gridBagConstraints);

        statusLabel.setText("jLabel2");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel2.add(statusLabel, gridBagConstraints);

        jPanel4.setLayout(new java.awt.GridBagLayout());

        statusTextPane.setEditable(false);
        statusTextPane.setForeground(java.awt.Color.red);
        statusScrollPane.setViewportView(statusTextPane);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridheight = 40;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel4.add(statusScrollPane, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridheight = 20;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        jPanel2.add(jPanel4, gridBagConstraints);

        jPanel5.setLayout(new java.awt.GridBagLayout());

        jLabel1.setText("Row limit ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 0);
        jPanel5.add(jLabel1, gridBagConstraints);

        limitComboBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 2;
        jPanel5.add(limitComboBox, gridBagConstraints);

        cancelButton.setText("Cancel");
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 2;
        jPanel5.add(cancelButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTH;
        jPanel2.add(jPanel5, gridBagConstraints);

        jLabel2.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        jPanel2.add(jLabel2, gridBagConstraints);

        jLabel3.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        jPanel2.add(jLabel3, gridBagConstraints);

        jLabel5.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 4;
        jPanel2.add(jLabel5, gridBagConstraints);

        jLabel6.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 5;
        jPanel2.add(jLabel6, gridBagConstraints);

        jLabel7.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 6;
        jPanel2.add(jLabel7, gridBagConstraints);

        jSplitPane1.setLeftComponent(jPanel2);

        jPanel3.setBorder(javax.swing.BorderFactory.createTitledBorder("Result"));
        jPanel3.setLayout(new java.awt.BorderLayout());
        jPanel3.add(jTabbedPane1, java.awt.BorderLayout.CENTER);

        jSplitPane1.setRightComponent(jPanel3);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel1.add(jSplitPane1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(jPanel1, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
        CancellationHandler.cancel(this);
    }//GEN-LAST:event_cancelButtonActionPerformed

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton cancelButton;
    private javax.swing.JPanel consoleContainerPanel;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JSplitPane jSplitPane1;
    private javax.swing.JTabbedPane jTabbedPane1;
    private javax.swing.JComboBox limitComboBox;
    private javax.swing.JLabel statusLabel;
    private javax.swing.JScrollPane statusScrollPane;
    private javax.swing.JTextPane statusTextPane;
    // End of variables declaration//GEN-END:variables

    class ResultContentPane extends BrowserContentPane {
		public ResultContentPane(DataModel dataModel, Table table, String condition, Session session, Row parentRow,
				List<Row> parentRows, Association association, Frame parentFrame,
				Set<Pair<BrowserContentPane, Row>> currentClosure,
				Set<Pair<BrowserContentPane, String>> currentClosureRowIDs, Integer limit, Boolean selectDistinct,
				boolean reload, ExecutionContext executionContext) {
			super(dataModel, table, condition, session, parentRow, parentRows, association, parentFrame, currentClosure,
					currentClosureRowIDs, limit, selectDistinct, reload, executionContext);
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
			jTabbedPane1.repaint();
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
			Window owner = SwingUtilities.getWindowAncestor(SQLConsole.this);
			if (owner instanceof JFrame) {
				return (JFrame) owner;
			}
			return null;
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
			return false;
		}
		@Override
		protected MetaDataSource getMetaDataSource() {
			return null;
		}
		@Override
		protected SQLConsole getSqlConsole() {
			return SQLConsole.this;
		}
	};

	private JPanel getTitlePanel(final JTabbedPane tabbedPane, final JComponent rTabContainer, String title) {
		JPanel titlePanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
		titlePanel.setOpaque(false);
		JLabel titleLbl = new JLabel(title);
		titleLbl.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 5));
		titlePanel.add(titleLbl);
		SmallButton closeButton = new SmallButton(closeIcon) {
			@Override
			protected void onClick() {
				tabbedPane.remove(rTabContainer);
			}
		};
		titlePanel.add(closeButton);

		return titlePanel;
	}
	
	public void grabFocus() {
		editorPane.grabFocus();
	}
	
	private Icon closeIcon;
	{
		String dir = "/net/sf/jailer/ui/resource";
		
		// load images
		try {
			closeIcon = new ImageIcon(getClass().getResource(dir + "/Close-16-1.png"));
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Appends a statement and eventually executes it.
	 * 
	 * @param sql the statement
	 * @param execute execute the statement?
	 */
	public void appendStatement(String sql, boolean execute) {
		String pre = "";
		int lineCount = editorPane.getLineCount();
		if (lineCount > 0 && editorPane.getDocument().getLength() > 0) {
			pre = "\n";
			if (editorPane.getText(lineCount - 1, lineCount - 1, true).trim().length() > 0) {
				pre += "\n";
			}
		}
		editorPane.append(pre + sql + "\n");
		resetStatus();
		if (execute) {
			executeSelectedStatements();
		}
	}

	private void executeSelectedStatements() {
		Pair<Integer, Integer> loc = editorPane.getCurrentStatementLocation(false);
		if (loc != null) {
			executeSQLBlock(editorPane.getText(loc.a, loc.b, true), loc);
		}
	}
	
}
