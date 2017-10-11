/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package net.sf.jailer.ui.databrowser.sqlconsole;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Types;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListCellRenderer;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.SwingUtilities;
import javax.swing.text.BadLocationException;

import org.fife.ui.autocomplete.AutoCompletion;
import org.fife.ui.autocomplete.BasicCompletion;
import org.fife.ui.autocomplete.Completion;
import org.fife.ui.autocomplete.CompletionCellRenderer;
import org.fife.ui.autocomplete.Util;
import org.fife.ui.rtextarea.RTextScrollPane;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.modelbuilder.MetaDataCache.CachedResultSet;
import net.sf.jailer.ui.DbConnectionDialog;
import net.sf.jailer.ui.Environment;
import net.sf.jailer.ui.JComboBox;
import net.sf.jailer.ui.QueryBuilderDialog;
import net.sf.jailer.ui.QueryBuilderDialog.Relationship;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.BrowserContentPane;
import net.sf.jailer.ui.databrowser.BrowserContentPane.LoadJob;
import net.sf.jailer.ui.databrowser.DataBrowser;
import net.sf.jailer.ui.databrowser.Desktop.RowBrowser;
import net.sf.jailer.ui.databrowser.QueryBuilderPathSelector;
import net.sf.jailer.ui.databrowser.Reference;
import net.sf.jailer.ui.databrowser.Row;
import net.sf.jailer.ui.databrowser.metadata.MetaDataPanel;
import net.sf.jailer.ui.databrowser.metadata.MetaDataSource;
import net.sf.jailer.ui.syntaxtextarea.RSyntaxTextAreaWithSQLSyntaxStyle;
import net.sf.jailer.ui.syntaxtextarea.SQLCompletionProvider;
import net.sf.jailer.ui.util.SmallButton;
import net.sf.jailer.util.CancellationException;
import net.sf.jailer.util.CancellationHandler;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.Pair;

/**
 * SQL Console.
 *
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public abstract class SQLConsole extends javax.swing.JPanel {

	private static final int MAX_TAB_COUNT = 8;
	private static final int MAX_HISTORY_SIZE = 100;
	
	private Session session;
	MetaDataSource metaDataSource;
	private RSyntaxTextAreaWithSQLSyntaxStyle editorPane;
	private final MetaDataBasedSQLCompletionProvider provider;
	private final BlockingQueue<Runnable> queue = new LinkedBlockingQueue<Runnable>();
	private final Reference<DataModel> datamodel;
	private final ExecutionContext executionContext;
	private final List<String> history = new ArrayList<String>();
	private final AtomicBoolean running = new AtomicBoolean(false);
	private final AtomicBoolean updatingStatus = new AtomicBoolean(false);
	
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

		historyComboBox.setMaximumRowCount(25);
        GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 16);
        jPanel5.add(historyComboBox, gridBagConstraints);
		
		this.editorPane = new RSyntaxTextAreaWithSQLSyntaxStyle(true) {
			@Override
			protected void runBlock() {
				executeSelectedStatements();
			}
			@Override
			protected void runAll() {
				executeAllStatements();
			}
			@Override
			public void updateMenuItemState() {
				updateMenuItemState(!running.get(), !running.get());
			}
		};

		historyComboBox.setRenderer(new DefaultListCellRenderer() {
			 public Component getListCellRendererComponent(JList<?> list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
				 if (index == 0) {
					 value = null;
				 }
				 Object shortValue = value;
				 if (value instanceof String) {
					 shortValue = shortSQL((String) value, 120);
				 }
				 Component c = super.getListCellRendererComponent(list, shortValue, index, isSelected, cellHasFocus);
				 if (c instanceof JLabel) {
					 if (value instanceof String && value.toString().length() > 0) {
						 ((JLabel) c).setToolTipText(UIUtil.toHTML(value.toString(), 160));
					 } else {
						 ((JLabel) c).setToolTipText(null);
					 }
				 }
				 return c;
			 }
		});
		restoreHistory();
		
		provider = new MetaDataBasedSQLCompletionProvider(session, metaDataSource);
		AutoCompletion ac = new AutoCompletion(provider);
		ac.install(editorPane);
		
		ac.setListCellRenderer(new CompletionCellRenderer() {
			@Override
			public Component getListCellRendererComponent(JList list, Object value, int index, boolean selected,
					boolean hasFocus) {
				Component c = super.getListCellRendererComponent(list, value, index, selected, hasFocus);
				if (c instanceof JLabel && value instanceof SQLCompletionProvider.SQLCompletion) {
					((JLabel) c).setToolTipText(((SQLCompletionProvider.SQLCompletion) value).tooltip);
				}
				return c;
			}

			protected void prepareForOtherCompletion(JList list,
				Completion c, int index, boolean selected, boolean hasFocus) {

				Color color = null;
				if (c instanceof SQLCompletionProvider.SQLCompletion) {
					color = ((SQLCompletionProvider.SQLCompletion) c).color;
				}
				
				StringBuilder sb = new StringBuilder("<html><nobr>");
				if (!selected && color != null) {
					sb.append("<font color='").append(Util.getHexString(color)).append("'>");
				}
				sb.append(c.getInputText());
				if (!selected && color != null) {
					sb.append("</font>");
				}

				if (c instanceof BasicCompletion) {
					String definition = ((BasicCompletion)c).getShortDescription();
					if (definition!=null) {
						sb.append(" - ");
						if (!selected) {
							sb.append("<font color='").append(Util.getHexString(Color.gray)).append("'>");
						}
						sb.append(definition);
						if (!selected) {
							sb.append("</font>");
						}
					}
				}
				
				setText(sb.toString());
			}
		});
		RTextScrollPane jScrollPane = new RTextScrollPane();
		jScrollPane.setViewportView(editorPane);
		consoleContainerPanel.add(jScrollPane);
		jScrollPane.setLineNumbersEnabled(true);

		runSQLButton.setAction(editorPane.runBlock);
		runnAllButton.setAction(editorPane.runAll);

		runSQLButton.setText("Run");
		runnAllButton.setText("Run all");

		runSQLButton.setIcon(getScaledIcon(this, runIcon));
		runnAllButton.setIcon(getScaledIcon(this, runAllIcon));
		runSQLButton.setToolTipText(runSQLButton.getText() + " - Ctrl-Enter");
		runnAllButton.setToolTipText(runnAllButton.getText() + " - Alt-Enter");
		runnAllButton.setMargin(new Insets(0, 0, 0, 0));
				
		cancelButton.setIcon(getScaledIcon(this, cancelIcon));
		
		limitComboBox.setModel(new DefaultComboBoxModel(DataBrowser.ROW_LIMITS));
		limitComboBox.setSelectedItem(1000);
		
		resetStatus();
		statusLabel.setVisible(true);

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

	protected String shortSQL(String sql, int maxLength) {
		sql = sql.trim().replaceAll("\\s+", " ");
    	if (sql.length() > maxLength) {
    		sql = sql.replaceFirst("^(?is)(\\bselect\\b).........*?(\\bfrom\\b)(.*)$", "$1 ... $2$3");
    	}
    	if (sql.length() > maxLength) {
    		sql = sql.substring(0, maxLength) + "...";
    	}
    	return sql;
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
	protected void executeSQLBlock(final String sqlBlock, final Pair<Integer, Integer> location) {
		if (!running.get()) {
			int lineStartOffset = -1;
			try {
				if (location != null) {
					lineStartOffset = editorPane.getLineStartOffset(location.a);
				}
			} catch (BadLocationException e) {
				lineStartOffset = -1;
			}
			final int finalLineStartOffset = lineStartOffset;
			queue.add(new Runnable() {
				@Override
				public void run() {
					running.set(true);
					updatingStatus.set(false);
					SwingUtilities.invokeLater(new Runnable() {
						@Override
						public void run() {
							editorPane.updateMenuItemState();
						}
					});
					Status status = new Status();
					status.location = location;
					status.linesExecuted = 0;
					status.linesExecuting = 0;
					status.running = true;
					int lineStartOffset = finalLineStartOffset;
					try {
						Pattern pattern = Pattern.compile("(?:(;\\s*(\\n\\r?|$))|(\\n\\r?([ \\t\\r]*\\n\\r?)+))", Pattern.DOTALL);
						Matcher matcher = pattern.matcher(sqlBlock);
						boolean result = matcher.find();
						StringBuffer sb = new StringBuffer();
						if (result) {
							do {
								sb.setLength(0);
								matcher.appendReplacement(sb, "$1");
								String sql = sb.toString();
								String pureSql = sql.replaceFirst(";\\s*$", "");
								status.linesExecuting += countLines(pureSql);
								if (sql.trim().length() > 0) {
									executeSQL(pureSql, status, lineStartOffset);
									if (status.failed) {
										break;
									}
								}
								if (lineStartOffset >= 0) {
									lineStartOffset += sql.length();
								}
								status.linesExecuted += countLines(sql) - 1;
								status.linesExecuting = status.linesExecuted;
								result = matcher.find();
							} while (result);
						}
						if (!status.failed) {
							sb.setLength(0);
							matcher.appendTail(sb);
							String sql = sb.toString();
							if (sql.trim().length() > 0) {
								status.linesExecuting += countLines(sql);
								executeSQL(sql, status, lineStartOffset);
								if (!status.failed) {
									status.linesExecuted = status.linesExecuting;
								}
							}
						}
						if (status.numStatements <= 7) {
							storeHistory();
						} else {
							restoreHistory();
						}
					} finally {
						status.running = false;
						running.set(false);
						status.updateView(true);
						SwingUtilities.invokeLater(new Runnable() {
							@Override
							public void run() {
					    		editorPane.updateMenuItemState(true, false);
							}
						});
					}
				}

				private int countLines(String line) {
					int lines = 1;
					for (int i = 0; i < line.length(); ++i) {
						if (line.charAt(i) == '\n') {
							++lines;
						}
					}
					return lines;
				}
			});
		}
	}

	/**
	 * Executes a single SQL statment.
	 * 
	 * @param sql the statement
	 * @param status the status to update
	 * @param statementStartOffset 
	 */
	private void executeSQL(final String sql, Status status, int statementStartOffset) {
		Statement statement = null;
		ResultSet resultSet = null;
		final Status localStatus = new Status();
		String sqlStatement = null;
		try {
			status.numStatements++;
			localStatus.numStatements++;
			status.updateView(false);
			statement = session.getConnection().createStatement();
			CancellationHandler.begin(statement, SQLConsole.this);
			long startTime = System.currentTimeMillis();
			sqlStatement = sql.replaceFirst("(?is)(;\\s*)+$", "");
			if (statement.execute(sqlStatement)) {
				resultSet = statement.getResultSet();
				Integer limit = (Integer) limitComboBox.getSelectedItem();
				final BrowserContentPane rb = new ResultContentPane(datamodel.get(), null, "", session, null, null,
						null, null, new HashSet<Pair<BrowserContentPane, Row>>(), new HashSet<Pair<BrowserContentPane, String>>(), limit, false, false, executionContext);
				final CachedResultSet metaDataDetails = new CachedResultSet(resultSet, limit, session, SQLConsole.this);
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
				status.updateView(false);
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
			        	tabContentPanel.statementLabel.setToolTipText(UIUtil.toHTML(sql, 160));
			        	if (stmt.length() > 200) {
			        		stmt = stmt.substring(0, 200) + "...";
			        	}
						tabContentPanel.statementLabel.setText(stmt);
			        	tabContentPanel.reloadButton.addActionListener(new ActionListener() {
							@Override
							public void actionPerformed(ActionEvent e) {
								if (!running.get()) {
									jTabbedPane1.remove(tabContentPanel);
									executeSQLBlock(sql, null);
								}
							}
						});
			        	rTabContainer = tabContentPanel;
			        	final int MAXLENGTH = 30;
			        	String title = shortSQL(sql, MAXLENGTH);
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
		    	status.updateView(false);
		    	if (updateCount != 0) {
		    		setDataHasChanged(true);
		    	}
		    	if (isDDLStatement(sql)) {
		    		status.withDDL = true;
		    	}
			}
			CancellationHandler.end(statement, SQLConsole.this);
			statement.close();
			appendHistory(sql);
		} catch (Throwable error) {
			try {
				CancellationHandler.checkForCancellation(SQLConsole.this);
			} catch (CancellationException e) {
				error = e;
			}
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
			if (error instanceof SQLException && sqlStatement != null && statementStartOffset >= 0) {
				int pos = retrieveErrorPos(sqlStatement, error.getMessage());
				if (pos >= 0) {
					status.errorPosition  = statementStartOffset + pos;
				}
			}
			
			if (error instanceof CancellationException) {
				CancellationHandler.reset(SQLConsole.this);
				queue.clear();
			}
			status.failed = true;
			status.error = error;
			status.updateView(false);
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
		public int errorPosition = -1;
		protected int linesExecuting;
		protected int linesExecuted;
		public boolean withDDL;
		boolean failed;
		boolean running;
		boolean limitExceeded;
		int numRowsRead;
		int numRowsUpdated;
		int numStatements;
		long timeInMS;
		Throwable error;
		Pair<Integer, Integer> location;

		private synchronized void updateView(boolean force) {
			if (force || !updatingStatus.get()) {
				updatingStatus.set(true);
				
				SwingUtilities.invokeLater(new Runnable() {
					@Override
					public void run() {
						try {
							synchronized (Status.this) {
								Font font = new JLabel("X").getFont();
								statusLabel.setFont(new Font(font.getName(), font.getStyle(), (font.getSize() * 13) / 10));
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
									} else if (error instanceof SQLException) {
										String pos = "";
										if (errorPosition >= 0) {
											try {
												int line = editorPane.getLineOfOffset(errorPosition);
												int col = errorPosition - editorPane.getLineStartOffset(line);
												pos = "Error at line " + (line + 1) + ", column " + col + ": ";
												editorPane.setCaretPosition(errorPosition);
											} catch (BadLocationException e) {
											}
										}
										statusTextPane.setText(pos + error.getMessage());
									} else {
										StringWriter sw = new StringWriter();
										PrintWriter pw = new PrintWriter(sw);
										error.printStackTrace(pw);
										String sStackTrace = sw.toString(); // stack trace as a string
										statusTextPane.setText(sStackTrace);
									}
									statusTextPane.setCaretPosition(0);
								}
							}
							Color failedColor = new Color(255, 170, 170);
							Color okColor = new Color(210, 255, 210);
							Color pendingColor = new Color(235, 235, 255);
							Color runningColor = new Color(255, 255, 210);
							if (location != null) {
								editorPane.removeAllLineHighlights();
								try {
									for (int i = location.a; i <= location.b; ++i) {
										Color hl;
										if (i < linesExecuted + location.a) {
											hl = okColor;
										} else if (i >= linesExecuting + location.a) {
											hl = pendingColor;
										} else {
											if (failed) {
												hl = failedColor;
											} else if (running){
												hl = runningColor;
											} else {
												hl = pendingColor;
											}
										}
										editorPane.addLineHighlight(i, hl);
									}
								} catch (BadLocationException e) {
								}
							}
							
							jPanel4.revalidate();
							jPanel2.repaint();
							
							if (withDDL && !running) {
								withDDL = false;
								refreshMetaData();
							}
						} finally {
							updatingStatus.set(false);
						}
					}
				});
			}
		}

		private String getText() {
			String text = "";
			if (running) {
				text = "Running... ";
				if (numStatements <= 1 && numRowsRead == 0 && numRowsUpdated == 0) {
					return text;
				}
			}
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
        jSplitPane2 = new javax.swing.JSplitPane();
        jPanel2 = new javax.swing.JPanel();
        consoleContainerPanel = new javax.swing.JPanel();
        jPanel5 = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        limitComboBox = new javax.swing.JComboBox();
        cancelButton = new javax.swing.JButton();
        runSQLButton = new javax.swing.JButton();
        runnAllButton = new javax.swing.JButton();
        jPanel6 = new javax.swing.JPanel();
        jLabel3 = new javax.swing.JLabel();
        jLabel6 = new javax.swing.JLabel();
        jLabel7 = new javax.swing.JLabel();
        statusLabel = new javax.swing.JLabel();
        jPanel4 = new javax.swing.JPanel();
        statusScrollPane = new javax.swing.JScrollPane();
        statusTextPane = new javax.swing.JTextPane();
        jPanel3 = new javax.swing.JPanel();
        jTabbedPane1 = new javax.swing.JTabbedPane();

        setLayout(new java.awt.GridBagLayout());

        jPanel1.setLayout(new java.awt.GridBagLayout());

        jSplitPane1.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        jSplitPane1.setResizeWeight(0.5);

        jSplitPane2.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        jSplitPane2.setResizeWeight(1.0);

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

        jPanel5.setLayout(new java.awt.GridBagLayout());

        jLabel1.setText("Row limit ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 13;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 0);
        jPanel5.add(jLabel1, gridBagConstraints);

        limitComboBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 14;
        gridBagConstraints.gridy = 2;
        jPanel5.add(limitComboBox, gridBagConstraints);

        cancelButton.setText("Cancel");
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 12;
        gridBagConstraints.gridy = 2;
        jPanel5.add(cancelButton, gridBagConstraints);

        runSQLButton.setText("Run");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        jPanel5.add(runSQLButton, gridBagConstraints);

        runnAllButton.setText("Run all");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 8);
        jPanel5.add(runnAllButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTH;
        gridBagConstraints.weightx = 1.0;
        jPanel2.add(jPanel5, gridBagConstraints);

        jSplitPane2.setLeftComponent(jPanel2);

        jPanel6.setLayout(new java.awt.GridBagLayout());

        jLabel3.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        jPanel6.add(jLabel3, gridBagConstraints);

        jLabel6.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 5;
        jPanel6.add(jLabel6, gridBagConstraints);

        jLabel7.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 6;
        jPanel6.add(jLabel7, gridBagConstraints);

        statusLabel.setForeground(java.awt.Color.gray);
        statusLabel.setText(" ctrl-space for code completion");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridheight = 20;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel6.add(statusLabel, gridBagConstraints);

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
        gridBagConstraints.weighty = 1.0;
        jPanel6.add(jPanel4, gridBagConstraints);

        jSplitPane2.setRightComponent(jPanel6);

        jSplitPane1.setLeftComponent(jSplitPane2);

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
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JSplitPane jSplitPane1;
    private javax.swing.JSplitPane jSplitPane2;
    private javax.swing.JTabbedPane jTabbedPane1;
    private javax.swing.JComboBox limitComboBox;
    private javax.swing.JButton runSQLButton;
    private javax.swing.JButton runnAllButton;
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
			rowsTableScrollPane.setWheelScrollingEnabled(true);
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
		protected SQLConsole getSqlConsole(boolean switchToConsole) {
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
		editorPane.append(pre + (sql.replaceFirst("(?is)(;\\s*)+$", "").trim()) + "\n");
		editorPane.setCaretPosition(editorPane.getDocument().getLength());
		editorPane.grabFocus();
		if (!running.get()) {
			resetStatus();
		}
		if (execute) {
			executeSelectedStatements();
		}
	}

	private void executeSelectedStatements() {
		Pair<Integer, Integer> loc = editorPane.getCurrentStatementLocation();
		if (loc != null) {
			executeSQLBlock(editorPane.getText(loc.a, loc.b, true), loc);
		}
	}

	private void executeAllStatements() {
		if (editorPane.getLineCount() > 0) {
			Pair<Integer, Integer> loc = new Pair<Integer, Integer>(0, editorPane.getLineCount() - 1);
			executeSQLBlock(editorPane.getText(loc.a, loc.b, true), loc);
		}
	}

	private final String HISTORY_FILE = ".history";
	private final String LF = System.getProperty("line.separator", "\n");
	private final JComboBox historyComboBox = new JComboBox() {
		@Override
		public java.awt.Dimension getMinimumSize() {
			Dimension s = super.getMinimumSize();
			return new Dimension(Math.min(80, s.width), s.height);
		}
	};
	
	private synchronized void restoreHistory() {
		try {
			File file = Environment.newFile(HISTORY_FILE);
			if (file.exists()) {
				BufferedReader in = new BufferedReader(new FileReader(file));
				history.clear();
				String line;
				while ((line = in.readLine()) != null) {
					String[] lines = CsvFile.decodeLine(line.trim());
					if (lines.length > 0 && !lines[0].isEmpty()) {
						history.add(lines[0]);
					}
				}
				in.close();
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		historyComboBox.setModel(historyComboboxModel());
	}

	private synchronized void storeHistory() {
		try {
			File file = Environment.newFile(HISTORY_FILE);
			FileWriter out = new FileWriter(file);
			for (String sql: history) {
				out.write(CsvFile.encodeCell(sql) + LF);
			}
			out.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		historyComboBox.setModel(historyComboboxModel());
	}

	private final ItemListener historyCBItemListener = new ItemListener() {
		@Override
		public void itemStateChanged(ItemEvent e) {
			if (e.getStateChange() == ItemEvent.SELECTED && e.getItem() != null && historyComboBox.getSelectedIndex() > 0) {
				appendStatement(e.getItem().toString(), false);
				historyComboBox.setSelectedIndex(0);
			}
		}
	};
	
	private DefaultComboBoxModel historyComboboxModel() {
		ArrayList<String> model = new ArrayList<String>(history);
		if (!model.isEmpty()) {
			model.add(0, new String(model.get(0) + " "));
		}
		historyComboBox.removeItemListener(historyCBItemListener);
		DefaultComboBoxModel cbModel = new DefaultComboBoxModel(model.toArray());
		historyComboBox.addItemListener(historyCBItemListener);
		return cbModel;
	}

	private synchronized void appendHistory(String sql) {
		sql = sql.trim();
		if (!sql.isEmpty()) {
			history.remove(sql);
			history.add(0, sql);
			if (history.size() > MAX_HISTORY_SIZE) {
				history.remove(history.size() - 1);
			}
		}
	}

	private int retrieveErrorPos(String sqlStatement, CharSequence errorMessage) {
		if (DBMS.ORACLE.equals(session.dbms)) {
			String statement = "declare "
					+ "l_theCursor integer default dbms_sql.open_cursor;" + "begin      " + "    begin     "
					+ "      dbms_sql.parse(  l_theCursor, ?, dbms_sql.native );"
					+ "      exception when others then ? := dbms_sql.last_error_position;" + "    end;"
					+ "    dbms_sql.close_cursor( l_theCursor );" + "end;";

			try {
				Connection connection = session.getConnection();
				CallableStatement cStmt = connection.prepareCall(statement);
				cStmt.registerOutParameter(2, Types.INTEGER);
				cStmt.setString(1, sqlStatement);

				cStmt.execute();

				return cStmt.getInt(2);
			} catch (Exception e) {
			}
		} else if (DBMS.POSTGRESQL.equals(session.dbms)) {
			Pattern pattern = Pattern.compile("\\n\\s*Position: ([0-9]+)");
			Matcher matcher = pattern.matcher(errorMessage);
			if (matcher.find()) {
				return Integer.parseInt(matcher.group(1)) - 1;
			}
		}
		return -1;
	}

	static private ImageIcon runIcon;
    static private ImageIcon runAllIcon;
    static private ImageIcon cancelIcon;
    static ImageIcon getScaledIcon(JComponent component, ImageIcon scaledIcon) {
    	if (scaledIcon != null) {
            if (scaledIcon != null) {
            	int heigth = component.getFontMetrics(new JLabel("M").getFont()).getHeight();
            	double s = heigth / (double) scaledIcon.getIconHeight();
            	try {
            		return new ImageIcon(scaledIcon.getImage().getScaledInstance((int)(scaledIcon.getIconWidth() * s), (int)(scaledIcon.getIconHeight() * s), Image.SCALE_SMOOTH));
            	} catch (Exception e) {
            		return null;
            	}
            }
    	}
    	return null;
    }
    static {
		String dir = "/net/sf/jailer/ui/resource";
		
		// load images
		try {
			runIcon = new ImageIcon(MetaDataPanel.class.getResource(dir + "/run.png"));
			runAllIcon = new ImageIcon(MetaDataPanel.class.getResource(dir + "/runall.png"));
			cancelIcon = new ImageIcon(MetaDataPanel.class.getResource(dir + "/Cancel.png"));
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
