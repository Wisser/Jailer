/*
 * Copyright 2007 - 2020 Ralf Wisser.
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
package net.sf.jailer.ui.databrowser;

import java.awt.BasicStroke;
import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Reader;
import java.math.BigDecimal;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.SQLXML;
import java.sql.Types;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.WeakHashMap;
import java.util.concurrent.PriorityBlockingQueue;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.BorderFactory;
import javax.swing.DefaultCellEditor;
import javax.swing.DefaultComboBoxModel;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.InputMap;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JInternalFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.JTree;
import javax.swing.KeyStroke;
import javax.swing.ListCellRenderer;
import javax.swing.RowSorter;
import javax.swing.RowSorter.SortKey;
import javax.swing.SortOrder;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.UIManager;
import javax.swing.border.LineBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.event.RowSorterEvent;
import javax.swing.event.RowSorterListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;
import javax.swing.table.TableRowSorter;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;

import net.coderazzi.filters.gui.AutoChoices;
import net.coderazzi.filters.gui.TableFilterHeader;
import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.database.InlineViewStyle;
import net.sf.jailer.database.Session;
import net.sf.jailer.database.Session.AbstractResultSetReader;
import net.sf.jailer.database.SqlException;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Cardinality;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.PrimaryKey;
import net.sf.jailer.datamodel.RestrictionDefinition;
import net.sf.jailer.datamodel.RowIdSupport;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.extractionmodel.ExtractionModel;
import net.sf.jailer.extractionmodel.SubjectLimitDefinition;
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;
import net.sf.jailer.modelbuilder.MemorizedResultSet.MemorizedResultSetMetaData;
import net.sf.jailer.subsetting.ScriptFormat;
import net.sf.jailer.ui.DataModelManager;
import net.sf.jailer.ui.DbConnectionDialog;
import net.sf.jailer.ui.Environment;
import net.sf.jailer.ui.ExtractionModelFrame;
import net.sf.jailer.ui.JComboBox2;
import net.sf.jailer.ui.QueryBuilderDialog;
import net.sf.jailer.ui.QueryBuilderDialog.Relationship;
import net.sf.jailer.ui.StringSearchPanel;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.Desktop.FindClosureContext;
import net.sf.jailer.ui.databrowser.Desktop.RowBrowser;
import net.sf.jailer.ui.databrowser.Desktop.RowToRowLink;
import net.sf.jailer.ui.databrowser.RowCounter.RowCount;
import net.sf.jailer.ui.databrowser.metadata.MetaDataSource;
import net.sf.jailer.ui.databrowser.sqlconsole.SQLConsole;
import net.sf.jailer.ui.scrollmenu.JScrollC2Menu;
import net.sf.jailer.ui.scrollmenu.JScrollMenu;
import net.sf.jailer.ui.syntaxtextarea.BasicFormatterImpl;
import net.sf.jailer.ui.util.AnimationController;
import net.sf.jailer.util.CancellationException;
import net.sf.jailer.util.CancellationHandler;
import net.sf.jailer.util.CellContentConverter;
import net.sf.jailer.util.CellContentConverter.PObjectWrapper;
import net.sf.jailer.util.Pair;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlUtil;

/**
 * Content UI of a row browser frame (as {@link JInternalFrame}s). Contains a
 * table for rendering rows.
 *
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public abstract class BrowserContentPane extends javax.swing.JPanel {

	/**
	 * Concurrently loads rows.
	 */
	public class LoadJob implements RunnableWithPriority {
		private List<Row> rows = Collections.synchronizedList(new ArrayList<Row>());
		private Throwable exception;
		private boolean isCanceled;
		private final int limit;
		private final String andCond;
		private final boolean selectDistinct;
		private boolean finished;
		private final ResultSet inputResultSet;
		private final RowBrowser parentBrowser;
		private Session theSession;
		private final Boolean forceAdjustRows;
		public boolean closureLimitExceeded = false;

		public LoadJob(int limit, String andCond, RowBrowser parentBrowser, boolean selectDistinct) {
			this.andCond = andCond;
			this.selectDistinct = selectDistinct;
			this.inputResultSet = null;
			synchronized (this) {
				this.limit = limit;
				finished = false;
				isCanceled = false;
				this.parentBrowser = parentBrowser;
				this.forceAdjustRows = Desktop.forceAdjustRows;
			}
		}

		public LoadJob(ResultSet inputResultSet, int limit) {
			this.andCond = "";
			this.selectDistinct = false;
			this.inputResultSet = inputResultSet;
			synchronized (this) {
				this.limit = limit;
				finished = false;
				isCanceled = false;
				parentBrowser = null;
				this.forceAdjustRows = Desktop.forceAdjustRows;
			}
		}

		private boolean reconnectIfConnectionIsInvalid(boolean updateMode) {
			try {
				if (inputResultSet == null) {
					if (!session.getConnection().isValid(8)) {
						if (updateMode) {
							UIUtil.invokeLater(new Runnable() {
								@Override
								public void run() {
									loadingLabel.setText("reconnecting...");
								}
							});
						}
						session.reconnect();
						return true;
					}
				}
			} catch (Throwable t) {
				// ignore
			}
			return false;
		}

		@Override
		public void run() {
			int l;
			synchronized (this) {
				theSession = session;
				l = limit;
				if (isCanceled) {
					CancellationHandler.reset(this);
					return;
				}
			}

			boolean reconnectAndRetry = false;

			rowCountCache.clear();
			try {
				reloadRows(inputResultSet, andCond, rows, this, l + 1, selectDistinct);
				CancellationHandler.checkForCancellation(this);
				synchronized (this) {
					finished = true;
				}
			} catch (SQLException e) {
				reconnectAndRetry = true;
			} catch (CancellationException e) {
				Session._log.info("cancelled");
				CancellationHandler.reset(this);
				return;
			} catch (Throwable e) {
				reconnectAndRetry = true;
			}

			if (reconnectAndRetry) {
				rowCountCache.clear();
				boolean reconnected = reconnectIfConnectionIsInvalid(true);
				try {
					if (reconnected) {
						session.getMetaData(); // fail-fast
					}
					reloadRows(inputResultSet, andCond, rows, this, l + 1, selectDistinct);
					CancellationHandler.checkForCancellation(this);
					synchronized (this) {
						finished = true;
					}
				} catch (SQLException e) {
					synchronized (rows) {
						exception = e;
					}
				} catch (CancellationException e) {
					Session._log.info("cancelled");
					CancellationHandler.reset(this);
					return;
				} catch (Throwable e) {
					synchronized (rows) {
						exception = e;
					}
				}
			}

			if (!finished) {
				reconnectIfConnectionIsInvalid(false);
			}
			CancellationHandler.reset(this);
			UIUtil.invokeLater(new Runnable() {
				@Override
				public void run() {
					Boolean oldForceAdjustRows = Desktop.forceAdjustRows;
					try	{
						Desktop.forceAdjustRows = forceAdjustRows;
						Throwable e;
						int l;
						boolean limitExceeded = false;
						synchronized (rows) {
							e = exception;
							l = limit;
							while (rows.size() > limit) {
								limitExceeded = true;
								rows.remove(rows.size() - 1);
							}
							isCanceled = true; // done
							sortRowsByParentViewIndex();
						}
						if (e != null) {
							updateMode("error", null);
							unhide();
							if (theSession == null || !theSession.isDown()) {
								errorMessageTextArea.setText(e.getMessage());
								errorMessageTextArea.setToolTipText(UIUtil.toHTML(UIUtil.lineWrap(e.getMessage(), 100).toString(), 120));
								errorMessageTextArea.setCaretPosition(0);
								if (shouldShowLoadErrors()) {
									SQLException sqlException = null;
									if (e instanceof SqlException && e.getCause() != null && e.getCause() instanceof SQLException) {
										sqlException = (SQLException) e.getCause();
									}
									if (sqlException != null && sqlException.getMessage() != null && sqlException.getMessage().trim().length() > 0) {
										currentErrorDetail = e;
										errorDetailsButton.setVisible(true);
										errorMessageTextArea.setText(sqlException.getMessage());
										errorMessageTextArea.setToolTipText(UIUtil.toHTML(UIUtil.lineWrap(e.getMessage(), 100).toString(), 120));
										if (e instanceof SqlException) {
											String sqlStatement = ((SqlException) e).sqlStatement;
											if (sqlStatement != null && sqlStatement.trim().length() > 0) {
												String HR = "(!insHorizontRulHere!)";
												String sql = HR
														+ new BasicFormatterImpl().format(sqlStatement);
												errorMessageTextArea.setToolTipText(
														UIUtil.toHTML(UIUtil.lineWrap(sqlException.getMessage().trim(), 100).toString()
																+ UIUtil.LINE_SEPARATOR + sql, 120).replace(HR, "<hr>"));
											}
										}
										errorMessageTextArea.setCaretPosition(0);
									} else if (!showingLoadErrorNow) {
										if (getRowBrowser() != null && getRowBrowser().internalFrame != null && getRowBrowser().internalFrame.isVisible()) {
											try {
												showingLoadErrorNow = true;
												UIUtil.showException(BrowserContentPane.this, "Error", e);
											} finally {
												showingLoadErrorNow = false;
											}
										}
									}
								}
							} else {
								theSession = null;
							}
						} else {
							Set<String> prevIDs = new TreeSet<String>();
							long prevHash = 0;
							for (Row r: BrowserContentPane.this.rows) {
								prevIDs.add(r.nonEmptyRowId);
								try {
									for (Object v: r.values) {
										if (v != null) {
											prevHash = 2 * prevHash + v.hashCode();
										}
									}
								} catch (RuntimeException e1) {
									// ignore
								}
							}
							onContentChange(new ArrayList<Row>(), false);
							BrowserContentPane.this.rows.clear();
							BrowserContentPane.this.rows.addAll(rows);
							updateTableModel(l, limitExceeded, closureLimitExceeded);
							Set<String> currentIDs = new TreeSet<String>();
							long currentHash = 0;
							for (Row r: rows) {
								currentIDs.add(r.nonEmptyRowId);
								try {
									for (Object v: r.values) {
										if (v != null) {
											currentHash = 2 * currentHash + v.hashCode();
										}
									}
								} catch (RuntimeException e1) {
									// ignore
								}
							}
							setPendingState(false, true);
							onContentChange(rows, true); // rows.isEmpty() || currentHash != prevHash || rows.size() != prevSize || !prevIDs.equals(currentIDs) || rows.size() != currentIDs.size());
							updateMode("table", null);
							updateWhereField();
							if (reloadAction != null) {
								reloadAction.run();
							}
						}
						afterReload();
					} finally {
						Desktop.forceAdjustRows = oldForceAdjustRows;
					}
				}
			});
		}

		protected void sortRowsByParentViewIndex() {
			// inherit from child-ancestor
			RowBrowser rb = getRowBrowser();
			Map<Row, Row> parentRow = new HashMap<Row, Row>();
			Set<Integer> distinctParentRowModelIndex = new HashSet<Integer>();
			Set<Row> singleBlockRows = new HashSet<Row>();
			synchronized (getMonitorForFindClosure()) {
				if (rb != null && rb.browserContentPane != null && rb.parent != null && rb.association != null && rb.rowToRowLinks != null && rb.association.isInsertDestinationBeforeSource()) {
					for (RowToRowLink l: rb.rowToRowLinks) {
						if (!parentRow.containsKey(l.childRow)) {
							parentRow.put(l.childRow, l.parentRow);
							distinctParentRowModelIndex.add(l.parentRow.getParentModelIndex());
						} else {
							singleBlockRows.add(l.childRow);
						}
					}
				}
			}

			useInheritedBlockNumbers = false;

			if (rb != null && rb.parent != null && rb.parent.browserContentPane != null && rb.parent.browserContentPane.rowsTable != null) {
				final RowSorter<? extends TableModel> parentSorter = rb.parent.browserContentPane.rowsTable.getRowSorter();
				Comparator<Row> comparator = new Comparator<Row>() {
					@Override
					public int compare(Row a, Row b) {
						int avi = a.getParentModelIndex();
						if (avi >= 0 && avi < parentSorter.getModelRowCount()) {
							avi = parentSorter.convertRowIndexToView(avi);
						}
						int bvi = b.getParentModelIndex();
						if (bvi >= 0 && avi < parentSorter.getModelRowCount()) {
							bvi = parentSorter.convertRowIndexToView(bvi);
						}
						return avi - bvi;
					}
				};
				Collections.sort(rows, comparator);
				if (!parentRow.isEmpty() && singleBlockRows.isEmpty() && distinctParentRowModelIndex.size() < parentRow.size() && !rb.parent.browserContentPane.useInheritedBlockNumbers) {
					useInheritedBlockNumbers = true;
					for (Row row: rows) {
						Row pRow = parentRow.get(row);
						if (pRow != null) {
							row.setInheritedParentModelIndex(pRow.getParentModelIndex());
						}
					}
				}
			}
		}

		public synchronized void cancel() {
			if (isCanceled) {
				return;
			}
			isCanceled = true;
			if (finished) {
				return;
			}
			CancellationHandler.cancel(this);
		}

		public synchronized void checkCancellation() {
			CancellationHandler.checkForCancellation(this);
		}

		@Override
		public int getPriority() {
			return 100;
		}
	}

	private Runnable reloadAction;

	public void setOnReloadAction(final Runnable runnable) {
		if (reloadAction == null) {
			reloadAction = runnable;
		} else {
			final Runnable prevReloadAction = reloadAction;
			reloadAction = new Runnable() {
				@Override
				public void run() {
					prevReloadAction.run();
					runnable.run();
				}
			};
		}
	}

	/**
	 * Current LoadJob.
	 */
	private LoadJob currentLoadJob;

	/**
	 * Table to read rows from.
	 */
	Table table;

	/**
	 * The data model.
	 */
	private final DataModel dataModel;

	/**
	 * The execution context.
	 */
	protected final ExecutionContext executionContext;

	/**
	 * {@link RowIdSupport} for data model.
	 */
	private final RowIdSupport rowIdSupport;

	/**
	 * {@link Association} with parent row, or <code>null</code>.
	 */
	Association association;

	/**
	 * Rows to render.
	 */
	public List<Row> rows = new ArrayList<Row>();

	/**
	 * For in-place editing.
	 */
	private BrowserContentCellEditor browserContentCellEditor = new BrowserContentCellEditor(new int[0], new String[0], null);

	/**
	 * Cache for association row count.
	 */
	private final Map<Pair<String, Association>, Pair<RowCount, Long>> rowCountCache =
			Collections.synchronizedMap(new HashMap<Pair<String,Association>, Pair<RowCount, Long>>());

	private final long MAX_ROWCOUNTCACHE_RETENTION_TIME = 5 * 60 * 1000L;

	/**
	 * Number of non-distinct rows;
	 */
	private int noNonDistinctRows;

	/**
	 * Number of distinct rows;
	 */
	private int noDistinctRows;

	/**
	 * Indexes of primary key columns.
	 */
	private Set<Integer> pkColumns = new HashSet<Integer>();

	/**
	 * Indexes of foreign key columns.
	 */
	private Set<Integer> fkColumns = new HashSet<Integer>();

	/**
	 * Edit mode?
	 */
	private boolean isEditMode = false;

	/**
	 * To be shown as "Detail..." in error view.
	 */
	private Throwable currentErrorDetail;

	/**
	 * DB session.
	 */
	Session session;

	private int currentRowSelection = -1;

	public int getCurrentRowSelection() {
		return currentRowSelection;
	}

	private List<Row> parentRows;
	private DetailsView singleRowDetailsView;
	private MouseListener rowTableListener;
	private int initialRowHeight;
	private boolean useInheritedBlockNumbers;
	boolean ignoreSortKey = false;

	public static class RowsClosure {
		Set<Pair<BrowserContentPane, Row>> currentClosure = Collections.synchronizedSet(new HashSet<Pair<BrowserContentPane, Row>>());
		Set<Pair<BrowserContentPane, String>> currentClosureRowIDs = new HashSet<Pair<BrowserContentPane, String>>();
		Set<String> currentClosureRootID = new HashSet<String>();
		BrowserContentPane currentClosureRootPane;
		Set<BrowserContentPane> parentPath = new HashSet<BrowserContentPane>();

		Set<Row> tempClosure = new HashSet<Row>();
	}

	private final RowsClosure rowsClosure;

	private boolean suppressReload;

	/**
	 * Alias for row number column.
	 */
	private static final String ROWNUMBERALIAS = "RN";

	protected static final String UNKNOWN = "- unknown column -";

	public static final int MAXBLOBLENGTH = 32;
	public static final int MAXCLOBLENGTH = 4000 - 16;

	private static final KeyStroke KS_COPY_TO_CLIPBOARD = KeyStroke.getKeyStroke(KeyEvent.VK_C, InputEvent.CTRL_DOWN_MASK);
	private static final KeyStroke KS_SQLCONSOLE = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, InputEvent.CTRL_DOWN_MASK);
	private static final KeyStroke KS_QUERYBUILDER = KeyStroke.getKeyStroke(KeyEvent.VK_Q, InputEvent.CTRL_DOWN_MASK);
	private static final KeyStroke KS_FILTER = KeyStroke.getKeyStroke(KeyEvent.VK_F, InputEvent.CTRL_DOWN_MASK);
	private static final KeyStroke KS_EDIT = KeyStroke.getKeyStroke(KeyEvent.VK_E, InputEvent.CTRL_DOWN_MASK);
	private static final KeyStroke KS_DETAILS = KeyStroke.getKeyStroke(KeyEvent.VK_D, InputEvent.CTRL_DOWN_MASK);

	/**
	 * And-condition-combobox model.
	 */
	private final DefaultComboBoxModel<Object> andCondModel = new DefaultComboBoxModel<Object>();

	private boolean isTableFilterEnabled = false;

	public void setTableFilterEnabled(boolean isTableFilterEnabled) {
		this.isTableFilterEnabled = isTableFilterEnabled;
	}

	static class SqlStatementTable extends Table {
		public SqlStatementTable(String name, PrimaryKey primaryKey, boolean defaultUpsert) {
			super(name, primaryKey, defaultUpsert, false);
		}
	}

	/**
	 * Constructor.
	 *
	 * @param dataModel
	 *            the data model
	 * @param table
	 *            to read rows from. Opens SQL browser if table is <code>null</code>.
	 * @param condition
	 *            initial condition
	 * @param session
	 *            DB session
	 * @param parentRow
	 *            parent row
	 * @param parentRows
	 *            all parent rows, if there are more than 1
	 * @param association
	 *            {@link Association} with parent row
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	public BrowserContentPane(final DataModel dataModel, final Table table, String condition, Session session, List<Row> parentRows,
			final Association association, final Frame parentFrame, RowsClosure rowsClosure,
			Boolean selectDistinct, boolean reload, ExecutionContext executionContext) {
		this.table = table;
		this.session = session;
		this.dataModel = dataModel;
		this.rowIdSupport = new RowIdSupport(dataModel, session.dbms, executionContext);
		this.parentRows = parentRows;
		this.association = association;
		this.rowsClosure = rowsClosure;
		this.executionContext = executionContext;

		rowIdSupport.setUseRowIdsOnlyForTablesWithoutPK(true);

		suppressReload = true;

		if (table == null) {
			this.table = new SqlStatementTable(null, null, false);
		}

		initComponents();
		try {
			Icon errorIcon = UIManager.getIcon("OptionPane.errorIcon");
			errorLabel.setIcon(errorIcon);
			errorLabel.setText(null);
		} catch (Throwable t) {
			// ignore
		}

		loadingCauseLabel.setVisible(false);
		sortColumnsCheckBox.setVisible(false);

		findColumnsLabel.setText(null);
		findColumnsLabel.setToolTipText("Find Column...");
		findColumnsLabel.setIcon(StringSearchPanel.getSearchIcon(false, this));

		findColumnsLabel.addMouseListener(new java.awt.event.MouseAdapter() {
			private boolean in = false;

			@Override
			public void mousePressed(MouseEvent e) {
				if (findColumnsLabel.isEnabled()) {
					UIUtil.invokeLater(new Runnable() {
						@Override
						public void run() {
							in = false;
							updateBorder();
							if (findColumnsPanel.isShowing()) {
								Point point = new Point();
								SwingUtilities.convertPointToScreen(point, findColumnsPanel);
								findColumns((int) point.getX(), (int) point.getY(), currentRowsTableReference == null? rowsTable : currentRowsTableReference.get());
							}
						}
					});
				}
			}

			@Override
			public void mouseEntered(java.awt.event.MouseEvent evt) {
				if (findColumnsLabel.isEnabled()) {
					in = true;
					updateBorder();
				}
			}

			@Override
			public void mouseExited(java.awt.event.MouseEvent evt) {
				in = false;
				updateBorder();
			}

			private void updateBorder() {
				findColumnsPanel.setBackground(in? new Color(255, 255, 100) : null);
				findColumnsPanel.setOpaque(in);
			}
		});

		andCondition = new JComboBox2();
		andCondition.setEditable(true);
		GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel7.add(andCondition, gridBagConstraints);

		setPendingState(false, false);

		sqlLabel1.setIcon(dropDownIcon);
		sortColumnsLabel.setIcon(dropDownIcon);

		final ListCellRenderer acRenderer = andCondition.getRenderer();
		andCondition.setRenderer(new ListCellRenderer() {
			@Override
			public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected,
					boolean cellHasFocus) {
				Component render = acRenderer.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
				if (render instanceof JLabel) {
					if (value != null && value.toString().trim().length() > 0) {
						String tooltip = (value.toString());
						((JLabel) render).setToolTipText(UIUtil.toHTML(tooltip, 200));
					} else {
						((JLabel) render).setToolTipText(null);
					}
				}
				return render;
			}
		});
		if (andCondition.getEditor() != null && (andCondition.getEditor().getEditorComponent() instanceof JTextField)) {
			JTextField f = ((JTextField) andCondition.getEditor().getEditorComponent());
			f.getDocument().addDocumentListener(new DocumentListener() {
				@Override
				public void removeUpdate(DocumentEvent e) {
					updateTooltip();
				}
				@Override
				public void insertUpdate(DocumentEvent e) {
					updateTooltip();
				}
				@Override
				public void changedUpdate(DocumentEvent e) {
					updateTooltip();
				}
				private void updateTooltip() {
					if (andCondition.getEditor() != null && (andCondition.getEditor().getEditorComponent() instanceof JTextField)) {
						JTextField f = ((JTextField) andCondition.getEditor().getEditorComponent());
						String value = f.getText();
						String ttInfo = "<i>Ctrl+Space</i> for code completion.";
						if (value != null && value.toString().trim().length() > 0) {
							String tooltip = (value.toString());
							andCondition.setToolTipText("<html>" + UIUtil.toHTMLFragment(tooltip, 200) + "<hr>" + ttInfo);
						} else {
							andCondition.setToolTipText("<html>" + ttInfo);
						}
					}
				}
			});
		}

		andCondModel.addElement("");
		andCondition.setModel(andCondModel);

		andCondition.addItemListener(new ItemListener() {
			@Override
			public void itemStateChanged(ItemEvent e) {
				if (!suppessReloadOnAndConditionAction) {
					if (e.getStateChange() == ItemEvent.SELECTED) {
						historizeAndCondition(e.getItem());
						reloadRows();
					}
				}
				if (andCondition.getEditor() != null && (andCondition.getEditor().getEditorComponent() instanceof JTextField)) {
					JTextField f = ((JTextField) andCondition.getEditor().getEditorComponent());
					f.setCaretPosition(0);
				}
			}
		});

		initialRowHeight = rowsTable.getRowHeight();

		rowsTable = new JTable() {
			private int x[] = new int[2];
			private int y[] = new int[2];
			private Color color = new Color(0, 0, 200);

			@Override
			public void paint(Graphics graphics) {
				super.paint(graphics);
				if (!(graphics instanceof Graphics2D)) {
					return;
				}

				int maxI = Math.min(rowsTable.getRowCount(), rows.size());
				Rectangle visRect = rowsTable.getVisibleRect();

				RowSorter<? extends TableModel> sorter = getRowSorter();
				if (sorter != null) {
					maxI = sorter.getViewRowCount();
				}

				Graphics2D g2d = (Graphics2D) graphics;
				g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
				g2d.setStroke(new BasicStroke(1));

				int width = (int) (visRect.width * 1.4);

				for (int i = 0; i < maxI; ++i) {
					int mi = sorter == null? i : sorter.convertRowIndexToModel(i);
					if (mi >= rows.size()) {
						continue;
					}
					Row row = rows.get(mi);
					if (BrowserContentPane.this.rowsClosure.tempClosure.contains(row)) {
						int vi = i;
						g2d.setColor(new Color(255, 0, 0, 50));
						Rectangle r = rowsTable.getCellRect(vi, 0, false);
						x[0] = (int) visRect.getMinX();
						y[0] = (int) r.getMinY();
						r = rowsTable.getCellRect(vi, rowsTable.getColumnCount() - 1, false);
						x[1] = (int) visRect.getMinX() + width;
						y[1] = (int) r.getMaxY();
						GradientPaint paint = new GradientPaint(
								x[0], y[0], new Color(255, 0, 0, 60),
								x[0] + width, y[1], new Color(255, 0, 0, 10));
						g2d.setPaint(paint);
						g2d.fillRect(x[0], y[0], x[1] - x[0], y[1] - y[0]);
					}
				}

				g2d.setPaint(null);

				if (!useInheritedBlockNumbers && BrowserContentPane.this.association != null && BrowserContentPane.this.association.isInsertDestinationBeforeSource()) {
					return;
				}

				double lastPMIndex = -1;
				for (int i = 0; i < maxI; ++i) {
					int mi = sorter == null? i : sorter.convertRowIndexToModel(i);
					if (mi >= rows.size()) {
						continue;
					}
					Row row = rows.get(mi);
					double parentModelIndex = useInheritedBlockNumbers? row.getInheritedParentModelIndex() : row.getParentModelIndex();
					if (parentModelIndex != lastPMIndex) {
						lastPMIndex = parentModelIndex;
						int vi = i;
						g2d.setColor(color);
						Rectangle r = rowsTable.getCellRect(vi, 0, false);
						x[0] = (int) r.getMinX();
						y[0] = (int) r.getMinY();
						r = rowsTable.getCellRect(vi, rowsTable.getColumnCount() - 1, false);
						x[1] = (int) r.getMaxX();
						y[1] = (int) r.getMinY();
						g2d.drawPolyline(x, y, 2);
					}
				}
			}
		};

		InputMap im = rowsTable.getInputMap();
		Object key = "copyClipboard";
		im.put(KS_COPY_TO_CLIPBOARD, key);
		ActionMap am = rowsTable.getActionMap();
		Action a = new AbstractAction() {
			@Override
			public void actionPerformed(ActionEvent e) {
				UIUtil.copyToClipboard(rowsTable, true);
			}
		};
		am.put(key, a);

		registerAccelerator(KS_SQLCONSOLE, new AbstractAction() {
			@Override
			public void actionPerformed(ActionEvent e) {
				rowsTable.grabFocus();
				UIUtil.invokeLater(8, new Runnable() {
					@Override
					public void run() {
						openQueryBuilder(true);
					}
				});
			}
		});
		registerAccelerator(KS_QUERYBUILDER, new AbstractAction() {
			@Override
			public void actionPerformed(ActionEvent e) {
				rowsTable.grabFocus();
				UIUtil.invokeLater(new Runnable() {
					@Override
					public void run() {
						openQueryBuilder(false);
					}
				});
			}
		});
		registerAccelerator(KS_FILTER, new AbstractAction() {
			@Override
			public void actionPerformed(ActionEvent e) {
				isTableFilterEnabled = !isTableFilterEnabled;
				updateTableModel();
			}
		});
		registerAccelerator(KS_EDIT, new AbstractAction() {
			@Override
			public void actionPerformed(ActionEvent e) {
				setEditMode(!isEditMode);
				updateTableModel();
			}
		});
		registerAccelerator(KS_DETAILS, new AbstractAction() {
			@Override
			public void actionPerformed(ActionEvent e) {
				Point loc = new Point(18, 16);
				SwingUtilities.convertPointToScreen(loc, rowsTable);
				openDetails(loc.x, loc.y);
			}
		});

		rowsTable.setAutoCreateRowSorter(true);
		rowsTable.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
        rowsTableScrollPane.setViewportView(rowsTable);
		rowsTable.setAutoscrolls(false);

		setAndCondition((condition), true);
		from.setText(table == null? "" : this.dataModel.getDisplayName(table));
		adjustGui();
		rowsTable.setShowGrid(false);
		final TableCellRenderer defaultTableCellRenderer = rowsTable.getDefaultRenderer(String.class);

		rowsTable.setDefaultRenderer(Object.class, new TableCellRenderer() {

			final Color BG1 = new Color(255, 255, 255);
			final Color BG2 = new Color(242, 255, 242);
			final Color BG1_EM = new Color(255, 255, 236);
			final Color BG2_EM = new Color(230, 255, 236);
			final Color BG3 = new Color(192, 236, 255);
			final Color BG3_2 = new Color(184, 226, 255);
			final Color BG4 = new Color(32, 210, 255, 200);
			final Color BG4_2 = new Color(30, 196, 255, 200);
			final Color BG4_LIGHT = new Color(30, 200, 255, 60);
			final Color FG1 = new Color(155, 0, 0);
			final Color FG2 = new Color(0, 0, 255);
			final Font font = new JLabel().getFont();
			final Font nonbold = new Font(font.getName(), font.getStyle() & ~Font.BOLD, font.getSize());
			final Font italic = new Font(nonbold.getName(), nonbold.getStyle() | Font.ITALIC, nonbold.getSize());

			@Override
			public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, final int row, final int column) {
				boolean cellSelected = isSelected;

				if (BrowserContentPane.this.getQueryBuilderDialog() != null || // SQL Console
					table.getSelectedColumnCount() <= 1 && table.getSelectedRowCount() <= 1) {

					if (table == rowsTable) {
						cellSelected = false;
					}
				}

				isSelected = currentRowSelection == row || currentRowSelection == -2;

				if (table != rowsTable) {
					isSelected = false;
					if (table.getSelectedRows().length <= 1 && table.getSelectedColumns().length <= 1) {
						for (int sr: table.getSelectedRows()) {
							if (sr == column) {
								isSelected = true;
								break;
							}
						}
					}
				}

				Component render = defaultTableCellRenderer.getTableCellRendererComponent(rowsTable, value, isSelected, false, row, column);
				final RowSorter<?> rowSorter = rowsTable.getRowSorter();
				if (rowSorter.getViewRowCount() == 0 && table == rowsTable) {
					return render;
				}

				boolean renderRowAsPK = false;
				if (render instanceof JLabel) {
					Row r = null;
					int rowIndex = row;
					((JLabel) render).setIcon(null);
					if (row < rowSorter.getViewRowCount()) {
						rowIndex = rowSorter.convertRowIndexToModel(row);
						if (rowIndex < rows.size()) {
							r = rows.get(rowIndex);
							if (r != null) {
								renderRowAsPK = renderRowAsPK(r);
								Object cellContent = r.values.length > column? r.values[column] : null;
								if (cellContent instanceof UIUtil.IconWithText) {
									((JLabel) render).setIcon(((UIUtil.IconWithText) cellContent).icon);
									((JLabel) render).setText(((UIUtil.IconWithText) cellContent).text);
								}
							}
						}
					}
					((JLabel) render).setBorder(cellSelected? BorderFactory.createEtchedBorder() : null);
					int convertedColumnIndex = rowsTable.convertColumnIndexToModel(column);
					if (!isSelected && (table == rowsTable || !cellSelected)) {
						if (BrowserContentPane.this.getQueryBuilderDialog() != null && // SQL Console
							BrowserContentPane.this.rowsClosure.currentClosureRowIDs != null &&
							row < rows.size() &&
							row < rowSorter.getViewRowCount() &&
							rowSorter.convertRowIndexToModel(row) < rows.size() &&
							BrowserContentPane.this.rowsClosure.currentClosureRowIDs.contains(new Pair<BrowserContentPane, String>(BrowserContentPane.this, rows.get(rowSorter.convertRowIndexToModel(row)).nonEmptyRowId))) {
							((JLabel) render).setBackground((row % 2) == 0? BG3 : BG3_2);
							if (BrowserContentPane.this.rowsClosure.currentClosureRootID != null
									&& !BrowserContentPane.this.rowsClosure.currentClosureRootID.isEmpty()
									&& BrowserContentPane.this.rowsClosure.currentClosureRootPane == BrowserContentPane.this) {
								String rid = rows.get(rowSorter.convertRowIndexToModel(row)).nonEmptyRowId;
								if (!rid.isEmpty() && BrowserContentPane.this.rowsClosure.currentClosureRootID.contains(rid)) {
									((JLabel) render).setBackground(currentRowSelection >= 0? BG4_LIGHT : (row % 2 == 0? BG4 : BG4_2));
								}
							}
						} else {
							Table type = getResultSetTypeForColumn(convertedColumnIndex);
							if (isEditMode && r != null && (r.rowId != null && !r.rowId.isEmpty()) && browserContentCellEditor.isEditable(type, rowIndex, convertedColumnIndex, r.values[convertedColumnIndex])
									&& isPKComplete(type, r) && !rowIdSupport.getPrimaryKey(type, BrowserContentPane.this.session).getColumns().isEmpty()) {
								((JLabel) render).setBackground((row % 2 == 0) ? BG1_EM : BG2_EM);
							} else {
								((JLabel) render).setBackground((row % 2 == 0) ? BG1 : BG2);
							}
						}
					} else {
						((JLabel) render).setBackground(currentRowSelection == row? BG4.brighter() : (row % 2 == 0? BG4 : BG4_2));
					}
					((JLabel) render).setForeground(
							renderRowAsPK || pkColumns.contains(convertedColumnIndex) ? FG1 :
								fkColumns.contains(convertedColumnIndex) ? FG2 :
										Color.BLACK);
					boolean isNull = false;
					if (((JLabel) render).getText() == UIUtil.NULL || ((JLabel) render).getText() == UNKNOWN) {
						((JLabel) render).setForeground(Color.gray);
						((JLabel) render).setFont(italic);
						isNull = true;
					}
					TableColumnModel columnModel = rowsTable.getColumnModel();
					if (!foundColumn.isEmpty() && column < columnModel.getColumnCount() && foundColumn.contains(columnModel.getColumn(column).getModelIndex())) {
						Color background = render.getBackground();
						render.setBackground(
								new Color(
										Math.max((int)(background.getRed()), 0),
										Math.max((int)(background.getGreen() * 0.90), 0),
										Math.max((int)(background.getBlue() * 0.91), 0),
						 background.getAlpha()));
					}
					try {
						((JLabel) render).setToolTipText(null);
						if (isNull) {
							((JLabel) render).setFont(
									// highlightedRows.contains(rowSorter.convertRowIndexToModel(row)) ? italicBold :
									italic);
						} else {
							((JLabel) render).setFont(
									// highlightedRows.contains(rowSorter.convertRowIndexToModel(row)) ? bold :
									nonbold);
							String text = ((JLabel) render).getText();
							if (text.length() > 400 || text.indexOf('\n') >= 0) {
								String tip = tipCache.get(text);
								if (tip == null) {
									tip = UIUtil.toHTML(hardWrap(text), 200);
									tipCache.put(text, tip);
								}
								((JLabel) render).setToolTipText(tip);
							} else if (text.length() > 20) {
								((JLabel) render).setToolTipText(text);
							}
						}
					} catch (Exception e) {
						// ignore
					}
				}
				// indent 1. column
				if (render instanceof JLabel) {
					((JLabel) render).setText(" " + ((JLabel) render).getText());
				}
				return render;
			}

			Map<String, String> tipCache = new WeakHashMap<String, String>();
		});
		rowsTable.setRowSelectionAllowed(true);
		rowsTable.setColumnSelectionAllowed(true);
		rowsTable.setCellSelectionEnabled(true);
		rowsTable.setEnabled(true);

		rowsTableScrollPane.getVerticalScrollBar().setUnitIncrement(32);
		singleRowViewScrollPane.getVerticalScrollBar().setUnitIncrement(32);

		rowTableListener = new MouseListener() {
			private JPopupMenu lastMenu;

			@Override
			public void mouseReleased(MouseEvent e) {
			}

			@Override
			public void mousePressed(MouseEvent e) {
				int ri;
				JComponent source = (JComponent) e.getSource();
				if (source == rowsTable) {
					ri = rowsTable.rowAtPoint(e.getPoint());
				} else {
					ri = 0;
				}
				if (ri >= 0 && !rows.isEmpty() && rowsTable.getRowSorter().getViewRowCount() > 0) {
					int i = 0;
					if (source == rowsTable) {
						i = rowsTable.getRowSorter().convertRowIndexToModel(ri);
					} else if (source == rowsTableScrollPane || source == singleRowViewContainterPanel) {
						if (rows.size() != 1 || getQueryBuilderDialog() == null /* SQL Console */) {
							return;
						}
						ri = 0;
						i = 0;
					}
					Row row = rows.get(i);

					if (getQueryBuilderDialog() == null // SQL Console
							|| ((e.getButton() != MouseEvent.BUTTON1 || e.getClickCount() != 1) && (lastMenu == null || !lastMenu.isVisible()))) {
						currentRowSelection = ri;
						onRedraw();
						int x, y;
						if (source != rowsTable) {
							x = e.getX();
							y = e.getY();
						} else {
							Rectangle r = rowsTable.getCellRect(ri, 0, false);
							x = Math.max(e.getPoint().x, (int) r.getMinX());
							y = (int) r.getMaxY() - 2;
						}
						Point p = SwingUtilities.convertPoint(source, x, y, null);
						if (e.getButton() == MouseEvent.BUTTON1 && e.getClickCount() > 1) {
							openDetailsView(i, p.x + getOwner().getX(), p.y + getOwner().getY());
						} else if (e.getButton() != MouseEvent.BUTTON1) {
							JPopupMenu popup;
							popup = createPopupMenu(row, i, p.x + getOwner().getX(), p.y + getOwner().getY(), rows.size() == 1);
							if (popup != null) {
								if (!row.nonEmptyRowId.isEmpty()) {
									for (Row r: rows) {
										if (r.nonEmptyRowId.equals(row.nonEmptyRowId) && BrowserContentPane.this.rowsClosure.currentClosureRootID.contains(r.nonEmptyRowId)) {
											currentRowSelection = -1;
											onRedraw();
											break;
										}
									}
								}
								UIUtil.showPopup(source, x, y, popup);
								popup.addPropertyChangeListener("visible", new PropertyChangeListener() {

									@Override
									public void propertyChange(PropertyChangeEvent evt) {
										if (Boolean.FALSE.equals(evt.getNewValue())) {
											currentRowSelection = -1;
											onRedraw();
										}
									}
								});
							}
							lastMenu = popup;
						} else {
							setCurrentRowSelectionAndReloadChildrenIfLimitIsExceeded(ri, false);
						}
					} else if (e.getComponent() == singleRowViewScrollContentPanel || e.getSource() == singleRowViewContainterPanel) {
						if (getQueryBuilderDialog() != null) { // !SQL Console
							for (RowBrowser tb: getTableBrowser()) {
								try {
									tb.browserContentPane.isUpdatingTableModel = true;
									tb.browserContentPane.rowsTable.clearSelection();
								} finally {
									tb.browserContentPane.isUpdatingTableModel = false;
								}
							}
							setCurrentRowSelectionAndReloadChildrenIfLimitIsExceeded(0, false);
						}
					}
				}
			}

			@Override
			public void mouseExited(MouseEvent e) {
			}

			@Override
			public void mouseEntered(MouseEvent e) {
			}

			@Override
			public void mouseClicked(MouseEvent e) {
			}
		};

		rowsTable.addMouseListener(rowTableListener);
		rowsTableScrollPane.addMouseListener(rowTableListener);
		singleRowViewScrollPane.addMouseListener(rowTableListener);
		singleRowViewContainterPanel.addMouseListener(rowTableListener);

		TempClosureListener tempClosureListener = new TempClosureListener();
		rowsTable.addMouseListener(tempClosureListener);
		rowsTable.addMouseMotionListener(tempClosureListener);
		rowsTableScrollPane.addMouseListener(tempClosureListener);
		rowsTableScrollPane.addMouseMotionListener(tempClosureListener);
		singleRowViewContainterPanel.addMouseListener(tempClosureListener);
		singleRowViewContainterPanel.addMouseMotionListener(tempClosureListener);

		if (getQueryBuilderDialog() != null) { // !SQL Console
			rowsTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
				List<Integer> prevSelectedRows = null;
				Timer timer = null;
				boolean pending = false;
				@Override
				public void valueChanged(ListSelectionEvent e) {
					if (!pending) {
						pending = true;
						UIUtil.invokeLater(() -> {
							selectRows();
							pending = false;
						});
					}
				}
				private void selectRows() {
					List<Integer> selectedRows = new ArrayList<Integer>();
					if (isUpdatingTableModel) {
						prevSelectedRows = selectedRows;
						return;
					} else {
						for (RowBrowser tb: getTableBrowser()) {
							if (tb.browserContentPane != BrowserContentPane.this) {
								try {
									tb.browserContentPane.isUpdatingTableModel = true;
									tb.browserContentPane.rowsTable.clearSelection();
								} finally {
									tb.browserContentPane.isUpdatingTableModel = false;
								}
							}
						}
					}
					for (int si: rowsTable.getSelectedRows()) {
						selectedRows.add(si);
					}
					if (prevSelectedRows == null || !prevSelectedRows.equals(selectedRows)) {
						prevSelectedRows = selectedRows;
						resetCurrentRowSelection();
						for (int si: selectedRows) {
							setCurrentRowSelection(si, true);
						}
						setCurrentRowSelection(-1, true);
						startTimer();
					}
				}
				private void startTimer() {
			    	final Timer newTimer = new Timer(100, null);
			    	timer = newTimer;
			    	timer.addActionListener(new ActionListener() {
						@Override
						public void actionPerformed(ActionEvent e) {
							if (newTimer == timer) {
								reloadChildrenIfLimitIsExceeded();
							}
						}
					});
					timer.setRepeats(false);
					timer.start();
				}
			});
		}

		openEditorButton.setIcon(UIUtil.scaleIcon(this, conditionEditorIcon));
		openEditorButton.setText(null);

		final Runnable openConditionEditor = new Runnable() {
			@Override
			public void run() {
				openConditionEditor();
			}

			public void openConditionEditor() {
				openEditorButton.setSelected(true);
				final Point pos = new Point(andCondition.getX(), andCondition.getY());
				SwingUtilities.convertPointToScreen(pos, andCondition.getParent());
				Window owner = SwingUtilities.getWindowAncestor(BrowserContentPane.this);
				andConditionEditor.setLocationAndFit(pos, owner != null? owner.getX() + owner.getWidth() - 8: Integer.MAX_VALUE);
				andConditionEditor.edit(getAndConditionText(), "Table", "A", table, null, null, null, false, true);
			}
		};

		final Runnable createConditionEditor = new Runnable() {
			@Override
			public void run() {
				createConditionEditor();
			}

			public void createConditionEditor() {
				if (andConditionEditor == null) {
					andConditionEditor = new DBConditionEditor(parentFrame, dataModel) {
						@Override
						protected void consume(String cond) {
							if (cond != null) {
								if (!getAndConditionText().equals((cond))) {
									setAndCondition((cond), true);
									loadButton.grabFocus();
									reloadRows();
								}
							}
							openEditorButton.setSelected(false);
						}
					};
					if (andCondition.getEditor().getEditorComponent() instanceof JTextField) {
						andConditionEditor.observe((JTextField) andCondition.getEditor().getEditorComponent(), openConditionEditor);
					}
				}
			}
		};

		openEditorButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				loadButton.grabFocus();
				UIUtil.invokeLater(new Runnable() {
					@Override
					public void run() {
						createConditionEditor.run();
						openConditionEditor.run();
					}
				});
			}
		});
		if (andCondition.getEditor().getEditorComponent() instanceof JTextField) {
			DBConditionEditor.initialObserve((JTextField) andCondition.getEditor().getEditorComponent(), new Runnable() {
				@Override
				public void run() {
					createConditionEditor.run();
					andConditionEditor.doCompletion((JTextField) andCondition.getEditor().getEditorComponent(), openConditionEditor);
				}
			});
		}
		relatedRowsLabel.setIcon(UIUtil.scaleIcon(this, relatedRowsIcon));
		relatedRowsLabel.setFont(relatedRowsLabel.getFont().deriveFont(relatedRowsLabel.getFont().getSize() * 1.1f));
		if (createPopupMenu(null, -1, 0, 0, false).getComponentCount() == 0) {
			relatedRowsLabel.setEnabled(false);
		} else {
			relatedRowsPanel.addMouseListener(new java.awt.event.MouseAdapter() {
				private JPopupMenu popup;
				private boolean in = false;

				@Override
				public void mousePressed(MouseEvent e) {
					loadButton.grabFocus();
					UIUtil.invokeLater(new Runnable() {
						@Override
						public void run() {
							popup = createPopupMenu(null, -1, 0, 0, false);
							setCurrentRowSelectionAndReloadChildrenIfLimitIsExceeded(-2, false);
							popup.addPropertyChangeListener("visible", new PropertyChangeListener() {
								@Override
								public void propertyChange(PropertyChangeEvent evt) {
									if (Boolean.FALSE.equals(evt.getNewValue())) {
										popup = null;
										updateBorder();
										setCurrentRowSelectionAndReloadChildrenIfLimitIsExceeded(-1, false);
									}
								}
							});
							UIUtil.showPopup(relatedRowsPanel, 0, relatedRowsPanel.getHeight(), popup);
						}
					});
				}

				@Override
				public void mouseEntered(java.awt.event.MouseEvent evt) {
					in = true;
					updateBorder();
				}

				@Override
				public void mouseExited(java.awt.event.MouseEvent evt) {
					in = false;
					updateBorder();
				}

				private void updateBorder() {
					relatedRowsPanel.setBorder(new javax.swing.border.SoftBevelBorder((in || popup != null) ? javax.swing.border.BevelBorder.LOWERED
							: javax.swing.border.BevelBorder.RAISED));
				}
			});
		}
		sqlPanel.addMouseListener(new java.awt.event.MouseAdapter() {
			private JPopupMenu popup;
			private boolean in = false;

			@Override
			public void mousePressed(MouseEvent e) {
				loadButton.grabFocus();
				UIUtil.invokeLater(new Runnable() {
					@Override
					public void run() {
						Point loc = sqlPanel.getLocationOnScreen();
						popup = createSqlPopupMenu(0, (int) loc.getX(), (int) loc.getY(), false, BrowserContentPane.this);
						setCurrentRowSelectionAndReloadChildrenIfLimitIsExceeded(-2, false);
						popup.addPropertyChangeListener("visible", new PropertyChangeListener() {
							@Override
							public void propertyChange(PropertyChangeEvent evt) {
								if (Boolean.FALSE.equals(evt.getNewValue())) {
									popup = null;
									updateBorder();
									setCurrentRowSelectionAndReloadChildrenIfLimitIsExceeded(-1, false);
								}
							}
						});
						UIUtil.showPopup(sqlPanel, 0, sqlPanel.getHeight(), popup);
					}
				});
			}

			@Override
			public void mouseEntered(java.awt.event.MouseEvent evt) {
				in = true;
				updateBorder();
			}

			@Override
			public void mouseExited(java.awt.event.MouseEvent evt) {
				in = false;
				updateBorder();
			}

			private void updateBorder() {
				sqlPanel.setBorder(new javax.swing.border.SoftBevelBorder((in || popup != null) ? javax.swing.border.BevelBorder.LOWERED
						: javax.swing.border.BevelBorder.RAISED));
			}
		});
		if (selectDistinct != null) {
			selectDistinctCheckBox.setSelected(selectDistinct);
		}
		sortColumnsPanel.addMouseListener(new java.awt.event.MouseAdapter() {
			private JPopupMenu popup;
			private boolean in = false;

			@Override
			public void mousePressed(MouseEvent e) {
				loadButton.grabFocus();
				UIUtil.invokeLater(new Runnable() {
					@Override
					public void run() {
						popup = new JPopupMenu();
						JCheckBoxMenuItem natural = new JCheckBoxMenuItem("Natural column order ");
						natural.setSelected(!sortColumnsCheckBox.isSelected());
						natural.addActionListener(new ActionListener() {
							@Override
							public void actionPerformed(ActionEvent e) {
								sortColumnsCheckBox.setSelected(false);
								sortColumnsCheckBoxActionPerformed(null);
								sortColumnsLabel.setText("Natural column order");
							}
						});
						popup.add(natural);
						JCheckBoxMenuItem sorted = new JCheckBoxMenuItem("Alphabetical column order ");
						sorted.setSelected(sortColumnsCheckBox.isSelected());
						sorted.addActionListener(new ActionListener() {
							@Override
							public void actionPerformed(ActionEvent e) {
								sortColumnsCheckBox.setSelected(true);
								sortColumnsCheckBoxActionPerformed(null);
								sortColumnsLabel.setText("A-Z column order");
							}
						});
						popup.add(sorted);
						popup.addSeparator();
						JMenuItem changeOrder = new JMenuItem("Change natural column order ");
						changeOrder.addActionListener(new ActionListener() {
							@Override
							public void actionPerformed(ActionEvent e) {
								sortColumnsCheckBox.setSelected(false);
								sortColumnsCheckBoxActionPerformed(null);
								sortColumnsLabel.setText("Natural column order");
								UIUtil.invokeLater(new Runnable() {
									@Override
									public void run() {
										changeColumnOrder(table);
									}
								});
							}
						});
						popup.add(changeOrder);
						popup.addPropertyChangeListener("visible", new PropertyChangeListener() {
							@Override
							public void propertyChange(PropertyChangeEvent evt) {
								if (Boolean.FALSE.equals(evt.getNewValue())) {
									popup = null;
									updateBorder();
								}
							}
						});
						UIUtil.showPopup(sortColumnsPanel, 0, sortColumnsPanel.getHeight(), popup);
					}
				});
			}

			@Override
			public void mouseEntered(java.awt.event.MouseEvent evt) {
				in = true;
				updateBorder();
			}

			@Override
			public void mouseExited(java.awt.event.MouseEvent evt) {
				in = false;
				updateBorder();
			}

			private void updateBorder() {
				sortColumnsPanel.setBorder(new javax.swing.border.SoftBevelBorder((in || popup != null) ? javax.swing.border.BevelBorder.LOWERED
						: javax.swing.border.BevelBorder.RAISED));
			}
		});
		updateTableModel(0, false, false);

		suppressReload = false;
		if (reload) {
			reloadRows();
		}
	}

	protected String hardWrap(String text) {
		final int MAXLENTH = 400;
		if (text != null && text.length() > MAXLENTH) {
			if (text.length() > MAXLENTH * 5) {
				text = text.substring(0, MAXLENTH * 4 - 26) + "..." + text.substring(text.length() - 26);
			}
			StringBuilder sb = new StringBuilder();
			int r = 0;
			for (int i = 0; i < text.length(); ++i) {
				char charAt = text.charAt(i);
				sb.append(charAt);
				if ('\n' == charAt) {
					r = 0;
				} else {
					++r;
				}
				if (r > MAXLENTH) {
					sb.append("\n");
					r = 0;
				}
			}
			text = sb.toString();
		}
		return text;
	}

	private void registerAccelerator(KeyStroke ks, AbstractAction a) {
		registerAccelerator(ks, a, this);
		registerAccelerator(ks, a, rowsTable);
		registerAccelerator(ks, a, loadButton);
		registerAccelerator(ks, a, sqlLabel1);
		registerAccelerator(ks, a, relatedRowsLabel);
		registerAccelerator(ks, a, andCondition);
		Component editor = andCondition.getEditor().getEditorComponent();
		if (editor instanceof JComponent) {
			registerAccelerator(ks, a, (JComponent) editor);
		}
	}

	private void registerAccelerator(KeyStroke ks, AbstractAction a, JComponent comp) {
		InputMap im = comp.getInputMap();
		im.put(ks, a);
		ActionMap am = comp.getActionMap();
		am.put(a, a);
	}

	protected abstract boolean renderRowAsPK(Row theRow);

	boolean isPending = false;

	void setPendingState(boolean pending, boolean propagate) {
		isPending = pending;
		((CardLayout) pendingNonpendingPanel.getLayout()).show(pendingNonpendingPanel, "nonpending");
		if (pending) {
			updateMode("pending", null);
		}
		if (propagate) {
			for (RowBrowser child: getChildBrowsers()) {
				child.browserContentPane.setPendingState(pending, propagate);
			}
		}
	}

	protected void historizeAndCondition(Object item) {
		if (item == null || (item.toString().trim().isEmpty() && !item.toString().isEmpty())) {
			return;
		}
		for (int i = 0; i < andCondModel.getSize(); ++i) {
			if (item.equals(andCondModel.getElementAt(i))) {
				return;
			}
		}
		andCondModel.insertElementAt(item, 1);
	}

	private boolean suppessReloadOnAndConditionAction = false;

	void setAndCondition(String cond, boolean historize) {
		try {
			suppessReloadOnAndConditionAction = true;
			andCondition.setSelectedItem(cond);
			if (historize) {
				historizeAndCondition(cond);
			}
		} finally {
			suppessReloadOnAndConditionAction = false;
		}
	}

	String getAndConditionText() {
		if (andCondition.getEditor() != null && (andCondition.getEditor().getEditorComponent() instanceof JTextField)) {
			JTextField f = ((JTextField) andCondition.getEditor().getEditorComponent());
			return f.getText();
		}
		Object sel = andCondition.getSelectedItem();
		if (sel == null) {
			return "";
		}
		return sel.toString().trim();
	}

	private void adjustGui() {
		if (this.association == null) {
			joinPanel.setVisible(false);
			onPanel.setVisible(false);

			jLabel1.setText(" ");
			jLabel4.setText(" ");

			jLabel6.setVisible(false);
			java.awt.GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.gridx = 8;
			gridBagConstraints.gridy = 4;
			gridBagConstraints.gridheight = 1;
			gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHEAST;
		} else {
			join.setText(this.dataModel.getDisplayName(this.association.source));
			on.setText(!this.association.reversed ? SqlUtil.reversRestrictionCondition(this.association.getUnrestrictedJoinCondition()) : this.association
					.getUnrestrictedJoinCondition());
			updateWhereField();
			join.setToolTipText(join.getText());
			on.setToolTipText(assocToolTip(on.getText(), this.association));
		}
	}

	private static String assocToolTip(String condition, Association assoc) {
		return "<html>" + UIUtil.toHTMLFragment(condition, 0) + "<br><hr>FK:&nbsp;<i>"
				+ UIUtil.toHTML(assoc.reversed? assoc.reversalAssociation.getName() : assoc.getName(), 0) + "</i></html>";
	}

	private class AllNonEmptyItem extends JMenuItem {
		int todo;
		int done;
		private List<ActionListener> todoList = new ArrayList<ActionListener>();
		private final String initText = "Counting rows... ";

		AllNonEmptyItem() {
			setEnabled(false);
			addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					Component parent = SwingUtilities.getWindowAncestor(BrowserContentPane.this);
					if (parent == null) {
						parent = BrowserContentPane.this;
					}
					UIUtil.setWaitCursor(parent);
					try {
						Desktop.noArrangeLayoutOnNewTableBrowser = true;
						Desktop.noArrangeLayoutOnNewTableBrowserWithAnchor = todoList.size() > 1;
						Desktop.resetLastArrangeLayoutOnNewTableBrowser();
						for (int i = 0; i < todoList.size(); ++i) {
							if (i == todoList.size() - 1) {
								Desktop.noArrangeLayoutOnNewTableBrowser = false;
							}
							todoList.get(i).actionPerformed(e);
						}
					} finally {
						Desktop.noArrangeLayoutOnNewTableBrowser = false;
						Desktop.noArrangeLayoutOnNewTableBrowserWithAnchor = false;
						UIUtil.resetWaitCursor(parent);
					}
				}
			});
		}

		public void rowsCounted(long count, ActionListener itemAction) {
			++done;
			if (count != 0) {
				todoList.add(itemAction);
			}
			int p = todo > 0? (100 * done) / todo : 0;
			if (done < todo) {
				setText(initText + p + "%");
			} else {
				setText("All non-empty (" + todoList.size() + ")");
				setEnabled(true);
			}
		}

		public void setInitialText() {
			if (todoList.isEmpty()) {
				setText("All non-empty (0)");
			} else {
				setText(initText + "100%");
			}
		}

	}

	private String currentSelectedRowCondition = "";

	/**
	 * Creates popup menu for navigation.
	 * @param navigateFromAllRows
	 * @param copyTCB
	 * @param runnable
	 * @param runnable
	 */
	public JPopupMenu createPopupMenu(final Row row, final int rowIndex, final int x, final int y, boolean navigateFromAllRows) {
		return createPopupMenu(row, rowIndex, x, y, navigateFromAllRows, null, null);
	}

	/**
	 * Creates popup menu for navigation.
	 * @param navigateFromAllRows
	 * @param runnable
	 */
	public JPopupMenu createPopupMenu(final Row row, final int rowIndex, final int x, final int y, boolean navigateFromAllRows, JMenuItem altCopyTCB, final Runnable repaint) {
		return createPopupMenu(rowsTable, row, rowIndex, x, y, navigateFromAllRows, altCopyTCB, repaint, true);
	}

	/**
	 * Creates popup menu for navigation.
	 * @param navigateFromAllRows
	 * @param runnable
	 */
	public JPopupMenu createPopupMenu(final JTable contextJTable, final Row row, final int rowIndex, final int x, final int y, boolean navigateFromAllRows, JMenuItem altCopyTCB, final Runnable repaint, final boolean withKeyStroke) {
		JMenuItem tableFilter = new JCheckBoxMenuItem("Table Filter");
		if (withKeyStroke) {
			tableFilter.setAccelerator(KS_FILTER);
		} else {
			tableFilter.setVisible(false);
		}
		tableFilter.setSelected(isTableFilterEnabled);
		if (isLimitExceeded) {
			tableFilter.setForeground(Color.red);
			tableFilter.setToolTipText("Row limit exceeded. Filtering may be incomplete.");
		}
		tableFilter.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				Component parent = null;
				Boolean oldForceAdjustRows = Desktop.forceAdjustRows;
				try {
					Desktop.forceAdjustRows = null;
					parent = SwingUtilities.getWindowAncestor(contextJTable);
					UIUtil.setWaitCursor(parent);
					isTableFilterEnabled = !isTableFilterEnabled;
					updateTableModel();
				} finally {
					UIUtil.resetWaitCursor(parent);
					Desktop.forceAdjustRows = oldForceAdjustRows;
				}
			}
		});
		JMenuItem copyTCB;
		if (altCopyTCB != null) {
			copyTCB = altCopyTCB;
		} else {
			copyTCB = null;
//			if (getQueryBuilderDialog() != null) { // !SQL Console
//				copyTCB = null;
//			} else {
//				copyTCB = new JMenuItem("Copy to Clipboard");
//				if (withKeyStroke) {
//					copyTCB.setAccelerator(KS_COPY_TO_CLIPBOARD);
//				}
//				copyTCB.setEnabled(rowsTable.getSelectedColumnCount() > 0);
//				copyTCB.addActionListener(new ActionListener() {
//					@Override
//					public void actionPerformed(ActionEvent e) {
//						UIUtil.copyToClipboard(rowsTable, true);
//					}
//				});
//			}
		}

		if (table instanceof SqlStatementTable && resultSetType == null) {
			JPopupMenu jPopupMenu = new JPopupMenu();
			if (row != null) {
				JMenuItem det = new JMenuItem("Details");
				if (withKeyStroke) {
					det.setAccelerator(KS_DETAILS);
				}
				jPopupMenu.add(det);
				jPopupMenu.add(new JSeparator());
				det.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						openDetailsView(rowIndex, x, y);
					}
				});
			}
			JMenu script = new JMenu("Create SQL");
			script.setEnabled(false);
			jPopupMenu.add(script);
			jPopupMenu.addSeparator();
			if (copyTCB != null) {
				jPopupMenu.add(copyTCB);
				jPopupMenu.addSeparator();
			}
			jPopupMenu.add(tableFilter);
			JMenuItem editMode = new JMenuItem("Edit Mode");
			if (withKeyStroke) {
				editMode.setAccelerator(KS_EDIT);
			}
			jPopupMenu.add(editMode);
			editMode.setEnabled(false);
			return jPopupMenu;
		}

		List<String> assList = new ArrayList<String>();
		Map<String, Association> assMap = new HashMap<String, Association>();
		for (Association a : table.associations) {
			int n = 0;
			for (Association a2 : table.associations) {
				if (a.destination == a2.destination) {
					++n;
				}
			}
			char c = ' ';
			if (a.isIgnored()) {
				c = '4';
			} else if (a.isInsertDestinationBeforeSource()) {
				c = '1';
			} else if (a.isInsertSourceBeforeDestination()) {
				c = '2';
			} else {
				c = '3';
			}
			String name = c + a.getDataModel().getDisplayName(a.destination) + (n > 1 ? " (" + (a.reversed? a.reversalAssociation.getName() : a.getName()) + ")": "");
			assList.add(name);
			assMap.put(name, a);
		}
		Collections.sort(assList);

		final Object context = new Object();
		AllNonEmptyItem allNonEmpty = new AllNonEmptyItem();

		JPopupMenu popup = new JPopupMenu();
		popup = createNavigationMenu(popup, row, rowIndex, assList, assMap, "Parents", "1", navigateFromAllRows, 80, allNonEmpty, context);
		popup = createNavigationMenu(popup, row, rowIndex, assList, assMap, "Children", "2", navigateFromAllRows, 60, allNonEmpty, context);
		popup = createNavigationMenu(popup, row, rowIndex, assList, assMap, "Associated Rows", "3", navigateFromAllRows, 40, allNonEmpty, context);
		popup = createNavigationMenu(popup, row, rowIndex, assList, assMap, "Detached Rows", "4", navigateFromAllRows, 40, allNonEmpty, context);

		popup.addPopupMenuListener(new PopupMenuListener() {

			@Override
			public void popupMenuWillBecomeVisible(PopupMenuEvent e) {
			}

			@Override
			public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
				cancel();
			}

			@Override
			public void popupMenuCanceled(PopupMenuEvent e) {
				cancel();
			}

			private void cancel() {
				CancellationHandler.cancelSilently(context);
				getRunnableQueue().add(new RunnableWithPriority() {

					@Override
					public void run() {
						CancellationHandler.reset(context);
					}

					@Override
					public int getPriority() {
						return 0;
					}
				});
			}
		});

		if (!isPending && !rows.isEmpty()) {
			popup.add(allNonEmpty);
			allNonEmpty.setInitialText();
		}

		if (row != null) {
			JMenuItem det = new JMenuItem("Details");
			if (withKeyStroke) {
				det.setAccelerator(KS_DETAILS);
			}
			popup.insert(det, 0);
			popup.insert(new JSeparator(), 1);
			det.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					openDetailsView(rowIndex, x, y);
				}
			});

			if (!(table instanceof SqlStatementTable) || resultSetType != null) {
				if (popup.getComponentCount() > 0) {
					popup.add(new JSeparator());
				}

				JMenuItem qb = new JMenuItem("Query Builder");
				qb.setAccelerator(KS_QUERYBUILDER);
				popup.add(qb);
				qb.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						openQueryBuilder(false, SqlUtil.replaceAliases(row.rowId, "A", "A"));
					}
				});

				JMenuItem sqlConsole = new JMenuItem("SQL Console");
				sqlConsole.setAccelerator(KS_SQLCONSOLE);
				popup.add(sqlConsole);
				sqlConsole.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						openQueryBuilder(true, SqlUtil.replaceAliases(row.rowId, "A", "A"));
					}
				});

				final List<Row> toSelect = new ArrayList<Row>();
				if (!row.nonEmptyRowId.isEmpty()) {
					for (Row r: rows) {
						if (!r.nonEmptyRowId.isEmpty() && rowsClosure.currentClosureRootID.contains(r.nonEmptyRowId)) {
							toSelect.add(r);
						}
					}
				}
				if (!toSelect.contains(row)) {
					toSelect.clear();
					toSelect.add(row);
				}
				if (!currentSelectedRowCondition.equals("") &&
						(rows.size() == 1 || toSelect.size() == rows.size())) {
					JMenuItem sr = new JMenuItem(rows.size() == 1? "Deselect Row" : ("Deselect Rows (" + rows.size() + ")"));
					popup.insert(sr, 0);
					sr.addActionListener(new ActionListener() {
						@Override
						public void actionPerformed(ActionEvent e) {
							andCondition.setSelectedItem("");
						}
					});
				} else {
					JMenuItem sr = new JMenuItem(toSelect.size() <= 1? "Select Row" : ("Select Rows (" + toSelect.size() + ")"));
					sr.setEnabled(rows.size() > 1 && !row.rowId.isEmpty());
					popup.insert(sr, 0);
					sr.addActionListener(new ActionListener() {
						@Override
						public void actionPerformed(ActionEvent e) {
							selectRow(toSelect);
						}
					});
				}

				JMenu sql = new JMenu("Create SQL");
				final String rowName = !(table instanceof SqlStatementTable)? dataModel.getDisplayName(table) + "(" + SqlUtil.replaceAliases(row.rowId, null, null) + ")" : "";
				JMenuItem update = new JMenuItem("Update");
				sql.add(update);
				update.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						openSQLDialog("Update Row " + rowName, x, y, SQLDMLBuilder.buildUpdate(table, toSelect, true, session));
					}
				});
				JMenuItem insert = new JMenuItem("Insert");
				sql.add(insert);
				insert.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						openSQLDialog("Insert Row " + rowName, x, y, SQLDMLBuilder.buildInsert(table, toSelect, true, session));
					}
				});
				JMenuItem insertNewRow = createInsertChildMenu(toSelect, x, y);
				sql.add(insertNewRow);
				JMenuItem delete = new JMenuItem("Delete");
				sql.add(delete);
				delete.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						openSQLDialog("Delete Row " + rowName, x, y, SQLDMLBuilder.buildDelete(table, toSelect, true, session));
					}
				});
				insert.setEnabled(resultSetType == null);
				update.setEnabled(resultSetType == null);
				delete.setEnabled(resultSetType == null);
				if (getQueryBuilderDialog() == null) {
					popup.removeAll();
					popup.add(det);
					popup.addSeparator();
					JMenu script = new JMenu("Create SQL");
					popup.add(script);
					if (table.getName() == null) {
						script.setEnabled(false);
					} else {
						final String tableName = dataModel.getDisplayName(table);
						script.add(update);
						script.add(insert);
						script.add(insertNewRow);
						script.add(delete);
						script.addSeparator();
						JMenuItem updates = new JMenuItem("Updates (all rows)");
						script.add(updates);
						updates.addActionListener(new ActionListener() {
							@Override
							public void actionPerformed(ActionEvent e) {
								openSQLDialog("Update " + tableName, x, y,  new Object() { @Override
								public String toString() { return SQLDMLBuilder.buildUpdate(table, sortedAndFiltered(rows), false, session); }});
							}
						});
						JMenuItem inserts = new JMenuItem("Inserts (all rows)");
						script.add(inserts);
						inserts.addActionListener(new ActionListener() {
							@Override
							public void actionPerformed(ActionEvent e) {
								openSQLDialog("Insert Into " + tableName, x, y, new Object() { @Override
								public String toString() { return SQLDMLBuilder.buildInsert(table, sortedAndFiltered(rows), false, session); }});
							}
						});
						JMenuItem deletes = new JMenuItem("Deletes (all rows)");
						script.add(deletes);
						deletes.addActionListener(new ActionListener() {
							@Override
							public void actionPerformed(ActionEvent e) {
								openSQLDialog("Delete from " + tableName, x, y,  new Object() { @Override
								public String toString() { return SQLDMLBuilder.buildDelete(table, sortedAndFiltered(rows), false, session); }
								});
							}
						});
						boolean hasPK = !rowIdSupport.getPrimaryKey(table).getColumns().isEmpty();
						if (!hasPK) {
							delete.setEnabled(false);
						}
						inserts.setEnabled(rows.size() > 0);
						updates.setEnabled(hasPK && rows.size() > 0);
						deletes.setEnabled(hasPK && rows.size() > 0);
					}
				} else {
					popup.add(sql);
				}
			}
			popup.addSeparator();
			if (copyTCB != null) {
				popup.add(copyTCB);
				popup.addSeparator();
			}
			popup.add(createFindColumnMenuItem(x, y, contextJTable));
			popup.add(new JSeparator());
			popup.add(tableFilter);
			JCheckBoxMenuItem editMode = new JCheckBoxMenuItem("Edit Mode");
			editMode.setEnabled(isTableEditable(table));
			if (withKeyStroke) {
				editMode.setAccelerator(KS_EDIT);
			}
			editMode.setSelected(isEditMode);
			editMode.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					Boolean oldForceAdjustRows = Desktop.forceAdjustRows;
					try {
						Desktop.forceAdjustRows = null;
						setEditMode(!isEditMode);
						updateTableModel();
						if (repaint != null) {
							UIUtil.invokeLater(repaint);
						}
					} finally {
						Desktop.forceAdjustRows = oldForceAdjustRows;
					}
				}
			});
			popup.add(editMode);
		}

		return popup;
	}

	private JMenuItem createFindColumnMenuItem(final int x, final int y, final JTable contextJTable) {
		final JMenuItem menuItem = new JMenuItem("Find Column...");
		menuItem.setEnabled(singleRowDetailsView == null);
		menuItem.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				findColumns(x, y, contextJTable);
			}
		});
		return menuItem;
	}

	/**
	 * Creates popup menu for SQL.
	 * @param forNavTree
	 * @param browserContentPane
	 */
	public JPopupMenu createSqlPopupMenu(final int rowIndex, final int x, final int y, boolean forNavTree, final Component parentComponent) {
		JPopupMenu popup = new JPopupMenu();

		JMenuItem rebase = new JMenuItem("Start Navigation here");
		popup.add(rebase);
		rebase.setEnabled(getParentBrowser() != null);
		rebase.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				rebase();
			}
		});
		popup.addSeparator();
		JMenuItem qb = new JMenuItem("Query Builder");
		qb.setAccelerator(KS_QUERYBUILDER);
		popup.add(qb);
		qb.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				openQueryBuilder(false);
			}
		});
		JMenuItem sqlConsole = new JMenuItem("SQL Console");
		sqlConsole.setAccelerator(KS_SQLCONSOLE);
		popup.add(sqlConsole);
		sqlConsole.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				openQueryBuilder(true);
			}
		});
		JMenu sqlDml = new JMenu("Create SQL");
		popup.add(sqlDml);
		final String tableName = dataModel.getDisplayName(table);
		JMenuItem update = new JMenuItem("Updates");
		sqlDml.add(update);
		update.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				openSQLDialog("Update " + tableName, x, y,  new Object() { @Override
				public String toString() { return SQLDMLBuilder.buildUpdate(table, sortedAndFiltered(rows), false, session); }});
			}
		});
		JMenuItem insert = new JMenuItem("Inserts");
		sqlDml.add(insert);
		insert.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				openSQLDialog("Insert Into " + tableName, x, y, new Object() { @Override
				public String toString() { return SQLDMLBuilder.buildInsert(table, sortedAndFiltered(rows), false, session); }});
			}
		});
		JMenuItem insertNewRow = createInsertChildMenu(sortedAndFiltered(rows), x, y);
		sqlDml.add(insertNewRow);
		JMenuItem delete = new JMenuItem("Deletes");
		sqlDml.add(delete);
		delete.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				openSQLDialog("Delete from " + tableName, x, y,  new Object() { @Override
				public String toString() { return SQLDMLBuilder.buildDelete(table, sortedAndFiltered(rows), false, session); }});
			}
		});
		boolean hasPK = !rowIdSupport.getPrimaryKey(table).getColumns().isEmpty();
		insert.setEnabled(rows.size() > 0);
		update.setEnabled(hasPK && rows.size() > 0);
		delete.setEnabled(hasPK && rows.size() > 0);

		popup.add(new JSeparator());
		JMenuItem exportData = new JMenuItem("Export Data");
		popup.add(exportData);
		exportData.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				openExtractionModelEditor(true);
			}
		});

		JMenuItem extractionModel = new JMenuItem("Create Extraction Model");
		popup.add(extractionModel);
		extractionModel.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				openExtractionModelEditor(false);
			}
		});

		popup.add(new JSeparator());

		if (!forNavTree) {
			JMenuItem det = new JMenuItem("Details");
			det.setAccelerator(KS_DETAILS);
			popup.add(det);
			det.setEnabled(rows.size() > 0);
			det.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					openDetails(x, y);
				}
			});
		}
//		popup.add(new JSeparator());
		JMenuItem snw = new JMenuItem("Show in New Window");
		popup.add(snw);
		snw.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				showInNewWindow();
			}
		});

//		JMenuItem al = new JMenuItem("Append Layout...");
//		popup.add(al);
//		al.addActionListener(new ActionListener() {
//			@Override
//			public void actionPerformed(ActionEvent e) {
//				appendLayout();
//			}
//		});
//
//		popup.addSeparator();

		JMenuItem m = new JMenuItem("Hide (Minimize)");
		popup.add(m);
		m.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				onHide();
			}
		});
		m = new JMenuItem("Close");
		popup.add(m);
		m.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				closeWithChildren(parentComponent);
			}
		});
		popup.add(new JSeparator());
		if (!forNavTree) {
			popup.add(createFindColumnMenuItem(x, y, rowsTable));
			popup.add(new JSeparator());
		}
		JMenuItem tableFilter = new JCheckBoxMenuItem("Table Filter");
		tableFilter.setAccelerator(KS_FILTER);
		tableFilter.setSelected(isTableFilterEnabled);
		if (isLimitExceeded) {
			tableFilter.setForeground(Color.red);
			tableFilter.setToolTipText("Row limit exceeded. Filtering may be incomplete.");
		}
		tableFilter.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				Component parent = null;
				Boolean oldForceAdjustRows = Desktop.forceAdjustRows;
				try {
					Desktop.forceAdjustRows = null;
					parent = SwingUtilities.getWindowAncestor(parentComponent);
					UIUtil.setWaitCursor(parent);
					isTableFilterEnabled = !isTableFilterEnabled;
					updateTableModel();
				} finally {
					UIUtil.resetWaitCursor(parent);
					Desktop.forceAdjustRows = oldForceAdjustRows;
				}
			}
		});
		popup.add(tableFilter);
		JCheckBoxMenuItem editMode = new JCheckBoxMenuItem("Edit Mode");
		editMode.setEnabled(isTableEditable(table));
		editMode.setAccelerator(KS_EDIT);
		editMode.setSelected(isEditMode);
		editMode.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				Boolean oldForceAdjustRows = Desktop.forceAdjustRows;
				try {
					Desktop.forceAdjustRows = null;
					setEditMode(!isEditMode);
					updateTableModel();
				} finally {
					Desktop.forceAdjustRows = oldForceAdjustRows;
				}
			}
		});
		popup.add(editMode);
		return popup;
	}

	protected boolean isTableEditable(Table theTable) {
		return resultSetType != null || !rowIdSupport.getPrimaryKey(theTable).getColumns().isEmpty();
	}

	public boolean closeWithChildren(Component parentComponent) {
		int count = countSubNodes(this);
		Component parent = SwingUtilities.getWindowAncestor(this);
		if (parent == null) {
			parent = BrowserContentPane.this;
		}
		boolean closeThisToo = true;
		if (count > 1) {
			int o = JOptionPane.showOptionDialog(parentComponent, "Which tables do you want to close?", "Close",
					JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE, null,
					new Object[] {
							"Only this table",
							"This and related tables (" + (count) + ")",
							"Related tables (" + (count - 1) + ")"
					},
					null);
			if (o == 0) {
				UIUtil.setWaitCursor(parent);
				try {
					close();
				} finally {
					UIUtil.resetWaitCursor(parent);
				}
				return true;
			}
			if (o == 1) {
				closeThisToo = true;
			} else if (o == 2) {
				closeThisToo = false;
			} else {
				return false;
			}
		}
		UIUtil.setWaitCursor(parent);
		try {
			closeSubTree(BrowserContentPane.this, closeThisToo);
		} finally {
			UIUtil.resetWaitCursor(parent);
		}
		return true;
	}

	private int countSubNodes(BrowserContentPane cp) {
		int count = 0;
		for (RowBrowser c: cp.getChildBrowsers()) {
			count += countSubNodes(c.browserContentPane);
		}
		return count + 1;
	}

	protected void closeSubTree(BrowserContentPane cp, boolean closeThisToo) {
		for (RowBrowser c: cp.getChildBrowsers()) {
			closeSubTree(c.browserContentPane, true);
		}
		if (closeThisToo) {
			cp.close();
		}
	}

	private JMenu createInsertChildMenu(List<Row> parents, final int x, final int y) {
		JScrollMenu insertNewRow = new JScrollMenu("Insert Child");
		if (parents == null || parents.isEmpty() || parents.size() > 1 || table == null || table.getName() == null) {
			insertNewRow.setEnabled(false);
			return insertNewRow;
		}
		List<Association> tableAssociations = table.associations;

		if (tableAssociations == null || tableAssociations.isEmpty()) {
			// artificial table from SQL Console?
			Table t = dataModel.getTable(table.getName());
			if (t == null) {
				insertNewRow.setEnabled(false);
				return insertNewRow;
			}
			tableAssociations = t.associations;
		}

		List<Association> assocs = new ArrayList<Association>();
		Map<Association, List<Row>> children = new HashMap<Association, List<Row>>();

		if (tableAssociations != null) {
			for (Association association: tableAssociations) {
				List<Row> child = createNewRow(parents, table, association);
				if (child != null && !child.isEmpty()) {
					assocs.add(association);
					children.put(association, child);
				}
			}
		}
		Collections.sort(assocs, new Comparator<Association>() {
			@Override
			public int compare(Association a, Association b) {
				String dNameA = dataModel.getDisplayName(a.destination);
				String dNameB = dataModel.getDisplayName(b.destination);
				return dNameA.compareTo(dNameB);
			}
		});
		for (int i = 0; i < assocs.size(); ++i) {
			final Association association = assocs.get(i);
			Association pred = i > 0? assocs.get(i - 1) : null;
			Association succ = i < assocs.size() - 1? assocs.get(i + 1) : null;
			final String dName = dataModel.getDisplayName(association.destination);
			JMenuItem item;
			if (pred != null && pred.destination == association.destination || succ != null && succ.destination == association.destination) {
				item = new JMenuItem(dName + " on " + association.getName());
			} else {
				item = new JMenuItem(dName);
			}
			final List<Row> child = children.get(association);
			item.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					openSQLDialog("Insert Child into " + dName, x, y, SQLDMLBuilder.buildInsert(association.destination, child, true, session));
				}
			});
			insertNewRow.add(item);
		}

		insertNewRow.setEnabled(!assocs.isEmpty());
		return insertNewRow;
	}

	void openExtractionModelEditor(boolean doExport) {
		Component parent = SwingUtilities.getWindowAncestor(this);
		if (parent == null) {
			parent = this;
		}
		UIUtil.setWaitCursor(parent);
		try {
			String file;
			String ts = new SimpleDateFormat("HH-mm-ss-SSS", Locale.ENGLISH).format(new Date());

			Table stable = table;
			String subjectCondition;

			QueryBuilderDialog.Relationship root = createQBRelations(false);
			Collection<Association> restrictedAssociations = new HashSet<Association>();
			Collection<Association> restrictedDependencies = new HashSet<Association>();
			Collection<RestrictionDefinition> restrictedDependencyDefinitions = new HashSet<RestrictionDefinition>();
			List<RestrictionDefinition> restrictionDefinitions = createRestrictions(root, stable, restrictedAssociations, restrictedDependencies, restrictedDependencyDefinitions);

//			if (!restrictedDependencies.isEmpty()) {
				Set<String> parents = new TreeSet<String>();
				for (Association association: restrictedDependencies) {
					parents.add(dataModel.getDisplayName(association.destination));
				}
//				String pList = "";
//				int i = 0;
//				for (String p: parents) {
//					pList += p + "\n";
//					if (++i > 20) {
//						break;
//					}
//				}

				final SbEDialog sbEDialog = new SbEDialog(SwingUtilities.getWindowAncestor(this),
						(doExport? "Export rows and related rows from \"" : "Create Extraction Model for Subject \"") + dataModel.getDisplayName(stable) + "\".", (parents.isEmpty()? "" : ("\n\n" + parents.size() + " disregarded parent tables.")));
				if (doExport) {
					sbEDialog.setTitle("Export Data");
				}
				sbEDialog.regardButton.setVisible(!parents.isEmpty());
				sbEDialog.dispose();
				DefaultTreeCellRenderer renderer = new DefaultTreeCellRenderer() {
					@Override
					public Component getTreeCellRendererComponent(JTree tree, Object value, boolean sel, boolean expanded, boolean leaf, int row,
							boolean hasFocus) {
						Component result = super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);
						if (value instanceof DefaultMutableTreeNode) {
							if (!(((DefaultMutableTreeNode) value).getUserObject() instanceof String)) {
								if (result instanceof JLabel) {
									((JLabel) result).setForeground(Color.red);
								} else {
									((JLabel) result).setForeground(Color.black);
								}
							}
						}
						return result;
					}

				};
				renderer.setOpenIcon(null);
				renderer.setLeafIcon(null);
				renderer.setClosedIcon(null);
				sbEDialog.browserTree.setCellRenderer(renderer);
				int[] count = new int[] { 0 };
				DefaultTreeModel treeModel = new DefaultTreeModel(addChildNodes(this, restrictedDependencies, count));
				sbEDialog.browserTree.setModel(treeModel);
				for (int i = 0; i < count[0]; ++i) {
					sbEDialog.browserTree.expandRow(i);
				}
				sbEDialog.browserTree.scrollRowToVisible(0);
				sbEDialog.browserTree.getSelectionModel().addTreeSelectionListener(new TreeSelectionListener() {
					@Override
					public void valueChanged(TreeSelectionEvent e) {
						sbEDialog.browserTree.getSelectionModel().clearSelection();
					}
				});
				sbEDialog.grabFocus();
				sbEDialog.setVisible(true);

				if (!sbEDialog.ok) {
					return;
				}
				if (sbEDialog.regardButton.isSelected()) {
					restrictionDefinitions.removeAll(restrictedDependencyDefinitions);
					for (Association a: restrictedDependencies) {
						disableDisregardedNonParentsOfDestination(a, restrictedAssociations, restrictionDefinitions);
					}
				}

//				int option = JOptionPane.showOptionDialog(parent, "Disregarded parent tables:\n\n" + pList + "\n", "Disregarded parent tables", JOptionPane.YES_NO_OPTION, JOptionPane.INFORMATION_MESSAGE, null, new Object[] { "regard parent tables", "Ok" }, "regard parent tables");
//				switch (option) {
//				case 0:
//					restrictionDefinitions.removeAll(restrictedDependencyDefinitions);
//					for (Association a: restrictedDependencies) {
//						disableDisregardedNonParentsOfDestination(a, restrictedAssociations, restrictionDefinitions);
//					}
//					break;
//				case 1:
//					break;
//				default: return;
//				}
//			}

			subjectCondition = root.whereClause;
			if (doExport && (getParentBrowser() != null /* || isLimitExceeded */)) {
				subjectCondition = root.whereClause;
				if (subjectCondition != null) {
					subjectCondition = subjectCondition.replace('\r', ' ').replace('\n', ' ');
				}
				StringBuilder sb = new StringBuilder();
				boolean f = true;
				for (Row row: rows) {
					if (f) {
						f = false;
					} else {
						sb.append(" or ");
					}
					sb.append("(" + row.rowId + ")");
				}

				if (subjectCondition != null && subjectCondition.trim().length() > 0) {
					subjectCondition = "(" + subjectCondition + ") and (" + sb + ")";
				} else {
					subjectCondition = sb.toString();
				}
			}
			if (subjectCondition == null) {
				subjectCondition = "";
			}
//			if (doExport && isLimitExceeded && rows != null && !rows.isEmpty()) {
//				StringBuilder sb = new StringBuilder();
//				boolean f = true;
//				for (Row row: rows) {
//					if (f) {
//						f = false;
//					} else {
//						sb.append(" or ");
//					}
//					sb.append("(" + row.rowId + ")");
//				}
//				subjectCondition = sb.toString();
//			}
			subjectCondition = SqlUtil.replaceAliases(subjectCondition, "T", "T");

			if (!doExport) {
				for (int i = 1; ; ++i) {
					file = Environment.newFile("extractionmodel" + File.separator + "by-example").getPath();
					File newFile = new File(file);
					newFile.mkdirs();
					file += File.separator + "SbE-" + (dataModel.getDisplayName(stable).replaceAll("['`\"/\\\\\\~]+", "")) + "-" + ts + (i > 1? "-" + Integer.toString(i) : "") + ".jm";
					newFile = new File(file);
					if (!newFile.exists()) {
						break;
					}
				}
			} else {
				file = Configuration.getInstance().createTempFile().getPath();
			}

			Map<String, Map<String, double[]>> positions = new TreeMap<String, Map<String,double[]>>();
			collectPositions(positions);
			String currentModelSubfolder = DataModelManager.getCurrentModelSubfolder(executionContext);
			dataModel.save(file, stable, new SubjectLimitDefinition(null, null), subjectCondition, ScriptFormat.SQL, restrictionDefinitions, positions, new ArrayList<ExtractionModel.AdditionalSubject>(), currentModelSubfolder);

			final ExtractionModelFrame extractionModelFrame = ExtractionModelFrame.createFrame(file, false, !doExport, null, executionContext);
			extractionModelFrame.setDbConnectionDialogClone(getDbConnectionDialog());
			if (doExport) {
				extractionModelFrame.openExportDialog(false,
					new Runnable() {
						@Override
						public void run() {
							try {
								reloadDataModel();
							} catch (Exception e) {
								throw new RuntimeException(e);
							}
						}
					},
					new Runnable() {
						@Override
						public void run() {
							extractionModelFrame.dispose();
						}
					});
			} else {
				extractionModelFrame.markDirty();
				extractionModelFrame.expandAll();
			}
		} catch (Throwable e) {
			UIUtil.showException(this, "Error", e, session);
		} finally {
			UIUtil.resetWaitCursor(parent);
		}
	}

	private DefaultMutableTreeNode addChildNodes(BrowserContentPane browserContentPane, Collection<Association> restrictedDependencies, int[] count) {
		DefaultMutableTreeNode node = new DefaultMutableTreeNode(dataModel.getDisplayName(browserContentPane.table));
		count[0]++;
		Set<Table> regardedChildren = new HashSet<Table>();
		for (RowBrowser rb: browserContentPane.getChildBrowsers()) {
			DefaultMutableTreeNode childNode = addChildNodes(rb.browserContentPane, restrictedDependencies, count);
			node.add(childNode);
			regardedChildren.add(rb.browserContentPane.table);
		}
		for (final Association dep: restrictedDependencies) {
			if (dep.source == browserContentPane.table && !regardedChildren.contains(dep.destination)) {
				node.add(new DefaultMutableTreeNode(new Object() {
					String item = dataModel.getDisplayName(dep.destination);
					@Override
					public String toString() {
						return item;
					}
				}));
				count[0]++;
			}
		}
		return node;
	}

	private void disableDisregardedNonParentsOfDestination(Association a,
			Collection<Association> regardedAssociations,
			List<RestrictionDefinition> restrictionDefinitions) {
		for (Association npa: a.destination.associations) {
			if (!regardedAssociations.contains(npa)) {
				regardedAssociations.add(npa);
				if (npa.isInsertDestinationBeforeSource()) {
					disableDisregardedNonParentsOfDestination(npa, regardedAssociations, restrictionDefinitions);
				} else {
					RestrictionDefinition rest = new RestrictionDefinition(npa.source, npa.destination, npa.getName(), null, true);
					restrictionDefinitions.add(rest);
				}
			}
		}
	}

	private static class RestrictionLiteral {
		public String condition;
		public int distanceFromRoot;
		public boolean isIgnored;
		public boolean isIgnoredIfReversalIsRestricted = false;
		@Override
		public String toString() {
			return "Cond:" + condition + " Dist: " + distanceFromRoot + " isIgnored: " + isIgnored + " isIgnoredIfReversalIsRestricted: " + isIgnoredIfReversalIsRestricted;
		}
	}

	/**
	 * Creates restriction according to the given {@link Relationship} tree.
	 *
	 * @param root root of tree
	 * @return restrictions
	 */
	private List<RestrictionDefinition> createRestrictions(Relationship root, Table subject, Collection<Association> regardedAssociations, Collection<Association> restrictedDependencies, Collection<RestrictionDefinition> restrictedDependencyDefinitions) {
		List<RestrictionDefinition> restrictionDefinitions = new ArrayList<RestrictionDefinition>();

		Map<Association, List<RestrictionLiteral>> restrictionLiterals = new HashMap<Association, List<RestrictionLiteral>>();
		collectRestrictionLiterals(restrictionLiterals, root, subject, 0);

		for (Association association: restrictionLiterals.keySet()) {
			RestrictionDefinition rest = createRestrictionDefinition(association, restrictionLiterals, true);

			regardedAssociations.add(association);
			if (rest.isIgnored || (rest.condition != null && rest.condition.trim().length() != 0)) {
				restrictionDefinitions.add(rest);
				if (association.isInsertDestinationBeforeSource() && rest.isIgnored) {
					restrictedDependencies.add(association);
					restrictedDependencyDefinitions.add(rest);
				}
			}
		}

		return restrictionDefinitions;
	}

	private RestrictionDefinition createRestrictionDefinition(Association association, Map<Association, List<RestrictionLiteral>> restrictionLiterals, boolean checkReversal) {
		List<RestrictionLiteral> lits = restrictionLiterals.get(association);
		boolean useDistance = false;
		boolean hasTrue = false;
		boolean hasNotTrue = false;
		boolean hasNotFalse = false;
		Integer lastDist = null;
		for (RestrictionLiteral l: lits) {

			if (l.isIgnoredIfReversalIsRestricted) {

				if (checkReversal) {
					RestrictionDefinition revRest = createRestrictionDefinition(association.reversalAssociation, restrictionLiterals, false);
					if (revRest.isIgnored || (revRest.condition != null && revRest.condition.trim().length() > 0)) {
						l.isIgnored = true;
					} else {
						l.isIgnored = false;
						l.condition = null;
					}
					l.isIgnoredIfReversalIsRestricted = false;
				} else {
					l.isIgnored = true;
					l.isIgnoredIfReversalIsRestricted = false;
				}
			}

			// disabled since 5.0

//			if (lastDist != null && lastDist != l.distanceFromRoot) {
//				useDistance = true;
//			}
//			lastDist = l.distanceFromRoot;

			if (!l.isIgnored) {
				hasNotFalse = true;
				if (l.condition == null || l.condition.trim().length() == 0) {
					hasTrue = true;
				} else {
					hasNotTrue = true;
				}
			} else {
				hasNotTrue = true;
			}
		}

		boolean isIgnored;
		String condition = null;

		if (!hasNotFalse) {
			isIgnored = true;
		} else if (!hasNotTrue) {
			isIgnored = false;
		} else if (hasTrue && !useDistance) {
			isIgnored = false;
		} else {
			for (RestrictionLiteral l: lits) {
				if (!l.isIgnored) {
					String c = null;
					if (useDistance) {
						c = l.distanceFromRoot == 0? "A.$IS_SUBJECT" : ("A.$DISTANCE=" + l.distanceFromRoot);
					}
					if (l.condition != null && l.condition.trim().length() > 0) {
						if (c == null) {
							c = l.condition;
						} else {
							c = c + " and (" + l.condition + ")";
						}
					}
					if (condition == null) {
						condition = c;
					} else {
						condition += " or " + c;
					}
				}
			}
			isIgnored = false;
		}
		RestrictionDefinition rest = new RestrictionDefinition(association.source, association.destination, association.getName(), condition, isIgnored);
		return rest;
	}

	/**
	 * Collects restriction literals per association according to a given {@link Relationship} tree.
	 *
	 * @param restrictionLiterals to put literals into
	 * @param root root of tree
	 * @param distanceFromRoot distance
	 */
	private void collectRestrictionLiterals(Map<Association, List<RestrictionLiteral>> restrictionLiterals, Relationship root, Table subject, int distanceFromRoot) {
		for (Association association: subject.associations) {
			List<Relationship> children = new ArrayList<QueryBuilderDialog.Relationship>();
			for (Relationship r: root.children) {
				if (r.association == association) {
					children.add(r);
				}
			}
			if (children.isEmpty()) {
				children.add(null);
			}
			for (Relationship child: children) {
				RestrictionLiteral restrictionLiteral = new RestrictionLiteral();
				restrictionLiteral.distanceFromRoot = distanceFromRoot;
				restrictionLiteral.isIgnored = false;
				if (child == null) {
					restrictionLiteral.isIgnored = true;
//					if (association.isInsertDestinationBeforeSource()) {
						if (root.association != null && association == root.association.reversalAssociation) {
							if (association.getCardinality() == Cardinality.MANY_TO_ONE
									||
								association.getCardinality() == Cardinality.ONE_TO_ONE) {
								restrictionLiteral.isIgnoredIfReversalIsRestricted = true;
							}
						}
//					}
				} else {
					restrictionLiteral.condition = child.whereClause == null? "" : SqlUtil.replaceAliases(child.whereClause, "B", "A");
					collectRestrictionLiterals(restrictionLiterals, child, association.destination, distanceFromRoot + 1);
				}
				List<RestrictionLiteral> literals = restrictionLiterals.get(association);
				if (literals == null) {
					literals = new ArrayList<BrowserContentPane.RestrictionLiteral>();
					restrictionLiterals.put(association, literals);
				}
				literals.add(restrictionLiteral);
			}
		}
	}

	private void openSQLDialog(String titel, int x, int y, Object sql) {
		UIUtil.setWaitCursor(this);
		JDialog d;
		try {
			String LF = System.getProperty("line.separator", "\n");
			String sqlString = sql.toString().trim() + LF;
			if (sqlString.length() > 10L*1024L*1024L) {
				int o = JOptionPane.showOptionDialog(this, "SQL Script is large (" + (sqlString.length() / 1024) + " KB)", "SQL Script is large", JOptionPane.YES_NO_OPTION, JOptionPane.INFORMATION_MESSAGE, null, new Object[] { "Open", "Cancel", "Save Script" }, "Save Script");
				if (o == 2) {
					String fn = UIUtil.choseFile(null, ".", "Save SQL Script", ".sql", this, false, false, false);
					if (fn != null) {
						try {
							PrintWriter out = new PrintWriter(new FileWriter(fn));
							out.print(sqlString);
							out.close();
						} catch (Throwable e) {
							UIUtil.showException(this, "Error saving script", e, UIUtil.EXCEPTION_CONTEXT_USER_ERROR);
						}
					}
				}
				if (o != 0) {
					return;
				}
			}
			d = new JDialog(getOwner(), "Create SQL - " + titel, true);
			d.getContentPane().add(new SQLDMLPanel(sqlString, getSqlConsole(false), session, getMetaDataSource(),
					new Runnable() {
						@Override
						public void run() {
							reloadRows();
						}
					},
					new Runnable() {
						@Override
						public void run() {
							getSqlConsole(true);
						}
					},
					d, executionContext));
			d.pack();
			d.setLocation(x - 50, y - 100);
			d.setSize(700, Math.max(d.getHeight() + 20, 400));
			d.setLocation(getOwner().getX() + (getOwner().getWidth() - d.getWidth()) / 2, Math.max(0, getOwner().getY() + (getOwner().getHeight() - d.getHeight()) / 2));
			UIUtil.fit(d);
		} catch (Throwable e) {
			UIUtil.showException(this, "Error", e);
			return;
		} finally {
			UIUtil.resetWaitCursor(this);
		}
		d.setVisible(true);
	}

	protected abstract SQLConsole getSqlConsole(boolean switchToConsole);

	private void appendClosure() {
		if (getParentBrowser() != null) {
			if (rowsClosure.currentClosureRootPane == this) {
				List<Integer> selectedRows = new ArrayList<Integer>();
				RowSorter<? extends TableModel> rowSorter = rowsTable.getRowSorter();
				for (int i = rows.size() - 1; i >= 0; --i) {
					if (rowsClosure.currentClosureRootID.contains(rows.get(i).nonEmptyRowId)) {
						int vi = rowSorter.convertRowIndexToView(i);
						if (vi >= 0) {
							selectedRows.add(vi);
						}
					}
				}
				resetCurrentRowSelection();
				for (int si: selectedRows ) {
					setCurrentRowSelection(si, true);
				}
				setCurrentRowSelection(-1, true);
			} else {
				BrowserContentPane parentContentPane = getParentBrowser().browserContentPane;

				Set<Pair<BrowserContentPane, Row>> newElements = new HashSet<Pair<BrowserContentPane, Row>>();
				synchronized (getMonitorForFindClosure()) {
					FindClosureContext findClosureContext = new FindClosureContext();
					for (Pair<BrowserContentPane, Row> e: rowsClosure.currentClosure) {
						if (e.a == parentContentPane) {
							parentContentPane.findClosure(e.b, newElements, true, findClosureContext);
						}
					}
				}
				rowsClosure.currentClosure.addAll(newElements);
				rowsClosure.currentClosureRowIDs.clear();
				for (Pair<BrowserContentPane, Row> r: rowsClosure.currentClosure) {
					rowsClosure.currentClosureRowIDs.add(new Pair<BrowserContentPane, String>(r.a, r.b.nonEmptyRowId));
				}
				rowsTable.repaint();
				adjustClosure(null, this);
			}
		}
	}

	protected void setCurrentRowSelectionAndReloadChildrenIfLimitIsExceeded(int i, boolean append) {
		setCurrentRowSelection(i, append);
		if (i >= 0) {
			reloadChildrenIfLimitIsExceeded();
		}
	}

	protected void setCurrentRowSelection(int i, boolean append) {
		currentRowSelection = i;
		if (i >= 0) {
			int mi = rowsTable.getRowSorter().convertRowIndexToModel(i);
			if (mi >= 0 && mi < rows.size()) {
				Row row = rows.get(mi);
				if (!append) {
					for (Pair<BrowserContentPane, Row> e: rowsClosure.currentClosure) {
						e.a.currentRowSelection = -1;
					}
					rowsClosure.currentClosure.clear();
					rowsClosure.parentPath.clear();
					rowsClosure.currentClosureRootID.clear();
				}
				rowsClosure.currentClosureRootID.add(row.nonEmptyRowId);
				rowsClosure.currentClosureRootPane = this;
				findClosure(row);
				Rectangle visibleRect = rowsTable.getVisibleRect();
				Rectangle pos = rowsTable.getCellRect(i, 0, false);
				rowsTable.scrollRectToVisible(new Rectangle(visibleRect.x, pos.y, 1, pos.height));
			}
		}
		rowsClosure.currentClosureRowIDs.clear();
		for (Pair<BrowserContentPane, Row> r: rowsClosure.currentClosure) {
			rowsClosure.currentClosureRowIDs.add(new Pair<BrowserContentPane, String>(r.a, r.b.nonEmptyRowId));
		}
		rowsTable.repaint();
		adjustClosure(this, null);
	}

	private void resetCurrentRowSelection() {
		currentRowSelection = -1;
		rowsClosure.currentClosure.clear();
		rowsClosure.parentPath.clear();
		rowsClosure.currentClosureRootID.clear();
		rowsTable.repaint();
		adjustClosure(this, null);
	}

	private void reloadChildrenIfLimitIsExceeded() {
		for (RowBrowser ch: getChildBrowsers()) {
			if (ch.browserContentPane != null) {
				if (ch.browserContentPane.isLimitExceeded) {
					Boolean oldForceAdjustRows = Desktop.forceAdjustRows;
					try {
						Desktop.forceAdjustRows = true;
						ch.browserContentPane.reloadRows("because rows limit is exceeded");
					} finally {
						Desktop.forceAdjustRows = oldForceAdjustRows;
					}
				} else {
					ch.browserContentPane.reloadChildrenIfLimitIsExceeded();
				}
			}
		}
	}

	private JPopupMenu createNavigationMenu(JPopupMenu popup, final Row row, final int rowIndex, List<String> assList, Map<String, Association> assMap,
			String title, String prefix, final boolean navigateFromAllRows, final int rowCountPriority, final AllNonEmptyItem allNonEmptyItem, final Object context) {
		JScrollC2Menu nav = new JScrollC2Menu(title);

		if (prefix.equals("1")) {
			nav.setIcon(UIUtil.scaleIcon(this, redDotIcon));
		}
		if (prefix.equals("2")) {
			nav.setIcon(UIUtil.scaleIcon(this, greenDotIcon));
		}
		if (prefix.equals("3")) {
			nav.setIcon(UIUtil.scaleIcon(this, blueDotIcon));
		}
		if (prefix.equals("4")) {
			nav.setIcon(UIUtil.scaleIcon(this, greyDotIcon));
		}
		JMenu current = nav;

		final List<Row> pRows;
		if (row == null) {
			pRows = Collections.emptyList();
		} else {
			pRows = getSelectedRows(row);
		}
		String countCondition = toCondition(pRows);

		int l = 0;
		for (String name : assList) {
			if (!name.startsWith(prefix)) {
				continue;
			}

			final Association association = assMap.get(name);

			++l;

			final JMenuItem item = new JMenuItem("  " + (name.substring(1)) + "   ");
			item.setToolTipText(assocToolTip(association.getUnrestrictedJoinCondition(), association));
			final JLabel countLabel = new JLabel(". >99999 ") {
				@Override
				public void paint(Graphics g) {
					if (!getText().startsWith(".")) {
						super.paint(g);
					}
				}
			};

			boolean excludeFromANEmpty = false;
			for (RowBrowser child: getChildBrowsers()) {
				if (association == child.association &&
						(child.browserContentPane.getAndConditionText().trim().length() == 0
						|| child.browserContentPane.getAndConditionText().trim().equals(countCondition))) {
					item.setFont(new Font(item.getFont().getName(), item.getFont().getStyle() | Font.ITALIC, item.getFont().getSize()));
					excludeFromANEmpty = true;
					break;
				}
			}

			if (association.reversalAssociation == this.association) {
				excludeFromANEmpty = true;
			}

			final ActionListener itemAction = new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					if (navigateFromAllRows) {
						navigateTo(association, null);
					} else {
						navigateTo(association, pRows);
					}
				}
			};
			item.addActionListener(itemAction);
			if (!excludeFromANEmpty) {
				allNonEmptyItem.todo++;
			}
			final boolean fExcludeFromANEmpty = excludeFromANEmpty;

			if (!isPending && !rows.isEmpty()) {
				getRunnableQueue().add(new RunnableWithPriority() {

					final int MAX_RC = 1000;

					String andConditionText = getAndConditionText();

					@Override
					public int getPriority() {
						return rowCountPriority;
					}

					@Override
					public void run() {
						List<Row> r;
						Pair<String, Association> key;
						if (rowIndex < 0) {
							r = rows;
							key = new Pair<String, Association>("", association);
						} else {
							r = pRows;
							key = new Pair<String, Association>(row.nonEmptyRowId, association);
						}

						Pair<RowCount, Long> cachedCount = rowCountCache.get(key);
						RowCount rowCount;

						if (cachedCount != null && cachedCount.b > System.currentTimeMillis()) {
							rowCount = cachedCount.a;
						} else {
							RowCounter rc = new RowCounter(table, association, r, session, rowIdSupport);
							try {
								rowCount = rc.countRows(andConditionText, context, MAX_RC + 1, false);
							} catch (SQLException e) {
								rowCount = new RowCount(-1, true);
							}
							rowCountCache.put(key, new Pair<RowCount, Long>(rowCount, System.currentTimeMillis() + MAX_ROWCOUNTCACHE_RETENTION_TIME));
						}

						final RowCount count = rowCount;

						UIUtil.invokeLater(new Runnable() {
							@Override
							public void run() {
								String cs = " " + (count.count < 0? "?" : (count.count > MAX_RC)? (">" + MAX_RC) : count.isExact? count.count : (">" + count.count)) + " ";
								countLabel.setText(cs);
								if (count.count == 0) {
									countLabel.setForeground(Color.lightGray);
								}
								if (!fExcludeFromANEmpty) {
									allNonEmptyItem.rowsCounted(count.count, itemAction);
								}
							}
						});
					}
				});
			}

			if (current != null) {
				GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
				gridBagConstraints.gridx = 1;
				gridBagConstraints.gridy = l;
				gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
				gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
				gridBagConstraints.weightx = 1.0;
				current.getPopupMenu().add(item, gridBagConstraints);
				gridBagConstraints = new java.awt.GridBagConstraints();
				gridBagConstraints.gridx = 2;
				gridBagConstraints.gridy = l;
				gridBagConstraints.fill = java.awt.GridBagConstraints.NONE;
				gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
				gridBagConstraints.weightx = 1.0;
				current.getPopupMenu().add(countLabel, gridBagConstraints);
			} else {
				popup.add(item);
			}
		}
		if (l > 0) {
			popup.add(nav);
		}
		return popup;
	}

	private long lastReloadTS = 0;

	/**
	 * Reloads rows.
	 */
	public void reloadRows() {
		reloadRows(null);
	}

	boolean loadedRowsAreRestricted = false;

	/**
	 * Reloads rows.
	 */
	public void reloadRows(String cause) {
		if (!suppressReload) {
			lastReloadTS = System.currentTimeMillis();
			cancelLoadJob(true);
			setPendingState(true, true);
			rows.clear();
			updateMode("loading", cause);
			setPendingState(false, false);
			int limit = getReloadLimit();
			LoadJob reloadJob;
			loadedRowsAreRestricted = false;
			if (statementForReloading != null) {
				reloadJob = new LoadJob(limit, statementForReloading, getParentBrowser(), false);
			} else {
				loadedRowsAreRestricted = !(table instanceof SqlStatementTable) && !getAndConditionText().trim().isEmpty();
				reloadJob = new LoadJob(limit, (table instanceof SqlStatementTable)? "" : getAndConditionText(), getParentBrowser(), selectDistinctCheckBox.isSelected());
				currentSelectedRowCondition = getAndConditionText();
			}
			synchronized (this) {
				currentLoadJob = reloadJob;
			}
			getRunnableQueue().add(reloadJob);
		}
	}

	/**
	 * Reload rows from {@link #table}.
	 *
	 * @param rows
	 *            to put the rows into
	 * @param loadJob
	 *            cancellation context
	 * @param limit
	 *            row number limit
	 */
	private void reloadRows(ResultSet inputResultSet, String andCond, final List<Row> rows, LoadJob loadJob, int limit, boolean selectDistinct) throws SQLException {
		try {
			session.setSilent(true);
			reloadRows(inputResultSet, andCond, rows, loadJob, limit, selectDistinct, null);
			return;
		} catch (Throwable e) { // embedded DBMS may throw non-SQLException
			Session._log.warn("failed, try another strategy (" +  e.getMessage() + ")");
		} finally {
			session.setSilent(false);
		}
		Set<String> existingColumnsLowerCase = null;
		if (!(table instanceof SqlStatementTable) && statementForReloading == null) {
			existingColumnsLowerCase = findColumnsLowerCase(table, session);
		}
		reloadRows(inputResultSet, andCond, rows, loadJob, limit, selectDistinct, existingColumnsLowerCase);
	}

	/**
	 * Finds the columns of a given {@link Table}.
	 *
	 * @param table the table
	 * @param session the statement executor for executing SQL-statements
	 */
	private Set<String> findColumnsLowerCase(Table table, Session session) {
		try {
			Set<String> columns = new HashSet<String>();
			Quoting quoting = Quoting.getQuoting(session);
			String defaultSchema = JDBCMetaDataBasedModelElementFinder.getDefaultSchema(session, session.getSchema());
			String schema = quoting.unquote(table.getOriginalSchema(defaultSchema));
			String tableName = quoting.unquote(table.getUnqualifiedName());
			ResultSet resultSet = JDBCMetaDataBasedModelElementFinder.getColumns(session, schema, tableName, "%", false, false, null);
			while (resultSet.next()) {
				String colName = resultSet.getString(4).toLowerCase(Locale.ENGLISH);
				columns.add(colName);
			}
			resultSet.close();
			if (columns.isEmpty()) {
				if (session.getMetaData().storesUpperCaseIdentifiers()) {
					schema = schema.toUpperCase(Locale.ENGLISH);
					tableName = tableName.toUpperCase(Locale.ENGLISH);
				} else {
					schema = schema.toLowerCase(Locale.ENGLISH);
					tableName = tableName.toLowerCase(Locale.ENGLISH);
				}
				resultSet = JDBCMetaDataBasedModelElementFinder.getColumns(session, schema, tableName, "%", false, false, null);
				while (resultSet.next()) {
					String colName = resultSet.getString(4).toLowerCase(Locale.ENGLISH);
					columns.add(colName);
				}
			}
			if (columns.isEmpty()) {
				return null;
			}
			return columns;
		} catch (Exception e) {
			return null;
		}
	}

	/**
	 * Reload rows from {@link #table}.
	 *
	 * @param rows
	 *            to put the rows into
	 * @param loadJob
	 *            cancellation context
	 * @param limit
	 *            row number limit
	 */
	private void reloadRows(ResultSet inputResultSet, String andCond, final List<Row> rows, LoadJob loadJob, int limit, boolean selectDistinct, Set<String> existingColumnsLowerCase) throws SQLException {
		if (table instanceof SqlStatementTable || statementForReloading != null) {
			try {
				session.setSilent(true);
				Map<String, List<Row>> rowsMap = new HashMap<String, List<Row>>();
				reloadRows(inputResultSet, null, andCond, null, rowsMap, loadJob, limit, false, null, existingColumnsLowerCase);
				if (rowsMap.get("") != null) {
					rows.addAll(rowsMap.get(""));
				}
			} finally {
				session.setSilent(false);
			}
			return;
		}

		List<Row> pRows = parentRows;
		if (pRows == null) {
			pRows = Collections.singletonList(null);
		} else {
			pRows = new ArrayList<Row>(pRows);
		}
		Map<String, Row> rowSet = new HashMap<String, Row>();
		loadJob.checkCancellation();
		if (parentRows != null) {
			beforeReload();
		}
		noNonDistinctRows = 0;
		noDistinctRows = 0;

		if (association != null && rowIdSupport.getPrimaryKey(association.source, session).getColumns().isEmpty()) {
			loadRowBlocks(inputResultSet, null, andCond, rows, loadJob, limit, selectDistinct, pRows, rowSet, 1, existingColumnsLowerCase);
		} else {
			if (useInlineViewForResolvingAssociation(session)) {
				try {
					InlineViewStyle inlineViewStyle = session.getInlineViewStyle();
					if (inlineViewStyle != null) {
						loadRowBlocks(inputResultSet, inlineViewStyle, andCond, rows, loadJob, limit, selectDistinct, pRows, rowSet, 510, existingColumnsLowerCase);
						return;
					}
				} catch (Throwable e) { // embedded DBMS may throw non-SQLException
					Session._log.warn("failed, try another blocking-size (" +  e.getMessage() + ")");
				}
			}
			try {
				loadRowBlocks(inputResultSet, null, andCond, rows, loadJob, limit, selectDistinct, pRows, rowSet, 510, existingColumnsLowerCase);
				return;
			} catch (Throwable e) { // embedded DBMS may throw non-SQLException
				Session._log.warn("failed, try another blocking-size (" +  e.getMessage() + ")");
			}
			try {
				loadRowBlocks(inputResultSet, null, andCond, rows, loadJob, limit, selectDistinct, pRows, rowSet, 300, existingColumnsLowerCase);
				return;
			} catch (Throwable e) { // embedded DBMS may throw non-SQLException
				Session._log.warn("failed, try another blocking-size (" +  e.getMessage() + ")");
			}
			try {
				loadRowBlocks(inputResultSet, null, andCond, rows, loadJob, limit, selectDistinct, pRows, rowSet, 100, existingColumnsLowerCase);
				return;
			} catch (Throwable e) { // embedded DBMS may throw non-SQLException
				Session._log.warn("failed, try another blocking-size (" +  e.getMessage() + ")");
			}
			try {
				loadRowBlocks(inputResultSet, null, andCond, rows, loadJob, limit, selectDistinct, pRows, rowSet, 40, existingColumnsLowerCase);
				return;
			} catch (Throwable e) { // embedded DBMS may throw non-SQLException
				Session._log.warn("failed, try another blocking-size (" +  e.getMessage() + ")");
			}
		}

		loadRowBlocks(inputResultSet, null, andCond, rows, loadJob, limit, selectDistinct, pRows, rowSet, 1, existingColumnsLowerCase);
	}

	static boolean useInlineViewForResolvingAssociation(Session session) {
		return session.dbms.isUseInlineViewsInDataBrowser();
	}

	private void loadRowBlocks(ResultSet inputResultSet, InlineViewStyle inlineViewStyle, String andCond, final List<Row> rows, LoadJob loadJob, int limit, boolean selectDistinct, List<Row> pRows,
			Map<String, Row> rowSet, int NUM_PARENTS, Set<String> existingColumnsLowerCase) throws SQLException {
		List<List<Row>> parentBlocks = new ArrayList<List<Row>>();
		List<Row> currentBlock = new ArrayList<Row>();
		Set<String> regPRows = new HashSet<String>();
		Map<Row, Integer> parentRowIndex = new IdentityHashMap<Row, Integer>();
		for (int i = 0; i < pRows.size(); ++i) {
			parentRowIndex.put(pRows.get(i), i);
		}
		parentBlocks.add(currentBlock);
		BrowserContentPane parentPane = null;
		if (getParentBrowser() != null) {
			parentPane = getParentBrowser().browserContentPane;
		}
		for (boolean inClosure: new boolean[] { true, false }) {
			boolean firstNonClosure = false;
			for (Row pRow : pRows) {
				if (parentPane != null) {
					if (rowsClosure.currentClosure.contains(new Pair<BrowserContentPane, Row>(parentPane, pRow))) {
						if (!inClosure) {
							continue;
						}
					} else {
						if (inClosure) {
							continue;
						}
					}
				} else if (!inClosure) {
					break;
				}
				if (currentBlock.size() >= NUM_PARENTS || (!inClosure && !firstNonClosure)) {
					if (!currentBlock.isEmpty()) {
						currentBlock = new ArrayList<Row>();
						parentBlocks.add(currentBlock);
					}
					if (!inClosure) {
						firstNonClosure = true;
					}
				}
				currentBlock.add(pRow);
			}
		}
		int parentIndex = 0;

		if (!pRows.isEmpty()) for (List<Row> pRowBlockI : parentBlocks) {
			List<Row> pRowBlock = pRowBlockI;
			Map<String, List<Row>> newBlockRows = new HashMap<String, List<Row>>();
			boolean loaded = false;

			if (pRowBlock.size() == 1 && pRowBlock.get(0) == null) {
				pRowBlock = null;
			}

			if (session.dbms.getSqlLimitSuffix() != null) {
				try {
					session.setSilent(true);
					reloadRows(inputResultSet, inlineViewStyle, andCond, pRowBlock, newBlockRows, loadJob, limit, false, session.dbms.getSqlLimitSuffix(), existingColumnsLowerCase);
					loaded = true;
				} catch (Throwable e) { // embedded DBMS may throw non-SQLException
					Session._log.warn("failed, try another limit-strategy (" +  e.getMessage() + ")");
				} finally {
					session.setSilent(false);
				}
			}
			if (!loaded) {
				try {
					session.setSilent(true);
					reloadRows(inputResultSet, inlineViewStyle, andCond, pRowBlock, newBlockRows, loadJob, limit, true, null, existingColumnsLowerCase);
					loaded = true;
				} catch (Throwable e) { // embedded DBMS may throw non-SQLException
					Session._log.warn("failed, try another limit-strategy (" +  e.getMessage() + ")");
				} finally {
					session.setSilent(false);
				}
				if (!loaded) {
					try {
						session.setSilent(true);
						reloadRows(inputResultSet, inlineViewStyle, andCond, pRowBlock, newBlockRows, loadJob, limit, false, null, existingColumnsLowerCase);
					} finally {
						session.setSilent(false);
					}
				}
			}
			if (pRowBlock == null) {
				pRowBlock = new ArrayList<Row>();
				pRowBlock.add(null);
			}
			for (Row pRow: pRowBlock) {
				loadJob.checkCancellation();
				boolean dupParent = false;
				if (pRow != null) {
					if (regPRows.contains(pRow.nonEmptyRowId)) {
						dupParent = true;
					}
					regPRows.add(pRow.nonEmptyRowId);
				}
				List<Row> newRows = new ArrayList<Row>();
				String rId = pRow == null? "" : pRow.nonEmptyRowId;
				if (newBlockRows.get(rId) != null) {
					newRows.addAll(newBlockRows.get(rId));
				}
				sortNewRows(newRows);
				if (parentRows != null) {
					if (!newRows.isEmpty()) {
						Integer i = parentRowIndex.get(pRow);
						for (Row r: newRows) {
							r.setParentModelIndex(i != null? i : parentIndex);
						}
					}
					++parentIndex;
					for (Row row : newRows) {
						Row exRow = rowSet.get(row.nonEmptyRowId);
						if (!dupParent) {
							if (exRow != null) {
								++noNonDistinctRows;
							} else {
								++noDistinctRows;
							}
						}
						if (exRow != null && (selectDistinct || dupParent)) {
							addRowToRowLink(pRow, exRow);
						} else {
							rows.add(row);
							addRowToRowLink(pRow, row);
							rowSet.put(row.nonEmptyRowId, row);
							--limit;
						}
					}
				} else {
					rows.addAll(newRows);
					limit -= newRows.size();
				}
				if (limit <= 0) {
					if (parentPane != null) {
						if (rowsClosure.currentClosure.contains(new Pair<BrowserContentPane, Row>(parentPane, pRow))) {
							loadJob.closureLimitExceeded = true;
						}
					}
					break;
				}
			}
			if (limit <= 0) {
				break;
			}
		}
	}

	private void sortNewRows(List<Row> newRows) {
		if (rowsTable != null && rowsTable.getRowSorter() != null) {
			List<? extends SortKey> sk = rowsTable.getRowSorter().getSortKeys();
			final int col;
			final boolean asc;
			if (!sk.isEmpty()) {
				col = sk.get(0).getColumn();
				asc = sk.get(0).getSortOrder() == SortOrder.ASCENDING;
			} else {
				col = getDefaultSortColumn();
				asc = true;
			}
			if (col >= 0) {
				Collections.sort(newRows, new Comparator<Row>() {
					@Override
					public int compare(Row a, Row b) {
						Object va = null;
						if (a != null && a.values != null && a.values.length > col) {
							va = a.values[col];
						}
						Object vb = null;
						if (b != null && b.values != null && b.values.length > col) {
							vb = b.values[col];
						}
						if (va == null && vb == null) {
							return 0;
						}
						if (va == null) {
							return -1;
						}
						if (vb == null) {
							return 1;
						}
						if (va.getClass().equals(vb.getClass())) {
							if (va instanceof Comparable<?>) {
								int cmp = ((Comparable<Object>) va).compareTo(vb);
								return asc? cmp : -cmp;
							}
							return 0;
						}
						int cmp = va.getClass().getName().compareTo(vb.getClass().getName());
						return asc? cmp : -cmp;
					}
				});
			}
		}
	}

	/**
	 * Reload rows from {@link #table}.
	 *
	 * @param rows
	 *            to put the rows into
	 * @param context
	 *            cancellation context
	 * @param rowCache
	 * @param allPRows
	 */
	private void reloadRows(ResultSet inputResultSet, InlineViewStyle inlineViewStyle, String andCond, final List<Row> parentRows, final Map<String, List<Row>> rows, LoadJob loadJob, int limit, boolean useOLAPLimitation,
			String sqlLimitSuffix, Set<String> existingColumnsLowerCase) throws SQLException {
		reloadRows0(inputResultSet, inlineViewStyle, andCond, parentRows, rows, loadJob, parentRows == null? limit : Math.max(5000, limit), useOLAPLimitation, sqlLimitSuffix, existingColumnsLowerCase);
	}

	/**
	 * Gets qualified table name.
	 *
	 * @param t the table
	 * @return qualified name of t
	 */
	private String qualifiedTableName(Table t, Quoting quoting) {
		String schema = t.getSchema("");
		if (schema.length() == 0) {
			return quoting.requote(t.getUnqualifiedName());
		}
		return quoting.requote(schema) + "." + quoting.requote(t.getUnqualifiedName());
	}

	/**
	 * Reload rows from {@link #table}.
	 *
	 * @param rows
	 *            to put the rows into
	 * @param loadJob
	 *            cancellation context
	 */
	private void reloadRows0(ResultSet inputResultSet, InlineViewStyle inlineViewStyle, String andCond, final List<Row> parentRows, final Map<String, List<Row>> rows, LoadJob loadJob, int limit, boolean useOLAPLimitation,
			String sqlLimitSuffix, Set<String> existingColumnsLowerCase) throws SQLException {
		String sql = "Select ";
		final Quoting quoting = Quoting.getQuoting(session);
		final Set<String> pkColumnNames = new HashSet<String>();
		final Set<String> parentPkColumnNames = new HashSet<String>();
		final boolean selectParentPK = association != null && parentRows != null && parentRows.size() > 1;
		final Set<Integer> unknownColumnIndexes = new HashSet<Integer>();
		int numParentPKColumns = 0;

		if (table instanceof SqlStatementTable || statementForReloading != null) {
			sql = andCond;
			if (!(table instanceof SqlStatementTable)) {
				for (Column pk: rowIdSupport.getPrimaryKey(table, session).getColumns()) {
					pkColumnNames.add(quoting.requote(pk.name));
				}
			} else {
				table.setColumns(new ArrayList<Column>());
			}
		} else {
			String olapPrefix = "Select ";
			String olapSuffix = ") S Where S." + ROWNUMBERALIAS + " <= " + limit;
			boolean limitSuffixInSelectClause = sqlLimitSuffix != null &&
					(sqlLimitSuffix.toLowerCase(Locale.ENGLISH).startsWith("top ") || sqlLimitSuffix.toLowerCase(Locale.ENGLISH).startsWith("first "));
			if (sqlLimitSuffix != null && limitSuffixInSelectClause) {
				sql += (sqlLimitSuffix.replace("%s", Integer.toString(limit))) + " ";
			}
			int colI = 1;
			boolean f = true;
			if (selectParentPK) {
				int i = 0;
				for (Column column: rowIdSupport.getPrimaryKey(association.source, session).getColumns()) {
					String name = quoting.requote(column.name);
					sql += (!f ? ", " : "") + "B." + name + " AS B" + i;
					olapPrefix += (!f ? ", " : "") + "S.B" + i;
					++numParentPKColumns;
					++i;
					++colI;
					f = false;
				}
			}
			int i = 0;

			for (Column column : rowIdSupport.getColumns(table, session)) {
				String name = quoting.requote(column.name);
				if (existingColumnsLowerCase != null && !rowIdSupport.isRowIdColumn(column) && !existingColumnsLowerCase.contains(quoting.unquote(name).toLowerCase(Locale.ENGLISH))) {
					sql += (!f ? ", " : "") + "'?' AS A" + i;
					unknownColumnIndexes.add(colI);
				} else {
					sql += (!f ? ", " : "") + "A." + name + " AS A" + i;
				}
				olapPrefix += (!f ? ", " : "") + "S.A" + i;
				++i;
				++colI;
				f = false;
			}
			f = true;
			if (selectParentPK) {
				int j = 0;
				for (Column pk: rowIdSupport.getPrimaryKey(association.source, session).getColumns()) {
					parentPkColumnNames.add(quoting.requote(pk.name));
					++j;
					f = false;
				}
			}
			int j = 0;
			for (Column pk: rowIdSupport.getPrimaryKey(table, session).getColumns()) {
				pkColumnNames.add(quoting.requote(pk.name));
				++j;
				f = false;
			}
			if (useOLAPLimitation) {
				sql += ", row_number() over(";
				if (useOLAPLimitation) {
					sql += "order by -1"; // + orderBy;
				}
				sql += ") as " + ROWNUMBERALIAS + "";
			}
			sql += " From ";
			if (association != null) {
				sql += qualifiedTableName(association.source, quoting) + " B join ";
			}
			sql += qualifiedTableName(table, quoting) + " A";
			if (association != null) {
				if (association.reversed) {
					sql += " on " + association.getUnrestrictedJoinCondition();
				} else {
					sql += " on " + SqlUtil.reversRestrictionCondition(association.getUnrestrictedJoinCondition());
				}
			}

			boolean whereExists = false;
			if (parentRows != null && !parentRows.isEmpty()) {
				if (association != null && parentRows.get(0).rowId.length() == 0) {
					throw new SqlException("Missing primary key for table: \"" + Quoting.staticUnquote(association.source.getName()) + "\"\n"
							+ "Resolution: define the primary key manually using the data model editor.", "", null);
				}
				if (parentRows.size() == 1) {
					sql += " Where (" + parentRows.get(0).rowId + ")";
				} else {
					StringBuilder sb = new StringBuilder();
					if (inlineViewStyle != null && association != null) {
						sb.append(" join ");
						List<String> columnNames = new ArrayList<String>();
						for (Column pkColumn: rowIdSupport.getPrimaryKey(association.source, session).getColumns()) {
							columnNames.add(pkColumn.name);
						}
						String[] columnNamesAsArray = columnNames.toArray(new String[columnNames.size()]);
						sb.append(inlineViewStyle.head(columnNamesAsArray));
						int rowNumber = 0;
						for (Row parentRow: parentRows) {
							if (rowNumber > 0) {
								sb.append(inlineViewStyle.separator());
							}
							sb.append(inlineViewStyle.item(parentRow.primaryKey, columnNamesAsArray, rowNumber));
							++rowNumber;
						}
						sb.append(inlineViewStyle.terminator("C", columnNamesAsArray));
						sb.append(" on (");
						boolean f2 = true;
						for (String pkColumnName: columnNames) {
							if (!f2) {
								sb.append(" and ");
							}
							sb.append("B." + pkColumnName + " = " + "C." + pkColumnName);
							f2 = false;
						}
						sb.append(")");
					} else {
						for (Row parentRow: parentRows) {
							if (sb.length() == 0) {
								sb.append(" Where ((");
							} else {
								sb.append(" or (");
							}
							sb.append(parentRow.rowId).append(")");
						}
						sb.append(")");
					}
					sql += sb.toString();
				}
				whereExists = true;
			}
			if (andCond.trim().length() > 0) {
				sql += (whereExists ? " and" : " Where") + " (" + (andCond) + ")";
			}
			olapPrefix += " From (";
			if (useOLAPLimitation) {
				sql = olapPrefix + sql + olapSuffix;
			}
			if (sqlLimitSuffix != null && !limitSuffixInSelectClause) {
				sql += " " + (sqlLimitSuffix.replace("%s", Integer.toString(limit)));
			}
		}

		if (sql.length() > 0 || inputResultSet != null) {
			final Map<Integer, Integer> pkPosToColumnPos = new HashMap<Integer, Integer>();
			if (!(table instanceof SqlStatementTable) && statementForReloading == null) {
				List<Column> pks = rowIdSupport.getPrimaryKey(table, session).getColumns();
				List<Column> columns = rowIdSupport.getColumns(table, session);
				for (int i = 0; i < columns.size(); ++i) {
					Column column = columns.get(i);
					for (int j = 0; j < pks.size(); ++j) {
						if (column.name.equals(pks.get(j).name)) {
							pkPosToColumnPos.put(j, i);
							break;
						}
					}
				}
			}
			final int finalNumParentPKColumns = numParentPKColumns;
			AbstractResultSetReader reader = new AbstractResultSetReader() {
				Map<Integer, Integer> typeCache = new HashMap<Integer, Integer>();
				int rowNr = 0;

				@Override
				public void init(ResultSet resultSet) throws SQLException {
					ResultSetMetaData metaData = getMetaData(resultSet);
					int columnCount = metaData.getColumnCount();
					if (table instanceof SqlStatementTable) {
						for (int ci = 1; ci <= columnCount; ++ci) {
							table.getColumns().add(new Column(metaData.getColumnLabel(ci), metaData.getColumnTypeName(ci), -1, -1));
						}
					}
					int[] columnTypes = new int[columnCount];
					String[] columnTypeNames = new String[columnCount];
					for (int ci = 1 + finalNumParentPKColumns; ci <= columnCount; ++ci) {
						if (metaData instanceof MemorizedResultSetMetaData) {
							columnTypes[ci - 1 - finalNumParentPKColumns] = ((MemorizedResultSetMetaData) metaData).types[ci - 1];
							columnTypeNames[ci - 1 - finalNumParentPKColumns] = ((MemorizedResultSetMetaData) metaData).typeNames[ci - 1];
						} else {
							columnTypes[ci - 1 - finalNumParentPKColumns] = metaData.getColumnType(ci);
							columnTypeNames[ci - 1 - finalNumParentPKColumns] = metaData.getColumnTypeName(ci);
						}
					}
					browserContentCellEditor = new BrowserContentCellEditor(columnTypes, columnTypeNames, session);
				}

				@Override
				public void readCurrentRow(ResultSet resultSet) throws SQLException {
					int i = 1, vi = 0;
					String parentRowId = "";
					if (selectParentPK) {
						Object v[] = new Object[rowIdSupport.getPrimaryKey(association.source, session).getColumns().size()];
						for (Column column: rowIdSupport.getPrimaryKey(association.source, session).getColumns()) {
							parentRowId = readRowFromResultSet(parentPkColumnNames, resultSet, i, vi, parentRowId, v, column, null, null, unknownColumnIndexes);
							++i;
							++vi;
						}
					} else {
						if (parentRows != null && parentRows.size() == 1) {
							parentRowId = parentRows.get(0).rowId;
						}
					}

					Map<String, String> pkColumn = new HashMap<String, String>();
					Map<String, String> pkColumnValue = new HashMap<String, String>();

					Object v[] = new Object[rowIdSupport.getColumns(table, session).size()];
					vi = 0;
					for (Column column: rowIdSupport.getColumns(table, session)) {
						readRowFromResultSet(pkColumnNames, resultSet, i, vi, "", v, column, pkColumn, pkColumnValue, unknownColumnIndexes);
						++i;
						++vi;
					}

					String rowId = "";
					String[] primaryKey = null;
					PrimaryKey primaryKeys = rowIdSupport.getPrimaryKey(table, session);
					if (primaryKeys != null && resultSetType == null) {
						int pkPos = 0;
						primaryKey = new String[primaryKeys.getColumns().size()];
						for (Column column : primaryKeys.getColumns()) {
							if (rowId.length() > 0) {
								rowId += " and ";
							}
							rowId += pkColumn.get(column.name);
							Integer colPos = pkPosToColumnPos.get(pkPos);
							if (colPos != null) {
								primaryKey[pkPos] = pkColumnValue.get(column.name);
							}
							++pkPos;
						}
					} else {
						rowId = Integer.toString(++rowNr);
					}

					List<Row> cRows = rows.get(parentRowId);
					if (cRows == null) {
						cRows = new ArrayList<Row>();
						rows.put(parentRowId, cRows);
					}
					cRows.add(new Row(rowId, primaryKey, v));
				}

				private String readRowFromResultSet(final Set<String> pkColumnNames, ResultSet resultSet, int i, int vi, String rowId, Object[] v, Column column, Map<String, String> pkColumn, Map<String, String> pkColumnValue, Set<Integer> unknownColumnIndexes)
						throws SQLException {
					Object value = "";
					if (unknownColumnIndexes.contains(i)) {
						value = new UnknownValue() {
							@Override
							public String toString() {
								return "?";
							}
						};
					} else {
						int type = SqlUtil.getColumnType(session.dbms, resultSet, getMetaData(resultSet), i, typeCache);
						Object lob = null;
						if (type == 0) {
							lob = resultSet.getObject(i);
						}
						CellContentConverter cellContentConverter = getCellContentConverter(resultSet, session, session.dbms);
						if (type == Types.BLOB || type == Types.CLOB || type == Types.NCLOB || type == Types.SQLXML
							|| (type == 0 &&
								(lob instanceof Blob || lob instanceof Clob || lob instanceof SQLXML)
								)) {
							Object object = cellContentConverter.getObject(resultSet, i);
							if (object == null || resultSet.wasNull()) {
								value = null;
							} else {
								boolean isBlob = object instanceof Blob;
								Object lobValue = toLobRender(object);
								if (lobValue != null) {
									object = cellContentConverter.getObject(resultSet, i);
								}
								String smallLob = CellContentConverter.getSmallLob(object, session.dbms, MAXBLOBLENGTH, MAXCLOBLENGTH);
								if (smallLob != null && lobValue != null) {
									final String val = lobValue.toString();
									lobValue = new SQLValue() {
										@Override
										public String getSQLExpression() {
											return smallLob;
										}
										@Override
										public String toString() {
											return isBlob? smallLob : val;
										}
									};
								}
								if (lobValue != null) {
									value = lobValue;
								}
							}
						} else {
							Object o;
							try {
								o = cellContentConverter.getObject(resultSet, i);
								if (o instanceof byte[]) {
									final long length = ((byte[]) o).length;
									StringBuilder sb = new StringBuilder();
									int j;
									for (j = 0; j < length && j < 16; ++j) {
										byte b = ((byte[]) o)[j];
										sb.append(" ");
										sb.append(CellContentConverter.hexChar[(b >> 4) & 15]);
										sb.append(CellContentConverter.hexChar[b & 15]);
									}
									if (j < length) {
										sb.append("... " + length + " bytes");
									}
									final String content = sb.toString();
									final byte[] finalO = (byte[]) o;
									o = new BinValue() {
										@Override
										public String toString() {
											return "<Bin>" + content;
										}

										@Override
										public byte[] getContent() {
											return finalO;
										}
									};
								}
							} catch (Throwable e) {
								o = "ERROR: " + e.getClass().getName() + ": " + e.getMessage();
							}
							boolean isPK = false;
							if (pkColumnNames.isEmpty()) {
								isPK = type != Types.BLOB && type != Types.CLOB && type != Types.DATALINK && type != Types.JAVA_OBJECT && type != Types.NCLOB
										&& type != Types.NULL && type != Types.OTHER && type != Types.REF && type != Types.SQLXML && type != Types.STRUCT;
							}
							if (pkColumnNames.contains(quoting.requote(column.name)) || isPK) {
								String cVal = cellContentConverter.toSql(o instanceof BinValue? ((BinValue) o).getContent() : o);
								String pkValue = "B." + quoting.requote(column.name) + ("null".equalsIgnoreCase(cVal)? " is null" : ("=" + cVal));
								if (pkColumn != null) {
									pkColumn.put(column.name, pkValue);
								}
								if (pkColumnValue != null) {
									pkColumnValue.put(column.name, cVal);
								}
								rowId += (rowId.length() == 0 ? "" : " and ") + pkValue;
							}
							if (o == null || resultSet.wasNull()) {
								value = null;
							}
							if (o != null) {
								value = o;
							}
						}
					}
					v[vi] = value;
					return rowId;
				}
			};
			if (inputResultSet != null) {
				reader.init(inputResultSet);
				while (inputResultSet.next()) {
					reader.readCurrentRow(inputResultSet);
				}
				inputResultSet.close();
			}
			else {
				session.executeQuery(sql, reader, null, loadJob, limit);
			}
		}
	}

	/**
	 * True if row-limit is exceeded.
	 */
	private boolean isLimitExceeded = false;
	private boolean isClosureLimitExceeded = false;

	/**
	 * Show single row in special view?
	 */
	protected boolean noSingleRowDetailsView = false;
	protected String singleRowDetailsViewTitel = "Single Row Details";

	/**
	 * Parent having row-limit exceeded.
	 */
	private RowBrowser parentWithExceededLimit() {
		RowBrowser parent = getParentBrowser();

		while (parent != null) {
			if (parent.browserContentPane.isLimitExceeded) {
				return parent;
			}
			parent = parent.browserContentPane.getParentBrowser();
		}

		return null;
	}

	public static class TableModelItem {
		public final int blockNr;
		public final double inheritedBlockNumber;
		public final Object value;
		private String valueAsString = null;

		public TableModelItem(int blockNr, double inheritedBlockNumber, Object value) {
			this.blockNr = blockNr;
			this.inheritedBlockNumber = inheritedBlockNumber;
			this.value = value;
		}

		@Override
		public String toString() {
			if (valueAsString == null) {
				if (value instanceof Double) {
					valueAsString = SqlUtil.toString((Double) value);
				} else if (value instanceof BigDecimal) {
					valueAsString = SqlUtil.toString((BigDecimal) value);
				} else {
					valueAsString = String.valueOf(value);
				}
			}
			return valueAsString;
		}
	}

	private int lastLimit;
	private boolean lastLimitExceeded;
	private boolean lastClosureLimitExceeded;
	private boolean isUpdatingTableModel;
	private Set<Integer> foundColumn = new HashSet<Integer>();

	/**
	 * Updates the model of the {@link #rowsTable}.
	 *
	 * @param limit
	 *            row limit
	 * @param limitExceeded
	 */
	private void updateTableModel() {
		updateTableModel(lastLimit, lastLimitExceeded, lastClosureLimitExceeded);
	}

	/**
	 * Updates the model of the {@link #rowsTable}.
	 *
	 * @param limit
	 *            row limit
	 * @param limitExceeded
	 */
	private void updateTableModel(int limit, boolean limitExceeded, boolean closureLimitExceeded) {
		try {
			isUpdatingTableModel = true;
			doUpdateTableModel(limit, limitExceeded, closureLimitExceeded);
		} finally {
			isUpdatingTableModel = false;
		}
	}

	private boolean dontPaintSortIcon = false;

	private void doUpdateTableModel(int limit, boolean limitExceeded, boolean closureLimitExceeded) {
		lastLimit = limit;
		lastLimitExceeded = limitExceeded;
		lastClosureLimitExceeded = closureLimitExceeded;
		pkColumns.clear();
		List<Column> columns = rowIdSupport.getColumns(table, session);
		String[] columnNames = new String[columns.size()];
		final Set<String> pkColumnNames = new HashSet<String>();
		if (rowIdSupport.getPrimaryKey(table, session) != null) {
			for (Column pk : rowIdSupport.getPrimaryKey(table, session).getColumns()) {
				pkColumnNames.add(pk.name);
			}
		}
		for (int i = 0; i < columnNames.length; ++i) {
			columnNames[i] = columns.get(i).name;
			if ("".equals(columnNames[i])) {
				columnNames[i] = " ";
			}
			if (pkColumnNames.contains(columnNames[i])) {
				pkColumns.add(i);
			}
			if (columnNames[i] == null) {
				if (alternativeColumnLabels != null && i < alternativeColumnLabels.length) {
					columnNames[i] = alternativeColumnLabels[i];
				}
			}
		}

		fkColumns.clear();
		final Set<String> fkColumnNames = new HashSet<String>();
		for (Association a: table.associations) {
			if (a.isInsertDestinationBeforeSource()) {
				Map<Column, Column> m = a.createSourceToDestinationKeyMapping();
				for (Column fkColumn: m.keySet()) {
					fkColumnNames.add(fkColumn.name);
				}
			}
		}
		if (rowIdSupport.getPrimaryKey(table, session) != null) {
			for (Column pk : rowIdSupport.getPrimaryKey(table, session).getColumns()) {
				pkColumnNames.add(pk.name);
			}
		}
		for (int i = 0; i < columnNames.length; ++i) {
			if (fkColumnNames.contains(columnNames[i])) {
				fkColumns.add(i);
			}
		}

		DefaultTableModel dtm;
		findColumnsLabel.setEnabled(true);
		singleRowDetailsView = null;
		singleRowViewScrollPaneContainer.setVisible(false);
		rowsTableContainerPanel.setVisible(true);
		boolean noFilter = true;
		int rn = 0;
		if (rows.size() != 1 || isEditMode || noSingleRowDetailsView) {
			noFilter = false;
			Map<String, Integer> columnNameMap = new HashMap<String, Integer>();
			for (int i = 0; i < columns.size(); ++i) {
				columnNameMap.put(columnNames[i], i);
			}
			String[] uqColumnNames = new String[columnNames.length];
			for (int i = 0; i < uqColumnNames.length; ++i) {
				if (columnNames[i] != null) {
					uqColumnNames[i] = Quoting.staticUnquote(columnNames[i]);
				}
			}
			dtm = new DefaultTableModel(uqColumnNames, 0) {
				@Override
				public boolean isCellEditable(int row, int column) {
					Row r = null;
					if (row < rows.size()) {
						r = rows.get(row);
					}
					Table type = getResultSetTypeForColumn(column);
					if (isEditMode && r != null && (r.rowId != null && !r.rowId.isEmpty()) && browserContentCellEditor.isEditable(type, row, column, r.values[column]) && isPKComplete(type, r)) {
						return !rowIdSupport.getPrimaryKey(type, session).getColumns().isEmpty();
					}
					return false;
				}

				@Override
				public void setValueAt(Object aValue, int row, int column) {
					String text = aValue.toString();
					Row theRow = null;
					if (row < rows.size()) {
						theRow = rows.get(row);
						Object content = browserContentCellEditor.textToContent(row, column, text, theRow.values[column]);
						if (content != BrowserContentCellEditor.INVALID) {
							if (!browserContentCellEditor.cellContentToText(row, column, theRow.values[column]).equals(text)) {
								Table type = getResultSetTypeForColumn(column);
								if (resultSetType != null) {
									theRow = createRowWithNewID(theRow, type);
								}
								Object oldContent = theRow.values[column];
								theRow.values[column] = content;
								final String updateStatement = SQLDMLBuilder.buildUpdate(type, theRow, false, column, session);
								theRow.values[column] = oldContent;
								updateMode("updating", null);

								getRunnableQueue().add(new RunnableWithPriority() {
									private Exception exception;

									@Override
									public void run() {
										final Object context = new Object();
										ActionListener listener = new ActionListener() {
											@Override
											public void actionPerformed(ActionEvent e) {
												CancellationHandler.cancel(context);
											}
										};
										cancelLoadButton.addActionListener(listener);
										try {
											session.execute(updateStatement, context, false);
										} catch (Exception e) {
											exception = e;
										} finally {
											CancellationHandler.reset(context);
											cancelLoadButton.removeActionListener(listener);
										}
										UIUtil.invokeLater(new Runnable() {
											@Override
											public void run() {
												if (exception != null && !(exception instanceof CancellationException)) {
													UIUtil.showException(BrowserContentPane.this, "Error", exception);
													updateMode("table", null);
												} else {
													reloadRows();
												}
											}
										});
									}

									@Override
									public int getPriority() {
										return 50;
									}
								});
							}
						}
					}
				}
			};

			boolean stripHour[] = new boolean[columns.size()];
			final String HOUR = " 00:00:00.0";
			for (int i = 0; i < columns.size(); ++i) {
				stripHour[i] = true;
				for (Row row : rows) {
					Object value = row.values[i];
					if (value == null) {
						continue;
					}
					if (!(value instanceof java.sql.Date) && !(value instanceof java.sql.Timestamp)) {
						stripHour[i] = false;
						break;
					}
					String asString = value.toString();
					if (asString.endsWith(HOUR)) {
						continue;
					}
					stripHour[i] = false;
					break;
				}
			}

			for (Row row : rows) {
				Object[] rowData = new Object[columns.size()];
				for (int i = 0; i < columns.size(); ++i) {
					rowData[i] = row.values[i];
					if (rowData[i] instanceof PObjectWrapper) {
						rowData[i] = ((PObjectWrapper) rowData[i]).getValue();
					}
					if (rowData[i] == null) {
						rowData[i] = UIUtil.NULL;
					} else if (rowData[i] instanceof UnknownValue) {
						rowData[i] = UNKNOWN;
					}
					if (stripHour[i] && (rowData[i] instanceof java.sql.Date || rowData[i] instanceof java.sql.Timestamp)) {
						String asString = rowData[i].toString();
						int endIndex = asString.length() - HOUR.length();
						if (endIndex > 0) {
							rowData[i] = asString.substring(0, endIndex);
						}
					}
				}
				if (tableContentViewFilter != null) {
					tableContentViewFilter.filter(rowData, columnNameMap);
				}
				for (int i = 0; i < columns.size(); ++i) {
					TableModelItem item = new TableModelItem(row.getParentModelIndex(), row.getInheritedParentModelIndex(), rowData[i]);
					rowData[i] = item;
				}
				dtm.addRow(rowData);
				if (++rn >= limit) {
					break;
				}
			}

			//set the editor as default on every column
			JTextField textField = new JTextField();
		    textField.setBorder(new LineBorder(Color.black));
			DefaultCellEditor anEditor = new DefaultCellEditor(textField) {
				@Override
				public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column) {
					if (table != rowsTable) {
						column = table.convertColumnIndexToModel(column);
						if (column == 0) {
							return null;
						}
						int h = row;
						row = column - 1;
						column = h;
					}
					if (row < rows.size()) {
						Row r = rows.get(rowsTable.getRowSorter().convertRowIndexToModel(row));
						int convertedColumnIndex = rowsTable.convertColumnIndexToModel(column);
						if (r != null) {
							value = browserContentCellEditor.cellContentToText(row, convertedColumnIndex, r.values[convertedColumnIndex]);
						}
					}
					return super.getTableCellEditorComponent(table, value, isSelected, row, column);
				}
			};
			anEditor.setClickCountToStart(1);
			for (int i = 0; i < rowsTable.getColumnCount(); i++) {
			   rowsTable.setDefaultEditor(rowsTable.getColumnClass(i), anEditor);
		    }

			List<? extends SortKey> sortKeys = new ArrayList(rowsTable.getRowSorter().getSortKeys());
			List<Object> filterContent = new ArrayList<Object>();
			if (filterHeader != null) {
				try {
					for (int i = 0; i < rowsTable.getColumnCount(); ++i) {
						filterContent.add(filterHeader.getFilterEditor(i).getContent());
					}
				} catch (Exception e) {
					// ignore
				}
			}

			rowsTable.setModel(dtm);
			rowsTable.getSelectionModel().clearSelection();
			rowsTable.setRowHeight(initialRowHeight);

			noRowsFoundPanel.setVisible(dtm.getRowCount() == 0 && getAndConditionText().length() > 0);

			final int defaultSortColumn = getDefaultSortColumn();

			TableRowSorter<TableModel> sorter = new TableRowSorter<TableModel>(dtm) {
				@Override
				protected boolean useToString(int column) {
					return false;
				}

				@Override
			    public List<? extends SortKey> getSortKeys() {
					if (dontPaintSortIcon) {
						return new ArrayList<SortKey>();
					}
			        return super.getSortKeys();
			    }

				@Override
			    public void toggleSortOrder(int column) {
					if (association != null && association.isInsertDestinationBeforeSource()) {
						if (!useInheritedBlockNumbers) {
							return;
						}
					}
					ignoreSortKey = false;
					boolean toggled = false;
			        List<? extends SortKey> sortKeys = getSortKeys();
			        if (sortKeys.size() > 0) {
			            if (sortKeys.get(0).getColumn() == column && sortKeys.get(0).getSortOrder() == SortOrder.DESCENDING) {
			                List<SortKey> sk = new ArrayList<SortKey>();
			                if (defaultSortColumn >= 0) {
			                	sk.add(new SortKey(defaultSortColumn, SortOrder.ASCENDING));
			                }
							setSortKeys(sk);
			                toggled = true;
			            }
			        }
			        if (!toggled) {
			        	super.toggleSortOrder(column);
			        }
					UIUtil.invokeLater(new Runnable() {
						@Override
						public void run() {
							sortChildren();
						}
					});
			    }

				@Override
				public Comparator<?> getComparator(int n) {
					List<? extends SortKey> sortKeys = super.getSortKeys();
					final boolean desc = sortKeys.size() > 0 && sortKeys.get(0).getSortOrder() == SortOrder.DESCENDING;

					return new Comparator<Object>() {
						@SuppressWarnings("unchecked")
						@Override
						public int compare(Object o1, Object o2) {
							RowSorter pSorter = null;
							RowSorter ppSorter = null;
							RowBrowser pb = getParentBrowser();
							if (pb != null) {
								if (pb.browserContentPane != null) {
									if (pb.browserContentPane.rowsTable != null) {
										pSorter = pb.browserContentPane.rowsTable.getRowSorter();
										if (pb.browserContentPane.getParentBrowser() != null) {
											if (pb.browserContentPane.getParentBrowser().browserContentPane != null) {
												if (pb.browserContentPane.getParentBrowser().browserContentPane.rowsTable != null) {
													ppSorter = pb.browserContentPane.getParentBrowser().browserContentPane.rowsTable.getRowSorter();
												}
											}
										}
									}
								}
							}
							if (o1 instanceof TableModelItem && o2 instanceof TableModelItem) {
								if (useInheritedBlockNumbers && !ignoreSortKey) {
									double b1 = ((TableModelItem) o1).inheritedBlockNumber;
									double b2 = ((TableModelItem) o2).inheritedBlockNumber;

									if (ppSorter != null) {
										double b;
										b = b1 < ppSorter.getModelRowCount()? ppSorter.convertRowIndexToView((int) b1) : -1;
										if (b < 0) {
											b = b1 + Integer.MAX_VALUE / 2;
										}
										b1 = b;
										b = b2 < ppSorter.getModelRowCount()? ppSorter.convertRowIndexToView((int) b2) : -1;
										if (b < 0) {
											b = b2 + Integer.MAX_VALUE / 2;
										}
										b2 = b;
									}
									if (b1 != b2) {
										return (int) ((b1 - b2) * (desc? -1 : 1));
									}
								} else {
									int b1 = ((TableModelItem) o1).blockNr;
									int b2 = ((TableModelItem) o2).blockNr;

									if (pSorter != null) {
										int b;
										b = b1 < pSorter.getModelRowCount()? pSorter.convertRowIndexToView(b1) : -1;
										if (b < 0) {
											b = b1 + Integer.MAX_VALUE / 2;
										}
										b1 = b;
										b = b2 < pSorter.getModelRowCount()? pSorter.convertRowIndexToView(b2) : -1;
										if (b < 0) {
											b = b2 + Integer.MAX_VALUE / 2;
										}
										b2 = b;
									}
									if (b1 != b2) {
										return (b1 - b2) * (desc? -1 : 1);
									}
								}
							}

							if (ignoreSortKey) {
								return 0;
							}

							if (o1 instanceof TableModelItem) {
								o1 = ((TableModelItem) o1).value;
							}
							if (o2 instanceof TableModelItem) {
								o2 = ((TableModelItem) o2).value;
							}
							if (o1 == UIUtil.NULL) {
								o1 = null;
							}
							if (o2 == UIUtil.NULL) {
								o2 = null;
							}
							if (o1 == null && o2 == null) {
								return 0;
							}
							if (o1 == null) {
								return 1;
							}
							if (o2 == null) {
								return -1;
							}
							if (o1.getClass().equals(o2.getClass())) {
								if (o1 instanceof Comparable<?>) {
									return ((Comparable<Object>) o1).compareTo(o2);
								}
								return 0;
							}
							return o1.getClass().getName().compareTo(o2.getClass().getName());
						}
					};
				}
			};
			sorter.addRowSorterListener(new RowSorterListener() {
				@Override
				public void sorterChanged(RowSorterEvent e) {
					if (e.getType() == RowSorterEvent.Type.SORTED) {
						List<RowBrowser> chBrs = getChildBrowsers();
						if (chBrs != null) {
							for (RowBrowser chBr: chBrs) {
								if (chBr.browserContentPane != null) {
									if (chBr.browserContentPane.rowsTable != null) {
										final RowSorter chSorter = chBr.browserContentPane.rowsTable.getRowSorter();
										if (chSorter instanceof TableRowSorter) {
											UIUtil.invokeLater(new Runnable() {
												@Override
												public void run() {
													((TableRowSorter) chSorter).sort();
												}
											});
										}
									}
								}
							}
						}
					}
				}
			});
			rowsTable.setRowSorter(sorter);
			try {
				if (!sortKeys.isEmpty()) {
					rowsTable.getRowSorter().setSortKeys(sortKeys);
				} else {
					List<SortKey> sk = new ArrayList<SortKey>();
					if (defaultSortColumn >= 0) {
						sk.add(new SortKey(defaultSortColumn, SortOrder.ASCENDING));
					}
					rowsTable.getRowSorter().setSortKeys(sk);
				}
				if (filterHeader != null) {
					for (int i = 0; i < rowsTable.getColumnCount(); ++i) {
						filterHeader.getFilterEditor(i).setContent(filterContent.get(i));
					}
				}
			} catch (Exception e) {
				// ignore
			}
		} else {
			final boolean deselect = !getAndConditionText().equals("")
				&& rows.size() == 1;
			singleRowDetailsView = new DetailsView(Collections.singletonList(rows.get(0)), 1, dataModel, BrowserContentPane.this.table, 0, null, false, false, rowIdSupport, deselect, session) {
				@Override
				protected void onRowChanged(int row) {
				}
				@Override
				protected void onClose() {
				}
				@Override
				protected void onSelectRow(Row row) {
				}
			};
			singleRowDetailsView.setSortColumns(sortColumnsCheckBox.isSelected());
	        dtm = new DefaultTableModel(new String[] { singleRowDetailsViewTitel }, 0) {
				@Override
				public boolean isCellEditable(int row, int column) {
					return false;
				}
			};
			findColumnsLabel.setEnabled(false);

			for (Row row : rows) {
				dtm.addRow(new Object[] { row });
				if (++rn >= limit) {
					break;
				}
			}
			rowsTable.setModel(dtm);

			JPanel detailsPanel = new JPanel(new java.awt.BorderLayout(0, 0)) {
				@Override
				public void paint(Graphics graphics) {
					super.paint(graphics);
					if (!(graphics instanceof Graphics2D)) {
						return;
					}
					if (rows.size() == 1 && BrowserContentPane.this.rowsClosure.tempClosure.size() > 1 && BrowserContentPane.this.rowsClosure.tempClosure.contains(rows.get(0))) {
						Rectangle visRect = singleRowViewContainterPanel.getVisibleRect();

						Graphics2D g2d = (Graphics2D) graphics;
						g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
						g2d.setStroke(new BasicStroke(1));

						int width = (int) (visRect.width * 1.4);

						g2d.setColor(new Color(255, 0, 0, 20));
						int[] x = new int[2];
						int[] y = new int[2];
						x[0] = (int) visRect.getMinX();
						y[0] = (int) visRect.getMinY();
						x[1] = (int) visRect.getMinX() + width;
						y[1] = (int) visRect.getMaxY();
						GradientPaint paint = new GradientPaint(
								x[0], y[0], new Color(255, 0, 0, 20),
								x[0] + width, y[1], new Color(255, 0, 0, 0));
						g2d.setPaint(paint);
						g2d.fillRect(x[0], y[0], x[1] - x[0], y[1] - y[0]);
						g2d.setPaint(null);
					}
				}
			};
			detailsPanel.add(singleRowDetailsView.getDetailsPanel(), java.awt.BorderLayout.CENTER);
			GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 1;
	        gridBagConstraints.gridy = 1;
	        gridBagConstraints.weightx = 1;
	        gridBagConstraints.weighty = 0;
	        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
	        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
	        singleRowViewContainterPanel.removeAll();
			singleRowViewContainterPanel.add(detailsPanel, gridBagConstraints);

			singleRowViewScrollPaneContainer.setVisible(true);
			rowsTableContainerPanel.setVisible(false);
			deselectButton.setVisible(deselect);
		}

		adjustRowTableColumnsWidth();

		if (sortColumnsCheckBox.isSelected()) {
			TableColumnModel cm = rowsTable.getColumnModel();
			for (int a = 0; a < rowsTable.getColumnCount(); ++a) {
				for (int b = a + 1; b < rowsTable.getColumnCount(); ++b) {
					if (cm.getColumn(a).getHeaderValue().toString().compareToIgnoreCase(cm.getColumn(b).getHeaderValue().toString()) > 0) {
						cm.moveColumn(b, a);
					}
				}
			}
		}

		rowsTable.setIntercellSpacing(new Dimension(0, 0));
		Set<BrowserContentPane> browserInClosure = new HashSet<BrowserContentPane>();
		for (Pair<BrowserContentPane, Row> rid: rowsClosure.currentClosure) {
			browserInClosure.add(rid.a);
		}
		updateRowsCountLabel(browserInClosure);

		final TableCellRenderer origRenderer = rowsTable.getTableHeader().getDefaultRenderer();
		rowsTable.getTableHeader().setDefaultRenderer(new TableCellRenderer() {
			@Override
			public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected,
					boolean hasFocus, int row, int column) {
				try {
					dontPaintSortIcon = ignoreSortKey;
					return origRenderer.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
				} finally {
					dontPaintSortIcon = false;
				}
			}
		});

		int nndr = noNonDistinctRows;
		if (noDistinctRows + noNonDistinctRows >= limit) {
			--nndr;
		}
		selectDistinctCheckBox.setVisible(nndr > 0);
		selectDistinctCheckBox.setText("select distinct (-" + nndr + ")");
		if (getParentBrowser() != null) {
			BrowserContentPane pBrowser = getParentBrowser().browserContentPane;
			if (pBrowser.selectDistinctCheckBox.isVisible() && !pBrowser.selectDistinctCheckBox.isSelected()) {
				if (selectDistinctCheckBox.isSelected()) {
					selectDistinctCheckBox.setVisible(false);
				}
			}
		}

		if (filterHeader != null) {
			if (rowsTable.getRowSorter() != null && rowsTable.getRowSorter().getViewRowCount() == 0) {
				filterHeader.setTable(null);
				filterHeader = null;
				adjustRowTableColumnsWidth();
			}
		}

		if (isTableFilterEnabled && !noFilter) {
			if (filterHeader == null) {
				filterHeader = new TableFilterHeader();
				filterHeader.setRowHeightDelta(2);
				filterHeader.setAutoChoices(AutoChoices.ENABLED);
				filterHeader.setTable(rowsTable);
				filterHeader.setMaxVisibleRows(20);
				try {
					for (int i = 0; i < rowsTable.getColumnCount(); ++i) {
						filterHeader.getFilterEditor(i).setChoicesComparator(new Comparator<Object>() {
							@SuppressWarnings("unchecked")
							@Override
							public int compare(Object o1, Object o2) {
								if (o1 instanceof TableModelItem) {
									o1 = ((TableModelItem) o1).value;
								}
								if (o2 instanceof TableModelItem) {
									o2 = ((TableModelItem) o2).value;
								}
								if (o1 == null && o2 == null) {
									return 0;
								}
								if (o1 == null) {
									return -1;
								}
								if (o2 == null) {
									return 1;
								}
								if (o1.getClass().equals(o2.getClass())) {
									if (o1 instanceof Comparable<?>) {
										return ((Comparable<Object>) o1).compareTo(o2);
									}
									return 0;
								}
								return o1.getClass().getName().compareTo(o2.getClass().getName());
							}
						});
					}
				}
				catch (Exception e) {
					// ignore
				}
			}
		} else {
			if (filterHeader != null) {
				filterHeader.setTable(null);
				filterHeader = null;
			}
		}

		isLimitExceeded = limitExceeded;
		isClosureLimitExceeded = closureLimitExceeded;
		appendClosure();
	}

	public void updateRowsCountLabel(Set<BrowserContentPane> browserInClosure) {
		int limit = lastLimit;
		boolean limitExceeded = lastLimitExceeded;
		boolean closureLimitExceeded = lastClosureLimitExceeded;

		int size = rows.size();
		if (size > limit) {
			size = limit;
		}
		rowsCount.setText((limitExceeded ? " more than " : " ") + size + " row" + (size != 1 ? "s" : ""));
		RowBrowser theParentWithExceededLimit = parentWithExceededLimit();
		boolean cle = closureLimitExceeded;
		boolean cleRelevant = true;
//		if (theParentWithExceededLimit != null && theParentWithExceededLimit.browserContentPane.isClosureLimitExceeded) {
//			cle = true;
//		}
		if (rowsClosure.parentPath.contains(this) || rowsClosure.currentClosureRowIDs.isEmpty()) {
			cleRelevant = false;
		} else if (getParentBrowser() != null) {
			BrowserContentPane parent = getParentBrowser().browserContentPane;
			if (!browserInClosure.contains(parent)) {
				cleRelevant = false;
			}
		}
		boolean bold = false;
		if (limitExceeded || theParentWithExceededLimit != null) {
			if (cle || !cleRelevant) {
				rowsCount.setForeground(Color.RED);
				bold = true;
			} else {
				rowsCount.setForeground(new Color(140, 0, 0));
			}
		} else {
			rowsCount.setForeground(new JLabel().getForeground());
		}
		if (bold) {
			rowsCount.setFont(rowsCount.getFont().deriveFont(rowsCount.getFont().getStyle() | Font.BOLD));
		} else {
			rowsCount.setFont(rowsCount.getFont().deriveFont(rowsCount.getFont().getStyle() & ~Font.BOLD));
		}

		if (cle && cleRelevant) {
			rowsCount.setToolTipText("row selection incomplete");
		} else if (!limitExceeded && theParentWithExceededLimit != null) {
			rowsCount.setToolTipText("potentially incomplete because " + theParentWithExceededLimit.internalFrame.getTitle() + " exceeded row limit");
		} else {
			rowsCount.setToolTipText(null);
		}
	}

	private int getDefaultSortColumn() {
		if (table == null || table instanceof SqlStatementTable || getQueryBuilderDialog() == null /* SQL Console */) {
			return -1;
		}
		if (association != null && association.isInsertDestinationBeforeSource()) {
			return -1;
		}
		if (table.primaryKey.getColumns() != null && table.primaryKey.getColumns().size() > 0) {
			Column pk = table.primaryKey.getColumns().get(0);
			for (int i = 0; i < table.getColumns().size(); ++i) {
				if (table.getColumns().get(i).equals(pk)) {
					return i;
				}
			}
		}
		return 0;
	}

	private TableFilterHeader filterHeader;

	protected Row createRowWithNewID(Row theRow, Table type) {
		CellContentConverter cellContentConverter = new CellContentConverter(null, session, session.dbms);
		String rowId = "";
		PrimaryKey primaryKeys = type.primaryKey;
		for (Column pkColumn : primaryKeys.getColumns()) {
			List<Column> cols = type.getColumns();
			int colSize = cols.size();
			for (int i = 0; i < colSize; ++i) {
				Column column = cols.get(i);
				if (column != null && pkColumn.name.equals(column.name)) {
					if (rowId.length() > 0) {
						rowId += " and ";
					}
					rowId += pkColumn.name + "=" + cellContentConverter.toSql(theRow.values[i]);
					break;
				}
			}
		}
		return new Row(rowId, theRow.primaryKey, theRow.values);
	}

	public void adjustRowTableColumnsWidth() {
		DefaultTableModel dtm = (DefaultTableModel) rowsTable.getModel();
		int MAXLINES = 400;
		if (rowsTable.getColumnCount() > 0) {
			MAXLINES = Math.max(10 * MAXLINES / rowsTable.getColumnCount(), 10);
		}
		DefaultTableCellRenderer defaultTableCellRenderer = new DefaultTableCellRenderer();
		int minTotalWidth = (int) (Desktop.BROWSERTABLE_DEFAULT_WIDTH * getLayoutFactor()) - 18;
		int totalWidth = 0;
		final int maxColumnWidth = 300;

		for (int i = 0; i < rowsTable.getColumnCount(); i++) {
			TableColumn column = rowsTable.getColumnModel().getColumn(i);
			int width = minTotalWidth / rowsTable.getColumnCount();

			Component comp = defaultTableCellRenderer.getTableCellRendererComponent(rowsTable, column.getHeaderValue(), false, false, 0, i);
			int pw = comp.getPreferredSize().width;
			if (pw < 100) {
				pw = (pw * 110) / 100 + 2;
			}
			width = Math.min(Math.max(width, pw), 150);

			int line = 0;
			for (; line < rowsTable.getRowCount(); ++line) {
				comp = rowsTable.getCellRenderer(line, i).getTableCellRendererComponent(rowsTable, dtm.getValueAt(line, i), false, false, line, i);
				width = Math.max(width, comp.getPreferredSize().width + 24);
				if (line > MAXLINES) {
					break;
				}
			}
			Object maxValue = null;
			int maxValueLength = 0;
			for (; line < rowsTable.getRowCount(); ++line) {
				Object value = dtm.getValueAt(line, i);
				if (value != null) {
					int valueLength = value.toString().length();
					if (maxValue == null || maxValueLength < valueLength) {
						maxValue = value;
						maxValueLength = valueLength;
					}
				}
				if (line > 4 * MAXLINES) {
					break;
				}
			}
			if (maxValue != null) {
				comp = rowsTable.getCellRenderer(line, i).getTableCellRendererComponent(rowsTable, maxValue, false, false, line, i);
				int maxValueWidth = comp.getPreferredSize().width + 16;
				if (maxValueWidth > width) {
					width = maxValueWidth;
				}
			}

			column.setPreferredWidth(width);
			totalWidth += width;
			if (i == rowsTable.getColumnCount() - 1) {
				if (totalWidth < minTotalWidth) {
					width = width + minTotalWidth - totalWidth;
				}
			}
			column.setPreferredWidth(Math.min(maxColumnWidth, width));
		}
	}

	/**
	 * This method is called from within the constructor to initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is always
	 * regenerated by the Form Editor.
	 */
	// <editor-fold defaultstate="collapsed"
	// <editor-fold defaultstate="collapsed"
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        andCondition = new javax.swing.JComboBox();
        openEditorLabel = new javax.swing.JLabel();
        pendingNonpendingPanel = new javax.swing.JPanel();
        cardPanel = new javax.swing.JPanel();
        tablePanel = new javax.swing.JPanel();
        jLayeredPane1 = new javax.swing.JLayeredPane();
        statusPanel = new javax.swing.JPanel();
        sortColumnsCheckBox = new javax.swing.JCheckBox();
        rowsCount = new javax.swing.JLabel();
        selectDistinctCheckBox = new javax.swing.JCheckBox();
        sortColumnsPanel = new javax.swing.JPanel();
        sortColumnsLabel = new javax.swing.JLabel();
        findColumnsPanel = new javax.swing.JPanel();
        findColumnsLabel = new javax.swing.JLabel();
        loadingPanel = new javax.swing.JPanel();
        jPanel1 = new javax.swing.JPanel();
        cancelLoadButton = new javax.swing.JButton();
        jLabel2 = new javax.swing.JLabel();
        jPanel13 = new javax.swing.JPanel();
        loadingCauseLabel = new javax.swing.JLabel();
        loadingLabel = new javax.swing.JLabel();
        jPanel2 = new javax.swing.JPanel();
        rowsTableContainerPanel = new javax.swing.JPanel();
        jLayeredPane2 = new javax.swing.JLayeredPane();
        rowsTableScrollPane = new javax.swing.JScrollPane();
        rowsTable = new javax.swing.JTable();
        noRowsFoundPanel = new javax.swing.JPanel();
        jLabel9 = new javax.swing.JLabel();
        removeConditionButton = new javax.swing.JButton();
        singleRowViewScrollPaneContainer = new javax.swing.JPanel();
        singleRowViewScrollPane = new javax.swing.JScrollPane();
        singleRowViewScrollContentPanel = new javax.swing.JPanel();
        singleRowViewContainterPanel = new javax.swing.JPanel();
        jPanel12 = new javax.swing.JPanel();
        jPanel11 = new javax.swing.JPanel();
        jLabel7 = new javax.swing.JLabel();
        deselectButton = new javax.swing.JButton();
        jPanel5 = new javax.swing.JPanel();
        errorLabel = new javax.swing.JLabel();
        errorDetailsButton = new javax.swing.JButton();
        jScrollPane2 = new javax.swing.JScrollPane();
        errorMessageTextArea = new javax.swing.JTextArea();
        jPanel4 = new javax.swing.JPanel();
        jLabel8 = new javax.swing.JLabel();
        jPanel8 = new javax.swing.JPanel();
        jLabel11 = new javax.swing.JLabel();
        menuPanel = new javax.swing.JPanel();
        jPanel7 = new javax.swing.JPanel();
        loadButton = new javax.swing.JButton();
        openEditorButton = new javax.swing.JToggleButton();
        onPanel = new javax.swing.JPanel();
        on = new javax.swing.JLabel();
        joinPanel = new javax.swing.JPanel();
        join = new javax.swing.JLabel();
        jLabel6 = new javax.swing.JLabel();
        jPanel10 = new javax.swing.JPanel();
        from = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        jLabel1 = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        rrPanel = new javax.swing.JPanel();
        relatedRowsPanel = new javax.swing.JPanel();
        relatedRowsLabel = new javax.swing.JLabel();
        sqlPanel = new javax.swing.JPanel();
        sqlLabel1 = new javax.swing.JLabel();
        jPanel9 = new javax.swing.JPanel();
        jPanel14 = new javax.swing.JPanel();
        wherejLabel = new javax.swing.JLabel();

        andCondition.setEditable(true);
        andCondition.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));

        openEditorLabel.setText(" And  ");

        setLayout(new java.awt.GridBagLayout());

        pendingNonpendingPanel.setLayout(new java.awt.CardLayout());

        cardPanel.setLayout(new java.awt.CardLayout());

        tablePanel.setLayout(new java.awt.GridBagLayout());

        jLayeredPane1.setLayout(new java.awt.GridBagLayout());

        statusPanel.setLayout(new java.awt.GridBagLayout());

        sortColumnsCheckBox.setText("sort columns   ");
        sortColumnsCheckBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                sortColumnsCheckBoxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 0);
        statusPanel.add(sortColumnsCheckBox, gridBagConstraints);

        rowsCount.setText("jLabel3");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        statusPanel.add(rowsCount, gridBagConstraints);

        selectDistinctCheckBox.setSelected(true);
        selectDistinctCheckBox.setText("select distinct (-100 rows)");
        selectDistinctCheckBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                selectDistinctCheckBoxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 4);
        statusPanel.add(selectDistinctCheckBox, gridBagConstraints);

        sortColumnsPanel.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
        sortColumnsPanel.setLayout(new javax.swing.BoxLayout(sortColumnsPanel, javax.swing.BoxLayout.LINE_AXIS));

        sortColumnsLabel.setText("Natural column order ");
        sortColumnsPanel.add(sortColumnsLabel);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 5;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 2);
        statusPanel.add(sortColumnsPanel, gridBagConstraints);

        findColumnsPanel.setLayout(new java.awt.GridBagLayout());

        findColumnsLabel.setText("Find Columns");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 6);
        findColumnsPanel.add(findColumnsLabel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 2);
        statusPanel.add(findColumnsPanel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTH;
        jLayeredPane1.add(statusPanel, gridBagConstraints);

        loadingPanel.setOpaque(false);
        loadingPanel.setLayout(new java.awt.GridBagLayout());

        jPanel1.setBackground(new Color(255,255,255,150));
        jPanel1.setLayout(new java.awt.GridBagLayout());

        cancelLoadButton.setText("Cancel");
        cancelLoadButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelLoadButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel1.add(cancelLoadButton, gridBagConstraints);

        jLabel2.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(jLabel2, gridBagConstraints);

        jPanel13.setOpaque(false);
        jPanel13.setLayout(new java.awt.GridBagLayout());

        loadingCauseLabel.setFont(loadingCauseLabel.getFont().deriveFont(loadingCauseLabel.getFont().getSize()+3f));
        loadingCauseLabel.setForeground(new java.awt.Color(141, 16, 16));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel13.add(loadingCauseLabel, gridBagConstraints);

        loadingLabel.setFont(loadingLabel.getFont().deriveFont(loadingLabel.getFont().getStyle() | java.awt.Font.BOLD, loadingLabel.getFont().getSize()+3));
        loadingLabel.setForeground(new java.awt.Color(141, 16, 16));
        loadingLabel.setText("loading...     ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel13.add(loadingLabel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(8, 8, 8, 8);
        jPanel1.add(jPanel13, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 2, 4);
        loadingPanel.add(jPanel1, gridBagConstraints);

        jLayeredPane1.setLayer(loadingPanel, javax.swing.JLayeredPane.MODAL_LAYER);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridheight = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        jLayeredPane1.add(loadingPanel, gridBagConstraints);

        jPanel2.setLayout(new java.awt.GridBagLayout());

        rowsTableContainerPanel.setLayout(new java.awt.GridBagLayout());

        jLayeredPane2.setLayout(new java.awt.GridBagLayout());

        rowsTableScrollPane.setWheelScrollingEnabled(false);

        rowsTable.setAutoCreateRowSorter(true);
        rowsTable.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null}
            },
            new String [] {
                "Title 1", "Title 2", "Title 3", "Title 4"
            }
        ));
        rowsTable.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
        rowsTableScrollPane.setViewportView(rowsTable);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jLayeredPane2.add(rowsTableScrollPane, gridBagConstraints);

        noRowsFoundPanel.setOpaque(false);
        noRowsFoundPanel.setLayout(new java.awt.GridBagLayout());

        jLabel9.setText("No rows found.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTH;
        gridBagConstraints.weighty = 1.0;
        noRowsFoundPanel.add(jLabel9, gridBagConstraints);

        removeConditionButton.setText("Remove condition");
        removeConditionButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                removeConditionButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
        gridBagConstraints.weighty = 1.0;
        noRowsFoundPanel.add(removeConditionButton, gridBagConstraints);

        jLayeredPane2.setLayer(noRowsFoundPanel, javax.swing.JLayeredPane.PALETTE_LAYER);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jLayeredPane2.add(noRowsFoundPanel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        rowsTableContainerPanel.add(jLayeredPane2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel2.add(rowsTableContainerPanel, gridBagConstraints);

        singleRowViewScrollPaneContainer.setLayout(new java.awt.GridBagLayout());

        singleRowViewScrollPane.setWheelScrollingEnabled(false);

        singleRowViewScrollContentPanel.setLayout(new java.awt.GridBagLayout());

        singleRowViewContainterPanel.setBackground(java.awt.Color.white);
        singleRowViewContainterPanel.setBorder(new javax.swing.border.LineBorder(java.awt.Color.gray, 1, true));
        singleRowViewContainterPanel.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
        gridBagConstraints.weightx = 1.0;
        singleRowViewScrollContentPanel.add(singleRowViewContainterPanel, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.weighty = 1.0;
        singleRowViewScrollContentPanel.add(jPanel12, gridBagConstraints);

        singleRowViewScrollPane.setViewportView(singleRowViewScrollContentPanel);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        singleRowViewScrollPaneContainer.add(singleRowViewScrollPane, gridBagConstraints);

        jPanel11.setBackground(new java.awt.Color(228, 228, 232));
        jPanel11.setLayout(new java.awt.GridBagLayout());

        jLabel7.setBackground(new java.awt.Color(200, 200, 200));
        jLabel7.setForeground(new java.awt.Color(1, 0, 0));
        jLabel7.setText(" Single Row Details ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.VERTICAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHWEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 0, 2);
        jPanel11.add(jLabel7, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.VERTICAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        singleRowViewScrollPaneContainer.add(jPanel11, gridBagConstraints);

        deselectButton.setText("Deselect Row");
        deselectButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                deselectButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHEAST;
        singleRowViewScrollPaneContainer.add(deselectButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel2.add(singleRowViewScrollPaneContainer, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jLayeredPane1.add(jPanel2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        tablePanel.add(jLayeredPane1, gridBagConstraints);

        cardPanel.add(tablePanel, "table");

        jPanel5.setLayout(new java.awt.GridBagLayout());

        errorLabel.setForeground(new java.awt.Color(141, 16, 16));
        errorLabel.setText("Error");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(8, 8, 8, 8);
        jPanel5.add(errorLabel, gridBagConstraints);

        errorDetailsButton.setText("Details...");
        errorDetailsButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                errorDetailsButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        jPanel5.add(errorDetailsButton, gridBagConstraints);

        jScrollPane2.setHorizontalScrollBarPolicy(javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);

        errorMessageTextArea.setEditable(false);
        errorMessageTextArea.setColumns(20);
        errorMessageTextArea.setLineWrap(true);
        errorMessageTextArea.setWrapStyleWord(true);
        jScrollPane2.setViewportView(errorMessageTextArea);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        jPanel5.add(jScrollPane2, gridBagConstraints);

        cardPanel.add(jPanel5, "error");

        jPanel4.setLayout(new java.awt.GridBagLayout());

        jLabel8.setFont(jLabel8.getFont().deriveFont(jLabel8.getFont().getStyle() | java.awt.Font.BOLD, jLabel8.getFont().getSize()+3));
        jLabel8.setForeground(new java.awt.Color(141, 16, 16));
        jLabel8.setText("Cancelled");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(8, 8, 8, 0);
        jPanel4.add(jLabel8, gridBagConstraints);

        cardPanel.add(jPanel4, "cancelled");

        pendingNonpendingPanel.add(cardPanel, "nonpending");

        jPanel8.setLayout(new java.awt.GridBagLayout());

        jLabel11.setFont(jLabel11.getFont().deriveFont(jLabel11.getFont().getStyle() | java.awt.Font.BOLD, jLabel11.getFont().getSize()+3));
        jLabel11.setForeground(new java.awt.Color(141, 16, 16));
        jLabel11.setText("pending...");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(8, 8, 8, 0);
        jPanel8.add(jLabel11, gridBagConstraints);

        pendingNonpendingPanel.add(jPanel8, "pending");

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(pendingNonpendingPanel, gridBagConstraints);

        menuPanel.setLayout(new java.awt.GridBagLayout());

        jPanel7.setLayout(new java.awt.GridBagLayout());

        loadButton.setText("Reload");
        loadButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                loadButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        jPanel7.add(loadButton, gridBagConstraints);

        openEditorButton.setText("Where");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        jPanel7.add(openEditorButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.gridwidth = 6;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        menuPanel.add(jPanel7, gridBagConstraints);

        onPanel.setMinimumSize(new java.awt.Dimension(66, 17));
        onPanel.setLayout(new java.awt.BorderLayout());

        on.setFont(on.getFont().deriveFont(on.getFont().getStyle() | java.awt.Font.BOLD));
        on.setText("jLabel3");
        onPanel.add(on, java.awt.BorderLayout.CENTER);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        menuPanel.add(onPanel, gridBagConstraints);

        joinPanel.setMinimumSize(new java.awt.Dimension(66, 17));
        joinPanel.setLayout(new java.awt.GridBagLayout());

        join.setFont(join.getFont().deriveFont(join.getFont().getStyle() | java.awt.Font.BOLD));
        join.setText("jLabel3");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        joinPanel.add(join, gridBagConstraints);

        jLabel6.setText(" as B  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 0);
        joinPanel.add(jLabel6, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        menuPanel.add(joinPanel, gridBagConstraints);

        jPanel10.setMinimumSize(new java.awt.Dimension(66, 17));
        jPanel10.setLayout(new java.awt.GridBagLayout());

        from.setFont(from.getFont().deriveFont(from.getFont().getStyle() | java.awt.Font.BOLD));
        from.setText("jLabel3");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        jPanel10.add(from, gridBagConstraints);

        jLabel5.setHorizontalAlignment(javax.swing.SwingConstants.LEFT);
        jLabel5.setText(" as A");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 0);
        jPanel10.add(jLabel5, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        menuPanel.add(jPanel10, gridBagConstraints);

        jLabel1.setText(" Join ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weighty = 1.0;
        menuPanel.add(jLabel1, gridBagConstraints);

        jLabel4.setText(" On ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weighty = 1.0;
        menuPanel.add(jLabel4, gridBagConstraints);

        jLabel3.setText(" From ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weighty = 1.0;
        menuPanel.add(jLabel3, gridBagConstraints);

        rrPanel.setLayout(new java.awt.GridBagLayout());

        relatedRowsPanel.setBackground(new java.awt.Color(224, 240, 255));
        relatedRowsPanel.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
        relatedRowsPanel.setLayout(new java.awt.GridBagLayout());

        relatedRowsLabel.setBackground(new java.awt.Color(224, 240, 255));
        relatedRowsLabel.setText(" Related Rows ");
        relatedRowsLabel.setOpaque(true);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(1, 3, 1, 3);
        relatedRowsPanel.add(relatedRowsLabel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 7;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 2, 0);
        rrPanel.add(relatedRowsPanel, gridBagConstraints);

        sqlPanel.setBackground(new java.awt.Color(255, 243, 218));
        sqlPanel.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
        sqlPanel.setLayout(new java.awt.GridBagLayout());

        sqlLabel1.setForeground(new java.awt.Color(1, 0, 0));
        sqlLabel1.setText("  Menu  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.insets = new java.awt.Insets(1, 3, 1, 3);
        sqlPanel.add(sqlLabel1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 8;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHEAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 0);
        rrPanel.add(sqlPanel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 7;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.gridheight = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 2);
        menuPanel.add(rrPanel, gridBagConstraints);

        jPanel9.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 5;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 0);
        menuPanel.add(jPanel9, gridBagConstraints);

        jPanel14.setLayout(new java.awt.GridBagLayout());

        wherejLabel.setText(" Where ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        jPanel14.add(wherejLabel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 8;
        menuPanel.add(jPanel14, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        add(menuPanel, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

		private void cancelLoadButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelLoadButtonActionPerformed
			cancelLoadJob(false);
			updateMode("cancelled", null);
		}//GEN-LAST:event_cancelLoadButtonActionPerformed

		private void selectDistinctCheckBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_selectDistinctCheckBoxActionPerformed
			reloadRows();
		}//GEN-LAST:event_selectDistinctCheckBoxActionPerformed

    private void sortColumnsCheckBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_sortColumnsCheckBoxActionPerformed
        updateTableModel();
    }//GEN-LAST:event_sortColumnsCheckBoxActionPerformed

    private void deselectButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_deselectButtonActionPerformed
    	andCondition.setSelectedItem("");
    }//GEN-LAST:event_deselectButtonActionPerformed

    private void removeConditionButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_removeConditionButtonActionPerformed
        andCondition.setSelectedItem("");
    }//GEN-LAST:event_removeConditionButtonActionPerformed

    private void errorDetailsButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_errorDetailsButtonActionPerformed
        UIUtil.showException(this, "Error", currentErrorDetail);
    }//GEN-LAST:event_errorDetailsButtonActionPerformed

	private void loadButtonActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_loadButtonActionPerformed
		if (System.currentTimeMillis() - lastReloadTS > 200) {
			reloadRows();
		}
	}// GEN-LAST:event_loadButtonActionPerformed

	private void limitBoxItemStateChanged(java.awt.event.ItemEvent evt) {// GEN-FIRST:event_limitBoxItemStateChanged
		if (evt.getStateChange() == ItemEvent.SELECTED) {
			reloadRows();
		}
	}// GEN-LAST:event_limitBoxItemStateChanged

	void openQueryBuilder(boolean openSQLConsole) {
		openQueryBuilder(openSQLConsole, null);
	}

	void openQueryBuilder(boolean openSQLConsole, String alternativeWhere) {
		QueryBuilderDialog.Relationship root = createQBRelations(true);
		if (root != null) {
			if (alternativeWhere != null) {
				root.whereClause = alternativeWhere;
			}
			root.selectColumns = true;
			getQueryBuilderDialog().buildQuery(table, root, dataModel, session, getMetaDataSource(), openSQLConsole);
		}
	}

    // Variables declaration - do not modify//GEN-BEGIN:variables
    javax.swing.JComboBox andCondition;
    private javax.swing.JButton cancelLoadButton;
    private javax.swing.JPanel cardPanel;
    private javax.swing.JButton deselectButton;
    private javax.swing.JButton errorDetailsButton;
    private javax.swing.JLabel errorLabel;
    private javax.swing.JTextArea errorMessageTextArea;
    private javax.swing.JLabel findColumnsLabel;
    public javax.swing.JPanel findColumnsPanel;
    private javax.swing.JLabel from;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel11;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JLayeredPane jLayeredPane1;
    private javax.swing.JLayeredPane jLayeredPane2;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel10;
    private javax.swing.JPanel jPanel11;
    private javax.swing.JPanel jPanel12;
    private javax.swing.JPanel jPanel13;
    private javax.swing.JPanel jPanel14;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanel7;
    private javax.swing.JPanel jPanel8;
    private javax.swing.JPanel jPanel9;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JLabel join;
    private javax.swing.JPanel joinPanel;
    public javax.swing.JButton loadButton;
    private javax.swing.JLabel loadingCauseLabel;
    private javax.swing.JLabel loadingLabel;
    private javax.swing.JPanel loadingPanel;
    javax.swing.JPanel menuPanel;
    private javax.swing.JPanel noRowsFoundPanel;
    private javax.swing.JLabel on;
    private javax.swing.JPanel onPanel;
    private javax.swing.JToggleButton openEditorButton;
    private javax.swing.JLabel openEditorLabel;
    private javax.swing.JPanel pendingNonpendingPanel;
    private javax.swing.JLabel relatedRowsLabel;
    javax.swing.JPanel relatedRowsPanel;
    private javax.swing.JButton removeConditionButton;
    public javax.swing.JLabel rowsCount;
    public javax.swing.JTable rowsTable;
    private javax.swing.JPanel rowsTableContainerPanel;
    protected javax.swing.JScrollPane rowsTableScrollPane;
    private javax.swing.JPanel rrPanel;
    javax.swing.JCheckBox selectDistinctCheckBox;
    protected javax.swing.JPanel singleRowViewContainterPanel;
    private javax.swing.JPanel singleRowViewScrollContentPanel;
    javax.swing.JScrollPane singleRowViewScrollPane;
    private javax.swing.JPanel singleRowViewScrollPaneContainer;
    public javax.swing.JCheckBox sortColumnsCheckBox;
    private javax.swing.JLabel sortColumnsLabel;
    public javax.swing.JPanel sortColumnsPanel;
    private javax.swing.JLabel sqlLabel1;
    javax.swing.JPanel sqlPanel;
    protected javax.swing.JPanel statusPanel;
    private javax.swing.JPanel tablePanel;
    private javax.swing.JLabel wherejLabel;
    // End of variables declaration//GEN-END:variables

	JPanel thumbnail;
	private DBConditionEditor andConditionEditor;
	private ImageIcon conditionEditorIcon;
	{
		// load images
		conditionEditorIcon = UIUtil.readImage("/edit.png");
	}

	/**
	 * Cancels current load job.
	 * @param propagate
	 * @return
	 */
	public void cancelLoadJob(boolean propagate) {
		LoadJob cLoadJob;
		synchronized (this) {
			cLoadJob = currentLoadJob;
		}
		if (cLoadJob != null) {
			cLoadJob.cancel();
		}
		if (propagate) {
			for (RowBrowser child: getChildBrowsers()) {
				child.browserContentPane.cancelLoadJob(propagate);
			}
		}
	}

	private void updateMode(String mode, String cause) {
		String suffix;
		if (cause != null) {
			suffix = cause;
		} else {
			suffix = "";
		}
		loadingCauseLabel.setVisible(false);
		if ("table".equals(mode)) {
			loadingPanel.setVisible(false);
			rowsTable.setEnabled(true);
		} else if ("loading".equals(mode)) {
			mode = "table";
			loadingPanel.setVisible(true);
			loadingLabel.setText("loading...");
			loadingCauseLabel.setText(suffix);
			loadingCauseLabel.setVisible(true);
			cancelLoadButton.setVisible(true);
			rowsTable.setEnabled(false);
		} else if ("pending".equals(mode)) {
			mode = "table";
			loadingPanel.setVisible(true);
			loadingLabel.setText("pending...");
			cancelLoadButton.setVisible(false);
			rowsTable.setEnabled(false);
		} else if ("updating".equals(mode)) {
			mode = "table";
			loadingPanel.setVisible(true);
			loadingLabel.setText("updating...");
			cancelLoadButton.setVisible(true);
			rowsTable.setEnabled(false);
		}
		errorDetailsButton.setVisible(false);
		((CardLayout) cardPanel.getLayout()).show(cardPanel, mode);
	}

	/**
	 * Creates new row. Fills in foreign key.
	 *
	 * @param parents rows holding the primary key
	 * @param table the table of the new row
	 * @return new row of table
	 */
	private List<Row> createNewRow(List<Row> parents, Table table, Association association) {
		List<Row> children = new ArrayList<Row>();
		try {
			if (parents != null && association != null && !association.isInsertDestinationBeforeSource()) {
				for (Row par: parents) {
					Map<Column, Column> sToDMap = association.createSourceToDestinationKeyMapping();
					if (!sToDMap.isEmpty()) {
						Row row = new Row("", null, new Object[association.destination.getColumns().size()]);
						for (Map.Entry<Column, Column> e: sToDMap.entrySet()) {
							int iS = -1;
							for (int i = 0; i < table.getColumns().size(); ++i) {
								if (Quoting.equalsIgnoreQuotingAndCase(e.getKey().name, table.getColumns().get(i).name)) {
									iS = i;
									break;
								}
							}
							int iD = -1;
							for (int i = 0; i < association.destination.getColumns().size(); ++i) {
								if (Quoting.equalsIgnoreQuotingAndCase(e.getValue().name, association.destination.getColumns().get(i).name)) {
									iD = i;
									break;
								}
							}
							if (iS >= 0 && iD >= 0) {
								row.values[iD] = par.values[iS];
							} else {
								break;
							}
						}
						children.add(row);
					}
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return children;
	}

	protected abstract RowBrowser navigateTo(Association association, List<Row> pRows);

	protected abstract void onContentChange(List<Row> rows, boolean reloadChildren);

	protected abstract void onRedraw();
	protected abstract void onHide();

	protected abstract void beforeReload();
	protected void afterReload() {}

	protected abstract QueryBuilderDialog.Relationship createQBRelations(boolean withParents);
	protected abstract List<QueryBuilderDialog.Relationship> createQBChildrenRelations(RowBrowser tabu, boolean all);

	protected abstract void addRowToRowLink(Row pRow, Row exRow);

	protected abstract JFrame getOwner();

	protected abstract void findClosure(Row row);
	protected Object getMonitorForFindClosure() {
		return "";
	}
	protected void findTempClosure(Row row) {}
	protected abstract void findClosure(Row row, Set<Pair<BrowserContentPane, Row>> closure, boolean forward, FindClosureContext findClosureContext);

	protected abstract QueryBuilderDialog getQueryBuilderDialog();

	protected abstract void openSchemaMappingDialog();
	protected abstract void openSchemaAnalyzer();
	protected abstract DbConnectionDialog getDbConnectionDialog();
	protected abstract double getLayoutFactor();
	protected abstract List<RowBrowser> getChildBrowsers();
	protected abstract RowBrowser getParentBrowser();
	protected abstract List<RowBrowser> getTableBrowser();
	protected abstract void unhide();
	protected abstract void close();
	protected abstract void showInNewWindow();
	protected abstract void appendLayout();
	protected abstract void adjustClosure(BrowserContentPane tabu, BrowserContentPane thisOne);
	protected abstract void reloadDataModel() throws Exception;
	protected abstract MetaDataSource getMetaDataSource();
	protected abstract void deselectChildrenIfNeededWithoutReload();
	protected abstract int getReloadLimit();
	protected void changeColumnOrder(Table table) {
	}
	protected void rebase() {
	}
	protected RowBrowser copy(RowBrowser tableBrowser, Association association, Row parentRow, RowBrowser childToIgnore, boolean newParent) {
		return null;
	}
	protected RowBrowser getRowBrowser() {
		return null;
	}
	protected boolean shouldShowLoadErrors() {
		return true;
	}

	public interface RunnableWithPriority extends Runnable {
		int getPriority();
	}

	protected abstract PriorityBlockingQueue<RunnableWithPriority> getRunnableQueue();

	/**
	 * Collect layout of tables in a extraction model.
	 *
	 * @param positions to put positions into
	 */
	protected abstract void collectPositions(Map<String, Map<String, double[]>> positions);


	private void openDetails(final int x, final int y) {
		final JDialog d = new JDialog(getOwner(), (table instanceof SqlStatementTable)? "" : dataModel.getDisplayName(table), true);
		final boolean deselect = !currentSelectedRowCondition.equals("")
				&& currentSelectedRowCondition.equals(getAndConditionText())
				&& rows.size() == 1;
		d.getContentPane().add(new DetailsView(rows, rowsTable.getRowCount(), dataModel, table, 0, rowsTable.getRowSorter(), true, getQueryBuilderDialog() != null, rowIdSupport, deselect, session) {
			@Override
			protected void onRowChanged(int row) {
				setCurrentRowSelectionAndReloadChildrenIfLimitIsExceeded(row, false);
			}
			@Override
			protected void onClose() {
				d.setVisible(false);
			}
			@Override
			protected void onSelectRow(Row row) {
				d.setVisible(false);
				if (deselect) {
					andCondition.setSelectedItem("");
				} else {
					List<Row> rowList = new ArrayList<Row>();
					rowList.add(row);
					selectRow(rowList);
				}
			}
		});
		d.pack();
		d.setLocation(x, y);
		d.setSize(400, d.getHeight() + 20);
		UIUtil.fit(d);
		Window p = SwingUtilities.getWindowAncestor(this);
		if (p != null) {
			int maxX = p.getX() + p.getWidth() - d.getWidth();
			d.setLocation(Math.max(0, Math.min(maxX, d.getX())), d.getY());
			int maxY = p.getY() + p.getHeight() - d.getHeight();
			if (maxY < d.getY()) {
				int deltaH = Math.min(d.getY() - maxY, (int) (0.30 * d.getHeight()));
				maxY += deltaH;
				d.setSize(d.getWidth(), d.getHeight() - deltaH);
				d.setLocation(d.getX(), Math.max(0, maxY));
			}
		}
		d.setVisible(true);
		setCurrentRowSelectionAndReloadChildrenIfLimitIsExceeded(-1, false);
		onRedraw();
	}

	private void updateWhereField() {
		if (association != null) {
			if (parentRows != null && parentRows.size() > 0) {
				StringBuilder sb = new StringBuilder();
				for (int i = 0; i < parentRows.size(); ++i) {
					if (i > 0) {
						sb.append(" or\n");
					}
					sb.append(parentRows.get(i).rowId);
					if (i > 50) {
						sb.append("\n...");
						break;
					}
				}
				String currentCond = getAndConditionText().trim();
				String toolTip;
				if (currentCond.length() > 0) {
					toolTip = currentCond + "\nand (\n" + sb + ")";
				} else {
					toolTip = sb.toString();
				}
				wherejLabel.setToolTipText(UIUtil.toHTML(toolTip, 0));
			} else {
				wherejLabel.setToolTipText(null);
			}
		} else {
			wherejLabel.setToolTipText(null);
		}
	}

	public void convertToRoot() {
		association = null;
		parentRows = null;
		rowsClosure.currentClosureRowIDs.clear();
		adjustGui();
		reloadRows();
	}

	public void openDetailsView(int rowIndex, int x, int y) {
		final JDialog d = new JDialog(getOwner(), (table instanceof SqlStatementTable)? "" : dataModel.getDisplayName(table), true);
		final boolean deselect = !currentSelectedRowCondition.equals("")
				&& currentSelectedRowCondition.equals(getAndConditionText())
				&& rows.size() == 1;
		d.getContentPane().add(new DetailsView(rows, rowsTable.getRowCount(), dataModel, table, rowIndex, rowsTable.getRowSorter(), true, getQueryBuilderDialog() != null, rowIdSupport, deselect, session) {
			@Override
			protected void onRowChanged(int row) {
				setCurrentRowSelectionAndReloadChildrenIfLimitIsExceeded(row, false);
			}
			@Override
			protected void onClose() {
				d.setVisible(false);
			}
			@Override
			protected void onSelectRow(Row row) {
				d.setVisible(false);
				if (deselect) {
					andCondition.setSelectedItem("");
				} else {
					List<Row> rowList = new ArrayList<Row>();
					rowList.add(row);
					selectRow(rowList);
				}
			}
		});
		d.pack();
		d.setLocation(x, y);
		d.setSize(400, d.getHeight() + 20);
		int h = d.getHeight();
		UIUtil.fit(d);
		if (d.getHeight() < h) {
			y = Math.max(y - Math.min(h - d.getHeight(), Math.max(400 - d.getHeight(), 0)), 20);
			d.pack();
			d.setLocation(x, y);
			d.setSize(400, d.getHeight() + 20);
			UIUtil.fit(d);
		}
		Window p = SwingUtilities.getWindowAncestor(this);
		if (p != null) {
			int maxX = p.getX() + p.getWidth() - d.getWidth();
			d.setLocation(Math.max(0, Math.min(maxX, d.getX())), d.getY());
			int maxY = p.getY() + p.getHeight() - d.getHeight();
			if (maxY < d.getY()) {
				int deltaH = Math.min(d.getY() - maxY, (int) (0.30 * d.getHeight()));
				maxY += deltaH;
				d.setSize(d.getWidth(), d.getHeight() - deltaH);
				d.setLocation(d.getX(), Math.max(0, maxY));
			}
		}
		d.setVisible(true);
		setCurrentRowSelectionAndReloadChildrenIfLimitIsExceeded(-1, false);
		onRedraw();
	}

	public void updateSingleRowDetailsView() {
		if (singleRowDetailsView != null) {
			if (rowsClosure != null && rowsClosure.currentClosureRowIDs != null) {
				singleRowDetailsView.updateInClosureState(rows.size() == 1 && rowsClosure.currentClosureRowIDs.contains(new Pair<BrowserContentPane, String>(this, rows.get(0).nonEmptyRowId)));
			}
		}
	}

	private static TableContentViewFilter tableContentViewFilter = TableContentViewFilter.create();

	private Icon dropDownIcon;
	private ImageIcon relatedRowsIcon;
	private ImageIcon redDotIcon;
	private ImageIcon blueDotIcon;
	private ImageIcon greenDotIcon;
	private ImageIcon greyDotIcon;
	{
		// load images
		dropDownIcon = UIUtil.readImage("/dropdown.png");
		relatedRowsIcon = UIUtil.readImage("/right.png");
		redDotIcon = UIUtil.readImage("/reddot.gif");
		blueDotIcon = UIUtil.readImage("/bluedot.gif");
		greenDotIcon = UIUtil.readImage("/greendot.gif");
		greyDotIcon = UIUtil.readImage("/greydot.gif");
	}

	public void resetRowsTableContainer() {
		cardPanel.setVisible(true);
	}

	public JComponent getRowsTableContainer() {
		return cardPanel;
	}

	public JTable getRowsTable() {
		return rowsTable;
	}

	public LoadJob newLoadJob(ResultSet resultSet, Integer limit) {
		return new LoadJob(resultSet, limit == null? Integer.MAX_VALUE : limit);
	}

	public boolean isEditMode() {
		return isEditMode;
	}

	public void setEditMode(boolean isEditMode) {
		this.isEditMode = isEditMode;
	}

	private String statementForReloading;

	public synchronized void setStatementForReloading(String statementForReloading) {
		this.statementForReloading = statementForReloading;
	}

	/**
	 * Type of result set for in-place editing of query result.
	 */
	private List<Table> resultSetType;

	/**
	 * Sets type of result set for in-place editing of query result.
	 *
	 * @param resultSetType the type
	 */
	public void setResultSetType(List<Table> resultSetType) {
		this.resultSetType = resultSetType;
	}

	/**
	 * Gets a table from {@link #resultSetType} where a given column is defined.
	 *
	 * @param column the column
	 * @return suitable table for column or {@link #table}
	 */
	private Table getResultSetTypeForColumn(int column) {
		if (resultSetType == null) {
			return table;
		}
		if (typePerColumn.containsKey(column)) {
			return typePerColumn.get(column);
		}
		for (Table type: resultSetType) {
			if (type.getColumns().size() > column) {
				Column col = type.getColumns().get(column);
				if (col != null && col.name != null) {
					typePerColumn.put(column, type);
					return type;
				}
			}
		}
		typePerColumn.put(column, table);
		return table;
	}

	private boolean isPKComplete(Table type, Row r) {
		if (type.primaryKey == null) {
			return false;
		}
		int[] indexes = pkColumnIndexes.get(type);
		if (indexes == null) {
			indexes = new int[type.primaryKey.getColumns().size()];
			int ii = 0;
			for (Column pk: type.primaryKey.getColumns()) {
				Integer index = null;
				int i = 0;
				for (Column c: type.getColumns()) {
					if (c.name != null && c.name.equals(pk.name)) {
						index = i;
						break;
					}
					++i;
				}
				if (index == null) {
					return false;
				}
				indexes[ii++] = index;
			}
			pkColumnIndexes.put(type, indexes);
		}
		for (int i: indexes) {
			if (i >= r.values.length) {
				return false;
			}
			Object content = r.values[i];
			if (/*content == null || */ content instanceof TableModelItem && (((TableModelItem) content).value == UIUtil.NULL || ((TableModelItem) content).value == null)) {
				return false;
			}
		}
		return true;
	}

	private static boolean showingLoadErrorNow = false;

	private Map<Integer, Table> typePerColumn = new HashMap<Integer, Table>();
	private Map<Table, int[]> pkColumnIndexes = new HashMap<Table, int[]>();

	private String[] alternativeColumnLabels;

	public void setAlternativeColumnLabels(String[] columnLabels) {
		this.alternativeColumnLabels = columnLabels;
	}

	private void selectRow(final List<Row> toSelect) {
		StringBuilder conds = new StringBuilder();
		int numConds = 0;
		boolean append = false;
		for (int i = 0; i < rows.size(); ++i) {
			String rowId = rows.get(i).rowId;
			for (Row r: toSelect) {
				if (!r.rowId.isEmpty()) {
					if (r.rowId.equals(rowId)) {
						int vi = rowsTable.getRowSorter().convertRowIndexToView(i);
						if (vi >= 0) {
							setCurrentRowSelection(vi, append);
							append = true;
						}
						currentRowSelection = -1;
						String cond = SqlUtil.replaceAliases(rowId, "A", "A");
						if (numConds == 1) {
							conds = new StringBuilder("(" + conds + ")");
						}
						if (conds.length() != 0) {
							conds.append(" or ");
						}
						if (numConds > 0) {
							conds.append("(");
						}
						conds.append(cond);
						if (numConds > 0) {
							conds.append(")");
						}
						++numConds;
						break;
					}
				}
			}
		}
		deselectChildrenIfNeededWithoutReload();
//		String currentCond = getAndConditionText().trim();
		String cond = conds.toString();
//		if (currentCond.length() > 0) {
//			cond = "(" + cond + ") and (" + currentCond + ")";
//		}
		andCondition.setSelectedItem(cond);
	}

	protected void deselectIfNeededWithoutReload() {
		if (rows.size() == 1) {
			String rowId = rows.get(0).rowId;
			if (rowId != null && !rowId.isEmpty()) {
				String cond = SqlUtil.replaceAliases(rowId, "A", "A");
				String currentCond = getAndConditionText().trim();
				if (cond.equals(currentCond)) {
					boolean isSingleRowNotInClosure = false;
					if (rowsClosure != null && rowsClosure.currentClosureRowIDs != null) {
						isSingleRowNotInClosure = !rowsClosure.currentClosureRowIDs.contains(new Pair<BrowserContentPane, String>(this, rowId));
					}
					if (isSingleRowNotInClosure) {
						try {
							suppessReloadOnAndConditionAction = true;
							andCondition.setSelectedItem("");
						} finally {
							suppessReloadOnAndConditionAction = false;
						}
					}
				}
			}
		}
	}

	private static String readCharacterStream(final Reader reader, String length)
			throws IOException {
		final StringBuilder sb = new StringBuilder();
		final BufferedReader br = new BufferedReader(reader);

		int b;
		while(-1 != (b = br.read()))
		{
			sb.append((char)b);
			if (sb.length() > MAXCLOBLENGTH) {
				sb.append("... " + length);
				break;
			}
		}
		br.close();
		return sb.toString();
	}

	private List<Row> sortedAndFiltered(List<Row> rows) {
		RowSorter<? extends TableModel> sorter = rowsTable.getRowSorter();
		if (sorter != null && !rows.isEmpty()) {
			List<Row> result = new ArrayList<Row>();
			for (int i = 0; i < sorter.getViewRowCount(); ++i) {
				result.add(rows.get(sorter.convertRowIndexToModel(i)));
			}
			return result;
		}
		return rows;
	}

	private void sortChildren() {
		try {
			if (rowsTable.getRowSorter().getSortKeys().isEmpty()) {
				List<SortKey> sk = new ArrayList<SortKey>();
				sk.add(new SortKey(rowsTable.getColumnCount() - 1, SortOrder.ASCENDING));
				rowsTable.getRowSorter().setSortKeys(sk);
				ignoreSortKey = true;
			}
			adjustClosure(null, BrowserContentPane.this);
		} catch (Exception e) {
			// ignore
		}
		((TableRowSorter) rowsTable.getRowSorter()).sort();
		for (RowBrowser ch: getChildBrowsers()) {
			if (ch.browserContentPane != null) {
				ch.browserContentPane.sortChildren();
			}
		}
	}

	private static String readClob(Clob clob) throws SQLException, IOException {
		String length;
		try {
			length = clob.length() > 0? Long.toString(clob.length()) + " chars" : "";
		} catch (Exception e) {
			length = "";
		}
		return readCharacterStream(clob.getCharacterStream(), "<Clob> " + length);
	}

	private static String readSQLXML(SQLXML xml) throws SQLException, IOException {
		return readCharacterStream(xml.getCharacterStream(), "<Xml>");
	}

	public static Object toLobRender(Object object) {
		Object value = null;
		if (object instanceof Blob) {
			try {
				final long length = ((Blob) object).length();
				value = new LobValue() {
					@Override
					public String toString() {
						return "<Blob> " + (length > 0? length + " bytes" : "");
					}
				};
			} catch (Exception e) {
				value = new LobValue() {
					@Override
					public String toString() {
						return "<Blob>";
					}
				};
			}
		}
		if (object instanceof Clob) {
			try {
				final String content = readClob((Clob) object);
				value = new LobValue() {
					@Override
					public String toString() {
						return content;
					}
				};
			} catch (Exception e) {
				value = new LobValue() {
					@Override
					public String toString() {
						return "<Clob>";
					}
				};e.printStackTrace();
			}
		}
		if (object instanceof SQLXML) {
			try {
				final String content = readSQLXML((SQLXML) object);
				value = new LobValue() {
					@Override
					public String toString() {
						return content;
					}
				};
			} catch (Exception e) {
				value = new LobValue() {
					@Override
					public String toString() {
						return "<XML>";
					}
				};
				e.printStackTrace();
			}
		}
		return value;
	}

	public List<Row> getSelectedRows(Row additionalRow) {
		final List<Row> toSelect = new ArrayList<Row>();
		if (rowsClosure.currentClosureRootID.contains(additionalRow.nonEmptyRowId)) {
			for (Row r: rows) {
				if (!r.nonEmptyRowId.isEmpty() &&
						(rowsClosure.currentClosureRootID.contains(r.nonEmptyRowId) || additionalRow == r)) {
					toSelect.add(r);
				}
			}
		} else {
			toSelect.add(additionalRow);
		}
		return toSelect;
	}

	public String toCondition(List<Row> r) {
		if (r == null) {
			return "";
		}
		StringBuilder sb = new StringBuilder();
		int neCount = 0;
		for (Row row: r) {
			if (row.rowId != null) {
				neCount++;
			}
		}
		for (Row row: r) {
			if (row.rowId != null) {
				if (sb.length() > 0) {
					sb.append(" or ");
				}
				if (neCount > 1) {
					sb.append("(");
				}
				sb.append(row.rowId);
				if (neCount > 1) {
					sb.append(")");
				}
			}
		}
		return sb.toString();
	}

	private Reference<JTable> currentRowsTableReference = null;

	public void setCurrentRowsTable(Reference<JTable> reference) {
		currentRowsTableReference = reference;
	}

	private void findColumns(final int x, final int y, final JTable contextJTable) {
		TableColumnModel columnModel = rowsTable.getColumnModel();
		List<String> columNames = new ArrayList<String>();
		Map<String, Integer> columNamesCount = new HashMap<String, Integer>();
		for (int i = 0; i < columnModel.getColumnCount(); ++i) {
			Object nameObj = columnModel.getColumn(i).getHeaderValue();
			if (nameObj != null) {
				String name = nameObj.toString();
				if (columNamesCount.containsKey(name)) {
					columNamesCount.put(name, columNamesCount.get(name) + 1);
				} else {
					columNames.add(name);
					columNamesCount.put(name, 1);
				}
			}
		}
		Collections.sort(columNames, new Comparator<String>() {
			@Override
			public int compare(String o1, String o2) {
				return o1.compareToIgnoreCase(o2);
			}
		});

		final Window owner = SwingUtilities.getWindowAncestor(contextJTable);

		final JComboBox2 combobox = new JComboBox2();
		combobox.setModel(new DefaultComboBoxModel(columNames.toArray()));
		StringSearchPanel searchPanel = new StringSearchPanel(null, combobox, null, null, null, new Runnable() {
			@Override
			public void run() {
				Object selected = combobox.getSelectedItem();
				if (selected != null) {
					TableColumnModel columnModel = rowsTable.getColumnModel();
					foundColumn.clear();
					boolean scrolled = false;
					for (int i = 0; i < columnModel.getColumnCount(); ++i) {
						Object name = columnModel.getColumn(i).getHeaderValue();
						if (name != null && name.equals(selected)) {
							int mi = i;
							if (!scrolled) {
								if (contextJTable != null && contextJTable != rowsTable) {
									Rectangle visibleRect = contextJTable.getVisibleRect();
									Rectangle cellRect = contextJTable.getCellRect(mi, 0, true);
									contextJTable.scrollRectToVisible(
											new Rectangle(
													visibleRect.x + visibleRect.width / 2,
													cellRect.y - 16,
													1, cellRect.width + 16));
									contextJTable.repaint();
								} else {
									Rectangle visibleRect = rowsTable.getVisibleRect();
									Rectangle cellRect = rowsTable.getCellRect(0, mi, true);
									rowsTable.scrollRectToVisible(
											new Rectangle(
													cellRect.x - 32, visibleRect.y + visibleRect.height / 2,
													cellRect.width + 64, 1));
									rowsTable.repaint();
								}
								scrolled = true;
							}
							foundColumn.add(columnModel.getColumn(i).getModelIndex());
						}
					}
				}
			}
		}) {
			@Override
			protected Integer preferredWidth() {
				return 260;
			}
			@Override
			protected Integer maxX() {
				if (owner != null) {
					return owner.getX() + owner.getWidth() - preferredWidth();
				} else {
					return null;
				}
			}
			@Override
			protected Integer maxY(int height) {
				if (owner != null) {
					return owner.getY() + owner.getHeight() - height - 8;
				} else {
					return null;
				}
			}
		};

		searchPanel.setStringCount(columNamesCount);
		searchPanel.find(owner, "Find Column", x, y, true);
	}

	/**
	 * Calculates temporary closure when mouse cursor is on new row.
	 */
	private class TempClosureListener implements MouseListener, MouseMotionListener {
		Row currentRow = null;

		private Row rowAt(Point point) {
			int ri = rowsTable.rowAtPoint(point);
			if (ri >= 0 && !rows.isEmpty() && rowsTable.getRowSorter().getViewRowCount() > 0) {
				int i = rowsTable.getRowSorter().convertRowIndexToModel(ri);
				if (i >= 0 && i < rows.size()) {
					return rows.get(i);
				}
			}
			return null;
		}

		private void updateClosure(Row row) {
			if (currentRow != row) {
				currentRow = row;
				rowsClosure.tempClosure.clear();
				if (currentRow != null) {
					findTempClosure(currentRow);
				}
				onRedraw();
				AnimationController.activateAnimation(SwingUtilities.getWindowAncestor(BrowserContentPane.this));
			}
		}

		@Override
		public void mouseMoved(MouseEvent e) {
			Row row = null;
			if (e.getSource() == rowsTable) {
				row = rowAt(e.getPoint());
			} else if (e.getSource() == singleRowViewContainterPanel && rows.size() == 1) {
				row = rows.get(0);
			}
			updateClosure(row);
		}

		@Override
		public void mouseEntered(MouseEvent e) {
		}

		@Override
		public void mouseExited(MouseEvent e) {
			updateClosure(null);
		}

		@Override
		public void mouseDragged(MouseEvent e) {
			mouseMoved(e);
		}
		@Override
		public void mouseClicked(MouseEvent e) {
		}
		@Override
		public void mousePressed(MouseEvent e) {
		}
		@Override
		public void mouseReleased(MouseEvent e) {
		}
	}
}
