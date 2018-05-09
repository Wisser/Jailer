/*
 * Copyright 2007 - 2018 the original author or authors.
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
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
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
import java.sql.DatabaseMetaData;
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
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
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
import javax.swing.border.LineBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
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
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;
import net.sf.jailer.modelbuilder.MemorizedResultSet.MemorizedResultSetMetaData;
import net.sf.jailer.subsetting.ScriptFormat;
import net.sf.jailer.ui.ConditionEditor;
import net.sf.jailer.ui.DataModelManager;
import net.sf.jailer.ui.DbConnectionDialog;
import net.sf.jailer.ui.Environment;
import net.sf.jailer.ui.ExtractionModelFrame;
import net.sf.jailer.ui.JComboBox;
import net.sf.jailer.ui.QueryBuilderDialog;
import net.sf.jailer.ui.QueryBuilderDialog.Relationship;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.Desktop.RowBrowser;
import net.sf.jailer.ui.databrowser.metadata.MetaDataSource;
import net.sf.jailer.ui.databrowser.sqlconsole.SQLConsole;
import net.sf.jailer.ui.scrollmenu.JScrollC2Menu;
import net.sf.jailer.ui.scrollmenu.JScrollPopupMenu;
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
		
		public LoadJob(int limit, String andCond, boolean selectDistinct) {
			this.andCond = andCond;
			this.selectDistinct = selectDistinct;
			this.inputResultSet = null;
			synchronized (this) {
				this.limit = limit;
				finished = false;
				isCanceled = false;
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
			}
		}

		@Override
		public void run() {
			int l;
			synchronized (this) {
				l = limit;
				if (isCanceled) {
					CancellationHandler.reset(this);
					return;
				}
			}
			rowCountCache.clear();
			try {
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
			CancellationHandler.reset(this);
			SwingUtilities.invokeLater(new Runnable() {
				@Override
				public void run() {
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
					}
					if (e != null) {
						updateMode("error");
						unhide();
						UIUtil.showException(BrowserContentPane.this, "Error", e);
					} else {
						Set<String> prevIDs = new TreeSet<String>();
						long prevHash = 0;
						if (BrowserContentPane.this.rows != null) {
							for (Row r: BrowserContentPane.this.rows) {
								prevIDs.add(r.rowId);
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
						}
						onContentChange(new ArrayList<Row>(), false);
						BrowserContentPane.this.rows.clear();
						BrowserContentPane.this.rows.addAll(rows);
						updateTableModel(l, limitExceeded);
						Set<String> currentIDs = new TreeSet<String>();
						long currentHash = 0;
						if (rows != null) {
							for (Row r: rows) {
								currentIDs.add(r.rowId);
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
						}
						setPendingState(false, true);
						onContentChange(rows, true); // rows.isEmpty() || currentHash != prevHash || rows.size() != prevSize || !prevIDs.equals(currentIDs) || rows.size() != currentIDs.size());
						updateMode("table");
						updateWhereField();
						if (reloadAction != null) {
							reloadAction.run();
						}
					}
				}
			});
		}

		public void cancel() {
			synchronized (this) {
				if (isCanceled) {
					return;
				}
				isCanceled = true;
				if (finished) {
					return;
				}
			}
			CancellationHandler.cancel(this);
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
	 * Parent row, or <code>null</code>.
	 */
	Row parentRow;

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
	List<Row> rows = new ArrayList<Row>();

	/**
	 * For in-place editing.
	 */
	private BrowserContentCellEditor browserContentCellEditor = new BrowserContentCellEditor(new int[0]);
	
	/**
	 * Cache for association row count.
	 */
	private final Map<Pair<String, Association>, Pair<Long, Long>> rowCountCache = 
			Collections.synchronizedMap(new HashMap<Pair<String,Association>, Pair<Long,Long>>());
	
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
	 * Indexes of highlighted rows.
	 */
	Set<Integer> highlightedRows = new HashSet<Integer>();

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
	 * DB session.
	 */
	Session session;

	private int currentRowSelection = -1;

	private List<Row> parentRows;
	protected final Set<Pair<BrowserContentPane, Row>> currentClosure;
	protected Set<Pair<BrowserContentPane, String>> currentClosureRowIDs;
	private DetailsView singleRowDetailsView;
	private int initialRowHeight;

	private boolean suppressReload;

	/**
	 * Alias for row number column.
	 */
	private static final String ROWNUMBERALIAS = "RN";

	protected static final String UNKNOWN = "- unknown column -";
		
	protected static final int MAXLOBLENGTH = 2000;

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
	public BrowserContentPane(final DataModel dataModel, final Table table, String condition, Session session, Row parentRow, List<Row> parentRows,
			final Association association, Frame parentFrame, Set<Pair<BrowserContentPane, Row>> currentClosure, Set<Pair<BrowserContentPane, String>> currentClosureRowIDs,
			Integer limit, Boolean selectDistinct, boolean reload, ExecutionContext executionContext) {
		this.table = table;
		this.session = session;
		this.dataModel = dataModel;
		this.rowIdSupport = new RowIdSupport(dataModel, session.dbms, executionContext);
		this.parentRow = parentRow;
		this.parentRows = parentRows;
		this.association = association;
		this.currentClosure = currentClosure;
		this.currentClosureRowIDs = currentClosureRowIDs;
		this.executionContext = executionContext;
		
		rowIdSupport.setUseRowIdsOnlyForTablesWithoutPK(true);
		
		suppressReload = true;
		
		if (table == null) {
			this.table = new SqlStatementTable(null, null, false);
		}
		
		initComponents();
		andCondition = new JComboBox();
		andCondition.setEditable(true);
		GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel7.add(andCondition, gridBagConstraints);
        
		setPendingState(false, false);
		
		dropA.setText(null);
		dropA.setIcon(dropDownIcon);
		sqlLabel1.setIcon(dropDownIcon);
		dropA.addMouseListener(new java.awt.event.MouseAdapter() {
			@Override
			public void mousePressed(java.awt.event.MouseEvent evt) {
				openColumnDropDownBox(dropA, "A", table);
			}
			
			@Override
			public void mouseEntered(java.awt.event.MouseEvent evt) {
				dropA.setEnabled(false);
			}
			@Override
			public void mouseExited(java.awt.event.MouseEvent evt) {
				dropA.setEnabled(true);
		   }
		});
		
		if (association != null) {
			dropB.setText(null);
			dropB.setIcon(dropDownIcon);
			dropB.addMouseListener(new java.awt.event.MouseAdapter() {
				@Override
				public void mousePressed(java.awt.event.MouseEvent evt) {
					openColumnDropDownBox(dropB, "B", association.source);
				}
				
				@Override
				public void mouseEntered(java.awt.event.MouseEvent evt) {
					dropB.setEnabled(false);
				}
				@Override
				public void mouseExited(java.awt.event.MouseEvent evt) {
					dropB.setEnabled(true);
			   }
			});
		}
		
		final ListCellRenderer acRenderer = andCondition.getRenderer();
		andCondition.setRenderer(new ListCellRenderer() {
			@Override
			public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected,
					boolean cellHasFocus) {
				Component render = acRenderer.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
				if (render instanceof JLabel) {
					if (value != null && value.toString().trim().length() > 0) {
						String tooltip = ConditionEditor.toMultiLine(value.toString());
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
						if (value != null && value.toString().trim().length() > 0) {
							String tooltip = ConditionEditor.toMultiLine(value.toString());
							andCondition.setToolTipText(UIUtil.toHTML(tooltip, 200));
						} else {
							andCondition.setToolTipText(null);
						}
					}
				};
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
				if (BrowserContentPane.this.association != null && BrowserContentPane.this.association.isInsertDestinationBeforeSource()) {
					return;
				}
				
				int maxI = Math.min(rowsTable.getRowCount(), rows.size());

				RowSorter<? extends TableModel> sorter = getRowSorter();
				if (sorter != null) {
					maxI = sorter.getViewRowCount();
				}
				
				int lastPMIndex = -1;
				for (int i = 0; i < maxI; ++i) {
					int mi = sorter == null? i : sorter.convertRowIndexToModel(i);
					if (mi >= rows.size()) {
						continue;
					}
					if (rows.get(mi).getParentModelIndex() != lastPMIndex) {
						lastPMIndex = rows.get(mi).getParentModelIndex();
						int vi = i;
						Graphics2D g2d = (Graphics2D) graphics;
						g2d.setColor(color);
						g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
						g2d.setStroke(new BasicStroke(1));
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
				SwingUtilities.invokeLater(new Runnable() {
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
				SwingUtilities.invokeLater(new Runnable() {
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
				Point loc = SwingUtilities.convertPoint(
						BrowserContentPane.this,
						0, 0,
						SwingUtilities.getWindowAncestor(BrowserContentPane.this) 
						);
				openDetails(loc.x, loc.y);
			}
		});

		rowsTable.setAutoCreateRowSorter(true);
		rowsTable.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
		rowsTableScrollPane.setViewportView(rowsTable);

		setAndCondition(ConditionEditor.toSingleLine(condition), true);
		from.setText(table == null? "" : this.dataModel.getDisplayName(table));
		adjustGui();
		rowsTable.setShowGrid(false);
		final TableCellRenderer defaultTableCellRenderer = rowsTable.getDefaultRenderer(String.class);

		rowsTable.setDefaultRenderer(Object.class, new TableCellRenderer() {

			final Color BG1 = new Color(255, 255, 255);
			final Color BG2 = new Color(230, 255, 255);
			final Color BG1_EM = new Color(255, 255, 236);
			final Color BG2_EM = new Color(230, 255, 236);
			final Color BG3 = new Color(180, 205, 255);
			final Color BG4 = new Color(30, 200, 255);
			final Color FG1 = new Color(155, 0, 0);
			final Color FG2 = new Color(0, 0, 255);
			final Font font = new JLabel().getFont();
			final Font nonbold = new Font(font.getName(), font.getStyle() & ~Font.BOLD, font.getSize());
			final Font bold = new Font(nonbold.getName(), nonbold.getStyle() | Font.BOLD, nonbold.getSize());
			final Font italic = new Font(nonbold.getName(), nonbold.getStyle() | Font.ITALIC, nonbold.getSize());
			final Font italicBold = new Font(nonbold.getName(), nonbold.getStyle() | Font.ITALIC | Font.BOLD, nonbold.getSize());

			@Override
			public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
				boolean cellSelected = isSelected; 
				
				if (table.getSelectedColumnCount() <= 1 && table.getSelectedRowCount() <= 1) {
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
				if (rowSorter.getViewRowCount() == 0) {
					return render;
				}
				if (value instanceof Row) {
					Row theRow = (Row) value;
					Pair<BrowserContentPane, String> pair = new Pair<BrowserContentPane, String>(BrowserContentPane.this, theRow.rowId);
					singleRowDetailsView.setBorderColor(isSelected? render.getBackground() : BrowserContentPane.this.currentClosureRowIDs.contains(pair)? BG3 : Color.WHITE);
					return singleRowDetailsView;
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
							BrowserContentPane.this.currentClosureRowIDs != null && row < rows.size() && BrowserContentPane.this.currentClosureRowIDs.contains(new Pair<BrowserContentPane, String>(BrowserContentPane.this, rows.get(rowSorter.convertRowIndexToModel(row)).rowId))) {
							((JLabel) render).setBackground(BG3);
						} else {
							Table type = getResultSetTypeForColumn(convertedColumnIndex);
							if (isEditMode && table == rowsTable && r != null && browserContentCellEditor.isEditable(type, rowIndex, convertedColumnIndex, r.values[convertedColumnIndex])
									&& isPKComplete(type, r)) {
								((JLabel) render).setBackground((row % 2 == 0) ? BG1_EM : BG2_EM);
							} else {
								((JLabel) render).setBackground((row % 2 == 0) ? BG1 : BG2);
							}
						}
					} else {
						((JLabel) render).setBackground(BG4);
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
					try {
						((JLabel) render).setToolTipText(null);
						if (isNull) {
							((JLabel) render).setFont(highlightedRows.contains(rowSorter.convertRowIndexToModel(row)) ? italicBold : italic);
						} else {
							((JLabel) render).setFont(highlightedRows.contains(rowSorter.convertRowIndexToModel(row)) ? bold : nonbold);
							String text = ((JLabel) render).getText();
							if (text.indexOf('\n') >= 0) {
								((JLabel) render).setToolTipText(UIUtil.toHTML(text, 200));
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
		});
		rowsTable.setRowSelectionAllowed(true);
		rowsTable.setColumnSelectionAllowed(true);
		rowsTable.setCellSelectionEnabled(true);
		rowsTable.setEnabled(true);
		
		rowsTableScrollPane.getVerticalScrollBar().setUnitIncrement(32);
		
		rowsTable.addMouseListener(new MouseListener() {
			private JPopupMenu lastMenu;

			@Override
			public void mouseReleased(MouseEvent e) {
			}

			@Override
			public void mousePressed(MouseEvent e) {
				int ri = rowsTable.rowAtPoint(e.getPoint());
				if (ri >= 0 && !rows.isEmpty()) {
					int i = rowsTable.getRowSorter().convertRowIndexToModel(ri);
					Row row = rows.get(i);

					if (lastMenu == null || !lastMenu.isVisible()) {
						currentRowSelection = ri;
						onRedraw();
						Rectangle r = rowsTable.getCellRect(ri, 0, false);
						int x = Math.max(e.getPoint().x, (int) r.getMinX());
						int y = (int) r.getMaxY() - 2;
						if (singleRowDetailsView != null) {
							y = e.getY();
						}
						Point p = SwingUtilities.convertPoint(rowsTable, x, y, null);
						if (e.getButton() == MouseEvent.BUTTON1 && e.getClickCount() > 1) {
							openDetailsView(i, p.x + getOwner().getX(), p.y + getOwner().getY());
						} else if (e.getButton() != MouseEvent.BUTTON1) {
							JPopupMenu popup;
							popup = createPopupMenu(row, i, p.x + getOwner().getX(), p.y + getOwner().getY(), rows.size() == 1);
							if (popup != null) {
								popup.show(rowsTable, x, y);
								popup.addPropertyChangeListener("visible", new PropertyChangeListener() {
	
									@Override
									public void propertyChange(PropertyChangeEvent evt) {
										if (Boolean.FALSE.equals(evt.getNewValue())) {
											currentRowSelection = -1;
											onRedraw();
	//										setCurrentRowSelection(-1);
										}
									}
								});
							}
							lastMenu = popup;
						} else {
							setCurrentRowSelection(ri);
							if (getQueryBuilderDialog() != null) { // !SQL Console
								setCurrentRowSelection(-1);
							}
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
		});

		andConditionEditor = new ConditionEditor(parentFrame, null, dataModel);
		openEditorLabel.setIcon(conditionEditorIcon);
		openEditorLabel.setText(null);
		openEditorLabel.addMouseListener(new java.awt.event.MouseAdapter() {
			@Override
			public void mouseReleased(MouseEvent e) {
				mouseClicked(e);
			}
			@Override
			public void mouseClicked(MouseEvent e) {
				loadButton.grabFocus();
				SwingUtilities.invokeLater(new Runnable() {
					@Override
					public void run() {
						String cond = andConditionEditor.edit(getAndConditionText(), "Table", "A", table, null, null, null, false, true);
						if (cond != null) {
							if (!getAndConditionText().equals(ConditionEditor.toSingleLine(cond))) {
								setAndCondition(ConditionEditor.toSingleLine(cond), true);
								loadButton.grabFocus();
								reloadRows();
							}
						}
						openEditorLabel.setIcon(conditionEditorSelectedIcon);
					}
				});
			}

			@Override
			public void mouseEntered(java.awt.event.MouseEvent evt) {
				openEditorLabel.setIcon(conditionEditorSelectedIcon);
			}

			@Override
			public void mouseExited(java.awt.event.MouseEvent evt) {
				openEditorLabel.setIcon(conditionEditorIcon);
			}
		});
		relatedRowsLabel.setIcon(UIUtil.scaleIcon(this, relatedRowsIcon));
		if (createPopupMenu(null, -1, 0, 0, false).getComponentCount() == 0) {
			relatedRowsLabel.setEnabled(false);
		} else {
			relatedRowsPanel.addMouseListener(new java.awt.event.MouseAdapter() {
				private JPopupMenu popup;
				private boolean in = false;
	
				@Override
				public void mousePressed(MouseEvent e) {
					loadButton.grabFocus();
					SwingUtilities.invokeLater(new Runnable() {
						@Override
						public void run() {
			//				if (rows.size() == 1) {
			//					popup = createPopupMenu(rows.get(0), 0, 0, 0);
			//				} else {
								popup = createPopupMenu(null, -1, 0, 0, false);
			//				}
							setCurrentRowSelection(-2);
							popup.addPropertyChangeListener("visible", new PropertyChangeListener() {
								@Override
								public void propertyChange(PropertyChangeEvent evt) {
									if (Boolean.FALSE.equals(evt.getNewValue())) {
										popup = null;
										updateBorder();
										setCurrentRowSelection(-1);
									}
								}
							});
							popup.show(relatedRowsPanel, 0, relatedRowsPanel.getHeight());
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
				SwingUtilities.invokeLater(new Runnable() {
					@Override
					public void run() {
						Point loc = sqlPanel.getLocationOnScreen();
						popup = createSqlPopupMenu(BrowserContentPane.this.parentRow, 0, (int) loc.getX(), (int) loc.getY(), false);
						setCurrentRowSelection(-2);
						popup.addPropertyChangeListener("visible", new PropertyChangeListener() {
							@Override
							public void propertyChange(PropertyChangeEvent evt) {
								if (Boolean.FALSE.equals(evt.getNewValue())) {
									popup = null;
									updateBorder();
									setCurrentRowSelection(-1);
								}
							}
						});
						popup.show(sqlPanel, 0, sqlPanel.getHeight());
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
		limitBox.setModel(new DefaultComboBoxModel(DataBrowser.ROW_LIMITS));
		limitBox.setSelectedItem(association == null? 200 : 500);
		if (limit != null) {
			limitBox.setSelectedItem(limit);
		}
		if (selectDistinct != null) {
			selectDistinctCheckBox.setSelected(selectDistinct);
		}
		updateTableModel(0, false);

		suppressReload = false;
		if (reload) {
			reloadRows();
		}
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
		registerAccelerator(ks, a, limitBox);
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
			updateMode("pending");
		}
		if (propagate) {
			for (RowBrowser child: getChildBrowsers()) {
				child.browserContentPane.setPendingState(pending, propagate);
			}
		}
	}

	protected void historizeAndCondition(Object item) {
		if (item == null) {
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
			dropB.setVisible(false);
			andLabel.setText(" Where ");
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
			on.setToolTipText(on.getText());
		}
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
					parent.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
					try {
						for (ActionListener al: todoList) {
							al.actionPerformed(e);
						}
					} finally {
						parent.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
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
		
	};
	
	private String currentSelectedRowCondition = "";
	
	/**
	 * Creates popup menu for navigation.
	 * @param navigateFromAllRows 
	 */
	public JPopupMenu createPopupMenu(final Row row, final int rowIndex, final int x, final int y, boolean navigateFromAllRows) {
		JMenuItem tableFilter = new JCheckBoxMenuItem("Table Filter");
		tableFilter.setAccelerator(KS_FILTER);
		tableFilter.setSelected(isTableFilterEnabled);
		tableFilter.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				isTableFilterEnabled = !isTableFilterEnabled;
				updateTableModel();
			}
		});
		JMenuItem copyTCB = new JMenuItem("Copy to Clipboard");
		copyTCB.setAccelerator(KS_COPY_TO_CLIPBOARD);
		copyTCB.setEnabled(rowsTable.getSelectedColumnCount() > 0);
		copyTCB.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				UIUtil.copyToClipboard(rowsTable, true);
			}
		});
		
		if (table instanceof SqlStatementTable && resultSetType == null) {
			JPopupMenu jPopupMenu = new JPopupMenu();
			JMenuItem update = new JMenuItem("Update");
			jPopupMenu.add(update);
			update.setEnabled(false);
			JMenuItem delete = new JMenuItem("Delete");
			jPopupMenu.add(delete);
			delete.setEnabled(false);
			JMenuItem insert = new JMenuItem("Insert");
			jPopupMenu.add(insert);
			insert.setEnabled(false);
			jPopupMenu.addSeparator();
			jPopupMenu.add(copyTCB);
			jPopupMenu.addSeparator();
			jPopupMenu.add(tableFilter);
			JMenuItem editMode = new JMenuItem("Edit Mode");
			editMode.setAccelerator(KS_EDIT);
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
			String name = c + a.getDataModel().getDisplayName(a.destination) + (n > 1 ? " on " + a.getName() : "");
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
			det.setAccelerator(KS_DETAILS);
			popup.insert(det, 0);
			popup.insert(new JSeparator(), 1);
			det.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					openDetailsView(rowIndex, x, y);
				}
			});

			if (!(table instanceof SqlStatementTable) || resultSetType != null) {
				popup.add(new JSeparator());

				JMenuItem qb = new JMenuItem("Query Builder");
				qb.setAccelerator(KS_QUERYBUILDER);
				popup.add(qb);
				qb.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						List<String> whereClauses = new ArrayList<String>();
						whereClauses.add(SqlUtil.replaceAliases(row.rowId, "A", "A"));
						getQueryBuilderDialog().buildQuery(table, true, false, new ArrayList<Association>(), whereClauses, dataModel, session, getMetaDataSource(), false);
					}
				});

				JMenuItem sqlConsole = new JMenuItem("SQL Console");
				sqlConsole.setAccelerator(KS_SQLCONSOLE);
				popup.add(sqlConsole);
				sqlConsole.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						List<String> whereClauses = new ArrayList<String>();
						whereClauses.add(SqlUtil.replaceAliases(row.rowId, "A", "A"));
						getQueryBuilderDialog().buildQuery(table, true, false, new ArrayList<Association>(), whereClauses, dataModel, session, getMetaDataSource(), true);
					}
				});

				if (!currentSelectedRowCondition.equals("") 
						&& currentSelectedRowCondition.equals(getAndConditionText())
						&& rows.size() == 1) {
					JMenuItem sr = new JMenuItem("Deselect Row");
					popup.insert(sr, 1);
					sr.addActionListener(new ActionListener() {
						@Override
						public void actionPerformed(ActionEvent e) {
							andCondition.setSelectedItem("");
							reloadRows();
						}
					});
				} else {
					JMenuItem sr = new JMenuItem("Select Row");
					sr.setEnabled(rows.size() > 1);
					popup.insert(sr, 1);
					sr.addActionListener(new ActionListener() {
						@Override
						public void actionPerformed(ActionEvent e) {
							String cond = SqlUtil.replaceAliases(row.rowId, "A", "A");
							String currentCond = getAndConditionText().trim();
							if (currentCond.length() > 0) {
								cond = "(" + cond + ") and (" + currentCond + ")";
							}
							andCondition.setSelectedItem(cond);
							currentSelectedRowCondition = cond;
							reloadRows();
						}
					});
				}
				
				JMenu sql = new JMenu("SQL/DML");
				final String rowName = !(table instanceof SqlStatementTable)? dataModel.getDisplayName(table) + "(" + SqlUtil.replaceAliases(row.rowId, null, null) + ")" : "";
				JMenuItem update = new JMenuItem("Update");
				sql.add(update);
				update.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						openSQLDialog("Update Row " + rowName, x, y, SQLDMLBuilder.buildUpdate(table, row, true, session));
					}
				});
				JMenuItem delete = new JMenuItem("Delete");
				sql.add(delete);
				delete.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						openSQLDialog("Delete Row " + rowName, x, y, SQLDMLBuilder.buildDelete(table, row, true, session));
					}
				});
				JMenuItem insert = new JMenuItem("Insert");
				sql.add(insert);
				insert.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						openSQLDialog("Insert Row " + rowName, x, y, SQLDMLBuilder.buildInsert(table, row, true, session));
					}
				});
				insert.setEnabled(resultSetType == null);
				update.setEnabled(resultSetType == null);
				delete.setEnabled(resultSetType == null);
				if (getQueryBuilderDialog() == null) {
					popup.removeAll();
					popup.add(det);
					popup.addSeparator();
					popup.add(insert);
					popup.add(update);
					popup.add(delete);
				} else {
					popup.add(sql);
				}
			}
			popup.addSeparator();
			popup.add(copyTCB);
			popup.addSeparator();
			popup.add(tableFilter);
			JCheckBoxMenuItem editMode = new JCheckBoxMenuItem("Edit Mode");
			editMode.setAccelerator(KS_EDIT);
			editMode.setSelected(isEditMode);
			editMode.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					setEditMode(!isEditMode);
					updateTableModel();
				}
			});
			popup.add(editMode);
		}
		
		return popup;
	}

	/**
	 * Creates popup menu for SQL.
	 * @param forNavTree 
	 */
	public JPopupMenu createSqlPopupMenu(final Row parentrow, final int rowIndex, final int x, final int y, boolean forNavTree) {
		JPopupMenu popup = new JPopupMenu();
		
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
		JMenu sqlDml = new JMenu("SQL/DML");
		popup.add(sqlDml);
		JMenuItem insertNewRow = new JMenuItem("Insert New Row");
		sqlDml.add(insertNewRow);
		final String tableName = dataModel.getDisplayName(table);
		insertNewRow.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				Row parent = null;
				if (association.isInsertSourceBeforeDestination()) {
					if (parentrow != null) {
						parent = parentrow;
					} else {
						RowBrowser parentRowBrowser = getParentBrowser();
						if (parentRowBrowser != null && parentRowBrowser.browserContentPane != null) {
							BrowserContentPane parentContentPane = parentRowBrowser.browserContentPane;
							if (parentContentPane.rows != null) {
								if (parentContentPane.rows.size() == 1) {
									parent = parentContentPane.rows.get(0);
								} else {
									if (parentContentPane.currentClosureRowIDs != null) {
										for (Row row: parentContentPane.rows) {
											if (parentContentPane.currentClosureRowIDs.contains(new Pair<BrowserContentPane, String>(parentContentPane, row.rowId))) {
												parent = row;
												break;
											}
										}
									}
								}
							}
						}
					}
				}
				openSQLDialog("Insert New Row Into " + tableName, x, y, SQLDMLBuilder.buildInsert(table, createNewRow(parent, table), true, session));
			}
		});
		JMenuItem insert = new JMenuItem("Inserts");
		sqlDml.add(insert);
		insert.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				openSQLDialog("Insert Into " + tableName, x, y, new Object() { @Override
				public String toString() { return SQLDMLBuilder.buildInsert(table, rows, session); }});
			}
		});
		JMenuItem update = new JMenuItem("Updates");
		sqlDml.add(update);
		update.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				openSQLDialog("Update " + tableName, x, y,  new Object() { @Override
				public String toString() { return SQLDMLBuilder.buildUpdate(table, rows, session); }});
			}
		});
		JMenuItem delete = new JMenuItem("Deletes");
		sqlDml.add(delete);
		delete.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				openSQLDialog("Delete from " + tableName, x, y,  new Object() { @Override
				public String toString() { return SQLDMLBuilder.buildDelete(table, rows, session); }});
			}
		});
		insert.setEnabled(rows.size() > 0);
		update.setEnabled(rows.size() > 0);
		delete.setEnabled(rows.size() > 0);

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
		JMenuItem al = new JMenuItem("Append Layout...");
		popup.add(al);
		al.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				appendLayout();
			}
		});
		
		popup.add(new JSeparator());
		JMenuItem tableFilter = new JCheckBoxMenuItem("Table Filter");
		tableFilter.setAccelerator(KS_FILTER);
		tableFilter.setSelected(isTableFilterEnabled);
		tableFilter.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				isTableFilterEnabled = !isTableFilterEnabled;
				updateTableModel();
			}
		});
		popup.add(tableFilter);
		JCheckBoxMenuItem editMode = new JCheckBoxMenuItem("Edit Mode");
		editMode.setAccelerator(KS_EDIT);
		editMode.setSelected(isEditMode);
		editMode.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				setEditMode(!isEditMode);
				updateTableModel();
			}
		});
		popup.add(editMode);
		popup.addSeparator();
		JMenuItem m = new JMenuItem("Hide from View");
		popup.add(m);
		m.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				onHide();
			}
		});
		m = new JMenuItem("Close" + (getChildBrowsers().isEmpty()? "" : " Subtree"));
		popup.add(m);
		m.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				closeSubTree(BrowserContentPane.this);
			}
			private void closeSubTree(BrowserContentPane cp) {
				for (RowBrowser c: cp.getChildBrowsers()) {
					closeSubTree(c.browserContentPane);
				}
				cp.close();
			}
		});
		return popup;
	}

	void openExtractionModelEditor(boolean doExport) {
		Component parent = SwingUtilities.getWindowAncestor(this);
		if (parent == null) {
			parent = this;
		}
		parent.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
		try {
			String file;
			String ts = new SimpleDateFormat("HH-mm-ss-SSS").format(new Date());
			File newFile;
			
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
			
			for (int i = 1; ; ++i) {
				file = Environment.newFile("extractionmodel" + File.separator + "by-example").getPath();
				newFile = new File(file);
				newFile.mkdirs();
				file += File.separator + "SbE-" + (dataModel.getDisplayName(stable).replaceAll("[\"'\\[\\]]", "")) + "-" + ts + (i > 1? "-" + Integer.toString(i) : "") + ".jm";
				newFile = new File(file);
				if (!newFile.exists()) {
					break;
				}
			}
			Map<String, Map<String, double[]>> positions = new TreeMap<String, Map<String,double[]>>();
			collectPositions(positions);
			String currentModelSubfolder = DataModelManager.getCurrentModelSubfolder(executionContext);
			dataModel.save(file, stable, subjectCondition, ScriptFormat.SQL, restrictionDefinitions, positions, new ArrayList<ExtractionModel.AdditionalSubject>(), currentModelSubfolder);

			ExtractionModelFrame extractionModelFrame = ExtractionModelFrame.createFrame(file, false, !doExport, null, executionContext);
			extractionModelFrame.setDbConnectionDialogClone(getDbConnectionDialog());
			if (doExport) {
				extractionModelFrame.openExportDialog(false, new Runnable() {
					@Override
					public void run() {
						try {
							reloadDataModel();
						} catch (Exception e) {
							throw new RuntimeException(e);
						}
					}
				});
				extractionModelFrame.dispose();
			} else {
				extractionModelFrame.markDirty();
				extractionModelFrame.expandAll();
			}
			newFile.delete();
		} catch (Throwable e) {
			parent.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
			UIUtil.showException(this, "Error", e, session);
		} finally {
			parent.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
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
		setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
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
			d = new JDialog(getOwner(), "SQL/DML - " + titel, true);
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
			setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		}
		d.setVisible(true);
	}
	
	protected abstract SQLConsole getSqlConsole(boolean switchToConsole);
	
	private void appendClosure() {
		if (getParentBrowser() != null) {
			BrowserContentPane parentContentPane = getParentBrowser().browserContentPane;
	
			Set<Pair<BrowserContentPane, Row>> newElements = new HashSet<Pair<BrowserContentPane, Row>>();
			for (Pair<BrowserContentPane, Row> e: currentClosure) {
				if (e.a == parentContentPane) {
					parentContentPane.findClosure(e.b, newElements, true);
				}
			}
			currentClosure.addAll(newElements);
			currentClosureRowIDs.clear();
			for (Pair<BrowserContentPane, Row> r: currentClosure) {
				currentClosureRowIDs.add(new Pair<BrowserContentPane, String>(r.a, r.b.rowId));
			}
			rowsTable.repaint();
			adjustClosure(null, this);
		}
	}

	protected void setCurrentRowSelection(int i) {
		currentRowSelection = i;
		if (i >= 0) {
			currentClosure.clear();
			findClosure(rows.get(rowsTable.getRowSorter().convertRowIndexToModel(i)));
			Rectangle visibleRect = rowsTable.getVisibleRect();
			Rectangle pos = rowsTable.getCellRect(i, 0, false);
			rowsTable.scrollRectToVisible(new Rectangle(visibleRect.x, pos.y, 1, pos.height));
		}
		currentClosureRowIDs.clear();
		for (Pair<BrowserContentPane, Row> r: currentClosure) {
			currentClosureRowIDs.add(new Pair<BrowserContentPane, String>(r.a, r.b.rowId));
		}
		rowsTable.repaint();
		adjustClosure(this, null);
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
		
		int l = 0;
		for (String name : assList) {
			if (!name.startsWith(prefix)) {
				continue;
			}

			final Association association = assMap.get(name);

			++l;
			
//			if (++l > 300) {
//				JMenu p = new JMenu("more...");
//				if (current != null) {
//					GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
//			        gridBagConstraints.gridx = 1;
//			        gridBagConstraints.gridy = l;
//			        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
//			        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
//			        gridBagConstraints.weightx = 1.0; 
//					current.getPopupMenu().add(p, gridBagConstraints);
//				} else {
//					popup.add(p);
//				}
//				l = 1;
//				current = p;
//				current.getPopupMenu().setLayout(new GridBagLayout());
//			}

			final JMenuItem item = new JMenuItem("  " + (name.substring(1)) + "   ");
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
				if (association == child.association) {
					if (rowIndex < 0 && child.rowIndex < 0 || rowIndex == child.rowIndex) {
						item.setFont(new Font(item.getFont().getName(), item.getFont().getStyle() | Font.ITALIC, item.getFont().getSize()));
						excludeFromANEmpty = true;
						break;
					}
				}
			}
			
			final ActionListener itemAction = new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					highlightedRows.add(rowIndex);
					if (navigateFromAllRows) {
						navigateTo(association, -1, null);
					} else {
						navigateTo(association, rowIndex, row);
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
							r = Collections.singletonList(row);
							key = new Pair<String, Association>(row.rowId, association);
						}
	
						Pair<Long, Long> cachedCount = rowCountCache.get(key);
						long rowCount;
						
						if (cachedCount != null && cachedCount.b > System.currentTimeMillis()) {
							rowCount = cachedCount.a;
						} else {
							RowCounter rc = new RowCounter(table, association, r, session, rowIdSupport);
							try {
								rowCount = rc.countRows(getAndConditionText(), context, MAX_RC + 1, false);
							} catch (SQLException e) {
								rowCount = -1;
							}
							rowCountCache.put(key, new Pair<Long, Long>(rowCount, System.currentTimeMillis() + MAX_ROWCOUNTCACHE_RETENTION_TIME));
						}
						
						final long count = rowCount;
						
						SwingUtilities.invokeLater(new Runnable() {
							@Override
							public void run() {
								String cs = " " + (count < 0? "?" : (count > MAX_RC)? (">" + MAX_RC) : count) + " ";
								countLabel.setText(cs);
								if (count == 0) {
									countLabel.setForeground(Color.lightGray);
								}
								if (!fExcludeFromANEmpty) {
									allNonEmptyItem.rowsCounted(count, itemAction);
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
		if (!suppressReload) {
			lastReloadTS = System.currentTimeMillis();
			cancelLoadJob(true);
			setPendingState(true, true);
			rows.clear();
			updateMode("loading");
			setPendingState(false, false);
			int limit = getReloadLimit();
			LoadJob reloadJob;
			if (statementForReloading != null) {
				reloadJob = new LoadJob(limit, statementForReloading, false);
			} else {
				reloadJob = new LoadJob(limit, (table instanceof SqlStatementTable)? "" : getAndConditionText(), selectDistinctCheckBox.isSelected());
			}
			synchronized (this) {
				currentLoadJob = reloadJob;
			}
			getRunnableQueue().add(reloadJob);
		}
	}

	protected int getReloadLimit() {
		int limit = 100;
		if (limitBox.getSelectedItem() instanceof Integer) {
			limit = (Integer) limitBox.getSelectedItem();
		}
		return limit;
	}

	/**
	 * Reload rows from {@link #table}.
	 * 
	 * @param rows
	 *            to put the rows into
	 * @param context
	 *            cancellation context
	 * @param limit
	 *            row number limit
	 */
	private void reloadRows(ResultSet inputResultSet, String andCond, final List<Row> rows, Object context, int limit, boolean selectDistinct) throws SQLException {
		try {
			session.setSilent(true);
			reloadRows(inputResultSet, andCond, rows, context, limit, selectDistinct, null);
			return;
		} catch (SQLException e) {
			Session._log.warn("failed, try another strategy (" +  e.getMessage() + ")");
		} finally {
			session.setSilent(false);
		}
		Set<String> existingColumnsLowerCase = null;
		if (!(table instanceof SqlStatementTable) && statementForReloading == null) {
			existingColumnsLowerCase = findColumnsLowerCase(table, session);
		}
		reloadRows(inputResultSet, andCond, rows, context, limit, selectDistinct, existingColumnsLowerCase);
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
			DatabaseMetaData metaData = session.getMetaData();
			Quoting quoting = new Quoting(session);
			String defaultSchema = JDBCMetaDataBasedModelElementFinder.getDefaultSchema(session, session.getSchema());
			String schema = quoting.unquote(table.getOriginalSchema(defaultSchema));
			String tableName = quoting.unquote(table.getUnqualifiedName());
			ResultSet resultSet = JDBCMetaDataBasedModelElementFinder.getColumns(session, metaData, schema, tableName, "%", false);
			while (resultSet.next()) {
				String colName = resultSet.getString(4).toLowerCase();
				columns.add(colName);
			}
			resultSet.close();
			if (columns.isEmpty()) {
				if (session.getMetaData().storesUpperCaseIdentifiers()) {
					schema = schema.toUpperCase();
					tableName = tableName.toUpperCase();
				} else {
					schema = schema.toLowerCase();
					tableName = tableName.toLowerCase();
				}
				resultSet = JDBCMetaDataBasedModelElementFinder.getColumns(session, metaData, schema, tableName, "%", false);
				while (resultSet.next()) {
					String colName = resultSet.getString(4).toLowerCase();
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
	 * @param context
	 *            cancellation context
	 * @param limit
	 *            row number limit
	 */
	private void reloadRows(ResultSet inputResultSet, String andCond, final List<Row> rows, Object context, int limit, boolean selectDistinct, Set<String> existingColumnsLowerCase) throws SQLException {
		if (table instanceof SqlStatementTable || statementForReloading != null) {
			try {
				session.setSilent(true);
				Map<String, List<Row>> rowsMap = new HashMap<String, List<Row>>();
				reloadRows(inputResultSet, null, andCond, null, rowsMap, context, limit, false, null, existingColumnsLowerCase);
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
			pRows = Collections.singletonList(parentRow);
		} else {
			pRows = new ArrayList<Row>(pRows);
//			RowBrowser pb = getParentBrowser();
//			if (pb != null) {
//				if (pb.browserContentPane != null) {
//					if (pb.browserContentPane.rowsTable != null) {
//						final RowSorter pSorter = pb.browserContentPane.rowsTable.getRowSorter();
//						ArrayList<Row> sortedPRows = new ArrayList<Row>();
//						for (int y = 0; y < pSorter.getViewRowCount(); ++y) {
//							int my = pSorter.convertRowIndexToModel(y);
//							sortedPRows.add(pRows.get(my));
//							pRows.set(my, null);
//						}
//						for (int i = 0; i < pRows.size(); i++) {
//							Row r = pRows.get(i);
//							if (r != null) {
//								sortedPRows.add(r);
//							}
//						}
//						pRows = sortedPRows;
//					}
//				}
//			}
		}
		Map<String, Row> rowSet = new HashMap<String, Row>();
		if (parentRows != null) {
			beforeReload();
		}
		noNonDistinctRows = 0;
		noDistinctRows = 0;
		
		if (association != null && rowIdSupport.getPrimaryKey(association.source).getColumns().isEmpty()) {
			loadRowBlocks(inputResultSet, null, andCond, rows, context, limit, selectDistinct, pRows, rowSet, 1, existingColumnsLowerCase);
		} else {
			if (useInlineViewForResolvingAssociation(session)) {
				try {
					InlineViewStyle inlineViewStyle = session.getInlineViewStyle();
					if (inlineViewStyle != null) {
						loadRowBlocks(inputResultSet, inlineViewStyle, andCond, rows, context, limit, selectDistinct, pRows, rowSet, 510, existingColumnsLowerCase);
						return;
					}
				} catch (Exception e) {
					Session._log.warn("failed, try another blocking-size (" +  e.getMessage() + ")");
				}
			}
			try {
				loadRowBlocks(inputResultSet, null, andCond, rows, context, limit, selectDistinct, pRows, rowSet, 510, existingColumnsLowerCase);
				return;
			} catch (SQLException e) {
				Session._log.warn("failed, try another blocking-size (" +  e.getMessage() + ")");
			}
			try {
				loadRowBlocks(inputResultSet, null, andCond, rows, context, limit, selectDistinct, pRows, rowSet, 300, existingColumnsLowerCase);
				return;
			} catch (SQLException e) {
				Session._log.warn("failed, try another blocking-size (" +  e.getMessage() + ")");
			}
			try {
				loadRowBlocks(inputResultSet, null, andCond, rows, context, limit, selectDistinct, pRows, rowSet, 100, existingColumnsLowerCase);
				return;
			} catch (SQLException e) {
				Session._log.warn("failed, try another blocking-size (" +  e.getMessage() + ")");
			}
			try {
				loadRowBlocks(inputResultSet, null, andCond, rows, context, limit, selectDistinct, pRows, rowSet, 40, existingColumnsLowerCase);
				return;
			} catch (SQLException e) {
				Session._log.warn("failed, try another blocking-size (" +  e.getMessage() + ")");
			}
		}
		
		loadRowBlocks(inputResultSet, null, andCond, rows, context, limit, selectDistinct, pRows, rowSet, 1, existingColumnsLowerCase);
	}

	static boolean useInlineViewForResolvingAssociation(Session session) {
		return session.dbms.isUseInlineViewsInDataBrowser();
	}

	private void loadRowBlocks(ResultSet inputResultSet, InlineViewStyle inlineViewStyle, String andCond, final List<Row> rows, Object context, int limit, boolean selectDistinct, List<Row> pRows,
			Map<String, Row> rowSet, int NUM_PARENTS, Set<String> existingColumnsLowerCase) throws SQLException {
		List<List<Row>> parentBlocks = new ArrayList<List<Row>>();
		List<Row> currentBlock = new ArrayList<Row>();
		Set<String> regPRows = new HashSet<String>();
		parentBlocks.add(currentBlock);
		for (Row pRow : pRows) {
			if (currentBlock.size() >= NUM_PARENTS) {
				currentBlock = new ArrayList<Row>();
				parentBlocks.add(currentBlock);		
			}
			currentBlock.add(pRow);
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
					reloadRows(inputResultSet, inlineViewStyle, andCond, pRowBlock, newBlockRows, context, limit, false, session.dbms.getSqlLimitSuffix(), existingColumnsLowerCase);
					loaded = true;
				} catch (SQLException e) {
					Session._log.warn("failed, try another limit-strategy (" +  e.getMessage() + ")");
				} finally {
					session.setSilent(false);
				}
			}
			if (!loaded) {
				try {
					session.setSilent(true);
					reloadRows(inputResultSet, inlineViewStyle, andCond, pRowBlock, newBlockRows, context, limit, true, null, existingColumnsLowerCase);
					loaded = true;
				} catch (SQLException e) {
					Session._log.warn("failed, try another limit-strategy (" +  e.getMessage() + ")");
				} finally {
					session.setSilent(false);
				}
				if (!loaded) {
					try {
						session.setSilent(true);
						reloadRows(inputResultSet, inlineViewStyle, andCond, pRowBlock, newBlockRows, context, limit, false, null, existingColumnsLowerCase);
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
				boolean dupParent = false;
				if (pRow != null) {
					if (regPRows.contains(pRow.rowId)) {
						dupParent = true;
					}
					regPRows.add(pRow.rowId);
				}
				List<Row> newRows = new ArrayList<Row>();
				String rId = pRow == null? "" : pRow.rowId;
				if (newBlockRows.get(rId) != null) {
					newRows.addAll(newBlockRows.get(rId));
				}
				sortNewRows(newRows);
				if (parentRows != null) {
					if (!newRows.isEmpty()) {
						for (Row r: newRows) {
							r.setParentModelIndex(parentIndex);
						}
					}
					++parentIndex;
					for (Row row : newRows) {
						Row exRow = rowSet.get(row.rowId);
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
							rowSet.put(row.rowId, row);
							--limit;
						}
					}
				} else {
					rows.addAll(newRows);
					limit -= newRows.size();
				}
				if (limit <= 0) {
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
						if (a == null && b == null) {
							return 0;
						}
						if (a == null) {
							return -1;
						}
						if (b == null) {
							return 1;
						}
						if (a.getClass().equals(b.getClass())) {
							if (a instanceof Comparable<?>) {
								int cmp = ((Comparable) a).compareTo(b);
								return asc? cmp : -cmp;
							}
							return 0;
						}
						int cmp = a.getClass().getName().compareTo(b.getClass().getName());
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
	private void reloadRows(ResultSet inputResultSet, InlineViewStyle inlineViewStyle, String andCond, final List<Row> parentRows, final Map<String, List<Row>> rows, Object context, int limit, boolean useOLAPLimitation,
			String sqlLimitSuffix, Set<String> existingColumnsLowerCase) throws SQLException {
		reloadRows0(inputResultSet, inlineViewStyle, andCond, parentRows, rows, context, parentRows == null? limit : Math.max(5000, limit), useOLAPLimitation, sqlLimitSuffix, existingColumnsLowerCase);
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
	 * @param context
	 *            cancellation context
	 */
	private void reloadRows0(ResultSet inputResultSet, InlineViewStyle inlineViewStyle, String andCond, final List<Row> parentRows, final Map<String, List<Row>> rows, Object context, int limit, boolean useOLAPLimitation,
			String sqlLimitSuffix, Set<String> existingColumnsLowerCase) throws SQLException {
		String sql = "Select ";
		final Quoting quoting = new Quoting(session);
		final Set<String> pkColumnNames = new HashSet<String>();
		final Set<String> parentPkColumnNames = new HashSet<String>();
		final boolean selectParentPK = association != null && parentRows != null && parentRows.size() > 1;
		final Set<Integer> unknownColumnIndexes = new HashSet<Integer>();
		int numParentPKColumns = 0;
		
		if (table instanceof SqlStatementTable || statementForReloading != null) {
			sql = andCond;
			if (!(table instanceof SqlStatementTable)) {
				for (Column pk: rowIdSupport.getPrimaryKey(table).getColumns()) {
					pkColumnNames.add(quoting.requote(pk.name));
				}
			} else {
				table.setColumns(new ArrayList<Column>());
			}
		} else {
			String olapPrefix = "Select ";
			String olapSuffix = ") S Where S." + ROWNUMBERALIAS + " <= " + limit;
			boolean limitSuffixInSelectClause = sqlLimitSuffix != null &&
					(sqlLimitSuffix.toLowerCase().startsWith("top ") || sqlLimitSuffix.toLowerCase().startsWith("first "));
			if (sqlLimitSuffix != null && limitSuffixInSelectClause) {
				sql += (sqlLimitSuffix.replace("%s", Integer.toString(limit))) + " ";
			}
			int colI = 1;
			boolean f = true;
			if (selectParentPK) {
				int i = 0;
				for (Column column: rowIdSupport.getPrimaryKey(association.source).getColumns()) {
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
			
			for (Column column : rowIdSupport.getColumns(table)) {
				String name = quoting.requote(column.name);
				if (existingColumnsLowerCase != null && !existingColumnsLowerCase.contains(quoting.unquote(name).toLowerCase())) {
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
			String orderBy = "";
			String olapOrderBy = "";
			if (selectParentPK) {
				int j = 0;
				for (Column pk: rowIdSupport.getPrimaryKey(association.source).getColumns()) {
					parentPkColumnNames.add(quoting.requote(pk.name));
					orderBy += (f ? "" : ", ") + "B." + quoting.requote(pk.name);
					olapOrderBy += (f ? "" : ", ") + "S.B" + j;
					++j;
					f = false;
				}
			}
			int j = 0;
			for (Column pk: rowIdSupport.getPrimaryKey(table).getColumns()) {
				pkColumnNames.add(quoting.requote(pk.name));
				orderBy += (f ? "" : ", ") + "A." + quoting.requote(pk.name);
				olapOrderBy += (f ? "" : ", ") + "S.A" + j;
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
					throw new SqlException("Missing primary key for table: \"" + association.source.getName() + "\"   ", null, null);
				}
				if (parentRows.size() == 1) {
					sql += " Where (" + parentRows.get(0).rowId + ")";
				} else {
					StringBuilder sb = new StringBuilder();
					if (inlineViewStyle != null && association != null) {
						sb.append(" join ");
						List<String> columnNames = new ArrayList<String>();
						for (Column pkColumn: rowIdSupport.getPrimaryKey(association.source).getColumns()) {
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
				sql += (whereExists ? " and" : " Where") + " (" + ConditionEditor.toMultiLine(andCond) + ")";
			}
			if (orderBy.length() > 0) {
				if (sqlLimitSuffix != null && !useOLAPLimitation) {
					sql += " order by " + orderBy;
				}
			}
			olapPrefix += " From (";
			if (useOLAPLimitation) {
				sql = olapPrefix + sql + olapSuffix + " Order by " + olapOrderBy;
			}
			if (sqlLimitSuffix != null && !limitSuffixInSelectClause) {
				sql += " " + (sqlLimitSuffix.replace("%s", Integer.toString(limit)));
			}
		}
		
		if (sql.length() > 0 || inputResultSet != null) {
			final Map<Integer, Integer> pkPosToColumnPos = new HashMap<Integer, Integer>();
			if (!(table instanceof SqlStatementTable) && statementForReloading == null) {
				List<Column> pks = rowIdSupport.getPrimaryKey(table).getColumns();
				List<Column> columns = rowIdSupport.getColumns(table);
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
					for (int ci = 1 + finalNumParentPKColumns; ci <= columnCount; ++ci) {
						if (metaData instanceof MemorizedResultSetMetaData) {
							columnTypes[ci - 1 - finalNumParentPKColumns] = ((MemorizedResultSetMetaData) metaData).types[ci - 1];
						} else {
							columnTypes[ci - 1 - finalNumParentPKColumns] = metaData.getColumnType(ci);
						}
					}
					browserContentCellEditor = new BrowserContentCellEditor(columnTypes);
				}
				
				@Override
				public void readCurrentRow(ResultSet resultSet) throws SQLException {
					int i = 1, vi = 0;
					String parentRowId = "";
					if (selectParentPK) {
						Object v[] = new Object[rowIdSupport.getPrimaryKey(association.source).getColumns().size()];
						for (Column column: rowIdSupport.getPrimaryKey(association.source).getColumns()) {
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
					
					Object v[] = new Object[rowIdSupport.getColumns(table).size()];
					vi = 0;
					for (Column column: rowIdSupport.getColumns(table)) {
						readRowFromResultSet(pkColumnNames, resultSet, i, vi, "", v, column, pkColumn, pkColumnValue, unknownColumnIndexes);
						++i;
						++vi;
					}
					
					String rowId = "";
					String[] primaryKey = null;
					PrimaryKey primaryKeys = rowIdSupport.getPrimaryKey(table);
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
						int type = SqlUtil.getColumnType(resultSet, getMetaData(resultSet), i, typeCache);
						Object lob = null;
						if (type == 0) {
							lob = resultSet.getObject(i);
						}
						if (type == Types.BLOB || type == Types.CLOB || type == Types.NCLOB || type == Types.SQLXML
							|| (type == 0 &&
								(lob instanceof Blob || lob instanceof Clob || lob instanceof SQLXML)
								)) {
							Object object = resultSet.getObject(i);
							if (object == null || resultSet.wasNull()) {
								value = null;
							} else {
								Object lobValue = toLobRender(object);
								if (lobValue != null) {
									value = lobValue;
								}
							}
						} else {
							CellContentConverter cellContentConverter = getCellContentConverter(resultSet, session, session.dbms);
							Object o;
							try {
								o = cellContentConverter.getObject(resultSet, i);
								if (o instanceof byte[]) {
									final long length = ((byte[]) o).length;
									o = new LobValue() {
										@Override
										public String toString() {
											return "<Blob> " + length + " bytes";
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
								String cVal = cellContentConverter.toSql(o);
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
				session.executeQuery(sql, reader, null, context, limit);
			}
		}
	}

	/**
	 * True if row-limit is exceeded.
	 */
	private boolean isLimitExceeded = false;

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
		public int blockNr;
		public Object value;
		@Override
		public String toString() {
			if (value instanceof Double) {
				return SqlUtil.toString((Double) value);
			}
			if (value instanceof BigDecimal) {
				return SqlUtil.toString((BigDecimal) value);
			}
			return String.valueOf(value);
		}
	}
	
	private int lastLimit;
	private boolean lastLimitExceeded;
	
	/**
	 * Updates the model of the {@link #rowsTable}.
	 * 
	 * @param limit
	 *            row limit
	 * @param limitExceeded 
	 */
	private void updateTableModel() {
		updateTableModel(lastLimit, lastLimitExceeded);
	}
	
	/**
	 * Updates the model of the {@link #rowsTable}.
	 * 
	 * @param limit
	 *            row limit
	 * @param limitExceeded 
	 */
	private void updateTableModel(int limit, boolean limitExceeded) {
		lastLimit = limit;
		lastLimitExceeded = limitExceeded;
		pkColumns.clear();
		List<Column> columns = rowIdSupport.getColumns(table);
		String[] columnNames = new String[columns.size()];
		final Set<String> pkColumnNames = new HashSet<String>();
		if (rowIdSupport.getPrimaryKey(table) != null) {
			for (Column pk : rowIdSupport.getPrimaryKey(table).getColumns()) {
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
		if (rowIdSupport.getPrimaryKey(table) != null) {
			for (Column pk : rowIdSupport.getPrimaryKey(table).getColumns()) {
				pkColumnNames.add(pk.name);
			}
		}
		for (int i = 0; i < columnNames.length; ++i) {
			if (fkColumnNames.contains(columnNames[i])) {
				fkColumns.add(i);
			}
		}

		DefaultTableModel dtm;
		singleRowDetailsView = null;
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
					return isEditMode && r != null && browserContentCellEditor.isEditable(type, row, column, r.values[column]) && isPKComplete(type, r);
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
								updateMode("updating");
								
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
											session.execute(updateStatement, context);
										} catch (Exception e) {
											exception = e;
										} finally {
											CancellationHandler.reset(context);
											cancelLoadButton.removeActionListener(listener);
										}
										SwingUtilities.invokeLater(new Runnable() {
											@Override
											public void run() {
												if (exception != null && !(exception instanceof CancellationException)) {
													UIUtil.showException(BrowserContentPane.this, "Error", exception);
													updateMode("table");
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
						rowData[i] = asString.substring(0, asString.length() - HOUR.length());
					}
				}
				if (tableContentViewFilter != null) {
					tableContentViewFilter.filter(rowData, columnNameMap);
				}
				for (int i = 0; i < columns.size(); ++i) {
					TableModelItem item = new TableModelItem();
					item.blockNr = row.getParentModelIndex();
					item.value = rowData[i];
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
			rowsTable.setRowHeight(initialRowHeight);

			final int defaultSortColumn = getDefaultSortColumn();
			
			TableRowSorter<TableModel> sorter = new TableRowSorter<TableModel>(dtm) {
				@Override
				protected boolean useToString(int column) {
					return false;
				}

				@Override
			    public void toggleSortOrder(int column) {
			        List<? extends SortKey> sortKeys = getSortKeys();
			        if (sortKeys.size() > 0) {
			            if (sortKeys.get(0).getSortOrder() == SortOrder.DESCENDING) {
			                List<SortKey> sk = new ArrayList<SortKey>();
			                if (defaultSortColumn >= 0) {
			                	sk.add(new SortKey(defaultSortColumn, SortOrder.ASCENDING));
			                }
							setSortKeys(sk);
			                return;
			            }
			        }
			        super.toggleSortOrder(column);
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
							RowBrowser pb = getParentBrowser();
							if (pb != null) {
								if (pb.browserContentPane != null) {
									if (pb.browserContentPane.rowsTable != null) {
										pSorter = pb.browserContentPane.rowsTable.getRowSorter();
									}
								}
							}
							if (o1 instanceof TableModelItem && o2 instanceof TableModelItem) {
								int b1 = ((TableModelItem) o1).blockNr;
								int b2 = ((TableModelItem) o2).blockNr;
								if (pSorter != null) {
									int b;
									b = pSorter.convertRowIndexToView(b1);
									if (b < 0) {
										b = b1 + Integer.MAX_VALUE / 2;
									}
									b1 = b;
									b = pSorter.convertRowIndexToView(b2);
									if (b < 0) {
										b = b1 + Integer.MAX_VALUE / 2;
									}
									b2 = b;
								}
								if (b1 != b2) {
									return (b1 - b2) * (desc? -1 : 1);
								}
							}
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
										RowSorter chSorter = chBr.browserContentPane.rowsTable.getRowSorter();
										if (chSorter instanceof TableRowSorter) {
											((TableRowSorter) chSorter).sort();
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
			singleRowDetailsView = new DetailsView(Collections.singletonList(rows.get(0)), 1, dataModel, BrowserContentPane.this.table, 0, null, false, rowIdSupport) {
				@Override
				protected void onRowChanged(int row) {
				}
				@Override
				protected void onClose() {
				}
			};
			singleRowDetailsView.setSortColumns(sortColumnsCheckBox.isSelected());
	        dtm = new DefaultTableModel(new String[] { singleRowDetailsViewTitel }, 0) {
				@Override
				public boolean isCellEditable(int row, int column) {
					return false;
				}
			};
			for (Row row : rows) {
				dtm.addRow(new Object[] { row });
				if (++rn >= limit) {
					break;
				}
			}
			rowsTable.setModel(dtm);
			rowsTable.setRowHeight(singleRowDetailsView.getPreferredSize().height);
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
		int size = rows.size();
		if (size > limit) {
			size = limit;
		}
		rowsCount.setText((limitExceeded ? " more than " : " ") + size + " row" + (size != 1 ? "s" : ""));
		RowBrowser theParentWithExceededLimit = parentWithExceededLimit();
		rowsCount.setForeground(limitExceeded || theParentWithExceededLimit != null? Color.RED : new JLabel().getForeground());
		
		if (theParentWithExceededLimit == null) {
			rowsCount.setToolTipText(null);
		} else {
			rowsCount.setToolTipText("potentially incomplete because " + theParentWithExceededLimit.internalFrame.getTitle() + " exceeded row limit");
		}
		
		int nndr = noNonDistinctRows;
		if (noDistinctRows + noNonDistinctRows >= limit) {
			--nndr;
		}
		selectDistinctCheckBox.setVisible(nndr > 0);
		selectDistinctCheckBox.setText("select distinct (-" + nndr + " row" + (nndr == 1? "" : "s") + ")");

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
		appendClosure();
	}

	private int getDefaultSortColumn() {
		if (table == null || table instanceof SqlStatementTable || getQueryBuilderDialog() == null /* SQL Console */) {
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
		int MAXLINES = 2000;
		if (rowsTable.getColumnCount() > 0) {
			MAXLINES = Math.max(10 * MAXLINES / rowsTable.getColumnCount(), 10);
		}
		DefaultTableCellRenderer defaultTableCellRenderer = new DefaultTableCellRenderer();
		for (int i = 0; i < rowsTable.getColumnCount(); i++) {
			TableColumn column = rowsTable.getColumnModel().getColumn(i);
			int width = ((int) (Desktop.BROWSERTABLE_DEFAULT_WIDTH * getLayoutFactor()) - 18) / rowsTable.getColumnCount();

			Component comp = defaultTableCellRenderer.getTableCellRendererComponent(rowsTable, column.getHeaderValue(), false, false, 0, i);
			int pw = comp.getPreferredSize().width;
			if (pw < 100) {
				pw = (pw * 110) / 100 + 2;
			}
			width = Math.max(width, pw);

			int line = 0;
			for (; line < rowsTable.getRowCount(); ++line) {
				comp = rowsTable.getCellRenderer(line, i).getTableCellRendererComponent(rowsTable, dtm.getValueAt(line, i), false, false, line, i);
				width = Math.max(width, comp.getPreferredSize().width + (singleRowDetailsView == null ? 24 : 0));
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
				int maxValueWidth = comp.getPreferredSize().width + (singleRowDetailsView == null ? 16 : 0);
				if (maxValueWidth > width) {
					width = maxValueWidth;
				}
			}
			if (singleRowDetailsView == null) {
				width = Math.min(width, maxColumnWidth);
			}

			column.setPreferredWidth(width);
		}
	}

	protected int maxColumnWidth = 400;
	
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
        jPanel7 = new javax.swing.JPanel();
        loadButton = new javax.swing.JButton();
        onPanel = new javax.swing.JPanel();
        on = new javax.swing.JLabel();
        joinPanel = new javax.swing.JPanel();
        join = new javax.swing.JLabel();
        jLabel6 = new javax.swing.JLabel();
        jPanel10 = new javax.swing.JPanel();
        from = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        pendingNonpendingPanel = new javax.swing.JPanel();
        cardPanel = new javax.swing.JPanel();
        tablePanel = new javax.swing.JPanel();
        jLayeredPane1 = new javax.swing.JLayeredPane();
        rowsTableScrollPane = new javax.swing.JScrollPane();
        rowsTable = new javax.swing.JTable();
        jPanel6 = new javax.swing.JPanel();
        sortColumnsCheckBox = new javax.swing.JCheckBox();
        rowsCount = new javax.swing.JLabel();
        selectDistinctCheckBox = new javax.swing.JCheckBox();
        loadingPanel = new javax.swing.JPanel();
        jPanel1 = new javax.swing.JPanel();
        loadingLabel = new javax.swing.JLabel();
        cancelLoadButton = new javax.swing.JButton();
        jLabel2 = new javax.swing.JLabel();
        jPanel5 = new javax.swing.JPanel();
        jLabel10 = new javax.swing.JLabel();
        jPanel4 = new javax.swing.JPanel();
        jLabel8 = new javax.swing.JLabel();
        jPanel8 = new javax.swing.JPanel();
        jLabel11 = new javax.swing.JLabel();
        jLabel1 = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();
        andLabel = new javax.swing.JLabel();
        openEditorLabel = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        fetchLabel = new javax.swing.JLabel();
        jPanel3 = new javax.swing.JPanel();
        limitBox = new javax.swing.JComboBox();
        relatedRowsPanel = new javax.swing.JPanel();
        relatedRowsLabel = new javax.swing.JLabel();
        jPanel9 = new javax.swing.JPanel();
        sqlPanel = new javax.swing.JPanel();
        sqlLabel1 = new javax.swing.JLabel();
        dropA = new javax.swing.JLabel();
        dropB = new javax.swing.JLabel();

        andCondition.setEditable(true);
        andCondition.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));

        setLayout(new java.awt.GridBagLayout());

        jPanel7.setLayout(new java.awt.GridBagLayout());

        loadButton.setText(" Reload ");
        loadButton.addActionListener(new java.awt.event.ActionListener() {
            @Override
			public void actionPerformed(java.awt.event.ActionEvent evt) {
                loadButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        jPanel7.add(loadButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.gridwidth = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        add(jPanel7, gridBagConstraints);

        onPanel.setMinimumSize(new java.awt.Dimension(66, 17));
        onPanel.setLayout(new java.awt.BorderLayout());

        on.setFont(new java.awt.Font("DejaVu Sans", 0, 13)); // NOI18N
        on.setText("jLabel3");
        onPanel.add(on, java.awt.BorderLayout.CENTER);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.gridwidth = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHWEST;
        gridBagConstraints.weightx = 1.0;
        add(onPanel, gridBagConstraints);

        joinPanel.setMinimumSize(new java.awt.Dimension(66, 17));
        joinPanel.setLayout(new java.awt.GridBagLayout());

        join.setFont(new java.awt.Font("DejaVu Sans", 0, 13)); // NOI18N
        join.setText("jLabel3");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        joinPanel.add(join, gridBagConstraints);

        jLabel6.setFont(new java.awt.Font("DejaVu Sans", 1, 13)); // NOI18N
        jLabel6.setText(" as B  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        joinPanel.add(jLabel6, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHWEST;
        gridBagConstraints.weightx = 1.0;
        add(joinPanel, gridBagConstraints);

        jPanel10.setMinimumSize(new java.awt.Dimension(66, 17));
        jPanel10.setLayout(new java.awt.GridBagLayout());

        from.setFont(new java.awt.Font("DejaVu Sans", 0, 13)); // NOI18N
        from.setText("jLabel3");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        jPanel10.add(from, gridBagConstraints);

        jLabel5.setFont(new java.awt.Font("DejaVu Sans", 1, 13)); // NOI18N
        jLabel5.setHorizontalAlignment(javax.swing.SwingConstants.LEFT);
        jLabel5.setText(" as A");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel10.add(jLabel5, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        add(jPanel10, gridBagConstraints);

        pendingNonpendingPanel.setLayout(new java.awt.CardLayout());

        cardPanel.setLayout(new java.awt.CardLayout());

        tablePanel.setLayout(new java.awt.GridBagLayout());

        jLayeredPane1.setLayout(new java.awt.GridBagLayout());

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
        jLayeredPane1.add(rowsTableScrollPane, gridBagConstraints);

        jPanel6.setLayout(new java.awt.GridBagLayout());

        sortColumnsCheckBox.setText("sort columns   ");
        sortColumnsCheckBox.addActionListener(new java.awt.event.ActionListener() {
            @Override
			public void actionPerformed(java.awt.event.ActionEvent evt) {
                sortColumnsCheckBoxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        jPanel6.add(sortColumnsCheckBox, gridBagConstraints);

        rowsCount.setText("jLabel3");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        jPanel6.add(rowsCount, gridBagConstraints);

        selectDistinctCheckBox.setSelected(true);
        selectDistinctCheckBox.setText("select distinct (-100 rows)");
        selectDistinctCheckBox.addActionListener(new java.awt.event.ActionListener() {
            @Override
			public void actionPerformed(java.awt.event.ActionEvent evt) {
                selectDistinctCheckBoxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        jPanel6.add(selectDistinctCheckBox, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTH;
        jLayeredPane1.add(jPanel6, gridBagConstraints);

        loadingPanel.setOpaque(false);
        loadingPanel.setLayout(new java.awt.GridBagLayout());

        jPanel1.setBackground(new Color(255,255,255,150));
        jPanel1.setLayout(new java.awt.GridBagLayout());

        loadingLabel.setFont(new java.awt.Font("DejaVu Sans", 1, 14)); // NOI18N
        loadingLabel.setForeground(new java.awt.Color(141, 16, 16));
        loadingLabel.setText("loading...     ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(8, 8, 8, 0);
        jPanel1.add(loadingLabel, gridBagConstraints);

        cancelLoadButton.setText("Cancel");
        cancelLoadButton.addActionListener(new java.awt.event.ActionListener() {
            @Override
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

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        tablePanel.add(jLayeredPane1, gridBagConstraints);

        cardPanel.add(tablePanel, "table");

        jPanel5.setLayout(new java.awt.GridBagLayout());

        jLabel10.setFont(new java.awt.Font("DejaVu Sans", 1, 14)); // NOI18N
        jLabel10.setForeground(new java.awt.Color(141, 16, 16));
        jLabel10.setText("Error");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(8, 8, 8, 0);
        jPanel5.add(jLabel10, gridBagConstraints);

        cardPanel.add(jPanel5, "error");

        jPanel4.setLayout(new java.awt.GridBagLayout());

        jLabel8.setFont(new java.awt.Font("DejaVu Sans", 1, 14)); // NOI18N
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

        jLabel11.setFont(new java.awt.Font("DejaVu Sans", 1, 14)); // NOI18N
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
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 12;
        gridBagConstraints.gridwidth = 7;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(pendingNonpendingPanel, gridBagConstraints);

        jLabel1.setFont(new java.awt.Font("DejaVu Sans", 1, 13)); // NOI18N
        jLabel1.setText(" Join ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        add(jLabel1, gridBagConstraints);

        jLabel4.setFont(new java.awt.Font("DejaVu Sans", 1, 13)); // NOI18N
        jLabel4.setText(" On ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        add(jLabel4, gridBagConstraints);

        andLabel.setFont(new java.awt.Font("DejaVu Sans", 1, 13)); // NOI18N
        andLabel.setText(" Where ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        add(andLabel, gridBagConstraints);

        openEditorLabel.setText(" And  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        add(openEditorLabel, gridBagConstraints);

        jLabel3.setFont(new java.awt.Font("DejaVu Sans", 1, 13)); // NOI18N
        jLabel3.setText(" From ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        add(jLabel3, gridBagConstraints);

        fetchLabel.setFont(new java.awt.Font("DejaVu Sans", 1, 13)); // NOI18N
        fetchLabel.setText(" Limit  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 9;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.VERTICAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        add(fetchLabel, gridBagConstraints);

        jPanel3.setLayout(new javax.swing.BoxLayout(jPanel3, javax.swing.BoxLayout.LINE_AXIS));

        limitBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        limitBox.addItemListener(new java.awt.event.ItemListener() {
            @Override
			public void itemStateChanged(java.awt.event.ItemEvent evt) {
                limitBoxItemStateChanged(evt);
            }
        });
        jPanel3.add(limitBox);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 9;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        add(jPanel3, gridBagConstraints);

        relatedRowsPanel.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
        relatedRowsPanel.setLayout(new javax.swing.BoxLayout(relatedRowsPanel, javax.swing.BoxLayout.LINE_AXIS));

        relatedRowsLabel.setBackground(new java.awt.Color(224, 240, 255));
        relatedRowsLabel.setText(" Related Rows ");
        relatedRowsLabel.setOpaque(true);
        relatedRowsPanel.add(relatedRowsLabel);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 5;
        gridBagConstraints.gridy = 9;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 2);
        add(relatedRowsPanel, gridBagConstraints);

        jPanel9.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 5;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 0);
        add(jPanel9, gridBagConstraints);

        sqlPanel.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
        sqlPanel.setLayout(new javax.swing.BoxLayout(sqlPanel, javax.swing.BoxLayout.LINE_AXIS));

        sqlLabel1.setText(" Menu ");
        sqlPanel.add(sqlLabel1);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 8;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.gridheight = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHEAST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 2, 2);
        add(sqlPanel, gridBagConstraints);

        dropA.setFont(new java.awt.Font("DejaVu Sans", 1, 13)); // NOI18N
        dropA.setText("drop");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 4;
        add(dropA, gridBagConstraints);

        dropB.setFont(new java.awt.Font("DejaVu Sans", 1, 13)); // NOI18N
        dropB.setText("drop");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 5;
        add(dropB, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

		private void cancelLoadButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelLoadButtonActionPerformed
			cancelLoadJob(false);
			updateMode("cancelled");
		}//GEN-LAST:event_cancelLoadButtonActionPerformed

		private void selectDistinctCheckBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_selectDistinctCheckBoxActionPerformed
			reloadRows();
		}//GEN-LAST:event_selectDistinctCheckBoxActionPerformed

    private void sortColumnsCheckBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_sortColumnsCheckBoxActionPerformed
        updateTableModel();
    }//GEN-LAST:event_sortColumnsCheckBoxActionPerformed

	private void loadButtonActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_loadButtonActionPerformed
		if (System.currentTimeMillis() - lastReloadTS > 200) {
			reloadRows();
		}
	}// GEN-LAST:event_loadButtonActionPerformed

	private void limitBoxItemStateChanged(java.awt.event.ItemEvent evt) {// GEN-FIRST:event_limitBoxItemStateChanged
		reloadRows();
	}// GEN-LAST:event_limitBoxItemStateChanged

	private void openQueryBuilder(boolean openSQLConsole) {
		QueryBuilderDialog.Relationship root = createQBRelations(true);
		if (root != null) {
			root.selectColumns = true;
			getQueryBuilderDialog().buildQuery(table, root, dataModel, session, getMetaDataSource(), openSQLConsole);
		}
	}

    // Variables declaration - do not modify//GEN-BEGIN:variables
    javax.swing.JComboBox andCondition;
    private javax.swing.JLabel andLabel;
    private javax.swing.JButton cancelLoadButton;
    private javax.swing.JPanel cardPanel;
    private javax.swing.JLabel dropA;
    private javax.swing.JLabel dropB;
    private javax.swing.JLabel fetchLabel;
    private javax.swing.JLabel from;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel10;
    private javax.swing.JLabel jLabel11;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLayeredPane jLayeredPane1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel10;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JPanel jPanel7;
    private javax.swing.JPanel jPanel8;
    private javax.swing.JPanel jPanel9;
    private javax.swing.JLabel join;
    private javax.swing.JPanel joinPanel;
    javax.swing.JComboBox limitBox;
    public javax.swing.JButton loadButton;
    private javax.swing.JLabel loadingLabel;
    private javax.swing.JPanel loadingPanel;
    private javax.swing.JLabel on;
    private javax.swing.JPanel onPanel;
    private javax.swing.JLabel openEditorLabel;
    private javax.swing.JPanel pendingNonpendingPanel;
    private javax.swing.JLabel relatedRowsLabel;
    private javax.swing.JPanel relatedRowsPanel;
    public javax.swing.JLabel rowsCount;
    public javax.swing.JTable rowsTable;
    protected javax.swing.JScrollPane rowsTableScrollPane;
    javax.swing.JCheckBox selectDistinctCheckBox;
    public javax.swing.JCheckBox sortColumnsCheckBox;
    private javax.swing.JLabel sqlLabel1;
    private javax.swing.JPanel sqlPanel;
    private javax.swing.JPanel tablePanel;
    // End of variables declaration//GEN-END:variables

	private ConditionEditor andConditionEditor;
	private Icon conditionEditorIcon;
	private Icon conditionEditorSelectedIcon;
	{
		String dir = "/net/sf/jailer/ui/resource";

		// load images
		try {
			conditionEditorIcon = new ImageIcon(getClass().getResource(dir + "/edit.png"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		try {
			conditionEditorSelectedIcon = new ImageIcon(getClass().getResource(dir + "/edit_s.png"));
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Cancels current load job.
	 * @param propagate 
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

	private void updateMode(String mode) {
		if ("table".equals(mode)) {
			loadingPanel.setVisible(false);
			rowsTable.setEnabled(true);
		} else if ("loading".equals(mode)) {
			mode = "table";
			loadingPanel.setVisible(true);
			loadingLabel.setText("loading...");
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
		((CardLayout) cardPanel.getLayout()).show(cardPanel, mode);
	}

	/**
	 * Opens a drop-down box which allows the user to select columns for restriction definitions.
	 */
	private void openColumnDropDownBox(JLabel label, String alias, Table table) {
		JPopupMenu popup = new JScrollPopupMenu();
		List<String> columns = new ArrayList<String>();
		
		for (Column c: table.getColumns()) {
			columns.add(alias + "." + c.name);
		}
		
		for (final String c: columns) {
			if (c.equals("")) {
				popup.add(new JSeparator());
				continue;
			}
			JMenuItem m = new JMenuItem(c);
			m.addActionListener(new ActionListener () {
				@Override
				public void actionPerformed(ActionEvent e) {
					if (andCondition.isEnabled()) {
						if (andCondition.isEditable()) {
							if (andCondition.getEditor() != null && (andCondition.getEditor().getEditorComponent() instanceof JTextField)) {
								JTextField f = ((JTextField) andCondition.getEditor().getEditorComponent());
								int pos = f.getCaretPosition();
								String current = f.getText();
								if (pos < 0 || pos >= current.length()) {
									setAndCondition(current + c, false);
								} else {
									setAndCondition(current.substring(0, pos) + c + current.substring(pos), false);
									f.setCaretPosition(pos + c.length());
								}
							}
							andCondition.grabFocus();
						}
					}
				}
			});
			popup.add(m);
		}
		UIUtil.fit(popup);
		popup.show(label, 0, label.getHeight());
	}

	/**
	 * Creates new row. Fills in foreign key.
	 * 
	 * @param parentrow row holding the primary key
	 * @param table the table of the new row
	 * @return new row of table
	 */
	private Row createNewRow(Row parentrow, Table table) {
		Row row = new Row(null, null, new Object[table.getColumns().size()]);
		if (parentrow != null && association != null) {
			Map<Column, Column> sToDMap = association.createSourceToDestinationKeyMapping();
			for (Map.Entry<Column, Column> e: sToDMap.entrySet()) {
				int iS = -1;
				for (int i = 0; i < association.source.getColumns().size(); ++i) {
					if (e.getKey() == association.source.getColumns().get(i)) {
						iS = i;
						break;
					}
				}
				int iD = -1;
				for (int i = 0; i < association.destination.getColumns().size(); ++i) {
					if (e.getValue() == association.destination.getColumns().get(i)) {
						iD = i;
						break;
					}
				}
				if (iS >= 0 && iD >= 0) {
					row.values[iD] = parentrow.values[iS];
				}
			}
		}
		return row;
	}

	protected abstract void navigateTo(Association association, int rowIndex, Row row);

	protected abstract void onContentChange(List<Row> rows, boolean reloadChildren);

	protected abstract void onRedraw();
	protected abstract void onHide();

	protected abstract void beforeReload();

	protected abstract QueryBuilderDialog.Relationship createQBRelations(boolean withParents);
	protected abstract List<QueryBuilderDialog.Relationship> createQBChildrenRelations(RowBrowser tabu, boolean all);

	protected abstract void addRowToRowLink(Row pRow, Row exRow);

	protected abstract JFrame getOwner();

	protected abstract void findClosure(Row row);
	protected abstract void findClosure(Row row, Set<Pair<BrowserContentPane, Row>> closure, boolean forward);

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
	
	public interface RunnableWithPriority extends Runnable {
		int getPriority();
	};
	
	protected abstract PriorityBlockingQueue<RunnableWithPriority> getRunnableQueue();
	
	/**
	 * Collect layout of tables in a extraction model.
	 * 
	 * @param positions to put positions into
	 */
	protected abstract void collectPositions(Map<String, Map<String, double[]>> positions);


	private void openDetails(final int x, final int y) {
		final JDialog d = new JDialog(getOwner(), (table instanceof SqlStatementTable)? "" : dataModel.getDisplayName(table), true);
		d.getContentPane().add(new DetailsView(rows, rowsTable.getRowCount(), dataModel, table, 0, rowsTable.getRowSorter(), true, rowIdSupport) {
			@Override
			protected void onRowChanged(int row) {
				setCurrentRowSelection(row);
			}
			@Override
			protected void onClose() {
				d.setVisible(false);
			}
		});
		d.pack();
		d.setLocation(x, y);
		d.setSize(400, d.getHeight() + 20);
		UIUtil.fit(d);
		d.setVisible(true);
		setCurrentRowSelection(-1);
		onRedraw();
	}

	private void updateWhereField() {
		if (association != null) {
			if (parentRow == null && parentRows != null && parentRows.size() > 0) {
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
				andLabel.setToolTipText(UIUtil.toHTML(toolTip, 0));
			} else {
				andLabel.setToolTipText(null);
			}
		} else {
			andLabel.setToolTipText(null);
		}
	}

	public void convertToRoot() {
		association = null;
		parentRow = null;
		parentRows = null;
		currentClosureRowIDs.clear();
		adjustGui();
		reloadRows();
	}

	private void openDetailsView(int rowIndex, int x, int y) {
		final JDialog d = new JDialog(getOwner(), (table instanceof SqlStatementTable)? "" : dataModel.getDisplayName(table), true);
		d.getContentPane().add(new DetailsView(rows, rowsTable.getRowCount(), dataModel, table, rowIndex, rowsTable.getRowSorter(), true, rowIdSupport) {
			@Override
			protected void onRowChanged(int row) {
				setCurrentRowSelection(row);
			}
			@Override
			protected void onClose() {
				d.setVisible(false);
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
		d.setVisible(true);
		setCurrentRowSelection(-1);
		onRedraw();
		}

	private static TableContentViewFilter tableContentViewFilter = TableContentViewFilter.create();
	
	private Icon dropDownIcon;
	private ImageIcon relatedRowsIcon;
	private ImageIcon redDotIcon;
	private ImageIcon blueDotIcon;
	private ImageIcon greenDotIcon;
	private ImageIcon greyDotIcon;
	{
		String dir = "/net/sf/jailer/ui/resource";
		
		// load images
		try {
			dropDownIcon = new ImageIcon(getClass().getResource(dir + "/dropdown.png"));
			relatedRowsIcon = new ImageIcon(getClass().getResource(dir + "/right.png"));
			redDotIcon = new ImageIcon(getClass().getResource(dir + "/reddot.gif"));
			blueDotIcon = new ImageIcon(getClass().getResource(dir + "/bluedot.gif"));
			greenDotIcon = new ImageIcon(getClass().getResource(dir + "/greendot.gif"));
			greyDotIcon = new ImageIcon(getClass().getResource(dir + "/greydot.gif"));
		} catch (Exception e) {
			e.printStackTrace();
		}
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
			if (content == null || content instanceof TableModelItem && (((TableModelItem) content).value == UIUtil.NULL || ((TableModelItem) content).value == null)) {
				return false;
			}
		}
		return true;
	}

	private Map<Integer, Table> typePerColumn = new HashMap<Integer, Table>();
	private Map<Table, int[]> pkColumnIndexes = new HashMap<Table, int[]>();

	private String[] alternativeColumnLabels;
	
	public void setAlternativeColumnLabels(String[] columnLabels) {
		this.alternativeColumnLabels = columnLabels;
	}

	private static String readCharacterStream(final Reader reader)
			throws IOException {
		final StringBuilder sb = new StringBuilder();
		final BufferedReader br = new BufferedReader(reader);

		int b;
		while(-1 != (b = br.read()))
		{
			sb.append((char)b);
			if (sb.length() > MAXLOBLENGTH) {
				sb.append("...");
				break;
			}
		}
		br.close();
		return sb.toString();
	}

	private static String readClob(Clob clob) throws SQLException, IOException {
		return readCharacterStream(clob.getCharacterStream());
	}

	private static String readSQLXML(SQLXML xml) throws SQLException, IOException {
		return readCharacterStream(xml.getCharacterStream());
	}

	public static Object toLobRender(Object object) {
		Object value = null;
		if (object instanceof Blob) {
			try {
				final long length = ((Blob) object).length();
				value = new LobValue() {
					@Override
					public String toString() {
						return "<Blob> " + length + " bytes";
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
	
}
