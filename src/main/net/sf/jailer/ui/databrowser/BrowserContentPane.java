/*
 * Copyright 2007 the original author or authors.
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
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.ResultSet;
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
import java.util.TreeSet;
import java.util.concurrent.LinkedBlockingQueue;

import javax.swing.DefaultComboBoxModel;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JInternalFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.JTable;
import javax.swing.JTree;
import javax.swing.RowSorter;
import javax.swing.SwingUtilities;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableModel;
import javax.swing.table.TableRowSorter;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;

import net.sf.jailer.CommandLineParser;
import net.sf.jailer.Configuration;
import net.sf.jailer.ScriptFormat;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Cardinality;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.PrimaryKey;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;
import net.sf.jailer.ui.ConditionEditor;
import net.sf.jailer.ui.DbConnectionDialog;
import net.sf.jailer.ui.ExtractionModelFrame;
import net.sf.jailer.ui.QueryBuilderDialog;
import net.sf.jailer.ui.QueryBuilderDialog.Relationship;
import net.sf.jailer.ui.RestrictionDefinition;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.Desktop.RowBrowser;
import net.sf.jailer.util.CancellationException;
import net.sf.jailer.util.CancellationHandler;
import net.sf.jailer.util.Pair;
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
	 * Allowed row limits.
	 */
	private static final Integer[] ROW_LIMITS = new Integer[] { 200, 500, 1000, 2000, 5000, 10000, 20000, 50000, 70000, 100000 };

	/**
	 * Concurrently loads rows.
	 */
	private final class LoadJob {
		private List<Row> rows = Collections.synchronizedList(new ArrayList<Row>());
		private Exception exception;
		private boolean isCanceled = false;
		private final int limit;
		private final String andCond;
		private final boolean selectDistinct;
		
		public LoadJob(int limit, String andCond, boolean selectDistinct) {
			this.andCond = andCond;
			this.selectDistinct = selectDistinct;
			synchronized (this) {
				this.limit = limit;
			}
		}

		public void run() {
			int l;
			synchronized (this) {
				l = limit;
				if (isCanceled) {
					CancellationHandler.reset(this);
					return;
				}
			}
			try {
				reloadRows(andCond, rows, this, l + 1, selectDistinct);
			} catch (SQLException e) {
				CancellationHandler.reset(this);
				synchronized (rows) {
					exception = e;
				}
			} catch (CancellationException e) {
				Session._log.info("cancelled");
				return;
			}
			SwingUtilities.invokeLater(new Runnable() {
				@Override
				public void run() {
					Exception e;
					int l;
					boolean limitExceeded = false;
					synchronized (rows) {
						e = exception;
						l = limit;
						if (rows.size() > limit) {
							limitExceeded = true;
							rows.remove(rows.size() - 1);
						}
						isCanceled = true; // done
					}
					if (e != null) {
						updateMode("error");
						if (!asking) {
							try {
								if (table.getName() != null && !table.exists(session, JDBCMetaDataBasedModelElementFinder.getDefaultSchema(session, session.getSchemaName()))) {
									asking = true;
									String schemaMappingOption = "Schema Mapping";
									switch (JOptionPane.showOptionDialog(BrowserContentPane.this, "Table \"" + table.getName() + "\" not found!", "Unknown table", JOptionPane.YES_NO_OPTION, JOptionPane.INFORMATION_MESSAGE, null, new Object[] { "Cancel", schemaMappingOption, "Analyze Database" }, "Cancel")) {
									case 1: openSchemaMappingDialog(); break;
									case 2: openSchemaAnalyzer(); break;
									}
									asking = false;
								} else {
									UIUtil.showException(BrowserContentPane.this, "Error", e);
								}
							} catch (Exception e1) {
								asking = false;
								UIUtil.showException(BrowserContentPane.this, "Error", e);
							}
						}
					} else {
						Set<String> prevIDs = new TreeSet<String>();
						int prevSize = 0;
						if (BrowserContentPane.this.rows != null) {
							prevSize = BrowserContentPane.this.rows.size();
							for (Row r: BrowserContentPane.this.rows) {
								prevIDs.add(r.rowId);
							}
						}
						onContentChange(new ArrayList<Row>(), false);
						BrowserContentPane.this.rows.clear();
						BrowserContentPane.this.rows.addAll(rows);
						updateTableModel(l, limitExceeded);
						Set<String> currentIDs = new TreeSet<String>();
						if (rows != null) {
							for (Row r: rows) {
								currentIDs.add(r.rowId);
							}
						}
						onContentChange(rows, rows.isEmpty() || rows.size() != prevSize || !prevIDs.equals(currentIDs) || rows.size() != currentIDs.size());
						updateMode("table");
						updateWhereField();
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
			}
			CancellationHandler.cancel(this);
		}
	}
	
	private static boolean asking = false;
	
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
	DataModel dataModel;

	/**
	 * {@link Association} with parent row, or <code>null</code>.
	 */
	private Association association;

	/**
	 * Rows to render.
	 */
	List<Row> rows = new ArrayList<Row>();

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
	 * DB session.
	 */
	Session session;

	private int currentRowSelection = -1;

	private List<Row> parentRows;
	protected final Set<Pair<BrowserContentPane, Row>> currentClosure;
	protected Set<Pair<BrowserContentPane, String>> currentClosureRowIDs;
	private DetailsView singleRowDetailsView;
	private int initialRowHeight;
	public SQLBrowserContentPane sqlBrowserContentPane;

	private boolean suppressReload;

	/**
	 * For concurrent reload of rows.
	 */
	private static final LinkedBlockingQueue<LoadJob> runnableQueue = new LinkedBlockingQueue<LoadJob>();

	/**
	 * Alias for row number column.
	 */
	private static final String ROWNUMBERALIAS = "RN";

	/**
	 * Maximum number of concurrent DB connections.
	 */
	private static int MAX_CONCURRENT_CONNECTIONS = 5;

	static {
		// initialize listeners for #runnableQueue
		for (int i = 0; i < MAX_CONCURRENT_CONNECTIONS; ++i) {
			Thread t = new Thread(new Runnable() {
				@Override
				public void run() {
					for (;;) {
						try {
							runnableQueue.take().run();
						} catch (InterruptedException e) {
							// ignore
						}
					}
				}
			}, "browser-" + i);
			t.setDaemon(true);
			t.start();
		}
	}
	
	static class SqlStatementTable extends Table {
		public SqlStatementTable(String name, PrimaryKey primaryKey, boolean defaultUpsert) {
			super(name, primaryKey, defaultUpsert);
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
	 * @param selectDistinct 
	 * @param limit 
	 * @param selectDistinct 
	 * @param limit 
	 * @param reload 
	 */
	public BrowserContentPane(final DataModel dataModel, final Table table, String condition, Session session, Row parentRow, List<Row> parentRows,
			final Association association, Frame parentFrame, Set<Pair<BrowserContentPane, Row>> currentClosure, Set<Pair<BrowserContentPane, String>> currentClosureRowIDs, Integer limit, Boolean selectDistinct, boolean reload) {
		this.table = table;
		this.session = session;
		this.dataModel = dataModel;
		this.parentRow = parentRow;
		this.parentRows = parentRows;
		this.association = association;
		this.currentClosure = currentClosure;
		this.currentClosureRowIDs = currentClosureRowIDs;

		suppressReload = true;
		
		if (table == null) {
			this.table = new SqlStatementTable(null, null, false);
		}
		
		initComponents();
		
		dropA.setText(null);
		dropA.setIcon(dropDownIcon);
		dropA.addMouseListener(new java.awt.event.MouseAdapter() {
			public void mousePressed(java.awt.event.MouseEvent evt) {
				openColumnDropDownBox(dropA, "A", table);
			}
			
			public void mouseEntered(java.awt.event.MouseEvent evt) {
				dropA.setEnabled(false);
            }
            public void mouseExited(java.awt.event.MouseEvent evt) {
            	dropA.setEnabled(true);
           }
        });
		
		if (association != null) {
			dropB.setText(null);
			dropB.setIcon(dropDownIcon);
			dropB.addMouseListener(new java.awt.event.MouseAdapter() {
				public void mousePressed(java.awt.event.MouseEvent evt) {
					openColumnDropDownBox(dropB, "B", association.source);
				}
				
				public void mouseEntered(java.awt.event.MouseEvent evt) {
					dropB.setEnabled(false);
	            }
	            public void mouseExited(java.awt.event.MouseEvent evt) {
	            	dropB.setEnabled(true);
	           }
	        });
		}
		
		andCondition.addKeyListener(new KeyAdapter() {
			public void keyPressed(KeyEvent e) {
				int key = e.getKeyCode();
				if (key == KeyEvent.VK_ENTER) {
					reloadRows();
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
				if (!(graphics instanceof Graphics2D))
					return;
				RowSorter<? extends TableModel> rowSorter = rowsTable.getRowSorter();
				for (int i = 0; i < rowsTable.getRowCount(); ++i) {
					if (rowSorter.convertRowIndexToView(i) != i) {
						return;
					}
				}
				int maxI = Math.min(rowsTable.getRowCount(), rows.size());

				for (int i = 0; i < maxI; ++i) {
					if (rows.get(i).isBlockEnd()) {
						Graphics2D g2d = (Graphics2D) graphics;
						g2d.setColor(color);
						g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
						g2d.setStroke(new BasicStroke(1));
						Rectangle r = rowsTable.getCellRect(i, 0, false);
						x[0] = (int) r.getMinX();
						y[0] = (int) r.getMaxY();
						r = rowsTable.getCellRect(i, rowsTable.getColumnCount() - 1, false);
						x[1] = (int) r.getMaxX();
						y[1] = (int) r.getMaxY();
						g2d.drawPolyline(x, y, 2);
					}
				}
			}
		};

		rowsTable.setAutoCreateRowSorter(true);
		rowsTable.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
		rowsTableScrollPane.setViewportView(rowsTable);

		andCondition.setText(ConditionEditor.toSingleLine(condition));
		from.setText(table == null? "" : this.dataModel.getDisplayName(table));
		adjustGui();
		rowsTable.setShowGrid(false);
		final TableCellRenderer defaultTableCellRenderer = rowsTable.getDefaultRenderer(String.class);

		rowsTable.setDefaultRenderer(Object.class, new TableCellRenderer() {

			final Color BG1 = new Color(255, 255, 255);
			final Color BG2 = new Color(230, 255, 255);
			final Color BG3 = new Color(190, 195, 255);
			final Color FG1 = new Color(155, 0, 0);
			final Font font = new JLabel().getFont();
			final Font nonbold = new Font(font.getName(), font.getStyle() & ~Font.BOLD, font.getSize());
			final Font bold = new Font(font.getName(), font.getStyle() | Font.BOLD, font.getSize());

			public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
				isSelected = currentRowSelection == row || currentRowSelection == -2;

				Component render = defaultTableCellRenderer.getTableCellRendererComponent(table, value, isSelected, false, row, column);
				
				if (value instanceof Row) {
					Row theRow = (Row) value;
					Pair<BrowserContentPane, String> pair = new Pair<BrowserContentPane, String>(BrowserContentPane.this, theRow.rowId);
					singleRowDetailsView.setBorderColor(isSelected? render.getBackground() : BrowserContentPane.this.currentClosureRowIDs.contains(pair)? BG3 : Color.WHITE);
					return singleRowDetailsView;
				}

				final RowSorter<?> rowSorter = rowsTable.getRowSorter();
				if (render instanceof JLabel) {
					if (!isSelected) {
						if (row < rows.size() && BrowserContentPane.this.currentClosureRowIDs.contains(new Pair<BrowserContentPane, String>(BrowserContentPane.this, rows.get(rowSorter.convertRowIndexToModel(row)).rowId))) {
							((JLabel) render).setBackground(BG3);
						} else {
							((JLabel) render).setBackground((row % 2 == 0) ? BG1 : BG2);
						}
					}
					((JLabel) render).setForeground(pkColumns.contains(rowsTable.convertColumnIndexToModel(column)) ? FG1 : Color.BLACK);
					try {
						((JLabel) render).setFont(highlightedRows.contains(rowSorter.convertRowIndexToModel(row)) ? bold : nonbold);
					} catch (Exception e) {
						// ignore
					}
				}
				return render;
			}
		});
		rowsTable.setRowSelectionAllowed(false);
		rowsTable.setColumnSelectionAllowed(false);
		rowsTable.setCellSelectionEnabled(false);
		rowsTable.setEnabled(false);
		rowsTable.addMouseListener(new MouseListener() {
			private JPopupMenu lastMenu;

			@Override
			public void mouseReleased(MouseEvent e) {
			}

			@Override
			public void mousePressed(MouseEvent e) {
				int ri = rowsTable.rowAtPoint(e.getPoint());
				if (ri >= 0) {
					int i = rowsTable.getRowSorter().convertRowIndexToModel(ri);
					Row row = rows.get(i);

					if (lastMenu == null || !lastMenu.isVisible()) {
						if (e.getButton() != MouseEvent.BUTTON1 || e.getClickCount() > 1) {
							currentRowSelection = ri;
							onRedraw();
//							setCurrentRowSelection(ri);
							Rectangle r = rowsTable.getCellRect(ri, 0, false);
							int x = Math.max((int) e.getPoint().x, (int) r.getMinX());
							int y = (int) r.getMaxY() - 2;
							if (singleRowDetailsView != null) {
								y = e.getY();
							}
							Point p = SwingUtilities.convertPoint(rowsTable, x, y, null);
							JPopupMenu popup;
							popup = createPopupMenu(row, i, p.x + getOwner().getX(), p.y + getOwner().getY(), rows.size() == 1);
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
							lastMenu = popup;
						} else {
							setCurrentRowSelection(ri);
							setCurrentRowSelection(-1);
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

		andConditionEditor = new ConditionEditor(parentFrame, null);
		openEditorLabel.setIcon(conditionEditorIcon);
		openEditorLabel.setText(null);
		openEditorLabel.addMouseListener(new java.awt.event.MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				String cond = andConditionEditor.edit(andCondition.getText(), "Table", "A", table, null, null, null, false);
				if (cond != null) {
					if (!andCondition.getText().equals(ConditionEditor.toSingleLine(cond))) {
						andCondition.setText(ConditionEditor.toSingleLine(cond));
						loadButton.grabFocus();
						reloadRows();
					}
				}
				openEditorLabel.setIcon(conditionEditorSelectedIcon);
			}

			public void mouseEntered(java.awt.event.MouseEvent evt) {
				openEditorLabel.setIcon(conditionEditorSelectedIcon);
			}

			public void mouseExited(java.awt.event.MouseEvent evt) {
				openEditorLabel.setIcon(conditionEditorIcon);
			}
		});
		relatedRowsPanel.addMouseListener(new java.awt.event.MouseAdapter() {
			private JPopupMenu popup;
			private boolean in = false;

			@Override
			public void mousePressed(MouseEvent e) {
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

			public void mouseEntered(java.awt.event.MouseEvent evt) {
				in = true;
				updateBorder();
			}

			public void mouseExited(java.awt.event.MouseEvent evt) {
				in = false;
				updateBorder();
			}

			private void updateBorder() {
				relatedRowsPanel.setBorder(new javax.swing.border.SoftBevelBorder((in || popup != null) ? javax.swing.border.BevelBorder.LOWERED
						: javax.swing.border.BevelBorder.RAISED));
			}
		});
		sqlPanel.addMouseListener(new java.awt.event.MouseAdapter() {
			private JPopupMenu popup;
			private boolean in = false;

			@Override
			public void mousePressed(MouseEvent e) {
				popup = createSqlPopupMenu(BrowserContentPane.this.parentRow, 0, 300, 300);
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

			public void mouseEntered(java.awt.event.MouseEvent evt) {
				in = true;
				updateBorder();
			}

			public void mouseExited(java.awt.event.MouseEvent evt) {
				in = false;
				updateBorder();
			}

			private void updateBorder() {
				sqlPanel.setBorder(new javax.swing.border.SoftBevelBorder((in || popup != null) ? javax.swing.border.BevelBorder.LOWERED
						: javax.swing.border.BevelBorder.RAISED));
			}
		});
		limitBox.setModel(new DefaultComboBoxModel(ROW_LIMITS));
		limitBox.setSelectedIndex(association == null? 0 : 1);
		if (limit != null) {
			limitBox.setSelectedItem(limit);
		}
		if (selectDistinct != null) {
			selectDistinctCheckBox.setSelected(selectDistinct);
		}
		updateTableModel(0, false);
		
		if (this.table instanceof SqlStatementTable) {
			sqlBrowserContentPane = new SQLBrowserContentPane();
			removeAll();
			GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 1;
	        gridBagConstraints.gridy = 1;
	        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
	        gridBagConstraints.weightx = 1.0;
	        gridBagConstraints.weighty = 1.0;
	        add(sqlBrowserContentPane, gridBagConstraints);
	        gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 2;
	        gridBagConstraints.gridy = 5;
	        gridBagConstraints.gridwidth = 2;
	        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
	        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
	        gridBagConstraints.weightx = 0;
	        gridBagConstraints.weighty = 0;
	        sqlBrowserContentPane.editorPanel.add(limitBox, gridBagConstraints);
	        sqlBrowserContentPane.rowListPanel.add(cardPanel, java.awt.BorderLayout.CENTER);
	        sqlBrowserContentPane.sqlEditorPane.setContentType("text/sql");
	        sqlBrowserContentPane.sqlEditorPane.setText(condition);
	        sqlBrowserContentPane.sqlEditorPane.setCaretPosition(0);
	        sqlBrowserContentPane.reloadButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent arg0) {
					reloadRows();
				}
			});
	        sqlBrowserContentPane.detailsButton.setEnabled(false);
	        sqlBrowserContentPane.detailsButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					openDetails(300, 300);
				}
			});
		}
		
		suppressReload = false;
		if (reload) {
			reloadRows();
		}
	}

	private void adjustGui() {
		if (this.association == null) {
			joinPanel.setVisible(false);
			onPanel.setVisible(false);
			wherePanel.setVisible(false);
			
			jLabel1.setText(" ");
			jLabel4.setText(" ");
			jLabel9.setText(" ");
			
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
			where.setToolTipText(where.getText());
		}
	}

	/**
	 * Creates popup menu for navigation.
	 * @param navigateFromAllRows 
	 */
	public JPopupMenu createPopupMenu(final Row row, final int rowIndex, final int x, final int y, boolean navigateFromAllRows) {
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

		JPopupMenu popup = new JPopupMenu();
		popup = createNavigationMenu(popup, row, rowIndex, assList, assMap, "Parents", "1", navigateFromAllRows);
		popup = createNavigationMenu(popup, row, rowIndex, assList, assMap, "Children", "2", navigateFromAllRows);
		popup = createNavigationMenu(popup, row, rowIndex, assList, assMap, "Associated Rows", "3", navigateFromAllRows);
		popup = createNavigationMenu(popup, row, rowIndex, assList, assMap, "Detached Rows", "4", navigateFromAllRows);

		if (row != null) {
			if (!(table instanceof SqlStatementTable)) {
				popup.add(new JSeparator());
			}

			JMenuItem det = new JMenuItem("Details");
			popup.add(det);
			det.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					JDialog d = new JDialog(getOwner(), (table instanceof SqlStatementTable)? "" : dataModel.getDisplayName(table), true);
					d.getContentPane().add(new DetailsView(rows, rowsTable.getRowCount(), dataModel, table, rowIndex, rowsTable.getRowSorter(), true) {
						@Override
						protected void onRowChanged(int row) {
							setCurrentRowSelection(row);
						}
					});
					d.pack();
					d.setLocation(x, y);
					d.setSize(400, d.getHeight() + 20);
					d.setVisible(true);
					setCurrentRowSelection(-1);
				}
			});

			if (!(table instanceof SqlStatementTable)) {
				popup.add(new JSeparator());

				JMenuItem qb = new JMenuItem("Query Builder");
				popup.add(qb);
				qb.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						List<String> whereClauses = new ArrayList<String>();
						whereClauses.add(SqlUtil.replaceAliases(row.rowId, "A", "A"));
						getQueryBuilderDialog().buildQuery(table, true, false, new ArrayList<Association>(), whereClauses, dataModel);
					}
				});			

				JMenuItem sr = new JMenuItem("Select Row");
				sr.setEnabled(rows.size() > 1);
				popup.add(sr);
				sr.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						String cond = SqlUtil.replaceAliases(row.rowId, "A", "A");
						String currentCond = andCondition.getText().trim();
						if (currentCond.length() > 0) {
							cond = "(" + cond + ") and (" + currentCond + ")";
						}
						andCondition.setText(cond);
						reloadRows();
					}
				});

				JMenu sql = new JMenu("SQL/DML");
				final String rowName = dataModel.getDisplayName(table) + "(" + SqlUtil.replaceAliases(row.rowId, null, null) + ")";
				JMenuItem insert = new JMenuItem("Insert");
				sql.add(insert);
				insert.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						openSQLDialog("Insert Row " + rowName, x, y, SQLDMLBuilder.buildInsert(table, row, true, session));
					}
				});
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
				popup.add(sql);
			}
		}
		return popup;
	}

	/**
	 * Creates popup menu for SQL.
	 */
	public JPopupMenu createSqlPopupMenu(final Row parentrow, final int rowIndex, final int x, final int y) {
		JPopupMenu popup = new JPopupMenu();
		JMenuItem qb = new JMenuItem("Open Query Builder");
		popup.add(qb);
		qb.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				openQueryBuilder();
			}
		});
		JMenuItem extractionModel = new JMenuItem("Create Extraction Model");
		popup.add(extractionModel);
		extractionModel.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				openExtractionModelEditor();
			}
		});
		popup.add(new JSeparator());
		JMenuItem det = new JMenuItem("Details");
		popup.add(det);
		det.setEnabled(rows.size() > 0);
		det.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				openDetails(x, y);
			}
		});
		popup.add(new JSeparator());
		JMenuItem insertNewRow = new JMenuItem("Insert New Row");
		popup.add(insertNewRow);
		final String tableName = dataModel.getDisplayName(table);
		insertNewRow.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				openSQLDialog("Insert New Row Into " + tableName, x, y, SQLDMLBuilder.buildInsert(table, createNewRow(parentrow, table), true, session));
			}
		});
		JMenuItem insert = new JMenuItem("Inserts");
		popup.add(insert);
		insert.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				openSQLDialog("Insert Into " + tableName, x, y, new Object() { public String toString() { return SQLDMLBuilder.buildInsert(table, rows, session); }});
			}
		});
		JMenuItem update = new JMenuItem("Updates");
		popup.add(update);
		update.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				openSQLDialog("Update " + tableName, x, y,  new Object() { public String toString() { return SQLDMLBuilder.buildUpdate(table, rows, session); }});
			}
		});
		JMenuItem delete = new JMenuItem("Deletes");
		popup.add(delete);
		delete.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				openSQLDialog("Delete from " + tableName, x, y,  new Object() { public String toString() { return SQLDMLBuilder.buildDelete(table, rows, session); }});
			}
		});
		insert.setEnabled(rows.size() > 0);
		update.setEnabled(rows.size() > 0);
		delete.setEnabled(rows.size() > 0);
		
		return popup;
	}

	void openExtractionModelEditor() {
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
						"Create Extraction Model for Subject \"" + dataModel.getDisplayName(stable) + "\".", (parents.isEmpty()? "" : ("\n\n" + parents.size() + " disregarded parent tables.")));
				sbEDialog.regardButton.setVisible(!parents.isEmpty());
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
			
			subjectCondition = root.needsAnchor? root.anchorWhereClause : root.whereClause;
			if (subjectCondition == null) {
				subjectCondition = "";
			}
			subjectCondition = SqlUtil.replaceAliases(subjectCondition, "T", "T");
			
			for (int i = 1; ; ++i) {
				file = "extractionmodel" + File.separator + "by-example";
				newFile = CommandLineParser.getInstance().newFile(file);
				newFile.mkdirs();
				file += File.separator + "SbE-" + (dataModel.getDisplayName(stable).replaceAll("[\"'\\[\\]]", "")) + "-" + ts + (i > 1? "-" + Integer.toString(i) : "") + ".csv";
				newFile = CommandLineParser.getInstance().newFile(file);
				if (!newFile.exists()) {
					break;
				}
			}
			dataModel.save(file, stable, subjectCondition, ScriptFormat.SQL, restrictionDefinitions);

			if (DataBrowserContext.isStandAlone()) {
				parent.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        		JOptionPane.showMessageDialog(parent, "Jailer Extraction Model created:\n'" + file+ "'\n\nJailer Database Subsetter Tool can be found at http://jailer.sourceforge.net", "Jailer Extraction Model", JOptionPane.INFORMATION_MESSAGE);
			} else {
				ExtractionModelFrame extractionModelFrame = ExtractionModelFrame.createFrame(file);
				extractionModelFrame.setDbConnectionDialogClone(getDbConnectionDialog());
				extractionModelFrame.markDirty();
				extractionModelFrame.expandAll();
				newFile.delete();
			}
		} catch (Throwable e) {
			parent.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
			UIUtil.showException(this, "Error", e);
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
					public String toString() {
						return item;
					}
				}));
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
			
			if (lastDist != null && lastDist != l.distanceFromRoot) {
				useDistance = true;
			}
			lastDist = l.distanceFromRoot;
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
			if (sqlString.length() > 2L*1024L*1024L) {
				if (1 == JOptionPane.showOptionDialog(this, "SQL Script is too long (" + (sqlString.length() / 1024) + " KB) to be edited.", "SQL Script too long", JOptionPane.YES_NO_OPTION, JOptionPane.INFORMATION_MESSAGE, null, new Object[] { "Ok", "Save Script" }, "Save Script")) {
					String fn = UIUtil.choseFile(null, ".", "Save SQL Script", ".sql", this, false, false, false);
			        if (fn != null) {
			            try {
			                PrintWriter out = new PrintWriter(new FileWriter(fn));
			                out.print(sqlString);
			                out.close();
			            } catch (Throwable e) {
			                UIUtil.showException(this, "Error saving script", e);
			            }
			        }
				}
				return;
			}
			d = new JDialog(getOwner(), "SQL/DML - " + titel, true);
			d.getContentPane().add(new SQLDMLPanel(sqlString, session, new Runnable() {
				@Override
				public void run() {
					reloadRows();
				}
			}));
			d.pack();
			d.setLocation(x - 50, y - 100);
			d.setSize(700, Math.max(d.getHeight() + 20, 400));
			UIUtil.fit(d);
		} catch (Throwable e) {
			UIUtil.showException(this, "Error", e);
			return;
		} finally {
			setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		}
		d.setVisible(true);
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
//		if (currentClosure.size() == 1) {
//			currentClosure.clear();
//		}
		currentClosureRowIDs.clear();
		for (Pair<BrowserContentPane, Row> r: currentClosure) {
			currentClosureRowIDs.add(new Pair<BrowserContentPane, String>(r.a, r.b.rowId));
		}
		
		Set<RowBrowser> toAdjust = new HashSet<Desktop.RowBrowser>(getChildBrowsers());
		if (getParentBrowser() != null) {
			toAdjust.add(getParentBrowser());
		}
		
		for (RowBrowser rb: toAdjust) {
			List<Row> rowsOfRB = new ArrayList<Row>();
			for (Pair<BrowserContentPane, Row> r: currentClosure) {
				if (r.a == rb.browserContentPane) {
					rowsOfRB.add(r.b);
				}
			}
			if (!rowsOfRB.isEmpty()) {
				Rectangle firstRowPos = null;
				Rectangle lastRowPos = null;
				Rectangle visibleRect = rb.browserContentPane.rowsTable.getVisibleRect();
				for (Row r: rowsOfRB) {
					int index = rb.browserContentPane.rows.indexOf(r);
					if (index < 0) {
						System.err.println("? row not found: " + r.rowId);
						continue;
					}
					index = rb.browserContentPane.rowsTable.getRowSorter().convertRowIndexToView(index);
					Rectangle pos = rb.browserContentPane.rowsTable.getCellRect(index, 0, false);
					if (pos.y >= visibleRect.y && pos.y + pos.height < visibleRect.y + visibleRect.height) {
						// already a visible row
						firstRowPos = null;
						break;
					}
					if (firstRowPos == null || firstRowPos.y > pos.y) {
						firstRowPos = pos;
					}
					if (lastRowPos == null || lastRowPos.y < pos.y) {
						lastRowPos = pos;
					}
				}
				if (lastRowPos != null) {
					rb.browserContentPane.rowsTable.scrollRectToVisible(new Rectangle(visibleRect.x, lastRowPos.y, 1, lastRowPos.height));
				}
				if (firstRowPos != null) {
					rb.browserContentPane.rowsTable.scrollRectToVisible(new Rectangle(visibleRect.x, firstRowPos.y, 1, firstRowPos.height));
				}
			}
		}
		
		onRedraw();
	}

	private JPopupMenu createNavigationMenu(JPopupMenu popup, final Row row, final int rowIndex, List<String> assList, Map<String, Association> assMap,
			String title, String prefix, final boolean navigateFromAllRows) {
		JMenu nav = new JMenu(title);
		if (prefix.equals("1")) {
			nav.setForeground(new java.awt.Color(170, 0, 0));
		}
		if (prefix.equals("2")) {
			nav.setForeground(new java.awt.Color(0, 112, 0));
		}
		if (prefix.equals("3")) {
			nav.setForeground(new java.awt.Color(0, 100, 255));
		}
		if (prefix.equals("4")) {
			nav.setForeground(new java.awt.Color(153, 153, 153));
		}
		JMenu current = nav;
		int l = 0;
		for (String name : assList) {
			if (!name.startsWith(prefix)) {
				continue;
			}

			final Association association = assMap.get(name);

			if (++l > 30) {
				l = 1;
				JMenu p = new JMenu("more...");
				if (current != null) {
					current.add(p);
				} else {
					popup.add(p);
				}
				current = p;
			}

			final JMenuItem item = new JMenuItem("  " + (name.substring(1)));

			for (RowBrowser child: getChildBrowsers()) {
				if (association == child.association) {
					item.setFont(new Font(item.getFont().getName(), item.getFont().getStyle() | Font.ITALIC, item.getFont().getSize()));
					break;
				}
			}
			
			item.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					highlightedRows.add(rowIndex);
					if (navigateFromAllRows) {
						navigateTo(association, -1, null);
					} else {
						navigateTo(association, rowIndex, row);
					}
				}
			});
			if (current != null) {
				current.add(item);
			} else {
				popup.add(item);
			}
		}
		if (l > 0) {
			popup.add(nav);
		}
		return popup;
	}

	/**
	 * Reloads rows.
	 */
	public void reloadRows() {
		if (!suppressReload) {
			cancelLoadJob();
			updateMode("loading");
			int limit = 100;
			if (limitBox.getSelectedItem() instanceof Integer) {
				limit = (Integer) limitBox.getSelectedItem();
			}
			LoadJob reloadJob = new LoadJob(limit, (table instanceof SqlStatementTable)? sqlBrowserContentPane.sqlEditorPane.getText() : andCondition.getText(), selectDistinctCheckBox.isSelected());
			synchronized (this) {
				currentLoadJob = reloadJob;
			}
			runnableQueue.add(reloadJob);
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
	private void reloadRows(String andCond, final List<Row> rows, Object context, int limit, boolean selectDistinct) throws SQLException {

		if (table instanceof SqlStatementTable) {
			try {
				session.setSilent(true);
				Map<String, List<Row>> rowsMap = new HashMap<String, List<Row>>();
				reloadRows(andCond, null, rowsMap, context, limit, false, null, false);
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
		}
		Map<String, Row> rowSet = new HashMap<String, Row>();
		if (parentRows != null) {
			beforeReload();
		}
		noNonDistinctRows = 0;
		noDistinctRows = 0;
		
		if (association != null && association.source.primaryKey.getColumns().isEmpty()) {
			loadRowBlocks(andCond, rows, context, limit, selectDistinct, pRows, rowSet, 1);
		} else {
			try {
				loadRowBlocks(andCond, rows, context, limit, selectDistinct, pRows, rowSet, 10000);
				return;
			} catch (SQLException e) {
				Session._log.warn("failed, try another blocking-size");
			}
			try {
				loadRowBlocks(andCond, rows, context, limit, selectDistinct, pRows, rowSet, 1000);
				return;
			} catch (SQLException e) {
				Session._log.warn("failed, try another blocking-size");
			}
			try {
				loadRowBlocks(andCond, rows, context, limit, selectDistinct, pRows, rowSet, 100);
				return;
			} catch (SQLException e) {
				Session._log.warn("failed, try another blocking-size");
			}
		}
		
		loadRowBlocks(andCond, rows, context, limit, selectDistinct, pRows, rowSet, 1);
	}

	private void loadRowBlocks(String andCond, final List<Row> rows, Object context, int limit, boolean selectDistinct, List<Row> pRows,
			Map<String, Row> rowSet, int NUM_PARENTS) throws SQLException {
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
		
		if (!pRows.isEmpty()) for (List<Row> pRowBlockI : parentBlocks) {
			List<Row> pRowBlock = pRowBlockI;
			Map<String, List<Row>> newBlockRows = new HashMap<String, List<Row>>();
			boolean loaded = false;
			
			if (pRowBlock.size() == 1 && pRowBlock.get(0) == null) {
				pRowBlock = null;
			}
			
			if (Configuration.forDbms(session).getSqlLimitSuffix() != null) {
				try {
					session.setSilent(true);
					reloadRows(andCond, pRowBlock, newBlockRows, context, limit, false, Configuration.forDbms(session).getSqlLimitSuffix(), selectDistinct);
					loaded = true;
				} catch (SQLException e) {
					Session._log.warn("failed, try another limit-strategy");
				} finally {
					session.setSilent(false);
				}
			}
			if (!loaded) {
				try {
					session.setSilent(true);
					reloadRows(andCond, pRowBlock, newBlockRows, context, limit, true, null, selectDistinct);
					loaded = true;
				} catch (SQLException e) {
					Session._log.warn("failed, try another limit-strategy");
				} finally {
					session.setSilent(false);
				}
				if (!loaded) {
					reloadRows(andCond, pRowBlock, newBlockRows, context, limit, false, null, selectDistinct);
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
				if (parentRows != null) {
					if (!newRows.isEmpty()) {
						newRows.get(newRows.size() - 1).setBlockEnd(true);
					}
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

	/**
	 * Reload rows from {@link #table}.
	 * 
	 * @param rows
	 *            to put the rows into
	 * @param context
	 *            cancellation context
	 * @param selectDistinct 
	 * @param rowCache 
	 * @param allPRows 
	 */
	private void reloadRows(String andCond, final List<Row> parentRows, final Map<String, List<Row>> rows, Object context, int limit, boolean useOLAPLimitation,
			String sqlLimitSuffix, boolean selectDistinct) throws SQLException {
		try {
			reloadRows0(andCond, parentRows, rows, context, parentRows == null? limit : Math.max(5000, limit), useOLAPLimitation, sqlLimitSuffix, selectDistinct);
		} catch (SQLException e) {
			if (selectDistinct) {
				// try without "distinct"
				reloadRows0(andCond, parentRows, rows, context, limit, useOLAPLimitation, sqlLimitSuffix, false);
			} else {
				throw e;
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
	 * @param selectDistinct 
	 */
	private void reloadRows0(String andCond, final List<Row> parentRows, final Map<String, List<Row>> rows, Object context, int limit, boolean useOLAPLimitation,
			String sqlLimitSuffix, boolean selectDistinct) throws SQLException {
		String sql = "Select "; // + (selectDistinct? "distinct " : "");
		final Set<String> pkColumnNames = new HashSet<String>();
		final Set<String> parentPkColumnNames = new HashSet<String>();
		final boolean selectParentPK = association != null && parentRows != null && parentRows.size() > 1;
		
		if (table instanceof SqlStatementTable) {
			sql = andCond;
			table.setColumns(new ArrayList<Column>());
		} else {
			String olapPrefix = "Select ";
			String olapSuffix = ") S Where S." + ROWNUMBERALIAS + " <= " + limit + " Order by S." + ROWNUMBERALIAS + "";
			if (sqlLimitSuffix != null && sqlLimitSuffix.toLowerCase().startsWith("top ")) {
				sql += (sqlLimitSuffix.replace("%s", Integer.toString(limit))) + " ";
			}
			boolean f = true;
			if (selectParentPK) {
				int i = 0;
				for (Column column : association.source.primaryKey.getColumns()) {
					String name = column.name;
					sql += (!f ? ", " : "") + "B." + name + " AS B" + i;
					olapPrefix += (!f ? ", " : "") + "S.B" + i;
					++i;
					f = false;
				}
			}
			int i = 0;
			for (Column column : table.getColumns()) {
				String name = column.name;
				sql += (!f ? ", " : "") + "A." + name + " AS A" + i;
				olapPrefix += (!f ? ", " : "") + "S.A" + i;
				++i;
				f = false;
			}
			f = true;
			String orderBy = "";
			if (selectParentPK) {
				for (Column pk : association.source.primaryKey.getColumns()) {
					parentPkColumnNames.add(pk.name);
					orderBy += (f ? "" : ", ") + "B." + pk.name;
					f = false;
				}
			}
			for (Column pk : table.primaryKey.getColumns()) {
				pkColumnNames.add(pk.name);
				orderBy += (f ? "" : ", ") + "A." + pk.name;
				f = false;
			}
			if (useOLAPLimitation) {
				sql += ", row_number() over(";
				if (useOLAPLimitation) {
					sql += "order by " + orderBy;
				}
				sql += ") as " + ROWNUMBERALIAS + "";
			}
			sql += " From ";
			if (association != null) {
				sql += association.source.getName() + " B join ";
			}
			sql += table.getName() + " A";
			if (association != null) {
				if (association.reversed) {
					sql += " on " + association.getUnrestrictedJoinCondition();
				} else {
					sql += " on " + SqlUtil.reversRestrictionCondition(association.getUnrestrictedJoinCondition());
				}
			}
	
			boolean whereExists = false;
			if (parentRows != null && !parentRows.isEmpty()) {
				if (parentRows.size() == 1) {
					sql += " Where (" + parentRows.get(0).rowId + ")";
				} else {
					StringBuilder sb = new StringBuilder();
					for (Row parentRow: parentRows) {
						if (sb.length() == 0) {
							sb.append(" Where ((");
						} else {
							sb.append(" or (");
						}
						sb.append(parentRow.rowId).append(")");
					}
					sb.append(")");
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
				sql = olapPrefix + sql + olapSuffix;
			}
			if (sqlLimitSuffix != null && !sqlLimitSuffix.toLowerCase().startsWith("top ")) {
				sql += " " + (sqlLimitSuffix.replace("%s", Integer.toString(limit)));
			}
		}
		if (sql.length() > 0) {
			session.executeQuery(sql, new Session.ResultSetReader() {
	
				Map<Integer, Integer> typeCache = new HashMap<Integer, Integer>();
	
				@Override
				public void readCurrentRow(ResultSet resultSet) throws SQLException {
					if ((table instanceof SqlStatementTable) && rows.isEmpty()) {
						for (int ci = 1; ci <= resultSet.getMetaData().getColumnCount(); ++ci) {
							table.getColumns().add(new Column(resultSet.getMetaData().getColumnName(ci), resultSet.getMetaData().getColumnTypeName(ci), -1, -1));
						}
					}
					
					int i = 1, vi = 0;
					String parentRowId = "";
					if (selectParentPK) {
						Object v[] = new Object[association.source.primaryKey.getColumns().size()];
						for (Column column : association.source.primaryKey.getColumns()) {
							parentRowId = readRowFromResultSet(parentPkColumnNames, resultSet, i, vi, parentRowId, v, column);
							++i;
							++vi;
						}
					} else {
						if (parentRows != null && parentRows.size() == 1) {
							parentRowId = parentRows.get(0).rowId;
						}
					}
					
					String rowId = "";
					
					Object v[] = new Object[table.getColumns().size()];
					vi = 0;
					for (Column column : table.getColumns()) {
						rowId = readRowFromResultSet(pkColumnNames, resultSet, i, vi, rowId, v, column);
						++i;
						++vi;
					}
					List<Row> cRows = rows.get(parentRowId);
					if (cRows == null) {
						cRows = new ArrayList<Row>();
						rows.put(parentRowId, cRows);
					}
					cRows.add(new Row(rowId, v));
				}

				private String readRowFromResultSet(final Set<String> pkColumnNames, ResultSet resultSet, int i, int vi, String rowId, Object[] v, Column column)
						throws SQLException {
					Object value = "";
					int type = SqlUtil.getColumnType(resultSet, i, typeCache);
					if (type == Types.BLOB || type == Types.CLOB || type == Types.SQLXML) {
						Object object = resultSet.getObject(i);
						if (object == null || resultSet.wasNull()) {
							value = null;
						}
						if (object instanceof Blob) {
							value = new LobValue() {
								public String toString() {
									return "<Blob>";
								}
							};
						}
						if (object instanceof Clob) {
							value = new LobValue() {
								public String toString() {
									return "<Clob>";
								}
							};
						}
						if (object instanceof SQLXML) {
							value = new LobValue() {
								public String toString() {
									return "<XML>";
								}
							};
						}
					} else {
						Object o = SqlUtil.getObject(resultSet, i, typeCache);
						boolean isPK = false;
						if (pkColumnNames.isEmpty()) {
							isPK = type != Types.BLOB && type != Types.CLOB && type != Types.DATALINK && type != Types.JAVA_OBJECT && type != Types.NCLOB
									&& type != Types.NULL && type != Types.OTHER && type != Types.REF && type != Types.SQLXML && type != Types.STRUCT;
						}
						if (pkColumnNames.contains(column.name) || isPK) {
							String cVal = SqlUtil.toSql(o, session);
							rowId += (rowId.length() == 0 ? "" : " and ") + "B." + column.name + ("null".equalsIgnoreCase(cVal)? " is null" : ("=" + cVal));
						}
						if (o == null || resultSet.wasNull()) {
							value = null;
						}
						if (o != null) {
							value = o;
						}
					}
					v[vi] = value;
					return rowId;
				}
	
				@Override
				public void close() {
				}
			}, null, context, limit);
		}
	}

	/**
	 * Updates the model of the {@link #rowsTable}.
	 * 
	 * @param limit
	 *            row limit
	 * @param limitExceeded 
	 */
	private void updateTableModel(int limit, boolean limitExceeded) {
		pkColumns.clear();
		String[] columnNames = new String[table.getColumns().size()];
		final Set<String> pkColumnNames = new HashSet<String>();
		if (table.primaryKey != null) {
			for (Column pk : table.primaryKey.getColumns()) {
				pkColumnNames.add(pk.name);
			}
		}
		for (int i = 0; i < columnNames.length; ++i) {
			columnNames[i] = table.getColumns().get(i).name;
			if (pkColumnNames.contains(columnNames[i])) {
				pkColumns.add(i);
			}
		}

		DefaultTableModel dtm;
		singleRowDetailsView = null;
		int rn = 0;
		if (rows.size() != 1) {
			dtm = new DefaultTableModel(columnNames, 0) {
				@Override
				public boolean isCellEditable(int row, int column) {
					return false;
				}
			};
			for (Row row : rows) {
				Object[] rowData = new Object[table.getColumns().size()];
				for (int i = 0; i < table.getColumns().size(); ++i) {
					rowData[i] = row.values[i];
				}
				dtm.addRow(rowData);
				if (++rn >= limit) {
					break;
				}
			}
			rowsTable.setModel(dtm);
			rowsTable.setRowHeight(initialRowHeight);

			TableRowSorter<TableModel> sorter = new TableRowSorter<TableModel>(dtm);
			for (int i = 0; i < columnNames.length; ++i) {
				sorter.setComparator(i, new Comparator<Object>() {
					@SuppressWarnings("unchecked")
					@Override
					public int compare(Object o1, Object o2) {
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
			rowsTable.setRowSorter(sorter);
		} else {
			singleRowDetailsView = new DetailsView(Collections.singletonList(rows.get(0)), 1, dataModel, BrowserContentPane.this.table, 0, null, false) {
				@Override
				protected void onRowChanged(int row) {
				}
			};
			dtm = new DefaultTableModel(new String[] { "Single Row Details" }, 0) {
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
		rowsTable.setIntercellSpacing(new Dimension(0, 0));
		int size = rows.size();
		if (size > limit) {
			size = limit;
		}
		rowsCount.setText((limitExceeded ? " more than " : " ") + size + " row" + (size != 1 ? "s" : ""));
		rowsCount.setForeground(limitExceeded ? Color.RED : new JLabel().getForeground());
		int nndr = noNonDistinctRows;
		if (noDistinctRows + noNonDistinctRows >= limit) {
			--nndr;
		}
		selectDistinctCheckBox.setVisible(nndr > 0);
		selectDistinctCheckBox.setText("select distinct (-" + nndr + " row" + (nndr == 1? "" : "s") + ")");
		
		if (sqlBrowserContentPane != null) {
			sqlBrowserContentPane.detailsButton.setEnabled(!rows.isEmpty());
		}
	}

	public void adjustRowTableColumnsWidth() {
		DefaultTableModel dtm = (DefaultTableModel) rowsTable.getModel();
		for (int i = 0; i < rowsTable.getColumnCount(); i++) {
			TableColumn column = rowsTable.getColumnModel().getColumn(i);
			int width = ((int) (Desktop.BROWSERTABLE_DEFAULT_WIDTH * getLayoutFactor()) - 18) / rowsTable.getColumnCount();

			Component comp = rowsTable.getDefaultRenderer(String.class).getTableCellRendererComponent(rowsTable, column.getHeaderValue(), false, false, 0, i);
			width = Math.max(width, comp.getPreferredSize().width);

			for (int line = 0; line < rowsTable.getRowCount(); ++line) {
				comp = rowsTable.getCellRenderer(line, i).getTableCellRendererComponent(rowsTable, dtm.getValueAt(line, i), false, false, line, i);
				width = Math.max(width, comp.getPreferredSize().width + (singleRowDetailsView == null ? 16 : 0));
				if (singleRowDetailsView == null) {
					width = Math.min(width, 400);
				}
			}

			column.setPreferredWidth(width);
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

        wherePanel = new javax.swing.JPanel();
        where = new javax.swing.JLabel();
        jPanel7 = new javax.swing.JPanel();
        andCondition = new javax.swing.JTextField();
        loadButton = new javax.swing.JButton();
        onPanel = new javax.swing.JPanel();
        on = new javax.swing.JLabel();
        joinPanel = new javax.swing.JPanel();
        join = new javax.swing.JLabel();
        jLabel6 = new javax.swing.JLabel();
        jPanel10 = new javax.swing.JPanel();
        from = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        cardPanel = new javax.swing.JPanel();
        jPanel2 = new javax.swing.JPanel();
        jLabel2 = new javax.swing.JLabel();
        cancelLoadButton = new javax.swing.JButton();
        jPanel1 = new javax.swing.JPanel();
        rowsTableScrollPane = new javax.swing.JScrollPane();
        rowsTable = new javax.swing.JTable();
        jPanel6 = new javax.swing.JPanel();
        rowsCount = new javax.swing.JLabel();
        selectDistinctCheckBox = new javax.swing.JCheckBox();
        jPanel5 = new javax.swing.JPanel();
        jLabel10 = new javax.swing.JLabel();
        jPanel4 = new javax.swing.JPanel();
        jLabel8 = new javax.swing.JLabel();
        jLabel1 = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();
        jLabel9 = new javax.swing.JLabel();
        andLabel = new javax.swing.JLabel();
        openEditorLabel = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        fetchLabel = new javax.swing.JLabel();
        jPanel3 = new javax.swing.JPanel();
        limitBox = new javax.swing.JComboBox();
        jLabel7 = new javax.swing.JLabel();
        relatedRowsPanel = new javax.swing.JPanel();
        relatedRowsLabel = new javax.swing.JLabel();
        jPanel9 = new javax.swing.JPanel();
        sqlPanel = new javax.swing.JPanel();
        sqlLabel1 = new javax.swing.JLabel();
        dropA = new javax.swing.JLabel();
        dropB = new javax.swing.JLabel();

        setLayout(new java.awt.GridBagLayout());

        wherePanel.setMinimumSize(new java.awt.Dimension(66, 17));
        wherePanel.setLayout(new java.awt.BorderLayout());

        where.setFont(new java.awt.Font("DejaVu Sans", 0, 14));
        where.setText("jLabel3");
        wherePanel.add(where, java.awt.BorderLayout.CENTER);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 7;
        gridBagConstraints.gridwidth = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHWEST;
        gridBagConstraints.weightx = 1.0;
        add(wherePanel, gridBagConstraints);

        jPanel7.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel7.add(andCondition, gridBagConstraints);

        loadButton.setText(" Reload ");
        loadButton.addActionListener(new java.awt.event.ActionListener() {
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

        on.setFont(new java.awt.Font("DejaVu Sans", 0, 14));
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

        join.setFont(new java.awt.Font("DejaVu Sans", 0, 14));
        join.setText("jLabel3");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        joinPanel.add(join, gridBagConstraints);

        jLabel6.setFont(new java.awt.Font("DejaVu Sans", 1, 13));
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
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHWEST;
        gridBagConstraints.weightx = 1.0;
        add(joinPanel, gridBagConstraints);

        jPanel10.setMinimumSize(new java.awt.Dimension(66, 17));
        jPanel10.setLayout(new java.awt.GridBagLayout());

        from.setFont(new java.awt.Font("DejaVu Sans", 0, 14));
        from.setText("jLabel3");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        jPanel10.add(from, gridBagConstraints);

        jLabel5.setFont(new java.awt.Font("DejaVu Sans", 1, 13));
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

        cardPanel.setLayout(new java.awt.CardLayout());

        jPanel2.setLayout(new java.awt.GridBagLayout());

        jLabel2.setFont(new java.awt.Font("DejaVu Sans", 1, 14));
        jLabel2.setForeground(new java.awt.Color(141, 16, 16));
        jLabel2.setText("loading...");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(8, 8, 8, 0);
        jPanel2.add(jLabel2, gridBagConstraints);

        cancelLoadButton.setText("Cancel");
        cancelLoadButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelLoadButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel2.add(cancelLoadButton, gridBagConstraints);

        cardPanel.add(jPanel2, "loading");

        jPanel1.setLayout(new java.awt.GridBagLayout());

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
        jPanel1.add(rowsTableScrollPane, gridBagConstraints);

        jPanel6.setLayout(new java.awt.GridBagLayout());

        rowsCount.setText("jLabel3");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        jPanel6.add(rowsCount, gridBagConstraints);

        selectDistinctCheckBox.setText("select distinct (-100 rows)");
        selectDistinctCheckBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                selectDistinctCheckBoxActionPerformed(evt);
            }
        });
        jPanel6.add(selectDistinctCheckBox, new java.awt.GridBagConstraints());

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTH;
        jPanel1.add(jPanel6, gridBagConstraints);

        cardPanel.add(jPanel1, "table");

        jPanel5.setLayout(new java.awt.GridBagLayout());

        jLabel10.setFont(new java.awt.Font("DejaVu Sans", 1, 14));
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

        jLabel8.setFont(new java.awt.Font("DejaVu Sans", 1, 14));
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

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 12;
        gridBagConstraints.gridwidth = 7;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(cardPanel, gridBagConstraints);

        jLabel1.setFont(new java.awt.Font("DejaVu Sans", 1, 13));
        jLabel1.setText(" Join ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        add(jLabel1, gridBagConstraints);

        jLabel4.setFont(new java.awt.Font("DejaVu Sans", 1, 13));
        jLabel4.setText(" On ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        add(jLabel4, gridBagConstraints);

        jLabel9.setFont(new java.awt.Font("DejaVu Sans", 1, 13));
        jLabel9.setText(" Where ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 7;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        add(jLabel9, gridBagConstraints);

        andLabel.setFont(new java.awt.Font("DejaVu Sans", 1, 13));
        andLabel.setText(" And  ");
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

        jLabel3.setFont(new java.awt.Font("DejaVu Sans", 1, 13));
        jLabel3.setText(" From ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        add(jLabel3, gridBagConstraints);

        fetchLabel.setFont(new java.awt.Font("DejaVu Sans", 1, 13));
        fetchLabel.setText(" Limit  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 9;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.VERTICAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        add(fetchLabel, gridBagConstraints);

        jPanel3.setLayout(new javax.swing.BoxLayout(jPanel3, javax.swing.BoxLayout.LINE_AXIS));

        limitBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        limitBox.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                limitBoxItemStateChanged(evt);
            }
        });
        jPanel3.add(limitBox);

        jLabel7.setFont(new java.awt.Font("DejaVu Sans", 1, 13));
        jLabel7.setText(" rows");
        jPanel3.add(jLabel7);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 9;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        add(jPanel3, gridBagConstraints);

        relatedRowsPanel.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
        relatedRowsPanel.setLayout(new javax.swing.BoxLayout(relatedRowsPanel, javax.swing.BoxLayout.LINE_AXIS));

        relatedRowsLabel.setText(" Related Rows ");
        relatedRowsPanel.add(relatedRowsLabel);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 5;
        gridBagConstraints.gridy = 9;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 2);
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

        sqlLabel1.setText(" SQL/Query ");
        sqlPanel.add(sqlLabel1);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 8;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.gridheight = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHEAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 2);
        add(sqlPanel, gridBagConstraints);

        dropA.setFont(new java.awt.Font("DejaVu Sans", 1, 13));
        dropA.setText("drop");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 4;
        add(dropA, gridBagConstraints);

        dropB.setFont(new java.awt.Font("DejaVu Sans", 1, 13));
        dropB.setText("drop");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 5;
        add(dropB, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

        private void cancelLoadButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelLoadButtonActionPerformed
            cancelLoadJob();
            updateMode("cancelled");
        }//GEN-LAST:event_cancelLoadButtonActionPerformed

        private void selectDistinctCheckBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_selectDistinctCheckBoxActionPerformed
        	reloadRows();
        }//GEN-LAST:event_selectDistinctCheckBoxActionPerformed

	private void loadButtonActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_loadButtonActionPerformed
		reloadRows();
	}// GEN-LAST:event_loadButtonActionPerformed

	private void limitBoxItemStateChanged(java.awt.event.ItemEvent evt) {// GEN-FIRST:event_limitBoxItemStateChanged
		reloadRows();
	}// GEN-LAST:event_limitBoxItemStateChanged

	private void openQueryBuilder() {
		
		QueryBuilderDialog.Relationship root = createQBRelations(true);
		root.selectColumns = true;
		getQueryBuilderDialog().buildQuery(table, root, dataModel);
	}

    // Variables declaration - do not modify//GEN-BEGIN:variables
    public javax.swing.JTextField andCondition;
    private javax.swing.JLabel andLabel;
    private javax.swing.JButton cancelLoadButton;
    private javax.swing.JPanel cardPanel;
    private javax.swing.JLabel dropA;
    private javax.swing.JLabel dropB;
    private javax.swing.JLabel fetchLabel;
    private javax.swing.JLabel from;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel10;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel10;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JPanel jPanel7;
    private javax.swing.JPanel jPanel9;
    private javax.swing.JLabel join;
    private javax.swing.JPanel joinPanel;
    javax.swing.JComboBox limitBox;
    private javax.swing.JButton loadButton;
    private javax.swing.JLabel on;
    private javax.swing.JPanel onPanel;
    private javax.swing.JLabel openEditorLabel;
    private javax.swing.JLabel relatedRowsLabel;
    private javax.swing.JPanel relatedRowsPanel;
    private javax.swing.JLabel rowsCount;
    public javax.swing.JTable rowsTable;
    javax.swing.JScrollPane rowsTableScrollPane;
    javax.swing.JCheckBox selectDistinctCheckBox;
    private javax.swing.JLabel sqlLabel1;
    private javax.swing.JPanel sqlPanel;
    private javax.swing.JLabel where;
    private javax.swing.JPanel wherePanel;
    // End of variables declaration//GEN-END:variables

	private ConditionEditor andConditionEditor;
	private Icon conditionEditorIcon;
	private Icon conditionEditorSelectedIcon;
	{
		String dir = "/net/sf/jailer/resource";

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
	 */
	public void cancelLoadJob() {
		LoadJob cLoadJob;
		synchronized (this) {
			cLoadJob = currentLoadJob;
		}
		if (cLoadJob != null) {
			cLoadJob.cancel();
		}
	}

	private void updateMode(String mode) {
		((CardLayout) cardPanel.getLayout()).show(cardPanel, mode);
//		relatedRowsPanel.setVisible("table".equals(mode) && rows.size() >= 1);
	}

    /**
     * Opens a drop-down box which allows the user to select columns for restriction definitions.
     */
	private void openColumnDropDownBox(JLabel label, String alias, Table table) {
		JPopupMenu popup = new JPopupMenu();
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
				public void actionPerformed(ActionEvent e) {
					if (andCondition.isEnabled()) {
						if (andCondition.isEditable()) {
							andCondition.replaceSelection(c);
							andCondition.grabFocus();
						}
					}
				}
			});
			popup.add(m);
		}
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
		Row row = new Row(null, new Object[table.getColumns().size()]);
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

	protected abstract void beforeReload();

	protected abstract QueryBuilderDialog.Relationship createQBRelations(boolean withParents);
	protected abstract List<QueryBuilderDialog.Relationship> createQBChildrenRelations(RowBrowser tabu, boolean all);

	protected abstract void addRowToRowLink(Row pRow, Row exRow);

	protected abstract JFrame getOwner();

	protected abstract void findClosure(Row row);
	protected abstract void findClosure(Row row, Set<Pair<BrowserContentPane, Row>> closure, boolean forward);

	protected abstract QueryBuilderDialog getQueryBuilderDialog();
	protected abstract QueryBuilderPathSelector getQueryBuilderPathSelector();

	protected abstract void openSchemaMappingDialog();
	protected abstract void openSchemaAnalyzer();
	protected abstract DbConnectionDialog getDbConnectionDialog();
	protected abstract double getLayoutFactor();
	protected abstract List<RowBrowser> getChildBrowsers();
	protected abstract RowBrowser getParentBrowser();
	
    private void openDetails(final int x, final int y) {
		JDialog d = new JDialog(getOwner(), (table instanceof SqlStatementTable)? "" : dataModel.getDisplayName(table), true);
		d.getContentPane().add(new DetailsView(rows, rowsTable.getRowCount(), dataModel, table, 0, rowsTable.getRowSorter(), true) {
			@Override
			protected void onRowChanged(int row) {
				setCurrentRowSelection(row);
			}
		});
		d.pack();
		d.setLocation(x, y);
		d.setSize(400, d.getHeight() + 20);
		d.setVisible(true);
		setCurrentRowSelection(-1);
	}

	private void updateWhereField() {
		if (association != null) {
			where.setText(parentRow == null ? (parentRows != null && parentRows.size() > 0? parentRows.get(0).rowId + (parentRows.size() > 1? " or ..." : "") : "") : parentRow.rowId);
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

	private Icon dropDownIcon;
    {
		String dir = "/net/sf/jailer/resource";
		
		// load images
		try {
			dropDownIcon = new ImageIcon(getClass().getResource(dir + "/dropdown.png"));
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

}
