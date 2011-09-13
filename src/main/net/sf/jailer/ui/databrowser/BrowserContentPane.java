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
import java.sql.Blob;
import java.sql.Clob;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.SQLXML;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
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
import javax.swing.RowSorter;
import javax.swing.SwingUtilities;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableModel;
import javax.swing.table.TableRowSorter;

import net.sf.jailer.Configuration;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.PrimaryKey;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;
import net.sf.jailer.ui.ConditionEditor;
import net.sf.jailer.ui.QueryBuilderDialog;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.util.CancellationException;
import net.sf.jailer.util.CancellationHandler;
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
						onContentChange(new ArrayList<Row>());
						BrowserContentPane.this.rows.clear();
						BrowserContentPane.this.rows.addAll(rows);
						updateTableModel(l, limitExceeded);
						onContentChange(rows);
						updateMode("table");
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
	final Row parentRow;

	/**
	 * The data model.
	 */
	DataModel dataModel;

	/**
	 * {@link Association} with parent row, or <code>null</code>.
	 */
	private final Association association;

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
	private final Session session;

	private int currentRowSelection = -1;

	private List<Row> parentRows;
	protected final Set<Row> currentClosure;
	protected Set<String> currentClosureRowIDs;
	private DetailsView singleRowDetailsView;
	private int initialRowHeight;
	public SQLBrowserContentPane sqlBrowserContentPane;
	
	/**
	 * For concurrent reload of rows.
	 */
	private static final LinkedBlockingQueue<LoadJob> runnableQueue = new LinkedBlockingQueue<LoadJob>();

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
	
	private static class SqlStatementTable extends Table {
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
	 */
	public BrowserContentPane(final DataModel dataModel, final Table table, String condition, Session session, Row parentRow, List<Row> parentRows,
			final Association association, Frame parentFrame, Set<Row> currentClosure, Set<String> currentClosureRowIDs) {
		this.table = table;
		this.session = session;
		this.dataModel = dataModel;
		this.parentRow = parentRow;
		this.parentRows = parentRows;
		this.association = association;
		this.currentClosure = currentClosure;
		this.currentClosureRowIDs = currentClosureRowIDs;

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
		jScrollPane1.setViewportView(rowsTable);

		andCondition.setText(ConditionEditor.toSingleLine(condition));
		from.setText(table == null? "" : this.dataModel.getDisplayName(table));
		if (association == null) {
			joinPanel.setVisible(false);
			onPanel.setVisible(false);
			wherePanel.setVisible(false);
			jLabel1.setVisible(false);
			jLabel4.setVisible(false);
			jLabel9.setVisible(false);
			jLabel6.setVisible(false);
			dropB.setVisible(false);
			andLabel.setText(" Where ");
			java.awt.GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 8;
	        gridBagConstraints.gridy = 4;
	        gridBagConstraints.gridheight = 1;
	        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHEAST;
	        add(sqlPanel, gridBagConstraints);
		} else {
			join.setText(dataModel.getDisplayName(association.source));
			on.setText(!association.reversed ? SqlUtil.reversRestrictionCondition(association.getUnrestrictedJoinCondition()) : association
					.getUnrestrictedJoinCondition());
			where.setText(parentRow == null ? parentRows.get(0).rowId + " or ..." : parentRow.rowId);
			join.setToolTipText(join.getText());
			on.setToolTipText(on.getText());
			where.setToolTipText(where.getText());
		}
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
					singleRowDetailsView.setBorderColor(isSelected? render.getBackground() : BrowserContentPane.this.currentClosureRowIDs.contains(theRow.rowId)? BG3 : Color.WHITE);
					return singleRowDetailsView;
				}

				final RowSorter<?> rowSorter = rowsTable.getRowSorter();
				if (render instanceof JLabel) {
					if (!isSelected) {
						if (row < rows.size() && BrowserContentPane.this.currentClosureRowIDs.contains(rows.get(rowSorter.convertRowIndexToModel(row)).rowId)) {
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
						setCurrentRowSelection(ri);
						Rectangle r = rowsTable.getCellRect(ri, 0, false);
						int x = Math.max((int) e.getPoint().x, (int) r.getMinX());
						int y = (int) r.getMaxY() - 2;
						if (singleRowDetailsView != null) {
							y = e.getY();
						}
						Point p = SwingUtilities.convertPoint(rowsTable, x, y, null);
						JPopupMenu popup = createPopupMenu(row, i, p.x + getOwner().getX(), p.y + getOwner().getY());
						popup.show(rowsTable, x, y);
						popup.addPropertyChangeListener("visible", new PropertyChangeListener() {

							@Override
							public void propertyChange(PropertyChangeEvent evt) {
								if (Boolean.FALSE.equals(evt.getNewValue())) {
									setCurrentRowSelection(-1);
								}
							}
						});
						lastMenu = popup;
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
				if (rows.size() == 1) {
					popup = createPopupMenu(rows.get(0), 0, 0, 0);
				} else {
					popup = createPopupMenu(null, -1, 0, 0);
				}
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
		limitBox.setModel(new DefaultComboBoxModel(new Integer[] { 100, 200, 500, 1000, 2000, 5000 }));
		limitBox.setSelectedIndex(association == null? 0 : 1);
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
		
		reloadRows();
	}

	/**
	 * Creates popup menu for navigation.
	 */
	public JPopupMenu createPopupMenu(final Row row, final int rowIndex, final int x, final int y) {
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
		popup = createNavigationMenu(popup, row, rowIndex, assList, assMap, "Parents", "1");
		popup = createNavigationMenu(popup, row, rowIndex, assList, assMap, "Children", "2");
		popup = createNavigationMenu(popup, row, rowIndex, assList, assMap, "Associated Rows", "3");
		popup = createNavigationMenu(popup, row, rowIndex, assList, assMap, "Detached Rows", "4");

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
				
				JMenu sql = new JMenu("SQL/DML");
				final String rowName = dataModel.getDisplayName(table) + "(" + SqlUtil.replaceAliases(row.rowId, null, null) + ")";
				JMenuItem insert = new JMenuItem("Insert");
				sql.add(insert);
				insert.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						openSQLDialog("Insert Row " + rowName, x, y, SQLDMLBuilder.buildInsert(table, row, session));
					}
				});
				JMenuItem update = new JMenuItem("Update");
				sql.add(update);
				update.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						openSQLDialog("Update Row " + rowName, x, y, SQLDMLBuilder.buildUpdate(table, row, session));
					}
				});
				JMenuItem delete = new JMenuItem("Delete");
				sql.add(delete);
				delete.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						openSQLDialog("Delete Row " + rowName, x, y, SQLDMLBuilder.buildDelete(table, row, session));
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
		JMenuItem qb = new JMenuItem("Query Builder");
		popup.add(qb);
		qb.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				openQueryBuilder();
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
				openSQLDialog("Insert New Row Into " + tableName, x, y, SQLDMLBuilder.buildInsert(table, createNewRow(parentrow, table), session));
			}
		});
		JMenuItem insert = new JMenuItem("Inserts");
		popup.add(insert);
		insert.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				openSQLDialog("Insert Into " + tableName, x, y, SQLDMLBuilder.buildInsert(table, rows, session));
			}
		});
		JMenuItem update = new JMenuItem("Updates");
		popup.add(update);
		update.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				openSQLDialog("Update " + tableName, x, y, SQLDMLBuilder.buildUpdate(table, rows, session));
			}
		});
		JMenuItem delete = new JMenuItem("Deletes");
		popup.add(delete);
		delete.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				openSQLDialog("Delete from " + tableName, x, y, SQLDMLBuilder.buildDelete(table, rows, session));
			}
		});
		insert.setEnabled(rows.size() > 0);
		update.setEnabled(rows.size() > 0);
		delete.setEnabled(rows.size() > 0);
		
		return popup;
	}

	private void openSQLDialog(String titel, int x, int y, String sql) {
		setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
		JDialog d;
		try {
			d = new JDialog(getOwner(), "SQL/DML - " + titel, true);
			d.getContentPane().add(new SQLDMLPanel(sql, session, new Runnable() {
				@Override
				public void run() {
					reloadRows();
				}
			}));
			d.pack();
			d.setLocation(x - 50, y - 100);
			d.setSize(700, Math.max(d.getHeight() + 20, 400));
			UIUtil.fit(d);
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
		}
		if (currentClosure.size() == 1) {
			currentClosure.clear();
		}
		currentClosureRowIDs.clear();
		for (Row r: currentClosure) {
			currentClosureRowIDs.add(r.rowId);
		}
		onRedraw();
	}

	private JPopupMenu createNavigationMenu(JPopupMenu popup, final Row row, final int rowIndex, List<String> assList, Map<String, Association> assMap,
			String title, String prefix) {
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

			item.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					highlightedRows.add(rowIndex);
					navigateTo(association, rowIndex, row);
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
				reloadRows(andCond, null, rows, context, limit, false, null);
			} finally {
				session.setSilent(false);
			}
			return;
		}
		
		List<Row> pRows = parentRows;
		if (pRows == null) {
			pRows = Collections.singletonList(parentRow);
		}
		Map<String, Row> rowSet = new HashMap<String, Row>();
		if (parentRows != null) {
			beforeReload();
		}
		noNonDistinctRows = 0;
		noDistinctRows = 0;
		for (Row pRow : pRows) {
			List<Row> newRows = new ArrayList<Row>();
			boolean loaded = false;
			if (Configuration.forDbms(session).getSqlLimitSuffix() != null) {
				try {
					session.setSilent(true);
					reloadRows(andCond, pRow, newRows, context, limit, false, Configuration.forDbms(session).getSqlLimitSuffix());
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
					reloadRows(andCond, pRow, newRows, context, limit, true, null);
					loaded = true;
				} catch (SQLException e) {
					Session._log.warn("failed, try another limit-strategy");
				} finally {
					session.setSilent(false);
				}
				if (!loaded) {
					reloadRows(andCond, pRow, newRows, context, limit, false, null);
				}
			}
			if (parentRows != null) {
				if (!newRows.isEmpty()) {
					newRows.get(newRows.size() - 1).setBlockEnd(true);
				}
				for (Row row : newRows) {
					Row exRow = rowSet.get(row.rowId);
					if (exRow != null) {
						++noNonDistinctRows;
					} else {
						++noDistinctRows;
					}
					if (exRow != null && selectDistinct) {
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
	}

	/**
	 * Reload rows from {@link #table}.
	 * 
	 * @param rows
	 *            to put the rows into
	 * @param context
	 *            cancellation context
	 */
	private void reloadRows(String andCond, final Row parentRow, final List<Row> rows, Object context, int limit, boolean useOLAPLimitation,
			String sqlLimitSuffix) throws SQLException {
		String sql = "Select ";
		final Set<String> pkColumnNames = new HashSet<String>();
		
		if (table instanceof SqlStatementTable) {
			sql = andCond;
			table.setColumns(new ArrayList<Column>());
		} else {
			String olapPrefix = "Select ";
			String olapSuffix = ") S Where S.jlr_rnum__ <= " + limit + " Order by S.jlr_rnum__";
			if (sqlLimitSuffix != null && sqlLimitSuffix.toLowerCase().startsWith("top ")) {
				sql += (sqlLimitSuffix.replace("%s", Integer.toString(limit))) + " ";
			}
			boolean f = true;
			for (Column column : table.getColumns()) {
				String name = column.name;
				sql += (!f ? ", " : "") + "A." + name;
				olapPrefix += (!f ? ", " : "") + "S." + name;
				f = false;
			}
			f = true;
			String orderBy = "";
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
				sql += ") as jlr_rnum__";
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
	
			if (parentRow != null) {
				sql += " Where (" + parentRow.rowId + ")";
			}
			if (andCond.trim().length() > 0) {
				sql += (parentRow != null ? " and" : " Where") + " (" + ConditionEditor.toMultiLine(andCond) + ")";
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
					
					int i = 1;
					String rowId = "";
					Object v[] = new Object[table.getColumns().size()];
					for (Column column : table.getColumns()) {
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
						v[i - 1] = value;
						++i;
					}
					rows.add(new Row(rowId, v));
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

		for (int i = 0; i < rowsTable.getColumnCount(); i++) {
			TableColumn column = rowsTable.getColumnModel().getColumn(i);
			int width = (Desktop.BROWSERTABLE_DEFAULT_WIDTH - 18) / rowsTable.getColumnCount();

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
        jScrollPane1 = new javax.swing.JScrollPane();
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

        jPanel1.setLayout(new java.awt.BorderLayout());

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
        jScrollPane1.setViewportView(rowsTable);

        jPanel1.add(jScrollPane1, java.awt.BorderLayout.CENTER);

        jPanel6.setLayout(new java.awt.GridBagLayout());

        rowsCount.setText("jLabel3");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        jPanel6.add(rowsCount, gridBagConstraints);

        selectDistinctCheckBox.setSelected(true);
        selectDistinctCheckBox.setText("select distinct (-100 rows)");
        selectDistinctCheckBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                selectDistinctCheckBoxActionPerformed(evt);
            }
        });
        jPanel6.add(selectDistinctCheckBox, new java.awt.GridBagConstraints());

        jPanel1.add(jPanel6, java.awt.BorderLayout.SOUTH);

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
        jLabel3.setText(" from ");
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
		List<String> whereClauses = new ArrayList<String>();
		List<Association> associationsOnPath = new ArrayList<Association>();
		createAssociationList(associationsOnPath, whereClauses, -1);
		
		int backCount = getQueryBuilderPathSelector().selectBackCount(associationsOnPath);
		if (backCount >= 0) {
			whereClauses = new ArrayList<String>();
			associationsOnPath = new ArrayList<Association>();
			createAssociationList(associationsOnPath, whereClauses, backCount);
			getQueryBuilderDialog().buildQuery(table, true, false, associationsOnPath, whereClauses, dataModel);
		}
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
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JLabel join;
    private javax.swing.JPanel joinPanel;
    private javax.swing.JComboBox limitBox;
    private javax.swing.JButton loadButton;
    private javax.swing.JLabel on;
    private javax.swing.JPanel onPanel;
    private javax.swing.JLabel openEditorLabel;
    private javax.swing.JLabel relatedRowsLabel;
    private javax.swing.JPanel relatedRowsPanel;
    private javax.swing.JLabel rowsCount;
    public javax.swing.JTable rowsTable;
    private javax.swing.JCheckBox selectDistinctCheckBox;
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
		relatedRowsPanel.setVisible("table".equals(mode) && rows.size() >= 1);
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

	protected abstract void onContentChange(List<Row> rows);

	protected abstract void onRedraw();

	protected abstract void beforeReload();

	protected abstract void createAssociationList(List<Association> associations, List<String> whereClauses, int backCount);

	protected abstract void addRowToRowLink(Row pRow, Row exRow);

	protected abstract JFrame getOwner();

	protected abstract void findClosure(Row row);
	protected abstract void findClosure(Row row, Set<Row> closure, boolean forward);

	protected abstract QueryBuilderDialog getQueryBuilderDialog();
	protected abstract QueryBuilderPathSelector getQueryBuilderPathSelector();

	protected abstract void openSchemaMappingDialog();
	protected abstract void openSchemaAnalyzer();
	
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
