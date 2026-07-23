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
package net.sf.jailer.ui.databrowser.sqlconsole;

import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionAdapter;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.Vector;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.InputMap;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JTable;
import javax.swing.KeyStroke;
import javax.swing.ListSelectionModel;
import javax.swing.RowSorter;
import javax.swing.SwingUtilities;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;

import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.Colors;
import net.sf.jailer.ui.ExtendetCopyPanel;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.UIUtil.PLAF;
import net.sf.jailer.ui.databrowser.BrowserContentPane;
import net.sf.jailer.ui.databrowser.BrowserContentPane.TableModelItem;
import net.sf.jailer.ui.databrowser.LobValue;
import net.sf.jailer.ui.databrowser.Row;
import net.sf.jailer.ui.databrowser.lob.BlobLengthPlaceholder;
import net.sf.jailer.ui.databrowser.lob.LobContentSupport;
import net.sf.jailer.ui.databrowser.lob.LobContentType;
import net.sf.jailer.util.LogUtil;
import net.sf.jailer.util.Pair;

/**
 * "Column view" of a query result table.
 *  
 * @author Ralf Wisser
 */
public class ColumnsTable extends JTable {
	private static final long serialVersionUID = 1L;

	private final int MAX_ROWS = 498;
	private static final KeyStroke KS_COPY_TO_CLIPBOARD = KeyStroke.getKeyStroke(KeyEvent.VK_C, InputEvent.CTRL_DOWN_MASK);
	private static final KeyStroke KS_ECOPY_TO_CLIPBOARD = KeyStroke.getKeyStroke(KeyEvent.VK_C, InputEvent.CTRL_DOWN_MASK|KeyEvent.SHIFT_DOWN_MASK);
	private static final KeyStroke KS_EDIT = KeyStroke.getKeyStroke(KeyEvent.VK_E, InputEvent.CTRL_DOWN_MASK);
	final BrowserContentPane rb;
	private Map<Integer, String> tableName = new HashMap<Integer, String>();
	private boolean useTableName = true;
	private final boolean inDesktop;
	private boolean inClosure;
	private int currentRow = -1;
	private Action copyAction;
	private Action ecopyAction;
	private Action editModeAction;

	private static final String LOB_TOOLTIP = "Show the content of this value";
	private static final String LOB_NOT_RELOADABLE_TOOLTIP = "Full content cannot be reloaded (no primary key available for this result).";
	private static final ImageIcon LOB_VIEW_ICON = loadLobViewIcon();

	private static ImageIcon loadLobViewIcon() {
		try {
			return UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/showinnewwindow.png"));
		} catch (Throwable t) {
			return null;
		}
	}

	// keyed by table row (= the data column shown in that row; the single-row view has exactly one data row)
	private final Map<Integer, LobContentType> lobTypeCache = new HashMap<Integer, LobContentType>();
	private final Map<Integer, ImageIcon> lobThumbCache = new HashMap<Integer, ImageIcon>();
	private final Set<Integer> lobRequested = new HashSet<Integer>();

	/**
	 * Whether LOB cells in this table show a content-type label, thumbnail and
	 * "View…" affordance (as in {@link net.sf.jailer.ui.databrowser.DetailsView}).
	 * Disabled by default; enabled only for the single-row details view.
	 */
	protected boolean isLobViewerEnabled() {
		return false;
	}


	/**
	 * Constructor.
	 *
	 * @param rb the browser content pane providing the row data
	 * @param inDesktop <code>true</code> if displayed inside the desktop
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public ColumnsTable(final BrowserContentPane rb, boolean inDesktop) {
		this.rb = rb;
		this.inDesktop = inDesktop;
		final JTable rowsTable = rb.rowsTable;
		
		final RowSorter<? extends TableModel> sorter = rowsTable.getRowSorter();
		Vector cNames = new Vector();
		cNames.add("Column");
		for (int i = 0; i < sorter.getViewRowCount(); ++i) {
			if (i > MAX_ROWS) {
				cNames.add("Row " + (i + 1) + " (" + (sorter.getViewRowCount() - i - 1) + " more)");
				break;
			}
			cNames.add("Row " + (i + 1));
		}
		final TableColumnModel cm = rowsTable.getColumnModel();
		TableModel cDm = new DefaultTableModel(cNames, rowsTable.getModel().getColumnCount()) {
			private static final long serialVersionUID = 1L;

			@Override
			public boolean isCellEditable(int rowIndex, int columnIndex) {
				if (columnIndex == 0) {
					return false;
				}
				int row = rowsTable.getRowSorter().convertRowIndexToModel(columnIndex - 1);
				int column = cm.getColumn(rowIndex).getModelIndex();
				return rowsTable.getModel().isCellEditable(row, column);
			}
			
			@Override
			public Object getValueAt(int rowIndex, int columnIndex) {
				int column = cm.getColumn(rowIndex).getModelIndex();
				if (columnIndex == 0) {
					String[] ntPair = rowsTable.getModel().getColumnName(column).replaceAll("<br>", "\t").replaceAll("<[^>]*>", "").split("\t");
					if (ntPair.length == 1) {
						return UIUtil.fromHTMLFragment(ntPair[0]);
					}
					if (ntPair.length == 2) {
						return UIUtil.fromHTMLFragment(ntPair[0]);
					}
					if (ntPair.length == 3) {
						tableName.put(rowIndex, ntPair[0]);
						return UIUtil.fromHTMLFragment(ntPair[1]);
					}
					return ntPair;
				}
				int row = rowsTable.getRowSorter().convertRowIndexToModel(columnIndex - 1);
				return rowsTable.getModel().getValueAt(row, column);
			}
			
			@Override
			public void setValueAt(Object value, int rowIndex, int columnIndex) {
				if (columnIndex == 0) {
					return;
				}
				int row = rowsTable.getRowSorter().convertRowIndexToModel(columnIndex - 1);
				int column = cm.getColumn(rowIndex).getModelIndex();
				rowsTable.getModel().setValueAt(value, row, column);
			}
		};

		if (!inDesktop) {
			setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
			setRowSelectionAllowed(true);
			setColumnSelectionAllowed(true);
			setCellSelectionEnabled(true);
		} else {
//			setFocusable(false);
			setRowSelectionAllowed(false);
			setColumnSelectionAllowed(false);
			rowsTable.setEnabled(false);
			rowsTable.setAutoscrolls(false);			
		}
		// getTableHeader().setReorderingAllowed(false);
		setShowGrid(false);
		setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
		setModel(cDm);
		
		for (int row = 0; row < cDm.getRowCount(); ++row) {
			cDm.getValueAt(row, 0);
		}
		int cnt[] = new int[1];
		new HashSet<String>(tableName.values()).forEach(s -> {
			if (!s.matches("^(<[^>]+>)*((&nbsp;)*)(<[^>]+>)*$")) {
				++cnt[0];
			}
		});
		if (cnt[0] <= 1) {
			useTableName = false;
		}
		
		for (int i = 0; i < getColumnCount(); i++) {
			TableCellEditor defaultEditor = rowsTable.getDefaultEditor(getColumnClass(i));
			if (defaultEditor != null) {
				defaultEditor.cancelCellEditing();
			}
			setDefaultEditor(getColumnClass(i), defaultEditor);
		}
		TableCellEditor defaultEditor = rowsTable.getDefaultEditor(Object.class);
		setDefaultEditor(Object.class, defaultEditor);
		InputMap im = getInputMap();
		Object key = "copyClipboard";
		im.put(KS_COPY_TO_CLIPBOARD, key);
		ActionMap am = getActionMap();
		Action a = new AbstractAction() {
			private static final long serialVersionUID = 1L;

			@Override
			public void actionPerformed(ActionEvent e) {
				JTable tab = ColumnsTable.this;
				if (inDesktop) {
					int s = currentRow;
					if (s >= 0) {
						Object o = tab.getValueAt(s, 1);
						String text = o != null? o instanceof TableModelItem? "" + ((TableModelItem) o).value :  o.toString() : "";
						UIUtil.setClipboardContent(new StringSelection(text.trim()));
					}
				} else {
					List<Integer> types = new ArrayList<Integer>();
					if (rb != null && rb.browserContentCellEditor != null) {
						for (int type: rb.browserContentCellEditor.getColumnTypes()) {
							types.add(type);
						}
					}
					ExtendetCopyPanel.openDialog(tab, false, "?no-name?", types, true, true);
				}
			}
		};
		am.put(key, a);
		copyAction = a;
		key = "ecopyClipboard";
		im.put(KS_ECOPY_TO_CLIPBOARD, key);
		am = getActionMap();
		a = new AbstractAction() {
			@Override
			public void actionPerformed(ActionEvent e) {
				JTable tab = ColumnsTable.this;
				List<Integer> types = new ArrayList<Integer>();
				if (rb != null && rb.browserContentCellEditor != null) {
					for (int type: rb.browserContentCellEditor.getColumnTypes()) {
						types.add(type);
					}
				}
				ExtendetCopyPanel.openDialog(tab, false, "?no-name?", types, true, false);
			}
		};
		am.put(key, a);
		ecopyAction = a;
		key = "editMode";
		im.put(KS_EDIT, key);
		am = getActionMap();
		a = new AbstractAction() {
			@Override
			public void actionPerformed(ActionEvent e) {
				rb.toggleEditMode(() -> resetCellEditor());
			}
		};
		am.put(key, a);
		editModeAction = a;
		
		addMouseListener(new MouseListener() {
			@Override
			public void mouseReleased(MouseEvent e) {
			}

			@Override
			public void mousePressed(MouseEvent e) {
				if (!inDesktop) {
					int ri = rowAtPoint(e.getPoint());
					if (ri >= 0) {
						Rectangle r = getCellRect(ri, 0, false);
						int x = Math.max(e.getPoint().x, (int) r.getMinX());
						int y = (int) r.getMaxY() - 2;
						if (e.getButton() != MouseEvent.BUTTON1) {
							JPopupMenu popup;
							popup = createPopupMenu(e);
							if (popup != null) {
								popup.addPropertyChangeListener("visible", new PropertyChangeListener() {
						    		@Override
									public void propertyChange(PropertyChangeEvent evt) {
						    			if (Boolean.FALSE.equals(evt.getNewValue())) {
						    				rb.setCurrentRowSelection(-1);
						    				repaint();
						    			}
						    		}
								});
						    	popup.show(ColumnsTable.this, x, y);
							}
						} else if (e.getClickCount() > 1) {
							int i = -1;
							ri = columnAtPoint(e.getPoint()) - 1;
							if (ri >= 0 && !rb.rows.isEmpty() && rb.rowsTable.getRowSorter().getViewRowCount() > 0) {
								i = rb.rowsTable.getRowSorter().convertRowIndexToModel(ri);
								Point p = new Point(e.getX(), e.getY());
								SwingUtilities.convertPointToScreen(p, ColumnsTable.this);
								rb.openDetails(i, (int) p.getX(), (int) p.getY());
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

		setDefaultRenderer(Object.class, new TableCellRenderer() {
			final Color BGCOLUMNS = Colors.Color_255_255_230;
			final Color BGSELECTED  = Colors.Color_255_230_220;
			@Override
			public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected,
					boolean hasFocus, int row, int column) {
				int dmColumn = column;
				if (dmColumn  > 0) {
					--dmColumn;
				}
				Component render = rowsTable.getCellRenderer(dmColumn, row).getTableCellRendererComponent(ColumnsTable.this, value, false, hasFocus, dmColumn, row);
				int currentColumn = rb.getCurrentRowSelection();
				if (render instanceof JLabel) {
					if (column == 0 && (inDesktop || "columnNames".equals(table.getName()))) {
						((JLabel) render).setBackground(BGCOLUMNS);
						if (ColumnsTable.this.inClosure || inTempClosure()) {
							((JLabel) render).setBackground(row % 2 == 0? UIUtil.TABLE_BACKGROUND_COLOR_1_INCLOSURE : UIUtil.TABLE_BACKGROUND_COLOR_2_INCLOSURE);
						}
					} else if (ColumnsTable.this.inDesktop && !"final".equals(render.getName())) {
						((JLabel) render).setBackground(row % 2 == 0? UIUtil.TABLE_BACKGROUND_COLOR_1 : UIUtil.TABLE_BACKGROUND_COLOR_2);
					} else if (column - 1 == currentColumn && !"final".equals(render.getName())) {
						((JLabel) render).setBackground(BGSELECTED);
					}
					if (column == 0) {
						String text = ((JLabel) render).getText();
						String tabName = useTableName && (table == ColumnsTable.this || "columnNames".equals(table.getName()))? tableName.get(row) : null;
						int mCol = cm.getColumn(row).getModelIndex();
						Table tab = ColumnsTable.this.rb.table;
						if (tabName != null) {
							JLabel lTab = new JLabel("<html>&nbsp;" + tabName + "&nbsp;</html>");
							lTab.setForeground(Colors.Color_0_0_180);
							lTab.setBackground(render.getBackground());
							lTab.setOpaque(render.isOpaque());
							JPanel panel = new JPanel(new GridBagLayout());
							panel.setToolTipText(text);
							GridBagConstraints gridBagConstraints;
							gridBagConstraints = new java.awt.GridBagConstraints();
					        gridBagConstraints.gridx = 1;
					        gridBagConstraints.gridy = 1;
					        gridBagConstraints.fill = GridBagConstraints.BOTH;
					        gridBagConstraints.weightx = 1;
					        gridBagConstraints.weighty = 1;
					        gridBagConstraints.anchor = GridBagConstraints.WEST;
					        panel.add(lTab, gridBagConstraints);
					        gridBagConstraints = new java.awt.GridBagConstraints();
					        gridBagConstraints.gridx = 3;
					        gridBagConstraints.gridy = 1;
					        gridBagConstraints.anchor = GridBagConstraints.EAST;
					        gridBagConstraints.fill = GridBagConstraints.BOTH;
					        gridBagConstraints.weighty = 1;
					        panel.add(render, gridBagConstraints);
					        ((JLabel) render).setText(text + "  ");
					        if (ColumnsTable.this.rb.getAlternativeColumnLabelsFull() != null && ColumnsTable.this.rb.getAlternativeColumnLabelsFull().length > mCol) {
					        	String ttt = ColumnsTable.this.rb.getAlternativeColumnLabelsFull()[mCol];
					        	Table rsTab = ColumnsTable.this.rb.getResultSetTypeForColumn(mCol);
					        	
					        	if (rsTab != null && rsTab.getName() != null && rsTab.getColumns().size() > mCol) {
									Column col = rsTab.getColumns().get(mCol);
									
									String nullContraint = null;
					        		if (col != null && col.name != null) {
										nullContraint = col.isNullable? "nullable" : "not null";
										if (tabName != null && tabName.toUpperCase().equals(tabName)) {
					        				nullContraint = nullContraint.toUpperCase();
					        			}
						        		if (ttt.startsWith("<html")) {
						        			ttt = ttt.replace("</html>",  "<font color=" + Colors.HTMLColor_808080 + "> <i>" + nullContraint + "</i></font></html>");
						        			((JLabel) render).setToolTipText(ttt);
						        		} else {
						        			ttt = "<html>" + UIUtil.toHTMLFragment(ttt, 0) + "<hr>" + "<font color=" + Colors.HTMLColor_808080 + "> <i>" + nullContraint + "</i></font></html>";
						        			((JLabel) render).setToolTipText(ttt);
						        		}
					        		}
					        	}
				        		panel.setToolTipText(ttt);
				        	}
					        return panel;
						}
						if (tab != null && tab.getColumns().size() > mCol) {
							Column col = tab.getColumns().get(mCol);
							
							String nullContraint = null;
							String ttt = col.name;
			        		if (col != null && col.name != null) {
								nullContraint = col.isNullable? "nullable" : "not null";
								if (tab.getName() != null && tab.getName().toUpperCase().equals(tab.getName())) {
			        				nullContraint = nullContraint.toUpperCase();
			        			}
								if (inDesktop) {
									nullContraint = "</i>" + col.toSQL(null).substring(col.name.length()).trim() + "<i> " + nullContraint;
								}
			        		}
				        	if (ColumnsTable.this.rb.getAlternativeColumnLabelsFull() != null && ColumnsTable.this.rb.getAlternativeColumnLabelsFull().length > mCol) {
				        		ttt = ColumnsTable.this.rb.getAlternativeColumnLabelsFull()[mCol];
				        	}
				        	if (nullContraint != null) {
				        		if (ttt.startsWith("<html")) {
				        			ttt = ttt.replace("</html>",  "<font color=" + Colors.HTMLColor_808080 + "> <i>" + nullContraint + "</i></font></html>");
				        			((JLabel) render).setToolTipText(ttt);
				        		} else {
				        			ttt = "<html>" + UIUtil.toHTMLFragment(ttt, 0) + "<hr>" + "<font color=" + Colors.HTMLColor_808080 + "> <i>" + nullContraint + "</i></font></html>";
				        			((JLabel) render).setToolTipText(ttt);
				        		}
				        	}
						}
						
						((JLabel) render).setText(" " + text);
					} else {
						if (inTempClosure()) {
							((JLabel) render).setBackground(blend(((JLabel) render).getBackground()));
						}
					}
					if (isSelected && column > 0) {
						if (UIUtil.TABLE_BACKGROUND_COLOR_1.equals(render.getBackground())) {
							render.setBackground(UIUtil.TABLE_BG1SELECTED);
						} else {
							render.setBackground(UIUtil.TABLE_BG2SELECTED);
						}
					}
					if ("found".equals(render.getName())) {
						Color background = render.getBackground();
						if (UIUtil.plaf == PLAF.FLATDARK) {
							render.setBackground(
									new Color(
											Math.max((int)(background.getRed()), 0),
											100,
											Math.max((int)(background.getBlue() * 0.5), 0),
											background.getAlpha()));
						} else {
							render.setBackground(
								new Color(
										Math.max((int)(background.getRed()), 0),
										Math.max((int)(background.getGreen() * 0.90), 0),
										Math.max((int)(background.getBlue() * 0.91), 0),
										background.getAlpha()));
						}
					}
				}
				if (currentRow == row) {
					render.setBackground(Colors.Color_122_210_255_200);
				}
				if (column > 0 && render instanceof JLabel) {
					Object rawValue = lobRawValueAt(row, column);
					if (rawValue instanceof LobValue) {
						String text = ((JLabel) render).getText();
						if (text != null) {
							((JLabel) render).setText(rawValue.toString().trim());
						}
					}
					if (isLobViewerEnabled() && LobContentSupport.isLobCell(rawValue)) {
						render = buildLobCell((JLabel) render, row, column, rawValue);
					}
				}
				return render;
			}
			private Color blend(Color color1) {
				int alpha = UIUtil.plaf == PLAF.FLATDARK? 20 : 14;
				Color color2 = Colors.Color_255_0_0;
				float factor = alpha / 255f;
				int red = (int) (color1.getRed() * (1 - factor) + color2.getRed() * factor);
				int green = (int) (color1.getGreen() * (1 - factor) + color2.getGreen() * factor);
				int blue = (int) (color1.getBlue() * (1 - factor) + color2.getBlue() * factor);
				return new Color(red, green, blue);
			}
		});
		adjustTableColumnsWidth();
		addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent e) {
				currentRow = rowAtPoint(e.getPoint());
				if (fixed != null) {
					fixed.repaint();
				}
				repaint();
			}
		});
		if (isLobViewerEnabled()) {
			setToolTipText(""); // registers with the ToolTipManager; getToolTipText(MouseEvent) below supplies the per-cell text
		}
		addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent e) {
				// mousePressed, not mouseClicked: a synthesized mouseClicked can be
				// suppressed when the same press also triggers focus transfer /
				// internal-frame activation on an unfocused table.
				if (!isLobViewerEnabled() || e.getButton() != MouseEvent.BUTTON1) {
					return;
				}
				int tableRow = rowAtPoint(e.getPoint());
				int tableColumn = columnAtPoint(e.getPoint());
				if (tableRow < 0 || tableColumn <= 0) {
					return;
				}
				Object rawValue = lobRawValueAt(tableRow, tableColumn);
				if (!LobContentSupport.isLobCell(rawValue)) {
					return;
				}
				if (!isLobViewCellReloadable(rawValue, tableRow, tableColumn)) {
					return;
				}
				Row dataRow = lobRowAt(tableColumn);
				if (dataRow != null) {
					rb.openLobViewer(dataRow, lobDataColumnModelIndex(tableRow), ColumnsTable.this);
				}
			}
		});
		addMouseMotionListener(new MouseMotionAdapter() {
			@Override
			public void mouseMoved(MouseEvent e) {
				if (!isLobViewerEnabled()) {
					return;
				}
				int tableRow = rowAtPoint(e.getPoint());
				int tableColumn = columnAtPoint(e.getPoint());
				Object rawValue = tableRow >= 0 && tableColumn > 0 ? lobRawValueAt(tableRow, tableColumn) : null;
				boolean isLob = rawValue != null && LobContentSupport.isLobCell(rawValue)
						&& isLobViewCellReloadable(rawValue, tableRow, tableColumn);
				setCursor(isLob ? Cursor.getPredefinedCursor(Cursor.HAND_CURSOR) : Cursor.getDefaultCursor());
			}
		});
	}

	/**
	 * Whether a LOB cell's "View" affordance should act as clickable/hoverable.
	 * Only {@link BlobLengthPlaceholder} cells need a resolvable primary key to
	 * reload their content (see {@link BrowserContentPane#isLobReloadable}) -
	 * every other LOB kind already has its content materialized locally.
	 */
	private boolean isLobViewCellReloadable(Object rawValue, int tableRow, int tableColumn) {
		if (!(rawValue instanceof BlobLengthPlaceholder)) {
			return true;
		}
		Row dataRow = lobRowAt(tableColumn);
		return dataRow != null && rb.isLobReloadable(dataRow, lobDataColumnModelIndex(tableRow));
	}

	@Override
	public String getToolTipText(MouseEvent event) {
		if (isLobViewerEnabled()) {
			int tableRow = rowAtPoint(event.getPoint());
			int tableColumn = columnAtPoint(event.getPoint());
			if (tableRow >= 0 && tableColumn > 0) {
				Object rawValue = lobRawValueAt(tableRow, tableColumn);
				if (LobContentSupport.isLobCell(rawValue)) {
					return isLobViewCellReloadable(rawValue, tableRow, tableColumn) ? LOB_TOOLTIP : LOB_NOT_RELOADABLE_TOOLTIP;
				}
			}
		}
		return super.getToolTipText(event);
	}

	/**
	 * Creates popup menu. 
	 * @param e mouse event 
	 */
	private JPopupMenu createPopupMenu(MouseEvent e) {
		int i = -1;
		Point p = new Point();
		Row row = null;
		if (e != null) {
			int ri = columnAtPoint(e.getPoint()) - 1;
			if (ri >= 0 && !rb.rows.isEmpty() && rb.rowsTable.getRowSorter().getViewRowCount() > 0) {
				i = rb.rowsTable.getRowSorter().convertRowIndexToModel(ri);
				row = rb.rows.get(i);
				rb.setCurrentRowSelection(ri);
				repaint();
			} else {
				return null;
			}
			p = new Point(e.getX(), e.getY());
			SwingUtilities.convertPointToScreen(p, this);
		}
		Set<Integer> selectedRowsIndexes = new TreeSet<>();
		try {
			for (int si: getSelectedColumns()) {
				int ri = getColumnModel().getColumn(si).getModelIndex() - 1;
				if (ri >= 0 && !rb.rows.isEmpty() && rb.rowsTable.getRowSorter().getViewRowCount() > 0) {
					i = rb.rowsTable.getRowSorter().convertRowIndexToModel(ri);
					selectedRowsIndexes.add(i);
				}
			}
		} catch (Exception ex) {
			LogUtil.warn(ex);
		}
		return rb.createPopupMenu(this, row, i, selectedRowsIndexes, (int) p.getX(), (int) p.getY(), false, copyAction, ecopyAction, new Runnable() {
			@Override
			public void run() {
				resetCellEditor();
			}
		}, true, true, true);
	}

	private void adjustTableColumnsWidth() {
		DefaultTableModel dtm = (DefaultTableModel) getModel();
		DefaultTableCellRenderer defaultTableCellRenderer = new DefaultTableCellRenderer();
		int maxWidth = getColumnCount() > 2? 300 : 1200;
		for (int i = 0; i < getColumnCount(); i++) {
			TableColumn column = getColumnModel().getColumn(i);
			int width = 0;

			Component comp = defaultTableCellRenderer.getTableCellRendererComponent(this, column.getHeaderValue(), false, false, 0, i);
			int pw = comp.getPreferredSize().width;
			if (pw < 100) {
				pw = (pw * 130) / 100 + 10;
			}
			width = Math.max(width, pw);

			int line = 0;
			for (; line < getRowCount(); ++line) {
				comp = getCellRenderer(line, i).getTableCellRendererComponent(this, dtm.getValueAt(line, i), false, false, line, i);
				width = Math.max(width, comp.getPreferredSize().width + 16);
			}
			Object maxValue = null;
			int maxValueLength = 0;
			for (; line < getRowCount(); ++line) {
				Object value = dtm.getValueAt(line, i);
				if (value != null) {
					int valueLength = value.toString().length();
					if (maxValue == null || maxValueLength < valueLength) {
						maxValue = value;
						maxValueLength = valueLength;
					}
				}
			}
			if (maxValue != null) {
				comp = getCellRenderer(line, i).getTableCellRendererComponent(this, maxValue, false, false, line, i);
				int maxValueWidth = comp.getPreferredSize().width + 16;
				if (maxValueWidth > width) {
					width = maxValueWidth;
				}
			}
			int prefWidth = Math.min(maxWidth, width);
			if (i == 0) {
				prefWidth = Math.min(prefWidth, 260);
			}
			column.setPreferredWidth(prefWidth);
		}
	}

	/**
	 * Scrolls the view so that the currently selected row is visible.
	 */
	public void scrollToCurrentRow() {
		final int currentColumn = rb.getCurrentRowSelection();
		if (currentColumn >= 0 && currentColumn + 1 < getColumnCount()) {
			UIUtil.invokeLater(2, new Runnable() {
				@Override
				public void run() {
					Rectangle cellRect = getCellRect(0, currentColumn + 1, true);
					Rectangle cellRectLast = getCellRect(0, getColumnCount() - 1, true);
					Rectangle visRect = getVisibleRect();
					int b = 0; // Math.max(cellRect.width / 4, 16);
					if (cellRect.x < visRect.x || cellRect.x + cellRect.width > visRect.x + visRect.width) {
						scrollRectToVisible(new Rectangle(Math.max(cellRectLast.x + cellRectLast.width + b, 1), visRect.y + visRect.height / 2, 1, Math.max(cellRect.height + b, 1)));
						scrollRectToVisible(new Rectangle(Math.max(cellRect.x - b, 1), visRect.y + visRect.height / 2, 1, Math.max(cellRect.height + b, 1)));
					}
				}
			});
		}
	}

	/**
	 * Updates the closure highlight state of this table.
	 *
	 * @param inClosure <code>true</code> if this table is in closure
	 */
	public void updateInClosureState(boolean inClosure) {
		this.inClosure = inClosure;
		repaint();
	}
	
	protected boolean inTempClosure() {
		return false;
	}
	
	@Override
	public void changeSelection(int rowIndex, int columnIndex, boolean toggle, boolean extend) {
		if (!inDesktop) {
			super.changeSelection(rowIndex, columnIndex, toggle, extend);
		}
	}

	private JTable fixed;
	
	/**
	 * Initializes a fixed companion table that mirrors row selection.
	 *
	 * @param fixed the fixed table to synchronize with
	 */
	public void initFixedTable(JTable fixed) {
		this.fixed = fixed;
		fixed.addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent e) {
				currentRow = rowAtPoint(e.getPoint());
				if (fixed != null) {
					fixed.repaint();
				}
				repaint();
				ColumnsTable.this.setRowSelectionInterval(currentRow, currentRow);
				ColumnsTable.this.setColumnSelectionInterval(0, ColumnsTable.this.getColumnCount() - 1);
			}
		});
		InputMap im = fixed.getInputMap();
		Object key = "copyClipboard";
		im.put(KS_COPY_TO_CLIPBOARD, key);
		ActionMap am = fixed.getActionMap();
		am.put(key, copyAction);
		key = "ecopyClipboard";
		im.put(KS_ECOPY_TO_CLIPBOARD, key);
		am = fixed.getActionMap();
		am.put(key, ecopyAction);
		key = "editMode";
		im.put(KS_EDIT, key);
		am = fixed.getActionMap();
		am.put(key, editModeAction);
	}
	
	/**
	 * Clears the current row selection highlight.
	 */
	public void clear() {
		currentRow = -1;
		repaint();
	}

	@Override
	public void paint(Graphics graphics) {
		super.paint(graphics);
		if (!(graphics instanceof Graphics2D)) {
			return;
		}
		Rectangle visRect = getVisibleRect();

		Graphics2D g2d = (Graphics2D) graphics;
		g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
		g2d.setStroke(new BasicStroke(1));

		int left = -1;
		int last = -1;
		List<Pair<Integer, Integer>> intervall = new ArrayList<Pair<Integer, Integer>>();
		for (int si : getSelectedColumns()) {
			if (left < 0) {
				left = si;
			} else if (last < si - 1) {
				intervall.add(new Pair<Integer, Integer>(left, last));
				left = si;
			}
			last = si;
		}
		if (left >= 0) {
			intervall.add(new Pair<Integer, Integer>(left, last));
		}

		int top = -1;
		last = -1;
		List<Pair<Integer, Integer>> rowIntervall = new ArrayList<Pair<Integer, Integer>>();
		for (int si : getSelectedRows()) {
			if (top < 0) {
				top = si;
			} else if (last < si - 1) {
				rowIntervall.add(new Pair<Integer, Integer>(top, last));
				top = si;
			}
			last = si;
		}
		if (top >= 0) {
			rowIntervall.add(new Pair<Integer, Integer>(top, last));
		}
		int x[] = new int[2];
		int y[] = new int[2];
		for (Pair<Integer, Integer> iv : intervall) {
			for (Pair<Integer, Integer> rowIv : rowIntervall) {
				int[] selectedRows = getSelectedRows();
				if (selectedRows.length > 0) {
					x[0] = Integer.MAX_VALUE;
					y[0] = Integer.MAX_VALUE;
					x[1] = Integer.MIN_VALUE;
					y[1] = Integer.MIN_VALUE;
					Rectangle r = getCellRect(rowIv.a, iv.a, false);
					x[0] = Math.min((int) r.getMinX(), x[0]);
					y[0] = Math.min((int) r.getMinY(), y[0]);
					r = getCellRect(rowIv.b, iv.b, false);
					x[1] = Math.max((int) r.getMaxX(), x[1]);
					y[1] = Math.max((int) r.getMaxY(), y[1]);
					x[0] = (int) Math.max(visRect.getMinX(), x[0]) + 1;
					y[0] = (int) Math.max(visRect.getMinY(), y[0]);
					x[1] = (int) Math.min(visRect.getMaxX(), x[1]) - 2;
					y[1] = (int) Math.min(visRect.getMaxY() - 1, y[1]);
					if (x[0] < x[1] && y[0] < y[1]) {
						g2d.setColor(UIUtil.BG_FLATMOUSEOVER);
						BasicStroke stroke = new BasicStroke();
						g2d.setStroke(stroke);
						g2d.drawRoundRect(x[0], y[0], x[1] - x[0], y[1] - y[0] - 1, 8, 8);
						g2d.setColor(Colors.Color_0_0_200_100);
						g2d.setStroke(new BasicStroke(stroke.getLineWidth(), stroke.getEndCap(), stroke.getLineJoin(),
								stroke.getMiterLimit(), new float[] { 11f, 5f },
								(float) ((System.currentTimeMillis() / 50.0 * 1.1) % 16)));
						g2d.drawRoundRect(x[0], y[0], x[1] - x[0], y[1] - y[0] - 1, 8, 8);
					}
				}
			}
		}
	}

	private void resetCellEditor() {
		for (int i = 0; i < getColumnCount(); i++) {
			TableCellEditor defaultEditor = getDefaultEditor(getColumnClass(i));
			if (defaultEditor != null) {
				defaultEditor.cancelCellEditing();
			}
		}
	}

	/**
	 * Maps a table row (in this transposed view, the data column shown on that row) to the
	 * model index of the underlying data column.
	 */
	private int lobDataColumnModelIndex(int tableRow) {
		return rb.rowsTable.getColumnModel().getColumn(tableRow).getModelIndex();
	}

	/**
	 * Maps a table column (&gt; 0; "Row N") to the model index of the underlying data row
	 * (an index into {@link BrowserContentPane#rows}), or -1 if it cannot be resolved.
	 */
	private int lobDataRowModelIndex(int tableColumn) {
		RowSorter<? extends TableModel> sorter = rb.rowsTable.getRowSorter();
		int viewRow = tableColumn - 1;
		if (sorter == null || viewRow < 0 || viewRow >= sorter.getViewRowCount()) {
			return -1;
		}
		return sorter.convertRowIndexToModel(viewRow);
	}

	/**
	 * The data {@link Row} shown by the given table column, or <code>null</code> if it
	 * cannot be resolved.
	 */
	private Row lobRowAt(int tableColumn) {
		int dataRow = lobDataRowModelIndex(tableColumn);
		if (dataRow < 0 || dataRow >= rb.rows.size()) {
			return null;
		}
		return rb.rows.get(dataRow);
	}

	/**
	 * The raw (unwrapped) cell value at the given table cell, read directly from
	 * {@link BrowserContentPane#rows} - the same source {@code DetailsView} uses - rather
	 * than the renderer's {@code value} parameter, which may be wrapped
	 * (e.g. in {@link BrowserContentPane.TableModelItem}).
	 */
	private Object lobRawValueAt(int tableRow, int tableColumn) {
		Row row = lobRowAt(tableColumn);
		if (row == null) {
			return null;
		}
		int dataCol = lobDataColumnModelIndex(tableRow);
		if (dataCol < 0 || dataCol >= row.values.length) {
			return null;
		}
		return row.values[dataCol];
	}

	private static boolean isDisplayableLobType(LobContentType type) {
		return type != null && type != LobContentType.BINARY && type != LobContentType.PLAIN_TEXT;
	}

	/**
	 * Wraps the base cell label into a composite LOB cell: a content-type label and a
	 * "View…" affordance (and, once known, a thumbnail for image content) - mirroring
	 * the LOB cell UI in {@code DetailsView}.
	 */
	private Component buildLobCell(JLabel base, final int tableRow, final int tableColumn, final Object rawValue) {
		final Row dataRow = lobRowAt(tableColumn);
		final int dataCol = lobDataColumnModelIndex(tableRow);

		JPanel lobPanel = new JPanel(new BorderLayout());
		lobPanel.setOpaque(true);
		lobPanel.setBackground(base.getBackground());
		lobPanel.add(base, BorderLayout.CENTER);
		// the placeholder text is always replaced by the button below, so it's
		// never shown once this composite cell is built.
		base.setVisible(false);

		ImageIcon thumb = lobThumbCache.get(tableRow);

		LobContentType type = lobTypeCache.get(tableRow);
		if (type == null) {
			type = LobContentSupport.detectType(rawValue);
		}
		String buttonText;
		boolean reloadable = true;
		if (isDisplayableLobType(type)) {
			buttonText = type.displayName;
		} else if (rawValue instanceof BlobLengthPlaceholder) {
			buttonText = ((BlobLengthPlaceholder) rawValue).plainText();
			reloadable = dataRow != null && rb.isLobReloadable(dataRow, dataCol);
		} else {
			buttonText = "View…";
		}
		JButton viewButton = new JButton(buttonText);
		if (thumb == null && LOB_VIEW_ICON != null) {
			viewButton.setIcon(LOB_VIEW_ICON);
		}
		viewButton.setMargin(new Insets(0, 4, 0, 4));
		viewButton.setEnabled(reloadable);
		viewButton.setToolTipText(reloadable ? LOB_TOOLTIP : LOB_NOT_RELOADABLE_TOOLTIP);
		// painted only - JTable renderer components are non-interactive; clicks are
		// handled by ColumnsTable's own mouse listener.

		JPanel westPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
		westPanel.setOpaque(true);
		westPanel.setBackground(base.getBackground());
		if (thumb != null) {
			JLabel thumbLabel = new JLabel(thumb);
			thumbLabel.setOpaque(true);
			thumbLabel.setBackground(base.getBackground());
			thumbLabel.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 6));
			westPanel.add(thumbLabel);
		}
		westPanel.add(viewButton);
		lobPanel.add(westPanel, BorderLayout.WEST);

		if (lobRequested.add(tableRow)) {
			if (dataRow != null) {
				if (type != null) {
					if (type.isImage()) {
						requestLobThumbnail(tableRow, dataRow, dataCol, rawValue);
					}
				} else {
					rb.retrieveLobContentType(dataRow, dataCol, new Object(), t -> {
						if (t != null) {
							lobTypeCache.put(tableRow, t);
						}
						if (t != null && t.isImage()) {
							requestLobThumbnail(tableRow, dataRow, dataCol, rawValue);
						}
						refreshLobCell();
					});
				}
			}
		}

		return lobPanel;
	}

	private void requestLobThumbnail(final int tableRow, Row dataRow, int dataCol, Object rawValue) {
		rb.retrieveLobImagePreview(dataRow, dataCol, rawValue, 160, 64, new Object(), img -> {
			if (img != null) {
				lobThumbCache.put(tableRow, new ImageIcon(img));
			}
			refreshLobCell();
		});
	}

	private void refreshLobCell() {
		adjustTableColumnsWidth();
		revalidate();
		repaint();
	}

}

