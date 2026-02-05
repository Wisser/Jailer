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
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
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
import javax.swing.InputMap;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
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
import net.sf.jailer.ui.databrowser.Row;
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
			setFocusable(false);
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
				List<Integer> types = new ArrayList<Integer>();
				if (rb != null && rb.browserContentCellEditor != null) {
					for (int type: rb.browserContentCellEditor.getColumnTypes()) {
						types.add(type);
					}
				}
				ExtendetCopyPanel.openDialog(tab, false, "?no-name?", types, true, true);
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
				UIUtil.traverse(createPopupMenu(null), null, c-> null, (c, o) -> null, (t, c) -> {
					if (c instanceof JMenuItem) {
						if (KS_EDIT.equals(((JMenuItem) c).getAccelerator())) {
							for (ActionListener al: ((JMenuItem) c).getActionListeners()) {
								al.actionPerformed(e);
							}
						}
					}
				});
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
				for (int i = 0; i < getColumnCount(); i++) {
					TableCellEditor defaultEditor = getDefaultEditor(getColumnClass(i));
					if (defaultEditor != null) {
						defaultEditor.cancelCellEditing();
					}
				}
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

}

