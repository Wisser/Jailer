/*
 * Copyright 2007 - 2021 Ralf Wisser.
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

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.Vector;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.InputMap;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
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

import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.BrowserContentPane;
import net.sf.jailer.ui.databrowser.Row;

/**
 * "Column view" of a query result table.
 *  
 * @author Ralf Wisser
 */
public class ColumnsTable extends JTable {
	private static final long serialVersionUID = 1L;

	private final int MAX_ROWS = 498;
	private static final KeyStroke KS_COPY_TO_CLIPBOARD = KeyStroke.getKeyStroke(KeyEvent.VK_C, InputEvent.CTRL_DOWN_MASK);
	final BrowserContentPane rb;
	
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public ColumnsTable(final BrowserContentPane rb) {
		this.rb = rb;
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
					return rowsTable.getModel().getColumnName(column);
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

		setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		setRowSelectionAllowed(true);
		setColumnSelectionAllowed(true);
		setCellSelectionEnabled(true);
		// getTableHeader().setReorderingAllowed(false);
		setShowGrid(false);
		setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
		setModel(cDm);
		
		for (int i = 0; i < getColumnCount(); i++) {
			TableCellEditor defaultEditor = rowsTable.getDefaultEditor(getColumnClass(i));
			if (defaultEditor != null) {
				defaultEditor.cancelCellEditing();
			}
			setDefaultEditor(getColumnClass(i), defaultEditor);
		}

		InputMap im = getInputMap();
		Object key = "copyClipboard";
		im.put(KS_COPY_TO_CLIPBOARD, key);
		ActionMap am = getActionMap();
		Action a = new AbstractAction() {
			private static final long serialVersionUID = 1L;

			@Override
			public void actionPerformed(ActionEvent e) {
				UIUtil.copyToClipboard(ColumnsTable.this, false);
			}
		};
		am.put(key, a);

		addMouseListener(new MouseListener() {
			@Override
			public void mouseReleased(MouseEvent e) {
			}

			@Override
			public void mousePressed(MouseEvent e) {
				int ri = rowAtPoint(e.getPoint());
				if (ri >= 0) {
					Rectangle r = getCellRect(ri, 0, false);
					int x = Math.max(e.getPoint().x, (int) r.getMinX());
					int y = (int) r.getMaxY() - 2;
					if (e.getButton() != MouseEvent.BUTTON1) {
						JPopupMenu popup;
						popup = createPopupMenu(e);
						if (popup != null) {
							popup.show(ColumnsTable.this, x, y);
						}
					} else if (e.getClickCount() > 1) {
						int i = -1;
						ri = columnAtPoint(e.getPoint()) - 1;
						if (ri >= 0 && !rb.rows.isEmpty() && rb.rowsTable.getRowSorter().getViewRowCount() > 0) {
							i = rb.rowsTable.getRowSorter().convertRowIndexToModel(ri);
							Point p = new Point(e.getX(), e.getY());
							SwingUtilities.convertPointToScreen(p, ColumnsTable.this);
							rb.openDetailsView(i, (int) p.getX(), (int) p.getY());
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
			final Color BGCOLUMNS = new Color(255, 255, 220);
			final Color BGSELECTED  = new Color(255, 230, 220);
			final Font font = new JLabel().getFont();
			final Font italic = new Font(font.getName(), font.getStyle() | Font.ITALIC, font.getSize());
			@Override
			public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected,
					boolean hasFocus, int row, int column) {
				int dmColumn = column;
				if (dmColumn  > 0) {
					--dmColumn;
				}
				Component render = rowsTable.getCellRenderer(dmColumn, row).getTableCellRendererComponent(ColumnsTable.this, value, isSelected, hasFocus, dmColumn, row);
				int currentColumn = rb.getCurrentRowSelection();
				if (render instanceof JLabel) {
					if (column == 0) {
						((JLabel) render).setFont(italic);
						((JLabel) render).setBackground(BGCOLUMNS);
					} else if (column - 1 == currentColumn) {
						((JLabel) render).setBackground(BGSELECTED);
					}
				}
				return render;
			}
		});
		adjustTableColumnsWidth();
	}

	/**
	 * Creates popup menu. 
	 * @param e mouse event 
	 */
	private JPopupMenu createPopupMenu(MouseEvent e) {
		int i = -1;
		Row row = null;
		int ri = columnAtPoint(e.getPoint()) - 1;
		if (ri >= 0 && !rb.rows.isEmpty() && rb.rowsTable.getRowSorter().getViewRowCount() > 0) {
			i = rb.rowsTable.getRowSorter().convertRowIndexToModel(ri);
			row = rb.rows.get(i);
		} else {
			return null;
		}
		JMenuItem copyTCB = new JMenuItem("Copy to Clipboard");
		// copyTCB.setAccelerator(KS_COPY_TO_CLIPBOARD);
		copyTCB.setEnabled(getSelectedColumnCount() > 0);
		copyTCB.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				UIUtil.copyToClipboard(ColumnsTable.this, false);
			}
		});
		Point p = new Point(e.getX(), e.getY());
		SwingUtilities.convertPointToScreen(p, this);
		return rb.createPopupMenu(this, row, i, (int) p.getX(), (int) p.getY(), false, copyTCB, new Runnable() {
			@Override
			public void run() {
				repaint();
				for (int i = 0; i < getColumnCount(); i++) {
					TableCellEditor defaultEditor = getDefaultEditor(getColumnClass(i));
					if (defaultEditor != null) {
						defaultEditor.cancelCellEditing();
					}
				}
			}
		}, false);
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
			column.setPreferredWidth(Math.min(maxWidth, width));
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

}
