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
package net.sf.jailer.ui.databrowser.sqlconsole;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
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
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;

import net.sf.jailer.ui.UIUtil;

/**
 * "Column view" of a query result table.
 *  
 * @author Ralf Wisser
 */
public class ColumnsTable extends JTable {

	private final int MAX_ROWS = 198;
	private static final KeyStroke KS_COPY_TO_CLIPBOARD = KeyStroke.getKeyStroke(KeyEvent.VK_C, InputEvent.CTRL_DOWN_MASK);

	public ColumnsTable(final JTable rowsTable) {
		TableModel rDm = rowsTable.getModel();
		RowSorter<? extends TableModel> sorter = rowsTable.getRowSorter();
		Vector cNames = new Vector();
		cNames.add("Column");
		for (int i = 0; i < sorter.getViewRowCount(); ++i) {
			if (i > MAX_ROWS) {
				cNames.add("Row " + (i + 1) + " (" + (sorter.getViewRowCount() - i - 1) + " more)");
				break;
			}
			cNames.add("Row " + (i + 1));
		}
		DefaultTableModel cDm = new DefaultTableModel(cNames, rDm.getColumnCount()) {
			@Override
			public boolean isCellEditable(int row, int column) {
				return false;
			}
		};
		TableColumnModel cm = rowsTable.getColumnModel();
		for (int x = 0; x < rDm.getColumnCount(); ++x) {
			int mx = cm.getColumn(x).getModelIndex();
			cDm.setValueAt(rDm.getColumnName(mx), x, 0);
			for (int y = 0; y < sorter.getViewRowCount(); ++y) {
				cDm.setValueAt(rDm.getValueAt(sorter.convertRowIndexToModel(y), mx), x, y + 1);
				if (y > MAX_ROWS) {
					break;
				}
			}
		}
		setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		setRowSelectionAllowed(true);
		setColumnSelectionAllowed(true);
		setCellSelectionEnabled(true);
		setShowGrid(false);
		setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
		setModel(cDm);
		
		InputMap im = getInputMap();
		Object key = "copyClipboard";
		im.put(KS_COPY_TO_CLIPBOARD, key);
		ActionMap am = getActionMap();
		Action a = new AbstractAction() {
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
						int x = Math.max((int) e.getPoint().x, (int) r.getMinX());
						int y = (int) r.getMaxY() - 2;
						if (e.getButton() != MouseEvent.BUTTON1) {
							JPopupMenu popup;
							popup = createPopupMenu();
							if (popup != null) {
								popup.show(ColumnsTable.this, x, y);
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
//			final Color BGSELECTED  = new Color(130, 200, 255);
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
				if (render instanceof JLabel) {
					if (column == 0) {
						((JLabel) render).setFont(italic);
						((JLabel) render).setBackground(BGCOLUMNS);
//					} else if (isSelected) {
//						((JLabel) render).setBackground(BGSELECTED);
					}
				}
				return render;
			}
		});
		adjustTableColumnsWidth();
	}

	/**
	 * Creates popup menu. 
	 */
	public JPopupMenu createPopupMenu() {
		JMenuItem copyTCB = new JMenuItem("Copy to Clipboard");
		copyTCB.setAccelerator(KS_COPY_TO_CLIPBOARD);
		copyTCB.setEnabled(getSelectedColumnCount() > 0);
		copyTCB.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				UIUtil.copyToClipboard(ColumnsTable.this, false);
			}
		});
				
		JPopupMenu popup = new JPopupMenu();
		popup.add(copyTCB);
		return popup;
	}

	private void adjustTableColumnsWidth() {
		DefaultTableModel dtm = (DefaultTableModel) getModel();
		int maxWidth = getColumnCount() > 2? 300 : 1200;
		for (int i = 0; i < getColumnCount(); i++) {
			TableColumn column = getColumnModel().getColumn(i);
			int width = 0;

			Component comp = getDefaultRenderer(String.class).getTableCellRendererComponent(this, column.getHeaderValue(), false, false, 0, i);
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

}
