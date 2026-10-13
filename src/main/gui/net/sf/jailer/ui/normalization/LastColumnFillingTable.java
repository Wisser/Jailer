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
package net.sf.jailer.ui.normalization;

import java.awt.Component;

import javax.swing.Icon;
import javax.swing.JTable;
import javax.swing.UIManager;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;

import net.sf.jailer.ui.UIUtil;

/**
 * Table whose columns are adjusted to their content. Space that is left over is not
 * distributed over all columns but given to the last column.
 *
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public class LastColumnFillingTable extends JTable {

	/**
	 * Width the content of the last column needs, -1 if not yet adjusted.
	 */
	private int lastColumnContentWidth = -1;

	public LastColumnFillingTable(TableModel model) {
		super(model);
	}

	/**
	 * Adjusts the widths of the columns to their content.
	 */
	public void adjustColumnsWidth() {
		UIUtil.adjustTableColumnsWidth(this, false);
		TableColumnModel columnModel = getColumnModel();
		int n = columnModel.getColumnCount();
		// the title must fit too, measured with the renderer of the header (its insets differ from those of a cell),
		// plus the sort icon, which every column may get
		JTableHeader header = getTableHeader();
		if (header != null) {
			Icon sortIcon = UIManager.getIcon("Table.ascendingSortIcon");
			int sortIconWidth = sortIcon == null? 0 : sortIcon.getIconWidth() + 4;
			for (int i = 0; i < n; ++i) {
				TableColumn column = columnModel.getColumn(i);
				TableCellRenderer renderer = column.getHeaderRenderer() != null? column.getHeaderRenderer() : header.getDefaultRenderer();
				Component comp = renderer.getTableCellRendererComponent(this, column.getHeaderValue(), false, false, -1, i);
				int titleWidth = comp.getPreferredSize().width + sortIconWidth;
				if (column.getPreferredWidth() < titleWidth) {
					column.setPreferredWidth(titleWidth);
				}
			}
		}
		lastColumnContentWidth = n > 0? columnModel.getColumn(n - 1).getPreferredWidth() : -1;
		revalidate();
		repaint();
	}

	@Override
	public void doLayout() {
		TableColumnModel columnModel = getColumnModel();
		int n = columnModel.getColumnCount();
		boolean userResizes = getTableHeader() != null && getTableHeader().getResizingColumn() != null;
		if (n > 0 && lastColumnContentWidth >= 0 && !userResizes && getAutoResizeMode() != AUTO_RESIZE_OFF) {
			int others = 0;
			for (int i = 0; i < n - 1; ++i) {
				others += columnModel.getColumn(i).getPreferredWidth();
			}
			// the last column gets all the space left over, but at least what its content needs
			TableColumn last = columnModel.getColumn(n - 1);
			last.setPreferredWidth(Math.max(lastColumnContentWidth, getWidth() - others));
		}
		super.doLayout();
	}

}
