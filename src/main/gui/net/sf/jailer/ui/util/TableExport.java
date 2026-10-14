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
package net.sf.jailer.ui.util;

import java.awt.Component;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;

import javax.swing.JTable;
import javax.swing.table.TableModel;

import net.sf.jailer.ui.UIUtil;

/**
 * Saves the rows of a table that are currently visible, i.e. that are not hidden by a filter,
 * in the order shown, as CSV or Excel file. Numbers stay numbers, everything else is written as text.
 *
 * @author Ralf Wisser
 */
public class TableExport {

	/**
	 * Saves the visible rows as CSV file, in the format the user chooses (see {@link CsvOptionsDialog}).
	 *
	 * @param parent the parent component of the dialogs
	 * @param table the table
	 * @param firstColumn index of the first model column to be written (to omit check box columns, for instance)
	 */
	public static void saveAsCsv(Component parent, JTable table, int firstColumn) {
		CsvOptionsDialog.CsvFormat format = CsvOptionsDialog.show(parent);
		if (format == null) {
			return;
		}
		String fileName = CsvOptionsDialog.chooseFile(parent);
		if (fileName == null) {
			return;
		}
		TableModel model = table.getModel();
		StringBuilder csv = new StringBuilder();
		if (format.header) {
			for (int c = firstColumn; c < model.getColumnCount(); ++c) {
				csv.append(c > firstColumn? format.separator : "").append(format.cell(model.getColumnName(c), true));
			}
			csv.append(format.lineBreak);
		}
		for (int i = 0; i < table.getRowCount(); ++i) {
			int row = table.convertRowIndexToModel(i);
			for (int c = firstColumn; c < model.getColumnCount(); ++c) {
				Object value = model.getValueAt(row, c);
				csv.append(c > firstColumn? format.separator : "").append(format.value(value == null || value instanceof Number? value : String.valueOf(value)));
			}
			csv.append(format.lineBreak);
		}
		try {
			format.write(fileName, csv.toString());
		} catch (Throwable t) {
			UIUtil.showException(parent, "Error", t);
		}
	}

	/**
	 * Saves the visible rows as Excel file (.xlsx), with filter drop-downs in the header row.
	 *
	 * @param parent the parent component of the dialogs
	 * @param table the table
	 * @param firstColumn index of the first model column to be written (to omit check box columns, for instance)
	 * @param sheetName name of the sheet
	 */
	public static void saveAsXlsx(Component parent, JTable table, int firstColumn, String sheetName) {
		String fileName = UIUtil.choseFile(null, ".", "Excel File", ".xlsx", parent, true, false);
		if (fileName == null) {
			return;
		}
		if (!fileName.toLowerCase(Locale.ENGLISH).endsWith(".xlsx")) {
			fileName += ".xlsx";
		}
		TableModel model = table.getModel();
		File file = new File(fileName);
		boolean written = false;
		try (SimpleXlsxWriter writer = new SimpleXlsxWriter(new BufferedOutputStream(new FileOutputStream(file)), Collections.singletonList(sheetName))) {
			writer.beginSheet(0);
			List<SimpleXlsxWriter.Cell> header = new ArrayList<SimpleXlsxWriter.Cell>();
			for (int c = firstColumn; c < model.getColumnCount(); ++c) {
				header.add(SimpleXlsxWriter.Cell.text(model.getColumnName(c), true));
			}
			writer.row(header);
			for (int i = 0; i < table.getRowCount(); ++i) {
				int row = table.convertRowIndexToModel(i);
				List<SimpleXlsxWriter.Cell> cells = new ArrayList<SimpleXlsxWriter.Cell>();
				for (int c = firstColumn; c < model.getColumnCount(); ++c) {
					Object value = model.getValueAt(row, c);
					cells.add(value == null? null : value instanceof Number? SimpleXlsxWriter.Cell.number((Number) value, false) : SimpleXlsxWriter.Cell.text(String.valueOf(value), false));
				}
				writer.row(cells);
			}
			writer.setAutoFilter(1, 0, writer.getRowNumber(), model.getColumnCount() - firstColumn - 1);
			writer.endSheet();
			written = true;
		} catch (Throwable t) {
			UIUtil.showException(parent, "Error", t);
		}
		if (!written) {
			file.delete();
		}
	}

}
