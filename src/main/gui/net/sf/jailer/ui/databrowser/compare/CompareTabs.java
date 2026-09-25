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
package net.sf.jailer.ui.databrowser.compare;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.Window;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.BrowserContentPane;
import net.sf.jailer.ui.databrowser.Row;
import net.sf.jailer.ui.databrowser.compare.RowComparison.RowPair;
import net.sf.jailer.ui.databrowser.compare.RowComparison.Side;

/**
 * Compares the rows of two SQL Console result tabs.
 *
 * @author Ralf Wisser
 */
public class CompareTabs {

	private static final String TITLE = "Compare Results";

	/**
	 * Lets the user choose the key columns and shows the comparison.
	 *
	 * @param owner the owner window
	 * @param leftTitle title of the first result
	 * @param left the first result
	 * @param rightTitle title of the second result
	 * @param right the second result
	 */
	public static void compare(Window owner, String leftTitle, BrowserContentPane left, String rightTitle, BrowserContentPane right) {
		RowComparison comparison = new RowComparison(side(leftTitle, left), side(rightTitle, right));
		List<Integer> common = comparison.commonColumns();
		if (common.isEmpty()) {
			JOptionPane.showMessageDialog(owner, "The two results have no column in common.", TITLE, JOptionPane.INFORMATION_MESSAGE);
			return;
		}

		// preselect the primary key columns known on both sides, otherwise the first common column
		Set<Integer> leftPK = left.getPrimaryKeyColumnIndexes();
		Set<Integer> rightPK = right.getPrimaryKeyColumnIndexes();
		Map<Integer, JCheckBox> checkBoxes = new LinkedHashMap<Integer, JCheckBox>();
		boolean anySelected = false;
		for (int c: common) {
			boolean pk = leftPK.contains(comparison.leftIndex(c)) && rightPK.contains(comparison.rightIndex(c));
			JCheckBox checkBox = new JCheckBox(comparison.columns.get(c), pk);
			anySelected |= pk;
			checkBoxes.put(c, checkBox);
		}
		if (!anySelected) {
			checkBoxes.values().iterator().next().setSelected(true);
		}
		JPanel list = new JPanel(new GridLayout(0, 1));
		for (JCheckBox checkBox: checkBoxes.values()) {
			list.add(checkBox);
		}
		JPanel panel = new JPanel(new BorderLayout(0, 6));
		panel.add(new JLabel("<html>Rows with equal values in these columns are compared with each other:</html>"), BorderLayout.NORTH);
		JScrollPane scrollPane = new JScrollPane(list);
		scrollPane.getVerticalScrollBar().setUnitIncrement(Math.max(16, checkBoxes.values().iterator().next().getPreferredSize().height));
		scrollPane.setPreferredSize(new java.awt.Dimension(360, Math.min(300, 28 * checkBoxes.size() + 8)));
		panel.add(scrollPane, BorderLayout.CENTER);

		JButton okButton = new JButton("OK");
		okButton.setIcon(UIUtil.scaleIcon(okButton, UIUtil.readImage("/buttonok.png")));
		JButton cancelButton = new JButton("Cancel");
		cancelButton.setIcon(UIUtil.scaleIcon(cancelButton, UIUtil.readImage("/buttoncancel.png")));
		for (JButton button: new JButton[] { okButton, cancelButton }) {
			button.addActionListener(e -> {
				JOptionPane pane = (JOptionPane) SwingUtilities.getAncestorOfClass(JOptionPane.class, button);
				if (pane != null) {
					pane.setValue(button);
				}
			});
		}

		List<Integer> keyColumns = new ArrayList<Integer>();
		while (keyColumns.isEmpty()) {
			int choice = JOptionPane.showOptionDialog(owner, panel, TITLE + " - Key Columns", JOptionPane.OK_CANCEL_OPTION, JOptionPane.PLAIN_MESSAGE,
					null, new Object[] { okButton, cancelButton }, okButton);
			if (choice != 0) {
				return;
			}
			for (Map.Entry<Integer, JCheckBox> e: checkBoxes.entrySet()) {
				if (e.getValue().isSelected()) {
					keyColumns.add(e.getKey());
				}
			}
		}
		List<RowPair> pairs = comparison.matchByKey(keyColumns);
		new CompareDialog(owner, TITLE, comparison, pairs);
	}

	private static Side side(String title, BrowserContentPane pane) {
		List<String> columns = new ArrayList<String>();
		for (int i = 0; i < pane.rowsTable.getModel().getColumnCount(); ++i) {
			columns.add(pane.rowsTable.getModel().getColumnName(i));
		}
		List<Object[]> rows = new ArrayList<Object[]>();
		for (Row row: pane.rows) {
			rows.add(row.values);
		}
		return new Side(title, columns, rows, pane.isRowLimitExceeded(),
				(column, value) -> pane.browserContentCellEditor.cellContentToText(column, value))
				.withKeyColumns(pane.getPrimaryKeyColumnIndexes(), pane.getForeignKeyColumnIndexes());
	}

}
