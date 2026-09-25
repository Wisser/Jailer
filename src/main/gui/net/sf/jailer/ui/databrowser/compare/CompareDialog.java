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
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.datatransfer.StringSelection;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableCellRenderer;

import net.sf.jailer.ui.Colors;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.compare.RowComparison.CellStatus;
import net.sf.jailer.ui.databrowser.compare.RowComparison.RowPair;
import net.sf.jailer.ui.databrowser.compare.RowComparison.Status;

/**
 * Shows the result of a {@link RowComparison}: an overview of the row pairs
 * and, for the selected pair, the column-by-column comparison.
 *
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public class CompareDialog extends JDialog {

	private final RowComparison comparison;
	private final List<RowPair> allPairs;
	private final List<RowPair> visiblePairs = new ArrayList<RowPair>();
	private final List<Integer> visibleColumns = new ArrayList<Integer>();
	private RowPair currentPair;

	private final JCheckBox showEqualRows = new JCheckBox("Show equal rows");
	private final JCheckBox showMissingRows = new JCheckBox("Show rows missing on one side", true);
	private final JCheckBox onlyDifferences = new JCheckBox("Only differences");
	private final JLabel summaryLabel = new JLabel();

	private final AbstractTableModel overviewModel = new AbstractTableModel() {
		@Override
		public int getRowCount() {
			return visiblePairs.size();
		}
		@Override
		public int getColumnCount() {
			return 3;
		}
		@Override
		public String getColumnName(int column) {
			return column == 0? "Status" : column == 1? "Key" : "Different Columns";
		}
		@Override
		public Object getValueAt(int rowIndex, int columnIndex) {
			RowPair pair = visiblePairs.get(rowIndex);
			switch (columnIndex) {
			case 0: return pair.status.description + (pair.duplicateKey? " (duplicate key)" : "");
			case 1: return pair.key;
			default: return pair.status == Status.CHANGED? pair.changedColumns : null;
			}
		}
	};

	private final AbstractTableModel detailModel = new AbstractTableModel() {
		@Override
		public int getRowCount() {
			return visibleColumns.size();
		}
		@Override
		public int getColumnCount() {
			return 3;
		}
		@Override
		public String getColumnName(int column) {
			return column == 0? "Column" : column == 1? comparison.left.title : comparison.right.title;
		}
		@Override
		public Object getValueAt(int rowIndex, int columnIndex) {
			int column = visibleColumns.get(rowIndex);
			if (columnIndex == 0) {
				return comparison.columns.get(column);
			}
			return currentPair == null? null : text(currentPair, column, columnIndex == 1);
		}
	};

	// created in the constructor: the models need the comparison to answer the column names
	private final JTable overviewTable;
	private final JTable detailTable;

	/**
	 * Opens the dialog.
	 *
	 * @param owner the owner window
	 * @param title the title
	 * @param comparison the comparison
	 * @param pairs the row pairs to show
	 */
	public CompareDialog(Window owner, String title, RowComparison comparison, List<RowPair> pairs) {
		super(owner, title, ModalityType.MODELESS);
		this.comparison = comparison;
		this.allPairs = pairs;
		this.overviewTable = new JTable(overviewModel);
		this.detailTable = new JTable(detailModel);
		setDefaultCloseOperation(DISPOSE_ON_CLOSE);

		JPanel content = new JPanel(new BorderLayout(0, 4));
		content.setBorder(BorderFactory.createEmptyBorder(8, 8, 8, 8));

		StringBuilder header = new StringBuilder("<html><b>" + UIUtil.toHTMLFragment(comparison.left.title, 0) + "</b> &nbsp;vs&nbsp; <b>" + UIUtil.toHTMLFragment(comparison.right.title, 0) + "</b>");
		if (comparison.left.truncated || comparison.right.truncated) {
			header.append("<br><font color=\"#cc0000\">The rows of "
					+ (comparison.left.truncated && comparison.right.truncated? "both sides have" : comparison.left.truncated? "the left side have" : "the right side have")
					+ " been cut by the row limit. Rows reported as missing may just not have been loaded.</font>");
		}
		header.append("</html>");
		content.add(new JLabel(header.toString()), BorderLayout.NORTH);

		overviewTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		overviewTable.setDefaultRenderer(Object.class, new OverviewRenderer());
		overviewTable.getColumnModel().getColumn(0).setPreferredWidth(160);
		overviewTable.getColumnModel().getColumn(1).setPreferredWidth(360);
		overviewTable.getColumnModel().getColumn(2).setPreferredWidth(120);
		overviewTable.getSelectionModel().addListSelectionListener(e -> {
			if (!e.getValueIsAdjusting()) {
				int i = overviewTable.getSelectedRow();
				currentPair = i >= 0 && i < visiblePairs.size()? visiblePairs.get(i) : null;
				updateDetails();
			}
		});
		detailTable.setDefaultRenderer(Object.class, new DetailRenderer());
		detailTable.getColumnModel().getColumn(0).setPreferredWidth(180);
		detailTable.getColumnModel().getColumn(1).setPreferredWidth(300);
		detailTable.getColumnModel().getColumn(2).setPreferredWidth(300);

		JScrollPane detailScrollPane = new JScrollPane(detailTable);
		boolean withOverview = pairs.size() != 1;
		if (withOverview) {
			JPanel overviewPanel = new JPanel(new BorderLayout());
			JPanel filterPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 8, 0));
			filterPanel.add(summaryLabel);
			filterPanel.add(showEqualRows);
			filterPanel.add(showMissingRows);
			overviewPanel.add(filterPanel, BorderLayout.NORTH);
			overviewPanel.add(new JScrollPane(overviewTable), BorderLayout.CENTER);
			JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, overviewPanel, detailScrollPane);
			splitPane.setResizeWeight(0.4);
			content.add(splitPane, BorderLayout.CENTER);
		} else {
			content.add(detailScrollPane, BorderLayout.CENTER);
		}

		JPanel buttonPanel = new JPanel(new BorderLayout());
		JPanel leftButtons = new JPanel(new FlowLayout(FlowLayout.LEFT, 8, 0));
		leftButtons.add(onlyDifferences);
		buttonPanel.add(leftButtons, BorderLayout.WEST);
		JPanel rightButtons = new JPanel(new FlowLayout(FlowLayout.RIGHT, 8, 0));
		JButton copyButton = new JButton("Copy");
		copyButton.setToolTipText("Copy the differences as text to the clipboard.");
		copyButton.addActionListener(e -> Toolkit.getDefaultToolkit().getSystemClipboard().setContents(new StringSelection(differencesAsText()), null));
		copyButton.setIcon(UIUtil.scaleIcon(copyButton, UIUtil.readImage("/copy.png")));
		JButton closeButton = new JButton("Close");
		closeButton.setIcon(UIUtil.scaleIcon(closeButton, UIUtil.readImage("/buttoncancel.png")));
		closeButton.addActionListener(e -> dispose());
		rightButtons.add(copyButton);
		rightButtons.add(closeButton);
		buttonPanel.add(rightButtons, BorderLayout.EAST);
		content.add(buttonPanel, BorderLayout.SOUTH);

		showEqualRows.addActionListener(e -> updateOverview());
		showMissingRows.addActionListener(e -> updateOverview());
		onlyDifferences.addActionListener(e -> updateDetails());

		boolean anyDifference = pairs.stream().anyMatch(p -> p.status != Status.EQUAL);
		showEqualRows.setSelected(!anyDifference);
		onlyDifferences.setSelected(false);

		setContentPane(content);
		UIUtil.initComponents(this);
		if (withOverview) {
			updateOverview();
		} else {
			currentPair = pairs.isEmpty()? null : pairs.get(0);
			updateDetails();
		}
		java.awt.geom.Rectangle2D screen = UIUtil.getScreenBounds();
		setSize(new Dimension((int) Math.min(1200, screen.getWidth() * 0.8), (int) Math.min(withOverview? 880 : 720, screen.getHeight() * 0.8)));
		UIUtil.setInitialWindowLocation(this, owner, 100, 100);
		UIUtil.fit(this);
		setVisible(true);
	}

	private void updateOverview() {
		visiblePairs.clear();
		int equal = 0, changed = 0, missing = 0;
		for (RowPair pair: allPairs) {
			switch (pair.status) {
			case EQUAL: ++equal; break;
			case CHANGED: ++changed; break;
			default: ++missing; break;
			}
			if (pair.status == Status.EQUAL && !showEqualRows.isSelected()) {
				continue;
			}
			if ((pair.status == Status.ONLY_LEFT || pair.status == Status.ONLY_RIGHT) && !showMissingRows.isSelected()) {
				continue;
			}
			visiblePairs.add(pair);
		}
		summaryLabel.setText(equal + " equal, " + changed + " different, " + missing + " missing on one side  ");
		overviewModel.fireTableDataChanged();
		UIUtil.adjustTableColumnsWidth(overviewTable, false);
		if (!visiblePairs.isEmpty()) {
			overviewTable.getSelectionModel().setSelectionInterval(0, 0);
		} else {
			currentPair = null;
			updateDetails();
		}
	}

	private void updateDetails() {
		visibleColumns.clear();
		for (int i = 0; i < comparison.columns.size(); ++i) {
			if (onlyDifferences.isSelected() && currentPair != null) {
				CellStatus s = comparison.cellStatus(currentPair, i);
				if (s == CellStatus.EQUAL || (s == CellStatus.MISSING && currentPair.left != null && currentPair.right != null
						&& comparison.leftIndex(i) >= 0 && comparison.rightIndex(i) >= 0)) {
					continue;
				}
			}
			visibleColumns.add(i);
		}
		detailModel.fireTableDataChanged();
		UIUtil.adjustTableColumnsWidth(detailTable, false);
	}

	private String text(RowPair pair, int column, boolean isLeft) {
		Object[] row = isLeft? pair.left : pair.right;
		int index = isLeft? comparison.leftIndex(column) : comparison.rightIndex(column);
		if (row == null || index < 0) {
			return null;
		}
		Object value = isLeft? comparison.leftValue(pair, column) : comparison.rightValue(pair, column);
		if (value == null) {
			return UIUtil.NULL;
		}
		String text = (isLeft? comparison.left : comparison.right).toText(index, value);
		return text == null? UIUtil.NULL : text;
	}

	private String differencesAsText() {
		StringBuilder sb = new StringBuilder();
		sb.append(comparison.left.title).append(" vs ").append(comparison.right.title).append("\n");
		for (RowPair pair: allPairs) {
			if (pair.status == Status.EQUAL) {
				continue;
			}
			sb.append("\n").append(pair.key).append(": ").append(pair.status.description).append("\n");
			if (pair.status == Status.CHANGED) {
				for (int i = 0; i < comparison.columns.size(); ++i) {
					CellStatus s = comparison.cellStatus(pair, i);
					if (s == CellStatus.CHANGED || (s == CellStatus.MISSING && (comparison.leftIndex(i) < 0 || comparison.rightIndex(i) < 0))) {
						String l = text(pair, i, true);
						String r = text(pair, i, false);
						sb.append("  ").append(comparison.columns.get(i)).append(": ")
							.append(l == null? "(no column)" : l).append(" -> ").append(r == null? "(no column)" : r).append("\n");
					}
				}
			}
		}
		return sb.toString();
	}

	private class OverviewRenderer extends DefaultTableCellRenderer {
		@Override
		public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
			Component c = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
			if (!isSelected && row < visiblePairs.size()) {
				Status status = visiblePairs.get(row).status;
				c.setBackground(status == Status.CHANGED? Colors.Color_255_255_205
						: status == Status.EQUAL? table.getBackground() : Colors.Color_255_210_210);
			}
			return c;
		}
	}

	/**
	 * Background of a value column, alternating per column like the "Columns" view of the SQL Console.
	 */
	private static java.awt.Color valueBackground(int column) {
		return column % 2 == 0? UIUtil.TABLE_BACKGROUND_COLOR_1 : UIUtil.TABLE_BACKGROUND_COLOR_2;
	}

	private class DetailRenderer extends DefaultTableCellRenderer {
		@Override
		public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
			Component c = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
			Font font = table.getFont();
			if (c instanceof JComponent) {
				((JComponent) c).setToolTipText(null);
			}
			if (column > 0 && currentPair != null && row < visibleColumns.size()) {
				int aligned = visibleColumns.get(row);
				CellStatus s = comparison.cellStatus(currentPair, aligned);
				boolean isLeft = column == 1;
				boolean sideMissing = (isLeft? currentPair.left == null || comparison.leftIndex(aligned) < 0
						: currentPair.right == null || comparison.rightIndex(aligned) < 0);
				if (!isSelected) {
					c.setForeground(table.getForeground());
					if (sideMissing) {
						c.setBackground(Colors.Color_255_210_210);
					} else if (s == CellStatus.CHANGED || s == CellStatus.MISSING) {
						c.setBackground(Colors.Color_255_255_205);
					} else {
						c.setBackground(valueBackground(column));
					}
					if (UIUtil.NULL.equals(value) && !sideMissing && (isLeft? comparison.leftValue(currentPair, aligned) : comparison.rightValue(currentPair, aligned)) == null) {
						c.setForeground(Colors.Color_128_128_128);
						font = font.deriveFont(Font.ITALIC);
					}
				}
				if (sideMissing && c instanceof JLabel) {
					((JLabel) c).setText(currentPair.left == null && isLeft || currentPair.right == null && !isLeft? "(no row)" : "(no column)");
					font = font.deriveFont(Font.ITALIC);
				}
				if (s == CellStatus.NOT_COMPARED && c instanceof JComponent) {
					((JComponent) c).setToolTipText("LOB content is not compared.");
					if (!isSelected) {
						c.setForeground(Colors.Color_128_128_128);
					}
				}
			} else if (!isSelected) {
				if (column == 0) {
					// the name column looks like the one of the single row view (ColumnsTable)
					c.setBackground(row % 2 == 0? UIUtil.TABLE_BACKGROUND_COLOR_1_INCLOSURE : UIUtil.TABLE_BACKGROUND_COLOR_2_INCLOSURE);
					int aligned = row < visibleColumns.size()? visibleColumns.get(row) : -1;
					c.setForeground(aligned >= 0 && comparison.isPrimaryKey(aligned)? UIUtil.FG_PK
							: aligned >= 0 && comparison.isForeignKey(aligned)? UIUtil.FG_FK : table.getForeground());
				} else {
					c.setBackground(valueBackground(column));
					c.setForeground(table.getForeground());
				}
			}
			c.setFont(font);
			return c;
		}
	}

}
