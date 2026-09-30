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
import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import javax.swing.AbstractAction;
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
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableRowSorter;

import net.sf.jailer.ui.Colors;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.BrowserContentPane;
import net.sf.jailer.ui.databrowser.FullTextSearchPanel;
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

	// replaced on a refresh
	private RowComparison comparison;
	private List<RowPair> allPairs;
	private final List<Integer> keyColumns;
	private final Function<Window, RowComparison> recompare;
	private final JLabel headerLabel = new JLabel();
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
			return 4;
		}
		@Override
		public String getColumnName(int column) {
			return column == 0? "Status" : column == 1? "Key" : column == 2? "Different Columns" : "Matches";
		}
		@Override
		public Object getValueAt(int rowIndex, int columnIndex) {
			RowPair pair = visiblePairs.get(rowIndex);
			switch (columnIndex) {
			case 0: return pair.status.description + (pair.duplicateKey? " (duplicate key)" : "");
			case 1: return pair.key;
			case 2: return pair.status == Status.CHANGED? pair.changedColumns : null;
			default:
				// occurrences of the search text in the column comparison of the pair
				String searchText = detailSearchPanel.getSearchField().getText();
				if (searchText.trim().isEmpty()) {
					return null;
				}
				int n = numberOfOccurrences(pair, searchText);
				return n > 0? n : null;
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
			return cellText(currentPair, visibleColumns.get(rowIndex), columnIndex);
		}
	};

	/**
	 * Text of a cell of the column comparison of a pair.
	 *
	 * @param pair the pair, or <code>null</code>
	 * @param column aligned column index
	 * @param columnIndex 0 (column name), 1 (left) or 2 (right)
	 */
	private String cellText(RowPair pair, int column, int columnIndex) {
		if (columnIndex == 0) {
			return comparison.columns.get(column);
		}
		if (pair == null) {
			return null;
		}
		boolean isLeft = columnIndex == 1;
		String text = text(pair, column, isLeft);
		if (text == null) {
			// part of the model (not only of the rendering) to be found by the full text search
			return (isLeft? pair.left : pair.right) == null? "(no row)" : "(no column)";
		}
		return text;
	}

	// created in the constructor: the models need the comparison to answer the column names
	private final JTable overviewTable;
	private final JTable detailTable;
	private final FullTextSearchPanel detailSearchPanel;

	/**
	 * Number of occurrences of a search text per pair.
	 */
	private final Map<String, Map<RowPair, Integer>> occurrencesCache = new HashMap<String, Map<RowPair, Integer>>();

	/**
	 * Opens the dialog.
	 *
	 * @param owner the owner window
	 * @param title the title
	 * @param comparison the comparison
	 * @param pairs the row pairs to show
	 */
	public CompareDialog(Window owner, String title, RowComparison comparison, List<RowPair> pairs) {
		this(owner, title, comparison, pairs, null, null);
	}

	/**
	 * Opens the dialog, which can be refreshed.
	 *
	 * @param owner the owner window
	 * @param title the title
	 * @param comparison the comparison
	 * @param pairs the row pairs to show
	 * @param keyColumns the aligned columns the pairs are matched by, or <code>null</code>
	 * @param recompare compares again (with the same left side) on "Refresh", returns <code>null</code> if cancelled or failed;
	 *                  or <code>null</code> if the comparison can't be refreshed
	 */
	public CompareDialog(Window owner, String title, RowComparison comparison, List<RowPair> pairs, List<Integer> keyColumns, Function<Window, RowComparison> recompare) {
		super(owner, title, ModalityType.MODELESS);
		this.comparison = comparison;
		this.allPairs = pairs;
		this.keyColumns = keyColumns;
		this.recompare = keyColumns == null? null : recompare;
		this.overviewTable = new JTable(overviewModel);
		this.detailTable = new JTable(detailModel);
		// only the column names are sortable; set before the search panel is created, which listens to the sorter
		TableRowSorter<AbstractTableModel> detailSorter = new TableRowSorter<AbstractTableModel>(detailModel);
		detailSorter.setComparator(0, String.CASE_INSENSITIVE_ORDER);
		detailSorter.setSortable(1, false);
		detailSorter.setSortable(2, false);
		detailTable.setRowSorter(detailSorter);
		// searches the column comparisons of all pairs shown in the overview, selecting another pair if needed
		this.detailSearchPanel = new FullTextSearchPanel(detailTable) {
			@Override
			protected boolean isSearchable(int modelColumn) {
				// only the values, not the column names
				return modelColumn != 0;
			}
			@Override
			protected boolean showNeighbor(String searchText, boolean forward) {
				return selectPairWithOccurrences(searchText, forward);
			}
			@Override
			protected boolean hasOccurrencesElsewhere(String searchText) {
				for (RowPair pair: searchablePairs()) {
					if (pair != currentPair && numberOfOccurrences(pair, searchText) > 0) {
						return true;
					}
				}
				return false;
			}
			@Override
			protected Integer numberOfAllOccurrences(String searchText) {
				List<RowPair> pairs = searchablePairs();
				if (pairs.size() <= 1) {
					return null;
				}
				int n = 0;
				for (RowPair pair: pairs) {
					n += numberOfOccurrences(pair, searchText);
				}
				return n;
			}
			@Override
			protected int numberOfOccurrencesBefore(String searchText) {
				int n = 0;
				for (RowPair pair: searchablePairs()) {
					if (pair == currentPair) {
						return n;
					}
					n += numberOfOccurrences(pair, searchText);
				}
				return 0;
			}
		};
		setDefaultCloseOperation(DISPOSE_ON_CLOSE);

		JPanel content = new JPanel(new BorderLayout(0, 4));
		content.setBorder(BorderFactory.createEmptyBorder(8, 8, 8, 8));

		updateHeader();
		content.add(headerLabel, BorderLayout.NORTH);

		overviewTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		overviewTable.setDefaultRenderer(Object.class, new OverviewRenderer());
		overviewTable.getColumnModel().getColumn(0).setPreferredWidth(160);
		overviewTable.getColumnModel().getColumn(1).setPreferredWidth(360);
		overviewTable.getColumnModel().getColumn(2).setPreferredWidth(120);
		overviewTable.getColumnModel().getColumn(3).setPreferredWidth(80);
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

		JPanel detailPanel = new JPanel(new BorderLayout());
		detailPanel.add(new JScrollPane(detailTable), BorderLayout.CENTER);
		// directly below the column comparison it applies to, flush with its left edge
		JPanel onlyDifferencesPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
		onlyDifferencesPanel.add(onlyDifferences);
		detailPanel.add(onlyDifferencesPanel, BorderLayout.SOUTH);
		// a refresh can add rows
		boolean withOverview = pairs.size() != 1 || this.recompare != null;
		if (withOverview) {
			JPanel overviewPanel = new JPanel(new BorderLayout());
			JPanel filterPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 8, 0));
			filterPanel.add(summaryLabel);
			filterPanel.add(showEqualRows);
			filterPanel.add(showMissingRows);
			overviewPanel.add(filterPanel, BorderLayout.NORTH);
			overviewPanel.add(new JScrollPane(overviewTable), BorderLayout.CENTER);
			JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, overviewPanel, detailPanel);
			splitPane.setResizeWeight(0.4);
			content.add(splitPane, BorderLayout.CENTER);
		} else {
			content.add(detailPanel, BorderLayout.CENTER);
		}
		initSearch();

		JPanel buttonPanel = new JPanel(new BorderLayout());
		// no leading gap: the search field is flush with the left edge of the table above
		JPanel leftButtons = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
		detailSearchPanel.removeToolBarBorder();
		leftButtons.add(detailSearchPanel);
		buttonPanel.add(leftButtons, BorderLayout.WEST);
		JPanel rightButtons = new JPanel(new FlowLayout(FlowLayout.RIGHT, 8, 0));
		if (this.recompare != null) {
			JButton refreshButton = new JButton("Refresh");
			refreshButton.setToolTipText("Read the current rows again and compare them with the rows on the left side.");
			refreshButton.setIcon(UIUtil.scaleIcon(refreshButton, UIUtil.readImage("/run.png")));
			refreshButton.addActionListener(e -> refresh());
			rightButtons.add(refreshButton);
		}
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
		onlyDifferences.addActionListener(e -> {
			// changes the columns searched
			occurrencesCache.clear();
			updateDetails();
			overviewTable.repaint();
		});
		// the column "Matches" of the overview follows the search text
		detailSearchPanel.getSearchField().getDocument().addDocumentListener(new DocumentListener() {
			@Override
			public void insertUpdate(DocumentEvent e) {
				overviewTable.repaint();
			}
			@Override
			public void removeUpdate(DocumentEvent e) {
				overviewTable.repaint();
			}
			@Override
			public void changedUpdate(DocumentEvent e) {
				overviewTable.repaint();
			}
		});

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

	/**
	 * Shows the titles of the sides and whether rows have been cut by the row limit.
	 */
	private void updateHeader() {
		StringBuilder header = new StringBuilder("<html><b>" + UIUtil.toHTMLFragment(comparison.left.title, 0) + "</b> &nbsp;vs&nbsp; <b>" + UIUtil.toHTMLFragment(comparison.right.title, 0) + "</b>");
		if (comparison.left.truncated || comparison.right.truncated) {
			header.append("<br><font color=\"#cc0000\">The rows of "
					+ (comparison.left.truncated && comparison.right.truncated? "both sides have" : comparison.left.truncated? "the left side have" : "the right side have")
					+ " been cut by the row limit. Rows reported as missing may just not have been loaded.</font>");
		}
		header.append("</html>");
		headerLabel.setText(header.toString());
	}

	/**
	 * Compares again and shows the result, keeping the selected pair if it still exists.
	 */
	private void refresh() {
		RowComparison newComparison = recompare.apply(this);
		if (newComparison == null) {
			return;
		}
		for (int k: keyColumns) {
			if (k >= newComparison.columns.size() || newComparison.leftIndex(k) < 0 || newComparison.rightIndex(k) < 0) {
				UIUtil.showException(this, getTitle(), new IllegalStateException("Key column \"" + comparison.columns.get(k) + "\" no longer found."), UIUtil.EXCEPTION_CONTEXT_USER_ERROR);
				return;
			}
		}
		String selectedKey = currentPair == null? null : currentPair.key;
		comparison = newComparison;
		allPairs = newComparison.matchByKey(keyColumns);
		occurrencesCache.clear();
		updateHeader();
		updateOverview();
		if (selectedKey != null) {
			for (int i = 0; i < visiblePairs.size(); ++i) {
				if (selectedKey.equals(visiblePairs.get(i).key)) {
					overviewTable.getSelectionModel().setSelectionInterval(i, i);
					overviewTable.scrollRectToVisible(overviewTable.getCellRect(i, 0, true));
					break;
				}
			}
		}
	}

	/**
	 * Opens the full text search panel of the column comparison. Ctrl+F focuses it.
	 */
	private void initSearch() {
		detailSearchPanel.openPermanently();
		getRootPane().getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(BrowserContentPane.KS_FIND, "find");
		getRootPane().getActionMap().put("find", new AbstractAction() {
			@Override
			public void actionPerformed(ActionEvent e) {
				detailSearchPanel.open();
			}
		});
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
			// clear first: selecting the row selected already would not update the details
			overviewTable.clearSelection();
			overviewTable.getSelectionModel().setSelectionInterval(0, 0);
		} else {
			currentPair = null;
			updateDetails();
		}
		// the pairs searched have changed
		detailSearchPanel.refresh();
	}

	private void updateDetails() {
		visibleColumns.clear();
		visibleColumns.addAll(visibleColumnsOf(currentPair));
		detailModel.fireTableDataChanged();
		UIUtil.adjustTableColumnsWidth(detailTable, false);
		detailSearchPanel.refresh();
	}

	/**
	 * Gets the aligned indexes of the columns shown in the column comparison of a pair.
	 */
	private List<Integer> visibleColumnsOf(RowPair pair) {
		List<Integer> result = new ArrayList<Integer>();
		for (int i = 0; i < comparison.columns.size(); ++i) {
			if (onlyDifferences.isSelected() && pair != null) {
				CellStatus s = comparison.cellStatus(pair, i);
				if (s == CellStatus.EQUAL || (s == CellStatus.MISSING && pair.left != null && pair.right != null
						&& comparison.leftIndex(i) >= 0 && comparison.rightIndex(i) >= 0)) {
					continue;
				}
			}
			result.add(i);
		}
		return result;
	}

	/**
	 * Gets the pairs the full text search covers: those of the overview, or the only pair.
	 */
	private List<RowPair> searchablePairs() {
		if (!visiblePairs.isEmpty()) {
			return visiblePairs;
		}
		return currentPair == null? Collections.<RowPair>emptyList() : Collections.singletonList(currentPair);
	}

	/**
	 * Counts the cells of the column comparison of a pair containing the search text.
	 */
	private int numberOfOccurrences(RowPair pair, String searchText) {
		if (occurrencesCache.size() > 32 && !occurrencesCache.containsKey(searchText)) {
			occurrencesCache.clear();
		}
		Map<RowPair, Integer> perPair = occurrencesCache.computeIfAbsent(searchText, t -> new IdentityHashMap<RowPair, Integer>());
		Integer n = perPair.get(pair);
		if (n == null) {
			n = 0;
			for (int column: visibleColumnsOf(pair)) {
				// the values only, not the column name (see isSearchable)
				for (int columnIndex = 1; columnIndex < 3; ++columnIndex) {
					if (detailSearchPanel.matches(searchText, cellText(pair, column, columnIndex))) {
						++n;
					}
				}
			}
			perPair.put(pair, n);
		}
		return n;
	}

	/**
	 * Selects the next (or previous) pair of the overview whose column comparison contains the search text.
	 *
	 * @return <code>true</code> if another pair has been selected
	 */
	private boolean selectPairWithOccurrences(String searchText, boolean forward) {
		int n = visiblePairs.size();
		int current = visiblePairs.indexOf(currentPair);
		for (int k = 1; k < n || (current < 0 && k == n); ++k) {
			int i = current < 0? (forward? k - 1 : n - k) : Math.floorMod(current + (forward? k : -k), n);
			if (numberOfOccurrences(visiblePairs.get(i), searchText) > 0) {
				if (current >= 0 && (forward? i < current : i > current)) {
					// wrapped around, like the search within a table
					Toolkit.getDefaultToolkit().beep();
				}
				overviewTable.getSelectionModel().setSelectionInterval(i, i);
				overviewTable.scrollRectToVisible(overviewTable.getCellRect(i, 0, true));
				return true;
			}
		}
		return false;
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
				if (column == 3 && value != null) {
					// the color of a match of the full text search
					c.setBackground(Colors.Color_190_255_180);
				}
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
			// the rows may be sorted by column name
			int modelRow = row >= 0 && row < table.getRowCount()? table.convertRowIndexToModel(row) : -1;
			if (column > 0 && currentPair != null && modelRow >= 0 && modelRow < visibleColumns.size()) {
				int aligned = visibleColumns.get(modelRow);
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
				if (sideMissing) {
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
					int aligned = modelRow >= 0 && modelRow < visibleColumns.size()? visibleColumns.get(modelRow) : -1;
					c.setForeground(aligned >= 0 && comparison.isPrimaryKey(aligned)? UIUtil.FG_PK
							: aligned >= 0 && comparison.isForeignKey(aligned)? UIUtil.FG_FK : table.getForeground());
				} else {
					c.setBackground(valueBackground(column));
					c.setForeground(table.getForeground());
				}
			}
			c.setFont(font);
			return c instanceof JLabel? detailSearchPanel.markOccurrence((JLabel) c, column, row) : c;
		}
	}

}
