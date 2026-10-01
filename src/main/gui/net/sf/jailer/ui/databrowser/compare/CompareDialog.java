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
import java.awt.GridLayout;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.WeakHashMap;
import java.util.function.Function;

import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.KeyStroke;
import javax.swing.ListSelectionModel;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.JTableHeader;
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

	/**
	 * Creates the SQL that makes the rows of the right side equal to the rows of the left side.
	 */
	public interface SyncHandler {
		/**
		 * Opens the SQL.
		 *
		 * @param owner the owner window
		 * @param comparison the comparison
		 * @param pairs the row pairs
		 * @param refresh compares again, to be called after the SQL has been executed
		 */
		void open(Window owner, RowComparison comparison, List<RowPair> pairs, Runnable refresh);

		/**
		 * Gets the text of the menu item offering this handler.
		 *
		 * @param comparison the comparison the handler gets
		 */
		default String menuText(RowComparison comparison) {
			return "Make \"" + comparison.right.title + "\" equal to \"" + comparison.left.title + "\"";
		}
	}

	// replaced on a refresh
	private RowComparison comparison;
	private List<RowPair> allPairs;
	private final List<Integer> keyColumns;
	private final Function<Window, RowComparison> recompare;
	// one label per side, so that each one has its own tool tip
	private final JLabel leftTitleLabel = new JLabel();
	private final JLabel rightTitleLabel = new JLabel();
	private final JLabel headerMessageLabel = new JLabel();
	private final List<RowPair> visiblePairs = new ArrayList<RowPair>();
	private final List<Integer> visibleColumns = new ArrayList<Integer>();
	private RowPair currentPair;

	private final JCheckBox showEqualRows = new JCheckBox("Show equal rows");
	private final JCheckBox showMissingRows = new JCheckBox("Show rows missing on one side", true);
	private final JCheckBox onlyDifferences = new JCheckBox("Only differences");
	private final JLabel summaryLabel = new JLabel();
	private JButton syncButton;
	private final JButton previousDifferenceButton = new JButton("Previous Difference");
	private final JButton nextDifferenceButton = new JButton("Next Difference");
	private final JButton columnsButton = new JButton("Columns...");
	private static final KeyStroke KS_NEXT_DIFFERENCE = KeyStroke.getKeyStroke(KeyEvent.VK_F7, 0);
	private static final KeyStroke KS_PREVIOUS_DIFFERENCE = KeyStroke.getKeyStroke(KeyEvent.VK_F7, InputEvent.SHIFT_DOWN_MASK);

	/**
	 * Columns excluded from the comparison per table (see {@link CompareWithConnection#ignoredColumnsKey(String)}),
	 * remembered while the application runs.
	 */
	private static final Map<String, Set<String>> IGNORED_COLUMNS = new HashMap<String, Set<String>>();

	/**
	 * Order of the pairs in the overview.
	 */
	private static final List<Status> STATUS_ORDER = Arrays.asList(Status.CHANGED, Status.ONLY_LEFT, Status.ONLY_RIGHT, Status.EQUAL);

	/**
	 * Normalized names of the columns excluded from the comparison (shared by the dialogs of the same table).
	 */
	private final Set<String> ignoredColumns;
	private final boolean withOverview;

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
		this(owner, title, comparison, pairs, keyColumns, recompare, Collections.<SyncHandler>emptyList(), Collections.<SyncHandler>emptyList(), null);
	}

	/**
	 * Opens the dialog, which can be refreshed and offers to synchronize the right side.
	 *
	 * @param owner the owner window
	 * @param title the title
	 * @param comparison the comparison
	 * @param pairs the row pairs to show
	 * @param keyColumns the aligned columns the pairs are matched by, or <code>null</code>
	 * @param recompare compares again (with the same left side) on "Refresh", returns <code>null</code> if cancelled or failed;
	 *                  or <code>null</code> if the comparison can't be refreshed
	 * @param sync creates the SQL that makes the right side equal to the left side, or <code>null</code>
	 * @param reverseSync creates the SQL that makes the left side equal to the right side, or <code>null</code>.
	 *                    It gets the comparison with the sides swapped, so that it too makes the right side equal to the left one.
	 * @param ignoredColumnsKey identifies the table whose columns excluded from the comparison are remembered,
	 *                          or <code>null</code> if they apply to this dialog only
	 */
	public CompareDialog(Window owner, String title, RowComparison comparison, List<RowPair> pairs, List<Integer> keyColumns, Function<Window, RowComparison> recompare,
			SyncHandler sync, SyncHandler reverseSync, String ignoredColumnsKey) {
		this(owner, title, comparison, pairs, keyColumns, recompare,
				sync == null? Collections.<SyncHandler>emptyList() : Collections.singletonList(sync),
				reverseSync == null? Collections.<SyncHandler>emptyList() : Collections.singletonList(reverseSync), ignoredColumnsKey);
	}

	/**
	 * Opens the dialog, which can be refreshed and offers several scripts that make one side equal to the other one.
	 *
	 * @param owner the owner window
	 * @param title the title
	 * @param comparison the comparison
	 * @param pairs the row pairs to show
	 * @param keyColumns the aligned columns the pairs are matched by, or <code>null</code>
	 * @param recompare compares again (with the same left side) on "Refresh", returns <code>null</code> if cancelled or failed;
	 *                  or <code>null</code> if the comparison can't be refreshed
	 * @param syncs create SQL that makes the right side equal to the left side
	 * @param reverseSyncs create SQL that makes the left side equal to the right side.
	 *                    They get the comparison with the sides swapped, so that they too make the right side equal to the left one.
	 * @param ignoredColumnsKey identifies the table whose columns excluded from the comparison are remembered,
	 *                          or <code>null</code> if they apply to this dialog only
	 */
	public CompareDialog(Window owner, String title, RowComparison comparison, List<RowPair> pairs, List<Integer> keyColumns, Function<Window, RowComparison> recompare,
			List<SyncHandler> syncs, List<SyncHandler> reverseSyncs, String ignoredColumnsKey) {
		super(owner, title, ModalityType.MODELESS);
		this.comparison = comparison;
		this.keyColumns = keyColumns;
		this.recompare = keyColumns == null? null : recompare;
		this.ignoredColumns = ignoredColumnsKey == null? new HashSet<String>() : IGNORED_COLUMNS.computeIfAbsent(ignoredColumnsKey, k -> new HashSet<String>());
		applyIgnoredColumns(comparison);
		this.allPairs = rePair(pairs);
		this.overviewTable = new JTable(overviewModel);
		this.detailTable = new JTable(detailModel);
		// the titles of the sides are often longer than the columns are wide
		detailTable.setTableHeader(new JTableHeader(detailTable.getColumnModel()) {
			@Override
			public String getToolTipText(MouseEvent e) {
				int column = columnAtPoint(e.getPoint());
				int modelColumn = column < 0? -1 : detailTable.convertColumnIndexToModel(column);
				if (modelColumn == 1 || modelColumn == 2) {
					RowComparison.Side side = modelColumn == 1? CompareDialog.this.comparison.left : CompareDialog.this.comparison.right;
					return side.getToolTip() != null? side.getToolTip() : UIUtil.toHTML(side.title, 100);
				}
				return null;
			}
		});
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
		JPanel headerPanel = new JPanel(new BorderLayout());
		JPanel titlePanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
		titlePanel.add(leftTitleLabel);
		titlePanel.add(new JLabel("  vs  "));
		titlePanel.add(rightTitleLabel);
		titlePanel.setBorder(BorderFactory.createEmptyBorder(0, 0, 6, 0));
		headerPanel.add(titlePanel, BorderLayout.NORTH);
		headerPanel.add(headerMessageLabel, BorderLayout.CENTER);
		content.add(headerPanel, BorderLayout.NORTH);

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
		detailTable.addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent e) {
				if (e.isPopupTrigger()) {
					showDetailPopup(e);
				}
			}
			@Override
			public void mouseReleased(MouseEvent e) {
				if (e.isPopupTrigger()) {
					showDetailPopup(e);
				}
			}
		});
		detailTable.getColumnModel().getColumn(0).setPreferredWidth(180);
		detailTable.getColumnModel().getColumn(1).setPreferredWidth(300);
		detailTable.getColumnModel().getColumn(2).setPreferredWidth(300);

		JPanel detailPanel = new JPanel(new BorderLayout());
		detailPanel.add(new JScrollPane(detailTable), BorderLayout.CENTER);
		// directly below the column comparison it applies to, flush with its left edge
		JPanel onlyDifferencesPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
		onlyDifferencesPanel.setBorder(BorderFactory.createEmptyBorder(4, 0, 0, 0));
		onlyDifferencesPanel.add(onlyDifferences);
		onlyDifferencesPanel.add(Box.createHorizontalStrut(16));
		previousDifferenceButton.setIcon(UIUtil.scaleIcon(previousDifferenceButton, UIUtil.readImage("/prev.png")));
		previousDifferenceButton.setToolTipText("Select the previous different value (Shift+F7).");
		previousDifferenceButton.addActionListener(e -> selectDifference(false));
		onlyDifferencesPanel.add(previousDifferenceButton);
		onlyDifferencesPanel.add(Box.createHorizontalStrut(4));
		nextDifferenceButton.setIcon(UIUtil.scaleIcon(nextDifferenceButton, UIUtil.readImage("/next.png")));
		nextDifferenceButton.setToolTipText("Select the next different value (F7).");
		nextDifferenceButton.addActionListener(e -> selectDifference(true));
		onlyDifferencesPanel.add(nextDifferenceButton);
		onlyDifferencesPanel.add(Box.createHorizontalStrut(16));
		columnsButton.setIcon(UIUtil.scaleIcon(columnsButton, UIUtil.readImage("/comparecolumns.png")));
		columnsButton.setToolTipText("Choose the columns to compare, e.g. to exclude audit columns that always differ. Also possible by right-clicking a column.");
		columnsButton.addActionListener(e -> openColumnsDialog());
		onlyDifferencesPanel.add(columnsButton);
		detailPanel.add(onlyDifferencesPanel, BorderLayout.SOUTH);
		// a refresh can add rows
		withOverview = allPairs.size() != 1 || this.recompare != null;
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
		if (!syncs.isEmpty() || !reverseSyncs.isEmpty()) {
			syncButton = new JButton("Synchronize...");
			syncButton.setIcon(UIUtil.scaleIcon(syncButton, UIUtil.readImage("/sync.png")));
			Runnable afterExecution = this.recompare != null? this::refresh : () -> {};
			if (syncs.size() + reverseSyncs.size() > 1) {
				syncButton.setToolTipText("SQL script that makes one side equal to the other one.");
				syncButton.addActionListener(e -> {
					JPopupMenu popup = new JPopupMenu();
					for (SyncHandler sync: syncs) {
						JMenuItem item = new JMenuItem(sync.menuText(this.comparison));
						item.addActionListener(ev -> sync.open(this, this.comparison, allPairs, afterExecution));
						popup.add(item);
					}
					RowComparison swapped = swappedComparison();
					for (SyncHandler reverseSync: reverseSyncs) {
						JMenuItem item = new JMenuItem(reverseSync.menuText(swapped));
						item.addActionListener(ev -> {
							List<RowPair> swappedPairs = new ArrayList<RowPair>();
							for (RowPair p: allPairs) {
								swappedPairs.add(swapped.pair(p.key, p.right, p.left));
							}
							reverseSync.open(this, swapped, swappedPairs, afterExecution);
						});
						popup.add(item);
					}
					UIUtil.showPopup(syncButton, 0, syncButton.getHeight(), popup);
				});
			} else if (!syncs.isEmpty()) {
				SyncHandler sync = syncs.get(0);
				syncButton.setToolTipText("SQL script that makes the rows in " + comparison.right.title + " equal to the rows of " + comparison.left.title + ".");
				syncButton.addActionListener(e -> sync.open(this, this.comparison, allPairs, afterExecution));
			} else {
				SyncHandler reverseSync = reverseSyncs.get(0);
				syncButton.setToolTipText("SQL script that makes the rows in " + comparison.left.title + " equal to the rows of " + comparison.right.title + ".");
				syncButton.addActionListener(e -> {
					RowComparison swapped = swappedComparison();
					List<RowPair> swappedPairs = new ArrayList<RowPair>();
					for (RowPair p: allPairs) {
						swappedPairs.add(swapped.pair(p.key, p.right, p.left));
					}
					reverseSync.open(this, swapped, swappedPairs, afterExecution);
				});
			}
			syncButton.setEnabled(allPairs.stream().anyMatch(p -> p.status != Status.EQUAL));
			rightButtons.add(syncButton);
		}
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

		boolean anyDifference = allPairs.stream().anyMatch(p -> p.status != Status.EQUAL);
		showEqualRows.setSelected(!anyDifference);
		onlyDifferences.setSelected(false);

		setContentPane(content);
		UIUtil.initComponents(this);
		if (withOverview) {
			updateOverview();
		} else {
			currentPair = allPairs.isEmpty()? null : allPairs.get(0);
			updateDetails();
			updateDifferenceButtons();
		}
		java.awt.geom.Rectangle2D screen = UIUtil.getScreenBounds();
		setSize(new Dimension((int) Math.min(1200, screen.getWidth() * 0.8), (int) Math.min(withOverview? 880 : 720, screen.getHeight() * 0.8)));
		UIUtil.setInitialWindowLocation(this, owner, 100, 100);
		UIUtil.fit(this);
		setVisible(true);
	}

	/**
	 * Shows the titles of the sides, whether there is no difference and whether rows have been cut by the row limit.
	 */
	private void updateHeader() {
		leftTitleLabel.setText("<html><b>" + UIUtil.toHTMLFragment(comparison.left.title, 0) + "</b></html>");
		leftTitleLabel.setToolTipText(comparison.left.getToolTip());
		rightTitleLabel.setText("<html><b>" + UIUtil.toHTMLFragment(comparison.right.title, 0) + "</b></html>");
		rightTitleLabel.setToolTipText(comparison.right.getToolTip());
		List<String> messages = new ArrayList<String>();
		if (!allPairs.isEmpty() && allPairs.stream().allMatch(p -> p.status == Status.EQUAL)) {
			messages.add("<font color=" + Colors.HTMLColor_008000 + "><b>No differences:</b> "
					+ (allPairs.size() == 1? "the rows are equal." : "all " + allPairs.size() + " rows are equal.") + "</font>");
		}
		if (comparison.left.truncated || comparison.right.truncated) {
			messages.add("<font color=" + Colors.HTMLColor_dd0000 + ">The rows of "
					+ (comparison.left.truncated && comparison.right.truncated? "both sides have" : comparison.left.truncated? "the left side have" : "the right side have")
					+ " been cut by the row limit. Rows reported as missing may just not have been loaded.</font>");
		}
		headerMessageLabel.setText(messages.isEmpty()? "" : "<html>" + String.join("<br>", messages) + "</html>");
		int ignored = comparison.ignoredColumnNames().size();
		columnsButton.setText(ignored == 0? "Columns..." : "Columns (" + ignored + " ignored)...");
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
		applyIgnoredColumns(newComparison);
		allPairs = newComparison.matchByKey(keyColumns);
		occurrencesCache.clear();
		updateHeader();
		updateOverview();
		selectPair(selectedKey);
	}

	/**
	 * Selects the pair with a given key in the overview, if it's there.
	 *
	 * @param key the key, or <code>null</code>
	 */
	private void selectPair(String key) {
		if (key != null) {
			for (int i = 0; i < visiblePairs.size(); ++i) {
				if (key.equals(visiblePairs.get(i).key)) {
					overviewTable.getSelectionModel().setSelectionInterval(i, i);
					overviewTable.scrollRectToVisible(overviewTable.getCellRect(i, 0, true));
					break;
				}
			}
		}
	}

	/**
	 * Whether an aligned column can be excluded from the comparison: it's present on both sides and no key column.
	 */
	private boolean canBeIgnored(RowComparison c, int column) {
		return c.leftIndex(column) >= 0 && c.rightIndex(column) >= 0 && !c.isPrimaryKeyOfBoth(column)
				&& (keyColumns == null || !keyColumns.contains(column));
	}

	/**
	 * Excludes the columns to be ignored from a comparison (except key columns).
	 */
	private void applyIgnoredColumns(RowComparison c) {
		Set<String> names = new HashSet<String>();
		for (int i = 0; i < c.columns.size(); ++i) {
			String name = RowComparison.normalizeName(c.columns.get(i));
			if (ignoredColumns.contains(name) && canBeIgnored(c, i)) {
				names.add(name);
			}
		}
		c.setIgnoredColumns(names);
	}

	/**
	 * Creates the pairs again with the current comparison, whose excluded columns may have changed.
	 */
	private List<RowPair> rePair(List<RowPair> pairs) {
		List<RowPair> result = new ArrayList<RowPair>();
		for (RowPair pair: pairs) {
			result.add(comparison.rePair(pair));
		}
		return result;
	}

	/**
	 * Gets the comparison with the sides swapped (for the reverse synchronization), with the same columns excluded.
	 */
	private RowComparison swappedComparison() {
		RowComparison swapped = new RowComparison(comparison.right, comparison.left).withIgnoreTrailingBlanks(comparison.isIgnoreTrailingBlanks());
		Set<String> names = new HashSet<String>();
		for (String name: comparison.ignoredColumnNames()) {
			names.add(RowComparison.normalizeName(name));
		}
		swapped.setIgnoredColumns(names);
		return swapped;
	}

	/**
	 * Includes a column in the comparison again, or excludes it.
	 *
	 * @param column the aligned column, or -1 to include all columns again
	 * @param ignore whether to exclude it
	 */
	private void setIgnored(int column, boolean ignore) {
		if (column < 0) {
			ignoredColumns.clear();
		} else if (ignore) {
			ignoredColumns.add(RowComparison.normalizeName(comparison.columns.get(column)));
		} else {
			ignoredColumns.remove(RowComparison.normalizeName(comparison.columns.get(column)));
		}
		ignoredColumnsChanged();
	}

	/**
	 * Lets the user choose the columns to compare.
	 */
	private void openColumnsDialog() {
		Map<Integer, JCheckBox> checkBoxes = new LinkedHashMap<Integer, JCheckBox>();
		JPanel list = new JPanel(new GridLayout(0, 1));
		for (int i = 0; i < comparison.columns.size(); ++i) {
			if (comparison.leftIndex(i) < 0 || comparison.rightIndex(i) < 0) {
				// not compared anyway
				continue;
			}
			boolean canBeIgnored = canBeIgnored(comparison, i);
			JCheckBox checkBox = new JCheckBox(comparison.columns.get(i) + (canBeIgnored? "" : " (key, always compared)"), !comparison.isIgnored(i));
			checkBox.setEnabled(canBeIgnored);
			checkBoxes.put(i, checkBox);
			list.add(checkBox);
		}
		if (checkBoxes.isEmpty()) {
			return;
		}
		JPanel panel = new JPanel(new BorderLayout(0, 6));
		panel.add(new JLabel("<html>Compared columns:</html>"), BorderLayout.NORTH);
		JScrollPane scrollPane = new JScrollPane(list);
		scrollPane.getVerticalScrollBar().setUnitIncrement(Math.max(16, checkBoxes.values().iterator().next().getPreferredSize().height));
		scrollPane.setPreferredSize(new Dimension(360, Math.min(400, 28 * checkBoxes.size() + 8)));
		panel.add(scrollPane, BorderLayout.CENTER);
		if (JOptionPane.showConfirmDialog(this, panel, "Compared Columns", JOptionPane.OK_CANCEL_OPTION, JOptionPane.PLAIN_MESSAGE) != JOptionPane.OK_OPTION) {
			return;
		}
		for (Map.Entry<Integer, JCheckBox> e: checkBoxes.entrySet()) {
			if (e.getValue().isEnabled()) {
				String name = RowComparison.normalizeName(comparison.columns.get(e.getKey()));
				if (e.getValue().isSelected()) {
					ignoredColumns.remove(name);
				} else {
					ignoredColumns.add(name);
				}
			}
		}
		ignoredColumnsChanged();
	}

	/**
	 * Compares again after the columns to be ignored have changed.
	 */
	private void ignoredColumnsChanged() {
		String selectedKey = currentPair == null? null : currentPair.key;
		int selectedDetailRow = detailTable.getSelectedRow();
		applyIgnoredColumns(comparison);
		allPairs = rePair(allPairs);
		occurrencesCache.clear();
		updateHeader();
		if (withOverview) {
			updateOverview();
			selectPair(selectedKey);
		} else {
			currentPair = allPairs.isEmpty()? null : allPairs.get(0);
			updateDetails();
			updateDifferenceButtons();
			if (syncButton != null) {
				syncButton.setEnabled(allPairs.stream().anyMatch(p -> p.status != Status.EQUAL));
			}
		}
		// keeps the row of the column if it's still shown (not with "Only differences")
		if (selectedDetailRow >= 0 && selectedDetailRow < detailTable.getRowCount() && !onlyDifferences.isSelected()) {
			detailTable.getSelectionModel().setSelectionInterval(selectedDetailRow, selectedDetailRow);
		}
	}

	/**
	 * Context menu of the column comparison: excludes a column from the comparison, or includes it again.
	 */
	private void showDetailPopup(MouseEvent e) {
		int row = detailTable.rowAtPoint(e.getPoint());
		if (row < 0) {
			return;
		}
		detailTable.getSelectionModel().setSelectionInterval(row, row);
		int modelRow = detailTable.convertRowIndexToModel(row);
		if (modelRow < 0 || modelRow >= visibleColumns.size()) {
			return;
		}
		int column = visibleColumns.get(modelRow);
		String name = comparison.columns.get(column);
		JPopupMenu popup = new JPopupMenu();
		if (comparison.isIgnored(column)) {
			JMenuItem item = new JMenuItem("Compare Column \"" + name + "\"");
			item.setToolTipText("Includes the column in the comparison again.");
			item.addActionListener(ev -> setIgnored(column, false));
			popup.add(item);
		} else {
			JMenuItem item = new JMenuItem("Ignore Column \"" + name + "\"");
			boolean canBeIgnored = canBeIgnored(comparison, column);
			item.setEnabled(canBeIgnored);
			item.setToolTipText(canBeIgnored
					? "Excludes the column from the comparison and from the synchronization (no Update of it). "
						+ "Remembered for further comparisons of the table until the application is closed."
					: "Only a column present on both sides and not part of the key can be ignored.");
			item.addActionListener(ev -> setIgnored(column, true));
			popup.add(item);
		}
		if (!ignoredColumns.isEmpty()) {
			JMenuItem all = new JMenuItem("Compare All Columns");
			all.setToolTipText("Includes all ignored columns in the comparison again.");
			all.addActionListener(ev -> setIgnored(-1, false));
			popup.add(all);
		}
		UIUtil.showPopup(detailTable, e.getX(), e.getY(), popup);
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
		getRootPane().getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KS_NEXT_DIFFERENCE, "nextDifference");
		getRootPane().getActionMap().put("nextDifference", new AbstractAction() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (nextDifferenceButton.isEnabled()) {
					selectDifference(true);
				}
			}
		});
		getRootPane().getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KS_PREVIOUS_DIFFERENCE, "previousDifference");
		getRootPane().getActionMap().put("previousDifference", new AbstractAction() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (previousDifferenceButton.isEnabled()) {
					selectDifference(false);
				}
			}
		});
	}

	/**
	 * Enables the navigation between the differences if there are any.
	 */
	private void updateDifferenceButtons() {
		boolean anyDifference = allPairs.stream().anyMatch(p -> p.status != Status.EQUAL);
		previousDifferenceButton.setEnabled(anyDifference);
		nextDifferenceButton.setEnabled(anyDifference);
	}

	/**
	 * Whether a value of a pair differs: changed, or its column is missing on one side.
	 */
	private boolean isDifference(RowPair pair, int column) {
		CellStatus s = comparison.cellStatus(pair, column);
		return s == CellStatus.CHANGED || (s == CellStatus.MISSING && (comparison.leftIndex(column) < 0 || comparison.rightIndex(column) < 0));
	}

	/**
	 * Gets the rows (view indexes) of the column comparison of the pair shown that are differences.
	 * A pair missing on one side is one difference, its first row.
	 */
	private List<Integer> differenceRows() {
		List<Integer> result = new ArrayList<Integer>();
		if (currentPair == null || currentPair.status == Status.EQUAL || detailTable.getRowCount() == 0) {
			return result;
		}
		if (currentPair.status != Status.CHANGED) {
			result.add(0);
			return result;
		}
		for (int row = 0; row < detailTable.getRowCount(); ++row) {
			int modelRow = detailTable.convertRowIndexToModel(row);
			if (modelRow >= 0 && modelRow < visibleColumns.size() && isDifference(currentPair, visibleColumns.get(modelRow))) {
				result.add(row);
			}
		}
		return result;
	}

	/**
	 * Selects the next (or previous) different value: in the pair shown, else in the next (or previous) pair
	 * of the overview that is not equal. Wraps around with a beep, like the full text search.
	 */
	private void selectDifference(boolean forward) {
		List<Integer> rows = differenceRows();
		int selected = detailTable.getSelectedRow();
		if (selected < 0 && !forward) {
			selected = detailTable.getRowCount();
		}
		for (int k = 0; k < rows.size(); ++k) {
			int row = rows.get(forward? k : rows.size() - 1 - k);
			if (forward? row > selected : row < selected) {
				selectDetailRow(row);
				return;
			}
		}
		int n = visiblePairs.size();
		int current = visiblePairs.indexOf(currentPair);
		for (int k = 1; k <= n; ++k) {
			int i = current < 0? (forward? k - 1 : n - k) : Math.floorMod(current + (forward? k : -k), n);
			if (visiblePairs.get(i).status != Status.EQUAL) {
				if (current >= 0 && (forward? i <= current : i >= current)) {
					Toolkit.getDefaultToolkit().beep();
				}
				// updates the column comparison
				overviewTable.getSelectionModel().setSelectionInterval(i, i);
				overviewTable.scrollRectToVisible(overviewTable.getCellRect(i, 0, true));
				List<Integer> pairRows = differenceRows();
				if (!pairRows.isEmpty()) {
					selectDetailRow(pairRows.get(forward? 0 : pairRows.size() - 1));
				}
				return;
			}
		}
		// the only pair (no overview)
		if (!rows.isEmpty()) {
			Toolkit.getDefaultToolkit().beep();
			selectDetailRow(rows.get(forward? 0 : rows.size() - 1));
		}
	}

	private void selectDetailRow(int row) {
		detailTable.getSelectionModel().setSelectionInterval(row, row);
		detailTable.scrollRectToVisible(detailTable.getCellRect(row, 0, true));
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
		// differences first; the order within a status (that of the rows) is kept
		visiblePairs.sort(Comparator.comparingInt(p -> STATUS_ORDER.indexOf(p.status)));
		summaryLabel.setText(equal + " equal, " + changed + " different, " + missing + " missing on one side  ");
		updateDifferenceButtons();
		if (syncButton != null) {
			syncButton.setEnabled(changed + missing > 0);
		}
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
				if (s == CellStatus.EQUAL || comparison.isIgnored(i) || (s == CellStatus.MISSING && pair.left != null && pair.right != null
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
		List<String> ignored = comparison.ignoredColumnNames();
		if (!ignored.isEmpty()) {
			sb.append("Not compared: ").append(String.join(", ", ignored)).append("\n");
		}
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
		private final Map<String, String> tipCache = new WeakHashMap<String, String>();

		/**
		 * Gets the tool tip of a value cell, like the one of a table browser: the value if it's long or has several lines.
		 *
		 * @param text the value's text, or <code>null</code> for none (null value, no row, no column)
		 * @param note why the value is not compared (a LOB, an ignored column), or <code>null</code>
		 */
		private String toolTip(String text, String note) {
			String html = null;
			if (text != null && (text.length() > 400 || text.indexOf('\n') >= 0 || text.indexOf((char) 182) >= 0)) {
				html = tipCache.computeIfAbsent(text, t -> UIUtil.toHTMLFragment(BrowserContentPane.hardWrap(t.replace((char) 182, '\n')), 200));
			} else if (text != null && text.length() > 10) {
				if (note == null) {
					return text;
				}
				html = UIUtil.toHTMLFragment(text, 0);
			}
			if (html == null) {
				return note;
			}
			return "<html>" + html + (note != null? "<hr>" + note : "") + "</html>";
		}

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
				boolean isNull = (isLeft? comparison.leftValue(currentPair, aligned) : comparison.rightValue(currentPair, aligned)) == null;
				if (c instanceof JComponent) {
					String note = s != CellStatus.NOT_COMPARED? null
							: comparison.isIgnored(aligned)? "The column is not compared (right-click to compare it again)." : "LOB content is not compared.";
					((JComponent) c).setToolTipText(toolTip(sideMissing || isNull || value == null? null : value.toString(), note));
				}
				if (s == CellStatus.NOT_COMPARED && !isSelected) {
					c.setForeground(Colors.Color_128_128_128);
				}
				// leading and trailing blanks are made visible like in the result tables (not the padding of a CHAR column)
				Object cellValue = isLeft? comparison.leftValue(currentPair, aligned) : comparison.rightValue(currentPair, aligned);
				if (!sideMissing && cellValue instanceof String && value != null && c instanceof JLabel) {
					RowComparison.Side side = isLeft? comparison.left : comparison.right;
					int sideIndex = isLeft? comparison.leftIndex(aligned) : comparison.rightIndex(aligned);
					((JLabel) c).setText(UIUtil.indicateLeadingAndTrailingSpaces(value.toString(), side.isCharColumn(sideIndex)));
				}
			} else if (!isSelected) {
				if (column == 0) {
					// the name column looks like the one of the single row view (ColumnsTable)
					c.setBackground(row % 2 == 0? UIUtil.TABLE_BACKGROUND_COLOR_1_INCLOSURE : UIUtil.TABLE_BACKGROUND_COLOR_2_INCLOSURE);
					int aligned = modelRow >= 0 && modelRow < visibleColumns.size()? visibleColumns.get(modelRow) : -1;
					c.setForeground(aligned >= 0 && comparison.isPrimaryKey(aligned)? UIUtil.FG_PK
							: aligned >= 0 && comparison.isForeignKey(aligned)? UIUtil.FG_FK : table.getForeground());
					if (aligned >= 0 && comparison.isIgnored(aligned)) {
						c.setForeground(Colors.Color_128_128_128);
						font = font.deriveFont(Font.ITALIC);
						if (c instanceof JComponent) {
							((JComponent) c).setToolTipText("Not compared (right-click to compare it again).");
						}
					}
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
