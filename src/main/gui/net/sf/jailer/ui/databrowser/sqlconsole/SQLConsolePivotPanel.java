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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dialog;
import java.awt.Window;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.awt.datatransfer.StringSelection;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.UncheckedIOException;
import java.io.Writer;
import java.math.MathContext;
import java.nio.charset.StandardCharsets;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Locale;
import java.util.List;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.IntPredicate;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.RowSorter;
import javax.swing.RowSorter.SortKey;
import javax.swing.Scrollable;
import javax.swing.SortOrder;
import javax.swing.SwingConstants;
import javax.swing.UIManager;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.TableColumnModelEvent;
import javax.swing.event.TableColumnModelListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.database.Session;
import net.sf.jailer.database.Session.AbstractResultSetReader;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.UIUtil.PLAF;
import net.sf.jailer.ui.databrowser.BrowserContentPane;
import net.sf.jailer.ui.databrowser.Row;
import net.sf.jailer.ui.databrowser.SQLDMLPanel;
import net.sf.jailer.ui.util.ConcurrentTaskControl;
import net.sf.jailer.ui.util.SimpleXlsxWriter;
import net.sf.jailer.util.CancellationHandler;
import net.sf.jailer.util.CellContentConverter;
import net.sf.jailer.util.CellContentConverter.PObjectWrapper;
import net.sf.jailer.util.Quoting;

/**
 * "Pivot" tab of the SQL Console result: a cross table of the loaded rows
 * with one or more (nested) row fields, optional (nested) column fields and one aggregated value field.
 *
 * @author Ralf Wisser
 */
public class SQLConsolePivotPanel extends JPanel {

	private static final String ROWS = "(rows)";
	private static final String COUNT = "Count";
	private static final String COUNT_DISTINCT = "Count distinct";
	private static final String SUM = "Sum";
	private static final String AVG = "Avg";
	private static final String MIN = "Min";
	private static final String MAX = "Max";
	private static final String[] ALL_AGGREGATES = { COUNT, COUNT_DISTINCT, SUM, AVG, MIN, MAX };
	private static final List<String> ROWS_AGGREGATES = Collections.singletonList(COUNT);
	private static final int MAX_COLUMN_KEYS = 500;
	private static final String COMBO_PROTOTYPE = "XXXXXXXXXXXX";
	private static final String MIDNIGHT = " 00:00:00.0";
	private static final Pattern BOLD_PATTERN = Pattern.compile("<b>(.*?)</b>");
	private static final Pattern TABLE_PATTERN = Pattern.compile("^<html><nobr><font[^>]*>(.*?)</font><br><b>");

	/**
	 * Key of the group of null values.
	 */
	private static final Object NULL_KEY = new Object() {
		@Override
		public String toString() {
			return "(null)";
		}
	};

	/**
	 * Value counted if the value field is {@link #ROWS}.
	 */
	private static final Object ROW_MARKER = new Object();

	private final List<Integer> columnTypes;
	private JTable currentTable;
	private boolean suppressUpdate = false;

	private List<String> pendingRows;
	private List<String> pendingColumns;
	private List<String> pendingValues;
	private List<String> pendingAggregates;
	private List<String> pendingShowAs;

	/**
	 * Names of the columns of the current table.
	 */
	private List<String> columnNames = new ArrayList<>();

	private final FieldListPanel rowsFieldList = new FieldListPanel("(add field)", "row field");
	private final FieldListPanel columnsFieldList = new FieldListPanel("(add field)", "column field");
	private final ValueListPanel valuesList = new ValueListPanel(this::isNumericField);
	private final JCheckBox singleElementTotalsCheckBox = new JCheckBox("Totals of single-element groups");
	private final JCheckBox heatmapCheckBox = new JCheckBox("Heatmap");
	private final JLabel infoLabel = new JLabel();
	private final JPanel centerPanel = new JPanel(new BorderLayout());
	private final JTable resultTable = new JTable();
	private final JSplitPane splitPane;
	private static ImageIcon deleteIcon;

	/**
	 * Keys and model columns of the pivot table currently shown (for the drilldown).
	 */
	private List<PivotLine> shownLines = null;
	private List<PivotColumn> shownColumns = null;
	private int[] shownRowsCols = null;
	private int[] shownColumnsCols = null;

	private Runnable showRowsTabAction;

	/**
	 * Header of the column the groups are sorted by (<code>null</code>: by key), and the column index of it in the pivot table shown (or -1).
	 */
	private String sortColumn = null;
	private boolean sortDescending = true;
	private int shownSortIndex = -1;

	/**
	 * Aggregation, values and headers of the pivot table currently shown (for the SQL generation).
	 */
	private Aggregation shownAggregation = null;
	private Map<Integer, Set<Object>> shownExcluded = new HashMap<>();
	private boolean shownBasedOnAllRows = false;
	private LineOrder shownLineOrder = null;
	private List<String> shownColumnFields = null;
	private List<String> shownValueNames = null;

	/**
	 * Filters of the fields: name of the field to the keys of the rows filtered out.
	 * Kept also for fields not selected (they take effect again when the field is selected again).
	 */
	private final Map<String, Set<Object>> fieldFilters = new HashMap<>();
	private List<ValueSpec> shownValues = null;
	private int[] shownValueCols = null;
	private List<String> shownHeaders = null;

	/**
	 * To generate the SQL of the pivot table (see {@link #setSqlSource(List, SQLConsole, ExecutionContext)}).
	 */
	private List<String> resultColumnLabels;
	private SQLConsole sqlConsole;
	private ExecutionContext executionContext;
	private final JButton sqlButton = new JButton("SQL...");
	private final JButton collapseAllButton = new JButton("Collapse all");
	private final JButton expandAllButton = new JButton("Expand all");
	private final JButton copyButton = new JButton("Copy");
	private final JButton saveCsvButton = new JButton("Save as CSV...");
	private final JButton saveXlsxButton = new JButton("Save as Excel...");

	/**
	 * The result whose rows are aggregated.
	 */
	private final BrowserContentPane rowBrowser;

	/**
	 * To execute the statement again if the row limit is exceeded (see {@link #setAllRowsSource(Supplier, Session, Consumer)}).
	 */
	private Supplier<String> allRowsStatement;
	private Session allRowsSession;
	private Consumer<Runnable> allRowsExecutor;

	/**
	 * Aggregation of all rows, and the key of the settings and rows it belongs to.
	 */
	private String allRowsKey;
	private Aggregation allRowsAggregation;
	private TooManyColumnKeysException allRowsTooManyColumnKeys;

	public SQLConsolePivotPanel(List<Integer> columnTypes, BrowserContentPane rowBrowser) {
		this.columnTypes = columnTypes;
		this.rowBrowser = rowBrowser;
		setLayout(new BorderLayout());
		JScrollPane configScrollPane = new JScrollPane(buildConfigPanel(), JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
		configScrollPane.setBorder(null);
		configScrollPane.getVerticalScrollBar().setUnitIncrement(16);
		splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, configScrollPane, centerPanel);
		splitPane.setResizeWeight(0);
		splitPane.setContinuousLayout(true);
		splitPane.setBorder(null);
		splitPane.setDividerLocation(Math.max(340, configScrollPane.getPreferredSize().width));
		add(splitPane, BorderLayout.CENTER);

		resultTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
		resultTable.setCellSelectionEnabled(true);
		resultTable.getTableHeader().setReorderingAllowed(false);
		TableCellRenderer defaultHeaderRenderer = resultTable.getTableHeader().getDefaultRenderer();
		resultTable.getTableHeader().setDefaultRenderer((table, value, isSelected, hasFocus, row, column) -> {
			Component render = defaultHeaderRenderer.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
			if (render instanceof JLabel) {
				((JLabel) render).setIcon(column >= 0 && column == shownSortIndex ? UIManager.getIcon(sortDescending ? "Table.descendingSortIcon" : "Table.ascendingSortIcon") : null);
				((JLabel) render).setHorizontalTextPosition(SwingConstants.LEADING);
			}
			return render;
		});
		resultTable.getTableHeader().setToolTipText("Click a value column to sort the groups by it (descending, ascending, by key)");
		resultTable.getTableHeader().addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				int column = resultTable.getTableHeader().columnAtPoint(e.getPoint());
				if (!SwingUtilities.isLeftMouseButton(e) || column < 0 || shownHeaders == null || shownRowsCols == null || column >= shownHeaders.size()) {
					return;
				}
				if (column < shownRowsCols.length) {
					sortColumn = null; // by key
				} else {
					String header = shownHeaders.get(column);
					if (!header.equals(sortColumn)) {
						sortColumn = header;
						sortDescending = true;
					} else if (sortDescending) {
						sortDescending = false;
					} else {
						sortColumn = null;
					}
				}
				saveConfig();
				updatePivot();
			}
		});
		resultTable.setDefaultRenderer(Object.class, new PivotCellRenderer());
		resultTable.setToolTipText("Click a group label to collapse or expand the group. Double-click a cell to select its rows in the table of the rows");
		resultTable.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				if (!SwingUtilities.isLeftMouseButton(e)) {
					return;
				}
				int r = resultTable.rowAtPoint(e.getPoint());
				int c = resultTable.columnAtPoint(e.getPoint());
				if (r < 0 || c < 0) {
					return;
				}
				boolean groupLabel = resultTable.getModel() instanceof PivotTableModel
						&& ((PivotTableModel) resultTable.getModel()).groupState(r, resultTable.convertColumnIndexToModel(c)) != 0;
				if (groupLabel) {
					if (e.getClickCount() == 1) {
						toggleGroup(r, resultTable.convertColumnIndexToModel(c));
					}
				} else if (e.getClickCount() == 2) {
					drillDown(r, c);
				}
			}
		});
		showMessage("Select fields on the left to create a pivot table.");

		Runnable changeAction = () -> {
			saveConfig();
			updatePivot();
		};
		rowsFieldList.setChangeAction(changeAction);
		columnsFieldList.setChangeAction(changeAction);
		rowsFieldList.setFilterHandler(this::isFieldFiltered, this::editFilter);
		columnsFieldList.setFilterHandler(this::isFieldFiltered, this::editFilter);
		valuesList.setChangeAction(changeAction);
		singleElementTotalsCheckBox.addActionListener(e -> changeAction.run());
		splitPane.addPropertyChangeListener(JSplitPane.DIVIDER_LOCATION_PROPERTY, e -> {
			if (isShowing()) {
				saveConfig();
			}
		});
	}

	/**
	 * Builds the side bar with the pivot configuration.
	 */
	private JPanel buildConfigPanel() {
		JPanel panel = new ConfigPanel();
		int y = 0;
		y = addSection(panel, y, "Rows", rowsFieldList, "Columns whose values become the rows of the pivot table (nested in this order)");
		y = addSection(panel, y, "Values", valuesList, "Columns to aggregate, each with its aggregate function. \"" + ROWS + "\" counts the rows.");
		y = addSection(panel, y, "Columns", columnsFieldList, "Columns whose values become the columns of the pivot table (nested in this order)");

		singleElementTotalsCheckBox.setToolTipText("Show the subtotal of a group even if it has only one subgroup (the subtotal then repeats its values)");
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = y++;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(12, 2, 0, 4);
		panel.add(singleElementTotalsCheckBox, gbc);

		heatmapCheckBox.setToolTipText("Colors the cells by their value (separately for each value, without the subtotals and totals)");
		heatmapCheckBox.addActionListener(e -> {
			saveConfig();
			resultTable.repaint();
		});
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = y++;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(0, 2, 0, 4);
		panel.add(heatmapCheckBox, gbc);

		sqlButton.setIcon(UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/procedure_32.png"))); // like "Create SQL"
		sqlButton.addActionListener(e -> showSql());
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = y++;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(8, 6, 0, 4);
		panel.add(sqlButton, gbc);

		JPanel exportPanel = new JPanel(new GridLayout(1, 2, 4, 0));
		exportPanel.add(copyButton);
		exportPanel.add(saveCsvButton);
		// like "Create SQL"
		copyButton.setIcon(UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/copy.png")));
		saveCsvButton.setIcon(UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/save.png")));
		copyButton.setToolTipText("Copy the pivot table (with header) to the clipboard, e.g. to paste it into a spreadsheet");
		saveCsvButton.setToolTipText("Save the pivot table (with header) as CSV file");
		copyButton.addActionListener(e -> copyToClipboard());
		saveCsvButton.addActionListener(e -> saveAsCsv());
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = y++;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(4, 6, 0, 4);
		panel.add(exportPanel, gbc);

		saveXlsxButton.setIcon(UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/save.png")));
		saveXlsxButton.setToolTipText("<html>Save the pivot table as Excel file (.xlsx, can also be opened with LibreOffice).<br>"
				+ "The sheet \"Data\" contains the rows, and the cells of the pivot table are formulas over them.</html>");
		saveXlsxButton.addActionListener(e -> saveAsXlsx());
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = y++;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(4, 6, 0, 4);
		panel.add(saveXlsxButton, gbc);

		JPanel collapsePanel = new JPanel(new GridLayout(1, 2, 4, 0));
		collapsePanel.add(collapseAllButton);
		collapsePanel.add(expandAllButton);
		collapseAllButton.setToolTipText("Collapse all row groups (only their subtotals are shown)");
		expandAllButton.setToolTipText("Expand all row groups");
		// like "Collapse all"/"Expand all" in the extraction model editor
		collapseAllButton.setIcon(UIUtil.scaleIcon(collapseAllButton, UIUtil.readImage("/minus.png")));
		expandAllButton.setIcon(UIUtil.scaleIcon(expandAllButton, UIUtil.readImage("/collapsed.png")));
		collapseAllButton.addActionListener(e -> collapseAll(true));
		expandAllButton.addActionListener(e -> collapseAll(false));
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = y++;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(4, 6, 0, 4);
		panel.add(collapsePanel, gbc);

		JPanel showPanel = new JPanel(new GridLayout(1, 2, 4, 0));
		ButtonGroup showGroup = new ButtonGroup();
		for (JRadioButton button: Arrays.asList(showTableButton, showChartButton)) {
			showGroup.add(button);
			showPanel.add(button);
			button.addActionListener(e -> {
				saveConfig();
				showResult();
			});
		}
		showTableButton.setToolTipText("Show the pivot table");
		showChartButton.setToolTipText("Show the pivot table as chart (the row groups without the subtotals and totals)");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = y++;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(8, 2, 0, 4);
		panel.add(showPanel, gbc);

		infoLabel.setForeground(infoLabel.getForeground().brighter());
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = y++;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(12, 6, 0, 4);
		panel.add(infoLabel, gbc);

		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = y++;
		gbc.weighty = 1;
		gbc.fill = GridBagConstraints.VERTICAL;
		panel.add(new JLabel(""), gbc);
		return panel;
	}

	/**
	 * Side bar panel. Its width follows the width of the scroll pane, so that the combo boxes shrink with it.
	 */
	private static class ConfigPanel extends JPanel implements Scrollable {
		ConfigPanel() {
			super(new GridBagLayout());
		}

		@Override
		public Dimension getPreferredScrollableViewportSize() {
			return getPreferredSize();
		}

		@Override
		public int getScrollableUnitIncrement(Rectangle visibleRect, int orientation, int direction) {
			return 16;
		}

		@Override
		public int getScrollableBlockIncrement(Rectangle visibleRect, int orientation, int direction) {
			return orientation == SwingConstants.VERTICAL ? visibleRect.height : visibleRect.width;
		}

		@Override
		public boolean getScrollableTracksViewportWidth() {
			return true;
		}

		@Override
		public boolean getScrollableTracksViewportHeight() {
			return false;
		}
	}

	private int addSection(JPanel panel, int y, String title, JComponent component, String toolTip) {
		JLabel label = new JLabel(title);
		label.setFont(label.getFont().deriveFont(Font.BOLD));
		label.setToolTipText(toolTip);
		if (!(component instanceof FieldListPanel)) {
			component.setToolTipText(toolTip);
		}
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = y++;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(y == 1 ? 6 : 12, 6, 2, 4);
		panel.add(label, gbc);

		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = y++;
		gbc.weightx = 1;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(0, 12, 0, 4);
		panel.add(component, gbc);
		return y;
	}

	/**
	 * Sets the table containing the rows and updates the pivot table.
	 *
	 * @param table the rows table
	 */
	public void setTable(JTable table) {
		this.currentTable = table;
		listenTo(table);
		refreshCombos();
		updatePivot();
	}

	/**
	 * The table whose changes are listened to.
	 */
	private JTable listenedTable;
	private boolean tableChangePending = false;

	/**
	 * Updates the pivot table (if visible) when the rows of the table are (re)loaded, e.g. after a reload of the result
	 * or a change of its condition. The table may still have no columns when {@link #setTable(JTable)} is called.
	 */
	private void listenTo(JTable table) {
		if (table == null || table == listenedTable) {
			return;
		}
		listenedTable = table;
		TableColumnModelListener columnModelListener = new TableColumnModelListener() {
			@Override
			public void columnAdded(TableColumnModelEvent e) {
				onTableChange(table);
			}
			@Override
			public void columnRemoved(TableColumnModelEvent e) {
				onTableChange(table);
			}
			@Override
			public void columnMoved(TableColumnModelEvent e) {
			}
			@Override
			public void columnMarginChanged(ChangeEvent e) {
			}
			@Override
			public void columnSelectionChanged(ListSelectionEvent e) {
			}
		};
		table.getColumnModel().addColumnModelListener(columnModelListener);
		table.addPropertyChangeListener("columnModel", e -> {
			if (e.getOldValue() instanceof TableColumnModel) {
				((TableColumnModel) e.getOldValue()).removeColumnModelListener(columnModelListener);
			}
			if (e.getNewValue() instanceof TableColumnModel) {
				((TableColumnModel) e.getNewValue()).addColumnModelListener(columnModelListener);
			}
			onTableChange(table);
		});
		table.addPropertyChangeListener("model", e -> onTableChange(table));
	}

	private void onTableChange(JTable table) {
		if (!tableChangePending) {
			tableChangePending = true;
			UIUtil.invokeLater(() -> {
				tableChangePending = false;
				if (table == currentTable && isSelectedTab()) {
					setTable(table);
				}
			});
		}
	}

	/**
	 * Whether this is the selected tab (also if the result isn't shown yet, e.g. while it is reloaded).
	 */
	private boolean isSelectedTab() {
		return getParent() instanceof JTabbedPane ? ((JTabbedPane) getParent()).getSelectedComponent() == this : isShowing();
	}

	/**
	 * The configuration of a pivot table, kept in the session: each pivot table starts with the last one configured
	 * (also after a reload of the result or a change of its condition).
	 */
	private static class PivotConfig {
		final List<String> rows;
		final List<String> columns;
		final List<String> valueNames;
		final List<String> aggregates;
		final boolean singleElementTotals;
		final int dividerLocation;
		final String sortColumn;
		final boolean sortDescending;
		final Set<List<Object>> collapsedGroups;
		final List<String> collapsedGroupsRowFields;
		final boolean showChart;
		final Map<String, Set<Object>> fieldFilters;
		final List<String> showAs;
		final boolean heatmap;

		PivotConfig(List<String> rows, List<String> columns, List<String> valueNames, List<String> aggregates, boolean singleElementTotals, int dividerLocation,
				String sortColumn, boolean sortDescending, Set<List<Object>> collapsedGroups, List<String> collapsedGroupsRowFields, boolean showChart,
				Map<String, Set<Object>> fieldFilters, List<String> showAs, boolean heatmap) {
			this.showAs = showAs;
			this.heatmap = heatmap;
			this.showChart = showChart;
			this.fieldFilters = fieldFilters;
			this.collapsedGroups = collapsedGroups;
			this.collapsedGroupsRowFields = collapsedGroupsRowFields;
			this.rows = rows;
			this.columns = columns;
			this.valueNames = valueNames;
			this.aggregates = aggregates;
			this.singleElementTotals = singleElementTotals;
			this.dividerLocation = dividerLocation;
			this.sortColumn = sortColumn;
			this.sortDescending = sortDescending;
		}
	}

	private static final String CONFIG_PROPERTY = "config";

	private static Map<String, Set<Object>> copyOf(Map<String, Set<Object>> filters) {
		Map<String, Set<Object>> copy = new HashMap<>();
		filters.forEach((field, keys) -> copy.put(field, new HashSet<>(keys)));
		return copy;
	}

	/**
	 * Whether the configuration of the session has been taken over.
	 */
	private boolean configured = false;
	private boolean loadingConfig = false;

	/**
	 * Saves the configuration in the session. Called on changes made by the user only, so that the defaults
	 * of a result without the configured fields don't replace it.
	 */
	private void saveConfig() {
		if (allRowsSession == null || !configured || loadingConfig) {
			return;
		}
		allRowsSession.setSessionProperty(SQLConsolePivotPanel.class, CONFIG_PROPERTY, new PivotConfig(
				selectedRowFields(), columnsFieldList.getFieldNames(), valuesList.getValueNames(), valuesList.getAggregates(),
				singleElementTotalsCheckBox.isSelected(), splitPane.getDividerLocation(), sortColumn, sortDescending,
				new HashSet<>(collapsedGroups), collapsedGroupsRowFields, showChartButton.isSelected(), copyOf(fieldFilters),
				valuesList.getShowAs(), heatmapCheckBox.isSelected()));
	}

	/**
	 * Takes over the configuration of the session. The fields are applied by name.
	 */
	private void loadConfig() {
		Object config = allRowsSession == null ? null : allRowsSession.getSessionProperty(SQLConsolePivotPanel.class, CONFIG_PROPERTY);
		if (config instanceof PivotConfig) {
			PivotConfig pivotConfig = (PivotConfig) config;
			pendingRows = pivotConfig.rows;
			pendingColumns = pivotConfig.columns;
			pendingValues = pivotConfig.valueNames;
			pendingAggregates = pivotConfig.aggregates;
			pendingShowAs = pivotConfig.showAs;
			heatmapCheckBox.setSelected(pivotConfig.heatmap);
			sortColumn = pivotConfig.sortColumn;
			sortDescending = pivotConfig.sortDescending;
			collapsedGroups.clear();
			collapsedGroups.addAll(pivotConfig.collapsedGroups);
			collapsedGroupsRowFields = pivotConfig.collapsedGroupsRowFields;
			fieldFilters.clear();
			fieldFilters.putAll(copyOf(pivotConfig.fieldFilters));
			rowsFieldList.refresh();
			columnsFieldList.refresh();
			loadingConfig = true;
			try {
				singleElementTotalsCheckBox.setSelected(pivotConfig.singleElementTotals);
				(pivotConfig.showChart ? showChartButton : showTableButton).setSelected(true);
				if (pivotConfig.dividerLocation > 0) {
					splitPane.setDividerLocation(pivotConfig.dividerLocation);
				}
			} finally {
				loadingConfig = false;
			}
		}
	}

	private void refreshCombos() {
		if (currentTable == null || currentTable.getColumnModel().getColumnCount() == 0 || isSingleRowView()) {
			// e.g. while the rows are reloaded: keep the selection
			return;
		}
		if (!configured) {
			loadConfig();
			configured = true;
		}
		boolean rowsSelByName = pendingRows != null;
		List<String> rowsSel = pendingRows != null ? pendingRows : selectedRowFields();
		List<Integer> oldRowsSelIndexes = selectedRowFieldIndexes();
		boolean columnsSelByName = pendingColumns != null;
		List<String> columnsSel = pendingColumns != null ? pendingColumns : columnsFieldList.getFieldNames();
		List<Integer> oldColumnsSelIndexes = columnsFieldList.getFieldIndexes();
		List<String> oldColumnNames = columnNames;
		boolean valuesSelByName = pendingValues != null;
		List<String> valuesSel = pendingValues != null ? pendingValues : valuesList.getValueNames();
		List<String> aggregatesSel = pendingAggregates != null ? pendingAggregates : valuesList.getAggregates();
		List<String> showAsSel = pendingShowAs != null ? pendingShowAs : valuesList.getShowAs();
		List<ValueSpec> oldValues = valuesList.getValues();
		pendingRows = null;
		pendingColumns = null;
		pendingValues = null;
		pendingAggregates = null;
		pendingShowAs = null;

		TableModel model = currentTable.getModel();
		TableColumnModel cm = currentTable.getColumnModel();
		List<String> names = new ArrayList<>();
		List<String> tables = new ArrayList<>();
		Map<String, Integer> nameCount = new HashMap<>();
		int firstNonNumeric = -1;
		int firstNumeric = -1;
		for (int i = 0; i < cm.getColumnCount(); i++) {
			int modelIdx = cm.getColumn(i).getModelIndex();
			String header = model.getColumnName(modelIdx);
			String name = stripHtml(header);
			names.add(name);
			tables.add(tableOfHeader(header));
			nameCount.merge(name, 1, Integer::sum);
			if (isNumeric(modelIdx)) {
				if (firstNumeric < 0) {
					firstNumeric = i;
				}
			} else if (firstNonNumeric < 0) {
				firstNonNumeric = i;
			}
		}
		// name collision: qualify with the table
		for (int i = 0; i < names.size(); i++) {
			if (nameCount.get(names.get(i)) > 1 && tables.get(i) != null) {
				names.set(i, tables.get(i) + "." + names.get(i));
			}
		}

		columnNames = names;

		suppressUpdate = true;
		try {
			List<Integer> rowsSelIndexes = restoreFieldIndexes(rowsSelByName, rowsSel, oldRowsSelIndexes, names, oldColumnNames);
			if (rowsSelIndexes.isEmpty() && !names.isEmpty()) {
				rowsSelIndexes.add(firstNonNumeric >= 0 ? firstNonNumeric : 0);
			}
			rowsFieldList.setColumnNames(names);
			rowsFieldList.setFieldIndexes(rowsSelIndexes);
			columnsFieldList.setColumnNames(names);
			columnsFieldList.setFieldIndexes(restoreFieldIndexes(columnsSelByName, columnsSel, oldColumnsSelIndexes, names, oldColumnNames));
			List<ValueSpec> values = new ArrayList<>();
			if (!valuesSelByName && names.equals(oldColumnNames)) {
				values.addAll(oldValues); // same columns: keep selection by index (column names may be ambiguous)
			} else {
				for (int i = 0; i < valuesSel.size() && i < aggregatesSel.size(); i++) {
					String name = valuesSel.get(i);
					int index = ROWS.equals(name) ? -1 : names.indexOf(name);
					if (index >= 0 || ROWS.equals(name)) {
						values.add(new ValueSpec(index, aggregatesSel.get(i), i < showAsSel.size() ? showAsSel.get(i) : SHOW_VALUE));
					}
				}
			}
			if (values.isEmpty()) {
				values.add(firstNumeric >= 0 ? new ValueSpec(firstNumeric, SUM) : new ValueSpec(-1, COUNT));
			}
			valuesList.setColumnNames(names);
			valuesList.setValues(values);
		} finally {
			suppressUpdate = false;
		}
	}

	/**
	 * Determines the fields to select after the columns of the table have been (re)read.
	 *
	 * @param byName <code>true</code> if the fields are taken over from another pivot panel (by name)
	 * @param selNames the names of the fields
	 * @param oldIndexes the indexes of the fields
	 * @param names the current column names
	 * @param oldNames the column names the indexes refer to
	 * @return the fields as indexes into the current column names
	 */
	private static List<Integer> restoreFieldIndexes(boolean byName, List<String> selNames, List<Integer> oldIndexes, List<String> names, List<String> oldNames) {
		List<Integer> result = new ArrayList<>();
		if (!byName && names.equals(oldNames)) {
			result.addAll(oldIndexes); // same columns: keep selection by index (column names may be ambiguous)
		} else {
			for (String name: selNames) {
				int index = names.indexOf(name);
				if (index >= 0) {
					result.add(index);
				}
			}
		}
		return result;
	}

	/**
	 * Gets the selected row fields.
	 */
	private List<String> selectedRowFields() {
		return rowsFieldList.getFieldNames();
	}

	/**
	 * Gets the selected row fields as indexes into {@link #columnNames}.
	 */
	private List<Integer> selectedRowFieldIndexes() {
		return rowsFieldList.getFieldIndexes();
	}

	/**
	 * Ordered list of fields (columns of the result): one line per field with a combo box
	 * to change it, buttons to move it up/down or remove it, and a trailing combo box to add a field.
	 */
	private static class FieldListPanel extends JPanel {
		private final String addText;
		private final String fieldText;
		private List<String> names = new ArrayList<>();
		private final List<Integer> fields = new ArrayList<>();
		private Runnable changeAction;
		private boolean rebuilding = false;

		FieldListPanel(String addText, String fieldText) {
			super(new GridBagLayout());
			this.addText = addText;
			this.fieldText = fieldText;
			rebuild();
		}

		void setColumnNames(List<String> names) {
			this.names = names;
			fields.removeIf(f -> f >= names.size());
			rebuild();
		}

		void setFieldIndexes(List<Integer> fieldIndexes) {
			fields.clear();
			for (int f: fieldIndexes) {
				if (f >= 0 && f < names.size()) {
					fields.add(f);
				}
			}
			rebuild();
		}

		List<Integer> getFieldIndexes() {
			return new ArrayList<>(fields);
		}

		List<String> getFieldNames() {
			List<String> result = new ArrayList<>();
			for (int f: fields) {
				result.add(names.get(f));
			}
			return result;
		}

		void setChangeAction(Runnable changeAction) {
			this.changeAction = changeAction;
		}

		private Predicate<String> isFiltered;
		private BiConsumer<String, JComponent> editFilter;

		/**
		 * Adds a filter button to each field.
		 *
		 * @param isFiltered whether a field (name) is filtered
		 * @param editFilter edits the filter of a field (name), the button is the anchor
		 */
		void setFilterHandler(Predicate<String> isFiltered, BiConsumer<String, JComponent> editFilter) {
			this.isFiltered = isFiltered;
			this.editFilter = editFilter;
			rebuild();
		}

		/**
		 * Updates the buttons (e.g. after a filter has been changed).
		 */
		void refresh() {
			rebuild();
		}

		private void changed() {
			UIUtil.invokeLater(() -> {
				rebuild();
				if (changeAction != null) {
					changeAction.run();
				}
			});
		}

		private void rebuild() {
			rebuilding = true;
			try {
				removeAll();
				int y = 0;
				int x0 = editFilter != null ? 1 : 0; // the filter button is the first one
				for (int i = 0; i < fields.size(); i++) {
					final int pos = i;
					JComboBox<String> combo = new JComboBox<>(names.toArray(new String[0]));
					combo.setPrototypeDisplayValue(COMBO_PROTOTYPE);
					combo.setSelectedIndex(fields.get(pos));
					combo.setToolTipText(fieldText.substring(0, 1).toUpperCase() + fieldText.substring(1) + " " + (pos + 1));
					combo.addActionListener(e -> {
						if (!rebuilding && combo.getSelectedIndex() >= 0) {
							fields.set(pos, combo.getSelectedIndex());
							changed();
						}
					});
					add(combo, constraints(0, y, true));

					if (editFilter != null) {
						String name = names.get(fields.get(pos));
						boolean filtered = isFiltered.test(name);
						JButton filter = smallButton(null, filtered ? "Filter (some values are hidden)" : "Filter: hide values");
						filter.setIcon(getFilterIcon());
						if (filtered) {
							filter.setBackground(UIUtil.BG_FLATSELECTED);
						}
						filter.addActionListener(e -> editFilter.accept(name, filter));
						add(filter, constraints(1, y, false));
					}

					JButton up = smallButton("↑", "Move up");
					up.setEnabled(pos > 0);
					up.addActionListener(e -> {
						Collections.swap(fields, pos, pos - 1);
						changed();
					});
					add(up, constraints(x0 + 1, y, false));

					JButton down = smallButton("↓", "Move down");
					down.setEnabled(pos < fields.size() - 1);
					down.addActionListener(e -> {
						Collections.swap(fields, pos, pos + 1);
						changed();
					});
					add(down, constraints(x0 + 2, y, false));

					JButton remove = smallButton(null, "Remove " + fieldText);
					remove.setIcon(getDeleteIcon());
					remove.addActionListener(e -> {
						fields.remove(pos);
						changed();
					});
					add(remove, constraints(x0 + 3, y, false));
					++y;
				}
				if (fields.size() < names.size()) {
					JComboBox<String> addCombo = new JComboBox<>();
					addCombo.setPrototypeDisplayValue(COMBO_PROTOTYPE);
					addCombo.addItem(addText);
					for (String name: names) {
						addCombo.addItem(name);
					}
					addCombo.setToolTipText("Add a " + fieldText);
					addCombo.addActionListener(e -> {
						if (!rebuilding && addCombo.getSelectedIndex() > 0) {
							fields.add(addCombo.getSelectedIndex() - 1);
							changed();
						}
					});
					add(addCombo, constraints(0, y, true));
				}
				revalidate();
				repaint();
			} finally {
				rebuilding = false;
			}
		}

		private static ImageIcon filterIcon;

		private static ImageIcon getFilterIcon() {
			if (filterIcon == null) {
				filterIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/filter.png")); // like "Table Filter" of the Data Browser
			}
			return filterIcon;
		}

		private static ImageIcon getDeleteIcon() {
			if (deleteIcon == null) {
				deleteIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/delete.png"));
			}
			return deleteIcon;
		}

		private static JButton smallButton(String text, String toolTip) {
			JButton button = new JButton(text);
			button.setToolTipText(toolTip);
			button.setMargin(new Insets(0, 2, 0, 2));
			button.setFocusable(false);
			return button;
		}

		private static GridBagConstraints constraints(int x, int y, boolean fill) {
			GridBagConstraints gbc = new GridBagConstraints();
			gbc.gridx = x;
			gbc.gridy = y;
			gbc.insets = new Insets(0, x == 0 ? 0 : 2, 2, 0);
			if (fill) {
				gbc.weightx = 1;
				gbc.fill = GridBagConstraints.HORIZONTAL;
			} else {
				gbc.fill = GridBagConstraints.VERTICAL;
			}
			return gbc;
		}
	}

	/**
	 * Is a field (index into {@link #columnNames}) numeric?
	 */
	private boolean isNumericField(int field) {
		if (currentTable == null || field < 0 || field >= currentTable.getColumnModel().getColumnCount()) {
			return false;
		}
		return isNumeric(currentTable.getColumnModel().getColumn(field).getModelIndex());
	}

	/**
	 * A value of the pivot table: a field (or the rows) and its aggregate function.
	 */
	private static class ValueSpec {
		final int field; // index into the column names, -1 for "(rows)"
		final String aggregate;
		final String showAs; // one of SHOW_AS

		ValueSpec(int field, String aggregate) {
			this(field, aggregate, SHOW_VALUE);
		}

		ValueSpec(int field, String aggregate, String showAs) {
			this.field = field;
			this.aggregate = ROWS_AGGREGATES.contains(aggregate) || field >= 0 && Arrays.asList(ALL_AGGREGATES).contains(aggregate) ? aggregate : COUNT;
			this.showAs = Arrays.asList(SHOW_AS).contains(showAs) ? showAs : SHOW_VALUE;
		}

		boolean isPercentage() {
			return !SHOW_VALUE.equals(showAs);
		}

		/**
		 * Gets the header of the value.
		 *
		 * @param name name of the field
		 */
		String header(String name) {
			return aggregate + "(" + (field < 0 ? "rows" : name) + ")" + (isPercentage() ? " " + showAs : "");
		}

		@Override
		public String toString() {
			return field + ":" + aggregate + ":" + showAs;
		}
	}

	/**
	 * How a value is shown: the value itself, or as percentage of the total of its line, of its column or of the grand total.
	 */
	private static final String SHOW_VALUE = "value";
	private static final String PERCENT_OF_ROW = "% of row";
	private static final String PERCENT_OF_COLUMN = "% of column";
	private static final String PERCENT_OF_TOTAL = "% of total";
	private static final String[] SHOW_AS = { SHOW_VALUE, PERCENT_OF_ROW, PERCENT_OF_COLUMN, PERCENT_OF_TOTAL };

	/**
	 * A value shown as percentage (the ratio, e.g. 0.25 for 25 %).
	 */
	private static final class Percentage extends Number {
		private static final long serialVersionUID = 1L;
		final double ratio;

		Percentage(double ratio) {
			this.ratio = ratio;
		}

		@Override
		public int intValue() {
			return (int) ratio;
		}

		@Override
		public long longValue() {
			return (long) ratio;
		}

		@Override
		public float floatValue() {
			return (float) ratio;
		}

		@Override
		public double doubleValue() {
			return ratio;
		}

		@Override
		public String toString() {
			return Double.toString(ratio);
		}

		/**
		 * Formats the percentage, e.g. "25.5 %" (with the decimal separator of the locale).
		 */
		String format() {
			NumberFormat format = NumberFormat.getPercentInstance();
			format.setMaximumFractionDigits(1);
			return format.format(ratio);
		}
	}

	/**
	 * Ordered list of values: one line per value with combo boxes for the field and the aggregate function,
	 * buttons to move it up/down or remove it, and a trailing combo box to add a value.
	 */
	private static class ValueListPanel extends JPanel {
		private final IntPredicate isNumeric;
		private List<String> names = new ArrayList<>();
		private final List<ValueSpec> values = new ArrayList<>();
		private Runnable changeAction;
		private boolean rebuilding = false;

		ValueListPanel(IntPredicate isNumeric) {
			super(new GridBagLayout());
			this.isNumeric = isNumeric;
			rebuild();
		}

		void setColumnNames(List<String> names) {
			this.names = names;
			values.removeIf(v -> v.field >= names.size());
			rebuild();
		}

		void setValues(List<ValueSpec> newValues) {
			values.clear();
			for (ValueSpec v: newValues) {
				if (v.field < names.size()) {
					values.add(v);
				}
			}
			rebuild();
		}

		List<ValueSpec> getValues() {
			return new ArrayList<>(values);
		}

		/**
		 * Gets the names of the fields of the values ("(rows)" for the rows).
		 */
		List<String> getValueNames() {
			List<String> result = new ArrayList<>();
			for (ValueSpec v: values) {
				result.add(v.field < 0 ? ROWS : names.get(v.field));
			}
			return result;
		}

		List<String> getAggregates() {
			List<String> result = new ArrayList<>();
			for (ValueSpec v: values) {
				result.add(v.aggregate);
			}
			return result;
		}

		List<String> getShowAs() {
			List<String> result = new ArrayList<>();
			for (ValueSpec v: values) {
				result.add(v.showAs);
			}
			return result;
		}

		void setChangeAction(Runnable changeAction) {
			this.changeAction = changeAction;
		}

		private void changed() {
			UIUtil.invokeLater(() -> {
				rebuild();
				if (changeAction != null) {
					changeAction.run();
				}
			});
		}

		private JComboBox<String> createFieldCombo(String firstItem) {
			JComboBox<String> combo = new JComboBox<>();
			combo.setPrototypeDisplayValue(COMBO_PROTOTYPE);
			combo.addItem(firstItem);
			for (String name: names) {
				combo.addItem(name);
			}
			return combo;
		}

		private void rebuild() {
			rebuilding = true;
			try {
				removeAll();
				int y = 0;
				for (int i = 0; i < values.size(); i++) {
					final int pos = i;
					ValueSpec value = values.get(pos);

					JComboBox<String> fieldCombo = createFieldCombo(ROWS);
					fieldCombo.setSelectedIndex(value.field + 1);
					fieldCombo.setToolTipText("Column to aggregate. \"" + ROWS + "\" counts the rows.");
					fieldCombo.addActionListener(e -> {
						if (!rebuilding && fieldCombo.getSelectedIndex() >= 0) {
							values.set(pos, new ValueSpec(fieldCombo.getSelectedIndex() - 1, values.get(pos).aggregate, values.get(pos).showAs));
							changed();
						}
					});
					add(fieldCombo, FieldListPanel.constraints(0, y, true));

					JComboBox<String> aggregateCombo = new JComboBox<>((value.field < 0 ? ROWS_AGGREGATES : Arrays.asList(ALL_AGGREGATES)).toArray(new String[0]));
					aggregateCombo.setSelectedItem(value.aggregate);
					aggregateCombo.setToolTipText("Aggregate function");
					aggregateCombo.addActionListener(e -> {
						if (!rebuilding && aggregateCombo.getSelectedItem() != null) {
							values.set(pos, new ValueSpec(values.get(pos).field, (String) aggregateCombo.getSelectedItem(), values.get(pos).showAs));
							changed();
						}
					});
					add(aggregateCombo, FieldListPanel.constraints(1, y, false));

					JButton up = FieldListPanel.smallButton("↑", "Move up");
					up.setEnabled(pos > 0);
					up.addActionListener(e -> {
						Collections.swap(values, pos, pos - 1);
						changed();
					});
					add(up, FieldListPanel.constraints(2, y, false));

					JButton down = FieldListPanel.smallButton("↓", "Move down");
					down.setEnabled(pos < values.size() - 1);
					down.addActionListener(e -> {
						Collections.swap(values, pos, pos + 1);
						changed();
					});
					add(down, FieldListPanel.constraints(3, y, false));

					JButton remove = FieldListPanel.smallButton(null, "Remove value");
					remove.setIcon(FieldListPanel.getDeleteIcon());
					remove.addActionListener(e -> {
						values.remove(pos);
						changed();
					});
					add(remove, FieldListPanel.constraints(4, y, false));
					++y;

					// below: shown as value or percentage
					JComboBox<String> showAsCombo = new JComboBox<>(SHOW_AS);
					showAsCombo.setSelectedItem(value.showAs);
					showAsCombo.setToolTipText("Show the value itself, or as percentage of the total of its line, of its column or of the grand total");
					showAsCombo.addActionListener(e -> {
						if (!rebuilding && showAsCombo.getSelectedItem() != null) {
							values.set(pos, new ValueSpec(values.get(pos).field, values.get(pos).aggregate, (String) showAsCombo.getSelectedItem()));
							changed();
						}
					});
					GridBagConstraints showAsConstraints = FieldListPanel.constraints(1, y, false);
					showAsConstraints.fill = GridBagConstraints.HORIZONTAL;
					showAsConstraints.insets = new Insets(0, 2, 6, 0);
					add(showAsCombo, showAsConstraints);
					++y;
				}
				JComboBox<String> addCombo = createFieldCombo("(add value)");
				addCombo.insertItemAt(ROWS, 1);
				addCombo.setSelectedIndex(0);
				addCombo.setToolTipText("Add a value");
				addCombo.addActionListener(e -> {
					if (!rebuilding && addCombo.getSelectedIndex() > 0) {
						int field = addCombo.getSelectedIndex() - 2; // items: "(add value)", ROWS, columns
						values.add(new ValueSpec(field, field >= 0 && isNumeric.test(field) ? SUM : COUNT));
						changed();
					}
				});
				GridBagConstraints gbc = FieldListPanel.constraints(0, y, true);
				gbc.gridwidth = 2;
				add(addCombo, gbc);
				revalidate();
				repaint();
			} finally {
				rebuilding = false;
			}
		}
	}

	/**
	 * Whether the table shows a single row as details (a table browser of the Data Browser does that), so that its columns are not those of the rows.
	 */
	private boolean isSingleRowView() {
		return rowBrowser != null && rowBrowser.rows.size() == 1 && currentTable != null
				&& currentTable.getModel().getColumnCount() != rowBrowser.rows.get(0).values.length;
	}

	private void updatePivot() {
		if (currentTable == null) {
			return;
		}
		if (isSingleRowView()) {
			showMessage("The pivot table needs more than one row.");
			return;
		}
		TableColumnModel cm = currentTable.getColumnModel();
		List<String> rowFields = selectedRowFields();
		List<ValueSpec> values = valuesList.getValues();
		List<String> valueNames = valuesList.getValueNames();
		int[] rowsCols = selectedRowFieldIndexes().stream()
				.mapToInt(view -> view < cm.getColumnCount() ? cm.getColumn(view).getModelIndex() : -1)
				.toArray();
		int[] columnsCols = columnsFieldList.getFieldIndexes().stream()
				.mapToInt(view -> view < cm.getColumnCount() ? cm.getColumn(view).getModelIndex() : -1)
				.toArray();
		int[] valueCols = values.stream()
				.mapToInt(v -> v.field < 0 ? -1 : v.field < cm.getColumnCount() ? cm.getColumn(v.field).getModelIndex() : -2)
				.toArray();
		if (rowsCols.length == 0 || values.isEmpty() || Arrays.stream(rowsCols).anyMatch(c -> c < 0) || Arrays.stream(columnsCols).anyMatch(c -> c < 0) || Arrays.stream(valueCols).anyMatch(c -> c < -1)) {
			showMessage("Select fields on the left to create a pivot table.");
			return;
		}
		boolean[] distinct = new boolean[values.size()];
		for (int v = 0; v < distinct.length; v++) {
			distinct[v] = COUNT_DISTINCT.equals(values.get(v).aggregate);
		}
		if (rowBrowser == null) {
			showMessage("No rows.");
			return;
		}

		// the filters of the row and column fields
		Map<Integer, Set<Object>> excluded = new TreeMap<>();
		List<String> columnFields = columnsFieldList.getFieldNames();
		for (int f = 0; f < rowsCols.length + columnsCols.length; f++) {
			String field = f < rowsCols.length ? rowFields.get(f) : columnFields.get(f - rowsCols.length);
			Set<Object> excludedKeys = fieldFilters.get(field);
			if (excludedKeys != null && !excludedKeys.isEmpty()) {
				excluded.merge(f < rowsCols.length ? rowsCols[f] : columnsCols[f - rowsCols.length], excludedKeys, (a, b) -> {
					Set<Object> union = new HashSet<>(a);
					union.addAll(b);
					return union;
				});
			}
		}
		StringBuilder filterKey = new StringBuilder();
		for (Map.Entry<Integer, Set<Object>> e: excluded.entrySet()) {
			filterKey.append(e.getKey()).append("=").append(e.getValue().stream().map(String::valueOf).sorted().collect(Collectors.toList())).append(";");
		}

		// the loaded rows, or all rows of the statement if the row limit is exceeded
		List<Row> loadedRows = rowBrowser.rows;
		Aggregation aggregation = null;
		boolean basedOnAllRows;
		String info;
		boolean limitExceeded = rowBrowser.isRowLimitExceeded();
		String allRowsSql = limitExceeded && allRowsStatement != null ? allRowsStatement.get() : null;
		try {
			if (allRowsSql != null) {
				String key = Arrays.toString(rowsCols) + "|" + Arrays.toString(columnsCols) + "|" + Arrays.toString(valueCols) + "|" + Arrays.toString(distinct)
						+ "|" + filterKey + "|" + allRowsSql + "|" + System.identityHashCode(loadedRows) + "|" + loadedRows.size();
				if (!key.equals(allRowsKey)) {
					allRowsKey = key;
					allRowsAggregation = null;
					allRowsTooManyColumnKeys = null;
					try {
						allRowsAggregation = readAllRows(allRowsSql, rowsCols, columnsCols, valueCols, distinct, excluded);
					} catch (TooManyColumnKeysException e) {
						allRowsTooManyColumnKeys = e;
					}
				}
				if (allRowsTooManyColumnKeys != null) {
					throw allRowsTooManyColumnKeys;
				}
				aggregation = allRowsAggregation;
			}
			basedOnAllRows = aggregation != null;
			if (aggregation != null) {
				info = "based on all " + UIUtil.format(aggregation.rowCount) + " row" + (aggregation.rowCount == 1 ? "" : "s");
			} else {
				aggregation = new Aggregation(rowsCols, columnsCols, valueCols, distinct, excluded);
				for (Row row: loadedRows) {
					aggregation.add(row.values);
				}
				info = "based on " + (limitExceeded ? "the first " : "") + UIUtil.format(aggregation.rowCount) + " row" + (aggregation.rowCount == 1 ? "" : "s")
						+ (limitExceeded ? " (row limit exceeded)" : "");
			}
			if (aggregation.filteredCount > 0) {
				info += " (" + UIUtil.format(aggregation.filteredCount) + " filtered out)";
			}
		} catch (TooManyColumnKeysException e) {
			showMessage("The column fields have more than " + MAX_COLUMN_KEYS + " different values.");
			return;
		}
		Map<List<Object>, Map<List<Object>, Accumulator[]>> cells = aggregation.cells;
		Map<List<Object>, Set<Object>> rowChildren = aggregation.rowChildren;
		Map<List<Object>, Set<Object>> columnChildren = aggregation.columnChildren;

		// column groups, each with one column per value
		List<PivotColumn> groups = new ArrayList<>();
		if (columnsCols.length > 0) {
			addColumns(new ArrayList<>(), columnsCols.length, columnChildren, groups, false);
			groups.add(new PivotColumn(new ArrayList<>(), true, 0));
		} else {
			groups.add(new PivotColumn(new ArrayList<>(), false, 0));
		}
		List<PivotColumn> columns = new ArrayList<>();
		for (PivotColumn group: groups) {
			for (int v = 0; v < values.size(); v++) {
				columns.add(new PivotColumn(group.prefix, group.isTotal, v));
			}
		}

		List<String> headers = new ArrayList<>(rowFields);
		for (PivotColumn column: columns) {
			ValueSpec value = values.get(column.valueIndex);
			String valueHeader = value.header(valueNames.get(column.valueIndex));
			String groupHeader;
			if (columnsCols.length == 0) {
				groupHeader = null;
			} else if (column.prefix.isEmpty()) {
				groupHeader = "Total";
			} else {
				groupHeader = column.prefix.stream().map(String::valueOf).collect(Collectors.joining(" / ")) + (column.isTotal ? " Total" : "");
			}
			if (groupHeader == null) {
				headers.add(valueHeader);
			} else if (values.size() == 1) {
				headers.add(groupHeader + (value.isPercentage() ? " (" + value.showAs + ")" : ""));
			} else {
				headers.add(groupHeader + " / " + valueHeader);
			}
		}

		LineOrder order = null;
		int sortIndex = sortColumn == null ? -1 : headers.subList(rowsCols.length, headers.size()).indexOf(sortColumn);
		if (sortIndex >= 0) {
			PivotColumn column = columns.get(sortIndex);
			order = new LineOrder(cells, column, values.get(column.valueIndex), sortDescending);
		}
		shownSortIndex = sortIndex < 0 ? -1 : rowsCols.length + sortIndex;

		if (!rowFields.equals(collapsedGroupsRowFields)) {
			// other row fields: other groups
			collapsedGroups.clear();
			collapsedGroupsRowFields = rowFields;
		}
		List<PivotLine> lines = new ArrayList<>();
		addLines(new ArrayList<>(), rowsCols.length, rowChildren, new boolean[rowsCols.length], lines, order, false);
		shownLineOrder = order;
		lines.add(new PivotLine(new ArrayList<>(), true));

		List<Object[]> data = new ArrayList<>();
		for (PivotLine line: lines) {
			Object[] lineData = new Object[headers.size()];
			int n = rowsCols.length;
			if (line.collapsed) {
				for (int f = 0; f < line.prefix.size(); f++) {
					if (line.showLabel[f]) {
						lineData[f] = String.valueOf(line.prefix.get(f));
					}
				}
			} else if (line.isTotal) {
				int k = line.prefix.size();
				lineData[k == 0 ? 0 : k - 1] = k == 0 ? "Total" : line.prefix.get(k - 1) + " Total";
			} else {
				for (int f = 0; f < n; f++) {
					if (line.showLabel[f]) {
						lineData[f] = String.valueOf(line.prefix.get(f));
					}
				}
			}
			Map<List<Object>, Accumulator[]> lineCells = cells.get(line.prefix);
			for (int c = 0; c < columns.size(); c++) {
				PivotColumn column = columns.get(c);
				lineData[n + c] = lineCells == null ? null : cellValue(cells, line.prefix, column, values.get(column.valueIndex));
			}
			data.add(lineData);
		}

		resultTable.setModel(new PivotTableModel(headers, data, lines, rowsCols.length, columns));
		shownLines = lines;
		shownColumns = columns;
		shownRowsCols = rowsCols;
		shownColumnsCols = columnsCols;
		shownAggregation = aggregation;
		shownBasedOnAllRows = basedOnAllRows;
		shownExcluded = excluded;
		shownColumnFields = columnFields;
		shownValueNames = valueNames;
		shownValues = values;
		shownValueCols = valueCols;
		shownHeaders = headers;
		adjustColumnWidths();
		infoLabel.setText(info);
		updateButtons();
		showResult();
	}

	private final JScrollPane resultTableScrollPane = new JScrollPane(resultTable);
	private final JRadioButton showTableButton = new JRadioButton("Table", true);
	private final JRadioButton showChartButton = new JRadioButton("Chart");
	private final List<Integer> chartColumnTypes = new ArrayList<>();
	private final SQLConsoleChartPanel chartPanel = new SQLConsoleChartPanel(chartColumnTypes);
	private boolean chartShownBefore = false;

	/**
	 * Shows the pivot table currently computed, as table or as chart.
	 */
	private void showResult() {
		Component content = resultTableScrollPane;
		if (showChartButton.isSelected() && shownLines != null) {
			updateChart();
			content = chartPanel;
		}
		if (centerPanel.getComponentCount() != 1 || centerPanel.getComponent(0) != content) {
			centerPanel.removeAll();
			centerPanel.add(content);
		}
		centerPanel.revalidate();
		centerPanel.repaint();
	}

	/**
	 * Passes the pivot table to the chart: one entry per row group shown (without the subtotal and total lines),
	 * one series per value column (without the subtotal and total columns).
	 */
	private void updateChart() {
		int n = shownRowsCols.length;
		TableModel pivotModel = resultTable.getModel();
		List<Integer> chartColumns = new ArrayList<>();
		for (int c = 0; c < shownColumns.size(); c++) {
			if (!shownColumns.get(c).isTotal || shownColumnsCols.length == 0) {
				chartColumns.add(n + c);
			}
		}
		List<String> names = new ArrayList<>();
		names.add(String.join(" / ", shownHeaders.subList(0, n)));
		for (int c: chartColumns) {
			names.add(shownHeaders.get(c));
		}
		DefaultTableModel chartModel = new DefaultTableModel(names.toArray(), 0) {
			private static final long serialVersionUID = 1L;
			@Override
			public boolean isCellEditable(int row, int column) {
				return false;
			}
		};
		for (int r = 0; r < shownLines.size(); r++) {
			PivotLine line = shownLines.get(r);
			if (line.isTotal && !line.collapsed) {
				continue;
			}
			Object[] chartRow = new Object[names.size()];
			chartRow[0] = line.prefix.stream().map(String::valueOf).collect(Collectors.joining(" / "));
			for (int i = 0; i < chartColumns.size(); i++) {
				chartRow[i + 1] = pivotModel.getValueAt(r, chartColumns.get(i));
			}
			chartModel.addRow(chartRow);
		}
		chartColumnTypes.clear();
		chartColumnTypes.add(Types.VARCHAR);
		for (int i = 0; i < chartColumns.size(); i++) {
			chartColumnTypes.add(Types.NUMERIC);
		}
		if (!chartShownBefore) {
			// initially, all values are series
			chartPanel.setPendingYColumns(names.subList(1, names.size()));
			chartShownBefore = true;
		}
		chartPanel.setTable(new JTable(chartModel));
	}

	/**
	 * Executes the statement again (without row limit) and aggregates all rows,
	 * in the thread and transaction of the SQL Console. Shows a dialog with "Cancel" if it takes a while.
	 *
	 * @return the aggregation, or <code>null</code> if cancelled or failed (the error is shown)
	 */
	private Aggregation readAllRows(String sql, int[] rowsCols, int[] columnsCols, int[] valueCols, boolean[] distinct, Map<Integer, Set<Object>> excluded) {
		Aggregation aggregation = new Aggregation(rowsCols, columnsCols, valueCols, distinct, excluded);
		return streamAllRows(sql, aggregation::isUsed, aggregation::add) ? aggregation : null;
	}

	/**
	 * Executes the statement again (without row limit) and passes all rows to a consumer,
	 * in the thread and transaction of the SQL Console. Shows a dialog with "Cancel" if it takes a while.
	 *
	 * @param isUsed whether the value of a column (model index) is needed (the others are <code>null</code>)
	 * @param consumer gets the (unconverted) values of each row (in the thread of the SQL Console)
	 * @return <code>false</code> if cancelled or failed (the error is shown)
	 */
	private boolean streamAllRows(String sql, IntPredicate isUsed, Consumer<Object[]> consumer) {
		Object context = new Object();
		AtomicBoolean cancelled = new AtomicBoolean(false);
		try {
			return ConcurrentTaskControl.call(SwingUtilities.getWindowAncestor(this), () -> {
				CompletableFuture<Boolean> future = new CompletableFuture<Boolean>();
				allRowsExecutor.accept(() -> {
					if (cancelled.get()) {
						future.completeExceptionally(new CancellationException());
						return;
					}
					try {
						allRowsSession.executeQuery(sql, new AbstractResultSetReader() {
							@Override
							public void readCurrentRow(ResultSet resultSet) throws SQLException {
								int count = getMetaData(resultSet).getColumnCount();
								CellContentConverter cellContentConverter = getCellContentConverter(resultSet, allRowsSession, allRowsSession.dbms);
								Object[] values = new Object[count];
								for (int i = 1; i <= count; ++i) {
									if (isUsed.test(i - 1)) {
										Object value = cellContentConverter.getObject(resultSet, i);
										values[i - 1] = resultSet.wasNull() ? null : value;
									}
								}
								consumer.accept(values);
							}
						}, null, context, 0);
						future.complete(true);
					} catch (Throwable t) {
						future.completeExceptionally(t);
					}
				});
				try {
					return future.get();
				} catch (ExecutionException e) {
					if (e.getCause() instanceof Exception) {
						throw (Exception) e.getCause();
					}
					throw e;
				}
			}, "Executing statement for the pivot table...", UIUtil.blinkingInfoLabel(null), true);
		} catch (TooManyColumnKeysException e) {
			throw e;
		} catch (CancellationException e) {
			cancelled.set(true);
			CancellationHandler.cancel(context);
		} catch (Throwable t) {
			UIUtil.showException(this, "Error", t);
		} finally {
			CancellationHandler.reset(context);
		}
		return false;
	}

	/**
	 * Thrown if the column fields have more than {@link #MAX_COLUMN_KEYS} different values.
	 */
	private static class TooManyColumnKeysException extends RuntimeException {
		private static final long serialVersionUID = 1L;
	}

	/**
	 * Aggregates rows into the cells of the pivot table.
	 * Keys of the row (column) groups are the lists of the values of the row (column) fields.
	 * Prefixes of these lists are the keys of the groups with subtotals, the empty list is the key of the total.
	 */
	private static class Aggregation {
		final int[] rowsCols;
		final int[] columnsCols;
		final int[] valueCols; // -1: the rows are counted
		final boolean[] distinct;
		final Map<List<Object>, Map<List<Object>, Accumulator[]>> cells = new HashMap<>();
		final Map<List<Object>, Set<Object>> rowChildren = new HashMap<>();
		final Map<List<Object>, Set<Object>> columnChildren = new HashMap<>();
		final Set<List<Object>> columnKeys = new HashSet<>();
		final List<Map<Object, Object>> rawColumnKeys = new ArrayList<>(); // per column field: a value (as read) of each key
		final Map<Integer, Set<Object>> excluded; // per column (of row and column fields): keys of the rows filtered out
		final Map<Integer, Map<Object, Object>> fieldValues = new HashMap<>(); // per column (of row and column fields): all keys (also of the rows filtered out) with a value (as read)
		long rowCount = 0;
		long filteredCount = 0;

		Aggregation(int[] rowsCols, int[] columnsCols, int[] valueCols, boolean[] distinct, Map<Integer, Set<Object>> excluded) {
			this.rowsCols = rowsCols;
			this.columnsCols = columnsCols;
			this.valueCols = valueCols;
			this.distinct = distinct;
			this.excluded = excluded;
			for (int i = 0; i < columnsCols.length; i++) {
				rawColumnKeys.add(new HashMap<>());
			}
		}

		/**
		 * Is the value of a column needed?
		 */
		boolean isUsed(int column) {
			return Arrays.stream(valueCols).anyMatch(c -> c == column) || Arrays.stream(rowsCols).anyMatch(c -> c == column) || Arrays.stream(columnsCols).anyMatch(c -> c == column);
		}

		/**
		 * Collects the key of a field value and checks whether it is filtered out.
		 */
		private boolean isExcluded(int column, Object key, Object raw) {
			fieldValues.computeIfAbsent(column, c -> new HashMap<>()).putIfAbsent(key, raw);
			Set<Object> excludedKeys = excluded.get(column);
			return excludedKeys != null && excludedKeys.contains(key);
		}

		private Accumulator[] newAccumulators() {
			Accumulator[] accumulators = new Accumulator[valueCols.length];
			for (int v = 0; v < valueCols.length; v++) {
				accumulators[v] = new Accumulator(distinct[v]);
			}
			return accumulators;
		}

		/**
		 * Adds a row.
		 *
		 * @param values the (unconverted) values of the row
		 */
		void add(Object[] values) {
			boolean filteredOut = false;
			List<Object> rowKey = new ArrayList<>(rowsCols.length);
			for (int rowsCol: rowsCols) {
				Object raw = rowsCol < values.length ? values[rowsCol] : null;
				Object key = toKey(raw);
				rowKey.add(key);
				filteredOut |= isExcluded(rowsCol, key, raw);
			}
			List<Object> columnKey = new ArrayList<>(columnsCols.length);
			for (int f = 0; f < columnsCols.length; f++) {
				Object raw = columnsCols[f] < values.length ? values[columnsCols[f]] : null;
				Object key = toKey(raw);
				columnKey.add(key);
				filteredOut |= isExcluded(columnsCols[f], key, raw);
				if (!filteredOut) {
					rawColumnKeys.get(f).putIfAbsent(key, raw);
				}
			}
			if (filteredOut) {
				++filteredCount;
				return;
			}
			Object[] rowValues = new Object[valueCols.length];
			for (int v = 0; v < valueCols.length; v++) {
				int valueCol = valueCols[v];
				rowValues[v] = valueCol < 0 ? ROW_MARKER : valueCol < values.length ? normalize(values[valueCol]) : null;
			}
			if (columnKeys.add(columnKey) && columnKeys.size() > MAX_COLUMN_KEYS) {
				throw new TooManyColumnKeysException();
			}
			++rowCount;
			for (int k = 0; k <= rowKey.size(); k++) {
				List<Object> rowPrefix = rowKey.subList(0, k);
				if (k < rowKey.size()) {
					rowChildren.computeIfAbsent(rowPrefix, p -> new HashSet<>()).add(rowKey.get(k));
				}
				Map<List<Object>, Accumulator[]> lineCells = cells.computeIfAbsent(rowPrefix, p -> new HashMap<>());
				for (int j = 0; j <= columnKey.size(); j++) {
					List<Object> columnPrefix = columnKey.subList(0, j);
					if (k == 0 && j < columnKey.size()) {
						columnChildren.computeIfAbsent(columnPrefix, p -> new HashSet<>()).add(columnKey.get(j));
					}
					Accumulator[] accumulators = lineCells.computeIfAbsent(columnPrefix, p -> newAccumulators());
					for (int v = 0; v < accumulators.length; v++) {
						accumulators[v].add(rowValues[v]);
					}
				}
			}
		}
	}

	/**
	 * Sorts the subgroups of a row group by their value in a column of the pivot table
	 * (null last, equal values by key).
	 */
	/**
	 * Gets the value of a cell of the pivot table: the aggregate, or its percentage of the total of the line, of the column or of the grand total.
	 *
	 * @param cells the aggregated cells
	 * @param rowPrefix key of the line
	 * @param column the column
	 * @param value the value of the column
	 * @return the value, a {@link Percentage}, or <code>null</code>
	 */
	private static Object cellValue(Map<List<Object>, Map<List<Object>, Accumulator[]>> cells, List<Object> rowPrefix, PivotColumn column, ValueSpec value) {
		Object result = aggregate(cells, rowPrefix, column.prefix, column.valueIndex, value);
		if (!value.isPercentage() || result == null) {
			return result;
		}
		List<Object> none = Collections.emptyList();
		Object total = PERCENT_OF_ROW.equals(value.showAs) ? aggregate(cells, rowPrefix, none, column.valueIndex, value)
				: PERCENT_OF_COLUMN.equals(value.showAs) ? aggregate(cells, none, column.prefix, column.valueIndex, value)
				: aggregate(cells, none, none, column.valueIndex, value);
		BigDecimal numerator = toBigDecimal(result);
		BigDecimal denominator = toBigDecimal(total);
		if (numerator == null || denominator == null || denominator.signum() == 0) {
			return null;
		}
		return new Percentage(numerator.divide(denominator, MathContext.DECIMAL64).doubleValue());
	}

	private static Object aggregate(Map<List<Object>, Map<List<Object>, Accumulator[]>> cells, List<Object> rowPrefix, List<Object> columnPrefix, int valueIndex, ValueSpec value) {
		Map<List<Object>, Accumulator[]> lineCells = cells.get(rowPrefix);
		Accumulator[] accumulators = lineCells == null ? null : lineCells.get(columnPrefix);
		return accumulators == null ? null : accumulators[valueIndex].result(value.aggregate);
	}

	private static class LineOrder {
		private final Map<List<Object>, Map<List<Object>, Accumulator[]>> cells;
		private final PivotColumn column;
		private final ValueSpec value;
		private final boolean descending;

		LineOrder(Map<List<Object>, Map<List<Object>, Accumulator[]>> cells, PivotColumn column, ValueSpec value, boolean descending) {
			this.cells = cells;
			this.column = column;
			this.value = value;
			this.descending = descending;
		}

		List<Object> sort(List<Object> prefix, Set<Object> keys) {
			List<Object> result = sortedKeys(keys);
			Map<Object, BigDecimal> values = new HashMap<>();
			for (Object key: result) {
				List<Object> subPrefix = new ArrayList<>(prefix);
				subPrefix.add(key);
				values.put(key, toBigDecimal(cellValue(cells, subPrefix, column, value)));
			}
			// stable: equal values keep the order of the keys
			result.sort((a, b) -> {
				BigDecimal va = values.get(a);
				BigDecimal vb = values.get(b);
				if (va == null || vb == null) {
					return va == null ? (vb == null ? 0 : 1) : -1;
				}
				return descending ? vb.compareTo(va) : va.compareTo(vb);
			});
			return result;
		}
	}

	/**
	 * Adds the lines of a row group (recursively, with a subtotal line after each subgroup).
	 *
	 * @param prefix key of the group
	 * @param n number of row fields
	 * @param children the keys of the subgroups per group
	 * @param pendingLabel per row field: has a new group started whose label has not yet been shown?
	 * @param lines to add the lines to
	 * @param order sorts the subgroups by a value, or <code>null</code> to sort them by key
	 * @param complete if <code>true</code>, no group is collapsed and each group has a subtotal line (e.g. for an export with outline)
	 */
	private void addLines(List<Object> prefix, int n, Map<List<Object>, Set<Object>> children, boolean[] pendingLabel, List<PivotLine> lines, LineOrder order, boolean complete) {
		int level = prefix.size();
		Set<Object> keys = children.get(prefix);
		if (keys == null) {
			return;
		}
		for (Object key: order == null ? sortedKeys(keys) : order.sort(prefix, keys)) {
			List<Object> subPrefix = new ArrayList<>(prefix);
			subPrefix.add(key);
			pendingLabel[level] = true;
			if (level == n - 1) {
				PivotLine line = new PivotLine(subPrefix, false);
				for (int f = 0; f < n; f++) {
					line.showLabel[f] = pendingLabel[f];
					pendingLabel[f] = false;
				}
				lines.add(line);
			} else if (!complete && collapsedGroups.contains(subPrefix)) {
				// only the subtotal line, labeled like the first line of the group
				PivotLine line = new PivotLine(subPrefix, true, true);
				for (int f = 0; f <= level; f++) {
					line.showLabel[f] = pendingLabel[f];
					pendingLabel[f] = false;
				}
				lines.add(line);
			} else {
				addLines(subPrefix, n, children, pendingLabel, lines, order, complete);
				if (complete || showSubtotal(children.get(subPrefix))) {
					lines.add(new PivotLine(subPrefix, true));
				}
			}
		}
	}

	/**
	 * Adds the columns of a column group (recursively, with a subtotal column after each subgroup).
	 *
	 * @param prefix key of the group
	 * @param n number of column fields
	 * @param children the keys of the subgroups per group
	 * @param columns to add the columns to
	 * @param complete if <code>true</code>, each group has a subtotal column (e.g. for an export with outline)
	 */
	private void addColumns(List<Object> prefix, int n, Map<List<Object>, Set<Object>> children, List<PivotColumn> columns, boolean complete) {
		int level = prefix.size();
		Set<Object> keys = children.get(prefix);
		if (keys == null) {
			return;
		}
		for (Object key: sortedKeys(keys)) {
			List<Object> subPrefix = new ArrayList<>(prefix);
			subPrefix.add(key);
			if (level == n - 1) {
				columns.add(new PivotColumn(subPrefix, false, 0));
			} else {
				addColumns(subPrefix, n, children, columns, complete);
				if (complete || showSubtotal(children.get(subPrefix))) {
					columns.add(new PivotColumn(subPrefix, true, 0));
				}
			}
		}
	}

	/**
	 * Whether to show the subtotal (line or column) of a group.
	 *
	 * @param subgroups the keys of the subgroups of the group
	 */
	private boolean showSubtotal(Set<Object> subgroups) {
		return singleElementTotalsCheckBox.isSelected() || subgroups == null || subgroups.size() > 1;
	}

	/**
	 * A value column of the pivot table: a value of either a group (all column fields restricted)
	 * or the subtotal/total of a group.
	 */
	private static class PivotColumn {
		final List<Object> prefix;
		final boolean isTotal;
		final int valueIndex;

		PivotColumn(List<Object> prefix, boolean isTotal, int valueIndex) {
			this.prefix = prefix;
			this.isTotal = isTotal;
			this.valueIndex = valueIndex;
		}
	}

	/**
	 * A line of the pivot table: either the rows of a group (all row fields restricted)
	 * or the subtotal/total of a group.
	 */
	private static class PivotLine {
		final List<Object> prefix;
		final boolean isTotal;
		final boolean collapsed; // subtotal line of a collapsed group (its subgroups are hidden)
		final boolean[] showLabel;

		PivotLine(List<Object> prefix, boolean isTotal) {
			this(prefix, isTotal, false);
		}

		PivotLine(List<Object> prefix, boolean isTotal, boolean collapsed) {
			this.prefix = prefix;
			this.isTotal = isTotal;
			this.collapsed = collapsed;
			this.showLabel = new boolean[prefix.size()];
		}
	}

	/**
	 * Keys of the collapsed row groups, and the row fields they belong to.
	 */
	private final Set<List<Object>> collapsedGroups = new HashSet<>();
	private List<String> collapsedGroupsRowFields = null;

	/**
	 * Collapses or expands a row group.
	 *
	 * @param r line of the pivot table
	 * @param c column of the label of the group
	 */
	private void toggleGroup(int r, int c) {
		if (shownLines == null || r >= shownLines.size()) {
			return;
		}
		List<Object> group = new ArrayList<>(shownLines.get(r).prefix.subList(0, c + 1));
		if (!collapsedGroups.remove(group)) {
			collapsedGroups.add(group);
		}
		saveConfig();
		updatePivot();
	}

	/**
	 * Selects the rows that make up a cell of the pivot table in the rows table
	 * and shows the "Rows" tab.
	 *
	 * @param r row of the pivot table
	 * @param c column of the pivot table
	 */
	private void drillDown(int r, int c) {
		if (currentTable == null || shownLines == null || r >= shownLines.size()) {
			return;
		}
		// grouping fields: row fields first, then column fields
		PivotLine line = shownLines.get(r);
		int n = shownRowsCols.length;
		List<Criterion> criteria = new ArrayList<>();
		for (int f = 0; f < n; f++) {
			criteria.add(new Criterion(shownRowsCols[f], f < line.prefix.size() ? line.prefix.get(f) : null)); // (sub)total: not all row fields restricted
		}
		PivotColumn column = c >= n && c - n < shownColumns.size() ? shownColumns.get(c - n) : null; // row field columns: no column criterion
		for (int f = 0; f < shownColumnsCols.length; f++) {
			criteria.add(new Criterion(shownColumnsCols[f], column != null && f < column.prefix.size() ? column.prefix.get(f) : null)); // (sub)total: not all column fields restricted
		}

		TableModel model = currentTable.getModel();
		for (Criterion criterion: criteria) {
			if (criterion.modelColumn >= model.getColumnCount()) {
				return;
			}
		}

		// sort by the restricted fields first, so that the rows of the cell are adjacent
		RowSorter<? extends TableModel> sorter = currentTable.getRowSorter();
		if (sorter != null) {
			List<SortKey> sortKeys = new ArrayList<>();
			for (Criterion criterion: criteria) {
				if (criterion.key != null) {
					sortKeys.add(new SortKey(criterion.modelColumn, SortOrder.ASCENDING));
				}
			}
			for (Criterion criterion: criteria) {
				if (criterion.key == null) {
					sortKeys.add(new SortKey(criterion.modelColumn, SortOrder.ASCENDING));
				}
			}
			sorter.setSortKeys(sortKeys);
		}

		int rowCount = sorter != null ? sorter.getViewRowCount() : model.getRowCount();
		List<Integer> viewRows = new ArrayList<>();
		for (int i = 0; i < rowCount; i++) {
			int row = sorter != null ? sorter.convertRowIndexToModel(i) : i;
			boolean matches = true;
			for (Criterion criterion: criteria) {
				if (criterion.key != null && !criterion.key.equals(toKey(rowValue(row, criterion.modelColumn)))) {
					matches = false;
					break;
				}
			}
			if (matches) {
				// rows filtered out don't belong to the cell
				for (Map.Entry<Integer, Set<Object>> e: shownExcluded.entrySet()) {
					if (e.getValue().contains(toKey(rowValue(row, e.getKey())))) {
						matches = false;
						break;
					}
				}
			}
			if (matches) {
				viewRows.add(i);
			}
		}

		currentTable.clearSelection();
		if (!viewRows.isEmpty() && currentTable.getColumnCount() > 0) {
			currentTable.setColumnSelectionInterval(0, currentTable.getColumnCount() - 1);
			int start = viewRows.get(0);
			int end = start;
			for (int i = 1; i <= viewRows.size(); i++) {
				if (i < viewRows.size() && viewRows.get(i) == end + 1) {
					end = viewRows.get(i);
				} else {
					currentTable.addRowSelectionInterval(start, end);
					if (i < viewRows.size()) {
						start = viewRows.get(i);
						end = start;
					}
				}
			}
		}
		if (showRowsTabAction != null) {
			showRowsTabAction.run();
		}
		if (!viewRows.isEmpty()) {
			int first = viewRows.get(0);
			UIUtil.invokeLater(() -> currentTable.scrollRectToVisible(currentTable.getCellRect(first, 0, true)));
		}
	}

	/**
	 * Gets a value of a loaded row (as read from the result set).
	 *
	 * @param row model index of the row
	 * @param column model index of the column
	 */
	private Object rowValue(int row, int column) {
		List<Row> rows = rowBrowser == null ? null : rowBrowser.rows;
		if (rows == null || row >= rows.size() || column >= rows.get(row).values.length) {
			return null;
		}
		return rows.get(row).values[column];
	}

	/**
	 * Grouping field of the drilldown and the key of the double-clicked cell.
	 */
	private static class Criterion {
		final int modelColumn;
		final Object key; // null: field is not restricted (total)

		Criterion(int modelColumn, Object key) {
			this.modelColumn = modelColumn;
			this.key = key;
		}
	}

	/**
	 * Enables aggregating all rows of the statement if the row limit is exceeded.
	 *
	 * @param statement gets the statement to execute again, or <code>null</code> if it must not be executed again
	 * @param session the session of the result
	 * @param executor executes the reading (in the thread and transaction of the SQL Console)
	 */
	public void setAllRowsSource(Supplier<String> statement, Session session, Consumer<Runnable> executor) {
		this.allRowsStatement = statement;
		this.allRowsSession = session;
		this.allRowsExecutor = executor;
	}

	/**
	 * Enables generating the SQL of the pivot table.
	 *
	 * @param resultColumnLabels the labels of the columns of the result, as returned by the JDBC driver
	 * @param sqlConsole the SQL Console the SQL can be appended to
	 * @param executionContext the execution context
	 */
	public void setSqlSource(List<String> resultColumnLabels, SQLConsole sqlConsole, ExecutionContext executionContext) {
		this.resultColumnLabels = resultColumnLabels;
		this.sqlConsole = sqlConsole;
		this.executionContext = executionContext;
		updateButtons();
	}

	private static final int MAX_FILTER_VALUES = 1000;

	private boolean isFieldFiltered(String field) {
		Set<Object> excludedKeys = fieldFilters.get(field);
		return excludedKeys != null && !excludedKeys.isEmpty();
	}

	/**
	 * Lets the user choose the values of a field to show (the rows with the other values are filtered out).
	 *
	 * @param field name of the field
	 * @param anchor the filter button
	 */
	private void editFilter(String field, JComponent anchor) {
		int view = columnNames.indexOf(field);
		Map<Object, Object> values = null;
		if (currentTable != null && shownAggregation != null && view >= 0 && view < currentTable.getColumnModel().getColumnCount()) {
			values = shownAggregation.fieldValues.get(currentTable.getColumnModel().getColumn(view).getModelIndex());
		}
		if (values == null || values.isEmpty()) {
			JOptionPane.showMessageDialog(anchor, "No values.", "Filter", JOptionPane.INFORMATION_MESSAGE);
			return;
		}
		List<Object> keys = sortedKeys(values.keySet());
		Set<Object> excludedKeys = fieldFilters.getOrDefault(field, Collections.emptySet());

		JPanel valuesPanel = new JPanel();
		valuesPanel.setLayout(new BoxLayout(valuesPanel, BoxLayout.Y_AXIS));
		Map<Object, JCheckBox> checkBoxes = new LinkedHashMap<>();
		for (Object key: keys.subList(0, Math.min(keys.size(), MAX_FILTER_VALUES))) {
			JCheckBox checkBox = new JCheckBox(String.valueOf(key), !excludedKeys.contains(key));
			checkBoxes.put(key, checkBox);
			valuesPanel.add(checkBox);
		}
		JCheckBox allCheckBox = new JCheckBox("(All)", checkBoxes.values().stream().allMatch(JCheckBox::isSelected));
		allCheckBox.addActionListener(e -> checkBoxes.values().forEach(cb -> cb.setSelected(allCheckBox.isSelected())));

		JPopupMenu popup = new JPopupMenu();
		JPanel panel = new JPanel(new BorderLayout(0, 4));
		panel.setBorder(BorderFactory.createEmptyBorder(4, 4, 4, 4));
		panel.add(allCheckBox, BorderLayout.NORTH);
		JScrollPane scrollPane = new JScrollPane(valuesPanel);
		scrollPane.getVerticalScrollBar().setUnitIncrement(16);
		scrollPane.setPreferredSize(new Dimension(Math.max(220, Math.min(400, valuesPanel.getPreferredSize().width + 30)), Math.min(300, valuesPanel.getPreferredSize().height + 6)));
		panel.add(scrollPane, BorderLayout.CENTER);
		JPanel south = new JPanel(new BorderLayout());
		if (keys.size() > MAX_FILTER_VALUES) {
			south.add(new JLabel("Only the first " + MAX_FILTER_VALUES + " of " + keys.size() + " values"), BorderLayout.NORTH);
		}
		JPanel buttons = new JPanel(new GridLayout(1, 2, 4, 0));
		JButton ok = new JButton("OK");
		JButton cancel = new JButton("Cancel");
		buttons.add(ok);
		buttons.add(cancel);
		south.add(buttons, BorderLayout.EAST);
		panel.add(south, BorderLayout.SOUTH);
		popup.add(panel);

		ok.addActionListener(e -> {
			popup.setVisible(false);
			Set<Object> newExcludedKeys = new HashSet<>();
			for (Object key: keys) {
				JCheckBox checkBox = checkBoxes.get(key);
				if (checkBox != null ? !checkBox.isSelected() : excludedKeys.contains(key)) {
					newExcludedKeys.add(key);
				}
			}
			if (newExcludedKeys.isEmpty()) {
				fieldFilters.remove(field);
			} else {
				fieldFilters.put(field, newExcludedKeys);
			}
			saveConfig();
			rowsFieldList.refresh();
			columnsFieldList.refresh();
			updatePivot();
		});
		cancel.addActionListener(e -> popup.setVisible(false));
		popup.show(anchor, 0, anchor.getHeight());
	}

	/**
	 * Gets the pivot table shown as text, for exporting it: the header, and one line per line shown
	 * (the labels of the groups are repeated in each line, numbers have the decimal separator of the locale).
	 *
	 * @return the lines, or <code>null</code> if there is no pivot table
	 */
	private List<String[]> exportLines() {
		if (shownLines == null || shownHeaders == null || shownRowsCols == null) {
			return null;
		}
		int n = shownRowsCols.length;
		TableModel model = resultTable.getModel();
		char decimalSeparator = DecimalFormatSymbols.getInstance().getDecimalSeparator();
		List<String[]> result = new ArrayList<>();
		result.add(shownHeaders.toArray(new String[0]));
		for (int r = 0; r < shownLines.size() && r < model.getRowCount(); r++) {
			PivotLine line = shownLines.get(r);
			String[] cells = new String[shownHeaders.size()];
			Arrays.fill(cells, "");
			int k = line.prefix.size();
			for (int f = 0; f < k; f++) {
				cells[f] = String.valueOf(line.prefix.get(f));
			}
			if (line.isTotal && !line.collapsed && k < n) {
				cells[k] = "Total";
			}
			for (int c = n; c < cells.length && c < model.getColumnCount(); c++) {
				Object value = model.getValueAt(r, c);
				if (value instanceof Percentage) {
					// e.g. "25,5%" (read as percentage by spreadsheet applications)
					cells[c] = BigDecimal.valueOf(((Percentage) value).ratio * 100).round(new MathContext(10)).stripTrailingZeros().toPlainString().replace('.', decimalSeparator) + "%";
					continue;
				}
				if (value instanceof Double || value instanceof Float) {
					double d = ((Number) value).doubleValue();
					value = Double.isNaN(d) || Double.isInfinite(d) ? String.valueOf(d) : BigDecimal.valueOf(d);
				}
				if (value instanceof BigDecimal) {
					cells[c] = ((BigDecimal) value).toPlainString().replace('.', decimalSeparator);
				} else if (value != null) {
					cells[c] = String.valueOf(value);
				}
			}
			result.add(cells);
		}
		return result;
	}

	/**
	 * Joins the cells of the lines, quoting cells that contain the separator, quotes or line breaks (RFC 4180).
	 */
	private static String join(List<String[]> lines, char separator) {
		StringBuilder sb = new StringBuilder();
		for (String[] line: lines) {
			for (int i = 0; i < line.length; i++) {
				if (i > 0) {
					sb.append(separator);
				}
				String cell = line[i];
				if (cell.indexOf(separator) >= 0 || cell.indexOf('"') >= 0 || cell.indexOf('\n') >= 0 || cell.indexOf('\r') >= 0) {
					cell = "\"" + cell.replace("\"", "\"\"") + "\"";
				}
				sb.append(cell);
			}
			sb.append("\r\n");
		}
		return sb.toString();
	}

	/**
	 * Copies the pivot table (tab separated, with header) to the clipboard.
	 */
	private void copyToClipboard() {
		List<String[]> lines = exportLines();
		if (lines != null) {
			UIUtil.setClipboardContent(new StringSelection(join(lines, '\t')));
		}
	}

	/**
	 * Saves the pivot table as CSV file (";" separated, with header, UTF-8 with BOM, like a spreadsheet application expects it).
	 */
	private void saveAsCsv() {
		List<String[]> lines = exportLines();
		if (lines == null) {
			return;
		}
		String fileName = withExtension(UIUtil.choseFile(null, ".", "CSV File", ".csv", this, true, false), ".csv");
		if (fileName == null) {
			return;
		}
		try (Writer out = new OutputStreamWriter(new FileOutputStream(fileName), StandardCharsets.UTF_8)) {
			out.write('﻿');
			out.write(join(lines, ';'));
		} catch (Throwable t) {
			UIUtil.showException(this, "Error", t);
		}
	}

	/**
	 * Appends the extension to a file name if it doesn't have it.
	 *
	 * @param fileName the file name, or <code>null</code>
	 * @param extension the extension (e.g. ".csv")
	 */
	private static String withExtension(String fileName, String extension) {
		if (fileName == null || fileName.toLowerCase(Locale.ENGLISH).endsWith(extension)) {
			return fileName;
		}
		return fileName + extension;
	}

	/**
	 * Saves the pivot table as Excel file (.xlsx): the sheet "Data" contains the rows (only the columns used, without the rows filtered out),
	 * and the cells of the sheet "Pivot" are formulas over them (except "Count distinct").
	 */
	private void saveAsXlsx() {
		if (shownAggregation == null || shownLines == null) {
			return;
		}
		String fileName = withExtension(UIUtil.choseFile(null, ".", "Excel File", ".xlsx", this, true, false), ".xlsx");
		if (fileName == null) {
			return;
		}
		File file = new File(fileName);
		boolean written = false;
		try (SimpleXlsxWriter writer = new SimpleXlsxWriter(new BufferedOutputStream(new FileOutputStream(file)), Arrays.asList("PivotTable", "Pivot", "Data"))) {
			written = writeXlsx(writer);
		} catch (Throwable t) {
			UIUtil.showException(this, "Error", t);
		}
		if (!written) {
			file.delete();
		}
	}

	/**
	 * Writes the sheets of the Excel file.
	 *
	 * @return <code>false</code> if cancelled or failed (the error is shown)
	 */
	private boolean writeXlsx(SimpleXlsxWriter writer) throws IOException {
		int n = shownRowsCols.length;
		int nc = shownColumnsCols.length;

		// the columns of the sheet "Data": row fields, column fields and values
		List<Integer> dataCols = new ArrayList<>();
		List<String> dataNames = new ArrayList<>();
		Set<Integer> fieldCols = new HashSet<>();
		for (int f = 0; f < n + nc; f++) {
			int col = f < n ? shownRowsCols[f] : shownColumnsCols[f - n];
			fieldCols.add(col);
			if (!dataCols.contains(col)) {
				dataCols.add(col);
				dataNames.add(f < n ? shownHeaders.get(f) : shownColumnFields.get(f - n));
			}
		}
		for (int v = 0; v < shownValueCols.length; v++) {
			if (shownValueCols[v] >= 0 && !dataCols.contains(shownValueCols[v])) {
				dataCols.add(shownValueCols[v]);
				dataNames.add(shownValueNames.get(v));
			}
		}

		// the rows are counted in the Excel pivot table as sum of a column with 1 in each row
		boolean rowsColumn = Arrays.stream(shownValueCols).anyMatch(c -> c < 0);
		List<String> fieldNames = new ArrayList<>(dataNames);
		if (rowsColumn) {
			fieldNames.add(ROWS);
		}
		for (int i = 0; i < fieldNames.size(); i++) {
			// the names of the fields of an Excel pivot table must be unique
			String name = fieldNames.get(i);
			for (int suffix = 2; fieldNames.subList(0, i).contains(fieldNames.get(i)); suffix++) {
				fieldNames.set(i, name + "_" + suffix);
			}
		}

		writer.beginSheet(2);
		writer.row(fieldNames.stream().map(name -> SimpleXlsxWriter.Cell.text(name, true)).collect(Collectors.toList()));
		Map<Integer, Set<Object>> excluded = shownExcluded;
		Consumer<Object[]> rowWriter = values -> {
			for (Map.Entry<Integer, Set<Object>> e: excluded.entrySet()) {
				if (e.getValue().contains(toKey(e.getKey() < values.length ? values[e.getKey()] : null))) {
					return; // filtered out
				}
			}
			List<SimpleXlsxWriter.Cell> cells = new ArrayList<>();
			for (int col: dataCols) {
				Object raw = col < values.length ? values[col] : null;
				if (fieldCols.contains(col)) {
					Object key = toKey(raw);
					cells.add(key == NULL_KEY ? null : keyCell(key, false));
				} else {
					Object value = normalize(raw);
					BigDecimal number = toBigDecimal(value);
					cells.add(value == null ? null : number != null ? SimpleXlsxWriter.Cell.number(number, false) : SimpleXlsxWriter.Cell.text(String.valueOf(value), false));
				}
			}
			if (rowsColumn) {
				cells.add(SimpleXlsxWriter.Cell.number(1, false));
			}
			try {
				writer.row(cells);
			} catch (IOException e) {
				throw new UncheckedIOException(e);
			}
		};
		String allRowsSql = shownBasedOnAllRows && allRowsStatement != null ? allRowsStatement.get() : null;
		if (allRowsSql != null) {
			if (!streamAllRows(allRowsSql, dataCols::contains, rowWriter)) {
				return false;
			}
		} else {
			for (Row row: rowBrowser.rows) {
				rowWriter.accept(row.values);
			}
		}
		int lastDataRow = writer.getRowNumber();
		writer.setAutoFilter(1, 0, lastDataRow, fieldNames.size() - 1);
		writer.endSheet();

		// the sheet "PivotTable": an Excel pivot table over the sheet "Data" (built by the application when opening the workbook)
		List<String[]> dataFields = new ArrayList<>();
		Set<String> dataFieldNames = new HashSet<>(fieldNames);
		for (int v = 0; v < shownValues.size(); v++) {
			ValueSpec value = shownValues.get(v);
			if (COUNT_DISTINCT.equals(value.aggregate)) {
				continue; // not supported by Excel pivot tables
			}
			String name = value.header(shownValueNames.get(v));
			for (int suffix = 2; dataFieldNames.contains(name); suffix++) {
				name = value.header(shownValueNames.get(v)) + "_" + suffix;
			}
			dataFieldNames.add(name);
			int field = shownValueCols[v] < 0 ? dataCols.size() : dataCols.indexOf(shownValueCols[v]);
			String function = shownValueCols[v] < 0 ? "sum"
					: SUM.equals(value.aggregate) ? "sum" : AVG.equals(value.aggregate) ? "average" : MIN.equals(value.aggregate) ? "min" : MAX.equals(value.aggregate) ? "max" : "count";
			String showDataAs = PERCENT_OF_ROW.equals(value.showAs) ? "percentOfRow" : PERCENT_OF_COLUMN.equals(value.showAs) ? "percentOfCol" : PERCENT_OF_TOTAL.equals(value.showAs) ? "percentOfTotal" : null;
			dataFields.add(new String[] { name, String.valueOf(field), function, showDataAs });
		}
		writer.beginSheet(0);
		boolean withPivotTable = lastDataRow >= 2 && !dataFields.isEmpty();
		String note = withPivotTable ? "Excel PivotTable over the sheet \"Data\"" : "No Excel PivotTable (no rows or values)";
		if (dataFields.size() < shownValues.size()) {
			note += ". \"" + COUNT_DISTINCT + "\" is only in the sheet \"Pivot\".";
		}
		writer.row(Collections.singletonList(SimpleXlsxWriter.Cell.text(note, false)));
		writer.endSheet();
		if (withPivotTable) {
			List<Integer> rowFields = new ArrayList<>();
			for (int col: shownRowsCols) {
				rowFields.add(dataCols.indexOf(col));
			}
			List<Integer> columnFields = new ArrayList<>();
			for (int col: shownColumnsCols) {
				columnFields.add(dataCols.indexOf(col));
			}
			writer.addPivotTable(0, 2, "A1:" + SimpleXlsxWriter.columnName(fieldNames.size() - 1) + lastDataRow, fieldNames, rowFields, columnFields, dataFields);
		}

		// the sheet "Pivot", with all lines and columns (also of collapsed groups, and each group with subtotal), so that the groups can be collapsed and expanded in the outline
		Aggregation aggregation = shownAggregation;
		List<PivotLine> lines = new ArrayList<>();
		addLines(new ArrayList<>(), n, aggregation.rowChildren, new boolean[n], lines, shownLineOrder, true);
		lines.add(new PivotLine(new ArrayList<>(), true));
		List<PivotColumn> groups = new ArrayList<>();
		if (nc > 0) {
			addColumns(new ArrayList<>(), nc, aggregation.columnChildren, groups, true);
			groups.add(new PivotColumn(new ArrayList<>(), true, 0));
		} else {
			groups.add(new PivotColumn(new ArrayList<>(), false, 0));
		}
		List<PivotColumn> columns = new ArrayList<>();
		for (PivotColumn group: groups) {
			for (int v = 0; v < shownValues.size(); v++) {
				columns.add(new PivotColumn(group.prefix, group.isTotal, v));
			}
		}
		// outline levels: groups of the last field have the highest level, the (sub)totals the level of their group
		List<int[]> columnOutline = new ArrayList<>();
		for (int c = 0; c < columns.size(); c++) {
			PivotColumn column = columns.get(c);
			int level = !column.isTotal ? nc - 1 : Math.max(0, column.prefix.size() - 1);
			columnOutline.add(new int[] { n + c, level });
		}

		boolean noData = lastDataRow < 2;
		Function<Integer, String> range = col -> {
			String column = SimpleXlsxWriter.columnName(dataCols.indexOf(col));
			return "Data!$" + column + "$2:$" + column + "$" + lastDataRow;
		};
		int headerRows = nc + 1;
		writer.beginSheet(1, Math.max(0, n - 1), Math.max(0, nc - 1), columnOutline);
		for (int j = 0; j < nc; j++) {
			List<SimpleXlsxWriter.Cell> cells = new ArrayList<>();
			for (int f = 0; f < n; f++) {
				cells.add(f == n - 1 ? SimpleXlsxWriter.Cell.text(shownColumnFields.get(j), true) : null);
			}
			for (PivotColumn column: columns) {
				if (j < column.prefix.size()) {
					cells.add(keyCell(column.prefix.get(j), true));
				} else if (column.isTotal && j == column.prefix.size()) {
					cells.add(SimpleXlsxWriter.Cell.text("Total", true));
				} else {
					cells.add(null);
				}
			}
			writer.row(cells);
		}
		List<SimpleXlsxWriter.Cell> headerCells = new ArrayList<>();
		for (int f = 0; f < n; f++) {
			headerCells.add(SimpleXlsxWriter.Cell.text(shownHeaders.get(f), true));
		}
		for (int c = 0; c < columns.size(); c++) {
			ValueSpec value = shownValues.get(columns.get(c).valueIndex);
			String header = value.header(shownValueNames.get(columns.get(c).valueIndex));
			headerCells.add(SimpleXlsxWriter.Cell.text(header, true));
		}
		writer.row(headerCells);

		for (int i = 0; i < lines.size(); i++) {
			PivotLine line = lines.get(i);
			int rowNumber = headerRows + 1 + i;
			boolean bold = line.isTotal;
			List<SimpleXlsxWriter.Cell> cells = new ArrayList<>();
			int k = line.prefix.size();
			// outline: hidden in a collapsed group, the summary line of a collapsed group is marked as collapsed
			int outlineLevel = !line.isTotal ? n - 1 : Math.max(0, k - 1);
			boolean hidden = false;
			for (int p = 1; p < k; p++) {
				hidden |= collapsedGroups.contains(line.prefix.subList(0, p));
			}
			boolean collapsed = line.isTotal && k > 0 && collapsedGroups.contains(line.prefix);
			for (int f = 0; f < n; f++) {
				if (f < k) {
					cells.add(keyCell(line.prefix.get(f), true));
				} else if (line.isTotal && f == k) {
					cells.add(SimpleXlsxWriter.Cell.text("Total", true));
				} else {
					cells.add(null);
				}
			}
			Map<List<Object>, Accumulator[]> lineCells = aggregation.cells.get(line.prefix);
			for (int c = 0; c < columns.size(); c++) {
				PivotColumn column = columns.get(c);
				ValueSpec value = shownValues.get(column.valueIndex);
				Object cached = lineCells == null ? null : cellValue(aggregation.cells, line.prefix, column, value);
				boolean cellBold = bold || column.isTotal;
				boolean percent = value.isPercentage();
				if (noData || COUNT_DISTINCT.equals(value.aggregate)) {
					cells.add(cached instanceof Number ? SimpleXlsxWriter.Cell.number(cached instanceof Percentage ? (Number) ((Percentage) cached).ratio : toBigDecimal(cached), cellBold, percent) : SimpleXlsxWriter.Cell.empty(cellBold));
					continue;
				}
				String[] formula = xlsxFormula(line, column, c, rowNumber, true, true, value, range, dataCols);
				if (percent) {
					// divided by the total of the line, of the column or the grand total
					String[] total = xlsxFormula(line, column, c, rowNumber, !PERCENT_OF_COLUMN.equals(value.showAs) && !PERCENT_OF_TOTAL.equals(value.showAs),
							!PERCENT_OF_ROW.equals(value.showAs) && !PERCENT_OF_TOTAL.equals(value.showAs), value, range, dataCols);
					formula = new String[] { "IFERROR((" + formula[0] + ")/(" + total[0] + "),\"\")", formula[1] != null || total[1] != null ? "array" : null };
				}
				Object cachedValue = cached instanceof Percentage ? (Object) ((Percentage) cached).ratio : cached instanceof Number ? toBigDecimal(cached) : cached;
				cells.add(SimpleXlsxWriter.Cell.formula(formula[0], cachedValue, formula[1] != null, cellBold, percent));
			}
			writer.row(cells, outlineLevel, hidden, collapsed);
		}
		writer.setAutoFilter(headerRows, 0, writer.getRowNumber(), n + columns.size() - 1);
		writer.endSheet();
		return true;
	}

	/**
	 * Gets the Excel formula of a cell of the pivot table, over the sheet "Data".
	 *
	 * @param line the line
	 * @param column the column
	 * @param c index of the column (among the value columns)
	 * @param rowNumber number of the row of the line in the sheet (the labels of its groups are in it)
	 * @param withRowConditions restrict to the group of the line?
	 * @param withColumnConditions restrict to the group of the column?
	 * @param range gets the range of a column (model index) in the sheet "Data"
	 * @param dataCols the columns (model indexes) of the sheet "Data"
	 * @return the formula and, if it is an array formula, a non-<code>null</code> second element
	 */
	private String[] xlsxFormula(PivotLine line, PivotColumn column, int c, int rowNumber, boolean withRowConditions, boolean withColumnConditions, ValueSpec value,
			Function<Integer, String> range, List<Integer> dataCols) {
		int n = shownRowsCols.length;
		// conditions of the restricted fields (on the label cells)
		List<String> conditions = new ArrayList<>();
		if (withRowConditions) {
			for (int f = 0; f < line.prefix.size(); f++) {
				Object key = line.prefix.get(f);
				conditions.add("(" + range.apply(shownRowsCols[f]) + "=" + (key == NULL_KEY ? "\"\"" : "$" + SimpleXlsxWriter.columnName(f) + "$" + rowNumber) + ")");
			}
		}
		if (withColumnConditions) {
			for (int j = 0; j < column.prefix.size(); j++) {
				Object key = column.prefix.get(j);
				conditions.add("(" + range.apply(shownColumnsCols[j]) + "=" + (key == NULL_KEY ? "\"\"" : "$" + SimpleXlsxWriter.columnName(n + c) + "$" + (j + 1)) + ")");
			}
		}
		int valueCol = shownValueCols[column.valueIndex];
		String values = valueCol >= 0 ? range.apply(valueCol) : null;
		String args = conditions.stream().map(cond -> "--" + cond).collect(Collectors.joining(","));
		String formula;
		boolean array = false;
		if (values == null) {
			formula = conditions.isEmpty() ? "ROWS(" + range.apply(dataCols.get(0)) + ")" : "SUMPRODUCT(" + args + ")";
		} else {
			String sum = conditions.isEmpty() ? "SUM(" + values + ")" : "SUMPRODUCT(" + args + "," + values + ")";
			String numbers = conditions.isEmpty() ? "COUNT(" + values + ")" : "SUMPRODUCT(" + args + ",--ISNUMBER(" + values + "))";
			switch (value.aggregate) {
				case SUM:
					formula = sum;
					break;
				case AVG:
					formula = "IFERROR(" + sum + "/" + numbers + ",\"\")";
					break;
				case MIN:
				case MAX:
					String function = MIN.equals(value.aggregate) ? "MIN" : "MAX";
					if (conditions.isEmpty()) {
						formula = "IF(" + numbers + "=0,\"\"," + function + "(" + values + "))";
					} else {
						formula = "IF(" + numbers + "=0,\"\"," + function + "(IF(" + String.join("*", conditions) + "*ISNUMBER(" + values + ")," + values + ")))";
						array = true;
					}
					break;
				default: // COUNT
					formula = conditions.isEmpty() ? "SUMPRODUCT(--(" + values + "<>\"\"))" : "SUMPRODUCT(" + args + ",--(" + values + "<>\"\"))";
			}
		}
		return new String[] { formula, array ? "array" : null };
	}

	/**
	 * Gets the cell of a key: numbers as numbers, the other values as text (like shown in the pivot table).
	 */
	private static SimpleXlsxWriter.Cell keyCell(Object key, boolean bold) {
		if (key instanceof Number) {
			BigDecimal number = toBigDecimal(key);
			if (number != null) {
				return SimpleXlsxWriter.Cell.number(number, bold);
			}
		}
		return SimpleXlsxWriter.Cell.text(String.valueOf(key), bold);
	}

	/**
	 * Collapses or expands all row groups (that have subgroups).
	 */
	private void collapseAll(boolean collapse) {
		collapsedGroups.clear();
		if (collapse && shownAggregation != null) {
			for (List<Object> group: shownAggregation.rowChildren.keySet()) {
				if (!group.isEmpty()) {
					collapsedGroups.add(new ArrayList<>(group));
				}
			}
		}
		saveConfig();
		updatePivot();
	}

	private void updateButtons() {
		boolean hasGroups = shownAggregation != null && shownRowsCols != null && shownRowsCols.length > 1;
		boolean allCollapsed = true;
		if (hasGroups) {
			for (List<Object> group: shownAggregation.rowChildren.keySet()) {
				if (!group.isEmpty() && !collapsedGroups.contains(group)) {
					allCollapsed = false;
					break;
				}
			}
		}
		collapseAllButton.setEnabled(hasGroups && !allCollapsed);
		expandAllButton.setEnabled(hasGroups && !collapsedGroups.isEmpty());
		copyButton.setEnabled(shownAggregation != null);
		saveCsvButton.setEnabled(shownAggregation != null);
		saveXlsxButton.setEnabled(shownAggregation != null);
		String reason = null;
		if (resultColumnLabels == null || sqlConsole == null) {
			reason = "No SQL can be generated for this result.";
		} else if (shownAggregation == null) {
			reason = "No pivot table.";
		} else if (allRowsStatement == null || allRowsStatement.get() == null) {
			reason = "Only for queries (statements starting with SELECT or WITH).";
		}
		sqlButton.setEnabled(reason == null);
		sqlButton.setToolTipText(reason == null ? "Generates the SQL statement that computes this pivot table." : reason);
	}

	/**
	 * Shows the SQL of the pivot table in a dialog.
	 */
	private void showSql() {
		String sql;
		try {
			sql = generateSql();
		} catch (Throwable t) {
			UIUtil.showException(this, "Error", t);
			return;
		}
		if (sql == null) {
			return;
		}
		Window owner = SwingUtilities.getWindowAncestor(this);
		JDialog dialog = new JDialog(owner, "Pivot SQL", Dialog.ModalityType.APPLICATION_MODAL);
		SQLDMLPanel panel = new SQLDMLPanel(sql, sqlConsole, allRowsSession, sqlConsole.metaDataSource, () -> {}, () -> {}, dialog, executionContext);
		panel.setExecutable(false);
		dialog.getContentPane().add(panel);
		dialog.pack();
		dialog.setSize(800, Math.max(dialog.getHeight() + 20, 400));
		if (owner != null) {
			dialog.setLocation(owner.getX() + (owner.getWidth() - dialog.getWidth()) / 2, Math.max((int) UIUtil.getScreenBounds().getY(), owner.getY() + (owner.getHeight() - dialog.getHeight()) / 2));
		}
		UIUtil.fit(dialog);
		dialog.setVisible(true);
	}

	/**
	 * Generates the SQL statement that computes the pivot table currently shown:
	 * one column per column group and value (with CASE), and one UNION ALL part per level of the row groups (for the subtotals).
	 *
	 * @return the SQL, or <code>null</code> if there is none
	 */
	private String generateSql() throws SQLException {
		String statement = allRowsStatement == null ? null : allRowsStatement.get();
		if (statement == null || shownAggregation == null || shownValues == null) {
			return null;
		}
		Quoting quoting = Quoting.getQuoting(allRowsSession);
		CellContentConverter converter = new CellContentConverter(null, allRowsSession, allRowsSession.dbms);
		int nr = shownRowsCols.length;
		int nc = shownColumnsCols.length;

		// Columns with the same label (e.g. "select * from A join B") can neither be referred to
		// nor be in a derived table. They are renamed (DEPTNO, DEPTNO_2) by rewriting the select list of the statement,
		// with the columns used by the pivot table only.
		sqlColumnNames = new ArrayList<>();
		Set<String> usedNames = new HashSet<>();
		Set<String> collisions = new LinkedHashSet<>();
		for (int i = 0; i < resultColumnLabels.size(); i++) {
			String label = resultColumnLabels.get(i);
			String base = label == null || label.trim().isEmpty() ? "C" + (i + 1) : label.trim();
			String name = base;
			for (int n = 2; usedNames.contains(name.toUpperCase(Locale.ENGLISH)); n++) {
				name = base + "_" + n;
			}
			if (!name.equals(base)) {
				collisions.add(base);
			}
			usedNames.add(name.toUpperCase(Locale.ENGLISH));
			sqlColumnNames.add(name);
		}
		String innerStatement = statement.trim();
		boolean rewritten = false;
		if (!collisions.isEmpty()) {
			QueryTypeAnalyser.SelectList selectList = QueryTypeAnalyser.getSelectList(statement, sqlConsole.metaDataSource);
			if (selectList != null && selectList.columns.size() == sqlColumnNames.size()) {
				Set<Integer> usedColumns = new TreeSet<>();
				if (selectList.distinct) {
					// fewer columns would change the result of "distinct"
					for (int i = 0; i < sqlColumnNames.size(); i++) {
						usedColumns.add(i);
					}
				} else {
					Arrays.stream(shownRowsCols).forEach(usedColumns::add);
					Arrays.stream(shownColumnsCols).forEach(usedColumns::add);
					Arrays.stream(shownValueCols).filter(c -> c >= 0).forEach(usedColumns::add);
				}
				StringBuilder list = new StringBuilder();
				for (int i: usedColumns) {
					String[] column = selectList.columns.get(i);
					list.append(list.length() > 0 ? ", " : "")
						.append(column.length == 1 ? column[0] : column[0] + "." + quoting.quote(column[1]))
						.append(" as ").append(quoting.quote(sqlColumnNames.get(i)));
				}
				innerStatement = (selectList.sql.substring(0, selectList.start) + list + selectList.sql.substring(selectList.end)).trim();
				rewritten = true;
			}
		}
		if (!rewritten) {
			// refer to the columns as they are
			for (int i = 0; i < sqlColumnNames.size(); i++) {
				String label = resultColumnLabels.get(i);
				sqlColumnNames.set(i, label == null ? "?" : label);
			}
		}

		// the filters of the fields
		List<String> conditions = new ArrayList<>();
		for (Map.Entry<Integer, Set<Object>> e: shownExcluded.entrySet()) {
			String ref = quoting.quote(label(e.getKey()));
			Map<Object, Object> rawValues = shownAggregation.fieldValues.getOrDefault(e.getKey(), Collections.emptyMap());
			boolean nullExcluded = e.getValue().contains(NULL_KEY);
			List<String> literals = new ArrayList<>();
			for (Object key: sortedKeys(e.getValue())) {
				if (key != NULL_KEY) {
					Object raw = rawValues.get(key);
					literals.add(converter.toSql(raw != null ? raw : key));
				}
			}
			String notIn = literals.isEmpty() ? null : ref + " not in (" + String.join(", ", literals) + ")";
			if (notIn == null) {
				conditions.add(ref + " is not null");
			} else if (nullExcluded) {
				conditions.add(ref + " is not null and " + notIn);
			} else {
				conditions.add("(" + ref + " is null or " + notIn + ")");
			}
		}

		// the value columns, one per column of the pivot table
		List<String> valueExpressions = new ArrayList<>();
		for (int c = 0; c < shownColumns.size(); c++) {
			PivotColumn column = shownColumns.get(c);
			StringBuilder condition = new StringBuilder();
			for (int f = 0; f < column.prefix.size(); f++) {
				Object key = column.prefix.get(f);
				String ref = quoting.quote(label(shownColumnsCols[f]));
				if (condition.length() > 0) {
					condition.append(" and ");
				}
				if (key == NULL_KEY) {
					condition.append(ref).append(" is null");
				} else {
					Object raw = shownAggregation.rawColumnKeys.get(f).get(key);
					condition.append(ref).append(" = ").append(converter.toSql(raw != null ? raw : key));
				}
			}
			ValueSpec value = shownValues.get(column.valueIndex);
			int valueCol = shownValueCols[column.valueIndex];
			String expression = aggregateExpression(value, valueCol, condition.length() == 0 ? null : condition.toString(), quoting);
			if (value.isPercentage()) {
				// in percent of the total of the line (same group), of the column or of all rows (subqueries)
				String total;
				if (PERCENT_OF_ROW.equals(value.showAs)) {
					total = aggregateExpression(value, valueCol, null, quoting);
				} else {
					String totalCondition = PERCENT_OF_COLUMN.equals(value.showAs) && condition.length() > 0 ? condition.toString() : null;
					total = "(Select " + aggregateExpression(value, valueCol, totalCondition, quoting) + " From (" + innerStatement + ") q2"
							+ (conditions.isEmpty() ? "" : " Where " + String.join(" and ", conditions)) + ")";
				}
				expression = "100.0 * " + expression + " / nullif(" + total + ", 0)";
			}
			valueExpressions.add(expression + " as " + quoting.requote(shownHeaders.get(nr + c), true));
		}

		String from = "From (" + innerStatement + ") q";
		StringBuilder sql = new StringBuilder();
		sql.append("-- Pivot table");
		if (nc > 0) {
			sql.append(" (with the column values currently shown)");
		}
		sql.append("\n");
		if (!rewritten) {
			for (String label: collisions) {
				sql.append("-- Note: the statement has more than one column \"").append(label).append("\". Please give them different aliases.\n");
			}
		}
		// the lines of the groups (without the subtotal and total lines)
		StringBuilder rowFieldList = new StringBuilder();
		for (int f = 0; f < nr; f++) {
			rowFieldList.append(f > 0 ? ", " : "").append(quoting.quote(label(shownRowsCols[f])));
		}
		sql.append("Select ");
		for (int f = 0; f < nr; f++) {
			sql.append(quoting.quote(label(shownRowsCols[f]))).append(" as ").append(quoting.requote(shownHeaders.get(f), true)).append(", ");
		}
		sql.append("\n    ").append(String.join(",\n    ", valueExpressions));
		sql.append("\n").append(from);
		if (!conditions.isEmpty()) {
			sql.append("\nWhere ").append(String.join("\n  and ", conditions));
		}
		sql.append("\nGroup by ").append(rowFieldList);
		sql.append("\nOrder by ").append(rowFieldList);
		return sql.toString();
	}

	/**
	 * Names of the columns of the result in the generated SQL (the labels, made unique).
	 */
	private List<String> sqlColumnNames;

	/**
	 * Gets the name of a column of the result in the generated SQL.
	 *
	 * @param modelColumn model index of the column
	 */
	private String label(int modelColumn) {
		return modelColumn < sqlColumnNames.size() ? sqlColumnNames.get(modelColumn) : "?";
	}

	/**
	 * Gets the SQL expression aggregating a value.
	 *
	 * @param value the value
	 * @param valueCol model index of the column of the value, -1 for the rows
	 * @param condition the condition of the column of the pivot table, or <code>null</code> for all rows
	 */
	private String aggregateExpression(ValueSpec value, int valueCol, String condition, Quoting quoting) {
		String expression = valueCol < 0 ? "1" : quoting.quote(label(valueCol));
		if (valueCol < 0 && condition == null) {
			return "count(*)";
		}
		String argument = condition == null ? expression : "case when " + condition + " then " + expression + " end";
		switch (value.aggregate) {
			case COUNT_DISTINCT: return "count(distinct " + argument + ")";
			case SUM: return "sum(" + argument + ")";
			case AVG: return "avg(" + argument + ")";
			case MIN: return "min(" + argument + ")";
			case MAX: return "max(" + argument + ")";
			default: return "count(" + argument + ")";
		}
	}

	/**
	 * Sets the action that shows the "Rows" tab (used by the drilldown).
	 */
	public void setShowRowsTabAction(Runnable showRowsTabAction) {
		this.showRowsTabAction = showRowsTabAction;
	}

	private void showMessage(String message) {
		shownLines = null;
		shownColumns = null;
		shownAggregation = null;
		shownSortIndex = -1;
		updateButtons();
		infoLabel.setText("");
		centerPanel.removeAll();
		centerPanel.add(new JLabel(message, SwingConstants.CENTER));
		centerPanel.revalidate();
		centerPanel.repaint();
	}

	private void adjustColumnWidths() {
		TableColumnModel cm = resultTable.getColumnModel();
		int maxRows = Math.min(resultTable.getRowCount(), 200);
		for (int c = 0; c < cm.getColumnCount(); c++) {
			TableColumn column = cm.getColumn(c);
			Component header = resultTable.getTableHeader().getDefaultRenderer()
					.getTableCellRendererComponent(resultTable, column.getHeaderValue(), false, false, -1, c);
			int width = header.getPreferredSize().width;
			for (int r = 0; r < maxRows; r++) {
				Component cell = resultTable.prepareRenderer(resultTable.getCellRenderer(r, c), r, c);
				width = Math.max(width, cell.getPreferredSize().width);
			}
			// the total line is the last one
			int r = resultTable.getRowCount() - 1;
			if (r >= maxRows) {
				Component cell = resultTable.prepareRenderer(resultTable.getCellRenderer(r, c), r, c);
				width = Math.max(width, cell.getPreferredSize().width);
			}
			column.setPreferredWidth(Math.min(width + 16, 400));
		}
	}

	private static List<Object> sortedKeys(Set<Object> keys) {
		List<Object> result = new ArrayList<>();
		boolean hasNull = false;
		Class<?> keyClass = null;
		boolean comparable = true;
		for (Object key: keys) {
			if (key == NULL_KEY) {
				hasNull = true;
				continue;
			}
			result.add(key);
			if (!(key instanceof Comparable) || keyClass != null && keyClass != key.getClass()) {
				comparable = false;
			}
			keyClass = key.getClass();
		}
		if (comparable) {
			@SuppressWarnings({ "unchecked", "rawtypes" })
			Comparator<Object> natural = (a, b) -> ((Comparable) a).compareTo(b);
			result.sort(natural);
		} else {
			result.sort(Comparator.comparing(String::valueOf));
		}
		if (hasNull) {
			result.add(NULL_KEY);
		}
		return result;
	}

	/**
	 * Normalizes a value of a row (as read from the result set).
	 */
	private static Object normalize(Object value) {
		if (value instanceof PObjectWrapper) {
			value = ((PObjectWrapper) value).getValue();
		}
		if (value == UIUtil.NULL) {
			value = null;
		}
		return value;
	}

	/**
	 * Gets the key of a group from a value of a row (as read from the result set).
	 */
	private static Object toKey(Object value) {
		value = normalize(value);
		if (value == null) {
			return NULL_KEY;
		}
		if (value instanceof java.sql.Date || value instanceof Timestamp) {
			// like the rows table: dates without time are shown without " 00:00:00.0"
			String asString = value.toString();
			if (asString.endsWith(MIDNIGHT) && asString.length() > MIDNIGHT.length()) {
				return asString.substring(0, asString.length() - MIDNIGHT.length());
			}
		}
		if (!(value instanceof Comparable)) {
			return String.valueOf(value);
		}
		return value;
	}

	/**
	 * Converts a numeric cell value into a {@link BigDecimal}.
	 *
	 * @param value the cell value
	 * @return the value as BigDecimal, or <code>null</code> if the value is not a (finite) number
	 */
	private static BigDecimal toBigDecimal(Object value) {
		if (!(value instanceof Number)) {
			return null;
		}
		try {
			if (value instanceof BigDecimal) {
				return (BigDecimal) value;
			}
			if (value instanceof BigInteger) {
				return new BigDecimal((BigInteger) value);
			}
			if (value instanceof Double || value instanceof Float) {
				double d = ((Number) value).doubleValue();
				if (Double.isNaN(d) || Double.isInfinite(d)) {
					return null;
				}
				return new BigDecimal(value.toString());
			}
			if (value instanceof Long || value instanceof Integer || value instanceof Short || value instanceof Byte) {
				return BigDecimal.valueOf(((Number) value).longValue());
			}
			return new BigDecimal(value.toString());
		} catch (NumberFormatException e) {
			return null;
		}
	}

	private boolean isNumeric(int modelCol) {
		if (columnTypes != null && modelCol < columnTypes.size()) {
			switch (columnTypes.get(modelCol)) {
				case Types.BIGINT: case Types.DECIMAL: case Types.DOUBLE:
				case Types.FLOAT:  case Types.INTEGER: case Types.NUMERIC:
				case Types.REAL:   case Types.SMALLINT: case Types.TINYINT:
					return true;
				default: break;
			}
		}
		return false;
	}

	private static String stripHtml(String html) {
		if (html == null) {
			return "";
		}
		if (html.startsWith("<html>")) {
			Matcher m = BOLD_PATTERN.matcher(html);
			if (m.find()) {
				return fromHtml(m.group(1));
			}
			return fromHtml(html.replaceAll("<br>", " ").replaceAll("<[^>]*>", " "));
		}
		return html;
	}

	/**
	 * Gets the table of a column header of the SQL Console ("&lt;html&gt;&lt;nobr&gt;&lt;font ...&gt;TABLE&lt;/font&gt;&lt;br&gt;&lt;b&gt;COLUMN&lt;/b&gt;...").
	 *
	 * @return the table, or <code>null</code> if the header has none
	 */
	private static String tableOfHeader(String html) {
		if (html == null) {
			return null;
		}
		Matcher m = TABLE_PATTERN.matcher(html);
		if (m.find()) {
			String table = fromHtml(m.group(1));
			return table.isEmpty() ? null : table;
		}
		return null;
	}

	private static String fromHtml(String html) {
		return UIUtil.fromHTMLFragment(html).replaceAll("\\s+", " ").trim();
	}

	/**
	 * Aggregates the values of one cell, row, column or of all rows.
	 */
	private static class Accumulator {
		private long count = 0;
		private long numCount = 0;
		private BigDecimal sum = BigDecimal.ZERO;
		private BigDecimal min = null;
		private BigDecimal max = null;
		private final Set<Object> distinctValues;

		Accumulator(boolean distinct) {
			distinctValues = distinct ? new HashSet<>() : null;
		}

		void add(Object value) {
			if (value == null) {
				return;
			}
			++count;
			if (distinctValues != null) {
				distinctValues.add(value);
			}
			BigDecimal number = toBigDecimal(value);
			if (number != null) {
				++numCount;
				sum = sum.add(number);
				if (min == null || number.compareTo(min) < 0) {
					min = number;
				}
				if (max == null || number.compareTo(max) > 0) {
					max = number;
				}
			}
		}

		Object result(String aggregate) {
			switch (aggregate) {
				case COUNT: return count;
				case COUNT_DISTINCT: return (long) distinctValues.size();
				case SUM: return numCount == 0 ? null : sum;
				case AVG: return numCount == 0 ? null : sum.divide(BigDecimal.valueOf(numCount), MathContext.DECIMAL64).doubleValue();
				case MIN: return min;
				case MAX: return max;
				default: return null;
			}
		}
	}

	private static class PivotTableModel extends AbstractTableModel {
		private final List<String> headers;
		private final List<Object[]> data;
		private final List<PivotLine> lines;
		private final int rowFieldCount;
		private final List<PivotColumn> columns;
		private final int[] detailIndex; // per line: running number of the line among the non-total lines

		PivotTableModel(List<String> headers, List<Object[]> data, List<PivotLine> lines, int rowFieldCount, List<PivotColumn> columns) {
			this.headers = headers;
			this.data = data;
			this.lines = lines;
			this.rowFieldCount = rowFieldCount;
			this.columns = columns;
			this.detailIndex = new int[lines.size()];
			int index = 0;
			for (int i = 0; i < lines.size(); i++) {
				detailIndex[i] = lines.get(i).isTotal ? i : index++;
			}
			// range of the values per value (for the heatmap), without the subtotals and totals
			int valueCount = columns.stream().mapToInt(c -> c.valueIndex + 1).max().orElse(0);
			heatMin = new double[valueCount];
			heatMax = new double[valueCount];
			Arrays.fill(heatMin, Double.POSITIVE_INFINITY);
			Arrays.fill(heatMax, Double.NEGATIVE_INFINITY);
			for (int r = 0; r < data.size() && r < lines.size(); r++) {
				if (lines.get(r).isTotal) {
					continue;
				}
				for (int c = 0; c < columns.size(); c++) {
					Object value = data.get(r)[rowFieldCount + c];
					if (!columns.get(c).isTotal && value instanceof Number) {
						double d = ((Number) value).doubleValue();
						int v = columns.get(c).valueIndex;
						heatMin[v] = Math.min(heatMin[v], d);
						heatMax[v] = Math.max(heatMax[v], d);
					}
				}
			}
		}

		private final double[] heatMin;
		private final double[] heatMax;

		/**
		 * Gets the position of the value of a cell within the range of its value (for the heatmap).
		 *
		 * @return 0 (lowest) to 1 (highest), or -1 if the cell isn't colored (subtotal, total, or no number)
		 */
		double heat(int row, int column) {
			int c = column - rowFieldCount;
			if (row < 0 || row >= data.size() || row >= lines.size() || lines.get(row).isTotal || c < 0 || c >= columns.size() || columns.get(c).isTotal) {
				return -1;
			}
			Object value = data.get(row)[column];
			if (!(value instanceof Number)) {
				return -1;
			}
			int v = columns.get(c).valueIndex;
			double range = heatMax[v] - heatMin[v];
			return range > 0 ? (((Number) value).doubleValue() - heatMin[v]) / range : 1;
		}

		/**
		 * Whether the line is drawn with the first of the two alternating background colors.
		 */
		boolean isEvenLine(int row) {
			return row < 0 || row >= detailIndex.length || detailIndex[row] % 2 == 0;
		}

		boolean isTotalLine(int row) {
			return row >= 0 && row < lines.size() && lines.get(row).isTotal;
		}

		/**
		 * Whether a cell is the label of a row group that can be collapsed or expanded.
		 *
		 * @return 0: no such label, 1: label of an expanded group, 2: label of a collapsed group
		 */
		int groupState(int row, int column) {
			if (row < 0 || row >= lines.size() || column < 0 || column >= rowFieldCount - 1) {
				return 0; // the groups of the last row field have no subgroups
			}
			PivotLine line = lines.get(row);
			int k = line.prefix.size();
			if (line.collapsed) {
				return column == k - 1 ? 2 : column < k - 1 && line.showLabel[column] ? 1 : 0;
			}
			if (line.isTotal) {
				return k > 0 && column == k - 1 ? 1 : 0;
			}
			return column < k && line.showLabel[column] ? 1 : 0;
		}

		boolean isTotalColumn(int column) {
			int c = column - rowFieldCount;
			return c >= 0 && c < columns.size() && columns.get(c).isTotal;
		}

		@Override
		public int getRowCount() {
			return data.size();
		}

		@Override
		public int getColumnCount() {
			return headers.size();
		}

		@Override
		public String getColumnName(int column) {
			return headers.get(column);
		}

		@Override
		public Object getValueAt(int rowIndex, int columnIndex) {
			return data.get(rowIndex)[columnIndex];
		}
	}

	private class PivotCellRenderer extends DefaultTableCellRenderer {
		private static final long serialVersionUID = 1L;

		@Override
		public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
			String text;
			if (value instanceof Percentage) {
				text = ((Percentage) value).format();
			} else if (value instanceof BigDecimal) {
				BigDecimal number = (BigDecimal) value;
				if (number.scale() < 0) {
					number = number.setScale(0);
				}
				text = UIUtil.format(number);
			} else if (value instanceof Double) {
				text = UIUtil.format((double) (Double) value);
			} else if (value instanceof Long) {
				text = UIUtil.format((long) (Long) value);
			} else {
				text = value == null ? "" : String.valueOf(value);
			}
			// like the rows table: selection is shown by the background only
			Component render = super.getTableCellRendererComponent(table, text, false, hasFocus, row, column);
			setHorizontalAlignment(value instanceof Number ? SwingConstants.RIGHT : SwingConstants.LEFT);
			boolean isTotal = false;
			boolean even = row % 2 == 0;
			int groupState = 0;
			if (table.getModel() instanceof PivotTableModel) {
				PivotTableModel model = (PivotTableModel) table.getModel();
				isTotal = model.isTotalLine(row) || model.isTotalColumn(table.convertColumnIndexToModel(column));
				even = model.isEvenLine(row);
				groupState = model.groupState(row, table.convertColumnIndexToModel(column));
			}
			setIcon(groupState == 0 ? null : UIManager.getIcon(groupState == 1 ? "Tree.expandedIcon" : "Tree.collapsedIcon"));
			render.setFont(isTotal ? getFont().deriveFont(Font.BOLD) : getFont().deriveFont(Font.PLAIN));
			render.setForeground(table.getForeground());
			if (isSelected) {
				render.setBackground(even ? UIUtil.TABLE_BG1SELECTED : UIUtil.TABLE_BG2SELECTED);
			} else if (isTotal) {
				render.setBackground(UIUtil.BG_FLATMOUSEOVER);
			} else {
				Color background = even ? UIUtil.TABLE_BACKGROUND_COLOR_1 : UIUtil.TABLE_BACKGROUND_COLOR_2;
				double heat = heatmapCheckBox.isSelected() && table.getModel() instanceof PivotTableModel
						? ((PivotTableModel) table.getModel()).heat(row, table.convertColumnIndexToModel(column)) : -1;
				if (heat >= 0) {
					background = blend(background, UIUtil.plaf == PLAF.FLATDARK ? HEAT_COLOR_DARK : HEAT_COLOR, 0.08 + 0.62 * heat);
				}
				render.setBackground(background);
			}
			return render;
		}
	}

	private static final Color HEAT_COLOR = new Color(255, 120, 0);
	private static final Color HEAT_COLOR_DARK = new Color(210, 90, 20);

	private static Color blend(Color a, Color b, double ratio) {
		return new Color(
				(int) Math.round(a.getRed() + (b.getRed() - a.getRed()) * ratio),
				(int) Math.round(a.getGreen() + (b.getGreen() - a.getGreen()) * ratio),
				(int) Math.round(a.getBlue() + (b.getBlue() - a.getBlue()) * ratio));
	}

}
