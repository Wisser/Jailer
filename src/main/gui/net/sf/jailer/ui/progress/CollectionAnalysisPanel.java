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
package net.sf.jailer.ui.progress;

import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.SQLException;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.JViewport;
import javax.swing.ListSelectionModel;
import javax.swing.RowFilter;
import javax.swing.SwingUtilities;
import javax.swing.RowSorter.SortKey;
import javax.swing.SortOrder;
import javax.swing.SwingConstants;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableColumn;
import javax.swing.table.TableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableRowSorter;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.ModelElement;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.entitygraph.RowOriginStep;
import net.sf.jailer.entitygraph.remote.RemoteEntityGraph;
import net.sf.jailer.ui.Colors;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.util.ConcurrentTaskControl;
import net.sf.jailer.ui.util.CsvOptionsDialog;
import net.sf.jailer.util.CancellationException;

/**
 * Shows which association is responsible for how many rows of the subset.
 * <p>
 * The rows of a subset are collected by resolving associations. If a subset becomes far
 * bigger than expected, the question is which association let the rows in. The progress
 * table answers this per table, this panel answers it per association, so that the
 * association to be restricted can be found without guessing.
 * <p>
 * For the selected association, the lower part shows the associations which have brought
 * rows into its source table one step earlier. Often it is not the association with the
 * most rows which has to be restricted, but one of those a few steps ahead of it.
 *
 * @author Ralf Wisser
 */
public class CollectionAnalysisPanel extends JPanel {

	/**
	 * The share of the rows for which the summary states how few entries produce it.
	 */
	private static final double SUMMARY_SHARE = 0.9;

	/**
	 * A single row of the upper table.
	 */
	private static class Row {
		String name;
		String from;
		String to;
		long rows;
		Change change;
		Ratio perSourceRow;
		double share;
		double cumulatedShare;
		Steps step;
		Association association;
		CollectionAnalysis.Contribution contribution;
	}

	/**
	 * Number of rows an association has brought in per row of its source table. A type of its own,
	 * so that the renderer can tell it from a share, which is a {@link Double}.
	 */
	private static class Ratio implements Comparable<Ratio> {
		final double value;

		Ratio(double value) {
			this.value = value;
		}

		@Override
		public int compareTo(Ratio other) {
			return Double.compare(value, other.value);
		}
	}

	/**
	 * Change in the number of rows compared to the previous run. A type of its own, so that the
	 * renderer can show the sign and the number of the previous run.
	 */
	private static class Change implements Comparable<Change> {
		final long value;
		final long rows;
		final long previousRows;
		/**
		 * The last step compared, {@link Integer#MAX_VALUE} if the whole runs are compared.
		 */
		final int upToStep;

		Change(long rows, long previousRows, int upToStep) {
			this.value = rows - previousRows;
			this.rows = rows;
			this.previousRows = previousRows;
			this.upToStep = upToStep;
		}

		/**
		 * Gets the tool tip of the cell.
		 *
		 * @return the tool tip
		 */
		String toolTip() {
			if (upToStep == Integer.MAX_VALUE) {
				return "Previous run: " + UIUtil.format(previousRows) + " rows";
			}
			return "Up to step " + upToStep + " - previous run: " + UIUtil.format(previousRows)
					+ " rows, this run: " + UIUtil.format(rows) + " rows";
		}

		@Override
		public int compareTo(Change other) {
			return Long.compare(value, other.value);
		}
	}

	/**
	 * A single row of the lower table.
	 */
	private static class PredecessorRow {
		String name;
		String from;
		long rows;
		Steps step;
		Association association;
		CollectionAnalysis.Contribution contribution;
	}

	private final JLabel summaryLabel;
	private TableRowSorter<AnalysisTableModel> sorter;
	private JButton dryRunAgainButton;
	private Runnable dryRunAgain;
	private final JLabel detailsLabel;
	private final JTable table;
	private final JTable detailsTable;
	private final AnalysisTableModel tableModel;
	private final PredecessorTableModel detailsTableModel;

	private List<Row> rows = new ArrayList<Row>();
	private List<PredecessorRow> predecessorRows = new ArrayList<PredecessorRow>();
	private List<CollectionAnalysis.Contribution> contributions = new ArrayList<CollectionAnalysis.Contribution>();
	private DataModel dataModel;
	private ModelElement selectedModelElement;
	private Consumer<Association> associationSelector;
	private Consumer<Association> associationDisabler;
	private BiConsumer<Table, Association> collectedRowsThroughOpener;
	private RowOriginContext rowOriginContext;
	private Runnable discardAction;
	private Consumer<List<RowOriginStep>> pathOpener;
	private Consumer<Table> tableOpener;
	private CollectionAnalysis.Baseline previousRun;
	/**
	 * Whether the run has been completed. Before, only the completed steps are compared with the
	 * previous run: the current one is still being collected.
	 */
	private boolean runCompleted = false;
	private CollectionAnalysis lastAnalysis;

	private final NumberFormat percentFormat = createPercentFormat();
	private final NumberFormat ratioFormat = createRatioFormat();

	/**
	 * Constructor.
	 */
	public CollectionAnalysisPanel() {
		setLayout(new BorderLayout());

		summaryLabel = new JLabel(" ");
		summaryLabel.setBorder(BorderFactory.createEmptyBorder(4, 0, 4, 4));
		summaryLabel.setToolTipText(
				"Each row is counted for the association through which it has been collected first. "
				+ "Rows which are reachable via several associations are not counted more than once. "
				+ "\"Per Source Row\" divides them by the rows collected into the source table: "
				+ "a value far above 1 marks an association that multiplies the subset.");
		JPanel northPanel = new JPanel(new BorderLayout());
		northPanel.add(summaryLabel, BorderLayout.CENTER);
		northPanel.add(createDryRunAgainButton(), BorderLayout.EAST);
		// second row: filter (both rows start without left inset, aligned with the table)
		JPanel controlsPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
		controlsPanel.add(createFilterPanel());
		northPanel.add(controlsPanel, BorderLayout.SOUTH);
		add(northPanel, BorderLayout.NORTH);

		tableModel = new AnalysisTableModel();
		table = createTable(tableModel, 3, tableModel.columnToolTips);
		sorter = new TableRowSorter<AnalysisTableModel>(tableModel);
		sorter.setSortKeys(Collections.singletonList(new SortKey(3, SortOrder.DESCENDING)));
		table.setRowSorter(sorter);
		table.getColumnModel().getColumn(0).setPreferredWidth(200);
		table.getColumnModel().getColumn(1).setPreferredWidth(140);
		table.getColumnModel().getColumn(2).setPreferredWidth(140);
		table.getColumnModel().getColumn(3).setPreferredWidth(80);
		table.getColumnModel().getColumn(4).setPreferredWidth(80);
		table.getColumnModel().getColumn(5).setPreferredWidth(80);
		table.getColumnModel().getColumn(6).setPreferredWidth(60);
		table.getColumnModel().getColumn(7).setPreferredWidth(80);
		table.getColumnModel().getColumn(8).setPreferredWidth(60);
		fitColumnsToHeaders(table);
		table.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			@Override
			public void valueChanged(ListSelectionEvent e) {
				if (!e.getValueIsAdjusting()) {
					Row row = selectedRow();
					selectedModelElement = row != null && row.contribution != null? row.contribution.getModelElement() : null;
					updateDetails();
				}
			}
		});
		table.addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent e) {
				maybeShowPopup(e, table, associationAt(e, table));
			}
			@Override
			public void mouseReleased(MouseEvent e) {
				maybeShowPopup(e, table, associationAt(e, table));
			}
			@Override
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() == 2 && !e.isPopupTrigger()) {
					selectInModel(associationAt(e, table));
				}
			}
		});

		detailsTableModel = new PredecessorTableModel();
		detailsTable = createTable(detailsTableModel, 2, detailsTableModel.columnToolTips);
		TableRowSorter<PredecessorTableModel> detailsSorter = new TableRowSorter<PredecessorTableModel>(detailsTableModel);
		detailsSorter.setSortKeys(Collections.singletonList(new SortKey(2, SortOrder.DESCENDING)));
		detailsTable.setRowSorter(detailsSorter);
		detailsTable.getColumnModel().getColumn(0).setPreferredWidth(200);
		detailsTable.getColumnModel().getColumn(1).setPreferredWidth(140);
		detailsTable.getColumnModel().getColumn(2).setPreferredWidth(80);
		detailsTable.getColumnModel().getColumn(3).setPreferredWidth(60);
		fitColumnsToHeaders(detailsTable);
		detailsTable.addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent e) {
				maybeShowPopup(e, detailsTable, predecessorAt(e));
			}
			@Override
			public void mouseReleased(MouseEvent e) {
				maybeShowPopup(e, detailsTable, predecessorAt(e));
			}
			@Override
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() == 2 && !e.isPopupTrigger()) {
					PredecessorRow row = rowAt(e, detailsTable, predecessorRows);
					if (row != null && row.contribution != null) {
						// walk one step further towards the subject
						selectContribution(row.contribution.getModelElement(), true);
					}
				}
			}
		});

		detailsLabel = new JLabel(" ");
		detailsLabel.setBorder(BorderFactory.createEmptyBorder(4, 6, 2, 4));
		detailsLabel.setToolTipText(
				"<html>Associations which have brought rows into the source table of the selected association"
				+ "<br>one step earlier. This is an assignment by step and table, not the origin of a single row:"
				+ "<br>if several associations feed the same table, all of them are shown."
				+ "<br>Double click to walk one step further towards the subject.</html>");
		JPanel detailsPanel = new JPanel(new BorderLayout());
		detailsPanel.add(detailsLabel, BorderLayout.NORTH);
		detailsPanel.add(new JScrollPane(detailsTable), BorderLayout.CENTER);

		JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, new JScrollPane(table), detailsPanel);
		splitPane.setOneTouchExpandable(true);
		splitPane.setResizeWeight(0.7);
		add(splitPane, BorderLayout.CENTER);

		updateDetails();
	}

	/**
	 * Creates the field which filters the upper table by the names of the associations and their
	 * tables. It filters as one types, Escape clears it.
	 *
	 * @return the panel holding the field
	 */
	private JPanel createFilterPanel() {
		final JTextField filterField = new JTextField(20);
		String toolTip = "Shows only associations whose name, source or destination table contains this text (case-insensitive).";
		filterField.setToolTipText(toolTip);
		filterField.getDocument().addDocumentListener(new DocumentListener() {
			@Override
			public void insertUpdate(DocumentEvent e) {
				applyFilter(filterField.getText());
			}
			@Override
			public void removeUpdate(DocumentEvent e) {
				applyFilter(filterField.getText());
			}
			@Override
			public void changedUpdate(DocumentEvent e) {
				applyFilter(filterField.getText());
			}
		});
		filterField.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				if (e.getKeyCode() == KeyEvent.VK_ESCAPE && filterField.getText().length() > 0) {
					filterField.setText("");
					e.consume();
				}
			}
		});
		JLabel label = new JLabel("Filter: ");
		label.setToolTipText(toolTip);
		JPanel panel = new JPanel(new BorderLayout());
		panel.setBorder(BorderFactory.createEmptyBorder(2, 0, 2, 4));
		panel.add(label, BorderLayout.WEST);
		panel.add(filterField, BorderLayout.CENTER);
		return panel;
	}

	/**
	 * Shows only the rows of the upper table whose association, source or destination contains a
	 * text, ignoring case.
	 *
	 * @param text the text, empty for all rows
	 */
	private void applyFilter(String text) {
		if (sorter == null) {
			return;
		}
		final String pattern = text.trim().toLowerCase(Locale.ENGLISH);
		if (pattern.isEmpty()) {
			sorter.setRowFilter(null);
			return;
		}
		sorter.setRowFilter(new RowFilter<AnalysisTableModel, Integer>() {
			@Override
			public boolean include(Entry<? extends AnalysisTableModel, ? extends Integer> entry) {
				for (int column = 0; column <= 2; ++column) {
					Object value = entry.getValue(column);
					if (value != null && value.toString().toLowerCase(Locale.ENGLISH).contains(pattern)) {
						return true;
					}
				}
				return false;
			}
		});
	}

	private static NumberFormat createPercentFormat() {
		NumberFormat format = NumberFormat.getPercentInstance();
		format.setMaximumFractionDigits(1);
		return format;
	}

	private static NumberFormat createRatioFormat() {
		NumberFormat format = NumberFormat.getNumberInstance();
		format.setMaximumFractionDigits(2);
		return format;
	}

	/**
	 * Makes every column at least as wide as its title, so that no title is cut off. Leaves
	 * columns which are wide enough as they are.
	 *
	 * @param theTable the table
	 */
	private static void fitColumnsToHeaders(JTable theTable) {
		TableCellRenderer headerRenderer = theTable.getTableHeader().getDefaultRenderer();
		for (int i = 0; i < theTable.getColumnModel().getColumnCount(); ++i) {
			TableColumn column = theTable.getColumnModel().getColumn(i);
			// room for the sort arrow, the sorted column shows one
			int width = headerRenderer.getTableCellRendererComponent(theTable, column.getHeaderValue(), false, false, -1, i).getPreferredSize().width + 24;
			column.setMinWidth(width);
			column.setPreferredWidth(Math.max(column.getPreferredWidth(), width));
		}
	}

	/**
	 * Creates a table with the look of the other tables of the application.
	 *
	 * @param model the model
	 * @param firstRightAlignedColumn index of the first column which is aligned to the right
	 * @param columnToolTips tool tips of the column headers, by model column
	 * @return the table
	 */
	private JTable createTable(final AbstractTableModel model, final int firstRightAlignedColumn, final String[] columnToolTips) {
		final JTable newTable = new JTable(model) {
			@Override
			public boolean getScrollableTracksViewportWidth() {
				// fill the width as long as no column has to go below its minimum, scroll otherwise.
				// Compared with the minimum widths, not with the preferred size, which follows the
				// current column widths and would keep the table wide once it has been stretched
				if (!(getParent() instanceof JViewport)) {
					return super.getScrollableTracksViewportWidth();
				}
				int minWidth = 0;
				for (int i = 0; i < getColumnModel().getColumnCount(); ++i) {
					minWidth += getColumnModel().getColumn(i).getMinWidth();
				}
				return getParent().getWidth() >= minWidth;
			}
			private static final long serialVersionUID = 4630297516837306417L;
		};
		newTable.setTableHeader(new JTableHeader(newTable.getColumnModel()) {
			@Override
			public String getToolTipText(MouseEvent e) {
				int column = columnAtPoint(e.getPoint());
				int modelColumn = column < 0? -1 : newTable.convertColumnIndexToModel(column);
				return modelColumn >= 0 && modelColumn < columnToolTips.length? columnToolTips[modelColumn] : null;
			}
			private static final long serialVersionUID = -2318845710023419533L;
		});
		newTable.setAutoResizeMode(JTable.AUTO_RESIZE_LAST_COLUMN);
		newTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		newTable.setShowGrid(false);

		final TableCellRenderer defaultRenderer = newTable.getDefaultRenderer(String.class);
		TableCellRenderer renderer = new TableCellRenderer() {
			final Color BG1 = UIUtil.TABLE_BACKGROUND_COLOR_1;
			final Color BG2 = UIUtil.TABLE_BACKGROUND_COLOR_2;

			@Override
			public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
				Object renderedValue = value;
				if (value instanceof Long) {
					renderedValue = UIUtil.format(((Long) value).longValue());
				} else if (value instanceof Double) {
					renderedValue = percentFormat.format(((Double) value).doubleValue());
				} else if (value instanceof Ratio) {
					renderedValue = ratioFormat.format(((Ratio) value).value);
				} else if (value instanceof Change) {
					long change = ((Change) value).value;
					renderedValue = (change > 0? "+" : "") + UIUtil.format(change);
				}
				Component render = defaultRenderer.getTableCellRendererComponent(table, renderedValue, isSelected, false, row, column);
				if (!isSelected) {
					render.setBackground((row % 2 == 0) ? BG1 : BG2);
				}
				if (render instanceof JLabel) {
					((JLabel) render).setHorizontalAlignment(column >= firstRightAlignedColumn? SwingConstants.RIGHT : SwingConstants.LEFT);
					String valueAsString = renderedValue == null? "" : String.valueOf(renderedValue);
					if (value instanceof Change) {
						valueAsString = ((Change) value).toolTip();
					} else if (value instanceof Steps) {
						valueAsString = ((Steps) value).toolTip();
					}
					((JLabel) render).setToolTipText(valueAsString.length() > 0? valueAsString : null);
				}
				return render;
			}
		};
		for (int i = 0; i < model.getColumnCount(); ++i) {
			newTable.getColumnModel().getColumn(i).setCellRenderer(renderer);
		}
		return newTable;
	}

	/**
	 * Sets the consumer which selects an association in the extraction model editor.
	 * If none is set, the corresponding menu item is not offered.
	 *
	 * @param associationSelector the consumer, or <code>null</code>
	 */
	public void setAssociationSelector(Consumer<Association> associationSelector) {
		this.associationSelector = associationSelector;
	}

	/**
	 * Sets the consumer which opens a table in a Data Browser, limited to the rows collected
	 * through an association. If none is set, the corresponding menu item is not offered.
	 *
	 * @param collectedRowsThroughOpener the consumer (given the table and the association, which is
	 *            <code>null</code> for the subject rows), or <code>null</code>
	 */
	public void setCollectedRowsThroughOpener(BiConsumer<Table, Association> collectedRowsThroughOpener) {
		this.collectedRowsThroughOpener = collectedRowsThroughOpener;
	}

	/**
	 * Sets the consumer which disables an association in the extraction model.
	 * If none is set, the corresponding menu item is not offered.
	 *
	 * @param associationDisabler the consumer, or <code>null</code>
	 */
	public void setAssociationDisabler(Consumer<Association> associationDisabler) {
		this.associationDisabler = associationDisabler;
	}

	/**
	 * Sets the result of the previous run, to compare this one with.
	 *
	 * @param previousRun the baseline, or <code>null</code> if there is nothing to compare with
	 */
	public void setPreviousRun(CollectionAnalysis.Baseline previousRun) {
		this.previousRun = previousRun;
	}

	/**
	 * Announces that the run is over, whether completed, cancelled or failed, so that it can be
	 * run again.
	 */
	public void setRunOver() {
		dryRunAgainButton.setEnabled(true);
	}

	/**
	 * Sets the action which closes the window and starts a dry run with the same settings again.
	 * If none is set, the button is not shown.
	 *
	 * @param dryRunAgain the action, or <code>null</code>
	 */
	public void setDryRunAgain(Runnable dryRunAgain) {
		this.dryRunAgain = dryRunAgain;
		dryRunAgainButton.setVisible(dryRunAgain != null);
	}

	/**
	 * Creates the "Dry Run Again" button, hidden until an action is set and disabled until the run
	 * is over.
	 *
	 * @return the button
	 */
	private JButton createDryRunAgainButton() {
		dryRunAgainButton = new JButton("Dry Run Again");
		dryRunAgainButton.setIcon(UIUtil.scaleIcon(dryRunAgainButton, UIUtil.readImage("/dryrun.png")));
		dryRunAgainButton.setToolTipText("Closes this window and starts a Dry Run with the same settings, to see the effect of a restriction in the \"Change\" column.");
		dryRunAgainButton.setVisible(false);
		dryRunAgainButton.setEnabled(false);
		dryRunAgainButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (dryRunAgain != null) {
					dryRunAgainButton.setEnabled(false);
					dryRunAgain.run();
				}
			}
		});
		return dryRunAgainButton;
	}

	/**
	 * Announces that the run has been completed, so that the whole runs are compared with each
	 * other, not only the steps completed so far.
	 */
	public void setRunCompleted() {
		runCompleted = true;
		if (lastAnalysis != null && dataModel != null) {
			setAnalysis(lastAnalysis, dataModel);
		}
	}

	/**
	 * Sets the consumer which lays the way of a row into the subset out in a Data Browser.
	 * If none is set, the chain is shown as a table only.
	 *
	 * @param pathOpener the consumer, or <code>null</code>
	 */
	public void setPathOpener(Consumer<List<RowOriginStep>> pathOpener) {
		this.pathOpener = pathOpener;
	}

	/**
	 * Sets the consumer which opens a table in a Data Browser.
	 * If none is set, the corresponding menu item is not offered.
	 *
	 * @param tableOpener the consumer, or <code>null</code>
	 */
	public void setTableOpener(Consumer<Table> tableOpener) {
		this.tableOpener = tableOpener;
	}

	/**
	 * Sets the context for the row origin analysis. Only if one is set and an entity-graph has
	 * been retained, the collected rows of an association can be inspected.
	 *
	 * @param rowOriginContext the context, or <code>null</code>
	 * @param discardAction discards the retained data, or <code>null</code>
	 */
	public void setRowOriginContext(RowOriginContext rowOriginContext, Runnable discardAction) {
		this.rowOriginContext = rowOriginContext;
		this.discardAction = discardAction;
	}

	/**
	 * Updates the content.
	 *
	 * @param analysis the analysis to show
	 * @param dataModel the data model, for display names
	 */
	public void setAnalysis(CollectionAnalysis analysis, DataModel dataModel) {
		this.dataModel = dataModel;
		this.lastAnalysis = analysis;
		contributions = analysis.getContributions();
		long total = analysis.getTotal();
		// while the run is going, only the completed steps are compared with the previous run
		int firstStep = Integer.MAX_VALUE;
		int lastStep = -1;
		for (CollectionAnalysis.Contribution contribution: contributions) {
			firstStep = Math.min(firstStep, contribution.getFirstDay());
			lastStep = Math.max(lastStep, contribution.getLastDay());
		}
		final int compareUpTo = runCompleted? Integer.MAX_VALUE : lastStep - 1;
		final boolean comparable = previousRun != null && (runCompleted || compareUpTo >= firstStep);
		// rows collected into each table, the denominator of "Per Source Row"
		Map<Table, Long> rowsPerTable = new HashMap<Table, Long>();
		for (CollectionAnalysis.Contribution contribution: contributions) {
			Table destination = contribution.getDestination();
			if (destination != null) {
				Long rowsOfTable = rowsPerTable.get(destination);
				rowsPerTable.put(destination, rowsOfTable == null? contribution.getRows() : rowsOfTable + contribution.getRows());
			}
		}
		List<Row> newRows = new ArrayList<Row>(contributions.size());
		long cumulated = 0;
		int associations = 0;
		int entriesForSummary = -1;
		for (CollectionAnalysis.Contribution contribution: contributions) {
			Row row = new Row();
			row.contribution = contribution;
			row.association = contribution.getAssociation();
			Table destination = contribution.getDestination();
			if (row.association != null) {
				row.name = row.association.getName();
				row.from = dataModel.getDisplayName(row.association.source);
				++associations;
			} else {
				row.name = "(subject)";
				row.from = "";
			}
			row.to = destination != null? dataModel.getDisplayName(destination) : "";
			row.rows = contribution.getRows();
			if (comparable) {
				row.change = runCompleted?
						new Change(row.rows, previousRun.getRows(contribution.getModelElement()), compareUpTo)
						: new Change(CollectionAnalysis.sumUpTo(contribution.getRowsPerDay(), compareUpTo),
								previousRun.getRows(contribution.getModelElement(), compareUpTo), compareUpTo);
			}
			if (row.association != null) {
				Long sourceRows = rowsPerTable.get(row.association.source);
				if (sourceRows != null && sourceRows > 0) {
					row.perSourceRow = new Ratio((double) row.rows / sourceRows);
				}
			}
			row.share = total > 0? (double) row.rows / total : 0.0;
			cumulated += row.rows;
			row.cumulatedShare = total > 0? (double) cumulated / total : 0.0;
			row.step = stepOf(contribution);
			newRows.add(row);
			if (entriesForSummary < 0 && total > 0 && row.cumulatedShare >= SUMMARY_SHARE) {
				entriesForSummary = newRows.size();
			}
		}
		rows = newRows;
		tableModel.fireTableDataChanged();
		restoreSelection();

		if (total <= 0) {
			summaryLabel.setText("No rows collected yet.");
		} else {
			StringBuilder summary = new StringBuilder();
			summary.append(UIUtil.format(total)).append(" rows, ");
			summary.append(associations).append(associations == 1? " association" : " associations");
			if (entriesForSummary > 0 && entriesForSummary < newRows.size()) {
				summary.append(". ").append(entriesForSummary)
					.append(entriesForSummary == 1? " entry produces " : " entries produce ")
					.append(percentFormat.format(SUMMARY_SHARE)).append(" of the rows.");
			} else {
				summary.append(".");
			}
			if (previousRun != null) {
				if (!comparable) {
					summary.append(" Previous run: compared once the first step is complete.");
				} else {
					long thisTotal = total;
					long previousTotal = previousRun.getTotal();
					if (runCompleted) {
						summary.append(" Previous run: ");
					} else {
						thisTotal = 0;
						for (CollectionAnalysis.Contribution contribution: contributions) {
							thisTotal += CollectionAnalysis.sumUpTo(contribution.getRowsPerDay(), compareUpTo);
						}
						previousTotal = previousRun.getTotal(compareUpTo);
						summary.append(" Up to step ").append(compareUpTo).append(": ").append(UIUtil.format(thisTotal))
							.append(" rows, previous run ");
					}
					summary.append(UIUtil.format(previousTotal)).append(" rows");
					if (previousTotal > 0) {
						double change = (double) (thisTotal - previousTotal) / previousTotal;
						summary.append(" (").append(change > 0? "+" : "").append(percentFormat.format(change)).append(")");
					}
					summary.append(".");
				}
			}
			summaryLabel.setText(summary.toString());
		}

		updateDetails();
	}

	private static Steps stepOf(CollectionAnalysis.Contribution contribution) {
		return new Steps(contribution);
	}

	/**
	 * The steps in which a contribution has collected rows. A type of its own, so that the column
	 * sorts by number and the renderer can show the rows per step.
	 */
	private static class Steps implements Comparable<Steps> {
		/**
		 * Maximum number of steps listed in the tool tip.
		 */
		private static final int MAX_LINES = 30;
		/**
		 * Width of the longest bar in the tool tip, in pixels.
		 */
		private static final int MAX_BAR_WIDTH = 120;

		final int firstDay;
		final int lastDay;
		final Map<Integer, Long> rowsPerDay;

		Steps(CollectionAnalysis.Contribution contribution) {
			this.firstDay = contribution.getFirstDay();
			this.lastDay = contribution.getLastDay();
			this.rowsPerDay = contribution.getRowsPerDay();
		}

		@Override
		public int compareTo(Steps other) {
			if (firstDay != other.firstDay) {
				return Integer.compare(firstDay, other.firstDay);
			}
			return Integer.compare(lastDay, other.lastDay);
		}

		@Override
		public String toString() {
			return firstDay == lastDay? String.valueOf(firstDay) : firstDay + " - " + lastDay;
		}

		/**
		 * Gets the rows per step as a small bar chart.
		 *
		 * @return HTML for the tool tip
		 */
		String toolTip() {
			long max = 0;
			for (Long rows: rowsPerDay.values()) {
				max = Math.max(max, rows.longValue());
			}
			// step green, rows bold, so that the two numbers are told apart at a glance
			StringBuilder sb = new StringBuilder("<html><table cellspacing=0 cellpadding=1>"
					+ "<tr><td align=right><font color=" + Colors.HTMLColor_006600 + ">Step</font></td><td align=right>&nbsp;<b>Rows</b>&nbsp;</td><td></td></tr>");
			int lines = 0;
			for (Map.Entry<Integer, Long> e: rowsPerDay.entrySet()) {
				if (++lines > MAX_LINES) {
					sb.append("<tr><td align=right>...</td><td></td><td></td></tr>");
					break;
				}
				long rows = e.getValue().longValue();
				int width = max > 0? (int) Math.max(1, rows * MAX_BAR_WIDTH / max) : 1;
				sb.append("<tr><td align=right><font color=" + Colors.HTMLColor_006600 + ">").append(e.getKey()).append("</font></td>")
					.append("<td align=right>&nbsp;<b>").append(UIUtil.format(rows)).append("</b>&nbsp;</td>")
					.append("<td><table cellspacing=0 cellpadding=0><tr><td bgcolor=\"#5070e0\" width=").append(width)
					// a small blank instead of "height": an empty cell has no fixed height in Swing's
					// HTML, it came out thinner with a single step than with several
					.append("><font size=1>&nbsp;</font></td></tr></table></td></tr>");
			}
			sb.append("</table></html>");
			return sb.toString();
		}
	}

	/**
	 * Re-selects the row which was selected before the content has been replaced.
	 */
	private void restoreSelection() {
		selectContribution(selectedModelElement, false);
	}

	/**
	 * Selects the row of a given model element.
	 *
	 * @param modelElement the association or subject table to select
	 * @param scrollIntoView <code>true</code> to scroll the row into view
	 */
	private void selectContribution(ModelElement modelElement, boolean scrollIntoView) {
		if (modelElement == null) {
			return;
		}
		for (int modelRow = 0; modelRow < rows.size(); ++modelRow) {
			if (modelElement.equals(rows.get(modelRow).contribution.getModelElement())) {
				int viewRow = table.convertRowIndexToView(modelRow);
				if (viewRow >= 0) {
					table.getSelectionModel().setSelectionInterval(viewRow, viewRow);
					if (scrollIntoView) {
						table.scrollRectToVisible(table.getCellRect(viewRow, 0, true));
					}
				}
				return;
			}
		}
	}

	/**
	 * Updates the lower table with the possible predecessors of the selected association.
	 */
	private void updateDetails() {
		List<PredecessorRow> newRows = new ArrayList<PredecessorRow>();
		Row selected = selectedRow();
		String label;
		if (selected == null || selected.contribution == null || dataModel == null) {
			label = "Select an association to see which associations have brought rows into its source table.";
		} else if (selected.association == null) {
			label = "Subject rows. They are the starting point of the collection, they have no predecessor.";
		} else {
			Table source = selected.association.source;
			for (CollectionAnalysis.Contribution candidate: contributions) {
				if (candidate == selected.contribution) {
					continue;
				}
				if (!source.equals(candidate.getDestination())) {
					continue;
				}
				long rowsOfCandidate = rowsOneStepBefore(selected.contribution, candidate);
				if (rowsOfCandidate <= 0) {
					continue;
				}
				PredecessorRow row = new PredecessorRow();
				row.contribution = candidate;
				row.association = candidate.getAssociation();
				row.name = row.association != null? row.association.getName() : "(subject)";
				row.from = row.association != null? dataModel.getDisplayName(row.association.source) : "";
				row.rows = rowsOfCandidate;
				row.step = stepOf(candidate);
				newRows.add(row);
			}
			String sourceName = dataModel.getDisplayName(source);
			if (newRows.isEmpty()) {
				label = "No association has brought rows into " + sourceName + " one step before "
						+ selected.association.getName() + " has been resolved.";
			} else {
				label = "Possible predecessors: rows brought into " + sourceName
						+ " one step before " + selected.association.getName() + " has been resolved.";
			}
		}
		predecessorRows = newRows;
		detailsTableModel.fireTableDataChanged();
		detailsLabel.setText(label);
	}

	/**
	 * Gets the number of rows a candidate has collected exactly one step before the
	 * association at hand has been resolved.
	 *
	 * @param contribution the contribution of the association at hand
	 * @param candidate a contribution into the source table of that association
	 * @return the number of rows, or 0 if the candidate is no possible predecessor
	 */
	private static long rowsOneStepBefore(CollectionAnalysis.Contribution contribution, CollectionAnalysis.Contribution candidate) {
		long result = 0;
		for (Integer day: contribution.getRowsPerDay().keySet()) {
			Long rowsOfDayBefore = candidate.getRowsPerDay().get(Integer.valueOf(day.intValue() - 1));
			if (rowsOfDayBefore != null) {
				result += rowsOfDayBefore.longValue();
			}
		}
		return result;
	}

	/**
	 * Returns whether a run keeps its collected rows, so that a way through the steps can be
	 * described at all.
	 *
	 * @return <code>true</code> if there is something to analyze
	 */
	public boolean hasRetainedRows() {
		return rowOriginContext != null && rowOriginContext.isAvailable();
	}

	/**
	 * Describes the way from a cell of the progress table to a subject, off the event dispatch
	 * thread, showing a dialog that can be cancelled.
	 *
	 * @param tableName name of the table of the cell
	 * @param day the collection step of the cell
	 * @return the steps, or <code>null</code> if cancelled, failed or nothing is retained
	 */
	public List<RowOriginPath.Step> buildPathFromCell(final String tableName, final int day) {
		if (!hasRetainedRows()) {
			return null;
		}
		final RowOriginContext context = rowOriginContext;
		final AtomicReference<JLabel> infoLabel = new AtomicReference<JLabel>();
		try {
			return ConcurrentTaskControl.call(SwingUtilities.getWindowAncestor(this),
					new Callable<List<RowOriginPath.Step>>() {
				@Override
				public List<RowOriginPath.Step> call() throws Exception {
					return pathFromCell(tableName, day, context, new Consumer<String>() {
						@Override
						public void accept(final String text) {
							UIUtil.invokeLater(new Runnable() {
								@Override
								public void run() {
									JLabel label = infoLabel.get();
									if (label != null) {
										label.setText(text);
									}
								}
							});
						}
					});
				}
				}, PROGRESS_WIDTH_RESERVE, UIUtil.blinkingInfoLabel(infoLabel));
		} catch (CancellationException e) {
			return null;
		} catch (Throwable t) {
			UIUtil.showException(this, "Error", t);
			return null;
		}
	}

	/**
	 * Describes the way from a cell of the progress table back to a subject, as browsers for the
	 * Data Browser.
	 * <p>
	 * The chain follows one of the rows of the cell back to a subject, as
	 * {@link net.sf.jailer.entitygraph.RowOriginFinder} finds it. That way has really been taken,
	 * so the chain holds together - one picked from row counts alone would not, since a choice
	 * which is the strongest in its own step need not lead out of the rows the step before it
	 * shows. The other ways into each link are added beside it, one level deep and not followed
	 * further, so that what has been taken is visible next to what has not.
	 * <p>
	 * Every browser is restricted to exactly the rows collected in its own step, which is why the
	 * retained entity-graph is needed. A link additionally shows only what can be joined to the
	 * rows its parent shows, so which end of the chain is the root decides which way that narrowing
	 * runs: {@link RowOriginPath#pathFromSelectionToSubject()}.
	 * <p>
	 * Talks to the database, so it must not be called on the event dispatch thread.
	 *
	 * @param tableName name of the table of the cell
	 * @param day the collection step of the cell
	 * @param context the context holding the retained rows
	 * @param progress told what is being worked on, or <code>null</code>
	 * @return the steps, the root first, or an empty list if nothing is known about that cell
	 */
	public List<RowOriginPath.Step> pathFromCell(String tableName, int day, RowOriginContext context,
			Consumer<String> progress) throws Exception {
		List<RowOriginPath.Step> path = new ArrayList<RowOriginPath.Step>();
		// the model of the run, not the one of the editor: the condition is held against the
		// rowIdSupport of the graph, and the two models have different universal primary keys
		DataModel runDataModel = context.getDataModel();
		if (runDataModel == null) {
			return path;
		}
		RemoteEntityGraph entityGraph = context.getEntityGraph();

		Table cellTable = runDataModel.getTable(tableName);
		if (cellTable == null) {
			return path;
		}

		// the backbone is a way which has really been taken: one of the rows of the cell, followed
		// back to a subject through the entity-graph. Which row it is does not matter - any of them
		// answers which tables and associations the chain consists of, and being a real way it
		// cannot fall apart the way a chain picked from row counts alone can
		report(progress, "following the chain");
		Object[] referenceKey = entityGraph.readAnyCollectedKey(cellTable, day);
		List<RowOriginStep> steps = referenceKey == null?
				null : context.createFinder().find(cellTable, referenceKey).getSteps();

		// what has been collected where, asked once per table and step
		Map<String, Map<Integer, Long>> collected = new HashMap<String, Map<Integer, Long>>();

		// the finder answers subject first, the chain is held the other way round: the cell first,
		// every further link one step back
		List<Link> chain = new ArrayList<Link>();
		if (steps == null || steps.isEmpty()) {
			// no row to follow: at least the cell itself, with the ways into it beside it
			chain.add(linkFor(cellTable, day, null, runDataModel, entityGraph, collected));
		} else {
			for (int i = steps.size() - 1; i >= 0; --i) {
				RowOriginStep step = steps.get(i);
				report(progress, "step " + (steps.size() - i) + " of " + steps.size());
				// the association which has brought this row, so its source is the table of the
				// next link - the same meaning the chain has always given this field
				Link link = linkFor(step.getTable(), step.getBirthday(), step.getIncomingAssociation(),
						runDataModel, entityGraph, collected);
				// no association has brought the row the chain ends with: it is a subject row
				link.subjectEnd = step.getIncomingAssociation() == null;
				chain.add(link);
			}
		}

		// the chain first. The comment always names the association which has brought the rows of a
		// link, whichever way the browsers are linked
		if (RowOriginPath.pathFromSelectionToSubject()) {
			// the cell to the left, the subject to the right
			for (int i = 0; i < chain.size(); ++i) {
				Link link = chain.get(i);
				// the association of the parent brought the parent's rows and its source is the
				// table of this link, so its reversal is the way from the parent to here - the same
				// shape the alternatives have
				Association ofParent = i == 0? null : chain.get(i - 1).main;
				path.add(new RowOriginPath.Step(link.table.getName(),
						ofParent == null? null : reversalNameOf(ofParent),
						entityGraph.collectedInStepCondition(link.table, link.step, "A",
								commentFor(link.step, link.main == null? null : link.main.getName(), link.subjectEnd)),
						i - 1,
						link.rowCount));
			}
		} else {
			// the subject to the left, the cell to the right: a link is reached through the
			// association which has brought its rows, whose source is the table of its parent
			for (int i = chain.size() - 1; i >= 0; --i) {
				Link link = chain.get(i);
				boolean isRoot = i == chain.size() - 1;
				path.add(new RowOriginPath.Step(link.table.getName(),
						isRoot || link.main == null? null : link.main.getName(),
						entityGraph.collectedInStepCondition(link.table, link.step, "A",
								commentFor(link.step, link.main == null? null : link.main.getName(), link.subjectEnd)),
						isRoot? -1 : path.size() - 1,
						link.rowCount));
			}
		}

		// the alternatives afterwards, so that the chain stays the linear opening stretch of the
		// list and its end can be found without a mark of its own
		for (int i = 0; i < chain.size(); ++i) {
			Link link = chain.get(i);
			int linkIndex = RowOriginPath.pathFromSelectionToSubject()? i : chain.size() - 1 - i;
			int branch = 0;
			for (Association association: link.branches) {
				report(progress, "side branch " + (++branch) + " of step " + (i + 1));
				addAlternative(path, runDataModel, entityGraph, association, link.step, linkIndex, collected);
			}
		}
		return path;
	}

	/**
	 * Tells what is being worked on, if anybody is listening.
	 *
	 * @param progress the listener, or <code>null</code>
	 * @param what what is being worked on
	 */
	private static void report(Consumer<String> progress, String what) {
		if (progress != null) {
			progress.accept(progressText(what));
		}
	}

	/**
	 * The wording of the progress.
	 *
	 * @param what what is being worked on
	 * @return the text
	 */
	private static String progressText(String what) {
		return "Preparing path... " + what;
	}

	/**
	 * The widest wording the progress can take.
	 * <p>
	 * The dialog is packed for the text it is opened with and does not grow afterwards, so it is
	 * opened with this one; the first real message replaces it before the dialog has faded in. The
	 * messages stay below it: a link carries at most {@link #MAX_BRANCHES_PER_LINK} branches, and
	 * chains of more than 999 steps do not occur.
	 */
	private static final String PROGRESS_WIDTH_RESERVE = progressText("side branch 99 of step 999");

	/**
	 * Builds one link of the chain out of what the entity-graph knows about its step: how many rows
	 * have been collected into that table, and through which associations.
	 *
	 * @param table the table of the link
	 * @param step the collection step
	 * @param main the association the chain follows out of this link, <code>null</code> for the
	 *        subject end
	 * @param runDataModel the data model of the run, which the association ids are resolved against
	 * @param entityGraph the graph
	 * @param collected what has been collected per table and step, filled as it is asked for
	 * @return the link
	 */
	private static Link linkFor(Table table, int step, Association main, DataModel runDataModel,
			RemoteEntityGraph entityGraph, Map<String, Map<Integer, Long>> collected) throws SQLException {
		Link link = new Link();
		link.table = table;
		link.step = step;
		link.main = main;
		Map<Integer, Long> perAssociation = collectedIn(table, step, entityGraph, collected);
		link.rowCount = totalOf(perAssociation);
		link.branches = new ArrayList<Association>();
		for (Map.Entry<Integer, Long> e: perAssociation.entrySet()) {
			if (e.getKey() == null) {
				continue;   // subject rows: no association, so no source table to open
			}
			if (main != null && e.getKey().intValue() == main.getId()) {
				continue;   // the way the chain itself follows, not to be opened a second time
			}
			Association association = runDataModel.getAssociationById(e.getKey().intValue());
			if (association != null) {
				// unknown means the data model has changed since the run: leave that branch out
				link.branches.add(association);
			}
		}
		sortByRows(link.branches, perAssociation);
		while (link.branches.size() > MAX_BRANCHES_PER_LINK) {
			link.branches.remove(link.branches.size() - 1);
		}
		return link;
	}

	/**
	 * How many branches are opened beside a link at most. Sorted by the number of rows they have
	 * brought, so what is left out are the weakest ways into that step.
	 */
	private static final int MAX_BRANCHES_PER_LINK = 4;

	/**
	 * Gets what has been collected into a table in a step, per association, asking the graph only
	 * once per table and step. A chain and its branches ask for the same steps repeatedly.
	 *
	 * @param table the table
	 * @param step the collection step
	 * @param entityGraph the graph
	 * @param collected the answers so far
	 * @return the number of rows per association id, the key <code>null</code> for subject rows
	 */
	private static Map<Integer, Long> collectedIn(Table table, int step, RemoteEntityGraph entityGraph,
			Map<String, Map<Integer, Long>> collected) throws SQLException {
		String key = table.getName() + "@" + step;
		Map<Integer, Long> perAssociation = collected.get(key);
		if (perAssociation == null) {
			perAssociation = entityGraph.readCollectedPerAssociation(table, step);
			collected.put(key, perAssociation);
		}
		return perAssociation;
	}

	/**
	 * The rows a "collected in this step" condition selects: everything of that step, whichever
	 * association has brought it.
	 *
	 * @param perAssociation the rows per association
	 * @return the number of rows, capped at {@link Integer#MAX_VALUE}
	 */
	private static int totalOf(Map<Integer, Long> perAssociation) {
		long sum = 0;
		for (Long rows: perAssociation.values()) {
			sum += rows.longValue();
		}
		return sum > Integer.MAX_VALUE? Integer.MAX_VALUE : (int) sum;
	}

	/**
	 * Sorts associations by the number of rows they have brought, the strongest first.
	 */
	private static void sortByRows(List<Association> associations, final Map<Integer, Long> perAssociation) {
		Collections.sort(associations, new Comparator<Association>() {
			@Override
			public int compare(Association a, Association b) {
				Long rowsA = perAssociation.get(Integer.valueOf(a.getId()));
				Long rowsB = perAssociation.get(Integer.valueOf(b.getId()));
				return Long.compare(rowsB == null? 0 : rowsB.longValue(), rowsA == null? 0 : rowsA.longValue());
			}
		});
	}

	/**
	 * One link of the chain, in the order in which it is walked: the cell first, the subject last.
	 * Written out in that order or the other way round, see
	 * {@link RowOriginPath#pathFromSelectionToSubject()}.
	 */
	private static class Link {
		Table table;
		int step;
		/**
		 * Number of rows collected into this table in this step, which is what the condition of
		 * this link selects.
		 */
		int rowCount;
		/**
		 * The <b>other</b> ways into this table in this step, the strongest first: opened beside
		 * this link and not followed further. The one the chain follows is not among them.
		 */
		List<Association> branches;
		/**
		 * The association the chain follows out of this link, towards the subject.
		 */
		Association main;
		/**
		 * Whether this link holds the subject rows the chain ends with. Not every link without an
		 * association does: if no row of a cell has been found, the way into it is just unknown.
		 */
		boolean subjectEnd;
	}

	/**
	 * Adds the browser of an alternative way into a link: the source table of that association,
	 * one step earlier, hanging on the link itself.
	 */
	private void addAlternative(List<RowOriginPath.Step> path, DataModel runDataModel, RemoteEntityGraph entityGraph,
			Association association, int step, int parentIndex, Map<String, Map<Integer, Long>> collected) throws SQLException {
		if (step - 1 < 0) {
			return;
		}
		// by name, out of the model of the run: see pathFromCell
		Table source = runDataModel.getTable(association.source.getName());
		if (source == null) {
			return;
		}
		path.add(new RowOriginPath.Step(source.getName(), reversalNameOf(association),
				entityGraph.collectedInStepCondition(source, step - 1, "A",
						commentFor(step - 1, association.getName(), false) + ", an alternative way"),
				parentIndex,
				totalOf(collectedIn(source, step - 1, entityGraph, collected))));
	}

	/**
	 * The comment which goes in front of the condition of a browser, so that the statement says
	 * what it selects.
	 */
	private static String commentFor(int step, String associationName, boolean subjectRows) {
		if (subjectRows) {
			return "collected in step " + step + " (the subject rows)";
		}
		// without an association the way in is unknown, e.g. no row of the cell has been found
		return associationName == null?
				"collected in step " + step
				: "collected in step " + step + " through " + associationName;
	}

	/**
	 * The chain runs towards smaller steps, so a link is reached by navigating the association
	 * which has brought its parent's rows in the other direction.
	 */
	private static String reversalNameOf(Association association) {
		return association.reversalAssociation == null? null : association.reversalAssociation.getName();
	}

	private Row selectedRow() {
		int viewRow = table.getSelectedRow();
		if (viewRow < 0) {
			return null;
		}
		int modelRow = table.convertRowIndexToModel(viewRow);
		if (modelRow < 0 || modelRow >= rows.size()) {
			return null;
		}
		return rows.get(modelRow);
	}

	private <T> T rowAt(MouseEvent e, JTable theTable, List<T> theRows) {
		int viewRow = theTable.rowAtPoint(e.getPoint());
		if (viewRow < 0) {
			return null;
		}
		int modelRow = theTable.convertRowIndexToModel(viewRow);
		if (modelRow < 0 || modelRow >= theRows.size()) {
			return null;
		}
		return theRows.get(modelRow);
	}

	private Association associationAt(MouseEvent e, JTable theTable) {
		Row row = rowAt(e, theTable, rows);
		return row == null? null : row.association;
	}

	private Association predecessorAt(MouseEvent e) {
		PredecessorRow row = rowAt(e, detailsTable, predecessorRows);
		return row == null? null : row.association;
	}

	/**
	 * Gets the table the rows of a row of one of the two tables have been collected into. Also
	 * known for the row "(subject)", which has no association.
	 *
	 * @param e the event which has opened the menu
	 * @param theTable the table which has been clicked
	 * @return the table, or <code>null</code>
	 */
	private Table destinationAt(MouseEvent e, JTable theTable) {
		CollectionAnalysis.Contribution contribution;
		if (theTable == detailsTable) {
			PredecessorRow row = rowAt(e, detailsTable, predecessorRows);
			contribution = row == null? null : row.contribution;
		} else {
			Row row = rowAt(e, table, rows);
			contribution = row == null? null : row.contribution;
		}
		return contribution == null? null : contribution.getDestination();
	}

	private void selectInModel(Association association) {
		Association resolved = resolveInModel(association);
		if (resolved != null && associationSelector != null) {
			associationSelector.accept(resolved);
		}
	}

	/**
	 * Resolves an association against the editor's data model.
	 *
	 * @param association the association
	 * @return the association of the editor's model, or <code>null</code>
	 */
	private Association resolveInModel(Association association) {
		if (association == null || dataModel == null) {
			return null;
		}
		// association belongs to the run's own data model, a different load than the editor's -
		// re-resolve it by name against the editor's model before handing it off, the same way
		// pathFromCell already has to (see runDataModel.getTable(...) there)
		return dataModel.namedAssociations.get(association.getName());
	}

	/**
	 * Creates a menu item with an icon.
	 *
	 * @param title the title
	 * @param image the resource of the icon
	 * @return the menu item
	 */
	private static JMenuItem menuItem(String title, String image) {
		JMenuItem item = new JMenuItem(title);
		item.setIcon(UIUtil.scaleIcon(item, UIUtil.readImage(image)));
		return item;
	}

	/**
	 * Draws a red stroke across an icon, from the lower left to the upper right corner.
	 *
	 * @param icon the icon
	 * @return the struck through icon
	 */
	private static ImageIcon struckThrough(ImageIcon icon) {
		int w = icon.getIconWidth();
		int h = icon.getIconHeight();
		if (w <= 0 || h <= 0) {
			return icon;
		}
		BufferedImage image = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB);
		Graphics2D g = image.createGraphics();
		try {
			g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
			g.drawImage(icon.getImage(), 0, 0, null);
			float width = Math.max(1.5f, Math.min(w, h) / 7f);
			g.setStroke(new BasicStroke(width, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
			g.setColor(new Color(220, 0, 0));
			int inset = (int) Math.ceil(width);
			g.drawLine(inset, h - 1 - inset, w - 1 - inset, inset);
		} finally {
			g.dispose();
		}
		return new ImageIcon(image);
	}

	private void maybeShowPopup(MouseEvent e, JTable theTable, final Association association) {
		if (!e.isPopupTrigger()) {
			return;
		}
		int viewRow = theTable.rowAtPoint(e.getPoint());
		if (viewRow >= 0) {
			theTable.getSelectionModel().setSelectionInterval(viewRow, viewRow);
		}
		JPopupMenu popup = new JPopupMenu();
		if (associationSelector != null) {
			JMenuItem select = menuItem("Select Association", "/select.png");
			select.setToolTipText("Select this association in the extraction model, in order to restrict it.");
			select.setEnabled(association != null);
			select.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent evt) {
					selectInModel(association);
				}
			});
			popup.add(select);
			if (associationDisabler != null) {
				final Association resolved = resolveInModel(association);
				JMenuItem disable = menuItem("Disable Association", "/select.png");
				// the icon of "Select Association", struck through
				disable.setIcon(struckThrough((ImageIcon) disable.getIcon()));
				disable.setToolTipText("Disables this association in the extraction model. Do a Dry Run again to see the effect in the \"Change\" column.");
				// as "Disable association..." of the closure view: never a dependency
				disable.setEnabled(resolved != null && !resolved.isInsertDestinationBeforeSource() && !resolved.isIgnored());
				disable.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent evt) {
						associationDisabler.accept(resolved);
					}
				});
				popup.add(disable);
			}
			popup.addSeparator();
		}
		if (rowOriginContext != null && rowOriginContext.isAvailable()) {
			JMenuItem showRows = menuItem("Show Collected Rows", "/findcolumn2.png");
			showRows.setToolTipText("Lists the rows collected through this association, and shows for a selected one how it has found its way into the subset.");
			showRows.setEnabled(association != null);
			showRows.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent evt) {
					new RowOriginDialog(SwingUtilities.getWindowAncestor(CollectionAnalysisPanel.this),
							rowOriginContext, association, pathOpener).setVisible(true);
				}
			});
			popup.add(showRows);
		}
		if (collectedRowsThroughOpener != null) {
			final Table destination = destinationAt(e, theTable);
			JMenuItem showInBrowser = menuItem("Show Collected Rows in Data Browser", "/navigation.png");
			showInBrowser.setToolTipText("Opens the table in the Data Browser, limited to the rows collected through this association.");
			showInBrowser.setEnabled(destination != null && hasRetainedRows());
			showInBrowser.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent evt) {
					collectedRowsThroughOpener.accept(destination, association);
				}
			});
			popup.add(showInBrowser);
		}
		if (tableOpener != null) {
			final Table destination = destinationAt(e, theTable);
			JMenuItem openInBrowser = menuItem("Open in Data Browser", "/table.png");
			// only beside the analysis of the collected rows: without it, the whole table says nothing about the subset
			boolean available = hasRetainedRows();
			openInBrowser.setToolTipText(available?
					"Opens the table these rows have been collected into in the Data Browser, without any condition."
					: "Not available: the collected rows of this run have not been kept for the analysis.");
			openInBrowser.setEnabled(destination != null && available);
			openInBrowser.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent evt) {
					tableOpener.accept(destination);
				}
			});
			popup.add(openInBrowser);
		}
		if (popup.getComponentCount() > 0) {
			popup.addSeparator();
		}
		JMenuItem export = menuItem("Export as CSV...", "/export.png");
		export.setToolTipText("Saves this table as CSV file, in the current order, only the rows shown.");
		export.setEnabled(theTable.getRowCount() > 0);
		export.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent evt) {
				exportAsCsv(theTable);
			}
		});
		popup.add(export);
		popup.show(theTable, e.getX(), e.getY());
	}

	/**
	 * Saves a table as CSV file, in the format the user chooses (see {@link CsvOptionsDialog}).
	 * The rows are written in the order the table currently shows them.
	 *
	 * @param theTable the table
	 */
	private void exportAsCsv(JTable theTable) {
		CsvOptionsDialog.CsvFormat format = CsvOptionsDialog.show(this);
		if (format == null) {
			return;
		}
		String fileName = CsvOptionsDialog.chooseFile(this);
		if (fileName == null) {
			return;
		}
		TableModel model = theTable.getModel();
		StringBuilder csv = new StringBuilder();
		if (format.header) {
			for (int column = 0; column < model.getColumnCount(); ++column) {
				if (column > 0) {
					csv.append(format.separator);
				}
				// shares are written as percentages
				String name = model.getColumnName(column) + (model.getColumnClass(column) == Double.class? " (%)" : "");
				csv.append(format.cell(name, true));
			}
			csv.append(format.lineBreak);
		}
		for (int viewRow = 0; viewRow < theTable.getRowCount(); ++viewRow) {
			int modelRow = theTable.convertRowIndexToModel(viewRow);
			for (int column = 0; column < model.getColumnCount(); ++column) {
				if (column > 0) {
					csv.append(format.separator);
				}
				csv.append(csvCell(format, model.getValueAt(modelRow, column)));
			}
			csv.append(format.lineBreak);
		}
		try {
			format.write(fileName, csv.toString());
		} catch (Throwable t) {
			UIUtil.showException(this, "Error", t);
		}
	}

	/**
	 * Gets the CSV cell of a value of the tables: numbers as numbers, shares as percentages.
	 *
	 * @param format the format
	 * @param value the value
	 * @return the cell
	 */
	private static String csvCell(CsvOptionsDialog.CsvFormat format, Object value) {
		if (value == null) {
			return "";
		}
		if (value instanceof Long) {
			return format.number(BigDecimal.valueOf(((Long) value).longValue()));
		}
		if (value instanceof Change) {
			return format.number(BigDecimal.valueOf(((Change) value).value));
		}
		if (value instanceof Ratio) {
			return format.number(BigDecimal.valueOf(((Ratio) value).value).setScale(2, RoundingMode.HALF_UP));
		}
		if (value instanceof Double) {
			return format.number(BigDecimal.valueOf(((Double) value).doubleValue() * 100).setScale(1, RoundingMode.HALF_UP));
		}
		return format.cell(value.toString(), true);
	}

	/**
	 * Model of the upper table.
	 */
	private class AnalysisTableModel extends AbstractTableModel {

		private final String[] columnNames = new String[] { "Association", "From", "To", "Rows", "Change", "Per Source Row", "Share", "Cumulated", "Step" };

		private final String[] columnToolTips = new String[] {
				"<html>The association through which the rows have been collected.<br>\"(subject)\" stands for the subject rows.</html>",
				"Source table of the association.",
				"Destination table, the table the rows have been collected into.",
				"<html>Rows collected through this association.<br>Each row is counted only for the association that has collected it first.</html>",
				"<html>Change in rows compared to the previous completed run of this extraction model.<br>Empty if there is no such run."
				+ "<br>While the run is going, the completed steps are compared with the same steps of the previous run.</html>",
				"<html>Rows divided by the rows collected into the source table.<br>A value far above 1 marks an association that multiplies the subset.</html>",
				"Share of this association in all collected rows.",
				"<html>Share of this entry together with all entries that have collected more rows.</html>",
				"<html>The step, or range of steps, in which this association has collected rows.<br>Hover over a cell to see the rows per step.</html>"
		};

		@Override
		public int getRowCount() {
			return rows.size();
		}

		@Override
		public int getColumnCount() {
			return columnNames.length;
		}

		@Override
		public String getColumnName(int column) {
			return columnNames[column];
		}

		@Override
		public Class<?> getColumnClass(int column) {
			switch (column) {
			case 3: return Long.class;
			case 4: return Change.class;
			case 5: return Ratio.class;
			case 6:
			case 7: return Double.class;
			case 8: return Steps.class;
			default: return String.class;
			}
		}

		@Override
		public boolean isCellEditable(int row, int column) {
			return false;
		}

		@Override
		public Object getValueAt(int rowIndex, int columnIndex) {
			if (rowIndex < 0 || rowIndex >= rows.size()) {
				return null;
			}
			Row row = rows.get(rowIndex);
			switch (columnIndex) {
			case 0: return row.name;
			case 1: return row.from;
			case 2: return row.to;
			case 3: return Long.valueOf(row.rows);
			case 4: return row.change;
			case 5: return row.perSourceRow;
			case 6: return Double.valueOf(row.share);
			case 7: return Double.valueOf(row.cumulatedShare);
			case 8: return row.step;
			default: return null;
			}
		}

		private static final long serialVersionUID = -6349839114905968689L;
	}

	/**
	 * Model of the lower table.
	 */
	private class PredecessorTableModel extends AbstractTableModel {

		private final String[] columnNames = new String[] { "Possible Predecessor", "From", "Rows", "Step" };

		private final String[] columnToolTips = new String[] {
				"<html>An association which has brought rows into the source table of the selected association<br>one step before the selected association has been resolved.</html>",
				"Source table of the possible predecessor.",
				"<html>Rows the predecessor has brought in one step before<br>the selected association has been resolved.</html>",
				"<html>The step, or range of steps, in which the predecessor has collected rows.<br>Hover over a cell to see the rows per step.</html>"
		};

		@Override
		public int getRowCount() {
			return predecessorRows.size();
		}

		@Override
		public int getColumnCount() {
			return columnNames.length;
		}

		@Override
		public String getColumnName(int column) {
			return columnNames[column];
		}

		@Override
		public Class<?> getColumnClass(int column) {
			return column == 2? Long.class : column == 3? Steps.class : String.class;
		}

		@Override
		public boolean isCellEditable(int row, int column) {
			return false;
		}

		@Override
		public Object getValueAt(int rowIndex, int columnIndex) {
			if (rowIndex < 0 || rowIndex >= predecessorRows.size()) {
				return null;
			}
			PredecessorRow row = predecessorRows.get(rowIndex);
			switch (columnIndex) {
			case 0: return row.name;
			case 1: return row.from;
			case 2: return Long.valueOf(row.rows);
			case 3: return row.step;
			default: return null;
			}
		}

		private static final long serialVersionUID = 7519104155699259321L;
	}

	private static final long serialVersionUID = 5379269144654921962L;

}
