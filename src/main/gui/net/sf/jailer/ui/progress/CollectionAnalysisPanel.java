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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.sql.SQLException;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.function.Consumer;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.RowSorter.SortKey;
import javax.swing.SortOrder;
import javax.swing.SwingConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableRowSorter;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.ModelElement;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.entitygraph.RowOriginStep;
import net.sf.jailer.entitygraph.remote.RemoteEntityGraph;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.util.ConcurrentTaskControl;
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
		double share;
		double cumulatedShare;
		String step;
		Association association;
		CollectionAnalysis.Contribution contribution;
	}

	/**
	 * A single row of the lower table.
	 */
	private static class PredecessorRow {
		String name;
		String from;
		long rows;
		String step;
		Association association;
		CollectionAnalysis.Contribution contribution;
	}

	private final JLabel summaryLabel;
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
	private RowOriginContext rowOriginContext;
	private Runnable discardAction;
	private Consumer<List<RowOriginStep>> pathOpener;
	private Consumer<Table> tableOpener;

	private final NumberFormat percentFormat = createPercentFormat();

	/**
	 * Constructor.
	 */
	public CollectionAnalysisPanel() {
		setLayout(new BorderLayout());

		summaryLabel = new JLabel(" ");
		summaryLabel.setBorder(BorderFactory.createEmptyBorder(4, 6, 4, 4));
		summaryLabel.setToolTipText(
				"Each row is counted for the association through which it has been collected first. "
				+ "Rows which are reachable via several associations are not counted more than once.");
		add(summaryLabel, BorderLayout.NORTH);

		tableModel = new AnalysisTableModel();
		table = createTable(tableModel, 3);
		TableRowSorter<AnalysisTableModel> sorter = new TableRowSorter<AnalysisTableModel>(tableModel);
		sorter.setSortKeys(Collections.singletonList(new SortKey(3, SortOrder.DESCENDING)));
		table.setRowSorter(sorter);
		table.getColumnModel().getColumn(0).setPreferredWidth(200);
		table.getColumnModel().getColumn(1).setPreferredWidth(140);
		table.getColumnModel().getColumn(2).setPreferredWidth(140);
		table.getColumnModel().getColumn(3).setPreferredWidth(80);
		table.getColumnModel().getColumn(4).setPreferredWidth(60);
		table.getColumnModel().getColumn(5).setPreferredWidth(80);
		table.getColumnModel().getColumn(6).setPreferredWidth(60);
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
		detailsTable = createTable(detailsTableModel, 2);
		TableRowSorter<PredecessorTableModel> detailsSorter = new TableRowSorter<PredecessorTableModel>(detailsTableModel);
		detailsSorter.setSortKeys(Collections.singletonList(new SortKey(2, SortOrder.DESCENDING)));
		detailsTable.setRowSorter(detailsSorter);
		detailsTable.getColumnModel().getColumn(0).setPreferredWidth(200);
		detailsTable.getColumnModel().getColumn(1).setPreferredWidth(140);
		detailsTable.getColumnModel().getColumn(2).setPreferredWidth(80);
		detailsTable.getColumnModel().getColumn(3).setPreferredWidth(60);
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

	private static NumberFormat createPercentFormat() {
		NumberFormat format = NumberFormat.getPercentInstance();
		format.setMaximumFractionDigits(1);
		return format;
	}

	/**
	 * Creates a table with the look of the other tables of the application.
	 *
	 * @param model the model
	 * @param firstRightAlignedColumn index of the first column which is aligned to the right
	 * @return the table
	 */
	private JTable createTable(final AbstractTableModel model, final int firstRightAlignedColumn) {
		final JTable newTable = new JTable(model);
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
				}
				Component render = defaultRenderer.getTableCellRendererComponent(table, renderedValue, isSelected, false, row, column);
				if (!isSelected) {
					render.setBackground((row % 2 == 0) ? BG1 : BG2);
				}
				if (render instanceof JLabel) {
					((JLabel) render).setHorizontalAlignment(column >= firstRightAlignedColumn? SwingConstants.RIGHT : SwingConstants.LEFT);
					String valueAsString = renderedValue == null? "" : String.valueOf(renderedValue);
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
		contributions = analysis.getContributions();
		long total = analysis.getTotal();
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
			summaryLabel.setText(summary.toString());
		}

		updateDetails();
	}

	private static String stepOf(CollectionAnalysis.Contribution contribution) {
		return contribution.getFirstDay() == contribution.getLastDay()?
				String.valueOf(contribution.getFirstDay())
				: contribution.getFirstDay() + " - " + contribution.getLastDay();
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
		try {
			return ConcurrentTaskControl.call(SwingUtilities.getWindowAncestor(this),
					new Callable<List<RowOriginPath.Step>>() {
				@Override
				public List<RowOriginPath.Step> call() throws Exception {
					return pathFromCell(tableName, day, context);
				}
			}, "Preparing path...", null);
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
	 * The chain follows, at every fork, the predecessor which has contributed <b>more</b> rows - the
	 * same yardstick the lower table uses for its possible predecessors. The other predecessors of
	 * each link are added beside it, one level deep and not followed further, so that what has been
	 * chosen is visible next to what has not.
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
	 * @return the steps, the root first, or an empty list if nothing is known about that cell
	 */
	public List<RowOriginPath.Step> pathFromCell(String tableName, int day, RowOriginContext context) throws Exception {
		List<RowOriginPath.Step> path = new ArrayList<RowOriginPath.Step>();
		// the model of the run, not the one of the editor: the condition is held against the
		// rowIdSupport of the graph, and the two models have different universal primary keys
		DataModel runDataModel = context.getDataModel();
		if (runDataModel == null) {
			return path;
		}
		RemoteEntityGraph entityGraph = context.getEntityGraph();

		// walked from the cell towards the subject, and written out in that same order: the cell is
		// the root, and every further link goes one step back
		List<Link> chain = new ArrayList<Link>();
		String currentName = tableName;
		int step = day;
		while (currentName != null && step >= 0) {
			Table currentTable = runDataModel.getTable(currentName);
			if (currentTable == null) {
				break;
			}
			// every way into this table in this step: one of them carries the chain on, the others
			// are opened beside this link and not followed further
			List<CollectionAnalysis.Contribution> into = contributionsInto(currentName, step);
			Link link = new Link();
			link.table = currentTable;
			link.step = step;
			link.into = into;
			chain.add(link);
			if (into.isEmpty()) {
				break;
			}
			Association main = into.get(0).getAssociation();
			if (main == null) {
				break;      // subject rows: the start of the collection
			}
			link.main = main;
			currentName = main.source.getName();
			step -= 1;
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
								commentFor(link.step, link.main == null? null : link.main.getName())),
						i - 1,
						rowsCollectedIn(link.table.getName(), link.step)));
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
								commentFor(link.step, link.main == null? null : link.main.getName())),
						isRoot? -1 : path.size() - 1,
						rowsCollectedIn(link.table.getName(), link.step)));
			}
		}

		// the alternatives afterwards, so that the chain stays the linear opening stretch of the
		// list and its end can be found without a mark of its own
		for (int i = 0; i < chain.size(); ++i) {
			Link link = chain.get(i);
			int linkIndex = RowOriginPath.pathFromSelectionToSubject()? i : chain.size() - 1 - i;
			for (int k = 1; k < link.into.size(); ++k) {
				addAlternative(path, runDataModel, entityGraph, link.into.get(k), link.step, linkIndex);
			}
		}
		return path;
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
		 * All ways into this table in this step, the strongest first.
		 */
		List<CollectionAnalysis.Contribution> into;
		/**
		 * The association the chain follows out of this link, towards the subject.
		 */
		Association main;
	}

	/**
	 * Adds the browser of an alternative way into a link: the source table of that association,
	 * one step earlier, hanging on the link itself.
	 */
	private void addAlternative(List<RowOriginPath.Step> path, DataModel runDataModel, RemoteEntityGraph entityGraph,
			CollectionAnalysis.Contribution alternative, int step, int parentIndex) throws SQLException {
		Association association = alternative.getAssociation();
		if (association == null || step - 1 < 0) {
			return;
		}
		// by name, out of the model of the run: see pathFromCell
		Table source = runDataModel.getTable(association.source.getName());
		if (source == null) {
			return;
		}
		path.add(new RowOriginPath.Step(source.getName(), reversalNameOf(association),
				entityGraph.collectedInStepCondition(source, step - 1, "A",
						commentFor(step - 1, association.getName()) + ", an alternative way"),
				parentIndex,
				rowsCollectedIn(source.getName(), step - 1)));
	}

	/**
	 * Gets how many rows have been collected into a table in a step. Every row is collected once
	 * and attributed to one contribution, so summing them gives the rows the condition of that
	 * link selects.
	 *
	 * @param tableName name of the table
	 * @param step the collection step
	 * @return the number of rows, capped at {@link Integer#MAX_VALUE}
	 */
	private int rowsCollectedIn(String tableName, int step) {
		long sum = 0;
		for (CollectionAnalysis.Contribution contribution: contributionsInto(tableName, step)) {
			sum += rowsAt(contribution, step);
		}
		return sum > Integer.MAX_VALUE? Integer.MAX_VALUE : (int) sum;
	}

	/**
	 * Gets the contributions which have brought rows into a table in a given step, the one with
	 * the most rows first. That is the yardstick for "which predecessor contributed more".
	 */
	private List<CollectionAnalysis.Contribution> contributionsInto(String tableName, final int day) {
		List<CollectionAnalysis.Contribution> result = new ArrayList<CollectionAnalysis.Contribution>();
		for (CollectionAnalysis.Contribution candidate: contributions) {
			if (candidate.getDestination() == null || !candidate.getDestination().getName().equals(tableName)) {
				continue;
			}
			if (candidate.getRowsPerDay().get(Integer.valueOf(day)) != null) {
				result.add(candidate);
			}
		}
		Collections.sort(result, new Comparator<CollectionAnalysis.Contribution>() {
			@Override
			public int compare(CollectionAnalysis.Contribution a, CollectionAnalysis.Contribution b) {
				return Long.compare(rowsAt(b, day), rowsAt(a, day));
			}
		});
		return result;
	}

	private static long rowsAt(CollectionAnalysis.Contribution contribution, int day) {
		Long rows = contribution.getRowsPerDay().get(Integer.valueOf(day));
		return rows == null? 0 : rows.longValue();
	}

	/**
	 * The comment which goes in front of the condition of a browser, so that the statement says
	 * what it selects.
	 */
	private static String commentFor(int step, String associationName) {
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
		if (association != null && associationSelector != null) {
			associationSelector.accept(association);
		}
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
			JMenuItem restrict = new JMenuItem("Restrict Association");
			restrict.setToolTipText("Select this association in the extraction model, in order to restrict it.");
			restrict.setEnabled(association != null);
			restrict.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent evt) {
					selectInModel(association);
				}
			});
			popup.add(restrict);
			popup.addSeparator();
		}
		if (tableOpener != null) {
			final Table destination = destinationAt(e, theTable);
			JMenuItem openInBrowser = new JMenuItem("Open in Data Browser");
			openInBrowser.setToolTipText("Opens the table these rows have been collected into in the Data Browser, without any condition.");
			openInBrowser.setEnabled(destination != null);
			openInBrowser.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent evt) {
					tableOpener.accept(destination);
				}
			});
			popup.add(openInBrowser);
			popup.addSeparator();
		}
		if (rowOriginContext != null && rowOriginContext.isAvailable()) {
			JMenuItem showRows = new JMenuItem("Show collected rows...");
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
			if (discardAction != null) {
				JMenuItem discard = new JMenuItem("Discard analysis data");
				discard.setToolTipText("Deletes the collected rows from the working tables. Afterwards the origin of a row can no longer be analyzed.");
				discard.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent evt) {
						discardAction.run();
					}
				});
				popup.add(discard);
			}
			popup.addSeparator();
		}
		JMenuItem copy = new JMenuItem("Copy to Clipboard");
		copy.setToolTipText("Copy the whole table as text, in the order shown.");
		copy.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent evt) {
				UIUtil.setClipboardContent(new StringSelection(asText()));
			}
		});
		popup.add(copy);
		popup.show(theTable, e.getX(), e.getY());
	}

	/**
	 * Gets the content of the upper table as tab separated text, in the order currently shown.
	 *
	 * @return the content as text
	 */
	private String asText() {
		StringBuilder result = new StringBuilder();
		for (int i = 0; i < tableModel.getColumnCount(); ++i) {
			result.append(i > 0? "\t" : "").append(tableModel.getColumnName(i));
		}
		result.append("\n");
		for (int viewRow = 0; viewRow < table.getRowCount(); ++viewRow) {
			int modelRow = table.convertRowIndexToModel(viewRow);
			if (modelRow < 0 || modelRow >= rows.size()) {
				continue;
			}
			Row row = rows.get(modelRow);
			result.append(row.name).append("\t")
				.append(row.from).append("\t")
				.append(row.to).append("\t")
				.append(row.rows).append("\t")
				.append(percentFormat.format(row.share)).append("\t")
				.append(percentFormat.format(row.cumulatedShare)).append("\t")
				.append(row.step).append("\n");
		}
		return result.toString();
	}

	/**
	 * Model of the upper table.
	 */
	private class AnalysisTableModel extends AbstractTableModel {

		private final String[] columnNames = new String[] { "Association", "From", "To", "Rows", "Share", "Cumulated", "Step" };

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
			case 4:
			case 5: return Double.class;
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
			case 4: return Double.valueOf(row.share);
			case 5: return Double.valueOf(row.cumulatedShare);
			case 6: return row.step;
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
			return column == 2? Long.class : String.class;
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
