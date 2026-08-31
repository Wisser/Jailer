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
import java.awt.Window;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellRenderer;

import net.sf.jailer.datamodel.Table;
import net.sf.jailer.entitygraph.RowOrigin;
import net.sf.jailer.entitygraph.RowOriginStep;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.util.ConcurrentTaskControl;
import net.sf.jailer.util.CancellationException;

/**
 * Shows the way a single row has taken into the subset: the chain from the subject down to
 * that row, one line per step.
 * <p>
 * Takes nothing but a {@link RowOriginContext}, a table and the primary key values of a row, so
 * that it can be shown wherever that question comes up.
 *
 * @author Ralf Wisser
 */
public class RowOriginPanel extends JPanel {

	private final RowOriginContext context;
	private final JLabel statusLabel;
	private final JTable table;
	private final ChainTableModel tableModel;

	private List<RowOriginStep> steps = new ArrayList<RowOriginStep>();

	/**
	 * Constructor.
	 *
	 * @param context the context to ask
	 */
	public RowOriginPanel(RowOriginContext context) {
		this.context = context;
		setLayout(new BorderLayout());

		statusLabel = new JLabel(" ");
		statusLabel.setBorder(BorderFactory.createEmptyBorder(4, 6, 4, 4));
		add(statusLabel, BorderLayout.NORTH);

		tableModel = new ChainTableModel();
		table = new JTable(tableModel);
		table.setAutoResizeMode(JTable.AUTO_RESIZE_LAST_COLUMN);
		table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		table.setShowGrid(false);

		final TableCellRenderer defaultRenderer = table.getDefaultRenderer(String.class);
		TableCellRenderer renderer = new TableCellRenderer() {
			final Color BG1 = UIUtil.TABLE_BACKGROUND_COLOR_1;
			final Color BG2 = UIUtil.TABLE_BACKGROUND_COLOR_2;

			@Override
			public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
				Component render = defaultRenderer.getTableCellRendererComponent(table, value, isSelected, false, row, column);
				if (!isSelected) {
					render.setBackground((row % 2 == 0) ? BG1 : BG2);
				}
				if (render instanceof JLabel) {
					String valueAsString = value == null? "" : String.valueOf(value);
					((JLabel) render).setToolTipText(valueAsString.length() > 0? valueAsString : null);
				}
				return render;
			}
		};
		for (int i = 0; i < tableModel.getColumnCount(); ++i) {
			table.getColumnModel().getColumn(i).setCellRenderer(renderer);
		}
		table.getColumnModel().getColumn(0).setPreferredWidth(50);
		table.getColumnModel().getColumn(1).setPreferredWidth(160);
		table.getColumnModel().getColumn(2).setPreferredWidth(200);
		table.getColumnModel().getColumn(3).setPreferredWidth(200);

		add(new JScrollPane(table), BorderLayout.CENTER);
		clear();
	}

	/**
	 * Clears the view.
	 */
	public void clear() {
		steps = new ArrayList<RowOriginStep>();
		tableModel.fireTableDataChanged();
		statusLabel.setText("Select a row to see how it has found its way into the subset.");
	}

	/**
	 * Analyzes and shows the origin of a row. Runs the queries off the event dispatch thread,
	 * showing a dialog that can be cancelled.
	 *
	 * @param rowTable the table of the row
	 * @param primaryKey the primary key values of the row
	 */
	public void showOrigin(final Table rowTable, final Object[] primaryKey) {
		Window window = SwingUtilities.getWindowAncestor(this);
		RowOrigin origin;
		try {
			origin = ConcurrentTaskControl.call(window, new Callable<RowOrigin>() {
				@Override
				public RowOrigin call() throws Exception {
					return context.createFinder().find(rowTable, primaryKey);
				}
			}, "Analyzing origin...", null);
		} catch (CancellationException e) {
			return;
		} catch (Throwable t) {
			UIUtil.showException(this, "Error", t);
			return;
		}
		if (origin == null) {
			return;
		}
		steps = origin.getSteps();
		tableModel.fireTableDataChanged();
		statusLabel.setText(statusText(origin, rowTable));
	}

	private String statusText(RowOrigin origin, Table rowTable) {
		int size = origin.getSteps().size();
		switch (origin.getStatus()) {
		case COMPLETE:
			return size <= 1?
					"This is a subject row: it is the starting point of the collection."
					: "Collected in " + (size - 1) + (size - 1 == 1? " step" : " steps") + ", starting from the subject.";
		case NOT_COLLECTED:
			return "This row is not part of the subset.";
		case BROKEN:
			return "The chain could not be followed up to the subject. "
					+ "Rows may have been removed from the working tables after the collection.";
		case TRUNCATED:
			return "The chain is longer than the limit and has been cut off.";
		default:
			return " ";
		}
	}

	/**
	 * Model of the chain table.
	 */
	private class ChainTableModel extends AbstractTableModel {

		private final String[] columnNames = new String[] { "Step", "Table", "Row", "Via Association" };

		@Override
		public int getRowCount() {
			return steps.size();
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
		public boolean isCellEditable(int row, int column) {
			return false;
		}

		@Override
		public Object getValueAt(int rowIndex, int columnIndex) {
			if (rowIndex < 0 || rowIndex >= steps.size()) {
				return null;
			}
			RowOriginStep step = steps.get(rowIndex);
			switch (columnIndex) {
			case 0: return String.valueOf(step.getBirthday());
			case 1: return context.getDataModel().getDisplayName(step.getTable());
			case 2: return step.getPrimaryKeyAsText();
			case 3: return step.getIncomingAssociation() == null?
					"(subject)"
					: step.getIncomingAssociation().getName() + (step.isAmbiguous()? "  (one of several matching rows)" : "");
			default: return null;
			}
		}

		private static final long serialVersionUID = 6884651914060915285L;
	}

	private static final long serialVersionUID = 5299341944654921962L;

}
