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
import java.awt.FlowLayout;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.function.Consumer;

import javax.swing.BorderFactory;
import javax.swing.JButton;
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
	private final JPanel buttonPanel;
	private final JButton openPathButton;

	private List<RowOriginStep> steps = new ArrayList<RowOriginStep>();
	private Consumer<List<RowOriginStep>> pathOpener;

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

		openPathButton = new JButton(" Open Path in Data Browser ");
		openPathButton.setIcon(UIUtil.scaleIcon(openPathButton, UIUtil.readImage("/subject.png")));
		openPathButton.setToolTipText("Opens the chain as table browsers of the Data Browser: one per step, each showing the single row of that step.");
		openPathButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (pathOpener != null && !steps.isEmpty()) {
					pathOpener.accept(steps);
				}
			}
		});
		buttonPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		buttonPanel.add(openPathButton);
		// no path opener, no button: the containers which cannot reach a Data Browser show none
		buttonPanel.setVisible(false);
		add(buttonPanel, BorderLayout.SOUTH);

		clear();
	}

	/**
	 * Sets the action which lays out the chain in a Data Browser. Only if one is set, the panel
	 * offers to open the path.
	 *
	 * @param pathOpener the action, or <code>null</code>
	 */
	public void setPathOpener(Consumer<List<RowOriginStep>> pathOpener) {
		this.pathOpener = pathOpener;
		buttonPanel.setVisible(pathOpener != null);
		updateOpenPathButton();
	}

	private void updateOpenPathButton() {
		// a chain of a single step has no predecessor to lay out - the row is the subject itself,
		// or the chain is broken right at the start. Either way there is nothing to open.
		openPathButton.setEnabled(pathOpener != null && steps.size() > 1);
	}

	/**
	 * Clears the view.
	 */
	public void clear() {
		steps = new ArrayList<RowOriginStep>();
		tableModel.fireTableDataChanged();
		setStatus("Select a row to see how it has found its way into the subset.");
		updateOpenPathButton();
	}

	/**
	 * Analyzes and shows the origin of a row. Runs the queries off the event dispatch thread,
	 * showing a dialog that can be cancelled.
	 *
	 * @param rowTable the table of the row
	 * @param primaryKey the primary key values of the row
	 */
	public void showOrigin(final Table rowTable, final Object[] primaryKey) {
		showOrigin(rowTable, new Callable<Object[]>() {
			@Override
			public Object[] call() {
				return primaryKey;
			}
		});
	}

	/**
	 * Analyzes and shows the origin of a row whose primary key values still have to be read.
	 * Reading them and following the chain happen in one background run, so the caller does not
	 * have to leave the event dispatch thread itself.
	 *
	 * @param rowTable the table of the row
	 * @param primaryKeySupplier delivers the primary key values of the row, <code>null</code> if
	 *        the row cannot be found any more
	 */
	public void showOrigin(final Table rowTable, final Callable<Object[]> primaryKeySupplier) {
		Window window = SwingUtilities.getWindowAncestor(this);
		RowOrigin origin;
		try {
			origin = ConcurrentTaskControl.call(window, new Callable<RowOrigin>() {
				@Override
				public RowOrigin call() throws Exception {
					Object[] primaryKey = primaryKeySupplier.call();
					if (primaryKey == null) {
						return null;
					}
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
			steps = new ArrayList<RowOriginStep>();
			tableModel.fireTableDataChanged();
			setStatus("The row could not be found.");
			updateOpenPathButton();
			return;
		}
		steps = origin.getSteps();
		tableModel.fireTableDataChanged();
		setStatus(statusText(origin, rowTable));
		updateOpenPathButton();
	}

	/**
	 * Sets the status line, and the same text as its tooltip: the label is a single line and cuts
	 * off rather than wrapping, so a narrow window would otherwise swallow the end of it.
	 *
	 * @param text the text
	 */
	private void setStatus(String text) {
		statusLabel.setText(text);
		statusLabel.setToolTipText(text.trim().length() > 0? text : null);
	}

	private String statusText(RowOrigin origin, Table rowTable) {
		int size = origin.getSteps().size();
		String text;
		switch (origin.getStatus()) {
		case COMPLETE:
			text = size <= 1?
					"This is a subject row: it is the starting point of the collection."
					: "Collected in " + (size - 1) + (size - 1 == 1? " step" : " steps") + ", starting from the subject.";
			break;
		case NOT_COLLECTED:
			text = "This row is not part of the subset.";
			break;
		case BROKEN:
			text = "The chain could not be followed up to the subject. "
					+ "Rows may have been removed from the working tables after the collection.";
			break;
		case TRUNCATED:
			text = "The chain is longer than the limit and has been cut off.";
			break;
		default:
			text = " ";
			break;
		}
		// said here rather than in a window of its own: which step it is stands in the column
		// "Via Association". Holds for a broken chain too, hence not tied to the status above.
		if (isAmbiguous(origin)) {
			text += "  The path is not unique: at some step more than one row matches.";
		}
		return text;
	}

	/**
	 * Returns whether any step of the chain has more than one matching row, so that the way shown
	 * is one of several.
	 *
	 * @param origin the chain
	 * @return <code>true</code> if the way is not unique
	 */
	private boolean isAmbiguous(RowOrigin origin) {
		for (RowOriginStep step: origin.getSteps()) {
			if (step.isAmbiguous()) {
				return true;
			}
		}
		return false;
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
