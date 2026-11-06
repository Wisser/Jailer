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
package net.sf.jailer.ui.databrowser;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Frame;
import java.awt.Window;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.KeyStroke;
import javax.swing.ListSelectionModel;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableRowSorter;

import net.sf.jailer.ui.Colors;
import net.sf.jailer.ui.UIUtil;

/**
 * Lets the user choose which of many related tables to open ("All non-empty" in the
 * navigation menu of a table browser), instead of opening all of them or none.
 *
 * @author Ralf Wisser
 */
public class OpenRelatedTablesDialog extends JDialog {

	private static final long serialVersionUID = 1L;

	private static ImageIcon selectIcon;
	private static ImageIcon clearIcon;
	private static ImageIcon okIcon;
	private static ImageIcon cancelIcon;

	static {
		// the same icons as the "Select all" / "Deselect all" / accept / close buttons elsewhere
		selectIcon = UIUtil.readImage("/select.png");
		clearIcon = UIUtil.readImage("/clear.png");
		okIcon = UIUtil.readImage("/buttonok.png");
		cancelIcon = UIUtil.readImage("/buttoncancel.png");
	}

	/** Number of rows the table shows without scrolling. */
	private static final int VISIBLE_ROWS = 15;

	private final List<String> names;
	private final List<String> counts;
	/** Indices into {@link #names}, sorted by name. */
	private final List<Integer> order;
	private final boolean[] selected;
	private final int manyLimit;
	private final JButton openButton = new JButton();
	private final AbstractTableModel model;
	private List<Integer> result;

	private OpenRelatedTablesDialog(Window owner, List<String> names, List<String> counts, int manyLimit) {
		super(owner, "Open related tables", Dialog.ModalityType.APPLICATION_MODAL);
		this.names = names;
		this.counts = counts;
		this.manyLimit = manyLimit;
		this.order = new ArrayList<Integer>();
		for (int i = 0; i < names.size(); ++i) {
			order.add(i);
		}
		Collections.sort(order, (a, b) -> names.get(a).compareToIgnoreCase(names.get(b)));
		// all selected at the start, as the plain "Yes" used to open all of them
		this.selected = new boolean[names.size()];
		java.util.Arrays.fill(selected, true);

		model = new AbstractTableModel() {
			private static final long serialVersionUID = 1L;

			@Override
			public int getRowCount() {
				return order.size();
			}

			@Override
			public int getColumnCount() {
				return 3;
			}

			@Override
			public String getColumnName(int column) {
				return column == 0 ? "" : column == 1 ? "Table" : "Rows";
			}

			@Override
			public Class<?> getColumnClass(int column) {
				return column == 0 ? Boolean.class : String.class;
			}

			@Override
			public boolean isCellEditable(int row, int column) {
				return column == 0;
			}

			@Override
			public Object getValueAt(int row, int column) {
				int i = order.get(row);
				switch (column) {
				case 0:
					return selected[i];
				case 1:
					return names.get(i);
				default:
					return counts.get(i);
				}
			}

			@Override
			public void setValueAt(Object value, int row, int column) {
				if (column == 0) {
					selected[order.get(row)] = Boolean.TRUE.equals(value);
					fireTableCellUpdated(row, column);
					updateOpenButton();
				}
			}
		};

		JPanel content = new JPanel(new BorderLayout(0, 8));
		content.setBorder(BorderFactory.createEmptyBorder(12, 12, 8, 12));
		content.add(createWarning(), BorderLayout.NORTH);
		content.add(createTable(), BorderLayout.CENTER);
		content.add(createButtons(), BorderLayout.SOUTH);
		setContentPane(content);

		getRootPane().registerKeyboardAction(e -> dispose(),
				KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), JComponent.WHEN_IN_FOCUSED_WINDOW);
		getRootPane().setDefaultButton(openButton);
		updateOpenButton();
		pack();
		setLocationRelativeTo(owner);
	}

	private JComponent createWarning() {
		JLabel warning = new JLabel("<html>" + names.size() + " non-empty associations. Opening a table browser for each "
				+ "of them can make the desktop hard to read and takes time.<br>Choose the tables to open.</html>");
		warning.setIcon(UIManager.getIcon("OptionPane.warningIcon"));
		warning.setIconTextGap(12);
		return warning;
	}

	private JComponent createTable() {
		JTable table = new JTable(model);
		table.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		table.setFillsViewportHeight(true);
		table.getTableHeader().setReorderingAllowed(false);
		table.getColumnModel().getColumn(0).setMaxWidth(32);
		table.getColumnModel().getColumn(0).setMinWidth(32);
		DefaultTableCellRenderer right = new DefaultTableCellRenderer();
		right.setHorizontalAlignment(SwingConstants.RIGHT);
		table.getColumnModel().getColumn(2).setCellRenderer(right);
		int countWidth = table.getFontMetrics(table.getFont()).stringWidth(" >99999 ") + 16;
		table.getColumnModel().getColumn(2).setMaxWidth(countWidth * 2);
		table.getColumnModel().getColumn(2).setPreferredWidth(countWidth);

		// sortable by clicking the column headers; initially sorted by name (the model's own order)
		TableRowSorter<AbstractTableModel> sorter = new TableRowSorter<AbstractTableModel>(model);
		sorter.setComparator(1, String.CASE_INSENSITIVE_ORDER);
		sorter.setComparator(2, Comparator.comparingLong(OpenRelatedTablesDialog::countSortKey));
		table.setRowSorter(sorter);

		// space: toggles all selected rows, so a block can be ticked or unticked at once
		table.getInputMap(JComponent.WHEN_FOCUSED).put(KeyStroke.getKeyStroke(KeyEvent.VK_SPACE, 0), "toggleChecked");
		table.getActionMap().put("toggleChecked", new javax.swing.AbstractAction() {
			private static final long serialVersionUID = 1L;

			@Override
			public void actionPerformed(java.awt.event.ActionEvent e) {
				int[] rows = table.getSelectedRows();
				if (rows.length == 0) {
					return;
				}
				// view rows: the table may be sorted
				boolean value = !selected[order.get(table.convertRowIndexToModel(rows[0]))];
				for (int row: rows) {
					selected[order.get(table.convertRowIndexToModel(row))] = value;
				}
				// "updated", not "changed": keeps the selection and does not re-sort under the user's hands
				model.fireTableRowsUpdated(0, model.getRowCount() - 1);
				updateOpenButton();
			}
		});

		JScrollPane scrollPane = new JScrollPane(table);
		int rowsShown = Math.min(VISIBLE_ROWS, Math.max(order.size(), 3));
		int width = 0;
		for (String name: names) {
			width = Math.max(width, table.getFontMetrics(table.getFont()).stringWidth(name));
		}
		width = Math.min(Math.max(width + 32 + countWidth + 40, 360), 700);
		scrollPane.setPreferredSize(new Dimension(width,
				rowsShown * table.getRowHeight() + table.getTableHeader().getPreferredSize().height + 4));
		return scrollPane;
	}

	private JComponent createButtons() {
		JPanel buttons = new JPanel(new BorderLayout());

		JPanel left = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
		JButton all = new JButton("Select all");
		all.setIcon(UIUtil.scaleIcon(all, selectIcon));
		all.addActionListener(e -> setAll(true));
		JButton none = new JButton("Select none");
		none.setIcon(UIUtil.scaleIcon(none, clearIcon));
		none.addActionListener(e -> setAll(false));
		left.add(all);
		left.add(javax.swing.Box.createHorizontalStrut(6));
		left.add(none);
		buttons.add(left, BorderLayout.WEST);

		JPanel right = new JPanel(new FlowLayout(FlowLayout.RIGHT, 0, 0));
		openButton.setIcon(UIUtil.scaleIcon(openButton, okIcon));
		openButton.addActionListener(e -> {
			List<Integer> chosen = new ArrayList<Integer>();
			for (int i: order) {
				if (selected[i]) {
					chosen.add(i);
				}
			}
			result = chosen;
			dispose();
		});
		JButton cancel = new JButton("Cancel");
		cancel.setIcon(UIUtil.scaleIcon(cancel, cancelIcon));
		cancel.addActionListener(e -> dispose());
		right.add(openButton);
		right.add(javax.swing.Box.createHorizontalStrut(6));
		right.add(cancel);
		buttons.add(right, BorderLayout.EAST);
		return buttons;
	}

	/**
	 * Sort key of a row count as shown in the menu: "123" counts as 123, ">1000" just above 1000,
	 * "?" (unknown) below everything.
	 */
	private static long countSortKey(Object count) {
		String s = count == null ? "" : count.toString().trim();
		try {
			if (s.startsWith(">")) {
				return Long.parseLong(s.substring(1).trim()) * 2 + 1;
			}
			return Long.parseLong(s) * 2;
		} catch (NumberFormatException e) {
			return -1;
		}
	}

	private void setAll(boolean value) {
		java.util.Arrays.fill(selected, value);
		model.fireTableRowsUpdated(0, model.getRowCount() - 1);
		updateOpenButton();
	}

	private void updateOpenButton() {
		int k = 0;
		for (boolean s: selected) {
			if (s) {
				++k;
			}
		}
		openButton.setText("Open (" + k + ")");
		openButton.setEnabled(k > 0);
		boolean many = k > manyLimit;
		Font font = openButton.getFont();
		openButton.setFont(font.deriveFont(many ? font.getStyle() | Font.BOLD : font.getStyle() & ~Font.BOLD));
		openButton.setForeground(many ? Colors.Color_255_0_0 : UIManager.getColor("Button.foreground"));
		openButton.setToolTipText(many ? "many table browsers" : null);
	}

	/**
	 * Lets the user choose the tables to open.
	 *
	 * @param parent component of the window the dialog belongs to
	 * @param names the names of the tables (associations)
	 * @param counts the row counts as shown in the navigation menu, parallel to <code>names</code>
	 * @param manyLimit number of tables above which the choice is marked as "many"
	 * @return the indices (into <code>names</code>) of the chosen tables, in the order of their names,
	 *         or <code>null</code> if cancelled
	 */
	public static List<Integer> choose(Component parent, List<String> names, List<String> counts, int manyLimit) {
		Window owner = parent == null ? null
				: parent instanceof Window ? (Window) parent : SwingUtilities.getWindowAncestor(parent);
		if (owner != null && !(owner instanceof Frame) && !(owner instanceof Dialog)) {
			owner = null;
		}
		OpenRelatedTablesDialog dialog = new OpenRelatedTablesDialog(owner, names, counts, manyLimit);
		dialog.setVisible(true);
		return dialog.result;
	}
}
