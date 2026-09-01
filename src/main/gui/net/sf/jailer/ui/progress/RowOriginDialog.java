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
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.function.Consumer;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.WindowConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellRenderer;

import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.entitygraph.RowOriginStep;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.util.ConcurrentTaskControl;
import net.sf.jailer.util.CancellationException;
import net.sf.jailer.util.CellContentConverter;

/**
 * Lists the rows which have been collected through a single association and shows, for the
 * selected one, the way it has taken into the subset.
 *
 * @author Ralf Wisser
 */
public class RowOriginDialog extends JDialog {

	/**
	 * Maximum number of rows to list.
	 */
	private static final int MAX_ROWS = 1000;

	private final RowOriginContext context;
	private final Table table;
	private final int numberOfPrimaryKeyColumns;

	private final JLabel headerLabel;
	private final JTable rowsTable;
	private final RowsTableModel rowsTableModel;
	private final RowOriginPanel originPanel;

	private List<String> columnNames = new ArrayList<String>();
	private List<Object[]> rows = new ArrayList<Object[]>();
	private List<Object[]> primaryKeys = new ArrayList<Object[]>();

	/**
	 * Constructor.
	 *
	 * @param owner the owner window
	 * @param context the context to ask
	 * @param association the association whose collected rows are to be listed
	 * @param pathOpener lays the chain of the selected row out in a Data Browser, or <code>null</code>
	 */
	public RowOriginDialog(final Window owner, RowOriginContext context, Association association,
			Consumer<List<RowOriginStep>> pathOpener) {
		// not modal: a Data Browser showing the path of a row has to be usable next to it, and
		// one row after the other can be looked at without closing the list in between
		super(owner, "Rows collected through \"" + association.getName() + "\"", ModalityType.MODELESS);
		this.context = context;
		this.table = association.destination;
		this.numberOfPrimaryKeyColumns = context.getRowIdSupport().getPrimaryKey(table).getColumns().size();

		headerLabel = new JLabel(" ");
		headerLabel.setBorder(BorderFactory.createEmptyBorder(4, 6, 4, 4));

		rowsTableModel = new RowsTableModel();
		rowsTable = new JTable(rowsTableModel);
		rowsTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
		rowsTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		rowsTable.setShowGrid(false);
		rowsTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			@Override
			public void valueChanged(ListSelectionEvent e) {
				if (!e.getValueIsAdjusting()) {
					onRowSelected();
				}
			}
		});

		originPanel = new RowOriginPanel(context);
		originPanel.setPathOpener(pathOpener);

		JPanel rowsPanel = new JPanel(new BorderLayout());
		rowsPanel.add(headerLabel, BorderLayout.NORTH);
		rowsPanel.add(new JScrollPane(rowsTable), BorderLayout.CENTER);

		JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, rowsPanel, originPanel);
		splitPane.setOneTouchExpandable(true);
		splitPane.setResizeWeight(0.6);

		JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		JButton closeButton = new JButton(" Close ");
		closeButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				setVisible(false);
				dispose();
			}
		});
		buttonPanel.add(closeButton);

		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(splitPane, BorderLayout.CENTER);
		getContentPane().add(buttonPanel, BorderLayout.SOUTH);

		setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		UIUtil.setDialogSize(this, 900, 700);
		setLocationRelativeTo(owner);

		// as a modeless dialog it outlives the progress window, so it has to hold the retained
		// rows itself: without this, closing the progress window would discard them right under
		// the open list, see RetainedEntityGraphs.discardWhenUnused
		RetainedEntityGraphs.addUser();
		addWindowListener(new WindowAdapter() {
			private boolean released = false;
			@Override
			public void windowClosed(WindowEvent e) {
				if (!released) {
					released = true;
					RetainedEntityGraphs.removeUser(owner);
				}
			}
		});

		loadRows(association);
	}

	/**
	 * Reads the collected rows, off the event dispatch thread.
	 */
	private void loadRows(final Association association) {
		final List<String> newColumnNames = new ArrayList<String>();
		final List<Object[]> newRows = new ArrayList<Object[]>();
		final List<Object[]> newPrimaryKeys = new ArrayList<Object[]>();
		try {
			ConcurrentTaskControl.call(this, new Callable<Object>() {
				@Override
				public Object call() throws Exception {
					final Session session = context.getEntityGraph().getSession();
					context.getEntityGraph().readCollectedRows(table, association.getId(), MAX_ROWS,
							new Session.AbstractResultSetReader() {
						@Override
						public void readCurrentRow(ResultSet resultSet) throws SQLException {
							ResultSetMetaData metaData = getMetaData(resultSet);
							CellContentConverter cellContentConverter = new CellContentConverter(metaData, session, session.dbms);
							int numberOfColumns = metaData.getColumnCount() - numberOfPrimaryKeyColumns;
							if (newColumnNames.isEmpty()) {
								for (int i = 1; i <= numberOfColumns; ++i) {
									newColumnNames.add(metaData.getColumnLabel(i));
								}
							}
							Object[] row = new Object[numberOfColumns];
							for (int i = 0; i < numberOfColumns; ++i) {
								row[i] = cellContentConverter.getObject(resultSet, i + 1);
							}
							newRows.add(row);
							Object[] primaryKey = new Object[numberOfPrimaryKeyColumns];
							for (int i = 0; i < numberOfPrimaryKeyColumns; ++i) {
								primaryKey[i] = cellContentConverter.getObject(resultSet, "PK" + i);
							}
							newPrimaryKeys.add(primaryKey);
						}
					});
					return null;
				}
			}, "Reading rows...", null);
		} catch (CancellationException e) {
			// nothing to show
		} catch (Throwable t) {
			UIUtil.showException(this, "Error", t);
		}

		columnNames = newColumnNames;
		rows = newRows;
		primaryKeys = newPrimaryKeys;
		rowsTableModel.fireTableStructureChanged();
		installRenderer();
		headerLabel.setText(rows.isEmpty()?
				"No rows."
				: UIUtil.format(rows.size()) + " rows"
					+ (rows.size() >= MAX_ROWS? " (limited to the first " + UIUtil.format(MAX_ROWS) + ")" : "")
					+ " of " + context.getDataModel().getDisplayName(table) + ".");
		if (!rows.isEmpty()) {
			rowsTable.getSelectionModel().setSelectionInterval(0, 0);
		} else {
			originPanel.clear();
		}
	}

	private void installRenderer() {
		final TableCellRenderer defaultRenderer = rowsTable.getDefaultRenderer(String.class);
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
		for (int i = 0; i < rowsTable.getColumnModel().getColumnCount(); ++i) {
			rowsTable.getColumnModel().getColumn(i).setCellRenderer(renderer);
			rowsTable.getColumnModel().getColumn(i).setPreferredWidth(120);
		}
	}

	private void onRowSelected() {
		int viewRow = rowsTable.getSelectedRow();
		if (viewRow < 0) {
			originPanel.clear();
			return;
		}
		int modelRow = rowsTable.convertRowIndexToModel(viewRow);
		if (modelRow < 0 || modelRow >= primaryKeys.size()) {
			originPanel.clear();
			return;
		}
		originPanel.showOrigin(table, primaryKeys.get(modelRow));
	}

	/**
	 * Model of the row list.
	 */
	private class RowsTableModel extends AbstractTableModel {

		@Override
		public int getRowCount() {
			return rows.size();
		}

		@Override
		public int getColumnCount() {
			return columnNames.size();
		}

		@Override
		public String getColumnName(int column) {
			return column < columnNames.size()? columnNames.get(column) : "";
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
			Object[] row = rows.get(rowIndex);
			Object value = columnIndex < row.length? row[columnIndex] : null;
			return value == null? null : String.valueOf(value);
		}

		private static final long serialVersionUID = -1948113073669515482L;
	}

	private static final long serialVersionUID = 7723845513349968128L;

}
