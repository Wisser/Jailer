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
import java.awt.GridLayout;
import java.awt.Window;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;
import java.util.function.Function;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

import net.sf.jailer.database.Session;
import net.sf.jailer.database.Session.AbstractResultSetReader;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.BrowserContentPane;
import net.sf.jailer.ui.databrowser.Row;
import net.sf.jailer.ui.databrowser.compare.CompareDialog.SyncHandler;
import net.sf.jailer.ui.syntaxtextarea.BasicFormatterImpl;
import net.sf.jailer.ui.databrowser.compare.RowComparison.RowPair;
import net.sf.jailer.ui.databrowser.compare.RowComparison.Side;
import net.sf.jailer.ui.util.ConcurrentTaskControl;
import net.sf.jailer.util.CancellationHandler;
import net.sf.jailer.util.CellContentConverter;

/**
 * Compares the rows of two SQL Console result tabs.
 *
 * @author Ralf Wisser
 */
public class CompareTabs {

	private static final String TITLE = "Compare Results";

	/**
	 * Title of the side holding the current rows.
	 */
	public static final String CURRENT_DATA = "Current Data";

	/**
	 * Lets the user choose the key columns and shows the comparison.
	 *
	 * @param owner the owner window
	 * @param leftTitle title of the first result
	 * @param left the first result
	 * @param selectedRows the rows of the first result to compare, or <code>null</code> for all
	 * @param rightTitle title of the second result
	 * @param right the second result
	 * @param leftSql the statement of the first result, or <code>null</code>
	 * @param rightSql the statement of the second result, or <code>null</code>
	 */
	public static void compare(Window owner, String leftTitle, BrowserContentPane left, List<Row> selectedRows, String rightTitle, BrowserContentPane right, String leftSql, String rightSql) {
		// both results are rows of the same table (with primary key), so the rows in the table can be made equal to either of them.
		// The results are not reloaded, one of them may be the state to go back to.
		// a column one result knows no name of may be known by the other one
		SyncHandler toRight = right.createSyncHandler(false, left.syncTargetColumns());
		SyncHandler toLeft = left.createSyncHandler(false, right.syncTargetColumns());
		boolean sameTable = toRight != null && toLeft != null
				&& CompareWithConnection.tableName(left.table).equalsIgnoreCase(CompareWithConnection.tableName(right.table));
		compare(owner, TITLE, new RowComparison(side(leftTitle, left, selectedRows).withToolTip(sqlToolTip(leftTitle, leftSql)), side(rightTitle, right, null).withToolTip(sqlToolTip(rightTitle, rightSql))),
				left.getPrimaryKeyColumnIndexes(), right.getPrimaryKeyColumnIndexes(), null,
				sameTable? toRight : null, sameTable? toLeft : null,
				sameTable? left.getIgnoredColumnsKey() : null);
	}

	/**
	 * Executes the statement of a result again, lets the user choose the key columns and
	 * compares the rows of the result with the current ones.
	 *
	 * @param owner the owner window
	 * @param title title of the result
	 * @param pane the result
	 * @param selectedRows the rows of the result to compare, or <code>null</code> for all
	 * @param sql the statement of the result
	 * @param limit row limit
	 * @param session the session of the result
	 * @param executor executes the reading (in the thread and transaction of the SQL Console)
	 */
	public static void compareWithCurrentData(Window owner, String title, BrowserContentPane pane, List<Row> selectedRows, String sql, int limit, Session session, Consumer<Runnable> executor) {
		// the snapshot, compared with the current rows again on each refresh
		Side left = side(title, pane, selectedRows).withToolTip(sqlToolTip(title, sql));
		Side right = readCurrentData(owner, left, sql, limit, session, executor);
		if (right == null) {
			return;
		}
		Set<Integer> pk = pane.getPrimaryKeyColumnIndexes();
		boolean sameShape = right.columns == left.columns;
		// restores the rows shown (the columns of the table are those of the result, so the current rows must have the same ones),
		// or creates a script that repeats the changes made since
		SyncHandler sync = sameShape? pane.createSyncHandler(false) : null;
		SyncHandler replay = sameShape? pane.createReplayHandler(null) : null;
		compare(owner, "Compare with Current Data", new RowComparison(left, right), pk, sameShape? pk : Collections.<Integer>emptySet(),
				o -> {
					Side r = readCurrentData(o, left, sql, limit, session, executor);
					return r == null? null : new RowComparison(left, r);
				}, sync, replay, pane.getIgnoredColumnsKey());
	}

	/**
	 * Executes the statement of a result again and reads the current rows.
	 *
	 * @param left the rows of the result
	 * @return the current rows, or <code>null</code> if cancelled or failed (the error is shown)
	 */
	private static Side readCurrentData(Window owner, Side left, String sql, int limit, Session session, Consumer<Runnable> executor) {
		Object context = new Object();
		AtomicBoolean cancelled = new AtomicBoolean(false);
		try {
			List<String> labels = new ArrayList<String>();
			Set<Integer> charColumns = new HashSet<Integer>();
			boolean[] truncated = new boolean[1];
			List<Object[]> rows = ConcurrentTaskControl.call(owner, () -> {
				CompletableFuture<List<Object[]>> future = new CompletableFuture<List<Object[]>>();
				executor.accept(() -> {
					if (cancelled.get()) {
						future.completeExceptionally(new CancellationException());
						return;
					}
					try {
						future.complete(readRows(session, sql, limit, labels, charColumns, truncated, context));
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
			}, "Executing statement...", UIUtil.blinkingInfoLabel(null), false);

			// same shape: the columns are aligned by position
			boolean sameShape = labels.size() == left.columns.size();
			return new Side(CURRENT_DATA, sameShape? left.columns : labels, rows, truncated[0], null)
					.withToolTip(sqlToolTip(CURRENT_DATA + " (the statement executed again)", sql))
					.withCharColumns(charColumns);
		} catch (CancellationException e) {
			cancelled.set(true);
			CancellationHandler.cancel(context);
		} catch (Throwable t) {
			UIUtil.showException(owner, "Error", t);
		} finally {
			CancellationHandler.reset(context);
		}
		return null;
	}

	/**
	 * Executes a statement and reads the rows.
	 *
	 * @param charColumns receives the indexes of the columns of type CHAR (or NCHAR)
	 */
	private static List<Object[]> readRows(Session session, String sql, int limit, List<String> labels, Set<Integer> charColumns, boolean[] truncated, Object context) throws SQLException {
		List<Object[]> result = new ArrayList<Object[]>();
		session.executeQuery(sql, new AbstractResultSetReader() {
			@Override
			public void readCurrentRow(ResultSet resultSet) throws SQLException {
				ResultSetMetaData metaData = getMetaData(resultSet);
				int count = metaData.getColumnCount();
				if (labels.isEmpty()) {
					for (int i = 1; i <= count; ++i) {
						labels.add(metaData.getColumnLabel(i));
						if (metaData.getColumnType(i) == Types.CHAR || metaData.getColumnType(i) == Types.NCHAR) {
							charColumns.add(i - 1);
						}
					}
				}
				if (result.size() >= limit) {
					truncated[0] = true;
					return;
				}
				CellContentConverter cellContentConverter = getCellContentConverter(resultSet, session, session.dbms);
				Object[] values = new Object[count];
				for (int i = 1; i <= count; ++i) {
					Object value = cellContentConverter.getObject(resultSet, i);
					if (resultSet.wasNull()) {
						value = null;
					} else {
						Object lobValue = BrowserContentPane.toLobRender(value);
						if (lobValue != null) {
							value = lobValue;
						}
					}
					values[i - 1] = value;
				}
				result.add(values);
			}
		}, null, context, limit + 1);
		return result;
	}

	/**
	 * Lets the user choose the key columns and shows the comparison.
	 *
	 * @param recompare compares again for a refresh of the dialog, or <code>null</code> if it can't be refreshed
	 * @param sync makes the right side equal to the left side, or <code>null</code>
	 * @param reverseSync makes the left side equal to the right side, or <code>null</code>
	 * @param ignoredColumnsKey identifies the table whose columns excluded from the comparison are remembered, or <code>null</code>
	 */
	private static void compare(Window owner, String title, RowComparison comparison, Set<Integer> leftPK, Set<Integer> rightPK,
			Function<Window, RowComparison> recompare, SyncHandler sync, SyncHandler reverseSync, String ignoredColumnsKey) {
		List<Integer> common = comparison.commonColumns();
		if (common.isEmpty()) {
			JOptionPane.showMessageDialog(owner, "The two results have no column in common.", title, JOptionPane.INFORMATION_MESSAGE);
			return;
		}

		// preselect the primary key columns known on both sides, otherwise the first common column
		Map<Integer, JCheckBox> checkBoxes = new LinkedHashMap<Integer, JCheckBox>();
		boolean anySelected = false;
		for (int c: common) {
			boolean pk = leftPK.contains(comparison.leftIndex(c)) && rightPK.contains(comparison.rightIndex(c));
			JCheckBox checkBox = new JCheckBox(comparison.columns.get(c), pk);
			anySelected |= pk;
			checkBoxes.put(c, checkBox);
		}
		if (!anySelected) {
			checkBoxes.values().iterator().next().setSelected(true);
		}
		JPanel list = new JPanel(new GridLayout(0, 1));
		for (JCheckBox checkBox: checkBoxes.values()) {
			list.add(checkBox);
		}
		JPanel panel = new JPanel(new BorderLayout(0, 6));
		panel.add(new JLabel("<html>Rows with equal values in these columns are compared with each other:</html>"), BorderLayout.NORTH);
		JScrollPane scrollPane = new JScrollPane(list);
		scrollPane.getVerticalScrollBar().setUnitIncrement(Math.max(16, checkBoxes.values().iterator().next().getPreferredSize().height));
		scrollPane.setPreferredSize(new java.awt.Dimension(360, Math.min(300, 28 * checkBoxes.size() + 8)));
		panel.add(scrollPane, BorderLayout.CENTER);

		JButton okButton = new JButton("OK");
		okButton.setIcon(UIUtil.scaleIcon(okButton, UIUtil.readImage("/buttonok.png")));
		JButton cancelButton = new JButton("Cancel");
		cancelButton.setIcon(UIUtil.scaleIcon(cancelButton, UIUtil.readImage("/buttoncancel.png")));
		for (JButton button: new JButton[] { okButton, cancelButton }) {
			button.addActionListener(e -> {
				JOptionPane pane = (JOptionPane) SwingUtilities.getAncestorOfClass(JOptionPane.class, button);
				if (pane != null) {
					pane.setValue(button);
				}
			});
		}

		List<Integer> keyColumns = new ArrayList<Integer>();
		while (keyColumns.isEmpty()) {
			int choice = JOptionPane.showOptionDialog(owner, panel, title + " - Key Columns", JOptionPane.OK_CANCEL_OPTION, JOptionPane.PLAIN_MESSAGE,
					null, new Object[] { okButton, cancelButton }, okButton);
			if (choice != 0) {
				return;
			}
			for (Map.Entry<Integer, JCheckBox> e: checkBoxes.entrySet()) {
				if (e.getValue().isSelected()) {
					keyColumns.add(e.getKey());
				}
			}
		}
		List<RowPair> pairs = comparison.matchByKey(keyColumns);
		new CompareDialog(owner, title, comparison, pairs, keyColumns, recompare, sync, reverseSync, ignoredColumnsKey);
	}

	/**
	 * Gets the tool tip of the side of a result: its title and its statement, like the menu "Compare with..." shows it.
	 *
	 * @return the tool tip, or <code>null</code> if the statement is unknown
	 */
	public static String sqlToolTip(String title, String sql) {
		if (sql == null || sql.trim().isEmpty()) {
			return null;
		}
		return "<html><b>" + UIUtil.toHTMLFragment(title, 0) + "</b><hr>" + UIUtil.toHTMLFragment(new BasicFormatterImpl().format(sql), 200) + "</html>";
	}

	/**
	 * Gets the side of a result.
	 *
	 * @param selectedRows the rows of the result, or <code>null</code> for all
	 */
	private static Side side(String title, BrowserContentPane pane, List<Row> selectedRows) {
		List<String> columns = new ArrayList<String>();
		for (int i = 0; i < pane.rowsTable.getModel().getColumnCount(); ++i) {
			columns.add(pane.rowsTable.getModel().getColumnName(i));
		}
		List<Object[]> rows = new ArrayList<Object[]>();
		for (Row row: selectedRows != null? selectedRows : pane.rows) {
			rows.add(row.values);
		}
		Side side = new Side(title, columns, rows, selectedRows == null && pane.isRowLimitExceeded(),
				(column, value) -> pane.browserContentCellEditor.cellContentToText(column, value))
				.withKeyColumns(pane.getPrimaryKeyColumnIndexes(), pane.getForeignKeyColumnIndexes())
				.withCharColumns(pane.getCharColumnIndexes());
		return selectedRows != null? side.withSelection() : side;
	}

}
