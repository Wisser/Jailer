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
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;

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
	 * @param rightTitle title of the second result
	 * @param right the second result
	 */
	public static void compare(Window owner, String leftTitle, BrowserContentPane left, String rightTitle, BrowserContentPane right) {
		compare(owner, TITLE, new RowComparison(side(leftTitle, left), side(rightTitle, right)),
				left.getPrimaryKeyColumnIndexes(), right.getPrimaryKeyColumnIndexes());
	}

	/**
	 * Executes the statement of a result again, lets the user choose the key columns and
	 * compares the rows of the result with the current ones.
	 *
	 * @param owner the owner window
	 * @param title title of the result
	 * @param pane the result
	 * @param sql the statement of the result
	 * @param limit row limit
	 * @param session the session of the result
	 * @param executor executes the reading (in the thread and transaction of the SQL Console)
	 */
	public static void compareWithCurrentData(Window owner, String title, BrowserContentPane pane, String sql, int limit, Session session, Consumer<Runnable> executor) {
		Side left = side(title, pane);
		Object context = new Object();
		AtomicBoolean cancelled = new AtomicBoolean(false);
		try {
			List<String> labels = new ArrayList<String>();
			boolean[] truncated = new boolean[1];
			List<Object[]> rows = ConcurrentTaskControl.call(owner, () -> {
				CompletableFuture<List<Object[]>> future = new CompletableFuture<List<Object[]>>();
				executor.accept(() -> {
					if (cancelled.get()) {
						future.completeExceptionally(new CancellationException());
						return;
					}
					try {
						future.complete(readRows(session, sql, limit, labels, truncated, context));
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
			}, "Executing statement...", null);

			// same shape: the columns are aligned by position
			boolean sameShape = labels.size() == left.columns.size();
			Side right = new Side(CURRENT_DATA, sameShape? left.columns : labels, rows, truncated[0], null);
			Set<Integer> pk = pane.getPrimaryKeyColumnIndexes();
			compare(owner, "Compare with Current Data", new RowComparison(left, right), pk, sameShape? pk : Collections.<Integer>emptySet());
		} catch (CancellationException e) {
			cancelled.set(true);
			CancellationHandler.cancel(context);
		} catch (Throwable t) {
			UIUtil.showException(owner, "Error", t);
		} finally {
			CancellationHandler.reset(context);
		}
	}

	/**
	 * Executes a statement and reads the rows.
	 */
	private static List<Object[]> readRows(Session session, String sql, int limit, List<String> labels, boolean[] truncated, Object context) throws SQLException {
		List<Object[]> result = new ArrayList<Object[]>();
		session.executeQuery(sql, new AbstractResultSetReader() {
			@Override
			public void readCurrentRow(ResultSet resultSet) throws SQLException {
				ResultSetMetaData metaData = getMetaData(resultSet);
				int count = metaData.getColumnCount();
				if (labels.isEmpty()) {
					for (int i = 1; i <= count; ++i) {
						labels.add(metaData.getColumnLabel(i));
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

	private static void compare(Window owner, String title, RowComparison comparison, Set<Integer> leftPK, Set<Integer> rightPK) {
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
		new CompareDialog(owner, title, comparison, pairs);
	}

	private static Side side(String title, BrowserContentPane pane) {
		List<String> columns = new ArrayList<String>();
		for (int i = 0; i < pane.rowsTable.getModel().getColumnCount(); ++i) {
			columns.add(pane.rowsTable.getModel().getColumnName(i));
		}
		List<Object[]> rows = new ArrayList<Object[]>();
		for (Row row: pane.rows) {
			rows.add(row.values);
		}
		return new Side(title, columns, rows, pane.isRowLimitExceeded(),
				(column, value) -> pane.browserContentCellEditor.cellContentToText(column, value))
				.withKeyColumns(pane.getPrimaryKeyColumnIndexes(), pane.getForeignKeyColumnIndexes());
	}

}
