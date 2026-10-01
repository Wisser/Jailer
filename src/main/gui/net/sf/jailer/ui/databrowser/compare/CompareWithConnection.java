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
import java.awt.Dialog.ModalityType;
import java.awt.Window;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.SQLXML;
import java.sql.Statement;
import java.sql.Types;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.WeakHashMap;
import java.util.concurrent.CancellationException;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BiFunction;
import java.util.function.Function;

import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.database.BasicDataSource;
import net.sf.jailer.database.Session;
import net.sf.jailer.database.Session.AbstractResultSetReader;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;
import net.sf.jailer.ui.DbConnectionDialog;
import net.sf.jailer.ui.DbConnectionDialog.ConnectionInfo;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.BrowserContentPane;
import net.sf.jailer.ui.databrowser.DataBrowserContext;
import net.sf.jailer.ui.databrowser.LobValue;
import net.sf.jailer.ui.databrowser.SQLDMLBuilder;
import net.sf.jailer.ui.databrowser.SQLDMLPanel;
import net.sf.jailer.ui.databrowser.SchemaMappingDialog;
import net.sf.jailer.ui.databrowser.compare.CompareDialog.SyncHandler;
import net.sf.jailer.ui.databrowser.compare.RowComparison.CellStatus;
import net.sf.jailer.ui.databrowser.compare.RowComparison.RowPair;
import net.sf.jailer.ui.databrowser.compare.RowComparison.Side;
import net.sf.jailer.ui.databrowser.metadata.MetaDataSource;
import net.sf.jailer.ui.databrowser.sqlconsole.SQLConsole;
import net.sf.jailer.ui.databrowser.compare.RowComparison.Status;
import net.sf.jailer.ui.util.ConcurrentTaskControl;
import net.sf.jailer.ui.util.UISettings;
import net.sf.jailer.util.CancellationHandler;
import net.sf.jailer.util.CellContentConverter;
import net.sf.jailer.util.ClasspathUtil;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlUtil;

/**
 * Compares rows of a table with the rows having the same primary key
 * in the database of another connection.
 *
 * @author Ralf Wisser
 */
public class CompareWithConnection {

	private static final String TITLE = "Compare with other Database";
	private static final int BATCH_SIZE = 100;

	/**
	 * Sessions to other databases, per owner window and connection. Closed with the owner window.
	 */
	private static final Map<Window, Map<String, Session>> sessions = new WeakHashMap<Window, Map<String, Session>>();

	/**
	 * Lets the user choose a connection, reads the rows with the same primary keys from it and shows the comparison.
	 *
	 * @param owner the owner window
	 * @param currentConnectionDialog the connection dialog of the current connection
	 * @param executionContext the execution context
	 * @param table the table
	 * @param columns the columns of the rows' values
	 * @param pkIndexes indexes of the primary key columns in <code>columns</code>
	 * @param fkIndexes indexes of the foreign key columns in <code>columns</code>
	 * @param rows the rows to compare
	 * @param display renders a cell value of the current connection as text
	 * @param currentSession the session of the current connection
	 * @param reverseSync makes the rows of the current connection equal to the ones of the other, or <code>null</code>
	 */
	public static void compare(Window owner, DbConnectionDialog currentConnectionDialog, ExecutionContext executionContext,
			Table table, List<Column> columns, List<Integer> pkIndexes, Set<Integer> fkIndexes, List<Object[]> rows, BiFunction<Integer, Object, String> display,
			Session currentSession, SyncHandler reverseSync) {
		ConnectionInfo currentCi = currentConnectionDialog != null? currentConnectionDialog.currentConnection : null;
		String currentAlias = currentCi != null? currentCi.alias : "current connection";
		// choosing the connection to compare with must not change the browser's connection dialog nor its context
		// (connecting sets the current connection alias of the context, which bookmarks refer to)
		ExecutionContext dialogContext = new ExecutionContext(executionContext);
		// e.g. a result of the meta data view has no connection dialog
		DbConnectionDialog connectionDialog = currentConnectionDialog != null
				? new DbConnectionDialog(owner, currentConnectionDialog, DataBrowserContext.getAppName(), null, dialogContext)
				: new DbConnectionDialog(owner, DataBrowserContext.getAppName(), null, null, dialogContext);
		// preselects the connection chosen last time instead of the current one, which is never the one to compare with
		Map<String, String> targetAliases = restoreMap(UISettings.COMPARE_TARGET_ALIASES);
		String targetKey = currentCi != null && currentCi.alias != null? currentCi.alias : "";
		String lastTarget = targetAliases.get(targetKey);
		if (lastTarget != null && connectionDialog.getConnectionList() != null) {
			for (ConnectionInfo c: connectionDialog.getConnectionList()) {
				if (lastTarget.equals(c.alias)) {
					// a copy, like the copy of the connection dialog has (the list is shared)
					ConnectionInfo copy = new ConnectionInfo();
					copy.assign(c);
					connectionDialog.currentConnection = copy;
					break;
				}
			}
		}
		if (!connectionDialog.connect(TITLE)) {
			return;
		}
		ConnectionInfo ci = connectionDialog.currentConnection;
		if (ci == null) {
			return;
		}
		if (ci.alias != null && !ci.alias.equals(lastTarget)) {
			targetAliases.put(targetKey, ci.alias);
			UISettings.store(UISettings.COMPARE_TARGET_ALIASES, targetAliases);
		}

		String originalSchema = table.getOriginalSchema("");
		Map<String, String> schemas = restoreMap(UISettings.COMPARE_SCHEMAS);
		String schemaKey = ci.alias + "\u0001" + originalSchema;
		String schema = schemas.get(schemaKey);
		if (schema == null) {
			schema = originalSchema;
			String mappedSchema = SchemaMappingDialog.restore(connectionDialog).get(schema);
			if (mappedSchema != null) {
				schema = mappedSchema;
			}
		}

		Session session;
		try {
			session = session(owner, ci, executionContext);
		} catch (CancellationException e) {
			return;
		} catch (Throwable t) {
			UIUtil.showException(owner, "Error", t);
			return;
		}
		String foundSchema = checkTable(owner, session, ci, table.getUnqualifiedName(), schema);
		if (foundSchema == null) {
			return;
		}
		if (!foundSchema.equals(schemas.containsKey(schemaKey)? schemas.get(schemaKey) : originalSchema)) {
			// remembered for the next time
			if (foundSchema.equals(originalSchema)) {
				schemas.remove(schemaKey);
			} else {
				schemas.put(schemaKey, foundSchema);
			}
			UISettings.store(UISettings.COMPARE_SCHEMAS, schemas);
		}
		String otherTableName = qualify(foundSchema, table.getUnqualifiedName());
		compare(owner, TITLE + " \"" + ci.alias + "\" - " + table.getUnqualifiedName(), session, otherTableName,
				currentAlias, ci.alias, " from \"" + ci.alias + "\"",
				connectionToolTip(currentAlias, currentCi, tableName(table), "The rows compared with those of \"" + ci.alias + "\"."),
				connectionToolTip(ci.alias, ci, otherTableName, "The rows having the same primary key, read from this table."),
				columns, pkIndexes, fkIndexes, rows, false, display,
				// a script repeating the changes would equal the synchronization script (except the Deletes), so none is offered
				nonNull(syncHandler(session, otherTableName, null, null, null, null, null, null, executionContext)),
				currentSession, tableName(table), nonNull(reverseSync));
	}

	/**
	 * Identifies a table whose columns excluded from comparisons are remembered (see {@link CompareDialog}).
	 *
	 * @param tableName the (schema qualified) name of the table in the database of the current connection
	 */
	public static String ignoredColumnsKey(String tableName) {
		return tableName.toUpperCase(Locale.ENGLISH);
	}

	/**
	 * Gets a name qualified by a schema, unqualified for the default schema ("").
	 */
	private static String qualify(String schema, String unqualifiedName) {
		return schema.isEmpty()? unqualifiedName : schema + "." + unqualifiedName;
	}

	/**
	 * Gets a map stored in the {@link UISettings}, as a copy that can be changed and stored again.
	 */
	@SuppressWarnings("unchecked")
	private static Map<String, String> restoreMap(String name) {
		Object value = UISettings.restore(name);
		return value instanceof Map? new HashMap<String, String>((Map<String, String>) value) : new HashMap<String, String>();
	}

	/**
	 * Checks that a table can be read in the database of another connection. If not, lets the user choose another schema.
	 *
	 * @param schema the schema to try first ("" for the default schema)
	 * @return the schema in which the table can be read, or <code>null</code> if cancelled or failed (the error is shown)
	 */
	private static String checkTable(Window owner, Session session, ConnectionInfo ci, String unqualifiedName, String schema) {
		while (true) {
			String tableName = qualify(schema, unqualifiedName);
			List<String> otherSchemas = new ArrayList<String>();
			String error;
			try {
				error = ConcurrentTaskControl.call(owner, () -> {
					String qualifiedName = qualifiedName(Quoting.getQuoting(session), tableName);
					try {
						readColumnLabels(session, qualifiedName);
						return null;
					} catch (SQLException e) {
						// offered to choose from
						otherSchemas.addAll(JDBCMetaDataBasedModelElementFinder.getSchemas(session, ci.user));
						return e.getMessage() != null? e.getMessage() : e.toString();
					}
				}, "Reading table \"" + tableName + "\" in \"" + ci.alias + "\"...", UIUtil.blinkingInfoLabel(null), false);
			} catch (CancellationException e) {
				return null;
			} catch (Throwable t) {
				UIUtil.showException(owner, "Error", t);
				return null;
			}
			if (error == null) {
				return schema;
			}
			String message = error.trim();
			int lineEnd = message.indexOf('\n');
			if (lineEnd > 0) {
				message = message.substring(0, lineEnd).trim();
			}
			JComboBox<String> schemaComboBox = new JComboBox<String>(otherSchemas.toArray(new String[0]));
			schemaComboBox.setEditable(true);
			schemaComboBox.setSelectedItem(schema);
			JPanel panel = new JPanel(new BorderLayout(0, 8));
			panel.add(new JLabel("<html>Table <b>" + UIUtil.toHTMLFragment(tableName, 0) + "</b> cannot be read in <b>" + UIUtil.toHTMLFragment(ci.alias, 0) + "</b>:<br>"
					+ UIUtil.toHTMLFragment(message, 100) + "<br><br>Schema of the table in <b>" + UIUtil.toHTMLFragment(ci.alias, 0) + "</b>:</html>"), BorderLayout.NORTH);
			panel.add(schemaComboBox, BorderLayout.CENTER);
			if (JOptionPane.showConfirmDialog(owner, panel, TITLE + " \"" + ci.alias + "\"", JOptionPane.OK_CANCEL_OPTION, JOptionPane.PLAIN_MESSAGE) != JOptionPane.OK_OPTION) {
				return null;
			}
			Object selected = schemaComboBox.getSelectedItem();
			schema = selected == null? "" : selected.toString().trim();
		}
	}

	/**
	 * Gets the tool tip of a side of a comparison with another connection: the table and the connection.
	 *
	 * @param title title of the side
	 * @param ci the connection, or <code>null</code> if unknown
	 * @param tableName the (schema qualified) name of the table
	 * @param description what the rows of the side are
	 */
	private static String connectionToolTip(String title, ConnectionInfo ci, String tableName, String description) {
		StringBuilder toolTip = new StringBuilder("<html><b>" + UIUtil.toHTMLFragment(title, 0) + "</b><hr>"
				+ UIUtil.toHTMLFragment(description, 0) + "<br>Table: " + UIUtil.toHTMLFragment(tableName, 0));
		if (ci != null) {
			toolTip.append("<br>URL: ").append(UIUtil.toHTMLFragment(ci.url, 0));
			toolTip.append("<br>User: ").append(UIUtil.toHTMLFragment(ci.user, 0));
		}
		return toolTip.append("</html>").toString();
	}

	/**
	 * Gets the given handlers that are not <code>null</code>.
	 */
	private static List<SyncHandler> nonNull(SyncHandler... handlers) {
		List<SyncHandler> result = new ArrayList<SyncHandler>();
		for (SyncHandler handler: handlers) {
			if (handler != null) {
				result.add(handler);
			}
		}
		return result;
	}

	/**
	 * Reads the current rows having the same primary keys as the given rows from the database of the given session
	 * and shows the comparison.
	 *
	 * @param owner the owner window
	 * @param session the session of the rows
	 * @param table the table
	 * @param columns the columns of the rows' values
	 * @param pkIndexes indexes of the primary key columns in <code>columns</code>
	 * @param fkIndexes indexes of the foreign key columns in <code>columns</code>
	 * @param rows the rows to compare
	 * @param truncated <code>true</code> if the rows have been cut by a row limit
	 * @param display renders a cell value as text
	 * @param sync restores the rows shown, or <code>null</code>
	 * @param replay creates a script that repeats the changes made since the rows have been shown, or <code>null</code>
	 */
	public static void compareWithCurrentData(Window owner, Session session, Table table, List<Column> columns, List<Integer> pkIndexes,
			Set<Integer> fkIndexes, List<Object[]> rows, boolean truncated, BiFunction<Integer, Object, String> display, SyncHandler sync, SyncHandler replay) {
		compare(owner, "Compare with Current Data - " + table.getUnqualifiedName(), session, tableName(table),
				"Rows shown", CompareTabs.CURRENT_DATA, "",
				"<html><b>Rows shown</b><hr>The rows of the table browser as they were shown.</html>",
				"<html><b>" + CompareTabs.CURRENT_DATA + "</b><hr>The rows of table " + UIUtil.toHTMLFragment(tableName(table), 0)
					+ " with the same primary key, read again.<br>Rows added since they were shown are not included.</html>",
				columns, pkIndexes, fkIndexes, rows, truncated, display, nonNull(sync), null, null, nonNull(replay));
	}

	/**
	 * Gets the (schema qualified) name of a table in the database of the current connection.
	 */
	public static String tableName(Table table) {
		String schema = table.getSchema("");
		return schema.isEmpty()? table.getUnqualifiedName() : schema + "." + table.getUnqualifiedName();
	}

	/**
	 * Creates the handler that makes the rows of a table (right side of a comparison) equal to the rows of the left side.
	 *
	 * The primary key columns are those known by one of the sides (see {@link Side#withKeyColumns(Set, Set)}).
	 *
	 * @param session the session of the table
	 * @param tableName the (schema qualified) name of the table
	 * @param targetColumns names of the columns of the table per index of the right side (<code>null</code> for one that is no column of the table),
	 *                      or <code>null</code> for the column names of the right side
	 * @param sourceColumns names of the columns of the same table per index of the left side, used where <code>targetColumns</code> has none, or <code>null</code>
	 * @param sqlConsole the SQL Console of the session, or <code>null</code>
	 * @param metaDataSource the meta data of the session (for code completion), or <code>null</code>
	 * @param switchToConsole shows the SQL Console, or <code>null</code>
	 * @param afterExecution called after the script has been executed (after the comparison has been refreshed), or <code>null</code>
	 * @param executionContext the execution context
	 */
	public static SyncHandler syncHandler(Session session, String tableName, List<String> targetColumns, List<String> sourceColumns,
			SQLConsole sqlConsole, MetaDataSource metaDataSource, Runnable switchToConsole, Runnable afterExecution, ExecutionContext executionContext) {
		return (owner, comparison, pairs, refresh) -> openScript(owner, session, tableName, targetColumns, sourceColumns, false, null,
				sqlConsole, metaDataSource, switchToConsole, () -> {
					// executing the script compares again
					refresh.run();
					if (afterExecution != null) {
						afterExecution.run();
					}
				}, executionContext, comparison, pairs);
	}

	/**
	 * Creates the handler that creates a script that repeats the changes, i.e. that makes rows of the state of the left side
	 * (e.g. rows shown before the changes) equal to the right side (the current rows).
	 * The rows in the table are in that state already, so the script is not executed, but saved or copied.
	 *
	 * @param session the session of the table
	 * @param tableName the (schema qualified) name of the table
	 * @param targetColumns names of the columns of the table per index of the right side, see {@link #syncHandler(Session, String, List, List, SQLConsole, MetaDataSource, Runnable, Runnable, ExecutionContext)}
	 * @param sqlConsole the SQL Console of the session, or <code>null</code>
	 * @param metaDataSource the meta data of the session (for code completion), or <code>null</code>
	 * @param switchToConsole shows the SQL Console, or <code>null</code>
	 * @param executionContext the execution context
	 * @param note additional comment in the head of the script, or <code>null</code>
	 */
	public static SyncHandler replayHandler(Session session, String tableName, List<String> targetColumns,
			SQLConsole sqlConsole, MetaDataSource metaDataSource, Runnable switchToConsole, ExecutionContext executionContext, String note) {
		return new SyncHandler() {
			@Override
			public void open(Window owner, RowComparison comparison, List<RowPair> pairs, Runnable refresh) {
				openScript(owner, session, tableName, targetColumns, null, true, note, sqlConsole, metaDataSource, switchToConsole, null, executionContext, comparison, pairs);
			}
			@Override
			public String menuText(RowComparison comparison) {
				return "Script that repeats the changes (makes \"" + comparison.right.title + "\" equal to \"" + comparison.left.title + "\")";
			}
		};
	}

	/**
	 * Builds the script that makes the right side equal to the left one and shows it.
	 *
	 * @param replay whether the script repeats changes (see {@link #replayHandler(Session, String, List, SQLConsole, MetaDataSource, Runnable, ExecutionContext, String)}),
	 *               it is not executed then
	 * @param afterExecution called after the script has been executed
	 */
	private static void openScript(Window owner, Session session, String tableName, List<String> targetColumns, List<String> sourceColumns, boolean replay, String note,
			SQLConsole sqlConsole, MetaDataSource metaDataSource, Runnable switchToConsole, Runnable afterExecution, ExecutionContext executionContext,
			RowComparison comparison, List<RowPair> pairs) {
		JDialog d;
		try {
			String script = buildSyncScript(session, tableName, targetColumns, sourceColumns, replay, note, comparison, pairs);
			d = new JDialog(owner, (replay? "Repeat Changes - " : "Synchronize \"" + comparison.right.title + "\" - ") + tableName, ModalityType.APPLICATION_MODAL);
			SQLDMLPanel panel = new SQLDMLPanel(script, sqlConsole, session, metaDataSource, afterExecution != null? afterExecution : () -> {}, switchToConsole, d, executionContext);
			// the SQL Console is behind the comparison dialog
			panel.setConsoleAppendedMessage("The statements have been appended to the SQL Console.\nThey have not been executed yet.");
			if (replay) {
				// the rows are in that state already
				panel.setExecutable(false);
			}
			d.getContentPane().add(panel);
			d.pack();
			d.setSize(800, Math.max(d.getHeight() + 20, 400));
			UIUtil.setInitialWindowLocation(d, owner, 100, 100);
			UIUtil.fit(d);
		} catch (MissingColumnException e) {
			UIUtil.showException(owner, "Synchronize", e, UIUtil.EXCEPTION_CONTEXT_USER_ERROR);
			return;
		} catch (Throwable t) {
			UIUtil.showException(owner, "Error", t);
			return;
		}
		d.setVisible(true);
	}

	/**
	 * Reads the rows with the same primary keys from a table and shows the comparison, which can be refreshed.
	 *
	 * @param leftToolTip tool tip of the given rows, or <code>null</code>
	 * @param rightToolTip tool tip of the rows read from the table, or <code>null</code>
	 * @param syncs make the rows in the table equal to the given ones
	 * @param leftSession the session the given rows have been read from, whose rows are read again after a reverse synchronization,
	 *                    or <code>null</code> if they are not (then <code>reverseSyncs</code> don't change them)
	 * @param leftTableName name of the table the given rows have been read from, or <code>null</code>
	 * @param reverseSyncs make the given rows equal to the ones in the table
	 */
	private static void compare(Window owner, String title, Session session, String tableName, String leftTitle, String rightTitle, String readingFrom,
			String leftToolTip, String rightToolTip, List<Column> columns, List<Integer> pkIndexes, Set<Integer> fkIndexes, List<Object[]> rows, boolean truncated, BiFunction<Integer, Object, String> display,
			List<SyncHandler> syncs, Session leftSession, String leftTableName, List<SyncHandler> reverseSyncs) {
		List<String> columnNames = new ArrayList<String>();
		Set<Integer> charColumns = new HashSet<Integer>();
		for (int i = 0; i < columns.size(); ++i) {
			// a column of a result of the SQL Console that is no column of the table (an expression) has no name
			String name = columns.get(i).name;
			columnNames.add(name == null? "(column " + (i + 1) + ")" : Quoting.staticUnquote(name));
			if (UIUtil.isCHARType(columns.get(i))) {
				charColumns.add(i);
			}
		}
		// the columns of the left side don't change, so their aligned indexes (the key) don't either.
		// Its rows are read again after they have been synchronized.
		AtomicReference<Side> left = new AtomicReference<Side>(new Side(leftTitle, columnNames, rows, truncated, display).withKeyColumns(new HashSet<Integer>(pkIndexes), fkIndexes)
				.withToolTip(leftToolTip).withCharColumns(charColumns));
		// the padding of CHAR columns differs between DBMS; with the same DBMS on both sides, values are compared exactly
		boolean ignoreTrailingBlanks = leftSession != null && leftSession.dbms != null && !leftSession.dbms.equals(session.dbms);
		Function<Window, RowComparison> recompare = o -> readComparison(o, title, session, tableName, rightTitle, readingFrom, rightToolTip, columns, pkIndexes, left.get(),
				ignoreTrailingBlanks);
		RowComparison comparison = recompare.apply(owner);
		if (comparison != null) {
			List<SyncHandler> reverse = new ArrayList<SyncHandler>();
			for (SyncHandler reverseSync: reverseSyncs) {
				reverse.add(leftSession == null? reverseSync : new SyncHandler() {
					@Override
					public void open(Window o, RowComparison c, List<RowPair> pairs, Runnable refresh) {
						reverseSync.open(o, c, pairs, () -> {
							List<Object[]> newRows = readCurrentRows(o, leftSession, leftTableName, columns, pkIndexes, left.get());
							if (newRows != null) {
								left.set(new Side(leftTitle, columnNames, newRows, false, display).withKeyColumns(new HashSet<Integer>(pkIndexes), fkIndexes)
										.withToolTip(leftToolTip).withCharColumns(charColumns));
							}
							refresh.run();
						});
					}
					@Override
					public String menuText(RowComparison c) {
						return reverseSync.menuText(c);
					}
				});
			}
			List<Integer> keyColumns = new ArrayList<Integer>(pkIndexes);
			new CompareDialog(owner, title, comparison, comparison.matchByKey(keyColumns), keyColumns, recompare, syncs, reverse,
					ignoredColumnsKey(leftTableName != null? leftTableName : tableName));
		}
	}

	/**
	 * Reads the current rows of a side again, in the order and with the columns of the side.
	 * A row deleted meanwhile is left out.
	 *
	 * @return the rows, or <code>null</code> if cancelled or failed (the error is shown)
	 */
	private static List<Object[]> readCurrentRows(Window owner, Session session, String tableName, List<Column> columns, List<Integer> pkIndexes, Side side) {
		Object context = new Object();
		try {
			List<String> labels = new ArrayList<String>();
			List<Object[]> read = ConcurrentTaskControl.call(owner, () -> readRows(session, tableName, "", columns, pkIndexes, side.rows, labels, new HashSet<Integer>(), context),
					"Reading " + side.rows.size() + " row" + (side.rows.size() == 1? "" : "s") + "...", UIUtil.blinkingInfoLabel(null), false);
			Map<String, Integer> labelIndex = new HashMap<String, Integer>();
			for (int i = 0; i < labels.size(); ++i) {
				labelIndex.putIfAbsent(RowComparison.normalizeName(labels.get(i)), i);
			}
			// index of the value per column of the side, -1 if it isn't a column of the table (keeps the value shown)
			int[] valueIndex = new int[side.columns.size()];
			for (int i = 0; i < valueIndex.length; ++i) {
				Integer index = columns.get(i).name == null? null : labelIndex.get(RowComparison.normalizeName(columns.get(i).name));
				valueIndex[i] = index == null? -1 : index;
			}
			Map<String, Object[]> readByKey = new HashMap<String, Object[]>();
			for (Object[] r: read) {
				StringBuilder key = new StringBuilder();
				for (int k: pkIndexes) {
					key.append(valueIndex[k] < 0? "" : RowComparison.normalize(r[valueIndex[k]], false)).append('\u0001');
				}
				readByKey.put(key.toString(), r);
			}
			List<Object[]> result = new ArrayList<Object[]>();
			for (Object[] old: side.rows) {
				StringBuilder key = new StringBuilder();
				for (int k: pkIndexes) {
					key.append(RowComparison.normalize(old[k], false)).append('\u0001');
				}
				Object[] r = readByKey.get(key.toString());
				if (r != null) {
					Object[] row = new Object[valueIndex.length];
					for (int i = 0; i < valueIndex.length; ++i) {
						row[i] = valueIndex[i] < 0? old[i] : r[valueIndex[i]];
					}
					result.add(row);
				}
			}
			return result;
		} catch (CancellationException e) {
			CancellationHandler.cancel(context);
		} catch (MissingColumnException e) {
			UIUtil.showException(owner, "Synchronize", e, UIUtil.EXCEPTION_CONTEXT_USER_ERROR);
		} catch (Throwable t) {
			UIUtil.showException(owner, "Error", t);
		} finally {
			CancellationHandler.reset(context);
		}
		return null;
	}

	/**
	 * Builds the script that makes the rows in the table (right side) equal to the given ones (left side):
	 * inserts the missing rows and updates the different columns, except the primary key. LOBs are not synchronized.
	 * A row on the right side only is not deleted, the script lists its Delete as a comment.
	 *
	 * @param replay whether the script repeats changes: a row on the right side only is deleted then
	 * @param note additional comment in the head of the script, or <code>null</code>
	 */
	private static String buildSyncScript(Session session, String tableName, List<String> targetColumns, List<String> sourceColumns,
			boolean replay, String note, RowComparison comparison, List<RowPair> pairs) throws SQLException {
		Quoting quoting = Quoting.getQuoting(session);
		String qualifiedName = qualifiedName(quoting, tableName);
		CellContentConverter converter = new CellContentConverter(null, session, session.dbms);
		List<String> targetNames = targetColumns != null? targetColumns : comparison.right.columns;
		if (targetNames.size() != comparison.right.columns.size()) {
			// e.g. a statement executed again now returns other columns
			throw new MissingColumnException("The columns of \"" + comparison.right.title + "\" are not those of table \"" + tableName + "\".");
		}
		// the name in the table per aligned column, null if it's none of its columns
		String[] target = new String[comparison.columns.size()];
		for (int i = 0; i < target.length; ++i) {
			int l = comparison.leftIndex(i);
			int r = comparison.rightIndex(i);
			if (r >= 0) {
				target[i] = targetNames.get(r);
				if (target[i] == null && l >= 0 && sourceColumns != null && l < sourceColumns.size()) {
					// the left side is a result of the same table that knows the column
					target[i] = sourceColumns.get(l);
				}
			}
		}
		// the sides may have been swapped, so the key is taken from the sides, not from the indexes of the columns
		List<Integer> pkColumns = new ArrayList<Integer>();
		List<String> pkNames = new ArrayList<String>();
		for (int i = 0; i < comparison.columns.size(); ++i) {
			if (comparison.isPrimaryKeyOfBoth(i) && target[i] != null) {
				pkColumns.add(i);
				pkNames.add(target[i]);
			}
		}
		if (pkColumns.isEmpty()) {
			throw new MissingColumnException("The primary key of table \"" + tableName + "\" is not found on both sides.");
		}
		// sorted by kind: Inserts, Updates, Deletes (each in the order of the rows)
		StringBuilder insertStatements = new StringBuilder();
		StringBuilder updateStatements = new StringBuilder();
		StringBuilder deleteStatements = new StringBuilder();
		int inserts = 0, updates = 0, deletes = 0;
		for (RowPair pair: pairs) {
			if (pair.status == Status.ONLY_LEFT) {
				StringBuilder names = new StringBuilder();
				StringBuilder values = new StringBuilder();
				StringBuilder skipped = new StringBuilder();
				for (int i = 0; i < comparison.columns.size(); ++i) {
					int l = comparison.leftIndex(i);
					int r = comparison.rightIndex(i);
					if (l < 0 || r < 0) {
						continue;
					}
					if (target[i] == null) {
						skipped.append(notAColumn(comparison, i, tableName));
						continue;
					}
					Object value = pair.left[l];
					String literal = literal(value, converter);
					if (literal == null) {
						skipped.append("-- ").append(comparison.columns.get(i)).append(": LOB not synchronized").append(LF);
						continue;
					}
					if (names.length() > 0) {
						names.append(", ");
						values.append(", ");
					}
					names.append(quoting.quote(target[i]));
					values.append(literal);
				}
				insertStatements.append(skipped).append("Insert into ").append(qualifiedName).append("(").append(names).append(")")
					.append(" Values(").append(values).append(");").append(LF);
				++inserts;
			} else if (pair.status == Status.CHANGED) {
				StringBuilder set = new StringBuilder();
				StringBuilder skipped = new StringBuilder();
				for (int i = 0; i < comparison.columns.size(); ++i) {
					// the key identifies the row there (it differs if two rows of one table are compared)
					if (comparison.cellStatus(pair, i) != CellStatus.CHANGED || pkColumns.contains(i)) {
						continue;
					}
					if (target[i] == null) {
						skipped.append(notAColumn(comparison, i, tableName));
						continue;
					}
					Object value = comparison.leftValue(pair, i);
					String literal = literal(value, converter);
					if (literal == null) {
						skipped.append("-- ").append(comparison.columns.get(i)).append(": LOB not synchronized").append(LF);
						continue;
					}
					if (set.length() > 0) {
						set.append(", ");
					}
					set.append(quoting.quote(target[i])).append("=").append(literal);
				}
				updateStatements.append(skipped);
				if (set.length() > 0) {
					StringBuilder where = new StringBuilder();
					appendPkCondition(where, rightKey(comparison, pair, pkColumns), pkNames, quoting, converter);
					updateStatements.append("Update ").append(qualifiedName).append(" Set ").append(set).append(" Where ").append(where).append(";").append(LF);
					++updates;
				} else {
					String row = pair.key.isEmpty()? comparison.right.title : pair.key;
					updateStatements.append("-- ").append(row.replaceAll("[\\r\\n]+", " ")).append(": nothing to update").append(LF);
				}
			} else if (pair.status == Status.ONLY_RIGHT) {
				StringBuilder where = new StringBuilder();
				appendPkCondition(where, rightKey(comparison, pair, pkColumns), pkNames, quoting, converter);
				if (replay) {
					// the row has been deleted
					deleteStatements.append("Delete from ").append(qualifiedName).append(" Where ").append(where).append(";").append(LF);
				} else if (where.indexOf("\n") >= 0 || where.indexOf("\r") >= 0) {
					// a line break would end the comment
					deleteStatements.append("-- (a row whose key contains a line break is not listed)").append(LF);
				} else {
					deleteStatements.append("-- Delete from ").append(qualifiedName).append(" Where ").append(where).append(";").append(LF);
				}
				++deletes;
			}
		}
		String head;
		String counts = inserts + " insert" + (inserts == 1? "" : "s") + ", " + updates + " update" + (updates == 1? "" : "s");
		if (replay) {
			head = "-- Redo script: repeats the changes from \"" + comparison.right.title + "\" to \"" + comparison.left.title
					+ "\". Execute it on a database whose rows are in the state of \"" + comparison.right.title + "\", not on \"" + comparison.left.title + "\"." + LF
				+ "-- " + counts + ", " + deletes + " delete" + (deletes == 1? "" : "s") + "." + LF;
		} else {
			head = "-- Synchronization script: makes the rows in \"" + comparison.right.title + "\" equal to the rows of \"" + comparison.left.title
					+ "\". Execute it on \"" + comparison.right.title + "\"." + LF
				+ "-- " + counts + "." + LF
				+ (deletes == 0? "" : "-- " + deletes + (deletes == 1? " row is" : " rows are") + " not in \"" + comparison.left.title
					+ "\". " + (deletes == 1? "Its Delete is" : "Their Deletes are") + " commented out: remove the \"--\" to delete " + (deletes == 1? "it" : "them") + "." + LF
					+ "-- To do so, select the line" + (deletes == 1? "" : "s") + " and press Ctrl+Shift+7 (or use \"Toggle Comment\" in the context menu)." + LF);
		}
		List<String> ignored = comparison.ignoredColumnNames();
		if (!ignored.isEmpty()) {
			head += "-- Not compared, so not updated: " + String.join(", ", ignored).replaceAll("[\\r\\n]+", " ") + LF;
		}
		return head + (note == null? "" : "-- " + note + LF) + LF + insertStatements + updateStatements + deleteStatements;
	}

	/**
	 * Comment on an aligned column that can't be synchronized as it is no column of the table (e.g. an expression of a result).
	 */
	private static String notAColumn(RowComparison comparison, int column, String tableName) {
		return "-- " + comparison.columns.get(column) + ": not a column of table \"" + tableName + "\" in \"" + comparison.right.title + "\"" + LF;
	}

	/**
	 * Gets the key values of the right row of a pair, as they are there.
	 */
	private static List<Object> rightKey(RowComparison comparison, RowPair pair, List<Integer> pkColumns) {
		List<Object> values = new ArrayList<Object>();
		for (int k: pkColumns) {
			values.add(comparison.rightValue(pair, k));
		}
		return values;
	}

	/**
	 * Gets the SQL literal of a value, or <code>null</code> for a LOB.
	 */
	private static String literal(Object value, CellContentConverter converter) {
		if (value == null) {
			return "null";
		}
		if (value instanceof LobValue || value instanceof Blob || value instanceof Clob || value instanceof SQLXML) {
			return null;
		}
		return SQLDMLBuilder.getSQLLiteral(value, converter);
	}

	/**
	 * Quotes the (possibly schema qualified) name of a table.
	 */
	private static String qualifiedName(Quoting quoting, String tableName) {
		int dot = SqlUtil.indexOfDot(tableName);
		if (dot > 0) {
			return quoting.requote(tableName.substring(0, dot)) + "." + quoting.requote(tableName.substring(dot + 1));
		}
		return quoting.requote(tableName);
	}

	/**
	 * Appends the condition selecting a row by its primary key.
	 *
	 * @param pkValues the primary key values of the row
	 * @param pkNames names of the primary key columns in the table, as read from there (in the order of <code>pkValues</code>)
	 */
	private static void appendPkCondition(StringBuilder where, List<Object> pkValues, List<String> pkNames, Quoting quoting, CellContentConverter converter) {
		for (int i = 0; i < pkValues.size(); ++i) {
			if (i > 0) {
				where.append(" and ");
			}
			Object value = pkValues.get(i);
			where.append(quoting.quote(pkNames.get(i)));
			where.append(value == null? " is null" : "=" + converter.toSql(value));
		}
	}

	private static final String LF = System.getProperty("line.separator", "\n");

	/**
	 * Reads the rows with the same primary keys from a table and compares them with the given ones.
	 *
	 * @param ignoreTrailingBlanks whether values differing in trailing blanks only are equal (see {@link RowComparison#withIgnoreTrailingBlanks(boolean)})
	 * @return the comparison, or <code>null</code> if cancelled or failed (the error is shown)
	 */
	private static RowComparison readComparison(Window owner, String title, Session session, String tableName, String rightTitle, String readingFrom,
			String rightToolTip, List<Column> columns, List<Integer> pkIndexes, Side left, boolean ignoreTrailingBlanks) {
		Object context = new Object();
		try {
			List<String> otherColumns = new ArrayList<String>();
			Set<Integer> otherCharColumns = new HashSet<Integer>();
			// the connection is named only if it's another one
			String connection = readingFrom.isEmpty()? "" : rightTitle;
			List<Object[]> otherRows = ConcurrentTaskControl.call(owner, () -> readRows(session, tableName, connection, columns, pkIndexes, left.rows, otherColumns, otherCharColumns, context),
					"Reading " + left.rows.size() + " row" + (left.rows.size() == 1? "" : "s") + readingFrom + "...", UIUtil.blinkingInfoLabel(null), false);

			RowComparison comparison = new RowComparison(left, new Side(rightTitle, otherColumns, otherRows, false, null).withToolTip(rightToolTip).withCharColumns(otherCharColumns))
					.withIgnoreTrailingBlanks(ignoreTrailingBlanks);
			for (int k: pkIndexes) {
				if (comparison.rightIndex(k) < 0) {
					UIUtil.showException(owner, title, new SQLException("Primary key column \"" + left.columns.get(k) + "\" not found in table \"" + tableName + "\"."), UIUtil.EXCEPTION_CONTEXT_USER_ERROR);
					return null;
				}
			}
			return comparison;
		} catch (CancellationException e) {
			CancellationHandler.cancel(context);
		} catch (MissingColumnException e) {
			UIUtil.showException(owner, title, e, UIUtil.EXCEPTION_CONTEXT_USER_ERROR);
		} catch (Throwable t) {
			UIUtil.showException(owner, "Error", t);
		} finally {
			CancellationHandler.reset(context);
		}
		return null;
	}

	/**
	 * Reads the rows with the given primary keys.
	 *
	 * @param rightTitle name of the connection of the table, or "" for the current one
	 * @param otherColumns receives the column labels of the table
	 * @param charColumns receives the indexes of its columns of type CHAR (or NCHAR)
	 */
	private static List<Object[]> readRows(Session session, String otherTableName, String rightTitle, List<Column> columns, List<Integer> pkIndexes,
			List<Object[]> rows, List<String> otherColumns, Set<Integer> charColumns, Object context) throws SQLException {
		Quoting quoting = Quoting.getQuoting(session);
		String qualifiedName = qualifiedName(quoting, otherTableName);
		CellContentConverter converter = new CellContentConverter(null, session, session.dbms);

		// the columns of the table there, which may differ from the ones here (another schema, another version)
		otherColumns.addAll(readColumnLabels(session, qualifiedName, charColumns));
		Map<String, String> otherByName = new HashMap<String, String>();
		for (String c: otherColumns) {
			otherByName.putIfAbsent(RowComparison.normalizeName(c), c);
		}
		List<String> pkNames = new ArrayList<String>();
		for (int k: pkIndexes) {
			String name = otherByName.get(RowComparison.normalizeName(columns.get(k).name));
			if (name == null) {
				throw new MissingColumnException("Primary key column \"" + Quoting.staticUnquote(columns.get(k).name) + "\" not found in table \"" + otherTableName + "\""
						+ (rightTitle.isEmpty()? "" : " of \"" + rightTitle + "\"") + ".\n"
						+ "Columns found there: " + String.join(", ", otherColumns));
			}
			pkNames.add(name);
		}

		List<Object[]> result = new ArrayList<Object[]>();
		for (int start = 0; start < rows.size(); start += BATCH_SIZE) {
			StringBuilder where = new StringBuilder();
			for (Object[] row: rows.subList(start, Math.min(rows.size(), start + BATCH_SIZE))) {
				if (where.length() > 0) {
					where.append(" or ");
				}
				List<Object> pkValues = new ArrayList<Object>();
				for (int k: pkIndexes) {
					pkValues.add(row[k]);
				}
				where.append("(");
				appendPkCondition(where, pkValues, pkNames, quoting, converter);
				where.append(")");
			}
			String sql = "Select * From " + qualifiedName + " Where " + where;
			session.executeQuery(sql, new AbstractResultSetReader() {
				@Override
				public void readCurrentRow(ResultSet resultSet) throws SQLException {
					int count = getMetaData(resultSet).getColumnCount();
					CellContentConverter cellContentConverter = getCellContentConverter(resultSet, session, session.dbms);
					Object[] values = new Object[count];
					for (int i = 1; i <= count; ++i) {
						Object value = cellContentConverter.getObject(resultSet, i);
						if (resultSet.wasNull()) {
							value = null;
						} else {
							// like the SQL Console: a LOB is shown by its render, and is no longer valid once the result set is closed
							Object lobValue = BrowserContentPane.toLobRender(value);
							if (lobValue != null) {
								value = lobValue;
							}
						}
						values[i - 1] = value;
					}
					result.add(values);
				}
			}, null, context, 0);
		}
		return result;
	}

	/**
	 * Reads the column labels of a table (also if it has no rows).
	 */
	private static List<String> readColumnLabels(Session session, String qualifiedName) throws SQLException {
		return readColumnLabels(session, qualifiedName, new HashSet<Integer>());
	}

	/**
	 * Reads the column labels of a table (also if it has no rows).
	 *
	 * @param charColumns receives the indexes of the columns of type CHAR (or NCHAR)
	 */
	private static List<String> readColumnLabels(Session session, String qualifiedName, Set<Integer> charColumns) throws SQLException {
		List<String> labels = new ArrayList<String>();
		try (Statement statement = session.getConnection().createStatement();
				ResultSet resultSet = statement.executeQuery("Select * From " + qualifiedName + " Where 1=0")) {
			ResultSetMetaData metaData = resultSet.getMetaData();
			for (int i = 1; i <= metaData.getColumnCount(); ++i) {
				labels.add(metaData.getColumnLabel(i));
				if (metaData.getColumnType(i) == Types.CHAR || metaData.getColumnType(i) == Types.NCHAR) {
					charColumns.add(i - 1);
				}
			}
		}
		return labels;
	}

	/**
	 * A column needed for the comparison doesn't exist in the table (a user error, not a failure).
	 */
	@SuppressWarnings("serial")
	private static class MissingColumnException extends SQLException {
		MissingColumnException(String message) {
			super(message);
		}
	}

	/**
	 * Gets the (cached) session to the database of a connection.
	 */
	private static synchronized Session session(Window owner, ConnectionInfo ci, ExecutionContext executionContext) throws Exception {
		Map<String, Session> perOwner = sessions.get(owner);
		if (perOwner == null) {
			perOwner = new HashMap<String, Session>();
			sessions.put(owner, perOwner);
			owner.addWindowListener(new WindowAdapter() {
				@Override
				public void windowClosed(WindowEvent e) {
					closeSessions(owner);
				}
			});
		}
		String key = ci.alias + "\u0001" + ci.user + "@" + ci.url;
		Session session = perOwner.get(key);
		if (session == null || session.isDown()) {
			BasicDataSource dataSource = UIUtil.createBasicDataSource(owner, ci.driverClass, ci.url, ci.user, ci.password, 0,
					ClasspathUtil.toURLArray(ci.jar1, ci.jar2, ci.jar3, ci.jar4));
			session = ConcurrentTaskControl.call(owner, () -> new Session(dataSource, dataSource.dbms, executionContext.getIsolationLevel()),
					"Connecting to \"" + ci.alias + "\"...", UIUtil.blinkingInfoLabel(null), false);
			// needed to execute a script (see DbConnectionDialog#addDbArgs)
			List<String> args = new ArrayList<String>();
			args.add(ci.driverClass);
			args.add(ci.url);
			args.add(ci.user);
			args.add(ci.password);
			String[] jars = new String[] { ci.jar1, ci.jar2, ci.jar3, ci.jar4 };
			for (int i = 0; i < jars.length; ++i) {
				if (jars[i] != null && jars[i].trim().length() > 0) {
					args.add(i == 0? "-jdbcjar" : "-jdbcjar" + (i + 1));
					args.add(jars[i].trim());
				}
			}
			session.setCliArguments(args);
			session.setPassword(ci.password);
			perOwner.put(key, session);
		}
		return session;
	}

	private static synchronized void closeSessions(Window owner) {
		Map<String, Session> perOwner = sessions.remove(owner);
		if (perOwner != null) {
			for (Session session: perOwner.values()) {
				session.shutDown();
			}
		}
	}

}
