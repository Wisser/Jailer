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

import java.awt.Window;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.WeakHashMap;
import java.util.concurrent.CancellationException;
import java.util.function.BiFunction;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.database.BasicDataSource;
import net.sf.jailer.database.Session;
import net.sf.jailer.database.Session.AbstractResultSetReader;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.DbConnectionDialog;
import net.sf.jailer.ui.DbConnectionDialog.ConnectionInfo;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.DataBrowserContext;
import net.sf.jailer.ui.databrowser.SchemaMappingDialog;
import net.sf.jailer.ui.databrowser.compare.RowComparison.RowPair;
import net.sf.jailer.ui.databrowser.compare.RowComparison.Side;
import net.sf.jailer.ui.util.ConcurrentTaskControl;
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

	private static final String TITLE = "Compare with Rows in";
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
	 */
	public static void compare(Window owner, DbConnectionDialog currentConnectionDialog, ExecutionContext executionContext,
			Table table, List<Column> columns, List<Integer> pkIndexes, Set<Integer> fkIndexes, List<Object[]> rows, BiFunction<Integer, Object, String> display) {
		// the SQL Console has no connection dialog of its own
		DbConnectionDialog connectionDialog = currentConnectionDialog != null
				? new DbConnectionDialog(owner, currentConnectionDialog, DataBrowserContext.getAppName(), null, executionContext)
				: new DbConnectionDialog(owner, DataBrowserContext.getAppName(), null, null, executionContext);
		if (!connectionDialog.connect(TITLE)) {
			return;
		}
		ConnectionInfo ci = connectionDialog.currentConnection;
		if (ci == null) {
			return;
		}

		String schema = table.getOriginalSchema("");
		String mappedSchema = SchemaMappingDialog.restore(connectionDialog).get(schema);
		if (mappedSchema != null) {
			schema = mappedSchema;
		}
		String otherTableName = schema.isEmpty()? table.getUnqualifiedName() : schema + "." + table.getUnqualifiedName();

		String currentAlias = currentConnectionDialog != null && currentConnectionDialog.currentConnection != null? currentConnectionDialog.currentConnection.alias : "current connection";
		Object context = new Object();
		try {
			Session session = session(owner, ci, executionContext);
			List<String> otherColumns = new ArrayList<String>();
			List<Object[]> otherRows = ConcurrentTaskControl.call(owner, () -> readRows(session, otherTableName, columns, pkIndexes, rows, otherColumns, context),
					"Reading " + rows.size() + " row" + (rows.size() == 1? "" : "s") + " from \"" + ci.alias + "\"...", null);

			List<String> columnNames = new ArrayList<String>();
			for (Column column: columns) {
				columnNames.add(Quoting.staticUnquote(column.name));
			}
			RowComparison comparison = new RowComparison(
					new Side(currentAlias, columnNames, rows, false, display).withKeyColumns(new HashSet<Integer>(pkIndexes), fkIndexes),
					new Side(ci.alias, otherColumns, otherRows, false, null));
			List<Integer> keyColumns = new ArrayList<Integer>(pkIndexes);
			for (int k: keyColumns) {
				if (comparison.rightIndex(k) < 0) {
					UIUtil.showException(owner, TITLE, new SQLException("Primary key column \"" + columnNames.get(k) + "\" not found in table \"" + otherTableName + "\"."), UIUtil.EXCEPTION_CONTEXT_USER_ERROR);
					return;
				}
			}
			List<RowPair> pairs = comparison.matchByKey(keyColumns);
			new CompareDialog(owner, TITLE + " \"" + ci.alias + "\" - " + table.getUnqualifiedName(), comparison, pairs);
		} catch (CancellationException e) {
			CancellationHandler.cancel(context);
		} catch (Throwable t) {
			UIUtil.showException(owner, "Error", t);
		} finally {
			CancellationHandler.reset(context);
		}
	}

	/**
	 * Reads the rows with the given primary keys.
	 */
	private static List<Object[]> readRows(Session session, String otherTableName, List<Column> columns, List<Integer> pkIndexes,
			List<Object[]> rows, List<String> otherColumns, Object context) throws SQLException {
		Quoting quoting = Quoting.getQuoting(session);
		String qualifiedName;
		int dot = SqlUtil.indexOfDot(otherTableName);
		if (dot > 0) {
			qualifiedName = quoting.requote(otherTableName.substring(0, dot)) + "." + quoting.requote(otherTableName.substring(dot + 1));
		} else {
			qualifiedName = quoting.requote(otherTableName);
		}
		CellContentConverter converter = new CellContentConverter(null, session, session.dbms);
		List<Object[]> result = new ArrayList<Object[]>();
		for (int start = 0; start < rows.size(); start += BATCH_SIZE) {
			StringBuilder where = new StringBuilder();
			for (Object[] row: rows.subList(start, Math.min(rows.size(), start + BATCH_SIZE))) {
				if (where.length() > 0) {
					where.append(" or ");
				}
				where.append("(");
				boolean first = true;
				for (int k: pkIndexes) {
					if (!first) {
						where.append(" and ");
					}
					first = false;
					Object value = row[k];
					where.append(quoting.requote(columns.get(k).name));
					where.append(value == null? " is null" : "=" + converter.toSql(value));
				}
				where.append(")");
			}
			String sql = "Select * From " + qualifiedName + " Where " + where;
			session.executeQuery(sql, new AbstractResultSetReader() {
				@Override
				public void readCurrentRow(ResultSet resultSet) throws SQLException {
					ResultSetMetaData metaData = getMetaData(resultSet);
					int count = metaData.getColumnCount();
					if (otherColumns.isEmpty()) {
						for (int i = 1; i <= count; ++i) {
							otherColumns.add(metaData.getColumnLabel(i));
						}
					}
					CellContentConverter cellContentConverter = getCellContentConverter(resultSet, session, session.dbms);
					Object[] values = new Object[count];
					for (int i = 1; i <= count; ++i) {
						Object value = cellContentConverter.getObject(resultSet, i);
						values[i - 1] = resultSet.wasNull()? null : value;
					}
					result.add(values);
				}
			}, null, context, 0);
		}
		if (otherColumns.isEmpty()) {
			// no row found, all rows are missing there
			for (Column column: columns) {
				otherColumns.add(column.name);
			}
		}
		return result;
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
					"Connecting to \"" + ci.alias + "\"...", null);
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
