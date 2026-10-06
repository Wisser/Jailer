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

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.Timer;

import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.metadata.MDSchema;
import net.sf.jailer.ui.progress.RowOriginContext;
import net.sf.jailer.util.CancellationException;
import net.sf.jailer.util.CancellationHandler;
import net.sf.jailer.util.LogUtil;

/**
 * Knows which rows of a table browser are part of the subset of the run which keeps its collected
 * rows, and which of them are subject rows. Asks the retained entity-graph in the background, only
 * for the rows which have become visible, and only once per row.
 *
 * @author Ralf Wisser
 */
class RowOriginMembership {

	/**
	 * The table browser the verdicts are for.
	 */
	interface Host {

		/**
		 * Whether the table of the browser belongs to a run which has kept its collected rows.
		 *
		 * @return the context, or <code>null</code> if nothing can be analyzed here
		 */
		RowOriginContext rowOriginContextForTable();

		/**
		 * @return the name of the table of the browser
		 */
		String getTableName();

		/**
		 * Called on the event dispatch thread when new verdicts have arrived.
		 */
		void membershipChanged();

		/**
		 * @return the component a failure of the scan is reported on
		 */
		Component getComponent();
	}

	private final Host host;

	/**
	 * Verdict per row, keyed by {@link Row#nonEmptyRowId}: is that row part of the subset of the
	 * run which currently keeps its collected rows? Absent means "not asked yet".
	 */
	private final Map<String, Boolean> rowOriginMembership = new HashMap<String, Boolean>();

	/**
	 * Rows which are subject rows, that is: collected without an association, the starting points
	 * of the collection. Only a row whose verdict in {@link #rowOriginMembership} is "true" can be
	 * in here.
	 */
	private final Set<String> rowOriginSubjectRows = new HashSet<String>();

	/**
	 * Rows which have been handed to the background scan already. Without this latch every
	 * repaint would ask again.
	 */
	private final Set<String> rowOriginRequested = new HashSet<String>();

	/**
	 * Rows which have become visible and are waiting for the next scan.
	 */
	private final Set<Row> pendingRowOriginRows = new LinkedHashSet<Row>();

	/**
	 * Cancellation contexts of the scans which are queued or in flight, so that they can be
	 * cancelled in bulk when the rows are reloaded or the browser is closed.
	 */
	private final List<Object> pendingRowOriginContexts = Collections.synchronizedList(new ArrayList<Object>());

	/**
	 * Collects the rows of one burst of scrolling into a single query.
	 */
	private Timer rowOriginTimer;

	/**
	 * The context the current verdicts belong to. If another run keeps the rows, they are void.
	 */
	private RowOriginContext lastRowOriginContext;

	/**
	 * Id of the graph the current verdicts belong to. The context alone is not enough to tell:
	 * it is mutable and outlives a change of the graph it points at, see
	 * {@link RowOriginContext#setGraphId(int)}.
	 */
	private int lastRowOriginGraphId = -1;

	/**
	 * The context a failure of the membership scan has already been reported for, so that it is
	 * said once and not with every burst of scrolling.
	 */
	private RowOriginContext rowOriginScanFailureReported;

	/**
	 * Maximum number of rows asked for in one statement.
	 */
	private static final int MAX_ROW_ORIGIN_CHUNK = 100;

	/**
	 * Constructor.
	 *
	 * @param host the table browser the verdicts are for
	 */
	RowOriginMembership(Host host) {
		this.host = host;
	}

	/**
	 * Gets the context whose collected rows the marks refer to, dropping the verdicts of a
	 * previous run. Called from both the rows table and the single row view, since only one of
	 * the two is painted at a time.
	 *
	 * @return the context, or <code>null</code> if nothing is marked here
	 */
	RowOriginContext currentRowOriginContext() {
		RowOriginContext rowOriginContext = host.rowOriginContextForTable();
		// object and id together: another run brings another object, and a graph exchanged on the
		// same object brings another id. The identity check has to stay, since two runs can well
		// end up with the same id - see EntityGraph.createUniqueGraphID
		int graphId = rowOriginContext == null? -1 : rowOriginContext.getGraphId();
		if (rowOriginContext != lastRowOriginContext || graphId != lastRowOriginGraphId) {
			resetRowOriginMembership();
			lastRowOriginContext = rowOriginContext;
			lastRowOriginGraphId = graphId;
		}
		return rowOriginContext;
	}

	/**
	 * Gets the verdict for a row, without asking for it.
	 *
	 * @param row the row
	 * @return whether the row is part of the subset, or <code>null</code> if that is not known yet
	 */
	Boolean getVerdict(Row row) {
		return rowOriginMembership.get(row.nonEmptyRowId);
	}

	/**
	 * Whether a row is known to be a subject row. Does not check {@link #currentRowOriginContext()}.
	 *
	 * @param row the row
	 * @return <code>true</code> if the row is known to be a subject row
	 */
	boolean isSubject(Row row) {
		return rowOriginSubjectRows.contains(row.nonEmptyRowId);
	}

	/**
	 * Whether it is already known that a row is not part of the subset of the run which keeps its
	 * collected rows.
	 * <p>
	 * Only a verdict which has already been read for the marker at the left edge counts. A row
	 * which has not been checked yet is not "known not to be in the subset", so the question stays
	 * open and the menu items stay enabled. Nothing is asked here: a popup has to be built at once,
	 * and an answer arriving later would come too late anyway.
	 *
	 * @param row the row
	 * @return <code>true</code> if the row is known not to belong to the subset
	 */
	boolean isKnownNotInSubset(Row row) {
		if (row == null || row.rowId == null || row.rowId.length() == 0) {
			return false;
		}
		// not rowOriginContextForTable: only this one drops the verdicts of a previous run, so
		// that nothing is disabled because of what an earlier graph said
		if (currentRowOriginContext() == null) {
			return false;
		}
		return Boolean.FALSE.equals(rowOriginMembership.get(row.nonEmptyRowId));
	}

	/**
	 * Whether it is already known that a row is a subject row, that is: the starting point of the
	 * collection rather than something collected through an association.
	 * <p>
	 * There is no path to open for such a row - the chain consists of the row itself. As with
	 * {@link #isKnownNotInSubset(Row)} only a verdict already read for the marker counts.
	 *
	 * @param row the row
	 * @return <code>true</code> if the row is known to be a subject row
	 */
	boolean isKnownSubjectRow(Row row) {
		if (row == null || row.rowId == null || row.rowId.length() == 0) {
			return false;
		}
		if (currentRowOriginContext() == null) {
			return false;
		}
		return rowOriginSubjectRows.contains(row.nonEmptyRowId);
	}

	/**
	 * Notes that a row has become visible and its membership is not known yet.
	 *
	 * @param row the row
	 */
	void requestRowOriginMembership(Row row) {
		if (row.rowId == null || row.rowId.isEmpty()) {
			return;
		}
		if (!rowOriginRequested.add(row.nonEmptyRowId)) {
			return;
		}
		pendingRowOriginRows.add(row);
		final Timer newTimer = new Timer(150, null);
		rowOriginTimer = newTimer;
		newTimer.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (newTimer == rowOriginTimer) {
					scanRowOriginMembership();
				}
			}
		});
		newTimer.setRepeats(false);
		newTimer.start();
	}

	/**
	 * Asks the retained entity-graph which of the rows collected so far are part of the subset.
	 * One statement per chunk, off the event dispatch thread.
	 */
	private void scanRowOriginMembership() {
		final RowOriginContext context = host.rowOriginContextForTable();
		if (context == null) {
			pendingRowOriginRows.clear();
			return;
		}
		final Table originTable = context.getDataModel().getTable(host.getTableName());
		if (originTable == null) {
			pendingRowOriginRows.clear();
			return;
		}
		// the graph this scan asks; if it is exchanged while the query runs, its answers are void
		final int scanGraphId = context.getGraphId();
		List<Row> pending = new ArrayList<Row>(pendingRowOriginRows);
		pendingRowOriginRows.clear();
		for (int from = 0; from < pending.size(); from += MAX_ROW_ORIGIN_CHUNK) {
			final List<Row> chunk = new ArrayList<Row>(pending.subList(from, Math.min(from + MAX_ROW_ORIGIN_CHUNK, pending.size())));
			final List<String> conditions = new ArrayList<String>(chunk.size());
			for (Row row: chunk) {
				conditions.add(row.rowId);
			}
			final Object scanContext = new Object();
			pendingRowOriginContexts.add(scanContext);
			MDSchema.loadMetaData(new Runnable() {
				@Override
				public void run() {
					final Set<Integer> members = new HashSet<Integer>();
					final Set<Integer> subjects = new HashSet<Integer>();
					try {
						context.getEntityGraph().readMembership(originTable, "B", conditions, scanContext,
								new Session.AbstractResultSetReader() {
							@Override
							public void readCurrentRow(ResultSet resultSet) throws SQLException {
								int index = resultSet.getInt(1);
								members.add(index);
								// no association means: collected as a subject row. Asked through
								// wasNull right after reading the column, not by comparing with 0
								resultSet.getInt(2);
								if (resultSet.wasNull()) {
									subjects.add(index);
								}
							}
						});
						UIUtil.invokeLater(new Runnable() {
							@Override
							public void run() {
								pendingRowOriginContexts.remove(scanContext);
								// asked against the context's current id, not against
								// lastRowOriginGraphId, so that this does not depend on whether
								// anything has been repainted in between
								if (lastRowOriginContext != context || context.getGraphId() != scanGraphId) {
									// another run keeps the rows now, or the graph has been
									// exchanged: the verdicts are void
									return;
								}
								for (int i = 0; i < chunk.size(); ++i) {
									String rowId = chunk.get(i).nonEmptyRowId;
									rowOriginMembership.put(rowId, members.contains(i));
									// removed as well, so that nothing of an earlier run remains
									if (subjects.contains(i)) {
										rowOriginSubjectRows.add(rowId);
									} else {
										rowOriginSubjectRows.remove(rowId);
									}
								}
								host.membershipChanged();
							}
						});
					} catch (CancellationException ce) {
						// reloaded or closed in the meantime: no verdict
						pendingRowOriginContexts.remove(scanContext);
					} catch (final Throwable t) {
						// the graph may be gone, or the statement too big for this DBMS
						LogUtil.warn(t);
						pendingRowOriginContexts.remove(scanContext);
						// said once per run, then silence: swallowing this completely is what let a
						// mismatching universal primary key go unnoticed - the marks simply stayed
						// away, with nothing to go on
						UIUtil.invokeLater(new Runnable() {
							@Override
							public void run() {
								if (lastRowOriginContext == context && rowOriginScanFailureReported != context) {
									rowOriginScanFailureReported = context;
									UIUtil.showException(host.getComponent(),
											"The rows of the subset could not be determined. The marks stay away.", t);
								}
							}
						});
					} finally {
						CancellationHandler.reset(scanContext);
					}
				}
			}, 1);
		}
	}

	/**
	 * Forgets all verdicts and stops the scans which are still on their way. The rows they refer
	 * to are gone, or they belong to another run.
	 */
	void resetRowOriginMembership() {
		rowOriginMembership.clear();
		rowOriginSubjectRows.clear();
		rowOriginRequested.clear();
		pendingRowOriginRows.clear();
		rowOriginTimer = null;
		synchronized (pendingRowOriginContexts) {
			for (Object context: pendingRowOriginContexts) {
				try {
					CancellationHandler.cancelSilently(context);
				} catch (Throwable t) {
					// ignore
				}
			}
			pendingRowOriginContexts.clear();
		}
	}

}
