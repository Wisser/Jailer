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
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;

import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.entitygraph.RowOrigin;
import net.sf.jailer.entitygraph.RowOriginFinder;
import net.sf.jailer.entitygraph.RowOriginStep;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.progress.RowOriginContext;
import net.sf.jailer.ui.progress.RowOriginPath;
import net.sf.jailer.ui.progress.RowOriginWindow;
import net.sf.jailer.ui.util.ConcurrentTaskControl;
import net.sf.jailer.util.CancellationException;
import net.sf.jailer.util.CellContentConverter;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlUtil;

/**
 * Opens the views which show how rows of a table browser have found their way into the subset:
 * the window "Why is this Row in the Subset?" and the chains of table browsers of
 * "Open Path to Subject".
 *
 * @author Ralf Wisser
 */
class RowOriginOpener {

	/**
	 * The table browser the rows are in.
	 */
	interface Host {

		/**
		 * @return the frame the windows belong to
		 */
		JFrame getOwner();

		/**
		 * @return the component messages are shown on
		 */
		Component getComponent();

		/**
		 * @return the name of the table of the browser
		 */
		String getTableName();

		/**
		 * @return the display name of the table of the browser
		 */
		String getDisplayName();

		/**
		 * @return the session of the browser
		 */
		Session getSession();

		/**
		 * Lays out the ways of rows into the subset as chains of table browsers.
		 *
		 * @param paths the paths, each subject first
		 */
		void openRowOriginPaths(List<List<RowOriginPath.Step>> paths);
	}

	private final Host host;

	/**
	 * Constructor.
	 *
	 * @param host the table browser the rows are in
	 */
	RowOriginOpener(Host host) {
		this.host = host;
	}

	/**
	 * Opens the window which shows how a row has found its way into the subset.
	 *
	 * @param row the row
	 * @param context the context of the run which has kept the collected rows
	 */
	void openRowOrigin(final Row row, final RowOriginContext context) {
		final Table originTable = context.getDataModel().getTable(host.getTableName());
		if (originTable == null) {
			return;
		}
		RowOriginWindow.open(host.getOwner(), context, originTable, new Callable<Object[]>() {
			@Override
			public Object[] call() throws SQLException {
				return readRowOriginKey(row, originTable, context);
			}
		}, host.getDisplayName() + "(" + SqlUtil.replaceAliases(row.rowId, null, null) + ")",
		new Consumer<List<RowOriginStep>>() {
			@Override
			public void accept(List<RowOriginStep> steps) {
				List<RowOriginPath.Step> path = RowOriginPath.build(host.getOwner(), context, steps);
				if (path != null) {
					host.openRowOriginPaths(Collections.singletonList(path));
				}
			}
		});
	}

	/**
	 * Opens the ways rows have taken into the subset as chains of table browsers.
	 * <p>
	 * Reading the keys, following the chains and describing them happen in one background run: one
	 * window to wait at, one place to cancel, and every row looked up only once. What is to be said
	 * afterwards - rows which are not part of the subset, chains which do not reach a subject - is
	 * gathered and said in a single message instead of one per row.
	 *
	 * @param theRows the rows
	 * @param context the context of the run which has kept the collected rows
	 */
	void openRowOriginPathFor(final Collection<Row> theRows, final RowOriginContext context) {
		final Table originTable = context.getDataModel().getTable(host.getTableName());
		if (originTable == null || theRows.isEmpty()) {
			return;
		}
		final List<String> notes = new ArrayList<String>();
		final int[] notFound = new int[1];
		final int[] notInSubset = new int[1];
		final AtomicReference<JLabel> infoLabel = new AtomicReference<JLabel>();
		// the widest wording the counter can take, so that the dialog is packed for it: it does not
		// grow with the text afterwards. Nobody sees it - the first real message is set before the
		// dialog has faded in, which takes about 400 ms
		String info = theRows.size() == 1? ROW_ORIGIN_ANALYZING : rowOriginProgress(theRows.size(), theRows.size());
		List<List<RowOriginPath.Step>> paths;
		try {
			paths = ConcurrentTaskControl.call(host.getOwner(), new Callable<List<List<RowOriginPath.Step>>>() {
				@Override
				public List<List<RowOriginPath.Step>> call() throws Exception {
					List<List<RowOriginPath.Step>> result = new ArrayList<List<RowOriginPath.Step>>();
					// one finder for all of them: each one asks the graph once for the birthday of
					// the subject rows
					RowOriginFinder finder = context.createFinder();
					int done = 0;
					for (Row row: theRows) {
						if (theRows.size() > 1) {
							showProgress(infoLabel, rowOriginProgress(++done, theRows.size()));
						}
						Object[] primaryKey = readRowOriginKey(row, originTable, context);
						if (primaryKey == null) {
							++notFound[0];
							continue;
						}
						RowOrigin origin = finder.find(originTable, primaryKey);
						if (origin.getSteps().isEmpty()) {
							++notInSubset[0];
							continue;
						}
						String note = rowOriginPathNote(origin);
						if (note != null && !notes.contains(note)) {
							notes.add(note);
						}
						result.add(RowOriginPath.describe(context, origin.getSteps()));
					}
					return result;
				}
			}, info, UIUtil.blinkingInfoLabel(infoLabel));
		} catch (CancellationException e) {
			return;
		} catch (Throwable t) {
			UIUtil.showException(host.getComponent(), "Error", t);
			return;
		}
		if (paths == null || paths.isEmpty()) {
			JOptionPane.showMessageDialog(host.getComponent(), nothingToShowMessage(theRows.size(), notFound[0]),
					BrowserContentPane.ROW_ORIGIN_PATH_TITLE, JOptionPane.INFORMATION_MESSAGE);
			return;
		}
		String message = leftOutMessage(notFound[0], notInSubset[0]);
		for (String note: notes) {
			message = message.isEmpty()? note : message + "\n" + note;
		}
		if (!message.isEmpty()) {
			JOptionPane.showMessageDialog(host.getComponent(), message, BrowserContentPane.ROW_ORIGIN_PATH_TITLE, JOptionPane.INFORMATION_MESSAGE);
		}
		host.openRowOriginPaths(paths);
	}

	private static final String ROW_ORIGIN_ANALYZING = "Analyzing origin...";

	/**
	 * The wording of the progress while the ways of several rows are being analyzed.
	 *
	 * @param done number of the row in hand
	 * @param numberOfRows number of rows altogether
	 * @return the text
	 */
	private static String rowOriginProgress(int done, int numberOfRows) {
		return ROW_ORIGIN_ANALYZING + " " + done + " of " + numberOfRows;
	}

	/**
	 * Writes a progress text into the info label of a running {@link ConcurrentTaskControl}, from
	 * whatever thread the task runs on.
	 *
	 * @param infoLabel the label, filled in by {@link UIUtil#blinkingInfoLabel(AtomicReference)}
	 * @param text the text
	 */
	private static void showProgress(final AtomicReference<JLabel> infoLabel, final String text) {
		UIUtil.invokeLater(new Runnable() {
			@Override
			public void run() {
				JLabel label = infoLabel.get();
				if (label != null) {
					label.setText(text);
				}
			}
		});
	}

	/**
	 * The message for the case that not a single way could be laid out.
	 *
	 * @param numberOfRows number of rows asked about
	 * @param notFound number of rows which could not be found
	 * @return the message
	 */
	private String nothingToShowMessage(int numberOfRows, int notFound) {
		if (numberOfRows == 1) {
			return notFound > 0?
					"The row could not be found." :
					"This row is not part of the subset of the last export.";
		}
		if (notFound >= numberOfRows) {
			return "None of the " + numberOfRows + " rows could be found.";
		}
		return "None of the " + numberOfRows + " rows is part of the subset of the last export.";
	}

	/**
	 * What is to be said about the rows which have been left out, or an empty text if there are
	 * none. Only the rows which are left out are worth a word; that the others are being laid out
	 * is about to be seen anyway.
	 *
	 * @param notFound number of rows which could not be found
	 * @param notInSubset number of rows which are not part of the subset
	 * @return the message, possibly empty
	 */
	private String leftOutMessage(int notFound, int notInSubset) {
		String reason = "";
		if (notInSubset > 0) {
			reason = notInSubset + (notInSubset == 1? " is" : " are") + " not part of the subset of the last export";
		}
		if (notFound > 0) {
			reason += (reason.isEmpty()? "" : ", ") + notFound + " could not be found";
		}
		if (reason.isEmpty()) {
			return "";
		}
		int left = notFound + notInSubset;
		return (left == 1? "One row has" : left + " rows have") + " been left out: " + reason + ".";
	}

	/**
	 * Tells what is to be said about a chain before it is laid out, or <code>null</code> if it is
	 * complete. Only an incomplete chain is reported: laid out on the desktop there is no status
	 * line to put it in, and that the chain does not reach the subject has to be known before one
	 * reads the browsers.
	 * <p>
	 * That the way is not unique is <b>not</b> reported here. It is the normal case rather than the
	 * exception, and a window one has to click away before anything is visible is out of all
	 * proportion to it. The chain view says it instead, in its status line and per step in the
	 * column "Via Association".
	 *
	 * @param origin the chain
	 * @return the note, or <code>null</code>
	 */
	private String rowOriginPathNote(RowOrigin origin) {
		if (origin == null) {
			return null;
		}
		if (origin.getStatus() == RowOrigin.Status.BROKEN) {
			return "The chain could not be followed up to the subject. Only the part which is still known is shown.";
		}
		if (origin.getStatus() == RowOrigin.Status.TRUNCATED) {
			return "The chain is too long to be followed completely. Only its last steps are shown.";
		}
		return null;
	}

	/**
	 * Reads the primary key values of a row the way the export run has identified it.
	 * <p>
	 * They cannot be taken from the row itself: {@link Row#primaryKey} holds SQL literals, not
	 * values, and this browser identifies a row with its own settings, which need not be the
	 * ones of the run - a run which uses rowids has another key than the browser has. So the key
	 * columns of the run are read once, with the condition of the row.
	 *
	 * @param row the row
	 * @param originTable the table of the row, as of the data model of the run
	 * @param context the context of the run
	 * @return the primary key values, in the order of the key of the run, or <code>null</code>
	 *         if the row does not exist any more
	 */
	private Object[] readRowOriginKey(Row row, Table originTable, RowOriginContext context) throws SQLException {
		final Session session = host.getSession();
		List<Column> pkColumns = context.getRowIdSupport().getPrimaryKey(originTable).getColumns();
		if (pkColumns.isEmpty()) {
			return null;
		}
		Quoting quoting = Quoting.getQuoting(session);
		StringBuilder selectList = new StringBuilder();
		for (int i = 0; i < pkColumns.size(); ++i) {
			if (i > 0) {
				selectList.append(", ");
			}
			selectList.append("B." + quoting.requote(pkColumns.get(i).name) + " as PK" + i);
		}
		// Row.rowId is a condition on the alias "B", see reloadRows0
		String sql = "Select " + selectList + " From " + BrowserContentPane.qualifiedTableName(originTable, quoting) + " B Where (" + row.rowId + ")";
		final Object[] result = new Object[pkColumns.size()];
		final boolean[] found = new boolean[1];
		session.executeQuery(sql, new Session.AbstractResultSetReader() {
			@Override
			public void readCurrentRow(ResultSet resultSet) throws SQLException {
				if (found[0]) {
					return;
				}
				CellContentConverter cellContentConverter = new CellContentConverter(getMetaData(resultSet), session, session.dbms);
				for (int i = 0; i < result.length; ++i) {
					result[i] = cellContentConverter.getObject(resultSet, "PK" + i);
				}
				found[0] = true;
			}
		}, null, null, 1);
		return found[0]? result : null;
	}

}
