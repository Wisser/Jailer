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

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.Callable;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.database.BasicDataSource;
import net.sf.jailer.database.Session;
import net.sf.jailer.database.WorkingTableScope;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.ui.util.UISettings;
import net.sf.jailer.util.LogUtil;

/**
 * Keeps track of the entity-graph that has been retained for a row origin analysis, and gets rid
 * of it again.
 * <p>
 * The database itself says nothing about the age or the owner of an entity-graph, so the
 * application that has created one has to remember it. Four occasions delete it: the progress
 * window is closed, the next run with retention starts, the application ends (shutdown hook),
 * and the next connection to the same database is made. The last one is the only one which
 * survives a crash; for it the ID is written to the UI settings.
 * <p>
 * The rows are needed by whoever looks at them, and that is not only the progress window: a
 * row origin can also be asked for from the Data Browser. Views therefore announce themselves
 * with {@link #addUser()} and {@link #removeUser()}, and closing the progress window
 * discards through {@link #discardWhenUnused()}, which waits for the last view to close.
 *
 * @author Ralf Wisser
 */
public class RetainedEntityGraphs {

	/**
	 * Name of the property in the UI settings.
	 */
	private static final String SETTING = "retainedEntityGraphs";

	/**
	 * Identifies this run of the application, so that leftovers of an earlier run can be told
	 * from the graph this run is currently using.
	 */
	private static final String SESSION_ID = UUID.randomUUID().toString();

	/**
	 * The graph currently retained by this application, at most one.
	 */
	private static RowOriginContext current;

	private static boolean shutdownHookInstalled = false;

	/**
	 * Number of views which are currently analyzing the retained graph. As long as at least one
	 * of them is open, the rows are not discarded.
	 */
	private static int users = 0;

	/**
	 * Set if a discard has been asked for while views were still open. The last view which is
	 * closed carries it out.
	 */
	private static boolean discardRequested = false;

	/**
	 * Registers a newly retained graph. The caller is expected to have discarded a previously
	 * retained one via {@link #discardCurrent()} beforehand, so that the user sees what
	 * happens and can cancel it.
	 *
	 * @param context the context of the new graph
	 */
	public static synchronized void register(RowOriginContext context) {
		current = context;
		installShutdownHook();
	}

	/**
	 * Gets the graph currently retained by this application.
	 *
	 * @return the context, or <code>null</code>
	 */
	public static synchronized RowOriginContext getCurrent() {
		return current;
	}

	/**
	 * Gets the graph retained for a given database, if there is anything to analyze in it. This
	 * is what a second entry point, for instance the Data Browser, has to ask: it knows the
	 * database it is connected to and nothing else.
	 *
	 * @param dbUrl URL of the database
	 * @return the context, or <code>null</code> if that database has no graph to analyze
	 */
	public static synchronized RowOriginContext getCurrentFor(String dbUrl) {
		if (current == null || dbUrl == null || !current.isAvailable()) {
			return null;
		}
		return dbUrl.equals(current.getDbUrl())? current : null;
	}

	/**
	 * Announces a view which analyzes the retained graph. Until it is gone again, see
	 * {@link #removeUser()}, {@link #discardWhenUnused()} keeps the rows.
	 */
	public static synchronized void addUser() {
		++users;
	}

	/**
	 * Announces that a view has been closed. If it was the last one and a discard has been
	 * asked for in the meantime, it is carried out now.
	 */
	public static void removeUser() {
		boolean discardNow;
		synchronized (RetainedEntityGraphs.class) {
			if (users > 0) {
				--users;
			}
			discardNow = users == 0 && discardRequested;
		}
		if (discardNow) {
			discardCurrent();
		}
	}

	/**
	 * Discards the retained graph unless a view is still analyzing it. In that case the discard
	 * is remembered and carried out as soon as the last view is closed.
	 */
	public static void discardWhenUnused() {
		synchronized (RetainedEntityGraphs.class) {
			if (users > 0 && current != null && current.isAvailable()) {
				discardRequested = true;
				return;
			}
		}
		discardCurrent();
	}

	/**
	 * Remembers a retained graph, so that it can be cleaned up after a crash.
	 *
	 * @param dbUrl URL of the database
	 * @param workingTableSchema the working table schema, or <code>null</code>
	 * @param graphId ID of the graph
	 */
	public static synchronized void remember(String dbUrl, String workingTableSchema, int graphId) {
		try {
			Map<String, String> retained = load();
			retained.put(dbUrl, SESSION_ID + "\t" + (workingTableSchema == null? "" : workingTableSchema) + "\t" + graphId);
			UISettings.store(SETTING, new HashMap<String, String>(retained));
		} catch (Throwable t) {
			LogUtil.warn(t);
		}
	}

	/**
	 * Forgets a graph which has been discarded.
	 *
	 * @param dbUrl URL of the database
	 */
	public static synchronized void forget(String dbUrl) {
		try {
			Map<String, String> retained = load();
			if (retained.remove(dbUrl) != null) {
				UISettings.store(SETTING, new HashMap<String, String>(retained));
			}
		} catch (Throwable t) {
			LogUtil.warn(t);
		}
	}

	/**
	 * Discards the graph currently retained by this application, in the background.
	 * <p>
	 * Deleting the rows out of the working tables can take a while, and there is nothing to decide
	 * while it runs, so nobody is kept waiting for it: it happens on a thread of its own, and the
	 * caller carries on at once.
	 */
	public static void discardCurrent() {
		final RowOriginContext context;
		synchronized (RetainedEntityGraphs.class) {
			context = current;
			discardRequested = false;
			if (context == null || !context.isAvailable()) {
				return;
			}
			// out of the way before the deleting begins: the next run may register its graph while
			// this one is still going, and a second discard must not start on the same context.
			// The two do not get in each other's way, deleting goes by the graph id
			current = null;
		}
		Thread thread = new Thread(new Runnable() {
			@Override
			public void run() {
				try {
					// as ConcurrentTaskControl does for its own worker: the session is not this
					// thread's own, it is shared with whoever else is looking at that graph
					Session.setThreadSharesConnection();
					context.discard();
					// only now: what could not be deleted stays in the list and is cleaned up on
					// the next connection to that database
					forget(context.getDbUrl());
				} catch (Throwable t) {
					LogUtil.warn(t);
				}
			}
		}, "discard-retained-entity-graph");
		thread.setDaemon(true);
		thread.start();
	}

	/**
	 * Discards graphs of a database which have been retained by an earlier run of the
	 * application. Runs in the background and never disturbs the caller.
	 *
	 * @param dataModel the data model
	 * @param dbUrl URL of the database
	 * @param dataSourceFactory creates a data source for that database
	 */
	public static void discardLeftovers(final DataModel dataModel, final String dbUrl,
			final Callable<BasicDataSource> dataSourceFactory) {
		final String entry;
		synchronized (RetainedEntityGraphs.class) {
			entry = load().get(dbUrl);
		}
		if (entry == null) {
			return;
		}
		String[] parts = entry.split("\t", -1);
		if (parts.length != 3 || SESSION_ID.equals(parts[0])) {
			// belongs to this run of the application, it is still in use
			return;
		}
		final String workingTableSchema = parts[1].length() == 0? null : parts[1];
		final int graphId;
		try {
			graphId = Integer.parseInt(parts[2]);
		} catch (NumberFormatException e) {
			forget(dbUrl);
			return;
		}
		Thread thread = new Thread(new Runnable() {
			@Override
			public void run() {
				try {
					Session.setThreadSharesConnection();
					ExecutionContext executionContext = new ExecutionContext();
					executionContext.setScope(WorkingTableScope.GLOBAL);
					executionContext.setWorkingTableSchema(workingTableSchema);
					RowOriginContext context = new RowOriginContext(dataModel, executionContext, dataSourceFactory, null, dbUrl);
					context.setGraphId(graphId);
					context.discard();
				} catch (Throwable t) {
					// the graph may be gone already, or the working tables may have been recreated
					LogUtil.warn(t);
				} finally {
					forget(dbUrl);
				}
			}
		}, "discard-retained-entity-graph");
		thread.setDaemon(true);
		thread.start();
	}

	/**
	 * Discards the current graph without a dialog and without complaining. Used when a new run
	 * replaces it and by the shutdown hook.
	 */
	private static synchronized void discardCurrentSilently() {
		discardRequested = false;
		if (current != null) {
			String dbUrl = current.getDbUrl();
			try {
				current.discard();
			} catch (Throwable t) {
				LogUtil.warn(t);
			}
			current = null;
			forget(dbUrl);
		}
	}

	/**
	 * The shutdown hook is the last resort. It must not hold up the JVM, so it gets a short
	 * time limit and stays silent; what it does not manage is cleaned up on the next connection
	 * to that database.
	 */
	private static synchronized void installShutdownHook() {
		if (shutdownHookInstalled) {
			return;
		}
		shutdownHookInstalled = true;
		Runtime.getRuntime().addShutdownHook(new Thread(new Runnable() {
			@Override
			public void run() {
				Thread worker = new Thread(new Runnable() {
					@Override
					public void run() {
						Session.setThreadSharesConnection();
						discardCurrentSilently();
					}
				}, "discard-retained-entity-graph-on-exit");
				worker.setDaemon(true);
				worker.start();
				try {
					worker.join(4000);
				} catch (InterruptedException e) {
					// ignore
				}
			}
		}, "jailer-shutdown-hook"));
	}

	@SuppressWarnings("unchecked")
	private static Map<String, String> load() {
		Object stored = UISettings.restore(SETTING);
		if (stored instanceof Map) {
			return new HashMap<String, String>((Map<String, String>) stored);
		}
		return new HashMap<String, String>();
	}

	/**
	 * Discards the current graph, for use as a menu action. Any thread may run it: the state is
	 * guarded, and the deleting happens in the background anyway.
	 *
	 * @return a runnable which discards the current graph
	 */
	public static Runnable discardAction() {
		return new Runnable() {
			@Override
			public void run() {
				discardCurrent();
			}
		};
	}

	/**
	 * Discards the current graph, but only if no view is analyzing it any more. For the window
	 * which owns the analysis, not for an explicit menu item.
	 *
	 * @return a runnable which discards the current graph as soon as it is unused
	 */
	public static Runnable discardWhenUnusedAction() {
		return new Runnable() {
			@Override
			public void run() {
				discardWhenUnused();
			}
		};
	}

}
