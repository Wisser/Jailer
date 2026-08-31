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

import java.awt.Window;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.Callable;

import javax.swing.SwingUtilities;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.database.BasicDataSource;
import net.sf.jailer.database.WorkingTableScope;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.util.ConcurrentTaskControl;
import net.sf.jailer.ui.util.UISettings;
import net.sf.jailer.util.CancellationException;
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
	 * Registers a newly retained graph. The caller is expected to have discarded a previously
	 * retained one via {@link #discardCurrent(Window)} beforehand, so that the user sees what
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
	 * Discards the graph currently retained by this application, showing a dialog which can be
	 * cancelled.
	 *
	 * @param window the owner of that dialog
	 * @return <code>true</code> if there was nothing left to discard afterwards
	 */
	public static boolean discardCurrent(Window window) {
		final RowOriginContext context;
		synchronized (RetainedEntityGraphs.class) {
			context = current;
		}
		if (context == null || !context.isAvailable()) {
			return true;
		}
		try {
			ConcurrentTaskControl.call(window, new Callable<Object>() {
				@Override
				public Object call() throws Exception {
					context.discard();
					return null;
				}
			}, "Discarding analysis data...", null);
		} catch (CancellationException e) {
			return false;
		} catch (Throwable t) {
			LogUtil.warn(t);
			UIUtil.showException(window, "Error", t);
			return false;
		}
		synchronized (RetainedEntityGraphs.class) {
			if (current == context) {
				current = null;
			}
		}
		forget(context.getDbUrl());
		return true;
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
	 * Discards the current graph on the event dispatch thread, for use as a menu action.
	 *
	 * @param window the owner of the dialog
	 * @return a runnable which discards the current graph
	 */
	public static Runnable discardAction(final Window window) {
		return new Runnable() {
			@Override
			public void run() {
				if (SwingUtilities.isEventDispatchThread()) {
					discardCurrent(window);
				} else {
					UIUtil.invokeLater(new Runnable() {
						@Override
						public void run() {
							discardCurrent(window);
						}
					});
				}
			}
		};
	}

}
