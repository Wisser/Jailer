/*
 * Copyright 2007 - 2023 Ralf Wisser.
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
package net.sf.jailer.util;

import java.sql.Statement;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.WeakHashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Handles cancellation request of the user and cancels database requests.
 * 
 * @author Ralf Wisser
 */
public class CancellationHandler {

	/**
	 * Contexts for which user have requested cancellation.
	 */
	private static Map<Object, Object> cancelled = new WeakHashMap<Object, Object>();
	
	/**
	 * List of all currently running statements per context.
	 */
	private static Map<Object, IdentityHashMap<Statement, Statement>> currentStatements = new WeakHashMap<Object, IdentityHashMap<Statement, Statement>>();

	/**
	 * The logger.
	 */
	private static final Logger _log = LoggerFactory.getLogger(JobManager.class);

	/**
	 * Default context
	 */
	private static final Object DEFAULT_CONTEXT = new Object();

	/**
	 * Resets the handler.
	 * 
	 * @param context cancellation context, <code>null</code> for default context
	 */
	public static synchronized void reset(Object context) {
		cancelled.remove(context == null? DEFAULT_CONTEXT : context);
		currentStatements.remove(context == null? DEFAULT_CONTEXT : context);
	}

	/**
	 * Requests cancellation.
	 * 
	 * @param context cancellation context, <code>null</code> for default context
	 */
	public static void cancel(Object context) {
		_log.warn("cancellation request received");
		synchronizedCancel(context);
	}

	/**
	 * Requests cancellation without logging.
	 * 
	 * @param context cancellation context, <code>null</code> for default context
	 */
	public static void cancelSilently(Object context) {
		synchronizedCancel(context);
	}

	/**
	 * Requests cancellation.
	 * 
	 * @param context cancellation context, <code>null</code> for default context
	 */
	private static synchronized void synchronizedCancel(Object context) {
		cancelled.put(context == null? DEFAULT_CONTEXT : context, context == null? DEFAULT_CONTEXT : context);
		if (currentStatements.containsKey(context == null? DEFAULT_CONTEXT : context)) {
			final IdentityHashMap<Statement, Statement> toBeCanceled = new IdentityHashMap<Statement, Statement>(currentStatements.get(context == null? DEFAULT_CONTEXT : context));
			currentStatements.remove(context == null? DEFAULT_CONTEXT : context);
			for (final IdentityHashMap.Entry<Statement, Statement> statement: toBeCanceled.entrySet()) {
				Thread thread = new Thread(new Runnable() {
					@Override
					public void run() {
						try {
							statement.getKey().cancel();
						} catch (Exception e) {
							// ignore
						}
					}
				}, "Cancel-Statement-" + (count++));
				thread.setDaemon(true);
				thread.start();
			}
		}
	}

	private static int count = 0;
	
	/**
	 * Indicates that a statement is going to be executed.
	 * 
	 * @param context cancellation context, <code>null</code> for default context
	 * @param statement the statement
	 */
	public static synchronized void begin(Statement statement, Object context) {
		checkForCancellation(context);
		IdentityHashMap<Statement, Statement> sl = currentStatements.get(context == null? DEFAULT_CONTEXT : context);
		if (sl == null) {
			sl = new IdentityHashMap<Statement, Statement>();
			currentStatements.put(context == null? DEFAULT_CONTEXT : context, sl);
		}
		sl.put(statement, statement);
	}
	
	/**
	 * Indicates that a statement has been executed.
	 * 
	 * @param statement the statement
	 * @param context cancellation context, <code>null</code> for default context
	 */
	public static synchronized void end(Statement statement, Object context) {
		if (currentStatements.containsKey(context == null? DEFAULT_CONTEXT : context)) {
			currentStatements.get(context == null? DEFAULT_CONTEXT : context).remove(statement);
		}
	}
	
	/**
	 * Checks for cancellation.
	 * 
	 * @param context cancellation context, <code>null</code> for default context
	 * @throws CancellationException if cancellation have been requested
	 */
	public static synchronized void checkForCancellation(Object context) throws CancellationException {
		if (cancelled.containsKey(context == null? DEFAULT_CONTEXT : context)) {
			throw new CancellationException();
		}
	}
	
}
