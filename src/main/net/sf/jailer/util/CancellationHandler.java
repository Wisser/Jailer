/*
 * Copyright 2007 the original author or authors.
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
import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;

/**
 * Handles cancellation request of the user and cancels database requests.
 * 
 * @author Ralf Wisser
 */
public class CancellationHandler {

	/**
	 * <code>true</code> if user has requested cancellation.
	 */
	private static boolean isCanceled = false;
	
	/**
	 * List of all currently running statements.
	 */
	private static List<Statement> currentStatements = new ArrayList<Statement>();
	
	/**
     * The logger.
     */
    private static final Logger _log = Logger.getLogger(JobManager.class);

	/**
	 * Resets the handler.
	 */
	public static synchronized void reset() {
		isCanceled = false;
		currentStatements.clear();
	}

	/**
	 * Requests cancellation.
	 */
	public static synchronized void cancel() {
		isCanceled = true;
		final List<Statement> toBeCanceled = new ArrayList<Statement>(currentStatements);
		currentStatements.clear();
		_log.warn("cancallation request received");
		new Thread(new Runnable() {
			@Override
			public void run() {
				for (Statement statement: toBeCanceled) {
					try {
						statement.cancel();
					} catch (Exception e) {
						// ignore
					}
				}
			}
		}).start();
	}

	/**
	 * Indicates that a statement is going to be executed.
	 * 
	 * @param statement the statement
	 */
	public static synchronized void begin(Statement statement) {
		checkForCancellation();
		currentStatements.add(statement);
	}
	
	/**
	 * Indicates that a statement has been executed.
	 * 
	 * @param statement the statement
	 */
	public static synchronized void end(Statement statement) {
		currentStatements.remove(statement);
	}
	
	/**
	 * Checks for cancellation.
	 * 
	 * @throws CancellationException if cancellation have been requested
	 */
	public static synchronized void checkForCancellation() throws CancellationException {
		if (isCanceled) {
			throw new CancellationException();
		}
	}
	
}
