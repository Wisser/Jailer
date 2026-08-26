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
package net.sf.jailer.ui.associationdiscovery;

/**
 * Sink for the statements and problems of an association discovery run,
 * and source of the cancellation state.
 *
 * @author Ralf Wisser
 */
public interface DiscoveryLog {

	/**
	 * A statement has been executed.
	 *
	 * @param sql the statement
	 */
	void statement(String sql);

	/**
	 * A problem occurred. Discovery continues.
	 *
	 * @param message the message
	 */
	void problem(String message);

	/**
	 * Reports the progress of the current phase.
	 *
	 * @param done number of steps done
	 * @param total number of steps of this phase
	 */
	void progress(int done, int total);

	/**
	 * @return <code>true</code> if the run has been cancelled
	 */
	boolean isCancelled();

}
