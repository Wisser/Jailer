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
package net.sf.jailer.progress;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;

import net.sf.jailer.datamodel.ModelElement;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.CancellationException;

/**
 * Registry for {@link ProgressListener}.
 *
 * @author Ralf Wisser
 */
public class ProgressListenerRegistry {

	private List<ProgressListener> progressListeners = new ArrayList<ProgressListener>();

	/**
	 * Adds a {@link ProgressListener}.
	 *
	 * @param theProgressListener
	 *            to add
	 */
	public synchronized void addProgressListener(ProgressListener theProgressListener) {
		progressListeners.add(theProgressListener);
	}

	/**
	 * Notifies all listeners that a collection-job has been enqueued.
	 *
	 * @param day the day
	 * @param modelElement the association or table to be resolved
	 */
	public synchronized void fireCollectionJobEnqueued(int day, ModelElement modelElement) {
		for (ProgressListener listener : progressListeners) {
			listener.collectionJobEnqueued(day, modelElement);
		}
	}

	/**
	 * Notifies all listeners that collection of rows has been started.
	 *
	 * @param day the day
	 * @param modelElement the association or table to be resolved
	 */
	public synchronized void fireCollectionJobStarted(int day, ModelElement modelElement) {
		for (ProgressListener listener : progressListeners) {
			listener.collectionJobStarted(day, modelElement);
		}
	}

	/**
	 * Notifies all listeners that rows have been collected.
	 *
	 * @param day the day
	 * @param modelElement the association or table which has been resolved
	 * @param numberOfRows the number of rows which have been collected
	 */
	public synchronized void fireCollected(int day, ModelElement modelElement, long numberOfRows) {
		for (ProgressListener listener : progressListeners) {
			listener.collected(day, modelElement, numberOfRows);
		}
	}

	/**
	 * Notifies all listeners that rows have been exported.
	 *
	 * @param table the table from which the rows have been exported
	 * @param rc the number of rows
	 */
	public synchronized void fireExported(Table table, long rc) {
		for (ProgressListener listener : progressListeners) {
			listener.exported(table, rc);
		}
	}

	/**
	 * Notifies all listeners that a new stage has begun.
	 *
	 * @param stage the stage
	 * @param isErrorStage <code>true</code> if the stage represents an error
	 * @param isFinalStage <code>true</code> if this is the final stage
	 */
	public synchronized void fireNewStage(String stage, boolean isErrorStage, boolean isFinalStage) {
		for (ProgressListener listener : progressListeners) {
			listener.newStage(stage, isErrorStage, isFinalStage);
		}
	}

	/**
	 * Notifies all listeners that export is ready. This might be cancelled.
	 *
	 * @throws CancellationException if a listener requests cancellation
	 */
	public synchronized void firePrepareExport() throws CancellationException {
		for (ProgressListener listener : progressListeners) {
			listener.prepareExport();
		}
	}

	/**
	 * Notifies all listeners that the user should be warned.
	 *
	 * @param msgSupplier supplier for the warning message
	 */
	public void warn(Supplier<String> msgSupplier) {
		for (ProgressListener listener : progressListeners) {
			listener.warn(msgSupplier);
		}
	}

}
