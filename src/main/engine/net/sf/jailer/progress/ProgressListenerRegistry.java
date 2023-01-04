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
package net.sf.jailer.progress;

import java.util.ArrayList;
import java.util.List;

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

	public synchronized void fireCollectionJobEnqueued(int day, ModelElement modelElement) {
		for (ProgressListener listener : progressListeners) {
			listener.collectionJobEnqueued(day, modelElement);
		}
	}

	public synchronized void fireCollectionJobStarted(int day, ModelElement modelElement) {
		for (ProgressListener listener : progressListeners) {
			listener.collectionJobStarted(day, modelElement);
		}
	}

	public synchronized void fireCollected(int day, ModelElement modelElement, long numberOfRows) {
		for (ProgressListener listener : progressListeners) {
			listener.collected(day, modelElement, numberOfRows);
		}
	}

	public synchronized void fireExported(Table table, long rc) {
		for (ProgressListener listener : progressListeners) {
			listener.exported(table, rc);
		}
	}

	public synchronized void fireNewStage(String stage, boolean isErrorStage, boolean isFinalStage) {
		for (ProgressListener listener : progressListeners) {
			listener.newStage(stage, isErrorStage, isFinalStage);
		}
	}

	public synchronized void firePrepareExport() throws CancellationException {
		for (ProgressListener listener : progressListeners) {
			listener.prepareExport();
		}
	}

}
