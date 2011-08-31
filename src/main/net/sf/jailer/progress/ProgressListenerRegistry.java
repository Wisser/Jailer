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
package net.sf.jailer.progress;

import net.sf.jailer.datamodel.ModelElement;
import net.sf.jailer.datamodel.Table;

/**
 * Registry for {@link ProgressListener}.
 * 
 * @author Ralf Wisser
 */
public class ProgressListenerRegistry {
	
	/**
	 * Sets the {@link ProgressListener} for current thread.
	 * 
	 * @param theProgressListener to set
	 */
	public static synchronized void setProgressListener(ProgressListener theProgressListener) {
		progressListener.set(theProgressListener);
	}

	/**
	 * Gets the {@link ProgressListener} for current thread.
	 * 
	 * @return the progressListener
	 */
	public static synchronized ProgressListener getProgressListener() {
		ProgressListener theProgressListener = progressListener.get();
		if (theProgressListener == null) {
			return NULL_PROGRESS_LISTENER;
		}
		return theProgressListener;
	}

	/**
	 * Holds {@link ProgressListener}.
	 */
	private static InheritableThreadLocal<ProgressListener> progressListener = new InheritableThreadLocal<ProgressListener>();
	
	private static ProgressListener NULL_PROGRESS_LISTENER = new ProgressListener() {
		@Override
		public void collectionJobEnqueued(int day, ModelElement association) {
		}
		@Override
		public void collectionJobStarted(int day, ModelElement modelElement) {
		}
		@Override
		public void collected(int day, ModelElement association, long numberOfRows) {
		}
		@Override
		public void exported(Table table, long rc) {
		}
		@Override
		public void newStage(String stage, boolean isErrorStage, boolean isFinalStage) {
		}
	};
	
}
