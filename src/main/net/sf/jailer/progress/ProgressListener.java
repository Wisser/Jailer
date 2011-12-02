/*
 * Copyright 2007 - 2012 the original author or authors.
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
 * Listener interface for monitoring progress.
 * 
 * @author Ralf Wisser
 */
public interface ProgressListener {

	/**
	 * A collection-job has been enqueued.
	 * 
	 * @param day the day
	 * @param modelElement the association or table to be resolved
	 */
	void collectionJobEnqueued(int day, ModelElement modelElement);
	
	/**
	 * Collection of rows has been started.
	 * 
	 * @param day the day
	 * @param modelElement the association or table to be resolved
	 */
	void collectionJobStarted(int day, ModelElement modelElement);
	
	/**
	 * Rows have been collected.
	 * 
	 * @param day the day
	 * @param modelElement the association or table which has been resolved
	 * @param rc the number of rows which have been collected
	 */
	void collected(int day, ModelElement modelElement, long rc);
	
	/**
	 * Rows have been exported.
	 * 
	 * @param table the table from which the rows have been exported
	 * @param rc the number of rows
	 */
	void exported(Table table, long rc);
	
	/**
	 * New stage has begun.
	 * 
	 * @param stage the stage
	 */
	void newStage(String stage, boolean isErrorStage, boolean isFinalStage);
	
}
