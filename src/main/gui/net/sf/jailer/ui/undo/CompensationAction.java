/*
 * Copyright 2007 - 2024 Ralf Wisser.
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
package net.sf.jailer.ui.undo;

/**
 * Compensates a change.
 */
public abstract class CompensationAction {

	final int prio;
	final String whatHasDone;
	final String whatToCompensate;
	final String where;
	
	public CompensationAction(int prio, String whatHasDone, String whatToCompensate, String where) {
		this.prio = prio;
		this.whatHasDone = whatHasDone;
		this.whatToCompensate = whatToCompensate;
		this.where = where;
	}
	
	/**
	 * Compensates a change.
	 */
	public abstract void run();

}
