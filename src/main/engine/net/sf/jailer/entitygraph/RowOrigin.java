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
package net.sf.jailer.entitygraph;

import java.util.Collections;
import java.util.List;

/**
 * The way a row has taken into the subset: the chain from the subject down to the row.
 * <p>
 * Note that this is the way the row has been found <b>first</b>, not every way it could have
 * been reached. That is the answer to the question why the row is part of the subset: without
 * this way it might not be.
 *
 * @author Ralf Wisser
 */
public class RowOrigin {

	/**
	 * How the chain ends.
	 */
	public enum Status {

		/**
		 * The chain ends at a subject row.
		 */
		COMPLETE,

		/**
		 * The row is not part of the entity-graph at all.
		 */
		NOT_COLLECTED,

		/**
		 * No predecessor could be found, although the row is not a subject row. This happens if
		 * rows have been removed from the graph after the collection, for instance by the
		 * delete stage.
		 */
		BROKEN,

		/**
		 * The chain is longer than the safety limit and has been cut off.
		 */
		TRUNCATED
	}

	private final List<RowOriginStep> steps;
	private final Status status;

	/**
	 * Constructor.
	 *
	 * @param steps the chain, subject first
	 * @param status how the chain ends
	 */
	public RowOrigin(List<RowOriginStep> steps, Status status) {
		this.steps = Collections.unmodifiableList(steps);
		this.status = status;
	}

	/**
	 * Gets the chain, subject first.
	 *
	 * @return the steps, subject first
	 */
	public List<RowOriginStep> getSteps() {
		return steps;
	}

	/**
	 * Gets the status of the chain.
	 *
	 * @return the status
	 */
	public Status getStatus() {
		return status;
	}

}
