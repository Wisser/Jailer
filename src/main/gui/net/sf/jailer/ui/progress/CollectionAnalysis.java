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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.ModelElement;
import net.sf.jailer.datamodel.Table;

/**
 * Aggregates the number of collected rows per association.
 * <p>
 * The subsetting engine reports the number of newly collected rows per association and
 * per day (see {@link net.sf.jailer.progress.ProgressListener#collected(int, ModelElement, long)}).
 * The progress display reduces this information to the destination table of the association.
 * This class keeps it, so that it can be answered which association is responsible for
 * how many rows of the subset.
 * <p>
 * A row is counted for the association through which it has been collected first. Rows which
 * are reachable via several associations are therefore not counted more than once.
 * <p>
 * Thread safe. It is written by the thread which runs the subsetting engine and read by the
 * event dispatch thread.
 *
 * @author Ralf Wisser
 */
public class CollectionAnalysis {

	/**
	 * Immutable snapshot of the contribution of a single association (or subject table).
	 */
	public static class Contribution {

		private final ModelElement modelElement;
		private final long rows;
		private final int firstDay;
		private final int lastDay;
		private final int numberOfResolutions;
		private final Map<Integer, Long> rowsPerDay;

		Contribution(ModelElement modelElement, long rows, int firstDay, int lastDay, int numberOfResolutions, Map<Integer, Long> rowsPerDay) {
			this.modelElement = modelElement;
			this.rows = rows;
			this.firstDay = firstDay;
			this.lastDay = lastDay;
			this.numberOfResolutions = numberOfResolutions;
			this.rowsPerDay = rowsPerDay;
		}

		/**
		 * Gets the association or the subject table this contribution belongs to.
		 *
		 * @return the association or the subject table
		 */
		public ModelElement getModelElement() {
			return modelElement;
		}

		/**
		 * Gets the association this contribution belongs to.
		 *
		 * @return the association or <code>null</code> if the rows have been collected as subject rows
		 */
		public Association getAssociation() {
			return modelElement instanceof Association? (Association) modelElement : null;
		}

		/**
		 * Gets the table the collected rows belong to.
		 *
		 * @return the destination table
		 */
		public Table getDestination() {
			return destinationOf(modelElement);
		}

		/**
		 * Gets the total number of rows collected through this association.
		 *
		 * @return the number of rows
		 */
		public long getRows() {
			return rows;
		}

		/**
		 * Gets the first day at which rows have been collected through this association.
		 *
		 * @return the first day
		 */
		public int getFirstDay() {
			return firstDay;
		}

		/**
		 * Gets the last day at which rows have been collected through this association.
		 *
		 * @return the last day
		 */
		public int getLastDay() {
			return lastDay;
		}

		/**
		 * Gets the number of times this association has been resolved with a result greater than zero.
		 *
		 * @return the number of resolutions
		 */
		public int getNumberOfResolutions() {
			return numberOfResolutions;
		}

		/**
		 * Gets the number of rows collected per day.
		 *
		 * @return unmodifiable map from day to number of rows, ordered by day
		 */
		public Map<Integer, Long> getRowsPerDay() {
			return rowsPerDay;
		}
	}

	/**
	 * Mutable counter per model element.
	 */
	private static class Counter {
		long rows;
		int firstDay = Integer.MAX_VALUE;
		int lastDay = -1;
		int numberOfResolutions;
		final Map<Integer, Long> rowsPerDay = new TreeMap<Integer, Long>();
	}

	private final Map<ModelElement, Counter> counters = new HashMap<ModelElement, Counter>();
	private long total;

	/**
	 * Adds the result of a single resolution step.
	 *
	 * @param day the day
	 * @param modelElement the association which has been resolved, or the subject table
	 * @param rows the number of rows which have been collected, ignored if not positive
	 */
	public synchronized void add(int day, ModelElement modelElement, long rows) {
		if (modelElement == null || rows <= 0) {
			return;
		}
		Counter counter = counters.get(modelElement);
		if (counter == null) {
			counter = new Counter();
			counters.put(modelElement, counter);
		}
		counter.rows += rows;
		counter.firstDay = Math.min(counter.firstDay, day);
		counter.lastDay = Math.max(counter.lastDay, day);
		++counter.numberOfResolutions;
		Long rowsOfDay = counter.rowsPerDay.get(day);
		counter.rowsPerDay.put(day, rowsOfDay == null? rows : rowsOfDay + rows);
		total += rows;
	}

	/**
	 * Gets the total number of collected rows.
	 *
	 * @return the total number of collected rows
	 */
	public synchronized long getTotal() {
		return total;
	}

	/**
	 * Gets a snapshot of all contributions, the one with the most rows first.
	 *
	 * @return the contributions, sorted by number of rows in descending order
	 */
	public synchronized List<Contribution> getContributions() {
		List<Contribution> result = new ArrayList<Contribution>(counters.size());
		for (Map.Entry<ModelElement, Counter> e: counters.entrySet()) {
			Counter counter = e.getValue();
			result.add(new Contribution(
					e.getKey(),
					counter.rows,
					counter.firstDay,
					counter.lastDay,
					counter.numberOfResolutions,
					Collections.unmodifiableMap(new LinkedHashMap<Integer, Long>(counter.rowsPerDay))));
		}
		Collections.sort(result, new Comparator<Contribution>() {
			@Override
			public int compare(Contribution a, Contribution b) {
				if (a.getRows() != b.getRows()) {
					return a.getRows() < b.getRows()? 1 : -1;
				}
				return 0;
			}
		});
		return result;
	}

	/**
	 * Gets the destination table of a model element.
	 *
	 * @param modelElement an association or a table
	 * @return the destination table of an association, or the table itself
	 */
	static Table destinationOf(ModelElement modelElement) {
		if (modelElement instanceof Association) {
			return ((Association) modelElement).destination;
		}
		if (modelElement instanceof Table) {
			return (Table) modelElement;
		}
		return null;
	}

}
