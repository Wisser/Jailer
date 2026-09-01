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

import java.awt.Window;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.entitygraph.RowOriginStep;
import net.sf.jailer.entitygraph.remote.RemoteEntityGraph;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.util.ConcurrentTaskControl;
import net.sf.jailer.util.CancellationException;

/**
 * The way of a row into the subset, described such that the Data Browser can lay it out as a
 * chain of table browsers without touching the entity-graph or the database.
 * <p>
 * Tables and associations are named, not referenced: the data model of the run and the one of
 * the Data Browser are two different load operations of the same model, so the Data Browser has
 * to resolve the names against its own model, as {@code Desktop.restoreSession} does for a
 * stored layout.
 *
 * @author Ralf Wisser
 */
public class RowOriginPath {

	/**
	 * One table browser of the chain.
	 */
	public static class Step {

		/**
		 * Name of the table to browse.
		 */
		public final String tableName;

		/**
		 * Name of the association leading here from the previous step, <code>null</code> for the
		 * subject row, which is the root of the chain.
		 */
		public final String associationName;

		/**
		 * Condition pinning the browser to the single row of this step, on the alias "A", which
		 * is the browser's own table.
		 */
		public final String condition;

		/**
		 * Constructor.
		 *
		 * @param tableName name of the table to browse
		 * @param associationName name of the association leading here, or <code>null</code>
		 * @param condition condition pinning the browser to the row of this step
		 */
		public Step(String tableName, String associationName, String condition) {
			this.tableName = tableName;
			this.associationName = associationName;
			this.condition = condition;
		}

		@Override
		public String toString() {
			return tableName + " (" + condition + ")"
					+ (associationName == null? " [subject]" : " via " + associationName);
		}
	}

	/**
	 * Describes the chain of a row origin. Talks to the database, so it must not be called on the
	 * event dispatch thread. Callers which have nothing else to do in the background use
	 * {@link #build(Window, RowOriginContext, List)} instead.
	 *
	 * @param context the context holding the retained rows
	 * @param steps the chain, subject first, as delivered by {@link net.sf.jailer.entitygraph.RowOrigin#getSteps()}
	 * @return the path
	 */
	public static List<Step> describe(RowOriginContext context, List<RowOriginStep> steps) throws Exception {
		RemoteEntityGraph entityGraph = context.getEntityGraph();
		List<Step> path = new ArrayList<Step>();
		for (RowOriginStep step: steps) {
			Association association = step.getIncomingAssociation();
			path.add(new Step(
					step.getTable().getName(),
					association == null? null : association.getName(),
					// "A" is the browser's own table, see BrowserContentPane.reloadRows0
					entityGraph.pkEqualsValues(step.getTable(), step.getPrimaryKey(), "A")));
		}
		return path;
	}

	/**
	 * Describes the chain of a row origin off the event dispatch thread, since it may have to
	 * open a session, showing a dialog that can be cancelled.
	 *
	 * @param owner the window to block while the description is prepared
	 * @param context the context holding the retained rows
	 * @param steps the chain, subject first
	 * @return the path, or <code>null</code> if the user has cancelled or an error has been shown
	 */
	public static List<Step> build(Window owner, final RowOriginContext context, final List<RowOriginStep> steps) {
		try {
			return ConcurrentTaskControl.call(owner, new Callable<List<Step>>() {
				@Override
				public List<Step> call() throws Exception {
					return describe(context, steps);
				}
			}, "Preparing path...", null);
		} catch (CancellationException e) {
			return null;
		} catch (Throwable t) {
			UIUtil.showException(owner, "Error", t);
			return null;
		}
	}

	private RowOriginPath() {
	}

}
