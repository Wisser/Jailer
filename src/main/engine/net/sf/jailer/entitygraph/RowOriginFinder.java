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

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.entitygraph.remote.RemoteEntityGraph;

/**
 * Answers the question "why is this row in the subset" by walking an entity-graph backwards,
 * from a given row up to the subject.
 * <p>
 * Needs an entity-graph which has been kept after the export, see
 * {@link net.sf.jailer.ExecutionContext#isKeepEntityGraph()}, and rows which carry the
 * association they have been collected through.
 * <p>
 * Knows nothing about any user interface: everything it needs is the attached graph, and a row
 * is addressed by its primary key values. Whoever holds those can ask.
 *
 * @author Ralf Wisser
 */
public class RowOriginFinder {

	/**
	 * Safety limit for the length of a chain.
	 */
	private static final int MAX_STEPS = 1000;

	/**
	 * Number of predecessors read per step. Two are enough to tell "exactly one" from "several".
	 */
	private static final int PREDECESSORS_PER_STEP = 2;

	private final RemoteEntityGraph entityGraph;
	private final DataModel dataModel;

	private boolean birthdayOfSubjectIsKnown = false;

	/**
	 * Constructor.
	 *
	 * @param entityGraph the entity-graph to analyze, attached via
	 *        {@link RemoteEntityGraph#attach(DataModel, int, net.sf.jailer.database.Session, net.sf.jailer.datamodel.PrimaryKey, net.sf.jailer.ExecutionContext)}
	 * @param dataModel the data model the graph has been collected with
	 */
	public RowOriginFinder(RemoteEntityGraph entityGraph, DataModel dataModel) {
		this.entityGraph = entityGraph;
		this.dataModel = dataModel;
	}

	/**
	 * Finds the way a row has taken into the subset.
	 *
	 * @param table the table of the row
	 * @param primaryKey the primary key values of the row, in the order of the table's primary key
	 * @return the chain from the subject down to the row
	 */
	public RowOrigin find(Table table, Object[] primaryKey) throws SQLException {
		if (!birthdayOfSubjectIsKnown) {
			// the pseudo-columns of a join condition are resolved against it
			entityGraph.setBirthdayOfSubject(entityGraph.readBirthdayOfSubject());
			birthdayOfSubjectIsKnown = true;
		}

		List<RowOriginStep> steps = new ArrayList<RowOriginStep>();
		RowOrigin.Status status = RowOrigin.Status.COMPLETE;
		Table currentTable = table;
		Object[] currentKey = primaryKey;

		for (;;) {
			RemoteEntityGraph.EntityRecord record = entityGraph.readEntityRecord(currentTable, currentKey);
			if (record == null) {
				status = steps.isEmpty()? RowOrigin.Status.NOT_COLLECTED : RowOrigin.Status.BROKEN;
				break;
			}
			Association association = record.associationId == null?
					null : dataModel.getAssociationById(record.associationId.intValue());
			if (record.associationId != null && association == null) {
				// the data model has changed since the export
				steps.add(new RowOriginStep(currentTable, currentKey, record.birthday, null, false));
				status = RowOrigin.Status.BROKEN;
				break;
			}
			if (association == null) {
				steps.add(new RowOriginStep(currentTable, currentKey, record.birthday, null, false));
				break;
			}
			List<Object[]> predecessors = entityGraph.readPredecessorKeys(
					association, currentTable, currentKey, record.birthday, PREDECESSORS_PER_STEP);
			steps.add(new RowOriginStep(currentTable, currentKey, record.birthday, association, predecessors.size() > 1));
			if (predecessors.isEmpty()) {
				status = RowOrigin.Status.BROKEN;
				break;
			}
			if (steps.size() >= MAX_STEPS) {
				status = RowOrigin.Status.TRUNCATED;
				break;
			}
			currentTable = association.source;
			currentKey = predecessors.get(0);
		}

		Collections.reverse(steps);
		return new RowOrigin(steps, status);
	}

}
