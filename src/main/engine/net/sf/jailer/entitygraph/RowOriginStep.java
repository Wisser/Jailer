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

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Table;

/**
 * One row on the way from the subject to the row in question.
 * <p>
 * Immutable and free of any UI dependency, so that it can be rendered wherever the question
 * "why is this row in the subset" is asked.
 *
 * @author Ralf Wisser
 */
public class RowOriginStep {

	private final Table table;
	private final Object[] primaryKey;
	private final int birthday;
	private final Association incomingAssociation;
	private final boolean ambiguous;

	/**
	 * Constructor.
	 *
	 * @param table the table of the row
	 * @param primaryKey the primary key values of the row, in the order of the table's primary key
	 * @param birthday the collection step at which the row has been collected
	 * @param incomingAssociation the association through which the row has been collected,
	 *        or <code>null</code> if it is a subject row
	 * @param ambiguous <code>true</code> if more than one row of the source table joins with this
	 *        one, so that the predecessor shown is one of several
	 */
	public RowOriginStep(Table table, Object[] primaryKey, int birthday, Association incomingAssociation, boolean ambiguous) {
		this.table = table;
		this.primaryKey = primaryKey;
		this.birthday = birthday;
		this.incomingAssociation = incomingAssociation;
		this.ambiguous = ambiguous;
	}

	/**
	 * Gets the table of the row.
	 *
	 * @return the table
	 */
	public Table getTable() {
		return table;
	}

	/**
	 * Gets the primary key values of the row, in the order of the table's primary key.
	 *
	 * @return the primary key values
	 */
	public Object[] getPrimaryKey() {
		return primaryKey;
	}

	/**
	 * Gets the collection step at which the row has been collected.
	 *
	 * @return the collection step
	 */
	public int getBirthday() {
		return birthday;
	}

	/**
	 * Gets the association through which the row has been collected.
	 *
	 * @return the association, or <code>null</code> if this is a subject row
	 */
	public Association getIncomingAssociation() {
		return incomingAssociation;
	}

	/**
	 * Returns whether more than one row of the source table joins with this one, so that the
	 * predecessor shown is one of several.
	 *
	 * @return <code>true</code> if the predecessor is not unique
	 */
	public boolean isAmbiguous() {
		return ambiguous;
	}

	/**
	 * Renders the primary key values as text, for display and logging.
	 *
	 * @return the primary key values, separated by ", "
	 */
	public String getPrimaryKeyAsText() {
		StringBuilder sb = new StringBuilder();
		for (Object value: primaryKey) {
			if (sb.length() > 0) {
				sb.append(", ");
			}
			sb.append(value == null? "null" : value.toString());
		}
		return sb.toString();
	}

	@Override
	public String toString() {
		return table.getName() + "(" + getPrimaryKeyAsText() + ")"
				+ (incomingAssociation == null? " [subject]" : " via " + incomingAssociation.getName());
	}

}
