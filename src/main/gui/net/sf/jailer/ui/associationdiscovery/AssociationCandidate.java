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
package net.sf.jailer.ui.associationdiscovery;

import java.util.List;

import net.sf.jailer.datamodel.Cardinality;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.Quoting;

/**
 * A potential association from a child table to a parent table, as found by the
 * {@link AssociationCandidateFinder} and verified by the {@link CandidateVerifier}.
 *
 * @author Ralf Wisser
 */
public class AssociationCandidate {

	/**
	 * What the candidate is based on.
	 */
	public enum Evidence {

		/**
		 * The child columns have the same names as the primary key columns of the parent.
		 */
		NAME_EXACT(1.0, "Name matches primary key"),

		/**
		 * The child column names follow a naming convention.
		 */
		NAME_PATTERN(0.9, "Name follows convention"),

		/**
		 * No name evidence at all, found by scanning the data.
		 */
		DATA_ONLY(0.6, "Data scan only");

		public final double weight;
		public final String description;

		private Evidence(double weight, String description) {
			this.weight = weight;
			this.description = description;
		}
	}

	public final Table child;
	public final Table parent;
	public final List<Column> childColumns;
	public final List<Column> parentColumns;
	public final Evidence evidence;

	/**
	 * Number of rows of the child table in which all child columns are not null,
	 * <code>-1</code> if unknown.
	 */
	public long nonNullCount = -1;

	/**
	 * Number of child rows without a matching parent row, <code>-1</code> if not verified.
	 */
	public long orphans = -1;

	/**
	 * Fraction of child rows that have a matching parent row, <code>-1</code> if not verified.
	 */
	public double matchRatio = -1;

	/**
	 * Cardinality as derived from the profile, <code>null</code> if unknown.
	 */
	public Cardinality cardinality;

	/**
	 * Confidence in percent.
	 */
	public double confidence;

	/**
	 * The statement used for the verification.
	 */
	public String verifyStatement;

	/**
	 * @param child the child table (the table holding the foreign key)
	 * @param parent the parent table (the table holding the primary key)
	 * @param childColumns the foreign key columns
	 * @param parentColumns the primary key columns, in the same order
	 * @param evidence what the candidate is based on
	 */
	public AssociationCandidate(Table child, Table parent, List<Column> childColumns, List<Column> parentColumns, Evidence evidence) {
		this.child = child;
		this.parent = parent;
		this.childColumns = childColumns;
		this.parentColumns = parentColumns;
		this.evidence = evidence;
	}

	/**
	 * Gets the join condition, with "A" being the child and "B" being the parent table.
	 *
	 * @return the join condition
	 */
	public String getCondition() {
		StringBuilder condition = new StringBuilder();
		for (int i = 0; i < childColumns.size(); ++i) {
			if (condition.length() > 0) {
				condition.append(" and ");
			}
			condition.append("A." + childColumns.get(i).name + "=B." + parentColumns.get(i).name);
		}
		return condition.toString();
	}

	/**
	 * Gets a human readable summary of the evidence for this candidate.
	 *
	 * @return the summary
	 */
	public String getEvidenceText() {
		StringBuilder text = new StringBuilder(evidence.description);
		if (matchRatio >= 0 && nonNullCount >= 0) {
			long matched = nonNullCount - Math.max(0, orphans);
			text.append("; " + matched + " of " + nonNullCount + " rows match");
			if (orphans > 0) {
				text.append(" (" + orphans + " without parent)");
			}
		}
		if (cardinality != null) {
			text.append("; " + cardinality.toString());
		}
		return text.toString();
	}

	/**
	 * Gets a key that identifies the column mapping of this candidate,
	 * independent of quoting and case.
	 *
	 * @return the key
	 */
	public String key() {
		StringBuilder key = new StringBuilder(Quoting.normalizeIdentifier(child.getName()) + ">" + Quoting.normalizeIdentifier(parent.getName()));
		for (int i = 0; i < childColumns.size(); ++i) {
			key.append("|" + Quoting.normalizeIdentifier(childColumns.get(i).name)
					+ "=" + Quoting.normalizeIdentifier(parentColumns.get(i).name));
		}
		return key.toString();
	}

	@Override
	public String toString() {
		return child.getName() + " -> " + parent.getName() + " on " + getCondition();
	}

}
