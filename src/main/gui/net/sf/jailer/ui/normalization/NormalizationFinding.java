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
package net.sf.jailer.ui.normalization;

import java.util.List;
import java.util.Map;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.Table;

/**
 * A possible violation of a normal form, found by the {@link NormalizationChecker}.
 *
 * @author Ralf Wisser
 */
public class NormalizationFinding {

	/**
	 * The normal forms, in ascending order.
	 */
	public enum NormalForm {
		NF1("1NF"), NF2("2NF"), NF3("3NF"), NF4_5("4NF/5NF"),
		/**
		 * Not a normal form: consistency of the schema, e.g. matching types of foreign keys.
		 */
		CONSISTENCY("Consistency");

		public final String displayName;

		NormalForm(String displayName) {
			this.displayName = displayName;
		}

		@Override
		public String toString() {
			return displayName;
		}
	}

	/**
	 * How certain a finding is.
	 */
	public enum Severity {
		LIKELY("likely"), POSSIBLE("possible");

		public final String displayName;

		Severity(String displayName) {
			this.displayName = displayName;
		}

		@Override
		public String toString() {
			return displayName;
		}
	}

	/**
	 * The table.
	 */
	public final Table table;

	/**
	 * The affected columns. Empty if the finding concerns the table as a whole.
	 */
	public final List<Column> columns;

	/**
	 * The violated normal form.
	 */
	public final NormalForm normalForm;

	/**
	 * How certain the finding is. Raised to {@link Severity#LIKELY} if the data confirms it.
	 */
	public volatile Severity severity;

	/**
	 * Short name of the rule that produced the finding.
	 */
	public final String rule;

	/**
	 * Explanation for the user.
	 */
	public final String explanation;

	/**
	 * For a copied column finding: the parent table. Otherwise <code>null</code>.
	 */
	public Table parent;

	/**
	 * For a copied column finding: foreign key column of the table -> primary key column of the parent.
	 */
	public Map<Column, Column> fkMapping;

	/**
	 * For a copied column finding: copied column of the table -> original column of the parent.
	 */
	public Map<Column, Column> copies;

	/**
	 * For a copied column finding: the association of the foreign key from the table to the parent.
	 */
	public Association association;

	/**
	 * Result of the check against the data, or <code>null</code> if the data has not been checked.
	 */
	public volatile String dataCheck;

	public NormalizationFinding(Table table, List<Column> columns, NormalForm normalForm, Severity severity, String rule, String explanation) {
		this.table = table;
		this.columns = columns;
		this.normalForm = normalForm;
		this.severity = severity;
		this.rule = rule;
		this.explanation = explanation;
	}

	/**
	 * @return <code>true</code> if the finding can be checked against the data
	 */
	public boolean isDataCheckable() {
		return parent != null && fkMapping != null && copies != null && !copies.isEmpty();
	}

	/**
	 * @return what "Show Table" selects: the foreign key, if the finding is about one, otherwise the table
	 */
	public String getShowTableTarget() {
		if (association != null) {
			return "the foreign key from " + table.getUnqualifiedName() + " to " + association.destination.getUnqualifiedName();
		}
		return "table " + table.getUnqualifiedName();
	}

	/**
	 * @return what "Browse Table" opens: the table, and the referenced table if the finding is about a foreign key
	 */
	public String getBrowseTableTarget() {
		if (association != null) {
			return "tables " + table.getUnqualifiedName() + " and " + association.destination.getUnqualifiedName();
		}
		return "table " + table.getUnqualifiedName();
	}

	/**
	 * @return comma separated names of the affected columns
	 */
	public String getColumnNames() {
		StringBuilder sb = new StringBuilder();
		for (Column column: columns) {
			if (sb.length() > 0) {
				sb.append(", ");
			}
			sb.append(column.name);
		}
		return sb.toString();
	}

}
