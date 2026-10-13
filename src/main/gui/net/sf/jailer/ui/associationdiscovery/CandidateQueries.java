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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.function.BiConsumer;

import javax.swing.ImageIcon;
import javax.swing.JMenuItem;

import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.util.Quoting;

/**
 * SQL queries that help to judge an {@link AssociationCandidate}. <br>
 * "A" is the child table (with the foreign key), "B" the parent table.
 *
 * @author Ralf Wisser
 */
public class CandidateQueries {

	/**
	 * Maximum length of generated constraint and index names (Oracle up to 12.1).
	 */
	private static final int MAX_NAME_LENGTH = 30;

	/**
	 * Child rows whose foreign key has no matching parent row.
	 */
	public static String orphans(AssociationCandidate c) {
		return "Select A.* From " + c.child.getName() + " A Where " + fkNotNull(c) + " and " + noParent(c);
	}

	/**
	 * The missing key values and how often each of them occurs.
	 */
	public static String missingKeys(AssociationCandidate c) {
		String aCols = columns("A.", c.childColumns);
		return "Select " + aCols + ", count(*) From " + c.child.getName() + " A Where " + fkNotNull(c) + " and " + noParent(c)
				+ " Group by " + aCols + " Order by " + (c.childColumns.size() + 1) + " desc";
	}

	/**
	 * Rows, filled foreign keys, (distinct values,) matching and orphaned rows in a single row.
	 */
	public static String summary(AssociationCandidate c) {
		String parentKey = "B." + c.parentColumns.get(0).name;
		StringBuilder sql = new StringBuilder("Select count(*) rows_a, ");
		sql.append("sum(case when " + fkNotNull(c) + " then 1 else 0 end) fk_filled, ");
		if (c.childColumns.size() == 1) {
			// count(distinct ...) over several columns is not portable
			sql.append("count(distinct A." + c.childColumns.get(0).name + ") fk_distinct, ");
		}
		sql.append("sum(case when " + parentKey + " is not null then 1 else 0 end) matched, ");
		sql.append("sum(case when " + fkNotNull(c) + " and " + parentKey + " is null then 1 else 0 end) orphans ");
		sql.append("From " + c.child.getName() + " A left join " + c.parent.getName() + " B on " + c.getCondition());
		return sql.toString();
	}

	/**
	 * Parent rows no child row refers to.
	 */
	public static String parentsWithoutChildren(AssociationCandidate c) {
		return "Select B.* From " + c.parent.getName() + " B Where not exists (Select 1 From " + c.child.getName() + " A Where " + c.getCondition() + ")";
	}

	/**
	 * How many parent rows have how many child rows.
	 */
	public static String childrenPerParent(AssociationCandidate c) {
		String aCols = columns("A.", c.childColumns);
		return "Select cnt, count(*) parents From (Select " + aCols + ", count(*) cnt From " + c.child.getName() + " A Where " + fkNotNull(c)
				+ " Group by " + aCols + ") X Group by cnt Order by cnt";
	}

	/**
	 * Values of the parent columns that are not unique. A foreign key requires unique parent columns.
	 */
	public static String duplicatesInParent(AssociationCandidate c) {
		String bCols = columns("B.", c.parentColumns);
		return "Select " + bCols + ", count(*) From " + c.parent.getName() + " B Group by " + bCols + " Having count(*) > 1";
	}

	/**
	 * DDL that turns the candidate into a foreign key constraint, plus an index on the foreign key columns.
	 */
	public static String foreignKeyDDL(AssociationCandidate c) {
		String childName = plainName(c.child);
		String parentName = plainName(c.parent);
		String fkName = limit("FK_" + childName + "_" + parentName);
		String ixName = limit("IX_" + childName + "_" + identifier(Quoting.staticUnquote(c.childColumns.get(0).name)));
		return "Alter Table " + c.child.getName() + " Add Constraint " + fkName
				+ " Foreign Key (" + columns("", c.childColumns) + ") References " + c.parent.getName() + " (" + columns("", c.parentColumns) + ");"
				+ UIUtil.LINE_SEPARATOR
				+ "Create Index " + ixName + " On " + c.child.getName() + " (" + columns("", c.childColumns) + ");";
	}

	/**
	 * Creates the menu items for the queries. <code>null</code> stands for a separator.
	 *
	 * @param c the candidate
	 * @param open opens statements in the SQL Console (statements, execute them)
	 * @param icon icon of the items
	 * @return the items
	 */
	public static List<JMenuItem> createMenuItems(AssociationCandidate c, BiConsumer<List<String>, Boolean> open, ImageIcon icon) {
		List<Query> queries = queries(c);
		List<JMenuItem> items = new ArrayList<JMenuItem>();
		for (Query query: queries) {
			if (!query.execute) {
				items.add(null);
			}
			items.add(item(query.label, query.description + (query.execute? "" : " Shown, not executed."), Collections.singletonList(query.commented()), query.execute, open, icon));
		}
		items.add(null);
		items.add(item("All Queries (not executed)", "All of the queries above, one after the other. None of them is executed: "
				+ "place the cursor into a statement and press Ctrl+Enter to run it.", allQueries(c, queries), false, open, icon));
		return items;
	}

	/**
	 * A query with its label and description.
	 */
	private static class Query {
		final String label;
		final String description;
		final String sql;
		final boolean execute;

		Query(String label, String description, String sql, boolean execute) {
			this.label = label;
			this.description = description;
			this.sql = sql;
			this.execute = execute;
		}

		/**
		 * @return the statement, preceded by a comment that describes it
		 */
		String commented() {
			return "-- " + label + ": " + description + UIUtil.LINE_SEPARATOR + sql;
		}
	}

	/**
	 * Gets all queries about a candidate. The ones that are not executed (DDL) come last.
	 */
	private static List<Query> queries(AssociationCandidate c) {
		String a = c.child.getUnqualifiedName();
		String b = c.parent.getUnqualifiedName();
		List<Query> queries = new ArrayList<Query>();
		queries.add(new Query("Join (left)", "All rows of " + a + " with the matching row of " + b + " (empty if there is none).", c.getJoinStatement(), true));
		if (c.verifyStatement != null) {
			// null if the proposal has not been checked against the data
			queries.add(new Query("Verification Query", "The query that checked the proposal during the discovery: the number of rows of " + a
					+ " whose foreign key has no matching row in " + b + ".", c.verifyStatement, true));
		}
		queries.add(new Query("Orphans", "Rows of " + a + " whose foreign key has no matching row in " + b + ".", orphans(c), true));
		queries.add(new Query("Missing Keys", "The foreign key values of " + a + " that have no matching row in " + b + ", and how often each occurs.", missingKeys(c), true));
		queries.add(new Query("Summary", "Rows of " + a + ", filled foreign keys, matching and orphaned rows, in a single row.", summary(c), true));
		queries.add(new Query("Parents without Children", "Rows of " + b + " no row of " + a + " refers to.", parentsWithoutChildren(c), true));
		queries.add(new Query("Children per Parent", "How many rows of " + b + " have how many rows of " + a + " (cardinality).", childrenPerParent(c), true));
		queries.add(new Query("Duplicates in B", "Values of the columns of " + b + " that are not unique. A foreign key requires unique columns.", duplicatesInParent(c), true));
		queries.add(new Query("Foreign Key DDL", "Statements that create the foreign key constraint and an index on " + a + ".", foreignKeyDDL(c), false));
		return queries;
	}

	/**
	 * All queries, each with its comment, the first one after a comment that says that none of them has been executed. <br>
	 * Each query is a part of its own: the SQL Console removes empty lines within an appended text,
	 * so that only separately appended parts are separated by an empty line (and are separate blocks for Ctrl+Enter).
	 */
	private static List<String> allQueries(AssociationCandidate c, List<Query> queries) {
		String nl = UIUtil.LINE_SEPARATOR;
		String line = "-- ==========================================================================";
		StringBuilder header = new StringBuilder();
		header.append(line + nl);
		header.append("-- Queries about the proposed association " + c.child.getUnqualifiedName() + " -> " + c.parent.getUnqualifiedName() + nl);
		header.append("-- on " + c.getCondition() + nl);
		header.append("--" + nl);
		header.append("-- NONE OF THE FOLLOWING STATEMENTS HAS BEEN EXECUTED." + nl);
		header.append("-- Place the cursor into a statement and press Ctrl+Enter to run it." + nl);
		header.append(line + nl);
		List<String> parts = new ArrayList<String>();
		for (Query query: queries) {
			// the header goes with the first query, on its own the console would append a ";" to it
			parts.add((parts.isEmpty()? header.toString() : "") + query.commented());
		}
		return parts;
	}

	/**
	 * @param parts the statements, each appended to the console separately
	 */
	private static JMenuItem item(String text, String toolTip, List<String> parts, boolean execute, BiConsumer<List<String>, Boolean> open, ImageIcon icon) {
		JMenuItem item = new JMenuItem(text);
		if (icon != null) {
			item.setIcon(UIUtil.scaleIcon(item, icon));
		}
		item.setToolTipText(toolTip);
		// all parts in one call: opening a browser for them may show modal dialogs, during which further
		// calls would already be processed (and each would open a browser of its own)
		item.addActionListener(e -> open.accept(parts, execute));
		return item;
	}

	private static String fkNotNull(AssociationCandidate c) {
		StringBuilder condition = new StringBuilder();
		for (Column column: c.childColumns) {
			if (condition.length() > 0) {
				condition.append(" and ");
			}
			condition.append("A." + column.name + " is not null");
		}
		return condition.toString();
	}

	private static String noParent(AssociationCandidate c) {
		return "not exists (Select 1 From " + c.parent.getName() + " B Where " + c.getCondition() + ")";
	}

	private static String columns(String prefix, List<Column> columns) {
		StringBuilder sb = new StringBuilder();
		for (Column column: columns) {
			if (sb.length() > 0) {
				sb.append(", ");
			}
			sb.append(prefix + column.name);
		}
		return sb.toString();
	}

	private static String plainName(Table table) {
		return identifier(Quoting.staticUnquote(table.getUnqualifiedName()));
	}

	/**
	 * Upper case, with all characters other than letters, digits and "_" replaced by "_".
	 */
	private static String identifier(String name) {
		return name.toUpperCase(Locale.ENGLISH).replaceAll("[^A-Z0-9_]", "_");
	}

	private static String limit(String name) {
		return name.length() > MAX_NAME_LENGTH? name.substring(0, MAX_NAME_LENGTH) : name;
	}

	private CandidateQueries() {
	}

}
