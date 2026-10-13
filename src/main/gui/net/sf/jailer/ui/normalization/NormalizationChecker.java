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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.modelbuilder.ModelBuilder;
import net.sf.jailer.ui.normalization.NormalizationFinding.NormalForm;
import net.sf.jailer.ui.normalization.NormalizationFinding.Severity;
import net.sf.jailer.util.Quoting;

/**
 * Looks for possible violations of the normal forms 1NF to 5NF. <br>
 * Uses the metadata of the data model only (primary keys, foreign keys, column names and types),
 * no data. So the findings are heuristic hints, not proofs.
 *
 * @author Ralf Wisser
 */
public class NormalizationChecker {

	/*
	 * The checks (names of the rules of the findings).
	 */
	public static final String RULE_NO_PRIMARY_KEY = "No primary key";
	public static final String RULE_REPEATING_GROUP = "Repeating group";
	public static final String RULE_ARRAY_COLUMN = "Array column";
	public static final String RULE_LIST_IN_COLUMN = "List in a column";
	public static final String RULE_PARTIAL_KEY_DEPENDENCY = "Partial key dependency";
	public static final String RULE_TRANSITIVE_DEPENDENCY = "Transitive dependency";
	public static final String RULE_ALL_KEY_TABLE = "All-key table";
	public static final String RULE_FK_TYPE_MISMATCH = "FK type mismatch";

	/**
	 * All checks with the normal form they check, in display order.
	 */
	public static final Map<String, NormalForm> RULES;
	static {
		Map<String, NormalForm> rules = new LinkedHashMap<String, NormalForm>();
		rules.put(RULE_NO_PRIMARY_KEY, NormalForm.NF1);
		rules.put(RULE_REPEATING_GROUP, NormalForm.NF1);
		rules.put(RULE_ARRAY_COLUMN, NormalForm.NF1);
		rules.put(RULE_LIST_IN_COLUMN, NormalForm.NF1);
		rules.put(RULE_PARTIAL_KEY_DEPENDENCY, NormalForm.NF2);
		rules.put(RULE_TRANSITIVE_DEPENDENCY, NormalForm.NF3);
		rules.put(RULE_ALL_KEY_TABLE, NormalForm.NF4_5);
		rules.put(RULE_FK_TYPE_MISMATCH, NormalForm.CONSISTENCY);
		RULES = Collections.unmodifiableMap(rules);
	}

	/**
	 * Definition of each check (HTML): the normal form, what the check looks for, an example and its limits.
	 */
	public static final Map<String, String> RULE_DEFINITIONS;
	static {
		Map<String, String> definitions = new LinkedHashMap<String, String>();
		definitions.put(RULE_NO_PRIMARY_KEY,
				"<p><b>First normal form (1NF)</b> requires that every row can be identified.</p>"
				+ "<p><b>The check</b> reports tables without a primary key.</p>"
				+ "<p><b>Example:</b> a log table without an id column.</p>"
				+ "<p>Without a key, duplicate rows are possible. The higher normal forms are defined in terms of keys, "
				+ "so they cannot be judged for such a table.</p>");
		definitions.put(RULE_REPEATING_GROUP,
				"<p><b>First normal form (1NF)</b> forbids repeating groups: several columns for the same kind of value.</p>"
				+ "<p><b>The check</b> reports columns whose names differ only by a number "
				+ "(from 0 to 99, the lowest being 0, 1 or 2). Groups with the same numbers are reported together.</p>"
				+ "<p><b>Example:</b> PHONE1, PHONE2, PHONE3, or ADDR1_STREET, ADDR1_CITY, ADDR2_STREET, ADDR2_CITY.</p>"
				+ "<p><b>Fix:</b> a separate table with one row per value.</p>");
		definitions.put(RULE_ARRAY_COLUMN,
				"<p><b>First normal form (1NF)</b> requires atomic values: one value per field.</p>"
				+ "<p><b>The check</b> reports columns with an array type.</p>"
				+ "<p><b>Example:</b> TAGS of type VARCHAR[].</p>"
				+ "<p>The values of an array cannot be referenced by a foreign key. "
				+ "Some databases use arrays on purpose, so this is only a hint.</p>");
		definitions.put(RULE_LIST_IN_COLUMN,
				"<p><b>First normal form (1NF)</b> requires atomic values: one value per field.</p>"
				+ "<p><b>The check</b> reports text columns whose name suggests a list, "
				+ "e.g. *_LIST, *_IDS, TAGS, KEYWORDS, *_CSV, *_CODES, *_VALUES.</p>"
				+ "<p><b>Example:</b> ROLE_IDS = '3,7,12'.</p>"
				+ "<p>Such values cannot be joined or constrained. The check uses the name only, "
				+ "it does not look at the content.</p>");
		definitions.put(RULE_PARTIAL_KEY_DEPENDENCY,
				"<p><b>Second normal form (2NF)</b> requires that every non-key column depends on the whole primary key, "
				+ "not just on a part of it.</p>"
				+ "<p><b>The check</b> looks for a foreign key that is a proper part of a composite primary key, "
				+ "and for non-key columns that repeat a column of the referenced table. "
				+ "The name of the copy must refer to that table (e.g. ORDER_DATE, or the table name plus the column name), "
				+ "and the types must be equal.</p>"
				+ "<p><b>Example:</b> ORDER_LINE(ORDER_ID, LINE_NO, ORDER_DATE) next to ORDERS.ORDER_DATE. "
				+ "ORDER_DATE depends on ORDER_ID only.</p>"
				+ "<p><b>Data check:</b> if connected to a database, the finding is kept only if the copy equals the original "
				+ "in all joined rows, and there are at least 5 of them. For large tables, a sample of 100,000 rows is checked.</p>");
		definitions.put(RULE_TRANSITIVE_DEPENDENCY,
				"<p><b>Third normal form (3NF)</b> requires that every non-key column depends on the primary key only, "
				+ "not on another non-key column.</p>"
				+ "<p><b>The check</b> looks for a foreign key that is not part of the primary key, "
				+ "and for columns that repeat a column of the referenced table. "
				+ "The name of the copy must refer to that table (e.g. DEPT_NAME for DEPT.NAME), "
				+ "and the types must be equal. Columns that are foreign keys themselves are not considered.</p>"
				+ "<p><b>Example:</b> EMPLOYEE(ID, DEPT_ID, DEPT_NAME) next to DEPT.NAME. "
				+ "DEPT_NAME depends on DEPT_ID, which depends on ID.</p>"
				+ "<p><b>Data check:</b> if connected to a database, the finding is kept only if the copy equals the original "
				+ "in all joined rows, and there are at least 5 of them. For large tables, a sample of 100,000 rows is checked.</p>");
		definitions.put(RULE_ALL_KEY_TABLE,
				"<p><b>Fourth normal form (4NF)</b> forbids independent multi-valued facts in one table. "
				+ "<b>Fifth normal form (5NF)</b> forbids tables that can be rebuilt by joining some of their projections.</p>"
				+ "<p><b>The check</b> reports tables whose columns all belong to the primary key "
				+ "and which link three or more tables by foreign keys.</p>"
				+ "<p><b>Example:</b> EMP_SKILL_LANGUAGE(EMP_ID, SKILL_ID, LANGUAGE_ID). If skills and languages "
				+ "are independent of each other, the table has to store every combination. "
				+ "EMP_SKILL and EMP_LANGUAGE would be better.</p>"
				+ "<p>Only knowledge of the domain can tell whether the relationships are independent.</p>");
		definitions.put(RULE_FK_TYPE_MISMATCH,
				"<p><b>Consistency</b> (not a normal form): a foreign key column should have exactly the type of the "
				+ "primary key column it references.</p>"
				+ "<p><b>The check</b> compares each foreign key column with the referenced column. "
				+ "A different type is reported as <i>likely</i>, the same type with a different length or precision as <i>possible</i>. "
				+ "Names of the same type are treated as equal (e.g. INT and INTEGER, NUMBER and NUMERIC, VARCHAR and VARCHAR2).</p>"
				+ "<p><b>Example:</b> ORDER_LINE.ORDER_ID INTEGER referencing ORDERS.ID BIGINT, "
				+ "or CUSTOMER_CODE VARCHAR(10) referencing CUSTOMER.CODE VARCHAR(20).</p>"
				+ "<p>Joins over such keys need implicit conversions, which can prevent the use of indexes, "
				+ "and values of the referenced column may not fit into the foreign key column.</p>");
		RULE_DEFINITIONS = Collections.unmodifiableMap(definitions);
	}

	/**
	 * A group of digits within a column name, e.g. the "2" in "PHONE2" or "ADDR_2_CITY".
	 */
	private static final Pattern DIGITS = Pattern.compile("\\d+");

	/**
	 * Highest index of a repeating group. Larger numbers are rather part of a name
	 * (e.g. "ISO3166") or a year.
	 */
	private static final int MAX_GROUP_INDEX = 99;

	/**
	 * Names of character columns that suggest a list of values.
	 */
	private static final Pattern LIST_NAME = Pattern.compile(".*(_LIST|LIST|_IDS|_CSV|_ARRAY|TAGS|KEYWORDS|_CODES|_VALUES)");

	/**
	 * Names of technical columns (audit, versioning) that exist in many tables
	 * without being redundant.
	 */
	private static final Pattern TECHNICAL_NAME = Pattern.compile(".*(CREAT|MODIF|UPDAT|CHANG|INSERT|DELET|VERSION|TIMESTAMP|TENANT).*");

	/**
	 * A foreign key of a table.
	 */
	private static class ForeignKey {
		final List<Column> columns;
		final Table parent;
		final Map<Column, Column> mapping;
		final Association association;

		ForeignKey(Association association, Map<Column, Column> mapping, Table parent) {
			this.association = association;
			this.mapping = mapping;
			this.columns = new ArrayList<Column>(mapping.keySet());
			this.parent = parent;
		}
	}

	/**
	 * Checks the data model.
	 *
	 * @param dataModel the data model
	 * @return the findings
	 */
	public List<NormalizationFinding> check(DataModel dataModel) {
		List<NormalizationFinding> findings = new ArrayList<NormalizationFinding>();
		fkColumnNamesCache.clear();
		for (Table table: dataModel.getSortedTables()) {
			if (ModelBuilder.isJailerTable(Quoting.staticUnquote(table.getUnqualifiedName()))
					|| table.getColumns().isEmpty()) {
				continue;
			}
			checkPrimaryKey(table, findings);
			checkRepeatingGroups(table, findings);
			checkNonAtomicColumns(table, findings);
			List<ForeignKey> foreignKeys = foreignKeys(table);
			checkCopiedColumns(table, foreignKeys, findings);
			checkAllKeyTable(table, foreignKeys, findings);
			checkForeignKeyTypes(table, foreignKeys, findings);
		}
		return findings;
	}

	/**
	 * 1NF: a table without primary key.
	 */
	private void checkPrimaryKey(Table table, List<NormalizationFinding> findings) {
		if (pkNames(table).isEmpty()) {
			findings.add(new NormalizationFinding(table, Collections.<Column>emptyList(), NormalForm.NF1, Severity.LIKELY, RULE_NO_PRIMARY_KEY,
					"The table has no primary key. Without a key, rows cannot be told apart and duplicate rows are possible. "
					+ "Note that the higher normal forms are defined in terms of keys, so the other checks are less meaningful for this table."));
		}
	}

	/**
	 * 1NF: columns that differ only by a number, like PHONE1, PHONE2, PHONE3.
	 */
	private void checkRepeatingGroups(Table table, List<NormalizationFinding> findings) {
		// stem (name with the number replaced by "#") -> index -> columns
		Map<String, Map<Integer, List<Column>>> stems = new LinkedHashMap<String, Map<Integer, List<Column>>>();
		for (Column column: table.getColumns()) {
			String name = norm(column.name);
			Matcher matcher = DIGITS.matcher(name);
			while (matcher.find()) {
				int index;
				try {
					index = Integer.parseInt(matcher.group());
				} catch (NumberFormatException e) {
					continue;
				}
				if (index > MAX_GROUP_INDEX) {
					continue;
				}
				String stem = name.substring(0, matcher.start()) + "#" + name.substring(matcher.end());
				Map<Integer, List<Column>> byIndex = stems.get(stem);
				if (byIndex == null) {
					byIndex = new TreeMap<Integer, List<Column>>();
					stems.put(stem, byIndex);
				}
				List<Column> columns = byIndex.get(index);
				if (columns == null) {
					columns = new ArrayList<Column>();
					byIndex.put(index, columns);
				}
				columns.add(column);
			}
		}

		// stems with the same indexes form one group, e.g. ADDR#_STREET and ADDR#_CITY
		Map<Set<Integer>, Set<Column>> groups = new LinkedHashMap<Set<Integer>, Set<Column>>();
		Map<Set<Integer>, Set<String>> groupStems = new LinkedHashMap<Set<Integer>, Set<String>>();
		for (Map.Entry<String, Map<Integer, List<Column>>> e: stems.entrySet()) {
			TreeSet<Integer> indexes = new TreeSet<Integer>(e.getValue().keySet());
			if (indexes.size() < 2 || indexes.first() > 2) {
				continue;
			}
			Set<Column> columns = groups.get(indexes);
			if (columns == null) {
				columns = new LinkedHashSet<Column>();
				groups.put(indexes, columns);
				groupStems.put(indexes, new TreeSet<String>());
			}
			for (List<Column> c: e.getValue().values()) {
				columns.addAll(c);
			}
			groupStems.get(indexes).add(e.getKey());
		}

		// largest groups first, so that groups contained in a reported one are skipped
		List<Set<Integer>> keys = new ArrayList<Set<Integer>>(groups.keySet());
		Collections.sort(keys, (a, b) -> groups.get(b).size() - groups.get(a).size());
		Set<Column> reported = new HashSet<Column>();
		for (Set<Integer> indexes: keys) {
			Set<Column> columns = groups.get(indexes);
			if (reported.containsAll(columns)) {
				continue;
			}
			reported.addAll(columns);
			StringBuilder stemList = new StringBuilder();
			for (String stem: groupStems.get(indexes)) {
				if (stemList.length() > 0) {
					stemList.append(", ");
				}
				stemList.append(stem);
			}
			findings.add(new NormalizationFinding(table, new ArrayList<Column>(columns), NormalForm.NF1, Severity.LIKELY, RULE_REPEATING_GROUP,
					"The columns differ only by a number (" + stemList + " with # = " + join(indexes) + "). "
					+ "This is a repeating group: the number of values is limited by the number of columns, "
					+ "and queries have to check every column. Consider a separate table with one row per value."));
		}
	}

	/**
	 * 1NF: columns that seem to hold several values.
	 */
	private void checkNonAtomicColumns(Table table, List<NormalizationFinding> findings) {
		for (Column column: table.getColumns()) {
			String type = column.type == null? "" : column.type.toUpperCase(Locale.ENGLISH);
			String name = norm(column.name);
			if (type.contains("ARRAY") || type.endsWith("[]")) {
				findings.add(new NormalizationFinding(table, Collections.singletonList(column), NormalForm.NF1, Severity.POSSIBLE, RULE_ARRAY_COLUMN,
						"The column has an array type (" + column.type + "), so a single field holds several values."));
			} else if ((type.contains("CHAR") || type.contains("TEXT") || type.contains("CLOB") || type.contains("STRING"))
					&& LIST_NAME.matcher(name).matches()) {
				findings.add(new NormalizationFinding(table, Collections.singletonList(column), NormalForm.NF1, Severity.POSSIBLE, RULE_LIST_IN_COLUMN,
						"The name suggests that the column holds a list of values in a single text field, e.g. separated by commas. "
						+ "Such values cannot be joined or constrained. Consider a separate table with one row per value."));
			}
		}
	}

	/**
	 * 2NF/3NF: non-key columns that seem to be copies of non-key columns of a parent table.
	 * If the foreign key is a proper part of the primary key, they depend on a part of the key (2NF),
	 * otherwise they depend on a non-key column (3NF).
	 */
	private void checkCopiedColumns(Table table, List<ForeignKey> foreignKeys, List<NormalizationFinding> findings) {
		Set<String> pk = pkNames(table);
		// a column that is itself a foreign key is a reference of its own, not a copy
		Set<String> allFkNames = fkColumnNames(table);
		for (ForeignKey fk: foreignKeys) {
			Set<String> fkNames = names(fk.columns);
			if (fkNames.equals(pk)) {
				// one-to-one, the copies depend on the key
				continue;
			}
			boolean partOfKey = pk.containsAll(fkNames);
			Set<String> parentPk = pkNames(fk.parent);
			Set<String> parentFkNames = fkColumnNames(fk.parent);
			Map<String, Column> parentColumns = new LinkedHashMap<String, Column>();
			for (Column column: fk.parent.getColumns()) {
				String name = norm(column.name);
				if (!parentPk.contains(name) && !parentFkNames.contains(name) && !TECHNICAL_NAME.matcher(name).matches()) {
					parentColumns.put(name, column);
				}
			}
			List<String> prefixes = tableNamePrefixes(fk.parent);

			Map<Column, Column> copies = new LinkedHashMap<Column, Column>();
			StringBuilder pairs = new StringBuilder();
			for (Column column: table.getColumns()) {
				String name = norm(column.name);
				if (pk.contains(name) || allFkNames.contains(name) || TECHNICAL_NAME.matcher(name).matches()) {
					continue;
				}
				Column original = null;
				// an equal name only counts if it refers to the parent (e.g. ORDER_DATE ~ ORDER.ORDER_DATE).
				// Plain names like AMOUNT or EMAIL occur in many related tables with a meaning of their own
				for (String prefix: prefixes) {
					if (name.contains(prefix)) {
						original = parentColumns.get(name);
						break;
					}
				}
				if (original == null) {
					for (String prefix: prefixes) {
						if (name.startsWith(prefix) && name.length() > prefix.length()) {
							String rest = name.substring(prefix.length());
							if (rest.startsWith("_")) {
								rest = rest.substring(1);
							}
							original = parentColumns.get(rest);
							if (original != null) {
								break;
							}
						}
					}
				}
				if (original != null && !sameType(column, original)) {
					// a different type is a strong sign of a different meaning
					original = null;
				}
				if (original != null) {
					copies.put(column, original);
					if (pairs.length() > 0) {
						pairs.append(", ");
					}
					pairs.append(column.name + " ~ " + fk.parent.getUnqualifiedName() + "." + original.name);
				}
			}
			if (copies.isEmpty()) {
				continue;
			}
			String fkText = join(fk.columns);
			List<Column> copiedColumns = new ArrayList<Column>(copies.keySet());
			NormalizationFinding finding;
			if (partOfKey) {
				finding = new NormalizationFinding(table, copiedColumns, NormalForm.NF2, Severity.POSSIBLE, RULE_PARTIAL_KEY_DEPENDENCY,
						"The columns seem to repeat columns of " + fk.parent.getUnqualifiedName() + " (" + pairs + "). "
						+ "They would then depend on " + fkText + " only, which is just a part of the primary key. "
						+ "Consider removing them and reading the values from " + fk.parent.getUnqualifiedName() + ".");
			} else {
				finding = new NormalizationFinding(table, copiedColumns, NormalForm.NF3, Severity.POSSIBLE, RULE_TRANSITIVE_DEPENDENCY,
						"The columns seem to repeat columns of " + fk.parent.getUnqualifiedName() + " (" + pairs + "). "
						+ "They would then depend on the foreign key " + fkText + " rather than on the primary key. "
						+ "Consider removing them and reading the values from " + fk.parent.getUnqualifiedName() + ".");
			}
			finding.parent = fk.parent;
			finding.fkMapping = fk.mapping;
			finding.copies = copies;
			finding.association = fk.association;
			findings.add(finding);
		}
	}

	/**
	 * 4NF/5NF: a table that consists of its primary key only and links three or more tables.
	 * It may combine independent multi-valued facts in one table.
	 */
	private void checkAllKeyTable(Table table, List<ForeignKey> foreignKeys, List<NormalizationFinding> findings) {
		Set<String> pk = pkNames(table);
		if (table.getColumns().size() < 3 || !pk.equals(names(table.getColumns()))) {
			return;
		}
		Set<Table> parents = new LinkedHashSet<Table>();
		for (ForeignKey fk: foreignKeys) {
			if (pk.containsAll(names(fk.columns))) {
				parents.add(fk.parent);
			}
		}
		if (parents.size() < 3) {
			return;
		}
		StringBuilder parentList = new StringBuilder();
		for (Table parent: parents) {
			if (parentList.length() > 0) {
				parentList.append(", ");
			}
			parentList.append(parent.getUnqualifiedName());
		}
		findings.add(new NormalizationFinding(table, new ArrayList<Column>(table.getColumns()), NormalForm.NF4_5, Severity.POSSIBLE, RULE_ALL_KEY_TABLE,
				"The table consists of its primary key only and links " + parents.size() + " tables (" + parentList + "). "
				+ "If some of these relationships are independent of each other (e.g. the skills and the languages of an employee), "
				+ "the table stores every combination and violates 4NF; if it can be rebuilt by joining its projections, it violates 5NF. "
				+ "Then it should be split into several link tables. Only the data or knowledge of the domain can tell."));
	}

	/**
	 * Gets the foreign keys of a table: equi-join associations from the table to the primary key
	 * of another table.
	 */
	private List<ForeignKey> foreignKeys(Table table) {
		List<ForeignKey> result = new ArrayList<ForeignKey>();
		Set<String> seen = new HashSet<String>();
		for (Association association: table.associations) {
			Table parent = association.destination;
			if (parent == table) {
				continue;
			}
			Set<String> parentPk = pkNames(parent);
			if (parentPk.isEmpty()) {
				continue;
			}
			Map<Column, Column> mapping = association.createSourceToDestinationKeyMapping();
			if (mapping.isEmpty() || !names(mapping.values()).equals(parentPk)) {
				continue;
			}
			if (seen.add(parent.getName() + ":" + new TreeSet<String>(names(mapping.keySet())))) {
				result.add(new ForeignKey(association, mapping, parent));
			}
		}
		return result;
	}

	/**
	 * Normalized names of the columns of all foreign keys of a table, per table.
	 */
	private final Map<Table, Set<String>> fkColumnNamesCache = new HashMap<Table, Set<String>>();

	/**
	 * Gets the normalized names of the columns of all foreign keys of a table.
	 */
	private Set<String> fkColumnNames(Table table) {
		Set<String> result = fkColumnNamesCache.get(table);
		if (result == null) {
			result = new HashSet<String>();
			for (ForeignKey fk: foreignKeys(table)) {
				result.addAll(names(fk.columns));
			}
			fkColumnNamesCache.put(table, result);
		}
		return result;
	}

	/**
	 * Gets the prefixes that a column copied from a table may have, e.g. "CUSTOMER" for CUSTOMERS.NAME
	 * copied as CUSTOMER_NAME.
	 */
	private List<String> tableNamePrefixes(Table table) {
		List<String> prefixes = new ArrayList<String>();
		String name = norm(table.getUnqualifiedName());
		prefixes.add(name);
		if (name.endsWith("IES") && name.length() > 4) {
			prefixes.add(name.substring(0, name.length() - 3) + "Y");
		} else if (name.endsWith("S") && name.length() > 2) {
			prefixes.add(name.substring(0, name.length() - 1));
		}
		return prefixes;
	}

	/**
	 * Consistency: foreign key columns whose type, length or precision differs from the referenced column.
	 */
	private void checkForeignKeyTypes(Table table, List<ForeignKey> foreignKeys, List<NormalizationFinding> findings) {
		for (ForeignKey fk: foreignKeys) {
			Map<Column, Column> mismatches = new LinkedHashMap<Column, Column>();
			boolean typeDiffers = false;
			StringBuilder pairs = new StringBuilder();
			for (Map.Entry<Column, Column> e: fk.mapping.entrySet()) {
				Column column = e.getKey();
				Column referenced = e.getValue();
				if (column.type == null || referenced.type == null || IGNORED_FK_TYPES.contains(typeName(column))) {
					continue;
				}
				boolean differentType = !canonicalTypeName(column).equals(canonicalTypeName(referenced));
				boolean differentSize = column.length != referenced.length || column.precision != referenced.precision;
				if (differentType || differentSize) {
					typeDiffers |= differentType;
					mismatches.put(column, referenced);
					if (pairs.length() > 0) {
						pairs.append(", ");
					}
					pairs.append(column.name + " " + typeOf(column) + " vs. " + fk.parent.getUnqualifiedName() + "." + referenced.name + " " + typeOf(referenced));
				}
			}
			if (mismatches.isEmpty()) {
				continue;
			}
			NormalizationFinding finding = new NormalizationFinding(table, new ArrayList<Column>(mismatches.keySet()), NormalForm.CONSISTENCY,
					typeDiffers? Severity.LIKELY : Severity.POSSIBLE, RULE_FK_TYPE_MISMATCH,
					"The foreign key " + (mismatches.size() == 1? "column differs" : "columns differ") + " from the referenced primary key ("
					+ pairs + "). "
					+ (typeDiffers? "Joins need implicit conversions, which can prevent the use of indexes. "
							: "Values of the referenced column may not fit into the foreign key column, or the declared sizes are just inconsistent. ")
					+ "Consider declaring the foreign key " + (mismatches.size() == 1? "column" : "columns") + " with the type of the referenced "
					+ (mismatches.size() == 1? "column" : "columns") + ".");
			finding.parent = fk.parent;
			// the whole foreign key; the mismatched columns are the finding's columns
			finding.fkMapping = fk.mapping;
			finding.association = fk.association;
			findings.add(finding);
		}
	}

	/**
	 * Types of foreign key columns for which no type mismatch is reported.
	 */
	private static final Set<String> IGNORED_FK_TYPES = new HashSet<String>(java.util.Arrays.asList("ROLE"));

	/**
	 * Names of the same type, mapped to one name.
	 */
	private static final Map<String, String> TYPE_SYNONYMS = new HashMap<String, String>();
	static {
		for (String[] synonyms: new String[][] {
				{ "INTEGER", "INT", "INT4" },
				{ "BIGINT", "INT8" },
				{ "SMALLINT", "INT2" },
				{ "VARCHAR", "VARCHAR2", "CHARACTER VARYING" },
				{ "NVARCHAR", "NVARCHAR2", "NATIONAL CHARACTER VARYING" },
				{ "CHAR", "CHARACTER" },
				{ "NUMERIC", "NUMBER", "DECIMAL", "DEC" },
				{ "DOUBLE", "DOUBLE PRECISION", "FLOAT8" },
				{ "REAL", "FLOAT4" },
				{ "BOOLEAN", "BOOL" } }) {
			for (String synonym: synonyms) {
				TYPE_SYNONYMS.put(synonym, synonyms[0]);
			}
		}
	}

	/**
	 * Gets the type name of a column, with synonyms mapped to one name.
	 */
	private String canonicalTypeName(Column column) {
		String name = typeName(column);
		String canonical = TYPE_SYNONYMS.get(name);
		return canonical == null? name : canonical;
	}

	/**
	 * Gets the type of a column including length and precision, like {@link Column#toSQL(String)}
	 * but without the name.
	 *
	 * @param column the column
	 * @return the type, empty if unknown
	 */
	static String typeOf(Column column) {
		if (column.type == null) {
			return "";
		}
		return column.type + (column.length <= 0? "" : column.length == Integer.MAX_VALUE? "(max)" :
			"(" + column.length + (column.precision >= 0? ", " + column.precision : "") + ")");
	}

	/**
	 * Compares the type names of two columns, ignoring case, length and precision.
	 * Columns of unknown type are considered to be of the same type.
	 */
	private boolean sameType(Column a, Column b) {
		if (a.type == null || b.type == null) {
			return true;
		}
		return typeName(a).equals(typeName(b));
	}

	private String typeName(Column column) {
		String type = column.type.toUpperCase(Locale.ENGLISH);
		int i = type.indexOf('(');
		if (i >= 0) {
			type = type.substring(0, i);
		}
		return type.trim();
	}

	private Set<String> pkNames(Table table) {
		if (table.primaryKey == null || table.primaryKey.getColumns() == null) {
			return Collections.emptySet();
		}
		return names(table.primaryKey.getColumns());
	}

	private Set<String> names(Iterable<Column> columns) {
		Set<String> names = new HashSet<String>();
		for (Column column: columns) {
			names.add(norm(column.name));
		}
		return names;
	}

	private String norm(String name) {
		return Quoting.normalizeIdentifier(name);
	}

	private String join(Iterable<?> values) {
		StringBuilder sb = new StringBuilder();
		for (Object value: values) {
			if (sb.length() > 0) {
				sb.append(", ");
			}
			sb.append(value instanceof Column? ((Column) value).name : String.valueOf(value));
		}
		return sb.toString();
	}

}
