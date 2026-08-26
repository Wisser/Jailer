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
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.modelbuilder.ModelBuilder;
import net.sf.jailer.ui.associationdiscovery.AssociationCandidate.Evidence;
import net.sf.jailer.ui.associationdiscovery.ColumnTypes.Category;
import net.sf.jailer.ui.associationdiscovery.NamingRule.Match;
import net.sf.jailer.ui.associationdiscovery.TableProfile.ColumnProfile;
import net.sf.jailer.ui.associationproposer.AssociationProposer;
import net.sf.jailer.util.Pair;
import net.sf.jailer.util.Quoting;

/**
 * Finds potential associations without evaluating foreign key constraints:
 * by the naming rules of the user, by the built-in naming conventions, or - if profiles
 * are available - by scanning the data. Does not access the database.
 *
 * @author Ralf Wisser
 */
public class AssociationCandidateFinder {

	/**
	 * Minimum number of distinct values a column must have to be considered a
	 * foreign key by the data scan alone. Flags and constants only create noise.
	 */
	private static final long MIN_DISTINCT_COUNT = 3;

	/**
	 * Column names that carry no information about their table. If a primary key
	 * consists of such names only, columns of the same name in other tables are not
	 * regarded as a reference to it.
	 */
	private static final Set<String> GENERIC_NAMES = new HashSet<String>(Arrays.asList(
			"ID", "NO", "NR", "NUM", "NUMBER", "KEY", "PK", "CODE", "SEQ", "NAME", "TYPE", "STATUS", "VERSION"));

	/**
	 * Number of tables from which on a primary key that is shared by that many tables
	 * is regarded as a house-wide convention rather than as a key that is referenced
	 * by name. See {@link #isHouseWidePrimaryKey(List)}.
	 */
	private static final int SHARED_PK_MIN_TABLES = 20;

	/**
	 * Number of candidate columns the data scan keeps per primary key column, the most
	 * plausible ones first. Bounds the number of column combinations of a composite key.
	 */
	private static final int MAX_SURVIVORS_PER_KEY_COLUMN = 3;

	/**
	 * Number of column combinations the data scan checks per pair of tables.
	 */
	private static final int MAX_COLUMN_COMBINATIONS = 4;

	private final DataModel dataModel;
	private final List<Table> tables;
	private final NamingRules namingRules;

	/**
	 * Counts how many tables have a primary key of exactly these (normalized) column names.
	 */
	private final Map<Set<String>, Integer> pkNameSetCount = new HashMap<Set<String>, Integer>();

	/**
	 * Maps a normalized name variant of a table onto the tables having it. Used to resolve
	 * the table a regular expression matched.
	 */
	private final Map<String, List<Table>> tablesByName = new HashMap<String, List<Table>>();

	/**
	 * Per table: normalized column name -> column. Built once, so that the name matching
	 * does not have to normalize every column name of every table again for every other
	 * table of the model.
	 */
	private final Map<Table, Map<String, Column>> columnsByName = new HashMap<Table, Map<String, Column>>();

	/**
	 * Per table: normalized column name without the affixes of a foreign key -> column.
	 */
	private final Map<Table, Map<String, Column>> columnsByStrippedName = new HashMap<Table, Map<String, Column>>();

	/**
	 * Number of table pairs for which the data scan did not check all column combinations.
	 */
	private int partiallyCheckedPairs;

	/**
	 * Set while a pair of tables is processed, if not all combinations are checked.
	 */
	private boolean pairTruncated;

	/**
	 * @param dataModel the data model
	 * @param namingRules the naming rules of the user
	 */
	public AssociationCandidateFinder(DataModel dataModel, NamingRules namingRules) {
		this.dataModel = dataModel;
		this.namingRules = namingRules;
		this.tables = new ArrayList<Table>();
		for (Table table: dataModel.getSortedTables()) {
			if (!ModelBuilder.isJailerTable(Quoting.staticUnquote(table.getUnqualifiedName()))
					&& !table.getColumns().isEmpty()) {
				tables.add(table);
			}
		}
		for (Table table: tables) {
			if (table.primaryKey != null && table.primaryKey.getColumns() != null && !table.primaryKey.getColumns().isEmpty()) {
				Set<String> names = normalizedNames(table.primaryKey.getColumns());
				Integer count = pkNameSetCount.get(names);
				pkNameSetCount.put(names, count == null? 1 : count + 1);
			}
			for (String name: baseNames(table)) {
				List<Table> byName = tablesByName.get(name);
				if (byName == null) {
					byName = new ArrayList<Table>();
					tablesByName.put(name, byName);
				}
				byName.add(table);
			}
			Map<String, Column> byColumnName = new HashMap<String, Column>();
			Map<String, Column> byStrippedName = new HashMap<String, Column>();
			for (Column column: table.getColumns()) {
				String name = Quoting.normalizeIdentifier(column.name);
				if (!byColumnName.containsKey(name)) {
					byColumnName.put(name, column);
				}
				String stripped = stripAffixes(name);
				if (!stripped.equals(name) && !byStrippedName.containsKey(stripped)) {
					byStrippedName.put(stripped, column);
				}
			}
			columnsByName.put(table, byColumnName);
			columnsByStrippedName.put(table, byStrippedName);
		}
	}

	/**
	 * Gets the tables taking part in the discovery.
	 *
	 * @return the tables
	 */
	public List<Table> getTables() {
		return tables;
	}

	/**
	 * Gets the number of table pairs for which the data scan checked only some of the
	 * possible column combinations.
	 *
	 * @return the number of pairs
	 */
	public int getPartiallyCheckedPairs() {
		return partiallyCheckedPairs;
	}

	/**
	 * Finds candidates whose columns match a naming rule of the user or a built-in
	 * naming convention.
	 *
	 * @param log receives the progress and tells whether the run has been cancelled
	 * @return the candidates
	 */
	public List<AssociationCandidate> findByName(DiscoveryLog log) {
		List<AssociationCandidate> result = new ArrayList<AssociationCandidate>();
		int done = 0;
		boolean builtIn = namingRules.isUseBuiltInRules();
		List<NamingRule> templates = namingRules.getTemplates();
		for (Table parent: tables) {
			log.progress(++done, tables.size());
			if (log.isCancelled()) {
				return result;
			}
			List<Column> pkColumns = primaryKeyOf(parent);
			if (pkColumns == null) {
				continue;
			}
			List<Map<String, NamingRule>> userRules = new ArrayList<Map<String, NamingRule>>();
			List<Set<String>> builtInNames = new ArrayList<Set<String>>();
			boolean specific = false;
			for (Column pkColumn: pkColumns) {
				userRules.add(templateRules(parent, pkColumn, templates));
				builtInNames.add(builtIn? patternNames(parent, pkColumn) : Collections.<String>emptySet());
				if (!isGenericName(Quoting.normalizeIdentifier(pkColumn.name))) {
					specific = true;
				}
			}
			if (!builtIn || isHouseWidePrimaryKey(pkColumns)) {
				// name equality is a built-in heuristic itself, and it says nothing at all
				// about a key that most tables of the model carry
				specific = false;
			}
			for (Table child: tables) {
				AssociationCandidate candidate = matchByName(child, parent, pkColumns, userRules, builtInNames, specific);
				if (candidate != null) {
					result.add(candidate);
				}
			}
		}
		return result;
	}

	/**
	 * Finds candidates by applying the regular expressions of the user to the column names.
	 * Driven by the columns, not by the pairs of tables: a regular expression is evaluated
	 * once per column, and the table it names is looked up.
	 *
	 * @param excludedKeys keys of candidates that have already been found
	 * @param log receives the progress and tells whether the run has been cancelled
	 * @return the candidates
	 */
	public List<AssociationCandidate> findByRegexRules(Set<String> excludedKeys, DiscoveryLog log) {
		List<NamingRule> regexRules = namingRules.getRegexRules();
		List<AssociationCandidate> result = new ArrayList<AssociationCandidate>();
		if (regexRules.isEmpty()) {
			return result;
		}
		int done = 0;
		for (Table child: tables) {
			log.progress(++done, tables.size());
			if (log.isCancelled()) {
				return result;
			}
			// parent -> (primary key column -> column of the child)
			Map<Table, Map<Column, Column>> hits = new LinkedHashMap<Table, Map<Column, Column>>();
			Set<Table> completing = new HashSet<Table>();
			Map<Table, List<NamingRule>> contributing = new HashMap<Table, List<NamingRule>>();
			for (Column column: child.getColumns()) {
				String name = Quoting.normalizeIdentifier(column.name);
				for (NamingRule rule: regexRules) {
					Match match = rule.match(name);
					if (match == null) {
						continue;
					}
					List<Table> parents = tablesByName.get(match.table);
					if (parents == null) {
						continue;
					}
					for (Table parent: parents) {
						List<Column> pkColumns = primaryKeyOf(parent);
						if (pkColumns == null) {
							continue;
						}
						if (match.pk != null) {
							for (Column pkColumn: pkColumns) {
								if (Quoting.normalizeIdentifier(pkColumn.name).equals(match.pk)
										&& record(hits, parent, pkColumn, column, child)) {
									contribute(contributing, parent, rule);
									if (rule.isCompleteByName()) {
										completing.add(parent);
									}
								}
							}
						} else if (pkColumns.size() == 1) {
							if (record(hits, parent, pkColumns.get(0), column, child)) {
								contribute(contributing, parent, rule);
								if (rule.isCompleteByName()) {
									completing.add(parent);
								}
							}
						}
					}
				}
			}
			for (Entry<Table, Map<Column, Column>> e: hits.entrySet()) {
				Table parent = e.getKey();
				List<Column> pkColumns = primaryKeyOf(parent);
				if (pkColumns == null) {
					continue;
				}
				if (e.getValue().size() != pkColumns.size() && completing.contains(parent)) {
					completeByName(child, pkColumns, e.getValue());
				}
				if (e.getValue().size() != pkColumns.size()) {
					// the key is not covered completely
					continue;
				}
				List<Column> childColumns = new ArrayList<Column>();
				for (Column pkColumn: pkColumns) {
					childColumns.add(e.getValue().get(pkColumn));
				}
				AssociationCandidate candidate = new AssociationCandidate(child, parent, childColumns, pkColumns, Evidence.USER_RULE);
				candidate.withoutDataCheck = withoutDataCheck(contributing.get(parent), false);
				if (!excludedKeys.contains(candidate.key())) {
					result.add(candidate);
				}
			}
		}
		return result;
	}

	/**
	 * Records that a column of the child matches a primary key column of the parent.
	 * The first match per primary key column wins, and a column is not used twice.
	 *
	 * @return <code>true</code> if the match has been recorded
	 */
	private boolean record(Map<Table, Map<Column, Column>> hits, Table parent, Column pkColumn, Column column, Table child) {
		if (parent == child && Quoting.normalizeIdentifier(pkColumn.name).equals(Quoting.normalizeIdentifier(column.name))) {
			// the column matched itself
			return false;
		}
		if (!ColumnTypes.areCompatible(column, pkColumn)) {
			return false;
		}
		Map<Column, Column> mapping = hits.get(parent);
		if (mapping == null) {
			mapping = new LinkedHashMap<Column, Column>();
			hits.put(parent, mapping);
		}
		if (mapping.containsKey(pkColumn) || mapping.containsValue(column)) {
			return false;
		}
		mapping.put(pkColumn, column);
		return true;
	}

	/**
	 * Notes that a rule contributed a column to the mapping of a parent.
	 */
	private void contribute(Map<Table, List<NamingRule>> contributing, Table parent, NamingRule rule) {
		List<NamingRule> rules = contributing.get(parent);
		if (rules == null) {
			rules = new ArrayList<NamingRule>();
			contributing.put(parent, rules);
		}
		rules.add(rule);
	}

	/**
	 * Fills the primary key columns that are still unmapped with columns of the child that
	 * have the identical name. Used for keys that carry a tenant or a version column, which
	 * is named alike in both tables.
	 *
	 * @param child the child table
	 * @param pkColumns the primary key columns of the parent
	 * @param mapping the mapping so far, extended in place
	 */
	private void completeByName(Table child, List<Column> pkColumns, Map<Column, Column> mapping) {
		for (Column pkColumn: pkColumns) {
			if (mapping.containsKey(pkColumn)) {
				continue;
			}
			String pkName = Quoting.normalizeIdentifier(pkColumn.name);
			for (Column column: child.getColumns()) {
				if (!ColumnTypes.areCompatible(column, pkColumn)) {
					continue;
				}
				if (Quoting.normalizeIdentifier(column.name).equals(pkName) && !mapping.containsValue(column)) {
					mapping.put(pkColumn, column);
					break;
				}
			}
		}
	}

	/**
	 * Finds candidates that have no name evidence at all, based on the value ranges and
	 * distinct counts of the profiles. Composite keys are supported: the pre-filter is
	 * applied per key column, and only the surviving columns are combined.
	 *
	 * @param profiles the table profiles
	 * @param excludedKeys keys of candidates that have already been found
	 * @return the candidates, the most promising ones first
	 */
	public List<AssociationCandidate> findByData(Map<Table, TableProfile> profiles, Set<String> excludedKeys) {
		List<AssociationCandidate> result = new ArrayList<AssociationCandidate>();
		final Map<AssociationCandidate, Double> rank = new IdentityHashMap<AssociationCandidate, Double>();
		for (Table parent: tables) {
			List<Column> pkColumns = primaryKeyOf(parent);
			if (pkColumns == null) {
				continue;
			}
			TableProfile parentProfile = profiles.get(parent);
			if (parentProfile == null || parentProfile.failed) {
				continue;
			}
			boolean houseWide = isHouseWidePrimaryKey(pkColumns);
			Set<String> parentNames = baseNames(parent);
			for (Table child: tables) {
				TableProfile childProfile = profiles.get(child);
				if (childProfile == null || childProfile.failed) {
					continue;
				}
				pairTruncated = false;
				List<List<Column>> survivors = survivors(child, parent, pkColumns, childProfile, parentProfile);
				if (survivors == null) {
					continue;
				}
				List<List<Column>> assignments = assignments(survivors, child, pkColumns, childProfile, houseWide, parentNames);
				if (pairTruncated) {
					++partiallyCheckedPairs;
				}
				for (List<Column> childColumns: assignments) {
					AssociationCandidate candidate = new AssociationCandidate(child, parent, childColumns, pkColumns, Evidence.DATA_ONLY);
					if (excludedKeys.contains(candidate.key())) {
						continue;
					}
					result.add(candidate);
					rank.put(candidate, coverage(childColumns, childProfile, parentProfile));
				}
			}
		}
		Collections.sort(result, new Comparator<AssociationCandidate>() {
			@Override
			public int compare(AssociationCandidate o1, AssociationCandidate o2) {
				return Double.compare(rank.get(o2), rank.get(o1));
			}
		});
		return result;
	}

	/**
	 * Collects, per primary key column, the columns of the child that can hold its values.
	 * The most plausible ones come first and the list is truncated, so that the number of
	 * combinations of a composite key stays manageable.
	 *
	 * @return one list per primary key column, or <code>null</code> if one of them is empty
	 */
	private List<List<Column>> survivors(Table child, Table parent, List<Column> pkColumns,
			final TableProfile childProfile, TableProfile parentProfile) {
		List<List<Column>> survivors = new ArrayList<List<Column>>();
		boolean truncated = false;
		for (final Column pkColumn: pkColumns) {
			ColumnProfile pkProfile = parentProfile.get(pkColumn);
			if (pkProfile == null) {
				return null;
			}
			List<Column> columns = new ArrayList<Column>();
			for (Column column: child.getColumns()) {
				if (accepts(child, parent, column, pkColumn, childProfile, parentProfile, pkProfile)) {
					columns.add(column);
				}
			}
			if (columns.isEmpty()) {
				return null;
			}
			Collections.sort(columns, new Comparator<Column>() {
				@Override
				public int compare(Column o1, Column o2) {
					boolean a1 = hasNameAffinity(o1, pkColumn);
					boolean a2 = hasNameAffinity(o2, pkColumn);
					if (a1 != a2) {
						return a1? -1 : 1;
					}
					return Long.compare(distinctCount(o2, childProfile), distinctCount(o1, childProfile));
				}
			});
			if (columns.size() > MAX_SURVIVORS_PER_KEY_COLUMN) {
				columns = new ArrayList<Column>(columns.subList(0, MAX_SURVIVORS_PER_KEY_COLUMN));
				truncated = true;
			}
			survivors.add(columns);
		}
		if (truncated) {
			pairTruncated = true;
		}
		return survivors;
	}

	/**
	 * Checks whether a column of the child can hold the values of a primary key column,
	 * as far as the profiles tell.
	 */
	private boolean accepts(Table child, Table parent, Column column, Column pkColumn,
			TableProfile childProfile, TableProfile parentProfile, ColumnProfile pkProfile) {
		if (!ColumnTypes.areCompatible(column, pkColumn)) {
			return false;
		}
		if (child == parent && Quoting.normalizeIdentifier(column.name).equals(Quoting.normalizeIdentifier(pkColumn.name))) {
			return false;
		}
		ColumnProfile columnProfile = childProfile.get(column);
		if (columnProfile == null || columnProfile.nonNullCount <= 0) {
			return false;
		}
		if (columnProfile.distinctCount >= 0 && columnProfile.distinctCount < MIN_DISTINCT_COUNT) {
			return false;
		}
		if (!childProfile.sampled && !parentProfile.sampled
				&& columnProfile.distinctCount >= 0 && parentProfile.rowCount >= 0
				&& columnProfile.distinctCount > parentProfile.rowCount) {
			return false;
		}
		if (!childProfile.sampled && !parentProfile.sampled
				&& columnProfile.hasRange() && pkProfile.hasRange()
				&& (columnProfile.min < pkProfile.min || columnProfile.max > pkProfile.max)) {
			return false;
		}
		return true;
	}

	/**
	 * Combines the surviving columns into assignments for the whole key: one column per
	 * primary key column, pairwise distinct, the most plausible assignments first and
	 * limited in number.
	 *
	 * @return the assignments, each in the order of the primary key columns
	 */
	private List<List<Column>> assignments(List<List<Column>> survivors, Table child,
			List<Column> pkColumns, final TableProfile childProfile, boolean houseWide, Set<String> parentNames) {
		List<List<Column>> assignments = new ArrayList<List<Column>>();
		assignments.add(new ArrayList<Column>());
		for (List<Column> columns: survivors) {
			List<List<Column>> extended = new ArrayList<List<Column>>();
			for (List<Column> assignment: assignments) {
				for (Column column: columns) {
					if (assignment.contains(column)) {
						continue;
					}
					List<Column> next = new ArrayList<Column>(assignment);
					next.add(column);
					extended.add(next);
				}
			}
			assignments = extended;
		}
		Set<String> ownKey = child.primaryKey == null || child.primaryKey.getColumns() == null? null
				: normalizedNames(child.primaryKey.getColumns());
		List<List<Column>> accepted = new ArrayList<List<Column>>();
		for (List<Column> assignment: assignments) {
			if (assignment.size() != pkColumns.size()) {
				continue;
			}
			if (houseWide && ownKey != null && ownKey.equals(normalizedNames(assignment))) {
				// in a model whose tables all share one key, this would propose every table
				// as a reference to every other one
				continue;
			}
			if (houseWide && !namesParent(assignment, parentNames)) {
				// same reason: if most tables of the model carry this key, its values appear
				// everywhere. At least one column has to name the parent table, otherwise
				// every pair of tables would become a candidate
				continue;
			}
			accepted.add(assignment);
		}
		final List<Column> keyColumns = pkColumns;
		Collections.sort(accepted, new Comparator<List<Column>>() {
			@Override
			public int compare(List<Column> o1, List<Column> o2) {
				int a1 = affinityCount(o1, keyColumns);
				int a2 = affinityCount(o2, keyColumns);
				if (a1 != a2) {
					return a2 - a1;
				}
				return Long.compare(minDistinctCount(o2, childProfile), minDistinctCount(o1, childProfile));
			}
		});
		if (accepted.size() > MAX_COLUMN_COMBINATIONS) {
			pairTruncated = true;
			accepted = accepted.subList(0, MAX_COLUMN_COMBINATIONS);
		}
		return accepted;
	}

	/**
	 * Checks whether at least one column of the assignment names the parent table.
	 *
	 * @param childColumns the columns of the child
	 * @param parentNames the name variants of the parent table
	 * @return <code>true</code> if one of the column names contains one of the table names
	 */
	private boolean namesParent(List<Column> childColumns, Set<String> parentNames) {
		for (Column column: childColumns) {
			String name = Quoting.normalizeIdentifier(column.name);
			for (String parentName: parentNames) {
				if (name.contains(parentName)) {
					return true;
				}
			}
		}
		return false;
	}

	/**
	 * Number of columns of an assignment whose name is related to the name of the
	 * primary key column they are mapped onto.
	 */
	private int affinityCount(List<Column> childColumns, List<Column> pkColumns) {
		int count = 0;
		for (int i = 0; i < pkColumns.size() && i < childColumns.size(); ++i) {
			if (hasNameAffinity(childColumns.get(i), pkColumns.get(i))) {
				++count;
			}
		}
		return count;
	}

	/**
	 * Checks whether one of the two column names contains the other.
	 */
	private boolean hasNameAffinity(Column column, Column pkColumn) {
		String name = Quoting.normalizeIdentifier(column.name);
		String pkName = Quoting.normalizeIdentifier(pkColumn.name);
		return name.contains(pkName) || pkName.contains(name);
	}

	private long distinctCount(Column column, TableProfile profile) {
		ColumnProfile columnProfile = profile.get(column);
		return columnProfile == null? -1 : columnProfile.distinctCount;
	}

	private long minDistinctCount(List<Column> columns, TableProfile profile) {
		long min = Long.MAX_VALUE;
		for (Column column: columns) {
			min = Math.min(min, distinctCount(column, profile));
		}
		return min == Long.MAX_VALUE? -1 : min;
	}

	/**
	 * Fraction of the parent rows that the values of the assignment could cover. Used to
	 * put the most promising candidates first.
	 */
	private double coverage(List<Column> childColumns, TableProfile childProfile, TableProfile parentProfile) {
		if (parentProfile.rowCount <= 0) {
			return 0;
		}
		long min = minDistinctCount(childColumns, childProfile);
		return min < 0? 0 : (double) min / parentProfile.rowCount;
	}

	/**
	 * Removes candidates that are already covered by an association of the data model,
	 * as well as duplicates.
	 *
	 * @param candidates the candidates, modified in place
	 * @return the removed candidates that are covered by the data model
	 */
	public List<AssociationCandidate> removeKnown(List<AssociationCandidate> candidates) {
		List<AssociationCandidate> known = new ArrayList<AssociationCandidate>();
		AssociationProposer proposer = new AssociationProposer(dataModel);
		int i = 0;
		for (AssociationCandidate candidate: new ArrayList<AssociationCandidate>(candidates)) {
			Association association = new Association(candidate.child, candidate.parent, false, false,
					candidate.getCondition(), dataModel, false, null, "Association Discovery");
			if (!proposer.addAssociation("AD" + (++i), new Pair<Table, Table>(candidate.child, candidate.parent), association, true)) {
				candidates.remove(candidate);
				if (!proposer.pickUpKnownAssociations().isEmpty()) {
					known.add(candidate);
				}
			}
		}
		return known;
	}

	/**
	 * Gets the primary key columns of a table, if all of them can be part of a foreign key.
	 *
	 * @param table the table
	 * @return the primary key columns, or <code>null</code>
	 */
	private List<Column> primaryKeyOf(Table table) {
		if (table.primaryKey == null) {
			return null;
		}
		List<Column> pkColumns = table.primaryKey.getColumns();
		if (pkColumns == null || pkColumns.isEmpty()) {
			return null;
		}
		for (Column column: pkColumns) {
			if (ColumnTypes.categoryOf(column) == Category.OTHER) {
				return null;
			}
		}
		return pkColumns;
	}

	/**
	 * Tries to map the primary key columns of the parent onto columns of the child,
	 * based on their names. Two passes: first what a naming rule of the user matches,
	 * then the remaining key columns.
	 *
	 * @param userRules the accepted names with the rule that accepts them, per primary key column
	 * @param builtInNames the names accepted by a built-in convention, per primary key column
	 * @param allowExact if <code>false</code>, a column of the same name is no evidence.
	 *        This is the case for primary keys that consist of generic names only, for keys
	 *        that most tables of the model share, and when the built-in conventions are off
	 * @return the candidate, or <code>null</code> if no complete mapping exists
	 */
	private AssociationCandidate matchByName(Table child, Table parent, List<Column> pkColumns,
			List<Map<String, NamingRule>> userRules, List<Set<String>> builtInNames, boolean allowExact) {
		Map<String, Column> byName = columnsByName.get(child);
		Map<String, Column> byStrippedName = columnsByStrippedName.get(child);
		if (byName == null) {
			return null;
		}
		List<Column> childColumns = new ArrayList<Column>(Collections.<Column>nCopies(pkColumns.size(), null));
		Set<Column> usedChildColumns = new HashSet<Column>();
		List<NamingRule> matchedRules = new ArrayList<NamingRule>();
		int exactMatches = 0;
		boolean anyBuiltInPattern = false;

		// what the rules of the user match. A rule of the user is taken literally,
		// without the built-in affixes
		for (int p = 0; p < pkColumns.size(); ++p) {
			RuleMatch match = firstRuleMatch(byName, pkColumns.get(p), userRules.get(p), usedChildColumns);
			if (match != null) {
				childColumns.set(p, match.column);
				usedChildColumns.add(match.column);
				matchedRules.add(match.rule);
			}
		}

		// the remaining key columns. A rule that completes the key licenses name equality
		// even where it would be suppressed otherwise: the rule has already shown that the
		// child refers to this parent, so the ambiguity that name equality would cause is gone
		boolean anyCompleting = false;
		for (NamingRule rule: matchedRules) {
			if (rule.isCompleteByName()) {
				anyCompleting = true;
				break;
			}
		}
		boolean exactAllowed = allowExact || anyCompleting;
		for (int p = 0; p < pkColumns.size(); ++p) {
			if (childColumns.get(p) != null) {
				continue;
			}
			Column pkColumn = pkColumns.get(p);
			Column matched = null;
			boolean exact = false;
			if (exactAllowed) {
				Column column = byName.get(Quoting.normalizeIdentifier(pkColumn.name));
				if (acceptable(column, pkColumn, usedChildColumns)) {
					matched = column;
					exact = true;
				}
			}
			if (matched == null) {
				matched = firstMatch(byName, pkColumn, builtInNames.get(p), usedChildColumns);
				if (matched == null) {
					matched = firstMatch(byStrippedName, pkColumn, builtInNames.get(p), usedChildColumns);
				}
			}
			if (matched == null) {
				return null;
			}
			if (exact) {
				++exactMatches;
			} else {
				anyBuiltInPattern = true;
			}
			childColumns.set(p, matched);
			usedChildColumns.add(matched);
		}
		if (child == parent && exactMatches == pkColumns.size()) {
			// the primary key of the table matched against itself
			return null;
		}
		// the weakest signal that took part decides
		Evidence evidence = anyBuiltInPattern? Evidence.NAME_PATTERN
				: (!matchedRules.isEmpty()? Evidence.USER_RULE : Evidence.NAME_EXACT);
		AssociationCandidate candidate = new AssociationCandidate(child, parent, childColumns, pkColumns, evidence);
		candidate.withoutDataCheck = withoutDataCheck(matchedRules, anyBuiltInPattern);
		return candidate;
	}

	/**
	 * Decides whether a candidate is accepted without being checked against the data.
	 * Deliberately conservative: at least one of the rules that matched has to say so, none
	 * of them may contradict, and no column may have been found by a built-in convention.
	 * A key column that was completed by identical name is no contradiction - the rule that
	 * licensed the completion has already spoken.
	 *
	 * @param matchedRules the rules that matched
	 * @param anyBuiltInPattern whether a built-in convention took part
	 * @return <code>true</code> if no verification query is to be run
	 */
	private boolean withoutDataCheck(List<NamingRule> matchedRules, boolean anyBuiltInPattern) {
		if (anyBuiltInPattern || matchedRules == null || matchedRules.isEmpty()) {
			return false;
		}
		for (NamingRule rule: matchedRules) {
			if (!rule.isWithoutDataCheck()) {
				return false;
			}
		}
		return true;
	}

	/**
	 * A column of the child together with the rule that accepted it.
	 */
	private static class RuleMatch {

		final Column column;
		final NamingRule rule;

		RuleMatch(Column column, NamingRule rule) {
			this.column = column;
			this.rule = rule;
		}
	}

	/**
	 * Looks the names accepted by the rules up in the columns of the child.
	 *
	 * @param byName the columns of the child, by their normalized name
	 * @param pkColumn the primary key column to be matched
	 * @param rules accepted name -> accepting rule, in the order of the rules
	 * @param usedChildColumns the columns that are taken already
	 * @return the match, or <code>null</code>
	 */
	private RuleMatch firstRuleMatch(Map<String, Column> byName, Column pkColumn,
			Map<String, NamingRule> rules, Set<Column> usedChildColumns) {
		if (rules.isEmpty() || byName.isEmpty()) {
			return null;
		}
		for (Entry<String, NamingRule> e: rules.entrySet()) {
			Column column = byName.get(e.getKey());
			if (acceptable(column, pkColumn, usedChildColumns)) {
				return new RuleMatch(column, e.getValue());
			}
		}
		return null;
	}

	/**
	 * Looks the accepted names up in the columns of the child. Runs over the names, not over
	 * the columns: the names are few and the map is built once per table.
	 *
	 * @param byName the columns of the child, by their normalized name
	 * @param pkColumn the primary key column to be matched
	 * @param names the accepted, normalized names, most specific first
	 * @param usedChildColumns the columns that are taken already
	 * @return the column, or <code>null</code>
	 */
	private Column firstMatch(Map<String, Column> byName, Column pkColumn, Set<String> names, Set<Column> usedChildColumns) {
		if (names.isEmpty() || byName.isEmpty()) {
			return null;
		}
		for (String name: names) {
			Column column = byName.get(name);
			if (acceptable(column, pkColumn, usedChildColumns)) {
				return column;
			}
		}
		return null;
	}

	/**
	 * Checks whether the found column can be mapped onto the primary key column.
	 *
	 * @param column the column, may be <code>null</code>
	 * @param pkColumn the primary key column
	 * @param usedChildColumns the columns that are taken already
	 * @return <code>true</code> if the column can be used
	 */
	private boolean acceptable(Column column, Column pkColumn, Set<Column> usedChildColumns) {
		return column != null && !usedChildColumns.contains(column) && ColumnTypes.areCompatible(column, pkColumn);
	}

	/**
	 * Expands the templates of the user for the given primary key column.
	 *
	 * @param parent the parent table
	 * @param pkColumn the primary key column
	 * @param templates the templates
	 * @return the accepted, normalized names
	 */
	private Map<String, NamingRule> templateRules(Table parent, Column pkColumn, List<NamingRule> templates) {
		if (templates.isEmpty()) {
			return Collections.emptyMap();
		}
		String pkName = Quoting.normalizeIdentifier(pkColumn.name);
		Map<String, NamingRule> names = new LinkedHashMap<String, NamingRule>();
		for (String base: baseNames(parent)) {
			for (NamingRule template: templates) {
				String name = template.expand(base, pkName);
				if (name != null && name.length() > 0 && !names.containsKey(name)) {
					// the first rule that accepts a name owns it
					names.put(name, template);
				}
			}
		}
		return names;
	}

	/**
	 * Builds the set of column names that are accepted as a reference to the given
	 * primary key column by a built-in naming convention.
	 *
	 * @param parent the parent table
	 * @param pkColumn the primary key column
	 * @return the accepted, normalized names
	 */
	private Set<String> patternNames(Table parent, Column pkColumn) {
		String pkName = Quoting.normalizeIdentifier(pkColumn.name);
		Set<String> names = new LinkedHashSet<String>();
		for (String base: baseNames(parent)) {
			names.add(base + "_" + pkName);
			names.add(base + pkName);
			if (pkName.equals("ID") || pkName.endsWith("_ID") || pkName.equals(base + "ID")) {
				names.add(base + "_ID");
				names.add(base + "ID");
				names.add(base + "_KEY");
				names.add(base + "_NO");
				names.add(base + "NO");
				names.add(base + "_NR");
				names.add(base + "_CODE");
			}
			if (pkName.startsWith(base) && pkName.length() > base.length()) {
				String rest = pkName.substring(base.length());
				while (rest.startsWith("_")) {
					rest = rest.substring(1);
				}
				if (rest.length() > 0) {
					names.add(rest);
				}
			}
		}
		return names;
	}

	/**
	 * Gets the name of a table together with its singular and plural form.
	 *
	 * @param table the table
	 * @return the normalized name variants
	 */
	private Set<String> baseNames(Table table) {
		String base = Quoting.normalizeIdentifier(Quoting.staticUnquote(table.getUnqualifiedName()));
		Set<String> names = new LinkedHashSet<String>();
		names.add(base);
		if (base.endsWith("IES")) {
			names.add(base.substring(0, base.length() - 3) + "Y");
		} else if (base.endsWith("ES") && base.length() > 3) {
			names.add(base.substring(0, base.length() - 2));
		}
		if (base.endsWith("S") && base.length() > 2) {
			names.add(base.substring(0, base.length() - 1));
		} else {
			names.add(base + "S");
		}
		return names;
	}

	/**
	 * Checks whether the given primary key is a house-wide convention: a column
	 * combination that serves as the key of a large part of the model (a technical object
	 * id, a tenant, a version, ...). Columns of the same name in another table are then no
	 * hint at a reference - otherwise every pair of tables would become a candidate, and
	 * since key ranges overlap many of those pairs would even pass the data check.
	 *
	 * @param pkColumns the primary key columns
	 * @return <code>true</code> if the key is shared by too many tables
	 */
	private boolean isHouseWidePrimaryKey(List<Column> pkColumns) {
		Integer count = pkNameSetCount.get(normalizedNames(pkColumns));
		if (count == null) {
			return false;
		}
		return count > SHARED_PK_MIN_TABLES
				|| (tables.size() >= SHARED_PK_MIN_TABLES && count * 4 > tables.size());
	}

	/**
	 * Gets the normalized names of the given columns.
	 *
	 * @param columns the columns
	 * @return the normalized names
	 */
	private Set<String> normalizedNames(List<Column> columns) {
		Set<String> names = new HashSet<String>();
		for (Column column: columns) {
			names.add(Quoting.normalizeIdentifier(column.name));
		}
		return names;
	}

	/**
	 * Checks whether a column name is so generic that it says nothing about the table
	 * it belongs to.
	 *
	 * @param name the normalized column name
	 * @return <code>true</code> if the name is generic
	 */
	private boolean isGenericName(String name) {
		return GENERIC_NAMES.contains(name);
	}

	/**
	 * Removes affixes that are commonly used to mark a foreign key column.
	 *
	 * @param name the normalized column name
	 * @return the name without affixes
	 */
	private String stripAffixes(String name) {
		String[] prefixes = new String[] { "FK_", "REF_", "ID_" };
		String[] suffixes = new String[] { "_FK", "_REF" };
		for (String prefix: prefixes) {
			if (name.startsWith(prefix) && name.length() > prefix.length()) {
				name = name.substring(prefix.length());
				break;
			}
		}
		for (String suffix: suffixes) {
			if (name.endsWith(suffix) && name.length() > suffix.length()) {
				name = name.substring(0, name.length() - suffix.length());
				break;
			}
		}
		return name;
	}

}
