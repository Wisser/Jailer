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
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.modelbuilder.ModelBuilder;
import net.sf.jailer.ui.associationdiscovery.AssociationCandidate.Evidence;
import net.sf.jailer.ui.associationdiscovery.ColumnTypes.Category;
import net.sf.jailer.ui.associationdiscovery.TableProfile.ColumnProfile;
import net.sf.jailer.ui.associationproposer.AssociationProposer;
import net.sf.jailer.util.Pair;
import net.sf.jailer.util.Quoting;

/**
 * Finds potential associations without evaluating foreign key constraints:
 * either by naming conventions, or - if profiles are available - by scanning the data.
 * Does not access the database.
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

	private final DataModel dataModel;
	private final List<Table> tables;

	/**
	 * Counts how many tables have a primary key of exactly these (normalized) column names.
	 */
	private final Map<Set<String>, Integer> pkNameSetCount = new HashMap<Set<String>, Integer>();

	/**
	 * @param dataModel the data model
	 */
	public AssociationCandidateFinder(DataModel dataModel) {
		this.dataModel = dataModel;
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
	 * Finds candidates whose columns follow a naming convention.
	 *
	 * @return the candidates
	 */
	public List<AssociationCandidate> findByName() {
		List<AssociationCandidate> result = new ArrayList<AssociationCandidate>();
		for (Table parent: tables) {
			List<Column> pkColumns = primaryKeyOf(parent);
			if (pkColumns == null) {
				continue;
			}
			List<Set<String>> patterns = new ArrayList<Set<String>>();
			boolean specific = false;
			for (Column pkColumn: pkColumns) {
				patterns.add(patternNames(parent, pkColumn));
				if (!isGenericName(Quoting.normalizeIdentifier(pkColumn.name))) {
					specific = true;
				}
			}
			if (isHouseWidePrimaryKey(pkColumns)) {
				specific = false;
			}
			for (Table child: tables) {
				AssociationCandidate candidate = matchByName(child, parent, pkColumns, patterns, specific);
				if (candidate != null) {
					result.add(candidate);
				}
			}
		}
		return result;
	}

	/**
	 * Finds candidates that have no name evidence at all, based on the value ranges
	 * and distinct counts of the profiles. Only parents with a single primary key column
	 * are considered - matching column tuples without any name evidence is not viable.
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
			if (pkColumns == null || pkColumns.size() != 1) {
				continue;
			}
			Column pkColumn = pkColumns.get(0);
			TableProfile parentProfile = profiles.get(parent);
			if (parentProfile == null || parentProfile.failed) {
				continue;
			}
			ColumnProfile pkProfile = parentProfile.get(pkColumn);
			if (pkProfile == null) {
				continue;
			}
			for (Table child: tables) {
				TableProfile childProfile = profiles.get(child);
				if (childProfile == null || childProfile.failed) {
					continue;
				}
				for (Column column: child.getColumns()) {
					if (!ColumnTypes.areCompatible(column, pkColumn)) {
						continue;
					}
					if (child == parent && Quoting.normalizeIdentifier(column.name).equals(Quoting.normalizeIdentifier(pkColumn.name))) {
						continue;
					}
					ColumnProfile columnProfile = childProfile.get(column);
					if (columnProfile == null || columnProfile.nonNullCount <= 0) {
						continue;
					}
					if (columnProfile.distinctCount >= 0 && columnProfile.distinctCount < MIN_DISTINCT_COUNT) {
						continue;
					}
					if (!childProfile.sampled && !parentProfile.sampled
							&& columnProfile.distinctCount >= 0 && parentProfile.rowCount >= 0
							&& columnProfile.distinctCount > parentProfile.rowCount) {
						continue;
					}
					if (!childProfile.sampled && !parentProfile.sampled
							&& columnProfile.hasRange() && pkProfile.hasRange()
							&& (columnProfile.min < pkProfile.min || columnProfile.max > pkProfile.max)) {
						continue;
					}
					AssociationCandidate candidate = new AssociationCandidate(child, parent,
							Collections.singletonList(column), Collections.singletonList(pkColumn), Evidence.DATA_ONLY);
					if (excludedKeys.contains(candidate.key())) {
						continue;
					}
					result.add(candidate);
					double coverage = 0;
					if (columnProfile.distinctCount >= 0 && parentProfile.rowCount > 0) {
						coverage = (double) columnProfile.distinctCount / parentProfile.rowCount;
					}
					rank.put(candidate, coverage);
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
	 * based on their names.
	 *
	 * @param patterns the accepted names per primary key column
	 * @param allowExact if <code>false</code>, a column of the same name is no evidence.
	 *        This is the case for primary keys that consist of generic names only:
	 *        a column named "ID" exists in almost every table and refers to its own table
	 * @return the candidate, or <code>null</code> if no complete mapping exists
	 */
	private AssociationCandidate matchByName(Table child, Table parent, List<Column> pkColumns, List<Set<String>> patterns, boolean allowExact) {
		List<Column> childColumns = new ArrayList<Column>();
		Set<String> usedChildColumns = new HashSet<String>();
		Evidence evidence = Evidence.NAME_EXACT;
		int exactMatches = 0;
		for (int p = 0; p < pkColumns.size(); ++p) {
			Column pkColumn = pkColumns.get(p);
			Set<String> pkPatterns = patterns.get(p);
			String pkName = Quoting.normalizeIdentifier(pkColumn.name);
			Column exact = null;
			Column pattern = null;
			for (Column column: child.getColumns()) {
				if (!ColumnTypes.areCompatible(column, pkColumn)) {
					continue;
				}
				String name = Quoting.normalizeIdentifier(column.name);
				if (usedChildColumns.contains(name)) {
					continue;
				}
				if (allowExact && name.equals(pkName)) {
					exact = column;
					break;
				}
				if (pattern == null && (pkPatterns.contains(name) || pkPatterns.contains(stripAffixes(name)))) {
					pattern = column;
				}
			}
			Column matched = exact != null? exact : pattern;
			if (matched == null) {
				return null;
			}
			if (exact != null) {
				++exactMatches;
			} else {
				evidence = Evidence.NAME_PATTERN;
			}
			childColumns.add(matched);
			usedChildColumns.add(Quoting.normalizeIdentifier(matched.name));
		}
		if (child == parent && exactMatches == pkColumns.size()) {
			// the primary key of the table matched against itself
			return null;
		}
		return new AssociationCandidate(child, parent, childColumns, pkColumns, evidence);
	}

	/**
	 * Builds the set of column names that are accepted as a reference to the given
	 * primary key column by naming convention.
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
