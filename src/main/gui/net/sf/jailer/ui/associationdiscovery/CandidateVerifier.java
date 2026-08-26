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

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Map;

import net.sf.jailer.database.Session;
import net.sf.jailer.database.Session.ResultSetReader;
import net.sf.jailer.datamodel.Cardinality;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.associationdiscovery.TableProfile.ColumnProfile;

/**
 * Verifies an {@link AssociationCandidate} against the data: counts the child rows
 * that have no matching parent row (an inclusion dependency check) and derives the
 * confidence of the candidate from that.
 *
 * @author Ralf Wisser
 */
public class CandidateVerifier {

	/**
	 * Number of distinct values below which a candidate is considered to carry
	 * little information, even if all of its values match.
	 */
	private static final long LOW_DISTINCT_COUNT = 10;

	private final Session session;
	private final Object cancellationContext;
	private final DiscoveryLog log;
	private final Map<Table, TableProfile> profiles;

	/**
	 * @param session the database session
	 * @param cancellationContext the cancellation context
	 * @param profiles the table profiles
	 * @param log receives executed statements and problems
	 */
	public CandidateVerifier(Session session, Object cancellationContext, Map<Table, TableProfile> profiles, DiscoveryLog log) {
		this.session = session;
		this.cancellationContext = cancellationContext;
		this.profiles = profiles;
		this.log = log;
	}

	/**
	 * Verifies a candidate. Never throws, problems are reported to the log.
	 *
	 * @param candidate the candidate, filled in on success
	 * @return the number of statements executed
	 */
	public int verify(AssociationCandidate candidate) {
		int statements = 0;
		long nonNullCount = nonNullCountFromProfile(candidate);
		if (nonNullCount < 0) {
			String sql = "Select count(*) From " + candidate.child.getName() + " A Where " + notNullCondition(candidate);
			Long count = count(sql);
			++statements;
			if (count == null) {
				log.problem(candidate.child.getName() + ": " + "could not be counted.");
				return statements;
			}
			nonNullCount = count;
		}
		candidate.nonNullCount = nonNullCount;
		if (nonNullCount == 0) {
			// no data, nothing can be confirmed
			return statements;
		}
		String sql = "Select count(*) From " + candidate.child.getName() + " A Where " + notNullCondition(candidate)
				+ " and not exists (Select 1 From " + candidate.parent.getName() + " P Where " + joinCondition(candidate) + ")";
		candidate.verifyStatement = sql;
		Long orphans = count(sql);
		++statements;
		if (orphans == null) {
			log.problem(candidate.toString() + ": could not be verified.");
			return statements;
		}
		candidate.orphans = orphans;
		candidate.matchRatio = (double) (nonNullCount - orphans) / nonNullCount;
		candidate.cardinality = cardinalityOf(candidate, nonNullCount);
		candidate.confidence = confidenceOf(candidate);
		return statements;
	}

	/**
	 * Executes a query returning a single count.
	 *
	 * @param sql the query
	 * @return the count, or <code>null</code> if the query failed
	 */
	private Long count(String sql) {
		final long[] result = new long[] { -1 };
		try {
			session.setSilent(true);
			log.statement(sql);
			session.executeQuery(sql, new ResultSetReader() {
				@Override
				public void readCurrentRow(ResultSet resultSet) throws SQLException {
					result[0] = resultSet.getLong(1);
				}
				@Override
				public void close() throws SQLException {
				}
			}, null, cancellationContext, 0);
		} catch (Throwable t) { // embedded DBMS may throw non-SQLException
			return null;
		} finally {
			session.setSilent(false);
		}
		return result[0] < 0? null : Long.valueOf(result[0]);
	}

	/**
	 * Gets the number of child rows in which all foreign key columns are not null,
	 * as far as it can be taken from the profile.
	 *
	 * @param candidate the candidate
	 * @return the number of rows, or <code>-1</code> if it has to be counted
	 */
	private long nonNullCountFromProfile(AssociationCandidate candidate) {
		if (candidate.childColumns.size() != 1) {
			return -1;
		}
		TableProfile profile = profiles.get(candidate.child);
		if (profile == null || profile.failed || profile.sampled) {
			return -1;
		}
		ColumnProfile columnProfile = profile.get(candidate.childColumns.get(0));
		if (columnProfile == null) {
			return -1;
		}
		return columnProfile.nonNullCount;
	}

	/**
	 * Derives the cardinality from the profile of the child columns.
	 *
	 * @param candidate the candidate
	 * @param nonNullCount number of child rows with a complete foreign key
	 * @return the cardinality, or <code>null</code> if unknown
	 */
	private Cardinality cardinalityOf(AssociationCandidate candidate, long nonNullCount) {
		if (candidate.childColumns.size() != 1) {
			return null;
		}
		TableProfile profile = profiles.get(candidate.child);
		if (profile == null || profile.failed || profile.sampled) {
			return null;
		}
		ColumnProfile columnProfile = profile.get(candidate.childColumns.get(0));
		if (columnProfile == null || columnProfile.distinctCount < 0) {
			return null;
		}
		return columnProfile.distinctCount == nonNullCount? Cardinality.ONE_TO_ONE : Cardinality.MANY_TO_ONE;
	}

	/**
	 * Calculates the confidence of a verified candidate, in percent.
	 *
	 * @param candidate the candidate
	 * @return the confidence
	 */
	private double confidenceOf(AssociationCandidate candidate) {
		double confidence = 100.0 * candidate.matchRatio * candidate.evidence.weight * distinctFactor(candidate);
		if (confidence < 1) {
			confidence = 1;
		} else if (confidence > 99) {
			confidence = 99;
		}
		return confidence;
	}

	/**
	 * A column with very few distinct values matches almost any key by chance.
	 * This factor damps the confidence of such candidates.
	 *
	 * @param candidate the candidate
	 * @return a factor between 0 and 1
	 */
	private double distinctFactor(AssociationCandidate candidate) {
		if (candidate.childColumns.size() != 1) {
			return 1;
		}
		TableProfile profile = profiles.get(candidate.child);
		if (profile == null || profile.failed) {
			return 1;
		}
		ColumnProfile columnProfile = profile.get(candidate.childColumns.get(0));
		if (columnProfile == null || columnProfile.distinctCount < 0) {
			return 1;
		}
		if (columnProfile.distinctCount >= LOW_DISTINCT_COUNT) {
			return 1;
		}
		return 0.5 + 0.5 * columnProfile.distinctCount / LOW_DISTINCT_COUNT;
	}

	/**
	 * Builds the "is not null" condition for all foreign key columns.
	 *
	 * @param candidate the candidate
	 * @return the condition
	 */
	private String notNullCondition(AssociationCandidate candidate) {
		StringBuilder condition = new StringBuilder();
		for (Column column: candidate.childColumns) {
			if (condition.length() > 0) {
				condition.append(" and ");
			}
			condition.append("A." + column.name + " is not null");
		}
		return condition.toString();
	}

	/**
	 * Builds the join condition between the child alias "A" and the parent alias "P".
	 *
	 * @param candidate the candidate
	 * @return the condition
	 */
	private String joinCondition(AssociationCandidate candidate) {
		StringBuilder condition = new StringBuilder();
		for (int i = 0; i < candidate.childColumns.size(); ++i) {
			if (condition.length() > 0) {
				condition.append(" and ");
			}
			condition.append("A." + candidate.childColumns.get(i).name + " = P." + candidate.parentColumns.get(i).name);
		}
		return condition.toString();
	}

}
