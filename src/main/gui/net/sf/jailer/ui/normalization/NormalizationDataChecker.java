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

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.Session;
import net.sf.jailer.database.Session.ResultSetReader;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.ui.associationdiscovery.SampleSize;
import net.sf.jailer.ui.associationdiscovery.TableProfiler;
import net.sf.jailer.ui.normalization.NormalizationFinding.Severity;

/**
 * Checks a copied column finding of the {@link NormalizationChecker} against the data:
 * joins the table with the parent table over the foreign key and counts the rows in which
 * the copy equals the original. Only a sample of the table is checked.
 *
 * @author Ralf Wisser
 */
public class NormalizationDataChecker {

	/**
	 * Minimum number of joined rows. With fewer rows, equal values may be a coincidence.
	 */
	private static final long MIN_ROWS = 5;

	private final Session session;
	private final Object cancellationContext;

	/**
	 * Maximum number of rows of the table that are checked, or {@link SampleSize#UNLIMITED}.
	 * A sample can only confirm a finding wrongly, never refute it wrongly.
	 */
	private final int sampleRows;

	/**
	 * @param session the database session
	 * @param cancellationContext the cancellation context
	 * @param sampleRows maximum number of rows of a table that are checked, or {@link SampleSize#UNLIMITED}
	 */
	public NormalizationDataChecker(Session session, Object cancellationContext, int sampleRows) {
		this.session = session;
		this.cancellationContext = cancellationContext;
		this.sampleRows = sampleRows;
	}

	/**
	 * Checks a finding. Never throws, the result is put into {@link NormalizationFinding#dataCheck}.
	 * If the data confirms the finding, its severity is raised to {@link Severity#LIKELY}.
	 *
	 * @param finding the finding, must be {@link NormalizationFinding#isDataCheckable() checkable}
	 * @return <code>false</code> if the finding is not supported by the data, i.e. each of its columns
	 *         is refuted or there are too few rows to tell
	 */
	public boolean check(NormalizationFinding finding) {
		boolean allConfirmed = true;
		boolean anyRefuted = false;
		boolean show = false;
		StringBuilder result = new StringBuilder();
		long[] counts = count(finding);
		boolean sampled = sampleRows != SampleSize.UNLIMITED && counts != null && counts[0] >= sampleRows;
		int i = 0;
		for (Map.Entry<Column, Column> copy: finding.copies.entrySet()) {
			String text;
			if (counts == null) {
				text = "not checkable";
				allConfirmed = false;
				show = true;
			} else {
				long joined = counts[1];
				long equal = counts[2 + i];
				if (joined == 0) {
					text = "no data";
					allConfirmed = false;
				} else if (joined < MIN_ROWS) {
					text = "too few rows (" + joined + ")";
					allConfirmed = false;
				} else {
					long differing = joined - equal;
					// confirmed only if the copy equals the original in every row
					if (differing == 0) {
						text = "confirmed (all " + joined + " rows equal" + (sampled? ", sample" : "") + ")";
						show = true;
					} else {
						text = "refuted (" + differing + " of " + joined + " rows differ" + (sampled? ", sample" : "") + ")";
						anyRefuted = true;
						allConfirmed = false;
					}
				}
			}
			++i;
			if (finding.copies.size() > 1) {
				text = copy.getKey().name + ": " + text;
			}
			if (result.length() > 0) {
				result.append("; ");
			}
			result.append(text);
		}
		if (allConfirmed && !anyRefuted) {
			finding.severity = Severity.LIKELY;
		}
		finding.dataCheck = result.toString();
		return show;
	}

	/**
	 * Counts, in one query over a sample of the table, the sampled rows, the rows that have a parent row,
	 * and per copied column the rows in which the copy equals the original.
	 *
	 * @return { sampled rows, joined rows, equal rows of the 1st copied column, of the 2nd, ... },
	 *         or <code>null</code> if the query failed
	 */
	private long[] count(NormalizationFinding finding) {
		StringBuilder join = new StringBuilder();
		Set<String> sampleColumns = new LinkedHashSet<String>();
		String parentKey = null;
		for (Map.Entry<Column, Column> e: finding.fkMapping.entrySet()) {
			if (join.length() > 0) {
				join.append(" and ");
			}
			join.append("A." + e.getKey().name + " = B." + e.getValue().name);
			sampleColumns.add(e.getKey().name);
			if (parentKey == null) {
				parentKey = "B." + e.getValue().name;
			}
		}
		// rows without parent row (left join) must not count as equal
		StringBuilder select = new StringBuilder("Select count(*), count(" + parentKey + ")");
		for (Map.Entry<Column, Column> copy: finding.copies.entrySet()) {
			String a = "A." + copy.getKey().name;
			String b = "B." + copy.getValue().name;
			select.append(", sum(case when " + parentKey + " is not null and (" + a + " = " + b + " or (" + a + " is null and " + b + " is null)) then 1 else 0 end)");
			sampleColumns.add(copy.getKey().name);
		}
		StringBuilder columns = new StringBuilder();
		for (String column: sampleColumns) {
			columns.append(columns.length() == 0? "" : ", ").append(column);
		}
		String sample = "Select " + columns + " From " + finding.table.getName();
		String limitedSample = null;
		if (sampleRows != SampleSize.UNLIMITED) {
			limitedSample = TableProfiler.limited(session, sample, sampleRows);
			if (limitedSample == null && DBMS.ORACLE.equals(session.dbms)) {
				// Oracle has no limit suffix in the configuration
				limitedSample = sample + " where rownum <= " + sampleRows;
			}
		}
		String sql = select + " From (" + (limitedSample != null? limitedSample : sample) + ") A"
				+ " left join " + finding.parent.getName() + " B on " + join;
		final int size = 2 + finding.copies.size();
		final long[] result = new long[size];
		result[0] = -1;
		try {
			session.setSilent(true);
			session.executeQuery(sql, new ResultSetReader() {
				@Override
				public void readCurrentRow(ResultSet resultSet) throws SQLException {
					for (int i = 0; i < size; ++i) {
						long value = resultSet.getLong(i + 1);
						result[i] = resultSet.wasNull()? 0 : value;
					}
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
		return result[0] < 0? null : result;
	}

}
