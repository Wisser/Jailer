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
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import net.sf.jailer.database.Session;
import net.sf.jailer.database.Session.ResultSetReader;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.associationdiscovery.ColumnTypes.Category;
import net.sf.jailer.ui.associationdiscovery.TableProfile.ColumnProfile;

/**
 * Gathers a statistical {@link TableProfile} of a table with a single aggregate query
 * per table (or per chunk of columns, if a table has many columns).
 *
 * @author Ralf Wisser
 */
public class TableProfiler {

	/**
	 * Maximum number of columns aggregated in one query.
	 */
	private static final int MAX_COLUMNS_PER_QUERY = 40;

	/**
	 * Number of rows read if a table has to be sampled.
	 */
	private static final int SAMPLE_ROWS = 100000;

	/**
	 * If profiling a single table takes longer than this, "count(distinct ...)" is dropped
	 * for all remaining tables.
	 */
	private static final long SLOW_PROFILING_MS = 5000;

	private final Session session;
	private final Object cancellationContext;
	private final DiscoveryLog log;

	/**
	 * Set to <code>false</code> as soon as a query with "count(distinct ...)" turns out
	 * to be slow. Holds for the rest of the run.
	 */
	private boolean distinctSupported = true;

	/**
	 * Duration of the last executed query.
	 */
	private long lastDurationMs;

	/**
	 * @param session the database session
	 * @param cancellationContext the cancellation context
	 * @param log receives executed statements and problems
	 */
	public TableProfiler(Session session, Object cancellationContext, DiscoveryLog log) {
		this.session = session;
		this.cancellationContext = cancellationContext;
		this.log = log;
	}

	/**
	 * Profiles a table. Never throws, problems are reported to the log.
	 *
	 * @param table the table
	 * @return the profile. TableProfile.failed is set if nothing could be gathered
	 */
	public TableProfile profile(Table table) {
		TableProfile profile = new TableProfile(table);
		List<Column> columns = new ArrayList<Column>();
		for (Column column: table.getColumns()) {
			if (ColumnTypes.categoryOf(column) != Category.OTHER) {
				columns.add(column);
			}
		}
		if (columns.isEmpty()) {
			profile.failed = true;
			return profile;
		}
		boolean any = false;
		for (int i = 0; i < columns.size(); i += MAX_COLUMNS_PER_QUERY) {
			if (log.isCancelled()) {
				break;
			}
			List<Column> chunk = columns.subList(i, Math.min(i + MAX_COLUMNS_PER_QUERY, columns.size()));
			if (profileChunk(table, chunk, profile)) {
				any = true;
			}
		}
		profile.failed = !any;
		return profile;
	}

	/**
	 * Profiles a chunk of columns, trying successively cheaper queries.
	 *
	 * @return <code>true</code> if the chunk could be profiled
	 */
	private boolean profileChunk(Table table, List<Column> chunk, TableProfile profile) {
		// 1. everything, as long as "count(distinct ...)" has not turned out to be too slow
		if (distinctSupported && execute(table, chunk, profile, true, false)) {
			if (lastDurationMs > SLOW_PROFILING_MS) {
				distinctSupported = false;
				log.problem("Profiling " + table.getName() + " took " + (lastDurationMs / 1000) + "s. "
						+ "Continuing without count(distinct ...) for the remaining tables, "
						+ "so the cardinality of their columns stays unknown.");
			}
			return true;
		}
		if (log.isCancelled()) {
			return false;
		}
		// 2. without "count(distinct ...)", which is the expensive part
		if (execute(table, chunk, profile, false, false)) {
			return true;
		}
		if (log.isCancelled()) {
			return false;
		}
		// 3. everything, but only over a sample of the table
		if (session.dbms.getSqlLimitSuffix() != null && execute(table, chunk, profile, true, true)) {
			profile.sampled = true;
			return true;
		}
		log.problem("Table " + table.getName() + ": could not be profiled.");
		return false;
	}

	/**
	 * Builds and executes one aggregate query.
	 *
	 * @param withDistinct include "count(distinct ...)"
	 * @param sampled read only a sample of the table
	 * @return <code>true</code> on success
	 */
	private boolean execute(Table table, final List<Column> chunk, final TableProfile profile, final boolean withDistinct, final boolean sampled) {
		final List<Column> minMaxColumns = new ArrayList<Column>();
		StringBuilder select = new StringBuilder("count(*)");
		for (Column column: chunk) {
			select.append(", count(" + column.name + ")");
			if (withDistinct) {
				select.append(", count(distinct " + column.name + ")");
			}
			if (ColumnTypes.categoryOf(column) == Category.NUMERIC) {
				select.append(", min(" + column.name + "), max(" + column.name + ")");
				minMaxColumns.add(column);
			}
		}
		String from = table.getName();
		if (sampled) {
			StringBuilder inner = new StringBuilder();
			for (Column column: chunk) {
				inner.append(inner.length() == 0? "" : ", ").append(column.name);
			}
			from = "(" + limited("Select " + inner + " From " + table.getName()) + ") A";
		}
		final String sql = "Select " + select + " From " + from;
		long startTime = System.currentTimeMillis();
		try {
			session.setSilent(true);
			log.statement(sql);
			session.executeQuery(sql, new ResultSetReader() {
				@Override
				public void readCurrentRow(ResultSet resultSet) throws SQLException {
					int i = 1;
					long rc = resultSet.getLong(i++);
					if (!sampled || profile.rowCount < 0) {
						profile.rowCount = rc;
					}
					for (Column column: chunk) {
						ColumnProfile columnProfile = profile.getOrCreate(column);
						columnProfile.nonNullCount = resultSet.getLong(i++);
						if (withDistinct) {
							columnProfile.distinctCount = resultSet.getLong(i++);
						}
						if (minMaxColumns.contains(column)) {
							columnProfile.min = toDouble(resultSet.getString(i++));
							columnProfile.max = toDouble(resultSet.getString(i++));
						}
					}
				}
				@Override
				public void close() throws SQLException {
				}
			}, null, cancellationContext, 0);
			return true;
		} catch (Throwable t) { // embedded DBMS may throw non-SQLException
			return false;
		} finally {
			lastDurationMs = System.currentTimeMillis() - startTime;
			session.setSilent(false);
		}
	}

	/**
	 * Applies the DBMS specific row limit to a Select statement.
	 *
	 * @param select the statement, starting with "Select "
	 * @return the limited statement
	 */
	private String limited(String select) {
		String suffix = session.dbms.getSqlLimitSuffix().replace("%s", Integer.toString(SAMPLE_ROWS));
		String lcSuffix = suffix.toLowerCase(Locale.ENGLISH);
		if (lcSuffix.startsWith("top ") || lcSuffix.startsWith("first ")) {
			return "Select " + suffix + " " + select.substring("Select ".length());
		}
		return select + " " + suffix;
	}

	private static Double toDouble(String value) {
		if (value == null) {
			return null;
		}
		try {
			return Double.valueOf(value.trim());
		} catch (NumberFormatException e) {
			return null;
		}
	}

}
