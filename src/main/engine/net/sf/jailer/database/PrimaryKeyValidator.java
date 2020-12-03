/*
 * Copyright 2007 - 2020 Ralf Wisser.
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
package net.sf.jailer.database;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;
import net.sf.jailer.util.CancellationException;
import net.sf.jailer.util.CancellationHandler;
import net.sf.jailer.util.JobManager;
import net.sf.jailer.util.JobManager.Job;
import net.sf.jailer.util.Quoting;

/**
 * Validates all primary keys of a set of tables.
 *
 * @author Ralf Wisser
 */
public abstract class PrimaryKeyValidator {

	private final Object cancellationContext;

	public PrimaryKeyValidator(Object cancellationContext) {
		this.cancellationContext = cancellationContext;
	}

	/**
	 * Validates all primary keys of a set of tables.
	 *
	 * @param session the session
	 * @param tables the tables
	 * @throws SQLException if a pk is invalid
	 */
	public void validatePrimaryKey(final Session session, Set<Table> tables, JobManager jobManager) throws SQLException {
		numTotal.set(0);
		numErrors.set(0);
		numDone.set(0);
		updateProgressBar();

		String defaultSchema = JDBCMetaDataBasedModelElementFinder.getDefaultSchema(session, session.getSchema());
		List<JobManager.Job> jobsUDPK = new ArrayList<JobManager.Job>();
		List<JobManager.Job> jobsRealPK = new ArrayList<JobManager.Job>();
		for (final Table table: tables) {
			CancellationHandler.checkForCancellation(cancellationContext);
			if (table.primaryKey == null || table.primaryKey.getColumns().isEmpty()) {
				// nothing to check here
				continue;
			}
			boolean realPK = false;
			try {
				ResultSet resultSet = JDBCMetaDataBasedModelElementFinder.getPrimaryKeys(
						session,
						Quoting.staticUnquote(table.getSchema(defaultSchema)),
						Quoting.staticUnquote(table.getUnqualifiedName()),
						true);
				Set<String> pkColumns = new HashSet<String>();
				while (resultSet.next()) {
					String colName = Quoting.normalizeIdentifier(resultSet.getString(4));
					pkColumns.add(colName);
				}
				resultSet.close();
				Set<String> tabPkColumns = new HashSet<String>();
				for (Column pkCol: table.primaryKey.getColumns()) {
					tabPkColumns.add(Quoting.normalizeIdentifier(pkCol.name));
				}
				if (tabPkColumns.equals(pkColumns)) {
					// real PK
					realPK = true;
				}
			} catch (Exception e) {
				// ignore
			}

			List<JobManager.Job> jobListToAddTo;
			if (realPK) {
				jobListToAddTo = jobsRealPK;
			} else {
				jobListToAddTo = jobsUDPK;
			}

			jobListToAddTo.add(() -> {
				checkUniqueness(session, table, Quoting.getQuoting(session));
				numDone.getAndIncrement();
				updateProgressBar();
			});
			jobListToAddTo.add(() -> {
				checkNoNull(session, table, Quoting.getQuoting(session));
				numDone.getAndIncrement();
				updateProgressBar();
			});
		}

		Collection<Job> jobs = new ArrayList<JobManager.Job>();
		jobs.addAll(jobsUDPK);
		jobs.addAll(jobsRealPK);

		numTotal.set(jobs.size());
		updateProgressBar();

		try {
			jobManager.executeJobs(jobs);
		} catch (CancellationException e) {
			throwIfErrorFound();
		}
		throwIfErrorFound();
		CancellationHandler.checkForCancellation(cancellationContext);
	}

	private void throwIfErrorFound() throws SqlException {
		if (errorMessage.length() > 0) {
			SqlException e = new SqlException("Invalid Primary Key", errorMessage.toString(), errorStatements.toString(), null);
			e.setFormatted(true);
			throw e;
		}
	}

	private void checkUniqueness(Session session, final Table table, Quoting quoting) throws SQLException {
		StringBuilder pks = new StringBuilder();
		for (Column pkCol: table.primaryKey.getColumns()) {
			if (pks.length() > 0) {
				pks.append(", ");
			}
			pks.append(quoting.requote(pkCol.name));
		}
		final String sql = "Select " + pks + " from " + quoting.requote(table.getName()) + " " +
				"Group by " + pks + " having count(*) > 1";
		try {
			session.executeQuery(sql, new Session.AbstractResultSetReader() {
				@Override
				public void readCurrentRow(ResultSet resultSet) throws SQLException {
					addError("Primary key of table \"" + table.getName() + "\" is not unique.", sql.toString());
				}
			}, null, cancellationContext, 1, true);
		} catch (SqlException e) {
			addError("Table \"" + table.getName() + "\": " + e.message, sql.toString());
		}
	}

	private void checkNoNull(Session session, final Table table, Quoting quoting) throws SQLException {
		StringBuilder hasNull = new StringBuilder();
		for (Column pkCol: table.primaryKey.getColumns()) {
			if (!pkCol.isNullable) {
				if (hasNull.length() > 0) {
					hasNull.append(" or ");
				}
				hasNull.append(quoting.requote(pkCol.name) + " is null");
			}
		}
		if (hasNull.length() > 0) {
			final String sql = "Select * from " + quoting.requote(table.getName()) + " " +
					"Where " + hasNull;
			try {
				session.executeQuery(sql, new Session.AbstractResultSetReader() {
					@Override
					public void readCurrentRow(ResultSet resultSet) throws SQLException {
						addError("Primary key of table \"" + table.getName() + "\" contains null.", sql.toString());
					}
				}, null, cancellationContext, 1, true);
			} catch (SqlException e) {
				addError("Table \"" + table.getName() + "\": " + e.message, sql.toString());
			}
		}
	}

	private StringBuilder errorMessage = new StringBuilder();
	private StringBuilder errorStatements = new StringBuilder();

	protected AtomicInteger numErrors = new AtomicInteger();
	protected AtomicInteger numDone = new AtomicInteger();
	protected AtomicInteger numTotal = new AtomicInteger();

	protected abstract void updateProgressBar();

	private void addError(String message, String sql) {
		errorMessage.append("- " + message + "\n");
		errorStatements.append("- " + sql + "\n");
		numErrors.getAndIncrement();
		updateProgressBar();
	}

}
