/*
 * Copyright 2007 - 2019 the original author or authors.
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
import java.sql.Types;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;
import net.sf.jailer.util.CancellationHandler;
import net.sf.jailer.util.JobManager;
import net.sf.jailer.util.Quoting;

/**
 * Validates all primary keys of a set of tables.
 * 
 * @author Ralf Wisser
 */
public class PrimaryKeyValidator {

	private static final boolean FAIL_FAST = false;
	
	/**
	 * Validates all primary keys of a set of tables.
	 * 
	 * @param session the session
	 * @param tables the tables
	 * @param hasRowID 
	 * @throws SQLException if a pk is invalid
	 */
	public void validatePrimaryKey(final Session session, Set<Table> tables, boolean hasRowID, JobManager jobManager) throws SQLException {
		String defaultSchema = JDBCMetaDataBasedModelElementFinder.getDefaultSchema(session, session.getSchema());
		List<JobManager.Job> jobs = new ArrayList<JobManager.Job>();
		for (final Table table: tables) {
			CancellationHandler.checkForCancellation(null);
			if (table.primaryKey == null || table.primaryKey.getColumns().isEmpty()) {
				// nothing to check here
				continue;
			}
			if (hasRowID && !Boolean.TRUE.equals(table.upsert) && !hasLOBColumns(table, defaultSchema, session)) {
				// not necessary to check here
				continue;
			}
			try {
				ResultSet resultSet = JDBCMetaDataBasedModelElementFinder.getPrimaryKeys(
						session,
						session.getMetaData(),
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
					continue;
				}
			} catch (Exception e) {
				// ignore
			}
			jobs.add(new JobManager.Job() {
				@Override
				public void run() throws SQLException {
					checkUniqueness(session, table, new Quoting(session));
					if (FAIL_FAST) {
						throwIfErrorFound();
					}
				}
			});
			jobs.add(new JobManager.Job() {
				@Override
				public void run() throws SQLException {
					checkNoNull(session, table, new Quoting(session));
					if (FAIL_FAST) {
						throwIfErrorFound();
					}
				}
			});
		}
		jobManager.executeJobs(jobs);
		throwIfErrorFound();
	}

	private boolean hasLOBColumns(Table table, String defaultSchema, Session session) throws SQLException {
		ResultSet resultSet = JDBCMetaDataBasedModelElementFinder.getColumns(
				session,
				session.getMetaData(),
				Quoting.staticUnquote(table.getSchema(defaultSchema)),
				Quoting.staticUnquote(table.getUnqualifiedName()),
				"%", false, false, null);
		boolean hasLOB = false;
		while (resultSet.next()) {
			int type = resultSet.getInt(5);
			if (type == Types.BLOB || type == Types.CLOB || type == Types.NCLOB || type == Types.SQLXML) {
				hasLOB = true;
				break;
			}
		}
		resultSet.close();
		return hasLOB;
	}

	private void throwIfErrorFound() throws SqlException {
		if (errorMessage.length() > 0) {
			SqlException e = new SqlException(errorMessage.toString(), errorStatements.toString(), null);
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
		final String sql = "Select 1 from " + quoting.requote(table.getName()) + " " +
				"Group by " + pks + " having count(*) > 1";
		session.executeQuery(sql, new Session.AbstractResultSetReader() {
			@Override
			public void readCurrentRow(ResultSet resultSet) throws SQLException {
				addError("Primary key of table \"" + table.getName() + "\" is not unique.", sql.toString());
			}
		}, null, null, 1);
	}

	private void checkNoNull(Session session, final Table table, Quoting quoting) throws SQLException {
		StringBuilder hasNull = new StringBuilder();
		for (Column pkCol: table.primaryKey.getColumns()) {
			if (hasNull.length() > 0) {
				hasNull.append(" or ");
			}
			hasNull.append(quoting.requote(pkCol.name) + " is null");
		}
		final String sql = "Select 1 from " + quoting.requote(table.getName()) + " " +
				"Where " + hasNull;
		session.executeQuery(sql, new Session.AbstractResultSetReader() {
			@Override
			public void readCurrentRow(ResultSet resultSet) throws SQLException {
				addError("Primary key of table \"" + table.getName() + "\" contains null.", sql.toString());
			}
		}, null, null, 1);
	}

	private StringBuilder errorMessage = new StringBuilder();
	private StringBuilder errorStatements = new StringBuilder();

	private void addError(String message, String sql) {
		errorMessage.append("- " + message + "\n");
		errorStatements.append("- " + sql + "\n");
	}

}
