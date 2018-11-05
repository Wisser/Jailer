/*
 * Copyright 2007 - 2018 the original author or authors.
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
import java.util.HashSet;
import java.util.Set;

import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;
import net.sf.jailer.util.Quoting;

/**
 * Validates all primary keys of a set of tables.
 * 
 * @author Ralf Wisser
 */
public class PrimaryKeyValidator {

	/**
	 * Validates all primary keys of a set of tables.
	 * 
	 * @param session the session
	 * @param tables the tables
	 * @throws SQLException if a pk is invalid
	 */
	public void validatePrimaryKey(Session session, Set<Table> tables) throws SQLException {
		String defaultSchema = JDBCMetaDataBasedModelElementFinder.getDefaultSchema(session, session.getSchema());
		for (Table table: tables) {
			if (table.primaryKey == null || table.primaryKey.getColumns().isEmpty()) {
				// nothing to check here
				return;
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
			checkUniqueness(session, table, new Quoting(session));
			checkNoNull(session, table, new Quoting(session));
		}
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
		});
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
		});
	}

	private StringBuilder errorMessage = new StringBuilder();
	private StringBuilder errorStatements = new StringBuilder();

	private void addError(String messge, String sql) {
		errorMessage.append("- " + messge + "\n");
		errorStatements.append("- " + sql + "\n");
	}

}
