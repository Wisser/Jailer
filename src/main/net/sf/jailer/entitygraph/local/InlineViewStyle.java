/*
 * Copyright 2007 - 2015 the original author or authors.
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
package net.sf.jailer.entitygraph.local;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;

import net.sf.jailer.database.Session;

/**
 * Styles of inline-views for different DBMS'es.
 */
enum InlineViewStyle {

	 DB2("(values (1, '2', 3), (4, '5', 6)) %s(A, B, C)") {
			@Override
			String head(ResultSet resultSet, ResultSetMetaData resultSetMetaData, String[] values, String[] columnNames) throws SQLException {
				return "(values ";
			}

			@Override
			String item(ResultSet resultSet, ResultSetMetaData resultSetMetaData, String[] values, String[] columnNames, int rowNumber) throws SQLException {
				StringBuilder sb = new StringBuilder("(");
				for (int i = 1; i <= columnNames.length; ++i) {
					if (i > 1) {
						sb.append(", ");
					}
					sb.append(values[i - 1]);
				}
				sb.append(")");
				return sb.toString();
			}

			@Override
			String separator(ResultSet resultSet,
					ResultSetMetaData resultSetMetaData) throws SQLException {
				return ", ";
			}

			@Override
			String terminator(ResultSet resultSet,
					ResultSetMetaData resultSetMetaData, String name, String[] columnNames) throws SQLException {
				StringBuilder sb = new StringBuilder(") " + name + "(");
				for (int i = 1; i <= columnNames.length; ++i) {
					if (i > 1) {
						sb.append(", ");
					}
					sb.append(columnNames[i - 1]);
				}
				sb.append(")");
				return sb.toString();
			}
		},
	
	 MySQL("(Select 1 A, '2' B, 3 C Union all " +
	        "Select 4, '5', 6) %s") {
			@Override
			String head(ResultSet resultSet, ResultSetMetaData resultSetMetaData, String[] values, String[] columnNames) throws SQLException {
				return "(Select ";
			}

			@Override
			String item(ResultSet resultSet, ResultSetMetaData resultSetMetaData, String[] values, String[] columnNames, int rowNumber) throws SQLException {
				StringBuilder sb = new StringBuilder();
				for (int i = 1; i <= columnNames.length; ++i) {
					if (i > 1) {
						sb.append(", ");
					}
					sb.append(values[i - 1]);
					if (rowNumber == 0) {
						sb.append(" " + columnNames[i - 1]);
					}
				}
				return sb.toString();
			}

			@Override
			String separator(ResultSet resultSet,
					ResultSetMetaData resultSetMetaData) throws SQLException {
				return " Union all Select ";
			}

			@Override
			String terminator(ResultSet resultSet,
					ResultSetMetaData resultSetMetaData, String name, String[] columnNames) throws SQLException {
				return ") " + name;
			}
		},
	
	Oracle("(Select 1 A, '2' B, 3 C from dual Union all "
		  + "Select 4, '5', 6 from dual) %s") {
		@Override
		String head(ResultSet resultSet, ResultSetMetaData resultSetMetaData, String[] values, String[] columnNames) throws SQLException {
			return "(Select ";
		}

		@Override
		String item(ResultSet resultSet, ResultSetMetaData resultSetMetaData, String[] values, String[] columnNames, int rowNumber) throws SQLException {
			StringBuilder sb = new StringBuilder();
			for (int i = 1; i <= columnNames.length; ++i) {
				if (i > 1) {
					sb.append(", ");
				}
				sb.append(values[i - 1]);
				if (rowNumber == 0) {
					sb.append(" " + columnNames[i - 1]);
				}
			}
			return sb.toString();
		}

		@Override
		String separator(ResultSet resultSet,
				ResultSetMetaData resultSetMetaData) throws SQLException {
			return " from dual Union all Select ";
		}

		@Override
		String terminator(ResultSet resultSet,
				ResultSetMetaData resultSetMetaData, String name, String[] columnNames) throws SQLException {
			StringBuilder sb = new StringBuilder(" from dual) " + name);
			return sb.toString();
		}
	};

	public final String example;

	private InlineViewStyle(String example) {
		this.example = example;
	}

	/**
	 * Gets a style for a session.
	 */
	public static InlineViewStyle forSession(Session session) {
		for (InlineViewStyle style : InlineViewStyle.values()) {
			boolean wasSilent = session.getSilent();
			try {
				session.setSilent(true);
				session.executeQuery(
						"Select * from "
								+ style.example.replace("%s", "Entity"),
						new Session.AbstractResultSetReader() {
							@Override
							public void readCurrentRow(ResultSet resultSet)
									throws SQLException {
								resultSet.getInt("A");
								resultSet.getInt("B");
								resultSet.getInt("C");
							}
						});
				return style;
			} catch (SQLException e) {
				// try next style
			} finally {
				session.setSilent(wasSilent);
			}
		}
		throw new RuntimeException("No suitable Inline-View Style known for "
				+ session.dbUrl);
	}

	abstract String head(ResultSet resultSet,
			ResultSetMetaData resultSetMetaData, String[] values, String[] columnNames) throws SQLException;

	abstract String item(ResultSet resultSet,
			ResultSetMetaData resultSetMetaData, String[] values, String[] columnNames, int rowNumber) throws SQLException;

	abstract String separator(ResultSet resultSet,
			ResultSetMetaData resultSetMetaData) throws SQLException;

	abstract String terminator(ResultSet resultSet,
			ResultSetMetaData resultSetMetaData, String name, String[] columnNames) throws SQLException;

}