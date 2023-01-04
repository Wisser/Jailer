/*
 * Copyright 2007 - 2023 Ralf Wisser.
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

/**
 * Styles of inline-views for different DBMS'es.
 */
public enum InlineViewStyle {

	MySQL("(Select 1 A, '2' B, 3 C Union all " + "Select 4, '5', 6) %s") {
		@Override
		public String head(String[] columnNames) throws SQLException {
			return "(Select ";
		}

		@Override
		public String item(String[] values, String[] columnNames, int rowNumber) throws SQLException {
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
		public String separator() throws SQLException {
			return " Union all Select ";
		}

		@Override
		public String terminator(String name, String[] columnNames) throws SQLException {
			return ") " + name;
		}
	},
	Oracle("(Select 1 A, '2' B, 3 C from dual Union all " + "Select 4, '5', 6 from dual) %s") {
		@Override
		public String head(String[] columnNames) throws SQLException {
			return "(Select ";
		}

		@Override
		public String item(String[] values, String[] columnNames, int rowNumber) throws SQLException {
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
		public String separator() throws SQLException {
			return " from dual Union all Select ";
		}

		@Override
		public String terminator(String name, String[] columnNames) throws SQLException {
			StringBuilder sb = new StringBuilder(" from dual) " + name);
			return sb.toString();
		}
	},
	Db2("(values (1, '2', 3), (4, '5', 6)) %s(A, B, C)") {
		@Override
		public String head(String[] columnNames) throws SQLException {
			return "(values ";
		}

		@Override
		public String item(String[] values, String[] columnNames, int rowNumber) throws SQLException {
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
		public String separator() throws SQLException {
			return ", ";
		}

		@Override
		public String terminator(String name, String[] columnNames) throws SQLException {
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

	INFORMIX1("(Select 1 A, '2' B, 3 C from sysmaster.sysdual Union all "
			+ "Select 4, '5', 6 from sysmaster.sysdual) %s") {
		@Override
		public String head(String[] columnNames) throws SQLException {
			return "(Select ";
		}

		@Override
		public String item(String[] values, String[] columnNames, int rowNumber) throws SQLException {
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
		public String separator() throws SQLException {
			return " from sysmaster.sysdual Union all Select ";
		}

		@Override
		public String terminator(String name, String[] columnNames) throws SQLException {
			StringBuilder sb = new StringBuilder(" from sysmaster.sysdual) " + name);
			return sb.toString();
		}
	},

	INFORMIX2("(Select 1 A, '2' B, 3 C from table(set{1}) Union all " + 
			   "Select 4, '5', 6 from table(set{1})) %s") {
		@Override
		public String head(String[] columnNames) throws SQLException {
			return "(Select ";
		}

		@Override
		public String item(String[] values, String[] columnNames, int rowNumber) throws SQLException {
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
		public String separator() throws SQLException {
			return " from table(set{1}) Union all Select ";
		}

		@Override
		public String terminator(String name, String[] columnNames) throws SQLException {
			StringBuilder sb = new StringBuilder(" from table(set{1})) " + name);
			return sb.toString();
		}
	},
	DB2_ZOS("(Select 1, '2', 3 from sysibm.sysdummy1 Union all " + 
		   "Select 4, '5', 6 from sysibm.sysdummy1) %s(A, B, C)") {
		@Override
		public String head(String[] columnNames) throws SQLException {
			return "(Select ";
		}

		@Override
		public String item(String[] values, String[] columnNames, int rowNumber) throws SQLException {
			StringBuilder sb = new StringBuilder();
			for (int i = 1; i <= columnNames.length; ++i) {
				if (i > 1) {
					sb.append(", ");
				}
				sb.append(values[i - 1]);
			}
			return sb.toString();
		}

		@Override
		public String separator() throws SQLException {
			return " from sysibm.sysdummy1 Union all Select ";
		}

		@Override
		public String terminator(String name, String[] columnNames) throws SQLException {
			StringBuilder sb = new StringBuilder(" from sysibm.sysdummy1) " + name + "(");
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
	INFORMIX3("(Select 1 A, '2' B, 3 C from systables WHERE tabid=1 Union all " + 
			   "Select 4, '5', 6 from systables WHERE tabid=1) %s") {
		@Override
		public String head(String[] columnNames) throws SQLException {
			return "(Select ";
		}

		@Override
		public String item(String[] values, String[] columnNames, int rowNumber) throws SQLException {
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
		public String separator() throws SQLException {
			return " from systables WHERE tabid=1 Union all Select ";
		}

		@Override
		public String terminator(String name, String[] columnNames) throws SQLException {
			StringBuilder sb = new StringBuilder(" from systables WHERE tabid=1) " + name);
			return sb.toString();
		}
	},
	INFORMIX4("(Select 1 A, '2' B, 3 C from sysmaster:\"informix\".sysdual Union all "
			+ "Select 4, '5', 6 from sysmaster:\"informix\".sysdual) %s") {
		@Override
		public String head(String[] columnNames) throws SQLException {
			return "(Select ";
		}

		@Override
		public String item(String[] values, String[] columnNames, int rowNumber) throws SQLException {
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
		public String separator() throws SQLException {
			return " from sysmaster:\"informix\".sysdual Union all Select ";
		}

		@Override
		public String terminator(String name, String[] columnNames) throws SQLException {
			StringBuilder sb = new StringBuilder(" from sysmaster:\"informix\".sysdual) " + name);
			return sb.toString();
		}
	},
	FIREBIRD("(Select 1 A, '2' B, 3 C from RDB$DATABASE Union all "
			+ "Select 4, '5', 6 from RDB$DATABASE) %s") {
		@Override
		public String head(String[] columnNames) throws SQLException {
			return "(Select ";
		}

		@Override
		public String item(String[] values, String[] columnNames, int rowNumber) throws SQLException {
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
		public String separator() throws SQLException {
			return " from RDB$DATABASE Union all Select ";
		}

		@Override
		public String terminator(String name, String[] columnNames) throws SQLException {
			StringBuilder sb = new StringBuilder(" from RDB$DATABASE) " + name);
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
		StringBuilder messages = new StringBuilder();
		for (InlineViewStyle style : InlineViewStyle.values()) {
			boolean wasSilent = session.getSilent();
			try {
				session.setSilent(true);
				session.executeQuery("Select A, B, C from " + style.example.replace("%s", "Entity"),
						new Session.AbstractResultSetReader() {
							@Override
							public void readCurrentRow(ResultSet resultSet) throws SQLException {
								resultSet.getInt("A");
								resultSet.getInt("B");
								resultSet.getInt("C");
							}
						});
				return style;
			} catch (SQLException e) {
				String message = e.getMessage();
				messages.append("   " + style + ": \"" + message + "\"\n\n");
				// try next style
			} finally {
				session.setSilent(wasSilent);
			}
		}
		throw new RuntimeException("No suitable Inline-View Style known for " + session.dbUrl + "\n\n" + messages);
	}

	public abstract String head(String[] columnNames) throws SQLException;

	public abstract String item(String[] values, String[] columnNames, int rowNumber) throws SQLException;

	public abstract String separator() throws SQLException;

	public abstract String terminator(String name, String[] columnNames) throws SQLException;

}
