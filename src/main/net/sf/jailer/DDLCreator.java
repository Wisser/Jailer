/*
 * Copyright 2007 - 2012 the original author or authors.
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
package net.sf.jailer;

import java.io.File;
import java.io.PrintWriter;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.sf.jailer.database.DBMS;
import net.sf.jailer.database.SQLDialect;
import net.sf.jailer.database.Session;
import net.sf.jailer.database.TemporaryTableManager;
import net.sf.jailer.database.TemporaryTableScope;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.util.PrintUtil;
import net.sf.jailer.util.SqlScriptExecutor;
import net.sf.jailer.util.SqlUtil;

/**
 * Creates the DDL for the working-tables.
 * 
 * @author Ralf Wisser
 */
public class DDLCreator {

	/**
	 * Creates the DDL for the working-tables.
	 */
	public static boolean createDDL(String driverClass, String dbUrl, String user, String password, TemporaryTableScope temporaryTableScope) throws Exception {
		Session session = null;
		if (driverClass != null) {
			session = new Session(driverClass, dbUrl, user, password);
		}
		try {
			return createDDL(new DataModel(), session, temporaryTableScope);
		} finally {
			if (session != null) {
				try { session.shutDown(); } catch (Exception e) { /* ignore */ }
			}
		}
	}

	/**
	 * Creates the DDL for the working-tables.
	 */
	public static void createDDL(Session localSession, TemporaryTableScope temporaryTableScope) throws Exception {
		createDDL(new DataModel(), localSession, temporaryTableScope);
	}

	/**
	 * Creates the DDL for the working-tables.
	 */
	public static boolean createDDL(DataModel datamodel, Session session, TemporaryTableScope temporaryTableScope) throws Exception {
		try {
			return createDDL(datamodel, session, temporaryTableScope, 0);
		} catch (SQLException e) {
		}
		// reconnect and retry with another index type
		session.reconnect();
		try {
			return createDDL(datamodel, session, temporaryTableScope, 1);
		} catch (SQLException e) {
		}
		// reconnect and retry with another index type
		session.reconnect();
		return createDDL(datamodel, session, temporaryTableScope, 2);
	}

	/**
	 * Creates the DDL for the working-tables.
	 */
	private static boolean createDDL(DataModel dataModel, Session session, TemporaryTableScope temporaryTableScope, int indexType) throws Exception {
		String template = "script" + File.separator + "ddl-template.sql";
		String contraint = session != null && (session.dbms == DBMS.SYBASE || session.dbms == DBMS.MySQL) ? " NULL" : "";
		Map<String, String> typeReplacement = Configuration.forDbms(session).getTypeReplacement();
		String universalPrimaryKey = dataModel.getUniversalPrimaryKey().toSQL(null, contraint, typeReplacement);
		Map<String, String> arguments = new HashMap<String, String>();
		arguments.put("upk", universalPrimaryKey);
		arguments.put("upk-hash", "" + (universalPrimaryKey.hashCode()));
		arguments.put("pre", dataModel.getUniversalPrimaryKey().toSQL("PRE_", contraint, typeReplacement));
		arguments.put("from", dataModel.getUniversalPrimaryKey().toSQL("FROM_", contraint, typeReplacement));
		arguments.put("to", dataModel.getUniversalPrimaryKey().toSQL("TO_", contraint, typeReplacement));
		arguments.put("version", Jailer.VERSION);
		arguments.put("constraint", contraint);

		TemporaryTableManager tableManager = null;
		if (temporaryTableScope == TemporaryTableScope.SESSION_LOCAL) {
			tableManager = Configuration.forDbms(session).sessionTemporaryTableManager;
		}
		if (temporaryTableScope == TemporaryTableScope.TRANSACTION_LOCAL) {
			tableManager = Configuration.forDbms(session).transactionTemporaryTableManager;
		}
		String tableName = SQLDialect.CONFIG_TABLE_;
		if (tableManager != null) {
			tableName = tableManager.getDmlTableReference(tableName);
		}
		arguments.put("config-dml-reference", tableName);
		if (tableManager != null) {
			arguments.put("table-suffix", "_T");
			arguments.put("drop-table", tableManager.getDropTablePrefix());
			arguments.put("create-table", tableManager.getCreateTablePrefix());
			arguments.put("create-table-suffix", tableManager.getCreateTableSuffix());
			arguments.put("create-index", tableManager.getCreateIndexPrefix());
			arguments.put("create-index-suffix", tableManager.getCreateIndexSuffix());
			arguments.put("index-table-prefix", tableManager.getIndexTablePrefix());
		} else {
			arguments.put("table-suffix", "");
			arguments.put("drop-table", "DROP TABLE ");
			arguments.put("create-table", "CREATE TABLE ");
			arguments.put("create-table-suffix", "");
			arguments.put("create-index", "CREATE INDEX ");
			arguments.put("create-index-suffix", "");
			arguments.put("index-table-prefix", "");
		}

		Map<String, List<String>> listArguments = new HashMap<String, List<String>>();
		if (indexType == 0) {
			// full index
			listArguments.put("column-list", Collections.singletonList(", " + dataModel.getUniversalPrimaryKey().columnList(null)));
			listArguments.put("column-list-from", Collections.singletonList(", " + dataModel.getUniversalPrimaryKey().columnList("FROM_")));
			listArguments.put("column-list-to", Collections.singletonList(", " + dataModel.getUniversalPrimaryKey().columnList("TO_")));
		} else if (indexType == 1) {
			// single column indexes
			List<String> cl = new ArrayList<String>();
			List<String> clFrom = new ArrayList<String>();
			List<String> clTo = new ArrayList<String>();
			for (Column c : dataModel.getUniversalPrimaryKey().getColumns()) {
				cl.add(", " + c.name);
				clFrom.add(", FROM_" + c.name);
				clTo.add(", FROM_" + c.name);
			}
			listArguments.put("column-list", cl);
			listArguments.put("column-list-from", clFrom);
			listArguments.put("column-list-to", clTo);
		} else {
			// minimal index
			listArguments.put("column-list", Collections.singletonList(""));
			listArguments.put("column-list-from", Collections.singletonList(""));
			listArguments.put("column-list-to", Collections.singletonList(""));
		}
		String ddl = PrintUtil.applyTemplate(template, arguments, listArguments);

		if (session != null) {
			// try {
			File tmp = CommandLineParser.getInstance().newFile("jailer_ddl.sql");
			PrintWriter pw = new PrintWriter(tmp);
			pw.println(ddl);
			pw.close();
			SqlScriptExecutor.executeScript(tmp.getCanonicalPath(), session);
			// } finally {
			// session.shutDown();
			// }
		}
		System.out.println(ddl);

		return true;
	}

	/**
	 * Checks whether working-tables schema is up-to-date.
	 * 
	 * @return <code>true</code> if working-tables schema is up-to-date
	 */
	public static boolean isUptodate(String driverClass, String dbUrl, String user, String password) {
		try {
			if (driverClass != null) {
				final Session session = new Session(driverClass, dbUrl, user, password);
				try {
					final boolean[] uptodate = new boolean[] { false };
					final DataModel datamodel = new DataModel();
					final Map<String, String> typeReplacement = Configuration.forDbms(session).getTypeReplacement();
					session.executeQuery("Select jvalue from " + SQLDialect.CONFIG_TABLE_ + " where jversion='" + Jailer.VERSION + "' and jkey='upk'",
							new Session.ResultSetReader() {
								public void readCurrentRow(ResultSet resultSet) throws SQLException {
									String contraint = session.dbms == DBMS.SYBASE ? " NULL" : "";
									String universalPrimaryKey = datamodel.getUniversalPrimaryKey().toSQL(null, contraint, typeReplacement);
									uptodate[0] = resultSet.getString(1).equals("" + universalPrimaryKey.hashCode());
								}

								public void close() {
								}
							});
					// look for jailer tables
					for (String table : SqlUtil.JAILER_MH_TABLES) {
						session.executeQuery("Select * from " + table + " Where 1=0", new Session.ResultSetReader() {
							public void readCurrentRow(ResultSet resultSet) throws SQLException {
							}
							public void close() {
							}
						});
					}
					return uptodate[0];
				} catch (Exception e) {
					return false;
				} finally {
					session.shutDown();
				}
			}
		} catch (Exception e) {
			return false;
		}

		return false;
	}

	/**
	 * Checks for conflicts of existing tables and working-tables.
	 * 
	 * @return name of table in conflict or <code>null</code>
	 */
	public static String getTableInConflict(String driverClass, String dbUrl, String user, String password) {
		try {
			if (driverClass != null) {
				Session session = new Session(driverClass, dbUrl, user, password);
				session.setSilent(true);
				try {
					final boolean[] uptodate = new boolean[] { false };
					session.executeQuery("Select jvalue from " + SQLDialect.CONFIG_TABLE_
							+ " where jkey='magic' and jvalue='837065098274756382534403654245288'", new Session.ResultSetReader() {
						public void readCurrentRow(ResultSet resultSet) throws SQLException {
							uptodate[0] = true;
						}

						public void close() {
						}
					});
					if (uptodate[0]) {
						return null;
					}
				} catch (Exception e) {
					// fall through
				}

				// look for jailer tables
				for (String table : SqlUtil.JAILER_TABLES) {
					try {
						session.executeQuery("Select * from " + table + " Where 1=0", new Session.ResultSetReader() {
							public void readCurrentRow(ResultSet resultSet) throws SQLException {
							}

							public void close() {
							}
						});
						session.shutDown();
						return table;
					} catch (Exception e) {
						// fall through
					}
				}
				session.shutDown();
			}
			return null;
		} catch (Exception e) {
			return null;
		}
	}

}
