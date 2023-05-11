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
package net.sf.jailer.ddl;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.sql.DataSource;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.JailerVersion;
import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.SQLDialect;
import net.sf.jailer.database.Session;
import net.sf.jailer.database.TemporaryTableManager;
import net.sf.jailer.database.WorkingTableScope;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.PrimaryKey;
import net.sf.jailer.datamodel.PrimaryKeyFactory;
import net.sf.jailer.datamodel.RowIdSupport;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.PrintUtil;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlScriptExecutor;
import net.sf.jailer.util.SqlUtil;

/**
 * Creates the DDL for the working-tables.
 *
 * @author Ralf Wisser
 */
public class DDLCreator {

	/**
	 * The execution context.
	 */
	private final ExecutionContext executionContext;

	/**
	 * Constructor.
	 *
	 * @param executionContext the command line arguments
	 */
	public DDLCreator(ExecutionContext executionContext) {
		this.executionContext = executionContext;
	}

	/**
	 * Creates the DDL for the working-tables.
	 */
	public boolean createDDL(DataSource dataSource, DBMS dbms, WorkingTableScope temporaryTableScope, String workingTableSchema) throws SQLException, FileNotFoundException, IOException {
		Session session = null;
		if (dataSource != null) {
			session = new Session(dataSource, dbms, executionContext.getIsolationLevel());
		}
		try {
			return createDDL(new DataModel(null, null, new HashMap<String, String>(), null, new PrimaryKeyFactory(executionContext), executionContext, true, null), session, temporaryTableScope, workingTableSchema);
		} finally {
			if (session != null) {
				try { session.shutDown(); } catch (Exception e) { /* ignore */ }
			}
		}
	}

	/**
	 * Creates the DDL for the working-tables.
	 */
	public void createDDL(Session localSession, WorkingTableScope temporaryTableScope, String workingTableSchema) throws FileNotFoundException, IOException, SQLException {
		// TODO register all current export processes.
		// Fail if a process is still active.
		// Use a heard beat concept to detect dead processes
		// Use this to create unique entity-graph-ids
		createDDL(new DataModel(executionContext), localSession, temporaryTableScope, workingTableSchema);
	}

	/**
	 * Creates the DDL for the working-tables.
	 */
	public boolean createDDL(DataModel datamodel, Session session, WorkingTableScope temporaryTableScope, String workingTableSchema) throws FileNotFoundException, IOException, SQLException {
		RowIdSupport rowIdSupport = new RowIdSupport(datamodel, targetDBMS(session), executionContext);
		if (session != null && session.dbms.getExperimentalTypeReplacement() != null && !session.dbms.getExperimentalTypeReplacement().isEmpty()) {
			Map<String, String> oldTypeReplacement = session.dbms.getTypeReplacement();
			if (oldTypeReplacement == null) {
				session.dbms.setTypeReplacement(session.dbms.getExperimentalTypeReplacement());
			} else {
				HashMap<String, String> repl = new HashMap<String, String>(session.dbms.getTypeReplacement());
				repl.putAll(session.dbms.getExperimentalTypeReplacement());
				session.dbms.setTypeReplacement(repl);
			}
			try {
				return createDDL(datamodel, session, temporaryTableScope, rowIdSupport, workingTableSchema);
			} catch (Throwable t) {
				// fall through
			} finally {
				session.dbms.setTypeReplacement(oldTypeReplacement);
			}
		}
		return createDDL(datamodel, session, temporaryTableScope, rowIdSupport, workingTableSchema);
	}

	/**
	 * Creates the DDL for the working-tables.
	 */
	public boolean createDDL(DataModel datamodel, Session session, WorkingTableScope temporaryTableScope, RowIdSupport rowIdSupport, String workingTableSchema) throws FileNotFoundException, IOException, SQLException {
		try {
			return createDDL(datamodel, session, temporaryTableScope, rowIdSupport, workingTableSchema, true);
		} catch (SQLException e) {
			// try without table property
		}
		return createDDL(datamodel, session, temporaryTableScope, rowIdSupport, workingTableSchema, false);
	}

	/**
	 * Creates the DDL for the working-tables.
	 */
	private boolean createDDL(DataModel datamodel, Session session, WorkingTableScope temporaryTableScope, RowIdSupport rowIdSupport, String workingTableSchema, boolean withTableProperties) throws FileNotFoundException, IOException, SQLException {
		uPKWasTooLong = false;
		try {
			return createDDL(datamodel, session, temporaryTableScope, 0, false, rowIdSupport, workingTableSchema, withTableProperties);
		} catch (SQLException e) {
			uPKWasTooLong = true;
			try {
				// [bugs:#37] PostreSQL: transactional execution
				session.getConnection().commit();
			} catch (SQLException e1) {
				// ignore
			}
		}
		// reconnect and retry with another index type
		session.reconnect();
		try {
			return createDDL(datamodel, session, temporaryTableScope, 1, false, rowIdSupport, workingTableSchema, withTableProperties);
		} catch (SQLException e) {
			try {
				// [bugs:#37] PostreSQL: transactional execution
				session.getConnection().commit();
			} catch (SQLException e1) {
				// ignore
			}
		}
		// reconnect and retry with another index type
		session.reconnect();
		return createDDL(datamodel, session, temporaryTableScope, 2, false, rowIdSupport, workingTableSchema, withTableProperties);
	}

	public static volatile boolean uPKWasTooLong = false;
	public static volatile String uPK;

	/**
	 * Creates the DDL for the working-tables.
	 */
	private boolean createDDL(DataModel dataModel, Session session, WorkingTableScope temporaryTableScope, int indexType, boolean uniqueIndex, RowIdSupport rowIdSupport, String workingTableSchema, boolean withTableProperties) throws FileNotFoundException, IOException, SQLException {
		String template = "script" + File.separator + "ddl-template.sql";
		String contraint = pkColumnConstraint(session);
		Map<String, String> typeReplacement = targetDBMS(session).getTypeReplacement();
		PrimaryKey upk = rowIdSupport.getUniversalPrimaryKey();
		if (upk.getColumns().isEmpty()) {
			Table table = null;
			if (executionContext.getUpkDomain() != null) {
				for (String tableName: executionContext.getUpkDomain()) {
					table = dataModel.getTable(tableName);
					break;
				}
			}
			throw new DataModel.NoPrimaryKeyException(table);
		}
		String universalPrimaryKey = upk.toSQL(null, contraint, typeReplacement);
		uPK = universalPrimaryKey;
		Map<String, String> arguments = new HashMap<String, String>();
		arguments.put("upk", universalPrimaryKey);
		String tableProperties = targetDBMS(session).getTableProperties();
		arguments.put("upk-hash", "" + ((universalPrimaryKey + tableProperties).hashCode()));
		arguments.put("pre", upk.toSQL("PRE_", contraint, typeReplacement));
		arguments.put("from", upk.toSQL("FROM_", contraint, typeReplacement));
		arguments.put("to", upk.toSQL("TO_", contraint, typeReplacement));
		arguments.put("version", "" + JailerVersion.WORKING_TABLE_VERSION);
		arguments.put("constraint", contraint);

		TemporaryTableManager tableManager = null;
		if (temporaryTableScope == WorkingTableScope.SESSION_LOCAL) {
			tableManager = targetDBMS(session).getSessionTemporaryTableManager();
		}
		if (temporaryTableScope == WorkingTableScope.TRANSACTION_LOCAL) {
			tableManager = targetDBMS(session).getTransactionTemporaryTableManager();
		}
		String tableName = SQLDialect.CONFIG_TABLE_;
		arguments.put("config-dml-reference", tableName);
		String schema = workingTableSchema != null? (session == null? workingTableSchema : Quoting.getQuoting(session).requote(workingTableSchema)) + "." : "";
		arguments.put("schema", schema);
		arguments.put("index-schema", supportsSchemasInIndexDefinitions(session)? schema : "");

		Map<String, List<String>> listArguments = new HashMap<String, List<String>>();
		if (indexType == 0) {
			// full index
			final int MAX_INDEX_SIZE = 12;
			PrimaryKey pk = upk;
			if (pk.numberOfIndexedPKColumns < pk.getColumns().size()) {
				uniqueIndex = false;
				int iSize = pk.getColumns().size();
				if (iSize > MAX_INDEX_SIZE) {
					iSize = Math.max(MAX_INDEX_SIZE, pk.numberOfIndexedPKColumns);
				}
				if (iSize < pk.getColumns().size()) {
					pk = new PrimaryKey(new ArrayList<Column>(pk.getColumns().subList(0, iSize)), false);
				}
			}

			listArguments.put("column-list", Collections.singletonList(", " + pk.columnList(null)));
			listArguments.put("column-list-from", Collections.singletonList(", " + pk.columnList("FROM_")));
			listArguments.put("column-list-to", Collections.singletonList(", " + pk.columnList("TO_")));
		} else if (indexType == 1) {
			// single column indexes
			List<String> cl = new ArrayList<String>();
			List<String> clFrom = new ArrayList<String>();
			List<String> clTo = new ArrayList<String>();
			for (Column c : upk.getColumns()) {
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
		if (tableManager != null) {
			arguments.put("table-suffix", "_T");
			arguments.put("drop-table", tableManager.getDropTablePrefix());
			arguments.put("create-table", tableManager.getCreateTablePrefix());
			arguments.put("create-table-suffix", tableManager.getCreateTableSuffix());
			arguments.put("create-index", tableManager.getCreateIndexPrefix());
			arguments.put("create-unique-index", tableManager.getCreateIndexPrefix());
			arguments.put("create-index-suffix", tableManager.getCreateIndexSuffix());
			arguments.put("index-table-prefix", tableManager.getIndexTablePrefix());
			arguments.put("schema", schema + tableManager.getDdlTableReferencePrefix());
		} else {
			String suffix = "";
			String prefix = "";
			if (withTableProperties) {
				prefix = tableProperties.replaceFirst("^\\s*CREATE\\s+(.*)\\s+TABLE\\s*$", "$1");
				if (prefix.equals(tableProperties)) {
					prefix = "";
					suffix = tableProperties;
				}
			}
			arguments.put("table-suffix", "");
			arguments.put("drop-table", "DROP TABLE ");
			arguments.put("create-table", !"".equals(prefix)? "CREATE " + prefix + " TABLE " : "CREATE TABLE ");
			arguments.put("create-table-suffix", suffix);
			arguments.put("create-index", "CREATE INDEX ");
			arguments.put("create-unique-index", uniqueIndex? "CREATE UNIQUE INDEX " : "CREATE INDEX ");
			arguments.put("create-index-suffix", "");
			arguments.put("index-table-prefix", "");
		}

		String ddl = new PrintUtil().applyTemplate(template, arguments, listArguments);

		if (session != null) {
			File tmp = Configuration.getInstance().createTempFile();
			PrintWriter pw = new PrintWriter(tmp);
			pw.println(ddl);
			pw.close();
			new SqlScriptExecutor(session, 1).executeScript(tmp.getPath());
			tmp.delete();
		} else {
			System.out.println(ddl);
		}

		return true;
	}

	private boolean supportsSchemasInIndexDefinitions(Session session) {
		Boolean result = targetDBMS(session).getSupportsSchemasInIndexDefinitions();
		if (result == null) {
			if (session == null) {
				return true;
			}
			try {
				result = session.getMetaData().supportsSchemasInDataManipulation();
			} catch (SQLException e) {
				return false;
			}
		}
		return result;
	}

	private String pkColumnConstraint(Session session) {
		String nullableContraint = targetDBMS(session).getNullableContraint();
		if (nullableContraint != null) {
			return " " + nullableContraint;
		}
		return "";
	}

	private DBMS targetDBMS(Session session) {
		if (session == null) {
			if (executionContext.getTargetDBMS() != null) {
				return executionContext.getTargetDBMS();
			}
			return DBMS.forDBMS(null); // default
		}
		return session.dbms;
	}

	/**
	 * Checks whether working-tables schema is up-to-date.
	 * @param useRowId
	 * @param workingTableSchema
	 *
	 * @return <code>true</code> if working-tables schema is up-to-date
	 */
	public boolean isUptodate(DataSource dataSource, DBMS dbms, boolean useRowId, boolean useRowIdsOnlyForTablesWithoutPK, String workingTableSchema) {
		try {
			if (dataSource != null) {
				final Session session = new Session(dataSource, dbms, executionContext.getIsolationLevel());
				try {
					return isUptodate(session, useRowId, useRowIdsOnlyForTablesWithoutPK, workingTableSchema);
				} finally {
					session.shutDown();
				}
			}
		} catch (Exception e) {
		}
		return false;
	}

	/**
	 * Checks whether working-tables schema is up-to-date.
	 * @param useRowId
	 * @param workingTableSchema
	 *
	 * @return <code>true</code> if working-tables schema is up-to-date
	 */
	public boolean isUptodate(final Session session, boolean useRowId, boolean useRowIdsOnlyForTablesWithoutPK, String workingTableSchema) {
		try {
			boolean wasSilent = session.getSilent();
			try {
				session.setSilent(true);
				final boolean[] uptodate = new boolean[] { false };
				final DataModel datamodel = new DataModel(executionContext);
				final Map<String, String> typeReplacement = targetDBMS(session).getTypeReplacement();
				final RowIdSupport rowIdSupport = new RowIdSupport(datamodel, targetDBMS(session), useRowId, useRowIdsOnlyForTablesWithoutPK);

				final String schema = workingTableSchema == null ? "" : Quoting.getQuoting(session).requote(workingTableSchema) + ".";

				Session.ResultSetReader reader = new Session.ResultSetReader() {
					@Override
					public void readCurrentRow(ResultSet resultSet) throws SQLException {
						String contraint = pkColumnConstraint(session);
						String universalPrimaryKey = rowIdSupport.getUniversalPrimaryKey().toSQL(null, contraint, typeReplacement);
						uPK = universalPrimaryKey;
						String h = "" + (universalPrimaryKey + targetDBMS(session).getTableProperties()).hashCode();
						uptodate[0] = resultSet.getString(1).equals(h);
					}

					@Override
					public void close() {
					}
				};
				if (!uptodate[0]) {
					session.executeQuery("Select jvalue from " + schema + SQLDialect.CONFIG_TABLE_ + " where jversion='" + JailerVersion.WORKING_TABLE_VERSION + "' and jkey='upk'", reader);
				}
				// look for jailer tables
				for (String table : SqlUtil.JAILER_MH_TABLES) {
					session.executeQuery("Select * from " + schema + table + " Where 1=0", new Session.ResultSetReader() {
						@Override
						public void readCurrentRow(ResultSet resultSet) throws SQLException {
						}
						@Override
						public void close() {
						}
					});
				}
				if (uptodate[0]) {
					String testId = "ID:" + System.currentTimeMillis();
					session.executeUpdate(
							"INSERT INTO " + schema + SQLDialect.CONFIG_TABLE_ + "(jversion, jkey, jvalue) " +
							"VALUES ('" + JailerVersion.WORKING_TABLE_VERSION + "', '" + testId + "', 'ok')");
					session.executeUpdate(
							"DELETE FROM " + schema + SQLDialect.CONFIG_TABLE_ + " " +
							"WHERE jversion='" + JailerVersion.WORKING_TABLE_VERSION + "' and jkey='" + testId + "'");
				}
				return uptodate[0];
			} catch (Throwable t) {
				try {
					// [bugs:#37] PostreSQL: transactional execution
					session.getConnection().commit();
				} catch (SQLException e1) {
					// ignore
				}
				return false;
			} finally {
				session.setSilent(wasSilent);
			}
		} catch (Throwable e) {
			return false;
		}
	}

	/**
	 * Checks for conflicts of existing tables and working-tables.
	 *
	 * @return name of table in conflict or <code>null</code>
	 */
	public String getTableInConflict(DataSource dataSource, DBMS dbms) {
		try {
			if (dataSource != null) {
				Session session = new Session(dataSource, dbms, executionContext.getIsolationLevel());
				session.setSilent(true);
				try {
					final boolean[] uptodate = new boolean[] { false };
					session.executeQuery("Select jvalue from " + SQLDialect.CONFIG_TABLE_
							+ " where jkey='magic' and jvalue='837065098274756382534403654245288'", new Session.ResultSetReader() {
						@Override
						public void readCurrentRow(ResultSet resultSet) throws SQLException {
							uptodate[0] = true;
						}

						@Override
						public void close() {
						}
					});
					if (uptodate[0]) {
						session.shutDown();
						return null;
					}
				} catch (Exception e) {
					// fall through
				}

				// look for jailer tables
				for (String table : SqlUtil.JAILER_TABLES) {
					try {
						session.executeQuery("Select * from " + table + " Where 1=0", new Session.ResultSetReader() {
							@Override
							public void readCurrentRow(ResultSet resultSet) throws SQLException {
							}

							@Override
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
