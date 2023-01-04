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
package net.sf.jailer.entitygraph.intradatabase;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.SQLDialect;
import net.sf.jailer.database.Session;
import net.sf.jailer.database.SqlException;
import net.sf.jailer.database.StatementBuilder;
import net.sf.jailer.database.UpdateTransformer;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Filter;
import net.sf.jailer.datamodel.PrimaryKey;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.entitygraph.EntityGraph;
import net.sf.jailer.entitygraph.remote.RemoteEntityGraph;
import net.sf.jailer.util.JobManager;
import net.sf.jailer.util.LogUtil;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlScriptExecutor;


/**
 * Specialized {@link RemoteEntityGraph} for exporting data into a different
 * schema within the same database.
 *
 * @author Ralf Wisser
 */
public class IntraDatabaseEntityGraph extends RemoteEntityGraph {

	private boolean upsertOnly;

	/**
	 * Constructor.
	 *
	 * @param graphID
	 *            the unique ID of the graph
	 * @param session
	 *            for executing SQL-Statements
	 * @param universalPrimaryKey
	 *            the universal primary key
	 * @throws SQLException
	 */
	private IntraDatabaseEntityGraph(DataModel dataModel, int graphID,
			Session session, PrimaryKey universalPrimaryKey, Runnable updateStatistics, ExecutionContext executionContext) throws SQLException {
		super(dataModel, graphID, session, universalPrimaryKey, updateStatistics, executionContext);
		upsertOnly = executionContext.getUpsertOnly();
		synchronized (this) {
			upsertStrategy = null;
			upsertStrategies = new ArrayList<UpsertStrategy>();

			if (DBMS.POSTGRESQL.equals(session.dbms)) {
				upsertStrategies.add(new UpsertPGUS());
			} else {
				upsertStrategies.add(new MergeUS(true));
				upsertStrategies.add(new MergeUS(false));
				upsertStrategies.add(new UpsertMYSQLUS());
				upsertStrategies.add(new UpsertPGUS());
				upsertStrategies.add(new UpsertStandardUS());
			}
		}
		quoting = Quoting.getQuoting(session);
	}

	private Quoting quoting = null;

	/**
	 * Creates a new entity-graph.
	 *
	 * @param graphID
	 *            the unique ID of the graph
	 * @param session
	 *            for executing SQL-Statements
	 * @param universalPrimaryKey
	 *            the universal primary key
	 * @return the newly created entity-graph
	 */
	public static IntraDatabaseEntityGraph create(DataModel dataModel,
			int graphID, Session session, PrimaryKey universalPrimaryKey, Runnable updateStatistics, ExecutionContext executionContext) throws SQLException {
		IntraDatabaseEntityGraph entityGraph = new IntraDatabaseEntityGraph(
				dataModel, graphID, session, universalPrimaryKey, updateStatistics, executionContext);
		init(graphID, session, executionContext);
		return entityGraph;
	}

	/**
	 * Copies an entity-graph.
	 *
	 * @param newGraphID
	 *            the unique ID of the new graph
	 * @param session
	 *            for executing SQL-Statements
	 * @return the newly created entity-graph
	 */
	@Override
	public EntityGraph copy(int newGraphID, Session session)
			throws SQLException {
		IntraDatabaseEntityGraph entityGraph = create(dataModel, newGraphID,
				session, universalPrimaryKey, null, executionContext);
		entityGraph.setBirthdayOfSubject(birthdayOfSubject);
		session.executeUpdate("Insert into "
				+ SQLDialect.dmlTableReference(ENTITY, session, executionContext)
				+ "(r_entitygraph, " + universalPrimaryKey.columnList(null)
				+ ", birthday, orig_birthday, type) " + "Select " + newGraphID
				+ ", " + universalPrimaryKey.columnList(null)
				+ ", birthday, birthday, type From "
				+ SQLDialect.dmlTableReference(ENTITY, session, executionContext)
				+ " Where r_entitygraph=" + graphID + "");
		entityGraph.setTransformerFactory(getTransformerFactory());
		return entityGraph;
	}

	/**
	 * Creates a new entity-graph of same type and session.
	 */
	public EntityGraph createNewGraph() throws SQLException {
		IntraDatabaseEntityGraph entityGraph = create(dataModel, createUniqueGraphID(),
				session, universalPrimaryKey, null, executionContext);
		entityGraph.setBirthdayOfSubject(birthdayOfSubject);
		return entityGraph;
	}

	private final String COLUMN_PREFIX = "JALR_";

	/**
	 * Reads all entities of a given table which are marked as independent or as
	 * roots.
	 *
	 * @param table
	 *            the table
	 * @param orderByPK
	 *            not used
	 */
	@Override
	public void readMarkedEntities(Table table, boolean orderByPK)
			throws SQLException {
		String selectionSchema = filteredSelectionClause(table, COLUMN_PREFIX, quoting, true);
		readEntitiesByQuery(table, "Select " + selectionSchema + " From "
				+ SQLDialect.dmlTableReference(ENTITY, session, executionContext) + " E join "
				+ quoting.requote(table.getName()) + " T on "
				+ pkEqualsEntityID(table, "T", "E")
				+ " Where (E.birthday=0 and E.r_entitygraph=" + graphID
				+ " and E.type=" + typeName(table) + ")");
	}

	/**
	 * Reads all entities of a given table.
	 *
	 * @param table
	 *            the table
	 * @param orderByPK
	 *            not used
	 */
	@Override
	public void readEntities(Table table, boolean orderByPK)
			throws SQLException {
		long incSize = session.dbms.getLimitTransactionSize().getSize(executionContext);
		if (incSize > 0) {
			String update =
					"Update " + session.dbms.getLimitTransactionSize().afterSelectFragment(executionContext) + SQLDialect.dmlTableReference(ENTITY, session, executionContext) + " " +
					"Set birthday=0 " +
					"Where (birthday>=0 and r_entitygraph=" + graphID + " " +
					"and type=" + typeName(table) + ") " + session.dbms.getLimitTransactionSize().additionalWhereConditionFragment(executionContext) +
					session.dbms.getLimitTransactionSize().statementSuffixFragment(executionContext);
			for (;;) {
				long rc = session.executeUpdate(update);
				if (rc <= 0) {
					break;
				}
				readMarkedEntities(table, orderByPK);
				session.executeUpdate(
						"Delete from " + SQLDialect.dmlTableReference(ENTITY, session, executionContext) + " " +
						"Where birthday=0 and r_entitygraph=" + graphID + " " +
						"and type=" + typeName(table) + "");
				if (rc != incSize) {
					break;
				}
			}
		} else {
			readEntitiesByQuery(table, "Select " + filteredSelectionClause(table, COLUMN_PREFIX, quoting, true) + " From "
				+ SQLDialect.dmlTableReference(ENTITY, session, executionContext) + " E join "
				+ quoting.requote(table.getName()) + " T on "
				+ pkEqualsEntityID(table, "T", "E")
				+ " Where (E.birthday>=0 and E.r_entitygraph=" + graphID
				+ " and E.type=" + typeName(table) + ")");
		}
	}

	/**
	 * Updates columns of a table.
	 *
	 * @param table the table
	 * @param columns the columns;
	 * @param inSourceSchema if <code>true</code>, use source-schema-mapping, else use schema-mapping
	 * @param reason to be written as comment
	 */
	@Override
	public void updateEntities(Table table, Set<Column> columns, OutputStreamWriter scriptFileWriter, DBMS targetConfiguration, boolean inSourceSchema, String reason) throws SQLException {
		File tmp = Configuration.getInstance().createTempFile();
		OutputStreamWriter tmpFileWriter;
		try {
			tmpFileWriter = new FileWriter(tmp);
			UpdateTransformer reader = new UpdateTransformer(table, columns, tmpFileWriter, executionContext.getNumberOfEntities(), getSession(), getSession().dbms, importFilterManager, inSourceSchema, reason, executionContext);
			readEntities(table, false, reader);
			tmpFileWriter.close();
			new SqlScriptExecutor(getSession(), executionContext.getNumberOfThreads()).executeScript(tmp.getPath());
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		tmp.delete();
	}

	/**
	 * Reads all entities of a given table.
	 *
	 * @param table
	 *            the table
	 * @param sql retrieves the entities
	 */
	private void readEntitiesByQuery(Table table, String sql) throws SQLException {
		boolean tableHasIdentityColumn = false;
		if (session.dbms.isIdentityInserts()) {
			for (Column c: table.getColumns()) {
				if (c.isIdentityColumn) {
					tableHasIdentityColumn = true;
					break;
				}
			}
		}
		long rc;
		if (tableHasIdentityColumn) {
			synchronized (session.getConnection()) {
				session.executeUpdate("SET IDENTITY_INSERT " + qualifiedTableName(table) + " ON");
				if (table.getUpsert() || upsertOnly) {
					rc = upsertRows(table, sql, true);
				} else {
					rc = insertRows(table, sql);
				}
				session.executeUpdate("SET IDENTITY_INSERT " + qualifiedTableName(table) + " OFF");
			}
		} else {
			if (table.getUpsert() || upsertOnly) {
				rc = upsertRows(table, sql, true);
			} else {
				rc = insertRows(table, sql);
			}
		}
		executionContext.getProgressListenerRegistry().fireExported(table, rc);
		addExportedCount(rc);
	}

	/**
	 * Gets qualified table name.
	 *
	 * @param t
	 *            the table
	 * @return qualified name of t
	 */
	private String qualifiedTableName(Table t) {
		String schema = t.getOriginalSchema("");
		String mappedSchema = executionContext
				.getSchemaMapping().get(schema);
		if (mappedSchema != null) {
			schema = mappedSchema;
		}
		if (schema.length() == 0) {
			return quoting.requote(t.getUnqualifiedName());
		}
		return quoting.requote(schema) + "." + quoting.requote(t.getUnqualifiedName());
	}

	/**
	 * Checks if columns is part of primary key.
	 *
	 * @param column
	 *            the column
	 * @return <code>true</code> if column is part of primary key
	 */
	private boolean isPrimaryKeyColumn(Table table, String column) {
		for (Column c : table.primaryKey.getColumns()) {
			if (c.name.equalsIgnoreCase(column)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Inserts rows into a table. Falls back to {@link #upsertRows(Table, String, boolean)} on error.
	 *
	 * @param table the table
	 * @param sqlSelect the rows to insert
	 * @return row count
	 */
	private long insertRows(Table table, String sqlSelect) throws SQLException {
		StringBuilder sb = new StringBuilder();
		String labelCSL = insertClause(table, null, null);
		sb.append("Insert into " + qualifiedTableName(table) + "(" + labelCSL
				+ ") " + sqlSelect);

		boolean silent = session.getSilent();
		session.setSilent(true);
		try {
			return session.executeUpdate(sb.toString());
		} catch (SQLException e) {
			try {
				// try upsert
				return upsertRows(table, sqlSelect, true);
			} catch (Exception uEx) {
				Session._log.warn("", uEx);
				LogUtil.warn(uEx);
				// throw original exception
				throw e;
			}
		} finally {
			session.setSilent(silent);
		}
	}

	/**
	 * Upserts rows of a table. Tries all {@link UpsertStrategy}s until it find one that works.
	 *
	 * @param table the table
	 * @param sqlSelect the rows to be upserted
	 * @param retry if <code>true</code>, try all {@link UpsertStrategy}s
	 * @return row count
	 */
	private long upsertRows(Table table, String sqlSelect, boolean retry) throws SQLException {
		if (table.primaryKey.getColumns().isEmpty()) {
			throw new RuntimeException("Unable to merge/upsert into table \""
					+ table.getName() + "\".\n" + "No primary key.");
		}

		if (upsertStrategies.size() == 1) {
			return upsertStrategies.get(0).upsert(table, sqlSelect);
		}

		UpsertStrategy us;
		boolean done = false;
		StringBuilder sqlErrorMessages = new StringBuilder();
		StringBuilder sqlErrorStatements = new StringBuilder();
		String firstSQLState = null;
		long rc = 0;
		synchronized (this) {
			us = upsertStrategy;
		}
		if (us == null) {
			for (UpsertStrategy strategy: upsertStrategies) {
				boolean silent = session.getSilent();
				session.setSilent(true);
				try {
					rc = strategy.upsert(table, sqlSelect);
					synchronized (this) {
						upsertStrategy = strategy;
						us = upsertStrategy;
					}
					done = true;
					break;
				} catch (SQLException e) {
					sqlErrorMessages.append("  - " + e.getMessage().replaceAll("\\s+", " ") + "\n");
					if (e instanceof SqlException) {
						sqlErrorStatements.append("- " + ((SqlException) e).sqlStatement.replaceAll("\\s+", " ") + "\n");
					}
					if (firstSQLState == null) {
						firstSQLState = e.getSQLState();
					}
					// try another strategy
				} finally {
					session.setSilent(silent);
				}
			}
			if (!done) {
				synchronized (this) {
					upsertStrategy = upsertStrategies.get(0);
					us = upsertStrategy;
				}
			}
		}

		if (!done) {
			if (retry) {
				boolean silent = session.getSilent();
				session.setSilent(true);
				try {
					rc = us.upsert(table, sqlSelect);
				} catch (SQLException e) {
					synchronized (this) {
						upsertStrategy = null;
					}
					try {
						return upsertRows(table, sqlSelect, false);
					} catch (SQLException e2) {
						if (e instanceof SqlException) {
							if (sqlErrorMessages.length() > 0) {
								SqlException se = new SqlException(
										"Tried various update strategies but all failed:\n" +
										sqlErrorMessages.toString(), sqlErrorStatements.toString(), null, firstSQLState);
								se.setFormatted(true);
								throw se;
							} else {
								throw new SqlException(e.getMessage() + sqlErrorMessages, ((SqlException) e).sqlStatement, null, firstSQLState);
							}
						}
						throw new SQLException(e.getMessage() + sqlErrorMessages, e);
					}
				} finally {
					session.setSilent(silent);
				}
			} else {
				rc = us.upsert(table, sqlSelect);
			}
		}

		return rc;
	}

	/**
	 * Gets insert clause for inserting rows of given type with respect of the
	 * column filters.
	 *
	 * @param table
	 *            the table to read rows from
	 * @return insert clause
	 */
	private String insertClause(Table table, String tableAlias, String columnPrefix) {
		StringBuilder sb = new StringBuilder();
		boolean first = true;

		for (Column c : getSelectionClause(table)) {
			if (!first) {
				sb.append(", ");
			}
			if (tableAlias != null) {
				sb.append(tableAlias + ".");
			}
			sb.append(prefixColumnName(columnPrefix, quoting, c));
			first = false;
		}

		return sb.toString();
	}

	private long insertWhereNotExists(Table table, String sqlSelect) throws SQLException {
		StatementBuilder upsertInsertStatementBuilder = new StatementBuilder(1);

		String insertHead = "Insert into " + qualifiedTableName(table)
				+ "(" + insertClause(table, null, null) + ") ";
		StringBuffer whereForTerminator = new StringBuffer("");

		// assemble 'where'
		boolean f = true;
		for (Column pk : table.primaryKey.getColumns()) {
			if (!f) {
				whereForTerminator.append(" and ");
			}
			f = false;
			whereForTerminator.append("T." + quoting.requote(pk.name) + "=Q." + prefixColumnName(COLUMN_PREFIX, quoting, pk));
		}

		insertHead += "Select " + insertClause(table, "Q", COLUMN_PREFIX) + " From (";
		StringBuffer terminator = new StringBuffer(") as Q"
				+ " Where not exists (Select * from "
				+ qualifiedTableName(table) + " T " + "Where ");
		terminator.append(whereForTerminator + ")");

		upsertInsertStatementBuilder.append(insertHead, sqlSelect, "",
				terminator.toString());

		String sql = upsertInsertStatementBuilder.build();
		return session.executeUpdate(sql);
	}

	/**
	 * A strategy to upsert (merge) rows.
	 */
	private interface UpsertStrategy {
		long upsert(Table table, String sqlSelect)
				throws SQLException;
	}

	/**
	 * Uses "MERGE INTO" statements to upsert rows.
	 */
	private class MergeUS implements UpsertStrategy {
		private final boolean withSemicolon;

		public MergeUS(boolean withSemicolon) {
			this.withSemicolon = withSemicolon;
		}

		@Override
		public long upsert(Table table, String sqlSelect)
				throws SQLException {
			String labelCSL = insertClause(table, null, null);
			StatementBuilder upsertInsertStatementBuilder = new StatementBuilder(1);

			String insertHead = "Insert into " + qualifiedTableName(table)
					+ "(" + labelCSL + ") ";
			StringBuffer whereForTerminator = new StringBuffer("");

			// assemble 'where'
			boolean f = true;
			for (Column pk : table.primaryKey.getColumns()) {
				if (!f) {
					whereForTerminator.append(" and ");
				}
				f = false;
				whereForTerminator.append(applyImportFilter("T." + quoting.requote(pk.name), pk) + "=Q." + prefixColumnName(COLUMN_PREFIX, quoting, pk));
			}

			insertHead = "MERGE INTO " + qualifiedTableName(table)
					+ " T USING(";
			StringBuffer terminator = new StringBuffer(") Q ON("
					+ whereForTerminator + ") ");

			StringBuffer sets = new StringBuffer();
			StringBuffer tSchema = new StringBuffer();
			StringBuffer iSchema = new StringBuffer();
			for (Column column : table.getColumns()) {
				if (!isPrimaryKeyColumn(table, column.name)) {
					if (sets.length() > 0) {
						sets.append(", ");
					}
					sets.append("T." + quoting.requote(column.name) + "=Q." + prefixColumnName(COLUMN_PREFIX, quoting, column));
				}
				if (tSchema.length() > 0) {
					tSchema.append(", ");
				}
				tSchema.append(quoting.requote(column.name));
				if (iSchema.length() > 0) {
					iSchema.append(", ");
				}
				iSchema.append("Q." + prefixColumnName(COLUMN_PREFIX, quoting, column));
			}
			if (sets.length() > 0) {
				terminator.append("WHEN MATCHED THEN UPDATE SET " + sets + " ");
			}
			terminator.append("WHEN NOT MATCHED THEN INSERT (" + tSchema
					+ ") VALUES(" + iSchema + ")");

			upsertInsertStatementBuilder.append(insertHead, sqlSelect, "",
					terminator.toString());

			String sql = upsertInsertStatementBuilder.build();
			return session.executeUpdate(sql + (withSemicolon? ";" : ""));
		}
	}

	/**
	 * Uses "INSERT" statements followed by "UPDATE" statements. (Postgres dialect);
	 */
	private class UpsertPGUS implements UpsertStrategy {
		@Override
		public long upsert(Table table, String sqlSelect)
				throws SQLException {
			StringBuffer sets = new StringBuffer();
			StringBuffer where = new StringBuffer();
			for (Column column : table.getColumns()) {
				if (!isPrimaryKeyColumn(table, column.name)) {
					if (sets.length() > 0) {
						sets.append(", ");
					}
					sets.append(quoting.requote(column.name) + "=Q." + prefixColumnName(COLUMN_PREFIX, quoting, column));
				} else {
					if (where.length() > 0) {
						where.append(" and ");
					}
					where.append("S." + quoting.requote(column.name) + "=" + applyImportFilter("T." + quoting.requote(column.name), column));
				}
			}

			if (sets.length() == 0) {
				// nothing to do
				return 0;
			}

			String sql = "Update " + qualifiedTableName(table) + " S set " + sets + " from (" + sqlSelect + " and (" + where + ")) Q ";

			long rc = session.executeUpdate(sql);
			return rc + insertWhereNotExists(table, sqlSelect);
		}
	}

	/**
	 * Uses "INSERT" statements followed by "UPDATE" statements. (MySQL dialect);
	 */
	private class UpsertMYSQLUS implements UpsertStrategy {
		@Override
		public long upsert(Table table, String sqlSelect)
				throws SQLException {
			StringBuffer sets = new StringBuffer();
			StringBuffer where = new StringBuffer();
			for (Column column : table.getColumns()) {
				if (!isPrimaryKeyColumn(table, column.name)) {
					if (sets.length() > 0) {
						sets.append(", ");
					}
					sets.append("S." + quoting.requote(column.name) + "=Q." + prefixColumnName(COLUMN_PREFIX, quoting, column));
				} else {
					if (where.length() > 0) {
						where.append(" and ");
					}
					where.append(applyImportFilter("S." + quoting.requote(column.name), column) + "=Q." + prefixColumnName(COLUMN_PREFIX, quoting, column));
				}
			}

			if (sets.length() == 0) {
				// nothing to do
				return 0;
			}

			String sql = "Update (" + sqlSelect + ") Q join " + qualifiedTableName(table) + " S on " + where + " set " + sets;

			long rc = session.executeUpdate(sql);
			return rc + insertWhereNotExists(table, sqlSelect);
		}
	}

	/**
	 * Uses "INSERT" statements followed by "UPDATE" statements. (Standard SQL);
	 */
	private class UpsertStandardUS implements UpsertStrategy {
		@Override
		public long upsert(Table table, String sqlSelect)
				throws SQLException {

			StringBuffer nonPKList = new StringBuffer();
			StringBuffer nonPKListQ = new StringBuffer();
			StringBuffer where = new StringBuffer();
			StringBuffer whereT = new StringBuffer();
			for (Column column : table.getColumns()) {
				if (!isPrimaryKeyColumn(table, column.name)) {
					if (nonPKList.length() > 0) {
						nonPKList.append(", ");
					}
					nonPKList.append(quoting.requote(column.name));
					if (nonPKListQ.length() > 0) {
						nonPKListQ.append(", ");
					}
					nonPKListQ.append("Q." + prefixColumnName(COLUMN_PREFIX, quoting, column));
				} else {
					if (where.length() > 0) {
						where.append(" and ");
					}
					where.append(applyImportFilter("S." + quoting.requote(column.name), column) + "=Q." + prefixColumnName(COLUMN_PREFIX, quoting, column));
					if (whereT.length() > 0) {
						whereT.append(" and ");
					}
					whereT.append(applyImportFilter("S." + quoting.requote(column.name), column) + "=T." + quoting.requote(column.name));
				}
			}

			if (nonPKList.length() == 0) {
				// nothing to do
				return 0;
			}

			String sql =
					"Update " + qualifiedTableName(table) + " S set (" + nonPKList + ")" +
					" = (Select " + nonPKListQ + " From (" + sqlSelect + ") Q Where " + where + ") " +
					"Where exists (" + sqlSelect + " and (" + whereT + "))";

			long rc = session.executeUpdate(sql);
			return rc + insertWhereNotExists(table, sqlSelect);
		}
	}

	private UpsertStrategy upsertStrategy;
	private List<UpsertStrategy> upsertStrategies;


	/**
	 * Insert the values of columns with non-derived-import-filters into the local database.
	 */
	@Override
	public void fillAndWriteMappingTables(JobManager jobManager, final OutputStreamWriter receiptWriter,
			int numberOfEntities, final Session targetSession, final DBMS targetDBMSConfiguration, DBMS dbmsConfiguration) throws IOException, SQLException {
		if (importFilterManager != null) {
			File tmp = Configuration.getInstance().createTempFile();
			OutputStreamWriter tmpFileWriter;
			tmpFileWriter = new FileWriter(tmp);
			importFilterManager.createMappingTables(dbmsConfiguration, tmpFileWriter);
			tmpFileWriter.close();
			new SqlScriptExecutor(getSession(), executionContext.getNumberOfThreads()).executeScript(tmp.getPath());
			tmp.delete();

			tmp = Configuration.getInstance().createTempFile();
			tmpFileWriter = new FileWriter(tmp);
			tmpFileWriter.write("-- sync\n");
			importFilterManager.fillAndWriteMappingTables(this, jobManager, tmpFileWriter, numberOfEntities, targetSession, targetDBMSConfiguration);
			tmpFileWriter.close();
			new SqlScriptExecutor(getSession(), executionContext.getNumberOfThreads()).executeScript(tmp.getPath());
			tmp.delete();
		}
	}

	private String applyImportFilter(String oldValue, Column column) {
		Filter filter = column.getFilter();
		if (filter != null && importFilterManager != null) {
			if (!filter.isApplyAtExport()) {
				return importFilterManager.transform(column, oldValue);
			}
		}
		return oldValue;
	}

	/**
	 * Creates the DROP-statements for the mapping tables.
	 */
	@Override
	public void dropMappingTables(OutputStreamWriter result, DBMS targetDBMSConfiguration) throws IOException, SQLException {
		if (importFilterManager != null) {
			File tmp = Configuration.getInstance().createTempFile();
			OutputStreamWriter tmpFileWriter;
			tmpFileWriter = new FileWriter(tmp);
			importFilterManager.dropMappingTables(tmpFileWriter);
			tmpFileWriter.close();
			new SqlScriptExecutor(getSession(), executionContext.getNumberOfThreads()).executeScript(tmp.getPath());
			tmp.delete();
		}
	}

}
