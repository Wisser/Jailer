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
package net.sf.jailer.entitygraph;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicLong;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.configuration.LimitTransactionSizeInfo;
import net.sf.jailer.database.SQLDialect;
import net.sf.jailer.database.Session;
import net.sf.jailer.database.Session.ResultSetReader;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.PrimaryKey;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.extractionmodel.SubjectLimitDefinition;
import net.sf.jailer.importfilter.ImportFilterManager;
import net.sf.jailer.subsetting.TransformerFactory;
import net.sf.jailer.util.JobManager;

/**
 * Persistent graph of entities.
 *
 * @author Ralf Wisser
 */
public abstract class EntityGraph {

	/**
	 * Name of the graph-table.
	 */
	public static final String ENTITY_GRAPH = "JAILER_GRAPH";

	/**
	 * Name of the (helper) set-table.
	 */
	public static final String ENTITY_SET_ELEMENT = "JAILER_SET";

	/**
	 * Name of the entity-table.
	 */
	public static final String ENTITY = "JAILER_ENTITY";

	/**
	 * Name of the dependency-table.
	 */
	public static final String DEPENDENCY = "JAILER_DEPENDENCY";

	/**
	 * Sets birthday of subject rows.
	 *
	 * @param birthdayOfSubject birthday of subject rows
	 */
	public abstract void setBirthdayOfSubject(int birthdayOfSubject);

	public final DataModel dataModel;
	protected boolean isTruncated = false;

	/**
	 * The execution context.
	 */
	protected final ExecutionContext executionContext;

	protected boolean inDeleteMode = false;

	/**
	 * The unique ID of the graph.
	 */
	public final int graphID;

	protected EntityGraph(int graphID, DataModel dataModel, ExecutionContext executionContext) {
		this.executionContext = executionContext;
		this.graphID = graphID;
		this.dataModel = dataModel;
	}

	/**
	 * Copies an entity-graph.
	 *
	 * @param graphID the unique ID of the graph
	 * @param session for executing SQL-Statements
	 * 
	 * @return the newly created entity-graph
	 */
	public abstract EntityGraph copy(int graphID, Session session) throws SQLException;

	/**
	 * Creates a new entity-graph of same type and session.
	 */
	public abstract EntityGraph createNewGraph() throws SQLException;

	/**
	 * Copies some tables.
	 *
	 * @param tables tables to copy
	 */
	public EntityGraph copy(Set<Table> tables) throws SQLException {
		EntityGraph entityGraph = createNewGraph();
		for (Table table: tables) {
			getSession().executeUpdate(
				"Insert into " + dmlTableReference(ENTITY, getSession()) + "(r_entitygraph, " + getUniversalPrimaryKey().columnList(null) + ", birthday, orig_birthday, type) " +
					"Select " + entityGraph.graphID + ", " + getUniversalPrimaryKey().columnList(null) + ", birthday, birthday, type From " + dmlTableReference(ENTITY, getSession()) + " Where r_entitygraph=" + graphID + " and type=" + typeName(table) + " and birthday>=0");
		}
		return entityGraph;
	}

	/**
	 * Gets the age of the graph.
	 *
	 * @return the age of the graph
	 */
	public abstract int getAge() throws SQLException;

	/**
	 * Sets the age of the graph.
	 *
	 * @param age the age of the graph
	 */
	public abstract void setAge(int age) throws SQLException;

	/**
	 * Gets the number of entities in the graph.
	 *
	 * @return the number of entities in the graph
	 */
	public abstract long getSize() throws SQLException;

	/**
	 * Gets the number of entities from given tables in the graph.
	 *
	 * @return the number of entities in the graph
	 * @throws SQLException
	 */
	public long getSize(final Set<Table> tables) throws SQLException {
		final long[] total = new long[1];
		total[0] = 0;
		if (!tables.isEmpty()) {
			getSession().executeQuery("Select type, count(*) From " + dmlTableReference(ENTITY, getSession()) + " Where r_entitygraph=" + graphID + " and birthday>=0 group by type", new Session.AbstractResultSetReader() {
				@Override
				public void readCurrentRow(ResultSet resultSet) throws SQLException {
					Table table = dataModel.getTableByOrdinal(resultSet.getInt(1));
					if (tables.contains(table)) {
						long count = resultSet.getLong(2);
						total[0] += count;
					}
				}
			});
		}
		return total[0];
	}

	/**
	 * Deletes the graph.
	 */
	public final void delete() throws SQLException {
		delete(false);
	}

	/**
	 * Deletes the graph.
	 *
	 * @param forced force deletion. if <code>true</code>, don't allow optimization.
	 */
	public abstract void delete(boolean forced);

	/**
	 * Adds entities to the graph.
	 *
	 * @param table the table
	 * @param condition the condition in SQL that the entities must fulfill
	 * @param today the birthday of the new entities
	 *
	 * @return row-count
	 */
	public abstract long addEntities(Table table, String condition, int today) throws SQLException;

	/**
	 * Adds limited number of entities to the graph.
	 *
	 * @param table the table
	 * @param condition the condition in SQL that the entities must fulfill
	 * @param today the birthday of the new entities
	 * @param limitDefinition limit
	 *
	 * @return row-count
	 */
	public abstract long addEntities(Table table, String condition, int today, SubjectLimitDefinition limitDefinition, boolean joinWithEntity) throws SQLException;

	/**
	 * Resolves an association. Retrieves and adds all entities
	 * associated with an entity born yesterday in the graph
	 * and adds the dependencies.
	 *
	 * @param table the table
	 * @param association the association to resolve
	 * @param today the birthday of the new entities
	 *
	 * @return row-count or -1, if association is ignored
	 */
	public abstract long resolveAssociation(Table table, Association association, int today) throws SQLException;

	/**
	 * Resolves an association. Retrieves and adds all entities
	 * associated with an entity into a given entity-graph.
	 * Restrictions are ignored.
	 *
	 * @param table the table
	 * @param association the association to resolve
	 *
	 * @return row-count
	 */
	public abstract long resolveAssociation(final Table table, Association association, EntityGraph otherGraph, EntityGraph universum, boolean forDelete) throws SQLException;

	/**
	 * Adds dependencies.
	 *
	 * @param from source of dependency
	 * @param fromAlias alias for from-table
	 * @param to destination of dependency
	 * @param toAlias alias for to-table
	 * @param condition condition of dependency
	 * @param aggregationId id of aggregation association (for XML export), 0 if not applicable
	 * @param dependencyId id of dependency
	 */
	public abstract void addDependencies(Table from, String fromAlias, Table to, String toAlias, String condition, int aggregationId, int dependencyId, boolean isAssociationReversed) throws SQLException;

	/**
	 * Gets distinct association-ids of all edged.
	 */
	public abstract Set<Integer> getDistinctDependencyIDs() throws SQLException;

	/**
	 * Marks all entities of a given table which don't dependent on other entities,
	 * s.t. they can be read and deleted.
	 */
	public abstract void markIndependentEntities(Table table) throws SQLException;

	/**
	 * Marks all rows which are not target of a dependency.
	 */
	public abstract void markRoots(Table table) throws SQLException;

	/**
	 * Reads all entities of a given table which are marked as independent or as roots.
	 *
	 * @param table the table
	 * @param orderByPK if <code>true</code>, result will be ordered by primary keys
	 */
	public abstract void readMarkedEntities(Table table, boolean orderByPK) throws SQLException;

	/**
	 * Reads all entities of a given table which are marked as independent or as roots.
	 *
	 * @param reader for reading the result-set
	 * @param table the table
	 * @param orderByPK if <code>true</code>, result will be ordered by primary keys
	 */
	public abstract void readMarkedEntities(Table table, Session.ResultSetReader reader, String selectionSchema, String originalPKAliasPrefix, boolean orderByPK) throws SQLException;

	/**
	 * Reads all entities of a given table.
	 *
	 * @param table the table
	 * @param orderByPK if <code>true</code>, result will be ordered by primary keys
	 */
	public abstract void readEntities(Table table, boolean orderByPK) throws SQLException;

	/**
	 * Updates columns of a table.
	 *
	 * @param table the table
	 * @param columns the columns;
	 * @param inSourceSchema if <code>true</code>, use source-schema-mapping, else use schema-mapping
	 * @param reason to be written as comment
	 */
	public abstract void updateEntities(Table table, Set<Column> columns, OutputStreamWriter scriptFileWriter, DBMS targetConfiguration, boolean inSourceSchema, String reason) throws SQLException;

	/**
	 * Reads some columns of all entities of a given table without using filters.
	 *
	 * @param table the table
	 * @param columns the columns
	 * @param reader to read
	 */
	public abstract long readUnfilteredEntityColumns(final Table table, final List<Column> columns, final Session.ResultSetReader reader) throws SQLException;

	/**
	 * Deletes all entities which are marked as independent.
	 */
	public abstract void deleteIndependentEntities(Table table) throws SQLException;

	/**
	 * Deletes all entities from a given table.
	 */
	public abstract long deleteEntities(Table table) throws SQLException;

	/**
	 * Counts the entities of a given table in this graph.
	 *
	 * @param table the table
	 * @return the number of entities from table in this graph
	 */
	public abstract long countEntities(Table table) throws SQLException;

	/**
	 * Removes all entities from this graph which are associated with an entity
	 * outside the graph.
	 *
	 * @param association the association
	 * @param deletedEntitiesAreMarked if true, consider entity as deleted if its birthday is negative
	 * @param allTables set of tables from which there are entities in E
	 * @return number of removed entities
	 */
	public abstract long removeAssociatedDestinations(Association association, boolean deletedEntitiesAreMarked, Set<Table> allTables) throws SQLException;

	/**
	 * Reads all entities which depends on given entity.
	 *
	 * @param table the table from which to read entities
	 * @param association the dependency
	 * @param resultSet current row is given entity
	 * @param reader reads the entities
	 * @param selectionSchema the selection schema
	 */
	public abstract void readDependentEntities(Table table, Association association, ResultSet resultSet, ResultSetMetaData resultSetMetaData, ResultSetReader reader, Map<String, Integer> typeCache, String selectionSchema, String originalPKAliasPrefix) throws SQLException;

	/**
	 * Marks all entities which depends on given entity as traversed.
	 *
	 * @param association the dependency
	 * @param resultSet current row is given entity
	 */
	public abstract void markDependentEntitiesAsTraversed(Association association, ResultSet resultSet, ResultSetMetaData resultSetMetaData, Map<String, Integer> typeCache) throws SQLException;

	/**
	 * Reads all non-traversed dependencies.
	 *
	 * @param table the source of dependencies to look for
	 * @param reader reads the entities
	 */
	public abstract void readNonTraversedDependencies(Table table, ResultSetReader reader) throws SQLException;

	/**
	 * Removes all reflexive dependencies of given table.
	 *
	 * @param table the table
	 */
	public abstract void removeReflexiveDependencies(Table table) throws SQLException;

	/**
	 * Gets total row-count.
	 *
	 * @return total row-count
	 */
	public abstract long getTotalRowcount();

	/**
	 * Gets the universal primary key.
	 *
	 * @return the universal primary key
	 */
	public abstract PrimaryKey getUniversalPrimaryKey();

	/**
	 * Shuts down statement-executor.
	 */
	public abstract void shutDown() throws SQLException;

	/**
	 * Gets the session.
	 *
	 * @return the session
	 */
	public abstract Session getSession();

	/**
	 * Creates a unique ID for a new graph.
	 *
	 * @return a unique ID
	 */
	public static int createUniqueGraphID() {
		return Math.abs(Math.abs((int) nextGraphId.getAndIncrement())) % (Integer.MAX_VALUE / 4);
	}

	private static AtomicLong nextGraphId = new AtomicLong(System.currentTimeMillis() % 30000);

	private int lobCount = 0;

	/**
	 * Increments lob-counter and returns new value.
	 */
	public synchronized int incLobCount() {
		return ++lobCount;
	}

	public abstract DataModel getDatamodel();

	/**
	 * Closes the graph. Deletes the local database.
	 */
	abstract public void close() throws SQLException;

	/**
	 * Removes all dependencies for a given association.
	 *
	 * @param association the asociation
	 */
	public void removeDependencies(Association association) throws SQLException {
		deleteRows(getSession(), dmlTableReference(DEPENDENCY, getSession()), "depend_id=" + association.getId() + " and r_entitygraph=" + graphID);
	}

	public abstract Session getTargetSession();

	public void setDeleteMode(boolean deleteMode) {
		inDeleteMode = deleteMode;
	}

	protected int typeName(Table table) {
		return table.getOrdinal();
	}

	/**
	 * The {@link TransformerFactory}.
	 */
	private TransformerFactory transformerFactory;

	/**
	 * The {@link ImportFilterManager}.
	 */
	protected ImportFilterManager importFilterManager;

	/**
	 * Sets the {@link TransformerFactory}.
	 *
	 * @param transformerFactory the factory
	 */
	public void setTransformerFactory(TransformerFactory transformerFactory) {
		this.transformerFactory = transformerFactory;
	}

	/**
	 * Gets the {@link TransformerFactory}.
	 *
	 * @return the factory
	 */
	public TransformerFactory getTransformerFactory() {
		return transformerFactory;
	}

	/**
	 * Sets the {@link ImportFilterManager}
	 *
	 * @param importFilterManager the {@link ImportFilterManager}
	 */
	public void setImportFilterManager(ImportFilterManager importFilterManager) {
		this.importFilterManager = importFilterManager;
	}

	/**
	 * Gets the {@link ImportFilterManager}
	 *
	 * @return the {@link ImportFilterManager}
	 */
	public ImportFilterManager getImportFilterManager() {
		return importFilterManager;
	}

	/**
	 * Insert the values of columns with non-derived-import-filters into the local database.
	 */
	public void fillAndWriteMappingTables(JobManager jobManager, final OutputStreamWriter dmlResultWriter,
			int numberOfEntities, final Session targetSession, final DBMS targetDBMSConfiguration, DBMS dbmsConfiguration) throws SQLException, IOException {
		if (importFilterManager != null) {
			importFilterManager.createMappingTables(dbmsConfiguration, dmlResultWriter);
			importFilterManager.fillAndWriteMappingTables(this, jobManager, dmlResultWriter, numberOfEntities, targetSession, targetDBMSConfiguration);
		}
	}

	/**
	 * Creates the DROP-statements for the mapping tables.
	 */
	public void dropMappingTables(OutputStreamWriter result, DBMS targetDBMSConfiguration) throws IOException, SQLException {
		if (importFilterManager != null) {
			importFilterManager.dropMappingTables(result);
		}
	}

	/**
	 * Gets table reference for DML statements for a given working table.
	 *
	 * @param tableName the working table
	 * @param session holds connection to DBMS
	 * @return table reference for the working table
	 * @throws SQLException
	 */
	protected String dmlTableReference(String tableName, Session session) throws SQLException {
		return SQLDialect.dmlTableReference(tableName, session, executionContext);
	}

	/**
	 * Deletes rows from table.
	 * Applies DBMS-specific deletion strategies, if available.
	 *
	 * @param session the session
	 * @param table the table
	 * @param where the "where" condition
	 *
	 * @return row count
	 */
	protected long deleteRows(Session session, String table, String where) throws SQLException {
		LimitTransactionSizeInfo limitTransactionSize = session.dbms.getLimitTransactionSize();
		long rc = 0;

		if (limitTransactionSize.isApplicable(executionContext)) {
			try {
				long c;
				do {
					c = session.executeUpdate("Delete " + limitTransactionSize.afterSelectFragment(executionContext)
							+ "from " + table + " where (" + where + ") "
							+ limitTransactionSize.additionalWhereConditionFragment(executionContext)
							+ limitTransactionSize.statementSuffixFragment(executionContext));
					rc += c;
				} while (c > 0 && c == limitTransactionSize.getLimit());
				return rc;
			} catch (Exception e) {
				// fall back
			}
		}

		return rc + session.executeUpdate("Delete from " + table + " where " + where);
	}

	/**
	 * Tries to delete this graph using "truncate".
	 *
	 * @param checkExist if <code>true</code>, checks existence of each graph
	 */
	public void truncate(ExecutionContext executionContext, boolean checkExist) throws SQLException {
		if (isTruncated) {
			return;
		}
		if (executionContext.isEmbedded()) {
			return;
		}
		if (checkExist) {
			checkExist(executionContext);
		}
		synchronized (EntityGraph.class) {
			final int count[] = new int[] { 0 };
			getSession().executeQuery("Select count(*) from " + SQLDialect.dmlTableReference(ENTITY_GRAPH, getSession(), executionContext), new Session.AbstractResultSetReader() {
				@Override
				public void readCurrentRow(ResultSet resultSet) throws SQLException {
					count[0] = resultSet.getInt(1);
				}
			});
			if (count[0] == 1) {
				boolean wasSilent = getSession().getSilent();
				try {
					getSession().setSilent(true);
					getSession().execute("Truncate Table " + SQLDialect.dmlTableReference(DEPENDENCY, getSession(), executionContext));
					getSession().execute("Truncate Table " + SQLDialect.dmlTableReference(ENTITY, getSession(), executionContext));
					getSession().execute("Truncate Table " + SQLDialect.dmlTableReference(ENTITY_GRAPH, getSession(), executionContext));
				} catch (SQLException e) {
					// "truncate" not supported
					return;
				} finally {
					getSession().setSilent(wasSilent);
				}
				isTruncated = true;
			}
		}
	}

	/**
	 * Check if the graph still exists.
	 */
	public void checkExist(ExecutionContext executionContext) throws SQLException {
		synchronized (EntityGraph.class) {
			final boolean found[] = new boolean[] { false };
			getSession().executeQuery("Select * from " + SQLDialect.dmlTableReference(ENTITY_GRAPH, getSession(), executionContext) + " Where id=" + graphID, new Session.AbstractResultSetReader() {
				@Override
				public void readCurrentRow(ResultSet resultSet) throws SQLException {
					found[0] = true;
				}
			});
			if (!found[0]) {
				throw new RuntimeException("EntityGraph has been deleted.");
			}
		}
	}

	private static Object EXPORT_COUNT_LOCK = new Object();
	
	protected void addExportedCount(long count) {
		synchronized (EXPORT_COUNT_LOCK) {
			Long cnt = (Long) getSession().getSessionProperty(EntityGraph.class, "ExportedCount");
			if (cnt == null) {
				cnt = count;
			} else {
				cnt += count;
			}
			getSession().setSessionProperty(EntityGraph.class, "ExportedCount", cnt);
		}
	}

	public long getExportedCount() {
		synchronized (EXPORT_COUNT_LOCK) {
			Long cnt = (Long) getSession().getSessionProperty(EntityGraph.class, "ExportedCount");
			return cnt == null? 0 : cnt;
		}
	}
	
	/**
	 * Gets all non-virtual columns of the table in the order in which they are selected.
	 * 
	 * @return all non-virtual columns of the table in the order in which they are selected
	 * 
	 * @throws Exception if selection clause is empty
	 */
	protected List<Column> getSelectionClause(Table table) {
		List<Column> selectionClause = table.getSelectionClause();
		if (selectionClause.isEmpty()) {
			throw new RuntimeException("All columns of the table \"" + table.getName() + "\" are virtual or have an exclude filter.");
		}
		return selectionClause;
	}

}
