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
package net.sf.jailer.entitygraph;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.sf.jailer.database.SQLDialect;
import net.sf.jailer.database.Session;
import net.sf.jailer.database.Session.ResultSetReader;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.PrimaryKey;
import net.sf.jailer.datamodel.Table;

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
    
    /**
     * The unique ID of the graph.
     */
    public final int graphID;

    protected EntityGraph(int graphID, DataModel dataModel) {
    	this.graphID = graphID;
    	this.dataModel = dataModel;
    }
    
    /**
     * Unique IDs for each association to be used for explanation.
     */
    public Map<Association, Integer> explainIdOfAssociation = new HashMap<Association, Integer>();
    
    /**
     * Copies an entity-graph.
     * 
     * @param graph the graph to copy
     * @param graphID the unique ID of the graph
     * @param session for executing SQL-Statements
     * @return the newly created entity-graph
     * @throws Exception 
     */
    public abstract EntityGraph copy(int graphID, Session session) throws SQLException, Exception;
    
    /**
     * Finds an entity-graph.
     * 
     * @param graphID the unique ID of the graph
     * @param universalPrimaryKey the universal primary key
     * @param session for executing SQL-Statements
     * @return the entity-graph
     * @throws Exception 
     */
    public abstract EntityGraph find(int graphID, Session session, PrimaryKey universalPrimaryKey) throws SQLException, Exception;
    
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
     * Deletes the graph.
     */
    public abstract void delete() throws SQLException;

    /**
     * Adds entities to the graph.
     * 
     * @param table the table 
     * @param condition the condition in SQL that the entities must fulfill
     * @param today the birthday of the new entities
     * @param limit a limit of the number of rows to be exported or <code>0</code>
     * 
     * @return row-count
     */
    public abstract long addEntities(Table table, String condition, int today, long limit) throws SQLException;
    
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
     * @param reader for reading the result-set
     * @param table the table
     * @param orderByPK if <code>true</code>, result will be ordered by primary keys
     */
    public abstract void readMarkedEntities(Table table, Session.ResultSetReader reader, boolean orderByPK) throws SQLException;
    
    /**
     * Reads all entities of a given table which are marked as independent or as roots.
     * 
     * @param reader for reading the result-set
     * @param table the table
     * @param orderByPK if <code>true</code>, result will be ordered by primary keys
     */
    public abstract void readMarkedEntities(Table table, Session.ResultSetReader reader, String selectionSchema, boolean orderByPK) throws SQLException;
    
    /**
     * Reads all entities of a given table which are marked as independent or as roots.
     * 
     * @param reader for reading the result-set
     * @param table the table
     * @param orderByPK if <code>true</code>, result will be ordered by primary keys
     */
    public abstract void readMarkedEntities(Table table, Session.ResultSetReader reader, String selectionSchema, String originalPKAliasPrefix, boolean orderByPK) throws SQLException;
    
    /**
     * Unites the graph with another one and deletes the other graph.
     * 
     * @param graph the graph to be united with this graph
     */
    public abstract void uniteWith(EntityGraph graph) throws SQLException;
    
    /**
     * Reads all entities of a given table.
     * 
     * @param reader for reading the result-set
     * @param table the table
     * @param orderByPK if <code>true</code>, result will be ordered by primary keys
     */
    public abstract void readEntities(Table table, Session.ResultSetReader reader, boolean orderByPK) throws SQLException;
    
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
     * @param deletedEntitiesAreMarked if true, consider entity as deleted if its birthday is negative
     * @param association the association
     * @return number of removed entities
     */
    public abstract long removeAssociatedDestinations(Association association, boolean deletedEntitiesAreMarked) throws SQLException;
    
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
     * @param table the table from which to read entities
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
     * Gets some statistical information.
     */
    public abstract List<String> getStatistics(final DataModel dataModel, Set<Table> tables) throws SQLException;

    /**
     * Gets total row-count.
     * 
     * @return total row-count
     */
    public abstract long getTotalRowcount();
    
    /**
     * Whether or not to store additional information in order to create a 'explain.log'.
     * 
     * @param explain <code>true</code> iff predecessors of each entity must be stored
     */
    public abstract void setExplain(boolean explain);

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
        return Math.abs((int) System.currentTimeMillis());
    }
    
    private int lobCount = 0;
    
    /**
     * Increments lob-counter and returns new value.
     */
	public synchronized int incLobCount() {
		return ++lobCount;
	}

	public abstract DataModel getDatamodel();
	
	public static long maxTotalRowcount;
    
	/**
	 * Closes the graph. Deletes the local database.
	 */
	abstract public void close() throws SQLException;
	
    /**
     * Gets some statistical information.
     */
    protected final List<String> getStatistics(Session session, final DataModel dataModel, Set<Table> tables) throws SQLException {
        final List<String> statistic = new ArrayList<String>();
        final long[] total = new long[1];
        total[0] = 0;
        final Set<Table> remaining = new HashSet<Table>(tables);
        session.executeQuery("Select type, count(*) From " + SQLDialect.dmlTableReference(ENTITY, session) + " Where r_entitygraph=" + graphID + " and birthday>=0 group by type", new Session.AbstractResultSetReader() {
            public void readCurrentRow(ResultSet resultSet) throws SQLException {
                String type = resultSet.getString(1);
                Table table = dataModel.getTable(type);
                if (table != null) {
                	type = dataModel.getDisplayName(table);
                	remaining.remove(table);
                }
                long count = resultSet.getLong(2);
                total[0] += count;
                while (type.length() < 30) {
                    type = type + " ";
                }
                statistic.add(type + " " + count);
            }
        });
        for (Table table: remaining) {
        	String type = dataModel.getDisplayName(table);
	        while (type.length() < 30) {
	            type = type + " ";
	        }
	        statistic.add(type + " 0");
        }
        Collections.sort(statistic);
        statistic.add(0, "" + total[0]);
        return statistic;
    }

	public abstract Session getTargetSession();
    
}
