/*
 * Copyright 2007 the original author or authors.
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
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.sf.jailer.database.DBMS;
import net.sf.jailer.database.StatementExecutor;
import net.sf.jailer.database.StatementExecutor.ResultSetReader;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.PrimaryKey;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.SqlScriptExecutor;
import net.sf.jailer.util.SqlUtil;

import org.apache.log4j.Logger;

/**
 * Persistent graph of entities. 
 * 
 * @author Ralf Wisser
 */
public class EntityGraph {

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
     * The unique ID of the graph.
     */
    public final int graphID;

    /**
     * For executing SQL-Statements.
     */
    public final StatementExecutor statementExecutor;

	/**
     * The logger.
     */
    private static final Logger _log = Logger.getLogger(SqlScriptExecutor.class);

    /**
     * The universal primary key.
     */
    private final PrimaryKey universalPrimaryKey;
    
    /**
     * Constructor.
     * 
     * @param graphID the unique ID of the graph
     * @param statementExecutor for executing SQL-Statements
     * @param universalPrimaryKey the universal primary key
     */
    private EntityGraph(int graphID, StatementExecutor statementExecutor, PrimaryKey universalPrimaryKey) {
        this.graphID = graphID;
        this.statementExecutor = statementExecutor;
        this.universalPrimaryKey = universalPrimaryKey;
    }
    
    /**
     * Creates a new entity-graph.
     * 
     * @param graphID the unique ID of the graph
     * @param statementExecutor for executing SQL-Statements
     * @param universalPrimaryKey the universal primary key
     * @return the newly created entity-graph
     */
    public static EntityGraph create(int graphID, StatementExecutor statementExecutor, PrimaryKey universalPrimaryKey) {
        EntityGraph entityGraph = new EntityGraph(graphID, statementExecutor, universalPrimaryKey);
        try {
            statementExecutor.executeUpdate("Insert into " + ENTITY_GRAPH + "(id, age) values (" + graphID + ", 1)");
        } catch (SQLException e) {
            throw new RuntimeException("Can't find working tables! " +
                    "Run 'bin/jailer.sh create-ddl' " +
                    "and execute the DDL-script first!", e);
        }
        return entityGraph;
    }

    /**
     * Copies an entity-graph.
     * 
     * @param graph the graph to copy
     * @param graphID the unique ID of the graph
     * @param statementExecutor for executing SQL-Statements
     * @return the newly created entity-graph
     */
    public static EntityGraph copy(EntityGraph graph, int graphID, StatementExecutor statementExecutor) throws SQLException {
        EntityGraph entityGraph = create(graphID, statementExecutor, graph.universalPrimaryKey);
        statementExecutor.executeUpdate(
                "Insert into " + ENTITY + "(r_entitygraph, " + graph.universalPrimaryKey.columnList(null) + ", birthday, type) " +
                    "Select " + graphID + ", " + graph.universalPrimaryKey.columnList(null) + ", birthday, type From " + ENTITY + " Where r_entitygraph=" + graph.graphID + "");
        return entityGraph;
    }

    /**
     * Finds an entity-graph.
     * 
     * @param graphID the unique ID of the graph
     * @param universalPrimaryKey the universal primary key
     * @param statementExecutor for executing SQL-Statements
     * @return the entity-graph
     */
    public static EntityGraph find(int graphID, StatementExecutor statementExecutor, PrimaryKey universalPrimaryKey) throws SQLException {
        EntityGraph entityGraph = new EntityGraph(graphID, statementExecutor, universalPrimaryKey);
        final boolean[] found = new boolean[1];
        found[0] = false;
        statementExecutor.executeQuery("Select * From " + ENTITY_GRAPH  + "Where id=" + graphID + "", new StatementExecutor.ResultSetReader() {
            public void readCurrentRow(ResultSet resultSet) throws SQLException {
                found[0] = true;
            }
            public void close() {
            }
        });
        if (!found[0]) {
            throw new RuntimeException("entity-graph " + graphID + " not found");
        }
        return entityGraph;
    }

    /**
     * Creates a unique ID for a new graph.
     * 
     * @return a unique ID
     */
    public static int createUniqueGraphID() {
        return Math.abs((int) System.currentTimeMillis());
    }
    
    /**
     * Gets the age of the graph.
     * 
     * @return the age of the graph
     */
    public int getAge() throws SQLException {
        final int[] age = new int[1];
        age[0] = -1;
        statementExecutor.executeQuery("Select age From " + ENTITY_GRAPH + " Where id=" + graphID + "", new StatementExecutor.ResultSetReader() {
            public void readCurrentRow(ResultSet resultSet) throws SQLException {
                age[0] = resultSet.getInt(1);
            }
            public void close() {
            }
        });
        return age[0];
    }

    /**
     * Sets the age of the graph.
     * 
     * @param age the age of the graph
     */
    public void setAge(int age) throws SQLException {
        statementExecutor.executeUpdate("Update " + ENTITY_GRAPH + " Set age=" + age + " Where id=" + graphID + "");
    }
    
    /**
     * Gets the number of entities in the graph.
     * 
     * @return the number of entities in the graph
     */
    public long getSize() throws SQLException {
        final int[] size = new int[1];
        size[0] = -1;
        statementExecutor.executeQuery("Select count(*) From " + ENTITY + " Where r_entitygraph=" + graphID + " and birthday >= 0", new StatementExecutor.ResultSetReader() {
            public void readCurrentRow(ResultSet resultSet) throws SQLException {
                size[0] = resultSet.getInt(1);
            }
            public void close() {
            }
        });
        return size[0];
    }
    
    /**
     * Deletes the graph.
     */
    public void delete() throws SQLException {
        statementExecutor.executeUpdate("Delete from " + DEPENDENCY + " Where r_entitygraph=" + graphID + "");
        statementExecutor.executeUpdate("Delete from " + ENTITY + " Where r_entitygraph=" + graphID + "");
        statementExecutor.executeUpdate("Delete from " + ENTITY_GRAPH + " Where id=" + graphID + "");
    }

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
    public long addEntities(Table table, String condition, int today, long limit) throws SQLException {
        return addEntities(table, "T", condition, null, null, null, null, false, today, limit, 0);
    }
    
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
    public long resolveAssociation(Table table, Association association, int today) throws SQLException {
        String jc = association.getJoinCondition();
        if (jc != null) {
            String destAlias, sourceAlias;
            if (association.reversed) {
                destAlias = "A";
                sourceAlias = "B";
            } else {
                destAlias = "B";
                sourceAlias = "A";
            }
            Integer associationExplanationID = 0;
            if (explain) {
                synchronized (explainIdOfAssociation) {
                    associationExplanationID = explainIdOfAssociation.get(association);
                    if (associationExplanationID == null) {
                        associationExplanationID = (nextExplainID++);
                        explainIdOfAssociation.put(association, associationExplanationID);
                    }
                }
            }
            return addEntities(association.destination, destAlias, "E.r_entitygraph=" + graphID + " and E.birthday = " + (today - 1) + " and E.type='" + table.getName() + "' and " + pkEqualsEntityID(table, sourceAlias, "E"), table, sourceAlias, association.source, jc, true, today, 0, associationExplanationID);
        }
        return -1;
    }

    /**
     * Adds entities to the graph.
     * 
     * @param table the table 
     * @param condition the condition in SQL that the entities must fulfill with 'E' as alias for the entity-table
     * @param joinedTable optional table to join with
     * @param source optional, the source-table
     * @param joinCondition optional condition to join with <code>joinedTable</code>
     * @param joinWithEntity whether to join with entity-table too
     * @param today the birthday of the new entities
     * @param limit a limit of the number of rows to be exported or <code>0</code>
     * 
     * @return row-count
     */
    private long addEntities(Table table, String alias, String condition, Table joinedTable, String joinedTableAlias, Table source, String joinCondition, boolean joinWithEntity, int today, long limit, int associationExplanationID) throws SQLException {
        if (maxTotalRowcount > 0 && limit == 0) {
            limit = Math.max(maxTotalRowcount - totalRowcount + 1, 1);
        }
        String select =
            "Select " + (joinedTable != null? "distinct " : "") + "" + graphID + " as GRAPH_ID, " + pkList(table, alias) + ", " + today + " AS TODAY, '" + table.getName() + "' AS TYPE" +
            (source == null || !explain? "" : ", " + associationExplanationID + " AS ASSOCIATION, '" + source.getName() + "' AS SOURCE_TYPE, " + pkList(source, joinedTableAlias, "PRE_")) +
            " From " + table.getName() + " " + alias +
            (joinedTable != null? " join " + joinedTable.getName() + " " + joinedTableAlias + " on " + joinCondition : "") +
            " left join " + ENTITY + " Duplicate on Duplicate.r_entitygraph=" + graphID + " and Duplicate.type='" + table.getName() + "' and " +
            pkEqualsEntityID(table, alias, "Duplicate") + 
            (joinWithEntity? ", " + ENTITY + " E" : "") +
            " Where (" + condition + ") and Duplicate." + universalPrimaryKey.getColumns().get(0).name + " is null" +
            (limit > 0? " fetch first " + limit + " rows only" : ""); 
        
        if (source != null && explain) {
            String max = "";
            for (Column column: universalPrimaryKey.getColumns()) {
                if (max.length() > 0) {
                    max += ", ";
                }
                max += "max(PRE_" + column.name + ")";
            }
            select = "Select GRAPH_ID, " + universalPrimaryKey.columnList(null) + ", TODAY, TYPE, ASSOCIATION, max(SOURCE_TYPE), " + max + " From (" + select + ") Q " +
                     "Group by GRAPH_ID, " + universalPrimaryKey.columnList(null) + ", TODAY, TYPE, ASSOCIATION";
        }
        
        String insert = "Insert into " + ENTITY + " (r_entitygraph, " + universalPrimaryKey.columnList(null) + ", birthday, type" + (source == null || !explain? "" : ", association, PRE_TYPE, " + universalPrimaryKey.columnList("PRE_"))  + ") " + select;
        long rc = statementExecutor.executeUpdate(insert);
        totalRowcount += rc;
        return rc;
    }

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
    public void addDependencies(Table from, String fromAlias, Table to, String toAlias, String condition, int aggregationId, int dependencyId) throws SQLException {
        String insert = "Insert into " + DEPENDENCY + "(r_entitygraph, assoc, depend_id, from_type, to_type, " + universalPrimaryKey.columnList("FROM_") + ", " + universalPrimaryKey.columnList("TO_") + ") " +
            "Select " + graphID + ", " + aggregationId  + ", " + dependencyId + ", '" + from.getName() + "', '" + to.getName() + "', " + pkList(from, fromAlias, "FROM") + ", " + pkList(to, toAlias, "TO") +
            " From " + ENTITY + " E1, " + ENTITY + " E2, " + from.getName() + " " + fromAlias + " join " + to.getName() + " " + toAlias + " on " + condition +
            " Where E1.r_entitygraph=" + graphID + " and E2.r_entitygraph=" + graphID + "" +
            " and E1.type='" + from.getName() + "' and E2.type='" + to.getName() + "'" +
            " and " + pkEqualsEntityID(from, fromAlias, "E1") +
            " and " + pkEqualsEntityID(to, toAlias, "E2");
        totalRowcount += statementExecutor.executeUpdate(insert);
    }
    
    /**
     * Gets distinct association-ids of all edged.
     */
    public Set<Integer> getDistinctDependencyIDs() throws SQLException {
        String select = "Select distinct depend_id from " + DEPENDENCY + " Where r_entitygraph=" + graphID;
        final Set<Integer> ids = new HashSet<Integer>();
        statementExecutor.executeQuery(select, new StatementExecutor.ResultSetReader() {
			public void readCurrentRow(ResultSet resultSet) throws SQLException {
				ids.add(resultSet.getInt(1));
			}
			public void close() {
			}
        });
        return ids;
    }

    /**
     * Marks all entities which don't dependent on other entities,
     * s.t. they can be read and deleted.
     */
    public void markIndependentEntities() throws SQLException {
    	markIndependentEntities(null);
    }
    
    /**
     * Marks all entities of a given table which don't dependent on other entities,
     * s.t. they can be read and deleted.
     */
    public void markIndependentEntities(Table table) throws SQLException {
        StringBuffer fromEqualsPK = new StringBuffer();
        for (Column column: universalPrimaryKey.getColumns()) {
            if (fromEqualsPK.length() > 0) {
                fromEqualsPK.append(" and ");
            }
            fromEqualsPK.append("D.FROM_" + column.name + "=" + ENTITY + "." + column.name);
        }
        statementExecutor.executeUpdate(
                "Update " + ENTITY + " set birthday=0 " +
                "Where r_entitygraph=" + graphID + " and birthday>0 and " +
                	   (table != null? "type='" + table.getName() + "' and " : "") +
                       "not exists (Select * from " + DEPENDENCY + " D " +
                           "Where D.r_entitygraph=" + graphID + " and D.assoc=0 and D.from_type=" + ENTITY + ".type and " +
                                 fromEqualsPK + ")");
    }

    /**
     * Marks all rows which are not target of a dependency.
     */
    public void markRoots() throws SQLException {
        StringBuffer toEqualsPK = new StringBuffer();
        for (Column column: universalPrimaryKey.getColumns()) {
            if (toEqualsPK.length() > 0) {
                toEqualsPK.append(" and ");
            }
            toEqualsPK.append("D.TO_" + column.name + "=" + ENTITY + "." + column.name);
        }
        statementExecutor.executeUpdate(
                "Update " + ENTITY + " set birthday=0 " +
                "Where r_entitygraph=" + graphID + " and birthday>0 and " +
                       "not exists (Select * from " + DEPENDENCY + " D " +
                           "Where D.r_entitygraph=" +graphID + " and D.to_type=" + ENTITY + ".type and " +
                                 toEqualsPK + ")");
    }

    /**
     * Reads all entities of a given table which are marked as independent or as roots.
     * 
     * @param reader for reading the result-set
     * @param table the table
     */
    public void readMarkedEntities(Table table, StatementExecutor.ResultSetReader reader) throws SQLException {
    	readMarkedEntities(table, reader, "T.*");
    }
    
    /**
     * Reads all entities of a given table which are marked as independent or as roots.
     * 
     * @param reader for reading the result-set
     * @param table the table
     */
    public void readMarkedEntities(Table table, StatementExecutor.ResultSetReader reader, String selectionSchema) throws SQLException {
        statementExecutor.executeQuery(
                "Select " + selectionSchema + " From " + ENTITY + " E join " + table.getName() + " T on " +
                pkEqualsEntityID(table, "T", "E") +
                " Where E.birthday=0 and E.r_entitygraph=" + graphID + " and E.type='" + table.getName() + "'",
                reader);
    }
    
    /**
     * Unites the graph with another one and deletes the other graph.
     * 
     * @param graph the graph to be united with this graph
     */
    public void uniteWith(EntityGraph graph) throws SQLException {
        StringBuffer e1EqualsE2 = new StringBuffer();
        for (Column column: universalPrimaryKey.getColumns()) {
            if (e1EqualsE2.length() > 0) {
                e1EqualsE2.append(" and ");
            }
            e1EqualsE2.append("E1." + column.name + "=E2." + column.name);
        }
        statementExecutor.executeUpdate("Update " + ENTITY + " E1 " +
                "set E1.r_entitygraph=" + graphID + " " +
                "Where E1.r_entitygraph=" + graph.graphID + " " +
                "and not exists(Select * from " + ENTITY + " E2 Where " +
                "E2.r_entitygraph=" + graphID + " and " +
                e1EqualsE2 +
                ")");
        graph.delete();
    }
    
    /**
     * Reads all entities of a given table.
     * 
     * @param reader for reading the result-set
     * @param table the table
     */
    public void readEntities(Table table, StatementExecutor.ResultSetReader reader) throws SQLException {
        statementExecutor.executeQuery(
                "Select " + table.getName() + ".* From " + ENTITY + " E join " + table.getName() + " on " +
                pkEqualsEntityID(table, table.getName(), "E") +
                " Where E.birthday>=0 and E.r_entitygraph=" + graphID + " and E.type='" + table.getName() + "'",
                reader);
    }
    
    /**
     * Deletes all entities which are marked as independent.
     */
    public void deleteIndependentEntities() throws SQLException {
        StringBuffer fromEqualsPK = new StringBuffer();
        StringBuffer toEqualsPK = new StringBuffer();
        for (Column column: universalPrimaryKey.getColumns()) {
            if (fromEqualsPK.length() > 0) {
                fromEqualsPK.append(" and ");
            }
            fromEqualsPK.append(DEPENDENCY + ".FROM_" + column.name + "=" + column.name);
            if (toEqualsPK.length() > 0) {
                toEqualsPK.append(" and ");
            }
            toEqualsPK.append(DEPENDENCY + ".TO_" + column.name + "=" + column.name);
        }
        statementExecutor.executeUpdate(
                "Delete From " + DEPENDENCY + " " +
                "Where " + DEPENDENCY + ".r_entitygraph=" + graphID + " and assoc=0 and " + 
                      "exists (Select * from " + ENTITY + " E Where " + 
                          "E.r_entitygraph=" + graphID + " and " +
                          fromEqualsPK + " and " + DEPENDENCY + ".from_type=E.type and " +
                          "E.birthday=0)");
        statementExecutor.executeUpdate(
                "Delete From " + DEPENDENCY + " " +
                "Where " + DEPENDENCY + ".r_entitygraph=" + graphID + " and assoc=0 and " + 
                      "exists (Select * from " + ENTITY + " E Where " + 
                          "E.r_entitygraph=" + graphID + " and " +
                          toEqualsPK + " and " + DEPENDENCY + ".to_type=E.type and " +
                          "E.birthday=0)");
        statementExecutor.executeUpdate(
                "Delete From " + ENTITY + " " +
                "Where r_entitygraph=" + graphID + " and " +
                       "birthday=0");
    }
    
    /**
     * Deletes all entities from a given table.
     */
    public long deleteEntities(Table table) throws SQLException {
        return statementExecutor.executeUpdate(
                "Delete From " + ENTITY + " " +
                "Where r_entitygraph=" + graphID + " and " +
                       "type='" + table.getName() +"'");
    }

    /**
     * Counts the entities of a given table in this graph.
     * 
     * @param table the table
     * @return the number of entities from table in this graph
     */
    public long countEntities(Table table) throws SQLException {
        final long[] count = new long[1];
        statementExecutor.executeQuery(
                "Select count(*) from " + ENTITY + " E " +
                "Where E.birthday>=0 and E.r_entitygraph=" + graphID + " and E.type='" + table.getName() + "'",
                new StatementExecutor.AbstractResultSetReader() {
                    public void readCurrentRow(ResultSet resultSet) throws SQLException {
                        count[0] = resultSet.getLong(1);
                    }
                });
        return count[0];
    }

    /**
     * Removes all entities from this graph which are associated with an entity
     * outside the graph.
     * 
     * @param deletedEntitiesAreMarked if true, consider entity as deleted if its birthday is negative
     * @param association the association
     * @return number of removed entities
     */
    public long removeAssociatedDestinations(Association association, boolean deletedEntitiesAreMarked) throws SQLException {
        String jc = association.getJoinCondition();
        if (jc != null) {
            String destAlias, sourceAlias;
            if (association.reversed) {
                destAlias = "A";
                sourceAlias = "B";
            } else {
                destAlias = "B";
                sourceAlias = "A";
            }
            int setId = getNextSetId();
            String remove = "Insert into " + ENTITY_SET_ELEMENT + "(set_id, type, " + universalPrimaryKey.columnList(null) + ") " +
                "Select distinct " + setId + ", EB.type, " + universalPrimaryKey.columnList("EB.") + " from " + ENTITY + " EB " +
                "join " + association.destination.getName() + " " + destAlias + " on "+ pkEqualsEntityID(association.destination, destAlias, "EB") + " " +
                "join " + association.source.getName() + " " + sourceAlias + " on " + association.getJoinCondition() + " " +
                (deletedEntitiesAreMarked? "join " : "left join ") + ENTITY + " EA on EA.r_entitygraph=" + graphID + " and EA.type='" + association.source.getName() + "' and " + pkEqualsEntityID(association.source, sourceAlias, "EA") + " " +
                "Where EB.r_entitygraph=" + graphID + " and EB.type='" + association.destination.getName() + "' " +
                "and " + (deletedEntitiesAreMarked? "EA.birthday=-1 and EB.birthday>=0" : "EA." + universalPrimaryKey.getColumns().get(0).name + " is null");
            long rc = statementExecutor.executeUpdate(remove);
            totalRowcount += rc;
            if (rc > 0) {
            	StringBuffer sEqualsE = new StringBuffer();
            	StringBuffer sEqualsEWoAlias = new StringBuffer();
                for (Column column: universalPrimaryKey.getColumns()) {
                	if (sEqualsE.length() > 0) {
                        sEqualsE.append(" and ");
                    }
                    sEqualsE.append("S." + column.name + "=E." + column.name);
                    if (sEqualsEWoAlias.length() > 0) {
                    	sEqualsEWoAlias.append(" and ");
                    }
                    sEqualsEWoAlias.append("S." + column.name + "=" + ENTITY + "." + column.name);
                }
                remove = "Update " + ENTITY + " E set E.birthday=-1 Where E.r_entitygraph=" + graphID + " and E.type='" + association.destination.getName() + "' " +
                          "and exists (Select * from " + ENTITY_SET_ELEMENT + " S where S.set_id=" + setId + " and E.type=S.type and " + sEqualsE + ")";
                try {
                	statementExecutor.setSilent(true);
                	statementExecutor.executeUpdate(remove);
                } catch (SQLException e) {
                	// postgreSQL
                	StatementExecutor._log.debug("failed, retry without alias (" + e.getMessage() + ")");
                	remove = "Update " + ENTITY + " set birthday=-1 Where " + ENTITY + ".r_entitygraph=" + graphID + " and " + ENTITY + ".type='" + association.destination.getName() + "' " +
                    "and exists (Select * from " + ENTITY_SET_ELEMENT + " S where S.set_id=" + setId + " and " + ENTITY + ".type=S.type and " + sEqualsEWoAlias + ")";
                	statementExecutor.executeUpdate(remove);
                } finally {
                	statementExecutor.setSilent(false);
                }
                statementExecutor.executeUpdate("Delete from " + ENTITY_SET_ELEMENT + " where set_id=" + setId + "");
            }
            return rc;
        }
        return 0;
    }
    
    /**
     * Reads all entities which depends on given entity. 
     * 
     * @param table the table from which to read entities
     * @param association the dependency
     * @param resultSet current row is given entity
     * @param reader reads the entities
     * @param selectionSchema the selection schema
     */
    public void readDependentEntities(Table table, Association association, ResultSet resultSet, ResultSetReader reader, Map<String, Integer> typeCache, String selectionSchema) throws SQLException {
    	String select = "Select " + selectionSchema + " from " + table.getName() + " T join " + DEPENDENCY + " D on " +
    		 pkEqualsEntityID(table, "T", "D", "TO_") + " and D.to_type='" + table.getName() + "'" +
    		 " Where " + pkEqualsEntityID(association.source, resultSet, "D", "FROM_", typeCache) +
    	     " and D.from_type='" + association.source.getName() + "' and assoc=" + association.getId() +
    	     " and D.r_entitygraph=" + graphID;
    	statementExecutor.executeQuery(select, reader);
    }
    
    /**
     * Marks all entities which depends on given entity as traversed. 
     * 
     * @param table the table from which to read entities
     * @param association the dependency
     * @param resultSet current row is given entity
     */
    public void markDependentEntitiesAsTraversed(Association association, ResultSet resultSet, Map<String, Integer> typeCache) throws SQLException {
    	String update;
    	if (statementExecutor.dbms == DBMS.SYBASE) {
    		update = "Update " + DEPENDENCY + " set traversed=1" +
    		 " Where " + pkEqualsEntityID(association.source, resultSet, DEPENDENCY, "FROM_", typeCache) +
    		 " and " + DEPENDENCY + ".from_type='" + association.source.getName() + "' and assoc=" + association.getId() +
    		 " and " + DEPENDENCY + ".r_entitygraph=" + graphID;
    	} else {
    		update = "Update " + DEPENDENCY + " D set traversed=1" +
    		 " Where " + pkEqualsEntityID(association.source, resultSet, "D", "FROM_", typeCache) +
    	     " and D.from_type='" + association.source.getName() + "' and assoc=" + association.getId() +
    	     " and D.r_entitygraph=" + graphID;
    	}
    	statementExecutor.executeUpdate(update);
    }
    
    /**
     * Reads all non-traversed dependencies. 
     * 
     * @param table the source of dependencies to look for
     * @param reader reads the entities
     */
    public void readNonTraversedDependencies(Table table, ResultSetReader reader) throws SQLException {
    	String select = "Select * from " + DEPENDENCY + " D " +
    		 " Where (traversed is null or traversed <> 1)" +
    	     " and D.from_type='" + table.getName() + "'" +
    	     " and D.r_entitygraph=" + graphID;
    	statementExecutor.executeQuery(select, reader);
    }
    
    /**
     * Gets a SQL comparition expression for comparing rows with given entity.
     * 
     * @param table the table
     * @param resultSet
     * @return a SQL comparition expression for comparing rows of <code>table</code> with current row of resultSet
     */
    private String pkEqualsEntityID(Table table, ResultSet resultSet, String alias, String columnPrefix, Map<String, Integer> typeCache) throws SQLException {
    	Map<Column, Column> match = universalPrimaryKey.match(table.primaryKey);
        StringBuffer sb = new StringBuffer();
        for (Column column: universalPrimaryKey.getColumns()) {
            if (sb.length() > 0) {
                sb.append(" and ");
            }
            sb.append(alias + "." + columnPrefix + column.name + "=");
            Column tableColumn = match.get(column);
            if (tableColumn != null) {
            	int i = 0;
            	for (Column c: table.primaryKey.getColumns()) {
            		if (c.name.equals(tableColumn.name)) {
            			break;
            		}
            		++i;
            	}
                sb.append(SqlUtil.toSql(SqlUtil.getObject(resultSet, "PK" + i /* tableColumn.name*/, typeCache)));
            } else {
                sb.append(column.getNullValue());
            }
        }
        return sb.toString();
    }

    /**
     * Gets a SQL comparition expression for comparing rows with entities.
     * 
     * @param table the table
     * @return a SQL comparition expression for comparing rows of <code>table</code> with entities
     */
    private String pkEqualsEntityID(Table table, String tableAlias, String entityAlias) {
    	return pkEqualsEntityID(table, tableAlias, entityAlias, "");
    }

    /**
     * Gets a SQL comparition expression for comparing rows with entities.
     * 
     * @param table the table
     * @return a SQL comparition expression for comparing rows of <code>table</code> with entities
     */
    private String pkEqualsEntityID(Table table, String tableAlias, String entityAlias, String columnPrefix) {
        Map<Column, Column> match = universalPrimaryKey.match(table.primaryKey);
        StringBuffer sb = new StringBuffer();
        for (Column column: universalPrimaryKey.getColumns()) {
            if (sb.length() > 0) {
                sb.append(" and ");
            }
            Column tableColumn = match.get(column);
            sb.append(typeConvert(entityAlias + "." + columnPrefix + column.name, tableColumn) + "=");
            if (tableColumn != null) {
                sb.append(tableAlias + "." + tableColumn.name);
            } else {
                sb.append(column.getNullValue());
            }
        }
        return sb.toString();
    }

    /**
     * Converts expression to type of given column.
     * 
     * @param expr the expression to convert
     * @param column the column
     * @return converted expression
     */
    private String typeConvert(String expr, Column column) {
    	if (SqlUtil.dbms != DBMS.SYBASE) return expr;
		return "convert(" + column.toSQL(null).substring(column.name.length()).trim() + ", " + expr + ")";
	}

	/**
     * Gets PK-column list for a table.
     * 
     * @param table the table
     * @param tableAlias the alias for table
     * @return PK-column list for table
     */
    private String pkList(Table table, String tableAlias) {
        return pkList(table, tableAlias, null);
    }
    
    /**
     * Gets PK-column list for a table.
     * 
     * @param table the table
     * @param tableAlias the alias for table
     * @param columnAliasPrefix optional prefix for column names
     */
    private String pkList(Table table, String tableAlias, String columnAliasPrefix) {
        Map<Column, Column> match = universalPrimaryKey.match(table.primaryKey);
        StringBuffer sb = new StringBuffer();
        for (Column column: universalPrimaryKey.getColumns()) {
            if (sb.length() > 0) {
                sb.append(", ");
            }
            Column tableColumn = match.get(column);
            if (tableColumn != null) {
                sb.append(tableAlias + "." + tableColumn.name);
            } else {
                sb.append(column.getNullValue());
            }
            sb.append(" AS " + (columnAliasPrefix == null? "" : columnAliasPrefix) + column.name);
        }
        return sb.toString();
    }

    /**
     * Gets some statistical information.
     */
    public List<String> getStatistics(final DataModel dataModel) throws SQLException {
        final List<String> statistic = new ArrayList<String>();
        final long[] total = new long[1];
        total[0] = 0;
        statementExecutor.executeQuery("Select type, count(*) From " + ENTITY + " Where r_entitygraph=" + graphID + " and birthday>=0 group by type", new StatementExecutor.AbstractResultSetReader() {
            public void readCurrentRow(ResultSet resultSet) throws SQLException {
                String type = resultSet.getString(1);
                Table table = dataModel.getTable(type);
                if (table != null) {
                	type = dataModel.getDisplayName(table);
                }
                long count = resultSet.getLong(2);
                total[0] += count;
                while (type.length() < 30) {
                    type = type + " ";
                }
                statistic.add(type + " " + count);
            }
        });
        statistic.add(0, "" + total[0]);
        return statistic;
    }

    /**
     * Total row-count.
     */
    private long totalRowcount = 0;
    
    /**
     * Maximum number of total row-count (or 0).
     * For tests.
     */
    public static long maxTotalRowcount = 0;
    
    /**
     * Gets total row-count.
     * 
     * @return total row-count
     */
    public long getTotalRowcount() {
        return totalRowcount;
    }

    /**
     * Whether or not to store additional information in order to create a 'explain.log'
     */
    private boolean explain = false;

    /**
     * Next unique ID for association to be used for explanation.
     */
    private int nextExplainID = 1;
    
    /**
     * Unique IDs for each association to be used for explanation.
     */
    public Map<Association, Integer> explainIdOfAssociation = new HashMap<Association, Integer>();
    
    /**
     * Whether or not to store additional information in order to create a 'explain.log'.
     * 
     * @param explain <code>true</code> iff predecessors of each entity must be stored
     */
    public void setExplain(boolean explain) {
        this.explain = explain;
    }

    /**
     * Gets the universal primary key.
     * 
     * @retrun the universal primary key
     */
    public PrimaryKey getUniversalPrimaryKey() {
        return universalPrimaryKey;
    }
    
    /**
     * For creation of unique set-ids.
     */
    private int nextSetId = 1;
    
    /**
     * Creates a unique set id.
     * 
     * @return a unique set id
     */
    private synchronized int getNextSetId() {
        return graphID + (nextSetId++);
    }

    /**
     * Shuts down statement-executor.
     */
    public void shutDown() throws SQLException {
        statementExecutor.shutDown();
    }

}
