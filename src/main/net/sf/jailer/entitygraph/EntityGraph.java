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
import net.sf.jailer.database.SQLDialect;
import net.sf.jailer.database.Session;
import net.sf.jailer.database.Session.ResultSetReader;
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
    public final Session session;

	/**
     * The logger.
     */
    private static final Logger _log = Logger.getLogger(SqlScriptExecutor.class);

    /**
     * The universal primary key.
     */
    private final PrimaryKey universalPrimaryKey;
    
    /**
     * Birthday of subject rows.
     */
    private int birthdayOfSubject = 0;
    
    /**
     * Constructor.
     * 
     * @param graphID the unique ID of the graph
     * @param session for executing SQL-Statements
     * @param universalPrimaryKey the universal primary key
     */
    private EntityGraph(int graphID, Session session, PrimaryKey universalPrimaryKey) {
        this.graphID = graphID;
        this.session = session;
        this.universalPrimaryKey = universalPrimaryKey;
    }
    
    /**
     * Sets birthday of subject rows.
     * 
     * @param birthdayOfSubject birthday of subject rows
     */
    public void setBirthdayOfSubject(int birthdayOfSubject) {
    	this.birthdayOfSubject = birthdayOfSubject;
    }
    
    /**
     * Creates a new entity-graph.
     * 
     * @param graphID the unique ID of the graph
     * @param session for executing SQL-Statements
     * @param universalPrimaryKey the universal primary key
     * @return the newly created entity-graph
     */
    public static EntityGraph create(int graphID, Session session, PrimaryKey universalPrimaryKey) {
        EntityGraph entityGraph = new EntityGraph(graphID, session, universalPrimaryKey);
        try {
            session.executeUpdate("Insert into " + SQLDialect.dmlTableReference(ENTITY_GRAPH, session) + "(id, age) values (" + graphID + ", 1)");
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
     * @param session for executing SQL-Statements
     * @return the newly created entity-graph
     */
    public static EntityGraph copy(EntityGraph graph, int graphID, Session session) throws SQLException {
        EntityGraph entityGraph = create(graphID, session, graph.universalPrimaryKey);
        entityGraph.setBirthdayOfSubject(graph.birthdayOfSubject);
        session.executeUpdate(
                "Insert into " + SQLDialect.dmlTableReference(ENTITY, session) + "(r_entitygraph, " + graph.universalPrimaryKey.columnList(null) + ", birthday, orig_birthday, type) " +
                    "Select " + graphID + ", " + graph.universalPrimaryKey.columnList(null) + ", birthday, birthday, type From " + SQLDialect.dmlTableReference(ENTITY, session) + " Where r_entitygraph=" + graph.graphID + "");
        return entityGraph;
    }

    /**
     * Finds an entity-graph.
     * 
     * @param graphID the unique ID of the graph
     * @param universalPrimaryKey the universal primary key
     * @param session for executing SQL-Statements
     * @return the entity-graph
     */
    public static EntityGraph find(int graphID, Session session, PrimaryKey universalPrimaryKey) throws SQLException {
        EntityGraph entityGraph = new EntityGraph(graphID, session, universalPrimaryKey);
        final boolean[] found = new boolean[1];
        found[0] = false;
        session.executeQuery("Select * From " + SQLDialect.dmlTableReference(ENTITY_GRAPH, session) + "Where id=" + graphID + "", new Session.ResultSetReader() {
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
        session.executeQuery("Select age From " + SQLDialect.dmlTableReference(ENTITY_GRAPH, session) + " Where id=" + graphID + "", new Session.ResultSetReader() {
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
        session.executeUpdate("Update " + SQLDialect.dmlTableReference(ENTITY_GRAPH, session) + " Set age=" + age + " Where id=" + graphID + "");
    }
    
    /**
     * Gets the number of entities in the graph.
     * 
     * @return the number of entities in the graph
     */
    public long getSize() throws SQLException {
        final int[] size = new int[1];
        size[0] = -1;
        session.executeQuery("Select count(*) From " + SQLDialect.dmlTableReference(ENTITY, session) + " Where r_entitygraph=" + graphID + " and birthday >= 0", new Session.ResultSetReader() {
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
        session.executeUpdate("Delete from " + SQLDialect.dmlTableReference(DEPENDENCY, session) + " Where r_entitygraph=" + graphID + "");
        session.executeUpdate("Delete from " + SQLDialect.dmlTableReference(ENTITY, session) + " Where r_entitygraph=" + graphID + "");
        session.executeUpdate("Delete from " + SQLDialect.dmlTableReference(ENTITY_GRAPH, session) + " Where id=" + graphID + "");
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
        return addEntities(table, "T", condition, null, null, null, null, false, today, limit, 0, true);
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
            return addEntities(association.destination, destAlias, "E.r_entitygraph=" + graphID + " and E.birthday = " + (today - 1) + " and E.type='" + table.getName() + "' and " + pkEqualsEntityID(table, sourceAlias, "E"), table, sourceAlias, association.source, jc, true, today, 0, associationExplanationID, association.reversed);
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
    private long addEntities(Table table, String alias, String condition, Table joinedTable, String joinedTableAlias, Table source, String joinCondition, boolean joinWithEntity, int today, long limit, int associationExplanationID, boolean isInverseAssociation) throws SQLException {
        if (maxTotalRowcount > 0 && limit == 0) {
            limit = Math.max(maxTotalRowcount - totalRowcount + 1, 1);
        }
        if (joinCondition != null) {
        	joinCondition = SqlUtil.resolvePseudoColumns(joinCondition, isInverseAssociation? null : "E", isInverseAssociation? "E" : null, today, birthdayOfSubject);
        }
        String select =
            "Select " + (joinedTable != null? "distinct " : "") + "" + graphID + " as GRAPH_ID, " + pkList(table, alias) + ", " + today + " AS TODAY, '" + table.getName() + "' AS TYPE" +
            (source == null || !explain? "" : ", " + associationExplanationID + " AS ASSOCIATION, '" + source.getName() + "' AS SOURCE_TYPE, " + pkList(source, joinedTableAlias, "PRE_")) +
            " From " + table.getName() + " " + alias +
            " left join " + SQLDialect.dmlTableReference(ENTITY, session) + " Duplicate on Duplicate.r_entitygraph=" + graphID + " and Duplicate.type='" + table.getName() + "' and " +
            pkEqualsEntityID(table, alias, "Duplicate") + 
            (joinedTable != null? ", " + joinedTable.getName() + " " + joinedTableAlias + " ": "") +
            (joinWithEntity? ", " + SQLDialect.dmlTableReference(ENTITY, session) + " E" : "") +
            " Where (" + condition + ") and Duplicate.type is null" +
            (joinedTable != null? " and (" + joinCondition + ")" : "") +
            (limit > 0? " fetch first " + limit + " rows only" : ""); 
        
        if (source != null && explain) {
            String max = "";
            Map<Column, Column> match = universalPrimaryKey.match(source.primaryKey);
            for (Column column: universalPrimaryKey.getColumns()) {
            	if (match.get(column) != null) {
	                if (max.length() > 0) {
	                    max += ", ";
	                }
	                max += "max(PRE_" + column.name + ")";
            	}
            }
            select = "Select GRAPH_ID, " + upkColumnList(table, null) + ", TODAY, TYPE, ASSOCIATION, max(SOURCE_TYPE), " + max + " From (" + select + ") Q " +
                     "Group by GRAPH_ID, " + upkColumnList(table, null) + ", TODAY, TYPE, ASSOCIATION";
        }
        
        String insert = "Insert into " + SQLDialect.dmlTableReference(ENTITY, session) + " (r_entitygraph, " + upkColumnList(table, null) + ", birthday, type" + (source == null || !explain? "" : ", association, PRE_TYPE, " + upkColumnList(source, "PRE_"))  + ") " + select;
        if (SqlUtil.dbms == DBMS.SYBASE) session.execute("set forceplan on ");
        long rc = session.executeUpdate(insert);
        totalRowcount += rc;
        if (SqlUtil.dbms == DBMS.SYBASE) session.execute("set forceplan off ");
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
    public void addDependencies(Table from, String fromAlias, Table to, String toAlias, String condition, int aggregationId, int dependencyId, boolean isAssociationReversed) throws SQLException {
    	condition = SqlUtil.resolvePseudoColumns(condition, isAssociationReversed? "E1" : "E2", isAssociationReversed? "E2" : "E1", 0, birthdayOfSubject);
        String insert = "Insert into " + SQLDialect.dmlTableReference(DEPENDENCY, session) + "(r_entitygraph, assoc, depend_id, from_type, to_type, " + upkColumnList(from, "FROM_") + ", " + upkColumnList(to, "TO_") + ") " +
            "Select " + graphID + ", " + aggregationId  + ", " + dependencyId + ", '" + from.getName() + "', '" + to.getName() + "', " + pkList(from, fromAlias, "FROM") + ", " + pkList(to, toAlias, "TO") +
            " From " + SQLDialect.dmlTableReference(ENTITY, session) + " E1, " + SQLDialect.dmlTableReference(ENTITY, session) + " E2, " + from.getName() + " " + fromAlias + " ," + to.getName() + " " + toAlias + " " +
            " Where E1.r_entitygraph=" + graphID + " and E2.r_entitygraph=" + graphID + "" +
            " and (" + condition + ")" +
            " and E1.type='" + from.getName() + "' and E2.type='" + to.getName() + "'" +
            " and " + pkEqualsEntityID(from, fromAlias, "E1") +
            " and " + pkEqualsEntityID(to, toAlias, "E2");
        totalRowcount += session.executeUpdate(insert);
    }
    
    /**
     * Gets distinct association-ids of all edged.
     */
    public Set<Integer> getDistinctDependencyIDs() throws SQLException {
        String select = "Select distinct depend_id from " + SQLDialect.dmlTableReference(DEPENDENCY, session) + " Where r_entitygraph=" + graphID;
        final Set<Integer> ids = new HashSet<Integer>();
        session.executeQuery(select, new Session.ResultSetReader() {
			public void readCurrentRow(ResultSet resultSet) throws SQLException {
				ids.add(resultSet.getInt(1));
			}
			public void close() {
			}
        });
        return ids;
    }

    /**
     * Marks all entities of a given table which don't dependent on other entities,
     * s.t. they can be read and deleted.
     */
    public void markIndependentEntities(Table table) throws SQLException {
        StringBuffer fromEqualsPK = new StringBuffer();
        Map<Column, Column> match = universalPrimaryKey.match(table.primaryKey);
        for (Column column: universalPrimaryKey.getColumns()) {
            if (fromEqualsPK.length() > 0) {
                fromEqualsPK.append(" and ");
            }
        	if (match.get(column) != null) {
	            fromEqualsPK.append("D.FROM_" + column.name + "=" + SQLDialect.dmlTableReference(ENTITY, session) + "." + column.name);
        	} else {
        		fromEqualsPK.append("D.FROM_" + column.name + " is null and " + SQLDialect.dmlTableReference(ENTITY, session) + "." + column.name + " is null");
        	}
        }
        session.executeUpdate(
                "Update " + SQLDialect.dmlTableReference(ENTITY, session) + " set birthday=0 " +
                "Where r_entitygraph=" + graphID + " and birthday>0 and " +
                	   (table != null? "type='" + table.getName() + "' and " : "") +
                       "not exists (Select * from " + SQLDialect.dmlTableReference(DEPENDENCY, session) + " D " +
                           "Where D.r_entitygraph=" + graphID + " and D.assoc=0 and D.from_type=" + SQLDialect.dmlTableReference(ENTITY, session) + ".type and " +
                                 fromEqualsPK + ")");
    }

    /**
     * Marks all rows which are not target of a dependency.
     */
    public void markRoots(Table table) throws SQLException {
        StringBuffer toEqualsPK = new StringBuffer();
        Map<Column, Column> match = universalPrimaryKey.match(table.primaryKey);
        for (Column column: universalPrimaryKey.getColumns()) {
            if (toEqualsPK.length() > 0) {
                toEqualsPK.append(" and ");
            }
            if (match.containsKey(column)) {
            	toEqualsPK.append("D.TO_" + column.name + "=" + SQLDialect.dmlTableReference(ENTITY, session) + "." + column.name);
            } else {
            	toEqualsPK.append("D.TO_" + column.name + " is null and " + SQLDialect.dmlTableReference(ENTITY, session) + "." + column.name + " is null");
            }
        }
        session.executeUpdate(
                "Update " + SQLDialect.dmlTableReference(ENTITY, session) + " set birthday=0 " +
                "Where r_entitygraph=" + graphID + " and birthday>0 and type='" + table.getName() + "' and " +
                       "not exists (Select * from " + SQLDialect.dmlTableReference(DEPENDENCY, session) + " D " +
                           "Where D.r_entitygraph=" +graphID + " and D.to_type=" + SQLDialect.dmlTableReference(ENTITY, session) + ".type and " +
                                 toEqualsPK + ")");
    }

    /**
     * Reads all entities of a given table which are marked as independent or as roots.
     * 
     * @param reader for reading the result-set
     * @param table the table
     * @param orderByPK if <code>true</code>, result will be ordered by primary keys
     */
    public void readMarkedEntities(Table table, Session.ResultSetReader reader, boolean orderByPK) throws SQLException {
    	readMarkedEntities(table, reader, filteredSelectionClause(table), orderByPK);
    }
    
    /**
     * Reads all entities of a given table which are marked as independent or as roots.
     * 
     * @param reader for reading the result-set
     * @param table the table
     * @param orderByPK if <code>true</code>, result will be ordered by primary keys
     */
    public void readMarkedEntities(Table table, Session.ResultSetReader reader, String selectionSchema, boolean orderByPK) throws SQLException {
        String orderBy = "";
        if (orderByPK) {
        	orderBy = " order by " + table.primaryKey.columnList("T.");
        }
    	session.executeQuery(
                "Select " + selectionSchema + " From " + SQLDialect.dmlTableReference(ENTITY, session) + " E join " + table.getName() + " T on " +
                pkEqualsEntityID(table, "T", "E") +
                " Where E.birthday=0 and E.r_entitygraph=" + graphID + " and E.type='" + table.getName() + "'" +
                orderBy,
                reader);
    }
    
    /**
     * Reads all entities of a given table which are marked as independent or as roots.
     * 
     * @param reader for reading the result-set
     * @param table the table
     * @param orderByPK if <code>true</code>, result will be ordered by primary keys
     */
    public void readMarkedEntities(Table table, Session.ResultSetReader reader, String selectionSchema, String originalPKAliasPrefix, boolean orderByPK) throws SQLException {
        if (originalPKAliasPrefix == null) {
        	readMarkedEntities(table, reader, selectionSchema, orderByPK);
        	return;
        }
    	String orderBy = "";
    	StringBuffer sb = new StringBuffer();
    	StringBuffer selectOPK = new StringBuffer();
    	for (int i = 0; i < table.primaryKey.getColumns().size(); ++i) {
    		if (i > 0) {
    			sb.append(", ");
    			selectOPK.append(", ");
    		}
    		sb.append(originalPKAliasPrefix + i);
    		selectOPK.append("T." + table.primaryKey.getColumns().get(i).name + " AS " + originalPKAliasPrefix + i);
    	}
    	orderBy = "order by " + sb;
    	session.executeQuery(
    			"Select " + selectionSchema + " From (" +
                "Select " + selectOPK + ", " + filteredSelectionClause(table) + " From " + SQLDialect.dmlTableReference(ENTITY, session) + " E join " + table.getName() + " T on " +
                pkEqualsEntityID(table, "T", "E") +
                " Where E.birthday=0 and E.r_entitygraph=" + graphID + " and E.type='" + table.getName() + "'" +
                ") T " +
                (orderByPK? orderBy : ""),
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
        session.executeUpdate("Update " + SQLDialect.dmlTableReference(ENTITY, session) + " E1 " +
                "set E1.r_entitygraph=" + graphID + " " +
                "Where E1.r_entitygraph=" + graph.graphID + " " +
                "and not exists(Select * from " + SQLDialect.dmlTableReference(ENTITY, session) + " E2 Where " +
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
     * @param orderByPK if <code>true</code>, result will be ordered by primary keys
     */
    public void readEntities(Table table, Session.ResultSetReader reader, boolean orderByPK) throws SQLException {
        session.executeQuery(
                "Select " + filteredSelectionClause(table) + " From " + SQLDialect.dmlTableReference(ENTITY, session) + " E join " + table.getName() + " T on " +
                pkEqualsEntityID(table, "T", "E") +
                " Where E.birthday>=0 and E.r_entitygraph=" + graphID + " and E.type='" + table.getName() + "'" +
                (orderByPK? " order by " + table.primaryKey.columnList("T.") : ""),
                reader);
    }
    
    /**
     * Gets select clause for reading rows of given type
     * with respect of the column filters.
     * 
     * @param table the table to read rows from
     * @return select clause
     */
    private String filteredSelectionClause(Table table) {
    	StringBuilder sb = new StringBuilder();
    	boolean first = true;
    	
    	for (Column c: table.getColumns()) {
    		if (!first) {
    			sb.append(", ");
    		}
    		String filterExpression = c.getFilterExpression();
			if (filterExpression != null) {
				if (filterExpression.trim().toLowerCase().startsWith("select")) {
					sb.append("(" + filterExpression + ")");
				} else {
					sb.append(filterExpression);
				}
    		} else {
    			sb.append("T." + c.name);
    		}
    		sb.append(" as " + c.name);
    		first = false;
    	}
    	
		return sb.toString();
	}

	/**
     * Deletes all entities which are marked as independent.
     */
    public void deleteIndependentEntities(Table table) throws SQLException {
        StringBuffer fromEqualsPK = new StringBuffer();
        StringBuffer toEqualsPK = new StringBuffer();
        Map<Column, Column> match = universalPrimaryKey.match(table.primaryKey);
        for (Column column: universalPrimaryKey.getColumns()) {
            if (fromEqualsPK.length() > 0) {
                fromEqualsPK.append(" and ");
            }
            if (match.containsKey(column)) {
            	fromEqualsPK.append(SQLDialect.dmlTableReference(DEPENDENCY, session) + ".FROM_" + column.name + "=" + column.name);
            } else {
            	fromEqualsPK.append(SQLDialect.dmlTableReference(DEPENDENCY, session) + ".FROM_" + column.name + " is null and " + column.name + " is null");
            }
            if (toEqualsPK.length() > 0) {
                toEqualsPK.append(" and ");
            }
            if (match.containsKey(column)) {
            	toEqualsPK.append(SQLDialect.dmlTableReference(DEPENDENCY, session) + ".TO_" + column.name + "=" + column.name);
            } else {
            	toEqualsPK.append(SQLDialect.dmlTableReference(DEPENDENCY, session) + ".TO_" + column.name + " is null and " + column.name + " is null");
            }
        }
        session.executeUpdate(
                "Delete From " + SQLDialect.dmlTableReference(DEPENDENCY, session) + " " +
                "Where " + SQLDialect.dmlTableReference(DEPENDENCY, session) + ".r_entitygraph=" + graphID + " and assoc=0 and from_type='" + table.getName() + "' and " + 
                      "exists (Select * from " + SQLDialect.dmlTableReference(ENTITY, session) + " E Where " + 
                          "E.r_entitygraph=" + graphID + " and " +
                          fromEqualsPK + " and " + SQLDialect.dmlTableReference(DEPENDENCY, session) + ".from_type=E.type and " +
                          "E.birthday=0)");
        session.executeUpdate(
                "Delete From " + SQLDialect.dmlTableReference(DEPENDENCY, session) + " " +
                "Where " + SQLDialect.dmlTableReference(DEPENDENCY, session) + ".r_entitygraph=" + graphID + " and assoc=0 and to_type='" + table.getName() + "' and " +
                      "exists (Select * from " + SQLDialect.dmlTableReference(ENTITY, session) + " E Where " + 
                          "E.r_entitygraph=" + graphID + " and " +
                          toEqualsPK + " and " + SQLDialect.dmlTableReference(DEPENDENCY, session) + ".to_type=E.type and " +
                          "E.birthday=0)");
        session.executeUpdate(
                "Delete From " + SQLDialect.dmlTableReference(ENTITY, session) + " " +
                "Where r_entitygraph=" + graphID + " and type='" + table.getName() + "' and " +
                       "birthday=0");
    }
    
    /**
     * Deletes all entities from a given table.
     */
    public long deleteEntities(Table table) throws SQLException {
        return session.executeUpdate(
                "Delete From " + SQLDialect.dmlTableReference(ENTITY, session) + " " +
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
        session.executeQuery(
                "Select count(*) from " + SQLDialect.dmlTableReference(ENTITY, session) + " E " +
                "Where E.birthday>=0 and E.r_entitygraph=" + graphID + " and E.type='" + table.getName() + "'",
                new Session.AbstractResultSetReader() {
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
            jc = SqlUtil.resolvePseudoColumns(jc, association.reversed? "EB" : "EA", association.reversed? "EA" : "EB", 0, birthdayOfSubject, "orig_birthday");
            String remove = "Insert into " + SQLDialect.dmlTableReference(ENTITY_SET_ELEMENT, session) + "(set_id, type, " + universalPrimaryKey.columnList(null) + ") " +
                "Select distinct " + setId + ", EB.type, " + universalPrimaryKey.columnList("EB.") + " from " + SQLDialect.dmlTableReference(ENTITY, session) + " EB " +
                "join " + association.destination.getName() + " " + destAlias + " on "+ pkEqualsEntityID(association.destination, destAlias, "EB") + " " +
                ", " + association.source.getName() + " " + sourceAlias + " " +
                (deletedEntitiesAreMarked? "join " : "left join ") + SQLDialect.dmlTableReference(ENTITY, session) + " EA on EA.r_entitygraph=" + graphID + " and EA.type='" + association.source.getName() + "' and " + pkEqualsEntityID(association.source, sourceAlias, "EA") + " " +
                "Where EB.r_entitygraph=" + graphID + " and EB.type='" + association.destination.getName() + "' " +
                "and (" + jc + ") " +
                "and " + (deletedEntitiesAreMarked? "EA.birthday=-1 and EB.birthday>=0" : "EA.type is null");
            long rc = session.executeUpdate(remove);
            totalRowcount += rc;
            if (rc > 0) {
            	Map<Column, Column> match = universalPrimaryKey.match(association.destination.primaryKey);
                StringBuffer sEqualsE = new StringBuffer();
            	StringBuffer sEqualsEWoAlias = new StringBuffer();
                for (Column column: universalPrimaryKey.getColumns()) {
                	if (sEqualsE.length() > 0) {
                        sEqualsE.append(" and ");
                    }
                    if (sEqualsEWoAlias.length() > 0) {
                    	sEqualsEWoAlias.append(" and ");
                    }
                	if (match.containsKey(column)) {
	                    sEqualsE.append("S." + column.name + "=E." + column.name);
	                    sEqualsEWoAlias.append("S." + column.name + "=" + SQLDialect.dmlTableReference(ENTITY, session) + "." + column.name);
                	} else {
	                    sEqualsE.append("S." + column.name + " is null and E." + column.name + " is null");
	                    sEqualsEWoAlias.append("S." + column.name + " is null and " + SQLDialect.dmlTableReference(ENTITY, session) + "." + column.name + " is null");
                	}
                }
                remove = "Update " + SQLDialect.dmlTableReference(ENTITY, session) + " E set E.birthday=-1 Where E.r_entitygraph=" + graphID + " and E.type='" + association.destination.getName() + "' " +
                          "and exists (Select * from " + SQLDialect.dmlTableReference(ENTITY_SET_ELEMENT, session) + " S where S.set_id=" + setId + " and E.type=S.type and " + sEqualsE + ")";
                boolean silent = session.getSilent();
                try {
                	session.setSilent(true);
                	session.executeUpdate(remove);
                } catch (SQLException e) {
                	// postgreSQL
                	Session._log.debug("failed, retry without alias (" + e.getMessage() + ")");
                	remove = "Update " + SQLDialect.dmlTableReference(ENTITY, session) + " set birthday=-1 Where " + SQLDialect.dmlTableReference(ENTITY, session) + ".r_entitygraph=" + graphID + " and " + SQLDialect.dmlTableReference(ENTITY, session) + ".type='" + association.destination.getName() + "' " +
                    "and exists (Select * from " + SQLDialect.dmlTableReference(ENTITY_SET_ELEMENT, session) + " S where S.set_id=" + setId + " and " + SQLDialect.dmlTableReference(ENTITY, session) + ".type=S.type and " + sEqualsEWoAlias + ")";
                	session.executeUpdate(remove);
                } finally {
                	session.setSilent(silent);
                }
                session.executeUpdate("Delete from " + SQLDialect.dmlTableReference(ENTITY_SET_ELEMENT, session) + " where set_id=" + setId + "");
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
    public void readDependentEntities(Table table, Association association, ResultSet resultSet, ResultSetReader reader, Map<String, Integer> typeCache, String selectionSchema, String originalPKAliasPrefix) throws SQLException {
    	String select;
    	if (originalPKAliasPrefix != null) {
        	StringBuffer selectOPK = new StringBuffer();
        	for (int i = 0; i < table.primaryKey.getColumns().size(); ++i) {
        		if (i > 0) {
        			selectOPK.append(", ");
        		}
        		selectOPK.append("T." + table.primaryKey.getColumns().get(i).name + " AS " + originalPKAliasPrefix + i);
        	}
    		select = 
    			"Select " + selectionSchema + " from (" +  
    			"Select " + selectOPK + ", " + filteredSelectionClause(table) + " from " + table.getName() + " T join " + SQLDialect.dmlTableReference(DEPENDENCY, session) + " D on " +
	    		 pkEqualsEntityID(table, "T", "D", "TO_") + " and D.to_type='" + table.getName() + "'" +
	    		 " Where " + pkEqualsEntityID(association.source, resultSet, "D", "FROM_", typeCache, session) +
	    	     " and D.from_type='" + association.source.getName() + "' and assoc=" + association.getId() +
	    	     " and D.r_entitygraph=" + graphID + ") T";
    	} else {
	    	select = "Select " + selectionSchema + " from " + table.getName() + " T join " + SQLDialect.dmlTableReference(DEPENDENCY, session) + " D on " +
	    		 pkEqualsEntityID(table, "T", "D", "TO_") + " and D.to_type='" + table.getName() + "'" +
	    		 " Where " + pkEqualsEntityID(association.source, resultSet, "D", "FROM_", typeCache, session) +
	    	     " and D.from_type='" + association.source.getName() + "' and assoc=" + association.getId() +
	    	     " and D.r_entitygraph=" + graphID;
    	}
    	session.executeQuery(select, reader);
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
    	if (session.dbms == DBMS.SYBASE) {
    		update = "Update " + SQLDialect.dmlTableReference(DEPENDENCY, session) + " set traversed=1" +
    		 " Where " + pkEqualsEntityID(association.source, resultSet, SQLDialect.dmlTableReference(DEPENDENCY, session), "FROM_", typeCache, session) +
    		 " and " + SQLDialect.dmlTableReference(DEPENDENCY, session) + ".from_type='" + association.source.getName() + "' and assoc=" + association.getId() +
    		 " and " + SQLDialect.dmlTableReference(DEPENDENCY, session) + ".r_entitygraph=" + graphID;
    	} else {
    		update = "Update " + SQLDialect.dmlTableReference(DEPENDENCY, session) + " D set traversed=1" +
    		 " Where " + pkEqualsEntityID(association.source, resultSet, "D", "FROM_", typeCache, session) +
    	     " and D.from_type='" + association.source.getName() + "' and assoc=" + association.getId() +
    	     " and D.r_entitygraph=" + graphID;
    	}
    	session.executeUpdate(update);
    }
    
    /**
     * Reads all non-traversed dependencies. 
     * 
     * @param table the source of dependencies to look for
     * @param reader reads the entities
     */
    public void readNonTraversedDependencies(Table table, ResultSetReader reader) throws SQLException {
    	String select = "Select * from " + SQLDialect.dmlTableReference(DEPENDENCY, session) + " D " +
    		 " Where (traversed is null or traversed <> 1)" +
    	     " and D.from_type='" + table.getName() + "'" +
    	     " and D.r_entitygraph=" + graphID;
    	session.executeQuery(select, reader);
    }
    
    /**
     * Removes all reflexive dependencies of given table.
     * 
     * @param table the table
     */
	public void removeReflexiveDependencies(Table table) throws SQLException {
		Map<Column, Column> match = universalPrimaryKey.match(table.primaryKey);
        StringBuffer sb = new StringBuffer();
        for (Column column: universalPrimaryKey.getColumns()) {
            Column tableColumn = match.get(column);
            if (tableColumn != null) {
	            if (sb.length() > 0) {
	                sb.append(" and ");
	            }
	            sb.append("FROM_" + column.name + " = TO_" + column.name);
            }
        }
        String delete = "Delete from " + SQLDialect.dmlTableReference(DEPENDENCY, session) +
			" Where " + sb +
			" and from_type='" + table.getName() + "'" +
			" and to_type='" + table.getName() + "'" +
			" and r_entitygraph=" + graphID;
		session.executeUpdate(delete);
	}

	/**
     * Gets a SQL comparition expression for comparing rows with given entity.
     * 
     * @param table the table
     * @param resultSet
     * @return a SQL comparition expression for comparing rows of <code>table</code> with current row of resultSet
     */
    private String pkEqualsEntityID(Table table, ResultSet resultSet, String alias, String columnPrefix, Map<String, Integer> typeCache, Session session) throws SQLException {
    	Map<Column, Column> match = universalPrimaryKey.match(table.primaryKey);
        StringBuffer sb = new StringBuffer();
        for (Column column: universalPrimaryKey.getColumns()) {
            if (sb.length() > 0) {
                sb.append(" and ");
            }
            sb.append(alias + "." + columnPrefix + column.name);
            Column tableColumn = match.get(column);
            if (tableColumn != null) {
            	int i = 0;
            	for (Column c: table.primaryKey.getColumns()) {
            		if (c.name.equals(tableColumn.name)) {
            			break;
            		}
            		++i;
            	}
                sb.append("=" + SqlUtil.toSql(SqlUtil.getObject(resultSet, "PK" + i /* tableColumn.name*/, typeCache), session));
            } else {
                sb.append(" is null");
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
            sb.append(entityAlias + "." + columnPrefix + column.name);
            if (tableColumn != null) {
                sb.append("=" + tableAlias + "." + tableColumn.name);
            } else {
                sb.append(" is null");
            }
        }
        return sb.toString();
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
     * Gets PK-column list for a table. (for Select clause)
     * 
     * @param table the table
     * @param tableAlias the alias for table
     * @param columnAliasPrefix optional prefix for column names
     */
    private String pkList(Table table, String tableAlias, String columnAliasPrefix) {
        Map<Column, Column> match = universalPrimaryKey.match(table.primaryKey);
        StringBuffer sb = new StringBuffer();
        for (Column column: universalPrimaryKey.getColumns()) {
            Column tableColumn = match.get(column);
            if (tableColumn != null) {
	            if (sb.length() > 0) {
	                sb.append(", ");
	            }
                sb.append(tableAlias + "." + tableColumn.name);
                sb.append(" AS " + (columnAliasPrefix == null? "" : columnAliasPrefix) + column.name);
           }
        }
        return sb.toString();
    }

    /**
     * Gets PK-column list for a table. (for Insert clause)
     * 
     * @param table the table
     * @param columnAliasPrefix optional prefix for column names
     */
    private String upkColumnList(Table table, String columnAliasPrefix) {
        Map<Column, Column> match = universalPrimaryKey.match(table.primaryKey);
        StringBuffer sb = new StringBuffer();
        for (Column column: universalPrimaryKey.getColumns()) {
            Column tableColumn = match.get(column);
            if (tableColumn != null) {
	            if (sb.length() > 0) {
	                sb.append(", ");
	            }
	            if (columnAliasPrefix != null) {
	            	sb.append(columnAliasPrefix);
	            }
                sb.append(column.name);
            }
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
        session.executeQuery("Select type, count(*) From " + SQLDialect.dmlTableReference(ENTITY, session) + " Where r_entitygraph=" + graphID + " and birthday>=0 group by type", new Session.AbstractResultSetReader() {
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
     * @return the universal primary key
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
        session.shutDown();
    }

}
