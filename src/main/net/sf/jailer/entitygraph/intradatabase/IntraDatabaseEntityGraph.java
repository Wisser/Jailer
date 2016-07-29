/*
 * Copyright 2007 - 2016 the original author or authors.
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

import java.sql.ResultSet;
import java.sql.SQLException;

import net.sf.jailer.CommandLineParser;
import net.sf.jailer.Configuration;
import net.sf.jailer.database.SQLDialect;
import net.sf.jailer.database.Session;
import net.sf.jailer.database.StatementBuilder;
import net.sf.jailer.database.UPSERT_MODE;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.PrimaryKey;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.entitygraph.EntityGraph;
import net.sf.jailer.entitygraph.remote.RemoteEntityGraph;
import net.sf.jailer.progress.ProgressListenerRegistry;

/**
 * Specialized {@link RemoteEntityGraph} for exporting data
 * into a different schema within the same database.
 * 
 * @author Ralf Wisser
 */
public class IntraDatabaseEntityGraph extends RemoteEntityGraph {

    private boolean upsertOnly;

	/**
     * Constructor.
     * 
     * @param graphID the unique ID of the graph
     * @param session for executing SQL-Statements
     * @param universalPrimaryKey the universal primary key
     */
    private IntraDatabaseEntityGraph(DataModel dataModel, int graphID, Session session, PrimaryKey universalPrimaryKey) {
    	super(dataModel, graphID, session, universalPrimaryKey);
        upsertOnly = CommandLineParser.getInstance().upsertOnly;
    }
    
    /**
     * Creates a new entity-graph.
     * 
     * @param graphID the unique ID of the graph
     * @param session for executing SQL-Statements
     * @param universalPrimaryKey the universal primary key
     * @return the newly created entity-graph
     */
    public static IntraDatabaseEntityGraph create(DataModel dataModel, int graphID, Session session, PrimaryKey universalPrimaryKey) {
        IntraDatabaseEntityGraph entityGraph = new IntraDatabaseEntityGraph(dataModel, graphID, session, universalPrimaryKey);
        init(graphID, session);
        return entityGraph;
    }

    /**
     * Copies an entity-graph.
     * 
     * @param newGraphID the unique ID of the new graph
     * @param session for executing SQL-Statements
     * @return the newly created entity-graph
     */
    public EntityGraph copy(int newGraphID, Session session) throws SQLException {
        IntraDatabaseEntityGraph entityGraph = create(dataModel, newGraphID, session, universalPrimaryKey);
        entityGraph.setBirthdayOfSubject(birthdayOfSubject);
        session.executeUpdate(
                "Insert into " + SQLDialect.dmlTableReference(ENTITY, session) + "(r_entitygraph, " + universalPrimaryKey.columnList(null) + ", birthday, orig_birthday, type) " +
                    "Select " + newGraphID + ", " + universalPrimaryKey.columnList(null) + ", birthday, birthday, type From " + SQLDialect.dmlTableReference(ENTITY, session) + " Where r_entitygraph=" + graphID + "");
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
    public EntityGraph find(int graphID, Session session, PrimaryKey universalPrimaryKey) throws SQLException {
        IntraDatabaseEntityGraph entityGraph = new IntraDatabaseEntityGraph(dataModel, graphID, session, universalPrimaryKey);
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
     * Reads all entities of a given table which are marked as independent or as roots.
     * 
     * @param table the table
     * @param orderByPK not used
     */
    public void readMarkedEntities(Table table, boolean orderByPK) throws SQLException {
    	String selectionSchema = filteredSelectionClause(table);
    	String sql = "Select " + selectionSchema + " From " + SQLDialect.dmlTableReference(ENTITY, session) + " E join " + table.getName() + " T on " +
		pkEqualsEntityID(table, "T", "E") +
		" Where E.birthday=0 and E.r_entitygraph=" + graphID + " and E.type='" + table.getName() + "'";
		long rc = insertRows(table, sql);
    	ProgressListenerRegistry.getProgressListener().exported(table, rc);
    }

    /**
     * Reads all entities of a given table.
     * 
     * @param table the table
     * @param orderByPK not used
     */
    public void readEntities(Table table, boolean orderByPK) throws SQLException {
    	String sql = "Select " + filteredSelectionClause(table) + " From " + SQLDialect.dmlTableReference(ENTITY, session) + " E join " + table.getName() + " T on " +
			pkEqualsEntityID(table, "T", "E") +
			" Where E.birthday>=0 and E.r_entitygraph=" + graphID + " and E.type='" + table.getName() + "'";
		long rc;
		rc = insertRows(table, sql);
        ProgressListenerRegistry.getProgressListener().exported(table, rc);
    }
    
    /**
     * Gets qualified table name.
     * 
     * @param t the table
     * @return qualified name of t
     */
    private String qualifiedTableName(Table t) {
    	String schema = t.getOriginalSchema("");
    	String mappedSchema = CommandLineParser.getInstance().getSchemaMapping().get(schema);
    	if (mappedSchema != null) {
    		schema = mappedSchema;
    	}
    	if (schema.length() == 0) {
    		return t.getUnqualifiedName();
    	}
		return schema + "." + t.getUnqualifiedName();
	}
    
	/**
     * Checks if columns is part of primary key.
     * 
     * @param column the column
     * @return <code>true</code> if column is part of primary key
     */
    private boolean isPrimaryKeyColumn(Table table, String column) {
    	for (Column c: table.primaryKey.getColumns()) {
    		if (c.name.equalsIgnoreCase(column)) {
    			return true;
    		}
    	}
		return false;
	}

	private long insertRows(Table table, String sqlSelect) throws SQLException {
	    String labelCSL = insertClause(table, null);
		
		SQLDialect currentDialect = Configuration.forDbms(session).getSqlDialect();
		StatementBuilder upsertInsertStatementBuilder = new StatementBuilder(1);
		
        if (table.getUpsert() || upsertOnly) {
            if (table.primaryKey.getColumns().isEmpty()) {
            	throw new RuntimeException("Unable to merge/upsert into table \"" + table.getName() + "\".\n" +
            			"No primary key.");
            }

            String insertHead = "Insert into " + qualifiedTableName(table) + "(" + labelCSL + ") ";
            StringBuffer whereForTerminator = new StringBuffer("");
            
            // assemble 'where'
            boolean f = true;
            for (Column pk: table.primaryKey.getColumns()) {
                if (!f) {
                    whereForTerminator.append(" and ");
                }
                f = false;
                whereForTerminator.append("T." + pk.name + "=Q." + pk.name);
            }

            if (currentDialect.upsertMode == UPSERT_MODE.MERGE) {
            	// MERGE INTO JL_TMP T USING (SELECT 1 c1, 2 c2 from dual) incoming 
            	// ON (T.c1 = incoming.c1) 
            	// WHEN MATCHED THEN UPDATE SET T.c2 = incoming.c2 
            	// WHEN NOT MATCHED THEN INSERT (T.c1, T.c2) VALUES (incoming.c1, incoming.c2)
            	insertHead = "MERGE INTO " + qualifiedTableName(table) + " T USING(";
                StringBuffer terminator = new StringBuffer(") Q ON(" + whereForTerminator + ") ");
                
                StringBuffer sets = new StringBuffer();
                StringBuffer tSchema = new StringBuffer();
                StringBuffer iSchema = new StringBuffer();
                for (Column column: table.getColumns()) {
                    if  (!isPrimaryKeyColumn(table, column.name)) {
	                    if (sets.length() > 0) {
	                    	sets.append(", ");
	                    }
	                    sets.append("T." + column.name + "=Q." + column.name);
                    }
                    if (tSchema.length() > 0) {
                    	tSchema.append(", ");
                    }
                    tSchema.append("T." + column.name);
                    if (iSchema.length() > 0) {
                    	iSchema.append(", ");
                    }
                    iSchema.append("Q." + column.name);
                }
                if (sets.length() > 0) {
                	terminator.append("WHEN MATCHED THEN UPDATE SET " + sets + " ");
                }
            	terminator.append("WHEN NOT MATCHED THEN INSERT (" + tSchema + ") VALUES(" + iSchema + ")\n");

                upsertInsertStatementBuilder.append(insertHead, sqlSelect, "", terminator.toString());
                
                String sql = upsertInsertStatementBuilder.build();
                return session.executeUpdate(sql);
            } else {
            	insertHead += "Select " + insertClause(table, "Q") + " From (";
                StringBuffer terminator = new StringBuffer(") as Q(" + labelCSL + ") Where not exists (Select * from " + qualifiedTableName(table) + " T "
                        + "Where ");
                terminator.append(whereForTerminator + ");\n");

                upsertInsertStatementBuilder.append(insertHead, sqlSelect, "", terminator.toString());

                StringBuffer insert = new StringBuffer("");
                insert.append("Update " + qualifiedTableName(table) + " set ");
                
                String sql = upsertInsertStatementBuilder.build();
                
                // Update Oracle + DB2 + H2:
                //    update EMPLOYEE E set (name, hiredate) = (select D.name, D.location from EMPLOYEE T inner join DEPARTMENT D on T.deptno = D.deptno where T.empno=E.empno)
                
                // MSSql, postgres
                //    update EMPLOYEE set name=D.name from EMPLOYEE T inner join DEPARTMENT D on T.deptno = D.deptno

                // MySql
                //    update EMPLOYEE E inner join DEPARTMENT D on E.deptno = D.deptno set E.name=D.name, E.job=D.location
                
                // TODO
                return 0;
            }
        } else {
    	    StringBuilder sb = new StringBuilder();
        	sb.append("Insert into " + qualifiedTableName(table) + "(" + labelCSL + ") " + sqlSelect); 
    		return session.executeUpdate(sb.toString());
        }
	}

    /**
     * Gets insert clause for inserting rows of given type
     * with respect of the column filters.
     * 
     * @param table the table to read rows from
     * @return insert clause
     */
    private String insertClause(Table table, String tableAlias) {
    	StringBuilder sb = new StringBuilder();
    	boolean first = true;
    	
    	for (Column c: table.getColumns()) {
    		if (Configuration.forDbms(session).exportBlocks.contains(c.type)) {
    			continue;
    		}
    		if (!first) {
    			sb.append(", ");
    		}
    		if (tableAlias != null) {
    			sb.append(tableAlias + ".");
    		}
    		sb.append(c.name);
    		first = false;
    	}
    	
		return sb.toString();
	}

}
