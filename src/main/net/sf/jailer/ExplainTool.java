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

import java.io.FileWriter;
import java.io.IOException;
import java.sql.ResultSet;
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
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.entitygraph.EntityGraph;
import net.sf.jailer.util.SqlUtil;

import org.apache.log4j.Logger;

/**
 * Creates a listing of all association-paths of exported entities 
 * back to a subject in an entity-graph (the 'explain.log' file)
 * 
 * @author Ralf Wisser
 */
public class ExplainTool {

	/**
     * The logger.
     */
    private static final Logger _log = Logger.getLogger(ExplainTool.class);

    /**
     * Creates a listing of all association-paths of exported entities 
     * back to a subject in an entity-graph (the 'explain.log' file)
     * 
     * @param graph
     *            the entity-graph to explain
     * @param tablesToIgnore
     *            don't list path to entities of this tables
     * @param session
     *            for executing SQL-statements
     */
    public static void explain(final EntityGraph graph, final Set<Table> tablesToIgnore, final Session session, final DataModel datamodel) throws Exception {
        _log.info("generating explain.log...");
    	final Set<String> namesOfTablesToIgnore = new HashSet<String>();
        for (Table table: tablesToIgnore) {
            namesOfTablesToIgnore.add(table.getName());
        }
        StringBuffer succEqualsE = new StringBuffer();
        for (Column column: graph.getUniversalPrimaryKey().getColumns()) {
            if (succEqualsE.length() > 0) {
                succEqualsE.append(" and ");
            }
            succEqualsE.append("Succ.PRE_" + column.name + "=E." + column.name);
        }
        final FileWriter writer = new FileWriter("explain.log");
        String selectLeafs = "Select type, " + graph.getUniversalPrimaryKey().columnList(null) + " From " + SQLDialect.dmlTableReference(EntityGraph.ENTITY, session) + " E Where E.r_entitygraph=" + graph.graphID +
            " and not exists (Select * from " + SQLDialect.dmlTableReference(EntityGraph.ENTITY, session) + " Succ Where Succ.r_entitygraph=" + graph.graphID + " and Succ.PRE_TYPE=E.type and " + succEqualsE + ")";
        session.executeQuery(selectLeafs, new Session.AbstractResultSetReader() {
            Map<Integer, Integer> typeCache = new HashMap<Integer, Integer>();
        	public void readCurrentRow(ResultSet resultSet) throws SQLException {
                String type = resultSet.getString(1);
                List<String> keys = new ArrayList<String>();
                int i = 2;
                for (@SuppressWarnings("unused") Column column: graph.getUniversalPrimaryKey().getColumns()) {
                    keys.add(SqlUtil.toSql(SqlUtil.getObject(resultSet, getMetaData(resultSet), i++, typeCache), session));
                }
                if (!namesOfTablesToIgnore.contains(type)) {
                    try {
                        writer.append(path(graph, session, type, keys, datamodel));
                        writer.append(".\n");
                    } catch (IOException e) {
                        throw new RuntimeException(e);
                    }
                }
            }
        });
        writer.append("\n");
        List<String> associations = new ArrayList<String>();
        for (Map.Entry<Association, Integer> e: graph.explainIdOfAssociation.entrySet()) {
            String nr = "" + e.getValue();
            while (nr.length() < 5) {
                nr = " " + nr;
            }
            String sourceName = e.getKey().source.getName();
            while (sourceName.length() < 24) {
                sourceName = sourceName + " ";
            }
            associations.add("#" + nr + " " + sourceName + " -> " + e.getKey());
        }
        Collections.sort(associations);
        for (String line: associations) {
            writer.append(line);
            writer.append("\n");
        }
        writer.close();
    }

    /**
     * Stringifies the path of predecessors of a given entity.
     * @throws SQLException 
     */
    private static String path(final EntityGraph graph, final Session session, String type, List<String> keys, DataModel datamodel) throws SQLException {
        String where = "";
        int i = 0;
        for (Column column: graph.getUniversalPrimaryKey().getColumns()) {
            if (where.length() > 0) {
                where += " and ";
            }
            String value = keys.get(i++);
			where += column.name + (value == null || value.equals("null")? " is null" : ("=" + value));
        }
        String selectPredecessor = "Select PRE_TYPE, association, " + graph.getUniversalPrimaryKey().columnList("PRE_") + " From " + SQLDialect.dmlTableReference(EntityGraph.ENTITY, session) + " E Where E.r_entitygraph=" + graph.graphID +
            " and type='" + type + "' and " + where;
        final String preType[] = new String[1];
        final List<String> preKeys = new ArrayList<String>();
        final Integer associationID[] = new Integer[1];
        session.executeQuery(selectPredecessor, new Session.AbstractResultSetReader() {
            private Map<Integer, Integer> typeCache = new HashMap<Integer, Integer>();
			public void readCurrentRow(ResultSet resultSet) throws SQLException {
                preType[0] = resultSet.getString(1);
                associationID[0] = resultSet.getInt(2);
                int i = 3;
                for (@SuppressWarnings("unused") Column column: graph.getUniversalPrimaryKey().getColumns()) {
                    preKeys.add(SqlUtil.toSql(SqlUtil.getObject(resultSet, getMetaData(resultSet), i++, typeCache), session));
                }
            }
        });
        String thePath = entityAsString(type, keys, datamodel);
        if (preType[0] != null) {
            thePath = path(graph, session, preType[0], preKeys, datamodel) + " --" + associationID[0] + "--> " + thePath;
        }
        return thePath;
    }

    /**
     * Stringifies an entity.
     */
    private static String entityAsString(String type, List<String> keys, DataModel datamodel) {
        StringBuffer sb = new StringBuffer(type + "(");
        int i = 0, ki = 0;
        Map<Column, Column> match = datamodel.getUniversalPrimaryKey().match(datamodel.getTable(type).primaryKey);
        for (Column column: datamodel.getUniversalPrimaryKey().getColumns()) {
            if (match.get(column) != null) {
                if (i > 0) {
                    sb.append(", ");
                }
                ++i;
                sb.append(keys.get(ki));
            }
            ++ki;
        }
        sb.append(")");
        return sb.toString();
    }
    
}
