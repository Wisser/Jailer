/*
 * Copyright 2007 - 2018 the original author or authors.
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
package net.sf.jailer.subsetting;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.apache.log4j.Logger;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.database.SQLDialect;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.RowIdSupport;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.entitygraph.EntityGraph;
import net.sf.jailer.util.CellContentConverter;
import net.sf.jailer.util.LogUtil;
import net.sf.jailer.util.Quoting;

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
	 * @param session
	 *            for executing SQL-statements
	 */
	public static void explain(final EntityGraph graph, final Session session, final ExecutionContext executionContext) throws SQLException, IOException {
		_log.info("generating explain.log...");
		final Quoting quoting = new Quoting(session);
		StringBuffer succEqualsE = new StringBuffer();
		for (Column column: graph.getUniversalPrimaryKey().getColumns()) {
			if (succEqualsE.length() > 0) {
				succEqualsE.append(" and ");
			}
			succEqualsE.append("Succ.PRE_" + column.name + "=E." + column.name);
		}
		final FileWriter writer = new FileWriter(newFile("explain.log"));
		final RowIdSupport rowIdSupport = new RowIdSupport(graph.getDatamodel(), session.dbms, executionContext);
		String selectLeafs = "Select type, " + graph.getUniversalPrimaryKey().columnList(null) + " From " + SQLDialect.dmlTableReference(EntityGraph.ENTITY, session, executionContext) + " E Where E.r_entitygraph=" + graph.graphID +
			" order by E.birthday, E.type";
		session.executeQuery(selectLeafs, new Session.AbstractResultSetReader() {
			@Override
			public void readCurrentRow(ResultSet resultSet) throws SQLException {
				int o = resultSet.getInt(1);
				String type = null;
				if (!resultSet.wasNull()) {
					Table tableByTypeName = graph.getDatamodel().getTableByOrdinal(o);
					type = tableByTypeName.getName();
				}
				List<String> keys = new ArrayList<String>();
				int i = 2;
				CellContentConverter cellContentConverter = getCellContentConverter(resultSet, session, session.dbms);
				for (@SuppressWarnings("unused") Column column: graph.getUniversalPrimaryKey().getColumns()) {
					keys.add(cellContentConverter.toSql(cellContentConverter.getObject(resultSet, i++)));
				}
				try {
					writer.append(path(graph, session, type, keys, graph.getDatamodel(), rowIdSupport, quoting, executionContext));
					writer.append(".\n");
					executionContext.getProgressListenerRegistry().fireExplained(1);
				} catch (IOException e) {
					throw new RuntimeException(e);
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
	 * @param rowIdSupport 
	 * @throws SQLException 
	 */
	private static String path(final EntityGraph graph, final Session session, String type, List<String> keys, DataModel datamodel, RowIdSupport rowIdSupport, Quoting quoting, ExecutionContext executionContext) throws SQLException {
		String where = "";
		int i = 0;
		for (Column column: graph.getUniversalPrimaryKey().getColumns()) {
			if (where.length() > 0) {
				where += " and ";
			}
			String value = keys.get(i++);
			where += quoting.requote(column.name) + (value == null || value.equals("null")? " is null" : ("=" + value));
		}
		String selectPredecessor = "Select PRE_TYPE, association, " + graph.getUniversalPrimaryKey().columnList("PRE_") + " From " + SQLDialect.dmlTableReference(EntityGraph.ENTITY, session, executionContext) + " E Where E.r_entitygraph=" + graph.graphID +
			" and type=" + datamodel.getTable(type).getOrdinal() + " and " + where;
		final String preType[] = new String[1];
		final List<String> preKeys = new ArrayList<String>();
		final Integer associationID[] = new Integer[1];
		session.executeQuery(selectPredecessor, new Session.AbstractResultSetReader() {
			@Override
			public void readCurrentRow(ResultSet resultSet) throws SQLException {
				int o = resultSet.getInt(1);
				if (!resultSet.wasNull()) {
					Table tableByTypeName = graph.getDatamodel().getTableByOrdinal(o);
					preType[0] = tableByTypeName.getName();
				}
				associationID[0] = resultSet.getInt(2);
				int i = 3;
				CellContentConverter cellContentConverter = getCellContentConverter(resultSet, session, session.dbms);
				for (@SuppressWarnings("unused") Column column: graph.getUniversalPrimaryKey().getColumns()) {
					preKeys.add(cellContentConverter.toSql(cellContentConverter.getObject(resultSet, i++)));
				}
			}
		});
		String thePath = entityAsString(type, keys, datamodel, rowIdSupport, session);
		if (preType[0] != null) {
			thePath = path(graph, session, preType[0], preKeys, datamodel, rowIdSupport, quoting, executionContext) + " --" + associationID[0] + "--> " + thePath;
		}
		return thePath;
	}

	/**
	 * Stringifies an entity.
	 * @param session 
	 */
	private static String entityAsString(String type, List<String> keys, DataModel datamodel, RowIdSupport rowIdSupport, Session session) {
		StringBuffer sb = new StringBuffer(type + "(");
		int i = 0, ki = 0;
		Map<Column, Column> match = rowIdSupport.getUniversalPrimaryKey().match(rowIdSupport.getPrimaryKey(datamodel.getTable(type)));
		for (Column column: rowIdSupport.getUniversalPrimaryKey().getColumns()) {
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
	
	private static File newFile(String name) {
		File home = null;
		if (new File(".singleuser").exists() // legacy 
				|| new File(".multiuser").exists()) {
			home = new File(System.getProperty("user.home"), ".jailer");
			home.mkdirs();
		}
		if (home == null || new File(name).isAbsolute()) {
			return new File(name);
		}
		return new File(home, name);
	}

}
