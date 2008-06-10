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

package net.sf.jailer.xml;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.sf.jailer.database.StatementExecutor.ResultSetReader;
import net.sf.jailer.datamodel.AggregationSchema;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.entitygraph.EntityGraph;
import net.sf.jailer.util.SqlUtil;

/**
 * A {@link ResultSetReader} that writes the read rows into an XML file.
 * 
 * @author Ralf Wisser
 */
public class XmlExportReader implements ResultSetReader {

	/**
	 * The table to read from.
	 */
	private final Table table;

	/**
	 * The file to write to.
	 */
	private final OutputStreamWriter scriptFileWriter;

	/**
	 * Number of columns.
	 */
	private int columnCount;

	/**
	 * Labels of columns.
	 */
	private String[] columnLabel = null;

	/**
	 * Lob columns.
	 */
	private List<String> lobColumns = null;

	/**
	 * Lob columns indexes.
	 */
	private List<Integer> lobColumnIndexes = null;

	/**
	 * Labels of columns as comma separated list.
	 */
	private String labelCSL;

	/**
	 * Counts the exported entities. (GUI support)
	 */
	public static long numberOfExportedEntities;

	/**
	 * Counts the exported LOBs. (GUI support)
	 */
	public static long numberOfExportedLOBs;

	/**
	 * The entity graph.
	 */
	private final EntityGraph entityGraph;

	/**
	 * Set of all tables for which entities exist in entityGraph.
	 */
	private final Set<Table> totalProgress;
	
	/**
	 * Maps clear text SQL-types to {@link java.sql.Types}.
	 */
	private Map<Table, Map<String, Integer>> typeCache = new HashMap<Table, Map<String,Integer>>();

	/**
	 * Constructor.
	 * 
	 * @param table
	 *            the table to read from
	 * @param scriptFileWriter
	 *            the file to write to
	 * @param entityGraph the entity graph
	 * @param totalProgress set of all tables for which entities exist in entityGraph
	 */
	public XmlExportReader(Table table, OutputStreamWriter scriptFileWriter,
			EntityGraph entityGraph, Set<Table> totalProgress) {
		this.table = table;
		this.scriptFileWriter = scriptFileWriter;
		this.entityGraph = entityGraph;
		this.totalProgress = totalProgress;
	}

	/**
	 * Reads result-set and writes into export-script.
	 */
	public void readCurrentRow(ResultSet resultSet) throws SQLException {
		writeEntity(table, resultSet, new ArrayList<String>());
	}

	/**
	 * Writes entity as XML hierarchy.
	 * 
	 * @param resultSet
	 *            current row contains entity to write out
	 * @param ancestors
	 *            ancestors of entity to write out
	 */
	private void writeEntity(Table table, ResultSet resultSet, final List<String> ancestors)
			throws SQLException {
		StringBuilder sb = new StringBuilder(table.getName() + "(");
		boolean f = true;
		for (Column pk : table.primaryKey.getColumns()) {
			if (!f) {
				sb.append(", ");
			}
			f = false;
			sb.append(SqlUtil.toSql(SqlUtil.getObject(resultSet, pk.name,
					getTypeCache(table))));
		}
		sb.append(")");
		String primaryKey = sb.toString();

		if (ancestors.contains(primaryKey)) {
			throw new RuntimeException("cyclic aggregation: " + primaryKey
					+ " aggregates itself");
		}

		ancestors.add(primaryKey);
		try {
			for (int i = 0; i < ancestors.size(); ++i) {
				writeToScriptFile("  ");
			}
			writeToScriptFile(primaryKey + "\n");
		} catch (IOException e) {
			throw new RuntimeException(e);
		}

		for (final Association association: table.associations) {
			if (totalProgress.contains(association.destination)) {
				ResultSetReader reader = new ResultSetReader() {
					public void readCurrentRow(ResultSet resultSet) throws SQLException {
						writeEntity(association.destination, resultSet, ancestors);
					}
					public void close() {
					}
				};
				if (association.getAggregationSchema() != AggregationSchema.NONE) {
					entityGraph.readDependentEntities(association.destination, association, resultSet, reader, getTypeCache(association.destination));
				}
			}
		}
		
		// rec call

		ancestors.remove(ancestors.size() - 1);
	}

	/**
	 * Gets type cache for given table.
	 * 
	 * @param table the table
	 * @return type cache for table
	 */
	private Map<String, Integer> getTypeCache(Table table2) {
		Map<String, Integer> cache = typeCache.get(table);
		if (cache == null) {
			cache = new HashMap<String, Integer>();
			typeCache.put(table, cache);
		}
		return cache;
	}

	/**
	 * Checks if columns is part of primary key.
	 * 
	 * @param column
	 *            the column
	 * @return <code>true</code> if column is part of primary key
	 */
	private boolean isPrimaryKeyColumn(String column) {
		for (Column c : table.primaryKey.getColumns()) {
			if (c.name.equalsIgnoreCase(column)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Flushes the export-reader.
	 */
	public void close() {
	}

	/**
	 * Writes into script.
	 */
	private void writeToScriptFile(String content) throws IOException {
		synchronized (scriptFileWriter) {
			scriptFileWriter.write(content);
		}
	}

}
