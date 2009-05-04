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
import java.io.OutputStream;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerConfigurationException;

import net.sf.jailer.database.Session.ResultSetReader;
import net.sf.jailer.datamodel.AggregationSchema;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.entitygraph.EntityGraph;
import net.sf.jailer.util.SqlScriptExecutor;
import net.sf.jailer.util.SqlUtil;

import org.apache.log4j.Logger;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

/**
 * A {@link ResultSetReader} that writes the read rows into an XML file.
 * 
 * @author Ralf Wisser
 */
public class XmlExportTransformer implements ResultSetReader {

	/**
	 * The table to read from.
	 */
	private Table table;

	/**
	 * For writing rows as xml.
	 */
	private final XmlRowWriter xmlRowWriter;

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
	 * Set of all cyclic aggregated tables.
	 */
	private final Set<Table> cyclicAggregatedTables;
	
	/**
     * The logger.
     */
    private static final Logger _log = Logger.getLogger(SqlScriptExecutor.class);

    /**
	 * Maps clear text SQL-types to {@link java.sql.Types}.
	 */
	private Map<Table, Map<String, Integer>> typeCache = new HashMap<Table, Map<String,Integer>>();

	/**
	 * Holds sorted lists of associations per table.
	 */
	private Map<Table, List<Association>> sortedAssociations = new HashMap<Table, List<Association>>();

	/**
	 * Association cache.
	 */
	private Map<Table, Map<String, Association>> associationCache = new HashMap<Table, Map<String,Association>>();

	/**
	 * Constructor.
	 * 
	 * @param out to write the xml into
	 * @param commentHeader comment at top of document
	 * @param entityGraph the entity graph
	 * @param totalProgress set of all tables for which entities exist in entityGraph
	 * @param rootTag root tag name
	 * @param datePattern pattern for dates
	 * @param timestampPattern pattern for time-stamps
	 */
	public XmlExportTransformer(OutputStream out, String commentHeader,
			EntityGraph entityGraph, Set<Table> totalProgress, Set<Table> cyclicAggregatedTables,
			String rootTag, String datePattern, String timestampPattern) throws TransformerConfigurationException, SAXException {
		this.xmlRowWriter = new XmlRowWriter(out, commentHeader, rootTag, datePattern, timestampPattern);
		this.entityGraph = entityGraph;
		this.totalProgress = totalProgress;
		this.cyclicAggregatedTables = cyclicAggregatedTables;
	}

	/**
	 * Reads result-set and writes into export-script.
	 */
	public void readCurrentRow(ResultSet resultSet) throws SQLException {
		try {
			writeEntity(table, null, resultSet, new ArrayList<String>());
		} catch (SAXException e) {
			throw new RuntimeException(e);
		} catch (ParserConfigurationException e) {
			throw new RuntimeException(e);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * Writes entity as XML hierarchy.
	 * 
	 * @param table entitie's table
	 * @param association association to parent, <code>null</code> for top-level entities
	 * @param resultSet current row contains entity to write out
	 * @param ancestors ancestors of entity to write out
	 */
	private void writeEntity(final Table table, Association association, final ResultSet resultSet, final List<String> ancestors)
			throws SQLException, SAXException, ParserConfigurationException, IOException {
		StringBuilder sb = new StringBuilder(table.getName() + "(");
		boolean f = true;
		int i = 0;
		for (Column pk : table.primaryKey.getColumns()) {
			if (!f) {
				sb.append(", ");
			}
			f = false;
			sb.append(SqlUtil.toSql(SqlUtil.getObject(resultSet, "PK" + i++,
					getTypeCache(table))));
		}
		sb.append(")");
		String primaryKey = sb.toString();

		if (ancestors.contains(primaryKey)) {
			throw new RuntimeException("cyclic aggregation: " + primaryKey
					+ " aggregates itself");
		}

		ancestors.add(primaryKey);
		
		TableMapping tableMapping = getTableMapping(table);
		
		Map<String, Association> associationMap = associationCache.get(table);
		if (associationMap == null) {
			associationMap = new HashMap<String, Association>();
			for (Association a: table.associations) {
				associationMap.put(a.getName(), a);
			}
			associationCache.put(table, associationMap);
		}
		final Map<String, Association> finalAssociationMap = associationMap;
		
		XmlUtil.visitDocumentNodes(tableMapping.template, xmlRowWriter.new XmlWritingNodeVisitor(resultSet, table, association) {
			public void visitAssociationElement(String associationName) {
				final Association sa = finalAssociationMap.get(associationName);
				if (sa != null) {
					if (totalProgress.contains(sa.destination)) {
						if (sa.getAggregationSchema() != AggregationSchema.NONE) {
							ResultSetReader reader = new ResultSetReader() {
								public void readCurrentRow(ResultSet resultSet) throws SQLException {
									try {
										writeEntity(sa.destination, sa, resultSet, ancestors);
									} catch (SAXException e) {
										throw new RuntimeException(e);
									} catch (ParserConfigurationException e) {
										throw new RuntimeException(e);
									} catch (IOException e) {
										throw new RuntimeException(e);
									}
								}
								public void close() {
								}
							};
							try {
								xmlRowWriter.startList(sa);
								entityGraph.readDependentEntities(sa.destination, sa, resultSet, reader, getTypeCache(sa.destination), getTableMapping(sa.destination).selectionSchema);
								if (cyclicAggregatedTables.contains(sa.destination)) {
									entityGraph.markDependentEntitiesAsTraversed(sa, resultSet, getTypeCache(sa.destination));
								}
								xmlRowWriter.endList(sa);
							} catch (Exception e) {
								throw new RuntimeException(e);
							}
						}
					}
				}
				
			}
		});
		ancestors.remove(ancestors.size() - 1);
	}

	/**
	 * Gets type cache for given table.
	 * 
	 * @param table the table
	 * @return type cache for table
	 */
	private Map<String, Integer> getTypeCache(Table table) {
		Map<String, Integer> cache = typeCache.get(table);
		if (cache == null) {
			cache = new HashMap<String, Integer>();
			typeCache.put(table, cache);
		}
		return cache;
	}

	/**
	 * Sets the table to read from.
	 */
	public void setTable(Table table) {
		this.table = table;
	}

	/**
	 * Closes the XML document.
	 */
	public void endDocument() throws SAXException {
		xmlRowWriter.close();
	}

	/**
	 * Flushes the export-reader.
	 */
	public void close() {
	}
	
	/**
	 * Holds XML mapping information.
	 */
	public class TableMapping {
		
		/**
		 * The template.
		 */
		public Document template;
		
		/**
		 * SQL selection schema.
		 */
		public String selectionSchema;
	}
	
	/**
	 * Mappings per table.
	 */
	private Map<Table, TableMapping> tableMappings = new HashMap<Table, TableMapping>();

	/**
	 * Gets the xml mapping for a table.
	 *  
	 * @param table the table
	 * @return xml mapping for table
	 */
	public TableMapping getTableMapping(Table table) throws ParserConfigurationException, SAXException, IOException {
		if (tableMappings.containsKey(table)) {
			return tableMappings.get(table);
		}
		TableMapping tableMapping = new TableMapping();
		tableMappings.put(table, tableMapping);
		
		tableMapping.template = table.getXmlTemplateAsDocument();
		final StringBuilder sb = new StringBuilder();
		int i = 0;
		for (Column pk: table.primaryKey.getColumns()) {
			if (sb.length() > 0) {
				sb.append(", ");
			}
			sb.append("T." + pk.name + " AS PK" + i++);
		}
		XmlUtil.visitDocumentNodes(tableMapping.template, new NodeVisitor() {
			int nr = 0;

			private void appendSchema(String text) {
				if (text != null && text.startsWith(XmlUtil.SQL_PREFIX)) {
					if (sb.length() > 0) {
						sb.append(", ");
					}
					sb.append(text.substring(XmlUtil.SQL_PREFIX.length()) + " AS C" + nr++);
				}
			}
			public void visitAssociationElement(String associationName) {
			}
			public void visitComment(String comment) {
			}
			public void visitElementEnd(String elementName, boolean isRoot) {
			}
			public void visitText(String text) {
				appendSchema(text);
			}
			public void visitElementStart(String elementName, boolean isRoot,
					String[] attributeNames, String[] attributeValues) {
				for (String value: attributeValues) {
					appendSchema(value);
				}
			}
		});
		tableMapping.selectionSchema = sb.toString();
		
		return tableMapping;
	}
	
}
