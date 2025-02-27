/*
 * Copyright 2007 - 2025 Ralf Wisser.
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
import java.nio.charset.Charset;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerConfigurationException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.database.Session;
import net.sf.jailer.database.Session.AbstractResultSetReader;
import net.sf.jailer.database.Session.ResultSetReader;
import net.sf.jailer.datamodel.AggregationSchema;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.RowIdSupport;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.entitygraph.EntityGraph;
import net.sf.jailer.subsetting.ObjectNotationOutputException;
import net.sf.jailer.subsetting.ScriptFormat;
import net.sf.jailer.util.CellContentConverter;
import net.sf.jailer.util.Quoting;

/**
 * A {@link ResultSetReader} that writes the read rows into an XML file.
 *
 * @author Ralf Wisser
 */
public class XmlExportTransformer extends AbstractResultSetReader {

	/**
	 * The table to read from.
	 */
	private Table table;
	
	/**
	 * <code>true</code> iff table is subject.
	 */
	private boolean tableIsSubject;
	
	/**
	 * For writing rows as xml.
	 */
	private final XmlRowWriter xmlRowWriter;

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
	private static final Logger _log = LoggerFactory.getLogger(XmlExportTransformer.class);

	/**
	 * Maps clear text SQL-types to {@link java.sql.Types}.
	 */
	private Map<Table, Map<String, Integer>> typeCache = new HashMap<Table, Map<String,Integer>>();
	
	/**
	 * Current session;
	 */
	private final Session session;

	/**
	 * Association cache.
	 */
	private Map<Table, Map<String, Association>> associationCache = new HashMap<Table, Map<String,Association>>();

	/**
	 * {@link RowIdSupport}.
	 */
	private final RowIdSupport rowIdSupport;
	
	private final ExecutionContext executionContext;

	private final Quoting quoting;
	
	private ScriptFormat scriptFormat;
	
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
	 * @param scriptFormat 
	 * @throws SQLException
	 */
	public XmlExportTransformer(OutputStream out, String commentHeader,
			EntityGraph entityGraph, Set<Table> totalProgress, Set<Table> cyclicAggregatedTables,
			String rootTag, String datePattern, String timestampPattern, Session session, ScriptFormat scriptFormat, Charset charset, ExecutionContext executionContext) throws TransformerConfigurationException, SAXException, SQLException {
		this.xmlRowWriter = new XmlRowWriter(out, commentHeader, rootTag, datePattern, timestampPattern, scriptFormat, charset, executionContext);
		this.entityGraph = entityGraph;
		this.scriptFormat = scriptFormat;
		this.totalProgress = totalProgress;
		this.cyclicAggregatedTables = cyclicAggregatedTables;
		this.session = session;
		this.quoting = Quoting.getQuoting(session);
		this.rowIdSupport = new RowIdSupport(entityGraph.getDatamodel(), session.dbms, executionContext);
		this.executionContext = executionContext;
	}

	private int rootCount = 0;
	
	/**
	 * Reads result-set and writes into export-script.
	 */
	@Override
	public void readCurrentRow(ResultSet resultSet) throws SQLException {
		try {
			if (executionContext.isDisallowNonAggregated() && !tableIsSubject) {
				throw new ObjectNotationOutputException("Non-aggregated objects (\"" + table.getName() + "\") are disallowed at root level.\n(" + scriptFormat + ")");
			}
			if (++rootCount > 1) {
				if (executionContext.isSingleRoot()) {
					throw new ObjectNotationOutputException("Multiple root objects are not allowed. (\"" + table.getName() + "\")\n(" + scriptFormat + ")");
				}
			}
			writeEntity(table, null, resultSet, new ArrayList<String>(), getCellContentConverter(resultSet, session, session.dbms));
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
	 * @param table entity's table
	 * @param association association to parent, <code>null</code> for top-level entities
	 * @param resultSet current row contains entity to write out
	 * @param ancestors ancestors of entity to write out
	 */
	private void writeEntity(final Table table, Association association, final ResultSet resultSet, final List<String> ancestors, final CellContentConverter cellContentConverter)
			throws SQLException, SAXException, ParserConfigurationException, IOException {
		StringBuilder sb = new StringBuilder(table.getName() + "(");
		boolean f = true;
		int i = 0;
		for (@SuppressWarnings("unused") Column pk : rowIdSupport.getPrimaryKey(table).getColumns()) {
			if (!f) {
				sb.append(", ");
			}
			f = false;
			sb.append(cellContentConverter.toSql(cellContentConverter.getObject(resultSet, "PK" + i++)));
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

		XmlUtil.visitDocumentNodes(tableMapping.template, xmlRowWriter.new XmlWritingNodeVisitor(resultSet, getMetaData(resultSet), table, association, session) {
			@Override
			public void visitAssociationElement(String associationName, String name) {
				final Association sa = finalAssociationMap.get(associationName);
				if (sa != null) {
					if (totalProgress.contains(sa.destination)) {
						if (sa.getAggregationSchema() != AggregationSchema.NONE) {
							ResultSetReader reader = new ResultSetReader() {
								@Override
								public void readCurrentRow(ResultSet resultSet) throws SQLException {
									try {
										writeEntity(sa.destination, sa, resultSet, ancestors, getCellContentConverter(resultSet, session, session.dbms));
									} catch (SAXException e) {
										throw new RuntimeException(e);
									} catch (ParserConfigurationException e) {
										throw new RuntimeException(e);
									} catch (IOException e) {
										throw new RuntimeException(e);
									}
								}
								@Override
								public void close() {
								}
							};
							try {
								xmlRowWriter.startList(sa, name);
								entityGraph.readDependentEntities(sa.destination, sa, resultSet, getMetaData(resultSet), reader, getTypeCache(sa.destination), getTableMapping(sa.destination).selectionSchema, getTableMapping(sa.destination).originalPKAliasPrefix);
								if (cyclicAggregatedTables.contains(sa.destination)) {
									entityGraph.markDependentEntitiesAsTraversed(sa, resultSet, getMetaData(resultSet), getTypeCache(sa.destination));
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
	 * @param tableIsSubject <code>true</code> iff table is subject
	 */
	public void setTableIsSubject(boolean tableIsSubject) {
		this.tableIsSubject = tableIsSubject;
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
	@Override
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

		/**
		 * Prefix of column aliases for selection of unfiltered PK values.
		 */
		public String originalPKAliasPrefix;
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
	public TableMapping getTableMapping(Table table) throws SAXException, IOException {
		if (tableMappings.containsKey(table)) {
			return tableMappings.get(table);
		}
		TableMapping tableMapping = new TableMapping();
		tableMappings.put(table, tableMapping);

		boolean isFiltered = false;
		for (Column c: table.getColumns()) {
			String filterExpression = null;
			if (c.getFilter() != null && c.getFilter().isApplyAtExport()) {
				filterExpression = c.getFilterExpression();
			}
			if (filterExpression != null) {
				isFiltered = true;
				break;
			}
		}

		if (isFiltered) {
			for (int i = 0; ; ++i) {
				tableMapping.originalPKAliasPrefix = "O" + i;
				boolean found = false;
				for (Column c: table.getColumns()) {
					if (c.name.startsWith(tableMapping.originalPKAliasPrefix)) {
						found = true;
						break;
					}
				}
				if (!found) {
					break;
				}
			}
		}

		try {
			tableMapping.template = table.getXmlTemplateAsDocument(quoting);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}

		final StringBuilder sb = new StringBuilder();
		int i = 0;
		for (Column pk: rowIdSupport.getPrimaryKey(table).getColumns()) {
			if (sb.length() > 0) {
				sb.append(", ");
			}
			if (tableMapping.originalPKAliasPrefix != null) {
				sb.append("T." + tableMapping.originalPKAliasPrefix + i + " as PK" + i);
				++i;
			} else {
				sb.append("T." + quoting.requote(pk.name) + " as PK" + i++);
			}
		}
		XmlUtil.visitDocumentNodes(tableMapping.template, new NodeVisitor() {
			int nr = 0;
			String reIdentifier = "((?:[\"][^\"]+[\"])|(?:[`][^`]+[`]))";
			Pattern pattern = Pattern.compile("T\\." + reIdentifier);
			
			private void appendSchema(String text) {
				if (text != null && text.startsWith(XmlUtil.SQL_PREFIX)) {
					if (sb.length() > 0) {
						sb.append(", ");
					}
					String sql = text.substring(XmlUtil.SQL_PREFIX.length());
					if (quoting.getQuote() != null && pattern.matcher(sql).matches()) {
						sql = sql.replace("\"", quoting.getQuote()).replace("`", quoting.getQuote());
					}
					sb.append(sql + " as C" + nr++);
				}
			}
			@Override
			public void visitAssociationElement(String associationName, String name) {
			}
			@Override
			public void visitComment(String comment) {
			}
			@Override
			public void visitElementEnd(String elementName, boolean isRoot) {
			}
			@Override
			public void visitText(String text) {
				appendSchema(text);
			}
			@Override
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
