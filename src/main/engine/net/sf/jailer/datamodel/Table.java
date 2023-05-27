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
package net.sf.jailer.datamodel;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Comment;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import net.sf.jailer.database.Session;
import net.sf.jailer.util.Pair;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlUtil;
import net.sf.jailer.xml.NodeVisitor;
import net.sf.jailer.xml.XmlUtil;

/**
 * Describes a database-table.
 * 
 * @author Ralf Wisser
 */
public class Table extends ModelElement implements Comparable<Table> {

	/**
	 * The table-name.
	 */
	private final String name;
	
	/**
	 * The primary-key of the table.
	 */
	public final PrimaryKey primaryKey;
	
	/**
	 * List of table columns.
	 */
	private List<Column> columns = new ArrayList<Column>();
	
	/**
	 * Associations to other tables.
	 */
	public final List<Association> associations = new ArrayList<Association>();
	
	/**
	 * Use upsert (merge) or insert-statement for entities of this table in export-script.
	 */
	public Boolean upsert;
	
	/**
	 * Data model default for export mode.
	 */
	public final boolean defaultUpsert;
	
	/**
	 * Exclude from Deletion?
	 */
	public Boolean excludeFromDeletion;
	
	/**
	 * Data Model default for Exclude from Deletion.
	 */
	public final boolean defaultExcludeFromDeletion;
	
	/**
	 * Template for XML exports.
	 */
	private String xmlTemplate = null;
	
	/**
	 * The original table-name. Differs from name if a source-schema-mapping has been applied to the name.
	 */
	private String originalName;

	/**
	 * Unique number of this table.
	 */
	int ordinal;

	/**
	 * Constructor.
	 * 
	 * @param name the table-name
	 * @param primaryKey the names of the primary-key columns
	 * @param defaultUpsert data model default for export mode
	 */
	public Table(String name, PrimaryKey primaryKey, boolean defaultUpsert, boolean defaultExcludeFromDeletion) {
		this.name = name;
		this.primaryKey = primaryKey != null? primaryKey : new PrimaryKey(new ArrayList<Column>(), false);
		this.defaultUpsert = defaultUpsert;
		this.defaultExcludeFromDeletion = defaultExcludeFromDeletion;
	}

	/**
	 * Gets the table name.
	 * 
	 * @return the table name
	 */
	public String getName() {
		return name;
	}
	
	/**
	 * Gets export modus.
	 * 
	 * @return Boolean.TRUE for upsert/merge, Boolean.FALSE for insert
	 */
	public Boolean getUpsert() {
		return upsert == null? defaultUpsert : upsert;
	}
	
	/**
	 * Sets columns.
	 * 
	 * @param columns list of table columns
	 */
	public void setColumns(List<Column> columns) {
		this.columns = columns;
	}

	/**
	 * Gets columns.
	 * 
	 * @return list of table columns
	 */
	public List<Column> getColumns() {
		if (columns == null) {
			return Collections.emptyList();
		}
		return columns;
	}

	/**
	 * Compares tables.
	 */
	@Override
	public boolean equals(Object other) {
		if (other instanceof Table) {
			if (name == null) {
				return false;
			}
			return name.equals(((Table) other).name);
		}
		return false;
	}

	/**
	 * The hash-code.
	 */
	@Override
	public int hashCode() {
		return name == null? 0 : name.hashCode();
	}
	
	/**
	 * Stringifies the table.
	 */
	@Override
	public String toString() {
		String str = name + " (" + primaryKey + ")\n";
		List<Association> all = new ArrayList<Association>(associations);
		Collections.sort(all, new Comparator<Association>() {
			@Override
			public int compare(Association o1, Association o2) {
				return o1.destination.getName().compareTo(o2.destination.getName());
			}
		});
		
		List<Association> dep = new ArrayList<Association>();
		List<Association> hasDep = new ArrayList<Association>();
		List<Association> assoc = new ArrayList<Association>();
		List<Association> ignored = new ArrayList<Association>();
		for (Association association: all) {
			if (association.isIgnored()) {
				ignored.add(association);
			} else if (association.isInsertDestinationBeforeSource()) {
				dep.add(association);
			} else if (association.isInsertSourceBeforeDestination()) {
				hasDep.add(association);
			} else {
				assoc.add(association);
			}
		}
		
		if (!dep.isEmpty()) {
			str += "\n  depends on:\n";
			for (Association association: dep) {
				if (!"".equals(association.toString())) {
					str += "    " + association + "\n";
				}
			}
		}
		if (!hasDep.isEmpty()) {
			str += "\n  has dependent:\n";
			for (Association association: hasDep) {
				if (!"".equals(association.toString())) {
					str += "    " + association + "\n";
				}
			}
		}
		if (!assoc.isEmpty()) {
			str += "\n  is associated with:\n";
			for (Association association: assoc) {
				if (!"".equals(assoc.toString())) {
					str += "    " + association + "\n";
				}
			}
		}
		if (!ignored.isEmpty()) {
			str += "\n  ignored:\n";
			for (Association association: ignored) {
				if (!"".equals(association.toString())) {
					str += "    " + association + "\n";
				}
			}
		}

		return str + "\n";
	}

	@Override
	public int compareTo(Table o) {
		return name.compareTo(o.name);
	}
	
	/**
	 * Gets the closure of the table.
	 * 
	 * @return closure of the table (all tables associated (in-)direct with table)
	 */
	public Set<Table> closure() {
		return closure(new HashSet<Table>());
	}

	/**
	 * Gets the closure of the table.
	 * 
	 * @param tablesToIgnore ignore this tables
	 * 
	 * @return closure of the table (all tables associated (in-)direct with table)
	 */
	public Set<Table> closure(Set<Table> tablesToIgnore) {
		Set<Table> closure = new HashSet<Table>();
		List<Table> toCheck = new LinkedList<Table>();
		Set<Table> checked = new HashSet<Table>(tablesToIgnore);
		toCheck.add(this);
		while (!toCheck.isEmpty()) {
			Table table = toCheck.remove(0);
			if (!checked.contains(table)) {
				closure.add(table);
				checked.add(table);
				for (Association association: table.associations) {
					if (!checked.contains(association.destination)) {
						if (association.getJoinCondition() != null) {
							toCheck.add(association.destination);
						}
					}
				}
			}
		}
		
		return closure;
	}

	/**
	 * Gets the closure of the table.
	 * 
	 * @param tablesToIgnore ignore this tables
	 * @param distances put distances into it
	 * 
	 * @return closure of the table (all tables associated (in-)direct with table)
	 */
	public Set<Table> closure(Set<Table> tablesToIgnore, Map<Table, Integer> distances) {
		Set<Table> closure = new HashSet<Table>();
		List<Pair<Table, Integer>> toCheck = new LinkedList<>();
		Set<Table> checked = new HashSet<Table>(tablesToIgnore);
		toCheck.add(new Pair<Table, Integer>(this, 0));
		while (!toCheck.isEmpty()) {
			Pair<Table, Integer> tableDist = toCheck.remove(0);
			if (!checked.contains(tableDist.a)) {
				closure.add(tableDist.a);
				distances.put(tableDist.a, tableDist.b);
				checked.add(tableDist.a);
				for (Association association: tableDist.a.associations) {
					if (!checked.contains(association.destination)) {
						if (association.getJoinCondition() != null) {
							toCheck.add(new Pair<Table, Integer>(association.destination, tableDist.b + 1));
						}
					}
				}
			}
		}
		
		return closure;
	}

	/**
	 * Gets the closure of the table, ignoring restrictions.
	 * 
	 * @return closure of the table (all tables associated (in-)directly with table)
	 */
	public Set<Table> unrestrictedClosure() {
		Set<Table> closure = new HashSet<Table>();
		List<Table> toCheck = new LinkedList<Table>();
		Set<Table> checked = new HashSet<Table>();
		toCheck.add(this);
		while (!toCheck.isEmpty()) {
			Table table = toCheck.remove(0);
			if (!checked.contains(table)) {
				closure.add(table);
				checked.add(table);
				for (Association association: table.associations) {
					if (!checked.contains(association.destination)) {
						toCheck.add(association.destination);
					}
				}
			}
		}
		
		return closure;
	}
	
	/**
	 * Sets template for XML exports.
	 */
	public void setXmlTemplate(String xmlTemplate) {
		this.xmlTemplate = xmlTemplate;
	}

	/**
	 * Gets template for XML exports.
	 */
	public String getXmlTemplate() {
		return xmlTemplate;
	}

	/**
	 * Gets template for XML exports as DOM.
	 */
	public Document getXmlTemplateAsDocument(Quoting quoting) throws ParserConfigurationException, SAXException, IOException {
		return getXmlTemplateAsDocument(xmlTemplate, quoting);
	}

	/**
	 * Gets default template for XML exports as DOM.
	 */
	public Document getDefaultXmlTemplate(Quoting quoting) throws ParserConfigurationException, SAXException, IOException {
		return getXmlTemplateAsDocument(quoting);
	}

	/**
	 * Gets template for XML exports as DOM.
	 */
	private Document getXmlTemplateAsDocument(String xmlTemplate, Quoting quoting) throws ParserConfigurationException, SAXException, IOException {
		Document template;
		if (xmlTemplate == null) {
			template = createInitialXmlTemplate(quoting);
		} else {
			template = XmlUtil.parse(xmlTemplate);
		}

		removeNonAggregatedAssociationElements(template.getChildNodes().item(0));
		
		// find associations:
		final Set<String> mappedAssociations = new HashSet<String>();
		XmlUtil.visitDocumentNodes(template, new NodeVisitor() {
			@Override
			public void visitAssociationElement(String associationName) {
				mappedAssociations.add(associationName);
			}
			@Override
			public void visitElementEnd(String elementName, boolean isRoot) {
			}
			@Override
			public void visitText(String text) {
			}
			@Override
			public void visitComment(String comment) {
			}
			@Override
			public void visitElementStart(String elementName, boolean isRoot,
					String[] attributeNames, String[] attributeValues) {
			}
		});
		
		// add associations:
		for (Association a: associations) {
			if (a.getAggregationSchema() != AggregationSchema.NONE && !mappedAssociations.contains(a.getName())) {
				Comment comment= template.createComment("associated " + a.destination.getUnqualifiedName() + (Cardinality.MANY_TO_ONE.equals(a.getCardinality()) || Cardinality.ONE_TO_ONE.equals(a.getCardinality())? " row" : " rows"));
				template.getChildNodes().item(0).appendChild(comment);
				Element associationElement = template.createElementNS(XmlUtil.NS_URI, XmlUtil.ASSOCIATION_TAG);
				associationElement.setPrefix(XmlUtil.NS_PREFIX);
				associationElement.appendChild(template.createTextNode(a.getName()));
				template.getChildNodes().item(0).appendChild(associationElement);
			}
		}
		
		return template;
	}

	private void removeNonAggregatedAssociationElements(Node node) {
		NodeList children = node.getChildNodes();
		int i = 0;
		while (i < children.getLength()) {
			if (children.item(i) instanceof Element) {
				Node e = children.item(i);
				if (XmlUtil.NS_URI.equals(e.getNamespaceURI()) && XmlUtil.ASSOCIATION_TAG.equals(e.getLocalName())) {
					boolean f = false;
					for (Association a: associations) {
						if (a.getAggregationSchema() != AggregationSchema.NONE && e.getTextContent() != null) {
							if (a.getName().equals(e.getTextContent().trim())) {
								f = true;
								break;
							}
						}
					}
					if (f) {
						++i;
					} else {
						node.removeChild(e);
					}
				} else {
					removeNonAggregatedAssociationElements(e);
					++i;
				}
			} else {
				++i;
			}
		}
	}

	/**
	 * Creates initial XML mapping template.
	 */
	private Document createInitialXmlTemplate(Quoting quoting) throws ParserConfigurationException {
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setNamespaceAware(true);
		DocumentBuilder builder = factory.newDocumentBuilder();
		Document template = builder.newDocument();
		
		Element root = template.createElement(XmlUtil.asElementName(getUnqualifiedName().toLowerCase(Locale.ENGLISH)));
		root.setAttributeNS("http://www.w3.org/2000/xmlns/",
				"xmlns:" + XmlUtil.NS_PREFIX,
				XmlUtil.NS_URI);
		template.appendChild(root);
		boolean commented = false;
		for (Column column: getColumns()) {
			if (!commented) {
				Comment comment= template.createComment("columns of " + getUnqualifiedName() + " as T");
				root.appendChild(comment);
				commented = true;
			}
			boolean isPK = false;
			for (Column pk: primaryKey.getColumns()) {
				if (pk.name.equals(column.name)) {
					isPK = true;
					break;
				}
			}
			Element columnElement = template.createElement(XmlUtil.asElementName(column.name.toLowerCase(Locale.ENGLISH)));
			String quotedName = quoting != null? quoting.requote(column.name) : column.name;
			if (!isPK) {
				columnElement.setAttribute(XmlUtil.NS_PREFIX + ":if-not-null", XmlUtil.SQL_PREFIX + "T." + quotedName);
			}
			columnElement.setTextContent(XmlUtil.SQL_PREFIX + "T." + quotedName);
			root.appendChild(columnElement);
		}
		
		return template;
	}

	/**
	 * Gets un-mapped schema name of table.
	 * 
	 * @param defaultSchema the default schema to return if table name is unqualified
	 * @return schema name
	 */
	public String getOriginalSchema(String defaultSchema) {
		int i = SqlUtil.indexOfDot(getOriginalName());
		if (i >= 0) {
			return getOriginalName().substring(0, i);
		}
		return defaultSchema;
	}

	/**
	 * Gets mapped schema name of table.
	 * 
	 * @param defaultSchema the default schema to return if table name is unqualified
	 * @return schema name
	 */
	public String getSchema(String defaultSchema) {
		int i = SqlUtil.indexOfDot(name);
		if (i >= 0) {
			return name.substring(0, i);
		}
		return defaultSchema;
	}

	/**
	 * Gets unqualified name of table.
	 * 
	 * @return unqualified name of table
	 */
	public String getUnqualifiedName() {
		int i = SqlUtil.indexOfDot(name);
		if (i >= 0) {
			return name.substring(i + 1);
		}
		return name;
	}

	/**
	 * Sets the original table-name. Differs from name if a source-schema-mapping has been applied to the name.
	 * 
	 * @param originalName the original name
	 */
	public void setOriginalName(String originalName) {
		this.originalName = originalName;
	}

	/**
	 * Gets the original table-name. Differs from name if a source-schema-mapping has been applied to the name.
	 * 
	 * @return the original name
	 */
	public String getOriginalName() {
		return originalName == null? name : originalName;
	}

	/**
	 * Gets unique number of this table to be used as type discriminator in JAILER_ENTITY table.
	 * 
	 * @return unique number
	 */
	public int getOrdinal() {
		return ordinal;
	}

	/**
	 * Gets all non-virtual columns of the table in the order in which they are selected.
	 * 
	 * @return all non-virtual columns of the table in the order in which they are selected
	 */
	public List<Column> getSelectionClause() {
		ArrayList<Column> result = new ArrayList<Column>();
		for (Column column: getColumns()) {
			if (!column.isVirtual()) {
				if (column.getFilter() == null || !Filter.EXCLUDED_VALUE.equals(column.getFilter().getExpression())) {
					result.add(column);
				}
			}
		}
		return result;
	}

	public boolean isExcludedFromDeletion() {
		return excludeFromDeletion == null? defaultExcludeFromDeletion : excludeFromDeletion;
	}

	private static final String NON_VIRTUAL_COLUMNS_PROP_PREFIX = "nonVirtualColumns:";

	public static void clearSessionProperties(Session session) {
		session.removeSessionProperties(Table.class);
	}
	
	/**
	 * Gets non-virtual and non-filtered primary-key columns.
	 * @param session the session
	 * 
	 * @return non-virtual and non-filtered primary-key columns
	 */
	public List<Column> getNonVirtualPKColumns(Session session) {
		if (primaryKey == null) {
			return new ArrayList<Column>();
		}

		String propertyName = NON_VIRTUAL_COLUMNS_PROP_PREFIX + name;
		@SuppressWarnings("unchecked")
		List<Column> nonVirtualColumns = (List<Column>) session.getSessionProperty(Table.class, propertyName);
		if (nonVirtualColumns == null) {
			List<Column> pkColumns = primaryKey.getColumns();
			nonVirtualColumns = new ArrayList<Column>(pkColumns.size());
			for (Column pkColumn: pkColumns) {
				for (Column column: columns) {
					if (column.name.equals(pkColumn.name)) {
						if (!column.isVirtual()) {
							if (column.getFilter() == null || !Filter.EXCLUDED_VALUE.equals(column.getFilter().getExpression())) {
								nonVirtualColumns.add(column);
							}
						}
						break;
					}
				}
			}
			session.setSessionProperty(Table.class, propertyName, nonVirtualColumns);
		}
		return nonVirtualColumns;
	}

	private boolean isArtifical = false;
	
	/**
	 * Sets whether this table is artificial (i.e. does not represent a table in a database).
	 * 
	 * @param isArtifical value to set
	 */
	public void setIsArtifical(boolean isArtifical) {
		this.isArtifical = isArtifical;
	}

	/**
	 * Gets whether this table is artificial (i.e. does not represent a table in a database).
	 */
	public boolean isArtifical() {
		return isArtifical;
	}


	private boolean isDistinct = false;
	
	/**
	 * Sets whether this table is a distinct join over several tables (needed for counting of column values).
	 * 
	 * @param isDistinct value to set
	 */
	public void setIsDistinct(boolean isDistinct) {
		this.isDistinct = isDistinct;
	}

	/**
	 * Gets whether this table is a distinct join over several tables (needed for counting of column values).
	 */
	public boolean isDistinct() {
		return isDistinct;
	}

}

