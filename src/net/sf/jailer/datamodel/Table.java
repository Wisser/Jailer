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
package net.sf.jailer.datamodel;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import net.sf.jailer.xml.NodeVisitor;
import net.sf.jailer.xml.XmlUtil;

import org.w3c.dom.Comment;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

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
    private List<Column> columns;
    
    /**
     * Associations to other tables.
     */
    public final List<Association> associations = new ArrayList<Association>();
    
    /**
     * Use upsert (merge) or insert-statement for entities of this table in export-script.
     */
    public final boolean upsert;
    
    /**
     * Template for XML exports.
     */
    private String xmlTemplate = null;
    
    /**
     * Constructor.
     * 
     * @param name the table-name
     * @param primaryKey the names of the primary-key columns
     * @param upsert use upsert (merge) or insert-statement for entities of this table in export-script
     */
    public Table(String name, PrimaryKey primaryKey,  boolean upsert) {
        this.name = name;
        this.primaryKey = primaryKey;
        this.upsert = upsert;
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
    public List<Column>getColumns() {
    	if (columns == null) {
    		return Collections.emptyList();
    	}
    	return columns;
    }

    /**
     * Compares tables.
     */
    public boolean equals(Object other) {
        if (other instanceof Table) {
            return name.equals(((Table) other).name);
        }
        return false;
    }

    /**
     * The hash-code.
     */
    public int hashCode() {
        return name.hashCode();
    }
    
    /**
     * Stringifies the table.
     */
    public String toString() {
        String str = name + " (" + primaryKey + ")\n";
        List<Association> all = new ArrayList<Association>(associations);
        Collections.sort(all, new Comparator<Association>() {
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

    public int compareTo(Table o) {
        return name.compareTo(o.name);
    }
    
    /**
     * Gets the closure of the table.
     * 
     * @param directed consider associations as directed?
     * 
     * @return closure of the table (all tables associated (in-)direct with table)
     */
    public Set<Table> closure(boolean directed) {
        return closure(new HashSet<Table>(), new HashSet<Table>(), directed);
    }

    /**
     * Gets the closure of the table.
     * 
     * @param directed consider associations as directed?
     * @param tablesToIgnore ignore this tables
     * 
     * @return closure of the table (all tables associated (in-)direct with table)
     */
    public Set<Table> closure(Set<Table> tablesToIgnore, boolean directed) {
        return closure(new HashSet<Table>(), tablesToIgnore, directed);
    }

    /**
     * Gets the closure of the table.
     * 
     * @param tables tables known in closure
     * @param directed consider associations as directed?
     * @param tablesToIgnore ignore this tables
     * 
     * @return closure of the table (all tables associated (in-)direct with table)
     */
    private Set<Table> closure(Set<Table> tables, Set<Table> tablesToIgnore, boolean directed) {
        Set<Table> closure = new HashSet<Table>();
        if (!tables.contains(this) && !tablesToIgnore.contains(this)) {
            closure.add(this);
            tables.add(this);
            for (Association association: associations) {
                if (!tables.contains(association.destination)) {
                    if (association.getJoinCondition() != null || (association.reversalAssociation.getJoinCondition() != null && !directed)) {
                        closure.addAll(association.destination.closure(tables, tablesToIgnore, directed));
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
    public Document getXmlTemplateAsDocument() throws ParserConfigurationException, SAXException, IOException {
    	return getXmlTemplateAsDocument(xmlTemplate);
    }

    /**
     * Gets default template for XML exports as DOM.
     */
    public Document getDefaultXmlTemplate() throws ParserConfigurationException, SAXException, IOException {
    	return getXmlTemplateAsDocument(null);
    }

    /**
     * Gets template for XML exports as DOM.
     */
    private Document getXmlTemplateAsDocument(String xmlTemplate) throws ParserConfigurationException, SAXException, IOException {
    	Document template;
    	if (xmlTemplate == null) {
    		template = createInitialXmlTemplate();
    	} else {
    		template = XmlUtil.parse(xmlTemplate);
    	}

    	removeNonAggregatedAssociationElements((Element) template.getChildNodes().item(0));
    	
    	// find associations:
    	final Set<String> mappedAssociations = new HashSet<String>();
    	XmlUtil.visitDocumentNodes(template, new NodeVisitor() {
			public void visitAssociationElement(String associationName) {
				mappedAssociations.add(associationName);
			}
			public void visitElementEnd(String elementName, boolean isRoot) {
			}
			public void visitText(String text) {
			}
			public void visitComment(String comment) {
			}
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

    private void removeNonAggregatedAssociationElements(Element element) {
    	NodeList children = element.getChildNodes();
    	int i = 0;
    	while (i < children.getLength()) {
    		if (children.item(i) instanceof Element) {
    			Element e = (Element) children.item(i);
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
    					element.removeChild(e);
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
	private Document createInitialXmlTemplate() throws ParserConfigurationException {
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setNamespaceAware(true);
		DocumentBuilder builder = factory.newDocumentBuilder();
		Document template = builder.newDocument();
		
		Element root = template.createElement(XmlUtil.asElementName(getUnqualifiedName().toLowerCase()));
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
			Element columnElement = template.createElement(XmlUtil.asElementName(column.name.toLowerCase()));
			columnElement.setTextContent(XmlUtil.SQL_PREFIX + "T." + column.name);
			root.appendChild(columnElement);
		}
		
		return template;
	}

	/**
	 * Gets schema name of table.
	 * 
	 * @param defaultSchema the default schema to return if table name is unqualified
	 * @return schema name
	 */
	public String getSchema(String defaultSchema) {
		int i = name.indexOf('.');
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
		int i = name.indexOf('.');
		if (i >= 0) {
			return name.substring(i + 1);
		}
		return name;
	}
    
}

