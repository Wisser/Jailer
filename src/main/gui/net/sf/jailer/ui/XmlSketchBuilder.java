/*
 * Copyright 2007 - 2024 Ralf Wisser.
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
package net.sf.jailer.ui;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;

import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.datamodel.AggregationSchema;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Cardinality;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.subsetting.ScriptFormat;
import net.sf.jailer.xml.XmlRowWriter;
import net.sf.jailer.xml.XmlUtil;
import net.sf.jailer.xml.XmlUtil.ObjectNotationTransformerHandler;

/**
 * Builds XML sketches.
 * 
 * @author Ralf Wisser
 */
public class XmlSketchBuilder {

	/**
	 * Builds XML sketches for given table.
	 * 
	 * @param table the table
	 * @return xml sketch for table 
	 */
	public static String buildSketch(Table table, int depth, ScriptFormat scriptFormat, ExecutionContext executionContext) throws Exception {
		if (table == null) {
			return "";
		}
		Document sketch = table.getXmlTemplateAsDocument(null);
		if (sketch.getChildNodes().getLength() > 0 && sketch.getChildNodes().item(0) instanceof Element) {
			insertAssociationSketch(sketch.getChildNodes().item(0), table, sketch, scriptFormat, depth);
		}
		String result = XmlUtil.buildOmitDeclaration(sketch);
		if (scriptFormat != ScriptFormat.XML) {
			OutputStream out = new ByteArrayOutputStream();
			ObjectNotationTransformerHandler th = XmlUtil.createObjectNotationTransformerHandler("", "", new OutputStreamWriter(out, Charset.defaultCharset()), true, scriptFormat, executionContext);
			XmlRowWriter xmlRowWriter = new XmlRowWriter(out, null, null, null, null, scriptFormat, Charset.defaultCharset(), th, executionContext);
			XmlUtil.visitDocumentNodes(XmlUtil.parse(result), xmlRowWriter.new XmlWritingNodeVisitor(null, null, table, null, null) {
				@Override
				protected Object toContent(String text, boolean returnNull) {
					return "...";
				}
				@Override
				public void visitAssociationElement(String associationName, String name) {
					if (name != null) {
						table.associations.stream().filter(a -> associationName.equals(a.getName())).findAny()
								.ifPresent(a -> {
									th.associationSketch(a, associationName, name);
								});
					}
				}
			});
			th.endDocument();
			result = out.toString();
		}
		return result;
	}
	
	private static void insertAssociationSketch(Node node, Table table, Document doc, ScriptFormat scriptFormat, int depth) throws DOMException, ParserConfigurationException, SAXException, IOException {
		NodeList children = node.getChildNodes();
		if (node instanceof Element) {
			Node a0 = ((Element) node).getAttributes().item(0);
			if (a0 != null && a0.getNodeName() != null && a0.getNodeName().startsWith("j:")) {
				((Element) node).removeAttribute(a0.getNodeName());
			}
			Node ch = ((Element) node).getFirstChild();
			if (ch != null && ch.getTextContent().trim().startsWith("SQL:")) {
				((Element) node).removeChild(ch);
				((Element) node).appendChild(doc.createTextNode("..."));
			}
		}
		int i = 0;
		while (i < children.getLength()) {
			if (children.item(i) instanceof Element) {
				Node e = children.item(i);
				if (XmlUtil.NS_URI.equals(e.getNamespaceURI()) && XmlUtil.ASSOCIATION_TAG.equals(e.getLocalName()) && e.getTextContent() != null) {
					Association association = null;
					for (Association a: table.associations) {
						if (a.getAggregationSchema() != AggregationSchema.NONE) {
							if (a.getName().equals(e.getTextContent().trim())) {
								association = a;
								break;
							}
						}
					}
					if (association != null && depth < 5) {
						Node[] ae = insertAssociationSketch(association, doc, scriptFormat, depth + 1);
						if (ae != null) {
							for (Node n: ae) {
								node.insertBefore(doc.importNode(n, true), e);
								++i;
							}
						}
					}
					if (scriptFormat == ScriptFormat.XML) {
						node.removeChild(e);
					} else {
						++i;
					}
				} else {
					insertAssociationSketch(e, table, doc, scriptFormat, depth + 1);
					++i;
				}
			} else {
				++i;
			}
		}
	}

	private static Node[] insertAssociationSketch(Association association, Document doc, ScriptFormat scriptFormat, int depth) throws ParserConfigurationException, SAXException, IOException {
		if (scriptFormat == ScriptFormat.XML) {
			if (association.getAggregationSchema() == AggregationSchema.EXPLICIT_LIST) {
				Element e1 = doc.createElement(association.getAggregationTagName());
				Element e2 = doc.createElement(association.destination.getUnqualifiedName().toLowerCase(Locale.ENGLISH));
				e1.appendChild(e2);
	//			if (association.getCardinality() != Cardinality.MANY_TO_ONE && association.getCardinality() != Cardinality.ONE_TO_ONE) {
					e1.appendChild(doc.createTextNode("..."));
	//			}
				return new Node[] { e1 };
			}
			else if (association.getAggregationSchema() == AggregationSchema.IMPLICIT_LIST) {
				Element e1 = doc.createElement(association.getAggregationTagName());
	//			if (association.getCardinality() != Cardinality.MANY_TO_ONE && association.getCardinality() != Cardinality.ONE_TO_ONE) {
				e1.appendChild(doc.createTextNode("..."));
					return new Node[] { e1 };
	//			}
	//			return new Node[] { e1 };
			} else if (association.getAggregationSchema() == AggregationSchema.FLAT) {
				Document sketch = association.destination.getXmlTemplateAsDocument(null);
				List<Node> nodes = new ArrayList<Node>();
				if (sketch.getChildNodes().getLength() > 0 && sketch.getChildNodes().item(0) instanceof Element) {
					insertAssociationSketch(sketch.getChildNodes().item(0), association.destination, sketch, scriptFormat, depth + 1);
				}
				NodeList children = sketch.getChildNodes().item(0).getChildNodes();
				for (int i = 0; i < children.getLength(); ++i) {
					nodes.add(children.item(i));
				}
	//			if (association.getCardinality() != Cardinality.MANY_TO_ONE && association.getCardinality() != Cardinality.ONE_TO_ONE) {
	//				nodes.add(sketch.createTextNode("..."));
	//			}
				return nodes.toArray(new Node[nodes.size()]);
			}
		}
		return null;
	}

	/**
	 * Builds XML sketches for given table.
	 * 
	 * @param table the table
	 * @return xml sketch for table 
	 */
	public static String buildSketch2(Table table, int depth) {
		StringBuilder sb = new StringBuilder();
		
		if (table != null && depth < 2) {
			List<Association> sortedSourceAssociations = new ArrayList<Association>(table.associations);
			Collections.sort(sortedSourceAssociations, new Comparator<Association>() {
				@Override
				public int compare(Association o1, Association o2) {
					return o1.destination.getName().compareTo(o2.destination.getName());
				}
			});

			if (depth == 0) {
				sb.append("<" + table.getUnqualifiedName().toLowerCase(Locale.ENGLISH) + ">\n");
				sb.append("    -- elements of " + table.getUnqualifiedName() + " --\n");
			}
			String indent = "    ";
			for (Association a: sortedSourceAssociations) {
				if (a.getAggregationSchema() == AggregationSchema.EXPLICIT_LIST) {
					sb.append(indent + "<" + a.getAggregationTagName() + ">\n");
					sb.append(indent + "    <" + a.destination.getUnqualifiedName().toLowerCase(Locale.ENGLISH) + "/>\n");
					if (a.getCardinality() != Cardinality.MANY_TO_ONE && a.getCardinality() != Cardinality.ONE_TO_ONE) {
						sb.append(indent + "    <" + a.destination.getUnqualifiedName().toLowerCase(Locale.ENGLISH) + "/>\n");
						sb.append(indent + "      ...\n");
					}
					sb.append(indent + "</" + a.getAggregationTagName() + ">\n");
				} else if (a.getAggregationSchema() == AggregationSchema.IMPLICIT_LIST) {
					sb.append(indent + "<" + a.getAggregationTagName() + "/>\n");
					if (a.getCardinality() != Cardinality.MANY_TO_ONE && a.getCardinality() != Cardinality.ONE_TO_ONE) {
						sb.append(indent + "<" + a.getAggregationTagName() + "/>\n");
						sb.append(indent + "  ...\n");
					}
				} else if (a.getAggregationSchema() == AggregationSchema.FLAT) {
					sb.append(indent + "-- elements of " + a.destination.getUnqualifiedName() + "(flattened " + a.getName() + ") --\n");
					sb.append(buildSketch2(a.destination, depth + 1));
					if (a.getCardinality() != Cardinality.MANY_TO_ONE && a.getCardinality() != Cardinality.ONE_TO_ONE) {
						sb.append(indent + "-- elements of " + a.destination.getUnqualifiedName() + "(flattened " + a.getName() + ") --\n");
						sb.append(indent + "     ...\n");
					}
				}
			}
			if (depth == 0) {
				sb.append("</" + table.getUnqualifiedName().toLowerCase(Locale.ENGLISH) + ">\n");
			}
		}
		
		return sb.toString();
	}

}
