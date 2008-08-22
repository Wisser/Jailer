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
import java.io.StringReader;
import java.io.StringWriter;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Attr;
import org.w3c.dom.Comment;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * XML parsing and building.
 * 
 * @author Ralf Wisser
 */
public class XmlUtil {

	/**
	 * Jailer namespace URI.
	 */
	public static final String NS_URI = "http://jailer.sf.net/";

	/**
	 * Jailer namespace URI.
	 */
	public static final String NS_PREFIX = "j";

	/**
	 * Tag name for embedding associated entities.
	 */
	public static final String ASSOCIATION_TAG = "assoc";

	/**
	 * Prefix for SQL expressions.
	 */
	public static final String SQL_PREFIX = "SQL:";
	
	/**
	 * Parses a XML document.
	 * 
	 * @param xml the XML document
	 * @return DOM
	 */
	public static Document parse(String xml) throws ParserConfigurationException, SAXException, IOException {
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setNamespaceAware(true);
		DocumentBuilder builder = factory.newDocumentBuilder();
		Document document = builder.parse(new InputSource(new StringReader(xml)));
		removeWhitespaces((Element) document.getChildNodes().item(0));
		return document;
	}

	private static void removeWhitespaces(Element element) {
		NodeList children = element.getChildNodes();
		for (int i = 0; i < children.getLength();) {
			Node n = children.item(i);
			if (n instanceof Element) {
				removeWhitespaces((Element) n);
				++i;
			} else if (n instanceof Text && ((Text) n).getTextContent().trim().length() == 0) {
				element.removeChild(n);
			} else {
				++i;
			}
		}
	}
	
	/**
	 * Identity transformer for building XML strings.
	 */
    private static Transformer transformer;

    /**
	 * Generates a XML string from DOM.
	 * 
	 * @param xmlDocument the DOM
	 * @return XML string
	 */
	public static String build(Document xmlDocument) throws TransformerException {
		if (transformer == null) {
			TransformerFactory xformFactory = TransformerFactory.newInstance();
			try {
				xformFactory.setAttribute("indent-number", new Integer(4));
			} catch (IllegalArgumentException e) {
				// ignore
			}
			transformer = xformFactory.newTransformer();
		    transformer.setOutputProperty(OutputKeys.INDENT,"yes");
		}
		StringWriter out = new StringWriter();
        transformer.transform(new DOMSource(xmlDocument), new StreamResult(out));
		return out.getBuffer().toString();
	}

	/**
	 * Removes invalid char from element names.
	 */
	public static String asElementName(String x) {
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < x.length(); ++i) {
			char c = x.charAt(i);
			if (Character.isUpperCase(c) || Character.isLowerCase(c) || Character.isDigit(c) || c == '-' || c == '_') {
				sb.append(c);
			}
		}
		String elementName = sb.toString();
		if (elementName.length() == 0 || (!Character.isUpperCase(elementName.charAt(0)) && !Character.isLowerCase(elementName.charAt(0)))) {
			return "e" + elementName;
		}
		return elementName;
	}

	/**
	 * Visits the nodes of {@link Document}.
	 * 
	 * @param document the document
	 * @param visitor the visitor
	 */
	public static void visitDocumentNodes(Document document, NodeVisitor visitor) {
		if (document.getChildNodes().getLength() > 0) {
			visitNodes(document.getChildNodes().item(0), true, visitor);
		}
	}
	
	/**
	 * Recursively visits nodes.
	 * 
	 * @param node the root
	 * @param visitor the visitor
	 */
	private static void visitNodes(Node node, boolean nodeIsRoot, NodeVisitor visitor) {
		if (node instanceof Text) {
			String text = ((Text) node).getTextContent().trim();
			if (text.length() > 0) {
				visitor.visitText(text);
			}
		} if (node instanceof Comment) {
			visitor.visitComment((((Comment) node).getTextContent()));
		} else if (node instanceof Element) {
			if (NS_URI.equals(node.getNamespaceURI()) && ASSOCIATION_TAG.equals(node.getLocalName()) && ((Element) node).getTextContent() != null) {
				visitor.visitAssociationElement(((Element) node).getTextContent().trim());
			} else {
				Element e = (Element) node;
				NamedNodeMap attr = e.getAttributes();
				String[] aNames = new String[0];
				String[] aValues = new String[0];
				if (attr != null) {
					aNames = new String[attr.getLength()];
					aValues = new String[attr.getLength()];
					for (int i = 0; i < attr.getLength(); ++i) {
						Node a = attr.item(i);
						aNames[i] = ((Attr) a).getName();
						aValues[i] = ((Attr) a).getValue();
					}
				}
				visitor.visitElementStart(e.getTagName(), nodeIsRoot, aNames, aValues);
				NodeList children = e.getChildNodes();
				for (int i = 0; i < children.getLength(); ++i) {
					visitNodes(children.item(i), false, visitor);
				}
				visitor.visitElementEnd(e.getTagName(), nodeIsRoot);
			}
		}
	}

}
