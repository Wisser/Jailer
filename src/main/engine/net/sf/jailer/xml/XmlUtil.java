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
package net.sf.jailer.xml;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.sax.SAXTransformerFactory;
import javax.xml.transform.sax.TransformerHandler;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Attr;
import org.w3c.dom.Comment;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.subsetting.ScriptFormat;
import net.sf.jailer.util.PrintUtil;
import net.sf.jailer.xml.XmlRowWriter.ObjectFormatTransformer;

/**
 * XML parsing and building.
 * 
 * @author Ralf Wisser
 */
public class XmlUtil {
	
	public static interface ObjectNotationWriter {
		public void writeStartObject() throws IOException;
		public void writeStartArray() throws IOException;
		public void writeEndObject() throws IOException;
		public void writeEndArray() throws IOException;
		public void flush() throws IOException;
		public void writeFieldName(String qName) throws IOException;
		public void writeArrayFieldStart(String qName) throws IOException;
		public void writeNull() throws IOException;
		public void writeBinary(byte[] content) throws IOException;
		public void writeBoolean(Boolean content) throws IOException;
		public void writeNumber(BigInteger content) throws IOException;
		public void writeNumber(BigDecimal content) throws IOException;
		public void writeNumber(Double content) throws IOException;
		public void writeNumber(Float content) throws IOException;
		public void writeNumber(Integer content) throws IOException;
		public void writeNumber(Long content) throws IOException;
		public void writeNumber(Short content) throws IOException;
		public void writeString(String string) throws IOException;
		public void writeComment(String comment) throws IOException;
	}

	public static class ObjectNotationTransformerHandler implements TransformerHandler, ObjectFormatTransformer {
		private final Writer out;
		private final ObjectNotationWriter jGenerator;
		private final ExecutionContext executionContext;
		private final boolean forSketch;
		Stack<Character> states = new Stack<>();
		
		private ObjectNotationTransformerHandler(Writer out, ObjectNotationWriter jGenerator, boolean forSketch, ExecutionContext executionContext) {
			this.out = out;
			this.jGenerator = jGenerator;
			this.forSketch = forSketch;
			this.executionContext = executionContext;
		}

		@Override
		public void setDocumentLocator(Locator locator) {
		}

		@Override
		public void startDocument() throws SAXException {
			try {
				if (forSketch || executionContext.isSingleRoot()) {
					jGenerator.writeStartObject();
					states.push('R');
				} else {
					jGenerator.writeStartArray();
					states.push('M');
				}
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}

		@Override
		public void endDocument() throws SAXException {
			try {
				Character c = states.pop();
				if (c == 'R') {
					jGenerator.writeEndObject();
				} else if (c == 'M') {
					jGenerator.writeEndArray();
				}
				jGenerator.flush();
			} catch (IOException e) {
				try {
					jGenerator.flush();out.close();
				} catch (IOException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
				throw new RuntimeException(e);
			}
		}

		@Override
		public void startPrefixMapping(String prefix, String uri) throws SAXException {
		}

		@Override
		public void endPrefixMapping(String prefix) throws SAXException {
		}

		@Override
		public void startElement(String uri, String localName, String qName, Attributes atts) throws SAXException {
			try {
				if (states.peek() == '=') {
					states.pop();
					states.push('{');
					jGenerator.writeStartObject();
					states.push('=');
					jGenerator.writeFieldName(qName);
				} else if (states.peek() == 'M') {
					jGenerator.writeStartObject();
					states.push('m');
				} else if (states.peek() == '[') {
					jGenerator.writeArrayFieldStart(qName);
					states.push('-');
				} else if (states.peek() == '-') {
					jGenerator.writeStartObject();
					states.push('{');
				} else if (states.peek() == 'R') {
					states.push('r');
				} else {
					states.push('=');
					jGenerator.writeFieldName(qName);
				}
			} catch (IOException e) {
				try {
					jGenerator.flush();out.close();
				} catch (IOException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
				throw new RuntimeException(e);
			}
		}

		@Override
		public void endElement(String uri, String localName, String qName) throws SAXException {
			try {
				Character pop = states.pop();
				if (pop == '=') {
					jGenerator.writeNull();
				}
				if (pop == '{') {
					jGenerator.writeEndObject();
				}
				if (pop == 'm') {
					jGenerator.writeEndObject();
				}
				if (pop == '-') {
					jGenerator.writeEndArray();
				}
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
		
		private Map<String, Object> constants = new HashMap<>();

		@Override
		public void characters(char[] ch, int start, int length) throws SAXException {
			try {
				if (states.peek() == '=') {
					String constant = new String(ch, start, length).trim();
					Object content = constants.get(constant);
					if (content == null) {
						if (constant.isEmpty()) {
							content = null;
						} else if (constant.equalsIgnoreCase("true")) {
							content = true;
						} else if (constant.equalsIgnoreCase("false")) {
							content = false;
						} else {
							try {
								content = Long.parseLong(constant);
							} catch (NumberFormatException e) {
								try {
									content = Double.parseDouble(constant);
								} catch (NumberFormatException e2) {
									try {
										content = new BigDecimal(constant);
									} catch (NumberFormatException e3) {
										content = constant;
									}
								}
							}
						}
						constants.put(constant, content);
					}
					writeContent(content);
					states.pop();
					states.push('?');
				}
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}

		@Override
		public void ignorableWhitespace(char[] ch, int start, int length) throws SAXException {
		}

		@Override
		public void processingInstruction(String target, String data) throws SAXException {
		}

		@Override
		public void skippedEntity(String name) throws SAXException {
		}

		@Override
		public void startDTD(String name, String publicId, String systemId) throws SAXException {
		}

		@Override
		public void endDTD() throws SAXException {
		}

		@Override
		public void startEntity(String name) throws SAXException {
		}

		@Override
		public void endEntity(String name) throws SAXException {
		}

		@Override
		public void startCDATA() throws SAXException {
		}

		@Override
		public void endCDATA() throws SAXException {
		}

		@Override
		public void comment(char[] ch, int start, int length) throws SAXException {
			try {
				String comment = new String(ch, start, length).trim();
				if (!comment.isEmpty()) {
					jGenerator.writeComment(comment);
				}
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}

		@Override
		public void notationDecl(String name, String publicId, String systemId) throws SAXException {
		}

		@Override
		public void unparsedEntityDecl(String name, String publicId, String systemId, String notationName)
				throws SAXException {
		}

		@Override
		public void setResult(Result result) throws IllegalArgumentException {
		}

		@Override
		public void setSystemId(String systemID) {
		}

		@Override
		public String getSystemId() {
			return null;
		}

		@Override
		public Transformer getTransformer() {
			return null;
		}

		@Override
		public void content(Object content) {
			try {
				if (states.peek() == '=') {
					writeContent(content);
					states.pop();
					states.push('?');
				}
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}

		private void writeContent(Object content) throws IOException {
			if (content == null) {
				jGenerator.writeNull();
			} else if (content instanceof byte[]) {
				jGenerator.writeBinary((byte[]) content);
			} else if (content instanceof Boolean) {
				jGenerator.writeBoolean((Boolean) content);
			} else if (content instanceof BigDecimal) {
				jGenerator.writeNumber((BigDecimal) content);
			} else if (content instanceof BigInteger) {
				jGenerator.writeNumber((BigInteger) content);
			} else if (content instanceof Double) {
				jGenerator.writeNumber((Double) content);
			} else if (content instanceof Float) {
				jGenerator.writeNumber((Float) content);
			} else if (content instanceof Integer) {
				jGenerator.writeNumber((Integer) content);
			} else if (content instanceof Long) {
				jGenerator.writeNumber((Long) content);
			} else if (content instanceof Short) {
				jGenerator.writeNumber((Short) content);
			} else {
				jGenerator.writeString(content.toString());
			}
		}

		@Override
		public void startArray() {
			try {
				if (states.peek() == '=') {
					jGenerator.writeStartObject();
					states.push('[');
				} else {
					states.push('[');
				}
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}

		@Override
		public void endArray() {
			try {
				states.pop();
				if (states.peek() == '=') {
					jGenerator.writeEndObject();
					states.pop();
					states.push('?');
				}
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}

		public void associationSketch(Association a, String associationName, String name) {
			try {
				if (states.peek() != '=') {
					jGenerator.writeFieldName(name);
				} else {
					jGenerator.writeStartObject();
					states.pop();
					states.push('?');
					associationSketch(a, associationName, name);
					jGenerator.writeEndObject();
					return;
				}
				boolean isArray = !a.isInsertDestinationBeforeSource();
				
				if (isArray) {
					jGenerator.writeStartArray();
					jGenerator.writeEndArray();
				} else {
					jGenerator.writeStartObject();
					jGenerator.writeEndObject();
				}
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
	}

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
		removeWhitespaces(document.getChildNodes().item(0));
		return document;
	}

	private static void removeWhitespaces(Node node) {
		NodeList children = node.getChildNodes();
		for (int i = 0; i < children.getLength();) {
			Node n = children.item(i);
			if (n instanceof Element) {
				removeWhitespaces(n);
				++i;
			} else if (n instanceof Text && ((Text) n).getTextContent().trim().length() == 0) {
				node.removeChild(n);
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
	 * Identity transformer for building XML strings without XML declaration.
	 */
	private static Transformer transformerWODecl;
	
	/**
	   * Generates a XML string from DOM without XML declaration.
	   * 
	   * @param xmlDocument the DOM 
	   * @return XML string
	   */
	  public static synchronized String buildOmitDeclaration(Document xmlDocument) throws TransformerException {
		if (transformerWODecl == null) {
			TransformerFactory xformFactory = TransformerFactory.newInstance();
			try {
				xformFactory.setAttribute("indent-number", 4);
			} catch (IllegalArgumentException e) {
				// ignore
			}
			transformerWODecl = xformFactory.newTransformer();
			transformerWODecl.setOutputProperty(OutputKeys.INDENT, "yes");
			transformerWODecl.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
		}
		StringWriter out = new StringWriter();
		transformerWODecl.transform(new DOMSource(xmlDocument), new StreamResult(out));
		return out.getBuffer().toString();
	}
	  
	/**
	 * Generates a XML string from DOM.
	 * 
	 * @param xmlDocument the DOM
	 * 
	 * @return XML string
	 */
	public static synchronized String build(Document xmlDocument) throws TransformerException {
		if (transformer == null) {
			TransformerFactory xformFactory = TransformerFactory.newInstance();
			try {
				xformFactory.setAttribute("indent-number", 4);
			} catch (IllegalArgumentException e) {
				// ignore
			}
			transformer = xformFactory.newTransformer();
			transformer.setOutputProperty(OutputKeys.INDENT, "yes");
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
		} else if (node instanceof Comment) {
			visitor.visitComment((((Comment) node).getTextContent()));
		} else if (node instanceof Element) {
			if (NS_URI.equals(node.getNamespaceURI()) && ASSOCIATION_TAG.equals(node.getLocalName()) && node.getTextContent() != null) {
				Attr nameAttr = ((Element) node).getAttributeNode("name");
				String name = nameAttr == null? null : nameAttr.getValue();
				visitor.visitAssociationElement(((Element) node).getTextContent().trim(), name);
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
	
	/**
	 * Creates a {@link TransformerHandler}.
	 * 
	 * @param commentHeader the comment header
	 * @param rootTag the root tag
	 * @param streamResult stream result
	 */
	public static TransformerHandler createTransformerHandler(String commentHeader, String rootTag,
			StreamResult streamResult, Charset charset)
			throws SAXException {
		SAXTransformerFactory tf = (SAXTransformerFactory) TransformerFactory.newInstance();
		try {
			tf.setAttribute("indent-number", 2);
		} catch (Exception e) {
			// ignore, workaround for JDK 1.5 bug, see http://forum.java.sun.com/thread.jspa?threadID=562510
		}
		TransformerHandler transformerHandler;
		try {
			transformerHandler = tf.newTransformerHandler();
		} catch (TransformerConfigurationException e) {
			throw new RuntimeException(e);
		}
		Transformer serializer = transformerHandler.getTransformer();
		serializer.setOutputProperty(OutputKeys.ENCODING, charset.name());
		serializer.setOutputProperty(OutputKeys.METHOD, "xml");
		serializer.setOutputProperty(OutputKeys.INDENT, "yes");
		transformerHandler.setResult(streamResult);
		transformerHandler.startDocument();
		commentHeader = (PrintUtil.LINE_SEPARATOR + commentHeader).replaceAll("\\r?\\n--", PrintUtil.LINE_SEPARATOR + " ");
		transformerHandler.characters("\n".toCharArray(), 0, 1);
		transformerHandler.comment(commentHeader.toCharArray(), 0, commentHeader.toCharArray().length);
		transformerHandler.characters("\n".toCharArray(), 0, 1);
		if (rootTag.length() > 0) {
			transformerHandler.startElement("", "", rootTag, null);
		}
		return transformerHandler;
	}

	public static ObjectNotationTransformerHandler createObjectNotationTransformerHandler(String commentHeader, String rootTag,
			Writer out, boolean forSketch, ScriptFormat scriptFormat, ExecutionContext executionContext) {
		
		ObjectNotationWriter jGenerator;
		try {
			if (scriptFormat == ScriptFormat.JSON) {
				jGenerator = new JSONWriter(out);
			} else if (scriptFormat == ScriptFormat.YAML) {
				// TODO
				jGenerator = new YAMLWriter(out);
			} else {
				throw new RuntimeException("unknown script format: " + scriptFormat);
			}
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		
		// TODO
		// TODO count XML/JSON/YMAML exports (s16-19) 

		ObjectNotationTransformerHandler th = new ObjectNotationTransformerHandler(out, jGenerator, forSketch, executionContext);
		try {
			th.startDocument();
			if (commentHeader != null && !commentHeader.isEmpty()) {
				// TODO
				// TODO nicht bei JSON, noch testen
				if (!(jGenerator instanceof JSONWriter)) {
					jGenerator.writeComment(commentHeader);
				}
			}
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		return th;
	}

}

// TODO
// TODO 2.2.224 H2 old, new is 2.3.?
// TODO check if old H2 is still necessary and how scripts have to be adjusted
// TODO in doubt include H2 2.2.224


