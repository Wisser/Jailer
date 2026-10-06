/*
 * Copyright 2007 - 2026 Ralf Wisser.
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
package net.sf.jailer.ui.databrowser.lob;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.helpers.DefaultHandler;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonFactoryBuilder;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.core.json.JsonReadFeature;

/**
 * Formats (pretty-prints) JSON and XML content for the {@link LobViewerPanel}.
 *
 * @author Ralf Wisser
 */
public class LobFormatter {

	private LobFormatter() {
	}

	/**
	 * Formats JSON. The tokens are copied one by one, so the order of the keys,
	 * duplicate keys and the notation of numbers are kept. Comments are accepted
	 * but dropped.
	 *
	 * @param text the JSON text
	 * @return the indented JSON text
	 * @throws IOException if the text is not well-formed JSON
	 */
	public static String formatJson(String text) throws IOException {
		JsonFactory factory = new JsonFactoryBuilder()
				.enable(JsonReadFeature.ALLOW_JAVA_COMMENTS)
				.build();
		StringWriter out = new StringWriter();
		try (JsonParser parser = factory.createParser(text);
				JsonGenerator generator = factory.createGenerator(out)) {
			generator.useDefaultPrettyPrinter();
			JsonToken token;
			while ((token = parser.nextToken()) != null) {
				if (token == JsonToken.VALUE_NUMBER_INT || token == JsonToken.VALUE_NUMBER_FLOAT) {
					// the number as written, e.g. "1e10" stays "1e10"
					generator.writeNumber(parser.getText());
				} else {
					generator.copyCurrentEvent(parser);
				}
			}
		}
		return out.toString();
	}

	/**
	 * Formats XML. The content comes from the database, so the parser accepts
	 * neither a DOCTYPE nor external entities.
	 *
	 * @param text the XML text
	 * @return the indented XML text
	 * @throws Exception if the text is not well-formed XML
	 */
	public static String formatXml(String text) throws Exception {
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setNamespaceAware(true);
		dbf.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);
		dbf.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
		dbf.setFeature("http://xml.org/sax/features/external-general-entities", false);
		dbf.setFeature("http://xml.org/sax/features/external-parameter-entities", false);
		dbf.setXIncludeAware(false);
		dbf.setExpandEntityReferences(false);
		DocumentBuilder builder = dbf.newDocumentBuilder();
		// no output on System.err, the exception says it all
		builder.setErrorHandler(new DefaultHandler());
		Document document = builder.parse(new InputSource(new StringReader(text)));

		// whitespace between the elements would end up as blank lines
		removeWhitespaceNodes(document);

		TransformerFactory tf = TransformerFactory.newInstance();
		try {
			tf.setAttribute("indent-number", 4);
		} catch (IllegalArgumentException e) {
			// ignore
		}
		Transformer transformer = tf.newTransformer();
		transformer.setOutputProperty(OutputKeys.INDENT, "yes");
		// the original declaration is put in front again instead, the generated one would
		// differ from it and lacks the line break after it
		transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
		StringWriter out = new StringWriter();
		transformer.transform(new DOMSource(document), new StreamResult(out));
		String result = out.toString().trim();
		String trimmedText = text.trim();
		int declarationEnd = trimmedText.startsWith("<?xml")? trimmedText.indexOf("?>") : -1;
		if (declarationEnd > 0) {
			result = trimmedText.substring(0, declarationEnd + 2) + "\n" + result;
		}
		return result;
	}

	private static void removeWhitespaceNodes(Node node) {
		NodeList children = node.getChildNodes();
		for (int i = children.getLength() - 1; i >= 0; --i) {
			Node child = children.item(i);
			if (child.getNodeType() == Node.TEXT_NODE && child.getNodeValue().trim().isEmpty()) {
				node.removeChild(child);
			} else {
				removeWhitespaceNodes(child);
			}
		}
	}

}
