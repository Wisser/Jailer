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

import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.nio.charset.Charset;
import java.sql.Date;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.sql.Types;
import java.text.SimpleDateFormat;
import java.util.HashMap;
import java.util.Map;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.sax.SAXTransformerFactory;
import javax.xml.transform.sax.TransformerHandler;
import javax.xml.transform.stream.StreamResult;

import net.sf.jailer.datamodel.AggregationSchema;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.SqlUtil;

import org.xml.sax.SAXException;


/**
 * Writes rows into XML file.
 * 
 * @author Ralf Wisser
 */
public class XmlRowWriter {

	/**
	 * SAX transformer.
	 */
	private final TransformerHandler transformerHandler;
	
	/**
	 * Root tag name.
	 */
	private final String rootTag;
	
	/**
	 * Pattern for dates.
	 */
	private final SimpleDateFormat datePattern;
	
	/**
	 * Pattern for time-stamps.
	 */
	private final SimpleDateFormat timestampPattern;

	/**
	 * Names of columns per table.
	 */
	private final Map<Table, String[]> columnNames = new HashMap<Table, String[]>();

	/**
	 * Type caches per table.
	 */
	private final Map<Table, Map<Integer, Integer>> typeCaches = new HashMap<Table, Map<Integer,Integer>>();
	
	/**
	 * Constructor.
	 * 
	 * @param out output stream to write the xml into
	 * @param commentHeader comment at top of document
	 * @param rootTag root tag name
	 * @param datePattern pattern for dates
	 * @param timestampPattern pattern for time-stamps
	 */
	public XmlRowWriter(OutputStream out, String commentHeader, String rootTag, String datePattern, String timestampPattern) throws SAXException, TransformerConfigurationException {
		this.rootTag = rootTag;
		this.datePattern = new SimpleDateFormat(datePattern);
		this.timestampPattern = new SimpleDateFormat(timestampPattern);
		StreamResult streamResult = new StreamResult(new OutputStreamWriter(out, Charset.defaultCharset()));
        SAXTransformerFactory tf = (SAXTransformerFactory) TransformerFactory.newInstance();
        try {
        	tf.setAttribute("indent-number", new Integer(2));
        } catch (Exception e) {
        	// ignore, workaround for JDK 1.5 bug, see http://forum.java.sun.com/thread.jspa?threadID=562510
        }
        transformerHandler = tf.newTransformerHandler();
        Transformer serializer = transformerHandler.getTransformer();
        serializer.setOutputProperty(OutputKeys.ENCODING, Charset.defaultCharset().name());
        serializer.setOutputProperty(OutputKeys.METHOD, "xml");
        serializer.setOutputProperty(OutputKeys.INDENT, "yes");
        transformerHandler.setResult(streamResult);
        transformerHandler.startDocument();
        commentHeader = ("\n" + commentHeader).replaceAll("\\n--", "\n ");
        transformerHandler.comment(commentHeader.toCharArray(), 0, commentHeader.toCharArray().length);
        transformerHandler.startElement("", "", rootTag, null);
	}

	/**
	 * Closes the writer.
	 */
	public void close() throws SAXException {
        transformerHandler.endElement("", "", rootTag);
        transformerHandler.endDocument();
	}

	/**
	 * Writes start element for a row.
	 * 
	 * @param resultSet holding to row
	 * @param table row's table
	 * @param association association to parent row or <code>null</code>
	 */
	public void startRow(ResultSet resultSet, Table table, Association association) throws SAXException, SQLException {
		if (association == null) {
			transformerHandler.startElement(null, null, asElementName(table.getName()).toLowerCase(), null);
		} else {
			if (association.getAggregationSchema() == AggregationSchema.IMPLICIT_LIST) {
				transformerHandler.startElement(null, null, association.getAggregationTagName(), null);
			} else if (association.getAggregationSchema() == AggregationSchema.EXPLICIT_LIST) {
				transformerHandler.startElement(null, null, asElementName(table.getName()).toLowerCase(), null);
			}
		}
		String[] cNames;
		if (!columnNames.containsKey(table)) {
			cNames = new String[resultSet.getMetaData().getColumnCount()];
			for (int i = 0; i < cNames.length; ++i) {
				cNames[i] = resultSet.getMetaData().getColumnName(i + 1);
			}
			columnNames.put(table, cNames);
		} else {
			cNames = columnNames.get(table);
		}
		Map<Integer, Integer> typeCache = typeCaches.get(table);
		if (typeCache == null) {
			typeCache = new HashMap<Integer, Integer>();
			typeCaches.put(table, typeCache);
		}
		for (int i = 0; i < cNames.length; ++i) {
			int type = SqlUtil.getColumnType(resultSet, i + 1, typeCache);
			if (type == Types.BLOB || type == Types.CLOB) {
				// (C|B)LOB is not yet supported
				continue;
			}
			Object o = SqlUtil.getObject(resultSet, i + 1, typeCache);
			if (o != null) {
				String value;
				
				if (o instanceof Timestamp) {
					value = timestampPattern.format((Timestamp) o);
				} else if (o instanceof Date) {
					value = timestampPattern.format((Date) o);
				} else {
					value = o.toString();
				}
				
				transformerHandler.startElement(null, null, asElementName(cNames[i]).toLowerCase(), null);
				transformerHandler.characters(value.toCharArray(), 0, value.length());
				transformerHandler.endElement(null, null, asElementName(cNames[i]).toLowerCase());
			}
		}
	}

	/**
	 * Writes end element for a row.
	 * 
	 * @param table row's table
	 * @param association association to parent row or <code>null</code>
	 */
	public void endRow(Table table, Association association) throws SAXException {
		if (association == null) {
			transformerHandler.endElement(null, null, asElementName(table.getName().toLowerCase()));
		} else {
			if (association.getAggregationSchema() == AggregationSchema.IMPLICIT_LIST) {
				transformerHandler.endElement(null, null, association.getAggregationTagName());
			} else if (association.getAggregationSchema() == AggregationSchema.EXPLICIT_LIST) {
				transformerHandler.endElement(null, null, asElementName(table.getName().toLowerCase()));
			}
		}
	}

	/**
	 * Writes start element for a list of rows.
	 * 
	 * @param association association describing the list
	 */
	public void startList(Association association) throws SAXException {
		if (association != null && association.getAggregationSchema() == AggregationSchema.EXPLICIT_LIST) {
			transformerHandler.startElement(null, null, association.getAggregationTagName(), null);
		}
	}

	/**
	 * Writes end element for a list of rows.
	 * 
	 * @param association association describing the list
	 */
	public void endList(Association association) throws SAXException {
		if (association != null && association.getAggregationSchema() == AggregationSchema.EXPLICIT_LIST) {
			transformerHandler.endElement(null, null, association.getAggregationTagName());
		}
	}

	/**
	 * Removes invalid char from element names.
	 */
	private String asElementName(String x) {
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < x.length(); ++i) {
			char c = x.charAt(i);
			if (Character.isUpperCase(c) || Character.isLowerCase(c) || Character.isDigit(c) || c == '-' || c == '_') {
				sb.append(c);
			}
		}
		return sb.toString();
	}
	
//        hd.startElement("", "", "DEPARTMENT", null);
//
//        String curTitle = "Somet<>üöähing inside a tag";
//        hd.characters(curTitle.toCharArray(), 0, curTitle.length());
//
//        hd.endElement("", "", "DEPARTMENT");
//
//        hd.endElement("", "", "MyTag");
//        hd.endDocument();

}
