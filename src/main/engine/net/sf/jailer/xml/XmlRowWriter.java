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
package net.sf.jailer.xml;

import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.nio.charset.Charset;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.Date;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.sql.Types;
import java.text.SimpleDateFormat;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.sax.TransformerHandler;
import javax.xml.transform.stream.StreamResult;

import org.xml.sax.SAXException;
import org.xml.sax.helpers.AttributesImpl;

import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.AggregationSchema;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.Base64;
import net.sf.jailer.util.CellContentConverter;
import net.sf.jailer.util.SqlUtil;


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
	final SimpleDateFormat datePattern;
	
	/**
	 * Pattern for time-stamps.
	 */
	final SimpleDateFormat timestampPattern;

	/**
	 * Current level of if-blocks to skip.
	 */
	private int ifLevel = 0;
	
	/**
	 * Type caches per table (String key).
	 */
	private final Map<Table, Map<String, Integer>> typeCachesForStringKey = new HashMap<Table, Map<String,Integer>>();
	
	/**
	 * Constructor.
	 * 
	 * @param out output stream to write the xml into
	 * @param commentHeader comment at top of document
	 * @param rootTag root tag name
	 * @param datePattern pattern for dates
	 * @param timestampPattern pattern for time-stamps
	 */
	public XmlRowWriter(OutputStream out, String commentHeader, String rootTag, String datePattern, String timestampPattern, Charset charset) throws SAXException, TransformerConfigurationException {
		this.rootTag = rootTag;
		this.datePattern = new SimpleDateFormat(datePattern, Locale.ENGLISH);
		this.timestampPattern = new SimpleDateFormat(timestampPattern, Locale.ENGLISH);
		StreamResult streamResult = new StreamResult(new OutputStreamWriter(out, charset));
		transformerHandler = XmlUtil.createTransformerHandler(commentHeader, rootTag, streamResult, charset);
	}

	/**
	 * Closes the writer.
	 */
	public void close() throws SAXException {
		if (rootTag.length() > 0) {
			transformerHandler.endElement("", "", rootTag);
		}
		transformerHandler.endDocument();
	}

	/**
	 * Writes start element for a list of rows.
	 * 
	 * @param association association describing the list
	 */
	public void startList(Association association) throws SAXException {
		if (association != null && association.getAggregationSchema() == AggregationSchema.EXPLICIT_LIST) {
			if (ifLevel == 0) {
				transformerHandler.startElement(null, null, association.getAggregationTagName(), null);
			}
		}
	}

	/**
	 * Writes end element for a list of rows.
	 * 
	 * @param association association describing the list
	 */
	public void endList(Association association) throws SAXException {
		if (association != null && association.getAggregationSchema() == AggregationSchema.EXPLICIT_LIST) {
			if (ifLevel == 0) {
				transformerHandler.endElement(null, null, association.getAggregationTagName());
			}
		}
	}
	
	/**
	 * Visits nodes of mapping templates and writes data as XML.
	 */
	public abstract class XmlWritingNodeVisitor implements NodeVisitor {
		
		/**
		 * To read rows from.
		 */
		private final ResultSet resultSet;
		
		/**
		 * Meta data.
		 */
		private final ResultSetMetaData resultSetMetaData;
		
		/**
		 * The table from which the data comes.
		 */
		private final Table table;
		
		/**
		 * The association which is currently resolved.
		 */
		private final Association association;
		
		/**
		 * The DB session.
		 */
		private final Session session;
		
		/**
		 * Next number of column to write out.
		 */
		private int nr = 0;
		
		/**
		 * {@link CellContentConverter}.
		 */
		protected final CellContentConverter cellContentConverter;
		
		/**
		 * Constructor.
		 * 
		 * @param resultSet to read rows from
		 */
		public XmlWritingNodeVisitor(ResultSet resultSet, ResultSetMetaData resultSetMetaData, Table table, Association association, Session session) {
			this.resultSet = resultSet;
			this.resultSetMetaData = resultSetMetaData;
			this.table = table;
			this.association = association;
			this.session = session;
			this.cellContentConverter =  new CellContentConverter(resultSetMetaData, session, session.dbms);
		}

		/**
		 * Gets text to write out. If it starts with "SQL:", write out next column value.
		 * 
		 * @param text the text
		 * @param returnNull if <code>true</code>, return null instead of empty string if sql-result is null
		 * @return the xml to write out
		 */
		private String toXml(String text, boolean returnNull) {
			if (text != null && text.startsWith(XmlUtil.SQL_PREFIX)) {
				String columnName = "C" + nr++;
				int type;
				try {
					Map<String, Integer> typeCache = typeCachesForStringKey.get(table);
					if (typeCache == null) {
						typeCache = new HashMap<String, Integer>();
						typeCachesForStringKey.put(table, typeCache);
					}
					type = SqlUtil.getColumnType(resultSet, resultSetMetaData, columnName, typeCache);
					if ((type == Types.BLOB || type == Types.CLOB|| type == Types.NCLOB) && !DBMS.SQLITE.equals(session.dbms)) {
						Object object = resultSet.getObject(columnName);
						if (returnNull && (object == null || resultSet.wasNull())) {
							return null;
						}
						if (object instanceof Blob) {
							Blob blob = (Blob) object;
							byte[] blobValue = blob.getBytes(1, (int) blob.length());
							return Base64.encodeBytes(blobValue);
						}

						if (object instanceof Clob) {
							Clob clobValue = (Clob) object;
							int length = (int) clobValue.length();
							if (length > 0) {
								return clobValue.getSubString(1, length);
							}
							return "";
						}
					} else {
						Object o = cellContentConverter.getObject(resultSet, columnName);
						if (returnNull && (o == null || resultSet.wasNull())) {
							return null;
						}
						if (o != null) {
							String value;
							
							if (o instanceof Timestamp) {
								value = timestampPattern.format((Timestamp) o);
							} else if (o instanceof Date) {
								value = datePattern.format((Date) o);
							} else {
								value = o.toString();
							}
							return value;
						}
					}
					return "";
				} catch (SQLException e) {
					throw new RuntimeException(e);
				}
			}
			return text;
		}
		
		@Override
		public void visitComment(String comment) {
			try {
				if (ifLevel == 0) {
					transformerHandler.comment(comment.toCharArray(), 0, comment.length());
				}
			} catch (SAXException e) {
				throw new RuntimeException(e);
			}
		}
		
		@Override
		public void visitElementEnd(String elementName, boolean isRoot) {
			if (ifLevel > 0) {
				--ifLevel;
			} else {
				try {
					if (!isRoot || association == null || association.getAggregationSchema() != AggregationSchema.FLAT) {
						String tagName = isRoot && association != null && association.getAggregationSchema() != AggregationSchema.EXPLICIT_LIST? association.getAggregationTagName() : elementName;
						transformerHandler.endElement("", "", tagName);
					}
				} catch (SAXException e) {
					throw new RuntimeException(e);
				}
			}
		}
		
		String jailerNamespaceDeclaration = "xmlns:" + XmlUtil.NS_PREFIX;
		
		@Override
		public void visitElementStart(String elementName, boolean isRoot, String[] aNames, String[] aValues) {
			if (ifLevel > 0) {
				++ifLevel;
			}
			try {
				AttributesImpl attr = null;
				boolean cond = true;
				if (aNames.length > 0) {
					attr = new AttributesImpl();
					for (int i = 0; i < aNames.length; ++i) {
						if (aNames[i].equals(XmlUtil.NS_PREFIX + ":if-not-null")) {
							if (toXml(aValues[i], true) == null) {
								if (ifLevel == 0) {
									cond = false;
								}
							}
						} else if (aNames[i].equals(XmlUtil.NS_PREFIX + ":if-null")) {
							if (toXml(aValues[i], true) != null) {
								if (ifLevel == 0) {
									cond = false;
								}
							}
						} else if (!aNames[i].equals(jailerNamespaceDeclaration)) {
							attr.addAttribute("", "", aNames[i], "CDATA", toXml(aValues[i], false));
						}
					}
				}
				if (ifLevel == 0) {
					if (!cond) {
						++ifLevel;
					} else {
						if (!isRoot || association == null || association.getAggregationSchema() != AggregationSchema.FLAT) {
							String tagName = isRoot && association != null && association.getAggregationSchema() != AggregationSchema.EXPLICIT_LIST? association.getAggregationTagName() : elementName;
							transformerHandler.startElement("", "", tagName, attr);
						}
					}
				}
			} catch (SAXException e) {
				throw new RuntimeException(e);
			}
		}
		
		@Override
		public void visitText(String text) {
			text = toXml(text, false);
			try {
				if (ifLevel == 0) {
					transformerHandler.characters(text.toCharArray(), 0, text.length());
				}
			} catch (SAXException e) {
				throw new RuntimeException(e);
			}
		}
	}

}
