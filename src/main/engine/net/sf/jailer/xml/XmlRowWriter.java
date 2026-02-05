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
import java.util.Stack;

import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.sax.TransformerHandler;
import javax.xml.transform.stream.StreamResult;

import org.xml.sax.SAXException;
import org.xml.sax.helpers.AttributesImpl;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.AggregationSchema;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.subsetting.ScriptFormat;
import net.sf.jailer.util.Base64;
import net.sf.jailer.util.CellContentConverter;
import net.sf.jailer.util.SqlUtil;


/**
 * Writes rows into XML/JSON/YAML file.
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
	
	protected boolean forSketch = false;
	
	/**
	 * Execution context.
	 */
	private final ExecutionContext executionContext;
	
	private final ScriptFormat scriptFormat;
	
	
	/**
	 * Constructor.
	 * 
	 * @param out output stream to write the xml into
	 * @param commentHeader comment at top of document
	 * @param rootTag root tag name
	 * @param datePattern pattern for dates
	 * @param timestampPattern pattern for time-stamps
	 * @param scriptFormat 
	 * @param executionContext 
	 */
	public XmlRowWriter(OutputStream out, String commentHeader, String rootTag, String datePattern, String timestampPattern, ScriptFormat scriptFormat, Charset charset, ExecutionContext executionContext) throws SAXException, TransformerConfigurationException {
		this.rootTag = rootTag;
		this.executionContext = executionContext;
		this.datePattern = datePattern == null? new SimpleDateFormat() : new SimpleDateFormat(datePattern, Locale.ENGLISH);
		this.timestampPattern = timestampPattern == null? new SimpleDateFormat() : new SimpleDateFormat(timestampPattern, Locale.ENGLISH);
		this.scriptFormat = scriptFormat;
		StreamResult streamResult = new StreamResult(new OutputStreamWriter(out, charset));
		transformerHandler = scriptFormat != ScriptFormat.XML
				? XmlUtil.createObjectNotationTransformerHandler(commentHeader, executionContext.isSingleRoot()? "" : rootTag, streamResult.getWriter(), false, scriptFormat, this.datePattern, this.timestampPattern, executionContext)
				: XmlUtil.createTransformerHandler(commentHeader, executionContext.isSingleRoot()? "" : rootTag, streamResult, charset, executionContext);
	}
	
	/**
	 * Constructor.
	 * 
	 * @param out output stream to write the xml into
	 * @param commentHeader comment at top of document
	 * @param rootTag root tag name
	 * @param datePattern patt;ern for dates
	 * @param timestampPattern pattern for time-stamps
	 * @param scriptFormat 
	 * @param executionContext 
	 */
	public XmlRowWriter(OutputStream out, String commentHeader, String rootTag, String datePattern, String timestampPattern, ScriptFormat scriptFormat, Charset charset, TransformerHandler transformerHandler, ExecutionContext executionContext) throws SAXException, TransformerConfigurationException {
		this.rootTag = rootTag;
		this.executionContext = executionContext;
		this.datePattern = datePattern == null? new SimpleDateFormat() : new SimpleDateFormat(datePattern, Locale.ENGLISH);
		this.timestampPattern = timestampPattern == null? new SimpleDateFormat() : new SimpleDateFormat(timestampPattern, Locale.ENGLISH);
		forSketch = true;
		this.scriptFormat = scriptFormat;
		this.transformerHandler = transformerHandler;
	}

	/**
	 * Closes the writer.
	 */
	public void close() throws SAXException {
		if (!executionContext.isSingleRoot() && !(transformerHandler instanceof ObjectFormatTransformer)) {
			transformerHandler.endElement("", "", rootTag);
		}
		transformerHandler.endDocument();
	}
	
	private Map<Association, Stack<String>> associationTempAggregationTagName = new HashMap<>();
	
	/**
	 * Writes start element for a list of rows.
	 * 
	 * @param association association describing the list
	 * @param name 
	 */
	public void startList(Association association, String name) throws SAXException {
		if (association != null) {
			Stack<String> stack = associationTempAggregationTagName.get(association);
			if (stack == null) {
				stack = new Stack<>();
				associationTempAggregationTagName.put(association, stack);
			}
			stack.push(name);
		}
		if (association != null && getAggregationSchema(association) == AggregationSchema.EXPLICIT_LIST) {
			if (ifLevel == 0) {
				if (transformerHandler instanceof ObjectFormatTransformer) {
					((ObjectFormatTransformer) transformerHandler).startArray();
				}
				transformerHandler.startElement(null, null, name != null? name : association.getAggregationTagName(), null);
			}
		}
	}

	/**
	 * Writes end element for a list of rows.
	 * 
	 * @param association association describing the list
	 */
	public void endList(Association association) throws SAXException {
		if (association != null && getAggregationSchema(association) == AggregationSchema.EXPLICIT_LIST) {
			if (ifLevel == 0) {
				transformerHandler.endElement(null, null, association.getAggregationTagName());
				if (transformerHandler instanceof ObjectFormatTransformer) {
					((ObjectFormatTransformer) transformerHandler).endArray();
				}
			}
		}
		if (association != null) {
			Stack<String> stack = associationTempAggregationTagName.get(association);
			if (stack != null && !stack.isEmpty()) {
				stack.pop();
			}
		}
	}
	
	private AggregationSchema getAggregationSchema(Association association) {
		if (transformerHandler instanceof ObjectFormatTransformer) {
			return association.getAggregationSchema() == null || association.getAggregationSchema() == AggregationSchema.NONE?
				AggregationSchema.NONE :
					(!association.isInsertDestinationBeforeSource()? AggregationSchema.EXPLICIT_LIST : AggregationSchema.IMPLICIT_LIST);
		} else {
			return association.getAggregationSchema();
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
		
		private String lastElementName;

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
			this.cellContentConverter = session == null? null : new CellContentConverter(resultSetMetaData, session, session.dbms);
		}

		/**
		 * Gets text to write out. If it starts with "SQL:", write out next column value.
		 * 
		 * @param text the text
		 * @param returnNull if <code>true</code>, return null instead of empty string if sql-result is null
		 * @return the xml to write out
		 */
		protected Object toContent(String text, boolean returnNull) {
			if (text != null && text.startsWith(XmlUtil.SQL_PREFIX)) {
				String columnName = "C" + nr++;
				int type;
				try {
					Map<String, Integer> typeCache = typeCachesForStringKey.get(table);
					if (typeCache == null) {
						typeCache = new HashMap<String, Integer>();
						typeCachesForStringKey.put(table, typeCache);
					}
					try {
						type = SqlUtil.getColumnType(resultSet, resultSetMetaData, columnName, typeCache);
						if ((type == Types.BLOB || type == Types.CLOB || type == Types.NCLOB)
								&& !DBMS.SQLITE.equals(session.dbms)) {
							Object object = resultSet.getObject(columnName);
							if (returnNull && (object == null || resultSet.wasNull())) {
								return null;
							}
							if (object instanceof Blob) {
								Blob blob = (Blob) object;
								byte[] blobValue = blob.getBytes(1, (int) blob.length());
								if (scriptFormat != ScriptFormat.XML) {
									return blobValue;
								}
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
								Object value;

								if (o instanceof Timestamp) {
									value = timestampPattern.format((Timestamp) o);
								} else if (o instanceof Date) {
									value = datePattern.format((Date) o);
								} else {
									value = o;
								}
								return value;
							}
						}
						return null;
					} catch (OutOfMemoryError e) {
						throw new OutOfMemoryError("Out of Memory. \nNot enough memory to read field "
								+ (table == null ? "" : ("\"" + table.getName() + "\".")) + " \"" + lastElementName
								+ "\"");
					}
				} catch (SQLException e) {
					throw new RuntimeException(e);
				}
			}
			return text;
		}
		
		@Override
		public void visitComment(String comment) {
			try {
				if (ifLevel == 0 && forSketch) {
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
					if (!isRoot || association == null || getAggregationSchema(association) != AggregationSchema.FLAT) {
						String tagName = isRoot && association != null && getAggregationSchema(association) != AggregationSchema.EXPLICIT_LIST? association.getAggregationTagName() : elementName;
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
			lastElementName = elementName;
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
							if (toContent(aValues[i], true) == null) {
								if (ifLevel == 0) {
									cond = false;
								}
							}
						} else if (aNames[i].equals(XmlUtil.NS_PREFIX + ":if-null")) {
							if (toContent(aValues[i], true) != null) {
								if (ifLevel == 0) {
									cond = false;
								}
							}
						} else if (!aNames[i].equals(jailerNamespaceDeclaration)) {
							Object content = toContent(aValues[i], false);
							attr.addAttribute("", "", aNames[i], "CDATA", content == null? "" : content.toString());
						}
					}
				}
				if (ifLevel == 0) {
					if (!cond) {
						++ifLevel;
					} else {
						if (!isRoot || association == null || getAggregationSchema(association) != AggregationSchema.FLAT) {
							String tagName;
							if (isRoot && association != null && getAggregationSchema(association) != AggregationSchema.EXPLICIT_LIST) {
								tagName = null;
								Stack<String> stack = associationTempAggregationTagName.get(association);
								if (stack != null && !stack.isEmpty()) {
									tagName = stack.peek();
								}
								if (tagName == null) {
									tagName = association.getAggregationTagName();
								}
							} else {
								tagName = elementName;
							}
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
			Object value = toContent(text, false);
			try {
				if (ifLevel == 0) {
					if (transformerHandler instanceof ObjectFormatTransformer) {
						((ObjectFormatTransformer) transformerHandler).content(value);
					} else {
						text = value == null? "" : value.toString();
						transformerHandler.characters(text.toCharArray(), 0, text.length());
					}
				}
			} catch (SAXException e) {
				throw new RuntimeException(e);
			}
		}
	}
	
	public interface ObjectFormatTransformer {
		void content(Object content);
		void startArray();
		void endArray();
	}

}
