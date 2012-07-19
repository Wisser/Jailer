/*
 * Copyright 2007 - 2012 the original author or authors.
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
package net.sf.jailer.dbunit;

import java.sql.Blob;
import java.sql.Clob;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
import java.util.HashMap;
import java.util.Map;

import javax.xml.transform.sax.TransformerHandler;

import net.sf.jailer.CommandLineParser;
import net.sf.jailer.Configuration;
import net.sf.jailer.database.SQLDialect;
import net.sf.jailer.database.Session.ResultSetReader;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.Base64;

import org.xml.sax.SAXException;
import org.xml.sax.helpers.AttributesImpl;

/**
 * Reads a JDBC result set and writes the read rows into a 
 * DbUnit flat XML dataset document.
 * 
 * @see http://www.dbunit.org/
 * @author Ralf Wisser
 */
public class FlatXMLTransformer implements ResultSetReader {

	/**
	 * Name of XML row element, or <code>null</code> if the table name can not
	 * be converted into a valid XML tag identifier.
	 */
	private final String rowElementName;

	/**
	 * To write the XML into.
	 */
	private final TransformerHandler transformerHandler;

	/**
	 * Number of columns.
	 */
	private int columnCount;

	/**
	 * Labels of columns.
	 */
	private String[] columnLabel = null;

	/**
	 * Maps clear text SQL-types to {@link java.sql.Types}.
	 */
	private Map<Integer, Integer> typeCache = new HashMap<Integer, Integer>();

	/**
	 * Constructor.
	 * 
	 * @param table
	 *            the table to read from
	 * @param transformerHandler
	 *            to write the XML into
	 * @param metaData
	 *            database meta data
	 */
	public FlatXMLTransformer(Table table, TransformerHandler transformerHandler, DatabaseMetaData metaData) throws SQLException {
		this.transformerHandler = transformerHandler;
		this.rowElementName = qualifiedTableName(table);
	}
	
	/**
     * Gets qualified table name.
     * 
     * @param t the table
     * @return qualified name of t
     */
    private String qualifiedTableName(Table t) {
    	String schema = t.getOriginalSchema("");
    	String mappedSchema = CommandLineParser.getInstance().getSchemaMapping().get(schema);
    	if (mappedSchema != null) {
    		schema = mappedSchema;
    	}
    	if (schema.length() == 0) {
    		return unquote(t.getUnqualifiedName());
    	}
		return unquote(schema) + "." + unquote(t.getUnqualifiedName());
	}

    /**
     * Removes quotes from table name.
     * 
     * @param name the table name
     * @return table name without quotes
     */
	private String unquote(String name) {
		if (!name.isEmpty()) {
			char fc = name.charAt(0);
			if (!Character.isLetterOrDigit(fc) && fc != '_') {
				String fcStr = Character.toString(fc);
				if (name.startsWith(fcStr) && name.endsWith(fcStr)) {
					name = name.substring(1, name.length() - 1);
				}
			}
		}
		return name;
	}

	/**
	 * Reads result-set and writes into export-script.
	 */
	public void readCurrentRow(ResultSet resultSet) throws SQLException {
		if (columnLabel == null) {
			columnCount = resultSet.getMetaData().getColumnCount();
			columnLabel = new String[columnCount + 1];
			for (int i = 1; i <= columnCount; ++i) {
				String mdColumnLabel = resultSet.getMetaData()
						.getColumnLabel(i);
				columnLabel[i] = mdColumnLabel;
			}
		}
		try {
			AttributesImpl attr = new AttributesImpl();
			for (int i = 1; i <= columnCount; ++i) {
				if (columnLabel[i] == null) {
					continue;
				}
				String value = getValue(resultSet, i, typeCache);
				if (value != null) {
					attr.addAttribute("", "", columnLabel[i], "CDATA", value);
				}
			}
			synchronized (transformerHandler) {
				transformerHandler.startElement("", "", rowElementName, attr);
				transformerHandler.endElement("", "", rowElementName);
			}
		} catch (SAXException e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * Gets value from current row a result-set.
	 * 
	 * @param resultSet
	 *            result-set
	 * @param i
	 *            column index
	 * @param typeCache
	 *            for caching types
	 * @return object
	 */
	private String getValue(ResultSet resultSet, int i, Map<Integer, Integer> typeCache) throws SQLException {
		Object object;
		Integer type = typeCache.get(i);
		if (type == null) {
			try {
				type = resultSet.getMetaData().getColumnType(i);
				if (SQLDialect.treatDateAsTimestamp) {
					if (type == Types.DATE) {
						type = Types.TIMESTAMP;
					}
				}
			} catch (SQLException e) {
				type = Types.OTHER;
			}
			typeCache.put(i, type);
		}

		if (type == Types.TIMESTAMP) {
			object = resultSet.getTimestamp(i);
		} else if (type == Types.TIME) {
			object = resultSet.getTime(i);
		} else if (type == Types.DATE) {
			object = resultSet.getDate(i);
		} else {
			object = resultSet.getObject(i);
		}

		if (object == null || resultSet.wasNull()) {
			return Configuration.getNullColumnPlaceholder();
		}

		if (object instanceof byte[]) {
			return Base64.encodeBytes((byte[]) object);
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

		return object.toString();
	};

	/**
	 * Finalizes reading.
	 */
	public void close() {
		if (columnLabel != null) {
			synchronized (transformerHandler) {
				try {
					String content = "\n\n  ";
					transformerHandler.characters(content.toCharArray(), 0,
							content.length());
				} catch (SAXException e) {
					throw new RuntimeException(e);
				}
			}
		}
	}

}