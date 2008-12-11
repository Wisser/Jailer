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
package net.sf.jailer.dbunit;

import java.io.IOException;
import java.security.Timestamp;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.xml.transform.sax.TransformerHandler;

import net.sf.jailer.database.DBMS;
import net.sf.jailer.database.ExportReader;
import net.sf.jailer.database.SQLDialect;
import net.sf.jailer.database.StatementExecutor.ResultSetReader;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlUtil;

import org.xml.sax.SAXException;
import org.xml.sax.helpers.AttributesImpl;

/**
 * A {@link ResultSetReader} that writes the read rows
 * into a DbUnit flat XML dataset.
 * 
 * @author Ralf Wisser
 */
public class FlatXMLWriter implements ResultSetReader {

    /**
     * The table to read from.
     */
    private final Table table;
    
    /**
     * Name of XML row element, or <code>null</code> if the table name can not be converted into a valid XML tag identifier.
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
     * For quoting of column names.
     */
    private final Quoting quoting;
    
    /**
     * Maps clear text SQL-types to {@link java.sql.Types}.
     */
    private Map<Integer, Integer> typeCache = new HashMap<Integer, Integer>();

    /**
     * Constructor.
     * 
     * @param table the table to read from
     * @param transformerHandler to write the XML into
     * @param maxBodySize maximum length of SQL values list (for generated inserts)
     * @param upsertOnly use 'upsert' statements for all entities
     */
    public FlatXMLWriter(Table table, TransformerHandler transformerHandler, boolean upsertOnly, int maxBodySize, DatabaseMetaData metaData) throws SQLException {
        this.table = table;
        this.transformerHandler = transformerHandler;
        this.quoting = new Quoting(metaData);
        this.rowElementName = table.getUnqualifiedName();
    }
    
    /**
     * Reads result-set and writes into export-script.
     */
    public void readCurrentRow(ResultSet resultSet) throws SQLException {
    	if (columnLabel == null) {
            columnCount = resultSet.getMetaData().getColumnCount();
            columnLabel = new String[columnCount + 1];
            for (int i = 1; i <= columnCount; ++i) {
                String mdColumnLabel = resultSet.getMetaData().getColumnLabel(i);
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
            ++ExportReader.numberOfExportedEntities;  // TODO: refactoring of entity counting
            
        } catch (SAXException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Gets value from result-set.
     * 
     * @param resultSet result-set
     * @param i column index
     * @param typeCache for caching types
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
		} else if (type == Types.DATE) {
			object = resultSet.getDate(i);
		} else {
			object = resultSet.getObject(i);
		}
		
		if (object == null) {
			return null;
		}
		
		if (object instanceof Timestamp) {
			return ((Timestamp) object).toString();
		}
		
		return object.toString();
	};
	
	/**
     * Closes the export-reader.
     */
    public void close() {
    }
    
    /**
     * Writes into script.
     */
    private void writeToScriptFile(String content) throws IOException {
        synchronized (transformerHandler) {
        	try {
				transformerHandler.characters(content.toCharArray(), 0, content.length());
			} catch (SAXException e) {
				throw new RuntimeException(e);
			}
        }
    }
    
}
