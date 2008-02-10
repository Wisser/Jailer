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

package net.sf.jailer.database;

import net.sf.jailer.database.StatementExecutor.ResultSetReader;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.Base64;
import net.sf.jailer.util.SqlScriptExecutor;
import net.sf.jailer.util.SqlUtil;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.sql.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


/**
 * A {@link ResultSetReader} that writes the read rows as SQL-INSERT-statements 
 * into the export-script.
 * 
 * @author Wisser
 */
public class ExportReader implements ResultSetReader {

    /**
     * The table to read from.
     */
    private final Table table;
    
    /**
     * The file to write to.
     */
    private final OutputStreamWriter scriptFileWriter;
    
    /**
     * Number of columns.
     */
    private int columnCount;

    /**
     * Labels of columns.
     */
    private String[] columnLabel = null;

    /**
     * Lob columns.
     */
    private List<String> lobColumns = null;

    /**
     * Lob columns indexes.
     */
    private List<Integer> lobColumnIndexes = null;

    /**
     * Labels of columns as comma separated list.
     */
    private String labelCSL;

    /**
     * For building compact insert-statements.
     */
    private final StatementBuilder insertStatementBuilder;
    
    /**
     * For building compact insert-parts of upsert-statements.
     */
    private Map<String, StatementBuilder> upsertInsertStatementBuilder = new HashMap<String, StatementBuilder>();

    /**
     * Whether to create INSERTs for 'initial-data' entities or UPSERTS for all entities.
     */
    private final boolean upsertOnly;

    /**
     * Maximum length of SQL values list (for generated inserts).
     */
    private final int maxBodySize;
    
    /**
     * Counts the exported entities. (GUI support)
     */
    public static long numberOfExportedEntities;
    
    /**
     * Counts the exported LOBs. (GUI support)
     */
    public static long numberOfExportedLOBs;
    
    /**
     * Constructor.
     * 
     * @param table the table to read from
     * @param scriptFileWriter the file to write to
     * @param maxBodySize maximum length of SQL values list (for generated inserts)
     * @param upsertOnly use 'upsert' statements for all entities
     */
    public ExportReader(Table table, OutputStreamWriter scriptFileWriter, boolean upsertOnly, int maxBodySize) {
        this.maxBodySize = maxBodySize;
        this.upsertOnly = upsertOnly;
        this.table = table;
        this.scriptFileWriter = scriptFileWriter;
        this.insertStatementBuilder = new StatementBuilder(maxBodySize);
    }
    
    /**
     * Reads result-set and writes into export-script.
     */
    public void readCurrentRow(ResultSet resultSet) throws SQLException {
    	++numberOfExportedEntities;
        if (columnLabel == null) {
            columnCount = resultSet.getMetaData().getColumnCount();
            columnLabel = new String[columnCount + 1];
            lobColumns = new ArrayList<String>();
            lobColumnIndexes = new ArrayList<Integer>();
            labelCSL = "";
            for (int i = 1; i <= columnCount; ++i) {
                String mdColumnLabel = resultSet.getMetaData().getColumnLabel(i);
                int mdColumnType = resultSet.getMetaData().getColumnType(i);
                
                if (mdColumnType == Types.BLOB || mdColumnType == Types.CLOB) {
                	lobColumnIndexes.add(i);
                	lobColumns.add(mdColumnLabel);
                    continue;
                }
                columnLabel[i] = mdColumnLabel;
                if (labelCSL.length() > 0) {
                    labelCSL += ", ";
                }
                labelCSL += columnLabel[i];
            }
        }
        try {
            StringBuffer valueList = new StringBuffer("");
            boolean f = true;
            for (int i = 1; i <= columnCount; ++i) {
                if (columnLabel[i] == null) {
                	continue;
                }
            	Object content = resultSet.getObject(i);
                if (resultSet.wasNull()) {
                    content = null;
                }
                if (!f) {
                    valueList.append(", ");
                }
                f = false;
                valueList.append(SqlUtil.toSql(content));
            }
            if (table.upsert || upsertOnly) {
                // Select * From (values(397, '30080000', 'Dresdner Bank')) 
                // AS Q(OID, A, B) Where not exists 
                // (Select * from BANK T Where T.OID=Q.OID)
                Map<String, String> val = new HashMap<String, String>();
                StringBuffer valuesWONull = new StringBuffer("");
                StringBuffer columnsWONull = new StringBuffer("");
                f = true;
                for (int i = 1; i <= columnCount; ++i) {
                    if (columnLabel[i] == null) {
                    	continue;
                    }
                    Object content = resultSet.getObject(i);
                    if (resultSet.wasNull()) {
                        content = null;
                    }
                    String cVal = SqlUtil.toSql(content);
                    val.put(columnLabel[i], cVal);
                    if (content != null) {
                        if (!f) {
                            valuesWONull.append(", ");
                            columnsWONull.append(", ");
                        }
                        f = false;
                        valuesWONull.append(cVal);
                        columnsWONull.append(columnLabel[i]);
                    }
                }
                
                String insertHead = "Insert into " + table.getName() + "(" + columnsWONull + ") "
                                    + "Select * From (values ";
                StringBuffer terminator = new StringBuffer(") as Q(" + columnsWONull + ") Where not exists (Select * from " + table.getName() + " T "
                        + "Where ");
                f = true;
                StringBuffer whereForTerminator = new StringBuffer("");
                StringBuffer where = new StringBuffer("");
                for (Column pk: table.primaryKey.getColumns()) {
                    if (!f) {
                        whereForTerminator.append(" and ");
                        where.append(" and ");
                    }
                    f = false;
                    whereForTerminator.append("T." + pk.name + "=Q." + pk.name);
                    where.append("T." + pk.name + "=" + val.get(pk.name));
                }
                terminator.append(whereForTerminator + ");\n");
                
                StatementBuilder sb = upsertInsertStatementBuilder.get(insertHead);
                if (sb == null) {
                    sb = new StatementBuilder(maxBodySize);
                    upsertInsertStatementBuilder.put(insertHead, sb);
                }
            
                String item = "(" + valuesWONull + ")";
                if (!sb.isAppendable(insertHead, item)) {
                    writeToScriptFile(sb.build());
                }
                sb.append(insertHead, item, ", ", terminator.toString());
                
                StringBuffer insert = new StringBuffer("");
                insert.append("Update " + table.getName() + " T set ");
                f = true;
                for (Map.Entry<String, String> e: val.entrySet()) {
                    if (!f) {
                        insert.append(", ");
                    }
                    f = false;
                    insert.append(e.getKey() + "=" + e.getValue());
                }
                insert.append(" Where " + where + ";\n");
                writeToScriptFile(insert.toString());
            } else {
                String insertSchema = "Insert into " + table.getName() + "(" + labelCSL + ") values ";
                String item = "(" + valueList + ")";
                if (!insertStatementBuilder.isAppendable(insertSchema, item)) {
                    writeToScriptFile(insertStatementBuilder.build());
                }
                insertStatementBuilder.append(insertSchema, item, ", ", ";\n");
            }
            
            exportLobs(table, resultSet);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Exports the (c|b)lob content.
     * 
     * @param resultSet export current row
     */
    private void exportLobs(Table table, ResultSet resultSet) throws IOException, SQLException {
		for (int i = 0; i < lobColumnIndexes.size(); ++i) {
			Object lob = resultSet.getObject(lobColumnIndexes.get(i));
			Map<String, String> val = new HashMap<String, String>();
			for (int j = 1; j <= columnCount; ++j) {
                if (columnLabel[j] == null) {
                	continue;
                }
                Object content = resultSet.getObject(j);
                if (resultSet.wasNull()) {
                    content = null;
                }
                String cVal = SqlUtil.toSql(content);
                val.put(columnLabel[j], cVal);
            }
			boolean f = true;
            StringBuffer where = new StringBuffer("");
            for (Column pk: table.primaryKey.getColumns()) {
            	if (!f) {
                    where.append(" and ");
                }
                f = false;
                where.append(pk.name + "=" + val.get(pk.name));
            }
            if (lob instanceof Clob) {
				extractClob((Clob)lob, table, where, lobColumns.get(i));
			}
            if (lob instanceof Blob) {
				extractBlob((Blob)lob, table, where, lobColumns.get(i));
			}
		}
	}

	private void extractClob(Clob clob, Table table, StringBuffer where, String columns) throws IOException, SQLException {
		++numberOfExportedLOBs;
		flush();
		writeToScriptFile(SqlScriptExecutor.UNFINISHED_MULTILINE_COMMENT + "CLOB " + table.getName() + ", " + columns + ", " + where + "\n");
		Reader in = clob.getCharacterStream();
		int c;
		StringBuffer line = new StringBuffer(SqlScriptExecutor.UNFINISHED_MULTILINE_COMMENT);
		while ((c = in.read()) != -1) {
			if ((char) c == '\n') {
				writeToScriptFile(line.toString() + "\\n\n");
				line = new StringBuffer(SqlScriptExecutor.UNFINISHED_MULTILINE_COMMENT);
			} else {
				line.append((char) c);
				if ((char) c == '\\') {
					line.append((char) c);
				}
			}
			if (line.length() >= 100) {
				writeToScriptFile(line.toString() + "\n");
				line = new StringBuffer(SqlScriptExecutor.UNFINISHED_MULTILINE_COMMENT);
			}
		}
		in.close();
		writeToScriptFile(line.toString() + "\n" + SqlScriptExecutor.FINISHED_MULTILINE_COMMENT + "\n");
	}

	private void extractBlob(Blob blob, Table table, StringBuffer where, String columns) throws IOException, SQLException {
		++numberOfExportedLOBs;
		flush();
		writeToScriptFile(SqlScriptExecutor.UNFINISHED_MULTILINE_COMMENT + "BLOB " + table.getName() + ", " + columns + ", " + where + "\n");
		InputStream input = blob.getBinaryStream();
		int b;
		StringBuffer line = new StringBuffer(SqlScriptExecutor.UNFINISHED_MULTILINE_COMMENT);
		byte[] buffer = new byte[64];
		int size = 0;
		while ((b = input.read()) != -1) {
			buffer[size++] = (byte) b;
			if (size == buffer.length) {
				writeToScriptFile(line.toString() + Base64.encodeBytes(buffer, Base64.DONT_BREAK_LINES) + "\n");
				line = new StringBuffer(SqlScriptExecutor.UNFINISHED_MULTILINE_COMMENT);
				size = 0;
			}
		}
		input.close();
		writeToScriptFile(line.toString() + Base64.encodeBytes(buffer, 0, size, Base64.DONT_BREAK_LINES) + "\n" + SqlScriptExecutor.FINISHED_MULTILINE_COMMENT + "\n");
	}

	/**
     * Flushes the export-reader.
     */
    public void flush() {
        try {
            writeToScriptFile(insertStatementBuilder.build());
            for (StatementBuilder sb: upsertInsertStatementBuilder.values()) {
                writeToScriptFile(sb.build());
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
    
	/**
     * Flushes the export-reader.
     */
    public void close() {
    	flush();
    }
    
    /**
     * Writes into script.
     */
    private void writeToScriptFile(String content) throws IOException {
        synchronized (scriptFileWriter) {
            scriptFileWriter.write(content);
        }
    }
    
}
