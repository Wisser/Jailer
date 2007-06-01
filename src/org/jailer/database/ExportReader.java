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

package org.jailer.database;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

import org.jailer.database.StatementExecutor.ResultSetReader;
import org.jailer.datamodel.Column;
import org.jailer.datamodel.Table;
import org.jailer.util.SqlUtil;


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
        if (columnLabel == null) {
            columnCount = resultSet.getMetaData().getColumnCount();
            columnLabel = new String[columnCount + 1];
            labelCSL = "";
            int li = 1;
            for (int i = 1; i <= columnCount; ++i) {
                String mdColumnLabel = resultSet.getMetaData().getColumnLabel(i);
                if (table.getName().equals("ADDRESS") && mdColumnLabel.equals("STREETNOADDITION")) {
                    // ignore column ADDRESS.STREETNOADDITION in crmapp-db
                    // TODO: fix schema-error and remove this fix
                    --columnCount;
                    continue;
                }
                columnLabel[li] = mdColumnLabel;
                if (li > 1) {
                    labelCSL += ", ";
                }
                labelCSL += columnLabel[i];
                li++;
            }
        }
        try {
            StringBuffer valueList = new StringBuffer("");
            for (int i = 1; i <= columnCount; ++i) {
                Object content = resultSet.getObject(i);
                if (resultSet.wasNull()) {
                    content = null;
                }
                if (i > 1) {
                    valueList.append(", ");
                }
                valueList.append(SqlUtil.toSql(content));
            }
            if (table.upsert || upsertOnly) {
                // Select * From (values(397, '30080000', 'Dresdner Bank')) 
                // AS Q(OID, A, B) Where not exists 
                // (Select * from BANK T Where T.OID=Q.OID)
                Map<String, String> val = new HashMap<String, String>();
                StringBuffer valuesWONull = new StringBuffer("");
                StringBuffer columnsWONull = new StringBuffer("");
                for (int i = 1; i <= columnCount; ++i) {
                    Object content = resultSet.getObject(i);
                    if (resultSet.wasNull()) {
                        content = null;
                    }
                    String cVal = SqlUtil.toSql(content);
                    val.put(columnLabel[i], cVal);
                    if (content != null) {
                        if (i > 1) {
                            valuesWONull.append(", ");
                            columnsWONull.append(", ");
                        }
                        valuesWONull.append(cVal);
                        columnsWONull.append(columnLabel[i]);
                    }
                }
                
                String insertHead = "Insert into " + table.getName() + "(" + columnsWONull + ") "
                                    + "Select * From (values ";
                StringBuffer terminator = new StringBuffer(") as Q(" + columnsWONull + ") Where not exists (Select * from " + table.getName() + " T "
                        + "Where ");
                boolean f = true;
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
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Flushes the export-reader.
     */
    public void close() {
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
     * Writes into script.
     */
    private void writeToScriptFile(String content) throws IOException {
        synchronized (scriptFileWriter) {
            scriptFileWriter.write(content);
        }
    }
    
}
