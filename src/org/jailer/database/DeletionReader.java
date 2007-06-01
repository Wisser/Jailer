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

import org.jailer.database.StatementExecutor.ResultSetReader;
import org.jailer.datamodel.Column;
import org.jailer.datamodel.Table;
import org.jailer.util.SqlUtil;


/**
 * A {@link ResultSetReader} that writes the read rows as SQL-DELETE-statements 
 * into the deletion-script.
 * 
 * @author Wisser
 */
public class DeletionReader implements ResultSetReader {

    /**
     * The table to read from.
     */
    private final Table table;
    
    /**
     * The file to write to.
     */
    private final OutputStreamWriter scriptFileWriter;
    
    /**
     * For building compact delete-statements.
     */
    private StatementBuilder deleteStatementBuilder;
    
    /**
     * Constructor.
     * 
     * @param table the table to read from
     * @param scriptFileWriter the file to write to
     * @param maxBodySize maximum length of SQL values list (for generated deletes)
     */
    public DeletionReader(Table table, OutputStreamWriter scriptFileWriter, int maxBodySize) {
        this.table = table;
        this.scriptFileWriter = scriptFileWriter;
        deleteStatementBuilder = new StatementBuilder(maxBodySize);
    }
    
    /**
     * Reads result-set and writes into export-script.
     */
    public void readCurrentRow(ResultSet resultSet) throws SQLException {
        try {
            String deleteHead;
            String item;
            if (table.primaryKey.getColumns().size() == 1) {
                deleteHead = "Delete from " + table.getName() + " Where " + table.primaryKey.getColumns().get(0).name + " in (";
                item = SqlUtil.toSql(resultSet.getObject(table.primaryKey.getColumns().get(0).name));
            } else {
                deleteHead = "Delete from " + table.getName() + " Where (";
                item = "(";
                boolean firstTime = true;
                for (Column pkColumn: table.primaryKey.getColumns()) {
                    deleteHead += (firstTime? "" : ", ") + pkColumn.name;
                    item += (firstTime? "" : ", ") + SqlUtil.toSql(resultSet.getObject(pkColumn.name));
                    firstTime = false;
                }
                item += ")";
                deleteHead += ") in (values ";
            }
            if (!deleteStatementBuilder.isAppendable(deleteHead, item)) {
                writeToScriptFile(deleteStatementBuilder.build());
            }
            deleteStatementBuilder.append(deleteHead, item, ", ", ");\n");
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

    /**
     * Finalizes reading.
     */
    public void close() {
        try {
            writeToScriptFile(deleteStatementBuilder.build());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
    
}
