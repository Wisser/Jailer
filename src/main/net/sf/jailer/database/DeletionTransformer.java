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

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

import net.sf.jailer.CommandLineParser;
import net.sf.jailer.database.Session.ResultSetReader;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlUtil;


/**
 * A {@link ResultSetReader} that writes the read rows as SQL-DELETE-statements 
 * into the deletion-script.
 * 
 * @author Ralf Wisser
 */
public class DeletionTransformer implements ResultSetReader {

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
     * Maps clear text SQL-types to {@link java.sql.Types}.
     */
    private Map<String, Integer> typeCache = new HashMap<String, Integer>();

    /**
     * For quoting of column names.
     */
    private final Quoting quoting;
    
    /**
     * Constructor.
     * 
     * @param table the table to read from
     * @param scriptFileWriter the file to write to
     * @param maxBodySize maximum length of SQL values list (for generated deletes)
     */
    public DeletionTransformer(Table table, OutputStreamWriter scriptFileWriter, int maxBodySize, DatabaseMetaData metaData) throws SQLException {
        this.table = table;
        this.scriptFileWriter = scriptFileWriter;
        deleteStatementBuilder = new StatementBuilder(maxBodySize);
        this.quoting = new Quoting(metaData);
    }
    
    /**
     * Reads result-set and writes into export-script.
     * 
     * @param resultSet the result set
     */
    public void readCurrentRow(ResultSet resultSet) throws SQLException {
        try {
        	if (SqlUtil.dbms == DBMS.SYBASE || (SQLDialect.currentDialect != null && !SQLDialect.currentDialect.supportsInClauseForDeletes)) {
        		String delete = "Delete from " + qualifiedTableName(table) + " Where ";
                boolean firstTime = true;
                for (Column pkColumn: table.primaryKey.getColumns()) {
                	delete += (firstTime? "" : " and ") + pkColumn.name + "="
                    		+ SqlUtil.toSql(SqlUtil.getObject(resultSet, quoting.unquote(pkColumn.name), typeCache));
                    firstTime = false;
                }
                writeToScriptFile(delete + "\n");
        	} else {
	            String deleteHead;
	            String item;
	            if (table.primaryKey.getColumns().size() == 1) {
	                deleteHead = "Delete from " + qualifiedTableName(table) + " Where " + table.primaryKey.getColumns().get(0).name + " in (";
	                item = SqlUtil.toSql(SqlUtil.getObject(resultSet, quoting.unquote(table.primaryKey.getColumns().get(0).name), typeCache));
	            } else {
	                deleteHead = "Delete from " + qualifiedTableName(table) + " Where (";
	                item = "(";
	                boolean firstTime = true;
	                for (Column pkColumn: table.primaryKey.getColumns()) {
	                    deleteHead += (firstTime? "" : ", ") + pkColumn.name;
	                    item += (firstTime? "" : ", ") + SqlUtil.toSql(SqlUtil.getObject(resultSet, quoting.unquote(pkColumn.name), typeCache));
	                    firstTime = false;
	                }
	                item += ")";
	                deleteHead += ") in (";
	                if (SQLDialect.currentDialect.needsValuesKeywordForDeletes) {
	                	deleteHead += "values ";
	                }
	            }
	            if (!deleteStatementBuilder.isAppendable(deleteHead, item)) {
	                writeToScriptFile(deleteStatementBuilder.build());
	            }
	            deleteStatementBuilder.append(deleteHead, item, ", ", ");\n");
        	}
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Gets qualified table name.
     * 
     * @param t the table
     * @return qualified name of t
     */
    private String qualifiedTableName(Table t) {
    	String schema = t.getOriginalSchema("");
    	String mappedSchema = CommandLineParser.getInstance().getSourceSchemaMapping().get(schema);
    	if (mappedSchema != null) {
    		schema = mappedSchema;
    	}
    	if (schema.length() == 0) {
    		return t.getUnqualifiedName();
    	}
		return schema + "." + t.getUnqualifiedName();
	}

    /**
     * Writes into script.
     */
    private void writeToScriptFile(String content) throws IOException {
        synchronized (scriptFileWriter) {
        	if (SqlUtil.dbms == DBMS.ORACLE) {
       			scriptFileWriter.write(SqlUtil.splitDMLStatement(content, 2400));
        	} else {
        		scriptFileWriter.write(content);
        	}
        }
    }

    /**
     * Finalizes reading.
     */
    public void close() {
        try {
            writeToScriptFile(deleteStatementBuilder.build());
            typeCache = new HashMap<String, Integer>();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
    
}
