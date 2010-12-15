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
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.DatabaseMetaData;
import java.sql.Date;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.SQLXML;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.sf.jailer.CommandLineParser;
import net.sf.jailer.Configuration;
import net.sf.jailer.database.SQLDialect.UPSERT_MODE;
import net.sf.jailer.database.Session.ResultSetReader;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.Base64;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlScriptExecutor;
import net.sf.jailer.util.SqlUtil;

/**
 * A {@link ResultSetReader} that writes the read rows as DML-statements 
 * into the export-script.
 * 
 * @author Ralf Wisser
 */
public class DMLTransformer implements ResultSetReader {

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
     * Literals for empty lob values.
     */
    private String[] emptyLobValue = null;

    /**
     * Whether or not the table has columns of type CLOB or BLOB.
     */
    private boolean tableHasLobs = false;
    
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
     * Whether to create INSERTs or UPSERTS for all entities.
     */
    private final boolean upsertOnly;

    /**
     * Maximum length of SQL values list (for generated inserts).
     */
    private final int maxBodySize;
    
    /**
     * Counts the exported LOBs. (GUI support)
     */
    public static long numberOfExportedLOBs;

    /**
     * For quoting of column names.
     */
    private final Quoting quoting;
    
    /**
     * If table has identity column (MSSQL/Sybase)
     */
    private boolean tableHasIdentityColumn;
    
    /**
     * Current session;
     */
    private final Session session;
    
    /**
     * Maps clear text SQL-types to {@link java.sql.Types}.
     */
    private Map<Integer, Integer> typeCache = new HashMap<Integer, Integer>();

    /**
     * Constructor.
     * 
     * @param table the table to read from
     * @param scriptFileWriter the file to write to
     * @param maxBodySize maximum length of SQL values list (for generated inserts)
     * @param upsertOnly use 'upsert' statements for all entities
     */
    public DMLTransformer(Table table, OutputStreamWriter scriptFileWriter, boolean upsertOnly, int maxBodySize, DatabaseMetaData metaData, Session session) throws SQLException {
        this.maxBodySize = maxBodySize;
        this.upsertOnly = upsertOnly;
        this.table = table;
        this.scriptFileWriter = scriptFileWriter;
        this.insertStatementBuilder = new StatementBuilder(SQLDialect.currentDialect.supportsMultiRowInserts || session.dbms == DBMS.ORACLE || session.dbms == DBMS.SQLITE? maxBodySize : 1);
        this.quoting = new Quoting(metaData);
        this.session = session;
        tableHasIdentityColumn = false;
        if (Configuration.forDbms(session).isIdentityInserts()) {
        	for (Column c: table.getColumns()) {
        		if (c.isIdentityColumn) {
        			tableHasIdentityColumn = true;
        			break;
        		}
        	}
        }
    }
    
    /**
     * Reads result-set and writes into export-script.
     */
    public void readCurrentRow(ResultSet resultSet) throws SQLException {
    	if (columnLabel == null) {
            columnCount = resultSet.getMetaData().getColumnCount();
            columnLabel = new String[columnCount + 1];
            lobColumns = new ArrayList<String>();
            emptyLobValue = new String[columnCount + 1];
            lobColumnIndexes = new ArrayList<Integer>();
            labelCSL = "";
            tableHasLobs = false;
            for (int i = 1; i <= columnCount; ++i) {
                String mdColumnLabel = quoting.quote(resultSet.getMetaData().getColumnLabel(i));
                int mdColumnType = resultSet.getMetaData().getColumnType(i);
                
                if ((mdColumnType == Types.BLOB || mdColumnType == Types.CLOB || mdColumnType == Types.SQLXML) && session.dbms != DBMS.SQLITE) {
                	tableHasLobs = true;
                	lobColumnIndexes.add(i);
                	lobColumns.add(mdColumnLabel);
                	if (mdColumnType == Types.SQLXML) {
                		emptyLobValue[i] = null;
                	} else {
                		emptyLobValue[i] = (mdColumnType == Types.BLOB)? SQLDialect.emptyBLOBValue : SQLDialect.emptyCLOBValue;
                	}
                    if (emptyLobValue[i] == null) {
                    	continue;
                    }
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
            StringBuffer namedValues = new StringBuffer("");
            boolean f = true;
            for (int i = 1; i <= columnCount; ++i) {
                if (columnLabel[i] == null) {
                	continue;
                }
            	Object content = SqlUtil.getObject(resultSet, i, typeCache);
                if (resultSet.wasNull()) {
                    content = null;
                }
                if (!f) {
                	namedValues.append(", ");
                	valueList.append(", ");
                }
                f = false;
                String cVal = SqlUtil.toSql(content, session);
            	if (content != null && emptyLobValue[i] != null) {
            		cVal = emptyLobValue[i];
            	}
            	valueList.append(cVal);
                namedValues.append(cVal + " " + columnLabel[i]);
            }
            if (table.getUpsert() || upsertOnly) {
                Map<String, String> val = new HashMap<String, String>();
                StringBuffer valuesWONull = new StringBuffer("");
                StringBuffer columnsWONull = new StringBuffer("");
                f = true;
                for (int i = 1; i <= columnCount; ++i) {
                    if (columnLabel[i] == null) {
                    	continue;
                    }
                    Object content = SqlUtil.getObject(resultSet, i, typeCache);
                    if (resultSet.wasNull()) {
                        content = null;
                    }
                    String cVal = SqlUtil.toSql(content, session);
                    if (SqlUtil.dbms == DBMS.POSTGRESQL && (content instanceof Date || content instanceof Timestamp)) {
                    	// explicit cast needed
                    	cVal = "timestamp " + cVal;
                    }
                	if (content != null && emptyLobValue[i] != null) {
                		cVal = emptyLobValue[i];
                	}
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
                
                String insertHead = "Insert into " + qualifiedTableName(table) + "(" + columnsWONull + ") ";
                f = true;
                StringBuffer whereForTerminator = new StringBuffer("");
                StringBuffer where = new StringBuffer("");
                StringBuffer whereWOAlias = new StringBuffer("");
                
                // assemble 'where' for sub-select and update
                for (Column pk: table.primaryKey.getColumns()) {
                    if (!f) {
                        whereForTerminator.append(" and ");
                        where.append(" and ");
                        whereWOAlias.append(" and ");
                    }
                    f = false;
                    whereForTerminator.append("T." + pk.name + "=Q." + pk.name);
                    String value;
                    if (val.containsKey(pk.name)) {
                    	value = val.get(pk.name);
                    } else if (val.containsKey(pk.name.toLowerCase())) {
                    	value = val.get(pk.name.toLowerCase());
                    } else {
                    	value = val.get(pk.name.toUpperCase());
                    }
                    where.append("T." + pk.name + "=" + value);
                    whereWOAlias.append(pk.name + "=" + value);
                }

                if (SQLDialect.currentDialect.upsertMode == UPSERT_MODE.ORACLE && !tableHasLobs) {
                	// MERGE INTO JL_TMP T USING (SELECT 1 c1, 2 c2 from dual) incoming 
                	// ON (T.c1 = incoming.c1) 
                	// WHEN MATCHED THEN UPDATE SET T.c2 = incoming.c2 
                	// WHEN NOT MATCHED THEN INSERT (T.c1, T.c2) VALUES (incoming.c1, incoming.c2)
                	insertHead = "MERGE INTO " + qualifiedTableName(table) + " T USING(";
                    StringBuffer terminator = new StringBuffer(") Q ON(" + whereForTerminator + ") ");
	                
                    StringBuffer sets = new StringBuffer();
                    StringBuffer tSchema = new StringBuffer();
                    StringBuffer iSchema = new StringBuffer();
	                for (int i = 1; i <= columnCount; ++i) {
	                    if (columnLabel[i] == null) {
	                    	continue;
	                    }
	                    if  (!isPrimaryKeyColumn(columnLabel[i])) {
		                    if (sets.length() > 0) {
		                    	sets.append(", ");
		                    }
		                    sets.append("T." + columnLabel[i] + "=Q." + columnLabel[i]);
	                    }
	                    if (tSchema.length() > 0) {
	                    	tSchema.append(", ");
	                    }
	                    tSchema.append("T." + columnLabel[i]);
	                    if (iSchema.length() > 0) {
	                    	iSchema.append(", ");
	                    }
	                    iSchema.append("Q." + columnLabel[i]);
	                }
	                if (sets.length() > 0) {
	                	terminator.append("WHEN MATCHED THEN UPDATE SET " + sets + " ");
	                }
                	terminator.append("WHEN NOT MATCHED THEN INSERT (" + tSchema + ") VALUES(" + iSchema + ");\n");

	                StatementBuilder sb = upsertInsertStatementBuilder.get(insertHead);
	                if (sb == null) {
	                    sb = new StatementBuilder(maxBodySize);
	                    upsertInsertStatementBuilder.put(insertHead, sb);
	                }
	            
	                String item = "Select " + valueList + " from dual";
	                if (!sb.isAppendable(insertHead, item)) {
	                    writeToScriptFile(sb.build(), true);
	                }
	                if (sb.isEmpty()) {
	                	item = "Select " + namedValues + " from dual";
	                }
                	sb.append(insertHead, item, " UNION ALL ", terminator.toString());
                } else if (SQLDialect.currentDialect.upsertMode == UPSERT_MODE.DB2) {
                	insertHead += "Select * From (values ";
	                StringBuffer terminator = new StringBuffer(") as Q(" + columnsWONull + ") Where not exists (Select * from " + qualifiedTableName(table) + " T "
	                        + "Where ");
	                terminator.append(whereForTerminator + ");\n");
	                
	                StatementBuilder sb = upsertInsertStatementBuilder.get(insertHead);
	                if (sb == null) {
	                    sb = new StatementBuilder(maxBodySize);
	                    upsertInsertStatementBuilder.put(insertHead, sb);
	                }
	            
	                String item = "(" + valuesWONull + ")";
	                if (!sb.isAppendable(insertHead, item)) {
	                    writeToScriptFile(sb.build(), true);
	                }
	                sb.append(insertHead, item, ", ", terminator.toString());
                } else {
                	String item = "Select " + valuesWONull + " From " + 
                		(SQLDialect.currentDialect.upsertMode == UPSERT_MODE.FROM_DUAL || 
                		 SQLDialect.currentDialect.upsertMode == UPSERT_MODE.ORACLE? // oracle table with lobs
                				 "dual" : SQLDialect.DUAL_TABLE);
                	StringBuffer terminator = new StringBuffer(" Where not exists (Select * from " + qualifiedTableName(table) + " T "
	                        + "Where ");
	                terminator.append(where + ");\n");
	                
	                StatementBuilder sb = upsertInsertStatementBuilder.get(insertHead);
	                if (sb == null) {
	                    sb = new StatementBuilder(1);
	                    upsertInsertStatementBuilder.put(insertHead, sb);
	                }
	            
	                if (!sb.isAppendable(insertHead, item)) {
	                    writeToScriptFile(sb.build(), true);
	                }
	                sb.append(insertHead, item, ", ", terminator.toString());
                }
                
                if (SQLDialect.currentDialect.upsertMode != UPSERT_MODE.ORACLE || tableHasLobs) {
	                StringBuffer insert = new StringBuffer("");
	                insert.append("Update " + qualifiedTableName(table) + " set ");
	                f = true;
	                for (int i = 1; i <= columnCount; ++i) {
	                    if (columnLabel[i] == null || (emptyLobValue[i] != null && !"null".equals(val.get(columnLabel[i])))) {
	                    	continue;
	                    }
	                    if (isPrimaryKeyColumn(columnLabel[i])) {
	                    	continue;
	                    }
	                    if (!f) {
	                        insert.append(", ");
	                    }
	                    f = false;
	                    insert.append(columnLabel[i] + "=" + val.get(columnLabel[i]));
	                }
	                if (!f) {
	                	insert.append(" Where " + whereWOAlias + ";\n");
	                    writeToScriptFile(insert.toString(), true);
	                }
                }
            } else {
            	if (session.dbms == DBMS.ORACLE && maxBodySize > 1) {
            		String insertSchema = "Insert into " + qualifiedTableName(table) + "(" + labelCSL + ") ";
	                String item = "\n Select " + valueList + " From DUAL";
	                if (!insertStatementBuilder.isAppendable(insertSchema, item)) {
	                    writeToScriptFile(insertStatementBuilder.build(), true);
	                }
	                insertStatementBuilder.append(insertSchema, item, " Union all ", ";\n");
            	} else if (session.dbms == DBMS.SQLITE && maxBodySize > 1) {
            		String insertSchema = "Insert into " + qualifiedTableName(table) + "(" + labelCSL + ") ";
	                String item = "\n Select " + valueList + " ";
	                if (!insertStatementBuilder.isAppendable(insertSchema, item)) {
	                    writeToScriptFile(insertStatementBuilder.build(), true);
	                }
	                insertStatementBuilder.append(insertSchema, item, " Union all ", ";\n");
            	} else {
	                String insertSchema = "Insert into " + qualifiedTableName(table) + "(" + labelCSL + ") values ";
	                String item = (maxBodySize > 1? "\n " : "") + "(" + valueList + ")";
	                if (!insertStatementBuilder.isAppendable(insertSchema, item)) {
	                    writeToScriptFile(insertStatementBuilder.build(), true);
	                }
	                insertStatementBuilder.append(insertSchema, item, ", ", ";\n");
            	}
            }
            
            exportLobs(table, resultSet);
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
    	String mappedSchema = CommandLineParser.getInstance().getSchemaMapping().get(schema);
    	if (mappedSchema != null) {
    		schema = mappedSchema;
    	}
    	if (schema.length() == 0) {
    		return t.getUnqualifiedName();
    	}
		return schema + "." + t.getUnqualifiedName();
	}

	/**
     * Checks if columns is part of primary key.
     * 
     * @param column the column
     * @return <code>true</code> if column is part of primary key
     */
    private boolean isPrimaryKeyColumn(String column) {
    	for (Column c: table.primaryKey.getColumns()) {
    		if (c.name.equalsIgnoreCase(column)) {
    			return true;
    		}
    	}
		return false;
	}

	/**
     * Exports the (c|b)lob content.
     * 
     * @param resultSet export current row
     */
    private void exportLobs(Table table, ResultSet resultSet) throws IOException, SQLException {
    	synchronized (scriptFileWriter) {
	    	for (int i = 0; i < lobColumnIndexes.size(); ++i) {
				Object lob = resultSet.getObject(lobColumnIndexes.get(i));
				Map<String, String> val = new HashMap<String, String>();
				for (int j = 1; j <= columnCount; ++j) {
	                if (columnLabel[j] == null) {
	                	continue;
	                }
	                Object content = SqlUtil.getObject(resultSet, j, typeCache);
	                if (resultSet.wasNull()) {
	                    content = null;
	                }
	                String cVal = SqlUtil.toSql(content, session);
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
	            if (lob instanceof SQLXML) {
	            	++numberOfExportedLOBs;
	            	flush();
	            	SQLXML xml = (SQLXML) lob;
					writeToScriptFile(SqlScriptExecutor.UNFINISHED_MULTILINE_COMMENT + "XML " + qualifiedTableName(table) + ", " + lobColumns.get(i) + ", " + where + "\n", false);
					Reader in = xml.getCharacterStream();
					int c;
					StringBuffer line = new StringBuffer(SqlScriptExecutor.UNFINISHED_MULTILINE_COMMENT);
					while ((c = in.read()) != -1) {
						if ((char) c == '\n') {
							writeToScriptFile(line.toString() + "\\n\n", false);
							line = new StringBuffer(SqlScriptExecutor.UNFINISHED_MULTILINE_COMMENT);
						} else {
							if ((char) c == '\r') {
								line.append("\\r");
							} else {
								line.append((char) c);
								if ((char) c == '\\') {
									line.append((char) c);
								}
							}
						}
						if (line.length() >= 200) {
							writeToScriptFile(line.toString() + "\n", false);
							line = new StringBuffer(SqlScriptExecutor.UNFINISHED_MULTILINE_COMMENT);
						}
					}
					in.close();
					writeToScriptFile(line.toString() + "\n" + SqlScriptExecutor.FINISHED_MULTILINE_COMMENT + "\n", false);
				}
	            if (lob instanceof Clob) {
	            	++numberOfExportedLOBs;
	            	flush();
					Clob clob = (Clob) lob;
					writeToScriptFile(SqlScriptExecutor.UNFINISHED_MULTILINE_COMMENT + "CLOB " + qualifiedTableName(table) + ", " + lobColumns.get(i) + ", " + where + "\n", false);
					Reader in = clob.getCharacterStream();
					int c;
					StringBuffer line = new StringBuffer(SqlScriptExecutor.UNFINISHED_MULTILINE_COMMENT);
					while ((c = in.read()) != -1) {
						if ((char) c == '\n') {
							writeToScriptFile(line.toString() + "\\n\n", false);
							line = new StringBuffer(SqlScriptExecutor.UNFINISHED_MULTILINE_COMMENT);
						} else {
							if ((char) c == '\r') {
								line.append("\\r");
							} else {
								line.append((char) c);
								if ((char) c == '\\') {
									line.append((char) c);
								}
							}
						}
						if (line.length() >= 200) {
							writeToScriptFile(line.toString() + "\n", false);
							line = new StringBuffer(SqlScriptExecutor.UNFINISHED_MULTILINE_COMMENT);
						}
					}
					in.close();
					writeToScriptFile(line.toString() + "\n" + SqlScriptExecutor.FINISHED_MULTILINE_COMMENT + "\n", false);
				}
	            if (lob instanceof Blob) {
	            	++numberOfExportedLOBs;
	            	flush();
					Blob blob = (Blob) lob;
					writeToScriptFile(SqlScriptExecutor.UNFINISHED_MULTILINE_COMMENT + "BLOB " + qualifiedTableName(table) + ", " + lobColumns.get(i) + ", " + where + "\n", false);
					InputStream in = blob.getBinaryStream();
					int b;
					StringBuffer line = new StringBuffer(SqlScriptExecutor.UNFINISHED_MULTILINE_COMMENT);
					byte[] buffer = new byte[64];
					int size = 0;
					while ((b = in.read()) != -1) {
						buffer[size++] = (byte) b;
						if (size == buffer.length) {
							writeToScriptFile(line.toString() + Base64.encodeBytes(buffer, Base64.DONT_BREAK_LINES) + "\n", false);
							line = new StringBuffer(SqlScriptExecutor.UNFINISHED_MULTILINE_COMMENT);
							size = 0;
						}
					}
					in.close();
					writeToScriptFile(line.toString() + Base64.encodeBytes(buffer, 0, size, Base64.DONT_BREAK_LINES) + "\n" + SqlScriptExecutor.FINISHED_MULTILINE_COMMENT + "\n", false);
				}
			}
    	}
	}

	/**
     * Flushes the export-reader.
     */
    public void flush() {
        try {
            writeToScriptFile(insertStatementBuilder.build(), true);
            for (StatementBuilder sb: upsertInsertStatementBuilder.values()) {
                writeToScriptFile(sb.build(), true);
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
    	synchronized (scriptFileWriter) {
    		if (identityInsertTable != null) {
    			try {
					scriptFileWriter.write("SET IDENTITY_INSERT " + qualifiedTableName(identityInsertTable) + " OFF;\n");
				} catch (IOException e) {
					throw new RuntimeException(e);
				}
    			identityInsertTable = null;
    		}
    	}
    }
    
    /**
     * Table which is currently enabled for identity-inserts.
     */
    private static Table identityInsertTable = null;
    
    /**
     * Writes into script.
     */
    private void writeToScriptFile(String content, boolean wrap) throws IOException {
        synchronized (scriptFileWriter) {
        	if (tableHasIdentityColumn) {
        		if (identityInsertTable != table) {
        			if (identityInsertTable != null) {
        				scriptFileWriter.write("SET IDENTITY_INSERT " + qualifiedTableName(identityInsertTable) + " OFF;\n");
            			identityInsertTable = null;
        			}
        			scriptFileWriter.write("SET IDENTITY_INSERT " + qualifiedTableName(table) + " ON;\n");
        			identityInsertTable = table;
        		}
        	}
        	if (wrap && SqlUtil.dbms == DBMS.ORACLE) {
       			scriptFileWriter.write(SqlUtil.splitDMLStatement(content, 2400));
        	} else {
        		scriptFileWriter.write(content);
        	}
        }
    }
    
}
