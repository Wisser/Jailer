/*
 * Copyright 2007 - 2017 the original author or authors.
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
import java.util.Set;

import net.sf.jailer.CommandLine;
import net.sf.jailer.Configuration;
import net.sf.jailer.TransformerFactory;
import net.sf.jailer.database.Session.AbstractResultSetReader;
import net.sf.jailer.database.Session.ResultSetReader;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.Filter;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.Base64;
import net.sf.jailer.util.CellContentConverter;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlScriptExecutor;
import net.sf.jailer.util.SqlUtil;

/**
 * A {@link ResultSetReader} that writes the read rows as DML-statements 
 * into the export-script.
 * 
 * @author Ralf Wisser
 */
public class DMLTransformer extends AbstractResultSetReader {

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
    private boolean[] isLobColumn;
    
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
    protected final Quoting quoting;
    
    /**
     * If table has identity column (MSSQL/Sybase)
     */
    private boolean tableHasIdentityColumn;
    
    /**
     * Current session;
     */
    private final Session session;

    /**
     * Configuration of the target DBMS.
     */
    private final Configuration targetDBMSConfiguration;

    /**
     * SQL Dialect.
     */
	private final SQLDialect currentDialect;

	/**
	 * The command line arguments.
	 */
	private final CommandLine commandLine;
	
	/**
	 * Transforms {@link Filter} into SQL-expressions.
	 */
	private final ImportFilterTransformer importFilterTransformer;
	
    /**
     * Factory.
     */
    public static class Factory implements TransformerFactory {
    	
    	private final int maxBodySize;
		private final boolean upsertOnly;
		private final OutputStreamWriter scriptFileWriter;
		private final Session session;
	    private final Configuration targetDBMSConfiguration;
		private ImportFilterTransformer importFilterTransformer;
		
		/**
		 * The command line arguments.
		 */
		private final CommandLine commandLine;
		
	    /**
	     * Constructor.
	     * 
	     * @param scriptFileWriter the file to write to
	     * @param maxBodySize maximum length of SQL values list (for generated inserts)
	     * @param upsertOnly use 'upsert' statements for all entities
	     */
		public Factory(OutputStreamWriter scriptFileWriter, boolean upsertOnly, int maxBodySize, Session session, Configuration targetDBMSConfiguration, CommandLine commandLine) {
	        this.maxBodySize = maxBodySize;
	        this.upsertOnly = upsertOnly;
	        this.scriptFileWriter = scriptFileWriter;
	        this.session = session;
	        this.targetDBMSConfiguration = targetDBMSConfiguration;
	        this.commandLine = commandLine;
    	}

		/**
		 * Creates transformer (as {@link ResultSetReader} which 
		 * transforms rows of a given table into an external representation.
		 * 
		 * @param table the table
		 * @return a transformer
		 */
		@Override
		public ResultSetReader create(Table table) throws SQLException {
			return new DMLTransformer(table, scriptFileWriter, upsertOnly, maxBodySize, session, targetDBMSConfiguration, importFilterTransformer, commandLine);
		}
		
		/**
		 * Sets the {@link ImportFilterTransformer}.
		 */
		public void setImportFilterTransformer(ImportFilterTransformer importFilterManager) {
			this.importFilterTransformer = importFilterManager;
		}

    };

	private final List<Column> selectionClause;
	
	/**
     * Constructor.
     * 
     * @param table the table to read from
     * @param scriptFileWriter the file to write to
     * @param maxBodySize maximum length of SQL values list (for generated inserts)
     * @param upsertOnly use 'upsert' statements for all entities
     * @param session the session
     * @param targetDBMSConfiguration configuration of the target DBMS
	 * @param commandLine 
	 * @param importFilterTransformer2 
     */
    protected DMLTransformer(Table table, OutputStreamWriter scriptFileWriter, boolean upsertOnly, int maxBodySize, Session session, Configuration targetDBMSConfiguration, ImportFilterTransformer importFilterTransformer, CommandLine commandLine) throws SQLException {
        this.commandLine = commandLine;
        this.targetDBMSConfiguration = targetDBMSConfiguration;
        this.maxBodySize = maxBodySize;
        this.upsertOnly = upsertOnly;
        this.table = table;
        this.scriptFileWriter = scriptFileWriter;
        this.currentDialect = targetDBMSConfiguration.getSqlDialect();
        this.insertStatementBuilder = new StatementBuilder(currentDialect.supportsMultiRowInserts || targetDBMSConfiguration.dbms == DBMS.ORACLE || targetDBMSConfiguration.dbms == DBMS.SQLITE? maxBodySize : 1);
        this.quoting = createQuoting(session);
        this.importFilterTransformer = importFilterTransformer;
        if (targetDBMSConfiguration != null && targetDBMSConfiguration != Configuration.forDbms(session)) {
        	if (targetDBMSConfiguration.getIdentifierQuoteString() != null) {
        		this.quoting.setIdentifierQuoteString(targetDBMSConfiguration.getIdentifierQuoteString());
        	}
        }
        this.session = session;
        this.selectionClause = table.getSelectionClause(session);
        tableHasIdentityColumn = false;
        if (targetDBMSConfiguration.isIdentityInserts()) {
        	for (Column c: table.getColumns()) {
        		if (c.isIdentityColumn) {
        			tableHasIdentityColumn = true;
        			break;
        		}
        	}
        }
    }

	protected Quoting createQuoting(Session session) throws SQLException {
		return new Quoting(session);
	}
    
    /**
     * Reads result-set and writes into export-script.
     */
    public void readCurrentRow(ResultSet resultSet) throws SQLException {
    	if (columnLabel == null) {
            columnCount = getMetaData(resultSet).getColumnCount();
            columnLabel = new String[columnCount + 1];
            lobColumns = new ArrayList<String>();
            isLobColumn = new boolean[columnCount + 1];
            emptyLobValue = new String[columnCount + 1];
            lobColumnIndexes = new ArrayList<Integer>();
            labelCSL = "";
            tableHasLobs = false;
            for (int i = 1; i <= columnCount; ++i) {
                String mdColumnLabel = quoting.quote(getMetaData(resultSet).getColumnLabel(i));
                int mdColumnType = getMetaData(resultSet).getColumnType(i);
                
                if ((mdColumnType == Types.BLOB || mdColumnType == Types.CLOB || mdColumnType == Types.NCLOB || mdColumnType == Types.SQLXML) && targetDBMSConfiguration.dbms != DBMS.SQLITE) {
                	tableHasLobs = true;
                	isLobColumn[i] = true;
                	lobColumnIndexes.add(i);
                	lobColumns.add(mdColumnLabel);
                	if (mdColumnType == Types.SQLXML) {
                		emptyLobValue[i] = null;
                	} else {
                		Configuration c = targetDBMSConfiguration;
                		emptyLobValue[i] = mdColumnType == Types.BLOB? c.emptyBLOBValue : mdColumnType == Types.CLOB? c.emptyCLOBValue : c.emptyNCLOBValue;
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
            Map<Integer, String> smallLobsPerIndex = new HashMap<Integer, String>();
            CellContentConverter cellContentConverter = getCellContentConverter(resultSet, session, targetDBMSConfiguration);
			for (int i = 1; i <= columnCount; ++i) {
				Object content = null;
				if (columnLabel[i] == null) {
					continue;
				}
				boolean isSmallLob = false;
				if (isLobColumn[i]) {
					content = cellContentConverter.getSmallLob(resultSet, i);
                	if (content != null) {
                		isSmallLob = true;
                		smallLobsPerIndex.put(i, (String) content);
                	}
                }
				if (content == null) {
	            	content = cellContentConverter.getObject(resultSet, i);
	                if (resultSet.wasNull()) {
	                    content = null;
	                }
				}
                if (!f) {
                	namedValues.append(", ");
                	valueList.append(", ");
                }
                f = false;
                String cVal = isSmallLob? (String) content : 
                	 convertToSql(cellContentConverter, resultSet, i, content);
                if (!isSmallLob && content != null && emptyLobValue[i] != null) {
            		cVal = emptyLobValue[i];
            	}
            	valueList.append(cVal);
                namedValues.append(cVal + " " + columnLabel[i]);
            }
            if (table.getUpsert() || upsertOnly) {
                if (table.primaryKey.getColumns().isEmpty()) {
                	throw new RuntimeException("Unable to merge/upsert into table \"" + table.getName() + "\".\n" +
                			"No primary key.");
                }

                Map<String, String> val = new HashMap<String, String>();
                StringBuffer valuesWONull = new StringBuffer("");
                StringBuffer columnsWONull = new StringBuffer("");
                f = true;
                for (int i = 1; i <= columnCount; ++i) {
                    if (columnLabel[i] == null) {
                    	continue;
                    }
                    Object content = cellContentConverter.getObject(resultSet, i);
                    if (resultSet.wasNull()) {
                        content = null;
                    }
                    String cVal = convertToSql(cellContentConverter, resultSet, i, content);
                    if (targetDBMSConfiguration.dbms == DBMS.POSTGRESQL && (content instanceof Date || content instanceof Timestamp)) {
                    	// explicit cast needed
                    	cVal = "timestamp " + cVal;
                    }
                    if (targetDBMSConfiguration.dbms == DBMS.POSTGRESQL) {
                    	// explicit cast needed
                    	int mdColumnType = getMetaData(resultSet).getColumnType(i);
                    	if (mdColumnType == Types.TIME) {
                    		cVal = "time " + cVal;
                    	}
                    }
                	if (content != null && emptyLobValue[i] != null) {
                		cVal = smallLobsPerIndex.get(i);
                		if (cVal == null) {
                			cVal = emptyLobValue[i];
                		}
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
                    whereForTerminator.append("T." + quoting.requote(pk.name) + "=Q." + quoting.requote(pk.name));
                    String value;
                    String name = quoting.quote(pk.name);
                    if (val.containsKey(name)) {
                    	value = val.get(name);
                    } else if (val.containsKey(name.toLowerCase())) {
                    	value = val.get(name.toLowerCase());
                    } else {
                    	value = val.get(name.toUpperCase());
                    }
                    where.append("T." + quoting.requote(pk.name) + "=" + value);
                    whereWOAlias.append(quoting.requote(pk.name) + "=" + value);
                }

                if (currentDialect.upsertMode == UPSERT_MODE.MERGE && !tableHasLobs) {
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
                } else if (currentDialect.upsertMode == UPSERT_MODE.DB2) {
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
                		(currentDialect.upsertMode == UPSERT_MODE.FROM_DUAL || 
                		 currentDialect.upsertMode == UPSERT_MODE.MERGE? // oracle table with lobs
                				 "dual" : currentDialect.upsertMode == UPSERT_MODE.FROM_SYSDUMMY1? "sysibm.sysdummy1" : SQLDialect.DUAL_TABLE);
                	StringBuffer terminator = new StringBuffer(" Where not exists (Select * from " + qualifiedTableName(table) + " T "
	                        + "Where ");
	                terminator.append(where + ");\n");
	                
	                StatementBuilder sb = upsertInsertStatementBuilder.get(insertHead);
	                if (sb == null) {
	                    sb = new StatementBuilder(1 /* insertStatementBuilder.getMaxBodySize() */);
	                    upsertInsertStatementBuilder.put(insertHead, sb);
	                }
	            
	                if (!sb.isAppendable(insertHead, item)) {
	                    writeToScriptFile(sb.build(), true);
	                }
	                sb.append(insertHead, item, ", ", terminator.toString());
                }
                
                if (currentDialect.upsertMode != UPSERT_MODE.MERGE || tableHasLobs) {
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
            	if (targetDBMSConfiguration.dbms == DBMS.DB2_ZOS && maxBodySize > 1) {
            		String insertSchema = "Insert into " + qualifiedTableName(table) + "(" + labelCSL + ") ";
	                String item = "\n Select " + valueList + " From sysibm.sysdummy1";
	                if (!insertStatementBuilder.isAppendable(insertSchema, item)) {
	                    writeToScriptFile(insertStatementBuilder.build(), true);
	                }
	                insertStatementBuilder.append(insertSchema, item, " Union all ", ";\n");
            	} else if (targetDBMSConfiguration.dbms == DBMS.ORACLE && maxBodySize > 1) {
            		String insertSchema = "Insert into " + qualifiedTableName(table) + "(" + labelCSL + ") ";
	                String item = "\n Select " + valueList + " From DUAL";
	                if (!insertStatementBuilder.isAppendable(insertSchema, item)) {
	                    writeToScriptFile(insertStatementBuilder.build(), true);
	                }
	                insertStatementBuilder.append(insertSchema, item, " Union all ", ";\n");
            	} else if (targetDBMSConfiguration.dbms == DBMS.SQLITE && maxBodySize > 1) {
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
            
            exportLobs(table, resultSet, smallLobsPerIndex.keySet());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Converts cell content to SQL literals.
     * 
     * @param cellContentConverter converter
     * @param resultSet points to current row
     * @param i current result set index
     * @param content cell content
     * @return SQL literal
     */
    protected String convertToSql(CellContentConverter cellContentConverter, ResultSet resultSet, int i, Object content) throws SQLException {
    	String cVal = cellContentConverter.toSql(content);
    	Column column = selectionClause.get(i - 1);
		Filter filter = column.getFilter();
		
    	if (filter != null && importFilterTransformer != null) {
    		if (!filter.isApplyAtExport()) {
    			return importFilterTransformer.transform(column, cVal);
    		}
    	}
    	
		if (content != null && filter != null && filter.getExpression().trim().startsWith(Filter.LITERAL_PREFIX)) {
			return content.toString();
		}

		return cVal;
	}

	/**
     * Gets qualified table name.
     * 
     * @param t the table
     * @return qualified name of t
     */
    protected String qualifiedTableName(Table t) {
    	String schema = t.getOriginalSchema("");
    	String mappedSchema = commandLine.getSchemaMapping().get(schema);
    	if (mappedSchema != null) {
    		schema = mappedSchema;
    	}
    	if (schema.length() == 0) {
    		return quoting.requote(t.getUnqualifiedName());
    	}
		return quoting.requote(schema) + "." + quoting.requote(t.getUnqualifiedName());
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
    private void exportLobs(Table table, ResultSet resultSet, Set<Integer> smallLobsIndexes) throws IOException, SQLException {
    	synchronized (scriptFileWriter) {
            CellContentConverter cellContentConverter = getCellContentConverter(resultSet, session, targetDBMSConfiguration);
	    	for (int i = 0; i < lobColumnIndexes.size(); ++i) {
	    		if (smallLobsIndexes.contains(lobColumnIndexes.get(i))) {
	    			continue;
	    		}
				Object lob = resultSet.getObject(lobColumnIndexes.get(i));
				Map<String, String> val = new HashMap<String, String>();
				for (int j = 1; j <= columnCount; ++j) {
	                if (columnLabel[j] == null) {
	                	continue;
	                }
					Object content = cellContentConverter.getObject(resultSet, j);
	                if (resultSet.wasNull()) {
	                    content = null;
	                }
	                String cVal = cellContentConverter.toSql(content);
	                val.put(columnLabel[j], cVal);
	            }
				boolean f = true;
	            StringBuffer where = new StringBuffer("");
	            for (Column pk: table.primaryKey.getColumns()) {
	            	if (!f) {
	                    where.append(" and ");
	                }
	                f = false;
	                where.append(quoting.requote(pk.name) + "=" + val.get(pk.name));
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
        	if (wrap && targetDBMSConfiguration.dbms == DBMS.ORACLE) {
       			scriptFileWriter.write(SqlUtil.splitDMLStatement(content, 2400));
        	} else {
        		scriptFileWriter.write(content);
        	}
        }
    }
    
}
