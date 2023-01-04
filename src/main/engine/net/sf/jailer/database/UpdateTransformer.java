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

package net.sf.jailer.database;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.Session.AbstractResultSetReader;
import net.sf.jailer.database.Session.ResultSetReader;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Filter;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.CellContentConverter;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlUtil;

/**
 * A {@link ResultSetReader} that writes the read rows as UPDATE statements 
 * into the export-script.
 * 
 * @author Ralf Wisser
 */
public class UpdateTransformer extends AbstractResultSetReader {

	/**
	 * The table to read from.
	 */
	private final Table table;
	
	/**
	 * The columns to update.
	 */
	private final Set<Column> columns;
	
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
	 * For building compact insert-parts of upsert-statements.
	 */
	private Map<String, StatementBuilder> upsertInsertStatementBuilder = new HashMap<String, StatementBuilder>();

	/**
	 * Maximum length of SQL values list (for generated inserts).
	 */
	private final int maxBodySize;
	
	/**
	 * For quoting of column names.
	 */
	private final Quoting quoting;
	
	/**
	 * Current session;
	 */
	private final Session session;

	/**
	 * Configuration of the target DBMS.
	 */
	private final DBMS targetDBMSConfiguration;

	/**
	 * SQL Dialect.
	 */
	private final SQLDialect currentDialect;
	
	/**
	 * Transforms {@link Filter} into SQL-expressions.
	 */
	private final ImportFilterTransformer importFilterTransformer;
	
	/**
	 * The execution context.
	 */
	private final ExecutionContext executionContext;

	/**
	 * If <code>true</code>, use source-schema-mapping, else use schema-mapping.
	 */
	private final boolean inSourceSchema;

	/**
	 * To be written as comment.
	 */
	private String reason;

	private final Set<String> primaryKeyColumnNames;

	/**
	 * Constructor.
	 * 
	 * @param table the table to read from
	 * @param scriptFileWriter the file to write to
	 * @param maxBodySize maximum length of SQL values list (for generated inserts)
	 * @param session the session
	 * @param targetDBMSConfiguration configuration of the target DBMS
	 * @param inSourceSchema if <code>true</code>, use source-schema-mapping, else use schema-mapping
	 * @param reason to be written as comment
	 */
	public UpdateTransformer(Table table, Set<Column> columns, OutputStreamWriter scriptFileWriter, int maxBodySize, Session session, DBMS targetDBMSConfiguration, ImportFilterTransformer importFilterTransformer, boolean inSourceSchema, String reason, ExecutionContext executionContext) throws SQLException {
		this.executionContext = executionContext;
		this.targetDBMSConfiguration = targetDBMSConfiguration;
		this.maxBodySize = maxBodySize;
		this.table = table;
		this.columns = columns;
		this.scriptFileWriter = scriptFileWriter;
		this.currentDialect = targetDBMSConfiguration.getSqlDialect();
		this.quoting = Quoting.getQuoting(session);
		this.importFilterTransformer = importFilterTransformer;
		this.inSourceSchema = inSourceSchema;
		this.reason = reason;
		if (targetDBMSConfiguration != session.dbms) {
			if (targetDBMSConfiguration.getIdentifierQuoteString() != null) {
				this.quoting.setIdentifierQuoteString(targetDBMSConfiguration.getIdentifierQuoteString());
			}
		}
		this.session = session;
		selectionClause = table.getSelectionClause();
		this.primaryKeyColumnNames = new HashSet<String>();
		for (Column c: table.getNonVirtualPKColumns(session)) {
			this.primaryKeyColumnNames.add(c.name.toUpperCase(Locale.ENGLISH));
		}
	}

	private final List<Column> selectionClause;

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

	private Set<String> columnNamesLower = new HashSet<String>();
	
	/**
	 * Reads result-set and writes into export-script.
	 */
	@Override
	public void readCurrentRow(ResultSet resultSet) throws SQLException {
		if (columnLabel == null) {
			columnCount = getMetaData(resultSet).getColumnCount();
			columnLabel = new String[columnCount + 1];
			labelCSL = "";
			for (int i = 1; i <= columnCount; ++i) {
				String mdColumnLabel = SqlUtil.columnLabel(quoting, session, targetDBMSConfiguration, table, getMetaData(resultSet).getColumnLabel(i));
				
				columnLabel[i] = mdColumnLabel;
				if (labelCSL.length() > 0) {
					labelCSL += ", ";
				}
				labelCSL += columnLabel[i];
			}
			for (Column column: columns) {
				columnNamesLower.add(column.name.toLowerCase(Locale.ENGLISH));
			}
		}
		try {
			StringBuffer valueList = new StringBuffer("");
			StringBuffer namedValues = new StringBuffer("");
			boolean f = true;
			CellContentConverter cellContentConverter = getCellContentConverter(resultSet, session, targetDBMSConfiguration);
			for (int i = 1; i <= columnCount; ++i) {
				Object content = null;
				if (columnLabel[i] == null) {
					continue;
				}
				if (!isPrimaryKeyColumn(columnLabel[i]) && !columnNamesLower.contains(columnLabel[i].toLowerCase(Locale.ENGLISH))) {
					continue;
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
				String cVal = convertToSql(cellContentConverter, resultSet, i, content);
				valueList.append(cVal);
				namedValues.append(cVal + " " + columnLabel[i]);
			}

			if (table.getNonVirtualPKColumns(session).isEmpty()) {
				throw new DataModel.NoPrimaryKeyException(table);
			}

			Map<String, String> val = new HashMap<String, String>();
			Map<String, Boolean> valIsNull = new HashMap<String, Boolean>();
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
				if (DBMS.POSTGRESQL.equals(targetDBMSConfiguration)) {
					// explicit cast needed
					int mdColumnType = getMetaData(resultSet).getColumnType(i);
					if (mdColumnType == Types.TIME) {
						cVal = "time " + cVal;
					}
				}
				valIsNull.put(columnLabel[i], content == null);
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
			
			f = true;
			StringBuffer whereForTerminator = new StringBuffer("");
			StringBuffer whereForTerminatorWONull = new StringBuffer("");
			StringBuffer where = new StringBuffer("");
			StringBuffer whereWOAlias = new StringBuffer("");
			
			// assemble 'where' for sub-select and update
			for (Column pk: table.getNonVirtualPKColumns(session)) {
				if (!f) {
					whereForTerminator.append(" and ");
					whereForTerminatorWONull.append(" and ");
					where.append(" and ");
					whereWOAlias.append(" and ");
				}
				f = false;
				String value;
				Boolean isNull;
				String name = quoting.quote(pk.name);
				if (val.containsKey(name)) {
					value = val.get(name);
					isNull = valIsNull.get(name);
				} else if (val.containsKey(name.toLowerCase(Locale.ENGLISH))) {
					value = val.get(name.toLowerCase(Locale.ENGLISH));
					isNull = valIsNull.get(name.toLowerCase(Locale.ENGLISH));
				} else {
					value = val.get(name.toUpperCase(Locale.ENGLISH));
					isNull = valIsNull.get(name.toUpperCase(Locale.ENGLISH));
				}
				String op = Boolean.TRUE.equals(isNull)? " is null" : ("=" + value);
				where.append("T." + quoting.requote(pk.name) + op);
				whereWOAlias.append(quoting.requote(pk.name) + op);
				if (pk.isNullable) {
					whereForTerminator.append("(T." + quoting.requote(pk.name) + "=Q." + quoting.requote(pk.name));
					whereForTerminator.append(" or (T." + quoting.requote(pk.name) + " is null and Q." + quoting.requote(pk.name) + " is null))");
				} else {
					whereForTerminator.append("T." + quoting.requote(pk.name) + "=Q." + quoting.requote(pk.name));
				}
				whereForTerminatorWONull.append("T." + quoting.requote(pk.name) + (Boolean.TRUE.equals(isNull)? " is null" : ("=Q." + quoting.requote(pk.name))));
			}

			if (currentDialect.getUpsertMode() == UPSERT_MODE.MERGE) {
				// MERGE INTO JL_TMP T USING (SELECT 1 c1, 2 c2 from dual) incoming 
				// ON (T.c1 = incoming.c1) 
				// WHEN MATCHED THEN UPDATE SET T.c2 = incoming.c2 
				String insertHead = "MERGE INTO " + qualifiedTableName(table) + " T USING(";
				StringBuffer terminator = new StringBuffer(") Q ON(" + whereForTerminator + ") ");
				
				StringBuffer sets = new StringBuffer();
				StringBuffer tSchema = new StringBuffer();
				StringBuffer iSchema = new StringBuffer();
				for (int i = 1; i <= columnCount; ++i) {
					if (columnLabel[i] == null) {
						continue;
					}
					if (!isPrimaryKeyColumn(columnLabel[i])) {
						if (columnNamesLower.contains(columnLabel[i].toLowerCase(Locale.ENGLISH))) {
							if (sets.length() > 0) {
								sets.append(", ");
							}
							sets.append("T." + columnLabel[i] + "=Q." + columnLabel[i]);
						}
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
					terminator.append("WHEN MATCHED THEN UPDATE SET " + sets +  ";\n");
				}
				
				StatementBuilder sb = upsertInsertStatementBuilder.get(insertHead);
				if (sb == null) {
					sb = new StatementBuilder(maxBodySize);
					upsertInsertStatementBuilder.put(insertHead, sb);
				}
			
				String item = "Select " + valueList + " from dual";
				if (!sb.isAppendable(insertHead)) {
					writeToScriptFile(sb.build(), true);
				}
				if (sb.isEmpty()) {
					item = "Select " + namedValues + " from dual";
				}
				sb.append(insertHead, item, " UNION ALL ", terminator.toString());
			}
			
			if (currentDialect.getUpsertMode() != UPSERT_MODE.MERGE) {
				StringBuffer insert = new StringBuffer("");
				insert.append("Update " + qualifiedTableName(table) + " set ");
				f = true;
				for (int i = 1; i <= columnCount; ++i) {
					if (!columnNamesLower.contains(columnLabel[i].toLowerCase(Locale.ENGLISH))) {
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
		String mappedSchema = inSourceSchema? executionContext.getDeletionSchemaMapping().get(schema) : executionContext.getSchemaMapping().get(schema);
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
		return primaryKeyColumnNames.contains(column.toUpperCase(Locale.ENGLISH));
	}

	/**
	 * Flushes the export-reader.
	 */
	public void flush() {
		try {
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
	@Override
	public void close() {
		flush();
	}
	
	/**
	 * Writes into script.
	 */
	private void writeToScriptFile(String content, boolean wrap) throws IOException {
		synchronized (scriptFileWriter) {
			if (reason != null) {
				scriptFileWriter.write("-- " + reason + "\n");
				reason = null;
			}
			if (wrap && DBMS.ORACLE.equals(targetDBMSConfiguration)) {
				   scriptFileWriter.write(SqlUtil.splitDMLStatement(content, 2400));
			} else {
				scriptFileWriter.write(content);
			}
		}
	}
	
}
