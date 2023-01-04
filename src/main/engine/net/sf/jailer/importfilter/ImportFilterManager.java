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
package net.sf.jailer.importfilter;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Matcher;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.configuration.LocalDatabaseConfiguration;
import net.sf.jailer.database.DMLTransformer;
import net.sf.jailer.database.ImportFilterTransformer;
import net.sf.jailer.database.LocalDatabase;
import net.sf.jailer.database.Session;
import net.sf.jailer.database.Session.ResultSetReader;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.Filter;
import net.sf.jailer.datamodel.PKColumnFilterSource;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.entitygraph.EntityGraph;
import net.sf.jailer.util.CancellationException;
import net.sf.jailer.util.CellContentConverter;
import net.sf.jailer.util.JobManager;
import net.sf.jailer.util.JobManager.Job;
import net.sf.jailer.util.Pair;
import net.sf.jailer.util.PrintUtil;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlScriptExecutor;


/**
 * Manages filtering of data during import.
 * 
 * @author Ralf Wisser
 */
public abstract class ImportFilterManager implements ImportFilterTransformer {
	
	private final Set<Table> totalProgress;

	private Session theLocalSession;
	private LocalDatabase theLocalDatabase;
	
	private synchronized Session getLocalSession() {
		if (theLocalSession == null) {
			LocalDatabaseConfiguration localConfiguration = Configuration.getInstance().localEntityGraphConfiguration;
			try {
				this.theLocalDatabase = new LocalDatabase(localConfiguration.getDriver(), localConfiguration.getUrlPattern(), localConfiguration.getUser(), localConfiguration.getPassword(), localConfiguration.getLib(), executionContext);
			} catch (ClassNotFoundException e) {
				throw new RuntimeException(e);
			} catch (FileNotFoundException e) {
				throw new RuntimeException(e);
			} catch (SQLException e) {
				throw new RuntimeException(e);
			}
			theLocalSession = theLocalDatabase.getSession();
		}
		return theLocalSession;
	}
	
	private final Map<Table, List<Column>> nonderivedFilteredColumnsPerTable = new HashMap<Table, List<Column>>();
	
	private class ColumnToMappingTable {
		public String mappingTableName;
		public String oldValueColumnName;
		public String newValueColumnName;
		public String type;
		public String filterExpression;
	}
	
	public static String MAPPINGTABLE_NAME_PREFIX = "JAILER_IFM";
	
	private IdentityHashMap<Column, ColumnToMappingTable> columnMapping = new IdentityHashMap<Column, ColumnToMappingTable>();
	private Set<String> mappingTables = new TreeSet<String>();
	private Set<ColumnToMappingTable> columnToMappingTableSet = new HashSet<ColumnToMappingTable>();
	private final String quotedMappingTablesSchema;
	
	/**
	 * The execution context.
	 */
	private final ExecutionContext executionContext;
	
	/**
	 * Constructor.
	 * 
	 * @param result to write results into
	 * @param targetQuoting
	 */
	public ImportFilterManager(Session localSession, OutputStreamWriter result, Set<Table> progress, Quoting targetQuoting, ExecutionContext executionContext) throws FileNotFoundException, SQLException {
		this.executionContext = executionContext;
		this.totalProgress = progress;
		
		String mappingTablesSchema = executionContext.getImportFilterMappingTableSchema().trim();
		quotedMappingTablesSchema = mappingTablesSchema.length() > 0? targetQuoting.requote(mappingTablesSchema) + "." : "";

		if (localSession != null) {
			this.theLocalSession = localSession;
		}
		collectNonderivedFilteredColumnsPerTable();
	}
	
	private void collectNonderivedFilteredColumnsPerTable() {
		for (Table table: totalProgress) {
			List<Column> filteredColumns = new ArrayList<Column>();
			for (Column column: table.getColumns()) {
				Filter filter = column.getFilter();

				if (filter != null && !filter.isApplyAtExport()) {
					boolean derivedFromOutside = false;
					if (filter.getFilterSource() instanceof PKColumnFilterSource) {
						if (!totalProgress.contains(((PKColumnFilterSource) filter.getFilterSource()).table)) {
							derivedFromOutside = true;
						}
					}
					if (derivedFromOutside || (!(filter.getFilterSource() instanceof PKColumnFilterSource))) {
						filteredColumns.add(column);
					}
				}
			}
			if (!filteredColumns.isEmpty()) {
				nonderivedFilteredColumnsPerTable.put(table, filteredColumns);
			}
		}
	}

	/**
	 * Creates the DDL for the mapping tables. 
	 */
	public void createMappingTables(DBMS configuration, OutputStreamWriter result) throws IOException, SQLException {
		if (nonderivedFilteredColumnsPerTable.isEmpty()) {
			return;
		}
		Map<String, String> typeReplacement = configuration.getTypeReplacement();
		Map<Pair<String, String>, List<Column>> partition = new HashMap<Pair<String, String>, List<Column>>();
		for (Entry<Table, List<Column>> e: nonderivedFilteredColumnsPerTable.entrySet()) {
			for (Column column: e.getValue()) {
				Pair<String, String> partKey = new Pair<String, String>(column.getFilter().getType() != null? column.getFilter().getType() : column.toSQL(null, typeReplacement).substring(column.name.length()).trim(), column.getFilter().getExpression());
				List<Column> part = partition.get(partKey);
				if (part == null) {
					part = new ArrayList<Column>();
					partition.put(partKey, part);
				}
				part.add(column);
			}
		}
		
		File tmpFile = Configuration.getInstance().createTempFile();
		OutputStreamWriter localDDL = new OutputStreamWriter(new FileOutputStream(tmpFile));
		
		List<ColumnToMappingTable> mapColumns = new ArrayList<ColumnToMappingTable>();
		List<ColumnToMappingTable> mapColumnsLocal = new ArrayList<ColumnToMappingTable>();
		int mapTableIndex = 0;
		LocalDatabaseConfiguration localConfiguration = Configuration.getInstance().localEntityGraphConfiguration;
		
		String schema = quotedMappingTablesSchema;
		
		int maxColumnsPerMappingTable = Configuration.getInstance().getColumnsPerIFMTable();

		for (Entry<Pair<String, String>, List<Column>> part: partition.entrySet()) {
			ColumnToMappingTable columnToMappingTableRemote = new ColumnToMappingTable();
			columnToMappingTableRemote.mappingTableName = MAPPINGTABLE_NAME_PREFIX + (mapTableIndex > 0? Integer.toString(mapTableIndex) : "");
			columnToMappingTableRemote.newValueColumnName = "NEW_VALUE" + (1 + mapColumns.size());
			columnToMappingTableRemote.oldValueColumnName = "OLD_VALUE" + (1 + mapColumns.size());
			columnToMappingTableRemote.type = part.getKey().a;
			columnToMappingTableRemote.filterExpression = part.getKey().b;
			mappingTables.add(columnToMappingTableRemote.mappingTableName);
			ColumnToMappingTable columnToMappingTableLocal = new ColumnToMappingTable();
			columnToMappingTableLocal.mappingTableName = columnToMappingTableRemote.mappingTableName;
			columnToMappingTableLocal.newValueColumnName = null;
			columnToMappingTableLocal.oldValueColumnName = columnToMappingTableRemote.oldValueColumnName;
			columnToMappingTableLocal.type = localConfiguration.getLocalPKType() + "(" + localConfiguration.getLocalPKLength() + ")";
			columnToMappingTableSet .add(columnToMappingTableRemote);
			for (Column c: part.getValue()) {
				columnMapping.put(c, columnToMappingTableRemote);
			}
			mapColumns.add(columnToMappingTableRemote);
			mapColumnsLocal.add(columnToMappingTableLocal);
			if (mapColumns.size() == maxColumnsPerMappingTable) {
				result.append(createDDL(schema, mapColumns, configuration, mapTableIndex == 0));
				localDDL.append(createDDL("", mapColumnsLocal, getLocalSession().dbms, mapTableIndex == 0));
				mapColumns.clear();
				mapColumnsLocal.clear();
				++mapTableIndex;
			}
		}
		if (mapColumns.size() > 0) {
			result.append(createDDL(schema, mapColumns, configuration, mapTableIndex == 0));
			localDDL.append(createDDL("", mapColumnsLocal, getLocalSession().dbms, mapTableIndex == 0));
			sync(result);
		}
		localDDL.close();
		new SqlScriptExecutor(getLocalSession(), 1).executeScript(tmpFile.getAbsolutePath());
		tmpFile.delete();
	}

	private String createDDL(String schema, List<ColumnToMappingTable> mapColumns, DBMS configuration, boolean withComment) throws FileNotFoundException, IOException {
		String template = "script" + File.separator + "imp-filter-map-ddl.sql";
		String nullableContraint = configuration.getNullableContraint();
		String contraint;
		if (nullableContraint != null) {
			contraint = " " + nullableContraint;
		} else {
			contraint = "";
		}
		Map<String, String> arguments = new HashMap<String, String>();
		arguments.put("constraint", contraint);
		
		String suffix = "";
		String prefix = "";
		String tableProperties = configuration.getTableProperties();
		prefix = tableProperties.replaceFirst("^\\s*CREATE\\s+(.*)\\s+TABLE\\s*$", "$1");
		if (prefix.equals(tableProperties)) {
			prefix = "";
			suffix = tableProperties;
		}
		
		String tableName = mapColumns.get(0).mappingTableName;
		arguments.put("mapping-table", tableName);
		arguments.put("schema", schema);
		arguments.put("index-schema", Boolean.TRUE.equals(configuration.getSupportsSchemasInIndexDefinitions())? schema : "");
		arguments.put("table-suffix", "");
		arguments.put("drop-table", "DROP TABLE ");
		arguments.put("create-table", !"".equals(prefix)? "CREATE " + prefix + " TABLE " : "CREATE TABLE ");
		arguments.put("create-table-suffix", suffix);
		arguments.put("create-index", "CREATE INDEX ");
		arguments.put("create-index-suffix", "");
		arguments.put("index-table-prefix", "");
		
		StringBuilder values = new StringBuilder();
		List<String> indexedColumns = new ArrayList<String>();
		for (ColumnToMappingTable col: mapColumns) {
			indexedColumns.add(col.oldValueColumnName);
			if (values.length() > 0) {
				values.append(", ");
			}
			values.append(col.oldValueColumnName + " " + col.type);
			if (col.newValueColumnName != null) {
				if (values.length() > 0) {
					values.append(", ");
				}
				values.append(col.newValueColumnName + " " + col.type + contraint);
			}
		}
		arguments.put("old-and-new-values", values.toString());
		Map<String, List<String>> listArguments = new HashMap<String, List<String>>();
		listArguments.put("indexed-columns", indexedColumns);
		listArguments.put("comments", withComment? Collections.singletonList("dummy") : new ArrayList<String>());
		return new PrintUtil().applyTemplate(template, arguments, listArguments);
	}

	/**
	 * Creates the DROP-statements for the mapping tables.
	 */
	public void dropMappingTables(OutputStreamWriter result) throws IOException, SQLException {
		if (mappingTables.isEmpty()) {
			return;
		}
		
		String template = "script" + File.separator + "imp-filter-map-drop.sql";
		Map<String, String> arguments = new HashMap<String, String>();

		String schema = quotedMappingTablesSchema;
		arguments.put("schema", schema);
		arguments.put("table-suffix", "");
		arguments.put("drop-table", "DROP TABLE ");
		
		Map<String, List<String>> listArguments = new HashMap<String, List<String>>();
		listArguments.put("mapping-tables", new ArrayList<String>(mappingTables));
		sync(result);
		result.append(new PrintUtil().applyTemplate(template, arguments, listArguments));
	}
	
	protected abstract void sync(OutputStreamWriter result) throws IOException;

	public synchronized void shutDown() throws SQLException {
		if (this.theLocalDatabase != null) {
			this.theLocalDatabase.shutDown();
			this.theLocalDatabase = null;
		}
	}

	/**
	 * Insert the values of columns with non-derived-import-filters into the local database.
	 * 
	 * @param entityGraph
	 * @param jobManager
	 * @param dmlResultWriter
	 */
	public void fillAndWriteMappingTables(final EntityGraph entityGraph, JobManager jobManager, final OutputStreamWriter dmlResultWriter,
			int numberOfEntities, final Session targetSession, final DBMS targetDBMSConfiguration) throws CancellationException, SQLException {
		executionContext.getProgressListenerRegistry().fireNewStage("processing import filters", false, false);
		
		Collection<Job> insertJobs = new ArrayList<Job>();
		Collection<Job> writeOutJobs = new ArrayList<Job>();
		
		for (final Entry<Table, List<Column>> filters: nonderivedFilteredColumnsPerTable.entrySet()) {
			final Table table = filters.getKey();
			insertJobs.add(new Job() {
				@Override
				public void run() throws SQLException {
					final Map<Column, PreparedStatement> insertStatement = new HashMap<Column, PreparedStatement>();
					Connection connection;
					connection = getLocalSession().getConnection();
					final List<Column> columns = filters.getValue();
					for (Column column: columns) {
						insertStatement.put(column, connection.prepareStatement( // lgtm [java/database-resource-leak]S
								"Insert into " + columnMapping.get(column).mappingTableName
								+ "(" + columnMapping.get(column).oldValueColumnName + ")"
								+ " values (?)"));
					}
					
					try {
						entityGraph.readUnfilteredEntityColumns(table, columns, new Session.AbstractResultSetReader() {
							private int batchSize[] = new int[columns.size()];
							private final int MAX_BATCH_SIZE = 1000;
							@Override
							public void readCurrentRow(ResultSet resultSet) throws SQLException {
								CellContentConverter cellContentConverter = getCellContentConverter(resultSet, entityGraph.getSession(), entityGraph.getSession().dbms);
								for (int i = 0; i < columns.size(); ++i) {
									Object content = cellContentConverter.getObject(resultSet, i + 1);
									if (content != null) {
										String value = cellContentConverter.toSql(content);
										insertStatement.get(columns.get(i)).setString(1, value);
										insertStatement.get(columns.get(i)).addBatch();
										++batchSize[i];
										if (batchSize[i] > MAX_BATCH_SIZE) {
											insertStatement.get(columns.get(i)).executeBatch();
											batchSize[i] = 0;
										}
									}
								}
							}
							
							@Override
							public void close() throws SQLException {
								for (int i = 0; i < columns.size(); ++i) {
									if (batchSize[i] > 0) {
										insertStatement.get(columns.get(i)).executeBatch();
										batchSize[i] = 0;
									}
								}
							}
						});
					} finally {
						for (PreparedStatement st: insertStatement.values()) {
							try {
								st.close();
							} catch (SQLException e) {
								// ignore
							}
						}
					}
				}
			});
		}

		final String schema = quotedMappingTablesSchema;
		AtomicReference<Table> identityInsertTable = new AtomicReference<Table>();
		
		for (final ColumnToMappingTable mapping: columnToMappingTableSet) {
			writeOutJobs.add(new Job() {
				@Override
				public void run() throws SQLException {
					Table mappingTable = new Table(schema + mapping.mappingTableName, null, false, false);
					Column newValueColumn = new Column(mapping.newValueColumnName, mapping.type, 0, -1);
					Column oldValueColumn = new Column(mapping.oldValueColumnName, mapping.type, 0, -1);
					mappingTable.setColumns(Arrays.asList(oldValueColumn, newValueColumn));
					ResultSetReader scriptFileWriter = new DMLTransformer(mappingTable, dmlResultWriter, false, 1, targetSession, targetDBMSConfiguration, null, identityInsertTable, executionContext) {
						@Override
						protected String convertToSql(CellContentConverter cellContentConverter,
								ResultSet resultSet, int i, Object content, int callerId, String suffix) throws SQLException {
							if (i == 1) {
								// old value
								String contentAsString = content != null? content.toString() : null;
								return contentAsString;
							} else {
								// new value
								String oldValue = resultSet.getString(1);
								return mapping.filterExpression.replaceAll(Filter.OLD_VALUE_PROP_RE, Matcher.quoteReplacement(oldValue));
							}
						}
						@Override
						protected String qualifiedTableName(Table t) {
							return schema + quoting.requote(t.getUnqualifiedName());
						}
						@Override
						protected Quoting createQuoting(Session session) throws SQLException {
							return new Quoting(session) {
								@Override
								public String quote(String identifier) {
									return identifier;
								}
								@Override
								public boolean isQuoted(String identifier) {
									return false;
								}
								@Override
								public String unquote(String identifier) {
									return identifier;
								}
								@Override
								public String requote(String identifier) {
									return identifier;
								}
								
							};
						}
					};
					String query = "Select distinct " + mapping.oldValueColumnName + ", 0 as " + mapping.newValueColumnName
							+ " From " + mapping.mappingTableName
							+ " Where " + mapping.oldValueColumnName + " is not null";
					getLocalSession().executeQuery(query, scriptFileWriter);
					scriptFileWriter.close();
				}
			});
		}

		jobManager.executeJobs(insertJobs);
		jobManager.executeJobs(writeOutJobs);
	}

	@Override
	public String transform(Column column, String oldValue) {
		Filter filter = column.getFilter();
		if (filter.getFilterSource() instanceof PKColumnFilterSource) {
			boolean derivedFromOutside = false;
			if (!totalProgress.contains(((PKColumnFilterSource) filter.getFilterSource()).table)) {
				derivedFromOutside = true;
			}
			if (!derivedFromOutside) {
				column = ((PKColumnFilterSource) filter.getFilterSource()).column;
			}
		}
		ColumnToMappingTable mapping = columnMapping.get(column);

		return "(Select " + mapping.newValueColumnName + " From " + quotedMappingTablesSchema + mapping.mappingTableName + " Where " + mapping.oldValueColumnName + " = " + oldValue + ")";
	}

}
