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

package net.sf.jailer.datamodel;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.JailerVersion;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.filter_template.Clause;
import net.sf.jailer.datamodel.filter_template.FilterTemplate;
import net.sf.jailer.extractionmodel.ExtractionModel;
import net.sf.jailer.extractionmodel.ExtractionModel.AdditionalSubject;
import net.sf.jailer.extractionmodel.SubjectLimitDefinition;
import net.sf.jailer.modelbuilder.KnownIdentifierMap;
import net.sf.jailer.restrictionmodel.RestrictionModel;
import net.sf.jailer.subsetting.ScriptFormat;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.CsvFile.LineFilter;
import net.sf.jailer.util.PrintUtil;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlUtil;

/**
 * Relational data model.
 *
 * @author Ralf Wisser
 */
public class DataModel {

	public static final String TABLE_CSV_FILE = "table.csv";
	public static final String MODELNAME_CSV_FILE = "modelname.csv";

	/**
	 * Maps table-names to tables.
	 */
	private Map<String, Table> tables = new HashMap<String, Table>();

	/**
	 * Maps table display names to tables.
	 */
	private Map<String, Table> tablesByDisplayName = new HashMap<String, Table>();

	/**
	 * Maps tables to display names.
	 */
	private Map<Table, String> displayName = new HashMap<Table, String>();

	/**
	 * Maps association-names to associations;
	 */
	public Map<String, Association> namedAssociations = new TreeMap<String, Association>();

	/**
	 * Set of names of associations for which no decision has been made.
	 */
	public Set<String> decisionPending = new HashSet<String>();

	/**
	 * The restriction model.
	 */
	private RestrictionModel restrictionModel;

	/**
	 * Order priority of a column.
	 */
	public enum ColumnOrderPriority {
		HI, LO
	}

	/**
	 * Maps normalized column name to order priority.
	 */
	public final Map<String, ColumnOrderPriority> columnOrderPrio = new TreeMap<String, ColumnOrderPriority>();

	/**
	 * Internal version number. Incremented on each modification.
	 */
	public long version = 0;

	/**
	 * The execution context.
	 */
	private final ExecutionContext executionContext;

	/**
	 * Default model name.
	 */
	public static final String DEFAULT_NAME = "New Model";

	/**
	 * For creation of primary-keys.
	 */
	private final PrimaryKeyFactory primaryKeyFactory;

	/**
	 * Gets name of data model folder.
	 */
	public static String getDatamodelFolder(ExecutionContext executionContext) {
		return executionContext.getQualifiedDatamodelFolder();
	}

	/**
	 * Gets name of file containing the table definitions.
	 */
	public static String getTablesFile(ExecutionContext executionContext) {
		return getDatamodelFolder(executionContext) + File.separator + TABLE_CSV_FILE;
	}

	/**
	 * Gets name of file containing the model name
	 */
	public static String getModelNameFile(ExecutionContext executionContext) {
		return getDatamodelFolder(executionContext) + File.separator + MODELNAME_CSV_FILE;
	}

	/**
	 * Gets name of file containing the display names.
	 */
	public static String getDisplayNamesFile(ExecutionContext executionContext) {
		return getDatamodelFolder(executionContext) + File.separator + "displayname.csv";
	}

	/**
	 * Gets name of file containing the column definitions.
	 */
	public static String getColumnsFile(ExecutionContext executionContext) {
		return getDatamodelFolder(executionContext) + File.separator + "column.csv";
	}

	/**
	 * Gets name of file containing the association definitions.
	 */
	public static String getAssociationsFile(ExecutionContext executionContext) {
		return getDatamodelFolder(executionContext) + File.separator + "association.csv";
	}

	/**
	 * Gets name of file containing the column order definitions.
	 */
	private static String getColumnOrderFile(ExecutionContext executionContext) {
		return getDatamodelFolder(executionContext) + File.separator + "columnorder.csv";
	}

	/**
	 * Gets name of file containing the comments.
	 */
	public static String getCommentsFile(ExecutionContext executionContext) {
		return getDatamodelFolder(executionContext) + File.separator + "comment.csv";
	}

	/**
	 * List of tables to be excluded from deletion.
	 */
	public static String getExcludeFromDeletionFile(ExecutionContext executionContext) {
		return getDatamodelFolder(executionContext) + File.separator + "exclude-from-deletion.csv";
	}

	/**
	 * Name of file containing the version number.
	 */
	public static String getVersionFile(ExecutionContext executionContext) {
		return getDatamodelFolder(executionContext) + File.separator + "version.csv";
	   }

	/**
	 * Export modus, SQL or XML. (GUI support).
	 */
	private String exportModus;

	/**
	 * Holds XML settings for exportation into XML files.
	 */
	public static class XmlSettings {
		public String datePattern = "yyyy-MM-dd";
		public String timestampPattern = "yyyy-MM-dd-HH.mm.ss";
		public String rootTag = "rowset";
	}

	/**
	 * XML settings for exportation into XML files.
	 */
	private XmlSettings xmlSettings = new XmlSettings();

	/**
	 * Name of the model.
	 */
	private String name;

	/**
	 * Time of last modification.
	 */
	private Long lastModified;

	/**
	 * The logger.
	 */
	private static final Logger _log = LoggerFactory.getLogger(DataModel.class);

	/**
	 * Gets a table by name.
	 *
	 * @param name the name of the table
	 * @return the table or <code>null</code> iff no table with the name exists
	 */
	public Table getTable(String name) {
		return tables.get(name);
	}

	/**
	 * Gets a table by display name.
	 *
	 * @param displayName the display name of the table
	 * @return the table or <code>null</code> iff no table with the display name exists
	 */
	public Table getTableByDisplayName(String displayName) {
		return tablesByDisplayName.get(displayName);
	}

	/**
	 * Gets name of the model.
	 *
	 * @return name of the model
	 */
	public String getName() {
		return name;
	}

	/**
	 * Gets time of last modification.
	 *
	 * @return time of last modification
	 */
	public Long getLastModified() {
		return lastModified;
	}

	/**
	 * Gets display name of a table
	 *
	 * @param table the table
	 * @return the display name of the table
	 */
	public String getDisplayName(Table table) {
		String displayName = this.displayName.get(table);
		if (displayName == null) {
			return table.getName();
		}
		return displayName;
	}

	/**
	 * Gets all tables.
	 *
	 * @return a collection of all tables
	 */
	public Collection<Table> getTables() {
		return tables.values();
	}

	/**
	 * Reads in <code>table.csv</code> and <code>association.csv</code>
	 * and builds the relational data model.
	 */
	public DataModel(PrimaryKeyFactory primaryKeyFactory, Map<String, String> sourceSchemaMapping, ExecutionContext executionContext) throws IOException {
		this(null, null, sourceSchemaMapping, null, primaryKeyFactory, executionContext, false, null);
	}

	/**
	 * Reads in <code>table.csv</code> and <code>association.csv</code>
	 * and builds the relational data model.
	 */
	public DataModel(ExecutionContext executionContext) throws IOException {
		this(null, null, new HashMap<String, String>(), null, new PrimaryKeyFactory(executionContext), executionContext, false, null);
	}

	/**
	 * Reads in <code>table.csv</code> and <code>association.csv</code>
	 * and builds the relational data model.
	 * @param knownIdentifiers
	 */
	public DataModel(KnownIdentifierMap knownIdentifiers, ExecutionContext executionContext) throws IOException {
		this(null, null, new HashMap<String, String>(), null, new PrimaryKeyFactory(executionContext), executionContext, false, knownIdentifiers);
	}

	/**
	 * Reads in <code>table.csv</code> and <code>association.csv</code>
	 * and builds the relational data model.
	 */
	public DataModel(Map<String, String> sourceSchemaMapping, ExecutionContext executionContext, boolean failOnMissingTables) throws IOException {
		this(null, null, sourceSchemaMapping, null, new PrimaryKeyFactory(executionContext), executionContext, failOnMissingTables, null);
	}

	/**
	 * Reads in <code>table.csv</code> and <code>association.csv</code>
	 * and builds the relational data model.
	 *
	 * @param additionalTablesFile table file to read too
	 * @param additionalAssociationsFile association file to read too
	 */
	public DataModel(String additionalTablesFile, String additionalAssociationsFile, PrimaryKeyFactory primaryKeyFactory, ExecutionContext executionContext) throws IOException {
		this(additionalTablesFile, additionalAssociationsFile, new HashMap<String, String>(), null, primaryKeyFactory, executionContext, false, null);
	}

	/**
	 * Reads in <code>table.csv</code> and <code>association.csv</code>
	 * and builds the relational data model.
	 *
	 * @param additionalTablesFile table file to read too
	 * @param additionalAssociationsFile association file to read too
	 */
	public DataModel(String additionalTablesFile, String additionalAssociationsFile, ExecutionContext executionContext) throws IOException {
		this(additionalTablesFile, additionalAssociationsFile, new HashMap<String, String>(), null, new PrimaryKeyFactory(executionContext), executionContext, false, null);
	}

	/**
	 * Reads in <code>table.csv</code> and <code>association.csv</code>
	 * and builds the relational data model.
	 *
	 * @param additionalTablesFile table file to read too
	 * @param additionalAssociationsFile association file to read too
	 */
	public DataModel(String additionalTablesFile, String additionalAssociationsFile, Map<String, String> sourceSchemaMapping, LineFilter assocFilter, ExecutionContext executionContext) throws IOException {
		this(additionalTablesFile, additionalAssociationsFile, sourceSchemaMapping, assocFilter, new PrimaryKeyFactory(executionContext), executionContext, false, null);
	}

	/**
	 * Reads in <code>table.csv</code> and <code>association.csv</code>
	 * and builds the relational data model.
	 *
	 * @param additionalTablesFile table file to read too
	 * @param additionalAssociationsFile association file to read too
	 * @throws IOException
	 */
	public DataModel(String additionalTablesFile, String additionalAssociationsFile, Map<String, String> sourceSchemaMapping, LineFilter assocFilter, PrimaryKeyFactory primaryKeyFactory, ExecutionContext executionContext, boolean failOnMissingTables) throws IOException {
		this(additionalTablesFile, additionalAssociationsFile, sourceSchemaMapping, assocFilter, primaryKeyFactory, executionContext, failOnMissingTables, null);
	}

	/**
	 * Reads in <code>table.csv</code> and <code>association.csv</code>
	 * and builds the relational data model.
	 *
	 * @param additionalTablesFile table file to read too
	 * @param additionalAssociationsFile association file to read too
	 * @throws IOException
	 */
	public DataModel(String additionalTablesFile, String additionalAssociationsFile, Map<String, String> sourceSchemaMapping, LineFilter assocFilter, PrimaryKeyFactory primaryKeyFactory, ExecutionContext executionContext, boolean failOnMissingTables, KnownIdentifierMap knownIdentifiers) throws IOException {
		this.executionContext = executionContext;
		this.primaryKeyFactory = primaryKeyFactory;
		try {
			List<String> excludeFromDeletion = new ArrayList<String>();
			PrintUtil.loadTableList(excludeFromDeletion, openModelFile(new File(DataModel.getExcludeFromDeletionFile(executionContext)), executionContext));

			// tables
			File tabFile = new File(getTablesFile(executionContext));
			StringBuilder resourceName = new StringBuilder();
			InputStream nTablesFile = openModelFile(tabFile, resourceName, false, executionContext);
			if (failOnMissingTables && nTablesFile == null) {
				throw new RuntimeException("Data model not found: " + resourceName);
			}
			CsvFile tablesFile = new CsvFile(nTablesFile, null, tabFile.getPath(), null);
			List<CsvFile.Line> tableList = new ArrayList<CsvFile.Line>(tablesFile.getLines());
			if (additionalTablesFile != null) {
				tableList.addAll(new CsvFile(new File(additionalTablesFile)).getLines());
			}
			for (CsvFile.Line line: tableList) {
				boolean defaultUpsert = "Y".equalsIgnoreCase(line.cells.get(1));
				List<Column> pk = new ArrayList<Column>();
				int j;
				for (j = 2; j < line.cells.size() && line.cells.get(j).toString().length() > 0; ++j) {
					String col = line.cells.get(j).trim();
					String newName = null;
					if (knownIdentifiers != null) {
						try {
							Column c = Column.parse(col);
							newName = knownIdentifiers.getColumnName(line.cells.get(0), c.name);
						} catch (Exception e) {
							// ignore
						}
					}
					try {
						pk.add(Column.parse(newName, col));
					} catch (Exception e) {
						_log.warn("", e);
						// ignore, was throw new RuntimeException("unable to load table '" + line.cells.get(0) + "'. " + line.location, e);
					}
				}
				String tabName = line.cells.get(0);
				if (knownIdentifiers != null) {
					String newName = knownIdentifiers.getTableName(tabName);
					if (newName != null) {
						tabName = newName;
					}
				}
				String mappedSchemaTableName = SqlUtil.mappedSchema(sourceSchemaMapping, tabName);
				Table table = new Table(mappedSchemaTableName, primaryKeyFactory.createPrimaryKey(pk, mappedSchemaTableName), defaultUpsert, excludeFromDeletion.contains(mappedSchemaTableName));
				table.setAuthor(line.cells.get(j + 1));
				table.setOriginalName(line.cells.get(0));
				if (tables.containsKey(mappedSchemaTableName)) {
					if (additionalTablesFile == null && knownIdentifiers == null) {
						throw new RuntimeException("Duplicate table name '" + mappedSchemaTableName + "'");
					}
				}
				tables.put(mappedSchemaTableName, table);
			}

			// column order
			File orderFile = new File(getColumnOrderFile(executionContext));
			if (orderFile.exists()) {
				List<CsvFile.Line> orderList = new CsvFile(orderFile).getLines();
				for (CsvFile.Line line: orderList) {
					String column = line.cells.get(0);
					if (column != null && !column.isEmpty()) {
						if ("hi".equalsIgnoreCase(line.cells.get(1))) {
							columnOrderPrio.put(column, ColumnOrderPriority.HI);
						} else if ("lo".equalsIgnoreCase(line.cells.get(1))) {
							columnOrderPrio.put(column, ColumnOrderPriority.LO);
						}
					}
				}
			}

			// columns
			File colFile = new File(getColumnsFile(executionContext));
			InputStream is = openModelFile(colFile, executionContext);
			if (is != null) {
				CsvFile columnsFile = new CsvFile(is, null, colFile.getPath(), null);
				List<CsvFile.Line> columnsList = new ArrayList<CsvFile.Line>(columnsFile.getLines());
				for (CsvFile.Line line: columnsList) {
					List<Column> columns = new ArrayList<Column>();
					for (int j = 1; j < line.cells.size() && line.cells.get(j).toString().length() > 0; ++j) {
						String col = line.cells.get(j).trim();
						String newName = null;
						if (knownIdentifiers != null) {
							try {
								Column c = Column.parse(col);
								newName = knownIdentifiers.getColumnName(Quoting.normalizeIdentifier(line.cells.get(0)), c.name);
							} catch (Exception e) {
								// ignore
							}
						}
						try {
							columns.add(Column.parse(newName, col));
						} catch (Throwable e) {
							e.printStackTrace();
							// ignore
						}
					}
					// order columns
					if (!columnOrderPrio.isEmpty()) {
						final Map<Column, ColumnOrderPriority> prio = new IdentityHashMap<>();
						String prefix = line.cells.get(0).trim().length() == 0? "" : (line.cells.get(0).trim() + ".");
						for (Column column: columns) {
							ColumnOrderPriority columnOrderPriority = columnOrderPrio.get(prefix + Quoting.normalizeIdentifier(column.name));
							if (columnOrderPriority == null) {
								columnOrderPriority = columnOrderPrio.get(Quoting.normalizeIdentifier(column.name));
							}
							prio.put(column, columnOrderPriority);
						}
						Collections.sort(columns, new Comparator<Column>() {
							@Override
							public int compare(Column a, Column b) {
								ColumnOrderPriority prioA = prio.get(a);
								ColumnOrderPriority prioB = prio.get(b);
								if (prioA != prioB) {
									if (prioA == ColumnOrderPriority.HI) {
											return -1;
									} else if (prioA == ColumnOrderPriority.LO) {
										return 1;
									} else {
										return prioB == ColumnOrderPriority.HI? 1 : -1;
									}
								}
								return 0;
							}
						});
					}
					Table table = tables.get(SqlUtil.mappedSchema(sourceSchemaMapping, line.cells.get(0)));
					if (table != null) {
						table.setColumns(columns);
					}
				}
			}

			// associations
			File assFile = new File(getAssociationsFile(executionContext));
			List<CsvFile.Line> associationList = new ArrayList<CsvFile.Line>(new CsvFile(openModelFile(assFile, executionContext), null, assFile.getPath(), assocFilter).getLines());
			if (additionalAssociationsFile != null) {
				associationList.addAll(new CsvFile(new File(additionalAssociationsFile)).getLines());
			}
			for (CsvFile.Line line: associationList) {
				String location = line.location;
				try {
					String associationLoadFailedMessage = "Unable to load association from " + line.cells.get(0) + " to " + line.cells.get(1) + " on " + line.cells.get(4) + " because: ";
					String aName = line.cells.get(0);
					if (knownIdentifiers != null) {
						aName = knownIdentifiers.getTableName(aName);
						if (aName == null) {
							aName = line.cells.get(0);
						}
					}
					Table tableA = tables.get(SqlUtil.mappedSchema(sourceSchemaMapping, aName));
					if (tableA == null) {
						 continue;
//	                     throw new RuntimeException(associationLoadFailedMessage + "Table '" + line.cells.get(0) + "' not found");
					}
					String bName = line.cells.get(1);
					if (knownIdentifiers != null) {
						bName = knownIdentifiers.getTableName(bName);
						if (bName == null) {
							bName = line.cells.get(1);
						}
					}
					Table tableB = tables.get(SqlUtil.mappedSchema(sourceSchemaMapping, bName));
					if (tableB == null) {
						continue;
//	                	throw new RuntimeException(associationLoadFailedMessage + "Table '" + line.cells.get(1) + "' not found");
					}
					boolean insertSourceBeforeDestination = "A".equalsIgnoreCase(line.cells.get(2));
					boolean insertDestinationBeforeSource = "B".equalsIgnoreCase(line.cells.get(2));
					Cardinality cardinality = Cardinality.parse(line.cells.get(3).trim());
					if (cardinality == null) {
						cardinality = Cardinality.MANY_TO_MANY;
					}
					String joinCondition = null;
					if (knownIdentifiers != null) {
						joinCondition = knownIdentifiers.getCondition(line.cells.get(4));
					}
					if (joinCondition == null) {
						joinCondition = line.cells.get(4);
					}
					String name = line.cells.get(5);
					if ("".equals(name)) {
						name = null;
					}
					if (name == null) {
						throw new RuntimeException(associationLoadFailedMessage + "Association name missing (column 6 is empty, each association must have an unique name)");
					}
					String author = line.cells.get(6);
					Association associationA = new Association(tableA, tableB, insertSourceBeforeDestination, insertDestinationBeforeSource, joinCondition, this, false, cardinality, author);
					Association associationB = new Association(tableB, tableA, insertDestinationBeforeSource, insertSourceBeforeDestination, joinCondition, this, true, cardinality.reverse(), author);
					associationA.reversalAssociation = associationB;
					associationB.reversalAssociation = associationA;
					tableA.associations.add(associationA);
					tableB.associations.add(associationB);
					if (namedAssociations.put(name, associationA) != null) {
						throw new RuntimeException("duplicate association name: " + name);
					}
					associationA.setName(name);
					name = "inverse-" + name;
					if (namedAssociations.put(name, associationB) != null) {
						throw new RuntimeException("duplicate association name: " + name);
					}
					associationB.setName(name);
				} catch (Exception e) {
					throw new RuntimeException(location + ": " + e.getMessage(), e);
				}
			}
			initDisplayNames();
			initTableOrdinals();

			// model name
			File nameFile = new File(getModelNameFile(executionContext));
			name = DEFAULT_NAME;
			lastModified = null;
			try {
				lastModified = nameFile.lastModified();
				if (nameFile.exists()) {
					List<CsvFile.Line> nameList = new ArrayList<CsvFile.Line>(new CsvFile(nameFile).getLines());
					if (nameList.size() > 0) {
						CsvFile.Line line =  nameList.get(0);
						name = line.cells.get(0);
						lastModified = Long.parseLong(line.cells.get(1));
					}
				}
			} catch (Throwable t) {
				// keep defaults
			}
		} catch (IOException e) {
			_log.error("failed to load data-model " + getDatamodelFolder(executionContext) + File.separator, e);
			throw e;
		}
		try {
			// comments
			File commentFile = new File(getCommentsFile(executionContext));
			InputStream is = openModelFile(commentFile, executionContext);
			if (is != null) {
				new CsvFile(is, null, commentFile.getPath(), null).getLines().forEach(l -> comments.put(l.cells.get(0), l.cells.get(1)));
			}
		} catch (Exception e) {
			// ignore
		}
	}

	private final List<Table> tableList = new ArrayList<Table>();
	private final List<FilterTemplate> filterTemplates = new ArrayList<FilterTemplate>();

	/**
	 * Initializes table ordinals.
	 */
	private void initTableOrdinals() {
		for (Table table: getSortedTables()) {
			table.ordinal = tableList.size();
			tableList.add(table);
		}
	}

	/**
	 * Initializes display names.
	 */
	private void initDisplayNames() throws IOException {
		Set<String> unqualifiedNames = new HashSet<String>();
		Set<String> nonUniqueUnqualifiedNames = new HashSet<String>();

		for (Table table: getTables()) {
			String uName = table.getUnqualifiedName();
			if (unqualifiedNames.contains(uName)) {
				nonUniqueUnqualifiedNames.add(uName);
			} else {
				unqualifiedNames.add(uName);
			}
		}

		for (Table table: getTables()) {
			String uName = table.getUnqualifiedName();
			if (uName != null && uName.length() > 0) {
				char fc = uName.charAt(0);
				if (!Character.isLetterOrDigit(fc) && fc != '_') {
				   String fcStr = Character.toString(fc);
				   if (uName.startsWith(fcStr) && uName.endsWith(fcStr)) {
					   uName = uName.substring(1, uName.length() -1);
				   }
				}
			}
			String schema = table.getSchema(null);
			String displayName;
			if (nonUniqueUnqualifiedNames.contains(uName) && schema != null) {
				displayName = uName + " (" + schema + ")";
			} else {
				displayName = uName;
			}
			this.displayName.put(table, displayName);
			tablesByDisplayName.put(displayName, table);
		}

		Map<String, String> userDefinedDisplayNames = new TreeMap<String, String>();
		File dnFile = new File(DataModel.getDisplayNamesFile(executionContext));
		if (dnFile.exists()) {
			for (CsvFile.Line dnl: new CsvFile(dnFile).getLines()) {
				userDefinedDisplayNames.put(dnl.cells.get(0), dnl.cells.get(1));
			}
		}

		for (Map.Entry<String, String> e: userDefinedDisplayNames.entrySet()) {
			Table table = getTable(e.getKey());
			if (table != null && !tablesByDisplayName.containsKey(e.getValue())) {
				String displayName = getDisplayName(table);
				this.displayName.remove(table);
				if (displayName != null) {
					tablesByDisplayName.remove(displayName);
				}
				this.displayName.put(table, e.getValue());
				tablesByDisplayName.put(e.getValue(), table);
			}
		}
	}

	/**
	 * Gets the primary-key to be used for the entity-table.
	 *
	 * @param session for null value guessing
	 * @return the universal primary key
	 */
	PrimaryKey getUniversalPrimaryKey(Session session) {
		return primaryKeyFactory.getUniversalPrimaryKey(session);
	}

	/**
	 * Gets the primary-key to be used for the entity-table.
	 *
	 * @return the universal primary key
	 */
	PrimaryKey getUniversalPrimaryKey() {
		return getUniversalPrimaryKey(null);
	}

	/**
	 * Gets the restriction model.
	 *
	 * @return the restriction model
	 */
	public RestrictionModel getRestrictionModel() {
		return restrictionModel;
	}

	/**
	 * Sets the restriction model.
	 *
	 * @param restrictionModel the restriction model
	 */
	public void setRestrictionModel(RestrictionModel restrictionModel) {
		this.restrictionModel = restrictionModel;
		++version;
	}

	/**
	 * Gets all independent tables
	 * (i.e. tables which don't depend on other tables in the set)
	 * of a given table-set.
	 *
	 * @param tableSet the table-set
	 * @return the sub-set of independent tables of the table-set
	 */
	public Set<Table> getIndependentTables(Set<Table> tableSet) {
		return getIndependentTables(tableSet, null);
	}

	/**
	 * Gets all independent tables
	 * (i.e. tables which don't depend on other tables in the set)
	 * of a given table-set.
	 *
	 * @param tableSet the table-set
	 * @param associations the associations to consider, <code>null</code> for all associations
	 * @return the sub-set of independent tables of the table-set
	 */
	public Set<Table> getIndependentTables(Set<Table> tableSet, Set<Association> associations) {
		Set<Table> independentTables = new TreeSet<Table>();

		for (Table table: tableSet) {
			boolean depends = false;
			for (Association a: table.associations) {
				if (associations == null || associations.contains(a)) {
					if (tableSet.contains(a.destination)) {
						// if (a.getJoinCondition() != null) {
							if (a.isInsertDestinationBeforeSource()) {
								depends = true;
								break;
						//	}
						}
					}
				}
			}
			if (!depends) {
				independentTables.add(table);
			}
		}
		return independentTables;
	}

	/**
	 * Transposes the data-model.
	 */
	public void transpose() {
		if (getRestrictionModel() != null) {
			getRestrictionModel().transpose();
		}
		++version;
	}

	/**
	 * Stringifies the data model.
	 */
	@Override
	public String toString() {
		List<Table> sortedTables;
		sortedTables = getSortedTables();
		StringBuffer str = new StringBuffer();
		if (restrictionModel != null) {
			str.append("restricted by: " + restrictionModel + "\n");
		}
		for (Table table: sortedTables) {
			str.append(table);
		}
		return str.toString();
	}

	/**
	 * Gets list of tables sorted by name.
	 *
	 * @return list of tables sorted by name
	 */
	public List<Table> getSortedTables() {
		List<Table> sortedTables;
		sortedTables = new ArrayList<Table>(getTables());
		Collections.sort(sortedTables, new Comparator<Table>() {
			@Override
			public int compare(Table o1, Table o2) {
				return o1.getName().compareTo(o2.getName());
			}
		});
		return sortedTables;
	}

	/**
	 * Normalizes a set of tables.
	 *
	 * @param tables set of tables
	 * @return set of all tables from this model for which a table with same name exists in <code>tables</code>
	 */
	public Set<Table> normalize(Set<Table> tables) {
		Set<Table> result = new HashSet<Table>();
		for (Table table: tables) {
			result.add(getTable(table.getName()));
		}
		return result;
	}

	/**
	 * Assigns a unique ID to each association.
	 */
	public void assignAssociationIDs() {
		int n = 1;
		for (Map.Entry<String, Association> e: namedAssociations.entrySet()) {
			e.getValue().id = n++;
		}
	}

	/**
	 * Gets export modus, SQL or XML. (GUI support).
	 */
	public String getExportModus() {
		return exportModus;
	}

	/**
	 * Sets export modus, SQL or XML. (GUI support).
	 */
	public void setExportModus(String modus) {
		exportModus = modus;
		++version;
	}

	/**
	 * Gets XML settings for exportation into XML files.
	 */
	public XmlSettings getXmlSettings() {
		return xmlSettings;
	}

	/**
	 * Sets XML settings for exportation into XML files.
	 */
	public void setXmlSettings(XmlSettings xmlSettings) {
		this.xmlSettings = xmlSettings;
		++version;
	}

	/**
	 * Gets internal version number. Incremented on each modification.
	 *
	 * @return internal version number. Incremented on each modification.
	 */
	public long getVersion() {
		return version;
	}

	/**
	 * Thrown if a table has no primary key.
	 */
	public static class NoPrimaryKeyException extends RuntimeException {
		private static final long serialVersionUID = 4523935351640139649L;
		public final Table table;
		public NoPrimaryKeyException(Table table, String message) {
			super((table == null? "Subject table " : ("Table '" + table.getName() + "' ")) + message);
			this.table = table;
		}
		public NoPrimaryKeyException(Table table) {
			this(table, "has no primary key");
		}
	}

	/**
	 * Checks whether all tables in the closure of a given subject have primary keys.
	 *
	 * @param subjects the subjects
	 * @param hasRowID
	 * 
	 * @throws NoPrimaryKeyException if a table has no primary key
	 */
	public Set<Table> checkForPrimaryKey(Set<Table> subjects, boolean hasRowID) throws NoPrimaryKeyException {
		Set<Table> checked = new HashSet<Table>();
		for (Table subject: subjects) {
			Set<Table> toCheck = new HashSet<Table>(subject.closure(checked));
			checked.addAll(toCheck);
		}
		DataModel.addRestrictedDependencyWithNulledFK(checked);
		for (Table table: checked) {
			if (!hasRowID) {
				if (table.primaryKey.getColumns().isEmpty()) {
					throw new NoPrimaryKeyException(table);
				}
			}
			if (table.getColumns().isEmpty()) {
				throw new NoPrimaryKeyException(table, "has no column");
			}
		}
		return checked;
	}

	/**
	 * Gets all parameters which occur in subject condition, association restrictions or XML templates.
	 *
	 * @param subjectCondition the subject condition
	 * @return all parameters which occur in subject condition, association restrictions or XML templates
	 */
	public SortedSet<String> getParameters(String subjectCondition, List<ExtractionModel.AdditionalSubject> additionalSubjects) {
		SortedSet<String> parameters = new TreeSet<String>();

		ParameterHandler.collectParameter(subjectCondition, parameters);
		if (additionalSubjects != null) {
			for (AdditionalSubject as: additionalSubjects) {
				ParameterHandler.collectParameter(as.getCondition(), parameters);
			}
		}
		for (Association a: namedAssociations.values()) {
			String r = a.getRestrictionCondition();
			if (r != null) {
				ParameterHandler.collectParameter(r, parameters);
			}
		}
		for (Table t: getTables()) {
			String r = t.getXmlTemplate();
			if (r != null) {
				ParameterHandler.collectParameter(r, parameters);
			}
			for (Column c: t.getColumns()) {
				if (c.getFilterExpression() != null) {
					ParameterHandler.collectParameter(c.getFilterExpression(), parameters);
				}
			}
		}
		return parameters;
	}

	private static InputStream openModelFile(File file, ExecutionContext executionContext) throws IOException {
		return openModelFile(file, new StringBuilder(), false, executionContext);
	}

	private static InputStream openModelFile(File file, StringBuilder resourceName, boolean failOnIOException, ExecutionContext executionContext) throws IOException {
		try {
			URL dataModelURL = executionContext.getDataModelURL();
			URI uri = dataModelURL.toURI();
			String uriAsString = uri.toString();
			if (!uriAsString.endsWith("/")) {
				uriAsString += "/";
			}
			URI resolved = new URI(uriAsString + file.getName()); // uri.resolve(file.getName());
			resourceName.append(resolved.toURL().toString());
			return resolved.toURL().openStream();
		} catch (MalformedURLException e) {
			throw new RuntimeException(e);
		} catch (IOException e) {
			if (failOnIOException) {
				throw e;
			}
			return null;
		} catch (URISyntaxException e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * Gets {@link #getLastModified()} as String.
	 *
	 * @return {@link #getLastModified()} as String
	 */
	public String getLastModifiedAsString() {
		try {
			return DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.MEDIUM).format(new Date(getLastModified()));
		} catch (Throwable t) {
			return "";
		}
	}

	/**
	 * Saves the data model.
	 *
	 * @param file the file name
	 * @param stable
	 * @param stable the subject table
	 * @param subjectLimitDefinition limit of subject
	 * @param subjectCondition
	 * @param scriptFormat
	 * @param positions table positions or <code>null</code>
	 * @param additionalSubjects
	 */
	public void save(String file, Table stable, SubjectLimitDefinition subjectLimitDefinition, String subjectCondition, ScriptFormat scriptFormat, List<RestrictionDefinition> restrictionDefinitions, Map<String, Map<String, double[]>> positions, List<AdditionalSubject> additionalSubjects, String currentModelSubfolder) throws FileNotFoundException {
		File extractionModel = new File(file);
		PrintWriter out = new PrintWriter(extractionModel);
		out.println("# subject; condition; limit; limit-order");
		out.println(
				CsvFile.encodeCell("" + stable.getName()) + "; " +
				CsvFile.encodeCell(subjectCondition) + "; " +
				CsvFile.encodeCell(subjectLimitDefinition.limit == null? "" : subjectLimitDefinition.limit.toString()) + "; " +
				CsvFile.encodeCell(subjectLimitDefinition.orderBy == null? "" : subjectLimitDefinition.orderBy)
				);
		saveRestrictions(out, restrictionDefinitions);
		out.println();
		out.println(CsvFile.BLOCK_INDICATOR + "additional subjects");
		out.println("# subject; condition; limit; limit-order");
		for (AdditionalSubject as: additionalSubjects) {
			out.println(
					CsvFile.encodeCell("" + as.getSubject().getName()) + "; " +
					CsvFile.encodeCell(as.getCondition()) + "; " +
					CsvFile.encodeCell(as.getSubjectLimitDefinition().limit == null? "" : as.getSubjectLimitDefinition().limit.toString()) + "; " +
					CsvFile.encodeCell(as.getSubjectLimitDefinition().orderBy == null? "" : as.getSubjectLimitDefinition().orderBy)
					);
		}
		saveXmlMapping(out);
		out.println();
		out.println(CsvFile.BLOCK_INDICATOR + "datamodelfolder");
		if (currentModelSubfolder != null) {
			out.println(currentModelSubfolder);
		}
		out.println();
		out.println(CsvFile.BLOCK_INDICATOR + "export modus");
		out.println(scriptFormat);
		out.println();
		out.println(CsvFile.BLOCK_INDICATOR + "xml settings");
		out.println(CsvFile.encodeCell(getXmlSettings().datePattern) + ";" +
				CsvFile.encodeCell(getXmlSettings().timestampPattern) + ";" +
				CsvFile.encodeCell(getXmlSettings().rootTag));
		out.println(CsvFile.BLOCK_INDICATOR + "xml column mapping");
		for (Table table: getTables()) {
			String xmlMapping = table.getXmlTemplate();
			if (xmlMapping != null) {
				out.println(CsvFile.encodeCell(table.getName()) + "; " + CsvFile.encodeCell(xmlMapping));
			}
		}
		out.println(CsvFile.BLOCK_INDICATOR + "upserts");
		for (Table table: getTables()) {
			if (table.upsert != null) {
				out.println(CsvFile.encodeCell(table.getName()) + "; " + CsvFile.encodeCell(table.upsert.toString()));
			}
		}
		out.println(CsvFile.BLOCK_INDICATOR + "exclude from deletion");
		for (Table table: getTables()) {
			if (table.excludeFromDeletion != null) {
				out.println(CsvFile.encodeCell(table.getName()) + "; " + CsvFile.encodeCell(table.excludeFromDeletion.toString()));
			}
		}
		saveFilters(out);
		saveFilterTemplates(out);
		out.println();
		if (positions == null) {
			executionContext.getLayoutStorage().store(out);
		} else {
			executionContext.getLayoutStorage().store(out, positions);
		}

		out.println();
		out.println(CsvFile.BLOCK_INDICATOR + "known");
		out.println("# known association; decision pending");
		if (!restrictionDefinitions.isEmpty()) {
			for (Association a: namedAssociations.values()) {
				if (!a.reversed) {
					out.println(CsvFile.encodeCell(a.getName()) + "; " + CsvFile.encodeCell(decisionPending.contains(a.getName())? "pending" : ""));
				}
			}
		}

		out.println();
		out.println(CsvFile.BLOCK_INDICATOR + "version");
		out.println(JailerVersion.VERSION);
		out.close();
	}

	/**
	 * Saves xml mappings.
	 *
	 * @param out to save xml mappings into
	 */
	private void saveXmlMapping(PrintWriter out) {
		out.println();
		out.println(CsvFile.BLOCK_INDICATOR + "xml-mapping");
		for (Table table: getTables()) {
			for (Association a: table.associations) {
				String name = a.getName();
				String tag = a.getAggregationTagName();
				String aggregation = a.getAggregationSchema().name();
				out.println(CsvFile.encodeCell(name) + ";" + CsvFile.encodeCell(tag) + ";" + CsvFile.encodeCell(aggregation));
			}
		}
	}

	/**
	 * Saves restrictions only.
	 *
	 * @param out to save restrictions into
	 * @param restrictionDefinitions
	 */
	private void saveRestrictions(PrintWriter out, List<RestrictionDefinition> restrictionDefinitions) {
		out.println();
		out.println("# association; ; restriction-condition");
		for (RestrictionDefinition rd: restrictionDefinitions) {
			String condition = rd.isIgnored? "ignore" : rd.condition;
			if (rd.name == null || rd.name.trim().length() == 0) {
				out.println(CsvFile.encodeCell(rd.from.getName()) + "; " + CsvFile.encodeCell(rd.to.getName()) + "; " + CsvFile.encodeCell(condition));
			} else {
				out.println(CsvFile.encodeCell(rd.name) + "; ; " + CsvFile.encodeCell(condition));
			}
		}
	}

	/**
	 * Saves restrictions only.
	 *
	 * @param file to save restrictions into
	 */
	public void saveRestrictions(File file, List<RestrictionDefinition> restrictionDefinitions) throws FileNotFoundException {
		PrintWriter out = new PrintWriter(file);
		saveRestrictions(out, restrictionDefinitions);
		out.close();
	}

	/**
	 * Saves filters.
	 *
	 * @param out to save filters into
	 */
	private void saveFilters(PrintWriter out) {
		out.println();
		out.println(CsvFile.BLOCK_INDICATOR + "filters");
		for (Table table: getTables()) {
			for (Column c: table.getColumns()) {
				if (c.getFilter() != null && !c.getFilter().isDerived()) {
					out.println(CsvFile.encodeCell(table.getName()) + ";" + CsvFile.encodeCell(c.name) + ";" + CsvFile.encodeCell(c.getFilter().getExpression())
					+ ";" + CsvFile.encodeCell(c.getFilter().isApplyAtExport()? "Export" : "Import")
					+ ";" + CsvFile.encodeCell(c.getFilter().getType() == null? "" : c.getFilter().getType()));
				}
			}
		}
	}


	/**
	 * Saves filter templates.
	 *
	 * @param out to save filters into
	 */
	private void saveFilterTemplates(PrintWriter out) {
		out.println();
		out.println(CsvFile.BLOCK_INDICATOR + "filter templates");
		for (FilterTemplate template: getFilterTemplates()) {
			out.println("T;"
					+ CsvFile.encodeCell(template.getName()) + ";"
					+ CsvFile.encodeCell(template.getExpression()) + ";"
					+ CsvFile.encodeCell(template.isEnabled()? "enabled" : "disabled") + ";"
					+ CsvFile.encodeCell(template.isApplyAtExport()? "Export" : "Import") + ";"
					+ CsvFile.encodeCell(template.getType() == null? "" : template.getType()) + ";");
			for (Clause clause: template.getClauses()) {
				out.println("C;"
						+ CsvFile.encodeCell(clause.getSubject().name()) + ";"
						+ CsvFile.encodeCell(clause.getPredicate().name()) + ";"
						+ CsvFile.encodeCell(clause.getObject()) + ";");
			}
		}
	}

	/**
	 * Saves the order priority of the columns.
	 */
	public void saveColumnOrderPrio() throws FileNotFoundException {
		File orderFile = new File(getColumnOrderFile(executionContext));
		PrintWriter out = new PrintWriter(orderFile);
		out.println("#column;prio (HI/LO)");
		for (Entry<String, ColumnOrderPriority> e: columnOrderPrio.entrySet()) {
			out.println(CsvFile.encodeCell(e.getKey()) + ";" + e.getValue());
		}
		out.close();
	}

	/**
	 * Gets table by {@link Table#getOrdinal()}.
	 *
	 * @param ordinal the ordinal
	 * @return the table
	 */
	public Table getTableByOrdinal(int ordinal) {
		return tableList.get(ordinal);
	}

	/**
	 * Gets the {@link FilterTemplate}s ordered by priority.
	 *
	 * @return template list
	 */
	public List<FilterTemplate> getFilterTemplates() {
		return filterTemplates;
	}

	private Map<Association, Map<Column, Column>> sToDMaps = new HashMap<Association, Map<Column,Column>>();

	/**
	 * Removes all derived filters and renews them.
	 */
	public void deriveFilters() {
		sToDMaps.clear();
		for (Table table: getTables()) {
			for (Column column: table.getColumns()) {
				Filter filter = column.getFilter();
				if (filter != null && filter.isDerived()) {
					column.setFilter(null);
				}
			}
		}
		Set<String> pkNames = new HashSet<String>();
		for (Table table: getTables()) {
			pkNames.clear();
			for (Column column: table.primaryKey.getColumns()) {
				pkNames.add(column.name);
			}
			for (Column column: table.getColumns()) {
				if (pkNames.contains(column.name)) {
					Filter filter = column.getFilter();
					if (filter != null && !filter.isDerived()) {
						List<String> aTo = new ArrayList<String>();
						deriveFilter(table, column, filter, new PKColumnFilterSource(table, column), aTo, null);
						if (!aTo.isEmpty()) {
							Collections.sort(aTo);
							filter.setAppliedTo(aTo);
						}
					}
				}
			}
		}

		// apply templates
		for (FilterTemplate template: getFilterTemplates()) {
			if (template.isEnabled()) {
				for (Table table: getTables()) {
					for (Column column: table.getColumns()) {
						if (column.getFilter() == null && template.matches(table, column)) {
							Filter filter = new Filter(template.getExpression(), template.getType(), true, template);
							filter.setApplyAtExport(template.isApplyAtExport());
							column.setFilter(filter);
							List<String> aTo = new ArrayList<String>();
							deriveFilter(table, column, filter, new PKColumnFilterSource(table, column), aTo, template);
							if (!aTo.isEmpty()) {
								Collections.sort(aTo);
								filter.setAppliedTo(aTo);
							}
						}
					}
				}
			}
		}
		sToDMaps.clear();
	}

	private void deriveFilter(Table table, Column column, Filter filter, FilterSource filterSource, List<String> aTo, FilterSource overwriteForSource) {
		for (Association association: table.associations) {
			if (association.isInsertSourceBeforeDestination()) {
				Map<Column, Column> sToDMap = sToDMaps.get(association);
				if (sToDMap == null) {
					sToDMap = association.createSourceToDestinationKeyMapping();
					sToDMaps.put(association, sToDMap);
				}
				Column destColumn = sToDMap.get(column);
				if (destColumn != null && (destColumn.getFilter() == null || overwriteForSource != null && destColumn.getFilter().getFilterSource() == overwriteForSource)) {
					Filter newFilter = new Filter(filter.getExpression(), filter.getType(), true, filterSource);
					newFilter.setApplyAtExport(filter.isApplyAtExport());
					destColumn.setFilter(newFilter);
					aTo.add(association.destination.getName() + "." + destColumn.name);
					deriveFilter(association.destination, destColumn, filter, filterSource, aTo, overwriteForSource);
				}
			}
		}
	}

	public static void addRestrictedDependencyWithNulledFK(Set<Table> tables) {
		Set<Table> toAdd = new HashSet<Table>();
		tables.forEach(table -> table.associations.forEach(association -> {
			if (association.reversalAssociation.isRestrictedDependencyWithNulledFK() || association.isRestrictedDependencyWithNulledFK()) {
				toAdd.add(association.source);
				toAdd.add(association.destination);
			}
		}));
		tables.addAll(toAdd);
	}

	private Map<String, String> comments = new HashMap<String, String>();
	
	public String getComment(Table table, Column column) {
		return comments.get(table.getName() + (column != null? "." + column.name : ""));
	}

	// TODO datamodel-file-monitoring. On file change, ask user if currently open extraction models/data browser desktop should be reloaded/refreshed.
	
}
