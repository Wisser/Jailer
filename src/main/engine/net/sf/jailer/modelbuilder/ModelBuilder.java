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
package net.sf.jailer.modelbuilder;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import javax.sql.DataSource;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Cardinality;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.PrimaryKeyFactory;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.importfilter.ImportFilterManager;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.CsvFile.Line;
import net.sf.jailer.util.PrintUtil;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlUtil;

/**
 * Automatically builds a data-model using a list of {@link ModelElementFinder}.
 *
 * Writes all model elements into the files
 * <ul>
 *   <li>model-builder-table.csv<li>
 *   <li>model-builder-association.csv<li>
 * <ul>
 * except the already known elements (table.csv/association.csv)
 * and the excluded elements listed in exclude-[tables|associations].csv
 *
 * @author Ralf Wisser
 */
public class ModelBuilder {

	/**
	 * The logger.
	 */
	private static final Logger _log = LoggerFactory.getLogger(ModelBuilder.class);

	/**
	 * The statement executor for executing SQL statements.
	 */
	private static Session session;

	/**
	 * Name of CSV file for generated table definitions.
	 */
	public static String getModelBuilderTablesFilename(ExecutionContext executionContext) {
		return DataModel.getDatamodelFolder(executionContext) + File.separator + "model-builder-table.csv";
	}

	/**
	 * Name of CSV file for generated column definitions.
	 */
	public static String getModelBuilderColumnsFilename(ExecutionContext executionContext) {
		return DataModel.getDatamodelFolder(executionContext) + File.separator + "model-builder-column.csv";
	}

	/**
	 * Name of CSV file for generated association definitions.
	 */
	public static String getModelBuilderAssociationsFilename(ExecutionContext executionContext) {
		return DataModel.getDatamodelFolder(executionContext) + File.separator + "model-builder-association.csv";
	}
	/**
	 * Name of CSV file for generated comments.
	 */
	public static String getModelBuilderCommentsFilename(ExecutionContext executionContext) {
		return DataModel.getDatamodelFolder(executionContext) + File.separator + "model-builder-comment.csv";
	}

	/**
	 * The exclude-tables file.
	 */
	private static CsvFile getExcludeTablesCSV(ExecutionContext executionContext) {
		try {
			File exTFile = new File(DataModel.getDatamodelFolder(executionContext) + File.separator + "exclude-tables.csv");
			if (!exTFile.exists()) {
				exTFile.createNewFile();
			}
			return new CsvFile(exTFile);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * The exclude-associations file.
	 */
	private static CsvFile getExcludeAssociationsCSV(ExecutionContext executionContext) {
		try {
			File exAFile = new File(DataModel.getDatamodelFolder(executionContext) + File.separator + "exclude-associations.csv");
			if (!exAFile.exists()) {
				exAFile.createNewFile();
			}
			return new CsvFile(exAFile);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * Builds and merges model.
	 *
	 * @param warnings string-buffer to print warnings into, may be <code>null</code>
	 */
	public static void buildAndMerge(DataSource dataSource, DBMS dbms, String schema, StringBuffer warnings, ExecutionContext executionContext) throws Exception {
		build(dataSource, dbms, schema, warnings, executionContext);
		merge(getModelBuilderTablesFilename(executionContext), DataModel.getTablesFile(executionContext), 0, TABLE_HEADER);
		merge(getModelBuilderAssociationsFilename(executionContext), DataModel.getAssociationsFile(executionContext), 5, ASSOC_HEADER);
		merge(getModelBuilderColumnsFilename(executionContext), DataModel.getColumnsFile(executionContext), 0, COLUMN_HEADER);
		merge(getModelBuilderCommentsFilename(executionContext), DataModel.getCommentsFile(executionContext), 0, COMMENT_HEADER);
		cleanUp(executionContext);
	}

	private static void merge(String sourceFile, String destFile, int keyColumn, String header) throws Exception {
		CsvFile source = new CsvFile(new File(sourceFile));
		CsvFile dest = new CsvFile(new File(destFile));
		StringBuilder result = new StringBuilder();

		Map<String, Line> destLines = new TreeMap<String, CsvFile.Line>();

		if (keyColumn >= 0) {
			for (Line line: dest.getLines()) {
				destLines.put(line.cells.get(keyColumn), line);
			}
	
			for (Line line: source.getLines()) {
				String key = line.cells.get(keyColumn);
				destLines.remove(key);
				result.append(line.toString() + PrintUtil.LINE_SEPARATOR);
			}
		}
		
		for (Line line: destLines.values()) {
			result.append(line.toString() + PrintUtil.LINE_SEPARATOR);
		}

		writeFile(destFile, header + result.toString());
	}

	/**
	 * Builds model.
	 *
	 * @param warnings string-buffer to print warnings into, may be <code>null</code>
	 */
	public static void build(DataSource dataSource, DBMS dbms, String schema, StringBuffer warnings, ExecutionContext executionContext) throws Exception {
		session = new Session(dataSource, dbms, executionContext.getIsolationLevel());
		session.disableMetaDataChecking();
		try {
			build(schema, warnings, executionContext);
		} finally {
			try {
				session.shutDown();
			} catch (Exception e) {
				// ignore
			}
			session = null;
		}
	}

	private static void build(String schema, StringBuffer warnings, ExecutionContext executionContext) throws Exception {
		session.setIntrospectionSchema(schema);

		resetFiles(executionContext);

		KnownIdentifierMap knownIdentifiers = new KnownIdentifierMap();

		Collection<Table> tables = new ArrayList<Table>();

		ModelElementFinder finder = new JDBCMetaDataBasedModelElementFinder();
		_log.info("find tables with " + finder);
		tables.addAll(finder.findTables(session, executionContext));

		Collection<Table> allTables = new ArrayList<Table>(tables);
		Set<Table> written = new HashSet<Table>();
		for (Iterator<Table> iT = tables.iterator(); iT.hasNext(); ) {
			Table table = iT.next();
			if (/* dataModel.getTable(table.getName()) != null || */ written.contains(table)) {
				iT.remove();
			} else {
				written.add(table);
				knownIdentifiers.putTableName(table.getName());
			}
		}

		String tableDefinitions = "";
		List<Table> sortedTables = new ArrayList<Table>(tables);
		Collections.sort(sortedTables, new Comparator<Table>() {
			@Override
			public int compare(Table t1, Table t2) {
				return t1.getName().compareTo(t2.getName());
			}
		});

		Map<Table, List<Column>> columnPerTable = new HashMap<Table, List<Column>>();

		Quoting quoting = Quoting.getQuoting(session);

		StringBuilder columnsDefinition = new StringBuilder();
		CsvFile excludeTablesCSV = getExcludeTablesCSV(executionContext);
		Map<String, Table> allTablesSet = new TreeMap<String, Table>();
		for (Table table: allTables) {
			allTablesSet.put(table.getName(), table);
		}
		for (Table table: allTablesSet.values()) {
			if (!isJailerTable(table, quoting) &&
				!excludeTablesCSV.contains(new String[] { table.getName()}) &&
				!excludeTablesCSV.contains(new String[] { table.getName().toUpperCase(Locale.ENGLISH) })) {
				_log.info("find colums with " + finder);
				List<Column> columns = finder.findColumns(table, session, executionContext);
				if (!columns.isEmpty()) {
					columnPerTable.put(table, columns);
					columnsDefinition.append(CsvFile.encodeCell(table.getName()) + "; ");
					for (Column c: columns) {
						columnsDefinition.append(CsvFile.encodeCell(c.toSQL(null) + (c.isIdentityColumn? " identity" : "") + (c.isVirtual? " virtual" : "") + (c.isNullable? " null" : "")) + "; ");
						knownIdentifiers.putColumnName(table.getName(), c.name);
					}
					columnsDefinition.append(PrintUtil.LINE_SEPARATOR);
				}
			}
		}
		resetColumnsFile(columnsDefinition.toString(), executionContext);

		DataModel dataModel = new DataModel(knownIdentifiers, executionContext);

		for (Table table: sortedTables) {
			if (!isJailerTable(table, quoting) &&
				!excludeTablesCSV.contains(new String[] { table.getName()}) &&
				!excludeTablesCSV.contains(new String[] { table.getName().toUpperCase(Locale.ENGLISH) })) {
				if (table.primaryKey.getColumns().isEmpty()) {
					// try find user defined pk
					Table old = dataModel.getTable(table.getName());
					if (old != null && !old.primaryKey.getColumns().isEmpty() && columnPerTable.containsKey(table)) {
						List<Column> newPk = new ArrayList<Column>();
						for (Column c: columnPerTable.get(table)) {
							for (Column opk: old.primaryKey.getColumns()) {
								if (c.name.equals(opk.name)) {
									newPk.add(c);
									break;
								}
							}
						}
						if (newPk.size() == old.primaryKey.getColumns().size()) {
							table = new Table(old.getName(), new PrimaryKeyFactory(executionContext).createPrimaryKey(newPk, old.getName()), false, false);
							table.setAuthor(old.getAuthor());
						}
					}

					String warning = "Table '" + table.getName() + "' has no primary key";
					if (table.primaryKey.getColumns().size() == 0) {
						warnings.append(warning + PrintUtil.LINE_SEPARATOR);
					} else {
						warning += ", taking manually defined key.";
					}
					_log.warn(warning);
				}
				tableDefinitions += CsvFile.encodeCell(table.getName()) + "; N; ";
				for (Column pk: table.primaryKey.getColumns()) {
					tableDefinitions += CsvFile.encodeCell(pk.toString()) + ";";
				}
				tableDefinitions += "   ;" + CsvFile.encodeCell(table.getAuthor()) + ";" + PrintUtil.LINE_SEPARATOR;
			}
		}

		resetTableFile(tableDefinitions, executionContext);

		// re-read data model with new tables
		dataModel = new DataModel(getModelBuilderTablesFilename(executionContext), getModelBuilderAssociationsFilename(executionContext), new HashMap<String, String>(), assocFilter, new PrimaryKeyFactory(executionContext), executionContext, false, knownIdentifiers);

		Collection<Association> associations = new ArrayList<Association>();
		Map<Association, String[]> namingSuggestion = new HashMap<Association, String[]>();
		_log.info("find associations with " + finder);
		associations.addAll(finder.findAssociations(dataModel, namingSuggestion, session, executionContext));

		Collection<Association> associationsToWrite = new ArrayList<Association>();
		CsvFile excludeAssociationsCSV = getExcludeAssociationsCSV(executionContext);
		for (Association association: associations) {
			if (!excludeAssociationsCSV.contains(new String[] {
					association.source.getName(),
					association.destination.getName(),
					null,
					association.getJoinCondition()
					})) {
				knownIdentifiers.putCondition(association.getUnrestrictedJoinCondition());
			}
		}
		for (Association association: associations) {
			if (!excludeAssociationsCSV.contains(new String[] {
					association.source.getName(),
					association.destination.getName(),
					null,
					association.getJoinCondition()
					})) {
				if (!contains(association, dataModel, knownIdentifiers)) {
					insert(association, dataModel);
					associationsToWrite.add(association);
				}
			}
		}

		Set<String> names = new HashSet<String>();
		for (Table table: dataModel.getTables()) {
			for (Association association: table.associations) {
				if (association.getName() != null) {
					names.add(association.getName());
				}
			}
		}

		Map<String, String> assocLines = new TreeMap<String, String>();

		for (Association association: associationsToWrite) {
			String firstInsert = " ";
			if (association.isInsertSourceBeforeDestination()) {
				firstInsert = "A";
			}
			if (association.isInsertDestinationBeforeSource()) {
				firstInsert = "B";
			}
			String card = "   ";
			if (association.getCardinality() != null) {
				card = association.getCardinality().toString();
			}
			String sep = "_to_";
			if (association.source.getName().charAt(0) >= 'A' && association.source.getName().charAt(0) <= 'Z') {
				sep = "_TO_";
			}
			String name = association.source.getName() + sep + association.destination.getName();
			if (namingSuggestion.containsKey(association)) {
				for (String nameSuggestion: namingSuggestion.get(association)) {
					name = nameSuggestion;
					if (!names.contains(name)) {
						break;
					}
				}
			}
			if (names.contains(name)) {
				for (int i = 1; ; ++i) {
					String nameWithSuffix = name + "_" + i;
					if (!names.contains(nameWithSuffix)) {
						name = nameWithSuffix;
						break;
					}
				}
			}
			names.add(name);
			assocLines.put(name, CsvFile.encodeCell(association.source.getName()) + "; " + CsvFile.encodeCell(association.destination.getName()) + "; " + firstInsert + "; " + card + "; " + CsvFile.encodeCell(association.getJoinCondition()) +
									 "; " + CsvFile.encodeCell(name) + "; " + CsvFile.encodeCell(association.getAuthor()));
		}

		StringBuilder associationDefinition = new StringBuilder();
		for (String line: assocLines.values()) {
			associationDefinition.append(line + PrintUtil.LINE_SEPARATOR);
		}

		resetAssociationFile(associationDefinition.toString(), executionContext);
		
		StringBuilder commentsDefinitions = new StringBuilder();
		finder.getComments().forEach((element, comment) -> {
			String line = CsvFile.encodeCell(element.a.getName() + (element.b != null? "." + element.b.name : "")) + "; "
						+ CsvFile.encodeCell(comment);
			commentsDefinitions.append(line + PrintUtil.LINE_SEPARATOR);
		});
		resetCommentsFile(commentsDefinitions.toString(), executionContext);
	}

	private static String ASSOC_HEADER = "# Table A; Table B; First-insert; Cardinality (opt); Join-condition; Name; Author" + PrintUtil.LINE_SEPARATOR;

	private static void resetAssociationFile(String associationDefinition, ExecutionContext executionContext) throws IOException {
		writeFile(getModelBuilderAssociationsFilename(executionContext),
				ASSOC_HEADER +
				associationDefinition);
	}

	private static String TABLE_HEADER = "# Name; Upsert; Primary Key; ; Author" + PrintUtil.LINE_SEPARATOR;

	private static void resetTableFile(String tableDefinitions, ExecutionContext executionContext) throws IOException {
		writeFile(getModelBuilderTablesFilename(executionContext),
				TABLE_HEADER +
				tableDefinitions);
	}

	private static String COLUMN_HEADER = "# Table; Columns" + PrintUtil.LINE_SEPARATOR;

	private static void resetColumnsFile(String columnsDefinitions, ExecutionContext executionContext) throws IOException {
		writeFile(getModelBuilderColumnsFilename(executionContext),
				COLUMN_HEADER +
				columnsDefinitions);
	}

	private static String COMMENT_HEADER = "# Table[.Column]; Comment" + PrintUtil.LINE_SEPARATOR;

	private static void resetCommentsFile(String commentsDefinitions, ExecutionContext executionContext) throws IOException {
		writeFile(getModelBuilderCommentsFilename(executionContext),
				COMMENT_HEADER +
				commentsDefinitions);
	}
	
	/**
	 * Inserts an association into a model.
	 *
	 * @param association the association
	 * @param dataModel the model
	 */
	private static void insert(Association association, DataModel dataModel) {
		Association associationA = association;
		Cardinality reversedCard = association.getCardinality();
		if (reversedCard != null) {
			reversedCard = reversedCard.reverse();
		}
		Association associationB = new Association(association.destination, association.destination, association.isInsertSourceBeforeDestination(), association.isInsertSourceBeforeDestination(), association.getJoinCondition(), dataModel, true, reversedCard);
		associationA.reversalAssociation = associationB;
		associationB.reversalAssociation = associationA;
		associationA.source.associations.add(associationA);
		associationB.source.associations.add(associationB);
	}

	/**
	 * Checks if table is one of Jailers working tables.
	 *
	 * @param table the table to check
	 * @param quoting
	 * @return <code>true</code> if table is one of Jailers working tables
	 */
	private static boolean isJailerTable(Table table, Quoting quoting) {
		String tName = quoting.unquote(table.getUnqualifiedName()).toUpperCase(Locale.ENGLISH);
		return SqlUtil.JAILER_TABLES.contains(tName)
			|| tName.startsWith(ImportFilterManager.MAPPINGTABLE_NAME_PREFIX)
			|| (tName.endsWith("_T") && SqlUtil.JAILER_TABLES.contains(tName.substring(0, tName.length() - 2)));
	}

	/**
	 * Checks if table is one of Jailers working tables.
	 *
	 * @param table the table to check
	 * @return <code>true</code> if table is one of Jailers working tables
	 */
	public static boolean isJailerTable(String table) {
		String tName = Quoting.normalizeIdentifier(table);
		return SqlUtil.JAILER_TABLES.contains(tName)
			|| tName.startsWith(ImportFilterManager.MAPPINGTABLE_NAME_PREFIX)
			|| (tName.endsWith("_T") && SqlUtil.JAILER_TABLES.contains(tName.substring(0, tName.length() - 2)));
	}

	/**
	 * Checks if an association is already in a model.
	 *
	 * @param association the association
	 * @param dataModel the model
	 * @param knownIdentifiers
	 * @return <code>true</code> iff association is already in model
	 */
	private static boolean contains(Association association, DataModel dataModel, KnownIdentifierMap knownIdentifiers) {
		for (Association a: association.source.associations) {
			if (a.source.equals(association.source)) {
				if (a.destination.equals(association.destination)) {
					if (a.isInsertDestinationBeforeSource() || !association.isInsertDestinationBeforeSource()) {
						if (a.isInsertSourceBeforeDestination() || !association.isInsertSourceBeforeDestination()) {
							String jcA = knownIdentifiers.getCondition(a.getJoinCondition());
							if (jcA == null) {
								jcA = a.getJoinCondition();
							}
							String jcB = knownIdentifiers.getCondition(association.getJoinCondition());
							if (jcB == null) {
								jcB = association.getJoinCondition();
							}
							if (jcA.equals(jcB)) {
								return true;
							}
						}
					}
				}
			}
		}
		return false;
	}

	/**
	 * Writes content into a file.
	 *
	 * @param content the content
	 * @param fileName the name of the file
	 */
	private static void writeFile(String fileName, String content) throws IOException {
		File f = new File(fileName);
		if (!f.exists()) {
			f.getParentFile().mkdirs();
		}
		PrintWriter out = new PrintWriter(new FileOutputStream(f));
		out.print(content);
		out.close();
		_log.info("file '" + fileName + "' written");
	}

	/**
	 * Resets 'model-builder-*.csv' files.
	 */
	public static void resetFiles(ExecutionContext executionContext) throws IOException {
		resetTableFile("", executionContext);
		resetAssociationFile("", executionContext);
	}

	/**
	 * Removes temporary files.
	 */
	public static void cleanUp(ExecutionContext executionContext) {
		File f = new File(getModelBuilderTablesFilename(executionContext));
		if (f.exists()) {
			f.delete();
			_log.info("File '" + f.getAbsolutePath() + "' removed");
		}
		f = new File(getModelBuilderAssociationsFilename(executionContext));
		if (f.exists()) {
			f.delete();
			_log.info("File '" + f.getAbsolutePath() + "' removed");
		}
		f = new File(getModelBuilderColumnsFilename(executionContext));
		if (f.exists()) {
			f.delete();
			_log.info("File '" + f.getAbsolutePath() + "' removed");
		}
		f = new File(getModelBuilderCommentsFilename(executionContext));
		if (f.exists()) {
			f.delete();
			_log.info("File '" + f.getAbsolutePath() + "' removed");
		}
	}

	public static CsvFile.LineFilter assocFilter = null;

	// TODO allow analyzing multiple schemas at once. GUI + CLI.

}
