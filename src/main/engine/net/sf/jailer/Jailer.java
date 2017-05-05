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
package net.sf.jailer;

import java.io.BufferedReader;
import java.io.File;
import java.io.StringReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.sql.DataSource;

import org.apache.log4j.Logger;

import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.database.BasicDataSource;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ddl.DDLCreator;
import net.sf.jailer.domainmodel.DomainModel;
import net.sf.jailer.entitygraph.EntityGraph;
import net.sf.jailer.modelbuilder.ModelBuilder;
import net.sf.jailer.progress.ProgressListener;
import net.sf.jailer.progress.ProgressListenerRegistry;
import net.sf.jailer.render.DataModelRenderer;
import net.sf.jailer.restrictionmodel.RestrictionModel;
import net.sf.jailer.subsetting.SubsettingEngine;
import net.sf.jailer.util.CancellationException;
import net.sf.jailer.util.CancellationHandler;
import net.sf.jailer.util.ClasspathUtil;
import net.sf.jailer.util.PrintUtil;
import net.sf.jailer.util.SqlScriptExecutor;
import net.sf.jailer.util.SqlUtil;

/**
 * Tool for database subsetting, schema browsing, and rendering. It exports
 * consistent, referentially intact row-sets from relational databases. It
 * removes obsolete data without violating integrity. It is DBMS agnostic (by
 * using JDBC), platform independent, and generates DbUnit datasets,
 * hierarchically structured XML, and topologically sorted SQL-DML.
 * 
 * <a href="http://jailer.sourceforge.net/">http://jailer.sourceforge.net</a>
 * 
 * @author Ralf Wisser
 */
public class Jailer {

	/**
	 * The logger.
	 */
	private static final Logger _log = Logger.getLogger(Jailer.class);

	/**
	 * Main-method for CLI.
	 * 
	 * @param args
	 *            arguments
	 */
	public static void main(String[] args) {
		final Thread mainThread = Thread.currentThread();
		Thread shutdownHook;
		Runtime.getRuntime().addShutdownHook(shutdownHook = new Thread("shutdown-hook") {
		    public void run() {
		        CancellationHandler.cancel(null);
		        try {
					mainThread.join();
				} catch (InterruptedException e) {
					// ignore
				}
		    }
		});
		try {
			System.setProperty("db2.jcc.charsetDecoderEncoder", "3");
		} catch (Exception e) {
		}
		try {
			jailerMain(args, new StringBuffer(), null);
		} catch (Exception e) {
			// Exception has already been logged
		} finally {
			try {
				Runtime.getRuntime().removeShutdownHook(shutdownHook);
			} catch (Exception e) {
				// ignore
			}
		}
	}

	/**
	 * Main-method for GUI.
	 * 
	 * @param args
	 *            arguments
	 * @param warnings
	 *            string-buffer to print warnings into, may be <code>null</code>
	 * @param progressListener listens to progess events, may be <code>null</code>
	 * @return <code>false</code> iff something went wrong
	 */
	public static boolean jailerMain(String[] args, StringBuffer warnings, ProgressListener progressListener) throws Exception {
		CancellationHandler.reset(null);
		Session.closeTemporaryTableSession();

		try {
			ProgressListenerRegistry.setProgressListener(progressListener);
			CommandLine commandLine = CommandLineParser.parse(args, false);
			if (commandLine == null) {
				return false;
			}
			ExecutionContext executionContext = new ExecutionContext(commandLine);

			String command = commandLine.arguments.get(0);
			if (!"create-ddl".equalsIgnoreCase(command)) {
				if (!"find-association".equalsIgnoreCase(command)) {
					_log.info("Jailer " + JailerVersion.VERSION);
				}
			}
			
			URL[] jdbcJarURLs = ClasspathUtil.toURLArray(commandLine.jdbcjar, commandLine.jdbcjar2);

			if ("check-domainmodel".equalsIgnoreCase(command)) {
				DataModel dataModel = new DataModel(executionContext);
				for (String rm : commandLine.arguments.subList(1, commandLine.arguments.size())) {
					if (dataModel.getRestrictionModel() == null) {
						dataModel.setRestrictionModel(new RestrictionModel(dataModel, executionContext));
					}
					URL modelURL = new File(rm).toURI().toURL();
					dataModel.getRestrictionModel().addRestrictionDefinition(modelURL, new HashMap<String, String>());
				}
				new DomainModel(dataModel).check();
			} else if ("render-datamodel".equalsIgnoreCase(command)) {
				if (commandLine.arguments.size() <= 1) {
					CommandLineParser.printUsage();
				} else {
					renderDataModel(commandLine.arguments, commandLine.withClosures, commandLine.schema, executionContext);
				}
			} else if ("import".equalsIgnoreCase(command)) {
				if (commandLine.arguments.size() != 6) {
					CommandLineParser.printUsage();
				} else {
					BasicDataSource dataSource = new BasicDataSource(commandLine.arguments.get(2), commandLine.arguments.get(3), commandLine.arguments.get(4),
							commandLine.arguments.get(5), 0, jdbcJarURLs);
					Session session = new Session(dataSource, dataSource.dbms, null, commandLine.transactional);
					try {
						new SqlScriptExecutor(session, commandLine.numberOfThreads).executeScript(commandLine.arguments.get(1), commandLine.transactional);
					} finally {
						try {
							session.shutDown();
						} catch (Exception e) {
							// ignore
						}
					}
				}
			} else if ("print-datamodel".equalsIgnoreCase(command)) {
				printDataModel(commandLine.arguments, commandLine.withClosures, executionContext);
			} else if ("export".equalsIgnoreCase(command)) {
				if (commandLine.arguments.size() != 6) {
					CommandLineParser.printUsage();
				} else {
					if (commandLine.maxNumberOfEntities > 0) {
						EntityGraph.maxTotalRowcount = commandLine.maxNumberOfEntities;
						_log.info("max-rowcount=" + EntityGraph.maxTotalRowcount);
					}
					
					if (commandLine.exportScriptFileName == null) {
						System.out.println("missing '-e' option");
						CommandLineParser.printUsage();
					} else {
						BasicDataSource dataSource = new BasicDataSource(commandLine.arguments.get(2), commandLine.arguments.get(3),
								commandLine.arguments.get(4), commandLine.arguments.get(5), 0, jdbcJarURLs);
						URL modelURL = new File(commandLine.arguments.get(1)).toURI().toURL();
						new SubsettingEngine(executionContext).export(modelURL, commandLine.exportScriptFileName, commandLine.deleteScriptFileName,
								dataSource, dataSource.dbms, commandLine.explain, executionContext.getScriptFormat());
					}
				}
			} else if ("delete".equalsIgnoreCase(command)) {
				if (commandLine.arguments.size() != 6) {
					CommandLineParser.printUsage();
				} else {
					if (commandLine.deleteScriptFileName == null) {
						System.out.println("missing '-d' option");
						CommandLineParser.printUsage();
					} else {
						BasicDataSource dataSource = new BasicDataSource(commandLine.arguments.get(2), commandLine.arguments.get(3),
								commandLine.arguments.get(4), commandLine.arguments.get(5), 0, jdbcJarURLs);
						// note we are passing null for script format and the export script name, as we are using the export tool
						// to generate the delete script only.
						URL modelURL = new File(commandLine.arguments.get(1)).toURI().toURL();
						new SubsettingEngine(executionContext).export(modelURL, /* clp.exportScriptFileName*/ null, commandLine.deleteScriptFileName,
								dataSource, dataSource.dbms, commandLine.explain, /*scriptFormat*/ null);
					}
				}
			} else if ("find-association".equalsIgnoreCase(command)) {
				if (commandLine.arguments.size() < 3) {
					CommandLineParser.printUsage();
				} else {
					findAssociation(commandLine.arguments.get(1), commandLine.arguments.get(2), commandLine.arguments.subList(3, commandLine.arguments.size()), commandLine.undirected, executionContext);
				}
			} else if ("create-ddl".equalsIgnoreCase(command)) {
				if (commandLine.arguments.size() == 5) {
					BasicDataSource dataSource = new BasicDataSource(commandLine.arguments.get(1), commandLine.arguments.get(2), commandLine.arguments.get(3), commandLine.arguments.get(4), 0, jdbcJarURLs);
					return new DDLCreator(executionContext).createDDL(dataSource, dataSource.dbms, executionContext.getScope(), commandLine.workingTableSchema);
				}
				return new DDLCreator(executionContext).createDDL((DataSource) null, null, executionContext.getScope(), commandLine.workingTableSchema);
			} else if ("build-model-wo-merge".equalsIgnoreCase(command)) {
				if (commandLine.arguments.size() != 5) {
					CommandLineParser.printUsage();
				} else {
					_log.info("Building data model.");
					BasicDataSource dataSource = new BasicDataSource(commandLine.arguments.get(1), commandLine.arguments.get(2), commandLine.arguments.get(3), commandLine.arguments.get(4), 0, jdbcJarURLs);
					ModelBuilder.build(dataSource, dataSource.dbms, commandLine.schema, warnings, executionContext);
				}
			} else if ("build-model".equalsIgnoreCase(command)) {
				if (commandLine.arguments.size() != 5) {
					CommandLineParser.printUsage();
				} else {
					_log.info("Building data model.");
					BasicDataSource dataSource = new BasicDataSource(commandLine.arguments.get(1), commandLine.arguments.get(2), commandLine.arguments.get(3), commandLine.arguments.get(4), 0, jdbcJarURLs);
					ModelBuilder.buildAndMerge(dataSource, dataSource.dbms, commandLine.schema, warnings, executionContext);
				}
			} else {
				CommandLineParser.printUsage();
				return false;
			}
			return true;
		} catch (Exception e) {
			if (e instanceof CancellationException) {
				_log.warn("cancelled");
				throw e;
			}
			_log.error(e.getMessage(), e);
			System.out.println("Error: " + e.getClass().getName() + ": " + e.getMessage());
			String workingDirectory = System.getProperty("user.dir");
			_log.error("working directory is " + workingDirectory);
			throw e;
		} finally {
			ProgressListenerRegistry.setProgressListener(null);
			Session.closeTemporaryTableSession();
		}
	}

	/**
	 * Render the data model.
	 * 
	 * @param schema schema to analyze
	 */
	private static void renderDataModel(List<String> arguments, boolean withClosures, String schema, ExecutionContext executionContext) throws Exception {
		DataModel dataModel = new DataModel(executionContext);
		for (String rm : arguments.subList(1, arguments.size())) {
			if (dataModel.getRestrictionModel() == null) {
				dataModel.setRestrictionModel(new RestrictionModel(dataModel, executionContext));
			}
			URL modelURL = new File(rm).toURI().toURL();
			dataModel.getRestrictionModel().addRestrictionDefinition(modelURL, new HashMap<String, String>());
		}
		DataModelRenderer renderer = Configuration.getInstance().getRenderer();
		if (renderer == null) {
			throw new RuntimeException("no renderer found");
		}
		renderer.render(dataModel, executionContext, arguments.subList(1, arguments.size()));
	}

	/**
	 * Prints shortest association between two tables.
	 */
	private static void findAssociation(String from, String to, List<String> restModels, boolean undirected, ExecutionContext executionContext) throws Exception {
		DataModel dataModel = new DataModel(executionContext);
		for (String rm : restModels) {
			if (dataModel.getRestrictionModel() == null) {
				dataModel.setRestrictionModel(new RestrictionModel(dataModel, executionContext));
			}
			URL modelURL = new File(rm).toURI().toURL();
			dataModel.getRestrictionModel().addRestrictionDefinition(modelURL, new HashMap<String, String>());
		}
		Table source = dataModel.getTable(from);
		if (source == null) {
			throw new RuntimeException("unknown table: '" + from);
		}
		Table destination = dataModel.getTable(to);
		if (destination == null) {
			throw new RuntimeException("unknown table: '" + to);
		}

		System.out.println();
		System.out.println("Shortest path from " + source.getName() + " to " + destination.getName() + ":");

		Map<Table, Table> successor = new HashMap<Table, Table>();
		Map<Table, Association> outgoingAssociation = new HashMap<Table, Association>();
		List<Table> agenda = new ArrayList<Table>();
		agenda.add(destination);

		while (!agenda.isEmpty()) {
			Table table = agenda.remove(0);
			for (Association association : incomingAssociations(table, undirected)) {
				if (!successor.containsKey(association.source)) {
					successor.put(association.source, table);
					outgoingAssociation.put(association.source, association);
					agenda.add(association.source);
					if (association.source.equals(source)) {
						agenda.clear();
						break;
					}
				}
			}
		}
		if (successor.containsKey(source)) {
			String joinedSelect = "Select * From " + source.getName();
			System.out.println("    " + source.getName());
			for (Table table = source; !table.equals(destination); table = successor.get(table)) {
				Association association = outgoingAssociation.get(table);
				System.out.println("    " + association);
				joinedSelect += " join "
						+ association.destination.getName()
						+ " on "
						+ (association.reversed ? SqlUtil.replaceAliases(association.getJoinCondition(), association.destination.getName(), association.source
								.getName()) : SqlUtil.replaceAliases(association.getJoinCondition(), association.source.getName(), association.destination
								.getName()));
			}
			System.out.println();
			System.out.println();
			System.out.println("SQL query:");
			System.out.println("    " + joinedSelect);
		} else {
			System.out.println("tables are not associated");
		}
	}

	/**
	 * Prints restricted data-model.
	 */
	private static void printDataModel(List<String> restrictionModels, boolean printClosures, ExecutionContext executionContext) throws Exception {
		DataModel dataModel = new DataModel(executionContext);
		if (printClosures) {
			DataModel.printClosures = true;
		}
		for (String rm : restrictionModels.subList(1, restrictionModels.size())) {
			if (dataModel.getRestrictionModel() == null) {
				dataModel.setRestrictionModel(new RestrictionModel(dataModel, executionContext));
			}
			URL modelURL = new File(rm).toURI().toURL();
			dataModel.getRestrictionModel().addRestrictionDefinition(modelURL, new HashMap<String, String>());
		}

		BufferedReader in = new BufferedReader(new StringReader(dataModel.toString()));
		String line;
		while ((line = in.readLine()) != null) {
			System.out.println(line);
			CancellationHandler.checkForCancellation(null);
		}

		printCycles(dataModel);
		printComponents(dataModel, executionContext);
	}

	/**
	 * Searches cycles in a data-model and prints out all tables involved in a
	 * cycle.
	 * 
	 * @param dataModel
	 *            the data-model
	 */
	private static void printCycles(DataModel dataModel) {
		Set<Table> independentTables;
		Set<Table> tables = new HashSet<Table>(dataModel.getTables());
		do {
			independentTables = dataModel.getIndependentTables(tables);
			tables.removeAll(independentTables);
		} while (!independentTables.isEmpty());
		if (tables.isEmpty()) {
			System.out.println("no cyclic dependencies" + SubsettingEngine.asString(tables));
		} else {
			System.out.println("tables in dependent-cycle: " + SubsettingEngine.asString(tables));
		}
	}

	/**
	 * Searches components in a data-model and prints out all components.
	 * 
	 * @param dataModel
	 *            the data-model
	 */
	private static void printComponents(DataModel dataModel, ExecutionContext executionContext) {
		List<Set<Table>> components = new ArrayList<Set<Table>>();
		Set<Table> tables = new HashSet<Table>(dataModel.getTables());
		while (!tables.isEmpty()) {
			Table table = tables.iterator().next();
			Set<Table> closure = table.closure(new HashSet<Table>(), false);
			components.add(closure);
			tables.removeAll(closure);
		}
		System.out.println(components.size() + " components: ");
		for (Set<Table> component : components) {
			System.out.println(new PrintUtil().tableSetAsString(component));
		}
	}

	/**
	 * Collects all non-ignored associations with a given table as destination.
	 * 
	 * @param table
	 *            the table
	 * @return all non-ignored associations with table as destination
	 */
	private static Collection<Association> incomingAssociations(Table table, boolean undirected) {
		Collection<Association> result = new ArrayList<Association>();
		for (Association association : table.associations) {
			if (association.reversalAssociation.getJoinCondition() != null || (undirected && association.getJoinCondition() != null)) {
				result.add(association.reversalAssociation);
			}
		}
		return result;
	}

}
