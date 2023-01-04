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
package net.sf.jailer;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.sql.DataSource;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.BasicDataSource;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.PrimaryKeyFactory;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ddl.DDLCreator;
import net.sf.jailer.extractionmodel.ExtractionModel;
import net.sf.jailer.extractionmodel.ExtractionModel.AdditionalSubject;
import net.sf.jailer.modelbuilder.ModelBuilder;
import net.sf.jailer.progress.ProgressListener;
import net.sf.jailer.render.DataModelRenderer;
import net.sf.jailer.render.HtmlDataModelRenderer;
import net.sf.jailer.restrictionmodel.RestrictionModel;
import net.sf.jailer.subsetting.SubsettingEngine;
import net.sf.jailer.util.CancellationException;
import net.sf.jailer.util.CancellationHandler;
import net.sf.jailer.util.ClasspathUtil;
import net.sf.jailer.util.LogUtil;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlScriptExecutor;

/**
 * Jailer is a tool for database subsetting and relational data browsing. <br>
 * <ul>
 * <li>
 * The Subsetter exports consistent, referentially intact row-sets from relational databases,
 * generates topologically sorted SQL-DML, DbUnit datasets
 * and hierarchically structured XML.
 * </li>
 * <li>
 * The Data Browser allows bidirectional navigation through the database
 * by following foreign-key-based or user-defined relationships.
 * </li>
 * </ul>
 *
 * <a href="http://jailer.sourceforge.net/">http://jailer.sourceforge.net</a> <br>
 * <a href="https://github.com/Wisser/Jailer">https://github.com/Wisser/Jailer</a> <br><br>
 *
 * @author Ralf Wisser
 */
public class Jailer {

	/**
	 * The logger.
	 */
	private static Logger logger;

	/**
	 * Gets the logger.
	 */
	private static synchronized Logger getLogger() {
		if (logger == null) {
			logger = LoggerFactory.getLogger(Jailer.class);
		}
		return logger;
	}

	/**
	 * Main-method for CLI.
	 *
	 * @param args arguments
	 */
	public static void main(String[] args) {
		final AtomicBoolean cleanUpFinished = new AtomicBoolean(false);
		Thread shutdownHook;
		Runtime.getRuntime().addShutdownHook(shutdownHook = new Thread("shutdown-hook") {
			@Override
			public void run() {
				CancellationHandler.cancel(null);
				while (!cleanUpFinished.get()) {
					try {
						Thread.sleep(1000);
					} catch (InterruptedException e) {
						e.printStackTrace();
					}
				}
			}
		});

		try {
			System.setProperty("com.sun.xml.bind.v2.bytecode.ClassTailor.noOptimize", "true");
		} catch (Exception e) {
		}

		if (new File(".singleuser").exists() // legacy
				|| new File(".multiuser").exists()) {
			File home = new File(System.getProperty("user.home"), ".jailer");
			home.mkdirs();
			LogUtil.initLog4jConfig(home);
			Configuration configuration = Configuration.getInstance();
			configuration.setTempFileFolder(new File(home, "tmp").getPath());
			try {
				HtmlDataModelRenderer renderer = configuration.getRenderer();
				if (renderer != null) {
					renderer.setOutputFolder(new File(home, renderer.getOutputFolder()).getAbsolutePath());
				}
			} catch (Exception e) {
			}
		} else {
			LogUtil.initLog4jConfig(null);
		}
		getLogger();
		try {
			System.setProperty("db2.jcc.charsetDecoderEncoder", "3");
		} catch (Exception e) {
		}
		boolean ok = true;
		try {
			jailerMain(args, new StringBuffer(), null, true);
		} catch (Throwable t) {
			// Exception has already been logged
			ok = false;
		} finally {
			try {
				Runtime.getRuntime().removeShutdownHook(shutdownHook);
			} catch (Exception e) {
				// ignore
			}
		}
		cleanUpFinished.set(true);
		if (!ok) {
			System.exit(1);
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
	public static boolean jailerMain(String[] args, StringBuffer warnings, ProgressListener progressListener, boolean fromCli) throws Exception {
		CancellationHandler.reset(null);
		String pw = null;

		try {
			CommandLine commandLine = CommandLineParser.parse(args, false);
			if (commandLine == null) {
				return false;
			}
			ExecutionContext executionContext = new ExecutionContext(commandLine);

			if (progressListener != null) {
				executionContext.getProgressListenerRegistry().addProgressListener(progressListener);
			}

			String command = commandLine.arguments.get(0);
			if (!"create-ddl".equalsIgnoreCase(command)) {
				if (!"print-closure".equalsIgnoreCase(command)) {
					getLogger().info("Jailer " + JailerVersion.VERSION);
				}
			}

			URL[] jdbcJarURLs = ClasspathUtil.toURLArray(commandLine.jdbcjar, commandLine.jdbcjar2, commandLine.jdbcjar3, commandLine.jdbcjar4);

			if ("render-datamodel".equalsIgnoreCase(command)) {
				if (commandLine.arguments.size() > 2) {
					CommandLineParser.printUsage(args);
				} else {
					if (commandLine.arguments.size() > 1) {
						updateDataModelFolder(commandLine, commandLine.arguments.get(1), executionContext);
					}
					renderDataModel(commandLine.arguments, commandLine.schema, executionContext);
				}
			} else if ("import".equalsIgnoreCase(command)) {
				checkPW(commandLine, 6);
				if (commandLine.arguments.size() != 6) {
					CommandLineParser.printUsage(args);
				} else {
					BasicDataSource dataSource = new BasicDataSource(commandLine.arguments.get(2), commandLine.arguments.get(3), commandLine.arguments.get(4),
							pw = commandLine.arguments.get(5), 0, jdbcJarURLs);
					Session session = new Session(dataSource, dataSource.dbms, commandLine.isolationLevel, null, commandLine.transactional);
					try {
						new SqlScriptExecutor(session, commandLine.numberOfThreads, false).executeScript(commandLine.arguments.get(1), commandLine.transactional);
					} finally {
						try {
							session.shutDown();
						} catch (Exception e) {
							// ignore
						}
					}
				}
			} else if ("export".equalsIgnoreCase(command)) {
				checkPW(commandLine, 6);
				if (commandLine.arguments.size() != 6) {
					CommandLineParser.printUsage(args);
				} else {
					pw = commandLine.arguments.get(5);

					if (commandLine.exportScriptFileName == null) {
						System.out.println("missing '-e' option");
						CommandLineParser.printUsage(args);
					} else {
						updateDataModelFolder(commandLine, commandLine.arguments.get(1), executionContext);
						if (!commandLine.independentWorkingTables) {
							PrimaryKeyFactory.createUPKScope(commandLine.arguments.get(1), executionContext);
						}

						BasicDataSource dataSource = new BasicDataSource(commandLine.arguments.get(2), commandLine.arguments.get(3),
								commandLine.arguments.get(4), commandLine.arguments.get(5), 0, jdbcJarURLs);
						URL modelURL = new File(commandLine.arguments.get(1)).toURI().toURL();
						new SubsettingEngine(executionContext).export(commandLine.where, modelURL, commandLine.exportScriptFileName, commandLine.deleteScriptFileName,
								dataSource, dataSource.dbms, executionContext.getScriptFormat(), 0);
					}
				}
			} else if ("delete".equalsIgnoreCase(command)) {
				checkPW(commandLine, 6);
				if (commandLine.arguments.size() != 6) {
					CommandLineParser.printUsage(args);
				} else {
					pw = commandLine.arguments.get(5);
					if (commandLine.deleteScriptFileName == null) {
						System.out.println("can't delete: missing '-d' option");
						CommandLineParser.printUsage(args);
					} else {
						updateDataModelFolder(commandLine, commandLine.arguments.get(1), executionContext);
						BasicDataSource dataSource = new BasicDataSource(commandLine.arguments.get(2), commandLine.arguments.get(3),
								commandLine.arguments.get(4), commandLine.arguments.get(5), 0, jdbcJarURLs);
						// note we are passing null for script format and the export script name, as we are using the export tool
						// to generate the delete script only.
						if (!commandLine.independentWorkingTables) {
							PrimaryKeyFactory.createUPKScope(commandLine.arguments.get(1), executionContext);
						}
						URL modelURL = new File(commandLine.arguments.get(1)).toURI().toURL();
						new SubsettingEngine(executionContext).export(commandLine.where, modelURL, /* clp.exportScriptFileName*/ null, commandLine.deleteScriptFileName,
								dataSource, dataSource.dbms, /*scriptFormat*/ null, 0);
					}
				}
			} else if ("print-closure".equalsIgnoreCase(command)) {
				if (commandLine.arguments.size() < 2) {
					CommandLineParser.printUsage(args);
				} else {
					updateDataModelFolder(commandLine, commandLine.arguments.get(1), executionContext);
					printClosure(commandLine.arguments.get(1), commandLine.arguments.size() > 2? commandLine.arguments.get(2) : null, executionContext);
				}
			} else if ("create-ddl".equalsIgnoreCase(command)) {
				checkPW(commandLine, 5);
				String extractionModelFileName = null;
				if (!commandLine.independentWorkingTables && commandLine.arguments.size() > 5) {
					extractionModelFileName = commandLine.arguments.get(5);
				} else if (!commandLine.independentWorkingTables && commandLine.arguments.size() != 5 && commandLine.arguments.size() > 1) {
					extractionModelFileName = commandLine.arguments.get(1);
				}
				updateDataModelFolder(commandLine, extractionModelFileName, executionContext);
				if (commandLine.arguments.size() >= 5) {
					pw = commandLine.arguments.get(4);
					if (!commandLine.independentWorkingTables && commandLine.arguments.size() > 5) {
						PrimaryKeyFactory.createUPKScope(extractionModelFileName, executionContext);
					}
					BasicDataSource dataSource = new BasicDataSource(commandLine.arguments.get(1), commandLine.arguments.get(2), commandLine.arguments.get(3), commandLine.arguments.get(4), 0, jdbcJarURLs);
					return new DDLCreator(executionContext).createDDL(dataSource, dataSource.dbms, executionContext.getScope(), commandLine.workingTableSchema);
				}
				if (!commandLine.independentWorkingTables && commandLine.arguments.size() > 1) {
					PrimaryKeyFactory.createUPKScope(extractionModelFileName, executionContext);
				}
				if (commandLine.targetDBMS == null) {
					List<DBMS> dbmss = Configuration.getInstance().getDBMS();
					System.err.println("");
					System.err.println("Warning: No DBMS specified (\"-target-dbms\". The worktables are potentially suboptimal. Perfomance could suffer. Known DBMS are:");
					for (DBMS dbms: dbmss) {
						if (dbms.getId() != null) {
							System.err.println(dbms.getId());
						}
					}
					System.err.println("");
				}
				return new DDLCreator(executionContext).createDDL((DataSource) null, null, executionContext.getScope(), commandLine.workingTableSchema);
			} else if ("build-model-wo-merge".equalsIgnoreCase(command)) {
				checkPW(commandLine, 5);
				if (commandLine.arguments.size() != 5) {
					CommandLineParser.printUsage(args);
				} else {
					pw = commandLine.arguments.get(4);
					getLogger().info("Building data model.");
					BasicDataSource dataSource = new BasicDataSource(commandLine.arguments.get(1), commandLine.arguments.get(2), commandLine.arguments.get(3), commandLine.arguments.get(4), 0, jdbcJarURLs);
					ModelBuilder.build(dataSource, dataSource.dbms, commandLine.schema, warnings, executionContext);
				}
			} else if ("build-model".equalsIgnoreCase(command)) {
				checkPW(commandLine, 5);
				if (commandLine.arguments.size() != 5) {
					CommandLineParser.printUsage(args);
				} else {
					pw = commandLine.arguments.get(4);
					getLogger().info("Building data model.");
					BasicDataSource dataSource = new BasicDataSource(commandLine.arguments.get(1), commandLine.arguments.get(2), commandLine.arguments.get(3), commandLine.arguments.get(4), 0, jdbcJarURLs);
					ModelBuilder.buildAndMerge(dataSource, dataSource.dbms, commandLine.schema, warnings, executionContext);
				}
			} else {
				CommandLineParser.printUsage(args);
				return false;
			}
			return true;
		} catch (Throwable t) {
			if (t instanceof CancellationException) {
				getLogger().warn("cancelled");
				throw t;
			}
			getLogger().error(t.getMessage(), t);
			System.err.println("Error: " + t.getClass().getName() + ": " + t.getMessage());
			CommandLineParser.printAruments(System.err, args, pw);
			String workingDirectory = System.getProperty("user.dir");
			getLogger().error("working directory is " + workingDirectory);
			throw t;
		}
	}

	private static void checkPW(CommandLine commandLine, int i) {
		if (commandLine.arguments.size() == i - 1) {
			commandLine.arguments.add("");
		}
	}

	private static void printClosure(String extractionModelFileName, String separator, ExecutionContext executionContext) throws MalformedURLException, IOException {
		ExtractionModel extractionModel = new ExtractionModel(new File(extractionModelFileName).toURI().toURL(), executionContext.getSourceSchemaMapping(), executionContext.getParameters(), executionContext, true);
		Set<Table> subjects = new HashSet<Table>();
		if (extractionModel.additionalSubjects != null) {
			for (AdditionalSubject as: extractionModel.additionalSubjects) {
				subjects.add(as.getSubject());
			}
		}
		subjects.add(extractionModel.subject);

		Set<String> closure = new TreeSet<String>();
		Set<Table> toIgnore = new HashSet<Table>();
		for (Table subject: subjects) {
			for (Table table: subject.closure(toIgnore)) {
				closure.add(Quoting.unquotedTableName(table, executionContext));
				toIgnore.add(table);
			}
		}

		int row = 0;
		for (String tableName: closure) {
			if (separator == null) {
				System.out.println(tableName);
			} else {
				if (row++ > 0) {
					System.out.print(separator);
				}
				System.out.print(tableName);
			}
		}
		if (separator != null) {
			System.out.println();
		}
	}

	private static void updateDataModelFolder(CommandLine commandLine, String extractionModelFileName,
			ExecutionContext executionContext) throws IOException {
		if (extractionModelFileName != null && "datamodel".equals(commandLine.datamodelFolder)) {
			String datamodelFolder = ExtractionModel.loadDatamodelFolder(extractionModelFileName, executionContext);
			if (datamodelFolder != null) {
				executionContext.setCurrentModelSubfolder(datamodelFolder);
			}
		}
	}

	/**
	 * Render the data model.
	 *
	 * @param schema schema to analyze
	 */
	private static void renderDataModel(List<String> arguments, String schema, ExecutionContext executionContext) throws Exception {
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
		renderer.render(dataModel, arguments.subList(1, arguments.size()));
	}

}
