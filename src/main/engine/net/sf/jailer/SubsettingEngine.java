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

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.nio.charset.Charset;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;
import java.util.zip.GZIPOutputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.sql.DataSource;
import javax.xml.transform.sax.TransformerHandler;
import javax.xml.transform.stream.StreamResult;

import org.apache.log4j.Logger;
import org.xml.sax.helpers.AttributesImpl;

import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.DMLTransformer;
import net.sf.jailer.database.DeletionTransformer;
import net.sf.jailer.database.Session;
import net.sf.jailer.database.StatisticRenovator;
import net.sf.jailer.database.TemporaryTableScope;
import net.sf.jailer.datamodel.AggregationSchema;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Cardinality;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Filter;
import net.sf.jailer.datamodel.ParameterHandler;
import net.sf.jailer.datamodel.RowIdSupport;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.dbunit.FlatXMLTransformer;
import net.sf.jailer.enhancer.ScriptEnhancer;
import net.sf.jailer.entitygraph.EntityGraph;
import net.sf.jailer.entitygraph.intradatabase.IntraDatabaseEntityGraph;
import net.sf.jailer.entitygraph.local.LocalEntityGraph;
import net.sf.jailer.entitygraph.remote.RemoteEntityGraph;
import net.sf.jailer.extractionmodel.ExtractionModel;
import net.sf.jailer.extractionmodel.ExtractionModel.AdditionalSubject;
import net.sf.jailer.importfilter.ImportFilterManager;
import net.sf.jailer.liquibase.LiquibaseXMLTransformer;
import net.sf.jailer.progress.ProgressListener;
import net.sf.jailer.progress.ProgressListenerRegistry;
import net.sf.jailer.util.CancellationException;
import net.sf.jailer.util.CancellationHandler;
import net.sf.jailer.util.CycleFinder;
import net.sf.jailer.util.JobManager;
import net.sf.jailer.util.PrintUtil;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.xml.XmlExportTransformer;
import net.sf.jailer.xml.XmlUtil;

/**
 * The Subsetting Engine.
 * 
 * @author Ralf Wisser
 */
public class SubsettingEngine {
	
	/**
	 * Constructor.
	 * 
	 * @param executionContext the command line arguments
	 */
	public SubsettingEngine(ExecutionContext executionContext) throws Exception {
		this.executionContext = executionContext;
		jobManager = new JobManager(executionContext.getNumberOfThreads());
	}

	/**
	 * The relational data model.
	 */
	private DataModel datamodel;

	/**
	 * The entity-graph to be used for finding the transitive closure.
	 */
	private EntityGraph entityGraph;
	
	/**
	 * The execution context.
	 */
	private final ExecutionContext executionContext;
	
	/**
	 * The job-manager to be used for concurrent execution of jobs.
	 */
	private final JobManager jobManager;

	/**
	 * The logger.
	 */
	private static final Logger _log = Logger.getLogger(SubsettingEngine.class);

	/**
	 * Comment header of the export-script.
	 */
	private StringBuffer commentHeader = new StringBuffer();
	
	/**
	 * Sets the entity-graph to be used for finding the transitive closure.
	 * 
	 * @param entityGraph
	 *            the entity-graph to be used for finding the transitive closure
	 */
	private void setEntityGraph(EntityGraph entityGraph) {
		this.entityGraph = entityGraph;
	}

	/**
	 * Sets the restricted data-model to be used for extraction.
	 * 
	 * @param dataModel
	 *            the restricted data-model to be used for extraction
	 */
	private void setDataModel(DataModel dataModel) {
		this.datamodel = dataModel;
	}

	/**
	 * Appends a line to the comment-header of the export script.
	 * 
	 * @param comment
	 *            the comment line (without '--'-prefix)
	 */
	private void appendCommentHeader(String comment) {
		commentHeader.append("-- " + (comment.replace('\n', ' ').replace('\r', ' ')) + "\n");
	}

	/**
	 * Exports rows from table.
	 * 
	 * @param table
	 *            the table
	 * @param condition
	 *            the condition (in SQL) the exported rows must fulfill

	 * @param progressOfYesterday
	 *            set of tables to account for resolvation
	 * @param completedTables 
	 * 
	 * @return set of tables from which entities are added
	 */
	private Set<Table> export(Table table, String condition, Collection<Table> progressOfYesterday, boolean skipRoot, Set<Table> completedTables) throws Exception {
		_log.info("exporting " + datamodel.getDisplayName(table) + " Where " + condition.replace('\n', ' ').replace('\r', ' '));
		int today = entityGraph.getAge();
		entityGraph.setAge(today + 1);
		Map<Table, Collection<Association>> progress = new HashMap<Table, Collection<Association>>();
		if (!skipRoot) {
			ProgressListenerRegistry.getProgressListener().collectionJobEnqueued(today, table);
			ProgressListenerRegistry.getProgressListener().collectionJobStarted(today, table);
			long rc = entityGraph.addEntities(table, condition, today);
			ProgressListenerRegistry.getProgressListener().collected(today, table, rc);
			if (rc > 0) {
				progress.put(table, new ArrayList<Association>());
			}
		}
		if (progressOfYesterday != null) {
			for (Table t: progressOfYesterday) {
				progress.put(t, new ArrayList<Association>());
			}
		}
		Set<Table> totalProgress = new HashSet<Table>();

		while (!progress.isEmpty()) {
			totalProgress.addAll(progress.keySet());
			_log.info("day " + today + ", progress: " + asString(progress.keySet()));
			++today;
			entityGraph.setAge(today + 1);
			progress = resolveAssociations(today, progress, completedTables);
		}

		_log.info("exported " + datamodel.getDisplayName(table) + " Where " + condition.replace('\n', ' ').replace('\r', ' '));
		_log.info("total progress: " + asString(totalProgress));
		_log.info("export statistic:");
		boolean firstLine = true;
		for (String line : entityGraph.getStatistics(datamodel, new HashSet<Table>())) {
			String l = (firstLine ? "Exported Rows:     " : "    ") + line;
			_log.info(l);
			appendCommentHeader(l);
			if (firstLine) {
				appendCommentHeader("");
			}
			firstLine = false;
		}
		appendCommentHeader("");
		boolean isFiltered = false;
		for (Table t : new TreeSet<Table>(totalProgress)) {
			for (Column c : t.getColumns()) {
				if (c.getFilterExpression() != null) {
					if (!isFiltered) {
						isFiltered = true;
						appendCommentHeader("Used Filters:");
					}
					String prefix = "";
					if (c.getFilter().getExpression().trim().startsWith(Filter.LITERAL_PREFIX)) {
			    		prefix = Filter.LITERAL_PREFIX + " ";
			    	}
					String suffix = c.getFilter().isApplyAtExport()? "" : " (applied at import phase)";
					appendCommentHeader("    " + t.getUnqualifiedName() + "." + c.name + " := " + prefix + c.getFilterExpression() + suffix);
				}
			}
		}

		return totalProgress;
	}

	/**
	 * Exports all entities from initial-data tables.
	 * 
	 * @param extractionModel the extraction model
	 */
	private Set<Table> exportSubjects(ExtractionModel extractionModel, Set<Table> completedTables) throws Exception {
		List<AdditionalSubject> allSubjects = new ArrayList<ExtractionModel.AdditionalSubject>();
		for (AdditionalSubject as: extractionModel.additionalSubjects) {
			allSubjects.add(new AdditionalSubject(as.getSubject(), ParameterHandler.assignParameterValues(as.getCondition(), executionContext.getParameters())));
		}
		allSubjects.add(new AdditionalSubject(extractionModel.subject, extractionModel.condition.equals("1=1")? "" : extractionModel.condition));
		Map<Table, String> conditionPerTable = new HashMap<Table, String>();
		for (AdditionalSubject as: allSubjects) {
			String cond = conditionPerTable.get(as.getSubject());
			if (cond == null || cond.trim().length() > 0) {
				if (as.getCondition().trim().length() > 0) {
					String newCond = "(" + as.getCondition() + ")";
					if (cond == null) {
						cond = newCond;
					} else {
						cond += " or " + newCond;
					}
				} else {
					cond = "";
				}
				conditionPerTable.put(as.getSubject(), cond);
			}
		}
		final Set<Table> progress = Collections.synchronizedSet(new HashSet<Table>());
		List<JobManager.Job> jobs = new ArrayList<JobManager.Job>();
		for (Map.Entry<Table, String> e: conditionPerTable.entrySet()) {
			final Table table = e.getKey();
			final String condition = e.getValue().trim();
			if (condition.length() > 0) {
				_log.info("exporting " + datamodel.getDisplayName(table) + " Where " + condition);
			} else {
				completedTables.add(table);
				_log.info("exporting all " + datamodel.getDisplayName(table));
			}
			jobs.add(new JobManager.Job() {
				@Override
				public void run() throws Exception {
					int today = entityGraph.getAge();
					ProgressListenerRegistry.getProgressListener().collectionJobEnqueued(today, table);
					ProgressListenerRegistry.getProgressListener().collectionJobStarted(today, table);
					long rc = entityGraph.addEntities(table, condition.length() > 0? condition : "1=1", today);
					if (rc > 0) {
						progress.add(table);
					}
					ProgressListenerRegistry.getProgressListener().collected(today, table, rc);
				}
			});
		}
		jobManager.executeJobs(jobs);
		return progress;
	}

	/**
	 * Resolves all associations defined in data-model.
	 * 
	 * @param today
	 *            birthday of newly created entities
	 * @param progressOfYesterday
	 *            set of tables to account for resolvation
	 * @param completedTables 
	 * 
	 * @return map from tables from which entities are added to all associations
	 *         which lead to the entities
	 */
	private Map<Table, Collection<Association>> resolveAssociations(final int today, Map<Table, Collection<Association>> progressOfYesterday, Set<Table> completedTables) throws Exception {
		final Map<Table, Collection<Association>> progress = new HashMap<Table, Collection<Association>>();

		// resolve associations with same dest-type sequentially
		Map<Table, List<JobManager.Job>> jobsPerDestination = new HashMap<Table, List<JobManager.Job>>();

		for (final Table table : progressOfYesterday.keySet()) {
			for (final Association association : table.associations) {
				Collection<Association> as = progressOfYesterday.get(table);
				if (as != null && as.size() == 1 && as.iterator().next() == association.reversalAssociation) {
					if (association.getCardinality() == Cardinality.MANY_TO_ONE || association.getCardinality() == Cardinality.ONE_TO_ONE) {
						_log.info("skip reversal association " + datamodel.getDisplayName(table) + " -> " + datamodel.getDisplayName(association.destination));
						continue;
					}
				}

				if (completedTables.contains(association.destination)) {
					_log.info("skip association " + datamodel.getDisplayName(table) + " -> " + datamodel.getDisplayName(association.destination) + ". All rows exported.");
					continue;
				}
				
				String jc = association.getJoinCondition();
		        if (jc != null) {
		        	ProgressListenerRegistry.getProgressListener().collectionJobEnqueued(today, association);
		        }
				JobManager.Job job = new JobManager.Job() {
					public void run() throws Exception {
						runstats(false);
						if (association.getJoinCondition() != null) {
							_log.info("resolving " + datamodel.getDisplayName(table) + " -> " + association.toString(0, true) + "...");
						}
						ProgressListenerRegistry.getProgressListener().collectionJobStarted(today, association);
						long rc = entityGraph.resolveAssociation(table, association, today);
						ProgressListenerRegistry.getProgressListener().collected(today, association, rc);
						if (rc >= 0) {
							_log.info(rc + " entities found resolving " + datamodel.getDisplayName(table) + " -> " + association.toString(0, true));
						}
						synchronized (progress) {
							if (rc > 0) {
								Collection<Association> as = progress.get(association.destination);
								if (as == null) {
									as = new ArrayList<Association>();
									progress.put(association.destination, as);
								}
								as.add(association);
							}
						}
					}
				};
				List<JobManager.Job> jobList = jobsPerDestination.get(association.destination);
				if (jobList == null) {
					jobList = new ArrayList<JobManager.Job>();
					jobsPerDestination.put(association.destination, jobList);
				}
				jobList.add(job);
			}
		}
		List<JobManager.Job> jobs = new ArrayList<JobManager.Job>();
		for (final Map.Entry<Table, List<JobManager.Job>> entry : jobsPerDestination.entrySet()) {
			jobs.add(new JobManager.Job() {
				public void run() throws Exception {
					for (JobManager.Job job : entry.getValue()) {
						job.run();
					}
				}
			});
		}
		jobManager.executeJobs(jobs);

		if (EntityGraph.maxTotalRowcount > 0 && EntityGraph.maxTotalRowcount < entityGraph.getTotalRowcount()) {
			throw new RuntimeException("found more than " + EntityGraph.maxTotalRowcount + " entities.");
		}

		return progress;
	}

	/**
	 * Adds all dependencies.
	 * 
	 * @param progress
	 *            set of tables to take into account
	 */
	private void addDependencies(Set<Table> progress, boolean treatAggregationAsDependency) throws Exception {
		List<JobManager.Job> jobs = new ArrayList<JobManager.Job>();
		Set<Association> done = new HashSet<Association>();
		for (final Table table : progress) {
			for (final Association association : table.associations) {
				if (done.contains(association.reversalAssociation)) {
					continue;
				}
				if (progress.contains(association.destination)) {
					final int aggregationId = treatAggregationAsDependency ? association.getId() : 0;
					final int dependencyId = association.getId();
					if (treatAggregationAsDependency) {
						if (association.getAggregationSchema() != AggregationSchema.NONE) {
							final String jc = association.getUnrestrictedJoinCondition();
							done.add(association);
							jobs.add(new JobManager.Job() {
								public void run() throws Exception {
									_log.info("find aggregation for " + datamodel.getDisplayName(table) + " -> "
											+ datamodel.getDisplayName(association.destination) + " on " + jc);
									String fromAlias, toAlias;
									fromAlias = association.reversed ? "B" : "A";
									toAlias = association.reversed ? "A" : "B";
									entityGraph.addDependencies(table, fromAlias, association.destination, toAlias, jc, aggregationId, dependencyId,
											association.reversed);
								}
							});
						}
					} else {
						final String jc = association.getUnrestrictedJoinCondition();
						if (jc != null && association.isInsertDestinationBeforeSource()) {
							done.add(association);
							jobs.add(new JobManager.Job() {
								public void run() throws Exception {
									_log.info("find dependencies " + datamodel.getDisplayName(table) + " -> "
											+ datamodel.getDisplayName(association.destination) + " on " + jc);
									String fromAlias, toAlias;
									fromAlias = association.reversed ? "B" : "A";
									toAlias = association.reversed ? "A" : "B";
									entityGraph.addDependencies(table, fromAlias, association.destination, toAlias, jc, aggregationId, dependencyId,
											association.reversed);
								}
							});
						}
//						if (jc != null && association.isInsertSourceBeforeDestination()) {
//							done.add(association);
//							jobs.add(new JobManager.Job() {
//								public void run() throws Exception {
//									_log.info("find dependencies " + datamodel.getDisplayName(association.destination) + " -> "
//											+ datamodel.getDisplayName(table) + " on " + jc);
//									String fromAlias, toAlias;
//									fromAlias = association.reversed ? "B" : "A";
//									toAlias = association.reversed ? "A" : "B";
//									entityGraph.addDependencies(association.destination, toAlias, table, fromAlias, jc, aggregationId, dependencyId,
//											association.reversed);
//								}
//							});
//						}
					}
				}
			}
		}
		jobManager.executeJobs(jobs);
	}

	/**
	 * Writes entities into extract-SQL-script.
	 * 
	 * @param table
	 *            write entities from this table only
	 * @param orderByPK
	 *            if <code>true</code>, result will be ordered by primary keys
	 */
	private void writeEntities(Table table, boolean orderByPK)
			throws Exception {
		entityGraph.readEntities(table, orderByPK);
		entityGraph.deleteEntities(table);
	}

	/**
	 * Retrieves the configuration of the target DBMS.
	 * 
	 * @param session the session
	 * @return configuration of the target DBMS
	 */
	private DBMS targetDBMSConfiguration(Session session) {
		DBMS targetDBMS = executionContext.getTargetDBMS();
		if (targetDBMS == null) {
			return session.dbms;
		}
		return targetDBMS;
	}

	/**
	 * Creates a factory for transformers for processing the rows to be exported.
	 * 
	 * @param outputWriter
	 *            writer into export file
	 * @param transformerHandler
	 *            SAX transformer handler for generating XML. <code>null</code>
	 *            if script format is not XML.
	 * @param scriptType
	 *            the script type
	 * 
	 * @return result set reader for processing the rows to be exported
	 */
	private TransformerFactory createTransformerFactory(OutputStreamWriter outputWriter, TransformerHandler transformerHandler, ScriptType scriptType, String filepath) throws SQLException	{
		Session targetSession = entityGraph.getTargetSession();
		if (scriptType == ScriptType.INSERT) {
			if (ScriptFormat.INTRA_DATABASE.equals(executionContext.getScriptFormat())) {
				return null;
			} if (ScriptFormat.DBUNIT_FLAT_XML.equals(executionContext.getScriptFormat())) {
				return new FlatXMLTransformer.Factory(transformerHandler, targetSession.getMetaData(), targetSession.dbms, executionContext);
			} else if (ScriptFormat.LIQUIBASE_XML.equals(executionContext.getScriptFormat())) {
				return new LiquibaseXMLTransformer.Factory(transformerHandler,targetSession.getMetaData(), entityGraph, filepath,
						executionContext.getXmlDatePattern(),
						executionContext.getXmlTimePattern(),
						executionContext.getXmlTimeStampPattern(), executionContext);
			} else {
				return new DMLTransformer.Factory(outputWriter, executionContext.getUpsertOnly(), executionContext.getNumberOfEntities(), targetSession, targetDBMSConfiguration(targetSession), executionContext);
			}
		} else {
			return new DeletionTransformer.Factory(outputWriter, executionContext.getNumberOfEntities(), targetSession, targetDBMSConfiguration(targetSession), executionContext);
		}
	}

	/**
	 * Writes entities into extract-SQL-script.
	 * 
	 * @param sqlScriptFile
	 *            the name of the sql-script to write the data to
	 * @param progress
	 *            set of tables to account for extraction
	 * @param stage stage name for {@link ProgressListener}
	 */
	private void writeEntities(final String sqlScriptFile, final ScriptType scriptType, final Set<Table> progress, Session session, String stage) throws Exception {
		_log.info("writing file '" + sqlScriptFile + "'...");

		OutputStream outputStream = new FileOutputStream(sqlScriptFile);
		if (sqlScriptFile.toLowerCase().endsWith(".zip")) {
			outputStream = new ZipOutputStream(outputStream);
			String zipFileName = new File(sqlScriptFile).getName();
			((ZipOutputStream)outputStream).putNextEntry(new ZipEntry(zipFileName.substring(0, zipFileName.length() - 4)));
		} else {
			if (sqlScriptFile.toLowerCase().endsWith(".gz")) {
				outputStream = new GZIPOutputStream(outputStream);
			}
		}
		TransformerHandler transformerHandler = null;
		ImportFilterManager importFilterManager = null;
		OutputStreamWriter result = null;
		Charset charset = Charset.defaultCharset();
		if (executionContext.getUTF8()) {
			charset = Charset.forName("UTF8");
		}
		if (scriptType == ScriptType.INSERT && ScriptFormat.DBUNIT_FLAT_XML.equals(executionContext.getScriptFormat())) {
			StreamResult streamResult = new StreamResult(new OutputStreamWriter(outputStream, charset));
			transformerHandler = XmlUtil.createTransformerHandler(commentHeader.toString(), "dataset", streamResult, charset);
		} else if(scriptType == ScriptType.INSERT && ScriptFormat.LIQUIBASE_XML.equals(executionContext.getScriptFormat())){
			StreamResult streamResult = new StreamResult(
					new OutputStreamWriter(outputStream,
							charset));
			
		
			transformerHandler = XmlUtil.createTransformerHandler(commentHeader.toString(), "", streamResult, charset);	//root tag removed to add namespaces 

			AttributesImpl attrdatabaseChangeLog = new AttributesImpl();
			attrdatabaseChangeLog.addAttribute("", "", "xmlns:xsi", "", "http://www.w3.org/2001/XMLSchema-instance");
			attrdatabaseChangeLog.addAttribute("", "", "xmlns:ext", "", "http://www.liquibase.org/xml/ns/dbchangelog-ext");
			attrdatabaseChangeLog.addAttribute("", "", "xsi:schemaLocation", "", "http://www.liquibase.org/xml/ns/dbchangelog http://www.liquibase.org/xml/ns/dbchangelog/dbchangelog-3.0.xsd http://www.liquibase.org/xml/ns/dbchangelog-ext http://www.liquibase.org/xml/ns/dbchangelog/dbchangelog-ext.xsd");
			transformerHandler.startElement("http://www.liquibase.org/xml/ns/dbchangelog", "", "databaseChangeLog",attrdatabaseChangeLog);
			
			AttributesImpl attrchangeset = new AttributesImpl();
			attrchangeset.addAttribute("", "", "id", "","JailerExport" );
			attrchangeset.addAttribute("", "", "author", "",System.getProperty("user.name") );
			
			transformerHandler.startElement("", "", "changeSet", attrchangeset);
		} else {
			if (executionContext.getUTF8()) {
				result = new OutputStreamWriter(outputStream, charset);
			} else {
				result = new OutputStreamWriter(outputStream);
			}
			result.append(commentHeader);
			// result.append(System.getProperty("line.separator"));
			for (ScriptEnhancer enhancer: Configuration.getScriptEnhancer()) {
				enhancer.addComments(result, scriptType, session, targetDBMSConfiguration(session), entityGraph, progress, executionContext);
			}
			// result.append(System.getProperty("line.separator"));
			for (ScriptEnhancer enhancer: Configuration.getScriptEnhancer()) {
				enhancer.addProlog(result, scriptType, session, targetDBMSConfiguration(session), entityGraph, progress, executionContext);
			}
			Session localSession = null;
			if (entityGraph instanceof LocalEntityGraph) {
				localSession = ((LocalEntityGraph) entityGraph).getSession();
			}
			DBMS sourceConfig = session.dbms;
			DBMS targetConfig = targetDBMSConfiguration(entityGraph.getTargetSession());
			Quoting targetQuoting;
			targetQuoting = new Quoting(session);
			if (sourceConfig != targetConfig) {
				targetQuoting.setIdentifierQuoteString(targetConfig.getIdentifierQuoteString());
			}
			importFilterManager = new ImportFilterManager(localSession, result, progress, targetQuoting, executionContext) {
				@Override
				protected void sync(OutputStreamWriter result) throws IOException {
					appendSync(result);
				}
			};

			entityGraph.setImportFilterManager(importFilterManager);
		}

		entityGraph.setTransformerFactory(createTransformerFactory(result, transformerHandler, scriptType, sqlScriptFile));
		if (importFilterManager != null && entityGraph.getTransformerFactory() instanceof DMLTransformer.Factory) {
			((DMLTransformer.Factory) entityGraph.getTransformerFactory()).setImportFilterTransformer(importFilterManager);
		}
		
		Session targetSession = entityGraph.getTargetSession();
		entityGraph.fillAndWriteMappingTables(jobManager, result, executionContext.getNumberOfEntities(), targetSession, targetDBMSConfiguration(targetSession), session.dbms);

		ProgressListenerRegistry.getProgressListener().newStage(stage, false, false);
		
		long rest = 0;
		Set<Table> dependentTables = null;
		Set<Table> currentProgress = progress;
		
		while (!currentProgress.isEmpty()) {
			// first write entities of independent tables
			dependentTables = writeEntitiesOfIndependentTables(result, transformerHandler, scriptType, currentProgress, sqlScriptFile);
			Set<Table> prevProgress = currentProgress;
			currentProgress = new HashSet<Table>();
			
			// then write entities of tables having cyclic-dependencies
			Set<Table> descendants = getDescentants(dependentTables);
			if (!descendants.isEmpty()) {
				dependentTables.removeAll(descendants);
				currentProgress = descendants;
				_log.info("cyclic dependency descendants: " + asString(descendants));
			}
			if (!dependentTables.isEmpty()) {
				_log.info("cyclic dependencies for: " + asString(dependentTables));
			}
			if (!executionContext.getNoSorting()) {
				addDependencies(dependentTables, false);
				runstats(true);
				removeSingleRowCycles(prevProgress, session);
			} else {
				_log.warn("skipping topological sorting");
			}
	
			rest = 0;
	
			if (scriptType == ScriptType.INSERT && (ScriptFormat.DBUNIT_FLAT_XML.equals(executionContext.getScriptFormat())||ScriptFormat.LIQUIBASE_XML.equals(executionContext.getScriptFormat()))) {
				Set<Table> remaining = new HashSet<Table>(dependentTables);
	
				// topologically sort remaining tables while ignoring reflexive
				// dependencies
				// and dependencies for which no edge exists in entity graph
				Set<Association> relevantAssociations = new HashSet<Association>(datamodel.namedAssociations.values());
				Set<Integer> existingEdges = entityGraph.getDistinctDependencyIDs();
				for (Iterator<Association> i = relevantAssociations.iterator(); i.hasNext();) {
					Association association = i.next();
					if (association.source.equals(association.destination)) {
						i.remove();
					} else if (!existingEdges.contains(association.getId())) {
						if (association.isInsertDestinationBeforeSource()) {
							_log.info("irrelevant dependency: " + datamodel.getDisplayName(association.source) + " -> "
									+ datamodel.getDisplayName(association.destination));
						}
						i.remove();
					}
				}
				Set<Table> independentTables = datamodel.getIndependentTables(remaining, relevantAssociations);
				rest = entityGraph.getSize(dependentTables);
				while (!independentTables.isEmpty()) {
					_log.info("independent tables: " + asString(independentTables));
					for (final Table independentTable : independentTables) {
						rest = entityGraph.getSize(dependentTables);
						for (;;) {
							entityGraph.markIndependentEntities(independentTable);
							// don't use jobManager, export rows sequentially, don't
							// mix rows of different tables in a dataset!
							entityGraph.readMarkedEntities(independentTable, true);
							entityGraph.deleteIndependentEntities(independentTable);
							long newRest = entityGraph.getSize(dependentTables);
							if (rest == newRest) {
								break;
							}
							rest = newRest;
						}
					}
					remaining.removeAll(independentTables);
					independentTables = datamodel.getIndependentTables(remaining, relevantAssociations);
				}
			} else {
				rest = writeIndependentEntities(result, dependentTables, entityGraph);
				appendSync(result);
				if (rest > 0) {
					EntityGraph egCopy = entityGraph.copy(EntityGraph.createUniqueGraphID(), entityGraph.getSession());
					egCopy.setImportFilterManager(entityGraph.getImportFilterManager());
					
					_log.info(rest + " entities in cycle. Involved tables: " + new PrintUtil(executionContext).tableSetAsString(dependentTables));
					Map<Table, Set<Column>> nullableForeignKeys = findAndRemoveNullableForeignKeys(dependentTables, entityGraph, scriptType != ScriptType.DELETE);
					_log.info("nullable foreign keys: " + nullableForeignKeys.values());

					ScriptFormat scriptFormat = executionContext.getScriptFormat();
					
					List<Runnable> resetFilters = new ArrayList<Runnable>();
					for (Map.Entry<Table, Set<Column>> entry: nullableForeignKeys.entrySet()) {
						for (final Column column: entry.getValue()) {
							final Filter filter = column.getFilter();
							resetFilters.add(new Runnable() {
								@Override
								public void run() {
									column.setFilter(filter);
								}
							});
							String nullExpression = "null";
							if (DBMS.POSTGRESQL.equals(session.dbms) && scriptFormat == ScriptFormat.INTRA_DATABASE) {
								nullExpression += "::" + column.type;
							}
							column.setFilter(new Filter(nullExpression, null, false, null));
						}
					}

					if (scriptType != ScriptType.DELETE) {
						rest = writeIndependentEntities(result, dependentTables, entityGraph);
						
						for (Runnable runnable: resetFilters) {
							runnable.run();
						}
						
						appendSync(result);
						updateNullableForeignKeys(result, egCopy, nullableForeignKeys);
						
					} else {
						updateNullableForeignKeys(result, egCopy, nullableForeignKeys);

						for (Runnable runnable: resetFilters) {
							runnable.run();
						}
						
						appendSync(result);
						rest = writeIndependentEntities(result, dependentTables, entityGraph);
					}
					
					egCopy.delete();
					appendSync(result);
				}
			}
			if (rest > 0) {
				break;
			}
		}
		
		if (importFilterManager != null) {
			importFilterManager.shutDown();
		}
			
		if (result != null) {
			entityGraph.dropMappingTables(result, targetDBMSConfiguration(targetSession));
			if (executionContext.getScriptFormat() != ScriptFormat.INTRA_DATABASE) {
				// write epilogs
				result.append("-- epilog");
				result.append(System.getProperty("line.separator"));
				for (ScriptEnhancer enhancer : Configuration.getScriptEnhancer()) {
					enhancer.addEpilog(result, scriptType, session, targetDBMSConfiguration(session), entityGraph, progress, executionContext);
				}
			}
			result.close();
		}

		if (transformerHandler != null) {
			String content = "\n";
			transformerHandler.characters(content.toCharArray(), 0, content.length());
			if (ScriptFormat.LIQUIBASE_XML.equals(executionContext.getScriptFormat())) {

				transformerHandler.endElement("","", "changeSet");
				transformerHandler.endElement("","", "databaseChangeLog");
				
			} else if (ScriptFormat.DBUNIT_FLAT_XML.equals(executionContext.getScriptFormat())) {
				transformerHandler.endElement("", "", "dataset");			
			}
			transformerHandler.endDocument();

		}

		if (rest > 0) {
			try {
				new File(sqlScriptFile).renameTo(new File(sqlScriptFile + ".failed"));
			} catch (Exception e) {
				_log.warn(e.getMessage());
			}
			Set<Table> cycle = CycleFinder.getCycle(dependentTables);
			String msgTitel = rest + " entities not exported due to cyclic dependencies.\n";
			String msg = msgTitel + (cycle.size() == 1? "Table" : "Tables") + " with cyclic dependencies: " + asString(cycle);
			_log.error(msg);
			try {
				// try to get a more sophisticated error message
				_log.info("starting cycle analysis...");
				ProgressListenerRegistry.getProgressListener().newStage("cycle error, analysing...", true, false);
				String sMsg = msgTitel + "Paths:\n";
				int i = 0;
				for (CycleFinder.Path path: CycleFinder.findCycle(datamodel, cycle)) {
					List<Table> pList = new ArrayList<Table>();
					path.fillPath(pList);
					sMsg += "[ ";
					boolean ft = true;
					for (Table t: pList) {
						if (!ft) {
							sMsg += " -> ";
						}
						ft = false;
						sMsg += datamodel.getDisplayName(t);
					}
					sMsg += " ]\n";
					if (++i > 30) {
						sMsg += "...\n";
						break;
					}
				}
				msg = sMsg + "\nConsider to disable the option \"sort topologically\" in the Data Export dialog";
			} catch (CancellationException e) {
				CancellationHandler.reset(null);
			} catch (Throwable t) {
				_log.warn("cycle analysis failed: " + t.getMessage());
			}
			throw new RuntimeException(msg);
		}
		_log.info("file '" + sqlScriptFile + "' written.");
	}

	private void updateNullableForeignKeys(final OutputStreamWriter result, final EntityGraph eg,
			Map<Table, Set<Column>> nullableForeignKeys) throws Exception {
		List<JobManager.Job> jobs = new ArrayList<JobManager.Job>();
		for (final Map.Entry<Table, Set<Column>> entry: nullableForeignKeys.entrySet()) {
			jobs.add(new JobManager.Job() {
				@Override
				public void run() throws Exception {
					eg.updateEntities(entry.getKey(), entry.getValue(), result, targetDBMSConfiguration(entityGraph.getTargetSession()));
				}
			});
		}
		jobManager.executeJobs(jobs);
	}

	private Map<Table, Set<Column>> findAndRemoveNullableForeignKeys(Set<Table> tables, EntityGraph theEntityGraph, boolean fkIsInSource) throws SQLException {
		Map<Table, Set<Column>> result = new HashMap<Table, Set<Column>>();
		
		for (Table table: tables) {
			for (Association association: table.associations) {
				if (!tables.contains(association.source) || !tables.contains(association.destination)) {
					continue;
				}
				if (association.isInsertDestinationBeforeSource()) {
					Map<Column, Column> mapping = association.createSourceToDestinationKeyMapping();
					if (!mapping.isEmpty()) {
						boolean nullable = true;
						Collection<Column> fk = fkIsInSource? mapping.keySet() : mapping.values();
						for (Column c: fk) {
							if (!c.isNullable) {
								nullable = false;
								break;
							}
						}
						if (nullable) {
							Table source = fkIsInSource? association.source : association.destination;
							if (!result.containsKey(source)) {
								result.put(source, new HashSet<Column>());
							}
							result.get(source).addAll(fk);
							theEntityGraph.removeDependencies(association);
						}
					}
				}
			}
		}
		
		return result;
	}

	/**
	 * Iteratively mark and write out independent entities from a given {@link EntityGraph}
	 * until no independent entity remains.
	 * 
	 * @param result writer to output file
	 * @param dependentTables tables to consider
	 * @param theEntityGraph the entity graph
	 * @return number of remaining entities
	 */
	private long writeIndependentEntities(OutputStreamWriter result, Set<Table> dependentTables, final EntityGraph theEntityGraph)
			throws SQLException, IOException, Exception {
		long rest;
		rest = theEntityGraph.getSize(dependentTables);
		for (;;) {
			for (final Table table : dependentTables) {
				theEntityGraph.markIndependentEntities(table);
			}
			List<JobManager.Job> jobs = new ArrayList<JobManager.Job>();
			for (final Table table : dependentTables) {
				jobs.add(new JobManager.Job() {
					public void run() throws Exception {
						theEntityGraph.readMarkedEntities(table, false);
					}
				});
			}
			if (result != null && !jobs.isEmpty()) {
				appendSync(result);
			}
			jobManager.executeJobs(jobs);
			for (final Table table : dependentTables) {
				theEntityGraph.deleteIndependentEntities(table);
			}
			long newRest = theEntityGraph.getSize(dependentTables);
			if (newRest == 0) {
				rest = 0;
				break;
			}
			if (rest == newRest) {
				break;
			}
			rest = newRest;
		}
		return rest;
	}
	
	/**
	 * Gets set of all tables, which are no parents (recursiv).
	 * 
	 * @param tables all tables
	 * @return subset of tables
	 */
	private Set<Table> getDescentants(Set<Table> tables) {
		Set<Table> result = new HashSet<Table>();

		for (;;) {
			Set<Table> des = new HashSet<Table>();
			for (Table table: tables) {
				if (!result.contains(table)) {
					boolean isParent = false;
					for (Association a: table.associations) {
						if (a.isInsertSourceBeforeDestination()) {
							if (tables.contains(a.destination) && !result.contains(a.destination)) {
								isParent = true;
								break;
							}
						}
					}
					if (!isParent) {
						des.add(table);
					}
				}
			}
			result.addAll(des);
			if (des.isEmpty()) {
				break;
			}
		}
		
		return result;
	}

	/**
	 * Removes all single-row cycles from dependency table.
	 * 
	 * @param progress
	 *            set of all tables from which rows are collected
	 * @param session
	 *            for executing SQL statements
	 */
	private void removeSingleRowCycles(Set<Table> progress, Session session) throws Exception {
		for (Table table : progress) {
			boolean hasReflexiveAssociation = false;
			for (Association a : table.associations) {
				if (a.destination == table) {
					hasReflexiveAssociation = true;
					break;
				}
			}
			if (hasReflexiveAssociation) {
				entityGraph.removeReflexiveDependencies(table);
			}
		}
	}

	/**
	 * Writes entities into XML-document.
	 * 
	 * @param xmlFile
	 *            the name of the xml-file to write the data to
	 * @param progress
	 *            set of tables to account for extraction
	 */
	private void writeEntitiesAsXml(String xmlFile, final Set<Table> progress, final Set<Table> subjects, Session session) throws Exception {
		_log.info("writing file '" + xmlFile + "'...");

		OutputStream outputStream = new FileOutputStream(xmlFile);
		if (xmlFile.toLowerCase().endsWith(".zip")) {
			outputStream = new ZipOutputStream(outputStream);
			String zipFileName = new File(xmlFile).getName();
			((ZipOutputStream)outputStream).putNextEntry(new ZipEntry(zipFileName.substring(0, zipFileName.length() - 4)));
		} else {
			if (xmlFile.toLowerCase().endsWith(".gz")) {
				outputStream = new GZIPOutputStream(outputStream);
			}
		}

		// then write entities of tables having cyclic-dependencies
		_log.info("create hierarchy for: " + asString(progress));
		addDependencies(progress, true);
		runstats(true);
		removeSingleRowCycles(progress, session);

		List<Table> lexSortedTables = new ArrayList<Table>(progress);
		Collections.sort(lexSortedTables, new Comparator<Table>() {
			public int compare(Table t1, Table t2) {
				boolean s1 = subjects.contains(t1);
				boolean s2 = subjects.contains(t2);
				if (s1 && !s2) {
					return -1;
				}
				if (!s1 && s2) {
					return 1;
				}
				return datamodel.getDisplayName(t1).compareTo(datamodel.getDisplayName(t2));
			}
		});

		List<Table> sortedTables = new ArrayList<Table>();
		boolean done = false;
		while (!done) {
			done = true;
			for (int step = 1; step <= 2; ++step) {
				for (Table table: lexSortedTables) {
					boolean depends = false;
					for (Association association: table.associations) {
						if (association.destination != table) {
							if (association.isInsertDestinationBeforeSource()) {
								if (lexSortedTables.contains(association.destination)) {
									if (step == 1 || (association.getAggregationSchema() == AggregationSchema.NONE && association.reversalAssociation.getAggregationSchema() == AggregationSchema.NONE)) {
										depends = true;
										break;
									}
								}
							}
						}
					}
					if (!depends) {
						sortedTables.add(table);
						done = false;
					}
				}
				if (!done) {
					break;
				}
			}
			lexSortedTables.removeAll(sortedTables);
		}
		if (!lexSortedTables.isEmpty()) {
			_log.warn("remaining tables after sorting: " + new PrintUtil(executionContext).tableSetAsString(new HashSet<Table>(lexSortedTables)));
			sortedTables.addAll(lexSortedTables);
		}
		
		Set<Table> cyclicAggregatedTables = getCyclicAggregatedTables(progress);
		_log.info("cyclic aggregated tables: " + new PrintUtil(executionContext).tableSetAsString(cyclicAggregatedTables));

		Charset charset = Charset.defaultCharset();
		if (executionContext.getUTF8()) {
			charset = Charset.forName("UTF8");
		}
		
		XmlExportTransformer reader = new XmlExportTransformer(outputStream, commentHeader.toString(), entityGraph, progress, cyclicAggregatedTables,
				executionContext.getXmlRootTag(), executionContext.getXmlDatePattern(),
				executionContext.getXmlTimeStampPattern(), entityGraph.getTargetSession(), charset, executionContext);

		for (Table table: sortedTables) {
			entityGraph.markRoots(table);
		}
		for (Table table: sortedTables) {
			_log.info("exporting table " + datamodel.getDisplayName(table));
			reader.setTable(table);
			entityGraph.readMarkedEntities(table, reader, reader.getTableMapping(table).selectionSchema, reader.getTableMapping(table).originalPKAliasPrefix, true);
		}
		reader.endDocument();

		outputStream.close();
		checkCompletenessOfXmlExport(cyclicAggregatedTables);
		_log.info("file '" + xmlFile + "' written.");
	}

	/**
	 * Gets set of cyclic aggregated tables.
	 */
	private Set<Table> getCyclicAggregatedTables(Set<Table> progress) {
		Set<Table> cyclicAggregatedTables = new HashSet<Table>(progress);

		for (;;) {
			Set<Table> nonAggregatedTables = new HashSet<Table>();
			for (Table t : cyclicAggregatedTables) {
				boolean isAggregated = false;
				for (Association association : t.associations) {
					if (association.reversalAssociation.getAggregationSchema() != AggregationSchema.NONE) {
						if (cyclicAggregatedTables.contains(association.destination)) {
							isAggregated = true;
							break;
						}
					}
				}
				if (!isAggregated) {
					nonAggregatedTables.add(t);
				}
			}
			if (nonAggregatedTables.isEmpty()) {
				break;
			}
			cyclicAggregatedTables.removeAll(nonAggregatedTables);
		}
		return cyclicAggregatedTables;
	}

	/**
	 * Checks whether some entities are not exported due to cyclic aggregation.
	 * 
	 * @param cyclicAggregatedTables
	 *            tables to check
	 */
	private void checkCompletenessOfXmlExport(Set<Table> cyclicAggregatedTables) throws SQLException {
		for (Table table: cyclicAggregatedTables) {
			entityGraph.readNonTraversedDependencies(table, new Session.ResultSetReader() {
				public void readCurrentRow(ResultSet resultSet) throws SQLException {
					String message = "Can't export all rows from table '" + datamodel.getTableByOrdinal(resultSet.getInt("TO_TYPE")) + "' due to cyclic aggregation";
					throw new RuntimeException(message);
				}

				public void close() {
				}
			});
		}
	}

	/**
	 * Writes entities of independent tables.
	 * 
	 * @param result
	 *            a writer for the extract-script
	 * @param progress
	 *            set of tables involved in export
	 * 
	 * @return set of tables from which no entities are written
	 */
	private Set<Table> writeEntitiesOfIndependentTables(final OutputStreamWriter result, final TransformerHandler transformerHandler, final ScriptType scriptType,
			Set<Table> progress, final String filepath) throws Exception {
		Set<Table> tables = new HashSet<Table>(progress);

		Set<Table> independentTables = datamodel.getIndependentTables(tables);
		while (!independentTables.isEmpty()) {
			_log.info("independent tables: " + asString(independentTables));
			List<JobManager.Job> jobs = new ArrayList<JobManager.Job>();
			for (final Table independentTable : independentTables) {
				if (ScriptFormat.DBUNIT_FLAT_XML.equals(executionContext.getScriptFormat()) || ScriptFormat.LIQUIBASE_XML.equals(executionContext.getScriptFormat())) {
					// export rows sequentially, don't mix rows of different
					// tables in a dataset!
					writeEntities(independentTable, true);
				} else {
					jobs.add(new JobManager.Job() {
						public void run() throws Exception {
							writeEntities(independentTable, false);
						}
					});
				}
			}
			if (!jobs.isEmpty()) {
				if (result != null) {
					appendSync(result);
				}
				jobManager.executeJobs(jobs);
			}
			tables.removeAll(independentTables);
			independentTables = datamodel.getIndependentTables(tables);
		}

		return tables;
	}
	
	private void appendSync(OutputStreamWriter result) throws IOException {
		if (executionContext.getScriptFormat() != ScriptFormat.INTRA_DATABASE) {
			result.append("-- sync" + System.getProperty("line.separator"));
		}
	}
	
	/**
	 * Prevents multiple shutdowns.
	 */
	private boolean isDown = false;

	/**
	 * Shuts the archiver down.
	 */
	private void shutDown() throws SQLException {
		if (!isDown) {
			jobManager.shutdown();
			entityGraph.shutDown();
			isDown = true;
		}
	}

	/**
	 * Stringifies progress-set.
	 * 
	 * @param progress
	 *            the progress-set
	 * @return the progress-set as string
	 */
	static String asString(Set<Table> progress) {
		String str = "";
		for (Table table : progress) {
			if (!"".equals(str)) {
				str += ", ";
			}
			str += table.getName();
		}
		return str;
	}

	/**
	 * Total row-count at last runstats run.
	 */
	private long lastRunstats = 0;

	/**
	 * Runs script for updating the DB-statistics.
	 */
	private synchronized void runstats(boolean force) throws Exception {
		if (entityGraph != null) {
			Session session = entityGraph.getSession();
			if (force || lastRunstats == 0 || (lastRunstats * 2 <= entityGraph.getTotalRowcount() && entityGraph.getTotalRowcount() > 1000)) {
				lastRunstats = entityGraph.getTotalRowcount();
	
				StatisticRenovator statisticRenovator = session.dbms.getStatisticRenovator();
				if (statisticRenovator != null) {
					_log.info("gather statistics after " + lastRunstats + " inserted rows...");
					try {
						statisticRenovator.renew(session, executionContext);
					} catch (Throwable t) {
						_log.warn("unable to update table statistics: " + t.getMessage());
					}
				}
			}
		}
	}

	/**
	 * Exports entities.
	 */
	public void export(String extractionModelFileName, String scriptFile, String deleteScriptFileName, DataSource dataSource, DBMS dbms, boolean explain, ScriptFormat scriptFormat) throws Exception {
		if (scriptFile != null) {
			_log.info("exporting '" + extractionModelFileName + "' to '" + scriptFile + "'");
		}
		
		Session session = new Session(dataSource, dbms, executionContext.getScope(), false);
		DDLCreator ddlCreator = new DDLCreator(executionContext);
		if (executionContext.getScope() == TemporaryTableScope.SESSION_LOCAL
		 || executionContext.getScope() == TemporaryTableScope.TRANSACTION_LOCAL) {
			ddlCreator.createDDL(session, executionContext.getScope(), executionContext.getWorkingTableSchema());
		} else if (executionContext.getScope() == TemporaryTableScope.GLOBAL) {
			if (!ddlCreator.isUptodate(session, !executionContext.getNoRowid(), executionContext.getWorkingTableSchema())) {
				throw new IllegalStateException("Jailer working tables do not exist or are not up to date. Use 'jailer create-ddl' to create them.");
			}
		}

		ExtractionModel extractionModel = new ExtractionModel(extractionModelFileName, executionContext.getSourceSchemaMapping(), executionContext.getParameters(), executionContext);

		_log.info(session.dbms.getSqlDialect());
		
		EntityGraph entityGraph;
		if (scriptFormat == ScriptFormat.INTRA_DATABASE) {
			RowIdSupport rowIdSupport = new RowIdSupport(extractionModel.dataModel, session.dbms, executionContext);
			entityGraph = IntraDatabaseEntityGraph.create(extractionModel.dataModel, EntityGraph.createUniqueGraphID(), session, rowIdSupport.getUniversalPrimaryKey(session), executionContext);
		} else if (executionContext.getScope() == TemporaryTableScope.LOCAL_DATABASE) {
			entityGraph = LocalEntityGraph.create(extractionModel.dataModel, EntityGraph.createUniqueGraphID(), session, executionContext);
		} else {
			RowIdSupport rowIdSupport = new RowIdSupport(extractionModel.dataModel, session.dbms, executionContext);
			entityGraph = RemoteEntityGraph.create(extractionModel.dataModel, EntityGraph.createUniqueGraphID(), session, rowIdSupport.getUniversalPrimaryKey(session), executionContext);
		}

		entityGraph.setExplain(explain);

		Charset charset = Charset.defaultCharset();
		if (executionContext.getUTF8()) {
			charset = Charset.forName("UTF8");
			appendCommentHeader("encoding " + charset.name());
			appendCommentHeader("");
		}
		appendCommentHeader("generated by Jailer " + Jailer.VERSION + ", " + new Date() + " from " + getUsername());
		Set<Table> totalProgress = new HashSet<Table>();
		Set<Table> subjects = new HashSet<Table>();

		if (executionContext.getWhere() != null && executionContext.getWhere().trim().length() > 0) {
			extractionModel.condition = executionContext.getWhere();
		}

		appendCommentHeader("");
		String condition = (extractionModel.condition != null && !"1=1".equals(extractionModel.condition)) ? extractionModel.subject.getName() + " where " + extractionModel.condition
				: "all rows from " + extractionModel.subject.getName();
		appendCommentHeader("Extraction Model:  " + condition + " (" + extractionModelFileName + ")");
		for (AdditionalSubject as: extractionModel.additionalSubjects) {
			condition = (as.getCondition() != null && as.getCondition().trim().length() > 0) ? as.getSubject().getName() + " where " + as.getCondition()
					: "all rows from " + as.getSubject().getName();
			appendCommentHeader("                   Union " + condition);
		}
		if (executionContext.getNoSorting()) {
			appendCommentHeader("                   unsorted");
		}
		appendCommentHeader("Source DBMS:       " + session.dbms.getDisplayName());
		appendCommentHeader("Target DBMS:       " + targetDBMSConfiguration(session).getDisplayName());
		if (session.dbUrl != null) {
			appendCommentHeader("Database URL:      " + session.dbUrl);
		}
		if (!"".equals(session.getSchema())) {
			appendCommentHeader("Database User:     " + session.getSchema());
		}
		appendCommentHeader("");

		if (session.dbms.getRowidName() == null) {
    		Set<Table> toCheck = new HashSet<Table>();
			if (extractionModel.additionalSubjects != null) {
				for (AdditionalSubject as: extractionModel.additionalSubjects) {
					toCheck.add(as.getSubject());
				}
			}
			toCheck.add(extractionModel.subject);
			extractionModel.dataModel.checkForPrimaryKey(toCheck, deleteScriptFileName != null);
		}

		extractionModel.condition = ParameterHandler.assignParameterValues(extractionModel.condition, executionContext.getParameters());
		
		if (!executionContext.getParameters().isEmpty()) {
			String suffix = "Parameters:        ";
			for (Map.Entry<String, String> e: executionContext.getParameters().entrySet()) {
				appendCommentHeader(suffix + e.getKey() + " = " + e.getValue());
				suffix = "                   ";
			}
			appendCommentHeader("");
		}
		
		EntityGraph graph = entityGraph;
		setEntityGraph(graph);
		setDataModel(extractionModel.dataModel);
		EntityGraph exportedEntities = null;
		
		try {
			runstats(false);
			ProgressListenerRegistry.getProgressListener().newStage("collecting rows", false, false);
			Set<Table> completedTables = new HashSet<Table>();
			Set<Table> progress = exportSubjects(extractionModel, completedTables);
			entityGraph.setBirthdayOfSubject(entityGraph.getAge());
			progress.addAll(export(extractionModel.subject, extractionModel.condition, progress, true, completedTables));
			totalProgress.addAll(progress);
			subjects.add(extractionModel.subject);
	
			if (explain) {
				ProgressListenerRegistry.getProgressListener().newStage("generating explain-log", false, false);
				ExplainTool.explain(entityGraph, session, executionContext);
			}
	
			totalProgress = datamodel.normalize(totalProgress);
			subjects = datamodel.normalize(subjects);
	
			if (deleteScriptFileName != null) {
				exportedEntities = entityGraph.copy(EntityGraph.createUniqueGraphID(), session);
			}

			if (scriptFile != null) {
				ProgressListenerRegistry.getProgressListener().prepareExport();
				
				setEntityGraph(entityGraph);
				if (ScriptFormat.XML.equals(scriptFormat)) {
					writeEntitiesAsXml(scriptFile, totalProgress, subjects, session);
				} else {
					writeEntities(scriptFile, ScriptType.INSERT, totalProgress, session, "exporting rows");
				}
			}
			entityGraph.delete();
			
			if (deleteScriptFileName != null) {
				ProgressListenerRegistry.getProgressListener().newStage("delete", false, false);
				ProgressListenerRegistry.getProgressListener().newStage("delete-reduction", false, false);
				setEntityGraph(exportedEntities);
				deleteEntities(subjects, totalProgress, session);
				datamodel.transpose();
				writeEntities(deleteScriptFileName, ScriptType.DELETE, totalProgress, session, "writing delete-script");
				exportedEntities.delete();
				exportedEntities.shutDown();
				setEntityGraph(entityGraph);
			}
			entityGraph.close();
		} catch (CancellationException e) {
			try {
				_log.info("cleaning up after cancellation...");
				CancellationHandler.reset(null);
				entityGraph.getSession().rollbackAll();
				entityGraph.delete();
				if (exportedEntities != null) {
					if (entityGraph.getSession().scope == TemporaryTableScope.GLOBAL) {
						exportedEntities.delete();
					} else {
						_log.info("skipping clean up of temporary tables");
					}
				}
				_log.info("cleaned up");
				entityGraph.close();
				shutDown();
			} catch (Throwable t) {
				_log.warn(t.getMessage());
			}
			throw e;
		} catch (Exception e) {
			try {
				_log.info("cleaning up...");
				entityGraph.delete();
				if (exportedEntities != null) {
					if (entityGraph.getSession().scope == TemporaryTableScope.GLOBAL) {
						exportedEntities.delete();
					} else {
						_log.info("skipping clean up of temporary tables");
					}
				}
				entityGraph.close();
				shutDown();
			} catch (Throwable t) {
				_log.warn(t.getMessage());
			}
			throw e;
		}
		shutDown();
	}

	/**
	 * Gets user-name.
	 * 
	 * @return the user-name
	 */
	private static String getUsername() {
		String host = "";
		try {
			host = "@" + InetAddress.getLocalHost().getHostName();
		} catch (UnknownHostException e) {
		}
		return System.getProperty("user.name") + host;
	}

	/**
	 * Calculates D=(E-T)-C*(U-(E-T)) where E is the entity-graph of this
	 * export-tool, see
	 * http://intra.*.de/dokuwiki/doku.php?id=projekte:sql-export-tool-phase2.
	 * 
	 * @param subjects
	 *            set of tables containing subjects of extraction-tasks
	 * @param allTables
	 *            set of tables from which there are entities in E
	 * @param statementExecutor
	 *            for executing SQL-statements
	 */
	private void deleteEntities(Set<Table> subjects, Set<Table> allTables, Session session) throws Exception {
		Set<Table> tabuTables = new HashSet<Table>();
		for (Table table: allTables) {
			if (table.isExcludedFromDeletion()) {
				tabuTables.add(table);
			}
		}
		appendCommentHeader("");
		appendCommentHeader("Tabu-tables: " + new PrintUtil(executionContext).tableSetAsString(tabuTables, "--                 "));
		_log.info("Tabu-tables: " + new PrintUtil(executionContext).tableSetAsString(tabuTables, null));
		entityGraph.setDeleteMode(true);
		removeFilters(datamodel);

		final Map<Table, Long> removedEntities = new HashMap<Table, Long>();

		// do not check tables in first step having exactly one 1:1 or 1:n
		// association
		// from another table
		Set<Table> dontCheckInitially = new HashSet<Table>();
		for (Table table: allTables) {
			int n = 0;
			boolean check = false;
			for (Association a: table.associations) {
				if (!a.reversalAssociation.isIgnored()) {
					if (tabuTables.contains(a.destination)) {
						check = true;
					} else if (a.reversalAssociation.getCardinality() == Cardinality.ONE_TO_MANY
							|| a.reversalAssociation.getCardinality() == Cardinality.ONE_TO_ONE) {
						++n;
					} else {
						check = true;
					}
				}
			}
			if ((!check) && n == 1) {
				dontCheckInitially.add(table);
			}
		}

		int today = 1;
		ProgressListenerRegistry.getProgressListener().prepareExport();

		// remove tabu entities
		for (Table tabuTable: tabuTables) {
			ProgressListenerRegistry.getProgressListener().collectionJobEnqueued(today, tabuTable);
		}
				
		// remove tabu entities
		for (Table tabuTable : tabuTables) {
			ProgressListenerRegistry.getProgressListener().collectionJobStarted(today, tabuTable);
			long rc = entityGraph.deleteEntities(tabuTable);
			ProgressListenerRegistry.getProgressListener().collected(today, tabuTable, rc);
			_log.info("excluded " + rc + " entities from " + datamodel.getDisplayName(tabuTable) + " (tabu)");
			allTables.remove(tabuTable);
		}

		// set of tables which are known to have no entities in entityGraph
		Set<Table> emptyTables = new HashSet<Table>();

		Set<Table> tablesToCheck = new HashSet<Table>(allTables);
		_log.info("don't check initially: " + new PrintUtil(executionContext).tableSetAsString(dontCheckInitially, null));
		tablesToCheck.removeAll(dontCheckInitially);

		boolean firstStep = true;
		final Set<Table> roots = new HashSet<Table>();
		final Map<Association, Long> rootAssocs = new HashMap<Association, Long>();
		// remove associated entities
		while (!tablesToCheck.isEmpty()) {
			++today;
			_log.info("tables to check: " + new PrintUtil(executionContext).tableSetAsString(tablesToCheck, null));
			List<JobManager.Job> jobs = new ArrayList<JobManager.Job>();
			final Set<Table> tablesToCheckNextTime = new HashSet<Table>();
			for (final Table table : tablesToCheck) {
				for (final Association a : table.associations) {
					if (emptyTables.contains(table)) {
						continue;
					}
					if (!a.reversalAssociation.isIgnored()) {
						if (entityGraph.countEntities(table) == 0) {
							emptyTables.add(table);
							continue;
						}
						final boolean isFirstStep = firstStep;
						final int finalToday = today;
						if (!isFirstStep) {
							ProgressListenerRegistry.getProgressListener().collectionJobEnqueued(today, a.reversalAssociation);
						}
						jobs.add(new JobManager.Job() {
							public void run() throws Exception {
								if (!isFirstStep) {
									ProgressListenerRegistry.getProgressListener().collectionJobStarted(finalToday, a.reversalAssociation);
								}
								long rc = entityGraph.removeAssociatedDestinations(a.reversalAssociation, !isFirstStep);
								if (!isFirstStep) {
									ProgressListenerRegistry.getProgressListener().collected(finalToday, a.reversalAssociation, rc);
								} else if (rc > 0) {
									roots.add(a.destination);
									rootAssocs.put(a, rc);
								}
								if (rc > 0) {
									synchronized (removedEntities) {
										Long oldRc = removedEntities.get(table);
										removedEntities.put(table, rc + (oldRc == null ? 0 : oldRc));
										_log.info("excluded " + rc + " entities from " + datamodel.getDisplayName(table) + " referenced by " + a);
										for (Association a2 : table.associations) {
											tablesToCheckNextTime.add(a2.destination);
										}
									}
								}
							}
						});
					}
				}
			}
			jobManager.executeJobs(jobs);
			if (firstStep) {
				for (Table table: roots) {
					if (!tabuTables.contains(table)) {
						ProgressListenerRegistry.getProgressListener().collectionJobEnqueued(today - 1, table);
					}
				}
				for (Entry<Association, Long> e: rootAssocs.entrySet()) {
					ProgressListenerRegistry.getProgressListener().collectionJobEnqueued(today, e.getKey().reversalAssociation);
					ProgressListenerRegistry.getProgressListener().collectionJobStarted(today, e.getKey().reversalAssociation);
					ProgressListenerRegistry.getProgressListener().collected(today, e.getKey().reversalAssociation, e.getValue());
									
				}
			}
			tablesToCheck = tablesToCheckNextTime;
			tablesToCheck.retainAll(allTables);
			firstStep = false;
		}

		_log.info("entities to delete:");
		appendCommentHeader("");
		boolean firstLine = true;
		for (String line : entityGraph.getStatistics(datamodel, removedEntities.keySet())) {
			if (!firstLine) {
				String tableName = line.split(" ")[0];
				Long re = removedEntities.get(datamodel.getTable(tableName));
				
				if (re == null) {
					for (Entry<Table, Long> e: removedEntities.entrySet()) {
						if (Quoting.staticUnquote(e.getKey().getName()).equals(tableName)) {
							re = e.getValue();
							break;
						}
					}
				}
				
				if (re != null && re != 0L) {
					line += " (-" + re + ")";
				}
			}
			_log.info(line);
			String l = (firstLine ? "Deleted Entities: " : "     ") + line;
			appendCommentHeader(l);
			if (firstLine) {
				appendCommentHeader("");
			}
			firstLine = false;
		}
		appendCommentHeader("");
	}

	/**
	 * Removes filters on every column.
	 */
	private void removeFilters(DataModel theDatamodel) {
		for (Table table: theDatamodel.getTables()) {
			for (Column column: table.getColumns()) {
				column.setFilter(null);
			}
		}
	}

}
