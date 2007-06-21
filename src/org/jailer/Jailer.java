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

package org.jailer;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.zip.GZIPOutputStream;

import org.apache.log4j.Logger;
import org.apache.log4j.PropertyConfigurator;
import org.jailer.database.DeletionReader;
import org.jailer.database.ExportReader;
import org.jailer.database.StatementExecutor;
import org.jailer.database.StatisticRenovator;
import org.jailer.database.StatementExecutor.ResultSetReader;
import org.jailer.datamodel.Association;
import org.jailer.datamodel.Cardinality;
import org.jailer.datamodel.DataModel;
import org.jailer.datamodel.Table;
import org.jailer.domainmodel.DomainModel;
import org.jailer.enhancer.ScriptEnhancer;
import org.jailer.entitygraph.EntityGraph;
import org.jailer.extractionmodel.ExtractionModel;
import org.jailer.modelbuilder.ModelBuilder;
import org.jailer.render.DataModelRenderer;
import org.jailer.restrictionmodel.RestrictionModel;
import org.jailer.util.CsvFile;
import org.jailer.util.JobManager;
import org.jailer.util.PrintUtil;
import org.jailer.util.SqlUtil;
import org.springframework.context.support.AbstractXmlApplicationContext;
import org.springframework.context.support.FileSystemXmlApplicationContext;


/**
 * Utility for extracting small consistent row-sets from
 * relational databases.
 * 
 * @author Wisser
 */
public class Jailer {
    
    /**
     * The relational data model.
     */
    private DataModel datamodel;

    /**
     * The entity-graph to be used for finding the transitive closure.
     */
    private EntityGraph entityGraph;
    
    /**
     * The job-manager to be used for concurrent execution of jobs.
     */
    private final JobManager jobManager;
    
    /**
     * The logger.
     */
    private static final Logger _log;

    /**
     * The configuration.
     */
    private AbstractXmlApplicationContext applicationContext = new FileSystemXmlApplicationContext("config/config.xml");

    /**
     * Comment header of the export-script.
     */
    private StringBuffer commentHeader = new StringBuffer();
    
    /**
     * The scipt-enhancer.
     */
    private final List<ScriptEnhancer> scriptEnhancer;
    
    /**
     * Constructor.
     * 
     * @param threads number of threads
     */
    public Jailer(int threads) throws Exception {
        jobManager = new JobManager(threads);
        scriptEnhancer = (List<ScriptEnhancer>) applicationContext.getBean("script-enhancer");
    }

    /**
     * Sets the entity-graph to be used for finding the transitive closure.
     * 
     * @param entityGraph the entity-graph to be used for finding the transitive closure
     */
    public void setEntityGraph(EntityGraph entityGraph) {
        this.entityGraph = entityGraph;
    }

    /**
     * Sets the restricted data-model to be used for extraction.
     * 
     * @param dataModel the restricted data-model to be used for extraction
     */
    public void setDataModel(DataModel dataModel) {
        this.datamodel = dataModel;
    }
    
    /**
     * Exports rows from table.
     * 
     * @param table the table
     * @param condition the condition (in SQL) the exported rows must fulfill
     * @param progressOfYesterday set of tables to account for resolvation
     * 
     * @return set of tables from which entities are added
     */
    public Set<Table> export(Table table, String condition, Collection<Table> progressOfYesterday) throws Exception {
        return export(table, condition, progressOfYesterday, 0);
    }
    
    /**
     * Appends a line to the comment-header of the export script.
     * 
     * @param comment the comment line (without '--'-prefix)
     */
    private void appendCommentHeader(String comment) {
        commentHeader.append("-- " + comment + "\n");
    }
    
    /**
     * Exports rows from table.
     * 
     * @param table the table
     * @param condition the condition (in SQL) the exported rows must fulfill
     * @param limit a limit of the number of rows to be exported
     * @param progressOfYesterday set of tables to account for resolvation
     * 
     * @return set of tables from which entities are added
     */
    public Set<Table> export(Table table, String condition, Collection<Table> progressOfYesterday, long limit) throws Exception {
        _log.info("exporting " + table.getName() + " Where " + condition);
        _log.info("using data-model:\n" + datamodel);

        if (table == null) {
            throw new RuntimeException("unknown table: " + table.getName());
        }
        int today = entityGraph.getAge();
        entityGraph.setAge(today + 1);
        Map<Table, Collection<Association>> progress = new HashMap<Table, Collection<Association>>();
        if (entityGraph.addEntities(table, condition, today, limit) > 0) {
            progress.put(table, new ArrayList<Association>());
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
            progress = resolveAssociations(today, progress);
        }
        
        _log.info("exported " + table.getName() + " Where " + condition);
        _log.info("total progress: " + asString(totalProgress));
        _log.info("export statistic:");
        boolean firstLine = true;
        for (String line: entityGraph.getStatistics()) {
            _log.info(line);
            appendCommentHeader((firstLine ? "exported entities: " : "    ") + line);
            firstLine = false;
        }
        
        return totalProgress;
    }
    
    /**
     * Exports all entities from inital-data tables.
     * 
     * @param subject the subject of the extraction-model
     */
    private Set<Table> exportInitialData(Table subject) throws Exception {
        Set<Table> idTables = readInitialDataTables();
        Set<Table> tables = new HashSet<Table>();
        for (Table table: idTables) {
            if (subject.closure(true).contains(table)) {
                tables.add(table);
                _log.info("exporting all " + table.getName());
    
                int today = entityGraph.getAge();
                entityGraph.addEntities(table, "1=1", today, 0);
            } else {
                _log.info(table.getName() + " not in closure(" + subject.getName() + ")");
            }
        }
        return tables;
    }

    /**
     * Reads the initial-data-tables list.
     * An initial-data-table is a table which will be exported completely if it is in closure from subject.
     * 
     * @return the initial-data-tables list
     */
    private Set<Table> readInitialDataTables() throws Exception {
        Set<Table> idTables = SqlUtil.readTableList(new CsvFile(new File("datamodel/initial_data_tables.csv")), datamodel);
        return idTables;
    }

    /**
     * Resolves all associations defined in data-model.
     * 
     * @param today birthday of newly created entities
     * @param progressOfYesterday set of tables to account for resolvation
     * 
     * @return map from tables from which entities are added to all associations which lead to the entities
     */
    private Map<Table, Collection<Association>> resolveAssociations(final int today, Map<Table, Collection<Association>> progressOfYesterday) throws Exception {
        final Map<Table, Collection<Association>> progress = new HashMap<Table, Collection<Association>>();

        Set<Table> initialDataTables = readInitialDataTables();
        
        // resolve associations with same dest-type sequentially
        Map<Table, List<JobManager.Job>> jobsPerDestination = new HashMap<Table, List<JobManager.Job>>();
        
        for (final Table table: progressOfYesterday.keySet()) {
            for (final Association association: table.associations) {
                if (initialDataTables.contains(association.destination)) {
                    // optimization: initial data tables
                    _log.info("skip association with initial table " + table.getName() + " -> " + association.destination.getName());
                    continue;
                }

                Collection<Association> as = progressOfYesterday.get(table);
                if (as != null && as.size() == 1 && as.iterator().next() == association.reversalAssociation) {
                    if (association.getCardinality() == Cardinality.MANY_TO_ONE || association.getCardinality() == Cardinality.ONE_TO_ONE) {
                        _log.info("skip reversal association " + table.getName() + " -> " + association.destination.getName());
                        continue;
                    }
                }

                JobManager.Job job = new JobManager.Job() {
                    public void run() throws Exception {
                        runstats(entityGraph.statementExecutor);
                        long rc = entityGraph.resolveAssociation(table, association, today);
                        if (rc >= 0) {
                            _log.info(rc + " entities found resolving " + table.getName() + " -> " + association);
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
        for (final Map.Entry<Table, List<JobManager.Job>> entry: jobsPerDestination.entrySet()) {
            jobs.add(new JobManager.Job() {
                public void run() throws Exception {
                    for (JobManager.Job job: entry.getValue()) {
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
     * @param progress set of tables to take into account
     */
    public void addDependencies(Set<Table> progress) throws Exception {
        List<JobManager.Job> jobs = new ArrayList<JobManager.Job>();
        _log.info("cyclic dependencies for: " + asString(progress));
        for (final Table table: progress) {
            for (final Association association: table.associations) {
                if (progress.contains(association.destination)) {
                    final String jc = association.getJoinCondition();
                    if (jc != null && association.isInsertDestinationBeforeSource()) {
                        jobs.add(new JobManager.Job() {
                            public void run() throws Exception {
                                _log.info("find dependencies " + table.getName() + " -> " + association.destination.getName() + " on " + jc);
                                String fromAlias, toAlias;
                                fromAlias = association.reversed? "B" : "A";
                                toAlias = association.reversed? "A" : "B";
                                entityGraph.addDependencies(table, fromAlias, association.destination, toAlias, jc);
                            }
                        });
                    }
                    if (jc != null && association.isInsertSourceBeforeDestination()) {
                        jobs.add(new JobManager.Job() {
                            public void run() throws Exception {
                                _log.info("find dependencies " + association.destination.getName() + " -> " + table.getName() + " on " + jc);
                                String fromAlias, toAlias;
                                fromAlias = association.reversed? "B" : "A";
                                toAlias = association.reversed? "A" : "B";
                                entityGraph.addDependencies(association.destination, toAlias, table, fromAlias, jc);
                            }
                        });
                    }
                }
            }
        }
        jobManager.executeJobs(jobs);
    }

    /**
     * Writes entities into extract-SQL-script.
     * 
     * @param sqlScriptFile the name of the sql-script to write the data to
     * @param table write entities from this table only
     * @param result a writer for the extract-script
     */
    private void writeEntities(OutputStreamWriter result, ScriptType scriptType, Table table) throws Exception {
        ResultSetReader reader = scriptType==ScriptType.INSERT? new ExportReader(table, result, CommandLineParser.getInstance().upsertOnly, CommandLineParser.getInstance().numberOfEntities) : new DeletionReader(table, result, CommandLineParser.getInstance().numberOfEntities);
        entityGraph.readEntities(table, reader);
        entityGraph.deleteEntities(table);
    }
    
    /**
     * Writes entities into extract-SQL-script.
     * 
     * @param sqlScriptFile the name of the sql-script to write the data to
     * @param progress set of tables to account for extraction
     */
    public void writeEntities(String sqlScriptFile, final ScriptType scriptType, final Set<Table> progress, StatementExecutor statementExecutor) throws Exception {
        OutputStream outputStream = new FileOutputStream(sqlScriptFile);
        if (sqlScriptFile.toLowerCase().endsWith(".zip") || sqlScriptFile.toLowerCase().endsWith(".gz")) {
            outputStream = new GZIPOutputStream(outputStream);
        }
        final OutputStreamWriter result = new OutputStreamWriter(outputStream);
        result.append(commentHeader);
        result.append(System.getProperty("line.separator"));
        for (ScriptEnhancer enhancer: scriptEnhancer) {
            enhancer.addComments(result, scriptType, statementExecutor, entityGraph, progress);
        }
        result.append(System.getProperty("line.separator"));
        result.append(System.getProperty("line.separator"));
        for (ScriptEnhancer enhancer: scriptEnhancer) {
            enhancer.addProlog(result, scriptType, statementExecutor, entityGraph, progress);
        }
        
        // first write entities of independent tables 
        final Set<Table> dependentTables = writeEntitiesOfIndependentTables(result, scriptType, progress);
        
        // then write entities of tables having cyclic-dependencies
        addDependencies(dependentTables);
        
        long rest = entityGraph.getSize();
        for (;;) {
            entityGraph.markIndependentEntities();
            List<JobManager.Job> jobs = new ArrayList<JobManager.Job>();
            for (final Table table: dependentTables) {
                jobs.add(new JobManager.Job() {
                    public void run() throws Exception {
                        ResultSetReader reader = scriptType==ScriptType.INSERT? new ExportReader(table, result, CommandLineParser.getInstance().upsertOnly, CommandLineParser.getInstance().numberOfEntities) : new DeletionReader(table, result, CommandLineParser.getInstance().numberOfEntities);
                        entityGraph.readIndependentEntities(table, reader);
                    }
                });
            }
            jobManager.executeJobs(jobs);
            entityGraph.deleteIndependentEntities();
            long newRest = entityGraph.getSize();
            if (rest == newRest) {
                break;
            }
            rest = newRest;
        }
        
        // write epilogs
        for (ScriptEnhancer enhancer: scriptEnhancer) {
            enhancer.addEpilog(result, scriptType, statementExecutor, entityGraph, progress);
        }
        
        result.close();
        if (rest > 0) {
            throw new RuntimeException(rest + " entities not exported due to cyclic dependencies");
        }
    }

    /**
     * Writes entities of independent tables.
     * 
     * @param result a writer for the extract-script
     * @param progress set of tables involved in export
     * 
     * @return set of tables from which no entities are written
     */ 
    Set<Table> writeEntitiesOfIndependentTables(final OutputStreamWriter result, final ScriptType scriptType, Set<Table> progress) throws Exception {
        Set<Table> tables = new HashSet<Table>(progress);
        
        Set<Table> independentTables = datamodel.getIndependentTables(tables);
        while (!independentTables.isEmpty()) {
            _log.info("independent tables: " + asString(independentTables));
            List<JobManager.Job> jobs = new ArrayList<JobManager.Job>();
            for (final Table independentTable: independentTables) {
                jobs.add(new JobManager.Job() {
                    public void run() throws Exception {
                        writeEntities(result, scriptType, independentTable);
                    }
                });
            }
            jobManager.executeJobs(jobs);
            tables.removeAll(independentTables);
            independentTables = datamodel.getIndependentTables(tables);
        }
        
        return tables;
    }
    
    /**
     * Prevent multiple shutdowns.
     */
    private boolean isDown = false;
    
    /**
     * Shuts the archiver down.
     */
    public void shutDown() throws SQLException {
        if (!isDown) {
            jobManager.shutdown();
            entityGraph.shutDown();
            isDown = true;
        }
    }
    
    /**
     * Stringifies progress-set.
     * 
     * @param progress the progress-set
     * @return the progress-set as string
     */
    private static String asString(Set<Table> progress) {
        String str = "";
        for (Table table: progress) {
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
    private synchronized void runstats(StatementExecutor statementExecutor) throws Exception {
        if (lastRunstats == 0 || (lastRunstats * 2 <= entityGraph.getTotalRowcount() && entityGraph.getTotalRowcount() > 1000)) {
            lastRunstats = entityGraph.getTotalRowcount();
            if (applicationContext.containsBean("statistic-renovator")) {
                StatisticRenovator statisticRenovator = (StatisticRenovator) applicationContext.getBean("statistic-renovator");  
                _log.info("gather statistics after " + lastRunstats + " inserted rows...");
                statisticRenovator.renew(statementExecutor);
            }
        }
    }

    /**
     * Main-method.
     * 
     * @param args arguments
     */
    public static void main(String[] args) throws Exception {
        
        CommandLineParser.parse(args);
        CommandLineParser clp = CommandLineParser.getInstance();
        
        String command = clp.arguments.get(0);
        if ("check-domainmodel".equalsIgnoreCase(command)) {
            DataModel dataModel = new DataModel();
            for (String rm: clp.arguments.subList(1, clp.arguments.size())) {
                if (dataModel.getRestrictionModel() == null) {
                    dataModel.setRestrictionModel(new RestrictionModel(dataModel));
                }
                dataModel.getRestrictionModel().addRestrictionDefinition(rm);
            }
           new DomainModel(dataModel).check();
        } else if ("render-datamodel".equalsIgnoreCase(command)) {
            if (clp.arguments.size() <= 4) {
                CommandLineParser.printUsage();
            } else {
                new Jailer(1).renderDataModel(clp.arguments, clp.withClosures);
            }
        } else if ("print-datamodel".equalsIgnoreCase(command)) {
            printDataModel(clp.arguments, clp.withClosures);
        } else if ("export".equalsIgnoreCase(command)) {
            if (clp.arguments.size() != 6) {
                CommandLineParser.printUsage();
            } else {
                if (clp.maxNumberOfEntities > 0) {
                    EntityGraph.maxTotalRowcount = clp.maxNumberOfEntities;
                    _log.info("max-rowcount=" + EntityGraph.maxTotalRowcount);
                }
                if (clp.exportScriptFileName == null) {
                    System.out.println("missing '-e' option");
                    CommandLineParser.printUsage();
                } else {
                    export(clp.arguments.get(1), clp.exportScriptFileName, clp.deleteScriptFileName, clp.arguments.get(2), clp.arguments.get(3), clp.arguments.get(4), clp.arguments.get(5), clp.explain, clp.numberOfThreads);
                }
            }
        } else if ("find-association".equalsIgnoreCase(command)) {
            if (clp.arguments.size() < 3) {
                CommandLineParser.printUsage();
            } else {
                findAssociation(clp.arguments.get(1), clp.arguments.get(2), clp.arguments.subList(3, clp.arguments.size()), clp.undirected);
            }
        } else if ("create-ddl".equalsIgnoreCase(command)) {
            DDLCreator.createDDL();
        } else if ("build-model".equalsIgnoreCase(command)) {
            if (clp.arguments.size() != 5) {
                CommandLineParser.printUsage();
            } else {
                ModelBuilder.build(clp.arguments.get(1), clp.arguments.get(2), clp.arguments.get(3), clp.arguments.get(4));
            }
        } else {
            CommandLineParser.printUsage();
        }
    }

    /**
     * Render the data model.
     */
    private void renderDataModel(List<String> arguments, boolean withClosures) throws Exception {
    	DataModel dataModel = new DataModel();
        StatementExecutor statementExecutor = new StatementExecutor(arguments.get(1), arguments.get(2), arguments.get(3), arguments.get(4));
        for (String rm: arguments.subList(1 + 4, arguments.size())) {
            if (dataModel.getRestrictionModel() == null) {
                dataModel.setRestrictionModel(new RestrictionModel(dataModel));
            }
            dataModel.getRestrictionModel().addRestrictionDefinition(rm);
        }
        DataModelRenderer renderer = (DataModelRenderer) applicationContext.getBean("renderer");
        if (renderer == null) {
            throw new RuntimeException("no renderer found in config/config.xml");
        }
        renderer.render(dataModel, statementExecutor);
    }

	/**
     * Exports entities.
     */
    private static void export(String extractionModelFileName, String scriptFile, String deleteScriptFileName, String driverClassName, String dbUrl, String dbUser, String dbPassword, boolean explain, int threads) throws Exception {
        StatementExecutor statementExecutor = new StatementExecutor(driverClassName, dbUrl, dbUser, dbPassword);
        ExtractionModel extractionModel = new ExtractionModel(extractionModelFileName);
        EntityGraph entityGraph = EntityGraph.create(EntityGraph.createUniqueGraphID(), statementExecutor, extractionModel.getTasks().get(0).dataModel.getUniversalPrimaryKey());
        entityGraph.setExplain(explain);
        final Jailer jailer = new Jailer(threads);
        
        jailer.appendCommentHeader("generated by Jailer at " + new Date() + " from " + getUsername());
        Set<Table> totalProgress = new HashSet<Table>();
        Set<Table> subjects = new HashSet<Table>();
        boolean firstTask = true;
        for (ExtractionModel.ExtractionTask task: extractionModel.getTasks()) {
            jailer.appendCommentHeader("");
            jailer.appendCommentHeader("extraction model:  " + task.subject.getName() + " where " + task.condition + " (" + extractionModelFileName + ")");
            jailer.appendCommentHeader("database URL:      " + dbUrl);
            jailer.appendCommentHeader("database user:     " + dbUser);
        
            EntityGraph graph = firstTask? entityGraph : EntityGraph.create(EntityGraph.createUniqueGraphID(), statementExecutor, task.dataModel.getUniversalPrimaryKey());
            jailer.setEntityGraph(graph);
            jailer.setDataModel(task.dataModel);
            jailer.runstats(statementExecutor);
            Set<Table> progress = jailer.exportInitialData(task.subject);
            progress.addAll(jailer.export(task.subject, task.condition, progress, task.limit));
            totalProgress.addAll(progress);
            subjects.add(task.subject);
            if (!firstTask) {
                entityGraph.uniteWith(graph);
            }
            firstTask = false;
        }
        
        if (explain) {
            ExplainTool.explain(entityGraph, jailer.readInitialDataTables(), statementExecutor, jailer.datamodel);
        }
        
        totalProgress = jailer.datamodel.normalize(totalProgress);
        subjects = jailer.datamodel.normalize(subjects);
        
        EntityGraph exportedEntities = null;
        if (deleteScriptFileName != null) {
            exportedEntities = EntityGraph.copy(entityGraph, EntityGraph.createUniqueGraphID(), statementExecutor);
        }
        
        jailer.setEntityGraph(entityGraph);
        jailer.writeEntities(scriptFile, ScriptType.INSERT, totalProgress, statementExecutor);
        entityGraph.delete();
        
        if (deleteScriptFileName != null) {
            jailer.setEntityGraph(exportedEntities);
            jailer.deleteEntities(subjects, totalProgress, statementExecutor, CommandLineParser.getInstance().getTabuTables(jailer.datamodel));
            jailer.datamodel.transpose();
            jailer.writeEntities(deleteScriptFileName, ScriptType.DELETE, totalProgress, statementExecutor);
            exportedEntities.delete();
        }
        
        jailer.shutDown();
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
     * Calculates D=(E-T)-C*(U-(E-T)) where E is the entity-graph of this export-tool,
     * see http://intra.*.de/dokuwiki/doku.php?id=projekte:sql-export-tool-phase2.
     * 
     * @param subjects set of tables containing subjects of extraction-tasks
     * @param allTables set of tables from which there are entities in E
     * @param statementExecutor for executing SQL-statements
     * @param tabuTables never deletes entities of one of this tables
     */
    private void deleteEntities(Set<Table> subjects, Set<Table> allTables, StatementExecutor statementExecutor, Set<Table> tabuTables) throws Exception {
        appendCommentHeader("");
        appendCommentHeader("Tabu-tables: " + PrintUtil.tableSetAsString(tabuTables, "--                 "));
        _log.info("Tabu-tables: " + PrintUtil.tableSetAsString(tabuTables, null));
        
        final Map<Table, Long> removedEntities = new HashMap<Table, Long>();
        
        // do not check tables in first step having exactly one 1:1 or 1:n association 
        // from another table
        Set<Table> dontCheckInitially = new HashSet<Table>();
        for (Table table: allTables) {
            int n = 0;
            boolean check = false;
            for (Association a: table.associations) {
                if (!a.reversalAssociation.isIgnored()) {
                    if (tabuTables.contains(a.destination)) {
                        check = true;
                    } else if (a.reversalAssociation.getCardinality() == Cardinality.ONE_TO_MANY || a.reversalAssociation.getCardinality() == Cardinality.ONE_TO_ONE) {
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
        
        // remove tabu entities
        for (Table tabuTable: tabuTables) {
            long rc = entityGraph.deleteEntities(tabuTable);
            _log.info("excluded " + rc + " entities from " + tabuTable.getName() + " (tabu)");
            allTables.remove(tabuTable);
        }
        
        // set of tables which are known to have no entities in entityGraph 
        Set<Table> emptyTables = new HashSet<Table>();
        
        Set<Table> tablesToCheck = new HashSet<Table>(allTables);
        _log.info("don't check initially: " + PrintUtil.tableSetAsString(dontCheckInitially, null));
        tablesToCheck.removeAll(dontCheckInitially);
        
        boolean firstStep = true;
        // remove associated entities
        while (!tablesToCheck.isEmpty()) {
            _log.info("tables to check: " + PrintUtil.tableSetAsString(tablesToCheck, null));
            List<JobManager.Job> jobs = new ArrayList<JobManager.Job>(); 
            final Set<Table> tablesToCheckNextTime = new HashSet<Table>();
            for (final Table table: tablesToCheck) {
                for (final Association a: table.associations) {
                    if (emptyTables.contains(table)) {
                        continue;
                    }
                    if (!a.reversalAssociation.isIgnored()) {
                        if (entityGraph.countEntities(table) == 0) {
                            emptyTables.add(table);
                            continue;
                        }
                        final boolean isFirstStep = firstStep;
                        jobs.add(new JobManager.Job() {
                            public void run() throws Exception {
                                long rc = entityGraph.removeAssociatedDestinations(a.reversalAssociation, !isFirstStep);
                                if (rc > 0) {
                                    synchronized (removedEntities) {
                                        Long oldRc = removedEntities.get(table);
                                        removedEntities.put(table, rc + (oldRc == null? 0 : oldRc));
                                        _log.info("excluded " + rc + " entities from " + table.getName() + " referenced by " + a);
                                        for (Association a2: table.associations) {
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
            tablesToCheck = tablesToCheckNextTime;
            tablesToCheck.retainAll(allTables);
            firstStep = false;
        }
        
        _log.info("entities to delete:");
        appendCommentHeader("");
        boolean firstLine = true;
        for (String line: entityGraph.getStatistics()) {
            if (!firstLine) {
                Long re = removedEntities.get(datamodel.getTable(line.split(" ")[0]));
                if (re != null && re != 0L) {
                    line += " (-" + re + ")";
                }
            }
            _log.info(line);
            appendCommentHeader((firstLine ? "entities to delete: " : "     ") + line);
            firstLine = false;               
        }
    }

    /**
     * Prints shortest association between two tables.
     */
    private static void findAssociation(String from, String to, List<String> restModels, boolean undirected) throws Exception {
        DataModel dataModel = new DataModel();
        for (String rm: restModels) {
            if (dataModel.getRestrictionModel() == null) {
                dataModel.setRestrictionModel(new RestrictionModel(dataModel));
            }
            dataModel.getRestrictionModel().addRestrictionDefinition(rm);
        }
        Table source = dataModel.getTable(from);
        if (source == null) {
            throw new RuntimeException("unknown table: '" + from);
        }
        Table destination = dataModel.getTable(to);
        if (destination == null) {
            throw new RuntimeException("unknown table: '" + to);
        }
        
        Set<Table> tablesToIgnore = CommandLineParser.getInstance().getTabuTables(dataModel);
        System.out.println("ignoring: " + PrintUtil.tableSetAsString(tablesToIgnore));
        System.out.println();
        
        Map<Table, Table> successor = new HashMap<Table, Table>();
        Map<Table, Association> outgoingAssociation = new HashMap<Table, Association>();
        List<Table> agenda = new ArrayList<Table>();
        agenda.add(destination);
        
        while (!agenda.isEmpty()) {
            Table table = agenda.remove(0);
            for (Association association: incomingAssociations(table, undirected)) {
                if (!tablesToIgnore.contains(association.source)) {
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
        }
        if (successor.containsKey(source)) {
            System.out.println(source.getName());
            for (Table table = source; !table.equals(destination); table = successor.get(table)) {
                System.out.println(outgoingAssociation.get(table));
            }
        } else {
            System.out.println("tables are not associated");
        }
    }
    
    /**
     * Prints restricted data-model.
     */
    private static void printDataModel(List<String> restrictionModels, boolean printClosures) throws Exception {
        DataModel dataModel = new DataModel();
        if (printClosures) {
            DataModel.printClosures = true;
        }
        for (String rm: restrictionModels.subList(1, restrictionModels.size())) {
            if (dataModel.getRestrictionModel() == null) {
                dataModel.setRestrictionModel(new RestrictionModel(dataModel));
            }
            dataModel.getRestrictionModel().addRestrictionDefinition(rm);
        }

        System.out.println(dataModel);
        
        printCycles(dataModel);
        printComponents(dataModel);
    }

    /**
     * Searches cycles in a data-model and prints out all tables
     * involved in a cycle.
     *  
     * @param dataModel the data-model
     */
    private static void printCycles(DataModel dataModel) {
        Set<Table> tables = new HashSet<Table>(dataModel.getTables());
        Set<Table> independentTables;
        do {
             independentTables = dataModel.getIndependentTables(tables);
             tables.removeAll(independentTables);
        } while (!independentTables.isEmpty());
        if (tables.isEmpty()) {
            System.out.println("no cyclic dependencies" + asString(tables));
        } else {
            System.out.println("tables in dependent-cycle: " + asString(tables));
        }
    }

    /**
     * Searches components in a data-model and prints out all components.
     *  
     * @param dataModel the data-model
     */
    private static void printComponents(DataModel dataModel) {
        Set<Table> tablesToIgnore = getTablesToIgnoreFromComponentAnalysis(dataModel);
        
        System.out.println();
        System.out.println("excluding following tables from component-analysis: " + PrintUtil.tableSetAsString(tablesToIgnore));
        System.out.println();
        
        List<Set<Table>> components = new ArrayList<Set<Table>>();
        Set<Table> tables = new HashSet<Table>(dataModel.getTables());
        tables.removeAll(tablesToIgnore);
        while (!tables.isEmpty()) {
            Table table = tables.iterator().next();
            Set<Table> closure = table.closure(tablesToIgnore, false);
            components.add(closure);
            tables.removeAll(closure);
        }
        System.out.println(components.size() + " components: ");
        for (Set<Table> component: components) {
            System.out.println(PrintUtil.tableSetAsString(component));
        }
    }

    /**
     * Gets set of tables which should not taken into account for component-analysis.
     */
    private static Set<Table> getTablesToIgnoreFromComponentAnalysis(DataModel dataModel) {
        Set<Table> tablesToIgnore = CommandLineParser.getInstance().getTabuTables(dataModel);
        for (Table table: dataModel.getTables()) {
            if (incomingAssociations(table, false).size() > 0 &&
                outgoingAssociations(table).isEmpty()) {
                tablesToIgnore.add(table);
            }
        }
        return tablesToIgnore;
    }

    /**
     * Collects all non-ignored associations with a given table as destination.
     * 
     * @param table the table
     * @return all non-ignored associations with table as destination
     */
    private static Collection<Association> incomingAssociations(Table table, boolean undirected) {
        Collection<Association> result = new ArrayList<Association>();
        for (Association association: table.associations) {
            if (association.reversalAssociation.getJoinCondition() != null ||
                (undirected && association.getJoinCondition() != null)) {
                result.add(association.reversalAssociation);
            }
        }
        return result;
    }

    /**
     * Collects all non-ignored associations with a given table as source.
     * 
     * @param table the table
     * @return all non-ignored associations with table as source
     */
    private static Collection<Association> outgoingAssociations(Table table) {
        Collection<Association> result = new ArrayList<Association>();
        for (Association association: table.associations) {
            if (association.getJoinCondition() != null) {
                result.add(association);
            }
        }
        return result;
    }

    // initialize log4j
    static {
        PropertyConfigurator.configure("config/log4j.properties");
        _log = Logger.getLogger(Jailer.class);
    }

}
