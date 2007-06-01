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

package org.jailer.modelbuilder;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.apache.log4j.Logger;
import org.jailer.database.StatementExecutor;
import org.jailer.datamodel.Association;
import org.jailer.datamodel.Cardinality;
import org.jailer.datamodel.Column;
import org.jailer.datamodel.DataModel;
import org.jailer.datamodel.Table;
import org.jailer.util.CsvFile;
import org.springframework.context.support.AbstractXmlApplicationContext;
import org.springframework.context.support.FileSystemXmlApplicationContext;


/**
 * Automatically builds a data-model using several {@link ModelElementFinder}.
 * 
 * Writes all found elements into the files
 * <ul>
 *   <li>datamodel/model-builder-table.csv<li>
 *   <li>datamodel/model-builder-association.csv<li>
 * <ul>
 * 
 * expect the already known elements (table.csv/association.csv)
 * and the excluded elements listet in datamodel/exclude-[tables|associations].csv
 * 
 * @author Wisser
 */
public class ModelBuilder {

    /**
     * The configuration.
     */
    private static AbstractXmlApplicationContext applicationContext = new FileSystemXmlApplicationContext("config/config.xml");

    /**
     * The logger.
     */
    private static final Logger _log = Logger.getLogger(ModelBuilder.class);

    /**
     * Gets all {@link ModelElementFinder}.
     * 
     * @return all {@link ModelElementFinder}
     */
    private static List<ModelElementFinder> getModelElementFinder() throws Exception {
        List<ModelElementFinder> modelElementFinder = (List<ModelElementFinder>) applicationContext.getBean("model-finder");
        return modelElementFinder;
    }
    
    /**
     * The statement executor for executing SQL statements.
     */
    private static StatementExecutor statementExecutor;

    /**
     * Name of CSV file for generated table definitions.
     */
    private static final String MODEL_BUILDER_TABLES_CSV = "datamodel/model-builder-table.csv";

    /**
     * Name of CSV file for generated association definitions.
     */
    private static final String MODEL_BUILDER_ASSOCIATIONS_CSV = "datamodel/model-builder-association.csv";

    /**
     * The exclude-tables file.
     */
    private static final CsvFile EXCLUDE_TABLES_CSV;

    /**
     * The exclude-associations file.
     */
    private static final CsvFile EXCLUDE_ASSOCIATION_CSV;
    
    static {
        try {
            EXCLUDE_TABLES_CSV = new CsvFile(new File("datamodel/exclude-tables.csv"));
            EXCLUDE_ASSOCIATION_CSV = new CsvFile(new File("datamodel/exclude-associations.csv"));
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
  
    /**
     * Builds model.
     * 
     * @param driverClassName the name of the JDBC driver class
     * @param dbUrl URL of the DB
     * @param dbUser name of DB-user
     * @param dbPassword DB-password
     */
    public static void build(String driverClassName, String dbUrl, String dbUser, String dbPassword) throws Exception {
        statementExecutor = new StatementExecutor(driverClassName, dbUrl, dbUser, dbPassword);
        
        writeFile(MODEL_BUILDER_TABLES_CSV, "");
        writeFile(MODEL_BUILDER_ASSOCIATIONS_CSV, "");

        DataModel dataModel = new DataModel();
        Collection<Table> tables = new ArrayList<Table>();
        
        List<ModelElementFinder> modelElementFinder = getModelElementFinder();
        for (ModelElementFinder finder: modelElementFinder) {
            _log.info("find tables with " + finder);
            tables.addAll(finder.findTables(statementExecutor));
        }
        
        Set<Table> written = new HashSet<Table>();
        for (Iterator<Table> iT = tables.iterator(); iT.hasNext(); ) {
            Table table = iT.next();
            if (dataModel.getTable(table.getName()) != null || written.contains(table)) {
                iT.remove();
            } else {
                written.add(table);
            }
        }

        String tableDefinitions = "";
        List<Table> sortedTables = new ArrayList<Table>(tables);
        Collections.sort(sortedTables, new Comparator<Table>() {
            public int compare(Table t1, Table t2) {
                return t1.getName().compareTo(t2.getName());
            }
        });
        for (Table table: sortedTables) {
            if (!EXCLUDE_TABLES_CSV.contains(new String[] { table.getName() })) {
                tableDefinitions += table.getName() + "; N; ";
                for (Column pk: table.primaryKey.getColumns()) {
                    tableDefinitions += pk + ";";
                }
                tableDefinitions += "   ;" + table.getAuthor() + ";\n";
            }
        }
        
        writeFile(MODEL_BUILDER_TABLES_CSV, 
                "# generated file. Do not edit!\n\n" +
                "Name; upsert; primary key;\n" +
                tableDefinitions);

        // re-read data model with new tables
        dataModel = new DataModel(MODEL_BUILDER_TABLES_CSV, MODEL_BUILDER_ASSOCIATIONS_CSV);

        Collection<Association> associations = new ArrayList<Association>();
        for (ModelElementFinder finder: modelElementFinder) {
            _log.info("find associations with " + finder);
            associations.addAll(finder.findAssociations(dataModel, statementExecutor));
        }

        Collection<Association> associationsToWrite = new ArrayList<Association>();
        for (Association association: associations) {
            if (!EXCLUDE_ASSOCIATION_CSV.contains(new String[] { 
                    association.source.getName(),
                    association.destination.getName(),
                    null,
                    association.getJoinCondition()
                    })) {
                if (!contains(association, dataModel)) {
                    insert(association, dataModel);
                    associationsToWrite.add(association);
                }
            }
        }
        
        String associationDefinition = "";
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
            associationDefinition += association.source.getName() + "; " + association.destination.getName() + "; " + firstInsert + "; " + card + "; " + association.getJoinCondition() + 
                                     ";   ; " + association.getAuthor() + ";\n";
        }
        
        writeFile(MODEL_BUILDER_ASSOCIATIONS_CSV, 
                "# generated file. Do not edit!\n\n" +
                "Table A; Table B; first-insert; cardinality (opt); join-condition; name (opt); author (opt)\n" +
                associationDefinition);
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
     * Checks if an association is already in a model.
     * 
     * @param association the association
     * @param dataModel the model
     * @return <code>true</code> iff association is already in model
     */
    private static boolean contains(Association association, DataModel dataModel) {
        for (Association a: association.source.associations) {
            if (a.source.equals(association.source)) {
                if (a.destination.equals(association.destination)) {
                    if (a.isInsertDestinationBeforeSource() || !association.isInsertDestinationBeforeSource()) {
                        if (a.isInsertSourceBeforeDestination() || !association.isInsertSourceBeforeDestination()) {
                            if (a.getJoinCondition().equals(association.getJoinCondition())) {
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
        PrintWriter out = new PrintWriter(new FileOutputStream(fileName));
        out.print(content);
        out.close();
        _log.info("file '" + fileName + "' written");
    }

}
