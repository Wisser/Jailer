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

package net.sf.jailer.modelbuilder;

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

import net.sf.jailer.database.StatementExecutor;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Cardinality;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.CsvFile;

import org.apache.log4j.Logger;
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
    public static final String MODEL_BUILDER_TABLES_CSV = "datamodel/model-builder-table.csv";

    /**
     * Name of CSV file for generated association definitions.
     */
    public static final String MODEL_BUILDER_ASSOCIATIONS_CSV = "datamodel/model-builder-association.csv";

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
     * @param warnings string-buffer to print warnings into, may be <code>null</code>
     */
    public static void build(String driverClassName, String dbUrl, String dbUser, String dbPassword, StringBuffer warnings) throws Exception {
        statementExecutor = new StatementExecutor(driverClassName, dbUrl, dbUser, dbPassword);

        resetFiles();

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
        	if (!EXCLUDE_TABLES_CSV.contains(new String[] { table.getName()}) && 
        	    !EXCLUDE_TABLES_CSV.contains(new String[] { table.getName().toUpperCase() })) {
                if (table.primaryKey.getColumns().isEmpty()) {
            		String warning = "Table " + table.getName() + " has no primary key!";
            		warnings.append(warning + "\n");
					_log.warn(warning);
                } else {
                    tableDefinitions += table.getName() + "; N; ";
	                for (Column pk: table.primaryKey.getColumns()) {
	                    tableDefinitions += pk + ";";
	                }
	                tableDefinitions += "   ;" + table.getAuthor() + ";\n";
                }
            }
        }
        
        resetTableFile(tableDefinitions);

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
        Set<String> names = new HashSet<String>();
        for (Table table: dataModel.getTables()) {
        	for (Association association: table.associations) {
        		if (association.getName() != null) {
        			names.add(association.getName());
        		}
        	}
        }
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
            associationDefinition += association.source.getName() + "; " + association.destination.getName() + "; " + firstInsert + "; " + card + "; " + association.getJoinCondition() + 
                                     "; " + name + "; " + association.getAuthor() + ";\n";
        }
        
        resetAssociationFile(associationDefinition);
    }

    private static void resetAssociationFile(String associationDefinition) throws IOException {
        writeFile(MODEL_BUILDER_ASSOCIATIONS_CSV, 
                "# generated by Jailer\n\n" +
                "# Table A;    Table B;  first-insert; cardinality (opt); join-condition; name (opt); author\n" +
                associationDefinition);
    }

    private static void resetTableFile(String tableDefinitions) throws IOException {
        writeFile(MODEL_BUILDER_TABLES_CSV, 
                "# generated by Jailer\n\n" +
                "# Name;   upsert; primary key;    ; author\n" +
                tableDefinitions);
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

    /**
     * Resets 'model-builder-*.csv' files.
     */
    public static void resetFiles() throws IOException {
        resetTableFile("");
        resetAssociationFile("");
    }

}
