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

package org.jailer.datamodel;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.jailer.restrictionmodel.RestrictionModel;
import org.jailer.util.CsvFile;
import org.jailer.util.PrintUtil;


/**
 * Model of the relational data.
 * 
 * @author Wisser
 */
public class DataModel {

    /**
     * Maps table-names to Tables;
     */
    private Map<String, Table> tables = new HashMap<String, Table>();
    
    /**
     * Maps association-names to associations;
     */
    public Map<String, Association> namedAssociations = new HashMap<String, Association>();
    
    /**
     * The restriction model.
     */
    private RestrictionModel restrictionModel;
    
    /**
     * For creation of primary-keys.
     */
    private final PrimaryKeyFactory PrimaryKeyFactory = new PrimaryKeyFactory();
    
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
    public DataModel() throws Exception {
        this(null, null);
    }

    /**
     * Reads in <code>table.csv</code> and <code>association.csv</code>
     * and builds the relational data model.
     */
    public DataModel(String additionalTablesFile, String additionalAssociationsFile) throws Exception {
        // tables
        Pattern typeWithSize = Pattern.compile("([^ ]+) +([^ \\(]+)\\(([0-9]+)\\)");
        Pattern typeWithoutSize = Pattern.compile("([^ ]+) +([^ \\(]+)");
        CsvFile tablesFile = new CsvFile(new File("datamodel/table.csv"));
        List<CsvFile.Line> tableList = new ArrayList<CsvFile.Line>(tablesFile.getLines());
        if (additionalTablesFile != null) {
            tableList.addAll(new CsvFile(new File(additionalTablesFile)).getLines());
        }
        for (CsvFile.Line line: tableList) {
            boolean upsert = "Y".equalsIgnoreCase(line.cells.get(1));
            List<Column> pk = new ArrayList<Column>();
            for (int j = 2; j < line.cells.size() && line.cells.get(j).toString().length() > 0; ++j) {
                String col = line.cells.get(j).trim();
                String name, type;
                int size = 0;
                Matcher matcher = typeWithSize.matcher(col);
                if (matcher.matches()) {
                    name = matcher.group(1);
                    type = matcher.group(2);
                    size = Integer.parseInt(matcher.group(3));
                } else {
                    matcher = typeWithoutSize.matcher(col);
                    if (matcher.matches()) {
                        name = matcher.group(1);
                        type = matcher.group(2);
                    } else {
                        throw new RuntimeException(line.location + ": can't parse primary-key");
                    }
                }
                pk.add(new Column(name, type, size));
            }
            tables.put(line.cells.get(0), new Table(line.cells.get(0), PrimaryKeyFactory.createPrimaryKey(pk), upsert));
        }
        
        // associations
        List<CsvFile.Line> associationList = new ArrayList<CsvFile.Line>(new CsvFile(new File("datamodel/association.csv")).getLines());
        if (additionalAssociationsFile != null) {
            associationList.addAll(new CsvFile(new File(additionalAssociationsFile)).getLines());
        }
        for (CsvFile.Line line: associationList) {
            String location = line.location;
            try {
                Table tableA = (Table) tables.get(line.cells.get(0));
                if (tableA == null) {
                    throw new RuntimeException("Table '" + line.cells.get(0) + "' not found");
                }
                Table tableB = (Table) tables.get(line.cells.get(1));
                if (tableB == null) {
                    throw new RuntimeException("Table '" + line.cells.get(1) + "' not found");
                }
                boolean insertSourceBeforeDestination = "A".equalsIgnoreCase(line.cells.get(2)); 
                boolean insertDestinationBeforeSource = "B".equalsIgnoreCase(line.cells.get(2));
                Cardinality cardinality = Cardinality.parse(line.cells.get(3).trim());
                String joinCondition = line.cells.get(4);
                String name = line.cells.get(5);
                if ("".equals(name)) {
                    name = null;
                }
                Association associationA = new Association(tableA, tableB, insertSourceBeforeDestination, insertDestinationBeforeSource, joinCondition, this, false, cardinality);
                Association associationB = new Association(tableB, tableA, insertDestinationBeforeSource, insertSourceBeforeDestination, joinCondition, this, true, cardinality.reverse());
                associationA.reversalAssociation = associationB;
                associationB.reversalAssociation = associationA;
                tableA.associations.add(associationA);
                tableB.associations.add(associationB);
                if (name != null) {
                    if (namedAssociations.put(name, associationA) != null) {
                        throw new RuntimeException("duplicate association name: " + name);
                    }
                    associationA.setName(name);
                    name = "inverse-" + name;
                    if (namedAssociations.put(name, associationB) != null) {
                        throw new RuntimeException("duplicate association name: " + name);
                    }
                    associationB.setName(name);
                }
            } catch (Exception e) {
                throw new RuntimeException(location + ": " + e.getMessage(), e);
            }
        }
    }

    /**
     * Gets the primary-key to be used for the entity-table.
     * 
     * @return the universal primary key
     */
    public PrimaryKey getUniversalPrimaryKey() {
        return PrimaryKeyFactory.getUniversalPrimaryKey();
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
        Set<Table> independentTables = new HashSet<Table>();
        
        for (Table table: tableSet) {
            boolean depends = false;
            for (Association a: table.associations) {
                if (tableSet.contains(a.destination)) {
                    if (a.getJoinCondition() != null) {
                        if (a.isInsertDestinationBeforeSource()) {
                            depends = true;
                            break;
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
    }
    
    /**
     * Stringifies the data model.
     */
    public String toString() {
        List<Table> sortedTables = new ArrayList<Table>(getTables());
        Collections.sort(sortedTables, new Comparator<Table>() {
            public int compare(Table o1, Table o2) {
                return o1.getName().compareTo(o2.getName());
            }
        });
        StringBuffer str = new StringBuffer();
        if (restrictionModel != null) {
            str.append("restricted by: " + restrictionModel + "\n");
        }
        for (Table table: sortedTables) {
            str.append(table);
            if (printClosures) {
                str.append("  closure =");
                str.append(PrintUtil.tableSetAsString(table.closure(true)) + "\n\n");
            }
        }
        return str.toString();
    }

    /**
     * Printing-mode.
     */
    public static boolean printClosures = false;

    public Set<Table> normalize(Set<Table> tables) {
        Set<Table> result = new HashSet<Table>();
        for (Table table: tables) {
            result.add(getTable(table.getName()));
        }
        return result;
    }

}
