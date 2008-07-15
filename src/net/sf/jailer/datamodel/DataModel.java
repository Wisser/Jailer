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

package net.sf.jailer.datamodel;

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
import java.util.TreeMap;

import net.sf.jailer.database.StatementExecutor;
import net.sf.jailer.restrictionmodel.RestrictionModel;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.PrintUtil;


/**
 * Model of the relational data.
 * 
 * @author Ralf Wisser
 */
public class DataModel {

	/**
     * Maps table-names to tables;
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
     * The restriction model.
     */
    private RestrictionModel restrictionModel;
    
    /**
     * For creation of primary-keys.
     */
    private final PrimaryKeyFactory PrimaryKeyFactory = new PrimaryKeyFactory();

    /**
     * Name of file containing the table definitions.
     */
    public static final String TABLES_FILE = "datamodel/table.csv";

    /**
     * Name of file containing the column definitions.
     */
    public static final String COLUMNS_FILE = "datamodel/column.csv";

    /**
     * Name of file containing the association definitions.
     */
	public static final String ASSOCIATIONS_FILE = "datamodel/association.csv";

	/**
	 * List of tables to be exported completely if in closure from subject.
	 */
	public static final String INITIAL_DATA_TABLES_FILE = "datamodel/initial_data_tables.csv";
	
	/**
	 * List of tables to be excluded from deletion.
	 */
	public static final String EXCLUDE_FROM_DELETION_FILE = "datamodel/exclude-from-deletion.csv";
	
	/**
     * Name of file containing the version number.
     */
    public static final String VERSION_FILE = "datamodel/version.csv";

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
		public String rootTag = "entities";
	}

	/**
	 * XML settings for exportation into XML files.
	 */
	private XmlSettings xmlSettings = new XmlSettings();
	
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
     * Gets display name of a table
     * 
     * @param table the table
     * @return the display name of the table
     */
    public String getDisplayName(Table table) {
        return displayName.get(table);
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
     * 
     * @param additionalTablesFile table file to read too
     * @param additionalAssociationsFile association file to read too
     */
    public DataModel(String additionalTablesFile, String additionalAssociationsFile) throws Exception {
    	// tables
    	CsvFile tablesFile = new CsvFile(new File(TABLES_FILE));
        List<CsvFile.Line> tableList = new ArrayList<CsvFile.Line>(tablesFile.getLines());
        if (additionalTablesFile != null) {
            tableList.addAll(new CsvFile(new File(additionalTablesFile)).getLines());
        }
        for (CsvFile.Line line: tableList) {
            boolean upsert = "Y".equalsIgnoreCase(line.cells.get(1));
            List<Column> pk = new ArrayList<Column>();
            for (int j = 2; j < line.cells.size() && line.cells.get(j).toString().length() > 0; ++j) {
                String col = line.cells.get(j).trim();
                pk.add(Column.parse(col));
            }
            tables.put(line.cells.get(0), new Table(line.cells.get(0), PrimaryKeyFactory.createPrimaryKey(pk), upsert));
        }
        
        // columns
        File file = new File(COLUMNS_FILE);
        if (file.exists()) {
	    	CsvFile columnsFile = new CsvFile(file);
	        List<CsvFile.Line> columnsList = new ArrayList<CsvFile.Line>(columnsFile.getLines());
	        for (CsvFile.Line line: columnsList) {
	            List<Column> columns = new ArrayList<Column>();
	            for (int j = 1; j < line.cells.size() && line.cells.get(j).toString().length() > 0; ++j) {
	                String col = line.cells.get(j).trim();
	                try {
	                	columns.add(Column.parse(col));
	                } catch (Exception e) {
	                	// ignore
					}
	            }
	            Table table = tables.get(line.cells.get(0));
	            if (table != null) {
	            	table.setColumns(columns);
	            }
	        }
        }
        
        // associations
        List<CsvFile.Line> associationList = new ArrayList<CsvFile.Line>(new CsvFile(new File(ASSOCIATIONS_FILE)).getLines());
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
                if (cardinality == null) {
                	cardinality = Cardinality.MANY_TO_MANY;
                }
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
        initDisplayNames();
    }

    /**
     * Initializes display names.
     */
    private void initDisplayNames() {
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
    }
    
    /**
     * Gets the primary-key to be used for the entity-table.
     *
     * @param statementExecutor for null value guessing
     * @return the universal primary key
     */
    public PrimaryKey getUniversalPrimaryKey(StatementExecutor statementExecutor) {
        return PrimaryKeyFactory.getUniversalPrimaryKey(statementExecutor);
    }

    /**
     * Gets the primary-key to be used for the entity-table.
     * 
     * @return the universal primary key
     */
    public PrimaryKey getUniversalPrimaryKey() {
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
	}

}
