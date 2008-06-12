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
package net.sf.jailer.restrictionmodel;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.SqlUtil;

import org.apache.log4j.Logger;

/**
 * Restricts association-definions in a {@link DataModel}.
 * 
 * @author Ralf Wisser
 */
public class RestrictionModel {

    /**
     * The data-model.
     */
    private final DataModel dataModel;

    /**
     * Restrictions (in SQL) for associations.
     */
    private final Map<Association, String> restriction = new HashMap<Association, String>();
    
    /**
     * The name of the restriction-files read.
     */
    private List<String> filesRead = new ArrayList<String>();

    /**
     * Whether the restriction-model is transposed.
     */
    private boolean transposed = false;
    
    /**
     * The logger.
     */
    private static final Logger _log = Logger.getLogger(RestrictionModel.class);
 
    /**
     * Constructor.
     * 
     * @param dataModel the data-model
     */
    public RestrictionModel(DataModel dataModel) {
        this.dataModel = dataModel;
    }

    /**
     * Transposes the restriction-model.
     */
    public void transpose() {
        transposed = !transposed;
    }

    /**
     * Is the restriction-model transposed?
     */
    public boolean isTransposed() {
        return transposed;
    }
    
    /**
     * "ignore the association" - restriction.
     */
    public static final String IGNORE = new String("ignore");

    /**
     * Special name for restriction models embedded into extraction model.
     */
	public static final Object EMBEDDED = ".embedded";
    
    /**
     * Gets the restriction (in SQL) for an association.
     * 
     * @param association the association
     * @return the restriction.
     *         <code>null</code> if association is not restricted.
     *         {@link #IGNORE} if association must be ignored.
     */
    public String getRestriction(Association association) {
        if (transposed) {
            association = association.reversalAssociation;
        }
        if (!restriction.containsKey(association)) {
            return null;
        }
        String rest = restriction.get(association);
        if (rest == null) {
            return IGNORE;
        }
        return rest;
    }
    
    /**
     * Adds restrictions defined in a restriction-file.
     * 
     * @param fileName the name of the restriction-file
     */
    public void addRestrictionDefinition(String fileName, String extractionModelFileName) throws Exception {
        File file;
        boolean embedded = false;
        if (EMBEDDED.equals(fileName)) {
        	file = new File(extractionModelFileName);
        	embedded = true;
        } else {
        	file = new File(fileName);
        }
        if (!file.exists()) {
        	try {
        		file = new File(new File(extractionModelFileName).getParent(), fileName);
        	} catch (Exception e) {
        	}
        }
        if (!file.exists()) {
            file = new File("restrictionmodel/" + fileName);
        }
        List<CsvFile.Line> lines = new CsvFile(file).getLines();
        int nr = 0;
        for (CsvFile.Line line: lines) {
        	++nr;
        	if (nr == 1 && embedded) {
        		continue;
        	}
            String location = line.location;
            if ("".equals(line.cells.get(1))) {
                Association association = dataModel.namedAssociations.get(line.cells.get(0));
                if (association == null) {
                    _log.warn(location + ": unknown association '" + line.cells.get(0) + "'");
                    continue;
                }
                String condition = line.cells.get(2);
                if ("".equals(condition)) {
                	_log.warn(location + ": missing condition");
                    continue;
                }
                addRestriction(null, association, condition, location);
                continue;
            }
            Table from;
            boolean reversFrom = false;
            if ("*".equals(line.cells.get(0))) {
                from = null;
            } else {
                if (line.cells.get(0).startsWith("*-")) {
                    from = dataModel.getTable(line.cells.get(0).substring(2));
                    reversFrom = true;
                } else {
                    from = dataModel.getTable(line.cells.get(0));
                }
                if (from == null) {
                	_log.warn(location + ": unknown table '" + line.cells.get(0) + "'");
                    continue;
                }
            }
            Table to;
            List<Table> excludeTo = new ArrayList<Table>();
            if (from != null && "*".equals(line.cells.get(1))) {
                to = null;
            } else if (line.cells.get(1).startsWith("*-")) {
                to = null;
                for (String t: line.cells.get(1).substring(2).split(",")) {
                    t = t.trim();
                    Table excludeTable = dataModel.getTable(t);
                    if (excludeTable == null) {
                    	_log.warn(location + ": unknown table '" + t + "'");
                        continue;
                    }
                    excludeTo.add(excludeTable);
                }
            } else {
                to = dataModel.getTable(line.cells.get(1));
                if (to == null) {
                	_log.warn(location + ": unknown table '" + line.cells.get(1) + "'");
                    continue;
                }
            }
            String condition = line.cells.get(2);
            if ("".equals(condition)) {
                throw new RuntimeException(location + ": missing condition");
            }
            if (from == null || reversFrom) {
                for (Table table: dataModel.getTables()) {
                    if (from != null && table.equals(from)) {
                        continue;
                    }
                    for (Association a: table.associations) {
                        if (a.destination.equals(to)) {
                            if (!a.isInsertDestinationBeforeSource()) {
                                addRestriction(table, a, condition, location);
                            }
                        }
                    }
                }
            } else if (to == null) {
                for (Association a: from.associations) {
                    if (!a.isInsertDestinationBeforeSource()) {
                        if (excludeTo == null || !excludeTo.contains(a.destination)) {
                            addRestriction(from, a, condition, location);
                        }
                    }
            }
            } else {
                for (Association a: from.associations) {
                    if (a.destination.equals(to)) {
                        addRestriction(from, a, condition, location);
                    }
                }
            }
        }
        filesRead.add(fileName);
    }

    /**
     * Adds a restriction to a association.
     * 
     * @param association the association
     * @param condition the restriction-condition
     * @param location location in CSV-file
     */
    public void addRestriction(Table source, Association association, String condition, String location) {
    	addRestriction(source, association, condition, location, false);
    }
    	
    /**
     * Adds a restriction to a association.
     * 
     * @param association the association
     * @param condition the restriction-condition
     * @param location location in CSV-file
     * @param removePreviousRestriction if <code>true</code>, remove any restriction on the association before adding the new one
     */
    public void addRestriction(Table source, Association association, String condition, String location, boolean removePreviousRestriction) {
//        if (association.isInsertDestinationBeforeSource()) {
//            String aName = source == null? association.getName() : (source.getName() + "->" + association.destination.getName());
//            throw new RuntimeException(location + ": can't restrict dependency: " + aName + " condition: " + condition);
//        }
        if ("ignore".equalsIgnoreCase(condition) || "false".equalsIgnoreCase(condition)) {
            condition = null;
        }
        if (condition != null && association.reversed) {
            condition = SqlUtil.reversRestrictionCondition(condition);
        }
        if (removePreviousRestriction && "".equals(condition)) {
        	restriction.remove(association);
        } else if (removePreviousRestriction || !restriction.containsKey(association)) {
            restriction.put(association, condition == null? null : "(" + condition + ")");
        } else {
            String oldCondition = restriction.get(association);
            if (oldCondition == null || condition == null) {
                condition = null;
            } else {
                condition = oldCondition + " and (" + condition + ")";
            }
            restriction.put(association, condition);
        }
    }

    /**
     * Stringifies the restriction model.
     */
    public String toString() {
        return filesRead.toString();
    }

}
