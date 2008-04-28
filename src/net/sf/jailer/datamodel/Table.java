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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Describes a database-table.
 * 
 * @author Ralf Wisser
 */
public class Table extends ModelElement implements Comparable<Table> {

    /**
     * The table-name.
     */
    private final String name;
    
    /**
     * The primary-key of the table.
     */
    public final PrimaryKey primaryKey;
    
    /**
     * Associations to other tables.
     */
    public final List<Association> associations = new ArrayList<Association>();
    
    /**
     * Use upsert (merge) or insert-statement for entities of this table in export-script.
     */
    public final boolean upsert;
    
    /**
     * Constructor.
     * 
     * @param name the table-name
     * @param primaryKey the names of the primary-key columns
     * @param upsert use upsert (merge) or insert-statement for entities of this table in export-script
     */
    public Table(String name, PrimaryKey primaryKey, boolean upsert) {
        this.name = name;
        this.primaryKey = primaryKey;
        this.upsert = upsert;
    }

    /**
     * Gets the table name.
     * 
     * @return the table name
     */
    public String getName() {
        return name;
    }

    /**
     * Compares tables.
     */
    public boolean equals(Object other) {
        if (other instanceof Table) {
            return name.equals(((Table) other).name);
        }
        return false;
    }

    /**
     * The hash-code.
     */
    public int hashCode() {
        return name.hashCode();
    }
    
    /**
     * Stringifies the table.
     */
    public String toString() {
        String str = name + " (" + primaryKey + ")\n";
        List<Association> all = new ArrayList<Association>(associations);
        Collections.sort(all, new Comparator<Association>() {
            public int compare(Association o1, Association o2) {
                return o1.destination.getName().compareTo(o2.destination.getName());
            }
        });
        
        List<Association> dep = new ArrayList<Association>();
        List<Association> hasDep = new ArrayList<Association>();
        List<Association> assoc = new ArrayList<Association>();
        List<Association> ignored = new ArrayList<Association>();
        for (Association association: all) {
            if (association.isIgnored()) {
                ignored.add(association);
            } else if (association.isInsertDestinationBeforeSource()) {
                dep.add(association);
            } else if (association.isInsertSourceBeforeDestination()) {
                hasDep.add(association);
            } else {
                assoc.add(association);
            }
        }
        
        if (!dep.isEmpty()) {
            str += "\n  depends on:\n";
            for (Association association: dep) {
                if (!"".equals(association.toString())) {
                    str += "    " + association + "\n";
                }
            }
        }
        if (!hasDep.isEmpty()) {
            str += "\n  has dependent:\n";
            for (Association association: hasDep) {
                if (!"".equals(association.toString())) {
                    str += "    " + association + "\n";
                }
            }
        }
        if (!assoc.isEmpty()) {
            str += "\n  is associated with:\n";
            for (Association association: assoc) {
                if (!"".equals(assoc.toString())) {
                    str += "    " + association + "\n";
                }
            }
        }
        if (!ignored.isEmpty()) {
            str += "\n  ignored:\n";
            for (Association association: ignored) {
                if (!"".equals(association.toString())) {
                    str += "    " + association + "\n";
                }
            }
        }

        return str + "\n";
    }

    public int compareTo(Table o) {
        return name.compareTo(o.name);
    }
    
    /**
     * Gets the closure of the table.
     * 
     * @param directed consider associations as directed?
     * 
     * @return closure of the table (all tables associated (in-)direct with table)
     */
    public Set<Table> closure(boolean directed) {
        return closure(new HashSet<Table>(), new HashSet<Table>(), directed);
    }

    /**
     * Gets the closure of the table.
     * 
     * @param directed consider associations as directed?
     * @param tablesToIgnore ignore this tables
     * 
     * @return closure of the table (all tables associated (in-)direct with table)
     */
    public Set<Table> closure(Set<Table> tablesToIgnore, boolean directed) {
        return closure(new HashSet<Table>(), tablesToIgnore, directed);
    }

    /**
     * Gets the closure of the table.
     * 
     * @param tables tables known in closure
     * @param directed consider associations as directed?
     * @param tablesToIgnore ignore this tables
     * 
     * @return closure of the table (all tables associated (in-)direct with table)
     */
    private Set<Table> closure(Set<Table> tables, Set<Table> tablesToIgnore, boolean directed) {
        Set<Table> closure = new HashSet<Table>();
        if (!tables.contains(this) && !tablesToIgnore.contains(this)) {
            closure.add(this);
            tables.add(this);
            for (Association association: associations) {
                if (!tables.contains(association.destination)) {
                    if (association.getJoinCondition() != null || (association.reversalAssociation.getJoinCondition() != null && !directed)) {
                        closure.addAll(association.destination.closure(tables, tablesToIgnore, directed));
                    }
                }
            }
        }
        return closure;
    }

}
