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
package net.sf.jailer.domainmodel;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Table;

/**
 * A group of tables to be treated in the same way as a single table
 * for rendering and domain model analysis.
 * 
 * @author Wisser
 */
public class Composite {

    /**
     * The main table representing the entire composite.
     */
    public final Table mainTable;
    
    /**
     * The component tables.
     */
    public final List<Table> componentTables;
    
    /**
     * Constructor.
     * 
     * @param mainTable the main table representing the entire composite
     * @param componentTables the component tables
     */
    public Composite(Table mainTable, List<Table> componentTables) {
        this.mainTable = mainTable;
        this.componentTables = new ArrayList<Table>(componentTables);
        Collections.sort(this.componentTables);
    }

    /**
     * Gets all associations of a composites' table with a table outside.
     * 
     * @return Set of all associations of a composites' table with a table outside
     */
    public Set<Association> getAssociations() {
        Set<Association> associationSet = new HashSet<Association>();
        Set<Table> allTables = new HashSet<Table>();
        allTables.add(mainTable);
        allTables.addAll(componentTables);
        
        for (Table table: allTables) {
            for (Association a: table.associations) {
                if (!allTables.contains(a.destination)) {
                    associationSet.add(a);
                }
            }
        }
        return associationSet;
    }

    /**
     * Stringifies a composite.
     */
    @Override
    public String toString() {
        StringBuffer str = new StringBuffer(componentTables.isEmpty()? "Table " : "Composite ");
        str.append(mainTable.getName());
        return str.toString();
    }

}
