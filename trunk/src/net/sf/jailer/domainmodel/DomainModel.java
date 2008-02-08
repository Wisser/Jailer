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

import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.PrintUtil;
import net.sf.jailer.util.CsvFile.Line;

import org.apache.log4j.Logger;

/**
 * Partitioning of the entire data model
 * into smaller sets of functionally related tables.
 * 
 * The definition of a domain model helps you to 
 * define the restriction and extraction models.
 * 
 * @author Wisser
 */
public class DomainModel {

    /**
     * Named domains.
     */
    Map<String, Domain> domains = new TreeMap<String, Domain>();
    
    /**
     * The data model.
     */
    private final DataModel dataModel;

    /**
     * Maps each table to the composite where the table is the main table.
     */
    public final Map<Table, Composite> composites;
    
    /**
     * The logger.
     */
    private static final Logger _log = Logger.getLogger(DomainModel.class);

    /**
     * Constructor.
     * 
     * Loads the domain definitions in directory 'domainmodel'.
     */
    public DomainModel(DataModel dataModel) throws Exception {
        this.dataModel = dataModel;
        composites = new TreeMap<Table, Composite>();
        loadComposites();
        
        // load files
        Map<String, CsvFile> csvFiles = new HashMap<String, CsvFile>();
        File domainModelDirectory = new File("domainmodel");
        if (!domainModelDirectory.exists() || !domainModelDirectory.isDirectory()) {
            return;
        }
        for (File domainFile: domainModelDirectory.listFiles(new FilenameFilter() {
            public boolean accept(File dir, String name) {
                return !"composites.csv".equalsIgnoreCase(name) && name.toLowerCase().endsWith(".csv");
            }
        })) {
            _log.info("loading domain " + domainFile);
            CsvFile csvFile = new CsvFile(domainFile);
            if (csvFile.getLines().isEmpty()) {
                throw new RuntimeException("empty domain file: " + domainFile);
            }
            String domainName = domainFile.getName().substring(0, domainFile.getName().length() - 4);
            if (domainName.length() == 0) {
                throw new RuntimeException("no domain name in: " + domainFile);
            }
            csvFiles.put(domainName, csvFile);
        }
        
        // create domains
        for (String domainName: csvFiles.keySet()) {
            Set<Table> tables = new TreeSet<Table>();
            CsvFile csvFile = csvFiles.get(domainName);
            for (int line = 1; line < csvFile.getLines().size(); ++line) {
                Line csvLine = csvFile.getLines().get(line);
                String tableName = csvLine.cells.get(0);
                Table table = dataModel.getTable(tableName);
                if (table == null) {
                    throw new RuntimeException(csvLine.location + ": unknown table '" + tableName + "'");
                }
                if (composites.get(table) == null) {
                    throw new RuntimeException(csvLine.location + ": table '" + tableName + "' is component of " + getComposite(table) + ".");
                } else {
                    tables.addAll(composites.get(table).componentTables);
                }
                tables.add(table);
            }
            Domain domain = new Domain(domainName, tables);
            domains.put(domainName, domain);
        }
        
        // link domains
        for (String domainName: csvFiles.keySet()) {
            Domain domain = domains.get(domainName);
            Line containsLine = csvFiles.get(domainName).getLines().get(0);
            for (String subDomainName: containsLine.cells) {
                if (subDomainName != null && subDomainName.length() > 0) {
                    Domain subDomain = domains.get(subDomainName);
                    if (subDomain == null) {
                        throw new RuntimeException(containsLine.location + ": unknown domain '" + subDomainName + "'");
                    }
                    domain.subDomains.add(subDomain);
                    subDomain.superDomains.add(domain);
                }
            }
        }
    }
    
    /**
     * Loads the composites definition file.
     */
    private void loadComposites() throws Exception {
        File compositesDefinition = new File("domainmodel" + File.separator + "composites.csv");
        Map<Table, CsvFile.Line> compositeDefinitionOfTable = new HashMap<Table, Line>();
        if (compositesDefinition.exists()) {
            CsvFile compostitesCsvFile = new CsvFile(compositesDefinition);
            for (CsvFile.Line line: compostitesCsvFile.getLines()) {
                Table table = dataModel.getTable(line.cells.get(0));
                if (table == null) {
                    throw new RuntimeException("unknown table '" + line.cells.get(0) + "'");
                }
                if (compositeDefinitionOfTable.containsKey(table)) {
                    throw new RuntimeException("duplicate composite definition for '" + line.cells.get(0) + "'");
                }
                compositeDefinitionOfTable.put(table, line);
            }
        } else {
            _log.info("no composites definition (" + compositesDefinition + ")");
        }
        Set<Table> allComponents = new HashSet<Table>();
        for (Table table: dataModel.getTables()) {
            CsvFile.Line line = compositeDefinitionOfTable.get(table);
            if (line != null) {
                allComponents.add(table);
                List<Table> components = new ArrayList<Table>();
                for (int i = 1; line.cells.get(i).length() > 0; ++i) {
                    Table component = dataModel.getTable(line.cells.get(i));
                    if (component == null) {
                        throw new RuntimeException("unknown table '" + line.cells.get(i) + "'");
                    }
                    allComponents.add(component);
                    components.add(component);
                }
                composites.put(table, new Composite(table, components));
            }
        }
        for (Table table: dataModel.getTables()) {
            if (!allComponents.contains(table)) {
                composites.put(table, new Composite(table, new ArrayList<Table>()));
            }
        }
    }

    /**
     * Gets all domains.
     * 
     * @return a map from domain names to domains
     */
    public Map<String, Domain> getDomains() {
        return domains;
    }
    
    /**
     * Gets domain of table.
     *
     * @param table the table
     * @return domain of table or <code>null</code>, if table belongs to no domain
     */
    public Domain getDomain(Table table) {
        for (Domain domain: domains.values()) {
            if (domain.tables.contains(table)) {
                return domain;
            }
        }
        return null;
    }
    
    /**
     * Cache for {@link #getComposite(Table)}.
     */
    private Map<Table, Composite> componentCache = new HashMap<Table, Composite>();
    
    /**
     * Gets composite in which a given table is contained.
     *
     * @param table the table
     * @return composite in which a table is contained
     */
    public Composite getComposite(Table table) {
        if (componentCache.containsKey(table)) {
            return componentCache.get(table);
        }
        for (Composite composite: composites.values()) {
            if (composite.mainTable.equals(table) || composite.componentTables.contains(table)) {
                componentCache.put(table, composite);
                return composite;
            }
        }
        return null;
    }
    
    /**
     * Checks model invariants:
     * <ul>
     *   <li>two different domains are disjoint.
     *   <li>the union of all domains is the set of all tables.
     * </ul>
     * 
     * @return <code>true</code> if no errors are found
     */
    public boolean check() {
        Set<Table> withoutDomain = new TreeSet<Table>();
        for (Table table: dataModel.getTables()) {
            Set<Domain> domainsOfTable = new TreeSet<Domain>();
            for (Domain domain: domains.values()) {
                if (domain.tables.contains(table)) {
                    domainsOfTable.add(domain);
                }
            }
            if (domainsOfTable.size() == 0) {
                withoutDomain.add(table);
            }
            if (domainsOfTable.size() > 1) {
                printError("Table '" + table.getName() + "' in more than 1 domain: " + domainsOfTable);
            }
        }
        if (withoutDomain.size() > 0) {
            warn("Tables without domain: " + PrintUtil.tableSetAsString(withoutDomain, "    "));
        }
        
        // escape analysis
        for (Domain domain: domains.values()) {
            for (Table table: domain.tables) {
                for (Association association: table.associations) {
                    if (!association.isIgnored()) {
                        Domain destinationDomain = getDomain(association.destination);
                        if (destinationDomain == null || (!destinationDomain.equals(domain) && !destinationDomain.isSubDomainOf(domain))) {
                            String associationName = association.source.getName() + "->" + association.destination.getName();
                            warn((association.isInsertDestinationBeforeSource()? "dependency '" : "association '") + associationName + "' deserts " + domain + (destinationDomain != null? " to " + destinationDomain : ""));
                        }
                    }
                }
            }
        }

        System.out.println(numberOfErrors + " errors");
        System.out.println(numberOfWarnings + " warnings");
        
        return numberOfErrors == 0;
    }
    
    private int numberOfWarnings = 0;
    private int numberOfErrors = 0;
    
    /**
     * Prints warning message.
     * 
     * @param message the message
     */
    private void warn(String message) {
        System.out.println("warning: " + message);
        ++numberOfWarnings;
    }

    /**
     * Prints error message.
     * 
     * @param message the message
     */
    private void printError(String message) {
        System.out.println("error: " + message);
        ++numberOfErrors;
    }

    /**
     * Stringifies the model.
     */
    public String toString() {
        StringBuffer modelAsString = new StringBuffer("domain model:\n");
        for (Domain domain: domains.values()) {
            if (domain.superDomains.isEmpty()) {
                modelAsString.append(domainTree(domain, 0, domains.size() + 2));
            }
        }
        modelAsString.append("\n");
        for (Domain domain: domains.values()) {
            modelAsString.append(domain.name + " = " + PrintUtil.tableSetAsString(domain.tables, "        ") + "\n");
        }
        return modelAsString.toString();
    }

    /**
     * Stringifies a domain with sub-domains.
     * 
     * @param domain the domain
     * @param maxLevel max indent level
     */
    private StringBuffer domainTree(Domain domain, int indent, int maxLevel) {
        if (indent > maxLevel) {
            throw new RuntimeException("cyclic domain containment: '" + domain.name + "'");
        }
        
        StringBuffer domainAsString = new StringBuffer();
        
        for (int i = 0; i < indent; ++i) {
            domainAsString.append("  ");
        }
        domainAsString.append(domain.name + "\n");
        for (Domain subDomain: domain.subDomains) {
            domainAsString.append(domainTree(subDomain, indent + 1, maxLevel));
        }
        
        return domainAsString;
    }
    
}
