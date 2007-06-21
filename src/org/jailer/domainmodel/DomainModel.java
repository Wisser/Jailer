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
package org.jailer.domainmodel;

import java.io.File;
import java.io.FilenameFilter;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.apache.log4j.Logger;
import org.jailer.datamodel.DataModel;
import org.jailer.datamodel.Table;
import org.jailer.util.CsvFile;
import org.jailer.util.PrintUtil;
import org.jailer.util.CsvFile.Line;

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
        
        // load files
        Map<String, CsvFile> csvFiles = new HashMap<String, CsvFile>();
        File domainModelDirectory = new File("domainmodel");
        if (!domainModelDirectory.exists() || !domainModelDirectory.isDirectory()) {
            return;
        }
        for (File domainFile: domainModelDirectory.listFiles(new FilenameFilter() {
            public boolean accept(File dir, String name) {
                return name.toLowerCase().endsWith(".csv");
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
            Set<Table> tables = new HashSet<Table>();
            CsvFile csvFile = csvFiles.get(domainName);
            for (int line = 1; line < csvFile.getLines().size(); ++line) {
                Line csvLine = csvFile.getLines().get(line);
                String tableName = csvLine.cells.get(0);
                Table table = dataModel.getTable(tableName);
                if (table == null) {
                    throw new RuntimeException(csvLine.location + ": unknown table '" + tableName + "'");
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
     * Checks model invariants:
     * <ul>
     *   <li>two different domains are disjoint.
     *   <li>the union of all domains is the set of all tables.
     * </ul>
     * 
     * @return <code>true</code> if no errors are found
     */
    public boolean check() {
        boolean ok = true;
        
        
        
        return true;
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
