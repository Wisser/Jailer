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
package net.sf.jailer.render;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import net.sf.jailer.CommandLineParser;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.domainmodel.Composite;
import net.sf.jailer.domainmodel.Domain;
import net.sf.jailer.domainmodel.DomainModel;
import net.sf.jailer.util.PrintUtil;
import net.sf.jailer.util.SqlUtil;

import org.apache.log4j.Logger;

/**
 * Generates a human readable HTML-representation of the data-model.
 *  
 * @author Ralf Wisser
 */
public class HtmlDataModelRenderer implements DataModelRenderer {

    private static final String COLOR_KEYWORDS = "font-style: italic; color: rgb(120, 0, 0);";

    /**
     * The directory to put the HTML-render in.
     */
    private final File outputDir;
    
    /**
     * Maximum depth of expansion on table render.
     */
    private final int maxDepth;
    
    /**
     * The logger.
     */
    private static final Logger _log = Logger.getLogger(HtmlDataModelRenderer.class);

    /**
     * Constructor.
     * 
     * @param outputDir the directory to put the HTML-render in
     * @param maxDepth maximum depth of expansion on table render
      */
    public HtmlDataModelRenderer(String outputDir, int maxDepth) {
        this.outputDir = new File(outputDir);
        this.maxDepth = maxDepth;
    }
    
    /** 
     * Generates a human readable HTML-representation of the data-model.
     * 
     * @param dataModel the data-model
     */
    public void render(DataModel dataModel) {
        try {
            List<Table> tableList = new ArrayList<Table>(dataModel.getTables());
            Collections.sort(tableList);
            List<String> tablesColumn = new ArrayList<String>();
            List<String> domainsColumn = new ArrayList<String>();
            DomainModel domainModel = new DomainModel(dataModel);
            
            for (Table table: tableList) {
                Composite composite = domainModel.composites.get(table);
                Domain domain = domainModel.getDomain(table);
                if (composite != null) {
                    tablesColumn.add(linkTo(table));
                    domainsColumn.add(domain == null? "" : "&nbsp;&nbsp;&nbsp;<small>" + linkTo(domain) + "</small>");
                }
                StringBuffer legend = new StringBuffer();
                String closure = renderClosure(domainModel, composite == null? domainModel.getComposite(table) : composite, legend);
                closure = PrintUtil.applyTemplate("template" + File.separatorChar + "table.html", new Object[] { "Closure", "", closure });
                String columns = generateColumnsTable(table);
                if (columns == null) {
                    columns = "";
                } else {
                    columns += "<br>";
                }
                String components = "";
                if (composite != null && composite.componentTables.size() > 0) {
                    components = generateComponentsTable(composite) + "<br>";
                }
                String domainSuffix = "";
                if (domain != null) {
                    domainSuffix = " <small>(" + linkTo(domain) + ")</small>";
                }
                String title = composite == null? "Component " + table.getName() : composite.toString();
                writeFile(new File(outputDir, toFileName(table)), PrintUtil.applyTemplate("template" + File.separator + "tableframe.html", new Object[] { title, renderTableBody(table, table, 0, 1, new HashSet<Table>()), closure + legend, components + columns, domainSuffix }));
            }
            
            String restrictions = "none";
            List<String> restrictionModels = CommandLineParser.getInstance().arguments;
            restrictionModels = restrictionModels.subList(0, restrictionModels.size());
            if (!restrictionModels.isEmpty()) {
                restrictionModels = restrictionModels.subList(1, restrictionModels.size());
            }
            if (!restrictionModels.isEmpty()) {
                restrictions = restrictionModels.toString();
                restrictions = restrictions.substring(1, restrictions.length() - 1);
            }
            
            String domains = "";
            if (!domainModel.getDomains().isEmpty()) {
                domains = renderDomainModel(domainModel) + "<br>";
            }
            
            writeFile(new File(outputDir, "index.html"), PrintUtil.applyTemplate("template" + File.separatorChar + "index.html", new Object[] { new Date(), generateHTMLTable("Tables", null, tablesColumn, domainsColumn), restrictions, domains }));
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Renders the closure of a composite.
     * 
     * @param composite the composite
     * @return render of composite's closure
     */
    private String renderClosure(DomainModel domainModel, Composite composite, StringBuffer legend) throws FileNotFoundException, IOException {
        StringBuffer lines = new StringBuffer();
        int distance = 0;
        Set<Composite> closure = new HashSet<Composite>();
        Set<Composite> associatedComposites = new HashSet<Composite>();
        boolean printLegend = false;
        Domain domain = domainModel.getDomain(composite.mainTable);
        do {
            associatedComposites.clear();
            if (distance == 0) {
                associatedComposites.add(composite);
            } else {
                for (Composite c: closure) {
                    for (Association a: c.getAssociations()) {
                        if (a.getJoinCondition() != null) {
                            Composite destinationComposite = domainModel.getComposite(a.destination);
                            if (!closure.contains(destinationComposite)) {
                                if (!excludeFromClosure(a.destination)) {
                                    associatedComposites.add(destinationComposite);
                                }
                            }
                        }
                    }
                }
            }
            
            List<Composite> cl = new ArrayList<Composite>(associatedComposites);
            Collections.sort(cl, new Comparator<Composite>() {
                public int compare(Composite o1, Composite o2) {
                    return o1.mainTable.compareTo(o2.mainTable);
                }
            });
            StringBuffer ts = new StringBuffer();
            boolean firstTime = true;
            for (Composite dt: cl) {
                if (!firstTime) {
                    ts.append(", ");
                }
                Domain dtDomain = domainModel.getDomain(dt.mainTable);
                boolean differentDomains = false;
                boolean subDomain = false;
                if (dtDomain == null && domain == null) {
                    differentDomains = false;
                } else if (dtDomain == null || domain == null) {
                    differentDomains = true;
                } else {
                    differentDomains = !domain.equals(dtDomain);
                    subDomain = dtDomain.isSubDomainOf(domain);
                }
                
                if (differentDomains && !subDomain) {
                    ts.append("<span style=\"font-style: italic;\">");
                    printLegend = true;
                }
                ts.append(dt.equals(composite)? dt.mainTable.getName() : linkTo(dt.mainTable));
                if (subDomain) {
                    ts.append("*");
                    printLegend = true;
                }
                if (differentDomains && !subDomain) {
                    ts.append("**</span>");
                }
                firstTime = false;
            }
            if (!cl.isEmpty()) {
                lines.append(PrintUtil.applyTemplate("template" + File.separator + "table_line.html", new Object[] { "", "&nbsp;&nbsp;distance&nbsp;" + distance, "", "&nbsp;", ts.toString(), COLOR_KEYWORDS, distance % 2 != 0? "class=\"highlightedrow\"" : "" }));
            }
            ++distance;
            closure.addAll(associatedComposites);
        } while (!associatedComposites.isEmpty());
        if (printLegend) {
            legend.append(PrintUtil.applyTemplate("template" + File.separatorChar + "legend.html", new Object[0]));
        }
        return lines.toString();
    }

    protected boolean excludeFromClosure(Table table) {
        return false;
    }

    protected boolean excludeFromNeighborhood(Table table) {
        return false;
    }

    /**
     * Renders a table.
     * 
     * @param table the table
     * @return render of table
     */
    private String renderTableBody(Table table, Table current, int depth, int indent, Set<Table> alreadyRendered) throws FileNotFoundException, IOException {
        if (alreadyRendered.contains(table)) {
            return "";
        }
        alreadyRendered.add(table);
        
        StringBuffer lines = new StringBuffer();
        List<Association> all = new ArrayList<Association>(table.associations);
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
        int lineNr;
        String prefix = "";
        String gap = "<small><small>&nbsp;<br></small></small>";
        
        if (!dep.isEmpty()) {
            lines.append(tableRow(indent, "depends&nbsp;on"));
            lineNr = 0;
            for (Association association: dep) {
                if (!"".equals(association.toString())) {
                    prefix = gap;
                    lines.append(tableRow(indent, association, current, ++lineNr % 2 == 0));
                }
            }
        }
        if (!hasDep.isEmpty()) {
            lines.append(tableRow(indent, prefix + "has&nbsp;dependent"));
            lineNr = 0;
            for (Association association: hasDep) {
                if (!"".equals(association.toString())) {
                    prefix = gap;
                    lines.append(tableRow(indent, association, current, ++lineNr % 2 == 0));
                }
            }
        }
        if (!assoc.isEmpty()) {
            lines.append(tableRow(indent, prefix + "is&nbsp;associated&nbsp;with"));
            lineNr = 0;
            for (Association association: assoc) {
                if (!"".equals(assoc.toString())) {
                    prefix = gap;
                    lines.append(tableRow(indent, association, current, ++lineNr % 2 == 0));
                }
            }
        }
        if (!ignored.isEmpty()) {
            lines.append(tableRow(indent, prefix + "ignored"));
            lineNr = 0;
            for (Association association: ignored) {
                if (!"".equals(association.toString())) {
                    prefix = gap;
                    lines.append(tableRow(indent, association, current, ++lineNr % 2 == 0));
                }
            }
        }

        StringBuffer result = new StringBuffer(PrintUtil.applyTemplate("template" + File.separator + "table.html", new Object[] { table.equals(current)? "Associations" : linkTo(table), indentSpaces(indent), lines.toString() }));
        
        if (depth < maxDepth) {
            if (depth == 0) {
                result.append("<br>"
                        + PrintUtil.applyTemplate("template" + File.separator + "table.html", new Object[] { "Neighborhood", indentSpaces(1), "" })
                        + "<br>");
            }
            Set<Table> rendered = new HashSet<Table>();
            boolean firstTime = true;
            for (Association association: all) {
                if (!rendered.contains(association.destination)) {
                    if (!excludeFromNeighborhood(association.destination)) {
                        String tableBody = renderTableBody(association.destination, current, depth + 1, indent + 1, alreadyRendered);
                        if (tableBody.length() > 0) {
                            if (!firstTime) {
                                result.append("<br>");
                            }
                            firstTime = false;
                        }
                        result.append(tableBody);
                        rendered.add(association.destination);
                    }
                }
            }
        }
        
        return result.toString();
    }

    /**
     * Generates a row in the table render.
     * 
     * @param indent the indentation
     * @return a row in the table render
     */
    private String tableRow(int indent, String content) throws FileNotFoundException, IOException {
        return PrintUtil.applyTemplate("template" + File.separator + "table_top_line.html", new Object[] { indentSpaces(indent), content, "", "", "", COLOR_KEYWORDS, "" });
    }

    /**
     * Generates a row in the table render.
     * 
     * @param indent the indentation
     * @param highlighted 
     * @return a row in the table render
     */
    private String tableRow(int indent, Association association, Table current, boolean highlighted) throws FileNotFoundException, IOException {
        String jc = association.renderJoinCondition("<span style=\"" + COLOR_KEYWORDS + "\">restricted by</span>");
        String aliasA = "A", aliasB = "B";
        if (!association.source.equals(association.destination)) {
            aliasA = association.source.getName();
            aliasB = association.destination.getName();
        }
        aliasA = linkTo(association.source, aliasA);
        aliasB = linkTo(association.destination, aliasB);
        jc = SqlUtil.replaceAliases(jc, aliasA, aliasB);
        return PrintUtil.applyTemplate("template" + File.separator + "table_line.html", new Object[] { indentSpaces(indent), "&nbsp;&nbsp;" + (association.destination.equals(current)? association.destination.getName() : linkTo(association.destination)), "&nbsp;&nbsp;" + (association.getCardinality() != null? association.getCardinality() : ""), "&nbsp;on&nbsp;", jc, "", highlighted? "class=\"highlightedrow\"" : "" });
    }
    
    /**
     * Generates a HTML render of a table schema.
     * 
     * @param table the table
     * @return HTML render of table schema
     */
    private String generateColumnsTable(Table table) throws SQLException, FileNotFoundException, IOException {
        StringBuffer result = new StringBuffer();
        
        int count = 0;
        for (Column column: table.getColumns()) {
            ++count;
            String COLUMN_NAME = column.name;
            boolean nullable = true;
            String type = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" + column.toSQL(null).substring(column.name.length()).trim().replaceAll(" ", "&nbsp;");
            String constraint = (!nullable ? "&nbsp;&nbsp;&nbsp;&nbsp;<small>NOT&nbsp;NULL</small>" : "");
            boolean isPK = false;
            for (Column c: table.primaryKey.getColumns()) {
                isPK = isPK || c.name.equalsIgnoreCase(COLUMN_NAME);
            }
            result.append(PrintUtil.applyTemplate("template" + File.separator + "table_line.html", new Object[] { indentSpaces(1), "&nbsp;&nbsp;" + COLUMN_NAME, type, "", constraint, isPK? COLOR_KEYWORDS : "", count % 2 == 0? "class=\"highlightedrow\"" : "" }));
        }
        
        return count == 0? null : (PrintUtil.applyTemplate("template" + File.separatorChar + "table.html", new Object[] { "Columns", "", result.toString() }));
    }
    
    /**
     * Generates a HTML render of a components table.
     * 
     * @param composite the composite
     * @return HTML render
     */
    private String generateComponentsTable(Composite composite) throws SQLException, FileNotFoundException, IOException {
        List<String> componentNames = new ArrayList<String>();
        
        for (Table component: composite.componentTables) {
            componentNames.add(linkTo(component));
        }
        return generateHTMLTable("Components", null, componentNames, null);
    }
    
    /**
     * Generates a HTML table.
     */
    private String generateHTMLTable(String title, List<Integer> indents, List<String> column1, List<String> column2) throws FileNotFoundException, IOException {
        StringBuffer result = new StringBuffer();
        for (int i = 0; i < column1.size(); ++i) {
            result.append(PrintUtil.applyTemplate("template" + File.separator + "table_line.html", new Object[] { "", indentSpaces(indents == null? 1 : indents.get(i)) + column1.get(i), column2 == null? "" : column2.get(i), "", "", "", i % 2 != 0? "class=\"highlightedrow\"" : "" }));
        }
        return column1.isEmpty()? null : (PrintUtil.applyTemplate("template" + File.separatorChar + "table.html", new Object[] { title, "", result.toString() }));
    }

    /** 
     * Generates a human readable HTML-representation of the domain-model.
     * 
     * @param domainModel the domain model
     */
    public String renderDomainModel(DomainModel domainModel) throws FileNotFoundException, IOException {
        List<String> column1 = new ArrayList<String>();
        List<String> column2 = new ArrayList<String>();
        List<Integer> indent = new ArrayList<Integer>();
        Set<Domain> renderedDomains = new HashSet<Domain>();
        for (Domain domain: domainModel.getDomains().values()) {
            if (domain.getSuperDomains().isEmpty()) {
                appendDomainTree(domain, column1, column2, 1, indent, domainModel.getDomains().size() + 2, domain.name, renderedDomains);
            }
            List<String> column = new ArrayList<String>();
            for (Domain subDomain: domain.getSubDomains()) {
                column.add(linkTo(subDomain));
            }
            String containsTable = generateHTMLTable("contains", null, column, null);
            String content = ""; 
            if (containsTable != null) {
                content += containsTable + "<br>";
            }
            column.clear();
            for (Domain superDomain: domain.getSuperDomains()) {
                column.add(linkTo(superDomain));
            }
            String partOfTable = generateHTMLTable("Part of", null, column, null);
            if (partOfTable != null) {
                content += partOfTable + "<br>";
            }
            column.clear();
            for (Table table: domain.tables) {
                if (domainModel.composites.get(table) != null) {
                    column.add(linkTo(table));
                }
            }
            String tablesTable = generateHTMLTable("Tables", null, column, null);
            if (tablesTable != null) {
                content += tablesTable + "<br>";
            }
            
            writeFile(new File(outputDir, domain.name + "_DOMAIN.html"), PrintUtil.applyTemplate("template" + File.separator + "tableframe.html", new Object[] { "Domain " + domain.name, content, "", "", "" }));
        }
        return generateHTMLTable("Domains", indent, column1, column2);
    }

    /**
     * Puts lines into HTML render table for the domain model. 
     * 
     * @param domain root domain
     * @param column1 domain name
     * @param column2 domain description
     * @param level current indent level
     * @param indent indent level for each line
     * @param renderedDomains set of already rendered domains (to prevent duplicate rendering)
     */
    private void appendDomainTree(Domain domain, List<String> column1, List<String> column2, int level, List<Integer> indent, int maxLevel, String path, Set<Domain> renderedDomains) {
        if (level > maxLevel) {
            throw new RuntimeException("cyclic domain containment: " + path);
        }
        String suffix = "";
        if (renderedDomains.contains(domain) && !domain.getSubDomains().isEmpty()) {
            suffix = "&nbsp;&#x2191;";
        }
        column1.add(linkTo(domain) + suffix + "&nbsp;&nbsp;&nbsp;");
        column2.add("&nbsp;&nbsp;<small>" + domain.tables.size() + "&nbsp;Tables</small>&nbsp;&nbsp;");
        indent.add(level);
        if (!renderedDomains.contains(domain)) {
            renderedDomains.add(domain);
            for (Domain subDomain: domain.getSubDomains()) {
                appendDomainTree(subDomain, column1, column2, level + 1, indent, maxLevel, path + "->" + subDomain.name, renderedDomains);
            }
        }
    }

    /**
     * Returns Space-string of given length.
     * 
     * @param indent the lenght
     */
    private String indentSpaces(int indent) {
        StringBuffer result = new StringBuffer();
        for (int i = 1; i < indent; ++i) {
            for (int j = 0; j < 8; ++j) {
                result.append("&nbsp;");
            }
        }
        return result.toString();
    }

    /**
     * Returns a HTML-hyper link to the render of a given domain. 
     * 
     * @param domain the domain
     * @return HTML-hyper link to the render of domain
     */
    private String linkTo(Domain domain) {
        return "<a href=\"" + domain.name + "_DOMAIN.html\">" + domain.name + "</a>";
    }

    /**
     * Returns a HTML-hyper link to the render of a given table. 
     * 
     * @param table the table
     * @return HTML-hyper link to the render of table
     */
    private String linkTo(Table table) {
        return linkTo(table, table.getName());
    }

    /**
     * Returns a HTML-hyper link to the render of a given table. 
     * 
     * @param table the table
     * @param name the name of the link
     * @return HTML-hyper link to the render of table
     */
    private String linkTo(Table table, String name) {
        return "<a href=\"" + toFileName(table) + "\">" + name + "</a>";
    }

    /**
     * Gets name of the file containing the HTML render of a given table.
     * 
     * @param table the table
     * @return name of the file containing the HTML render of table
     */
    public static String toFileName(Table table) {
    	StringBuilder sb = new StringBuilder();
    	String tableName = table.getName();
    	String chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_.";
    	
    	for (int i = 0; i < tableName.length(); ++i) {
    		char c = tableName.charAt(i);
    		if (chars.indexOf(c) >= 0) {
    			sb.append(c);
    		}
    	}
    	
    	return sb.toString() + ".html";
    }
    
    /**
     * Writes content into a file.
     * 
     * @param content the content
     * @param file the file
     */
    public static void writeFile(File file, String content) throws IOException {
        PrintWriter out = new PrintWriter(new FileOutputStream(file));
        out.print(content);
        out.close();
        _log.info("file '" + file + "' written");
    }

}
