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

package org.jailer.render;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.log4j.Logger;
import org.jailer.CommandLineParser;
import org.jailer.database.StatementExecutor;
import org.jailer.datamodel.Association;
import org.jailer.datamodel.Column;
import org.jailer.datamodel.DataModel;
import org.jailer.datamodel.Table;
import org.jailer.util.PrintUtil;
import org.jailer.util.SqlUtil;

/**
 * Generates a human readable HTML-representation of the data-model.
 *  
 * @author Wisser
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
     * @param statementExecutor for accessing the DB
     */
    public void render(DataModel dataModel, StatementExecutor statementExecutor) {
        try {
            List<Table> tableList = new ArrayList<Table>(dataModel.getTables());
            Collections.sort(tableList);
            StringBuffer listItems = new StringBuffer();
            
            for (Table table: tableList) {
                listItems.append(PrintUtil.applyTemplate("template" + File.separatorChar + "index_table.html", new Object[] { linkTo(table) }));
                String closure = renderClosure(table);
                closure = PrintUtil.applyTemplate("template" + File.separatorChar + "table.html", new Object[] { "Closure", "", closure });
                String columns = generateColumnsTable(table, statementExecutor);
                if (columns == null) {
                    closure = "";
                }
                writeFile(new File(outputDir, table.getName() + ".html"), PrintUtil.applyTemplate("template/tableframe.html", new Object[] { table.getName(), renderTableBody(table, table, 0, 1, new HashSet<Table>()), closure, columns }));
            }
            
            String restrictions = "none";
            List<String> restrictionModels = CommandLineParser.getInstance().arguments;
            restrictionModels = restrictionModels.subList(4, restrictionModels.size());
            if (!restrictionModels.isEmpty()) {
                restrictionModels = restrictionModels.subList(1, restrictionModels.size());
            }
            if (!restrictionModels.isEmpty()) {
                restrictions = restrictionModels.toString();
                restrictions = restrictions.substring(1, restrictions.length() - 1);
            }
            
            writeFile(new File(outputDir, "index.html"), PrintUtil.applyTemplate("template" + File.separatorChar + "index.html", new Object[] { new Date(), listItems.toString(), restrictions }));
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Renders the closure of a table.
     * 
     * @param table the table
     * @return render of table's closure
     */
    private String renderClosure(Table table) throws FileNotFoundException, IOException {
        StringBuffer lines = new StringBuffer();
        int distance = 0;
        Set<Table> closure = new HashSet<Table>();
        Set<Table> associatedTables = new HashSet<Table>();
        do {
            associatedTables.clear();
            if (distance == 0) {
                associatedTables.add(table);
            } else {
                for (Table t: closure) {
                    for (Association a: t.associations) {
                        if (a.getJoinCondition() != null) {
                            if (!closure.contains(a.destination)) {
                                if (!excludeFromClosure(a.destination)) {
                                    associatedTables.add(a.destination);
                                }
                            }
                        }
                    }
                }
            }
            
            List<Table> cl = new ArrayList<Table>(associatedTables);
            Collections.sort(cl);
            StringBuffer ts = new StringBuffer();
            boolean firstTime = true;
            for (Table dt: cl) {
                if (!firstTime) {
                    ts.append(", ");
                }
                ts.append(dt.equals(table)? dt.getName() : linkTo(dt));
                firstTime = false;
            }
            if (!cl.isEmpty()) {
                lines.append(PrintUtil.applyTemplate("template/table_line.html", new Object[] { "", "&nbsp;&nbsp;distance&nbsp;" + distance, "", "&nbsp;", ts.toString(), COLOR_KEYWORDS, distance % 2 != 0? "class=\"highlightedrow\"" : "" }));
            }
            ++distance;
            closure.addAll(associatedTables);
        } while (!associatedTables.isEmpty());
        return lines.toString();
    }

    protected boolean excludeFromClosure(Table table) {
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

        StringBuffer result = new StringBuffer(PrintUtil.applyTemplate("template/table.html", new Object[] { table.equals(current)? "Associations" : linkTo(table), indentSpaces(indent), lines.toString() }));
        
        if (depth < maxDepth) {
            if (depth == 0) {
                result.append("<br>"
                        + PrintUtil.applyTemplate("template/table.html", new Object[] { "Neighborhood", indentSpaces(1), "" })
                        + "<br>");
            }
            Set<Table> rendered = new HashSet<Table>();
            boolean firstTime = true;
            for (Association association: all) {
                if (!rendered.contains(association.destination)) {
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
        
        return result.toString();
    }

    /**
     * Generates a row in the table render.
     * 
     * @param indent the indentation
     * @return a row in the table render
     */
    private String tableRow(int indent, String content) throws FileNotFoundException, IOException {
        return PrintUtil.applyTemplate("template/table_top_line.html", new Object[] { indentSpaces(indent), content, "", "", "", COLOR_KEYWORDS, "" });
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
        if (!association.destination.equals(current)) {
            jc = jc.replaceAll("B\\.", linkTo(association.destination, "B."));
        }
        return PrintUtil.applyTemplate("template/table_line.html", new Object[] { indentSpaces(indent), "&nbsp;&nbsp;" + (association.destination.equals(current)? association.destination.getName() : linkTo(association.destination)), "&nbsp;&nbsp;" + (association.getCardinality() != null? association.getCardinality() : ""), "&nbsp;on&nbsp;", jc, "", highlighted? "class=\"highlightedrow\"" : "" });
    }
    
    /**
     * Generates a HTML render of a table schema.
     * 
     * @param table the table
     * @param statementExecutor for accessing the DB
     * @return HTML render of table schema
     */
    private String generateColumnsTable(Table table, StatementExecutor statementExecutor) throws SQLException, FileNotFoundException, IOException {
        DatabaseMetaData metaData = statementExecutor.getMetaData();
        StringBuffer result = new StringBuffer();
        
        ResultSet rs = metaData.getColumns(null, statementExecutor.dbUser.toUpperCase(), table.getName(), null);
        int count = 0;
        while (rs.next()) {
            ++count;
            String COLUMN_NAME = rs.getString("COLUMN_NAME");
            String SQL_TYPE = SqlUtil.SQL_TYPE.get(rs.getInt("DATA_TYPE"));
            boolean nullable = "columnNullable".equals(rs.getString("NULLABLE"));
            String type = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" + SQL_TYPE;
            String constraint = (!nullable ? "&nbsp;&nbsp;&nbsp;&nbsp;<small>NOT&nbsp;NULL</small>" : "");
            boolean isPK = false;
            for (Column c: table.primaryKey.getColumns()) {
                isPK = isPK || c.name.equalsIgnoreCase(COLUMN_NAME);
            }
            result.append(PrintUtil.applyTemplate("template/table_line.html", new Object[] { indentSpaces(1), "&nbsp;&nbsp;" + COLUMN_NAME, type, "", constraint, isPK? COLOR_KEYWORDS : "", count % 2 == 0? "class=\"highlightedrow\"" : "" }));
        }
        rs.close();
        
        return count == 0? null : (PrintUtil.applyTemplate("template" + File.separatorChar + "table.html", new Object[] { "Columns", "", result.toString() }));
    }

    /**
     * Returns Space-string of given length.
     * 
     * @param indent the lenght
     */
    private String indentSpaces(int indent) {
        StringBuffer result = new StringBuffer();
        for (int i = 1; i < indent; ++i) {
            for (int j = 0; j < 10; ++j) {
                result.append("&nbsp;");
            }
        }
        return result.toString();
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
        return "<a href=\"" + table.getName() + ".html\">" + name + "</a>";
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
