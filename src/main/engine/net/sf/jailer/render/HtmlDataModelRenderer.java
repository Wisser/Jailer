/*
 * Copyright 2007 - 2023 Ralf Wisser.
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
import java.nio.charset.Charset;
import java.nio.charset.CharsetEncoder;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.CancellationException;
import net.sf.jailer.util.CancellationHandler;
import net.sf.jailer.util.PrintUtil;
import net.sf.jailer.util.SqlUtil;

/**
 * Generates a human readable HTML-representation of the data-model.
 *  
 * @author Ralf Wisser
 */
public class HtmlDataModelRenderer implements DataModelRenderer {

	private static final String COLOR_KEYWORDS = "font-style: italic; color: rgb(180, 0, 0);";

	/**
	 * The directory to put the HTML-render in.
	 */
	private String outputFolder;
	
	/**
	 * Maximum depth of expansion on table render.
	 */
	private int maxDepth;

	private String overviewHtml = null;
	
	/**
	 * The logger.
	 */
	private static final Logger _log = LoggerFactory.getLogger(HtmlDataModelRenderer.class);

	public static final String CONTENT_FOLDER_NAME = "tables";

	/**
	 * @return the outputDir
	 */
	public String getOutputFolder() {
		return outputFolder;
	}

	/**
	 * @param outputFolder the outputDir to set
	 */
	public void setOutputFolder(String outputFolder) {
		this.outputFolder = outputFolder;
	}

	/**
	 * @return the maxDepth
	 */
	public int getMaxDepth() {
		return maxDepth;
	}

	/**
	 * @param maxDepth the maxDepth to set
	 */
	public void setMaxDepth(int maxDepth) {
		this.maxDepth = maxDepth;
	}

	/** 
	 * Generates a human readable HTML-representation of the data-model.
	 * 
	 * @param dataModel the data-model
	 */
	@Override
	public void render(DataModel dataModel, List<String> restrictionFiles) {
		String oldOutputFolder = outputFolder;
		try {
			new File(outputFolder).mkdirs();
			outputFolder = outputFolderOf(dataModel);
			new File(outputFolder).mkdir();
			String outputFolderContent = new File(outputFolder, CONTENT_FOLDER_NAME).getPath();
			new File(outputFolderContent).mkdir();
			List<Table> tableList = new ArrayList<Table>(dataModel.getTables());
			Collections.sort(tableList);
			List<String> tablesColumn = new ArrayList<String>();
			
			for (Table table: tableList) {
				tablesColumn.add(linkTo(table, table.getName(), CONTENT_FOLDER_NAME + "/", dataModel));
				StringBuffer legend = new StringBuffer();
				String closure = renderClosure(table, legend, dataModel);
				closure = new PrintUtil().applyTemplate("template" + File.separatorChar + "table.html", new Object[] { "Closure", "", closure });
				String columns = generateColumnsTable(table, dataModel);
				if (columns == null) {
					columns = "";
				} else {
					columns += "<br>";
				}
				String components = "";
				String domainSuffix = "";
				String title = "Table " + escapeHtmlEntities(table.getName());
				writeFile(new File(outputFolderContent, toFileName(table)), new PrintUtil().applyTemplate("template" + File.separator + "tableframe.html", new Object[] { title, renderTableBody(table, table, 0, 1, new HashSet<Table>(), dataModel), closure + legend, components + columns, domainSuffix, escapeHtmlEntities(dataModel.getName()), escapeHtmlEntities(dataModel.getComment(table, null)).replace("\n", "<br>") }));
				CancellationHandler.checkForCancellation(null);
			}
			
			String restrictions = "none";
			List<String> restrictionModels = restrictionFiles;
			if (!restrictionModels.isEmpty()) {
				restrictions = "&nbsp;" + new File(restrictionModels.get(0)).getName().replace("-restrictions.jm", ".jm");
			}
			
			String domains = "";

			writeFile(new File(outputFolder, "index.html"), new PrintUtil().applyTemplate("template" + File.separatorChar + "index.html", new Object[] { new Date(), generateHTMLTable("Tables", null, tablesColumn), restrictions, domains, overviewHtml == null? "" : (overviewHtml + " <br> <br>"), escapeHtmlEntities(dataModel.getName()) }));
			writeFile(new File(outputFolderContent, "styles.css"), new PrintUtil().applyTemplate("template" + File.separatorChar + "styles.css", new Object[] { } ));
		} catch (CancellationException e) {
			throw e;
		} catch (Exception e) {
			throw new RuntimeException(e);
		} finally {
			outputFolder = oldOutputFolder;
		}
	}

	public String outputFolderOf(DataModel dataModel) {
		return new File(outputFolder, toFileName(dataModel.getName())).getPath();
	}

	/**
	 * Renders the closure of a table.
	 * 
	 * @param table the table
	 * @param dataModel 
	 * 
	 * @return render of closure
	 */
	private String renderClosure(Table table, StringBuffer legend, DataModel dataModel) throws FileNotFoundException, IOException {
		StringBuffer lines = new StringBuffer();
		int distance = 0;
		Set<Table> closure = new HashSet<Table>();
		Set<Table> associatedTables = new HashSet<Table>();
		boolean printLegend = false;
		do {
			associatedTables.clear();
			if (distance == 0) {
				associatedTables.add(table);
			} else {
				for (Table c: closure) {
					for (Association a: c.associations) {
						if (a.getJoinCondition() != null) {
							Table destinationTable = a.destination;
							if (!closure.contains(destinationTable)) {
								associatedTables.add(destinationTable);
							}
						}
					}
				}
			}
			
			List<Table> cl = new ArrayList<Table>(associatedTables);
			Collections.sort(cl, new Comparator<Table>() {
				@Override
				public int compare(Table o1, Table o2) {
					return o1.compareTo(o2);
				}
			});
			StringBuffer ts = new StringBuffer();
			boolean firstTime = true;
			for (Table dt: cl) {
				if (!firstTime) {
					ts.append(", ");
				}
				ts.append(dt.equals(table)? escapeHtmlEntities(dt.getName()) : linkTo(dt, dataModel));
				firstTime = false;
			}
			if (!cl.isEmpty()) {
				lines.append(new PrintUtil().applyTemplate("template" + File.separator + "table_line.html", new Object[] { "", "&nbsp;&nbsp;distance&nbsp;" + distance, "", "&nbsp;", ts.toString(), COLOR_KEYWORDS, distance % 2 != 0? "class=\"highlightedrow\"" : "", "" }));
			}
			++distance;
			closure.addAll(associatedTables);
		} while (!associatedTables.isEmpty());
		if (printLegend) {
			legend.append(new PrintUtil().applyTemplate("template" + File.separatorChar + "legend.html", new Object[0]));
		}
		return lines.toString();
	}

	/**
	 * Renders a table.
	 * 
	 * @param table the table
	 * @return render of table
	 */
	private String renderTableBody(Table table, Table current, int depth, int indent, Set<Table> alreadyRendered, DataModel dataModel) throws FileNotFoundException, IOException {
		if (alreadyRendered.contains(table)) {
			return "";
		}
		alreadyRendered.add(table);
		
		StringBuffer lines = new StringBuffer();
		List<Association> all = new ArrayList<Association>(table.associations);
		Collections.sort(all, new Comparator<Association>() {
			@Override
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
					lines.append(tableRow(indent, association, current, ++lineNr % 2 == 0, dataModel));
				}
			}
		}
		if (!hasDep.isEmpty()) {
			lines.append(tableRow(indent, prefix + "has&nbsp;dependent"));
			lineNr = 0;
			for (Association association: hasDep) {
				if (!"".equals(association.toString())) {
					prefix = gap;
					lines.append(tableRow(indent, association, current, ++lineNr % 2 == 0, dataModel));
				}
			}
		}
		if (!assoc.isEmpty()) {
			lines.append(tableRow(indent, prefix + "is&nbsp;associated&nbsp;with"));
			lineNr = 0;
			for (Association association: assoc) {
				if (!"".equals(assoc.toString())) {
					prefix = gap;
					lines.append(tableRow(indent, association, current, ++lineNr % 2 == 0, dataModel));
				}
			}
		}
		if (!ignored.isEmpty()) {
			lines.append(tableRow(indent, prefix + "ignored"));
			lineNr = 0;
			for (Association association: ignored) {
				if (!"".equals(association.toString())) {
					prefix = gap;
					lines.append(tableRow(indent, association, current, ++lineNr % 2 == 0, dataModel));
				}
			}
		}

		StringBuffer result = new StringBuffer(new PrintUtil().applyTemplate("template" + File.separator + "table.html", new Object[] { table.equals(current)? "Associations" : linkTo(table, dataModel), indentSpaces(indent), lines.toString() }));
		
		if (depth < maxDepth) {
			if (depth == 0) {
				result.append("<br>"
						+ new PrintUtil().applyTemplate("template" + File.separator + "table.html", new Object[] { "Neighborhood", indentSpaces(1), "" })
						+ "<br>");
			}
			Set<Table> rendered = new HashSet<Table>();
			boolean firstTime = true;
			for (Association association: all) {
				if (!rendered.contains(association.destination)) {
					String tableBody = renderTableBody(association.destination, current, depth + 1, indent + 1, alreadyRendered, dataModel);
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
		return new PrintUtil().applyTemplate("template" + File.separator + "table_top_line.html", new Object[] { indentSpaces(indent), content, "", "", "", COLOR_KEYWORDS, "" });
	}

	/**
	 * Generates a row in the table render.
	 * 
	 * @param indent the indentation
	 * @param highlighted 
	 * @return a row in the table render
	 */
	private String tableRow(int indent, Association association, Table current, boolean highlighted, DataModel dataModel) throws FileNotFoundException, IOException {
		String jc = association.renderJoinCondition("<span style=\"" + COLOR_KEYWORDS + "\">restricted by</span>");
		String aliasA = "A", aliasB = "B";
		if (!association.source.equals(association.destination)) {
			aliasA = association.source.getName();
			aliasB = association.destination.getName();
		}
		aliasA = linkTo(association.source, aliasA, "", dataModel);
		aliasB = linkTo(association.destination, aliasB, "", dataModel);
		jc = SqlUtil.replaceAliases(jc, aliasA, aliasB);
		return new PrintUtil().applyTemplate("template" + File.separator + "table_line.html", new Object[] { indentSpaces(indent), "&nbsp;&nbsp;" + (association.destination.equals(current)? escapeHtmlEntities(association.destination.getName()) : linkTo(association.destination, dataModel)), "&nbsp;&nbsp;" + (association.getCardinality() != null? association.getCardinality() : ""), "&nbsp;on&nbsp;", jc, "", highlighted? "class=\"highlightedrow\"" : "", "" });
	}
	
	/**
	 * Generates a HTML render of a table schema.
	 * 
	 * @param table the table
	 * @return HTML render of table schema
	 */
	private String generateColumnsTable(Table table, DataModel dataModel) throws SQLException, FileNotFoundException, IOException {
		StringBuffer result = new StringBuffer();
		
		int count = 0;
		for (Column column: table.getColumns()) {
			++count;
			String COLUMN_NAME = column.name;
			boolean nullable = true;
			String type = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" + escapeHtmlEntities(column.toSQL(null).substring(column.name.length()).trim()).replaceAll(" ", "&nbsp;");
			String constraint = (!nullable ? "&nbsp;&nbsp;&nbsp;&nbsp;<small>NOT&nbsp;NULL</small>" : "");
			boolean isPK = false;
			for (Column c: table.primaryKey.getColumns()) {
				isPK = isPK || c.name.equalsIgnoreCase(COLUMN_NAME);
			}
			result.append(new PrintUtil().applyTemplate("template" + File.separator + "table_line.html", new Object[] { indentSpaces(1), "&nbsp;&nbsp;" + escapeHtmlEntities(COLUMN_NAME), type, "", constraint, isPK? COLOR_KEYWORDS : "", count % 2 == 0? "class=\"highlightedrow\"" : "", "&nbsp;&nbsp;" + escapeHtmlEntities(dataModel.getComment(table, column)).replace("\n", "<br>&nbsp;&nbsp;") }));
		}
		
		return count == 0? null : (new PrintUtil().applyTemplate("template" + File.separatorChar + "table.html", new Object[] { "Columns", "", result.toString() }));
	}

	/**
	 * Generates a HTML table.
	 */
	private String generateHTMLTable(String title, List<Integer> indents, List<String> column1) throws FileNotFoundException, IOException {
		StringBuffer result = new StringBuffer();
		for (int i = 0; i < column1.size(); ++i) {
			result.append(new PrintUtil().applyTemplate("template" + File.separator + "table_line.html", new Object[] { "", indentSpaces(indents == null? 1 : indents.get(i)) + column1.get(i), "", "", "", "", i % 2 != 0? "class=\"highlightedrow\"" : "", "" }));
		}
		return column1.isEmpty()? null : (new PrintUtil().applyTemplate("template" + File.separatorChar + "table.html", new Object[] { title, "", result.toString() }));
	}

	/**
	 * Returns Space-string of given length.
	 * 
	 * @param indent the length
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
	 * Returns a HTML-hyper link to the render of a given table. 
	 * 
	 * @param table the table
	 * @return HTML-hyper link to the render of table
	 */
	private String linkTo(Table table, DataModel dataModel) {
		return linkTo(table, table.getName(), "", dataModel);
	}

	/**
	 * Returns a HTML-hyper link to the render of a given table. 
	 * 
	 * @param table the table
	 * @param name the name of the link
	 * @return HTML-hyper link to the render of table
	 */
	private String linkTo(Table table, String name, String parent, DataModel dataModel) {
		String title = dataModel.getComment(table, null);
		if (title == null) {
			title = "";
		} else {
			title = " title=\"" + (escapeHtmlEntities(title).replace("\n", "&#10;")) + "\"";
		}
		return "<a href=\"" + parent + toFileName(table) + "\"" + title +">" + escapeHtmlEntities(name) + "</a>";
	}

	/**
	 * Gets name of the file containing the HTML render of a given table.
	 * 
	 * @param tableName the table
	 * 
	 * @return name of the file containing the HTML render of table
	 */
	public static String toFileName(String tableName) {
		StringBuilder sb = new StringBuilder();
		String chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_.";
		
		for (int i = 0; i < tableName.length(); ++i) {
			char c = tableName.charAt(i);
			if (chars.indexOf(c) >= 0) {
				sb.append(c);
			}
		}
		
		return sb.toString();
	}
	
	/**
	 * Gets name of the file containing the HTML render of a given table.
	 * 
	 * @param table the table
	 * @return name of the file containing the HTML render of table
	 */
	public static String toFileName(Table table) {
		return toFileName(table.getName()) + ".html";
	}
	
	/**
	 * Writes content into a file.
	 * 
	 * @param content the content
	 * @param file the file
	 */
	private static void writeFile(File file, String content) throws IOException {
		try {
			file.getParentFile().mkdir();
		} catch (Throwable t) {
			// ignore
		}
		PrintWriter out = new PrintWriter(new FileOutputStream(file));
		out.print(encodeUnencodableChars(content));
		out.close();
		_log.info("file '" + file + "' written");
	}

	public void setOverviewHtml(String overviewHtml) {
		this.overviewHtml  = overviewHtml;
	}

    public static String escapeHtmlEntities(String input){
    	if (input == null) {
    		return "";
    	}
        String result=input.replaceAll("&", "&amp;");
        result=result.replaceAll("\"", "&quot;");
        result=result.replaceAll("<", "&lt;");
        result=result.replaceAll(">", "&gt;");
        return result;
    }
    
	private static String encodeUnencodableChars(String content) {
		try {
			CharsetEncoder encoder = Charset.defaultCharset().newEncoder();
			StringBuilder sb = null;
			int l = content.length();
			for (int i = 0; i < l; ++i) {
				char c = content.charAt(i);
				if (!encoder.canEncode(c)) {
					if (sb == null) {
						sb = new StringBuilder(content.substring(0, i));
					}
					sb.append("&#" + Integer.toString((int) c) + ";");
				} else if (sb != null) {
					sb.append(c);
				}
			}
			if (sb != null) {
				return sb.toString();
			}
		} catch (Exception e) {
			// ignore
		}
		return content;
	}
	
}
