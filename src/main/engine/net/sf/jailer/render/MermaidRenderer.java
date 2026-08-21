/*
 * Copyright 2007 - 2026 Ralf Wisser.
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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

import net.sf.jailer.JailerVersion;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Cardinality;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;

/**
 * Renders a data model as a Mermaid entity relationship diagram
 * (see <a href="https://mermaid.js.org/syntax/entityRelationshipDiagram.html">mermaid.js.org</a>).
 * <br>
 * The result is plain text and can be embedded into Markdown documents, wikis or
 * README files, where it is rendered by the hosting platform. Unlike an image it
 * can be diffed and versioned.
 *
 * @author Ralf Wisser
 */
public class MermaidRenderer {

	/**
	 * Rendering options.
	 */
	public static class Options {

		/**
		 * If <code>true</code>, all columns are rendered. Otherwise only primary key
		 * columns and columns which are part of a join condition (foreign keys).
		 */
		public boolean allColumns = false;

		/**
		 * If <code>true</code>, the display names of the tables are used instead of their names.
		 */
		public boolean useDisplayNames = true;

		/**
		 * If <code>true</code>, disabled (ignored) associations are rendered too.
		 */
		public boolean includeIgnored = false;
	}

	/**
	 * Line separator of the generated text.
	 */
	private static final String LF = "\n";

	/**
	 * Names matching this pattern need no quoting in Mermaid.
	 */
	private static final Pattern SIMPLE_NAME = Pattern.compile("[A-Za-z_][A-Za-z0-9_\\-]*");

	/**
	 * Renders a data model as a Mermaid ER diagram.
	 *
	 * @param dataModel the data model
	 * @param tables the tables to render, or <code>null</code> for all tables of the model
	 * @param options the rendering options, or <code>null</code> for the defaults
	 * @return the diagram as Mermaid source text
	 */
	public static String render(final DataModel dataModel, Collection<Table> tables, Options options) {
		final Options opt = options != null? options : new Options();
		final Set<Table> tableSet = new HashSet<Table>(tables != null? tables : dataModel.getTables());

		List<Table> sortedTables = new ArrayList<Table>(tableSet);
		Collections.sort(sortedTables, new Comparator<Table>() {
			@Override
			public int compare(Table a, Table b) {
				return tableName(dataModel, a, opt).compareTo(tableName(dataModel, b, opt));
			}
		});

		StringBuilder out = new StringBuilder();
		// the diagram type must come first: a leading comment can keep Mermaid from recognizing it
		out.append("erDiagram" + LF);
		out.append("    %% data model " + singleLine(dataModel.getName()) + ", rendered by Jailer " + JailerVersion.VERSION + LF);

		for (Table table: sortedTables) {
			appendEntity(out, dataModel, table, opt);
		}
		for (Association association: relationships(sortedTables, tableSet, opt)) {
			appendRelationship(out, dataModel, association, opt);
		}

		return out.toString();
	}

	/**
	 * Appends the entity block of a single table.
	 */
	private static void appendEntity(StringBuilder out, DataModel dataModel, Table table, Options options) {
		String comment = dataModel.getComment(table, null);
		if (comment != null && !comment.trim().isEmpty()) {
			out.append("    %% " + singleLine(comment) + LF);
		}
		String entity = entityName(tableName(dataModel, table, options));
		List<Column> columns = columnsOf(table, options);
		if (columns.isEmpty()) {
			// an entity without attributes is declared by its name alone
			out.append("    " + entity + LF);
			return;
		}
		Set<Column> pkColumns = primaryKeyColumns(table);
		Set<Column> fkColumns = foreignKeyColumns(table);
		out.append("    " + entity + " {" + LF);
		for (Column column: columns) {
			String key = pkColumns.contains(column)? " PK" : fkColumns.contains(column)? " FK" : "";
			out.append("        " + typeToken(column) + " " + attributeName(column.name) + key + LF);
		}
		out.append("    }" + LF);
	}

	/**
	 * Appends the relationship line of a single association.
	 */
	private static void appendRelationship(StringBuilder out, DataModel dataModel, Association association, Options options) {
		String source = entityName(tableName(dataModel, association.source, options));
		String destination = entityName(tableName(dataModel, association.destination, options));
		out.append("    " + source + " " + relation(association) + " " + destination + " : " + label(association) + LF);
	}

	/**
	 * Collects the associations to be rendered. Each association is rendered once,
	 * although the model holds it in both directions.
	 *
	 * @param sortedTables the tables in rendering order
	 * @param tableSet the tables to be rendered
	 */
	private static Collection<Association> relationships(List<Table> sortedTables, Set<Table> tableSet, Options options) {
		Set<Association> result = new LinkedHashSet<Association>();
		for (Table table: sortedTables) {
			List<Association> associations = new ArrayList<Association>(table.associations);
			Collections.sort(associations, new Comparator<Association>() {
				@Override
				public int compare(Association a, Association b) {
					int c = a.destination.getName().compareTo(b.destination.getName());
					if (c != 0) {
						return c;
					}
					return a.getId() - b.getId();
				}
			});
			for (Association association: associations) {
				if (association.reversed && association.reversalAssociation != null) {
					continue; // rendered at the opposite table
				}
				if (!tableSet.contains(association.source) || !tableSet.contains(association.destination)) {
					continue;
				}
				if (association.isIgnored() && !options.includeIgnored) {
					continue;
				}
				result.add(association);
			}
		}
		return result;
	}

	/**
	 * Gets the columns of a table to be rendered as attributes.
	 */
	private static List<Column> columnsOf(Table table, Options options) {
		List<Column> columns = table.getColumns();
		if (columns == null) {
			return new ArrayList<Column>();
		}
		if (options.allColumns) {
			return new ArrayList<Column>(columns);
		}
		Set<Column> keyColumns = primaryKeyColumns(table);
		keyColumns.addAll(foreignKeyColumns(table));
		List<Column> result = new ArrayList<Column>();
		for (Column column: columns) {
			if (keyColumns.contains(column)) {
				result.add(column);
			}
		}
		return result;
	}

	/**
	 * Gets the primary key columns of a table.
	 */
	private static Set<Column> primaryKeyColumns(Table table) {
		Set<Column> result = new HashSet<Column>();
		if (table.primaryKey != null && table.primaryKey.getColumns() != null) {
			result.addAll(table.primaryKey.getColumns());
		}
		return result;
	}

	/**
	 * Gets the columns of a table which are part of the join condition of an association.
	 */
	private static Set<Column> foreignKeyColumns(Table table) {
		Set<Column> result = new HashSet<Column>();
		for (Association association: table.associations) {
			result.addAll(association.createSourceToDestinationKeyMapping().keySet());
		}
		return result;
	}

	/**
	 * Gets the Mermaid relationship symbol of an association.
	 * A restricted association is rendered as a non-identifying (dashed) relationship.
	 */
	private static String relation(Association association) {
		String line = association.isRestricted()? ".." : "--";
		Cardinality cardinality = association.getCardinality();
		if (Cardinality.ONE_TO_ONE.equals(cardinality)) {
			return "||" + line + "||";
		}
		if (Cardinality.ONE_TO_MANY.equals(cardinality)) {
			return "||" + line + "o{";
		}
		if (Cardinality.MANY_TO_ONE.equals(cardinality)) {
			return "}o" + line + "||";
		}
		return "}o" + line + "o{"; // MANY_TO_MANY or unknown
	}

	/**
	 * Gets the label of a relationship. Mermaid requires a non-empty label.
	 */
	private static String label(Association association) {
		String label = association.getName();
		if (label == null || label.trim().isEmpty()) {
			Cardinality cardinality = association.getCardinality();
			label = cardinality != null? cardinality.toString() : "ref";
		}
		return "\"" + singleLine(label).replace("\"", "'") + "\"";
	}

	/**
	 * Gets the name of a table as it is to be rendered.
	 */
	private static String tableName(DataModel dataModel, Table table, Options options) {
		return options.useDisplayNames? dataModel.getDisplayName(table) : table.getName();
	}

	/**
	 * Quotes a name if Mermaid does not accept it as an entity name.
	 */
	private static String entityName(String name) {
		if (name == null || name.isEmpty()) {
			return "\"?\"";
		}
		if (SIMPLE_NAME.matcher(name).matches()) {
			return name;
		}
		return "\"" + singleLine(name).replace("\"", "'") + "\"";
	}

	/**
	 * Makes an attribute name out of a column name.
	 */
	private static String attributeName(String name) {
		if (name == null || name.isEmpty()) {
			return "unknown";
		}
		String result = name.replaceAll("[^A-Za-z0-9_\\-]", "_");
		if (!Character.isLetter(result.charAt(0)) && result.charAt(0) != '_') {
			result = "_" + result;
		}
		return result;
	}

	/**
	 * Makes an attribute type token out of a column type, including length and precision.
	 * Mermaid does not accept whitespace or brackets here.
	 */
	private static String typeToken(Column column) {
		StringBuilder type = new StringBuilder(column.type != null? column.type : "");
		if (column.length == Integer.MAX_VALUE) {
			type.append("_max");
		} else if (column.length > 0) {
			type.append("_" + column.length);
			if (column.precision >= 0) {
				type.append("_" + column.precision);
			}
		}
		String result = type.toString().replaceAll("[^A-Za-z0-9_]", "_").replaceAll("_+", "_").replaceAll("^_|_$", "");
		return result.isEmpty()? "unknown" : result;
	}

	/**
	 * Reduces a text to a single line. Mermaid is line oriented.
	 */
	private static String singleLine(String text) {
		if (text == null) {
			return "";
		}
		return text.replaceAll("\\s*[\\r\\n]+\\s*", " ").trim();
	}

}
