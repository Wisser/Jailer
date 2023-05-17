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

package net.sf.jailer.datamodel;

import java.util.Locale;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.sf.jailer.util.SqlUtil;

/**
 * Column of a table.
 *
 * @author Ralf Wisser
 */
public class Column {

	/**
	 * The name.
	 */
	public final String name;

	/**
	 * The type.
	 */
	public final String type;

	/**
	 * The length (for VARCHAR, DECIMAL, ...) or <code>0</code> if type-length is not variable.
	 */
	public final int length;

	/**
	 * The precision (for DECIMAL, NUMBER ...) or <code>-1</code> if precision is not variable.
	 */
	public final int precision;

	/**
	 * <code>true</code> if column is identity column.
	 */
	public boolean isIdentityColumn = false;

	/**
	 * <code>true</code> if column is virtual.
	 */
	public boolean isVirtual = false;

	/**
	 * <code>true</code> if column is nullable.
	 */
	public boolean isNullable = false;

	/**
	 * SQL Expression for server-side column data filtering.
	 */
	private Filter filter = null;

	/**
	 * Constructor.
	 *
	 * @param name the name (upper-case)
	 * @param type the type (in SQL)
	 * @param length the length (for VARCHAR, DECIMAL, ...) or <code>0</code> if type-length is not variable
	 * @param precision the precision (for DECIMAL, NUMBER ...) or <code>-1</code> if precision is not variable
	 */
	public Column(String name, String type, int length, int precision) {
		this.name = name;
		this.type = type;
		this.length = length;
		this.precision = precision;
	}

	/**
	 * Gets SQL expression for server-side column data filtering.
	 * {@link Filter#OLD_VALUE_PROP} is replaced by column name.
	 *
	 * @return SQL expression for server-side column data filtering
	 *         or <code>null</code>, if no filter is defined for this column
	 */
	public String getFilterExpression() {
		if (filter == null) {
			return null;
		}
		String expr = filter.getExpression().replaceAll(Filter.OLD_VALUE_PROP_RE, Matcher.quoteReplacement("T." + name)).trim();
		if (expr.startsWith(Filter.LITERAL_PREFIX)) {
			expr = expr.substring(Filter.LITERAL_PREFIX.length()).trim();
		}
		return expr;
	}

	/**
	 * Gets filter for server-side column data filtering.
	 *
	 * @return filter for server-side column data filtering
	 *        or <code>null</code>, if no filter is defined for this column
	 */
	public Filter getFilter() {
		return filter;
	}

	/**
	 * Sets filter for server-side column data filtering.
	 *
	 * @param filter SQL expression for server-side column data filtering
	 *        or <code>null</code>, if no filter is defined for this column
	 */
	public void setFilter(Filter filter) {
		this.filter = filter;
	}


	private static Pattern typeWithSizeAndPrecision = Pattern.compile("([^ ]+) +([^ \\(]+) *\\( *([0-9]+) *, *([0-9]+) *\\)");
	private static Pattern typeWithSize = Pattern.compile("([^ ]+) +([^ \\(]+) *\\( *([0-9]+|max) *\\)");
	private static Pattern typeWithoutSize = Pattern.compile("([^ ]+) +([^ \\(]+)");

	/**
	 * Parses a column declaration in SQL syntax.
	 *
	 * @param columnDeclaration the column declaration in SQL syntax
	 * @return the column
	 */
	public static Column parse(String columnDeclaration) {
		return parse(null, columnDeclaration);
	}

	/**
	 * Parses a column declaration in SQL syntax.
	 *
	 * @param theColumnDeclaration the column declaration in SQL syntax
	 * @param columnName (optional) name of column
	 * @return the column
	 */
	public static Column parse(String columnName, String theColumnDeclaration) {
		String columnDeclaration = theColumnDeclaration.trim();

		// work-around for bug 2849047
		String normalizedcolumnDeclaration = PATTERN_IDENTITY.matcher(columnDeclaration).replaceFirst("");
		if (!normalizedcolumnDeclaration.equals(columnDeclaration)) {
			columnDeclaration = normalizedcolumnDeclaration + " identity";
		}
		normalizedcolumnDeclaration = PATTERN_VIRTUAL.matcher(columnDeclaration).replaceFirst("");
		if (!normalizedcolumnDeclaration.equals(columnDeclaration)) {
			columnDeclaration = normalizedcolumnDeclaration + " virtual";
		}
		normalizedcolumnDeclaration = PATTERN_NULL.matcher(columnDeclaration).replaceFirst("");
		if (!normalizedcolumnDeclaration.equals(columnDeclaration)) {
			columnDeclaration = normalizedcolumnDeclaration + " null";
		}

		boolean isNullable = false;
		if (columnDeclaration.toLowerCase(Locale.ENGLISH).endsWith(" null")) {
			isNullable = true;
			columnDeclaration = columnDeclaration.substring(0, columnDeclaration.length() - 5).trim();
		}

		boolean isVirtual = false;
		if (columnDeclaration.toLowerCase(Locale.ENGLISH).endsWith(" virtual")) {
			isVirtual = true;
			columnDeclaration = columnDeclaration.substring(0, columnDeclaration.length() - 8).trim();
		}

		boolean isIdent = false;
		if (columnDeclaration.toLowerCase(Locale.ENGLISH).endsWith(" identity")) {
			isIdent = true;
			columnDeclaration = columnDeclaration.substring(0, columnDeclaration.length() - 9).trim();
		}

		Character quote = null;
		if (columnDeclaration.length() > 0 && !SqlUtil.isLetterOrDigit(columnDeclaration.charAt(0))) {
			quote = columnDeclaration.charAt(0);
			if (columnDeclaration.substring(1).indexOf(quote) >= 0) {
				StringBuilder sb = new StringBuilder();
				sb.append(quote);
				boolean inScope = true;
				for (int i = 1; i < columnDeclaration.length(); ++i) {
					char c = columnDeclaration.charAt(i);
					if (c == quote) {
						inScope = !inScope;
					}
					if (inScope && c == ' ') {
						c = '\f';
					}
					sb.append(c);
				}
				columnDeclaration = sb.toString();
			}
		}

		String name, type;
		int size = 0;
		int precision = -1;
		Matcher matcher = typeWithSizeAndPrecision.matcher(columnDeclaration);
		if (matcher.matches()) {
			name = matcher.group(1);
			type = matcher.group(2);
			size = Integer.parseInt(matcher.group(3));
			precision = Integer.parseInt(matcher.group(4));
		} else {
			matcher = typeWithSize.matcher(columnDeclaration);
			if (matcher.matches()) {
				name = matcher.group(1);
				type = matcher.group(2);
				size = "max".equalsIgnoreCase(matcher.group(3))? Integer.MAX_VALUE : Integer.parseInt(matcher.group(3));
			} else {
				matcher = typeWithoutSize.matcher(columnDeclaration);
				if (matcher.matches()) {
					name = matcher.group(1);
					type = matcher.group(2);
				} else {
					throw new RuntimeException("invalid column declaration: " + theColumnDeclaration);
				}
			}
		}
		if (quote != null) {
			name = name.replace('\f', ' ');
		}
		Column column = new Column(columnName == null? name : columnName, type, size, precision);
		column.isNullable = isNullable;
		column.isIdentityColumn = isIdent;
		column.isVirtual = isVirtual;
		return column;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Column other = (Column) obj;
		if (length != other.length)
			return false;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		if (type == null) {
			if (other.type != null)
				return false;
		} else if (!type.equals(other.type))
			return false;
		return true;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + length;
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		result = prime * result + ((type == null) ? 0 : type.hashCode());
		return result;
	}

	/**
	 * Returns the primary key in SQL syntax.
	 *
	 * @param columnPrefix an optional prefix for each PK-column
	 * @param typeReplacement column types replacements
	 */
	public String toSQL(String columnPrefix, Map<String, String> typeReplacement) {
		String theType = type;
		if (typeReplacement != null && typeReplacement.containsKey(theType)) {
			theType = typeReplacement.get(theType);
		}
		return (columnPrefix == null? "": columnPrefix) + name + " " + theType + (length <= 0? "" : length == Integer.MAX_VALUE? "(max)" :
			"(" + length + (precision >= 0? ", " + precision : "") + ")");
	}

	/**
	 * Returns the column definition in SQL syntax.
	 *
	 * @param columnPrefix an optional prefix for the column name
	 */
	public String toSQL(String columnPrefix) {
		return toSQL(columnPrefix, null);
	}

	/**
	 * Returns a string representation of the column.
	 */
	@Override
	public String toString() {
		return toSQL(null);
	}

	/**
	 * Returns <code>true</code> iff this column cannot be updated.
	 *
	 * @return <code>true</code> iff this column cannot be updated
	 */
	public boolean isVirtual() {
		return isVirtual;
	}

	private static final Pattern PATTERN_VIRTUAL = Pattern.compile("(\\(\\))? +[vV][iI][rR][tT][uU][aA][lL]");
	private static final Pattern PATTERN_IDENTITY = Pattern.compile("(\\(\\))? +[iI][dD][eE][nN][tT][iI][tT][yY]");
	private static final Pattern PATTERN_NULL = Pattern.compile("(\\(\\))? +[nN][uU][lL][lL]");

}
