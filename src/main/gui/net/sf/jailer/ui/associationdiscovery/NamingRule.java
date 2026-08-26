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
package net.sf.jailer.ui.associationdiscovery;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.sf.jailer.util.Quoting;

/**
 * A naming rule entered by the user. It describes how a foreign key column of a child
 * table is named, and thus which associations exist without being declared as a
 * constraint. <br>
 * A rule is either a template with the placeholders <code>${table}</code> and
 * <code>${pk}</code>, or a regular expression with a group for the parent table and,
 * optionally, one for the primary key column.
 *
 * @author Ralf Wisser
 */
public class NamingRule {

	/**
	 * The two ways to express a rule.
	 */
	public enum Kind {

		/**
		 * A name with the placeholders <code>${table}</code> and <code>${pk}</code>,
		 * for instance <code>${table}${pk}</code> or <code>FK_${table}</code>.
		 */
		TEMPLATE("Template"),

		/**
		 * A regular expression on the column name with a group for the parent table,
		 * for instance <code>^FK_(?&lt;table&gt;.+)$</code>.
		 */
		REGEX("Regular expression");

		public final String displayName;

		private Kind(String displayName) {
			this.displayName = displayName;
		}

		/**
		 * Gets the kind with the given name, tolerating the display name as well.
		 *
		 * @param name the name
		 * @return the kind, or <code>null</code>
		 */
		public static Kind parse(String name) {
			if (name != null) {
				for (Kind kind: values()) {
					if (kind.name().equalsIgnoreCase(name.trim()) || kind.displayName.equalsIgnoreCase(name.trim())) {
						return kind;
					}
				}
			}
			return null;
		}
	}

	/**
	 * The placeholder for the name of the parent table.
	 */
	public static final String TABLE_PLACEHOLDER = "${table}";

	/**
	 * The placeholder for the name of the primary key column.
	 */
	public static final String PK_PLACEHOLDER = "${pk}";

	/**
	 * Name of the regular expression group holding the parent table.
	 */
	private static final String TABLE_GROUP = "(?<table>";

	/**
	 * Name of the regular expression group holding the primary key column.
	 */
	private static final String PK_GROUP = "(?<pk>";

	private static final Pattern PLACEHOLDER = Pattern.compile("\\$\\{(table|pk)\\}", Pattern.CASE_INSENSITIVE);

	private Kind kind;
	private String pattern;
	private boolean completeByName;
	private boolean withoutDataCheck;
	private Pattern compiledPattern;

	/**
	 * What a regular expression matched.
	 */
	public static class Match {

		/**
		 * Normalized name of the parent table.
		 */
		public final String table;

		/**
		 * Normalized name of the primary key column, or <code>null</code> if the
		 * expression does not name one.
		 */
		public final String pk;

		public Match(String table, String pk) {
			this.table = table;
			this.pk = pk;
		}
	}

	/**
	 * @param kind the kind of the rule
	 * @param pattern the template or regular expression
	 * @param completeByName if <code>true</code> and this rule matches one column of a
	 *        composite key, the remaining key columns may be matched by identical name
	 */
	public NamingRule(Kind kind, String pattern, boolean completeByName, boolean withoutDataCheck) {
		this.kind = kind;
		this.pattern = pattern;
		this.completeByName = completeByName;
		this.withoutDataCheck = withoutDataCheck;
	}

	public Kind getKind() {
		return kind;
	}

	public String getPattern() {
		return pattern;
	}

	/**
	 * Tells whether a partially matched composite key may be completed by columns of
	 * identical name. Useful for keys that carry a tenant or a version column, which is
	 * named alike in both tables. Without effect for a single column key.
	 *
	 * @return <code>true</code> if the remaining key columns may be matched by name
	 */
	public boolean isCompleteByName() {
		return completeByName;
	}

	/**
	 * Tells whether the matches of this rule are accepted without being checked against the
	 * data. For a convention the user knows to be binding, and for tables that are empty or
	 * too large to check.
	 *
	 * @return <code>true</code> if no verification query is to be run
	 */
	public boolean isWithoutDataCheck() {
		return withoutDataCheck;
	}

	/**
	 * Sets kind, pattern and the options. The compiled expression is discarded.
	 *
	 * @param kind the kind of the rule
	 * @param pattern the template or regular expression
	 * @param completeByName whether the remaining key columns may be matched by name
	 * @param withoutDataCheck whether matches are accepted without checking the data
	 */
	public void set(Kind kind, String pattern, boolean completeByName, boolean withoutDataCheck) {
		this.kind = kind;
		this.pattern = pattern;
		this.completeByName = completeByName;
		this.withoutDataCheck = withoutDataCheck;
		this.compiledPattern = null;
	}

	/**
	 * Checks a rule before it is accepted.
	 *
	 * @param kind the kind of the rule
	 * @param pattern the template or regular expression
	 * @return an error message, or <code>null</code> if the rule is valid
	 */
	public static String validate(Kind kind, String pattern) {
		if (kind == null) {
			return "No kind selected.";
		}
		if (pattern == null || pattern.trim().length() == 0) {
			return "The pattern is empty.";
		}
		if (kind == Kind.TEMPLATE) {
			if (!PLACEHOLDER.matcher(pattern).find() || !containsTablePlaceholder(pattern)) {
				return "A template must contain " + TABLE_PLACEHOLDER + ", the name of the parent table.\n"
						+ "Without it the rule would not depend on the parent table at all, "
						+ "which would make every pair of tables a candidate.";
			}
			return null;
		}
		try {
			Pattern compiled = Pattern.compile(pattern, Pattern.CASE_INSENSITIVE);
			if (!pattern.contains(TABLE_GROUP) && compiled.matcher("").groupCount() < 1) {
				return "A regular expression must have a group holding the name of the parent table:\n"
						+ "either a named group " + TABLE_GROUP + "...) or at least one capturing group.\n"
						+ "Without it the rule would not depend on the parent table at all, "
						+ "which would make every pair of tables a candidate.";
			}
		} catch (Throwable t) {
			return "Not a valid regular expression: " + t.getMessage();
		}
		return null;
	}

	/**
	 * Checks whether the given template contains the table placeholder, ignoring case.
	 *
	 * @param pattern the template
	 * @return <code>true</code> if the placeholder is present
	 */
	private static boolean containsTablePlaceholder(String pattern) {
		Matcher matcher = PLACEHOLDER.matcher(pattern);
		while (matcher.find()) {
			if ("table".equalsIgnoreCase(matcher.group(1))) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Expands a template into the normalized column name it accepts.
	 *
	 * @param tableName name of the parent table (or one of its name variants)
	 * @param pkName name of the primary key column
	 * @return the normalized column name, or <code>null</code> if this is not a template
	 */
	public String expand(String tableName, String pkName) {
		if (kind != Kind.TEMPLATE) {
			return null;
		}
		Matcher matcher = PLACEHOLDER.matcher(pattern);
		StringBuffer result = new StringBuffer();
		while (matcher.find()) {
			String value = "table".equalsIgnoreCase(matcher.group(1))? tableName : pkName;
			matcher.appendReplacement(result, Matcher.quoteReplacement(value == null? "" : value));
		}
		matcher.appendTail(result);
		return Quoting.normalizeIdentifier(result.toString());
	}

	/**
	 * Applies a regular expression to a column name.
	 *
	 * @param columnName the normalized name of the column
	 * @return what the expression matched, or <code>null</code> if it does not match or
	 *         this is not a regular expression
	 */
	public Match match(String columnName) {
		if (kind != Kind.REGEX) {
			return null;
		}
		if (compiledPattern == null) {
			try {
				compiledPattern = Pattern.compile(pattern, Pattern.CASE_INSENSITIVE);
			} catch (Throwable t) {
				return null;
			}
		}
		Matcher matcher = compiledPattern.matcher(columnName);
		if (!matcher.matches()) {
			return null;
		}
		String table;
		String pk = null;
		try {
			if (pattern.contains(TABLE_GROUP)) {
				table = matcher.group("table");
				if (pattern.contains(PK_GROUP)) {
					pk = matcher.group("pk");
				}
			} else {
				table = matcher.group(1);
				if (matcher.groupCount() >= 2) {
					pk = matcher.group(2);
				}
			}
		} catch (Throwable t) { // malformed group reference
			return null;
		}
		if (table == null) {
			return null;
		}
		return new Match(Quoting.normalizeIdentifier(table), pk == null? null : Quoting.normalizeIdentifier(pk));
	}

	@Override
	public String toString() {
		return kind.displayName + ": " + pattern;
	}

}
