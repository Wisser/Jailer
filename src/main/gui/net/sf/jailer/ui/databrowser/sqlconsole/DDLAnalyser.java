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
package net.sf.jailer.ui.databrowser.sqlconsole;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.sf.jailer.ui.syntaxtextarea.SQLCompletionProvider;

/**
 * Best-effort, regex-based recognition of the single table/view/index a DDL
 * statement affects. This is deliberately not a real SQL parser: whenever the
 * affected object can't be determined with confidence, {@link #analyse(String)}
 * returns {@link Kind#UNRECOGNIZED}, signalling the caller to fall back to a
 * full metadata refresh instead of guessing.
 *
 * @author Ralf Wisser
 */
public class DDLAnalyser {

	/**
	 * The kind of change a DDL statement was recognized to perform.
	 */
	public enum Kind {
		CREATE_TABLE, CREATE_VIEW, ALTER_TABLE, DROP_TABLE, DROP_VIEW, RENAME_TABLE,
		CREATE_INDEX, DROP_INDEX, UNRECOGNIZED
	}

	/**
	 * A single recognized (or unrecognized) DDL change.
	 */
	public static class DDLChange {
		public final Kind kind;
		public final String schemaName;
		public final String tableName;
		public final String newTableName;

		DDLChange(Kind kind, String schemaName, String tableName, String newTableName) {
			this.kind = kind;
			this.schemaName = schemaName;
			this.tableName = tableName;
			this.newTableName = newTableName;
		}

		public static final DDLChange UNRECOGNIZED = new DDLChange(Kind.UNRECOGNIZED, null, null, null);
	}

	private static final String ID = "(?:[\"][^\"]+[\"])|(?:[`][^`]+[`])|(?:['][^']+['])|(?:[\\w]+)";
	private static final String QUALIFIED_NAME = "(?:(" + ID + ")\\s*\\.\\s*)?(" + ID + ")";

	private static final Pattern CREATE_TABLE = Pattern.compile(
			"^\\s*create\\s+(?:or\\s+replace\\s+)?(?:global\\s+temporary\\s+|temp(?:orary)?\\s+|memory\\s+|cached\\s+)?table\\s+(?:if\\s+not\\s+exists\\s+)?" + QUALIFIED_NAME,
			Pattern.CASE_INSENSITIVE | Pattern.DOTALL);

	private static final Pattern CREATE_VIEW = Pattern.compile(
			"^\\s*create\\s+(?:or\\s+replace\\s+)?(?:materialized\\s+)?view\\s+(?:if\\s+not\\s+exists\\s+)?" + QUALIFIED_NAME,
			Pattern.CASE_INSENSITIVE | Pattern.DOTALL);

	private static final Pattern ALTER_TABLE = Pattern.compile(
			"^\\s*alter\\s+table\\s+(?:if\\s+exists\\s+)?" + QUALIFIED_NAME,
			Pattern.CASE_INSENSITIVE | Pattern.DOTALL);

	// negative lookahead for a trailing comma rejects comma-separated multi-table forms
	// (e.g. "DROP TABLE a, b") while still tolerating trailing modifiers like CASCADE/RESTRICT.
	private static final String NOT_FOLLOWED_BY_COMMA = "(?!\\s*,)";

	private static final Pattern DROP_TABLE = Pattern.compile(
			"^\\s*drop\\s+table\\s+(?:if\\s+exists\\s+)?" + QUALIFIED_NAME + NOT_FOLLOWED_BY_COMMA,
			Pattern.CASE_INSENSITIVE | Pattern.DOTALL);

	private static final Pattern DROP_VIEW = Pattern.compile(
			"^\\s*drop\\s+view\\s+(?:if\\s+exists\\s+)?" + QUALIFIED_NAME + NOT_FOLLOWED_BY_COMMA,
			Pattern.CASE_INSENSITIVE | Pattern.DOTALL);

	private static final Pattern RENAME_TABLE_STMT = Pattern.compile(
			"^\\s*rename\\s+table\\s+" + QUALIFIED_NAME + "\\s+to\\s+" + QUALIFIED_NAME + NOT_FOLLOWED_BY_COMMA,
			Pattern.CASE_INSENSITIVE | Pattern.DOTALL);

	private static final Pattern ALTER_TABLE_RENAME = Pattern.compile(
			"^\\s*alter\\s+table\\s+(?:if\\s+exists\\s+)?" + QUALIFIED_NAME + "\\s+rename\\s+to\\s+(?:(" + ID + ")\\s*\\.\\s*)?(" + ID + ")" + NOT_FOLLOWED_BY_COMMA,
			Pattern.CASE_INSENSITIVE | Pattern.DOTALL);

	private static final Pattern CREATE_INDEX = Pattern.compile(
			"^\\s*create\\s+(?:unique\\s+)?index\\s+(?:if\\s+not\\s+exists\\s+)?(?:" + ID + ")\\s+on\\s+" + QUALIFIED_NAME,
			Pattern.CASE_INSENSITIVE | Pattern.DOTALL);

	private static final Pattern DROP_INDEX_ON = Pattern.compile(
			"^\\s*drop\\s+index\\s+(?:if\\s+exists\\s+)?(?:" + ID + ")\\s+on\\s+" + QUALIFIED_NAME,
			Pattern.CASE_INSENSITIVE | Pattern.DOTALL);

	/**
	 * Analyses a single SQL statement and determines the table/view/index it affects, if any.
	 *
	 * @param sql the statement, as typed by the user (may still contain comments/literals)
	 * @return the recognized change, or {@link DDLChange#UNRECOGNIZED} if it can't be determined with confidence
	 */
	public static DDLChange analyse(String sql) {
		String stmt = SQLCompletionProvider.removeCommentsAndLiterals(sql);

		Matcher m;

		if ((m = ALTER_TABLE_RENAME.matcher(stmt)).find()) {
			return new DDLChange(Kind.RENAME_TABLE, m.group(1), m.group(2), m.group(4));
		}
		if ((m = RENAME_TABLE_STMT.matcher(stmt)).find()) {
			return new DDLChange(Kind.RENAME_TABLE, m.group(1), m.group(2), m.group(4));
		}
		if ((m = CREATE_TABLE.matcher(stmt)).find()) {
			return new DDLChange(Kind.CREATE_TABLE, m.group(1), m.group(2), null);
		}
		if ((m = CREATE_VIEW.matcher(stmt)).find()) {
			return new DDLChange(Kind.CREATE_VIEW, m.group(1), m.group(2), null);
		}
		if ((m = DROP_TABLE.matcher(stmt)).find()) {
			return new DDLChange(Kind.DROP_TABLE, m.group(1), m.group(2), null);
		}
		if ((m = DROP_VIEW.matcher(stmt)).find()) {
			return new DDLChange(Kind.DROP_VIEW, m.group(1), m.group(2), null);
		}
		if ((m = ALTER_TABLE.matcher(stmt)).find()) {
			return new DDLChange(Kind.ALTER_TABLE, m.group(1), m.group(2), null);
		}
		if ((m = CREATE_INDEX.matcher(stmt)).find()) {
			return new DDLChange(Kind.CREATE_INDEX, m.group(1), m.group(2), null);
		}
		if ((m = DROP_INDEX_ON.matcher(stmt)).find()) {
			return new DDLChange(Kind.DROP_INDEX, m.group(1), m.group(2), null);
		}

		return DDLChange.UNRECOGNIZED;
	}

	private DDLAnalyser() {
	}

}
