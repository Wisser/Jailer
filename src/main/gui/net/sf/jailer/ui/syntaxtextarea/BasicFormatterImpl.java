/*
 * Hibernate, Relational Persistence for Idiomatic Java
 *
 * Copyright (c) 2008, Red Hat Middleware LLC or third-party contributors as
 * indicated by the @author tags or express copyright attribution
 * statements applied by the authors.  All third-party contributions are
 * distributed under license by Red Hat Middleware LLC.
 *
 * This copyrighted material is made available to anyone wishing to use, modify,
 * copy, or redistribute it subject to the terms and conditions of the GNU
 * Lesser General Public License, as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License
 * for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this distribution; if not, write to:
 * Free Software Foundation, Inc.
 * 51 Franklin Street, Fifth Floor
 * Boston, MA  02110-1301  USA
 *
 */
package net.sf.jailer.ui.syntaxtextarea;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.github.vertical_blank.sqlformatter.SqlFormatter;
import com.github.vertical_blank.sqlformatter.core.FormatConfig;

/**
 * Performs formatting of basic SQL statements (DML + query).
 *
 * @author Gavin King
 * @author Steve Ebersole
 */
public class BasicFormatterImpl {

	private static final Set BEGIN_CLAUSES = new HashSet();
	private static final Set END_CLAUSES = new HashSet();
	private static final Set LOGICAL = new HashSet();
	private static final Set QUANTIFIERS = new HashSet();
	private static final Set DML = new HashSet();
	private static final Set MISC = new HashSet();

	static {
		BEGIN_CLAUSES.add( "left" );
		BEGIN_CLAUSES.add( "right" );
		BEGIN_CLAUSES.add( "inner" );
		BEGIN_CLAUSES.add( "outer" );
		BEGIN_CLAUSES.add( "cross" );
		BEGIN_CLAUSES.add( "group" );
		BEGIN_CLAUSES.add( "order" );

		END_CLAUSES.add( "where" );
		END_CLAUSES.add( "set" );
		END_CLAUSES.add( "having" );
		END_CLAUSES.add( "join" );
		END_CLAUSES.add( "from" );
		END_CLAUSES.add( "by" );
		END_CLAUSES.add( "join" );
		END_CLAUSES.add( "into" );
		END_CLAUSES.add( "union" );

		LOGICAL.add( "and" );
		LOGICAL.add( "or" );
		LOGICAL.add( "when" );
		LOGICAL.add( "else" );
		LOGICAL.add( "end" );

		QUANTIFIERS.add( "in" );
		QUANTIFIERS.add( "all" );
		QUANTIFIERS.add( "exists" );
		QUANTIFIERS.add( "some" );
		QUANTIFIERS.add( "any" );

		DML.add( "insert" );
		DML.add( "update" );
		DML.add( "delete" );

		MISC.add( "select" );
		MISC.add( "on" );
	}

	static final String indentString = "     ";
	static final String initial = "\n     ";
	
	public String format(String sql) {
		if (sql.length() > 100_000) {
			return sql; // too big
		}
		String marker;
		for (int i = 0; ; ++i) {
			marker = "prop" + i + "porp";
			if (!sql.contains(marker)) {
				break;
			}
		}
		
		Pattern pattern = Pattern.compile(
						Pattern.quote("@>") + "|" +
						Pattern.quote("<@") + "|" +
						Pattern.quote("&&") + "|" +
						Pattern.quote(">>") + "|" +
						Pattern.quote("<<") + "|" +
						Pattern.quote("&<") + "|" +
						Pattern.quote("&>") + "|" +
						Pattern.quote("-|-"), Pattern.DOTALL);
		Matcher matcher = pattern.matcher(sql);
		int i = 0;
		Map<Integer, String> op = new HashMap<Integer, String>();
		boolean result = matcher.find();
		StringBuffer sb = new StringBuffer();
		if (result) {
			do {
				++i;
				op.put(i, matcher.group(0));
				matcher.appendReplacement(sb, marker + i + "o");
				result = matcher.find();
			} while (result);
		}
		matcher.appendTail(sb);
		sql = sb.toString();
		
		pattern = Pattern.compile("\\$\\{([^\\}]+)\\}", Pattern.DOTALL);
		matcher = pattern.matcher(sql);
		i = 0;
		Map<Integer, String> prop = new HashMap<Integer, String>();
		result = matcher.find();
		sb = new StringBuffer();
		if (result) {
			do {
				++i;
				prop.put(i, matcher.group(1));
				matcher.appendReplacement(sb, marker + i + "z");
				result = matcher.find();
			} while (result);
		}
		matcher.appendTail(sb);
		sql = sb.toString().replace("&", marker + "r");
		sql = sql.replaceAll("\\$(\\w+)\\b", marker + "d$1-");
		sql = sql.replaceAll("\\b(\\w')", marker + "p$1"); // N'nvarchar'
		sql = format1(sql);
		sql = sql.replaceAll(marker + "p(\\w)\\s*'", "$1'"); // N'nvarchar'
		sql = sql.replaceAll(marker + "d([^\\-\\s]+)\\s*\\-", Matcher.quoteReplacement("$") + "$1");
		
		for (Entry<Integer, String> e: prop.entrySet()) {
			sql = sql.replace(marker + e.getKey() + "z", "${" + e.getValue() + "}");
		}
		for (Entry<Integer, String> e: op.entrySet()) {
			sql = sql.replace(marker + e.getKey() + "o", e.getValue());
		}
		return sql.replace(marker + "r", "&");
	}

	private String format1(String sql) {
		try {
			String source = sql;
			if (source.contains("\f")) {
				String marker;
				String markerRE;
				for (int i = 0; ; ++i) {
					marker = "(alpha" + i + "omega)";
					markerRE = "\\(\\s*alpha" + i + "omega\\s*\\)";
					if (!source.contains(marker)) {
						break;
					}
				}
				source = source.replace("\f", marker);
				source = SqlFormatter.format(source, FormatConfig.builder().indent(indentString).build()).trim();
				return source.replaceAll(markerRE, "\f");
			}
			return SqlFormatter.format(source, FormatConfig.builder().indent(indentString).build()).trim();
		} catch (Exception e) {
			return format0(sql)
				.replaceAll("(?is)\\)\\s+or\\s+\\(", ") or (")
				.replaceAll("(?is)\\b(insert)\\n\\s*(into)\\b", "$1 $2")
				.replaceAll("(?is)\\b(delete)\\n\\s*(from)\\b", "$1 $2")
				.replaceAll("(?is)\\b(select)\\n\\s+(\\*)", "$1 $2")
				.replaceAll("(?is)\\b(select)(\\n\\s+) (distinct)\\b", "$1 $3$2");
		}
	}

	private String format0(String source) {
		try {
			if (source.trim().startsWith("(")) {
				String formatted = new FormatProcess("where \n" + source).perform();
				String result = formatted.replaceFirst("^\\s*where *\\n\\s*", "");
				if (!result.equals(formatted)) {
					String[] lines = result.split("\\r?\\n");
					int pl = Integer.MAX_VALUE;
					for (String line: lines) {
						for (int i = 0; i < line.length(); ++i) {
							if (line.charAt(i) != ' ') {
								if (pl > i) {
									pl = i;
									break;
								}
							}
						}
						if (pl == 0) {
							break;
						}
					}
					if (pl > 0) {
						StringBuilder sb = new StringBuilder();
						for (String line: lines) {
							if (sb.length() > 0) {
								sb.append('\n');
							}
							sb.append(pl < line.length()? line.substring(pl) : line);
						}
						return sb.toString();
					}
					return result;
				}
			}
			return new FormatProcess(source).perform();
		} catch (Throwable t) {
			return source;
		}
	}

	private static class FormatProcess {
		boolean beginLine = true;
		boolean afterBeginBeforeEnd = false;
		boolean afterByOrSetOrFromOrSelect = false;
		boolean afterValues = false;
		boolean afterOn = false;
		boolean afterBetween = false;
		boolean afterInsert = false;
		int inFunction = 0;
		int parensSinceSelect = 0;
		private LinkedList parenCounts = new LinkedList();
		private LinkedList afterByOrFromOrSelects = new LinkedList();
		
		int indent = 0;

		StringBuffer result = new StringBuffer();
		StringTokenizer tokens;
		String lastToken;
		String token;
		String lcToken;
		
		private final String FUNCTION_SUFFIX = "_DUI_FUNCTION_SUFFIX_1029039456";

		public FormatProcess(String sql) {
			tokens = new StringTokenizer(
					sql.replaceAll("(?i)(\\b(?:insert|update|delete)\\b)(\\s*\\()", "$1" + FUNCTION_SUFFIX + "$2"),
					"()+*/-=<>'`\"[], \n\r\f\t",
					true
			);
		}

		public String perform() {

			result.append( initial );

			String nextToken = null;
			boolean lastWasWS = false;
			while ( tokens.hasMoreTokens() ) {
				if (nextToken == null) {
					token = tokens.nextToken();
					if ("\f".equals(token)) {
						out();
						continue;
					}
				} else {
					token = nextToken;
				}
				nextToken = null;
				lcToken = token.toLowerCase(Locale.ENGLISH);

				if ("-".equals(token) && tokens.hasMoreTokens()) {
					nextToken = tokens.nextToken();
					if ("-".equals(nextToken)) {
						token += nextToken;
						nextToken = null;
						String t;
						if (tokens.hasMoreTokens())
							do {
								t = tokens.nextToken();
								token += t;
							}
							while ( !"\n".equals( t ) && tokens.hasMoreTokens() ); // cannot handle single quotes
					}
				} else if ("/".equals(token) && tokens.hasMoreTokens()) {
					nextToken = tokens.nextToken();
					if ("*".equals(nextToken)) {
						token += nextToken;
						nextToken = null;
						String t;
						do {
							t = tokens.nextToken();
							token += t;
						}
						while ( (!token.endsWith("*/") || !"/".equals( t )) && tokens.hasMoreTokens() ); // cannot handle single quotes
					}
				} else if ( "'".equals( token ) ) {
					String t;
					do {
						t = tokens.nextToken();
						token += t;
					}
					while ( !"'".equals( t ) && tokens.hasMoreTokens() ); // cannot handle single quotes
				}
				else if ( "\"".equals( token ) ) {
					String t;
					do {
						t = tokens.nextToken();
						token += t;
					}
					while ( !"\"".equals( t ) );
				}

				if ( afterByOrSetOrFromOrSelect && ",".equals( token ) ) {
					commaAfterByOrFromOrSelect();
				}
				else if ( afterOn && ",".equals( token ) ) {
					commaAfterOn();
				}

				else if ( "(".equals( token ) ) {
					openParen();
				}
				else if ( ")".equals( token ) ) {
					closeParen();
				}

				else if ( BEGIN_CLAUSES.contains( lcToken ) ) {
					beginNewClause();
				}

				else if ( END_CLAUSES.contains( lcToken ) ) {
					endNewClause();
				}

				else if ( "select".equals( lcToken ) ) {
					select();
				}

				else if ( DML.contains( lcToken ) ) {
					updateOrInsertOrDelete();
				}

				else if ( "values".equals( lcToken ) ) {
					values();
				}

				else if ( "on".equals( lcToken ) ) {
					on();
				}

				else if ( afterBetween && lcToken.equals( "and" ) ) {
					misc();
					afterBetween = false;
				}

				else if ( LOGICAL.contains( lcToken ) ) {
					logical();
				}

				else if ( isWhitespace( token ) ) {
					white();
				}

				else if (" ".equals(token)) {
					if (result.length() == 0 || !" ".equals(result.substring(result.length() - 1, result.length()))) {
						misc();
					}
				}
				
				else {
					misc();
				}
				
				if (" ".equals(token)) {
					lastWasWS = true;
				} else {
					lastWasWS = false;
				}
				
				if ( !isWhitespace( token ) ) {
					lastToken = lcToken;
				}

			}
			CharSequence FREP = "(\f)";
			return result.toString()
					.replaceAll("^\\s*\\f", "\f")
					.replaceAll("\\f\\s*$", "\f")
					.replace("\f", FREP)
					.trim()
					.replaceAll("\\s*\\n", "\n")
					.replace(FREP, "\f")
					.replace(FUNCTION_SUFFIX, "");
		}

		private void commaAfterOn() {
			out();
			indent--;
			newline();
			afterOn = false;
			afterByOrSetOrFromOrSelect = true;
		}

		private void commaAfterByOrFromOrSelect() {
			out();
			newline();
		}

		private void logical() {
			if ( "end".equals( lcToken ) ) {
				indent--;
			}
			newline();
			out();
			beginLine = false;
		}

		private void on() {
			indent++;
			afterOn = true;
			newline();
			out();
			beginLine = false;
		}

		private void misc() {
			out();
			if ( "between".equals( lcToken ) ) {
				afterBetween = true;
			}
			if ( afterInsert ) {
				newline();
				afterInsert = false;
			}
			else {
				beginLine = false;
				if ( "case".equals( lcToken ) ) {
					indent++;
				}
			}
		}

		private void white() {
			if ( !beginLine ) {
				result.append( " " );
			}
		}

		private void updateOrInsertOrDelete() {
			out();
			indent++;
			beginLine = false;
			if ( "update".equals( lcToken ) ) {
				newline();
			}
			if ( "insert".equals( lcToken ) ) {
				afterInsert = true;
			}
		}

		private void select() {
			out();
			indent++;
			newline();
			parenCounts.addLast( parensSinceSelect );
			afterByOrFromOrSelects.addLast( Boolean.valueOf( afterByOrSetOrFromOrSelect ) );
			parensSinceSelect = 0;
			afterByOrSetOrFromOrSelect = true;
		}

		private void out() {
			result.append( token );
		}

		private void endNewClause() {
			if ( !afterBeginBeforeEnd ) {
				indent--;
				if ( afterOn ) {
					indent--;
					afterOn = false;
				}
				newline();
			}
			out();
			if ( !"union".equals( lcToken ) ) {
				indent++;
			}
			newline();
			afterBeginBeforeEnd = false;
			afterByOrSetOrFromOrSelect = "by".equals( lcToken )
					|| "set".equals( lcToken )
					|| "from".equals( lcToken );
		}

		private void beginNewClause() {
			if ( !afterBeginBeforeEnd ) {
				if ( afterOn ) {
					indent--;
					afterOn = false;
				}
				indent--;
				newline();
			}
			out();
			beginLine = false;
			afterBeginBeforeEnd = true;
		}

		private void values() {
			indent--;
			newline();
			out();
			indent++;
			newline();
			afterValues = true;
		}

		private void closeParen() {
			parensSinceSelect--;
			if ( parensSinceSelect < 0 ) {
				indent--;
				parensSinceSelect = ( ( Integer ) parenCounts.removeLast() ).intValue();
				afterByOrSetOrFromOrSelect = ( ( Boolean ) afterByOrFromOrSelects.removeLast() ).booleanValue();
			}
			if ( inFunction > 0 ) {
				inFunction--;
				out();
			}
			else {
				if ( !afterByOrSetOrFromOrSelect ) {
					indent--;
					newline();
				}
				out();
			}
			beginLine = false;
		}

		private void openParen() {
			if ( isFunctionName( lastToken ) || inFunction > 0 ) {
				inFunction++;
			}
			beginLine = false;
			if ( inFunction > 0 ) {
				out();
			}
			else {
				out();
				if ( !afterByOrSetOrFromOrSelect ) {
					indent++;
					newline();
					beginLine = true;
				}
			}
			parensSinceSelect++;
		}

		private static boolean isFunctionName(String tok) {
			final char begin = tok.charAt( 0 );
			final boolean isIdentifier = Character.isJavaIdentifierStart( begin ) || '"' == begin;
			return isIdentifier &&
					!LOGICAL.contains( tok ) &&
					!END_CLAUSES.contains( tok ) &&
					!QUANTIFIERS.contains( tok ) &&
					!DML.contains( tok ) &&
					!MISC.contains( tok );
		}

		private static boolean isWhitespace(String token) {
			return  "\n\r\f\t".indexOf( token ) >= 0;
		}

		private void newline() {
			result.append( "\n" );
			for ( int i = 0; i < indent; i++ ) {
				result.append( indentString );
			}
			beginLine = true;
		}
	}

}