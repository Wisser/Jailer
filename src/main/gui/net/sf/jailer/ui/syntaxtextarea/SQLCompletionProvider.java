/*
 * Copyright 2007 - 2017 the original author or authors.
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
package net.sf.jailer.ui.syntaxtextarea;

import java.awt.Color;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.Stack;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.JComponent;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;

import org.fife.ui.autocomplete.Completion;
import org.fife.ui.autocomplete.CompletionProvider;
import org.fife.ui.autocomplete.DefaultCompletionProvider;
import org.fife.ui.autocomplete.ShorthandCompletion;

import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.modelbuilder.ModelBuilder;
import net.sf.jailer.util.Pair;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlUtil;

/**
 * Auto completions for SQL statements.
 * 
 * @author Ralf Wisser
 */
public abstract class SQLCompletionProvider<SOURCE, SCHEMA, TABLE> extends DefaultCompletionProvider {

	protected SOURCE metaDataSource;
	private Quoting quoting;
	private Map<String, TABLE> userDefinedAliases = new HashMap<String, TABLE>();
	private Map<String, TABLE> aliases = new LinkedHashMap<String, TABLE>();

	/**
	 * @param session
	 *            the session
	 */
	public SQLCompletionProvider(Session session, SOURCE metaDataSource) throws SQLException {
		this.metaDataSource = metaDataSource;
		this.quoting = session == null? null : new Quoting(session);
	}

	/**
	 * Adds an alias for a table.
	 * 
	 * @param name alias
	 * @param table the table
	 */
	public void addAlias(String name, TABLE table) {
		userDefinedAliases.put(Quoting.staticUnquote(name).toUpperCase(Locale.ENGLISH), table);
	}

	/**
	 * Removes all aliases.
	 */
	public void removeAliases() {
		userDefinedAliases.clear();
	}

	/**
	 * Resets the provider.
	 * 
	 * @param session
	 *            new session
	 * @param metaDataSource
	 *            new meta data source
	 */
	public void reset(Session session, SOURCE metaDataSource) throws SQLException {
		this.metaDataSource = metaDataSource;
		this.quoting = session == null? null : new Quoting(session);
	}
	
	/**
	 * Returns the text just before the current caret position that could be
	 * the start of something auto-completable.<p>
	 *
	 * This method returns all characters before the caret that are matched
	 * by  {@link #isValidChar(char)}.
	 *
	 * {@inheritDoc}
	 */
	@Override
	public String getAlreadyEnteredText(JTextComponent comp) {		
		Document doc = comp.getDocument();

		int dot = comp.getCaretPosition();
		Element root = doc.getDefaultRootElement();
		int index = root.getElementIndex(dot);
		Element elem = root.getElement(index);
		int start = elem.getStartOffset();
		int len = dot-start;
		try {
			doc.getText(start, len, seg);
		} catch (BadLocationException ble) {
			ble.printStackTrace();
			return EMPTY_STRING;
		}

		int segEnd = seg.offset + len;
		
		start = segEnd - 1;
		char ch = start >= 0? seg.array[start] : ' ';
		while (start>=seg.offset && Character.isWhitespace(ch)) {
			start--;
			ch = start >= 0? seg.array[start] : ' ';
		}
		if (ch != '*') {
			start = segEnd - 1;
			ch = start >= 0? seg.array[start] : ' ';
			while (start>=seg.offset && (Character.isLetterOrDigit(ch) || ch=='_'|| ch=='"' || ch=='`')) {
				start--;
				ch = start >= 0? seg.array[start] : ' ';
			}
			start++;
		}

		len = segEnd - start;
		return len==0 ? EMPTY_STRING : new String(seg.array, start, len);
	}
	
	private long timeOut;
	private JComponent waitCursorSubject;
	private final int COLUMN_LOADING_TIMEOUT = 7000;
	
	@Override
	protected List<Completion> getCompletionsImpl(JTextComponent comp) {
		timeOut = System.currentTimeMillis() + COLUMN_LOADING_TIMEOUT;
		waitCursorSubject = comp;
		String text = getAlreadyEnteredText(comp);
		if (text!=null) {
			List<SQLCompletion> potentialCompletions = new ArrayList<SQLCompletion>();
			potentialCompletions = getPotentialCompletions(comp, text);
			List<SQLCompletion> matched = new ArrayList<SQLCompletion>(); 
			for (Iterator<SQLCompletion> i = potentialCompletions.iterator(); i.hasNext();) {
				SQLCompletion completion = i.next();
				if (completion.matches(text)) {
					matched.add(completion);
				}
			}

			SQLCompletion.initShortDescriptions(matched);
			List<Completion> compl = new ArrayList<Completion>();
			for (SQLCompletion c: matched) {
				compl.add(c);
			}
			return compl;
		}
		return new ArrayList<Completion>();
	}

	private List<SQLCompletion> getPotentialCompletions(JTextComponent comp, String alreadyEnteredText) {
		Pair<Integer, Integer> loc = ((RSyntaxTextAreaWithSQLSyntaxStyle) comp)
				.getCurrentStatementLocation(true, true);
		String line = ((RSyntaxTextAreaWithSQLSyntaxStyle) comp).getText(loc.a, loc.b, true);
		String lineBeforeCaret = ((RSyntaxTextAreaWithSQLSyntaxStyle) comp).getText(loc.a, loc.b, false);

		int l = comp.getCaret().getDot() - alreadyEnteredText.length() - ((RSyntaxTextAreaWithSQLSyntaxStyle) comp).getLineStartOffsetOfCurrentLine();
		StringBuilder sb = new StringBuilder("\n");
		for (int i = 0; i < l; ++i) {
			sb.append(" ");
		}
		boolean isCaretAtEOL = false;
		try {
			isCaretAtEOL = ((RSyntaxTextAreaWithSQLSyntaxStyle) comp).getText(comp.getCaret().getDot(), ((RSyntaxTextAreaWithSQLSyntaxStyle) comp).getLineEndOffsetOfCurrentLine() - comp.getCaret().getDot()).trim().isEmpty();
		} catch (BadLocationException e) {
		}
		return retrieveCompletions(line, lineBeforeCaret, sb.toString(), isCaretAtEOL);
	}

	private List<SQLCompletion> retrieveCompletions(String line, String beforeCaret, String indent, boolean isCaretAtEOL) {
		int pos = beforeCaret.length();
		String afterCaret = null;
		if (pos > 0) {
			line = reduceStatement(line, pos - 1);
			if (pos < line.length()) {
				beforeCaret = line.substring(0, pos);
				afterCaret = line.substring(pos, line.length());
			}
		}
		aliases.clear();
		aliases.putAll(findAliases(afterCaret != null? beforeCaret + "=" + afterCaret : line));
		aliases.putAll(userDefinedAliases);
		Clause clause = currentClause(beforeCaret);
		List<SQLCompletion> result = new ArrayList<SQLCompletion>();
		withOnCompletions = false;
		for (CompletionRetriever<TABLE, SOURCE> completionRetriever: completionRetrievers) {
			List<SQLCompletion> compl = completionRetriever.retrieveCompletion(line, beforeCaret, clause, metaDataSource, indent, isCaretAtEOL);
			if (compl != null) {
				result.addAll(compl);
			}
		}
		return result;
	}

	public static class SQLCompletion extends ShorthandCompletion {
		
		private final String context;
		public final Color color;
		public final String tooltip;
		
		private static final Color COLOR_SCHEMA = new Color(145, 50, 0);
		private static final Color COLOR_TABLE = new Color(0, 40, 90);
		private static final Color COLOR_COLUMN = new Color(0, 55, 0);
		private static final Color COLOR_KEYWORD = Color.BLUE;

		public SQLCompletion(CompletionProvider provider, String inputText, String replacementText, String context, Color color, String tooltip) {
			super(provider, inputText, replacementText);
			this.context = context != null && context.length() > 0? context : null;
			this.color = color;
			this.tooltip = tooltip;
		}

		public SQLCompletion(CompletionProvider provider, String inputText, String replacementText, String context, Color color) {
			this(provider, inputText, replacementText, context, color, null);
		}
		
		public boolean matches(String inputText) {
			if ("*".equals(getInputText())) {
				String input = inputText.trim();
				return input.equals("*") || input.isEmpty();
			}
			return stripQuote(getInputText()).toUpperCase(Locale.ENGLISH).startsWith(stripQuote(inputText).toUpperCase(Locale.ENGLISH));
		}

		private String stripQuote(String text) {
			if (text.length() > 0) {
				char c = text.charAt(0);
				if (c == '\"' || c == '`') {
					return text.substring(1);
				}
			}
			return text;
		}

		private static void initShortDescriptions(List<SQLCompletion> completions) {
			for (SQLCompletion completion: completions) {
				if (completion.context != null) {
					completion.setShortDescription(completion.context);
				}
			}
		}

		public String getContext() {
			return context;
		}
	}

	private interface CompletionRetriever<TABLE, SOURCE> {
		List<SQLCompletion> retrieveCompletion(String line, String beforeCaret, Clause clausem, SOURCE metaDataSource, String indent, boolean isCaretAtEOL);
	}

	private List<CompletionRetriever<TABLE, SOURCE>> completionRetrievers = new ArrayList<CompletionRetriever<TABLE, SOURCE>>();
	boolean withOnCompletions = false;

	{
		completionRetrievers.add(new CompletionRetriever<TABLE, SOURCE>() {
			@Override
			public List<SQLCompletion> retrieveCompletion(String line, String beforeCaret, Clause clause, SOURCE metaDataSource, String indent, boolean isCaretAtEOL) {
				if (!(clause == Clause.FROM || clause == Clause.TABLE || clause == Clause.ON || clause == Clause.UPDATE || clause == Clause.JOIN || clause == Clause.INTO)) {
					return null;
				}
				
				List<SQLCompletion> result = new ArrayList<SQLCompletion>();
				boolean notDotWord = false;
				
				if (clause != Clause.INTO) {
					String withoutOnClauses = beforeCaret.replaceAll("(?is)\\bon\\b.*?\\bjoin\\b", " join");
					String removeLastOn = withoutOnClauses.replaceAll("(?is)(?:\\bon\\b)\\s+$", "");
					boolean endsWithOn = false;
					if (!removeLastOn.equals(withoutOnClauses)) {
						endsWithOn = true;
						withoutOnClauses = removeLastOn;
					}
					Pattern pattern = Pattern.compile(
							".*?"
							+ "(?:(?:" + reIdentifier + ")\\s*\\.\\s*)?"
							+ "(?:(?:" + reIdentifier + ")\\s+)?"
							+ "(?:as\\s+)?"
							+ "(?!\\b(?:left|right|inner|outer|left\\s+outer|right\\s+outer)\\b)"
							+ "(" + reIdentifier + ")\\s*"
							+ "(?:\\b(?:left|right|inner|outer)\\b\\s*)*\\bjoin\\b\\s*"
							+ "(?:(?:" + reIdentifier + ")\\s*\\.\\s*)?"
							+ "(?:(?:" + reIdentifier + ")\\s+)?"
							+ "(?:as\\s+)?"
							+ "(" + reIdentifier + ")\\s+"
							+ "$", Pattern.CASE_INSENSITIVE|Pattern.DOTALL);
					Matcher matcher = pattern.matcher(withoutOnClauses);
	
					if (matcher.matches() && !"join".equalsIgnoreCase(matcher.group(1)) && !"join".equalsIgnoreCase(matcher.group(2))) {
						withOnCompletions = true;
						TABLE source = findAlias(matcher.group(1));
						TABLE destination = findAlias(matcher.group(2));
						if (source != null && destination != null) {
							List<Association> associations = getAssociations(source, destination);
							for (Association a: associations) {
								String cond = a.getUnrestrictedJoinCondition();
								if (a.reversed) {
									cond = SqlUtil.reversRestrictionCondition(cond);
								}
								cond = SqlUtil.replaceAliases(cond, matcher.group(1), matcher.group(2));
								Color color = null;
								if (a.isInsertDestinationBeforeSource()) {
									color = new Color(100, 0, 0);
								} else if (a.isInsertSourceBeforeDestination()) {
									color = new Color(0, 100, 0);
								} else {
									color = Color.BLUE;
								}
								result.add(new SQLCompletion(SQLCompletionProvider.this, (endsWithOn? "" : "on ") + cond, (endsWithOn? "" : "on ") + cond + " ", a.getName(), color, cond));
							}
						}
						return result;
					}
					
					pattern = Pattern.compile(".*?(" + reIdentifier + ")\\s*(\\b(left|right|inner|outer)\\b\\s*)*\\bjoin\\b\\s*\\w*$", Pattern.CASE_INSENSITIVE|Pattern.DOTALL);
					matcher = pattern.matcher(withoutOnClauses);
	
					if (matcher.matches()) {
						TABLE table = findAlias(matcher.group(1));
						if (table != null) {
							List<Association> associations = getAssociations(table, null);
							Set<String> destSet = new HashSet<String>();
							for (Association a: associations) {
								String schemaName = a.destination.getSchema("");
								SCHEMA schema = schemaName.isEmpty()? getDefaultSchema(metaDataSource) : findSchema(metaDataSource, schemaName);
								if (schema != null) {
									TABLE dest = findTable(schema, a.destination.getUnqualifiedName());
									if (dest != null) {
										String qualifiedName = "";
										if (!schemaName.isEmpty()) {
											qualifiedName = (quoting == null? getSchemaName(schema) : quoting.quote(getSchemaName(schema))) + ".";
										}
										qualifiedName += (quoting == null? getTableName(dest) : quoting.quote(getTableName(dest)));
										destSet.add(qualifiedName);
									}
								}
							}
							for (String dest: destSet) {
								result.add(new SQLCompletion(SQLCompletionProvider.this, Quoting.staticUnquote(dest), dest + " ", null, SQLCompletion.COLOR_TABLE));
							}
						}
						return result;
					}
					
					matcher = identWSWordPattern.matcher(beforeCaret);
					if (matcher.matches() && !"from".equalsIgnoreCase(matcher.group(1)) && !"table".equalsIgnoreCase(matcher.group(1)) && !"update".equalsIgnoreCase(matcher.group(1))) {
						notDotWord = true;
						result.addAll(keywordCompletion("Join", "Left Join"));
					}
				}
				
				Matcher matcher = identDotOnlyPattern.matcher(beforeCaret);
				if (matcher.matches()) {
					SCHEMA schema = findSchema(metaDataSource, matcher.group(1));
					if (schema != null) {
						result.addAll(schemaCompletions(schema));
					}
				} else if (!notDotWord && clause != Clause.ON) {
					// all tables in default schema
					SCHEMA schema = getDefaultSchema(metaDataSource);
					if (schema != null) {
						result.addAll(schemaCompletions(schema));
						if (result != null) {
							for (SCHEMA s: getSchemas(metaDataSource)) {
								if (!s.equals(schema)) {
									result.add(new SQLCompletion(SQLCompletionProvider.this, Quoting.staticUnquote(getSchemaName(s)), quoting == null? getSchemaName(s) : quoting.quote(getSchemaName(s)), 
										null, SQLCompletion.COLOR_SCHEMA));
								}
							}
						}
					}
				}
				
				return result;
			}
		});
		completionRetrievers.add(new CompletionRetriever<TABLE, SOURCE>() {
			@Override
			public List<SQLCompletion> retrieveCompletion(String line, String beforeCaret, Clause clause, SOURCE metaDataSource, String indent, boolean isCaretAtEOL) {
				if (!(clause != Clause.FROM && clause != Clause.TABLE && clause != Clause.UPDATE && clause != Clause.JOIN)) {
					return null;
				}
				
				Matcher matcher = identDotOnlyPattern.matcher(beforeCaret);

				if (matcher.matches()) {
					TABLE context = null;
					List<SQLCompletion> result = null;
					
					SCHEMA schema = getDefaultSchema(metaDataSource);
					String aliasName = matcher.group(1);
					if (schema != null) {
						context = findAlias(aliasName);
						if (context != null) {
							result = tableCompletions(context);
						}
					}
					if (result == null) {
						schema = findSchema(metaDataSource, aliasName);
						if (schema != null) {
							result = schemaCompletions(schema);
						}
					}
					return result;
				}
				return null;
			}
		});
		completionRetrievers.add(new CompletionRetriever<TABLE, SOURCE>() {
			@Override
			public List<SQLCompletion> retrieveCompletion(String line, String beforeCaret, Clause clause, SOURCE metaDataSource, String indent, boolean isCaretAtEOL) {
				if (!(clause != Clause.FROM && clause != Clause.TABLE && clause != Clause.UPDATE && clause != Clause.JOIN)) {
					return null;
				}
				List<SQLCompletion> result = new ArrayList<SQLCompletion>();
				
				if (clause == Clause.SELECT) {
					Pattern pattern = Pattern.compile(".*?(\\bselect\\b|,|(" + reIdentifier + ")\\.)\\s*\\*?\\s*$", Pattern.DOTALL|Pattern.CASE_INSENSITIVE);
					Matcher matcher = pattern.matcher(beforeCaret);
	
					if (matcher.matches()) {
						List<TABLE> tables = new ArrayList<TABLE>(); 
						List<String> tableNames = new ArrayList<String>(); 
						String alias = matcher.group(2);
						if (alias != null) {
							if (indent.length() > alias.length() + 1) {
								indent = indent.substring(0, indent.length() - alias.length() - 1);
							}
							TABLE table = findAlias(alias);
							if (table != null) {
								tables.add(table);
							}
						} else {
							for (Entry<String, TABLE> entry: aliases.entrySet()) {
								tables.add(entry.getValue());
								tableNames.add(entry.getKey());
							}
						}
						boolean timedOut = false;
						if (!tables.isEmpty()) {
							Map<String, Integer> count = new HashMap<String, Integer>();
							for (TABLE table: tables) {
								List<String> tableColumns = getAndWaitForColumns(table);
								if (tableColumns.isEmpty()) {
									// time out, no completion
									timedOut = true;
									break;
								}
								for (String column: tableColumns) {
									String unquotedColumn = Quoting.staticUnquote(column).toUpperCase(Locale.ENGLISH);
									if (count.containsKey(unquotedColumn)) {
										count.put(unquotedColumn, count.get(unquotedColumn) + 1);
									} else {
										count.put(unquotedColumn, 1);
									}
								}
							}
							for (String key: new HashSet<String>(count.keySet())) {
								if (count.get(key) == 1) {
									count.remove(key);
								} else {
									count.put(key, 1);
								}
							}
							if (!timedOut) {
								String replacement = createStarReplacement(tables, tableNames, alias, count, "", isCaretAtEOL);
								if (replacement.length() > 60 && indent.length() < 60) {
									replacement  = createStarReplacement(tables, tableNames, alias, count, indent, isCaretAtEOL);
								}
								result.add(new SQLCompletion(SQLCompletionProvider.this, "*", replacement + " ", replacement, SQLCompletion.COLOR_COLUMN));
							}
						}
					}
				}
				
				Matcher matcher = identDotOnlyPattern.matcher(beforeCaret);

				if (!matcher.matches()) {
					SCHEMA schema = getDefaultSchema(metaDataSource);
					if (schema != null) {
						Map<String, Integer> colCount = new HashMap<String, Integer>();
						for (Entry<String, TABLE> entry: aliases.entrySet()) {
							result.add(new SQLCompletion(SQLCompletionProvider.this, Quoting.staticUnquote(entry.getKey()), entry.getKey(), null, SQLCompletion.COLOR_TABLE));
							for (String c: getAndWaitForColumns(entry.getValue())) {
								if (!colCount.containsKey(c)) {
									colCount.put(c, 1);
								} else {
									colCount.put(c, colCount.get(c) + 1);
								}
							}
						}
						for (Entry<String, TABLE> entry: aliases.entrySet()) {
							for (String c: getAndWaitForColumns(entry.getValue())) {
								if (colCount.get(c) > 1 || defaultClause != null) {
									result.add(new SQLCompletion(SQLCompletionProvider.this, Quoting.staticUnquote(c), entry.getKey() + "." + c, entry.getKey(), SQLCompletion.COLOR_COLUMN));
								} else {
									result.add(new SQLCompletion(SQLCompletionProvider.this, Quoting.staticUnquote(c), c, entry.getKey(), SQLCompletion.COLOR_COLUMN));
								}	
							}
						}
					}
				}
				return result;
			}

			public String createStarReplacement(List<TABLE> tables, List<String> tableNames, String alias,
					Map<String, Integer> countMap, String indent, boolean isCaretAtEOL) {
				Map<String, Integer> count = new HashMap<String, Integer>(countMap);
				Set<String> allColumnNames = new HashSet<String>();
				for (int i = 0; i < tables.size(); ++i) {
					TABLE table = tables.get(i);
					for (String column: getAndWaitForColumns(table)) {
						allColumnNames.add(Quoting.staticUnquote(column).toUpperCase(Locale.ENGLISH));
					}
				}
				StringBuilder sb = new StringBuilder();
				for (int i = 0; i < tables.size(); ++i) {
					TABLE table = tables.get(i);
					for (String column: getAndWaitForColumns(table)) {
						for (;;) {
							String suffix = "";
							String unquotedColumn = Quoting.staticUnquote(column).toUpperCase(Locale.ENGLISH);
							if (count.containsKey(unquotedColumn)) {
								int nr = count.get(unquotedColumn);
								if (nr != 1) {
									if (nr > 1) {
										suffix = "_" + (nr - 1);
									} else {
										suffix = "_" + nr;
									}
								}
								count.put(unquotedColumn, nr + 1);
							}
							String columnWithSuffix;
							if (suffix.isEmpty() || column.isEmpty() || column.charAt(0) != column.charAt(column.length() - 1) || !Quoting.isPotentialIdentifierQuote(column.charAt(0))) {
								columnWithSuffix = column + suffix;
							} else {
								columnWithSuffix = column.substring(0, column.length() - 1) + suffix + column.charAt(column.length() - 1);
							}
							String unquotedColumnWithSuffix = Quoting.staticUnquote(columnWithSuffix).toUpperCase(Locale.ENGLISH);
							if (!suffix.isEmpty()) {
								if (allColumnNames.contains(unquotedColumnWithSuffix)) {
									continue;
								}
							}
							if (sb.length() > 0) {
								sb.append(", " + indent);
							}
							if (alias != null) {
								if (sb.length() > 0) {
									sb.append(alias + ".");
								}
							} else {
								sb.append(tableNames.get(i) + ".");
							}
							if (column.equals(columnWithSuffix)) {
								sb.append(column);
							} else {
								sb.append(column + " as " + columnWithSuffix);
							}
							allColumnNames.add(unquotedColumnWithSuffix);
							break;
						}
					}
				}
				if (isCaretAtEOL) {
					sb.append(" ");
				} else if (indent.length() > 2) {
					sb.append(indent.substring(0, indent.length() - 2));
				}
				String replacement = sb.toString();
				return replacement;
			}
		});
		completionRetrievers.add(new CompletionRetriever<TABLE, SOURCE>() {
			@Override
			public List<SQLCompletion> retrieveCompletion(String line, String beforeCaret, Clause clause, SOURCE metaDataSource, String indent, boolean isCaretAtEOL) {
				if (clause == null) {
					return keywordCompletion("Select", "Insert", "Delete");
				} else {
					switch (clause) {
					case FROM: return keywordCompletion("Where");
					case UPDATE: return keywordCompletion("set");
					case SET: return keywordCompletion("Where");
					case GROUP: return keywordCompletion("Having");
					case HAVING: return null;
					case INTO: return keywordCompletion("Values", "Select");
					case JOIN: 
						if (!withOnCompletions) {
							return keywordCompletion("Where", "Group by");
						}
						break;
					case ON: return keywordCompletion("Where", "Group by");
					case ORDER: return null;
					case SELECT: return keywordCompletion("From");
					case WHERE: 
						if (defaultClause != Clause.WHERE) {
							return keywordCompletion("Group by", "Order by");
						}
					default:
						break;
					}
				}
				return null;
			}
		});
	}

	private List<SQLCompletion> tableCompletions(TABLE context) {
		List<SQLCompletion> newCompletions = new ArrayList<SQLCompletion>();
		if (context != null) {
			for (String column: getAndWaitForColumns(context)) {
				newCompletions.add(new SQLCompletion(this, Quoting.staticUnquote(column), quoting == null? column : quoting.quote(column), 
						getTableName(context), SQLCompletion.COLOR_COLUMN));
			}
		}
		return newCompletions;
	}

	private List<SQLCompletion> keywordCompletion(String... keywords) {
		List<SQLCompletion> newCompletions = new ArrayList<SQLCompletion>();
		for (String keyword: keywords) {
			newCompletions.add(new SQLCompletion(this, keyword, keyword + " ", null, SQLCompletion.COLOR_KEYWORD));
		}
		return newCompletions;
	}

	private List<SQLCompletion> schemaCompletions(SCHEMA schema) {
		List<SQLCompletion> newCompletions = new ArrayList<SQLCompletion>();
		if (schema != null) {
			for (TABLE table: getTables(schema)) {
				String tableName = getTableName(table);
				if (!ModelBuilder.isJailerTable(tableName)) {
					newCompletions.add(new SQLCompletion(this, Quoting.staticUnquote(tableName), quoting == null? tableName : quoting.quote(tableName),
							getSchemaName(schema), SQLCompletion.COLOR_TABLE));
				}
			}
		}
		return newCompletions;
	}

	private static String reduceStatement(String statement, int caretPos) {
		Pattern pattern = Pattern.compile("('([^']*'))|(/\\*.*?\\*/)|(\\-\\-.*?(\n|$))", Pattern.DOTALL);
		Matcher matcher = pattern.matcher(statement);
		boolean result = matcher.find();
		StringBuffer sb = new StringBuffer();
		if (result) {
			do {
				int l = matcher.group(0).length();
				matcher.appendReplacement(sb, "");
				while (l > 0) {
					--l;
					sb.append(' ');
				}
				result = matcher.find();
			} while (result);
		}
		matcher.appendTail(sb);
		
		Set<Integer> myStarts = new HashSet<Integer>();
		Stack<Integer> ordPos = new Stack<Integer>();
		Map<Integer, Integer> endPerOrdPos = new HashMap<Integer, Integer>();
		for (int i = 0; i < sb.length(); ++i) {
			char c = sb.charAt(i);
			if (c == '(') {
				ordPos.push(i);
			} else if (c == ')') {
				if (!ordPos.isEmpty()) {
					int start = ordPos.pop();
					endPerOrdPos.put(start, i);
				}
			}
			if (i == caretPos) {
				myStarts.addAll(ordPos);
			}
		}
		
		ordPos = new Stack<Integer>();
		for (int i = 0; i < sb.length(); ++i) {
			char c = sb.charAt(i);
			if (c == '(') {
				ordPos.push(i);
			} else if (c == ')') {
				if (!ordPos.isEmpty()) {
					ordPos.pop();
				}
			} else {
				if (!ordPos.isEmpty() && !myStarts.contains(ordPos.peek())) {
					sb.setCharAt(i, ' ');
				}
			}
		}
		
		String reduced = sb.toString();
		return reduced;
	}

	private Map<String, TABLE> findAliases(String statement) {
		Map<String, TABLE> aliases = new LinkedHashMap<String, TABLE>();
		Pattern pattern = Pattern.compile("(?:\\bas\\b)|(" + reClauseKW + ")|(,|\\(|\\)|=|<|>|!|\\.|\\b(?:on|where|left|right|full|inner|outer|join|and|or|not)\\b)|(" + reIdentifier + ")", Pattern.DOTALL|Pattern.CASE_INSENSITIVE);
		Matcher matcher = pattern.matcher(statement + ")");
		boolean inFrom = false;
		int level = 0;
		Map<String, Integer> levelPerAlias = new HashMap<String, Integer>();
		Stack<String> tokenStack = new Stack<String>();
		boolean result = matcher.find();
		if (result) {
			do {
				String clause = matcher.group(1);
				String keyword = matcher.group(2);
				String identifier = matcher.group(3);
				
				if (clause != null) {
					if (!"from".equalsIgnoreCase(clause) && !"update".equalsIgnoreCase(clause)) {
						keyword = clause;
					}
				}
				
				if (keyword != null) {
					if ("(".equals(keyword)) {
						++level;
					} else if (")".equals(keyword)) {
						--level;
					}
				}
				boolean clear = false;
				if (inFrom) {
					if (keyword != null) {
						if (keyword.equals(".")) {
							tokenStack.push(keyword);
						} else {
							clear = true;
						}
					} else if (identifier != null) {
						tokenStack.push(identifier);
					}
					if (!clear && !tokenStack.isEmpty() && !".".equals(tokenStack.peek())) {
						ArrayList<String> tokens = new ArrayList<String>(tokenStack);
						String schema = null;
						String table = null;
						String alias = null;
						if (tokens.size() >= 4) {
							if (tokens.get(tokens.size() - 3).equals(".")) {
								alias = tokens.get(tokens.size() - 1);
								table = tokens.get(tokens.size() - 2);
								schema = tokens.get(tokens.size() - 4);
							}
						} else if (tokens.size() >= 2) {
							if (!tokens.get(tokens.size() - 2).equals(".")) {
								alias = tokens.get(tokens.size() - 1);
								table = tokens.get(tokens.size() - 2);
								schema = null;
							}
						}
						if (alias != null && table != null) {
							SCHEMA mdSchema = null;
							if (schema != null) {
								mdSchema = findSchema(metaDataSource, schema);
							} else {
								mdSchema = getDefaultSchema(metaDataSource);
							}
							if (mdSchema != null) {
								TABLE mdTable = findTable(mdSchema, table);
								if (mdTable != null) {
									Integer prevLevel = levelPerAlias.get(alias);
									if (prevLevel == null || prevLevel < level) {
										aliases.put(alias, mdTable);
										levelPerAlias.put(alias, level);
										tokenStack.clear();
									}
								}
							}
						}
					}
					if (clear) {
						ArrayList<String> tokens = new ArrayList<String>(tokenStack);
						String schema = null;
						String table = null;
						String alias = null;
						if (tokens.size() >= 3) {
							if (tokens.get(tokens.size() - 2).equals(".")) {
								table = tokens.get(tokens.size() - 1);
								alias = table;
								schema = tokens.get(tokens.size() - 3);
							}
						} else if (tokens.size() >= 1) {
							if (!tokens.get(tokens.size() - 1).equals(".")) {
								table = tokens.get(tokens.size() - 1);
								alias = table;
								schema = null;
							}
						}
						if (alias != null && table != null) {
							SCHEMA mdSchema = null;
							if (schema != null) {
								mdSchema = findSchema(metaDataSource, schema);
							} else {
								mdSchema = getDefaultSchema(metaDataSource);
							}
							if (mdSchema != null) {
								TABLE mdTable = findTable(mdSchema, table);
								if (mdTable != null) {
									Integer prevLevel = levelPerAlias.get(alias);
									if (prevLevel == null || prevLevel < level) {
										aliases.put(alias, mdTable);
										levelPerAlias.put(alias, level);
									}
								}
							}
						}
						tokenStack.clear();
					}
				}
				if (clause != null) {
					inFrom = "from".equalsIgnoreCase(clause) || "update".equalsIgnoreCase(clause);
					clear = true;
				}
				result = matcher.find();
			} while (result);
		}
		return aliases;
	}

	public enum Clause {
		SELECT("select"),
		FROM("from"),
		WHERE("where"),
		GROUP("group"),
		HAVING("having"),
		JOIN("join"),
		ORDER("order"),
		INTO("into"),
		UPDATE("update"),
		SET("set"),
		ON("on"),
		TABLE("table");
		
		private final String name;

		Clause(String name) {
			this.name = name;
		}
	};

	private Clause currentClause(String sql) {
		Pattern pattern = Pattern.compile(".*\\b(select|from|where|group|having|order|join|on|update|set|into|table)\\b(.*?)$", Pattern.DOTALL|Pattern.CASE_INSENSITIVE);
		Matcher matcher = pattern.matcher(sql);
		if (matcher.matches()) {
			for (Clause clause: Clause.values()) {
				if (clause.name.equalsIgnoreCase(matcher.group(1))) {
					if (clause == Clause.ON) {
						String rest = matcher.group(2);
						int level = 0;
						for (int i = 0; i < rest.length(); ++i) {
							char c = rest.charAt(i);
							if (c == '(') {
								++level;
							} else if (c == ')') {
								--level;
							} else if (c == ',') {
								if (level == 0) {
									clause = Clause.FROM;
									break;
								}
							}
						}
					}
					return clause;
				}
			}
		}
		return defaultClause;
	}

	private static String reIdentifier = "(?:[\"][^\"]+[\"])|(?:[`][^`]+[`])|(?:['][^']+['])|(?:[\\w]+)";
	private static String reIdentDotOnly = ".*?(" + reIdentifier + ")\\s*\\.\\s*[\"'`]?\\w*$";
	private static String reClauseKW = "\\b(?:select|from|update|where|group|having)\\b";
	private static String reIdentWSWordPattern = ".*?(" + reIdentifier + ")\\s+\\w*$";
	
	private static Pattern identDotOnlyPattern = Pattern.compile(reIdentDotOnly, Pattern.DOTALL);
	private static Pattern identWSWordPattern = Pattern.compile(reIdentWSWordPattern, Pattern.DOTALL);

	private TABLE findAlias(String aliasName) {
		TABLE context;
		context = null;
		for (Entry<String, TABLE> entry: aliases.entrySet()) {
			if (Quoting.staticUnquote(entry.getKey()).toUpperCase(Locale.ENGLISH).equals(Quoting.staticUnquote(aliasName).toUpperCase(Locale.ENGLISH))) {
				context = entry.getValue();
				break;
			}
		}
		return context;
	}

	public List<String> getAndWaitForColumns(TABLE table) {
		return getColumns(table, timeOut, waitCursorSubject);
	}

	private Clause defaultClause = null;
	
	public void setDefaultClause(Clause clause) {
		this.defaultClause = clause;
	}

	protected abstract List<String> getColumns(TABLE table, long timeOut, JComponent waitCursorSubject);
	protected abstract SCHEMA getDefaultSchema(SOURCE metaDataSource);
	protected abstract SCHEMA findSchema(SOURCE metaDataSource, String name);
	protected abstract TABLE findTable(SCHEMA schema, String name);
	protected abstract String getTableName(TABLE table);
	protected abstract List<TABLE> getTables(SCHEMA schema);
	protected abstract String getSchemaName(SCHEMA schema);
	protected abstract List<SCHEMA> getSchemas(SOURCE metaDataSource);
	protected abstract List<Association> getAssociations(TABLE source, TABLE destination);

};
