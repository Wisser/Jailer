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

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;

import org.fife.ui.autocomplete.Completion;
import org.fife.ui.autocomplete.CompletionProvider;
import org.fife.ui.autocomplete.DefaultCompletionProvider;
import org.fife.ui.autocomplete.ShorthandCompletion;

import net.sf.jailer.database.Session;
import net.sf.jailer.util.Pair;
import net.sf.jailer.util.Quoting;

/**
 * Auto completions for SQL statements.
 * 
 * @author Ralf Wisser
 */
public abstract class SQLCompletionProvider<SOURCE, SCHEMA, TABLE> extends DefaultCompletionProvider {

	private SOURCE metaDataSource;
	private Quoting quoting;
	private Map<String, TABLE> alias = new HashMap<String, TABLE>();

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
		alias.put(Quoting.staticUnquote(name).toUpperCase(Locale.ENGLISH), table);
	}

	/**
	 * Removes all aliases.
	 */
	public void removeAliases() {
		alias.clear();
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
		while (start>=seg.offset && (Character.isLetterOrDigit(ch) || ch=='_'|| ch=='"' || ch=='`')) {
			start--;
			ch = start >= 0? seg.array[start] : ' ';
		}
		start++;

		len = segEnd - start;
		return len==0 ? EMPTY_STRING : new String(seg.array, start, len);
	}
	
	@Override
	protected List<Completion> getCompletionsImpl(JTextComponent comp) {
		String text = getAlreadyEnteredText(comp);

		if (text!=null) {
			List<Completion> potentialCompletions = new ArrayList<Completion>();
			potentialCompletions = getPotentialCompletions(comp);
			List<Completion> compl = new ArrayList<Completion>(potentialCompletions); 
			for (Iterator<Completion> i = compl.iterator(); i.hasNext();) {
				Completion completion = i.next();
				if (completion instanceof ColumnCompletion) {
					if (!((ColumnCompletion<TABLE>) completion).matches(text)) {
						i.remove();
					}
				}
			}

			return compl;
		}
		return new ArrayList<Completion>();
	}

	private List<Completion> getPotentialCompletions(JTextComponent comp) {
		Pair<Integer, Integer> loc = ((RSyntaxTextAreaWithSQLSyntaxStyle) comp)
				.getCurrentStatementLocation(true);
		String line = ((RSyntaxTextAreaWithSQLSyntaxStyle) comp).getText(loc.a, loc.b, true);
		String lineBeforeCaret = ((RSyntaxTextAreaWithSQLSyntaxStyle) comp).getText(loc.a, loc.b, false);

		return retrieveCompletions(line, lineBeforeCaret);
	}

	private List<Completion> retrieveCompletions(String line, String beforeCaret) {
		for (ContextRetriever<TABLE, SOURCE> contextRetriever : contextRetrievers) {
			List<Completion> compl = contextRetriever.retrieveContext(line, beforeCaret, metaDataSource);
			if (compl != null) {
				return compl;
			}
		}
		return Collections.emptyList();
	}

	private static class ColumnCompletion<TABLE> extends ShorthandCompletion {
		private final boolean isAlias;

		public ColumnCompletion(CompletionProvider provider, String inputText, String replacementText, String description) {
			this(provider, inputText, replacementText, description, false);
		}
		
		public ColumnCompletion(CompletionProvider provider, String inputText, String replacementText, String description, boolean isAlias) {
			super(provider, inputText, replacementText);
			this.setShortDescription(description);
			this.isAlias = isAlias;
		}

		public boolean matches(String inputText) {
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

		@Override
		public int compareTo(Completion c2) {
			boolean c2IsAlias = false;
			if (c2 instanceof ColumnCompletion) {
				c2IsAlias = ((ColumnCompletion) c2).isAlias;
			}
			if (isAlias == c2IsAlias) {
				return super.compareTo(c2);
			}
			return isAlias? -1 : 1;
		}
	}

	private interface ContextRetriever<TABLE, SOURCE> {
		List<Completion> retrieveContext(String line, String beforeCaret, SOURCE metaDataSource);
	}

	private List<ContextRetriever<TABLE, SOURCE>> contextRetrievers = new ArrayList<ContextRetriever<TABLE, SOURCE>>();

	{
		contextRetrievers.add(new ContextRetriever<TABLE, SOURCE>() {
			@Override
			public List<Completion> retrieveContext(String line, String beforeCaret, SOURCE metaDataSource) {
				Matcher matcher = schemaTableAttributePattern.matcher(beforeCaret);

				if (matcher.matches()) {
					// System.out.println(": " + matcher.group(1) + " " +
					// matcher.group(2));
					TABLE context = null;

					SCHEMA schema = findSchema(metaDataSource, matcher.group(1));
					if (schema != null) {
						context = findTable(schema, matcher.group(2));
					}
					return tableCompletions(context);
				}
				return null;
			}
		});
		contextRetrievers.add(new ContextRetriever<TABLE, SOURCE>() {
			@Override
			public List<Completion> retrieveContext(String line, String beforeCaret, SOURCE metaDataSource) {
				Matcher matcher = tableAttributePattern.matcher(beforeCaret);

				if (matcher.matches()) {
					TABLE context = null;
					List<Completion> result = null;
					
					SCHEMA schema = getDefaultSchema(metaDataSource);
					if (schema != null) {
						context = alias.get(Quoting.staticUnquote(matcher.group(1)).toUpperCase(Locale.ENGLISH));
						if (context == null) {
							context = findTable(schema, matcher.group(1));
						}
						if (context != null) {
							result = tableCompletions(context);
						}
					}
					if (result == null) {
						schema = findSchema(metaDataSource, matcher.group(1));
						if (schema != null) {
							result = schemaCompletions(schema);
						}
					}
					return result;
				}
				return null;
			}
		});
		contextRetrievers.add(new ContextRetriever<TABLE, SOURCE>() {
			@Override
			public List<Completion> retrieveContext(String line, String beforeCaret, SOURCE metaDataSource) {
				// all tables in default schema
				List<Completion> result = null;
				
				SCHEMA schema = getDefaultSchema(metaDataSource);
				if (schema != null) {
					result = schemaCompletions(schema);
					if (result != null) {
						for (String a: alias.keySet()) {
							result.add(new ColumnCompletion<TABLE>(SQLCompletionProvider.this, a, a, null, true));
						}
						for (SCHEMA s: getSchemas(metaDataSource)) {
							if (!s.equals(schema)) {
								result.add(new ColumnCompletion<TABLE>(SQLCompletionProvider.this, Quoting.staticUnquote(getSchemaName(s)), quoting == null? getSchemaName(s) : quoting.quote(getSchemaName(s)), 
									null));
							}
						}
					}
				}
				return result;
			}
		});
	}

	private List<Completion> tableCompletions(TABLE context) {
		List<Completion> newCompletions = new ArrayList<Completion>();
		if (context != null) {
			for (String column: getColumns(context)) {
				newCompletions.add(new ColumnCompletion<TABLE>(this, Quoting.staticUnquote(column), quoting == null? column : quoting.quote(column), 
						null)); // getTableName(context)));
			}
		}
		return newCompletions;
	}

	private List<Completion> schemaCompletions(SCHEMA schema) {
		List<Completion> newCompletions = new ArrayList<Completion>();
		if (schema != null) {
//			SCHEMA defaultSchema = getDefaultSchema(metaDataSource);
			for (TABLE table: getTables(schema)) {
				newCompletions.add(new ColumnCompletion<TABLE>(this, Quoting.staticUnquote(getTableName(table)), quoting == null? getTableName(table) : quoting.quote(getTableName(table)),
					null)); // schema.equals(defaultSchema)? null : getSchemaName(schema)));
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
		String reduced = sb.toString();
		return reduced;
	}

	private static String reIdentifier = "(?:[\"][^\"]+[\"])|(?:[`][^`]+[`])|(?:['][^']+['])|(?:[\\w]+)";
	private static String re = ".*?(?:(" + reIdentifier + ")\\s*\\.\\s*)(" + reIdentifier + ")\\s*\\.\\s*[\"'`]?\\w*$";
	private static String reTableOnly = ".*?(" + reIdentifier + ")\\s*\\.\\s*[\"'`]?\\w*$";

	private static Pattern schemaTableAttributePattern = Pattern.compile(re, Pattern.DOTALL);
	private static Pattern tableAttributePattern = Pattern.compile(reTableOnly, Pattern.DOTALL);

	protected abstract List<String> getColumns(TABLE table);
	protected abstract SCHEMA getDefaultSchema(SOURCE metaDataSource);
	protected abstract SCHEMA findSchema(SOURCE metaDataSource, String name);
	protected abstract TABLE findTable(SCHEMA schema, String name);
	protected abstract String getTableName(TABLE table);
	protected abstract List<TABLE> getTables(SCHEMA schema);
	protected abstract String getSchemaName(SCHEMA schema);
	protected abstract List<SCHEMA> getSchemas(SOURCE metaDataSource);


	public static void main(String[] args) throws Exception {
		String statement =
				"Select '1(', '2' (Select x from y) from table1 /* comment -- \n"
				+ "123 */ , tabel2 -- xy\n" 
				+ ", table3 /* comment 2 \n ... \n*/, table4";
		System.out.println(reduceStatement(statement, 43));
	}
	
};
