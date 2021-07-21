/*
 * Copyright 2007 - 2021 Ralf Wisser.
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
import java.util.Collections;
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
import javax.swing.text.Segment;

import org.fife.ui.autocomplete.Completion;
import org.fife.ui.autocomplete.CompletionProvider;
import org.fife.ui.autocomplete.DefaultCompletionProvider;
import org.fife.ui.autocomplete.ShorthandCompletion;

import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.modelbuilder.ModelBuilder;
import net.sf.jailer.ui.databrowser.metadata.MDTable;
import net.sf.jailer.ui.databrowser.metadata.MetaDataPanel.OutlineInfo;
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
    private final Session session;
    private Quoting llQuoting;
    private Map<String, TABLE> userDefinedAliases = new HashMap<String, TABLE>();
    private Map<String, TABLE> aliases = new LinkedHashMap<String, TABLE>();
    private Map<String, TABLE> aliasesTopLevel = new LinkedHashMap<String, TABLE>();
    
    /**
     * @param session
     *            the session
     */
    public SQLCompletionProvider(Session session, SOURCE metaDataSource) {
        this.metaDataSource = metaDataSource;
        this.session = session;
    }

    /**
     * Adds an alias for a table.
     * 
     * @param name alias
     * @param table the table
     */
    public void addAlias(String name, TABLE table) {
        userDefinedAliases.put(Quoting.normalizeIdentifier(name), table);
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
        this.llQuoting = session == null? null : Quoting.getQuoting(session);
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

    private Quoting getQuoting() {
    	if (llQuoting == null) {
    		try {
				llQuoting = session == null? null : Quoting.getQuoting(session);
			} catch (SQLException e) {
				// ignore
			}
    	}
    	return llQuoting;
    }
    
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
                .getCurrentStatementLocation(true, true, null, false);
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
        aliasesTopLevel.clear();
        aliases.putAll(findAliases(afterCaret != null? beforeCaret + "=" + afterCaret : line, aliasesTopLevel, null));
        aliases.putAll(userDefinedAliases);
        aliasesTopLevel.putAll(userDefinedAliases);
        Clause clause = currentClause(beforeCaret);
        List<SQLCompletion> result = new ArrayList<SQLCompletion>();
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
    
                    boolean matchesJoinPattern = false;
                    if (matcher.matches() && !"join".equalsIgnoreCase(matcher.group(1)) && !"join".equalsIgnoreCase(matcher.group(2))) {
                        matchesJoinPattern = true;
                        TABLE source = findAlias(matcher.group(1));
                        TABLE destination = findAlias(matcher.group(2));
                        if (source != null && destination != null) {
                            List<Association> associations = getAssociations(source, destination);
                            for (Association a: associations) {
                                String cond = a.getUnrestrictedJoinCondition();
                                if (a.reversed) {
                                    cond = SqlUtil.reversRestrictionCondition(cond);
                                } else {
                                    cond = SqlUtil.normalizeRestrictionCondition(cond);
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
                                result.add(new SQLCompletion(SQLCompletionProvider.this, (endsWithOn? " " : "on ") + cond, (endsWithOn? "" : "on ") + cond + " ", a.getName(), color, cond));
                            }
                        }
                        if (!result.isEmpty()) {
                            return result;
                        }
                    }
                    
                    pattern = Pattern.compile(".*?(" + reIdentifier + ")\\s*(\\b(left|right|inner|outer)\\b\\s*)*\\bjoin\\b\\s*\\w*$", Pattern.CASE_INSENSITIVE|Pattern.DOTALL);
                    matcher = pattern.matcher(withoutOnClauses);

                    if (matcher.matches()) {
                        matchesJoinPattern = true;
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
                                            qualifiedName = (getQuoting() == null? getSchemaName(schema) : getQuoting().quote(getSchemaName(schema))) + ".";
                                        }
                                        qualifiedName += (getQuoting() == null? getTableName(dest) : getQuoting().quote(getTableName(dest)));
                                        destSet.add(qualifiedName);
                                        String cond = a.getUnrestrictedJoinCondition();
                                        if (a.reversed) {
                                            cond = SqlUtil.reversRestrictionCondition(cond);
                                        } else {
                                            cond = SqlUtil.normalizeRestrictionCondition(cond);
                                        }
                                        cond = SqlUtil.replaceAliases(cond, matcher.group(1), getTableName(dest));
                                        Color color = null;
                                        if (a.isInsertDestinationBeforeSource()) {
                                            color = new Color(100, 0, 0);
                                        } else if (a.isInsertSourceBeforeDestination()) {
                                            color = new Color(0, 100, 0);
                                        } else {
                                            color = Color.BLUE;
                                        }
                                        result.add(new SQLCompletion(SQLCompletionProvider.this, Quoting.staticUnquote(qualifiedName) + " on " + cond, qualifiedName + " on " + cond + " ", a.getName(), color, cond));
                                    }
                                }
                            }
                            for (String dest: destSet) {
                                result.add(new SQLCompletion(SQLCompletionProvider.this, Quoting.staticUnquote(dest), dest + " ", null, SQLCompletion.COLOR_TABLE));
                            }
                        }
                        if (!result.isEmpty()) {
                            return result;
                        }
                    }
                    
                    matcher = identWSWordPattern.matcher(beforeCaret);
                    if (matcher.matches() && !"from".equalsIgnoreCase(matcher.group(1)) && !"table".equalsIgnoreCase(matcher.group(1)) && !"update".equalsIgnoreCase(matcher.group(1))) {
                        if (!matchesJoinPattern) {
                            notDotWord = true;
                            result.addAll(keywordCompletion("Join", "Left Join"));
                        }
                    }
                }
                
                Matcher matcher = identDotOnlyPattern.matcher(beforeCaret);
                if (matcher.matches()) {
                    SCHEMA schema = findSchema(metaDataSource, matcher.group(1));
                    if (schema != null) {
                        result.addAll(schemaCompletions(schema));
                    }
                } else if (!notDotWord && clause != Clause.ON) {
                    if (clause == Clause.INTO) {
                        String reIdentIntoPattern = ".*?(?:(" + reIdentifier + ")\\s*\\.\\s*)?(" + reIdentifier + ")(\\s*\\(?\\s*)$";
                        Pattern identIntoPattern = Pattern.compile(reIdentIntoPattern, Pattern.DOTALL);

                        matcher = identIntoPattern.matcher(beforeCaret);
                        if (matcher.matches()) {
                            if (matcher.group(3) != null || matcher.group(3).length() > 0) {
                                String schema = matcher.group(1);
                                String table = matcher.group(2);
                                SCHEMA mdSchema = null;
                                if (schema != null) {
                                    mdSchema = findSchema(metaDataSource, schema);
                                } else {
                                    mdSchema = getDefaultSchema(metaDataSource);
                                }
                                if (mdSchema != null) {
                                    TABLE mdTable = findTable(mdSchema, table);
                                    if (mdTable != null) {
                                        Map<String, Integer> countMap = new HashMap<String, Integer>();
                                        countMap.put(getTableName(mdTable), 1);
                                        String intoList = createStarReplacement(Collections.singletonList(mdTable), Collections.singletonList(getTableName(mdTable)), null, countMap, "", true).trim();
                                        if (intoList.length() > 100 && indent.length() < 60) {
                                            intoList = createStarReplacement(Collections.singletonList(mdTable), Collections.singletonList(getTableName(mdTable)), null, countMap, indent, true).trim();
                                        }
                                        if (matcher.group(3) == null || !matcher.group(3).contains("(")) {
                                            intoList = "(" + intoList;
                                        }
                                        intoList += ") ";
                                        result.add(new SQLCompletion(SQLCompletionProvider.this, intoList, intoList, intoList, SQLCompletion.COLOR_COLUMN));
                                        return result;
                                    }
                                }
                            }
                        }
                    }
                    
                    // all tables in default schema
                    SCHEMA schema = getDefaultSchema(metaDataSource);
                    if (schema != null) {
                        result.addAll(schemaCompletions(schema));
                        if (result != null) {
                            for (SCHEMA s: getSchemas(metaDataSource)) {
                                if (!s.equals(schema)) {
                                    result.add(new SQLCompletion(SQLCompletionProvider.this, Quoting.staticUnquote(getSchemaName(s)), getQuoting() == null? getSchemaName(s) : getQuoting().quote(getSchemaName(s)), 
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
                        	Stack<Pair<String, TABLE>> stack = new Stack<Pair<String, TABLE>>();
                            for (Entry<String, TABLE> entry: aliasesTopLevel.entrySet()) {
                                stack.push(new Pair<String, TABLE>(entry.getKey(), entry.getValue()));
                            }
                            Set<String> seen = new HashSet<String>();
                            while (!stack.isEmpty()) {
                            	Pair<String, TABLE> entry = stack.pop();
                            	if (!seen.contains(Quoting.normalizeIdentifier(entry.a))) {
	                            	tables.add(entry.b);
	                                tableNames.add(entry.a);
	                                seen.add(Quoting.normalizeIdentifier(entry.a));
                            	}
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
                                    String unquotedColumn = Quoting.normalizeIdentifier(column);
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
                        break;
                    case ON: return keywordCompletion("Where", "Group by");
                    case ORDER: return null;
                    case SELECT: return keywordCompletion("from");
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
    
    public String createStarReplacement(List<TABLE> tables, List<String> tableNames, String alias,
            Map<String, Integer> countMap, String indent, boolean isCaretAtEOL) {
        Map<String, Integer> count = new HashMap<String, Integer>(countMap);
        Set<String> allColumnNames = new HashSet<String>();
        for (int i = tables.size() - 1; i >= 0; --i) {
            TABLE table = tables.get(i);
            for (String column: getAndWaitForColumns(table)) {
                allColumnNames.add(Quoting.normalizeIdentifier(column));
            }
        }
        StringBuilder sb = new StringBuilder();
        for (int i = tables.size() - 1; i >= 0; --i) {
        	TABLE table = tables.get(i);
            for (String column: getAndWaitForColumns(table)) {
                for (;;) {
                    String suffix = "";
                    String unquotedColumn = Quoting.normalizeIdentifier(column);
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
                    String unquotedColumnWithSuffix = Quoting.normalizeIdentifier(columnWithSuffix);
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
                        if (tableNames.size() > 1 || tableNames.size() == 1 && !tableNames.get(0).equals(getTableName(table))) {
                            sb.append(tableNames.get(i) + ".");
                        }
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
    
    private List<SQLCompletion> tableCompletions(TABLE context) {
        List<SQLCompletion> newCompletions = new ArrayList<SQLCompletion>();
        if (context != null) {
            for (String column: getAndWaitForColumns(context)) {
                newCompletions.add(new SQLCompletion(this, Quoting.staticUnquote(column), getQuoting() == null? column : getQuoting().quote(column), 
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
                    newCompletions.add(new SQLCompletion(this, Quoting.staticUnquote(tableName), getQuoting() == null? tableName : getQuoting().quote(tableName),
                            getSchemaName(schema), SQLCompletion.COLOR_TABLE));
                }
            }
        }
        return newCompletions;
    }

    private static String reduceStatement(String statement, int caretPos) {
        StringBuilder sb = new StringBuilder(removeCommentsAndLiterals(statement));
        
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

    /**
     * Removes comments and literals from SQL statement.
     * 
     * @param statement the statement
     * 
     * @return statement the statement without comments and literals
     */
    public static String removeCommentsAndLiterals(String statement) {
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
        return sb.toString();
    }

    public Map<String, TABLE> findAliases(String statement, Map<String, TABLE> aliasesOnTopLevel, List<OutlineInfo> outlineInfos) {
        final int MAX_OUTLINE_INFOS = 500;
        Map<String, String> scopeDescriptionPerLastKeyword = new HashMap<String, String>();
        
        statement = prepareStatementForAliasAnalysis(statement);
        
        scopeDescriptionPerLastKeyword.put("select", "Select");
        scopeDescriptionPerLastKeyword.put("from", "From");
        scopeDescriptionPerLastKeyword.put("with", "With");
        scopeDescriptionPerLastKeyword.put("where", "Where");
        scopeDescriptionPerLastKeyword.put("group", "Group by");
        scopeDescriptionPerLastKeyword.put("order", "Order by");
        scopeDescriptionPerLastKeyword.put("having", "Having");
        scopeDescriptionPerLastKeyword.put("update", "Update");
        scopeDescriptionPerLastKeyword.put("into", "into");
        scopeDescriptionPerLastKeyword.put("delete", "Delete");
        scopeDescriptionPerLastKeyword.put("insert", "Insert");
        scopeDescriptionPerLastKeyword.put("union", "Union");
        scopeDescriptionPerLastKeyword.put("values", "Values");
        scopeDescriptionPerLastKeyword.put("merge", "Merge");
        
        HashSet<String> scopesWithContext = new HashSet<String>();
        scopesWithContext.add("select");
        scopesWithContext.add("where");
        scopesWithContext.add("group");
        scopesWithContext.add("order");
        scopesWithContext.add("having");
        scopesWithContext.add("values");
        
        Map<String, TABLE> aliases = new LinkedHashMap<String, TABLE>();
        Pattern pattern = Pattern.compile("(?:\\bas\\b)|(?:[&][\\w]+\\s*\\.\\.)|(" + reClauseKW + ")|(,|\\(|\\)|=|<|>|!|\\.|\\b(?:on|where|left|right|full|inner|outer|cross|join|and|or|not|set)\\b)|(" + reIdentifier + ")", Pattern.DOTALL|Pattern.CASE_INSENSITIVE);
        Matcher matcher = pattern.matcher(statement + ")");
        boolean inFrom = false;
        boolean inWith = false;
        boolean cteExpected = false;
        boolean firstCTE = true;
        boolean isSubselect = false;
        boolean wasSubselect = false;
        int level = 0;
        boolean isNewScope = true;
        Map<String, Integer> levelPerAlias = new LinkedHashMap<String, Integer>();
        Map<String, Boolean> isStarRelevantPerAlias = new HashMap<String, Boolean>();
        Stack<String> tokenStack = new Stack<String>();
        Stack<Integer> tokenPosStack = new Stack<Integer>();
		Stack<Boolean> inWithStack = new Stack<Boolean>();
		Stack<Boolean> isSubselectStack = new Stack<Boolean>();
		Stack<Boolean> inFromStack = new Stack<Boolean>();
		Set<String> ctes = new HashSet<String>();
		Set<String> inferedTables = new HashSet<String>();
		String prevClauseLC = null;
        int nextInsertPos = -1;
        int scopeBeginn = 0;
        int beginEndCount = 0;
        boolean result = matcher.find();
        StringBuffer head = new StringBuffer();
        boolean nextIsStarRelevant = true;
        if (result) {
            do {
                String clause = matcher.group(1);
                String clauseLC = clause == null? null : clause.toLowerCase(Locale.ENGLISH);
                boolean isStarRelevant = nextIsStarRelevant;
                String keyword = matcher.group(2);
                String identifier = matcher.group(3);

                if (prevClauseLC != null && identifier != null) {
                	if ("from".equals(prevClauseLC) || "join".equals(prevClauseLC) || "into".equals(prevClauseLC) || "update".equals(prevClauseLC)) {
                		inferedTables.add(Quoting.normalizeIdentifier(identifier));
                	}
                }
                
                head.setLength(0);
        		matcher.appendReplacement(head, "");

                if (clause != null) {
                	// remove "by"
                	if (clauseLC.startsWith("order")) {
                		clauseLC = "order";
                		clause = clauseLC;
                	} else if (clauseLC.startsWith("group")) {
                		clauseLC = "group";
                		clause = clauseLC;
                	} 
                	nextIsStarRelevant = true;
                	boolean notDmlKW = !"update".equals(clauseLC) && !"into".equals(clauseLC);
					if (!"from".equals(clauseLC) && notDmlKW) {
                        keyword = clause;
                    }
					if (!notDmlKW) {
                    	nextIsStarRelevant = false;
                    }
                }
                
				if (isNewScope && "select".equalsIgnoreCase(keyword)) {
                	isSubselect = true;
                	if (outlineInfos != null && outlineInfos.size() > 0) {
                		OutlineInfo info = outlineInfos.get(outlineInfos.size() - 1);
                		if (info.withContext) {
                			if (info.contextEnd <= 0 || info.contextEnd > scopeBeginn) {
                				info.contextEnd = scopeBeginn;
                			}
                		}
                	}
                } else if (keyword != null || clause != null || identifier != null) {
                	isNewScope = false;
                }

                if (clause != null && outlineInfos != null) {
					String scopeDescription = scopeDescriptionPerLastKeyword.get(clauseLC);
                    if (scopeDescription != null) {
                    	if (isSubselect || (!"group".equals(clauseLC) && !"order".equals(clauseLC))) {
	                        int pos = matcher.start();
	                        nextInsertPos = tokenStack.isEmpty()? -1 : outlineInfos.size();
	                        OutlineInfo info = new OutlineInfo(null, null, level, pos, scopeDescription);
	                        info.withContext = scopesWithContext.contains(clauseLC);
	                        info.contextPosition = matcher.end();
							outlineInfos.add(info);
                    	}
                    }
                }
                
                if (wasSubselect && identifier != null) {
                	if (outlineInfos != null) {
                		if (inFrom && head.toString().trim().isEmpty() && !Quoting.UCSQL2003KEYWORDS.contains(identifier.toUpperCase(Locale.ENGLISH))) {
	                        int pos = matcher.start();
	                        nextInsertPos = tokenStack.isEmpty()? -1 : outlineInfos.size();
	                        OutlineInfo info = new OutlineInfo(null, identifier, level, pos, "");
	                        info.isCTE = true;
	                        if (outlineInfos.size() > 0 && outlineInfos.get(outlineInfos.size() - 1).isEnd) {
	                        	outlineInfos.add(outlineInfos.size() - 1, info);
	                        	info.level++;
	                        } else {
	                        	outlineInfos.add(info);
	                        }
                		}
                	}
                }

                if (inWith && outlineInfos != null) {
                	if (",".equals(keyword)) {
                		cteExpected = true;
                		firstCTE = false;
                	} else {
                		if (cteExpected && identifier != null) {
                			ctes.add(Quoting.normalizeIdentifier(identifier));
                            int pos = matcher.start();
                            nextInsertPos = tokenStack.isEmpty()? -1 : outlineInfos.size();
                            OutlineInfo info = new OutlineInfo(null, "", level, pos, identifier);
                            info.isCTE = true;
                            info.withSeparator = !firstCTE;
                            outlineInfos.add(info);
                		}
                		cteExpected = false;
                	}
                }
                if ("with".equalsIgnoreCase(keyword)) {
                	inWith = true;
                	cteExpected = true;
                	firstCTE = true;
                }

                boolean clear = false;
                if (inFrom) {
                    if (keyword != null) {
                        if (keyword.equals(".")) {
                            tokenStack.push(keyword);
                            tokenPosStack.push(matcher.start());
                        } else {
                            clear = true;
                        }
                    } else if (identifier != null) {
                        tokenStack.push(identifier);
                        tokenPosStack.push(matcher.start());
                    }
                    if (!clear && !tokenStack.isEmpty() && !".".equals(tokenStack.peek())) {
                        ArrayList<String> tokens = new ArrayList<String>(tokenStack);
                        String schema = null;
                        String table = null;
                        String alias = null;
                        int pos = 0;
                        if (tokens.size() >= 4) {
                            if (tokens.get(tokens.size() - 3).equals(".")) {
                                alias = tokens.get(tokens.size() - 1);
                                table = tokens.get(tokens.size() - 2);
                                schema = tokens.get(tokens.size() - 4);
                                pos = tokenPosStack.get(tokenPosStack.size() - 4);
                            }
                        } else if (tokens.size() >= 2) {
                            if (!tokens.get(tokens.size() - 2).equals(".")) {
                                alias = tokens.get(tokens.size() - 1);
                                table = tokens.get(tokens.size() - 2);
                                pos = tokenPosStack.get(tokenPosStack.size() - 2);
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
                                if (mdTable == null && inferedTables.contains(Quoting.normalizeIdentifier(table))) {
                                	mdTable = createDummyTable(mdSchema, table);
                                }
                                if (mdTable != null || "dual".equalsIgnoreCase(table) || ctes.contains(Quoting.normalizeIdentifier(table))) {
                                    if (outlineInfos != null && (mdTable == null || mdTable instanceof MDTable)) {
                                    	OutlineInfo info = new OutlineInfo((MDTable) mdTable, alias, level, pos, mdTable == null? table : null);
                                    	info.isCTE = mdTable == null;
                                        outlineInfos.add(nextInsertPos >= 0? nextInsertPos : outlineInfos.size(), info);
                                        mergeOutlineInfos(outlineInfos, (nextInsertPos >= 0? nextInsertPos : (outlineInfos.size() - 1)) + 1);
                                        tokenStack.clear();
                                    }
                                    if (mdTable != null) {
	                                    Integer prevLevel = levelPerAlias.get(alias);
	                                    isStarRelevantPerAlias.put(alias, isStarRelevant);
	                                    if (prevLevel == null || prevLevel < level) {
	                                        aliases.put(alias, mdTable);
	                                        levelPerAlias.put(alias, level);
	                                        tokenStack.clear();
	                                    }
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
                        int pos = 0;
                        if (tokens.size() >= 3) {
                            if (tokens.get(tokens.size() - 2).equals(".")) {
                                table = tokens.get(tokens.size() - 1);
                                alias = table;
                                schema = tokens.get(tokens.size() - 3);
                                pos = tokenPosStack.get(tokenPosStack.size() - 3);
                            }
                        } else if (tokens.size() >= 1) {
                            if (!tokens.get(tokens.size() - 1).equals(".")) {
                                table = tokens.get(tokens.size() - 1);
                                pos = tokenPosStack.get(tokenPosStack.size() - 1);
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
                                if (mdTable == null && inferedTables.contains(Quoting.normalizeIdentifier(table))) {
                                	mdTable = createDummyTable(mdSchema, table);
                                }
                                if (mdTable != null || "dual".equalsIgnoreCase(table) || ctes.contains(Quoting.normalizeIdentifier(table))) {
                                    if (outlineInfos != null) {
                                        if (mdTable == null || mdTable instanceof MDTable) {
                                            OutlineInfo info = new OutlineInfo((MDTable) mdTable, null, level, pos, mdTable == null? table : null);
                                            info.isCTE = mdTable == null;
											outlineInfos.add(nextInsertPos >= 0? nextInsertPos : outlineInfos.size(), info);
                                            mergeOutlineInfos(outlineInfos, (nextInsertPos >= 0? nextInsertPos : (outlineInfos.size() - 1)) + 1);
	                                        tokenStack.clear();
                                        }
                                    }
                                    if (mdTable != null) {
	                                    Integer prevLevel = levelPerAlias.get(alias);
	                                    if (prevLevel == null || prevLevel < level) {
	                                        aliases.put(alias, mdTable);
	                                        isStarRelevantPerAlias.put(alias, isStarRelevant);
		                                    levelPerAlias.put(alias, level);
	                                        tokenStack.clear();
	                                    }
                                    }
                                }
                            }
                        }
                        tokenStack.clear();
                        tokenPosStack.clear();
                    }
                }
                if (clause != null) {
                    inFrom = "from".equalsIgnoreCase(clause) || "update".equalsIgnoreCase(clause) || "into".equalsIgnoreCase(clause);
                    inWith = "with".equalsIgnoreCase(clause);
                    clear = true;
                }
                if (keyword != null || clause != null || identifier != null) {
                	wasSubselect = false;
                }
                if (keyword != null) {
                    if ("(".equals(keyword)) {
                        ++level;
                        if (outlineInfos != null) {
                        	OutlineInfo info = new OutlineInfo(null, null, level, matcher.start(), keyword);
                        	info.isBegin = true;
							outlineInfos.add(info);
							++beginEndCount;
                        }
                        inWithStack.push(inWith);
                        inFromStack.push(inFrom);
                        isSubselectStack.push(isSubselect);
                        inWith = false;
                        inFrom = false;
                        isSubselect = false;
                        isNewScope = true;
                        scopeBeginn = matcher.start();
                    } else if (")".equals(keyword)) {
                        if (outlineInfos != null) {
	                        if (outlineInfos.size() > 0) {
	                        	OutlineInfo info = outlineInfos.get(outlineInfos.size() - 1);
	                        	if (info.level > level - 1) {
	                        		if (info.contextEnd == 0) {
	                        			info.contextEnd = matcher.start();
	                        		}
	                        	}
	                        }
                        	if (outlineInfos.size() > 0 && outlineInfos.get(outlineInfos.size() - 1).isBegin) {
    							outlineInfos.remove(outlineInfos.size() - 1);
                        		--beginEndCount;
                        	} else {
	                        	OutlineInfo info = new OutlineInfo(null, null, level, matcher.start(), keyword);
	                        	info.isEnd = true;
								outlineInfos.add(info);
								++beginEndCount;
                        	}
                        }
                        --level;
                        if (level == 0) {
                        	isNewScope = true;
                        }
                        wasSubselect = isSubselect;
                        inWith = false;
                        inFrom = false;
                        isSubselect = false;
                        if (!inWithStack.isEmpty()) {
                        	inWith = inWithStack.pop();
                        }
                        if (!inFromStack.isEmpty()) {
                        	inFrom = inFromStack.pop();
                        }
                        if (!isSubselectStack.isEmpty()) {
                        	isSubselect = isSubselectStack.pop();
                        }
                        nextInsertPos = -1;
                    }
                }

                if (outlineInfos != null) {
                    mergeOutlineInfos(outlineInfos, outlineInfos.size());
                    
                    if (outlineInfos.size() - beginEndCount > MAX_OUTLINE_INFOS) {
                        ArrayList<OutlineInfo> reducedOutlineInfos = new ArrayList<OutlineInfo>(outlineInfos.subList(0, MAX_OUTLINE_INFOS));
                        reducedOutlineInfos.add(new OutlineInfo(null, null, 0, reducedOutlineInfos.get(reducedOutlineInfos.size() - 1).position, "more..."));
                        outlineInfos.clear();
                        outlineInfos.addAll(reducedOutlineInfos);
                        break;
                    }
                }
                
                prevClauseLC = clauseLC != null? clauseLC : keyword != null? keyword.toLowerCase(Locale.ENGLISH) : null;
                result = matcher.find();
            } while (result);
        }
        int maxLevel = 0;
        for (Integer l: levelPerAlias.values()) {
            if (l > maxLevel) {
                maxLevel = l;
            }
        }
        for (Entry<String, Integer> e: levelPerAlias.entrySet()) {
            if (e.getValue() == maxLevel) {
                if (aliasesOnTopLevel != null && Boolean.TRUE.equals(isStarRelevantPerAlias.get(e.getKey()))) {
                    aliasesOnTopLevel.put(e.getKey(), aliases.get(e.getKey()));
                }
            }
        }
        return aliases;
    }

    protected String prepareStatementForAliasAnalysis(String statement) {
		return statement;
	}

	public void mergeOutlineInfos(List<OutlineInfo> outlineInfos, int endIndex) {
        // merge "select from dual"
        if (outlineInfos != null && endIndex >= 2 
                && "from".equalsIgnoreCase(outlineInfos.get(endIndex - 2).scopeDescriptor) 
                && "dual".equalsIgnoreCase(outlineInfos.get(endIndex - 1).scopeDescriptor)) {
            int infoLevel = outlineInfos.get(endIndex - 1).level;
            if (infoLevel == outlineInfos.get(endIndex - 2).level) {
                int pos;
                String description;
                pos = outlineInfos.get(endIndex - 2).position;
                description = "from " + outlineInfos.get(endIndex - 1).scopeDescriptor;
                outlineInfos.remove(endIndex - 1);
                --endIndex;
                outlineInfos.remove(endIndex - 1);
                --endIndex;
                outlineInfos.add(endIndex++, new OutlineInfo(null, null, infoLevel, pos, description));
            }
        }
        
        // merge "select from dual union select from dual"
        if (outlineInfos != null && endIndex >= 5
                && "select".equalsIgnoreCase(outlineInfos.get(endIndex - 5).scopeDescriptor) 
                && "from dual".equalsIgnoreCase(outlineInfos.get(endIndex - 4).scopeDescriptor)
                && "union".equalsIgnoreCase(outlineInfos.get(endIndex - 3).scopeDescriptor) 
                && "select".equalsIgnoreCase(outlineInfos.get(endIndex - 2).scopeDescriptor) 
                && "from dual".equalsIgnoreCase(outlineInfos.get(endIndex - 1).scopeDescriptor)) {
            int infoLevel = outlineInfos.get(endIndex - 1).level;
            if (infoLevel == outlineInfos.get(endIndex - 2).level
            	&& infoLevel == outlineInfos.get(endIndex - 3).level
            	&& infoLevel == outlineInfos.get(endIndex - 4).level
            	&& infoLevel == outlineInfos.get(endIndex - 5).level
        		) {
                int pos;
                String description;
                pos = outlineInfos.get(endIndex - 5).position;
                description = "Select " + outlineInfos.get(endIndex - 1).scopeDescriptor;
                OutlineInfo info = new OutlineInfo(null, null, infoLevel, pos, description);
                info.rowCount = 2;
                info.contextPosition = outlineInfos.get(endIndex - 5).contextPosition;
                info.withContext = outlineInfos.get(endIndex - 5).withContext;
                outlineInfos.remove(endIndex - 1);
                --endIndex;
                outlineInfos.remove(endIndex - 1);
                --endIndex;
                outlineInfos.remove(endIndex - 1);
                --endIndex;
                outlineInfos.remove(endIndex - 1);
                --endIndex;
                outlineInfos.remove(endIndex - 1);
                --endIndex;
				outlineInfos.add(endIndex++, info);
            }
        }
        
        // merge "select from dual (n rows) union select from dual"
        if (outlineInfos != null && endIndex >= 4
                && "select from dual".equalsIgnoreCase(outlineInfos.get(endIndex - 4).scopeDescriptor) 
                && "union".equalsIgnoreCase(outlineInfos.get(endIndex - 3).scopeDescriptor) 
                && "select".equalsIgnoreCase(outlineInfos.get(endIndex - 2).scopeDescriptor) 
                && "from dual".equalsIgnoreCase(outlineInfos.get(endIndex - 1).scopeDescriptor)) {
            int infoLevel = outlineInfos.get(endIndex - 1).level;
            if (infoLevel == outlineInfos.get(endIndex - 2).level
            	&& infoLevel == outlineInfos.get(endIndex - 3).level
            	&& infoLevel == outlineInfos.get(endIndex - 4).level
        		) {
                int pos;
                String description;
                pos = outlineInfos.get(endIndex - 4).position;
                description = outlineInfos.get(endIndex - 4).scopeDescriptor;
                OutlineInfo info = new OutlineInfo(null, null, infoLevel, pos, description);
                info.rowCount = outlineInfos.get(endIndex - 4).rowCount + 1;
                info.contextPosition = outlineInfos.get(endIndex - 4).contextPosition;
                info.withContext = outlineInfos.get(endIndex - 4).withContext;
                outlineInfos.remove(endIndex - 1);
                --endIndex;
                outlineInfos.remove(endIndex - 1);
                --endIndex;
                outlineInfos.remove(endIndex - 1);
                --endIndex;
                outlineInfos.remove(endIndex - 1);
                --endIndex;
				outlineInfos.add(endIndex++, info);
            }
        }
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
    }

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
    private static String reClauseKW = "\\b(?:select|from|update|where|(?:group\\s+by)|having|with|in|exists|into|delete|insert|(?:order\\s+by)|union|values|merge)\\b";
    private static String reIdentWSWordPattern = ".*?(" + reIdentifier + ")\\s+\\w*$";
    
    private static Pattern identDotOnlyPattern = Pattern.compile(reIdentDotOnly, Pattern.DOTALL);
    private static Pattern identWSWordPattern = Pattern.compile(reIdentWSWordPattern, Pattern.DOTALL);

    private TABLE findAlias(String aliasName) {
        TABLE context;
        context = null;
        for (Entry<String, TABLE> entry: aliases.entrySet()) {
            if (Quoting.normalizeIdentifier(entry.getKey()).equals(Quoting.normalizeIdentifier(aliasName))) {
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

	/**
	 * A segment to use for fast char access.
	 */
	private Segment s = new Segment();

	@Override
	public boolean isAutoActivateOkay(JTextComponent tc) {
		Document doc = tc.getDocument();
		String text = "";
		try {
			int caretPosition = tc.getCaretPosition();
			int o = Math.min(caretPosition, 8);
			doc.getText(caretPosition - o, 1 + o, s);
			text = s.toString();
		} catch (BadLocationException ble) { // Never happens
			ble.printStackTrace();
		}
		return text.matches("(?is).*(\\w|((,|join|on|select|where|from|into)\\s?)|\\*|\\.)");
	}

	// TODO improve "join" auto completion
	
    protected abstract List<String> getColumns(TABLE table, long timeOut, JComponent waitCursorSubject);
    protected abstract SCHEMA getDefaultSchema(SOURCE metaDataSource);
    protected abstract SCHEMA findSchema(SOURCE metaDataSource, String name);
    protected abstract TABLE findTable(SCHEMA schema, String name);
    protected TABLE createDummyTable(SCHEMA schema, String name) {
    	return null;
    }
    protected abstract String getTableName(TABLE table);
    protected abstract List<TABLE> getTables(SCHEMA schema);
    protected abstract String getSchemaName(SCHEMA schema);
    protected abstract List<SCHEMA> getSchemas(SOURCE metaDataSource);
    protected abstract List<Association> getAssociations(TABLE source, TABLE destination);
	protected abstract boolean isInitialized();

}
