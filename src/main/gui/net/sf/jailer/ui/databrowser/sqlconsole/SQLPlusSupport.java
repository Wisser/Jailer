/*
 * Copyright 2007 - 2022 Ralf Wisser.
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

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.sf.jailer.modelbuilder.MemorizedResultSet;

/**
 * Supports some Oracle SQL+ statements.
 *
 * @author Ralf Wisser
 */
public class SQLPlusSupport {

	/**
	 * The logger.
	 */
	private static final Logger logger = LoggerFactory.getLogger(SQLPlusSupport.class);
	
	/**
	 * The variables.
	 */
	private static Map<String, String> variables = Collections.synchronizedMap(new TreeMap<String, String>());

	/**
	 * Column substitutions.
	 */
	private Map<String, String[]> columnSubstitutions = new TreeMap<String, String[]>();
	
	private final Pattern DEFINE_PATTERN = Pattern.compile("\\s*DEFINE\\s+(\\w+)\\s*=\\s*(.*)\\s*", Pattern.CASE_INSENSITIVE);
	private final Pattern UNDEFINE_PATTERN = Pattern.compile("\\s*UNDEFINE\\s+((?:\\w+\\s*)+)", Pattern.CASE_INSENSITIVE); // lgtm [java/redos]
	private final Pattern COLUMN_PATTERN = Pattern.compile("\\s*COLUMN\\s+(\\w+)\\s*((?:(?:new_value|old_value)\\s+\\w+\\s*)+)", Pattern.CASE_INSENSITIVE);
	private final Pattern COMMENTS_PATTERN = Pattern.compile("(/\\*.*?\\*/)|(\\-\\-.*?(\n|$))", Pattern.DOTALL);
	
	/**
	 * Executes a SQLPlus statement.
	 * 
	 * @param statement the statement
	 * @return <code>true</code> if statement is a valid SQLPlus statement
	 */
	public boolean executeSQLPLusStatement(String statement) {
		statement = removeComments(statement).trim();
		Matcher matcher = DEFINE_PATTERN.matcher(statement);
		if (matcher.matches()) {
			String var = matcher.group(1);
			String val = matcher.group(2).trim();
			if (val.length() > 1 && val.startsWith("\"") && val.endsWith("\"")) {
				val = val.substring(1, val.length() - 1);
			}
			variables.put(var.toUpperCase(Locale.ENGLISH), val);
			logger.info("DEFINE " + var + " = " + val);
			return true;
		}
		matcher = UNDEFINE_PATTERN.matcher(statement);
		if (matcher.matches()) {
			for (String var: matcher.group(1).split("\\s+")) {
				variables.remove(var.toUpperCase(Locale.ENGLISH));
			}
			return true;
		}
		matcher = COLUMN_PATTERN.matcher(statement);
		if (matcher.matches()) {
			String column = matcher.group(1);
			String[] variables = matcher.group(2).split("(?i:\\s*(new_value|old_value)\\s*)");
			columnSubstitutions.put(column.toUpperCase(Locale.ENGLISH), variables);
			return true;
		}
		return false;
	}

	/**
	 * Executes a SQLPlus query.
	 * 
	 * @param query the query
	 * @return <code>true</code> if statement is a valid SQLPlus query
	 */
	public ResultSet executeSQLPLusQuery(String query) {
		if ("DEFINE".equalsIgnoreCase(query.trim())) {
			List<Object[]> rowList = new ArrayList<Object[]>();
			synchronized (variables) {
				for (Entry<String, String> e: variables.entrySet()) {
					if (e.getKey().length() > 0) {
						rowList.add(new Object[] { e.getKey(), e.getValue() });
					}
				}
			}
			return new MemorizedResultSet(rowList, 2, new String[] { "Variable", "Value" }, new int[] { Types.VARCHAR, Types.VARCHAR } );
		}
		return null;
	}

	/**
	 * Replaces all occurrences of "&amp;&lt;var&gt;[.]" with variable values.
	 * 
	 * @param statement the statement
	 * @param positionOffsets map to put position offsets into
	 * @return statement with variable replacements
	 */
	public String replaceVariables(String statement, SortedMap<Integer, Integer> positionOffsets) {
		synchronized (variables) {
			if (!variables.isEmpty()) {
				Matcher matcher = Pattern.compile("&(\\w+)(\\.|\\b)").matcher(statement);
				matcher.reset();
				int offset = 0;
				boolean result = matcher.find();
				if (result) {
					StringBuffer sb = new StringBuffer();
					do {
						String var = matcher.group(1);
						String replacement = variables.get(var.toUpperCase(Locale.ENGLISH));
						if (replacement != null) {
							matcher.appendReplacement(sb,  Matcher.quoteReplacement(replacement));
							if (positionOffsets != null) {
								for (int i = 0; i < replacement.length(); ++i) {
									positionOffsets.put(sb.length() - replacement.length() + i, offset - i);
								}
								offset += matcher.group().length() - replacement.length();
								positionOffsets.put(sb.length(), offset);
							}
						}
						result = matcher.find();
					} while (result);
					matcher.appendTail(sb);
					return sb.toString();
				}
			}
			return statement;
		}
	}

	private Map<Integer, String[]> varsPerIndex = new HashMap<Integer, String[]>();

	public void prepareColumnSubstitution(ResultSetMetaData metaData) throws SQLException {
		varsPerIndex.clear();
		if (!columnSubstitutions.isEmpty()) {
			for (Entry<String, String[]> e: columnSubstitutions.entrySet()) {
				for (int i = 1; i <= metaData.getColumnCount(); ++i) {
					if (e.getKey().equalsIgnoreCase(metaData.getColumnLabel(i))) {
						varsPerIndex.put(i, e.getValue());
					}
				}
			}
		}
	}

	public void substituteColumns(ResultSet resultSet) throws SQLException {
		if (!varsPerIndex.isEmpty()) {
			for (Entry<Integer, String[]> e: varsPerIndex.entrySet()) {
				String value = resultSet.getString(e.getKey());
				for (String var: e.getValue()) {
					variables.put(var.toUpperCase(Locale.ENGLISH), value);
				}
			}
		}
	}

    /**
     * Removes comments SQL statement.
     * 
     * @param statement the statement
     * 
     * @return statement the statement without comments
     */
    public String removeComments(String statement) {
        Pattern pattern = COMMENTS_PATTERN;
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
}
