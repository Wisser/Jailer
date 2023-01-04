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
package net.sf.jailer.modelbuilder;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.sf.jailer.util.Quoting;

/**
 * Maps normalized form of known unique identifiers (table names and column names)
 * to the the identifiers.
 * 
 * @author Wisser
 */
public class KnownIdentifierMap {

	/**
	 * The map.
	 */
	private final Map<String, String> knownUniqueIdentifiers = new HashMap<String, String>();

	/**
	 * Puts normalized table name into the map.
	 * 
	 * @param tableName the table name
	 */
	public void putTableName(String tableName) {
		put(normalizeTableName(tableName), tableName);
	}

	/**
	 * Returns previously put non-normalized unique table name.
	 * 
	 * @param tableName the table name
	 * @return previously put non-normalized unique table name or <code>null</code>, if non exists
	 */
	public String getTableName(String tableName) {
		return get(normalizeTableName(tableName));
	}

	/**
	 * Puts normalized column name into the map.
	 * 
	 * @param tableName the column's table name
	 * @param columnName the column name
	 */
	public void putColumnName(String tableName, String columnName) {
		put(normalizeColumnName(tableName, columnName), columnName);
	}

	/**
	 * Returns previously put non-normalized unique column name.
	 * 
	 * @param tableName the column's table name
	 * @param columnName the column name
	 * @return previously put non-normalized unique table name or <code>null</code>, if non exists
	 */
	public String getColumnName(String tableName, String columnName) {
		return get(normalizeColumnName(tableName, columnName));
	}

	/**
	 * Puts normalized join condition into the map.
	 * 
	 * @param condition the join condition
	 */
	public void putCondition(String condition) {
		put(normalizeCondition(condition), condition);
	}

	/**
	 * Returns previously put non-normalized unique join condition.
	 * 
	 * @param condition the join condition
	 * @return previously put non-normalized unique table name or <code>null</code>, if non exists
	 */
	public String getCondition(String condition) {
		return get(normalizeCondition(condition));
	}

	private String normalizeTableName(String tableName) {
		return "T" + Quoting.normalizeIdentifier(tableName);
	}

	private String normalizeColumnName(String tableName, String columnName) {
		return "C" + Quoting.normalizeIdentifier(tableName) + "(.)" + Quoting.normalizeIdentifier(columnName);
	}

	private Pattern columnComparisionPattern = Pattern.compile("(A|a|B|b)( *\\. *)((?:[\"][^\"]+[\"])|(?:[`][^`]+[`])|(?:['][^']+['])|(?:[\\w]+))");

	private String normalizeCondition(String condition) {
		Matcher m = columnComparisionPattern.matcher(condition.replaceAll("\\)|\\(", ""));
        boolean result = m.find();
        if (result) {
            StringBuffer sb = new StringBuffer();
            do {
                m.appendReplacement(sb, Matcher.quoteReplacement(m.group(1).toUpperCase(Locale.ENGLISH) + m.group(2) + Quoting.normalizeIdentifier(m.group(3))));
                result = m.find();
            } while (result);
            m.appendTail(sb);
            condition = sb.toString();
        }
    
		return "J" + condition;
	}

	private final static String NOT_UNIQUE = new String("not unique");
	
	private void put(String normalizedName, String name) {
		if (knownUniqueIdentifiers.get(normalizedName) != NOT_UNIQUE) {
			String oldName = knownUniqueIdentifiers.put(normalizedName, name);
			if (oldName != null && !oldName.equals(name)){
				knownUniqueIdentifiers.put(normalizedName, NOT_UNIQUE);
			}
		}
	}

	private String get(String normalizedName) {
		String name = knownUniqueIdentifiers.get(normalizedName);
		if (name == NOT_UNIQUE) {
			return null;
		}
		return name;
	}

}
