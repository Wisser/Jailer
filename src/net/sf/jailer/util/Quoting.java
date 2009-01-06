/*
 * Copyright 2007 the original author or authors.
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
package net.sf.jailer.util;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Quotes and un-quotes SQL identifier in a DBMS specific way.
 * 
 * @author Ralf Wisser
 */
public class Quoting {

	/**
	 * The quote character, null if quoting is not supported.
	 */
	private String quote = null;

	/**
	 * Whether the database treats mixed case unquoted SQL identifiers as case
	 * insensitive and stores them in upper case.
	 */
	private final boolean unquotedIdentifierInUpperCase;

	/**
	 * Whether the database treats mixed case unquoted SQL identifiers as case
	 * insensitive and stores them in mixed case.
	 */
	private final boolean unquotedIdentifierInMixedCase;

	/**
	 * All SQL keywords for each DBMS.
	 */
	private static Map<String, Set<String>> keyWordsMap = new HashMap<String, Set<String>>(); 

	/**
	 * All SQL keywords for this DBMS.
	 */
	private static Set<String> keyWords = new HashSet<String>(); 

	/**
	 * Constructor.
	 * 
	 * @param metaData
	 *            from jdbc driver
	 */
	public Quoting(DatabaseMetaData metaData) throws SQLException {
		String quoteString = metaData.getIdentifierQuoteString();
		if (quoteString != null
				&& (quoteString.equals(" ") || quoteString.equals(""))) {
			quoteString = null;
		}
		try {
			String productName = metaData.getDatabaseProductName();
    		if (productName != null) {
    			if (productName.toUpperCase().contains("ADAPTIVE SERVER")) {
    				// Sybase don't handle quoting correctly
    				quoteString = null;
          		}
    		}
		} catch (Exception e) {
			// ignore
		}
		quote = quoteString;
		unquotedIdentifierInUpperCase = metaData.storesUpperCaseIdentifiers();
		unquotedIdentifierInMixedCase = metaData.storesMixedCaseIdentifiers();
		String k = metaData.getSQLKeywords();
		if (k != null) {
			keyWords = keyWordsMap.get(k);
			if (keyWords == null) {
				keyWords = new HashSet<String>();
				for (String key : k.split(",")) {
					keyWords.add(key.trim().toUpperCase());
				}
				// add all SQL 92 keywords
				keyWords.addAll(Arrays.asList(new String[] { "ABSOLUTE", "ACTION",
					"ADD", "ALL", "ALLOCATE", "ALTER", "AND", "ANY", "ARE", "AS",
					"ASC", "ASSERTION", "AT", "AUTHORIZATION", "AVG", "BEGIN",
					"BETWEEN", "BIT", "BIT_LENGTH", "BOTH", "BY", "CALL",
					"CASCADE", "CASCADED", "CASE", "CAST", "CATALOG", "CHAR",
					"CHARACTER", "CHARACTER_LENGTH", "CHAR_LENGTH", "CHECK",
					"CLOSE", "COALESCE", "COLLATE", "COLLATION", "COLUMN",
					"COMMIT", "CONDITION", "CONNECT", "CONNECTION", "CONSTRAINT",
					"CONSTRAINTS", "CONTAINS", "CONTINUE", "CONVERT",
					"CORRESPONDING", "COUNT", "CREATE", "CROSS", "CURRENT",
					"CURRENT_DATE", "CURRENT_PATH", "CURRENT_TIME",
					"CURRENT_TIMESTAMP", "CURRENT_USER", "CURSOR", "DATE", "DAY",
					"DEALLOCATE", "DEC", "DECIMAL", "DECLARE", "DEFAULT",
					"DEFERRABLE", "DEFERRED", "DELETE", "DESC", "DESCRIBE",
					"DESCRIPTOR", "DETERMINISTIC", "DIAGNOSTICS", "DISCONNECT",
					"DISTINCT", "DO", "DOMAIN", "DOUBLE", "DROP", "ELSE", "ELSEIF",
					"END", "ESCAPE", "EXCEPT", "EXCEPTION", "EXEC", "EXECUTE",
					"EXISTS", "EXIT", "EXTERNAL", "EXTRACT", "FALSE", "FETCH",
					"FIRST", "FLOAT", "FOR", "FOREIGN", "FOUND", "FROM", "FULL",
					"FUNCTION", "GET", "GLOBAL", "GO", "GOTO", "GRANT", "GROUP",
					"HANDLER", "HAVING", "HOUR", "IDENTITY", "IF", "IMMEDIATE",
					"IN", "INDICATOR", "INITIALLY", "INNER", "INOUT", "INPUT",
					"INSENSITIVE", "INSERT", "INT", "INTEGER", "INTERSECT",
					"INTERVAL", "INTO", "IS", "ISOLATION", "JOIN", "KEY",
					"LANGUAGE", "LAST", "LEADING", "LEAVE", "LEFT", "LEVEL",
					"LIKE", "LOCAL", "LOOP", "LOWER", "MATCH", "MAX", "MIN",
					"MINUTE", "MODULE", "MONTH", "NAMES", "NATIONAL", "NATURAL",
					"NCHAR", "NEXT", "NO", "NOT", "NULL", "NULLIF", "NUMERIC",
					"OCTET_LENGTH", "OF", "ON", "ONLY", "OPEN", "OPTION", "OR",
					"ORDER", "OUT", "OUTER", "OUTPUT", "OVERLAPS", "PAD",
					"PARAMETER", "PARTIAL", "PATH", "POSITION", "PRECISION",
					"PREPARE", "PRESERVE", "PRIMARY", "PRIOR", "PRIVILEGES",
					"PROCEDURE", "PUBLIC", "READ", "REAL", "REFERENCES",
					"RELATIVE", "REPEAT", "RESIGNAL", "RESTRICT", "RETURN",
					"RETURNS", "REVOKE", "RIGHT", "ROLLBACK", "ROUTINE", "ROWS",
					"SCHEMA", "SCROLL", "SECOND", "SECTION", "SELECT", "SESSION",
					"SESSION_USER", "SET", "SIGNAL", "SIZE", "SMALLINT", "SOME",
					"SPACE", "SPECIFIC", "SQL", "SQLCODE", "SQLERROR",
					"SQLEXCEPTION", "SQLSTATE", "SQLWARNING", "SUBSTRING", "SUM",
					"SYSTEM_USER", "TABLE", "TEMPORARY", "THEN", "TIME",
					"TIMESTAMP", "TIMEZONE_HOUR", "TIMEZONE_MINUTE", "TO",
					"TRAILING", "TRANSACTION", "TRANSLATE", "TRANSLATION", "TRIM",
					"TRUE", "UNDO", "UNION", "UNIQUE", "UNKNOWN", "UNTIL",
					"UPDATE", "UPPER", "USAGE", "USER", "USING", "VALUE", "VALUES",
					"VARCHAR", "VARYING", "VIEW", "WHEN", "WHENEVER", "WHERE",
					"WHILE", "WITH", "WORK", "WRITE", "YEAR", "ZONE" }));
				keyWordsMap.put(k, keyWords);
			}
		}
	}

	/**
	 * Quotes an identifier.
	 * 
	 * @param identifier
	 *            the identifier
	 * @return quoted identifier
	 */
	public String quote(String identifier) {
		if (identifier != null) {
			identifier = identifier.trim();
		}
		if (quote != null && identifier != null && identifier.length() > 0) {
			if (!keyWords.contains(identifier.toUpperCase())) {
				String lower = "abcdefghijklmnopqrstuvwxyz_0123456789";
				String upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789";
				String digits = "0123456789";
				boolean allUpperCase = true;
				boolean allLowerCase = true;
				boolean allLetters = true;
				for (int i = identifier.length() - 1; i >= 0; --i) {
					char c = identifier.charAt(i);
					if (lower.indexOf(c) < 0) {
						allLowerCase = false;
					}
					if (upper.indexOf(c) < 0) {
						allUpperCase = false;
					}
					if (lower.indexOf(c) < 0 && upper.indexOf(c) < 0 && digits.indexOf(c) < 0) {
						allLetters = false;
					}
				}
				if (digits.indexOf(identifier.charAt(0)) < 0) {
					if (unquotedIdentifierInMixedCase && allLetters) {
						return identifier;
					}
					if (unquotedIdentifierInUpperCase && allUpperCase) {
						return identifier;
					}
					if ((!unquotedIdentifierInUpperCase) && allLowerCase) {
						return identifier;
					}
				}
			}
			return quote + identifier + quote;
		}
		return identifier;
	}

	/**
	 * Remove quotes from identifier.
	 * 
	 * @param identifier the identifier
	 * @return identifier without quotes
	 */
	public String unquote(String identifier) {
		if (quote != null && identifier != null && identifier.startsWith(quote) && identifier.endsWith(quote)) {
			return identifier.substring(quote.length(), identifier.length() - quote.length());
		}
		return identifier;
	}

}
