/*
 * Copyright 2007 - 2020 Ralf Wisser.
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

import net.sf.jsqlparser.JSQLParserException;
import net.sf.jsqlparser.parser.CCJSqlParser;
import net.sf.jsqlparser.parser.StringProvider;
import net.sf.jsqlparser.statement.Statement;

/**
 * SQL parsing.
 *
 * @see https://github.com/JSQLParser/JSqlParser
 */
public final class JSqlParserUtil {

	/**
	 * Parses a SQL statement.
	 *
	 * @param sql the statement
	 * @return parsed statement
	 *
	 * @throws JSQLParserException if JSQLParser is not able to parse the statement
	 */
    public static Statement parse(String sql) throws JSQLParserException {
    	String simplifiedSql = SqlUtil.removeNonMeaningfulFragments(sql);
        CCJSqlParser parser = new CCJSqlParser(new StringProvider(simplifiedSql)).withSquareBracketQuotation(false);;
		try {
		    return parser.Statement();
		} catch (Exception e) {
			if (simplifiedSql.contains("[")) {
				parser = new CCJSqlParser(new StringProvider(simplifiedSql)).withSquareBracketQuotation(true);
				try {
				    return parser.Statement();
				} catch (Exception e2) {
				    // ignore
				}
			}
		    throw new JSQLParserException(e);
		}
    }
}
