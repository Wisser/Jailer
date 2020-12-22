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
import net.sf.jsqlparser.parser.ParseException;
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
        CCJSqlParser parser = createSQLParser(simplifiedSql).withSquareBracketQuotation(false);
		try {
		    return parser.Statement();
		} catch (Exception e) {
			if (simplifiedSql.contains("[")) {
				parser = createSQLParser(simplifiedSql).withSquareBracketQuotation(true);
				try {
				    return parser.Statement();
				} catch (Exception e2) {
				    // ignore
				}
			}
		    throw new JSQLParserException(e);
		}
    }

    /**
     * Workaround for https://github.com/JSQLParser/JSqlParser/issues/1013
     * <br>
     * Creates a parser that throws an exception with a simplified error message in case of an error.
     * This avoids calling the CCJSqlParser#jj_rescan_token() method, which is extremely inperformant
     * in some cases.
     * <br>
     * In the error message the "Was expecting: ..." part is therefore missing.
     *
     * @param sqlStatement the statement
	 * @return parsed statement
	 *
	 * @see https://github.com/JSQLParser/JSqlParser/issues/1013
     */
    private static CCJSqlParser createSQLParser(String sqlStatement) {
    	return new CCJSqlParser(new StringProvider(sqlStatement)) {
    		@Override
    		public ParseException generateParseException() {
    			int[][] exptokseq = new int[1][];
    			exptokseq[0] = new int[] { 0 }; // EOF
				ParseException parseException = new ParseException(token, exptokseq, tokenImage, null);
				return new ParseException(parseException.getMessage().replaceFirst("(?s)Was expecting(:|(one of:)).*", "").trim());
    		}
    	};
    }
}
