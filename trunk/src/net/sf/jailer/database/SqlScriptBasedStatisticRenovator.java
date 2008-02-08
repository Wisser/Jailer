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

package net.sf.jailer.database;

import net.sf.jailer.util.SqlScriptExecutor;

/**
 * Renews the statistics for the working-tables
 * by executing a SQL-script.
 * 
 * @author Wisser
 */
public class SqlScriptBasedStatisticRenovator implements StatisticRenovator {

	/**
     * DB-URL pattern for this renovator.
     */
	private final String urlPattern;
	
	/**
     * Name of SQL-script file.
     */
    private final String scriptFileName;
    
    /**
     * Constructor.
     * 
     * @param urlPattern DB-URL pattern for this renovator
     * @param scriptFileName name of SQL-script file
     */
    public SqlScriptBasedStatisticRenovator(String urlPattern, String scriptFileName) {
    	this.urlPattern = urlPattern;
        this.scriptFileName = scriptFileName;
    }

	/**
     * Gets DB-URL pattern for this renovator.
     * 
     * @return DB-URL pattern for this renovator
     */
	public String getUrlPattern() {
		return urlPattern;
	}

    /**
     * Renews the DB table statistics for the working-tables
     * by executing the SQL-script.
     * 
     * @param statementExecutor for execution of SQL-statements
     */
    public void renew(StatementExecutor statementExecutor) throws Exception {
        SqlScriptExecutor.executeScript(scriptFileName, statementExecutor);
    }

}
