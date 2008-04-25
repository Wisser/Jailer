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

import java.io.BufferedReader;
import java.io.InputStreamReader;

import org.apache.log4j.Logger;

/**
 * Renews the DB table statistics for the working-tables
 * by executing a shell-script.
 * 
 * @author Ralf Wisser
 */
public class ShellScriptBasedStatisticRenovator implements StatisticRenovator {

    /**
     * DB-URL pattern for this renovator.
     */
	private final String urlPattern;
	
    /**
     * Invocation of the script-file.
     */
    private final String scriptInvocation;
    
    /**
     * The logger.
     */
    private static final Logger _log = Logger.getLogger(ShellScriptBasedStatisticRenovator.class);
    
    /**
     * Constructor.
     * 
     * @param urlPattern DB-URL pattern for this renovator
     * @param scriptInvocation invocation of the script-file
     */
    public ShellScriptBasedStatisticRenovator(String urlPattern, String scriptInvocation) {
    	this.urlPattern = urlPattern;
    	this.scriptInvocation = scriptInvocation;
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
     * by executing the shell-script.
     * 
     * @param statementExecutor for execution of SQL-statements
     */
    public void renew(StatementExecutor statementExecutor) throws Exception {
        String theScriptInvocation = getScriptInvocation(statementExecutor);
        _log.info("executing: " + theScriptInvocation);
        Process process = Runtime.getRuntime().exec(theScriptInvocation);
        process.waitFor();
        BufferedReader input = new BufferedReader(new InputStreamReader(process.getInputStream()));
        String line;
        while ((line = input.readLine()) != null) {
            _log.info("+ " + line);
        }
        _log.info("exit value = " + process.exitValue());
    }

    /**
     * Gets shell-invocation.
     * 
     * @param statementExecutor for execution of SQL-statements
     * @return shell-invocation
     */
    protected String getScriptInvocation(StatementExecutor statementExecutor) throws Exception {
        return scriptInvocation;
    }

}
