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

package net.sf.jailer.database;

import java.io.BufferedReader;
import java.io.InputStreamReader;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.sf.jailer.ExecutionContext;

/**
 * Renews the DB table statistics for the working-tables
 * by executing a shell-script.
 * 
 * @author Ralf Wisser
 */
public class ShellScriptBasedStatisticRenovator implements StatisticRenovator {

	/**
	 * Invocation of the script-file.
	 */
	private final String scriptInvocation;
	
	/**
	 * The logger.
	 */
	private static final Logger _log = LoggerFactory.getLogger(ShellScriptBasedStatisticRenovator.class);
	
	/**
	 * Constructor.
	 * 
	 * @param scriptInvocation invocation of the script-file
	 */
	public ShellScriptBasedStatisticRenovator(String scriptInvocation) {
		this.scriptInvocation = scriptInvocation;
	}
	
	/**
	 * Renews the DB table statistics for the working-tables
	 * by executing the shell-script.
	 * 
	 * @param session for execution of SQL-statements
	 */
	@Override
	public void renew(Session session, ExecutionContext executionContext) throws Exception {
		String theScriptInvocation = getScriptInvocation(session);
		_log.info("executing: " + theScriptInvocation);
		Process process = Runtime.getRuntime().exec(theScriptInvocation);
		process.waitFor();
		BufferedReader input = new BufferedReader(new InputStreamReader(process.getInputStream())); // lgtm [java/input-resource-leak]
		String line;
		while ((line = input.readLine()) != null) {
			_log.info("+ " + line);
		}
		_log.info("exit value = " + process.exitValue());
	}

	/**
	 * Gets shell-invocation.
	 * 
	 * @param session for execution of SQL-statements
	 * @return shell-invocation
	 */
	protected String getScriptInvocation(Session session) throws Exception {
		return scriptInvocation;
	}

}
