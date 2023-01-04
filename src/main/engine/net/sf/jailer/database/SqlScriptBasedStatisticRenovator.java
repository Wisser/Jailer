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

import java.io.File;
import java.io.FileOutputStream;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.util.PrintUtil;
import net.sf.jailer.util.SqlScriptExecutor;

/**
 * Renews the statistics for the working-tables
 * by executing a SQL-script.
 * 
 * @author Ralf Wisser
 */
public class SqlScriptBasedStatisticRenovator implements StatisticRenovator {

	/**
	 * Name of SQL-script file.
	 */
	private String scriptFileName;
	
	/**
	 * Gets name of SQL-script file.
	 * 
	 * @return Name of SQL-script file
	 */
	public String getScriptFileName() {
		return scriptFileName;
	}

	/**
	 * Sets name of SQL-script file.
	 * 
	 * @param scriptFileName Name of SQL-script file
	 */
	public void setScriptFileName(String scriptFileName) {
		this.scriptFileName = scriptFileName;
	}

	/**
	 * Constructor.
	 * 
	 * @param scriptFileName name of SQL-script file
	 */
	public SqlScriptBasedStatisticRenovator(String scriptFileName) {
		this.scriptFileName = scriptFileName;
	}
	
	/**
	 * Constructor.
	 */
	public SqlScriptBasedStatisticRenovator() {
	}

	/**
	 * Renews the DB table statistics for the working-tables
	 * by executing the SQL-script.
	 * 
	 * @param session for execution of SQL-statements
	 */
	@Override
	public void renew(Session session, ExecutionContext executionContext) throws Exception {
		Map<String, String> arguments = new HashMap<String, String>();
		arguments.put("JAILER_ENTITY", SQLDialect.dmlTableReference("JAILER_ENTITY", session, executionContext));
		arguments.put("JAILER_GRAPH", SQLDialect.dmlTableReference("JAILER_GRAPH", session, executionContext));
		arguments.put("JAILER_DEPENDENCY", SQLDialect.dmlTableReference("JAILER_DEPENDENCY", session, executionContext));
		arguments.put("JAILER_SET", SQLDialect.dmlTableReference("JAILER_SET", session, executionContext));
		Map<String, String> splittedArguments = new HashMap<String, String>();
		for (Entry<String, String> e: arguments.entrySet()) {
			String schtab = e.getValue();
			int iDot = schtab.lastIndexOf('.');
			String schema;
			String tab;
			if (iDot >= 0) {
				schema = schtab.substring(0, iDot);
				tab = schtab.substring(iDot + 1);
			} else {
				schema = session.getSchema();
				tab = schtab;
			}
			splittedArguments.put("SCHEMA_" + e.getKey(), schema);
			splittedArguments.put("TABLE_" + e.getKey(), tab);
		}
		arguments.putAll(splittedArguments);
		File file = Configuration.getInstance().createTempFile();
		PrintWriter out = new PrintWriter(new FileOutputStream(file));
		out.print(new PrintUtil().applyTemplate(scriptFileName.replace('/', File.separatorChar), arguments, null));
		out.close();
		boolean silent = session.getSilent();
		session.setSilent(true);
		new SqlScriptExecutor(session, 1).executeScript(file.getPath());
		session.setSilent(silent);
		file.delete();
	}

}
