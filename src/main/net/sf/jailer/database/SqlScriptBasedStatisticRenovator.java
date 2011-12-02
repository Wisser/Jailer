/*
 * Copyright 2007 - 2012 the original author or authors.
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
    private final String scriptFileName;
    
    /**
     * Constructor.
     * 
     * @param scriptFileName name of SQL-script file
     */
    public SqlScriptBasedStatisticRenovator(String scriptFileName) {
        this.scriptFileName = scriptFileName;
    }

	/**
     * Renews the DB table statistics for the working-tables
     * by executing the SQL-script.
     * 
     * @param session for execution of SQL-statements
     */
    public void renew(Session session) throws Exception {
		Map<String, String> arguments = new HashMap<String, String>();
		arguments.put("JAILER_ENTITY", SQLDialect.dmlTableReference("JAILER_ENTITY", session));
		arguments.put("JAILER_GRAPH", SQLDialect.dmlTableReference("JAILER_GRAPH", session));
		arguments.put("JAILER_DEPENDENCY", SQLDialect.dmlTableReference("JAILER_DEPENDENCY", session));
		arguments.put("JAILER_SET", SQLDialect.dmlTableReference("JAILER_SET", session));
		String fileName = "renew.sql";
		PrintWriter out = new PrintWriter(new FileOutputStream(fileName));
		out.print(PrintUtil.applyTemplate(scriptFileName.replace('/', File.separatorChar), arguments, null));
		out.close();
		boolean silent = session.getSilent();
		session.setSilent(true);
        SqlScriptExecutor.executeScript(fileName, session);
		session.setSilent(silent);
    }

}
