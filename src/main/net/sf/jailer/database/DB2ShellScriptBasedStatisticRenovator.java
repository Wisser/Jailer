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


/**
 * Renews the DB2 table statistics for the working-tables
 * by executing a shell-script.
 * 
 * @author Ralf Wisser
 */
public class DB2ShellScriptBasedStatisticRenovator extends ShellScriptBasedStatisticRenovator {

    /**
     * Constructor.
     * 
     * @param scriptInvocation invocation of the script-file
     */
    public DB2ShellScriptBasedStatisticRenovator(String scriptInvocation) {
        super(scriptInvocation);
    }

    /**
     * Gets shell-invocation.
     * 
     * @param session for execution of SQL-statements
     * @return shell-invocation
     */
    protected String getScriptInvocation(Session session) throws Exception {
        int indexOf = session.dbUrl.indexOf("://");
        String dbName;
        if (indexOf >= 0) {
			String s = session.dbUrl.substring(indexOf + 3);
	        dbName = s.substring(s.indexOf("/") + 1);
        } else {
			String s = session.dbUrl;
        	dbName = s.substring(s.lastIndexOf(":") + 1);
        }
        return super.getScriptInvocation(session) + " " + dbName + " " + session.dbUser + " " + session.dbPassword;
    }

}
