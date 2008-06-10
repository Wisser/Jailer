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
package net.sf.jailer;

import java.io.File;
import java.io.PrintWriter;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

import net.sf.jailer.database.StatementExecutor;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.util.PrintUtil;
import net.sf.jailer.util.SqlScriptExecutor;

/**
 * Creates the DDL for the working-tables.
 * 
 * @author Ralf Wisser
 */
public class DDLCreator {

    /**
     * Creates the DDL for the working-tables.
     */
    public static boolean createDDL(String driverClass, String dbUrl, String user, String password) throws Exception {
        DataModel dataModel = new DataModel();
        
        String template = "script/ddl-template.sql";
        String universalPrimaryKey = dataModel.getUniversalPrimaryKey().toSQL(null);
		Object[] arguments = new Object[] { 
                universalPrimaryKey,
                dataModel.getUniversalPrimaryKey().toSQL("PRE_", false),
                dataModel.getUniversalPrimaryKey().toSQL("FROM_"),
                dataModel.getUniversalPrimaryKey().toSQL("TO_"),
                dataModel.getUniversalPrimaryKey().columnList(null),
                dataModel.getUniversalPrimaryKey().columnList("FROM_"),
                dataModel.getUniversalPrimaryKey().columnList("TO_"),
                Jailer.VERSION
            };
        String ddl = PrintUtil.applyTemplate(template, arguments);
        
        System.out.println(ddl);
        
        if (driverClass != null) {
        	StatementExecutor statementExecutor = new StatementExecutor(driverClass, dbUrl, user, password);
        	try {
        		File tmp = new File("jailer_ddl.sql");
        		PrintWriter pw = new PrintWriter(tmp);
        		pw.println(ddl);
        		pw.close();
        		SqlScriptExecutor.executeScript(tmp.getCanonicalPath(), statementExecutor);
        	} finally {
        		statementExecutor.shutDown();
        	}
        }
        
        return true;
    }
    
    /**
     * Checks whether working-tables schema is up-to-date.
     * 
     * @return <code>true</code> if working-tables schema is up-to-date
     */
	public static boolean isUptodate(String driverClass, String dbUrl, String user, String password) {
		try {
			if (driverClass != null) {
	        	StatementExecutor statementExecutor = new StatementExecutor(driverClass, dbUrl, user, password);
	        	try {
	        		final boolean[] uptodate = new boolean[] { false };
	        		final DataModel datamodel = new DataModel();
	        		statementExecutor.executeQuery("Select jvalue from JL_CONFIG where jversion='" + Jailer.VERSION + "' and jkey='upk'", new StatementExecutor.ResultSetReader() {
						public void readCurrentRow(ResultSet resultSet) throws SQLException {
							uptodate[0] = resultSet.getString(1).equals(datamodel.getUniversalPrimaryKey().toSQL(null));
						}
						public void close() {
						}
	        		});
	        		return uptodate[0];
	        	} catch (Exception e) {
	        		return false;
	        	} finally {
						statementExecutor.shutDown();
	        	}
	        }
		} catch (Exception e) {
			return false;
		}
        
        return false;
	}

}
