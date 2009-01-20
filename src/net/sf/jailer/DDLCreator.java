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
import java.util.HashMap;
import java.util.Map;

import net.sf.jailer.database.DBMS;
import net.sf.jailer.database.SQLDialect;
import net.sf.jailer.database.StatementExecutor;
import net.sf.jailer.database.TemporaryTableManager;
import net.sf.jailer.database.TemporaryTableScope;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.util.PrintUtil;
import net.sf.jailer.util.SqlScriptExecutor;
import net.sf.jailer.util.SqlUtil;

/**
 * Creates the DDL for the working-tables.
 * 
 * @author Ralf Wisser
 */
public class DDLCreator {

    /**
     * Creates the DDL for the working-tables.
     */
    public static boolean createDDL(String driverClass, String dbUrl, String user, String password, TemporaryTableScope temporaryTableScope) throws Exception {
    	StatementExecutor statementExecutor = null;
        if (driverClass != null) {
        	statementExecutor = new StatementExecutor(driverClass, dbUrl, user, password);
        }
        return createDDL(statementExecutor, temporaryTableScope);
    }
    
    /**
     * Creates the DDL for the working-tables.
     */
    public static boolean createDDL(StatementExecutor statementExecutor, TemporaryTableScope temporaryTableScope) throws Exception {
        DataModel dataModel = new DataModel();
        
        String template = "script/ddl-template.sql";
		String contraint = statementExecutor != null && (statementExecutor.dbms == DBMS.SYBASE || statementExecutor.dbms == DBMS.MySQL)? " NULL" : "";
		String universalPrimaryKey = dataModel.getUniversalPrimaryKey().toSQL(null, contraint);
		Map<String, String> arguments = new HashMap<String, String>();
        arguments.put("upk", universalPrimaryKey);
		arguments.put("pre", dataModel.getUniversalPrimaryKey().toSQL("PRE_", contraint));
		arguments.put("from", dataModel.getUniversalPrimaryKey().toSQL("FROM_", contraint));
		arguments.put("to", dataModel.getUniversalPrimaryKey().toSQL("TO_", contraint));
		arguments.put("column-list", dataModel.getUniversalPrimaryKey().columnList(null));
		arguments.put("column-list-from", dataModel.getUniversalPrimaryKey().columnList("FROM_"));
		arguments.put("column-list-to", dataModel.getUniversalPrimaryKey().columnList("TO_"));
		arguments.put("version", Jailer.VERSION);
		arguments.put("constraint", contraint);
		
		TemporaryTableManager tableManager = null;
		if (temporaryTableScope == TemporaryTableScope.SESSION_LOCAL) {
			tableManager = Configuration.forDbms(statementExecutor).sessionTemporaryTableManager;
		}
		if (temporaryTableScope == TemporaryTableScope.TRANSACTION_LOCAL) {
			tableManager = Configuration.forDbms(statementExecutor).transactionTemporaryTableManager;
		}
		String tableName = SQLDialect.CONFIG_TABLE_;
		if (tableManager != null) {
			tableName = tableManager.getDmlTableReference(tableName);
		}
		arguments.put("config-dml-reference", tableName);
		if (tableManager != null) {
			arguments.put("table-suffix", "_T");
			arguments.put("drop-table", "DROP TABLE ");
			arguments.put("create-table", tableManager.getCreateTablePrefix());
			arguments.put("create-table-suffix", tableManager.getCreateTableSuffix());
			arguments.put("create-index", tableManager.getCreateIndexPrefix());
			arguments.put("create-index-suffix", tableManager.getCreateIndexSuffix());
			arguments.put("index-table-prefix", tableManager.getIndexTablePrefix());
		} else {
			arguments.put("table-suffix", "");
			arguments.put("drop-table", "DROP TABLE ");
			arguments.put("create-table", "CREATE TABLE ");
			arguments.put("create-table-suffix", "");
			arguments.put("create-index", "CREATE INDEX ");
			arguments.put("create-index-suffix", "");
			arguments.put("index-table-prefix", "");
		}
		
		String ddl = PrintUtil.applyTemplate(template, arguments);
        
        System.out.println(ddl);
        
        if (statementExecutor != null) {
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
	        	final StatementExecutor statementExecutor = new StatementExecutor(driverClass, dbUrl, user, password);
// TODO
if (1==1 /* use_temporary_tables */) {
	return false;
}
	        	try {
	        		final boolean[] uptodate = new boolean[] { false };
	        		final DataModel datamodel = new DataModel();
	        		statementExecutor.executeQuery("Select jvalue from " + SQLDialect.dmlTableReference(SQLDialect.CONFIG_TABLE_, statementExecutor) + " where jversion='" + Jailer.VERSION + "' and jkey='upk'", new StatementExecutor.ResultSetReader() {
						public void readCurrentRow(ResultSet resultSet) throws SQLException {
							String contraint = statementExecutor.dbms == DBMS.SYBASE? " NULL" : "";
							String universalPrimaryKey = datamodel.getUniversalPrimaryKey().toSQL(null, contraint);
							uptodate[0] = resultSet.getString(1).equals(universalPrimaryKey);
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

	/**
     * Checks for conflicts of existing tables and working-tables.
     * 
     * @return name of table in conflict or <code>null</code>
     */
	public static String getTableInConflict(String driverClass, String dbUrl, String user, String password) {
		try {
			if (driverClass != null) {
	        	StatementExecutor statementExecutor = new StatementExecutor(driverClass, dbUrl, user, password);
// TODO
if (1==1 /* use_temporary_tables */) {
	return null;
}
	        	statementExecutor.setSilent(true);
	        	try {
	        		final boolean[] uptodate = new boolean[] { false };
	        		statementExecutor.executeQuery("Select jvalue from " + SQLDialect.dmlTableReference(SQLDialect.CONFIG_TABLE_, statementExecutor) + " where jkey='magic' and jvalue='837065098274756382534403654245288'", new StatementExecutor.ResultSetReader() {
						public void readCurrentRow(ResultSet resultSet) throws SQLException {
							uptodate[0] = true;
						}
						public void close() {
						}
	        		});
	        		if (uptodate[0]) {
	        			return null;
	        		}
	        	} catch (Exception e) {
	        		// fall through
	        	}
	            
	    		// look for jailer tables
	    		for (String table: SqlUtil.JAILER_TABLES) {
		        	try {
		        		statementExecutor.executeQuery("Select * from " + table, new StatementExecutor.ResultSetReader() {
							public void readCurrentRow(ResultSet resultSet) throws SQLException {
							}
							public void close() {
							}
		        		});
		        		statementExecutor.shutDown();
		        		return table;
		        	} catch (Exception e) {
		        		// fall through
		        	}
	    		}
				statementExecutor.shutDown();
	        }
			return null;
		} catch (Exception e) {
			return null;
        }
	}

}
