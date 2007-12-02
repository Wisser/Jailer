package net.sf.jailer.util;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.sql.SQLException;

import net.sf.jailer.database.StatementExecutor;


/**
 * Reads in and executes SQL-scripts.
 * 
 * @author Wisser
 */
public class SqlScriptExecutor {

    /**
     * Reads in and executes a SQL-script.
     * 
     * @param scriptFileName the name of the script-file
     * @param statementExecutor for execution of statements
     */
    public static void executeScript(String scriptFileName, StatementExecutor statementExecutor) throws IOException, SQLException {
        BufferedReader reader = new BufferedReader(new FileReader(scriptFileName));
        String line = null;
        StringBuffer currentStatement = new StringBuffer();
        while ((line = reader.readLine()) != null) {
            line = line.trim();
            if (line.length() == 0 || line.startsWith("#") || line.startsWith("--")) {
                continue;
            }
            if (line.endsWith(";")) {
            	String stmt = currentStatement + line.substring(0, line.length() - 1);
                try {
                	statementExecutor.execute(stmt);
                } catch (SQLException e) {
                	// drops may fail
                	if (!stmt.trim().toLowerCase().startsWith("drop")) {
                		throw e;
                	}
                }
                currentStatement.setLength(0);
            } else {
                currentStatement.append(line + " ");
            }
        }
        reader.close();
    }
    
}
