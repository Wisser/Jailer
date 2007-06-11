package org.jailer.util;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.sql.SQLException;

import org.jailer.database.StatementExecutor;


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
        while ((line = reader.readLine()) != null) {
            line = line.trim();
            if (line.length() == 0 || line.startsWith("#") || line.startsWith("--")) {
                // comment or empty line
                continue;
            }
            if (line.endsWith(";")) {
                line = line.substring(0, line.length() - 1);
            }
            statementExecutor.execute(line);
        }
        reader.close();
    }
    
}
