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
package net.sf.jailer.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.Writer;
import java.sql.SQLException;
import java.util.zip.GZIPInputStream;

import net.sf.jailer.CommandLineParser;
import net.sf.jailer.database.Session;

import org.apache.log4j.Logger;

/**
 * Reads in and executes SQL-scripts.
 * 
 * @author Ralf Wisser
 */
public class SqlScriptExecutor {

	/**
	 * Comment prefix for multi-line comments.
	 */
	public static final String UNFINISHED_MULTILINE_COMMENT = "--+";

	/**
	 * Comment prefix for last line of a multi-line comment.
	 */
	public static final String FINISHED_MULTILINE_COMMENT = "--.";
	
	/**
     * The logger.
     */
    private static final Logger _log = Logger.getLogger(SqlScriptExecutor.class);

    /**
     * Reads in and executes a SQL-script.
     * 
     * @param scriptFileName the name of the script-file
     * @param session for execution of statements
     */
    public static void executeScript(String scriptFileName, Session session) throws IOException, SQLException {
        _log.info("reading file '" + scriptFileName + "'");
    	BufferedReader reader;
    	long fileSize = 0;
    	if (scriptFileName.toLowerCase().endsWith(".gz") || scriptFileName.toLowerCase().endsWith(".zip")) {
    		reader = new BufferedReader(new InputStreamReader(new GZIPInputStream(new FileInputStream(scriptFileName))));
    	} else {
    		File f = CommandLineParser.getInstance().newFile(scriptFileName);
    		fileSize = f.length();
    		reader = new BufferedReader(new FileReader(f));
    	}
        String line = null;
        StringBuffer currentStatement = new StringBuffer();
        long linesRead = 0;
        long bytesRead = 0;
        long t = System.currentTimeMillis();
        while ((line = reader.readLine()) != null) {
        	bytesRead += line.length() + 1;
            line = line.trim();
            if (line.length() == 0 || line.startsWith("--")) {
            	if (line.startsWith(UNFINISHED_MULTILINE_COMMENT)) {
            		String cmd = line.substring(UNFINISHED_MULTILINE_COMMENT.length());
            		if (cmd.startsWith("XML")) {
            			importSQLXML(cmd.substring(3).trim(), reader, session);
            		}
            		if (cmd.startsWith("CLOB")) {
            			importCLob(cmd.substring(4).trim(), reader, session);
            		}
            		if (cmd.startsWith("BLOB")) {
            			importBLob(cmd.substring(4).trim(), reader, session);
            		}
            	}
                continue;
            }
            if (line.endsWith(";")) {
            	++linesRead;
            	currentStatement.append(line.substring(0, line.length() - 1));
            	String stmt = currentStatement.toString();
            	boolean silent = session.getSilent();
            	session.setSilent(silent || stmt.trim().toLowerCase().startsWith("drop"));
            	try {
                	session.execute(stmt);
                } catch (SQLException e) {
                	// drops may fail
                	if (!stmt.trim().toLowerCase().startsWith("drop")) {
                    	// fix for bug [2946477]
                		if (!stmt.trim().toLowerCase().contains("jailer_dual")) {
                    		throw e;
                    	}
                	}
                }
                session.setSilent(silent);
                currentStatement.setLength(0);
                if (System.currentTimeMillis() > t + 1000) {
                	t = System.currentTimeMillis();
                	long p = 0;
                	if (fileSize > 0) {
                		p = (100 * bytesRead) / fileSize;
                		if (p > 100) {
                			p = 100;
                		}
                	}
                	_log.info(linesRead + " statements" + (p > 0? " (" + p + "%)" : ""));
                }
            } else {
                currentStatement.append(line + " ");
            }
            CancellationHandler.checkForCancellation();
        }
        reader.close();
        _log.info(linesRead + " statements (100%)");
    	_log.info("successfully read file '" + scriptFileName + "'");
    }

    /**
     * Imports clob from sql-script.
     * 
     * @param clobLocator locates the clob
     * @param reader for reading content
     */
	private static void importCLob(final String clobLocator, final BufferedReader reader, Session session) throws IOException, SQLException {
		int c1 = clobLocator.indexOf(',');
		int c2 = clobLocator.indexOf(',', c1 + 1);
		String table = clobLocator.substring(0, c1).trim();
		String column = clobLocator.substring(c1 + 1, c2).trim();
		String where = clobLocator.substring(c2 + 1).trim();
		String line;
		File lobFile = CommandLineParser.getInstance().newFile("lob." + System.currentTimeMillis());
		Writer out = new FileWriter(lobFile);
		while ((line = reader.readLine()) != null) {
		    // line = line.trim();
			if (line.startsWith(UNFINISHED_MULTILINE_COMMENT)) {
				String content = line.substring(UNFINISHED_MULTILINE_COMMENT.length());
				int l = content.length();
				boolean inEscape = false;
				for (int i = 0; i < l; ++i) {
					char c = content.charAt(i);
					if (c == '\\') {
						if (inEscape) {
							inEscape = false;
						} else {
							inEscape = true;
							continue;
						}
					} else {
						if (inEscape) {
							if (c == 'n') {
								c = '\n';
							} else if (c == 'r') {
								c = '\r';
							}
							inEscape = false;
						}
					}
					out.write(c);
				}
			} else {
				break;
			}
		}
		out.close();
		session.insertClob(table, column, where, lobFile);
		lobFile.delete();
	}
    
	/**
     * Imports SQL-XML from sql-script.
     * 
     * @param xmlLocator locates the XML column
     * @param reader for reading content
     */
	private static void importSQLXML(final String xmlLocator, final BufferedReader reader, Session session) throws IOException, SQLException {
		int c1 = xmlLocator.indexOf(',');
		int c2 = xmlLocator.indexOf(',', c1 + 1);
		String table = xmlLocator.substring(0, c1).trim();
		String column = xmlLocator.substring(c1 + 1, c2).trim();
		String where = xmlLocator.substring(c2 + 1).trim();
		String line;
		File lobFile = CommandLineParser.getInstance().newFile("lob." + System.currentTimeMillis());
		Writer out = new FileWriter(lobFile);
		while ((line = reader.readLine()) != null) {
		    // line = line.trim();
			if (line.startsWith(UNFINISHED_MULTILINE_COMMENT)) {
				String content = line.substring(UNFINISHED_MULTILINE_COMMENT.length());
				int l = content.length();
				boolean inEscape = false;
				for (int i = 0; i < l; ++i) {
					char c = content.charAt(i);
					if (c == '\\') {
						if (inEscape) {
							inEscape = false;
						} else {
							inEscape = true;
							continue;
						}
					} else {
						if (inEscape) {
							if (c == 'n') {
								c = '\n';
							} else if (c == 'r') {
								c = '\r';
							}
							inEscape = false;
						}
					}
					out.write(c);
				}
			} else {
				break;
			}
		}
		out.close();
		session.insertSQLXML(table, column, where, lobFile);
		lobFile.delete();
	}
    
	/**
     * Imports blob from sql-script.
     * 
     * @param clobLocator locates the clob
     * @param reader for reading content
     */
	private static void importBLob(final String clobLocator, final BufferedReader reader, Session session) throws IOException, SQLException {
		int c1 = clobLocator.indexOf(',');
		int c2 = clobLocator.indexOf(',', c1 + 1);
		String table = clobLocator.substring(0, c1).trim();
		String column = clobLocator.substring(c1 + 1, c2).trim();
		String where = clobLocator.substring(c2 + 1).trim();
		String line;
		File lobFile = CommandLineParser.getInstance().newFile("lob." + System.currentTimeMillis());
		OutputStream out = new FileOutputStream(lobFile);
		while ((line = reader.readLine()) != null) {
		    line = line.trim();
			if (line.startsWith(UNFINISHED_MULTILINE_COMMENT)) {
				String content = line.substring(UNFINISHED_MULTILINE_COMMENT.length());
				out.write(Base64.decode(content));
			} else {
				break;
			}
		}
		out.close();
		session.insertBlob(table, column, where, lobFile);
		lobFile.delete();
	}

}
