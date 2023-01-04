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
import java.io.FileNotFoundException;
import java.net.URL;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.UUID;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.util.ClasspathUtil;


/**
 * Provides a local database (H2).
 * 
 * @author Ralf Wisser
 */
public class LocalDatabase {
	
	/**
	 * The session for the local database.
	 */
	private final Session session;

	/**
	 * Name of the folder containing the local database.
	 */
	private String databaseFolder;
	
	/**
	 * Creates a local database.
	 */
	public LocalDatabase(String driverClassName, String urlPattern, String user, String password, String jarfile, ExecutionContext executionContext) throws ClassNotFoundException, FileNotFoundException, SQLException {
		String tempFileFolder = determineTempFileFolder(executionContext);
		this.databaseFolder = new File(tempFileFolder + File.separator + UUID.randomUUID().toString()).getAbsolutePath();
		new File(databaseFolder).mkdirs();
		BasicDataSource dataSource;
		URL[] urlArray;
		FileNotFoundException urlException = null;
		try {
			urlArray = ClasspathUtil.toURLArray(jarfile, null, null, null);
		} catch (FileNotFoundException e) {
			urlArray = new URL[0];
			urlException = new FileNotFoundException(e.getMessage() + ". This file is required for using WorkingTableScope.LOCAL_DATABASE. Make the file available or add it to the classpath.");
		}
		if (urlException != null) {
			try {
				dataSource = new BasicDataSource(driverClassName, urlPattern.replace("%s", new File(databaseFolder, "local").getAbsolutePath()), user, password, 0, urlArray);
			} catch (Exception e) {
				throw urlException;
			}
		} else {
			dataSource = new BasicDataSource(driverClassName, urlPattern.replace("%s", new File(databaseFolder, "local").getAbsolutePath()), user, password, 0, urlArray);
		}
		session = new Session(dataSource, dataSource.dbms, Connection.TRANSACTION_READ_UNCOMMITTED, null, false, true);
		session.setLogPrefix("[local-db] ");
	}

	/**
	 * Determines the folder to be used for local db's temp files.
	 * 
	 * @param executionContext the {@link ExecutionContext}
	 * 
	 * @return the folder to be used for local db's temp files
	 */
	public static String determineTempFileFolder(ExecutionContext executionContext) {
		String tempFileFolder = Configuration.getInstance().getTempFileFolder();
		if (executionContext.getLocalDatabaseStorage() != null) {
			String lds = executionContext.getLocalDatabaseStorage().trim();
			if (new File(lds).isDirectory()) {
				tempFileFolder = lds;
			}
		}
		tempFileFolder = tempFileFolder.replace(";", "-");
		return tempFileFolder;
	}

	/**
	 * Shut local database down. Remove all database files.
	 */
	public void shutDown() throws SQLException {
		session.shutDown();
		File localFolder = new File(databaseFolder);
		File[] listFiles = localFolder.listFiles();
		if (listFiles != null) {
			for (File file: listFiles) {
				file.delete();
			}
		}
		localFolder.delete();
	}

	/**
	 * Gets the {@link Session} for the local database.
	 * 
	 * @return the session for the local database
	 */
	public Session getSession() {
		return session;
	}

}
