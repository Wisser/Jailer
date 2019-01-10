/*
 * Copyright 2007 - 2019 the original author or authors.
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
	public LocalDatabase(String driverClassName, String urlPattern, String user, String password, String jarfile) throws ClassNotFoundException, FileNotFoundException, SQLException {
		this.databaseFolder = new File(Configuration.getInstance().getTempFileFolder() + File.separator + UUID.randomUUID().toString()).getAbsolutePath();
		new File(databaseFolder).mkdirs();
		BasicDataSource dataSource;
		try {
			dataSource = new BasicDataSource(driverClassName, urlPattern.replace("%s", databaseFolder + File.separator + "local"), user, password, 0, new URL[0]);
		} catch (Exception e) {
			dataSource = new BasicDataSource(driverClassName, urlPattern.replace("%s", databaseFolder + File.separator + "local"), user, password, 0, ClasspathUtil.toURLArray(jarfile, null, null, null));
		}
		session = new Session(dataSource, dataSource.dbms, Connection.TRANSACTION_READ_UNCOMMITTED, null, false, true);
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
