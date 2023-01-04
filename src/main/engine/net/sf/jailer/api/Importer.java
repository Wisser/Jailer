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
package net.sf.jailer.api;

import java.io.File;
import java.io.IOException;
import java.sql.Connection;
import java.sql.SQLException;

import javax.sql.DataSource;

import net.sf.jailer.database.Session;
import net.sf.jailer.util.SqlScriptExecutor;

/**
 * Imports subset data.
 * 
 * @author Ralf Wisser
 */
public class Importer {
	
	/**
	 * Default constructor.
	 */
	public Importer() {
	}
	
	/**
	 * Creates a new Importer with all mandatory attributes.
	 * 
	 * @param dataSource the data-source to connect with the database
	 */
	public Importer(DataSource dataSource) {
		this.dataSource = dataSource;
	}

	/**
	 * Imports a SQL-script-file.
	 * 
	 * @param inputScript the SQL-script-file
	 */
	public void execute(File inputScript) throws IOException, SQLException {
		Session session = new Session(getDataSource(), null, isolationLevel, null, getTransactional());
		new SqlScriptExecutor(session, getNumberOfThreads(), false).executeScript(inputScript.getPath(), getTransactional());
	}
	
	/**
	 * Gets the data-source to connect with the database.
	 * 
	 * @return the data-source to connect with the database
	 */
	public DataSource getDataSource() {
		return dataSource;
	}

	/**
	 * Sets the data-source to connect with the database.
	 * 
	 * @param dataSource the data-source to connect with the database
	 */
	public void setDataSource(DataSource dataSource) {
		this.dataSource = dataSource;
	}

	/**
	 * Gets number of threads. (default is 1)
	 *
	 * @return number of threads (default is 1)
	 */
	public int getNumberOfThreads() {
		return numberOfThreads;
	}

	/**
	 * Sets number of threads. (default is 1)
	 *
	 * @param numberOfThreads
	 *            number of threads (default is 1)
	 */
	public void setNumberOfThreads(int numberOfThreads) {
		this.numberOfThreads = numberOfThreads;
	}

	/**
	 * If <code>true</code>, import rows in a single transaction. (default is true)
	 *
	 * @return <code>true</code> if Import rows in a single transaction
	 */
	public boolean getTransactional() {
		return transactional;
	}

	/**
	 * If <code>true</code>, Import rows in a single transaction. (default is true)
	 *
	 * @param transactional
	 *            <code>true</code> if import rows in a single transaction
	 */
	public void setTransactional(boolean transactional) {
		this.transactional = transactional;
	}

	/**
	 * Gets IsolationLevel.
	 * 
	 * @see Connection#setTransactionIsolation(int)
	 */
	public Integer getIsolationLevel() {
		return isolationLevel;
	}

	/**
	 * Sets IsolationLevel.
	 * 
	 * @see Connection#setTransactionIsolation(int)
	 */
	public void setIsolationLevel(Integer isolationLevel) {
		this.isolationLevel = isolationLevel;
	}

	private DataSource dataSource;
	private int numberOfThreads = 1;
	private boolean transactional = true;
	private Integer isolationLevel;

}
