/*
 * Copyright 2007 - 2026 Ralf Wisser.
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
package net.sf.jailer.configuration;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * Configuration of the local database.
 * 
 * @author Ralf Wisser
 */
@JsonIgnoreProperties({"comment"})
public class LocalDatabaseConfiguration {

	private String localPKType = "VARCHAR";
	private String localNPKType = "NVARCHAR";
	private int localPKLength = 10000;

	private String urlPattern = "jdbc:h2:%s";

	private String user = "";
	private String password = "";

	private String driver = "org.h2.Driver";
	private String lib = "lib/h2-2.2.224.jar";

	/**
	 * Gets the SQL type used for primary key columns in the local database.
	 *
	 * @return the local primary key type
	 */
	public String getLocalPKType() {
		return localPKType;
	}

	/**
	 * Sets the SQL type used for primary key columns in the local database.
	 *
	 * @param localPKType the local primary key type to set
	 */
	public void setLocalPKType(String localPKType) {
		this.localPKType = localPKType;
	}

	/**
	 * Gets the SQL type used for non-primary-key columns in the local database.
	 *
	 * @return the local non-primary-key type
	 */
	public String getLocalNPKType() {
		return localNPKType;
	}

	/**
	 * Sets the SQL type used for non-primary-key columns in the local database.
	 *
	 * @param localNPKType the local non-primary-key type to set
	 */
	public void setLocalNPKType(String localNPKType) {
		this.localNPKType = localNPKType;
	}

	/**
	 * Gets the maximum length for primary key columns in the local database.
	 *
	 * @return the local primary key column length
	 */
	public int getLocalPKLength() {
		return localPKLength;
	}

	/**
	 * Sets the maximum length for primary key columns in the local database.
	 *
	 * @param localPKLength the local primary key column length to set
	 */
	public void setLocalPKLength(int localPKLength) {
		this.localPKLength = localPKLength;
	}

	/**
	 * Gets the JDBC URL pattern for the local database.
	 *
	 * @return the JDBC URL pattern
	 */
	public String getUrlPattern() {
		return urlPattern;
	}

	/**
	 * Sets the JDBC URL pattern for the local database.
	 *
	 * @param urlPattern the JDBC URL pattern to set
	 */
	public void setUrlPattern(String urlPattern) {
		this.urlPattern = urlPattern;
	}

	/**
	 * Gets the JDBC driver class name for the local database.
	 *
	 * @return the JDBC driver class name
	 */
	public String getDriver() {
		return driver;
	}

	/**
	 * Sets the JDBC driver class name for the local database.
	 *
	 * @param driver the JDBC driver class name to set
	 */
	public void setDriver(String driver) {
		this.driver = driver;
	}

	/**
	 * Gets the path to the JDBC driver library for the local database.
	 *
	 * @return the path to the JDBC driver library
	 */
	public String getLib() {
		return lib;
	}

	/**
	 * Sets the path to the JDBC driver library for the local database.
	 *
	 * @param lib the path to the JDBC driver library to set
	 */
	public void setLib(String lib) {
		this.lib = lib;
	}

	/**
	 * Gets the database user name for the local database.
	 *
	 * @return the database user name
	 */
	public String getUser() {
		return user;
	}

	/**
	 * Sets the database user name for the local database.
	 *
	 * @param user the database user name to set
	 */
	public void setUser(String user) {
		this.user = user;
	}

	/**
	 * Gets the database password for the local database.
	 *
	 * @return the database password
	 */
	public String getPassword() {
		return password;
	}

	/**
	 * Sets the database password for the local database.
	 *
	 * @param password the database password to set
	 */
	public void setPassword(String password) {
		this.password = password;
	}
}
