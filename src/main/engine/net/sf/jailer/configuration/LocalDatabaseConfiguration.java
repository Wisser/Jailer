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
package net.sf.jailer.configuration;

/**
 * Configuration of the local database.
 * 
 * @author Ralf Wisser
 */
public class LocalDatabaseConfiguration {

	private String localPKType = "VARCHAR";
	private String localNPKType = "NVARCHAR";
	private int localPKLength = 10000;

	private String urlPattern = "jdbc:h2:%s";

	private String user = "";
	private String password = "";

	private String driver = "org.h2.Driver";
	private String lib = "lib/h2-2.1.212.jar";

	/**
	 * @return the localPKType
	 */
	public String getLocalPKType() {
		return localPKType;
	}

	/**
	 * @param localPKType
	 *            the localPKType to set
	 */
	public void setLocalPKType(String localPKType) {
		this.localPKType = localPKType;
	}

	/**
	 * @return the localNPKType
	 */
	public String getLocalNPKType() {
		return localNPKType;
	}

	/**
	 * @param localNPKType
	 *            the localNPKType to set
	 */
	public void setLocalNPKType(String localNPKType) {
		this.localNPKType = localNPKType;
	}

	/**
	 * @return the localPKLength
	 */
	public int getLocalPKLength() {
		return localPKLength;
	}

	/**
	 * @param localPKLength
	 *            the localPKLength to set
	 */
	public void setLocalPKLength(int localPKLength) {
		this.localPKLength = localPKLength;
	}

	/**
	 * @return the urlPattern
	 */
	public String getUrlPattern() {
		return urlPattern;
	}

	/**
	 * @param urlPattern
	 *            the urlPattern to set
	 */
	public void setUrlPattern(String urlPattern) {
		this.urlPattern = urlPattern;
	}

	/**
	 * @return the driver
	 */
	public String getDriver() {
		return driver;
	}

	/**
	 * @param driver
	 *            the driver to set
	 */
	public void setDriver(String driver) {
		this.driver = driver;
	}

	/**
	 * @return the lib
	 */
	public String getLib() {
		return lib;
	}

	/**
	 * @param lib
	 *            the lib to set
	 */
	public void setLib(String lib) {
		this.lib = lib;
	}

	/**
	 * @return the user
	 */
	public String getUser() {
		return user;
	}

	/**
	 * @param user
	 *            the user to set
	 */
	public void setUser(String user) {
		this.user = user;
	}

	/**
	 * @return the password
	 */
	public String getPassword() {
		return password;
	}

	/**
	 * @param password
	 *            the password to set
	 */
	public void setPassword(String password) {
		this.password = password;
	}
}
