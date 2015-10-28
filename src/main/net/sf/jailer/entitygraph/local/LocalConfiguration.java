/*
 * Copyright 2007 - 2015 the original author or authors.
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
package net.sf.jailer.entitygraph.local;

/**
 * Configuration of the {@link LocalEntityGraph}.
 * 
 * @author Ralf Wisser
 */
public class LocalConfiguration {

	public String localPKType = "VARCHAR";
	public String localNPKType = "NVARCHAR";
	public int localPKLength = 10000;

	/**
	 * Name of the folder containing the local database's folders.
	 */
	public String databasesFolder = "local";
	public String urlPattern = "jdbc:h2:%s;MODE=DB2";
	
	public String driver = "org.h2.Driver";
	public String lib = "lib/h2-1.3.160.jar";
	
	
	/**
	 * @return the localPKType
	 */
	public String getLocalPKType() {
		return localPKType;
	}
	/**
	 * @param localPKType the localPKType to set
	 */
	public void setLocalPKType(String localPKType) {
		this.localPKType = localPKType;
	}
	/**
	 * @return the localPKLength
	 */
	public int getLocalPKLength() {
		return localPKLength;
	}
	/**
	 * @param localPKLength the localPKLength to set
	 */
	public void setLocalPKLength(int localPKLength) {
		this.localPKLength = localPKLength;
	}
	/**
	 * @return the databasesFolder
	 */
	public String getDatabasesFolder() {
		return databasesFolder;
	}
	/**
	 * @param databasesFolder the databasesFolder to set
	 */
	public void setDatabasesFolder(String databasesFolder) {
		this.databasesFolder = databasesFolder;
	}
	/**
	 * @return the urlPattern
	 */
	public String getUrlPattern() {
		return urlPattern;
	}
	/**
	 * @param urlPattern the urlPattern to set
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
	 * @param driver the driver to set
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
	 * @param lib the lib to set
	 */
	public void setLib(String lib) {
		this.lib = lib;
	}
	
	/**
	 * @return the localNPKType
	 */
	public String getLocalNPKType() {
		return localNPKType;
	}
	/**
	 * @param localNPKType the localNPKType to set
	 */
	public void setLocalNPKType(String localNPKType) {
		this.localNPKType = localNPKType;
	}
}
