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
package net.sf.jailer.database;

/**
 * Default implementation of {@link TemporaryTableManager}.
 * 
 * @author Ralf Wisser
 */
public class DefaultTemporaryTableManager implements TemporaryTableManager {

	/**
	 * Prefix of references to a temporary table in DML statements.
	 */
	private String dmlTableReferencePrefix;
	
	/**
	 * Prefix of DDL statement to create temporary table.
	 */
	private String createTablePrefix;
	
	/**
	 * Suffix of DDL statement to create temporary table.
	 */
	private String createTableSuffix;
	
	/**
	 * Prefix of DDL statement to create temporary index.
	 */
	private String createIndexPrefix;
	
	/**
	 * Suffix of DDL statement to create temporary index.
	 */
	private String createIndexSuffix;
	
	/**
	 * Gets prefix of table name to be used in DDL for creating temporary index.
	 */
	private String indexTablePrefix;

	/**
	 * Gets prefix of DDL statement to create temporary table.
	 */
	public String getCreateTablePrefix() {
		return createTablePrefix;
	}

	/**
	 * Gets prefix of table name to be used in DDL for creating temporary index.
	 */
	public String getIndexTablePrefix() {
		return indexTablePrefix;
	}
	
	/**
	 * Sets prefix of table name to be used in DDL for creating temporary index.
	 */
	public void setIndexTablePrefix(String v) {
		this.indexTablePrefix = v;
	}
	
	/**
	 * Gets suffix of DDL statement to create temporary table.
	 */
	public String getCreateTableSuffix() {
		return createTableSuffix;
	}
	
	/**
	 * Gets prefix of DDL statement to create temporary index.
	 */
	public String getCreateIndexPrefix() {
		return createIndexPrefix;
	}

	/**
	 * Gets suffix of DDL statement to create temporary index.
	 */
	public String getCreateIndexSuffix() {
		return createIndexSuffix;
	}

	/**
	 * Sets prefix of references to a temporary table in DML statements.
	 */
	public void setDmlTableReferencePrefix(String dmlTableReferencePrefix) {
		this.dmlTableReferencePrefix = dmlTableReferencePrefix;
	}

	/**
	 * Gets prefix of DDL statement to create temporary table.
	 */
	public void setCreateTablePrefix(String v) {
		createTablePrefix = v;
	}

	/**
	 * Gets suffix of DDL statement to create temporary table.
	 */
	public void setCreateTableSuffix(String v) {
		createTableSuffix = v;
	}
	
	/**
	 * Gets prefix of DDL statement to create temporary index.
	 */
	public void setCreateIndexPrefix(String v) {
		createIndexPrefix = v;
	}

	/**
	 * Gets suffix of DDL statement to create temporary index.
	 */
	public void setCreateIndexSuffix(String v) {
		createIndexSuffix = v;
	}

	/**
	 * Gets reference to a temporary table in DML statements.
	 */
	public String getDmlTableReference(String table) {
		return dmlTableReferencePrefix + table;
	}
	
}
