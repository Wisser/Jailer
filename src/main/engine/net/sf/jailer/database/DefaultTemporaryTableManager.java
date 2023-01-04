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

/**
 * Default implementation of {@link TemporaryTableManager}.
 *
 * @author Ralf Wisser
 */
public class DefaultTemporaryTableManager implements TemporaryTableManager {

	/**
	 * Prefix of references to a temporary table in DML statements.
	 */
	private String dmlTableReferencePrefix = "";

	/**
	 * Prefix of references to a temporary table in DDL statements.
	 */
	private String ddlTableReferencePrefix = "";

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
	 * Prefix of table name to be used in DDL for creating temporary index.
	 */
	private String indexTablePrefix;

	/**
	 * Prefix to be used in DDL for dropping temporary tables.
	 */
	private String dropTablePrefix;

	// Whether temp tables are shared between sessions.
	private boolean needsExclusiveAccess = true;

	/**
	 * Gets prefix of DDL statement to create temporary table.
	 */
	@Override
	public String getCreateTablePrefix() {
		return createTablePrefix;
	}

	/**
	 * Gets prefix of table name to be used in DDL for creating temporary index.
	 */
	@Override
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
	@Override
	public String getCreateTableSuffix() {
		return createTableSuffix;
	}

	/**
	 * Gets prefix of DDL statement to create temporary index.
	 */
	@Override
	public String getCreateIndexPrefix() {
		return createIndexPrefix;
	}

	/**
	 * Gets suffix of DDL statement to create temporary index.
	 */
	@Override
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
	 * Gets prefix to be used in DDL for dropping temporary tables.
	 */
	@Override
	public String getDropTablePrefix() {
		return dropTablePrefix;
	}

	/**
	 * Sets prefix to be used in DDL for dropping temporary tables.
	 */
	public void setDropTablePrefix(String v) {
		dropTablePrefix = v;
	}

	/**
	 * Gets reference to a temporary table in DML statements.
	 */
	@Override
	public String getDmlTableReference(String table) {
		return dmlTableReferencePrefix + table + "_T";
	}

	public String getDmlTableReferencePrefix() {
		return dmlTableReferencePrefix;
	}

	public void setDdlTableReferencePrefix(String ddlTableReferencePrefix) {
		this.ddlTableReferencePrefix = ddlTableReferencePrefix;
	}

	public String getDdlTableReferencePrefix() {
		return ddlTableReferencePrefix;
	}

	/**
	 * Whether temp tables are shared between sessions.
	 *
	 * @return whether temp tables are shared between sessions
	 */
	public boolean isNeedsExclusiveAccess() {
		return needsExclusiveAccess;
	}

	/**
	 * Whether temp tables are shared between sessions.
	 *
	 * @param needsExclusiveAccess whether temp tables are shared between sessions
	 */
	public void setNeedsExclusiveAccess(boolean needsExclusiveAccess) {
		this.needsExclusiveAccess = needsExclusiveAccess;
	}

}
