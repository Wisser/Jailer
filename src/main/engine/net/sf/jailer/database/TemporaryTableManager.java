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
package net.sf.jailer.database;

/**
 * Manages DDL/DML for temporary working tables.
 * 
 * @author Ralf Wisser
 */
public interface TemporaryTableManager {

	/**
	 * Gets the reference to a temporary table for use in DML statements.
	 *
	 * @param table the base name of the working table
	 * @return the DML table reference
	 */
	String getDmlTableReference(String table);

	/**
	 * Gets prefix of DDL statement to create a temporary table.
	 *
	 * @return the CREATE TABLE prefix
	 */
	String getCreateTablePrefix();

	/**
	 * Gets suffix of DDL statement to create a temporary table.
	 *
	 * @return the CREATE TABLE suffix
	 */
	String getCreateTableSuffix();

	/**
	 * Gets prefix of DDL statement to create a temporary index.
	 *
	 * @return the CREATE INDEX prefix
	 */
	String getCreateIndexPrefix();

	/**
	 * Gets suffix of DDL statement to create a temporary index.
	 *
	 * @return the CREATE INDEX suffix
	 */
	String getCreateIndexSuffix();

	/**
	 * Gets prefix of table name to be used in DDL for creating a temporary index.
	 *
	 * @return the index table name prefix
	 */
	String getIndexTablePrefix();

	/**
	 * Gets prefix to be used in DDL for dropping temporary tables.
	 *
	 * @return the DROP TABLE prefix
	 */
	String getDropTablePrefix();

	/**
	 * Gets prefix of reference to a temporary table in DDL statements.
	 *
	 * @return the DDL table reference prefix
	 */
	String getDdlTableReferencePrefix();

}
