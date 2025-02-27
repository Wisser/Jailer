/*
 * Copyright 2007 - 2025 Ralf Wisser.
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
	 * Gets reference to a temporary table in DML statements.
	 */
	String getDmlTableReference(String table);

	/**
	 * Gets prefix of DDL statement to create temporary table.
	 */
	String getCreateTablePrefix();

	/**
	 * Gets suffix of DDL statement to create temporary table.
	 */
	String getCreateTableSuffix();
	
	/**
	 * Gets prefix of DDL statement to create temporary index.
	 */
	String getCreateIndexPrefix();

	/**
	 * Gets suffix of DDL statement to create temporary index.
	 */
	String getCreateIndexSuffix();
	
	/**
	 * Gets prefix of table name to be used in DDL for creating temporary index.
	 */
	String getIndexTablePrefix();
	
	/**
	 * Gets prefix to be used in DDL for dropping temporary tables.
	 */
	String getDropTablePrefix();
	
	/**
	 * Gets prefix of reference to a temporary table in DDL statements.
	 */
	String getDdlTableReferencePrefix();

}
