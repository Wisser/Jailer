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
 * Working-tables scopes.
 *
 * @author Ralf Wisser
 */
public enum WorkingTableScope {

	/**
	 * Create the working-tables (JAILER_*) in the source database.
	 */
	GLOBAL,

	/**
	 * Create the working-tables (JAILER_*) as temporary tables in the source database.
	 */
	SESSION_LOCAL,

	/**
	 * Not supported. Do not use.
	 */
	TRANSACTION_LOCAL, // not supported

	/**
	 * Create a local database (H2) for the working-tables (JAILER_*).
	 */
	LOCAL_DATABASE

}
