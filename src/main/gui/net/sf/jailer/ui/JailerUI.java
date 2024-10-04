/*
 * Copyright 2007 - 2024 Ralf Wisser.
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
package net.sf.jailer.ui;

/**
 * Jailer is a tool for database subsetting and relational data browsing. <br>
 * <ul>
 * <li>
 * The Subsetter exports consistent, referentially intact row-sets from relational databases,
 * generates topologically sorted SQL-DML, DbUnit datasets
 * and hierarchically structured XML.
 * </li>
 * <li>
 * The Data Browser allows bidirectional navigation through the database
 * by following foreign-key-based or user-defined relationships.
 * </li>
 * </ul>
 *
 * <a href="http://jailer.sourceforge.net/">http://jailer.sourceforge.net</a> <br>
 * <a href="https://github.com/Wisser/Jailer">https://github.com/Wisser/Jailer</a> <br><br>
 *
 * @author Ralf Wisser
 */
public class JailerUI {

	/**
	 * Main-method for UI.
	 *
	 * @param args arguments
	 */
	public static void main(String[] args) {
		ExtractionModelFrame.main(args);
	}

}


// TODO 
// TODO EMail Passwort

// TODO
// TODO Doku: CLI only available with jailer<n>.zip

