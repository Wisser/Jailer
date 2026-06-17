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
package net.sf.jailer;

/**
 * The Jailer Version.
 *
 * @author Ralf Wisser
 */
public class JailerVersion {
	
	/**
	 * The Jailer version.
	 */
	public static final String VERSION = "17.1.2.1";

	/**
	 * The Jailer working tables version.
	 */
	public static final int WORKING_TABLE_VERSION = 4;

	/**
	 * The Jailer application name.
	 */
	public static final String APPLICATION_NAME = "Jailer";

	/**
	 * Prints version.
	 *
	 * @param args command-line arguments (unused)
	 */
	public static void main(String[] args) {
		System.out.print(VERSION);
	}

}


// TODO
// TODO sql console: indicate (using bg-color?) if statement under caret is executed. Look at results tab to determine if statement is executed. If not, then statement is not executed. If yes, then statement is executed. If statement is executed, then indicate that in sql console using bg-color or some other visual indicator. If statement is not executed, then do not indicate that in sql console. If statement is executed, then indicate that in sql console using bg-color or some other visual indicator. If statement is not executed, then do not indicate that in sql console.
