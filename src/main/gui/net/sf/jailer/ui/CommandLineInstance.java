/*
 * Copyright 2007 - 2017 the original author or authors.
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

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.CommandLineParser;
import net.sf.jailer.CommandLine;

/**
 * Global (singleton) instance of the {@link ExecutionContext}.
 * 
 * @author Ralf Wisser
 */
public class CommandLineInstance {

	/**
	 * The singleton.
	 */
	private static CommandLine commandLine;
	
	/**
	 * The singleton as {@link ExecutionContext}.
	 */
	private static ExecutionContext executionContext;
	
	/**
	 * Gets the singleton.
	 * 
	 * @return the singleton
	 */
	public static CommandLine getInstance() {
		return commandLine;
	}

	/**
	 * Gets the singleton as Ex.
	 * 
	 * @return the singleton
	 */
	public static ExecutionContext getExecutionContext() {
		return executionContext;
	}

	/**
	 * Sets the singleton.
	 */
	public static synchronized void init(String[] args) throws Exception {
		commandLine = CommandLineParser.parse(args, true);
		executionContext = new ExecutionContext(commandLine);
	}

}
