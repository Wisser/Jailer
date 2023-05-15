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
package net.sf.jailer.ui.commandline;

import java.io.File;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.ui.Environment;

/**
 * Global (singleton) instance of the {@link ExecutionContext}.
 * 
 * @author Ralf Wisser
 */
public class CommandLineInstance {

	/**
	 * The singleton.
	 */
	private static UICommandLine commandLine;
	
	/**
	 * Gets the singleton.
	 * 
	 * @return the singleton
	 */
	public static UICommandLine getInstance() {
		return commandLine;
	}

	/**
	 * Sets the singleton.
	 */
	public static synchronized void init(String[] args) throws Exception {
		commandLine = UICommandLineParser.parse(args, true);
		if (commandLine.datamodelFolder != null) {
			if (!new File(commandLine.datamodelFolder).isAbsolute()) {
				commandLine.datamodelFolder = Environment.newFile(commandLine.datamodelFolder).getPath();
			}
		}
	}

	public static void clear() {
		commandLine = new UICommandLine();
	}

}
