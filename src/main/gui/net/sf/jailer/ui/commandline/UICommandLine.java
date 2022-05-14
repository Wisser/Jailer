/*
 * Copyright 2007 - 2022 Ralf Wisser.
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

import java.util.ArrayList;
import java.util.List;

import org.kohsuke.args4j.Argument;
import org.kohsuke.args4j.Option;

/**
 * Holds command-line arguments (UI).
 * 
 * @author Ralf Wisser
 */
public class UICommandLine {

	@Option(name="-schemamapping",usage="schema map", metaVar="schema-in-model=schema-in-db[','x=y]*")
	public String rawschemamapping = null;

	@Option(name="-datamodel", usage="folder holding the data model.")
	public String datamodelFolder = null;

	@Option(name="-jdbcjar", usage="JDBC driver's jar file")
	public String jdbcjar = null;

	@Option(name="-jdbcjar2", usage="JDBC driver's additional jar file")
	public String jdbcjar2 = null;

	@Option(name="-jdbcjar3", usage="JDBC driver's 2. additional jar file")
	public String jdbcjar3 = null;

	@Option(name="-jdbcjar4", usage="JDBC driver's 3. additional jar file")
	public String jdbcjar4 = null;
	
	@Option(name="-driver", usage="JDBC driver class name.")
	public String driver = null;
	
	@Option(name="-url", usage="JDBC URL.")
	public String url = null;
	
	@Option(name="-user", usage="Database user name.")
	public String user = null;
	
	@Option(name="-password", usage="Database password.")
	public String password = null;
	
	@Option(name="-file-lookup", usage="read the next parameter from the (1st line of the) file named VAL. \n(This is especially useful for not making passwords visible by querying the command line parameters)")
	public List<String> parameterFile = new ArrayList<String>();

	@Option(name="-bookmark", usage="Bookmark to open.")
	public String bookmark = null;

	@Option(name="-tool", usage="Preselect tool (\"D\" for Data Browser or \"S\" for Subsetter)")
	public String tool = null;

	@Option(name="-", usage="do not interpret the next word as an option, even if it begins with '-'. For example, if the username is \"-abc\", use \"- -abc\".")
	public List<String> escapedWords = new ArrayList<String>();

	// Connection alias, to be set via DMMD
	public String alias;

	@Argument
	public List<String> arguments = new ArrayList<String>();


}
