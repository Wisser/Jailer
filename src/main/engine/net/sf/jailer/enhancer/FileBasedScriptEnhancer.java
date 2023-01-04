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
package net.sf.jailer.enhancer;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.Writer;
import java.sql.SQLException;
import java.util.Set;
import java.util.TreeSet;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.entitygraph.EntityGraph;
import net.sf.jailer.subsetting.ScriptFormat;
import net.sf.jailer.subsetting.ScriptType;

/**
 * Inserts the content of the files
 * <ul>
 * <li><code>prolog/[INSERT|DELETE]/&lt;TABLE&gt;.sql</code></li>
 * <li><code>epilog/[INSERT|DELETE]/&lt;TABLE&gt;.sql</code></li>
 * </ul>
 * at top/bottom of the generated script for each table &lt;TABLE&gt;
 * for which at least one row is inserted/deleted.
 *
 * @author Ralf Wisser
 */
public class FileBasedScriptEnhancer implements ScriptEnhancer {
	
	/**
	 * Adds nothing.
	 */
	@Override
	public void addComments(Writer script, ScriptType scriptType, Session session, DBMS targetDBMSConfiguration, EntityGraph entityGraph,
			Set<Table> progress, ExecutionContext executionContext) throws IOException, SQLException {
	}
	/**
	 * Adds epilogs.
	 */
	@Override
	public void addEpilog(Writer script, ScriptType scriptType, Session session, DBMS targetDBMSConfiguration, EntityGraph entityGraph,
			Set<Table> progress, ExecutionContext executionContext) throws IOException, SQLException {
		File dir = newWorkingFolderFile("epilog" + File.separatorChar + scriptType);
		addEnhancement(script, progress, dir, executionContext);
		addEnhancement(script, dir, "EPILOG.sql");
	}
	/**
	 * Adds prologs.
	 */
	@Override
	public void addProlog(Writer script, ScriptType scriptType, Session session, DBMS targetDBMSConfiguration, EntityGraph entityGraph,
			Set<Table> progress, ExecutionContext executionContext) throws IOException, SQLException {
		File dir = newWorkingFolderFile("prolog" + File.separatorChar + scriptType);
		addEnhancement(script, dir, "PROLOG.sql");
		addEnhancement(script, progress, dir, executionContext);
	}
	/**
	 * Adds enhancement to the script.
	 *
	 * @param script writer to script
	 * @param progress the export progress
	 */
	private void addEnhancement(Writer script, Set<Table> progress, File dir, ExecutionContext executionContext) throws IOException {
		if (ScriptFormat.SQL.equals(executionContext.getScriptFormat())) {
			Set<String> fileNames = new TreeSet<String>();
			for (Table table: progress) {
				fileNames.add(table.getOriginalName() + ".sql");
			}
			for (String fileName: fileNames) {
				addEnhancement(script, dir, fileName);
			}
		}
	}
	/**
	 * Adds enhancement to the script.
	 *
	 * @param script writer to script
	 */
	private void addEnhancement(Writer script, File dir, String fileName)
			throws FileNotFoundException, IOException {
		File enhancement = new File(dir, fileName);
		if (enhancement.exists()) {
			BufferedReader in = new BufferedReader(new FileReader(enhancement));
			String line;
			while ((line = in.readLine()) != null) {
				script.append(line);
				script.append(System.getProperty("line.separator", "\n"));
			}
			in.close();
		}
	}
	
	private static File newWorkingFolderFile(String name) {
		if (Configuration.applicationBase == null || new File(name).isAbsolute()) {
			return new File(name);
		}
		return new File(Configuration.applicationBase, name);
	}

}

