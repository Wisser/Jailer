/*
 * Copyright 2007 the original author or authors.
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

import java.io.File;
import java.io.FileWriter;
import java.io.Writer;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;

import net.sf.jailer.database.StatementExecutor;
import net.sf.jailer.util.SqlScriptExecutor;

import org.dbunit.dataset.IDataSet;
import org.dbunit.dataset.xml.FlatXmlDataSet;


/**
 * Tests deletion feature.
 * 
 * @author Ralf Wisser
 */
public class DeletionTestCase extends DbmsAwareTestCase {

	private final File testDir;
	private final File datamodelDir;
	private final File initialDataset;
	private final File expectedResult;
	private final String scope;
		
	/**
	 * Constructor.
	 * 
	 * @param name test case name
	 * @param initialDataset initial data
	 */
	public DeletionTestCase(String name, File testDir, File datamodelDir, File initialDataset, File expectedResult, String scope) {
		super(name);
		this.testDir = testDir;
		this.datamodelDir = datamodelDir;
		this.initialDataset = initialDataset;
		this.expectedResult = expectedResult;
		this.scope = scope;
	}

	/**
     * Gets data set holding the initial DB state.
     */
    protected IDataSet getDataSet() throws Exception {
        return new FlatXmlDataSet(initialDataset, false, true);
    }

	/**
	 * Tests export.
	 */
	public void testDeletion() throws Exception {
		File resultDir = new File(testDir, "result");
		if (!resultDir.exists()) {
			resultDir.mkdir();
		}
		File result = null;
		result = new File(resultDir, "delete.sql");
		File insertResult = new File(resultDir, "insert.sql");
		
		if (doDeletion(datamodelDir, new File(testDir, "extractionmodel.csv"), result, insertResult, scope)) {
			StatementExecutor statementExecutor = new StatementExecutor(connectionArguments.get(0), connectionArguments.get(1), connectionArguments.get(2), connectionArguments.get(3));
			SqlScriptExecutor.executeScript(result.getCanonicalPath(), statementExecutor);
			statementExecutor.shutDown();
			
			File actualState = new File(resultDir, "remaining-state.xml");
			IDataSet depDataset = getConnection().createDataSet(getDataSet().getTableNames());
			Writer out = new FileWriter(actualState);
		    FlatXmlDataSet.write(depDataset, out, Charset.defaultCharset().name());
		    out.close();

			assertEqualDatasets(expectedResult, actualState);
		}
	}
	
	/**
	 * Deletes data.
	 * 
	 * @param extractionModel extraction model file name
	 * @param scope GLOBAL or SESSION_LOCAL
	 */
	private boolean doDeletion(File datamodel, File extractionModel, File result, File insertResult, String scope) throws Exception {
		if ("SESSION_LOCAL".equals(scope)) {
			if (!"yes".equals(System.getProperty("DB_SUPPORTS_SESSION_LOCAL"))) {
				return false;
			}
		} else {
			List<String> ddlArgs = new ArrayList<String>();
			ddlArgs.add("create-ddl");
			ddlArgs.addAll(connectionArguments);
			ddlArgs.add("-datamodel");
			ddlArgs.add(datamodel.getCanonicalPath());
			Jailer.main(ddlArgs.toArray(new String[0]));
		}
		List<String> args = new ArrayList<String>();
		args.add("export");
		args.add(extractionModel.getCanonicalPath());
		args.addAll(connectionArguments);
		args.add("-d");
		args.add(result.getCanonicalPath());
		args.add("-datamodel");
		args.add(datamodel.getCanonicalPath());
		args.add("-script-enhancer");
		args.add(datamodel.getParent());
		args.add("-scope");
		args.add(scope);
		args.add("-e");
		args.add(insertResult.getAbsolutePath());
		System.out.println(args);
		Jailer.main(args.toArray(new String[0]));
		return true;
	}

	public String toString() {
		return super.toString() + ": " + testDir + ", " + scope;
	}
	
}
