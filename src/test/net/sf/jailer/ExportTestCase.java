/*
 * Copyright 2007 - 2012 the original author or authors.
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
import java.util.ArrayList;
import java.util.List;

import net.sf.jailer.database.Session;
import net.sf.jailer.util.SqlScriptExecutor;

import org.dbunit.dataset.IDataSet;
import org.dbunit.dataset.xml.FlatXmlDataSet;
import org.dbunit.operation.DatabaseOperation;


/**
 * Tests export feature.
 * 
 * @author Ralf Wisser
 */
public class ExportTestCase extends DbmsAwareTestCase {

	private final File testDir;
	private final File datamodelDir;
	private final File initialDataset;
	private final File expectedResult;
	private final String format;
	private final String scope;
	private final boolean overwrite;
		
	/**
	 * Constructor.
	 * 
	 * @param name test case name
	 * @param initialDataset initial data
	 */
	public ExportTestCase(String name, File testDir, File datamodelDir, File initialDataset, File expectedResult, String format, String scope, boolean overwrite) {
		super(name);
		this.testDir = testDir;
		this.datamodelDir = datamodelDir;
		this.initialDataset = initialDataset;
		this.expectedResult = expectedResult;
		this.scope = scope;
		this.format = format;
		this.overwrite = overwrite;
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
	public void testExport() throws Exception {
		File resultDir = new File(testDir, "result");
		if (!resultDir.exists()) {
			resultDir.mkdir();
		}
		File result = null;
		if ("SQL".equals(format)) {
			result = new File(resultDir, "export.sql");
		}
		if ("DBUNIT_FLAT_XML".equals(format)) {
			result = new File(resultDir, "export-dataset.xml");
		}
		if ("XML".equals(format)) {
			result = new File(resultDir, "export.xml");
		}
		if (!doExport(datamodelDir, new File(testDir, "extractionmodel.csv"), result, scope, format, overwrite)) {
			return;
		}
		if (format.equals("DBUNIT_FLAT_XML")) {
			assertEqualDatasets(expectedResult, result);
		}
		if (format.equals("SQL")) {
			DatabaseOperation.DELETE_ALL.execute(getConnection(), getDataSet());
			Session statementExecutor = new Session(connectionArguments.get(0), connectionArguments.get(1), connectionArguments.get(2), connectionArguments.get(3));
			SqlScriptExecutor.executeScript(result.getCanonicalPath(), statementExecutor);
			statementExecutor.shutDown();
			assertDatabaseState(expectedResult.getCanonicalPath());
		}
		if (format.equals("XML")) {
			assertTrue(XmlDocumentComparator.equals(
					XmlDocumentComparator.parseXmlDocument(new File(testDir, "expected-xml.xml")),
					XmlDocumentComparator.parseXmlDocument(result)));
		}
	}
	
    /**
	 * Exports data.
	 * 
	 * @param extractionModel extraction model file name
	 * @param scope GLOBAL or SESSION_LOCAL
     * @param format 
     * @param overwrite 
	 */
	private boolean doExport(File datamodel, File extractionModel, File result, String scope, String format, boolean overwrite) throws Exception {
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
		args.add("-e");
		args.add(result.getCanonicalPath());
		args.add("-datamodel");
		args.add(datamodel.getCanonicalPath());
		args.add("-script-enhancer");
		args.add(datamodel.getParent());
		args.add("-scope");
		args.add(scope);
		args.add("-format");
		args.add(format);
		if (overwrite) {
			args.add("-upsert-only");
		}
		System.out.println(args);
		Jailer.main(args.toArray(new String[0]));
		return true;
	}

	public String toString() {
		return super.toString() + ": " + testDir + ", " + format + ", " + scope;
	}
	
}
