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

import junit.framework.TestSuite;

/**
 * DB-based tests.
 * 
 * @author Ralf Wisser
 */
public class GeneralDbmsTestSuite extends TestSuite {

	/**
	 * Creates the suite.
	 * 
	 * @return the suite
	 */
	public static TestSuite suite() {
		TestSuite suite = new TestSuite();
		for (String baseDirName: System.getProperty("BASE_DIR").split(":")) {
			File baseDir = new File(baseDirName);
	
			for (String testDirName: baseDir.list()) {
				File testDir = new File(baseDir, testDirName);
				if (testDir.isDirectory() && !"datamodel".equals(testDirName) && !testDirName.startsWith(".")) {
					File expectedResult = new File(testDir, "expected-dataset.xml");
					if (expectedResult.exists()) {
						suite.addTest(new ExportTestCase("testExport", testDir, new File(baseDir, "datamodel"), new File(baseDir, "initial-dataset.xml"), expectedResult, "DBUNIT_FLAT_XML", "SESSION_LOCAL", false));
						suite.addTest(new ExportTestCase("testExport", testDir, new File(baseDir, "datamodel"), new File(baseDir, "initial-dataset.xml"), expectedResult, "DBUNIT_FLAT_XML", "GLOBAL", false));
						suite.addTest(new ExportTestCase("testExport", testDir, new File(baseDir, "datamodel"), new File(baseDir, "initial-dataset.xml"), expectedResult, "SQL", "SESSION_LOCAL", false));
						suite.addTest(new ExportTestCase("testExport", testDir, new File(baseDir, "datamodel"), new File(baseDir, "initial-dataset.xml"), expectedResult, "SQL", "GLOBAL", false));
						suite.addTest(new ExportTestCase("testExport", testDir, new File(baseDir, "datamodel"), new File(baseDir, "initial-dataset.xml"), expectedResult, "SQL", "SESSION_LOCAL", true));
						suite.addTest(new ExportTestCase("testExport", testDir, new File(baseDir, "datamodel"), new File(baseDir, "initial-dataset.xml"), expectedResult, "SQL", "GLOBAL", true));
					}
					expectedResult = new File(testDir, "expected-xml.xml");
					if (expectedResult.exists()) {
						suite.addTest(new ExportTestCase("testExport", testDir, new File(baseDir, "datamodel"), new File(baseDir, "initial-dataset.xml"), expectedResult, "XML", "SESSION_LOCAL", false));
						suite.addTest(new ExportTestCase("testExport", testDir, new File(baseDir, "datamodel"), new File(baseDir, "initial-dataset.xml"), expectedResult, "XML", "GLOBAL", false));
					}
					expectedResult = new File(testDir, "expected-remaining-dataset.xml");
					if (expectedResult.exists()) {
						suite.addTest(new DeletionTestCase("testDeletion", testDir, new File(baseDir, "datamodel"), new File(baseDir, "initial-dataset.xml"), expectedResult, "SESSION_LOCAL"));
						suite.addTest(new DeletionTestCase("testDeletion", testDir, new File(baseDir, "datamodel"), new File(baseDir, "initial-dataset.xml"), expectedResult, "GLOBAL"));
					}
				}
			}
		}
		return suite;
	}
	
}
