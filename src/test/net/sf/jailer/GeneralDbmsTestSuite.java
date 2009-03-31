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
		File baseDir = new File(System.getProperty("BASE_DIR"));

		for (String testDirName: baseDir.list()) {
			File testDir = new File(baseDir, testDirName);
			if (testDir.isDirectory() && !"datamodel".equals(testDirName) && !testDirName.startsWith(".")) {
				suite.addTest(new ExportTestCase("testDatasetExport", testDir, new File(baseDir, "datamodel")));
			}
		}
		return suite;
	}
	
}
