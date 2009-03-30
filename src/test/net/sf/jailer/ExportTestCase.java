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


/**
 * Tests export feature.
 * 
 * @author Ralf Wisser
 */
public class ExportTestCase extends DbmsAwareTestCase {

	/**
	 * Constructor.
	 * 
	 * @param name test case name
	 */
	public ExportTestCase(String name) {
		super(name);
	}

	/**
	 * Tests whether DB is set up correctly.
	 */
	public void testDatabaseSetup() throws Exception {
		assertDatabaseState("src/test/initial-dataset.xml");
	}

	/**
	 * Tests export of employee SCOTT without restriction.
	 */
	public void testExport() throws Exception {
		doExport("src/test/export-1.csv", "SESSION_LOCAL");
	}

}
