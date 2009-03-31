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
import java.util.ArrayList;
import java.util.List;

import org.dbunit.Assertion;
import org.dbunit.DBTestCase;
import org.dbunit.PropertiesBasedJdbcDatabaseTester;
import org.dbunit.dataset.IDataSet;
import org.dbunit.dataset.ITable;
import org.dbunit.dataset.SortedTable;
import org.dbunit.dataset.xml.FlatXmlDataSet;

/**
 * DBMS aware test case.
 * 
 * @author Ralf Wisser
 */
public abstract class DbmsAwareTestCase extends DBTestCase {
	
	/**
	 * Command line arguments for database connection.
	 */
	protected final List<String> connectionArguments;
	
	/**
	 * Constructor.
	 * 
	 * @param name test case name
	 */
    public DbmsAwareTestCase(String name) {
        super(name);
        
        connectionArguments = new ArrayList<String>();
        
        connectionArguments.add(System.getProperty("DRIVER_CLASS"));
        connectionArguments.add(System.getProperty("DB_URL"));
        connectionArguments.add(System.getProperty("DB_USER"));
        connectionArguments.add(System.getProperty("DB_PASSWORD"));
        System.out.println(connectionArguments);

        System.setProperty( PropertiesBasedJdbcDatabaseTester.DBUNIT_DRIVER_CLASS, System.getProperty("DRIVER_CLASS"));
        System.setProperty( PropertiesBasedJdbcDatabaseTester.DBUNIT_CONNECTION_URL, System.getProperty("DB_URL"));
        System.setProperty( PropertiesBasedJdbcDatabaseTester.DBUNIT_USERNAME, System.getProperty("DB_USER"));
        System.setProperty( PropertiesBasedJdbcDatabaseTester.DBUNIT_PASSWORD, System.getProperty("DB_PASSWORD"));
    }

    /**
     * Gets data set holding the initial DB state.
     */
    protected IDataSet getDataSet() throws Exception {
        return new FlatXmlDataSet(new File("src/test/initial-dataset.xml"), false, true);
    }

    /**
     * Asserts that the database is in a given state.
     * 
     * @param expectedState file name of dataset holding the expected state
     */
    protected void assertDatabaseState(String expectedState) throws Exception {
		// Fetch database data after executing your code
        IDataSet databaseDataSet = getConnection().createDataSet();
        IDataSet initialDataSet = new FlatXmlDataSet(new File(expectedState), false, true);
        
        // compare tables
        for (ITable table: initialDataSet.getTables()) {
        	ITable actualTable = databaseDataSet.getTable(table.getTableMetaData().getTableName());
        	Assertion.assertEquals(new SortedTable(table), actualTable);
        }
	}

    /**
	 * Exports data.
	 * 
	 * @param extractionModel extraction model file name
	 * @param scope GLOBAL or SESSION_LOCAL
	 */
	protected void doExport(File datamodel, File extractionModel, File result, String scope) throws Exception {
		List<String> args = new ArrayList<String>();
		args.add("export");
		args.add(extractionModel.getCanonicalPath());
		args.addAll(connectionArguments);
		args.add("-e");
		args.add(result.getCanonicalPath());
		args.add("-datamodel");
		args.add(datamodel.getCanonicalPath());
		args.add("-scope");
		args.add(scope);
		System.out.println(args);
		Jailer.main(args.toArray(new String[0]));
	}

}