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
package net.sf.jailer.api_example;

import java.io.File;
import java.io.IOException;
import java.sql.SQLException;

import net.sf.jailer.api.Importer;
import net.sf.jailer.api.Subsetter;
import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.database.BasicDataSource;
import net.sf.jailer.subsetting.ScriptFormat;

/**
 * Jailer API Example. <br>
 * 
 * Extracts some data from the "demo-scott" database
 * and imports it into another database "demo-scott-subset".
 */
public class APIExample {
	
	// JDBC connection pool size
	private static final int POOL_SIZE = 10;

	// The subsetter
	private static Subsetter subsetter = 
		new Subsetter(
			new BasicDataSource(
					"org.h2.Driver", "jdbc:h2:" + new File("demo-scott").getAbsolutePath(), "sa", "",
					POOL_SIZE,
					new File("lib/h2-2.1.212.jar")),
			null,
			APIExample.class.getResource("Demo-Scott"),
			APIExample.class.getResource("Demo-Scott.jm"),
			ScriptFormat.SQL);
	
	// The importer
	private static Importer importer =
		new Importer(
			new BasicDataSource(
				"org.h2.Driver", "jdbc:h2:" + new File("demo-scott-subset").getAbsolutePath(), "sa", "",
				10,
				new File("lib/h2-2.1.212.jar")));

	/**
	 * Exports data related with employee "SCOTT"
	 * and imports it into another database.
	 */
	public static void main(String[] args) throws SQLException, IOException {
		File exportScriptFile = Configuration.getInstance().createTempFile();
		
		subsetter.setUpsertOnly(true); // overwrite previous data
		subsetter.execute("NAME='SCOTT'", exportScriptFile);
		
		importer.execute(exportScriptFile);
		
		exportScriptFile.delete();
	}

}
