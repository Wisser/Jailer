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

package net.sf.jailer.subsetting;

import java.sql.SQLException;

import net.sf.jailer.database.Session.ResultSetReader;
import net.sf.jailer.datamodel.Table;

/**
 * Creates transformer (as {@link ResultSetReader} which transforms rows into an external representation.
 * 
 * @author Ralf Wisser
 */
public interface TransformerFactory {

	/**
	 * Creates transformer (as {@link ResultSetReader} which 
	 * transforms rows of a given table into an external representation.
	 * 
	 * @param table the table
	 * @return a transformer
	 */
	ResultSetReader create(Table table) throws SQLException;
	
}
