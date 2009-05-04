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
package net.sf.jailer.modelbuilder;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;

/**
 * Finds model-elements (tables and associations).
 *  
 * @author Ralf Wisser
 */
public interface ModelElementFinder {

    /**
     * Finds a set of {@link Table}s.
     * 
     * @param statementExecutor the statement executor for executing SQL-statements 
     * @return a set of {@link Table}s
     * 
     * @throws Exception on each error
     */
    Collection<Table> findTables(Session statementExecutor) throws Exception;

    /**
     * Finds the {@link Column}s of a given {@link Table}.
     *
     * @param table the table
     * @param statementExecutor the statement executor for executing SQL-statements 
     * 
     * @throws Exception on each error
     */
    List<Column> findColumns(Table table, Session statementExecutor) throws Exception;

    /**
     * Finds a set of {@link Association}s.
     * 
     * @param dataModel model containing already known elements. 
     * @param statementExecutor the statement executor for executing SQL-statements 
     * @param namingSuggestion to put naming suggestions for associations into
     * @return a set of {@link Association}s
     * 
     * @throws Exception on each error
     */
    Collection<Association> findAssociations(DataModel dataModel, Map<Association, String[]> namingSuggestions, Session statementExecutor) throws Exception;
    
}
