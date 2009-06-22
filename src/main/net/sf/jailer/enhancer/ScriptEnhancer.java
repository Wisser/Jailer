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
package net.sf.jailer.enhancer;

import java.io.IOException;
import java.io.Writer;
import java.sql.SQLException;
import java.util.Set;

import net.sf.jailer.ScriptType;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.entitygraph.EntityGraph;


/**
 * Enhances the generated SQL-export-script with statements or comments.
 * 
 * @author Ralf Wisser
 */
public interface ScriptEnhancer {

    /**
     * Adds comments to the script.
     * 
     * @param script to write into the script
     * @param scriptType the type of the script
     * @param session for executing SQL-statements in the source-DB
     * @param progress the export progess
     */
    void addComments(Writer script, ScriptType scriptType, Session session, EntityGraph entityGraph, Set<Table> progress) throws IOException, SQLException;
    
    /**
     * Adds statements at top of the script.
     * 
     * @param script to write into the script
     * @param scriptType the type of the script
     * @param session for executing SQL-statements in the source-DB
     * @param progress the export progess
     */
    void addProlog(Writer script, ScriptType scriptType, Session session, EntityGraph entityGraph, Set<Table> progress) throws IOException, SQLException;
    
    /**
     * Adds comments at bottom of the script.
     * 
     * @param script to write into the script
     * @param scriptType the type of the script
     * @param session for executing SQL-statements in the source-DB
     * @param progress the export progess
     */
    void addEpilog(Writer script, ScriptType scriptType, Session session, EntityGraph entityGraph, Set<Table> progress) throws IOException, SQLException;
    
}
