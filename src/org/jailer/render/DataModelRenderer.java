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

package org.jailer.render;

import org.jailer.database.StatementExecutor;
import org.jailer.datamodel.DataModel;

/**
 * Generates a human readable representation of the data-model.
 *  
 * @author Wisser
 */
public interface DataModelRenderer {

    /**
     * Generates a human readable representation of the data-model.
     * 
     * @param dataModel the data-model
     * @param statementExecutor for accessing the DB
     */
    void render(DataModel dataModel, StatementExecutor statementExecutor);

}
