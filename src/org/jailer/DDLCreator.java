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

package org.jailer;

import org.jailer.datamodel.DataModel;
import org.jailer.util.PrintUtil;

/**
 * Creates the DDL for the working-tables.
 * 
 * @author Wisser
 */
public class DDLCreator {

    /**
     * Creates the DDL for the working-tables.
     */
    public static void createDDL() throws Exception {
        DataModel dataModel = new DataModel();
        
        String template = "script/ddl-template.sql";
        Object[] arguments = new Object[] { 
                dataModel.getUniversalPrimaryKey().toSQL(null),
                dataModel.getUniversalPrimaryKey().toSQL("PRE_", false),
                dataModel.getUniversalPrimaryKey().toSQL("FROM_"),
                dataModel.getUniversalPrimaryKey().toSQL("TO_"),
                dataModel.getUniversalPrimaryKey().columnList(null),
                dataModel.getUniversalPrimaryKey().columnList("FROM_"),
                dataModel.getUniversalPrimaryKey().columnList("TO_")
            };
        String ddl = PrintUtil.applyTemplate(template, arguments);
        
        System.out.println(ddl);
    }

}
