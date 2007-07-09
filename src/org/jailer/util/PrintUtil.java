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

package org.jailer.util;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.jailer.datamodel.Table;


/**
 * Utility for printing.
 * 
 * @author Wisser
 */
public class PrintUtil {
    
    /**
     * Converts a set of tables into a string.
     * 
     * @param tables the set
     */
    public static String tableSetAsString(Set<Table> tables) {
        return tableSetAsString(tables, "      ");
    }
    
    /**
     * Converts a set of tables into a string.
     * 
     * @param tables the set
     */
    public static String tableSetAsString(Set<Table> tables, String linePrefix) {
        List<String> tableNames = new ArrayList<String>();
        for (Table table: tables) {
            tableNames.add(table.getName());
        }
        Collections.sort(tableNames);
        StringBuffer str = new StringBuffer();
        str.append(" { ");
        int i = 1;
        for (String tableName: tableNames) {
            if (i > 1) {
                str.append(", ");
            }
            if (i % 5 == 0 && linePrefix != null) {
                str.append("\n" + linePrefix);
            }
            str.append(tableName);
            ++i;
        }
        str.append(" }");
        return str.toString();
    }

    /**
     * Cache for {@link #applyTemplate(String, Object[])}.
     */
    private static Map<String, String> templateCache = new HashMap<String, String>();
    
    /**
     * Applies arguments to template.
     * 
     * @param template file name of template
     * @param arguments the arguments
     * 
     * @return template with arguments filled in
     */
    public static String applyTemplate(String template, Object[] arguments) throws FileNotFoundException, IOException {
        String sb = templateCache.get(template);
        if (sb == null) {
            sb = loadFile(template);
            templateCache.put(template, sb);
        }
        
        return MessageFormat.format(sb, arguments);
    }

    /**
     * Loads a file.
     * 
     * @param file the file to load
     * @return content of file
     * @throws FileNotFoundException
     * @throws IOException
     */
    public static String loadFile(String file) throws FileNotFoundException, IOException {
        StringBuffer sb;
        sb = new StringBuffer();
        BufferedReader reader = new BufferedReader(new FileReader(file));
        String line = null;
        while ((line = reader.readLine()) != null) {
            sb.append(line + "\n");
        }
        reader.close();
        return sb.toString();
    }

}
