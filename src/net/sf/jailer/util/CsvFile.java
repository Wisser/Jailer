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

package net.sf.jailer.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

/**
 * The content of a CSV-file.
 * 
 * @author Wisser
 */
public class CsvFile {

    /**
     * A line in a CSV-file.
     */
    public static class Line {
        
        /**
         * Describes the position of the line in a file.
         */
        public final String location;
        
        /**
         * The cells.
         */
        public final List<String> cells;
        
        /**
         * Length of the line.
         */
        public int length;
        
        /**
         * Constructor.
         * 
         * @param location describes the position of the line in a file
         * @param cells the cells
         */
        public Line(String location, List<String> cells) {
            this.location = location;
            this.cells = cells;
            int num = 0;
            int l = 0;
            for (String s : cells) {
            	++num;
            	if (s != null && s.trim().length() > 0) {
            		l = num;
            	}
            }
            this.length = l;
        }

        /**
         * Line as String.
         */
        public String toString() {
        	int num = 0;
        	int l = 0;
            for (String s : cells) {
            	++num;
            	if (s != null && s.trim().length() > 0) {
            		l = num;
            	}
            }
            StringBuffer sb = new StringBuffer();
            for (int i = 0; i <= l; ++i) {
            	sb.append(cells.get(i) + "; ");
            }
            return sb.toString();
        }
    };
    
    /**
     * List of lines.
     */
    private List<Line> rows = new ArrayList<Line>();
    
    /**
     * Constructor.
     * 
     * @param csvFile the csv file
     * @throws Exception
     */
    public CsvFile(File csvFile) throws Exception {
        BufferedReader reader = new BufferedReader(new FileReader(csvFile));
        String line = null;
        int lineNr = 0;
        while ((line = reader.readLine()) != null) {
            ++lineNr;
            if (line.trim().length() == 0) {
                continue;
            }
            if (line.trim().startsWith("#include ")) {
                String includeFile = line.trim().substring(9).trim();
                rows.addAll(new CsvFile(new File(csvFile.getParent() + File.separator + includeFile)).rows);
                continue;
            }
            if (line.trim().startsWith("#")) {
                continue;
            }
            List<String> row = new ArrayList<String>();
            final String NOSEP = "///--n-o-s-e-p--///";
            line = line.replace("\\;", NOSEP);
            String[] col = line.split(";");
            for (int i = 0; i < col.length; ++i) {
                String s = col[i].replace(NOSEP, ";");
                if (s.startsWith("\"")) {
                    s = s.substring(1);
                }
                if (s.endsWith("\"")) {
                    s = s.substring(0, s.length() - 1);
                }
                row.add(s.trim());
            }
            while (row.size() < 100) {
                row.add("");
            }
            rows.add(new Line("line " + lineNr + ", file " + csvFile.getName(), row));
        }
        reader.close();
    }
    
    /**
     * Gets the list of lines.
     * 
     * @return list of lists of cell-contents
     */
    public List<Line> getLines() {
        return rows;
    }

    /**
     * Checks if a certain line can be found in this file.
     * 
     * @param the line
     * @return <code>true</code> iff this file contains the line 
     */
    public boolean contains(String[] line) {
        for (Line l: getLines()) {
            boolean differenceFound = false;
            int i = 0;
            for (String cell: line) {
                if (cell != null && !cell.equals(l.cells.get(i))) {
                    differenceFound = true;
                    break;
                }
                ++i;
            }
            if (!differenceFound) {
                return true;
            }
        }
        return false;
    }
    
}
