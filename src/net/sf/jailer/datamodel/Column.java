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

package net.sf.jailer.datamodel;

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.sf.jailer.util.SqlUtil;


/**
 * Describes a column of a table.
 * 
 * @author Ralf Wisser
 */
public class Column {

	/**
     * The name (upper-case).
     */
    public final String name;
    
    /**
     * The type (in SQL).
     */
    public final String type;
    
    /**
     * The length (for VARCHAR, DECIMAL, ...) or <code>0</code> if type-length is not variable.
     */
    public final int length;
    
    /**
     * The precision (for DECIMAL, NUMBER ...) or <code>-1</code> if precision is not variable.
     */
    public final int precision;
    
    /**
     * Maps columns types in SQL syntax to 'null'-value of this type.
     */
    private static Map<String, String> nullValues = new HashMap<String, String>();
    static {
    	nullValues.put("VARCHAR", "'0'");
    	nullValues.put("VARCHAR2", "'0'");
    	nullValues.put("CHAR", "'0'");
    }

    /**
     * Null-value for this column, or <code>null</code> if null-value is not known. (use nullValues then)
     */
    public String nullValue = null;
    
    /**
     * Constructor.
     * 
     * @param name the name (upper-case)
     * @param type the type (in SQL)
     * @param length the length (for VARCHAR, DECIMAL, ...) or <code>0</code> if type-length is not variable
     * @param precision the precision (for DECIMAL, NUMBER ...) or <code>-1</code> if precision is not variable
     */
    public Column(String name, String type, int length, int precision) {
        this.name = name;
        this.type = type;
        this.length = length;
        this.precision = precision;
    }
 
    private static Pattern typeWithSizeAndPrecision = Pattern.compile("([^ ]+) +([^ \\(]+) *\\( *([0-9]+) *, *([0-9]+) *\\)");
    private static Pattern typeWithSize = Pattern.compile("([^ ]+) +([^ \\(]+) *\\( *([0-9]+) *\\)");
    private static Pattern typeWithoutSize = Pattern.compile("([^ ]+) +([^ \\(]+)");
	
    /**
     * Parses a column declaration in SQL syntax.
     * 
     * @param columnDeclaration the column declaration in SQL syntax
     * @return the column
     */
    public static Column parse(String columnDeclaration) {
    	columnDeclaration = columnDeclaration.trim();

    	Character quote = null;
    	if (columnDeclaration.length() > 0 && SqlUtil.LETTERS_AND_DIGITS.indexOf(columnDeclaration.charAt(0)) < 0) {
    		quote = columnDeclaration.charAt(0);
    		if (columnDeclaration.substring(1).indexOf(quote) >= 0) {
	    		StringBuilder sb = new StringBuilder();
	    		sb.append(quote);
	    		boolean inScope = true;
	    		for (int i = 1; i < columnDeclaration.length(); ++i) {
	    			char c = columnDeclaration.charAt(i);
	    			if (c == quote) {
	    				inScope = false;
	    			}
	    			if (inScope && c == ' ') {
	    				c = '\n';
	    			}
	    			sb.append(c);
	    		}
	    		columnDeclaration = sb.toString();
    		}
    	}
    	
    	String name, type;
	    int size = 0;
	    int precision = -1;
	    Matcher matcher = typeWithSizeAndPrecision.matcher(columnDeclaration);
	    if (matcher.matches()) {
	        name = matcher.group(1);
	        type = matcher.group(2);
	        size = Integer.parseInt(matcher.group(3));
	        precision = Integer.parseInt(matcher.group(4));
	    } else {
	        matcher = typeWithSize.matcher(columnDeclaration);
	        if (matcher.matches()) {
	            name = matcher.group(1);
	            type = matcher.group(2);
	            size = Integer.parseInt(matcher.group(3));
	        } else {
	            matcher = typeWithoutSize.matcher(columnDeclaration);
	            if (matcher.matches()) {
	                name = matcher.group(1);
	                type = matcher.group(2);
	            } else {
	                throw new RuntimeException("can't parse primary-key: " + columnDeclaration);
	            }
	        }
	    }
	    if (quote != null) {
	    	name = name.replace('\n', ' ');
	    }
	    return new Column(name, type, size, precision);
    }
    
    /**
     * Compares two columns.
     */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Column) {
            return name.equals(((Column) obj).name) &&
                   type.equals(((Column) obj).type) &&
                   length == ((Column) obj).length;
        }
        return super.equals(obj);
    }

    @Override
    public int hashCode() {
        return toSQL(null).hashCode();
    }

    /**
     * Returns the primary key in SQL syntax.
     * 
     * @param columnPrefix an optional prefix for each PK-column
     */
    public String toSQL(String columnPrefix) {
        return (columnPrefix == null? "": columnPrefix) + name + " " + type + (length == 0? "" : 
        	"(" + length + (precision >= 0? ", " + precision : "") + ")");
    }
 
    /**
     * Returns a string representation of the column.
     */ 
    @Override
    public String toString() {
        return toSQL(null);
    }

    /**
     * Gets a 'null'-value depending on columns type in SQL syntax.
     * 
     * @return a 'null'-value
     */
    public String getNullValue() {
    	if (nullValue != null) {
    		return nullValue;
    	}
        String nv = nullValues.get(type.toUpperCase());
    	if (nv != null) {
            return nv;
        }
        return "0";
    }
    
}