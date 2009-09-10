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

import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.sf.jailer.util.SqlUtil;


/**
 * Column of a table.
 * 
 * @author Ralf Wisser
 */
public class Column {

	/**
     * The name.
     */
    public final String name;
    
    /**
     * The type.
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
     * <code>true</code> if column is identity column.
     */
    public boolean isIdentityColumn = false;
    
    /**
     * SQL Expression for server-side column data filtering.
     */
    private String filterExpression = null;
    
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

    /**
     * Gets SQL expression for server-side column data filtering.
     * 
     * @return SQL expression for server-side column data filtering
     *         or <code>null</code>, if no filter is defined for this column
     */
    public String getFilterExpression() {
    	return filterExpression;
    }

    /**
     * Sets SQL expression for server-side column data filtering.
     * 
     * @param filterExpression SQL expression for server-side column data filtering
     *        or <code>null</code>, if no filter is defined for this column
     */
    public void setFilterExpression(String filterExpression) {
    	this.filterExpression = filterExpression;
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
    	
    	// work-around for bug 2849047
    	String normalizedcolumnDeclaration = columnDeclaration.replaceFirst("(\\(\\))? +[iI][dD][eE][nN][tT][iI][tT][yY]", "");
    	if (!normalizedcolumnDeclaration.equals(columnDeclaration)) {
    		columnDeclaration = normalizedcolumnDeclaration + " identity";
    	}
    	
    	boolean isIdent = false;
    	if (columnDeclaration.toLowerCase().endsWith(" identity")) {
    		isIdent = true;
    		columnDeclaration = columnDeclaration.substring(0, columnDeclaration.length() - 9).trim();
    	}
    	
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
	    Column column = new Column(name, type, size, precision);
	    column.isIdentityColumn = isIdent;
	    return column;
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
     * @param typeReplacement column types replacements
     */
    public String toSQL(String columnPrefix, Map<String, String> typeReplacement) {
    	String theType = type;
    	if (typeReplacement != null && typeReplacement.containsKey(theType)) {
    		theType = typeReplacement.get(theType);
    	}
        return (columnPrefix == null? "": columnPrefix) + name + " " + theType + (length == 0? "" : 
        	"(" + length + (precision >= 0? ", " + precision : "") + ")");
    }
 
    /**
     * Returns the primary key in SQL syntax.
     * 
     * @param columnPrefix an optional prefix for each PK-column
     */
    public String toSQL(String columnPrefix) {
    	return toSQL(columnPrefix, null);
    }
    
    /**
     * Returns a string representation of the column.
     */ 
    @Override
    public String toString() {
        return toSQL(null);
    }

}