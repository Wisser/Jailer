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

package org.jailer.datamodel;

/**
 * Describes a column of a table.
 * 
 * @author Wisser
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
     * The length (for VARCHAR, DECIMAL, ...) or <code>0</code> if type-length is not variable
     */
    public final int length;
    
    /**
     * Constructor.
     * 
     * @param name the name (upper-case)
     * @param type the type (in SQL)
     * @param length the length (for VARCHAR, DECIMAL, ...) or <code>0</code> if type-length is not variable
     */
    public Column(String name, String type, int length) {
        this.name = name;
        this.type = type;
        this.length = length;
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
        return (columnPrefix == null? "": columnPrefix) + name + " " + type + (length == 0? "" : "(" + length + ")");
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
        if ("VARCHAR".equals(type)) {
            return "'0'";
        }
        return "0";
    }
    
}