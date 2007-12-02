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

package net.sf.jailer.database;

import java.util.ArrayList;
import java.util.List;

/**
 * Builds compact SQL-statements having the pattern:
 * HEAD ITEM {SEPARATOR ITEM}* TERMINATOR.
 *  
 * (for instance an 'INSERT' with a 'values'-list)
 *  
 * @author Wisser
 */
public class StatementBuilder {

    /**
     * The maximal size of a body-list.
     */
    private final int maxBodySize;

    /**
     * The head of the statement (for instance 'INSERT INTO T(...) VALUES ')
     */
    private String head = null;

    /**
     * Body-list (for instance a 'values'-list)
     */
    private List<String> body = new ArrayList<String>();

    /**
     * Separates the items.
     */
    private String separator;
    
    /**
     * Terminates the statement.
     */
    private String terminator;

    /**
     * Constructor.
     * 
     * @param maxBodySize the maximal size of a body-list
     */
    public StatementBuilder(int maxBodySize) {
        this.maxBodySize = maxBodySize;
    }
    
    /**
     * Checks whether an item is appendable to previously appended items.
     * 
     * @param head the statements head
     * @param item the item
     * @return <code>true</code> iff item is appendable
     */
    public boolean isAppendable(String head, String item) {
        return body.size() < maxBodySize && (this.head == null || this.head.equals(head));
    }

    /**
     * Builds the SQL-statement and resets the builder.
     * 
     * @return the SQL-statement
     */
    public String build() {
        if (this.head != null) {
            StringBuffer sqlStatement = new StringBuffer(head);
            boolean firstTime = true;
            for (String item: body) {
                if (!firstTime) {
                    sqlStatement.append(separator);
                }
                firstTime = false;
                sqlStatement.append(item);
            }
            sqlStatement.append(terminator);
            head = null;
            body.clear();
            return sqlStatement.toString();
        }
        return "";
    }

    /**
     * Appends an item.
     * 
     * @param head same head as previouslly appended, if any
     * @param item the item
     * @param separator separates the items
     * @param terminator terminates the statement
     */
    public void append(String head, String item, String separator, String terminator) {
        if (this.head != null && !this.head.equals(head)) {
            throw new IllegalStateException("can't append, '" + this.head + "'!='" + head + "'");
        }
        this.head = head;
        this.terminator = terminator;
        this.separator = separator;
        body.add(item);
    }
    
}
