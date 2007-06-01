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

import java.util.ArrayList;
import java.util.List;

/**
 * Factory for {@link PrimaryKey}s.
 * Builds the universal primary key as a super-set of all created primary key.
 * 
 * @author Wisser
 */
public class PrimaryKeyFactory {

    /**
     * {@link #getUniversalPrimaryKey()} closes the factory, no further creation of PKs is allowed then.
     */
    private boolean closed = false; 
    
    /**
     * A super-set of all columns of created primary-keys.
     */
    private PrimaryKey universalPrimaryKey = new PrimaryKey(new ArrayList<Column>());

    /**
     * Constructs a new primary-key.
     * 
     * @return a newly created primary-key
     * 
     * @exception IllegalStateException if factory is closed
     */
    public PrimaryKey createPrimaryKey(List<Column> columns) {
        if (closed) {
            throw new IllegalStateException("factory is closed");
        }
        PrimaryKey primaryKey = new PrimaryKey(columns);
        
        int n = 0;
        for (int i = 0; i <  universalPrimaryKey.getColumns().size(); ++i) {
            Column uPKColumn =  universalPrimaryKey.getColumns().get(i);
            Column column =  columns.get(n);
            if (uPKColumn.type.equals(column.type)) {
                if (column.length > 0 && column.length > uPKColumn.length) {
                    // increase size
                    universalPrimaryKey.getColumns().set(i, new Column(uPKColumn.name, uPKColumn.type, column.length));
                }
                ++n;
                if (n >= columns.size()) {
                    break;
                }
            }
        }
        // add new columns to universal primary key
        for (; n < columns.size(); ++n) {
            Column column = columns.get(n);
            universalPrimaryKey.getColumns().add(new Column(createUniqueUPKName(), column.type, column.length));
        }
        return primaryKey;
    }
    
    /**
     * Creates a unique name for a new universal primary key column.
     * 
     * @return a unique name for a new universal primary key column
     */
    private String createUniqueUPKName() {
        return "PK" + universalPrimaryKey.getColumns().size();
    }

    /**
     * Gets the primary-key to be used for the entity-table and closes the factory.
     * 
     * @return the universal primary key
     */
    public PrimaryKey getUniversalPrimaryKey() {
        closed = true;
        return universalPrimaryKey;
    }
    
}
