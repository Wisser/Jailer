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


/**
 * Enumeration of association's cardinalities.
 * 
 * @author Ralf Wisser
 */
public enum Cardinality {
    
    ONE_TO_ONE("1:1"),
    ONE_TO_MANY("1:n"),
    MANY_TO_ONE("n:1"),
    MANY_TO_MANY("n:m"); 

    /**
     * The name of the cardinality.
     */
    private final String name;
    
    /**
     * Constructor.
     * 
     * @param name the name of the cardinality
     */
    private Cardinality(String name) {
        this.name = name;
    }

    /**
     * Gets the cardinality of the reversal association.
     * 
     * @return the cardinality of the reversal association
     */
    public Cardinality reverse() {
        if (this == ONE_TO_ONE) return ONE_TO_ONE;
        if (this == ONE_TO_MANY) return MANY_TO_ONE;
        if (this == MANY_TO_ONE) return ONE_TO_MANY;
        if (this == MANY_TO_MANY) return MANY_TO_MANY;
        return null;
    }

    /**
     * Parses a stringified cardinality.
     * 
     * @param cardinality cardinality name
     * @return cardinality with given name
     */
    public static Cardinality parse(String cardinality) {
        if (cardinality == null || "".equals(cardinality)) {
            return null;
        }
        for (Cardinality card: Cardinality.values()) {
            if (card.name.equalsIgnoreCase(cardinality)) {
                return card;
            }
        }
        throw new RuntimeException("unknown cardinality: " + cardinality);
    }
    
    /**
     * Stringifies cardinality.
     */
    public String toString() {
        return name;
    }
    
}
