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

import net.sf.jailer.modelbuilder.ModelElementFinder;

/**
 * Element of data-model ({@link Table} or {@link Association}).
 * 
 * @author Wisser
 */
public abstract class ModelElement {

    /**
     * Name of the {@link ModelElementFinder} who defined this element.
     * Empty string for manually defined elements.
     */
    private String author = "";

    /**
     * Gets the name of the {@link ModelElementFinder} who defined this element.
     * Empty string for manually defined elements.
     * 
     * @return the name of the {@link ModelElementFinder} who defined this element.
     * Empty string for manually defined elements.
     */
    public String getAuthor() {
        return author;
    }

    /**
     * Sets the name of the {@link ModelElementFinder} who defined this element.
     * Empty string for manually defined elements.
     * 
     * @param author the name of the {@link ModelElementFinder} who defined this element.
     * Empty string for manually defined elements.
     */
    public void setAuthor(String author) {
        this.author = author;
    }
    
}
