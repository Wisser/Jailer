/*
 * Copyright 2007 - 2012 the original author or authors.
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
package net.sf.jailer.domainmodel;

import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;

import net.sf.jailer.datamodel.Table;

/**
 * Set of functionally related tables. Part of a {@link DomainModel}.
 *  
 * @author Ralf Wisser
 */
public class Domain implements Comparable<Domain> {

    /**
     * The domain name.
     */
    public final String name;
    
    /**
     * Set of tables in this domain.
     */
    public final Set<Table> tables;
    
    /**
     * Set of sub-domains.
     */
    Set<Domain> subDomains = new TreeSet<Domain>();
    
    /**
     * Set of super-domains.
     */
    Set<Domain> superDomains = new TreeSet<Domain>();
    
    /**
     * Constructor.
     * 
     * @param name the domain name
     * @param tables set of tables in this domain
     */
    Domain(String name, Set<Table> tables) {
        this.name = name;
        this.tables = tables;
    }
    
    /**
     * Gets sub-domains.
     * 
     * @return set of sub-domains
     */
    public Set<Domain> getSubDomains() {
        return subDomains;
    }
    
    /**
     * Gets super-domains.
     * 
     * @return set of super-domains
     */
    public Set<Domain> getSuperDomains() {
        return superDomains;
    }
    
    /**
     * Checks wether this is a sub domain of a given domain.
     * 
     * @param domain the domain
     * @return <code>true</code> iff this is a sub domain of domain
     */
    public boolean isSubDomainOf(Domain domain) {
        Set<Domain> superClosure = new HashSet<Domain>(superDomains);
        boolean ready = false;
        while (!ready) {
            Set<Domain> increment = new HashSet<Domain>(); 
            for (Domain superDomain: superClosure) {
                increment.addAll(superDomain.superDomains);
            }
            increment.removeAll(superClosure);
            superClosure.addAll(increment);
            if (increment.isEmpty()) {
                break;
            }
        }
        return superClosure.contains(domain);
    }

    public int compareTo(Domain o) {
        return name.compareTo(o.name);
    }
    
    /**
     * The hash-code.
     */
    public int hashCode() {
        return name.hashCode();
    }

    /**
     * Compares domains.
     */
    public boolean equals(Object other) {
        if (other instanceof Domain) {
            return name.equals(((Domain) other).name);
        }
        return false;
    }
 
    /**
     * Stringifies the domain.
     */
    public String toString() {
        return "Domain " + name;
    }

}



