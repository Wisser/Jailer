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

import org.jailer.restrictionmodel.RestrictionModel;
import org.jailer.util.SqlUtil;

/**
 * Describes an association between two database-tables.
 * 
 * @author Wisser
 */
public class Association extends ModelElement {

    /**
     * The source table.
     */
    public final Table source;

    /**
     * The destination table.
     */
    public final Table destination;

    /**
     * The join-condition for joining source with destination table.
     */
    private final String joinCondition;
    
    /**
     * The cardinality.
     */
    private final Cardinality cardinality;
    
    /**
     * Whether or not to insert source-rows before destination rows
     * in order to prevent foreing-key-constraint voilation.
     */
    private final boolean insertSourceBeforeDestination;
    
    /**
     * Whether or not to insert destination-rows before source-rows
     * in order to prevent foreing-key-constraint voilation.
     */
    private final boolean insertDestinationBeforeSource;
    
    /**
     * <code>true</code> for reversed association.
     */
    public final boolean reversed;

    /**
     * The name of the association.
     */
    private String name;

    /**
     * The counterpart of the association for the reversal direction.
     */
    public Association reversalAssociation = null;
    
    /**
     * Data-model containing this association.
     */
    private final DataModel dataModel;
    
    /**
     * Constructor.
     * 
     * @param source the source table
     * @param destination the destination table
     * @param joinCondition the join-condition for join with destination table
     * @param insertSourceBeforeDestination whether or not to insert source-rows before destination rows
     *             in order to prevent foreing-key-constraint voilation
     * @param insertDestinationBeforeSource whether or not to insert destination-rows before source-rows
     *             in order to prevent foreing-key-constraint voilation
     * @param dataModel data-model containing this association
     * @param reversed <code>true</code> for reversed association
     * @param cardinality the cardinality (optional)
     */
    public Association(Table source, Table destination, boolean insertSourceBeforeDestination, boolean insertDestinationBeforeSource, String joinCondition, DataModel dataModel, boolean reversed, Cardinality cardinality) {
        this.source = source;
        this.destination = destination;
        this.insertSourceBeforeDestination = insertSourceBeforeDestination;
        this.insertDestinationBeforeSource = insertDestinationBeforeSource;
        this.joinCondition = joinCondition;
        this.dataModel = dataModel;
        this.reversed = reversed;
        this.cardinality = cardinality;
    }

    /**
     * Gets the restricted join-condition for joining source with destination table.
     * 
     * @return the restricted join-condition for joining source with destination table,
     *         <code>null</code> if association must be ignored
     */
    public String getJoinCondition() {
        if (dataModel.getRestrictionModel() != null) {
            String restriction = dataModel.getRestrictionModel().getRestriction(this);
            if (restriction != null) {
                if (restriction == RestrictionModel.IGNORE) {
                    return null;
                }
                return "(" + joinCondition + ") and " + restriction;
            }
        }
        return joinCondition;
    }
    
    /**
     * Is this association ignored?
     * 
     * @return <code>true</code> iff this association is ignored
     */
    public boolean isIgnored() {
        RestrictionModel restrictionModel = dataModel.getRestrictionModel();
        String restriction = "";
        if (restrictionModel != null) {
            restriction = restrictionModel.getRestriction(this);
            if (restriction == RestrictionModel.IGNORE) {
                return true;
            }
        }
        return false;
    }
    
    /**
     * Gets the cardinality.
     * 
     * @return the cardinality. <code>null</code> if cardinality is not known.
     */
    public Cardinality getCardinality() {
        return cardinality;
    }
    
    /**
     * Stringifies the association.
     */
    public String toString() {
        RestrictionModel restrictionModel = dataModel.getRestrictionModel();
        String restriction = "";
        if (restrictionModel != null) {
            String r = restrictionModel.getRestriction(this);
            if (r != null && r != RestrictionModel.IGNORE) {
                restriction = " restricted by " + r;
            }
        }
        String gap = "";
        String aName = destination.getName();
        if (name != null) {
            aName += "(" + name + ")";
        }
        while ((aName + gap).length() < 30) {
            gap += " ";
        }
        String jc = joinCondition;
        String r = restriction;
        if (reversed) {
            jc = SqlUtil.reversRestrictionCondition(jc);
            r = SqlUtil.reversRestrictionCondition(r);
        }
        String card = "   ";
        if (cardinality != null) {
            card = cardinality.toString();
        }
        return aName + gap + " " + card + " on " + jc + r;
    }

    /**
     * Sets the name of the association.
     * 
     * @param name the name of the association
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Gets the name of the association.
     * 
     * @return the name of the association
     */
    public String getName() {
        return name;
    }

    /**
     * Whether or not to insert source-rows before destination rows
     * in order to prevent foreign-key-constraint voilation.
     * 
     * @return the insertSourceBeforeDestination
     */
    public boolean isInsertSourceBeforeDestination() {
        if (dataModel.getRestrictionModel() != null && dataModel.getRestrictionModel().isTransposed()) {
            return reversalAssociation.insertSourceBeforeDestination;
        }
        return insertSourceBeforeDestination;
    }

    /**
     * Whether or not to insert destination-rows before source-rows
     * in order to prevent foreing-key-constraint voilation.
     * 
     * @return the insertDestinationBeforeSource
     */
    public boolean isInsertDestinationBeforeSource() {
        if (dataModel.getRestrictionModel() != null && dataModel.getRestrictionModel().isTransposed()) {
            return reversalAssociation.insertDestinationBeforeSource;
        }
        return insertDestinationBeforeSource;
    }

    /**
     * Whether there is any restriction of this association.
     */
    public boolean isRestricted() {
        RestrictionModel restrictionModel = dataModel.getRestrictionModel();
        String restriction = "";
        if (restrictionModel != null) {
            restriction = restrictionModel.getRestriction(this);
            if (restriction != null) {
                return true;
            }
        }
        return false;
    }
    
}
