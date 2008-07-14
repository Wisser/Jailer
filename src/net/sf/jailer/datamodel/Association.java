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

import net.sf.jailer.restrictionmodel.RestrictionModel;
import net.sf.jailer.util.SqlUtil;

/**
 * Describes an association between two database-tables.
 * 
 * @author Ralf Wisser
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
    private String joinCondition;
    
    /**
     * The cardinality.
     */
    private final Cardinality cardinality;
    
    /**
     * Whether or not to insert source-rows before destination rows
     * in order to prevent foreign-key-constraint violation.
     */
    private final boolean insertSourceBeforeDestination;
    
    /**
     * Whether or not to insert destination-rows before source-rows
     * in order to prevent foreign-key-constraint violation.
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
     * The XML aggregation schema.
     */
    private AggregationSchema aggregationSchema = AggregationSchema.NONE;
    
    /**
     * Name of XML-tag used for aggregation.
     */
    private String aggregationTagName;
    
    /**
     * Data-model containing this association.
     */
    private final DataModel dataModel;
    
    /**
     * Unique association ID. -1 if id is not yet given.
     */
    int id = -1;
    
    /**
     * Constructor.
     * 
     * @param source the source table
     * @param destination the destination table
     * @param joinCondition the join-condition for join with destination table
     * @param insertSourceBeforeDestination whether or not to insert source-rows before destination rows
     *             in order to prevent foreign-key-constraint violation
     * @param insertDestinationBeforeSource whether or not to insert destination-rows before source-rows
     *             in order to prevent foreign-key-constraint violation
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
    	return toString(30);
    }
    
    /**
     * Stringifies the association.
     */
    public String toString(int maxGab) {
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
            aName += " (" + name + ")";
        }
        while ((aName + gap).length() < maxGab) {
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
     * Stringifies the join condition.
     * 
     * @param restrictionSeparator separates join-condition from restriction condition in the result
     */
    public String renderJoinCondition(String restrictionSeparator) {
        RestrictionModel restrictionModel = dataModel.getRestrictionModel();
        String restriction = "";
        if (restrictionModel != null) {
            String r = restrictionModel.getRestriction(this);
            if (r != null && r != RestrictionModel.IGNORE) {
                restriction = " " + restrictionSeparator + " " + r;
            }
        }
        String jc = joinCondition;
        String r = restriction;
        if (reversed) {
            jc = SqlUtil.reversRestrictionCondition(jc);
            r = SqlUtil.reversRestrictionCondition(r);
        }
        return jc + r;
    }

    /**
     * Gets join-condition without any restrictions.
     * 
     * @return join-condition as defined in data model
     */
	public String getUnrestrictedJoinCondition() {
		return joinCondition;
	}
    
    /**
     * Gets restriction-condition.
     * 
     * @return restriction-condition, <code>null</code> if association is not restricted
     */
	public String getRestrictionCondition() {
		RestrictionModel restrictionModel = dataModel.getRestrictionModel();
	    if (restrictionModel != null) {
	        String r = restrictionModel.getRestriction(this);
	        if (r != null) {
	        	if (r == RestrictionModel.IGNORE) {
	        		return "false";
	        	}
	        	if (reversed) {
	        		r = SqlUtil.reversRestrictionCondition(r);
	        	}
	        	if (r.startsWith("(") && r.endsWith(")")) {
	        		r = r.substring(1, r.length() - 1);
	        	}
	        	return r;
	        }
	    }
	    return null;
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
     * in order to prevent foreign-key-constraint violation.
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
     * in order to prevent foreign-key-constraint violation.
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

    /**
     * Appends condition to join-condition.
     * 
     * @param condition the condition
     */
	public void appendCondition(String condition) {
		if (joinCondition == null) {
			joinCondition = condition;
		} else {
			joinCondition = "(" + joinCondition + ") and (" + condition + ")";
		}
	}
    
    /**
     * Gets the XML aggregation schema.
     * 
     * @return the XML aggregation schema
     */
    public AggregationSchema getAggregationSchema() {
    	return aggregationSchema;
    }
    
    /**
     * Gets name of XML-tag used for aggregation.
     * 
     * @return name of XML-tag used for aggregation
     */
    public String getAggregationTagName() {
    	String tag;
    	if (aggregationTagName == null) {
    		if (name.startsWith("inverse-")) {
    			tag = destination.getUnqualifiedName().toLowerCase();
    		} else {
    			tag = name.toLowerCase();
    		}
    	} else {
    		tag = aggregationTagName;
    	}

    	StringBuilder sb = new StringBuilder();
		for (int i = 0; i < tag.length(); ++i) {
			char c = tag.charAt(i);
			if (Character.isUpperCase(c) || Character.isLowerCase(c) || Character.isDigit(c) || c == '-' || c == '_') {
				sb.append(c);
			}
		}
		return sb.toString();
    }
    
    /**
     * Sets the XML aggregation schema.
     * 
     * @param aggregationSchema the XML aggregation schema
     */
    public void setAggregationSchema(AggregationSchema aggregationSchema) {
    	this.aggregationSchema = aggregationSchema;
    }
    
    /**
     * Sets name of XML-tag used for aggregation.
     * 
     * @param aggregationTagName name of XML-tag used for aggregation
     */
    public void setAggregationTagName(String aggregationTagName) {
    	this.aggregationTagName = aggregationTagName;
    }

    /**
     * Gets unique ID.
     * 
     * @return unique ID
     */
	public int getId() {
		if (id < 0) {
			dataModel.assignAssociationIDs();
		}
		return id;
	}

	/**
     * Gets data-model to which this association belongs to.
     */
    public DataModel getDataModel() {
    	return dataModel;
    }
        
}
