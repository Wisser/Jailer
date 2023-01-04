/*
 * Copyright 2007 - 2023 Ralf Wisser.
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

import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import net.sf.jailer.restrictionmodel.RestrictionModel;
import net.sf.jailer.util.Pair;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlUtil;

/**
 * An association between database-tables.
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
	 * Whether or not to insert source-rows before destination rows in order to
	 * prevent foreign-key-constraint violation.
	 */
	private final boolean insertSourceBeforeDestination;

	/**
	 * Whether or not to insert destination-rows before source-rows in order to
	 * prevent foreign-key-constraint violation.
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
	 * Unique association ID. -1 if id is not yet defined.
	 */
	int id = -1;

	/**
	 * Constructor.
	 *
	 * @param source
	 *            the source table
	 * @param destination
	 *            the destination table
	 * @param joinCondition
	 *            the join-condition for join with destination table
	 * @param insertSourceBeforeDestination
	 *            whether or not to insert source-rows before destination rows
	 *            in order to prevent foreign-key-constraint violation
	 * @param insertDestinationBeforeSource
	 *            whether or not to insert destination-rows before source-rows
	 *            in order to prevent foreign-key-constraint violation
	 * @param dataModel
	 *            data-model containing this association
	 * @param reversed
	 *            <code>true</code> for reversed association
	 * @param cardinality
	 *            the cardinality (optional)
	 */
	public Association(Table source, Table destination, boolean insertSourceBeforeDestination,
			boolean insertDestinationBeforeSource, String joinCondition, DataModel dataModel, boolean reversed,
			Cardinality cardinality) {
		this(source, destination, insertSourceBeforeDestination, insertDestinationBeforeSource, joinCondition,
				dataModel, reversed, cardinality, null);
	}

	/**
	 * Constructor.
	 *
	 * @param source
	 *            the source table
	 * @param destination
	 *            the destination table
	 * @param joinCondition
	 *            the join-condition for join with destination table
	 * @param insertSourceBeforeDestination
	 *            whether or not to insert source-rows before destination rows
	 *            in order to prevent foreign-key-constraint violation
	 * @param insertDestinationBeforeSource
	 *            whether or not to insert destination-rows before source-rows
	 *            in order to prevent foreign-key-constraint violation
	 * @param dataModel
	 *            data-model containing this association
	 * @param reversed
	 *            <code>true</code> for reversed association
	 * @param cardinality
	 *            the cardinality (optional)
	 * @param author
	 *            the author
	 */
	public Association(Table source, Table destination, boolean insertSourceBeforeDestination,
			boolean insertDestinationBeforeSource, String joinCondition, DataModel dataModel, boolean reversed,
			Cardinality cardinality, String author) {
		this.source = source;
		this.destination = destination;
		this.insertSourceBeforeDestination = insertSourceBeforeDestination;
		this.insertDestinationBeforeSource = insertDestinationBeforeSource;
		this.joinCondition = joinCondition;
		this.dataModel = dataModel;
		this.reversed = reversed;
		this.cardinality = cardinality;
		if (author != null) {
			setAuthor(author);
		}
	}

	/**
	 * Gets the restricted join-condition for joining source with destination
	 * table.
	 *
	 * @return the restricted join-condition for joining source with destination
	 *         table, <code>null</code> if association must be ignored
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
	@Override
	public String toString() {
		return toString(30, false);
	}

	/**
	 * Stringifies the association.
	 */
	public String toString(int maxGab, boolean useDisplayName) {
		RestrictionModel restrictionModel = dataModel.getRestrictionModel();
		String restriction = "";
		if (restrictionModel != null) {
			String r = restrictionModel.getRestriction(this);
			if (r != null && r != RestrictionModel.IGNORE) {
				restriction = " restricted by " + r.replace('\n', ' ').replace('\r', ' ');
			}
		}
		String gap = "";
		String aName = useDisplayName ? dataModel.getDisplayName(destination) : destination.getName();
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
	 * @param restrictionSeparator
	 *            separates join-condition from restriction condition in the
	 *            result
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
	 * @return restriction-condition, <code>null</code> if association is not
	 *         restricted
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
					String rWOParentheses = r.substring(1, r.length() - 1);
					if (checkParentheses(r) && checkParentheses(rWOParentheses)) {
						r = rWOParentheses;
					}
				}
				return r;
			}
		}
		return null;
	}

	private static boolean checkParentheses(String condition) {
		int level = 0;
		for (int i = 0; i < condition.length(); ++i) {
			char c = condition.charAt(i);
			if (c == '(') {
				++level;
			} else if (c == ')') {
				--level;
				if (level < 0) {
					return false;
				}
			}
		}
		return level == 0;
	}

	/**
	 * Sets the name of the association.
	 *
	 * @param name
	 *            the name of the association
	 */
	public void setName(String name) {
		this.name = name;
		if (dataModel != null) {
			dataModel.version++;
		}
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
	 * Whether or not to insert source-rows before destination rows in order to
	 * prevent foreign-key-constraint violation.
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
	 * Whether or not to insert destination-rows before source-rows in order to
	 * prevent foreign-key-constraint violation.
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
	 * @param condition
	 *            the condition
	 */
	public void appendCondition(String condition) {
		if (joinCondition == null) {
			joinCondition = condition;
		} else {
			joinCondition = joinCondition + " and " + condition;
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
				tag = destination.getUnqualifiedName().toLowerCase(Locale.ENGLISH);
			} else {
				tag = name.toLowerCase(Locale.ENGLISH);
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
	 * @param aggregationSchema
	 *            the XML aggregation schema
	 */
	public void setAggregationSchema(AggregationSchema aggregationSchema) {
		this.aggregationSchema = aggregationSchema;
		if (dataModel != null) {
			dataModel.version++;
		}
	}

	/**
	 * Sets name of XML-tag used for aggregation.
	 *
	 * @param aggregationTagName
	 *            name of XML-tag used for aggregation
	 */
	public void setAggregationTagName(String aggregationTagName) {
		this.aggregationTagName = aggregationTagName;
		if (dataModel != null) {
			dataModel.version++;
		}
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

	/**
	 * Maps source-columns to destination-columns, if this represents an
	 * equi-join. Otherwise it returns an empty map.
	 *
	 * @return map from source-columns to destination-columns, if this
	 *         represents an equi-join
	 */
	public Map<Column, Column> createSourceToDestinationKeyMapping() {
		return createSourceToDestinationKeyMapping(null);
	}

	/**
	 * Maps source-columns to destination-columns, if this represents an
	 * equi-join. Otherwise it returns an empty map.
	 *
	 * @param assignments if not <code>null</code>, put column assignments into it
	 *
	 * @return map from source-columns to destination-columns, if this
	 *         represents an equi-join
	 */
	public Map<Column, Column> createSourceToDestinationKeyMapping(Set<Pair<Column, Column>> assignments) {
		String[] equations = getUnrestrictedJoinCondition().replaceAll("\\(|\\)", " ").trim()
				.split("\\s*\\b(a|A)(n|N)(d|D)\\b\\s*");
		Map<Column, Column> mapping = new LinkedHashMap<Column, Column>();
		Set<Column> destinationColumns = new HashSet<Column>();
		boolean isValid = true;
		for (String equation: equations) {
			String hs[] = equation.split("\\s*=\\s*");
			if (hs.length != 2) {
				return Collections.emptyMap();
			}
			String lhs[] = hs[0].split("\\s*\\.\\s*");
			String rhs[] = hs[1].split("\\s*\\.\\s*");
			if (lhs.length != 2 || rhs.length != 2 || lhs[0].length() != 1 || rhs[0].length() != 1) {
				return Collections.emptyMap();
			}
			String dColumn = null, sColumn = null;
			if ("A".equalsIgnoreCase(lhs[0])) {
				sColumn = lhs[1];
			}
			if ("B".equalsIgnoreCase(lhs[0])) {
				dColumn = lhs[1];
			}
			if ("A".equalsIgnoreCase(rhs[0])) {
				sColumn = rhs[1];
			}
			if ("B".equalsIgnoreCase(rhs[0])) {
				dColumn = rhs[1];
			}
			if (sColumn == null || dColumn == null) {
				return Collections.emptyMap();
			}

			sColumn = Quoting.normalizeIdentifier(sColumn);
			dColumn = Quoting.normalizeIdentifier(dColumn);

			if (reversed) {
				String h = sColumn;
				sColumn = dColumn;
				dColumn = h;
			}

			Column sourceColumn = null;
			for (Column c : source.getColumns()) {
				if (Quoting.normalizeIdentifier(c.name).equals(sColumn)) {
					sourceColumn = c;
					break;
				}
			}
			Column destinationColumn = null;
			for (Column c : destination.getColumns()) {
				if (Quoting.normalizeIdentifier(c.name).equals(dColumn)) {
					destinationColumn = c;
					break;
				}
			}
			if (sourceColumn == null || destinationColumn == null) {
				isValid = false;
			} else {
				if (assignments != null) {
					assignments.add(new Pair<Column, Column>(sourceColumn, destinationColumn));
				}
				if (mapping.put(sourceColumn, destinationColumn) != null) {
					isValid = false;
				}
				if (!destinationColumns.add(destinationColumn)) {
					isValid = false;
				}
			}
		}

		if (isValid) {
			return mapping;
		} else {
			return Collections.emptyMap();
		}
	}

	public static final String NULL_FILTER_COMMENT_PREFIX = "foreign key to ";
	private static final Pattern NULL_FILTER_PATTERN = Pattern.compile("(?i:\\s*(/\\*.*\\*/\\s*)?null(\\s*/\\*.*\\*/)?)\\s*");

	private boolean isNullFilter(Filter filter) {
		return filter.getExpression() != null
				&&
				((filter.isApplyAtExport() && NULL_FILTER_PATTERN.matcher(filter.getExpression()).matches())
				||
				Filter.EXCLUDED_VALUE.equals(filter.getExpression()));
	}

	public boolean hasNullableFK() {
		if (!isInsertDestinationBeforeSource()) {
			return false;
		}
		Map<Column, Column> sdMap = createSourceToDestinationKeyMapping();
		if (sdMap.isEmpty()) {
			return false;
		}
		for (Column c: sdMap.keySet()) {
			if (!c.isNullable) {
				return false;
			}
			if (c.getFilter() != null && !c.getFilter().isDerived() && !isNullFilter(c.getFilter())) {
				return false;
			}
			for (Column pk: source.primaryKey.getColumns()) {
				if (c.name.equals(pk.name)) {
					return false;
				}
			}
		}
		return true;
	}

	public boolean fkHasNullFilter() {
		Map<Column, Column> sdMap = createSourceToDestinationKeyMapping();
		for (Column c: sdMap.keySet()) {
			if (c.getFilter() == null || !isNullFilter(c.getFilter())) {
				return false;
			}
		}
		return true;
	}

	public boolean fkHasExcludeFilter() {
		Map<Column, Column> sdMap = createSourceToDestinationKeyMapping();
		for (Column c: sdMap.keySet()) {
			if (c.getFilter() == null || !Filter.EXCLUDED_VALUE.equals(c.getFilter().getExpression())) {
				return false;
			}
		}
		return true;
	}

	public boolean setOrResetFKNullFilter(boolean set) {
		boolean changed = false;
		Map<Column, Column> sdMap = createSourceToDestinationKeyMapping();
		for (Column c: sdMap.keySet()) {
			if (set) {
				if (c.getFilter() == null || !isNullFilter(c.getFilter())) {
					c.setFilter(new Filter("null /* " + NULL_FILTER_COMMENT_PREFIX + getName() + " */", null, false, null));
					changed = true;
				}
			} else {
				if (c.getFilter() != null) {
					c.setFilter(null);
					changed = true;
				}
			}
		}
		getDataModel().deriveFilters();
		getDataModel().version++;
		return changed;
	}

	public boolean isRestrictedDependencyWithNulledFK() {
		boolean restrictedDep = isInsertDestinationBeforeSource() && getRestrictionCondition() != null;
		if (restrictedDep) {
			if (fkHasNullFilter() && hasNullableFK() && !fkHasExcludeFilter()) {
				return true;
			}
		}
		return false;
	}

}
