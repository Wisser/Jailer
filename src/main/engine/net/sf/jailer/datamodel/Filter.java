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

import java.util.List;

/**
 * SQL Expression for server-side column data filtering.
 * 
 * @author Wisser
 */
public class Filter {

	/**
	 * Filter expression (in SQL).
	 */
	private final String expression;
	
	/**
	 * Derived from parent primary key column?
	 */
	final private boolean derived;
	
	/**
	 * The source.
	 */
	final private FilterSource filterSource;
	
	/**
	 * Reason (optional).
	 */
	private final String reason;
	
	/**
	 * <code>true</code> if filter is applied at export instead of import.
	 */
	private boolean applyAtExport = true;
	
	/**
	 * Optional type of filter expression. Determines the type of the import-filter-mapping-table columns.
	 */
	private String type;
	
	/**
	 * Prefix for "literal" filter expressions.
	 */
	public static final String LITERAL_PREFIX = "literal:";

	/**
	 * Filter expression for exclusion of columns from export.
	 */
	public static final String EXCLUDED_VALUE = "excluded";

	/**
	 * Gets optional type of filter expression. Determines the type of the import-filter-mapping-table columns.
	 * 
	 * @return the type
	 */
	public String getType() {
		return type;
	}

	/**
	 * Sets optional type of filter expression. Determines the type of the import-filter-mapping-table columns.
	 * 
	 * @param type the type to set
	 */
	public void setType(String type) {
		this.type = type;
	}

	/**
	 * Description of destinations.
	 */
	private List<String> appliedTo;
	
	public static String OLD_VALUE_PROP = "${old-value}";
	public static String OLD_VALUE_PROP_PURE = "old-value";
	public static String OLD_VALUE_PROP_RE = "\\$\\{old-value\\}";
	
	/**
	 * Constructor.
	 * 
	 * @param expression filter expression (in SQL)
	 * @param derived derived from parent primary key column?
	 */
	public Filter(String expression, String type, boolean derived, FilterSource filterSource) {
		this(expression, type, derived, filterSource, null);
	}

	/**
	 * Constructor.
	 * 
	 * @param expression filter expression (in SQL)
	 * @param derived derived from parent primary key column?
	 * @param reason reason
	 */
	public Filter(String expression, String type, boolean derived, FilterSource filterSource, String reason) {
		this.expression = expression;
		this.type = type;
		this.derived = derived;
		this.filterSource = filterSource;
		this.reason = reason;
	}
	
	/**
	 * Gets the filter expression (in SQL).
	 * 
	 * @return the filter expression (in SQL)
	 */
	public String getExpression() {
		return expression;
	}

	/**
	 * Returns <code>true</code> iff filter is derived.
	 * 
	 * @return <code>true</code> iff filter is derived
	 */
	public boolean isDerived() {
		return derived;
	}

	/**
	 * @return source
	 */
	public FilterSource getFilterSource() {
		return filterSource;
	}

	/**
	 * @return the appliedTo
	 */
	public List<String> getAppliedTo() {
		return appliedTo;
	}

	/**
	 * @param appliedTo the appliedTo to set
	 */
	public void setAppliedTo(List<String> appliedTo) {
		this.appliedTo = appliedTo;
	}

	/**
	 * Sets if filter is applied at export instead of import.
	 *
	 * @param b the value
	 */
	public void setApplyAtExport(boolean b) {
		applyAtExport = b;
	}

	/**
	 * Gets reasion.
	 * 
	 * @return reasion
	 */
	public String getReason() {
		return reason;
	}

	/**
	 * Gets if filter is applied at export instead of import.
	 *
	 * @return the value
	 */
	public boolean isApplyAtExport() {
		return applyAtExport;
	}

}
