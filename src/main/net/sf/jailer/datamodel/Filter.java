/*
 * Copyright 2007 - 2016 the original author or authors.
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
	 * Description of source.
	 */
	final private String derivedFrom;
	
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
	public Filter(String expression, boolean derived, String derivedFrom) {
		this.expression = expression;
		this.derived = derived;
		this.derivedFrom = derivedFrom;
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
	 * @return description of source
	 */
	public String getDerivedFrom() {
		return derivedFrom;
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

}
