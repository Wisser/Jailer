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
package net.sf.jailer.datamodel.filter_template;

import java.util.ArrayList;
import java.util.List;

import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.Filter;
import net.sf.jailer.datamodel.FilterSource;
import net.sf.jailer.datamodel.Table;

/**
 * Template of {@link Filter}s.
 * 
 * @author Wisser
 */
public class FilterTemplate implements FilterSource {

	/**
	 * Name of the template.
	 */
	private String name;
	
	/**
	 * Enabled/disabled?
	 */
	private boolean enabled = true;
	
	/**
	 * <code>true</code> if filter is applied at export instead of import.
	 */
	private boolean applyAtExport = true;
	
	/**
	 * Clauses.
	 */
	private final List<Clause> clauses;
	
	/**
	 * Filter expression (in SQL).
	 */
	private String expression = Filter.OLD_VALUE_PROP;
	
	/**
	 * Optional type of filter expression. Determines the type of the import-filter-mapping-table columns.
	 */
	private String type;
	
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
	 * Constructor.
	 */
	public FilterTemplate() {
		clauses = new ArrayList<Clause>();
	}

	/**
	 * Copy constructor.
	 */
	public FilterTemplate(FilterTemplate other) {
		this.expression = other.expression;
		this.name = other.name;
		this.enabled = other.enabled;
		this.applyAtExport = other.applyAtExport;
		this.type = other.type;
		this.clauses = new ArrayList<Clause>();
		for (Clause clause: other.clauses) {
			this.clauses.add(new Clause(clause));
		}
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
	 * Sets the filter expression (in SQL).
	 * 
	 * @return the filter expression (in SQL)
	 */
	public void setExpression(String expression) {
		this.expression = expression;
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
	 * Gets if filter is applied at export instead of import.
	 *
	 * @return the value
	 */
	public boolean isApplyAtExport() {
		return applyAtExport;
	}
	
	/**
	 * Gets clear text description of what the source is.
	 * 
	 * @return clear text description
	 */
	@Override
	public String getDescription() {
		return "Template \"" + name + "\"";
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * @param name the name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @return the enabled
	 */
	public boolean isEnabled() {
		return enabled;
	}

	/**
	 * @param enabled the enabled to set
	 */
	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	/**
	 * Gets the clauses.
	 * 
	 * @return the clauses
	 */
	public List<Clause> getClauses() {
		return clauses;
	}
	
	/**
	 * Checks if the template matches a given column.
	 * 
	 * @param table the column's table 
	 * @param column the column
	 * @return <code>true</code> iff template is enabled and all clauses evaluates to true
	 */
	public boolean matches(Table table, Column column) {
		if (!enabled || clauses.isEmpty()) {
			return false;
		}
		for (Clause clause: clauses) {
			if (!clause.eval(table, column)) {
				return false;
			}
		}
		return true;
	}

}
