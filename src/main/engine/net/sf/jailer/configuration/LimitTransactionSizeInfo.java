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
package net.sf.jailer.configuration;

import java.util.Locale;

import net.sf.jailer.ExecutionContext;

/**
 * Holds information about how to do incremental inserts.
 * 
 * @author Ralf Wisser
 */
public class LimitTransactionSizeInfo {
	
	/**
	 * Limit.
	 */
	private long limit = 0;

	/**
	 * Pattern to be inserted after "SELECT" in a select query.
	 */
	private String afterSelect;

	/**
	 * Pattern to be inserted as condition into the "WHERE".
	 */
	private String additionalWhereCondition;

	/**
	 * Pattern to be appended.
	 */
	private String statementSuffix;

	/**
	 * Is this applicable?
	 * 
	 * @return <code>true</code> if applicable
	 */
	public boolean isApplicable(ExecutionContext executionContext) {
		return limit > 0 && executionContext.isInsertIncrementally();
	}

	/**
	 * Gets increment size or <code>null</code>, if incremental inserts are not applicable.
	 * 
	 * @return increment size or <code>null</code>, if incremental inserts are not applicable
	 */
	public long getSize(ExecutionContext executionContext) {
		return isApplicable(executionContext)? limit : 0;
	}

	/**
	 * Gets fragment to be inserted into a select statement after the "SELECT".
	 * 
	 * @return fragment (may be the empty string)
	 */
	public String afterSelectFragment(ExecutionContext executionContext) {
		if (afterSelect != null && !afterSelect.trim().isEmpty() && isApplicable(executionContext)) {
			return String.format(Locale.ENGLISH, afterSelect, limit) + " ";
		} else {
			return "";
		}
	}

	/**
	 * Gets fragment to be inserted as condition into the "WHERE".
	 * 
	 * @return fragment (may be the empty string)
	 */
	public String additionalWhereConditionFragment(ExecutionContext executionContext) {
		if (additionalWhereCondition != null && !additionalWhereCondition.trim().isEmpty() && isApplicable(executionContext)) {
			return "and " + String.format(Locale.ENGLISH, additionalWhereCondition, limit) + " ";
		} else {
			return "";
		}
	}

	/**
	 * Gets fragment to be appended.
	 * 
	 * @return fragment (may be the empty string)
	 */
	public String statementSuffixFragment(ExecutionContext executionContext) {
		if (statementSuffix != null && !statementSuffix.trim().isEmpty() && isApplicable(executionContext)) {
			return String.format(Locale.ENGLISH, statementSuffix, limit) + " ";
		} else {
			return "";
		}
	}

	/**
	 * Limit or 0.
	 * 
	 * @return limit or 0 if limiting is not possible
	 */
	public long getLimit() {
		return limit;
	}

	/**
	 * Limit.
	 */
	public void setLimit(long size) {
		this.limit = size;
	}

	/**
	 * Pattern to be inserted after "SELECT" in a select query.
	 */
	public String getAfterSelect() {
		return afterSelect;
	}

	/**
	 * Pattern to be inserted after "SELECT" in a select query.
	 */
	public void setAfterSelect(String afterSelect) {
		this.afterSelect = afterSelect;
	}

	/**
	 * Pattern to be inserted as condition into the "WHERE".
	 */
	public String getAdditionalWhereCondition() {
		return additionalWhereCondition;
	}

	/**
	 * Pattern to be inserted as condition into the "WHERE".
	 */
	public void setAdditionalWhereCondition(String additionalWhereCondition) {
		this.additionalWhereCondition = additionalWhereCondition;
	}

	/**
	 * Pattern to be appended.
	 */
	public String getStatementSuffix() {
		return statementSuffix;
	}

	/**
	 * Pattern to be appended.
	 */
	public void setStatementSuffix(String statementSuffix) {
		this.statementSuffix = statementSuffix;
	}

}
