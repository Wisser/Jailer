/*
 * Copyright 2007 - 2026 Ralf Wisser.
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
	 * @param executionContext the execution context
	 * @return <code>true</code> if applicable
	 */
	public boolean isApplicable(ExecutionContext executionContext) {
		return limit > 0 && executionContext.isInsertIncrementally();
	}

	/**
	 * Gets increment size or <code>null</code>, if incremental inserts are not applicable.
	 *
	 * @param executionContext the execution context
	 * @return increment size or <code>null</code>, if incremental inserts are not applicable
	 */
	public long getSize(ExecutionContext executionContext) {
		return isApplicable(executionContext)? limit : 0;
	}

	/**
	 * Gets fragment to be inserted into a select statement after the "SELECT".
	 *
	 * @param executionContext the execution context
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
	 * @param executionContext the execution context
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
	 * @param executionContext the execution context
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
	 * Gets the limit.
	 *
	 * @return limit or 0 if limiting is not possible
	 */
	public long getLimit() {
		return limit;
	}

	/**
	 * Sets the limit.
	 *
	 * @param size the limit to set
	 */
	public void setLimit(long size) {
		this.limit = size;
	}

	/**
	 * Gets the pattern to be inserted after "SELECT" in a select query.
	 *
	 * @return the pattern to be inserted after "SELECT"
	 */
	public String getAfterSelect() {
		return afterSelect;
	}

	/**
	 * Sets the pattern to be inserted after "SELECT" in a select query.
	 *
	 * @param afterSelect the pattern to set
	 */
	public void setAfterSelect(String afterSelect) {
		this.afterSelect = afterSelect;
	}

	/**
	 * Gets the pattern to be inserted as condition into the "WHERE".
	 *
	 * @return the additional WHERE condition pattern
	 */
	public String getAdditionalWhereCondition() {
		return additionalWhereCondition;
	}

	/**
	 * Sets the pattern to be inserted as condition into the "WHERE".
	 *
	 * @param additionalWhereCondition the additional WHERE condition pattern to set
	 */
	public void setAdditionalWhereCondition(String additionalWhereCondition) {
		this.additionalWhereCondition = additionalWhereCondition;
	}

	/**
	 * Gets the pattern to be appended to the statement.
	 *
	 * @return the statement suffix pattern
	 */
	public String getStatementSuffix() {
		return statementSuffix;
	}

	/**
	 * Sets the pattern to be appended to the statement.
	 *
	 * @param statementSuffix the statement suffix pattern to set
	 */
	public void setStatementSuffix(String statementSuffix) {
		this.statementSuffix = statementSuffix;
	}

}
