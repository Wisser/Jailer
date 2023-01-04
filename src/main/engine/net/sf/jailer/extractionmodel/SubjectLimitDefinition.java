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
package net.sf.jailer.extractionmodel;

/**
 * Defines a limit of rows to be exported per subject table wrt an ordering.
 * 
 * @author Ralf Wisser
 */
public class SubjectLimitDefinition {

	/**
	 * Row limit.
	 */
	public Long limit;

	/**
	 * "order by" clause.
	 */
	public String orderBy;

	/**
	 * Constructor.
	 * 
	 * @param limit row limit
	 * @param orderBy "order by" clause
	 */
	public SubjectLimitDefinition(Long limit, String orderBy) {
		this.limit = limit;
		this.orderBy = orderBy;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((limit == null) ? 0 : limit.hashCode());
		result = prime * result + ((orderBy == null) ? 0 : orderBy.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		SubjectLimitDefinition other = (SubjectLimitDefinition) obj;
		if (limit == null) {
			if (other.limit != null)
				return false;
		} else if (!limit.equals(other.limit))
			return false;
		if (orderBy == null) {
			if (other.orderBy != null)
				return false;
		} else if (!orderBy.equals(other.orderBy))
			return false;
		return true;
	}

}
