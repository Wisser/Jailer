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
package net.sf.jailer.ui.databrowser;

/**
 * A row of a database table to be rendered in a row-browser.
 * 
 * @author Ralf Wisser
 */
public class Row {

	/**
	 * Constructor.
	 *
	 * @param rowId unique ID, also serves as SQL predicate which identifies this row
	 * @param primaryKey primary key values
	 * @param v column values
	 */
	public Row(String rowId, String[] primaryKey, Object[] v) {
		this.rowId = rowId;
		this.values = v;
		this.primaryKey = primaryKey;
		if (rowId.isEmpty()) {
			synchronized (Row.class) {
				this.nonEmptyRowId = Long.toString(nextUniqueId++);
			}
		} else {
			nonEmptyRowId = rowId;
		}
	}

	/**
	 * Unique ID, also serves as SQL predicate which identifies this row.
	 * Empty string if row has no key.
	 */
	public final String rowId;
	
	/**
	 * Unique ID. Equals rowId if it's not empty.
	 */
	public final String nonEmptyRowId;
	
	/**
	 * Column values.
	 */
	public final Object[] values;
	
	/**
	 * Primary key values.
	 */
	public final String[] primaryKey;

	/**
	 * The block number.
	 */
	private int parentModelIndex = 0;

	/**
	 * Returns the parent model index (block number).
	 *
	 * @return the parentModelIndex
	 */
	public int getParentModelIndex() {
		return parentModelIndex;
	}

	/**
	 * Sets the parent model index (block number).
	 *
	 * @param parentModelIndex the parentModelIndex to set
	 */
	public void setParentModelIndex(int parentModelIndex) {
		this.parentModelIndex = parentModelIndex;
	}

	/**
	 * The inherited block number.
	 */
	private int inheritedParentModelIndex = 0;

	/**
	 * Returns the inherited parent model index (inherited block number).
	 *
	 * @return the inheritedParentModelIndex
	 */
	public int getInheritedParentModelIndex() {
		return inheritedParentModelIndex;
	}

	/**
	 * Sets the inherited parent model index (inherited block number).
	 *
	 * @param inheritedParentModelIndex the inheritedParentModelIndex to set
	 */
	public void setInheritedParentModelIndex(int inheritedParentModelIndex) {
		this.inheritedParentModelIndex = inheritedParentModelIndex;
	}

	private static long nextUniqueId = 0;
	
}
