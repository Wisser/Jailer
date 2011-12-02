/*
 * Copyright 2007 - 2012 the original author or authors.
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
	 * @param v column values
	 */
	public Row(String rowId, Object[] v) {
		this.rowId = rowId;
		this.values = v;
	}
	
	/**
	 * @param isBlockEnd <code>true</code> if this row is the last one of a block.
	 */
	public void setBlockEnd(boolean isBlockEnd) {
		this.isBlockEnd = isBlockEnd;
	}

	/**
	 * @return <code>true</code> if this row is the last one of a block.
	 */
	public boolean isBlockEnd() {
		return isBlockEnd;
	}

	/**
	 * Unique ID, also serves as SQL predicate which identifies this row.
	 */
	public final String rowId;
	
	/**
	 * Column values.
	 */
	public final Object[] values;
	
	/**
	 * <code>true</code> if this row is the last one of a block.
	 */
	private boolean isBlockEnd = false;
	
}
