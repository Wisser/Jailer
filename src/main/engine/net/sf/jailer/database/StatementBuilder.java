/*
 * Copyright 2007 - 2025 Ralf Wisser.
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

package net.sf.jailer.database;

import java.util.ArrayList;
import java.util.List;

/**
 * Builds compact SQL-statements of the pattern:
 * HEAD ITEM {SEPARATOR ITEM}* TERMINATOR.
 *  
 * (for example, an 'INSERT' with a 'values'-list)
 *  
 * @author Ralf Wisser
 */
public class StatementBuilder {

	/**
	 * The maximal size of a body-list.
	 */
	private final int maxBodySize;

	/**
	 * The head of the statement.
	 */
	private String head = null;

	/**
	 * Item-list.
	 */
	private List<String> body = new ArrayList<String>();

	/**
	 * Separates the items.
	 */
	private String separator;
	
	/**
	 * Terminates the statement.
	 */
	private String terminator;

	/**
	 * Constructor.
	 * 
	 * @param maxBodySize the maximal size of a body-list
	 */
	public StatementBuilder(int maxBodySize) {
		this.maxBodySize = maxBodySize;
	}
	
	/**
	 * Checks whether an item is appendable to previously appended items.
	 * 
	 * @param head the statements head
	 * @param item the item
	 * @return <code>true</code> iff item is appendable
	 * 
	 * @deprecated use {@link #isAppendable(String)} instead
	 */
	private boolean isAppendable(String head, String item) {
		return body.size() < maxBodySize && (this.head == null || this.head.equals(head));
	}
	
	/**
	 * Checks whether an item is appendable to previously appended items.
	 * 
	 * @param head the statements head
	 * @return <code>true</code> iff item is appendable
	 */
	public boolean isAppendable(String head) {
		return isAppendable(head, null);
	}

	/**
	 * Builds the SQL-statement and resets the builder.
	 * 
	 * @return the SQL-statement
	 */
	public String build() {
		if (this.head != null) {
			StringBuilder sqlStatement = new StringBuilder(head);
			boolean firstTime = true;
			for (String item: body) {
				if (!firstTime) {
					sqlStatement.append(separator);
				}
				firstTime = false;
				sqlStatement.append(item);
			}
			sqlStatement.append(terminator);
			head = null;
			body.clear();
			return sqlStatement.toString();
		}
		return "";
	}

	/**
	 * Appends an item.
	 * 
	 * @param head same head as previously appended, if any
	 * @param item the item
	 * @param separator separates the items
	 * @param terminator terminates the statement
	 */
	public void append(String head, String item, String separator, String terminator) {
		if (this.head != null && !this.head.equals(head)) {
			throw new IllegalStateException("can't append, '" + this.head + "'!='" + head + "'");
		}
		this.head = head;
		this.terminator = terminator;
		this.separator = separator;
		body.add(item);
	}

	/**
	 * Checks if builder is empty.
	 * 
	 * @return <code>true</code> if builder is empty
	 */
	public boolean isEmpty() {
		return body.isEmpty();
	}

	/**
	 * Returns the size of the current statement.
	 */
	public int size() {
		return body.size();
	}
	
	/**
	 * Returns the items of the current statement.
	 */
	public List<String> getItems() {
		return body;
	}
	
	/**
	 * Gets the maximal size of a body-list.
	 */
	public int getMaxBodySize() {
		return maxBodySize;
	}
	
}
