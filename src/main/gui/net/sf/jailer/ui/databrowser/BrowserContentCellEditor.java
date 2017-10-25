/*
 * Copyright 2007 - 2017 the original author or authors.
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

import java.math.BigDecimal;
import java.sql.Types;

import net.sf.jailer.util.SqlUtil;

/**
 * For in-place editing of query results.
 * 
 * @author Ralf Wisser
 */
public class BrowserContentCellEditor {
	
	/**
	 * {@link Types} per column.
	 */
	private final int[] columnTypes;
	
	/**
	 * Constructor.
	 * 
	 * @param columnTypes {@link Types} per column
	 */
	public BrowserContentCellEditor(int[] columnTypes) {
		this.columnTypes = columnTypes;
	}

	/**
	 * Is given cell editable?
	 * 
	 * @param row row number
	 * @param column column number
	 * @param content cell content
	 */
	public boolean isEditable(int row, int column, Object content) {
		if (column < 0 || column >= columnTypes.length) {
			return false;
		}
		return columnTypes[column] == Types.DECIMAL && content instanceof BigDecimal && ((BigDecimal) content).longValue() < 10;
	}

	/**
	 * Converts cell content to text.
	 * 
	 * @param row row number
	 * @param column column number
	 * @param content cell content
	 * 
	 * @return text
	 */
	public String cellContentToText(int row, int column, Object content) {
		if (columnTypes[column] == Types.DECIMAL && content instanceof BigDecimal) {
			return SqlUtil.toString((BigDecimal) content);
		}
		return "";
	}

	/**
	 * Converts text to cell content.
	 * 
	 * @param row row number
	 * @param column column number
	 * @param text the text
	 * 
	 * @return cell content or {@link #INVALID} if text cannot be converted
	 */
	public Object textToContent(int row, int column, String text) {
		if (columnTypes[column] == Types.DECIMAL) {
			try {
				return new BigDecimal(text);
			} catch (NumberFormatException e) {
				return INVALID;
			}
		}
		return INVALID;
	}

	/**
	 * Invalid content.
	 */
	public static final  Object INVALID = new Object();

}
