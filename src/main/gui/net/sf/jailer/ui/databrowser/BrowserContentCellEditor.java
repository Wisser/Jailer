/*
 * Copyright 2007 - 2018 the original author or authors.
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
import java.sql.Timestamp;
import java.sql.Types;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.SqlUtil;

/**
 * For in-place editing of query results.
 * 
 * @author Ralf Wisser
 */
public class BrowserContentCellEditor {
	
	/**
	 * Cell content to text converter.
	 */
	private enum Converter {
		DECMAL {
			@Override
			String cellContentToText(int columnType, Object content) {
				if (content instanceof BigDecimal) {
					return SqlUtil.toString((BigDecimal) content);
				}
				if (content instanceof Double) {
					return SqlUtil.toString((Double) content);
				}
				if (content instanceof Float) {
					return SqlUtil.toString(((Float) content).doubleValue());
				}
				if (content instanceof Number) {
					return ((Number) content).toString();
				}
				return "";
			}

			@Override
			Object textToContent(int columnType, String text, Object oldContent) {
				return new BigDecimal(text);
			}

			@Override
			boolean isEditable(int columnType, Object content) {
				return true;
			}
		},
		CHAR {
			@Override
			String cellContentToText(int columnType, Object content) {
				return String.valueOf(content);
			}

			@Override
			Object textToContent(int columnType, String text, Object oldContent) {
				return text;
			}

			@Override
			boolean isEditable(int columnType, Object content) {
				return content == null || (content instanceof String && !(content.toString().indexOf('\n') >= 0 || content.toString().indexOf('\t') >= 0));
			}
		},
		DATE {
			SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
			SimpleDateFormat timeStampFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS");
		
			@Override
			String cellContentToText(int columnType, Object content) {
				if (columnType == Types.DATE && (content == null || content instanceof Date)) {
					return dateFormat.format((Date) content);
				}
				return timeStampFormat.format((Timestamp) content);
			}

			@Override
			Object textToContent(int columnType, String text, Object oldContent) {
				try {
					if (columnType == Types.DATE && (oldContent == null || oldContent instanceof Date)) {
						return new java.sql.Date(dateFormat.parse(text).getTime());
					}
					try {
						return new java.sql.Timestamp(timeStampFormat.parse(text).getTime());
					} catch (ParseException e) {
						if (text.length() <= 11) {
							return new java.sql.Timestamp(dateFormat.parse(text).getTime());
						}
					}
				} catch (ParseException e) {
					// ignore
				}
				return INVALID;
			}

			@Override
			boolean isEditable(int columnType, Object content) {
				return true;
			}
		};
		
		abstract boolean isEditable(int columnType, Object content);
		abstract String cellContentToText(int columnType, Object content);
		abstract Object textToContent(int columnType, String text, Object oldContent);
	};
	
	private Map<Integer, Converter> converterPerType = new HashMap<Integer, Converter>();
	{
		converterPerType.put(Types.DECIMAL, Converter.DECMAL);
		converterPerType.put(Types.DOUBLE, Converter.DECMAL);
		converterPerType.put(Types.FLOAT, Converter.DECMAL);
		converterPerType.put(Types.INTEGER, Converter.DECMAL);
		converterPerType.put(Types.NUMERIC, Converter.DECMAL);
		converterPerType.put(Types.SMALLINT, Converter.DECMAL);
		converterPerType.put(Types.TINYINT, Converter.DECMAL);
		converterPerType.put(Types.BIGINT, Converter.DECMAL);
		
		converterPerType.put(Types.CHAR, Converter.CHAR);
		converterPerType.put(Types.NCHAR, Converter.CHAR);
		converterPerType.put(Types.LONGNVARCHAR, Converter.CHAR);
		converterPerType.put(Types.LONGVARCHAR, Converter.CHAR);
		converterPerType.put(Types.NVARCHAR, Converter.CHAR);
		converterPerType.put(Types.VARCHAR, Converter.CHAR);

		converterPerType.put(Types.DATE, Converter.DATE);
		converterPerType.put(Types.TIMESTAMP, Converter.DATE);
	}
	
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
	 * @param table the table 
	 * @param row row number
	 * @param column column number
	 * @param content cell content
	 */
	public boolean isEditable(Table table, int row, int column, Object content) {
		if (column < 0 || column >= columnTypes.length) {
			return false;
		}
		if (table == null || table.getColumns().size() <= column) {
			return false;
		}
		Column theColumn = table.getColumns().get(column);
		if (theColumn.name == null || theColumn.isVirtual()) {
			return false;
		}
		Converter converter = converterPerType.get(columnTypes[column]);
		if (converter != null) {
			return converter.isEditable(columnTypes[column], content);
		}
		return false;
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
		if (content == null) {
			return "";
		}
		Converter converter = converterPerType.get(columnTypes[column]);
		if (converter != null) {
			try {
				return converter.cellContentToText(columnTypes[column], content);
			} catch (Exception e) {
				// ignore
			}
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
	public Object textToContent(int row, int column, String text, Object oldContent) {
		if (text.trim().equals("")) {
			return null;
		}
		Converter converter = converterPerType.get(columnTypes[column]);
		if (converter != null) {
			try {
				return converter.textToContent(columnTypes[column], text, oldContent);
			} catch (Exception e) {
				// ignore
			}
		}
		return INVALID;
	}

	/**
	 * Invalid content.
	 */
	public static final  Object INVALID = new Object();

}
