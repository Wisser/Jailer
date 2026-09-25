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
package net.sf.jailer.ui.databrowser.compare;

import java.math.BigDecimal;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.SQLXML;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.sf.jailer.ui.databrowser.LobValue;
import net.sf.jailer.ui.databrowser.SQLValue;
import net.sf.jailer.util.Quoting;

/**
 * Compares rows of two sides (two rows of one table, two result tabs, or a table
 * in two databases) column by column.
 * <p>
 * Columns are aligned by name (ignoring quoting and case), rows are aligned by key.
 *
 * @author Ralf Wisser
 */
public class RowComparison {

	/**
	 * Status of a pair of rows.
	 */
	public enum Status {
		EQUAL("equal"), CHANGED("different"), ONLY_LEFT("only left"), ONLY_RIGHT("only right");

		public final String description;

		private Status(String description) {
			this.description = description;
		}
	}

	/**
	 * Status of a single cell.
	 */
	public enum CellStatus {
		EQUAL, CHANGED, NOT_COMPARED, MISSING
	}

	/**
	 * One side of the comparison.
	 */
	public static class Side {
		public final String title;
		public final List<String> columns;
		public final List<Object[]> rows;
		public final boolean truncated;
		private final BiFunction<Integer, Object, String> display;
		private Set<Integer> pkColumns = Collections.emptySet();
		private Set<Integer> fkColumns = Collections.emptySet();

		/**
		 * @param title title of the side (alias, tab name, ...)
		 * @param columns column names
		 * @param rows the rows, each an array of cell values in column order
		 * @param truncated <code>true</code> if the rows have been cut by a row limit
		 * @param display renders a cell value (column index, value) as text, or <code>null</code> for the default rendering
		 */
		public Side(String title, List<String> columns, List<Object[]> rows, boolean truncated, BiFunction<Integer, Object, String> display) {
			this.title = title;
			this.columns = columns;
			this.rows = rows;
			this.truncated = truncated;
			this.display = display;
		}

		/**
		 * Sets the indexes of the primary key and foreign key columns of this side.
		 *
		 * @return this side
		 */
		public Side withKeyColumns(Set<Integer> pk, Set<Integer> fk) {
			this.pkColumns = pk == null? Collections.<Integer>emptySet() : pk;
			this.fkColumns = fk == null? Collections.<Integer>emptySet() : fk;
			return this;
		}

		/**
		 * Renders a cell value as text.
		 */
		public String toText(int column, Object value) {
			if (value == null) {
				return null;
			}
			if (display != null) {
				try {
					return display.apply(column, value);
				} catch (RuntimeException e) {
					// fall through
				}
			}
			return String.valueOf(value);
		}
	}

	/**
	 * A pair of rows, one of which may be missing.
	 */
	public static class RowPair {
		public final String key;
		public final Object[] left;
		public final Object[] right;
		public final Status status;
		public final int changedColumns;
		public final boolean duplicateKey;

		private RowPair(String key, Object[] left, Object[] right, Status status, int changedColumns, boolean duplicateKey) {
			this.key = key;
			this.left = left;
			this.right = right;
			this.status = status;
			this.changedColumns = changedColumns;
			this.duplicateKey = duplicateKey;
		}
	}

	public final Side left;
	public final Side right;

	/**
	 * The aligned column names. Columns of the left side come first.
	 */
	public final List<String> columns = new ArrayList<String>();

	/**
	 * Index into the columns of the left/right side per aligned column, -1 if the side has no such column.
	 */
	private final List<Integer> leftIndex = new ArrayList<Integer>();
	private final List<Integer> rightIndex = new ArrayList<Integer>();

	public RowComparison(Side left, Side right) {
		this.left = left;
		this.right = right;
		Map<String, Integer> rightPos = new HashMap<String, Integer>();
		for (int i = 0; i < right.columns.size(); ++i) {
			rightPos.putIfAbsent(normalizeName(right.columns.get(i)), i);
		}
		Map<String, Integer> used = new HashMap<String, Integer>();
		for (int i = 0; i < left.columns.size(); ++i) {
			String name = normalizeName(left.columns.get(i));
			Integer r = rightPos.get(name);
			if (r != null && used.containsKey(name)) {
				r = null;
			}
			used.put(name, i);
			columns.add(plainName(left.columns.get(i)));
			leftIndex.add(i);
			rightIndex.add(r == null? -1 : r);
		}
		for (int i = 0; i < right.columns.size(); ++i) {
			if (!rightIndex.contains(i)) {
				columns.add(plainName(right.columns.get(i)));
				leftIndex.add(-1);
				rightIndex.add(i);
			}
		}
	}

	/**
	 * Normalizes a column name for alignment.
	 */
	public static String normalizeName(String name) {
		return Quoting.staticUnquote(plainName(name)).toUpperCase(Locale.ENGLISH);
	}

	private static final Pattern BOLD_PATTERN = Pattern.compile("<b>(.*?)</b>", Pattern.DOTALL);

	/**
	 * Gets the plain name of a column. The SQL Console labels its columns with multi-line HTML
	 * (table, name in bold, type), of which only the name is wanted.
	 */
	public static String plainName(String name) {
		if (name == null) {
			return "";
		}
		if (!name.startsWith("<html>")) {
			return name;
		}
		String plain;
		Matcher m = BOLD_PATTERN.matcher(name);
		if (m.find()) {
			plain = m.group(1).replaceAll("<[^>]*>", "");
		} else {
			plain = name.replaceAll("<[^>]*>", " ").replaceAll("\\s+", " ");
		}
		return plain.replace("&lt;", "<").replace("&gt;", ">").replace("&quot;", "\"").replace("&nbsp;", " ").replace("&amp;", "&").trim();
	}

	/**
	 * Gets the index of an aligned column in the left side, or -1.
	 */
	public int leftIndex(int column) {
		return leftIndex.get(column);
	}

	/**
	 * Gets the index of an aligned column in the right side, or -1.
	 */
	public int rightIndex(int column) {
		return rightIndex.get(column);
	}

	/**
	 * Whether an aligned column belongs to a primary key (as known by the left side, or else the right side).
	 */
	public boolean isPrimaryKey(int column) {
		int l = leftIndex.get(column);
		return l >= 0? left.pkColumns.contains(l) : right.pkColumns.contains(rightIndex.get(column));
	}

	/**
	 * Whether an aligned column belongs to a foreign key (as known by the left side, or else the right side).
	 */
	public boolean isForeignKey(int column) {
		int l = leftIndex.get(column);
		return l >= 0? left.fkColumns.contains(l) : right.fkColumns.contains(rightIndex.get(column));
	}

	/**
	 * Gets the aligned columns present on both sides.
	 */
	public List<Integer> commonColumns() {
		List<Integer> result = new ArrayList<Integer>();
		for (int i = 0; i < columns.size(); ++i) {
			if (leftIndex.get(i) >= 0 && rightIndex.get(i) >= 0) {
				result.add(i);
			}
		}
		return result;
	}

	/**
	 * Gets the left value of an aligned column.
	 */
	public Object leftValue(RowPair pair, int column) {
		int i = leftIndex.get(column);
		return pair.left == null || i < 0 || i >= pair.left.length? null : pair.left[i];
	}

	/**
	 * Gets the right value of an aligned column.
	 */
	public Object rightValue(RowPair pair, int column) {
		int i = rightIndex.get(column);
		return pair.right == null || i < 0 || i >= pair.right.length? null : pair.right[i];
	}

	/**
	 * Compares one cell of a pair.
	 */
	public CellStatus cellStatus(RowPair pair, int column) {
		if (pair.left == null || pair.right == null || leftIndex.get(column) < 0 || rightIndex.get(column) < 0) {
			return CellStatus.MISSING;
		}
		Object l = leftValue(pair, column);
		Object r = rightValue(pair, column);
		if (l == null || r == null) {
			return l == null && r == null? CellStatus.EQUAL : CellStatus.CHANGED;
		}
		if (isLob(l) || isLob(r)) {
			return CellStatus.NOT_COMPARED;
		}
		return normalize(l).equals(normalize(r))? CellStatus.EQUAL : CellStatus.CHANGED;
	}

	/**
	 * Creates a pair of two given rows.
	 */
	public RowPair pair(String key, Object[] l, Object[] r) {
		return pair(key, l, r, false);
	}

	private RowPair pair(String key, Object[] l, Object[] r, boolean duplicateKey) {
		if (l == null) {
			return new RowPair(key, null, r, Status.ONLY_RIGHT, 0, duplicateKey);
		}
		if (r == null) {
			return new RowPair(key, l, null, Status.ONLY_LEFT, 0, duplicateKey);
		}
		RowPair tmp = new RowPair(key, l, r, Status.EQUAL, 0, duplicateKey);
		int changed = 0;
		for (int i = 0; i < columns.size(); ++i) {
			// a column present on one side only is shown, but doesn't make every row different
			if (cellStatus(tmp, i) == CellStatus.CHANGED) {
				++changed;
			}
		}
		return new RowPair(key, l, r, changed == 0? Status.EQUAL : Status.CHANGED, changed, duplicateKey);
	}

	/**
	 * Aligns the rows of both sides by key.
	 *
	 * @param keyColumns aligned column indexes forming the key (must be present on both sides)
	 * @return the pairs, in the order of the left side, followed by the rows only present on the right side
	 */
	public List<RowPair> matchByKey(List<Integer> keyColumns) {
		Map<String, List<Object[]>> rightByKey = new LinkedHashMap<String, List<Object[]>>();
		for (Object[] r: right.rows) {
			rightByKey.computeIfAbsent(key(r, keyColumns, false), k -> new ArrayList<Object[]>()).add(r);
		}
		Map<String, Integer> leftCount = new HashMap<String, Integer>();
		for (Object[] l: left.rows) {
			leftCount.merge(key(l, keyColumns, true), 1, Integer::sum);
		}
		List<RowPair> result = new ArrayList<RowPair>();
		for (Object[] l: left.rows) {
			String key = key(l, keyColumns, true);
			List<Object[]> candidates = rightByKey.get(key);
			Object[] r = candidates == null || candidates.isEmpty()? null : candidates.remove(0);
			boolean dup = leftCount.get(key) > 1 || (candidates != null && !candidates.isEmpty());
			result.add(pair(displayKey(l, keyColumns, true), l, r, dup));
		}
		for (List<Object[]> rest: rightByKey.values()) {
			for (Object[] r: rest) {
				result.add(pair(displayKey(r, keyColumns, false), null, r, rest.size() > 1));
			}
		}
		return result;
	}

	private String key(Object[] row, List<Integer> keyColumns, boolean isLeft) {
		StringBuilder sb = new StringBuilder();
		for (int c: keyColumns) {
			int i = isLeft? leftIndex.get(c) : rightIndex.get(c);
			Object v = i < 0 || i >= row.length? null : row[i];
			sb.append(v == null? "\u0000" : normalize(v)).append('\u0001');
		}
		return sb.toString();
	}

	/**
	 * Renders the key values of a row, like "7839, KING".
	 */
	public String displayKey(Object[] row, List<Integer> keyColumns, boolean isLeft) {
		StringBuilder sb = new StringBuilder();
		for (int c: keyColumns) {
			int i = isLeft? leftIndex.get(c) : rightIndex.get(c);
			Object v = i < 0 || i >= row.length? null : row[i];
			if (sb.length() > 0) {
				sb.append(", ");
			}
			String text = (isLeft? left : right).toText(i, v);
			sb.append(text == null? "null" : text);
		}
		return sb.toString();
	}

	/**
	 * Whether a value is a LOB (or a placeholder of one), which is not compared.
	 */
	public static boolean isLob(Object value) {
		return value instanceof Blob || value instanceof Clob || value instanceof SQLXML
				|| value instanceof byte[] || value instanceof LobValue || value instanceof SQLValue;
	}

	/**
	 * Normalizes a value for comparison: numbers are compared by value (1 equals 1.0).
	 */
	public static String normalize(Object value) {
		if (value == null) {
			return "";
		}
		if (value instanceof Number) {
			try {
				return new BigDecimal(value.toString()).stripTrailingZeros().toPlainString();
			} catch (NumberFormatException e) {
				// NaN, Infinity
			}
		}
		return value.toString();
	}

}
