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
package net.sf.jailer.ui.associationdiscovery;

import java.util.HashMap;
import java.util.Map;

import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.Quoting;

/**
 * Statistical profile of a table, gathered by the {@link TableProfiler}.
 * Used to pre-filter association candidates and to determine their cardinality.
 *
 * @author Ralf Wisser
 */
public class TableProfile {

	/**
	 * Statistics of a single column. A count of <code>-1</code> means "unknown".
	 */
	public static class ColumnProfile {

		public final Column column;

		/**
		 * Number of rows in which the column is not null.
		 */
		public long nonNullCount = -1;

		/**
		 * Number of distinct non-null values.
		 */
		public long distinctCount = -1;

		/**
		 * Smallest value, for numeric columns only.
		 */
		public Double min;

		/**
		 * Largest value, for numeric columns only.
		 */
		public Double max;

		public ColumnProfile(Column column) {
			this.column = column;
		}

		/**
		 * @return <code>true</code> if the value range of this column is known
		 */
		public boolean hasRange() {
			return min != null && max != null;
		}
	}

	public final Table table;

	/**
	 * Number of rows, <code>-1</code> if unknown.
	 */
	public long rowCount = -1;

	/**
	 * <code>true</code> if the profile is based on a sample instead of the whole table.
	 */
	public boolean sampled;

	/**
	 * <code>true</code> if the table could not be profiled at all.
	 */
	public boolean failed;

	private final Map<String, ColumnProfile> columnProfiles = new HashMap<String, ColumnProfile>();

	public TableProfile(Table table) {
		this.table = table;
	}

	/**
	 * Gets the profile of a column.
	 *
	 * @param column the column
	 * @return the profile, or <code>null</code> if the column has not been profiled
	 */
	public ColumnProfile get(Column column) {
		return columnProfiles.get(Quoting.normalizeIdentifier(column.name));
	}

	/**
	 * Creates or returns the profile of a column.
	 *
	 * @param column the column
	 * @return the profile
	 */
	public ColumnProfile getOrCreate(Column column) {
		String key = Quoting.normalizeIdentifier(column.name);
		ColumnProfile profile = columnProfiles.get(key);
		if (profile == null) {
			profile = new ColumnProfile(column);
			columnProfiles.put(key, profile);
		}
		return profile;
	}

}
