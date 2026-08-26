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

import java.util.Locale;

import net.sf.jailer.datamodel.Column;

/**
 * Rough classification of column types, as needed to decide whether a column can
 * take part in a foreign key and whether two columns are compatible.
 *
 * @author Ralf Wisser
 */
public class ColumnTypes {

	/**
	 * Type category of a column.
	 */
	public enum Category {

		/**
		 * Numeric types. Value ranges of these columns are comparable.
		 */
		NUMERIC,

		/**
		 * Short character types.
		 */
		STRING,

		/**
		 * Date/time types.
		 */
		TEMPORAL,

		/**
		 * UUID types.
		 */
		UUID,

		/**
		 * Anything that cannot be part of a key or is too expensive to profile
		 * (LOBs, binaries, XML, geometries, long text, ...).
		 */
		OTHER
	}

	/**
	 * Maximum length of a character column that is still considered a potential key column.
	 */
	private static final int MAX_STRING_LENGTH = 256;

	/**
	 * Determines the category of a column.
	 *
	 * @param column the column
	 * @return the category, {@link Category#OTHER} if the column cannot be part of a key
	 */
	public static Category categoryOf(Column column) {
		if (column == null || column.type == null || column.isVirtual()) {
			return Category.OTHER;
		}
		String type = column.type.toUpperCase(Locale.ENGLISH);
		if (type.contains("LOB") || type.contains("BINARY") || type.contains("BLOB")
				|| type.contains("RAW") || type.contains("LONG") || type.contains("XML")
				|| type.contains("GEOM") || type.contains("GEOGRAPHY") || type.contains("JSON")
				|| type.contains("ARRAY") || type.contains("BOOL") || type.contains("BIT")
				|| type.contains("CURSOR") || type.contains("OBJECT")) {
			return Category.OTHER;
		}
		if (type.contains("UUID") || type.contains("UNIQUEIDENTIFIER")) {
			return Category.UUID;
		}
		if (type.contains("INT") || type.contains("NUMBER") || type.contains("NUMERIC")
				|| type.contains("DECIMAL") || type.startsWith("DEC") || type.contains("FLOAT")
				|| type.contains("DOUBLE") || type.contains("REAL") || type.contains("MONEY")
				|| type.contains("SERIAL")) {
			return Category.NUMERIC;
		}
		if (type.contains("TIMESTAMP") || type.contains("DATE") || type.contains("TIME")) {
			return Category.TEMPORAL;
		}
		if (type.contains("CHAR") || type.equals("TEXT") || type.equals("NTEXT") || type.equals("STRING")) {
			if (column.length > MAX_STRING_LENGTH) {
				return Category.OTHER;
			}
			return Category.STRING;
		}
		return Category.OTHER;
	}

	/**
	 * Checks whether two columns are type-compatible, i.e. whether an equi-join
	 * between them is meaningful.
	 *
	 * @param a a column
	 * @param b another column
	 * @return <code>true</code> if both columns have the same, joinable category
	 */
	public static boolean areCompatible(Column a, Column b) {
		Category ca = categoryOf(a);
		if (ca == Category.OTHER) {
			return false;
		}
		return ca == categoryOf(b);
	}

	private ColumnTypes() {
	}

}
