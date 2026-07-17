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
package net.sf.jailer.ui.databrowser.lob;

import net.sf.jailer.ui.databrowser.BinValue;
import net.sf.jailer.ui.databrowser.LobValue;

/**
 * Recognizes DataBrowser cell values that hold a BLOB/CLOB/binary value worth
 * showing in the {@link LobViewerPanel}, and builds a {@link LobContent} from
 * whatever is already materialized in the cell (the degraded fallback used when
 * the full content cannot be re-read from the database).
 *
 * <p>Mirrors the role of {@code SpatialCellSupport} for the geometry preview.
 *
 * @author Ralf Wisser
 */
public final class LobContentSupport {

	private LobContentSupport() {
	}

	/**
	 * Whether a cell's raw value is a LOB/binary value the viewer can handle.
	 */
	public static boolean isLobCell(Object cellValue) {
		return cellValue instanceof LobValue || cellValue instanceof BinValue || cellValue instanceof LobCellValue;
	}

	/**
	 * Detects the content type of a LOB/binary cell value from the data already
	 * materialized in the cell, without touching the database. Returns
	 * <code>null</code> when the type cannot be determined (e.g. a BLOB
	 * placeholder that only knows its length).
	 *
	 * @param cellValue the raw cell value
	 */
	public static LobContentType detectType(Object cellValue) {
		LobContent content = fromCellValue(cellValue, null, null, null);
		return content == null ? null : content.getType();
	}

	/**
	 * Builds a {@link LobContent} from the data already present in the cell
	 * value, without touching the database. Returns <code>null</code> when
	 * nothing usable is materialized (e.g. a BLOB placeholder that only knows
	 * its length).
	 *
	 * @param cellValue the raw cell value
	 * @param table     source table name (for the export filename)
	 * @param column    source column name (for the export filename)
	 * @param notice    notice to attach (e.g. why the full content is unavailable)
	 */
	public static LobContent fromCellValue(Object cellValue, String table, String column, String notice) {
		if (cellValue instanceof LobCellValue) {
			return LobContent.textInMemory(((LobCellValue) cellValue).getText(), table, column, notice);
		}
		if (cellValue instanceof BinValue) {
			byte[] content = ((BinValue) cellValue).getContent();
			if (content == null) {
				return null;
			}
			return LobContent.binaryInMemory(content, table, column, notice);
		}
		if (cellValue instanceof LobValue) {
			String s = String.valueOf(cellValue);
			// BLOB placeholders only carry their length ("<Blob> N bytes"); no bytes to show.
			if (s.startsWith("<Blob>")) {
				return null;
			}
			return LobContent.textInMemory(s, table, column, notice);
		}
		return null;
	}
}
