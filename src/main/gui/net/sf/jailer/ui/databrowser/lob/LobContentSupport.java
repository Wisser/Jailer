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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.zip.GZIPInputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import net.sf.jailer.ui.databrowser.BinValue;
import net.sf.jailer.ui.databrowser.LobValue;
import net.sf.jailer.util.LogUtil;

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

	/**
	 * If the given content is a single-file archive - a GZIP stream, or a ZIP
	 * holding exactly one file entry - decompresses that single file and returns
	 * it as a fresh {@link LobContent} with its own guessed
	 * {@link LobContentType}. Returns <code>null</code> when drilling in is not
	 * applicable, so the caller falls back to rendering the archive itself.
	 *
	 * <p>Only one level is unwrapped: a single-file archive nested inside another
	 * is returned as its (archive) type, not recursed into. The decompressed size
	 * is bounded by <code>maxBytes</code>; content exceeding it yields
	 * <code>null</code> (too large to preview inline). Any read/format error also
	 * yields <code>null</code>.
	 *
	 * @param archive  the archive content (only ZIP/GZIP are considered)
	 * @param maxBytes cap on the decompressed size held in memory
	 * @return the decompressed inner content, or <code>null</code>
	 */
	public static LobContent unwrapSingleFileArchive(LobContent archive, long maxBytes) {
		if (archive == null || !archive.hasFullContent()) {
			return null;
		}
		try {
			if (archive.getType() == LobContentType.GZIP) {
				try (InputStream in = new GZIPInputStream(archive.openBinaryStream())) {
					byte[] bytes = readBounded(in, maxBytes);
					if (bytes == null || bytes.length == 0) {
						return null;
					}
					return LobContent.binaryInMemory(bytes, archive.getTable(), archive.getColumn(),
							"Decompressed from GZIP");
				}
			}
			if (archive.getType() == LobContentType.ZIP) {
				try (ZipInputStream zis = new ZipInputStream(archive.openBinaryStream())) {
					ZipEntry entry;
					byte[] bytes = null;
					String entryName = null;
					while ((entry = zis.getNextEntry()) != null) {
						if (entry.isDirectory()) {
							zis.closeEntry();
							continue;
						}
						if (bytes != null) {
							return null; // more than one file entry - keep the entry-list view
						}
						bytes = readBounded(zis, maxBytes);
						entryName = entry.getName();
						zis.closeEntry();
						if (bytes == null) {
							return null; // over the cap
						}
					}
					if (bytes == null || bytes.length == 0) {
						return null;
					}
					return LobContent.binaryInMemory(bytes, archive.getTable(), archive.getColumn(),
							"Extracted from ZIP entry \"" + entryName + "\"").withOriginalFileName(entryName);
				}
			}
		} catch (IOException e) {
			LogUtil.warn(e);
			return null;
		} catch (RuntimeException e) {
			LogUtil.warn(e);
			return null;
		}
		return null;
	}

	/**
	 * Reads up to <code>maxBytes</code> from the stream. Returns <code>null</code>
	 * if the stream holds more than <code>maxBytes</code> (detected by reading one
	 * extra byte).
	 */
	static byte[] readBounded(InputStream in, long maxBytes) throws IOException {
		ByteArrayOutputStream bos = new ByteArrayOutputStream();
		byte[] buf = new byte[8192];
		long total = 0;
		int n;
		while ((n = in.read(buf)) != -1) {
			total += n;
			if (total > maxBytes) {
				return null;
			}
			bos.write(buf, 0, n);
		}
		return bos.toByteArray();
	}
}
