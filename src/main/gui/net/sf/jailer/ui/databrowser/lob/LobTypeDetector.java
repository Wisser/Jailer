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

import java.util.Locale;

/**
 * Guesses the {@link LobContentType} of a byte buffer (the leading bytes of a
 * BLOB) or of an already decoded text (a CLOB) by inspecting magic bytes and
 * the leading characters. Pure and side-effect free, mirroring the role of
 * {@code SpatialCellSupport.parse(...)} for the geometry preview.
 *
 * @author Ralf Wisser
 */
public final class LobTypeDetector {

	private LobTypeDetector() {
	}

	/**
	 * Guesses the content type of a binary value from its leading bytes.
	 *
	 * @param head        the first bytes of the content (~4 KB is plenty)
	 * @param totalLength the total content length in bytes, or -1 if unknown
	 * @return the guessed type, never <code>null</code>
	 */
	public static LobContentType detect(byte[] head, long totalLength) {
		if (head == null || head.length == 0) {
			return LobContentType.BINARY;
		}
		if (startsWith(head, 0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A)) {
			return LobContentType.PNG;
		}
		if (startsWith(head, 0xFF, 0xD8, 0xFF)) {
			return LobContentType.JPEG;
		}
		if (isGif(head)) {
			return LobContentType.GIF;
		}
		if (isBmp(head)) {
			return LobContentType.BMP;
		}
		if (startsWith(head, 0x25, 0x50, 0x44, 0x46, 0x2D)) { // "%PDF-"
			return LobContentType.PDF;
		}
		if (startsWith(head, 0x1F, 0x8B)) {
			return LobContentType.GZIP;
		}
		if (isZip(head)) {
			return LobContentType.ZIP;
		}
		if (LobText.looksLikeText(head)) {
			return classifyText(LobText.decode(head));
		}
		return LobContentType.BINARY;
	}

	/**
	 * Classifies an already decoded text (e.g. a CLOB) into one of the text
	 * sub-types. Never returns a non-text type.
	 *
	 * @param text the (possibly truncated) text content
	 * @return a text {@link LobContentType}, never <code>null</code>
	 */
	public static LobContentType detectText(String text) {
		return classifyText(text);
	}

	private static LobContentType classifyText(String text) {
		if (text == null) {
			return LobContentType.PLAIN_TEXT;
		}
		String s = text.trim();
		if (s.isEmpty()) {
			return LobContentType.PLAIN_TEXT;
		}
		String lower = s.toLowerCase(Locale.ENGLISH);
		if (lower.startsWith("<!doctype html") || lower.startsWith("<html")
				|| lower.startsWith("<head") || lower.startsWith("<body")) {
			return LobContentType.HTML;
		}
		if (lower.startsWith("<?xml")) {
			return isSvgAfterXmlDecl(s) ? LobContentType.SVG : LobContentType.XML;
		}
		if (s.charAt(0) == '<') {
			if (lower.startsWith("<svg")) {
				return LobContentType.SVG;
			}
			// generic markup: treat as XML unless it clearly looks like HTML
			if (lower.contains("<html") || lower.contains("<!doctype html")) {
				return LobContentType.HTML;
			}
			return LobContentType.XML;
		}
		char c0 = s.charAt(0);
		if (c0 == '{' || c0 == '[') {
			return LobContentType.JSON;
		}
		return LobContentType.PLAIN_TEXT;
	}

	/**
	 * After an {@code <?xml ...?>} declaration, checks whether the root element
	 * (skipping an optional {@code <!DOCTYPE ...>}) is {@code <svg}.
	 */
	private static boolean isSvgAfterXmlDecl(String s) {
		int end = s.indexOf("?>");
		if (end < 0) {
			return false;
		}
		String rest = s.substring(end + 2).trim();
		if (rest.toLowerCase(Locale.ENGLISH).startsWith("<!doctype")) {
			int docTypeEnd = rest.indexOf('>');
			if (docTypeEnd < 0) {
				return false;
			}
			rest = rest.substring(docTypeEnd + 1).trim();
		}
		return rest.toLowerCase(Locale.ENGLISH).startsWith("<svg");
	}

	private static boolean isGif(byte[] b) {
		// "GIF87a" or "GIF89a"
		if (!startsWith(b, 0x47, 0x49, 0x46, 0x38)) {
			return false;
		}
		if (b.length < 6) {
			return false;
		}
		int v = b[4] & 0xFF;
		return (v == 0x37 || v == 0x39) && (b[5] & 0xFF) == 0x61;
	}

	private static boolean isBmp(byte[] b) {
		// "BM" plus a known DIB header size at offset 14 - guards against
		// ordinary text that happens to start with the letters "BM".
		if (!startsWith(b, 0x42, 0x4D) || b.length < 18) {
			return false;
		}
		int dibHeaderSize = le32(b, 14);
		return dibHeaderSize == 12 || dibHeaderSize == 40 || dibHeaderSize == 52
				|| dibHeaderSize == 56 || dibHeaderSize == 64 || dibHeaderSize == 108
				|| dibHeaderSize == 124;
	}

	private static boolean isZip(byte[] b) {
		// local file header, empty archive, or spanned archive marker
		return startsWith(b, 0x50, 0x4B, 0x03, 0x04)
				|| startsWith(b, 0x50, 0x4B, 0x05, 0x06)
				|| startsWith(b, 0x50, 0x4B, 0x07, 0x08);
	}

	private static int le32(byte[] b, int off) {
		return (b[off] & 0xFF) | ((b[off + 1] & 0xFF) << 8)
				| ((b[off + 2] & 0xFF) << 16) | ((b[off + 3] & 0xFF) << 24);
	}

	private static boolean startsWith(byte[] b, int... prefix) {
		if (b.length < prefix.length) {
			return false;
		}
		for (int i = 0; i < prefix.length; i++) {
			if ((b[i] & 0xFF) != (prefix[i] & 0xFF)) {
				return false;
			}
		}
		return true;
	}
}
