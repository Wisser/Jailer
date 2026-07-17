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

import java.nio.ByteBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Character-set resolution and text-vs-binary heuristics for BLOB content that
 * might actually be text. CLOB/XML content is already Unicode and does not need
 * any of this.
 *
 * <p>Charset resolution order: byte-order-mark, then an XML {@code encoding="…"}
 * declaration, then UTF-8 validity, and finally ISO-8859-1 as a never-failing
 * 1:1 fallback so raw bytes round-trip on export.
 *
 * @author Ralf Wisser
 */
public final class LobText {

	private LobText() {
	}

	/**
	 * Maximum fraction of "binary-looking" control bytes tolerated in text.
	 */
	private static final double MAX_CONTROL_RATIO = 0.05;

	private static final Pattern XML_ENCODING = Pattern.compile(
			"<\\?xml[^>]*encoding\\s*=\\s*[\"']([^\"']+)[\"']", Pattern.CASE_INSENSITIVE);

	/**
	 * Number of byte-order-mark bytes at the start of the buffer (0 if none).
	 */
	public static int bomLength(byte[] b) {
		if (b == null) {
			return 0;
		}
		if (startsWith(b, 0x00, 0x00, 0xFE, 0xFF) || startsWith(b, 0xFF, 0xFE, 0x00, 0x00)) {
			return 4; // UTF-32
		}
		if (startsWith(b, 0xEF, 0xBB, 0xBF)) {
			return 3; // UTF-8
		}
		if (startsWith(b, 0xFE, 0xFF) || startsWith(b, 0xFF, 0xFE)) {
			return 2; // UTF-16
		}
		return 0;
	}

	/**
	 * Resolves the character set of a byte buffer that is assumed to be text.
	 * Never returns <code>null</code>.
	 */
	public static Charset resolveCharset(byte[] head) {
		if (head == null || head.length == 0) {
			return StandardCharsets.UTF_8;
		}
		if (startsWith(head, 0x00, 0x00, 0xFE, 0xFF)) {
			return charset("UTF-32BE", StandardCharsets.UTF_8);
		}
		if (startsWith(head, 0xFF, 0xFE, 0x00, 0x00)) {
			return charset("UTF-32LE", StandardCharsets.UTF_8);
		}
		if (startsWith(head, 0xEF, 0xBB, 0xBF)) {
			return StandardCharsets.UTF_8;
		}
		if (startsWith(head, 0xFE, 0xFF)) {
			return StandardCharsets.UTF_16BE;
		}
		if (startsWith(head, 0xFF, 0xFE)) {
			return StandardCharsets.UTF_16LE;
		}
		// XML declaration
		String ascii = new String(head, 0, Math.min(head.length, 400), StandardCharsets.ISO_8859_1);
		Matcher m = XML_ENCODING.matcher(ascii);
		if (m.find()) {
			Charset c = charset(m.group(1).trim(), null);
			if (c != null) {
				return c;
			}
		}
		if (isValidUtf8(head)) {
			return StandardCharsets.UTF_8;
		}
		return StandardCharsets.ISO_8859_1;
	}

	/**
	 * Heuristically decides whether a byte buffer holds text (as opposed to
	 * opaque binary data).
	 */
	public static boolean looksLikeText(byte[] head) {
		if (head == null || head.length == 0) {
			return true;
		}
		if (bomLength(head) > 0) {
			return true;
		}
		if (isValidUtf8(head) && controlRatio(head, StandardCharsets.UTF_8) <= MAX_CONTROL_RATIO) {
			return true;
		}
		// fall back to a printable-Latin-1 check (Windows-1252 style text)
		return controlRatio(head, StandardCharsets.ISO_8859_1) <= MAX_CONTROL_RATIO;
	}

	/**
	 * Decodes a byte buffer to text using the resolved charset, stripping any
	 * byte-order-mark.
	 */
	public static String decode(byte[] bytes) {
		if (bytes == null || bytes.length == 0) {
			return "";
		}
		Charset cs = resolveCharset(bytes);
		int bom = bomLength(bytes);
		return new String(bytes, bom, bytes.length - bom, cs);
	}

	/**
	 * Checks whether the buffer is valid UTF-8, tolerating a truncated
	 * multi-byte sequence at the very end (the head may cut a character).
	 */
	public static boolean isValidUtf8(byte[] b) {
		if (b == null || b.length == 0) {
			return true;
		}
		for (int trim = 0; trim <= 3 && trim < b.length; trim++) {
			CharsetDecoder dec = StandardCharsets.UTF_8.newDecoder();
			dec.onMalformedInput(CodingErrorAction.REPORT);
			dec.onUnmappableCharacter(CodingErrorAction.REPORT);
			try {
				dec.decode(ByteBuffer.wrap(b, 0, b.length - trim));
				return true;
			} catch (CharacterCodingException e) {
				// try trimming one more trailing byte (possible truncated char)
			}
		}
		return false;
	}

	private static double controlRatio(byte[] b, Charset cs) {
		String s = new String(b, cs);
		if (s.isEmpty()) {
			return 0;
		}
		int control = 0;
		for (int i = 0; i < s.length(); i++) {
			char c = s.charAt(i);
			if (c == '\t' || c == '\n' || c == '\r' || c == '\f') {
				continue;
			}
			if (c == 0xFFFD /* replacement char */ || c < 0x20 || c == 0x7F) {
				control++;
			}
		}
		return control / (double) s.length();
	}

	private static Charset charset(String name, Charset fallback) {
		try {
			return Charset.forName(name);
		} catch (Exception e) {
			return fallback;
		}
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
