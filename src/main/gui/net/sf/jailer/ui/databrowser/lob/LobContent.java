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

import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;

import javax.imageio.ImageIO;

import com.github.weisj.jsvg.SVGDocument;
import com.github.weisj.jsvg.parser.LoaderContext;
import com.github.weisj.jsvg.parser.SVGLoader;
import com.github.weisj.jsvg.view.FloatSize;
import com.github.weisj.jsvg.view.ViewBox;

import net.sf.jailer.util.LogUtil;

/**
 * The full (or best-available) content of one BLOB/CLOB cell, together with its
 * guessed {@link LobContentType}. This is the input model of the
 * {@link LobViewerPanel} and the source for exporting to a file.
 *
 * <p>The payload is held either in memory (small values) or in a temporary file
 * (large values streamed off the database). Binary-backed content keeps the raw
 * bytes; text-backed content (CLOB/XML) keeps decoded characters. A BLOB that
 * turns out to be text is binary-backed but has a text {@link LobContentType} -
 * it is decoded for display with {@link #displayCharset} and exported raw.
 *
 * @author Ralf Wisser
 */
public final class LobContent {

	/** Bytes of head kept in memory for detection / preview. */
	public static final int HEAD_SIZE = 8192;

	private final LobContentType type;
	private final boolean binaryBacked;
	private final byte[] bytes;       // full raw bytes, in memory (nullable)
	private final File file;          // full content on disk (nullable)
	private final String text;        // full decoded text, in memory (nullable)
	private final Charset displayCharset; // for decoding binary-backed text
	private final byte[] head;        // <= HEAD_SIZE bytes (nullable for pure text)
	private final long length;        // bytes (binary) / chars (text); -1 unknown
	private final String table;
	private final String column;
	private final String notice;      // e.g. "content truncated"; nullable
	private final String originalFileName; // e.g. a ZIP entry's file name; nullable
	private final String previewText; // derived preview (e.g. extracted Office text); nullable

	private LobContent(LobContentType type, boolean binaryBacked, byte[] bytes, File file, String text,
			Charset displayCharset, byte[] head, long length, String table, String column, String notice,
			String originalFileName, String previewText) {
		this.type = type;
		this.binaryBacked = binaryBacked;
		this.bytes = bytes;
		this.file = file;
		this.text = text;
		this.displayCharset = displayCharset != null ? displayCharset : StandardCharsets.UTF_8;
		this.head = head;
		this.length = length;
		this.table = table;
		this.column = column;
		this.notice = notice;
		this.originalFileName = originalFileName;
		this.previewText = previewText;
	}

	// --- factories ---------------------------------------------------------

	public static LobContent binaryInMemory(byte[] full, String table, String column, String notice) {
		byte[] head = full == null ? new byte[0] : (full.length <= HEAD_SIZE ? full : java.util.Arrays.copyOf(full, HEAD_SIZE));
		LobContentType type = LobTypeDetector.detect(head, full == null ? -1 : full.length);
		return new LobContent(type, true, full, null, null, LobText.resolveCharset(head), head, full == null ? 0 : full.length, table, column, notice, null, null);
	}

	public static LobContent binaryFile(File file, long length, byte[] head, String table, String column, String notice) {
		LobContentType type = LobTypeDetector.detect(head, length);
		return new LobContent(type, true, null, file, null, LobText.resolveCharset(head), head, length, table, column, notice, null, null);
	}

	public static LobContent textInMemory(String text, String table, String column, String notice) {
		LobContentType type = LobTypeDetector.detectText(text);
		long len = text == null ? 0 : text.length();
		byte[] head = text == null ? new byte[0]
				: text.substring(0, Math.min(text.length(), HEAD_SIZE)).getBytes(StandardCharsets.UTF_8);
		return new LobContent(type, false, null, null, text, StandardCharsets.UTF_8, head, len, table, column, notice, null, null);
	}

	public static LobContent textFile(File file, long lengthChars, String headText, String table, String column, String notice) {
		LobContentType type = LobTypeDetector.detectText(headText);
		byte[] head = headText == null ? new byte[0] : headText.getBytes(StandardCharsets.UTF_8);
		return new LobContent(type, false, null, file, null, StandardCharsets.UTF_8, head, lengthChars, table, column, notice, null, null);
	}

	/**
	 * Returns a copy of this content that carries the given original file name
	 * (e.g. a ZIP entry's name), used to derive a better export/open file name
	 * than the generic {@code table_column} name.
	 */
	public LobContent withOriginalFileName(String originalFileName) {
		return new LobContent(type, binaryBacked, bytes, file, text, displayCharset, head, length, table, column, notice, originalFileName, previewText);
	}

	/**
	 * Returns a copy of this content relabeled with the given type. The
	 * underlying payload (bytes/file/text) is left untouched, so export/open
	 * still operate on the real content - only the displayed type (and its
	 * extension/display name) changes.
	 */
	public LobContent withType(LobContentType type) {
		return new LobContent(type, binaryBacked, bytes, file, text, displayCharset, head, length, table, column, notice, originalFileName, previewText);
	}

	/**
	 * Returns a copy of this content carrying the given derived preview text
	 * (e.g. text extracted from a DOCX/XLSX/PPTX package), shown by the viewer
	 * instead of the raw payload without altering what "Save to file..."/"Open
	 * externally" export.
	 */
	public LobContent withPreviewText(String previewText) {
		return new LobContent(type, binaryBacked, bytes, file, text, displayCharset, head, length, table, column, notice, originalFileName, previewText);
	}

	// --- accessors ---------------------------------------------------------

	public LobContentType getType() {
		return type;
	}

	public boolean isText() {
		return type.isText();
	}

	public long getLength() {
		return length;
	}

	public String getTable() {
		return table;
	}

	public String getColumn() {
		return column;
	}

	public String getNotice() {
		return notice;
	}

	public String getOriginalFileName() {
		return originalFileName;
	}

	public String getPreviewText() {
		return previewText;
	}

	public byte[] getHead() {
		return head == null ? new byte[0] : head;
	}

	/**
	 * Whether the full content (not just the head) is accessible.
	 */
	public boolean hasFullContent() {
		return bytes != null || file != null || text != null;
	}

	/**
	 * Opens a stream over the full raw bytes (binary-backed content only).
	 */
	public InputStream openBinaryStream() throws IOException {
		if (bytes != null) {
			return new ByteArrayInputStream(bytes);
		}
		if (file != null) {
			return new BufferedInputStream(new FileInputStream(file));
		}
		return new ByteArrayInputStream(getHead());
	}

	/**
	 * Decodes and returns the content as an image, capped at <code>maxBytes</code>
	 * raw bytes. Returns <code>null</code> if the content is not an image type or
	 * cannot be decoded.
	 */
	public BufferedImage readImage(long maxBytes) throws IOException {
		if (!type.isImage()) {
			return null;
		}
		byte[] data = readAllBytes(maxBytes);
		if (type == LobContentType.SVG) {
			return renderSvg(data);
		}
		return ImageIO.read(new ByteArrayInputStream(data));
	}

	/** Clamp on a rasterized SVG's pixel dimensions (per side), guarding against a pathological viewBox. */
	private static final int SVG_MAX_DIMENSION = 4096;

	/** Small SVGs (icons) are rendered at least this large on their longer side, so they're a legible preview rather than a postage stamp. */
	private static final int SVG_MIN_DISPLAY_DIMENSION = 256;

	/**
	 * Parses and rasterizes an SVG document, scaling it (vector-crisp, via JSVG's
	 * {@link ViewBox}-targeted render) so its longer side is between
	 * {@link #SVG_MIN_DISPLAY_DIMENSION} and {@link #SVG_MAX_DIMENSION} - small
	 * icons are enlarged to a legible size, oversized documents are shrunk,
	 * proportionally in both cases. Returns <code>null</code> if the document
	 * cannot be parsed.
	 */
	private static BufferedImage renderSvg(byte[] data) {
		SVGDocument document;
		try {
			document = new SVGLoader().load(new ByteArrayInputStream(data), null, LoaderContext.createDefault());
		} catch (Throwable t) {
			LogUtil.warn(t);
			return null;
		}
		if (document == null) {
			return null;
		}
		FloatSize size = document.size();
		float w = size.width;
		float h = size.height;
		float longer = Math.max(w, h);
		if (longer > 0) {
			float scale = 1f;
			if (longer < SVG_MIN_DISPLAY_DIMENSION) {
				scale = SVG_MIN_DISPLAY_DIMENSION / longer;
			} else if (longer > SVG_MAX_DIMENSION) {
				scale = SVG_MAX_DIMENSION / longer;
			}
			w *= scale;
			h *= scale;
		}
		int targetW = Math.max(1, Math.round(w));
		int targetH = Math.max(1, Math.round(h));
		BufferedImage image = new BufferedImage(targetW, targetH, BufferedImage.TYPE_INT_ARGB);
		Graphics2D g = image.createGraphics();
		try {
			g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
			g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
			g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);
			g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);
			g.setRenderingHint(RenderingHints.KEY_ALPHA_INTERPOLATION, RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY);
			document.render(null, g, new ViewBox(0, 0, targetW, targetH));
		} finally {
			g.dispose();
		}
		return image;
	}

	/**
	 * Reads the full raw bytes into memory, capped at <code>maxBytes</code>.
	 * Intended for images/archives which need the whole (bounded) payload.
	 */
	public byte[] readAllBytes(long maxBytes) throws IOException {
		if (bytes != null) {
			return bytes.length <= maxBytes ? bytes : java.util.Arrays.copyOf(bytes, (int) maxBytes);
		}
		if (file != null) {
			try (InputStream in = openBinaryStream()) {
				return readCapped(in, maxBytes);
			}
		}
		return getHead();
	}

	/**
	 * Returns text for display, capped at <code>maxChars</code> characters.
	 */
	public String getTextForDisplay(int maxChars) throws IOException {
		String s;
		if (text != null) {
			s = text;
		} else if (!binaryBacked && file != null) {
			s = new String(readCapped(new FileInputStream(file), (long) maxChars * 4), displayCharset);
		} else if (bytes != null) {
			s = new String(bytes, displayCharset);
		} else if (binaryBacked && file != null) {
			s = new String(readCapped(openBinaryStream(), (long) maxChars * 4), displayCharset);
		} else {
			s = LobText.decode(getHead());
		}
		if (s.length() > maxChars) {
			s = s.substring(0, maxChars);
		}
		return s;
	}

	/**
	 * Exports the full content to a file: raw bytes for binary-backed content,
	 * decoded text (UTF-8) for text-backed content.
	 */
	public void exportTo(File dest) throws IOException {
		if (binaryBacked) {
			if (bytes != null) {
				try (OutputStream out = new FileOutputStream(dest)) {
					out.write(bytes);
				}
			} else if (file != null) {
				Files.copy(file.toPath(), dest.toPath(), java.nio.file.StandardCopyOption.REPLACE_EXISTING);
			} else {
				try (OutputStream out = new FileOutputStream(dest)) {
					out.write(getHead());
				}
			}
		} else {
			if (text != null) {
				Files.write(dest.toPath(), text.getBytes(StandardCharsets.UTF_8));
			} else if (file != null) {
				Files.copy(file.toPath(), dest.toPath(), java.nio.file.StandardCopyOption.REPLACE_EXISTING);
			} else {
				Files.write(dest.toPath(), getHead());
			}
		}
	}

	private static byte[] readCapped(InputStream in, long maxBytes) throws IOException {
		try {
			java.io.ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
			byte[] buf = new byte[8192];
			long total = 0;
			int n;
			while (total < maxBytes && (n = in.read(buf, 0, (int) Math.min(buf.length, maxBytes - total))) != -1) {
				bos.write(buf, 0, n);
				total += n;
			}
			return bos.toByteArray();
		} finally {
			in.close();
		}
	}
}
