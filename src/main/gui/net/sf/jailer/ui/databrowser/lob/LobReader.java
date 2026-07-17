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

import java.io.BufferedOutputStream;
import java.io.BufferedWriter;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.SQLException;
import java.sql.SQLXML;

import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.util.CancellationHandler;

/**
 * Reads the full content of a live JDBC {@link Blob}/{@link Clob}/{@link SQLXML}
 * (or a raw {@code byte[]}) into a {@link LobContent}. Small values are kept in
 * memory; larger ones are streamed to a temporary file so a multi-hundred-MB LOB
 * never becomes a single byte array. The read polls the cancellation context so
 * the user can abort it, and deletes any partial temp file on failure/abort.
 *
 * <p>Must be called while the originating {@code ResultSet} is still open.
 *
 * @author Ralf Wisser
 */
public final class LobReader {

	/** Values above this size are streamed to a temp file instead of memory. */
	private static final long INMEMORY_THRESHOLD = 8L * 1024 * 1024;

	private static final int BUFFER = 64 * 1024;

	private LobReader() {
	}

	/**
	 * Reads the given JDBC object into a {@link LobContent}.
	 *
	 * @param obj     a {@link Blob}, {@link Clob}, {@link SQLXML}, {@code byte[]} or other
	 * @param context cancellation context (see {@link CancellationHandler})
	 * @param table   source table name (for the export filename)
	 * @param column  source column name (for the export filename)
	 */
	public static LobContent read(Object obj, Object context, String table, String column) throws SQLException, IOException {
		if (obj instanceof Blob) {
			Blob blob = (Blob) obj;
			try (InputStream in = blob.getBinaryStream()) {
				return readBinary(in, context, table, column);
			}
		}
		if (obj instanceof Clob) {
			Clob clob = (Clob) obj;
			try (Reader in = clob.getCharacterStream()) {
				return readText(in, context, table, column);
			}
		}
		if (obj instanceof SQLXML) {
			SQLXML xml = (SQLXML) obj;
			try (Reader in = xml.getCharacterStream()) {
				return readText(in, context, table, column);
			}
		}
		if (obj instanceof byte[]) {
			return LobContent.binaryInMemory((byte[]) obj, table, column, null);
		}
		return LobContent.textInMemory(String.valueOf(obj), table, column, null);
	}

	/**
	 * Reads only the leading bytes/characters of the given JDBC object - just
	 * enough to guess its {@link LobContentType} - and classifies it. Unlike
	 * {@link #read}, the full content is never buffered: reading stops after
	 * <code>maxHeadBytes</code>. Polls the cancellation context.
	 *
	 * @param obj          a {@link Blob}, {@link Clob}, {@link SQLXML}, {@code byte[]} or other
	 * @param context      cancellation context (see {@link CancellationHandler})
	 * @param maxHeadBytes number of leading bytes (BLOB/{@code byte[]}) or characters (CLOB/XML) to inspect
	 * @return the guessed type, never <code>null</code>
	 */
	public static LobContentType detectType(Object obj, Object context, int maxHeadBytes) throws SQLException, IOException {
		if (obj instanceof Blob) {
			Blob blob = (Blob) obj;
			long length;
			try {
				length = blob.length();
			} catch (Exception e) {
				length = -1;
			}
			try (InputStream in = blob.getBinaryStream()) {
				return LobTypeDetector.detect(readHead(in, maxHeadBytes, context), length);
			}
		}
		if (obj instanceof Clob) {
			try (Reader in = ((Clob) obj).getCharacterStream()) {
				return LobTypeDetector.detectText(readHeadText(in, maxHeadBytes, context));
			}
		}
		if (obj instanceof SQLXML) {
			try (Reader in = ((SQLXML) obj).getCharacterStream()) {
				return LobTypeDetector.detectText(readHeadText(in, maxHeadBytes, context));
			}
		}
		if (obj instanceof byte[]) {
			byte[] bytes = (byte[]) obj;
			byte[] head = bytes.length <= maxHeadBytes ? bytes : java.util.Arrays.copyOf(bytes, maxHeadBytes);
			return LobTypeDetector.detect(head, bytes.length);
		}
		return LobTypeDetector.detectText(String.valueOf(obj));
	}

	private static byte[] readHead(InputStream in, int maxHeadBytes, Object context) throws IOException {
		byte[] buffer = new byte[Math.max(1, maxHeadBytes)];
		int off = 0;
		int n;
		while (off < buffer.length && (n = in.read(buffer, off, buffer.length - off)) != -1) {
			CancellationHandler.checkForCancellation(context);
			off += n;
		}
		return off == buffer.length ? buffer : java.util.Arrays.copyOf(buffer, off);
	}

	private static String readHeadText(Reader in, int maxHeadChars, Object context) throws IOException {
		char[] buffer = new char[Math.max(1, maxHeadChars)];
		int off = 0;
		int n;
		while (off < buffer.length && (n = in.read(buffer, off, buffer.length - off)) != -1) {
			CancellationHandler.checkForCancellation(context);
			off += n;
		}
		return new String(buffer, 0, off);
	}

	private static LobContent readBinary(InputStream in, Object context, String table, String column) throws IOException {
		ByteArrayOutputStream buffer = new ByteArrayOutputStream();
		File tempFile = null;
		OutputStream fileOut = null;
		byte[] chunk = new byte[BUFFER];
		long total = 0;
		try {
			int n;
			while ((n = in.read(chunk)) != -1) {
				CancellationHandler.checkForCancellation(context);
				total += n;
				if (fileOut == null) {
					buffer.write(chunk, 0, n);
					if (buffer.size() > INMEMORY_THRESHOLD) {
						tempFile = Configuration.getInstance().createTempFile();
						tempFile.deleteOnExit();
						fileOut = new BufferedOutputStream(new FileOutputStream(tempFile));
						buffer.writeTo(fileOut);
					}
				} else {
					fileOut.write(chunk, 0, n);
				}
			}
		} catch (RuntimeException | IOException e) {
			closeQuietly(fileOut);
			deleteQuietly(tempFile);
			throw e;
		}
		if (fileOut != null) {
			fileOut.close();
			byte[] head = readHead(tempFile);
			return LobContent.binaryFile(tempFile, total, head, table, column, null);
		}
		return LobContent.binaryInMemory(buffer.toByteArray(), table, column, null);
	}

	private static LobContent readText(Reader in, Object context, String table, String column) throws IOException {
		StringBuilder buffer = new StringBuilder();
		File tempFile = null;
		Writer fileOut = null;
		String headText = null;
		char[] chunk = new char[BUFFER];
		long total = 0;
		try {
			int n;
			while ((n = in.read(chunk)) != -1) {
				CancellationHandler.checkForCancellation(context);
				total += n;
				if (fileOut == null) {
					buffer.append(chunk, 0, n);
					if (buffer.length() > INMEMORY_THRESHOLD) {
						tempFile = Configuration.getInstance().createTempFile();
						tempFile.deleteOnExit();
						fileOut = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(tempFile), StandardCharsets.UTF_8));
						fileOut.write(buffer.toString());
						headText = buffer.substring(0, Math.min(buffer.length(), LobContent.HEAD_SIZE));
					}
				} else {
					fileOut.write(chunk, 0, n);
				}
			}
		} catch (RuntimeException | IOException e) {
			closeQuietly(fileOut);
			deleteQuietly(tempFile);
			throw e;
		}
		if (fileOut != null) {
			fileOut.close();
			return LobContent.textFile(tempFile, total, headText, table, column, null);
		}
		return LobContent.textInMemory(buffer.toString(), table, column, null);
	}

	private static byte[] readHead(File file) throws IOException {
		byte[] head = new byte[LobContent.HEAD_SIZE];
		try (InputStream in = new FileInputStream(file)) {
			int off = 0;
			int n;
			while (off < head.length && (n = in.read(head, off, head.length - off)) != -1) {
				off += n;
			}
			if (off == head.length) {
				return head;
			}
			byte[] exact = new byte[off];
			System.arraycopy(head, 0, exact, 0, off);
			return exact;
		}
	}

	private static void closeQuietly(java.io.Closeable c) {
		if (c != null) {
			try {
				c.close();
			} catch (IOException e) {
				// ignore
			}
		}
	}

	private static void deleteQuietly(File f) {
		if (f != null) {
			try {
				f.delete();
			} catch (RuntimeException e) {
				// ignore
			}
		}
	}
}
