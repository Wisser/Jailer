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

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Deque;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import net.sf.jailer.util.LogUtil;

/**
 * Extracts a plain-text preview from a DOCX/XLSX/PPTX (Office Open XML)
 * package, without any third-party dependency, using the JDK's built-in StAX
 * parser. No formatting, images, or table/cell-position fidelity - text runs
 * and rough tab-separated rows only. Mirrors the role of
 * {@link LobContentSupport#unwrapSingleFileArchive} for single-file archives,
 * but for the (always multi-entry) Office packages.
 *
 * @author Ralf Wisser
 */
public final class OfficeTextExtractor {

	private OfficeTextExtractor() {
	}

	/**
	 * If the given ZIP content is a DOCX/XLSX/PPTX package, extracts its text
	 * and returns a copy of the content carrying the extracted text as its
	 * {@link LobContent#getPreviewText() preview} and the more specific
	 * {@link LobContentType}. The original bytes are left untouched, so
	 * "Save to file..."/"Open externally" still export the real Office file.
	 * Returns <code>null</code> when this is not a recognized Office package,
	 * the relevant parts exceed <code>maxBytes</code>, or extraction fails.
	 *
	 * @param zipContent the ZIP content (must have full content available)
	 * @param maxBytes   cap on the cumulative size of the (text-bearing) parts read
	 * @return the content with its preview text attached, or <code>null</code>
	 */
	public static LobContent extractPreview(LobContent zipContent, long maxBytes) {
		if (zipContent == null || zipContent.getType() != LobContentType.ZIP || !zipContent.hasFullContent()) {
			return null;
		}
		try {
			Map<String, byte[]> parts = readRelevantParts(zipContent.openBinaryStream(), maxBytes);
			if (parts == null) {
				return null;
			}
			if (parts.containsKey("word/document.xml")) {
				return relabel(zipContent, LobContentType.DOCX, extractRunText(parts.get("word/document.xml")));
			}
			if (parts.containsKey("xl/workbook.xml")) {
				return relabel(zipContent, LobContentType.XLSX, extractWorkbookText(parts));
			}
			if (parts.containsKey("ppt/presentation.xml")) {
				return relabel(zipContent, LobContentType.PPTX, extractPresentationText(parts));
			}
			return null;
		} catch (IOException e) {
			LogUtil.warn(e);
			return null;
		} catch (XMLStreamException e) {
			LogUtil.warn(e);
			return null;
		} catch (RuntimeException e) {
			LogUtil.warn(e);
			return null;
		}
	}

	private static LobContent relabel(LobContent zipContent, LobContentType type, String text) {
		if (text == null) {
			return null;
		}
		return zipContent.withType(type).withPreviewText(text).withOriginalFileName(zipContent.getOriginalFileName());
	}

	// --- ZIP reading ---------------------------------------------------------

	/**
	 * Reads the entries relevant to DOCX/XLSX/PPTX text extraction from the
	 * given stream into memory (name -&gt; bytes), skipping everything else
	 * (e.g. embedded media) without decompressing it. Returns <code>null</code>
	 * if the cumulative size of the kept entries exceeds <code>maxBytes</code>.
	 */
	private static Map<String, byte[]> readRelevantParts(InputStream in, long maxBytes) throws IOException {
		Map<String, byte[]> parts = new LinkedHashMap<String, byte[]>();
		long total = 0;
		try (ZipInputStream zis = new ZipInputStream(in)) {
			ZipEntry entry;
			while ((entry = zis.getNextEntry()) != null) {
				String name = entry.getName();
				if (entry.isDirectory() || !isRelevant(name)) {
					zis.closeEntry();
					continue;
				}
				byte[] bytes = LobContentSupport.readBounded(zis, maxBytes - total);
				zis.closeEntry();
				if (bytes == null) {
					return null; // over the cap
				}
				total += bytes.length;
				parts.put(name, bytes);
			}
		}
		return parts;
	}

	private static boolean isRelevant(String name) {
		if ("word/document.xml".equals(name)
				|| "xl/workbook.xml".equals(name)
				|| "xl/_rels/workbook.xml.rels".equals(name)
				|| "xl/sharedStrings.xml".equals(name)
				|| "ppt/presentation.xml".equals(name)) {
			return true;
		}
		if (name.startsWith("xl/worksheets/") && name.endsWith(".xml")) {
			return true;
		}
		if (name.startsWith("ppt/slides/") && name.endsWith(".xml") && !name.contains("_rels")) {
			return true;
		}
		return false;
	}

	private static XMLStreamReader newReader(byte[] xml) throws XMLStreamException {
		XMLInputFactory f = XMLInputFactory.newInstance();
		f.setProperty(XMLInputFactory.SUPPORT_DTD, Boolean.FALSE);
		try {
			f.setProperty(XMLInputFactory.IS_SUPPORTING_EXTERNAL_ENTITIES, Boolean.FALSE);
		} catch (IllegalArgumentException e) {
			// not supported by this StAX implementation - DTDs are already disabled above
		}
		return f.createXMLStreamReader(new ByteArrayInputStream(xml));
	}

	// --- DOCX / PPTX: generic text-run extraction -----------------------------

	/**
	 * Extracts text from the runs (elements with local name {@code t}) of a
	 * WordprocessingML or DrawingML XML part, inserting a newline after each
	 * paragraph (local name {@code p}). Both formats happen to use the same
	 * local names for text runs and paragraphs, so one implementation covers
	 * both.
	 */
	private static String extractRunText(byte[] xml) throws XMLStreamException {
		XMLStreamReader r = newReader(xml);
		try {
			StringBuilder sb = new StringBuilder();
			Deque<String> stack = new ArrayDeque<String>();
			while (r.hasNext()) {
				int ev = r.next();
				if (ev == XMLStreamConstants.START_ELEMENT) {
					stack.push(r.getLocalName());
				} else if (ev == XMLStreamConstants.END_ELEMENT) {
					String name = stack.isEmpty() ? null : stack.pop();
					if ("p".equals(name)) {
						sb.append('\n');
					}
				} else if (ev == XMLStreamConstants.CHARACTERS || ev == XMLStreamConstants.CDATA) {
					if (!stack.isEmpty() && "t".equals(stack.peek())) {
						sb.append(r.getText());
					}
				}
			}
			return sb.toString();
		} finally {
			try {
				r.close();
			} catch (XMLStreamException e) {
				// ignore - we are done reading
			}
		}
	}

	// --- PPTX: slides in numeric order -----------------------------------------

	private static final Pattern SLIDE_NUMBER = Pattern.compile("ppt/slides/slide(\\d+)\\.xml");

	private static String extractPresentationText(Map<String, byte[]> parts) throws XMLStreamException {
		List<String> slideNames = new ArrayList<String>();
		for (String name : parts.keySet()) {
			if (SLIDE_NUMBER.matcher(name).matches()) {
				slideNames.add(name);
			}
		}
		Collections.sort(slideNames, new Comparator<String>() {
			@Override
			public int compare(String a, String b) {
				return Integer.compare(slideNumber(a), slideNumber(b));
			}
		});
		StringBuilder sb = new StringBuilder();
		int i = 0;
		for (String name : slideNames) {
			++i;
			if (sb.length() > 0) {
				sb.append("\n\n");
			}
			sb.append("=== Slide ").append(i).append(" ===\n");
			sb.append(extractRunText(parts.get(name)));
		}
		return sb.length() == 0 ? null : sb.toString();
	}

	private static int slideNumber(String name) {
		Matcher m = SLIDE_NUMBER.matcher(name);
		return m.matches() ? Integer.parseInt(m.group(1)) : Integer.MAX_VALUE;
	}

	// --- XLSX: sheets/rows/cells -----------------------------------------------

	private static String extractWorkbookText(Map<String, byte[]> parts) throws XMLStreamException {
		List<String> sharedStrings = parts.containsKey("xl/sharedStrings.xml")
				? readSharedStrings(parts.get("xl/sharedStrings.xml")) : Collections.<String>emptyList();
		Map<String, String> relIdToTarget = parts.containsKey("xl/_rels/workbook.xml.rels")
				? readWorkbookRels(parts.get("xl/_rels/workbook.xml.rels")) : Collections.<String, String>emptyMap();
		List<String[]> sheets = readSheetList(parts.get("xl/workbook.xml")); // {name, r:id}

		StringBuilder sb = new StringBuilder();
		for (String[] sheet : sheets) {
			String target = relIdToTarget.get(sheet[1]);
			if (target == null) {
				continue;
			}
			byte[] sheetXml = parts.get(resolveWorkbookRelativePath(target));
			if (sheetXml == null) {
				continue;
			}
			if (sb.length() > 0) {
				sb.append("\n\n");
			}
			sb.append("=== ").append(sheet[0]).append(" ===\n");
			sb.append(extractSheetText(sheetXml, sharedStrings));
		}
		return sb.length() == 0 ? null : sb.toString();
	}

	private static String resolveWorkbookRelativePath(String target) {
		if (target.startsWith("/")) {
			return target.substring(1);
		}
		return "xl/" + target;
	}

	private static List<String> readSharedStrings(byte[] xml) throws XMLStreamException {
		XMLStreamReader r = newReader(xml);
		try {
			List<String> result = new ArrayList<String>();
			Deque<String> stack = new ArrayDeque<String>();
			StringBuilder current = null;
			while (r.hasNext()) {
				int ev = r.next();
				if (ev == XMLStreamConstants.START_ELEMENT) {
					String ln = r.getLocalName();
					stack.push(ln);
					if ("si".equals(ln)) {
						current = new StringBuilder();
					}
				} else if (ev == XMLStreamConstants.END_ELEMENT) {
					String ln = stack.isEmpty() ? null : stack.pop();
					if ("si".equals(ln)) {
						result.add(current == null ? "" : current.toString());
						current = null;
					}
				} else if (ev == XMLStreamConstants.CHARACTERS || ev == XMLStreamConstants.CDATA) {
					if (current != null && !stack.isEmpty() && "t".equals(stack.peek())) {
						current.append(r.getText());
					}
				}
			}
			return result;
		} finally {
			closeQuietly(r);
		}
	}

	private static Map<String, String> readWorkbookRels(byte[] xml) throws XMLStreamException {
		XMLStreamReader r = newReader(xml);
		try {
			Map<String, String> result = new LinkedHashMap<String, String>();
			while (r.hasNext()) {
				int ev = r.next();
				if (ev == XMLStreamConstants.START_ELEMENT && "Relationship".equals(r.getLocalName())) {
					String id = r.getAttributeValue(null, "Id");
					String target = r.getAttributeValue(null, "Target");
					if (id != null && target != null) {
						result.put(id, target);
					}
				}
			}
			return result;
		} finally {
			closeQuietly(r);
		}
	}

	/** The relationships namespace of the {@code r:id} attribute on a {@code <sheet>} element. */
	private static final String REL_NS = "http://schemas.openxmlformats.org/officeDocument/2006/relationships";

	private static List<String[]> readSheetList(byte[] xml) throws XMLStreamException {
		XMLStreamReader r = newReader(xml);
		try {
			List<String[]> result = new ArrayList<String[]>();
			while (r.hasNext()) {
				int ev = r.next();
				if (ev == XMLStreamConstants.START_ELEMENT && "sheet".equals(r.getLocalName())) {
					String name = r.getAttributeValue(null, "name");
					String rid = r.getAttributeValue(REL_NS, "id");
					if (rid == null) {
						rid = r.getAttributeValue(null, "id"); // defensive fallback
					}
					if (name != null && rid != null) {
						result.add(new String[] { name, rid });
					}
				}
			}
			return result;
		} finally {
			closeQuietly(r);
		}
	}

	/**
	 * Extracts a worksheet as rough tab-separated rows: cells in document
	 * order, without reconstructing column positions from the {@code r="B2"}
	 * cell reference. Resolves shared-string ({@code t="s"}) and inline-string
	 * ({@code t="inlineStr"}) cells; other cells show their raw {@code <v>}
	 * (numbers, booleans, cached formula results). A formula's source
	 * ({@code <f>}) is intentionally not captured, only its cached value.
	 */
	private static String extractSheetText(byte[] xml, List<String> sharedStrings) throws XMLStreamException {
		XMLStreamReader r = newReader(xml);
		try {
			StringBuilder sb = new StringBuilder();
			StringBuilder rowBuf = new StringBuilder();
			StringBuilder cellBuf = null;
			String cellType = null;
			boolean firstCellInRow = true;
			Deque<String> stack = new ArrayDeque<String>();
			while (r.hasNext()) {
				int ev = r.next();
				if (ev == XMLStreamConstants.START_ELEMENT) {
					String ln = r.getLocalName();
					stack.push(ln);
					if ("row".equals(ln)) {
						rowBuf.setLength(0);
						firstCellInRow = true;
					} else if ("c".equals(ln)) {
						cellType = r.getAttributeValue(null, "t");
						cellBuf = new StringBuilder();
					}
				} else if (ev == XMLStreamConstants.CHARACTERS || ev == XMLStreamConstants.CDATA) {
					if (cellBuf != null && !stack.isEmpty() && ("v".equals(stack.peek()) || "t".equals(stack.peek()))) {
						cellBuf.append(r.getText());
					}
				} else if (ev == XMLStreamConstants.END_ELEMENT) {
					String ln = stack.isEmpty() ? null : stack.pop();
					if ("c".equals(ln) && cellBuf != null) {
						if (!firstCellInRow) {
							rowBuf.append('\t');
						}
						firstCellInRow = false;
						rowBuf.append(resolveCellValue(cellType, cellBuf.toString(), sharedStrings));
						cellBuf = null;
						cellType = null;
					} else if ("row".equals(ln)) {
						if (sb.length() > 0) {
							sb.append('\n');
						}
						sb.append(rowBuf);
					}
				}
			}
			return sb.toString();
		} finally {
			closeQuietly(r);
		}
	}

	private static String resolveCellValue(String cellType, String raw, List<String> sharedStrings) {
		if ("s".equals(cellType)) {
			try {
				int idx = Integer.parseInt(raw.trim());
				if (idx >= 0 && idx < sharedStrings.size()) {
					return sharedStrings.get(idx);
				}
			} catch (NumberFormatException e) {
				// fall through - show the raw value
			}
		}
		return raw;
	}

	private static void closeQuietly(XMLStreamReader r) {
		try {
			r.close();
		} catch (XMLStreamException e) {
			// ignore - we are done reading
		}
	}
}
