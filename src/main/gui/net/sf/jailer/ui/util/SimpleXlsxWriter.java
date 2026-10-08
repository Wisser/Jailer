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
package net.sf.jailer.ui.util;

import java.io.Closeable;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * Writes a simple Excel workbook (.xlsx, Office Open XML), without any library.
 * Supports text, numbers and formulas (with cached value, optionally array formulas), normal or bold.
 * The sheets are written one after the other, row by row (streaming), in any order.
 *
 * @author Ralf Wisser
 */
public class SimpleXlsxWriter implements Closeable {

	/**
	 * A cell.
	 */
	public static class Cell {
		final String text;
		final Number number;
		final String formula;
		final Object cachedValue;
		final boolean arrayFormula;
		final boolean bold;
		final boolean percent;

		private Cell(String text, Number number, String formula, Object cachedValue, boolean arrayFormula, boolean bold) {
			this(text, number, formula, cachedValue, arrayFormula, bold, false);
		}

		private Cell(String text, Number number, String formula, Object cachedValue, boolean arrayFormula, boolean bold, boolean percent) {
			this.text = text;
			this.number = number;
			this.formula = formula;
			this.cachedValue = cachedValue;
			this.arrayFormula = arrayFormula;
			this.bold = bold;
			this.percent = percent;
		}

		/**
		 * Number cell, optionally formatted as percentage (the number is the ratio, e.g. 0.25 for 25 %).
		 */
		public static Cell number(Number number, boolean bold, boolean percent) {
			Cell cell = number(number, bold);
			return percent && cell.number != null ? new Cell(null, number, null, null, false, bold, true) : cell;
		}

		/**
		 * Formula cell, optionally formatted as percentage.
		 */
		public static Cell formula(String formula, Object cachedValue, boolean arrayFormula, boolean bold, boolean percent) {
			return new Cell(null, null, formula, cachedValue, arrayFormula, bold, percent);
		}

		/**
		 * Text cell.
		 */
		public static Cell text(String text, boolean bold) {
			return new Cell(text, null, null, null, false, bold);
		}

		/**
		 * Number cell (a NaN or infinite number becomes text).
		 */
		public static Cell number(Number number, boolean bold) {
			if (number instanceof Double || number instanceof Float) {
				double d = number.doubleValue();
				if (Double.isNaN(d) || Double.isInfinite(d)) {
					return text(String.valueOf(d), bold);
				}
			}
			return new Cell(null, number, null, null, false, bold);
		}

		/**
		 * Formula cell.
		 *
		 * @param formula the formula (without "="), with English function names and "," as separator
		 * @param cachedValue the value of the formula (shown by applications not recalculating on load), a number, a string or <code>null</code>
		 * @param arrayFormula whether it is an array formula
		 */
		public static Cell formula(String formula, Object cachedValue, boolean arrayFormula, boolean bold) {
			return new Cell(null, null, formula, cachedValue, arrayFormula, bold);
		}

		/**
		 * Empty cell.
		 */
		public static Cell empty(boolean bold) {
			return new Cell(null, null, null, null, false, bold);
		}
	}

	private final ZipOutputStream zip;
	private final Writer out;
	private final List<String> sheetNames;
	private int rowNumber;
	private boolean inSheet = false;

	/**
	 * Constructor.
	 *
	 * @param outputStream to write the workbook to
	 * @param sheetNames names of the sheets (in this order in the workbook)
	 */
	public SimpleXlsxWriter(OutputStream outputStream, List<String> sheetNames) {
		this.zip = new ZipOutputStream(outputStream);
		this.out = new OutputStreamWriter(zip, StandardCharsets.UTF_8);
		this.sheetNames = sheetNames;
	}

	/**
	 * Begins writing a sheet.
	 *
	 * @param sheetIndex index of the sheet (see constructor)
	 */
	public void beginSheet(int sheetIndex) throws IOException {
		beginSheet(sheetIndex, 0, 0, null);
	}

	/**
	 * Begins writing a sheet with an outline (groups of rows and columns that can be collapsed).
	 * The summary rows are below their groups, the summary columns right of them.
	 *
	 * @param sheetIndex index of the sheet (see constructor)
	 * @param outlineLevelRows the highest outline level of the rows (0: none)
	 * @param outlineLevelColumns the highest outline level of the columns (0: none)
	 * @param columnOutline the outline levels of the columns: { 0-based index of the column, level }, or <code>null</code>
	 */
	public void beginSheet(int sheetIndex, int outlineLevelRows, int outlineLevelColumns, List<int[]> columnOutline) throws IOException {
		if (inSheet) {
			endSheet();
		}
		currentSheet = sheetIndex;
		zip.putNextEntry(new ZipEntry("xl/worksheets/sheet" + (sheetIndex + 1) + ".xml"));
		out.write("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n");
		out.write("<worksheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\">");
		if (outlineLevelRows > 0 || outlineLevelColumns > 0) {
			out.write("<sheetPr><outlinePr summaryBelow=\"1\" summaryRight=\"1\"/></sheetPr>");
			out.write("<sheetFormatPr defaultRowHeight=\"15\"" + (outlineLevelRows > 0 ? " outlineLevelRow=\"" + outlineLevelRows + "\"" : "")
					+ (outlineLevelColumns > 0 ? " outlineLevelCol=\"" + outlineLevelColumns + "\"" : "") + "/>");
		}
		if (columnOutline != null && columnOutline.stream().anyMatch(c -> c[1] > 0)) {
			out.write("<cols>");
			for (int[] column: columnOutline) {
				if (column[1] > 0) {
					out.write("<col min=\"" + (column[0] + 1) + "\" max=\"" + (column[0] + 1) + "\" width=\"12\" customWidth=\"1\" outlineLevel=\"" + column[1] + "\"/>");
				}
			}
			out.write("</cols>");
		}
		out.write("<sheetData>");
		rowNumber = 0;
		autoFilter = null;
		inSheet = true;
	}

	/**
	 * Writes the next row of the current sheet.
	 *
	 * @param cells the cells, <code>null</code> elements are omitted
	 */
	public void row(List<Cell> cells) throws IOException {
		row(cells, 0, false, false);
	}

	/**
	 * Writes the next row of the current sheet.
	 *
	 * @param cells the cells, <code>null</code> elements are omitted
	 * @param outlineLevel outline level of the row (0: not in a group)
	 * @param hidden whether the row is hidden (e.g. in a collapsed group)
	 * @param collapsed whether the row is the summary row of a collapsed group
	 */
	public void row(List<Cell> cells, int outlineLevel, boolean hidden, boolean collapsed) throws IOException {
		++rowNumber;
		out.write("<row r=\"" + rowNumber + "\"" + (outlineLevel > 0 ? " outlineLevel=\"" + outlineLevel + "\"" : "")
				+ (hidden ? " hidden=\"1\"" : "") + (collapsed ? " collapsed=\"1\"" : "") + ">");
		for (int i = 0; i < cells.size(); i++) {
			Cell cell = cells.get(i);
			if (cell == null) {
				continue;
			}
			String ref = columnName(i) + rowNumber;
			int styleIndex = (cell.bold ? 1 : 0) + (cell.percent ? 2 : 0); // see styles.xml
			String style = styleIndex > 0 ? " s=\"" + styleIndex + "\"" : "";
			if (cell.formula != null) {
				boolean stringResult = cell.cachedValue != null && !(cell.cachedValue instanceof Number);
				out.write("<c r=\"" + ref + "\"" + style + (stringResult || cell.cachedValue == null ? " t=\"str\"" : "") + ">");
				out.write("<f" + (cell.arrayFormula ? " t=\"array\" ref=\"" + ref + "\"" : "") + ">" + escape(cell.formula) + "</f>");
				out.write("<v>" + (cell.cachedValue == null ? "" : cell.cachedValue instanceof Number ? toString((Number) cell.cachedValue) : escape(String.valueOf(cell.cachedValue))) + "</v>");
				out.write("</c>");
			} else if (cell.number != null) {
				out.write("<c r=\"" + ref + "\"" + style + "><v>" + toString(cell.number) + "</v></c>");
			} else if (cell.text != null) {
				out.write("<c r=\"" + ref + "\"" + style + " t=\"inlineStr\"><is><t xml:space=\"preserve\">" + escape(cell.text) + "</t></is></c>");
			} else if (cell.bold) {
				out.write("<c r=\"" + ref + "\"" + style + "/>");
			}
		}
		out.write("</row>");
	}

	/**
	 * Gets the number of the last row written to the current sheet (1-based).
	 */
	public int getRowNumber() {
		return rowNumber;
	}

	/**
	 * Ends writing the current sheet.
	 */
	public void endSheet() throws IOException {
		out.write("</sheetData>");
		if (autoFilter != null) {
			out.write("<autoFilter ref=\"" + autoFilter + "\"/>");
			filterRanges.put(currentSheet, autoFilter);
		}
		out.write("</worksheet>");
		out.flush();
		zip.closeEntry();
		inSheet = false;
	}

	private int currentSheet;
	private String autoFilter;
	private final Map<Integer, String> filterRanges = new TreeMap<>();

	/**
	 * Adds an AutoFilter (filter drop-downs in the header row) to the current sheet.
	 *
	 * @param firstRow 1-based number of the header row
	 * @param firstColumn 0-based index of the first column
	 * @param lastRow 1-based number of the last row
	 * @param lastColumn 0-based index of the last column
	 */
	public void setAutoFilter(int firstRow, int firstColumn, int lastRow, int lastColumn) {
		autoFilter = columnName(firstColumn) + firstRow + ":" + columnName(lastColumn) + Math.max(firstRow, lastRow);
	}

	/**
	 * Definition of a pivot table (see {@link #addPivotTable(int, int, String, List, List, List, List)}).
	 */
	private static class PivotTableDefinition {
		int pivotSheet;
		int dataSheet;
		String dataRange;
		List<String> fieldNames;
		List<Integer> rowFields;
		List<Integer> columnFields;
		List<String[]> dataFields;
	}

	private PivotTableDefinition pivotTable;

	/**
	 * Adds an Excel pivot table (only its definition, the application builds it when opening the workbook).
	 *
	 * @param pivotSheet index of the sheet to show the pivot table on (it starts in cell A3)
	 * @param dataSheet index of the sheet with the data (with a header row)
	 * @param dataRange range of the data, including the header row (e.g. "A1:D100")
	 * @param fieldNames names of the columns of the data (the header row, each name unique)
	 * @param rowFields indexes of the fields of the rows
	 * @param columnFields indexes of the fields of the columns
	 * @param dataFields the values: { name (unique, not the name of a field), index of the field, function ("sum", "count", "average", "min" or "max"),
	 *            optionally how to show it ("percentOfRow", "percentOfCol", "percentOfTotal" or <code>null</code> for the value) }
	 */
	public void addPivotTable(int pivotSheet, int dataSheet, String dataRange, List<String> fieldNames, List<Integer> rowFields, List<Integer> columnFields, List<String[]> dataFields) {
		pivotTable = new PivotTableDefinition();
		pivotTable.pivotSheet = pivotSheet;
		pivotTable.dataSheet = dataSheet;
		pivotTable.dataRange = dataRange;
		pivotTable.fieldNames = fieldNames;
		pivotTable.rowFields = rowFields;
		pivotTable.columnFields = columnFields;
		pivotTable.dataFields = dataFields;
	}

	private static final String MAIN_NS = "http://schemas.openxmlformats.org/spreadsheetml/2006/main";
	private static final String RELATIONSHIPS_NS = "http://schemas.openxmlformats.org/officeDocument/2006/relationships";
	private static final String PACKAGE_RELATIONSHIPS_NS = "http://schemas.openxmlformats.org/package/2006/relationships";

	private static String relationships(String type, String target) {
		return "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
				+ "<Relationships xmlns=\"" + PACKAGE_RELATIONSHIPS_NS + "\">"
				+ "<Relationship Id=\"rId1\" Type=\"" + RELATIONSHIPS_NS + "/" + type + "\" Target=\"" + target + "\"/>"
				+ "</Relationships>";
	}

	/**
	 * Writes the parts of the pivot table. The cache has no records, the application refreshes it when opening the workbook.
	 */
	private void writePivotTable() throws IOException {
		PivotTableDefinition p = pivotTable;
		StringBuilder cache = new StringBuilder();
		cache.append("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n");
		cache.append("<pivotCacheDefinition xmlns=\"" + MAIN_NS + "\" xmlns:r=\"" + RELATIONSHIPS_NS + "\" r:id=\"rId1\""
				+ " refreshOnLoad=\"1\" createdVersion=\"3\" refreshedVersion=\"3\" minRefreshableVersion=\"3\" recordCount=\"0\">");
		cache.append("<cacheSource type=\"worksheet\"><worksheetSource ref=\"" + escape(p.dataRange) + "\" sheet=\"" + escape(sheetNames.get(p.dataSheet)) + "\"/></cacheSource>");
		cache.append("<cacheFields count=\"" + p.fieldNames.size() + "\">");
		for (String name: p.fieldNames) {
			cache.append("<cacheField name=\"" + escape(name) + "\" numFmtId=\"0\"><sharedItems/></cacheField>");
		}
		cache.append("</cacheFields></pivotCacheDefinition>");
		writeEntry("xl/pivotCache/pivotCacheDefinition1.xml", cache.toString());
		writeEntry("xl/pivotCache/_rels/pivotCacheDefinition1.xml.rels", relationships("pivotCacheRecords", "pivotCacheRecords1.xml"));
		writeEntry("xl/pivotCache/pivotCacheRecords1.xml", "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
				+ "<pivotCacheRecords xmlns=\"" + MAIN_NS + "\" xmlns:r=\"" + RELATIONSHIPS_NS + "\" count=\"0\"/>");

		StringBuilder table = new StringBuilder();
		table.append("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n");
		table.append("<pivotTableDefinition xmlns=\"" + MAIN_NS + "\" name=\"PivotTable1\" cacheId=\"1\" dataCaption=\"Values\""
				+ " updatedVersion=\"3\" minRefreshableVersion=\"3\" createdVersion=\"3\" useAutoFormatting=\"1\" itemPrintTitles=\"1\""
				+ " indent=\"0\" outline=\"1\" outlineData=\"1\" multipleFieldFilters=\"0\">");
		table.append("<location ref=\"A3\" firstHeaderRow=\"1\" firstDataRow=\"1\" firstDataCol=\"1\"/>");
		table.append("<pivotFields count=\"" + p.fieldNames.size() + "\">");
		for (int i = 0; i < p.fieldNames.size(); i++) {
			final int field = i;
			boolean isData = p.dataFields.stream().anyMatch(d -> Integer.parseInt(d[1]) == field);
			String axis = p.rowFields.contains(i) ? "axisRow" : p.columnFields.contains(i) ? "axisCol" : null;
			table.append("<pivotField" + (axis != null ? " axis=\"" + axis + "\"" : "") + (isData ? " dataField=\"1\"" : "") + " showAll=\"0\"");
			if (axis != null) {
				table.append("><items count=\"1\"><item t=\"default\"/></items></pivotField>");
			} else {
				table.append("/>");
			}
		}
		table.append("</pivotFields>");
		table.append("<rowFields count=\"" + p.rowFields.size() + "\">");
		for (int f: p.rowFields) {
			table.append("<field x=\"" + f + "\"/>");
		}
		table.append("</rowFields>");
		List<Integer> columnFields = new ArrayList<>(p.columnFields);
		if (p.dataFields.size() > 1) {
			columnFields.add(-2); // the values
		}
		if (!columnFields.isEmpty()) {
			table.append("<colFields count=\"" + columnFields.size() + "\">");
			for (int f: columnFields) {
				table.append("<field x=\"" + f + "\"/>");
			}
			table.append("</colFields>");
		}
		table.append("<dataFields count=\"" + p.dataFields.size() + "\">");
		for (String[] d: p.dataFields) {
			String showDataAs = d.length > 3 && d[3] != null ? " showDataAs=\"" + d[3] + "\" numFmtId=\"10\"" : "";
			table.append("<dataField name=\"" + escape(d[0]) + "\" fld=\"" + d[1] + "\" subtotal=\"" + d[2] + "\"" + showDataAs + " baseField=\"0\" baseItem=\"0\"/>");
		}
		table.append("</dataFields>");
		table.append("<pivotTableStyleInfo name=\"PivotStyleLight16\" showRowHeaders=\"1\" showColHeaders=\"1\" showRowStripes=\"0\" showColStripes=\"0\" showLastColumn=\"1\"/>");
		table.append("</pivotTableDefinition>");
		writeEntry("xl/pivotTables/pivotTable1.xml", table.toString());
		writeEntry("xl/pivotTables/_rels/pivotTable1.xml.rels", relationships("pivotCacheDefinition", "../pivotCache/pivotCacheDefinition1.xml"));
		writeEntry("xl/worksheets/_rels/sheet" + (p.pivotSheet + 1) + ".xml.rels", relationships("pivotTable", "../pivotTables/pivotTable1.xml"));
	}

	/**
	 * Writes the other parts of the workbook and closes it.
	 */
	@Override
	public void close() throws IOException {
		if (inSheet) {
			endSheet();
		}
		StringBuilder contentTypes = new StringBuilder();
		contentTypes.append("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n");
		contentTypes.append("<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\">");
		contentTypes.append("<Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\"/>");
		contentTypes.append("<Default Extension=\"xml\" ContentType=\"application/xml\"/>");
		contentTypes.append("<Override PartName=\"/xl/workbook.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml\"/>");
		contentTypes.append("<Override PartName=\"/xl/styles.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml\"/>");
		for (int i = 0; i < sheetNames.size(); i++) {
			contentTypes.append("<Override PartName=\"/xl/worksheets/sheet" + (i + 1) + ".xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml\"/>");
		}
		if (pivotTable != null) {
			contentTypes.append("<Override PartName=\"/xl/pivotTables/pivotTable1.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.pivotTable+xml\"/>");
			contentTypes.append("<Override PartName=\"/xl/pivotCache/pivotCacheDefinition1.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.pivotCacheDefinition+xml\"/>");
			contentTypes.append("<Override PartName=\"/xl/pivotCache/pivotCacheRecords1.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.pivotCacheRecords+xml\"/>");
		}
		contentTypes.append("</Types>");
		writeEntry("[Content_Types].xml", contentTypes.toString());

		writeEntry("_rels/.rels", "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
				+ "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">"
				+ "<Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument\" Target=\"xl/workbook.xml\"/>"
				+ "</Relationships>");

		StringBuilder workbook = new StringBuilder();
		workbook.append("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n");
		workbook.append("<workbook xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\"><sheets>");
		StringBuilder workbookRels = new StringBuilder();
		workbookRels.append("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n");
		workbookRels.append("<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">");
		for (int i = 0; i < sheetNames.size(); i++) {
			workbook.append("<sheet name=\"" + escape(sheetNames.get(i)) + "\" sheetId=\"" + (i + 1) + "\" r:id=\"rId" + (i + 1) + "\"/>");
			workbookRels.append("<Relationship Id=\"rId" + (i + 1) + "\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet\" Target=\"worksheets/sheet" + (i + 1) + ".xml\"/>");
		}
		workbook.append("</sheets>");
		if (!filterRanges.isEmpty()) {
			// the ranges of the AutoFilters
			workbook.append("<definedNames>");
			for (Map.Entry<Integer, String> e: filterRanges.entrySet()) {
				String[] range = e.getValue().split(":");
				workbook.append("<definedName name=\"_xlnm._FilterDatabase\" localSheetId=\"" + e.getKey() + "\" hidden=\"1\">"
						+ escape("'" + sheetNames.get(e.getKey()).replace("'", "''") + "'!" + absolute(range[0]) + ":" + absolute(range[1])) + "</definedName>");
			}
			workbook.append("</definedNames>");
		}
		workbook.append("<calcPr fullCalcOnLoad=\"1\"/>");
		workbookRels.append("<Relationship Id=\"rId" + (sheetNames.size() + 1) + "\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles\" Target=\"styles.xml\"/>");
		if (pivotTable != null) {
			String id = "rId" + (sheetNames.size() + 2);
			workbook.append("<pivotCaches><pivotCache cacheId=\"1\" r:id=\"" + id + "\"/></pivotCaches>");
			workbookRels.append("<Relationship Id=\"" + id + "\" Type=\"" + RELATIONSHIPS_NS + "/pivotCacheDefinition\" Target=\"pivotCache/pivotCacheDefinition1.xml\"/>");
			writePivotTable();
		}
		workbook.append("</workbook>");
		workbookRels.append("</Relationships>");
		writeEntry("xl/workbook.xml", workbook.toString());
		writeEntry("xl/_rels/workbook.xml.rels", workbookRels.toString());

		writeEntry("xl/styles.xml", "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
				+ "<styleSheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\">"
				+ "<fonts count=\"2\"><font><sz val=\"11\"/><name val=\"Calibri\"/></font><font><b/><sz val=\"11\"/><name val=\"Calibri\"/></font></fonts>"
				+ "<fills count=\"2\"><fill><patternFill patternType=\"none\"/></fill><fill><patternFill patternType=\"gray125\"/></fill></fills>"
				+ "<borders count=\"1\"><border><left/><right/><top/><bottom/><diagonal/></border></borders>"
				+ "<cellStyleXfs count=\"1\"><xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" borderId=\"0\"/></cellStyleXfs>"
				// 0: normal, 1: bold, 2: percent, 3: bold percent (numFmtId 10 is the built-in format "0.00%")
				+ "<cellXfs count=\"4\"><xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" borderId=\"0\" xfId=\"0\"/><xf numFmtId=\"0\" fontId=\"1\" fillId=\"0\" borderId=\"0\" xfId=\"0\" applyFont=\"1\"/>"
				+ "<xf numFmtId=\"10\" fontId=\"0\" fillId=\"0\" borderId=\"0\" xfId=\"0\" applyNumberFormat=\"1\"/><xf numFmtId=\"10\" fontId=\"1\" fillId=\"0\" borderId=\"0\" xfId=\"0\" applyFont=\"1\" applyNumberFormat=\"1\"/></cellXfs>"
				+ "<cellStyles count=\"1\"><cellStyle name=\"Normal\" xfId=\"0\" builtinId=\"0\"/></cellStyles>"
				+ "</styleSheet>");
		out.flush();
		zip.close();
	}

	private void writeEntry(String name, String content) throws IOException {
		zip.putNextEntry(new ZipEntry(name));
		out.write(content);
		out.flush();
		zip.closeEntry();
	}

	/**
	 * Gets the name of a column ("A", "B", ..., "Z", "AA", ...).
	 *
	 * @param index 0-based index of the column
	 */
	public static String columnName(int index) {
		StringBuilder name = new StringBuilder();
		for (int i = index + 1; i > 0; i = (i - 1) / 26) {
			name.insert(0, (char) ('A' + (i - 1) % 26));
		}
		return name.toString();
	}

	/**
	 * Makes a cell reference absolute ("B3" to "$B$3").
	 */
	private static String absolute(String ref) {
		return ref.replaceFirst("^([A-Z]+)([0-9]+)$", "\\$$1\\$$2");
	}

	private static String toString(Number number) {
		if (number instanceof BigDecimal) {
			return ((BigDecimal) number).toPlainString();
		}
		if (number instanceof BigInteger || number instanceof Long || number instanceof Integer || number instanceof Short || number instanceof Byte) {
			return number.toString();
		}
		return BigDecimal.valueOf(number.doubleValue()).toPlainString();
	}

	private static String escape(String text) {
		StringBuilder sb = new StringBuilder(text.length());
		for (int i = 0; i < text.length(); i++) {
			char c = text.charAt(i);
			switch (c) {
				case '&': sb.append("&amp;"); break;
				case '<': sb.append("&lt;"); break;
				case '>': sb.append("&gt;"); break;
				case '"': sb.append("&quot;"); break;
				default:
					if (c < 0x20 && c != '\t' && c != '\n' && c != '\r') {
						sb.append(' '); // not allowed in XML 1.0
					} else {
						sb.append(c);
					}
			}
		}
		return sb.toString();
	}

}
