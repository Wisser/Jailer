/*
 * Copyright 2007 - 2023 Ralf Wisser.
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
package net.sf.jailer.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.nio.charset.CharsetEncoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parser for CSV-files.
 * 
 * @author Ralf Wisser
 */
public class CsvFile {

	/**
	 * A line in a CSV-file.
	 */
	public static class Line {
		
		/**
		 * Describes the position of the line in a file.
		 */
		public final String location;
		
		/**
		 * The cells.
		 */
		public final List<String> cells;
		
		/**
		 * Length of the line.
		 */
		public int length;
		
		/**
		 * Constructor.
		 * 
		 * @param location describes the position of the line in a file
		 * @param cells the cells
		 */
		public Line(String location, List<String> cells) {
			this.location = location;
			this.cells = cells;
			int num = 0;
			int l = 0;
			for (String s : cells) {
				++num;
				if (s != null && s.trim().length() > 0) {
					l = num;
				}
			}
			for (int i = 0; i < 32; ++i) {
				this.cells.add(""); // legacy
			}
			this.length = l;
		}

		/**
		 * Line as String.
		 */
		@Override
		public String toString() {
			int num = 0;
			int l = 0;
			for (String s : cells) {
				++num;
				if (s != null && s.trim().length() > 0) {
					l = num;
				}
			}
			StringBuffer sb = new StringBuffer();
			if (l >= cells.size()) {
				l = cells.size() - 1;
			}
			for (int i = 0; i <= l; ++i) {
				sb.append(encodeCell(cells.get(i)) + "; ");
			}
			return sb.toString();
		}
	}

	public static interface LineFilter {
		boolean accept(Line line);
	}
	
	/**
	 * List of lines.
	 */
	private List<Line> rows = new ArrayList<Line>();
	
	/**
	 * Indicates start of block inside a CSV file.
	 */
	public static String BLOCK_INDICATOR = "#! block ";

	/**
	 * Constructor.
	 * 
	 * @param csvFile the csv file
	 */
	public CsvFile(File csvFile) throws IOException {
		this(csvFile, null, null);
	}

	/**
	 * Constructor.
	 * 
	 * @param csvFile the csv file
	 */
	public CsvFile(File csvFile, LineFilter filter) throws IOException {
		this(csvFile, null, filter);
	}

	/**
	 * Constructor.
	 * 
	 * @param csvFile the csv file
	 * @param block the block to read, <code>null</code> to read default block
	 */
	public CsvFile(File csvFile, String block) throws IOException {
		this(csvFile, block, null);
	}
	
	/**
	 * Constructor.
	 * 
	 * @param csvFile the csv file
	 * @param block the block to read, <code>null</code> to read default block
	 */
	public CsvFile(File csvFile, String block, LineFilter filter) throws IOException {
		if (csvFile.exists()) {
			BufferedReader reader = new BufferedReader(new FileReader(csvFile));
			String line = null;
			int lineNr = 0;
			boolean inBlock = block == null;
			while ((line = reader.readLine()) != null) {
				++lineNr;
				if (line.trim().length() == 0) {
					continue;
				}
				if (line.trim().startsWith(BLOCK_INDICATOR)) {
					if (inBlock) {
						break;
					}
					String blockName = line.trim().substring(BLOCK_INDICATOR.length()).trim();
					inBlock = blockName.equals(block);
					continue;
				}
				if (line.trim().startsWith("#")) {
					continue;
				}
				if (!inBlock) {
					continue;
				}
				String[] col = decodeLine(line);
				List<String> row = new ArrayList<String>(col.length + 34);
				for (int i = 0; i < col.length; ++i) {
					String s = col[i];
					row.add(s.trim());
				}
				Line cvsLine = new Line(csvFile.getName() + ", line " + lineNr, row);
				if (filter == null || filter.accept(cvsLine)) {
					rows.add(cvsLine);
				}
			}
			reader.close();
		}
	}

	/**
	 * Special block-name for reading default block and cache all others.
	 * 
	 * @see CsvFile#getLines()
	 */
	public static final String ALL_BLOCKS = "all-blocks";

	private static final String DEFAULT_BLOCK = "default-block";
	private Map<String, List<Line>> blocks = null;

	/**
	 * Constructor.
	 * 
	 * @param in to read from
	 * @param block the block to read, <code>null</code> to read default block
	 */
	public CsvFile(InputStream in, String block, String location, LineFilter filter) throws IOException {
		if (in != null) {
			BufferedReader reader = new BufferedReader(new InputStreamReader(in));
			String line = null;
			int lineNr = 0;
			boolean all = ALL_BLOCKS.equals(block);
			if (all) {
				block = null;
				blocks = new HashMap<String, List<Line>>();
			}
			boolean inBlock = block == null;
			String blockName = null;
			while ((line = reader.readLine()) != null) {
				++lineNr;
				if (line.trim().length() == 0) {
					continue;
				}
				if (line.trim().startsWith(BLOCK_INDICATOR)) {
					if (all) {
						blocks.put(blockName == null? DEFAULT_BLOCK : blockName, rows);
						rows = new ArrayList<CsvFile.Line>();
					} else {
						if (inBlock) {
							break;
						}
					}
					blockName = line.trim().substring(BLOCK_INDICATOR.length()).trim();
					inBlock = blockName.equals(block);
					continue;
				}
				if (line.trim().startsWith("#")) {
					continue;
				}
				if (!inBlock && !all) {
					continue;
				}
				String[] col = decodeLine(line);
				List<String> row = new ArrayList<String>(col.length + 34);
				for (int i = 0; i < col.length; ++i) {
					String s = col[i];
					row.add(s.trim());
				}
				Line cvsLine = new Line(location + ", " + "line " + lineNr, row);
				if (filter == null || filter.accept(cvsLine)) {
					rows.add(cvsLine);
				}
			}
			if (all) {
				blocks.put(blockName == null? DEFAULT_BLOCK : blockName, rows);
				rows = new ArrayList<CsvFile.Line>();
			}
			in.close();
		}
	}

	/**
	 * Decodes and splits csv-line.
	 * 
	 * @param line the line to decode
	 * @return decoded and splitted line
	 */
	public static String[] decodeLine(String line) {
		String content = decodeUnencodableChars(line);
		List<String> cells = new ArrayList<String>(1000);
		StringBuilder sb = new StringBuilder(1000);
		boolean esc = false;
		int length = content.length();
		for (int i = 0; i < length; ++i) {
			char c = content.charAt(i);
			if (c == '\\') {
				if (esc) {
					esc = false;
				} else {
					esc = true;
					continue;
				}
			}
			if (!esc && c == ';') {
				cells.add(sb.toString());
				sb.setLength(0);
			} else {
				if (esc && c == 'n') {
					c = '\n';
				} else if (esc && c == 'r') {
					c = '\r';
				}
				sb.append(c);
			}
			esc = false;
		}
		cells.add(sb.toString());
		return cells.toArray(new String[cells.size()]);
	}

	/**
	 * Encodes and csv-cell.
	 * 
	 * @param cell the cell to encode
	 * @return encoded cell
	 */
	public static String encodeCell(String cell) {
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < cell.length(); ++i) {
			char c = cell.charAt(i);
			if (c == ';') {
				sb.append("\\;");
			} else if (c == '\\') {
				sb.append("\\\\");
			} else if (c == '\n') {
				sb.append("\\n");
			} else if (c == '\r') {
				sb.append("\\r");
			} else {
				sb.append(c);
			}
		}
		return encodeUnencodableChars(sb.toString());
	}

	/**
	 * Gets the list of lines.
	 * 
	 * @return list of lists of cell-contents
	 */
	public List<Line> getLines() {
		if (blocks != null) {
			return getLines(null);
		} else {
			return rows;
		}
	}

	/**
	 * Gets the list of lines of a cached block.
	 * 
	 * @return list of lists of cell-contents of given block
	 * 
	 * @see CsvFile#ALL_BLOCKS
	 */
	public List<Line> getLines(String block) {
		if (blocks == null) {
			throw new IllegalStateException();
		}
		List<Line> lines = blocks.get(block == null? DEFAULT_BLOCK : block);
		return lines != null? lines : new ArrayList<CsvFile.Line>();
	}

	/**
	 * Checks if a certain line can be found in this file.
	 * 
	 * @param line the line
	 * 
	 * @return <code>true</code> iff this file contains the line 
	 */
	public boolean contains(String[] line) {
		for (Line l: getLines()) {
			boolean differenceFound = false;
			int i = 0;
			for (String cell: line) {
				if (cell != null && !cell.equals(l.cells.get(i))) {
					differenceFound = true;
					break;
				}
				++i;
			}
			if (!differenceFound) {
				return true;
			}
		}
		return false;
	}

	private static CharsetEncoder DEFAULT_CHARSET_ENCODER = null;
	private static boolean DEFAULT_CHARSET_ENCODER_INITIALIZED = false;

	private static synchronized String encodeUnencodableChars(String content) {
		try {
			if (!DEFAULT_CHARSET_ENCODER_INITIALIZED) {
				DEFAULT_CHARSET_ENCODER_INITIALIZED = true;
				DEFAULT_CHARSET_ENCODER = Charset.defaultCharset().newEncoder();
			} else if (DEFAULT_CHARSET_ENCODER == null) {
				return content;
			}
			StringBuilder sb = null;
			int l = content.length();
			for (int i = 0; i < l; ++i) {
				char c = content.charAt(i);
				if (!DEFAULT_CHARSET_ENCODER.canEncode(c)) {
					if (sb == null) {
						sb = new StringBuilder(content.substring(0, i));
					}
					sb.append("{\\u" + Integer.toHexString((int) c) + "}");
				} else if (sb != null) {
					sb.append(c);
				}
			}
			if (sb != null) {
				return sb.toString();
			}
		} catch (Exception e) {
			// ignore
		}
		return content;
	}
	
	private static final Pattern UNCODABLE_PATTERN = Pattern.compile(Pattern.quote("{\\u") + "([0123456789abcdef]+)" + Pattern.quote("}"));

	private static String decodeUnencodableChars(String content) {
		if (content.indexOf('{') >= 0) {
			Matcher matcher = UNCODABLE_PATTERN.matcher(content);
			boolean result = matcher.find();
			if (result) {
				try {
					StringBuffer sb = new StringBuffer();
					do {
						matcher.appendReplacement(sb, String.valueOf((char) Integer.parseInt(matcher.group(1), 16)));
						result = matcher.find();
					} while (result);
					matcher.appendTail(sb);
					return sb.toString();
				} catch (Exception e) {
					// ignore
				}
			}
		}
		return content;
	}
}
