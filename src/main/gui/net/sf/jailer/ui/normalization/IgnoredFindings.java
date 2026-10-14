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
package net.sf.jailer.ui.normalization;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.CsvFile.Line;

/**
 * The findings of the normalization check that the user has decided to ignore. <br>
 * Stored as "ignored_findings.csv" in the folder of the data model, so that the decisions
 * travel with the model. A finding is identified by its table, its rule and its columns,
 * so that it is recognized again in a later check.
 *
 * @author Ralf Wisser
 */
public class IgnoredFindings {

	/**
	 * Name of the file inside the data model folder.
	 */
	private static final String FILE_NAME = "ignored_findings.csv";

	private static final String HEADER = "# Table; Rule; Columns";

	/**
	 * The lines of the file (table, rule, columns), per key.
	 */
	private final Map<String, String[]> ignored = new TreeMap<String, String[]>();

	/**
	 * Tells whether a finding is ignored.
	 *
	 * @param finding the finding
	 * @return <code>true</code> if it is ignored
	 */
	public boolean isIgnored(NormalizationFinding finding) {
		return ignored.containsKey(key(finding));
	}

	/**
	 * Ignores a finding or stops ignoring it.
	 *
	 * @param finding the finding
	 * @param ignore <code>true</code> to ignore it
	 */
	public void setIgnored(NormalizationFinding finding, boolean ignore) {
		String[] line = line(finding);
		if (ignore) {
			ignored.put(key(line), line);
		} else {
			ignored.remove(key(line));
		}
	}

	private static String[] line(NormalizationFinding finding) {
		List<String> columns = new ArrayList<String>();
		for (Column column: finding.columns) {
			columns.add(column.name);
		}
		// the order of the columns does not matter
		Collections.sort(columns);
		StringBuilder sb = new StringBuilder();
		for (String column: columns) {
			if (sb.length() > 0) {
				sb.append(", ");
			}
			sb.append(column);
		}
		return new String[] { finding.table.getName(), finding.rule, sb.toString() };
	}

	private static String key(NormalizationFinding finding) {
		return key(line(finding));
	}

	private static String key(String[] line) {
		return line[0] + "\t" + line[1] + "\t" + line[2];
	}

	/**
	 * Gets the file holding the ignored findings of the current data model.
	 *
	 * @param executionContext the execution context
	 * @return the file
	 */
	public static File getFile(ExecutionContext executionContext) {
		return new File(DataModel.getDatamodelFolder(executionContext) + File.separator + FILE_NAME);
	}

	/**
	 * Loads the ignored findings of the current data model. A missing file means: none.
	 *
	 * @param executionContext the execution context
	 * @return the ignored findings, never <code>null</code>
	 */
	public static IgnoredFindings load(ExecutionContext executionContext) {
		IgnoredFindings result = new IgnoredFindings();
		File file = getFile(executionContext);
		if (!file.exists()) {
			return result;
		}
		try {
			for (Line line: new CsvFile(file).getLines()) {
				String[] cells = new String[] { line.cells.get(0).trim(), line.cells.get(1).trim(), line.cells.get(2).trim() };
				if (cells[0].length() > 0 && cells[1].length() > 0) {
					result.ignored.put(key(cells), cells);
				}
			}
		} catch (IOException e) {
			// an unreadable file must not break the check
		}
		return result;
	}

	/**
	 * Stores the ignored findings of the current data model.
	 *
	 * @param executionContext the execution context
	 */
	public void store(ExecutionContext executionContext) throws IOException {
		BufferedWriter out = new BufferedWriter(new FileWriter(getFile(executionContext)));
		try {
			out.append(HEADER);
			out.append("\n");
			for (String[] line: ignored.values()) {
				out.append(CsvFile.encodeCell(line[0]) + "; " + CsvFile.encodeCell(line[1]) + "; " + CsvFile.encodeCell(line[2]) + "\n");
			}
		} finally {
			out.close();
		}
	}

}
