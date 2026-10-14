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
package net.sf.jailer.ui.associationdiscovery;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Map;
import java.util.TreeMap;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.CsvFile.Line;

/**
 * The association candidates that the user has rejected. A later discovery neither verifies
 * nor proposes them. <br>
 * Stored as "rejected_associations.csv" in the folder of the data model, so that the decisions
 * travel with the model. A candidate is identified by its {@link AssociationCandidate#key()}
 * (tables and columns); table A, table B and the condition are stored only for the reader of the file. <br>
 * Thread safe: the discovery reads it in the background while the user rejects candidates.
 *
 * @author Ralf Wisser
 */
public class RejectedCandidates {

	/**
	 * Name of the file inside the data model folder.
	 */
	private static final String FILE_NAME = "rejected_associations.csv";

	private static final String HEADER = "# Key; Table A; Table B; Condition";

	/**
	 * The lines of the file (key, table A, table B, condition), per key.
	 */
	private final Map<String, String[]> rejected = new TreeMap<String, String[]>();

	/**
	 * Tells whether a candidate is rejected.
	 *
	 * @param candidate the candidate
	 * @return <code>true</code> if it is rejected
	 */
	public synchronized boolean isRejected(AssociationCandidate candidate) {
		return rejected.containsKey(candidate.key());
	}

	/**
	 * Rejects a candidate or stops rejecting it.
	 *
	 * @param candidate the candidate
	 * @param reject <code>true</code> to reject it
	 */
	public synchronized void setRejected(AssociationCandidate candidate, boolean reject) {
		String key = candidate.key();
		if (reject) {
			rejected.put(key, new String[] { key, candidate.child.getName(), candidate.parent.getName(), candidate.getCondition() });
		} else {
			rejected.remove(key);
		}
	}

	/**
	 * Gets the file holding the rejected candidates of the current data model.
	 *
	 * @param executionContext the execution context
	 * @return the file
	 */
	public static File getFile(ExecutionContext executionContext) {
		return new File(DataModel.getDatamodelFolder(executionContext) + File.separator + FILE_NAME);
	}

	/**
	 * Loads the rejected candidates of the current data model. A missing file means: none.
	 *
	 * @param executionContext the execution context
	 * @return the rejected candidates, never <code>null</code>
	 */
	public static RejectedCandidates load(ExecutionContext executionContext) {
		RejectedCandidates result = new RejectedCandidates();
		File file = getFile(executionContext);
		if (!file.exists()) {
			return result;
		}
		try {
			for (Line line: new CsvFile(file).getLines()) {
				String key = line.cells.get(0).trim();
				if (key.length() > 0) {
					result.rejected.put(key, new String[] { key, line.cells.get(1).trim(), line.cells.get(2).trim(), line.cells.get(3).trim() });
				}
			}
		} catch (IOException e) {
			// an unreadable file must not break the discovery
		}
		return result;
	}

	/**
	 * Stores the rejected candidates of the current data model.
	 *
	 * @param executionContext the execution context
	 */
	public synchronized void store(ExecutionContext executionContext) throws IOException {
		BufferedWriter out = new BufferedWriter(new FileWriter(getFile(executionContext)));
		try {
			out.append(HEADER);
			out.append("\n");
			for (String[] line: rejected.values()) {
				out.append(CsvFile.encodeCell(line[0]) + "; " + CsvFile.encodeCell(line[1]) + "; "
						+ CsvFile.encodeCell(line[2]) + "; " + CsvFile.encodeCell(line[3]) + "\n");
			}
		} finally {
			out.close();
		}
	}

}
