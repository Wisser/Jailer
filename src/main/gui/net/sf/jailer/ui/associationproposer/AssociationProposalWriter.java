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
package net.sf.jailer.ui.associationproposer;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.modelbuilder.ModelBuilder;
import net.sf.jailer.ui.DataModelEditor;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.Pair;
import net.sf.jailer.util.Quoting;

/**
 * Writes accepted association proposals into the model builder's associations file,
 * from where the {@link DataModelEditor} merges them into the data model. <br>
 * Used by all views that propose associations ("Analyze SQL Script", "Discover Associations").
 *
 * @author Ralf Wisser
 */
public class AssociationProposalWriter {

	/**
	 * A single accepted proposal.
	 */
	public static class Proposal {

		public final String fromName;
		public final String toName;
		public final String condition;

		/**
		 * @param fromName name of table "A"
		 * @param toName name of table "B"
		 * @param condition the join condition
		 */
		public Proposal(String fromName, String toName, String condition) {
			this.fromName = fromName;
			this.toName = toName;
			this.condition = condition;
		}
	}

	/**
	 * Writes the given proposals into the model builder's associations file.
	 * Proposals that are already known (i.e. an equivalent association exists in the
	 * data model) are silently skipped.
	 *
	 * @param proposals the accepted proposals
	 * @param dataModel the data model
	 * @param executionContext the execution context
	 * @return an array {number of written proposals + skipped ones, number of skipped ones}
	 */
	public static int[] write(List<Proposal> proposals, DataModel dataModel, ExecutionContext executionContext) throws IOException {
		List<String> names = new ArrayList<String>();
		for (Proposal proposal: proposals) {
			names.add("P_" + Quoting.staticUnquote(proposal.fromName) + "_" + Quoting.staticUnquote(proposal.toName));
		}
		Map<String, Integer> nameCount = new HashMap<String, Integer>();
		for (String name: names) {
			Integer count = nameCount.get(name);
			if (count == null) {
				count = 1;
			} else {
				++count;
			}
			nameCount.put(name, count);
		}
		for (int i = 0; i < names.size(); ++i) {
			String name = names.get(i);
			int n = 0;
			while (nameCount.get(name) != null && nameCount.get(name) > 1 || dataModel.namedAssociations.containsKey(name)) {
				name = names.get(i) + "_" + (++n);
			}
			if (n > 0) {
				nameCount.put(name, 2);
			}
			names.set(i, name);
		}

		int knownCount = 0;
		int allCount = 0;
		BufferedWriter out = new BufferedWriter(new FileWriter(ModelBuilder.getModelBuilderAssociationsFilename(executionContext)));
		try {
			out.append("\n");
			AssociationProposer ap = new AssociationProposer(dataModel);
			for (int i = 0; i < proposals.size(); ++i) {
				Proposal proposal = proposals.get(i);
				String condition = proposal.condition.replaceAll(" *\n", " ");
				Table from = dataModel.getTable(proposal.fromName);
				Table to = dataModel.getTable(proposal.toName);
				if (from != null && to != null) {
					++allCount;
					Association association = new Association(from, to, false, false, condition, dataModel, false, null);
					if (ap.addAssociation(names.get(i), new Pair<Table, Table>(from, to), association, true)) {
						out.append(
							CsvFile.encodeCell(String.valueOf(proposal.fromName)) + "; " +
							CsvFile.encodeCell(String.valueOf(proposal.toName)) + "; ; ; " +
							CsvFile.encodeCell(condition) + "; " + names.get(i) + "; " + DataModelEditor.DATA_MODEL_EDITOR_AUTHOR + "\n");
					} else {
						++knownCount;
					}
				}
			}
		} finally {
			out.close();
		}
		return new int[] { allCount, knownCount };
	}

	private AssociationProposalWriter() {
	}

}
