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
import java.util.ArrayList;
import java.util.List;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.ui.associationdiscovery.NamingRule.Kind;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.CsvFile.Line;

/**
 * The naming rules of a data model, together with the decision whether the built-in
 * naming conventions are used as well. <br>
 * Stored as "naming_rules.csv" in the folder of the data model, so that the rules travel
 * with the model. The path is built here and not in {@link DataModel}, because the rules
 * are used by the association discovery only - the subsetting engine never reads them.
 *
 * @author Ralf Wisser
 */
public class NamingRules {

	/**
	 * Name of the file inside the data model folder.
	 */
	private static final String FILE_NAME = "naming_rules.csv";

	/**
	 * Kind of the line that holds the "use built-in conventions" flag.
	 */
	private static final String BUILTIN = "BUILTIN";

	private static final String HEADER = "# Kind; Pattern; Complete by name; Without data check";

	private final List<NamingRule> rules = new ArrayList<NamingRule>();

	private boolean useBuiltInRules = true;

	private boolean isNew;

	/**
	 * Tells whether the data model had no rules file yet. Only then an editor prefills the
	 * list with the built-in conventions - whoever deletes them all is not to get them back
	 * unasked.
	 *
	 * @return <code>true</code> if there was no file
	 */
	public boolean isNew() {
		return isNew;
	}

	/**
	 * The built-in naming conventions, as far as they can be expressed as rules. Kept in the
	 * order of their specificity, and without the completion option, which is what the
	 * built-in matching does as well. <br>
	 * What is <b>not</b> in here, because it cannot be written as a rule, stays behind the
	 * "use the built-in naming conventions" flag: name equality, the tolerance towards the
	 * affixes of a foreign key column, and the primary key name without the table prefix.
	 *
	 * @return the rules
	 */
	public static List<NamingRule> builtInConventionRules() {
		String table = NamingRule.TABLE_PLACEHOLDER;
		String pk = NamingRule.PK_PLACEHOLDER;
		List<NamingRule> result = new ArrayList<NamingRule>();
		for (String pattern: new String[] {
				table + "_" + pk, table + pk,
				table + "_ID", table + "ID", table + "_KEY",
				table + "_NO", table + "NO", table + "_NR", table + "_CODE" }) {
			// completing a composite key by identical names is the usual shape; the built-in
			// conventions are heuristics though, so their matches have to be checked
			result.add(new NamingRule(Kind.TEMPLATE, pattern, true, false));
		}
		return result;
	}

	/**
	 * Gets the rules, in the order in which they are applied.
	 *
	 * @return the rules, modifiable
	 */
	public List<NamingRule> getRules() {
		return rules;
	}

	/**
	 * @return <code>true</code> if the built-in naming conventions are used as well
	 */
	public boolean isUseBuiltInRules() {
		return useBuiltInRules;
	}

	public void setUseBuiltInRules(boolean useBuiltInRules) {
		this.useBuiltInRules = useBuiltInRules;
	}

	/**
	 * Gets the templates of all rules.
	 *
	 * @return the rules of kind {@link Kind#TEMPLATE}
	 */
	public List<NamingRule> getTemplates() {
		return rulesOfKind(Kind.TEMPLATE);
	}

	/**
	 * Gets the regular expressions of all rules.
	 *
	 * @return the rules of kind {@link Kind#REGEX}
	 */
	public List<NamingRule> getRegexRules() {
		return rulesOfKind(Kind.REGEX);
	}

	private List<NamingRule> rulesOfKind(Kind kind) {
		List<NamingRule> result = new ArrayList<NamingRule>();
		for (NamingRule rule: rules) {
			if (rule.getKind() == kind) {
				result.add(rule);
			}
		}
		return result;
	}

	/**
	 * Gets the file holding the rules of the current data model.
	 *
	 * @param executionContext the execution context
	 * @return the file
	 */
	public static File getFile(ExecutionContext executionContext) {
		return new File(DataModel.getDatamodelFolder(executionContext) + File.separator + FILE_NAME);
	}

	/**
	 * Loads the rules of the current data model. A missing file means: no rules, built-in
	 * conventions active.
	 *
	 * @param executionContext the execution context
	 * @return the rules, never <code>null</code>
	 */
	public static NamingRules load(ExecutionContext executionContext) {
		NamingRules result = new NamingRules();
		File file = getFile(executionContext);
		if (!file.exists()) {
			result.isNew = true;
			return result;
		}
		try {
			for (Line line: new CsvFile(file).getLines()) {
				String kindName = line.cells.get(0).trim();
				String pattern = line.cells.get(1).trim();
				if (BUILTIN.equalsIgnoreCase(kindName)) {
					result.useBuiltInRules = !"false".equalsIgnoreCase(pattern);
				} else {
					Kind kind = Kind.parse(kindName);
					if (kind != null && pattern.length() > 0) {
						// a file written by an earlier version has no third and fourth cell
						boolean completeByName = "true".equalsIgnoreCase(line.cells.get(2).trim());
						boolean withoutDataCheck = "true".equalsIgnoreCase(line.cells.get(3).trim());
						result.rules.add(new NamingRule(kind, pattern, completeByName, withoutDataCheck));
					}
				}
			}
		} catch (IOException e) {
			// unreadable rules must not break the discovery
		}
		return result;
	}

	/**
	 * Stores the rules of the current data model.
	 *
	 * @param executionContext the execution context
	 */
	public void store(ExecutionContext executionContext) throws IOException {
		BufferedWriter out = new BufferedWriter(new FileWriter(getFile(executionContext)));
		try {
			out.append(HEADER);
			out.append("\n");
			out.append(BUILTIN + "; " + useBuiltInRules + "\n");
			for (NamingRule rule: rules) {
				out.append(CsvFile.encodeCell(rule.getKind().name()) + "; " + CsvFile.encodeCell(rule.getPattern())
						+ "; " + rule.isCompleteByName() + "; " + rule.isWithoutDataCheck() + "\n");
			}
		} finally {
			out.close();
		}
	}

	/**
	 * Creates a copy, so that an editor can work on it without touching the original.
	 *
	 * @return the copy
	 */
	public NamingRules copy() {
		NamingRules result = new NamingRules();
		result.useBuiltInRules = useBuiltInRules;
		result.isNew = isNew;
		for (NamingRule rule: rules) {
			result.rules.add(new NamingRule(rule.getKind(), rule.getPattern(), rule.isCompleteByName(), rule.isWithoutDataCheck()));
		}
		return result;
	}

}
