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
package net.sf.jailer.restrictionmodel;

import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.ParameterHandler;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.SqlUtil;

/**
 * Restricts association-definitions in a {@link DataModel}.
 *
 * @author Ralf Wisser
 */
public class RestrictionModel {

	/**
	 * The data-model.
	 */
	private final DataModel dataModel;

	/**
	 * Restrictions (in SQL) for associations.
	 */
	private final Map<Association, String> restriction = new HashMap<Association, String>();

	/**
	 * The name of the restriction-files read.
	 */
	private List<String> filesRead = new ArrayList<String>();

	/**
	 * Whether the restriction-model is transposed.
	 */
	private boolean transposed = false;

	/**
	 * The logger.
	 */
	private static final Logger _log = LoggerFactory.getLogger(RestrictionModel.class);

	/**
	 * Constructor.
	 *
	 * @param dataModel the data-model
	 */
	public RestrictionModel(DataModel dataModel, ExecutionContext executionContext) {
		this.dataModel = dataModel;
	}

	/**
	 * Transposes the restriction-model.
	 */
	public void transpose() {
		transposed = !transposed;
		dataModel.version++;
	}

	/**
	 * Is the restriction-model transposed?
	 */
	public boolean isTransposed() {
		return transposed;
	}

	/**
	 * "ignore the association" - restriction.
	 */
	public static final String IGNORE = new String("ignore");

	/**
	 * Gets the restriction (in SQL) for an association.
	 *
	 * @param association the association
	 * @return the restriction.
	 *         <code>null</code> if association is not restricted.
	 *         {@link #IGNORE} if association must be ignored.
	 */
	public String getRestriction(Association association) {
		if (transposed) {
			association = association.reversalAssociation;
		}
		if (!restriction.containsKey(association)) {
			return null;
		}
		String rest = restriction.get(association);
		if (rest == null) {
			return IGNORE;
		}
		return rest;
	}

	/**
	 * Adds restrictions defined in a restriction-file.
	 *
	 * @param parameters apply this parameter-value mapping to all restriction conditions
	 */
	public void addRestrictionDefinition(URL extractionModelURL, Map<String, String> parameters) throws Exception {
		addRestrictionDefinition(new CsvFile(extractionModelURL.openStream(), null, extractionModelURL.toString(), null), extractionModelURL.toString(), parameters);
	}

	/**
	 * Adds restrictions defined in a restriction-file.
	 *
	 * @param parameters apply this parameter-value mapping to all restriction conditions
	 * @param fileName
	 */
	public void addRestrictionDefinition(CsvFile csvFile, String fileName, Map<String, String> parameters) throws Exception {
		dataModel.version++;

		List<CsvFile.Line> lines = csvFile.getLines();
		int nr = 0;
		for (CsvFile.Line line: lines) {
			++nr;
			if (nr == 1) {
				continue;
			}
			String location = line.location;
			if ("".equals(line.cells.get(1))) {
				Association association = dataModel.namedAssociations.get(line.cells.get(0));
				if (association == null) {
					_log.warn(location + ": unknown association '" + line.cells.get(0) + "'");
					continue;
				}
				String condition = line.cells.get(2);
				if ("".equals(condition)) {
					_log.warn(location + ": missing condition");
					continue;
				}
				addRestriction(association, condition, location, parameters);
				continue;
			}
			Table from;
			boolean reversFrom = false;
			if ("*".equals(line.cells.get(0))) {
				from = null;
			} else {
				if (line.cells.get(0).startsWith("*-")) {
					from = dataModel.getTable(line.cells.get(0).substring(2));
					reversFrom = true;
				} else {
					from = dataModel.getTable(line.cells.get(0));
				}
				if (from == null) {
					_log.warn(location + ": unknown table '" + line.cells.get(0) + "'");
					continue;
				}
			}
			Table to;
			List<Table> excludeTo = new ArrayList<Table>();
			if (from != null && "*".equals(line.cells.get(1))) {
				to = null;
			} else if (line.cells.get(1).startsWith("*-")) {
				to = null;
				for (String t: line.cells.get(1).substring(2).split(",")) {
					t = t.trim();
					Table excludeTable = dataModel.getTable(t);
					if (excludeTable == null) {
						_log.warn(location + ": unknown table '" + t + "'");
						continue;
					}
					excludeTo.add(excludeTable);
				}
			} else {
				to = dataModel.getTable(line.cells.get(1));
				if (to == null) {
					_log.warn(location + ": unknown table '" + line.cells.get(1) + "'");
					continue;
				}
			}
			String condition = line.cells.get(2);
			if ("".equals(condition)) {
				throw new RuntimeException(location + ": missing condition");
			}
			if (from == null || reversFrom) {
				for (Table table: dataModel.getTables()) {
					if (from != null && table.equals(from)) {
						continue;
					}
					for (Association a: table.associations) {
						if (a.destination.equals(to)) {
							if (!a.isInsertDestinationBeforeSource()) {
								addRestriction(a, condition, location, parameters);
							}
						}
					}
				}
			} else if (to == null) {
				for (Association a: from.associations) {
					if (!a.isInsertDestinationBeforeSource()) {
						if (!excludeTo.contains(a.destination)) {
							addRestriction(a, condition, location, parameters);
						}
					}
			}
			} else {
				for (Association a: from.associations) {
					if (a.destination.equals(to)) {
						addRestriction(a, condition, location, parameters);
					}
				}
			}
		}
		filesRead.add(fileName);
	}

	/**
	 * Adds a restriction to a association.
	 *
	 * @param association the association
	 * @param condition the restriction-condition
	 * @param parameters apply this parameter-value mapping to all restriction conditions
	 * @param location location in CSV-file
	 */
	public void addRestriction(Association association, String condition, String location, Map<String, String> parameters) {
		addRestriction(association, condition, location, false, parameters);
	}

	/**
	 * Adds a restriction to a association.
	 *
	 * @param association the association
	 * @param condition the restriction-condition
	 * @param location location in CSV-file
	 * @param parameters apply this parameter-value mapping to all restriction conditions
	 * @param removePreviousRestriction if <code>true</code>, remove any restriction on the association before adding the new one
	 */
	public void addRestriction(Association association, String condition, String location, boolean removePreviousRestriction, Map<String, String> parameters) {
		dataModel.version++;

		condition = ParameterHandler.assignParameterValues(condition, parameters);
		if ("ignore".equalsIgnoreCase(condition) || "false".equalsIgnoreCase(condition)) {
			condition = null;
		}
		if (condition != null && association.reversed) {
			condition = SqlUtil.reversRestrictionCondition(condition);
		}
		if (removePreviousRestriction && "".equals(condition)) {
			restriction.remove(association);
		} else if (removePreviousRestriction || !restriction.containsKey(association)) {
			restriction.put(association, condition == null? null : "(" + condition + ")");
		} else {
			String oldCondition = restriction.get(association);
			if (oldCondition == null || condition == null) {
				condition = null;
			} else {
				condition = oldCondition + " and (" + condition + ")";
			}
			restriction.put(association, condition);
		}
	}

	/**
	 * Stringifies the restriction model.
	 */
	@Override
	public String toString() {
		return filesRead.toString();
	}

}
