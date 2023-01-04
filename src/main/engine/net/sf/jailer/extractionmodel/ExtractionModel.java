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
package net.sf.jailer.extractionmodel;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.datamodel.AggregationSchema;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Filter;
import net.sf.jailer.datamodel.ParameterHandler;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.datamodel.filter_template.Clause;
import net.sf.jailer.datamodel.filter_template.Clause.Predicate;
import net.sf.jailer.datamodel.filter_template.Clause.Subject;
import net.sf.jailer.datamodel.filter_template.FilterTemplate;
import net.sf.jailer.modelbuilder.KnownIdentifierMap;
import net.sf.jailer.restrictionmodel.RestrictionModel;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.CsvFile.Line;
import net.sf.jailer.util.SqlUtil;

/**
 * Extraction-model, defines the subject and the {@link RestrictionModel}
 * of an extraction.
 * 
 * @author Ralf Wisser
 */
public class ExtractionModel {

	/**
	 * The logger.
	 */
	private static final Logger _log = LoggerFactory.getLogger(ExtractionModel.class);

	/**
	 * The table to read from.
	 */
	public final Table subject;

	/**
	 * Subject Limit Definition
	 */
	public SubjectLimitDefinition subjectLimitDefinition = new SubjectLimitDefinition(null, null);
	
	/**
	 * Additional Subject.
	 */
	public static class AdditionalSubject {
		
		private Table subject;
		
		/**
		 * Subject Limit Definition
		 */
		private SubjectLimitDefinition subjectLimitDefinition = new SubjectLimitDefinition(null, null);

		/**
		 * "Where" condition.
		 */
		private String condition;

		/**
		 * @return the subject
		 */
		public Table getSubject() {
			return subject;
		}
		/**
		 * @param subject the subject to set
		 */
		public void setSubject(Table subject) {
			this.subject = subject;
		}
		/**
		 * @return the condition
		 */
		public String getCondition() {
			return condition;
		}
		/**
		 * @param condition the condition to set
		 */
		public void setCondition(String condition) {
			this.condition = condition;
		}

		/**
		 * @return Subject Limit Definition
		 */
		public SubjectLimitDefinition getSubjectLimitDefinition() {
			return subjectLimitDefinition;
		}
		
		/**
		 * @param subjectLimitDefinition Subject Limit Definition
		 */
		public void setSubjectLimitDefinition(SubjectLimitDefinition subjectLimitDefinition) {
			this.subjectLimitDefinition = subjectLimitDefinition;
		}

		public AdditionalSubject(Table subject, String condition, SubjectLimitDefinition subjectLimitDefinition) {
			this.subject = subject;
			this.condition = condition;
			this.subjectLimitDefinition = subjectLimitDefinition;
		}
		
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + ((condition == null) ? 0 : condition.hashCode());
			result = prime * result + ((subject == null) ? 0 : subject.hashCode());
			result = prime * result + ((subjectLimitDefinition == null) ? 0 : subjectLimitDefinition.hashCode());
			return result;
		}
		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			AdditionalSubject other = (AdditionalSubject) obj;
			if (condition == null) {
				if (other.condition != null)
					return false;
			} else if (!condition.equals(other.condition))
				return false;
			if (subject == null) {
				if (other.subject != null)
					return false;
			} else if (!subject.equals(other.subject))
				return false;
			if (subjectLimitDefinition == null) {
				if (other.subjectLimitDefinition != null)
					return false;
			} else if (!subjectLimitDefinition.equals(other.subjectLimitDefinition))
				return false;
			return true;
		}
	}

	/**
	 * Additional Subjects.
	 */
	public List<AdditionalSubject> additionalSubjects = new ArrayList<AdditionalSubject>();
	
	/**
	 * The SQL-condition.
	 */
	private final String condition;

	/**
	 * The restricted data-model to be used for extraction.
	 */
	public final DataModel dataModel;
	
	/**
	 * Version.
	 */
	public final int[] version;
	
	/**
	 * Constructor for empty restriction models.
	 * 
	 * @param dataModel the data model to restrict
	 */
	public ExtractionModel(DataModel dataModel, ExecutionContext executionContext) {
		this.dataModel = dataModel;
		this.version = null;
		subject = dataModel.getTables().iterator().hasNext()? dataModel.getTables().iterator().next() : null;
		condition = "";
		dataModel.setRestrictionModel(new RestrictionModel(dataModel, executionContext));
		dataModel.deriveFilters();
	}

	/**
	 * Constructor.
	 *  
	 * @param fileName the name of the model-file
	 * @param parameters apply this parameter-value mapping to all restriction conditions, XML templates and filters 
	 */
	public ExtractionModel(String fileName, Map<String, String> sourceSchemaMapping, Map<String, String> parameters, ExecutionContext executionContext) throws IOException {
		this(new File(fileName).toURI().toURL(), sourceSchemaMapping, parameters, executionContext, false);
	}

	/**
	 * Constructor.
	 *  
	 * @param modelURL the URL of the model-file
	 * @param parameters apply this parameter-value mapping to all restriction conditions, XML templates and filters 
	 */
	public ExtractionModel(URL modelURL, Map<String, String> sourceSchemaMapping, Map<String, String> parameters, ExecutionContext executionContext, boolean failOnMissingSubject) throws IOException {
		String csvLocation = modelURL.toString();
		CsvFile csvFile = new CsvFile(modelURL.openStream(), CsvFile.ALL_BLOCKS, csvLocation, null);
		List<CsvFile.Line> csv = csvFile.getLines();
		if (csv.isEmpty()) {
			throw new RuntimeException(modelURL + "' is empty");
		}
		CsvFile.Line subjectLine = csv.get(0);
		String location = subjectLine.location;
		DataModel dataModel = new DataModel(sourceSchemaMapping, executionContext, true);
		Table subject = getTable(dataModel, SqlUtil.mappedSchema(sourceSchemaMapping, subjectLine.cells.get(0)));
		if (subject == null) {
			subjectTableName = subjectLine.cells.get(0);
			String message = location + ": unknown subject table " + subjectTableName;
			if (failOnMissingSubject) {
				throw new IncompatibleModelException(message);
			} else {
				_log.warn(message);
			}
		}
		String condition = subjectLine.cells.get(1);
		if ("".equals(condition)) {
			condition = SqlUtil.SQL_TRUE;
		}
		subjectLimitDefinition = createLimitDefinition(subjectLine.cells.get(2), subjectLine.cells.get(3));
		if (dataModel.getRestrictionModel() == null) {
			dataModel.setRestrictionModel(new RestrictionModel(dataModel, executionContext));
		}
		try {
			dataModel.getRestrictionModel().addRestrictionDefinition(csvFile, csvLocation, parameters);
		} catch (Exception e) {
			throw new RuntimeException(location, e);
		}
		this.subject = subject;
		this.condition = condition;
		this.dataModel = dataModel;

		// read xml mapping
		List<CsvFile.Line> xmlMapping = csvFile.getLines("xml-mapping");
		for (CsvFile.Line xmLine: xmlMapping) {
			location = subjectLine.location;
			String name = xmLine.cells.get(0);
			String tag = xmLine.cells.get(1);
			AggregationSchema aggregationSchema = AggregationSchema.valueOf(xmLine.cells.get(2));
			Association association = dataModel.namedAssociations.get(name);
			if (association == null) {
				_log.warn("unknown association '" + name + "'");
			} else {
				if (aggregationSchema != null) {
					association.setAggregationSchema(aggregationSchema);
				}
				association.setAggregationTagName(tag);
			}
		}

		// read upserts
		List<CsvFile.Line> upserts = csvFile.getLines("upserts");
		for (CsvFile.Line upsert: upserts) {
			location = subjectLine.location;
			String name = upsert.cells.get(0);
			String tag = upsert.cells.get(1);
			Table table = getTable(dataModel, name);
			if (table == null) {
				_log.warn("unknown table '" + name + "'");
			} else {
				table.upsert = Boolean.parseBoolean(tag);
			}
		}
		
		// read "exclude from deletion"
		List<CsvFile.Line> excludes = csvFile.getLines("exclude from deletion");
		for (CsvFile.Line excludesLine: excludes) {
			location = subjectLine.location;
			String name = excludesLine.cells.get(0);
			String tag = excludesLine.cells.get(1);
			Table table = getTable(dataModel, name);
			if (table == null) {
				_log.warn("unknown table '" + name + "'");
			} else {
				table.excludeFromDeletion = Boolean.parseBoolean(tag);
			}
		}
		
		// read export modus
		List<CsvFile.Line> exportModusFile = csvFile.getLines("export modus");
		Iterator<CsvFile.Line> i = exportModusFile.iterator();
		if (i.hasNext()) {
			dataModel.setExportModus(i.next().cells.get(0));
		}
		
		// read column mapping
		List<CsvFile.Line> columnMappingFile = csvFile.getLines("xml column mapping");
		for (CsvFile.Line xmLine: columnMappingFile) {
			String name = xmLine.cells.get(0);
			String mapping = xmLine.cells.get(1);
			Table table = getTable(dataModel, SqlUtil.mappedSchema(sourceSchemaMapping, name));
			if (table == null) {
				_log.warn("unknown table " + name);
			} else {
				table.setXmlTemplate(ParameterHandler.assignParameterValues(mapping, parameters));
			}
		}
		
		// read filters
		List<CsvFile.Line> filtersFile = csvFile.getLines("filters");
		for (CsvFile.Line xmLine: filtersFile) {
			String name = xmLine.cells.get(0);
			String column = xmLine.cells.get(1);
			String filter = xmLine.cells.get(2);
			Table table = getTable(dataModel, SqlUtil.mappedSchema(sourceSchemaMapping, name));
			if (table == null) {
				_log.warn("unknown table " + name);
			} else {
				Column col = null;
				for (Column c: table.getColumns()) {
					if (c.name.equals(column)) {
						col = c;
						break;
					}
				}
				if (col == null) {
					_log.warn("unknown table" + name + "." + column);
				} else {
					String type = xmLine.cells.get(4);
					if (type.trim().length() == 0) {
						type = null;
					}
					Filter theFilter = new Filter(ParameterHandler.assignParameterValues(filter, parameters), type, false, null);
					theFilter.setApplyAtExport(!"Import".equalsIgnoreCase(xmLine.cells.get(3)));
					
					col.setFilter(theFilter);
				}
			}
		}
		
		// read filter templates
		List<CsvFile.Line> templatesFile = csvFile.getLines("filter templates");
		int lineNr = 0;
		FilterTemplate template = null;
		while (lineNr < templatesFile.size()) {
			CsvFile.Line xmLine = templatesFile.get(lineNr);
			if (xmLine.cells.get(0).equals("T")) {
				template = new FilterTemplate();
				template.setName(xmLine.cells.get(1));
				template.setExpression(xmLine.cells.get(2));
				template.setEnabled("enabled".equals(xmLine.cells.get(3)));
				template.setApplyAtExport(!"Import".equalsIgnoreCase(xmLine.cells.get(4)));
				String type = xmLine.cells.get(5);
				if (type.trim().length() == 0) {
					type = null;
				}
				template.setType(type);
				dataModel.getFilterTemplates().add(template);
			} else if (xmLine.cells.get(0).equals("C") && template != null) {
				Clause clause = new Clause();
				clause.setSubject(Enum.valueOf(Subject.class, xmLine.cells.get(1)));
				clause.setPredicate(Enum.valueOf(Predicate.class, xmLine.cells.get(2)));
				clause.setObject(xmLine.cells.get(3));
				template.getClauses().add(clause);
			}
			++lineNr;
		}
		
		// read xml settings
		List<CsvFile.Line> xmlSettingsFile = csvFile.getLines("xml settings");
		i = xmlSettingsFile.iterator();
		if (i.hasNext()) {
			List<String> cells = i.next().cells;
			dataModel.getXmlSettings().datePattern = cells.get(0);
			dataModel.getXmlSettings().timestampPattern = cells.get(1);
			dataModel.getXmlSettings().rootTag = cells.get(2);
		}

		// read version
		List<CsvFile.Line> versionBlock = csvFile.getLines("version");
		if (!versionBlock.isEmpty()) {
			String vCell = versionBlock.get(0).cells.get(0);
			String[] versionLine = vCell.split("[^0-9]+");
			int[] v = new int[Math.max(4, versionLine.length)];
			int p = 0;
			for (String number: versionLine) {
				if (number.length() > 0) {
					try {
						v[p++] = Integer.parseInt(number);
					} catch (NumberFormatException e) {
						// version is unknown
						v = null;
						break;
					}
				}
			}
			version = v;
		} else {
			version = null;
		}
		
		// read additional subjects
		List<CsvFile.Line> additionalSubsLines = csvFile.getLines("additional subjects");
		for (CsvFile.Line line: additionalSubsLines) {
			Table additSubject = getTable(dataModel, SqlUtil.mappedSchema(sourceSchemaMapping, line.cells.get(0)));
			if (additSubject != null) {
				additionalSubjects.add(new AdditionalSubject(additSubject, line.cells.get(1), createLimitDefinition(line.cells.get(2), line.cells.get(3))));
			}
		}
		
		dataModel.deriveFilters();
		disableUnknownAssociations(csvFile.getLines("known"));
		
		if (version != null) {
			int v1 = version.length > 0? version[0] : 0;
			int v2 = version.length > 1? version[1] : 0;
			int v3 = version.length > 2? version[2] : 0;
			int v4 = version.length > 3? version[3] : 0;
			// < 9.5.4.6
			if (v1 < 9 || (v1 == 9 && (v2 < 5 || (v2 == 5 && (v3 < 4 || (v3 == 4 && (v4 < 6))))))) {
				migrateDisabledFKNullFilter();
			}
		}
	}

	private void migrateDisabledFKNullFilter() {
		for (Table table: dataModel.getTables()) {
			for (Association association: table.associations) {
				if (association.isInsertDestinationBeforeSource() && association.isIgnored()) {
					Map<Column, Column> sdMap = association.createSourceToDestinationKeyMapping();
					boolean hasNullFilter = true;
					for (Column c: sdMap.keySet()) {
						if (c.getFilter() == null || !"null".equals(c.getFilter().getExpression())) {
							hasNullFilter = false;
							break;
						}
					}
					if (hasNullFilter) {
						for (Column c: sdMap.keySet()) {
							c.setFilter(null);
						}
						association.setOrResetFKNullFilter(true);
					}
				}
			}
		}
	}

	private SubjectLimitDefinition createLimitDefinition(String limit, String orderBy) {
		Long lLimit = null;
		if (!"".equals(limit)) {
			try {
				lLimit = Long.parseLong(limit);
			} catch (NumberFormatException e) {
				lLimit = null;
			}
		}
		return new SubjectLimitDefinition(lLimit, orderBy.length() == 0? null : orderBy);
	}

	private KnownIdentifierMap knownIdentifierMap;
	
	private Table getTable(DataModel dataModel, String name) {
		Table table = dataModel.getTable(name);
		if (table == null) {
			if (knownIdentifierMap == null) {
				knownIdentifierMap = new KnownIdentifierMap();
				for (Table t: dataModel.getTables()) {
					knownIdentifierMap.putTableName(t.getName());
				}
			}
			String known = knownIdentifierMap.getTableName(name);
			if (known != null) {
				table = dataModel.getTable(known);
			}
		}
		return table;
	}

	private void disableUnknownAssociations(List<Line> lines) {
		Set<String> known = new HashSet<String>();
		dataModel.decisionPending.clear();
		for (Line line: lines) {
			known.add(line.cells.get(0));
			if ("pending".equalsIgnoreCase(line.cells.get(1))) {
				dataModel.decisionPending.add(line.cells.get(0));
			}
		}
		if (known.isEmpty()) {
			return;
		}
		for (Association a: dataModel.namedAssociations.values()) {
			String name = a.reversed? a.reversalAssociation.getName() : a.getName();
			if (!known.contains(name)) {
				if (a.isInsertSourceBeforeDestination()) {
					dataModel.getRestrictionModel().addRestriction(a, "false", "SYSTEM", true, new HashMap<String, String>());
				}
				dataModel.decisionPending.add(name);
			}
		}
	}

	public static String loadDatamodelFolder(String fileName, ExecutionContext executionContext) throws IOException {
		File csvFile = new File(fileName);
		if (!csvFile.exists()) {
			throw new FileNotFoundException("'" + fileName + "' does not exist");
		}
		List<CsvFile.Line> dmf = new CsvFile(csvFile, "datamodelfolder").getLines();
		if (dmf.size() > 0) {
			return dmf.get(0).cells.get(0);
		}
		List<CsvFile.Line> csv = new CsvFile(csvFile, "export modus").getLines();
		if (csv.isEmpty()) {
			throw new RuntimeException("'" + fileName + "' is not a valid Jailer extraction model");
		}
		return null;
	}

	/**
	 * @return the subject condition
	 */
	public String getCondition() {
		return condition;
	}

	private String subjectTableName;

	public String getSubjectTableName() {
		return subjectTableName;
	}
	
	public static class IncompatibleModelException extends RuntimeException {
		
		public IncompatibleModelException(String message) {
			super(message);
		}

		private static final long serialVersionUID = -4612042345601502761L;
		
	}
	
}
