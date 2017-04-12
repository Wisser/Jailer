/*
 * Copyright 2007 - 2017 the original author or authors.
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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.sf.jailer.CommandLine;
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
import net.sf.jailer.restrictionmodel.RestrictionModel;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.CsvFile.Line;
import net.sf.jailer.util.SqlUtil;

import org.apache.log4j.Logger;

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
    private static final Logger _log = Logger.getLogger(ExtractionModel.class);
 
    /**
     * The table to read from.
     */
    public final Table subject;
    
    /**
     * Additional Subject.
     */
    public static class AdditionalSubject {
		private Table subject;
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
    	public AdditionalSubject(Table subject, String condition) {
    		this.subject = subject;
    		this.condition = condition;
    	}
    	/**
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result
					+ ((condition == null) ? 0 : condition.hashCode());
			result = prime * result
					+ ((subject == null) ? 0 : subject.hashCode());
			return result;
		}
		/**
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
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
    public String condition;
    
    /**
     * A limit for the number of subject-entities. (-1 for unlimited)
     */
    public final long limit;
    
    /**
     * The restricted data-model to be used for extraction.
     */
    public final DataModel dataModel;

	/**
	 * The command line arguments.
	 */
	private final CommandLine commandLine;

	/**
     * Constructor for empty restriction models.
     * 
     * @param dataModel the data model to restrict
     */
    public ExtractionModel(DataModel dataModel, CommandLine commandLine) throws Exception {
    	this.commandLine = commandLine;
    	this.dataModel = dataModel;
    	subject = dataModel.getTables().iterator().hasNext()? dataModel.getTables().iterator().next() : null;
        condition = "";
        dataModel.setRestrictionModel(new RestrictionModel(dataModel, commandLine));
        limit = -1;
        dataModel.deriveFilters();
    }

    /**
     * Constructor.
     *  
     * @param the name of the model-file
     * @param parameters apply this parameter-value mapping to all restriction conditions, XML templates and filters 
     */
    public ExtractionModel(String fileName, Map<String, String> sourceSchemaMapping, Map<String, String> parameters, CommandLine commandLine) throws Exception {
    	this.commandLine = commandLine;
    	List<CsvFile.Line> csv = new CsvFile(commandLine.newFile(fileName)).getLines();
        if (csv.isEmpty()) {
        	throw new RuntimeException("file '" + fileName + "' is empty");
        }
        CsvFile.Line subjectLine = csv.get(0);
        String location = subjectLine.location;
        DataModel dataModel = new DataModel(sourceSchemaMapping, commandLine);
        Table subject = dataModel.getTable(SqlUtil.mappedSchema(sourceSchemaMapping, subjectLine.cells.get(0)));
        if (subject == null) {
        	_log.warn(location + ": unknown table " + subjectLine.cells.get(0));
        }
        String condition = subjectLine.cells.get(1);
        if ("".equals(condition)) {
        	condition = "1=1";
        }
        long limit = 0;
        if (!"".equals(subjectLine.cells.get(2))) {
            try {
                limit = Long.parseLong(subjectLine.cells.get(2));
            } catch (NumberFormatException e) {
                throw new RuntimeException(location, e);
            }
        }
        for (int i = 3; subjectLine.cells.get(i).length() > 0; ++i) {
            if (dataModel.getRestrictionModel() == null) {
                dataModel.setRestrictionModel(new RestrictionModel(dataModel, commandLine));
            }
            try {
                dataModel.getRestrictionModel().addRestrictionDefinition(subjectLine.cells.get(i), fileName, parameters);
            } catch (Exception e) {
                throw new RuntimeException(location, e);
            }
        }
        this.subject = subject;
        this.condition = condition;
        this.limit = limit;
        this.dataModel = dataModel;

        // read xml mapping
        List<CsvFile.Line> xmlMapping = new CsvFile(commandLine.newFile(fileName), "xml-mapping").getLines();
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
        List<CsvFile.Line> upserts = new CsvFile(commandLine.newFile(fileName), "upserts").getLines();
        for (CsvFile.Line upsert: upserts) {
            location = subjectLine.location;
			String name = upsert.cells.get(0);
			String tag = upsert.cells.get(1);
            Table table = dataModel.getTable(name);
            if (table == null) {
            	_log.warn("unknown table '" + name + "'");
            } else {
            	table.upsert = Boolean.parseBoolean(tag);
            }
        }
        
        // read "exclude from deletion"
        List<CsvFile.Line> excludes = new CsvFile(commandLine.newFile(fileName), "exclude from deletion").getLines();
        for (CsvFile.Line excludesLine: excludes) {
            location = subjectLine.location;
			String name = excludesLine.cells.get(0);
			String tag = excludesLine.cells.get(1);
            Table table = dataModel.getTable(name);
            if (table == null) {
            	_log.warn("unknown table '" + name + "'");
            } else {
            	table.excludeFromDeletion = Boolean.parseBoolean(tag);
            }
        }
        
        // read export modus
        List<CsvFile.Line> exportModusFile = new CsvFile(commandLine.newFile(fileName), "export modus").getLines();
        Iterator<CsvFile.Line> i = exportModusFile.iterator();
        if (i.hasNext()) {
        	dataModel.setExportModus(i.next().cells.get(0));
        }
        
        // read column mapping
        List<CsvFile.Line> columnMappingFile = new CsvFile(commandLine.newFile(fileName), "xml column mapping").getLines();
        for (CsvFile.Line xmLine: columnMappingFile) {
        	String name = xmLine.cells.get(0);
			String mapping = xmLine.cells.get(1);
			Table table = dataModel.getTable(SqlUtil.mappedSchema(sourceSchemaMapping, name));
			if (table == null) {
				_log.warn("unknown table" + name);
			} else {
				table.setXmlTemplate(ParameterHandler.assignParameterValues(mapping, parameters));
			}
        }
        
        // read filters
        List<CsvFile.Line> filtersFile = new CsvFile(commandLine.newFile(fileName), "filters").getLines();
        for (CsvFile.Line xmLine: filtersFile) {
        	String name = xmLine.cells.get(0);
			String column = xmLine.cells.get(1);
			String filter = xmLine.cells.get(2);
			Table table = dataModel.getTable(SqlUtil.mappedSchema(sourceSchemaMapping, name));
			if (table == null) {
				_log.warn("unknown table" + name);
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
        List<CsvFile.Line> templatesFile = new CsvFile(commandLine.newFile(fileName), "filter templates").getLines();
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
        List<CsvFile.Line> xmlSettingsFile = new CsvFile(commandLine.newFile(fileName), "xml settings").getLines();
        i = xmlSettingsFile.iterator();
        if (i.hasNext()) {
        	List<String> cells = i.next().cells;
			dataModel.getXmlSettings().datePattern = cells.get(0);
			dataModel.getXmlSettings().timestampPattern = cells.get(1);
			dataModel.getXmlSettings().rootTag = cells.get(2);
        }

        // read version
        int[] version = null;
        List<CsvFile.Line> versionBlock = new CsvFile(commandLine.newFile(fileName), "version").getLines();
        if (!versionBlock.isEmpty()) {
        	String vCell = versionBlock.get(0).cells.get(0);
        	String[] versionLine = vCell.split("[^0-9]+");
        	version = new int[Math.max(4, versionLine.length)];
        	int p = 0;
        	for (String number: versionLine) {
        		if (number.length() > 0) {
        			try {
        				version[p++] = Integer.parseInt(number);
        			} catch (NumberFormatException e) {
        				// version is unknown
        				version = null;
        				break;
        			}
        		}
        	}
        }
        
        // read additional subjects
        List<CsvFile.Line> additionalSubsLines = new CsvFile(commandLine.newFile(fileName), "additional subjects").getLines();
        for (CsvFile.Line line: additionalSubsLines) {
        	Table additSubject = dataModel.getTable(SqlUtil.mappedSchema(sourceSchemaMapping, line.cells.get(0)));
            if (additSubject != null) {
            	additionalSubjects.add(new AdditionalSubject(additSubject, line.cells.get(1)));
            }
        }
        
        if (version != null) {
        	if (version[0] < 5 || version[0] == 5 && version[1] < 4) {
        		// convert pre 5.4 "initial data" tables into additional subjects
        		for (Table table: readInitialDataTables(sourceSchemaMapping, dataModel)) {
        			additionalSubjects.add(new AdditionalSubject(table, ""));
        		}
        	}
        }
        dataModel.deriveFilters();
        disableUnknownChildren(new CsvFile(commandLine.newFile(fileName), "known").getLines());
    }

	private void disableUnknownChildren(List<Line> lines) {
		Set<String> known = new HashSet<String>();
		for (Line line: lines) {
			known.add(line.cells.get(0));
		}
		if (known.isEmpty()) {
			return;
		}
		for (Association a: dataModel.namedAssociations.values()) {
			String name = a.reversed? a.reversalAssociation.getName() : a.getName();
			if (!known.contains(name) && a.isInsertSourceBeforeDestination()) {
				dataModel.getRestrictionModel().addRestriction(a.source, a, "false", "SYSTEM", true, new HashMap<String, String>());
			}
		}
	}

	/**
	 * <b>
	 * Initial-data-tables are no longer supported! This method is used to convert old (pre 5.4) extraction-models.
	 * </b>
	 * 
	 * Reads the initial-data-tables list. An initial-data-table is a table
	 * which will be exported completely if it is in closure from subject.
	 * 
	 * @return the initial-data-tables list
	 */
	private Set<Table> readInitialDataTables(Map<String, String> sourceSchemaMapping, DataModel datamodel) throws Exception {
		File file = commandLine.newFile(DataModel.getDatamodelFolder(commandLine) + File.separator + "initial_data_tables.csv");
		if (file.exists()) {
			Set<Table> idTables = SqlUtil.readTableList(new CsvFile(file), datamodel, sourceSchemaMapping);
			return idTables;
		} else {
			return new HashSet<Table>();
		}
	}

    public static String loadDatamodelFolder(String fileName, CommandLine commandLine) throws Exception {
        List<CsvFile.Line> dmf = new CsvFile(commandLine.newFile(fileName), "datamodelfolder").getLines();
        if (dmf.size() > 0) {
        	return dmf.get(0).cells.get(0);
        }
        return null;
    }
    
}
