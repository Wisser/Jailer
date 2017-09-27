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
package net.sf.jailer.ui.databrowser.metadata;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;
import net.sf.jailer.util.Quoting;

/**
 * Meta Data Source.
 *
 * @author Ralf Wisser
 */
public class MetaDataSource {

	/**
	 * The database session.
	 */
	private final Session session;
	
	/**
	 * Name of the data source.
	 */
	final String dataSourceName;
	
	/**
	 * Schemas.
	 */
	private List<MDSchema> schemas = new ArrayList<MDSchema>();
	
	/**
	 * Constructor.
	 * 
	 * @param session the database session
	 * @param dataModel the data model
	 * @param dataSourceName name of the data source 
	 * @param executionContext the execution context
	 */
	public MetaDataSource(Session session, DataModel dataModel, String dataSourceName, ExecutionContext executionContext) {
		this.session = session;
		this.dataSourceName = dataSourceName;

		for (Table table: dataModel.getTables()) {
        	tablePerUnquotedName.put(Quoting.unquotedTableName(table, executionContext), table);
        	tablePerUnquotedNameUC.put(Quoting.unquotedTableName(table, executionContext).toUpperCase(Locale.ENGLISH), table);
        }
		readSchemas();
	}

	private void readSchemas() {
		List<String> sList = JDBCMetaDataBasedModelElementFinder.getSchemas(session, session.getSchema());
		String defaultSchema = JDBCMetaDataBasedModelElementFinder.getDefaultSchema(session, session.getSchema());
		for (String s: sList) {
			schemas.add(new MDSchema(s, s.equals(defaultSchema), this));
		}
	}

	ResultSet readTables(String schemaPattern) throws SQLException {
		return JDBCMetaDataBasedModelElementFinder.getTables(session, session.getMetaData(), schemaPattern, "%", new String[] { "TABLE" });
	}
	
	/**
	 * @return the schemas
	 */
	public List<MDSchema> getSchemas() {
		return schemas;
	}

	/**
	 * Gets the users default schema.
	 */
	public MDSchema getDefaultSchema() {
		for (MDSchema schema: getSchemas()) {
			if (schema.isDefaultSchema) {
				return schema;
			}
		}
		return null;
	}
	
	/**
	 * Removes all chached data.
	 */
	public void clear() {
		schemas.clear();
    	mDTableToTable.clear();
    	tableToMDTable.clear();
    	schemaPerUnquotedNameUC.clear();
		readSchemas();
	}

	/**
	 * Gets the session
	 * 
	 * @return the session
	 */
	public Session getSession() {
		return session;
	}

	private final Map<MDTable, Table> mDTableToTable = new HashMap<MDTable, Table>();
	private final Map<Table, MDTable> tableToMDTable = new HashMap<Table, MDTable>();
	private final Map<String, Table> tablePerUnquotedName = new HashMap<String, Table>();
	private final Map<String, Table> tablePerUnquotedNameUC = new HashMap<String, Table>();
	private final Map<String, MDSchema> schemaPerUnquotedNameUC = new HashMap<String, MDSchema>();

    public Table toTable(MDTable mdTable) {
    	if (mDTableToTable.containsKey(mdTable)) {
    		return mDTableToTable.get(mdTable);
    	}
    	Table table = null;
    	if (mdTable.getSchema().isDefaultSchema) {
    		table = tablePerUnquotedName.get(mdTable.getName());
    	}
    	if (table == null) {
    		table = tablePerUnquotedName.get(mdTable.getSchema().getName() + "." + mdTable.getName());
    	}
    	if (table == null) {
        	if (mdTable.getSchema().isDefaultSchema) {
        		table = tablePerUnquotedNameUC.get(mdTable.getName().toUpperCase(Locale.ENGLISH));
        	}
        	if (table == null) {
        		table = tablePerUnquotedNameUC.get((mdTable.getSchema().getName() + "." + mdTable.getName()).toUpperCase(Locale.ENGLISH));
        	}
    	}
    	if (table != null) {
	    	mDTableToTable.put(mdTable, table);
	    	tableToMDTable.put(table, mdTable);
    	}
    	return table;
    }

    public MDTable toMDTable(Table table) {
    	if (tableToMDTable.containsKey(table)) {
    		return tableToMDTable.get(table);
    	}
    	
    	MDSchema defaultSchema = getDefaultSchema();
    	MDTable mdTable = null;
    	if (defaultSchema != null) {
    		String schemaName = Quoting.staticUnquote(table.getSchema(defaultSchema.getName()));
    		String schemaNameUC = schemaName.toUpperCase(Locale.ENGLISH);
    		String tableName = Quoting.staticUnquote(table.getUnqualifiedName());
    		String tableNameUC = tableName.toUpperCase(Locale.ENGLISH);
    		
    		MDSchema schemaExact = null;
    		MDSchema schemaIC = null;
    		for (MDSchema schema: getSchemas()) {
    			if (schema.getName().equals(schemaName)) {
    				schemaExact = schema;
    				break;
    			}
    			if (schema.getName().toUpperCase(Locale.ENGLISH).equals(schemaNameUC)) {
    				schemaIC = schema;
    			}
    		}
    		List<MDTable> tables = null;
    		if (schemaExact != null) {
    			tables = schemaExact.getTables();
    		} else if (schemaIC != null) {
    			tables = schemaIC.getTables();
    		}
    		if (tables != null) {
    			MDTable mdTableExact = null;
    			MDTable mdTableIC = null;
    			for (MDTable mdT: tables) {
        			if (mdT.getName().equals(tableName)) {
        				mdTableExact = mdT;
        				break;
        			}
        			if (mdT.getName().toUpperCase(Locale.ENGLISH).equals(tableNameUC)) {
        				mdTableIC = mdT;
        			}
    			}
    			if (mdTableExact != null) {
    				mdTable = mdTableExact;
    			} else if (mdTableIC != null) {
    				mdTable = mdTableIC;
    			}
    		}
    	}
    	if (mdTable != null) {
    		mDTableToTable.put(mdTable, table);
    		tableToMDTable.put(table, mdTable);
    	}
    	return mdTable;
    }

    /**
     * Find schema by name.
     * 
     * @param schemaName schema name
     * @return schema by name
     */
	public synchronized MDSchema find(String schemaName) {
		if (schemaPerUnquotedNameUC.isEmpty()) {
			for (MDSchema schema: getSchemas()) {
				schemaPerUnquotedNameUC.put(Quoting.staticUnquote(schema.getName().toUpperCase(Locale.ENGLISH)), schema);
			}
		}
		return schemaPerUnquotedNameUC.get(Quoting.staticUnquote(schemaName.toUpperCase(Locale.ENGLISH)));
	}

}
