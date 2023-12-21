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
package net.sf.jailer.ui.databrowser.metadata;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;
import net.sf.jailer.ui.SessionForUI;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlUtil;

/**
 * Meta Data Source.
 *
 * @author Ralf Wisser
 */
public class MetaDataSource {

	/**
	 * The logger.
	 */
	private static final Logger logger = LoggerFactory.getLogger(MetaDataDetailsPanel.class);

	/**
	 * The database session.
	 */
	private final Session session;

	/**
	 * For identifier quoting.
	 */
	private final Quoting quoting;

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
	public MetaDataSource(Session session, DataModel dataModel, String dataSourceName, ExecutionContext executionContext) throws SQLException {
		this.session = session;
		this.dataSourceName = dataSourceName;
		this.quoting = Quoting.getQuoting(session);

		initTableMapping(dataModel);
	}

	private void initTableMapping(DataModel dataModel) {
		for (Table table: dataModel.getTables()) {
        	tablePerUnquotedName.put(unquotedTableName(table), table);
        	tablePerUnquotedNameUC.put(unquotedTableName(table).toUpperCase(Locale.ENGLISH), table);
        }
	}

	/**
	 * Gets unquoted qualified table name.
	 *
	 * @param t
	 *            the table
	 * @return unquoted qualified name of t
	 */
	private String unquotedTableName(Table t) {
		String schema = t.getSchema("");
		if (schema.length() == 0) {
			return Quoting.staticUnquote(t.getUnqualifiedName());
		}
		return Quoting.staticUnquote(schema) + "." + Quoting.staticUnquote(t.getUnqualifiedName());
	}

	private AtomicBoolean initialized = new AtomicBoolean(false);

	public boolean isInitialized() {
		return initialized.get();
	}

	private synchronized void readSchemas() {
		if (initialized.get()) {
			return;
		}
		
		Object md = new Object();
		try {
			md = session.getMetaData();
		} catch (SQLException e) {
			logger.info("error", e);
		}
		synchronized (md) {
			List<String> sList = new ArrayList<String>();
			List<String> schemaList = (List<String>) session.getSessionProperty(SessionForUI.class, "schemas");
			if (schemaList == null) {
				schemaList = JDBCMetaDataBasedModelElementFinder.getSchemas(session, session.getSchema());
			}
			for (String schema: schemaList) {
				sList.add(quoting.quote(schema));
			}
			String defaultSchemaName = (String) session.getSessionProperty(SessionForUI.class, "defSchema");
			if (defaultSchemaName == null) {
				defaultSchemaName = JDBCMetaDataBasedModelElementFinder.getDefaultSchema(session, session.getSchema());
			}
			String defaultSchema = quoting.quote(defaultSchemaName);
			if (sList.isEmpty()) {
				schemas.add(new MDSchema(defaultSchema, true, this));
			} else {
				for (String s: sList) {
					schemas.add(new MDSchema(s, s.equals(defaultSchema), this));
				}
			}
		}
		initialized.set(true);
	}

	ResultSet readTables(String schemaPattern) throws SQLException {
		try {
			if (DBMS.MySQL.equals(session.dbms)) {
				return JDBCMetaDataBasedModelElementFinder.getTables(session, Quoting.staticUnquote(schemaPattern), "%", new String[] { "SYSTEM TABLE", "SYSTEM VIEW", "TABLE", "VIEW", "SYNONYM", "ALIAS" });
			}
			if (DBMS.MSSQL.equals(session.dbms)) {
				return JDBCMetaDataBasedModelElementFinder.getTables(session, Quoting.staticUnquote(schemaPattern), "%", new String[] { "SYSTEM TABLE", "TABLE", "VIEW", "SYNONYM", "ALIAS" });
			}
			if (DBMS.POSTGRESQL.equals(session.dbms)) {
				// TODO 2 make types-selection configurable
				return JDBCMetaDataBasedModelElementFinder.getTables(session, Quoting.staticUnquote(schemaPattern), "%", new String[] { "PARTITIONED TABLE", "FOREIGN TABLE", "MATERIALIZED VIEW", "TABLE", "VIEW", "SYSTEM VIEW", "SYNONYM", "ALIAS" });
			}
			return JDBCMetaDataBasedModelElementFinder.getTables(session, Quoting.staticUnquote(schemaPattern), "%", new String[] { "SYSTEM VIEW", "TABLE", "VIEW", "SYNONYM", "ALIAS" });
		} catch (Exception e) {
			if (!session.isDown()) {
				logger.info("error", e);
				try {
					return JDBCMetaDataBasedModelElementFinder.getTables(session, Quoting.staticUnquote(schemaPattern), "%", new String[] { "TABLE", "VIEW", "SYNONYM", "ALIAS" });
				} catch (Exception e2) {
					if (!session.isDown()) {
						return JDBCMetaDataBasedModelElementFinder.getTables(session, Quoting.staticUnquote(schemaPattern), "%", new String[] { "TABLE", "VIEW" });
					} else {
						throw e;
					}
				}
			} else {
				throw e;
			}
		}
	}

	/**
	 * @return the schemas
	 */
	public synchronized List<MDSchema> getSchemas() {
		readSchemas();
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
	public synchronized void clear() {
    	for (MDSchema mdSchema: schemas) {
    		mdSchema.setValid(false);
    	}
		schemas.clear();
    	mDTableToTable.clear();
    	tableToMDTable.clear();
    	schemaPerUnquotedNameUC.clear();
    	initialized.set(false);
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

	/**
	 * Gets the quoting.
	 *
	 * @return the quoting
	 */
	public Quoting getQuoting() {
		return quoting;
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
    		table = tablePerUnquotedName.get(mdTable.getUnquotedName());
    	}
    	if (table == null) {
    		table = tablePerUnquotedName.get(mdTable.getSchema().getUnquotedName() + "." + mdTable.getUnquotedName());
    	}
    	if (table == null) {
        	if (mdTable.getSchema().isDefaultSchema) {
        		table = tablePerUnquotedNameUC.get(mdTable.getUnquotedName().toUpperCase(Locale.ENGLISH));
        	}
        	if (table == null) {
        		table = tablePerUnquotedNameUC.get((mdTable.getSchema().getUnquotedName() + "." + mdTable.getUnquotedName()).toUpperCase(Locale.ENGLISH));
        	}
    	}
    	if (table != null) {
	    	mDTableToTable.put(mdTable, table);
	    	tableToMDTable.put(table, mdTable);
    	}
    	return table;
    }

    public MDSchema getSchemaOfTable(Table table) {
    	MDSchema defaultSchema = getDefaultSchema();
    	if (defaultSchema != null) {
    		String schemaName = Quoting.staticUnquote(table.getSchema(defaultSchema.getName()));
    		String schemaNameUC = schemaName.toUpperCase(Locale.ENGLISH);

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
    		if (schemaExact != null) {
    			return schemaExact;
    		} else if (schemaIC != null) {
    			return schemaIC;
    		}
    	}
    	return null;
    }

    public MDTable toMDTable(Table table) {
    	if (tableToMDTable.containsKey(table)) {
    		return tableToMDTable.get(table);
    	}
    	if (table.getName() == null) {
    		return null;
    	}

    	MDSchema defaultSchema = getDefaultSchema();
    	MDTable mdTable = null;
    	if (defaultSchema != null) {
    		String schemaName = Quoting.staticUnquote(table.getSchema(defaultSchema.getName()));
    		String schemaNameUC = schemaName.toUpperCase(Locale.ENGLISH);
    		String tableName = table.getName().trim();
    		int i = SqlUtil.indexOfDot(tableName);
    		if (i >= 0) {
    				tableName = tableName.substring(i + 1);
    			}
    		Quoting.staticUnquote(tableName);
    		String tableNameUC = tableName.toUpperCase(Locale.ENGLISH);

    		MDSchema schemaExact = null;
    		MDSchema schemaIC = null;
    		for (MDSchema schema: getSchemas()) {
    			String name = Quoting.staticUnquote(schema.getName());
				if (name.equals(schemaName)) {
    				schemaExact = schema;
    				break;
    			}
    			if (name.toUpperCase(Locale.ENGLISH).equals(schemaNameUC)) {
    				schemaIC = schema;
    			}
    		}
    		List<MDTable> tables = null;
    		if (schemaExact != null) {
    			if (schemaExact.isLoaded()) {
    				tables = schemaExact.getTables();
    			}
    		} else if (schemaIC != null) {
    			if (schemaIC.isLoaded()) {
    				tables = schemaIC.getTables();
    			}
    		}
    		if (tables != null) {
    			MDTable mdTableExact = null;
    			MDTable mdTableIC = null;
    			for (MDTable mdT: tables) {
        			String name = Quoting.staticUnquote(mdT.getName());
					if (name.equals(tableName)) {
        				mdTableExact = mdT;
        				break;
        			}
        			if (name.toUpperCase(Locale.ENGLISH).equals(tableNameUC)) {
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
    	if (mdTable != null && !table.isArtifical()) {
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
				schemaPerUnquotedNameUC.put(Quoting.normalizeIdentifier(schema.getName()), schema);
			}
		}
		return schemaPerUnquotedNameUC.get(Quoting.normalizeIdentifier(schemaName));
	}

}
