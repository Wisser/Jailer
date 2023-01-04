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

package net.sf.jailer.datamodel;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.Session;
import net.sf.jailer.util.Quoting;


/**
 * Support for rowids.
 *
 * @author Ralf Wisser
 */
public class RowIdSupport {

	private final boolean useRowIds;
	private boolean useRowIdsOnlyForTablesWithoutPK;
	private final DataModel dataModel;
	private Column rowIdColumn;
	private PrimaryKey tablePK;
	private PrimaryKey uPK;

	/**
	 * Constructor.
	 *
	 * @param dataModel the data model
	 * @param configuration DBMS configuration
	 * @param rowIdType type of rowid-columns
	 * @param useRowIds <code>true</code> iff rowid column is used instead of primary keys
	 */
	public RowIdSupport(DataModel dataModel, DBMS configuration, String rowIdType, boolean useRowIds, boolean useRowIdsOnlyForTablesWithoutPK) {
		this.dataModel = dataModel;
		this.useRowIds = useRowIds || useRowIdsOnlyForTablesWithoutPK;
		this.useRowIdsOnlyForTablesWithoutPK = useRowIdsOnlyForTablesWithoutPK;
		if (this.useRowIds) {
			rowIdColumn = new Column(configuration.getRowidName(), rowIdType, 0, -1);
			tablePK = new PrimaryKey(Arrays.asList(rowIdColumn), true);
			List<Column> upkColumns = new ArrayList<Column>();
			upkColumns.add(new Column("PK", rowIdType, 0, -1));
			if (this.useRowIdsOnlyForTablesWithoutPK) {
				upkColumns.addAll(dataModel.getUniversalPrimaryKey().getColumns());
			}
			uPK = new PrimaryKey(upkColumns, true);
		}
	}

	/**
	 * Constructor.
	 *
	 * @param dataModel the data model
	 * @param configuration DBMS configuration
	 * @param useRowIds <code>true</code> iff rowid column is used instead of primary keys
	 */
	public RowIdSupport(DataModel dataModel, DBMS configuration, boolean useRowIds, boolean useRowIdsOnlyForTablesWithoutPK) {
		this(dataModel, configuration, configuration.getRowidType(), useRowIds && configuration.getRowidName() != null, useRowIdsOnlyForTablesWithoutPK);
	}

	/**
	 * Constructor.
	 *
	 * @param dataModel the data model
	 * @param rowIdType type of rowid-columns
	 * @param configuration DBMS configuration
	 */
	public RowIdSupport(DataModel dataModel, DBMS configuration, String rowIdType, ExecutionContext executionContext) {
		this(dataModel, configuration, rowIdType, initialUseRowId(configuration, executionContext), initialuseRowIdsOnlyForTablesWithoutPK(configuration, executionContext));
	}

	/**
	 * Constructor.
	 *
	 * @param dataModel the data model
	 * @param configuration DBMS configuration
	 */
	public RowIdSupport(DataModel dataModel, DBMS configuration, ExecutionContext executionContext) {
		this(dataModel, configuration, initialUseRowId(configuration, executionContext), initialuseRowIdsOnlyForTablesWithoutPK(configuration, executionContext));
	}

	private static boolean initialUseRowId(DBMS configuration, ExecutionContext executionContext) {
		return configuration.getRowidName() != null && executionContext.getUseRowid();
	}

	private static boolean initialuseRowIdsOnlyForTablesWithoutPK(DBMS configuration, ExecutionContext executionContext) {
		return configuration.getRowidName() != null && executionContext.getUseRowIdsOnlyForTablesWithoutPK();
	}

	/**
	 * Gets the primary key of a table.
	 *
	 * @param table the table
	 * @param exact 
	 * @return the primary key of the table
	 */
	public PrimaryKey getPrimaryKey(Table table, Session session, boolean exact) {
		if (table.isArtifical()) {
			return table.primaryKey;
		}
		if (table.primaryKey != null) {
			if (useRowIds && (!useRowIdsOnlyForTablesWithoutPK || table.primaryKey.getColumns().isEmpty())) {
				if (session == null || isRowIDApplicable(table, session, exact)) {
					return tablePK;
				}
			}
		}
		return table.primaryKey;
	}

	/**
	 * Gets the primary key of a table.
	 *
	 * @param table the table
	 * @return the primary key of the table
	 */
	public PrimaryKey getPrimaryKey(Table table, Session session) {
		return getPrimaryKey(table, session, true);
	}
	
	/**
	 * Gets the primary key of a table.
	 *
	 * @param table the table
	 * @return the primary key of the table
	 */
	public PrimaryKey getPrimaryKey(Table table) {
		return getPrimaryKey(table, null, true);
	}

	/**
	 * Gets the universal primary key.
	 *
	 * @param session for null value guessing
	 * @return the universal primary key
	 */
	public PrimaryKey getUniversalPrimaryKey(Session session) {
		if (useRowIds) {
			return uPK;
		}
		return dataModel.getUniversalPrimaryKey(session);
	}

	/**
	 * Gets the universal primary key.
	 *
	 * @return the universal primary key
	 */
	public PrimaryKey getUniversalPrimaryKey() {
		if (useRowIds) {
			return uPK;
		}
		return dataModel.getUniversalPrimaryKey();
	}

	/**
	 * @param useRowIdsOnlyForTablesWithoutPK the useRowIdsOnlyForTablesWithoutPK to set
	 */
	public void setUseRowIdsOnlyForTablesWithoutPK(
			boolean useRowIdsOnlyForTablesWithoutPK) {
		this.useRowIdsOnlyForTablesWithoutPK = useRowIdsOnlyForTablesWithoutPK;
	}

	/**
	 * Gets the columns with additional rowid-column of a table
	 *
	 * @param table the table
	 * @return the columns of the table
	 */
	public List<Column> getColumns(Table table, Session session) {
		return getColumns(table, session, true);
	}
	
	/**
	 * Gets the columns with additional rowid-column of a table
	 *
	 * @param table the table
	 * @param exact 
	 * @return the columns of the table
	 */
	public List<Column> getColumns(Table table, Session session, boolean exact) {
		List<Column> columns = table.getColumns();
		if (table.isArtifical()) {
			return columns;
		}
		if (table.primaryKey != null) {
			if (useRowIds && (!useRowIdsOnlyForTablesWithoutPK || table.primaryKey.getColumns().isEmpty())) {
				if (session == null || isRowIDApplicable(table, session, exact)) {
					columns = new ArrayList<Column>(columns);
					columns.addAll(tablePK.getColumns());
					return columns;
				}
			}
		}
		return columns;
	}

	public boolean isRowIdColumn(Column column) {
		return rowIdColumn != null && rowIdColumn.name.equals(column.name);
	}

	private boolean isRowIDApplicable(Table table, Session session, boolean exact) {
		if (table == null || table.getName() == null) {
			return false;
		}
		Boolean result = (Boolean) session.getSessionProperty(RowIdSupport.class, table.getName());
		if (result != null) {
			return result;
		}
		Quoting quoting;
		result = false;
		try {
			quoting = Quoting.getQuoting(session);
			String schema = table.getSchema("");
			String tableName;
			if (schema.length() == 0) {
				tableName = quoting.requote(table.getUnqualifiedName());
			} else {
				tableName = quoting.requote(schema) + "." + quoting.requote(table.getUnqualifiedName());
			}
			if (rowIdColumn != null) {
				if (!exact) {
					return true;
				}
				if (session.checkQuery("Select 1 from " + tableName + " Where 1=0")) {
					result = session.checkQuery("Select 1 from " + tableName + " Where 1=0 and " + rowIdColumn.name + "=" + rowIdColumn.name);
				} else {
					return false;
				}
			}
		} catch (SQLException e) {
			result = true;
		}

		session.setSessionProperty(RowIdSupport.class, table.getName(), result);
		return result;
	}

}
