/*
 * Copyright 2007 - 2018 the original author or authors.
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.Session;


/**
 * Support for rowids.
 * 
 * @author Ralf Wisser
 */
public class RowIdSupport {

	private final boolean useRowIds;
	private boolean useRowIdsOnlyForTablesWithoutPK = false;
	private final DataModel dataModel;
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
	public RowIdSupport(DataModel dataModel, DBMS configuration, String rowIdType, boolean useRowIds) {
		this.dataModel = dataModel;
		this.useRowIds = useRowIds;
		if (useRowIds) {
			tablePK = new PrimaryKey(Arrays.asList(new Column(configuration.getRowidName(), rowIdType, 0, -1)));
			uPK = new PrimaryKey(Arrays.asList(new Column("PK", rowIdType, 0, -1)));
		}
	}
	
	/**
	 * Constructor.
	 * 
	 * @param dataModel the data model
	 * @param configuration DBMS configuration
	 * @param useRowIds <code>true</code> iff rowid column is used instead of primary keys
	 */
	public RowIdSupport(DataModel dataModel, DBMS configuration, boolean useRowIds) {
		this(dataModel, configuration, configuration.getRowidType(), useRowIds && configuration.getRowidName() != null);
	}

	/**
	 * Constructor.
	 * 
	 * @param dataModel the data model
	 * @param rowIdType type of rowid-columns
	 * @param configuration DBMS configuration
	 */
	public RowIdSupport(DataModel dataModel, DBMS configuration, String rowIdType, ExecutionContext executionContext) {
		this(dataModel, configuration, rowIdType, initialUseRowId(configuration, executionContext));
	}

	/**
	 * Constructor.
	 * 
	 * @param dataModel the data model
	 * @param configuration DBMS configuration
	 */
	public RowIdSupport(DataModel dataModel, DBMS configuration, ExecutionContext executionContext) {
		this(dataModel, configuration, initialUseRowId(configuration, executionContext));
	}
	
	private static boolean initialUseRowId(DBMS configuration, ExecutionContext executionContext) {
		return configuration.getRowidName() != null && !executionContext.getNoRowid();
	}
	
	/**
	 * Gets the primary key of a table.
	 * 
	 * @param table the table
	 * @return the primary key of the table
	 */
	public PrimaryKey getPrimaryKey(Table table) {
		if (table.primaryKey != null) {
			if (useRowIds && (!useRowIdsOnlyForTablesWithoutPK || table.primaryKey.getColumns().isEmpty())) {
				return tablePK;
			}
		}
		return table.primaryKey;
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
	public List<Column> getColumns(Table table) {
		List<Column> columns = table.getColumns();
		if (table.primaryKey != null) {
			if (useRowIds && (!useRowIdsOnlyForTablesWithoutPK || table.primaryKey.getColumns().isEmpty())) {
				columns = new ArrayList<Column>(columns);
				columns.addAll(tablePK.getColumns());
				return columns;
			}
		}
		return columns;
	}
		
}
