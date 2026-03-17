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
package net.sf.jailer.database;

import java.sql.SQLException;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.util.Quoting;

/**
 * Description of the DBMS's SQL dialect.
 * 
 * @author Ralf Wisser
 */
public class SQLDialect {

	/**
	 * Name of config table.
	 */
	public static final String CONFIG_TABLE_ = "JAILER_CONFIG";
	
	/**
	 * Name of temp table.
	 */
	public static final String TMP_TABLE_ = "JAILER_TMP";
	
	/**
	 * Name of dual table.
	 */
	public static final String DUAL_TABLE = "JAILER_DUAL";

	/**
	 * 'DELETE FROM T where PK IN (values ...)'
	 * or
	 * 'DELETE FROM T where PK IN (...)'
	 */
	private boolean needsValuesKeywordForDeletes = false;
	private boolean supportsInClauseForDeletes = false;
	private boolean supportsMultiRowInserts = true;

	/**
	 * Upsert mode.
	 */
	private UPSERT_MODE upsertMode = UPSERT_MODE.FROM_JL_DUAL;
	
	/**
	 * Update mode.
	 */
	private UPDATE_MODE updateMode = null;

	/**
	 * Returns whether the DBMS requires the {@code VALUES} keyword in DELETE statements with an IN-clause.
	 *
	 * @return <code>true</code> if the {@code VALUES} keyword is needed for deletes
	 */
	public boolean isNeedsValuesKeywordForDeletes() {
		return needsValuesKeywordForDeletes;
	}

	/**
	 * Sets whether the DBMS requires the {@code VALUES} keyword in DELETE statements with an IN-clause.
	 *
	 * @param needsValuesKeywordForDeletes <code>true</code> if the {@code VALUES} keyword is needed for deletes
	 */
	public void setNeedsValuesKeywordForDeletes(boolean needsValuesKeywordForDeletes) {
		this.needsValuesKeywordForDeletes = needsValuesKeywordForDeletes;
	}

	/**
	 * Returns whether the DBMS supports IN-clauses in DELETE statements.
	 *
	 * @return <code>true</code> if IN-clauses for deletes are supported
	 */
	public boolean isSupportsInClauseForDeletes() {
		return supportsInClauseForDeletes;
	}

	/**
	 * Sets whether the DBMS supports IN-clauses in DELETE statements.
	 *
	 * @param supportsInClauseForDeletes <code>true</code> if IN-clauses for deletes are supported
	 */
	public void setSupportsInClauseForDeletes(boolean supportsInClauseForDeletes) {
		this.supportsInClauseForDeletes = supportsInClauseForDeletes;
	}

	/**
	 * Returns whether the DBMS supports multi-row INSERT statements.
	 *
	 * @return <code>true</code> if multi-row inserts are supported
	 */
	public boolean isSupportsMultiRowInserts() {
		return supportsMultiRowInserts;
	}

	/**
	 * Sets whether the DBMS supports multi-row INSERT statements.
	 *
	 * @param supportsMultiRowInserts <code>true</code> if multi-row inserts are supported
	 */
	public void setSupportsMultiRowInserts(boolean supportsMultiRowInserts) {
		this.supportsMultiRowInserts = supportsMultiRowInserts;
	}

	/**
	 * Gets the upsert mode.
	 *
	 * @return the upsert mode
	 */
	public UPSERT_MODE getUpsertMode() {
		return upsertMode;
	}

	/**
	 * Sets the upsert mode.
	 *
	 * @param upsertMode the upsert mode to set
	 */
	public void setUpsertMode(UPSERT_MODE upsertMode) {
		this.upsertMode = upsertMode;
	}

	/**
	 * Gets the update mode.
	 *
	 * @return the update mode
	 */
	public UPDATE_MODE getUpdateMode() {
		return updateMode;
	}

	/**
	 * Sets the update mode.
	 *
	 * @param updateMode the update mode to set
	 */
	public void setUpdateMode(UPDATE_MODE updateMode) {
		this.updateMode = updateMode;
	}

	/**
	 * Gets table reference for DML statements for a given working table.
	 * 
	 * @param tableName the working table
	 * @param session holds connection to DBMS
	 * @return table reference for the working table
	 * @throws SQLException 
	 */
	public static String dmlTableReference(String tableName, Session session, ExecutionContext executionContext) throws SQLException {
		String tableRef;
		TemporaryTableManager tableManager = null;
		WorkingTableScope temporaryTableScope = executionContext.getScope();
		if (temporaryTableScope == WorkingTableScope.SESSION_LOCAL) {
			tableManager = session.dbms.getSessionTemporaryTableManager();
		}
		if (temporaryTableScope == WorkingTableScope.TRANSACTION_LOCAL) {
			tableManager = session.dbms.getTransactionTemporaryTableManager();
		}
		if (tableManager != null) {
			tableRef = tableManager.getDmlTableReference(tableName);
		} else {
			tableRef = tableName;
		}
		if (temporaryTableScope != WorkingTableScope.LOCAL_DATABASE) {
			String schema = executionContext.getWorkingTableSchema();
			if (schema != null) {
				tableRef = Quoting.getQuoting(session).requote(schema) + "." + tableRef;
			}
		}
		return tableRef;
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "SQLDialect [needsValuesKeywordForDeletes="
				+ needsValuesKeywordForDeletes
				+ ", supportsInClauseForDeletes=" + supportsInClauseForDeletes
				+ ", supportsMultiRowInserts=" + supportsMultiRowInserts
				+ ", upsertMode=" + upsertMode + "]";
	}
	
}
