/*
 * Copyright 2007 - 2015 the original author or authors.
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

import net.sf.jailer.CommandLineParser;
import net.sf.jailer.Configuration;

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
	public boolean needsValuesKeywordForDeletes = false;
	public boolean supportsInClauseForDeletes = false;
	public boolean supportsMultiRowInserts = false;

	/**
	 * Upsert mode.
	 */
	public UPSERT_MODE upsertMode = UPSERT_MODE.FROM_JL_DUAL;

    /**
	 * @return the needsValuesKeywordForDeletes
	 */
	public boolean isNeedsValuesKeywordForDeletes() {
		return needsValuesKeywordForDeletes;
	}

	/**
	 * @param needsValuesKeywordForDeletes the needsValuesKeywordForDeletes to set
	 */
	public void setNeedsValuesKeywordForDeletes(boolean needsValuesKeywordForDeletes) {
		this.needsValuesKeywordForDeletes = needsValuesKeywordForDeletes;
	}

	/**
	 * @return the supportsInClauseForDeletes
	 */
	public boolean isSupportsInClauseForDeletes() {
		return supportsInClauseForDeletes;
	}

	/**
	 * @param supportsInClauseForDeletes the supportsInClauseForDeletes to set
	 */
	public void setSupportsInClauseForDeletes(boolean supportsInClauseForDeletes) {
		this.supportsInClauseForDeletes = supportsInClauseForDeletes;
	}

	/**
	 * @return the supportsMultiRowInserts
	 */
	public boolean isSupportsMultiRowInserts() {
		return supportsMultiRowInserts;
	}

	/**
	 * @param supportsMultiRowInserts the supportsMultiRowInserts to set
	 */
	public void setSupportsMultiRowInserts(boolean supportsMultiRowInserts) {
		this.supportsMultiRowInserts = supportsMultiRowInserts;
	}

	/**
	 * @return the upsertMode
	 */
	public UPSERT_MODE getUpsertMode() {
		return upsertMode;
	}

	/**
	 * @param upsertMode the upsertMode to set
	 */
	public void setUpsertMode(UPSERT_MODE upsertMode) {
		this.upsertMode = upsertMode;
	}

	/**
     * Gets table reference for DML statements for a given working table.
     * 
     * @param tableName the working table
     * @param session holds connection to DBMS
     * @return table reference for the working table
     */
    public static String dmlTableReference(String tableName, Session session) {
    	TemporaryTableManager tableManager = null;
    	TemporaryTableScope temporaryTableScope = CommandLineParser.getInstance().getTemporaryTableScope();
		if (temporaryTableScope == TemporaryTableScope.SESSION_LOCAL) {
			tableManager = Configuration.forDbms(session).sessionTemporaryTableManager;
		}
		if (temporaryTableScope == TemporaryTableScope.TRANSACTION_LOCAL) {
			tableManager = Configuration.forDbms(session).transactionTemporaryTableManager;
		}
		if (tableManager != null) {
			return tableManager.getDmlTableReference(tableName);
		}
		return tableName;
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
