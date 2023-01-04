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
package net.sf.jailer.subsetting;

import java.util.HashMap;
import java.util.Map;

import net.sf.jailer.datamodel.Table;

/**
 * Provides statistical information about the export process.
 * 
 * @author Ralf Wisser
 */
public class ExportStatistic {

	private Map<Table, Long> exportedRows = new HashMap<Table, Long>();
	private long total;

	/**
	 * Gets number of exported rows per table.
	 * 
	 * @return number of exported rows per table
	 */
	public Map<Table, Long> getExportedRows() {
		return exportedRows;
	}
	
	/**
	 * Sets number of exported rows per table.
	 * 
	 * @param exportedRows number of exported rows per table
	 */
	public void setExportedRows(Map<Table, Long> exportedRows) {
		this.exportedRows = exportedRows;
	}

	/**
	 * Gets total number of exported rows.
	 * 
	 * @return total number of exported rows
	 */
	public long getTotal() {
		return total;
	}
	
	/**
	 * sets total number of exported rows.
	 * 
	 * @param total total number of exported rows
	 */
	public void setTotal(long total) {
		this.total = total;
	}
	
}
