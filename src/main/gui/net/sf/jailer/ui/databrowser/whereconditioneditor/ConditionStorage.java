package net.sf.jailer.ui.databrowser.whereconditioneditor;

/*
 * Copyright 2007 - 2021 Ralf Wisser.
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
import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.Environment;
import net.sf.jailer.ui.UIUtil;

/**
 * Stores and restores condition-configurations.
 * 
 * @author Ralf Wisser
 */
class ConditionStorage {
	
	private static final String CONDITIONFILE_EXTENSION = ".cond";

	public static class ConditionConfigItem {
		public final String columnName;
		public final Operator operator;
		
		// TODO
//	public ConditionConfigItem(String columnName, Operator operator) { // TODO
//			this.columnName = columnName;
//			this.operator = operator;
//		}
//	}
	
	// datamodel -> table -> column-list
	private static Map<String, Map<String, List<String>>> storage = new HashMap<String, Map<String,List<String>>>();
	
	public static synchronized Set<String> load(Table table, ExecutionContext executionContext) {
		if (storage.containsKey(executionContext.getCurrentModelSubfolder())) {
			return storage.get(executionContext.getCurrentModelSubfolder());
		}
		Set<String> result = ;
		
		storage.put(table, result);
		return result;
	}

	public static synchronized void store(String tableName, Set<String> columnNames, ExecutionContext executionContext) {
		
		storage.put(tableName, columnNames);
		
		Thread tread = new Thread(() -> doStore(table, config, executionContext), "condition-store");
		tread.setDaemon(true);
		tread.start();
	}
	
	private static synchronized void doStore(Table table, List<ConditionConfigItem> config, ExecutionContext executionContext) {
		
	}

	private static File getWhereConditionFolder(String currentModelSubfolder) {
		if (currentModelSubfolder == null) {
			currentModelSubfolder = "default";
		}
		return Environment.newFile("condition" + File.separator + new File(currentModelSubfolder).getName());
	}

	/**
     * @param tableName table name
     * @return file containing config with given name or <code>null</code>, if no such config exists
     */
	private static File getWhereConditionFile(String tableName, ExecutionContext executionContext) {
		if (tableName == null) {
			return null;
		}
		File bmFile = new File(getWhereConditionFolder(executionContext.getCurrentModelSubfolder()), tableName + CONDITIONFILE_EXTENSION);
		try {
			if (bmFile.exists()) {
				return bmFile;
			}
		} catch (Exception e) {
			// ignore
		}
		bmFile = new File(getWhereConditionFolder(executionContext.getCurrentModelSubfolder()), UIUtil.toValidFileName(tableName) + CONDITIONFILE_EXTENSION);
		if (bmFile.exists()) {
			return bmFile;
		} else {
			return null;
		}
	}


}
