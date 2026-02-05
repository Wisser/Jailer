package net.sf.jailer.ui.databrowser.whereconditioneditor;

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
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.Environment;

/**
 * Stores and restores condition-configurations.
 * 
 * @author Ralf Wisser
 */
class ConditionStorage {
	
	private static final String CONDITIONFILE_EXTENSION = ".cond";
	
	private static File conditionFile(ExecutionContext executionContext) {
		String currentModelSubfolder = modelSubfolder(executionContext);
		return Environment.newFile("condition" + File.separator + new File(currentModelSubfolder).getName() + CONDITIONFILE_EXTENSION);
	}

	private static String modelSubfolder(ExecutionContext executionContext) {
		String currentModelSubfolder = executionContext.getCurrentModelSubfolder();
		if (currentModelSubfolder  == null) {
			currentModelSubfolder = "default";
		}
		return currentModelSubfolder;
	}

	// datamodel -> table -> column-list
	private static Map<String, Map<String, List<String>>> storage = new HashMap<String, Map<String,List<String>>>();
	
	@SuppressWarnings("unchecked")
	private static synchronized Map<String, List<String>> loadConfigs(ExecutionContext executionContext) {
		File configFile = conditionFile(executionContext);
		configFile.getParentFile().mkdirs();
		String modelSubfolder = modelSubfolder(executionContext);
		if (!storage.containsKey(modelSubfolder)) {
			storage.put(modelSubfolder, new HashMap<String,List<String>>());
			File file = configFile;
			if (file.exists()) {
				try {
					ObjectInputStream in = new ObjectInputStream(new FileInputStream(file)); // lgtm [java/input-resource-leak]
					storage.put(modelSubfolder, (Map<String,List<String>>) in.readObject());
					in.close();
				} catch (Exception e) {
					// ignore
				}
			}
		}
		return storage.get(modelSubfolder);
	}

	public static synchronized List<String> load(Table table, ExecutionContext executionContext) {
		Map<String, List<String>> configs = loadConfigs(executionContext);
		return configs.getOrDefault(table.getName(), new ArrayList<String>());
	}

	public static synchronized void store(Table table, List<String> config, ExecutionContext executionContext) {
		Map<String, List<String>> configs = loadConfigs(executionContext);
		configs.put(table.getName(), config);
		Thread tread = new Thread(() -> {
			doStore(executionContext);
		}, "condition-store");
		tread.setDaemon(true);
		tread.start();
	}
	
	private static void doStore(ExecutionContext executionContext) {
		File file = conditionFile(executionContext);
		file.getParentFile().mkdirs();
		for (int retry = 0; retry < 4; ++retry) {
			try {
				String modelSubfolder = modelSubfolder(executionContext);
				synchronized (ConditionStorage.class) {
					ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(file)); // lgtm [java/output-resource-leak]
					out.writeObject(storage.get(modelSubfolder));
					out.close();
				}
				return;
			} catch (Exception e) {
				e.printStackTrace();
			}
			try {
				Thread.sleep(100);
			} catch (InterruptedException e) {
				// ignore
			}
		}
	}
}
