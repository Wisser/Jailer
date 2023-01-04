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
package net.sf.jailer.ui;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.Pair;

/**
 * Methods for managing data model files and folders.
 * 
 * @author Ralf Wisser
 */
public class DataModelManager {
	
	/**
	 * Gets names of all model folders. Including <code>null</code> for the
	 * default model.
	 */
	public static List<String> getModelFolderNames(ExecutionContext executionContext) {
		File dmFolder = new File(getBaseFolder(executionContext));
		List<String> folders = new ArrayList<String>();
		if (new File(dmFolder, DataModel.TABLE_CSV_FILE).exists()) {
			folders.add(null);
		}
		File[] files = dmFolder.listFiles();
		if (files != null) {
			for (File f : files) {
				if (f.isDirectory()) {
					if (new File(f, DataModel.TABLE_CSV_FILE).exists()) {
						folders.add(f.getName());
					}
				}
			}
		}
		return folders;
	}

	private static String getBaseFolder(ExecutionContext executionContext) {
		return executionContext.getQualifiedDatamodelFolder();
	}

	/**
	 * Deletes a Data Model.
	 * 
	 * @param modelFolder
	 *            folder name, <code>null</code> for the default model
	 */
	public static boolean deleteModel(String modelFolder, ExecutionContext executionContext) {
		String dir = getBaseFolder(executionContext) + File.separator + (modelFolder != null ? modelFolder + File.separator : "");
		File nameFile = new File(dir + DataModel.TABLE_CSV_FILE);
		try {
			copyFile(nameFile, new File(dir + DataModel.TABLE_CSV_FILE + ".bak"));
		} catch (Exception e) {
			// ignore
		}
		if (!nameFile.delete()) {
			return false;
		}
		if (modelFolder != null) {
			nameFile.getParentFile().renameTo(new File(getBaseFolder(executionContext) + File.separator + modelFolder + ".bak"));
		}
		return true;
	}

	/**
	 * Gets display name and last-modified timestamp of a given model folders as
	 * a pair.
	 * 
	 * @param modelFolder
	 *            folder name, <code>null</code> for the default model
	 */
	public static Pair<String, Long> getModelDetails(String modelFolder, ExecutionContext executionContext) {
		File nameFile = new File(
				(modelFolder != null? executionContext.getDatamodelFolder() : getBaseFolder(executionContext)) + File.separator + (modelFolder != null ? modelFolder + File.separator : "") + DataModel.MODELNAME_CSV_FILE);
		String name = null;
		Long lastModified = null;
		try {
			if (nameFile.exists()) {
				List<CsvFile.Line> nameList = new ArrayList<CsvFile.Line>(new CsvFile(nameFile).getLines());
				if (nameList.size() > 0) {
					CsvFile.Line line = nameList.get(0);
					name = line.cells.get(0);
					lastModified = Long.parseLong(line.cells.get(1));
				}
			}
		} catch (Throwable t) {
			// keep defaults
		}
		if (name == null) {
			if (modelFolder == null) {
				name = "Default";
			} else {
				name = modelFolder;
			}
		}
		return new Pair<String, Long>(name, lastModified);
	}

	/**
	 * Sets folder of current data model.
	 * 
	 * @param modelFolder
	 *            the folder, <code>null</code> for default model
	 */
	public static void setCurrentModelSubfolder(String modelFolder, ExecutionContext executionContext) {
		executionContext.setCurrentModelSubfolder(modelFolder);
	}

	/**
	 * Gets folder of current data model.
	 * 
	 * @return modelFolder the folder, <code>null</code> for default model
	 */
	public static String getCurrentModelSubfolder(ExecutionContext executionContext) {
		return executionContext.getCurrentModelSubfolder();
	}

	/**
	 * Creates a new model
	 * 
	 * @param newName
	 *            model name
	 * @param folderName
	 *            folder name
	 * @throws IOException
	 */
	public static void createNewModel(String newName, String folderName, ExecutionContext executionContext) throws IOException {
		setCurrentModelSubfolder(null, executionContext);
		File modelFolder = new File(getBaseFolder(executionContext) + File.separator + folderName);
		if (!modelFolder.exists()) {
			if (!modelFolder.mkdir()) {
				throw new IOException("Unable to create folder \"" + modelFolder.getAbsolutePath() + "\"");
			}
		}
		
		setCurrentModelSubfolder(folderName, executionContext);

		for (String file : new String[] { DataModel.getTablesFile(executionContext), DataModel.getAssociationsFile(executionContext), DataModel.getColumnsFile(executionContext) }) {
			File toCreate = new File(file);
			BufferedWriter out = new BufferedWriter(new FileWriter(toCreate));
			out.write(" ");
			out.close();
		}
		DataModelEditor.createNameFile(newName, executionContext);
	}

	private static void copyFile(File in, File out) throws Exception {
		FileInputStream fis = new FileInputStream(in);
		FileOutputStream fos = new FileOutputStream(out);

		byte[] buf = new byte[16 * 1024];
		int i = 0;

		while ((i = fis.read(buf)) != -1) {
			fos.write(buf, 0, i);
		}

		fis.close();
		fos.close();
	}
}
