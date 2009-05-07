/*
 * Copyright 2007 the original author or authors.
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
package net.sf.jailer.ui.graphical_view;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import net.sf.jailer.util.CsvFile;

/**
 * Stores and restores graph layout.
 * 
 * @author Ralf Wisser
 */
public class LayoutStorage {
    
	/**
	 * File holding global layouts.
	 */
	private static final String GLOBAL_STORAGE = ".layout";
	
	/**
	 * Sets position.
	 * 
	 * @param rootTable root table
	 * @param table table
	 * @param position position: x, y, isFixed
	 */
	public static void setPosition(String rootTable, String table, double[] position) {
		if (rootTable == null) {
			throw new RuntimeException("rootTable is null");
		}
		if (table == null) {
			throw new RuntimeException("table is null");
		}
		Map<String, double[]> posMap = tablePositions.get(rootTable);
		if (posMap == null) {
			posMap = new HashMap<String, double[]>();
			tablePositions.put(rootTable, posMap);
		}
		posMap.put(table, position);
	}

	/**
	 * Gets position.
	 * 
	 * @param rootTable root table
	 * @param table table
	 * @return position: x, y, isFixed
	 */
	public static double[] getPosition(String rootTable, String table) {
		Map<String, double[]> posMap = tablePositions.get(rootTable);
		if (posMap != null) {
			return posMap.get(table);
		}
		return null;
	}

	/**
	 * Gets positions of all tables for a given root-table.
	 * 
	 * @param root the root table
	 * @return positions of all tables for a the root-table
	 */
	public static Map<String, double[]> getPositions(String root) {
		return tablePositions.get(root);
	}

	/**
	 * Removes all position information for a given root table.
	 * 
	 * @param root the root table
	 */
	public static void removeAll(String root) {
		tablePositions.remove(root);
	}

	/**
	 * Store layout into stream and {@link #GLOBAL_STORAGE}.
	 * 
	 * @param out the stream
	 */
	public static void store(PrintWriter out) {
		store0(out);
//		try {
//			PrintWriter gout = new PrintWriter(new File(GLOBAL_STORAGE));
//			store0(gout);
//			gout.close();
//		} catch (FileNotFoundException e) {
//			e.printStackTrace();
//		}
	}

	/**
	 * Store layout into stream.
	 * 
	 * @param out the stream
	 */
	private static void store0(PrintWriter out) {
		out.println(CsvFile.BLOCK_INDICATOR + "layout");
		for (String root: tablePositions.keySet()) {
			Map<String, double[]> pos = tablePositions.get(root);
			for (String table: pos.keySet()) {
				double[] xyf = pos.get(table);
				out.println(CsvFile.encodeCell(root) + "; " + CsvFile.encodeCell(table) + "; " + xyf[0] + "; " + xyf[1] + "; " + xyf[2]);
			}
		}
	}

	/**
	 * Restores a layout from file
	 *  
	 * @param file the file
	 */
	public static void restore(String file) {
		tablePositions.clear();
		try {
			CsvFile csv = new CsvFile(new File(file), "layout");
			for (CsvFile.Line line: csv.getLines()) {
				if (line.cells.get(0).length() > 0) {
					double x = 0.0;
					double y = 0.0;
					double f = 0.0;
					try {
						x = Double.parseDouble(line.cells.get(2));
						y = Double.parseDouble(line.cells.get(3));
						f = Double.parseDouble(line.cells.get(4));
						setPosition(line.cells.get(0), line.cells.get(1), new double[] { x, y, f });
					} catch (Exception e) {
						//ignore
					}
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Stores positions per root-table.
	 */
	private static Map<String, Map<String, double[]>> tablePositions = Collections.synchronizedMap(new HashMap<String, Map<String,double[]>>());

}
