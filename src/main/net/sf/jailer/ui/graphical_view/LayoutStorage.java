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
	 * Folder holding global layouts.
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
	 * Removes layout if it is not significant.
	 * 
	 * @param rootTable root table for which the layout is
	 */
	public static void checkSignificance(String rootTable) {
		Map<String, double[]> layout = tablePositions.get(rootTable);
		if (!isSignificant(rootTable, layout)) {
			tablePositions.remove(rootTable);
		} else {
			globalTablePositions.put(rootTable, new HashMap<String, double[]>(layout));
		}
	}

	/**
	 * Checks whether a layout is significant.
	 * 
	 * @param rootTable root table for which the layout is
	 * @param layout the layout
	 * @return <code>true</code> if layout is significant
	 */
	private static boolean isSignificant(String rootTable, Map<String, double[]> layout) {
		if (layout != null) {
			for (String t: layout.keySet()) {
				if (!t.equals(rootTable) && !"$ZOOMBOX".equals(t)) {
					if (layout.get(t)[2] == 1.0) {
						return true;
					}
				}
			}
		}
		return false;
	}
	
	/**
	 * Gets file to store global layout in for a given root table.
	 * 
	 * @param rootTable the root table
	 * @return file to store global layout in, <code>null</code> if the file cannot be created
	 */
	private static File getGlobalStorageFile(String rootTable) {
		try {
			File dir = new File(GLOBAL_STORAGE);
			if (!dir.exists()) {
				if (!dir.mkdir()) {
					return null;
				}
			}
			String fn = "";
			for (int i = 0; i < rootTable.length(); ++i) {
				char c = rootTable.charAt(i);
				if (Character.isLetterOrDigit(c) || c == '.') {
					fn += c;
				}
			}
			if (fn.length() > 0) {
				return new File(dir, fn);
			}
			return null;
		} catch (Exception e) {
			return null;
		}
	}
	
	/**
	 * If storage is disabled, no persistent layout is available.
	 */
	public volatile static boolean enabled = true;
	
	/**
	 * Gets position.
	 * 
	 * @param rootTable root table
	 * @param table table
	 * @return position: x, y, isFixed
	 */
	public static double[] getPosition(String rootTable, String table) {
		if (enabled) {
			readGlobalLayout(rootTable);
			Map<String, double[]> posMap = tablePositions.get(rootTable);
			if (posMap != null) {
				return posMap.get(table);
			}
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
		if (enabled) {
			readGlobalLayout(root);
			return tablePositions.get(root);
		}
		return null;
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
	 * Removes all position information.
	 * 
	 * @param root the root table
	 */
	public static void removeAll() {
		tablePositions.clear();
		globalTablePositions.clear();
	}

	/**
	 * Store layout into stream.
	 * 
	 * @param out the stream
	 */
	public static void store(PrintWriter out) {
		store(out, tablePositions);
		
		try {
			for (String rootTable: tablePositions.keySet()) {
				Map<String, double[]> layout = globalTablePositions.get(rootTable);
				if (layout == null) {
					continue;
				}
				globalTablePositions.put(rootTable, new HashMap<String, double[]>(layout));
				Map<String, Map<String, double[]>> tmp = new HashMap<String, Map<String,double[]>>();
				tmp.put(rootTable, layout);
				File file = getGlobalStorageFile(rootTable);
				if (file != null) {
					PrintWriter tout = new PrintWriter(file);
					store(tout, tmp);
					tout.close();
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Reads global layout if local layout does not exist.
	 * 
	 * @param rootTable
	 */
	private static void readGlobalLayout(String rootTable) {
		if (!tablePositions.containsKey(rootTable)) {
			if (!globalTablePositions.containsKey(rootTable)) {
				File file = getGlobalStorageFile(rootTable);
				Map<String, double[]> layout = null;
				if (file != null) {
					try {
						restore(file.getAbsolutePath(), false);
						if (tablePositions.get(rootTable) != null) {
							layout = new HashMap<String, double[]>(tablePositions.get(rootTable));
						}
					} catch (Exception e) {
						e.printStackTrace();
					}
				}
				globalTablePositions.put(rootTable, layout);
			}
			Map<String, double[]> layout = globalTablePositions.get(rootTable);
			if (layout != null) {
				tablePositions.put(rootTable, new HashMap<String, double[]>(globalTablePositions.get(rootTable)));
			}
		}
	}

	/**
	 * Store layout into stream.
	 * 
	 * @param out the stream
	 */
	public static void store(PrintWriter out, Map<String, Map<String, double[]>> positions) {
		out.println(CsvFile.BLOCK_INDICATOR + "layout");
		for (String root: positions.keySet()) {
			Map<String, double[]> pos = positions.get(root);
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
		restore(file, true);
	}

	/**
	 * Restores a layout from file
	 *  
	 * @param file the file
	 */
	private static void restore(String file, boolean resetLayout) {
		if (resetLayout) {
			tablePositions.clear();
			globalTablePositions.clear();
		}
		try {
			File csvFile = new File(file);
			if (csvFile.exists()) {
				CsvFile csv = new CsvFile(csvFile, "layout");
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
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Stores positions per root-table (local).
	 */
	private static Map<String, Map<String, double[]>> tablePositions = Collections.synchronizedMap(new HashMap<String, Map<String,double[]>>());

	/**
	 * Stores positions per root-table (global).
	 */
	private static Map<String, Map<String, double[]>> globalTablePositions = Collections.synchronizedMap(new HashMap<String, Map<String,double[]>>());

}
