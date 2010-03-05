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
package net.sf.jailer.ui;

import java.awt.Color;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import javax.swing.SwingUtilities;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.ModelElement;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.progress.ProgressListener;

/**
 * Keeps progress indicators up to date.
 * 
 * @author Ralf Wisser
 */
public class UIProgressListener implements ProgressListener {

	/**
	 * Table showing collected rows.
	 */
	private final ProgressTable progressTable;

	/**
	 * Holds collections per day.
	 */
	private final List<Map<ModelElement, Long>> collections = new ArrayList<Map<ModelElement, Long>>();

	/**
	 * Holds rows per tables.
	 */
	private final Map<String, Long> rowsPerTable = new TreeMap<String, Long>();

	/**
	 * Today.
	 */
	private int today = -1, lastUpdated = -1;
	private boolean lastRowIsUptodate = false;

	/**
	 * To stop the thread.
	 */
	private boolean stop = false;

	/**
	 * Row counters.
	 */
	private long exportedRows = 0;
	private long collectedRows = 0;
	private String currentStep = "";
	private boolean isErrorStage = false;
	private boolean stopClock = false;
	private boolean rowsPerTableIsUptodate = false;

	private final DataModel dataModel;

	/**
	 * Constructor.
	 * 
	 * @param progressTable
	 *            table showing collected rows
	 */
	public UIProgressListener(final ProgressTable progressTable, final ProgressPanel progressPanel, DataModel dataModel) {
		this.progressTable = progressTable;
		this.dataModel = dataModel;
		new Thread(new Runnable() {

			@Override
			public void run() {
				final long startTime = System.currentTimeMillis();
				while (!getStop()) {
					final long[] nextUpdateTS = new long[] { 0 };
					try {
						Thread.sleep(100);
					} catch (InterruptedException e) {
						e.printStackTrace();
					}
					boolean update = false, timeForUpdate = true;
					synchronized (UIProgressListener.this) {
						update = today >= 0 && (lastUpdated != today || !lastRowIsUptodate);
						if (nextUpdateTS[0] != 0 && System.currentTimeMillis() < nextUpdateTS[0]) {
							timeForUpdate = false;
						}
					}
					final boolean fUpdate = update;
					if (timeForUpdate) {
						SwingUtilities.invokeLater(new Runnable() {
							@Override
							public void run() {
								if (fUpdate) {
									addOrUpdateRows();
								}
								synchronized (UIProgressListener.this) {
									progressTable.setTotalNumberOfCollectedRows(collectedRows);
									progressPanel.collectedRowsLabel.setText("" + collectedRows);
									progressPanel.exportedRowsLabel.setText("" + exportedRows);
									progressPanel.stepLabel.setText(currentStep);
									if (isErrorStage) {
										progressPanel.stepLabel.setForeground(Color.RED);
									}
									if (!rowsPerTableIsUptodate) {
										progressPanel.updateRowsPerTable(rowsPerTable);
										rowsPerTableIsUptodate = true;
									}
									long t = System.currentTimeMillis();
									if (!stopClock) {
										long et = (t - startTime)/1000;
										long sec = et % 60;
										long min = (et / 60) % 60;
										long h = et / 3600;
										progressPanel.elapsedTimeLabel.setText((h<10? "0" : "") + h + ":" + (min<10? "0" : "") + min + ":" + (sec<10? "0" : "") + sec);
									}
									nextUpdateTS[0] = t + 200;
								}
							}
						});
					}
				}
			}
		}).start();
	}

	/**
	 * Adds or updates a row of the progress table.
	 */
	private synchronized void addOrUpdateRows() {
		for (int day = lastUpdated + (lastRowIsUptodate ? 1 : 0); day <= today; ++day) {
			if (day >= 0) {
				addOrUpdateRow(day);
			}
		}
		lastUpdated = today;
		lastRowIsUptodate = true;
	}

	/**
	 * Adds or updates a row of the progress table.
	 * 
	 * @param day
	 *            the day
	 */
	private void addOrUpdateRow(int day) {
		Map<Table, ProgressTable.CellInfo> row = new HashMap<Table, ProgressTable.CellInfo>();
		for (Map.Entry<ModelElement, Long> e : collections.get(day).entrySet()) {
			ProgressTable.CellInfo cell = row.get(getDestination(e.getKey()));
			if (cell != null) {
				if (e.getValue() == null) {
					cell.numberOfRows = -1;
				} else {
					if (cell.numberOfRows >= 0) {
						cell.numberOfRows += e.getValue();
					}
				}
			} else {
				cell = new ProgressTable.CellInfo();
				cell.numberOfRows = e.getValue() == null ? -1 : e.getValue();
				cell.tableName = dataModel.getDisplayName(getDestination(e.getKey()));
				cell.parentNames = new HashSet<String>();
				row.put(getDestination(e.getKey()), cell);
			}
			if (e.getKey() instanceof Association) {
				if (day == today || e.getValue() != null && e.getValue() > 0) {
					cell.parentNames.add(dataModel.getDisplayName(((Association) e.getKey()).source));
				}
			}
		}
		List<ProgressTable.CellInfo> theRow = new ArrayList<ProgressTable.CellInfo>(row.values());
		if (day < today) {
			for (Iterator<ProgressTable.CellInfo> i = theRow.iterator(); i.hasNext();) {
				if (i.next().numberOfRows == 0) {
					i.remove();
				}
			}
		}
		if (day == lastUpdated) {
			progressTable.replaceLastRow(theRow, day);
		} else {
			progressTable.addRow(theRow, day);
			progressTable.adjustColumnWidth();
		}
	}

	private Table getDestination(ModelElement key) {
		if (key instanceof Association) {
			return ((Association) key).destination;
		}
		return (Table) key;
	}

	/**
	 * Rows have been collected.
	 * 
	 * @param day
	 *            the day
	 * @param modelElement
	 *            the association or table which has been resolved
	 * @param rc
	 *            the number of rows which have been collected
	 */
	@Override
	public synchronized void collected(int day, ModelElement modelElement, long rc) {
		if (collections.size() > day) {
			collections.get(day).put(modelElement, rc);
			if (rc < 0) {
				collections.get(day).remove(modelElement);
			} else {
				collectedRows += rc;
				if (rc > 0) {
					rowsPerTableIsUptodate = false;
					String tableName = dataModel.getDisplayName(getDestination(modelElement));
					if (rowsPerTable.containsKey(tableName)) {
						rowsPerTable.put(tableName, rowsPerTable.get(tableName) + rc);
					} else {
						rowsPerTable.put(tableName, rc);
					}
				}
			}
			lastRowIsUptodate = false;
		}
	}

	/**
	 * Collection of rows has been started.
	 * 
	 * @param day
	 *            the day
	 * @param modelElement
	 *            the association or table to be resolved
	 */
	@Override
	public synchronized void startedCollection(int day, ModelElement modelElement) {
		today = day;
		while (collections.size() <= today) {
			collections.add(new HashMap<ModelElement, Long>());
		}
		collections.get(today).put(modelElement, null);
		lastRowIsUptodate = false;
	}

	private synchronized boolean getStop() {
		return stop;
	}

	public synchronized void stop() {
		stop = true;
	}

	/**
	 * Rows have been exported.
	 * 
	 * @param table
	 *            the table from which the rows have been exported
	 * @param rc
	 *            the number of rows
	 */
	@Override
	public synchronized void exported(Table table, long rc) {
		exportedRows += rc;
	}

	/**
	 * New stage has begun.
	 * 
	 * @param stage
	 *            the stage
	 */
	@Override
	public synchronized void newStage(String stage, boolean isErrorStage, boolean isFinalStage) {
		this.currentStep = stage;
		this.isErrorStage = isErrorStage;
		this.stopClock = isFinalStage;
	}

}
