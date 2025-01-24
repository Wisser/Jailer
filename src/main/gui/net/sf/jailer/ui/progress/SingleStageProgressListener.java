/*
 * Copyright 2007 - 2025 Ralf Wisser.
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
package net.sf.jailer.ui.progress;

import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.ModelElement;
import net.sf.jailer.datamodel.RowIdSupport;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ddl.DDLCreator;
import net.sf.jailer.progress.ProgressListener;
import net.sf.jailer.ui.Colors;
import net.sf.jailer.ui.ProgressPanel;
import net.sf.jailer.ui.ProgressTable;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.util.CancellationException;

/**
 * Keeps progress indicators up to date for a single {@link ProgressTable}.
 *
 * @author Ralf Wisser
 */
public abstract class SingleStageProgressListener implements ProgressListener {

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
	private boolean readjustColumnWidth = false;

	/**
	 * To stop the thread.
	 */
	private boolean stop = false;

	/**
	 * Row counters.
	 */
	private AtomicLong exportedRows = new AtomicLong();
	private long collectedRows = 0;
	private String currentStep = "";
	private boolean isErrorStage = false;
	private boolean stopClock;
	private boolean rowsPerTableIsUptodate = false;

	private final DataModel dataModel;

	private Map<Table, Integer> inProgress = new HashMap<Table, Integer>();
	private boolean cleanupLastLine = false;
	private final boolean confirm;
	private long timeDelay = 0;
	private final Set<String> targetSchemaSet;
	private final boolean forExportStage;
	private final boolean checkPK;

	/**
	 * Constructor.
	 *
	 * @param progressTable
	 *            table showing collected rows
	 * @param targetSchemaSet
	 */
	public SingleStageProgressListener(final ProgressTable progressTable, final ProgressPanel progressPanel, DataModel dataModel, final boolean confirm, Set<String> targetSchemaSet, boolean forExportStage, boolean checkPK) {
		this.progressTable = progressTable;
		this.dataModel = dataModel;
		this.confirm = confirm;
		this.targetSchemaSet = targetSchemaSet;
		this.forExportStage = forExportStage;
		this.stopClock = !forExportStage;
		this.checkPK = checkPK;
		Thread thread = new Thread(new Runnable() {

			@Override
			public void run() {
				final long startTime = System.currentTimeMillis();
				while (!getStop()) {
					final long[] nextUpdateTS = new long[] { 0 };
					try {
						Thread.sleep(500);
					} catch (InterruptedException e) {
						e.printStackTrace();
					}
					boolean update = false, timeForUpdate = true;
					synchronized (SingleStageProgressListener.this) {
						update = today >= 0 && (lastUpdated != today || !lastRowIsUptodate);
						if (nextUpdateTS[0] != 0 && System.currentTimeMillis() < nextUpdateTS[0]) {
							timeForUpdate = false;
						}
					}
					final boolean fUpdate = update;
					if (timeForUpdate) {
						updateRowTable(progressTable, progressPanel, startTime,
								nextUpdateTS, fUpdate);
					}
				}
				synchronized (SingleStageProgressListener.this) {
					lastRowIsUptodate = false;
				}
				updateRowTable(progressTable, progressPanel, startTime, new long[] { 0 }, true);
			}

			private void updateRowTable(final ProgressTable progressTable,
					final ProgressPanel progressPanel, final long startTime,
					final long[] nextUpdateTS, final boolean fUpdate) {
				UIUtil.invokeLater(new Runnable() {
					@Override
					public void run() {
						if (fUpdate) {
							addOrUpdateRows();
						}
						synchronized (SingleStageProgressListener.this) {
							if (!rowsPerTableIsUptodate) {
								if (SingleStageProgressListener.this.forExportStage) {
									progressPanel.updateRowsPerTable(rowsPerTable);
								} else {
									progressPanel.updateRowsReductionPerTable(rowsPerTable);
								}
								rowsPerTableIsUptodate = true;
							}
							if (SingleStageProgressListener.this.forExportStage) {
								progressTable.setTotalNumberOfCollectedRows(collectedRows);
								progressPanel.collectedRowsLabel.setText(UIUtil.format(collectedRows));
								progressPanel.exportedRowsLabel.setText(UIUtil.format(exportedRows.get()));
								if (!progressPanel.inCancellingStep || isErrorStage) {
									progressPanel.stepLabel.setText(currentStep);
									if (isErrorStage) {
										progressPanel.setStepLabelForeground(Colors.Color_255_0_0);
									}
								}
								long t = System.currentTimeMillis();
								if (!stopClock) {
									long et = (t - startTime - timeDelay)/1000;
									long sec = et % 60;
									long min = (et / 60) % 60;
									long h = et / 3600;
									progressPanel.elapsedTimeLabel.setText((h<10? "0" : "") + h + ":" + (min<10? "0" : "") + min + ":" + (sec<10? "0" : "") + sec);
								}
								nextUpdateTS[0] = t + 500;
							} else {
								progressPanel.deletedRowsLabel.setText(UIUtil.format(exportedRows.get()));
							}
						}
					}
				});
			}
		});
		UIUtil.startDemon(thread);
	}

	/**
	 * Adds or updates a row of the progress table.
	 */
	private void addOrUpdateRows() {
		Runnable r = null;
		synchronized (this) {
			for (int day = lastUpdated + (lastRowIsUptodate ? 1 : 0); day <= today; ++day) {
				if (day >= 0) {
					r = addOrUpdateRow(day);
					if (r != null && day < today) {
						r.run();
						r = null;
					}
				}
			}
			lastUpdated = today;
			lastRowIsUptodate = true;
		}
		if (r != null) {
			r.run();
		}
	}

	/**
	 * Adds or updates a row of the progress table.
	 *
	 * @param day
	 *            the day
	 * @return
	 */
	private Runnable addOrUpdateRow(final int day) {
		Map<Table, ProgressTable.CellInfo> row = new HashMap<Table, ProgressTable.CellInfo>();
		for (Map.Entry<ModelElement, Long> e : collections.get(day).entrySet()) {
			ProgressTable.CellInfo cell = row.get(getDestination(e.getKey()));
			if (cell != null) {
				cell.inProgress = inProgress.containsKey(getDestination(e.getKey()));
				if (e.getValue() == null) {
					cell.numberOfRows = -1;
				} else {
					if (cell.numberOfRows >= 0) {
						cell.numberOfRows += e.getValue();
					}
				}
			} else {
				cell = new ProgressTable.CellInfo();
				cell.inProgress = inProgress.containsKey(getDestination(e.getKey()));
				cell.numberOfRows = e.getValue() == null ? -1 : e.getValue();
				cell.tableName = dataModel.getDisplayName(getDestination(e.getKey()));
				cell.parentNames = new HashSet<String>();
				row.put(getDestination(e.getKey()), cell);
			}
			if (e.getKey() instanceof Association) {
				if (day == today || e.getValue() != null && e.getValue() > 0) {
					cell.parentNames.add(dataModel.getDisplayName(((Association) e.getKey()).source));
					cell.excludeFromDeletion = getDestination(e.getKey()).isExcludedFromDeletion();
				}
			} else if (e.getKey() instanceof Table) {
				cell.excludeFromDeletion = ((Table) e.getKey()).isExcludedFromDeletion();
			}
		}
		final List<ProgressTable.CellInfo> theRow = new ArrayList<ProgressTable.CellInfo>(row.values());
		if (day < today || cleanupLastLine ) {
			if (day > 1) {
				for (Iterator<ProgressTable.CellInfo> i = theRow.iterator(); i.hasNext();) {
					if (i.next().numberOfRows == 0) {
						i.remove();
					}
				}
			}
			cleanupLastLine = false;
		}
		if (day == lastUpdated) {
			progressTable.replaceLastRow(theRow, day);
			if (readjustColumnWidth) {
				readjustColumnWidth = false;
				progressTable.adjustColumnWidth();
			}
		} else {
			return new Runnable() {
				@Override
				public void run() {
					progressTable.addRow(theRow, day);
					progressTable.adjustColumnWidth();
					readjustColumnWidth = false;
				}
			};
		}
		return null;
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
		Table destination = getDestination(modelElement);
		Integer count = inProgress.get(destination);
		if (count == null || count <= 1) {
			inProgress.remove(destination);
		} else {
			inProgress.put(destination, count - 1);
		}
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
//			if (rc <= 0) {
//				readjustColumnWidth = true;
//			}
			lastRowIsUptodate = false;
		}
	}

	/**
	 * A collection-job has been enqueued.
	 *
	 * @param day
	 *            the day
	 * @param modelElement
	 *            the association or table to be resolved
	 */
	@Override
	public synchronized void collectionJobEnqueued(int day, ModelElement modelElement) {
		today = day;
		while (collections.size() <= today) {
			collections.add(new HashMap<ModelElement, Long>());
		}
		collections.get(today).put(modelElement, null);
		lastRowIsUptodate = false;
		readjustColumnWidth = true;
	}

	/**
	 * A collection-job has been enqueued.
	 *
	 * @param day the day
	 * @param modelElement the association or table to be resolved
	 */
	@Override
	public synchronized void collectionJobStarted(int day, ModelElement modelElement) {
		Table destination = getDestination(modelElement);
		Integer count = inProgress.get(destination);
		if (count == null) {
			count = 1;
		} else {
			count = count + 1;
		}
		inProgress.put(destination, count);
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
	public void exported(Table table, long rc) {
		exportedRows.addAndGet(rc);
	}

	/**
	 * New stage has begun.
	 *
	 * @param stage
	 *            the stage
	 */
	@Override
	public synchronized void newStage(String stage, final boolean isErrorStage, final boolean isFinalStage) {
		this.currentStep = stage;
		this.isErrorStage = isErrorStage;
		this.stopClock = isFinalStage;
		this.lastRowIsUptodate = false;
		this.cleanupLastLine = true;
		if (isFinalStage || isErrorStage) {
			final long finalCollectedRows = collectedRows;
			UIUtil.invokeLater(new Runnable() {
				@Override
				public void run() {
					synchronized (SingleStageProgressListener.class) {
						lastRowIsUptodate = false;
						readjustColumnWidth = true;
					}
					addOrUpdateRows();
					if (checkPK && forExportStage && isFinalStage && !isErrorStage) {
						if (!warned && exportedRows.get() != finalCollectedRows) {
							warned = true;
							String universalPrimaryKey = "";
							try {
								universalPrimaryKey = new RowIdSupport(dataModel, DBMS.H2, false, false).getUniversalPrimaryKey().toString();
							} catch (Throwable t) {
								// ignore
							}
							String message = Stream.of(("<b><font color=" + Colors.HTMLColor_ff0000 + ">Warning:</font></b> The number of rows collected ("
									+ finalCollectedRows + ") differs from that of the exported ones ("
									+ exportedRows.get() + ").<br> <br>"
									+ "This may have been caused by an invalid primary key definition.<br>Please note that each primary key must be unique.<br> <br>"
									+ "It is recommended to check the integrity of the primary keys.<br>"
									+ "To do this, use the button here below <br>or the menu item \"<b><u><font color=" + Colors.HTMLColor_ff0000 + ">Check primary keys</font></u></b>\" in the menu called \"Data Model\"."
									+ "<br><br>If that doesn't help, try using the \"local database\" working table scope."
									+ "<br> <br>" + "<small>UPK: " + DDLCreator.uPK + "; " + universalPrimaryKey
									+ "</small>")).map(l -> "<html>" + l + "<br></html>")
									.collect(Collectors.joining(""));
							final JButton button = new JButton("Check Primary Keys");
							button.addActionListener(new ActionListener() {
								@Override
								public void actionPerformed(ActionEvent e) {
									Window window = SwingUtilities.getWindowAncestor(button);
									if (window != null) {
										window.setVisible(false);
										window.dispose();
									}
									validatePrimaryKeys();
								}
							});

							UIUtil.validatePrimaryKeysPending = true;
							UIUtil.showException(progressTable, "Warning", new RuntimeException(message), UIUtil.EXCEPTION_CONTEXT_USER_WARNING, button);
						}
					}
				}
			});
		}
	}

	protected abstract void validatePrimaryKeys();

	private boolean warned = false;

	private boolean confirmInsert() {
		StringBuilder sb = new StringBuilder();
		int linelength = 0;
		for (String targetSchema: targetSchemaSet) {
			if (sb.length() > 0) {
				sb.append(", ");
				if (linelength > 40) {
					sb.append("\n");
					linelength = 0;
				}
			}
			sb.append(targetSchema);
			linelength += targetSchema.length() + 3;
		}
		return JOptionPane.showConfirmDialog(SwingUtilities.getRoot(progressTable),
				"Insert " + collectedRows + " collected rows into the target schema?\n\n" +
						"Target: " + sb,
				"Export collected rows", JOptionPane.YES_NO_OPTION)
				== JOptionPane.YES_OPTION;
	}

	/**
	 * Export is ready. This might be cancelled.
	 *
	 * @throws CancellationException
	 */
	@Override
	public void prepareExport() throws CancellationException {
		if (confirm) {
			boolean prevStopClock;
			synchronized (this) {
				prevStopClock = stopClock;
				stopClock = true;
			}
			long startTime = System.currentTimeMillis();
			try {
				if (SwingUtilities.isEventDispatchThread()) {
					if (!confirmInsert()) {
						throw new CancellationException();
					}
				}
				final boolean confirmed[] = new boolean[1];
				synchronized (confirmed) {
					confirmed[0] = false;
				}
				try {
					SwingUtilities.invokeAndWait(new Runnable() {
						@Override
						public void run() {
							if (confirmInsert()) {
								synchronized (confirmed) {
									confirmed[0] = true;
								}
							}
						}
					});
				} catch (Exception e) {
					throw new RuntimeException(e);
				}

				synchronized (confirmed) {
					if (!confirmed[0]) {
						throw new CancellationException();
					}
				}
			} finally {
				synchronized (this) {
					timeDelay += System.currentTimeMillis() - startTime;
					stopClock = prevStopClock;
				}
			}
		}
	}

}
