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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.ModelElement;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.progress.ProgressListener;
import net.sf.jailer.util.CancellationException;

/**
 * Count the number of collected (and deleted) rows.
 *
 * @author Ralf Wisser
 */
public class CollectedRowsCounter implements ProgressListener {

	private boolean inDeleteStage = false;
	private Map<Table, Long> collectedRowsCount = new HashMap<Table, Long>();
	private Map<Table, Long> deletedRowsCount = new HashMap<Table, Long>();

	public Map<Table, Long> getCollectedRowsCount() {
		return collectedRowsCount;
	}

	public Map<Table, Long> getDeletedRowsCount() {
		return deletedRowsCount;
	}

	/**
	 * Creates the statistic.
	 *
	 * @return the statistic
	 */
	public List<String> createStatistic(boolean forDelete, final DataModel datamodel, ExportStatistic exportStatistic) {
		List<String> result = new ArrayList<String>();
		Map<Table, Long> rowsCount = getCollectedRowsCount();
		Map<Table, Long> deletedCount = getDeletedRowsCount();
		Set<Table> tableSet = new HashSet<Table>(rowsCount.keySet());
		if (forDelete) {
			tableSet.addAll(deletedCount.keySet());
		}
		List<Table> tabs = new ArrayList<Table>(tableSet);
		Collections.sort(tabs, new Comparator<Table>() {
			@Override
			public int compare(Table o1, Table o2) {
				return datamodel.getDisplayName(o1).compareTo(datamodel.getDisplayName(o2));
			}
		});

		long sum = 0;
		for (Table stable: tabs) {
			if (rowsCount.containsKey(stable)) {
				sum += rowsCount.get(stable);
			}
			if (forDelete && deletedCount.containsKey(stable)) {
				sum -= deletedCount.get(stable);
			}
		}
		result.add((forDelete? "Deleted Rows:      " : "Exported Rows:     ") + sum);
		Map<Table, Long> exportedRows = new HashMap<Table, Long>();
		long total = 0;
		for (Table stable: tabs) {
			Long rc = rowsCount.get(stable);
			if (rc == null) {
				rc = 0L;
			} else {
				exportedRows.put(stable, rc);
				total += rc;
			}
			String reduct = "";
			if (forDelete && deletedCount.containsKey(stable)) {
				long r = deletedCount.get(stable);
				if (r > 0) {
					reduct = "(" + rc + " - " + r + ")";
					rc -= r;
				}
			}
			result.add(String.format(Locale.ENGLISH, "   %-24s %10d %s", datamodel.getDisplayName(stable), rc, reduct));
		}
		if (exportStatistic != null) {
			exportStatistic.setExportedRows(exportedRows);
			exportStatistic.setTotal(total);
		}
		return result;
	}

	@Override
	public void collected(int day, ModelElement modelElement, long rc) {
		if (rc > 0) {
			Map<Table, Long> rows = inDeleteStage? deletedRowsCount : collectedRowsCount;
			Table table = getDestination(modelElement);
			Long forTable = rows.get(table);
			if (forTable == null) {
				forTable = rc;
			} else {
				forTable += rc;
			}
			rows.put(table, forTable);
		}
	}

	@Override
	public void newStage(String stage, boolean isErrorStage, boolean isFinalStage) {
		if ("delete".equals(stage)) {
			inDeleteStage = true;
		}
	}

	private Table getDestination(ModelElement key) {
		if (key instanceof Association) {
			return ((Association) key).destination;
		}
		return (Table) key;
	}

	@Override
	public void exported(Table table, long rc) {
	}

	@Override
	public void collectionJobEnqueued(int day, ModelElement modelElement) {
	}

	@Override
	public void collectionJobStarted(int day, ModelElement modelElement) {
	}

	@Override
	public void prepareExport() throws CancellationException {
	}

}
