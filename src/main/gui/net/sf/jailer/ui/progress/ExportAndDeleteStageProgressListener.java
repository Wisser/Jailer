/*
 * Copyright 2007 - 2020 Ralf Wisser.
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

import java.util.Set;

import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.ModelElement;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.progress.ProgressListener;
import net.sf.jailer.ui.ProgressPanel;
import net.sf.jailer.ui.ProgressTable;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.util.CancellationException;

/**
 * Keeps progress indicators up to date.
 *
 * @author Ralf Wisser
 */
public abstract class ExportAndDeleteStageProgressListener implements ProgressListener {

	private final SingleStageProgressListener exportProgressListener;
	private final SingleStageProgressListener deleteProgressListener;
	private SingleStageProgressListener currentProgressListener;
	private final ProgressPanel progressPanel;

	/**
	 * Constructor.
	 *
	 * @param progressTable
	 *            table showing collected rows
	 * @param targetSchemaSet
	 */
	public ExportAndDeleteStageProgressListener(final ProgressTable exportProgressTable, final ProgressTable deleteProgressTable, final ProgressPanel progressPanel, DataModel dataModel, final boolean confirm, Set<String> targetSchemaSet, boolean checkPK) {
		this.exportProgressListener = new SingleStageProgressListener(exportProgressTable, progressPanel, dataModel, confirm, targetSchemaSet, true, checkPK) {
			@Override
			protected void validatePrimaryKeys() {
				ExportAndDeleteStageProgressListener.this.validatePrimaryKeys();
			}
		};
		this.deleteProgressListener = new SingleStageProgressListener(deleteProgressTable, progressPanel, dataModel, confirm, targetSchemaSet, false, checkPK) {
			@Override
			protected void validatePrimaryKeys() {
				ExportAndDeleteStageProgressListener.this.validatePrimaryKeys();
			}
		};
		this.progressPanel = progressPanel;
		currentProgressListener = exportProgressListener;
	}

	@Override
	public void collectionJobEnqueued(int day, ModelElement modelElement) {
		currentProgressListener.collectionJobEnqueued(day, modelElement);
		switchToDeleteTab();
	}

	@Override
	public void collectionJobStarted(int day, ModelElement modelElement) {
		currentProgressListener.collectionJobStarted(day, modelElement);
		switchToDeleteTab();
	}

	@Override
	public void collected(int day, ModelElement modelElement, long rc) {
		currentProgressListener.collected(day, modelElement, rc);
		switchToDeleteTab();
	}

	@Override
	public void exported(Table table, long rc) {
		currentProgressListener.exported(table, rc);
	}

	@Override
	public synchronized void newStage(String stage, boolean isErrorStage, boolean isFinalStage) {
		exportProgressListener.newStage(stage, isErrorStage, isFinalStage);
		deleteProgressListener.newStage(stage, isErrorStage, isFinalStage);
		if ("delete".equals(stage)) {
			currentProgressListener = deleteProgressListener;
		}
	}

	boolean switched = false;

	private synchronized void switchToDeleteTab() {
		if (!switched && currentProgressListener == deleteProgressListener) {
			switched = true;
			UIUtil.invokeLater(new Runnable() {
				@Override
				public void run() {
					progressPanel.switchToDeleteTab();
				}
			});
		}
	}

	@Override
	public void prepareExport() throws CancellationException {
		currentProgressListener.prepareExport();
	}

	public void stop() {
		exportProgressListener.stop();
		deleteProgressListener.stop();
	}

	protected abstract void validatePrimaryKeys();

}
