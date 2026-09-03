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
package net.sf.jailer.ui;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Consumer;

import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.SwingUtilities;
import javax.swing.Timer;

import net.sf.jailer.database.DMLTransformer;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.entitygraph.RowOriginStep;
import net.sf.jailer.ui.progress.CollectionAnalysis;
import net.sf.jailer.ui.progress.CollectionAnalysisPanel;
import net.sf.jailer.ui.progress.RowOriginPath;

/**
 * Progress panel.
 *
 * @author Ralf Wisser
 */
public class ProgressPanel extends javax.swing.JPanel {
	private Font font = new JLabel("normal").getFont();
	private Font nonbold = font.deriveFont(font.getStyle() & ~Font.BOLD, font.getSize());
	private Font nonboldbig = font.deriveFont(font.getStyle() & ~Font.BOLD, (font.getSize() * 14) / 10);
	private final ProgressTable progressTable;
	private final ProgressTable deleteProgressTable;
	private final CollectionAnalysisPanel analysisPanel;

	/**
	 * Creates new form ProgressPanel.
	 *
	 * @param progressTable the progress table for the export phase
	 * @param deleteProgressTable the progress table for the delete reduction phase
	 * @param withDelete if <code>true</code>, the delete reduction tab is shown
	 */
	public ProgressPanel(ProgressTable progressTable, ProgressTable deleteProgressTable, boolean withDelete) {
		this.progressTable = progressTable;
		this.deleteProgressTable = deleteProgressTable;
		initComponents(); UIUtil.initComponents(this);
		jLabel1.setForeground(jLabel1.getBackground());
		progressTableHolder.setViewportView(progressTable);
		progressTableHolderForDelete.setViewportView(deleteProgressTable);
		stepLabel.setFont(nonboldbig);
		exportedRowsLabel.setFont(nonbold);
		collectedRowsLabel.setFont(nonbold);
		elapsedTimeLabel.setFont(nonbold);
		progressTableHolder.setColumnHeaderView(null);
		progressTableHolderForDelete.setColumnHeaderView(null);
		if (!withDelete) {
			progressTable.setShowExcludeFromDeletionImage(false);
			deletedRowsLabel.setVisible(false);
			deletedRowsTitelLabel.setVisible(false);
			// the tabbed pane is kept even without the delete tab, as it holds the analysis tab
			jTabbedPane1.remove(panel4);
		}
		analysisPanel = new CollectionAnalysisPanel();
		initCellPathAction();
		UIUtil.initComponents(analysisPanel);
		jTabbedPane1.addTab("Analysis", analysisPanel);
		jTabbedPane1.setToolTipTextAt(jTabbedPane1.indexOfComponent(analysisPanel),
				"Which association is responsible for how many rows of the subset");
		stepLabelColor = stepLabel.getForeground();
		initialStepLabelColor = stepLabelColor;
		stepLabel.addPropertyChangeListener("text", new PropertyChangeListener() {
			@Override
			public void propertyChange(PropertyChangeEvent evt) {
				onNewStep();
			}
		});
	}

	private Map<String, JLabel> reductionLabels = new HashMap<String, JLabel>();

	/**
	 * Updates the row reduction counts displayed per table.
	 *
	 * @param rowsReductionPerTable map from table name to the number of rows reduced
	 */
	public void updateRowsReductionPerTable(Map<String, Long> rowsReductionPerTable) {
		for (Entry<String, Long> e: rowsReductionPerTable.entrySet()) {
			JLabel label = reductionLabels.get(e.getKey());
			if (label != null) {
				label.setText(" (-" + e.getValue() + ") ");
			}
		}
	}

	/**
	 * Updates the collected row counts displayed per table.
	 *
	 * @param rowsPerTable map from table name to the number of collected rows
	 */
	public void updateRowsPerTable(Map<String, Long> rowsPerTable) {
		rowsPerTablePanel.removeAll();
		allMouseListener.clear();
		int y = 0;

		GridBagConstraints gridBagConstraints;
		for (String tableName: rowsPerTable.keySet()) {
			Color bgColor;
			if (y % 2 == 0) {
				bgColor = UIUtil.TABLE_BACKGROUND_COLOR_1;
			} else {
				bgColor = UIUtil.TABLE_BACKGROUND_COLOR_2;
			}
			JLabel l = createLabel(y, tableName, bgColor);
			l.setText(" " + tableName + " ");
			l.setOpaque(true);
			l.setFont(nonbold);
			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.gridx = 1;
			gridBagConstraints.gridy = y;
			gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
//            gridBagConstraints.insets = new Insets(2, 0, 2, 0);
			rowsPerTablePanel.add(l, gridBagConstraints);

			l = new JLabel("" + UIUtil.format(rowsPerTable.get(tableName)) + "  ");
			l.setBackground(bgColor);
			l.setOpaque(true);
			l.setFont(nonbold);
			l.setHorizontalAlignment(javax.swing.SwingConstants.RIGHT);
			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.gridx = 2;
			gridBagConstraints.gridy = y;
			gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
			gridBagConstraints.weightx = 1.0;
//            gridBagConstraints.insets = new Insets(2, 0, 2, 0);
			rowsPerTablePanel.add(l, gridBagConstraints);

			l = new JLabel(" ");
			reductionLabels.put(tableName, l);
			if (y % 2 == 0) {
				l.setBackground(Colors.Color_240_255_255);
			} else {
				l.setBackground(Colors.Color_255_255_255);
			}
			l.setOpaque(true);
			l.setFont(nonbold);
			l.setHorizontalAlignment(javax.swing.SwingConstants.RIGHT);
			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.gridx = 3;
			gridBagConstraints.gridy = y;
			gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
			gridBagConstraints.weightx = 0;
//            gridBagConstraints.insets = new Insets(2, 0, 2, 0);
			rowsPerTablePanel.add(l, gridBagConstraints);

			++y;
		}
		JLabel l = new JLabel("");
		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 1;
		gridBagConstraints.gridy = y;
		gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
		gridBagConstraints.weighty = 1.0;
		rowsPerTablePanel.add(l, gridBagConstraints);

		rowsPerTablePanel.repaint();
	}

	private int currentlySelectedRow = -1;
	private final Color BGCOLOR_OF_SELECTED_ROW = Colors.Color_0_255_255;
	private List<MouseListener> allMouseListener = new ArrayList<MouseListener>();

	private JLabel createLabel(final int y, final String tableName, Color bgColor) {
		final JLabel label = new JLabel();
		label.setBackground(bgColor);
		MouseListener l;
		label.addMouseListener(l = new MouseListener() {
			Color bgColor;
			@Override
			public void mouseReleased(MouseEvent e) {
			}
			@Override
			public void mousePressed(MouseEvent e) {
			}
			@Override
			public void mouseExited(MouseEvent e) {
				if (bgColor != null) {
					label.setBackground(bgColor);
				}
				if (currentlySelectedRow == y) {
					currentlySelectedRow = -1;
				}
			}
			@Override
			public void mouseEntered(MouseEvent e) {
				for (MouseListener l: allMouseListener) {
					if (l != this) {
						l.mouseExited(e);
					}
				}
				if (bgColor == null) {
					bgColor = label.getBackground();
				}
				label.setBackground(BGCOLOR_OF_SELECTED_ROW);
				currentlySelectedRow = y;
			}
			@Override
			public void mouseClicked(MouseEvent e) {
				progressTable.selectAllCells(tableName);
				deleteProgressTable.selectAllCells(tableName);
			}
		});
		if (y == currentlySelectedRow) {
			l.mouseEntered(null);
		}
		allMouseListener.add(l);
		return label;
	}

	/**
	 * Switches the view to the delete reduction tab.
	 */
	public void switchToDeleteTab() {
		jTabbedPane1.setSelectedIndex(1);
	}

	/**
	 * Updates the collection analysis, which shows the number of collected rows per association.
	 *
	 * @param analysis the analysis
	 * @param dataModel the data model, for display names
	 */
	public void updateAnalysis(CollectionAnalysis analysis, DataModel dataModel) {
		analysisPanel.setAnalysis(analysis, dataModel);
	}

	/**
	 * Sets the consumer which selects an association in the extraction model editor.
	 * If none is set, the analysis does not offer to jump to an association.
	 *
	 * @param associationSelector the consumer, or <code>null</code>
	 */
	public void setAssociationSelector(Consumer<Association> associationSelector) {
		analysisPanel.setAssociationSelector(associationSelector);
	}

	/**
	 * Sets the consumer which lays the way of a row into the subset out in a Data Browser.
	 * If none is set, the chain is shown as a table only.
	 *
	 * @param pathOpener the consumer, or <code>null</code>
	 */
	public void setPathOpener(Consumer<List<RowOriginStep>> pathOpener) {
		analysisPanel.setPathOpener(pathOpener);
	}

	/**
	 * Sets the consumer which opens a table in a Data Browser.
	 * If none is set, the analysis does not offer to open one.
	 *
	 * @param tableOpener the consumer, or <code>null</code>
	 */
	public void setTableOpener(Consumer<Table> tableOpener) {
		analysisPanel.setTableOpener(tableOpener);
	}

	/**
	 * Sets the consumer which lays a branching path out in a Data Browser. Only if one is set, a
	 * cell of the progress table offers to open its way to a subject.
	 *
	 * @param cellPathOpener the consumer, or <code>null</code>
	 */
	public void setCellPathOpener(Consumer<List<RowOriginPath.Step>> cellPathOpener) {
		this.cellPathOpener = cellPathOpener;
	}

	private Consumer<List<RowOriginPath.Step>> cellPathOpener;

	private static final String CELL_PATH_TITLE = "Open Path to Subject";
	private static final String CELL_PATH_TOOLTIP = "Opens the way of these rows to a subject as table browsers: one per step, each showing exactly the rows collected in it. Where several associations have brought rows into a step, the chain follows the one which contributed most and shows the others beside it.";
	private static final String CELL_PATH_NO_GRAPH_TOOLTIP = "Requires a run which keeps its collected rows: switch on \"Enable row origin analysis by keeping the collected rows\" in the export dialog.";

	/**
	 * Offers the way to a subject on the cells of the progress table, through the context menu and
	 * through a double click.
	 */
	private void initCellPathAction() {
		MouseAdapter listener = new MouseAdapter() {
			@Override
			public void mousePressed(java.awt.event.MouseEvent e) {
				maybeShowPopup(e);
			}
			@Override
			public void mouseReleased(java.awt.event.MouseEvent e) {
				maybeShowPopup(e);
			}
			@Override
			public void mouseClicked(java.awt.event.MouseEvent e) {
				if (e.getClickCount() == 2 && !e.isPopupTrigger()) {
					openPath(progressTable.cellAtPoint(e.getPoint()));
				}
			}
			private void maybeShowPopup(java.awt.event.MouseEvent e) {
				if (!e.isPopupTrigger()) {
					return;
				}
				final ProgressTable.CellInfo cell = progressTable.cellAtPoint(e.getPoint());
				if (cell == null) {
					return;
				}
				boolean available = cellPathOpener != null && analysisPanel.hasRetainedRows();
				JPopupMenu popup = new JPopupMenu();
				JMenuItem item = new JMenuItem(CELL_PATH_TITLE);
				item.setToolTipText(available? CELL_PATH_TOOLTIP : CELL_PATH_NO_GRAPH_TOOLTIP);
				item.setEnabled(available);
				item.addActionListener(new java.awt.event.ActionListener() {
					@Override
					public void actionPerformed(java.awt.event.ActionEvent evt) {
						openPath(cell);
					}
				});
				popup.add(item);
				popup.show(progressTable, e.getX(), e.getY());
			}
			private void openPath(ProgressTable.CellInfo cell) {
				if (cell == null || cellPathOpener == null || !analysisPanel.hasRetainedRows()) {
					return;
				}
				// the cursor first, the work only in the next event: laying the chain out runs on
				// the event dispatch thread from beginning to end - opening the Data Browser,
				// rebuilding the desktop, adding the browsers - and a cursor set right before that
				// would not be painted any more
				final List<Component> waiting = waitCursorComponents();
				for (Component component: waiting) {
					UIUtil.setWaitCursor(component);
				}
				UIUtil.invokeLater(new Runnable() {
					@Override
					public void run() {
						try {
							List<RowOriginPath.Step> path = analysisPanel.buildPathFromCell(cell.tableName, cell.day);
							if (path != null && !path.isEmpty()) {
								cellPathOpener.accept(path);
							}
						} finally {
							for (Component component: waiting) {
								UIUtil.resetWaitCursor(component);
							}
						}
					}
				});
			}
		};
		progressTable.addMouseListener(listener);
	}

	/**
	 * The components which are to show the wait cursor while the way of a cell to a subject is
	 * being prepared and laid out: this window, the "Export" tab and the progress table in it. Each
	 * of them separately, since a component with a cursor of its own does not inherit the one of
	 * its window.
	 *
	 * @return the components, without the ones which are not there
	 */
	private List<Component> waitCursorComponents() {
		List<Component> components = new ArrayList<Component>();
		for (Component component: new Component[] {
				SwingUtilities.getWindowAncestor(this), panel3, progressTableHolder, progressTable }) {
			if (component != null) {
				components.add(component);
			}
		}
		return components;
	}

	/**
	 * Sets the context for the row origin analysis, and the action which discards the retained
	 * data. Only if one is set and an entity-graph has been retained, the collected rows of an
	 * association can be inspected.
	 *
	 * @param rowOriginContext the context, or <code>null</code>
	 * @param discardAction discards the retained data at once, or <code>null</code>
	 * @param discardOnCloseAction discards the retained data as soon as nothing looks at it any
	 *        more, used when this window is closed, or <code>null</code>
	 */
	public void setRowOriginContext(net.sf.jailer.ui.progress.RowOriginContext rowOriginContext, final Runnable discardAction, final Runnable discardOnCloseAction) {
		analysisPanel.setRowOriginContext(rowOriginContext, discardAction);
		if (rowOriginContext != null && discardOnCloseAction != null) {
			// the retained rows live as long as this window, or as long as a view of them
			addAncestorListener(new javax.swing.event.AncestorListener() {
				@Override
				public void ancestorAdded(javax.swing.event.AncestorEvent event) {
					Window window = SwingUtilities.getWindowAncestor(ProgressPanel.this);
					if (window != null) {
						removeAncestorListener(this);
						window.addWindowListener(new java.awt.event.WindowAdapter() {
							@Override
							public void windowClosed(java.awt.event.WindowEvent e) {
								discardOnCloseAction.run();
							}
						});
					}
				}
				@Override
				public void ancestorRemoved(javax.swing.event.AncestorEvent event) {
				}
				@Override
				public void ancestorMoved(javax.swing.event.AncestorEvent event) {
				}
			});
		}
	}

	/** This method is called from within the constructor to
	 * initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is
	 * always regenerated by the Form Editor.
	 */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jSplitPane1 = new javax.swing.JSplitPane();
        jPanel3 = new javax.swing.JPanel();
        jPanel2 = new javax.swing.JPanel();
        jLabel3 = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();
        stepLabel = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        collectedRowsLabel = new javax.swing.JLabel();
        exportedRowsLabel = new javax.swing.JLabel();
        jScrollPane1 = new javax.swing.JScrollPane();
        rowsPerTablePanel = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        jLabel6 = new javax.swing.JLabel();
        elapsedTimeLabel = new javax.swing.JLabel();
        deletedRowsTitelLabel = new javax.swing.JLabel();
        deletedRowsLabel = new javax.swing.JLabel();
        jLabel10 = new javax.swing.JLabel();
        jPanel4 = new javax.swing.JPanel();
        jTabbedPane1 = new javax.swing.JTabbedPane();
        panel3 = new javax.swing.JPanel();
        jPanel6 = new javax.swing.JPanel();
        jLabel2 = new javax.swing.JLabel();
        jLabel7 = new javax.swing.JLabel();
        progressTableHolder = new javax.swing.JScrollPane();
        panel4 = new javax.swing.JPanel();
        jPanel7 = new javax.swing.JPanel();
        jLabel8 = new javax.swing.JLabel();
        jLabel9 = new javax.swing.JLabel();
        progressTableHolderForDelete = new javax.swing.JScrollPane();

        setLayout(new java.awt.GridLayout(1, 0));

        jSplitPane1.setOneTouchExpandable(true);

        jPanel3.setLayout(new javax.swing.BoxLayout(jPanel3, javax.swing.BoxLayout.LINE_AXIS));

        jPanel2.setLayout(new java.awt.GridBagLayout());

        jLabel3.setFont(jLabel3.getFont().deriveFont(jLabel3.getFont().getStyle() & ~java.awt.Font.BOLD, jLabel3.getFont().getSize()+2));
        jLabel3.setText("Stage ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 0, 0);
        jPanel2.add(jLabel3, gridBagConstraints);

        jLabel4.setFont(jLabel4.getFont().deriveFont(jLabel4.getFont().getStyle() & ~java.awt.Font.BOLD, jLabel4.getFont().getSize()+2));
        jLabel4.setText("Collected Rows  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(12, 4, 0, 0);
        jPanel2.add(jLabel4, gridBagConstraints);

        stepLabel.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        jPanel2.add(stepLabel, gridBagConstraints);

        jLabel5.setFont(jLabel5.getFont().deriveFont(jLabel5.getFont().getStyle() & ~java.awt.Font.BOLD, jLabel5.getFont().getSize()+2));
        jLabel5.setText("Exported Rows  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 0, 0);
        jPanel2.add(jLabel5, gridBagConstraints);

        collectedRowsLabel.setText("0");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(12, 0, 0, 0);
        jPanel2.add(collectedRowsLabel, gridBagConstraints);

        exportedRowsLabel.setText("0");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        jPanel2.add(exportedRowsLabel, gridBagConstraints);

        rowsPerTablePanel.setLayout(new java.awt.GridBagLayout());
        jScrollPane1.setViewportView(rowsPerTablePanel);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 0);
        jPanel2.add(jScrollPane1, gridBagConstraints);

        jLabel1.setForeground(/* Renaming also in *.form! */ Colors.Color_230_230_230);
        jLabel1.setText("                                                     ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 11;
        jPanel2.add(jLabel1, gridBagConstraints);

        jLabel6.setFont(jLabel6.getFont().deriveFont(jLabel6.getFont().getStyle() & ~java.awt.Font.BOLD, jLabel6.getFont().getSize()+2));
        jLabel6.setText("Elapsed Time ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 0, 0);
        jPanel2.add(jLabel6, gridBagConstraints);

        elapsedTimeLabel.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        jPanel2.add(elapsedTimeLabel, gridBagConstraints);

        deletedRowsTitelLabel.setFont(deletedRowsTitelLabel.getFont().deriveFont(deletedRowsTitelLabel.getFont().getStyle() & ~java.awt.Font.BOLD, deletedRowsTitelLabel.getFont().getSize()+2));
        deletedRowsTitelLabel.setText("Deleted Rows  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 0, 0);
        jPanel2.add(deletedRowsTitelLabel, gridBagConstraints);

        deletedRowsLabel.setText("0");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        jPanel2.add(deletedRowsLabel, gridBagConstraints);

        jLabel10.setFont(jLabel10.getFont().deriveFont(jLabel10.getFont().getStyle() | java.awt.Font.BOLD));
        jLabel10.setText("Rows per Table");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 19;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 0);
        jPanel2.add(jLabel10, gridBagConstraints);

        jPanel3.add(jPanel2);

        jSplitPane1.setLeftComponent(jPanel3);

        jPanel4.setLayout(new javax.swing.BoxLayout(jPanel4, javax.swing.BoxLayout.LINE_AXIS));

        panel3.setLayout(new java.awt.BorderLayout());

        jPanel6.setLayout(new java.awt.GridBagLayout());

        jLabel2.setText(" Day ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 2, 0);
        jPanel6.add(jLabel2, gridBagConstraints);

        jLabel7.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
        jLabel7.setText(" Progress ");
        jLabel7.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 2, 0);
        jPanel6.add(jLabel7, gridBagConstraints);

        panel3.add(jPanel6, java.awt.BorderLayout.PAGE_START);
        panel3.add(progressTableHolder, java.awt.BorderLayout.CENTER);

        jTabbedPane1.addTab("Export", panel3);

        panel4.setLayout(new java.awt.BorderLayout());

        jPanel7.setLayout(new java.awt.GridBagLayout());

        jLabel8.setText(" Day ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 2, 0);
        jPanel7.add(jLabel8, gridBagConstraints);

        jLabel9.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
        jLabel9.setText(" Progress ");
        jLabel9.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 2, 0);
        jPanel7.add(jLabel9, gridBagConstraints);

        panel4.add(jPanel7, java.awt.BorderLayout.PAGE_START);
        panel4.add(progressTableHolderForDelete, java.awt.BorderLayout.CENTER);

        jTabbedPane1.addTab("Delete Reduction", panel4);

        jPanel4.add(jTabbedPane1);

        jSplitPane1.setRightComponent(jPanel4);

        add(jSplitPane1);
    }// </editor-fold>//GEN-END:initComponents

	/**
	 * Shows a confirmation dialog indicating successful completion.
	 */
	public void confirm() {
		String message;
		message = "Successfully completed.";
		if (DMLTransformer.numberOfExportedLOBs.get() > 0) {
			message += "\n" + DMLTransformer.numberOfExportedLOBs.get() + " CLOBs/BLOBs exported.\n\n" +
					   "Note that the CLOBs/BLOBs can only\n" +
					   "be imported with the 'Import SQL Data' Tool";
		}
		Window owner = SwingUtilities.getWindowAncestor(this);
		if (JailerConsole.openResultActions.get(owner) != null) {
			int option = JOptionPane.showOptionDialog(this, message, "Finished", JOptionPane.DEFAULT_OPTION, JOptionPane.INFORMATION_MESSAGE,
					null, new Object[] { "OK", "Open Result", "Open Result and close this" }, null);
			if (option == 1) {
				JailerConsole.openResultActions.get(owner).accept(owner);
			}
			if (option == 2) {
				JailerConsole.openResultActions.get(owner).accept(owner);
				Window window = SwingUtilities.getWindowAncestor(this);
				window.setVisible(false);
				window.dispose();
				
			}
		} else {
			JOptionPane.showMessageDialog(this, message, "Finished", JOptionPane.INFORMATION_MESSAGE);
		}
	}

	/**
	 * Updates the UI to indicate that a cancellation is in progress.
	 */
	public void onCancel() {
		inCancellingStep = true;
		stepLabel.setText("cancelling...");
		setStepLabelForeground(Colors.Color_255_0_0);
    }

	public boolean inCancellingStep = false;

    // Variables declaration - do not modify//GEN-BEGIN:variables
    public javax.swing.JLabel collectedRowsLabel;
    public javax.swing.JLabel deletedRowsLabel;
    private javax.swing.JLabel deletedRowsTitelLabel;
    public javax.swing.JLabel elapsedTimeLabel;
    public javax.swing.JLabel exportedRowsLabel;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel10;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JPanel jPanel7;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JSplitPane jSplitPane1;
    private javax.swing.JTabbedPane jTabbedPane1;
    private javax.swing.JPanel panel3;
    private javax.swing.JPanel panel4;
    private javax.swing.JScrollPane progressTableHolder;
    private javax.swing.JScrollPane progressTableHolderForDelete;
    private javax.swing.JPanel rowsPerTablePanel;
    public javax.swing.JLabel stepLabel;
    // End of variables declaration//GEN-END:variables

	protected void onNewStep() {
		if (timer == null && stepLabel.getText().endsWith("...")) {
			startTimer();
		}
	}

    private Timer timer;
    private boolean isOn;
    private Color stepLabelColor;
    private Color initialStepLabelColor;

    /**
     * Sets the foreground color of the step label.
     *
     * @param color the foreground color to set
     */
    public void setStepLabelForeground(Color color) {
    	stepLabel.setForeground(color);
    	stepLabelColor = color;
	}

    private void startTimer() {
    	Window window = SwingUtilities.getWindowAncestor(this);
    	if (window == null || !window.isVisible()) {
    		return;
    	}
		timer = new Timer(500, new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				timer = null;
				if (isOn) {
					isOn = false;
					if (stepLabel.getText().endsWith("...")) {
						stepLabel.setForeground(initialStepLabelColor);
						startTimer();
					} else {
						stepLabel.setForeground(stepLabelColor);
					}
				} else {
					stepLabel.setForeground(Colors.Color_255_0_0);
					isOn = true;
					startTimer();
				}
			}
		});
		timer.setRepeats(false);
		timer.start();
	}

	private static final long serialVersionUID = -2750282839722695036L;
}
