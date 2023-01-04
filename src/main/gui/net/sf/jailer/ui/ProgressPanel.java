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

import java.awt.Color;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import javax.swing.Timer;

import net.sf.jailer.database.DMLTransformer;

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

	/** Creates new form ProgressPanel
	 * @param progressTable */
	public ProgressPanel(ProgressTable progressTable, ProgressTable deleteProgressTable, boolean withDelete) {
		this.progressTable = progressTable;
		this.deleteProgressTable = deleteProgressTable;
		initComponents();
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
			jTabbedPane1.remove(panel4);
			jPanel4.remove(jTabbedPane1);
			jTabbedPane1.remove(panel3);
			jPanel4.add(panel3);
		}
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

	public void updateRowsReductionPerTable(Map<String, Long> rowsReductionPerTable) {
		for (Entry<String, Long> e: rowsReductionPerTable.entrySet()) {
			JLabel label = reductionLabels.get(e.getKey());
			if (label != null) {
				label.setText(" (-" + e.getValue() + ") ");
			}
		}
	}

	public void updateRowsPerTable(Map<String, Long> rowsPerTable) {
		rowsPerTablePanel.removeAll();
		allMouseListener.clear();
		int y = 0;

		GridBagConstraints gridBagConstraints;
		for (String tableName: rowsPerTable.keySet()) {
			Color bgColor;
			if (y % 2 == 0) {
				bgColor = new java.awt.Color(240, 255, 255);
			} else {
				bgColor = Color.WHITE;
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
			if (y % 2 == 0) {
				l.setBackground(new java.awt.Color(240, 255, 255));
			} else {
				l.setBackground(Color.WHITE);
			}
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
				l.setBackground(new java.awt.Color(240, 255, 255));
			} else {
				l.setBackground(Color.WHITE);
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
	private final Color BGCOLOR_OF_SELECTED_ROW = Color.CYAN;
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

	public void switchToDeleteTab() {
		jTabbedPane1.setSelectedIndex(1);
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

        jSplitPane1.setContinuousLayout(true);
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

        jLabel1.setForeground(new java.awt.Color(230, 230, 230));
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

	public void onCancel() {
		inCancellingStep = true;
		stepLabel.setText("cancelling...");
		setStepLabelForeground(Color.RED);
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
					stepLabel.setForeground(Color.red);
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
