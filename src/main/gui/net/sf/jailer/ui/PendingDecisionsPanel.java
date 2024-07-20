/*
 * Copyright 2007 - 2024 Ralf Wisser.
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
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import javax.swing.DefaultCellEditor;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.RowSorter.SortKey;
import javax.swing.SortOrder;
import javax.swing.SwingConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;
import javax.swing.table.TableModel;
import javax.swing.table.TableRowSorter;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;

/**
 * Pending Decisions Panel.
 *
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public abstract class PendingDecisionsPanel extends javax.swing.JPanel {

	private final DataModel dataModel;
	private final ExtractionModelEditor extractionModelEditor;
	
	private List<Association> atBorder = new ArrayList<Association>();
	private Set<Association> checked = new HashSet<Association>();
	public final Set<String> origDecisionPending;

	/**
	 * Creates new form PendingDecisionsPanel
	 */
	public PendingDecisionsPanel(DataModel dataModel, ExtractionModelEditor extractionModelEditor) {
		this.dataModel = dataModel;
		this.extractionModelEditor = extractionModelEditor;
		this.origDecisionPending = new HashSet<String>(dataModel.decisionPending);
		initComponents(); UIUtil.initComponents(this);

		DefaultTableCellRenderer associationsListItemRenderer = new DefaultTableCellRenderer() {
			final Color BG_SELCOLOR = Colors.Color_115_217_255;
			final Color BG1 = UIUtil.TABLE_BACKGROUND_COLOR_1;
			final Color BG2 = UIUtil.TABLE_BACKGROUND_COLOR_2;

			@Override
			public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected,
					boolean hasFocus, int row, int column) {
				if (value == null) {
					value = "";
				}
				if (column == 4) {
					isSelected = false;
				}
				Component render = super.getTableCellRendererComponent(table, value, isSelected, false, row, column);
				if (value instanceof Boolean) {
					JCheckBox checkBox = new JCheckBox(" ");
					checkBox.setHorizontalAlignment(SwingConstants.CENTER);
					checkBox.setSelected(Boolean.TRUE.equals(value));
					checkBox.setOpaque(true);
					render = checkBox;
				}
				render.setBackground(isSelected ? BG_SELCOLOR : (row % 2 == 0) ? BG1 : BG2);
				render.setForeground(Colors.Color_0_0_0);
				if (render instanceof JLabel) {
					((JLabel) render).setToolTipText(UIUtil.toHTML(String.valueOf(value), 100));
				}
				return render;
			}

			private static final long serialVersionUID = -6057505075587930064L;
		};
		associationsTable.setModel(createAssociationsListModel());
		associationsTable.setDefaultRenderer(Object.class, associationsListItemRenderer);
		associationsTable.setDefaultRenderer(Boolean.class, associationsListItemRenderer);
		associationsTable.getColumnModel().getColumn(4).setCellRenderer(associationsListItemRenderer);

		associationsTable.setAutoCreateRowSorter(true);
		associationsTable.setShowVerticalLines(false);
		associationsTable.setShowHorizontalLines(false);
		
		associationsTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		associationsTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			@Override
			public void valueChanged(ListSelectionEvent e) {
				if (associationsTable.getSelectedRow() >= 0) {
					if (!ignoreSelection) {
						select(atBorder.get(associationsTable.getRowSorter().convertRowIndexToModel(associationsTable.getSelectedRow())));
					}
				}
			}
		});
		
		initRowSorter(associationsTable);

		updateView();
	}

	private boolean activated = false;
	private boolean updatePending = false;
	private boolean ignoreSelection = false;
	
	public boolean isActivated() {
		return activated;
	}

	void updateView() {
		if (updatePending) {
			return;
		}
		updatePending = true;
		UIUtil.invokeLater(new Runnable() {
			@Override
			public void run() {
				try {
					Association selected = null;
					if (associationsTable.getSelectedRow() >= 0) {
						selected = atBorder.get(associationsTable.getRowSorter().convertRowIndexToModel(associationsTable.getSelectedRow()));
					}
					for (Iterator<Association> i = atBorder.iterator(); i.hasNext(); ) {
						if (!checked.contains(i.next())) {
							i.remove();
						}
					}
					Set<Association> atBorderSet = new HashSet<Association>(atBorder);
					Set<Table> closure = extractionModelEditor.getCurrentSubjectClosure();
					for (String a : dataModel.decisionPending) {
						Association association = dataModel.namedAssociations.get(a);
						if (association != null) {
							if (closure.contains(association.source) || closure.contains(association.destination)) {
								if (!atBorderSet.contains(association.reversalAssociation)) {
									atBorder.add(association.reversalAssociation);
									atBorderSet.add(association.reversalAssociation);
								}
							}
						}
					}
					
					Collections.sort(atBorder, new Comparator<Association>() {

						@Override
						public int compare(Association o1, Association o2) {
							int c = dataModel.getDisplayName(o1.source).compareTo(dataModel.getDisplayName(o2.source));
							if (c != 0) {
								return c;
							}
							c = dataModel.getDisplayName(o1.destination).compareTo(dataModel.getDisplayName(o2.destination));
							if (c != 0) {
								return c;
							}
							return o1.getName().compareTo(o2.getName());
						}
					});
					
					List<? extends SortKey> sortKeys = new ArrayList<>(associationsTable.getRowSorter().getSortKeys());
					associationsTable.setModel(createAssociationsListModel());
					try {
						associationsTable.getRowSorter().setSortKeys(sortKeys);
					} catch (Throwable t) {
						// ignore
					}
					selectAssociation(selected);

					adjustTableColumnsWidth(associationsTable);
					
					boolean hasChecked = false;
					for (Association association: atBorder) {
						if (checked.contains(association)) {
							hasChecked = true;
							break;
						}
					}
					
					checkAllButton.setEnabled(!checked.containsAll(atBorder));
					uncheckAllButton.setEnabled(hasChecked);
					clearButton.setEnabled(hasChecked);
					
					if (checked.containsAll(atBorder)) {
						if (activated) {
							deactivate();
							activated = false;
						}
					} else {
						if (!activated) {
							activate();
							activated = true;
						}
					}
				} finally {
					updatePending = false;
				}
			}
		});
	}

	private void initRowSorter(JTable table) {
		TableRowSorter<TableModel> sorter = new TableRowSorter<TableModel>(table.getModel()) {
			@Override
			protected boolean useToString(int column) {
				return false;
			}

			@Override
			public void toggleSortOrder(int column) {
				List<? extends SortKey> sortKeys = getSortKeys();
				if (sortKeys.size() > 0) {
					if (sortKeys.get(0).getSortOrder() == SortOrder.DESCENDING) {
						setSortKeys(null);
						return;
					}
				}
				super.toggleSortOrder(column);
			}
		};
		table.setRowSorter(sorter);
	}

	public void adjustTableColumnsWidth(JTable table) {
		DefaultTableModel dtm = (DefaultTableModel) table.getModel();
		DefaultTableCellRenderer defaultTableCellRenderer = new DefaultTableCellRenderer();
		for (int i = 0; i < table.getColumnCount(); i++) {
			TableColumn column = table.getColumnModel().getColumn(i);
			Component comp = defaultTableCellRenderer.getTableCellRendererComponent(table, column.getHeaderValue(),
					false, false, 0, i);
			int width = 1;
			width = Math.max(width, comp.getPreferredSize().width);

			int line = 0;
			for (; line < table.getRowCount(); ++line) {
				comp = table.getCellRenderer(line, i).getTableCellRendererComponent(table, dtm.getValueAt(line, i),
						false, false, line, i);
				width = Math.max(width, comp.getPreferredSize().width);
			}
			if (i >= 2) {
				width = Math.min(width, 200);
			}
			column.setPreferredWidth(width);
		}
		table.getColumnModel().getColumn(table.getColumnModel().getColumnCount() - 1).setPreferredWidth(120);
	}

	/**
	 * Creates model for associations-list component.
	 * 
	 * @return model for associations-list component
	 */
	private TableModel createAssociationsListModel() {
		DefaultTableModel associationsTableModel = new DefaultTableModel(
				new Object[] { "From", "To", "Condition", "Name", "Checked" }, 0) {
			@Override
			public boolean isCellEditable(int row, int column) {
				return column == 4;
			}
			@Override
            public Class<?> getColumnClass(int columnIndex) {
                if(columnIndex == 4) {
                    return Boolean.class;
                }
                return super.getColumnClass(columnIndex);
            }
			@Override
			public void setValueAt(Object aValue, int row, int column) {
				super.setValueAt(aValue, row, column);
				final Association association = atBorder.get(row);
				if (Boolean.TRUE.equals(aValue)) {
					decisionMade(association);
				} else {
					decisionUndone(association);
				}
			}
		};
		
		final JCheckBox checkBox = new JCheckBox(" ");
		checkBox.setOpaque(true);
		checkBox.setHorizontalAlignment(SwingConstants.CENTER);
		DefaultCellEditor anEditor = new DefaultCellEditor(checkBox);
		anEditor.setClickCountToStart(1);
		associationsTable.setDefaultEditor(Boolean.class, anEditor);
		
		for (Association a: atBorder) {
			String name = a.reversalAssociation.getName();
			associationsTableModel.addRow(
					new Object[] { 
							dataModel.getDisplayName(a.source),
							dataModel.getDisplayName(a.destination),
							a.getUnrestrictedJoinCondition(),
							name,
							checked.contains(a) });
		}
		return associationsTableModel;
	}

	private void select(Association association) {
		if (!extractionModelEditor.select(association)) {
			extractionModelEditor.setRootSelection(association.destination);
			extractionModelEditor.select(association);
		}
	}

	/**
	 * Removes an association from "pendingDecisions" list.
	 * 
	 * @param association the association
	 */
	public void decisionMade(Association association) {
		checked.add(association);
		dataModel.decisionPending.remove(association.reversalAssociation.getName());
		decisionMade = true;
		markDirty();
		updateView();
	}

	/**
	 * Adds an association to "pendingDecisions" list.
	 * 
	 * @param association the association
	 */
	public void decisionUndone(Association association) {
		dataModel.decisionPending.add(association.reversalAssociation.getName());
		checked.remove(association);
		
		markDirty();
		updateView();
	}

	protected boolean decisionMade = false;

	protected abstract void toggleDockingState();
	protected abstract void activate();
	protected abstract void deactivate();
	protected abstract void markDirty();

	/**
	 * This method is called from within the constructor to initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is always
	 * regenerated by the Form Editor.
	 */
	// <editor-fold defaultstate="collapsed" desc="Generated
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jPanel1 = new javax.swing.JPanel();
        jScrollPane2 = new javax.swing.JScrollPane();
        associationsTable = new javax.swing.JTable();
        jPanel2 = new javax.swing.JPanel();
        dockButton = new javax.swing.JButton();
        checkAllButton = new javax.swing.JButton();
        uncheckAllButton = new javax.swing.JButton();
        clearButton = new javax.swing.JButton();
        infoPanel = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();

        setLayout(new java.awt.GridLayout(1, 0));

        jPanel1.setLayout(new java.awt.GridBagLayout());

        associationsTable.setAutoCreateRowSorter(true);
        associationsTable.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null}
            },
            new String [] {
                "Title 1", "Title 2", "Title 3", "Title 4"
            }
        ));
        jScrollPane2.setViewportView(associationsTable);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel1.add(jScrollPane2, gridBagConstraints);

        jPanel2.setLayout(new java.awt.GridBagLayout());

        dockButton.setText("Dock");
        dockButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                dockButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
        gridBagConstraints.weightx = 1.0;
        jPanel2.add(dockButton, gridBagConstraints);

        checkAllButton.setText("Check all");
        checkAllButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                checkAllButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel2.add(checkAllButton, gridBagConstraints);

        uncheckAllButton.setText("Uncheck all");
        uncheckAllButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                uncheckAllButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel2.add(uncheckAllButton, gridBagConstraints);

        clearButton.setText("Clear");
        clearButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                clearButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 14;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel2.add(clearButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 0);
        jPanel1.add(jPanel2, gridBagConstraints);

        infoPanel.setBackground(/* Renaming also in *.form! */ Colors.Color_255_255_250);
        infoPanel.setLayout(new java.awt.GridBagLayout());

        jLabel1.setForeground(/* Renaming also in *.form! */ Colors.Color_0_0_62);
        jLabel1.setText("<html>\nThis tool allows you to find and edit the newly added associations if the data model has been extended after the last change to this extraction model. <br>\nSelect associations here in the table and define restrictions, or check off an association if you're comfortable with the given restriction.\n</html>");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        infoPanel.add(jLabel1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanel1.add(infoPanel, gridBagConstraints);

        add(jPanel1);
    }// </editor-fold>//GEN-END:initComponents

    private void dockButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_dockButtonActionPerformed
        toggleDockingState();
    }//GEN-LAST:event_dockButtonActionPerformed

    private void checkAllButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_checkAllButtonActionPerformed
        List<Association> associations = new ArrayList<Association>();
    	for (Association association: atBorder) {
        	if (!checked.contains(association)) {
        		associations.add(association);
        	}
        }
    	for (Association association: associations) {
    		decisionMade(association);
    	}
    }//GEN-LAST:event_checkAllButtonActionPerformed

    private void uncheckAllButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_uncheckAllButtonActionPerformed
        List<Association> associations = new ArrayList<Association>();
        Set<Association> atBorderSet = new HashSet<Association>(atBorder);
        for (Association association: checked) {
        	if (atBorderSet.contains(association)) {
        		associations.add(association);
        	}
        }
    	for (Association association: associations) {
    		decisionUndone(association);
    	}
    }//GEN-LAST:event_uncheckAllButtonActionPerformed

    private void clearButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_clearButtonActionPerformed
        checked.clear();
        updateView();
    }//GEN-LAST:event_clearButtonActionPerformed

    public void selectAssociation(Association selected) {
		if (selected != null) {
			int i = atBorder.indexOf(selected);
			if (i < 0) {
				i = atBorder.indexOf(selected.reversalAssociation);
			}
			if (i >= 0) {
				try {
					ignoreSelection = true;
					i = associationsTable.getRowSorter().convertRowIndexToView(i);
					associationsTable.getSelectionModel().setSelectionInterval(i, i);
				} finally {
					ignoreSelection = false;
				}
			}
		}
	}
    
    public boolean isChecked(Association association) {
    	return checked.contains(association);
    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JTable associationsTable;
    protected javax.swing.JButton checkAllButton;
    protected javax.swing.JButton clearButton;
    protected javax.swing.JButton dockButton;
    protected javax.swing.JPanel infoPanel;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JScrollPane jScrollPane2;
    protected javax.swing.JButton uncheckAllButton;
    // End of variables declaration//GEN-END:variables

}
