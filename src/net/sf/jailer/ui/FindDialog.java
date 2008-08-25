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
import java.awt.Component;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;

/**
 * Dialog for finding tables.
 *
 * @author Ralf Wisser
 */
public class FindDialog extends javax.swing.JDialog {

	/**
	 * Maximum number of tables in a closure-table's line.
	 */
	private final static int MAX_TABLES_PER_LINE = 5;
	
	/**
	 * The extraction model frame.
	 */
	private final ExtractionModelFrame extractionModelFrame;
	
	/**
	 * Currently selected table (in closure-table).
	 */
	private String selectedTable;
	
	/**
	 * Background colors per row.
	 */
	private final List<Color> bgColor = new ArrayList<Color>();
	
    /** Creates new form FindDialog */
    public FindDialog(ExtractionModelFrame extractionModelFrame) {
        super(extractionModelFrame, false);
    	this.extractionModelFrame = extractionModelFrame;
        initComponents();
        final TableCellRenderer defaultTableCellRenderer = closureTable.getDefaultRenderer(String.class);
		closureTable.setDefaultRenderer(Object.class, new TableCellRenderer() {
			public Component getTableCellRendererComponent(JTable table,
					Object value, boolean isSelected, boolean hasFocus,
					int row, int column) {
				isSelected = selectedTable != null && selectedTable.equals(value);
				if (value == null || column < 1 || "".equals(value)) {
					hasFocus = false;
				}
				Component render = defaultTableCellRenderer.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
				if (render instanceof JLabel && !isSelected) {
					((JLabel) render).setBackground(bgColor.get(row));
				}
				return render;
			}
		});
		closureTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		closureTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			@Override
			public void valueChanged(ListSelectionEvent evt) {
				int col = closureTable.getSelectedColumn();
				int row = closureTable.getSelectedRow();
				if (col >= 1 && row >= 0) {
					String displayName = (String) closureTable.getModel().getValueAt(row, col);
					closureTable.getSelectionModel().clearSelection();
					if (displayName != null && !"".equals(displayName)) {
						if (selectedTable == null || !selectedTable.equals(displayName)) {
							selectedTable = displayName;
							repaint();
							Table table = getDataModel().getTableByDisplayName(selectedTable);
							if (table != null) {
								if (!FindDialog.this.extractionModelFrame.extractionModelEditor.select(table)) {
									FindDialog.this.extractionModelFrame.extractionModelEditor.setRoot(table);
								}
							}
						}
					}
				}
			}
		});
		tableSelection.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
            	Table table = getDataModel().getTableByDisplayName((String) tableSelection.getSelectedItem());
				refresh(table);
            }
        });
		setLocation(100, 150);
        setSize(500, 400);
        setAlwaysOnTop(true);
    }
    
    /**
     * Gets current data model.
     * 
     * @return current data model
     */
    private DataModel getDataModel() {
    	return extractionModelFrame.extractionModelEditor.dataModel;
    }

    /**
     * Make {@link #refresh(Table)} reentrant.
     */
    private boolean refreshing = false;
    
    /**
     * Refreshes the dialog.
     * 
     * @param tableToSelect the table to select initially or <code>null</code> to keep the current selection
     */
    public void refresh(Table tableToSelect) {
    	if (refreshing) {
    		return;
    	}
    	refreshing = true;
    	
    	selectedTable = null;
    	
    	// table list model
    	if (tableToSelect == null) {
    		Object currentSelection = tableSelection.getSelectedItem();
    		if (currentSelection instanceof String) {
    			tableToSelect = getDataModel().getTableByDisplayName((String) currentSelection);
    		}
    	}
    	Vector<String> tableNames = new Vector<String>();
    	for (Table table: getDataModel().getTables()) {
    		tableNames.add(getDataModel().getDisplayName(table));
    	}
    	Collections.sort(tableNames);
    	DefaultComboBoxModel model = new DefaultComboBoxModel(tableNames);
        tableSelection.setModel(model);
    	if (tableToSelect != null) {
    		tableSelection.setSelectedItem(getDataModel().getDisplayName(tableToSelect));
    	} else {
    		tableSelection.setSelectedItem(0);
    	}
    	
    	// table model
    	refreshTableModel();

    	refreshing = false;
    }
    
    /**
     * Refreshes the table model.
     */
    private void refreshTableModel() {
    	Table selectedTable = null;
    	Object currentSelection = tableSelection.getSelectedItem();
		if (currentSelection instanceof String) {
			selectedTable = getDataModel().getTableByDisplayName((String) currentSelection);
		}
		
		Object[] columns = new Object[MAX_TABLES_PER_LINE + 1];
		for (int i = 0; i < columns.length; ++i) {
			columns[i] = "";
		}
		columns[0] = "Distance";
		columns[1] = "Table";
		
		List<Object[]> data = new ArrayList<Object[]>();
		
		Set<String> visited = new HashSet<String>();
		List<String> currentLine = new ArrayList<String>();
		if (selectedTable != null) {
			currentLine.add(getDataModel().getDisplayName(selectedTable));
			visited.add(getDataModel().getDisplayName(selectedTable));
		}
		int distance = 0;
		final Color BG1 = new Color(255, 255, 255);
		final Color BG2 = new Color(220, 245, 245);
		bgColor.clear();
		bgColor.add(BG1);
		
		while (!currentLine.isEmpty()) {
			// add current line to table model
			Collections.sort(currentLine);
			Object[] lineAsObjects = new Object[MAX_TABLES_PER_LINE + 1];
			Arrays.fill(lineAsObjects, "");
			int col = 0;
			lineAsObjects[col++] = distance > 0? ("" + distance) : "";
			for (String t: currentLine) {
				if (col < MAX_TABLES_PER_LINE) {
					lineAsObjects[col++] = t;
				} else {
					data.add(lineAsObjects);
					bgColor.add(distance % 2 == 0? BG1 : BG2);
					lineAsObjects = new Object[MAX_TABLES_PER_LINE + 1];
					Arrays.fill(lineAsObjects, "");
					col = 1;
				}
			}
			if (col > 1) {
				data.add(lineAsObjects);
				bgColor.add(distance % 2 != 0? BG1 : BG2);
			}
			
			// get next line
			List<String> nextLine = new ArrayList<String>();
			for (String t: currentLine) {
				Table table = getDataModel().getTableByDisplayName(t);
				if (table != null) {
					for (Association association: table.associations) {
						String displayName = getDataModel().getDisplayName(association.destination);
						if (!visited.contains(displayName) && !association.isIgnored()) {
							nextLine.add(displayName);
							visited.add(displayName);
						}
					}
				}
			}
			currentLine = nextLine;
			++distance;
		}
		
		Object[][] dataArray = (Object[][]) data.toArray(new Object[data.size()][]);
		DefaultTableModel tableModel = new DefaultTableModel(dataArray, columns) {
			public boolean isCellEditable(int row, int column) {
				return false;
			}
    	};
    	closureTable.setModel(tableModel);

		for (int i = 0; i < closureTable.getColumnCount(); i++) {
            TableColumn column = closureTable.getColumnModel().getColumn(i);
            int width = 1;
            
            Component comp = closureTable.getDefaultRenderer(String.class).
            						getTableCellRendererComponent(
            								closureTable, column.getHeaderValue(),
            								false, false, 0, i);
            width = Math.max(width, comp.getPreferredSize().width);

            for (int line = 0; line < dataArray.length; ++line) {
	            comp = closureTable.getDefaultRenderer(String.class).
	                             getTableCellRendererComponent(
	                            		 closureTable, dataArray[line][i],
	                                 false, false, line, i);
	            width = Math.max(width, comp.getPreferredSize().width);
            }
            
            column.setPreferredWidth(width);
        }
    	
	}

	/** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc=" Erzeugter Quelltext ">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jPanel1 = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        tableSelection = new javax.swing.JComboBox();
        refreshButton = new javax.swing.JButton();
        jScrollPane1 = new javax.swing.JScrollPane();
        closureTable = new javax.swing.JTable();
        jLabel2 = new javax.swing.JLabel();

        getContentPane().setLayout(new java.awt.GridBagLayout());

        setTitle("Find Table");
        jPanel1.setLayout(new java.awt.GridBagLayout());

        jLabel1.setText(" Table ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(jLabel1, gridBagConstraints);

        tableSelection.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Eintrag 1", "Eintrag 2", "Eintrag 3", "Eintrag 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 20;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 0);
        jPanel1.add(tableSelection, gridBagConstraints);

        refreshButton.setText("Refresh");
        refreshButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                refreshButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 30;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
        jPanel1.add(refreshButton, gridBagConstraints);

        closureTable.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null}
            },
            new String [] {
                "Titel 1", "Titel 2", "Titel 3", "Titel 4"
            }
        ));
        closureTable.setShowGrid(false);
        closureTable.setSurrendersFocusOnKeystroke(true);
        jScrollPane1.setViewportView(closureTable);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.gridwidth = 30;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 4);
        jPanel1.add(jScrollPane1, gridBagConstraints);

        jLabel2.setText(" Closure");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 15;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        jPanel1.add(jLabel2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        getContentPane().add(jPanel1, gridBagConstraints);

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void refreshButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_refreshButtonActionPerformed
    	refresh(null);
    }//GEN-LAST:event_refreshButtonActionPerformed

    // Variablendeklaration - nicht modifizieren//GEN-BEGIN:variables
    private javax.swing.JTable closureTable;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JButton refreshButton;
    private javax.swing.JComboBox tableSelection;
    // Ende der Variablendeklaration//GEN-END:variables
    
}
