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
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Vector;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.ListCellRenderer;

import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;

/**
 * Column filter editor.
 * 
 * @author Ralf Wisser
 */
public class FilterEditorDialog extends javax.swing.JDialog {

	/**
	 * Parent frame.
	 */
	private final ExtractionModelFrame parent;

	private boolean isInitialized = false;
	
	/**
	 * Currently selected table (in closure-table).
	 */
	private Table selectedTable;
	
	/**
     * Make {@link #refresh(Table)} reentrant.
     */
    private boolean refreshing = false;
    
    /** Creates new form FilterEditor */
    public FilterEditorDialog(ExtractionModelFrame parent) {
        super(parent, true);
        this.parent = parent;
        initComponents();
        final ListCellRenderer tableBoxRenderer = tableBox.getRenderer();
    	tableBox.setRenderer(new ListCellRenderer() {
			@Override
			public Component getListCellRendererComponent(JList list,
					Object value, int index, boolean isSelected,
					boolean cellHasFocus) {
				if (value instanceof String) {
					Table table = getDataModel().getTableByDisplayName((String) value);
					if (table != null) {
						int n = 0;
						for (Column c: table.getColumns()) {
							if (c.getFilterExpression() != null) {
								++n;
							}
						}
						if (n > 0) {
							value = value + " (" + n + ")";
						}
					}
				}
				return tableBoxRenderer.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
			}
    	});
    }

    /**
     * Gets current data model.
     * 
     * @return current data model
     */
    private DataModel getDataModel() {
    	return parent.extractionModelEditor.dataModel;
    }

    /**
     * Opens the editor.
     * 
     * @param table the initially selected table
     */
	public void open(Table table) {
		if (!isInitialized) {
	        int w = 600, h = 400;
	        setSize(w, h);
	        setLocation(Math.max(0, parent.getX() + parent.getWidth() / 2 - w / 2),
	                    Math.max(0, parent.getY() + parent.getHeight() / 2 - h / 2));
	        isInitialized = true;
		}
		refresh(table);
		setVisible(true);
	}
	
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
    	selectedTable = tableToSelect;
    	
    	Vector<String> tableNames = new Vector<String>();
    	for (Table table: getDataModel().getTables()) {
    		tableNames.add(getDataModel().getDisplayName(table));
    	}
    	Collections.sort(tableNames);
    	DefaultComboBoxModel model = new DefaultComboBoxModel(tableNames);
    	tableBox.setModel(model);
    	if (tableToSelect == null && tableNames.size() > 0) {
    		tableToSelect = getDataModel().getTableByDisplayName(tableNames.firstElement());
    	}
    	if (tableToSelect != null) {
    		tableBox.setSelectedItem(getDataModel().getDisplayName(tableToSelect));
    	} else {
    		tableBox.setSelectedItem(0);
    	}
    	
    	refreshFilterPane();

    	refreshing = false;
    }

    private Font font = new JLabel("normal").getFont();

    /**
     * Bold font.
     */
	private Font boldFont = new Font(font.getName(), font.getStyle() | Font.BOLD, font.getSize());
	
	/**
     * Non-bold font.
     */
	private Font nonBoldFont = new Font(font.getName(), font.getStyle() & ~Font.BOLD, font.getSize());
	
	/**
	 * Maps columns to the text-field with which to edit the filter expression.
	 */
	private Map<Column, JTextField> filterTextfieldsPerColumn = new HashMap<Column, JTextField>();
	
    /**
     * Refreshes the filter pane.
     */
	private void refreshFilterPane() {
		filterPane.removeAll();
		if (selectedTable != null) {
			int y = 0;
			java.awt.GridBagConstraints gridBagConstraints;
			filterTextfieldsPerColumn.clear();
			
			for (Column c: selectedTable.getColumns()) {
				String filter = c.getFilterExpression();

				javax.swing.JPanel columnPanel = new JPanel();
				columnPanel.setLayout(new java.awt.BorderLayout());

				javax.swing.JLabel label = new javax.swing.JLabel();
				label.setText(" " + c.name);
		        label.setFont(filter == null? nonBoldFont : boldFont);
				
		        boolean isPK = false;
		        for (Column pk: selectedTable.primaryKey.getColumns()) {
		        	if (pk.equals(c)) {
		        		isPK = true;
		        		break;
		        	}
		        }
		        
		        if (isPK) {
		        	label.setForeground(Color.RED);
		        }
		        columnPanel.add(label, java.awt.BorderLayout.WEST);
				label = new javax.swing.JLabel();
				label.setText(" " + (c.toSQL(null).substring(c.name.length())) + " ");
				label.setForeground(Color.GRAY);
		        label.setFont(filter == null? nonBoldFont : boldFont);
				columnPanel.add(label, java.awt.BorderLayout.EAST);
				
				gridBagConstraints = new java.awt.GridBagConstraints();
		        gridBagConstraints.gridx = 0;
		        gridBagConstraints.gridy = y;
		        gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
		        gridBagConstraints.weightx = 0.0;
		        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
		        filterPane.add(columnPanel, gridBagConstraints);
		        
				label = new javax.swing.JLabel();
				label.setText(" is  ");
		        label.setFont(nonBoldFont);
				gridBagConstraints = new java.awt.GridBagConstraints();
		        gridBagConstraints.gridx = 1;
		        gridBagConstraints.gridy = y;
		        gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
		        gridBagConstraints.weightx = 0.0;
		        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
		        filterPane.add(label, gridBagConstraints);
		        
		        javax.swing.JTextField textField = new javax.swing.JTextField();
		        textField.setText(filter == null? "T." + c.name : filter);
		        filterTextfieldsPerColumn.put(c, textField);
		        
		        gridBagConstraints = new java.awt.GridBagConstraints();
		        gridBagConstraints.gridx = 2;
		        gridBagConstraints.gridy = y;
		        gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
		        gridBagConstraints.weightx = 1.0;
		        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
		        gridBagConstraints.insets = new Insets(1, 0, 0, 0);
		        filterPane.add(textField, gridBagConstraints);
		        
		        ++y;
			}
			javax.swing.JLabel label = new javax.swing.JLabel();
			label.setText(" ");
			
			gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 0;
	        gridBagConstraints.gridy = y;
	        gridBagConstraints.weighty = 1.0;
	        filterPane.add(label, gridBagConstraints);
		}
		validate();
		repaint();
	}

	/**
	 * Stores the text-field content.
	 */
	private void storeFilterExpressions() {
		if (selectedTable != null) {
			for (Column c: selectedTable.getColumns()) {
				String newFilter = filterTextfieldsPerColumn.get(c).getText().trim();
				if (newFilter.equals("T." + c.name) || newFilter.length() == 0) {
					newFilter = null;
				}
				c.setFilterExpression(newFilter);
			}
		}
	}
	
	/**
	 * Whether there is unsaved data.
	 */
	private boolean needsSave() {
		if (selectedTable != null) {
			for (Column c: selectedTable.getColumns()) {
				String newFilter = filterTextfieldsPerColumn.get(c).getText().trim();
				if (newFilter.equals("T." + c.name) || newFilter.length() == 0) {
					newFilter = null;
				}
				if (c.getFilterExpression() == null && newFilter != null) {
					return true;
				}
				if (c.getFilterExpression() != null) {
					if (!c.getFilterExpression().equals(newFilter)) {
						return true;
					}
				}
			}
		}
		return false;
	}
	
	/** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jPanel1 = new javax.swing.JPanel();
        filterScrollPane = new javax.swing.JScrollPane();
        filterPane = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        tableBox = new javax.swing.JComboBox();
        jLabel3 = new javax.swing.JLabel();
        jPanel2 = new javax.swing.JPanel();
        okButton = new javax.swing.JButton();
        cancelButton = new javax.swing.JButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Filter Editor");
        getContentPane().setLayout(new java.awt.GridBagLayout());

        jPanel1.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        jPanel1.setLayout(new javax.swing.BoxLayout(jPanel1, javax.swing.BoxLayout.LINE_AXIS));

        filterScrollPane.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));

        filterPane.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        filterPane.setLayout(new java.awt.GridBagLayout());
        filterScrollPane.setViewportView(filterPane);

        jPanel1.add(filterScrollPane);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.gridwidth = 12;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        getContentPane().add(jPanel1, gridBagConstraints);

        jLabel1.setText(" Table   ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jLabel1, gridBagConstraints);

        jLabel2.setText(" Filters");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        getContentPane().add(jLabel2, gridBagConstraints);

        tableBox.setMaximumRowCount(20);
        tableBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        tableBox.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                tableBoxItemStateChanged(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 10;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 2);
        getContentPane().add(tableBox, gridBagConstraints);

        jLabel3.setText(" as T ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 12;
        gridBagConstraints.gridy = 1;
        getContentPane().add(jLabel3, gridBagConstraints);

        jPanel2.setLayout(new java.awt.GridBagLayout());

        okButton.setText("  OK  ");
        okButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                okButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
        jPanel2.add(okButton, gridBagConstraints);

        cancelButton.setText(" Cancel ");
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 11;
        gridBagConstraints.gridy = 20;
        jPanel2.add(cancelButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.gridwidth = 13;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        getContentPane().add(jPanel2, gridBagConstraints);

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void okButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_okButtonActionPerformed
    	storeFilterExpressions();
    	setVisible(false);
    }//GEN-LAST:event_okButtonActionPerformed

    private void tableBoxItemStateChanged(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_tableBoxItemStateChanged
    	if (tableBox.getSelectedItem() instanceof String) {
    		Table table = getDataModel().getTableByDisplayName((String) tableBox.getSelectedItem());
    		if (selectedTable != table) {
    			if (needsSave()) {
    				if (JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(this, "Save changes?", "", JOptionPane.YES_NO_OPTION)) {
    					storeFilterExpressions();
    				}
    			}
    			selectedTable = table;
    			refreshFilterPane();
    		}
    	}
    }//GEN-LAST:event_tableBoxItemStateChanged
 
    private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
        setVisible(false);
    }//GEN-LAST:event_cancelButtonActionPerformed
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton cancelButton;
    private javax.swing.JPanel filterPane;
    private javax.swing.JScrollPane filterScrollPane;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JButton okButton;
    private javax.swing.JComboBox tableBox;
    // End of variables declaration//GEN-END:variables
    
}
