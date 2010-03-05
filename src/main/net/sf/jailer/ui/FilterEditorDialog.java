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
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import javax.swing.DefaultComboBoxModel;
import javax.swing.Icon;
import javax.swing.ImageIcon;
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
    public FilterEditorDialog(ExtractionModelFrame parent, ParameterSelector.ParametersGetter parametersGetter) {
        super(parent, true);
        this.conditionEditor = new ConditionEditor(parent, parametersGetter);
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
		filterTextfieldsPerColumn.clear();
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
    	tableNames.add(0, "---");
    	tableNames.add(0, "All tables");
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
	 * The editor for filter conditions.
	 */
	private ConditionEditor conditionEditor;
	
    /**
     * Refreshes the filter pane.
     */
	private void refreshFilterPane() {
		filterPane.removeAll();
		List<Table> tables = new ArrayList<Table>();
		if (selectedTable != null) {
			tables.add(selectedTable);
		} else {
			tables.addAll(getDataModel().getTables());
			Collections.sort(tables);
		}
		int y = 0;
		java.awt.GridBagConstraints gridBagConstraints;
		filterTextfieldsPerColumn.clear();
		
		for (final Table table: tables) {	
			for (Column c: table.getColumns()) {
				String filter = c.getFilterExpression();

				if (filter == null && selectedTable == null) {
					continue;
				}

				javax.swing.JLabel label = new javax.swing.JLabel();
				if (selectedTable != null) {
					label.setText(" T.");
			        label.setFont(nonBoldFont);
			        label.setForeground(Color.gray);
			        gridBagConstraints = new java.awt.GridBagConstraints();
			        gridBagConstraints.gridx = 0;
			        gridBagConstraints.gridy = y;
			        gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
			        gridBagConstraints.weightx = 0.0;
			        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
			        filterPane.add(label, gridBagConstraints);
				}
		        
				javax.swing.JPanel columnPanel = new JPanel();
				columnPanel.setLayout(new java.awt.BorderLayout());

				label = new javax.swing.JLabel();
				final String columnName = (selectedTable == null? " " + table.getUnqualifiedName() + "." : "") + c.name;
				label.setText(columnName);
		        label.setFont(filter == null || selectedTable == null? nonBoldFont : boldFont);
				
		        boolean isPK = false;
		        for (Column pk: table.primaryKey.getColumns()) {
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
		        label.setFont(filter == null || selectedTable == null? nonBoldFont : boldFont);
				columnPanel.add(label, java.awt.BorderLayout.EAST);
				
				gridBagConstraints = new java.awt.GridBagConstraints();
		        gridBagConstraints.gridx = 1;
		        gridBagConstraints.gridy = y;
		        gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
		        gridBagConstraints.weightx = 0.0;
		        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
		        filterPane.add(columnPanel, gridBagConstraints);
		        
				label = new javax.swing.JLabel();
				label.setText(" :=  ");
		        label.setFont(nonBoldFont);
				gridBagConstraints = new java.awt.GridBagConstraints();
		        gridBagConstraints.gridx = 2;
		        gridBagConstraints.gridy = y;
		        gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
		        gridBagConstraints.weightx = 0.0;
		        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
		        filterPane.add(label, gridBagConstraints);
		        
		        final javax.swing.JTextField textField = new javax.swing.JTextField();
		        textField.setText(filter == null? "" : ConditionEditor.toSingleLine(filter));
		        filterTextfieldsPerColumn.put(c, textField);
		        
		        label = new javax.swing.JLabel();
				label.setText(null);
		        
				label.setIcon(conditionEditorIcon);
				
		        gridBagConstraints = new java.awt.GridBagConstraints();
		        gridBagConstraints.gridx = 3;
		        gridBagConstraints.gridy = y;
		        gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
		        gridBagConstraints.weightx = 0.0;
		        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
		        gridBagConstraints.insets = new Insets(1, 0, 0, 2);
		        filterPane.add(label, gridBagConstraints);
		        
		        gridBagConstraints = new java.awt.GridBagConstraints();
		        gridBagConstraints.gridx = 4;
		        gridBagConstraints.gridy = y;
		        gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
		        gridBagConstraints.weightx = 1.0;
		        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
		        gridBagConstraints.insets = new Insets(1, 0, 0, 0);
		        filterPane.add(textField, gridBagConstraints);

		        final JLabel theLabel = label;
		        label.addMouseListener(new java.awt.event.MouseAdapter() {
					public void mouseClicked(java.awt.event.MouseEvent evt) {
						conditionEditor.setTitle(columnName.trim());
						String cond = conditionEditor.edit(textField.getText(), "Table", "T", table, null, null, null, false);
						if (cond != null) {
							if (!textField.getText().equals(ConditionEditor.toSingleLine(cond))) {
								textField.setText(ConditionEditor.toSingleLine(cond));
							}
							theLabel.setIcon(conditionEditorIcon);
						}
					}
					
					public void mouseEntered(java.awt.event.MouseEvent evt) {
						theLabel.setIcon(conditionEditorSelectedIcon);
		            }
		            public void mouseExited(java.awt.event.MouseEvent evt) {
		            	theLabel.setIcon(conditionEditorIcon);
		           }
		        });

		        ++y;
			}
		}
		javax.swing.JLabel label = new javax.swing.JLabel();
		label.setText(y == 0? " no filter defined" : " ");
		label.setFont(nonBoldFont);
		
		gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = y;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        filterPane.add(label, gridBagConstraints);
		validate();
		repaint();
	}

	/**
	 * Stores the text-field content.
	 */
	private void storeFilterExpressions() {
		for (Column c: filterTextfieldsPerColumn.keySet()) {
			String newFilter = ConditionEditor.toMultiLine(filterTextfieldsPerColumn.get(c).getText()).trim();
			if (newFilter.length() == 0) {
				newFilter = null;
			}
			c.setFilterExpression(newFilter);
		}
		getDataModel().version++;
		parent.extractionModelEditor.refresh(false, false, true, true);
		parent.extractionModelEditor.markDirty();
	}
	
	/**
	 * Whether there is unsaved data.
	 */
	private boolean needsSave() {
		for (Column c: filterTextfieldsPerColumn.keySet()) {
			String newFilter = ConditionEditor.toMultiLine(filterTextfieldsPerColumn.get(c).getText()).trim();
			if (newFilter.length() == 0) {
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
        tableBox = new net.sf.jailer.ui.JComboBox();
        jPanel2 = new javax.swing.JPanel();
        okButton = new javax.swing.JButton();
        cancelButton = new javax.swing.JButton();
        jLabel2 = new javax.swing.JLabel();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Filter Editor");
        getContentPane().setLayout(new java.awt.GridBagLayout());

        jPanel1.setBorder(javax.swing.BorderFactory.createTitledBorder("Filters"));
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

        jLabel1.setText(" Table ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jLabel1, gridBagConstraints);

        tableBox.setMaximumRowCount(20);
        tableBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        tableBox.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                tableBoxItemStateChanged(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 10;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 2);
        getContentPane().add(tableBox, gridBagConstraints);

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

        jLabel2.setForeground(java.awt.Color.gray);
        jLabel2.setText("as T ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        getContentPane().add(jLabel2, gridBagConstraints);

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void okButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_okButtonActionPerformed
    	if (needsSave()) {
    		storeFilterExpressions();
    	}
    	setVisible(false);
    }//GEN-LAST:event_okButtonActionPerformed

    private void tableBoxItemStateChanged(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_tableBoxItemStateChanged
    	if (tableBox.getSelectedItem() instanceof String) {
    		Table table = getDataModel().getTableByDisplayName((String) tableBox.getSelectedItem());
			if (needsSave()) {
				if (JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(this, "Keep filter changes?", "", JOptionPane.YES_NO_OPTION)) {
					storeFilterExpressions();
				}
			}
			selectedTable = table;
			refreshFilterPane();
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
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JButton okButton;
    private javax.swing.JComboBox tableBox;
    // End of variables declaration//GEN-END:variables
    
    private Icon conditionEditorIcon;
    private Icon conditionEditorSelectedIcon;
	{
		String dir = "/net/sf/jailer/resource";
		
		// load images
		try {
			conditionEditorIcon = new ImageIcon(getClass().getResource(dir + "/edit.png"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		try {
			conditionEditorSelectedIcon = new ImageIcon(getClass().getResource(dir + "/edit_s.png"));
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private static final long serialVersionUID = 7869830170667759018L;
}
