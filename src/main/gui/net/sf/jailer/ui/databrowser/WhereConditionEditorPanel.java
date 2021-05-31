/*
 * Copyright 2007 - 2021 Ralf Wisser.
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
package net.sf.jailer.ui.databrowser;

import java.awt.Color;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Window;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;

import javax.swing.DefaultComboBoxModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.JToggleButton;

import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.StringSearchPanel;
import net.sf.jailer.ui.UIUtil;

/**
 * SQL-Where-Condition Editor.
 *
 * @author Ralf Wisser
 */
public class WhereConditionEditorPanel extends javax.swing.JPanel {

	// TODO remove empty lines before putting text back into sql console after user edit
	
	private final DataModel dataModel;
	private final Table table;
	private List<Column> searchColumns = new ArrayList<Column>();
	private JToggleButton searchButton;
	private Map<String, Color> fgColorMap;
	
    /**
     * Creates new form SearchPanel
     * @param sorted 
     */
    public WhereConditionEditorPanel(Window parent, DataModel dataModel, Table table, Boolean sorted, WhereConditionEditorPanel predecessor) {
    	this.dataModel = dataModel;
    	this.table = table;
        initComponents();
        
        Font font = new JLabel("L").getFont();
		tableLabel.setFont(new Font(font.getName(), font.getStyle(), (int)(font.getSize() * 1.2)));
		if (table == null) {
        	jPanel2.setVisible(false);
        } else {
        	fgColorMap = new HashMap<String, Color>();
        	if (table.primaryKey != null) {
        		table.primaryKey.getColumns().forEach(c -> { if (c.name != null) { fgColorMap.put(c.name, Color.red); }});
        	}
        	tableLabel.setText(this.dataModel.getDisplayName(table));
	        GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 1;
	        gridBagConstraints.gridy = 20;
			final Window owner = parent;
			final JComboBox comboBox = searchComboBox;
	        searchButton = StringSearchPanel.createSearchButton(owner, comboBox, "Add Search Field", new Runnable() {
			    @Override
			    public void run() {
			        addColumn();
			    }
			}, null, null, null, false, null, false, false, fgColorMap);
	        searchButton.setText("Add Search Field");
	        gridBagConstraints = new GridBagConstraints();
	        gridBagConstraints.gridx = 1;
	        gridBagConstraints.gridy = 4;
	        gridBagConstraints.anchor = GridBagConstraints.WEST;
	        jPanel6.add(searchButton, gridBagConstraints);
	        searchComboBox.setVisible(false);
	
	        if (jScrollPane2.getHorizontalScrollBar() != null) {
	        	jScrollPane2.getHorizontalScrollBar().setUnitIncrement(16);
	        }
	        
	        updateSearchUI();
	        sortCheckBox.setSelected(Boolean.TRUE.equals(sorted));
        }
        if (predecessor != null) {
        	jSplitPane1.setDividerLocation(predecessor.jSplitPane1.getDividerLocation());
        }
    }
    
    private void updateSearchUI() {
    	List<String> colNames = new ArrayList<String>();
    	for (Column column: table.getColumns()) {
    		if (!searchColumns.contains(column)) {
    			if (!colNames.contains(column.name)) {
    				colNames.add(column.name);
    			}
    		}
    	}
    	if (sortCheckBox.isSelected()) {
    		colNames.sort(String::compareToIgnoreCase);
    	}
    	searchButton.setEnabled(!colNames.isEmpty());
		searchComboBox.setModel(new DefaultComboBoxModel<String>(colNames.toArray(new String[0])));
		
		List<Column> sortedSearchColumns = new ArrayList<Column>(searchColumns);
		if (sortCheckBox.isSelected()) {
			sortedSearchColumns.sort((a, b) -> a.name.compareToIgnoreCase(b.name));
		}
		BiFunction<JComponent, Integer, JComponent> wrap = (c, y) -> {
			JPanel panel = new JPanel(new GridBagLayout());
			c.setForeground(Color.black);
			panel.setBackground(y % 2 != 0? UIUtil.TABLE_BACKGROUND_COLOR_1 : UIUtil.TABLE_BACKGROUND_COLOR_2);
			panel.setOpaque(true);
			GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 1;
	        gridBagConstraints.gridy = y;
	        gridBagConstraints.weightx = 1;
	        gridBagConstraints.fill = GridBagConstraints.BOTH;
	        if (c instanceof JLabel) {
	        	gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 8);
	        }
	        panel.add(c, gridBagConstraints);
			return panel;
		};
		searchFieldsPanel.removeAll();
		int y = 0;
		for (Column column: sortedSearchColumns) {
	        JLabel nameLabel = new JLabel();
			nameLabel.setText(column.name);
	        GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 1;
	        gridBagConstraints.gridy = y;
	        gridBagConstraints.fill = GridBagConstraints.BOTH;
	        searchFieldsPanel.add(wrap.apply(nameLabel, y), gridBagConstraints);
	        boolean isPk = table.primaryKey != null && table.primaryKey.getColumns().contains(column);
			nameLabel.setForeground(isPk? Color.red : Color.black);
			
	        JTextField valueTextField = new JTextField();
			valueTextField.setText("jTextField1");
	        gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 2;
	        gridBagConstraints.gridy = y;
	        gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
	        gridBagConstraints.weightx = 1;
	        gridBagConstraints.fill = GridBagConstraints.BOTH;
	        searchFieldsPanel.add(wrap.apply(valueTextField, y), gridBagConstraints);

	        JButton hideButton = new JButton();
	        hideButton.setIcon(UIUtil.scaleIcon(this, deleteIcon));
			gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 3;
	        gridBagConstraints.gridy = y;
	        gridBagConstraints.fill = GridBagConstraints.BOTH;
	        searchFieldsPanel.add(wrap.apply(hideButton, y), gridBagConstraints);
	        
	        ++y;
		}
    }

    protected void addColumn() {
    	Object toFind = searchComboBox.getSelectedItem();
    	for (Column column: table.getColumns()) {
        	if (column.name.equals(toFind)) {
        		searchColumns.add(column);
        	}
        }
    	updateSearchUI();
	}

	/**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jSplitPane1 = new javax.swing.JSplitPane();
        jPanel1 = new javax.swing.JPanel();
        sepsPanel = new javax.swing.JPanel();
        jPanel2 = new javax.swing.JPanel();
        jScrollPane2 = new javax.swing.JScrollPane();
        jPanel5 = new javax.swing.JPanel();
        jPanel6 = new javax.swing.JPanel();
        searchComboBox = new javax.swing.JComboBox<>();
        sortCheckBox = new javax.swing.JCheckBox();
        searchFieldsPanel = new javax.swing.JPanel();
        jPanel4 = new javax.swing.JPanel();
        jPanel7 = new javax.swing.JPanel();
        tableLabel = new javax.swing.JLabel();
        jSeparator1 = new javax.swing.JSeparator();
        jPanel3 = new javax.swing.JPanel();
        jLabel2 = new javax.swing.JLabel();

        setLayout(new java.awt.GridBagLayout());

        jPanel1.setLayout(new java.awt.GridBagLayout());

        sepsPanel.setMinimumSize(new java.awt.Dimension(400, 0));
        sepsPanel.setLayout(null);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(sepsPanel, gridBagConstraints);

        jPanel2.setLayout(new java.awt.GridBagLayout());

        jScrollPane2.setHorizontalScrollBarPolicy(javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);

        jPanel5.setLayout(new java.awt.GridBagLayout());

        jPanel6.setLayout(new java.awt.GridBagLayout());

        searchComboBox.setModel(new javax.swing.DefaultComboBoxModel<>(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel6.add(searchComboBox, gridBagConstraints);

        sortCheckBox.setText("Sort Columns");
        sortCheckBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                sortCheckBoxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        jPanel6.add(sortCheckBox, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(8, 0, 0, 0);
        jPanel5.add(jPanel6, gridBagConstraints);

        searchFieldsPanel.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        jPanel5.add(searchFieldsPanel, gridBagConstraints);

        jPanel4.setLayout(null);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 100;
        gridBagConstraints.weighty = 1.0;
        jPanel5.add(jPanel4, gridBagConstraints);

        jPanel7.setBackground(java.awt.Color.white);
        jPanel7.setLayout(new java.awt.GridBagLayout());

        tableLabel.setText("<html><i>no table selected</i></html>");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 0);
        jPanel7.add(tableLabel, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        jPanel7.add(jSeparator1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        jPanel5.add(jPanel7, gridBagConstraints);

        jScrollPane2.setViewportView(jPanel5);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel2.add(jScrollPane2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weighty = 0.001;
        jPanel1.add(jPanel2, gridBagConstraints);

        jSplitPane1.setLeftComponent(jPanel1);

        jPanel3.setLayout(new java.awt.GridBagLayout());

        jLabel2.setText("jLabel1");
        jPanel3.add(jLabel2, new java.awt.GridBagConstraints());

        jSplitPane1.setRightComponent(jPanel3);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(jSplitPane1, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    private void sortCheckBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_sortCheckBoxActionPerformed
    	updateSearchUI();
    }//GEN-LAST:event_sortCheckBoxActionPerformed


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JLabel jLabel2;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JPanel jPanel7;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JSplitPane jSplitPane1;
    private javax.swing.JComboBox<String> searchComboBox;
    private javax.swing.JPanel searchFieldsPanel;
    private javax.swing.JPanel sepsPanel;
    private javax.swing.JCheckBox sortCheckBox;
    private javax.swing.JLabel tableLabel;
    // End of variables declaration//GEN-END:variables
    
	private static ImageIcon tableIcon;
	private static ImageIcon deleteIcon;
	static {
        // load images
        tableIcon = UIUtil.readImage("/table.png");
        deleteIcon = UIUtil.readImage("/delete.png");
    }
}
