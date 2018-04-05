/*
 * Copyright 2007 - 2018 the original author or authors.
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
package net.sf.jailer.ui.databrowser.metadata;

import java.awt.Color;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;

import javax.swing.JLabel;
import javax.swing.JPanel;

import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.modelbuilder.ModelBuilder;
import net.sf.jailer.util.Quoting;

/**
 * Table Details View.
 *
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public class TableDetailsView extends javax.swing.JPanel {

	private final Runnable updateColumnsTable;
	private boolean cacheable = true;
	
	/**
     * Creates new form TableDetailsView
     */
    public TableDetailsView(final Table table, final MDTable mdTable, final MetaDataDetailsPanel metaDataDetailsPanel, final DataModel dataModel) {
        initComponents();
        updateColumnsTable = new Runnable() {
			@Override
			public void run() {
		        String typeLabel = "";
		        if (mdTable != null) {
		        	if (mdTable.isView()) {
		        		typeLabel = "View ";
		        	} else if (mdTable.isSynonym()) {
		        		typeLabel = "Synonym ";
		        	}
		        }
				tableNameLabel.setText(typeLabel + dataModel.getDisplayName(table));
		        Font font = new JLabel("L").getFont();
				tableNameLabel.setFont(new Font(font.getName(), font.getStyle(), (int)(font.getSize() * 1.2)));
		
				boolean mdTableIsUpTodate = true;
				if (mdTable != null) {
					if (!mdTable.isLoaded()) {
						cacheable = false;
					} else {
						mdTableIsUpTodate = mdTable.isUptodate(table);
					}
				}
						
				if (mdTable != null && !mdTableIsUpTodate && !ModelBuilder.isJailerTable(table.getUnqualifiedName())) {
					warnLabel.setIcon(MetaDataPanel.getScaledIcon(TableDetailsView.this, MetaDataPanel.warnIcon));
					analyseButton.setText("Analyse schema \"" + mdTable.getSchema().getUnquotedName() + "\"");
					analyseButton.addActionListener(new ActionListener() {
		                @Override
		                public void actionPerformed(ActionEvent e) {
		                	metaDataDetailsPanel.analyseSchema(mdTable.getSchema().getName(), mdTable.isView(), mdTable.isSynonym());
		                }
					});
				} else {
					warnPanel.setVisible(false);
				}
		
				int y = 1;
				List<Column> columns = table.getColumns();
				if (sortColumnsCheckBox.isSelected()) {
					columns = new ArrayList<Column>(columns);
					Collections.sort(columns, new Comparator<Column>() {
						@Override
						public int compare(Column o1, Column o2) {
							return Quoting.staticUnquote(o1.name).compareToIgnoreCase(Quoting.staticUnquote(o2.name));
						}
					});
				}
				columnsPanel.removeAll();
				for (Column column: columns) {
					boolean isPk = false;
					if (table.primaryKey.getColumns() != null) {
						for (Column pk: table.primaryKey.getColumns()) {
							if (pk.name.equals(column.name)) {
								isPk = true;
							}
						}
					}
					
					JPanel panel = new JPanel();
					panel.setOpaque(false);
			        panel.setLayout(new java.awt.GridBagLayout());
		
			        JLabel label;
			        label = new JLabel();
			        
			        if (isPk) {
			        	label.setForeground(Color.red);
			        }
			        
			        label.setText(Quoting.staticUnquote(column.name));
			        GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
			        gridBagConstraints.gridx = 1;
			        gridBagConstraints.gridy = y;
			        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
			        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
			        gridBagConstraints.weightx = 1.0;
			        panel.add(label, gridBagConstraints);
		
			        label = new JLabel();
			        label.setForeground(Color.gray);
			        label.setText(column.toSQL("").substring(column.name.length()).trim());
			        gridBagConstraints = new java.awt.GridBagConstraints();
			        gridBagConstraints.gridx = 2;
			        gridBagConstraints.gridy = y;
			        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
			        gridBagConstraints.weightx = 1.0;
			        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 0);
			        panel.add(label, gridBagConstraints);
		
			        StringBuilder constraints = new StringBuilder();
			        
			        if (!column.isNullable) {
			        	constraints.append("not null ");
			        }
			        if (column.isVirtual) {
			        	constraints.append("virtual ");
			        }
			        if (column.isIdentityColumn) {
			        	constraints.append("identity ");
			        }
			        
			        if (constraints.length() > 0) {
				        label = new JLabel();
				        label.setForeground(Color.gray);
			        	if (column.type.toUpperCase(Locale.ENGLISH).equals(column.type)) {
							label.setText(constraints.toString().toUpperCase(Locale.ENGLISH));
			        	} else {
							label.setText(constraints.toString());
			        	}
				        gridBagConstraints = new java.awt.GridBagConstraints();
				        gridBagConstraints.gridx = 3;
				        gridBagConstraints.gridy = y;
				        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
				        gridBagConstraints.weightx = 0;
				        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 0);
				        columnsPanel.add(label, gridBagConstraints);
			        }
		
			        gridBagConstraints = new java.awt.GridBagConstraints();
			        gridBagConstraints.gridx = 1;
			        gridBagConstraints.gridy = y;
			        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
			        gridBagConstraints.weightx = 1;
			        columnsPanel.add(panel, gridBagConstraints);
		
			        ++y;
				}
			}
		};
		updateColumnsTable.run();
    }

    public boolean isCacheable() {
		return cacheable;
	}

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jScrollPane1 = new javax.swing.JScrollPane();
        jPanel1 = new javax.swing.JPanel();
        tableNameLabel = new javax.swing.JLabel();
        jSeparator1 = new javax.swing.JSeparator();
        columnsPanel = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        warnPanel = new javax.swing.JPanel();
        warnLabel = new javax.swing.JLabel();
        analyseButton = new javax.swing.JButton();
        warnLabel1 = new javax.swing.JLabel();
        warnLabel2 = new javax.swing.JLabel();
        sortColumnsCheckBox = new javax.swing.JCheckBox();

        setBackground(new java.awt.Color(255, 255, 216));
        setLayout(new java.awt.GridBagLayout());

        jScrollPane1.setBorder(null);

        jPanel1.setBackground(new java.awt.Color(255, 255, 216));
        jPanel1.setLayout(new java.awt.GridBagLayout());

        tableNameLabel.setText("Test");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 0);
        jPanel1.add(tableNameLabel, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(jSeparator1, gridBagConstraints);

        columnsPanel.setOpaque(false);
        columnsPanel.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 0, 4);
        jPanel1.add(columnsPanel, gridBagConstraints);

        jLabel1.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.weighty = 1.0;
        jPanel1.add(jLabel1, gridBagConstraints);

        warnPanel.setOpaque(false);
        warnPanel.setLayout(new java.awt.GridBagLayout());

        warnLabel.setText("  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        warnPanel.add(warnLabel, gridBagConstraints);

        analyseButton.setText("jButton1");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.gridwidth = 2;
        warnPanel.add(analyseButton, gridBagConstraints);

        warnLabel1.setText("Data model differs from");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        warnPanel.add(warnLabel1, gridBagConstraints);

        warnLabel2.setText("database table definition");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        warnPanel.add(warnLabel2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 12, 2, 0);
        jPanel1.add(warnPanel, gridBagConstraints);

        jScrollPane1.setViewportView(jPanel1);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(jScrollPane1, gridBagConstraints);

        sortColumnsCheckBox.setText("sort columns      ");
        sortColumnsCheckBox.setOpaque(false);
        sortColumnsCheckBox.addActionListener(new java.awt.event.ActionListener() {
            @Override
			public void actionPerformed(java.awt.event.ActionEvent evt) {
                sortColumnsCheckBoxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        add(sortColumnsCheckBox, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    private void sortColumnsCheckBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_sortColumnsCheckBoxActionPerformed
		updateColumnsTable.run();
		repaint();
    }//GEN-LAST:event_sortColumnsCheckBoxActionPerformed

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton analyseButton;
    private javax.swing.JPanel columnsPanel;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JCheckBox sortColumnsCheckBox;
    private javax.swing.JLabel tableNameLabel;
    private javax.swing.JLabel warnLabel;
    private javax.swing.JLabel warnLabel1;
    private javax.swing.JLabel warnLabel2;
    private javax.swing.JPanel warnPanel;
    // End of variables declaration//GEN-END:variables
}
