/*
 * Copyright 2007 - 2017 the original author or authors.
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
import java.util.Locale;

import javax.swing.JLabel;
import javax.swing.JPanel;

import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.Quoting;

/**
 * Table Details View.
 *
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public class TableDetailsView extends javax.swing.JPanel {

    /**
     * Creates new form TableDetailsView
     */
    public TableDetailsView(Table table, final MDTable mdTable, final MetaDataDetailsPanel metaDataDetailsPanel, DataModel dataModel) {
        initComponents();
        
        tableNameLabel.setText(dataModel.getDisplayName(table));
        Font font = tableNameLabel.getFont();
		tableNameLabel.setFont(new Font(font.getName(), font.getStyle(), (int)(font.getSize() * 1.2)));

		if (mdTable != null && !mdTable.isUptodate(table)) {
			warnLabel.setIcon(MetaDataPanel.getWarnIcon(this));
			analyseButton.setText("Analyse schema \"" + mdTable.getSchema().getName() + "\"");
			analyseButton.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                	metaDataDetailsPanel.analyseSchema(mdTable.getSchema().getName());
                }
			});
		} else {
			warnPanel.setVisible(false);
		}

		int y = 1;
		for (Column column: table.getColumns()) {
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

//	        gridBagConstraints = new java.awt.GridBagConstraints();
//	        gridBagConstraints.gridx = 10;
//	        gridBagConstraints.gridy = y;
//	        gridBagConstraints.weightx = 1;
//	        columnsPanel.add(new JLabel(""), gridBagConstraints);

	        ++y;
		}
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

        setLayout(new java.awt.BorderLayout());

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
        gridBagConstraints.gridy = 4;
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

        add(jScrollPane1, java.awt.BorderLayout.CENTER);
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton analyseButton;
    private javax.swing.JPanel columnsPanel;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JLabel tableNameLabel;
    private javax.swing.JLabel warnLabel;
    private javax.swing.JLabel warnLabel1;
    private javax.swing.JLabel warnLabel2;
    private javax.swing.JPanel warnPanel;
    // End of variables declaration//GEN-END:variables
}
