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
package net.sf.jailer.ui.databrowser;

import java.awt.Color;
import java.awt.Font;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.RowSorter;
import javax.swing.ScrollPaneConstants;
import javax.swing.SpinnerNumberModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.table.TableModel;

import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;

/**
 * Row Details View.
 * 
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public abstract class DetailsView extends javax.swing.JPanel {

	private final Table table;
	private final List<Row> rows;
	private final RowSorter<? extends TableModel> rowSorter;
	
    /** Creates new form DetailsView 
     * @param rowSorter 
    */
    public DetailsView(List<Row> rows, int size, DataModel dataModel, Table table, int rowIndex, RowSorter<? extends TableModel> rowSorter, boolean showSpinner) {
        this.table = table;
    	this.rows = rows;
    	this.rowSorter = rowSorter;
        initComponents();
        if (rowSorter != null) {
        	rowIndex = rowSorter.convertRowIndexToView(rowIndex);
        }
        final SpinnerNumberModel model = new SpinnerNumberModel(rowIndex + 1, 1, size, -1);
        rowSpinner.setModel(model);
        rowSpinner.addChangeListener(new ChangeListener() {
			@Override
			public void stateChanged(ChangeEvent e) {
				setCurrentRow((Integer) model.getValue() - 1, true);
			}
		});
        if (!showSpinner) {
        	jLabel1.setVisible(false);
        	rowSpinner.setVisible(false);
//        	jScrollPane1.setBorder(null);
        	jScrollPane1.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER);
			jScrollPane1.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
		}
        setCurrentRow(rowIndex, showSpinner);
    }
    
    private final Font font = new JLabel().getFont();
    private final Font nonbold = new Font(font.getName(), font.getStyle() & ~Font.BOLD, font.getSize()); 
    private final Font italic = new Font(font.getName(), font.getStyle() & ~Font.BOLD | Font.ITALIC, font.getSize()); 
    private final Color BG1 = new Color(255, 255, 255);
    private final Color BG2 = new Color(230, 255, 255);
    private final Color FG1 = new Color(155, 0, 0);
	
    public void setBorderColor(Color color) {
		jScrollPane1.setBorder(BorderFactory.createEtchedBorder(color, Color.GRAY));
    }
    
    private void setCurrentRow(int row, boolean selectableFields) {
    	jPanel1.removeAll();
    	int i = 0;
    	java.awt.GridBagConstraints gridBagConstraints;
    	while (i < table.getColumns().size()) {
    		Column c = table.getColumns().get(i);
    		JLabel l = new JLabel();
    		l.setText(" " + c.name + "  ");
    		l.setFont(nonbold);
    		gridBagConstraints = new java.awt.GridBagConstraints();
            gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
            gridBagConstraints.weightx = 0;
            gridBagConstraints.weighty = 0;
            gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
            gridBagConstraints.gridx = 0;
            gridBagConstraints.gridy = i;
            jPanel1.add(l, gridBagConstraints);

            gridBagConstraints = new java.awt.GridBagConstraints();
            gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
            gridBagConstraints.weighty = 0;
            gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
            gridBagConstraints.weightx = 1;
            gridBagConstraints.gridx = 1;
            gridBagConstraints.gridy = i;
            Object v = rows.get(rowSorter != null? rowSorter.convertRowIndexToModel(row) : row).values[i];
            if (selectableFields) {
	            JTextField f = new JTextField();
				f.setText(v == null? "" : v.toString());
				f.setEnabled(v != null);
	            f.setEditable(false);
	            jPanel1.add(f, gridBagConstraints);
            } else {
	            JLabel f = new JLabel();
				f.setText((v == null? "null" : v.toString()) + "    ");
	    		f.setFont(v == null? italic : nonbold);
	    		if (v == null) {
	    			f.setForeground(Color.GRAY);
	    		}
	            jPanel1.add(f, gridBagConstraints);
	            f.setBackground(i % 2 == 0? BG1 : BG2);
	            l.setBackground(i % 2 == 0? BG1 : BG2);
	            f.setOpaque(true);
	            l.setOpaque(true);
	            if (table.primaryKey != null && table.primaryKey.getColumns().contains(c)) {
	            	l.setForeground(FG1);
	            } else {
	            	l.setForeground(Color.BLUE);
	            }
            }
            
//          l = new JLabel();
//    		l.setText(" ");
//    		gridBagConstraints = new java.awt.GridBagConstraints();
//            gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
//            gridBagConstraints.weightx = 1.0;
//            gridBagConstraints.gridx = 2;
//            gridBagConstraints.gridy = i;
//            gridBagConstraints.fill = GridBagConstraints.BOTH;
//            jPanel1.add(l, gridBagConstraints);
            
            ++i;
    	}
    	if (selectableFields) {
    		JLabel l = new JLabel();
    		l.setText(" ");
    		gridBagConstraints = new java.awt.GridBagConstraints();
            gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
            gridBagConstraints.weightx = 0;
            gridBagConstraints.weighty = 1;
            gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
            gridBagConstraints.gridx = 1;
            gridBagConstraints.gridy = i;
            jPanel1.add(l, gridBagConstraints);
    	}
    	invalidate();
    	validate();
    	setSize(getWidth() - 1, getHeight() - 1);
    	setSize(getWidth() + 1, getHeight() + 1);
		onRowChanged((row));
	}

	protected abstract void onRowChanged(int row);
	
	/** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jLabel1 = new javax.swing.JLabel();
        rowSpinner = new javax.swing.JSpinner();
        jScrollPane1 = new javax.swing.JScrollPane();
        jPanel1 = new javax.swing.JPanel();
        jLabel2 = new javax.swing.JLabel();
        jTextField1 = new javax.swing.JTextField();

        setLayout(new java.awt.GridBagLayout());

        jLabel1.setText(" Row ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        add(jLabel1, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        add(rowSpinner, gridBagConstraints);

        jPanel1.setLayout(new java.awt.GridBagLayout());

        jLabel2.setText("jLabel2");
        jPanel1.add(jLabel2, new java.awt.GridBagConstraints());

        jTextField1.setText("jTextField1");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(jTextField1, gridBagConstraints);

        jScrollPane1.setViewportView(jPanel1);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(jScrollPane1, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTextField jTextField1;
    private javax.swing.JSpinner rowSpinner;
    // End of variables declaration//GEN-END:variables

}
