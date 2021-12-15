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
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import javax.swing.ImageIcon;
import javax.swing.JLabel;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.JComboBox2;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.util.SqlUtil;

/**
 *
 * @author RalfW
 */
public class AssociationPathPanel extends javax.swing.JPanel {

	final Association[] selectedAssociations;
	
    /**
     * Creates new form AssociationPathPanel
     * @param path the path
     * @param ignoredColor 
     * @param assocColor 
     * @param hasDepColor 
     * @param depOnColor 
     */
    public AssociationPathPanel(DataModel dataModel, List<Table> path, Color depOnColor, Color hasDepColor, Color assocColor, Color ignoredColor) {
        initComponents();
        Font font = new JLabel("normal").getFont();
		Font bold = new Font(font.getName(), font.getStyle() | Font.BOLD, font.getSize());
		
		okButton.setIcon(UIUtil.scaleIcon(okButton, okIcon));
		cancelButton.setIcon(UIUtil.scaleIcon(cancelButton, cancelIcon));
		
		jLabel1.setFont(bold);
		jLabel2.setFont(bold);
		jLabel3.setFont(bold);
		
		selectedAssociations = new Association[path.size() - 1];
		
		final Map<Association, Color> colorMap = new HashMap<Association, Color>();

		int y = 2;
		for (int i = path.size() - 1; i >= 0; --i) {
			final int finalI = i;
			Table table = path.get(i);
			JLabel label = new JLabel();
			label.setText(dataModel.getDisplayName(table) + "   ");
	        GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 1;
	        gridBagConstraints.gridy = y;
	        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
	        gridBagConstraints.insets = new java.awt.Insets(4, 4, 0, 4);
	        pathPanel.add(label, gridBagConstraints);

    		if (i > 0) {
        		final List<Association> associations = new ArrayList<Association>();
	    		Table to = path.get(i - 1);
	    		for (Association a: table.associations) {
	    			if (a.destination.equals(to)) {
	    				associations.add(a);
	    				Color c;
	    				if (a.isIgnored()) {
	    					c = ignoredColor;
	    				} else if (a.isInsertDestinationBeforeSource()) {
	    					c = depOnColor;
	    				} else if (a.isInsertSourceBeforeDestination()) {
	    					c = hasDepColor;
	    				} else {
	    					c = assocColor;
	    				}
	    				colorMap.put(a, c);
	    			}
	    		}
	        	Association first = associations.get(0);
	        	selectedAssociations[i - 1] = first;

				label = new JLabel();
				String jc = first.getUnrestrictedJoinCondition();
				if (first.reversed) {
					jc = SqlUtil.reversRestrictionCondition(jc);
				}
				label.setText(jc);
				label.setForeground(colorMap.get(first));
		        gridBagConstraints = new java.awt.GridBagConstraints();
		        gridBagConstraints.gridx = 3;
		        gridBagConstraints.gridy = y;
		        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
		        gridBagConstraints.weightx = 1.0;
		        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
		        gridBagConstraints.insets = new java.awt.Insets(4, 4, 0, 4);
		        pathPanel.add(label, gridBagConstraints);
		        
		        final JLabel conditionLabel = label;

		        gridBagConstraints = new java.awt.GridBagConstraints();
		        gridBagConstraints.gridx = 2;
		        gridBagConstraints.gridy = y;
		        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
		        gridBagConstraints.insets = new java.awt.Insets(4, 4, 0, 4);
	    		
		        if (associations.size() == 1) {
					label = new JLabel();
					label.setText(associations.get(0).getName() + "   ");
					label.setForeground(colorMap.get(first));
			        pathPanel.add(label, gridBagConstraints);
	        	} else {
	        		needToAsk = true;
	        		Vector<String> model = new Vector<String>();
	        		for (Association a: associations) {
	        			model.add(a.getName());
	        		}
	        		final JComboBox2 comboBox = new JComboBox2(model);
	        		comboBox.setSelectedIndex(0);
	        		comboBox.addItemListener(new ItemListener() {
						@Override
						public void itemStateChanged(ItemEvent e) {
							int si = comboBox.getSelectedIndex();
							if (si >= 0) {
								Association association = associations.get(si);
								selectedAssociations[finalI - 1] = association;
								String jc = association.getUnrestrictedJoinCondition();
								if (association.reversed) {
									jc = SqlUtil.reversRestrictionCondition(jc);
								}
								conditionLabel.setText(jc);
								conditionLabel.setForeground(colorMap.get(association));
							}
						}
					});
	        		gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
	        		pathPanel.add(comboBox, gridBagConstraints);
	        	}
    		}
    		
	        ++y;
		}
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

        jPanel1 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        pathPanel = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jSeparator1 = new javax.swing.JSeparator();
        dummyPanel = new javax.swing.JPanel();
        jPanel2 = new javax.swing.JPanel();
        okButton = new javax.swing.JButton();
        cancelButton = new javax.swing.JButton();

        setLayout(new java.awt.GridBagLayout());

        jPanel1.setLayout(new javax.swing.BoxLayout(jPanel1, javax.swing.BoxLayout.LINE_AXIS));

        pathPanel.setLayout(new java.awt.GridBagLayout());

        jLabel1.setText("Table ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
        pathPanel.add(jLabel1, gridBagConstraints);

        jLabel2.setText("Association ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
        pathPanel.add(jLabel2, gridBagConstraints);

        jLabel3.setText("Condition");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
        pathPanel.add(jLabel3, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        pathPanel.add(jSeparator1, gridBagConstraints);

        javax.swing.GroupLayout dummyPanelLayout = new javax.swing.GroupLayout(dummyPanel);
        dummyPanel.setLayout(dummyPanelLayout);
        dummyPanelLayout.setHorizontalGroup(
            dummyPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 100, Short.MAX_VALUE)
        );
        dummyPanelLayout.setVerticalGroup(
            dummyPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 100, Short.MAX_VALUE)
        );

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 300;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.weighty = 1.0;
        pathPanel.add(dummyPanel, gridBagConstraints);

        jScrollPane1.setViewportView(pathPanel);

        jPanel1.add(jScrollPane1);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(jPanel1, gridBagConstraints);

        jPanel2.setLayout(new java.awt.GridBagLayout());

        okButton.setText("Open Path");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        jPanel2.add(okButton, gridBagConstraints);

        cancelButton.setText("Cancel");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 0);
        jPanel2.add(cancelButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
        add(jPanel2, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    public javax.swing.JButton cancelButton;
    private javax.swing.JPanel dummyPanel;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JSeparator jSeparator1;
    public javax.swing.JButton okButton;
    private javax.swing.JPanel pathPanel;
    // End of variables declaration//GEN-END:variables

    protected boolean needToAsk = false;
    protected boolean ok = false;
    

	private static ImageIcon okIcon;
	private static ImageIcon cancelIcon;
	
	static {
        // load images
        okIcon = UIUtil.readImage("/buttonok.png");
        cancelIcon = UIUtil.readImage("/buttoncancel.png");
	}
}
