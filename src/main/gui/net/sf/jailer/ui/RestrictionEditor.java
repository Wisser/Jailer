/*
 * Copyright 2007 - 2025 Ralf Wisser.
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
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

/**
 * Editor for a restricted association.
 *
 * @author Ralf Wisser
 */
public class RestrictionEditor extends javax.swing.JPanel {

	private Color origGBColor;
	
	/** Creates new form RestrictionEditor */
	public RestrictionEditor() {
		initComponents(); UIUtil.initComponents(this);
		
		ImageIcon scaledWarnIcon = UIUtil.scaleIcon(this, warnIcon, 1);
		
		restrictedDependencyWarning.setIcon(scaledWarnIcon);
		fk20DisabledHintLabel.setIcon(scaledWarnIcon);
				
		ButtonGroup buttonGroup = new ButtonGroup();

		buttonGroup.add(restricted);
		buttonGroup.add(ignore);
		
		Color white = Colors.Color_255_255_255_200;
		jPanel1.setBackground(white);
		jPanel4.setBackground(white);
		jPanel5.setBackground(white);
		jPanel8.setBackground(white);
		
		origGBColor = restriction.getBackground();
		restriction.getDocument().addDocumentListener(new DocumentListener() {
			@Override
			public void removeUpdate(DocumentEvent e) {
				setBG();
			}
			@Override
			public void insertUpdate(DocumentEvent e) {
				setBG();
			}
			@Override
			public void changedUpdate(DocumentEvent e) {
				setBG();
			}
			private void setBG() {
				restriction.setBackground(Colors.Color_255_255_220);
			}
		});
		restriction.addKeyListener(new KeyListener() {
			@Override
			public void keyTyped(KeyEvent e) {
				if (e.getKeyChar() == '\n') {
					apply.doClick();
				}
			}
			@Override
			public void keyReleased(KeyEvent e) {
			}
			@Override
			public void keyPressed(KeyEvent arg0) {
			}
		});
	}

	public void resetBGColor() {
		restriction.setBackground(origGBColor);
	}

	/** This method is called from within the constructor to
	 * initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is
	 * always regenerated by the Form Editor.
	 */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        joinCondition2 = new javax.swing.JTextField();
        columnsA = new javax.swing.JLabel();
        columnsB = new javax.swing.JLabel();
        apply = new javax.swing.JButton();
        openRestrictionConditionEditor = new javax.swing.JLabel();
        jPanel2 = new javax.swing.JPanel();
        jPanel3 = new javax.swing.JPanel();
        jPanel7 = new javax.swing.JPanel();
        jLabel4 = new javax.swing.JLabel();
        jPanel1 = new javax.swing.JPanel();
        ignore = new javax.swing.JRadioButton();
        restrictedDependencyWarning = new javax.swing.JLabel();
        jPanel8 = new javax.swing.JPanel();
        restricted = new javax.swing.JRadioButton();
        jPanel9 = new javax.swing.JPanel();
        restriction = new javax.swing.JTextField();
        jPanel10 = new javax.swing.JPanel();
        fkToNullCheckBox = new javax.swing.JCheckBox();
        jLabel5 = new javax.swing.JLabel();
        fk20DisabledHintLabel = new javax.swing.JLabel();
        jPanel11 = new javax.swing.JPanel();
        jPanel4 = new javax.swing.JPanel();
        source = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        type = new javax.swing.JLabel();
        cardinality = new javax.swing.JLabel();
        destination = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jLabel6 = new javax.swing.JLabel();
        jPanel5 = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        joinCondition = new javax.swing.JLabel();
        xPanel = new javax.swing.JPanel();
        xMappingLabel = new javax.swing.JLabel();
        xAggregateCheckBox = new javax.swing.JCheckBox();
        xMappingButton = new javax.swing.JButton();
        aggTypejLabel = new javax.swing.JLabel();
        aggTypePanel = new javax.swing.JPanel();
        jPanel6 = new javax.swing.JPanel();

        joinCondition2.setEditable(false);
        joinCondition2.setText("jTextField1");
        joinCondition2.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        joinCondition2.setCaretPosition(1);
        joinCondition2.setFocusable(false);
        joinCondition2.setRequestFocusEnabled(false);

        columnsA.setText("V");

        columnsB.setText("V");

        apply.setText("apply");

        openRestrictionConditionEditor.setText("jLabel5");
        openRestrictionConditionEditor.setToolTipText("open editor");

        setOpaque(false);
        setLayout(new java.awt.GridBagLayout());

        jPanel2.setBackground(/* Renaming also in *.form! */ Colors.Color_255_255_255);
        jPanel2.setOpaque(false);
        jPanel2.setLayout(new java.awt.GridBagLayout());

        jPanel3.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 0, 0));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 44;
        gridBagConstraints.gridwidth = 10;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel2.add(jPanel3, gridBagConstraints);

        jPanel7.setOpaque(false);
        jPanel7.setLayout(new java.awt.GridBagLayout());

        jLabel4.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 50;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.weightx = 1.0;
        jPanel7.add(jLabel4, gridBagConstraints);

        jPanel1.setBackground(/* Renaming also in *.form! */ Colors.Color_255_255_255);
        jPanel1.setOpaque(false);
        jPanel1.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 0, 0));

        ignore.setBackground(Colors.Color_NeigbBG);
        ignore.setText("Disabled    ");
        ignore.setOpaque(true);
        jPanel1.add(ignore);

        restrictedDependencyWarning.setBackground(Colors.Color_NeigbBG);
        restrictedDependencyWarning.setForeground(/* Renaming also in *.form! */ Colors.Color_255_0_0);
        restrictedDependencyWarning.setText("Restricted Dependency! ");
        restrictedDependencyWarning.setOpaque(true);
        jPanel1.add(restrictedDependencyWarning);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 10;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel7.add(jPanel1, gridBagConstraints);

        jPanel8.setBackground(/* Renaming also in *.form! */ Colors.Color_255_255_255);
        jPanel8.setOpaque(false);
        jPanel8.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 0, 0));

        restricted.setBackground(Colors.Color_NeigbBG);
        restricted.setText("Enabled. Restricted by   ");
        restricted.setOpaque(true);
        jPanel8.add(restricted);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.VERTICAL;
        gridBagConstraints.insets = new java.awt.Insets(3, 0, 0, 0);
        jPanel7.add(jPanel8, gridBagConstraints);

        jPanel9.setOpaque(false);
        jPanel9.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 0, 0));

        restriction.setBackground(Colors.Color_255_255_255);
        restriction.setColumns(32);
        restriction.setToolTipText("<html>\nSQL expression to restrict the join condition. <br>\nKeep empty to reach all associated rows.\n</html>");
        jPanel9.add(restriction);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridheight = 10;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
        gridBagConstraints.insets = new java.awt.Insets(3, 0, 0, 0);
        jPanel7.add(jPanel9, gridBagConstraints);

        jPanel10.setOpaque(false);
        jPanel10.setLayout(new java.awt.GridBagLayout());

        fkToNullCheckBox.setBackground(Colors.Color_NeigbBG);
        fkToNullCheckBox.setText("Set foreign key columns to null ");
        fkToNullCheckBox.setToolTipText("<html><i>on Export</i>: Set all foreign keys to null to which the row with the corresponding primary key is not exported. <br><hr>\n<i>on Delete</i>: Set all foreign keys in the rows that cannot be deleted to null when the row with the corresponding primary key is deleted.</html>");
        fkToNullCheckBox.setOpaque(true);
        fkToNullCheckBox.addComponentListener(new java.awt.event.ComponentAdapter() {
            public void componentShown(java.awt.event.ComponentEvent evt) {
                fkToNullCheckBoxComponentShown(evt);
            }
        });
        fkToNullCheckBox.addPropertyChangeListener(new java.beans.PropertyChangeListener() {
            public void propertyChange(java.beans.PropertyChangeEvent evt) {
                fkToNullCheckBoxPropertyChange(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel10.add(fkToNullCheckBox, gridBagConstraints);

        jLabel5.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 12, 0);
        jPanel10.add(jLabel5, gridBagConstraints);

        fk20DisabledHintLabel.setBackground(Colors.Color_NeigbBG);
        fk20DisabledHintLabel.setForeground(/* Renaming also in *.form! */ Colors.Color_255_0_0);
        fk20DisabledHintLabel.setText("(Foreign key is not nullable)");
        fk20DisabledHintLabel.setOpaque(true);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        jPanel10.add(fk20DisabledHintLabel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.gridwidth = 6;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel7.add(jPanel10, gridBagConstraints);

        jPanel11.setOpaque(false);
        jPanel11.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 5;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridheight = 10;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
        jPanel7.add(jPanel11, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.gridwidth = 10;
        gridBagConstraints.insets = new java.awt.Insets(0, 20, 0, 0);
        jPanel2.add(jPanel7, gridBagConstraints);

        jPanel4.setOpaque(false);
        jPanel4.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 4, 0));

        source.setBackground(Colors.Color_NeigbBG);
        source.setFont(source.getFont().deriveFont(source.getFont().getStyle() | java.awt.Font.BOLD, source.getFont().getSize()+1));
        source.setText("jLabel3");
        source.setOpaque(true);
        jPanel4.add(source);

        jLabel2.setBackground(Colors.Color_NeigbBG);
        jLabel2.setText("A");
        jLabel2.setOpaque(true);
        jPanel4.add(jLabel2);

        type.setBackground(Colors.Color_NeigbBG);
        type.setFont(type.getFont().deriveFont(type.getFont().getSize()+1f));
        type.setText("jLabel3");
        type.setOpaque(true);
        jPanel4.add(type);

        cardinality.setBackground(Colors.Color_NeigbBG);
        cardinality.setFont(cardinality.getFont().deriveFont(cardinality.getFont().getSize()+1f));
        cardinality.setText("jLabel3");
        cardinality.setOpaque(true);
        jPanel4.add(cardinality);

        destination.setBackground(Colors.Color_NeigbBG);
        destination.setFont(destination.getFont().deriveFont(destination.getFont().getStyle() | java.awt.Font.BOLD, destination.getFont().getSize()+1));
        destination.setText("jLabel3");
        destination.setOpaque(true);
        jPanel4.add(destination);

        jLabel3.setBackground(Colors.Color_NeigbBG);
        jLabel3.setText("B");
        jLabel3.setOpaque(true);
        jPanel4.add(jLabel3);

        jLabel6.setBackground(Colors.Color_NeigbBG);
        jLabel6.setText("as");
        jLabel6.setOpaque(true);
        jPanel4.add(jLabel6);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridwidth = 20;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel2.add(jPanel4, gridBagConstraints);

        jPanel5.setOpaque(false);
        jPanel5.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 4, 0));

        jLabel1.setBackground(Colors.Color_NeigbBG);
        jLabel1.setText("on");
        jPanel5.add(jLabel1);

        joinCondition.setBackground(Colors.Color_NeigbBG);
        joinCondition.setFont(joinCondition.getFont().deriveFont(joinCondition.getFont().getSize()+1f));
        joinCondition.setText("jLabel5");
        joinCondition.setOpaque(true);
        jPanel5.add(joinCondition);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 20;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 18, 0, 0);
        jPanel2.add(jPanel5, gridBagConstraints);

        xPanel.setOpaque(false);
        xPanel.setLayout(new java.awt.GridBagLayout());

        xMappingLabel.setBackground(Colors.Color_NeigbBG);
        xMappingLabel.setFont(xMappingLabel.getFont().deriveFont(xMappingLabel.getFont().getStyle() | java.awt.Font.BOLD, xMappingLabel.getFont().getSize()+1));
        xMappingLabel.setText(" ? Mapping");
        xMappingLabel.setOpaque(true);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 12;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        xPanel.add(xMappingLabel, gridBagConstraints);

        xAggregateCheckBox.setBackground(Colors.Color_NeigbBG);
        xAggregateCheckBox.setText("Aggregate");
        xAggregateCheckBox.setOpaque(true);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 20, 6, 0);
        xPanel.add(xAggregateCheckBox, gridBagConstraints);

        xMappingButton.setText("Column Mapping");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 40, 0, 0);
        xPanel.add(xMappingButton, gridBagConstraints);

        aggTypejLabel.setText(" as ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 20, 8, 4);
        xPanel.add(aggTypejLabel, gridBagConstraints);

        aggTypePanel.setOpaque(false);
        aggTypePanel.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 0, 0));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 8, 0);
        xPanel.add(aggTypePanel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 40;
        gridBagConstraints.gridwidth = 20;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(16, 0, 8, 0);
        jPanel2.add(xPanel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 8, 0, 0);
        add(jPanel2, gridBagConstraints);

        jPanel6.setOpaque(false);
        jPanel6.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 0, 0));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 15;
        gridBagConstraints.gridwidth = 10;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        add(jPanel6, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    private void fkToNullCheckBoxPropertyChange(java.beans.PropertyChangeEvent evt) {//GEN-FIRST:event_fkToNullCheckBoxPropertyChange
    	updateHint();
    }//GEN-LAST:event_fkToNullCheckBoxPropertyChange

	public void updateHint() {
		fk20DisabledHintLabel.setVisible(fkToNullCheckBox.isVisible() && !fkToNullCheckBox.isEnabled() && !fkToNullCheckBox.isSelected());
    	fk20DisabledHintLabel.setToolTipText(fkToNullCheckBox.getToolTipText());
	}

    private void fkToNullCheckBoxComponentShown(java.awt.event.ComponentEvent evt) {//GEN-FIRST:event_fkToNullCheckBoxComponentShown
    	fkToNullCheckBoxPropertyChange(null);
    }//GEN-LAST:event_fkToNullCheckBoxComponentShown

    // Variables declaration - do not modify//GEN-BEGIN:variables
    public javax.swing.JPanel aggTypePanel;
    public javax.swing.JLabel aggTypejLabel;
    public javax.swing.JButton apply;
    public javax.swing.JLabel cardinality;
    javax.swing.JLabel columnsA;
    javax.swing.JLabel columnsB;
    public javax.swing.JLabel destination;
    public javax.swing.JLabel fk20DisabledHintLabel;
    public javax.swing.JCheckBox fkToNullCheckBox;
    public javax.swing.JRadioButton ignore;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JPanel jPanel1;
    public javax.swing.JPanel jPanel10;
    public javax.swing.JPanel jPanel11;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanel6;
    public javax.swing.JPanel jPanel7;
    private javax.swing.JPanel jPanel8;
    private javax.swing.JPanel jPanel9;
    javax.swing.JLabel joinCondition;
    javax.swing.JTextField joinCondition2;
    javax.swing.JLabel openRestrictionConditionEditor;
    public javax.swing.JRadioButton restricted;
    public javax.swing.JLabel restrictedDependencyWarning;
    public javax.swing.JTextField restriction;
    public javax.swing.JLabel source;
    public javax.swing.JLabel type;
    public javax.swing.JCheckBox xAggregateCheckBox;
    public javax.swing.JButton xMappingButton;
    public javax.swing.JLabel xMappingLabel;
    public javax.swing.JPanel xPanel;
    // End of variables declaration//GEN-END:variables
	
	private static final long serialVersionUID = -6735468124049608700L;
	
	static ImageIcon warnIcon;
    static {
        // load images
        warnIcon = UIUtil.readImage("/wanr.png");
    }

}
