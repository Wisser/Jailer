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

import java.awt.Font;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

/**
 * Parameter editor.
 * 
 * @author Ralf Wisser
 */
public class ParameterEditor extends javax.swing.JDialog {

    private Font font = new JLabel("normal").getFont();
    
    /**
     * Non-bold font.
     */
	private Font nonBoldFont = new Font(font.getName(), font.getStyle() & ~Font.BOLD, font.getSize());
	
	/**
     * Non-bold font + italic.
     */
	private Font nonBoldItalicFont = new Font(font.getName(), font.getStyle() & ~Font.BOLD | Font.ITALIC, font.getSize());
	
	/**
	 * Maps parameters to the text-field.
	 */
	public Map<String, JTextField> textfieldsPerParameter = new HashMap<String, JTextField>();
	
	/** Creates new form */
    public ParameterEditor(Frame parent) {
        super(parent, true);
        initComponents();
    }

    /**
     * Creates the editor pane.
     */
	public JComponent createPane(Set<String> parameters) {
		filterPane.removeAll();
		int y = 0;
		java.awt.GridBagConstraints gridBagConstraints;
		
		for (Iterator<String> i = parameters.iterator(); i.hasNext(); ) {
			String parameter = i.next();
			
			javax.swing.JLabel label;

			label = new javax.swing.JLabel();
			label.setText(parameter);
				
			gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 1;
	        gridBagConstraints.gridy = y;
	        gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
	        gridBagConstraints.weightx = 0.0;
//	        gridBagConstraints.weighty = i.hasNext()? 0.0 : 1.0;
	        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
	        filterPane.add(label, gridBagConstraints);
	        
	        label = new javax.swing.JLabel();
			label.setText(" = ");
			label.setFont(nonBoldFont);
				
			gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 2;
	        gridBagConstraints.gridy = y;
	        gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
	        gridBagConstraints.weightx = 0.0;
//	        gridBagConstraints.weighty = i.hasNext()? 0.0 : 1.0;
	        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
	        filterPane.add(label, gridBagConstraints);
		        
	        javax.swing.JTextField textField = new javax.swing.JTextField();
	        textfieldsPerParameter.put(parameter, textField);
	        if (firstTextField == null) {
	        	firstTextField = textField;
	        }
	        
	        gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 3;
	        gridBagConstraints.gridy = y;
	        gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
	        gridBagConstraints.weightx = 1.0;
//	        gridBagConstraints.weighty = i.hasNext()? 0.0 : 1.0;
	        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
	        gridBagConstraints.insets = new Insets(1, 0, 0, 0);
	        filterPane.add(textField, gridBagConstraints);
	        
	        ++y;
		}
		if (y == 0) {
			javax.swing.JLabel label = new javax.swing.JLabel();
			label.setText("no parameters");
			label.setFont(nonBoldItalicFont);
			
			gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 1;
	        gridBagConstraints.gridy = y++;
	        gridBagConstraints.weightx = 1.0;
	        gridBagConstraints.fill = GridBagConstraints.NONE;
	        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
	        filterPane.add(label, gridBagConstraints);
		}
		gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = y++;
        gridBagConstraints.weightx = 0.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.fill = GridBagConstraints.NONE;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        filterPane.add(new JPanel(), gridBagConstraints);
        return filterScrollPane;
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

        pack();
    }// </editor-fold>//GEN-END:initComponents
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JPanel filterPane;
    private javax.swing.JScrollPane filterScrollPane;
    private javax.swing.JPanel jPanel1;
    // End of variables declaration//GEN-END:variables

	public JTextField firstTextField = null;
    
}
