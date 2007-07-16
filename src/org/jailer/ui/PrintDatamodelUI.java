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
package org.jailer.ui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

public class PrintDatamodelUI extends javax.swing.JPanel {

    private JPanel jContentPane = null;
    private JLabel jLabel31 = null;
    private JTextField restrictionModel = null;
    private JButton jButton = null;
    private JLabel jLabel4 = null;
    private JComboBox settings = null;
    private JButton OK = null;
    private JLabel jLabel5 = null;
    private JLabel jLabel51 = null;
    
    private Settings theSettings;  //  @jve:decl-index=0:
    private JLabel jLabel511 = null;
    private JButton findRestrictionModel = null;
    /**
     * This method initializes 
     * 
     */
    public PrintDatamodelUI() {
        super();
        initialize();
    }

    /**
     * This method initializes this
     * 
     */
    private void initialize() {
        BorderLayout borderLayout = new BorderLayout();
        borderLayout.setHgap(0);
        this.setLayout(borderLayout);
        this.setSize(new Dimension(491, 255));
        this.add(getJContentPane(), BorderLayout.NORTH);
        getSettings().setEditable(true);
        Map<String, JTextField> fields = new HashMap<String, JTextField>();
        fields.put("restrictionModel", getRestrictionModel());
        theSettings = new Settings(".printmodel.ui", fields);
        ComboBoxModel aModel = new DefaultComboBoxModel(theSettings.getSettingNames());
        getSettings().setModel(aModel);
        if (theSettings.currentSetting != null) {
            theSettings.restore(theSettings.currentSetting);
            for (Object item: theSettings.getSettingNames()) {
                if (theSettings.currentSetting.equals(item)) {
                    getSettings().setSelectedItem(item);
                }
            }
        }
        getSettings().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                String name = "" + getSettings().getSelectedItem();
                theSettings.restore(name);
            }
        });
        getJButton().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                String name = "" + getSettings().getSelectedItem();
                theSettings.save(name);
                ComboBoxModel aModel = new DefaultComboBoxModel(theSettings.getSettingNames());
                getSettings().setModel(aModel);
                getSettings().setSelectedItem(name);
            }
        });
        getOK().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                String name = "" + getSettings().getSelectedItem();
                theSettings.save(name);
                List<String> args = new ArrayList<String>();
                args.add("print-datamodel");
                String rm = getRestrictionModel().getText();
                if (rm != null && rm.trim().length() > 0) {
                    args.add(rm);
                }
                UIUtil.runJailer(args, false, false, false);
            }
        });
        getFindRestrictionModel().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                String fn = UIUtil.choseFile("restrictionmodel", "restriction models", "csv", PrintDatamodelUI.this);
                if (fn != null) {
                    getRestrictionModel().setText(fn);
                }
            }
        });
     }
    
    /**
     * This method initializes jContentPane 
     *  
     * @return javax.swing.JPanel   
     */
    private JPanel getJContentPane() {
        if (jContentPane == null) {
            GridBagConstraints gridBagConstraints24 = new GridBagConstraints();
            gridBagConstraints24.gridx = 3;
            gridBagConstraints24.gridy = 8;
            GridBagConstraints gridBagConstraints22 = new GridBagConstraints();
            gridBagConstraints22.gridx = 1;
            gridBagConstraints22.gridy = 0;
            jLabel511 = new JLabel();
            jLabel511.setText("   ");
            GridBagConstraints gridBagConstraints21 = new GridBagConstraints();
            gridBagConstraints21.gridx = 1;
            gridBagConstraints21.gridy = 2;
            jLabel51 = new JLabel();
            jLabel51.setText("   ");
            GridBagConstraints gridBagConstraints20 = new GridBagConstraints();
            gridBagConstraints20.gridx = 4;
            gridBagConstraints20.gridy = 6;
            jLabel5 = new JLabel();
            jLabel5.setText("   ");
            GridBagConstraints gridBagConstraints19 = new GridBagConstraints();
            gridBagConstraints19.gridx = 2;
            gridBagConstraints19.fill = GridBagConstraints.HORIZONTAL;
            gridBagConstraints19.gridy = 11;
            gridBagConstraints19.insets = new Insets(10, 10, 0, 0);
            GridBagConstraints gridBagConstraints18 = new GridBagConstraints();
            gridBagConstraints18.fill = GridBagConstraints.BOTH;
            gridBagConstraints18.gridy = 1;
            gridBagConstraints18.weightx = 1.0;
            gridBagConstraints18.gridx = 1;
            GridBagConstraints gridBagConstraints17 = new GridBagConstraints();
            gridBagConstraints17.gridx = 0;
            gridBagConstraints17.anchor = GridBagConstraints.WEST;
            gridBagConstraints17.fill = GridBagConstraints.HORIZONTAL;
            gridBagConstraints17.gridy = 1;
            gridBagConstraints17.insets = new Insets(0, 10, 0, 0);
            jLabel4 = new JLabel();
            jLabel4.setText("Setting");
            GridBagConstraints gridBagConstraints13 = new GridBagConstraints();
            gridBagConstraints13.gridx = 2;
            gridBagConstraints13.anchor = GridBagConstraints.WEST;
            gridBagConstraints13.gridy = 1;
            gridBagConstraints13.gridwidth = 1;
            gridBagConstraints13.fill = GridBagConstraints.HORIZONTAL;
            gridBagConstraints13.insets = new Insets(0, 10, 0, 0);
            GridBagConstraints gridBagConstraints10 = new GridBagConstraints();
            gridBagConstraints10.fill = GridBagConstraints.BOTH;
            gridBagConstraints10.gridy = 8;
            gridBagConstraints10.weightx = 1.0;
            gridBagConstraints10.gridx = 1;
            gridBagConstraints10.gridwidth = 2;
            GridBagConstraints gridBagConstraints4 = new GridBagConstraints();
            gridBagConstraints4.gridx = 0;
            gridBagConstraints4.anchor = GridBagConstraints.WEST;
            gridBagConstraints4.ipadx = 8;
            gridBagConstraints4.insets = new Insets(0, 10, 0, 0);
            gridBagConstraints4.gridy = 8;
            jLabel31 = new JLabel();
            jLabel31.setText("restriction model");
            jContentPane = new JPanel();
            jContentPane.setLayout(new GridBagLayout());
            jContentPane.add(jLabel31, gridBagConstraints4);
            jContentPane.add(getRestrictionModel(), gridBagConstraints10);
            jContentPane.add(getJButton(), gridBagConstraints13);
            jContentPane.add(jLabel4, gridBagConstraints17);
            jContentPane.add(getSettings(), gridBagConstraints18);
            jContentPane.add(getOK(), gridBagConstraints19);
            jContentPane.add(jLabel5, gridBagConstraints20);
            jContentPane.add(jLabel51, gridBagConstraints21);
            jContentPane.add(jLabel511, gridBagConstraints22);
            jContentPane.add(getFindRestrictionModel(), gridBagConstraints24);
        }
        return jContentPane;
    }

    /**
     * This method initializes restrictionModel  
     *  
     * @return javax.swing.JTextField   
     */
    private JTextField getRestrictionModel() {
        if (restrictionModel == null) {
            restrictionModel = new JTextField();
        }
        return restrictionModel;
    }

    /**
     * This method initializes jButton  
     *  
     * @return javax.swing.JButton  
     */
    private JButton getJButton() {
        if (jButton == null) {
            jButton = new JButton();
            jButton.setText("Save Settings");
        }
        return jButton;
    }

    /**
     * This method initializes settings 
     *  
     * @return javax.swing.JComboBox    
     */
    private JComboBox getSettings() {
        if (settings == null) {
            settings = new JComboBox();
        }
        return settings;
    }

    /**
     * This method initializes OK   
     *  
     * @return javax.swing.JButton  
     */
    private JButton getOK() {
        if (OK == null) {
            OK = new JButton();
            OK.setText("Print Datamodel");
        }
        return OK;
    }

    /**
     * This method initializes findRestrictionModel	
     * 	
     * @return javax.swing.JButton	
     */
    private JButton getFindRestrictionModel() {
        if (findRestrictionModel == null) {
            findRestrictionModel = new JButton();
            findRestrictionModel.setText("...");
        }
        return findRestrictionModel;
    }

    public static void main(String args[]) throws FileNotFoundException, IOException {
        PrintDatamodelUI jailerUI = new PrintDatamodelUI();
        jailerUI.setVisible(true);
    }

}  //  @jve:decl-index=0:visual-constraint="10,10"
