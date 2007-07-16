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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;

public class CreateDDLUI extends javax.swing.JPanel {

    private JPanel jContentPane = null;
    private JButton OK = null;
    private Settings theSettings;  //  @jve:decl-index=0:
    private JLabel jLabel = null;
    private JLabel jLabel1 = null;
    private JLabel jLabel2 = null;
    private JLabel jLabel3 = null;
    /**
     * This method initializes 
     * 
     */
    public CreateDDLUI() {
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
        
        getOK().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                List<String> args = new ArrayList<String>();
                args.add("create-ddl");
                UIUtil.runJailer(args, false, false, false);
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
            GridBagConstraints gridBagConstraints3 = new GridBagConstraints();
            gridBagConstraints3.gridx = 0;
            gridBagConstraints3.gridy = 2;
            jLabel3 = new JLabel();
            jLabel3.setText("   ");
            GridBagConstraints gridBagConstraints = new GridBagConstraints();
            gridBagConstraints.gridx = 0;
            gridBagConstraints.gridy = 0;
            jLabel2 = new JLabel();
            jLabel2.setText("   ");
            GridBagConstraints gridBagConstraints26 = new GridBagConstraints();
            gridBagConstraints26.gridx = 0;
            gridBagConstraints26.weightx = 1.0;
            gridBagConstraints26.anchor = GridBagConstraints.WEST;
            gridBagConstraints26.gridy = 1;
            GridBagConstraints gridBagConstraints2 = new GridBagConstraints();
            gridBagConstraints2.gridx = 2;
            gridBagConstraints2.gridy = 3;
            jLabel = new JLabel();
            jLabel.setText("   ");
            GridBagConstraints gridBagConstraints1 = new GridBagConstraints();
            gridBagConstraints1.gridy = 3;
            gridBagConstraints1.anchor = GridBagConstraints.EAST;
            jContentPane = new JPanel();
            jContentPane.setLayout(new GridBagLayout());
            jContentPane.add(getOK(), gridBagConstraints1);
            jContentPane.add(jLabel, gridBagConstraints2);
            jContentPane.add(getJLabel1(), gridBagConstraints26);
            jContentPane.add(jLabel2, gridBagConstraints);
            jContentPane.add(jLabel3, gridBagConstraints3);
        }
        return jContentPane;
    }

    /**
     * This method initializes OK   
     *  
     * @return javax.swing.JButton  
     */
    private JButton getOK() {
        if (OK == null) {
            OK = new JButton();
            OK.setText("Create DDL");
        }
        return OK;
    }

    /**
     * This method initializes jLabel1	
     * 	
     * @return javax.swing.JLabel	
     */
    private JLabel getJLabel1() {
        if (jLabel1 == null) {
            jLabel1 = new JLabel();
            jLabel1.setText("  Please execute the DDL-Script manually.");
        }
        return jLabel1;
    }

    public static void main(String args[]) throws FileNotFoundException, IOException {
        CreateDDLUI jailerUI = new CreateDDLUI();
        jailerUI.setVisible(true);
    }

}  //  @jve:decl-index=0:visual-constraint="10,10"
