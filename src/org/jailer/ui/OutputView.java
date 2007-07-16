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

import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;

public class OutputView extends javax.swing.JFrame {

    private JTextPane content = null;
    private JTextPane jTextPane = null;
    private JPanel jPanel = null;
    private JScrollPane jScrollPane = null;
    private JButton loadExportLog = null;
    private JButton loadSqlLog = null;
    private JButton loadExplainLog = null;
    /**
     * This method initializes 
     * 
     */
    public OutputView() {
        super();
        initialize();
    }

    /**
     * This method initializes this
     * 
     */
    private void initialize() {
        this.setSize(new Dimension(700, 640));
        this.setContentPane(getJPanel());
        this.setTitle("Jailer");
        getJTextPane().setFont(new Font("Monospaced", Font.PLAIN, 12));
        
        getLoadExportLog().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                try {
                    new FileView("export.log");
                } catch (Exception e1) {
                    e1.printStackTrace();
                }
            }
        });
        getLoadSqlLog().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                try {
                    new FileView("sql.log");
                } catch (Exception e1) {
                    e1.printStackTrace();
                }
            }
        });
        getLoadExplainLog().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                try {
                    new FileView("explain.log");
                } catch (Exception e1) {
                    e1.printStackTrace();
                }
            }
        });
    }

    /**
     * This method initializes jTextPane    
     *  
     * @return javax.swing.JTextPane    
     */
    private JTextPane getJTextPane() {
        if (jTextPane == null) {
            jTextPane = new JTextPane();
        }
        return jTextPane;
    }

    public OutputView(boolean showLogfileButton, boolean showExplainLogButton) {
        super();
        initialize();
        setLocation(300, 50);
        setTitle("Jailer Output");
        if (!showLogfileButton) {
            getLoadExportLog().setVisible(false);
            getLoadSqlLog().setVisible(false);
        }
        if (!showExplainLogButton) {
            getLoadExplainLog().setVisible(false);
        }
        setVisible(true);
    }

    public void setText(String output) {
        getJTextPane().setText(output);
    }

    /**
     * This method initializes jPanel	
     * 	
     * @return javax.swing.JPanel	
     */
    private JPanel getJPanel() {
        if (jPanel == null) {
            GridBagConstraints gridBagConstraints28 = new GridBagConstraints();
            gridBagConstraints28.gridx = 4;
            gridBagConstraints28.gridy = 1;
            gridBagConstraints28.insets = new Insets(0, 4, 0, 0);
            GridBagConstraints gridBagConstraints2 = new GridBagConstraints();
            gridBagConstraints2.gridx = 3;
            gridBagConstraints2.gridy = 1;
            gridBagConstraints2.insets = new Insets(0, 4, 0, 0);
            GridBagConstraints gridBagConstraints1 = new GridBagConstraints();
            gridBagConstraints1.gridx = 1;
            gridBagConstraints1.weightx = 1.0;
            gridBagConstraints1.anchor = GridBagConstraints.EAST;
            gridBagConstraints1.gridy = 1;
            GridBagConstraints gridBagConstraints = new GridBagConstraints();
            gridBagConstraints.fill = GridBagConstraints.BOTH;
            gridBagConstraints.gridy = 0;
            gridBagConstraints.weightx = 1.0;
            gridBagConstraints.weighty = 1.0;
            gridBagConstraints.gridx = 0;
            gridBagConstraints.gridwidth = 5;
            jPanel = new JPanel();
            jPanel.setLayout(new GridBagLayout());
            jPanel.add(getJScrollPane(), gridBagConstraints);
            jPanel.add(getLoadExportLog(), gridBagConstraints1);
            jPanel.add(getLoadSqlLog(), gridBagConstraints2);
            jPanel.add(getLoadExplainLog(), gridBagConstraints28);
        }
        return jPanel;
    }

    /**
     * This method initializes jScrollPane	
     * 	
     * @return javax.swing.JScrollPane	
     */
    private JScrollPane getJScrollPane() {
        if (jScrollPane == null) {
            jScrollPane = new JScrollPane();
            jScrollPane.setViewportView(getJTextPane());
            jScrollPane.setViewportView(getJTextPane());
        }
        return jScrollPane;
    }

    /**
     * This method initializes loadExportLog	
     * 	
     * @return javax.swing.JButton	
     */
    private JButton getLoadExportLog() {
        if (loadExportLog == null) {
            loadExportLog = new JButton();
            loadExportLog.setText("Open Export.log");
        }
        return loadExportLog;
    }

    /**
     * This method initializes loadSqlLog	
     * 	
     * @return javax.swing.JButton	
     */
    private JButton getLoadSqlLog() {
        if (loadSqlLog == null) {
            loadSqlLog = new JButton();
            loadSqlLog.setText("Open Sql.log");
        }
        return loadSqlLog;
    }

    /**
     * This method initializes loadExplainLog	
     * 	
     * @return javax.swing.JButton	
     */
    private JButton getLoadExplainLog() {
        if (loadExplainLog == null) {
            loadExplainLog = new JButton();
            loadExplainLog.setText("Open Explain.log");
        }
        return loadExplainLog;
    }
    
}  //  @jve:decl-index=0:visual-constraint="10,10"
