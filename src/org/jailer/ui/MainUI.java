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

import javax.swing.JTabbedPane;

import org.jailer.Jailer;

public class MainUI extends javax.swing.JFrame {

    private JTabbedPane jTabbedPane = null;
    private ExportUI exportUI = null;
    private BuildDatamodelUI buildDatamodelUI = null;
    private PrintDatamodelUI printDatamodelUI = null;
    private CreateDDLUI createDDLUI = null;

    /**
     * This method initializes 
     * 
     */
    public MainUI() {
    	super();
    	initialize();
        setVisible(true);
    }

    /**
     * This method initializes this
     * 
     */
    private void initialize() {
        this.setSize(new Dimension(520, 336));
        this.setContentPane(getJTabbedPane());
        this.setTitle("Jailer " + Jailer.VERSION);
        setDefaultCloseOperation(EXIT_ON_CLOSE);
        setLocation(200, 200);
    }
    
    /**
     * This method initializes jTabbedPane	
     * 	
     * @return javax.swing.JTabbedPane	
     */
    private JTabbedPane getJTabbedPane() {
        if (jTabbedPane == null) {
            jTabbedPane = new JTabbedPane();
            jTabbedPane.addTab("Build Model", null, getBuildDatamodelUI(), null);
            jTabbedPane.addTab("Print Model", null, getPrintDatamodelUI(), null);
            jTabbedPane.addTab("Create DDL", null, getCreateDDLUI(), null);
            jTabbedPane.addTab("Export", null, getExportUI(), null);
        }
        return jTabbedPane;
    }

    /**
     * This method initializes exportUI	
     * 	
     * @return org.jailer.ui.ExportUI	
     */
    private ExportUI getExportUI() {
        if (exportUI == null) {
            exportUI = new ExportUI();
        }
        return exportUI;
    }

    /**
     * This method initializes buildDatamodelUI	
     * 	
     * @return org.jailer.ui.BuildDatamodelUI	
     */
    private BuildDatamodelUI getBuildDatamodelUI() {
        if (buildDatamodelUI == null) {
            buildDatamodelUI = new BuildDatamodelUI();
        }
        return buildDatamodelUI;
    }

    /**
     * This method initializes printDatamodelUI	
     * 	
     * @return org.jailer.ui.PrintDatamodelUI	
     */
    private PrintDatamodelUI getPrintDatamodelUI() {
        if (printDatamodelUI == null) {
            printDatamodelUI = new PrintDatamodelUI();
        }
        return printDatamodelUI;
    }

    /**
     * This method initializes createDDLUI	
     * 	
     * @return org.jailer.ui.CreateDDLUI	
     */
    private CreateDDLUI getCreateDDLUI() {
        if (createDDLUI == null) {
            createDDLUI = new CreateDDLUI();
        }
        return createDDLUI;
    }

}  //  @jve:decl-index=0:visual-constraint="88,15"
