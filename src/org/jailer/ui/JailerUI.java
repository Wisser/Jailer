package org.jailer.ui;

import java.awt.Dimension;
import java.io.FileNotFoundException;
import java.io.IOException;

import org.jailer.Jailer;
import javax.swing.JTabbedPane;
import javax.swing.SwingUtilities;

public class JailerUI extends javax.swing.JFrame {

    private JTabbedPane jTabbedPane = null;
    private ExportUI exportUI = null;

    /**
     * This method initializes 
     * 
     */
    public JailerUI() {
    	super();
    	initialize();
    }

    /**
     * This method initializes this
     * 
     */
    private void initialize() {
        this.setSize(new Dimension(520, 312));
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

    public static void main(String args[]) throws FileNotFoundException, IOException {
        JailerUI jailerUI = new JailerUI();
        jailerUI.setVisible(true);
    }

}  //  @jve:decl-index=0:visual-constraint="88,15"
