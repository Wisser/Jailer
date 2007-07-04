package org.jailer.ui;

import java.awt.Dimension;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.JTextPane;

import org.jailer.util.PrintUtil;
import javax.swing.JScrollPane;

public class FileView extends javax.swing.JFrame {

    private JTextPane content = null;
    private JScrollPane jScrollPane = null;
    private JTextPane jTextPane = null;
    /**
     * This method initializes 
     * 
     */
    public FileView() {
    	super();
    	initialize();
    }

    /**
     * This method initializes this
     * 
     */
    private void initialize() {
        this.setSize(new Dimension(491, 286));
        this.setContentPane(getJScrollPane());
        this.setTitle("Jailer");
    		
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
        }
        return jScrollPane;
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

    public static void main(String args[]) throws FileNotFoundException, IOException {
        FileView jailerUI = new FileView();
        jailerUI.setVisible(true);
        
        jailerUI.getJTextPane().setText(PrintUtil.applyTemplate("export.log", null));
    }

}  //  @jve:decl-index=0:visual-constraint="10,10"
