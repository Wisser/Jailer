package org.jailer.ui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.JPanel;
import javax.swing.JTextArea;

import org.jailer.util.PrintUtil;

public class CsvFileView extends JPanel {

    private JTextArea jTextArea = null;

    /**
     * This method initializes 
     * 
     */
    public CsvFileView() {
    	super();
    	initialize();
    }

    /**
     * This method initializes this
     * 
     */
    private void initialize() {
        this.setLayout(new BorderLayout());
        this.setSize(new Dimension(472, 284));
        this.add(getJTextArea(), BorderLayout.CENTER);
        
    }

    /**
     * This method initializes jTextArea	
     * 	
     * @return javax.swing.JTextArea	
     */
    private JTextArea getJTextArea() {
        if (jTextArea == null) {
            jTextArea = new JTextArea();
        }
        return jTextArea;
    }

    public void setContent(String content) {
        getJTextArea().setText(content);
    }

}  //  @jve:decl-index=0:visual-constraint="190,18"
