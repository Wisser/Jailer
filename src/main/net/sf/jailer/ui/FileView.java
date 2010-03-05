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

import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.Font;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import net.sf.jailer.util.PrintUtil;

/**
 * Renders a text files.
 * 
 * @author Ralf Wisser
 */
public class FileView extends javax.swing.JDialog {

	/**
     * Component for rendering the file content.
     */
	private JTextArea jTextPane = null;
	
	/**
	 * Scroll-pane around content.
	 */
    private JScrollPane jScrollPane = null;
    
    /**
     * Constructor. 
     * 
     * @param owner the enclosing component.
     */
    public FileView(Dialog owner) {
    	super(owner);
    	initialize();
    	setModal(true);
        UIUtil.initPeer();
    }

    /**
     * Constructor. 
     * 
     * @param owner the enclosing component.
     * @param file the file to render
     */
    public FileView(Dialog owner, String file) throws FileNotFoundException, IOException {
        super(owner);
        setModal(true);
        try {
			initialize();
			setTitle(file);
			getJTextPane().setText(PrintUtil.loadFile(file));
	        UIUtil.initPeer();
			setVisible(true);
		} catch (Throwable t) {
			UIUtil.showException(owner, "Error", t);
		}
    }

    /**
     * Creates components.
     */
    private void initialize() {
        this.setSize(new Dimension(700, 600));
        this.setContentPane(getJScrollPane());
        this.setTitle("Jailer");
        setLocation(100, 100);
        getJTextPane().setFont(new Font("Monospaced", Font.PLAIN, 12));
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
    private JTextArea getJTextPane() {
        if (jTextPane == null) {
            jTextPane = new JTextArea();
            jTextPane.setLineWrap(false);
        }
        return jTextPane;
    }
    
	private static final long serialVersionUID = -8991473715127153931L;
}  //  @jve:decl-index=0:visual-constraint="10,10"
