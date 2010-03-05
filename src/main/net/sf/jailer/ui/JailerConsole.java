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

import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;

/**
 * Jailer console window.
 * 
 * @author Ralf Wisser
 */
public class JailerConsole {

	private final ProgressPanel progressPanel;
	public final JDialog dialog;
	private final boolean fullSize;
	
    /**
     * Constructor.
     * 
     * @param owner the enclosing component
     * @param showLogfileButton <code>true</code> for offering a button to open the log-file
     * @param showExplainLogButton <code>true</code> for offering a button to open the explain-log-file
     * @param progressPanel progress panel, optional
     */
	public JailerConsole(Frame owner, JDialog dialog, boolean showLogfileButton, boolean showExplainLogButton, ProgressPanel progressPanel, boolean fullSize) {
		this.dialog = dialog == null? new JDialog(owner) : dialog;
        this.progressPanel = progressPanel;
        this.fullSize = fullSize;
        initialize();
        getJTextPane().setAutoscrolls(true);
        Font f = getJTextPane().getFont();
        getJTextPane().setFont(new Font(Font.MONOSPACED, f.getStyle(), f.getSize()));
        getJTextPane().setLineWrap(false);
        dialog.setModal(true);
        dialog.setDefaultCloseOperation(0);
        getLoadExplainLog().setEnabled(false);
    	getLoadSqlLog().setEnabled(false);
        getLoadExportLog().setVisible(false);
    	if (!showLogfileButton) {
            // getLoadExportLog().setVisible(false);
            getLoadSqlLog().setVisible(false);
        }
        if (!showExplainLogButton) {
            getLoadExplainLog().setVisible(false);
        }
        UIUtil.initPeer();
    }

    /**
     * Initializes components.
     */
    private void initialize() {
    	JPanel jPanel = getJPanel();
        if (progressPanel != null) {
        	jPanel.remove(getJScrollPane());
        	JTabbedPane contentPane = new JTabbedPane();
        	contentPane.add(progressPanel, "Progress");
        	contentPane.add(getJScrollPane(), "Console");
        	GridBagConstraints gridBagConstraints = new GridBagConstraints();
            gridBagConstraints.fill = GridBagConstraints.BOTH;
            gridBagConstraints.gridy = 0;
            gridBagConstraints.weightx = 1.0;
            gridBagConstraints.weighty = 1.0;
            gridBagConstraints.gridx = 0;
            gridBagConstraints.gridwidth = 5;
            gridBagConstraints.insets = new Insets(0, 0, 4, 0);
        	jPanel.add(contentPane, gridBagConstraints);
        } 
        if (fullSize) {
        	dialog.setSize(new Dimension(1010, 740));
           	dialog.setLocation(10, 50);
        } else {
        	dialog.setSize(new Dimension(400, 400));
        	dialog.setLocation(200, 250);
        }
        dialog.setContentPane(jPanel);
        dialog.setTitle("Jailer Console - in progress");
        getJTextPane().setFont(new Font("Monospaced", Font.PLAIN, 12));
        
        getLoadExportLog().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                try {
                    new FileView(dialog, "export.log");
                } catch (Exception e1) {
                    e1.printStackTrace();
                }
            }
        });
        getLoadSqlLog().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                try {
                    new FileView(dialog, "sql.log");
                } catch (Exception e1) {
                    e1.printStackTrace();
                }
            }
        });
        getLoadExplainLog().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                try {
                    new FileView(dialog, "explain.log");
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
    private JTextArea getJTextPane() {
        if (jTextArea == null) {
        	jTextArea = new JTextArea();
        	jTextArea.setEditable(false);
        }
        return jTextArea;
    }

    /**
     * Appends text to console window.
     * 
     * @param output the text
     */
    public void appendText(String output) {
    	final int MAXLENGTH = 100000;
        StringBuffer newText = new StringBuffer(getJTextPane().getText() + output);
        if (newText.length() > MAXLENGTH) {
        	int fll = newText.indexOf("\n");
        	String firstLine = "";
        	if (fll > 0) {
        		firstLine = newText.substring(0, fll);
        	}
        	int start = newText.length() - (3 * MAXLENGTH / 4);
        	newText = new StringBuffer(firstLine + "\n...\n" + newText.substring(start, newText.length()));
        }
		getJTextPane().setText(newText.toString());
    }

    /**
     * Sets status of console to "finished".
     * 
     * @param ok indicates errors
     */
    public void finish(boolean ok) {
    	getLoadSqlLog().setEnabled(true);
    	getLoadExplainLog().setEnabled(true);
    	dialog.setTitle("Jailer Console - " + (ok? "finished" : "failed!"));
    }
    
    /**
     * This method initializes jPanel	
     * 	
     * @return javax.swing.JPanel	
     */
    private JPanel getJPanel() {
        if (jPanel == null) {
            GridBagConstraints gridBagConstraints28 = new GridBagConstraints();
            gridBagConstraints28.anchor = GridBagConstraints.WEST;
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
    
    private JTextArea jTextArea = null;
    private JPanel jPanel = null;
    private JScrollPane jScrollPane = null;
    private JButton loadExportLog = null;
    private JButton loadSqlLog = null;
    private JButton loadExplainLog = null;

}  //  @jve:decl-index=0:visual-constraint="10,10"
