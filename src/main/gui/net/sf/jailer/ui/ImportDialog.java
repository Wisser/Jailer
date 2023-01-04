/*
 * Copyright 2007 - 2023 Ralf Wisser.
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

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.sql.Connection;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListCellRenderer;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JTextField;
import javax.swing.ListCellRenderer;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;

import net.sf.jailer.util.CancellationHandler;

/**
 * Data Export Dialog.
 * 
 * @author Ralf Wisser
 */
public class ImportDialog extends javax.swing.JDialog {

	private final List<String> initialArgs;
	private final String user;
	private final String password;

	private static int numThreads = 1;
	
	/**
	 * Creates new form DbConnectionDialog
	 */
	public ImportDialog(java.awt.Frame parent, String importFile,
			List<String> initialArgs, String user, String password, boolean showCmd) {
		super(parent, true);
		this.initialArgs = new ArrayList<String>(initialArgs);
		this.user = user;
		this.password = password;
		initComponents();
		copyButton.setIcon(copyIcon);
		
		cliArea.setDocument(new DefaultStyledDocument());
		
		okButton.setIcon(UIUtil.scaleIcon(okButton, okIcon));
		cancelButton.setIcon(UIUtil.scaleIcon(cancelButton, cancelIcon));

		this.importFile.setText(new File(importFile).getName());
		this.importFile.setToolTipText(new File(importFile).getAbsolutePath());

		final Font font = this.importFile.getFont();
		final Font bold = font.deriveFont(font.getStyle() | Font.BOLD, font.getSize());
		this.importFile.setFont(bold);

		initIsolationLevel();
		
		CancellationHandler.reset(null);

		if (!showCmd) {
			commandLinePanel.setVisible(false);
		}

		setModal(true);
		UIUtil.setInitialWindowLocation(this, parent, 100, 150);

		threadComboBox.setSelectedItem(Integer.toString(numThreads));
		DocumentListener dl = new DocumentListener() {
			@Override
			public void removeUpdate(DocumentEvent e) {
				updateCLIArea();
			}

			@Override
			public void insertUpdate(DocumentEvent e) {
				updateCLIArea();
			}

			@Override
			public void changedUpdate(DocumentEvent e) {
				updateCLIArea();
			}
		};
		ActionListener al = new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				updateCLIArea();
			}
		};
		try {
			((JTextField) threadComboBox.getEditor().getEditorComponent()).getDocument().addDocumentListener(dl);
		} catch (ClassCastException e) {
			// ignore
		}
		transactionalCheckBox.addActionListener(al);
		isolationLevelComboBox.addActionListener(al);

		updateCLIArea();

		pack();
		setSize(480, getSize().height);
		placeholder1.setVisible(false);
		UIUtil.fit(this);
		setVisible(true);
	}

	private void updateCLIArea() {
		List<String> args = new ArrayList<String>(initialArgs);
		fillCLIArgs(args);
		String cmd = "sh jailer.sh";
		if (System.getProperty("os.name", "").toLowerCase(Locale.ENGLISH)
				.startsWith("windows")) {
			cmd = "jailer.bat";
		}
		cliArea.setText(cmd + UIUtil.createPlainCLIArguments(user, password, args, true));
		cliArea.setCaretPosition(0);
		
		SimpleAttributeSet set = new SimpleAttributeSet();
        ((DefaultStyledDocument) cliArea.getDocument()).setCharacterAttributes(0, cliArea.getText().length(), set, true);
		addStyle("\"\\<password\\>\"", Color.RED);
		addStyle("jailer\\.[^ ]+ ", Color.BLUE);
		addStyle("export|delete|import", Color.BLUE);
	}

	/**
	 * @param color
	 */
	private void addStyle(String reg, Color color) {
		SimpleAttributeSet set;
		Pattern pattern = Pattern.compile(reg);
		Matcher matcher = pattern.matcher(cliArea.getText());
		if (matcher.find()) {
			set = new SimpleAttributeSet();
            StyleConstants.setForeground(set, color);
            ((DefaultStyledDocument) cliArea.getDocument()).setCharacterAttributes(matcher.start(), matcher.end() - matcher.start(), set, true);
		}
	}

	@SuppressWarnings({ "unchecked", "serial" })
	private void initIsolationLevel() {
		final Map<String, String> levels = new TreeMap<String, String>();
		levels.put(String.valueOf(Connection.TRANSACTION_READ_COMMITTED), "Read committed");
		levels.put(String.valueOf(Connection.TRANSACTION_READ_UNCOMMITTED), "Read uncommitted");
		levels.put(String.valueOf(Connection.TRANSACTION_REPEATABLE_READ), "Repeatable read");
		levels.put(String.valueOf(Connection.TRANSACTION_SERIALIZABLE), "Serializable");
		levels.put(String.valueOf(Connection.TRANSACTION_NONE), "Default");
		isolationLevelComboBox.setModel(new DefaultComboBoxModel<String>(levels.keySet().toArray(new String[0])));
		isolationLevelComboBox.setRenderer(new DefaultListCellRenderer() {
			@SuppressWarnings("rawtypes")
			ListCellRenderer renderer = isolationLevelComboBox.getRenderer();
			@SuppressWarnings("rawtypes")
			@Override
			public Component getListCellRendererComponent(JList list,
					Object value, int index, boolean isSelected,
					boolean cellHasFocus) {
				return renderer.getListCellRendererComponent(list, levels.get(value), index, isSelected, cellHasFocus);
			}
		});
		isolationLevelComboBox.setSelectedItem(String.valueOf(Connection.TRANSACTION_NONE));
		isolationLevelComboBox.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				updateCLIArea();				
			}
		});
	}

	/**
	 * This method is called from within the constructor to initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is always
	 * regenerated by the Form Editor.
	 */
	// <editor-fold defaultstate="collapsed"
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        buttonGroup1 = new javax.swing.ButtonGroup();
        jPanel6 = new javax.swing.JPanel();
        jPanel1 = new javax.swing.JPanel();
        exportLabel = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        jLabel6 = new javax.swing.JLabel();
        jLabel9 = new javax.swing.JLabel();
        commandLinePanel = new javax.swing.JPanel();
        jLabel22 = new javax.swing.JLabel();
        jLabel23 = new javax.swing.JLabel();
        jLabel24 = new javax.swing.JLabel();
        jLabel25 = new javax.swing.JLabel();
        jScrollPane1 = new javax.swing.JScrollPane();
        cliArea = new javax.swing.JTextPane();
        placeholder1 = new javax.swing.JLabel();
        transactionalCheckBox = new javax.swing.JCheckBox();
        importFile = new javax.swing.JLabel();
        threadComboBox = new javax.swing.JComboBox();
        isolationLevelComboBox = new javax.swing.JComboBox();
        targetDBMSLabel1 = new javax.swing.JLabel();
        jPanel7 = new javax.swing.JPanel();
        jPanel2 = new javax.swing.JPanel();
        okButton = new javax.swing.JButton();
        cancelButton = new javax.swing.JButton();
        copyButton = new javax.swing.JButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Data Import"); // NOI18N
        getContentPane().setLayout(new java.awt.GridBagLayout());

        jPanel6.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        jPanel6.setLayout(new java.awt.GridBagLayout());

        jPanel1.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        jPanel1.setLayout(new java.awt.GridBagLayout());

        exportLabel.setText(" Import file"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 30;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(exportLabel, gridBagConstraints);

        jLabel5.setText(" "); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 45;
        jPanel1.add(jLabel5, gridBagConstraints);

        jLabel6.setText(" Parallel threads   "); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 50;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(jLabel6, gridBagConstraints);

        jLabel9.setText("           "); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 58;
        jPanel1.add(jLabel9, gridBagConstraints);

        commandLinePanel.setLayout(new java.awt.GridBagLayout());

        jLabel22.setText(" Command line"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        commandLinePanel.add(jLabel22, gridBagConstraints);

        jLabel23.setText(" "); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        commandLinePanel.add(jLabel23, gridBagConstraints);

        jLabel24.setText(" "); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        commandLinePanel.add(jLabel24, gridBagConstraints);

        jLabel25.setText(" "); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        commandLinePanel.add(jLabel25, gridBagConstraints);

        cliArea.setEditable(false);
        jScrollPane1.setViewportView(cliArea);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridheight = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 0);
        commandLinePanel.add(jScrollPane1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 85;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel1.add(commandLinePanel, gridBagConstraints);

        placeholder1.setText(" "); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(placeholder1, gridBagConstraints);

        transactionalCheckBox.setText("transactional");
        transactionalCheckBox.setToolTipText("execute all statements in a single transaction");
        transactionalCheckBox.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        transactionalCheckBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                transactionalCheckBoxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 42;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 2, 0);
        jPanel1.add(transactionalCheckBox, gridBagConstraints);

        importFile.setText("jLabel1");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 30;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(importFile, gridBagConstraints);

        threadComboBox.setEditable(true);
        threadComboBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "1", "5", "10", "20", "50", "100" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 50;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(threadComboBox, gridBagConstraints);

        isolationLevelComboBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 41;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(8, 0, 0, 0);
        jPanel1.add(isolationLevelComboBox, gridBagConstraints);

        targetDBMSLabel1.setText(" Isolation level"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 41;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(8, 0, 4, 0);
        jPanel1.add(targetDBMSLabel1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel6.add(jPanel1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        getContentPane().add(jPanel6, gridBagConstraints);

        jPanel7.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        jPanel7.setLayout(new java.awt.GridBagLayout());

        jPanel2.setLayout(new java.awt.GridBagLayout());

        okButton.setText("Import Data"); // NOI18N
        okButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                okButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 2, 2);
        jPanel2.add(okButton, gridBagConstraints);

        cancelButton.setText("Cancel");
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 2, 2);
        jPanel2.add(cancelButton, gridBagConstraints);

        copyButton.setText("Copy to Clipboard"); // NOI18N
        copyButton.setToolTipText("Copy to Clipboard"); // NOI18N
        copyButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                copyButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 2, 0);
        jPanel2.add(copyButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 100;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel7.add(jPanel2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        getContentPane().add(jPanel7, gridBagConstraints);

        pack();
    }// </editor-fold>//GEN-END:initComponents

	private void okButtonActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_okButtonActionPerformed
		isOk = true;
		updateCLIArea();
		Integer nt = numThreads();
		if (nt != null) {
			numThreads = nt;
		}
		setVisible(false);
	}// GEN-LAST:event_okButtonActionPerformed

	private void copyButtonActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_copyButtonActionPerformed
		cliArea.selectAll();
		cliArea.copy();
		updateCLIArea();
	}// GEN-LAST:event_copyButtonActionPerformed

	private void transactionalCheckBoxActionPerformed(
			java.awt.event.ActionEvent evt) {// GEN-FIRST:event_transactionalCheckBoxActionPerformed
	}// GEN-LAST:event_transactionalCheckBoxActionPerformed

	private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_cancelButtonActionPerformed
		dispose();
	}// GEN-LAST:event_cancelButtonActionPerformed

	public boolean isOk() {
		return isOk;
	}

	/**
	 * Fills field content into cli-args.
	 * 
	 * @param args
	 *            the argument-list to fill
	 */
	public void fillCLIArgs(List<String> args) {
		if (transactionalCheckBox.isSelected()) {
			args.add("-transactional");
		}
		Object isolationLevel = isolationLevelComboBox.getSelectedItem();
		if (isolationLevel != null && !String.valueOf(Connection.TRANSACTION_NONE).equals(isolationLevel)) {
			args.add("-isolation-level");
			args.add(isolationLevel.toString());
		}
		Integer nt = numThreads();
		if (nt != null) {
			args.add("-threads");
			args.add("" + nt);
		}
	}

	private Integer numThreads() {
		try {
			String text = threadComboBox.getSelectedItem().toString().trim();
			if (threadComboBox.getEditor().getEditorComponent() instanceof JTextField) {
				text = ((JTextField) threadComboBox.getEditor().getEditorComponent()).getText().trim();
			}
			int nt = Math.min(Integer.parseInt(text), 10000);
			if (nt > 0) {
				return nt;
			}
		} catch (Exception e) {
		}
		return null;
	}

	/**
	 * true iff ok-button was clicked.
	 */
	public boolean isOk = false;

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.ButtonGroup buttonGroup1;
    private javax.swing.JButton cancelButton;
    private javax.swing.JTextPane cliArea;
    public javax.swing.JPanel commandLinePanel;
    private javax.swing.JButton copyButton;
    private javax.swing.JLabel exportLabel;
    private javax.swing.JLabel importFile;
    private javax.swing.JComboBox isolationLevelComboBox;
    private javax.swing.JLabel jLabel22;
    private javax.swing.JLabel jLabel23;
    private javax.swing.JLabel jLabel24;
    private javax.swing.JLabel jLabel25;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JPanel jPanel7;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JButton okButton;
    private javax.swing.JLabel placeholder1;
    private javax.swing.JLabel targetDBMSLabel1;
    private javax.swing.JComboBox threadComboBox;
    private javax.swing.JCheckBox transactionalCheckBox;
    // End of variables declaration//GEN-END:variables

	private static ImageIcon okIcon;
	private static ImageIcon cancelIcon;
	private static ImageIcon copyIcon;
	
	static {
        // load images
        okIcon = UIUtil.readImage("/buttonok.png");
        cancelIcon = UIUtil.readImage("/buttoncancel.png");
        copyIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/copy.png"));
	}

	private static final long serialVersionUID = 952553009821662964L;

}
