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

import java.awt.GridBagConstraints;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.net.URI;
import java.net.URLEncoder;
import java.util.Date;

import javax.swing.AbstractAction;
import javax.swing.ActionMap;
import javax.swing.Icon;
import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JRootPane;
import javax.swing.JScrollPane;
import javax.swing.KeyStroke;
import javax.swing.UIManager;

import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.fife.ui.rsyntaxtextarea.SyntaxConstants;

import net.sf.jailer.ui.syntaxtextarea.BasicFormatterImpl;
import net.sf.jailer.ui.util.UISettings;
import net.sf.jailer.ui.util.UpdateInfoManager;

/**
 * Shows SQL-Exception.
 * 
 * @author Ralf Wisser
 */
public class SqlErrorDialog extends javax.swing.JDialog {

	private static final long serialVersionUID = -6737420167295938488L;

	/** Creates new form SqlErrorDialog 
	 * @param isWarning */
	@SuppressWarnings("serial")
	public SqlErrorDialog(Window parent, String message, String sql, boolean isFormatted, boolean sqlError, String title, boolean isWarning, JComponent additionalControl) {
		super(parent, ModalityType.APPLICATION_MODAL);
		this.sqlError = sqlError;
		this.message = message;
		this.sqlEditorPane = new RSyntaxTextArea();
		sqlEditorPane.setEditable(false);
		if (sqlError) {
			this.sqlEditorPane.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_SQL);
			this.sqlEditorPane.setFadeCurrentLineHighlight(true);
			if (!isFormatted) {
				sql = new BasicFormatterImpl().format(sql);
			}
		}
		initComponents(); UIUtil.initComponents(this);

		if (isWarning) {
			jPanel1.setVisible(false);
			copyButton.setVisible(false);
		}
		if (additionalControl != null) {
			GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 1;
	        gridBagConstraints.gridy = 3;
	        gridBagConstraints.weightx = 1;
	        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
	        getContentPane().add(additionalControl, gridBagConstraints);
		}
		
		updateInfoPanel.setVisible(false);
		
		JScrollPane jScrollPane1 = new JScrollPane();
		jScrollPane1.setViewportView(sqlEditorPane);
		jPanel1.add(jScrollPane1, java.awt.BorderLayout.CENTER);
		
		sendButton.setVisible(true);
		if (!sqlError) {
			if (title != null) {
				setTitle(title);
				sendButton.setVisible(false);
			} else {
				setTitle("Unexpected Error");
				sendButton.grabFocus();
				KeyListener keyListener = new KeyListener() {
					@Override
					public void keyTyped(KeyEvent e) {
						if (e.getKeyChar() == '\n') {
							sendButtonActionPerformed(null);
							setVisible(false);
						}
					}
	
					@Override
					public void keyReleased(KeyEvent e) {
					}
	
					@Override
					public void keyPressed(KeyEvent arg0) {
					}
				};
				sendButton.addKeyListener(keyListener);
			}
		} else {
			if (title != null) {
				setTitle(title);
				if (title.equals("Invalid Primary Key")) {
					sendButton.setVisible(false);
				}
			}
		}

		if (sendButton.isVisible() && UpdateInfoManager.currentDownloadableRelease != null) {
			updateInfoLabel.setText("Release " + UpdateInfoManager.currentDownloadableRelease + " available");
			updateInfoPanel.setVisible(true);
		}

		message = message.trim();

		int y = 1;
		String[] splitted = message.split("\n");
		for (String line : splitted) {
			JLabel label = new JLabel(line);
			java.awt.GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.gridx = 1;
			gridBagConstraints.gridy = y++;
			gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
			gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
			gridBagConstraints.weightx = 1.0;
			gridBagConstraints.weighty = 0;
			final int limit = 1000;
			if (y > limit) {
				messagePanel.add(new JLabel((splitted.length - y + 1) + " lines more..."), gridBagConstraints);
				break;
			}
			messagePanel.add(label, gridBagConstraints);
		}
		
		sqlEditorPane.setText(sql.trim());
		sqlEditorPane.setCaretPosition(0);
		try {
			errorLabel.setText(null);
			Icon errorIcon = UIManager.getIcon("OptionPane.errorIcon");
			errorLabel.setIcon(errorIcon);
		} catch (Throwable t) {
			// ignore
		}
		pack();
		setSize(Math.max(700, Math.min(getWidth(), 1000)), Math.min(getHeight() + 32, 600));
		if (parent == null) {
			setLocation(200, 100);
		} else {
			setLocation(getParent().getX() + (getParent().getWidth() - getWidth()) / 2,
					getParent().getY() + (getParent().getHeight() - getHeight()) / 2);
		}

		JRootPane rootPane = getRootPane();
		InputMap im = rootPane.getInputMap(
								JComponent.WHEN_IN_FOCUSED_WINDOW);
		ActionMap actionMap = rootPane.getActionMap();
		KeyStroke ks = KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0);

		im.put(ks, ks);
   		actionMap.put(ks, new AbstractAction() {
			@Override
			public void actionPerformed(ActionEvent e) {
				setVisible(false);
			}
		});
		
		UIUtil.fit(this);
		setVisible(true);
	}

	private final boolean sqlError;
	private final String message;

	/**
	 * This method is called from within the constructor to initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is always
	 * regenerated by the Form Editor.
	 */
	// <editor-fold defaultstate="collapsed" desc="Generated
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jPanel1 = new javax.swing.JPanel();
        errorLabel = new javax.swing.JLabel();
        copyButton = new javax.swing.JButton();
        jScrollPane2 = new javax.swing.JScrollPane();
        messagePanel = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        jPanel2 = new javax.swing.JPanel();
        sendButton = new javax.swing.JButton();
        jButton1 = new javax.swing.JButton();
        updateInfoPanel = new javax.swing.JPanel();
        updateInfoLabel = new javax.swing.JLabel();
        downloadButton = new javax.swing.JButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("SQL Statement failed");
        getContentPane().setLayout(new java.awt.GridBagLayout());

        jPanel1.setLayout(new java.awt.BorderLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        getContentPane().add(jPanel1, gridBagConstraints);

        errorLabel.setText("jLabel1");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 12);
        getContentPane().add(errorLabel, gridBagConstraints);

        copyButton.setText("Copy to Clipboard");
        copyButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                copyButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 2);
        getContentPane().add(copyButton, gridBagConstraints);

        messagePanel.setLayout(new java.awt.GridBagLayout());

        jLabel1.setText("  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 100;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        messagePanel.add(jLabel1, gridBagConstraints);

        jScrollPane2.setViewportView(messagePanel);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.ipadx = 1;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 0.5;
        getContentPane().add(jScrollPane2, gridBagConstraints);

        jPanel2.setLayout(new java.awt.GridBagLayout());

        sendButton.setText("Report and Close");
        sendButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                sendButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 4, 2);
        jPanel2.add(sendButton, gridBagConstraints);

        jButton1.setText(" Close ");
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 4, 4);
        jPanel2.add(jButton1, gridBagConstraints);

        updateInfoPanel.setBackground(new java.awt.Color(255, 255, 236));
        updateInfoPanel.setLayout(new java.awt.GridBagLayout());

        updateInfoLabel.setText("Release x available");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(8, 8, 8, 8);
        updateInfoPanel.add(updateInfoLabel, gridBagConstraints);

        downloadButton.setText("Download");
        downloadButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                downloadButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 8);
        updateInfoPanel.add(downloadButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 4, 8);
        jPanel2.add(updateInfoPanel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHEAST;
        getContentPane().add(jPanel2, gridBagConstraints);

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void downloadButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_downloadButtonActionPerformed
        updateInfoPanel.setVisible(false);
        UpdateInfoManager.download();
    }//GEN-LAST:event_downloadButtonActionPerformed

	private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_jButton1ActionPerformed
		setVisible(false);
	}// GEN-LAST:event_jButton1ActionPerformed

	private void copyButtonActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_copyButtonActionPerformed
		sqlEditorPane.selectAll();
		sqlEditorPane.copy();
	}// GEN-LAST:event_copyButtonActionPerformed

	private void sendButtonActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_sendButtonActionPerformed
		setVisible(false);
		URI url;
		try {
			int MAX_LENGTH = 1600;
			String issue = (sqlError ? message + "\n\n" : "")
					+ (sqlEditorPane.getText().replaceAll("\\s+", " "));
			if (issue.length() > MAX_LENGTH) {
				issue = issue.substring(0, MAX_LENGTH);
			}
			url = new URI("http://jailer.sourceforge.net/issueReport.php?type="
					+ URLEncoder.encode(sqlError ? "SQL" : "GUI", "UTF-8") + "&" + "issue="
					+ URLEncoder.encode(issue.replace('\r', ' '), "UTF-8")
					+ "&uuid=" + URLEncoder.encode(String.valueOf(UISettings.restore("uuid")), "UTF-8")
					+ "&ts=" + URLEncoder.encode(new Date().toString(), "UTF-8")
					+ "&jversion=" + URLEncoder.encode(System.getProperty("java.version") + "/" + System.getProperty("java.vm.vendor") + "/" + System.getProperty("java.vm.name") + "/" + System.getProperty("os.name"), "UTF-8") + "/(" + Environment.state + ")");
			BrowserLauncher.openURL(url, this);
		} catch (Throwable e) {
			// ignore
		}
	}// GEN-LAST:event_sendButtonActionPerformed

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton copyButton;
    private javax.swing.JButton downloadButton;
    private javax.swing.JLabel errorLabel;
    private javax.swing.JButton jButton1;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JPanel messagePanel;
    private javax.swing.JButton sendButton;
    private javax.swing.JLabel updateInfoLabel;
    private javax.swing.JPanel updateInfoPanel;
    // End of variables declaration//GEN-END:variables

    private final RSyntaxTextArea sqlEditorPane;
}

// TODO mark user defined SQL fragments in a way that it is possible to visualize their type/reason/... here (mark with SQL comments?)
