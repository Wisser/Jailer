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

import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.WeakHashMap;
import java.util.function.Consumer;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;

import net.sf.jailer.util.CancellationHandler;

/**
 * Jailer console window.
 *
 * @author Ralf Wisser
 */
public class JailerConsole {

	private final ProgressPanel progressPanel;
	public final Window dialog;
	public final Window owner;
	private final boolean fullSize;

	/**
	 * Constructor.
	 *
	 * @param owner the enclosing component
	 * @param showLogfileButton <code>true</code> for offering a button to open the log-file
	 * @param progressPanel progress panel, optional
	 */
	public JailerConsole(Window owner, Window dialog, boolean showLogfileButton, ProgressPanel progressPanel, boolean fullSize) {
		this.dialog = dialog;
		this.owner = owner;
		this.progressPanel = progressPanel;
		this.fullSize = fullSize;
		closeFinishedPredecessors();
		initialize();
		cancelButton.setIcon(UIUtil.scaleIcon(cancelButton, cancelIcon));
		getJTextPane().setAutoscrolls(true);
		Font f = getJTextPane().getFont();
		getJTextPane().setFont(new Font(Font.MONOSPACED, f.getStyle(), f.getSize()));
		getJTextPane().setLineWrap(false);
		if (dialog instanceof JDialog) {
			((JDialog) dialog).setModal(true);
			((JDialog) dialog).setDefaultCloseOperation(0);
		} else {
			((JFrame) dialog).setDefaultCloseOperation(0);
		}
		getOpenResultButton();
		getLoadSqlLog().setEnabled(false);
		getLoadExportLog().setVisible(false);
		if (!showLogfileButton) {
			// getLoadExportLog().setVisible(false);
			getLoadSqlLog().setVisible(false);
			getOpenResultButton().setVisible(false);
		}
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
			gridBagConstraints.gridwidth = 7;
			gridBagConstraints.insets = new Insets(0, 0, 0, 0);
			jPanel.add(contentPane, gridBagConstraints);
		}
		if (fullSize) {
			dialog.setSize(new Dimension(1110, 740));
			UIUtil.setInitialWindowLocation(dialog, owner, 10, 50);
		} else {
			dialog.setSize(new Dimension(600, 400));
			UIUtil.setInitialWindowLocation(dialog, owner, 10, 50);
		}
		if (dialog instanceof JDialog) {
			((JDialog) dialog).setContentPane(jPanel);
			((JDialog) dialog).setTitle("Jailer Console - in progress");
		} else {
			((JFrame) dialog).setContentPane(jPanel);
			((JFrame) dialog).setTitle("Jailer Console - in progress");
		}
		getJTextPane().setFont(new Font("Monospaced", Font.PLAIN, 12));

		getLoadExportLog().addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					new FileView(dialog, dialog, Environment.newFile("export.log").getAbsolutePath(), false);
				} catch (Exception e1) {
					e1.printStackTrace();
				}
			}
		});
		getLoadSqlLog().addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					new FileView(dialog, dialog, Environment.newFile("sql.log").getAbsolutePath(), false);
				} catch (Exception e1) {
					e1.printStackTrace();
				}
			}
		});
		UIUtil.fit(this.dialog);

		getCancelButton().addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (hasFinished) {
					dialog.setVisible(false);
					return;
				}
				if (JOptionPane.showConfirmDialog(dialog,
						"Cancel operation?", "Cancellation",
						JOptionPane.YES_NO_OPTION,
						JOptionPane.QUESTION_MESSAGE) == JOptionPane.YES_OPTION) {
					if (!hasFinished) {
						hasCancelled = true;
						if (progressPanel != null) {
                            progressPanel.onCancel();
                        }
						new Thread(new Runnable() {
							@Override
							public void run() {
								CancellationHandler.cancel(null);
							}
						}).start();
						if (dialog instanceof JDialog) {
							((JDialog) dialog).setTitle("Jailer Console - cancelled");
						} else {
							((JFrame) dialog).setTitle("Jailer Console - cancelled");
						}
						getCancelButton().setEnabled(false);
					}
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
		final int MAXLENGTH = 50000;
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
		getCancelButton().setText("Close");
		getCancelButton().setEnabled(true);
		getOpenResultButton().setEnabled(ok);
		String title = "Jailer Console - " + (ok? "finished" : "failed!");
		if (dialog instanceof JDialog) {
			((JDialog) dialog).setTitle(title);
		} else {
			((JFrame) dialog).setTitle(title);
		}
		hasFinished = true;
	}

	/**
	 * This method initializes jPanel
	 *
	 * @return javax.swing.JPanel
	 */
	private JPanel getJPanel() {
		if (jPanel == null) {
			GridBagConstraints gridBagConstraints29 = new GridBagConstraints();
			gridBagConstraints29.anchor = GridBagConstraints.EAST;
			gridBagConstraints29.gridx = 6;
			gridBagConstraints29.gridy = 1;
			gridBagConstraints29.weightx = 0.0;
			gridBagConstraints29.insets = new Insets(4, 4, 4, 2);
			GridBagConstraints gridBagConstraints2 = new GridBagConstraints();
			gridBagConstraints2.gridx = 3;
			gridBagConstraints2.gridy = 1;
			gridBagConstraints2.insets = new Insets(4, 4, 4, 0);
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
			gridBagConstraints.gridwidth = 7;
			jPanel = new JPanel();
			jPanel.setLayout(new GridBagLayout());
			jPanel.add(getJScrollPane(), gridBagConstraints);
			jPanel.add(getLoadExportLog(), gridBagConstraints1);
			jPanel.add(getLoadSqlLog(), gridBagConstraints2);
			jPanel.add(getCancelButton(), gridBagConstraints29);
			
			gridBagConstraints = new GridBagConstraints();
			gridBagConstraints29.anchor = GridBagConstraints.EAST;
			gridBagConstraints.gridy = 1;
			gridBagConstraints.gridx = 4;
			gridBagConstraints.insets = new Insets(4, 4, 4, 2);
			gridBagConstraints.weightx = 1.0;
			jPanel.add(new JLabel(" "), gridBagConstraints);
			
			gridBagConstraints = new GridBagConstraints();
			gridBagConstraints29.anchor = GridBagConstraints.EAST;
			gridBagConstraints.gridy = 1;
			gridBagConstraints.gridx = 5;
			gridBagConstraints.insets = new Insets(4, 4, 4, 0);
			jPanel.add(getOpenResultButton(), gridBagConstraints);
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
	JButton getCancelButton() {
		if (cancelButton == null) {
			cancelButton = new JButton();
			cancelButton.setText("Cancel");
		}
		return cancelButton;
	}

	private JButton getOpenResultButton() {
		if (openResultButton == null) {
			openResultButton = new JButton();
			openResultButton.setEnabled(false);
			openResultButton.setText("Open Result");
			openResultButton.setIcon(UIUtil.scaleIcon(openResultButton, editIcon));
			openResultButton.setVisible(openResultActions.containsKey(dialog));
			openResultButton.addActionListener(e -> openResultActions.get(dialog).accept(dialog));
		}
		return openResultButton;
	}
	
	private void closeFinishedPredecessors() {
		if (openResultActions.containsKey(dialog)) {
			openResultActions.forEach((d, a) -> {
				if (d != null && d.isVisible() && d != dialog) {
					JailerConsole jc = jConsoles.get(d);
					if (jc != null && (jc.hasCancelled || jc.hasFinished)) {
						jConsoles.remove(jc.dialog);
						d.setVisible(false);
						d.dispose();
					}
				}
			});
			jConsoles.put(dialog, this);
		}
	}

	private JTextArea jTextArea = null;
	private JPanel jPanel = null;
	private JScrollPane jScrollPane = null;
	private JButton loadExportLog = null;
	private JButton loadSqlLog = null;
	private JButton cancelButton = null;
	private JButton openResultButton = null;
	public boolean hasCancelled = false;
	boolean hasFinished = false;
	
	public static WeakHashMap<Window, Consumer<Window>> openResultActions = new WeakHashMap<Window, Consumer<Window>>();
	public static WeakHashMap<Window, JailerConsole> jConsoles = new WeakHashMap<Window, JailerConsole>();

	private static ImageIcon cancelIcon;
	private static ImageIcon editIcon;
	
	static {
        // load images
        cancelIcon = UIUtil.readImage("/buttoncancel.png");
        editIcon = UIUtil.readImage("/edit.png");
	}
}  //  @jve:decl-index=0:visual-constraint="10,10"
