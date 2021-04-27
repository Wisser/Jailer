/*
 * Copyright 2007 - 2021 Ralf Wisser.
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
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import net.sf.jailer.ui.DbConnectionDialog.ConnectionInfo;
import net.sf.jailer.ui.util.ConcurrentTaskControl;
import net.sf.jailer.ui.util.HttpDownload;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.CsvFile.Line;

/**
 * "Connect with DB" dialog.
 *
 * @author Ralf Wisser
 */
public class DbConnectionDetailsEditor extends javax.swing.JDialog {

	/**
	 * <code>true</code> if valid connection is available.
	 */
	private boolean isOk = false;
	
	/**
	 * The connection to edit.
	 */
	private ConnectionInfo ci;

	/**
	 * <code>true</code> if connection must be tested on OK.
	 */
	private final boolean needsTest;
	
	/**
	 * Opens detail editor for a connection.
	 * 
	 * @param ci the connection
	 * @return <code>true</code> if connection has been edited
	 */
	public boolean edit(ConnectionInfo ci) {
		setDetails(ci);
		setVisible(true);
		return isOk;
	}

	/**
	 * Sets details.
	 * 
	 * @param ci the connection
	 * @return <code>true</code> if connection has been edited
	 */
	public void setDetails(ConnectionInfo ci) {
		this.ci = ci;
		alias.setText(ci.alias);
		dbUrl.setText(ci.url);
		user.setText(ci.user);
		password.setText(ci.password);
		driverClass.setText(ci.driverClass);
		jar1.setText(ci.jar1);
		jar2.setText(ci.jar2);
		jar3.setText(ci.jar3);
		jar4.setText(ci.jar4);
		try {
			dbUrl.setCaretPosition(0);
			driverClass.setCaretPosition(0);
		} catch (Exception e) {
			// ignore
		}
	}

	/** Creates new form DbConnectionDialog 
	 * @param forNew */
	public DbConnectionDetailsEditor(Window parent, final String jdbcHelpURL, boolean forNew) {
		this(parent, jdbcHelpURL, forNew, null, false);
	}

	private final Window parent;
	private List<Line> driverlist;
	
	/** Creates new form DbConnectionDialog 
	 * @param forNew 
	 * @param b */
	public DbConnectionDetailsEditor(Window parent, final String jdbcHelpURL, boolean forNew, InfoBar infoBar, boolean needsTest) {
		super(parent);
		setModal(true);
		this.parent = parent;
		this.needsTest = needsTest;
		initComponents();
		feedbackTimer = new Timer(4000, e -> {
			feedbackLabel.setText(" ");
		});
		feedbackTimer.setRepeats(false);
		textFields = new JTextField[] {
	    		alias,
	    		jar1,
	    		jar2,
	    		jar3,
	    		jar4,
	    		driverClass,
	    		dbUrl,
	    		user
	    };
		try {
			CsvFile drivers = new CsvFile(Environment.newWorkingFolderFile("driverlist.csv"));
			driverlist = new ArrayList<Line>(drivers.getLines());
		} catch (Throwable t) {
			driverlist = null;
		}
		
		Arrays.asList(jar1, jar2, jar3, jar4, dbUrl).forEach(f -> f.getDocument().addDocumentListener(new DocumentListener() {
			@Override
			public void removeUpdate(DocumentEvent e) {
				check();
			}
			@Override
			public void insertUpdate(DocumentEvent e) {
				check();
			}
			@Override
			public void changedUpdate(DocumentEvent e) {
				check();
			}
			private void check() {
				List<String> driverURLs = retrieveDriverURLs(driverlist);
				downloadButton.setEnabled(driverURLs != null
						&& Arrays.asList(jar1, jar2, jar3, jar4).stream().allMatch(f -> f.getText().trim().isEmpty()));
				downloadButton.setToolTipText(driverURLs == null? null :
					driverURLs.stream().collect(Collectors.joining("<br>", "<html>", "</html>"))
					);
			}
		}));
		if (needsTest) {
			testConnectionButton.setVisible(false);
		} else {
			selectConnectionButton.setVisible(false);
		}
		InfoBar theInfoBar = null;
		if (infoBar != null) {
			UIUtil.replace(infoBarLabel, theInfoBar = infoBar);
		} else if (forNew) {
			UIUtil.replace(infoBarLabel, theInfoBar = new InfoBar("New Connection",
					"Enter connection credentials for the database.\n" +
					"Replace placeholders (\"<...>\") with appropriate URL parameters.", null));
		} else {
			UIUtil.replace(infoBarLabel, theInfoBar = new InfoBar("Edit Connection",
					"Edit connection credentials for the database.", null));
		}
		if (theInfoBar != null) {
			theInfoBar.shrink();
		}
		loadButton1.addActionListener(new java.awt.event.ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				String fn = UIUtil.choseFile(null, Environment.isJPacked()? Environment.newFile("lib").getAbsolutePath() : new File(".").getAbsolutePath(), "Jdbc Driver", ".jar", DbConnectionDetailsEditor.this, true, true, false);
				if (fn != null) {
					jar1.setText(fn);
				}
			}
		});
		loadButton2.addActionListener(new java.awt.event.ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				String fn = UIUtil.choseFile(null, Environment.isJPacked()? Environment.newFile("lib").getAbsolutePath() : new File(".").getAbsolutePath(), "Jdbc Driver", ".jar", DbConnectionDetailsEditor.this, true, true, false);
				if (fn != null) {
					jar2.setText(fn);
				}
			}
		});
		loadButton3.addActionListener(new java.awt.event.ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				String fn = UIUtil.choseFile(null, Environment.isJPacked()? Environment.newFile("lib").getAbsolutePath() : new File(".").getAbsolutePath(), "Jdbc Driver", ".jar", DbConnectionDetailsEditor.this, true, true, false);
				if (fn != null) {
					jar3.setText(fn);
				}
			}
		});
		loadButton4.addActionListener(new java.awt.event.ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				String fn = UIUtil.choseFile(null, Environment.isJPacked()? Environment.newFile("lib").getAbsolutePath() : new File(".").getAbsolutePath(), "Jdbc Driver", ".jar", DbConnectionDetailsEditor.this, true, true, false);
				if (fn != null) {
					jar4.setText(fn);
				}
			}
		});
		helpjdbc.addMouseListener(new java.awt.event.MouseAdapter() {
			@Override
			public void mouseEntered(java.awt.event.MouseEvent evt) {
				helpjdbc.setEnabled(false);
			}
			@Override
			public void mouseExited(java.awt.event.MouseEvent evt) {
				helpjdbc.setEnabled(true);
		   }
		});
		helpjdbc.addMouseListener(new java.awt.event.MouseAdapter() {
			@Override
			public void mouseClicked(java.awt.event.MouseEvent evt) {
				try {
					BrowserLauncher.openURL(new URI(jdbcHelpURL), DbConnectionDetailsEditor.this);
				} catch (Exception e) {
					UIUtil.showException(DbConnectionDetailsEditor.this, "Error", e);
				}
			}
		});
		helpjdbc.setIcon(helpIcon);
		helpjdbc.setText(null);
		loadButton1.setIcon(loadIcon);
		loadButton2.setIcon(loadIcon);
		loadButton3.setIcon(loadIcon);
		loadButton4.setIcon(loadIcon);
		
		addWindowListener(new WindowAdapter() {
			@Override
			public void windowOpened(WindowEvent evt) {
				alias.grabFocus();
				UIUtil.invokeLater(4, () -> {
					List<String> driverURLs = retrieveDriverURLs(driverlist);
					if (forNew && driverURLs != null && downloadButton.isEnabled() && driverURLs.stream().allMatch(
							url -> {
								try {
									return new File(Environment.newFile(HttpDownload.DOWNLOADFOLDER), HttpDownload.toFileName(new URL(url))).exists();
								} catch (Exception e) {
									return false;
								}
							})) {
						UIUtil.invokeLater(() -> {
							List<String> files = driverURLs.stream().map(url -> {
								try {
									return new File(Environment.newFile(HttpDownload.DOWNLOADFOLDER), HttpDownload.toFileName(new URL(url))).getAbsolutePath();
								} catch (Exception e) {
									return "";
								}
							}).collect(Collectors.toList());
							if (files.size() > 0) {
								jar1.setText(toRelFileName(files.get(0)));
							}
							if (files.size() > 1) {
								jar2.setText(toRelFileName(files.get(1)));
							}
							if (files.size() > 2) {
								jar3.setText(toRelFileName(files.get(2)));
							}
							if (files.size() > 3) {
								jar4.setText(toRelFileName(files.get(3)));
							}
						});
					}
				});
			}
			@Override
			public void windowClosed(WindowEvent e) {
				feedbackTimer.stop();
			}
		});
		
		pack();
		setSize(Math.max(570, getWidth()), getHeight());
		if (parent != null) {
			setLocation(parent.getX() + (parent.getWidth() - getWidth()) / 2, Math.max(0, parent.getY() + (parent.getHeight() - getHeight()) / 2));			
		} else {
			setLocation(120, 170);
		}
	 }

	protected List<String> retrieveDriverURLs(List<Line> driverlist) {
		if (driverlist != null) {
			String url = dbUrl.getText().trim().replaceAll("^(\\w+:\\w+:).*", "$1");
			Line line = driverlist.stream().filter(l -> l.cells.get(1).startsWith(url)).findFirst().orElse(null);
			if (line != null) {
				if (!line.cells.get(4).isEmpty()) {
					return new ArrayList<String>(Arrays.asList(line.cells.get(4).split("\\s+")));
				}
				if (!line.cells.get(3).isEmpty()) {
					return Arrays.stream(line.cells.get(3).split("\\s+")).map(fileName -> " " + fileName).collect(Collectors.toList());
				}
			}
		}
		return null;
	}

	/** This method is called from within the constructor to
	 * initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is
	 * always regenerated by the Form Editor.
	 */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jPanel1 = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();
        jLabel6 = new javax.swing.JLabel();
        jLabel7 = new javax.swing.JLabel();
        jLabel8 = new javax.swing.JLabel();
        jLabel9 = new javax.swing.JLabel();
        jar1 = new javax.swing.JTextField();
        jar2 = new javax.swing.JTextField();
        driverClass = new javax.swing.JTextField();
        dbUrl = new javax.swing.JTextField();
        user = new javax.swing.JTextField();
        jPanel2 = new javax.swing.JPanel();
        okButton = new javax.swing.JButton();
        cancelButton = new javax.swing.JButton();
        testConnectionButton = new javax.swing.JButton();
        selectConnectionButton = new javax.swing.JButton();
        jLabel11 = new javax.swing.JLabel();
        password = new javax.swing.JPasswordField();
        helpjdbc = new javax.swing.JLabel();
        alias = new javax.swing.JTextField();
        loadButton1 = new javax.swing.JButton();
        loadButton2 = new javax.swing.JButton();
        jar3 = new javax.swing.JTextField();
        jar4 = new javax.swing.JTextField();
        jLabel10 = new javax.swing.JLabel();
        jLabel12 = new javax.swing.JLabel();
        loadButton3 = new javax.swing.JButton();
        loadButton4 = new javax.swing.JButton();
        jPanel4 = new javax.swing.JPanel();
        infoBarLabel = new javax.swing.JLabel();
        jPanel3 = new javax.swing.JPanel();
        jPanel5 = new javax.swing.JPanel();
        exportCBButton = new javax.swing.JButton();
        importCBButton = new javax.swing.JButton();
        feedbackLabel = new javax.swing.JLabel();
        jSeparator1 = new javax.swing.JSeparator();
        downloadButton = new javax.swing.JButton();
        jSeparator2 = new javax.swing.JSeparator();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Database Connection");
        getContentPane().setLayout(new java.awt.CardLayout());

        jPanel1.setLayout(new java.awt.GridBagLayout());

        jLabel1.setText(" Alias ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(jLabel1, gridBagConstraints);

        jLabel2.setText(" JDBC Driver JAR ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(jLabel2, gridBagConstraints);

        jLabel3.setText(" 3. additional JAR ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 42;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(jLabel3, gridBagConstraints);

        jLabel4.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridy = 15;
        jPanel1.add(jLabel4, gridBagConstraints);

        jLabel6.setText(" Driver-Class");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 50;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(jLabel6, gridBagConstraints);

        jLabel7.setText(" DB-URL");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 60;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(jLabel7, gridBagConstraints);

        jLabel8.setText(" User");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 70;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(jLabel8, gridBagConstraints);

        jLabel9.setText(" Password");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 80;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(jLabel9, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(jar1, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 40;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(jar2, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 50;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(driverClass, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 60;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(dbUrl, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 70;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(user, gridBagConstraints);

        jPanel2.setLayout(new java.awt.GridBagLayout());

        okButton.setText("  Ok  ");
        okButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                okButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHEAST;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 0, 4);
        jPanel2.add(okButton, gridBagConstraints);

        cancelButton.setText(" Cancel ");
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHEAST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        jPanel2.add(cancelButton, gridBagConstraints);

        testConnectionButton.setText(" Test Connection ");
        testConnectionButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                testConnectionButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHEAST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 0, 12);
        jPanel2.add(testConnectionButton, gridBagConstraints);

        selectConnectionButton.setText("Select Connection ");
        selectConnectionButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                selectConnectionButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHEAST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 0, 12);
        jPanel2.add(selectConnectionButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 100;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        jPanel1.add(jPanel2, gridBagConstraints);

        jLabel11.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 10;
        jPanel1.add(jLabel11, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 80;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        jPanel1.add(password, gridBagConstraints);

        helpjdbc.setText("help");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 4);
        jPanel1.add(helpjdbc, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(alias, gridBagConstraints);

        loadButton1.setText(" Browse..");
        loadButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                loadButton1ActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 20;
        jPanel1.add(loadButton1, gridBagConstraints);

        loadButton2.setText(" Browse..");
        loadButton2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                loadButton2ActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 40;
        jPanel1.add(loadButton2, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 41;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(jar3, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 42;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(jar4, gridBagConstraints);

        jLabel10.setText(" 1. additional JAR ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 40;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(jLabel10, gridBagConstraints);

        jLabel12.setText(" 2. additional JAR ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 41;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(jLabel12, gridBagConstraints);

        loadButton3.setText(" Browse..");
        loadButton3.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                loadButton3ActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 41;
        jPanel1.add(loadButton3, gridBagConstraints);

        loadButton4.setText(" Browse..");
        loadButton4.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                loadButton4ActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 42;
        jPanel1.add(loadButton4, gridBagConstraints);

        jPanel4.setLayout(new java.awt.GridBagLayout());

        infoBarLabel.setText("info bar");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 12);
        jPanel4.add(infoBarLabel, gridBagConstraints);

        jPanel3.setLayout(new java.awt.GridBagLayout());

        jPanel5.setLayout(new java.awt.GridBagLayout());

        exportCBButton.setText("Copy Credentials");
        exportCBButton.setToolTipText("Copy credentials to clipboard");
        exportCBButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                exportCBButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 7;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel5.add(exportCBButton, gridBagConstraints);

        importCBButton.setText("Paste Credentials");
        importCBButton.setToolTipText("Paste credentials from clipboard");
        importCBButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                importCBButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel5.add(importCBButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        jPanel3.add(jPanel5, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHEAST;
        gridBagConstraints.insets = new java.awt.Insets(6, 0, 0, 4);
        jPanel4.add(jPanel3, gridBagConstraints);

        feedbackLabel.setFont(feedbackLabel.getFont().deriveFont((feedbackLabel.getFont().getStyle() | java.awt.Font.ITALIC)));
        feedbackLabel.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 8);
        jPanel4.add(feedbackLabel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 11;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(jPanel4, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = java.awt.GridBagConstraints.RELATIVE;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 2, 16, 0);
        jPanel1.add(jSeparator1, gridBagConstraints);

        downloadButton.setText("Download Driver");
        downloadButton.setEnabled(false);
        downloadButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                downloadButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 44;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        jPanel1.add(downloadButton, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 46;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 16, 0);
        jPanel1.add(jSeparator2, gridBagConstraints);

        getContentPane().add(jPanel1, "card2");

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void okButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_okButtonActionPerformed
    	if (fillConnectionInfo()) {
    		if (needsTest) {
    			if (!DbConnectionDialog.testConnection(isVisible()? this : parent, ci, createDownloadButton())) {
    				return;
    			}
    		}
		   isOk = true;
		   onClose(isOk, ci);
		}
	}//GEN-LAST:event_okButtonActionPerformed

	protected void onClose(boolean ok, ConnectionInfo info) {
		setVisible(false);
	}

    @SuppressWarnings("deprecation")
	private boolean fillConnectionInfo() {
		boolean ok = true;
		Color red = new Color(255, 200, 180);
		if (alias.getText().trim().length() == 0) {
			alias.setBackground(red);
			ok = false;
		}
		if (driverClass.getText().trim().length() == 0) {
			driverClass.setBackground(red);
			ok = false;
		}
		if (dbUrl.getText().trim().length() == 0) {
			dbUrl.setBackground(red);
			ok = false;
		}
		if (ok) {
			ci.alias = alias.getText().trim();
			ci.driverClass = driverClass.getText().trim();
			ci.jar1 = jar1.getText().trim();
			ci.jar2 = jar2.getText().trim();
			ci.jar3 = jar3.getText().trim();
			ci.jar4 = jar4.getText().trim();
			ci.url = dbUrl.getText().trim();
			ci.user = user.getText().trim();
			ci.password = password.getText().trim();
		}
		return ok;
	}

	private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
		onClose(false, ci);
	}//GEN-LAST:event_cancelButtonActionPerformed

	private void testConnectionButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_testConnectionButtonActionPerformed
		ConnectionInfo oldCi = new ConnectionInfo();
		oldCi.assign(ci);
		if (fillConnectionInfo()) {
			if (DbConnectionDialog.testConnection(isVisible()? this : parent, ci, createDownloadButton())) {
				JOptionPane.showMessageDialog(isVisible()? this : parent, "Successfully established connection.", "Connected", JOptionPane.INFORMATION_MESSAGE);
			}
			ci.assign(oldCi);
		}
	}//GEN-LAST:event_testConnectionButtonActionPerformed

	private JButton createDownloadButton() {
		JButton button = null;
		if (downloadButton.isEnabled()) {
			JButton finalButton = new JButton("Download Driver");
			button = finalButton;
			button.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					Window window = SwingUtilities.getWindowAncestor(finalButton);
					if (window != null) {
						window.setVisible(false);
						window.dispose();
					}
					downloadButton.doClick();
				}
			});
		}
		return button;
	}

	private void loadButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_loadButton1ActionPerformed
	}//GEN-LAST:event_loadButton1ActionPerformed

	private void loadButton2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_loadButton2ActionPerformed
	}//GEN-LAST:event_loadButton2ActionPerformed

    private void loadButton3ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_loadButton3ActionPerformed
    }//GEN-LAST:event_loadButton3ActionPerformed

    private void loadButton4ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_loadButton4ActionPerformed
    }//GEN-LAST:event_loadButton4ActionPerformed

    private void selectConnectionButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_selectConnectionButtonActionPerformed
        onSelect();
    }//GEN-LAST:event_selectConnectionButtonActionPerformed

    private final static String[] SEPARATORS = new String[] { "|", ";", ",", "#", "*", "~", "!", "-" };
	
    private void exportCBButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_exportCBButtonActionPerformed
    	try {
        	Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
        	String content;
        	String rawContent = Arrays.stream(textFields).map(field -> field.getText()).collect(Collectors.joining());
        	String separator = Arrays.stream(SEPARATORS).filter(sep -> !rawContent.contains(sep)).findAny().orElseGet(() -> null);
        	if (separator != null) {
        		content = separator + Arrays.stream(textFields).map(field -> field.getText()).collect(Collectors.joining(separator)) + separator;
        		StringSelection contents = new StringSelection(content);
        		clipboard.setContents(contents, contents);
        		showFeedback("credentials copied to clipboard");
        		return;
        	}
    	} catch (Exception e) {
    		e.printStackTrace();
    	}
		Toolkit.getDefaultToolkit().beep();
    }//GEN-LAST:event_exportCBButtonActionPerformed

    private void importCBButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_importCBButtonActionPerformed
    	String content = null;
    	try {
    		Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
    		Transferable contents = clipboard.getContents(this);
    		if (contents != null) {
    			content = contents.getTransferData(DataFlavor.stringFlavor).toString().trim();
    			if (content.length() > 1
    					&& content.substring(0, 1).equals(content.substring(content.length() - 1))
    					&& Arrays.asList(SEPARATORS).contains(content.substring(0, 1))) {
    				String separator = content.substring(0, 1);
    				content = content.substring(1);
    				String[] fields = content.split(Pattern.quote(separator));
    				for (int i = 0; i < textFields.length && i < fields.length; ++i) {
    					textFields[i].setText(fields[i]);
    				}
    				showFeedback("credentials taken from clipboard");
            		return;
    			}
    		}
    	} catch (Exception e) {
    		// ignore
    	}
    	Toolkit.getDefaultToolkit().beep();
    }//GEN-LAST:event_importCBButtonActionPerformed

	private void downloadButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_downloadButtonActionPerformed
    	List<String> driverURLs = retrieveDriverURLs(driverlist);
		if (driverURLs != null) {
			final List<String> files;
			final Object LOCK = new Object();
			synchronized (LOCK) {
				files = new ArrayList<String>();
			}
			
			AtomicBoolean ok = new AtomicBoolean(true);
			Function<Long, String> updateInfo = total -> {
				String text = "<html><b>Downloading... " + (total > 0? "(" + total / 1024 + "K)" : "") + "</b><br>"
						+ driverURLs.stream().collect(Collectors.joining("<br>")) + "</html>";
				return text;
			};
			
			@SuppressWarnings("serial")
			final ConcurrentTaskControl concurrentTaskControl = new ConcurrentTaskControl(
					this, updateInfo.apply(0L)) {

				@Override
				protected void onError(Throwable error) {
					UIUtil.showException(this, "Error", error);
					ok.set(false);
					closeWindow();
				}

				@Override
				protected void onCancellation() {
					ok.set(false);
					closeWindow();
				}
			};
			
			ConcurrentTaskControl.openInModalDialog(this, concurrentTaskControl, 
					new ConcurrentTaskControl.Task() {
				@Override
				public void run() throws Throwable {
					long[] total = { 0 };
					driverURLs.forEach(url -> {
						try {
							if (url.startsWith(" ")) {
								synchronized (LOCK) {
									files.add(url.substring(1));
								}
							} else {
								String result = HttpDownload.get(url, vol -> {
									String text = updateInfo.apply(total[0] += vol);
									UIUtil.invokeLater(() -> {
										concurrentTaskControl.master.infoLabel.setText(text);
									});
								});
								if (result.length() == 0) {
									throw new RuntimeException("cannot download \"" + url + "\"");
								}
								synchronized (LOCK) {
									files.add(result.toString());
								}
							}
						} catch (Throwable t) {
							throw new RuntimeException("cannot download \"" + url + "\"", t);
						}
					});

					UIUtil.invokeLater(new Runnable() {
						@Override
						public void run() {
							if (ok.get()) {
								synchronized (LOCK) {
									if (files.size() > 0) {
										jar1.setText(toRelFileName(files.get(0)));
									}
									if (files.size() > 1) {
										jar2.setText(toRelFileName(files.get(1)));
									}
									if (files.size() > 2) {
										jar3.setText(toRelFileName(files.get(2)));
									}
									if (files.size() > 3) {
										jar4.setText(toRelFileName(files.get(3)));
									}
								}
							}
							concurrentTaskControl.closeWindow();
						}
					});
				}
			}, "Downloading Driver");
		}
    }//GEN-LAST:event_downloadButtonActionPerformed

	private String toRelFileName(final String fileName) {
		try {
            File f = new File(fileName);
            String work = new File(".").getCanonicalPath();
            if (f.getCanonicalPath().startsWith(work)) {
                String fn = f.getName();
                f = f.getParentFile();
                while (f != null && !f.getCanonicalPath().equals(work)) {
                    fn = f.getName() + File.separator + fn;
                    f = f.getParentFile();
                }
                return fn;
            } else {
                return fileName;
            }
		} catch (Throwable t) {
			return fileName;
		}
	}

	protected void onSelect() {
	}

    // Variables declaration - do not modify//GEN-BEGIN:variables
    public javax.swing.JTextField alias;
    private javax.swing.JButton cancelButton;
    public javax.swing.JTextField dbUrl;
    private javax.swing.JButton downloadButton;
    public javax.swing.JTextField driverClass;
    private javax.swing.JButton exportCBButton;
    private javax.swing.JLabel feedbackLabel;
    private javax.swing.JLabel helpjdbc;
    private javax.swing.JButton importCBButton;
    private javax.swing.JLabel infoBarLabel;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel10;
    private javax.swing.JLabel jLabel11;
    private javax.swing.JLabel jLabel12;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JSeparator jSeparator2;
    private javax.swing.JTextField jar1;
    private javax.swing.JTextField jar2;
    private javax.swing.JTextField jar3;
    private javax.swing.JTextField jar4;
    private javax.swing.JButton loadButton1;
    private javax.swing.JButton loadButton2;
    private javax.swing.JButton loadButton3;
    private javax.swing.JButton loadButton4;
    private javax.swing.JButton okButton;
    javax.swing.JPasswordField password;
    private javax.swing.JButton selectConnectionButton;
    private javax.swing.JButton testConnectionButton;
    public javax.swing.JTextField user;
    // End of variables declaration//GEN-END:variables

    private final Timer feedbackTimer;
    
    private void showFeedback(String message) {
		feedbackLabel.setText(message);
		feedbackTimer.restart();
	}

    private javax.swing.JTextField[] textFields;
    
	private Icon helpIcon;
	private Icon loadIcon;
	{
		// load images
		helpIcon = UIUtil.readImage("/help.png");
		loadIcon = UIUtil.readImage("/load.png");
	}
	 
	private static final long serialVersionUID = -492511696901313920L;
}
