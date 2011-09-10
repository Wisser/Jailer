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
package net.sf.jailer.ui.databrowser;

import java.awt.Cursor;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import javax.swing.ToolTipManager;
import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;

import jsyntaxpane.DefaultSyntaxKit;
import net.sf.jailer.CommandLineParser;
import net.sf.jailer.Jailer;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.modelbuilder.ModelBuilder;
import net.sf.jailer.ui.About;
import net.sf.jailer.ui.AnalyseOptionsDialog;
import net.sf.jailer.ui.BrowserLauncher;
import net.sf.jailer.ui.DataModelEditor;
import net.sf.jailer.ui.DbConnectionDialog;
import net.sf.jailer.ui.DbConnectionDialog.ConnectionInfo;
import net.sf.jailer.ui.UIUtil;

import org.apache.log4j.PropertyConfigurator;

/**
 * Data Browser Frame.
 * 
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public class DataBrowser extends javax.swing.JFrame {

	/**
	 * The desktop.
	 */
	private Desktop desktop;

	/**
	 * Icon for the frame.
	 */
	private ImageIcon jailerIcon = null;

	/**
	 * The {@link DataModel}.
	 */
	private final Reference<DataModel> datamodel;

	/**
	 * The DB connection dialog.
	 */
	private DbConnectionDialog dbConnectionDialog;

	/**
	 * Session.
	 */
	private Session session;

	/**
	 * Constructor.
	 * 
	 * @param datamodel
	 *            the {@link DataModel}
	 * @param root
	 *            table to start browsing with
	 * @param condition
	 *            initial condition
	 * @param dbConnectionDialog
	 *            DB-connection dialog
	 */
	public DataBrowser(DataModel datamodel, Table root, String condition, DbConnectionDialog dbConnectionDialog, boolean embedded) throws Exception {
		this.datamodel = new Reference<DataModel>(datamodel);
		this.dbConnectionDialog = dbConnectionDialog != null? new DbConnectionDialog(dbConnectionDialog) : null;
		initComponents();
		setTitle("Jailer " + Jailer.VERSION + " Data Browser");
		if (embedded) {
			menuTools.setVisible(false);
		}

		try {
			for (final LookAndFeelInfo lfInfo : UIManager.getInstalledLookAndFeels()) {
				JMenuItem mItem = new JMenuItem();
				mItem.setText(lfInfo.getName());
				view.add(mItem);
				mItem.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent arg0) {
						setPLAF(lfInfo.getClassName());
					}
				});
			}
		} catch (Throwable t) {
		}

		try {
			setIconImage((jailerIcon = new ImageIcon(getClass().getResource("/net/sf/jailer/resource/jailer.png"))).getImage());
		} catch (Throwable t) {
			try {
				setIconImage((jailerIcon = new ImageIcon(getClass().getResource("/net/sf/jailer/resource/jailer.gif"))).getImage());
			} catch (Throwable t2) {
			}
		}

		jailerIcon.setImage(jailerIcon.getImage().getScaledInstance(16, 16, Image.SCALE_SMOOTH));

		if (dbConnectionDialog != null) {
			createSession(dbConnectionDialog);
		}
		desktop = new Desktop(this.datamodel, jailerIcon, session, this, dbConnectionDialog);

		jScrollPane1.setViewportView(desktop);
		addWindowListener(new WindowListener() {
			@Override
			public void windowOpened(WindowEvent e) {
			}

			@Override
			public void windowIconified(WindowEvent e) {
			}

			@Override
			public void windowDeiconified(WindowEvent e) {
			}

			@Override
			public void windowDeactivated(WindowEvent e) {
			}

			@Override
			public void windowClosing(WindowEvent e) {
				desktop.stop();
			}

			@Override
			public void windowClosed(WindowEvent e) {
				desktop.stop();
			}

			@Override
			public void windowActivated(WindowEvent e) {
			}
		});
		setLocation(100, 100);
		setSize(900, 580);
		if (root != null) {
			desktop.addTableBrowser(null, 0, root, null, condition);
		}
		schemaNamePanel.addMouseListener(new java.awt.event.MouseAdapter() {
			private boolean in = false;

			@Override
			public void mousePressed(MouseEvent e) {
				desktop.openSchemaMappingDialog(false);
			}

			public void mouseEntered(java.awt.event.MouseEvent evt) {
				in = true;
				updateBorder();
			}

			public void mouseExited(java.awt.event.MouseEvent evt) {
				in = false;
				updateBorder();
			}

			private void updateBorder() {
				schemaNamePanel.setBorder(new javax.swing.border.SoftBevelBorder(in ? javax.swing.border.BevelBorder.LOWERED
						: javax.swing.border.BevelBorder.RAISED));
			}
		});
		updateStatusBar();
	}

	private void createSession(DbConnectionDialog dbConnectionDialog)
			throws Exception {
		ConnectionInfo connection = dbConnectionDialog.currentConnection;
		session = new Session(connection.driverClass, connection.url, connection.user, connection.password);
		List<String> args = new ArrayList<String>();
		dbConnectionDialog.addDbArgs(args);
		session.setCliArguments(args);
		session.setPassword(dbConnectionDialog.getPassword());
	}

	protected void setConnection(DbConnectionDialog dbConnectionDialog) throws Exception {
		if (dbConnectionDialog != null) {
			dbConnectionDialog = new DbConnectionDialog(dbConnectionDialog);
		}
		this.dbConnectionDialog = dbConnectionDialog;
		desktop.dbConnectionDialog = dbConnectionDialog;
		if (dbConnectionDialog != null) {
			ConnectionInfo connection = dbConnectionDialog.currentConnection;
			if (connection != null) {
				createSession(dbConnectionDialog);
				desktop.session = session;
				updateStatusBar();
			}
		}
	}

	public void updateStatusBar() {
		final int MAX_LENGTH = 50;
		ConnectionInfo connection = dbConnectionDialog != null? dbConnectionDialog.currentConnection : null;
		String dburl = connection != null ? (connection.user + "@" + connection.url) : " ";
		connectivityState.setToolTipText(dburl);
		if (dburl.length() > MAX_LENGTH) {
			dburl = dburl.substring(0, MAX_LENGTH - 3) + "...";
		}
		connectivityState.setText(dburl);
		DataModel dataModel = datamodel != null? datamodel.get() : null;
		String modelname = "Data Model \"" + (dataModel == null? DataModel.DEFAULT_NAME : dataModel.getName()) + "\"";
		String lastMod = dataModel == null? "" : dataModel.getLastModifiedAsString();
		if (lastMod.length() > 0) {
			lastMod = " (" + lastMod + ")";
		}
		modelName.setText(modelname);
		modelName.setToolTipText(modelname + lastMod);
		
		String modelpath = CommandLineParser.getInstance().datamodelFolder;
		try {
			modelpath = CommandLineParser.getInstance().newFile(modelpath).getAbsolutePath();
		} catch (Throwable t) {
			// use default modelpath
		}
		modelpath += File.separator;
		modelPath.setToolTipText(modelpath);
		if (modelpath.length() > MAX_LENGTH + 4) {
			modelpath = modelpath.substring(0, MAX_LENGTH/2) + "..." + modelpath.substring(modelpath.length() - MAX_LENGTH/2);
		}
		modelPath.setText(modelpath);
		
		String nonDefaultSchema = null;
		if (desktop.schemaMapping != null) {
			for (Map.Entry<String, String> e: desktop.schemaMapping.entrySet()) {
				if (!e.getKey().equalsIgnoreCase(e.getValue())) {
					nonDefaultSchema = e.getValue();
					break;
				}
			}
		}
		schemaNamePanel.setVisible(nonDefaultSchema != null);
		if (nonDefaultSchema != null) {
			if (nonDefaultSchema.equals("")) {
				schemaName.setText("Default Schema");
			} else {
				schemaName.setText("Schema " + nonDefaultSchema + "");
			}
//			schemaName.setToolTipText(schemaName.getText());
		}
	}

	/**
	 * This method is called from within the constructor to initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is always
	 * regenerated by the Form Editor.
	 */
	// <editor-fold defaultstate="collapsed"
	// <editor-fold defaultstate="collapsed"
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jPanel1 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        jInternalFrame1 = new javax.swing.JInternalFrame();
        jLabel1 = new javax.swing.JLabel();
        jPanel11 = new javax.swing.JPanel();
        legende1 = new javax.swing.JPanel();
        modelName = new javax.swing.JLabel();
        modelPath = new javax.swing.JLabel();
        legende = new javax.swing.JPanel();
        dependsOn = new javax.swing.JLabel();
        hasDependent = new javax.swing.JLabel();
        associatedWith = new javax.swing.JLabel();
        ignored = new javax.swing.JLabel();
        schemaNamePanel = new javax.swing.JPanel();
        schemaName = new javax.swing.JLabel();
        legende2 = new javax.swing.JPanel();
        connectivityState = new javax.swing.JLabel();
        jMenuBar1 = new javax.swing.JMenuBar();
        jMenu1 = new javax.swing.JMenu();
        jMenuItem1 = new javax.swing.JMenuItem();
        jSeparator2 = new javax.swing.JPopupMenu.Separator();
        schemaMappingMenuItem = new javax.swing.JMenuItem();
        jSeparator3 = new javax.swing.JPopupMenu.Separator();
        cloaseAllMenuItem = new javax.swing.JMenuItem();
        menuTools = new javax.swing.JMenu();
        analyseMenuItem = new javax.swing.JMenuItem();
        dataModelEditorjMenuItem = new javax.swing.JMenuItem();
        menuWindow = new javax.swing.JMenu();
        jMenuItem2 = new javax.swing.JMenuItem();
        jSeparator1 = new javax.swing.JPopupMenu.Separator();
        view = new javax.swing.JMenu();
        jMenu2 = new javax.swing.JMenu();
        helpForum = new javax.swing.JMenuItem();
        jMenuItem4 = new javax.swing.JMenuItem();

        jPanel1.setLayout(new java.awt.BorderLayout());

        jScrollPane1.setAutoscrolls(true);

        jInternalFrame1.setVisible(true);

        jLabel1.setText("jLabel1");
        jInternalFrame1.getContentPane().add(jLabel1, java.awt.BorderLayout.CENTER);

        jScrollPane1.setViewportView(jInternalFrame1);

        jPanel1.add(jScrollPane1, java.awt.BorderLayout.CENTER);

        jPanel11.setLayout(new java.awt.GridBagLayout());

        legende1.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
        legende1.setLayout(new java.awt.GridBagLayout());

        modelName.setFont(new java.awt.Font("Dialog", 0, 12));
        modelName.setText("Data Model \"Demo\"");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 12);
        legende1.add(modelName, gridBagConstraints);

        modelPath.setFont(new java.awt.Font("Dialog", 0, 12));
        modelPath.setForeground(java.awt.Color.gray);
        modelPath.setText("/home/jailer/datamodel/");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        legende1.add(modelPath, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 2);
        jPanel11.add(legende1, gridBagConstraints);

        legende.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
        legende.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 0, 0));

        dependsOn.setFont(new java.awt.Font("Dialog", 0, 12));
        dependsOn.setForeground(new java.awt.Color(170, 0, 0));
        dependsOn.setText(" depends on");
        legende.add(dependsOn);

        hasDependent.setFont(new java.awt.Font("Dialog", 0, 12));
        hasDependent.setForeground(new java.awt.Color(0, 112, 0));
        hasDependent.setText("  has dependent");
        legende.add(hasDependent);

        associatedWith.setFont(new java.awt.Font("Dialog", 0, 12));
        associatedWith.setForeground(new java.awt.Color(0, 100, 255));
        associatedWith.setText("  associated with");
        legende.add(associatedWith);

        ignored.setFont(new java.awt.Font("Dialog", 0, 12));
        ignored.setForeground(new java.awt.Color(153, 153, 153));
        ignored.setText("  disabled ");
        legende.add(ignored);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 2);
        jPanel11.add(legende, gridBagConstraints);

        schemaNamePanel.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
        schemaNamePanel.setLayout(new java.awt.GridBagLayout());

        schemaName.setFont(new java.awt.Font("Dialog", 0, 12));
        schemaName.setText("Schema");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 12);
        schemaNamePanel.add(schemaName, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 2);
        jPanel11.add(schemaNamePanel, gridBagConstraints);

        legende2.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
        legende2.setLayout(new java.awt.GridBagLayout());

        connectivityState.setFont(new java.awt.Font("Dialog", 0, 12)); // NOI18N
        connectivityState.setText("offline");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 6);
        legende2.add(connectivityState, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 4);
        jPanel11.add(legende2, gridBagConstraints);

        jPanel1.add(jPanel11, java.awt.BorderLayout.SOUTH);

        getContentPane().add(jPanel1, java.awt.BorderLayout.CENTER);

        jMenu1.setText("File");

        jMenuItem1.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_T, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItem1.setText("New Table Browser");
        jMenuItem1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem1ActionPerformed(evt);
            }
        });
        jMenu1.add(jMenuItem1);
        jMenu1.add(jSeparator2);

        schemaMappingMenuItem.setText("Schema Mapping");
        schemaMappingMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                schemaMappingMenuItemActionPerformed(evt);
            }
        });
        jMenu1.add(schemaMappingMenuItem);
        jMenu1.add(jSeparator3);

        cloaseAllMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F4, java.awt.event.InputEvent.SHIFT_MASK | java.awt.event.InputEvent.CTRL_MASK));
        cloaseAllMenuItem.setText("Close All");
        cloaseAllMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cloaseAllMenuItemActionPerformed(evt);
            }
        });
        jMenu1.add(cloaseAllMenuItem);

        jMenuBar1.add(jMenu1);

        menuTools.setText("Data Model");

        analyseMenuItem.setText("Analyse Database");
        analyseMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                analyseMenuItemActionPerformed(evt);
            }
        });
        menuTools.add(analyseMenuItem);

        dataModelEditorjMenuItem.setText("Data Model Editor");
        dataModelEditorjMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                dataModelEditorjMenuItemActionPerformed(evt);
            }
        });
        menuTools.add(dataModelEditorjMenuItem);

        jMenuBar1.add(menuTools);

        menuWindow.setText("Window");

        jMenuItem2.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_L, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItem2.setText("Layout");
        jMenuItem2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem2ActionPerformed(evt);
            }
        });
        menuWindow.add(jMenuItem2);
        menuWindow.add(jSeparator1);

        view.setText("Look&Feel");
        menuWindow.add(view);

        jMenuBar1.add(menuWindow);

        jMenu2.setText("Help");

        helpForum.setText("Forum");
        helpForum.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                helpForumActionPerformed(evt);
            }
        });
        jMenu2.add(helpForum);

        jMenuItem4.setText("About Jailer");
        jMenuItem4.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem4ActionPerformed(evt);
            }
        });
        jMenu2.add(jMenuItem4);

        jMenuBar1.add(jMenu2);

        setJMenuBar(jMenuBar1);

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void jMenuItem1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem1ActionPerformed
        openNewTableBrowser();
    }//GEN-LAST:event_jMenuItem1ActionPerformed

    private void jMenuItem2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem2ActionPerformed
        desktop.layoutBrowser();
    }//GEN-LAST:event_jMenuItem2ActionPerformed

    private void cloaseAllMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cloaseAllMenuItemActionPerformed
    	desktop.closeAll();
    }//GEN-LAST:event_cloaseAllMenuItemActionPerformed

    private void schemaMappingMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_schemaMappingMenuItemActionPerformed
        desktop.openSchemaMappingDialog(false);
    }//GEN-LAST:event_schemaMappingMenuItemActionPerformed

	private void openNewTableBrowser() {
		new NewTableBrowser(this, datamodel.get()) {
        	@Override
			void openTableBrowser(String tableName) {
				desktop.addTableBrowser(null, 0, datamodel.get().getTableByDisplayName(tableName), null, "");
			}  	
        };
	}

	private void helpForumActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_helpForumActionPerformed
		try {
			BrowserLauncher.openURL(new URI("https://sourceforge.net/forum/?group_id=197260"));
		} catch (Exception e) {
			UIUtil.showException(this, "Error", e);
		}
	}// GEN-LAST:event_helpForumActionPerformed

	private void jMenuItem4ActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_jMenuItem4ActionPerformed
		About about = new About(this, true);
		about.setTitle("Jailer " + Jailer.VERSION);
		about.pack();
		about.setLocation(getLocation().x + (getSize().width - about.getPreferredSize().width) / 2, getLocation().y
				+ (getSize().height - about.getPreferredSize().height) / 2);
		about.setVisible(true);
	}// GEN-LAST:event_jMenuItem4ActionPerformed

	private void analyseMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_analyseMenuItemActionPerformed
		updateDataModel();
	}// GEN-LAST:event_analyseMenuItemActionPerformed

	private void dataModelEditorjMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_dataModelEditorjMenuItemActionPerformed
		openDataModelEditor();
	}// GEN-LAST:event_dataModelEditorjMenuItemActionPerformed

	/**
	 * File in which plaf-setting is stored.
	 */
	private static final String PLAFSETTING = ".plaf2.ui";

	/**
	 * @param args
	 *            the command line arguments
	 */
	public static void main(final String args[]) {
		// check working folder
		String configFileName = "jailer.xml";
		File configFile = new File(configFileName);
		if (!configFile.exists()) {
			JOptionPane.showMessageDialog(null, "File '" + configFileName + "' not found!", "Missing configuration file", JOptionPane.ERROR_MESSAGE);
			return;
		}

		// turn off logging for prefuse library
		try {
			Logger.getLogger("prefuse").setLevel(Level.OFF);

			// trigger log4j initialization
			new Jailer(1);
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		try {
			CommandLineParser.parse(args, true);
		} catch (Exception e) {
			UIUtil.showException(null, "Illegal arguments", e);
		}
		try {
			// create initial data-model files
			File file = new File(DataModel.getDatamodelFolder());
			if (!file.exists()) {
				file.mkdir();
			}
			file = new File(DataModel.getTablesFile());
			if (!file.exists()) {
				file.createNewFile();
			}
			file = new File(DataModel.getAssociationsFile());
			if (!file.exists()) {
				file.createNewFile();
			}
		} catch (Exception e) {
		}
		java.awt.EventQueue.invokeLater(new Runnable() {
			public void run() {
				try {
					DefaultSyntaxKit.initKit();
				} catch (Throwable e) {
					e.printStackTrace();
				}
				DataModel datamodel;
				try {
					CommandLineParser.parse(args, true);
					datamodel = new DataModel();
					try {
						File plafSetting = new File(PLAFSETTING);
						String plaf;
						if (!plafSetting.exists()) {
							plaf = "com.sun.java.swing.plaf.nimbus.NimbusLookAndFeel";
						} else {
							BufferedReader in = new BufferedReader(new FileReader(plafSetting));
							plaf = in.readLine();
							in.close();
						}
						UIManager.setLookAndFeel(plaf);
					} catch (Exception x) {
					}
					DataBrowser dataBrowser = new DataBrowser(datamodel, null, "", null, false);
					dataBrowser.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
					dataBrowser.setVisible(true);
					DbConnectionDialog dbConnectionDialog = new DbConnectionDialog(dataBrowser);
					if (dbConnectionDialog.connect("Jailer Data Browser")) {
						dataBrowser.setConnection(dbConnectionDialog);
						dataBrowser.askForDataModel();
						dataBrowser.desktop.openSchemaMappingDialog(true);
						dataBrowser.updateStatusBar();
						dataBrowser.openNewTableBrowser();
					} else {
						System.exit(0);
					}
					ToolTipManager.sharedInstance().setInitialDelay(500);
					ToolTipManager.sharedInstance().setDismissDelay(20000);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		});
	}

	/**
	 * Opens the data model editor.
	 */
	private void openDataModelEditor() {
		try {
			String modelname = datamodel == null || datamodel.get() == null? DataModel.DEFAULT_NAME : datamodel.get().getName();
    		DataModelEditor dataModelEditor = new DataModelEditor(this, false, false, null, null, null, modelname, null);
			dataModelEditor.setVisible(true);
			desktop.reloadDataModel(desktop.schemaMapping);
			updateStatusBar();
			askForDataModel();
		} catch (Exception e) {
			UIUtil.showException(this, "Error", e);
		}
	}

	private void updateDataModel() {
		try {
			List<String> args = new ArrayList<String>();
			args.add("build-model");
			dbConnectionDialog.addDbArgs(args);
			
			AnalyseOptionsDialog analyseOptionsDialog = new AnalyseOptionsDialog(this, datamodel == null? null : datamodel.get());
        	boolean[] isDefaultSchema = new boolean[1];
        	String[] defaultSchema = new String[1];
    		List<String> schemas;
    		setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
    		try {
    			schemas = dbConnectionDialog.getDBSchemas(defaultSchema);
    		} finally {
    			setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
    		}
    		if (analyseOptionsDialog.edit(schemas, defaultSchema[0], isDefaultSchema, dbConnectionDialog.currentConnection.user)) {
    			String schema = analyseOptionsDialog.getSelectedSchema();
				if (schema != null) {
					args.add("-schema");
					args.add(schema);
				}
				if (!isDefaultSchema[0]) {
					args.add("-qualifyNames");
				}
				ModelBuilder.assocFilter = analyseOptionsDialog.getAssociationLineFilter();
				if (UIUtil.runJailer(this, args, false, true, false, true, null, dbConnectionDialog.getPassword(), null, null, false, true)) {
					ModelBuilder.assocFilter = null;
					String modelname = datamodel == null || datamodel.get() == null? DataModel.DEFAULT_NAME : datamodel.get().getName();
	        		DataModelEditor dataModelEditor = new DataModelEditor(this, true, analyseOptionsDialog.isRemoving(), null, analyseOptionsDialog.getTableLineFilter(), analyseOptionsDialog.getAssociationLineFilter(), modelname, schema == null? dbConnectionDialog.getName() : schema);
	        		if (dataModelEditor.dataModelHasChanged()) {
						dataModelEditor.setVisible(true);
					}
	        		desktop.reloadDataModel(desktop.schemaMapping);
					updateStatusBar();
					askForDataModel();
				}
			} else {
				askForDataModel();
			}
		} catch (Exception e) {
			UIUtil.showException(this, "Error", e);
		} finally {
			ModelBuilder.assocFilter = null;
		}
	}

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JMenuItem analyseMenuItem;
    private javax.swing.JLabel associatedWith;
    private javax.swing.JMenuItem cloaseAllMenuItem;
    public javax.swing.JLabel connectivityState;
    private javax.swing.JMenuItem dataModelEditorjMenuItem;
    private javax.swing.JLabel dependsOn;
    private javax.swing.JLabel hasDependent;
    private javax.swing.JMenuItem helpForum;
    private javax.swing.JLabel ignored;
    private javax.swing.JInternalFrame jInternalFrame1;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JMenu jMenu1;
    private javax.swing.JMenu jMenu2;
    private javax.swing.JMenuBar jMenuBar1;
    private javax.swing.JMenuItem jMenuItem1;
    private javax.swing.JMenuItem jMenuItem2;
    private javax.swing.JMenuItem jMenuItem4;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel11;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JPopupMenu.Separator jSeparator1;
    private javax.swing.JPopupMenu.Separator jSeparator2;
    private javax.swing.JPopupMenu.Separator jSeparator3;
    private javax.swing.JPanel legende;
    private javax.swing.JPanel legende1;
    private javax.swing.JPanel legende2;
    private javax.swing.JMenu menuTools;
    private javax.swing.JMenu menuWindow;
    private javax.swing.JLabel modelName;
    private javax.swing.JLabel modelPath;
    private javax.swing.JMenuItem schemaMappingMenuItem;
    private javax.swing.JLabel schemaName;
    private javax.swing.JPanel schemaNamePanel;
    private javax.swing.JMenu view;
    // End of variables declaration//GEN-END:variables

	/**
	 * Sets Look&Feel.
	 * 
	 * @param plaf
	 *            the l&f
	 */
	private void setPLAF(String plaf) {
		try {
			UIManager.setLookAndFeel(plaf);
			SwingUtilities.updateComponentTreeUI(this);
			try {
				File file = new File(PLAFSETTING);
				file.delete();
			} catch (Exception e) {
			}
			try {
				File plafSetting = new File(PLAFSETTING);
				PrintWriter out = new PrintWriter(plafSetting);
				out.println(plaf);
				out.close();
			} catch (Exception x) {
			}
		} catch (Exception e) {
			UIUtil.showException(this, "Error", e);
		}
	}

	private void askForDataModel() {
		if (datamodel.get().getTables().isEmpty()) {
			switch (JOptionPane.showOptionDialog(this, "No Data Model found.", "Jailer " + Jailer.VERSION, JOptionPane.YES_NO_OPTION,
					JOptionPane.INFORMATION_MESSAGE, null, new Object[] { "Analyze Database", "Data Model Editor" }, null)) {
			case 0:
				updateDataModel();
				break;
			case 1:
				openDataModelEditor();
				break;
			}
		} else if (!new File(DataModel.getColumnsFile()).exists()) {
			switch (JOptionPane.showOptionDialog(this, "No column definition found.", "Jailer " + Jailer.VERSION, JOptionPane.YES_NO_OPTION,
					JOptionPane.INFORMATION_MESSAGE, null, new Object[] { "Analyze Database", "Data Model Editor" }, null)) {
			case 0:
				updateDataModel();
				break;
			case 1:
				openDataModelEditor();
				break;
			}
		}
	}

	// initialize log4j
	static {
		InputStream in = Jailer.class.getResourceAsStream("/net/sf/jailer/resource/log4j.properties");
		Properties p = new Properties();
		try {
			p.load(in);
		} catch (IOException e) {
			e.printStackTrace();
		}
		PropertyConfigurator.configure(p);
	}
}
