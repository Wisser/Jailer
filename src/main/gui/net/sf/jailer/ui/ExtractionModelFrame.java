/*
 * Copyright 2007 - 2022 Ralf Wisser.
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

import java.awt.CardLayout;
import java.awt.Container;
import java.awt.Frame;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.Callable;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;

import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JRadioButton;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.JailerVersion;
import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.database.BasicDataSource;
import net.sf.jailer.database.DMLTransformer;
import net.sf.jailer.database.Session;
import net.sf.jailer.database.WorkingTableScope;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.DataModel.NoPrimaryKeyException;
import net.sf.jailer.datamodel.PrimaryKeyFactory;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ddl.DDLCreator;
import net.sf.jailer.extractionmodel.ExtractionModel;
import net.sf.jailer.extractionmodel.ExtractionModel.AdditionalSubject;
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;
import net.sf.jailer.modelbuilder.ModelBuilder;
import net.sf.jailer.render.HtmlDataModelRenderer;
import net.sf.jailer.subsetting.ScriptFormat;
import net.sf.jailer.ui.UIUtil.ResultConsumer;
import net.sf.jailer.ui.associationproposer.AssociationProposerView;
import net.sf.jailer.ui.commandline.CommandLineInstance;
import net.sf.jailer.ui.commandline.UICommandLine;
import net.sf.jailer.ui.constraintcheck.ConstraintChecker;
import net.sf.jailer.ui.databrowser.BookmarksPanel.BookmarkId;
import net.sf.jailer.ui.databrowser.DataBrowser;
import net.sf.jailer.ui.progress.ExportAndDeleteStageProgressListener;
import net.sf.jailer.ui.util.AnimationController;
import net.sf.jailer.ui.util.UISettings;
import net.sf.jailer.ui.util.UpdateInfoManager;
import net.sf.jailer.util.PrintUtil;

/**
 * Main frame of Extraction-Model-Editor.
 *
 * @author Ralf Wisser
 */
public class ExtractionModelFrame extends javax.swing.JFrame {

	/**
	 * The embedded editor.
	 */
	ExtractionModelEditor extractionModelEditor;

	/**
	 * Dialog for DB-connects.
	 */
	DbConnectionDialog dbConnectionDialog;

	/**
	 * The filter editor.
	 */
	private final FilterEditorDialog filterEditorDialog;

	/**
	 * The border browser.
	 */
	final ClosureBorderDialog restrictedDependenciesView;

	/**
	 * The "Cycle View" dialog.
	 */
	final CyclesView cycleViewDialog;

	/**
	 * The execution context.
	 */
	private final ExecutionContext executionContext;

	/**
	 *  Creates new form ExtractionModelFrame.
	 *
	 *  @param extractionModelFile file containing the model, <code>null</code> for new model
	 *  @param isHorizonal
	 */
	public ExtractionModelFrame(String extractionModelFile, boolean isHorizonal, ExecutionContext executionContext) throws IOException {
		this(extractionModelFile, isHorizonal, null, executionContext);
	}

	private void storeLastSession() {
   		BookmarkId bookmark;
		bookmark = new BookmarkId(extractionModelEditor.extractionModelFile, ExtractionModelFrame.this.executionContext.getCurrentModelSubfolder(), ExtractionModelFrame.this.executionContext.getCurrentConnectionAlias(), null);
		UISettings.storeLastSession(bookmark, "S");
	}

	/**
	 *  Creates new form ExtractionModelFrame.
	 *
	 *  @param extractionModelFile file containing the model, <code>null</code> for new model
	 *  @param isHorizonal
	 */
	public ExtractionModelFrame(String extractionModelFile, boolean isHorizonal, DbConnectionDialog initDbConnectionDialog, ExecutionContext executionContext) throws IOException {
		this.executionContext = executionContext;
		initComponents();
		initMenu();
		initSandbox();
        AnimationController.registerWindow(this, new AnimationController.AnimationControl() {
			@Override
			public void setEnabled(boolean enabled) {
				if (extractionModelEditor != null && extractionModelEditor.graphView != null) {
					extractionModelEditor.graphView.setAnimationEnabled(enabled);
				}
			}
		});
		UIUtil.initPLAFMenuItem(plafMenu, this);
        UpdateInfoManager.checkUpdateAvailability(updateInfoPanel, updateInfoLabel, downloadMenuItem, "S");
        initAnimationSteptime();
		isHorizontalLayout = isHorizonal;
		horizontalLayoutMenuItem.setSelected(isHorizontalLayout);

		restrictedDependenciesView = new RestrictedDependenciesListDialog(this) {
			private static final long serialVersionUID = -7426280043553389753L;
			@Override
			protected List<Table> getRoots() {
				ArrayList<Table> roots = new ArrayList<Table>();
				if (extractionModelEditor != null) {
					if (extractionModelEditor.root != null) {
						roots.add(extractionModelEditor.root);
					}
				}
				return roots;
			}
			@Override
			protected DataModel getDataModel() {
				return extractionModelEditor == null? null : extractionModelEditor.dataModel;
			}
			@Override
			protected void removeRestrictions(Collection<Association> associations) {
				if (extractionModelEditor != null) {
					extractionModelEditor.removeRestrictions(associations);
				}
			}
			@Override
			protected void onSelect(Association association) {
				extractionModelEditor.select(association);
			}
		};

		editorPanel.add(extractionModelEditor = new ExtractionModelEditor(extractionModelFile, this, isHorizontalLayout, getConnectivityState(), getConnectivityStateToolTip(), getDBMSLogo(), executionContext), "editor");
		extractionModelEditor.extractionModelFile = extractionModelFile;
		restrictedDependenciesView.refresh();
		pack();
		updateTitle(extractionModelEditor.needsSave);
		if (initDbConnectionDialog != null) {
			dbConnectionDialog = new DbConnectionDialog(this, initDbConnectionDialog, JailerVersion.APPLICATION_NAME, executionContext);
		} else {
			dbConnectionDialog = new DbConnectionDialog(this, JailerVersion.APPLICATION_NAME, null, executionContext);
		}
        dbConnectionDialog.autoConnect();

        final String bmFile = CommandLineInstance.getInstance().bookmark;
		if (bmFile != null && !"".equals(bmFile) && new File(bmFile).exists()) {
			showWizzard = false;
			UIUtil.invokeLater(4, new Runnable() {
				@Override
				public void run() {
					load(bmFile);
				}
			});
		}

		updateMenuItems();

		cycleViewDialog = new CyclesView(this);
		filterEditorDialog = new FilterEditorDialog(this, new ParameterSelector.ParametersGetter() {
			@Override
			public Set<String> getParameters() {
				return extractionModelEditor.dataModel.getParameters(extractionModelEditor.condition.getText(), extractionModelEditor.extractionModel.additionalSubjects);
			}
		}, executionContext);
	}

	private boolean showWizzard = true;

	private void initMenu() {
		int mask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
		if (mask != InputEvent.CTRL_MASK) {
			for (int i = 0; i < jMenuBar2.getMenuCount(); ++i) {
				JMenu menu = jMenuBar2.getMenu(i);
				for (int j = 0; j < menu.getItemCount(); ++j) {
					JMenuItem item = menu.getItem(j);
					if (item != null) {
						KeyStroke accelerator = item.getAccelerator();
						if (accelerator != null) {
							item.setAccelerator(KeyStroke.getKeyStroke(accelerator.getKeyCode(), mask, accelerator.isOnKeyRelease()));
						}
					}
				}
			}
		}
	}

	private void initSandbox() {
		List<String> list = new ArrayList<String>();
		try {
			PrintUtil.loadTableList(list, ".sandbox");
			if (!list.isEmpty()) {
				JMenu sandBox = new JMenu("Sandbox");
				jMenuBar2.add(sandBox);
				for (final String item: list) {
					JMenuItem menuItem = new JMenuItem(item.split(",")[0]);
					sandBox.add(menuItem);
					final ActionListener a;
					menuItem.addActionListener(a = new ActionListener() {
						@Override
						public void actionPerformed(ActionEvent e) {
							try {
								Class<?> c = Class.forName(item.split(",")[1]);
								String tmpFileName = Configuration.getInstance().createTempFile().getPath();
								extractionModelEditor.save(tmpFileName);
								ExtractionModel extractionModel = new ExtractionModel(tmpFileName, new HashMap<String, String>(), new HashMap<String, String>(), executionContext);
								c.getMethod("main", ExtractionModel.class, JFrame.class).invoke(c.newInstance(), extractionModel, ExtractionModelFrame.this);
							} catch (Throwable t) {
								UIUtil.showException(ExtractionModelFrame.this, "Error", t);
							}
						}
					});
					if (menuItem.getText().endsWith("!")) {
						UIUtil.invokeLater(4, new Runnable() {
							@Override
							public void run() {
								a.actionPerformed(null);
							}
						});
					}
				}
			}
		} catch (Throwable e) {
			e.printStackTrace();
		}
	}

	/**
	 * Updates state of some menu items.
	 */
	private void updateMenuItems() {
		connectDb.setSelected(dbConnectionDialog.isConnected);
		disconnectDb.setEnabled(dbConnectionDialog.isConnected);
		extractionModelEditor.connectivityState.setText(getConnectivityState());
		extractionModelEditor.connectivityState.setToolTipText(getConnectivityStateToolTip());
		extractionModelEditor.connectivityState.setIcon(getDBMSLogo());
	}

	private String getConnectivityState() {
		if (dbConnectionDialog != null && dbConnectionDialog.isConnected) {
			return ((dbConnectionDialog.currentConnection.user != null && dbConnectionDialog.currentConnection.user.trim().length() > 0 && !dbConnectionDialog.currentConnection.alias.startsWith(dbConnectionDialog.currentConnection.user + "@")? dbConnectionDialog.currentConnection.user + "@" : "") + dbConnectionDialog.currentConnection.alias);
		} else {
			return "offline";
		}
	}

	private String getConnectivityStateToolTip() {
		if (dbConnectionDialog != null && dbConnectionDialog.isConnected) {
			return dbConnectionDialog.currentConnection.url;
		} else {
			return "offline";
		}
	}

	private ImageIcon getDBMSLogo() {
		if (dbConnectionDialog != null && dbConnectionDialog.isConnected) {
			String dbmsLogoURL = UIUtil.getDBMSLogoURL(dbConnectionDialog.currentConnection.url);
	        if (dbmsLogoURL == null) {
	        	return null;
	        } else {
	        	return UIUtil.scaleIcon(new JLabel(), UIUtil.readImage(dbmsLogoURL, false), 1.5);
	        }
		} else {
			return null;
		}
	}

	/**
	 * Opens the filter editor for a given table.
	 *
	 * @param table the table
	 */
	public void openFilterEditor(Table table) {
		filterEditorDialog.open(table);
	}

	/** This method is called from within the constructor to
	 * initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is
	 * always regenerated by the Form Editor.
	 */
	@SuppressWarnings("deprecation")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        steptimeGroup = new javax.swing.ButtonGroup();
        jLayeredPane1 = new javax.swing.JLayeredPane();
        editorPanel = new javax.swing.JPanel();
        updateInfoPanel = new javax.swing.JPanel();
        updateInfoLabel = new javax.swing.JLabel();
        downloadButton = new javax.swing.JButton();
        jButton1 = new javax.swing.JButton();
        jMenuBar2 = new javax.swing.JMenuBar();
        fileMenu = new javax.swing.JMenu();
        newModel = new javax.swing.JMenuItem();
        load = new javax.swing.JMenuItem();
        reload = new javax.swing.JMenuItem();
        jSeparator1 = new javax.swing.JSeparator();
        save = new javax.swing.JMenuItem();
        saveAs = new javax.swing.JMenuItem();
        exportDisplay = new javax.swing.JMenuItem();
        exportDisplay1 = new javax.swing.JMenuItem();
        jSeparator2 = new javax.swing.JSeparator();
        connectDb = new javax.swing.JCheckBoxMenuItem();
        disconnectDb = new javax.swing.JMenuItem();
        jSeparator10 = new javax.swing.JSeparator();
        exit = new javax.swing.JMenuItem();
        jMenu1 = new javax.swing.JMenu();
        updateDataModel = new javax.swing.JMenuItem();
        openDataModelEditor = new javax.swing.JMenuItem();
        jSeparator3 = new javax.swing.JPopupMenu.Separator();
        columnOrderItem = new javax.swing.JMenuItem();
        jSeparator14 = new javax.swing.JPopupMenu.Separator();
        analyzeSQLMenuItem = new javax.swing.JMenuItem();
        jSeparator15 = new javax.swing.JPopupMenu.Separator();
        modelMigrationMenuItem = new javax.swing.JMenuItem();
        checkPKMenuItem = new javax.swing.JMenuItem();
        consistencyCheckMenuItem1 = new javax.swing.JMenuItem();
        editMenu = new javax.swing.JMenu();
        undoMenuItem = new javax.swing.JMenuItem();
        redoMenuItem = new javax.swing.JMenuItem();
        jSeparator13 = new javax.swing.JPopupMenu.Separator();
        ignoreAll = new javax.swing.JMenuItem();
        removeAllRestrictions = new javax.swing.JMenuItem();
        jSeparator12 = new javax.swing.JSeparator();
        jMenuItem2 = new javax.swing.JMenuItem();
        viewMenu = new javax.swing.JMenu();
        collapseAll = new javax.swing.JMenuItem();
        expandAll = new javax.swing.JMenuItem();
        expandAllVisible = new javax.swing.JMenuItem();
        jMenuItem3 = new javax.swing.JMenuItem();
        jMenuItem4 = new javax.swing.JMenuItem();
        refresh = new javax.swing.JMenuItem();
        zoomToFit = new javax.swing.JMenuItem();
        jSeparator4 = new javax.swing.JSeparator();
        showIgnored = new javax.swing.JCheckBoxMenuItem();
        showTableDetails = new javax.swing.JCheckBoxMenuItem();
        jMenu3 = new javax.swing.JMenu();
        dataExport = new javax.swing.JMenuItem();
        dataImport = new javax.swing.JMenuItem();
        jSeparator5 = new javax.swing.JSeparator();
        openDataBrowserItem = new javax.swing.JMenuItem();
        queryBuilder = new javax.swing.JMenuItem();
        cycleView = new javax.swing.JMenuItem();
        renderHtml = new javax.swing.JMenuItem();
        renderHtml1 = new javax.swing.JMenuItem();
        consistencyCheckMenuItem = new javax.swing.JMenuItem();
        jMenu5 = new javax.swing.JMenu();
        horizontalLayoutMenuItem = new javax.swing.JCheckBoxMenuItem();
        jMenu4 = new javax.swing.JMenu();
        steptime0 = new javax.swing.JRadioButtonMenuItem();
        steptime10 = new javax.swing.JRadioButtonMenuItem();
        steptime20 = new javax.swing.JRadioButtonMenuItem();
        steptime30 = new javax.swing.JRadioButtonMenuItem();
        steptime50 = new javax.swing.JRadioButtonMenuItem();
        steptime75 = new javax.swing.JRadioButtonMenuItem();
        steptime100 = new javax.swing.JRadioButtonMenuItem();
        steptime200 = new javax.swing.JRadioButtonMenuItem();
        steptime500 = new javax.swing.JRadioButtonMenuItem();
        jSeparator6 = new javax.swing.JPopupMenu.Separator();
        plafMenu = new javax.swing.JMenu();
        jMenu2 = new javax.swing.JMenu();
        helpContent = new javax.swing.JMenuItem();
        tutorial = new javax.swing.JMenuItem();
        jSeparator7 = new javax.swing.JSeparator();
        helpForum = new javax.swing.JMenuItem();
        downloadMenuItem = new javax.swing.JMenuItem();
        jSeparator8 = new javax.swing.JSeparator();
        jMenuItem1 = new javax.swing.JMenuItem();

        setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);
        setTitle("Extraction Model Editor");
        addWindowFocusListener(new java.awt.event.WindowFocusListener() {
            public void windowGainedFocus(java.awt.event.WindowEvent evt) {
                formWindowGainedFocus(evt);
            }
            public void windowLostFocus(java.awt.event.WindowEvent evt) {
                formWindowLostFocus(evt);
            }
        });
        addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent evt) {
                formWindowClosing(evt);
            }
        });
        getContentPane().setLayout(new java.awt.GridBagLayout());

        jLayeredPane1.setLayout(new java.awt.GridBagLayout());

        editorPanel.setLayout(new java.awt.CardLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jLayeredPane1.add(editorPanel, gridBagConstraints);

        updateInfoPanel.setBackground(new java.awt.Color(255, 255, 236));
        updateInfoPanel.setLayout(new java.awt.GridBagLayout());

        updateInfoLabel.setText("Release x available");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
        updateInfoPanel.add(updateInfoLabel, gridBagConstraints);

        downloadButton.setText("Download");
        downloadButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                downloadButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 4, 0);
        updateInfoPanel.add(downloadButton, gridBagConstraints);

        jButton1.setText("Close");
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.insets = new java.awt.Insets(0, 2, 4, 4);
        updateInfoPanel.add(jButton1, gridBagConstraints);

        jLayeredPane1.setLayer(updateInfoPanel, javax.swing.JLayeredPane.MODAL_LAYER);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHEAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 32, 24);
        jLayeredPane1.add(updateInfoPanel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        getContentPane().add(jLayeredPane1, gridBagConstraints);

        fileMenu.setText("File");
        fileMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                fileMenuActionPerformed(evt);
            }
        });

        newModel.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_N, java.awt.event.InputEvent.CTRL_MASK));
        newModel.setText("New");
        newModel.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                newModelActionPerformed(evt);
            }
        });
        fileMenu.add(newModel);

        load.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_O, java.awt.event.InputEvent.CTRL_MASK));
        load.setText("Load");
        load.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                loadActionPerformed(evt);
            }
        });
        fileMenu.add(load);

        reload.setText("Reload");
        reload.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                reloadActionPerformed(evt);
            }
        });
        fileMenu.add(reload);
        fileMenu.add(jSeparator1);

        save.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_S, java.awt.event.InputEvent.CTRL_MASK));
        save.setText("Save");
        save.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                saveActionPerformed(evt);
            }
        });
        fileMenu.add(save);

        saveAs.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_A, java.awt.event.InputEvent.CTRL_MASK));
        saveAs.setText("Save as...");
        saveAs.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                saveAsActionPerformed(evt);
            }
        });
        fileMenu.add(saveAs);

        exportDisplay.setText("Export graph as image...");
        exportDisplay.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                exportDisplayActionPerformed(evt);
            }
        });
        fileMenu.add(exportDisplay);

        exportDisplay1.setText("Export graph as image map");
        exportDisplay1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                exportDisplay1ActionPerformed(evt);
            }
        });
        fileMenu.add(exportDisplay1);
        fileMenu.add(jSeparator2);

        connectDb.setText("Connect with database");
        connectDb.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                connectDbActionPerformed(evt);
            }
        });
        fileMenu.add(connectDb);

        disconnectDb.setText("Disconnect");
        disconnectDb.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                disconnectDbActionPerformed(evt);
            }
        });
        fileMenu.add(disconnectDb);
        fileMenu.add(jSeparator10);

        exit.setText("Exit");
        exit.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                exitActionPerformed(evt);
            }
        });
        fileMenu.add(exit);

        jMenuBar2.add(fileMenu);

        jMenu1.setText("Model");

        updateDataModel.setText("Analyze Database");
        updateDataModel.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                updateDataModelActionPerformed(evt);
            }
        });
        jMenu1.add(updateDataModel);

        openDataModelEditor.setLabel("Data Model Editor");
        openDataModelEditor.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                openDataModelEditorActionPerformed(evt);
            }
        });
        jMenu1.add(openDataModelEditor);
        jMenu1.add(jSeparator3);

        columnOrderItem.setText("Define Column Order");
        columnOrderItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                columnOrderItemActionPerformed(evt);
            }
        });
        jMenu1.add(columnOrderItem);
        jMenu1.add(jSeparator14);

        analyzeSQLMenuItem.setText("Analyze SQL Script");
        analyzeSQLMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                analyzeSQLMenuItemActionPerformed(evt);
            }
        });
        jMenu1.add(analyzeSQLMenuItem);
        jMenu1.add(jSeparator15);

        modelMigrationMenuItem.setText("Model Migration Tool");
        modelMigrationMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                modelMigrationMenuItemActionPerformed(evt);
            }
        });
        jMenu1.add(modelMigrationMenuItem);

        checkPKMenuItem.setText("Check Primary Keys");
        checkPKMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                checkPKMenuItemActionPerformed(evt);
            }
        });
        jMenu1.add(checkPKMenuItem);

        consistencyCheckMenuItem1.setText("Check Referential Consistency");
        consistencyCheckMenuItem1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                consistencyCheckMenuItem1ActionPerformed(evt);
            }
        });
        jMenu1.add(consistencyCheckMenuItem1);

        jMenuBar2.add(jMenu1);

        editMenu.setText("Edit");

        undoMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Z, java.awt.event.InputEvent.CTRL_MASK));
        undoMenuItem.setText("Undo");
        undoMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                undoMenuItemActionPerformed(evt);
            }
        });
        editMenu.add(undoMenuItem);

        redoMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Y, java.awt.event.InputEvent.CTRL_MASK));
        redoMenuItem.setText("Redo");
        redoMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                redoMenuItemActionPerformed(evt);
            }
        });
        editMenu.add(redoMenuItem);
        editMenu.add(jSeparator13);

        ignoreAll.setActionCommand("Disable all associations");
        ignoreAll.setLabel("Disable all associations");
        ignoreAll.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                ignoreAllActionPerformed(evt);
            }
        });
        editMenu.add(ignoreAll);

        removeAllRestrictions.setLabel("Remove all restrictions");
        removeAllRestrictions.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                removeAllRestrictionsActionPerformed(evt);
            }
        });
        editMenu.add(removeAllRestrictions);
        editMenu.add(jSeparator12);

        jMenuItem2.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_T, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItem2.setText("Filter editor...");
        jMenuItem2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem2ActionPerformed(evt);
            }
        });
        editMenu.add(jMenuItem2);

        jMenuBar2.add(editMenu);

        viewMenu.setText("View");

        collapseAll.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_L, java.awt.event.InputEvent.CTRL_MASK));
        collapseAll.setText("Collapse all");
        collapseAll.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                collapseAllActionPerformed(evt);
            }
        });
        viewMenu.add(collapseAll);

        expandAll.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_E, java.awt.event.InputEvent.CTRL_MASK));
        expandAll.setLabel("Expand all");
        expandAll.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                expandAllActionPerformed(evt);
            }
        });
        viewMenu.add(expandAll);

        expandAllVisible.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_W, java.awt.event.InputEvent.CTRL_MASK));
        expandAllVisible.setText("Expand visible tables");
        expandAllVisible.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                expandAllVisibleActionPerformed(evt);
            }
        });
        viewMenu.add(expandAllVisible);

        jMenuItem3.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItem3.setText("Fix all");
        jMenuItem3.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem3ActionPerformed(evt);
            }
        });
        viewMenu.add(jMenuItem3);

        jMenuItem4.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_U, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItem4.setText("Unfix all");
        jMenuItem4.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem4ActionPerformed(evt);
            }
        });
        viewMenu.add(jMenuItem4);

        refresh.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_R, java.awt.event.InputEvent.CTRL_MASK));
        refresh.setText("Reset");
        refresh.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                refreshActionPerformed(evt);
            }
        });
        viewMenu.add(refresh);

        zoomToFit.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_M, java.awt.event.InputEvent.CTRL_MASK));
        zoomToFit.setText("Zoom to fit");
        zoomToFit.setVerifyInputWhenFocusTarget(false);
        zoomToFit.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                zoomToFitActionPerformed(evt);
            }
        });
        viewMenu.add(zoomToFit);
        viewMenu.add(jSeparator4);

        showIgnored.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_H, java.awt.event.InputEvent.CTRL_MASK));
        showIgnored.setText("Show disabled associations");
        showIgnored.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                showIgnoredActionPerformed(evt);
            }
        });
        viewMenu.add(showIgnored);

        showTableDetails.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_D, java.awt.event.InputEvent.CTRL_MASK));
        showTableDetails.setText("Show table details");
        showTableDetails.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                showTableDetailsActionPerformed(evt);
            }
        });
        viewMenu.add(showTableDetails);

        jMenuBar2.add(viewMenu);

        jMenu3.setText("Tools");

        dataExport.setLabel("Export Data");
        dataExport.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                dataExportActionPerformed(evt);
            }
        });
        jMenu3.add(dataExport);

        dataImport.setLabel("Import SQL Data");
        dataImport.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                dataImportActionPerformed(evt);
            }
        });
        jMenu3.add(dataImport);
        jMenu3.add(jSeparator5);

        openDataBrowserItem.setText("Data Browser");
        openDataBrowserItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                openDataBrowserItemActionPerformed(evt);
            }
        });
        jMenu3.add(openDataBrowserItem);

        queryBuilder.setText("Query Builder");
        queryBuilder.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                queryBuilderActionPerformed(evt);
            }
        });
        jMenu3.add(queryBuilder);

        cycleView.setText("Cycle Finder");
        cycleView.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cycleViewActionPerformed(evt);
            }
        });
        jMenu3.add(cycleView);

        renderHtml.setText("HTML Renderer");
        renderHtml.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                renderHtmlActionPerformed(evt);
            }
        });
        jMenu3.add(renderHtml);

        renderHtml1.setText("HTML Renderer (with graphic)");
        renderHtml1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                renderHtml1ActionPerformed(evt);
            }
        });
        jMenu3.add(renderHtml1);

        consistencyCheckMenuItem.setText("Referential Consistency Check");
        consistencyCheckMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                consistencyCheckMenuItemActionPerformed(evt);
            }
        });
        jMenu3.add(consistencyCheckMenuItem);

        jMenuBar2.add(jMenu3);

        jMenu5.setText("Settings");

        horizontalLayoutMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_I, java.awt.event.InputEvent.CTRL_MASK));
        horizontalLayoutMenuItem.setSelected(true);
        horizontalLayoutMenuItem.setText("Horizontal layout");
        horizontalLayoutMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                horizontalLayoutMenuItemActionPerformed(evt);
            }
        });
        jMenu5.add(horizontalLayoutMenuItem);

        jMenu4.setText("Animation step time");

        steptimeGroup.add(steptime0);
        steptime0.setSelected(true);
        steptime0.setText("default");
        steptime0.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                steptime0ActionPerformed(evt);
            }
        });
        jMenu4.add(steptime0);

        steptimeGroup.add(steptime10);
        steptime10.setText("10");
        steptime10.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                steptime10ActionPerformed(evt);
            }
        });
        jMenu4.add(steptime10);

        steptimeGroup.add(steptime20);
        steptime20.setText("20");
        steptime20.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                steptime20ActionPerformed(evt);
            }
        });
        jMenu4.add(steptime20);

        steptimeGroup.add(steptime30);
        steptime30.setText("30");
        steptime30.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                steptime30ActionPerformed(evt);
            }
        });
        jMenu4.add(steptime30);

        steptimeGroup.add(steptime50);
        steptime50.setText("50");
        steptime50.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                steptime50ActionPerformed(evt);
            }
        });
        jMenu4.add(steptime50);

        steptimeGroup.add(steptime75);
        steptime75.setText("75");
        steptime75.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                steptime75ActionPerformed(evt);
            }
        });
        jMenu4.add(steptime75);

        steptimeGroup.add(steptime100);
        steptime100.setText("100");
        steptime100.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                steptime100ActionPerformed(evt);
            }
        });
        jMenu4.add(steptime100);

        steptimeGroup.add(steptime200);
        steptime200.setText("200");
        steptime200.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                steptime200ActionPerformed(evt);
            }
        });
        jMenu4.add(steptime200);

        steptimeGroup.add(steptime500);
        steptime500.setText("500");
        steptime500.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                steptime500ActionPerformed(evt);
            }
        });
        jMenu4.add(steptime500);

        jMenu5.add(jMenu4);
        jMenu5.add(jSeparator6);

        plafMenu.setText("Look and Feel");
        jMenu5.add(plafMenu);

        jMenuBar2.add(jMenu5);

        jMenu2.setText("Help");

        helpContent.setText("Manual");
        helpContent.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                helpContentActionPerformed(evt);
            }
        });
        jMenu2.add(helpContent);

        tutorial.setLabel("Tutorial");
        tutorial.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                tutorialActionPerformed(evt);
            }
        });
        jMenu2.add(tutorial);
        jMenu2.add(jSeparator7);

        helpForum.setText("Forum");
        helpForum.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                helpForumActionPerformed(evt);
            }
        });
        jMenu2.add(helpForum);

        downloadMenuItem.setText("Download Latest Version");
        downloadMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                downloadMenuItemActionPerformed(evt);
            }
        });
        jMenu2.add(downloadMenuItem);
        jMenu2.add(jSeparator8);

        jMenuItem1.setText("About Jailer");
        jMenuItem1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem1ActionPerformed(evt);
            }
        });
        jMenu2.add(jMenuItem1);

        jMenuBar2.add(jMenu2);

        setJMenuBar(jMenuBar2);

        pack();
    }// </editor-fold>//GEN-END:initComponents

	/**
	 * <code>true</code> if find-dialog was hidden due to lost focus.
	 */
//    private boolean findDialogWasVisible = false;

	private void formWindowLostFocus(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_formWindowLostFocus
//    	if (findDialog.isVisible()) {
//    		findDialogWasVisible = findDialog.isVisible();
//    		findDialog.setVisible(false);
//    	}
	}//GEN-LAST:event_formWindowLostFocus

	private void formWindowGainedFocus(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_formWindowGainedFocus
//    	if (findDialogWasVisible && !findDialog.isVisible()) {
//    		findDialogWasVisible = false;
//    		findDialog.setVisible(true);
//    	}
	}//GEN-LAST:event_formWindowGainedFocus

	private void queryBuilderActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_queryBuilderActionPerformed
		extractionModelEditor.graphView.openQueryBuilder(extractionModelEditor.root, true);
	}//GEN-LAST:event_queryBuilderActionPerformed

	private void expandAllVisibleActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_expandAllVisibleActionPerformed
		extractionModelEditor.expandAllVisibleTables();
	}//GEN-LAST:event_expandAllVisibleActionPerformed

	private void showTableDetailsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_showTableDetailsActionPerformed
		extractionModelEditor.graphView.updateTableDetailsMode();
	}//GEN-LAST:event_showTableDetailsActionPerformed

	private void helpForumActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_helpForumActionPerformed
		try {
			BrowserLauncher.openURL(new URI("https://sourceforge.net/p/jailer/discussion/"), this);
		} catch (Exception e) {
			UIUtil.showException(this, "Error", e);
		}
	}//GEN-LAST:event_helpForumActionPerformed

	private void zoomToFitActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_zoomToFitActionPerformed
		extractionModelEditor.zoomToFit();
	}//GEN-LAST:event_zoomToFitActionPerformed

	private void tutorialActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_tutorialActionPerformed
		try {
			BrowserLauncher.openURL(new URI("https://wisser.github.io/Jailer/exporting-data.htm"), this);
		} catch (Exception e) {
			UIUtil.showException(this, "Error", e);
		}
	}//GEN-LAST:event_tutorialActionPerformed

	private void helpContentActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_helpContentActionPerformed
		try {
			BrowserLauncher.openURL(new URI("https://wisser.github.io/Jailer/home.htm"), this);
		} catch (Exception e) {
			UIUtil.showException(this, "Error", e);
		}
	}//GEN-LAST:event_helpContentActionPerformed

	private void removeAllRestrictionsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_removeAllRestrictionsActionPerformed
		if (JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(this, "Remove all restrictions?", "Remove restrictions", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE)) {
			extractionModelEditor.removeAllRestrictions(null);
		}
	}//GEN-LAST:event_removeAllRestrictionsActionPerformed

	private void ignoreAllActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_ignoreAllActionPerformed
		if (JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(this, "Disable each association (except dependencies)?", "Add restrictions", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE)) {
			extractionModelEditor.ignoreAll(null);
		}
	}//GEN-LAST:event_ignoreAllActionPerformed

	private void dataImportActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_dataImportActionPerformed
		if (!UIUtil.canRunJailer()) {
			return;
		}
		try {
			String sqlFile = UIUtil.choseFile(null, ".", "Data Import", ".sql", this, false, true);
			if (sqlFile != null) {
				disconnect();
				if (connectToDBIfNeeded("Data Import")) {
					List<String> args = new ArrayList<String>();
					args.add("import");
					args.add(sqlFile);
					dbConnectionDialog.addDbArgs(args);
					ImportDialog importDialog = new ImportDialog(this, sqlFile, args, dbConnectionDialog.getUser(), dbConnectionDialog.getPassword(), true);
					if (importDialog.isOk) {
						disconnect();
						importDialog.fillCLIArgs(args);
						ResultConsumer consumer = new ResultConsumer() {
							@Override
							public void consume(boolean result, Throwable t) {
							}
							public void cleanUp() {
							}
						};
						UIUtil.runJailer(this, args, false, true,
						false, null, dbConnectionDialog.getUser(), dbConnectionDialog.getPassword(), null, null, false,
						true, false, false, false, consumer, null, executionContext);
					}
				}
			}
		} catch (Exception e) {
			UIUtil.showException(this, "Error", e);
		}
	}//GEN-LAST:event_dataImportActionPerformed

	/**
	 * Looks up "show disabled associations" setting.
	 *
	 * @return true if "show disabled associations" is set
	 */
	public boolean showDisabledAssociations() {
		return !hideIgnored();
	}

	/**
	 * Opens the closure view.
	 *
	 * @param tableToSelect the table to select initially or <code>null</code> to keep the current selection
	 */
	public void openClosureView(Table tableToSelect) {
//		closureView.refresh(tableToSelect);
//		closureView.setVisible(true);
//		closureView.toFront();
	}

	/**
	 * Opens the closure view.
	 *
	 * @param root the table to open initially
	 */
	public DataBrowser openDataBrowser(Table root, String condition) {
		if (dbConnectionDialog.isConnected || dbConnectionDialog.connect("Data Browser")) {
			updateMenuItems();
			DataBrowser dataBrowser;
			try {
				UIUtil.setWaitCursor(this);
				dataBrowser = new DataBrowser(extractionModelEditor.dataModel, root, condition, dbConnectionDialog, null, true, executionContext);
				if (dataBrowser.isReady()) {
					dataBrowser.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
					dataBrowser.setExtendedState(Frame.MAXIMIZED_BOTH);
					dataBrowser.setVisible(true);
				}
				return dataBrowser;
			} catch (Exception e) {
				UIUtil.showException(this, "Error", e);
			} finally {
				UIUtil.resetWaitCursor(this);
			}
		}
		return null;
	}

	private void openDataModelEditorActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_openDataModelEditorActionPerformed
		openDataModelEditor(null, false);
	}//GEN-LAST:event_openDataModelEditorActionPerformed

	/**
	 * Opens the data model editor.
	 */
	private void openDataModelEditor(final Table toEdit, final boolean merge) {
        executeAndReload(new Callable<Boolean>() {
			@Override
			public Boolean call() {
				try {
		        	UIUtil.setWaitCursor(ExtractionModelFrame.this);
					String modelname = extractionModelEditor.dataModel == null? DataModel.DEFAULT_NAME : extractionModelEditor.dataModel.getName();
					DataModelEditor dataModelEditor = new DataModelEditor(ExtractionModelFrame.this, merge, false, toEdit, null, null, modelname, null, dbConnectionDialog, executionContext);
					dataModelEditor.setVisible(true);
				} catch (Exception e) {
					UIUtil.showException(ExtractionModelFrame.this, "Error", e);
				} finally {
		        	UIUtil.resetWaitCursor(ExtractionModelFrame.this);
				}
				return true;
			}
        });
	}

	private void updateDataModelActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_updateDataModelActionPerformed
		if (!UIUtil.canRunJailer()) {
			return;
		}
		executeAndReload(new Callable<Boolean>() {
			@Override
			public Boolean call() {
				try {
					JDBCMetaDataBasedModelElementFinder.privilegedSessionProvider = new PrivilegedSessionProviderDialog.Provider(ExtractionModelFrame.this);
					if (connectToDBIfNeeded("Analyze Database")) {
						List<String> args = new ArrayList<String>();
						args.add("build-model-wo-merge");
						dbConnectionDialog.addDbArgs(args);

						AnalyseOptionsDialog analyseOptionsDialog = new AnalyseOptionsDialog(ExtractionModelFrame.this, extractionModelEditor.dataModel, executionContext);
						boolean[] isDefaultSchema = new boolean[1];
						if (analyseOptionsDialog.edit(dbConnectionDialog, isDefaultSchema, dbConnectionDialog.currentConnection.user)) {
							String schema = analyseOptionsDialog.getSelectedSchema();
							if (schema != null) {
								args.add("-schema");
								args.add(schema);
							}
							if (!isDefaultSchema[0]) {
								args.add("-qualifyNames");
							}
							analyseOptionsDialog.appendAnalyseCLIOptions(args);
							ModelBuilder.assocFilter = analyseOptionsDialog.getAssociationLineFilter();
							if (UIUtil.runJailer(ExtractionModelFrame.this, args, false, true, true, null, dbConnectionDialog.getUser(), dbConnectionDialog.getPassword(), null, null, false, true, false, null, executionContext)) {
								ModelBuilder.assocFilter = null;
								String modelname = extractionModelEditor.dataModel == null? DataModel.DEFAULT_NAME : extractionModelEditor.dataModel.getName();
								DataModelEditor dataModelEditor = new DataModelEditor(ExtractionModelFrame.this, true, analyseOptionsDialog.isRemoving(), null, analyseOptionsDialog.getTableLineFilter(), analyseOptionsDialog.getAssociationLineFilter(), modelname, schema == null? dbConnectionDialog.getName() : schema, dbConnectionDialog, executionContext);
								if (dataModelEditor.dataModelHasChanged()) {
									dataModelEditor.setVisible(true);
								}
							}
						}
					}
				} catch (Exception e) {
					UIUtil.showException(ExtractionModelFrame.this, "Error", e);
				} finally {
					ModelBuilder.assocFilter = null;
					JDBCMetaDataBasedModelElementFinder.privilegedSessionProvider = null;
				}
				return true;
			}
		});
	}//GEN-LAST:event_updateDataModelActionPerformed

	void dataExportActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_dataExportActionPerformed
		openExportDialog(true, null, null);
	}//GEN-LAST:event_dataExportActionPerformed

	private String createTempFileName() {
		String file;
		String ts = new SimpleDateFormat("HH-mm-ss-SSS", Locale.ENGLISH).format(new Date());
		File newFile;
		for (int i = 1; ; ++i) {
			file = Configuration.getInstance().getTempFileFolder();
			newFile = new File(file);
			newFile.mkdirs();
			file += File.separator + "em" + "-" + ts + (i > 1? "-" + Integer.toString(i) : "") + ".jm";
			newFile = new File(file);
			if (!newFile.exists()) {
				break;
			}
		}
		return file;
	}

	@SuppressWarnings("serial")
	public void openExportDialog(boolean checkRI, final Runnable onDataModelUpdate, final Runnable cleanup) {
		if (!UIUtil.canRunJailer()) {
			return;
		}
		
		final String tmpFileName;
		boolean isRunning = false;
		try {
			if (checkRI && extractionModelEditor.dataModel != null && !ScriptFormat.XML.equals(extractionModelEditor.scriptFormat)) {
				List<Association> restrictedDependencies = findRestrictedDependencies(extractionModelEditor.dataModel);
				if (!restrictedDependencies.isEmpty()) {
					Association restrictedDependency = restrictedDependencies.get(0);
					String more = "";
					if (restrictedDependencies.size() > 1) {
						more = "(" + (restrictedDependencies.size() - 1) + " more)";
					}
					switch (JOptionPane.showOptionDialog(this,
							"Dependency from '" + restrictedDependency.source.getName() + "' to '" + restrictedDependency.destination.getName() + "'\n" +
							"is restricted. " + more + "\nReferential integrity is not guaranteed!", "Restricted Dependency", JOptionPane.YES_NO_OPTION, JOptionPane.INFORMATION_MESSAGE, null, new Object[] { "Ok", "Cancel", "Show dependency" }, "Cancel")) {
					case 0: break;
					case 2:
						extractionModelEditor.closureView.selectTabComponent(extractionModelEditor.restrDepsView);
						extractionModelEditor.select(restrictedDependency);
						return;
					default: return;
					}
				}
			}
			if (extractionModelEditor.subject == null) {
				askForDataModel(this);
				return;
			}

			String tmpF = null;
//			if (extractionModelEditor.extractionModelFile == null || extractionModelEditor.needsSave) {
				tmpF = createTempFileName();
				if (!extractionModelEditor.save(tmpF)) {
					tmpF = null;
				} else {
					Table stable = extractionModelEditor.dataModel.getTableByDisplayName((String) extractionModelEditor.subjectTable.getSelectedItem());
					if (stable == null) {
						throw new IllegalStateException("No \"Export from\" table specified. (" + extractionModelEditor.dataModel.getTables().size() + " tables total)");
					}
				}
//			}
			tmpFileName = tmpF;
			if (tmpFileName != null || saveIfNeeded("Export data", false, true)) {
				if (tmpFileName != null || (extractionModelEditor.extractionModelFile != null || extractionModelEditor.save(true, "Export data"))) {
					if (connectToDBIfNeeded("Export data")) {
						List<String> args = new ArrayList<String>();
						args.add(tmpFileName != null? tmpFileName : extractionModelEditor.extractionModelFile != null? new File(extractionModelEditor.extractionModelFile).getAbsolutePath() : null);
						dbConnectionDialog.addDbArgs(args);
						BasicDataSource dataSource = new BasicDataSource(dbConnectionDialog.currentConnection.driverClass, dbConnectionDialog.currentConnection.url, dbConnectionDialog.currentConnection.user, dbConnectionDialog.getPassword(), 0, dbConnectionDialog.currentJarURLs());
						if (theSession != null) {
							theSession.shutDown();
							theSession = null;
						}
			    		Session session = SessionForUI.createSession(dataSource, dataSource.dbms, executionContext.getIsolationLevel(), true, false, this);

						final Set<Table> toCheck = new HashSet<Table>();
						if (session != null) {
							final ExportDialog exportDialog;
							String jmFile;
							try {
								Session.setGlobalFallbackConnection(session.getConnection());
								if (extractionModelEditor.dataModel != null) {
									if (extractionModelEditor.extractionModel != null) {
										if (extractionModelEditor.extractionModel.additionalSubjects != null) {
											for (AdditionalSubject as: extractionModelEditor.extractionModel.additionalSubjects) {
												toCheck.add(as.getSubject());
											}
										}
									}
									toCheck.add(extractionModelEditor.subject);
									extractionModelEditor.dataModel.checkForPrimaryKey(new HashSet<Table>(toCheck), session.dbms.getRowidName() != null);
								}

								jmFile = extractionModelEditor.extractionModelFile != null? extractionModelEditor.extractionModelFile : tmpFileName;

								exportDialog = new ExportDialog(this, extractionModelEditor.dataModel, extractionModelEditor.getSubject(), extractionModelEditor.getSubjectCondition(), extractionModelEditor.extractionModel.additionalSubjects, session, args, dbConnectionDialog.getUser(), dbConnectionDialog.getPassword(), checkRI, dbConnectionDialog, extractionModelEditor.extractionModelFile, jmFile, tmpFileName, defaultExportFileName, executionContext) {
									@Override
									protected boolean checkForPKs(JRadioButton rowidButton, Runnable saveSettings) {
										try {
											extractionModelEditor.dataModel.checkForPrimaryKey(new HashSet<Table>(toCheck), false);
										} catch (Exception e) {
											if (e instanceof DataModel.NoPrimaryKeyException) {
												int result = JOptionPane.showOptionDialog(this, e.getMessage(), "No Primary Key", JOptionPane.YES_NO_OPTION, JOptionPane.ERROR_MESSAGE, null,
														rowidButton == null?
																new Object[] { "Edit Table", "Cancel" }
																:
																new Object[] { "Edit Table", rowidButton.getText().replace("<html>", "<html>Use row identification: \"").replace("</html>", "\"</html>"), "Cancel" },
																null);
												if (result == 0) {
													if (saveSettings != null) {
														saveSettings.run();
													}
													dispose();
													setVisible(false);
													openDataModelEditor(((NoPrimaryKeyException) e).table == null? extractionModelEditor.subject : ((NoPrimaryKeyException) e).table, false);
													if (onDataModelUpdate != null) {
														onDataModelUpdate.run();
													}
													UIUtil.invokeLater(() -> openExportDialog(checkRI, onDataModelUpdate, cleanup));
												} else if (result == 1 && rowidButton != null) {
													rowidButton.setSelected(true);
													return true;
												}
											} else {
												UIUtil.showException(this, "Error", e);
											}
											return false;
										}
										return true;
									}
								};
							} finally {
								Session.setGlobalFallbackConnection(null);
							}
							session.shutDown();
							if (exportDialog.isOk()) {
								exportDialog.fillCLIArgs(args);
								final List<String> ddlArgs = new ArrayList<String>();
								ddlArgs.add("create-ddl");
								dbConnectionDialog.addDbArgs(ddlArgs);
								ddlArgs.add(tmpFileName != null? tmpFileName : jmFile);
								++UISettings.s6;
								if (exportDialog.isIndependentWorkingTablesSelected()) {
									ddlArgs.add("-independent-working-tables");
									String delFile = exportDialog.getDeleteFileName();
									if (delFile != null) {
										ddlArgs.add("-d");
										ddlArgs.add(delFile);
									}
								}
								if (exportDialog.isUseRowId()) {
									ddlArgs.add("-use-rowid");
								}
								if (exportDialog.isUseRowIdsOnlyForTablesWithoutPK()) {
									ddlArgs.add("-use-rowid-if-needed");
								}
								if (exportDialog.getWorkingTableSchema() != null) {
									ddlArgs.add("-working-table-schema");
									ddlArgs.add(exportDialog.getWorkingTableSchema());
								}
								DMLTransformer.numberOfExportedLOBs = 0;

								ExecutionContext cDDLExecutionContext = new ExecutionContext(executionContext);
								cDDLExecutionContext.setIndependentWorkingTables(exportDialog.isIndependentWorkingTablesSelected());
								cDDLExecutionContext.setUseRowid(exportDialog.isUseRowId());
								cDDLExecutionContext.setUseRowIdsOnlyForTablesWithoutPK(exportDialog.isUseRowIdsOnlyForTablesWithoutPK());

								DDLCreator ddlCreator = new DDLCreator(cDDLExecutionContext);
								if (!cDDLExecutionContext.isIndependentWorkingTables()) {
									PrimaryKeyFactory.createUPKScope(tmpFileName != null? tmpFileName : jmFile, cDDLExecutionContext);
								}

								dataSource = new BasicDataSource(ddlArgs.get(1), ddlArgs.get(2), ddlArgs.get(3), ddlArgs.get(4), 0, dbConnectionDialog.currentJarURLs());
								String tableInConflict = exportDialog.getTemporaryTableScope().equals(WorkingTableScope.GLOBAL)? UIUtil.getDDLTableInConflict(ddlCreator, ExtractionModelFrame.this, dataSource, dataSource.dbms) : null;
								if (tableInConflict != null && exportDialog.getTemporaryTableScope().equals(WorkingTableScope.GLOBAL)) {
									JOptionPane.showMessageDialog(this, "Can't drop table '" + tableInConflict + "' as it is not created by Jailer.\nDrop or rename this table first.", "Error", JOptionPane.ERROR_MESSAGE);
								} else {
									Consumer<Window> openResult = new Consumer<Window>() {
										@Override
										public void accept(Window window) {
											String delFile = exportDialog.getDeleteFileName();
											if (delFile != null) {
												try {
													UIUtil.setWaitCursor(window);
													if (resultFileLoader == null || !resultFileLoader.apply(delFile)) {
														new FileView(ExtractionModelFrame.this, window, delFile, true);
													}
												} catch (Exception e) {
													UIUtil.showException(window, "Error", e);
												} finally {
													UIUtil.resetWaitCursor(window);
												}
											}
											String file = exportDialog.getInsertFileName();
											if (file != null) {
												file = Environment.newFile(file).getPath();
												try {
													UIUtil.setWaitCursor(window);
													if (resultFileLoader == null || !resultFileLoader.apply(file)) {
														new FileView(ExtractionModelFrame.this, window, file, true);
													}
												} catch (Exception e) {
													UIUtil.showException(window, "Error", e);
												} finally {
													UIUtil.resetWaitCursor(window);
												}
											}
										}
									};
									if (!exportDialog.getTemporaryTableScope().equals(WorkingTableScope.GLOBAL) || UIUtil.isDDLUptodate(ddlCreator, ExtractionModelFrame.this, dataSource, dataSource.dbms, exportDialog.isUseRowId(), exportDialog.isUseRowIdsOnlyForTablesWithoutPK(), exportDialog.getWorkingTableSchema()) || UIUtil.runJailer(this, ddlArgs, true, true, true,
										"Automatic creation of working-tables failed!\n" +
										"Please execute the Jailer-DDL manually (jailer_ddl.sql),\n" +
										"try another \"Working table schema\"," +
										"or use the Working table scope \"local database\"\n\n" +
										"Continue Data Export?", dbConnectionDialog.getUser(), dbConnectionDialog.getPassword(), null, null, true, false, true, null, executionContext)) {
										ProgressTable progressTable = new ProgressTable();
										ProgressTable progressTableForDelete = new ProgressTable();
										final ProgressPanel progressPanel = new ProgressPanel(progressTable, progressTableForDelete, exportDialog.hasDeleteScript());
										boolean confirm = exportDialog.scriptFormat == ScriptFormat.INTRA_DATABASE && exportDialog.getConfirmExport();
										final ExportAndDeleteStageProgressListener progressListener = new ExportAndDeleteStageProgressListener(progressTable, progressTableForDelete, progressPanel, extractionModelEditor.dataModel, confirm, exportDialog.getTargetSchemaSet(), exportDialog.scriptFormat != ScriptFormat.XML && !exportDialog.insertScripFileNameFieldIsEmpty()) {
											@Override
											protected void validatePrimaryKeys() {
												try {
													UIUtil.validatePrimaryKeys(SwingUtilities.getWindowAncestor(progressPanel), UIUtil.createBasicDataSource(ExtractionModelFrame.this, ddlArgs.get(1), ddlArgs.get(2), ddlArgs.get(3), ddlArgs.get(4), 0, dbConnectionDialog.currentJarURLs()), new TreeSet<Table>(extractionModelEditor.dataModel.getTables()));
												} catch (Exception e) {
													UIUtil.showException(ExtractionModelFrame.this, "Error", e);
												}
											}
										};

										ResultConsumer consumer = new ResultConsumer() {
											@Override
											public void consume(boolean result, Throwable e) {
												progressListener.stop();
												exportDialog.dispose();
												if (cleanup != null) {
													cleanup.run();
												}
												if (tmpFileName != null) {
													new File(tmpFileName).delete();
												}
											}
											public void cleanUp() {
											}										};
										UIUtil.runJailer(this, args, true, true,
											false, null, dbConnectionDialog.getUser(), dbConnectionDialog.getPassword(), progressListener, progressPanel, true,
											true, false, false, false, consumer, openResult, executionContext);
										isRunning = true;
									}
								}
								try {
									if (!isRunning) {
										exportDialog.dispose();
									}
								} catch (Throwable t) {
									// ignore
								}
							}
						}
					}
				}
			}
		} catch (Exception e) {
			if (e instanceof DataModel.NoPrimaryKeyException) {
				if (JOptionPane.showOptionDialog(this, e.getMessage(), "No Primary Key", JOptionPane.YES_NO_OPTION, JOptionPane.ERROR_MESSAGE, null, new Object[] { "Edit Table", "Cancel" }, null) == 0) {
					openDataModelEditor(((NoPrimaryKeyException) e).table == null? extractionModelEditor.subject : ((NoPrimaryKeyException) e).table, false);
					if (onDataModelUpdate != null) {
						onDataModelUpdate.run();
					}
				}
			} else {
				UIUtil.showException(this, "Error", e);
			}
		}
	}

	private StringBuilder defaultExportFileName;
	
	public void setDefaultExportFileName(String defaultExportFileName) {
		this.defaultExportFileName = new StringBuilder(defaultExportFileName);
	}

	private Function<String, Boolean> resultFileLoader;
	
	public void setResultFileLoader(Function<String, Boolean> resultFileLoader) {
		this.resultFileLoader = resultFileLoader;
	}

	/**
	 * Finds restricted dependencies.
	 */
	private List<Association> findRestrictedDependencies(DataModel dataModel) {
		Set<Table> closure = extractionModelEditor.getCurrentSubjectClosure();
		List<Association> result = new ArrayList<Association>();
		for (Association association: dataModel.namedAssociations.values()) {
			if (association.isInsertDestinationBeforeSource() && association.isRestricted()) {
				if (closure.contains(association.source)) {
					if (!association.fkHasNullFilter()) {
						result.add(association);
					}
				}
			}
		}
		return result;
	}

	private void disconnectDbActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_disconnectDbActionPerformed
		disconnect();
	}//GEN-LAST:event_disconnectDbActionPerformed

	private void disconnect() {
		if (theSession != null) {
			theSession.shutDown();
			theSession = null;
		}
		dbConnectionDialog.isConnected = false;
		updateMenuItems();
	}

	private void connectDbActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_connectDbActionPerformed
		disconnect();
		connectToDBIfNeeded(null);
	}//GEN-LAST:event_connectDbActionPerformed

	private void renderHtmlActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_renderHtmlActionPerformed
		openHTMLRender(null);
	}//GEN-LAST:event_renderHtmlActionPerformed

	/**
	 * Opens browser to render a given table.
	 *
	 * @param table the table
	 */
	public void openHTMLRender(Table table) {
		try {
			List<String> args = new ArrayList<String>();
			args.add("render-datamodel");
			File file = saveRestrictions();
			args.add(file.getAbsolutePath());
			if (UIUtil.canRunJailer()) {
				UIUtil.runJailer(this, args, false, true, true, null, null, null /* dbConnectionDialog.getPassword() */, null, null, false, true, false, null, executionContext);
				HtmlDataModelRenderer renderer = Configuration.getInstance().getRenderer();
				String of = renderer.outputFolderOf(extractionModelEditor.dataModel);
				BrowserLauncher.openURL(Environment.newFile(table == null? (of + "/index.html") : (of + "/" + HtmlDataModelRenderer.CONTENT_FOLDER_NAME + "/" + HtmlDataModelRenderer.toFileName(table))).toURI(), this);
			}
		} catch (Exception e) {
			UIUtil.showException(this, "Error", e);
		}
	}

	/**
	 * Shows shortest path between two given table.
	 */
	public void showShortestPath(Table from, Table to) {
		if (from != null && to != null) {
			try {
				List<String> args = new ArrayList<String>();
				args.add("find-association");
				args.add(from.getName());
				args.add(to.getName());
				File file = saveRestrictions();
				args.add(file.getAbsolutePath());
				UIUtil.runJailer(this, args, false, false, false, null, dbConnectionDialog.getUser(), dbConnectionDialog.getPassword(), null, null, false, true, false, null, executionContext);
			} catch (Exception e) {
				UIUtil.showException(this, "Error", e);
			}
		}
	}

	/**
	 * Saves restrictions of current extraction model.
	 *
	 * @return restrictions file
	 */
	private File saveRestrictions() throws Exception {
		String tempFileFolder = Configuration.getInstance().getTempFileFolder();
		new File(tempFileFolder).mkdirs();
		File file = new File(tempFileFolder, "NewModel");
		String extractionModelFile = extractionModelEditor.extractionModelFile;
		if (extractionModelFile != null) {
			extractionModelFile = new File(extractionModelFile).getName();
			if (extractionModelFile.toLowerCase(Locale.ENGLISH).endsWith(".jm")) {
				file = new File(tempFileFolder, extractionModelFile.substring(0, extractionModelFile.length() - 3) + "-restrictions.jm");
			} else {
				file = new File(tempFileFolder, extractionModelFile + "-restrictions.jm");
			}
		}
		extractionModelEditor.saveRestrictions(file);
		return file;
	}

	/**
	 * Opens connection dialog to establish DB-connection.
	 *
	 * @return <code>false</code> if connection fails
	 */
	boolean connectToDBIfNeeded(String reason) {
		try {
			if (!dbConnectionDialog.isConnected) {
				if (theSession != null) {
					theSession.shutDown();
					theSession = null;
				}
				return dbConnectionDialog.connect(reason);
			}
			return true;
		} finally {
			updateMenuItems();
		}
	}

	/**
	 * Sets a clone of a given {@link DbConnectionDialog}.
	 */
	public void setDbConnectionDialogClone(DbConnectionDialog dbConnectionDialog) {
		try {
			this.dbConnectionDialog = new DbConnectionDialog(this, dbConnectionDialog, JailerVersion.APPLICATION_NAME, executionContext);
		} finally {
			updateMenuItems();
		}
	}

	private void jMenuItem1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem1ActionPerformed
		About about = new About(this, true);
		about.setTitle("Jailer " + JailerVersion.VERSION);
		about.pack();
		about.setSize(Math.min(about.getWidth(), 600), about.getHeight());
		about.setLocation(getLocation().x + (getSize().width - about.getSize().width) / 2, getLocation().y + (getSize().height - about.getSize().height) / 2);
		about.setVisible(true);
	}//GEN-LAST:event_jMenuItem1ActionPerformed

	private void loadActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_loadActionPerformed
		try {
			String modelFile = UIUtil.choseFile(null, "extractionmodel", "Load Extraction Model", ".jm", this, false, true, false);
			if (modelFile != null && UIUtil.checkFileExistsAndWarn(modelFile, this)) {
				try {
					UIUtil.setWaitCursor(this);
					if (modelFile != null && !toFront(modelFile)) {
						String currentModelSubfolder = DataModelManager.getCurrentModelSubfolder(executionContext);
						if (extractionModelEditor.extractionModelFile == null && !extractionModelEditor.needsSave
								&& currentModelSubfolder != null && currentModelSubfolder.equals(ExtractionModel.loadDatamodelFolder(modelFile, executionContext))) {
							load(modelFile);
						} else {
							createFrame(modelFile, false, "S", executionContext);
						}
					}
				} finally {
					UIUtil.resetWaitCursor(this);
				}
			}
		} catch (Throwable t) {
			UIUtil.showException(this, "Error", t);
		}
	}//GEN-LAST:event_loadActionPerformed

	private boolean toFront(String modelFile) {
		for (Frame frame: Frame.getFrames()) {
			if (frame instanceof ExtractionModelFrame && frame.isVisible()) {
				ExtractionModelEditor em = ((ExtractionModelFrame) frame).extractionModelEditor;
				if (em != null && em.extractionModelFile != null && em.extractionModelFile.equals(modelFile)) {
					frame.toFront();
					return true;
				}
			}
		}
		return false;
	}

	/**
	 * Loads an extraction model.
	 *
	 * @param modelFile name of model file
	 */
	private void load(String modelFile) {
		try {
			String dmf = ExtractionModel.loadDatamodelFolder(modelFile, executionContext);
			if (dmf == null && DataModelManager.getCurrentModelSubfolder(executionContext) != null
				||
				dmf != null && !dmf.equals(DataModelManager.getCurrentModelSubfolder(executionContext))) {
				JOptionPane.showMessageDialog(this, "Unable to load \"" + new File(modelFile).getName() + "\"\nExtraction model is assigned to data model \"" + DataModelManager.getModelDetails(dmf, executionContext).a + "\"", "Wrong Data Model", JOptionPane.ERROR_MESSAGE);
				return;
			}
			UIUtil.setWaitCursor(this);
			ExtractionModelEditor newModelEditor = new ExtractionModelEditor(modelFile, this, isHorizontalLayout, getConnectivityState(), getConnectivityStateToolTip(), getDBMSLogo(), executionContext);
			extractionModelEditor.extractionModelFrame = null;
			editorPanel.remove(extractionModelEditor);
			extractionModelEditor = null;
			editorPanel.add(extractionModelEditor = newModelEditor, "editor");
			((CardLayout) editorPanel.getLayout()).show(editorPanel, "editor");
			validate();
			extractionModelEditor.closureBorderView.refresh();
			restrictedDependenciesView.refresh();
			updateTitle(extractionModelEditor.needsSave);
		} catch (Throwable t) {
			UIUtil.showException(this, "Error", t);
		} finally {
			UIUtil.resetWaitCursor(this);
		}
	}

	private void newModelActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_newModelActionPerformed
		try {
		    UIUtil.setWaitCursor(this);
			createFrame(null, false, "S", executionContext);
		} catch (Throwable t) {
			UIUtil.showException(this, "Error", t);
		} finally {
            UIUtil.resetWaitCursor(this);
		}
	}//GEN-LAST:event_newModelActionPerformed

	private void reload() {
		try {
			UIUtil.setWaitCursor(this);
			ExtractionModelEditor newModelEditor = new ExtractionModelEditor(extractionModelEditor.extractionModelFile, this, isHorizontalLayout, getConnectivityState(), getConnectivityStateToolTip(), getDBMSLogo(), executionContext);
			extractionModelEditor.extractionModelFrame = null;
			editorPanel.remove(extractionModelEditor);
			editorPanel.add(extractionModelEditor = newModelEditor, "editor");
			((CardLayout) editorPanel.getLayout()).show(editorPanel, "editor");
			validate();
			extractionModelEditor.closureBorderView.refresh();
			restrictedDependenciesView.refresh();
			updateTitle(extractionModelEditor.needsSave);
		} catch (Throwable t) {
			UIUtil.showException(this, "Error", t);
		} finally {
			UIUtil.resetWaitCursor(this);
		}
	}

	private void expandAllActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_expandAllActionPerformed
		expandAll();
	}//GEN-LAST:event_expandAllActionPerformed

	public void expandAll() {
		extractionModelEditor.expand();
	}

	private void showIgnoredActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_showIgnoredActionPerformed
//      extractionModelEditor.refresh(true, false);
		extractionModelEditor.resetGraphEditor(false, true, true, true);
	}//GEN-LAST:event_showIgnoredActionPerformed

	private void collapseAllActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_collapseAllActionPerformed
		extractionModelEditor.captureLayout();
		try {
			executionContext.getLayoutStorage().enabled = false;
			extractionModelEditor.refresh(false, true, false, true);
			extractionModelEditor.resetGraphEditor(true, false, true, true);
		} finally {
			executionContext.getLayoutStorage().enabled = true;
			extractionModelEditor.checkLayoutStack();
		}
	}//GEN-LAST:event_collapseAllActionPerformed

	private void refreshActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_refreshActionPerformed
		extractionModelEditor.captureLayout();
		try {
			extractionModelEditor.refresh(false, true, false, false);
		} finally {
			extractionModelEditor.checkLayoutStack();
		}
	}//GEN-LAST:event_refreshActionPerformed

	private void saveAsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_saveAsActionPerformed
		extractionModelEditor.save(true, null);
	}//GEN-LAST:event_saveAsActionPerformed

	private void saveActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_saveActionPerformed
		extractionModelEditor.save(false, null);
	}//GEN-LAST:event_saveActionPerformed

	private void formWindowClosing(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_formWindowClosing
		onExit();
	}//GEN-LAST:event_formWindowClosing

	private void exitActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_exitActionPerformed
		onExit();
	}//GEN-LAST:event_exitActionPerformed

	/**
	 * Saves model if needed.
	 *
	 * @return <code>false</code> if user cancels saving
	 */
	private boolean saveIfNeeded(String cause, boolean ask, boolean withNoOption) {
		if (!extractionModelEditor.needsSave) {
			return true;
		}
		if (ask) {
			int option = JOptionPane.showConfirmDialog(this, "Save changes before " + cause + "?", "", withNoOption? JOptionPane.YES_NO_CANCEL_OPTION : JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
			if (option == JOptionPane.CANCEL_OPTION) {
				return false;
			}
			if (option == JOptionPane.NO_OPTION) {
				return withNoOption;
			}
		}
		return extractionModelEditor.save(false, cause);
	}

	/**
	 * Exits GUI.
	 */
	private void onExit() {
		if (extractionModelEditor.needsSave) {
			if ((getExtendedState() & JFrame.ICONIFIED) != 0) {
				setExtendedState(getExtendedState() & ~JFrame.ICONIFIED);
			}
			Object[] options = { "Yes", "No" };
			if (0 == JOptionPane.showOptionDialog(
					this,
					"Close without saving?",
					"Closing",
					JOptionPane.YES_NO_OPTION, 
					JOptionPane.WARNING_MESSAGE, 
					null, options, options[1])) {
				storeLastSession();
				dispose();
				UIUtil.checkTermination();
			}
		} else {
			storeLastSession();
			dispose();
			UIUtil.checkTermination();
		}
	}

	private void fileMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_fileMenuActionPerformed
	}//GEN-LAST:event_fileMenuActionPerformed

	private void jMenuItem3ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem3ActionPerformed
		extractionModelEditor.graphView.setFix(true);
	}//GEN-LAST:event_jMenuItem3ActionPerformed

	private void jMenuItem4ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem4ActionPerformed
		extractionModelEditor.graphView.setFix(false);
	}//GEN-LAST:event_jMenuItem4ActionPerformed

	public int animationStepTime = 0;

	/**
	 * Initializes the "step time" menu items.
	 */
	private void initAnimationSteptime() {
		try {
			BufferedReader in = new BufferedReader(new FileReader(".steptime"));
			try {
				animationStepTime = Integer.parseInt(in.readLine());
			} catch (Exception e) {
			}
			in.close();
		} catch (FileNotFoundException e) {
		} catch (IOException e) {
		}
		switch (animationStepTime) {
		case 0: steptime0.setSelected(true); break;
		case 10: steptime10.setSelected(true); break;
		case 20: steptime20.setSelected(true); break;
		case 30: steptime30.setSelected(true); break;
		case 50: steptime50.setSelected(true); break;
		case 75: steptime75.setSelected(true); break;
		case 100: steptime100.setSelected(true); break;
		case 200: steptime200.setSelected(true); break;
		case 500: steptime500.setSelected(true); break;
		}
	}

	private void setAnimationSteptime(int steptime) {
		animationStepTime = steptime;
		FileWriter out;
		try {
			out = new FileWriter(".steptime");
			out.write("" + animationStepTime);
			out.close();
		} catch (IOException e) {
		}
		extractionModelEditor.refresh(false, true, true, true);
	}

	private void steptime0ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_steptime0ActionPerformed
		setAnimationSteptime(0);
	}//GEN-LAST:event_steptime0ActionPerformed

	private void steptime10ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_steptime10ActionPerformed
		setAnimationSteptime(10);
	}//GEN-LAST:event_steptime10ActionPerformed

	private void steptime20ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_steptime20ActionPerformed
		setAnimationSteptime(20);
	}//GEN-LAST:event_steptime20ActionPerformed

	private void steptime30ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_steptime30ActionPerformed
		setAnimationSteptime(30);
	}//GEN-LAST:event_steptime30ActionPerformed

	private void steptime50ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_steptime50ActionPerformed
		setAnimationSteptime(50);
	}//GEN-LAST:event_steptime50ActionPerformed

	private void steptime75ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_steptime75ActionPerformed
		setAnimationSteptime(75);
	}//GEN-LAST:event_steptime75ActionPerformed

	private void steptime100ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_steptime100ActionPerformed
		setAnimationSteptime(100);
	}//GEN-LAST:event_steptime100ActionPerformed

	private void steptime200ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_steptime200ActionPerformed
		setAnimationSteptime(200);
	}//GEN-LAST:event_steptime200ActionPerformed

	private void steptime500ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_steptime500ActionPerformed
		setAnimationSteptime(500);
	}//GEN-LAST:event_steptime500ActionPerformed

	private void jMenuItem2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem2ActionPerformed
		openFilterEditor(extractionModelEditor.root);
	}//GEN-LAST:event_jMenuItem2ActionPerformed

	private void exportDisplayActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_exportDisplayActionPerformed
		try {
			extractionModelEditor.graphView.exportDisplayToImage(null, null);
		} catch (Throwable e) {
			UIUtil.showException(this, "Error", e);
		}
	}//GEN-LAST:event_exportDisplayActionPerformed

	private void horizontalLayoutMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_horizontalLayoutMenuItemActionPerformed
		isHorizontalLayout = !isHorizontalLayout;
		extractionModelEditor.setOrientation(isHorizontalLayout);
		horizontalLayoutMenuItem.setSelected(isHorizontalLayout);
		jMenuBar2.grabFocus();
	}//GEN-LAST:event_horizontalLayoutMenuItemActionPerformed

	private void cycleViewActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cycleViewActionPerformed
		cycleViewDialog.refresh();
//		cycleViewDialog.setVisible(true);
		cycleViewDialog.toFront();
		cycleViewDialog.findCycles();
	}//GEN-LAST:event_cycleViewActionPerformed

	private void openDataBrowserItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_openDataBrowserItemActionPerformed
		openDataBrowser(extractionModelEditor.root, "");
	}//GEN-LAST:event_openDataBrowserItemActionPerformed

	private void closureToolMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_closureToolMenuItemActionPerformed
		openClosureView(extractionModelEditor.root);
	}//GEN-LAST:event_closureToolMenuItemActionPerformed

	private void closureBorderToolMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_closureBorderToolMenuItemActionPerformed
		extractionModelEditor.closureBorderView.setVisible(true);
		extractionModelEditor.closureBorderView.toFront();
	}//GEN-LAST:event_closureBorderToolMenuItemActionPerformed

    private void reloadActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_reloadActionPerformed
    	try {
    		if (extractionModelEditor.extractionModelFile == null) {
    			return;
    		}
        	if (extractionModelEditor.needsSave) {
    			int option = JOptionPane.showConfirmDialog(this, "Are you sure you want to reload the current modell and lose the changes?", "", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
    			if (option != JOptionPane.YES_OPTION) {
    				return;
    			}
        	}
        	load(extractionModelEditor.extractionModelFile);
		} catch (Throwable t) {
			UIUtil.showException(this, "Error", t);
		}
    }//GEN-LAST:event_reloadActionPerformed

    private void analyzeSQLMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_analyzeSQLMenuItemActionPerformed
    	AssociationProposerView assocProposerView = new AssociationProposerView(this, extractionModelEditor.dataModel, null, 4, executionContext);
    	if (assocProposerView.isAccepted()) {
    		openDataModelEditor(null, true);
    	}
    }//GEN-LAST:event_analyzeSQLMenuItemActionPerformed

    private void columnOrderItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_columnOrderItemActionPerformed
        executeAndReload(new Callable<Boolean>() {
			@Override
			public Boolean call() throws Exception {
	        	return new ColumnOrderEditor(ExtractionModelFrame.this, null, extractionModelEditor.dataModel, executionContext).wasOk();
			}
        });
    }//GEN-LAST:event_columnOrderItemActionPerformed

    private void downloadButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_downloadButtonActionPerformed
        updateInfoPanel.setVisible(false);
        UpdateInfoManager.download();
    }//GEN-LAST:event_downloadButtonActionPerformed

    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
        updateInfoPanel.setVisible(false);
    }//GEN-LAST:event_jButton1ActionPerformed

    private void downloadMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_downloadMenuItemActionPerformed
    	UpdateInfoManager.download();
    }//GEN-LAST:event_downloadMenuItemActionPerformed

    private void modelMigrationMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_modelMigrationMenuItemActionPerformed
        extractionModelEditor.openPendingDecisionsEditor();
    }//GEN-LAST:event_modelMigrationMenuItemActionPerformed

    private void undoMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_undoMenuItemActionPerformed
        extractionModelEditor.undoChange();
    }//GEN-LAST:event_undoMenuItemActionPerformed

    private void redoMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_redoMenuItemActionPerformed
        extractionModelEditor.redoChange();
    }//GEN-LAST:event_redoMenuItemActionPerformed

    private void checkPKMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_checkPKMenuItemActionPerformed
    	try {
    		if (dbConnectionDialog.isConnected || dbConnectionDialog.connect("Check Primary Keys")) {
    			updateMenuItems();
    			BasicDataSource dataSource = UIUtil.createBasicDataSource(this, dbConnectionDialog.currentConnection.driverClass, dbConnectionDialog.currentConnection.url, dbConnectionDialog.currentConnection.user, dbConnectionDialog.getPassword(), 0, dbConnectionDialog.currentJarURLs());
    			UIUtil.validatePrimaryKeys(this, dataSource, new TreeSet<Table>(extractionModelEditor.dataModel.getTables()));
    		}
		} catch (Exception e) {
			// ignore
		}
    }//GEN-LAST:event_checkPKMenuItemActionPerformed

    @SuppressWarnings("serial")
	private void consistencyCheckMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_consistencyCheckMenuItemActionPerformed
    	try {
	    	if (dbConnectionDialog.isConnected || dbConnectionDialog.connect("Referential Consistency Check")) {
	    		BasicDataSource dataSource = new BasicDataSource(dbConnectionDialog.currentConnection.driverClass, dbConnectionDialog.currentConnection.url, dbConnectionDialog.currentConnection.user, dbConnectionDialog.getPassword(), 0, dbConnectionDialog.currentJarURLs());
				if (theSession != null) {
					theSession.shutDown();
					theSession = null;
				}
	    		Session session = SessionForUI.createSession(dataSource, dataSource.dbms, executionContext.getIsolationLevel(), false, false, this);
				if (session != null) {
					try {
						new ConstraintChecker(this, extractionModelEditor.dataModel, false, session) {
				            @Override
				            protected void openTableBrowser(Table source, String where) {
				            }
				        };
					} finally {
						session.shutDown();
					}
				}
	    	}
    	} catch (Exception e) {
    		UIUtil.showException(this, "Error", e);
    	}
    }//GEN-LAST:event_consistencyCheckMenuItemActionPerformed

    private void consistencyCheckMenuItem1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_consistencyCheckMenuItem1ActionPerformed
    	consistencyCheckMenuItemActionPerformed(evt);
    }//GEN-LAST:event_consistencyCheckMenuItem1ActionPerformed

    private void exportDisplay1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_exportDisplay1ActionPerformed
    	HtmlDataModelRenderer renderer = Configuration.getInstance().getRenderer();
		try {
    		File overviewImg = new File(new File(renderer.outputFolderOf(extractionModelEditor.dataModel), HtmlDataModelRenderer.CONTENT_FOLDER_NAME), "graph.png");
    		File overviewHtml = Configuration.getInstance().createTempFile();
			
			extractionModelEditor.graphView.exportDisplayToImage(overviewImg, overviewHtml);
			
			renderer.setOverviewHtml(Files.readAllLines(overviewHtml.toPath()).stream().collect(Collectors.joining(UIUtil.LINE_SEPARATOR)));
			openHTMLRender(null);
			overviewHtml.delete();
		} catch (Throwable e) {
			UIUtil.showException(this, "Error", e);
		} finally {
			renderer.setOverviewHtml(null);
		}
    }//GEN-LAST:event_exportDisplay1ActionPerformed

    private void renderHtml1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_renderHtml1ActionPerformed
    	exportDisplay1ActionPerformed(evt);
    }//GEN-LAST:event_renderHtml1ActionPerformed

    private void executeAndReload(Callable<Boolean> callable) {
        File tmpFile = null;
        String extractionModelFile = extractionModelEditor.extractionModelFile;
        boolean needsSave = extractionModelEditor.needsSave;
    	try {
    		tmpFile = Configuration.getInstance().createTempFile();
        	if (extractionModelEditor.save(tmpFile.getPath())) {
                if (Boolean.TRUE.equals(callable.call())) {
	        		extractionModelEditor.extractionModelFile = tmpFile.getPath();
					reload();
					askForDataModel(this);
					extractionModelEditor.closureBorderView.refresh();
					restrictedDependenciesView.refresh();
                }
        	}
        } catch (Throwable e) {
            UIUtil.showException(this, "Error", e);
        } finally {
    		extractionModelEditor.extractionModelFile = extractionModelFile;
    		extractionModelEditor.needsSave |= needsSave;
        	extractionModelEditor.extractionModelFrame.updateTitle(extractionModelEditor.needsSave);
        	reload.setEnabled(extractionModelEditor.extractionModelFile != null && needsSave);
        	if (tmpFile != null) {
        		tmpFile.delete();
        	}
    		if (theSession != null) {
    			theSession.shutDown();
    			theSession = null;
    		}
        }
    }

	boolean isHorizontalLayout = false;

	/**
	 * Updates title.
	 */
	public void updateTitle(boolean needsSave) {
		if (extractionModelEditor == null) {
			return;
		}
		reload.setEnabled(extractionModelEditor.extractionModelFile != null && needsSave);
		String title = "Jailer " + JailerVersion.VERSION + " Extraction Model Editor";
//		if (!"datamodel".equals(CommandLineParser.getInstance().getDataModelFolder())) {
//			title += " (" + new File(CommandLineParser.getInstance().getDataModelFolder()).getName() + ")";
//		}
		if (extractionModelEditor.extractionModelFile == null) {
			title = "New Model - " + title;
		} else {
			title = new File(extractionModelEditor.extractionModelFile).getName() + " - " + title;
		}
		if (needsSave) {
			title = "*" + title;
		}
		setTitle(title);
	}

	boolean hideIgnored() {
		return !showIgnored.isSelected();
	}

	public boolean showTableDetails() {
		return showTableDetails.isSelected();
	}

	/**
	 * Changes some nimbus LAF defaults.
	 */
	public static void customizeNimbus() {
		// nothing to do yet
	}

	/**
	 * @param args the command line arguments
	 */
	public static void main(String args[]) {
		try {
			List<String> aList = new ArrayList<String>(Arrays.asList(args));
			aList.remove("JailerDataBrowser"); // legacy
			if (aList.size() != 1) {
				DataBrowser.main(aList.toArray(new String[0]));
				return;
			}
			start(args);
		} catch (Throwable t) {
			t.printStackTrace();
			UIUtil.showException(null, "Error", t);
		}
	}

	/**
	 * @param args the command line arguments
	 */
	private static void start(String args[]) {
		try {
			args = Environment.init(args);
		} catch (Throwable e) {
			e.printStackTrace();
			UIUtil.showException(null, "Error", e);
			return;
		}
		try {
			CommandLineInstance.init(args);
		} catch (Exception e) {
			e.printStackTrace();
			UIUtil.showException(null, "Illegal arguments", e);
			return;
		}
		try {
			// create initial data-model files
			if (CommandLineInstance.getInstance().datamodelFolder == null) {
				File file = new File(DataModel.getDatamodelFolder(new ExecutionContext()));
				if (!file.exists()) {
					file.mkdir();
				}
			}
		} catch (Exception e) {
		}
		java.awt.EventQueue.invokeLater(new Runnable() {
			@Override
			public void run() {
				UIUtil.initPLAF();

				String file = null;
				try {
					UICommandLine commandLine = CommandLineInstance.getInstance();
					if (commandLine.arguments != null) {
						if (commandLine.arguments.size() > 0) {
							file = commandLine.arguments.get(0);
						}
					}
					createFrame(file, true, null, new ExecutionContext());
				} catch (Throwable e) {
					e.printStackTrace();
					UIUtil.showException(null, "Error", e);
					UIUtil.checkTermination();
				}
			}
		});
	}

	private static void askForDataModel(ExtractionModelFrame extractionModelFrame) throws Exception {
		if (extractionModelFrame.extractionModelEditor == null || extractionModelFrame.extractionModelEditor.dataModel == null || extractionModelFrame.extractionModelEditor.dataModel.getTables().isEmpty()) {
			switch (JOptionPane.showOptionDialog(extractionModelFrame, "Data model \"" + DataModelManager.getModelDetails(DataModelManager.getCurrentModelSubfolder(extractionModelFrame.executionContext), extractionModelFrame.executionContext).a + "\" is empty.", "Jailer " + JailerVersion.VERSION, JOptionPane.YES_NO_OPTION, JOptionPane.INFORMATION_MESSAGE, null, new Object[] { "Analyze Database", "Data Model Editor" }, null)) {
				case 0: extractionModelFrame.updateDataModelActionPerformed(null); break;
				   case 1: extractionModelFrame.openDataModelEditorActionPerformed(null); break;
			}
		} else if (!new File(DataModel.getColumnsFile(extractionModelFrame.executionContext)).exists()) {
			   switch (JOptionPane.showOptionDialog(extractionModelFrame, "No column definition found.", "Jailer " + JailerVersion.VERSION, JOptionPane.YES_NO_OPTION, JOptionPane.INFORMATION_MESSAGE, null, new Object[] { "Analyze Database", "Data Model Editor" }, null)) {
				case 0: extractionModelFrame.updateDataModelActionPerformed(null); break;
				   case 1: extractionModelFrame.openDataModelEditorActionPerformed(null); break;
			}
		}
		if (extractionModelFrame.extractionModelEditor != null) {
			if (extractionModelFrame.extractionModelEditor.closureBorderView != null) {
				extractionModelFrame.extractionModelEditor.closureBorderView.refresh();
			}
		}
		if (extractionModelFrame.restrictedDependenciesView != null) {
			extractionModelFrame.restrictedDependenciesView.refresh();
		}
	}

	public static ExtractionModelFrame createFrame(String file, boolean maximize, boolean visible, DbConnectionDialog connectionDialog, ExecutionContext executionContext) throws IOException {
		boolean isHorizonal = false;
		ExtractionModelFrame extractionModelFrame = new ExtractionModelFrame(file, isHorizonal, connectionDialog, executionContext);
		try {
			extractionModelFrame.setIconImage(UIUtil.readImage("/jailer.png").getImage());
		} catch (Throwable t) {
			// ignore
		}

		int c = 0;
		for (Frame frame: Frame.getFrames()) {
			if (frame instanceof ExtractionModelFrame && frame.isVisible()) {
				c = (c + 1) % 10;
			}
		}

		extractionModelFrame.setLocation(40 + c * 32, 40 + c * 32);
		extractionModelFrame.setSize(1100, 640);
		extractionModelFrame.setVisible(visible);
		extractionModelFrame.setExtendedState(Frame.MAXIMIZED_BOTH);
		return extractionModelFrame;
	}

	public static void createFrame(String file, final boolean withStartupWizzard, String module, ExecutionContext executionContext) {
		try {
			final String finalFile = file;
			if (file != null) {
				if (executionContext == null) {
					executionContext = new ExecutionContext();
				} else {
					executionContext = new ExecutionContext(executionContext);
				}
				DataModelManagerDialog.setCurrentBaseFolder(executionContext);
				DataModelManager.setCurrentModelSubfolder(ExtractionModel.loadDatamodelFolder(file, executionContext), executionContext);

				String datamodelFolder = CommandLineInstance.getInstance().datamodelFolder;
				if (datamodelFolder != null) {
					executionContext.setDatamodelFolder(new File(datamodelFolder).getParent());
					executionContext.setCurrentModelSubfolder(new File(datamodelFolder).getName());
				}

				createFrame(finalFile, true, true, null, executionContext);
			} else {
				DataModelManagerDialog.start(module, withStartupWizzard, executionContext);
			}
		} catch (Throwable e) {
			UIUtil.showException(null, "Error", e);
			UIUtil.checkTermination();
		}
	}

	/**
	 * @param withStartupWizzard
	 * @return
	 */
	public static DataModelManagerDialog createDMMDialog(boolean withStartupWizzard, ExecutionContext executionContext) {
		DataModelManagerDialog dataModelManagerDialog = new DataModelManagerDialog(JailerVersion.APPLICATION_NAME + " " + JailerVersion.VERSION + " - Database Subsetting Tool", true, "S", executionContext) {
			@Override
			protected void onLoadExtractionmodel(String modelFile, ExecutionContext executionContext) {
				createFrame(modelFile, false, "S", executionContext);
			}
			@Override
			protected void onSelect(DbConnectionDialog connectionDialog, ExecutionContext executionContext) {
				ExtractionModelFrame extractionModelFrame = null;
				try {
					extractionModelFrame = createFrame(null, true, true, connectionDialog, executionContext);
					CommandLineInstance.clear();
					final ExtractionModelFrame finalExtractionModelFrame = extractionModelFrame;
					UIUtil.invokeLater(new Runnable() {
						@SuppressWarnings("serial")
						@Override
						public void run() {
							try {
								askForDataModel(finalExtractionModelFrame);
							} catch (Exception e) {
								UIUtil.showException(finalExtractionModelFrame, "Error", e);
							}
							if (withStartupWizzard && finalExtractionModelFrame.showWizzard) {
								Point pos = null;
								if (finalExtractionModelFrame.extractionModelEditor != null && finalExtractionModelFrame.extractionModelEditor.layeredPane != null) {
									pos = new Point(14, 40);
									SwingUtilities.convertPointToScreen(pos, finalExtractionModelFrame.extractionModelEditor.layeredPane);
								}
								new StartupWizzardDialog(finalExtractionModelFrame, pos) {
									@Override
									protected void onClose() {
										try {
											if (loadModel) {
												finalExtractionModelFrame.loadActionPerformed(null);
											}
											if (newModelWithRestrictions) {
												finalExtractionModelFrame.extractionModelEditor.ignoreAll(null);
												finalExtractionModelFrame.extractionModelEditor.extractionModelFrame.updateTitle(finalExtractionModelFrame.extractionModelEditor.needsSave);
											}
										} catch (Exception e) {
											UIUtil.showException(finalExtractionModelFrame, "Error", e);
										}
									}
								};
							}
						}
					});
				} catch (Exception e) {
					UIUtil.showException(extractionModelFrame, "Error", e);
					UIUtil.checkTermination();
				}
			}
			private static final long serialVersionUID = 1L;
		};
		return dataModelManagerDialog;
	}

	JDialog pendingDecisionsDialog;

	/**
	 * Marks the model as dirty (needs save)
	 */
	public void markDirty() {
		extractionModelEditor.markDirty();
	}

	public JComponent tearOutGraphViewContainer(JFrame ownerFrame) {
		extractionModelEditor.layeredPane.remove(extractionModelEditor.inspectorHolder);
		extractionModelEditor.focusPanel.setVisible(false);
		extractionModelEditor.layeredPane.remove(extractionModelEditor.toolBarPanel);
		extractionModelEditor.addAdditionalPopupMenuItems(
				Arrays.asList(collapseAll, expandAll, expandAllVisible, refresh));
		AnimationController.registerWindow(ownerFrame, new AnimationController.AnimationControl() {
			@Override
			public void setEnabled(boolean enabled) {
				if (extractionModelEditor != null && extractionModelEditor.graphView != null) {
					extractionModelEditor.graphView.setAnimationEnabled(enabled);
				}
			}
		});

		Container parent = extractionModelEditor.undoViewHolder.getParent();
		if (parent != null) {
			parent.remove(extractionModelEditor.undoViewHolder);
		}
		return extractionModelEditor.layeredPane;
	}

	public void select(final Table table) {
		extractionModelEditor.subjectTable.setSelectedItem(extractionModelEditor.dataModel.getDisplayName(table));
		extractionModelEditor.graphView.updateTableDetailsMode();
		extractionModelEditor.graphView.toggleShowDetails(table);
		extractionModelEditor.graphView.selectTable(table);
		UIUtil.invokeLater(new Runnable() {
			@Override
			public void run() {
				extractionModelEditor.graphView.zoomToFit(0);
			}
		});
	}

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JMenuItem analyzeSQLMenuItem;
    javax.swing.JMenuItem checkPKMenuItem;
    private javax.swing.JMenuItem collapseAll;
    private javax.swing.JMenuItem columnOrderItem;
    private javax.swing.JCheckBoxMenuItem connectDb;
    private javax.swing.JMenuItem consistencyCheckMenuItem;
    private javax.swing.JMenuItem consistencyCheckMenuItem1;
    private javax.swing.JMenuItem cycleView;
    private javax.swing.JMenuItem dataExport;
    private javax.swing.JMenuItem dataImport;
    private javax.swing.JMenuItem disconnectDb;
    private javax.swing.JButton downloadButton;
    private javax.swing.JMenuItem downloadMenuItem;
    private javax.swing.JMenu editMenu;
    private javax.swing.JPanel editorPanel;
    private javax.swing.JMenuItem exit;
    private javax.swing.JMenuItem expandAll;
    private javax.swing.JMenuItem expandAllVisible;
    private javax.swing.JMenuItem exportDisplay;
    private javax.swing.JMenuItem exportDisplay1;
    private javax.swing.JMenu fileMenu;
    private javax.swing.JMenuItem helpContent;
    private javax.swing.JMenuItem helpForum;
    private javax.swing.JCheckBoxMenuItem horizontalLayoutMenuItem;
    private javax.swing.JMenuItem ignoreAll;
    private javax.swing.JButton jButton1;
    private javax.swing.JLayeredPane jLayeredPane1;
    private javax.swing.JMenu jMenu1;
    private javax.swing.JMenu jMenu2;
    private javax.swing.JMenu jMenu3;
    private javax.swing.JMenu jMenu4;
    private javax.swing.JMenu jMenu5;
    private javax.swing.JMenuBar jMenuBar2;
    private javax.swing.JMenuItem jMenuItem1;
    private javax.swing.JMenuItem jMenuItem2;
    private javax.swing.JMenuItem jMenuItem3;
    private javax.swing.JMenuItem jMenuItem4;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JSeparator jSeparator10;
    private javax.swing.JSeparator jSeparator12;
    private javax.swing.JPopupMenu.Separator jSeparator13;
    private javax.swing.JPopupMenu.Separator jSeparator14;
    private javax.swing.JPopupMenu.Separator jSeparator15;
    private javax.swing.JSeparator jSeparator2;
    private javax.swing.JPopupMenu.Separator jSeparator3;
    private javax.swing.JSeparator jSeparator4;
    private javax.swing.JSeparator jSeparator5;
    private javax.swing.JPopupMenu.Separator jSeparator6;
    private javax.swing.JSeparator jSeparator7;
    private javax.swing.JSeparator jSeparator8;
    private javax.swing.JMenuItem load;
    javax.swing.JMenuItem modelMigrationMenuItem;
    private javax.swing.JMenuItem newModel;
    private javax.swing.JMenuItem openDataBrowserItem;
    private javax.swing.JMenuItem openDataModelEditor;
    private javax.swing.JMenu plafMenu;
    private javax.swing.JMenuItem queryBuilder;
    javax.swing.JMenuItem redoMenuItem;
    private javax.swing.JMenuItem refresh;
    public javax.swing.JMenuItem reload;
    private javax.swing.JMenuItem removeAllRestrictions;
    private javax.swing.JMenuItem renderHtml;
    private javax.swing.JMenuItem renderHtml1;
    private javax.swing.JMenuItem save;
    private javax.swing.JMenuItem saveAs;
    private javax.swing.JCheckBoxMenuItem showIgnored;
    private javax.swing.JCheckBoxMenuItem showTableDetails;
    private javax.swing.JRadioButtonMenuItem steptime0;
    private javax.swing.JRadioButtonMenuItem steptime10;
    private javax.swing.JRadioButtonMenuItem steptime100;
    private javax.swing.JRadioButtonMenuItem steptime20;
    private javax.swing.JRadioButtonMenuItem steptime200;
    private javax.swing.JRadioButtonMenuItem steptime30;
    private javax.swing.JRadioButtonMenuItem steptime50;
    private javax.swing.JRadioButtonMenuItem steptime500;
    private javax.swing.JRadioButtonMenuItem steptime75;
    private javax.swing.ButtonGroup steptimeGroup;
    private javax.swing.JMenuItem tutorial;
    javax.swing.JMenuItem undoMenuItem;
    private javax.swing.JMenuItem updateDataModel;
    private javax.swing.JLabel updateInfoLabel;
    private javax.swing.JPanel updateInfoPanel;
    private javax.swing.JMenu viewMenu;
    private javax.swing.JMenuItem zoomToFit;
    // End of variables declaration//GEN-END:variables

	Session theSession;

	private static final long serialVersionUID = -2252377308370736756L;

}