/*
 * Copyright 2007 - 2012 the original author or authors.
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
import java.awt.Cursor;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;
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
import net.sf.jailer.DDLCreator;
import net.sf.jailer.Jailer;
import net.sf.jailer.ScriptFormat;
import net.sf.jailer.database.DMLTransformer;
import net.sf.jailer.database.Session;
import net.sf.jailer.database.TemporaryTableScope;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.DataModel.NoPrimaryKeyException;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.extractionmodel.ExtractionModel;
import net.sf.jailer.modelbuilder.ModelBuilder;
import net.sf.jailer.render.HtmlDataModelRenderer;
import net.sf.jailer.ui.databrowser.DataBrowser;
import net.sf.jailer.ui.graphical_view.LayoutStorage;

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
	private DbConnectionDialog dbConnectionDialog;
	
	/**
	 * The filter editor.
	 */
	private final FilterEditorDialog filterEditorDialog;
	
	/**
	 * The "Find Table" dialog.
	 */
	final ClosureView closureView;

	/**
	 * The border browser.
	 */
	final ClosureBorderDialog closureBorderView;

	/**
	 * The border browser.
	 */
	final ClosureBorderDialog restrictedDependenciesView;

	/**
	 * The "Cycle View" dialog.
	 */
	final CyclesView cycleViewDialog;

	/**
	 * File in which plaf-setting is stored.
	 */
	private static final String PLAFSETTING = ".plaf2.ui";

	/**
	 * File in which orientation is stored.
	 */
	private static final String ORIENTATIONSETTING = ".orientation.ui";

    /**
     *  Creates new form ExtractionModelFrame.
     *  
     *  @param extractionModelFile file containing the model, <code>null</code> for new model
     *  @param isHorizonal 
     */
    public ExtractionModelFrame(String extractionModelFile, boolean isHorizonal) {
        initComponents();
        initAnimationSteptime();
        isHorizontalLayout = isHorizonal;
        horizontalLayoutMenuItem.setSelected(isHorizontalLayout);
        editorPanel.add(extractionModelEditor = new ExtractionModelEditor(extractionModelFile, this, isHorizontalLayout, getConnectivityState(), getConnectivityStateToolTip()), "editor");
        extractionModelEditor.extractionModelFile = extractionModelFile;
        pack();
        updateTitle(extractionModelEditor.needsSave);
        dbConnectionDialog = new DbConnectionDialog(this, Jailer.APPLICATION_NAME, null);

		// L&F can no longer be changed
        view.setVisible(false);
        
        try {
	        for (final LookAndFeelInfo lfInfo: UIManager.getInstalledLookAndFeels()) {
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
        updateMenuItems();
        closureView = new ClosureView(this);
        closureBorderView = new ClosureBorderDialog(this, true) {
			private static final long serialVersionUID = -7426280043553389753L;
			@Override
			protected Table getRoot() {
				return extractionModelEditor == null? null : extractionModelEditor.root;
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
        
        restrictedDependenciesView = new RestrictedDependenciesListDialog(this) {
			private static final long serialVersionUID = -7426280043553389753L;
			@Override
			protected Table getRoot() {
				return extractionModelEditor == null? null : extractionModelEditor.root;
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
        
        cycleViewDialog = new CyclesView(this);
        filterEditorDialog = new FilterEditorDialog(this, new ParameterSelector.ParametersGetter() {
			@Override
			public Set<String> getParameters() {
				return extractionModelEditor.dataModel.getParameters(extractionModelEditor.condition.getText());
			}
		});
    }
    
    /**
     * Updates state of some menu items.
     */
    private void updateMenuItems() {
		connectDb.setSelected(dbConnectionDialog.isConnected);
		extractionModelEditor.connectivityState.setText(getConnectivityState());
		extractionModelEditor.connectivityState.setToolTipText(getConnectivityStateToolTip());
    }
	
	private String getConnectivityState() {
		if (dbConnectionDialog != null && dbConnectionDialog.isConnected) {
			return dbConnectionDialog.currentConnection.user + "@" + (dbConnectionDialog.currentConnection.alias);
		} else {
			return "offline";
		}
	}
	
	private String getConnectivityStateToolTip() {
		if (dbConnectionDialog != null && dbConnectionDialog.isConnected) {
			return (dbConnectionDialog.currentConnection.url);
		} else {
			return "offline";
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
        editorPanel = new javax.swing.JPanel();
        jMenuBar2 = new javax.swing.JMenuBar();
        fileMenu = new javax.swing.JMenu();
        newModel = new javax.swing.JMenuItem();
        load = new javax.swing.JMenuItem();
        jSeparator1 = new javax.swing.JSeparator();
        save = new javax.swing.JMenuItem();
        saveAs = new javax.swing.JMenuItem();
        exportDisplay = new javax.swing.JMenuItem();
        jSeparator2 = new javax.swing.JSeparator();
        connectDb = new javax.swing.JCheckBoxMenuItem();
        disconnectDb = new javax.swing.JMenuItem();
        jSeparator10 = new javax.swing.JSeparator();
        exit = new javax.swing.JMenuItem();
        editMenu = new javax.swing.JMenu();
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
        showReachability = new javax.swing.JMenuItem();
        zoomToFit = new javax.swing.JMenuItem();
        jSeparator4 = new javax.swing.JSeparator();
        closureMenuItem = new javax.swing.JMenuItem();
        jSeparator9 = new javax.swing.JSeparator();
        showIgnored = new javax.swing.JCheckBoxMenuItem();
        showTableDetails = new javax.swing.JCheckBoxMenuItem();
        jSeparator11 = new javax.swing.JSeparator();
        horizontalLayoutMenuItem = new javax.swing.JCheckBoxMenuItem();
        view = new javax.swing.JMenu();
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
        jMenu1 = new javax.swing.JMenu();
        updateDataModel = new javax.swing.JMenuItem();
        openDataModelEditor = new javax.swing.JMenuItem();
        jMenu3 = new javax.swing.JMenu();
        dataExport = new javax.swing.JMenuItem();
        dataImport = new javax.swing.JMenuItem();
        jSeparator5 = new javax.swing.JSeparator();
        openDataBrowserItem = new javax.swing.JMenuItem();
        closureToolMenuItem = new javax.swing.JMenuItem();
        closureBorderToolMenuItem = new javax.swing.JMenuItem();
        queryBuilder = new javax.swing.JMenuItem();
        cycleView = new javax.swing.JMenuItem();
        restrictedDependenciesToolMenuItem = new javax.swing.JMenuItem();
        renderHtml = new javax.swing.JMenuItem();
        jMenu2 = new javax.swing.JMenu();
        helpContent = new javax.swing.JMenuItem();
        tutorial = new javax.swing.JMenuItem();
        jSeparator7 = new javax.swing.JSeparator();
        helpForum = new javax.swing.JMenuItem();
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

        editorPanel.setLayout(new java.awt.CardLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        getContentPane().add(editorPanel, gridBagConstraints);

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

        exportDisplay.setText("Export graph as image");
        exportDisplay.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                exportDisplayActionPerformed(evt);
            }
        });
        fileMenu.add(exportDisplay);
        fileMenu.add(jSeparator2);

        connectDb.setText("Connect database");
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

        editMenu.setText("Edit");

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
        expandAllVisible.setText("Expand all visible tables");
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

        showReachability.setText("Show reachability of selected table");
        showReachability.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                showReachabilityActionPerformed(evt);
            }
        });
        viewMenu.add(showReachability);

        zoomToFit.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Z, java.awt.event.InputEvent.CTRL_MASK));
        zoomToFit.setText("Zoom to fit");
        zoomToFit.setVerifyInputWhenFocusTarget(false);
        zoomToFit.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                zoomToFitActionPerformed(evt);
            }
        });
        viewMenu.add(zoomToFit);
        viewMenu.add(jSeparator4);

        closureMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_G, java.awt.event.InputEvent.CTRL_MASK));
        closureMenuItem.setText("Open closure browser");
        closureMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                closureMenuItemActionPerformed(evt);
            }
        });
        viewMenu.add(closureMenuItem);
        viewMenu.add(jSeparator9);

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
        viewMenu.add(jSeparator11);

        horizontalLayoutMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_I, java.awt.event.InputEvent.CTRL_MASK));
        horizontalLayoutMenuItem.setSelected(true);
        horizontalLayoutMenuItem.setText("Horizontal layout");
        horizontalLayoutMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                horizontalLayoutMenuItemActionPerformed(evt);
            }
        });
        viewMenu.add(horizontalLayoutMenuItem);

        view.setLabel("Look&Feel");
        viewMenu.add(view);

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

        viewMenu.add(jMenu4);

        jMenuBar2.add(viewMenu);

        jMenu1.setText("DataModel");

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

        jMenuBar2.add(jMenu1);

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

        closureToolMenuItem.setText("Closure Browser");
        closureToolMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                closureToolMenuItemActionPerformed(evt);
            }
        });
        jMenu3.add(closureToolMenuItem);

        closureBorderToolMenuItem.setText("Closure Border Browser");
        closureBorderToolMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                closureBorderToolMenuItemActionPerformed(evt);
            }
        });
        jMenu3.add(closureBorderToolMenuItem);

        queryBuilder.setText("Query Builder");
        queryBuilder.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                queryBuilderActionPerformed(evt);
            }
        });
        jMenu3.add(queryBuilder);

        cycleView.setText("Cycle View");
        cycleView.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cycleViewActionPerformed(evt);
            }
        });
        jMenu3.add(cycleView);

        restrictedDependenciesToolMenuItem.setText("Restricted Dependencies View");
        restrictedDependenciesToolMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                restrictedDependenciesToolMenuItemActionPerformed(evt);
            }
        });
        jMenu3.add(restrictedDependenciesToolMenuItem);

        renderHtml.setText("HTML Rendering");
        renderHtml.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                renderHtmlActionPerformed(evt);
            }
        });
        jMenu3.add(renderHtml);

        jMenuBar2.add(jMenu3);

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

        helpForum.setLabel("Help Forum");
        helpForum.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                helpForumActionPerformed(evt);
            }
        });
        jMenu2.add(helpForum);
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

    private void closureMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_closureMenuItemActionPerformed
    	openClosureView(extractionModelEditor.root);
    }//GEN-LAST:event_closureMenuItemActionPerformed

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
			BrowserLauncher.openURL(new URI("https://sourceforge.net/forum/?group_id=197260"));
		} catch (Exception e) {
			UIUtil.showException(this, "Error", e);
		}
    }//GEN-LAST:event_helpForumActionPerformed

    private void zoomToFitActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_zoomToFitActionPerformed
    	extractionModelEditor.zoomToFit();
    }//GEN-LAST:event_zoomToFitActionPerformed

    private void tutorialActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_tutorialActionPerformed
    	try {
			BrowserLauncher.openURL(new URI("http://jailer.sourceforge.net/doc/exporting-data.htm"));
		} catch (Exception e) {
			UIUtil.showException(this, "Error", e);
		}
    }//GEN-LAST:event_tutorialActionPerformed

    private void helpContentActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_helpContentActionPerformed
    	try {
			BrowserLauncher.openURL(new URI("http://jailer.sourceforge.net/doc/home.htm"));
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
    	try {
    		String sqlFile = UIUtil.choseFile(null, ".", "Data Import", ".sql", this, false, true);
    		if (sqlFile != null) {
    			disconnect();
    			if (connectToDBIfNeeded("Data Import")) {
    				List<String> args = new ArrayList<String>();
    				args.add("import");
    				args.add(sqlFile);
    				dbConnectionDialog.addDbArgs(args);
    				disconnect();
    				UIUtil.runJailer(this, args, false, true, false, false, null, dbConnectionDialog.getPassword(), null, null, false, true, false);
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
		closureView.refresh(tableToSelect);
		closureView.setVisible(true);
		closureView.toFront();
	}
	
	/**
     * Opens the closure view.
     * 
     * @param root the table to open initially
     */
	public void openDataBrowser(Table root, String condition) {
    	if (dbConnectionDialog.isConnected || dbConnectionDialog.connect("Data Browser")) {
			updateMenuItems();
    		DataBrowser dataBrowser;
			try {
				dataBrowser = new DataBrowser(extractionModelEditor.dataModel, root, condition, dbConnectionDialog, true);
				dataBrowser.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
				dataBrowser.setVisible(true);
			} catch (Exception e) {
				UIUtil.showException(this, "Error", e);
			}
		}
	}

	/**
     * Sets Look&Feel.
     * 
     * @param plaf the l&f
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
	    }
    	catch (Exception e) {
	    	UIUtil.showException(this, "Error", e);
	    }
    }
    
    /**
     * Stores orientation.
     */
    private void storeOrientation() {
	    try {
	    	try {
                File file = new File(ORIENTATIONSETTING);
                file.delete();
            } catch (Exception e) {
            }
            try {
	    		File setting = new File(ORIENTATIONSETTING);
	    		PrintWriter out = new PrintWriter(setting);
	    		out.println(isHorizontalLayout);
	    		out.close();
	    	} catch (Exception x) {
	    	}
	    }
    	catch (Exception e) {
	    	UIUtil.showException(this, "Error", e);
	    }
    }
    
    private void openDataModelEditorActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_openDataModelEditorActionPerformed
    	openDataModelEditor(null);
    }//GEN-LAST:event_openDataModelEditorActionPerformed

	/**
	 * Opens the data model editor.
	 */
	private void openDataModelEditor(Table toEdit) {
		try {
    		if (saveIfNeeded("edit data model", true)) {
    			String modelname = extractionModelEditor.dataModel == null? DataModel.DEFAULT_NAME : extractionModelEditor.dataModel.getName();
       			DataModelEditor dataModelEditor = new DataModelEditor(this, false, false, toEdit, null, null, modelname, null);
       			dataModelEditor.setVisible(true);
       		//	if (dataModelEditor.saved) {
       				reload();
       		//	}
       			askForDataModel(this);
    			closureBorderView.refresh();
    			restrictedDependenciesView.refresh();
    		}
        } catch (Exception e) {
        	UIUtil.showException(this, "Error", e);
        }
	}
 
    private void updateDataModelActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_updateDataModelActionPerformed
    	try {
    		if (saveIfNeeded("Analyze Database", true)) {
	        	if (connectToDBIfNeeded("Analyze Database")) {
		        	List<String> args = new ArrayList<String>();
		        	args.add("build-model");
		        	dbConnectionDialog.addDbArgs(args);
		        	
		        	AnalyseOptionsDialog analyseOptionsDialog = new AnalyseOptionsDialog(this, extractionModelEditor.dataModel);
		        	boolean[] isDefaultSchema = new boolean[1];
		        	String[] defaultSchema = new String[1];
		        	setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
		        	List<String> schemas;
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
						if (UIUtil.runJailer(this, args, false, true, false, true, null, dbConnectionDialog.getPassword(), null, null, false, true, false)) {
			        		ModelBuilder.assocFilter = null;
			    			String modelname = extractionModelEditor.dataModel == null? DataModel.DEFAULT_NAME : extractionModelEditor.dataModel.getName();
			        		DataModelEditor dataModelEditor = new DataModelEditor(this, true, analyseOptionsDialog.isRemoving(), null, analyseOptionsDialog.getTableLineFilter(), analyseOptionsDialog.getAssociationLineFilter(), modelname, schema == null? dbConnectionDialog.getName() : schema);
							if (dataModelEditor.dataModelHasChanged()) {
								dataModelEditor.setVisible(true);
							}
		           //			if (dataModelEditor.saved) {
		           				reload();
		           //			}
		           			askForDataModel(this);
		        		}
	        		}
	        	}
    			askForDataModel(this);
    		}
        } catch (Exception e) {
        	UIUtil.showException(this, "Error", e);
        } finally {
        	ModelBuilder.assocFilter = null;
        }
    }//GEN-LAST:event_updateDataModelActionPerformed

	void dataExportActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_dataExportActionPerformed
    	try {
    		if (extractionModelEditor.dataModel != null && !ScriptFormat.XML.equals(extractionModelEditor.scriptFormat)) {
    			Association restrictedDependency = findRestrictedDependency(extractionModelEditor.dataModel);
    			if (restrictedDependency != null) {
    				switch (JOptionPane.showOptionDialog(this, 
    						"Dependency from '" + restrictedDependency.source.getName() + "' to '" + restrictedDependency.destination.getName() + "'\n" +
        					"is restricted.\nReferential integrity is not guaranteed!", "Restricted Dependency", JOptionPane.YES_NO_OPTION, JOptionPane.INFORMATION_MESSAGE, null, new Object[] { "Ok", "Cancel", "Show Dependency" }, "Cancel")) {
					case 1: return;
					case 2: 
						restrictedDependenciesView.setVisible(true);
						restrictedDependenciesView.toFront();
						return;
					}
    			}
    		}
    		if (extractionModelEditor.subject == null) {
    			askForDataModel(this);
    			return;
    		}
    		if (extractionModelEditor.dataModel != null) {
    			extractionModelEditor.dataModel.checkForPrimaryKey(extractionModelEditor.subject, false);
    		}
    		if (saveIfNeeded("Export data", false)) {
    			if (extractionModelEditor.extractionModelFile != null || extractionModelEditor.save(true, "Export data")) {
		        	if (connectToDBIfNeeded("Export data")) {
			        	List<String> args = new ArrayList<String>();
			        	args.add("export");
			        	args.add(extractionModelEditor.extractionModelFile);
			        	dbConnectionDialog.addDbArgs(args);
			        	Session.closeTemporaryTableSession();
			        	Session session = new Session(dbConnectionDialog.currentConnection.driverClass, dbConnectionDialog.currentConnection.url, dbConnectionDialog.currentConnection.user, dbConnectionDialog.getPassword());
			        	ExportDialog exportDialog = new ExportDialog(this, extractionModelEditor.dataModel, extractionModelEditor.getSubject(), extractionModelEditor.getSubjectCondition(), session, args, dbConnectionDialog.getPassword());
			        	session.shutDown();
			        	Session.closeTemporaryTableSession();
			        	if (exportDialog.isOk()) {
			        		exportDialog.fillCLIArgs(args);
				        	List<String> ddlArgs = new ArrayList<String>();
				        	ddlArgs.add("create-ddl");
				        	dbConnectionDialog.addDbArgs(ddlArgs);
				        	DMLTransformer.numberOfExportedLOBs = 0;
				        	String tableInConflict = DDLCreator.getTableInConflict(ddlArgs.get(1), ddlArgs.get(2), ddlArgs.get(3), ddlArgs.get(4));
				        	if (tableInConflict != null && exportDialog.getTemporaryTableScope().equals(TemporaryTableScope.GLOBAL)) {
				        		JOptionPane.showMessageDialog(this, "Can't drop table '" + tableInConflict + "' as it is not created by Jailer.\nDrop or rename this table first.", "Error", JOptionPane.ERROR_MESSAGE);
				        	}
				        	else {
				        		if (!exportDialog.getTemporaryTableScope().equals(TemporaryTableScope.GLOBAL) || DDLCreator.isUptodate(ddlArgs.get(1), ddlArgs.get(2), ddlArgs.get(3), ddlArgs.get(4)) || UIUtil.runJailer(this, ddlArgs, true, true, false, true, 
				        			"Automatic creation of working-tables failed!\n" +
			        				"Please execute the Jailer-DDL manually (jailer_ddl.sql)\n\n" +
			        				"Continue Data Export?", dbConnectionDialog.getPassword(), null, null, false, false, true)) {
					        		ProgressTable progressTable = new ProgressTable();
					        		ProgressPanel progressPanel = new ProgressPanel(progressTable);
					        		UIProgressListener progressListener = new UIProgressListener(progressTable, progressPanel, extractionModelEditor.dataModel);
					        		try {
					        			UIUtil.runJailer(this, args, true, true, exportDialog.explain.isSelected(), false /* !exportDialog.explain.isSelected() */, null, dbConnectionDialog.getPassword(), progressListener, progressPanel, true, true, false);
					        		} finally {
					        			progressListener.stop();
					        		}
				        		}
			        		}
			        	}
		        	}
	        	}
    		}
        } catch (Exception e) {
        	if (e instanceof DataModel.NoPrimaryKeyException) {
    			if (JOptionPane.showOptionDialog(this, e.getMessage(), "No Primary Key", JOptionPane.YES_NO_OPTION, JOptionPane.ERROR_MESSAGE, null, new Object[] { "Edit Table", "Cancel" }, null) == 0) {
    				openDataModelEditor(((NoPrimaryKeyException) e).table);
    			}
    		} else {
    			UIUtil.showException(this, "Error", e);
    		}
        }
    }//GEN-LAST:event_dataExportActionPerformed

	/**
	 * Finds restricted dependency.
	 */
    private Association findRestrictedDependency(DataModel dataModel) {
    	for (Association association: dataModel.namedAssociations.values()) {
    		if (association.isInsertDestinationBeforeSource() && association.isRestricted()) {
    			return association;
    		}
    	}
		return null;
	}

	private void disconnectDbActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_disconnectDbActionPerformed
    	disconnect();
    }//GEN-LAST:event_disconnectDbActionPerformed

	private void disconnect() {
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
        	args.add(file.getName());
        	UIUtil.runJailer(this, args, false, true, false, true, null, null /* dbConnectionDialog.getPassword() */, null, null, false, true, false);
        	BrowserLauncher.openURL(table == null? "render/index.html" : ("render/" + HtmlDataModelRenderer.toFileName(table)));
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
	        	args.add(file.getName());
	        	UIUtil.runJailer(this, args, false, false, false, false, null, dbConnectionDialog.getPassword(), null, null, false, true, false);
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
		File file;
		String extractionModelFile = extractionModelEditor.extractionModelFile;
		if (extractionModelFile == null) {
			file = new File("tmp_restrictions.csv");
		} else {
			extractionModelFile = new File(extractionModelFile).getName();
			if (extractionModelFile.toLowerCase().endsWith(".csv")) {
				file = new File(extractionModelFile.substring(0, extractionModelFile.length() - 4) + "-restrictions.csv");
			} else {
				file = new File(extractionModelFile + "-restrictions.csv");
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
    private boolean connectToDBIfNeeded(String reason) {
    	try {
    		if (!dbConnectionDialog.isConnected) {
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
    		this.dbConnectionDialog = new DbConnectionDialog(this, dbConnectionDialog, Jailer.APPLICATION_NAME);
    	} finally {
        	updateMenuItems();
    	}
	}

	private void jMenuItem1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem1ActionPerformed
		About about = new About(this, true);
		about.setTitle("Jailer " + Jailer.VERSION);
		about.pack();
		about.setLocation(getLocation().x + (getSize().width - about.getPreferredSize().width) / 2, getLocation().y + (getSize().height - about.getPreferredSize().height) / 2);
		about.setVisible(true);
    }//GEN-LAST:event_jMenuItem1ActionPerformed

    private void loadActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_loadActionPerformed
    	if (saveIfNeeded("loading", true)) {
    		String modelFile = UIUtil.choseFile(null, "extractionmodel", "Load Extraction Model", ".csv", this, true, true);
    		if (modelFile != null) {
	    		load(modelFile);
    		}
    	}
    }//GEN-LAST:event_loadActionPerformed

    /**
     * Loads an extraction model.
     * 
     * @param modelFile name of model file
     */
	private void load(String modelFile) {
		try {
			String dmf = ExtractionModel.loadDatamodelFolder(modelFile);
			if (dmf == null && DataModelManager.getCurrentModelSubfolder() != null
				||
				dmf != null && !dmf.equals(DataModelManager.getCurrentModelSubfolder())) {
				JOptionPane.showMessageDialog(this, "Unable to load \"" + new File(modelFile).getName() + "\"\nExtraction model is assigned to data model \"" + DataModelManager.getModelDetails(dmf).a + "\"", "Wrong Data Model", JOptionPane.ERROR_MESSAGE);
				return;
			}
			setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
			extractionModelEditor.extractionModelFrame = null;
			editorPanel.remove(extractionModelEditor);
			extractionModelEditor = null;
    		editorPanel.add(extractionModelEditor = new ExtractionModelEditor(modelFile, this, isHorizontalLayout, getConnectivityState(), getConnectivityStateToolTip()), "editor");
			((CardLayout) editorPanel.getLayout()).show(editorPanel, "editor");
			validate();
			closureBorderView.refresh();
			restrictedDependenciesView.refresh();
			updateTitle(extractionModelEditor.needsSave);
		} catch (Throwable t) {
			UIUtil.showException(this, "Error", t);
		} finally {
			setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		}
	}

    private void newModelActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_newModelActionPerformed
    	try {
    		setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
    		if (saveIfNeeded("creating new model", true)) {
	    		extractionModelEditor.extractionModelFrame = null;
	    		editorPanel.remove(extractionModelEditor);
				extractionModelEditor = null;
	    		editorPanel.add(extractionModelEditor = new ExtractionModelEditor(null, this, isHorizontalLayout, getConnectivityState(), getConnectivityStateToolTip()), "editor");
	    		((CardLayout) editorPanel.getLayout()).show(editorPanel, "editor");
	    		validate();
				closureBorderView.refresh();
				restrictedDependenciesView.refresh();
	    		updateTitle(extractionModelEditor.needsSave);
	    	}
		} catch (Throwable t) {
			UIUtil.showException(this, "Error", t);
		} finally {
			setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		}
    }//GEN-LAST:event_newModelActionPerformed

    private void reload() {
		try {
    		setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
	    	extractionModelEditor.extractionModelFrame = null;
			editorPanel.remove(extractionModelEditor);
			editorPanel.add(extractionModelEditor = new ExtractionModelEditor(extractionModelEditor.extractionModelFile, this, isHorizontalLayout, getConnectivityState(), getConnectivityStateToolTip()), "editor");
			((CardLayout) editorPanel.getLayout()).show(editorPanel, "editor");
			validate();
			closureBorderView.refresh();
			restrictedDependenciesView.refresh();
			updateTitle(extractionModelEditor.needsSave);
		} catch (Throwable t) {
			UIUtil.showException(this, "Error", t);
		} finally {
			setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
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
        try {
	    	LayoutStorage.enabled = false;
        	extractionModelEditor.refresh(false, true, false, true);
	        extractionModelEditor.resetGraphEditor(true, false, true, true);
        } finally {
	    	LayoutStorage.enabled = true;
        }
    }//GEN-LAST:event_collapseAllActionPerformed

    private void refreshActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_refreshActionPerformed
    	extractionModelEditor.refresh(false, true, false, false);
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
    private boolean saveIfNeeded(String cause, boolean ask) {
    	if (!extractionModelEditor.needsSave) {
    		return true;
    	}
    	if (ask) {
	    	int option = JOptionPane.showConfirmDialog(this, "Save changes before " + cause + "?", "", JOptionPane.YES_NO_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE);
	    	if (option == JOptionPane.CANCEL_OPTION) {
	    		return false;
	    	}
	    	if (option == JOptionPane.NO_OPTION) {
	    		return true;
	    	}
    	}
    	return extractionModelEditor.save(false, cause);
	}

    /**
     * Exits GUI.
     */
    private void onExit() {
    	if (extractionModelEditor.needsSave) {
	    	if (0 == JOptionPane.showConfirmDialog(
	    			this,  
	    			"Exit without saving?",
	                "",
	                JOptionPane.YES_NO_OPTION,
	                JOptionPane.QUESTION_MESSAGE)) {
	    		dispose();
	    	}
	    } else {
	    	dispose();
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
        openFilterEditor(null);
    }//GEN-LAST:event_jMenuItem2ActionPerformed

    private void exportDisplayActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_exportDisplayActionPerformed
        try {
        	extractionModelEditor.graphView.exportDisplayToImage();
        } catch (Throwable e) {
        	UIUtil.showException(this, "Error", e);
        }
    }//GEN-LAST:event_exportDisplayActionPerformed

    private void horizontalLayoutMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_horizontalLayoutMenuItemActionPerformed
    	isHorizontalLayout = !isHorizontalLayout;
    	extractionModelEditor.setOrientation(isHorizontalLayout);
    	horizontalLayoutMenuItem.setSelected(isHorizontalLayout);
    	storeOrientation();
    	jMenuBar2.grabFocus();
    }//GEN-LAST:event_horizontalLayoutMenuItemActionPerformed

    private void showReachabilityActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_showReachabilityActionPerformed
        if (extractionModelEditor.currentAssociation != null) {
        	extractionModelEditor.showReachability(extractionModelEditor.currentAssociation.destination);
        }
    }//GEN-LAST:event_showReachabilityActionPerformed

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
    	closureBorderView.setVisible(true);
    	closureBorderView.toFront();
    }//GEN-LAST:event_closureBorderToolMenuItemActionPerformed

    private void restrictedDependenciesToolMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_restrictedDependenciesToolMenuItemActionPerformed
    	restrictedDependenciesView.setVisible(true);
    	restrictedDependenciesView.toFront();
    }//GEN-LAST:event_restrictedDependenciesToolMenuItemActionPerformed
    
    boolean isHorizontalLayout = false;
	
    /**
     * Updates title.
     */
	public void updateTitle(boolean needsSave) {
		if (extractionModelEditor == null) {
			return;
		}
		String title = "Jailer " + Jailer.VERSION + " Extraction Model Editor";
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
		try {
			ToolTipManager.sharedInstance().setInitialDelay(400);
			ToolTipManager.sharedInstance().setDismissDelay(20000);
		} catch (Exception x) {
			x.printStackTrace();
		}
	}

	/**
     * @param args the command line arguments
     */
    public static void main(final String args[]) {
    	
//		UIUtil.showMaxMemory();
    	
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
    	} catch (Exception e) {
    	}
        java.awt.EventQueue.invokeLater(new Runnable() {
            public void run() {
            	
            	try {
            		// L&F can no longer be changed
//    	    		File plafSetting = new File(PLAFSETTING);
    	    		String plaf;
//    	    		if (!plafSetting.exists()) {
    	    			plaf = "com.sun.java.swing.plaf.nimbus.NimbusLookAndFeel";
//    	    		} else {
//	    	    		BufferedReader in = new BufferedReader(new FileReader(plafSetting));
//	    	    		plaf = in.readLine();
//	    	    		in.close();
//    	    		}
    		    	UIManager.setLookAndFeel(plaf);
    		    	customizeNimbus();
    			} catch (Exception x) {
    			}
            	try {
        	    	DefaultSyntaxKit.initKit();
            	} catch (Throwable x) {
            		x.printStackTrace();
    			}
            	
//    			JFrame dummy = new JFrame();
//    			dummy.setIconImage(new ImageIcon(ExtractionModelFrame.class.getResource("/net/sf/jailer/resource/jailer.png")).getImage());
//    	    	switch (JOptionPane.showOptionDialog(dummy, "Choose Module", "Jailer " + Jailer.VERSION, JOptionPane.YES_NO_OPTION, JOptionPane.INFORMATION_MESSAGE, null,
//            			new Object[] { "Data Browser", "Database Subsetter" }, null)) {
//            	case 0: 
//            		DataBrowser.main(args);
//            		return;
//            	case 1: break;
//            	default: System.exit(0);
//            	}
            	
            	String file = null;
                try {
	            	if (CommandLineParser.getInstance().arguments != null) {
	            		if (CommandLineParser.getInstance().arguments.size() > 0) {
	            			file = CommandLineParser.getInstance().arguments.get(0);
	            		}	
	            	}
	            	final String finalFile = file;
	            	if (file != null && new File(file).exists()) {
	            		DataModelManager.setCurrentModelSubfolder(ExtractionModel.loadDatamodelFolder(file));
	            		createFrame(finalFile, true);
	            	} else {
		            	DataModelManagerDialog dataModelManagerDialog = new DataModelManagerDialog(Jailer.APPLICATION_NAME + " " + Jailer.VERSION + " - Database Subsetting Tool") {
							@Override
							protected void onSelect() {
				            	ExtractionModelFrame extractionModelFrame = null;
				            	try {
				            		extractionModelFrame = createFrame(finalFile, true);
				            		final ExtractionModelFrame finalExtractionModelFrame = extractionModelFrame;
					            	SwingUtilities.invokeLater(new Runnable() {
										@Override
										public void run() {
											try {
												askForDataModel(finalExtractionModelFrame);
											} catch (Exception e) {
												UIUtil.showException(finalExtractionModelFrame, "Error", e);
											}
										}
									});
								} catch (Exception e) {
									UIUtil.showException(extractionModelFrame, "Error", e);
								}
							}
							private static final long serialVersionUID = 1L;
		            	};
		            	dataModelManagerDialog.setVisible(true);
	            	}
				} catch (Exception e) {
					UIUtil.showException(null, "Error", e);
				}
            }

        });
    }
    
    private static void askForDataModel(
			ExtractionModelFrame extractionModelFrame) throws Exception {
		if (extractionModelFrame.extractionModelEditor == null || extractionModelFrame.extractionModelEditor.dataModel == null || extractionModelFrame.extractionModelEditor.dataModel.getTables().isEmpty()) {
        	switch (JOptionPane.showOptionDialog(extractionModelFrame, "Data model \"" + DataModelManager.getModelDetails(DataModelManager.getCurrentModelSubfolder()).a + "\" is empty.", "Jailer " + Jailer.VERSION, JOptionPane.YES_NO_OPTION, JOptionPane.INFORMATION_MESSAGE, null, new Object[] { "Analyze Database", "Data Model Editor" }, null)) {
        		case 0: extractionModelFrame.updateDataModelActionPerformed(null); break;
               	case 1: extractionModelFrame.openDataModelEditorActionPerformed(null); break;
        	}
        } else if (!new File(DataModel.getColumnsFile()).exists()) {
           	switch (JOptionPane.showOptionDialog(extractionModelFrame, "No column definition found.", "Jailer " + Jailer.VERSION, JOptionPane.YES_NO_OPTION, JOptionPane.INFORMATION_MESSAGE, null, new Object[] { "Analyze Database", "Data Model Editor" }, null)) {
        		case 0: extractionModelFrame.updateDataModelActionPerformed(null); break;
               	case 1: extractionModelFrame.openDataModelEditorActionPerformed(null); break;
            }
        }
		if (extractionModelFrame.closureBorderView != null) {
			extractionModelFrame.closureBorderView.refresh();
		}
		if (extractionModelFrame.restrictedDependenciesView != null) {
			extractionModelFrame.restrictedDependenciesView.refresh();
		}
	}
    	
    public static ExtractionModelFrame createFrame(String file, boolean maximize) {
		boolean isHorizonal = false;
		try {
			File setting = new File(ORIENTATIONSETTING);
			BufferedReader in = new BufferedReader(new FileReader(setting));
			String or = in.readLine();
			in.close();
			isHorizonal = Boolean.valueOf(or);
		} catch (Exception x) {
		}
		ExtractionModelFrame extractionModelFrame = new ExtractionModelFrame(file, isHorizonal);
		try {
			extractionModelFrame.setIconImage(new ImageIcon(extractionModelFrame.getClass().getResource("/net/sf/jailer/resource/jailer.png")).getImage());
		} catch (Throwable t) {
			try {
				extractionModelFrame.setIconImage(new ImageIcon(extractionModelFrame.getClass().getResource("/net/sf/jailer/resource/jailer.gif")).getImage());
			} catch (Throwable t2) {
			}
		}
		extractionModelFrame.setLocation(40, 40);
		extractionModelFrame.setSize(1100, 640);
		extractionModelFrame.setVisible(true);
		if (maximize) {
			extractionModelFrame.setExtendedState(JFrame.MAXIMIZED_BOTH);
		}
		return extractionModelFrame;
	}

	/**
     * Marks the model as dirty (needs save)
     */
	public void markDirty() {
		extractionModelEditor.markDirty();
	}

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JMenuItem closureBorderToolMenuItem;
    private javax.swing.JMenuItem closureMenuItem;
    private javax.swing.JMenuItem closureToolMenuItem;
    private javax.swing.JMenuItem collapseAll;
    private javax.swing.JCheckBoxMenuItem connectDb;
    private javax.swing.JMenuItem cycleView;
    private javax.swing.JMenuItem dataExport;
    private javax.swing.JMenuItem dataImport;
    private javax.swing.JMenuItem disconnectDb;
    private javax.swing.JMenu editMenu;
    private javax.swing.JPanel editorPanel;
    private javax.swing.JMenuItem exit;
    private javax.swing.JMenuItem expandAll;
    private javax.swing.JMenuItem expandAllVisible;
    private javax.swing.JMenuItem exportDisplay;
    private javax.swing.JMenu fileMenu;
    private javax.swing.JMenuItem helpContent;
    private javax.swing.JMenuItem helpForum;
    private javax.swing.JCheckBoxMenuItem horizontalLayoutMenuItem;
    private javax.swing.JMenuItem ignoreAll;
    private javax.swing.JMenu jMenu1;
    private javax.swing.JMenu jMenu2;
    private javax.swing.JMenu jMenu3;
    private javax.swing.JMenu jMenu4;
    private javax.swing.JMenuBar jMenuBar2;
    private javax.swing.JMenuItem jMenuItem1;
    private javax.swing.JMenuItem jMenuItem2;
    private javax.swing.JMenuItem jMenuItem3;
    private javax.swing.JMenuItem jMenuItem4;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JSeparator jSeparator10;
    private javax.swing.JSeparator jSeparator11;
    private javax.swing.JSeparator jSeparator12;
    private javax.swing.JSeparator jSeparator2;
    private javax.swing.JSeparator jSeparator4;
    private javax.swing.JSeparator jSeparator5;
    private javax.swing.JSeparator jSeparator7;
    private javax.swing.JSeparator jSeparator8;
    private javax.swing.JSeparator jSeparator9;
    private javax.swing.JMenuItem load;
    private javax.swing.JMenuItem newModel;
    private javax.swing.JMenuItem openDataBrowserItem;
    private javax.swing.JMenuItem openDataModelEditor;
    private javax.swing.JMenuItem queryBuilder;
    private javax.swing.JMenuItem refresh;
    private javax.swing.JMenuItem removeAllRestrictions;
    private javax.swing.JMenuItem renderHtml;
    private javax.swing.JMenuItem restrictedDependenciesToolMenuItem;
    private javax.swing.JMenuItem save;
    private javax.swing.JMenuItem saveAs;
    private javax.swing.JCheckBoxMenuItem showIgnored;
    private javax.swing.JMenuItem showReachability;
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
    private javax.swing.JMenuItem updateDataModel;
    private javax.swing.JMenu view;
    private javax.swing.JMenu viewMenu;
    private javax.swing.JMenuItem zoomToFit;
    // End of variables declaration//GEN-END:variables

	private static final long serialVersionUID = -2252377308370736756L;

}
