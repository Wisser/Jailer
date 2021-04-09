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
package net.sf.jailer.ui.databrowser;

import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.beans.PropertyVetoException;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.URI;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.ButtonGroup;
import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JInternalFrame;
import javax.swing.JLabel;
import javax.swing.JLayeredPane;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JToggleButton;
import javax.swing.JTree;
import javax.swing.JViewport;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;
import javax.swing.WindowConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.MouseInputAdapter;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.text.DefaultEditorKit;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.database.BasicDataSource;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.PrimaryKeyFactory;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;
import net.sf.jailer.modelbuilder.ModelBuilder;
import net.sf.jailer.render.HtmlDataModelRenderer;
import net.sf.jailer.ui.About;
import net.sf.jailer.ui.AnalyseOptionsDialog;
import net.sf.jailer.ui.AssociationListUI;
import net.sf.jailer.ui.AssociationListUI.AssociationModel;
import net.sf.jailer.ui.AssociationListUI.DefaultAssociationModel;
import net.sf.jailer.ui.AutoCompletion;
import net.sf.jailer.ui.BrowserLauncher;
import net.sf.jailer.ui.CLIPanel;
import net.sf.jailer.ui.ColumnOrderEditor;
import net.sf.jailer.ui.DataModelEditor;
import net.sf.jailer.ui.DataModelManager;
import net.sf.jailer.ui.DataModelManagerDialog;
import net.sf.jailer.ui.DbConnectionDialog;
import net.sf.jailer.ui.DbConnectionDialog.ConnectionInfo;
import net.sf.jailer.ui.Environment;
import net.sf.jailer.ui.ExtractionModelFrame;
import net.sf.jailer.ui.ImportDialog;
import net.sf.jailer.ui.JComboBox2;
import net.sf.jailer.ui.PrivilegedSessionProviderDialog;
import net.sf.jailer.ui.SessionForUI;
import net.sf.jailer.ui.StringSearchPanel;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.UIUtil.ResultConsumer;
import net.sf.jailer.ui.associationproposer.AssociationProposerView;
import net.sf.jailer.ui.commandline.CommandLineInstance;
import net.sf.jailer.ui.constraintcheck.ConstraintChecker;
import net.sf.jailer.ui.databrowser.BookmarksPanel.BookmarkId;
import net.sf.jailer.ui.databrowser.BrowserContentPane.SqlStatementTable;
import net.sf.jailer.ui.databrowser.Desktop.LayoutMode;
import net.sf.jailer.ui.databrowser.Desktop.RowBrowser;
import net.sf.jailer.ui.databrowser.metadata.MDGeneric;
import net.sf.jailer.ui.databrowser.metadata.MDSchema;
import net.sf.jailer.ui.databrowser.metadata.MDTable;
import net.sf.jailer.ui.databrowser.metadata.MetaDataDetailsPanel;
import net.sf.jailer.ui.databrowser.metadata.MetaDataPanel;
import net.sf.jailer.ui.databrowser.metadata.MetaDataPanel.OutlineInfo;
import net.sf.jailer.ui.databrowser.metadata.MetaDataSource;
import net.sf.jailer.ui.databrowser.sqlconsole.SQLConsole;
import net.sf.jailer.ui.syntaxtextarea.BasicFormatterImpl;
import net.sf.jailer.ui.util.AnimationController;
import net.sf.jailer.ui.util.SmallButton;
import net.sf.jailer.ui.util.UISettings;
import net.sf.jailer.ui.util.UpdateInfoManager;
import net.sf.jailer.util.CancellationHandler;
import net.sf.jailer.util.Quoting;

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
    Desktop desktop;

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
     * The border browser.
     */
    private final AssociationListUI borderBrowser;

    /**
     * The execution context.
     */
    private final ExecutionContext executionContext;

    private final JComboBox2<String> tablesComboBox;
	private boolean initialized = false;

	/**
	 * Allowed row limits.
	 */
	public static final Integer[] ROW_LIMITS = new Integer[] { 100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000, 70000, 100000 };
	public static final int ROW_LIMIT_DEFAULT = 500;

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
    public DataBrowser(final DataModel datamodel, final Table root, String condition, DbConnectionDialog dbConnectionDialog, Map<String, String> schemaMapping, boolean embedded, final ExecutionContext executionContext) throws Exception {
        this.executionContext = executionContext;
        this.datamodel = new Reference<DataModel>(datamodel);
        this.dbConnectionDialog = dbConnectionDialog != null ? new DbConnectionDialog(this, dbConnectionDialog, DataBrowserContext.getAppName(), executionContext) : null;
        this.borderBrowser = new AssociationListUI("Resolve", "Resolve selected Associations", true) {
            @Override
            protected void applyAction(Collection<AssociationModel> selection) {
                resolveSelection(selection);
            }
        };
        executionContext.setUseRowIdsOnlyForTablesWithoutPK(true);
        if (embedded) {
            DataBrowserContext.setSupportsDataModelUpdates(false);
        }
        initComponents();
        initMenu();
        
        UpdateInfoManager.checkUpdateAvailability(updateInfoPanel, updateInfoLabel, downloadMenuItem, "B");
		UIUtil.initPLAFMenuItem(nativeLAFCheckBoxMenuItem, this);
		if (datamodel != null) {
			UISettings.dmStats(datamodel);
		}
		initRowLimitButtons();
        workbenchTabbedPane.setTabComponentAt(0, new JLabel("Desktop", desktopIcon, JLabel.LEFT));
        workbenchTabbedPane.setTabComponentAt(1, new JLabel("SQL Console ", sqlConsoleIcon, JLabel.LEFT));
        workbenchTabbedPane.setTabComponentAt(workbenchTabbedPane.getTabCount() - 1, new JLabel(addSqlConsoleIcon));

        tableTreesTabbedPane.setTabComponentAt(0, new JLabel("Navigation", navigationIcon, JLabel.LEFT));
        tableTreesTabbedPane.setTabComponentAt(1, new JLabel("Database", databaseIcon, JLabel.LEFT));

        tableTreesTabbedPane.addChangeListener(new ChangeListener() {
			@Override
			public void stateChanged(ChangeEvent e) {
		        if (createMetaDataPanel != null && tableTreesTabbedPane.getSelectedComponent() == tablesCardPanel) {
					UIUtil.invokeLater(10, new Runnable() {
						@Override
						public void run() {
							if (createMetaDataPanel != null) {
								createMetaDataPanel.run();
							}
						}
					});
				}
			}
		});

        initialized = true;

        tablesComboBox = new JComboBox2<String>() {
        	@Override
        	public Dimension getMinimumSize() {
				return new Dimension(40, super.getMinimumSize().height);
        	}
        };
        tablesComboBox.setMaximumRowCount(20);
        updateNavigationCombobox();
        AutoCompletion.enable(tablesComboBox);

		tablesComboBox.grabFocus();

        GridBagConstraints gridBagConstraints = new GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1;
        navigationPanel.add(tablesComboBox, gridBagConstraints);

        gridBagConstraints = new GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.NONE;
        gridBagConstraints.weightx = 0;
        searchButton = StringSearchPanel.createSearchButton(this, tablesComboBox, "Open Table Browser", new Runnable() {
			@Override
			public void run() {
				openTableButtonActionPerformed(null);
			}
		});
		navigationPanel.add(searchButton, gridBagConstraints);

		tablesComboBox.setVisible(false);
		openTableButton.setVisible(false);
		searchButton.setText("Open Table");

        metaDataDetailsPanel = createMetaDataDetailsPanel(executionContext);
        metaDataViewPanel.add(metaDataDetailsPanel);

        jLayeredPane1.removeAll();
        jLayeredPane1.setLayout(null);
        jLayeredPane1.setLayer(layeredPaneContent, JLayeredPane.PALETTE_LAYER);
        jLayeredPane1.setLayer(dummy, JLayeredPane.DEFAULT_LAYER);
        gridBagConstraints = new GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1;
        gridBagConstraints.weighty = 1;
        jLayeredPane1.add(layeredPaneContent /*, gridBagConstraints */);
        layeredPaneContent.setLocation(0, 0);
//        jLayeredPane1.add(dummy /*, gridBagConstraints */);

        addComponentListener(new ComponentListener() {
			@Override
			public void componentShown(ComponentEvent e) {
				layeredPaneContent.setSize(jLayeredPane1.getSize());
				UIUtil.invokeLater(new Runnable() {
					@Override
					public void run() {
						desktopSplitPane.setDividerLocation(0.75);
					}
				});
			}
			@Override
			public void componentResized(ComponentEvent e) {
			}
			@Override
			public void componentMoved(ComponentEvent e) {
			}
			@Override
			public void componentHidden(ComponentEvent e) {
			}
		});

        jLayeredPane1.addComponentListener(new ComponentListener() {
			@Override
			public void componentShown(ComponentEvent e) {
				layeredPaneContent.setSize(jLayeredPane1.getSize());
			}
			@Override
			public void componentResized(ComponentEvent e) {
				layeredPaneContent.setSize(jLayeredPane1.getSize());
				jLayeredPane1.validate();
			}
			@Override
			public void componentMoved(ComponentEvent e) {
				layeredPaneContent.setSize(jLayeredPane1.getSize());
			}
			@Override
			public void componentHidden(ComponentEvent e) {
			}
		});

        gridBagConstraints = new GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1;
        gridBagConstraints.weighty = 1;
        JPanel anchorPanel = new JPanel(null);
        anchorPanel.setOpaque(false);
        jLayeredPane1.setLayer(anchorPanel, JLayeredPane.POPUP_LAYER);
        jLayeredPane1.add(anchorPanel/* , gridBagConstraints */);

        anchorManager = new DesktopAnchorManager(anchorPanel) {
			@Override
			protected void layout(RowBrowser anchor) {
				try {
					anchor.internalFrame.setSelected(true);
				} catch (PropertyVetoException e) {
					// ignore
				}
				arrangeLayout(true, anchor);
			}
			@Override
			protected boolean isApplicable(RowBrowser tableBrowser) {
				if (tableBrowser.parent == null) {
					return false;
				}
				if (desktop.desktopAnimation != null && desktop.desktopAnimation.isActive()) {
					return false;
				}
				RowBrowser ancestor = tableBrowser.parent;
				while (ancestor != null) {
					if (ancestor.internalFrame.isVisible() && Math.abs(tableBrowser.internalFrame.getY() - ancestor.internalFrame.getY()) > 2) {
						return true;
					}
					ancestor = ancestor.parent;
				}
				return false;
			}
        };

        if (jScrollPane1.getVerticalScrollBar() != null) {
            jScrollPane1.getVerticalScrollBar().setUnitIncrement(16);
            jScrollPane1.getVerticalScrollBar().addAdjustmentListener(new AdjustmentListener() {
				@Override
				public void adjustmentValueChanged(AdjustmentEvent e) {
					anchorManager.reset();
				}
			});
        }
        if (jScrollPane1.getHorizontalScrollBar() != null) {
            jScrollPane1.getHorizontalScrollBar().setUnitIncrement(16);
            jScrollPane1.getHorizontalScrollBar().addAdjustmentListener(new AdjustmentListener() {
				@Override
				public void adjustmentValueChanged(AdjustmentEvent e) {
					anchorManager.reset();
				}
			});
        }

        hiddenPanel.setVisible(false);
        borderBrowserPanel.add(borderBrowser, java.awt.BorderLayout.CENTER);

        gridBagConstraints = new GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.NONE;
        gridBagConstraints.weightx = 0;
        gridBagConstraints.weighty = 0;
//        jPanel4.add(new JPanel() {
//            @Override
//            public Dimension getMinimumSize() {
//                return new Dimension(1, 300);
//            }
//
//            private static final long serialVersionUID = -947582621664272477L;
//        }, gridBagConstraints);

        gridBagConstraints = new GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.NONE;
        gridBagConstraints.weightx = 0;
        gridBagConstraints.weighty = 0;
//        borderBrowserTitledPanel.add(new JPanel() {
//            @Override
//            public Dimension getMinimumSize() {
//                return new Dimension(1, 180);
//            }
//
//            private static final long serialVersionUID = -947582621664272477L;
//        }, gridBagConstraints);

        DefaultTreeCellRenderer renderer = new DefaultTreeCellRenderer() {
			@Override
			public Component getTreeCellRendererComponent(JTree tree, Object value, boolean sel, boolean expanded,
					boolean leaf, int row, boolean hasFocus) {
				Component render = super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);
				if (render instanceof JLabel) {
					ImageIcon icon = null;
					if (value instanceof DefaultMutableTreeNode && ((DefaultMutableTreeNode) value).getUserObject() instanceof TreeNodeForRowBrowser) {
						TreeNodeForRowBrowser node = (TreeNodeForRowBrowser) ((DefaultMutableTreeNode) value).getUserObject();
						if (node.rowBrowser.association != null) {
							if (node.rowBrowser.association.isInsertDestinationBeforeSource()) {
								icon = redIcon;
							} else if (node.rowBrowser.association.isInsertSourceBeforeDestination()) {
								icon = greenIcon;
							} else {
								icon = blueIcon;
							}
						} else {
							icon = tableIcon;
						}
					} else {
						if (((JLabel) render).getText().trim().length() > 0) {
							icon = navigationIcon;
						}
					}
					((JLabel) render).setIcon(icon);
					if (Environment.nimbus && ((JLabel) render).getText() != null) {
						((JLabel) render).setText(UIUtil.toHTML(((JLabel) render).getText(), 100));
					}
				}
				return render;
			}
        };
        renderer.setOpenIcon(null);
        renderer.setLeafIcon(null);
        renderer.setClosedIcon(null);
        navigationTree.setModel(new DefaultTreeModel(new DefaultMutableTreeNode("")));

        navigationTree.setCellRenderer(renderer);

        ButtonGroup buttonGroup = new ButtonGroup();

        buttonGroup.add(thumbnailLayoutRadioButtonMenuItem);
        buttonGroup.add(tinyLayoutRadioButtonMenuItem);
        buttonGroup.add(smallLayoutRadioButtonMenuItem);
        buttonGroup.add(mediumLayoutRadioButtonMenuItem);
        buttonGroup.add(largeLayoutRadioButtonMenuItem);

        mediumLayoutRadioButtonMenuItem.setSelected(true);

        setTitle(DataBrowserContext.getAppName(false));
        if (embedded) {
            analyseMenuItem.setEnabled(false);
            dataModelEditorjMenuItem.setEnabled(false);
            analyseSQLMenuItem1.setEnabled(false);
        }

        // L&F can no longer be changed
        jSeparator6.setVisible(false);
        view.setVisible(false);

        try {
            for (final LookAndFeelInfo lfInfo : UIManager.getInstalledLookAndFeels()) {
                JMenuItem mItem = new JMenuItem();
                mItem.setText(lfInfo.getName());
                view.add(mItem);
                mItem.addActionListener(new ActionListener() {
                    @Override
					public void actionPerformed(ActionEvent arg0) {
                        setPLAF(lfInfo.getClassName());
                    }
                });
            }
        } catch (Throwable t) {
        }

        try {
            setIconImage((jailerIcon = UIUtil.readImage("/jailer.png")).getImage());
        } catch (Throwable t) {
           // ignore
        }

        if (jailerIcon != null) {
        	jailerIcon.setImage(UIUtil.scaleIcon(jailerIcon, 16, 16).getImage());
        }
        
        if (dbConnectionDialog != null) {
            createSession(dbConnectionDialog);
            if (session == null) {
            	return;
            }
        }
        desktop = new Desktop(this.datamodel, jailerIcon, session, this, dbConnectionDialog, schemaMapping == null? new HashMap<String, String>() : schemaMapping, anchorManager, executionContext) {
            @Override
            public void openSchemaAnalyzer() {
                updateDataModel();
            }

            protected void updateMenu(boolean hasTableBrowser, boolean hasIFrame) {
            	storeSessionItem.setEnabled(hasIFrame);
            	closeAllMenuItem.setEnabled(hasIFrame);
            	addBookmarkMenuItem.setEnabled(hasTableBrowser);
            	exportDataMenuItem.setEnabled(hasTableBrowser);
                createExtractionModelMenuItem.setEnabled(hasTableBrowser);
                updateIFramesBar();
                super.updateMenu(hasTableBrowser, hasIFrame);
            }

            @Override
            protected void updateMenu(LayoutMode layoutMode) {
            	zoomInMenuItem.setEnabled(layoutMode != Desktop.LayoutMode.LARGE);
            	zoomOutMenuItem.setEnabled(layoutMode != Desktop.LayoutMode.THUMBNAIL);
            	tinyLayoutRadioButtonMenuItem.setSelected(layoutMode == Desktop.LayoutMode.TINY);
            	smallLayoutRadioButtonMenuItem.setSelected(layoutMode == Desktop.LayoutMode.SMALL);
            	mediumLayoutRadioButtonMenuItem.setSelected(layoutMode == Desktop.LayoutMode.MEDIUM);
            	largeLayoutRadioButtonMenuItem.setSelected(layoutMode == Desktop.LayoutMode.LARGE);
            	thumbnailLayoutRadioButtonMenuItem.setSelected(layoutMode == Desktop.LayoutMode.THUMBNAIL);
            }

            @Override
            protected DataBrowser openNewDataBrowser() {
                try {
                    return DataBrowser.openNewDataBrowser(DataBrowser.this.datamodel.get(), dbConnectionDialog, false, executionContext, null);
                } catch (Exception e) {
                    UIUtil.showException(this, "Error", e, session);
                    return null;
                }
            }

			@Override
			public void onNewDataModel() {
				onNewSession(session);
			}

			@Override
			protected SQLConsole getSqlConsole(boolean switchToConsole) {
				if (switchToConsole) {
					workbenchTabbedPane.setSelectedComponent(getCurrentSQLConsole());
				}
				return getCurrentSQLConsole();
			}

			@Override
			public void onLayoutChanged(boolean isLayouted, boolean scrollToCenter) {
				if (!isLayouted) {
					arrangeLayout(scrollToCenter);
				}
			}

			@Override
			protected int getRowLimit() {
				for (Entry<JRadioButtonMenuItem, Integer> e: rowLimitButtonToLimit.entrySet()) {
					if (e.getKey().isSelected()) {
						return e.getValue();
					}
				}
				return ROW_LIMIT_DEFAULT;
			}

			@Override
			protected void setRowLimit(int limit) {
				itemPerLimit.get(limit).setSelected(true);
				itemPerLimit.get(limit).doClick();
			}

			@Override
			protected boolean isDesktopVisible() {
				return workbenchTabbedPane.getSelectedComponent() == desktopSplitPane;
			}

			@Override
			protected void checkAnchorRetension() {
				if (anchorManager != null) {
					anchorManager.checkRetention();
				}
			}

			@Override
			public void updateBookmarksMenu() {
		        new BookmarksPanel(DataBrowser.this, bookmarkMenu, desktop, executionContext).updateBookmarksMenu();
			}

			@Override
			protected void changeColumnOrder(Table table) {
				openColumnOrderEditor(table);
			}

			@Override
			protected void onRowSelect(Table table, Row row) {
				if (metaDataDetailsPanel != null) {
					MetaDataSource metaDataSource = getMetaDataSource(session);
					boolean passTable = metaDataSource.isInitialized() && metaDataSource.getDefaultSchema() != null && metaDataSource.getDefaultSchema().isLoaded();
					if (passTable) {
						MDSchema schema = metaDataSource.getSchemaOfTable(table);
						if (schema != null && schema != metaDataSource.getDefaultSchema()) {
							if (!schema.isLoaded()) {
								passTable = false;
								schema.loadTables(true, null, null, null);
							}
						}
					}
					metaDataDetailsPanel.showMetaDataDetails(passTable? metaDataSource.toMDTable(table): null, table, row, true, datamodel);
				}
			}
			
			@Override
			protected void repaintOutline() {
				DataBrowser.this.repaintOutline();
			}

			@Override
			protected void openGlobalPopup(MouseEvent e) {
				DataBrowser.this.openGlobalPopup(e);
			}

			@Override
			protected boolean desktopOutlineDraggingInProgress() {
				return desktopOutline != null && desktopOutline.draggingInProgress();
			}
        };

		desktop.addMouseMotionListener(new MouseMotionListener() {
			@Override
			public void mouseMoved(MouseEvent e) {
				double buttonWidth = anchorManager.getButtonWidth() * 1.4;
				double minDist = Double.MAX_VALUE;
				RowBrowser nearest = null;
				for (RowBrowser br : desktop.getBrowsers()) {
					if (br.internalFrame != null && br.internalFrame.isVisible()) {
						if (e.getX() - buttonWidth <= br.internalFrame.getX()) {
							double dx = e.getX() - br.internalFrame.getX();
							double dy = e.getY() - br.internalFrame.getY();
							double dist2 = dx * dx + dy * dy;
							if (nearest == null || dist2 < minDist) {
								nearest = br;
								minDist = dist2;
							}
						}
					}
				}
				if (nearest != null) {
					anchorManager.onTableBrowserNeared(nearest);
				}
			}

			@Override
			public void mouseDragged(MouseEvent e) {
			}
		});

		desktopOutline = new DesktopOutline(navigationPanel, controlPanel, jScrollPane1, desktop);
        java.awt.GridBagConstraints constraints = new java.awt.GridBagConstraints();
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 1;
        constraints.weighty = 1;
        constraints.fill = GridBagConstraints.BOTH;
		outLinePanel.add(desktopOutline, constraints);

        new BookmarksPanel(this, bookmarkMenu, desktop, executionContext).updateBookmarksMenu();

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
               	if (closeAllSQLConsoles()) {
               		DataBrowser.this.dispose();
               	}
            }

            @Override
            public void windowClosed(WindowEvent e) {
            	storeLastSession();
                desktop.stop();
                UIUtil.checkTermination();
            }

            @Override
            public void windowActivated(WindowEvent e) {
            }
        });

        MouseInputAdapter mia = new MouseInputAdapter() {
            int m_XDifference, m_YDifference;
            Container c;

            @Override
			public void mouseDragged(MouseEvent e) {
                if (e.getButton() == MouseEvent.BUTTON3) {
                    return;
                }
                c = desktop.getParent();
                if (c instanceof JViewport) {
                    JViewport jv = (JViewport) c;
                    Point p = jv.getViewPosition();
                    int newX = p.x - (e.getX() - m_XDifference);
                    int newY = p.y - (e.getY() - m_YDifference);
                    int maxX = desktop.getWidth() - jv.getWidth();
                    int maxY = desktop.getHeight() - jv.getHeight();
                    if (newX < 0)
                        newX = 0;
                    if (newX > maxX)
                        newX = maxX;
                    if (newY < 0)
                        newY = 0;
                    if (newY > maxY)
                        newY = maxY;
                    jv.setViewPosition(new Point(newX, newY));
                    DesktopAnimation.stopScrolling = true;
                }
            }

            @Override
			public void mousePressed(MouseEvent e) {
                if (e.getButton() == MouseEvent.BUTTON3) {
                    return;
                }
                setCursor(Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR));
                m_XDifference = e.getX();
                m_YDifference = e.getY();
                DesktopAnimation.stopScrolling = true;
            }

            @Override
			public void mouseReleased(MouseEvent e) {
                if (e.getButton() == MouseEvent.BUTTON3) {
                    return;
                }
                setCursor(null);
            }

            @Override
            public void mouseClicked(MouseEvent e) {
                openGlobalPopup(e);
            }
        };

        desktop.addMouseMotionListener(mia);
        desktop.addMouseListener(mia);

        AnimationController.registerWindow(this, new AnimationController.AnimationControl() {
			@Override
			public void setEnabled(boolean enabled) {
				desktop.setAnimationEnabled(enabled);
			}
		});

        int c = 0;
        for (Frame frame: Frame.getFrames()) {
            if (frame instanceof DataBrowser && frame.isVisible()) {
                c = (c + 1) % 6;
            }
        }

        setLocation(40 + c * 32, 32 + c * 32);
        setSize(980, 800);

        UIUtil.fit(this);

        if (root != null) {
            final RowBrowser rb = desktop.addTableBrowser(null, null, root, null, condition, null, null, true);
            if (rb != null && rb.internalFrame != null) {
                UIUtil.invokeLater(10, new Runnable() {
                    @Override
                    public void run() {
                        try {
                            rb.internalFrame.setSelected(true);
                            desktop.getiFrameStateChangeRenderer().onIFrameSelected(rb.internalFrame);
                        } catch (PropertyVetoException e) {
                            // ignore
                        }
                    }
                });
            }
        }
        schemaNamePanel.addMouseListener(new java.awt.event.MouseAdapter() {
            private boolean in = false;

            @Override
            public void mousePressed(MouseEvent e) {
                desktop.openSchemaMappingDialog(false);
            }

            @Override
			public void mouseEntered(java.awt.event.MouseEvent evt) {
                in = true;
                updateBorder();
            }

            @Override
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
        updateClosureBrowser(null);

        TreeSelectionListener treeListener = new TreeSelectionListener() {
            @Override
            public void valueChanged(TreeSelectionEvent e) {
                if (e.getNewLeadSelectionPath() != null) {
                    Object lastPathComponent = e.getNewLeadSelectionPath().getLastPathComponent();
                    if (lastPathComponent != null && lastPathComponent instanceof DefaultMutableTreeNode) {
                        Object userObject = ((DefaultMutableTreeNode) lastPathComponent).getUserObject();
                        if (userObject instanceof TreeNodeForRowBrowser) {
                        	updateDataModelView(null);
                            updateClosureBrowser(((TreeNodeForRowBrowser) userObject).rowBrowser);
                            if (metaDataPanel != null) {
                            	if (((TreeNodeForRowBrowser) userObject).rowBrowser.browserContentPane != null) {
                            		if (!(((TreeNodeForRowBrowser) userObject).rowBrowser.browserContentPane.table instanceof SqlStatementTable)) {
                            			metaDataPanel.select(((TreeNodeForRowBrowser) userObject).rowBrowser.browserContentPane.table);
                            		}
                            	}
                            }
                            return;
                        }
                    }
                } else {
                	updateClosureBrowser(null);
                }
            }
        };

        navigationTree.getSelectionModel().addTreeSelectionListener(treeListener);
        initDnD(this);
    }

    private Map<JRadioButtonMenuItem, Integer> rowLimitButtonToLimit = new HashMap<JRadioButtonMenuItem, Integer>();

	private void initRowLimitButtons() {
		ButtonGroup group = new ButtonGroup();
		JRadioButtonMenuItem def = null;
        for (final Integer limit: ROW_LIMITS) {
        	JRadioButtonMenuItem item = new JRadioButtonMenuItem(limit.toString());
        	group.add(item);
        	rowLimitMenu.add(item);
        	itemPerLimit.put(limit, item);
        	item.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent arg0) {
	        		try {
	        			Component sc = workbenchTabbedPane.getSelectedComponent();
		        		if (sc instanceof SQLConsole) {
		        			SQLConsole sqlConsole = (SQLConsole) sc;
		        			sqlConsole.setRowLimit(limit);
		        		} else {
		        			rowLimitStore = limit;
		        			desktop.reloadRoots();
		        		}
					} catch (Exception e) {
						UIUtil.showException(DataBrowser.this, "Error", e);
					}
				}
			});
        	rowLimitButtonToLimit.put(item, limit);
        	if (limit == ROW_LIMIT_DEFAULT) {
        		def = item;
        	}
		}
        if (def != null) {
        	def.setSelected(true);
        }
	}

	private Integer rowLimitStore = ROW_LIMIT_DEFAULT;
	private Map<Integer, JRadioButtonMenuItem> itemPerLimit = new HashMap<Integer, JRadioButtonMenuItem>();
	
	@SuppressWarnings("unchecked")
	public void updateNavigationCombobox() {
		List<String> tables = new ArrayList<String>();

		for (Table table: datamodel.get().getTables()) {
			tables.add(datamodel.get().getDisplayName(table));
		}
		Collections.sort(tables);
		@SuppressWarnings({ "rawtypes" })
		ComboBoxModel model = new DefaultComboBoxModel(new Vector(tables));

		tablesComboBox.setModel(model);
	}

	private MetaDataDetailsPanel createMetaDataDetailsPanel(final ExecutionContext executionContext) {
		MetaDataDetailsPanel panel = new MetaDataDetailsPanel(this.datamodel, session, this, executionContext) {
			@Override
			protected void analyseSchema(String schemaName, boolean withViews, boolean withSynonyms) {
				updateDataModel(schemaName, withViews, withSynonyms);
			}
        };
        panel.tabbedPane.addTab("Closure Border", borderBrowserTabPane);
        panel.tabbedPane.addTab("Data Model", dataModelPanel);
        panel.tabbedPane.addChangeListener(new ChangeListener() {
 			@Override
 			public void stateChanged(ChangeEvent e) {
 				updateBorderBrowser();
 				updateDataModelView(null);
 				showDataModelMenuItem.setSelected(metaDataDetailsPanel.tabbedPane.getSelectedComponent() == dataModelPanel);
 			}
 		});
        return panel;
	}

    private void createSession(DbConnectionDialog dbConnectionDialog) throws Exception {
        ConnectionInfo connection = dbConnectionDialog.currentConnection;
        BasicDataSource dataSource = UIUtil.createBasicDataSource(this, connection.driverClass, connection.url, connection.user, connection.password, 0, dbConnectionDialog.currentJarURLs());
        SessionForUI newSession = SessionForUI.createSession(dataSource, dataSource.dbms, executionContext.getIsolationLevel(), false, this);
        if (newSession != null) {
            if (session != null) {
                try {
                    session.shutDown();
                    session = null;
                } catch (Exception e) {
                    // ignore
                }
            }
        	session = newSession;
	        List<String> args = new ArrayList<String>();
	        dbConnectionDialog.addDbArgs(args);
	        session.setCliArguments(args);
	        session.setPassword(dbConnectionDialog.getPassword());
	        onNewSession(session);
        }
    }

	protected void setConnection(DbConnectionDialog dbConnectionDialog) throws Exception {
        if (dbConnectionDialog != null) {
            dbConnectionDialog = new DbConnectionDialog(this, dbConnectionDialog, DataBrowserContext.getAppName(), executionContext);
        }
        this.dbConnectionDialog = dbConnectionDialog;
        desktop.dbConnectionDialog = dbConnectionDialog;
        if (dbConnectionDialog != null) {
            ConnectionInfo connection = dbConnectionDialog.currentConnection;
            if (connection != null) {
                createSession(dbConnectionDialog);
                if (session != null) {
	                desktop.session = session;
	                onNewSession(session);
	                desktop.openSchemaMappingDialog(true);
	                updateStatusBar();
	                desktop.updateMenu();
	                for (RowBrowser rb : desktop.getBrowsers()) {
	                	rb.browserContentPane.session = session;
	                	rb.browserContentPane.rows.clear();
	                }
	                for (RowBrowser rb : desktop.getRootBrowsers(false)) {
	                	rb.browserContentPane.reloadRows();
	                }
                }
            }
        }
    }

    public void updateStatusBar() {
        final int MAX_LENGTH = 50;
        ConnectionInfo connection = dbConnectionDialog != null ? dbConnectionDialog.currentConnection : null;
        String dburl = connection != null ? (connection.url) : " ";
        connectivityState.setToolTipText(dburl);
        dburl = connection != null ? (connection.user + "@" + connection.alias) : " ";
        if (dburl.length() > MAX_LENGTH) {
            dburl = dburl.substring(0, MAX_LENGTH - 3) + "...";
        }
        connectivityState.setText(dburl);
        DataModel dataModel = datamodel != null ? datamodel.get() : null;
        String modelname = "Data Model \"" + (dataModel == null ? DataModel.DEFAULT_NAME : dataModel.getName()) + "\"";
        String lastMod = dataModel == null ? "" : dataModel.getLastModifiedAsString();
        if (lastMod.length() > 0) {
            lastMod = " (" + lastMod + ")";
        }
        modelName.setText(modelname);
        modelName.setToolTipText(modelname + lastMod);

        String modelpath = executionContext.getQualifiedDatamodelFolder();
        try {
            modelpath = new File(modelpath).getAbsolutePath();
        } catch (Throwable t) {
            // use default modelpath
        }
        modelpath += File.separator;
        modelPath.setToolTipText(modelpath);
        if (modelpath.length() > MAX_LENGTH + 4) {
            modelpath = modelpath.substring(0, MAX_LENGTH / 2) + "..." + modelpath.substring(modelpath.length() - MAX_LENGTH / 2);
        }
        modelPath.setText(modelpath);

        String nonDefaultSchema = null;
        if (desktop.schemaMapping != null) {
            for (Map.Entry<String, String> e : desktop.schemaMapping.entrySet()) {
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
            // schemaName.setToolTipText(schemaName.getText());
        }
    }

    @SuppressWarnings("deprecation")
    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed"
    // <editor-fold defaultstate="collapsed"
    // <editor-fold defaultstate="collapsed"
    // <editor-fold defaultstate="collapsed"
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        dummy = new javax.swing.JPanel();
        jScrollPane3 = new javax.swing.JScrollPane();
        jTable1 = new javax.swing.JTable();
        borderBrowserTabPane = new javax.swing.JPanel();
        borderBrowserPanel = new javax.swing.JPanel();
        titleLabel = new javax.swing.JLabel();
        dataModelPanel = new javax.swing.JPanel();
        jLabel26 = new javax.swing.JLabel();
        jLayeredPane2 = new javax.swing.JLayeredPane();
        jPanel1 = new javax.swing.JPanel();
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
        jPanel2 = new javax.swing.JPanel();
        jSplitPane1 = new javax.swing.JSplitPane();
        jPanel3 = new javax.swing.JPanel();
        jPanel5 = new javax.swing.JPanel();
        workbenchTabbedPane = new javax.swing.JTabbedPane();
        desktopSplitPane = new javax.swing.JSplitPane();
        jLayeredPane1 = new javax.swing.JLayeredPane();
        layeredPaneContent = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        jInternalFrame1 = new javax.swing.JInternalFrame();
        hiddenPanel = new javax.swing.JPanel();
        closurePanel = new javax.swing.JPanel();
        consoleDummyPanel = new javax.swing.JPanel();
        addSQLConsoleTab = new javax.swing.JPanel();
        controlPanel = new javax.swing.JPanel();
        jSplitPane4 = new javax.swing.JSplitPane();
        tableTreesTabbedPane = new javax.swing.JTabbedPane();
        navigationPanel = new javax.swing.JPanel();
        navigationTreeScrollPane = new javax.swing.JScrollPane();
        navigationTree = new javax.swing.JTree();
        outLinePanel = new javax.swing.JPanel();
        openTableButton = new javax.swing.JButton();
        tablesCardPanel = new javax.swing.JPanel();
        tablesPanel = new javax.swing.JPanel();
        jPanel6 = new javax.swing.JPanel();
        jScrollPane2 = new javax.swing.JScrollPane();
        jTree1 = new javax.swing.JTree();
        refreshButton = new javax.swing.JButton();
        jPanel7 = new javax.swing.JPanel();
        metaDataViewPanel = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        jLabel6 = new javax.swing.JLabel();
        jLabel7 = new javax.swing.JLabel();
        jLabel8 = new javax.swing.JLabel();
        jLabel9 = new javax.swing.JLabel();
        jLabel10 = new javax.swing.JLabel();
        jLabel11 = new javax.swing.JLabel();
        jLabel12 = new javax.swing.JLabel();
        jLabel13 = new javax.swing.JLabel();
        jLabel14 = new javax.swing.JLabel();
        jLabel19 = new javax.swing.JLabel();
        jLabel20 = new javax.swing.JLabel();
        jLabel21 = new javax.swing.JLabel();
        jLabel22 = new javax.swing.JLabel();
        updateInfoPanel = new javax.swing.JPanel();
        updateInfoLabel = new javax.swing.JLabel();
        downloadButton = new javax.swing.JButton();
        jButton1 = new javax.swing.JButton();
        menuBar = new javax.swing.JMenuBar();
        jMenu1 = new javax.swing.JMenu();
        jMenuItem3 = new javax.swing.JMenuItem();
        closeAllMenuItem = new javax.swing.JMenuItem();
        jSeparator4 = new javax.swing.JPopupMenu.Separator();
        loadScriptMenuItem = new javax.swing.JMenuItem();
        saveScriptMenuItem = new javax.swing.JMenuItem();
        saveScriptAsMenuItem = new javax.swing.JMenuItem();
        jSeparator9 = new javax.swing.JPopupMenu.Separator();
        storeSessionItem = new javax.swing.JMenuItem();
        restoreSessionItem = new javax.swing.JMenuItem();
        jSeparator7 = new javax.swing.JPopupMenu.Separator();
        reconnectMenuItem = new javax.swing.JMenuItem();
        newBrowserjMenuItem = new javax.swing.JMenuItem();
        jSeparator12 = new javax.swing.JPopupMenu.Separator();
        exitMenuItem = new javax.swing.JMenuItem();
        menuTools = new javax.swing.JMenu();
        analyseMenuItem = new javax.swing.JMenuItem();
        dataModelEditorjMenuItem = new javax.swing.JMenuItem();
        schemaMappingMenuItem = new javax.swing.JMenuItem();
        jSeparator2 = new javax.swing.JPopupMenu.Separator();
        columnOrderItem = new javax.swing.JMenuItem();
        jSeparator11 = new javax.swing.JPopupMenu.Separator();
        analyseSQLMenuItem1 = new javax.swing.JMenuItem();
        jSeparator10 = new javax.swing.JPopupMenu.Separator();
        showDataModelMenuItem = new javax.swing.JCheckBoxMenuItem();
        checkPKMenuItem = new javax.swing.JMenuItem();
        consistencyCheckMenuItem1 = new javax.swing.JMenuItem();
        jviewMenu = new javax.swing.JMenu();
        rowLimitMenu = new javax.swing.JMenu();
        bookmarkMenu = new javax.swing.JMenu();
        addBookmarkMenuItem = new javax.swing.JMenuItem();
        editBookmarkMenuItem = new javax.swing.JMenuItem();
        jSeparator13 = new javax.swing.JPopupMenu.Separator();
        jMenu2 = new javax.swing.JMenu();
        exportDataMenuItem = new javax.swing.JMenuItem();
        dataImport = new javax.swing.JMenuItem();
        jSeparator8 = new javax.swing.JPopupMenu.Separator();
        createExtractionModelMenuItem = new javax.swing.JMenuItem();
        consistencyCheckMenuItem = new javax.swing.JMenuItem();
        renderHtml = new javax.swing.JMenuItem();
        createCLIItem = new javax.swing.JMenuItem();
        menuWindow = new javax.swing.JMenu();
        layoutMenuItem = new javax.swing.JMenuItem();
        jSeparator5 = new javax.swing.JPopupMenu.Separator();
        zoomInMenuItem = new javax.swing.JMenuItem();
        zoomOutMenuItem = new javax.swing.JMenuItem();
        jSeparator15 = new javax.swing.JPopupMenu.Separator();
        thumbnailLayoutRadioButtonMenuItem = new javax.swing.JRadioButtonMenuItem();
        tinyLayoutRadioButtonMenuItem = new javax.swing.JRadioButtonMenuItem();
        smallLayoutRadioButtonMenuItem = new javax.swing.JRadioButtonMenuItem();
        mediumLayoutRadioButtonMenuItem = new javax.swing.JRadioButtonMenuItem();
        largeLayoutRadioButtonMenuItem = new javax.swing.JRadioButtonMenuItem();
        jSeparator1 = new javax.swing.JPopupMenu.Separator();
        newWindowMenuItem = new javax.swing.JMenuItem();
        jSeparator6 = new javax.swing.JPopupMenu.Separator();
        view = new javax.swing.JMenu();
        jMenu3 = new javax.swing.JMenu();
        nativeLAFCheckBoxMenuItem = new javax.swing.JCheckBoxMenuItem();
        helpMenu = new javax.swing.JMenu();
        jMenuItem4 = new javax.swing.JMenuItem();
        helpForum = new javax.swing.JMenuItem();
        downloadMenuItem = new javax.swing.JMenuItem();
        jSeparator14 = new javax.swing.JPopupMenu.Separator();
        aboutMenuItem = new javax.swing.JMenuItem();

        jTable1.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null}
            },
            new String [] {
                "Title 1", "Title 2", "Title 3", "Title 4"
            }
        ));
        jScrollPane3.setViewportView(jTable1);

        dummy.add(jScrollPane3);

        borderBrowserTabPane.setLayout(new java.awt.GridBagLayout());

        borderBrowserPanel.setLayout(new java.awt.BorderLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        borderBrowserTabPane.add(borderBrowserPanel, gridBagConstraints);

        titleLabel.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(6, 0, 0, 0);
        borderBrowserTabPane.add(titleLabel, gridBagConstraints);

        dataModelPanel.setLayout(new java.awt.BorderLayout());

        jLabel26.setText("  Loading...");
        dataModelPanel.add(jLabel26, java.awt.BorderLayout.CENTER);

        jLayeredPane2.setLayout(new java.awt.GridBagLayout());

        jPanel1.setLayout(new java.awt.GridBagLayout());

        jPanel11.setLayout(new java.awt.GridBagLayout());

        legende1.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
        legende1.setLayout(new java.awt.GridBagLayout());

        modelName.setFont(modelName.getFont().deriveFont(modelName.getFont().getSize()+1f));
        modelName.setText("Data Model \"Demo\"");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 12);
        legende1.add(modelName, gridBagConstraints);

        modelPath.setFont(modelPath.getFont().deriveFont(modelPath.getFont().getSize()+1f));
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

        dependsOn.setFont(dependsOn.getFont().deriveFont(dependsOn.getFont().getSize()+1f));
        dependsOn.setForeground(new java.awt.Color(170, 0, 0));
        dependsOn.setText(" depends on (has parent) ");
        legende.add(dependsOn);

        hasDependent.setFont(hasDependent.getFont().deriveFont(hasDependent.getFont().getSize()+1f));
        hasDependent.setForeground(new java.awt.Color(0, 112, 0));
        hasDependent.setText("  has dependent (has child) ");
        legende.add(hasDependent);

        associatedWith.setFont(associatedWith.getFont().deriveFont(associatedWith.getFont().getSize()+1f));
        associatedWith.setForeground(new java.awt.Color(0, 100, 255));
        associatedWith.setText("  associated with");
        legende.add(associatedWith);

        ignored.setFont(ignored.getFont().deriveFont(ignored.getFont().getSize()+1f));
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

        schemaName.setFont(schemaName.getFont().deriveFont(schemaName.getFont().getSize()+1f));
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

        connectivityState.setFont(connectivityState.getFont().deriveFont(connectivityState.getFont().getSize()+1f));
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

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(jPanel11, gridBagConstraints);

        jPanel2.setLayout(new java.awt.GridBagLayout());

        jSplitPane1.setDividerLocation(340);
        jSplitPane1.setContinuousLayout(true);
        jSplitPane1.setOneTouchExpandable(true);

        jPanel3.setLayout(new java.awt.GridBagLayout());
        jSplitPane1.setLeftComponent(jPanel3);

        jPanel5.setLayout(new java.awt.GridBagLayout());

        workbenchTabbedPane.addChangeListener(new javax.swing.event.ChangeListener() {
            public void stateChanged(javax.swing.event.ChangeEvent evt) {
                workbenchTabbedPaneStateChanged(evt);
            }
        });

        desktopSplitPane.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        desktopSplitPane.setResizeWeight(0.95);
        desktopSplitPane.setContinuousLayout(true);
        desktopSplitPane.setOneTouchExpandable(true);

        layeredPaneContent.setLayout(new java.awt.GridBagLayout());

        jScrollPane1.setAutoscrolls(true);
        jScrollPane1.setWheelScrollingEnabled(false);
        jScrollPane1.addMouseWheelListener(new java.awt.event.MouseWheelListener() {
            public void mouseWheelMoved(java.awt.event.MouseWheelEvent evt) {
                jScrollPane1MouseWheelMoved(evt);
            }
        });

        jInternalFrame1.setVisible(true);
        jScrollPane1.setViewportView(jInternalFrame1);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        layeredPaneContent.add(jScrollPane1, gridBagConstraints);

        hiddenPanel.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        layeredPaneContent.add(hiddenPanel, gridBagConstraints);

        jLayeredPane1.setLayer(layeredPaneContent, javax.swing.JLayeredPane.PALETTE_LAYER);
        jLayeredPane1.add(layeredPaneContent);
        layeredPaneContent.setBounds(0, 0, 24, 36);

        desktopSplitPane.setLeftComponent(jLayeredPane1);

        closurePanel.setLayout(new java.awt.GridBagLayout());
        desktopSplitPane.setRightComponent(closurePanel);

        workbenchTabbedPane.addTab("Desktop", desktopSplitPane);
        workbenchTabbedPane.addTab("SQL Console", consoleDummyPanel);
        workbenchTabbedPane.addTab("+", addSQLConsoleTab);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(1, 0, 0, 0);
        jPanel5.add(workbenchTabbedPane, gridBagConstraints);

        jSplitPane1.setRightComponent(jPanel5);

        controlPanel.setLayout(new java.awt.GridBagLayout());

        jSplitPane4.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        jSplitPane4.setResizeWeight(1.0);
        jSplitPane4.setContinuousLayout(true);
        jSplitPane4.setOneTouchExpandable(true);

        navigationPanel.setLayout(new java.awt.GridBagLayout());

        navigationTree.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                navigationTreeMouseClicked(evt);
            }
        });
        navigationTreeScrollPane.setViewportView(navigationTree);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        navigationPanel.add(navigationTreeScrollPane, gridBagConstraints);

        outLinePanel.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        navigationPanel.add(outLinePanel, gridBagConstraints);

        openTableButton.setText("Open");
        openTableButton.setToolTipText("Open table browser for the selected table");
        openTableButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                openTableButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 0);
        navigationPanel.add(openTableButton, gridBagConstraints);

        tableTreesTabbedPane.addTab("Navigation", navigationPanel);

        tablesCardPanel.setLayout(new java.awt.CardLayout());

        tablesPanel.setLayout(new java.awt.BorderLayout());
        tablesCardPanel.add(tablesPanel, "tables");

        jPanel6.setLayout(new java.awt.GridBagLayout());

        javax.swing.tree.DefaultMutableTreeNode treeNode1 = new javax.swing.tree.DefaultMutableTreeNode("loading database meta data...");
        jTree1.setModel(new javax.swing.tree.DefaultTreeModel(treeNode1));
        jTree1.setSelectionModel(null);
        jScrollPane2.setViewportView(jTree1);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel6.add(jScrollPane2, gridBagConstraints);

        refreshButton.setText("Refresh");
        refreshButton.setToolTipText("Refresh Database Meta Data Cache");
        refreshButton.setEnabled(false);
        refreshButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                refreshButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        jPanel6.add(refreshButton, gridBagConstraints);

        tablesCardPanel.add(jPanel6, "loading");

        tableTreesTabbedPane.addTab("Database", tablesCardPanel);

        jSplitPane4.setLeftComponent(tableTreesTabbedPane);

        jPanel7.setLayout(new java.awt.GridBagLayout());

        metaDataViewPanel.setLayout(new javax.swing.BoxLayout(metaDataViewPanel, javax.swing.BoxLayout.LINE_AXIS));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridheight = 30;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel7.add(metaDataViewPanel, gridBagConstraints);

        jLabel1.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        jPanel7.add(jLabel1, gridBagConstraints);

        jLabel3.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        jPanel7.add(jLabel3, gridBagConstraints);

        jLabel4.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        jPanel7.add(jLabel4, gridBagConstraints);

        jLabel5.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 4;
        jPanel7.add(jLabel5, gridBagConstraints);

        jLabel6.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 5;
        jPanel7.add(jLabel6, gridBagConstraints);

        jLabel7.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 6;
        jPanel7.add(jLabel7, gridBagConstraints);

        jLabel8.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 7;
        jPanel7.add(jLabel8, gridBagConstraints);

        jLabel9.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 8;
        jPanel7.add(jLabel9, gridBagConstraints);

        jLabel10.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 10;
        jPanel7.add(jLabel10, gridBagConstraints);

        jLabel11.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 11;
        jPanel7.add(jLabel11, gridBagConstraints);

        jLabel12.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 12;
        jPanel7.add(jLabel12, gridBagConstraints);

        jLabel13.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 13;
        jPanel7.add(jLabel13, gridBagConstraints);

        jLabel14.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 14;
        jPanel7.add(jLabel14, gridBagConstraints);

        jLabel19.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 19;
        jPanel7.add(jLabel19, gridBagConstraints);

        jLabel20.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 20;
        jPanel7.add(jLabel20, gridBagConstraints);

        jLabel21.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 21;
        jPanel7.add(jLabel21, gridBagConstraints);

        jLabel22.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 22;
        jPanel7.add(jLabel22, gridBagConstraints);

        jSplitPane4.setRightComponent(jPanel7);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        controlPanel.add(jSplitPane4, gridBagConstraints);

        jSplitPane1.setLeftComponent(controlPanel);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel2.add(jSplitPane1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel1.add(jPanel2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jLayeredPane2.add(jPanel1, gridBagConstraints);

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

        jLayeredPane2.setLayer(updateInfoPanel, javax.swing.JLayeredPane.MODAL_LAYER);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHEAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 72, 24);
        jLayeredPane2.add(updateInfoPanel, gridBagConstraints);

        getContentPane().add(jLayeredPane2, java.awt.BorderLayout.CENTER);

        jMenu1.setText("File");

        jMenuItem3.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_T, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItem3.setText("Open Table");
        jMenuItem3.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem3ActionPerformed(evt);
            }
        });
        jMenu1.add(jMenuItem3);

        closeAllMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F4, java.awt.event.InputEvent.SHIFT_MASK | java.awt.event.InputEvent.CTRL_MASK));
        closeAllMenuItem.setText("Close All Tables");
        closeAllMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                closeAllMenuItemActionPerformed(evt);
            }
        });
        jMenu1.add(closeAllMenuItem);
        jMenu1.add(jSeparator4);

        loadScriptMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_O, java.awt.event.InputEvent.CTRL_MASK));
        loadScriptMenuItem.setText("Load SQL Script...");
        loadScriptMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                loadScriptMenuItemActionPerformed(evt);
            }
        });
        jMenu1.add(loadScriptMenuItem);

        saveScriptMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_S, java.awt.event.InputEvent.CTRL_MASK));
        saveScriptMenuItem.setText("Save");
        saveScriptMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                saveScriptMenuItemActionPerformed(evt);
            }
        });
        jMenu1.add(saveScriptMenuItem);

        saveScriptAsMenuItem.setText("Save as...");
        saveScriptAsMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                saveScriptAsMenuItemActionPerformed(evt);
            }
        });
        jMenu1.add(saveScriptAsMenuItem);
        jMenu1.add(jSeparator9);

        storeSessionItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_P, java.awt.event.InputEvent.CTRL_MASK));
        storeSessionItem.setText("Store Layout");
        storeSessionItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                storeSessionItemActionPerformed(evt);
            }
        });
        jMenu1.add(storeSessionItem);

        restoreSessionItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_R, java.awt.event.InputEvent.CTRL_MASK));
        restoreSessionItem.setText("Restore Layout");
        restoreSessionItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                restoreSessionItemActionPerformed(evt);
            }
        });
        jMenu1.add(restoreSessionItem);
        jMenu1.add(jSeparator7);

        reconnectMenuItem.setText("Reconnect...");
        reconnectMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                reconnectMenuItemActionPerformed(evt);
            }
        });
        jMenu1.add(reconnectMenuItem);

        newBrowserjMenuItem.setText("New Data Browser");
        newBrowserjMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                newBrowserjMenuItemActionPerformed(evt);
            }
        });
        jMenu1.add(newBrowserjMenuItem);
        jMenu1.add(jSeparator12);

        exitMenuItem.setText("Exit");
        exitMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                exitMenuItemActionPerformed(evt);
            }
        });
        jMenu1.add(exitMenuItem);

        menuBar.add(jMenu1);

        menuTools.setText("Model");

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

        schemaMappingMenuItem.setText("Schema Mapping");
        schemaMappingMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                schemaMappingMenuItemActionPerformed(evt);
            }
        });
        menuTools.add(schemaMappingMenuItem);
        menuTools.add(jSeparator2);

        columnOrderItem.setText("Define Column Order");
        columnOrderItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                columnOrderItemActionPerformed(evt);
            }
        });
        menuTools.add(columnOrderItem);
        menuTools.add(jSeparator11);

        analyseSQLMenuItem1.setText("Analyze SQL Script");
        analyseSQLMenuItem1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                analyseSQLMenuItem1ActionPerformed(evt);
            }
        });
        menuTools.add(analyseSQLMenuItem1);
        menuTools.add(jSeparator10);

        showDataModelMenuItem.setText("Show Data Model");
        showDataModelMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                showDataModelMenuItemActionPerformed(evt);
            }
        });
        menuTools.add(showDataModelMenuItem);

        checkPKMenuItem.setText("Check Primary Keys");
        checkPKMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                checkPKMenuItemActionPerformed(evt);
            }
        });
        menuTools.add(checkPKMenuItem);

        consistencyCheckMenuItem1.setText("Check Referential Consistency");
        consistencyCheckMenuItem1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                consistencyCheckMenuItem1ActionPerformed(evt);
            }
        });
        menuTools.add(consistencyCheckMenuItem1);

        menuBar.add(menuTools);

        jviewMenu.setText("View");

        rowLimitMenu.setText("Row Limit");
        rowLimitMenu.setToolTipText("Desktop Row Limit");
        jviewMenu.add(rowLimitMenu);

        menuBar.add(jviewMenu);

        bookmarkMenu.setText("Bookmark");

        addBookmarkMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_B, java.awt.event.InputEvent.CTRL_MASK));
        addBookmarkMenuItem.setText("Add Bookmark");
        addBookmarkMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                addBookmarkMenuItemActionPerformed(evt);
            }
        });
        bookmarkMenu.add(addBookmarkMenuItem);

        editBookmarkMenuItem.setText("Edit Bookmarks");
        editBookmarkMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                editBookmarkMenuItemActionPerformed(evt);
            }
        });
        bookmarkMenu.add(editBookmarkMenuItem);
        bookmarkMenu.add(jSeparator13);

        menuBar.add(bookmarkMenu);

        jMenu2.setText("Tools");

        exportDataMenuItem.setText("Export Data");
        exportDataMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                exportDataMenuItemActionPerformed(evt);
            }
        });
        jMenu2.add(exportDataMenuItem);

        dataImport.setLabel("Import SQL Data");
        dataImport.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                dataImportActionPerformed(evt);
            }
        });
        jMenu2.add(dataImport);
        jMenu2.add(jSeparator8);

        createExtractionModelMenuItem.setText("Create Extraction Model");
        createExtractionModelMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                createExtractionModelMenuItemActionPerformed(evt);
            }
        });
        jMenu2.add(createExtractionModelMenuItem);

        consistencyCheckMenuItem.setText("Referential Consistency Check");
        consistencyCheckMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                consistencyCheckMenuItemActionPerformed(evt);
            }
        });
        jMenu2.add(consistencyCheckMenuItem);

        renderHtml.setText("HTML Renderer");
        renderHtml.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                renderHtmlActionPerformed(evt);
            }
        });
        jMenu2.add(renderHtml);

        createCLIItem.setText("Show Command Line");
        createCLIItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                createCLIItemActionPerformed(evt);
            }
        });
        jMenu2.add(createCLIItem);

        menuBar.add(jMenu2);

        menuWindow.setText("Window");

        layoutMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_L, java.awt.event.InputEvent.CTRL_MASK));
        layoutMenuItem.setText("Arrange Layout");
        layoutMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                layoutMenuItemActionPerformed(evt);
            }
        });
        menuWindow.add(layoutMenuItem);
        menuWindow.add(jSeparator5);

        zoomInMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_PLUS, java.awt.event.InputEvent.CTRL_MASK));
        zoomInMenuItem.setText("Zoom In (Mouse Wheel Up)");
        zoomInMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                zoomInMenuItemActionPerformed(evt);
            }
        });
        menuWindow.add(zoomInMenuItem);

        zoomOutMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_MINUS, java.awt.event.InputEvent.CTRL_MASK));
        zoomOutMenuItem.setText("Zoom Out (Mouse Wheel Down)");
        zoomOutMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                zoomOutMenuItemActionPerformed(evt);
            }
        });
        menuWindow.add(zoomOutMenuItem);
        menuWindow.add(jSeparator15);

        thumbnailLayoutRadioButtonMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_0, java.awt.event.InputEvent.CTRL_MASK));
        thumbnailLayoutRadioButtonMenuItem.setText("Thumbnail Layout");
        thumbnailLayoutRadioButtonMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                thumbnailLayoutRadioButtonMenuItemActionPerformed(evt);
            }
        });
        menuWindow.add(thumbnailLayoutRadioButtonMenuItem);

        tinyLayoutRadioButtonMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_1, java.awt.event.InputEvent.CTRL_MASK));
        tinyLayoutRadioButtonMenuItem.setText("Tiny Layout");
        tinyLayoutRadioButtonMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                tinyLayoutRadioButtonMenuItemActionPerformed(evt);
            }
        });
        menuWindow.add(tinyLayoutRadioButtonMenuItem);

        smallLayoutRadioButtonMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_2, java.awt.event.InputEvent.CTRL_MASK));
        smallLayoutRadioButtonMenuItem.setText("Small Layout");
        smallLayoutRadioButtonMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                smallLayoutRadioButtonMenuItemActionPerformed(evt);
            }
        });
        menuWindow.add(smallLayoutRadioButtonMenuItem);

        mediumLayoutRadioButtonMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_3, java.awt.event.InputEvent.CTRL_MASK));
        mediumLayoutRadioButtonMenuItem.setText("Medium Layout");
        mediumLayoutRadioButtonMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                mediumLayoutRadioButtonMenuItemActionPerformed(evt);
            }
        });
        menuWindow.add(mediumLayoutRadioButtonMenuItem);

        largeLayoutRadioButtonMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_4, java.awt.event.InputEvent.CTRL_MASK));
        largeLayoutRadioButtonMenuItem.setText("Large Layout");
        largeLayoutRadioButtonMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                largeLayoutRadioButtonMenuItemActionPerformed(evt);
            }
        });
        menuWindow.add(largeLayoutRadioButtonMenuItem);
        menuWindow.add(jSeparator1);

        newWindowMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_N, java.awt.event.InputEvent.CTRL_MASK));
        newWindowMenuItem.setText("New Window");
        newWindowMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                newWindowMenuItemActionPerformed(evt);
            }
        });
        menuWindow.add(newWindowMenuItem);
        menuWindow.add(jSeparator6);

        view.setText("Look&Feel");
        menuWindow.add(view);

        menuBar.add(menuWindow);

        jMenu3.setText("Settings");

        nativeLAFCheckBoxMenuItem.setText("Native Look&Feel");
        nativeLAFCheckBoxMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                nativeLAFCheckBoxMenuItemActionPerformed(evt);
            }
        });
        jMenu3.add(nativeLAFCheckBoxMenuItem);

        menuBar.add(jMenu3);

        helpMenu.setText("Help");

        jMenuItem4.setText("Manual");
        jMenuItem4.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem4ActionPerformed(evt);
            }
        });
        helpMenu.add(jMenuItem4);

        helpForum.setText("Forum");
        helpForum.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                helpForumActionPerformed(evt);
            }
        });
        helpMenu.add(helpForum);

        downloadMenuItem.setText("Download Latest Version");
        downloadMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                downloadMenuItemActionPerformed(evt);
            }
        });
        helpMenu.add(downloadMenuItem);
        helpMenu.add(jSeparator14);

        aboutMenuItem.setText("About Jailer");
        aboutMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                aboutMenuItemActionPerformed(evt);
            }
        });
        helpMenu.add(aboutMenuItem);

        menuBar.add(helpMenu);

        setJMenuBar(menuBar);

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void exportDataMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_exportDataMenuItemActionPerformed
        desktop.createExtractionModel(true);
    }//GEN-LAST:event_exportDataMenuItemActionPerformed

    private void dataImportActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_dataImportActionPerformed
		if (!UIUtil.canRunJailer()) {
			return;
		}
		try {
            String sqlFile = UIUtil.choseFile(null, ".", "Data Import", ".sql", this, false, true);
            if (sqlFile != null) {
                DbConnectionDialog dcd = new DbConnectionDialog(this, dbConnectionDialog, DataBrowserContext.getAppName(), executionContext);
                if (dcd.connect("Data Import")) {
                    List<String> args = new ArrayList<String>();
                    args.add("import");
                    args.add(sqlFile);
                    dcd.addDbArgs(args);
                    ImportDialog importDialog = new ImportDialog(this, sqlFile, args, dbConnectionDialog.getUser(), dbConnectionDialog.getPassword(), true);
                    if (importDialog.isOk) {
                    	importDialog.fillCLIArgs(args);
						ResultConsumer consumer = new ResultConsumer() {
							@Override
							public void consume(boolean result, Throwable t) {
							}
							public void cleanUp() {
								try {
			                        if (desktop != null) {
			                            desktop.updateMenu();
			                            for (RowBrowser rb : desktop.getBrowsers()) {
			                                rb.browserContentPane.session = session;
			                                rb.browserContentPane.rows.clear();
			                            }
			                            for (RowBrowser rb : desktop.getRootBrowsers(false)) {
			                                rb.browserContentPane.reloadRows();
			                            }
			                        }
						        } catch (Exception e) {
						            UIUtil.showException(DataBrowser.this, "Error", e, session);
						        }
							}
						};
						UIUtil.runJailer(this, args, false, true,
							false, null, dcd.getUser(), dcd.getPassword(), null, null, false,
							true, false, false, false, consumer, executionContext);
                    }
                }
            }
        } catch (Exception e) {
            UIUtil.showException(this, "Error", e, session);
        }
    }//GEN-LAST:event_dataImportActionPerformed

    private void newBrowserjMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_newBrowserjMenuItemActionPerformed
        createFrame();
    }//GEN-LAST:event_newBrowserjMenuItemActionPerformed

    private void openTableButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_openTableButtonActionPerformed
    	if (tablesComboBox.getSelectedItem() != null) {
    		String tableName = tablesComboBox.getSelectedItem().toString();
    		desktop.addTableBrowser(null, null, datamodel.get().getTableByDisplayName(tableName), null, "", null, null, true);
    		switchToDesktop();
    	}
    }//GEN-LAST:event_openTableButtonActionPerformed

    private boolean ignoreTabChangeEvent = false;

    private void workbenchTabbedPaneStateChanged(javax.swing.event.ChangeEvent evt) {//GEN-FIRST:event_workbenchTabbedPaneStateChanged
		if (initialized) {
			if (workbenchTabbedPane.getSelectedComponent() == addSQLConsoleTab) {
				if (!ignoreTabChangeEvent) {
					try {
						createNewSQLConsole(getMetaDataSource(session));
					} catch (SQLException e) {
						UIUtil.showException(this, "Error", e);
					}
				}
			} else if (!(workbenchTabbedPane.getSelectedComponent() instanceof SQLConsole)) {
				for (SQLConsole sqlConsole: sqlConsoles) {
		        	if (sqlConsole.getDataHasChanged()) {
		        		try {
							desktop.reloadRoots();
						} catch (Exception e) {
							UIUtil.showException(this, "Error", e);
						}
		        		sqlConsole.setDataHasChanged(false);
		        	}
				}
	    		tableTreesTabbedPane.setSelectedComponent(navigationPanel);
	    		if (itemPerLimit.get(rowLimitStore) != null) {
	    			itemPerLimit.get(rowLimitStore).setSelected(true);
	    		}
	        } else {
	        	SQLConsole sqlConsole = getCurrentSQLConsole();
	        	if (sqlConsole != null) {
	        		tableTreesTabbedPane.setSelectedComponent(tablesCardPanel);
	        		sqlConsole.grabFocus();
	        		sqlConsole.update();
	        		if (itemPerLimit.get(sqlConsole.getRowLimit()) != null) {
		    			itemPerLimit.get(sqlConsole.getRowLimit()).setSelected(true);
		    		}
	        	}
	        }
			updateLoadSaveScriptMenuItemsState();
		}
    }//GEN-LAST:event_workbenchTabbedPaneStateChanged

    private void showDataModelMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_showDataModelMenuItemActionPerformed
    	metaDataDetailsPanel.tabbedPane.setSelectedComponent(dataModelPanel);
    }//GEN-LAST:event_showDataModelMenuItemActionPerformed

    private void consistencyCheckMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_consistencyCheckMenuItemActionPerformed
       	new ConstraintChecker(this, datamodel.get(), true, session) {
			@Override
			protected void openTableBrowser(Table source, String where) {
				workbenchTabbedPane.setSelectedComponent(desktopSplitPane);
	    		desktop.addTableBrowser(null, null, source, null, UIUtil.toSingleLineSQL(new BasicFormatterImpl().format(where)), null, null, true);
			}
       	};
    }//GEN-LAST:event_consistencyCheckMenuItemActionPerformed

    private void newWindowMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_newWindowMenuItemActionPerformed
        try {
            openNewDataBrowser(datamodel.get(), dbConnectionDialog, false, executionContext, null);
        } catch (Exception e) {
            UIUtil.showException(this, "Error", e, session);
        }
    }// GEN-LAST:event_newWindowMenuItemActionPerformed

    private void reconnectMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_reconnectMenuItemActionPerformed
        synchronized (this) {
            if (dbConnectionDialog.connect("Reconnect", true)) {
                try {
                    setConnection(dbConnectionDialog);
                } catch (Exception e) {
                    UIUtil.showException(this, "Error", e, session);
                }
            }
        }
    }// GEN-LAST:event_reconnectMenuItemActionPerformed

    private void thumbnailLayoutRadioButtonMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_thumbnailLayoutRadioButtonMenuItemActionPerformed
        desktop.rescaleLayout(Desktop.LayoutMode.THUMBNAIL, null);
        wheelzoomTip();
    }// GEN-LAST:event_thumbnailLayoutRadioButtonMenuItemActionPerformed

    private void jMenuItem3ActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_jMenuItem1ActionPerformed
        openNewTableBrowser(false);
    }// GEN-LAST:event_jMenuItem1ActionPerformed

    private void closeAllMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_cloaseAllMenuItemActionPerformed
        desktop.closeAll();
    }// GEN-LAST:event_cloaseAllMenuItemActionPerformed

    private void schemaMappingMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_schemaMappingMenuItemActionPerformed
        desktop.openSchemaMappingDialog(false);
    }// GEN-LAST:event_schemaMappingMenuItemActionPerformed

    private void jMenuItem4ActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_jMenuItem4ActionPerformed
        try {
            BrowserLauncher.openURL(new URI("http://jailer.sourceforge.net/data-browsing.html"), this);
        } catch (Exception e) {
            UIUtil.showException(this, "Error", e, session);
        }
    }// GEN-LAST:event_jMenuItem4ActionPerformed

    private void createExtractionModelMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_createExtractionModelMenuItemActionPerformed
        desktop.createExtractionModel(false);
    }// GEN-LAST:event_createExtractionModelMenuItemActionPerformed

    private void storeSessionItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_storeSessionItemActionPerformed
        desktop.storeSession((BookmarksPanel) null);
    }// GEN-LAST:event_storeSessionItemActionPerformed

    private void restoreSessionItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_restoreSessionItemActionPerformed
 		UISettings.s6 += 10000000;
 		desktop.restoreSession(null, null);
    }// GEN-LAST:event_restoreSessionItemActionPerformed

    private void tinyLayoutRadioButtonMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_tinyLayoutRadioButtonMenuItemActionPerformed
    	desktop.rescaleLayout(Desktop.LayoutMode.TINY, null);
        wheelzoomTip();
    }// GEN-LAST:event_tinyLayoutRadioButtonMenuItemActionPerformed

    private void wheelzoomTip() {
        TipDialog.showTip(this, "WHEELZOOM", "You can use the mouse-wheel to zoom in or out.");
    }

    private void smallLayoutRadioButtonMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_smallLayoutRadioButtonMenuItemActionPerformed
    	desktop.rescaleLayout(Desktop.LayoutMode.SMALL, null);
        wheelzoomTip();
    }// GEN-LAST:event_smallLayoutRadioButtonMenuItemActionPerformed

    private void mediumLayoutRadioButtonMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_mediumLayoutRadioButtonMenuItemActionPerformed
    	desktop.rescaleLayout(Desktop.LayoutMode.MEDIUM, null);
        wheelzoomTip();
    }// GEN-LAST:event_mediumLayoutRadioButtonMenuItemActionPerformed

    private void largeLayoutRadioButtonMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_largeLayoutRadioButtonMenuItemActionPerformed
    	desktop.rescaleLayout(Desktop.LayoutMode.LARGE, null);
        wheelzoomTip();
    }// GEN-LAST:event_largeLayoutRadioButtonMenuItemActionPerformed

    private void navigationTreeMouseClicked(java.awt.event.MouseEvent evt) {// GEN-FIRST:event_navigationTreeMouseClicked
        TreePath node = navigationTree.getPathForLocation(evt.getX(), evt.getY());
        if (node == null) {
            for (int x = navigationTree.getWidth(); x > 0; x -= 32) {
                node = navigationTree.getPathForLocation(x, evt.getY());
                if (node != null) {
                    break;
                }
            }
        }
        Object sel = null;
        Object selNode = null;
        RowBrowser rowBrowser = null;
        int row = 0;
        if (node != null) {
            sel = node.getLastPathComponent();
            if (sel instanceof DefaultMutableTreeNode) {
                selNode = ((DefaultMutableTreeNode) sel).getUserObject();
                if (selNode instanceof TreeNodeForRowBrowser) {
                    rowBrowser = ((TreeNodeForRowBrowser) selNode).rowBrowser;
                    row = ((TreeNodeForRowBrowser) selNode).rowIndex;
                }
            }
        }
    	if (evt.getButton() == MouseEvent.BUTTON1) {
    		if (rowBrowser != null) {
    			desktop.scrollToCenter(rowBrowser.internalFrame);
    			desktop.getiFrameStateChangeRenderer().onIFrameSelected(rowBrowser.internalFrame);
    			navigationTree.setSelectionPath(node);
    		}
    	}
    	if (evt.getButton() == MouseEvent.BUTTON3) {
            if (evt.getClickCount() == 1) {
                if (rowBrowser != null) {
                    navigationTree.setSelectionRow(row);
                    JPopupMenu popup = rowBrowser.browserContentPane.createPopupMenu(null, -1, 0, 0, false, false);
                    if (popup != null) {
                        JPopupMenu popup2 = rowBrowser.browserContentPane.createSqlPopupMenu(-1, 0, 0, true, navigationTreeScrollPane);
                        if (popup2.getComponentCount() > 0 && popup.getComponentCount() > 0) {
	                         popup.add(new JSeparator());
	                    }
                        for (Component c : popup2.getComponents()) {
                            popup.add(c);
                        }
                        UIUtil.fit(popup);
                        UIUtil.showPopup(evt.getComponent(), evt.getX(), evt.getY(), popup);
                    }
                }
            }
        }
    }// GEN-LAST:event_navigationTreeMouseClicked

    private void layoutMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_layoutMenuItemActionPerformed
        arrangeLayout(true);
    }// GEN-LAST:event_layoutMenuItemActionPerformed

    public void arrangeLayout(boolean scrollToCenter) {
    	arrangeLayout(scrollToCenter, Desktop.noArrangeLayoutOnNewTableBrowserWithAnchor? null : anchorManager.getNewestBrowser());
    	anchorManager.setNewestBrowser(null);
    }

    public void arrangeLayout(boolean scrollToCenter, RowBrowser anchor) {
		UIUtil.setWaitCursor(this);
        try {
            desktop.layoutBrowser(null, scrollToCenter, anchor);
        } finally {
        	UIUtil.resetWaitCursor(this);
        }
    }

    private void jScrollPane1MouseWheelMoved(java.awt.event.MouseWheelEvent evt) {// GEN-FIRST:event_jScrollPane1MouseWheelMoved
		long currentTime = System.currentTimeMillis();
		desktop.startRescaleMode(currentTime, evt.getX(), evt.getY(), evt.getComponent());
		desktop.onMouseWheelMoved(evt.getX(), evt.getY(), evt.getWheelRotation(), evt.getComponent(), currentTime);
        desktop.onMouseWheelMoved(evt, jScrollPane1, currentTime);
    }// GEN-LAST:event_jScrollPane1MouseWheelMoved

    private void openNewTableBrowser(boolean offerAlternatives) {
        new NewTableBrowser(this, datamodel.get(), offerAlternatives) {
            @Override
            void openTableBrowser(String tableName) {
                desktop.addTableBrowser(null, null, datamodel.get().getTableByDisplayName(tableName), null, "", null, null, true);
        		switchToDesktop();
           }

            @Override
            void openDatabaseAnalyzer() {
                updateDataModel();
            }

            @Override
            void restoreSession() {
                desktop.restoreSession(null, null);
            }
        };
    }

    private void helpForumActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_helpForumActionPerformed
        try {
            BrowserLauncher.openURL(new URI("https://sourceforge.net/p/jailer/discussion/"), this);
        } catch (Exception e) {
            UIUtil.showException(this, "Error", e, session);
        }
    }// GEN-LAST:event_helpForumActionPerformed

    private void aboutMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_jMenuItem4ActionPerformed
        About about = new About(this, true);
        about.setTitle(DataBrowserContext.getAppName(false));
        about.pack();
        about.setLocation(getLocation().x + (getSize().width - about.getPreferredSize().width) / 2,
                getLocation().y + (getSize().height - about.getPreferredSize().height) / 2);
        about.setVisible(true);
    }// GEN-LAST:event_jMenuItem4ActionPerformed

    private void analyseMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_analyseMenuItemActionPerformed
        updateDataModel();
    }// GEN-LAST:event_analyseMenuItemActionPerformed

    private void dataModelEditorjMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_dataModelEditorjMenuItemActionPerformed
        openDataModelEditor(false);
    }// GEN-LAST:event_dataModelEditorjMenuItemActionPerformed

    /**
     * File in which plaf-setting is stored.
     */
    private static final String PLAFSETTING = ".plaf2.ui";

    /**
     * @param args the command line arguments
     */
    public static void main(String args[]) {
        try {
            start(args);
        } catch (Throwable t) {
        	t.printStackTrace();
        	UIUtil.showException(null, "Error", t);
        }
    }

    /**
     * @param args
     *            the command line arguments
     */
    private static void start(String args[]) {
        try {
        	args = Environment.init(args);
        } catch (Throwable e) {
        	e.printStackTrace();
			UIUtil.showException(null, "Error", e);
			return;
		}

        // turn off logging for prefuse library
        try {
            Logger.getLogger("prefuse").setLevel(Level.OFF);
        } catch (Exception e1) {
            e1.printStackTrace();
        }
        try {
            CommandLineInstance.init(args);
        } catch (Exception e) {
        	e.printStackTrace();
            UIUtil.showException(null, "Illegal arguments", e);
            return;
        }
        try {
            System.setProperty("db2.jcc.charsetDecoderEncoder", "3");
        } catch (Exception e) {
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
            	try {
            		if (!Boolean.TRUE.equals(UISettings.restore(UISettings.USE_NATIVE_PLAF))) {
            			try {
            				for (LookAndFeelInfo info : UIManager.getInstalledLookAndFeels()) {
	                            if ("Nimbus".equals(info.getName())) {
	                            	UIManager.put("nimbusBase", new Color(66, 118, 187)); // orig. color: 51, 98, 140
	                				UIManager.setLookAndFeel(info.getClassName());
	                                Environment.nimbus = true;
	                                break;
	                            }
	                        }
            				initMacKeyStrokes();
	                        ((InputMap) UIManager.get("Button.focusInputMap")).put(KeyStroke.getKeyStroke("pressed ENTER"), "pressed");
	                        ((InputMap) UIManager.get("Button.focusInputMap")).put(KeyStroke.getKeyStroke("released ENTER"), "released");
	                        Object dSize = UIManager.get("SplitPane.dividerSize");
	                        if (Integer.valueOf(10).equals(dSize)) {
	                        	UIManager.put("SplitPane.dividerSize", Integer.valueOf(14));
	                        }

		                    if (UIManager.get("InternalFrame:InternalFrameTitlePane[Enabled].textForeground") instanceof Color) {
		                    	UIManager.put("InternalFrame:InternalFrameTitlePane[Enabled].textForeground", Color.BLUE);
		                    }
	                    } catch (Exception x) {
	                    	UIUtil.showException(null, "Error", x);
	                    }
            		}
            		createFrame();
            	} catch (Exception e) {
                	UIUtil.showException(null, "Error", e);
                }
            }

			private void initMacKeyStrokes() {
				try {
					if (System.getProperty("os.name", "").startsWith("Mac")) {
						addOSXKeyStrokes((InputMap) UIManager.get("EditorPane.focusInputMap"));
						addOSXKeyStrokes((InputMap) UIManager.get("FormattedTextField.focusInputMap"));
						addOSXKeyStrokes((InputMap) UIManager.get("PasswordField.focusInputMap"));
						addOSXKeyStrokes((InputMap) UIManager.get("TextField.focusInputMap"));
						addOSXKeyStrokes((InputMap) UIManager.get("TextPane.focusInputMap"));
						addOSXKeyStrokes((InputMap) UIManager.get("TextArea.focusInputMap"));
						addOSXKeyStrokesList((InputMap) UIManager.get("Table.ancestorInputMap"));
						addOSXKeyStrokesList((InputMap) UIManager.get("Tree.focusInputMap"));
					}
				} catch (Throwable t) {
					// ignore
				}
			}

			private void addOSXKeyStrokes(InputMap inputMap) {
				inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_C, KeyEvent.META_DOWN_MASK),
						DefaultEditorKit.copyAction);
				inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_X, KeyEvent.META_DOWN_MASK),
						DefaultEditorKit.cutAction);
				inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_V, KeyEvent.META_DOWN_MASK),
						DefaultEditorKit.pasteAction);
				inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_A, KeyEvent.META_DOWN_MASK),
						DefaultEditorKit.selectAllAction);
			}
			private void addOSXKeyStrokesList(InputMap inputMap) {
				inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_C, KeyEvent.META_DOWN_MASK), "copy");
				inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_A, KeyEvent.META_DOWN_MASK), "selectAll");
			}
        });
    }

    private static DataBrowser openNewDataBrowser(DataModel datamodel, DbConnectionDialog dbConnectionDialog, boolean maximize, ExecutionContext executionContext, DataBrowser theDataBrowser) throws Exception {
        final DataBrowser dataBrowser = theDataBrowser != null? theDataBrowser : new DataBrowser(datamodel, null, "", null, ExecutionContext.getSchemaMapping(CommandLineInstance.getInstance().rawschemamapping), false, executionContext);
        dataBrowser.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        dataBrowser.setVisible(true);
        dataBrowser.setExtendedState(Frame.MAXIMIZED_BOTH);

        if (dbConnectionDialog == null) {
            dbConnectionDialog = new DbConnectionDialog(dataBrowser, DataBrowserContext.getAppName(), null, executionContext);
        } else {
            dbConnectionDialog = new DbConnectionDialog(dataBrowser, dbConnectionDialog, DataBrowserContext.getAppName(), executionContext);
        }
        dbConnectionDialog.autoConnect();
        if (dbConnectionDialog.isConnected || dbConnectionDialog.connect(DataBrowserContext.getAppName(true))) {
        	try {
                dataBrowser.setConnection(dbConnectionDialog);
        	} catch (Throwable t) {
            	UIUtil.showException(null, "Error", t);
            	dataBrowser.dispose();
                return dataBrowser;
            }
    		if (dataBrowser.session != null) {
	            dataBrowser.askForDataModel();
	            dataBrowser.desktop.openSchemaMappingDialog(true);
	            dataBrowser.updateStatusBar();
            }
        } else {
            if (dbConnectionDialog.isConnected) {
                dataBrowser.setConnection(dbConnectionDialog);
            } else {
            	dataBrowser.dbConnectionDialog = dbConnectionDialog;
            }
            dataBrowser.dispose();
            return dataBrowser;
        }
        String bmName = CommandLineInstance.getInstance().bookmark;
		final File bmFile;
		final boolean restoreLastSession;
		if ("".equals(bmName)) {
			bmFile = null;
			restoreLastSession = true;
		} else {
			bmFile = BookmarksPanel.getBookmarksFile(bmName, dataBrowser.executionContext);
			restoreLastSession = false;
		}
        UIUtil.invokeLater(3, new Runnable() {
			@Override
			public void run() {
		        dataBrowser.toFront();
		        if (restoreLastSession) {
		        	dataBrowser.desktop.restoreSession(null, Environment.newFile(LAST_SESSION_FILE));
		        } else if (bmFile != null) {
		        	dataBrowser.desktop.restoreSession(null, bmFile);
		     		BookmarksPanel.setLastUsedBookmark(bmFile.getName(), dataBrowser.executionContext);
		     		bmFile.setLastModified(System.currentTimeMillis());
					new BookmarksPanel(dataBrowser, dataBrowser.bookmarkMenu, dataBrowser.desktop, dataBrowser.executionContext).updateBookmarksMenu();
		        }
			}
		});
        return dataBrowser;
    }

    private static void createFrame() {
        DataModelManagerDialog dataModelManagerDialog = new DataModelManagerDialog(DataBrowserContext.getAppName(true)
                + " - Relational Data Browser", false, "B") {
            @Override
            protected void onSelect(final DbConnectionDialog connectionDialog, final ExecutionContext executionContext) {
                try {
                    final DataModel datamodel;
                    Map<String, String> schemaMapping = ExecutionContext.getSchemaMapping(CommandLineInstance.getInstance().rawschemamapping);
					datamodel = new DataModel(null, null, schemaMapping, null, new PrimaryKeyFactory(executionContext), executionContext, true, null);
                	final DataBrowser databrowser = new DataBrowser(datamodel, null, "", null, schemaMapping, false, executionContext);
                    UIUtil.invokeLater(new Runnable() {
						@Override
						public void run() {
			                try {
								openNewDataBrowser(datamodel, connectionDialog, true, executionContext, databrowser);
								CommandLineInstance.clear();
			                } catch (Exception e) {
			                    UIUtil.showException(null, "Error", e);
			                }
						}
					});
                } catch (Exception e) {
                    UIUtil.showException(null, "Error", e);
                    UIUtil.checkTermination();
                }
            }
			@Override
			protected void onLoadExtractionmodel(String modelFile, ExecutionContext executionContext2) {
				// nothing to do
			}
			private static final long serialVersionUID = 1L;
        };
        dataModelManagerDialog.start();
    }

    /**
     * Opens the data model editor.
     */
    private void openDataModelEditor(boolean merge) {
        try {
        	UIUtil.setWaitCursor(this);
            String modelname = datamodel == null || datamodel.get() == null ? DataModel.DEFAULT_NAME : datamodel.get().getName();
            DataModelEditor dataModelEditor = new DataModelEditor(this, merge, false, null, null, null, modelname, null, dbConnectionDialog, executionContext);
            dataModelEditor.setVisible(true);
            removeMetaDataSource(session);
            desktop.reloadDataModel(desktop.schemaMapping);
            dataModelViewFrame = null;
            updateDataModelView(null);
            updateStatusBar();
            askForDataModel();
        } catch (Exception e) {
            UIUtil.showException(this, "Error", e, session);
        } finally {
        	UIUtil.resetWaitCursor(this);
        }
    }

    private void updateDataModel() {
    	updateDataModel(null, false, false);
    }

    private void updateDataModel(String schemaName, boolean withViews, boolean withSynonyms) {
		if (!UIUtil.canRunJailer()) {
			return;
		}
		try {
			JDBCMetaDataBasedModelElementFinder.privilegedSessionProvider = new PrivilegedSessionProviderDialog.Provider(this);

            List<String> args = new ArrayList<String>();
            args.add("build-model-wo-merge");
            dbConnectionDialog.addDbArgs(args);

            AnalyseOptionsDialog analyseOptionsDialog = new AnalyseOptionsDialog(this, datamodel == null ? null : datamodel.get(), executionContext);
            analyseOptionsDialog.setInitiallyWithViews(withViews);
            analyseOptionsDialog.setInitiallyWithSynonyms(withSynonyms);
            boolean[] isDefaultSchema = new boolean[1];
            if (analyseOptionsDialog.edit(dbConnectionDialog, schemaName == null? null : Quoting.staticUnquote(schemaName), isDefaultSchema, dbConnectionDialog.currentConnection.user)) {
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
                if (UIUtil.runJailer(this, args, false, true, true, null, dbConnectionDialog.getUser(), dbConnectionDialog.getPassword(), null, null, false, true, false, executionContext)) {
                    ModelBuilder.assocFilter = null;
                    String modelname = datamodel == null || datamodel.get() == null ? DataModel.DEFAULT_NAME : datamodel.get().getName();
                    DataModelEditor dataModelEditor = new DataModelEditor(this, true, analyseOptionsDialog.isRemoving(), null,
                            analyseOptionsDialog.getTableLineFilter(), analyseOptionsDialog.getAssociationLineFilter(), modelname,
                            schema == null? dbConnectionDialog.getName() : schema, dbConnectionDialog, executionContext);
                    if (dataModelEditor.dataModelHasChanged()) {
                        dataModelEditor.setVisible(true);
                    }
                    removeMetaDataSource(session);
                    desktop.reloadDataModel(desktop.schemaMapping);
                    dataModelViewFrame = null;
                    updateDataModelView(null);
                    updateStatusBar();
                    askForDataModel();
                }
            } else {
                askForDataModel();
            }
        } catch (Exception e) {
            UIUtil.showException(this, "Error", e, session);
        } finally {
            ModelBuilder.assocFilter = null;
            JDBCMetaDataBasedModelElementFinder.privilegedSessionProvider = null;
        }
    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JMenuItem aboutMenuItem;
    private javax.swing.JMenuItem addBookmarkMenuItem;
    private javax.swing.JPanel addSQLConsoleTab;
    private javax.swing.JMenuItem analyseMenuItem;
    private javax.swing.JMenuItem analyseSQLMenuItem1;
    private javax.swing.JLabel associatedWith;
    private javax.swing.JMenu bookmarkMenu;
    private javax.swing.JPanel borderBrowserPanel;
    private javax.swing.JPanel borderBrowserTabPane;
    javax.swing.JMenuItem checkPKMenuItem;
    private javax.swing.JMenuItem closeAllMenuItem;
    private javax.swing.JPanel closurePanel;
    private javax.swing.JMenuItem columnOrderItem;
    public javax.swing.JLabel connectivityState;
    private javax.swing.JMenuItem consistencyCheckMenuItem;
    private javax.swing.JMenuItem consistencyCheckMenuItem1;
    private javax.swing.JPanel consoleDummyPanel;
    public javax.swing.JPanel controlPanel;
    private javax.swing.JMenuItem createCLIItem;
    private javax.swing.JMenuItem createExtractionModelMenuItem;
    private javax.swing.JMenuItem dataImport;
    private javax.swing.JMenuItem dataModelEditorjMenuItem;
    private javax.swing.JPanel dataModelPanel;
    private javax.swing.JLabel dependsOn;
    private javax.swing.JSplitPane desktopSplitPane;
    private javax.swing.JButton downloadButton;
    private javax.swing.JMenuItem downloadMenuItem;
    private javax.swing.JPanel dummy;
    private javax.swing.JMenuItem editBookmarkMenuItem;
    private javax.swing.JMenuItem exitMenuItem;
    private javax.swing.JMenuItem exportDataMenuItem;
    private javax.swing.JLabel hasDependent;
    private javax.swing.JMenuItem helpForum;
    private javax.swing.JMenu helpMenu;
    private javax.swing.JPanel hiddenPanel;
    private javax.swing.JLabel ignored;
    private javax.swing.JButton jButton1;
    private javax.swing.JInternalFrame jInternalFrame1;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel10;
    private javax.swing.JLabel jLabel11;
    private javax.swing.JLabel jLabel12;
    private javax.swing.JLabel jLabel13;
    private javax.swing.JLabel jLabel14;
    private javax.swing.JLabel jLabel19;
    private javax.swing.JLabel jLabel20;
    private javax.swing.JLabel jLabel21;
    private javax.swing.JLabel jLabel22;
    private javax.swing.JLabel jLabel26;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JLayeredPane jLayeredPane1;
    private javax.swing.JLayeredPane jLayeredPane2;
    private javax.swing.JMenu jMenu1;
    private javax.swing.JMenu jMenu2;
    private javax.swing.JMenu jMenu3;
    private javax.swing.JMenuItem jMenuItem3;
    private javax.swing.JMenuItem jMenuItem4;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel11;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JPanel jPanel7;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JScrollPane jScrollPane3;
    private javax.swing.JPopupMenu.Separator jSeparator1;
    private javax.swing.JPopupMenu.Separator jSeparator10;
    private javax.swing.JPopupMenu.Separator jSeparator11;
    private javax.swing.JPopupMenu.Separator jSeparator12;
    private javax.swing.JPopupMenu.Separator jSeparator13;
    private javax.swing.JPopupMenu.Separator jSeparator14;
    private javax.swing.JPopupMenu.Separator jSeparator15;
    private javax.swing.JPopupMenu.Separator jSeparator2;
    private javax.swing.JPopupMenu.Separator jSeparator4;
    private javax.swing.JPopupMenu.Separator jSeparator5;
    private javax.swing.JPopupMenu.Separator jSeparator6;
    private javax.swing.JPopupMenu.Separator jSeparator7;
    private javax.swing.JPopupMenu.Separator jSeparator8;
    private javax.swing.JPopupMenu.Separator jSeparator9;
    private javax.swing.JSplitPane jSplitPane1;
    private javax.swing.JSplitPane jSplitPane4;
    private javax.swing.JTable jTable1;
    private javax.swing.JTree jTree1;
    private javax.swing.JMenu jviewMenu;
    private javax.swing.JRadioButtonMenuItem largeLayoutRadioButtonMenuItem;
    private javax.swing.JPanel layeredPaneContent;
    private javax.swing.JMenuItem layoutMenuItem;
    private javax.swing.JPanel legende;
    private javax.swing.JPanel legende1;
    private javax.swing.JPanel legende2;
    private javax.swing.JMenuItem loadScriptMenuItem;
    private javax.swing.JRadioButtonMenuItem mediumLayoutRadioButtonMenuItem;
    private javax.swing.JMenuBar menuBar;
    private javax.swing.JMenu menuTools;
    private javax.swing.JMenu menuWindow;
    private javax.swing.JPanel metaDataViewPanel;
    private javax.swing.JLabel modelName;
    private javax.swing.JLabel modelPath;
    private javax.swing.JCheckBoxMenuItem nativeLAFCheckBoxMenuItem;
    private javax.swing.JPanel navigationPanel;
    private javax.swing.JTree navigationTree;
    private javax.swing.JScrollPane navigationTreeScrollPane;
    private javax.swing.JMenuItem newBrowserjMenuItem;
    private javax.swing.JMenuItem newWindowMenuItem;
    private javax.swing.JButton openTableButton;
    private javax.swing.JPanel outLinePanel;
    private javax.swing.JMenuItem reconnectMenuItem;
    private javax.swing.JButton refreshButton;
    private javax.swing.JMenuItem renderHtml;
    private javax.swing.JMenuItem restoreSessionItem;
    private javax.swing.JMenu rowLimitMenu;
    private javax.swing.JMenuItem saveScriptAsMenuItem;
    private javax.swing.JMenuItem saveScriptMenuItem;
    private javax.swing.JMenuItem schemaMappingMenuItem;
    private javax.swing.JLabel schemaName;
    private javax.swing.JPanel schemaNamePanel;
    private javax.swing.JCheckBoxMenuItem showDataModelMenuItem;
    private javax.swing.JRadioButtonMenuItem smallLayoutRadioButtonMenuItem;
    private javax.swing.JMenuItem storeSessionItem;
    private javax.swing.JTabbedPane tableTreesTabbedPane;
    private javax.swing.JPanel tablesCardPanel;
    private javax.swing.JPanel tablesPanel;
    private javax.swing.JRadioButtonMenuItem thumbnailLayoutRadioButtonMenuItem;
    private javax.swing.JRadioButtonMenuItem tinyLayoutRadioButtonMenuItem;
    private javax.swing.JLabel titleLabel;
    private javax.swing.JLabel updateInfoLabel;
    private javax.swing.JPanel updateInfoPanel;
    private javax.swing.JMenu view;
    private javax.swing.JTabbedPane workbenchTabbedPane;
    private javax.swing.JMenuItem zoomInMenuItem;
    private javax.swing.JMenuItem zoomOutMenuItem;
    // End of variables declaration//GEN-END:variables

    private JToggleButton searchButton;

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
        try {
            if (datamodel.get().getTables().isEmpty()) {
                switch (JOptionPane.showOptionDialog(this, "Data model \"" + DataModelManager.getModelDetails(DataModelManager.getCurrentModelSubfolder(executionContext), executionContext).a
                        + "\" is empty.", DataBrowserContext.getAppName(true), JOptionPane.YES_NO_OPTION, JOptionPane.INFORMATION_MESSAGE, null, new Object[] {
                        "Analyze Database", "Data Model Editor" }, null)) {
                case 0:
                    updateDataModel();
                    break;
                case 1:
                    openDataModelEditor(false);
                    break;
                }
            } else if (!new File(DataModel.getColumnsFile(executionContext)).exists()) {
                switch (JOptionPane.showOptionDialog(this, "No column definition found.", DataBrowserContext.getAppName(true), JOptionPane.YES_NO_OPTION,
                        JOptionPane.INFORMATION_MESSAGE, null, new Object[] { "Analyze Database", "Data Model Editor" }, null)) {
                case 0:
                    updateDataModel();
                    break;
                case 1:
                    openDataModelEditor(false);
                    break;
                }
            }
        } catch (Exception e) {
            UIUtil.showException(this, "Error", e, session);
        }
    }

    private void updateIFramesBar() {
        updateNavigationTree();
        updateBorderBrowser();
        updateHiddenPanel();

        // iFramesPanel is obsolete
        return;
    }

    private void updateHiddenPanel() {
        if (desktop == null) {
            return;
        }

        hiddenPanel.removeAll();
        hiddenPanel.setVisible(false);

        int num = desktop.getAllFrames().length;
        if (num == 0) {
            jPanel1.revalidate();
            return;
        }
        int COLUMNS = 7;
        int y = 1;

        GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = y;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1;
        JPanel iFramesRowPanel = new JPanel();
        iFramesRowPanel.setLayout(new GridBagLayout());
        hiddenPanel.add(iFramesRowPanel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = COLUMNS + 2;
        gridBagConstraints.gridy = y;
        gridBagConstraints.weightx = 1;
        iFramesRowPanel.add(new JLabel(" "), gridBagConstraints);

        int x = 1;
        boolean visible = false;
        for (final RowBrowser rb : desktop.getBrowsers()) {
            if (!rb.isHidden()) {
                continue;
            }
            visible = true;
            if (++x > COLUMNS) {
                x = 1;
                ++y;
                gridBagConstraints = new java.awt.GridBagConstraints();
                gridBagConstraints.gridx = 1;
                gridBagConstraints.gridy = y;
                gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
                gridBagConstraints.anchor = GridBagConstraints.WEST;
                gridBagConstraints.weightx = 1;
                iFramesRowPanel = new JPanel();
                iFramesRowPanel.setLayout(new GridBagLayout());
                hiddenPanel.add(iFramesRowPanel, gridBagConstraints);
                gridBagConstraints = new java.awt.GridBagConstraints();
                gridBagConstraints.gridx = COLUMNS + 2;
                gridBagConstraints.gridy = y;
                gridBagConstraints.weightx = 1;
                iFramesRowPanel.add(new JLabel(" "), gridBagConstraints);
            }

            final JToggleButton toggleButton = new JToggleButton();
            toggleButton.setText(rb.internalFrame.getTitle());
            toggleButton.setIcon(UIUtil.jailerLogo16);
            toggleButton.setSelected(false);

            toggleButton.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    toggleButton.setSelected(true);
                    rb.setHidden(false);
                }
            });
            gridBagConstraints = new java.awt.GridBagConstraints();
            gridBagConstraints.gridx = x;
            gridBagConstraints.gridy = y;
            iFramesRowPanel.add(toggleButton, gridBagConstraints);

            hiddenPanel.setVisible(visible);
        }

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new Insets(0, 0, 4, 0);

        jPanel1.revalidate();
    }

	private static final class BrowserAssociationModel extends DefaultAssociationModel {
        private final RowBrowser rowBrowser;

        public BrowserAssociationModel(RowBrowser rowBrowser, Association association) {
            super(association);
            this.rowBrowser = rowBrowser;
        }

        @Override
        public String getSourceName() {
            return rowBrowser.internalFrame.getTitle();
        }

        @Override
        public boolean equals(Object other) {
        	if (other == null) {
        		return false;
        	}
            if (other.getClass().equals(getClass())) {
                BrowserAssociationModel otherModel = (BrowserAssociationModel) other;
                return rowBrowser == otherModel.rowBrowser && association == otherModel.association;
            }
            return false;
        }

        @Override
        public int hashCode() {
            return rowBrowser.hashCode() + 3 * association.hashCode();
        }

        public RowBrowser getRowBrowser() {
            return rowBrowser;
        }

        public Association getAssociation() {
            return association;
        }
    }

    private boolean disableBorderBrowserUpdates = false;

    private void updateBorderBrowser() {
        if (disableBorderBrowserUpdates || metaDataDetailsPanel.tabbedPane.getSelectedComponent() != borderBrowserTabPane) {
            return;
        }
        try {
        	UIUtil.setWaitCursor(this);

            Collection<AssociationModel> model = new ArrayList<AssociationModel>();
            if (desktop != null) {
                titleLabel.setText(" Related Rows");
                List<RowBrowser> allChildren = new ArrayList<RowBrowser>();
                for (RowBrowser rb : desktop.getBrowsers()) {
                    if (rb.internalFrame == desktop.getSelectedFrame() && !rb.isHidden()) {
                        allChildren.add(rb);
                        allChildren.addAll(collectChildren(rb));
                        titleLabel.setText(" Related Rows of Subtree " + rb.internalFrame.getTitle());
                        break;
                    }
                }
                for (RowBrowser rb : allChildren) {
                    if (rb.browserContentPane.table != null) {
                        Set<Association> associations = new HashSet<Association>(rb.browserContentPane.table.associations);
                        for (RowBrowser c : desktop.getChildBrowsers(rb, false)) {
                            if (c.browserContentPane.association != null) {
                                associations.remove(c.browserContentPane.association);
                            }
                        }
                        if (rb.browserContentPane.association != null && rb.parent != null) {
                            if (allChildren.contains(rb.parent)) {
                                associations.remove(rb.browserContentPane.association.reversalAssociation);
                            }
                        }
                        for (Association association : associations) {
                            model.add(new BrowserAssociationModel(rb, association));
                        }
                    }
                }
            }

            borderBrowser.setModel(model);
        } finally {
        	UIUtil.resetWaitCursor(this);
        }
    }

    private ExtractionModelFrame dataModelViewFrame;
    private Table lastFocusTable = null;
    private Table currentTableInDMV = null;

	private void updateDataModelView(final Table table) {
		if (table != null) {
			lastFocusTable = table;
		}
		if (metaDataDetailsPanel.tabbedPane.getSelectedComponent() != dataModelPanel) {
			return;
		}
		UIUtil.invokeLater(new Runnable() {
			@Override
			public void run() {
				try {
					UIUtil.setWaitCursor(DataBrowser.this);
					if (dataModelViewFrame == null) {
						dataModelViewFrame = ExtractionModelFrame.createFrame(null, false, false, null, executionContext);
						JComponent graphViewContainer = dataModelViewFrame.tearOutGraphViewContainer(DataBrowser.this);
						dataModelPanel.removeAll();
						dataModelPanel.add(graphViewContainer);
					}
                    if (table != null) {
                    	dataModelViewFrame.select(table);
                    } else {
                    	Table toSelect = null;
                    	if (lastFocusTable != null && tableTreesTabbedPane.getSelectedComponent() == tablesCardPanel) {
                    		toSelect = datamodel.get().getTable(lastFocusTable.getName());
                    	}
                    	if (toSelect == null) {
	                    	TreePath selectionPath = navigationTree.getSelectionPath();
		                    toSelect = datamodel.get().getTables().isEmpty()? null : datamodel.get().getTables().iterator().next();
		                    if (selectionPath != null) {
								Object lastPathComponent = selectionPath.getLastPathComponent();
			                    if (lastPathComponent != null && lastPathComponent instanceof DefaultMutableTreeNode) {
			                        Object userObject = ((DefaultMutableTreeNode) lastPathComponent).getUserObject();
			                        if (userObject instanceof TreeNodeForRowBrowser) {
			                        	Table table = ((TreeNodeForRowBrowser) userObject).rowBrowser.browserContentPane.table;
			                        	if (!(table instanceof SqlStatementTable)) {
			                        		toSelect = table;
			                        	}
			                        }
			                    }
		                    }
                    	}
	                    if (toSelect != null && currentTableInDMV != toSelect) {
	                    	dataModelViewFrame.select(toSelect);
	                    	currentTableInDMV = toSelect;
	                    }
                    }
				} catch (IOException e) {
					UIUtil.showException(DataBrowser.this, "Error", e);
				} finally {
					UIUtil.resetWaitCursor(DataBrowser.this);
				}
			}
		});
	}

    protected void resolveSelection(Collection<AssociationModel> selection) {
        try {
        	UIUtil.setWaitCursor(this);

            disableBorderBrowserUpdates = true;
            JInternalFrame currentSelection = desktop.getSelectedFrame();
            for (AssociationModel a : selection) {
                BrowserAssociationModel associationModel = (BrowserAssociationModel) a;
                desktop.addTableBrowser(associationModel.getRowBrowser(), associationModel.getRowBrowser(), associationModel.getAssociation().destination, associationModel.getAssociation(),
                        "", null, null, true);
            }
            if (currentSelection != null) {
                try {
                    currentSelection.setSelected(true);
                    desktop.getiFrameStateChangeRenderer().onIFrameSelected(currentSelection);
                } catch (PropertyVetoException e) {
                    // ignore
                }
            }
            arrangeLayout(true);
        } finally {
        	UIUtil.resetWaitCursor(this);
            disableBorderBrowserUpdates = false;
            updateBorderBrowser();
        }
    }

    private List<RowBrowser> collectChildren(RowBrowser rb) {
        List<RowBrowser> result = new ArrayList<Desktop.RowBrowser>();
        for (RowBrowser c : desktop.getChildBrowsers(rb, true)) {
            result.add(c);
        }
        for (RowBrowser c : desktop.getChildBrowsers(rb, true)) {
            result.addAll(collectChildren(c));
        }
        return result;
    }

    private Map<JInternalFrame, TreeNodeForRowBrowser> treeNodeByIFrame = new HashMap<JInternalFrame, DataBrowser.TreeNodeForRowBrowser>();
    private TreeSelectionListener navigationTreeListener = null;

    private class TreeNodeForRowBrowser {
        public final RowBrowser rowBrowser;
        public final int rowIndex;
        private final String title;

        public TreeNodeForRowBrowser(RowBrowser rowBrowser, int rowIndex) {
            this.rowBrowser = rowBrowser;
            this.rowIndex = rowIndex;
            this.title = " " + rowBrowser.internalFrame.getTitle() + "  ";
            treeNodeByIFrame.put(rowBrowser.internalFrame, this);
        }

        @Override
		public String toString() {
            return title;
        }
    }

    private void updateNavigationTree() {
        if (navigationTreeListener != null) {
            navigationTree.getSelectionModel().removeTreeSelectionListener(navigationTreeListener);
        }

        ConnectionInfo connection = dbConnectionDialog != null ? dbConnectionDialog.currentConnection : null;
        DefaultMutableTreeNode root = new DefaultMutableTreeNode(connection != null ? connection.alias : " ");

        treeNodeByIFrame.clear();

        int[] count = new int[1];
        count[0] = 1;
        if (desktop != null) {
            for (RowBrowser rb : desktop.getRootBrowsers(true)) {
                DefaultMutableTreeNode node = new DefaultMutableTreeNode(new TreeNodeForRowBrowser(rb, count[0]++));
                root.add(node);
                addChildNodes(node, rb, count);
            }
        }
        DefaultTreeModel treeModel = new DefaultTreeModel(root);
        navigationTree.setModel(treeModel);
        for (int i = 0; i < count[0]; ++i) {
            navigationTree.expandRow(i);
        }
        JInternalFrame activeFrame = desktop != null ? desktop.getSelectedFrame() : null;
        if (activeFrame != null) {
            TreeNodeForRowBrowser node = treeNodeByIFrame.get(activeFrame);
            if (node != null) {
                navigationTree.setSelectionRow(node.rowIndex);
                Rectangle bounds = navigationTree.getRowBounds(node.rowIndex);
                navigationTree.scrollRectToVisible(new Rectangle(bounds.x, bounds.y, 1, bounds.height));
                for (RowBrowser rb : desktop.getRootBrowsers(true)) {
                	if (rb.internalFrame == activeFrame) {
                		if (rb.getMDTable() != null && metaDataPanel != null) {
                			metaDataPanel.select(rb.getMDTable());
                		}
                		break;
                	}
                }
            }
        }

        navigationTreeListener = new TreeSelectionListener() {
            @Override
            public void valueChanged(TreeSelectionEvent e) {
                if (e.getPath() != null) {
                    Object lastPathComponent = e.getPath().getLastPathComponent();
                    if (lastPathComponent != null && lastPathComponent instanceof DefaultMutableTreeNode) {
                        Object userObject = ((DefaultMutableTreeNode) lastPathComponent).getUserObject();
                        if (userObject instanceof TreeNodeForRowBrowser) {
                            try {
                                JInternalFrame iFrame = ((TreeNodeForRowBrowser) userObject).rowBrowser.internalFrame;
                                desktop.scrollToCenter(iFrame);
                                desktop.getiFrameStateChangeRenderer().onIFrameSelected(iFrame);
                                iFrame.setSelected(true);
                                iFrame.grabFocus();
                            } catch (PropertyVetoException e1) {
                                // ignore
                            }
                            return;
                        } else {
                            searchButton.doClick(1);
                        }
                    }
                }
                updateNavigationTree();
            }
        };

        navigationTree.getSelectionModel().addTreeSelectionListener(navigationTreeListener);
    }

    private void addChildNodes(DefaultMutableTreeNode node, RowBrowser browser, int[] count) {
        for (RowBrowser rb : desktop.getChildBrowsers(browser, true)) {
            DefaultMutableTreeNode childNode = new DefaultMutableTreeNode(new TreeNodeForRowBrowser(rb, count[0]++));
            node.add(childNode);
            addChildNodes(childNode, rb, count);
        }
    }

    public JScrollPane getDesktopScrollPane() {
        return jScrollPane1;
    }

    private DBClosureView closureView;
    private boolean suppressUpdateClosureBrowser = false;
    
    protected void updateClosureBrowser(final RowBrowser rowBrowser) {
    	if (suppressUpdateClosureBrowser) {
    		return;
    	}
        if (rowBrowser != null) {
            if (rowBrowser.browserContentPane.table instanceof SqlStatementTable) {
                updateClosureBrowser(null);
                return;
            }
        }
        closureView = new DBClosureView(this) {
        	private RowBrowser closureRoot = rowBrowser;

        	@Override
            protected DataModel getDataModel() {
                return DataBrowser.this.datamodel.get();
            }

            @Override
            protected Table getRootTable() {
                if (rowBrowser != null) {
                    return rowBrowser.browserContentPane.table;
                }
                return null;
            }

            private Map<Table, RowBrowser> visibleTables = null;
            private Map<Table, Integer> levels = null;

            @Override
            protected Map<Table, RowBrowser> getVisibleTables() {
                if (visibleTables == null) {
                    visibleTables = new HashMap<Table, RowBrowser>();
                    levels = new HashMap<Table, Integer>();
                    if (rowBrowser != null) {
                        collectVisibleTables(rowBrowser, 1);
                    }
                }
                return visibleTables;
            }

            private void collectVisibleTables(RowBrowser rowBrowser, int level) {
                Integer prevLevel = levels.get(rowBrowser.browserContentPane.table);
                if (prevLevel == null || prevLevel > level) {
                	visibleTables.put(rowBrowser.browserContentPane.table, rowBrowser);
                	levels.put(rowBrowser.browserContentPane.table, level);
                }
                for (RowBrowser cb: rowBrowser.browserContentPane.getChildBrowsers()) {
                    collectVisibleTables(cb, level + 1);
                }
            }

            @Override
            protected void scrollToTable(Table table) {
            	RowBrowser rb = visibleTables.get(table);
            	if (rb != null) {
            		desktop.scrollToCenter(rb.internalFrame);
            		desktop.getiFrameStateChangeRenderer().onIFrameSelected(rb.internalFrame);
            	}
        	}

            @Override
            protected void repaintClosureView() {
                desktopSplitPane.repaint();
                desktopSplitPane.setDividerLocation(desktopSplitPane.getDividerLocation() - 1);
                desktopSplitPane.setDividerLocation(desktopSplitPane.getDividerLocation() + 1);
            }

            @Override
            protected void expandTablePath(List<Table> path) {
                int i;
                i = path.size() - 1;
                while (i >= 0) {
                	if (!getVisibleTables().containsKey(path.get(i))) {
                		++i;
                		break;
                	}
                	--i;
                }
        		Association[] associations = openAssociationPathPanel(path.subList(0, i + 1));
        		if (associations != null) {
        			try {
						Desktop.noArrangeLayoutOnNewTableBrowser = true;
        				desktop.getiFrameStateChangeRenderer().startAtomic();
        				disableBorderBrowserUpdates = true;
        	            suppressUpdateClosureBrowser = true;
        	            RowBrowser nextRb = null;
	            		while (i > 0) {
		            		Table table = path.get(i);
		            		RowBrowser rb;
		            		if (nextRb != null && nextRb.association != null && nextRb.association.destination.equals(table)) {
		            			rb = nextRb;
		            		} else {
		            			rb = getVisibleTables().get(table);
		            		}
		            		Association association = associations[i - 1];
		            		if (association != null) {
		            			nextRb = rb.browserContentPane.navigateTo(association, null);
		            			visibleTables = null;
		            		} else {
		            			break;
		            		}
		            		--i;
		            	}
	                    suppressUpdateClosureBrowser = false;

		            	closureRoot.internalFrame.setSelected(true);
					} catch (Exception e) {
						// ignore
					} finally {
						Desktop.noArrangeLayoutOnNewTableBrowser = false;
						disableBorderBrowserUpdates = false;
			            suppressUpdateClosureBrowser = false;
        				desktop.getiFrameStateChangeRenderer().endAtomic();
        				desktop.catchUpLastArrangeLayoutOnNewTableBrowser();
					}
	            	closureView.find(getDataModel().getDisplayName(path.get(0)));
        		}
            }

            @Override
			protected void select(String selectedTable) {
				 try {
					 if (selectedTable != null) {
						 RowBrowser rb = getVisibleTables().get(datamodel.get().getTableByDisplayName(selectedTable));
						 if (rb != null) {
							 JInternalFrame iFrame = rb.internalFrame;
							 desktop.scrollToCenter(iFrame);
							 iFrame.setSelected(true);
							 iFrame.grabFocus();
							 desktop.getiFrameStateChangeRenderer().onIFrameSelected(iFrame);
						 }
					 }
                } catch (PropertyVetoException e1) {
                    // ignore
                }
			}

			private Association[] openAssociationPathPanel(List<Table> path) {
				if (path.isEmpty()) {
					return null;
				}
				final AssociationPathPanel assocPanel = new AssociationPathPanel(getDataModel(), path, dependsOn.getForeground(), hasDependent.getForeground(), associatedWith.getForeground(), ignored.getForeground());
				if (!assocPanel.needToAsk) {
					return assocPanel.selectedAssociations;
				}
				final JDialog d = new JDialog(DataBrowser.this, "Open Path", true);
				assocPanel.okButton.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						assocPanel.ok = true;
						d.setVisible(false);
					}
				});
				assocPanel.cancelButton.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						assocPanel.ok = false;
						d.setVisible(false);
					}
				});
				d.getContentPane().add(assocPanel);
				d.pack();
				d.setSize(600, Math.max(d.getHeight() + 20, 400));
				d.setLocation(DataBrowser.this.getX() + (DataBrowser.this.getWidth() - d.getWidth()) / 2, Math.max(0, DataBrowser.this.getY() + (DataBrowser.this.getHeight() - d.getHeight()) / 2));
				UIUtil.fit(d);
				assocPanel.okButton.grabFocus();
				d.setVisible(true);
				if (assocPanel.ok) {
					return assocPanel.selectedAssociations;
				}
				return null;
			}
        };
        Container cVContentPane = closureView.tablePanel;
        closureView.dispose();
        closurePanel.removeAll();
        GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1;
        gridBagConstraints.weighty = 1;
        closurePanel.add(cVContentPane, gridBagConstraints);
        closureView.refresh();
    }

    private MetaDataSource getMetaDataSource(Session session) {
    	return (MetaDataSource) session.getSessionProperty(DataBrowser.class, "MetaDataSource");
    }

    private void removeMetaDataSource(Session session) {
    	session.setSessionProperty(DataBrowser.class, "removeMetaDataSource", Boolean.TRUE);
    }

    private MetaDataPanel metaDataPanel;
    private Runnable createMetaDataPanel;

    private void onNewSession(Session newSession) {
    	if (newSession == null) {
    		return;
    	}

    	ConnectionInfo connection = dbConnectionDialog != null ? dbConnectionDialog.currentConnection : null;
    	String alias = connection != null ? " " + connection.alias : " ";

    	UIUtil.setWaitCursor(this);
    	CancellationHandler.reset(null);
    	try {
	    	updateNavigationCombobox();

	    	tablesPanel.removeAll();
	    	metaDataPanel = (MetaDataPanel) session.getSessionProperty(getClass(), "metaDataPanel");
			MetaDataSource metaDataSource;
			try {
				metaDataSource = getMetaDataSource(newSession);
				if (metaDataSource == null || Boolean.TRUE.equals(session.getSessionProperty(DataBrowser.class, "removeMetaDataSource"))) {
					metaDataSource = new MetaDataSource(newSession, datamodel.get(), alias, executionContext);
					final MetaDataSource finalMetaDataSource = metaDataSource;
					Thread thread = new Thread(new Runnable() {
						@Override
						public void run() {
							Session.setThreadSharesConnection();
							final MDSchema defaultSchema = finalMetaDataSource.getDefaultSchema();
							if (defaultSchema != null) {
								// trigger reading meta data asynchronously
								defaultSchema.loadTables(true, null, new Runnable() {
									@Override
									public void run() {
										UIUtil.invokeLater(new Runnable() {
											@Override
											public void run() {
												if (createMetaDataPanel != null) {
													createMetaDataPanel.run();
												}
												if (metaDataPanel != null) {
													metaDataPanel.refresh();
												}
												try {
													Thread.sleep(200);
												} catch (InterruptedException e) {
													// ignore
												}
												UIUtil.invokeLater(8, () -> {
													updateNavigationTree();
												});
											}
										});
									}
								}, new Runnable() {
									@Override
									public void run() {
										if (metaDataPanel != null) {
											metaDataPanel.refresh();
										}
									}
								});
							} else {
								if (createMetaDataPanel != null) {
									createMetaDataPanel.run();
								}
								if (metaDataPanel != null) {
									metaDataPanel.refresh();
								}
							}
						}
					});
					thread.setDaemon(true);
					thread.start();
					metaDataPanel = null;
				}
				session.setSessionProperty(DataBrowser.class, "removeMetaDataSource", null);
				newSession.setSessionProperty(DataBrowser.class, "MetaDataSource", metaDataSource);
			} catch (SQLException e) {
				throw new RuntimeException(e);
			}

			metaDataViewPanel.remove(metaDataDetailsPanel);
			metaDataDetailsPanel = createMetaDataDetailsPanel(executionContext);
			metaDataViewPanel.add(metaDataDetailsPanel);

			try {
				if (sqlConsoles.isEmpty()) {
					createNewSQLConsole(metaDataSource);
					workbenchTabbedPane.setSelectedComponent(desktopSplitPane);
				} else {
					for (SQLConsole sqlConsole: sqlConsoles) {
						sqlConsole.reset(session, metaDataSource);
					}
				}
			} catch (SQLException e) {
				e.printStackTrace();
			}

			((CardLayout) tablesCardPanel.getLayout()).show(tablesCardPanel, "loading");

			final MetaDataSource fMetaDataSource = metaDataSource;
			final Runnable createMetaDataPanelImmediately = new Runnable() {
				@Override
				public void run() {
					if (metaDataPanel == null) {
						metaDataPanel = new MetaDataPanel(DataBrowser.this, fMetaDataSource, metaDataDetailsPanel, datamodel.get(), executionContext) {
							@Override
							protected void open(Table table) {
								if (!selectNavTreeNode(navigationTree.getModel().getRoot(), table)) {
									if (workbenchTabbedPane.getSelectedComponent() != getCurrentSQLConsole()) {
										desktop.addTableBrowser(null, null, table, null, "", null, null, true);
									}
								}
								try {
									String sql;
									Quoting quoting = Quoting.getQuoting(session);
									MDTable mdTable = getMetaDataSource(session).toMDTable(table);
									String tableName;
									String schemaName;
									if (mdTable != null) {
										tableName = mdTable.getName();
										schemaName = mdTable.getSchema().isDefaultSchema? "": mdTable.getSchema().getName();
									} else {
										tableName = table.getUnqualifiedName();
										schemaName = table.getSchema("");
									}
									sql = "Select * From " + (schemaName == null || schemaName.length() == 0? "" : quoting.quote(schemaName) + ".") + quoting.quote(tableName);
									if (workbenchTabbedPane.getSelectedComponent() == getCurrentSQLConsole()) {
										workbenchTabbedPane.setSelectedComponent(getCurrentSQLConsole());
										getCurrentSQLConsole().grabFocus();
										getCurrentSQLConsole().appendStatement(sql, true);
									}
								} catch (SQLException e) {
									UIUtil.showException(this, "Error", e);
								}
							}

							private boolean selectNavTreeNode(Object root, Table table) {
								if (root instanceof DefaultMutableTreeNode) {
									Object userObject = ((DefaultMutableTreeNode) root).getUserObject();
					                if (userObject instanceof TreeNodeForRowBrowser) {
					                    RowBrowser rowBrowser = ((TreeNodeForRowBrowser) userObject).rowBrowser;
					                    if (table.equals(rowBrowser.browserContentPane.table)) {
					                    	navigationTree.getSelectionModel().setSelectionPath(new TreePath(((DefaultMutableTreeNode) root).getPath()));
					                    	return true;
					                    }
					                }
					                int cc = ((DefaultMutableTreeNode) root).getChildCount();
					                for (int i = 0; i < cc; ++i) {
					                	if (selectNavTreeNode(((DefaultMutableTreeNode) root).getChildAt(i), table)) {
					                		return true;
					                	}
					                }
								}
								return false;
							}

							private boolean selectNavTreeNode(Object root, MDTable mdTable) {
								if (root instanceof DefaultMutableTreeNode) {
									Object userObject = ((DefaultMutableTreeNode) root).getUserObject();
					                if (userObject instanceof TreeNodeForRowBrowser) {
					                    RowBrowser rowBrowser = ((TreeNodeForRowBrowser) userObject).rowBrowser;
					                    if (mdTable.equals(rowBrowser.getMDTable())) {
					                    	navigationTree.getSelectionModel().setSelectionPath(new TreePath(((DefaultMutableTreeNode) root).getPath()));
					                    	return true;
					                    }
					                }
					                int cc = ((DefaultMutableTreeNode) root).getChildCount();
					                for (int i = 0; i < cc; ++i) {
					                	if (selectNavTreeNode(((DefaultMutableTreeNode) root).getChildAt(i), mdTable)) {
					                		return true;
					                	}
					                }
								}
								return false;
							}

							@Override
							protected void analyseSchema(String schemaName) {
								updateDataModel(schemaName, false, false);
							}

							@Override
							protected void open(MDTable mdTable) {
								String schemaName = mdTable.getSchema().isDefaultSchema? null : mdTable.getSchema().getName();
								String tableName = mdTable.getName();
								try {
									Quoting quoting = Quoting.getQuoting(session);
									String sql = "Select * From " + (schemaName == null? "" : quoting.quote(schemaName) + ".") + quoting.quote(tableName);
									if (!selectNavTreeNode(navigationTree.getModel().getRoot(), mdTable)
										|| workbenchTabbedPane.getSelectedComponent() == getCurrentSQLConsole()) {
										workbenchTabbedPane.setSelectedComponent(getCurrentSQLConsole());
										getCurrentSQLConsole().grabFocus();
										getCurrentSQLConsole().appendStatement(sql, true);
									}
								} catch (SQLException e) {
									UIUtil.showException(this, "Error", e);
								}
							}

							@Override
							protected void appendScript(String script, boolean execute) {
								try {
									workbenchTabbedPane.setSelectedComponent(getCurrentSQLConsole());
									getCurrentSQLConsole().grabFocus();
									getCurrentSQLConsole().appendStatement(script, execute);
								} catch (Throwable e) {
									UIUtil.showException(this, "Error", e);
								}
							}

							@Override
							protected void onTableSelect(MDTable mdTable) {
								metaDataDetailsPanel
									.showMetaDataDetails(mdTable, getMetaDataSource(session).toTable(mdTable), null, false, datamodel.get());
							}

							@Override
							protected void onRowSelect(Table table, Row row) {
								metaDataDetailsPanel
									.showMetaDataDetails(null, table, row, true, datamodel.get());
							}

							@Override
							protected void onMDOtherSelect(MDGeneric mdOther, ExecutionContext executionContext) {
								metaDataDetailsPanel
									.showMetaDataDetails(mdOther, executionContext);
							}

							@Override
							protected void onSchemaSelect(MDSchema mdSchema) {
								metaDataDetailsPanel.clear();
							}

							@Override
							protected void openNewTableBrowser() {
								DataBrowser.this.openNewTableBrowser(false);
							}

							@Override
							protected void updateDataModelView(Table table) {
								DataBrowser.this.updateDataModelView(table);
							}

							@Override
							protected void setCaretPosition(int position) {
								getCurrentSQLConsole().setCaretPosition(position);
							}

							@Override
							protected void setOrResetWaitState(boolean set) {
								((CardLayout) tablesCardPanel.getLayout()).show(tablesCardPanel, set? "loading" : "tables");
							}
						};
					}
					session.setSessionProperty(getClass(), "metaDataPanel", metaDataPanel);
					tablesPanel.add(metaDataPanel, java.awt.BorderLayout.CENTER);
					((CardLayout) tablesCardPanel.getLayout()).show(tablesCardPanel, "tables");
				}
			};
			createMetaDataPanel = new Runnable() {
				@Override
				public void run() {
					Thread thread = new Thread(new Runnable() {
						@Override
						public void run() {
							Session.setThreadSharesConnection();

							// trigger loading meta data
							fMetaDataSource.getSchemas();

							UIUtil.invokeLater(new Runnable() {
								@Override
								public void run() {
									createMetaDataPanelImmediately.run();
								}
							});
						}
					});
					thread.setDaemon(true);
					thread.start();
					createMetaDataPanel = null;
				}
			};
			if (tableTreesTabbedPane.getSelectedComponent() == tablesPanel) {
				createMetaDataPanel.run();
			}
    	}
		finally {
			UIUtil.resetWaitCursor(this);
		}
	}

    private final class SQLConsoleWithTitle extends SQLConsole {
		private final String initialTitle;
		private final JLabel titleLbl;
		private String title;

		private SQLConsoleWithTitle(Session session, MetaDataSource metaDataSource, Reference<DataModel> datamodel,
				ExecutionContext executionContext, String title, JLabel titleLbl) throws SQLException {
			super(session, metaDataSource, datamodel, executionContext);
			this.initialTitle = title;
			this.titleLbl = titleLbl;
		}

		@Override
		protected void refreshMetaData() {
			if (metaDataPanel != null) {
				metaDataPanel.reset();
			}
		}

		@Override
		protected void selectTable(MDTable mdTable) {
			if (metaDataPanel != null) {
				metaDataPanel.select(mdTable);
			}
		}

		@Override
		protected void setOutlineTables(List<OutlineInfo> outlineTables, int indexOfInfoAtCaret) {
			if (metaDataPanel != null) {
				metaDataPanel.setOutline(outlineTables, indexOfInfoAtCaret);
			}
		}

		@Override
		protected void onContentStateChange(File file, boolean dirty) {
			if (file == null) {
				title = initialTitle;
			} else {
				title = file.getName();
			}
			if (dirty && file != null) {
				titleLbl.setText("* " + title);
			} else {
				titleLbl.setText(title);
			}
			updateLoadSaveScriptMenuItemsState();
		}

		public String getTitle() {
			return title;
		}

		@Override
		protected JFrame getOwner() {
			return DataBrowser.this;
		}

		@Override
		protected void openDataModelEditor(boolean merge) {
			DataBrowser.this.openDataModelEditor(merge);
		}

		@Override
		protected void setReloadLimit(int limit) {
			desktop.setRowLimit(limit);
		}
	}

    private int sqlConsoleNr = 0;

	private SQLConsole createNewSQLConsole(MetaDataSource metaDataSource) throws SQLException {
		final JLabel titleLbl = new JLabel(sqlConsoleIcon);
		String tabName = "SQL Console";
		++sqlConsoleNr;
		String title = tabName + (sqlConsoleNr > 1? " (" + sqlConsoleNr + ")" : "") + " ";

		final SQLConsoleWithTitle sqlConsole = new SQLConsoleWithTitle(session, metaDataSource, datamodel, executionContext, title, titleLbl);
		initDnD(sqlConsole.getEditorPane());
		sqlConsoles.add(sqlConsole);

		try {
			ignoreTabChangeEvent = true;
    		for (int i = 0; i < workbenchTabbedPane.getTabCount(); ++i) {
    			if (workbenchTabbedPane.getComponentAt(i) == consoleDummyPanel) {
    				workbenchTabbedPane.removeTabAt(i);
    				break;
    			}
    		}
			workbenchTabbedPane.insertTab(title, null, sqlConsole, null, sqlConsoles.size());
			JPanel titelPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
			titelPanel.setOpaque(false);
			titleLbl.setText(title);
			// titleLbl.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 0));
			titelPanel.add(titleLbl);
			SmallButton closeButton = new SmallButton(closeIcon) {
				@Override
				protected void onClick() {
					if (closeSQLConsole(sqlConsole, true)) {
						workbenchTabbedPane.setSelectedComponent(desktopSplitPane);
					}
				}
			};
			titelPanel.add(closeButton);
			if (sqlConsoles.size() > 1) {
				workbenchTabbedPane.setTabComponentAt(workbenchTabbedPane.indexOfComponent(sqlConsole), titelPanel);
			} else {
				workbenchTabbedPane.setTabComponentAt(workbenchTabbedPane.indexOfComponent(sqlConsole), titleLbl);
			}
		} finally {
			ignoreTabChangeEvent = false;
		}
		workbenchTabbedPane.setSelectedComponent(sqlConsole);
		return sqlConsole;
	}

	private boolean closeAllSQLConsoles() {
		List<SQLConsoleWithTitle> toClose = new ArrayList<SQLConsoleWithTitle>(sqlConsoles);
		for (SQLConsoleWithTitle sqlConsole: toClose) {
			workbenchTabbedPane.setSelectedComponent(sqlConsole);
			if (sqlConsole.getFile() != null && sqlConsole.isDirty()) {
				if (!closeSQLConsole(sqlConsole, true)) {
					return false;
				}
			}
		}
		toClose = new ArrayList<SQLConsoleWithTitle>(sqlConsoles);
		for (SQLConsoleWithTitle sqlConsole: toClose) {
			workbenchTabbedPane.setSelectedComponent(sqlConsole);
			if (!closeSQLConsole(sqlConsole, false)) {
				return false;
			}
		}
		return true;
	}

	private boolean closeSQLConsole(SQLConsoleWithTitle sqlConsole, boolean ask) {
		if (ask) {
			if (!sqlConsole.isEmpty() && !(sqlConsole.getFile() != null && !sqlConsole.isDirty())) {
				if (JOptionPane.YES_OPTION != JOptionPane.showConfirmDialog(DataBrowser.this, "Close \"" + sqlConsole.getTitle() + "\"" + (sqlConsole.getFile() == null? "?" : " without saving?"), "Close Console", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE)) {
					return false;
				}
			}
		}
		try {
			sqlConsole.close();
			ignoreTabChangeEvent = true;
			workbenchTabbedPane.remove(sqlConsole);
			sqlConsoles.remove(sqlConsole);
		} finally {
			ignoreTabChangeEvent = false;
		}
		return true;
	}

	private void updateLoadSaveScriptMenuItemsState() {
		SQLConsole sqlConsole = null;
		Component sc = workbenchTabbedPane.getSelectedComponent();
		if (sc instanceof SQLConsole) {
			sqlConsole = (SQLConsole) sc;
		}
		saveScriptMenuItem.setEnabled(sqlConsole != null && sqlConsole.isDirty());
		saveScriptAsMenuItem.setEnabled(sqlConsole != null && !sqlConsole.isEmpty());
	}

	private void loadScriptMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_loadScriptMenuItemActionPerformed
        String fName = UIUtil.choseFile(null, ".", "Load SQL Script", "", this, false, true);
        if (fName != null) {
        	File file = new File(fName);
        	loadSQLScriptFile(file);
        }
    }//GEN-LAST:event_loadScriptMenuItemActionPerformed

	private void loadSQLScriptFile(File file) {
		for (SQLConsole sqlConsole: sqlConsoles) {
			if (file.equals(sqlConsole.getFile())) {
				workbenchTabbedPane.setSelectedComponent(sqlConsole);
				return;
			}
		}
		try {
			UIUtil.setWaitCursor(this);
			createNewSQLConsole(getMetaDataSource()).loadFromFile(file);
		} catch (Throwable e) {
			UIUtil.showException(this, "Error", e, session);
		} finally {
			UIUtil.resetWaitCursor(this);
		}
	}

    private void saveScriptMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_saveScriptMenuItemActionPerformed
    	SQLConsole sqlConsole = getCurrentSQLConsole();
		if (sqlConsole != null) {
			if (sqlConsole.getFile() == null) {
				saveScriptAsMenuItemActionPerformed(evt);
			} else {
				try {
					UIUtil.setWaitCursor(this);
					sqlConsole.storeToFile(null);
				} catch (Throwable e) {
					UIUtil.showException(this, "Error", e, session);
				} finally {
					UIUtil.resetWaitCursor(this);
				}
			}
		}
    }//GEN-LAST:event_saveScriptMenuItemActionPerformed

	private void initMenu() {
		int mask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
		if (mask != InputEvent.CTRL_MASK) {
			for (int i = 0; i < menuBar.getMenuCount(); ++i) {
				JMenu menu = menuBar.getMenu(i);
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

    private void saveScriptAsMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_saveScriptAsMenuItemActionPerformed
		SQLConsole sqlConsole = getCurrentSQLConsole();
		if (sqlConsole != null) {
			String fName = UIUtil.choseFile(null, ".", "Save SQL Script", "", this, false, false);
			if (fName != null) {
				File file = new File(fName);
		    	try {
		    		UIUtil.setWaitCursor(this);
					sqlConsole.storeToFile(file);
				} catch (Throwable e) {
					UIUtil.showException(this, "Error", e, session);
				} finally {
					UIUtil.resetWaitCursor(this);
				}
			}
		}
    }//GEN-LAST:event_saveScriptAsMenuItemActionPerformed

    private void exitMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_exitMenuItemActionPerformed
       	if (closeAllSQLConsoles()) {
       		DataBrowser.this.dispose();
       	}
    }//GEN-LAST:event_exitMenuItemActionPerformed

    private void analyseSQLMenuItem1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_analyseSQLMenuItem1ActionPerformed
    	AssociationProposerView assocProposerView = new AssociationProposerView(this, datamodel.get(), null, 3, executionContext);
    	if (assocProposerView.isAccepted()) {
    		openDataModelEditor(true);
    	}
    }//GEN-LAST:event_analyseSQLMenuItem1ActionPerformed

    private void columnOrderItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_columnOrderItemActionPerformed
        openColumnOrderEditor(null);
    }//GEN-LAST:event_columnOrderItemActionPerformed

	private void openColumnOrderEditor(Table table) {
		try {
        	if (new ColumnOrderEditor(this, table, datamodel.get(), executionContext).wasOk()) {
				removeMetaDataSource(session);
	            desktop.reloadDataModel(desktop.schemaMapping);
	            dataModelViewFrame = null;
	            updateDataModelView(null);
	            updateStatusBar();
	            askForDataModel();
        	}
		} catch (Throwable e) {
			UIUtil.showException(this, "Error", e);
		}
	}

	private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
        updateInfoPanel.setVisible(false);
    }//GEN-LAST:event_jButton1ActionPerformed

    private void downloadButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_downloadButtonActionPerformed
        updateInfoPanel.setVisible(false);
        UpdateInfoManager.download();
    }//GEN-LAST:event_downloadButtonActionPerformed

    private void nativeLAFCheckBoxMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_nativeLAFCheckBoxMenuItemActionPerformed

    }//GEN-LAST:event_nativeLAFCheckBoxMenuItemActionPerformed

    private void addBookmarkMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_addBookmarkMenuItemActionPerformed
        desktop.storeSession(new BookmarksPanel(this, bookmarkMenu, desktop, executionContext));
    }//GEN-LAST:event_addBookmarkMenuItemActionPerformed

    private void editBookmarkMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_editBookmarkMenuItemActionPerformed
    	new BookmarksPanel(this, bookmarkMenu, desktop, executionContext).editBookmarks();
    	desktop.updateAllBookmarkMenues();
    }//GEN-LAST:event_editBookmarkMenuItemActionPerformed

    private void downloadMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_downloadMenuItemActionPerformed
        UpdateInfoManager.download();
    }//GEN-LAST:event_downloadMenuItemActionPerformed

    private void zoomInMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_zoomInMenuItemActionPerformed
        desktop.zoom(1);
    }//GEN-LAST:event_zoomInMenuItemActionPerformed

    private void zoomOutMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_zoomOutMenuItemActionPerformed
        desktop.zoom(-1);
    }//GEN-LAST:event_zoomOutMenuItemActionPerformed

    private void checkPKMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_checkPKMenuItemActionPerformed
        try {
            if (dbConnectionDialog.isConnected || dbConnectionDialog.connect("Check Primary Keys")) {
                BasicDataSource dataSource = UIUtil.createBasicDataSource(this, dbConnectionDialog.currentConnection.driverClass, dbConnectionDialog.currentConnection.url, dbConnectionDialog.currentConnection.user, dbConnectionDialog.getPassword(), 0, dbConnectionDialog.currentJarURLs());
                UIUtil.validatePrimaryKeys(this, dataSource, new TreeSet<Table>(datamodel.get().getTables()));
            }
        } catch (Exception e) {
            // ignore
        }
    }//GEN-LAST:event_checkPKMenuItemActionPerformed

    private void refreshButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_refreshButtonActionPerformed
    }//GEN-LAST:event_refreshButtonActionPerformed

    private void createCLIItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_createCLIItemActionPerformed
    	String mapping = desktop.getRawSchemaMapping();
    	String bookmark = BookmarksPanel.getLastUsedBookmark(executionContext);
    	if (bookmark != null) {
    		bookmark = bookmarkName(bookmark);
    	}
		List<String> bookmarks = new ArrayList<String>();
		for (StringBuilder sb: BookmarksPanel.loadBookmarks(executionContext)) {
			bookmarks.add(bookmarkName(sb.toString()));
		}
		new CLIPanel(dbConnectionDialog, true, null, mapping, bookmarks, bookmark, executionContext).open(this);
    }//GEN-LAST:event_createCLIItemActionPerformed

    private void consistencyCheckMenuItem1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_consistencyCheckMenuItem1ActionPerformed
    	consistencyCheckMenuItemActionPerformed(evt);
    }//GEN-LAST:event_consistencyCheckMenuItem1ActionPerformed

    private void renderHtmlActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_renderHtmlActionPerformed
    	try {
			List<String> args = new ArrayList<String>();
			args.add("render-datamodel");
			if (UIUtil.canRunJailer()) {
				UIUtil.runJailer(this, args, false, true, true, null, null, null /* dbConnectionDialog.getPassword() */, null, null, false, true, false, executionContext);
				HtmlDataModelRenderer renderer = Configuration.getInstance().getRenderer();
				String of = renderer.outputFolderOf(datamodel.get());
				BrowserLauncher.openURL(Environment.newFile(of + "/index.html").toURI(), this);
			}
		} catch (Exception e) {
			UIUtil.showException(this, "Error", e);
		}
    }//GEN-LAST:event_renderHtmlActionPerformed

    private String bookmarkName(String bookmarkFileName) {
		if (bookmarkFileName.endsWith(BookmarksPanel.BOOKMARKFILE_EXTENSION)) {
			return bookmarkFileName.substring(0, bookmarkFileName.length() - BookmarksPanel.BOOKMARKFILE_EXTENSION.length());
		} else {
			return bookmarkFileName;
		}
	}

	private MetaDataDetailsPanel metaDataDetailsPanel;
	private List<SQLConsoleWithTitle> sqlConsoles = new ArrayList<SQLConsoleWithTitle>();

	private SQLConsole getCurrentSQLConsole() {
		Component sc = workbenchTabbedPane.getSelectedComponent();
		if (sc instanceof SQLConsole) {
			return (SQLConsole) sc;
		}
		if (sqlConsoles.isEmpty()) {
			return null;
		}
		for (int i = sqlConsoles.size() - 1; i >= 0; --i) {
			if (sqlConsoles.get(i).getFile() == null) {
				return sqlConsoles.get(i);
			}
		}
		return sqlConsoles.get(sqlConsoles.size() - 1);
	}

	public MetaDataSource getMetaDataSource() {
		return getMetaDataSource(session);
	}

	private void switchToDesktop() {
		workbenchTabbedPane.setSelectedComponent(desktopSplitPane);
	}

	private void initDnD(Component target) {
		new DropTarget(target, DnDConstants.ACTION_COPY, new DropTargetListener() {
			@Override
			public void drop(DropTargetDropEvent dtde) {
				try {
					Transferable tr = dtde.getTransferable();
					DataFlavor[] flavors = tr.getTransferDataFlavors();
					for (int i = 0; i < flavors.length; i++) {
						if (flavors[i].isFlavorJavaFileListType()) {
							dtde.acceptDrop(dtde.getDropAction());
							@SuppressWarnings("unchecked")
							java.util.List<File> files = (java.util.List<File>) tr.getTransferData(flavors[i]);
							for (int k = 0; k < files.size(); k++) {
								loadSQLScriptFile(files.get(k));
							}

							dtde.dropComplete(true);
						}
					}
					return;
				} catch (Throwable t) {
					t.printStackTrace();
				}
				dtde.rejectDrop();
			}

			@Override
			public void dragEnter(DropTargetDragEvent dtde) {
			}

			@Override
			public void dragOver(DropTargetDragEvent dtde) {
			}

			@Override
			public void dropActionChanged(DropTargetDragEvent dtde) {
			}

			@Override
			public void dragExit(DropTargetEvent dte) {
			}

		});
	}

	public boolean isReady() {
		return session != null;
	}

	public SQLConsole getSqlConsole(boolean switchToConsole) {
		return desktop.getSqlConsole(switchToConsole);
	}

	private Dimension outlineMinimumSize = null;

	private void repaintOutline() {
		if (desktopOutline != null) {
			Dimension minimumSize = desktopOutline.getMinimumSize();
			if (outlineMinimumSize == null || !outlineMinimumSize.equals(minimumSize)) {
				outlineMinimumSize = minimumSize;
				outLinePanel.revalidate();
			}
			outLinePanel.repaint();
		}
	}

	public void openGlobalPopup(MouseEvent e) {
		if (e.getButton() != MouseEvent.BUTTON3) {
            return;
        }
        JPopupMenu popup = new JPopupMenu();
        JMenuItem i = new JMenuItem("Arrange Layout");
        popup.add(i);
        i.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                layoutMenuItemActionPerformed(e);
            }
        });
        ButtonGroup group = new ButtonGroup();
        popup.add(new JSeparator());
        i = new JRadioButtonMenuItem("Thumbnail Layout");
        i.setSelected(desktop.layoutMode == LayoutMode.THUMBNAIL);
        group.add(i);
        popup.add(i);
        i.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                thumbnailLayoutRadioButtonMenuItemActionPerformed(e);
            }
        });
        i = new JRadioButtonMenuItem("Tiny Layout");
        i.setSelected(desktop.layoutMode == LayoutMode.TINY);
        group.add(i);
        popup.add(i);
        i.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                tinyLayoutRadioButtonMenuItemActionPerformed(e);
            }
        });
        i = new JRadioButtonMenuItem("Small Layout");
        i.setSelected(desktop.layoutMode == LayoutMode.SMALL);
        group.add(i);
        popup.add(i);
        i.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                smallLayoutRadioButtonMenuItemActionPerformed(e);
            }
        });
        i = new JRadioButtonMenuItem("Medium Layout");
        i.setSelected(desktop.layoutMode == LayoutMode.MEDIUM);
        group.add(i);
        popup.add(i);
        i.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                mediumLayoutRadioButtonMenuItemActionPerformed(e);
            }
        });
        i = new JRadioButtonMenuItem("Large Layout");
        i.setSelected(desktop.layoutMode == LayoutMode.LARGE);
        group.add(i);
        popup.add(i);
        i.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                largeLayoutRadioButtonMenuItemActionPerformed(e);
            }
        });
        UIUtil.showPopup(e.getComponent(), e.getX(), e.getY(), popup);
	}
	
	private static final String LAST_SESSION_FILE = ".lastsession";

	private void storeLastSession() {
		BookmarkId bookmark;
		try {
			desktop.storeSession(Environment.newFile(LAST_SESSION_FILE).getPath());
			bookmark = new BookmarkId(null, executionContext.getCurrentModelSubfolder(), executionContext.getCurrentConnectionAlias(), desktop.getRawSchemaMapping());
		} catch (IOException e) {
			bookmark = null;
		}
		UISettings.storeLastSession(bookmark, "B");
	}

	public static Date getLastSessionDate() {
		try {
			long lastModified = Environment.newFile(LAST_SESSION_FILE).lastModified();
			if (lastModified == 0) {
				return null;
			}
			return new Date(lastModified);
		} catch (Exception e) {
			return null;
		}
	}

	private DesktopAnchorManager anchorManager;
	private DesktopOutline desktopOutline;

	private ImageIcon tableIcon;
	private ImageIcon databaseIcon;
	private ImageIcon redIcon;
	private ImageIcon blueIcon;
	private ImageIcon greenIcon;
	private Icon closeIcon;
	private ImageIcon sqlConsoleIcon;
	private ImageIcon addSqlConsoleIcon;
	private ImageIcon navigationIcon;
	private ImageIcon desktopIcon;
	{
        // load images
        tableIcon = UIUtil.readImage("/table.png");
        databaseIcon = UIUtil.readImage("/database.png");
        redIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/reddot.gif"));
        blueIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/bluedot.gif"));
        greenIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/greendot.gif"));
        closeIcon = UIUtil.readImage("/Close-16-1.png");
        sqlConsoleIcon = UIUtil.readImage("/runall.png");
        desktopIcon = UIUtil.readImage("/navigation.png");
        addSqlConsoleIcon = UIUtil.readImage("/add.png");
        navigationIcon = UIUtil.readImage("/navigation.png");
    }

	// TODO $props in "where", with values-query, dialog with combo-box, persisted in the *.dbl files
}
