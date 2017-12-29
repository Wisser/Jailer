/*
 * Copyright 2007 - 2017 the original author or authors.
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

import java.awt.Component;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.beans.PropertyVetoException;
import java.io.File;
import java.io.PrintWriter;
import java.net.URI;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.ButtonGroup;
import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.ImageIcon;
import javax.swing.InputMap;
import javax.swing.JButton;
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
import javax.swing.JViewport;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.ToolTipManager;
import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.MouseInputAdapter;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.database.BasicDataSource;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.modelbuilder.ModelBuilder;
import net.sf.jailer.ui.About;
import net.sf.jailer.ui.AnalyseOptionsDialog;
import net.sf.jailer.ui.AssociationListUI;
import net.sf.jailer.ui.AssociationListUI.AssociationModel;
import net.sf.jailer.ui.AssociationListUI.DefaultAssociationModel;
import net.sf.jailer.ui.AutoCompletion;
import net.sf.jailer.ui.BrowserLauncher;
import net.sf.jailer.ui.CommandLineInstance;
import net.sf.jailer.ui.DataModelEditor;
import net.sf.jailer.ui.DataModelManager;
import net.sf.jailer.ui.DataModelManagerDialog;
import net.sf.jailer.ui.DbConnectionDialog;
import net.sf.jailer.ui.DbConnectionDialog.ConnectionInfo;
import net.sf.jailer.ui.Environment;
import net.sf.jailer.ui.ExtractionModelFrame;
import net.sf.jailer.ui.ImportDialog;
import net.sf.jailer.ui.JComboBox;
import net.sf.jailer.ui.StringSearchPanel;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.constraintcheck.ConstraintChecker;
import net.sf.jailer.ui.databrowser.BrowserContentPane.SqlStatementTable;
import net.sf.jailer.ui.databrowser.Desktop.LayoutMode;
import net.sf.jailer.ui.databrowser.Desktop.RowBrowser;
import net.sf.jailer.ui.databrowser.metadata.MDSchema;
import net.sf.jailer.ui.databrowser.metadata.MDTable;
import net.sf.jailer.ui.databrowser.metadata.MetaDataDetailsPanel;
import net.sf.jailer.ui.databrowser.metadata.MetaDataPanel;
import net.sf.jailer.ui.databrowser.metadata.MetaDataPanel.OutlineInfo;
import net.sf.jailer.ui.databrowser.metadata.MetaDataSource;
import net.sf.jailer.ui.databrowser.sqlconsole.SQLConsole;
import net.sf.jailer.ui.syntaxtextarea.BasicFormatterImpl;
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

    private final JComboBox<String> tablesComboBox; 
    
	/**
	 * Allowed row limits.
	 */
	public static final Integer[] ROW_LIMITS = new Integer[] { 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000, 70000, 100000 };

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
    public DataBrowser(final DataModel datamodel, final Table root, String condition, DbConnectionDialog dbConnectionDialog, boolean embedded, final ExecutionContext executionContext) throws Exception {
        this.executionContext = executionContext;
        this.datamodel = new Reference<DataModel>(datamodel);
        this.dbConnectionDialog = dbConnectionDialog != null ? new DbConnectionDialog(this, dbConnectionDialog, DataBrowserContext.getAppName(), executionContext) : null;
        this.borderBrowser = new AssociationListUI("Resolve", "Resolve selected Associations", true) {
            @Override
            protected void applyAction(Collection<AssociationModel> selection) {
                resolveSelection(selection);
            }
        };
        if (embedded) {
            DataBrowserContext.setSupportsDataModelUpdates(false);
        }
        initComponents();
        
        tablesComboBox = new JComboBox<String>() {
        	@Override
        	public Dimension getMinimumSize() {
				return new Dimension(40, super.getMinimumSize().height);
        	}
        };
        tablesComboBox.setMaximumRowCount(20);
        updateNavigationCombobox();
        AutoCompletion.enable(tablesComboBox);
        
		tablesComboBox.grabFocus();
		
        GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1;
        navigationPanel.add(tablesComboBox, gridBagConstraints);
        
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.NONE;
        gridBagConstraints.weightx = 0;
        JButton searchButton = StringSearchPanel.createSearchButton(this, tablesComboBox, "Open Table Browser", new Runnable() {
			@Override
			public void run() {
				openTableButtonActionPerformed(null);
			}
		});
		navigationPanel.add(searchButton, gridBagConstraints);
        
		tablesComboBox.setVisible(false);
		openTableButton.setVisible(false);
		searchButton.setText("Open Table");
		
        detailsAndBorderBrowserTabbedPane.addChangeListener(new ChangeListener() {
			@Override
			public void stateChanged(ChangeEvent e) {
				updateBorderBrowser();
				updateDataModelView(null);
				showDataModelMenuItem.setSelected(detailsAndBorderBrowserTabbedPane.getSelectedComponent() == dataModelPanel);
			}
		});
        
        metaDataDetailsPanel = createMetaDataDetailsPanel(executionContext);
        metaDataViewPanel.add(metaDataDetailsPanel);
        
        jLayeredPane1.removeAll();
        jLayeredPane1.setLayout(new GridBagLayout());
        jLayeredPane1.setLayer(layeredPaneContent, JLayeredPane.PALETTE_LAYER);
        jLayeredPane1.setLayer(dummy, JLayeredPane.DEFAULT_LAYER);
        gridBagConstraints = new GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1;
        gridBagConstraints.weighty = 1;
        jLayeredPane1.add(layeredPaneContent, gridBagConstraints);
        jLayeredPane1.add(dummy, gridBagConstraints);
        
        if (jScrollPane1.getVerticalScrollBar() != null) {
            jScrollPane1.getVerticalScrollBar().setUnitIncrement(16);
        }
        if (jScrollPane1.getHorizontalScrollBar() != null) {
            jScrollPane1.getHorizontalScrollBar().setUnitIncrement(16);
        }
        
        hiddenPanel.setVisible(false);
        borderBrowserPanel.add(borderBrowser, java.awt.BorderLayout.CENTER);

        gridBagConstraints = new java.awt.GridBagConstraints();
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

        gridBagConstraints = new java.awt.GridBagConstraints();
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

        DefaultTreeCellRenderer renderer = new DefaultTreeCellRenderer();
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
            menuTools.setVisible(false);
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
                    public void actionPerformed(ActionEvent arg0) {
                        setPLAF(lfInfo.getClassName());
                    }
                });
            }
        } catch (Throwable t) {
        }

        try {
            setIconImage((jailerIcon = new ImageIcon(getClass().getResource("/net/sf/jailer/ui/resource/jailer.png"))).getImage());
        } catch (Throwable t) {
            try {
                setIconImage((jailerIcon = new ImageIcon(getClass().getResource("/net/sf/jailer/ui/resource/jailer.gif"))).getImage());
            } catch (Throwable t2) {
            }
        }

        jailerIcon.setImage(jailerIcon.getImage().getScaledInstance(16, 16, Image.SCALE_SMOOTH));

        if (dbConnectionDialog != null) {
            createSession(dbConnectionDialog);
        }
        desktop = new Desktop(this.datamodel, jailerIcon, session, this, dbConnectionDialog, executionContext) {
            @Override
            public void openSchemaAnalyzer() {
                updateDataModel();
            }

            @Override
            protected void updateMenu(boolean hasTableBrowser, boolean hasIFrame) {
                storeSessionItem.setEnabled(hasIFrame);
                exportDataMenuItem.setEnabled(hasTableBrowser);
                createExtractionModelMenuItem.setEnabled(hasTableBrowser);
                updateIFramesBar();
                super.updateMenu(hasTableBrowser, hasIFrame);
            }

            @Override
            protected void updateMenu(LayoutMode layoutMode) {
                if (layoutMode == Desktop.LayoutMode.TINY) {
                    tinyLayoutRadioButtonMenuItem.setSelected(true);
                } else if (layoutMode == Desktop.LayoutMode.SMALL) {
                    smallLayoutRadioButtonMenuItem.setSelected(true);
                } else if (layoutMode == Desktop.LayoutMode.MEDIUM) {
                    mediumLayoutRadioButtonMenuItem.setSelected(true);
                } else if (layoutMode == Desktop.LayoutMode.LARGE) {
                    largeLayoutRadioButtonMenuItem.setSelected(true);
                } else if (layoutMode == Desktop.LayoutMode.THUMBNAIL) {
                    thumbnailLayoutRadioButtonMenuItem.setSelected(true);
                }
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
					workbenchTabbedPane.setSelectedComponent(sqlConsoleContainerPanel);
				}
				return sqlConsole;
			}
        };

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
                // desktop.stop();
            }

            @Override
            public void windowClosed(WindowEvent e) {
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
                }
            }

            public void mousePressed(MouseEvent e) {
                if (e.getButton() == MouseEvent.BUTTON3) {
                    return;
                }
                setCursor(Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR));
                m_XDifference = e.getX();
                m_YDifference = e.getY();
            }

            public void mouseReleased(MouseEvent e) {
                if (e.getButton() == MouseEvent.BUTTON3) {
                    return;
                }
                setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
            }

            @Override
            public void mouseClicked(MouseEvent e) {
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
                popup.show(desktop, e.getX(), e.getY());
            }
        };

        desktop.addMouseMotionListener(mia);
        desktop.addMouseListener(mia);

        int c = 0;
        for (Frame frame: Frame.getFrames()) {
            if (frame instanceof DataBrowser && frame.isVisible()) {
                c = (c + 1) % 10;
            }
        }
        
        setLocation(40 + c * 32, 40 + c * 32);
        setSize(980, 840);
        
        UIUtil.fit(this);
        
        if (root != null) {
            final RowBrowser rb = desktop.addTableBrowser(null, null, 0, root, null, condition, null, null, true);
            if (rb != null && rb.internalFrame != null) {
                SwingUtilities.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        try {
                            rb.internalFrame.setSelected(true);
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
    }

	public void updateNavigationCombobox() {
		List<String> tables = new ArrayList<String>();
		
		for (Table table: datamodel.get().getTables()) {
			tables.add(datamodel.get().getDisplayName(table));
		}
		Collections.sort(tables);
		ComboBoxModel model = new DefaultComboBoxModel(new Vector(tables));
			
		tablesComboBox.setModel(model);
	}

	private MetaDataDetailsPanel createMetaDataDetailsPanel(final ExecutionContext executionContext) {
		return new MetaDataDetailsPanel(this.datamodel, session, this, executionContext) {
			@Override
			protected void analyseSchema(String schemaName, boolean withViews, boolean withSynonyms) {
				updateDataModel(schemaName, withViews, withSynonyms);
			}
        };
	}

    private void createSession(DbConnectionDialog dbConnectionDialog) throws Exception {
        if (session != null) {
            try {
                session.shutDown();
                session = null;
            } catch (Exception e) {
                // ignore
            }
        }
        ConnectionInfo connection = dbConnectionDialog.currentConnection;
        BasicDataSource dataSource = new BasicDataSource(connection.driverClass, connection.url, connection.user, connection.password, 0, dbConnectionDialog.currentJarURLs());
        session = new Session(dataSource, dataSource.dbms);
        List<String> args = new ArrayList<String>();
        dbConnectionDialog.addDbArgs(args);
        session.setCliArguments(args);
        session.setPassword(dbConnectionDialog.getPassword());
        onNewSession(session);
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
                desktop.session = session;
                onNewSession(session);
                desktop.openSchemaMappingDialog(true);
                updateStatusBar();
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
        sqlConsoleContainerPanel = new javax.swing.JPanel();
        jPanel8 = new javax.swing.JPanel();
        jPanel4 = new javax.swing.JPanel();
        jSplitPane4 = new javax.swing.JSplitPane();
        tableTreesTabbedPane = new javax.swing.JTabbedPane();
        navigationPanel = new javax.swing.JPanel();
        navigationTreeScrollPane = new javax.swing.JScrollPane();
        navigationTree = new javax.swing.JTree();
        openTableButton = new javax.swing.JButton();
        tablesPanel = new javax.swing.JPanel();
        detailsAndBorderBrowserTabbedPane = new javax.swing.JTabbedPane();
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
        borderBrowserTabPane = new javax.swing.JPanel();
        borderBrowserPanel = new javax.swing.JPanel();
        titleLabel = new javax.swing.JLabel();
        dataModelPanel = new javax.swing.JPanel();
        jLabel26 = new javax.swing.JLabel();
        menuBar = new javax.swing.JMenuBar();
        jMenu1 = new javax.swing.JMenu();
        newBrowserjMenuItem = new javax.swing.JMenuItem();
        jSeparator9 = new javax.swing.JPopupMenu.Separator();
        jMenuItem3 = new javax.swing.JMenuItem();
        jSeparator4 = new javax.swing.JPopupMenu.Separator();
        storeSessionItem = new javax.swing.JMenuItem();
        restoreSessionItem = new javax.swing.JMenuItem();
        jSeparator2 = new javax.swing.JPopupMenu.Separator();
        reconnectMenuItem = new javax.swing.JMenuItem();
        jSeparator7 = new javax.swing.JPopupMenu.Separator();
        cloaseAllMenuItem = new javax.swing.JMenuItem();
        schemaMappingMenuItem = new javax.swing.JMenuItem();
        menuTools = new javax.swing.JMenu();
        analyseMenuItem = new javax.swing.JMenuItem();
        dataModelEditorjMenuItem = new javax.swing.JMenuItem();
        jSeparator10 = new javax.swing.JPopupMenu.Separator();
        showDataModelMenuItem = new javax.swing.JCheckBoxMenuItem();
        jMenu2 = new javax.swing.JMenu();
        exportDataMenuItem = new javax.swing.JMenuItem();
        dataImport = new javax.swing.JMenuItem();
        jSeparator8 = new javax.swing.JPopupMenu.Separator();
        createExtractionModelMenuItem = new javax.swing.JMenuItem();
        jSeparator3 = new javax.swing.JPopupMenu.Separator();
        consistencyCheckMenuItem = new javax.swing.JMenuItem();
        menuWindow = new javax.swing.JMenu();
        layoutMenuItem = new javax.swing.JMenuItem();
        jSeparator5 = new javax.swing.JPopupMenu.Separator();
        thumbnailLayoutRadioButtonMenuItem = new javax.swing.JRadioButtonMenuItem();
        tinyLayoutRadioButtonMenuItem = new javax.swing.JRadioButtonMenuItem();
        smallLayoutRadioButtonMenuItem = new javax.swing.JRadioButtonMenuItem();
        mediumLayoutRadioButtonMenuItem = new javax.swing.JRadioButtonMenuItem();
        largeLayoutRadioButtonMenuItem = new javax.swing.JRadioButtonMenuItem();
        jSeparator1 = new javax.swing.JPopupMenu.Separator();
        newWindowMenuItem = new javax.swing.JMenuItem();
        jSeparator6 = new javax.swing.JPopupMenu.Separator();
        view = new javax.swing.JMenu();
        helpMenu = new javax.swing.JMenu();
        jMenuItem4 = new javax.swing.JMenuItem();
        helpForum = new javax.swing.JMenuItem();
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

        jPanel1.setLayout(new java.awt.GridBagLayout());

        jPanel11.setLayout(new java.awt.GridBagLayout());

        legende1.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
        legende1.setLayout(new java.awt.GridBagLayout());

        modelName.setFont(new java.awt.Font("Dialog", 0, 12)); // NOI18N
        modelName.setText("Data Model \"Demo\"");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 12);
        legende1.add(modelName, gridBagConstraints);

        modelPath.setFont(new java.awt.Font("Dialog", 0, 12)); // NOI18N
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

        dependsOn.setFont(new java.awt.Font("Dialog", 0, 12)); // NOI18N
        dependsOn.setForeground(new java.awt.Color(170, 0, 0));
        dependsOn.setText(" depends on (has parent) ");
        legende.add(dependsOn);

        hasDependent.setFont(new java.awt.Font("Dialog", 0, 12)); // NOI18N
        hasDependent.setForeground(new java.awt.Color(0, 112, 0));
        hasDependent.setText("  has dependent (has child) ");
        legende.add(hasDependent);

        associatedWith.setFont(new java.awt.Font("Dialog", 0, 12)); // NOI18N
        associatedWith.setForeground(new java.awt.Color(0, 100, 255));
        associatedWith.setText("  associated with");
        legende.add(associatedWith);

        ignored.setFont(new java.awt.Font("Dialog", 0, 12)); // NOI18N
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

        schemaName.setFont(new java.awt.Font("Dialog", 0, 12)); // NOI18N
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

        hiddenPanel.setBorder(javax.swing.BorderFactory.createTitledBorder("Hidden"));
        hiddenPanel.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        layeredPaneContent.add(hiddenPanel, gridBagConstraints);

        jLayeredPane1.setLayer(layeredPaneContent, javax.swing.JLayeredPane.PALETTE_LAYER);
        jLayeredPane1.add(layeredPaneContent);
        layeredPaneContent.setBounds(0, 0, 24, 58);

        desktopSplitPane.setLeftComponent(jLayeredPane1);

        closurePanel.setLayout(new java.awt.GridBagLayout());
        desktopSplitPane.setRightComponent(closurePanel);

        workbenchTabbedPane.addTab("Desktop", desktopSplitPane);

        sqlConsoleContainerPanel.setLayout(new java.awt.BorderLayout());

        jPanel8.setLayout(new java.awt.GridBagLayout());
        sqlConsoleContainerPanel.add(jPanel8, java.awt.BorderLayout.CENTER);

        workbenchTabbedPane.addTab("SQL Console", sqlConsoleContainerPanel);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(1, 0, 0, 0);
        jPanel5.add(workbenchTabbedPane, gridBagConstraints);

        jSplitPane1.setRightComponent(jPanel5);

        jPanel4.setLayout(new java.awt.GridBagLayout());

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

        tablesPanel.setLayout(new java.awt.BorderLayout());
        tableTreesTabbedPane.addTab("Tables", tablesPanel);

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

        detailsAndBorderBrowserTabbedPane.addTab("Details", jPanel7);

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

        detailsAndBorderBrowserTabbedPane.addTab("Closure Border", borderBrowserTabPane);

        dataModelPanel.setLayout(new java.awt.BorderLayout());

        jLabel26.setText("  Loading...");
        dataModelPanel.add(jLabel26, java.awt.BorderLayout.CENTER);

        detailsAndBorderBrowserTabbedPane.addTab("Data Model", dataModelPanel);

        jSplitPane4.setRightComponent(detailsAndBorderBrowserTabbedPane);
        detailsAndBorderBrowserTabbedPane.getAccessibleContext().setAccessibleName("Closure Border");

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel4.add(jSplitPane4, gridBagConstraints);

        jSplitPane1.setLeftComponent(jPanel4);

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

        getContentPane().add(jPanel1, java.awt.BorderLayout.CENTER);

        jMenu1.setText("File");

        newBrowserjMenuItem.setText("New Data Browser");
        newBrowserjMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                newBrowserjMenuItemActionPerformed(evt);
            }
        });
        jMenu1.add(newBrowserjMenuItem);
        jMenu1.add(jSeparator9);

        jMenuItem3.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_T, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItem3.setText("Open Table");
        jMenuItem3.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem3ActionPerformed(evt);
            }
        });
        jMenu1.add(jMenuItem3);
        jMenu1.add(jSeparator4);

        storeSessionItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_S, java.awt.event.InputEvent.CTRL_MASK));
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
        jMenu1.add(jSeparator2);

        reconnectMenuItem.setText("Reconnect...");
        reconnectMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                reconnectMenuItemActionPerformed(evt);
            }
        });
        jMenu1.add(reconnectMenuItem);
        jMenu1.add(jSeparator7);

        cloaseAllMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F4, java.awt.event.InputEvent.SHIFT_MASK | java.awt.event.InputEvent.CTRL_MASK));
        cloaseAllMenuItem.setText("Close All");
        cloaseAllMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cloaseAllMenuItemActionPerformed(evt);
            }
        });
        jMenu1.add(cloaseAllMenuItem);

        schemaMappingMenuItem.setText("Schema Mapping");
        schemaMappingMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                schemaMappingMenuItemActionPerformed(evt);
            }
        });
        jMenu1.add(schemaMappingMenuItem);

        menuBar.add(jMenu1);

        menuTools.setText("DataModel");

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
        menuTools.add(jSeparator10);

        showDataModelMenuItem.setText("Show Data Model");
        showDataModelMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                showDataModelMenuItemActionPerformed(evt);
            }
        });
        menuTools.add(showDataModelMenuItem);

        menuBar.add(menuTools);

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
        jMenu2.add(jSeparator3);

        consistencyCheckMenuItem.setText("Check Consistency");
        consistencyCheckMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                consistencyCheckMenuItemActionPerformed(evt);
            }
        });
        jMenu2.add(consistencyCheckMenuItem);

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
                        UIUtil.runJailer(this, args, false, true, false, false, null, dcd.getUser(), dcd.getPassword(), null, null, false, true, false, executionContext);
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
    		desktop.addTableBrowser(null, null, 0, datamodel.get().getTableByDisplayName(tableName), null, "", null, null, true);
    	}
    }//GEN-LAST:event_openTableButtonActionPerformed

    private void workbenchTabbedPaneStateChanged(javax.swing.event.ChangeEvent evt) {//GEN-FIRST:event_workbenchTabbedPaneStateChanged
        if (workbenchTabbedPane.getSelectedComponent() != sqlConsoleContainerPanel && sqlConsole != null) {
        	if (sqlConsole.getDataHasChanged()) {
        		try {
					desktop.reloadRoots();
				} catch (Exception e) {
					UIUtil.showException(this, "Error", e);
				}
        		sqlConsole.setDataHasChanged(false);
        	}
    		tableTreesTabbedPane.setSelectedComponent(navigationPanel);
        } else {
        	if (sqlConsole != null) {
        		tableTreesTabbedPane.setSelectedComponent(tablesPanel);
        		sqlConsole.grabFocus();
        	}
        }
    }//GEN-LAST:event_workbenchTabbedPaneStateChanged

    private void showDataModelMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_showDataModelMenuItemActionPerformed
    	detailsAndBorderBrowserTabbedPane.setSelectedComponent(dataModelPanel);
    }//GEN-LAST:event_showDataModelMenuItemActionPerformed

    private void consistencyCheckMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_consistencyCheckMenuItemActionPerformed
       	new ConstraintChecker(this, datamodel.get(), session) {
			@Override
			protected void openTableBrowser(Table source, String where) {
				workbenchTabbedPane.setSelectedComponent(desktopSplitPane);
	    		desktop.addTableBrowser(null, null, 0, source, null, new BasicFormatterImpl().format(where), null, null, true);
			}

			@Override
			protected void appendSQLConsole(String text) {
				sqlConsole.appendStatement(text, false);
				workbenchTabbedPane.setSelectedComponent(sqlConsoleContainerPanel);
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
            if (dbConnectionDialog.connect("Reconnect")) {
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

    private void cloaseAllMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_cloaseAllMenuItemActionPerformed
        desktop.closeAll();
    }// GEN-LAST:event_cloaseAllMenuItemActionPerformed

    private void schemaMappingMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_schemaMappingMenuItemActionPerformed
        desktop.openSchemaMappingDialog(false);
    }// GEN-LAST:event_schemaMappingMenuItemActionPerformed

    private void jMenuItem4ActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_jMenuItem4ActionPerformed
        try {
            BrowserLauncher.openURL(new URI("http://jailer.sourceforge.net/doc/data-browsing.html"));
        } catch (Exception e) {
            UIUtil.showException(this, "Error", e, session);
        }
    }// GEN-LAST:event_jMenuItem4ActionPerformed

    private void createExtractionModelMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_createExtractionModelMenuItemActionPerformed
        desktop.createExtractionModel(false);
    }// GEN-LAST:event_createExtractionModelMenuItemActionPerformed

    private void storeSessionItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_storeSessionItemActionPerformed
        desktop.storeSession();
    }// GEN-LAST:event_storeSessionItemActionPerformed

    private void restoreSessionItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_restoreSessionItemActionPerformed
        desktop.restoreSession(null);
    }// GEN-LAST:event_restoreSessionItemActionPerformed

    private void tinyLayoutRadioButtonMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_tinyLayoutRadioButtonMenuItemActionPerformed
        desktop.rescaleLayout(Desktop.LayoutMode.TINY, null);
        wheelzoomTip();
    }// GEN-LAST:event_tinyLayoutRadioButtonMenuItemActionPerformed

    private void wheelzoomTip() {
        TipDialog.showTip(this, "WHEELZOOM", "While holding down the Ctrl-key you can use the mouse-wheel to zoom in or out.");
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
        if (evt.getButton() == MouseEvent.BUTTON3) {
            if (evt.getClickCount() == 1) {
                TreePath node = navigationTree.getPathForLocation(evt.getX(), evt.getY());
                if (node == null) {
                    for (int x = navigationTree.getWidth(); x > 0; x -= 32) {
                        node = navigationTree.getPathForLocation(x, evt.getY());
                        if (node != null) {
                            break;
                        }
                    }
                }
                if (node != null) {
                    Object sel = node.getLastPathComponent();
                    if (sel instanceof DefaultMutableTreeNode) {
                        Object selNode = ((DefaultMutableTreeNode) sel).getUserObject();
                        RowBrowser rowBrowser = null;
                        int row = 0;
                        if (selNode instanceof TreeNodeForRowBrowser) {
                            rowBrowser = ((TreeNodeForRowBrowser) selNode).rowBrowser;
                            row = ((TreeNodeForRowBrowser) selNode).rowIndex;
                        }
                        if (rowBrowser != null) {
                            navigationTree.setSelectionRow(row);
                            JPopupMenu popup = rowBrowser.browserContentPane.createPopupMenu(null, -1, 0, 0, false);
                            if (popup != null) {
                                JPopupMenu popup2 = rowBrowser.browserContentPane.createSqlPopupMenu(null, -1, 0, 0, true);
                                popup.add(new JSeparator());
                                for (Component c : popup2.getComponents()) {
                                    popup.add(c);
                                }
                                UIUtil.fit(popup);
                                popup.show(evt.getComponent(), evt.getX(), evt.getY());
                            }
                        }
                    }
                }
            }
        }
    }// GEN-LAST:event_navigationTreeMouseClicked

    private void layoutMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_layoutMenuItemActionPerformed
        arrangeLayout();
    }// GEN-LAST:event_layoutMenuItemActionPerformed

    public void arrangeLayout() {
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        try {
            desktop.layoutBrowser();
        } finally {
            setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        }
    }

    private void jScrollPane1MouseWheelMoved(java.awt.event.MouseWheelEvent evt) {// GEN-FIRST:event_jScrollPane1MouseWheelMoved
        desktop.onMouseWheelMoved(evt);
        desktop.onMouseWheelMoved(evt, jScrollPane1);
    }// GEN-LAST:event_jScrollPane1MouseWheelMoved

    private void openNewTableBrowser(boolean offerAlternatives) {
        new NewTableBrowser(this, datamodel.get(), offerAlternatives) {
            @Override
            void openTableBrowser(String tableName) {
                desktop.addTableBrowser(null, null, 0, datamodel.get().getTableByDisplayName(tableName), null, "", null, null, true);
            }

            @Override
            void openDatabaseAnalyzer() {
                updateDataModel();
            }

            @Override
            void restoreSession() {
                desktop.restoreSession(null);
            }
        };
    }

    private void helpForumActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_helpForumActionPerformed
        try {
            BrowserLauncher.openURL(new URI("https://sourceforge.net/forum/?group_id=197260"));
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
        openDataModelEditor();
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
            UIUtil.showException(null, "Error", t);
        }
    }
    
    /**
     * @param args
     *            the command line arguments
     */
    private static void start(final String args[]) {
        Environment.init();

        // turn off logging for prefuse library
        try {
            Logger.getLogger("prefuse").setLevel(Level.OFF);
        } catch (Exception e1) {
            e1.printStackTrace();
        }
        try {
            CommandLineInstance.init(args);
        } catch (Exception e) {
            UIUtil.showException(null, "Illegal arguments", e);
            return;
        }
        try {
            System.setProperty("db2.jcc.charsetDecoderEncoder", "3");
        } catch (Exception e) {
        }
        try {
            // create initial data-model files
            File file = new File(DataModel.getDatamodelFolder(new ExecutionContext(CommandLineInstance.getInstance())));
            if (!file.exists()) {
                file.mkdir();
            }
        } catch (Exception e) {
        }
        java.awt.EventQueue.invokeLater(new Runnable() {
            public void run() {
                try {
                    try {
                        // L&F can no longer be changed
//						File plafSetting = new File(PLAFSETTING);
                        String plaf;
//						if (!plafSetting.exists()) {
                            plaf = "com.sun.java.swing.plaf.nimbus.NimbusLookAndFeel";
//						} else {
//							BufferedReader in = new BufferedReader(new FileReader(plafSetting));
//							plaf = in.readLine();
//							in.close();
//						}
                        UIManager.setLookAndFeel(plaf);
                        ((InputMap) UIManager.get("Button.focusInputMap")).put(KeyStroke.getKeyStroke("pressed ENTER"), "pressed");
                        ((InputMap) UIManager.get("Button.focusInputMap")).put(KeyStroke.getKeyStroke("released ENTER"), "released");
                        Object dSize = UIManager.get("SplitPane.dividerSize");
                        if (new Integer(10).equals(dSize)) {
                        	UIManager.put("SplitPane.dividerSize", new Integer(14));
                        }
                    } catch (Exception x) {
                    }
//					try {
//						initInterceptingEventQueue();
//					} catch (Exception x) {
//					}
                    ToolTipManager.sharedInstance().setInitialDelay(400);
                    ToolTipManager.sharedInstance().setDismissDelay(20000);

                    createFrame();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        });
    }

    private static DataBrowser openNewDataBrowser(DataModel datamodel, DbConnectionDialog dbConnectionDialog, boolean maximize, ExecutionContext executionContext, DataBrowser theDataBrowser) throws Exception {
        DataBrowser dataBrowser = theDataBrowser != null? theDataBrowser : new DataBrowser(datamodel, null, "", null, false, executionContext);
        dataBrowser.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
//		if (maximize) {
//			dataBrowser.setExtendedState(JFrame.MAXIMIZED_BOTH);
//		}
        dataBrowser.setVisible(true);
        dataBrowser.setExtendedState(JFrame.MAXIMIZED_BOTH);

        if (dbConnectionDialog == null) {
            dbConnectionDialog = new DbConnectionDialog(dataBrowser, DataBrowserContext.getAppName(), null, executionContext);
        } else {
            dbConnectionDialog = new DbConnectionDialog(dataBrowser, dbConnectionDialog, DataBrowserContext.getAppName(), executionContext);
        }
        if (dbConnectionDialog.isConnected || dbConnectionDialog.connect(DataBrowserContext.getAppName(true))) {
            dataBrowser.setConnection(dbConnectionDialog);
            dataBrowser.askForDataModel();
            dataBrowser.desktop.openSchemaMappingDialog(true);
            dataBrowser.updateStatusBar();
        } else {
            if (dbConnectionDialog.isConnected) {
                dataBrowser.setConnection(dbConnectionDialog);
            } else {
            	dataBrowser.dbConnectionDialog = dbConnectionDialog;
            }
            for (int i = 0; i < dataBrowser.menuBar.getMenuCount(); ++i) {
                JMenu menu = dataBrowser.menuBar.getMenu(i);
                if (menu != dataBrowser.helpMenu) {
                    for (int j = 0; j < menu.getItemCount(); ++j) {
                        JMenuItem item = menu.getItem(j);
                        if (item != null) {
                        	if (item != dataBrowser.reconnectMenuItem && item != dataBrowser.newBrowserjMenuItem) {
                        		item.setEnabled(false);
                        	}
                        }
                    }
                }
            }
        }
        return dataBrowser;
    }

    private static void createFrame() {
        DataModelManagerDialog dataModelManagerDialog = new DataModelManagerDialog(DataBrowserContext.getAppName(true)
                + " - Relational Data Browser") {
            @Override
            protected void onSelect(final DbConnectionDialog connectionDialog, final ExecutionContext executionContext) {
                try {
                    final DataModel datamodel;
                    datamodel = new DataModel(executionContext);
                	final DataBrowser databrowser = new DataBrowser(datamodel, null, "", null, false, executionContext);
                    SwingUtilities.invokeLater(new Runnable() {
						@Override
						public void run() {
			                try {
								openNewDataBrowser(datamodel, connectionDialog, true, executionContext, databrowser);
			                } catch (Exception e) {
			                    UIUtil.showException(null, "Error", e);
			                }
						}
					});
                } catch (Exception e) {
                    UIUtil.showException(null, "Error", e);
                }
            }
			private static final long serialVersionUID = 1L;
        };
        dataModelManagerDialog.setVisible(true);
    }

    /**
     * Opens the data model editor.
     */
    private void openDataModelEditor() {
        try {
            String modelname = datamodel == null || datamodel.get() == null ? DataModel.DEFAULT_NAME : datamodel.get().getName();
            DataModelEditor dataModelEditor = new DataModelEditor(this, false, false, null, null, null, modelname, null, executionContext);
            dataModelEditor.setVisible(true);
            removeMetaDataSource(session);
            desktop.reloadDataModel(desktop.schemaMapping);
            dataModelViewFrame = null;
            updateDataModelView(null);
            updateStatusBar();
            askForDataModel();
        } catch (Exception e) {
            UIUtil.showException(this, "Error", e, session);
        }
    }

    private void updateDataModel() {
    	updateDataModel(null, false, false);
    }

    private void updateDataModel(String schemaName, boolean withViews, boolean withSynonyms) {
        try {
            List<String> args = new ArrayList<String>();
            args.add("build-model-wo-merge");
            dbConnectionDialog.addDbArgs(args);

            AnalyseOptionsDialog analyseOptionsDialog = new AnalyseOptionsDialog(this, datamodel == null ? null : datamodel.get(), executionContext);
            analyseOptionsDialog.setInitiallyWithViews(withViews);
            analyseOptionsDialog.setInitiallyWithSynonyms(withSynonyms);
            boolean[] isDefaultSchema = new boolean[1];
            String[] defaultSchema = new String[1];
            List<String> schemas;
            setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
            try {
				CancellationHandler.reset(null);
                schemas = dbConnectionDialog.getDBSchemas(defaultSchema);
            } finally {
                setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
            }
            if (analyseOptionsDialog.edit(schemas, defaultSchema[0], schemaName == null? null : Quoting.staticUnquote(schemaName), isDefaultSchema, dbConnectionDialog.currentConnection.user)) {
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
                if (UIUtil.runJailer(this, args, false, true, false, true, null, dbConnectionDialog.getUser(), dbConnectionDialog.getPassword(), null, null, false, true, false, executionContext)) {
                    ModelBuilder.assocFilter = null;
                    String modelname = datamodel == null || datamodel.get() == null ? DataModel.DEFAULT_NAME : datamodel.get().getName();
                    DataModelEditor dataModelEditor = new DataModelEditor(this, true, analyseOptionsDialog.isRemoving(), null,
                            analyseOptionsDialog.getTableLineFilter(), analyseOptionsDialog.getAssociationLineFilter(), modelname,
                            schema == null ? dbConnectionDialog.getName() : schema, executionContext);
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
        }
    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JMenuItem aboutMenuItem;
    private javax.swing.JMenuItem analyseMenuItem;
    private javax.swing.JLabel associatedWith;
    private javax.swing.JPanel borderBrowserPanel;
    private javax.swing.JPanel borderBrowserTabPane;
    private javax.swing.JMenuItem cloaseAllMenuItem;
    private javax.swing.JPanel closurePanel;
    public javax.swing.JLabel connectivityState;
    private javax.swing.JMenuItem consistencyCheckMenuItem;
    private javax.swing.JMenuItem createExtractionModelMenuItem;
    private javax.swing.JMenuItem dataImport;
    private javax.swing.JMenuItem dataModelEditorjMenuItem;
    private javax.swing.JPanel dataModelPanel;
    private javax.swing.JLabel dependsOn;
    private javax.swing.JSplitPane desktopSplitPane;
    private javax.swing.JTabbedPane detailsAndBorderBrowserTabbedPane;
    private javax.swing.JPanel dummy;
    private javax.swing.JMenuItem exportDataMenuItem;
    private javax.swing.JLabel hasDependent;
    private javax.swing.JMenuItem helpForum;
    private javax.swing.JMenu helpMenu;
    private javax.swing.JPanel hiddenPanel;
    private javax.swing.JLabel ignored;
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
    private javax.swing.JMenu jMenu1;
    private javax.swing.JMenu jMenu2;
    private javax.swing.JMenuItem jMenuItem3;
    private javax.swing.JMenuItem jMenuItem4;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel11;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanel7;
    private javax.swing.JPanel jPanel8;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane3;
    private javax.swing.JPopupMenu.Separator jSeparator1;
    private javax.swing.JPopupMenu.Separator jSeparator10;
    private javax.swing.JPopupMenu.Separator jSeparator2;
    private javax.swing.JPopupMenu.Separator jSeparator3;
    private javax.swing.JPopupMenu.Separator jSeparator4;
    private javax.swing.JPopupMenu.Separator jSeparator5;
    private javax.swing.JPopupMenu.Separator jSeparator6;
    private javax.swing.JPopupMenu.Separator jSeparator7;
    private javax.swing.JPopupMenu.Separator jSeparator8;
    private javax.swing.JPopupMenu.Separator jSeparator9;
    private javax.swing.JSplitPane jSplitPane1;
    private javax.swing.JSplitPane jSplitPane4;
    private javax.swing.JTable jTable1;
    private javax.swing.JRadioButtonMenuItem largeLayoutRadioButtonMenuItem;
    private javax.swing.JPanel layeredPaneContent;
    private javax.swing.JMenuItem layoutMenuItem;
    private javax.swing.JPanel legende;
    private javax.swing.JPanel legende1;
    private javax.swing.JPanel legende2;
    private javax.swing.JRadioButtonMenuItem mediumLayoutRadioButtonMenuItem;
    private javax.swing.JMenuBar menuBar;
    private javax.swing.JMenu menuTools;
    private javax.swing.JMenu menuWindow;
    private javax.swing.JPanel metaDataViewPanel;
    private javax.swing.JLabel modelName;
    private javax.swing.JLabel modelPath;
    private javax.swing.JPanel navigationPanel;
    private javax.swing.JTree navigationTree;
    private javax.swing.JScrollPane navigationTreeScrollPane;
    private javax.swing.JMenuItem newBrowserjMenuItem;
    private javax.swing.JMenuItem newWindowMenuItem;
    private javax.swing.JButton openTableButton;
    private javax.swing.JMenuItem reconnectMenuItem;
    private javax.swing.JMenuItem restoreSessionItem;
    private javax.swing.JMenuItem schemaMappingMenuItem;
    private javax.swing.JLabel schemaName;
    private javax.swing.JPanel schemaNamePanel;
    private javax.swing.JCheckBoxMenuItem showDataModelMenuItem;
    private javax.swing.JRadioButtonMenuItem smallLayoutRadioButtonMenuItem;
    private javax.swing.JPanel sqlConsoleContainerPanel;
    private javax.swing.JMenuItem storeSessionItem;
    private javax.swing.JTabbedPane tableTreesTabbedPane;
    private javax.swing.JPanel tablesPanel;
    private javax.swing.JRadioButtonMenuItem thumbnailLayoutRadioButtonMenuItem;
    private javax.swing.JRadioButtonMenuItem tinyLayoutRadioButtonMenuItem;
    private javax.swing.JLabel titleLabel;
    private javax.swing.JMenu view;
    private javax.swing.JTabbedPane workbenchTabbedPane;
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
        try {
            if (datamodel.get().getTables().isEmpty()) {
                switch (JOptionPane.showOptionDialog(this, "Data model \"" + DataModelManager.getModelDetails(DataModelManager.getCurrentModelSubfolder(executionContext), executionContext).a
                        + "\" is empty.", DataBrowserContext.getAppName(true), JOptionPane.YES_NO_OPTION, JOptionPane.INFORMATION_MESSAGE, null, new Object[] {
                        "Analyze Database", "Data Model Editor" }, null)) {
                case 0:
                    updateDataModel();
                    break;
                case 1:
                    openDataModelEditor();
                    break;
                }
            } else if (!new File(DataModel.getColumnsFile(executionContext)).exists()) {
                switch (JOptionPane.showOptionDialog(this, "No column definition found.", DataBrowserContext.getAppName(true), JOptionPane.YES_NO_OPTION,
                        JOptionPane.INFORMATION_MESSAGE, null, new Object[] { "Analyze Database", "Data Model Editor" }, null)) {
                case 0:
                    updateDataModel();
                    break;
                case 1:
                    openDataModelEditor();
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
            if (other instanceof BrowserAssociationModel) {
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
        if (disableBorderBrowserUpdates || detailsAndBorderBrowserTabbedPane.getSelectedComponent() != borderBrowserTabPane) {
            return;
        }
        try {
            setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
            
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
            setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        }
    }

    private ExtractionModelFrame dataModelViewFrame;
    private Table lastFocusTable = null;
    private Table currentTableInDMV = null;
     
	private void updateDataModelView(final Table table) {
		if (table != null) {
			lastFocusTable = table;
		}
		if (detailsAndBorderBrowserTabbedPane.getSelectedComponent() != dataModelPanel) {
			return;
		}
		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				try {
					setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
					if (dataModelViewFrame == null) {
						dataModelViewFrame = ExtractionModelFrame.createFrame(null, false, false, null, executionContext);
						JComponent graphViewContainer = dataModelViewFrame.tearOutGraphViewContainer();
						dataModelPanel.removeAll();
						dataModelPanel.add(graphViewContainer);
					}
                    if (table != null) {
                    	dataModelViewFrame.select(table);
                    } else {
                    	Table toSelect = null;
                    	if (lastFocusTable != null && tableTreesTabbedPane.getSelectedComponent() == tablesPanel) {
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
				} finally {
					setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
				}
			}
		});
	}
    
    protected void resolveSelection(Collection<AssociationModel> selection) {
        try {
            setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
            
            disableBorderBrowserUpdates = true;
            JInternalFrame currentSelection = desktop.getSelectedFrame();
            for (AssociationModel a : selection) {
                BrowserAssociationModel associationModel = (BrowserAssociationModel) a;
                desktop.addTableBrowser(associationModel.getRowBrowser(), associationModel.getRowBrowser(), -1, associationModel.getAssociation().destination, associationModel.getAssociation(),
                        "", null, null, true);
            }
            if (currentSelection != null) {
                try {
                    currentSelection.setSelected(true);
                } catch (PropertyVetoException e) {
                    // ignore
                }
            }
        } finally {
            setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
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

        public String toString() {
            return title;
        }
    }

    private void updateNavigationTree() {
        if (navigationTreeListener != null) {
            navigationTree.getSelectionModel().removeTreeSelectionListener(navigationTreeListener);
        }

        ConnectionInfo connection = dbConnectionDialog != null ? dbConnectionDialog.currentConnection : null;
        DefaultMutableTreeNode root = new DefaultMutableTreeNode(connection != null ? " " + connection.alias : " ");

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
                                iFrame.setSelected(true);
                                iFrame.grabFocus();
                            } catch (PropertyVetoException e1) {
                                // ignore
                            }
                            return;
                        } else {
                            openNewTableBrowser(false);
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
                if (prevLevel == null || prevLevel < level) {
                	visibleTables.put(rowBrowser.browserContentPane.table, rowBrowser);
                	levels.put(rowBrowser.browserContentPane.table, level);
                }
                for (RowBrowser cb: rowBrowser.browserContentPane.getChildBrowsers()) {
                    collectVisibleTables(cb, level + 1);
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
        				disableBorderBrowserUpdates = true;
        	            suppressUpdateClosureBrowser = true;
	            		while (i > 0) {
		            		Table table = path.get(i);
		            		RowBrowser rb = getVisibleTables().get(table);
		            		Association association = associations[i - 1];
		            		if (association != null) {
		            			rb.browserContentPane.navigateTo(association, -1, null);
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
						disableBorderBrowserUpdates = false;
			            suppressUpdateClosureBrowser = false;
					}
	            	closureView.find(getDataModel().getDisplayName(path.get(0)));
        		}
            }

            protected void select(String selectedTable) {
				 try {
					 if (selectedTable != null) {
						 RowBrowser rb = getVisibleTables().get(datamodel.get().getTableByDisplayName(selectedTable));
						 if (rb != null) {
							 JInternalFrame iFrame = rb.internalFrame;
							 desktop.scrollToCenter(iFrame);
							 iFrame.setSelected(true);
							 iFrame.grabFocus();
						 }
					 }
                } catch (PropertyVetoException e1) {
                    // ignore
                }
			}

			private Association[] openAssociationPathPanel(List<Table> path) {
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
    
    private void onNewSession(Session newSession) {
    	ConnectionInfo connection = dbConnectionDialog != null ? dbConnectionDialog.currentConnection : null;
    	String alias = connection != null ? " " + connection.alias : " ";
    	
    	setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
    	try {
	    	updateNavigationCombobox();
	    	
	    	tablesPanel.removeAll();
	    	metaDataPanel = (MetaDataPanel) session.getSessionProperty(getClass(), "metaDataPanel");
			MetaDataSource metaDataSource;
			try {
				metaDataSource = getMetaDataSource(newSession);
				if (metaDataSource == null || Boolean.TRUE.equals(session.getSessionProperty(DataBrowser.class, "removeMetaDataSource"))) {
					metaDataSource = new MetaDataSource(newSession, datamodel.get(), alias, executionContext);
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
			
			if (metaDataPanel == null) {
				metaDataPanel = new MetaDataPanel(this, metaDataSource, metaDataDetailsPanel, datamodel.get(), executionContext) {
					@Override
					protected void open(Table table) {
						if (!selectNavTreeNode(navigationTree.getModel().getRoot(), table)) {
							if (workbenchTabbedPane.getSelectedComponent() != sqlConsoleContainerPanel) {
								desktop.addTableBrowser(null, null, 0, table, null, "", null, null, true);
							}
						}
						try {
							String sql;
							Quoting quoting = new Quoting(session);
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
							if (workbenchTabbedPane.getSelectedComponent() == sqlConsoleContainerPanel) {
								workbenchTabbedPane.setSelectedComponent(sqlConsoleContainerPanel);
								sqlConsole.grabFocus();
								sqlConsole.appendStatement(sql, true);
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
							Quoting quoting = new Quoting(session);
							String sql = "Select * From " + (schemaName == null? "" : quoting.quote(schemaName) + ".") + quoting.quote(tableName);
							if (!selectNavTreeNode(navigationTree.getModel().getRoot(), mdTable)
								|| workbenchTabbedPane.getSelectedComponent() == sqlConsoleContainerPanel) {
								workbenchTabbedPane.setSelectedComponent(sqlConsoleContainerPanel);
								sqlConsole.grabFocus();
								sqlConsole.appendStatement(sql, true);
							}
						} catch (SQLException e) {
							UIUtil.showException(this, "Error", e);
						}
					}
		
					@Override
					protected void onTableSelect(MDTable mdTable) {
						metaDataDetailsPanel
							.showMetaDataDetails(mdTable, getMetaDataSource(session).toTable(mdTable), datamodel.get());
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
						sqlConsole.setCaretPosition(position);
					}
				};
			}
	    	session.setSessionProperty(getClass(), "metaDataPanel", metaDataPanel);
			tablesPanel.add(metaDataPanel, java.awt.BorderLayout.CENTER);
			
			try {
				if (sqlConsole == null) {
					sqlConsole = new SQLConsole(session, metaDataSource, datamodel, executionContext) {
						@Override
						protected void refreshMetaData() {
							metaDataPanel.reset();
						}
						@Override
						protected void selectTable(MDTable mdTable) {
							metaDataPanel.select(mdTable);
						}
						@Override
						protected void setOutlineTables(List<OutlineInfo> outlineTables, int indexOfInfoAtCaret) {
							metaDataPanel.setOutline(outlineTables, indexOfInfoAtCaret);
						}
					};
					sqlConsoleContainerPanel.removeAll();
					sqlConsoleContainerPanel.add(sqlConsole);
				} else {
					sqlConsole.reset(session, metaDataSource);
				}
			} catch (SQLException e) {
				e.printStackTrace();
			}
    	}
		finally {
			setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		}
	}
    
	private MetaDataDetailsPanel metaDataDetailsPanel;
	private SQLConsole sqlConsole;

	public MetaDataSource getMetaDataSource() {
		return getMetaDataSource(session);
	}
	
}
