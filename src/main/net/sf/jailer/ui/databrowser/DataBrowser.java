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

import java.awt.Component;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Dimension;
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
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JInternalFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.JSeparator;
import javax.swing.JToggleButton;
import javax.swing.JViewport;
import javax.swing.SwingUtilities;
import javax.swing.ToolTipManager;
import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;
import javax.swing.event.MouseInputAdapter;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;

import jsyntaxpane.DefaultSyntaxKit;
import net.sf.jailer.CommandLineParser;
import net.sf.jailer.Jailer;
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
import net.sf.jailer.ui.BrowserLauncher;
import net.sf.jailer.ui.DataModelEditor;
import net.sf.jailer.ui.DataModelManager;
import net.sf.jailer.ui.DataModelManagerDialog;
import net.sf.jailer.ui.DbConnectionDialog;
import net.sf.jailer.ui.DbConnectionDialog.ConnectionInfo;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.Desktop.LayoutMode;
import net.sf.jailer.ui.databrowser.Desktop.RowBrowser;

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
		this.dbConnectionDialog = dbConnectionDialog != null ? new DbConnectionDialog(this, dbConnectionDialog, DataBrowserContext.getAppName()) : null;
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
		hiddenPanel.setVisible(false);
		borderBrowserPanel.add(borderBrowser, java.awt.BorderLayout.CENTER);

		GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.NONE;
        gridBagConstraints.weightx = 0;
        gridBagConstraints.weighty = 0;
        jPanel4.add(new JPanel() {
			@Override
			public Dimension getMinimumSize() {
				return new Dimension(1, 300);
			}
			private static final long serialVersionUID = -947582621664272477L;
		}, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.NONE;
        gridBagConstraints.weightx = 0;
        gridBagConstraints.weighty = 0;
        borderBrowserTitledPanel.add(new JPanel() {
			@Override
			public Dimension getMinimumSize() {
				return new Dimension(1, 180);
			}
			private static final long serialVersionUID = -947582621664272477L;
		}, gridBagConstraints);
        
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

		if (DataBrowserContext.isStandAlone()) {
			aboutMenuItem.setText("About " + DataBrowserContext.getAppName(true));
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
		desktop = new Desktop(this.datamodel, jailerIcon, session, this, dbConnectionDialog) {
			@Override
			public void openSchemaAnalyzer() {
				updateDataModel();
			}

			@Override
			protected void updateMenu(boolean hasTableBrowser, boolean hasIFrame) {
				storeSessionItem.setEnabled(hasIFrame);
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
					return DataBrowser.openNewDataBrowser(DataBrowser.this.datamodel.get(), dbConnectionDialog, false);
				} catch (Exception e) {
					UIUtil.showException(this, "Error", e);
					return null;
				}
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
//				desktop.stop();
			}

			@Override
			public void windowClosed(WindowEvent e) {
				desktop.stop();
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

		setLocation(60, 50);
		setSize(900, 640);
		if (root != null) {
			desktop.addTableBrowser(null, 0, root, null, condition, null, null, true);
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

	private void createSession(DbConnectionDialog dbConnectionDialog) throws Exception {
		ConnectionInfo connection = dbConnectionDialog.currentConnection;
		session = new Session(connection.driverClass, connection.url, connection.user, connection.password);
		List<String> args = new ArrayList<String>();
		dbConnectionDialog.addDbArgs(args);
		session.setCliArguments(args);
		session.setPassword(dbConnectionDialog.getPassword());
	}

	protected void setConnection(DbConnectionDialog dbConnectionDialog) throws Exception {
		if (dbConnectionDialog != null) {
			dbConnectionDialog = new DbConnectionDialog(this, dbConnectionDialog, DataBrowserContext.getAppName());
		}
		this.dbConnectionDialog = dbConnectionDialog;
		desktop.dbConnectionDialog = dbConnectionDialog;
		if (dbConnectionDialog != null) {
			ConnectionInfo connection = dbConnectionDialog.currentConnection;
			if (connection != null) {
				createSession(dbConnectionDialog);
				desktop.session = session;
				// trigger sql dialect guessing
				datamodel.get().getUniversalPrimaryKey(session);
				updateStatusBar();
				if (desktop != null) {
					desktop.updateMenu();
					for (RowBrowser rb: desktop.getBrowsers()) {
						rb.browserContentPane.session = session;
						rb.browserContentPane.rows.clear();
					}
					for (RowBrowser rb: desktop.getRootBrowsers(false)) {
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

		String modelpath = CommandLineParser.getInstance().getDataModelFolder();
		try {
			modelpath = CommandLineParser.getInstance().newFile(modelpath).getAbsolutePath();
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
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

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
        jSplitPane2 = new javax.swing.JSplitPane();
        jPanel4 = new javax.swing.JPanel();
        navigationTreeScrollPane = new javax.swing.JScrollPane();
        navigationTree = new javax.swing.JTree();
        jLabel2 = new javax.swing.JLabel();
        borderBrowserTitledPanel = new javax.swing.JPanel();
        titleLabel = new javax.swing.JLabel();
        borderBrowserPanel = new javax.swing.JPanel();
        jPanel5 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        jInternalFrame1 = new javax.swing.JInternalFrame();
        jLabel1 = new javax.swing.JLabel();
        hiddenPanel = new javax.swing.JPanel();
        menuBar = new javax.swing.JMenuBar();
        jMenu1 = new javax.swing.JMenu();
        jMenuItem1 = new javax.swing.JMenuItem();
        jMenuItem3 = new javax.swing.JMenuItem();
        jSeparator4 = new javax.swing.JPopupMenu.Separator();
        storeSessionItem = new javax.swing.JMenuItem();
        restoreSessionItem = new javax.swing.JMenuItem();
        jSeparator2 = new javax.swing.JPopupMenu.Separator();
        reconnectMenuItem = new javax.swing.JMenuItem();
        jSeparator7 = new javax.swing.JPopupMenu.Separator();
        cloaseAllMenuItem = new javax.swing.JMenuItem();
        jSeparator3 = new javax.swing.JPopupMenu.Separator();
        schemaMappingMenuItem = new javax.swing.JMenuItem();
        menuTools = new javax.swing.JMenu();
        analyseMenuItem = new javax.swing.JMenuItem();
        dataModelEditorjMenuItem = new javax.swing.JMenuItem();
        jMenu2 = new javax.swing.JMenu();
        createExtractionModelMenuItem = new javax.swing.JMenuItem();
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

        jPanel1.setLayout(new java.awt.GridBagLayout());

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

        connectivityState.setFont(new java.awt.Font("Dialog", 0, 12));
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

        jSplitPane2.setDividerLocation(350);
        jSplitPane2.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        jSplitPane2.setResizeWeight(1.0);
        jSplitPane2.setContinuousLayout(true);
        jSplitPane2.setOneTouchExpandable(true);

        jPanel4.setBorder(null);
        jPanel4.setLayout(new java.awt.GridBagLayout());

        navigationTree.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                navigationTreeMouseClicked(evt);
            }
        });
        navigationTreeScrollPane.setViewportView(navigationTree);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel4.add(navigationTreeScrollPane, gridBagConstraints);

        jLabel2.setText(" Navigation Tree");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        jPanel4.add(jLabel2, gridBagConstraints);

        jSplitPane2.setLeftComponent(jPanel4);

        borderBrowserTitledPanel.setBorder(null);
        borderBrowserTitledPanel.setLayout(new java.awt.GridBagLayout());

        titleLabel.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(6, 0, 0, 0);
        borderBrowserTitledPanel.add(titleLabel, gridBagConstraints);

        borderBrowserPanel.setLayout(new java.awt.BorderLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        borderBrowserTitledPanel.add(borderBrowserPanel, gridBagConstraints);

        jSplitPane2.setRightComponent(borderBrowserTitledPanel);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel3.add(jSplitPane2, gridBagConstraints);

        jSplitPane1.setLeftComponent(jPanel3);

        jPanel5.setLayout(new java.awt.BorderLayout());

        jScrollPane1.setAutoscrolls(true);
        jScrollPane1.setWheelScrollingEnabled(false);
        jScrollPane1.addMouseWheelListener(new java.awt.event.MouseWheelListener() {
            public void mouseWheelMoved(java.awt.event.MouseWheelEvent evt) {
                jScrollPane1MouseWheelMoved(evt);
            }
        });

        jInternalFrame1.setVisible(true);

        jLabel1.setText("jLabel1");
        jInternalFrame1.getContentPane().add(jLabel1, java.awt.BorderLayout.CENTER);

        jScrollPane1.setViewportView(jInternalFrame1);

        jPanel5.add(jScrollPane1, java.awt.BorderLayout.CENTER);

        hiddenPanel.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "Hidden"));
        hiddenPanel.setLayout(new java.awt.GridBagLayout());
        jPanel5.add(hiddenPanel, java.awt.BorderLayout.SOUTH);

        jSplitPane1.setRightComponent(jPanel5);

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

        jMenuItem1.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_T, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItem1.setText("New Table Browser");
        jMenuItem1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem1ActionPerformed(evt);
            }
        });
        jMenu1.add(jMenuItem1);

        jMenuItem3.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Q, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItem3.setText("New SQL Browser");
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
        jMenu1.add(jSeparator3);

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

        menuBar.add(menuTools);

        jMenu2.setText("Tools");

        createExtractionModelMenuItem.setText("Create Extraction Model");
        createExtractionModelMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                createExtractionModelMenuItemActionPerformed(evt);
            }
        });
        jMenu2.add(createExtractionModelMenuItem);

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

        private void newWindowMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_newWindowMenuItemActionPerformed
            try {
				openNewDataBrowser(datamodel.get(), dbConnectionDialog, false);
			} catch (Exception e) {
				UIUtil.showException(this, "Error", e);
			}
        }//GEN-LAST:event_newWindowMenuItemActionPerformed

        private void reconnectMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_reconnectMenuItemActionPerformed
            synchronized (this) {
	        	if (dbConnectionDialog.connect("Reconnect")) {
	            	try {
						setConnection(dbConnectionDialog);
					} catch (Exception e) {
						UIUtil.showException(this, "Error", e);
					}
	        	}
            }
        }//GEN-LAST:event_reconnectMenuItemActionPerformed

        private void thumbnailLayoutRadioButtonMenuItemActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_thumbnailLayoutRadioButtonMenuItemActionPerformed
        	desktop.rescaleLayout(Desktop.LayoutMode.THUMBNAIL, null);
}//GEN-LAST:event_thumbnailLayoutRadioButtonMenuItemActionPerformed

	private void jMenuItem1ActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_jMenuItem1ActionPerformed
		openNewTableBrowser(false);
	}// GEN-LAST:event_jMenuItem1ActionPerformed

	private void cloaseAllMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_cloaseAllMenuItemActionPerformed
		desktop.closeAll();
	}// GEN-LAST:event_cloaseAllMenuItemActionPerformed

	private void schemaMappingMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_schemaMappingMenuItemActionPerformed
		desktop.openSchemaMappingDialog(false);
	}// GEN-LAST:event_schemaMappingMenuItemActionPerformed

	private void jMenuItem3ActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_jMenuItem3ActionPerformed
		desktop.addTableBrowser(null, 0, null, null, "", null, null, true);
	}// GEN-LAST:event_jMenuItem3ActionPerformed

	private void jMenuItem4ActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_jMenuItem4ActionPerformed
		try {
			if (DataBrowserContext.isStandAlone()) {
				BrowserLauncher.openURL(new URI("http://dbeauty.sourceforge.net"));
			} else {
				BrowserLauncher.openURL(new URI("http://jailer.sourceforge.net/doc/data-browsing.html"));
			}
		} catch (Exception e) {
			UIUtil.showException(this, "Error", e);
		}
	}// GEN-LAST:event_jMenuItem4ActionPerformed

	private void createExtractionModelMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_createExtractionModelMenuItemActionPerformed
		desktop.createExtractionModel();
	}// GEN-LAST:event_createExtractionModelMenuItemActionPerformed

	private void storeSessionItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_storeSessionItemActionPerformed
		desktop.storeSession();
	}// GEN-LAST:event_storeSessionItemActionPerformed

	private void restoreSessionItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_restoreSessionItemActionPerformed
		desktop.restoreSession(null);
	}// GEN-LAST:event_restoreSessionItemActionPerformed

	private void tinyLayoutRadioButtonMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_tinyLayoutRadioButtonMenuItemActionPerformed
		desktop.rescaleLayout(Desktop.LayoutMode.TINY, null);
	}// GEN-LAST:event_tinyLayoutRadioButtonMenuItemActionPerformed

	private void smallLayoutRadioButtonMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_smallLayoutRadioButtonMenuItemActionPerformed
		desktop.rescaleLayout(Desktop.LayoutMode.SMALL, null);
	}// GEN-LAST:event_smallLayoutRadioButtonMenuItemActionPerformed

	private void mediumLayoutRadioButtonMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_mediumLayoutRadioButtonMenuItemActionPerformed
		desktop.rescaleLayout(Desktop.LayoutMode.MEDIUM, null);
	}// GEN-LAST:event_mediumLayoutRadioButtonMenuItemActionPerformed

	private void largeLayoutRadioButtonMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_largeLayoutRadioButtonMenuItemActionPerformed
		desktop.rescaleLayout(Desktop.LayoutMode.LARGE, null);
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
							JPopupMenu popup2 = rowBrowser.browserContentPane.createSqlPopupMenu(null, -1, 0, 0, true);
							popup.add(new JSeparator());
							for (Component c: popup2.getComponents()) {
								popup.add(c);
							}
							popup.show(evt.getComponent(), evt.getX(), evt.getY());
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
	}// GEN-LAST:event_jScrollPane1MouseWheelMoved

	private void openNewTableBrowser(boolean offerAlternatives) {
		new NewTableBrowser(this, datamodel.get(), offerAlternatives) {
			@Override
			void openTableBrowser(String tableName) {
				desktop.addTableBrowser(null, 0, datamodel.get().getTableByDisplayName(tableName), null, "", null, null, true);
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
			if (DataBrowserContext.isStandAlone()) {
				BrowserLauncher.openURL(new URI("https://sourceforge.net/apps/phpbb/dbeauty/index.php"));
			} else {
				BrowserLauncher.openURL(new URI("https://sourceforge.net/forum/?group_id=197260"));
			}
		} catch (Exception e) {
			UIUtil.showException(this, "Error", e);
		}
	}// GEN-LAST:event_helpForumActionPerformed

	private void aboutMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_jMenuItem4ActionPerformed
		About about = new About(this, true);
		about.setTitle(DataBrowserContext.getAppName(false));
		if (DataBrowserContext.isStandAlone()) {
			about.homeTextField.setText("http://dbeauty.sourceforge.net");
			about.forumTextField.setText("https://sourceforge.net/apps/phpbb/dbeauty/index.php");
			about.nameLabel.setText(DataBrowserContext.getAppName(false));
		}
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
	 * @param args
	 *            the command line arguments
	 */
	public static void main(final String args[]) {

		// UIUtil.showMaxMemory();

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
					DefaultSyntaxKit.initKit();
				} catch (Throwable e) {
					e.printStackTrace();
				}
				try {
					CommandLineParser.parse(args, true);
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
					DataModelManagerDialog dataModelManagerDialog = new DataModelManagerDialog(DataBrowserContext.getAppName(true) + " - Relational Data Browser") {
						@Override
						protected void onSelect() {
			            	try {
			            		final DataModel datamodel;
			            		datamodel = new DataModel();
								openNewDataBrowser(datamodel, null, true); 
							} catch (Exception e) {
								UIUtil.showException(null, "Error", e);
							}
						}
						private static final long serialVersionUID = 1L;
	            	};
					ToolTipManager.sharedInstance().setInitialDelay(400);
					ToolTipManager.sharedInstance().setDismissDelay(20000);
	            	
					dataModelManagerDialog.setVisible(true);
	            } catch (Exception e) {
					e.printStackTrace();
				}
			}
		});
	}

	private static DataBrowser openNewDataBrowser(DataModel datamodel, DbConnectionDialog dbConnectionDialog, boolean maximize) throws Exception {
		boolean silent = dbConnectionDialog != null;
		DataBrowser dataBrowser = new DataBrowser(datamodel, null, "", null, false);
		dataBrowser.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		if (maximize) {
			dataBrowser.setExtendedState(JFrame.MAXIMIZED_BOTH);
		}
		dataBrowser.setVisible(true);
		if (dbConnectionDialog == null) {
			dbConnectionDialog = new DbConnectionDialog(dataBrowser, DataBrowserContext.getAppName(), null);
		} else {
			dbConnectionDialog = new DbConnectionDialog(dataBrowser, dbConnectionDialog, DataBrowserContext.getAppName());
		}
		if (DataBrowserContext.isStandAlone()) {
			dbConnectionDialog.setJdbcHelpURL("http://dbeauty.sourceforge.net/jdbc.html");
		}
		if (dbConnectionDialog.isConnected || dbConnectionDialog.connect(DataBrowserContext.getAppName(true))) {
			dataBrowser.setConnection(dbConnectionDialog);
			dataBrowser.askForDataModel();
			dataBrowser.desktop.openSchemaMappingDialog(true);
			dataBrowser.updateStatusBar();
			if (!silent) {
				dataBrowser.openNewTableBrowser(true);
			}
		} else {
			if (dbConnectionDialog.isConnected) {
				dataBrowser.setConnection(dbConnectionDialog);
			}
			for (int i = 0; i < dataBrowser.menuBar.getMenuCount(); ++i) {
				JMenu menu = dataBrowser.menuBar.getMenu(i);
				if (menu != dataBrowser.helpMenu) {
					for (int j = 0; j < menu.getItemCount(); ++j) {
						JMenuItem item = menu.getItem(j);
						if (item != null) {
							item.setEnabled(false);
						}
					}
				}
			}
		}
		return dataBrowser;
	}

	/**
	 * Opens the data model editor.
	 */
	private void openDataModelEditor() {
		try {
			String modelname = datamodel == null || datamodel.get() == null ? DataModel.DEFAULT_NAME : datamodel.get().getName();
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

			AnalyseOptionsDialog analyseOptionsDialog = new AnalyseOptionsDialog(this, datamodel == null ? null : datamodel.get());
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
				if (DataBrowserContext.isStandAlone()) {
					UIUtil.disableWarnings = true;
				}
				if (UIUtil.runJailer(this, args, false, true, false, true, null, dbConnectionDialog.getPassword(), null, null, false, true)) {
					ModelBuilder.assocFilter = null;
					String modelname = datamodel == null || datamodel.get() == null ? DataModel.DEFAULT_NAME : datamodel.get().getName();
					DataModelEditor dataModelEditor = new DataModelEditor(this, true, analyseOptionsDialog.isRemoving(), null,
							analyseOptionsDialog.getTableLineFilter(), analyseOptionsDialog.getAssociationLineFilter(), modelname,
							schema == null ? dbConnectionDialog.getName() : schema);
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
    private javax.swing.JMenuItem aboutMenuItem;
    private javax.swing.JMenuItem analyseMenuItem;
    private javax.swing.JLabel associatedWith;
    private javax.swing.JPanel borderBrowserPanel;
    private javax.swing.JPanel borderBrowserTitledPanel;
    private javax.swing.JMenuItem cloaseAllMenuItem;
    public javax.swing.JLabel connectivityState;
    private javax.swing.JMenuItem createExtractionModelMenuItem;
    private javax.swing.JMenuItem dataModelEditorjMenuItem;
    private javax.swing.JLabel dependsOn;
    private javax.swing.JLabel hasDependent;
    private javax.swing.JMenuItem helpForum;
    private javax.swing.JMenu helpMenu;
    private javax.swing.JPanel hiddenPanel;
    private javax.swing.JLabel ignored;
    private javax.swing.JInternalFrame jInternalFrame1;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JMenu jMenu1;
    private javax.swing.JMenu jMenu2;
    private javax.swing.JMenuItem jMenuItem1;
    private javax.swing.JMenuItem jMenuItem3;
    private javax.swing.JMenuItem jMenuItem4;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel11;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JPopupMenu.Separator jSeparator1;
    private javax.swing.JPopupMenu.Separator jSeparator2;
    private javax.swing.JPopupMenu.Separator jSeparator3;
    private javax.swing.JPopupMenu.Separator jSeparator4;
    private javax.swing.JPopupMenu.Separator jSeparator5;
    private javax.swing.JPopupMenu.Separator jSeparator6;
    private javax.swing.JPopupMenu.Separator jSeparator7;
    private javax.swing.JSplitPane jSplitPane1;
    private javax.swing.JSplitPane jSplitPane2;
    private javax.swing.JRadioButtonMenuItem largeLayoutRadioButtonMenuItem;
    private javax.swing.JMenuItem layoutMenuItem;
    private javax.swing.JPanel legende;
    private javax.swing.JPanel legende1;
    private javax.swing.JPanel legende2;
    private javax.swing.JRadioButtonMenuItem mediumLayoutRadioButtonMenuItem;
    private javax.swing.JMenuBar menuBar;
    private javax.swing.JMenu menuTools;
    private javax.swing.JMenu menuWindow;
    private javax.swing.JLabel modelName;
    private javax.swing.JLabel modelPath;
    private javax.swing.JTree navigationTree;
    private javax.swing.JScrollPane navigationTreeScrollPane;
    private javax.swing.JMenuItem newWindowMenuItem;
    private javax.swing.JMenuItem reconnectMenuItem;
    private javax.swing.JMenuItem restoreSessionItem;
    private javax.swing.JMenuItem schemaMappingMenuItem;
    private javax.swing.JLabel schemaName;
    private javax.swing.JPanel schemaNamePanel;
    private javax.swing.JRadioButtonMenuItem smallLayoutRadioButtonMenuItem;
    private javax.swing.JMenuItem storeSessionItem;
    private javax.swing.JRadioButtonMenuItem thumbnailLayoutRadioButtonMenuItem;
    private javax.swing.JRadioButtonMenuItem tinyLayoutRadioButtonMenuItem;
    private javax.swing.JLabel titleLabel;
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
		try {
			if (datamodel.get().getTables().isEmpty()) {
				switch (JOptionPane.showOptionDialog(this, "Data model \"" + DataModelManager.getModelDetails(DataModelManager.getCurrentModelSubfolder()).a + "\" is empty.", DataBrowserContext.getAppName(true), JOptionPane.YES_NO_OPTION,
						JOptionPane.INFORMATION_MESSAGE, null, new Object[] { "Analyze Database", "Data Model Editor" }, null)) {
				case 0:
					updateDataModel();
					break;
				case 1:
					openDataModelEditor();
					break;
				}
			} else if (!new File(DataModel.getColumnsFile()).exists()) {
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
			UIUtil.showException(this, "Error", e);
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

	private static class BrowserAssociationModel extends DefaultAssociationModel {
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
		if (disableBorderBrowserUpdates) {
			return;
		}
		Collection<AssociationModel> model = new ArrayList<AssociationModel>();
		if (desktop != null) {
			titleLabel.setText(" Related Rows");
			List<RowBrowser> allChildren = new ArrayList<RowBrowser>();
			for (RowBrowser rb: desktop.getBrowsers()) {
				if (rb.internalFrame == desktop.getSelectedFrame() && !rb.isHidden()) {
					allChildren.add(rb);
					allChildren.addAll(collectChildren(rb));
					titleLabel.setText(" Related Rows of Subtree " + rb.internalFrame.getTitle());
					break;
				}
			}
			for (RowBrowser rb: allChildren) {
				if (rb.browserContentPane.table != null) {
					Set<Association> associations = new HashSet<Association>(rb.browserContentPane.table.associations);
					for (RowBrowser c: desktop.getChildBrowsers(rb, false)) {
						if (c.browserContentPane.association != null) {
							associations.remove(c.browserContentPane.association);
						}
					}
					if (rb.browserContentPane.association != null && rb.parent != null) {
						if (allChildren.contains(rb.parent)) {
							associations.remove(rb.browserContentPane.association.reversalAssociation);
						}
					}
					for (Association association: associations) {
						model.add(new BrowserAssociationModel(rb, association));	
					}
				}
			}
		}
		
		borderBrowser.setModel(model);
	}

	protected void resolveSelection(Collection<AssociationModel> selection) {
		try {
			disableBorderBrowserUpdates = true;
			JInternalFrame currentSelection = desktop.getSelectedFrame();
			for (AssociationModel a: selection) {
				BrowserAssociationModel associationModel = (BrowserAssociationModel) a;
				desktop.addTableBrowser(associationModel.getRowBrowser(), -1, associationModel.getAssociation().destination, 
						associationModel.getAssociation(), "", null, null, true);
			}
			if (currentSelection != null) {
				try {
					currentSelection.setSelected(true);
				} catch (PropertyVetoException e) {
					// ignore
				}
			}
		} finally {
			disableBorderBrowserUpdates = false;
			updateBorderBrowser();
		}
	}

	private List<RowBrowser> collectChildren(RowBrowser rb) {
		List<RowBrowser> result = new ArrayList<Desktop.RowBrowser>();
		for (RowBrowser c: desktop.getChildBrowsers(rb, true)) {
			result.add(c);
		}
		for (RowBrowser c: desktop.getChildBrowsers(rb, true)) {
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
