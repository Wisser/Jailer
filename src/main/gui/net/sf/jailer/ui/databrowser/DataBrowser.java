/*
 * Copyright 2007 - 2024 Ralf Wisser.
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

import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.Toolkit;
import java.awt.Window;
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
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowFocusListener;
import java.awt.event.WindowListener;
import java.beans.PropertyVetoException;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.URI;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NavigableMap;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Vector;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.function.BiConsumer;
import java.util.stream.Collectors;

import javax.swing.AbstractButton;
import javax.swing.ButtonGroup;
import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JCheckBoxMenuItem;
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
import javax.swing.JToolBar;
import javax.swing.JTree;
import javax.swing.JViewport;
import javax.swing.KeyStroke;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;
import javax.swing.WindowConstants;
import javax.swing.border.LineBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.MouseInputAdapter;
import javax.swing.event.TreeExpansionEvent;
import javax.swing.event.TreeExpansionListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.database.BasicDataSource;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
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
import net.sf.jailer.ui.Colors;
import net.sf.jailer.ui.ColumnOrderEditor;
import net.sf.jailer.ui.DataModelEditor;
import net.sf.jailer.ui.DataModelManager;
import net.sf.jailer.ui.DataModelManagerDialog;
import net.sf.jailer.ui.DbConnectionDialog;
import net.sf.jailer.ui.DbConnectionDialog.ConnectionInfo;
import net.sf.jailer.ui.DbConnectionDialog.ConnectionType;
import net.sf.jailer.ui.DbConnectionDialog.ConnectionTypeChangeListener;
import net.sf.jailer.ui.DbConnectionDialog.DataModelChanger;
import net.sf.jailer.ui.Environment;
import net.sf.jailer.ui.ExtractionModelFrame;
import net.sf.jailer.ui.ImportDialog;
import net.sf.jailer.ui.JComboBox2;
import net.sf.jailer.ui.PrivilegedSessionProviderDialog;
import net.sf.jailer.ui.RowCountRenderer;
import net.sf.jailer.ui.SessionForUI;
import net.sf.jailer.ui.StringSearchPanel;
import net.sf.jailer.ui.StringSearchPanel.StringSearchDialog;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.UIUtil.PLAF;
import net.sf.jailer.ui.UIUtil.PlafAware;
import net.sf.jailer.ui.UIUtil.ResultConsumer;
import net.sf.jailer.ui.associationproposer.AssociationProposerView;
import net.sf.jailer.ui.commandline.CommandLineInstance;
import net.sf.jailer.ui.constraintcheck.ConstraintChecker;
import net.sf.jailer.ui.databrowser.BookmarksPanel.BookmarkId;
import net.sf.jailer.ui.databrowser.BrowserContentPane.RowsClosure;
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
import net.sf.jailer.ui.databrowser.whereconditioneditor.WhereConditionEditorPanel;
import net.sf.jailer.ui.ddl_script_generator.DDLScriptGeneratorPanel;
import net.sf.jailer.ui.scrollmenu.JScrollPopupMenu;
import net.sf.jailer.ui.syntaxtextarea.BasicFormatterImpl;
import net.sf.jailer.ui.syntaxtextarea.DataModelBasedSQLCompletionProvider;
import net.sf.jailer.ui.syntaxtextarea.RSyntaxTextAreaWithSQLSyntaxStyle;
import net.sf.jailer.ui.util.AnimationController;
import net.sf.jailer.ui.util.CompoundIcon;
import net.sf.jailer.ui.util.CompoundIcon.Axis;
import net.sf.jailer.ui.util.HSLColor;
import net.sf.jailer.ui.util.RotatedIcon;
import net.sf.jailer.ui.util.SmallButton;
import net.sf.jailer.ui.util.TextIcon;
import net.sf.jailer.ui.util.UISettings;
import net.sf.jailer.ui.util.UpdateInfoManager;
import net.sf.jailer.util.CancellationHandler;
import net.sf.jailer.util.LogUtil;
import net.sf.jailer.util.Pair;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlUtil;

/**
 * Data Browser Frame.
 *
 * @author Ralf Wisser
 */
public class DataBrowser extends javax.swing.JFrame implements ConnectionTypeChangeListener, PlafAware {

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

	/**
	 * Initial width of search panel.
	 */
	private static final int INITIAL_SEARCH_PANEL_WIDTH = 228;

	private final JComboBox2<String> tablesComboBox;

	private class WhereConditionEditorPanelForDataBrowser extends WhereConditionEditorPanel {
		private final RowBrowser rowBrowser;
		private final boolean asPopup;

		private WhereConditionEditorPanelForDataBrowser(Window parent, DataModel dataModel, Table table,
				BrowserContentCellEditor cellEditor, Boolean sorted, WhereConditionEditorPanel predecessor,
				RSyntaxTextAreaWithSQLSyntaxStyle editor, JComponent closeButton, boolean asPopup, int initialColumn,
				Session session, ExecutionContext executionContext, RowBrowser rowBrowser) {
			super(parent, dataModel, table, cellEditor, sorted, predecessor, editor, closeButton, asPopup,
					initialColumn, true, session, new DataModelBasedSQLCompletionProvider(session, datamodel.get()),
					executionContext);
			this.rowBrowser = rowBrowser;
			this.asPopup = asPopup;
		}

		@Override
		protected void consume(String condition, Set<Integer> involvedColumns) {
			if (rowBrowser != null && rowBrowser.browserContentPane != null) {
				rowBrowser.browserContentPane.filteredColumns = involvedColumns;
				String andConditionText = rowBrowser.browserContentPane.getAndConditionText();
				andConditionText = new BasicFormatterImpl().format(andConditionText.trim());
				condition = new BasicFormatterImpl().format(condition.trim());
				if (!andConditionText.equals(condition)) {
					boolean oldSuppessReloadOnAndConditionAction = rowBrowser.browserContentPane.suppessReloadOnAndConditionAction;
					try {
						rowBrowser.browserContentPane.suppessReloadOnAndConditionAction = true;
						rowBrowser.browserContentPane.setAndCondition(UIUtil.toSingleLineSQL(condition), true);
						rowBrowser.browserContentPane.reloadRows();
						if (asPopup) {
							rowBrowser.browserContentPane
									.onConditionChange(rowBrowser.browserContentPane.getAndConditionText());
						}
						UISettings.s12 += 1000;
					} finally {
						rowBrowser.browserContentPane.suppessReloadOnAndConditionAction = oldSuppessReloadOnAndConditionAction;
					}
				}
			}
		}

		@Override
		protected void onEscape() {
			// do nothing
		}

		@Override
		protected boolean isColumnNullable(Table table, Column column) {
			MetaDataSource metaDataSource = session != null ? getMetaDataSource(session) : null;
			return isColumnNullable(table, column, metaDataSource);
		}
	};

	private static class SearchBarRSyntaxTextArea extends RSyntaxTextAreaWithSQLSyntaxStyle {
		WhereConditionEditorPanel whereConditionEditorPanel;

		public SearchBarRSyntaxTextArea() {
			super(false, false);
		}

		@Override
		protected void runBlock() {
			super.runBlock();
			if (whereConditionEditorPanel != null) {
				whereConditionEditorPanel.parseCondition();
			}
		}

		@Override
		protected boolean withFindAndReplace() {
			return false;
		}
	};

	private boolean initialized = false;

	private final DesktopUndoManager desktopUndoManager;

	/**
	 * Allowed row limits.
	 */
	public static final Integer[] ROW_LIMITS = new Integer[] { 500, 1000, 2000, 5000, 10000, 20000, 50000, 70000,
			100000 };
	public static final int ROW_LIMIT_DEFAULT = 500;

	/**
	 * Constructor.
	 *
	 * @param datamodel          the {@link DataModel}
	 * @param root               table to start browsing with
	 * @param condition          initial condition
	 * @param dbConnectionDialog DB-connection dialog
	 */
	public DataBrowser(final DataModel datamodel, final Table root, String condition,
			DbConnectionDialog dbConnectionDialog, Map<String, String> schemaMapping,
			final ExecutionContext executionContext) throws Exception {
		this.executionContext = executionContext;
		this.datamodel = new Reference<DataModel>(datamodel);
		this.dbConnectionDialog = dbConnectionDialog != null
				? new DbConnectionDialog(this, dbConnectionDialog, DataBrowserContext.getAppName(),
						createDataModelChanger(), executionContext)
				: null;
		this.borderBrowser = new AssociationListUI("Resolve", "Resolve selected Associations", true) {
			private static final long serialVersionUID = 183805595423236039L;

			@Override
			protected void applyAction(Collection<AssociationModel> selection) {
				resolveSelection(selection);
			}
		};
		executionContext.setUseRowIdsOnlyForTablesWithoutPK(true);
		initComponents();
		UIUtil.initComponents(this);
		jToolBar1.setFloatable(false);
		jToolBar2.setFloatable(false);

		initMenu();
		initNavTree();
		initTabSelectionAnimationManager();
		searchPanelSplitSizerPanel.setCursor(Cursor.getPredefinedCursor(Cursor.W_RESIZE_CURSOR));
		searchPanelSplitSizerPanel.add(createSearchPanelSplitSizer());
		searchPanelSplitSizerPanel.setVisible(searchPanelContainer.isVisible());

		modelNavigationSplitSizerPanel.setCursor(Cursor.getPredefinedCursor(Cursor.W_RESIZE_CURSOR));
		modelNavigationSplitSizerPanel.setVisible(false);
		modelNavigationSplitSizerPanel.add(createModelNavSplitSizer());
		modelNavigationConnectButtonV.setText(null);
		modelNavigationConnectButtonV.setIcon(new CompoundIcon(Axis.Y_AXIS, 4, onePxIcon, noconnectionIcon, onePxIcon));
		modelNavigationConnectButtonV.addActionListener(this::reconnectMenuItemActionPerformed);
		modelNavigationButtonV.setText(null);
		modelNavigationButtonV.setVerticalAlignment(SwingConstants.TOP);
		CompoundIcon ti = new CompoundIcon(Axis.Y_AXIS, 2, new TextIcon(modelNavigationButtonV, " Connections "),
				onePxIcon);
		Icon ii = new CompoundIcon(Axis.Y_AXIS, 4, onePxIcon, menuIcon, onePxIcon);
		Icon ri1 = new RotatedIcon(new CompoundIcon(Axis.X_AXIS, 2, ti, new RotatedIcon(ii, RotatedIcon.Rotate.DOWN)),
				RotatedIcon.Rotate.UP);
		modelNavigationButtonV.setIcon(ri1);
		ActionListener al = e -> {
			modelNavigationScrollPane.setVisible(!modelNavigationScrollPane.isVisible());
			modelNavigationButtonV.setVisible(!modelNavigationScrollPane.isVisible());
			modelNavigationConnectButtonV.setVisible(!modelNavigationScrollPane.isVisible());
			modelNavigationSplitSizerPanel.setVisible(modelNavigationScrollPane.isVisible());
			modelNavigationGapPanel.setVisible(modelNavigationScrollPane.isVisible());
			modelNavigationGapPanel.setVisible(modelNavigationScrollPane.isVisible());

			if (modelNavigationScrollPane.isVisible()) {
				modelNavigationButtonV.setVisible(true);
				modelNavigationConnectButtonV.setVisible(true);

				modelNavigationGapPanel.setMinimumSize(
						new Dimension(Math.max(200, modelNavigationGapPanel.getPreferredSize().width), 1));
				modelNavigationGapPanel.setPreferredSize(modelNavigationGapPanel.getMinimumSize());
			}
		};
		modelNavigationButtonV.addActionListener(al);
		modelNavigationPanel.setVisible(false);
		modelNavigationGapPanel.setMinimumSize(new Dimension(300, 1));
		modelNavigationGapPanel.setPreferredSize(modelNavigationGapPanel.getMinimumSize());

		if (UIUtil.plaf.isFlat) {
			modelNavigationButtonV.addMouseListener(new MouseAdapter() {
				@Override
				public void mouseExited(MouseEvent e) {
					if (!modelNavigationButtonV.isSelected()) {
						modelNavigationButtonV.setBackground(new JToggleButton().getBackground());
					}
				}

				@Override
				public void mouseEntered(MouseEvent e) {
					if (!modelNavigationButtonV.isSelected()) {
						modelNavigationButtonV.setBackground(Colors.Color_224_224_224);
					}
				}
			});
		}
		if (UIUtil.plaf == PLAF.NATIVE) {
			modelNavigationButtonV.setMinimumSize(new Dimension(24, modelNavigationButtonV.getPreferredSize().height));
			modelNavigationButtonV.setPreferredSize(modelNavigationButtonV.getMinimumSize());
			modelNavigationConnectButtonV
					.setMinimumSize(new Dimension(24, modelNavigationConnectButtonV.getPreferredSize().height));
			modelNavigationConnectButtonV.setPreferredSize(modelNavigationConnectButtonV.getMinimumSize());
		}

		initModelNavigation();
		updateModelNavigation();

		UIUtil.invokeLater(14, () -> {
			al.actionPerformed(null);
			modelNavigationPanel.setVisible(true);
		});

		boolean zoom = Boolean.TRUE.equals(UISettings.restore(UISettings.ZOOM_WITH_MOUSE_WHEEL));
		zoomWithMouseWheelMenuItem
				.setSelected(Boolean.TRUE.equals(UISettings.restore(UISettings.ZOOM_WITH_MOUSE_WHEEL)));
		setZoomWithMouseWheel(zoom);

		if (UIUtil.plaf.isFlat) {
			jSplitPane4.setDividerSize(16);
		}

		tbBackButton.setText(null);
		tbForewardButton.setText(null);
		tbClearButton.setText(null);
		tbZoom0Button.setText(null);
		tbZoom1Button.setText(null);
		tbZoom2Button.setText(null);
		tbZoom3Button.setText(null);
		tbZoom4Button.setText(null);
		tbZoomInButton.setText(null);
		tbZoomOutButton.setText(null);

		tbBackButton.setIcon(tbBackIcon);
		tbForewardButton.setIcon(tbForwardIcon);
		tbZoom0Button.setIcon(tbZoom0Icon);
		tbZoom1Button.setIcon(tbZoom1Icon);
		tbZoom2Button.setIcon(tbZoom2Icon);
		tbZoom3Button.setIcon(tbZoom3Icon);
		tbZoom4Button.setIcon(tbZoom4Icon);
		tbZoomInButton.setIcon(tbZoomInIcon);
		tbZoomOutButton.setIcon(tbZoomOutIcon);
		tbreloadButton.setIcon(runIcon);
		tbreloadButton.setHorizontalTextPosition(SwingConstants.RIGHT);
		tbClearButton.setIcon(clearIcon);
		tbClearButton.setHorizontalTextPosition(SwingConstants.RIGHT);

		desktopUndoManager = new DesktopUndoManager(tbBackButton, tbForewardButton, goBackItem, goForwardItem, this,
				jScrollPane1);

		linkToolbarButton(tbZoomInButton, zoomInMenuItem);
		linkToolbarButton(tbZoomOutButton, zoomOutMenuItem);
		linkToolbarButton(tbZoom0Button, thumbnailLayoutRadioButtonMenuItem);
		linkToolbarButton(tbZoom1Button, tinyLayoutRadioButtonMenuItem);
		linkToolbarButton(tbZoom2Button, smallLayoutRadioButtonMenuItem);
		linkToolbarButton(tbZoom3Button, mediumLayoutRadioButtonMenuItem);
		linkToolbarButton(tbZoom4Button, largeLayoutRadioButtonMenuItem);

		setSearchPanelWidth(INITIAL_SEARCH_PANEL_WIDTH);

		searchBarToggleButton.setSelected(!Boolean.FALSE.equals(UISettings.restore("searchBarToggleButton")));
		searchPanelContainer.setVisible(searchBarToggleButton.isSelected());
		searchPanelSplitSizerPanel.setVisible(searchPanelContainer.isVisible());
		whereConditionEditorCloseButton = new SmallButton(UIUtil.plaf == PLAF.FLATDARK? closeDarkIcon : closeIcon, closeOverIcon, false) {
			@Override
			protected void onClick(MouseEvent e) {
				searchBarToggleButton.doClick();
			}
		};

		SearchBarRSyntaxTextArea searchBarEditor = new SearchBarRSyntaxTextArea();
		SearchBarRSyntaxTextArea popUpSearchBarEditor = new SearchBarRSyntaxTextArea();

		searchBarEditor.setEnabled(true);
		searchBarEditor.setMarkOccurrences(false);
		popUpSearchBarEditor.setEnabled(true);
		popUpSearchBarEditor.setMarkOccurrences(false);
		searchBarEditor.grabFocus();
		popUpSearchBarEditor.grabFocus();

		whereConditionEditorPanel = new WhereConditionEditorPanel(this, datamodel, null, null, null,
				whereConditionEditorPanel, searchBarEditor, whereConditionEditorCloseButton, false, -1, true, session,
				null, executionContext) {
			@Override
			protected void consume(String condition, Set<Integer> involvedColumns) {
				// nothing to do
			}

			@Override
			protected void onEscape() {
				// nothing to do
			}

			@Override
			protected boolean isColumnNullable(Table table, Column column) {
				MetaDataSource metaDataSource = session != null ? getMetaDataSource(session) : null;
				return isColumnNullable(table, column, metaDataSource);
			}
		};
		searchBarEditor.whereConditionEditorPanel = whereConditionEditorPanel;
		whereConditionEditorSubject = null;
		searchPanelContainer.setVisible(false);
		searchPanelSplitSizerPanel.setVisible(searchPanelContainer.isVisible());

		UpdateInfoManager.checkUpdateAvailability(updateInfoPanel, updateInfoLabel, downloadMenuItem, "B");
		UIUtil.initPLAFMenuItem(plafMenu, this);
		if (datamodel != null) {
			UISettings.dmStats(datamodel);
		}
		initRowLimitButtons();
		workbenchTabbedPane.setTabComponentAt(0, new JLabel("Desktop", desktopIcon, JLabel.LEFT));
		workbenchTabbedPane.setTabComponentAt(1, new JLabel("SQL Console ", sqlConsoleIcon, JLabel.LEFT));
		JLabel newConsoleLabel = new JLabel(addSqlConsoleIcon);
		int tabIndex = workbenchTabbedPane.getTabCount() - 1;
		workbenchTabbedPane.setTabComponentAt(tabIndex, newConsoleLabel);
		String toolTipText = "New SQL Console";
		workbenchTabbedPane.setToolTipTextAt(tabIndex, toolTipText);
//		newConsoleLabel.setToolTipText(toolTipText);

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

		tbAddTableButton = createSearchButton();
		tbAddTableButton.setText(null);
		tbAddTableButton.setIcon(addTableIcon);
		tbAddTableButton.setToolTipText("Open Table");
		jToolBar1.add(tbAddTableButton, 2);

		searchButton = createSearchButton();
		jToolBar2.add(searchButton);

		tablesComboBox.setVisible(false);
		openTableButton.setVisible(false);
		searchButton.setText("Open Table");
		searchButton.setIcon(addTableIcon);
		searchButton.setToolTipText("<html>Open Table.<br><hr><i>Cntrl-T</i></html>");

		metaDataDetailsPanel = createMetaDataDetailsPanel(executionContext);
		metaDataViewPanel.add(metaDataDetailsPanel);

		jLayeredPane1.removeAll();
		jLayeredPane1.setLayout(null);
		jLayeredPane1.setLayer(layeredPaneContent, JLayeredPane.PALETTE_LAYER);
		jLayeredPane1.setLayer(dummy, JLayeredPane.DEFAULT_LAYER);
		searchBarToggleButton.setText(null);
		searchBarToggleButton.setIcon(searchIcon);
		gridBagConstraints = new GridBagConstraints();
		gridBagConstraints.gridx = 1;
		gridBagConstraints.gridy = 1;
		gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
		gridBagConstraints.weightx = 1;
		gridBagConstraints.weighty = 1;
		jLayeredPane1.add(layeredPaneContent /* , gridBagConstraints */);
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

		anchorManager = new DesktopAnchorManager(topLayerPanel) {
			@Override
			protected void layout(RowBrowser anchor) {
				if (desktopUndoManager != null) {
					String description = "Align Horizontally";
					desktopUndoManager.beforeModification(description, description);
				}
				try {
					anchor.internalFrame.setSelected(true);
				} catch (PropertyVetoException e) {
					// ignore
				}
				anchor.browserContentPane.rowsClosure.hAlignedPath.clear();
				anchor.browserContentPane.rowsClosure.hAlignedPathOnSelection = false;
				for (RowBrowser rb = anchor; rb != null; rb = rb.parent) {
					if (!rb.isHidden()) {
						anchor.browserContentPane.rowsClosure.hAlignedPath.add(rb.browserContentPane);
					}
				}
				arrangeLayout(true, anchor, true);
			}

			@Override
			protected boolean isAvailable() {
				if (desktop.desktopAnimation != null && desktop.desktopAnimation.isActive()) {
					return false;
				} else {
					return true;
				}
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
					if (ancestor.internalFrame.isVisible()
							&& Math.abs(tableBrowser.internalFrame.getY() - ancestor.internalFrame.getY()) > 2) {
						return true;
					}
					ancestor = ancestor.parent;
				}
				RowsClosure closure = tableBrowser.browserContentPane.rowsClosure;
				for (RowBrowser tb = tableBrowser; tb != null; tb = tb.parent) {
					if (!tb.isHidden()) {
						RowBrowser parent = tb.parent;
						while (parent != null && parent.parent != null && parent.isHidden()) {
							parent = parent.parent;
						}
						if (parent != null && !parent.isHidden()) {
							if (closure.hAlignedPath.isEmpty()) {
								if (desktop.getChildBrowsers(parent, true).size() > 1) {
									return true;
								}
							} else {
								RowBrowser ftb = tb;
								RowBrowser fParent = parent;
								if (desktop.getChildBrowsers(parent, true).stream().filter(c -> {
									return c != ftb && !(!closure.hAlignedPath.contains(c.browserContentPane)
											&& closure.hAlignedPath.contains(fParent.browserContentPane));
								}).findAny().isPresent()) {
									return true;
								}
							}
						}
					}
				}
				return false;
			}

			@Override
			protected void addAdditionalActions(RowBrowser tableBrowser) {
				JPopupMenu menu = tableBrowser.browserContentPane.createSqlPopupMenu(0, 0, 0,
						desktop.layoutMode == LayoutMode.THUMBNAIL, tableBrowser.browserContentPane);
				addAdditionalAction(null, null, null, false, false, false);
				boolean needsSep = false;
				int compsASep = 1;
				for (Component comp : menu.getComponents()) {
					if (comp instanceof JMenuItem) {
						JMenuItem item = (JMenuItem) comp;
						if (item.getName() != null && item.getName().startsWith("icon:")) {
							String tooltip = item.getToolTipText() != null ? item.getToolTipText() : item.getText();
							if (item.getAccelerator() != null) {
								String ttHtml = item.getAccelerator().toString().replaceAll("\\s*pressed\\s*", "+");
								StringBuilder sb = new StringBuilder();
								for (int i = 0; i < ttHtml.length(); ++i) {
									if (i > 0 && Character.isLetter(ttHtml.charAt(i - 1))) {
										sb.append(ttHtml.substring(i, i + 1).toLowerCase());
									} else {
										sb.append(ttHtml.substring(i, i + 1).toUpperCase());
									}
								}
								ttHtml = sb.toString();
								tooltip = "<html>" + UIUtil.toHTMLFragment(tooltip, 0) + "&nbsp;&nbsp;&nbsp;&nbsp;<i>"
										+ UIUtil.toHTMLFragment(ttHtml, 0) + "</i></html>";
							}
							addAdditionalAction(UIUtil.readImage(item.getName().substring(5)), tooltip,
									() -> item.doClick(), item instanceof JCheckBoxMenuItem,
									(item instanceof JCheckBoxMenuItem) ? ((JCheckBoxMenuItem) item).isSelected()
											: false,
									item.isEnabled());
							needsSep = true;
							++compsASep;
						}
					} else if (comp instanceof JSeparator) {
						if (needsSep && compsASep > 1) {
							addAdditionalAction(null, null, null, false, false, false);
							needsSep = false;
							compsASep = 0;
						}
					}
				}
			}
		};

		if (jScrollPane1.getVerticalScrollBar() != null) {
			jScrollPane1.getVerticalScrollBar().setUnitIncrement(48);
			jScrollPane1.getVerticalScrollBar().addAdjustmentListener(new AdjustmentListener() {
				@Override
				public void adjustmentValueChanged(AdjustmentEvent e) {
					anchorManager.reset();
				}
			});
		}
		if (jScrollPane1.getHorizontalScrollBar() != null) {
			jScrollPane1.getHorizontalScrollBar().setUnitIncrement(48);
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
					if (value instanceof DefaultMutableTreeNode
							&& ((DefaultMutableTreeNode) value).getUserObject() instanceof TreeNodeForRowBrowser) {
						TreeNodeForRowBrowser node = (TreeNodeForRowBrowser) ((DefaultMutableTreeNode) value)
								.getUserObject();
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
					switch (UIUtil.plaf) {
					case NATIVE:
						// nothing to do
						break;
					case FLAT:
						// nothing to do
						break;
					case FLATDARK:
						// nothing to do
						break;
					case NIMBUS:
						if (((JLabel) render).getText() != null) {
							((JLabel) render).setText(UIUtil.toHTML(((JLabel) render).getText(), 100));
						}
						break;
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

		// L&F can no longer be changed
		jSeparator6.setVisible(false);
		view.setVisible(false);

		try {
			for (final PLAF lfInfo : UIUtil.availablePlafs) {
				JMenuItem mItem = new JMenuItem();
				mItem.setText(lfInfo.description);
				view.add(mItem);
				mItem.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent arg0) {
						setPLAF(lfInfo);
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
		UIUtil.invokeLater(64, () -> {
			initialHGPanel.setVisible(false);
		});

		desktop = new Desktop(this.datamodel, jailerIcon, session, this, dbConnectionDialog,
				schemaMapping == null ? new HashMap<String, String>() : schemaMapping, anchorManager,
				executionContext) {

			@Override
			public void openSchemaAnalyzer() {
				updateDataModel();
			}

			boolean animationStepTimeWasNonZero = false;

			@Override
			protected void updateMenu(boolean hasTableBrowser, boolean hasIFrame) {
				storeSessionItem.setEnabled(hasIFrame);
				closeAllMenuItem.setEnabled(hasIFrame);
				tbreloadButton.setEnabled(hasIFrame);
				tbClearButton.setEnabled(hasIFrame);
				addBookmarkMenuItem.setEnabled(hasTableBrowser);
				exportDataMenuItem.setEnabled(hasTableBrowser);
				createExtractionModelMenuItem.setEnabled(hasTableBrowser);
				if (animationStepTime.get() != 0) {
					animationStepTimeWasNonZero = true;
				}
				animationStepTimeMenu.setEnabled(
						animationStepTimeWasNonZero || getBrowsers().stream().anyMatch(b -> b.parent != null));
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
					return DataBrowser.openNewDataBrowser(DataBrowser.this.datamodel.get(), dbConnectionDialog, false,
							executionContext, null);
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
				for (Entry<JRadioButtonMenuItem, Integer> e : rowLimitButtonToLimit.entrySet()) {
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
					boolean passTable = metaDataSource.isInitialized() && metaDataSource.getDefaultSchema() != null
							&& metaDataSource.getDefaultSchema().isLoaded();
					if (passTable) {
						MDSchema schema = metaDataSource.getSchemaOfTable(table);
						if (schema != null && schema != metaDataSource.getDefaultSchema()) {
							if (!schema.isLoaded()) {
								passTable = false;
								schema.loadTables(true, null, null, null);
							}
						}
					}
					metaDataDetailsPanel.showMetaDataDetails(passTable ? metaDataSource.toMDTable(table) : null, table,
							row, true, datamodel);
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

			@Override
			protected void onConditionChange(BrowserContentPane browserContentPane, String cond) {
				if (whereConditionEditorPanel != null && whereConditionEditorSubject == browserContentPane) {
					whereConditionEditorPanel.parseCondition(cond);
				}
			}

			@Override
			protected void onContentCellEditorCreated(BrowserContentPane browserContentPane,
					BrowserContentCellEditor cellEditor) {
				UIUtil.invokeLater(4, () -> {
					if (whereConditionEditorPanel != null && whereConditionEditorSubject == browserContentPane) {
						whereConditionEditorPanel.setCellEditor(cellEditor);
					}
				});
			}

			@Override
			protected void openConditionEditor(BrowserContentPane browserContentPane, Point location, int column,
					Runnable onClose) {
				DataBrowser.this.openConditionEditor(browserContentPane, location, column, onClose);
			}

			@Override
			protected void loadScriptFile(String fileName) {
				loadSQLScriptFile(new File(fileName), false, null);
			}

			@Override
			protected boolean isZoomWithMouseWheel() {
				return DataBrowser.this.isZoomWithMouseWheel();
			}

			@Override
			protected void forceRepaint() {
				DataBrowser.this.forceRepaint();
			}

			@Override
			protected void findPathTo(Table table) {
				closureView.findPath(DataBrowser.this.datamodel.get().getDisplayName(table));
			}

			@Override
			protected WhereConditionEditorPanel getWhereConditionEditorPanel(RowBrowser rowBrowser) {
				Table table = rowBrowser != null && rowBrowser.browserContentPane != null
						? rowBrowser.browserContentPane.table
						: null;
				if (table != null) {
					SearchBarRSyntaxTextArea searchBarEditor = new SearchBarRSyntaxTextArea();

					BrowserContentCellEditor cellEditor = rowBrowser.browserContentPane.browserContentCellEditor;
					WhereConditionEditorPanelForDataBrowser wced = new WhereConditionEditorPanelForDataBrowser(
							DataBrowser.this, datamodel, table, cellEditor,
							rowBrowser.browserContentPane.sortColumnsCheckBox.isSelected(), whereConditionEditorPanel,
							searchBarEditor, whereConditionEditorCloseButton, false, -1, session, executionContext,
							rowBrowser);
					wced.parseCondition(rowBrowser.browserContentPane.getAndConditionText());
					searchBarEditor.whereConditionEditorPanel = wced;
					return wced;
				}
				return null;
			}
		};

		desktop.setUndoManager(desktopUndoManager);

		desktop.addMouseMotionListener(new MouseMotionListener() {
			@Override
			public void mouseMoved(MouseEvent e) {
				double minDist = 40;
				minDist *= minDist;
				RowBrowser nearest = null;
				for (RowBrowser br : desktop.getBrowsers()) {
					if (br.internalFrame != null && br.internalFrame.isVisible()) {
						double dx = e.getX() - (br.internalFrame.getX() - anchorManager.getButtonWidth());
						if (dx >= 0 && dx < anchorManager.getButtonWidth()) {
							dx = 0;
						} else if (dx > anchorManager.getButtonWidth()) {
							dx -= anchorManager.getButtonWidth();
						}
						double dy = e.getY() - br.internalFrame.getY();
						if (dy >= 0 && dy < anchorManager.getHeight()) {
							dy = 0;
						} else if (dy > anchorManager.getHeight()) {
							dy -= anchorManager.getHeight();
						}
						double dist2 = dx * dx + dy * dy;
						if (dist2 < minDist) {
							nearest = br;
							minDist = dist2;
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

		desktopOutline = new DesktopOutline(navigationPanel, controlPanel, jScrollPane1, desktop, tableBrowser -> {
			if (anchorManager.isApplicable(tableBrowser)) {
				anchorManager.layout(tableBrowser);
				UISettings.s14 += 1000;
			}
		});
		java.awt.GridBagConstraints constraints = new java.awt.GridBagConstraints();
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.weightx = 1;
		constraints.weighty = 1;
		constraints.fill = GridBagConstraints.BOTH;
		outLinePanel.add(desktopOutline, constraints);

		new BookmarksPanel(this, bookmarkMenu, desktop, executionContext).updateBookmarksMenu();
		initAnimationSteptime();

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
				if (!sqlConsoles.isEmpty()) {
					sqlConsoles.get(0).saveContent();
				}
				if (closeAllSQLConsoles()) {
					DataBrowser.this.dispose();
				}
			}

			@Override
			public void windowClosed(WindowEvent e) {
				if (wasConnected) {
					storeLastSession();
				}
				desktop.stop();
				DataBrowser.this.removeAll();
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
		for (Frame frame : Frame.getFrames()) {
			if (frame instanceof DataBrowser && frame.isVisible()) {
				c = (c + 1) % 6;
			}
		}

		setLocation(40 + c * 32, 32 + c * 32);
		setSize(980, 800);

		UIUtil.fit(this);

		if (root != null) {
			desktopUndoManager.setEnabled(false);
			RowBrowser rb;
			try {
				rb = desktop.addTableBrowser(null, null, root, null, condition, null, true);
			} finally {
				desktopUndoManager.setEnabled(true);
			}
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
										metaDataPanel.select(
												((TreeNodeForRowBrowser) userObject).rowBrowser.browserContentPane.table);
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

	private JToggleButton createSearchButton() {
		return StringSearchPanel.createSearchButton(this, -1, tablesComboBox, "Open Table Browser", new Runnable() {
			@Override
			public void run() {
				openTableButtonActionPerformed(null);
			}
		}, null, null, null, false, null, true, false, null, false, () -> {
			Map<String, Integer> stringCount = new HashMap<String, Integer>();
			MetaDataSource metaDataSource = session != null ? getMetaDataSource(session) : null;
			if (metaDataSource != null && this.datamodel.get() != null) {
				for (int i = 0; i < tablesComboBox.getModel().getSize(); ++i) {
					Object e = tablesComboBox.getModel().getElementAt(i);
					if (e instanceof String) {
						Table table = this.datamodel.get().getTableByDisplayName((String) e);
						if (table != null && metaDataSource.isInitialized()) {
							MDTable mdTable = metaDataSource.toMDTable(table);
							if (mdTable != null) {
								Long count = mdTable.getEstimatedRowCount();
								if (count != null) {
									if (count == 1 && !mdTable.isEstRCIsLowerBound()) {
										stringCount.put((String) e, Integer.MAX_VALUE / 2);
									} else {
										stringCount.put((String) e, count.intValue()
												+ (mdTable.isEstRCIsLowerBound() ? Integer.MAX_VALUE / 2 : 0));
									}
								}
							}
						}
					}
				}
			}
			return stringCount;
		});
	}

	private void setSearchPanelWidth(int width) {
		searchPanelContainer.setMinimumSize(new Dimension(width, searchPanelContainer.getMinimumSize().height));
		searchPanelContainer.setPreferredSize(new Dimension(width, searchPanelContainer.getPreferredSize().height));
		searchPanelContainer.setMaximumSize(new Dimension(width, searchPanelContainer.getMaximumSize().height));
	}

	private static final int SPLIT_SIZER_MIN_WIDTH = 20;

	private Component createModelNavSplitSizer() {
		return createSplitSizer((xDelta, yDelta) -> {
			modelNavigationGapPanel.setMinimumSize(
					new Dimension((int) (modelNavigationGapPanel.getMinimumSize().getWidth() + xDelta), 1));
			modelNavigationGapPanel.setPreferredSize(modelNavigationGapPanel.getMinimumSize());
			modelNavigationGapPanel.revalidate();
			modelNavigationScrollPane
					.setVisible(modelNavigationGapPanel.getMinimumSize().getWidth() >= SPLIT_SIZER_MIN_WIDTH);
		}, () -> {
			modelNavigationScrollPane.setVisible(true);
			if (modelNavigationGapPanel.getMinimumSize().getWidth() < SPLIT_SIZER_MIN_WIDTH) {
				modelNavigationButtonV.doClick();
			}
		}, 22, 0);
	}

	private Component createSearchPanelSplitSizer() {
		return createSplitSizer((xDelta, yDelta) -> {
			setSearchPanelWidth(searchPanelContainer.getMinimumSize().width + xDelta);
			searchPanelContainer.revalidate();
		}, () -> {
			if (searchPanelContainer.getMinimumSize().width < SPLIT_SIZER_MIN_WIDTH) {
				searchBarToggleButton.doClick();
				setSearchPanelWidth(INITIAL_SEARCH_PANEL_WIDTH);
			}
		}, 0, 0);
	}

	private Component createSplitSizer(BiConsumer<Integer, Integer> deltaConsumer, Runnable onRelease, int vOffset,
			int hOffset) {
		return new JPanel(null) {
			{
				MouseInputAdapter ml = new MouseInputAdapter() {
					private Point origPos;

					@Override
					public void mouseDragged(MouseEvent e) {
						if (origPos == null) {
							return;
						}
						Point newPos = e.getPoint();
						SwingUtilities.convertPointToScreen(newPos, DataBrowser.this);
						int xDelta = newPos.x - origPos.x;
						int yDelta = newPos.y - origPos.y;
						deltaConsumer.accept(xDelta, yDelta);
					}

					@Override
					public void mousePressed(MouseEvent e) {
						origPos = e.getPoint();
						SwingUtilities.convertPointToScreen(origPos, DataBrowser.this);
					}

					@Override
					public void mouseReleased(MouseEvent e) {
						origPos = null;
						onRelease.run();
					}
				};
				addMouseListener(ml);
				addMouseMotionListener(ml);
			}

			@Override
			protected void paintComponent(Graphics g) {
				super.paintComponent(g);
				Dimension dim = getSize();
				Graphics2D g2d = (Graphics2D) g;
				Shape clip = g2d.getClip();
				g2d.clipRect(0, 0, dim.width, dim.height);

				Image image = splitIcon.getImage();
				g2d.drawImage(image, (dim.width - image.getWidth(null)) / 2 - hOffset,
						((dim.height - vOffset) - image.getHeight(null)) / 2, null);

				g2d.setClip(clip);
			}
		};
	}

	private Set<String> connectedAliases = new HashSet<String>();
	private ConnectionInfo currentConnectionInfo = null;

	private void initModelNavigation() {
		DefaultTreeCellRenderer renderer = new DefaultTreeCellRenderer() {
			boolean gotBG = false;
			Color bg;
			boolean opaque;
			boolean sel;
			float lDiff;
			boolean paintBG;

			@Override
			public Component getTreeCellRendererComponent(JTree tree, Object value, boolean sel, boolean expanded,
					boolean leaf, int row, boolean hasFocus) {
				Component render = super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);
				this.sel = sel;
				this.paintBG = false;
				this.lDiff = 20;
				if (render instanceof JLabel) {
					if (!gotBG) {
						gotBG = true;
						bg = ((JLabel) render).getBackground();
						opaque = ((JLabel) render).isOpaque();
					}
					((JLabel) render).setBackground(bg);
					((JLabel) render).setOpaque(opaque);
					Icon icon = null;
					if (value instanceof DefaultMutableTreeNode
							&& ((DefaultMutableTreeNode) value).getUserObject() instanceof ConnectionInfo) {
						ConnectionInfo node = (ConnectionInfo) ((DefaultMutableTreeNode) value).getUserObject();
						((JLabel) render).setText(ciRender(node));

						String dburl = node.url;
						String dbmsLogoURL = UIUtil.getDBMSLogoURL(dburl);
						if (dbmsLogoURL != null) {
							icon = UIUtil.scaleIcon(connectivityState, UIUtil.readImage(dbmsLogoURL, false), 1.2);
							icon = new CompoundIcon(icon) {
								@Override
								public int getIconWidth() {
									return 32;
								}
							};
						}
						Color bgCol = node.getConnectionType().getBackground();

						if (bgCol != null && !sel) {
							((JLabel) render).setBackground(bgCol);
							((JLabel) render).setOpaque(true);
							paintBG = getComponentOrientation().isLeftToRight() && UIUtil.plaf.isFlat;
							switch (node.getConnectionType()) {
							case Staging:
								lDiff = UIUtil.plaf == PLAF.FLATDARK? 10 : 30;
								break;
							case Development:
								lDiff = 25;
								break;
							case Production:
								lDiff = 10;
								break;
							case Test:
								lDiff = 10;
								break;
							default:
								break;
							}
						}

						if (connectedAliases.contains(node.alias)) {
							((JLabel) render)
									.setText("<html><nobr><b>" + UIUtil.toHTMLFragment(((JLabel) render).getText(), 0)
											+ "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</b></html>");
						} else {
							((JLabel) render)
									.setText("<html><nobr>" + UIUtil.toHTMLFragment(((JLabel) render).getText(), 0)
											+ "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</html>");
						}
					} else {
						icon = modelIcon;
					}
					((JLabel) render).setIcon(icon);
				}
				if (paintBG) {
					setOpaque(false);
				}
				return render;
			}

			public void paint(Graphics g) {
				int imageOffset = 0;
				Icon currentI = getIcon();
				if (currentI != null && getText() != null) {
					imageOffset = currentI.getIconWidth() + Math.max(0, getIconTextGap() - 1);
				}
				if (paintBG && !sel && getBackground() != null && g instanceof Graphics2D) {
					int h2 = UIUtil.plaf == PLAF.FLATDARK? 0 : (int) (getHeight() / 1.8);
					g.setColor(getBackground());
					g.fillRect(imageOffset, 0, getWidth() - imageOffset, h2);
					HSLColor hslColor = new HSLColor(getBackground());
					GradientPaint paint = new GradientPaint(0, h2, getBackground(), 0, getHeight(),
							hslColor.adjustLuminance(Math.min(100f, Math.max(0f, hslColor.getLuminance() - lDiff))));
					((Graphics2D) g).setPaint(paint);
					g.fillRect(imageOffset, h2, getWidth() - imageOffset, getHeight());
				}
				super.paint(g);
			}
		};
		renderer.setOpenIcon(null);
		renderer.setLeafIcon(null);
		renderer.setClosedIcon(null);
		modelNavigationTree.setModel(new DefaultTreeModel(new DefaultMutableTreeNode("")));
		modelNavigationTree.setRootVisible(false);
		modelNavigationTree.setCellRenderer(renderer);
		modelNavigationTree.addMouseMotionListener(new MouseMotionListener() {
			@Override
			public void mouseMoved(MouseEvent e) {
				TreePath path = modelNavigationTree.getClosestPathForLocation(e.getX(), e.getY());
				if (path != null) {
					Object p = path.getLastPathComponent();
					Object o = null;
					if (p instanceof DefaultMutableTreeNode) {
						o = ((DefaultMutableTreeNode) p).getUserObject();
					}
					if (o instanceof ConnectionInfo) {
						ConnectionInfo ci = (ConnectionInfo) o;
						modelNavigationTree.setToolTipText("<html><b>" + UIUtil.toHTMLFragment(ciRender(ci), 0)
								+ "</b><br>"
								+ (ci.user != null && ci.user.length() > 0 ? UIUtil.toHTMLFragment(ci.user, 0) + "<br>"
										: "")
								+ UIUtil.toHTMLFragment(ci.url, 0)
								+ (ci.getConnectionType().getBackground() != null
										? "<br>" + ci.getConnectionType().displayName
										: ""));
						return;
					}
				}
				modelNavigationTree.setToolTipText(null);
			}

			@Override
			public void mouseDragged(MouseEvent e) {
			}
		});
		modelNavigationTree.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				if (dbConnectionDialog != null) {
					TreePath path = modelNavigationTree.getClosestPathForLocation(e.getX(), e.getY());
					if (path != null) {
						Object p = path.getLastPathComponent();
						Object o = null;
						if (p instanceof DefaultMutableTreeNode) {
							o = ((DefaultMutableTreeNode) p).getUserObject();
						}
						if (SwingUtilities.isLeftMouseButton(e)) {
							if (o instanceof ConnectionInfo) {
								if (e.getClickCount() > 1) {
									connect(DataBrowser.this, o);
								} else {
									dbConnectionDialog.select((ConnectionInfo) o);
								}
							}
						} else if (SwingUtilities.isRightMouseButton(e)) {
							modelNavigationTree.setSelectionPath(path);
							JPopupMenu popup = new JScrollPopupMenu(true);
							if (o instanceof ConnectionInfo) {
								ConnectionInfo ci = (ConnectionInfo) o;
								JMenuItem i = new JMenuItem("<html>Connect <i>(double click)</i></html>");
								i.setIcon(noconnectionIcon);
								i.setEnabled(!ci.equals(currentConnectionInfo));
								popup.add(i);
								i.addActionListener(new ActionListener() {
									@Override
									public void actionPerformed(ActionEvent e) {
										connect(DataBrowser.this, ci);
									}
								});
								i = new JMenuItem("Connect in New Window");
								i.setIcon(noconnectionIconnw);
								i.setEnabled(!ci.equals(currentConnectionInfo));
								popup.add(i);
								popup.add(new JSeparator());
								i.addActionListener(new ActionListener() {
									@Override
									public void actionPerformed(ActionEvent e) {
										DataBrowser newDataBrowser;
										if (executionContext.getCurrentModelSubfolder() != null && executionContext
												.getCurrentModelSubfolder().equals(ci.dataModelFolder)) {
											newDataBrowser = openNewWindow();
										} else {
											newDataBrowser = openNewEmptyWindow();
										}
										if (newDataBrowser != null) {
											newDataBrowser.modelNavigationButtonV.doClick();
											connect(newDataBrowser, ci);
										}
									}
								});
								i = new JMenuItem("Edit");
								i.setIcon(editIcon);
								popup.add(i);
								i.addActionListener(new ActionListener() {
									@Override
									public void actionPerformed(ActionEvent e) {
										UIUtil.invokeLater(4, () -> dbConnectionDialog.doEdit(ci));
										reconnectMenuItemActionPerformed(e);
									}
								});
								i = new JMenuItem("Clone");
								i.setIcon(copyIcon);
								popup.add(i);
								i.addActionListener(new ActionListener() {
									@Override
									public void actionPerformed(ActionEvent e) {
										UIUtil.invokeLater(4, () -> dbConnectionDialog.doClone(ci));
										reconnectMenuItemActionPerformed(e);
									}
								});
								i = new JMenuItem("Delete");
								popup.add(i);
								i.setIcon(deleteIcon);
								i.addActionListener(new ActionListener() {
									@Override
									public void actionPerformed(ActionEvent e) {
										UIUtil.invokeLater(4, () -> dbConnectionDialog.doDelete(ci));
										reconnectMenuItemActionPerformed(e);
									}
								});
							} else {
								JMenuItem i = new JMenuItem("Open");
								i.setEnabled(modelNavigationTree.isCollapsed(path));
								popup.add(i);
								i.addActionListener(new ActionListener() {
									@Override
									public void actionPerformed(ActionEvent e) {
										modelNavigationTree.expandPath(path);
									}
								});
							}
							UIUtil.showPopup(e.getComponent(), e.getX(), e.getY(), popup);
						}
					}
				}
			}

			private void connect(DataBrowser dataBrowser, Object o) {
				ConnectionInfo oldCurrentConnectionInfo = currentConnectionInfo;
				if (dataBrowser.dbConnectionDialog.connectSilent((ConnectionInfo) o)) {
					try {
						if (!dataBrowser.setConnection(dataBrowser.dbConnectionDialog)) {
							if (oldCurrentConnectionInfo != null) {
								dataBrowser.dbConnectionDialog.connectSilent(oldCurrentConnectionInfo);
								updateModelNavigation();
							}
						}
					} catch (Exception ex) {
						UIUtil.showException(dataBrowser, "Error", ex);
					}
				}
			}
		});
	}

	private String ciRender(ConnectionInfo ci) {
		return ci.alias;
	}

	private void updateModelNavigation() {
		final DefaultMutableTreeNode root = new DefaultMutableTreeNode("");
		DefaultTreeModel model = new DefaultTreeModel(root);

		String sf = DataModelManager.getCurrentModelSubfolder(executionContext);
		DataModelManager.setCurrentModelSubfolder(null, executionContext);
		List<String> existingModels = DataModelManager.getModelFolderNames(executionContext);
		DataModelManager.setCurrentModelSubfolder(sf, executionContext);

		List<DefaultMutableTreeNode> current = new ArrayList<DefaultMutableTreeNode>();
		if (dbConnectionDialog != null) {
			if (currentConnectionInfo == null) {
				currentConnectionInfo = dbConnectionDialog.currentConnection;
			}
			Set<String> dup = new HashSet<>();
			String cmsf = DataModelManager.getCurrentModelSubfolder(executionContext);
			try {
				DataModelManager.setCurrentModelSubfolder(null, executionContext);
				Set<String> seen = new HashSet<>();
				for (String m : DataModelManager.getModelFolderNames(executionContext)) {
					String displayName = DataModelManager.getModelDetails(m, executionContext).a;
					if (!seen.add(displayName)) {
						dup.add(displayName);
					}
				}
			} finally {
				DataModelManager.setCurrentModelSubfolder(cmsf, executionContext);
			}
			Map<String, Set<ConnectionInfo>> models = new HashMap<String, Set<ConnectionInfo>>();
			for (ConnectionInfo ci : dbConnectionDialog.getConnectionList()) {
				if (existingModels.contains(ci.dataModelFolder)) {
					Pair<String, Long> modelDetails = DataModelManager.getModelDetails(ci.dataModelFolder,
							executionContext);
					String mName = modelDetails == null ? ci.dataModelFolder : modelDetails.a;
					if (dup.contains(mName)) {
						mName += " (" + ci.dataModelFolder + ")";
					}
					Set<ConnectionInfo> cis = models.get(mName);
					if (cis == null) {
						cis = new TreeSet<DbConnectionDialog.ConnectionInfo>(
								(a, b) -> ciRender(a).compareToIgnoreCase(ciRender(b)));
						models.put(mName, cis);
					}
					cis.add(ci);
				}
			}

			ArrayList<String> keys = new ArrayList<String>(models.keySet());
			keys.sort(String::compareToIgnoreCase);

			for (String mName : keys) {
				Set<ConnectionInfo> cis = models.get(mName);
				DefaultMutableTreeNode node = new DefaultMutableTreeNode(mName.length() == 0 ? "Default" : mName);
				root.add(node);
				for (ConnectionInfo ci : cis) {
					DefaultMutableTreeNode child = new DefaultMutableTreeNode(ci);
					if (currentConnectionInfo != null && ci.alias.equals(currentConnectionInfo.alias)) {
						current.add(child);
					}
					node.add(child);
				}
			}
			;
		}

		Set<String> expandedModels = new HashSet<String>();
		Object oldRoot = modelNavigationTree.getModel().getRoot();
		if (oldRoot != null) {
			for (int i = 0; i < modelNavigationTree.getModel().getChildCount(oldRoot); ++i) {
				Object node = modelNavigationTree.getModel().getChild(oldRoot, i);
				if (node instanceof DefaultMutableTreeNode) {
					if (modelNavigationTree.isExpanded(new TreePath(((DefaultMutableTreeNode) node).getPath()))) {
						Object uo = ((DefaultMutableTreeNode) node).getUserObject();
						if (uo instanceof String) {
							expandedModels.add((String) uo);
						}
					}
				}
			}
		}

		modelNavigationTree.setModel(model);

		for (int i = 0; i < modelNavigationTree.getModel().getChildCount(root); ++i) {
			Object node = modelNavigationTree.getModel().getChild(root, i);
			if (node instanceof DefaultMutableTreeNode) {
				Object uo = ((DefaultMutableTreeNode) node).getUserObject();
				if (uo instanceof String) {
					if (expandedModels.contains((String) uo)) {
						modelNavigationTree.expandPath(new TreePath(((DefaultMutableTreeNode) node).getPath()));
					}
				}
			}
		}

		if (!current.isEmpty()) {
			modelNavigationTree.setSelectionPath(new TreePath(current.get(0).getPath()));
			UIUtil.invokeLater(() -> {
				Rectangle b = modelNavigationTree.getPathBounds(new TreePath(current.get(0).getPath()));
				if (b != null) {
					modelNavigationTree.scrollRectToVisible(new Rectangle(0, b.y, 1, b.height));
				}
			});
		}
	}

	private DataModelChanger createDataModelChanger() {
		return new DataModelChanger() {
			private Runnable afterReconnectAction = null;

			@Override
			public void onConnectionListChanged() {
				updateModelNavigation();
			}

			Map<String, Map<String, String>> schemamappings = new HashMap<String, Map<String, String>>();

			@Override
			public void change(String dataModelSubfolder) {
				afterReconnectAction = null;

//				String sFile = cDTmpFilePrefix + DataModelManager.getCurrentModelSubfolder(executionContext);
//				new File(sFile).deleteOnExit();

				File bookmarksFolder = BookmarksPanel.getBookmarksFolder(executionContext);
				bookmarksFolder.mkdirs();
				String sFile = new File(bookmarksFolder, "default").getPath();

				try {
					desktop.storeSession(sFile);
				} catch (IOException e1) {
					LogUtil.warn(e1);
				}

				schemamappings.put(dataModelSubfolder, desktop.schemaMapping);
				DataModelManager.setCurrentModelSubfolder(dataModelSubfolder, executionContext);
				Map<String, String> schemamapping = schemamappings.get(dataModelSubfolder);
				if (schemamapping == null) {
					schemamapping = new HashMap<String, String>();
				}
				boolean oldSMT = desktop.showMissingTablesOnRestoreSession;
				try {
					desktop.showMissingTablesOnRestoreSession = false;
					bookmarksFolder = BookmarksPanel.getBookmarksFolder(executionContext);
					bookmarksFolder.mkdirs();
					String sessionFile = new File(bookmarksFolder, "default").getPath(); // cDTmpFilePrefix +
																							// dataModelSubfolder;
					desktop.reloadDataModel(schemamapping, !new File(sessionFile).exists(), false, false);
					if (new File(sessionFile).exists()) {
						afterReconnectAction = () -> {
							try {
								BrowserContentPane.suppressReloadStatic = true;
								desktop.restoreSession(null, DataBrowser.this, sessionFile, true);
							} catch (Exception e) {
								LogUtil.warn(e);
							} finally {
								BrowserContentPane.suppressReloadStatic = false;
							}
						};
					}
					desktop.updateBookmarksMenu();
				} catch (Exception e) {
					desktop.showMissingTablesOnRestoreSession = oldSMT;
					UIUtil.showException(DataBrowser.this, "Error", e);
				}
			}

			@Override
			public void afterConnect(ConnectionInfo ci) {
				currentConnectionInfo = ci;
				if (afterReconnectAction != null) {
					afterReconnectAction.run();
					afterReconnectAction = null;
				}
			}

			@Override
			public Window ownerWindow() {
				return DataBrowser.this;
			}
		};
	}

	private static Timer tabSelectionAnimationTimer = null;

	private static synchronized void initTabSelectionAnimationManager() {
		if (tabSelectionAnimationTimer == null) {
			tabSelectionAnimationTimer = new Timer(100, e -> {
				for (Window w : Window.getWindows()) {
					if (w instanceof DataBrowser) {
						DataBrowser b = (DataBrowser) w;
						if (!w.isShowing()) {
							continue;
						}
						SQLConsole sqlConsole = b.getSqlConsole(false);
						if (sqlConsole != null) {
							sqlConsole.repaintShowingAnimatedTables();
						}
					}
				}
			});
			tabSelectionAnimationTimer.setInitialDelay(100);
			tabSelectionAnimationTimer.start();
		}

	}

	private void linkToolbarButton(AbstractButton button, JMenuItem menuItem) {
		String text = menuItem.getText();
		button.setToolTipText(text);
		menuItem.addPropertyChangeListener("enabled", e -> {
			button.setEnabled(menuItem.isEnabled());
		});
		if (button instanceof JToggleButton) {
			button.addChangeListener(e -> {
				if (!button.isSelected()) {
					button.setSelected(menuItem.isSelected());
				}
			});
			menuItem.addChangeListener(e -> {
				button.setSelected(menuItem.isSelected());
			});
		}
		button.addActionListener(e -> {
			if ((!(button instanceof JToggleButton)) || button.isSelected()) {
				desktop.strictZoom = true;
				menuItem.doClick();
				desktop.strictZoom = false;
			}
		});
	}

	private Map<JRadioButtonMenuItem, Integer> rowLimitButtonToLimit = new HashMap<JRadioButtonMenuItem, Integer>();

	private void initRowLimitButtons() {
		ButtonGroup group = new ButtonGroup();
		JRadioButtonMenuItem def = null;
		for (final Integer limit : ROW_LIMITS) {
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

		for (Table table : datamodel.get().getTables()) {
			tables.add(datamodel.get().getDisplayName(table));
		}
		Collections.sort(tables, String::compareToIgnoreCase);
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
		JPanel borderBrowserTabPaneContainer = new JPanel(new BorderLayout());
		panel.tabbedPane.addTab("Closure Border", borderBrowserTabPaneContainer);
		panel.tabbedPane.addTab("Data Model", dataModelPanel);
		panel.tabbedPane.addChangeListener(new ChangeListener() {
			@Override
			public void stateChanged(ChangeEvent e) {
				updateBorderBrowser(borderBrowserTabPaneContainer);
				updateDataModelView(null);
				showDataModelMenuItem
						.setSelected(metaDataDetailsPanel.tabbedPane.getSelectedComponent() == dataModelPanel);
				if (dataModelPanel.getComponentCount() > 0) {
					dataModelPanel.getComponent(0)
							.setVisible(metaDataDetailsPanel.tabbedPane.getSelectedComponent() == dataModelPanel);
				}
			}
		});
		return panel;
	}

	private boolean createSession(DbConnectionDialog dbConnectionDialog) throws Exception {
		ConnectionInfo connection = dbConnectionDialog.currentConnection;
		connectedAliases.clear();
		connectedAliases.add(connection.alias);
		BasicDataSource dataSource = UIUtil.createBasicDataSource(this, connection.driverClass, connection.url,
				connection.user, connection.password, 0, dbConnectionDialog.currentJarURLs());
		SessionForUI newSession = SessionForUI.createSession(dataSource, dataSource.dbms,
				executionContext.getIsolationLevel(), false, true, this);

		if (newSession != null) {
			wasConnected = true;
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
			return true;
		}
		return false;
	}

	private boolean wasConnected = false;

	protected boolean setConnection(DbConnectionDialog dbConnectionDialog) throws Exception {
		String prevDatabaseName = currentDatabaseName;
		if (dbConnectionDialog != null) {
			dbConnectionDialog = new DbConnectionDialog(this, dbConnectionDialog, DataBrowserContext.getAppName(),
					createDataModelChanger(), executionContext);
		}
		this.dbConnectionDialog = dbConnectionDialog;
		updateModelNavigation();
		desktop.dbConnectionDialog = dbConnectionDialog;
		if (dbConnectionDialog != null) {
			ConnectionInfo connection = dbConnectionDialog.currentConnection;
			if (connection != null) {
				if (!createSession(dbConnectionDialog)) {
					return false;
				}
				if (session != null) {
					desktop.session = session;
					onNewSession(session);
					desktop.openSchemaMappingDialog(true);
					updateStatusBar();
					desktop.updateMenu();
					if (prevDatabaseName != null && !prevDatabaseName.equals(currentDatabaseName)) {
						sqlConsoles
								.forEach(sqlConsole -> sqlConsole.onReconnect(prevDatabaseName, currentDatabaseName));
					}
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
		return true;
	}

	private String currentDatabaseName;

	private boolean origL2Op;
	private boolean origKnown = false;
	private static boolean conTypeHadBG = false;
	
	private ConnectionInfo lastConnectionInfo = null;

	@Override
	public void onConnectionTypeChange() {
		ConnectionType connectionType = ConnectionType.Development;
		if (lastConnectionInfo != null && dbConnectionDialog != null) {
			lastConnectionInfo
					.setConnectionType(connectionType = dbConnectionDialog.retrieveConnectionType(lastConnectionInfo));
		}
		Color bg = connectionType != null ? connectionType.getBackground() : null;
		if (bg == null) {
			if (conTypeHadBG) {
				jToolBar1.setToolTipText(null);
				jToolBar1.setBackground(new JToolBar().getBackground());
				schemaNamePanel.setBackground(new JPanel().getBackground());
				connectivityState.setBackground(new JLabel().getBackground());
				legende2.setBackground(new JPanel().getBackground());
				legende2.setOpaque(origL2Op);
			}
		} else {
			conTypeHadBG = true;
			jToolBar1.setToolTipText(connectionType.displayName);
			jToolBar1.setBackground(bg);
			schemaNamePanel.setBackground(bg);
			connectivityState.setBackground(bg);
			legende2.setBackground(bg);
			legende2.setOpaque(true);
		}
		String dburl = lastConnectionInfo != null ? (lastConnectionInfo.url) : " ";
		if (bg != null) {
			dburl = "<html>" + UIUtil.toHTMLFragment(dburl, 0) + "<br><hr>" + connectionType.displayName + "</html>";
		}
		connectivityState.setToolTipText(dburl);
		schemaName.setToolTipText(dburl);
		schemaNamePanel.setToolTipText(dburl);

		for (SQLConsoleWithTitle console : sqlConsoles) {
			console.updateConnectionType(connectionType);
		}
	}

	public void updateStatusBar() {
		final int MAX_LENGTH = 40;
		ConnectionInfo connection = dbConnectionDialog != null ? dbConnectionDialog.currentConnection : null;
		ConnectionType connectionType = connection != null ? connection.getConnectionType() : null;
		if (connection == null) {
			lastConnectionInfo = null;
		} else {
			lastConnectionInfo = new ConnectionInfo();
			lastConnectionInfo.assign(connection);
		}

		if (!origKnown) {
			origKnown = true;
//			origTBBG = jToolBar1.getBackground();
//			origSNPBG = schemaNamePanel.getBackground();
//			origCSBG = connectivityState.getBackground();
//			origL2BG = legende2.getBackground();
			origL2Op = legende2.isOpaque();
		}
		onConnectionTypeChange();
		Color bg = connectionType != null ? connectionType.getBackground() : null;

		String dburl = connection != null ? (connection.url) : " ";
		String dbmsLogoURL = UIUtil.getDBMSLogoURL(dburl);
		if (bg != null) {
			dburl = "<html>" + UIUtil.toHTMLFragment(dburl, 0) + "<br><hr>" + connectionType.displayName + "</html>";
		}
		connectivityState.setToolTipText(dburl);
		schemaName.setToolTipText(dburl);
		schemaNamePanel.setToolTipText(dburl);
		if (dbmsLogoURL == null) {
			connectivityState.setIcon(null);
		} else {
			connectivityState.setIcon(UIUtil.scaleIcon(connectivityState, UIUtil.readImage(dbmsLogoURL, false), 1.5));
		}
		dburl = connection != null
				? ((connection.user != null && connection.user.trim().length() > 0
						&& !connection.alias.startsWith(connection.user + "@") ? connection.user + "@" : "")
						+ connection.alias)
				: " ";
		if (dburl.length() > MAX_LENGTH) {
			dburl = dburl.substring(0, MAX_LENGTH - 3) + "...";
		}
		currentDatabaseName = dburl;
		connectivityState.setText(dburl);
		setTitle((dburl.trim().length() > 0 ? dburl + " - " : "") + DataBrowserContext.getAppName(false));

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
			modelpath = modelpath.substring(0, MAX_LENGTH / 2) + "..."
					+ modelpath.substring(modelpath.length() - MAX_LENGTH / 2);
		}
		modelPath.setText(modelpath);

		if (session != null) {
			Object nonDefaultSchema = session.getSessionProperty(SessionForUI.class, "defSchema");
			if (nonDefaultSchema != null && dburl.startsWith(nonDefaultSchema + "@")) {
				nonDefaultSchema = null;
			}
			schemaNamePanel.setVisible(nonDefaultSchema != null);
			if (nonDefaultSchema != null) {
				schemaName.setText(nonDefaultSchema.toString());
			}
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
	// <editor-fold defaultstate="collapsed" desc="Generated
	// <editor-fold defaultstate="collapsed" desc="Generated
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
        buttonGroupStepTime = new javax.swing.ButtonGroup();
        jLayeredPane2 = new javax.swing.JLayeredPane();
        modelNavigationSplitSizerPanel = new javax.swing.JPanel();
        modelNavigationPanel = new javax.swing.JPanel();
        modelNavigationGapPanel = new javax.swing.JPanel();
        modelNavigationScrollPane = new javax.swing.JScrollPane();
        modelNavigationTree = new javax.swing.JTree();
        modelNavigationButtonV = new javax.swing.JToggleButton();
        modelNavigationConnectButtonV = new javax.swing.JButton();
        jPanel1 = new javax.swing.JPanel();
        jPanel11 = new javax.swing.JPanel();
        legende1 = new javax.swing.JPanel();
        modelName = new javax.swing.JLabel();
        modelPath = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        jSeparator19 = new javax.swing.JSeparator();
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
        jPanel5 = new javax.swing.JPanel();
        workbenchTabbedPane = new javax.swing.JTabbedPane();
        desktopSplitPane = new javax.swing.JSplitPane();
        jPanel4 = new javax.swing.JPanel();
        jLayeredPane1 = new javax.swing.JLayeredPane();
        layeredPaneContent = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        jInternalFrame1 = new javax.swing.JInternalFrame();
        hiddenPanel = new javax.swing.JPanel();
        searchPanelContainer = new javax.swing.JPanel();
        jToolBar1 = new javax.swing.JToolBar();
        searchBarToggleButton = new javax.swing.JToggleButton();
        jSeparator17 = new javax.swing.JToolBar.Separator();
        jSeparator24 = new javax.swing.JToolBar.Separator();
        tbBackButton = new javax.swing.JButton();
        tbForewardButton = new javax.swing.JButton();
        jSeparator18 = new javax.swing.JToolBar.Separator();
        tbClearButton = new javax.swing.JButton();
        jSeparator22 = new javax.swing.JToolBar.Separator();
        tbZoomInButton = new javax.swing.JButton();
        tbZoomOutButton = new javax.swing.JButton();
        tbZoom0Button = new javax.swing.JToggleButton();
        tbZoom1Button = new javax.swing.JToggleButton();
        tbZoom2Button = new javax.swing.JToggleButton();
        tbZoom3Button = new javax.swing.JToggleButton();
        tbZoom4Button = new javax.swing.JToggleButton();
        jSeparator21 = new javax.swing.JToolBar.Separator();
        tbreloadButton = new javax.swing.JButton();
        searchPanelSplitSizerPanel = new javax.swing.JPanel();
        closurePanel = new javax.swing.JPanel();
        consoleDummyPanel = new javax.swing.JPanel();
        addSQLConsoleTab = new javax.swing.JPanel();
        controlPanel = new javax.swing.JPanel();
        jSplitPane4 = new javax.swing.JSplitPane();
        tableTreesTabbedPane = new javax.swing.JTabbedPane();
        navigationPanel = new javax.swing.JPanel();
        outLinePanel = new javax.swing.JPanel();
        jToolBar2 = new javax.swing.JToolBar();
        openTableButton = new javax.swing.JButton();
        navTreeLayeredPane = new javax.swing.JLayeredPane();
        navigationTreeScrollPane = new javax.swing.JScrollPane();
        navigationTree = new javax.swing.JTree();
        tablesCardPanel = new javax.swing.JPanel();
        tablesPanel = new javax.swing.JPanel();
        jPanel6 = new javax.swing.JPanel();
        jScrollPane2 = new javax.swing.JScrollPane();
        jTree1 = new javax.swing.JTree();
        refreshButton = new javax.swing.JButton();
        jPanel7 = new javax.swing.JPanel();
        metaDataViewPanel = new javax.swing.JPanel();
        initialHGPanel = new javax.swing.JPanel();
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
        topLayerPanel = new javax.swing.JPanel();
        menuBar = new javax.swing.JMenuBar();
        jMenu1 = new javax.swing.JMenu();
        jMenuItem3 = new javax.swing.JMenuItem();
        closeAllMenuItem = new javax.swing.JMenuItem();
        jSeparator7 = new javax.swing.JPopupMenu.Separator();
        reconnectMenuItem = new javax.swing.JMenuItem();
        newBrowserjMenuItem = new javax.swing.JMenuItem();
        jSeparator4 = new javax.swing.JPopupMenu.Separator();
        loadScriptMenuItem = new javax.swing.JMenuItem();
        saveScriptMenuItem = new javax.swing.JMenuItem();
        saveScriptAsMenuItem = new javax.swing.JMenuItem();
        jSeparator9 = new javax.swing.JPopupMenu.Separator();
        storeSessionItem = new javax.swing.JMenuItem();
        restoreSessionItem = new javax.swing.JMenuItem();
        jSeparator12 = new javax.swing.JPopupMenu.Separator();
        exportMenuItem = new javax.swing.JMenuItem();
        importMenuItem = new javax.swing.JMenuItem();
        jSeparator23 = new javax.swing.JPopupMenu.Separator();
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
        jSeparator25 = new javax.swing.JPopupMenu.Separator();
        generateDDLMenuItem = new javax.swing.JMenuItem();
        jviewMenu = new javax.swing.JMenu();
        rowLimitMenu = new javax.swing.JMenu();
        jSeparator3 = new javax.swing.JPopupMenu.Separator();
        goBackItem = new javax.swing.JMenuItem();
        goForwardItem = new javax.swing.JMenuItem();
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
        newEmptyWindowMenuItem = new javax.swing.JMenuItem();
        jSeparator6 = new javax.swing.JPopupMenu.Separator();
        view = new javax.swing.JMenu();
        jMenu3 = new javax.swing.JMenu();
        plafMenu = new javax.swing.JMenu();
        jSeparator20 = new javax.swing.JPopupMenu.Separator();
        autoLayoutMenuItem = new javax.swing.JCheckBoxMenuItem();
        zoomWithMouseWheelMenuItem = new javax.swing.JCheckBoxMenuItem();
        jSeparator16 = new javax.swing.JPopupMenu.Separator();
        animationStepTimeMenu = new javax.swing.JMenu();
        steptime10 = new javax.swing.JRadioButtonMenuItem();
        steptime20 = new javax.swing.JRadioButtonMenuItem();
        steptime30 = new javax.swing.JRadioButtonMenuItem();
        steptime50 = new javax.swing.JRadioButtonMenuItem();
        steptime75 = new javax.swing.JRadioButtonMenuItem();
        steptime100 = new javax.swing.JRadioButtonMenuItem();
        steptime200 = new javax.swing.JRadioButtonMenuItem();
        steptime300 = new javax.swing.JRadioButtonMenuItem();
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

        modelNavigationSplitSizerPanel.setMinimumSize(new java.awt.Dimension(8, 0));
        modelNavigationSplitSizerPanel.setPreferredSize(new java.awt.Dimension(8, 0));
        modelNavigationSplitSizerPanel.setLayout(new javax.swing.BoxLayout(modelNavigationSplitSizerPanel, javax.swing.BoxLayout.LINE_AXIS));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.VERTICAL;
        jLayeredPane2.add(modelNavigationSplitSizerPanel, gridBagConstraints);

        modelNavigationPanel.setLayout(new java.awt.GridBagLayout());

        modelNavigationGapPanel.setLayout(new javax.swing.BoxLayout(modelNavigationGapPanel, javax.swing.BoxLayout.LINE_AXIS));

        javax.swing.tree.DefaultMutableTreeNode treeNode1 = new javax.swing.tree.DefaultMutableTreeNode(".");
        modelNavigationTree.setModel(new javax.swing.tree.DefaultTreeModel(treeNode1));
        modelNavigationScrollPane.setViewportView(modelNavigationTree);

        modelNavigationGapPanel.add(modelNavigationScrollPane);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.gridheight = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(61, 0, 0, 0);
        modelNavigationPanel.add(modelNavigationGapPanel, gridBagConstraints);

        modelNavigationButtonV.setText(" Connections ");
        modelNavigationButtonV.setFocusable(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 4);
        modelNavigationPanel.add(modelNavigationButtonV, gridBagConstraints);

        modelNavigationConnectButtonV.setText("jButton2");
        modelNavigationConnectButtonV.setToolTipText("Open connection dialog");
        modelNavigationConnectButtonV.setFocusable(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(61, 0, 4, 4);
        modelNavigationPanel.add(modelNavigationConnectButtonV, gridBagConstraints);

        jLayeredPane2.setLayer(modelNavigationPanel, javax.swing.JLayeredPane.PALETTE_LAYER);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        jLayeredPane2.add(modelNavigationPanel, gridBagConstraints);

        jPanel1.setLayout(new java.awt.GridBagLayout());

        jPanel11.setLayout(new java.awt.GridBagLayout());

        legende1.setLayout(new java.awt.GridBagLayout());

        modelName.setFont(modelName.getFont().deriveFont(modelName.getFont().getSize()+1f));
        modelName.setText("Data Model \"Demo\"");
        modelName.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                modelNameMouseClicked(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 12);
        legende1.add(modelName, gridBagConstraints);

        modelPath.setFont(modelPath.getFont().deriveFont(modelPath.getFont().getSize()+1f));
        modelPath.setForeground(/* Renaming also in *.form! */ Colors.Color_128_128_128);
        modelPath.setText("/home/jailer/datamodel/");
        modelPath.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                modelPathMouseClicked(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        legende1.add(modelPath, gridBagConstraints);

        jLabel2.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.weightx = 1.0;
        legende1.add(jLabel2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 2);
        jPanel11.add(legende1, gridBagConstraints);

        jSeparator19.setOrientation(javax.swing.SwingConstants.VERTICAL);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        jPanel11.add(jSeparator19, gridBagConstraints);

        legende.setLayout(new java.awt.GridBagLayout());

        dependsOn.setFont(dependsOn.getFont().deriveFont(dependsOn.getFont().getSize()+1f));
        dependsOn.setForeground(/* Renaming also in *.form! */ Colors.Color_170_0_0);
        dependsOn.setText(" Parent (depends on) ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        legende.add(dependsOn, gridBagConstraints);

        hasDependent.setFont(hasDependent.getFont().deriveFont(hasDependent.getFont().getSize()+1f));
        hasDependent.setForeground(/* Renaming also in *.form! */ Colors.Color_0_112_0);
        hasDependent.setText("  Child (has dependent) ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        legende.add(hasDependent, gridBagConstraints);

        associatedWith.setFont(associatedWith.getFont().deriveFont(associatedWith.getFont().getSize()+1f));
        associatedWith.setForeground(/* Renaming also in *.form! */ Colors.Color_0_100_255);
        associatedWith.setText("  associated with");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        legende.add(associatedWith, gridBagConstraints);

        ignored.setFont(ignored.getFont().deriveFont(ignored.getFont().getSize()+1f));
        ignored.setForeground(/* Renaming also in *.form! */ Colors.Color_153_153_153);
        ignored.setText("  disabled ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        legende.add(ignored, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.VERTICAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 2);
        jPanel11.add(legende, gridBagConstraints);

        schemaNamePanel.setLayout(new java.awt.GridBagLayout());

        schemaName.setFont(schemaName.getFont().deriveFont(schemaName.getFont().getSize()+1f));
        schemaName.setText("Schema");
        schemaName.setCursor(new java.awt.Cursor(java.awt.Cursor.DEFAULT_CURSOR));
        schemaName.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                schemaNameMouseClicked(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 12);
        schemaNamePanel.add(schemaName, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel11.add(schemaNamePanel, gridBagConstraints);

        legende2.setLayout(new java.awt.GridBagLayout());

        connectivityState.setFont(connectivityState.getFont().deriveFont(connectivityState.getFont().getSize()+1f));
        connectivityState.setText("offline");
        connectivityState.setCursor(new java.awt.Cursor(java.awt.Cursor.DEFAULT_CURSOR));
        connectivityState.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                connectivityStateMouseClicked(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        legende2.add(connectivityState, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 6;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
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
        jSplitPane1.setOneTouchExpandable(true);

        jPanel5.setLayout(new java.awt.GridBagLayout());

        workbenchTabbedPane.addChangeListener(new javax.swing.event.ChangeListener() {
            public void stateChanged(javax.swing.event.ChangeEvent evt) {
                workbenchTabbedPaneStateChanged(evt);
            }
        });

        desktopSplitPane.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        desktopSplitPane.setResizeWeight(0.95);
        desktopSplitPane.setName("desktopSplitPane"); // NOI18N
        desktopSplitPane.setOneTouchExpandable(true);

        jPanel4.setLayout(new java.awt.GridBagLayout());

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
        layeredPaneContent.setBounds(0, 0, 30, 38);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel4.add(jLayeredPane1, gridBagConstraints);

        searchPanelContainer.setLayout(new java.awt.BorderLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        jPanel4.add(searchPanelContainer, gridBagConstraints);

        jToolBar1.setRollover(true);

        searchBarToggleButton.setText("search");
        searchBarToggleButton.setToolTipText("Open Search Sidebar");
        searchBarToggleButton.setFocusable(false);
        searchBarToggleButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        searchBarToggleButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        searchBarToggleButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                searchBarToggleButtonActionPerformed(evt);
            }
        });
        jToolBar1.add(searchBarToggleButton);
        jToolBar1.add(jSeparator17);
        jToolBar1.add(jSeparator24);

        tbBackButton.setText("back");
        tbBackButton.setFocusable(false);
        tbBackButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        tbBackButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar1.add(tbBackButton);

        tbForewardButton.setText("forward");
        tbForewardButton.setFocusable(false);
        tbForewardButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        tbForewardButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar1.add(tbForewardButton);
        jToolBar1.add(jSeparator18);

        tbClearButton.setText("clear");
        tbClearButton.setToolTipText("Close all Tables");
        tbClearButton.setFocusable(false);
        tbClearButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        tbClearButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        tbClearButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                tbClearButtonActionPerformed(evt);
            }
        });
        jToolBar1.add(tbClearButton);
        jToolBar1.add(jSeparator22);

        tbZoomInButton.setText("zoomin");
        tbZoomInButton.setFocusable(false);
        tbZoomInButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        tbZoomInButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar1.add(tbZoomInButton);

        tbZoomOutButton.setText("zoomin");
        tbZoomOutButton.setFocusable(false);
        tbZoomOutButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        tbZoomOutButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        tbZoomOutButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                tbZoomOutButtonActionPerformed(evt);
            }
        });
        jToolBar1.add(tbZoomOutButton);

        tbZoom0Button.setText("zoom0");
        tbZoom0Button.setFocusable(false);
        tbZoom0Button.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        tbZoom0Button.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        tbZoom0Button.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                tbZoom0ButtonActionPerformed(evt);
            }
        });
        jToolBar1.add(tbZoom0Button);

        tbZoom1Button.setText("zoom0");
        tbZoom1Button.setFocusable(false);
        tbZoom1Button.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        tbZoom1Button.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        tbZoom1Button.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                tbZoom1ButtonActionPerformed(evt);
            }
        });
        jToolBar1.add(tbZoom1Button);

        tbZoom2Button.setText("zoom0");
        tbZoom2Button.setFocusable(false);
        tbZoom2Button.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        tbZoom2Button.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        tbZoom2Button.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                tbZoom2ButtonActionPerformed(evt);
            }
        });
        jToolBar1.add(tbZoom2Button);

        tbZoom3Button.setText("zoom0");
        tbZoom3Button.setFocusable(false);
        tbZoom3Button.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        tbZoom3Button.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        tbZoom3Button.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                tbZoom3ButtonActionPerformed(evt);
            }
        });
        jToolBar1.add(tbZoom3Button);

        tbZoom4Button.setText("zoom0");
        tbZoom4Button.setFocusable(false);
        tbZoom4Button.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        tbZoom4Button.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        tbZoom4Button.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                tbZoom4ButtonActionPerformed(evt);
            }
        });
        jToolBar1.add(tbZoom4Button);
        jToolBar1.add(jSeparator21);

        tbreloadButton.setText("Reload");
        tbreloadButton.setToolTipText("Reload Rows of all Tables");
        tbreloadButton.setFocusable(false);
        tbreloadButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        tbreloadButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        tbreloadButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                tbreloadButtonActionPerformed(evt);
            }
        });
        jToolBar1.add(tbreloadButton);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        jPanel4.add(jToolBar1, gridBagConstraints);

        searchPanelSplitSizerPanel.setMaximumSize(new java.awt.Dimension(8, 2));
        searchPanelSplitSizerPanel.setMinimumSize(new java.awt.Dimension(8, 0));
        searchPanelSplitSizerPanel.setPreferredSize(new java.awt.Dimension(8, 2));
        searchPanelSplitSizerPanel.setLayout(new javax.swing.BoxLayout(searchPanelSplitSizerPanel, javax.swing.BoxLayout.LINE_AXIS));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.VERTICAL;
        jPanel4.add(searchPanelSplitSizerPanel, gridBagConstraints);

        desktopSplitPane.setTopComponent(jPanel4);

        closurePanel.setLayout(new java.awt.GridBagLayout());
        desktopSplitPane.setBottomComponent(closurePanel);

        workbenchTabbedPane.addTab("Desktop", desktopSplitPane);
        workbenchTabbedPane.addTab("SQL Console", consoleDummyPanel);
        workbenchTabbedPane.addTab("+", addSQLConsoleTab);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel5.add(workbenchTabbedPane, gridBagConstraints);

        jSplitPane1.setRightComponent(jPanel5);

        controlPanel.setLayout(new java.awt.GridBagLayout());

        jSplitPane4.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        jSplitPane4.setResizeWeight(1.0);
        jSplitPane4.setOneTouchExpandable(true);

        navigationPanel.setLayout(new java.awt.GridBagLayout());

        outLinePanel.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        navigationPanel.add(outLinePanel, gridBagConstraints);

        jToolBar2.setRollover(true);

        openTableButton.setText("Open");
        openTableButton.setToolTipText("Open table browser for the selected table");
        openTableButton.setFocusable(false);
        openTableButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        openTableButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        openTableButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                openTableButtonActionPerformed(evt);
            }
        });
        jToolBar2.add(openTableButton);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        navigationPanel.add(jToolBar2, gridBagConstraints);

        navTreeLayeredPane.setLayout(new java.awt.GridBagLayout());

        navigationTree.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                navigationTreeMouseClicked(evt);
            }
        });
        navigationTreeScrollPane.setViewportView(navigationTree);

        navTreeLayeredPane.setLayer(navigationTreeScrollPane, javax.swing.JLayeredPane.PALETTE_LAYER);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        navTreeLayeredPane.add(navigationTreeScrollPane, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        navigationPanel.add(navTreeLayeredPane, gridBagConstraints);

        tableTreesTabbedPane.addTab("Navigation", navigationPanel);

        tablesCardPanel.setLayout(new java.awt.CardLayout());

        tablesPanel.setLayout(new java.awt.BorderLayout());
        tablesCardPanel.add(tablesPanel, "tables");

        jPanel6.setLayout(new java.awt.GridBagLayout());

        treeNode1 = new javax.swing.tree.DefaultMutableTreeNode("loading database meta data...");
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

        initialHGPanel.setLayout(new java.awt.GridBagLayout());

        jLabel1.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        initialHGPanel.add(jLabel1, gridBagConstraints);

        jLabel3.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        initialHGPanel.add(jLabel3, gridBagConstraints);

        jLabel4.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        initialHGPanel.add(jLabel4, gridBagConstraints);

        jLabel5.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 4;
        initialHGPanel.add(jLabel5, gridBagConstraints);

        jLabel6.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 5;
        initialHGPanel.add(jLabel6, gridBagConstraints);

        jLabel7.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 6;
        initialHGPanel.add(jLabel7, gridBagConstraints);

        jLabel8.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 7;
        initialHGPanel.add(jLabel8, gridBagConstraints);

        jLabel9.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 8;
        initialHGPanel.add(jLabel9, gridBagConstraints);

        jLabel10.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 10;
        initialHGPanel.add(jLabel10, gridBagConstraints);

        jLabel11.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 11;
        initialHGPanel.add(jLabel11, gridBagConstraints);

        jLabel12.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 12;
        initialHGPanel.add(jLabel12, gridBagConstraints);

        jLabel13.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 13;
        initialHGPanel.add(jLabel13, gridBagConstraints);

        jLabel14.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 14;
        initialHGPanel.add(jLabel14, gridBagConstraints);

        jLabel19.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 19;
        initialHGPanel.add(jLabel19, gridBagConstraints);

        jLabel20.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 20;
        initialHGPanel.add(jLabel20, gridBagConstraints);

        jLabel21.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 21;
        initialHGPanel.add(jLabel21, gridBagConstraints);

        jLabel22.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 22;
        initialHGPanel.add(jLabel22, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        jPanel7.add(initialHGPanel, gridBagConstraints);

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
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jLayeredPane2.add(jPanel1, gridBagConstraints);

        updateInfoPanel.setBackground(/* Renaming also in *.form! */ Colors.Color_255_255_236);
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
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHEAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 72, 24);
        jLayeredPane2.add(updateInfoPanel, gridBagConstraints);

        topLayerPanel.setOpaque(false);
        topLayerPanel.setLayout(null);
        jLayeredPane2.setLayer(topLayerPanel, javax.swing.JLayeredPane.PALETTE_LAYER);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        jLayeredPane2.add(topLayerPanel, gridBagConstraints);

        getContentPane().add(jLayeredPane2, java.awt.BorderLayout.CENTER);

        jMenu1.setText("File");

        jMenuItem3.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_T, java.awt.event.InputEvent.CTRL_DOWN_MASK));
        jMenuItem3.setText("Open Table");
        jMenuItem3.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem3ActionPerformed(evt);
            }
        });
        jMenu1.add(jMenuItem3);

        closeAllMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F4, java.awt.event.InputEvent.SHIFT_DOWN_MASK | java.awt.event.InputEvent.CTRL_DOWN_MASK));
        closeAllMenuItem.setText("Close all Tables");
        closeAllMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                closeAllMenuItemActionPerformed(evt);
            }
        });
        jMenu1.add(closeAllMenuItem);
        jMenu1.add(jSeparator7);

        reconnectMenuItem.setText("Connect to Database...");
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
        jMenu1.add(jSeparator4);

        loadScriptMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_O, java.awt.event.InputEvent.CTRL_DOWN_MASK));
        loadScriptMenuItem.setText("Load SQL Script...");
        loadScriptMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                loadScriptMenuItemActionPerformed(evt);
            }
        });
        jMenu1.add(loadScriptMenuItem);

        saveScriptMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_S, java.awt.event.InputEvent.CTRL_DOWN_MASK));
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

        storeSessionItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_P, java.awt.event.InputEvent.CTRL_DOWN_MASK));
        storeSessionItem.setText("Store Layout");
        storeSessionItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                storeSessionItemActionPerformed(evt);
            }
        });
        jMenu1.add(storeSessionItem);

        restoreSessionItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_R, java.awt.event.InputEvent.CTRL_DOWN_MASK));
        restoreSessionItem.setText("Restore Layout");
        restoreSessionItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                restoreSessionItemActionPerformed(evt);
            }
        });
        jMenu1.add(restoreSessionItem);
        jMenu1.add(jSeparator12);

        exportMenuItem.setText("Export Models and Connections");
        exportMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                exportMenuItemActionPerformed(evt);
            }
        });
        jMenu1.add(exportMenuItem);

        importMenuItem.setText("Import Models and Connections");
        importMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                importMenuItemActionPerformed(evt);
            }
        });
        jMenu1.add(importMenuItem);
        jMenu1.add(jSeparator23);

        exitMenuItem.setText("Exit");
        exitMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                exitMenuItemActionPerformed(evt);
            }
        });
        jMenu1.add(exitMenuItem);

        menuBar.add(jMenu1);

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
        menuTools.add(jSeparator25);

        generateDDLMenuItem.setText("Generate DDL Script");
        generateDDLMenuItem.setToolTipText("Generate a SQL/DDL script that creates the database objects (CREATE TABLE, VIEW etc.)");
        generateDDLMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                generateDDLMenuItemActionPerformed(evt);
            }
        });
        menuTools.add(generateDDLMenuItem);

        menuBar.add(menuTools);

        jviewMenu.setText("View");

        rowLimitMenu.setText("Row Limit");
        rowLimitMenu.setToolTipText("Desktop Row Limit");
        jviewMenu.add(rowLimitMenu);
        jviewMenu.add(jSeparator3);

        goBackItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_LEFT, java.awt.event.InputEvent.ALT_DOWN_MASK));
        goBackItem.setText("Go Back");
        jviewMenu.add(goBackItem);

        goForwardItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_RIGHT, java.awt.event.InputEvent.ALT_DOWN_MASK));
        goForwardItem.setText("Go Forward");
        jviewMenu.add(goForwardItem);

        menuBar.add(jviewMenu);

        bookmarkMenu.setText("Layout");

        addBookmarkMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_B, java.awt.event.InputEvent.CTRL_DOWN_MASK));
        addBookmarkMenuItem.setText("Add Desktop Layout");
        addBookmarkMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                addBookmarkMenuItemActionPerformed(evt);
            }
        });
        bookmarkMenu.add(addBookmarkMenuItem);

        editBookmarkMenuItem.setText("Edit Desktop Layouts");
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

        layoutMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_L, java.awt.event.InputEvent.CTRL_DOWN_MASK));
        layoutMenuItem.setText("Arrange Layout");
        layoutMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                layoutMenuItemActionPerformed(evt);
            }
        });
        menuWindow.add(layoutMenuItem);
        menuWindow.add(jSeparator5);

        zoomInMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_PLUS, java.awt.event.InputEvent.CTRL_DOWN_MASK));
        zoomInMenuItem.setText("Zoom In (%Mouse Wheel Up)");
        zoomInMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                zoomInMenuItemActionPerformed(evt);
            }
        });
        menuWindow.add(zoomInMenuItem);

        zoomOutMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_MINUS, java.awt.event.InputEvent.CTRL_DOWN_MASK));
        zoomOutMenuItem.setText("Zoom Out (%Mouse Wheel Down)");
        zoomOutMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                zoomOutMenuItemActionPerformed(evt);
            }
        });
        menuWindow.add(zoomOutMenuItem);
        menuWindow.add(jSeparator15);

        thumbnailLayoutRadioButtonMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_0, java.awt.event.InputEvent.CTRL_DOWN_MASK));
        thumbnailLayoutRadioButtonMenuItem.setText("Thumbnail Layout");
        thumbnailLayoutRadioButtonMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                thumbnailLayoutRadioButtonMenuItemActionPerformed(evt);
            }
        });
        menuWindow.add(thumbnailLayoutRadioButtonMenuItem);

        tinyLayoutRadioButtonMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_1, java.awt.event.InputEvent.CTRL_DOWN_MASK));
        tinyLayoutRadioButtonMenuItem.setText("Tiny Layout");
        tinyLayoutRadioButtonMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                tinyLayoutRadioButtonMenuItemActionPerformed(evt);
            }
        });
        menuWindow.add(tinyLayoutRadioButtonMenuItem);

        smallLayoutRadioButtonMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_2, java.awt.event.InputEvent.CTRL_DOWN_MASK));
        smallLayoutRadioButtonMenuItem.setText("Small Layout");
        smallLayoutRadioButtonMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                smallLayoutRadioButtonMenuItemActionPerformed(evt);
            }
        });
        menuWindow.add(smallLayoutRadioButtonMenuItem);

        mediumLayoutRadioButtonMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_3, java.awt.event.InputEvent.CTRL_DOWN_MASK));
        mediumLayoutRadioButtonMenuItem.setText("Medium Layout");
        mediumLayoutRadioButtonMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                mediumLayoutRadioButtonMenuItemActionPerformed(evt);
            }
        });
        menuWindow.add(mediumLayoutRadioButtonMenuItem);

        largeLayoutRadioButtonMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_4, java.awt.event.InputEvent.CTRL_DOWN_MASK));
        largeLayoutRadioButtonMenuItem.setText("Large Layout");
        largeLayoutRadioButtonMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                largeLayoutRadioButtonMenuItemActionPerformed(evt);
            }
        });
        menuWindow.add(largeLayoutRadioButtonMenuItem);
        menuWindow.add(jSeparator1);

        newWindowMenuItem.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_N, java.awt.event.InputEvent.CTRL_DOWN_MASK));
        newWindowMenuItem.setText("New Window");
        newWindowMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                newWindowMenuItemActionPerformed(evt);
            }
        });
        menuWindow.add(newWindowMenuItem);

        newEmptyWindowMenuItem.setText("New Empty Window");
        newEmptyWindowMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                newEmptyWindowMenuItemActionPerformed(evt);
            }
        });
        menuWindow.add(newEmptyWindowMenuItem);
        menuWindow.add(jSeparator6);

        view.setText("Look&Feel");
        menuWindow.add(view);

        menuBar.add(menuWindow);

        jMenu3.setText("Settings");

        plafMenu.setText("Theme (Look and Feel)");
        jMenu3.add(plafMenu);
        jMenu3.add(jSeparator20);

        autoLayoutMenuItem.setSelected(true);
        autoLayoutMenuItem.setText("Automatic layout adjustment");
        jMenu3.add(autoLayoutMenuItem);

        zoomWithMouseWheelMenuItem.setSelected(true);
        zoomWithMouseWheelMenuItem.setText("Zoom with Mouse Wheel");
        zoomWithMouseWheelMenuItem.setToolTipText("Zooming instead of scrolling when the mouse wheel is rolled without control key.");
        zoomWithMouseWheelMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                zoomWithMouseWheelMenuItemActionPerformed(evt);
            }
        });
        jMenu3.add(zoomWithMouseWheelMenuItem);
        jMenu3.add(jSeparator16);

        animationStepTimeMenu.setText("Animation step time");

        buttonGroupStepTime.add(steptime10);
        steptime10.setText("10 ms (fast, high CPU load)");
        steptime10.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                steptime10ActionPerformed(evt);
            }
        });
        animationStepTimeMenu.add(steptime10);

        buttonGroupStepTime.add(steptime20);
        steptime20.setText("20 ms");
        steptime20.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                steptime20ActionPerformed(evt);
            }
        });
        animationStepTimeMenu.add(steptime20);

        buttonGroupStepTime.add(steptime30);
        steptime30.setText("30 ms");
        steptime30.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                steptime30ActionPerformed(evt);
            }
        });
        animationStepTimeMenu.add(steptime30);

        buttonGroupStepTime.add(steptime50);
        steptime50.setText("50 ms");
        steptime50.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                steptime50ActionPerformed(evt);
            }
        });
        animationStepTimeMenu.add(steptime50);

        buttonGroupStepTime.add(steptime75);
        steptime75.setText("75 ms");
        steptime75.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                steptime75ActionPerformed(evt);
            }
        });
        animationStepTimeMenu.add(steptime75);

        buttonGroupStepTime.add(steptime100);
        steptime100.setText("100 ms");
        steptime100.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                steptime100ActionPerformed(evt);
            }
        });
        animationStepTimeMenu.add(steptime100);

        buttonGroupStepTime.add(steptime200);
        steptime200.setText("200 ms");
        steptime200.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                steptime200ActionPerformed(evt);
            }
        });
        animationStepTimeMenu.add(steptime200);

        buttonGroupStepTime.add(steptime300);
        steptime300.setText("300 ms (slow, low CPU load)");
        steptime300.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                steptime300ActionPerformed(evt);
            }
        });
        animationStepTimeMenu.add(steptime300);

        jMenu3.add(animationStepTimeMenu);

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

	private void zoomWithMouseWheelMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_zoomWithMouseWheelMenuItemActionPerformed
		boolean zoom = zoomWithMouseWheelMenuItem.isSelected();
		setZoomWithMouseWheel(zoom);
		UISettings.store(UISettings.ZOOM_WITH_MOUSE_WHEEL,
				zoomWithMouseWheel = zoomWithMouseWheelMenuItem.isSelected());
	}// GEN-LAST:event_zoomWithMouseWheelMenuItemActionPerformed

	private void connectivityStateMouseClicked(java.awt.event.MouseEvent evt) {// GEN-FIRST:event_connectivityStateMouseClicked
		if (SwingUtilities.isLeftMouseButton(evt)) {
			reconnectMenuItemActionPerformed(null);
		}
	}// GEN-LAST:event_connectivityStateMouseClicked

	private void schemaNameMouseClicked(java.awt.event.MouseEvent evt) {// GEN-FIRST:event_schemaNameMouseClicked
		if (SwingUtilities.isLeftMouseButton(evt)) {
			reconnectMenuItemActionPerformed(null);
		}
	}// GEN-LAST:event_schemaNameMouseClicked

	private void modelNameMouseClicked(java.awt.event.MouseEvent evt) {// GEN-FIRST:event_modelNameMouseClicked
		if (SwingUtilities.isLeftMouseButton(evt)) {
			newBrowserjMenuItemActionPerformed(null);
		}
	}// GEN-LAST:event_modelNameMouseClicked

	private void modelPathMouseClicked(java.awt.event.MouseEvent evt) {// GEN-FIRST:event_modelPathMouseClicked
		if (SwingUtilities.isLeftMouseButton(evt)) {
			newBrowserjMenuItemActionPerformed(null);
		}
	}// GEN-LAST:event_modelPathMouseClicked

	/**
	 * Initializes the "step time" menu items.
	 */
	private void initAnimationSteptime() {
		Integer st = (Integer) UISettings.restore("ANIMATION_STEP_TIME_B");
		if (st != null) {
			desktop.animationStepTime.set(st);
		}
		switch (desktop.animationStepTime.get()) {
		case 0:
			steptime10.setSelected(true);
			break;
		case 10:
			steptime10.setSelected(true);
			break;
		case 20:
			steptime20.setSelected(true);
			break;
		case 30:
			steptime30.setSelected(true);
			break;
		case 50:
			steptime50.setSelected(true);
			break;
		case 75:
			steptime75.setSelected(true);
			break;
		case 100:
			steptime100.setSelected(true);
			break;
		case 200:
			steptime200.setSelected(true);
			break;
		case 300:
			steptime300.setSelected(true);
			break;
		}
	}

	private void setAnimationSteptime(int steptime) {
		desktop.animationStepTime.set(steptime);
		UISettings.store("ANIMATION_STEP_TIME_B", desktop.animationStepTime.get());
	}

	private void steptime10ActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_steptime10ActionPerformed
		setAnimationSteptime(0);
	}// GEN-LAST:event_steptime10ActionPerformed

	private void steptime20ActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_steptime20ActionPerformed
		setAnimationSteptime(20);
	}// GEN-LAST:event_steptime20ActionPerformed

	private void steptime30ActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_steptime30ActionPerformed
		setAnimationSteptime(30);
	}// GEN-LAST:event_steptime30ActionPerformed

	private void steptime50ActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_steptime50ActionPerformed
		setAnimationSteptime(50);
	}// GEN-LAST:event_steptime50ActionPerformed

	private void steptime75ActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_steptime75ActionPerformed
		setAnimationSteptime(75);
	}// GEN-LAST:event_steptime75ActionPerformed

	private void steptime100ActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_steptime100ActionPerformed
		setAnimationSteptime(100);
	}// GEN-LAST:event_steptime100ActionPerformed

	private void steptime200ActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_steptime200ActionPerformed
		setAnimationSteptime(200);
	}// GEN-LAST:event_steptime200ActionPerformed

	private void steptime300ActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_steptime300ActionPerformed
		setAnimationSteptime(300);
	}// GEN-LAST:event_steptime300ActionPerformed

	private void tbreloadButtonActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_tbreloadButtonActionPerformed
		desktop.tableBrowsers.forEach(tb -> {
			if (tb.parent == null && tb.browserContentPane != null) {
				tb.browserContentPane.reloadRows();
			}
		});
	}// GEN-LAST:event_tbreloadButtonActionPerformed

	private void newEmptyWindowMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_newEmptyWindowMenuItemActionPerformed
		openNewEmptyWindow();
	}// GEN-LAST:event_newEmptyWindowMenuItemActionPerformed

	private void tbClearButtonActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_tbClearButtonActionPerformed
		desktop.closeAll();
	}// GEN-LAST:event_tbClearButtonActionPerformed

	private void exportMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_exportMenuItemActionPerformed
		dbConnectionDialog.exportConnections(this, null);
	}// GEN-LAST:event_exportMenuItemActionPerformed

	private void importMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_importMenuItemActionPerformed
		dbConnectionDialog.importConnections(this);
	}// GEN-LAST:event_importMenuItemActionPerformed

	private void generateDDLMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_generateDDLMenuItemActionPerformed
		DDLScriptGeneratorPanel.open(this, null, datamodel.get(), null, null, false, session,
				dbConnectionDialog.getExecutionContext());
	}// GEN-LAST:event_generateDDLMenuItemActionPerformed

	private void newWindowMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_newWindowMenuItemActionPerformed
		openNewWindow();
	}// GEN-LAST:event_newWindowMenuItemActionPerformed

	private DataBrowser openNewWindow() {
		try {
			File tmpFile = Configuration.getInstance().createTempFile();
			desktop.storeSession(tmpFile.toString());
			DataBrowser newBrowser = openNewDataBrowser(datamodel.get(), dbConnectionDialog, false, executionContext,
					null);
			newBrowser.desktop.restoreSession(null, newBrowser, tmpFile.toString(), true);
			tmpFile.delete();
			return newBrowser;
		} catch (Exception e) {
			UIUtil.showException(this, "Error", e, session);
		}
		return null;
	}

	private DataBrowser openNewEmptyWindow() {
		try {
			return openNewDataBrowser(datamodel.get(), dbConnectionDialog, false, executionContext, null);
		} catch (Exception e) {
			UIUtil.showException(this, "Error", e, session);
		}
		return null;
	}

	private void exportDataMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_exportDataMenuItemActionPerformed
		desktop.createExtractionModel(true);
	}// GEN-LAST:event_exportDataMenuItemActionPerformed

	private void dataImportActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_dataImportActionPerformed
		if (!UIUtil.canRunJailer()) {
			return;
		}
		try {
			String sqlFile = UIUtil.choseFile(null, ".", "Data Import", ".sql", this, false, true);
			if (sqlFile != null) {
				DbConnectionDialog dcd = new DbConnectionDialog(this, dbConnectionDialog,
						DataBrowserContext.getAppName(), null, executionContext);
				if (dcd.connect("Data Import")) {
					List<String> args = new ArrayList<String>();
					args.add("import");
					args.add(sqlFile);
					dcd.addDbArgs(args);
					ImportDialog importDialog = new ImportDialog(this, sqlFile, args, dcd.getUser(), dcd.getPassword(),
							true);
					if (importDialog.isOk) {
						importDialog.fillCLIArgs(args);
						ResultConsumer consumer = new ResultConsumer() {
							@Override
							public void consume(boolean result, Throwable t) {
								UIUtil.showException(DataBrowser.this, "Error", t, session);
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
						UIUtil.runJailer(this, args, false, true, false, null, dcd.getUser(), dcd.getPassword(), null,
								null, false, false, true, false, true, consumer, null, false, executionContext);
					}
				}
			}
		} catch (Exception e) {
			UIUtil.showException(this, "Error", e, session);
		}
	}// GEN-LAST:event_dataImportActionPerformed

	private void newBrowserjMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_newBrowserjMenuItemActionPerformed
		try {
			UIUtil.setWaitCursor(this);
			createFrame("B", new ExecutionContext(executionContext));
		} catch (Throwable t) {
			UIUtil.showException(this, "Error", t);
		} finally {
			UIUtil.resetWaitCursor(this);
		}
	}// GEN-LAST:event_newBrowserjMenuItemActionPerformed

	private void openTableButtonActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_openTableButtonActionPerformed
		if (tablesComboBox.getSelectedItem() != null) {
			String tableName = tablesComboBox.getSelectedItem().toString();
			desktop.addTableBrowser(null, null, datamodel.get().getTableByDisplayName(tableName), null, "", null, true);
			switchToDesktop();
		}
	}// GEN-LAST:event_openTableButtonActionPerformed

	private boolean ignoreTabChangeEvent = false;

	private void workbenchTabbedPaneStateChanged(javax.swing.event.ChangeEvent evt) {// GEN-FIRST:event_workbenchTabbedPaneStateChanged
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
				for (SQLConsole sqlConsole : sqlConsoles) {
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
	}// GEN-LAST:event_workbenchTabbedPaneStateChanged

	private void showDataModelMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_showDataModelMenuItemActionPerformed
		metaDataDetailsPanel.tabbedPane.setSelectedComponent(dataModelPanel);
	}// GEN-LAST:event_showDataModelMenuItemActionPerformed

	private void consistencyCheckMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_consistencyCheckMenuItemActionPerformed
		new ConstraintChecker(this, datamodel.get(), true, session) {
			@Override
			protected void openTableBrowser(Table source, String where) {
				workbenchTabbedPane.setSelectedComponent(desktopSplitPane);
				desktop.addTableBrowser(null, null, source, null,
						UIUtil.toSingleLineSQL(new BasicFormatterImpl().format(where)), null, true);
			}
		};
	}// GEN-LAST:event_consistencyCheckMenuItemActionPerformed

	private void reconnectMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_reconnectMenuItemActionPerformed
		synchronized (this) {
			if (lastConnectionInfo != null) {
				dbConnectionDialog.select(lastConnectionInfo);
			}
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
	}// GEN-LAST:event_thumbnailLayoutRadioButtonMenuItemActionPerformed

	private void jMenuItem3ActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_jMenuItem1ActionPerformed
		if (searchButton != null && searchButton.isShowing() && searchButton.isEnabled()) {
			searchButton.doClick(1);
		} else {
			openNewTableBrowser(false);
		}
	}// GEN-LAST:event_jMenuItem1ActionPerformed

	private void closeAllMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_cloaseAllMenuItemActionPerformed
		desktop.closeAll();
	}// GEN-LAST:event_cloaseAllMenuItemActionPerformed

	private void schemaMappingMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_schemaMappingMenuItemActionPerformed
		desktop.openSchemaMappingDialog(false);
	}// GEN-LAST:event_schemaMappingMenuItemActionPerformed

	private void jMenuItem4ActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_jMenuItem4ActionPerformed
		try {
			BrowserLauncher.openURL(new URI("https://wisser.github.io/Jailer/data-browsing.html"), this);
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
					JPopupMenu popup = rowBrowser.browserContentPane.createPopupMenu(null, -1, 0, 0, false, false,
							false);
					if (popup != null) {
						JPopupMenu popup2 = rowBrowser.browserContentPane.createSqlPopupMenu(-1, 0, 0, true,
								navigationTreeScrollPane);
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
		arrangeLayout(true,
				Desktop.noArrangeLayoutOnNewTableBrowserWithAnchor ? null : anchorManager.getNewestBrowser(), true);
		anchorManager.setNewestBrowser(null);
	}// GEN-LAST:event_layoutMenuItemActionPerformed

	public void arrangeLayout(boolean scrollToCenter) {
		arrangeLayout(scrollToCenter,
				Desktop.noArrangeLayoutOnNewTableBrowserWithAnchor ? null : anchorManager.getNewestBrowser());
		anchorManager.setNewestBrowser(null);
	}

	public void arrangeLayout(boolean scrollToCenter, RowBrowser anchor) {
		arrangeLayout(scrollToCenter, anchor, false);
	}

	public void arrangeLayout(boolean scrollToCenter, RowBrowser anchor, boolean force) {
		if (!force && !autoLayoutMenuItem.isSelected()) {
			return;
		}
		if (anchor == null) {
			desktop.checkHAlignedPath();
		}
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
		desktop.onMouseWheelMoved(evt.getX(), evt.getY(), evt.getWheelRotation(), evt.getComponent(), currentTime, evt);
		desktop.onMouseWheelMoved(evt, jScrollPane1, currentTime);
	}// GEN-LAST:event_jScrollPane1MouseWheelMoved

	private void openNewTableBrowser(boolean offerAlternatives) {
		new NewTableBrowser(this, datamodel.get(), offerAlternatives) {
			@Override
			void openTableBrowser(String tableName) {
				Table tableByDisplayName = datamodel.get().getTableByDisplayName(tableName);
				if (tableByDisplayName != null) {
					desktop.addTableBrowser(null, null, tableByDisplayName, null, "", null, true);
					switchToDesktop();
				}
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
		about.setSize(Math.min(about.getWidth(), 600), about.getHeight());
		about.setLocation(getLocation().x + (getSize().width - about.getSize().width) / 2,
				getLocation().y + (getSize().height - about.getSize().height) / 2);
		about.setVisible(true);
	}// GEN-LAST:event_jMenuItem4ActionPerformed

	private void analyseMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_analyseMenuItemActionPerformed
		updateDataModel();
	}// GEN-LAST:event_analyseMenuItemActionPerformed

	private void dataModelEditorjMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_dataModelEditorjMenuItemActionPerformed
		openDataModelEditor(false);
	}// GEN-LAST:event_dataModelEditorjMenuItemActionPerformed

	private boolean zoomWithMouseWheel;
	private String zoomInMenuText = null;
	private String zoomOutMenuText = null;

	public boolean isZoomWithMouseWheel() {
		return zoomWithMouseWheel;
	}

	public void setZoomWithMouseWheel(boolean zoomWithMouseWheel) {
		this.zoomWithMouseWheel = zoomWithMouseWheel;

		if (zoomInMenuText == null) {
			zoomInMenuText = zoomInMenuItem.getText();
		}
		if (zoomOutMenuText == null) {
			zoomOutMenuText = zoomOutMenuItem.getText();
		}

		String mask = this.zoomWithMouseWheel ? ""
				: ((UIUtil.isMacOS() ? "Command" : InputEvent.getModifiersExText(InputEvent.CTRL_DOWN_MASK)) + "+");
		zoomInMenuItem.setText(zoomInMenuText.replace("%", mask));
		zoomOutMenuItem.setText(zoomOutMenuText.replace("%", mask));
	}

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

		// turn off logging for prefuse library
		try {
//			LoggerFactory.getLogger("prefuse").setLevel(Level.OFF);
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
					UIUtil.readAndInitPLAF();
					DataModelManagerDialog.start(null, true, new ExecutionContext());
				} catch (Exception e) {
					UIUtil.showException(null, "Error", e);
				}
			}
		});
	}

	private static DataBrowser openNewDataBrowser(DataModel datamodel, DbConnectionDialog dbConnectionDialog,
			boolean maximize, ExecutionContext executionContext, DataBrowser theDataBrowser) throws Exception {
		ExecutionContext origExecContext = executionContext;
		if (executionContext != null) {
			executionContext = new ExecutionContext(executionContext);
		}
		DataBrowser dataBrowser = theDataBrowser != null ? theDataBrowser
				: new DataBrowser(datamodel, null, "", null,
						ExecutionContext.getSchemaMapping(CommandLineInstance.getInstance().rawschemamapping),
						executionContext);
		dataBrowser.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
		dataBrowser.setVisible(true);
		dataBrowser.setExtendedState(Frame.MAXIMIZED_BOTH);

		if (dbConnectionDialog == null) {
			dbConnectionDialog = new DbConnectionDialog(dataBrowser, DataBrowserContext.getAppName(), null, null,
					executionContext);
		} else {
			dbConnectionDialog = new DbConnectionDialog(dataBrowser, dbConnectionDialog,
					DataBrowserContext.getAppName(), null, executionContext);
		}
		dbConnectionDialog.autoConnect();
		if (origExecContext != null) {
			origExecContext
					.setCurrentConnectionAlias(dbConnectionDialog.getExecutionContext().getCurrentConnectionAlias());
		}
		if (dbConnectionDialog.isConnected || dbConnectionDialog.connect(DataBrowserContext.getAppName(true))) {
			if (origExecContext != null) {
				origExecContext.setCurrentConnectionAlias(
						dbConnectionDialog.getExecutionContext().getCurrentConnectionAlias());
			}
			try {
				dataBrowser.setConnection(dbConnectionDialog);
				if (dataBrowser.desktop.session == null) {
					dataBrowser.dispose();
					return dataBrowser;
				}
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
				if (dataBrowser.desktop.session == null) {
					dataBrowser.dispose();
					return dataBrowser;
				}
			} else {
				dataBrowser.dbConnectionDialog = dbConnectionDialog;
				dataBrowser.updateModelNavigation();
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
					dataBrowser.autoLayoutMenuItem.setSelected(true);
				} else if (bmFile != null) {
					dataBrowser.desktop.restoreSession(null, bmFile);
					BookmarksPanel.setLastUsedBookmark(bmFile.getName(), dataBrowser.executionContext);
					bmFile.setLastModified(System.currentTimeMillis());
					new BookmarksPanel(dataBrowser, dataBrowser.bookmarkMenu, dataBrowser.desktop,
							dataBrowser.executionContext).updateBookmarksMenu();
				}
			}
		});
		return dataBrowser;
	}

	private static void createFrame(String module, ExecutionContext executionContext) {
		DataModelManagerDialog.start(module, false, executionContext);
	}

	/**
	 * @return
	 */
	public static DataModelManagerDialog createDMMDialog(ExecutionContext executionContext) {
		DataModelManagerDialog dataModelManagerDialog = new DataModelManagerDialog(
				DataBrowserContext.getAppName(true) + " - Relational Data Browser", false, "B", executionContext) {
			@Override
			protected void onSelect(final DbConnectionDialog connectionDialog,
					final ExecutionContext executionContext) {
				try {
					final DataModel datamodel;
					Map<String, String> schemaMapping = ExecutionContext
							.getSchemaMapping(CommandLineInstance.getInstance().rawschemamapping);
					datamodel = new DataModel(null, null, schemaMapping, null, new PrimaryKeyFactory(executionContext),
							executionContext, true, null);
					final DataBrowser databrowser = new DataBrowser(datamodel, null, "", null, schemaMapping,
							executionContext);
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
		return dataModelManagerDialog;
	}

	/**
	 * Opens the data model editor.
	 */
	private void openDataModelEditor(boolean merge) {
		try {
			UIUtil.setWaitCursor(this);
			String modelname = datamodel == null || datamodel.get() == null ? DataModel.DEFAULT_NAME
					: datamodel.get().getName();
			DataModelEditor dataModelEditor = new DataModelEditor(this, merge, false, null, null, null, modelname, null,
					dbConnectionDialog, executionContext);
			dataModelEditor.setVisible(true);
			desktop.reloadDataModel(desktop.schemaMapping);
			dataModelViewFrame = null;
			updateDataModelView(null);
			toFront();
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
			JDBCMetaDataBasedModelElementFinder.privilegedSessionProvider = new PrivilegedSessionProviderDialog.Provider(
					this);

			List<String> args = new ArrayList<String>();
			args.add("build-model-wo-merge");
			dbConnectionDialog.addDbArgs(args);

			AnalyseOptionsDialog analyseOptionsDialog = new AnalyseOptionsDialog(this,
					datamodel == null ? null : datamodel.get(), executionContext);
			analyseOptionsDialog.setInitiallyWithViews(withViews);
			analyseOptionsDialog.setInitiallyWithSynonyms(withSynonyms);
			boolean[] isDefaultSchema = new boolean[1];
			if (analyseOptionsDialog.edit(dbConnectionDialog,
					schemaName == null ? null : Quoting.staticUnquote(schemaName), isDefaultSchema,
					dbConnectionDialog.currentConnection.user)) {
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
				if (UIUtil.runJailer(this, args, false, true, true, null, dbConnectionDialog.getUser(),
						dbConnectionDialog.getPassword(), null, null, false, true, false, null, executionContext)) {
					ModelBuilder.assocFilter = null;
					String modelname = datamodel == null || datamodel.get() == null ? DataModel.DEFAULT_NAME
							: datamodel.get().getName();
					DataModelEditor dataModelEditor = new DataModelEditor(this, true, analyseOptionsDialog.isRemoving(),
							null, analyseOptionsDialog.getTableLineFilter(),
							analyseOptionsDialog.getAssociationLineFilter(), modelname,
							schema == null ? dbConnectionDialog.getName() : schema, dbConnectionDialog,
							executionContext);
					if (dataModelEditor.dataModelHasChanged()) {
						dataModelEditor.setVisible(true);
					}
					desktop.reloadDataModel(desktop.schemaMapping);
					dataModelViewFrame = null;
					updateDataModelView(null);
					toFront();
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
    private javax.swing.JMenu animationStepTimeMenu;
    private javax.swing.JLabel associatedWith;
    javax.swing.JCheckBoxMenuItem autoLayoutMenuItem;
    private javax.swing.JMenu bookmarkMenu;
    private javax.swing.JPanel borderBrowserPanel;
    private javax.swing.JPanel borderBrowserTabPane;
    private javax.swing.ButtonGroup buttonGroupStepTime;
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
    private javax.swing.JMenuItem exportMenuItem;
    private javax.swing.JMenuItem generateDDLMenuItem;
    private javax.swing.JMenuItem goBackItem;
    private javax.swing.JMenuItem goForwardItem;
    private javax.swing.JLabel hasDependent;
    private javax.swing.JMenuItem helpForum;
    private javax.swing.JMenu helpMenu;
    private javax.swing.JPanel hiddenPanel;
    private javax.swing.JLabel ignored;
    private javax.swing.JMenuItem importMenuItem;
    private javax.swing.JPanel initialHGPanel;
    private javax.swing.JButton jButton1;
    private javax.swing.JInternalFrame jInternalFrame1;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel10;
    private javax.swing.JLabel jLabel11;
    private javax.swing.JLabel jLabel12;
    private javax.swing.JLabel jLabel13;
    private javax.swing.JLabel jLabel14;
    private javax.swing.JLabel jLabel19;
    private javax.swing.JLabel jLabel2;
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
    private javax.swing.JPanel jPanel4;
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
    private javax.swing.JPopupMenu.Separator jSeparator16;
    private javax.swing.JToolBar.Separator jSeparator17;
    private javax.swing.JToolBar.Separator jSeparator18;
    private javax.swing.JSeparator jSeparator19;
    private javax.swing.JPopupMenu.Separator jSeparator2;
    private javax.swing.JPopupMenu.Separator jSeparator20;
    private javax.swing.JToolBar.Separator jSeparator21;
    private javax.swing.JToolBar.Separator jSeparator22;
    private javax.swing.JPopupMenu.Separator jSeparator23;
    private javax.swing.JToolBar.Separator jSeparator24;
    private javax.swing.JPopupMenu.Separator jSeparator25;
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
    private javax.swing.JToolBar jToolBar1;
    private javax.swing.JToolBar jToolBar2;
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
    private javax.swing.JToggleButton modelNavigationButtonV;
    private javax.swing.JButton modelNavigationConnectButtonV;
    private javax.swing.JPanel modelNavigationGapPanel;
    private javax.swing.JPanel modelNavigationPanel;
    private javax.swing.JScrollPane modelNavigationScrollPane;
    private javax.swing.JPanel modelNavigationSplitSizerPanel;
    private javax.swing.JTree modelNavigationTree;
    private javax.swing.JLabel modelPath;
    private javax.swing.JLayeredPane navTreeLayeredPane;
    private javax.swing.JPanel navigationPanel;
    private javax.swing.JTree navigationTree;
    private javax.swing.JScrollPane navigationTreeScrollPane;
    private javax.swing.JMenuItem newBrowserjMenuItem;
    private javax.swing.JMenuItem newEmptyWindowMenuItem;
    private javax.swing.JMenuItem newWindowMenuItem;
    private javax.swing.JButton openTableButton;
    private javax.swing.JPanel outLinePanel;
    private javax.swing.JMenu plafMenu;
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
    private javax.swing.JToggleButton searchBarToggleButton;
    private javax.swing.JPanel searchPanelContainer;
    private javax.swing.JPanel searchPanelSplitSizerPanel;
    private javax.swing.JCheckBoxMenuItem showDataModelMenuItem;
    private javax.swing.JRadioButtonMenuItem smallLayoutRadioButtonMenuItem;
    private javax.swing.JRadioButtonMenuItem steptime10;
    private javax.swing.JRadioButtonMenuItem steptime100;
    private javax.swing.JRadioButtonMenuItem steptime20;
    private javax.swing.JRadioButtonMenuItem steptime200;
    private javax.swing.JRadioButtonMenuItem steptime30;
    private javax.swing.JRadioButtonMenuItem steptime300;
    private javax.swing.JRadioButtonMenuItem steptime50;
    private javax.swing.JRadioButtonMenuItem steptime75;
    private javax.swing.JMenuItem storeSessionItem;
    private javax.swing.JTabbedPane tableTreesTabbedPane;
    private javax.swing.JPanel tablesCardPanel;
    private javax.swing.JPanel tablesPanel;
    private javax.swing.JButton tbBackButton;
    private javax.swing.JButton tbClearButton;
    private javax.swing.JButton tbForewardButton;
    private javax.swing.JToggleButton tbZoom0Button;
    private javax.swing.JToggleButton tbZoom1Button;
    private javax.swing.JToggleButton tbZoom2Button;
    private javax.swing.JToggleButton tbZoom3Button;
    private javax.swing.JToggleButton tbZoom4Button;
    private javax.swing.JButton tbZoomInButton;
    private javax.swing.JButton tbZoomOutButton;
    private javax.swing.JButton tbreloadButton;
    private javax.swing.JRadioButtonMenuItem thumbnailLayoutRadioButtonMenuItem;
    private javax.swing.JRadioButtonMenuItem tinyLayoutRadioButtonMenuItem;
    private javax.swing.JLabel titleLabel;
    private javax.swing.JPanel topLayerPanel;
    private javax.swing.JLabel updateInfoLabel;
    private javax.swing.JPanel updateInfoPanel;
    private javax.swing.JMenu view;
    private javax.swing.JTabbedPane workbenchTabbedPane;
    private javax.swing.JMenuItem zoomInMenuItem;
    private javax.swing.JMenuItem zoomOutMenuItem;
    private javax.swing.JCheckBoxMenuItem zoomWithMouseWheelMenuItem;
    // End of variables declaration//GEN-END:variables

	private JToggleButton searchButton;
	private JToggleButton tbAddTableButton;

	/**
	 * Sets Look&Feel.
	 *
	 * @param plaf the l&f
	 */
	private void setPLAF(PLAF plaf) {
		try {
			plaf.install();
			SwingUtilities.updateComponentTreeUI(this);
			Colors.init();
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
			forceRepaint();
			if (datamodel.get().getTables().isEmpty()) {
				switch (JOptionPane.showOptionDialog(this,
						"Data model \""
								+ DataModelManager.getModelDetails(
										DataModelManager.getCurrentModelSubfolder(executionContext), executionContext).a
								+ "\" is empty.",
						DataBrowserContext.getAppName(true), JOptionPane.YES_NO_OPTION, JOptionPane.INFORMATION_MESSAGE,
						null, new Object[] { "Analyze Database", "Data Model Editor", "SQL Console" }, null)) {
				case 0:
					updateDataModel();
					break;
				case 1:
					openDataModelEditor(false);
					forceRepaint();
					break;
				case 2:
					workbenchTabbedPane.setSelectedComponent(getCurrentSQLConsole());
					break;
				}
			}
		} catch (Exception e) {
			UIUtil.showException(this, "Error", e, session);
		}
	}

	private void forceRepaint() {
		UIUtil.invokeLater(12, () -> {
			jSplitPane1.setDividerLocation(jSplitPane1.getDividerLocation() + 1);
			UIUtil.invokeLater(() -> {
				jSplitPane1.setDividerLocation(jSplitPane1.getDividerLocation() - 1);
			});
		});
	}

	private void updateIFramesBar() {
		updateNavigationTree();
		updateBorderBrowser(null);
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
			return rowBrowser.getTitle();
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

	private void updateBorderBrowser(JPanel borderBrowserTabPaneContainer) {
		if (disableBorderBrowserUpdates
				|| metaDataDetailsPanel.tabbedPane.getSelectedComponent() != borderBrowserTabPaneContainer) {
			return;
		}
		try {
			UIUtil.setWaitCursor(this);
			if (borderBrowserTabPaneContainer != null) {
				borderBrowserTabPaneContainer.add(borderBrowserTabPane);
			}
			Collection<AssociationModel> model = new ArrayList<AssociationModel>();
			if (desktop != null) {
				titleLabel.setText(" Related Rows");
				List<RowBrowser> allChildren = new ArrayList<RowBrowser>();
				for (RowBrowser rb : desktop.getBrowsers()) {
					if (rb.internalFrame == desktop.getSelectedFrame() && !rb.isHidden()) {
						allChildren.add(rb);
						allChildren.addAll(collectChildren(rb));
						titleLabel.setText(" Related Rows of Subtree " + rb.getTitle());
						break;
					}
				}
				for (RowBrowser rb : allChildren) {
					if (rb.browserContentPane.table != null) {
						Set<Association> associations = new HashSet<Association>(
								rb.browserContentPane.table.associations);
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
						dataModelViewFrame = ExtractionModelFrame.createFrame(null, false, false, null,
								executionContext);
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
							toSelect = datamodel.get().getTables().isEmpty() ? null
									: datamodel.get().getTables().iterator().next();
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
				desktop.addTableBrowser(associationModel.getRowBrowser(), associationModel.getRowBrowser(),
						associationModel.getAssociation().destination, associationModel.getAssociation(), "", null,
						true);
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
			updateBorderBrowser(null);
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
			this.title = rowBrowser.internalFrame.getTitle();
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
		updateRowCounters();
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
		Table table = rowBrowser != null && rowBrowser.browserContentPane != null ? rowBrowser.browserContentPane.table
				: null;
		searchPanelContainer.removeAll();
		searchPanelContainer.setVisible(table != null && searchBarToggleButton.isSelected());
		searchPanelSplitSizerPanel.setVisible(searchPanelContainer.isVisible());
		searchBarToggleButton.setEnabled(rowBrowser != null && SessionForUI.isWCEditorSupported(session));
		if (table != null) {
			SearchBarRSyntaxTextArea searchBarEditor = new SearchBarRSyntaxTextArea();
			searchBarEditor.setMarkOccurrences(false);

			BrowserContentCellEditor cellEditor = rowBrowser.browserContentPane.browserContentCellEditor;
			whereConditionEditorPanel = new WhereConditionEditorPanelForDataBrowser(this, datamodel.get(), table,
					cellEditor, rowBrowser.browserContentPane.sortColumnsCheckBox.isSelected(),
					whereConditionEditorPanel, searchBarEditor, whereConditionEditorCloseButton, false, -1, session,
					executionContext, rowBrowser);
			searchPanelContainer.add(whereConditionEditorPanel);
			searchBarEditor.whereConditionEditorPanel = whereConditionEditorPanel;
			whereConditionEditorSubject = rowBrowser.browserContentPane;
			whereConditionEditorPanel.parseCondition(rowBrowser.browserContentPane.getAndConditionText());
			whereConditionEditorPanel
					.setExtJoinsUpdater(() -> addWCEditorExtRows(whereConditionEditorPanel, rowBrowser));
		}

		if (rowBrowser != null && rowBrowser.browserContentPane != null) {
			if (rowBrowser.browserContentPane.table instanceof SqlStatementTable) {
				updateClosureBrowser(null);
				return;
			}
		}
		closureView = new DBClosureView(this, () -> {
			if (session != null) {
				return getMetaDataSource(session);
			}
			return null;

		}) {
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
				for (RowBrowser cb : rowBrowser.browserContentPane.getChildBrowsers()) {
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
				RowBrowser rootRB = null;
				for (RowBrowser rb : desktop.getBrowsers()) {
					if (rb.internalFrame == desktop.getSelectedFrame() && !rb.isHidden()) {
						rootRB = rb;
					}
				}
				if (rootRB != null && (rootRB.browserContentPane.table == null
						|| !rootRB.browserContentPane.table.equals(path.get(path.size() - 1)))) {
					rootRB = null;
				}
				int i;
				i = path.size() - 1;
				Map<Table, RowBrowser> visTables = getVisibleTables();

				RowBrowser nextRb = null;

				if (rootRB != null) {
					try {
						while (i >= 0) {
							RowBrowser rb = null;
							Table tableAtI = path.get(i);
							for (RowBrowser r : desktop.getBrowsers()) {
								if (tableAtI.equals(r.browserContentPane.table)) {
									boolean ok = true;
									RowBrowser b = r;
									for (int j = i; j < path.size(); ++j) {
										if (b.parent == null && j < path.size() - 1) {
											ok = false;
											break;
										}
										Table tableAtJ = path.get(j);
										if (!tableAtJ.equals(b.browserContentPane.table)) {
											ok = false;
											break;
										}
										if (j == path.size() - 1 && b != rootRB) {
											ok = false;
											break;
										}
										b = b.parent;
									}
									if (ok) {
										rb = r;
										break;
									}
								}
							}
							if (rb == null) {
								++i;
								break;
							}
							nextRb = rb;
							--i;
						}
					} catch (Throwable t) {
						rootRB = null;
						LogUtil.warn(t);
					}
				}

				if (rootRB == null) {
					nextRb = null;
					Table predTable = null;
					while (i >= 0) {
						Table tableAtI = path.get(i);
						RowBrowser rb = visTables.get(tableAtI);
						if (rb == null) {
							++i;
							break;
						}
						if (predTable != null
								&& !(rb.parent != null && predTable.equals(rb.parent.browserContentPane.table))) {
							++i;
							break;
						}
						--i;
						predTable = tableAtI;
					}
				}

				Association[] associations = openAssociationPathPanel(path.subList(0, i + 1));
				if (associations != null) {
					try {
						Desktop.noArrangeLayoutOnNewTableBrowser = true;
						desktop.getiFrameStateChangeRenderer().startAtomic();
						disableBorderBrowserUpdates = true;
						suppressUpdateClosureBrowser = true;
						while (i > 0) {
							Table table = path.get(i);
							RowBrowser rb;
							if (nextRb != null && nextRb.association != null
									&& nextRb.association.destination.equals(table)) {
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
				try {
					final AssociationPathPanel assocPanel = new AssociationPathPanel(getDataModel(), path,
							dependsOn.getForeground(), hasDependent.getForeground(), associatedWith.getForeground(),
							ignored.getForeground());
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
					d.setLocation(DataBrowser.this.getX() + (DataBrowser.this.getWidth() - d.getWidth()) / 2,
							Math.max(0, DataBrowser.this.getY() + (DataBrowser.this.getHeight() - d.getHeight()) / 2));
					UIUtil.fit(d);
					assocPanel.okButton.grabFocus();
					d.setVisible(true);
					if (assocPanel.ok) {
						return assocPanel.selectedAssociations;
					}
				} catch (Exception e) {
					LogUtil.warn(e);
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

	private WhereConditionEditorPanelForDataBrowser popUpWhereConditionEditorPanel;

	private boolean neverOpened = true;
	private JDialog conditionEditorDialog = null;

	protected void openConditionEditor(BrowserContentPane browserContentPane, Point location, int column,
			Runnable onClose) {
		if (conditionEditorDialog != null) {
			if (conditionEditorDialog.isVisible()) {
				conditionEditorDialog.setVisible(false);
				conditionEditorDialog.dispose();
			}
		}
		JDialog dialog = new JDialog(this, "");
		conditionEditorDialog = dialog;
		UIUtil.invokeLater(() -> {
			final Runnable close = () -> {
				dialog.setVisible(false);
				dialog.dispose();
			};
			dialog.addWindowListener(new WindowAdapter() {
				@Override
				public void windowClosed(WindowEvent e) {
					if (popUpWhereConditionEditorPanel != null) {
						popUpWhereConditionEditorPanel.close();
					}
					if (onClose != null) {
						onClose.run();
					}
				}
			});
			RowBrowser rowBrowser = browserContentPane.getRowBrowser();
			Table table = rowBrowser != null && rowBrowser.browserContentPane != null
					? rowBrowser.browserContentPane.table
					: null;
			if (table != null) {
				SearchBarRSyntaxTextArea searchBarEditor = new SearchBarRSyntaxTextArea();
				SearchBarRSyntaxTextArea popUpSearchBarEditor = new SearchBarRSyntaxTextArea();

				BrowserContentCellEditor cellEditor = rowBrowser.browserContentPane.browserContentCellEditor;
				popUpWhereConditionEditorPanel = new WhereConditionEditorPanelForDataBrowser(dialog, datamodel.get(),
						table, cellEditor, rowBrowser.browserContentPane.sortColumnsCheckBox.isSelected(),
						popUpWhereConditionEditorPanel, popUpSearchBarEditor, null, true, column, session,
						executionContext, rowBrowser) {
					@Override
					protected void onEscape() {
						close.run();
					}
				};
				/* if (rowBrowser != null && rowBrowser.browserContentPane != null) */ {
					popUpSearchBarEditor.whereConditionEditorPanel = popUpWhereConditionEditorPanel;
					popUpWhereConditionEditorPanel.parseCondition(rowBrowser.browserContentPane.getAndConditionText());
					addWCEditorExtRows(popUpWhereConditionEditorPanel, rowBrowser);
				}
				dialog.setModal(false);
				dialog.setUndecorated(true);
				openingTime = System.currentTimeMillis();
				UIUtil.invokeLater(6, () -> openingTime = System.currentTimeMillis());
				dialog.addWindowFocusListener(new WindowFocusListener() {
					@Override
					public void windowLostFocus(WindowEvent e) {
						if (!(e.getOppositeWindow() instanceof StringSearchDialog)) {
							if (System.currentTimeMillis() < openingTime + 200) {
								dialog.requestFocus();
								return;
							}
							dialog.setSize(1, 1);
							delayPopupAction(v -> close.run());
						}
					}

					@Override
					public void windowGainedFocus(WindowEvent e) {
					}
				});

				int x = location.x;
				int y = location.y;

				if (column >= 0) {
					x += 32;
				}

				dialog.getContentPane().add(popUpWhereConditionEditorPanel);

				dialog.pack();
				double mh = column >= 0 ? 220 : 280;
				int height = Math.max(dialog.getHeight(), (int) mh);
				dialog.setLocation(x, y);
				int minWidth = 600;
				int wid = Math.max(minWidth, dialog.getWidth());
				int maxX = getX() + getWidth() - wid - 8;
				dialog.setSize(wid, Math.min(height, 600));
				dialog.setLocation(Math.max(0, Math.min(maxX, dialog.getX())), dialog.getY());
				int maxY = getY() + getHeight() - dialog.getHeight() - 8;
				if (maxY < dialog.getY()) {
					int deltaH = Math.min(dialog.getY() - maxY, (int) (0.30 * dialog.getHeight()));
					maxY += deltaH;
					dialog.setSize(dialog.getWidth(), dialog.getHeight() - deltaH);
					dialog.setLocation(dialog.getX(), Math.max(0, maxY));
				}
				popUpWhereConditionEditorPanel.prepareStringSearchPanelOfInitialColumn(dialog);
				UIUtil.invokeLater(4, () -> {
					popUpWhereConditionEditorPanel.openStringSearchPanelOfInitialColumn(dialog);
				});
				UIUtil.invokeLater(() -> dialog.requestFocus());
				dialog.setVisible(true);
			}
		});
		if (neverOpened) {
			neverOpened = false;
			UIUtil.invokeLater(8, () -> {
				Timer timer = new Timer(100, e -> {
					if (!dialog.isVisible()) {
						openConditionEditor(browserContentPane, location, column, onClose);
					}
				});
				timer.setRepeats(false);
				timer.start();
			});
		}
	}

	void addWCEditorExtRows(WhereConditionEditorPanel wcEditor, RowBrowser rowBrowser) {
		List<Row> parentRows = rowBrowser.browserContentPane.parentRows;
		if (rowBrowser.browserContentPane.association != null && parentRows != null && !parentRows.isEmpty()) {
			wcEditor.setDontCount(rowBrowser.browserContentPane.selectDistinctCheckBox.isVisible()
					&& rowBrowser.browserContentPane.selectDistinctCheckBox.isEnabled()
					&& rowBrowser.browserContentPane.selectDistinctCheckBox.isSelected());

			String cond;
			String unrestrictedJoinCondition = rowBrowser.browserContentPane.association.getUnrestrictedJoinCondition();
			if (!rowBrowser.browserContentPane.association.reversed) {
				cond = SqlUtil.reversRestrictionCondition(unrestrictedJoinCondition);
			} else {
				cond = unrestrictedJoinCondition;
			}
			ArrayList<Row> parentRowsBatch = new ArrayList<Row>();
			Runnable addExtRow = () -> {
				String bRows = parentRowsBatch.stream().map(r -> "(" + r.rowId + ")")
						.collect(Collectors.joining(" or "));
				String extJoin = "join " + rowBrowser.browserContentPane.association.source.getName() + " B on (" + cond
						+ ") and (" + bRows + ")";
				wcEditor.addExtJoin(extJoin);
			};
			parentRows.forEach(row -> {
				parentRowsBatch.add(row);
				if (parentRowsBatch.size() >= 50) {
					addExtRow.run();
					parentRowsBatch.clear();
				}
			});
			if (parentRowsBatch.size() > 0) {
				addExtRow.run();
				parentRowsBatch.clear();
			}
		}
	}

	private static final int MINIMUM_POPUP_RETENSION = 1000;
	private long openingTime;

	private void delayPopupAction(ActionListener action) {
		if (openingTime > 0) {
			long rest = openingTime + MINIMUM_POPUP_RETENSION - System.currentTimeMillis();
			if (rest > 0) {
				Timer timer = new Timer((int) rest, action);
				timer.setRepeats(false);
				timer.start();
			} else {
				action.actionPerformed(null);
			}
		}
	}

	private MetaDataSource getMetaDataSource(Session session) {
		return (MetaDataSource) session.getSessionProperty(DataBrowser.class, "MetaDataSource");
	}

	private MetaDataPanel metaDataPanel;
	private Runnable createMetaDataPanel;

	private static BlockingQueue<Runnable> onsQueue = new LinkedBlockingQueue<Runnable>();
	static {
		Thread thread = new Thread(new Runnable() {
			@Override
			public void run() {
				for (;;) {
					try {
						onsQueue.take().run();
					} catch (Throwable t) {
						LogUtil.warn(t);
					}
				}
			}
		});
		thread.setDaemon(true);
		thread.start();
	}

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

			if (!SessionForUI.isWCEditorSupported(newSession)) {
				searchBarToggleButton.setEnabled(false);
				searchBarToggleButton.setSelected(false);
				searchBarToggleButton.setToolTipText(
						"<html>Open Search Sidebar<br>" + "<b>Condition editor not supported for DBMS \""
								+ (newSession.dbms == null ? null : newSession.dbms.getId()) + "\"</b></html>");
			}

			JDBCMetaDataBasedModelElementFinder.resetCaches(newSession);

			tablesPanel.removeAll();
			metaDataPanel = (MetaDataPanel) session.getSessionProperty(getClass(), "metaDataPanel");
			MetaDataSource metaDataSource;
			try {
				metaDataSource = getMetaDataSource(newSession);
				if (metaDataSource == null
						|| Boolean.TRUE.equals(session.getSessionProperty(DataBrowser.class, "removeMetaDataSource"))) {
					metaDataSource = new MetaDataSource(newSession, datamodel.get(), alias, executionContext);
					final MetaDataSource finalMetaDataSource = metaDataSource;
					onsQueue.add(new Runnable() {
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
												if (finalMetaDataSource.getSession().isDown()) {
													return;
												}
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
													if (finalMetaDataSource.getSession().isDown()) {
														return;
													}
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
					for (SQLConsole sqlConsole : sqlConsoles) {
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
						metaDataPanel = new MetaDataPanel(DataBrowser.this, fMetaDataSource, metaDataDetailsPanel,
								datamodel.get(), executionContext) {
							@Override
							public void refresh() {
								super.refresh();
								UIUtil.invokeLater(() -> updateNavigationTree());
							}

							@Override
							protected void open(Table table) {
								if (!selectNavTreeNode(navigationTree.getModel().getRoot(), table)) {
									if (workbenchTabbedPane.getSelectedComponent() != getCurrentSQLConsole()) {
										desktop.addTableBrowser(null, null, table, null, "", null, true);
									}
								}
								try {
									String sql;
									Quoting quoting = Quoting.getQuoting(session);
									MetaDataSource metaDataSource = getMetaDataSource(session);
									MDTable mdTable = metaDataSource.isInitialized() ? metaDataSource.toMDTable(table)
											: null;
									String tableName;
									String schemaName;
									if (mdTable != null) {
										tableName = mdTable.getName();
										schemaName = mdTable.getSchema().isDefaultSchema ? ""
												: mdTable.getSchema().getName();
									} else {
										tableName = table.getUnqualifiedName();
										schemaName = table.getSchema("");
									}
									sql = "Select * From " + (schemaName == null || schemaName.length() == 0 ? ""
											: quoting.quote(schemaName) + ".") + quoting.quote(tableName);
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
											navigationTree.getSelectionModel().setSelectionPath(
													new TreePath(((DefaultMutableTreeNode) root).getPath()));
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
											navigationTree.getSelectionModel().setSelectionPath(
													new TreePath(((DefaultMutableTreeNode) root).getPath()));
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
								String schemaName = mdTable.getSchema().isDefaultSchema ? null
										: mdTable.getSchema().getName();
								String tableName = mdTable.getName();
								try {
									Quoting quoting = Quoting.getQuoting(session);
									String sql = "Select * From "
											+ (schemaName == null ? "" : quoting.quote(schemaName) + ".")
											+ quoting.quote(tableName);
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
									UIUtil.invokeLater(8,
											() -> getCurrentSQLConsole().appendStatement(script, execute));
								} catch (Throwable e) {
									UIUtil.showException(this, "Error", e);
								}
							}

							@Override
							protected void onTableSelect(MDTable mdTable) {
								metaDataDetailsPanel.showMetaDataDetails(mdTable,
										getMetaDataSource(session).toTable(mdTable), null, false, datamodel.get());
							}

							@Override
							protected void onRowSelect(Table table, Row row) {
								metaDataDetailsPanel.showMetaDataDetails(null, table, row, true, datamodel.get());
							}

							@Override
							protected void onMDOtherSelect(MDGeneric mdOther, ExecutionContext executionContext) {
								metaDataDetailsPanel.showMetaDataDetails(mdOther, executionContext);
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
								((CardLayout) tablesCardPanel.getLayout()).show(tablesCardPanel,
										set ? "loading" : "tables");
							}

							@Override
							protected Map<String, String> getSchemaMapping() {
								return desktop.schemaMapping;
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
		} finally {
			UIUtil.resetWaitCursor(this);
		}
	}

	private final class SQLConsoleWithTitle extends SQLConsole {
		private final String initialTitle;
		private final JLabel titleLbl;
		private String title;

		private SQLConsoleWithTitle(Session session, MetaDataSource metaDataSource, Reference<DataModel> datamodel,
				ExecutionContext executionContext, String title, JLabel titleLbl, ConnectionType connectionType)
				throws SQLException {
			super(session, metaDataSource, datamodel, executionContext);
			this.initialTitle = title;
			this.titleLbl = titleLbl;
			updateConnectionType(connectionType);
		}

		@Override
		protected void refreshMetaData() {
			if (metaDataPanel != null) {
				metaDataPanel.reset();
			}
		}

		@Override
		protected void repaintMetaData() {
			if (metaDataPanel != null) {
				metaDataPanel.doRepaint();
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
				titleLbl.setToolTipText(null);
			} else {
				title = file.getName();
				titleLbl.setToolTipText(alternativeToolTip == null ? file.getAbsolutePath() : alternativeToolTip);
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
		String title = tabName + (sqlConsoleNr > 1 ? " (" + sqlConsoleNr + ")" : "") + " ";
		ConnectionInfo connection = lastConnectionInfo != null ? lastConnectionInfo
				: dbConnectionDialog != null ? dbConnectionDialog.currentConnection : null;
		ConnectionType connectionType = connection != null ? connection.getConnectionType() : null;

		final SQLConsoleWithTitle sqlConsole = new SQLConsoleWithTitle(session, metaDataSource, datamodel,
				executionContext, title, titleLbl, connectionType);
		titleLbl.addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent e) {
				workbenchTabbedPane.setSelectedComponent(sqlConsole);
			}

			@Override
			public void mouseClicked(MouseEvent e) {
				workbenchTabbedPane.setSelectedComponent(sqlConsole);
			}
		});
		;
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
			SmallButton closeButton = new SmallButton(UIUtil.plaf == PLAF.FLATDARK? closeDarkIcon : closeIcon) {
				@Override
				protected void onClick(MouseEvent e) {
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
		if (sqlConsoles.size() == 1) {
			SQLConsoleWithTitle console0 = sqlConsoles.get(0);
			workbenchTabbedPane.addChangeListener(new javax.swing.event.ChangeListener() {
				boolean loaded = false;

				public void stateChanged(javax.swing.event.ChangeEvent evt) {
					if (!loaded && workbenchTabbedPane.getSelectedComponent() == console0) {
						loaded = true;
						UIUtil.invokeLater(4, () -> console0.loadContent());
					}
				}
			});
		}
		return sqlConsole;
	}

	private boolean closeAllSQLConsoles() {
		List<SQLConsoleWithTitle> toClose = new ArrayList<SQLConsoleWithTitle>(sqlConsoles);
		for (SQLConsoleWithTitle sqlConsole : toClose) {
			workbenchTabbedPane.setSelectedComponent(sqlConsole);
			if (sqlConsole.getFile() != null && sqlConsole.isDirty() && !sqlConsole.isTempFileBased()) {
				if (!closeSQLConsole(sqlConsole, true)) {
					return false;
				}
			}
		}
		toClose = new ArrayList<SQLConsoleWithTitle>(sqlConsoles);
		for (SQLConsoleWithTitle sqlConsole : toClose) {
			workbenchTabbedPane.setSelectedComponent(sqlConsole);
			if (!closeSQLConsole(sqlConsole, false)) {
				return false;
			}
		}
		return true;
	}

	private boolean closeSQLConsole(SQLConsole sqlConsole, boolean ask) {
		if (ask) {
			if (!sqlConsole.isEmpty() && !(sqlConsole.getFile() != null && !sqlConsole.isDirty())) {
				if (JOptionPane.YES_OPTION != JOptionPane.showConfirmDialog(DataBrowser.this, "Close \""
						+ (sqlConsole instanceof SQLConsoleWithTitle ? ((SQLConsoleWithTitle) sqlConsole).getTitle()
								: "SQL Console")
						+ "\""
						+ (sqlConsole.getFile() == null && !sqlConsole.isTempFileBased() ? "?" : " without saving?"),
						"Close Console", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE)) {
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
		saveScriptMenuItem.setEnabled(sqlConsole != null && (sqlConsole.isDirty() && !sqlConsole.isTempFileBased()));
		saveScriptAsMenuItem.setEnabled(sqlConsole != null && !sqlConsole.isEmpty());
		if (sqlConsole != null) {
			goBackItem.setEnabled(false);
			goForwardItem.setEnabled(false);
		} else if (desktopUndoManager != null) {
			desktopUndoManager.updateUI();
		}
	}

	private void loadScriptMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_loadScriptMenuItemActionPerformed
		String fName = UIUtil.choseFile(null, ".", "Load SQL Script", "", this, false, true);
		if (fName != null) {
			File file = new File(fName);
			loadSQLScriptFile(file, false, null);
		}
	}// GEN-LAST:event_loadScriptMenuItemActionPerformed

	public void loadSQLScriptFile(File file, boolean silent, String alternativeToolTip) {
		if (file.exists() && file.isDirectory()) {
			return;
		}
		if (file.exists() && file.length() > 65L * 1024L * 1024L
				/ (file.getName().toLowerCase().endsWith(".zip") || file.getName().toLowerCase().endsWith(".gz") ? 5
						: 1)) {
			int o = JOptionPane.showOptionDialog(SwingUtilities.getWindowAncestor(this),
					"File " + file.getAbsolutePath() + "\nis large (" + (file.length() / 1024 / 1024)
							+ " MB). Loading might fail.",
					"File is large", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE, null,
					new Object[] { "Open", "Cancel" }, "Open");
			if (o != 0) {
				return;
			}
		}
		for (SQLConsole sqlConsole : sqlConsoles) {
			if (file.equals(sqlConsole.getFile()) && !sqlConsole.isDirty()) {
				closeSQLConsole(sqlConsole, false);
				break;
			}
		}
		try {
			UIUtil.setWaitCursor(this);
			createNewSQLConsole(getMetaDataSource()).loadFromFile(file, silent, alternativeToolTip);
		} catch (Throwable e) {
			UIUtil.showException(this, "Error", e, session);
		} finally {
			UIUtil.resetWaitCursor(this);
		}
	}

	private void saveScriptMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_saveScriptMenuItemActionPerformed
		SQLConsole sqlConsole = getCurrentSQLConsole();
		if (sqlConsole != null) {
			if (sqlConsole.getFile() == null && sqlConsole.isTempFileBased()) {
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
	}// GEN-LAST:event_saveScriptMenuItemActionPerformed

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
							item.setAccelerator(KeyStroke.getKeyStroke(accelerator.getKeyCode(), mask,
									accelerator.isOnKeyRelease()));
						}
					}
				}
			}
		}
	}

	private void saveScriptAsMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_saveScriptAsMenuItemActionPerformed
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
	}// GEN-LAST:event_saveScriptAsMenuItemActionPerformed

	private void exitMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_exitMenuItemActionPerformed
		if (closeAllSQLConsoles()) {
			DataBrowser.this.dispose();
		}
	}// GEN-LAST:event_exitMenuItemActionPerformed

	private void analyseSQLMenuItem1ActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_analyseSQLMenuItem1ActionPerformed
		AssociationProposerView assocProposerView = new AssociationProposerView(this, datamodel.get(), null, 3,
				executionContext);
		if (assocProposerView.isAccepted()) {
			openDataModelEditor(true);
		}
	}// GEN-LAST:event_analyseSQLMenuItem1ActionPerformed

	private void columnOrderItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_columnOrderItemActionPerformed
		openColumnOrderEditor(null);
	}// GEN-LAST:event_columnOrderItemActionPerformed

	private void openColumnOrderEditor(Table table) {
		try {
			if (new ColumnOrderEditor(this, table, datamodel.get(), executionContext).wasOk()) {
				desktop.reloadDataModel(desktop.schemaMapping);
				dataModelViewFrame = null;
				updateDataModelView(null);
				updateStatusBar();
				toFront();
				askForDataModel();
			}
		} catch (Throwable e) {
			UIUtil.showException(this, "Error", e);
		}
	}

	private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_jButton1ActionPerformed
		updateInfoPanel.setVisible(false);
	}// GEN-LAST:event_jButton1ActionPerformed

	private void downloadButtonActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_downloadButtonActionPerformed
		updateInfoPanel.setVisible(false);
		UpdateInfoManager.download();
	}// GEN-LAST:event_downloadButtonActionPerformed

	private void nativeLAFCheckBoxMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_nativeLAFCheckBoxMenuItemActionPerformed

	}// GEN-LAST:event_nativeLAFCheckBoxMenuItemActionPerformed

	private void addBookmarkMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_addBookmarkMenuItemActionPerformed
		desktop.storeSession(new BookmarksPanel(this, bookmarkMenu, desktop, executionContext));
	}// GEN-LAST:event_addBookmarkMenuItemActionPerformed

	private void editBookmarkMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_editBookmarkMenuItemActionPerformed
		new BookmarksPanel(this, bookmarkMenu, desktop, executionContext).editBookmarks();
		desktop.updateAllBookmarkMenues();
	}// GEN-LAST:event_editBookmarkMenuItemActionPerformed

	private void downloadMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_downloadMenuItemActionPerformed
		UpdateInfoManager.download();
	}// GEN-LAST:event_downloadMenuItemActionPerformed

	private void zoomInMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_zoomInMenuItemActionPerformed
		desktop.zoom(1);
	}// GEN-LAST:event_zoomInMenuItemActionPerformed

	private void zoomOutMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_zoomOutMenuItemActionPerformed
		desktop.zoom(-1);
	}// GEN-LAST:event_zoomOutMenuItemActionPerformed

	private void checkPKMenuItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_checkPKMenuItemActionPerformed
		try {
			if (dbConnectionDialog.isConnected || dbConnectionDialog.connect("Check Primary Keys")) {
				BasicDataSource dataSource = UIUtil.createBasicDataSource(this,
						dbConnectionDialog.currentConnection.driverClass, dbConnectionDialog.currentConnection.url,
						dbConnectionDialog.currentConnection.user, dbConnectionDialog.getPassword(), 0,
						dbConnectionDialog.currentJarURLs());
				UIUtil.validatePrimaryKeys(this, dataSource, new TreeSet<Table>(datamodel.get().getTables()));
			}
		} catch (Exception e) {
			// ignore
		}
	}// GEN-LAST:event_checkPKMenuItemActionPerformed

	private void refreshButtonActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_refreshButtonActionPerformed
	}// GEN-LAST:event_refreshButtonActionPerformed

	private void createCLIItemActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_createCLIItemActionPerformed
		String mapping = desktop.getRawSchemaMapping();
		String bookmark = BookmarksPanel.getLastUsedBookmark(executionContext);
		if (bookmark != null) {
			bookmark = bookmarkName(bookmark);
		}
		List<String> bookmarks = new ArrayList<String>();
		for (StringBuilder sb : BookmarksPanel.loadBookmarks(executionContext)) {
			bookmarks.add(bookmarkName(sb.toString()));
		}
		new CLIPanel(dbConnectionDialog, true, null, mapping, bookmarks, bookmark, executionContext).open(this);
	}// GEN-LAST:event_createCLIItemActionPerformed

	private void consistencyCheckMenuItem1ActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_consistencyCheckMenuItem1ActionPerformed
		consistencyCheckMenuItemActionPerformed(evt);
	}// GEN-LAST:event_consistencyCheckMenuItem1ActionPerformed

	private void renderHtmlActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_renderHtmlActionPerformed
		try {
			List<String> args = new ArrayList<String>();
			args.add("render-datamodel");
			if (UIUtil.canRunJailer()) {
				UIUtil.runJailer(this, args, false, true, true, null, null, null /* dbConnectionDialog.getPassword() */,
						null, null, false, true, false, null, executionContext);
				HtmlDataModelRenderer renderer = Configuration.getInstance().getRenderer();
				String of = renderer.outputFolderOf(datamodel.get());
				BrowserLauncher.openURL(Environment.newFile(of + "/index.html").toURI(), this);
			}
		} catch (Exception e) {
			UIUtil.showException(this, "Error", e);
		}
	}// GEN-LAST:event_renderHtmlActionPerformed

	private void searchBarToggleButtonActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_searchBarToggleButtonActionPerformed
		UISettings.store("searchBarToggleButton", searchBarToggleButton.isSelected());
		searchPanelContainer.setVisible(searchBarToggleButton.isSelected());
		searchPanelSplitSizerPanel.setVisible(searchPanelContainer.isVisible());
	}// GEN-LAST:event_searchBarToggleButtonActionPerformed

	private void tbZoomOutButtonActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_tbZoomOutButtonActionPerformed
	}// GEN-LAST:event_tbZoomOutButtonActionPerformed

	private void tbZoom0ButtonActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_tbZoom0ButtonActionPerformed
	}// GEN-LAST:event_tbZoom0ButtonActionPerformed

	private void tbZoom1ButtonActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_tbZoom1ButtonActionPerformed
	}// GEN-LAST:event_tbZoom1ButtonActionPerformed

	private void tbZoom2ButtonActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_tbZoom2ButtonActionPerformed
	}// GEN-LAST:event_tbZoom2ButtonActionPerformed

	private void tbZoom3ButtonActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_tbZoom3ButtonActionPerformed
	}// GEN-LAST:event_tbZoom3ButtonActionPerformed

	private void tbZoom4ButtonActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_tbZoom4ButtonActionPerformed
	}// GEN-LAST:event_tbZoom4ButtonActionPerformed

	private String bookmarkName(String bookmarkFileName) {
		if (bookmarkFileName.endsWith(BookmarksPanel.BOOKMARKFILE_EXTENSION)) {
			return bookmarkFileName.substring(0,
					bookmarkFileName.length() - BookmarksPanel.BOOKMARKFILE_EXTENSION.length());
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
								loadSQLScriptFile(files.get(k), false, null);
							}

							dtde.dropComplete(true);
						}
					}
					return;
				} catch (Throwable t) {
					LogUtil.warn(t);
				}
				try {
					dtde.rejectDrop();
				} catch (Throwable t) {
					LogUtil.warn(t);
				}
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

	public static final String LAST_SESSION_FILE = ".lastsession";

	private void storeLastSession() {
		try {
			desktop.storeSession(Environment.newFile(LAST_SESSION_FILE).getPath());
			BufferedReader in = new BufferedReader(new FileReader(Environment.newFile(LAST_SESSION_FILE)));
			int c;
			StringBuilder sb = new StringBuilder();
			while ((c = in.read()) != -1) {
				sb.append((char) c);
			}
			in.close();
			BookmarkId bookmark = new BookmarkId(null, executionContext.getCurrentModelSubfolder(),
					executionContext.getCurrentConnectionAlias(), desktop.getRawSchemaMapping());
			bookmark.setContent(sb.toString());

			RowBrowser root = null;
			for (RowBrowser tb : desktop.tableBrowsers) {
				if (tb.browserContentPane != null && tb.browserContentPane.getParentBrowser() == null
						&& tb.internalFrame != null) {
					if (root == null || root.internalFrame.getY() > tb.internalFrame.getY()) {
						root = tb;
					}
				}
			}
			bookmark.setContentInfo(root == null ? ""
					: (Quoting.staticUnquote(root.browserContentPane.table.getName())
							+ (desktop.tableBrowsers.size() > 1 ? " (+" + (desktop.tableBrowsers.size() - 1) + ")"
									: "")));

			// default data model layout
			File bookmarksFolder = BookmarksPanel.getBookmarksFolder(executionContext);
			bookmarksFolder.mkdirs();
			String sFile = new File(bookmarksFolder, "default").getPath();
			desktop.storeSession(sFile);
			UISettings.storeLastSession(bookmark, "B");
		} catch (Throwable t) {
			LogUtil.warn(t);
		}
	}

	private DesktopAnchorManager anchorManager;
	private DesktopOutline desktopOutline;
	private WhereConditionEditorPanel whereConditionEditorPanel;
	private BrowserContentPane whereConditionEditorSubject;
	private JComponent whereConditionEditorCloseButton;

	private void initNavTree() {
		JPanel p;
		p = new JPanel(new BorderLayout()) {
			{
				setOpaque(false);
			}

			@Override
			public void paint(Graphics g) {
				super.paint(g);
				if (g instanceof Graphics2D) {
					paintRowCounters((Graphics2D) g, getBounds());
				}
			}
		};
		GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 1;
		gridBagConstraints.gridy = 1;
		gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
		gridBagConstraints.weightx = 1.0;
		gridBagConstraints.weighty = 1.0;
		navTreeLayeredPane.add(p, gridBagConstraints);
		navTreeLayeredPane.setLayer(p, javax.swing.JLayeredPane.POPUP_LAYER);
		navigationTree.addComponentListener(new ComponentListener() {
			int width = -1;

			@Override
			public void componentResized(ComponentEvent e) {
				if (navigationTree.getWidth() != width) {
					width = navigationTree.getWidth();
					updateRowCounters();
				}
			}

			@Override
			public void componentShown(ComponentEvent e) {
			}

			@Override
			public void componentMoved(ComponentEvent e) {
			}

			@Override
			public void componentHidden(ComponentEvent e) {
			}
		});

		navigationTree.addTreeExpansionListener(new TreeExpansionListener() {
			@Override
			public void treeExpanded(TreeExpansionEvent event) {
				updateRowCounters();
			}

			@Override
			public void treeCollapsed(TreeExpansionEvent event) {
				updateRowCounters();
			}
		});
	}

	private NavigableMap<Integer, MDTable> rowCounters = new TreeMap<Integer, MDTable>();

	private void paintRowCounters(Graphics2D g, Rectangle bounds) {
		Rectangle visibleRect = navigationTree.getVisibleRect();
		g.clipRect(1, 1, visibleRect.width + 1, visibleRect.height);
		g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
		g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
		int oh = UIUtil.plaf == PLAF.NIMBUS ? 3 : 2;
		int ow = UIUtil.plaf == PLAF.NIMBUS ? 1 : 0;

		rowCounters.subMap(visibleRect.y - 16, visibleRect.y + visibleRect.height + 16).forEach((ry, mdTable) -> {
			Long rc = mdTable.getEstimatedRowCount();
			if (rc != null) {
				RowCountRenderer value;
				String prefix = mdTable.isEstRCIsLowerBound() ? ">" : "";

				if (rc == 0) {
					return;
				} else if (rc >= 1000000000) {
					value = new RowCountRenderer(prefix + String.format("%,1.1f", (double) rc / 1000000000.0),
							RowCountRenderer.g);
				} else if (rc >= 1000000) {
					value = new RowCountRenderer(prefix + String.format("%,1.1f", (double) rc / 1000000.0),
							RowCountRenderer.m);
				} else if (rc >= 1000) {
					value = new RowCountRenderer(prefix + String.format("%,1.1f", (double) rc / 1000.0),
							RowCountRenderer.k);
				} else {
					value = new RowCountRenderer(prefix + String.format("%,1.0f", (double) rc), null);
				}
				int x = visibleRect.width - value.getWidth() - 1;
				int y = ry - visibleRect.y + value.getHeight() - 1;
				g.setColor(Colors.Color_FlatTreeViewBG);
				g.fillRect(x - 8, y - value.getHeight() + 2, visibleRect.width - x + 9 + ow, value.getHeight() + oh);
				y -= value.getHeight() - oh;
				g.translate(x, y);
				value.doPaint((Graphics) g);
				(g).translate(-x, -y);
			}
		});
	}

	private void updateRowCounters() {
		DefaultTreeModel m = (DefaultTreeModel) navigationTree.getModel();
		TreeNode root2 = (TreeNode) m.getRoot();
		rowCounters = new TreeMap<Integer, MDTable>();
		if (session != null) {
			MetaDataSource metaDataSource = getMetaDataSource(session);
			if (metaDataSource != null && metaDataSource.isInitialized()) {
				trav(m, root2, new TreePath(root2), metaDataSource, rowCounters);
			}
		}
		navTreeLayeredPane.repaint();
	}

	private void trav(DefaultTreeModel m, TreeNode n, TreePath path, MetaDataSource metaDataSource,
			Map<Integer, MDTable> rowCounters) {
//		if (n.isLeaf()) {
		Rectangle b = navigationTree.getPathBounds(path);
		if (b != null) {
			Object uo = ((DefaultMutableTreeNode) n).getUserObject();
			if (uo instanceof TreeNodeForRowBrowser) {
				Table table = ((TreeNodeForRowBrowser) uo).rowBrowser.browserContentPane.table;
				if (table != null) {
					MDTable mdTable = metaDataSource.toMDTable(table);
					if (mdTable != null) {
						rowCounters.put(b.y, mdTable);
					}
				}
			}
		}
//		}
		Enumeration<? extends TreeNode> e = n.children();
		while (e != null && e.hasMoreElements()) {
			TreeNode nextElement = e.nextElement();
			trav(m, nextElement, path.pathByAddingChild(nextElement), metaDataSource, rowCounters);
		}
	}

	@Override
	public void onDataModelsChanged() {
		try {
			UIUtil.setWaitCursor(this);
			desktop.reloadDataModel(desktop.schemaMapping);
			dataModelViewFrame = null;
			updateDataModelView(null);
			toFront();
			updateStatusBar();
		} catch (Exception e) {
			UIUtil.showException(this, "Error", e, session);
		} finally {
			UIUtil.resetWaitCursor(this);
		}
	}

	@Override
	public void onNewPlaf() {
		updateStatusBar();
		UIUtil.invokeLater(() -> {
			try {
				if (desktop != null) {
					File tmpFile = Configuration.getInstance().createTempFile();
					desktop.storeSession(tmpFile.toString());
					desktop.restoreSession(null, this, tmpFile.toString(), true);
					tmpFile.delete();
				}
			} catch (Exception e) {
				UIUtil.showException(this, "Error", e, session);
			}
		});
	}

	private ImageIcon tableIcon;
	private ImageIcon addTableIcon;
	private ImageIcon databaseIcon;
	private ImageIcon redIcon;
	private ImageIcon blueIcon;
	private ImageIcon greenIcon;
	private Icon closeIcon;
	private Icon closeDarkIcon;
	private Icon closeOverIcon;
	private ImageIcon sqlConsoleIcon;
	private ImageIcon addSqlConsoleIcon;
	private ImageIcon navigationIcon;
	private ImageIcon desktopIcon;
	private ImageIcon closureIcon;
	private ImageIcon searchIcon;
	private ImageIcon tbBackIcon;
	private ImageIcon tbForwardIcon;
	private ImageIcon tbZoom0Icon;
	private ImageIcon tbZoom1Icon;
	private ImageIcon tbZoom2Icon;
	private ImageIcon tbZoom3Icon;
	private ImageIcon tbZoom4Icon;
	private ImageIcon tbZoomInIcon;
	private ImageIcon runIcon;
	private ImageIcon tbZoomOutIcon;
	private ImageIcon connectionIcon;
	private ImageIcon noconnectionIcon;
	private ImageIcon noconnectionIconnw;
	private ImageIcon modelIcon;
	private ImageIcon ieditdetailsIcon;
	private ImageIcon menuIcon;
	private ImageIcon splitIcon;
	private ImageIcon onePxIcon;
	private ImageIcon editIcon;
	private ImageIcon copyIcon;
	private ImageIcon deleteIcon;
	private ImageIcon clearIcon;
	{
		// load images
		tableIcon = UIUtil.readImage("/table.png");
		addTableIcon = UIUtil.readImage("/addtable.png");
		databaseIcon = UIUtil.readImage("/database.png");
		redIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/reddot.gif"));
		blueIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/bluedot.gif"));
		greenIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/greendot.gif"));
		closeIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/close.png"), 1.4);
		closeDarkIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/closedark.png"), 1.4);
		closeOverIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/close_over.png"), 1.4);
		runIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/run.png"));
		sqlConsoleIcon = UIUtil.readImage("/runall.png");
		desktopIcon = UIUtil.readImage("/navigation.png");
		addSqlConsoleIcon = UIUtil.readImage("/add.png");
		navigationIcon = UIUtil.readImage("/navigation.png");
		closureIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/closure.png"));
		searchIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/search.png"));
		tbBackIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/tb_back.png"));
		tbForwardIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/tb_forward.png"));
		tbZoom0Icon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/tb_zoom0.png"));
		tbZoom1Icon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/tb_zoom1.png"));
		tbZoom2Icon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/tb_zoom2.png"));
		tbZoom3Icon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/tb_zoom3.png"));
		tbZoom4Icon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/tb_zoom4.png"));
		tbZoomInIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/tb_zoomin.png"));
		tbZoomOutIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/tb_zoomout.png"));
		connectionIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/connection.png"));
		noconnectionIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/nonconnection.png"));
		noconnectionIconnw = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/nonconnectionnw.png"));
		modelIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/model.png"));
		ieditdetailsIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/ieditdetails_64.png"));
		menuIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/menu.png"));
		splitIcon = UIUtil.scaleIcon(UIUtil.readImage("/spliticon.png"), 1f / 1.75f);
		onePxIcon = UIUtil.readImage("/1px.png");
		editIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/editdetailsitem.png"));
		copyIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/copy.png"));
		deleteIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/delete.png"));
		clearIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/clear.png"));
	}

}
