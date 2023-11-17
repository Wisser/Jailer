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

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Window;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.stream.Collectors;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.RowSorter;
import javax.swing.RowSorter.SortKey;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;
import javax.swing.border.LineBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.database.BasicDataSource;
import net.sf.jailer.database.Session;
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;
import net.sf.jailer.ui.commandline.CommandLineInstance;
import net.sf.jailer.ui.commandline.UICommandLine;
import net.sf.jailer.ui.databrowser.metadata.MetaDataPanel;
import net.sf.jailer.ui.util.StringObfuscator;
import net.sf.jailer.ui.util.UISettings;
import net.sf.jailer.util.ClasspathUtil;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.CsvFile.Line;
import net.sf.jailer.util.LogUtil;
import net.sf.jailer.util.Pair;

/**
 * Database connection dialog.
 * 
 * @author Ralf Wisser
 */
public class DbConnectionDialog extends javax.swing.JDialog {

	/**
	 * The logger.
	 */
	private static final Logger logger = LoggerFactory.getLogger(MetaDataPanel.class);

	/**
	 * <code>true</code> if valid connection is available.
	 */
	public boolean isConnected = false;
	
	public enum ConnectionType {
		Development("Development database") {
			@Override
			public Color getBackground() {
				return null;
			}
		},
		Test("Test (QA) database") {
			@Override
			public Color getBackground() {
				return new Color(205, 255, 205);
			}
		},
		Staging("Staging database") {
			@Override
			public Color getBackground() {
				return new Color(255, 255, 176);
			}
		},
		Production("Production database") {
			@Override
			public Color getBackground() {
				return new Color(255, 205, 205);
			}
		};
		
		private ConnectionType(String displayName) {
			this.displayName = displayName;
		}
		
		@Override
		public String toString() {
			return displayName;
		}
		
		public abstract Color getBackground();
		public final String displayName;
	}

	/**
	 * Holds connection information.
	 */
	public static class ConnectionInfo implements Serializable, Cloneable {
		
		// TODO Implement true encryption. The credentials are currently only obfuscated. This protects the data from being stolen too easily, but of course it is not secure.
		private static StringObfuscator stringObfuscator = new StringObfuscator();
		
		@Override
		protected Object clone() throws CloneNotSupportedException {
			return super.clone();
		}
		private static final long serialVersionUID = -8034755966212631808L;
		private boolean encrypted = false;
		public String alias = "";
		public String driverClass = "";
		public String url = "";
		public String user = "";
		public String password = "";
		public String jar1 = "";
		public String jar2 = "";
		public String jar3 = "";
		public String jar4 = "";
		public transient String dataModelFolder;
		private String connectionTypeName;
		private transient ConnectionType connectionType;
		
		/**
		 * Constructor.
		 */
		public ConnectionInfo() {
		}
		
		/**
		 * Constructor.
		 */
		public ConnectionInfo(ExecutionContext executionContext) {
			dataModelFolder = DataModelManager.getCurrentModelSubfolder(executionContext);
		}
		
		public ConnectionType getConnectionType() {
			if (connectionType != null) {
				return connectionType;
			}
			if (connectionTypeName == null) {
				return connectionType = ConnectionType.Development;
			}
			try {
				connectionType = ConnectionType.valueOf(connectionTypeName);
			} catch (Exception e) {
				connectionType = ConnectionType.Development;
			}
			return connectionType;
		}
		
		public void setConnectionType(ConnectionType type) {
			connectionType = type;
			connectionTypeName = type.name();
		}
		
		public void assign(ConnectionInfo ci) {
			encrypted = ci.encrypted;
			alias = ci.alias;
			driverClass = ci.driverClass;
			url = ci.url;
			user = ci.user;
			password = ci.password;
			jar1 = ci.jar1;
			jar2 = ci.jar2;
			jar3 = ci.jar3;
			jar4 = ci.jar4;
			dataModelFolder = ci.dataModelFolder;
			connectionType = ci.connectionType;
			connectionTypeName = ci.connectionTypeName;
		}
		
		public boolean encrypt() {
			if (!encrypted && System.getProperty("unencrypted-credentials") == null) {
				url = stringObfuscator.encrypt(url);
				user = stringObfuscator.encrypt(user);
				password = stringObfuscator.encrypt(password);
				encrypted = true;
				return true;
			} else {
				return false;
			}
		}
		
		public boolean decrypt() {
			if (encrypted) {
				url = stringObfuscator.decrypt(url);
				user = stringObfuscator.decrypt(user);
				password = stringObfuscator.decrypt(password);
				encrypted = false;
				return true;
			} else {
				return false;
			}
		}
	}

	/**
	 * List of available connections.
	 */
	private List<ConnectionInfo> connectionList;

	/**
	 * Gets list of available connections.
	 */
	public List<ConnectionInfo> getConnectionList() {
		return connectionList;
	}

	/**
	 * Currently selected connection.
	 */
	public ConnectionInfo currentConnection;

	/**
	 * The parent frame.
	 */
	private Window parent;
	
	final ExecutionContext executionContext;
	
	/**
	 * Gets connection to DB.
	 * 
	 * @return <code>true</code> if connection succeeded
	 */
	public boolean connect(String reason) {
		return connect(reason, false);
	}

	public ExecutionContext getExecutionContext() {
		return executionContext;
	}

	private Font font =  new JLabel("normal").getFont();
	private Font normal = font.deriveFont(font.getStyle() & ~Font.BOLD, font.getSize());
    private Font bold = font.deriveFont(font.getStyle() | Font.BOLD, font.getSize());
	private Map<String, Date> aliasTimestamp = new HashMap<String, Date>();
	private boolean ok;
	
	/**
	 * Gets connection to DB.
	 * 
	 * @return <code>true</code> if connection succeeded
	 */
	public boolean connect(String reason, boolean keepState) {
		boolean oldIsConnected = isConnected;
		ConnectionInfo oldCurrentConnection = currentConnection;
		ok = false;
		try {
			pack();
			setSize(Math.max(840, getWidth()), 450);
			if (parent != null && parent.isVisible()) {
				int os = parent.getWidth() > 800 ? 0 : 80;
				setLocation(os + parent.getX() + (parent.getWidth() - getWidth()) / 2,
						Math.max((int) UIUtil.getScreenBounds().getY(),
								os + parent.getY() + (parent.getHeight() - getHeight()) / 2));
			} else {
				setLocation(100, 150);
			}
			setTitle((reason == null ? "" : (reason + " - ")) + "Connect");
			sortConnectionList();
			refresh();
			if (connectionsTable.getModel().getRowCount() > 0) {
				int cRI = -1;
				if (oldCurrentConnection != null) {
					for (int i = 0; i < connectionList.size(); ++i) {
						if (connectionList.get(i).alias != null && connectionList.get(i).alias.equals(oldCurrentConnection.alias)) {
							cRI = i;
							break;
						}
					}
				}
				if (cRI >= 0) {
					connectionsTable.getSelectionModel().setSelectionInterval(cRI, cRI);
					int fCRI = cRI;
					UIUtil.invokeLater(() -> connectionsTable.scrollRectToVisible(connectionsTable.getCellRect(fCRI, 0, true)));
				} else {
					connectionsTable.getSelectionModel().setSelectionInterval(0, 0);
				}
			}
			setVisible(true);
			if (currentConnection == null) {
				isConnected = false;
			}
			return isConnected && ok;
		} finally {
			if (keepState && !isConnected) {
				isConnected = oldIsConnected;
				currentConnection = oldCurrentConnection;
			}
		}
	}

	public void select(ConnectionInfo ci) {
		if (connectionsTable.getModel().getRowCount() > 0) {
			int cRI = -1;
			for (int i = 0; i < connectionList.size(); ++i) {
				if (connectionList.get(i).alias != null && connectionList.get(i).alias.equals(ci.alias)) {
					cRI = i;
					break;
				}
			}
			if (cRI >= 0) {
				connectionsTable.getSelectionModel().setSelectionInterval(cRI, cRI);
				refresh();
			}
		}
	}
	
	public boolean connectSilent(ConnectionInfo ci) {
		if (connectionsTable.getModel().getRowCount() > 0) {
			int cRI = -1;
			for (int i = 0; i < connectionList.size(); ++i) {
				if (connectionList.get(i).alias != null && connectionList.get(i).alias.equals(ci.alias)) {
					cRI = i;
					break;
				}
			}
			if (cRI >= 0) {
				connectionsTable.getSelectionModel().setSelectionInterval(cRI, cRI);
				refresh();
			} else {
				return false;
			}
		}

		isConnected = false;
		Component root = SwingUtilities.getWindowAncestor(mainPanel);
		if (root == null) {
			root = mainPanel;
		}
		try {
			UIUtil.setWaitCursor(root);
			SessionForUI.startFadeIn();
			if (testConnection(dataModelChanger != null? dataModelChanger.ownerWindow() : mainPanel, currentConnection, null)) {
				isConnected = true;
				ok = true;
				executionContext.setCurrentConnectionAlias(currentConnection.alias);
				if (dataModelChanger != null && DataModelManager.getCurrentModelSubfolder(executionContext) != null && warnOnConnect) {
					dataModelChanger.change(currentConnection.dataModelFolder);
				}
				onConnect(currentConnection);
				if (dataModelChanger != null) {
					dataModelChanger.afterConnect(currentConnection);
				}
				if (currentConnection.alias != null && !"".equals(currentConnection.alias)) {
					UISettings.addRecentConnectionAliases(currentConnection.alias);
				}
			}
		} finally {
			UIUtil.resetWaitCursor(root);
		}
		return isConnected;
	}

	private final InfoBar infoBar;
	private final boolean dataModelAware;
	private final DataModelChanger dataModelChanger;
	
	// TODO remove, no longer used
	private final boolean showOnlyRecentyUsedConnections;
	
	public interface DataModelChanger {
		void change(String dataModelSubfolder);
		void onConnectionListChanged();
		void afterConnect(ConnectionInfo ci);
		Window ownerWindow();
	}
	
	/** Creates new form DbConnectionDialog */
	public DbConnectionDialog(Window parent, DbConnectionDialog other, String applicationName, DataModelChanger dataModelChanger, ExecutionContext executionContext) {
		this(parent, applicationName, other.infoBar == null? null : new InfoBar(other.infoBar), dataModelChanger, executionContext);
		this.isConnected = other.isConnected;
		this.connectionList = other.connectionList;
		if (other.currentConnection != null) {
			try {
				this.currentConnection = (ConnectionInfo) other.currentConnection.clone();
			} catch (CloneNotSupportedException e) {
				this.currentConnection = other.currentConnection;
			}
		}
	}

	/** 
	 * Creates new form DbConnectionDialog
	 * 
	 * @param applicationName application name. Used to create the name of the demo database alias. 
	 */
	public DbConnectionDialog(Window parent, String applicationName, InfoBar infoBar, DataModelChanger dataModelChanger, ExecutionContext executionContext) {
		this(parent, applicationName, infoBar, dataModelChanger, executionContext, true, false);
	}
	
	/** 
	 * Creates new form DbConnectionDialog
	 * 
	 * @param applicationName application name. Used to create the name of the demo database alias. 
	 * @param showOnlyRecentyUsedConnections 
	 */
	public DbConnectionDialog(Window parent, String applicationName, InfoBar infoBar, DataModelChanger dataModelChanger, ExecutionContext executionContext, boolean dataModelAware, boolean showOnlyRecentyUsedConnections) {
		super(parent);
		setModal(true);
		this.dataModelChanger = dataModelChanger;
		this.executionContext = executionContext;
		this.parent = parent;
		this.infoBar = infoBar;
		this.dataModelAware = dataModelAware;
		this.showOnlyRecentyUsedConnections = showOnlyRecentyUsedConnections;
		allDialogs.put(this, this);
		initComponents(); UIUtil.initComponents(this);
		loadConnectionList(showOnlyRecentyUsedConnections);
		jButton1.setIcon(UIUtil.scaleIcon(jButton1, okIcon));
		closeButton.setIcon(UIUtil.scaleIcon(closeButton, cancelIcon));
		restoreLastSessionButton.setVisible(false);
		connectionsTable.setAutoCreateRowSorter(true);

		if (infoBar == null) {
			infoBar = new InfoBar("Connect to Database", 
					"Select a connection to the database, or create a new connection.\n" +
					"New connections will be assigned to the datamodel \"" + DataModelManager.getModelDetails(DataModelManager.getCurrentModelSubfolder(executionContext), executionContext).a + "\".", null);
		}
		
		UIUtil.replace(infoBarLabel, infoBar);
		
//		newButton.setVisible(dataModelAware);
		jLabel1.setVisible(dataModelAware);
//		copy.setVisible(dataModelAware);
		
		if (showOnlyRecentyUsedConnections) {
			editButton.setVisible(false);
			deleteButton.setVisible(false);
		}
		
		int i;
		initTableModel();
		
		connectionsTable.setAutoCreateRowSorter(true);

		final TableCellRenderer defaultTableCellRenderer = connectionsTable
				.getDefaultRenderer(String.class);
		connectionsTable.setShowGrid(false);
		connectionsTable.setDefaultRenderer(Object.class,
				new TableCellRenderer() {

					@Override
					public Component getTableCellRendererComponent(
							JTable table, Object value, boolean isSelected,
							boolean hasFocus, int row, int column) {
						Component render = defaultTableCellRenderer
								.getTableCellRendererComponent(table, value,
										isSelected, false /* hasFocus */, row, column);
						if (render instanceof JLabel) {
							((JLabel) render).setToolTipText(String.valueOf(value));
							if (!isSelected) {
								final Color BG1 = UIUtil.TABLE_BACKGROUND_COLOR_1;
								final Color BG2 = UIUtil.TABLE_BACKGROUND_COLOR_2;
								((JLabel) render)
										.setBackground((row % 2 == 0) ? BG1
												: BG2);
							} else {
								((JLabel) render).setBackground(new Color(160, 200, 255));
							}
							RowSorter<?> rowSorter = connectionsTable.getRowSorter();
							int selectedRowIndex = -1;
							if (row >= 0 && row < connectionList.size()) {
								selectedRowIndex = rowSorter.convertRowIndexToModel(row);
							}
							boolean inContext = isAssignedToDataModel(selectedRowIndex);
							if (inContext) {
								((JLabel) render).setForeground(Color.black);
							} else {
								((JLabel) render).setForeground(Color.gray);
							}
							
							Color bg = null;
							
							ConnectionType connectionType = null;
							if (selectedRowIndex >= 0 && column == 1 && selectedRowIndex < connectionList.size()) {
								connectionType = connectionList.get(selectedRowIndex).getConnectionType();
							}
							
							if (connectionType != null && connectionType.getBackground() != null) {
								bg = connectionType.getBackground();
								if (isSelected) {
									Border lb = new LineBorder(new Color(160, 200, 255), 2, false) {
										@Override
										public Insets getBorderInsets(Component c, Insets insets) {
											return new Insets(0, 0, 0, 0);
										}
									};
									if (((JLabel) render).getBorder() == null) {
										((JLabel) render).setBorder(lb);
									} else {
										((JLabel) render).setBorder(BorderFactory.createCompoundBorder(lb, ((JLabel) render).getBorder()));
									}
								}
								((JLabel) render).setToolTipText("<html>" + UIUtil.toHTMLFragment(((JLabel) render).getToolTipText(), 0) + "<br><hr>" + connectionType.displayName + "</html>");
							}
							
							if (bg != null) {
								((JLabel) render).setBackground(bg);
							}
						}
						render.setFont(column == 0? bold : normal);
						((JLabel) render).setIcon(null);
						if (value instanceof String && ((String) value).startsWith("*")) {
							((JLabel) render).setToolTipText(null);
							((JLabel) render).setText(null);
							((JLabel) render).setIcon(UIUtil.scaleIcon(((JLabel) render), UIUtil.readImage(((String) value).substring(1), false), 1.4));
						}
						return render;
					}
				});
		
		connectionsTable.getTableHeader().setReorderingAllowed(false);
		try {
			((DefaultTableCellRenderer) connectionsTable.getTableHeader().getDefaultRenderer()).setHorizontalAlignment(JLabel.LEFT);
		} catch (Exception e) {
			// ignore
		}
		
		KeyListener keyListener = new KeyListener() {
            @Override
            public void keyTyped(KeyEvent e) {
            }
            
            @Override
            public void keyReleased(KeyEvent e) {
            }
            
            @Override
            public void keyPressed(KeyEvent e) {
            	int delta = 0;
                if (e.getKeyCode() == KeyEvent.VK_UP) {
                	delta = -1;
                } else if (e.getKeyCode() == KeyEvent.VK_DOWN) {
                	delta = 1;
                }
                if (delta != 0) {
                	int selectedRow = connectionsTable.getSelectedRow();
                	if (selectedRow >= 0) {
                		selectedRow += delta;
            			if (selectedRow >= 0 && selectedRow < connectionsTable.getRowCount()) {
            				connectionsTable.getSelectionModel().setSelectionInterval(selectedRow, selectedRow);
                		}
                	}
                }
            }
        };
        jButton1.addKeyListener(keyListener);
        closeButton.addKeyListener(keyListener);
		
		connectionsTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		connectionsTable.getSelectionModel().addListSelectionListener(
				new ListSelectionListener() {
					@Override
					public void valueChanged(ListSelectionEvent evt) {
						refresh();
						if (jButton1.isEnabled()) {
							jButton1.grabFocus();
						}
					}
				});
		connectionsTable.addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent me) {
				JTable table =(JTable) me.getSource();
				Point p = me.getPoint();
				int row = table.rowAtPoint(p);
				if (me.getClickCount() >= 2) {
					connectionsTable.getSelectionModel().setSelectionInterval(row, row);
					refresh();
					if (jButton1.isEnabled()) {
						connect();
					}
				} else if (jButton1.isEnabled()) {
					jButton1.grabFocus();
				}
			}
		});
		
		if (currentConnection != null) {
			i = connectionList.indexOf(currentConnection);
			if (i >= 0) {
				connectionsTable.getSelectionModel().setSelectionInterval(i, i);
			}
		}
		
		addComponentListener(new ComponentListener() {
			@Override
			public void componentShown(ComponentEvent e) {
				if (connectionList != null && connectionList.isEmpty()) {
					newButtonActionPerformed(null);
				}
			}
			@Override
			public void componentHidden(ComponentEvent e) {
			}
			@Override
			public void componentMoved(ComponentEvent e) {
			}
			@Override
			public void componentResized(ComponentEvent e) {
			}
		});
		
		connectionsTable.setRowHeight((int) (connectionsTable.getRowHeight() * 1.5));
		refresh();
	}

	/**
	 * Initializes the table model.
	 */
	@SuppressWarnings("rawtypes")
	private Object[][] initTableModel() {
		Object[][] data = new Object[connectionList.size()][];
		int i = 0;
		for (ConnectionInfo ci: connectionList) {
			Pair<String, Long> modelDetails = DataModelManager.getModelDetails(ci.dataModelFolder, executionContext);
			String dbmsLogoUrl = UIUtil.getDBMSLogoURL(ci.url);
			String img = dbmsLogoUrl == null? "": ("*" + dbmsLogoUrl);
			if (showOnlyRecentyUsedConnections) {
				data[i++] = new Object[] { img, ci.alias, ci.user, ci.url, ci.dataModelFolder == null? "Default" : modelDetails == null? "" : modelDetails.a, UIUtil.toDateAsString(aliasTimestamp.get(ci.alias)) };
			} else {
				data[i++] = new Object[] { img, ci.alias, ci.user, ci.url, ci.dataModelFolder == null? "Default" : modelDetails == null? "" : modelDetails.a };
			}
		}
		DefaultTableModel tableModel = new DefaultTableModel(data, !showOnlyRecentyUsedConnections? new String[] { "DBMS", "Name", "User", "URL", "Data Model" } : new String[] { "Name", "User", "URL", "Data Model", "Time" }) {
			@Override
			public boolean isCellEditable(int row, int column) {
				return false;
			}
			private static final long serialVersionUID = 1535384744352159695L;
		};
		@SuppressWarnings("unchecked")
		List<? extends SortKey> sortKeys = new ArrayList(connectionsTable.getRowSorter().getSortKeys());
		connectionsTable.setModel(tableModel);
		try {
			connectionsTable.getRowSorter().setSortKeys(sortKeys);
		} catch (Exception e) {
			// ignore
		}
		return data;
	}

	private boolean inRefresh = false;

	public static final String jdbcHelpURL = "http://jailer.sourceforge.net/doc/jdbc.html?src=app";

	/**
	 * Refreshes the dialog after model changes.
	 */
	private void refresh() {
		if (inRefresh) return;
		inRefresh = true;
		try {
			ConnectionInfo oldCurrentConnection = currentConnection;
			final int selectedRow = connectionsTable.getSelectedRow();
			Object[][] data = initTableModel();
			RowSorter<?> rowSorter = connectionsTable.getRowSorter();
			int selectedRowIndex = -1;
			if (selectedRow >= 0 && selectedRow < connectionList.size()) {
				selectedRowIndex = rowSorter.convertRowIndexToModel(selectedRow);
			}
			if (selectedRowIndex >= 0 && selectedRowIndex < connectionList.size()) {
				connectionsTable.getSelectionModel().setSelectionInterval(selectedRow, selectedRow);
				currentConnection = connectionList.get(selectedRowIndex);
			} else {
				currentConnection = null;
				isConnected = false;
			}
			for (int i = 0; i < connectionsTable.getColumnCount(); i++) {
				TableColumn column = connectionsTable.getColumnModel().getColumn(i);
				int width = 1;
				
				Component comp = connectionsTable.getDefaultRenderer(String.class).
										getTableCellRendererComponent(
												connectionsTable, column.getHeaderValue(),
												false, false, 0, i);
				width = Math.max(width, comp.getPreferredSize().width);
	
				for (int line = 0; line < data.length && line < 30; ++line) {
					comp = connectionsTable.getDefaultRenderer(String.class).
									 getTableCellRendererComponent(
											 connectionsTable, data[line][i],
										 false, false, line, i);
					width = Math.max(width, Math.min(220, comp.getPreferredSize().width));
				}
				
				column.setPreferredWidth(width);
			}
			editButton.setEnabled(currentConnection != null);
			deleteButton.setEnabled(currentConnection != null);
			copy.setEnabled(currentConnection != null);
			jButton1.setEnabled(currentConnection != null && selectedRowIndex >= 0 && selectedRowIndex < connectionList.size());
			warnOnConnect = !(selectedRowIndex < 0 || isAssignedToDataModel(selectedRowIndex));
			if (DataModelManager.getCurrentModelSubfolder(executionContext) == null && warnOnConnect) {
				warnOnConnect = false;
				jButton1.setEnabled(false);
			}
			jButton1.setIcon(!warnOnConnect? UIUtil.scaleIcon(jButton1, okIcon) : getScaledWarnIcon());
			if (oldCurrentConnection != null && currentConnection == null) {
				currentConnection = oldCurrentConnection;
			}
		} finally {
			inRefresh = false;
		}
	}

	private boolean warnOnConnect = false;

	/**
	 * File to store connections.
	 */
	public static String CONNECTIONS_FILE = "connections.csv";
	private static String CONNECTIONS_FILE_LEGACY = ".connections";

	/**
	 * Stores the connections into the CONNECTIONS_FILE.
	 */
	private void store() {
		if (showOnlyRecentyUsedConnections) {
			return;
		}
		try {
			File file = Environment.newFile(CONNECTIONS_FILE);
			saveAsCSV(connectionList, file);
			
			Environment.newFile(CONNECTIONS_FILE_LEGACY).renameTo(Environment.newFile(CONNECTIONS_FILE_LEGACY + ".obsolete"));
			
			if (!onConnectionListChangedPending) {
				onConnectionListChangedPending = true;
				UIUtil.invokeLater(() -> {
					onConnectionListChangedPending = false;
					onConnectionListChangedReload();
					notifyConnectionTypeChangeListener();
					UIUtil.invokeLater(() -> onConnectionListChangedAll());
				});
			}
			
		} catch (Exception e) {
			LogUtil.warn(e);
		}
	}
	
	private boolean onConnectionListChangedPending = false;
	
	private void onConnectionListChanged() {
		refresh();
		if (dataModelChanger != null) {
			dataModelChanger.onConnectionListChanged();
		}
	}
	
	private static void onConnectionListChangedReload() {
		allDialogs.forEach((a, b) -> a.loadConnectionList(false));
	}

	private static void onConnectionListChangedAll() {
		allDialogs.forEach((a, b) -> a.onConnectionListChanged());
	}

	private static Map<DbConnectionDialog, DbConnectionDialog> allDialogs = new WeakHashMap<DbConnectionDialog, DbConnectionDialog>();

	/**
	 * Loads connection list.
	 * @param showOnlyRecentyUsedConnections 
	 */
	@SuppressWarnings("unchecked")
	private void loadConnectionList(boolean showOnlyRecentyUsedConnections) {
		connectionList = new ArrayList<ConnectionInfo>();
		String currentAlias = currentConnection != null? currentConnection.alias : null;
		ConnectionInfo oldCurrentConnection = currentConnection;
		currentConnection = null;
		boolean ok = false;
		
		try {
			File file = Environment.newFile(CONNECTIONS_FILE);
			if (file.exists()) {
				connectionList = loadAsCSV(file);
				ok = true;
			} else {
				file = Environment.newFile(CONNECTIONS_FILE_LEGACY);
				if (file.exists()) {
					ObjectInputStream in = new ObjectInputStream(new FileInputStream(file)); // lgtm [java/input-resource-leak]
					List<ConnectionInfo> cis = (List<ConnectionInfo>) in.readObject();
					int i = in.readInt();
					boolean isEncrypted = true;
					try {
						List<String> dma = (List<String>) in.readObject();
						for (int n = 0; n < dma.size(); ++n) {
							ConnectionInfo connectionInfo = cis.get(n);
							if (!connectionInfo.decrypt()) {
								isEncrypted = false;
							}
							connectionInfo.dataModelFolder = dma.get(n);
							if (connectionInfo.jar1 == null) {
								connectionInfo.jar1 = "";
							}
							if (connectionInfo.jar2 == null) {
								connectionInfo.jar2 = "";
							}
							if (connectionInfo.jar3 == null) {
								connectionInfo.jar3 = "";
							}
							if (connectionInfo.jar4 == null) {
								connectionInfo.jar4 = "";
							}
							connectionInfo.jar1 = UIUtil.correctFileSeparator(connectionInfo.jar1);
							connectionInfo.jar2 = UIUtil.correctFileSeparator(connectionInfo.jar2);
							connectionInfo.jar3 = UIUtil.correctFileSeparator(connectionInfo.jar3);
							connectionInfo.jar4 = UIUtil.correctFileSeparator(connectionInfo.jar4);
						}
					} catch (Throwable t) {
						// ignore. pre 4.0 files do not contain data model assignments.
					}
					in.close();
					connectionList = cis;
					if (!isEncrypted) {
						// The access data is not yet encrypted. Save it in encrypted form.
						store();
					}
					ok = true;
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		if (!ok) {
			// migration of old settings
			try {
				File settingsFile = new File(".connect.ui");
				if (settingsFile.exists()) {
					ObjectInputStream in = new ObjectInputStream(
							new FileInputStream(settingsFile)); // lgtm [java/input-resource-leak]
					Map<String, Map<String, String>> settings = (Map<String, Map<String, String>>) in
							.readObject();
					in.close();
					for (String name : settings.keySet()) {
						if (settings.get(name).get("dbUser").trim().length() > 0) {
							ConnectionInfo ci = new ConnectionInfo(executionContext);
							ci.alias = name;
							ci.driverClass = settings.get(name).get("driver");
							ci.user = settings.get(name).get("dbUser");
							ci.url = settings.get(name).get("dbUrl");
							ci.password = settings.get(name).get("password");
							ci.jar1 = settings.get(name).get("jar1");
							ci.jar2 = settings.get(name).get("jar2");
							connectionList.add(ci);
						}
					}
				}
			} catch (Throwable e) {
				// ignore
			}
		}
		if (connectionList.size() == 0) {
			ConnectionInfo ci = new ConnectionInfo(executionContext);
			ci.alias = "Demo Scott";
			ci.driverClass = "org.h2.Driver";
			ci.jar1 = "lib" + File.separator + "h2-2.1.212.jar";
			ci.url = "jdbc:h2:" + Environment.newFile("demo-scott").getAbsolutePath();
			ci.user = "sa";
			ci.password = "";
			ci.dataModelFolder = "Demo-Scott";
			connectionList.add(ci);
			
			ci = new ConnectionInfo(executionContext);
			ci.alias = "Demo Sakila";
			ci.driverClass = "org.h2.Driver";
			ci.jar1 = "lib" + File.separator + "h2-2.1.212.jar";
			ci.url = "jdbc:h2:" + Environment.newFile("demo-sakila").getAbsolutePath();
			ci.user = "sa";
			ci.password = "";
			ci.dataModelFolder = "Demo-Sakila";
			connectionList.add(ci);
			store();
		}
		sortConnectionList();
		
		if (showOnlyRecentyUsedConnections) {
			List<ConnectionInfo> recUsedConnectionList = new ArrayList<ConnectionInfo>();
			
			for (Pair<String, Date> alias: UISettings.loadRecentConnectionAliases()) {
				for (ConnectionInfo ci: connectionList) {
					if (ci.alias != null && ci.alias.equals(alias.a)) {
						recUsedConnectionList.add(ci);
						aliasTimestamp.put(alias.a, alias.b);
						break;
					}
				}
			}
			
			connectionList = recUsedConnectionList;
		}

		if (currentAlias != null) {
			connectionList.stream().filter(ci -> currentAlias.equals(ci.alias)).findAny().ifPresent(ci -> currentConnection = ci);
		}
		if (oldCurrentConnection != null && currentConnection == null) {
			currentConnection = oldCurrentConnection;
		}
	}

	private void sortConnectionList() {
		ConnectionInfo sci = null;
		if (connectionsTable != null) {
			try {
				RowSorter<?> rowSorter = connectionsTable.getRowSorter();
				int selectedRowIndex = connectionsTable.getSelectedRow();
				if (selectedRowIndex >= 0) {
					sci = connectionList.get(rowSorter.convertRowIndexToModel(selectedRowIndex));
				}
			} catch (Exception e) {
				// ignore
			}
		}
		Collections.sort(connectionList, new Comparator<ConnectionInfo>() {
			@Override
			public int compare(ConnectionInfo o1, ConnectionInfo o2) {
				if (dataModelAware) {
					if (DataModelManager.getCurrentModelSubfolder(executionContext) != null) {
						boolean c1 = DataModelManager.getCurrentModelSubfolder(executionContext)
								.equals(o1.dataModelFolder);
						boolean c2 = DataModelManager.getCurrentModelSubfolder(executionContext)
								.equals(o2.dataModelFolder);

						if (c1 && !c2)
							return -1;
						if (!c1 && c2)
							return 1;
					}
				} else {
					boolean c1 = isAssignedToDataModel(o1.dataModelFolder);
					boolean c2 = isAssignedToDataModel(o2.dataModelFolder);

					if (c1 && !c2)
						return -1;
					if (!c1 && c2)
						return 1;
				}
				return o1.alias.compareToIgnoreCase(o2.alias);
			}
		});
		if (connectionsTable != null) {
			try {
				if (sci != null) {
					int selectedRowIndex = connectionList.indexOf(sci);
					if (selectedRowIndex >= 0) {
						RowSorter<?> rowSorter = connectionsTable.getRowSorter();
						selectedRowIndex = rowSorter.convertRowIndexToView(selectedRowIndex);
						if (selectedRowIndex >= 0) {
							connectionsTable.getSelectionModel().setSelectionInterval(selectedRowIndex,
									selectedRowIndex);
							connectionsTable
									.scrollRectToVisible(connectionsTable.getCellRect(selectedRowIndex, 0, true));
						}
					}
				}
			} catch (Exception e) {
				// ignore
			}
		}
	}

	/**
	 * This method is called from within the constructor to initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is always
	 * regenerated by the Form Editor.
	 */
	// <editor-fold defaultstate="collapsed"
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jPanel1 = new javax.swing.JPanel();
        mainPanel = new javax.swing.JPanel();
        jPanel2 = new javax.swing.JPanel();
        restoreLastSessionButton = new javax.swing.JButton();
        closeButton = new javax.swing.JButton();
        jButton1 = new javax.swing.JButton();
        jLabel2 = new javax.swing.JLabel();
        jPanel3 = new javax.swing.JPanel();
        newButton = new javax.swing.JButton();
        editButton = new javax.swing.JButton();
        copy = new javax.swing.JButton();
        deleteButton = new javax.swing.JButton();
        jLabel1 = new javax.swing.JLabel();
        importButton = new javax.swing.JButton();
        exportButton = new javax.swing.JButton();
        infoBarLabel = new javax.swing.JLabel();
        borderPanel = new javax.swing.JPanel();
        jScrollPane2 = new javax.swing.JScrollPane();
        connectionsTable = new javax.swing.JTable();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Connect with DB");
        getContentPane().setLayout(new java.awt.CardLayout());

        jPanel1.setLayout(new java.awt.GridBagLayout());

        mainPanel.setLayout(new java.awt.GridBagLayout());

        jPanel2.setLayout(new java.awt.GridBagLayout());

        restoreLastSessionButton.setText("Restore last Session");
        restoreLastSessionButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                restoreLastSessionButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHEAST;
        gridBagConstraints.insets = new java.awt.Insets(4, 2, 0, 16);
        jPanel2.add(restoreLastSessionButton, gridBagConstraints);

        closeButton.setText("Cancel");
        closeButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                closeButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHEAST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 4);
        jPanel2.add(closeButton, gridBagConstraints);

        jButton1.setText("Connect");
        jButton1.setEnabled(false);
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHEAST;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 0, 2);
        jPanel2.add(jButton1, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.weightx = 1.0;
        jPanel2.add(jLabel2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 100;
        gridBagConstraints.gridwidth = 20;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        mainPanel.add(jPanel2, gridBagConstraints);

        jPanel3.setLayout(new java.awt.GridBagLayout());

        newButton.setText(" New ");
        newButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                newButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 2, 0);
        jPanel3.add(newButton, gridBagConstraints);

        editButton.setText(" Edit ");
        editButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                editButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 11;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 2, 0);
        jPanel3.add(editButton, gridBagConstraints);

        copy.setText("Clone");
        copy.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                copyActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 2, 0);
        jPanel3.add(copy, gridBagConstraints);

        deleteButton.setText("    Delete    ");
        deleteButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                deleteButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 30;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 2, 0);
        jPanel3.add(deleteButton, gridBagConstraints);

        jLabel1.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 9;
        jPanel3.add(jLabel1, gridBagConstraints);

        importButton.setText("  Import  ");
        importButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                importButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 42;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(2, 4, 2, 0);
        jPanel3.add(importButton, gridBagConstraints);

        exportButton.setText("  Export  ");
        exportButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                exportButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 40;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(16, 4, 0, 0);
        jPanel3.add(exportButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 4);
        mainPanel.add(jPanel3, gridBagConstraints);

        infoBarLabel.setText("info bar");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 11;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        mainPanel.add(infoBarLabel, gridBagConstraints);

        borderPanel.setBorder(javax.swing.BorderFactory.createTitledBorder("Connections"));
        borderPanel.setLayout(new java.awt.GridBagLayout());

        connectionsTable.setModel(new javax.swing.table.DefaultTableModel(
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
        jScrollPane2.setViewportView(connectionsTable);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        borderPanel.add(jScrollPane2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        mainPanel.add(borderPanel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        jPanel1.add(mainPanel, gridBagConstraints);

        getContentPane().add(jPanel1, "card2");
        jPanel1.getAccessibleContext().setAccessibleName("");

        pack();
    }// </editor-fold>//GEN-END:initComponents

	private void copyActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_copyActionPerformed
		if (currentConnection != null) {
			int i = connectionList.indexOf(currentConnection);
			if (i >= 0) {
				for (int nr = 1; ; ++nr) {
					String newAlias = currentConnection.alias.replaceFirst("\\s*\\(\\d+\\)$", "") + (nr > 1? " (" + nr + ")" : "");
					boolean found = false;
					for (ConnectionInfo ci: connectionList) {
						if (ci.alias.equals(newAlias)) {
							found = true;
							break;
						}
					}
					if (!found) {
						ConnectionInfo ci = new ConnectionInfo(executionContext);
						ci.alias = newAlias;
						ci.driverClass = currentConnection.driverClass;
						ci.jar1 = currentConnection.jar1;
						ci.jar2 = currentConnection.jar2;
						ci.jar3 = currentConnection.jar3;
						ci.jar4 = currentConnection.jar4;
						ci.password = currentConnection.password;
						ci.url = currentConnection.url;
						ci.user = currentConnection.user;
						ci.connectionType = currentConnection.connectionType;
						ci.connectionTypeName = currentConnection.connectionTypeName;
						if (edit(ci, false, true)) {
							connectionList.add(i + 1, ci);
							refresh();
							RowSorter<?> rowSorter = connectionsTable.getRowSorter();
							int selectedRowIndex = rowSorter.convertRowIndexToView(i + 1);
							connectionsTable.getSelectionModel().setSelectionInterval(selectedRowIndex, selectedRowIndex);
							connectionsTable.scrollRectToVisible(connectionsTable.getCellRect(selectedRowIndex, 0, true));
							store();
						}
						break;
					}
				}
			}
		}
	}//GEN-LAST:event_copyActionPerformed

	private void deleteButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_deleteButtonActionPerformed
		if (currentConnection != null) {
			int i = connectionList.indexOf(currentConnection);
			if (i >= 0) {
				if (JOptionPane.showConfirmDialog(mainPanel, "Delete '" + currentConnection.alias + "'?", "Delete", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE) == JOptionPane.YES_OPTION) {
					connectionList.remove(i);
					connectionsTable.getSelectionModel().clearSelection();
					refresh();
					store();
				}
			}
		}
	}//GEN-LAST:event_deleteButtonActionPerformed

	private void newButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_newButtonActionPerformed
		ConnectionInfo ci = new ConnectionInfo(executionContext);
		List<Line> lines = UIUtil.loadDriverList(this);
		try {
			Component root = SwingUtilities.getWindowAncestor(mainPanel);
			if (root == null) {
				root = mainPanel;
			}
			Pair<String, String> s = new DbConnectionSettings(root).edit(lines);
			
			if (s == null) return;
			for (Line line: lines) {
				if (line.cells.get(0).equals(s.a)) {
					ci.url = s.b;
					if (ci.url == null) {
						return;
					}
					ci.driverClass = line.cells.get(2);
					String[] jars = line.cells.get(3).replace("/", File.separator).split(" ");
					if (jars.length > 0) {
						ci.jar1 = jars[0];
					}
					if (jars.length > 1) {
						ci.jar2 = jars[1];
					}
					ci.alias = ""; // s.a;
					break;
				}
			}
		} catch (Exception e) {
			UIUtil.showException(this, "Error", e);
		}
		if (edit(ci, true, false)) {
			for (int nr = 1; ; ++nr) {
				String newAlias = ci.alias + (nr > 1? " (" + nr + ")" : "");
				boolean found = false;
				for (ConnectionInfo ci2: connectionList) {
					if (newAlias.equals(ci2.alias)) {
						found = true;
						break;
					}
				}
				if (!found) {
					ci.alias = newAlias;
					connectionList.add(0, ci);
					int i = 0;
					connectionsTable.getSelectionModel().setSelectionInterval(i, i);
					connectionsTable.scrollRectToVisible(connectionsTable.getCellRect(i, 0, true));
					refresh();
					store();
					break;
				}
			}
		}
	}//GEN-LAST:event_newButtonActionPerformed

	private void editButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_editButtonActionPerformed
		if (currentConnection == null) return;
		if (edit(currentConnection, false, false)) {
			refresh();
			store();
		}
	}//GEN-LAST:event_editButtonActionPerformed

	private void closeButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_closeButtonActionPerformed
		isConnected = false;
		setVisible(false);
	}//GEN-LAST:event_closeButtonActionPerformed

    private void restoreLastSessionButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_restoreLastSessionButtonActionPerformed

    }//GEN-LAST:event_restoreLastSessionButtonActionPerformed

	public void importConnections(Window owner) {
		Pair<Boolean, Pair<Integer, List<ConnectionInfo>>> result = ExportPanel.openImportDialog(owner, this);
		if (result.a) {
			if (result.b.a > 0) {
				notifyDataModelsChanged();
			}

			if (result.b.b != null) {
				for (ConnectionInfo ci: result.b.b) {
					boolean found = false;
					for (int i = 0; i < connectionList.size(); ++i) {
						if (connectionList.get(i).alias.equals(ci.alias)) {
							found = true;
							connectionList.set(i, ci);
							break;
						}
					}
					if (!found) {
						connectionList.add(ci);
					}
				}
				store();
			}
			UIUtil.invokeLater(() -> {
				onConnectionListChangedReload();
				notifyConnectionTypeChangeListener();
				UIUtil.invokeLater(() -> onConnectionListChangedAll());
			});
		}
	}

    private void importButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_importButtonActionPerformed
    	importConnections(SwingUtilities.getWindowAncestor(importButton));
    }//GEN-LAST:event_importButtonActionPerformed

	public void exportConnections(Window owner, Object initiallySelected) {
		ExportPanel exportPanel = new ExportPanel();
		exportPanel.openExportDialog(owner, initiallySelected, this);
	}

    private void exportButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_exportButtonActionPerformed
        exportConnections(SwingUtilities.getWindowAncestor(importButton), currentConnection);
    }//GEN-LAST:event_exportButtonActionPerformed

	/**
	 * Opens detail editor for a connection.
	 * 
	 * @param ci the connection
	 * @param forNew 
	 * @return <code>true</code> if connection has been edited
	 */
	private boolean edit(ConnectionInfo ci, boolean forNew, boolean forClone) {
		return new DbConnectionDetailsEditor(parent, jdbcHelpURL, forNew, forClone, dataModelAware).edit(ci, connectionList, executionContext);
	}

	private void connect() {
		if (currentConnection == null) {
			return;
		}
		
		if (warnOnConnect) {
			boolean noDM = DataModelManager.getCurrentModelSubfolder(executionContext) == null;
			if (JOptionPane.YES_OPTION != JOptionPane.showConfirmDialog(mainPanel, "Connection is not associated with " + (noDM? "any data model." : ("data model \"" + DataModelManager.getCurrentModelSubfolder(executionContext) + "\".")) + "\n"
					+ (noDM || dataModelChanger == null? "Do you still want to connect?" : "Do you want to change data model and connect?"), "Connect", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE)) {
				return;
			}
		}
		
		isConnected = false;

		Component root = SwingUtilities.getWindowAncestor(mainPanel);
		if (root == null) {
			root = mainPanel;
		}
		try {
			UIUtil.setWaitCursor(root);
			if (testConnection(mainPanel, currentConnection, null)) {
				isConnected = true;
				ok = true;
				executionContext.setCurrentConnectionAlias(currentConnection.alias);
				if (dataModelChanger != null && DataModelManager.getCurrentModelSubfolder(executionContext) != null && warnOnConnect) {
					dataModelChanger.change(currentConnection.dataModelFolder);
				}
				onConnect(currentConnection);
				if (dataModelChanger != null) {
					dataModelChanger.afterConnect(currentConnection);
				}
				if (currentConnection.alias != null && !"".equals(currentConnection.alias)) {
					UISettings.addRecentConnectionAliases(currentConnection.alias);
				}
			}
		} finally {
			UIUtil.resetWaitCursor(root);
		}
	}

	protected void onConnect(ConnectionInfo currentConnection) {
		setVisible(false);
	}

	private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {                                         
		connect();
	}// GEN-LAST:event_jButton1ActionPerformed

	public static boolean testConnection(Component parent, ConnectionInfo ci, JButton downloadButton) {
		String d1 = ci.jar1.trim();
		String d2 = ci.jar2.trim();
		String d3 = ci.jar3.trim();
		String d4 = ci.jar4.trim();
		if (d1.length() == 0) {
			d1 = null;
		}
		if (d2.length() == 0) {
			d2 = null;
		}
		if (d3.length() == 0) {
			d3 = null;
		}
		if (d4.length() == 0) {
			d4 = null;
		}
		URL[] urls;
		try {
			urls = ClasspathUtil.toURLArray(d1, d2, d3, d4);
		} catch (Throwable e) {
			if (e instanceof FileNotFoundException) {
				UIUtil.showException(parent, "Unable to connect", new FileNotFoundException(e.getMessage()
						+ (downloadButton == null? "" : ".\nYou can download the driver with the button \"Download Driver\".")), UIUtil.EXCEPTION_CONTEXT_MB_USER_ERROR, downloadButton);
			} else {
				UIUtil.showException(parent, "Error loading driver jars", e, UIUtil.EXCEPTION_CONTEXT_USER_ERROR);
			}
			return false;
		}

		try {
			Window w = parent instanceof Window? (Window) parent : SwingUtilities.getWindowAncestor(parent);
			BasicDataSource dataSource = UIUtil.createBasicDataSource(w, ci.driverClass, ci.url, ci.user, ci.password, 0, urls);
			SessionForUI session = SessionForUI.createSession(dataSource, dataSource.dbms, null, true, false, true, w, null);
			if (session != null) {
				session.shutDown();
				try {
					UISettings.s10 = ci.url.replaceAll("[^:]*:([^:]*):.*", "$1");
					UISettings.s10 = UISettings.s10.substring(0, Math.min(UISettings.s10.length(), 100));
				} catch (Throwable t) {
					// ignore
				}
				return true;
			}
			return false;
		} catch (Throwable e) {
			if (e.getCause() instanceof ClassNotFoundException && e.getCause().getMessage() != null && e.getCause().getMessage().contains(ci.driverClass)) {
				UIUtil.showException(parent, "Unable to connect", new ClassNotFoundException("JDBC driver class not found: '" + ci.driverClass + "'"
						+ (downloadButton == null? "" : ".\nYou can download the driver with the button \"Download Driver\".")), UIUtil.EXCEPTION_CONTEXT_MB_USER_ERROR, downloadButton);
			} else {
				UIUtil.showException(parent, "Unable to connect (" + (e.getClass().getSimpleName()) + ")", e, UIUtil.EXCEPTION_CONTEXT_MB_USER_ERROR);
			}
			return false;
		}
	}

	/**
	 * Gets all DB schemas.
	 * 
	 * @param defaultSchema array of size 1 to put default schema into (null if no schema exists)
	 * @return all DB schemas
	 */
	public List<String> getDBSchemas(String[] defaultSchema) throws Exception {
		BasicDataSource dataSource = new BasicDataSource(currentConnection.driverClass,
				currentConnection.url, currentConnection.user,
				currentConnection.password, 0, ClasspathUtil.toURLArray(currentConnection.jar1, currentConnection.jar2, currentConnection.jar3, currentConnection.jar4));
		Session session = new Session(dataSource, dataSource.dbms, executionContext.getIsolationLevel());
		List<String> schemas = JDBCMetaDataBasedModelElementFinder.getSchemas(
				session, currentConnection.user);
		defaultSchema[0] = JDBCMetaDataBasedModelElementFinder
				.getDefaultSchema(session, currentConnection.user);
		session.shutDown();
		return schemas;
	}

	/**
	 * Adds jailer cli-arguments for DB connection.
	 * 
	 * @param args
	 *            the arg-list to add arguments to
	 */
	public void addDbArgs(List<String> args) {
		args.add(currentConnection.driverClass);
		args.add(currentConnection.url);
		args.add(currentConnection.user);
		args.add(currentConnection.password);
		if (currentConnection.jar1.trim().length() > 0) {
			args.add("-jdbcjar");
			args.add(currentConnection.jar1.trim());
		}
		if (currentConnection.jar2.trim().length() > 0) {
			args.add("-jdbcjar2");
			args.add(currentConnection.jar2.trim());
		}
		if (currentConnection.jar3.trim().length() > 0) {
			args.add("-jdbcjar3");
			args.add(currentConnection.jar3.trim());
		}
		if (currentConnection.jar4.trim().length() > 0) {
			args.add("-jdbcjar4");
			args.add(currentConnection.jar4.trim());
		}
	}

    // Variables declaration - do not modify//GEN-BEGIN:variables
    javax.swing.JPanel borderPanel;
    javax.swing.JButton closeButton;
    private javax.swing.JTable connectionsTable;
    private javax.swing.JButton copy;
    private javax.swing.JButton deleteButton;
    private javax.swing.JButton editButton;
    private javax.swing.JButton exportButton;
    private javax.swing.JButton importButton;
    private javax.swing.JLabel infoBarLabel;
    private javax.swing.JButton jButton1;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JScrollPane jScrollPane2;
    public javax.swing.JPanel mainPanel;
    private javax.swing.JButton newButton;
    public javax.swing.JButton restoreLastSessionButton;
    // End of variables declaration//GEN-END:variables

	public String getUser() {
		return currentConnection.user;
	}

	public String getPassword() {
		return currentConnection.password;
	}

	private boolean isAssignedToDataModel(int row) {
		if (row < 0 || row >= connectionList.size()) {
			return false;
		}
		String rowFN = connectionList.get(row).dataModelFolder;
		return isAssignedToDataModel(rowFN);
	}

	protected boolean isAssignedToDataModel(String dataModelFolder) {
		String fn = DataModelManager.getCurrentModelSubfolder(executionContext);
		return fn == null && dataModelFolder == null || (fn != null && fn.equals(dataModelFolder));
	}

	private static final long serialVersionUID = -3983034803834547687L;

	public URL[] currentJarURLs() throws Exception {
		if (currentConnection != null) {
			return ClasspathUtil.toURLArray(currentConnection.jar1, currentConnection.jar2, currentConnection.jar3, currentConnection.jar4);
		}
		return null;
	}

    private ImageIcon getScaledWarnIcon() {
        if (scaledWarnIcon == null && warnIcon != null) {
            int heigth = getFontMetrics(new JLabel("M").getFont()).getHeight();
            double s = heigth / (double) warnIcon.getIconHeight();
            try {
            	scaledWarnIcon = UIUtil.scaleIcon(warnIcon, s);
            } catch (Exception e) {
            	logger.info("error", e);
                return null;
            }
        }
        return scaledWarnIcon;
    }

    /**
     * Try to automatically connect to the db specified via CLI.
     */
	public void autoConnect() {
		ConnectionInfo ci = new ConnectionInfo();
		UICommandLine cli = CommandLineInstance.getInstance();
		
		ci.alias = cli.alias != null? cli.alias : cli.url;
		ci.dataModelFolder = cli.datamodelFolder;
		ci.driverClass = cli.driver;
		ci.jar1 = cli.jdbcjar != null? cli.jdbcjar : "";
		ci.jar2 = cli.jdbcjar2 != null? cli.jdbcjar2 : "";
		ci.jar3 = cli.jdbcjar3 != null? cli.jdbcjar3 : "";
		ci.jar4 = cli.jdbcjar4 != null? cli.jdbcjar4 : "";
		ci.url = cli.url;
		ci.user = cli.user;
		ci.password = cli.password != null? cli.password : "";
		
		if (ci.driverClass != null
				&& ci.url != null
				&& ci.user != null) {
				ci.setConnectionType(retrieveConnectionType(ci));
				currentConnection = ci;
				executionContext.setCurrentConnectionAlias(currentConnection.alias);
				isConnected = true;
		}
	}

	public void selectFirstConnection() {
		if (connectionsTable.getModel().getRowCount() > 0) {
			connectionsTable.getSelectionModel().setSelectionInterval(0, 0);
			jButton1.grabFocus();
		}
	}
	
	public void setParent(Window parent) {
		this.parent = parent;
	}

	public void selectAlias(String alias) {
		for (int j = 0; j < connectionList.size(); ++j) {
			if (alias != null && alias.equals(connectionList.get(j).alias)) {
				connectionsTable.getSelectionModel().setSelectionInterval(j, j);
				connectionsTable.scrollRectToVisible(connectionsTable.getCellRect(j, 0, true));
				break;
			}
		}
	}
	
	public void doEdit(ConnectionInfo ci) {
		selectAlias(ci.alias);
		editButton.doClick(1);
	}

	public void doClone(ConnectionInfo ci) {
		selectAlias(ci.alias);
		copy.doClick(1);
	}

	public void doDelete(ConnectionInfo ci) {
		selectAlias(ci.alias);
		deleteButton.doClick(1);
	}


	private static ImageIcon warnIcon;
    private static ImageIcon scaledWarnIcon;
    private static ImageIcon okIcon;
	private static ImageIcon cancelIcon;
	
    static {
        // load images
    	warnIcon = UIUtil.readImage("/wanr.png");
	  	okIcon = UIUtil.readImage("/buttonok.png");
        cancelIcon = UIUtil.readImage("/buttoncancel.png");
	}

	public ConnectionType retrieveConnectionType(ConnectionInfo ci) {
		List<ConnectionType> result = connectionList.stream().filter(c -> ci.url.equals(c.url) && ci.user.equals(c.user))
				.map(c -> c.getConnectionType()).collect(Collectors.toList());
		if (result.size() == 1) {
			return result.get(0);
		}
		result = connectionList.stream().filter(c -> ci.alias.equals(c.alias))
				.map(c -> c.getConnectionType()).collect(Collectors.toList());
		if (result.size() == 1) {
			return result.get(0);
		}
		ConnectionType ciType = ci.getConnectionType();
		if (ciType != null) {
			return ciType;
		}
		return ConnectionType.Development;
	}
	
	public interface ConnectionTypeChangeListener {
		void onConnectionTypeChange();
		void onDataModelsChanged();
	}
	
	private static void notifyConnectionTypeChangeListener() {
		for (Window w : Window.getWindows()) {
			if (w instanceof ConnectionTypeChangeListener) {
				((ConnectionTypeChangeListener) w).onConnectionTypeChange();
			}
		}
	}
	
	private static void notifyDataModelsChanged() {
		for (Window w : Window.getWindows()) {
			if (w instanceof ConnectionTypeChangeListener) {
				((ConnectionTypeChangeListener) w).onDataModelsChanged();
			}
		}
	}
	
	private static String OBFUSCATED_MARKER_PREFIX = "encrypted:";
	// TODO
	// TODO test non-obf. connections.csv
	
	public static void saveAsCSV(Collection<ConnectionInfo> infos, File file) throws IOException {
		StringObfuscator stringObfuscator = new StringObfuscator();
		BufferedWriter writer = new BufferedWriter(new FileWriter(file));
		writer.append("# Jailer Database connections." + UIUtil.LINE_SEPARATOR);
		writer.append(UIUtil.LINE_SEPARATOR);
		writer.append("# Format: CVS, columns are separated by ';'" + UIUtil.LINE_SEPARATOR);
		writer.append("#         Lines starting with '#' and empty lines are comments" + UIUtil.LINE_SEPARATOR);
		writer.append("#         Escape sequences: '\"' -> '\\\"' and '\\' -> '\\\\'" + UIUtil.LINE_SEPARATOR);
		writer.append("#         Passwords and URLs starting with '" + OBFUSCATED_MARKER_PREFIX + "' are encrypted." + UIUtil.LINE_SEPARATOR);
		writer.append("#         You may use non-encrypted text here by omitting the prefix." + UIUtil.LINE_SEPARATOR);
		writer.append(UIUtil.LINE_SEPARATOR);
		writer.append("# Name; User; Data model folder; Connection type (Development/Test/Staging/Production or empty); URL; Password; Driver class; Driver JAR; additional JAR 1; additional JAR 2; additional JAR 3; additional JAR 4;" + UIUtil.LINE_SEPARATOR);
		for (ConnectionInfo ci: infos) {
			writer.append(CsvFile.encodeCell(ci.alias) + "; ");
			writer.append(CsvFile.encodeCell(ci.user) + "; ");
			writer.append(CsvFile.encodeCell(ci.dataModelFolder) + "; ");
			writer.append(CsvFile.encodeCell(ci.getConnectionType().name()) + "; ");
			writer.append(OBFUSCATED_MARKER_PREFIX + CsvFile.encodeCell(stringObfuscator.encrypt(ci.url)) + "; ");
			writer.append(OBFUSCATED_MARKER_PREFIX + CsvFile.encodeCell(stringObfuscator.encrypt(ci.password)) + "; ");
			writer.append(CsvFile.encodeCell(ci.driverClass) + "; ");
			writer.append(CsvFile.encodeCell(ci.jar1) + "; ");
			writer.append(CsvFile.encodeCell(ci.jar2) + "; ");
			writer.append(CsvFile.encodeCell(ci.jar3) + "; ");
			writer.append(CsvFile.encodeCell(ci.jar4) + UIUtil.LINE_SEPARATOR);
		}
		writer.close();
	}
	
	public static List<ConnectionInfo> loadAsCSV(File file) throws IOException {
		StringObfuscator stringObfuscator = new StringObfuscator();
		List<ConnectionInfo> list = new ArrayList<>();
		CsvFile csvFile = new CsvFile(file);
		for (Line line: csvFile.getLines()) {
			if (!line.cells.get(0).isEmpty() && !line.cells.get(0).matches("^\"#.*\"$")) {
				ConnectionInfo ci = new ConnectionInfo();
				ci.alias = line.cells.get(0);
				ci.user = line.cells.get(1);
				ci.dataModelFolder = line.cells.get(2);
				ci.connectionTypeName = line.cells.get(3);
				ci.url = line.cells.get(4);
				if (ci.url.startsWith(OBFUSCATED_MARKER_PREFIX)) {
					ci.url = stringObfuscator.decrypt(ci.url.substring(OBFUSCATED_MARKER_PREFIX.length()));
				}
				ci.password = line.cells.get(5);
				if (ci.password.startsWith(OBFUSCATED_MARKER_PREFIX)) {
					ci.password = stringObfuscator.decrypt(ci.password.substring(OBFUSCATED_MARKER_PREFIX.length()));
				}
				ci.driverClass = line.cells.get(6);
				ci.jar1 = line.cells.get(7);
				ci.jar2 = line.cells.get(8);
				ci.jar3 = line.cells.get(9);
				ci.jar4 = line.cells.get(10);
				list.add(ci);
			}
		}
		return list;
	}

}


// TODO 1
// TODO programmatic SLL certificate import?
// TODO https://stackoverflow.com/questions/18889058/programmatically-import-ca-trust-cert-into-existing-keystore-file-without-using

