/*
 * Copyright 2007 - 2018 the original author or authors.
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
import java.awt.Cursor;
import java.awt.Image;
import java.awt.Point;
import java.awt.Window;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.RowSorter;
import javax.swing.RowSorter.SortKey;
import javax.swing.SwingUtilities;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import org.apache.log4j.Logger;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.database.BasicDataSource;
import net.sf.jailer.database.Session;
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;
import net.sf.jailer.ui.databrowser.metadata.MetaDataPanel;
import net.sf.jailer.util.ClasspathUtil;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.CsvFile.Line;
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
	private static final Logger logger = Logger.getLogger(MetaDataPanel.class);

	/**
	 * <code>true</code> if valid connection is available.
	 */
	public boolean isConnected = false;

	/**
	 * Holds connection information.
	 */
	public static class ConnectionInfo implements Serializable, Cloneable {
		@Override
		protected Object clone() throws CloneNotSupportedException {
			return super.clone();
		}
		private static final long serialVersionUID = -8034755966212631808L;
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
	}

	/**
	 * List of available connections.
	 */
	private List<ConnectionInfo> connectionList;

	/**
	 * Currently selected connection.
	 */
	public ConnectionInfo currentConnection;

	/**
	 * The parent frame.
	 */
	private final java.awt.Frame parent;
	
	private final ExecutionContext executionContext;
	
	/**
	 * Gets connection to DB.
	 * 
	 * @return <code>true</code> if connection succeeded
	 */
	public boolean connect(String reason) {
		return connect(reason, false);
	}

	/**
	 * Gets connection to DB.
	 * 
	 * @return <code>true</code> if connection succeeded
	 */
	public boolean connect(String reason, boolean keepState) {
		boolean oldIsConnected = isConnected;
		ConnectionInfo oldCurrentConnection = currentConnection;
		try {
			setTitle((reason == null ? "" : (reason + " - ")) + "Connect.");
			sortConnectionList();
			refresh();
			setVisible(true);
			if (currentConnection == null) {
				isConnected = false;
			}
			return isConnected;
		} finally {
			if (keepState && !isConnected) {
				isConnected = oldIsConnected;
				currentConnection = oldCurrentConnection;
			}
		}
	}

	private final InfoBar infoBar;
	private final String currentModelSubfolder;
	private final boolean dataModelAware;
	
	/** Creates new form DbConnectionDialog */
	public DbConnectionDialog(java.awt.Frame parent, DbConnectionDialog other, String applicationName, ExecutionContext executionContext) {
		this(parent, applicationName, other.infoBar == null? null : new InfoBar(other.infoBar), executionContext);
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
	public DbConnectionDialog(java.awt.Frame parent, String applicationName, InfoBar infoBar, ExecutionContext executionContext) {
		this(parent, applicationName, infoBar, executionContext, true);
	}
	
	/** 
	 * Creates new form DbConnectionDialog
	 * 
	 * @param applicationName application name. Used to create the name of the demo database alias. 
	 */
	public DbConnectionDialog(java.awt.Frame parent, String applicationName, InfoBar infoBar, ExecutionContext executionContext, boolean dataModelAware) {
		super(parent, true);
		this.executionContext = executionContext;
		this.parent = parent;
		this.infoBar = infoBar;
		this.currentModelSubfolder = DataModelManager.getCurrentModelSubfolder(executionContext);
		this.dataModelAware = dataModelAware;
		loadConnectionList();
		initComponents();
		connectionsTable.setAutoCreateRowSorter(true);

		if (infoBar == null) {
			infoBar = new InfoBar("Connect with Database", 
					"Select a connection to the database, or create a new connection.\n" +
					"New connections will be assigned to the datamodel \"" + DataModelManager.getModelDetails(DataModelManager.getCurrentModelSubfolder(executionContext), executionContext).a + "\".", null);
		}
		
		UIUtil.replace(infoBarLabel, infoBar);
		
		newButton.setVisible(dataModelAware);
		copy.setVisible(dataModelAware);
		
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
										isSelected, hasFocus, row, column);
						if (render instanceof JLabel) {
							if (!isSelected) {
								final Color BG1 = new Color(255, 255, 255);
								final Color BG2 = new Color(242, 255, 242);
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
						}
						((JLabel) render).setToolTipText(String.valueOf(value));
						return render;
					}
				});
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
		
		setLocation(100, 150);
		pack();
		setSize(Math.max(710, getWidth()), 450);
		refresh();
		UIUtil.initPeer();
	}

	/**
	 * Initializes the table model.
	 */
	private Object[][] initTableModel() {
		Object[][] data = new Object[connectionList.size()][];
		int i = 0;
		for (ConnectionInfo ci: connectionList) {
			Pair<String, Long> modelDetails = DataModelManager.getModelDetails(ci.dataModelFolder, executionContext);
			data[i++] = new Object[] { ci.alias, ci.user, ci.url, ci.dataModelFolder == null? "Default" : modelDetails == null? "" : modelDetails.a };
		}
		DefaultTableModel tableModel = new DefaultTableModel(data, new String[] { "Alias", "User", "URL", "Data Model" }) {
			@Override
			public boolean isCellEditable(int row, int column) {
				return false;
			}
			private static final long serialVersionUID = 1535384744352159695L;
		};
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
			int selectedRow = connectionsTable.getSelectedRow();
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
	
				for (int line = 0; line < data.length; ++line) {
					comp = connectionsTable.getDefaultRenderer(String.class).
									 getTableCellRendererComponent(
											 connectionsTable, data[line][i],
										 false, false, line, i);
					width = Math.max(width, Math.min(250, comp.getPreferredSize().width));
				}
				
				column.setPreferredWidth(width);
			}
			editButton.setEnabled(currentConnection != null);
			deleteButton.setEnabled(currentConnection != null);
			copy.setEnabled(currentConnection != null);
			jButton1.setEnabled(currentConnection != null && selectedRowIndex >= 0 && selectedRowIndex < connectionList.size());
			warnOnConnect = !(selectedRowIndex < 0 || isAssignedToDataModel(selectedRowIndex));
			if (currentModelSubfolder == null && warnOnConnect) {
				warnOnConnect = false;
				jButton1.setEnabled(false);
			} else {
				jButton1.setEnabled(true);
			}
			jButton1.setIcon(!warnOnConnect? null : getScaledWarnIcon());
		} finally {
			inRefresh = false;
		}
	}

	private boolean warnOnConnect = false;

	/**
	 * File to store connections.
	 */
	private static String CONNECTIONS_FILE = ".connections";

	/**
	 * Stores the connections into the CONNECTIONS_FILE.
	 */
	private void store() {
		try {
			File file = Environment.newFile(CONNECTIONS_FILE);
			ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(file));
			out.writeObject(connectionList);
			out.writeInt(connectionList.indexOf(currentConnection));
			List<String> dataModels = new ArrayList<String>();
			for (ConnectionInfo ci: connectionList) {
				dataModels.add(ci.dataModelFolder);
			}
			out.writeObject(dataModels);
			out.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Loads connection list.
	 */
	@SuppressWarnings("unchecked")
	private void loadConnectionList() {
		connectionList = new ArrayList<ConnectionInfo>();
		currentConnection = null;
		boolean ok = false;
		boolean preV4 = true;
		
		try {
			File file = Environment.newFile(CONNECTIONS_FILE);
			if (file.exists()) {
				ObjectInputStream in = new ObjectInputStream(new FileInputStream(file));
				List<ConnectionInfo> cis = (List<ConnectionInfo>) in.readObject();
				int i = in.readInt();
				try {
					List<String> dma = (List<String>) in.readObject();
					for (int n = 0; n < dma.size(); ++n) {
						ConnectionInfo connectionInfo = cis.get(n);
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
					}
					preV4 = false;
				} catch (Throwable t) {
					// ignore. pre 4.0 files do not contain data model assignments.
				}
				in.close();
				connectionList = cis;
				if (i >= 0 && i < connectionList.size()) {
//                	currentConnection = connectionList.get(i);
				}
				ok = true;
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
							new FileInputStream(settingsFile));
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
			} catch (Exception e) {
				// ignore
			}
		}
		if (connectionList.size() == 0) {
			ConnectionInfo ci = new ConnectionInfo(executionContext);
			ci.alias = "Demo Scott";
			ci.driverClass = "org.h2.Driver";
			ci.jar1 = "lib" + File.separator + "h2-1.3.175.jar";
			ci.url = "jdbc:h2:" + Environment.newFile("demo-scott").getPath();
			ci.user = "sa";
			ci.password = "";
			ci.dataModelFolder = "Demo-Scott";
			connectionList.add(ci);
			store();
		}
		if (preV4) {
			ConnectionInfo ci = new ConnectionInfo(executionContext);
			ci.alias = "Demo Sakila";
			ci.driverClass = "org.h2.Driver";
			ci.jar1 = "lib" + File.separator + "h2-1.3.175.jar";
			ci.url = "jdbc:h2:" + Environment.newFile("demo-sakila").getPath();
			ci.user = "sa";
			ci.password = "";
			ci.dataModelFolder = "Demo-Sakila";
			connectionList.add(ci);
			store();
		}
		if (connectionList.size() == 1) {
			currentConnection = connectionList.get(0);
		}
		sortConnectionList();
	}

	private void sortConnectionList() {
		Collections.sort(connectionList, new Comparator<ConnectionInfo>() {
			@Override
			public int compare(ConnectionInfo o1, ConnectionInfo o2) {
				if (dataModelAware) {
					if (currentModelSubfolder != null) {
						boolean c1 = currentModelSubfolder.equals(o1.dataModelFolder);
						boolean c2 = currentModelSubfolder.equals(o2.dataModelFolder);
						
						if (c1 && !c2) return -1;
						if (!c1 && c2) return 1;
					}
				} else {
					boolean c1 = isAssignedToDataModel(o1.dataModelFolder);
					boolean c2 = isAssignedToDataModel(o2.dataModelFolder);
					
					if (c1 && !c2) return -1;
					if (!c1 && c2) return 1;
				}
				return o1.alias.compareToIgnoreCase(o2.alias);
			}
		});
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

        mainPanel = new javax.swing.JPanel();
        jPanel2 = new javax.swing.JPanel();
        closeButton = new javax.swing.JButton();
        jButton1 = new javax.swing.JButton();
        jPanel3 = new javax.swing.JPanel();
        newButton = new javax.swing.JButton();
        editButton = new javax.swing.JButton();
        copy = new javax.swing.JButton();
        deleteButton = new javax.swing.JButton();
        infoBarLabel = new javax.swing.JLabel();
        jPanel4 = new javax.swing.JPanel();
        jScrollPane2 = new javax.swing.JScrollPane();
        connectionsTable = new javax.swing.JTable();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Connect with DB");
        getContentPane().setLayout(new java.awt.CardLayout());

        mainPanel.setLayout(new java.awt.GridBagLayout());

        jPanel2.setLayout(new java.awt.GridBagLayout());

        closeButton.setText(" Cancel ");
        closeButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                closeButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHEAST;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 4);
        jPanel2.add(closeButton, gridBagConstraints);

        jButton1.setText(" Connect ");
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHEAST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 0, 4);
        jPanel2.add(jButton1, gridBagConstraints);

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
        gridBagConstraints.insets = new java.awt.Insets(16, 4, 2, 0);
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

        copy.setText(" Copy ");
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

        deleteButton.setText(" Delete ");
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

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
        mainPanel.add(jPanel3, gridBagConstraints);

        infoBarLabel.setText("info bar");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 11;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        mainPanel.add(infoBarLabel, gridBagConstraints);

        jPanel4.setBorder(javax.swing.BorderFactory.createTitledBorder("Connections"));
        jPanel4.setLayout(new java.awt.GridBagLayout());

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
        jPanel4.add(jScrollPane2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        mainPanel.add(jPanel4, gridBagConstraints);

        getContentPane().add(mainPanel, "card2");

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
						if (edit(ci, false)) {
							connectionList.add(i + 1, ci);
							refresh();
							RowSorter<?> rowSorter = connectionsTable.getRowSorter();
							int selectedRowIndex = rowSorter.convertRowIndexToView(i + 1);
							connectionsTable.getSelectionModel().setSelectionInterval(selectedRowIndex, selectedRowIndex);
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
				if (JOptionPane.showConfirmDialog(this, "Delete '" + currentConnection.alias + "'?", "Delete", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE) == JOptionPane.YES_OPTION) {
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
		try {
			CsvFile drivers = new CsvFile(new File("driverlist.csv"));
			List<Line> lines = new ArrayList<Line>(drivers.getLines());
			Collections.sort(lines, new Comparator<Line>() {
				@Override
				public int compare(Line o1, Line o2) {
					return o1.cells.get(0).compareTo(o2.cells.get(0));
				}
			});
			List<String> dbmsNames = new ArrayList<String>();
			for (Line line: lines) {
				if (line.cells.get(0).length() > 0) {
					dbmsNames.add(line.cells.get(0));
				}
			}
			String s = (String) JOptionPane.showInputDialog(this,
					"Select DBMS", "Select DBMS",
					JOptionPane.QUESTION_MESSAGE, null, dbmsNames.toArray(), dbmsNames.get(0));
			if (s == null) return;
			for (Line line: lines) {
				if (line.cells.get(0).equals(s)) {
					ci.url = line.cells.get(1);
					ci.driverClass = line.cells.get(2);
					String[] jars = line.cells.get(3).replace("/", File.separator).split(" ");
					if (jars.length > 0) {
						ci.jar1 = jars[0];
					}
					if (jars.length > 1) {
						ci.jar2 = jars[1];
					}
					ci.alias = s;
				}
			}
		} catch (Exception e) {
		}
		if (edit(ci, true)) {
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
					refresh();
					store();
					break;
				}
			}
		}
	}//GEN-LAST:event_newButtonActionPerformed

	private void editButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_editButtonActionPerformed
		if (currentConnection == null) return;
		if (edit(currentConnection, false)) {
			refresh();
			store();
		}
	}//GEN-LAST:event_editButtonActionPerformed

	private void closeButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_closeButtonActionPerformed
		isConnected = false;
		setVisible(false);
	}//GEN-LAST:event_closeButtonActionPerformed

	/**
	 * Opens detail editor for a connection.
	 * 
	 * @param ci the connection
	 * @param forNew 
	 * @return <code>true</code> if connection has been edited
	 */
	private boolean edit(ConnectionInfo ci, boolean forNew) {
		return new DbConnectionDetailsEditor(parent, jdbcHelpURL, forNew).edit(ci);
	}

	private void connect() {
		if (currentConnection == null) {
			return;
		}
		
		if (warnOnConnect) {
			if (JOptionPane.YES_OPTION != JOptionPane.showConfirmDialog(this, "Connection is not associated with " + (currentModelSubfolder == null? "any data model." : ("data model \"" + currentModelSubfolder + "\".")) + "\nDo you still want to connect?", "Connect", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE)) {
				return;
			}
		}
		
		isConnected = false;

		Component root = SwingUtilities.getWindowAncestor(mainPanel);
		if (root == null) {
			root = mainPanel;
		}
		try {
			root.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
			if (testConnection(mainPanel, currentConnection)) {
				isConnected = true;
				onConnect(currentConnection);
			}
		} finally {
			root.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		}
	}

	protected void onConnect(ConnectionInfo currentConnection) {
		if (currentConnection != null && currentConnection.url != null) {
			if (currentConnection.url.toLowerCase().startsWith("jdbc:jtds:")) {
				JOptionPane.showMessageDialog(this, "The jTDS JDBC Driver is no longer supported.");
				return;
			}
		}
		setVisible(false);
	}

	private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {                                         
		connect();
	}// GEN-LAST:event_jButton1ActionPerformed

	public static boolean testConnection(Component parent, ConnectionInfo ci) {
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
			UIUtil.showException(parent, "Error loading driver jars", e, UIUtil.EXCEPTION_CONTEXT_USER_ERROR);
			return false;
		}

		try {
			BasicDataSource dataSource = new BasicDataSource(ci.driverClass, ci.url, ci.user, ci.password, 0, urls);
			Window w = parent instanceof Window? (Window) parent : SwingUtilities.getWindowAncestor(parent);
			SessionForUI session = SessionForUI.createSession(dataSource, dataSource.dbms, null, w);
			if (session != null) {
				session.shutDown();
				return true;
			}
			return false;
		} catch (Throwable e) {
			if (e.getCause() instanceof ClassNotFoundException) {
				UIUtil.showException(parent, "Could not connect to DB", new ClassNotFoundException("JDBC driver class not found: '" + e.getMessage() + "'", e.getCause()), UIUtil.EXCEPTION_CONTEXT_USER_ERROR);
			} else {
				UIUtil.showException(parent, "Could not connect to DB (" + (e.getClass().getSimpleName()) + ")", e, UIUtil.EXCEPTION_CONTEXT_USER_ERROR);
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
	 * Selects the DB-schema to analyze.
	 * 
	 * @param isDefaultSchema
	 *            array with a least one field to be set to true if the selected
	 *            schema is the default schema
	 * @return the DB-schema to analyze
	 */
	public String selectDBSchema(Component parent, boolean[] isDefaultSchema)
			throws Exception {
		BasicDataSource dataSource = new BasicDataSource(currentConnection.driverClass,
				currentConnection.url, currentConnection.user,
				currentConnection.password, 0, ClasspathUtil.toURLArray(currentConnection.jar1, currentConnection.jar2, currentConnection.jar3, currentConnection.jar4));
		Session session = new Session(dataSource, dataSource.dbms, executionContext.getIsolationLevel());
		List<String> schemas = JDBCMetaDataBasedModelElementFinder.getSchemas(
				session, currentConnection.user);
		String defaultSchema = JDBCMetaDataBasedModelElementFinder
				.getDefaultSchema(session, currentConnection.user);
		session.shutDown();
		isDefaultSchema[0] = false;
		if (schemas.size() == 1) {
			if (schemas.get(0).equalsIgnoreCase(currentConnection.user)) {
				isDefaultSchema[0] = true;
			}
			return schemas.get(0);
		}
		if (schemas.isEmpty()) {
			isDefaultSchema[0] = true;
			return null;
		}
		String s = (String) JOptionPane.showInputDialog(parent,
				"Select schema to analyze", "Schema",
				JOptionPane.QUESTION_MESSAGE, null, schemas.toArray(),
				defaultSchema);
		if (s == null) {
			isDefaultSchema[0] = true;
			return "";
		}
		if (s.equalsIgnoreCase(defaultSchema)) {
			isDefaultSchema[0] = true;
		}
		return s;
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
    javax.swing.JButton closeButton;
    private javax.swing.JTable connectionsTable;
    private javax.swing.JButton copy;
    private javax.swing.JButton deleteButton;
    private javax.swing.JButton editButton;
    private javax.swing.JLabel infoBarLabel;
    private javax.swing.JButton jButton1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JScrollPane jScrollPane2;
    public javax.swing.JPanel mainPanel;
    private javax.swing.JButton newButton;
    // End of variables declaration//GEN-END:variables

	public String getUser() {
		return currentConnection.user;
	}

	public String getPassword() {
		return currentConnection.password;
	}

	private boolean isAssignedToDataModel(int row) {
		if (row >= connectionList.size()) {
			return false;
		}
		String rowFN = connectionList.get(row).dataModelFolder;
		return isAssignedToDataModel(rowFN);
	}

	protected boolean isAssignedToDataModel(String dataModelFolder) {
		String fn = currentModelSubfolder;
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
        if (scaledWarnIcon == null) {
            int heigth = getFontMetrics(new JLabel("M").getFont()).getHeight();
            double s = heigth / (double) warnIcon.getIconHeight();
            try {
            	scaledWarnIcon = new ImageIcon(warnIcon.getImage().getScaledInstance((int)(warnIcon.getIconWidth() * s), (int)(warnIcon.getIconHeight() * s), Image.SCALE_SMOOTH));
            } catch (Exception e) {
            	logger.info("error", e);
                return null;
            }
        }
        return scaledWarnIcon;
    }

    private static ImageIcon warnIcon;
    private static ImageIcon scaledWarnIcon;
    static {
        String dir = "/net/sf/jailer/ui/resource";
        
        // load images
        try {
            warnIcon = new ImageIcon(MetaDataPanel.class.getResource(dir + "/wanr.png"));
 	    } catch (Exception e) {
	    	logger.info("error", e);
	        e.printStackTrace();
	    }
	}
}
