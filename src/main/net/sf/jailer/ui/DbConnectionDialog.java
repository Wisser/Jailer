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
package net.sf.jailer.ui;

import java.awt.Color;
import java.awt.Component;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.net.URL;
import java.net.URLClassLoader;
import java.sql.Connection;
import java.sql.Driver;
import java.sql.DriverManager;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import net.sf.jailer.database.Session;
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.CsvFile.Line;

/**
 * "Connect with DB" dialog.
 * 
 * @author Ralf Wisser
 */
public class DbConnectionDialog extends javax.swing.JDialog {

	/**
	 * <code>true</code> if valid connection is available.
	 */
	boolean isConnected = false;

	/**
	 * Holds connection information.
	 */
	public static class ConnectionInfo implements Serializable {
		private static final long serialVersionUID = -8034755966212631808L;
		public String alias = "";
		public String driverClass = "";
		public String url = "";
		public String user = "";
		public String password = "";
		public String jar1 = "";
		public String jar2 = "";
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
	
	/**
	 * Gets connection to DB.
	 * 
	 * @return <code>true</code> if connection succeeded
	 */
	public boolean connect(String reason) {
		setTitle((reason == null ? "" : (reason + " - ")) + "Connect.");
		setVisible(true);
		return isConnected;
	}

	/** Creates new form DbConnectionDialog */
	public DbConnectionDialog(java.awt.Frame parent) {
		super(parent, true);
		this.parent = parent;
		loadConnectionList();
		initComponents();

		int i;
		initTableModel();
		
		final TableCellRenderer defaultTableCellRenderer = connectionsTable
				.getDefaultRenderer(String.class);
		connectionsTable.setShowGrid(false);
		connectionsTable.setDefaultRenderer(Object.class,
				new TableCellRenderer() {

					public Component getTableCellRendererComponent(
							JTable table, Object value, boolean isSelected,
							boolean hasFocus, int row, int column) {
						Component render = defaultTableCellRenderer
								.getTableCellRendererComponent(table, value,
										isSelected, hasFocus, row, column);
						if (render instanceof JLabel && !isSelected) {
							final Color BG1 = new Color(255, 255, 255);
							final Color BG2 = new Color(230, 255, 255);
							((JLabel) render)
									.setBackground((row % 2 == 0) ? BG1
											: BG2);
						}
						return render;
					}
				});
		connectionsTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		connectionsTable.getSelectionModel().addListSelectionListener(
				new ListSelectionListener() {
					@Override
					public void valueChanged(ListSelectionEvent evt) {
						int row = connectionsTable.getSelectedRow();
						refresh();
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
				// TODO Auto-generated method stub
				
			}
			@Override
			public void componentMoved(ComponentEvent e) {
				// TODO Auto-generated method stub
				
			}
			@Override
			public void componentResized(ComponentEvent e) {
				// TODO Auto-generated method stub
				
			}
		});
		
		setLocation(100, 150);
		pack();
		setSize(Math.max(680, getWidth()), 300);
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
			data[i++] = new Object[] { ci.alias, ci.user, ci.url };
		}
		connectionsTable.setModel(new DefaultTableModel(data, new String[] { "Alias", "User", "URL" }));
		return data;
	}

	private boolean inRefresh = false;
	
	/**
	 * Refreshes the dialog after model changes.
	 */
	private void refresh() {
		if (inRefresh) return;
		inRefresh = true;
		try {
			int selectedRow = connectionsTable.getSelectedRow();
			Object[][] data = initTableModel();
			if (selectedRow >= 0) {
				connectionsTable.getSelectionModel().setSelectionInterval(selectedRow, selectedRow);
				currentConnection = connectionList.get(selectedRow);
			} else {
				currentConnection = null;
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
		            width = Math.max(width, comp.getPreferredSize().width);
	            }
	            
	            column.setPreferredWidth(width);
	        }
			editButton.setEnabled(currentConnection != null);
			deleteButton.setEnabled(currentConnection != null);
			copy.setEnabled(currentConnection != null);
			upButton.setEnabled(currentConnection != null && selectedRow > 0);
			downButton.setEnabled(currentConnection != null && selectedRow < connectionList.size() - 1);
			jButton1.setEnabled(currentConnection != null);
		} finally {
			inRefresh = false;
		}
	}
	
	/**
	 * File to store connections.
	 */
	private static String CONNECTIONS_FILE = ".connections";
	
	/**
	 * Stores the connections into the CONNECTIONS_FILE.
	 */
	private void store() {
		try {
            File file = new File(CONNECTIONS_FILE);
            ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(file));
            out.writeObject(connectionList);
            out.writeInt(connectionList.indexOf(currentConnection));
            out.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
	}
	
	/**
	 * Loads connection list.
	 */
	private void loadConnectionList() {
		connectionList = new ArrayList<ConnectionInfo>();
		currentConnection = null;
		
		try {
            File file = new File(CONNECTIONS_FILE);
            if (file.exists()) {
            	ObjectInputStream in = new ObjectInputStream(new FileInputStream(CONNECTIONS_FILE));
                List<ConnectionInfo> cis = (List<ConnectionInfo>) in.readObject();
                int i = in.readInt();
                in.close();
                connectionList = cis;
                if (i >= 0 && i < connectionList.size()) {
                	currentConnection = connectionList.get(i);
                }
                return;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        
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
						ConnectionInfo ci = new ConnectionInfo();
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
				if (connectionList.size() > 0) {
					currentConnection = connectionList.get(0);
				}
			}
		} catch (Exception e) {
			// ignore
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
        jLabel1 = new javax.swing.JLabel();
        jPanel2 = new javax.swing.JPanel();
        jButton2 = new javax.swing.JButton();
        jButton1 = new javax.swing.JButton();
        jLabel11 = new javax.swing.JLabel();
        jScrollPane2 = new javax.swing.JScrollPane();
        connectionsTable = new javax.swing.JTable();
        jPanel3 = new javax.swing.JPanel();
        newButton = new javax.swing.JButton();
        editButton = new javax.swing.JButton();
        copy = new javax.swing.JButton();
        deleteButton = new javax.swing.JButton();
        upButton = new javax.swing.JButton();
        downButton = new javax.swing.JButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Connect with DB");
        getContentPane().setLayout(new java.awt.CardLayout());

        jPanel1.setLayout(new java.awt.GridBagLayout());

        jLabel1.setText(" Connections");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 4, 0);
        jPanel1.add(jLabel1, gridBagConstraints);

        jPanel2.setLayout(new java.awt.GridBagLayout());

        jButton2.setText(" Cancel ");
        jButton2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton2ActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHEAST;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 4);
        jPanel2.add(jButton2, gridBagConstraints);

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
        jPanel1.add(jPanel2, gridBagConstraints);

        jLabel11.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 10;
        jPanel1.add(jLabel11, gridBagConstraints);

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
        gridBagConstraints.gridy = 20;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel1.add(jScrollPane2, gridBagConstraints);

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

        upButton.setText(" Up ");
        upButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                upButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 40;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(8, 4, 2, 0);
        jPanel3.add(upButton, gridBagConstraints);

        downButton.setText(" Down ");
        downButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                downButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 50;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 0);
        jPanel3.add(downButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
        jPanel1.add(jPanel3, gridBagConstraints);

        getContentPane().add(jPanel1, "card2");

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void upButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_upButtonActionPerformed
        if (currentConnection != null) {
        	int i = connectionList.indexOf(currentConnection);
        	if (i > 0) {
        		connectionList.set(i, connectionList.get(i - 1));
        		connectionList.set(i - 1, currentConnection);
        		connectionsTable.getSelectionModel().setSelectionInterval(i - 1, i - 1);
        		refresh();
        		store();
        	}
        }
    }//GEN-LAST:event_upButtonActionPerformed

    private void downButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_downButtonActionPerformed
        if (currentConnection != null) {
        	int i = connectionList.indexOf(currentConnection);
        	if (i < connectionList.size() - 1) {
        		connectionList.set(i, connectionList.get(i + 1));
        		connectionList.set(i + 1, currentConnection);
        		connectionsTable.getSelectionModel().setSelectionInterval(i + 1, i + 1);
        		refresh();
        		store();
        	}
        }
    }//GEN-LAST:event_downButtonActionPerformed

    private void copyActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_copyActionPerformed
    	if (currentConnection != null) {
        	int i = connectionList.indexOf(currentConnection);
        	if (i >= 0) {
        		for (int nr = 1; ; ++nr) {
        			String newAlias = "Copy of " + currentConnection.alias + (nr > 1? " (" + nr + ")" : "");
        			boolean found = false;
        			for (ConnectionInfo ci: connectionList) {
        				if (ci.alias.equals(newAlias)) {
        					found = true;
        					break;
        				}
        			}
        			if (!found) {
        				ConnectionInfo ci = new ConnectionInfo();
        				ci.alias = newAlias;
        				ci.driverClass = currentConnection.driverClass;
        				ci.jar1 = currentConnection.jar1;
        				ci.jar2 = currentConnection.jar2;
        				ci.password = currentConnection.password;
        				ci.url = currentConnection.url;
        				ci.user = currentConnection.user;
        				connectionList.add(i + 1, ci);
            			connectionsTable.getSelectionModel().setSelectionInterval(i + 1, i + 1);
            			refresh();
                		store();
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
    	ConnectionInfo ci = new ConnectionInfo();
    	try {
			CsvFile drivers = new CsvFile(new File("driverlist.csv"));
			List<Line> lines = new ArrayList<Line>(drivers.getLines());
			Collections.sort(lines, new Comparator<Line>() {
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
					ci.alias = s;
				}
			}
		} catch (Exception e) {
		}
		if (edit(ci)) {
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
    				connectionList.add(ci);
    				int i = connectionList.size() - 1;
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
    	if (edit(currentConnection)) {
			refresh();
    		store();
    	}
    }//GEN-LAST:event_editButtonActionPerformed

    private void jButton2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton2ActionPerformed
        isConnected = false;
        setVisible(false);
    }//GEN-LAST:event_jButton2ActionPerformed

    /**
     * Opens detail editor for a connection.
     * 
     * @param ci the connection
     * @return <code>true</code> if connection has been edited
     */
	private boolean edit(ConnectionInfo ci) {
		return new DbConnectionDetailsEditor(parent).edit(ci);
	}

	private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-
		// FIRST
		// :
		// event_jButton1ActionPerformed
		if (currentConnection == null) {
			return;
		}

		store();

		isConnected = false;

		String d1 = currentConnection.jar1.trim();
		String d2 = currentConnection.jar2.trim();
		if (d1.length() == 0) {
			d1 = null;
		}
		if (d2.length() == 0) {
			d2 = null;
		}
		try {
			Session.setClassLoaderForJdbcDriver(addJarToClasspath(d1, d2));
		} catch (Exception e) {
			UIUtil.showException(this, "Error loading driver jars", e);
			return;
		}

		try {
			if (Session.classLoaderForJdbcDriver != null) {
				Driver d = (Driver) Class.forName(
						currentConnection.driverClass, true,
						Session.classLoaderForJdbcDriver).newInstance();
				DriverManager.registerDriver(new Session.DriverShim(d));
			} else {
				Class.forName(currentConnection.driverClass);
			}
			Connection con = DriverManager.getConnection(currentConnection.url,
					currentConnection.user, currentConnection.password);
			con.close();
			isConnected = true;
			setVisible(false);
		} catch (Exception e) {
			UIUtil.showException(this, "Cannot connect with DB", e);
			return;
		}

	}// GEN-LAST:event_jButton1ActionPerformed

	/**
	 * Holds all class-loader in order to prevent loading a jar twice.
	 */
	private Map<String, URLClassLoader> classloaders = new HashMap<String, URLClassLoader>();

	/**
	 * Adds one or two jars to classpath.
	 * 
	 * @param jarName1
	 *            filename of jar 1
	 * @param jarName2
	 *            filename of jar 2
	 */
	private URLClassLoader addJarToClasspath(String jarName1, String jarName2)
			throws Exception {
		String mapKey = jarName1 + "," + jarName2;
		if (classloaders.containsKey(mapKey)) {
			return classloaders.get(mapKey);
		}
		URL[] urls;
		if (jarName1 == null) {
			if (jarName2 == null) {
				return null;
			}
			jarName1 = jarName2;
			jarName2 = null;
		}
		System.out.println("add '" + jarName1 + "' to classpath");
		if (jarName2 == null) {
			urls = new URL[] { new File(jarName1).toURI().toURL() };
		} else {
			System.out.println("add '" + jarName2 + "' to classpath");
			urls = new URL[] { new File(jarName1).toURI().toURL(),
					new File(jarName2).toURI().toURL() };
		}
		URLClassLoader urlLoader = new URLClassLoader(urls);
		classloaders.put(mapKey, urlLoader);
		return urlLoader;
	}

	/**
	 * Selects the DB-schema to for introspection.
	 * 
	 * @param isDefaultSchema
	 *            array with a least one field to be set to true if the selected
	 *            schema is the default schema
	 * @return the DB-schema to for introspection
	 */
	public String selectDBSchema(Component parent, boolean[] isDefaultSchema)
			throws Exception {
		Session statementExecutor = new Session(currentConnection.driverClass,
				currentConnection.url, currentConnection.user,
				currentConnection.password);
		List<String> schemas = JDBCMetaDataBasedModelElementFinder.getSchemas(
				statementExecutor, currentConnection.user);
		String defaultSchema = JDBCMetaDataBasedModelElementFinder
				.getDefaultSchema(statementExecutor, currentConnection.user);
		statementExecutor.shutDown();
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
				"Select schema to introspect", "Schema",
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
	}

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JTable connectionsTable;
    private javax.swing.JButton copy;
    private javax.swing.JButton deleteButton;
    private javax.swing.JButton downButton;
    private javax.swing.JButton editButton;
    private javax.swing.JButton jButton1;
    private javax.swing.JButton jButton2;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel11;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JButton newButton;
    private javax.swing.JButton upButton;
    // End of variables declaration//GEN-END:variables

	public String getPassword() {
		return currentConnection.password;
	}

}
