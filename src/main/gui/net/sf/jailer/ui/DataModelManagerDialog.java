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
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.DefaultComboBoxModel;
import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JTable;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.JailerVersion;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.modelbuilder.ModelBuilder;
import net.sf.jailer.util.CancellationHandler;
import net.sf.jailer.util.Pair;

/**
 * Data Model Management dialog.
 * 
 * @author Ralf Wisser
 */
public abstract class DataModelManagerDialog extends javax.swing.JFrame {

	/**
	 * <code>true</code> if a model is selected.
	 */
	public boolean hasSelectedModel = false;

	/**
	 * List of available models.
	 */
	private List<String> modelList;

	/**
	 * Model details as pair of folder-name and last-modified timestamp.
	 */
	private Map<String, Pair<String, Long>> modelDetails;

	/**
	 * Currently selected model.
	 */
	private String currentModel;

	/**
	 * Model base folders.
	 */
	private List<String> baseFolders = new ArrayList<String>();;

	private final ExecutionContext executionContext = new ExecutionContext(CommandLineInstance.getInstance());
	
	private DbConnectionDialog dbConnectionDialog;
	private InfoBar infoBarConnection; 
	
	/** 
	 * Creates new.
	 */
	public DataModelManagerDialog(String applicationName) {
		this.applicationName = applicationName;
		initComponents();

		InfoBar infoBar = new InfoBar("Data Model Configuration", 
				"A data model is a set of interrelated tables. Acquire information about tables by analyzing\n" +
				"database schemas, or use the data model editor to manually define tables and associations.\n \n",
				"Select a data model to work with.");
		UIUtil.replace(infoBarLabel, infoBar);
		
		infoBarConnection = new InfoBar("Database Connection", 
				"Select a connection to the database.\n" +
				"\n \n \n",
				"Select a database to work with.");
		
		try {
			ImageIcon imageIcon = new ImageIcon(getClass().getResource("/net/sf/jailer/ui/resource/jailer.png"));
			setIconImage(imageIcon.getImage());
			infoBar.setIcon(imageIcon);
			infoBarConnection.setIcon(imageIcon);
		} catch (Throwable t) {
			try {
				ImageIcon imageIcon = new ImageIcon(getClass().getResource("/net/sf/jailer/ui/resource/jailer.gif"));
				setIconImage(imageIcon.getImage());
				infoBar.setIcon(imageIcon);
				infoBarConnection.setIcon(imageIcon);
			} catch (Throwable t2) {
			}
		}
		
		restore();
		
		loadModelList();
		initTableModel();
		initConnectionDialog();
		
		final TableCellRenderer defaultTableCellRenderer = dataModelsTable
				.getDefaultRenderer(String.class);
		dataModelsTable.setShowGrid(false);
		dataModelsTable.setDefaultRenderer(Object.class,
				new TableCellRenderer() {

					@Override
					public Component getTableCellRendererComponent(
							JTable table, Object value, boolean isSelected,
							boolean hasFocus, int row, int column) {
						Component render = defaultTableCellRenderer
								.getTableCellRendererComponent(table, value,
										false, hasFocus, row, column);
						if (render instanceof JLabel) {
							final Color BG1 = new Color(255, 255, 255);
							final Color BG2 = new Color(242, 255, 242);
							if (!isSelected) {
								((JLabel) render)
									.setBackground((row % 2 == 0) ? BG1
											: BG2);
							} else {
								((JLabel) render).setBackground(new Color(160, 200, 255));
							}
						}
						return render;
					}
				});
		dataModelsTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		dataModelsTable.getSelectionModel().addListSelectionListener(
				new ListSelectionListener() {
					@Override
					public void valueChanged(ListSelectionEvent evt) {
						if (dataModelsTable.getSelectedRow() >= 0) {
							currentModel = modelList.get(dataModelsTable.getSelectedRow());
						} else {
							currentModel = null;
						}
						refreshButtons();
					}
				});
		dataModelsTable.addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent me) {
				JTable table =(JTable) me.getSource();
				Point p = me.getPoint();
				int row = table.rowAtPoint(p);
				if (me.getClickCount() >= 2) {
					table.getSelectionModel().setSelectionInterval(row, row);
					if (row >= 0) {
						currentModel = modelList.get(row);
					} else {
						currentModel = null;
					}
					refreshButtons();
					if (currentModel != null) {
						okButtonActionPerformed(null);
					}
				}
			}
		});
		
		updateLocationComboboxModel();
		locationComboBox.setSelectedItem(executionContext.getDatamodelFolder());
		final ListCellRenderer<String> renderer = locationComboBox.getRenderer();
		locationComboBox.setRenderer(new ListCellRenderer<String>() {
			@Override
			public Component getListCellRendererComponent(JList<? extends String> list, String value, int index,
					boolean isSelected, boolean cellHasFocus) {
				try {
					value = new File(value).getAbsolutePath();
				} catch (Exception e) {
					// ignore
				}
				return renderer.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
			}
		});
		locationComboBox.addItemListener(new ItemListener() {
			@Override
			public void itemStateChanged(ItemEvent e) {
				if (e.getItem() != null) {
					executionContext.setDatamodelFolder((String) e.getItem());
					loadModelList();
					refresh();
				}
			}
		});
		
		if (currentModel != null) {
			int i = modelList.indexOf(currentModel);
			if (i >= 0) {
				dataModelsTable.getSelectionModel().setSelectionInterval(i, i);
			} else {
				currentModel = null;
			}
		}
		
		setTitle(applicationName);
		
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
			}
			@Override
			public void windowClosed(WindowEvent e) {
		        SwingUtilities.invokeLater(new Runnable() {
					@Override
					public void run() {
						UIUtil.checkTermination();
					}
				});
			}
			@Override
			public void windowActivated(WindowEvent e) {
			}
		});
		
		setLocation(80, 130);
		pack();
		setSize(Math.max(740, getWidth()), 450);
		refresh();
		UIUtil.initPeer();
		okButton.grabFocus();
	}

	private void initConnectionDialog() {
		dbConnectionDialog = new DbConnectionDialog(null, JailerVersion.APPLICATION_NAME, infoBarConnection, executionContext, false) {
			@Override
			protected boolean isAssignedToDataModel(String dataModelFolder) {
				return modelList.contains(dataModelFolder);
			}
			@Override
			protected void onConnect(ConnectionInfo currentConnection) {
				if (!modelList.contains(currentConnection.dataModelFolder)) {
					JOptionPane.showMessageDialog(DataModelManagerDialog.this,
							"Data Model \"" + currentConnection.dataModelFolder + "\" does not exist.\n");
				} else {
					DataModelManager.setCurrentModelSubfolder(currentConnection.dataModelFolder, executionContext);
					setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
					onSelect(dbConnectionDialog, executionContext);
					setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
					DataModelManagerDialog.this.setVisible(false);
					DataModelManagerDialog.this.dispose();
				}
			}
		};
		dbConnectionDialog.closeButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				close();
			}
		});
		connectionDialogPanel.removeAll();
		connectionDialogPanel.add(dbConnectionDialog.mainPanel);
	}
	
	private void updateLocationComboboxModel() {
		List<String> existingBaseFolders = new ArrayList<String>();
		for (String bf: baseFolders) {
			if (new File(bf).exists()) {
				existingBaseFolders.add(bf);
			}
		}
		locationComboBox.setModel(new DefaultComboBoxModel<String>(existingBaseFolders.toArray(new String[0])));
	}

	private void loadModelList() {
		DataModelManager.setCurrentModelSubfolder(null, executionContext);
		
		modelList = new ArrayList<String>();
		modelDetails = new HashMap<String, Pair<String,Long>>();
		for (String mf: DataModelManager.getModelFolderNames(executionContext)) {
			String modelFolder = mf == null? "" : mf;
			modelList.add(modelFolder);
			modelDetails.put(modelFolder, DataModelManager.getModelDetails(mf, executionContext));
		}
		Collections.sort(modelList, new Comparator<String>() {
			@Override
			public int compare(String o1, String o2) {
				return modelDetails.get(o1).a.compareToIgnoreCase(modelDetails.get(o2).a);
			}
		});
	}

	/**
	 * Initializes the table model.
	 */
	private Object[][] initTableModel() {
		Object[][] data = new Object[modelList.size()][];
		int i = 0;
		for (String model: modelList) {
			Pair<String, Long> details = modelDetails.get(model);
			data[i++] = new Object[] { details == null? "" : details.a, model == null || model.length() == 0? "." : model, details == null? "" : toDateAsString(details.b) };
		}
		DefaultTableModel tableModel = new DefaultTableModel(data, new String[] { "Data Model", "Subfolder", "Last Modified" }) {
			@Override
			public boolean isCellEditable(int row, int column) {
				return false;
			}
			private static final long serialVersionUID = 1535384744352159695L;
		};
		dataModelsTable.setModel(tableModel);
		return data;
	}

	private String toDateAsString(Long time) {
		if (time == null) {
			return "";
		}
		return DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.SHORT, Environment.initialLocal).format(new Date(time));
	}

	private boolean inRefresh = false;

	/**
	 * Application name. Used to create the name of the demo database alias. 
	 */
	private final String applicationName;
	
	/**
	 * Refreshes the dialog after model changes.
	 */
	private void refresh() {
		if (inRefresh) return;
		inRefresh = true;
		try {
			String theModel = currentModel;
			Object[][] data = initTableModel();
			currentModel = theModel;
			if (currentModel != null) {
				final int i = modelList.indexOf(currentModel);
				if (i >= 0) {
					dataModelsTable.getSelectionModel().setSelectionInterval(i, i);
					SwingUtilities.invokeLater(new Runnable() {
						@Override
						public void run() {
							Rectangle cellRect = dataModelsTable.getCellRect(i, 1, true);
							dataModelsTable.scrollRectToVisible(cellRect);
						}
					});
				} else {
					dataModelsTable.getSelectionModel().clearSelection();
					currentModel = null;
				}
			} else {
				dataModelsTable.getSelectionModel().clearSelection();
			}
			for (int i = 0; i < dataModelsTable.getColumnCount(); i++) {
				TableColumn column = dataModelsTable.getColumnModel().getColumn(i);
				int width = 1;
				
				Component comp = dataModelsTable.getDefaultRenderer(String.class).
										getTableCellRendererComponent(
												dataModelsTable, column.getHeaderValue(),
												false, false, 0, i);
				width = Math.max(width, comp.getPreferredSize().width);
	
				for (int line = 0; line < data.length; ++line) {
					comp = dataModelsTable.getDefaultRenderer(String.class).
									 getTableCellRendererComponent(
											 dataModelsTable, data[line][i],
										 false, false, line, i);
					width = Math.max(width, comp.getPreferredSize().width);
				}
				
				column.setPreferredWidth(width);
			}
			refreshButtons();
			initConnectionDialog();
		} finally {
			inRefresh = false;
		}
	}

	private void refreshButtons() {
		editButton.setEnabled(currentModel != null);
		deleteButton.setEnabled(currentModel != null);
		okButton.setEnabled(currentModel != null);
		analyzeButton.setEnabled(currentModel != null);
	}
	
	/**
	 * File to store selection.
	 */
	private static String MODEL_SELECTION_FILE = ".selecteddatamodel";
	
	/**
	 * Stores the selection.
	 */
	private void store() {
		if (currentModel != null) {
			try {
				File file = Environment.newFile(MODEL_SELECTION_FILE);
				BufferedWriter out = new BufferedWriter(new FileWriter(file));
				out.write(currentModel + "\n");
				out.write(executionContext.getDatamodelFolder() + "\n");
				for (String bf: baseFolders) {
					out.write(bf + "\n");
				}
				out.close();
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}
	
	/**
	 * Restores the selection.
	 */
	private void restore() {
		currentModel = null;
		String currentBaseFolder = null;
		baseFolders.clear();
		try {
			File file = Environment.newFile(MODEL_SELECTION_FILE);
			BufferedReader in = new BufferedReader(new FileReader(file));
			currentModel = in.readLine();
			if (currentModel != null) {
				currentBaseFolder = in.readLine();
				if (currentBaseFolder != null) {
					for (;;) {
						String bf = in.readLine();
						if (bf == null) {
							break;
						}
						baseFolders.add(bf);
					}
				}
			}
			in.close();
		} catch (Exception e) {
			// ignore
		}
		if (currentBaseFolder == null || !new File(currentBaseFolder).exists()) {
			currentBaseFolder = executionContext.getDatamodelFolder();
		}
		if (!baseFolders.contains(executionContext.getDatamodelFolder())) {
			baseFolders.add(0, executionContext.getDatamodelFolder());
		}
		baseFolders.remove(currentBaseFolder);
		baseFolders.add(0, currentBaseFolder);
		executionContext.setDatamodelFolder(currentBaseFolder);
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

        jTabbedPane1 = new javax.swing.JTabbedPane();
        jPanel1 = new javax.swing.JPanel();
        jPanel2 = new javax.swing.JPanel();
        jPanel4 = new javax.swing.JPanel();
        jLabel2 = new javax.swing.JLabel();
        locationComboBox = new javax.swing.JComboBox();
        browseButton = new javax.swing.JButton();
        jSeparator1 = new javax.swing.JSeparator();
        jButton2 = new javax.swing.JButton();
        okButton = new javax.swing.JButton();
        jScrollPane2 = new javax.swing.JScrollPane();
        dataModelsTable = new javax.swing.JTable();
        jPanel3 = new javax.swing.JPanel();
        newButton = new javax.swing.JButton();
        editButton = new javax.swing.JButton();
        analyzeButton = new javax.swing.JButton();
        deleteButton = new javax.swing.JButton();
        infoBarLabel = new javax.swing.JLabel();
        jPanel5 = new javax.swing.JPanel();
        connectionDialogPanel = new javax.swing.JPanel();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Connect with DB");
        getContentPane().setLayout(new java.awt.GridBagLayout());

        jPanel1.setLayout(new java.awt.GridBagLayout());

        jPanel2.setLayout(new java.awt.GridBagLayout());

        jPanel4.setLayout(new java.awt.GridBagLayout());

        jLabel2.setText(" Base Folder  ");
        jPanel4.add(jLabel2, new java.awt.GridBagConstraints());

        locationComboBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        jPanel4.add(locationComboBox, gridBagConstraints);

        browseButton.setText("Browse..");
        browseButton.addActionListener(new java.awt.event.ActionListener() {
            @Override
			public void actionPerformed(java.awt.event.ActionEvent evt) {
                browseButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        jPanel4.add(browseButton, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        jPanel4.add(jSeparator1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        jPanel2.add(jPanel4, gridBagConstraints);

        jButton2.setText(" Cancel ");
        jButton2.addActionListener(new java.awt.event.ActionListener() {
            @Override
			public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton2ActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHEAST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 4);
        jPanel2.add(jButton2, gridBagConstraints);

        okButton.setText(" OK ");
        okButton.addActionListener(new java.awt.event.ActionListener() {
            @Override
			public void actionPerformed(java.awt.event.ActionEvent evt) {
                okButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHEAST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 0, 4);
        jPanel2.add(okButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 100;
        gridBagConstraints.gridwidth = 20;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(jPanel2, gridBagConstraints);

        jScrollPane2.setBorder(javax.swing.BorderFactory.createTitledBorder("Data Models"));

        dataModelsTable.setModel(new javax.swing.table.DefaultTableModel(
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
        jScrollPane2.setViewportView(dataModelsTable);

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
            @Override
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
            @Override
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

        analyzeButton.setText(" Analyze Database ");
        analyzeButton.addActionListener(new java.awt.event.ActionListener() {
            @Override
			public void actionPerformed(java.awt.event.ActionEvent evt) {
                analyzeButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 30;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 2, 0);
        jPanel3.add(analyzeButton, gridBagConstraints);

        deleteButton.setText(" Delete ");
        deleteButton.addActionListener(new java.awt.event.ActionListener() {
            @Override
			public void actionPerformed(java.awt.event.ActionEvent evt) {
                deleteButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 31;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 2, 0);
        jPanel3.add(deleteButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
        jPanel1.add(jPanel3, gridBagConstraints);

        infoBarLabel.setText("info bar");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 11;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        jPanel1.add(infoBarLabel, gridBagConstraints);

        jTabbedPane1.addTab("Data Model", jPanel1);

        jPanel5.setLayout(new java.awt.GridBagLayout());

        connectionDialogPanel.setLayout(new java.awt.BorderLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel5.add(connectionDialogPanel, gridBagConstraints);

        jTabbedPane1.addTab("Connection", jPanel5);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        getContentPane().add(jTabbedPane1, gridBagConstraints);

        pack();
    }// </editor-fold>//GEN-END:initComponents

	private void deleteButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_deleteButtonActionPerformed
		if (currentModel != null) {
			if (JOptionPane.showConfirmDialog(this, "Do you really want to delete Data Model \"" + modelDetails.get(currentModel).a + "\"?", "Delete", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE) == JOptionPane.YES_OPTION) {
				modelList.remove(currentModel);
				DataModelManager.deleteModel(currentModel, executionContext);
				dataModelsTable.getSelectionModel().clearSelection();
				refresh();
				store();
			}
		}
	}//GEN-LAST:event_deleteButtonActionPerformed

	private void newButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_newButtonActionPerformed
		DataModelManager.setCurrentModelSubfolder(null, executionContext);
		
		NewDataModelDialog newDataModelDialog = new NewDataModelDialog(this, modelList);
		
		String newName = newDataModelDialog.getNameEntered();
		if (newName != null) {
			try {
				DataModelManager.createNewModel(newName, newDataModelDialog.getFolderName(), executionContext);
				loadModelList();
				currentModel = newName;
				refresh();
				store();
			} catch (Exception e) {
				UIUtil.showException(this, "Error", e, UIUtil.EXCEPTION_CONTEXT_USER_ERROR);
			}
		}
	}//GEN-LAST:event_newButtonActionPerformed

	private void editButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_editButtonActionPerformed
		if (currentModel == null) return;
		activateCurrentModel();
		edit(currentModel.length() == 0? null : currentModel);
		loadModelList();
		refresh();
		store();
	}//GEN-LAST:event_editButtonActionPerformed

	private void activateCurrentModel() {
		DataModelManager.setCurrentModelSubfolder(currentModel.length() == 0? null : currentModel, executionContext);
	}

	private void jButton2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton2ActionPerformed
		close();
	}//GEN-LAST:event_jButton2ActionPerformed

	private void close() {
		hasSelectedModel = false;
		setVisible(false);
		dispose();
	}

	private void analyzeButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_analyzeButtonActionPerformed
		activateCurrentModel();
		
		try {
			DbConnectionDialog dbConnectionDialog = new DbConnectionDialog(this, applicationName,
					new InfoBar("Connect with Database", 
							"Select a connection to the database to be analyzed, or create a new connection.\n" +
							"New connections will be assigned to the datamodel \"" + modelDetails.get(currentModel).a + "\".", null), executionContext);
			if (dbConnectionDialog.connect("Analyze Database")) {
				List<String> args = new ArrayList<String>();
				args.add("build-model-wo-merge");
				dbConnectionDialog.addDbArgs(args);
				
				DataModel dataModel = new DataModel(executionContext);
				AnalyseOptionsDialog analyseOptionsDialog = new AnalyseOptionsDialog(this, dataModel, executionContext);
				boolean[] isDefaultSchema = new boolean[1];
				String[] defaultSchema = new String[1];
				setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
				List<String> schemas;
				try {
					CancellationHandler.reset(null);
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
					analyseOptionsDialog.appendAnalyseCLIOptions(args);
					ModelBuilder.assocFilter = analyseOptionsDialog.getAssociationLineFilter();
					if (UIUtil.runJailer(this, args, false, true, false, true, null, dbConnectionDialog.getUser(), dbConnectionDialog.getPassword(), null, null, false, true, false, executionContext)) {
						ModelBuilder.assocFilter = null;
						String modelname = dataModel.getName();
						DataModelEditor dataModelEditor = new DataModelEditor(this, true, analyseOptionsDialog.isRemoving(), null, analyseOptionsDialog.getTableLineFilter(), analyseOptionsDialog.getAssociationLineFilter(), modelname, schema == null? dbConnectionDialog.getName() : schema, executionContext);
						if (dataModelEditor.dataModelHasChanged()) {
							dataModelEditor.setVisible(true);
						}
					}
					loadModelList();
					refresh();
					store();
				}
			}
		} catch (Exception e) {
			UIUtil.showException(this, "Error", e);
		} finally {
			ModelBuilder.assocFilter = null;
		}

	}//GEN-LAST:event_analyzeButtonActionPerformed

    private void browseButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_browseButtonActionPerformed
       	String folder = choseFolder();
       	if (folder != null) {
       		executionContext.setDatamodelFolder(folder);
       		baseFolders.remove(folder);
       		baseFolders.add(0, folder);
       		updateLocationComboboxModel();
    		locationComboBox.setSelectedItem(executionContext.getDatamodelFolder());
			loadModelList();
			refresh();
			store();
       	}
    }//GEN-LAST:event_browseButtonActionPerformed

	/**
	 * Opens file chooser.
	 */
	public String choseFolder() {
		
		JFileChooser fc = new JFileChooser();
		fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		fc.setDialogTitle("Base Folder");
		int returnVal = fc.showOpenDialog(this);
	    if (returnVal == JFileChooser.APPROVE_OPTION) {
	    	return fc.getSelectedFile().getAbsolutePath();
	    }
		return null;
	}

	/**
	 * Opens data model editor.
	 */
	private void edit(String modelFolder) {
		try {
			DataModelEditor dataModelEditor = new DataModelEditor(this, false, false, null, null, null, modelDetails.get(modelFolder == null? "" : modelFolder).a, null, executionContext);
			dataModelEditor.setVisible(true);
		} catch (Exception e) {
			UIUtil.showException(this, "Error", e);
		}
	}

	private void okButtonActionPerformed(java.awt.event.ActionEvent evt) {                                          
		if (currentModel == null) {
			return;
		}

		activateCurrentModel();
		
		hasSelectedModel = true;
		setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
		store();
		onSelect(null, executionContext);
		setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		setVisible(false);
		dispose();
	}// GEN-LAST:event_okButtonActionPerformed

	protected abstract void onSelect(DbConnectionDialog dbConnectionDialog, ExecutionContext executionContext);

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton analyzeButton;
    private javax.swing.JButton browseButton;
    private javax.swing.JPanel connectionDialogPanel;
    private javax.swing.JTable dataModelsTable;
    private javax.swing.JButton deleteButton;
    private javax.swing.JButton editButton;
    private javax.swing.JLabel infoBarLabel;
    private javax.swing.JButton jButton2;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JTabbedPane jTabbedPane1;
    private javax.swing.JComboBox locationComboBox;
    private javax.swing.JButton newButton;
    private javax.swing.JButton okButton;
    // End of variables declaration//GEN-END:variables

	private static final long serialVersionUID = -3983034803834547687L;

}
