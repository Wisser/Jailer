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

import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Point;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import net.sf.jailer.CommandLineParser;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.modelbuilder.ModelBuilder;
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
	 * Currently selected connection.
	 */
	private String currentModel;

	/** 
	 * Creates new.
	 */
	public DataModelManagerDialog(String applicationName) {
		this.applicationName = applicationName;
		initComponents();

		InfoBar infoBar = new InfoBar("Data Model Configuration", 
				"A data model is a set of interrelated tables. Acquire information about tables by analyzing\n" +
				"database schemas, or use the data model editor to manually define tables and associations.\n" +
				"Select a data model to work with.");
		UIUtil.replace(infoBarLabel, infoBar);
		
		String modelpath = CommandLineParser.getInstance().getDataModelFolder();
		try {
			modelpath = CommandLineParser.getInstance().newFile(modelpath).getAbsolutePath();
		} catch (Throwable t) {
			// use default modelpath
		}
		locationLabel.setText(modelpath);
		locationLabel.setToolTipText(modelpath);
		
		try {
			ImageIcon imageIcon = new ImageIcon(getClass().getResource("/net/sf/jailer/resource/jailer.png"));
			setIconImage(imageIcon.getImage());
			infoBar.setIcon(imageIcon);
		} catch (Throwable t) {
			try {
				ImageIcon imageIcon = new ImageIcon(getClass().getResource("/net/sf/jailer/resource/jailer.gif"));
				setIconImage(imageIcon.getImage());
				infoBar.setIcon(imageIcon);
			} catch (Throwable t2) {
			}
		}
		
		loadModelList();
		initTableModel();
		
		final TableCellRenderer defaultTableCellRenderer = dataModelsTable
				.getDefaultRenderer(String.class);
		dataModelsTable.setShowGrid(false);
		dataModelsTable.setDefaultRenderer(Object.class,
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
						refresh();
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
		        	refresh();
		        	if (currentModel != null) {
		        		okButtonActionPerformed(null);
		        	}
		        }
		    }
		});
		
		currentModel = restore();
		
		if (currentModel != null) {
			int i = modelList.indexOf(currentModel);
			if (i >= 0) {
				dataModelsTable.getSelectionModel().setSelectionInterval(i, i);
			} else {
				currentModel = null;
			}
		}
		
		setTitle(applicationName);
		
		setLocation(80, 130);
		pack();
		setSize(Math.max(740, getWidth()), 360);
		refresh();
		UIUtil.initPeer();
		okButton.grabFocus();
	}

	private void loadModelList() {
		DataModelManager.setCurrentModelSubfolder(null);
		
		modelList = new ArrayList<String>();
		modelDetails = new HashMap<String, Pair<String,Long>>();
		for (String mf: DataModelManager.getModelFolderNames()) {
			String modelFolder = mf == null? "" : mf;
			modelList.add(modelFolder);
			modelDetails.put(modelFolder, DataModelManager.getModelDetails(mf));
		}
		Collections.sort(modelList, new Comparator<String>() {
			@Override
			public int compare(String o1, String o2) {
				return modelDetails.get(o1).a.compareTo(modelDetails.get(o2).a);
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
		return SimpleDateFormat.getDateTimeInstance(SimpleDateFormat.SHORT, SimpleDateFormat.SHORT).format(new Date(time));
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
				int i = modelList.indexOf(currentModel);
				if (i >= 0) {
					dataModelsTable.getSelectionModel().setSelectionInterval(i, i);
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
			editButton.setEnabled(currentModel != null);
			deleteButton.setEnabled(currentModel != null);
			okButton.setEnabled(currentModel != null);
			analyzeButton.setEnabled(currentModel != null);
		} finally {
			inRefresh = false;
		}
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
	            File file = new File(MODEL_SELECTION_FILE);
	            BufferedWriter out = new BufferedWriter(new FileWriter(file));
	            out.write(currentModel + "\n");
	            out.close();
	        } catch (Exception e) {
	            e.printStackTrace();
	        }
		}
	}
	
	/**
	 * Restores the selection.
	 */
	private String restore() {
		String selection = null;
		try {
            File file = new File(MODEL_SELECTION_FILE);
            BufferedReader in = new BufferedReader(new FileReader(file));
            selection = (String) in.readLine();
            in.close();
        } catch (Exception e) {
            // ignore
        }
		return selection;
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
        jPanel2 = new javax.swing.JPanel();
        jPanel4 = new javax.swing.JPanel();
        jLabel2 = new javax.swing.JLabel();
        locationLabel = new javax.swing.JLabel();
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

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Connect with DB");
        getContentPane().setLayout(new java.awt.CardLayout());

        jPanel1.setLayout(new java.awt.GridBagLayout());

        jPanel2.setLayout(new java.awt.GridBagLayout());

        jPanel4.setLayout(new java.awt.GridBagLayout());

        jLabel2.setText(" Base Folder:  ");
        jPanel4.add(jLabel2, new java.awt.GridBagConstraints());

        locationLabel.setText("jLabel3");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        jPanel4.add(locationLabel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        jPanel2.add(jPanel4, gridBagConstraints);

        jButton2.setText(" Cancel ");
        jButton2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton2ActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHEAST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 4);
        jPanel2.add(jButton2, gridBagConstraints);

        okButton.setText(" OK ");
        okButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                okButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHEAST;
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

        analyzeButton.setText(" Analyze Database ");
        analyzeButton.addActionListener(new java.awt.event.ActionListener() {
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

        getContentPane().add(jPanel1, "card2");

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void deleteButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_deleteButtonActionPerformed
        if (currentModel != null) {
    		if (JOptionPane.showConfirmDialog(this, "Do you really want to delete Data Model \"" + modelDetails.get(currentModel).a + "\"?", "Delete", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE) == JOptionPane.YES_OPTION) {
    			modelList.remove(currentModel);
    			DataModelManager.deleteModel(currentModel);
    			dataModelsTable.getSelectionModel().clearSelection();
    			refresh();
        		store();
    		}
        }
    }//GEN-LAST:event_deleteButtonActionPerformed

    private void newButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_newButtonActionPerformed
    	DataModelManager.setCurrentModelSubfolder(null);
    	
    	NewDataModelDialog newDataModelDialog = new NewDataModelDialog(this, modelList);
    	
    	String newName = newDataModelDialog.getNameEntered();
    	if (newName != null) {
    		try {
    			DataModelManager.createNewModel(newName, newDataModelDialog.getFolderName());
    			loadModelList();
    			currentModel = newName;
    	    	refresh();
    	    	store();
    		} catch (Exception e) {
    			UIUtil.showException(this, "Error", e);
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
		DataModelManager.setCurrentModelSubfolder(currentModel.length() == 0? null : currentModel);
	}

    private void jButton2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton2ActionPerformed
        hasSelectedModel = false;
        setVisible(false);
        dispose();
    }//GEN-LAST:event_jButton2ActionPerformed

    private void analyzeButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_analyzeButtonActionPerformed
		activateCurrentModel();
		
		try {
			DbConnectionDialog dbConnectionDialog = new DbConnectionDialog(this, applicationName,
					new InfoBar("Connect with Database", 
							"Select a connection to the database to be analyzed, or create a new connection.\n" +
							"New connections will be assigned to the datamodel \"" + modelDetails.get(currentModel).a + "\"."));
	        if (dbConnectionDialog.connect("Analyze Database")) {
	        	List<String> args = new ArrayList<String>();
	        	args.add("build-model");
	        	dbConnectionDialog.addDbArgs(args);
	        	
	        	DataModel dataModel = new DataModel();
				AnalyseOptionsDialog analyseOptionsDialog = new AnalyseOptionsDialog(this, dataModel);
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
		    			String modelname = dataModel.getName();
		        		DataModelEditor dataModelEditor = new DataModelEditor(this, true, analyseOptionsDialog.isRemoving(), null, analyseOptionsDialog.getTableLineFilter(), analyseOptionsDialog.getAssociationLineFilter(), modelname, schema == null? dbConnectionDialog.getName() : schema);
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

    /**
     * Opens data model editor.
     */
	private void edit(String modelFolder) {
		try {
			DataModelEditor dataModelEditor = new DataModelEditor(this, false, false, null, null, null, modelDetails.get(modelFolder == null? "" : modelFolder).a, null);
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
		onSelect();
		setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		setVisible(false);
		dispose();
	}// GEN-LAST:event_okButtonActionPerformed

    protected abstract void onSelect();

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton analyzeButton;
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
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JLabel locationLabel;
    private javax.swing.JButton newButton;
    private javax.swing.JButton okButton;
    // End of variables declaration//GEN-END:variables

	private static final long serialVersionUID = -3983034803834547687L;

}
