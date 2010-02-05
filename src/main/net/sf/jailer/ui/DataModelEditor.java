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
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import javax.activation.FileDataSource;
import javax.swing.DefaultListCellRenderer;
import javax.swing.DefaultListModel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.ListCellRenderer;
import javax.swing.ListModel;

import net.sf.jailer.Jailer;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.modelbuilder.ModelBuilder;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.CsvFile.Line;

/**
 * Data Model Editor.
 *
 * @author Ralf Wisser
 */
public class DataModelEditor extends javax.swing.JDialog {

	/**
	 * List of tables.
	 */
	private List<CsvFile.Line> tables;
	
	/**
     * Maps table names to display names.
     */
    private Map<String, String> displayNames;
    
    /**
	 * List of associations.
	 */
	private List<CsvFile.Line> associations;
	
	/**
	 * Table- and association definitions from model-finder result files.
	 */
    private List<CsvFile.Line> linesFromModelFinder = new ArrayList<CsvFile.Line>();

    /**
     * Set of tables with modified columns.
     */
    private Set<String> modifiedColumnTables = new HashSet<String>();
    
	/**
	 * Columns for each table.
	 */
    private Map<String, CsvFile.Line> columns = new TreeMap<String, Line>();
    
    /**
     * List of tables to be excluded from deletion.
     */
    private List<String> excludeFromDeletion = new ArrayList<String>();
    
    /**
     * List of tables to export entirely if in closure of subject.
     */
    private List<String> initialDataTables = new ArrayList<String>();
    
    /**
     * <code>true</code> iff model is modified.
     */
    private boolean needsSave = false;
    
    /**
     * <code>true</code> iff model is saved.
     */
    public boolean saved = false;
    
    /** 
     * Creates new form DataModelEditor.
     * 
     * @param toEdit if not null, open table editor for this table immediately
     */
    public DataModelEditor(java.awt.Frame parent, boolean merge, final Table toEdit) throws Exception {
        super(parent, true);
        tables = new CsvFile(new File(DataModel.getTablesFile())).getLines();

        displayNames = new TreeMap<String, String>();
        File dnFile = new File(DataModel.getDisplayNamesFile());
        if (dnFile.exists()) {
        	for (CsvFile.Line dnl: new CsvFile(dnFile).getLines()) {
        		displayNames.put(dnl.cells.get(0), dnl.cells.get(1));
        	}
        }
        
        associations = new CsvFile(new File(DataModel.getAssociationsFile())).getLines();
        boolean isDemoModel = true;
        for (CsvFile.Line dt: tables) {
        	String lastV = null;
        	for (String v: dt.cells) {
        		if (v != null && v.trim().length() > 0) {
        			lastV = v.trim();
        		}
       		}
        	if (lastV != null) {
        		if (!"Demo".equals(lastV)) {
        			isDemoModel = false;
        			break;
        		}
        	}
        }
        boolean initiallyDirty = false;
        if (merge && isDemoModel) {
        	tables.clear();
        	associations.clear();
        	initiallyDirty = true;
        } else {
	        UIUtil.loadTableList(excludeFromDeletion, DataModel.getExcludeFromDeletionFile());
	        UIUtil.loadTableList(initialDataTables, DataModel.getInitialDataTablesFile());
        }
        int newTables = 0;
        int newAssociations = 0;
        
        File file = new File(DataModel.getColumnsFile());
        if (file.exists()) {
			for (CsvFile.Line l: new CsvFile(file).getLines()) {
	        	columns.put(l.cells.get(0), l);
	        }
        }
        
		File modelFinderTablesFile = new File(ModelBuilder.MODEL_BUILDER_TABLES_CSV);
		if (merge && modelFinderTablesFile.exists()) {
		    List<CsvFile.Line> tablesFromModelFinder = new CsvFile(modelFinderTablesFile).getLines();
	        for (Iterator<CsvFile.Line> i = tablesFromModelFinder.iterator(); i.hasNext(); ) {
	        	CsvFile.Line t = i.next();
	        	for (CsvFile.Line l: tables) {
	        		boolean eq = true;
	        		int n = 0;
	        		while (n < l.cells.size() && n < t.cells.size()) {
	        			if (n != 1 // ignore upsert flag 
	        				&& !l.cells.get(n).equals(t.cells.get(n))) {
	        				eq = false;
	        				break;
	        			}
	        			if (l.cells.get(n).length() == 0) {
	        				break;
	        			}
	        			++n;
	        		}
	        		if (eq) {
	        			i.remove();
	        			break;
	        		}
	        	}
	        }
	        linesFromModelFinder.addAll(tablesFromModelFinder);
	        for (Iterator<CsvFile.Line> i = tables.iterator(); i.hasNext(); ) {
	        	CsvFile.Line t = i.next();
	        	for (CsvFile.Line l: linesFromModelFinder) {
	        		if (l.cells.get(0).equals(t.cells.get(0))) {
	        			i.remove();
	        			break;
	        		}
	        	}
	        }
	        tables.addAll(tablesFromModelFinder);
	        newTables += tablesFromModelFinder.size();
		}
		sortLineList(tables, true);
        File modelFinderAssociationsFile = new File(ModelBuilder.MODEL_BUILDER_ASSOCIATIONS_CSV);
		if (merge && modelFinderAssociationsFile.exists()) {
	        List<CsvFile.Line> associationsFromModelFinder = new CsvFile(modelFinderAssociationsFile).getLines();
	        associations.addAll(associationsFromModelFinder);
	        linesFromModelFinder.addAll(associationsFromModelFinder);
	        newAssociations += associationsFromModelFinder.size();
		}
		
		sortLineList(associations, false);
		initComponents();
		setSize(900, 700);
		setLocation(100, 32);

		File modelFinderColumnFile = new File(ModelBuilder.MODEL_BUILDER_COLUMNS_CSV);
		if (merge && modelFinderColumnFile.exists()) {
	        for (CsvFile.Line l: new CsvFile(modelFinderColumnFile).getLines()) {
	        	CsvFile.Line ol = columns.get(l.cells.get(0));
	        	if (ol == null || !ol.cells.equals(l.cells)) {
	        		modifiedColumnTables.add(l.cells.get(0));
	        		markDirty();
	        	}
	        	columns.put(l.cells.get(0), l);
	        }
		}
		if (merge) {
			info.setText("Found " + newTables + " new tables and " + newAssociations + " new associations");
			if (!linesFromModelFinder.isEmpty()) {
				markDirty();
			}
		}
		info.setVisible(merge);
		updateButtons();
        
		final Color BG_COLOR = new Color(0.8f, 1.0f, 0.6f);
		final Color BG_SELCOLOR = new Color(0.4f, 1.0f, 0.1f);
    	
        ListCellRenderer tablesListItemRenderer = new DefaultListCellRenderer() {
			@Override
			public Component getListCellRendererComponent(JList list,
					Object value,
                    int index,
                    boolean isSelected,
                    boolean cellHasFocus) {
				boolean fromModelFinder = linesFromModelFinder.contains(value);
				CsvFile.Line line = (CsvFile.Line) value;
				String tableName = line.cells.get(0);
				String pk = "";
				for (int i = 2; i < line.length; ++i) {
					if (line.cells.get(i).length() == 0) {
						break;
					}
					if (pk.length() > 0) {
						pk += ", ";
					}
					pk += line.cells.get(i);
				}
				value = line.cells.get(0) + " (" + pk + ")";
				Component render = super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
				if (pk.equals("")) {
					render.setForeground(Color.RED);
				}
				if (fromModelFinder || modifiedColumnTables.contains(tableName)) {
					render.setBackground(isSelected? BG_SELCOLOR : BG_COLOR);
				}
				return render;
			}
        };
        
        ListCellRenderer associationsListItemRenderer = new DefaultListCellRenderer() {
			@Override
			public Component getListCellRendererComponent(JList list,
					Object value,
                    int index,
                    boolean isSelected,
                    boolean cellHasFocus) {
				boolean fromModelFinder = linesFromModelFinder.contains(value);
				CsvFile.Line line = (CsvFile.Line) value;
				String type = "associates";
				if ("B".equalsIgnoreCase(line.cells.get(2))) {
					type = "depends on";
				}
				if ("A".equalsIgnoreCase(line.cells.get(2))) {
					type = "has dependent";
				}
				String name = "";
				if (line.cells.get(5).length() > 0) {
					name = "(" + line.cells.get(5) + ") ";
				}
				value = line.cells.get(0) + " " + type + " " + line.cells.get(1) + " " + name + line.cells.get(3) + " on " + line.cells.get(4);
				Component render = super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
				if (fromModelFinder) {
					render.setBackground(isSelected? BG_SELCOLOR: BG_COLOR);
				}
				return render;
			}
        };
    
        tablesList.setCellRenderer(tablesListItemRenderer);
        associationsList.setCellRenderer(associationsListItemRenderer);
		invalidate();
		if (initiallyDirty) {
			markDirty();
		}
		UIUtil.initPeer();
		
		if (toEdit != null) {
			addWindowListener(new WindowListener() {
				@Override
				public void windowActivated(WindowEvent e) {
				}
				@Override
				public void windowClosed(WindowEvent e) {
				}
				@Override
				public void windowClosing(WindowEvent e) {
				}
				@Override
				public void windowDeactivated(WindowEvent e) {
				}
				@Override
				public void windowDeiconified(WindowEvent e) {
				}
				@Override
				public void windowIconified(WindowEvent e) {
				}
				@Override
				public void windowOpened(WindowEvent e) {
					for (Line l: tables) {
						if (toEdit.getName().equals(l.cells.get(0))) {
							if (new TableEditor(DataModelEditor.this, displayNames, tables, associations, excludeFromDeletion, initialDataTables).edit(l, columns)) {
					    		markDirty();
					    		repaint();
					    	}
							break;
						}
					}
				}
			});
		}
    }
    
	/**
     * Marks data model as modified.
     */
    private void markDirty() {
    	if (!needsSave) {
    		needsSave = true;
    		setTitle("*" + getTitle());
    	}
    }
    
    /**
     * Sorts lists of csv-lines.
     * 
     * @param lines the list to sort
     */
    private void sortLineList(List<CsvFile.Line> list, final boolean sortTables) {
		Collections.sort(list, new Comparator<CsvFile.Line> () {
			public int compare(CsvFile.Line o1, CsvFile.Line o2) {
				int c1 = linesFromModelFinder.contains(o1)? 0 : 1;
				int c2 = linesFromModelFinder.contains(o2)? 0 : 1;
				if (c1 != c2) {
					return c1 - c2;
				}
				if (sortTables) {
					String pk1 = o1.cells.get(2);
					String pk2 = o2.cells.get(2);
					if (pk1.length() == 0 && pk2.length() > 0) return -1;
					if (pk1.length() > 0 && pk2.length() == 0) return 1;
				}
				for (int i = 0; i < 3; ++i) {
					int r = o1.cells.get(i).compareTo(o2.cells.get(i));
					if (r != 0) {
						return r;
					}
				}
				return 0;
			}
		});
	}

	/** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc=" Erzeugter Quelltext ">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jPanel6 = new javax.swing.JPanel();
        okButton = new javax.swing.JButton();
        cancelButton = new javax.swing.JButton();
        jPanel1 = new javax.swing.JPanel();
        jPanel2 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        tablesList = new javax.swing.JList();
        jPanel4 = new javax.swing.JPanel();
        newTable = new javax.swing.JButton();
        editTable = new javax.swing.JButton();
        deleteTables = new javax.swing.JButton();
        jPanel3 = new javax.swing.JPanel();
        jPanel5 = new javax.swing.JPanel();
        newAssociation = new javax.swing.JButton();
        editAssociation = new javax.swing.JButton();
        deleteAssociations = new javax.swing.JButton();
        jScrollPane2 = new javax.swing.JScrollPane();
        associationsList = new javax.swing.JList();
        info = new javax.swing.JLabel();

        getContentPane().setLayout(new java.awt.GridBagLayout());

        setTitle("Data Model Editor");
        setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);
        addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent evt) {
                formWindowClosing(evt);
            }
        });

        okButton.setText("Ok");
        okButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                okButtonActionPerformed(evt);
            }
        });

        jPanel6.add(okButton);

        cancelButton.setText("Cancel");
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });

        jPanel6.add(cancelButton);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        getContentPane().add(jPanel6, gridBagConstraints);

        jPanel1.setLayout(new java.awt.GridLayout(1, 0));

        jPanel2.setLayout(new java.awt.GridBagLayout());

        jPanel2.setBorder(javax.swing.BorderFactory.createTitledBorder("Tables"));
        tablesList.setModel(createTablesListModel());
        tablesList.addListSelectionListener(new javax.swing.event.ListSelectionListener() {
            public void valueChanged(javax.swing.event.ListSelectionEvent evt) {
                tablesListValueChanged(evt);
            }
        });

        jScrollPane1.setViewportView(tablesList);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel2.add(jScrollPane1, gridBagConstraints);

        jPanel4.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 2, 2));

        newTable.setText("Add");
        newTable.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                newTableActionPerformed(evt);
            }
        });

        jPanel4.add(newTable);

        editTable.setText("Edit");
        editTable.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                editTableActionPerformed(evt);
            }
        });

        jPanel4.add(editTable);

        deleteTables.setText("Delete");
        deleteTables.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                deleteTablesActionPerformed(evt);
            }
        });

        jPanel4.add(deleteTables);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel2.add(jPanel4, gridBagConstraints);

        jPanel1.add(jPanel2);

        jPanel3.setLayout(new java.awt.GridBagLayout());

        jPanel3.setBorder(javax.swing.BorderFactory.createTitledBorder("Associations"));
        jPanel5.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 2, 2));

        newAssociation.setText("Add");
        newAssociation.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                newAssociationActionPerformed(evt);
            }
        });

        jPanel5.add(newAssociation);

        editAssociation.setText("Edit");
        editAssociation.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                editAssociationActionPerformed(evt);
            }
        });

        jPanel5.add(editAssociation);

        deleteAssociations.setText("Delete");
        deleteAssociations.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                deleteAssociationsActionPerformed(evt);
            }
        });

        jPanel5.add(deleteAssociations);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel3.add(jPanel5, gridBagConstraints);

        associationsList.setModel(createAssociationsListModel());
        associationsList.addListSelectionListener(new javax.swing.event.ListSelectionListener() {
            public void valueChanged(javax.swing.event.ListSelectionEvent evt) {
                associationsListValueChanged(evt);
            }
        });

        jScrollPane2.setViewportView(associationsList);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 2.0;
        gridBagConstraints.weighty = 1.0;
        jPanel3.add(jScrollPane2, gridBagConstraints);

        jPanel1.add(jPanel3);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        getContentPane().add(jPanel1, gridBagConstraints);

        info.setForeground(new java.awt.Color(0, 153, 0));
        info.setText("jLabel1");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
        getContentPane().add(info, gridBagConstraints);

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void okButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_okButtonActionPerformed
        save();
    }//GEN-LAST:event_okButtonActionPerformed

    private void formWindowClosing(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_formWindowClosing
        close();
    }//GEN-LAST:event_formWindowClosing

    private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
        close();
    }//GEN-LAST:event_cancelButtonActionPerformed

    private void newAssociationActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_newAssociationActionPerformed
    	List<String> cells = new ArrayList<String>();
    	for (int i = 0; i < 100; ++i) {
    		cells.add("");
    	}
		CsvFile.Line line = new CsvFile.Line("?", cells);
    	if (new AssociationEditor(this, tables, associations).edit(line)) {
    		associations.add(0, line);
    		associationsList.setModel(createAssociationsListModel());
    		repaint();
    		markDirty();
    	}
    }//GEN-LAST:event_newAssociationActionPerformed

    private void editAssociationActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_editAssociationActionPerformed
    	CsvFile.Line line = null;
    	for (int i = 0; i < associations.size(); ++i) {
    		if (associationsList.getSelectionModel().isSelectedIndex(i)) {
    			line = associations.get(i);
    			break;
    		}
    	}
    	if (line != null) {
	    	if (new AssociationEditor(this, tables, associations).edit(line)) {
	    		markDirty();
	    		repaint();
	    	}
    	}
    }//GEN-LAST:event_editAssociationActionPerformed

    private void newTableActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_newTableActionPerformed
    	List<String> cells = new ArrayList<String>();
    	cells.add("");
    	cells.add("N");
    	cells.add("");
    	cells.add("Data Model Editor");
    	for (int i = 0; i < 100; ++i) {
    		cells.add("");
    	}
		CsvFile.Line line = new CsvFile.Line("?", cells);
    	if (new TableEditor(this, displayNames, tables, associations, excludeFromDeletion, initialDataTables).edit(line, columns)) {
    		tables.add(0, line);
    		tablesList.setModel(createTablesListModel());
    		markDirty();
    		repaint();
    	}
    }//GEN-LAST:event_newTableActionPerformed

    private void editTableActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_editTableActionPerformed
    	CsvFile.Line line = null;
    	for (int i = 0; i < tables.size(); ++i) {
    		if (tablesList.getSelectionModel().isSelectedIndex(i)) {
    			line = tables.get(i);
    			break;
    		}
    	}
    	if (line != null) {
	    	if (new TableEditor(this, displayNames, tables, associations, excludeFromDeletion, initialDataTables).edit(line, columns)) {
	    		markDirty();
	    		repaint();
	    	}
    	}
    }//GEN-LAST:event_editTableActionPerformed

    private void updateButtons() {
    	editTable.setEnabled(!tablesList.isSelectionEmpty());
    	deleteTables.setEnabled(!tablesList.isSelectionEmpty());
    	editAssociation.setEnabled(!associationsList.isSelectionEmpty());
    	deleteAssociations.setEnabled(!associationsList.isSelectionEmpty());
    }
    
    private void associationsListValueChanged(javax.swing.event.ListSelectionEvent evt) {//GEN-FIRST:event_associationsListValueChanged
    	updateButtons();
    }//GEN-LAST:event_associationsListValueChanged

    private void tablesListValueChanged(javax.swing.event.ListSelectionEvent evt) {//GEN-FIRST:event_tablesListValueChanged
    	updateButtons();
    }//GEN-LAST:event_tablesListValueChanged

    private void deleteTablesActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_deleteTablesActionPerformed
    	Collection<CsvFile.Line> toDelete = new ArrayList<CsvFile.Line>();
    	Collection<String> namesOfTablesToDelete = new ArrayList<String>();
    	for (int i = 0; i < tables.size(); ++i) {
    		if (tablesList.getSelectionModel().isSelectedIndex(i)) {
    			Line table = tables.get(i);
    			if (table != null) {
    				toDelete.add(table);
    				namesOfTablesToDelete.add(table.cells.get(0));
    			}
    		}
    	}
    	Collection<CsvFile.Line> assToDelete = new HashSet<CsvFile.Line>();
    	for (CsvFile.Line t: toDelete) {
    		for (CsvFile.Line a: associations) {
    			if (a.cells.get(0).equalsIgnoreCase(t.cells.get(0)) || a.cells.get(1).equalsIgnoreCase(t.cells.get(0))) {
    				assToDelete.add(a);
    			}
    		}	
    	}
    	if (JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(this, "Delete " + toDelete.size() + " tables with\n" + assToDelete.size() + " related associations?", "Delete Table", JOptionPane.YES_NO_OPTION)) {
	    	tables.removeAll(toDelete);
	    	for (String k: namesOfTablesToDelete) {
	    		displayNames.remove(k);
	    	}
	    	excludeFromDeletion.removeAll(namesOfTablesToDelete);
	    	initialDataTables.removeAll(namesOfTablesToDelete);
	    	tablesList.setModel(createTablesListModel());
	    	associations.removeAll(assToDelete);
	    	associationsList.setModel(createAssociationsListModel());
	    	for (Line l: toDelete) {
	    		columns.remove(l.cells.get(0));
	    	}
    		markDirty();
    	}
    }//GEN-LAST:event_deleteTablesActionPerformed

    private void deleteAssociationsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_deleteAssociationsActionPerformed
    	Collection<CsvFile.Line> toDelete = new ArrayList<CsvFile.Line>();
    	for (int i = 0; i < associations.size(); ++i) {
    		if (associationsList.getSelectionModel().isSelectedIndex(i)) {
    			toDelete.add(associations.get(i));
    		}
    	}
    	associations.removeAll(toDelete);
    	associationsList.setModel(createAssociationsListModel());
		markDirty();
    }//GEN-LAST:event_deleteAssociationsActionPerformed

    /**
     * Creates model for tables-list component.
     * 
     * @return model for tables-list component
     */
    private ListModel createTablesListModel() {
        DefaultListModel tablesListModel = new DefaultListModel();
        for (CsvFile.Line line: tables) {
        	tablesListModel.addElement(line);
        }
        return tablesListModel;
    }

    /**
     * Creates model for associations-list component.
     * 
     * @return model for associations-list component
     */
    private ListModel createAssociationsListModel() {
        DefaultListModel associationsListModel = new DefaultListModel();
        for (CsvFile.Line line: associations) {
        	associationsListModel.addElement(line);
        }
        return associationsListModel;
    }
    
    /**
     * Closes editor.
     */
    private void close() {
    	if (needsSave) {
    		if (JOptionPane.YES_OPTION != JOptionPane.showConfirmDialog(this, "Close without save?", "Close", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE)) {
    			return;
    		}
    	}
    	setVisible(false);
    }
    
    /**
     * Saves the model.
     */
    private void save() {
    	try {
    		if (needsSave) {
		    	save(tables, DataModel.getTablesFile(), "# Name; Upsert; Primary key; ; Author");
		    	save(associations, DataModel.getAssociationsFile(), "# Table A; Table B; First-insert; Cardinality; Join-condition; Name; Author");
		    	save(new ArrayList<Line>(columns.values()), DataModel.getColumnsFile(), "# Table; Columns");
	    		saveTableList(excludeFromDeletion, DataModel.getExcludeFromDeletionFile());
	    		saveTableList(initialDataTables, DataModel.getInitialDataTablesFile());
	    		saveTableList(Arrays.asList(Jailer.VERSION), DataModel.getVersionFile());
	    		saveDisplayNames();
	    		saved = true;
    		}
    	} catch (Throwable t) {
    		UIUtil.showException(this, "Error", t);
    	}
    	setVisible(false);
    }
    
    /**
     * Save the display names.
     */
    private void saveDisplayNames() throws FileNotFoundException {
    	PrintWriter out = new PrintWriter(DataModel.getDisplayNamesFile());
		out.println("# table; display name");
		for (Map.Entry<String, String> e: displayNames.entrySet()) {
			out.println(CsvFile.encodeCell(e.getKey()) + "; " + CsvFile.encodeCell(e.getValue()));
		}
		out.close();
	}

    /**
     * Save a table list.
     * 
     * @param tableList list to save
     * @param fileName the file to save into
     */
    private void saveTableList(List<String> tableList, String fileName)  throws FileNotFoundException {
    	PrintWriter out = new PrintWriter(fileName);
    	for (String table: tableList) {
			out.println(table);
		}
		out.close();
	}

	/**
     * Saves a list of csv-lines.
     * 
     * @param lines the lines
     * @param fileName the file to save into
     * @param title title line
     */
    private void save(List<Line> lines, String fileName, String title) throws FileNotFoundException {
    	PrintWriter out = new PrintWriter(fileName);
		out.println(title);
		for (Line line: lines) {
			out.println(line);
		}
		out.close();
	}

    // Variablendeklaration - nicht modifizieren//GEN-BEGIN:variables
    private javax.swing.JList associationsList;
    private javax.swing.JButton cancelButton;
    private javax.swing.JButton deleteAssociations;
    private javax.swing.JButton deleteTables;
    private javax.swing.JButton editAssociation;
    private javax.swing.JButton editTable;
    private javax.swing.JLabel info;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JButton newAssociation;
    private javax.swing.JButton newTable;
    private javax.swing.JButton okButton;
    private javax.swing.JList tablesList;
    // Ende der Variablendeklaration//GEN-END:variables
    
}
