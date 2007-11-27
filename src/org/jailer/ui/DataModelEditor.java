/*
 * DataModelEditor.java
 *
 * Created on 26. November 2007, 09:12
 */

package org.jailer.ui;

import java.awt.Color;
import java.awt.Component;
import java.awt.HeadlessException;
import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import javax.swing.DefaultListCellRenderer;
import javax.swing.DefaultListModel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.ListCellRenderer;
import javax.swing.ListModel;

import org.jailer.datamodel.DataModel;
import org.jailer.modelbuilder.ModelBuilder;
import org.jailer.util.CsvFile;
import org.jailer.util.CsvFile.Line;

/**
 * Data Model Editor.
 *
 * @author Wisser
 */
public class DataModelEditor extends javax.swing.JDialog {

	/**
	 * List of tables.
	 */
	private List<CsvFile.Line> tables;
	
	/**
	 * List of associations.
	 */
	private List<CsvFile.Line> associations;
	
	/**
	 * Table- and association definitions from model-finder result files.
	 */
    private List<CsvFile.Line> linesFromModelFinder = new ArrayList<CsvFile.Line>();
    
    /** 
     * Creates new form DataModelEditor.
     */
    public DataModelEditor(java.awt.Frame parent, boolean merge) throws Exception {
        super(parent, true);
        tables = new CsvFile(new File(DataModel.TABLES_FILE)).getLines();
        associations = new CsvFile(new File(DataModel.ASSOCIATIONS_FILE)).getLines();
        int newTables = 0;
        int newAssociations = 0;
        File modelFinderTablesFile = new File(ModelBuilder.MODEL_BUILDER_TABLES_CSV);
		if (merge && modelFinderTablesFile.exists()) {
	        List<CsvFile.Line> tablesFromModelFinder = new CsvFile(modelFinderTablesFile).getLines();
	        tables.addAll(tablesFromModelFinder);
	        linesFromModelFinder.addAll(tablesFromModelFinder);
	        newTables += tablesFromModelFinder.size();
		}
		sortLineList(tables);
        File modelFinderAssociationsFile = new File(ModelBuilder.MODEL_BUILDER_ASSOCIATIONS_CSV);
		if (merge && modelFinderAssociationsFile.exists()) {
	        List<CsvFile.Line> associationsFromModelFinder = new CsvFile(modelFinderAssociationsFile).getLines();
	        associations.addAll(associationsFromModelFinder);
	        linesFromModelFinder.addAll(associationsFromModelFinder);
	        newAssociations += associationsFromModelFinder.size();
		}
		sortLineList(associations);
		initComponents();
		setSize(900, 700);
		setLocation(100, 100);
		if (merge) {
			info.setText("Found " + newTables + " new tables and " + newAssociations + " new associations");
		}
		info.setVisible(merge);
		updateButtons();
        
		final Color BG_COLOR = new Color(0.8f, 1.0f, 0.8f);
		final Color BG_SELCOLOR = new Color(0.4f, 1.0f, 0.4f);
    	
        ListCellRenderer tablesListItemRenderer = new DefaultListCellRenderer() {
			@Override
			public Component getListCellRendererComponent(JList list,
					Object value,
                    int index,
                    boolean isSelected,
                    boolean cellHasFocus) {
				boolean fromModelFinder = linesFromModelFinder.contains(value);
				CsvFile.Line line = (CsvFile.Line) value;
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
				if (fromModelFinder) {
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
				value = line.cells.get(0) + " " + type + " " + line.cells.get(1) + " " + line.cells.get(3) + " on " + line.cells.get(4);
				Component render = super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
				if (fromModelFinder) {
					render.setBackground(isSelected? BG_SELCOLOR: BG_COLOR);
				}
				return render;
			}
        };
    
        tablesList.setCellRenderer(tablesListItemRenderer);
        associationsList.setCellRenderer(associationsListItemRenderer);
    }
    
    /**
     * Sorts lists of csv-lines.
     * 
     * @param lines the list to sort
     */
    private void sortLineList(List<CsvFile.Line> list) {
		Collections.sort(list, new Comparator<CsvFile.Line> () {
			public int compare(CsvFile.Line o1, CsvFile.Line o2) {
				int c1 = linesFromModelFinder.contains(o1)? 0 : 1;
				int c2 = linesFromModelFinder.contains(o2)? 0 : 1;
				if (c1 != c2) {
					return c1 - c2;
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

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Data Model Editor");
        okButton.setText("Ok");
        jPanel6.add(okButton);

        cancelButton.setText("Cancel");
        jPanel6.add(cancelButton);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        getContentPane().add(jPanel6, gridBagConstraints);

        jPanel1.setLayout(new java.awt.GridLayout(1, 0));

        jPanel2.setLayout(new java.awt.GridBagLayout());

        jPanel2.setBorder(javax.swing.BorderFactory.createTitledBorder("Table"));
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

        jPanel3.setBorder(javax.swing.BorderFactory.createTitledBorder("Association"));
        jPanel5.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 2, 2));

        newAssociation.setText("Add");
        jPanel5.add(newAssociation);

        editAssociation.setText("Edit");
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

        info.setText("jLabel1");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
        getContentPane().add(info, gridBagConstraints);

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void newTableActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_newTableActionPerformed
    	List<String> cells = new ArrayList<String>();
    	for (int i = 0; i < 100; ++i) {
    		cells.add("");
    	}
		CsvFile.Line line = new CsvFile.Line("?", cells);
    	if (new TableEditor(this, tables, associations).edit(line)) {
    		tables.add(0, line);
    		tablesList.setModel(createTablesListModel());
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
	    	if (new TableEditor(this, tables, associations).edit(line)) {
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
    	for (int i = 0; i < tables.size(); ++i) {
    		if (tablesList.getSelectionModel().isSelectedIndex(i)) {
    			toDelete.add(tables.get(i));
    		}
    	}
    	Collection<CsvFile.Line> assToDelete = new ArrayList<CsvFile.Line>();
    	for (CsvFile.Line t: toDelete) {
    		for (CsvFile.Line a: associations) {
    			if (a.cells.get(0).equalsIgnoreCase(t.cells.get(0)) || a.cells.get(1).equalsIgnoreCase(t.cells.get(0))) {
    				assToDelete.add(a);
    			}
    		}	
    	}
    	if (JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(this, "Delete " + toDelete.size() + " tables with\n" + assToDelete.size() + " related associations?", "Delete Table", JOptionPane.YES_NO_OPTION)) {
	    	tables.removeAll(toDelete);
	    	tablesList.setModel(createTablesListModel());
	    	associations.removeAll(assToDelete);
	    	associationsList.setModel(createAssociationsListModel());
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
     * @param args the command line arguments
     */
    public static void main(String args[]) {
        java.awt.EventQueue.invokeLater(new Runnable() {
            public void run() {
                try {
					new DataModelEditor(new javax.swing.JFrame(), true).setVisible(true);
				} catch (HeadlessException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (Exception e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
            }
        });
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
