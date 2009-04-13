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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import javax.swing.JOptionPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableModel;

import net.sf.jailer.datamodel.Column;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.CsvFile.Line;

/**
 * Editor for single tables. Part of {@link DataModelEditor}.
 *
 * @author Ralf Wisser
 */
public class TableEditor extends javax.swing.JDialog {
    
	/**
	 * All tables (as csv-lines).
	 */
	private Collection<Line> tables;

	/**
	 * All associations (as csv-lines).
	 */
	private Collection<Line> associations;
	
    /**
     * List of tables to be excluded from deletion.
     */
    private List<String> excludeFromDeletionList = new ArrayList<String>();
    
    /**
     * List of tables to export entirely if in closure of subject.
     */
    private List<String> initialDataTablesList = new ArrayList<String>();
 
    private boolean needsSave;
    
    /** 
     * Creates new form TableEditor
     * 
     * @param tables all tables (as csv-lines)
     * @param associations all associations (as csv-line)
     * @param excludeFromDeletionList list of tables to be excluded from deletion
     * @param initialDataTablesList list of tables to export entirely if in closure of subject
     */
    public TableEditor(java.awt.Dialog parent, Collection<Line> tables, List<Line> associations, List<String> excludeFromDeletionList, List<String> initialDataTablesList) {
    	super(parent, true);
        this.tables = tables;
        this.associations = associations;
        this.excludeFromDeletionList = excludeFromDeletionList;
        this.initialDataTablesList = initialDataTablesList;
        initComponents();
        primaryKey.addActionListener(new ActionListener() {
        	@Override
			public void actionPerformed(ActionEvent e) {
        		int s = columnsTable.getSelectedRow();
        		updateButtonActionPerformed(e);
        		if (s >= 0) {
        			columnsTable.getSelectionModel().addSelectionInterval(s, s);
        		}
        	}
        });
        columnsTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        final Color BG1 = new Color(255, 255, 255);
		final Color BG2 = new Color(230, 255, 255);
		TableCellRenderer renderer = new DefaultTableCellRenderer() {
			@Override
			public Component getTableCellRendererComponent(JTable table,
					Object value, boolean isSelected, boolean hasFocus,
					int row, int column) {
				Component render = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
				if (!isSelected) {
					render.setBackground(row % 2 == 0? BG1 : BG2);
				}
				Column col = Column.parse(currentColumnLine.cells.get(1 + row));
				render.setForeground(Color.BLACK);
				for (Column pk: getCurrentPrimaryKeys()) {
					if (pk.name.equals(col.name)) {
						render.setForeground(Color.RED);
						break;
					}
				}
				return render;
			}
        };
        columnsTable.setDefaultRenderer(Object.class, renderer);
        columnsTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent e) {
				if (columnsTable.getSelectedRow() < 0) {
					column.setText("");
					primaryKey.setSelected(false);
				} else {
					String selectedColumn = currentColumnLine.cells.get(1 + columnsTable.getSelectedRow());
					column.setText(selectedColumn);
					Column selCol = Column.parse(selectedColumn);
					primaryKey.setSelected(false);
					for (Column pk: getCurrentPrimaryKeys()) {
						if (pk.name.equals(selCol.name)) {
							primaryKey.setSelected(true);
							break;
						}
					}
				}
				updateEnableState();
			}
        });
        setSize(600, 350);
        setLocation(parent.getLocation().x + parent.getSize().width/2 - getPreferredSize().width/2,
    			parent.getLocation().y + parent.getSize().height/2 - getPreferredSize().height/2);
        
        column.getDocument().addDocumentListener(new DocumentListener() {
			public void changedUpdate(DocumentEvent e) {
				updateEnableState();
			}
			public void insertUpdate(DocumentEvent e) {
				updateEnableState();
			}
			public void removeUpdate(DocumentEvent e) {
				updateEnableState();
			}
        });

        UIUtil.initPeer();
    }
    
    /**
     * Gets list of current primary key columns.
     * 
     * @return list of current primary key columns
     */
	private List<Column> getCurrentPrimaryKeys() {
		List<Column> pk = new ArrayList<Column>();
		for (int i = 2; i < currentTableLine.length; ++i) {
			if (currentTableLine.cells.get(i).length() == 0) {
				break;
			}
			pk.add(Column.parse(currentTableLine.cells.get(i)));
		}
		return pk;
	}
	
	/**
     * Gets list of current columns.
     * 
     * @return list of current columns
     */
	private List<Column> getCurrentColumns() {
		List<Column> columns = new ArrayList<Column>();
		for (int i = 1; i < currentColumnLine.length; ++i) {
			if (currentColumnLine.cells.get(i).length() == 0) {
				break;
			}
			columns.add(Column.parse(currentColumnLine.cells.get(i)));
		}
		return columns;
	}

	/**
     * Sets list of current columns.
     * 
     * @return list of current columns
     */
	private void setCurrentColumns(List<Column> columns) {
		int i = 1;
		for (Column column: columns) {
			currentColumnLine.cells.set(i++, column.toSQL(null));
		}
		currentColumnLine.cells.set(i++, "");
		currentColumnLine.cells.set(i++, "Data Model Editor");
		currentColumnLine.length = i;
	}

	/** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc=" Erzeugter Quelltext ">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        nameField = new javax.swing.JTextField();
        upsertCheckbox = new javax.swing.JCheckBox();
        jPanel1 = new javax.swing.JPanel();
        jButton1 = new javax.swing.JButton();
        jLabel3 = new javax.swing.JLabel();
        jButton2 = new javax.swing.JButton();
        jLabel5 = new javax.swing.JLabel();
        jLabel6 = new javax.swing.JLabel();
        excludeFromDeletion = new javax.swing.JCheckBox();
        exportAllRows = new javax.swing.JCheckBox();
        jPanel2 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        columnsTable = new javax.swing.JTable();
        upButton = new javax.swing.JButton();
        downButton = new javax.swing.JButton();
        jPanel3 = new javax.swing.JPanel();
        column = new javax.swing.JTextField();
        addButton = new javax.swing.JButton();
        updateButton = new javax.swing.JButton();
        deleteButton = new javax.swing.JButton();
        primaryKey = new javax.swing.JCheckBox();
        jLabel4 = new javax.swing.JLabel();

        getContentPane().setLayout(new java.awt.GridBagLayout());

        setTitle("Table");
        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        jLabel1.setText(" Name ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jLabel1, gridBagConstraints);

        jLabel2.setText(" Columns ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 9;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        getContentPane().add(jLabel2, gridBagConstraints);

        nameField.setText("jTextField1j");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        getContentPane().add(nameField, gridBagConstraints);

        upsertCheckbox.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        upsertCheckbox.setMargin(new java.awt.Insets(0, 0, 0, 0));
        upsertCheckbox.setText(" generate 'Upsert'-statements for exported rows");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(8, 0, 0, 0);
        getContentPane().add(upsertCheckbox, gridBagConstraints);

        jPanel1.setLayout(new java.awt.GridBagLayout());

        jButton1.setText("Ok");
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridheight = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTH;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 0);
        jPanel1.add(jButton1, gridBagConstraints);

        jLabel3.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel3.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(8, 0, 0, 0);
        jPanel1.add(jLabel3, gridBagConstraints);

        jButton2.setText("Cancel");
        jButton2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton2ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridheight = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTH;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 0);
        jPanel1.add(jButton2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 40;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        getContentPane().add(jPanel1, gridBagConstraints);

        jLabel5.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        getContentPane().add(jLabel5, gridBagConstraints);

        jLabel6.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 10;
        getContentPane().add(jLabel6, gridBagConstraints);

        excludeFromDeletion.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        excludeFromDeletion.setMargin(new java.awt.Insets(0, 0, 0, 0));
        excludeFromDeletion.setText(" exclude from deletion");
        excludeFromDeletion.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                excludeFromDeletionActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 14;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        getContentPane().add(excludeFromDeletion, gridBagConstraints);

        exportAllRows.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        exportAllRows.setMargin(new java.awt.Insets(0, 0, 0, 0));
        exportAllRows.setText(" export all rows if in closure of subject");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 18;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        getContentPane().add(exportAllRows, gridBagConstraints);

        jPanel2.setLayout(new java.awt.GridBagLayout());

        columnsTable.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null}
            },
            new String [] {
                "Titel 1", "Titel 2", "Titel 3", "Titel 4"
            }
        ));
        columnsTable.setShowGrid(false);
        jScrollPane1.setViewportView(columnsTable);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridheight = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        jPanel2.add(jScrollPane1, gridBagConstraints);

        upButton.setText("Up");
        upButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                upButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(4, 2, 0, 0);
        jPanel2.add(upButton, gridBagConstraints);

        downButton.setText("Down");
        downButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                downButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 0, 0);
        jPanel2.add(downButton, gridBagConstraints);

        jPanel3.setLayout(new java.awt.GridBagLayout());

        column.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                columnActionPerformed(evt);
            }
        });
        column.addInputMethodListener(new java.awt.event.InputMethodListener() {
            public void caretPositionChanged(java.awt.event.InputMethodEvent evt) {
            }
            public void inputMethodTextChanged(java.awt.event.InputMethodEvent evt) {
                columnInputMethodTextChanged(evt);
            }
        });
        column.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyReleased(java.awt.event.KeyEvent evt) {
                columnKeyReleased(evt);
            }
            public void keyTyped(java.awt.event.KeyEvent evt) {
                columnKeyTyped(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 22, 0, 0);
        jPanel3.add(column, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 4, 0);
        jPanel2.add(jPanel3, gridBagConstraints);

        addButton.setText("Add");
        addButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                addButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 0, 0);
        jPanel2.add(addButton, gridBagConstraints);

        updateButton.setText("Update");
        updateButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                updateButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 0, 0);
        jPanel2.add(updateButton, gridBagConstraints);

        deleteButton.setText("Delete");
        deleteButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                deleteButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 0, 0);
        jPanel2.add(deleteButton, gridBagConstraints);

        primaryKey.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        primaryKey.setMargin(new java.awt.Insets(0, 0, 0, 0));
        primaryKey.setText(" primary key");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 22, 0, 0);
        jPanel2.add(primaryKey, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 9;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        getContentPane().add(jPanel2, gridBagConstraints);

        jLabel4.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.weightx = 1.0;
        getContentPane().add(jLabel4, gridBagConstraints);

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void deleteButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_deleteButtonActionPerformed
    	try {
    		Column column = Column.parse(this.column.getText());
    		for (int i = 1; i < currentColumnLine.length; ++i) {
    			if (Column.parse(currentColumnLine.cells.get(i)).name.equals(column.name)) {
    				currentColumnLine.cells.remove(i);
    				--currentColumnLine.length;
    				break;
    			}
    		}
    		for (int i = 2; i < currentTableLine.length; ++i) {
    			if (currentTableLine.cells.get(i).length() == 0) {
    				break;
    			}
    			if (Column.parse(currentTableLine.cells.get(i)).name.equals(column.name)) {
    				currentTableLine.cells.remove(i);
    				--currentTableLine.length;
    				break;
    			}
    		}
    		columnsTable.setModel(columnsTableModel());
    		updateEnableState();
    		needsSave = true;
    	} catch (Exception e) {
    		// ignore
    	}
    }//GEN-LAST:event_deleteButtonActionPerformed

    private void updateButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_updateButtonActionPerformed
    	try {
    		Column column = Column.parse(this.column.getText());
    		for (int i = 1; i < currentColumnLine.length; ++i) {
    			if (Column.parse(currentColumnLine.cells.get(i)).name.equals(column.name)) {
    				currentColumnLine.cells.set(i, column.toSQL(null));
    				break;
    			}
    		}
    		boolean found = false;
    		for (int i = 2; i < currentTableLine.length; ++i) {
    			if (currentTableLine.cells.get(i).length() == 0) {
    				break;
    			}
    			if (Column.parse(currentTableLine.cells.get(i)).name.equals(column.name)) {
    				found = true;
    				if (primaryKey.isSelected()) {
    					currentTableLine.cells.set(i, column.toSQL(null));
    				} else {
    					currentTableLine.cells.remove(i);
    					currentTableLine.length--;
    				}
    				break;
    			}
    		}
    		if (!found && primaryKey.isSelected()) {
    			currentTableLine.cells.add(currentTableLine.length - 2, column.toSQL(null));
        		currentTableLine.length++;
    		}
    		columnsTable.setModel(columnsTableModel());
    		updateEnableState();
    		needsSave = true;
    	} catch (Exception e) {
    		// ignore
    	}
    }//GEN-LAST:event_updateButtonActionPerformed

    private void addButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_addButtonActionPerformed
    	try {
    		Column column = Column.parse(this.column.getText());
    		currentColumnLine.cells.add(currentColumnLine.length, column.toSQL(null));
    		currentColumnLine.length++;
    		if (primaryKey.isSelected()) {
        		currentTableLine.cells.add(currentTableLine.length - 2, column.toSQL(null));
        		currentTableLine.length++;
    		}
    		columnsTable.setModel(columnsTableModel());
    		updateEnableState();
    		needsSave = true;
    	} catch (Exception e) {
    		// ignore
    	}
    }//GEN-LAST:event_addButtonActionPerformed

    private void downButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_downButtonActionPerformed
    	int i = columnsTable.getSelectedRow();
    	if (i >= 0 && i < currentColumnLine.length - 2) {
    		swapColumns(i, i + 1);
    	}
    }//GEN-LAST:event_downButtonActionPerformed

    private void upButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_upButtonActionPerformed
    	int i = columnsTable.getSelectedRow();
    	if (i > 0) {
    		swapColumns(i, i - 1);
    	}
    }//GEN-LAST:event_upButtonActionPerformed

    /**
     * Swaps two columns in current column line.
     * 
     * @param i index of column a
     * @param j index of column b
     */
    private void swapColumns(int i, int j) {
		String h = currentColumnLine.cells.get(i + 1);
		currentColumnLine.cells.set(i + 1, currentColumnLine.cells.get(j + 1));
		currentColumnLine.cells.set(j + 1, h);
		columnsTable.setModel(columnsTableModel());
		columnsTable.getSelectionModel().setSelectionInterval(j, j);
		needsSave = true;
	}

	private void columnKeyReleased(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_columnKeyReleased
    }//GEN-LAST:event_columnKeyReleased

    private void columnInputMethodTextChanged(java.awt.event.InputMethodEvent evt) {//GEN-FIRST:event_columnInputMethodTextChanged
    }//GEN-LAST:event_columnInputMethodTextChanged

    private void columnKeyTyped(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_columnKeyTyped
    }//GEN-LAST:event_columnKeyTyped

    private void columnActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_columnActionPerformed
    }//GEN-LAST:event_columnActionPerformed

    private void excludeFromDeletionActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_excludeFromDeletionActionPerformed
    }//GEN-LAST:event_excludeFromDeletionActionPerformed

    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
    	String msg = null;
    	if (nameField.getText().trim().length() == 0) {
    		msg = "No table name";
    	} else if (getCurrentPrimaryKeys().isEmpty()) {
    		msg = "Primary key missing";
    	} else {
    		for (Line l: tables) {
    			if (l != currentTableLine && l.cells.get(0).equalsIgnoreCase(nameField.getText().trim())) {
    				msg = "Table already exists";
    				break;
    			}
    		}
    	}
    	if (msg != null) {
    		JOptionPane.showMessageDialog(this, msg, "Error", JOptionPane.ERROR_MESSAGE);
    	} else {
    		isOk = true;
    		setVisible(false);
    	}
    }//GEN-LAST:event_jButton1ActionPerformed

    private void jButton2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton2ActionPerformed
        setVisible(false);
    }//GEN-LAST:event_jButton2ActionPerformed
    
    private boolean isOk;
    private Line currentTableLine;
    private Line currentColumnLine;
    
    /**
     * Edits a table (as csv-line).
     * 
     * @param tableLine the table-line
     * @return <code>true</code> if line was modified
     */
	public boolean edit(Line tableLine, Map<String, Line> columnLines) {
		needsSave = false;
		currentTableLine = tableLine;
		currentColumnLine = columnLines.get(tableLine.cells.get(0));
		if (currentColumnLine == null) {
			currentColumnLine = new CsvFile.Line("", new ArrayList<String>(Arrays.asList(tableLine.cells.get(0))));
			if (currentColumnLine.length == 0) {
				// new table without name
				currentColumnLine.length = 1;
			}
		}
		
		List<String> oldTableLineCells = new ArrayList<String>(currentTableLine.cells);
		int oldTableLineLength = currentTableLine.length;
		List<String> oldColumnLineCells = new ArrayList<String>(currentColumnLine.cells);
		int oldColumnLineLength = currentColumnLine.length;
		
		nameField.setText(tableLine.cells.get(0));
		upsertCheckbox.setSelected("Y".equals(tableLine.cells.get(1)));
		
		String origName = nameField.getText().trim();
		boolean origUpsert = upsertCheckbox.isSelected();
		isOk = false;

		boolean origExcludeSet = excludeFromDeletionList.contains(origName);
		excludeFromDeletion.setSelected(origExcludeSet);
		boolean origExportAllSet = initialDataTablesList.contains(origName);
		exportAllRows.setSelected(origExportAllSet);

		columnsTable.setModel(columnsTableModel());
		updateEnableState();
		
		setVisible(true);
		boolean excludeSet = excludeFromDeletion.isSelected();
		boolean exportAllSet = exportAllRows.isSelected();

		if (isOk && 
			    !(origName.equals(nameField.getText()) 
				&& !needsSave
				&& origUpsert == upsertCheckbox.isSelected()
				&& origExcludeSet == excludeSet
				&& origExportAllSet == exportAllSet)) {
			columnLines.remove(currentColumnLine.cells.get(0));
			currentColumnLine.cells.set(0, nameField.getText().trim());
			columnLines.put(currentColumnLine.cells.get(0), currentColumnLine);

			tableLine.cells.set(0, nameField.getText().trim());
			tableLine.cells.set(1, upsertCheckbox.isSelected()? "Y" : "N");
			
			//rename associations source/destination
			for (Line a: associations) {
				if (a.cells.get(0).equalsIgnoreCase(origName.trim())) {
					a.cells.set(0, nameField.getText().trim());
				}
				if (a.cells.get(1).equalsIgnoreCase(origName.trim())) {
					a.cells.set(1, nameField.getText().trim());
				}
			}
			
			excludeFromDeletionList.remove(origName);
			initialDataTablesList.remove(origName);
			
			if (excludeSet) {
				excludeFromDeletionList.add(nameField.getText().trim());
			}
			if (exportAllSet) {
				initialDataTablesList.add(nameField.getText().trim());
			}
			
			return true;
		} else {
			currentTableLine.cells.clear();
			currentTableLine.cells.addAll(oldTableLineCells);
			currentTableLine.length = oldTableLineLength;
			currentColumnLine.cells.clear();
			currentColumnLine.cells.addAll(oldColumnLineCells);
			currentColumnLine.length = oldColumnLineLength;
		}
		return false;
	}

	/**
	 * Updates enable-state of each button.
	 */
	private void updateEnableState() {
		Column current;
		try {
			current = Column.parse(column.getText());
		} catch (Exception e) {
			current = null;
		}
		boolean currentExists = false;
		if (current != null) {
			for (Column c: getCurrentColumns()) {
				if (c.name.equals(current.name)) {
					currentExists = true;
					break;
				}
			}
		}
		addButton.setEnabled(current != null && !currentExists);
		updateButton.setEnabled(current != null && currentExists);
		deleteButton.setEnabled(current != null && currentExists);
		upButton.setEnabled(columnsTable.getSelectedRow() > 0);
		downButton.setEnabled(columnsTable.getSelectedRow() >= 0 && columnsTable.getSelectedRow() < currentColumnLine.length - 2);
	}

	/**
	 * Creates the model for the columns table.
	 * 
	 * @return the model for the columns table
	 */
    private TableModel columnsTableModel() {
    	DefaultTableModel model = new DefaultTableModel() {
			public boolean isCellEditable(int row, int column) {
				return false;
			}
    	};
    	model.addColumn("Name");
    	model.addColumn("Type");
    	model.addColumn("Scale");
    	model.addColumn("Precision");
    	for (int i = 1; i < currentColumnLine.length; ++i) {
    		Column column = Column.parse(currentColumnLine.cells.get(i));
        	model.addRow(new Object[] { column.name, column.type, column.length > 0? column.length : "", column.precision >= 0? column.precision : ""});
    	}
    	return model;
	}

    // Variablendeklaration - nicht modifizieren//GEN-BEGIN:variables
    private javax.swing.JButton addButton;
    private javax.swing.JTextField column;
    private javax.swing.JTable columnsTable;
    private javax.swing.JButton deleteButton;
    private javax.swing.JButton downButton;
    private javax.swing.JCheckBox excludeFromDeletion;
    private javax.swing.JCheckBox exportAllRows;
    private javax.swing.JButton jButton1;
    private javax.swing.JButton jButton2;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTextField nameField;
    private javax.swing.JCheckBox primaryKey;
    private javax.swing.JButton upButton;
    private javax.swing.JButton updateButton;
    private javax.swing.JCheckBox upsertCheckbox;
    // Ende der Variablendeklaration//GEN-END:variables
    
}
