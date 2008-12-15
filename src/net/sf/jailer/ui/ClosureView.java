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

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JLabel;
import javax.swing.JPopupMenu;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;

/**
 * Dialog for browsing through the closure of a table.
 *
 * @author Ralf Wisser
 */
public class ClosureView extends javax.swing.JDialog {

	/**
	 * Maximum number of tables in a closure-table's line.
	 */
	private final static int MAX_TABLES_PER_LINE = 3;
	
	/**
	 * The extraction model frame.
	 */
	private final ExtractionModelFrame extractionModelFrame;
	
	/**
	 * Currently selected table (in closure-table).
	 */
	private String selectedTable;
	
	/**
	 * Background colors per row.
	 */
	private final List<Color> bgColor = new ArrayList<Color>();
	
	/**
	 * Holds infos about a cell in the closure-table.
	 */
	private class CellInfo {
		public int row, column;
		List<String> pathToRoot = new ArrayList<String>();
	};

	/**
	 * Holds infos about a cell in the closure-table.
	 */
	private Map<String, CellInfo> cellInfo = new HashMap<String, CellInfo>();
	
    /** Creates new form FindDialog */
    public ClosureView(ExtractionModelFrame extractionModelFrame) {
        super(extractionModelFrame, false);
    	this.extractionModelFrame = extractionModelFrame;
        initComponents();
        closureTable = new JTable() {

			@Override
			public void paint(Graphics graphics) {
				super.paint(graphics);
				if (!(graphics instanceof Graphics2D)) return;
				Graphics2D g2d = (Graphics2D) graphics;
				CellInfo selectionInfo = cellInfo.get(selectedTable);
				if (selectionInfo == null) return;
				int[] x = new int[selectionInfo.pathToRoot.size() + 1];
				int[] y = new int[selectionInfo.pathToRoot.size() + 1];
				
				int pos = 0;
				for (String t: selectionInfo.pathToRoot) {
					CellInfo posInfo = cellInfo.get(t);
					Rectangle r = closureTable.getCellRect(posInfo.row, posInfo.column, false);
					x[pos] = (int) r.getCenterX();
					y[pos] = (int) r.getCenterY();
					++pos;
				}
				CellInfo posInfo = selectionInfo;
				Rectangle r = closureTable.getCellRect(posInfo.row, posInfo.column, false);
				x[pos] = (int) r.getCenterX();
				y[pos] = (int) r.getCenterY();
				++pos;
				Color color = new Color(0, 120, 255, 60);
    	    	g2d.setColor(color);
    	    	g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
    	    	g2d.setStroke(new BasicStroke(5));
        	    g2d.drawPolyline(x, y, pos);
			}
        };
        closureTable.setShowGrid(false);
        closureTable.setSurrendersFocusOnKeystroke(true);
        jScrollPane1.setViewportView(closureTable);

        closureTable.addMouseListener(new MouseListener() {
			public void mouseClicked(MouseEvent e) {
				// context menu
                if (SwingUtilities.isRightMouseButton(e)) {
                	int row = closureTable.rowAtPoint(e.getPoint());
                	int column = closureTable.columnAtPoint(e.getPoint());
                	if (row < 0 || column < 0) return;
                	Object value = closureTable.getModel().getValueAt(row, column);
                	if (value == null || !(value instanceof String)) return;
                	Table table = getDataModel().getTableByDisplayName((String) value);
                	if (table != null) {
						JPopupMenu popup = ClosureView.this.extractionModelFrame.extractionModelEditor.graphView.createPopupMenu(table);
						popup.show(e.getComponent(), e.getX(), e.getY());
                	}
                }
			}
			public void mouseEntered(MouseEvent e) {
			}
			public void mouseExited(MouseEvent e) {
			}
			public void mousePressed(MouseEvent e) {
			}
			public void mouseReleased(MouseEvent e) {
			}
        });
        
        final TableCellRenderer defaultTableCellRenderer = closureTable.getDefaultRenderer(String.class);
		closureTable.setDefaultRenderer(Object.class, new TableCellRenderer() {
			private Font font = new JLabel("normal").getFont();
			private Font normal = new Font(font.getName(), font.getStyle() & ~Font.BOLD, font.getSize());
			private Font bold = new Font(font.getName(), font.getStyle() | Font.BOLD, font.getSize());
			
			public Component getTableCellRendererComponent(JTable table,
					Object value, boolean isSelected, boolean hasFocus,
					int row, int column) {
				isSelected = selectedTable != null && selectedTable.equals(value);
				if (value == null || column < 1 || "".equals(value)) {
					hasFocus = false;
				}
				Component render = defaultTableCellRenderer.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
				if (render instanceof JLabel && !isSelected) {
					((JLabel) render).setBackground(bgColor.get(row));
				}
				CellInfo cellInfo = ClosureView.this.cellInfo.get(selectedTable);
				if (render instanceof JLabel) {
					((JLabel) render).setForeground(Color.BLACK);
					((JLabel) render).setFont(normal);
					if (cellInfo != null && selectedTable != null) {
						if (selectedTable.equals(value) || cellInfo.pathToRoot.contains(value)) {
							((JLabel) render).setFont(bold);
						}
					}
					Table t = getDataModel().getTableByDisplayName((String) value);
					if (t != null) {
						boolean allDisabled = true;
						boolean someRestricted = false;
						for (Association association: t.associations) {
							if (!association.isInsertDestinationBeforeSource()) {
								if (!association.isIgnored()) {
									allDisabled = false;
								}
							}
							if (association.isRestricted()) {
								someRestricted = true;
							}
						}
						if (allDisabled && someRestricted) {
							((JLabel) render).setForeground(new Color(160, 80, 0));
						} else if (!allDisabled && someRestricted) {
							((JLabel) render).setForeground(new Color(0, 80, 160));
						}
					}
				}
				return render;
			}
		});
		closureTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		closureTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			@Override
			public void valueChanged(ListSelectionEvent evt) {
				int col = closureTable.getSelectedColumn();
				int row = closureTable.getSelectedRow();
				if (col >= 1 && row >= 0) {
					String displayName = (String) closureTable.getModel().getValueAt(row, col);
					closureTable.getSelectionModel().clearSelection();
					if (displayName != null && !"".equals(displayName)) {
//						if (selectedTable == null || !selectedTable.equals(displayName)) {
							selectedTable = displayName;
							repaint();
							Table table = getDataModel().getTableByDisplayName(selectedTable);
							if (table != null) {
								CellInfo selectionInfo = cellInfo.get(selectedTable);
								if (selectionInfo != null) {
									Association association = null;
									if (selectionInfo.pathToRoot != null && selectionInfo.pathToRoot.size() > 0) {
										Table pre = getDataModel().getTableByDisplayName(selectionInfo.pathToRoot.get(selectionInfo.pathToRoot.size() - 1));
										if (pre != null) {
											for (Association a: pre.associations) {
												if (!a.isIgnored()) {
													if (a.destination.equals(table)) {
														association = a;
														break;
													}
												}
											}
										}
									}
									if (association != null && ClosureView.this.extractionModelFrame.extractionModelEditor.select(association)) {
										return;
									}
								}
								if (!ClosureView.this.extractionModelFrame.extractionModelEditor.select(table)) {
									ClosureView.this.extractionModelFrame.extractionModelEditor.setRootSelection(table);
								}
							}
//						}
					}
				}
			}
		});
		tableSelection.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
            	Table table = getDataModel().getTableByDisplayName((String) tableSelection.getSelectedItem());
				refresh(table);
            }
        });
		setLocation(400, 200);
        setSize(400, 400);
        setAlwaysOnTop(true);
    }
    
    /**
     * Gets current data model.
     * 
     * @return current data model
     */
    private DataModel getDataModel() {
    	return extractionModelFrame.extractionModelEditor.dataModel;
    }

    /**
     * Make {@link #refresh(Table)} reentrant.
     */
    private boolean refreshing = false;
    
    /**
     * Refreshes the dialog.
     * 
     * @param tableToSelect the table to select initially or <code>null</code> to keep the current selection
     */
    public void refresh(Table tableToSelect) {
    	if (refreshing) {
    		return;
    	}
    	refreshing = true;
    	
    	selectedTable = null;
    	
    	// table list model
    	if (tableToSelect == null) {
    		Object currentSelection = tableSelection.getSelectedItem();
    		if (currentSelection instanceof String) {
    			tableToSelect = getDataModel().getTableByDisplayName((String) currentSelection);
    		}
    	}
    	Vector<String> tableNames = new Vector<String>();
    	for (Table table: getDataModel().getTables()) {
    		tableNames.add(getDataModel().getDisplayName(table));
    	}
    	Collections.sort(tableNames);
    	DefaultComboBoxModel model = new DefaultComboBoxModel(tableNames);
        tableSelection.setModel(model);
    	if (tableToSelect != null) {
    		tableSelection.setSelectedItem(getDataModel().getDisplayName(tableToSelect));
    	} else {
    		tableSelection.setSelectedItem(0);
    	}
    	
    	// table model
    	refreshTableModel();

    	refreshing = false;
    }
    
    /**
     * Refreshes the dialog after the model has been changed.
     */
    public void refresh() {
    	String prevSelection = selectedTable;
    	refreshTableModel();
    	if (cellInfo.containsKey(prevSelection)) {
    		selectedTable = prevSelection;
    	} else {
    		selectedTable = null;
    	}
    	repaint();
    }
    
    /**
     * Refreshes the table model.
     */
    private void refreshTableModel() {
    	Table selectedTable = null;
    	cellInfo.clear();
    	Object currentSelection = tableSelection.getSelectedItem();
		if (currentSelection instanceof String) {
			selectedTable = getDataModel().getTableByDisplayName((String) currentSelection);
		}
		
		Object[] columns = new Object[MAX_TABLES_PER_LINE + 1];
		for (int i = 0; i < columns.length; ++i) {
			columns[i] = "";
		}
		columns[0] = "Distance";
		columns[1] = "Table";
		
		List<Object[]> data = new ArrayList<Object[]>();
		
		Set<String> visited = new HashSet<String>();
		List<String> currentLine = new ArrayList<String>();
		if (selectedTable != null) {
			String displayName = getDataModel().getDisplayName(selectedTable);
			currentLine.add(displayName);
			visited.add(displayName);
			CellInfo cellInfo = new CellInfo();
			cellInfo.column = 1;
			cellInfo.row = 0;
			this.cellInfo.put(displayName, cellInfo);
		}
		int distance = 0;
		final Color BG1 = new Color(255, 255, 255);
		final Color BG2 = new Color(230, 255, 255);
		bgColor.clear();
		bgColor.add(BG1);
		
		while (!currentLine.isEmpty()) {
			// add current line to table model
			Collections.sort(currentLine);
			Object[] lineAsObjects = new Object[MAX_TABLES_PER_LINE + 1];
			Arrays.fill(lineAsObjects, "");
			int col = 0;
			lineAsObjects[col++] = distance > 0? ("" + distance) : "";
			for (String t: currentLine) {
				CellInfo cellInfo = this.cellInfo.get(t);
				if (col <= MAX_TABLES_PER_LINE) {
					cellInfo.column = col;
					lineAsObjects[col++] = t;					
				} else {
					data.add(lineAsObjects);
					bgColor.add(distance % 2 == 0? BG1 : BG2);
					lineAsObjects = new Object[MAX_TABLES_PER_LINE + 1];
					Arrays.fill(lineAsObjects, "");
					col = 1;
					cellInfo.column = col;
					lineAsObjects[col++] = t;
				}
				cellInfo.row = data.size();
			}
			if (col > 1) {
				data.add(lineAsObjects);
				bgColor.add(distance % 2 != 0? BG1 : BG2);
			}
			
			// get next line
			List<String> nextLine = new ArrayList<String>();
			for (String t: currentLine) {
				Table table = getDataModel().getTableByDisplayName(t);
				if (table != null) {
					CellInfo cellInfoT = this.cellInfo.get(t);
					for (Association association: table.associations) {
						String displayName = getDataModel().getDisplayName(association.destination);
						if (!visited.contains(displayName) && !association.isIgnored()) {
							nextLine.add(displayName);
							visited.add(displayName);
							CellInfo cellInfo = new CellInfo();
							cellInfo.pathToRoot.addAll(cellInfoT.pathToRoot);
							cellInfo.pathToRoot.add(t);
							this.cellInfo.put(displayName, cellInfo);
						}
					}
				}
			}
			currentLine = nextLine;
			++distance;
		}
		
		Object[][] dataArray = (Object[][]) data.toArray(new Object[data.size()][]);
		DefaultTableModel tableModel = new DefaultTableModel(dataArray, columns) {
			public boolean isCellEditable(int row, int column) {
				return false;
			}
    	};
    	closureTable.setModel(tableModel);

		for (int i = 0; i < closureTable.getColumnCount(); i++) {
            TableColumn column = closureTable.getColumnModel().getColumn(i);
            int width = 1;
            
            Component comp = closureTable.getDefaultRenderer(String.class).
            						getTableCellRendererComponent(
            								closureTable, column.getHeaderValue(),
            								false, false, 0, i);
            width = Math.max(width, comp.getPreferredSize().width);

            for (int line = 0; line < dataArray.length; ++line) {
	            comp = closureTable.getDefaultRenderer(String.class).
	                             getTableCellRendererComponent(
	                            		 closureTable, dataArray[line][i],
	                                 false, false, line, i);
	            width = Math.max(width, comp.getPreferredSize().width);
            }
            
            column.setPreferredWidth(width);
        }
	}

	/** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc=" Erzeugter Quelltext ">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jPanel1 = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        tableSelection = new net.sf.jailer.ui.JComboBox();
        jScrollPane1 = new javax.swing.JScrollPane();
        closureTable = new javax.swing.JTable();
        jLabel2 = new javax.swing.JLabel();

        getContentPane().setLayout(new java.awt.GridBagLayout());

        setTitle("Closure Browser");
        jPanel1.setLayout(new java.awt.GridBagLayout());

        jLabel1.setText(" Table ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(jLabel1, gridBagConstraints);

        tableSelection.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Eintrag 1", "Eintrag 2", "Eintrag 3", "Eintrag 4" }));
        tableSelection.setMaximumRowCount(32);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 20;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 2);
        jPanel1.add(tableSelection, gridBagConstraints);

        closureTable.setModel(new javax.swing.table.DefaultTableModel(
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
        closureTable.setShowGrid(false);
        closureTable.setSurrendersFocusOnKeystroke(true);
        jScrollPane1.setViewportView(closureTable);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.gridwidth = 30;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 2, 0, 2);
        jPanel1.add(jScrollPane1, gridBagConstraints);

        jLabel2.setText(" Closure");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 15;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        jPanel1.add(jLabel2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        getContentPane().add(jPanel1, gridBagConstraints);

        pack();
    }// </editor-fold>//GEN-END:initComponents

    // Variablendeklaration - nicht modifizieren//GEN-BEGIN:variables
    private javax.swing.JTable closureTable;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JComboBox tableSelection;
    // Ende der Variablendeklaration//GEN-END:variables
    
}
