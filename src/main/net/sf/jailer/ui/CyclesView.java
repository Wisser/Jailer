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

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

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

import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.CancellationException;
import net.sf.jailer.util.CancellationHandler;
import net.sf.jailer.util.CycleFinder;

/**
 * Shows dependency cycles.
 *
 * @author Ralf Wisser
 */
public class CyclesView extends javax.swing.JDialog {

	/**
	 * Maximum number of tables in a view's line.
	 */
	private final static int MAX_TABLES_PER_LINE = 4;
	
	/**
	 * The extraction model frame.
	 */
	private final ExtractionModelFrame extractionModelFrame;
	
	/**
	 * Currently selected table.
	 */
	private String selectedTable;
	
	/**
	 * Background colors per row.
	 */
	private final List<Color> bgColor = new ArrayList<Color>();
	
	/**
	 * Holds infos about a cell in the view.
	 */
	private class CellInfo {
		public int row, column;
		boolean arrowLeft, arrowRight;
	};

	/**
	 * Holds infos about a cell in the view.
	 */
	private Set<CellInfo> cellInfo = new HashSet<CellInfo>();
	
    /** Creates new form FindDialog */
    public CyclesView(ExtractionModelFrame extractionModelFrame) {
        super(extractionModelFrame, false);
    	this.extractionModelFrame = extractionModelFrame;
        initComponents();
        
        cyclesTable = new JTable() {
			private static final long serialVersionUID = 8960056200057023368L;

			/**
			 * Paints selected path.
			 */
			@Override
			public void paint(Graphics graphics) {
				super.paint(graphics);
				if (!(graphics instanceof Graphics2D)) return;
				Graphics2D g2d = (Graphics2D) graphics;
				for (CellInfo posInfo: cellInfo) {
					Rectangle r = cyclesTable.getCellRect(posInfo.row, posInfo.column, false);
        	    	Color color = new Color(255, 0, 0, 150);
        	    	g2d.setColor(color);
        	    	g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        	    	g2d.setStroke(new BasicStroke(2));
	        	    int a = r.height / 4;
	        	    int w = 3 * a;
	        	    if (posInfo.arrowLeft) {
		        	    int x = r.x - w - a * 2;
		        	    int y = r.y + r.height / 2;
		        	    g2d.drawLine(x, y, x + w, y);
		        	    g2d.drawLine(x + w - 1, y, x + w - a, y - a);
		        	    g2d.drawLine(x + w - 1, y, x + w - a, y + a);
	        	    }
	        	    if (posInfo.arrowRight) {
		        	    int x = r.x + r.width - w - a * 2;
		        	    int y = r.y + r.height / 2;
		        	    g2d.drawLine(x, y, x + w, y);
		        	    g2d.drawLine(x + w - 1, y, x + w - a, y - a);
		        	    g2d.drawLine(x + w - 1, y, x + w - a, y + a);
	        	    }
				}
			}
        };
        cyclesTable.setShowGrid(false);
        cyclesTable.setSurrendersFocusOnKeystroke(true);
        jScrollPane1.setViewportView(cyclesTable);
        jScrollPane1.setColumnHeaderView(null);
        
        cyclesTable.addMouseListener(new MouseListener() {
			public void mouseClicked(MouseEvent e) {
				// context menu
                if (SwingUtilities.isRightMouseButton(e)) {
                	int row = cyclesTable.rowAtPoint(e.getPoint());
                	int column = cyclesTable.columnAtPoint(e.getPoint());
                	if (row < 0 || column < 0) return;
                	Object value = cyclesTable.getModel().getValueAt(row, column);
                	if (value == null || !(value instanceof String)) return;
                	Table table = getDataModel().getTableByDisplayName((String) value);
                	if (table != null) {
						JPopupMenu popup = CyclesView.this.extractionModelFrame.extractionModelEditor.graphView.createPopupMenu(table, false);
						UIUtil.fit(popup);
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
        
        final TableCellRenderer defaultTableCellRenderer = cyclesTable.getDefaultRenderer(String.class);
		cyclesTable.setDefaultRenderer(Object.class, new TableCellRenderer() {
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
				if (render instanceof JLabel) {
					((JLabel) render).setForeground(Color.BLACK);
					((JLabel) render).setFont(normal);
					((JLabel) render).setToolTipText(((JLabel) render).getText());
					if (selectedTable != null) {
						if (selectedTable.equals(value)) {
							((JLabel) render).setFont(bold);
						}
					}
				}
				return render;
			}
		});
		cyclesTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		cyclesTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			@Override
			public void valueChanged(ListSelectionEvent evt) {
				int col = cyclesTable.getSelectedColumn();
				int row = cyclesTable.getSelectedRow();
				if (col >= 1 && row >= 0) {
					String displayName = (String) cyclesTable.getModel().getValueAt(row, col);
					cyclesTable.getSelectionModel().clearSelection();
					if (displayName != null && !"".equals(displayName)) {
//						if (selectedTable == null || !selectedTable.equals(displayName)) {
							selectedTable = displayName;
							repaint();
							Table table = getDataModel().getTableByDisplayName(selectedTable);
							if (table != null) {
								if (!CyclesView.this.extractionModelFrame.extractionModelEditor.select(table)) {
									CyclesView.this.extractionModelFrame.extractionModelEditor.setRootSelection(table);
								}
							}
//						}
					}
				}
			}
		});
		setLocation(500, 150);
        setSize(600, 400);
//        setAlwaysOnTop(true);
    }

    private class FindCyclesDialog extends javax.swing.JDialog {
        public FindCyclesDialog() {
            super(extractionModelFrame, false);
            initComponents();
            UIUtil.initPeer();
        }
        
        private void initComponents() {
            java.awt.GridBagConstraints gridBagConstraints;

            label = new javax.swing.JLabel();
            jButton1 = new javax.swing.JButton();

            setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);
            getContentPane().setLayout(new java.awt.GridBagLayout());

            label.setText("       Finding cycles...     ");
            gridBagConstraints = new java.awt.GridBagConstraints();
            gridBagConstraints.gridx = 0;
            gridBagConstraints.gridy = 0;
            gridBagConstraints.gridwidth = 2;
            gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
            gridBagConstraints.insets = new java.awt.Insets(12, 12, 12, 12);
            getContentPane().add(label, gridBagConstraints);

            jButton1.setText(" Abort ");
            jButton1.addActionListener(new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    jButton1ActionPerformed(evt);
                }
            });
            gridBagConstraints = new java.awt.GridBagConstraints();
            gridBagConstraints.gridx = 0;
            gridBagConstraints.gridy = 10;
            gridBagConstraints.gridwidth = 2;
            gridBagConstraints.insets = new java.awt.Insets(12, 0, 12, 0);
            getContentPane().add(jButton1, gridBagConstraints);

            pack();
        }// </editor-fold>//GEN-END:initComponents

        private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
            this.setVisible(false);
            CancellationHandler.cancel(null);
        }//GEN-LAST:event_jButton1ActionPerformed
        
        private javax.swing.JButton jButton1;
        private javax.swing.JLabel label;
        
        private static final long serialVersionUID = -6499791486275376059L;
    }

    public void findCycles() {
		final FindCyclesDialog findCyclesDialog = new FindCyclesDialog();
		findCyclesDialog.setLocation(getLocation().x + getSize().width / 2 - findCyclesDialog.getSize().width / 2,
				getLocation().y + getSize().height / 2 - findCyclesDialog.getSize().height / 2);
		CancellationHandler.reset(null);
		new Thread() {
			@Override
			public void run() {
				try {
					final Collection<CycleFinder.Path> cycles = CycleFinder.findCycle(extractionModelFrame.extractionModelEditor.dataModel, extractionModelFrame.extractionModelEditor.dataModel.getTables());
					SwingUtilities.invokeLater(new Runnable() {
						@Override
						public void run() {
							findCyclesDialog.setVisible(false);
							CyclesView.this.setVisible(true);
							refreshTableModel(cycles);
						}
					});
				} catch (CancellationException ce) {
					CancellationHandler.reset(null);
					SwingUtilities.invokeLater(new Runnable() {
						@Override
						public void run() {
							CyclesView.this.setVisible(false);
						}
					});
				} catch (final Throwable t) {
					SwingUtilities.invokeLater(new Runnable() {
						@Override
						public void run() {
							UIUtil.showException(CyclesView.this, "Error", t);
							CyclesView.this.setVisible(false);
						}
					});
				}
			}
		}.start();
		findCyclesDialog.setVisible(true);
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
     * Refreshes the dialog after the model has been changed.
     */
    public void refresh() {
    }
    
    /**
     * Refreshes the table model.
     */
    private synchronized void refreshTableModel(Collection<CycleFinder.Path> cycles) {
    	cellInfo.clear();
		
    	numLabel.setText(" " + cycles.size() + " Cycles");
    	
		Object[] columns = new Object[MAX_TABLES_PER_LINE + 1];
		for (int i = 0; i < columns.length; ++i) {
			columns[i] = "";
		}
		columns[0] = "Distance";
		columns[1] = "Table";
		
		List<Object[]> data = new ArrayList<Object[]>();
		
		int distance = 0;
		final Color BG1 = new Color(255, 255, 255);
		final Color BG2 = new Color(230, 255, 255);
		bgColor.clear();
		
		for (CycleFinder.Path cycle: cycles) {
			// add current line to table model
			List<Table> currentLineT = new ArrayList<Table>();
			cycle.fillPath(currentLineT);
			List<String> currentLine = new ArrayList<String>();
			for (Table t: currentLineT) {
				currentLine.add(extractionModelFrame.extractionModelEditor.dataModel.getDisplayName(t));
			}
			Object[] lineAsObjects = new Object[MAX_TABLES_PER_LINE + 1];
			Arrays.fill(lineAsObjects, "");
			int col = 0;
			lineAsObjects[col++] = "";
			CellInfo cellInfo = null;
			for (String t: currentLine) {
				cellInfo = new CellInfo();
				cellInfo.arrowLeft = false;
				cellInfo.arrowRight = true;
				this.cellInfo.add(cellInfo);
				if (col <= MAX_TABLES_PER_LINE) {
					cellInfo.column = col;
					lineAsObjects[col++] = t;					
				} else {
					data.add(lineAsObjects);
					bgColor.add(distance % 2 == 0? BG1 : BG2);
					lineAsObjects = new Object[MAX_TABLES_PER_LINE + 1];
					Arrays.fill(lineAsObjects, "");
					col = 1;
					cellInfo.arrowLeft = true;
					cellInfo.column = col;
					lineAsObjects[col++] = t;
				}
				cellInfo.row = data.size();
			}
			if (cellInfo != null) {
				cellInfo.arrowRight = false;
			}
			if (col > 1) {
				data.add(lineAsObjects);
				bgColor.add(distance % 2 == 0? BG1 : BG2);
			}
			lineAsObjects = new Object[MAX_TABLES_PER_LINE + 1];
			Arrays.fill(lineAsObjects, "");
			data.add(lineAsObjects);
			bgColor.add(distance % 2 == 0? BG1 : BG2);
			++distance;
		}
		
		Object[][] dataArray = (Object[][]) data.toArray(new Object[data.size()][]);
		DefaultTableModel tableModel = new DefaultTableModel(dataArray, columns) {
			public boolean isCellEditable(int row, int column) {
				return false;
			}
			private static final long serialVersionUID = -6639310191624899380L;
    	};
    	cyclesTable.setModel(tableModel);

		for (int i = 0; i < cyclesTable.getColumnCount(); i++) {
            TableColumn column = cyclesTable.getColumnModel().getColumn(i);
            int width = 1;
            
            Component comp = cyclesTable.getDefaultRenderer(String.class).
            						getTableCellRendererComponent(
            								cyclesTable, column.getHeaderValue(),
            								false, false, 0, i);
            width = Math.max(width, comp.getPreferredSize().width);

            for (int line = 0; line < dataArray.length; ++line) {
	            comp = cyclesTable.getDefaultRenderer(String.class).
	                             getTableCellRendererComponent(
	                            		 cyclesTable, dataArray[line][i],
	                                 false, false, line, i);
	            width = Math.max(width, comp.getPreferredSize().width);
            }
            
            if (i == 0) {
            	width = 50;
            	column.setMaxWidth(width);
            }
            
            column.setPreferredWidth(width);
        }
		cyclesTable.setIntercellSpacing(new Dimension(0, 0));
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
        numLabel = new javax.swing.JLabel();
        jScrollPane1 = new javax.swing.JScrollPane();
        cyclesTable = new javax.swing.JTable();
        
        getContentPane().setLayout(new java.awt.GridBagLayout());

        setTitle("Cycle View");
        jPanel1.setLayout(new java.awt.GridBagLayout());

        numLabel.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
        jPanel1.add(numLabel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 20;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 2);

        cyclesTable.setTableHeader(null);
        cyclesTable.setModel(new javax.swing.table.DefaultTableModel(
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
        cyclesTable.setShowGrid(false);
        cyclesTable.setSurrendersFocusOnKeystroke(true);
        jScrollPane1.setViewportView(cyclesTable);
        jScrollPane1.setColumnHeaderView(null);
        jScrollPane1.setColumnHeader(null);
        
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.gridwidth = 30;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 2, 0, 2);
        jPanel1.add(jScrollPane1, gridBagConstraints);

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
    private javax.swing.JTable cyclesTable;
    private javax.swing.JLabel numLabel;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JScrollPane jScrollPane1;
    // Ende der Variablendeklaration//GEN-END:variables
    
	private static final long serialVersionUID = 5485949274233292142L;

}
