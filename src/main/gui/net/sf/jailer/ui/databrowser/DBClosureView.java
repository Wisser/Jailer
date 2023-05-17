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
package net.sf.jailer.ui.databrowser;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Queue;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Vector;
import java.util.function.Supplier;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.JTable;
import javax.swing.JToggleButton;
import javax.swing.SwingUtilities;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.AutoCompletion;
import net.sf.jailer.ui.ClosureView;
import net.sf.jailer.ui.JComboBox2;
import net.sf.jailer.ui.StringSearchPanel;
import net.sf.jailer.ui.StringSearchPanel.AdditionalComponentFactory;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.UIUtil.PLAF;
import net.sf.jailer.ui.databrowser.Desktop.RowBrowser;
import net.sf.jailer.ui.databrowser.metadata.MDTable;
import net.sf.jailer.ui.databrowser.metadata.MetaDataSource;
import net.sf.jailer.ui.pathfinder.HistoryPanel;
import net.sf.jailer.ui.pathfinder.PathFinder;
import net.sf.jailer.ui.pathfinder.PathFinder.Result;
import net.sf.jailer.util.Pair;

/**
 * Dialog for browsing through the closure of a table.
 *
 * @author Ralf Wisser
 */
public abstract class DBClosureView extends javax.swing.JDialog {

    /**
     * Number of tables in a closure-table's line.
     */
    private static int tablesPerLine = 8;

    /**
     * Currently selected table (in closure-table).
     */
    private String selectedTable;

    /**
     * Background colors per row.
     */
    private final List<Color> bgColor = new ArrayList<Color>();

    private final class TableMouseListener implements MouseListener {
		private Map<Integer, String> manuallySelected = new TreeMap<Integer, String>();

		public void openPathFinder(final Table table, boolean fromHistory) {
			PathFinder pathFinder = new PathFinder();
			Result result = pathFinder.find(getRootTable(), table, getDataModel(), true, fromHistory, DBClosureView.this.parent);
			if (result != null) {
				List<Table> path = result.path;
				mainPath.clear();
			    mainPathAsSet.clear();
			    Map<Table, Integer> fd = new HashMap<Table, Integer>();
				for (int i = 0; i < result.path.size(); ++i) {
					fd.put(result.path.get(i), i);
				}
				refreshTableModel(fd);
			    selectedTable = null;
//			    refresh();

			    for (int i = 0; i < path.size(); ++i) {
			    	Table r = path.get(i);
			    	String tabName = getDataModel().getDisplayName(r);
			        selectCell(result.expand && i == path.size() - 1, tabName, r);
			        if (i == path.size() - 1) {
			        	scrollTableCellToVisible(tabName);
			        }
			    }
			}
		}

		private void expandPath() {
		    List<Table> path = new ArrayList<Table>();
		    for (CellInfo ci: mainPath) {
		        path.add(ci.table);
		    }
		    expandTablePath(path);
		}

		@Override
		public void mouseReleased(MouseEvent e) {
		}

		@Override
		public void mouseClicked(final MouseEvent e) {
			// reset view
			{
			    Point position = e.getPoint();
		        int row = closureTable.rowAtPoint(position);
		        int column = closureTable.columnAtPoint(position);
		        if (row >= 0 && column >= 0) {
			    	Object value = closureTable.getModel().getValueAt(row, column);
			        CellInfo ci = cellInfo.get(value);
					if (currentForcedDistance != null && ci != null && !mainPathAsSet.contains(ci)) {
				        mainPath.clear();
		                mainPathAsSet.clear();
		                currentForcedDistance = null;
		                refreshTableModel(null);
		                selectedTable = null;
		                refresh();
			        }
		        }
			}

			// context menu
		    if (SwingUtilities.isRightMouseButton(e)) {
		        int row = closureTable.rowAtPoint(e.getPoint());
		        int column = closureTable.columnAtPoint(e.getPoint());
		        if (row < 0 || column < 0) return;
		        final Object value = closureTable.getModel().getValueAt(row, column);
		        if (value == null || !(value instanceof String)) return;
		        final Table table = getDataModel().getTableByDisplayName((String) value);
		        if (table != null) {
		            if (selectedTable == null || !selectedTable.equals(value)) {
		                if (cellInfo.containsKey(value) && !cellInfo.get(value).selected) {
		                    selectTableCell(column, row);
		                }
		            }

		            JMenuItem pathFinder = new JMenuItem("Find more complex path to " + getDataModel().getDisplayName(table));
		            Table rt = getRootTable();
		            if (rt == null || !ClosureView.getBiDirClosure(rt).contains(table)) {
		            	pathFinder.setEnabled(false);
		            }
		            pathFinder.addActionListener(new ActionListener() {
		                @Override
		                public void actionPerformed(ActionEvent e) {
		                	openPathFinder(table, false);
		                }
		            });

		            JMenuItem openPath = new JMenuItem("Open path to " + getDataModel().getDisplayName(table));
                    if (rt == null || !ClosureView.getBiDirClosure(rt).contains(table)) {
		            	openPath.setEnabled(false);
		            }
                    openPath.addActionListener(new ActionListener() {
                        @Override
                        public void actionPerformed(ActionEvent e) {
                            expandPath();
                            DBClosureView.this.select(selectedTable);
                        }
                    });

		            RowBrowser rb = getVisibleTables().get(table);
		            if (rb == null) {
		                if (!mainPath.isEmpty()) {
		                    JPopupMenu menu = new JPopupMenu();
		                    menu.add(openPath);
		                    menu.add(new JSeparator());
		                    menu.add(pathFinder);
		                    UIUtil.showPopup(e.getComponent(), e.getX(), e.getY(), menu);
		                }
		            } else {
		                JPopupMenu menu = new JPopupMenu();
		                JMenuItem select = new JMenuItem("Select " + getDataModel().getDisplayName(table));
		                select.addActionListener(new ActionListener() {
		                    @Override
		                    public void actionPerformed(ActionEvent e) {
		                        DBClosureView.this.select((String) value);
		                    }
		                });
		                menu.add(select);
		                menu.add(new JSeparator());
		                JPopupMenu popup = rb.browserContentPane.createPopupMenu(null, -1, 0, 0, false, false, false);
		                JPopupMenu popup2 = rb.browserContentPane.createSqlPopupMenu(-1, 0, 0, true, closureTable);
		                popup.add(new JSeparator());
			            for (Component c : popup.getComponents()) {
		                    menu.add(c);
		                }
		                for (Component c : popup2.getComponents()) {
		                    menu.add(c);
		                }
		                if (!mainPath.isEmpty()) {
		                	try {
				                if (!((popup.getComponent(popup.getComponentCount() - 1)) instanceof JSeparator)) {
				                	 popup.add(new JSeparator());
				                }
			                } catch (Exception ex) {
			                	// ignore
			                }
		                	menu.add(openPath);
		                    menu.add(pathFinder);
		                }
		                UIUtil.showPopup(e.getComponent(), e.getX(), e.getY(), menu);
		            }
		        }
		    }

		    if (SwingUtilities.isLeftMouseButton(e)) {
		        Point position = e.getPoint();
		        int row = closureTable.rowAtPoint(position);
		        int column = closureTable.columnAtPoint(position);
		        if (row < 0 || column < 0) return;

		        Object value = closureTable.getModel().getValueAt(row, column);
		        if (value == null || !(value instanceof String)) return;
		        final Table table = getDataModel().getTableByDisplayName((String) value);
		        selectCell(e.getClickCount() > 1, value, table);
		    }
		}

		private void selectCell(boolean expandPath, Object value, final Table table) {
			if (table != null) {
			    if (cellInfo.containsKey(value)) {
			        String prevSelectedTable = selectedTable;
			        CellInfo selectedCellInfo = cellInfo.get(value);
			        if (selectedCellInfo.selected && !mainPathAsSet.contains(selectedCellInfo)) {
			            manuallySelected.put(selectedCellInfo.level, (String) value);
			            select(prevSelectedTable, selectedCellInfo);
			        } else if (!selectedCellInfo.selected) {
			            manuallySelected.clear();
			            selectTableCell((String) value);
			        } else {
			        	scrollToTable(table);
			        }
			    }

			    if (expandPath) {
			    	if (!mainPath.isEmpty()) {
			    		expandPath();
			        }
			    }
			}
		}

		private void select(String toSelect, CellInfo selectedCellInfo) {
		    TreeMap<Integer, String> newMS = new TreeMap<Integer, String>(manuallySelected);
		    String lastFound = null;
		    for (Entry<Integer, String> ms: manuallySelected.entrySet()) {
		        find(ms.getValue());
		        if (selectedCellInfo.level < ms.getKey() && !mainPathAsSet.contains(selectedCellInfo)) {
		            manuallySelected = newMS;
		            if (lastFound != null) {
		            find(lastFound);
		            }
		            break;
		        }
		        newMS.put(ms.getKey(), ms.getValue());
		        lastFound = ms.getValue();
		    }
		    if (toSelect != null && !toSelect.equals(selectedTable)) {
		        find(toSelect);
		    }
		}

		@Override
		public void mouseEntered(MouseEvent e) {
		}

		@Override
		public void mouseExited(MouseEvent e) {
		}

		@Override
		public void mousePressed(MouseEvent e) {
		}
	}
	/**
     * Holds infos about a cell in the closure-table.
     */
    private class CellInfo {
        public int row, column, level;
        boolean ignored = false;
        List<CellInfo> parents = new ArrayList<CellInfo>(4);
        boolean selected;
        CellInfo(int level) {
            this.level = level;
        }
        void select() {
            if (!selected) {
                selected = true;
                for (CellInfo parent: parents) {
                    parent.select();
                }
            }
        }
        Table table;
    }

    /**
     * Holds infos about a cell in the closure-table.
     */
    private Map<String, CellInfo> cellInfo = new HashMap<String, CellInfo>();
    private List<CellInfo> mainPath = new ArrayList<CellInfo>();
    private HashSet<CellInfo> mainPathAsSet = new HashSet<CellInfo>();
    private Set<Pair<String, String>> dependencies = new HashSet<Pair<String,String>>();
    private Map<Table, Integer> currentForcedDistance = null;
    private final JFrame parent;
    private final Supplier<MetaDataSource> metaDataSourceSupplier;

	private TableMouseListener tableMouseListener;

    /** Creates new form FindDialog
     */
    public DBClosureView(JFrame parent, Supplier<MetaDataSource> metaDataSourceSupplier) {
        super();
        this.parent = parent;
        this.metaDataSourceSupplier = metaDataSourceSupplier;
        initComponents();

        AutoCompletion.enable(searchComboBox);
        searchComboBox.getEditor().getEditorComponent().addKeyListener(new KeyListener() {
            @Override
            public void keyTyped(KeyEvent e) {
                if (e.getKeyChar() == '\n') {
                    findButtonActionPerformed(null);
                }
            }
            @Override
            public void keyReleased(KeyEvent e) {
            }
            @Override
            public void keyPressed(KeyEvent arg0) {
            }
        });
        GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.insets = new Insets(0, 0, 0, 2);
        final JComboBox2 comboBox = searchComboBox;
        JToggleButton searchButton = StringSearchPanel.createSearchButton(this.parent, comboBox, "Find Table", new Runnable() {
		    @Override
		    public void run() {
		        findButtonActionPerformed(null);
		    }
		}, null, null, null, false, null, false, false, null, false,
        		() -> createStringCount(metaDataSourceSupplier));
        createStringCount(metaDataSourceSupplier);
        tablePanel.add(searchButton, gridBagConstraints);

        searchComboBox.setVisible(false);
        findButton.setVisible(false);
        searchButton.setText("Find Table");

        AutoCompletion.enable(findPathComboBox);
        findPathComboBox.getEditor().getEditorComponent().addKeyListener(new KeyListener() {
            @Override
            public void keyTyped(KeyEvent e) {
                if (e.getKeyChar() == '\n') {
                	findPathComboBoxActionPerformed(null);
                }
            }
            @Override
            public void keyReleased(KeyEvent e) {
            }
            @Override
            public void keyPressed(KeyEvent arg0) {
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 20;
		final JComboBox comboBox1 = findPathComboBox;
		JToggleButton stFindPathButton = StringSearchPanel.createSearchButton(this.parent, comboBox1, new Object() {
					public String toString() {
						Table rootTable = getRootTable();
						return (rootTable != null? ("From " + getDataModel().getDisplayName(rootTable) + " - ") : "") + "Select destination or choose from History";
					}
				}, new Runnable() {
					@Override
		            public void run() {
		            	Object toFind = findPathComboBox.getSelectedItem();
						if (toFind != null) {
						    CellInfo cellInfo = DBClosureView.this.cellInfo.get(toFind);
						    if (cellInfo != null) {
						    	tableMouseListener.openPathFinder(cellInfo.table, false);
						    }
						}
		            }
				}, null, null, null, true, new AdditionalComponentFactory() {
			@Override
			public JComponent create(final StringSearchPanel searchPanel) {
				HistoryPanel historyPanel = new HistoryPanel(getRootTable(), getDataModel()) {
					private static final long serialVersionUID = 1L;
		
					@Override
					protected void close() {
						searchPanel.close(true);
					}
		
					@Override
					protected void apply(Table source, Table destination) {
						tableMouseListener.openPathFinder(destination, true);
					}
				};
		        return historyPanel;
			}
		}, false, false, null, false, () -> createStringCount(metaDataSourceSupplier));
		tablePanel.add(stFindPathButton, gridBagConstraints);

        findPathComboBox.setVisible(false);
        findPathButton.setVisible(false);
        stFindPathButton.setText("Find Path to...");

        columnsComboBox.setModel(new DefaultComboBoxModel<Integer>(new Integer[] {
                4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
        }));
        columnsComboBox.setSelectedItem(tablesPerLine);
        columnsComboBox.addItemListener(new java.awt.event.ItemListener() {
            @Override
			public void itemStateChanged(java.awt.event.ItemEvent evt) {
                if (evt.getItem() instanceof Integer) {
                    tablesPerLine = (Integer) evt.getItem();
                    refresh();
                }
            }
        });

        closureTable = new JTable() {
            private static final long serialVersionUID = 8960056200057023368L;

            /**
             * Paints selected path.
             */
            @Override
            public void paint(Graphics graphics) {
                super.paint(graphics);
                if (!(graphics instanceof Graphics2D)) return;
                Graphics2D g2d = (Graphics2D) graphics;
                CellInfo selectionInfo = cellInfo.get(selectedTable);
                if (selectionInfo == null) return;

                paint(g2d, selectionInfo, false, new HashSet<CellInfo>());
                paint(g2d, selectionInfo, true, new HashSet<CellInfo>());
            }

            private void paint(Graphics2D g2d, CellInfo selectionInfo, boolean drawDependencies, Set<CellInfo> painted) {
                if (painted.contains(selectionInfo)) {
                    return;
                }
                painted.add(selectionInfo);
                for (CellInfo parent: selectionInfo.parents) {
                    boolean isDependency = dependencies.contains(new Pair<String, String>(
                            getDataModel().getDisplayName(parent.table), getDataModel().getDisplayName(selectionInfo.table)));
                    if (isDependency == drawDependencies) {
                        int[] x = new int[2];
                        int[] y = new int[2];
                        Rectangle r = closureTable.getCellRect(parent.row, parent.column, false);
                        x[0] = ((int) r.getCenterX());
                        y[0] = ((int) r.getCenterY());
                        CellInfo posInfo = selectionInfo;
                        r = closureTable.getCellRect(posInfo.row, posInfo.column, false);
                        x[1] = ((int) r.getCenterX());
                        y[1] = ((int) r.getCenterY());
                        int alpha = 60;
                        int lineWidth = 1;
                        if (mainPathAsSet.contains(selectionInfo) && mainPathAsSet.contains(parent)) {
                            alpha = 100;
                            lineWidth = 3;
                        }
                        Color color = new Color(0, 0, 245, alpha);
                        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                        if (selectionInfo.ignored) {
                            BasicStroke stroke = new BasicStroke(lineWidth);
                            g2d.setStroke(new BasicStroke(stroke.getLineWidth(), stroke.getEndCap(), stroke.getLineJoin(), stroke.getMiterLimit(), new float[] { 2f, 6f },
                                1.0f));
                            color = new Color(0, 0, 0, alpha);
                        } else {
                            g2d.setStroke(new BasicStroke(lineWidth));
                        }
                        if (isDependency) {
                            color = new Color(245, 0, 0, alpha);
                        }
                        g2d.setColor(color);
                        g2d.drawPolyline(x, y, 2);
                    }
                    paint(g2d, parent, drawDependencies, painted);
                }
            }
        };
        closureTable.setShowGrid(false);
        closureTable.setSurrendersFocusOnKeystroke(true);
        closureTable.getTableHeader().setReorderingAllowed(false);
        if (UIUtil.plaf == PLAF.FLAT) {
			closureTable.getTableHeader().putClientProperty( "FlatLaf.style", "separatorColor: #fff; bottomSeparatorColor: #ccc" );
			try {
				((DefaultTableCellRenderer) closureTable.getTableHeader().getDefaultRenderer()).setHorizontalAlignment(JLabel.LEFT);
			} catch (Exception e) {
				// ignore
			}
		}
        jScrollPane1.setViewportView(closureTable);

        closureTable.addMouseListener(tableMouseListener = new TableMouseListener());

        searchComboBox.setMaximumRowCount(30);

        final TableCellRenderer defaultTableCellRenderer = closureTable.getDefaultRenderer(String.class);
        closureTable.setDefaultRenderer(Object.class, new TableCellRenderer() {
            private Font font = new JLabel("normal").getFont();
            private Font normal = font.deriveFont(font.getStyle() & ~Font.BOLD);
            private Font bold = font.deriveFont(font.getStyle() | Font.BOLD);
            private Font italic = font.deriveFont(font.getStyle() | Font.ITALIC);

            @Override
			public Component getTableCellRendererComponent(JTable table,
                    Object value, boolean isSelected, boolean hasFocus,
                    int row, int column) {
                isSelected = selectedTable != null && selectedTable.equals(value);
                hasFocus = false;
                Component render = defaultTableCellRenderer.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
                if (render instanceof JLabel && !isSelected) {
                    if (row < bgColor.size()) {
                        ((JLabel) render).setBackground(bgColor.get(row));
                    }
                }
                CellInfo cellInfo = DBClosureView.this.cellInfo.get(value);
                boolean onPath = false;
                if (render instanceof JLabel) {
                    ((JLabel) render).setForeground(Color.BLACK);
                    ((JLabel) render).setFont(normal);
                    String text = ((JLabel) render).getText();
                    if (!"".equals(text)) {
                        ((JLabel) render).setToolTipText(toolTip(text, cellInfo));
                    } else {
                        ((JLabel) render).setToolTipText(null);
                    }
                    if (row == 0) {
                        ((JLabel) render).setFont(bold);
                    }
                    if (cellInfo != null && selectedTable != null) {
                        if (selectedTable.equals(value) || cellInfo.selected) {
                            if (mainPathAsSet.contains(cellInfo)) {
                                onPath = true;
                                ((JLabel) render).setFont(bold);
                                ((JLabel) render).setBackground(new Color(255, 230, 200));
                            } else {
                                ((JLabel) render).setFont(italic);
                                ((JLabel) render).setBackground(new Color(200, 200, 200, 140));
                            }
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
                        if (getVisibleTables().containsKey(t)) {
                        	((JLabel) render).setForeground(new Color(0, 0, 225));
                        }
                    }
                }
                return render;
            }
        });
        closureTable.setRowSelectionAllowed(false);
        setLocation(100, 100);
        UIUtil.setDialogSize(this, 500, 500);
        setAlwaysOnTop(true);

        jLabel8.setVisible(false);
        jLabel9.setVisible(false);
    }

	/**
	 * @param metaDataSourceSupplier
	 * @return 
	 */
	private Map<String, Integer> createStringCount(Supplier<MetaDataSource> metaDataSourceSupplier) {
		Map<String, Integer> stringCount = new HashMap<String, Integer>();
		MetaDataSource metaDataSource = metaDataSourceSupplier.get();
		if (metaDataSource != null) {
	        for (int i = 0; i < searchComboBox.getModel().getSize(); ++i) {
	        	Object e = searchComboBox.getModel().getElementAt(i);
	        	if (e instanceof String) {
	        		Table table = getDataModel().getTableByDisplayName((String) e);
	        		if (table != null && metaDataSource.isInitialized()) {
	        			MDTable mdTable = metaDataSource.toMDTable(table);
	        			if (mdTable != null) {
	        				Long count = mdTable.getEstimatedRowCount();
	        				if (count != null) {
	        					stringCount.put((String) e, count.intValue() + (mdTable.isEstRCIsLowerBound()? Integer.MAX_VALUE / 2 : 0));
	        				}
	        			}
	        		}
	        	}
        	}
        }
		return stringCount;
	}

	protected String toolTip(String tableName, CellInfo theCellInfo) {
        if (theCellInfo == null || theCellInfo.table == null|| theCellInfo.level >= Integer.MAX_VALUE / 2) {
            return tableName;
        }
        Set<String> nb_up = new TreeSet<String>();
        Set<String> nb_same = new TreeSet<String>();
        Set<String> nb_down = new TreeSet<String>();
        for (Association a: theCellInfo.table.associations) {
            Table dest = a.destination;
            String destName = getDataModel().getDisplayName(dest);
            CellInfo destInfo = cellInfo.get(destName);
            if (destInfo != null) {
	            int dif = destInfo.level - theCellInfo.level;
	            if (dif == -1) {
	                nb_up.add(destName);
	            } else if (dif == 0) {
	                nb_same.add(destName);
	            } else if (dif == 1) {
	                nb_down.add(destName);
	            }
            }
        }
        nb_same.add(tableName);
//      int maxWidth = 8;
        String sep = "";
//      if (nb_up.size() > maxWidth || nb_same.size() > maxWidth || nb_down.size() > maxWidth) {
//      	sep = "<tr><td></td></tr>";
//      }
        int max = Math.max(nb_up.size(), Math.max(nb_same.size(), nb_down.size()));
        String tip = "<html>"
	      	+ "<table cellspacing=0 cellpadding=0>"
	      	+ tipJoin(nb_up, theCellInfo.level - 1, max, null)
	      	+ sep
	      	+ tipJoin(nb_same, theCellInfo.level, max, tableName)
	      	+ sep
	      	+ tipJoin(nb_down, theCellInfo.level + 1, max, null)
	      	+ "</table></html>";
        return tip;
	}

	private String tipJoin(Set<String> tipSet, int level, int max, String tableName) {
	  	if (tipSet.isEmpty()) {
	  		return "";
	  	}
	  	StringBuilder sb = new StringBuilder();
	  	int w = 0;
	  	int l = 0;
	  	int i = 0;
	  	int size = tipSet.size();
		List<String> tipList = new ArrayList<String>(tipSet);
	  	while (tipList.size() <= max) {
	  		tipList.add("");
	  	}
	  	if (tableName != null && tipList.remove(tableName)) {
	  		tipList.add(0, tableName);
	  	}
	  	for (String tip: tipList) {
	  		if (++w > 8) {
	  			w = 0;
	  			if (++l >= 1) { // lgtm [java/constant-comparison]
	  				if (size > i) {
	  					sb.append("<td>&nbsp;<i>" + (size - i) + " more...</i>&nbsp;</td>");
	  				}
	  	    		break;
	  			}
	  			sb.append("</tr><tr><td></td>");
	  		}
	  		if (tip.equals(tableName)) {
	  			sb.append("<td><b>&nbsp;" + tip + "&nbsp;</b></td>");
	  		} else {
	  			sb.append("<td>&nbsp;" + tip + "&nbsp;</td>");
	  		}
	  		if (tip.length() > 0) {
	  			++i;
	  		}
	  	}
	  	if (sb.length() == 0) {
	  		return "";
	  	}
	  	return "<tr><td>&nbsp;&nbsp;" + (level + 1) + "&nbsp;</td>" + sb.toString() + "</tr>";
	}

	protected SortedMap<String, Association> sortedNamed(List<Association> aList) {
        SortedMap<String, Association> result = new TreeMap<String, Association>();

        for (Association a: aList) {
            boolean isDup = false;
            for (Association a2: aList) {
                if (a2 != a && a2.destination.equals(a.destination)) {
                    isDup = true;
                    break;
                }
            }
            result.put(getDataModel().getDisplayName(a.destination) + (isDup? " (" + a.getName() + ")" : ""), a);
        }
        return result;
    }

    private void selectTableCell(int col, int row) {
        if (col >= 1 && row >= 0) {
            String displayName = (String) closureTable.getModel().getValueAt(row, col);
            selectTableCell(displayName);
        }
    }

    private void selectTableCell(String displayName) {
        closureTable.getSelectionModel().clearSelection();
        for (CellInfo c: cellInfo.values()) {
            c.selected = false;
        }
        if (displayName != null && !"".equals(displayName)) {
            Table prevTable = null;
            if (selectedTable != null) {
                prevTable = getDataModel().getTableByDisplayName(selectedTable);
            }
            selectedTable = displayName;
            searchComboBox.setSelectedItem(selectedTable);
            repaintClosureView();
            Table table = getDataModel().getTableByDisplayName(selectedTable);
            if (table != null) {
                CellInfo selectionInfo = cellInfo.get(selectedTable);
                selectionInfo.select();
                if (prevTable != table) {
                    Set<Table> visibleTables = getVisibleTables().keySet();
                    Set<Table> mainPathTables = new HashSet<Table>();
                    for (CellInfo ci: mainPath) {
                        mainPathTables.add(ci.table);
                    }
                    Set<CellInfo> pathToVisibleTable = shortestPathToVisibleTable(visibleTables, selectionInfo);
                    Set<CellInfo> pathToMainPathTable = shortestPathToVisibleTable(mainPathTables, selectionInfo);
                    List<CellInfo> pathVT = new ArrayList<CellInfo>();
                    int vtMaxRow = fillPath(selectionInfo, visibleTables, pathToVisibleTable, pathVT);
                    List<CellInfo> pathST = new ArrayList<CellInfo>();
                    int stMaxRow = fillPath(selectionInfo, mainPathTables, pathToMainPathTable, pathST);
                    if (pathToMainPathTable.isEmpty()) { // (vtMaxRow > stMaxRow) {
                        mainPath = pathVT;
                    } else {
                        mainPath = pathST;
                    }
                }
                mainPathAsSet = new HashSet<CellInfo>(mainPath);
            }
        }
    }

    private int fillPath(CellInfo selectionInfo, Set<Table> visibleTables, Set<CellInfo> pathToVisibleTable,
            List<CellInfo> path) {
        int maxRow = -1;
        for (CellInfo ci = selectionInfo; ci != null; ) {
            CellInfo nextParent = null;
            path.add(ci);
            for (CellInfo p: ci.parents) {
                if (nextParent == null) {
                    nextParent = p;
                }
                if (pathToVisibleTable.contains(p) || visibleTables.contains(p.table)) {
                    nextParent = p;
                    if (maxRow < 0 && visibleTables.contains(p.table)) {
                        maxRow = p.row;
                    }
                    break;
                }
            }
            ci = nextParent;
        }
        return maxRow;
    }

    private Set<CellInfo> shortestPathToVisibleTable(Set<Table> visibleTables, CellInfo root) {
        Map<CellInfo, CellInfo> pred = new HashMap<CellInfo, CellInfo>();
        Queue<CellInfo> next = new LinkedList<CellInfo>();
        Set<CellInfo> path = new HashSet<CellInfo>();
        next.add(root);
        while (!next.isEmpty()) {
            CellInfo info = next.poll();
            if (visibleTables.contains(info.table)) {
                for (CellInfo ci = info; ci != null; ci = pred.get(ci)) {
                    path.add(ci);
                }
                break;
            }
            for (CellInfo p: info.parents) {
                next.add(p);
                pred.put(p, info);
            }
        }

        return path;
    }

    /**
     * Gets current data model.
     *
     * @return current data model
     */
    protected abstract DataModel getDataModel();

    protected abstract Table getRootTable();

    /**
     * Refreshes the dialog after the model has been changed.
     */
    public void refresh() {
        String prevSelection = selectedTable;
        refreshTableModel(null);
        if (cellInfo.containsKey(prevSelection)) {
            selectedTable = prevSelection;
        } else {
            selectedTable = null;
        }
        repaintClosureView();
    }

    /**
     * Refreshes the table model.
     */
    private void refreshTableModel(Map<Table, Integer> forcedDistance) {
        cellInfo.clear();
        dependencies.clear();
        Table selectedTable = getSelectedTable();

        currentForcedDistance = forcedDistance;
		if (forcedDistance == null) {
			forcedDistance = new HashMap<Table, Integer>();
		}

		Object[] columns = new Object[tablesPerLine + 1];
        for (int i = 0; i < columns.length; ++i) {
            columns[i] = "";
        }
        columns[0] = "Distance";
        columns[1] = "Table";

        List<Object[]> data = new ArrayList<Object[]>();

        Set<String> visited = new TreeSet<String>();
        List<String> currentLine = new ArrayList<String>();
        if (selectedTable != null) {
            String displayName = getDataModel().getDisplayName(selectedTable);
            currentLine.add(displayName);
            visited.add(displayName);
            CellInfo cellInfo = new CellInfo(-1);
            cellInfo.column = 1;
            cellInfo.row = 0;
            cellInfo.table = selectedTable;
            this.cellInfo.put(displayName, cellInfo);
        }

        int distance = 0;
        final int OMEGA = Integer.MAX_VALUE / 2;
        boolean isolated = false;

        final Color BG1 = UIUtil.TABLE_BACKGROUND_COLOR_1;
        final Color BG2 = UIUtil.TABLE_BACKGROUND_COLOR_2;
        final Color BG3 = new Color(255, 255, 240);
        final Color BG4 = UIUtil.plaf == PLAF.FLAT? new Color(242, 242, 242) : new Color(220, 220, 220);
        final Color BG5 = new Color(255, 240, 240);
        bgColor.clear();

        TreeSet<String> nonIsolated = new TreeSet<String>(String::compareToIgnoreCase);

        while (!currentLine.isEmpty()) {
            // add current line to table model
            if (distance == OMEGA || isolated) {
                Object[] lineAsObjects = new Object[tablesPerLine + 1];
                Arrays.fill(lineAsObjects, "");
                data.add(lineAsObjects);
                bgColor.add(BG4);
            }

            Collections.sort(currentLine);
            Object[] lineAsObjects = new Object[tablesPerLine + 1];
            Arrays.fill(lineAsObjects, "");
            int col = 0;
            lineAsObjects[col++] = isolated? "isolated" : distance > OMEGA? "" : distance == OMEGA ? "infinite" : distance > 0? ("" + distance) : "";
            Color color = distance >= OMEGA? (distance % 2 == 0? BG5 : BG3) : distance % 2 == 0? BG1 : BG2;
            for (String t: currentLine) {
                CellInfo cellInfo = this.cellInfo.get(t);
                if (col <= tablesPerLine) {
                    if (cellInfo != null) {
                        cellInfo.column = col;
                    }
                    lineAsObjects[col++] = t;
                } else {
                    data.add(lineAsObjects);
                    bgColor.add(color);
                    lineAsObjects = new Object[tablesPerLine + 1];
                    Arrays.fill(lineAsObjects, "");
                    col = 1;
                    if (cellInfo != null) {
                        cellInfo.column = col;
                    }
                    lineAsObjects[col++] = t;
                }
                if (cellInfo != null) {
                    cellInfo.row = data.size();
                }
            }
            if (col > 1) {
                data.add(lineAsObjects);
                bgColor.add(color);
            }

            // get next line
            List<String> nextLine = new ArrayList<String>();
            for (String t: currentLine) {
                Table table = getDataModel().getTableByDisplayName(t);
                if (table != null) {
                    CellInfo cellInfoT = this.cellInfo.get(t);
                    for (Association association: table.associations) {
                    	Integer fd = forcedDistance.get(association.destination);
						if (fd != null && fd != distance + 1) {
							continue;
						}
						boolean addToParent = true;
                        if (forcedDistance.containsKey(association.destination)) {
                        	if (!forcedDistance.containsKey(table)) {
                        		addToParent = false;
                        	}
                        }
						String displayName = getDataModel().getDisplayName(association.destination);
                        if (!association.isIgnored() ||
                        		(forcedDistance.containsKey(association.source) && forcedDistance.containsKey(association.destination))) {
                            if (!visited.contains(displayName)) {
                                nextLine.add(displayName);
                                visited.add(displayName);
                                CellInfo cellInfo = new CellInfo(distance);
                                if (addToParent) {
    								cellInfo.parents.add(cellInfoT);
                                }
                                cellInfo.table = association.destination;
                                if (association.isInsertDestinationBeforeSource()) {
                                    dependencies.add(new Pair<String, String>(t, displayName));
                                }
                                this.cellInfo.put(displayName, cellInfo);
                            } else {
                                if (nextLine.contains(displayName)) {
                                	if (addToParent) {
                                		this.cellInfo.get(displayName).parents.add(cellInfoT);
                                	}
                                    if (association.isInsertDestinationBeforeSource()) {
                                        dependencies.add(new Pair<String, String>(t, displayName));
                                    }
                                }
                            }
                        }
                    }
                }
            }

            ++distance;

            if (nextLine.isEmpty()) {
                if (distance < OMEGA) {
                    distance = OMEGA;
                }
                Set<String> preVisited = new TreeSet<String>(visited);
                for (Table table: getDataModel().getTables()) {
                    String displayName = getDataModel().getDisplayName(table);
                    if (!visited.contains(displayName)) {
                        CellInfo cellInfoT = null;
                        String destName = null;
                        for (Association a: table.associations) {
                            destName = getDataModel().getDisplayName(a.destination);
                            if (preVisited.contains(destName)) {
                                cellInfoT = this.cellInfo.get(destName);
                                CellInfo cellInfo = this.cellInfo.get(displayName);
                                if (cellInfo == null) {
                                    cellInfo = new CellInfo(distance);
                                    cellInfo.table = table;
                                    nextLine.add(displayName);
                                    visited.add(displayName);
                                }
                                cellInfo.ignored = true;
                                cellInfo.parents.add(cellInfoT);
                                if (a.isInsertDestinationBeforeSource()) {
                                    dependencies.add(new Pair<String, String>(displayName, destName));
                                }
                                cellInfo.table = table;
                                this.cellInfo.put(displayName, cellInfo);
                            }
                        }
                    }
                }
                if (nextLine.isEmpty()) {
                    if (!isolated) {
                        isolated = true;
                        nonIsolated = new TreeSet<String>(String::compareToIgnoreCase);
                        nonIsolated.addAll(visited);
                    }
                    for (Table table: getDataModel().getTables()) {
                        String displayName = getDataModel().getDisplayName(table);
                        if (!visited.contains(displayName)) {
                            nextLine.add(displayName);
                            visited.add(displayName);
                        }
                    }
                }
            }

            currentLine = nextLine;
        }

        // sort parents by column
        for (CellInfo ci: cellInfo.values()) {
            Collections.sort(ci.parents, new Comparator<CellInfo>() {
                @Override
                public int compare(CellInfo o1, CellInfo o2) {
                    return o1.column - o2.column;
                }
            });
            CellInfo prev = null;
            for (Iterator<CellInfo> i = ci.parents.iterator(); i.hasNext(); ) {
                CellInfo cur = i.next();
                if (cur == prev) {
                    i.remove();
                }
                prev = cur;
            }
        }

        if (selectedTable != null) {
            CellInfo cInfo = cellInfo.get(this.selectedTable);
            if (cInfo != null) {
                cInfo.select();
            }
        }

        Object[][] dataArray = data.toArray(new Object[data.size()][]);
        DefaultTableModel tableModel = new DefaultTableModel(dataArray, columns) {
            @Override
			public boolean isCellEditable(int row, int column) {
                return false;
            }
            private static final long serialVersionUID = -6639310191624899380L;
        };
        closureTable.setModel(tableModel);

        int lastNonEmptyI = -1;
		Deque<TableColumn> toDelete = new ArrayDeque<TableColumn>();
		for (int i = 0; i < closureTable.getColumnCount(); i++) {
            TableColumn column = closureTable.getColumnModel().getColumn(i);
            int width = 1;
            boolean isEmpty = true;

            Component comp = closureTable.getDefaultRenderer(String.class).
                                    getTableCellRendererComponent(
                                            closureTable, column.getHeaderValue(),
                                            false, false, 0, i);
            if (column.getHeaderValue() != null && !"".equals(column.getHeaderValue())) {
				isEmpty = false;
			}
			width = Math.max(width, comp.getPreferredSize().width);

            for (int line = 0; line < dataArray.length; ++line) {
                comp = closureTable.getDefaultRenderer(String.class).
                                 getTableCellRendererComponent(
                                         closureTable, dataArray[line][i],
                                     false, false, line, i);
                width = Math.max(width, comp.getPreferredSize().width + 10);
                if (dataArray[line][i] != null && !"".equals(dataArray[line][i])) {
					isEmpty = false;
				}
            }
            if (i == 0) {
            	width += 20;
            }
			
            column.setPreferredWidth(width);
            if (isEmpty) {
				if (lastNonEmptyI >= 0) {
					closureTable.getColumnModel().getColumn(lastNonEmptyI).setPreferredWidth(width + closureTable.getColumnModel().getColumn(lastNonEmptyI).getPreferredWidth());
					toDelete.push(column);
				}
			} else {
				lastNonEmptyI = i;
			}
		}
		if (!toDelete.isEmpty()) {
			toDelete.pop();
		}
		while (!toDelete.isEmpty()) {
			closureTable.getColumnModel().removeColumn(toDelete.pop());
		}
		for (int i = 0; i < closureTable.getColumnCount() - 1; i++) {
            TableColumn column = closureTable.getColumnModel().getColumn(i);
            column.setMaxWidth(column.getPreferredWidth());
		}

		closureTable.setIntercellSpacing(new Dimension(0, 0));
//    	disableAssocButton.setEnabled(false);

        Vector<String> vector = new Vector<String>();
        vector.add("");
        vector.addAll(nonIsolated);
        searchComboBox.setModel(new DefaultComboBoxModel<String>(vector));
        searchComboBox.setSelectedItem("");
        findPathComboBox.setModel(new DefaultComboBoxModel<String>(vector));
        findPathComboBox.setSelectedItem("");
    }

    private Table getSelectedTable() {
        Table selectedTable = null;
        selectedTable = getRootTable();
        return selectedTable;
    }

    /**
     * Names of associations which have been recently disabled and therefore are still be visible.
     */
    private Set<String> editedAssociations = new TreeSet<String>();

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        contentPanel = new javax.swing.JPanel();
        tablePanel = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        closureTable = new javax.swing.JTable();
        searchComboBox = new JComboBox2();
        jLabel7 = new javax.swing.JLabel();
        columnsComboBox = new JComboBox2();
        findButton = new javax.swing.JButton();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        jLabel6 = new javax.swing.JLabel();
        jLabel8 = new javax.swing.JLabel();
        jLabel9 = new javax.swing.JLabel();
        findPathComboBox = new JComboBox2();
        findPathButton = new javax.swing.JButton();

        contentPanel.setLayout(new java.awt.GridBagLayout());

        setTitle("Closure Browser");
        getContentPane().setLayout(new java.awt.GridBagLayout());

        tablePanel.setLayout(new java.awt.GridBagLayout());

        jScrollPane1.setMinimumSize(new java.awt.Dimension(23, 64));

        closureTable.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
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
        closureTable.setMinimumSize(new java.awt.Dimension(60, 80));
        closureTable.setPreferredSize(new java.awt.Dimension(300, 80));
        closureTable.setSurrendersFocusOnKeystroke(true);
        jScrollPane1.setViewportView(closureTable);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 30;
        gridBagConstraints.gridheight = 12;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 0);
        tablePanel.add(jScrollPane1, gridBagConstraints);

        searchComboBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        searchComboBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                searchComboBoxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 20;
        tablePanel.add(searchComboBox, gridBagConstraints);

        jLabel7.setText("Columns ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 7;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        tablePanel.add(jLabel7, gridBagConstraints);

        columnsComboBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        columnsComboBox.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                columnsComboBoxItemStateChanged(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 8;
        gridBagConstraints.gridy = 20;
        tablePanel.add(columnsComboBox, gridBagConstraints);

        findButton.setText("Find");
        findButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                findButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 0);
        tablePanel.add(findButton, gridBagConstraints);

        jLabel1.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridy = 1;
        tablePanel.add(jLabel1, gridBagConstraints);

        jLabel2.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridy = 2;
        tablePanel.add(jLabel2, gridBagConstraints);

        jLabel3.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridy = 3;
        tablePanel.add(jLabel3, gridBagConstraints);

        jLabel4.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridy = 4;
        tablePanel.add(jLabel4, gridBagConstraints);

        jLabel5.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridy = 5;
        tablePanel.add(jLabel5, gridBagConstraints);

        jLabel6.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridy = 6;
        tablePanel.add(jLabel6, gridBagConstraints);

        jLabel8.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridy = 7;
        tablePanel.add(jLabel8, gridBagConstraints);

        jLabel9.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridy = 8;
        tablePanel.add(jLabel9, gridBagConstraints);

        findPathComboBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        findPathComboBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                findPathComboBoxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 20;
        tablePanel.add(findPathComboBox, gridBagConstraints);

        findPathButton.setText("Find Path to...");
        findPathButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                findPathButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 5;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 0);
        tablePanel.add(findPathButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        getContentPane().add(tablePanel, gridBagConstraints);

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void searchComboBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_searchComboBoxActionPerformed
    }//GEN-LAST:event_searchComboBoxActionPerformed

    private void columnsComboBoxItemStateChanged(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_columnsComboBoxItemStateChanged
    }//GEN-LAST:event_columnsComboBoxItemStateChanged

    private void findButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_findButtonActionPerformed
        Object toFind = searchComboBox.getSelectedItem();
        find((String) toFind);
        scrollTableCellToVisible((String) toFind);
    }//GEN-LAST:event_findButtonActionPerformed

    private void findPathComboBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_findPathComboBoxActionPerformed
    }//GEN-LAST:event_findPathComboBoxActionPerformed

    private void findPathButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_findPathButtonActionPerformed
    }//GEN-LAST:event_findPathButtonActionPerformed

    protected void find(String toFind) {
        if (toFind != null && !toFind.equals(selectedTable)) {
            CellInfo cellInfo = this.cellInfo.get(toFind);
            if (cellInfo != null) {
                selectTableCell(cellInfo.column, cellInfo.row);
            }
        }
    }

    private void scrollTableCellToVisible(String toFind) {
    	CellInfo cellInfo = this.cellInfo.get(toFind);
        if (cellInfo != null) {
	    	Rectangle cellRect = closureTable.getCellRect(cellInfo.row, cellInfo.column, true);
			closureTable.scrollRectToVisible(new Rectangle(cellRect.x, Math.max(cellRect.y - cellRect.height, 0), cellRect.width, cellRect.height * 3));
        }
    }

    protected abstract void repaintClosureView();
    protected abstract Map<Table, RowBrowser> getVisibleTables();
    protected abstract void expandTablePath(List<Table> path);
    protected abstract void select(String selectedTable);
	protected abstract void scrollToTable(Table table);

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JTable closureTable;
    private JComboBox2 columnsComboBox;
    public javax.swing.JPanel contentPanel;
    private javax.swing.JButton findButton;
    private javax.swing.JButton findPathButton;
    private JComboBox2 findPathComboBox;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JScrollPane jScrollPane1;
    private JComboBox2 searchComboBox;
    public javax.swing.JPanel tablePanel;
    // End of variables declaration//GEN-END:variables

    private static final long serialVersionUID = 5485949274233292142L;

}
