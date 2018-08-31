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
package net.sf.jailer.ui.databrowser;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
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

import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.JTable;
import javax.swing.SwingUtilities;
import javax.swing.border.BevelBorder;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.AutoCompletion;
import net.sf.jailer.ui.StringSearchPanel;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.Desktop.RowBrowser;
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
    };

    /**
     * Holds infos about a cell in the closure-table.
     */
    private Map<String, CellInfo> cellInfo = new HashMap<String, CellInfo>();
    private List<CellInfo> mainPath = new ArrayList<CellInfo>();
    private HashSet<CellInfo> mainPathAsSet = new HashSet<CellInfo>();
    private Set<Pair<String, String>> dependencies = new HashSet<Pair<String,String>>();
    private Set<Table> excludedFromPath = new HashSet<Table>();
    private final JFrame parent;
    
    /** Creates new form FindDialog 
     * @param rootTable */
    public DBClosureView(JFrame parent) {
        super();
        this.parent = parent;
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
        JButton searchButton = StringSearchPanel.createSearchButton(this.parent, searchComboBox, "Find Table", new Runnable() {
            @Override
            public void run() {
                findButtonActionPerformed(null);
            }
        });
        tablePanel.add(searchButton, gridBagConstraints);
        
        searchComboBox.setVisible(false);
        findButton.setVisible(false);
        searchButton.setText("Find Table");
        
        columnsComboBox.setModel(new DefaultComboBoxModel<Integer>(new Integer[] { 
                4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
        }));
        columnsComboBox.setSelectedItem(new Integer(tablesPerLine));
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
        jScrollPane1.setViewportView(closureTable);

        closureTable.addMouseListener(new MouseListener() {
            
        	private Map<Integer, String> manuallySelected = new TreeMap<Integer, String>();

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
                        int tableLevel = -1;
                        if (cellInfo.containsKey(value)) {
                        	tableLevel = cellInfo.get(value).level;
                        }
                        JCheckBoxMenuItem exclude = new JCheckBoxMenuItem("Exclude " + getDataModel().getDisplayName(table) + " from Path");
                        exclude.setSelected(excludedFromPath.contains(table));
                        exclude.setEnabled(mainPath.contains(cellInfo.get(value)) || excludedFromPath.contains(table));
                        exclude.addActionListener(new ActionListener() {
                            @Override
                            public void actionPerformed(ActionEvent e) {
                                if (!excludedFromPath.contains(table)) {
                                    excludedFromPath.add(table);
                                } else {
                                    excludedFromPath.remove(table);
                                }
                                mainPath.clear();
                                mainPathAsSet.clear();
                                refresh();
                                CellInfo ci = cellInfo.get(selectedTable);
                                if (ci != null) {
                                    String st = selectedTable;
                                    selectedTable = null;
                                    select(st, ci);
                                }
                            }
                        });
                        
                        final Set<Table> toExclude = new HashSet<Table>();
                        for (Entry<String, CellInfo> ciE: cellInfo.entrySet()) {
                            if (ciE.getValue().selected && ciE.getValue().level == tableLevel) {
                            	Table tableByDisplayName = getDataModel().getTableByDisplayName(ciE.getKey());
								if (tableByDisplayName != null) {
									toExclude.add(tableByDisplayName);
								}
                            }
                        }
                        JCheckBoxMenuItem excludeAll = new JCheckBoxMenuItem("Exclude all with Distance " + (tableLevel  + 1) + " from Path");
                        excludeAll.setEnabled(toExclude.size() > 1);
                        excludeAll.addActionListener(new ActionListener() {
                            @Override
                            public void actionPerformed(ActionEvent e) {
                            	excludedFromPath.addAll(toExclude);
                                mainPath.clear();
                                mainPathAsSet.clear();
                                refresh();
                                CellInfo ci = cellInfo.get(selectedTable);
                                if (ci != null) {
                                    String st = selectedTable;
                                    selectedTable = null;
                                    select(st, ci);
                                }
                            }
                        });
                        
                        JMenuItem deselect = new JMenuItem("Deselect Path");
                        deselect.addActionListener(new ActionListener() {
                            @Override
                            public void actionPerformed(ActionEvent e) {
                            	mainPath.clear();
                                mainPathAsSet.clear();
                                excludedFromPath.clear();
                                refreshTableModel();
                                selectedTable = null;
                                refresh();
                            }
                        });
                        RowBrowser rb = getVisibleTables().get(table);
                        if (rb == null) {
                            if (!mainPath.isEmpty()) {
                                JPopupMenu menu = new JPopupMenu();
                                JMenuItem open = new JMenuItem("Open Path to " + selectedTable);
                                open.addActionListener(new ActionListener() {
                                    @Override
                                    public void actionPerformed(ActionEvent e) {
                                        expandPath();
                                    }
                                });
                                menu.add(open);
                                JMenuItem openAndSelect = new JMenuItem("Open Path to and Select " + selectedTable);
                                openAndSelect.addActionListener(new ActionListener() {
                                    @Override
                                    public void actionPerformed(ActionEvent e) {
                                        expandPath();
                                        DBClosureView.this.select(selectedTable);
                                    }
                                });
                                menu.add(openAndSelect);
                                menu.add(new JSeparator());
                                menu.add(exclude);
                                menu.add(excludeAll);
                                menu.add(new JSeparator());
                                menu.add(deselect);
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
                            menu.addSeparator();
                            menu.add(exclude);
                            menu.add(excludeAll);
                            menu.addSeparator();
                            JPopupMenu popup = rb.browserContentPane.createPopupMenu(null, -1, 0, 0, false);
                            JPopupMenu popup2 = rb.browserContentPane.createSqlPopupMenu(null, -1, 0, 0, true, closureTable);
                            popup.add(new JSeparator());
                            for (Component c : popup.getComponents()) {
                                menu.add(c);
                            }
                            for (Component c : popup2.getComponents()) {
                                menu.add(c);
                            }
                            if (!mainPath.isEmpty()) {
                            	menu.addSeparator();
                            	menu.add(deselect);
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
                    if (table != null) {
                        if (cellInfo.containsKey(value)) {
                            String prevSelectedTable = selectedTable;
                            CellInfo selectedCellInfo = cellInfo.get(value);
                            if (selectedCellInfo.selected && !mainPathAsSet.contains(selectedCellInfo)) {
                                manuallySelected.put(selectedCellInfo.level, (String) value);
                                select(prevSelectedTable, selectedCellInfo);
                            } else if (!selectedCellInfo.selected) {
                                manuallySelected.clear();
                                selectTableCell(column, row);
                            } else {
                            	scrollToTable(table);
                            }
                        }
                        
                        if (e.getClickCount() > 1) {
                            RowBrowser rb = getVisibleTables().get(table);
                            if (rb == null) {
                                if (!mainPath.isEmpty()) {
                                    expandPath();
                                }
                            }
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
        });
        
        searchComboBox.setMaximumRowCount(30);
        
        final TableCellRenderer defaultTableCellRenderer = closureTable.getDefaultRenderer(String.class);
        closureTable.setDefaultRenderer(Object.class, new TableCellRenderer() {
            private Font font = new JLabel("normal").getFont();
            private Font normal = new Font(font.getName(), font.getStyle() & ~Font.BOLD, font.getSize());
            private Font bold = new Font(font.getName(), font.getStyle() | Font.BOLD, font.getSize());
            private Font italic = new Font(font.getName(), font.getStyle() | Font.ITALIC, font.getSize());
            private Font italicBold = new Font(font.getName(), font.getStyle() | Font.ITALIC | Font.BOLD, font.getSize());
            
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
                        } else if (getVisibleTables().containsKey(t)) {
                            ((JLabel) render).setFont(onPath? italicBold : italic);
                        }
                        if (excludedFromPath.contains(t)) {
                            ((JLabel) render).setForeground(new Color(180, 180, 180));
                        }
                        if (getVisibleTables().containsKey(t)) {
                            ((JLabel) render).setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED, Color.lightGray, Color.gray));
                        }
                    }
                }
                return render;
            }
        });
        closureTable.setRowSelectionAllowed(false);
        setLocation(100, 100);
        setSize(500, 500);
        setAlwaysOnTop(true);
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
        nb_same.add("<b>" + tableName + "</b>");
        int maxWidth = 6;
        String sep = "";
        if (nb_up.size() > maxWidth || nb_same.size() > maxWidth || nb_down.size() > maxWidth) {
        	sep = "<tr><td></td></tr>";
        }
        String tip = "<html>"
        	+ "<table cellspacing=0 cellpadding=0>"
        	+ tipJoin(nb_up, theCellInfo.level - 1)
        	+ sep
        	+ tipJoin(nb_same, theCellInfo.level)
        	+ sep
        	+ tipJoin(nb_down, theCellInfo.level + 1)
        	+ "</table>";
        return tip;
    }

    private String tipJoin(Set<String> tipList, int level) {
    	StringBuilder sb = new StringBuilder();
    	int w = 0;
    	for (String tip: tipList) {
    		if (++w > 6) {
    			w = 0;
    			sb.append("</tr><tr><td></td>");
    		}
    		sb.append("<td>&nbsp;" + tip + "&nbsp;</td>");
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
        refreshTableModel();
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
    private void refreshTableModel() {
        cellInfo.clear();
        dependencies.clear();
        Table selectedTable = getSelectedTable();
        
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
        
        final Color BG1 = new Color(255, 255, 255);
        final Color BG2 = new Color(242, 255, 242);
        final Color BG3 = new Color(255, 255, 240);
        final Color BG4 = new Color(220, 220, 220);
        final Color BG5 = new Color(255, 240, 240);
        bgColor.clear();
        
        TreeSet<String> nonIsolated = new TreeSet<String>();
        
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
                    if (!excludedFromPath.contains(table)) {
                        for (Association association: table.associations) {
                            String displayName = getDataModel().getDisplayName(association.destination);
                            if (!association.isIgnored()) {
                                if (!visited.contains(displayName)) {
                                    nextLine.add(displayName);
                                    visited.add(displayName);
                                    CellInfo cellInfo = new CellInfo(distance);
                                    cellInfo.parents.add(cellInfoT);
                                    cellInfo.table = association.destination;
                                    if (association.isInsertDestinationBeforeSource()) {
                                        dependencies.add(new Pair<String, String>(t, displayName));
                                    }
                                    this.cellInfo.put(displayName, cellInfo);
                                } else {
                                    if (nextLine.contains(displayName)) {
                                        this.cellInfo.get(displayName).parents.add(cellInfoT);
                                        if (association.isInsertDestinationBeforeSource()) {
                                            dependencies.add(new Pair<String, String>(t, displayName));
                                        }
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
                        nonIsolated = new TreeSet<String>(visited);
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
        closureTable.setIntercellSpacing(new Dimension(0, 0));
//    	disableAssocButton.setEnabled(false);
        
        Vector<String> vector = new Vector<String>();
        vector.add("");
        vector.addAll(nonIsolated);
        searchComboBox.setModel(new DefaultComboBoxModel<String>(vector));
        searchComboBox.setSelectedItem("");
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
        searchComboBox = new net.sf.jailer.ui.JComboBox();
        jLabel7 = new javax.swing.JLabel();
        columnsComboBox = new net.sf.jailer.ui.JComboBox();
        findButton = new javax.swing.JButton();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        jLabel6 = new javax.swing.JLabel();
        jLabel8 = new javax.swing.JLabel();
        jLabel9 = new javax.swing.JLabel();

        contentPanel.setLayout(new java.awt.GridBagLayout());

        setTitle("Closure Browser");
        getContentPane().setLayout(new java.awt.GridBagLayout());

        tablePanel.setBorder(javax.swing.BorderFactory.createTitledBorder("Closure"));
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
        gridBagConstraints.insets = new java.awt.Insets(0, 2, 0, 2);
        tablePanel.add(jScrollPane1, gridBagConstraints);

        searchComboBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        searchComboBox.addActionListener(new java.awt.event.ActionListener() {
            @Override
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
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        tablePanel.add(jLabel7, gridBagConstraints);

        columnsComboBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        columnsComboBox.addItemListener(new java.awt.event.ItemListener() {
            @Override
			public void itemStateChanged(java.awt.event.ItemEvent evt) {
                columnsComboBoxItemStateChanged(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 20;
        tablePanel.add(columnsComboBox, gridBagConstraints);

        findButton.setText("Find");
        findButton.addActionListener(new java.awt.event.ActionListener() {
            @Override
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
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        tablePanel.add(jLabel1, gridBagConstraints);

        jLabel2.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        tablePanel.add(jLabel2, gridBagConstraints);

        jLabel3.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        tablePanel.add(jLabel3, gridBagConstraints);

        jLabel4.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        tablePanel.add(jLabel4, gridBagConstraints);

        jLabel5.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 5;
        tablePanel.add(jLabel5, gridBagConstraints);

        jLabel6.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 6;
        tablePanel.add(jLabel6, gridBagConstraints);

        jLabel8.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 7;
        tablePanel.add(jLabel8, gridBagConstraints);

        jLabel9.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 8;
        tablePanel.add(jLabel9, gridBagConstraints);

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
    }//GEN-LAST:event_findButtonActionPerformed

    protected void find(String toFind) {
        if (toFind != null && !toFind.equals(selectedTable)) {
            CellInfo cellInfo = this.cellInfo.get(toFind);
            if (cellInfo != null) {
                selectTableCell(cellInfo.column, cellInfo.row);
            }
        }
    }

    protected abstract void repaintClosureView();
    protected abstract Map<Table, RowBrowser> getVisibleTables();
    protected abstract void expandTablePath(List<Table> path);
    protected abstract void select(String selectedTable);
	protected abstract void scrollToTable(Table table);

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JTable closureTable;
    private net.sf.jailer.ui.JComboBox columnsComboBox;
    public javax.swing.JPanel contentPanel;
    private javax.swing.JButton findButton;
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
    private net.sf.jailer.ui.JComboBox searchComboBox;
    public javax.swing.JPanel tablePanel;
    // End of variables declaration//GEN-END:variables
    
    private static final long serialVersionUID = 5485949274233292142L;

}
