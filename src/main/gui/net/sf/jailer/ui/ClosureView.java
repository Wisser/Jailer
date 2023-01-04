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
package net.sf.jailer.ui;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.Stack;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Vector;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.JTable;
import javax.swing.JToggleButton;
import javax.swing.RowSorter;
import javax.swing.RowSorter.SortKey;
import javax.swing.SortOrder;
import javax.swing.SwingUtilities;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableModel;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.StringSearchPanel.AdditionalComponentFactory;
import net.sf.jailer.ui.UIUtil.PLAF;
import net.sf.jailer.ui.pathfinder.HistoryPanel;
import net.sf.jailer.ui.pathfinder.PathFinder;
import net.sf.jailer.ui.pathfinder.PathFinder.Result;
import net.sf.jailer.ui.scrollmenu.JScrollMenu;
import net.sf.jailer.ui.scrollmenu.JScrollPopupMenu;
import net.sf.jailer.ui.util.UISettings;
import net.sf.jailer.util.Pair;
import net.sf.jailer.util.SqlUtil;

/**
 * Dialog for browsing through the closure of a table.
 *
 * @author Ralf Wisser
 */
public abstract class ClosureView extends javax.swing.JDialog {

	/**
	 * Number of tables in a closure-table's line.
	 */
	private int tablesPerLine = 8;

	/**
	 * The extraction model editor.
	 */
	private final ExtractionModelEditor extractionModelEditor;

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
		Set<CellInfo> parents = new HashSet<CellInfo>(4);
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
	private Set<Pair<String, String>> dependencies = new HashSet<Pair<String,String>>();

	private final JComboBox2 rootTable;

	/** Creates new form FindDialog
	 * @param rootTable */
	public ClosureView(ExtractionModelEditor extractionModelEditor, JComboBox2 rootTable) {
		super();
		this.extractionModelEditor = extractionModelEditor;
		this.rootTable = rootTable;
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
        gridBagConstraints.gridy = 2;
        gridBagConstraints.insets = new Insets(0, 2, 0, 2);
		final JComboBox2 comboBox1 = searchComboBox;
        JToggleButton searchButton = StringSearchPanel.createSearchButton(extractionModelEditor.extractionModelFrame, comboBox1, "Find Table", new Runnable() {
			@Override
			public void run() {
				findButtonActionPerformed(null);
			}
		}, null, null, null, false, null, false);
        closureTablePanel.add(searchButton, gridBagConstraints);

        searchComboBox.setVisible(false);
        findButton.setVisible(false);
        searchButton.setText("Find Table");

		AutoCompletion.enable(findPathComboBox);
		findPathComboBox.getEditor().getEditorComponent().addKeyListener(new KeyListener() {
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
		gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 2;
        Object currentSelection = rootTable.getSelectedItem();
        Table source = null;
		if (currentSelection instanceof String) {
			source = getDataModel().getTableByDisplayName((String) currentSelection);
		}
        final JComboBox2 comboBox = findPathComboBox;
        JToggleButton stFindButtonButton = StringSearchPanel.createSearchButton(extractionModelEditor.extractionModelFrame, comboBox,
    		new Object() {
				public String toString() {
					Table source = null;
					Object currentSelection = ClosureView.this.rootTable.getSelectedItem();
					if (currentSelection instanceof String) {
						source = getDataModel().getTableByDisplayName((String) currentSelection);
					}
					return (source != null? ("From " + getDataModel().getDisplayName(source) + " - ") : "") + "Select destination or choose from History";
				}
			},
			new Runnable() {
				@Override
				public void run() {
					findPathButtonActionPerformed(null);
				}
			}, null, null, null, true,
			new AdditionalComponentFactory() {
				@Override
				public JComponent create(final StringSearchPanel searchPanel) {
					HistoryPanel historyPanel = new HistoryPanel(ClosureView.this.getSelectedTable(), getDataModel()) {
						private static final long serialVersionUID = 1L;

						@Override
						protected void close() {
							searchPanel.close(true);
						}
						@Override
						protected void apply(Table source, Table destination) {
							findPath(source, destination, true);
						}
					};
			        return historyPanel;
				}
			}, false);
		closureTablePanel.add(stFindButtonButton, gridBagConstraints);

        findPathComboBox.setVisible(false);
        findPathButton.setVisible(false);
        stFindButtonButton.setText("Find Path to...");

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
		showOnlyEnabledCheckBox.setMnemonic(KeyEvent.VK_S);
//        disableAssocButton.setMnemonic(KeyEvent.VK_D);
//        disableAssocButton.setEnabled(false);

//        tableSelection.setMaximumRowCount(22);

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
						Color color = new Color(0, 0, 245, 60);
						g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

						if (selectionInfo.ignored) {
							BasicStroke stroke = new BasicStroke(3);
							g2d.setStroke(new BasicStroke(stroke.getLineWidth(), stroke.getEndCap(), stroke.getLineJoin(), stroke.getMiterLimit(), new float[] { 2f, 6f },
								1.0f));
							color = new Color(0, 0, 0, 60);
						} else {
							g2d.setStroke(new BasicStroke(3));
						}
						if (isDependency) {
							color = new Color(245, 0, 0, 60);
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

		MouseListener ml = new MouseListener() {
			@Override
			public void mouseClicked(final MouseEvent e) {
			}
			@Override
			public void mouseReleased(MouseEvent e) {
				// context menu
				if (!(e.getComponent() instanceof JTable)) {
					return;
				}
				JTable theTable = (JTable) e.getComponent();
				if (SwingUtilities.isRightMouseButton(e)) {
					int row = theTable.rowAtPoint(e.getPoint());
					int column = theTable.columnAtPoint(e.getPoint());
					if (row < 0 || column < 0) return;
					Object value;
					RowSorter<? extends TableModel> rowSorter = theTable.getRowSorter();
					if (theTable != neighborTable) {
						value = theTable.getModel().getValueAt(row, column);
					} else {
						value = theTable.getModel().getValueAt(rowSorter.convertRowIndexToModel(row), column);
					}
					if (value == null || !(value instanceof String)) return;
					Table table = getDataModel().getTableByDisplayName((String) value);
					if (table != null) {
						JPopupMenu popup = ClosureView.this.extractionModelEditor.graphView.createPopupMenu(table, createFindPathMenuItem(table), true);
						popup.show(e.getComponent(), e.getX(), e.getY());
					}
				}
				if (SwingUtilities.isLeftMouseButton(e)) {
					int row = theTable.rowAtPoint(e.getPoint());
					int column = theTable.columnAtPoint(e.getPoint());
					if (row < 0 || column < 0) return;

					final Rectangle cellRect = theTable.getCellRect(row, column, false);

					Object value;
					RowSorter<? extends TableModel> rowSorter = theTable.getRowSorter();
					if (theTable != neighborTable) {
						value = theTable.getModel().getValueAt(row, column);
					} else {
						value = theTable.getModel().getValueAt(rowSorter.convertRowIndexToModel(row), column);
					}
					if (value == null || !(value instanceof String)) return;
					final Table table = getDataModel().getTableByDisplayName((String) value);
					if (table != null) {
						JMenuItem restrictAll = new JMenuItem("Disable all associations");
						restrictAll.setToolTipText("disables every non-dependent (child) association with this table");
						restrictAll.addActionListener(new ActionListener () {
							@Override
							public void actionPerformed(ActionEvent e) {
								ClosureView.this.extractionModelEditor.ignoreAll(table);
							}
						});
						restrictAll.setEnabled(ClosureView.this.extractionModelEditor.isIgnoreAllApplicable(table));

						JMenuItem removeRestrictions = new JMenuItem("Remove all restrictions");
						removeRestrictions.addActionListener(new ActionListener () {
							@Override
							public void actionPerformed(ActionEvent e) {
								ClosureView.this.extractionModelEditor.removeAllRestrictions(table);
							}
						});
						removeRestrictions.setEnabled(ClosureView.this.extractionModelEditor.isRemovalOfAllRestrictionsApplicable(table));
						removeRestrictions.setToolTipText("removes all restrictions on all associations with this table");

						JMenu restrict = new JScrollMenu("Disable association...");
						restrict.setToolTipText("disables an association with this table");

						JMenu remove = new JScrollMenu("Remove restrictions on...");
						remove.setToolTipText("removes all restrictions on an association with this table");

						List<Association> aLDisable = new ArrayList<Association>();
						List<Association> aLEnable = new ArrayList<Association>();
						for (Association association: table.associations) {
							if (!association.isInsertDestinationBeforeSource()) {
								aLDisable.add(association);
							}
							aLEnable.add(association);
						}

						Map<String, Association> namedAssoc;
						namedAssoc = sortedNamed(aLDisable);
						boolean enab;
						enab = false;
						for (Map.Entry<String, Association> entry: namedAssoc.entrySet()) {
							JMenuItem menuItem = new JMenuItem(entry.getKey());
							final Association association = entry.getValue();
							if (association.isIgnored()) {
								menuItem.setEnabled(false);
							}
							enab = true;
							menuItem.addActionListener(new ActionListener() {
								@Override
								public void actionPerformed(ActionEvent arg0) {
									ClosureView.this.extractionModelEditor.ignorAssociation(association);
								}
							});
							restrict.getPopupMenu().add(menuItem);
						}
						restrict.setEnabled(enab);
						namedAssoc = sortedNamed(aLEnable);
						enab = false;
						for (Map.Entry<String, Association> entry: namedAssoc.entrySet()) {
							JMenuItem menuItem = new JMenuItem(entry.getKey());
							remove.getPopupMenu().add(menuItem);
							final Association association = entry.getValue();
							if (!association.isRestricted()) {
								menuItem.setEnabled(false);
							}
							enab = true;
							menuItem.addActionListener(new ActionListener() {
								@Override
								public void actionPerformed(ActionEvent arg0) {
									ClosureView.this.extractionModelEditor.removeRestriction(association);
								}
							});
						}
						remove.setEnabled(enab);

						final JPopupMenu popup = new JScrollPopupMenu();
						popup.add(restrictAll);
						popup.add(removeRestrictions);
						popup.add(new JSeparator());
						popup.add(restrict);
						popup.add(remove);

						if (cellInfo.containsKey(value)) {
							if (theTable == neighborTable) {
								CellInfo ci = cellInfo.get(value);
								selectTableCell(ci.column, ci.row);
							} else {
								selectTableCell(column, row);
							}
						}

						ClosureView.this.extractionModelEditor.graphView.createPopupMenu(table, null, false);
						popup.show(e.getComponent(), cellRect.x, cellRect.y + cellRect.height + 4);
					}
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
		};
		closureTable.addMouseListener(ml);
		neighborTable.addMouseListener(ml);

		searchComboBox.setMaximumRowCount(30);

		UIUtil.invokeLater(12, () -> jSplitPane1.setDividerLocation(0.75));
		
		final TableCellRenderer defaultTableCellRenderer = closureTable.getDefaultRenderer(String.class);
		closureTable.setDefaultRenderer(Object.class, new TableCellRenderer() {
			private Font font = new JLabel("normal").getFont();
			private Font normal = font.deriveFont(font.getStyle() & ~Font.BOLD, font.getSize());
			private Font bold = font.deriveFont(font.getStyle() | Font.BOLD, font.getSize());

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
				CellInfo cellInfo = ClosureView.this.cellInfo.get(value);
				if (render instanceof JLabel) {
					((JLabel) render).setForeground(Color.BLACK);
					((JLabel) render).setFont(normal);
					String text = ((JLabel) render).getText();
					if (!"".equals(text)) {
						((JLabel) render).setToolTipText(toolTip(text, cellInfo));
					} else {
						((JLabel) render).setToolTipText(null);
					}
					if (row == 0 && table != neighborTable) {
						((JLabel) render).setFont(bold);
					}
					if (cellInfo != null && selectedTable != null) {
						if (selectedTable.equals(value) || cellInfo.selected) {
							((JLabel) render).setFont(bold);
							((JLabel) render).setBackground(new Color(255, 230, 200));
						}
					}
					if (table == neighborTable) {
						((JLabel) render).setBackground(row % 2 == 0? BG1 : BG2);
					}
					Table t = value instanceof String? getDataModel().getTableByDisplayName((String) value) : null;
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
						if (currentForcedDistance != null && currentForcedDistance.containsKey(t)) {
							((JLabel) render).setFont(bold);
//							((JLabel) render).setForeground(new Color(150, 150, 150));
						}
					}
				}
				return render;
			}
		});
//		closureTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

		closureTable.setRowSelectionAllowed(false);

		neighborTable.setRowSelectionAllowed(false);
		neighborTable.setIntercellSpacing(new Dimension(0, 0));
		neighborTable.setShowGrid(false);
		neighborTable.setDefaultRenderer(Object.class, closureTable.getDefaultRenderer(Object.class));
		neighborTable.setDefaultRenderer(Integer.class, closureTable.getDefaultRenderer(Object.class));
		neighborTable.setAutoCreateRowSorter(true);
		neighborTable.getTableHeader().setReorderingAllowed(false);
		neighborTable.getTableHeader().setToolTipText("<html>The degree of a table is the total number of adjacent tables<br>that can be reached directly with an enabled association.</html>");
		if (UIUtil.plaf == PLAF.FLAT) {
			neighborTable.getTableHeader().putClientProperty( "FlatLaf.style", "separatorColor: #fff; bottomSeparatorColor: #ccc" );
			try {
				((DefaultTableCellRenderer) neighborTable.getTableHeader().getDefaultRenderer()).setHorizontalAlignment(JLabel.LEFT);
			} catch (Exception e) {
				// ignore
			}
		}
		try {
			List<SortKey> keys = new ArrayList<SortKey>();
			keys.add(new SortKey(1, SortOrder.DESCENDING));
			neighborTable.getRowSorter().setSortKeys(keys);
		} catch (Exception e) {
			e.printStackTrace();
			// ignore
		}

		rootTable.addItemListener(new java.awt.event.ItemListener() {
			@Override
			public void itemStateChanged(java.awt.event.ItemEvent evt) {
				refreshClear();
			}
		});
		if (jScrollPane2.getHorizontalScrollBar() != null) {
        	jScrollPane2.getHorizontalScrollBar().setUnitIncrement(16);
        }
        if (jScrollPane2.getVerticalScrollBar() != null) {
        	jScrollPane2.getVerticalScrollBar().setUnitIncrement(16);
        }
		initTabbedPane();
		setLocation(100, 100);
		UIUtil.setDialogSize(this, 500, 500);
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
        nb_same.add(tableName);
//        int maxWidth = 8;
        String sep = "";
//        if (nb_up.size() > maxWidth || nb_same.size() > maxWidth || nb_down.size() > maxWidth) {
//        	sep = "<tr><td></td></tr>";
//        }
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
			closureTable.getSelectionModel().clearSelection();
			for (CellInfo c: cellInfo.values()) {
				c.selected = false;
			}
			if (displayName != null && !"".equals(displayName)) {
				selectedTable = displayName;
				searchComboBox.setSelectedItem(selectedTable);
				repaintClosureView();
				Table table = getDataModel().getTableByDisplayName(selectedTable);
				if (table != null) {
					CellInfo selectionInfo = cellInfo.get(selectedTable);
					if (selectionInfo != null) {
						selectionInfo.select();
						Association association = null;
						for (CellInfo parent: selectionInfo.parents) {
							Table pre = parent.table;
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
						Stack<Table> toSelect = new Stack<Table>();
						CellInfo ci = selectionInfo;
						final int MAX_PATH_LENGTH = 1000;
						while (ci != null) {
							if (toSelect.size() > MAX_PATH_LENGTH) {
								break;
							}
							if (ci.table != null) {
								toSelect.push(ci.table);
							}
							CellInfo nextCi = null;
							for (CellInfo parent: ci.parents) {
								Table pre = parent.table;
								if (pre != null) {
									CellInfo preInfo = cellInfo.get(getDataModel().getDisplayName(pre));
									if (preInfo != null) {
										if (preInfo.selected) {
											if (preInfo.table != null) {
												nextCi = preInfo;
												break;
											}
										}
									}
								}
							}
							ci = nextCi;
						}
						Table selTable = getSelectedTable();
						if (currentForcedDistance != null) {
							if (selTable != null && !toSelect.contains(selTable)
								||
								!currentForcedDistance.containsKey(table)) {
								refresh();
								selectTableCell(col, row);
								return;
							}
						}
						Rectangle aRect = closureTable.getCellRect(row, col, true);
						aRect.setBounds(aRect.x, Math.max(0, aRect.y - aRect.height), aRect.width, aRect.height * 3);
						closureTable.scrollRectToVisible(aRect);
						extractionModelEditor.incCaptureLevel();
						try {
							if (!toSelect.isEmpty()) {
								Table source = toSelect.pop();
								while (!toSelect.isEmpty()) {
									Table dest = toSelect.pop();
									if (!ClosureView.this.extractionModelEditor.graphView.isTableVisible(dest)) {
										++UISettings.s9;
										ClosureView.this.extractionModelEditor.graphView.showTable(source, dest);
									}
									source = dest;
								}
							}
						} finally {
							extractionModelEditor.decCaptureLevel();
						}
						if (association != null && ClosureView.this.extractionModelEditor.select(association)) {
							return;
						}
					}
					if (!ClosureView.this.extractionModelEditor.select(table)) {
						ClosureView.this.extractionModelEditor.setRootSelection(table);
					}
				}
			}
		}
	}

	/**
	 * Gets current data model.
	 *
	 * @return current data model
	 */
	private DataModel getDataModel() {
		return extractionModelEditor.dataModel;
	}

	/**
	 * Refreshes the dialog.
	 */
	public void refreshClear() {
		selectedTable = null;
		refreshTableModel(null);
	}

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

	private Map<Table, Integer> currentForcedDistance = null;

	private final Color BG1 = UIUtil.TABLE_BACKGROUND_COLOR_1;
	private final Color BG2 = UIUtil.TABLE_BACKGROUND_COLOR_2;
	private final Color BG3 = new Color(255, 255, 240);
	private final Color BG4 = UIUtil.plaf == PLAF.FLAT? new Color(242, 242, 242) : new Color(220, 220, 220);
	private final Color BG5 = new Color(255, 240, 240);

	/**
	 * Refreshes the table model.
	 */
	private void refreshTableModel(Map<Table, Integer> forcedDistance) {
		cellInfo.clear();
		dependencies.clear();
		currentForcedDistance = forcedDistance;
		if (forcedDistance == null) {
			forcedDistance = new HashMap<Table, Integer>();
		}
		Table selectedTable = getSelectedTable();
		refreshAssociationView(selectedTable);
		refreshNeigborView(selectedTable);

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
            	width += 40;
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
		Object currentSelection = rootTable.getSelectedItem();
		if (currentSelection instanceof String) {
			selectedTable = getDataModel().getTableByDisplayName((String) currentSelection);
		}
		return selectedTable;
	}

	/**
	 * Names of associations which have been recently disabled and therefore are still be visible.
	 */
	private Set<String> editedAssociations = new TreeSet<String>();

	/**
	 * Names of associations in the closure ordered by distance.
	 */
	private List<List<String>> associationClosure = new ArrayList<List<String>>();

	/**
	 * All components of the association-closure render.
	 */
	private Collection<JComponent> associationClosureRender = new ArrayList<JComponent>();

	/**
	 * Name of focused table for which no association closure size limit exists.
	 */
	private String noAssocLimitTableName;
	private final int MAX_ASSOC_CLOSURE_SIZE_LIMIT = 200;

	/**
	 * Refreshes the associations view.
	 *
	 * @param selectedTable selected table (focus) or <code>null</code>
	 */
	private void refreshAssociationView(Table selectedTable) {
		jLabel2.setFont(nonbold);
		jLabel3.setFont(nonbold);
		jLabel4.setFont(nonbold);
		jLabel5.setFont(nonbold);
		jLabel6.setFont(nonbold);
		for (JComponent c: associationClosureRender) {
			assocViewPanel.remove(c);
		}
		associationClosureRender.clear();

		createAssociationClosure(selectedTable);

		int y = 5;
		int surplus = 0;

		GridBagConstraints gridBagConstraints;
		int distance = 0;
		boolean limitExceeded = false;
		final Table st = getSelectedTable();
		boolean unlimited = noAssocLimitTableName != null && st != null && noAssocLimitTableName.equals(st.getName());
		for (List<String> assocList: associationClosure) {
			boolean firstTime = true;
			Color bgColor;
			if (distance % 2 == 0) {
				bgColor = new java.awt.Color(240, 255, 255);
			} else {
				bgColor = Color.WHITE;
			}
			for (final String assocName: assocList) {
				Association association = getDataModel().namedAssociations.get(assocName);
				if (association == null || association.isInsertDestinationBeforeSource()) {
					continue;
				}
				if (y - 3 > MAX_ASSOC_CLOSURE_SIZE_LIMIT && !unlimited) {
					limitExceeded = true;
				}
				if (limitExceeded) {
					++surplus;
					continue;
				}
				Font font = nonbold;
				Color bgc = bgColor;
				if (extractionModelEditor.graphView.selectedAssociation != null &&
					assocName.equals(extractionModelEditor.graphView.selectedAssociation.getName())) {
					bgc = new Color(255, 230, 220);
				}
				JLabel l = createLabel(y, null, assocName, bgc, distance == 0 || !firstTime? " " : (" " + distance + " "), false);
				firstTime = false;
				l.setOpaque(true);
				l.setFont(font);
				gridBagConstraints = new java.awt.GridBagConstraints();
				gridBagConstraints.gridx = 0;
				gridBagConstraints.gridy = y;
				gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
				assocViewPanel.add(l, gridBagConstraints);

				l = createLabel(y, association.source.getName(), assocName, bgc, " " + getDataModel().getDisplayName(association.source), false);
				l.setOpaque(true);
				l.setFont(font);
				gridBagConstraints = new java.awt.GridBagConstraints();
				gridBagConstraints.gridx = 1;
				gridBagConstraints.gridy = y;
				gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
				assocViewPanel.add(l, gridBagConstraints);

				l = createLabel(y, association.destination.getName(), assocName, bgc, " " + getDataModel().getDisplayName(association.destination), false);
				l.setOpaque(true);
				l.setFont(font);
				gridBagConstraints = new java.awt.GridBagConstraints();
				gridBagConstraints.gridx = 2;
				gridBagConstraints.gridy = y;
				gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
				assocViewPanel.add(l, gridBagConstraints);

				final JCheckBox checkBox = new JCheckBox();
				checkBox.setSelected(!association.isIgnored());
				checkBox.addItemListener(new ItemListener() {
					@Override
					public void itemStateChanged(ItemEvent e) {
						editedAssociations.add(assocName);
						Association association = getDataModel().namedAssociations.get(assocName);
						if (association != null) {
							if (!ClosureView.this.extractionModelEditor.select(association)) {
								ClosureView.this.extractionModelEditor.setRootSelection(association.destination);
								ClosureView.this.extractionModelEditor.select(association);
							}
							extractionModelEditor.graphView.setRestriction(association, !checkBox.isSelected());
						}
					}
				});
				associationClosureRender.add(checkBox);
				checkBox.setBackground(bgc);
				checkBox.setOpaque(true);
				checkBox.setFont(nonbold);
				gridBagConstraints = new java.awt.GridBagConstraints();
				gridBagConstraints.gridx = 3;
				gridBagConstraints.gridy = y;
				gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
				assocViewPanel.add(checkBox, gridBagConstraints);

				String unrestrictedJoinCondition = association.getUnrestrictedJoinCondition();
				if (association.reversed) {
					unrestrictedJoinCondition = SqlUtil.reversRestrictionCondition(unrestrictedJoinCondition);
				}
				l = createLabel(y, null, assocName, bgc, " " + unrestrictedJoinCondition, true);
				l.setOpaque(true);
				l.setFont(nonbold);
				l.setForeground(Color.GRAY);
				gridBagConstraints = new java.awt.GridBagConstraints();
				gridBagConstraints.gridx = 4;
				gridBagConstraints.gridy = y;
				gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
				assocViewPanel.add(l, gridBagConstraints);

				++y;
			}
			if (firstTime && !limitExceeded) {
				JLabel l = createLabel(y, null, null, bgColor, distance == 0 || !firstTime? " " : (" " + distance + " "), false);
				firstTime = false;
				l.setOpaque(true);
				l.setFont(nonbold);
				gridBagConstraints = new java.awt.GridBagConstraints();
				gridBagConstraints.gridx = 0;
				gridBagConstraints.gridy = y;
				gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
				assocViewPanel.add(l, gridBagConstraints);

				l = createLabel(y, null, null, bgColor, " no non-dependency association", true);
				l.setOpaque(true);
				l.setFont(nonbold);
				l.setForeground(Color.GRAY);
				gridBagConstraints = new java.awt.GridBagConstraints();
				gridBagConstraints.gridx = 1;
				gridBagConstraints.gridy = y;
				gridBagConstraints.gridwidth = 4;
				gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
				assocViewPanel.add(l, gridBagConstraints);
				++y;
			}
			++distance;
		}
		if (limitExceeded) {
			JLabel l = createLabel(y, null, null, null, " " + surplus + " more...", false);
			l.setOpaque(true);
			l.setFont(nonbold);
			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.gridx = 0;
			gridBagConstraints.gridwidth = 4;
			gridBagConstraints.gridy = y;
			gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
			assocViewPanel.add(l, gridBagConstraints);

			JButton showAll = new JButton("Show all");
			associationClosureRender.add(showAll);
			showAll.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					noAssocLimitTableName = st != null? st.getName() : null;
					refreshAssociationView(getSelectedTable());
					repaintClosureView();
				}
			});
			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.gridx = 0;
			gridBagConstraints.gridy = y+1;
			gridBagConstraints.gridwidth = 4;
			gridBagConstraints.fill = java.awt.GridBagConstraints.NONE;
			gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
			assocViewPanel.add(showAll, gridBagConstraints);
			++y;
		}
		JLabel l = new JLabel("");
		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 1;
		gridBagConstraints.gridy = y;
		gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
		gridBagConstraints.weighty = 1.0;
		assocViewPanel.add(l, gridBagConstraints);
		associationClosureRender.add(l);
	}

	private JLabel createLabel(final int y, final String tableName, final String assocName, Color bgColor, String text, boolean unbounded) {
		final int MAX_TEXT_LENGTH = 22;
		final JLabel label = new JLabel();
		if (text.length() > MAX_TEXT_LENGTH && !unbounded) {
			label.setText(text.substring(0, MAX_TEXT_LENGTH) + "...");
			label.setToolTipText(text);
		} else {
			label.setText(text);
		}
		associationClosureRender.add(label);
		if (bgColor != null) {
			label.setBackground(bgColor);
		}
		if (assocName != null) {
			label.addMouseListener(new MouseListener() {
				@Override
				public void mouseReleased(MouseEvent e) {
				}
				@Override
				public void mousePressed(MouseEvent e) {
				}
				@Override
				public void mouseExited(MouseEvent e) {
				}
				@Override
				public void mouseEntered(MouseEvent e) {
				}
				@Override
				public void mouseClicked(MouseEvent e) {
					if (SwingUtilities.isRightMouseButton(e)) {
						if (tableName != null) {
							final Table table = getDataModel().getTable(tableName);
							if (table != null) {
								JPopupMenu popup = ClosureView.this.extractionModelEditor.graphView.createPopupMenu(table, createFindPathMenuItem(table), false);
								popup.show(e.getComponent(), e.getX(), e.getY());
							}
						}
					}
					else if (SwingUtilities.isLeftMouseButton(e)) {
						Association association = getDataModel().namedAssociations.get(assocName);
						if (association != null) {
							if (!ClosureView.this.extractionModelEditor.select(association)) {
								ClosureView.this.extractionModelEditor.setRootSelection(association.destination);
								ClosureView.this.extractionModelEditor.select(association);
							}
							refreshAssociationView(getSelectedTable());
							repaintClosureView();
						}
					}
				}
			});
		}
		return label;
	}

	/**
	 * Collects names of associations in the closure ordered by distance into {@link #associationClosure}.
	 *
	 * @param selectedTable focus
	 */
	private void createAssociationClosure(Table selectedTable) {
		associationClosure.clear();
		if (selectedTable != null) {
			Set<Association> seen = new HashSet<Association>();
			Set<Association> next = new HashSet<Association>();
			next.addAll(selectedTable.associations);
			for (Association a: selectedTable.associations) {
				next.add(a.reversalAssociation);
			}

			Set<Table> closure = selectedTable.closure();
			while (!next.isEmpty()) {
				Set<Association> neighbors  = new HashSet<Association>();
				List<String> assocList = new ArrayList<String>();
				seen.addAll(next);
				for (Association a: next) {
					if (closure.contains(a.source) /* && closure.contains(a.destination) */ || editedAssociations.contains(a.getName())) {
						if (!a.isIgnored() || !showOnlyEnabledCheckBox.isSelected()) {
							assocList.add(a.getName());
						}
						if (!a.isIgnored()) {
							for (Association n: a.destination.associations) {
								if (!seen.contains(n)) {
									neighbors.add(n);
								}
								if (!seen.contains(n.reversalAssociation)) {
									neighbors.add(n.reversalAssociation);
								}
							}
							for (Association n: a.source.associations) {
								if (!seen.contains(n)) {
									neighbors.add(n);
								}
								if (!seen.contains(n.reversalAssociation)) {
									neighbors.add(n.reversalAssociation);
								}
							}
						}
					}
				}
				Collections.sort(assocList, new Comparator<String>() {
					private Map<String, Association> assocs = getDataModel().namedAssociations;
					@Override
					public int compare(String o1, String o2) {
						Association a1 = assocs.get(o1);
						Association a2 = assocs.get(o2);
						String n11 = "", n21 = "";
						String n12 = "", n22 = "";
						if (a1 != null) {
							n11 = a1.source.getName();
							n12 = a1.destination.getName();
						}
						if (a2 != null) {
							n21 = a2.source.getName();
							n22 = a2.destination.getName();
						}
						int r = n11.compareTo(n21);
						if (r == 0) {
							r = n12.compareTo(n22);
						}
						return r;
					}
				});
				if (!assocList.isEmpty()) {
					associationClosure.add(assocList);
				}
				next = neighbors;
			}
		}
	}

	private void refreshNeigborView(Table selectedTable) {
		DefaultTableModel tableModel = new DefaultTableModel(new Object[] { "Table", "Degree" }, 0) {
			@Override
			public boolean isCellEditable(int row, int column) {
				return false;
			}
			@Override
            public Class<?> getColumnClass(int columnIndex) {
				if(columnIndex == 1){
                    return Integer.class;
                }
                return super.getColumnClass(columnIndex);
            }
			private static final long serialVersionUID = -6639310191624899380L;
		};

		while (tableModel.getRowCount() > 0) {
			tableModel.removeRow(0);
		}
		if (selectedTable != null) {
			Set<Table> closure = selectedTable.closure();
			for (Table table: closure) {
				Set<Table> neighbors = new HashSet<Table>();
				for (Association a: table.associations) {
					if (a.getJoinCondition() != null) {
						neighbors.add(a.destination);
					}
				}
				tableModel.addRow(new Object[] { getDataModel().getDisplayName(table), neighbors.size() });
			}
		}

		List<? extends SortKey> keys = null;
		try {
			keys = neighborTable.getRowSorter().getSortKeys();
		} catch (Exception e) {
			// ignore
		}
		neighborTable.setModel(tableModel);
		adjustTableColumnsWidth(neighborTable);
		if (keys != null) {
			try {
				neighborTable.getRowSorter().setSortKeys(keys);
			} catch (Exception e) {
				// ignore
			}
		}
	}

	/** This method is called from within the constructor to
	 * initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is
	 * always regenerated by the Form Editor.
	 */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        contentPanel = new javax.swing.JPanel();
        tabbedPane = new javax.swing.JTabbedPane();
        tablePane = new javax.swing.JPanel();
        tablePanel = new javax.swing.JPanel();
        jPanel1 = new javax.swing.JPanel();
        jSplitPane1 = new javax.swing.JSplitPane();
        closureTablePanel = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        closureTable = new javax.swing.JTable();
        searchComboBox = new JComboBox2();
        jLabel7 = new javax.swing.JLabel();
        columnsComboBox = new JComboBox2();
        findButton = new javax.swing.JButton();
        findPathComboBox = new JComboBox2();
        findPathButton = new javax.swing.JButton();
        jScrollPane3 = new javax.swing.JScrollPane();
        neighborTable = new javax.swing.JTable();
        associationPane = new javax.swing.JPanel();
        associationPanel = new javax.swing.JPanel();
        jScrollPane2 = new javax.swing.JScrollPane();
        assocViewPanel = new javax.swing.JPanel();
        jLabel2 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        jLabel6 = new javax.swing.JLabel();
        jSeparator1 = new javax.swing.JSeparator();
        jPanel3 = new javax.swing.JPanel();
        jPanel2 = new javax.swing.JPanel();
        showOnlyEnabledCheckBox = new javax.swing.JCheckBox();
        tableAssociationPane = new javax.swing.JPanel();
        tabAssPanel = new javax.swing.JPanel();
        tabAssTabPanel = new javax.swing.JPanel();
        tabAssAssPanel = new javax.swing.JPanel();

        contentPanel.setLayout(new java.awt.GridBagLayout());

        setTitle("Closure Browser");
        getContentPane().setLayout(new java.awt.GridBagLayout());

        tabbedPane.setMinimumSize(new java.awt.Dimension(182, 190));
        tabbedPane.addChangeListener(new javax.swing.event.ChangeListener() {
            public void stateChanged(javax.swing.event.ChangeEvent evt) {
                tabbedPaneStateChanged(evt);
            }
        });

        tablePane.setLayout(new javax.swing.BoxLayout(tablePane, javax.swing.BoxLayout.LINE_AXIS));

        tablePanel.setLayout(new java.awt.GridBagLayout());

        jPanel1.setLayout(new java.awt.GridBagLayout());

        jSplitPane1.setResizeWeight(0.75);
        jSplitPane1.setContinuousLayout(true);
        jSplitPane1.setOneTouchExpandable(true);

        closureTablePanel.setLayout(new java.awt.GridBagLayout());

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
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 2);
        closureTablePanel.add(jScrollPane1, gridBagConstraints);

        searchComboBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        searchComboBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                searchComboBoxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 2;
        closureTablePanel.add(searchComboBox, gridBagConstraints);

        jLabel7.setText("Columns ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 7;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        closureTablePanel.add(jLabel7, gridBagConstraints);

        columnsComboBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        columnsComboBox.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                columnsComboBoxItemStateChanged(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridy = 2;
        closureTablePanel.add(columnsComboBox, gridBagConstraints);

        findButton.setText("Search");
        findButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                findButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 5;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 0);
        closureTablePanel.add(findButton, gridBagConstraints);

        findPathComboBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        findPathComboBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                findPathComboBoxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 2;
        closureTablePanel.add(findPathComboBox, gridBagConstraints);

        findPathButton.setText("Search");
        findPathButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                findPathButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 4);
        closureTablePanel.add(findPathButton, gridBagConstraints);

        jSplitPane1.setLeftComponent(closureTablePanel);

        neighborTable.setModel(new javax.swing.table.DefaultTableModel(
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
        jScrollPane3.setViewportView(neighborTable);

        jSplitPane1.setRightComponent(jScrollPane3);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel1.add(jSplitPane1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 100;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        tablePanel.add(jPanel1, gridBagConstraints);

        tablePane.add(tablePanel);

        tabbedPane.addTab("Table", tablePane);

        associationPane.setLayout(new javax.swing.BoxLayout(associationPane, javax.swing.BoxLayout.LINE_AXIS));

        associationPanel.setLayout(new java.awt.GridBagLayout());

        assocViewPanel.setLayout(new java.awt.GridBagLayout());

        jLabel2.setBackground(java.awt.Color.white);
        jLabel2.setText(" Distance  ");
        jLabel2.setOpaque(true);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        assocViewPanel.add(jLabel2, gridBagConstraints);

        jLabel3.setBackground(java.awt.Color.white);
        jLabel3.setText(" to ");
        jLabel3.setOpaque(true);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        assocViewPanel.add(jLabel3, gridBagConstraints);

        jLabel4.setBackground(java.awt.Color.white);
        jLabel4.setText("         ");
        jLabel4.setOpaque(true);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        assocViewPanel.add(jLabel4, gridBagConstraints);

        jLabel5.setBackground(java.awt.Color.white);
        jLabel5.setText(" on");
        jLabel5.setOpaque(true);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        assocViewPanel.add(jLabel5, gridBagConstraints);

        jLabel6.setBackground(java.awt.Color.white);
        jLabel6.setText(" from     ");
        jLabel6.setOpaque(true);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        assocViewPanel.add(jLabel6, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.gridwidth = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        assocViewPanel.add(jSeparator1, gridBagConstraints);

        jPanel3.setBackground(java.awt.Color.white);
        jPanel3.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 1, 1));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        assocViewPanel.add(jPanel3, gridBagConstraints);

        jPanel2.setBackground(java.awt.Color.white);
        jPanel2.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 2, 2));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridwidth = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        assocViewPanel.add(jPanel2, gridBagConstraints);

        jScrollPane2.setViewportView(assocViewPanel);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        associationPanel.add(jScrollPane2, gridBagConstraints);

        showOnlyEnabledCheckBox.setText("Show only enabled associations");
        showOnlyEnabledCheckBox.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                showOnlyEnabledCheckBoxItemStateChanged(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        associationPanel.add(showOnlyEnabledCheckBox, gridBagConstraints);

        associationPane.add(associationPanel);

        tabbedPane.addTab("Association", associationPane);

        tableAssociationPane.setLayout(new javax.swing.BoxLayout(tableAssociationPane, javax.swing.BoxLayout.LINE_AXIS));

        tabAssPanel.setLayout(new java.awt.GridBagLayout());

        tabAssTabPanel.setLayout(new javax.swing.BoxLayout(tabAssTabPanel, javax.swing.BoxLayout.LINE_AXIS));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 4);
        tabAssPanel.add(tabAssTabPanel, gridBagConstraints);

        tabAssAssPanel.setLayout(new javax.swing.BoxLayout(tabAssAssPanel, javax.swing.BoxLayout.LINE_AXIS));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        tabAssPanel.add(tabAssAssPanel, gridBagConstraints);

        tableAssociationPane.add(tabAssPanel);

        tabbedPane.addTab("Table + Association", tableAssociationPane);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 16;
        gridBagConstraints.gridwidth = 20;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        getContentPane().add(tabbedPane, gridBagConstraints);

        pack();
    }// </editor-fold>//GEN-END:initComponents

	private void showOnlyEnabledCheckBoxItemStateChanged(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_showOnlyEnabledCheckBoxItemStateChanged
		clearView();
	}//GEN-LAST:event_showOnlyEnabledCheckBoxItemStateChanged

	private void tabbedPaneStateChanged(javax.swing.event.ChangeEvent evt) {//GEN-FIRST:event_tabbedPaneStateChanged
		initTabbedPane();
	}//GEN-LAST:event_tabbedPaneStateChanged

	private void searchComboBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_searchComboBoxActionPerformed
	}//GEN-LAST:event_searchComboBoxActionPerformed

	private void columnsComboBoxItemStateChanged(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_columnsComboBoxItemStateChanged
	}//GEN-LAST:event_columnsComboBoxItemStateChanged

    private void findButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_findButtonActionPerformed
		Object toFind = searchComboBox.getSelectedItem();
		if (toFind != null && !toFind.equals(selectedTable)) {
			CellInfo cellInfo = this.cellInfo.get(toFind);
			if (cellInfo != null) {
				selectTableCell(cellInfo.column, cellInfo.row);
				scrollTableCellToVisible(cellInfo.column, cellInfo.row);
			}
		}
    }//GEN-LAST:event_findButtonActionPerformed

	private void findPathComboBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_findPathComboBoxActionPerformed
    }//GEN-LAST:event_findPathComboBoxActionPerformed

    private void findPathButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_findPathButtonActionPerformed
    	Object toFind = findPathComboBox.getSelectedItem();
		if (toFind != null) {
			CellInfo cellInfo = this.cellInfo.get(toFind);
			if (cellInfo != null) {
				Object currentSelection = rootTable.getSelectedItem();
				if (currentSelection instanceof String) {
					final Table source = getDataModel().getTableByDisplayName((String) currentSelection);
					// if (source.closure(true).contains(cellInfo.table)) {
						findPath(source, cellInfo.table, false);
					// }
				}
			}
		}
	}//GEN-LAST:event_findPathButtonActionPerformed

    protected JMenuItem createFindPathMenuItem(final Table table) {
		JMenuItem findPath = new JMenuItem("Find Path to " + getDataModel().getDisplayName(table));
		Object currentSelection = rootTable.getSelectedItem();
		if (currentSelection instanceof String) {
			final Table source = getDataModel().getTableByDisplayName((String) currentSelection);
			if (getBiDirClosure(source).contains(table)) {
				findPath.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent arg0) {
						findPath(source, table, false);
					}
				});
				return findPath;
			 }
		}
		findPath.setEnabled(false);
		return findPath;
	}

	public static Set<Table> getBiDirClosure(final Table source) {
		return getBiDirClosure(source, new HashSet<Table>(), new HashSet<Table>());
	}

	private static Set<Table> getBiDirClosure(Table table, Set<Table> tables, Set<Table> tablesToIgnore) {
		Set<Table> closure = new HashSet<Table>();
		if (!tables.contains(table) && !tablesToIgnore.contains(table)) {
			closure.add(table);
			tables.add(table);
			for (Association association: table.associations) {
				if (!tables.contains(association.destination)) {
					if (association.getJoinCondition() != null || (association.reversalAssociation.getJoinCondition() != null)) {
						closure.addAll(getBiDirClosure(association.destination, tables, tablesToIgnore));
					}
				}
			}
		}
		return closure;
	}

	private void findPath(Table source, Table destination, boolean fromHistory) {
		Frame parent = null;
		Window w = SwingUtilities.getWindowAncestor(rootTable);
		if (w instanceof Frame) {
			parent = (Frame) w;
		}
		Result result = new PathFinder().find(source, destination, getDataModel(), false, fromHistory, parent);
		if (result != null) {
			Map<Table, Integer> fd = new HashMap<Table, Integer>();
			for (int i = 0; i < result.path.size(); ++i) {
				fd.put(result.path.get(i), i);
			}
			refreshTableModel(fd);
			List<Table> path = result.path;
			Table table = null;
			for (int i = 0; i < path.size(); i++) {
				table = path.get(i);
//				ClosureView.this.extractionModelEditor.select(table);
			}
			if (table != null) {
				CellInfo ci = cellInfo.get(getDataModel().getDisplayName(table));
				if (ci != null) {
					selectTableCell(ci.column, ci.row);
					scrollTableCellToVisible(ci.column, ci.row);
				}
			}
		}
	}

	private void initTabbedPane() {
		if (tabbedPane.getSelectedIndex() == 2) {
			tabAssTabPanel.removeAll();
			tabAssTabPanel.add(tablePanel);
			tabAssAssPanel.removeAll();
			tabAssAssPanel.add(associationPanel);
		} else {
			tablePane.removeAll();
			tablePane.add(tablePanel);
			associationPane.removeAll();
			associationPane.add(associationPanel);
		}
	}

	private void clearView() {
		editedAssociations.clear();
		refreshAssociationView(getSelectedTable());
		repaintClosureView();
	}

    private void scrollTableCellToVisible(int column, int row) {
    	Rectangle cellRect = closureTable.getCellRect(row, column, true);
		closureTable.scrollRectToVisible(new Rectangle(cellRect.x, Math.max(cellRect.y - cellRect.height, 0), cellRect.width, cellRect.height * 3));
    }

	protected abstract void repaintClosureView();

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JPanel assocViewPanel;
    private javax.swing.JPanel associationPane;
    private javax.swing.JPanel associationPanel;
    private javax.swing.JTable closureTable;
    private javax.swing.JPanel closureTablePanel;
    private JComboBox2 columnsComboBox;
    public javax.swing.JPanel contentPanel;
    private javax.swing.JButton findButton;
    private javax.swing.JButton findPathButton;
    private JComboBox2 findPathComboBox;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JScrollPane jScrollPane3;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JSplitPane jSplitPane1;
    private javax.swing.JTable neighborTable;
    private JComboBox2 searchComboBox;
    private javax.swing.JCheckBox showOnlyEnabledCheckBox;
    private javax.swing.JPanel tabAssAssPanel;
    private javax.swing.JPanel tabAssPanel;
    private javax.swing.JPanel tabAssTabPanel;
    private javax.swing.JTabbedPane tabbedPane;
    private javax.swing.JPanel tableAssociationPane;
    private javax.swing.JPanel tablePane;
    private javax.swing.JPanel tablePanel;
    // End of variables declaration//GEN-END:variables

	private static final long serialVersionUID = 5485949274233292142L;
	private Font normalfont = new JLabel("normal").getFont();
	private Font nonbold = normalfont.deriveFont(normalfont.getStyle() & ~Font.BOLD, normalfont.getSize());

    public void adjustTableColumnsWidth(JTable table) {
		DefaultTableModel dtm = (DefaultTableModel) table.getModel();
		DefaultTableCellRenderer defaultTableCellRenderer = new DefaultTableCellRenderer();
		for (int i = 0; i < table.getColumnCount(); i++) {
			TableColumn column = table.getColumnModel().getColumn(i);
			Component comp = defaultTableCellRenderer.getTableCellRendererComponent(table, column.getHeaderValue(), false, false, 0, i);
			int width = 1;
			width = Math.max(width, comp.getPreferredSize().width);

			int line = 0;
			for (; line < table.getRowCount(); ++line) {
				comp = table.getCellRenderer(line, i).getTableCellRendererComponent(table, dtm.getValueAt(line, i), false, false, line, i);
				width = Math.max(width, comp.getPreferredSize().width);
			}
			column.setPreferredWidth(Math.min(width, 400));
			if (i == 0) {
				column.setWidth(column.getPreferredWidth());
			}
		}
	}

    public void addTabComponent(String titel, Container tab) {
		 tabbedPane.addTab(titel, tab);
	}

	public void selectTabComponent(Container tab) {
		tabbedPane.setSelectedComponent(tab);
	}

	public void selectTabIndex(int index) {
		tabbedPane.setSelectedIndex(index);
	}

}
