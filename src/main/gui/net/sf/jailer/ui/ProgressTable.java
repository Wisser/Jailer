/*
 * Copyright 2007 - 2024 Ralf Wisser.
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
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.geom.Path2D;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.border.Border;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

/**
 * Table showing collection progress.
 * 
 * @author Ralf Wisser
 */
public class ProgressTable extends JTable {

	/**
	 * Maximum number of tables in a closure-table's line.
	 */
	private final static int MAX_TABLES_PER_LINE = 7;

	/**
	 * Background colors.
	 */
	private final List<Color> bgColors = new ArrayList<Color>();
	private final Color BG1 = UIUtil.TABLE_BACKGROUND_COLOR_1;
	private final Color BG2 = UIUtil.TABLE_BACKGROUND_COLOR_2;
	private final Color SELECTED_FG = new Color(180, 160, 0);
	private final Color INPROGRESS_FG = new Color(255, 40, 0);

	/**
	 * Holds infos about a cell.
	 */
	public static class CellInfo {
		// In
		public String tableName;
		public long numberOfRows;
		public Set<String> parentNames;
		public boolean excludeFromDeletion;
		// Calculated
		public int row, column;
		public List<CellInfo> parents;
		public boolean inProgress = false;
		public boolean hasSelectedChild = false;
	}

	/**
	 * Holds infos about a cells.
	 */
	private List<List<CellInfo>> cellInfos = new ArrayList<List<CellInfo>>();

	/**
	 * Selected cells.
	 */
	private Set<CellInfo> selectedCells = new HashSet<CellInfo>();
	private String selectedTableName = null;
	
	private void setSelectedTableName(String tableName) {
		selectedTableName = tableName;
		for (List<CellInfo> cL : cellInfos) {
			for (CellInfo cellInfo : cL) {
				cellInfo.hasSelectedChild = false;
			}
		}
		if (tableName != null) {
			for (List<CellInfo> cL : cellInfos) {
				for (CellInfo cellInfo : cL) {
					if (tableName.contentEquals(cellInfo.tableName)) {
						if (cellInfo.parents != null) {
							for (CellInfo p: cellInfo.parents) {
								p.hasSelectedChild = true;
							}
						}
					}
				}
			}
		}
	}
	
	/**
	 * Total number of collected rows.
	 */
	private long totalNumberOfCollectedRows = 0;

	/**
	 * Cell render components.
	 */
	private final JPanel cellPanel = new JPanel();
	private final JPanel layerdPane = new JPanel();
	private final JLabel tableRender = new JLabel("");
	private final JLabel numberRender = new JLabel("");
	private final JLabel iconRender = new JLabel(" ");
	private final Icon scaledSourceIcon;
	private final Icon scaledExcludeFromDeletionImage;
	
	/** Creates new table */
	public ProgressTable() {
		setShowGrid(false);
		setSurrendersFocusOnKeystroke(true);
		
		scaledSourceIcon = UIUtil.scaleIcon(iconRender, sourceIcon, 1.4);
		scaledExcludeFromDeletionImage = UIUtil.scaleIcon(iconRender, excludeFromDeletionImage, 1.4);
		tableRender.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
		numberRender.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
		cellPanel.setLayout(new GridBagLayout());
		layerdPane.setLayout(new GridBagLayout());
		GridBagConstraints gridBagConstraints = new GridBagConstraints();
		gridBagConstraints.gridx = 1;
		gridBagConstraints.gridy = 1;
		gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
		gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTH;
		gridBagConstraints.weightx = 0;
		gridBagConstraints.weighty = 0;
		cellPanel.add(tableRender, gridBagConstraints);
		gridBagConstraints = new GridBagConstraints();
		gridBagConstraints.gridx = 1;
		gridBagConstraints.gridy = 2;
		gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
		gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
		gridBagConstraints.weightx = 0;
		gridBagConstraints.weighty = 0;
		cellPanel.add(numberRender, gridBagConstraints);
		
		gridBagConstraints = new GridBagConstraints();
		gridBagConstraints.gridx = 0;
		gridBagConstraints.gridy = 1;
		gridBagConstraints.gridheight = 2;
		gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
		gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
		gridBagConstraints.weightx = 0;
		gridBagConstraints.weighty = 0;
		cellPanel.add(iconRender, gridBagConstraints);
		
		gridBagConstraints = new GridBagConstraints();
		gridBagConstraints.gridx = 2;
		gridBagConstraints.gridy = 1;
		gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
		gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
		gridBagConstraints.weightx = 0;
		gridBagConstraints.weighty = 0;
		layerdPane.add(cellPanel, gridBagConstraints);

//		layerdPane.setLayer(cellPanel, javax.swing.JLayeredPane.FRAME_CONTENT_LAYER);
//		layerdPane.setLayer(iconRender, javax.swing.JLayeredPane.PALETTE_LAYER);

		tableRender.setOpaque(true);
		numberRender.setOpaque(true);
		iconRender.setOpaque(true);
		cellPanel.setOpaque(true);
		setRowHeight(getRowHeight() * 3);

		final Border defaultBorder = cellPanel.getBorder();
		final Border selBorder = BorderFactory.createLineBorder(Color.BLACK, 1);

		setDefaultRenderer(Object.class, new TableCellRenderer() {
			private Font font = new JLabel("normal").getFont();
			private Font normal = font.deriveFont(font.getStyle() & ~Font.BOLD, font.getSize());
			private Font kursiv = font.deriveFont((font.getStyle() & ~Font.BOLD) | Font.ITALIC, font.getSize());
			private Font bold = font.deriveFont(font.getStyle() | Font.BOLD, font.getSize());

			@Override
			public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
				Color color = bgColors.get(row);
				boolean fontIsSet = false;
				tableRender.setForeground(Color.BLACK);
				numberRender.setForeground(Color.BLACK);
				JPanel outer = layerdPane;
				outer.setToolTipText(null);
				outer.setBorder(defaultBorder);
				if (value instanceof CellInfo) {
					CellInfo cellInfo = (CellInfo) value;
					
					iconRender.setIcon(null);
					if (showExcludeFromDeletionImage && cellInfo.excludeFromDeletion) {
						iconRender.setIcon(scaledExcludeFromDeletionImage);
					}
					if (cellInfo.tableName.equals(selectedTableName)) {
						outer.setBorder(selBorder);
					} else if (cellInfo.hasSelectedChild) {
						iconRender.setIcon(scaledSourceIcon);
					}
					
					tableRender.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
					tableRender.setText(cellInfo.tableName);
					outer.setToolTipText(cellInfo.tableName);
					if (selectedCells.contains(cellInfo)) {
						numberRender.setFont(bold);
						tableRender.setFont(bold);
						fontIsSet = true;
					}
					if (cellInfo.numberOfRows < 0) {
						numberRender.setText(" ");
						tableRender.setForeground(cellInfo.inProgress? INPROGRESS_FG : SELECTED_FG);
						numberRender.setForeground(cellInfo.inProgress? INPROGRESS_FG : SELECTED_FG);
					} else if (cellInfo.numberOfRows == 0) {
						numberRender.setText(cellInfo.numberOfRows + " rows");
						tableRender.setForeground(Color.GRAY);
						numberRender.setForeground(Color.GRAY);
					} else {
						long p = 0;
						if (totalNumberOfCollectedRows > 0) {
							p = (cellInfo.numberOfRows * 100) / totalNumberOfCollectedRows;
							if (p > 100) { // should not happen
								p = 100;
							}
						}
						numberRender.setText(UIUtil.format(cellInfo.numberOfRows) + " rows" + (p == 0 ? "" : " (" + p + "%)"));
						tableRender.setForeground(Color.BLACK);
						float f = 0.2f + (p / 100.0f) * 0.8f;
						if (f > 1.0f) {
							f = 1.0f;
						}
						if (p == 0) {
							f = 0.0f;
						}
						numberRender.setForeground(new Color(0, 0, 0));
					}
				} else {
					tableRender.setHorizontalAlignment(javax.swing.SwingConstants.LEFT);
					tableRender.setText(" " + value);
					numberRender.setText("");
					iconRender.setIcon(null);
				}
				if (!fontIsSet) {
					numberRender.setFont(kursiv);
					tableRender.setFont(normal);
				}
				tableRender.setBackground(color);
				numberRender.setBackground(color);
				outer.setBackground(color);
				iconRender.setBackground(color);
				return outer;
			}
		});

		setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			@Override
			public void valueChanged(ListSelectionEvent evt) {
				int col = getSelectedColumn();
				int row = getSelectedRow();
				if (col >= 1 && row >= 0) {
					Object o = getModel().getValueAt(row, col);
					if (o instanceof CellInfo) {
						CellInfo cellInfo = (CellInfo) o;
						selectedCells.clear();
						setSelectedTableName(cellInfo.tableName);
						multiSelection = false;
						selectCell(cellInfo, 0);
						repaint();
						getSelectionModel().clearSelection();
					}
				}
			}
		});

		Object[] colNames = new String[MAX_TABLES_PER_LINE + 1];
		for (int i = 0; i < colNames.length; ++i) {
			colNames[i] = "";
		}
		tableModel = new DefaultTableModel(colNames, 0) {
			@Override
			public boolean isCellEditable(int row, int column) {
				return false;
			}
			private static final long serialVersionUID = 772166119760096519L;
		};
		setModel(tableModel);
		setIntercellSpacing(new Dimension(0, 0));
		getColumnModel().getColumns().nextElement().setMaxWidth(50);
		setTableHeader(null);
	}

	private boolean multiSelection = false;

	private final int MAX_DEPTH = 1000;

	/**
	 * Recursively selects cells.
	 * 
	 * @param cellInfo current cell
	 */
	private void selectCell(CellInfo cellInfo, int depth) {
		if (!selectedCells.contains(cellInfo) && depth < MAX_DEPTH) {
			selectedCells.add(cellInfo);
			if (cellInfo.parents != null) {
				for (CellInfo p : cellInfo.parents) {
					selectCell(p, depth + 1);
				}
			}
		}
	}

	/**
	 * Selects all cells which render a given table.
	 * 
	 * @param tableName name of the table
	 */
	public void selectAllCells(String tableName) {
		multiSelection = true;
		selectedCells.clear();
		setSelectedTableName(tableName);
		for (List<CellInfo> cL : cellInfos) {
			for (CellInfo cellInfo : cL) {
				if (cellInfo != null) {
					if (cellInfo.tableName.equals(tableName)) {
						if (cellInfo.numberOfRows > 0) {
							selectCell(cellInfo, 0);
						}
					}
				}
			}
		}
		repaint();
	}

	private DefaultTableModel tableModel;

	/**
	 * Paints the links to parent tables.
	 */
	@Override
	public void paint(Graphics graphics) {
		super.paint(graphics);
		if (!(graphics instanceof Graphics2D))
			return;
		Graphics2D g2d = (Graphics2D) graphics;
		Color color = new Color(0, 80, 255, 80);
		Color selColor = new Color(255, 0, 0, 120);
		g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
		int[] x = new int[2];
		int[] y = new int[2];
		List<int[]> hLineCount = new ArrayList<int[]>();
		for (List<CellInfo> cL : cellInfos) {
			for (CellInfo cellInfo : cL) {
				g2d.setColor(selectedCells.contains(cellInfo) ? selColor : color);
				g2d.setStroke(new BasicStroke(selectedCells.contains(cellInfo) ? 3 : 2, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER));
				while (cellInfo != null && hLineCount.size() <= cellInfo.row) {
					hLineCount.add(new int[MAX_TABLES_PER_LINE]);
				}
				if (cellInfo != null && cellInfo.parents != null) {
					for (CellInfo parent : cellInfo.parents) {
						int offset = 0;
						if (cellInfo.column == parent.column) {
							offset = 0;
							for (int r = parent.row; r < cellInfo.row; ++r) {
								if (offset < hLineCount.get(r)[cellInfo.column]) {
									offset = hLineCount.get(r)[cellInfo.column];
								}
								++hLineCount.get(r)[cellInfo.column];
							}
							offset = ((offset + 1) / 2) * (2 * (offset % 2) - 1) * 6;
						}
						Rectangle r = getCellRect(cellInfo.row, cellInfo.column + 1, false);
						x[0] = (int) r.getCenterX();
						y[0] = (int) r.getMinY() + r.height / 5;
						r = getCellRect(parent.row, parent.column + 1, false);
						x[1] = (int) r.getCenterX();
						y[1] = (int) r.getMaxY() - r.height / 4;
						drawArrow(g2d, x[1] + offset, y[1], x[0] + offset, y[0]);
					}
				}
			}
		}
	}

	/**
	 * Returns visibility of last row of the table.
	 * 
	 * @return <code>true</code> iff last row of the table is visible
	 */
	public boolean isLastRowVisible() {
		if (getRowCount() == 0) {
			return false;
		}
		Rectangle r = getCellRect(getRowCount() - 1, 1, true);
		Rectangle visible = getVisibleRect();
		
		boolean isVisible = r.y <= visible.y + visible.height;
		return isVisible;
	}

	/**
	 * Removes last row and adds a new one.
	 * 
	 * @param cells the row to add
	 * @param day the day
	 */
	public void replaceLastRow(List<CellInfo> cells, int day) {
		boolean scrollToBottom = isLastRowVisible();
		boolean checkSelection = false;
		if (selectedTableName != null) {
			for (CellInfo cellInfo : cellInfos.get(cellInfos.size() - 1)) {
				if (cellInfo != null && selectedTableName.equals(cellInfo.tableName) && selectedCells.contains(cellInfo)) {
					selectCell(cellInfo, 0);
					checkSelection = true;
					break;
				}
			}
		}
		if (cellInfos.size() > 0) {
			Set<Integer> rowsToRemove = new TreeSet<Integer>();
			for (CellInfo ci : cellInfos.remove(cellInfos.size() - 1)) {
				rowsToRemove.add(ci.row);
			}
			for (int rowI = rowsToRemove.size(); rowI > 0; --rowI) {
				tableModel.removeRow(tableModel.getRowCount() - 1);
				bgColors.remove(bgColors.size() - 1);
			}
		}
		addRow(cells, day, scrollToBottom);

		if (selectedTableName != null && (checkSelection || multiSelection)) {
			if (multiSelection) {
				selectAllCells(selectedTableName);
			} else {
				boolean f = false;
				for (CellInfo cellInfo : cellInfos.get(cellInfos.size() - 1)) {
					if (cellInfo != null && selectedTableName.equals(cellInfo.tableName)) {
						selectCell(cellInfo, 0);
						f = true;
						break;
					}
				}
				if (!f) {
					selectedCells.clear();
					setSelectedTableName(null);
				}
			}
		}
	}

	/**
	 * Adds a row of {@link CellInfo}s to the table.
	 * 
	 * @param cells the row to add
	 * @param day the day
	 */
	public void addRow(List<CellInfo> cells, int day) {
		addRow(cells, day, isLastRowVisible());
	}

	/**
	 * Caches optimization result.
	 */
	private List<CellInfo> lastBestRow = null;
	
	/**
	 * Adds a row of {@link CellInfo}s to the table.
	 * 
	 * @param cells the row to add
	 * @param day the day
	 * @param scrollToBottom if <code>true</code>, scroll table to bottom row
	 */
	public void addRow(List<CellInfo> cells, int day, boolean scrollToBottom) {
		List<CellInfo> row = new LinkedList<CellInfo>(cells);
		int leftPad = (MAX_TABLES_PER_LINE - cells.size()) / 2;
		while (leftPad-- > 0) {
			row.add(0, null);
		}
		
		int s = (row.size() * 3) / 2;
		while (row.size() < s) {
			row.add(null);
		}
		while (row.size() % MAX_TABLES_PER_LINE != 0) {
			row.add(null);
		}

		String dayS = "" + day;
		List<CellInfo> parentRow = null;
		for (int i = cellInfos.size() - 1; i > 0; --i) {
			if (cellInfos.get(i).isEmpty()) {
				dayS = i + ", " + dayS;
			} else {
				parentRow = cellInfos.get(i);
				break;
			}
		}

		for (CellInfo cellInfo : row) {
			if (cellInfo != null && cellInfo.parentNames != null && parentRow != null) {
				cellInfo.parents = new ArrayList<CellInfo>();
				for (String pName : cellInfo.parentNames) {
					for (CellInfo pInfo : parentRow) {
						if (pName.equals(pInfo.tableName)) {
							cellInfo.parents.add(pInfo);
							break;
						}
					}
				}
			}
		}

		boolean takeLastOrdering = false;
		if (lastBestRow != null) {
			takeLastOrdering = true;
			Set<String> lastNames = new HashSet<String>();
			for (CellInfo ci: lastBestRow) {
				if (ci != null) {
					lastNames.add(ci.tableName);
				}
			}
			Set<String> currentNames = new HashSet<String>();
			for (CellInfo ci: row) {
				if (ci != null) {
					currentNames.add(ci.tableName);
				}
			}
			if (!currentNames.equals(lastNames)) {
				takeLastOrdering = false;
			} else {
				for (CellInfo ci: lastBestRow) {
					if (ci != null) {
						CellInfo cie = null;
						for (CellInfo ciRow: row) {
							if (ciRow != null && ciRow.tableName.equals(ci.tableName)) {
								cie = ciRow;
								break;
							}
						}
						if (cie == null) {
							takeLastOrdering = false;
							break;
						}
						if (cie.parentNames == null && ci.parentNames != null || cie.parentNames != null && ci.parentNames == null) {
							takeLastOrdering = false;
							break;
						}
						if (cie.parentNames != null && !cie.parentNames.equals(ci.parentNames)) {
							takeLastOrdering = false;
							break;
						}
					}
				}
			}
		}

		if (takeLastOrdering) {
			List<CellInfo> newRow = new ArrayList<CellInfo>();
			for (CellInfo ci: lastBestRow) {
				CellInfo cie = null;
				if (ci != null) {
					for (CellInfo ciRow: row) {
						if (ciRow != null && ciRow.tableName.equals(ci.tableName)) {
							cie = ciRow;
							break;
						}
					}
				}
				newRow.add(cie);
			}
			row = newRow;
		} else {
			int numUnknown = 0;
			int all = 0;
			for (CellInfo ci: row) {
				if (ci != null) {
					++all;
					if (ci.numberOfRows < 0) {
						++numUnknown;
					}
				}
			}
			
			int cd = 120000;
			int reducedTimeQuot = 1;
			if (numUnknown > 0.7 * all) {
				reducedTimeQuot = 2;
				cd /= reducedTimeQuot;
			}
			
			if (row.size() > 0) {
				cd = cd * 50 / row.size();
			}
			int maxParentRow = 1;
			if (parentRow != null) {
				for (CellInfo i: parentRow) {
					if (i != null && i.row > maxParentRow) {
						maxParentRow = i.row;
					}
				}
			}
			List<CellInfo> bestRow;
			double fitness = fitness(row, maxParentRow);
			bestRow = new ArrayList<CellInfo>(row);
			
			long startTime = System.currentTimeMillis();
			
			do {
				for (int a = row.size() - 1; a >= 0; --a) {
					for (int b = a - 1; b >= 0; --b) {
						CellInfo cA = row.get(a);
						CellInfo cB = row.get(b);
						row.set(a, cB);
						row.set(b, cA);
						double f = fitness(row, maxParentRow);
						if (f < fitness) {
							bestRow = new ArrayList<CellInfo>(row);
							fitness = f;
						}
						row.set(a, cA);
						row.set(b, cB);
						--cd;
						if (cd < 0) {
							break;
						}
						if (cd % 100 == 0) {
							if (System.currentTimeMillis() - startTime > (500 / reducedTimeQuot)) {
								cd = -1;
								break;
							}
						}
					}
					if (cd < 0) {
						break;
					}
				}
				if (bestRow != null) {
					row = bestRow;
				}
			} while (bestRow != null && cd > 0);
		
			lastBestRow = new ArrayList<CellInfo>(row);
		}

		dayS = " " + dayS;
		cellInfos.add(cells);
		int y = tableModel.getRowCount();
		int l = 0;
		for (List<CellInfo> cl : cellInfos) {
			if (!cl.isEmpty()) {
				++l;
			}
		}
		Color bg = (l % 2 == 0) ? BG1 : BG2;
		while (!row.isEmpty()) {
			Object[] rowO = new Object[MAX_TABLES_PER_LINE + 1];
			boolean rowIsEmpty = true;
			rowO[0] = dayS;
			dayS = "";
			for (int x = 0; x < MAX_TABLES_PER_LINE; ++x) {
				CellInfo cellInfo = row.isEmpty() ? null : row.remove(0);
				if (cellInfo == null) {
					rowO[x + 1] = "";
				} else {
					rowIsEmpty = false;
					cellInfo.column = x;
					cellInfo.row = y;
					rowO[x + 1] = cellInfo;
				}
			}
			if (!rowIsEmpty) {
				bgColors.add(bg);
				tableModel.addRow(rowO);
				++y;
			}
		}
		if (scrollToBottom && getRowCount() > 0) {
			Rectangle cellRect = getCellRect(getRowCount() - 1, 1, true);
			scrollRectToVisible(cellRect);
			scrollRectToVisible(cellRect);
		}
		invalidate();
		repaint();
	}

	/**
	 * Fitness (qualitiy) of the ordering of a row.
	 * 
	 * @param row the row
	 * @return the fitness, less is better
	 */
	private double fitness(List<CellInfo> row, int maxParentRow) {
		double f = 0;
		int conflictCount[] = new int[MAX_TABLES_PER_LINE];
		int lines = (int) Math.ceil(row.size() / (double) MAX_TABLES_PER_LINE);
		int minX[] = new int[lines];
		int maxX[] = new int[lines];

		for (int x = row.size() - 1; x >= 0; --x) {
			CellInfo cellInfo = row.get(x);
//			if (cellInfo != null) {
//				f += (x / MAX_TABLES_PER_LINE) / 2.0;
//			}
			if (cellInfo != null && cellInfo.parents != null) {
				for (CellInfo parent : cellInfo.parents) {
					int x0 = x % MAX_TABLES_PER_LINE;
					int y0 = x / MAX_TABLES_PER_LINE;
					double xabs = x0 - parent.column;
					if (xabs == 0.0) {
						f += MAX_TABLES_PER_LINE * MAX_TABLES_PER_LINE * conflictCount[parent.column]++;
					}
					double yabs = y0 + maxParentRow - parent.row;
					f += xabs * xabs + yabs * yabs / 10;
					if (y0 < lines) {
						if (minX[y0] == 0 || minX[y0] >= x0) {
							minX[y0] = x0 + 1;
						}
						if (maxX[y0] < x0) {
							maxX[y0] = x0;
						}
					}
				}
			}
		}
		for (int y = 0; y < lines; ++y) {
			f -= (maxX[y] - minX[y]) / (double) MAX_TABLES_PER_LINE;
		}
		return f;
	}

	/**
	 * Sets total number of collected rows.
	 * 
	 * @param totalNumberOfCollectedRows
	 *            total number of collected rows
	 */
	public void setTotalNumberOfCollectedRows(long totalNumberOfCollectedRows) {
		this.totalNumberOfCollectedRows = totalNumberOfCollectedRows;
	}

	/**
	 * Draws an arrow on the given Graphics2D context
	 * 
	 * @param g
	 *            The Graphics2D context to draw on
	 * @param x
	 *            The x location of the "tail" of the arrow
	 * @param y
	 *            The y location of the "tail" of the arrow
	 * @param xx
	 *            The x location of the "head" of the arrow
	 * @param yy
	 *            The y location of the "head" of the arrow
	 */
	private void drawArrow(Graphics2D g, int x, int y, int xx, int yy) {
		float arrowWidth = 8.0f;
		float theta = 0.623f;
		int[] xPoints = new int[3];
		int[] yPoints = new int[3];
		float[] vecLine = new float[2];
		float[] vecLeft = new float[2];
		float fLength;
		float th;
		float ta;
		float baseX, baseY;

		int mx1, my1;
		int mx2, my2;
		
		if (xx < x) {
			mx1 = yy - y;
			my1 = x - xx;
			mx2 = y - yy;
			my2 = xx - x;
		} else {
			mx1 = y - yy;
			my1 = xx - x;
			mx2 = yy - y;
			my2 = x - xx;
		}
		double f = 0.03;
		mx1 = (int) (mx1 * f + (x + xx) * 0.5);
		my1 = (int) (my1 * f + (y + yy) * 0.5);
		f = 0.04;
		mx2 = (int) (mx2 * f + (x + xx) * 0.5);
		my2 = (int) (my2 * f + (y + yy) * 0.5);

		xPoints[0] = xx;
		yPoints[0] = yy;

		// build the line vector
		vecLine[0] = (float) xPoints[0] - (x == xx? x : mx2);
		vecLine[1] = (float) yPoints[0] - (x == xx? y : my2);

		// build the arrow base vector - normal to the line
		vecLeft[0] = -vecLine[1];
		vecLeft[1] = vecLine[0];

		// setup length parameters
		fLength = (float) Math.sqrt(vecLine[0] * vecLine[0] + vecLine[1] * vecLine[1]);
		th = arrowWidth / (2.0f * fLength);
		ta = arrowWidth / (2.0f * ((float) Math.tan(theta) / 2.0f) * fLength);

		// find the base of the arrow
		baseX = (xPoints[0] - ta * vecLine[0]);
		baseY = (yPoints[0] - ta * vecLine[1]);

		// build the points on the sides of the arrow
		xPoints[1] = (int) (baseX + th * vecLeft[0]);
		yPoints[1] = (int) (baseY + th * vecLeft[1]);
		xPoints[2] = (int) (baseX - th * vecLeft[0]);
		yPoints[2] = (int) (baseY - th * vecLeft[1]);

		if (x == xx) {
			g.drawLine(x, y + 2, (int) baseX, (int) baseY);
		} else {
			Path2D.Double path = new Path2D.Double();
			path.moveTo(x, y + 4);
			path.curveTo(mx1, my1, mx2, my2, (int) baseX, (int) baseY);
			g.draw(path);
		}
		g.fillPolygon(xPoints, yPoints, 3);
	}

	/**
	 * Adjusts with of table columns.
	 */
	public void adjustColumnWidth() {
		if (getRowCount() == 0) {
			return;
		}
		
		DefaultTableColumnModel colModel = (DefaultTableColumnModel) getColumnModel();
		for (int vColIndex = 1; vColIndex < colModel.getColumnCount(); ++vColIndex) {
			TableColumn col = colModel.getColumn(vColIndex);
			boolean isEmpty = true;
			for (int r = 0; r < getRowCount(); r++) {
				TableCellRenderer renderer = getCellRenderer(r, vColIndex);
				Component comp = renderer.getTableCellRendererComponent(this, getValueAt(r, vColIndex), false, false, r, vColIndex);
				if (comp != layerdPane || tableRender.getText().trim().length() > 0) {
					isEmpty = false;
					break;
				}
			}
			int w;
			if (isEmpty) {
				w = 1;
			} else {
				w = 10000;
			}
			col.setMaxWidth(w);
		}
	}
	
	private boolean showExcludeFromDeletionImage = true;

	public void setShowExcludeFromDeletionImage(boolean showExcludeFromDeletionImage) {
		this.showExcludeFromDeletionImage = showExcludeFromDeletionImage;
	}

	private static ImageIcon sourceIcon;
	private static ImageIcon excludeFromDeletionImage;
	static {
		// load images
		sourceIcon = UIUtil.readImage("/source.png");
		excludeFromDeletionImage = UIUtil.readImage("/database-lock.png");
	}

	private static final long serialVersionUID = -6284876860992859979L;
}
