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
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
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
	private final Color BG1 = new Color(255, 255, 255);
	private final Color BG2 = new Color(240, 255, 255);
	private final Color SELECTED_FG = new Color(255, 40, 0);

	/**
	 * Holds infos about a cell.
	 */
	public static class CellInfo {
		// In
		public String tableName;
		public long numberOfRows;
		public Set<String> parentNames;
		// Calculated
		public int row, column;
		public List<CellInfo> parents;
	};

	/**
	 * Holds infos about a cells.
	 */
	private List<List<CellInfo>> cellInfos = new ArrayList<List<CellInfo>>();

	/**
	 * Selected cells.
	 */
	private Set<CellInfo> selectedCells = new HashSet<CellInfo>();
	private String selectedTableName = null;

	/**
	 * Total number of collected rows.
	 */
	private long totalNumberOfCollectedRows = 0;

	/**
	 * Cell render components.
	 */
	private final JPanel cellPanel = new JPanel();
	private final JLabel tableRender = new JLabel("");
	private final JLabel numberRender = new JLabel("");
	
	/** Creates new table */
	public ProgressTable() {
		setShowGrid(false);
		setSurrendersFocusOnKeystroke(true);
		
		tableRender.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
		numberRender.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
		cellPanel.setLayout(new GridBagLayout());
        GridBagConstraints gridBagConstraints = new GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        cellPanel.add(tableRender, gridBagConstraints);
        gridBagConstraints = new GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        cellPanel.add(numberRender, gridBagConstraints);
				
		tableRender.setOpaque(true);
		numberRender.setOpaque(true);
		setRowHeight(getRowHeight() * 3);

		setDefaultRenderer(Object.class, new TableCellRenderer() {
			private Font font = new JLabel("normal").getFont();
			private Font normal = new Font(font.getName(), font.getStyle() & ~Font.BOLD, font.getSize());
			private Font kursiv = new Font(font.getName(), (font.getStyle() & ~Font.BOLD) | Font.ITALIC, font.getSize());
			private Font bold = new Font(font.getName(), font.getStyle() | Font.BOLD, font.getSize());

			public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
				Color color = bgColors.get(row);
				boolean fontIsSet = false;
				tableRender.setForeground(Color.BLACK);
				numberRender.setForeground(Color.BLACK);
				cellPanel.setToolTipText(null);
				if (value instanceof CellInfo) {
					CellInfo cellInfo = (CellInfo) value;
					tableRender.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
					tableRender.setText(cellInfo.tableName);
					cellPanel.setToolTipText(cellInfo.tableName);
					if (selectedCells.contains(cellInfo)) {
						numberRender.setFont(bold);
						tableRender.setFont(bold);
						fontIsSet = true;
					}
					if (cellInfo.numberOfRows < 0) {
						numberRender.setText("?");
						tableRender.setForeground(SELECTED_FG);
						numberRender.setForeground(SELECTED_FG);
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
						numberRender.setText(cellInfo.numberOfRows + " rows" + (p == 0 ? "" : " (" + p + "%)"));
						tableRender.setForeground(Color.BLACK);
						float f = 0.2f + (p / 100.0f) * 0.8f;
						if (f > 1.0f) {
							f = 1.0f;
						}
						if (p == 0) {
							f = 0.0f;
						}
						numberRender.setForeground(new Color(f, 0.0f, 0.0f));
					}
				} else {
					tableRender.setHorizontalAlignment(javax.swing.SwingConstants.LEFT);
					tableRender.setText(" " + value);
					numberRender.setText("");
				}
				if (!fontIsSet) {
					numberRender.setFont(kursiv);
					tableRender.setFont(normal);
				}
				tableRender.setBackground(color);
				numberRender.setBackground(color);
				cellPanel.setBackground(color);
				return cellPanel;
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
						selectedTableName = cellInfo.tableName;
						selectCell(cellInfo);
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

	/**
	 * Recursively selects cells.
	 * 
	 * @param cellInfo current cell
	 */
	private void selectCell(CellInfo cellInfo) {
		selectedCells.add(cellInfo);
		if (cellInfo.parents != null) {
			for (CellInfo p : cellInfo.parents) {
				selectCell(p);
			}
		}
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

		return visible.y <= r.y && visible.y + visible.height >= r.y + r.height;
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
					selectCell(cellInfo);
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

		if (selectedTableName != null && checkSelection) {
			boolean f = false;
			for (CellInfo cellInfo : cellInfos.get(cellInfos.size() - 1)) {
				if (cellInfo != null && selectedTableName.equals(cellInfo.tableName)) {
					selectCell(cellInfo);
					f = true;
					break;
				}
			}
			if (!f) {
				selectedCells.clear();
				selectedTableName = null;
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
			int cd = 100000;
			int maxParentRow = 1;
			if (parentRow != null) {
				for (CellInfo i: parentRow) {
					if (i != null && i.row > maxParentRow) {
						maxParentRow = i.row;
					}
				}
			}
			List<CellInfo> bestRow = null;
			double fitness = fitness(row, maxParentRow);
			do {
				double best = -1;
				bestRow = null;
				for (int a = row.size() - 1; a >= 0; --a) {
					for (int b = a - 1; b >= 0; --b) {
						CellInfo cA = row.get(a);
						CellInfo cB = row.get(b);
						row.set(a, cB);
						row.set(b, cA);
						double f = fitness(row, maxParentRow);
						if (f < fitness) {
							if (best < 0 || best > f) {
								bestRow = new ArrayList<CellInfo>(row);
								best = f;
							}
						}
						row.set(a, cA);
						row.set(b, cB);
						--cd;
					}
				}
				if (bestRow != null) {
					row = bestRow;
					fitness = best;
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
			scrollRectToVisible(getCellRect(getRowCount() - 1, 1, true));
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

		for (int x = row.size() - 1; x >= 0; --x) {
			CellInfo cellInfo = row.get(x);
//			if (cellInfo != null) {
//				f += (x / MAX_TABLES_PER_LINE) / 2.0;
//			}
			if (cellInfo != null && cellInfo.parents != null) {
				for (CellInfo parent : cellInfo.parents) {
					double xabs = (x % MAX_TABLES_PER_LINE) - parent.column;
					if (xabs == 0.0) {
						f += MAX_TABLES_PER_LINE * MAX_TABLES_PER_LINE * conflictCount[parent.column]++;
					}
					double yabs = (x / MAX_TABLES_PER_LINE) + maxParentRow - parent.row;
					f += xabs * xabs + yabs * yabs / 10;
				}
			}
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
		float arrowWidth = 6.0f;
		float theta = 0.423f;
		int[] xPoints = new int[3];
		int[] yPoints = new int[3];
		float[] vecLine = new float[2];
		float[] vecLeft = new float[2];
		float fLength;
		float th;
		float ta;
		float baseX, baseY;

		xPoints[0] = xx;
		yPoints[0] = yy;

		// build the line vector
		vecLine[0] = (float) xPoints[0] - x;
		vecLine[1] = (float) yPoints[0] - y;

		// build the arrow base vector - normal to the line
		vecLeft[0] = -vecLine[1];
		vecLeft[1] = vecLine[0];

		// setup length parameters
		fLength = (float) Math.sqrt(vecLine[0] * vecLine[0] + vecLine[1] * vecLine[1]);
		th = arrowWidth / (2.0f * fLength);
		ta = arrowWidth / (2.0f * ((float) Math.tan(theta) / 2.0f) * fLength);

		// find the base of the arrow
		baseX = ((float) xPoints[0] - ta * vecLine[0]);
		baseY = ((float) yPoints[0] - ta * vecLine[1]);

		// build the points on the sides of the arrow
		xPoints[1] = (int) (baseX + th * vecLeft[0]);
		yPoints[1] = (int) (baseY + th * vecLeft[1]);
		xPoints[2] = (int) (baseX - th * vecLeft[0]);
		yPoints[2] = (int) (baseY - th * vecLeft[1]);

		g.drawLine(x, y, (int) baseX, (int) baseY);
		g.fillPolygon(xPoints, yPoints, 3);
	}

	/**
	 * Original column width.
	 */
	private int[] originalWidth;
	
	/**
	 * Adjusts with of table columns.
	 */
	public void adjustColumnWidth() {
		if (getRowCount() == 0) {
			return;
		}
		DefaultTableColumnModel colModel = (DefaultTableColumnModel) getColumnModel();
		if (originalWidth == null) {
			originalWidth = new int[colModel.getColumnCount()];
			for (int vColIndex = 0; vColIndex < colModel.getColumnCount(); ++vColIndex) {
				TableColumn col = colModel.getColumn(vColIndex);
				originalWidth[vColIndex] = col.getMaxWidth();
			}
		}
		for (int vColIndex = 0; vColIndex < colModel.getColumnCount(); ++vColIndex) {
			TableColumn col = colModel.getColumn(vColIndex);
			boolean isEmpty = true;
			for (int r = 0; r < getRowCount(); r++) {
				TableCellRenderer renderer = getCellRenderer(r, vColIndex);
				Component comp = renderer.getTableCellRendererComponent(this, getValueAt(r, vColIndex), false, false, r, vColIndex);
				if (comp != cellPanel || tableRender.getText().trim().length() > 0) {
					isEmpty = false;
				}
			}
			if (isEmpty) {
				col.setMaxWidth(1);
			} else {
				col.setMaxWidth(originalWidth[vColIndex]);
			}
		}
	}

	private static final long serialVersionUID = -6284876860992859979L;
}
