/*
 * Copyright 2007 - 2022 Ralf Wisser.
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
package net.sf.jailer.ui.databrowser.sqlconsole;

import java.awt.Color;
import java.awt.Component;
import java.awt.FontMetrics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.Types;
import java.util.ArrayList;
import java.util.List;

import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.RowSorter;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;

import org.fife.ui.rsyntaxtextarea.RSyntaxDocument;
import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.fife.ui.rsyntaxtextarea.SyntaxConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.formdev.flatlaf.FlatClientProperties;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;
import net.sf.jailer.modelbuilder.MemorizedResultSet;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.UIUtil.PLAF;
import net.sf.jailer.ui.databrowser.BrowserContentPane.TableModelItem;
import net.sf.jailer.ui.databrowser.metadata.MetaDataDetailsPanel;
import net.sf.jailer.ui.syntaxtextarea.RSyntaxTextAreaWithSQLSyntaxStyle;
import net.sf.jailer.util.Pair;

public class TabContentPanel extends javax.swing.JPanel {

	private final String SEPARATOR_TAB = "Tabulator";
	private final String SEPARATOR_SPACE = "Space";

	/**
	 * The logger.
	 */
	private static final Logger logger = LoggerFactory.getLogger(MetaDataDetailsPanel.class);
	
	private final boolean silent;
	
	/**
     * Creates new form TabContentPanel
     * @param caretDotMark 
     */
    public TabContentPanel(JLabel rowsCount, JTable theRowsTable, JComponent metaDataDetails, String type, boolean explain, javax.swing.JPanel shimPanel, Pair<Integer, Integer> caretDotMark, List<Integer> rowColumnTypes, boolean onlySelectedCells, boolean silent) {
    	this.shimPanel = shimPanel == null? new javax.swing.JPanel(new GridBagLayout()) : shimPanel;
    	this.caretDotMark = caretDotMark;
        this.rowColumnTypes = rowColumnTypes;
        this.onlySelectedCells = onlySelectedCells;
        this.theRowsTable = theRowsTable;
        this.silent = silent;
        
        initComponents();
        if (UIUtil.plaf == PLAF.FLAT) {
			tabbedPane.putClientProperty(FlatClientProperties.TABBED_PANE_TAB_HEIGHT, 16);
		}
        loadingPanel.setVisible(false);
        
        textArea = new RSyntaxTextAreaWithSQLSyntaxStyle(
				false, false) {
			protected boolean withModifingMenuItems() {
				return false;
			}
		};
		textArea.setSyntaxEditingStyle(null);
		textArea.setEditable(false);
		
        textViewScrollPane.setViewportView(textArea);
        
        cancelLoadButton.setIcon(UIUtil.scaleIcon(cancelLoadButton, cancelIcon));
        copyCBButton.setIcon(UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/copy.png")));
		
        statementLabel = new JLabel();
        statementLabel.setForeground(new Color(100, 110, 210));
        GridBagConstraints gridBagConstraints;
        
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1;
        gridBagConstraints.anchor = explain? java.awt.GridBagConstraints.WEST : java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new Insets(0, explain? 8 : 16, 0, 2);
		//        panel.add(statementLabel, gridBagConstraints);
        panel.add(new JPanel() {
	        	{
	        		setLayout(null);
	        		setOpaque(false);
	        		statementLabel.addPropertyChangeListener("text", e -> resize());
	        		addComponentListener(new ComponentListener() {
						@Override
						public void componentShown(ComponentEvent e) {
						}
						@Override
						public void componentResized(ComponentEvent e) {
							resize();
						}
						@Override
						public void componentMoved(ComponentEvent e) {
						}
						@Override
						public void componentHidden(ComponentEvent e) {
						}
					});
	        		statementLabel.setName("statementLabel");
		        	add(statementLabel);
		    	}
	        	
				private void resize() {
					Rectangle visibleRect = getBounds();
					int x = Math.max(32, visibleRect.width - statementLabel.getPreferredSize().width);
					statementLabel.setBounds(x, 0, visibleRect.width - x, visibleRect.height);
				}
        	}
        	, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 20;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new Insets(0, 10, 0, 10);
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        panel.add(rowsCount, gridBagConstraints);
        
        ComboBoxModel<String> sepModel = new DefaultComboBoxModel<String>(
        		new String[] {
        				"",
        				";",
        				",",
        				"|",
        				"!",
        				SEPARATOR_TAB,
        				SEPARATOR_SPACE
        		});
		columnSeparatorComboBox.setModel(sepModel);
		if (!silent && !explain && lastColumnSeparator != null && !"".equals(lastColumnSeparator)) {
			columnSeparatorComboBox.setSelectedItem(lastColumnSeparator);
		}
	    Component ec = columnSeparatorComboBox.getEditor().getEditorComponent();
		if (ec instanceof JTextField) {
			((JTextField) ec).getDocument().addDocumentListener(new DocumentListener() {
				@Override
				public void removeUpdate(DocumentEvent e) {
					update();
				}
				@Override
				public void insertUpdate(DocumentEvent e) {
					update();
				}
				@Override
				public void changedUpdate(DocumentEvent e) {
					update();
				}
				private void update() {
					if (TabContentPanel.this.theRowsTable != null) {
						updateTextView(TabContentPanel.this.theRowsTable);
					}
				}
			});
		}
		
		if (!silent && !explain && lastHeaderCheckBoxIsSelected != null) {
			headerCheckBox.setSelected(lastHeaderCheckBoxIsSelected);
		}
		if (!silent && !explain && lastRotated != null) {
			rotateCheckBox.setSelected(lastRotated);
		}
		
		JScrollPane scrollPane = new JScrollPane();
		RSyntaxTextArea area = new RSyntaxTextArea();
		area.setBracketMatchingEnabled(false);

		area.setEditable(false);
		scrollPane.setViewportView(area);
		area.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_SQL);
		area.setText(type);
		area.setCaretPosition(0);
		typePanel.add(scrollPane); 
		
		if (explain) {
			tabbedPane.remove(contentPanel);
			tabbedPane.remove(columnsPanel);
			tabbedPane.remove(metaTabPanel);
			columnSeparatorLabel.setVisible(false);
			headerCheckBox.setVisible(false);
			copyCBButton.setVisible(false);
			rotateCheckBox.setVisible(false);
			columnSeparatorComboBox.setVisible(false);
			// controlsContainer.setVisible(false);
			rowsCount.setVisible(false);
		} else {
			if (metaDataDetails != null) {
				metaPanel.add(metaDataDetails);
			}
		}
  		
		this.shimPanel.removeAll();
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.weightx = 1;
        gridBagConstraints.weighty = 1;
        gridBagConstraints.fill = GridBagConstraints.BOTH;
        this.shimPanel.add(this, gridBagConstraints);
    }

    public static String toType(ResultSetMetaData metaData, Session session, ExecutionContext executionContext) throws Exception {
        StringBuilder columnTypes = new StringBuilder("(" + UIUtil.LINE_SEPARATOR);
    	String nullableContraint = session.dbms.getNullableContraint();
        for (int i = 0; i < metaData.getColumnCount(); ++i) {
        	if (i > 0) {
        		columnTypes.append(","+ UIUtil.LINE_SEPARATOR);
        	}
        	Column column = JDBCMetaDataBasedModelElementFinder.toColumn(metaData, i + 1, session);
        	String constraint;
        	if (nullableContraint != null) {
        		constraint = column.isNullable? " " + nullableContraint :  " NOT NULL";
            } else {
            	constraint = column.isNullable? "" : " NOT NULL";
            }
			columnTypes.append("     "
					+ (column.name == null ? ""
							: column.name.replaceAll("\\s+", "").replaceFirst("^(.{64})....*", "$1..."))
					+ " " 
					+ (column.name == null ? ""
							: column.toSQL("").substring(column.name.length()).trim()) + constraint);
        }
        columnTypes.append(UIUtil.LINE_SEPARATOR + ")");
        return columnTypes.toString();
    }

    public static ResultSet toMetaDataResultSet(ResultSetMetaData metaData, Session session, ExecutionContext executionContext) throws Exception {
        List<Object[]> rowList = new ArrayList<Object[]>();
        
        String[] names = new String[] {
        		"getColumnLabel",
        		"getColumnTypeName",
        		"getPrecision",
        		"getScale",
        		"getColumnType",
        		"isAutoIncrement",
        		"isCaseSensitive",
        		"isNullable",
        		"isReadOnly",
        		"isSigned"
	    };

        String[] titel = new String[names.length];
        int[] types = new int[names.length];
    	int i = 0;
        for (String name: names) {
    		titel[i] = name.startsWith("get")? name.substring(3) : name;
    		types[i] = Types.VARCHAR;
    		++i;
        }
        for (int col = 0; col < metaData.getColumnCount(); ++col) {
        	Object[] row = new Object[names.length];
        	i = 0;
        	try {
        		row[i] = metaData.getColumnLabel(col + 1);
        	} catch (Throwable t) {
        		logger.info("error", t);
        	}
        	++i;
        	try {
        		row[i] = metaData.getColumnTypeName(col + 1);
        	} catch (Throwable t) {
        		logger.info("error", t);
        	}
        	++i;
        	try {
        		row[i] = metaData.getPrecision(col + 1);
        	} catch (Throwable t) {
        		logger.info("error", t);
        	}
        	++i;
        	try {
        		row[i] = metaData.getScale(col + 1);
        	} catch (Throwable t) {
        		logger.info("error", t);
        	}
        	++i;
        	try {
        		row[i] = metaData.getColumnType(col + 1);
        	} catch (Throwable t) {
        		logger.info("error", t);
        	}
        	++i;
        	try {
        		row[i] = metaData.isAutoIncrement(col + 1);
        	} catch (Throwable t) {
        		logger.info("error", t);
        	}
        	++i;
        	try {
        		// not necessary, MySQL driver throws exception here, row[i] = metaData.isCaseSensitive(col + 1);
        	} catch (Throwable t) {
        		logger.info("error", t);
        	}
        	++i;
        	try {
        		row[i] = metaData.isNullable(col + 1);
        	} catch (Throwable t) {
        		logger.info("error", t);
        	}
        	++i;
        	try {
        		row[i] = metaData.isReadOnly(col + 1);
        	} catch (Throwable t) {
        		logger.info("error", t);
        	}
        	++i;
        	try {
        		row[i] = metaData.isSigned(col + 1);
        	} catch (Throwable t) {
        		logger.info("error", t);
        	}
        	++i;
	        rowList.add(row);
        }

        return new MemorizedResultSet(rowList, names.length, titel, types);
    }

    private JTable theRowsTable = null;
    protected boolean forDetailsView = false;
    private boolean columnNamesInFirstRow = false;
    
    public void setColumnNamesInFirstRow(boolean columnNamesInFirstRow) {
		this.columnNamesInFirstRow = columnNamesInFirstRow;
	}

	private static Boolean lastHeaderCheckBoxIsSelected;
	private static Object lastColumnSeparator;
	private static Boolean lastRotated;
    private final boolean onlySelectedCells;
    
    private int rowAndColumnsLimit = Integer.MAX_VALUE;
    
    public void setRowAndColumnsLimit(int rowAndColumnsLimit) {
		this.rowAndColumnsLimit = rowAndColumnsLimit;
	}

	public void updateTextView(JTable rowsTable) {
    	theRowsTable = rowsTable;
		Object sep = getSeparatorFromCombobox();
    	StringBuilder sb = createContent(rowsTable, sep, false, false, null, rowAndColumnsLimit, rowAndColumnsLimit, new boolean[2]);

		Point vPos = textViewScrollPane.getViewport().getViewPosition();
		textArea.setText(sb.toString());
		textArea.setCaretPosition(0);
		textArea.setEditable(false);
		textArea.discardAllEdits();
		if (UIUtil.plaf == PLAF.FLAT) {
			textArea.setBackground(Color.white);
		}
		UIUtil.invokeLater(() -> textViewScrollPane.getViewport().setViewPosition(vPos));
	}

	/**
	 * @return
	 */
	private Object getSeparatorFromCombobox() {
		Object sep = columnSeparatorComboBox.getEditor().getItem();
		if (!silent && columnSeparatorComboBox.isVisible()) {
			lastColumnSeparator = sep;
		}
		if ("".equals(sep)) {
			sep = null;
		} else if (SEPARATOR_SPACE.equals(sep)) {
			sep = " ";
		} else if (SEPARATOR_TAB.equals(sep)) {
			sep = "\t";
		}
		return sep;
	}

	private StringBuilder createContent(JTable rowsTable, Object sep, boolean aligned, boolean colored, Integer cellpadding, int maxColumns, int maxRows, boolean[] stopped) {
		boolean html = false;
    	if ("HTML".equals(sep)) {
    		sep = "";
    		html = true;
    	}
    	TableColumnModel cm = rowsTable.getColumnModel();
    	TableModel rDm = rowsTable.getModel();
    	RowSorter<? extends TableModel> sorter = rowsTable.getRowSorter();
		int startI = 0;
		boolean incHeader = headerCheckBox.isSelected();
		boolean rotate = rotateCheckBox.isSelected();
    	if (!silent && headerCheckBox.isVisible()) {
			lastHeaderCheckBoxIsSelected = incHeader;
		}
    	if (!silent && rotateCheckBox.isVisible()) {
//    		lastRotated = rotateCheckBox.isVisible();
    	}
		if (forDetailsView) {
			if (!incHeader) {
				startI = cm.getColumnCount() - 1;
			} else if (rotate) {
				incHeader = false;
			}
		}
		String[][] cell = columnNamesInFirstRow? (rotate? new String[rDm.getColumnCount()][] : new String[sorter.getViewRowCount()][]) : rotate? new String[rDm.getColumnCount()][] : new String[sorter.getViewRowCount() + 1][];
    	int[] maxLength = columnNamesInFirstRow? (rotate? new int[sorter.getViewRowCount()] : new int[rDm.getColumnCount()]) : rotate? new int[sorter.getViewRowCount() + 1] : new int[rDm.getColumnCount()];
    	for (int y = 0; y < cell.length; ++y) {
			cell[y] = columnNamesInFirstRow? (rotate? new String[sorter.getViewRowCount()] : new String[rDm.getColumnCount()]) : rotate? new String[sorter.getViewRowCount() + 1] : new String[rDm.getColumnCount()];
    	}
    	int pSHX = -1;
    	int yCount = 0;
    	for (int y = 0; y < cell.length; ++y) {
    		if (yCount > maxRows) {
    			cell[y][0] = html? null : "Preview stops here...";
    			stopped[1] = true;
    			if (pSHX > 0) {
    				for (int x = 1; x < pSHX; ++x) {
    					cell[y][x] = "";
    				}
    				cell[y][pSHX] = cell[y][0];
    			}
    			break;
    		}
    		int xCount = 0;
        	for (int x = 0; x < cell[y].length; ++x) {
				int mx;
				int my;
				int vx;
				int vy;
				String colName;
				boolean isColName;
				if (columnNamesInFirstRow) {
					if (!rotate) {
						isColName = x == 0;
						vx = x - 1;
						vy = y;
						mx = x == 0? 0 : cm.getColumn(x - 1).getModelIndex();
						my = sorter.convertRowIndexToModel(y);
						colName = String.valueOf(rDm.getValueAt(my, 0));
					} else {
						isColName = y == 0;
						vx = y - 1;
						vy = x;
						mx = y == 0? 0 : cm.getColumn(y - 1).getModelIndex();
						my = x == 0? 0 : sorter.convertRowIndexToModel(x);
						colName = String.valueOf(rDm.getValueAt(my, 0));
					}
				} else {
					if (rotate) {
						isColName = x == 0;
						vx = y;
						vy = x - 1;
						mx = cm.getColumn(y).getModelIndex();
						my = x == 0? 0 : sorter.convertRowIndexToModel(x - 1);
						colName = rDm.getColumnName(mx);
					} else {
						isColName = y == 0;
						vx = x;
						vy = y - 1;
						mx = cm.getColumn(x).getModelIndex();
						my = y == 0? 0 : sorter.convertRowIndexToModel(y - 1);
						colName = rDm.getColumnName(mx);
					}
				}
				Object value;
				if (isColName) {
					if (onlySelectedCells) {
						if (columnNamesInFirstRow && !rowsTable.isRowSelected(vy)
								||
							!columnNamesInFirstRow && !rowsTable.isColumnSelected(vx)) {
							continue;
						}
					}
					value = colName;
					if (value != null && value.toString().startsWith("<html>")) {
						String[] ntPair = value.toString().replaceAll("<br>", "\t").replaceAll("<[^>]*>", "").split("\t");
						if (ntPair.length == 2) {
							value = ntPair[0];
						}
						if (ntPair.length == 3) {
							value = ntPair[1];
						}
					}
				} else {
					if (onlySelectedCells && !rowsTable.isCellSelected(vy, vx)) {
						continue;
					}
					value = rDm.getValueAt(my, mx);
				}
				if (value instanceof TableModelItem) {
					Object v = ((TableModelItem) value).value;
					if (sep != null || (v == UIUtil.NULL || v == null)) {
						value = v;
					} else {
						value = value.toString().replace((char) 182, '\n').trim();
					}
				}
				String cellContent = value == UIUtil.NULL || value == null? "" : value.toString();
				if (sep != null && !html) {
					if (cellContent.contains(sep.toString()) || cellContent.indexOf("\n") >= 0 || cellContent.indexOf("\r") >= 0 || cellContent.indexOf("\"") >= 0) {
						cellContent = "\"" + (cellContent.replaceAll("\"", "\"\"")) + "\"";
					}
				}
				 ++xCount;
				boolean stop = xCount > maxColumns;
				if (stop) {
					cellContent = "";
	    			stopped[0] = true;
	    			if (yCount == 1 && !html) {
						cellContent = "Preview stops here...";
					} else {
						cellContent = "";
					}
				}
				cell[y][x] = cellContent;
				maxLength[x] = Math.max(maxLineLength(cellContent), maxLength[x]);
				pSHX = Math.max(pSHX, x);
				if (stop) {
					break;
				}
			}
        	if (xCount > 0) {
        		++yCount;
        	}
		}
		StringBuilder sb = new StringBuilder();
		int cellsRendered = 0;
		synchronized (rowColumnTypes) {
			if (html) {
				sb.append("<html><body><table" + (cellpadding != null? " cellpadding=\"" + cellpadding + "\"" : "") + ">");
			}
			int rowNum = 0;
			if (!columnNamesInFirstRow) {
				if (incHeader && !rotate) {
					++rowNum;
				}
			}
			for (int y = !rotate? 0 : startI; y < cell.length; ++y) {
				int sbLength = sb.length();
				for (int x = !rotate? startI : 0; x < cell[y].length; ++x) {
					if (cell[y][x] == null) {
						continue;
					}
					if (columnNamesInFirstRow && (!rotate && x == 0 || rotate && y == 0) && !incHeader) {
						continue;
					}
					if (!columnNamesInFirstRow && (rotate && x == 0 || !rotate && y == 0) && !incHeader) {
						continue;
					}
					boolean rightAlign = false;
					if (!rotate && !columnNamesInFirstRow) {
						int mx = cm.getColumn(x).getModelIndex();
						if (rowColumnTypes.size() > mx) {
							switch (rowColumnTypes.get(mx)) {
							case Types.BIGINT:
							case Types.DECIMAL:
							case Types.DOUBLE:
							case Types.FLOAT:
							case Types.INTEGER:
							case Types.NUMERIC:
							case Types.REAL:
							case Types.SMALLINT:
								rightAlign = true;
							}
						}
					} else if (rotate && columnNamesInFirstRow) {
						int mx = sorter.convertRowIndexToModel(x);
						if (rowColumnTypes.size() > mx) {
							switch (rowColumnTypes.get(mx)) {
							case Types.BIGINT:
							case Types.DECIMAL:
							case Types.DOUBLE:
							case Types.FLOAT:
							case Types.INTEGER:
							case Types.NUMERIC:
							case Types.REAL:
							case Types.SMALLINT:
								rightAlign = true;
							}
						}
					}
					if (sep != null) {
						if (html) {
							if (sb.length() > sbLength) {
								sb.append("</td><td");
								if (columnNamesInFirstRow) {
									++rowNum;
								}
							} else {
								sb.append("<tr><td");
								if (columnNamesInFirstRow) {
									rowNum = 0;
									if (!incHeader || rotate) {
										++rowNum;
									}
								} else {
									++rowNum;
								}
							}
							if (incHeader && ((rotate ^ columnNamesInFirstRow)? (x == 0) : (y == 0))) {
								if (colored) {
									sb.append(" bgcolor=\"#eeeeff\"");
								}
							} else {
								if (colored) {
									sb.append(rowNum % 2 == 0? "" : " bgcolor=\"#eeffee\"");
								}
							}
							if (aligned) {
								sb.append(rightAlign? " align=\"right\"" : "");
							}
							sb.append(">" + UIUtil.toHTMLFragment(cell[y][x], 0, true, false));
						} else {
							if (sb.length() > sbLength) {
								sb.append(sep);
							}
							sb.append(cell[y][x]);
						}
					} else {
						if (x < cell[y].length - 1) {
							if (sb.length() > sbLength) {
								sb.append(" ");
							}
							if (rightAlign) {
								for (int i = maxLength[x] - lastLineLength(cell[y][x]); i > 0; --i) {
									sb.append(" ");
								}
								sb.append(cell[y][x]);
							} else {
								sb.append(cell[y][x]);
								for (int i = maxLength[x] - lastLineLength(cell[y][x]); i > 0; --i) {
									sb.append(" ");
								}
							}
							sb.append(" ");
						} else {
							sb.append(" ");
							if (rightAlign) {
								for (int i = maxLength[x] - lastLineLength(cell[y][x]); i > 0; --i) {
									sb.append(" ");
								}
							}
							sb.append(cell[y][x]);
						}
						++cellsRendered;
					}
				}
				if (y < cell.length - 1 && sb.length() > sbLength) {
					if (html) {
						sb.append("</td></tr>");
					}
					sb.append(UIUtil.LINE_SEPARATOR);
				}
				if (y == 0 && sep == null && incHeader && !(rotate ^ columnNamesInFirstRow)) {
					int o = 2;
					if (cell[y].length == 1) {
						sb.append(" ");
					}
					for (int x = 0; x < cell[y].length; ++x) {
						if (maxLength[x] > 0) {
							for (int i = o; i < 2 + maxLength[x]; ++i) {
								sb.append(i < 2? " " : "-");
							}
							o = 0;
						}
					}
					sb.append(UIUtil.LINE_SEPARATOR);
				}
			}
		}
		if (html) {
			sb.append("</body></table></html>");
		} else if (cellsRendered == 1) {
			sb = new StringBuilder(sb.toString().trim());
		}
		
		return sb;
	}

    private int lastLineLength(String cellContent) {
		int i = cellContent.lastIndexOf('\n');
		int l = cellContent.length();
		if (i >= 0) {
			return l - i - 1;
		}
		return l;
	}

	private int maxLineLength(String cellContent) {
    	int l = cellContent.length();
    	int currentLength = 0;
    	int maxLength = 0;
		for (int i = 0; i < l; ++i) {
			char c = cellContent.charAt(i);
			if (c == '\n') {
				currentLength = 0;
			} else {
				++currentLength;
			}
			if (maxLength < currentLength) {
				maxLength = currentLength;
			}
		}
		return maxLength;
	}

	public String getHTMLContent(JTable rowsTable, boolean aligned, boolean colored, Integer cellpadding, int maxColumns, int maxRows, boolean[] stopped) {
		return createContent(rowsTable, "HTML", aligned, colored, cellpadding, maxColumns, maxRows, stopped).toString();
	}
	
	public String getPlainContent(JTable rowsTable, boolean aligned, boolean colored, int maxColumns, int maxRows, boolean[] stopped) {
		return createContent(rowsTable, getSeparatorFromCombobox(), aligned, colored, null, maxColumns, maxRows, stopped).toString();
	}

	/**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        panel = new javax.swing.JPanel();
        controlsContainer = new javax.swing.JToolBar();
        jPanel1 = new javax.swing.JPanel();
        jLayeredPane1 = new javax.swing.JLayeredPane();
        loadingPanel = new javax.swing.JPanel();
        cancelLoadButton = new javax.swing.JButton();
        jLabel2 = new javax.swing.JLabel();
        jPanel13 = new javax.swing.JPanel();
        loadingCauseLabel = new javax.swing.JLabel();
        loadingLabel = new javax.swing.JLabel();
        tabbedPane = new javax.swing.JTabbedPane();
        contentPanel = new javax.swing.JPanel();
        columnsPanel = new javax.swing.JPanel();
        columnsScrollPane = new javax.swing.JScrollPane();
        columnsSortedStateLabel = new javax.swing.JLabel();
        fullTSearchPanel = new javax.swing.JPanel();
        textTabPanel = new javax.swing.JPanel();
        textViewScrollPane = new javax.swing.JScrollPane();
        jTextArea = new javax.swing.JTextArea();
        columnSeparatorComboBox = new javax.swing.JComboBox<>();
        copyCBButton = new javax.swing.JButton();
        columnSeparatorLabel = new javax.swing.JLabel();
        headerCheckBox = new javax.swing.JCheckBox();
        textSortedStateLabel = new javax.swing.JLabel();
        rotateCheckBox = new javax.swing.JCheckBox();
        metaTabPanel = new javax.swing.JPanel();
        metaPanel = new javax.swing.JPanel();
        typePanel = new javax.swing.JPanel();

        setLayout(new java.awt.GridBagLayout());

        panel.setLayout(new java.awt.GridBagLayout());

        controlsContainer.setFloatable(false);
        controlsContainer.setRollover(true);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        panel.add(controlsContainer, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        add(panel, gridBagConstraints);

        jPanel1.setLayout(new java.awt.GridBagLayout());

        jLayeredPane1.setLayout(new java.awt.GridBagLayout());

        loadingPanel.setBackground(new Color(255,255,255,150));
        loadingPanel.setLayout(new java.awt.GridBagLayout());

        cancelLoadButton.setText("Cancel");
        cancelLoadButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelLoadButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        loadingPanel.add(cancelLoadButton, gridBagConstraints);

        jLabel2.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.weightx = 1.0;
        loadingPanel.add(jLabel2, gridBagConstraints);

        jPanel13.setOpaque(false);
        jPanel13.setLayout(new java.awt.GridBagLayout());

        loadingCauseLabel.setFont(loadingCauseLabel.getFont().deriveFont(loadingCauseLabel.getFont().getSize()+3f));
        loadingCauseLabel.setForeground(new java.awt.Color(141, 16, 16));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel13.add(loadingCauseLabel, gridBagConstraints);

        loadingLabel.setFont(loadingLabel.getFont().deriveFont(loadingLabel.getFont().getStyle() | java.awt.Font.BOLD, loadingLabel.getFont().getSize()+3));
        loadingLabel.setForeground(new java.awt.Color(141, 16, 16));
        loadingLabel.setText("loading...     ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel13.add(loadingLabel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(8, 8, 8, 8);
        loadingPanel.add(jPanel13, gridBagConstraints);

        jLayeredPane1.setLayer(loadingPanel, javax.swing.JLayeredPane.PALETTE_LAYER);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 2, 4);
        jLayeredPane1.add(loadingPanel, gridBagConstraints);

        tabbedPane.setTabPlacement(javax.swing.JTabbedPane.BOTTOM);

        contentPanel.setFont(new java.awt.Font("Monospaced", 0, 12)); // NOI18N
        contentPanel.setLayout(new java.awt.BorderLayout());
        tabbedPane.addTab("Rows", contentPanel);

        columnsPanel.setLayout(new java.awt.BorderLayout());
        columnsPanel.add(columnsScrollPane, java.awt.BorderLayout.CENTER);

        columnsSortedStateLabel.setForeground(java.awt.Color.blue);
        columnsSortedStateLabel.setText(" ");
        columnsPanel.add(columnsSortedStateLabel, java.awt.BorderLayout.NORTH);

        fullTSearchPanel.setLayout(new javax.swing.BoxLayout(fullTSearchPanel, javax.swing.BoxLayout.LINE_AXIS));
        columnsPanel.add(fullTSearchPanel, java.awt.BorderLayout.PAGE_END);

        tabbedPane.addTab("Columns", columnsPanel);

        textTabPanel.setLayout(new java.awt.GridBagLayout());

        textViewScrollPane.setEnabled(false);
        textViewScrollPane.setFont(new java.awt.Font("Monospaced", 0, 12)); // NOI18N

        jTextArea.setColumns(20);
        jTextArea.setFont(new java.awt.Font("Monospaced", 0, 12)); // NOI18N
        jTextArea.setRows(5);
        textViewScrollPane.setViewportView(jTextArea);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 0);
        textTabPanel.add(textViewScrollPane, gridBagConstraints);

        columnSeparatorComboBox.setEditable(true);
        columnSeparatorComboBox.setModel(new javax.swing.DefaultComboBoxModel<>(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 4);
        textTabPanel.add(columnSeparatorComboBox, gridBagConstraints);

        copyCBButton.setText("Copy to Clipboard");
        copyCBButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                copyCBButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 5;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 2);
        textTabPanel.add(copyCBButton, gridBagConstraints);

        columnSeparatorLabel.setText("  Separator ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        textTabPanel.add(columnSeparatorLabel, gridBagConstraints);

        headerCheckBox.setSelected(true);
        headerCheckBox.setText("With Columns");
        headerCheckBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                headerCheckBoxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        textTabPanel.add(headerCheckBox, gridBagConstraints);

        textSortedStateLabel.setForeground(java.awt.Color.blue);
        textSortedStateLabel.setText("jLabel2");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        textTabPanel.add(textSortedStateLabel, gridBagConstraints);

        rotateCheckBox.setText("Rotated");
        rotateCheckBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                rotateCheckBoxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        textTabPanel.add(rotateCheckBox, gridBagConstraints);

        tabbedPane.addTab("Text", textTabPanel);

        metaTabPanel.setLayout(new java.awt.GridBagLayout());

        metaPanel.setLayout(new java.awt.BorderLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        metaTabPanel.add(metaPanel, gridBagConstraints);

        typePanel.setBorder(javax.swing.BorderFactory.createTitledBorder("Type"));
        typePanel.setLayout(new javax.swing.BoxLayout(typePanel, javax.swing.BoxLayout.LINE_AXIS));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        metaTabPanel.add(typePanel, gridBagConstraints);

        tabbedPane.addTab("Meta", metaTabPanel);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jLayeredPane1.add(tabbedPane, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel1.add(jLayeredPane1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(jPanel1, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    private void copyCBButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_copyCBButtonActionPerformed
    	StringSelection selection = new StringSelection(textArea.getText());
	    Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
	    clipboard.setContents(selection, selection);
    }//GEN-LAST:event_copyCBButtonActionPerformed

    private void headerCheckBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_headerCheckBoxActionPerformed
    	if (theRowsTable != null) {
			updateTextView(theRowsTable);
		}
    }//GEN-LAST:event_headerCheckBoxActionPerformed

    private void cancelLoadButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelLoadButtonActionPerformed
    }//GEN-LAST:event_cancelLoadButtonActionPerformed

    private void rotateCheckBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_rotateCheckBoxActionPerformed
    	if (theRowsTable != null) {
			updateTextView(theRowsTable);
		}
    }//GEN-LAST:event_rotateCheckBoxActionPerformed

	public void repaintShowingAnimatedTables() {
		if (isShowing()) {
			columnsScrollPane.repaint();
			if (theRowsTable != null) {
				theRowsTable.repaint();
			}
		}
	}
	
	public void destroy() {
    	if (theRowsTable != null) {
    		theRowsTable.setModel(new DefaultTableModel());
    	}
    	
    	textArea.discardAllEdits();
    	textArea.setDocument(new RSyntaxDocument(null, SyntaxConstants.SYNTAX_STYLE_NONE)); // prevent memory leak
    }
    
    private final RSyntaxTextAreaWithSQLSyntaxStyle textArea;
    final Pair<Integer, Integer> caretDotMark;
    private final List<Integer> rowColumnTypes;

    // Variables declaration - do not modify//GEN-BEGIN:variables
    javax.swing.JButton cancelLoadButton;
    public javax.swing.JComboBox<String> columnSeparatorComboBox;
    public javax.swing.JLabel columnSeparatorLabel;
    public javax.swing.JPanel columnsPanel;
    public javax.swing.JScrollPane columnsScrollPane;
    public javax.swing.JLabel columnsSortedStateLabel;
    public javax.swing.JPanel contentPanel;
    javax.swing.JToolBar controlsContainer;
    public javax.swing.JButton copyCBButton;
    public javax.swing.JPanel fullTSearchPanel;
    public javax.swing.JCheckBox headerCheckBox;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLayeredPane jLayeredPane1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel13;
    private javax.swing.JTextArea jTextArea;
    private javax.swing.JLabel loadingCauseLabel;
    private javax.swing.JLabel loadingLabel;
    javax.swing.JPanel loadingPanel;
    private javax.swing.JPanel metaPanel;
    private javax.swing.JPanel metaTabPanel;
    public javax.swing.JPanel panel;
    public javax.swing.JCheckBox rotateCheckBox;
    public javax.swing.JTabbedPane tabbedPane;
    public javax.swing.JLabel textSortedStateLabel;
    public javax.swing.JPanel textTabPanel;
    public javax.swing.JScrollPane textViewScrollPane;
    private javax.swing.JPanel typePanel;
    // End of variables declaration//GEN-END:variables
    public final javax.swing.JLabel statementLabel;
	public final javax.swing.JPanel shimPanel;
	javax.swing.JButton loadButton;
	protected boolean doSync;
	
	private static ImageIcon cancelIcon;
	
	static {
        // load images
        cancelIcon = UIUtil.readImage("/buttoncancel.png");
	}

}
