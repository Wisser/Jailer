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

import org.apache.log4j.Logger;
import org.fife.ui.rsyntaxtextarea.RSyntaxDocument;
import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.fife.ui.rsyntaxtextarea.SyntaxConstants;

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

	private final String SEPARATOR_TAB = "Tab Character";
	private final String SEPARATOR_SPACE = "Space";

	/**
	 * The logger.
	 */
	private static final Logger logger = Logger.getLogger(MetaDataDetailsPanel.class);
	
	/**
     * Creates new form TabContentPanel
     * @param caretDotMark 
     */
    public TabContentPanel(JLabel rowsCount, JComponent metaDataDetails, String type, boolean explain, javax.swing.JPanel shimPanel, Pair<Integer, Integer> caretDotMark, List<Integer> rowColumnTypes) {
    	this.shimPanel = shimPanel == null? new javax.swing.JPanel(new GridBagLayout()) : shimPanel;
    	this.caretDotMark = caretDotMark;
        this.rowColumnTypes = rowColumnTypes;
        initComponents();
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
		
        statementLabel = new JLabel();
        statementLabel.setForeground(new Color(100, 110, 210));
        GridBagConstraints gridBagConstraints;
        
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1;
        gridBagConstraints.anchor = explain? java.awt.GridBagConstraints.WEST : java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new Insets(0, explain? 8 : 16, 0, 0);
		//        panel.add(statementLabel, gridBagConstraints);
        panel.add(new JPanel() {
	        	{
	        		setLayout(null);
	        		setOpaque(false);
	        		addComponentListener(new ComponentListener() {
						@Override
						public void componentShown(ComponentEvent e) {
						}
						@Override
						public void componentResized(ComponentEvent e) {
							Rectangle visibleRect = getBounds();
							String value = statementLabel.getText();
							FontMetrics fontMetrics = getFontMetrics(getFont());
							int x = Math.max(32, visibleRect.width - fontMetrics.stringWidth(value));
							int y = (visibleRect.height - fontMetrics.getHeight() / 2) + fontMetrics.getAscent();
							statementLabel.setBounds(x, 0, visibleRect.width - x, visibleRect.height);
						}
						@Override
						public void componentMoved(ComponentEvent e) {
						}
						@Override
						public void componentHidden(ComponentEvent e) {
						}
					});
		        	add(statementLabel);
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
					if (theRowsTable != null) {
						updateTextView(theRowsTable);
					}
				}
			});
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
			jLabel1.setVisible(false);
			headerCheckBox.setVisible(false);
			copyCBButton.setVisible(false);
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
    
    public void updateTextView(JTable rowsTable) {
    	theRowsTable = rowsTable;
		Object sep = columnSeparatorComboBox.getEditor().getItem();
		if ("".equals(sep)) {
			sep = null;
		} else if (SEPARATOR_SPACE.equals(sep)) {
			sep = " ";
		} else if (SEPARATOR_TAB.equals(sep)) {
			sep = "\t";
		}
    	TableColumnModel cm = rowsTable.getColumnModel();
    	TableModel rDm = rowsTable.getModel();
    	RowSorter<? extends TableModel> sorter = rowsTable.getRowSorter();
    	String[][] cell = new String[sorter.getViewRowCount() + 1][];
    	int[] maxLength = new int[rDm.getColumnCount()];
		for (int y = -1; y < sorter.getViewRowCount(); ++y) {
			cell[y + 1] = new String[rDm.getColumnCount()];
			for (int x = 0; x < rDm.getColumnCount(); ++x) {
				int mx = cm.getColumn(x).getModelIndex();
				Object value;
				if (y < 0) {
					value = rDm.getColumnName(mx);
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
					value = rDm.getValueAt(sorter.convertRowIndexToModel(y), mx);
				}
				if (value instanceof TableModelItem) {
					Object v = ((TableModelItem) value).value;
					if (sep != null || (v == UIUtil.NULL || v == null)) {
						value = v;
					} else {
						value = value.toString().trim();
					}
				}
				String cellContent = value == UIUtil.NULL || value == null? "" : value.toString();
				if (sep != null) {
					if (cellContent.contains(sep.toString()) || cellContent.indexOf("\n") >= 0 || cellContent.indexOf("\r") >= 0 || cellContent.indexOf("\"") >= 0) {
						cellContent = "\"" + (cellContent.replaceAll("\"", "\"\"")) + "\"";
					}
				}
				cell[y + 1][x] = cellContent;
				maxLength[x] = Math.max(maxLineLength(cellContent), maxLength[x]);
			}
		}
		boolean incHeader = headerCheckBox.isSelected();
		StringBuilder sb = new StringBuilder();
		for (int y = 0; y < cell.length; ++y) {
			if (y == 0 && !incHeader) {
				continue;
			}
			for (int x = 0; x < cell[y].length; ++x) {
				if (sep != null) {
					sb.append(cell[y][x]);
					if (x < cell[y].length - 1)
					sb.append(sep);
				} else {
					boolean rightAlign = false;
					synchronized (rowColumnTypes) {
						if (rowColumnTypes.size() > x) {
							switch (rowColumnTypes.get(x)) {
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
					if (x < cell[y].length - 1) {
						sb.append(" ");
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
				}
			}
			sb.append(UIUtil.LINE_SEPARATOR);
			if (y == 0 && sep == null && incHeader) {
				for (int x = 0; x < cell[y].length; ++x) {
					for (int i = 2 + maxLength[x]; i > 0; --i) {
						sb.append("-");
					}
				}
				sb.append(UIUtil.LINE_SEPARATOR);
			}
		}

		textArea.setText(sb.toString());
		textArea.setCaretPosition(0);
		textArea.setEditable(false);
		textArea.discardAllEdits();
		if (UIUtil.plaf == PLAF.FLAT) {
			textArea.setBackground(Color.white);
		}
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
        jLabel1 = new javax.swing.JLabel();
        headerCheckBox = new javax.swing.JCheckBox();
        textSortedStateLabel = new javax.swing.JLabel();
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
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
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
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 2;
        textTabPanel.add(copyCBButton, gridBagConstraints);

        jLabel1.setText("Column Separator ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        textTabPanel.add(jLabel1, gridBagConstraints);

        headerCheckBox.setSelected(true);
        headerCheckBox.setText("Include Header");
        headerCheckBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                headerCheckBoxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        textTabPanel.add(headerCheckBox, gridBagConstraints);

        textSortedStateLabel.setForeground(java.awt.Color.blue);
        textSortedStateLabel.setText("jLabel2");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        textTabPanel.add(textSortedStateLabel, gridBagConstraints);

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
    private javax.swing.JComboBox<String> columnSeparatorComboBox;
    public javax.swing.JPanel columnsPanel;
    public javax.swing.JScrollPane columnsScrollPane;
    public javax.swing.JLabel columnsSortedStateLabel;
    public javax.swing.JPanel contentPanel;
    javax.swing.JToolBar controlsContainer;
    private javax.swing.JButton copyCBButton;
    public javax.swing.JPanel fullTSearchPanel;
    private javax.swing.JCheckBox headerCheckBox;
    private javax.swing.JLabel jLabel1;
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
    public javax.swing.JTabbedPane tabbedPane;
    public javax.swing.JLabel textSortedStateLabel;
    public javax.swing.JPanel textTabPanel;
    public javax.swing.JScrollPane textViewScrollPane;
    private javax.swing.JPanel typePanel;
    // End of variables declaration//GEN-END:variables
    public javax.swing.JLabel statementLabel;
	public final javax.swing.JPanel shimPanel;
	javax.swing.JButton loadButton;
	protected boolean doSync;
	
	private static ImageIcon cancelIcon;
	
	static {
        // load images
        cancelIcon = UIUtil.readImage("/buttoncancel.png");
	}
}
