/*
 * Copyright 2007 - 2017 the original author or authors.
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
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.lang.reflect.Method;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.Types;
import java.util.ArrayList;
import java.util.List;

import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.RowSorter;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;

import org.apache.log4j.Logger;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.database.Session;
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;
import net.sf.jailer.modelbuilder.MetaDataCache.CachedResultSet;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.BrowserContentPane.TableModelItem;
import net.sf.jailer.ui.databrowser.metadata.MetaDataDetailsPanel;

public class TabContentPanel extends javax.swing.JPanel {

	private final String SEPARATOR_TAB = "Tab Character";
	private final String SEPARATOR_SPACE = "Space";

	/**
	 * The logger.
	 */
	private static final Logger logger = Logger.getLogger(MetaDataDetailsPanel.class);
	
    /**
     * Creates new form TabContentPanel
     */
    public TabContentPanel(JLabel rowsCount, JComponent metaDataDetails, boolean explain) {
        initComponents();
        statementLabel = new JLabel() {
        	@Override
        	public Dimension getMinimumSize() {
				return new Dimension(8, super.getMinimumSize().height);
        	}
        };
        statementLabel.setForeground(new Color(0, 0, 180));
        GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new Insets(0, 0, 0, 10);
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        panel.add(rowsCount, gridBagConstraints);
        
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.weightx = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        panel.add(statementLabel, gridBagConstraints);
        
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
		
		if (explain) {
			tabbedPane.remove(contentPanel);
			tabbedPane.remove(columnsPanel);
			tabbedPane.remove(metaTabPanel);
			jLabel1.setVisible(false);
			headerCheckBox.setVisible(false);
			copyCBButton.setVisible(false);
			columnSeparatorComboBox.setVisible(false);
			controlsPanel1.setVisible(false);
			rowsCount.setVisible(false);
		} else {
			if (metaDataDetails != null) {
				metaPanel.add(metaDataDetails);
			}
		}
    }

    public static ResultSet toMetaDataResultSet(ResultSetMetaData metaData, Session session, ExecutionContext executionContext) throws Exception {
        List<Object[]> rowList = new ArrayList<Object[]>();
        
        StringBuilder columnTypes = new StringBuilder();
        for (int i = 0; i < metaData.getColumnCount(); ++i) {
        	if (i > 0) {
        		columnTypes.append(", ");
        	}
        	columnTypes.append(JDBCMetaDataBasedModelElementFinder.toColumn(metaData, i + 1, session));
        }
        // TODO
        System.out.println(columnTypes);
        
        String[] names = new String[] {
        		"getColumnName",
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
        for (int col = 1; col < metaData.getColumnCount(); ++col) {
        	Object[] row = new Object[names.length];
        	i = 0;
        	try {
        		row[i] = metaData.getColumnName(col + 1);
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
        		row[i] = metaData.isCaseSensitive(col + 1);
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

        return new CachedResultSet(rowList, names.length, titel, types);
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
				} else {
					value = rDm.getValueAt(sorter.convertRowIndexToModel(y), mx);
				}
				if (value instanceof TableModelItem) {
					value = ((TableModelItem) value).value;
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
		String nl = System.getProperty("line.separator", "\n");
		StringBuilder sb = new StringBuilder();
		for (int y = 0; y < cell.length; ++y) {
			if (y == 0 && !incHeader) {
				continue;
			}
			for (int x = 0; x < cell[y].length; ++x) {
				sb.append(cell[y][x]);
				if (sep != null) {
					if (x < cell[y].length - 1)
					sb.append(sep);
				} else {
					if (x < cell[y].length - 1) {
						for (int i = 3 + maxLength[x] - lastLineLength(cell[y][x]); i > 0; --i) {
							sb.append(" ");
						}
					}
				}
			}
			sb.append(nl);
			if (y == 0 && sep == null && incHeader) {
				for (int x = 0; x < cell[y].length; ++x) {
					for (int i = (x > 0? 3 : 0) + maxLength[x]; i > 0; --i) {
						sb.append("-");
					}
				}
				sb.append(nl);
			}
		}

		jTextArea.setText(sb.toString());
		jTextArea.setCaretPosition(0);
		jTextArea.setEditable(false);
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
        controlsPanel1 = new javax.swing.JPanel();
        jPanel1 = new javax.swing.JPanel();
        tabbedPane = new javax.swing.JTabbedPane();
        contentPanel = new javax.swing.JPanel();
        columnsPanel = new javax.swing.JPanel();
        columnsScrollPane = new javax.swing.JScrollPane();
        textTabPanel = new javax.swing.JPanel();
        jScrollPane2 = new javax.swing.JScrollPane();
        jTextArea = new javax.swing.JTextArea();
        columnSeparatorComboBox = new javax.swing.JComboBox<>();
        copyCBButton = new javax.swing.JButton();
        jLabel1 = new javax.swing.JLabel();
        headerCheckBox = new javax.swing.JCheckBox();
        metaTabPanel = new javax.swing.JPanel();
        metaPanel = new javax.swing.JPanel();
        jPanel2 = new javax.swing.JPanel();

        setLayout(new java.awt.GridBagLayout());

        panel.setLayout(new java.awt.GridBagLayout());

        controlsPanel1.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 5, 0));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        panel.add(controlsPanel1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridheight = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        add(panel, gridBagConstraints);

        jPanel1.setLayout(new java.awt.BorderLayout());

        tabbedPane.setTabPlacement(javax.swing.JTabbedPane.BOTTOM);

        contentPanel.setFont(new java.awt.Font("Monospaced", 0, 12)); // NOI18N
        contentPanel.setLayout(new java.awt.BorderLayout());
        tabbedPane.addTab("Rows", contentPanel);

        columnsPanel.setLayout(new java.awt.BorderLayout());
        columnsPanel.add(columnsScrollPane, java.awt.BorderLayout.CENTER);

        tabbedPane.addTab("Columns", columnsPanel);

        textTabPanel.setLayout(new java.awt.GridBagLayout());

        jScrollPane2.setEnabled(false);
        jScrollPane2.setFont(new java.awt.Font("Monospaced", 0, 12)); // NOI18N

        jTextArea.setColumns(20);
        jTextArea.setFont(new java.awt.Font("Monospaced", 0, 12)); // NOI18N
        jTextArea.setRows(5);
        jScrollPane2.setViewportView(jTextArea);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        textTabPanel.add(jScrollPane2, gridBagConstraints);

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

        jPanel2.setBorder(javax.swing.BorderFactory.createTitledBorder("Names and types"));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        metaTabPanel.add(jPanel2, gridBagConstraints);

        tabbedPane.addTab("Meta", metaTabPanel);

        jPanel1.add(tabbedPane, java.awt.BorderLayout.CENTER);

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
    	StringSelection selection = new StringSelection(jTextArea.getText());
	    Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
	    clipboard.setContents(selection, selection);
    }//GEN-LAST:event_copyCBButtonActionPerformed

    private void headerCheckBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_headerCheckBoxActionPerformed
    	if (theRowsTable != null) {
			updateTextView(theRowsTable);
		}
    }//GEN-LAST:event_headerCheckBoxActionPerformed

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JComboBox<String> columnSeparatorComboBox;
    public javax.swing.JPanel columnsPanel;
    public javax.swing.JScrollPane columnsScrollPane;
    public javax.swing.JPanel contentPanel;
    public javax.swing.JPanel controlsPanel1;
    private javax.swing.JButton copyCBButton;
    private javax.swing.JCheckBox headerCheckBox;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JTextArea jTextArea;
    private javax.swing.JPanel metaPanel;
    private javax.swing.JPanel metaTabPanel;
    public javax.swing.JPanel panel;
    public javax.swing.JTabbedPane tabbedPane;
    public javax.swing.JPanel textTabPanel;
    // End of variables declaration//GEN-END:variables
    public javax.swing.JLabel statementLabel;
}
