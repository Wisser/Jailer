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
package net.sf.jailer.ui;

import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Font;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.LinkedBlockingQueue;

import javax.swing.DefaultCellEditor;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.RowSorter.SortKey;
import javax.swing.SortOrder;
import javax.swing.SwingConstants;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import org.fife.rsta.ui.EscapableDialog;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.Quoting;

/**
 * Analyzes SQL statements and proposes association definitions. <br>
 * This allows to reverse-engineer the data model based on existing SQL queries. <br><br>
 *
 * @author Ralf Wisser
 */
public class ColumnOrderEditor extends javax.swing.JPanel {

	private DefaultTableModel columnOrderModel;
	private final DataModel dataModel;
	private JDialog dialog;
	private final ExecutionContext executionContext;
	private Map<String, DataModel.ColumnOrderPriority> columnOrderPrio;

	private static final String TYPE_PK = "PK";
	private static final String TYPE_FK = "FK";
	private static final String TYPE_FK_PK = "FK+PK";

    /**
     * Creates new form ConstraintChecker
     * @param scriptFile file to load, if not <code>null</code>
     */
    public ColumnOrderEditor(JFrame owner, DataModel dataModel, ExecutionContext executionContext) {
        this.dataModel = dataModel;
    	this.executionContext = executionContext;
    	
    	initComponents();
        
		Font infoFont = jinfoLabe.getFont();
		infoFont = new Font(infoFont.getName(), infoFont.getStyle(), (int) (infoFont.getSize() * 1.2));
		jinfoLabe.setFont(infoFont);
	
		columnOrderModel = new DefaultTableModel(new String[] { "Column", "Frequency", "Type", "Left/Top", "Right/Bottom" }, 0) {
			@Override
			public boolean isCellEditable(int row, int column) {
				return column == 3 || column == 4;
			}
			
			@Override
			public void setValueAt(Object aValue, int row, int column) {
				if (Boolean.TRUE.equals(aValue)) {
					if (column == 3) {
						super.setValueAt(Boolean.FALSE, row, 4);
					}
					if (column == 4) {
						super.setValueAt(Boolean.FALSE, row, 3);
					}
				}
				super.setValueAt(aValue, row, column);
				if (column == 3) {
					if (Boolean.TRUE.equals(aValue)) {
						columnOrderPrio.put((String) getValueAt(row, 0), DataModel.ColumnOrderPriority.HI);
					} else {
						columnOrderPrio.remove(getValueAt(row, 0));
					}
				}
				if (column == 4) {
					if (Boolean.TRUE.equals(aValue)) {
						columnOrderPrio.put((String) getValueAt(row, 0), DataModel.ColumnOrderPriority.LO);
					} else {
						columnOrderPrio.remove(getValueAt(row, 0));
					}
				}
			}
			
			@Override
            public Class<?> getColumnClass(int columnIndex) {
                if(columnIndex == 3 || columnIndex == 4){
                    return Boolean.class;
                }
                if (columnIndex == 1) {
                	return Integer.class;
                }
                return super.getColumnClass(columnIndex);
            }
		};
		final JCheckBox checkBox = new JCheckBox("  ");
		checkBox.setHorizontalAlignment(SwingConstants.RIGHT);
		DefaultCellEditor anEditor = new DefaultCellEditor(checkBox);
		anEditor.setClickCountToStart(1);
		columnOrderTable.setDefaultEditor(Boolean.class, anEditor);
		
		columnOrderTable.setModel(columnOrderModel);
		columnOrderTable.setRowSelectionAllowed(false);
		
		final TableCellRenderer defaultTableCellRenderer = columnOrderTable.getDefaultRenderer(String.class);
		TableCellRenderer renderer = new TableCellRenderer() {
			final Color BG1 = new Color(255, 255, 255);
			final Color BG2 = new Color(242, 255, 242);

			@Override
			public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
				Component render = defaultTableCellRenderer.getTableCellRendererComponent(table, value, isSelected, false, row, column);
				if (value instanceof Boolean) {
					JCheckBox checkBox = new JCheckBox("  ");
					checkBox.setHorizontalAlignment(SwingConstants.RIGHT);
					checkBox.setSelected(Boolean.TRUE.equals(value));
					render = checkBox;
				}
				if (!isSelected) {
					render.setBackground((row % 2 == 0) ? BG1 : BG2);
				}
				if (render instanceof JLabel) {
					if (column == 2) {
						if (TYPE_FK.equals(value)) {
							((JLabel) render).setToolTipText("Foreign Key");
						} else if (TYPE_PK.equals(value)) {
							((JLabel) render).setToolTipText("Primary Key");
						} else if (TYPE_FK_PK.equals(value)) {
							((JLabel) render).setToolTipText("Primary Key + Foreign Key");
						} else {
							((JLabel) render).setToolTipText(null);
						}
					} else {
						String valueAsString = String.valueOf(value);
						if (valueAsString.length() > 0) {
							((JLabel) render).setToolTipText(UIUtil.toHTML(valueAsString, 200));
						} else {
							((JLabel) render).setToolTipText(null);
						}
					}
				}
				return render;
			}
		};
		
		try {
			UIUtil.setWaitCursor(owner);
	        
			Set<String> pks = new HashSet<String>();
			Set<String> fks = new HashSet<String>();
			
			Map<String, Integer> columnsCount = new TreeMap<String, Integer>();
			for (Table table: dataModel.getTables()) {
				for (Column column: table.getColumns()) {
					String name = Quoting.normalizeIdentifier(column.name);
					Integer i = columnsCount.get(name);
					if (i == null) {
						columnsCount.put(name, 1);
					} else {
						columnsCount.put(name, i + 1);
					}
				}
				if (table.primaryKey != null && table.primaryKey.getColumns() != null) {
					for (Column column: table.primaryKey.getColumns()) {
						pks.add(Quoting.normalizeIdentifier(column.name));
					}
				}
				for (Association a: table.associations) {
					if (a.isInsertDestinationBeforeSource()) {
						Map<Column, Column> mapping = a.createSourceToDestinationKeyMapping();
						if (mapping != null) {
							for (Column column: mapping.keySet()) {
								fks.add(Quoting.normalizeIdentifier(column.name));
							}
						}
					}
				}
			}
			for (Entry<String, Integer> e: columnsCount.entrySet()) {
				String type = "";
				if (fks.contains(e.getKey())) {
					if (pks.contains(e.getKey())) {
						type = TYPE_FK_PK;
					} else {
						type = TYPE_FK;
					}
				} else if (pks.contains(e.getKey())) {
					type = TYPE_PK;
				}
				columnOrderModel.addRow(new Object[] { e.getKey(), e.getValue(), type, DataModel.ColumnOrderPriority.HI == dataModel.columnOrderPrio.get(e.getKey()), DataModel.ColumnOrderPriority.LO == dataModel.columnOrderPrio.get(e.getKey()) });
			}
			
			columnOrderPrio = new TreeMap<String, DataModel.ColumnOrderPriority>(dataModel.columnOrderPrio);
			
			columnOrderTable.getColumnModel().getColumn(3).setCellRenderer(renderer);
			columnOrderTable.getColumnModel().getColumn(4).setCellRenderer(renderer);
			columnOrderTable.setDefaultRenderer(Object.class, renderer);
			columnOrderTable.setAutoCreateRowSorter(true);
			List<SortKey> keys = new ArrayList<SortKey>();
			keys.add(new SortKey(1, SortOrder.DESCENDING));
			columnOrderTable.getRowSorter().setSortKeys(keys);
			adjustTableColumnsWidth();
		} finally {
			UIUtil.resetWaitCursor(owner);
		}
        dialog = new EscapableDialog(owner, "Column Ordering") {
        };
        dialog.setModal(true);
		dialog.getContentPane().add(this);
		dialog.pack();
		dialog.setSize(550, 600);
		dialog.setLocation(owner.getX() + (owner.getWidth() - dialog.getWidth()) / 2, Math.max(0, owner.getY() + (owner.getHeight() - dialog.getHeight()) / 2));
		UIUtil.fit(dialog);

		dialog.setVisible(true);
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

        jPanel1 = new javax.swing.JPanel();
        jPanel2 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        columnOrderTable = new javax.swing.JTable();
        closeButton = new javax.swing.JButton();
        jinfoLabe = new javax.swing.JLabel();
        jSeparator1 = new javax.swing.JSeparator();
        okButton = new javax.swing.JButton();

        setLayout(new java.awt.GridBagLayout());

        jPanel1.setLayout(new java.awt.GridBagLayout());

        jPanel2.setLayout(new java.awt.GridBagLayout());

        columnOrderTable.setModel(new javax.swing.table.DefaultTableModel(
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
        jScrollPane1.setViewportView(columnOrderTable);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel2.add(jScrollPane1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 2.0;
        gridBagConstraints.weighty = 1.0;
        jPanel1.add(jPanel2, gridBagConstraints);

        closeButton.setText("Cancel");
        closeButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                closeButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 100;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHEAST;
        jPanel1.add(closeButton, gridBagConstraints);

        jinfoLabe.setText("Define the rendering position of columns.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        jPanel1.add(jinfoLabe, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 8, 0);
        jPanel1.add(jSeparator1, gridBagConstraints);

        okButton.setText(" Ok ");
        okButton.setFocusCycleRoot(true);
        okButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                okButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 100;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(okButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
        add(jPanel1, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    private void closeButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_closeButtonActionPerformed
        dialog.dispose();
    }//GEN-LAST:event_closeButtonActionPerformed

    private void okButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_okButtonActionPerformed
    	try {
    		dataModel.columnOrderPrio.clear();
    		dataModel.columnOrderPrio.putAll(columnOrderPrio);
			dataModel.saveColumnOrderPrio();
	    	dialog.dispose();
		} catch (FileNotFoundException e) {
			UIUtil.showException(this, "Error", e);
		}
    }//GEN-LAST:event_okButtonActionPerformed

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton closeButton;
    private javax.swing.JTable columnOrderTable;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JLabel jinfoLabe;
    private javax.swing.JButton okButton;
    // End of variables declaration//GEN-END:variables

	static final LinkedBlockingQueue<Runnable> queue = new LinkedBlockingQueue<Runnable>();
    static {
    	Thread thread = new Thread(new Runnable() {
			@Override
			public void run() {
				for (;;) {
					try {
						queue.take().run();
					} catch (Throwable t) {
					}
				}
			}
		});
        thread.setDaemon(true);
        thread.start();
    }

	public void adjustTableColumnsWidth() {
		adjustTableColumnsWidth(columnOrderTable, false);
	}

	public void adjustTableColumnsWidth(JTable table, boolean fixFirstColumn) {
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
			if (i == 3 || i == 4) {
				column.setWidth(column.getPreferredWidth());
			}
		}
	}

}

