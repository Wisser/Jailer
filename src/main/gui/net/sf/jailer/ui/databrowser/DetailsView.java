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

import java.awt.Color;
import java.awt.Font;
import java.awt.GridBagLayout;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.RowSorter;
import javax.swing.ScrollPaneConstants;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingConstants;
import javax.swing.event.AncestorEvent;
import javax.swing.event.AncestorListener;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.table.TableModel;

import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.RowIdSupport;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlUtil;

/**
 * Row Details View.
 * 
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public abstract class DetailsView extends javax.swing.JPanel {

	private final Table table;
	private final List<Row> rows;
	private final RowSorter<? extends TableModel> rowSorter;
	private final RowIdSupport rowIdSupport;
	private final boolean showSpinner;
	
	/** Creates new form DetailsView 
	 * @param rowSorter 
	 * @param showSelectButton 
	*/
	public DetailsView(List<Row> rows, int size, DataModel dataModel, Table table, int rowIndex, RowSorter<? extends TableModel> rowSorter, boolean showSpinner, boolean showSelectButton, RowIdSupport rowIdSupport) {
		this.table = table;
		this.rows = rows;
		this.rowSorter = rowSorter;
		this.rowIdSupport = rowIdSupport;
		this.showSpinner = showSpinner;
		initComponents();
		if (rowSorter != null) {
			rowIndex = rowSorter.convertRowIndexToView(rowIndex);
		}
		final SpinnerNumberModel model = new SpinnerNumberModel(rowIndex + 1, 1, size, -1);
		rowSpinner.setModel(model);
		rowSpinner.addChangeListener(new ChangeListener() {
			@Override
			public void stateChanged(ChangeEvent e) {
				setCurrentRow((Integer) model.getValue() - 1, true);
			}
		});
		KeyListener keyListener = new KeyListener() {
			@Override
			public void keyTyped(KeyEvent e) {
				if (e.getKeyChar() == KeyEvent.VK_ESCAPE) {
					onClose();
				}
			}

			@Override
			public void keyReleased(KeyEvent arg0) {
			}

			@Override
			public void keyPressed(KeyEvent arg0) {
			}
		};
		rowSpinner.addKeyListener(keyListener);
		sortCheckBox.addKeyListener(keyListener);
		closeButton.addKeyListener(keyListener);
		if (!showSelectButton) {
			selectButton.setVisible(false);
		}
		if (!showSpinner) {
			jLabel1.setVisible(false);
			rowSpinner.setVisible(false);
			sortCheckBox.setVisible(false);
			closeButton.setVisible(false);
			selectButton.setVisible(false);
			jScrollPane1.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER);
			jScrollPane1.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
		} else {
			addAncestorListener(new AncestorListener() {
				@Override
				public void ancestorRemoved(AncestorEvent event) {
				}
				@Override
				public void ancestorMoved(AncestorEvent event) {
				}
				@Override
				public void ancestorAdded(AncestorEvent event) {
					closeButton.grabFocus();
					jScrollPane1.getVerticalScrollBar().setValue(jScrollPane1.getVerticalScrollBar().getMinimum());					
					jScrollPane1.getHorizontalScrollBar().setValue(jScrollPane1.getHorizontalScrollBar().getMinimum());
				}
			});
		}
		setCurrentRow(rowIndex, showSpinner);
	}

	private final Font font = new JLabel().getFont();
	private final Font nonbold = new Font(font.getName(), font.getStyle() & ~Font.BOLD, font.getSize()); 
	private final Font italic = new Font(font.getName(), font.getStyle() & ~Font.BOLD | Font.ITALIC, font.getSize()); 
	private final Color BG1 = new Color(255, 255, 255);
	private final Color BG2 = new Color(230, 255, 255);
	private final Color FG1 = new Color(155, 0, 0);
	
	public void setBorderColor(Color color) {
		jScrollPane1.setBorder(BorderFactory.createEtchedBorder(color, Color.GRAY));
	}
	
	private int currentRow;
	private boolean sortColumns;
	private JPanel content;
	
	private void setCurrentRow(int row, boolean selectableFields) {
		currentRow = row;

		java.awt.GridBagConstraints gridBagConstraints;
		
		JPanel oldContent = content;
		content = new JPanel(new GridBagLayout());
		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.weightx = 1;
		gridBagConstraints.weighty = 1;
		gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
		gridBagConstraints.gridx = 0;
		gridBagConstraints.gridy = 0;
		
		int i = 0;
		final List<Column> columns = rowIdSupport.getColumns(table);
		List<Integer> columnIndex = new ArrayList<Integer>();
		for (int j = 0; j < columns.size(); ++j) {
			columnIndex.add(j);
		}
		if (sortColumns) {
			Collections.sort(columnIndex, new Comparator<Integer>() {
				@Override
				public int compare(Integer o1, Integer o2) {
					return Quoting.staticUnquote(columns.get(o1).name).compareTo(Quoting.staticUnquote(columns.get(o2).name));
				}
			});
		}
		while (i < columns.size()) {
			Column c = columns.get(columnIndex.get(i));
			JLabel l = new JLabel();
			l.setText(" " + c.name + "    ");
			l.setFont(nonbold);
			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
			gridBagConstraints.weightx = 0;
			gridBagConstraints.weighty = 0;
			gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
			gridBagConstraints.gridx = 0;
			gridBagConstraints.gridy = i;
			if (!selectableFields) {
				l.setVerticalAlignment(SwingConstants.TOP);
			}
			content.add(l, gridBagConstraints);

			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
			gridBagConstraints.weighty = 0;
			gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
			gridBagConstraints.weightx = 1;
			gridBagConstraints.gridx = 1;
			gridBagConstraints.gridy = i;
			Object v = rows.get(rowSorter != null? rowSorter.convertRowIndexToModel(row) : row).values[columnIndex.get(i)];
			if (v instanceof BigDecimal) {
				v = SqlUtil.toString((BigDecimal) v);
			} else if (v instanceof Double) {
				v = SqlUtil.toString((Double) v);
			}
			if (selectableFields) {
				JTextArea f = new JTextArea();
				if (v instanceof UIUtil.IconWithText) {
					f.setText(((UIUtil.IconWithText) v).text);
				} else {
					f.setText(v == null? "" : v.toString());
				}
				f.setEnabled(v != null);
				f.setEditable(false);
				content.add(f, gridBagConstraints);
			} else {
				JLabel f = new JLabel();
				String text;
				if (v instanceof UIUtil.IconWithText) {
					text = ((UIUtil.IconWithText) v).text + "    ";
					f.setIcon(((UIUtil.IconWithText) v).icon);
				} else {
					text = (v == null? "null" : v.toString()) + "    ";
				}
				f.setText(text.indexOf('\n') >= 0? UIUtil.toHTML(text, 0) : text);
				f.setFont(v == null? italic : nonbold);
				if (v == null) {
					f.setForeground(Color.GRAY);
				}
				content.add(f, gridBagConstraints);
				f.setBackground(i % 2 == 0? BG1 : BG2);
				l.setBackground(i % 2 == 0? BG1 : BG2);
				f.setOpaque(true);
				l.setOpaque(true);
				if (rowIdSupport.getPrimaryKey(table) != null && rowIdSupport.getPrimaryKey(table).getColumns().contains(c)) {
					l.setForeground(FG1);
				} else {
					l.setForeground(Color.BLUE);
				}
			}
			++i;
		}
		if (selectableFields) {
			JLabel l = new JLabel();
			l.setText(" ");
			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
			gridBagConstraints.weightx = 1;
			gridBagConstraints.weighty = 1;
			gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
			gridBagConstraints.gridx = 1;
			gridBagConstraints.gridy = i;
			content.add(l, gridBagConstraints);
		}
		jPanel1.add(content, gridBagConstraints);
		if (oldContent != null) {
			jPanel1.remove(oldContent);
		}
		jPanel1.revalidate();
		jPanel1.repaint();
		onRowChanged(row);
	}

	protected abstract void onRowChanged(int row);
	protected abstract void onClose();
	protected abstract void onSelectRow(Row row);

	/** This method is called from within the constructor to
	 * initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is
	 * always regenerated by the Form Editor.
	 */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jLabel1 = new javax.swing.JLabel();
        rowSpinner = new javax.swing.JSpinner();
        jScrollPane1 = new javax.swing.JScrollPane();
        jPanel1 = new javax.swing.JPanel();
        sortCheckBox = new javax.swing.JCheckBox();
        closeButton = new javax.swing.JButton();
        selectButton = new javax.swing.JButton();

        setLayout(new java.awt.GridBagLayout());

        jLabel1.setText(" Row ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(0, 2, 0, 0);
        add(jLabel1, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        add(rowSpinner, gridBagConstraints);

        jPanel1.setLayout(new java.awt.GridBagLayout());
        jScrollPane1.setViewportView(jPanel1);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(jScrollPane1, gridBagConstraints);

        sortCheckBox.setText("Sort Columns");
        sortCheckBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                sortCheckBoxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 4);
        add(sortCheckBox, gridBagConstraints);

        closeButton.setText("CLose");
        closeButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                closeButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        add(closeButton, gridBagConstraints);

        selectButton.setText("Select Row");
        selectButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                selectButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        add(selectButton, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    private void sortCheckBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_sortCheckBoxActionPerformed
    	sortColumns = !sortColumns;
    	setCurrentRow(currentRow, showSpinner);
    }//GEN-LAST:event_sortCheckBoxActionPerformed

    private void closeButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_closeButtonActionPerformed
        onClose();
    }//GEN-LAST:event_closeButtonActionPerformed

    private void selectButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_selectButtonActionPerformed
        onSelectRow(rows.get(rowSorter != null? rowSorter.convertRowIndexToModel(currentRow) : currentRow));
    }//GEN-LAST:event_selectButtonActionPerformed


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton closeButton;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JSpinner rowSpinner;
    private javax.swing.JButton selectButton;
    private javax.swing.JCheckBox sortCheckBox;
    // End of variables declaration//GEN-END:variables

	public void setSortColumns(boolean selected) {
		sortColumns = selected;
    	setCurrentRow(currentRow, showSpinner);
	}

}
