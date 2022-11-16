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
package net.sf.jailer.ui;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Window;

import javax.swing.ImageIcon;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.JViewport;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import net.sf.jailer.util.LogUtil;

/**
 * Extendet Copy Panel.
 * 
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public class ExtendetCopyPanel extends javax.swing.JPanel {

	public static void openDialog(JTable jTable) {
		Window owner = SwingUtilities.getWindowAncestor(jTable);
		JDialog window = new JDialog(owner, "Extended Copy");
		window.setModal(false);
		
		window.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		ExtendetCopyPanel copyPanel = new ExtendetCopyPanel();
		copyPanel.initContentTable(jTable);
		
		window.getContentPane().add(copyPanel);
		
		window.pack();
		window.setSize(Math.max(window.getWidth(), jTable.getParent().getWidth()), window.getHeight());
		copyPanel.jPanel3.setMinimumSize(new Dimension(1, 160));
		copyPanel.jPanel3.setPreferredSize(copyPanel.jPanel3.getMinimumSize());
		
		Point off = new Point();
		off = SwingUtilities.convertPoint(copyPanel, off, copyPanel.contentTable.getParent());

		Point mid = new Point(off.x + -window.getContentPane().getLocation().x - window.getInsets().left, off.y + -window.getContentPane().getLocation().y - window.getInsets().top); // jTable.getParent().getWidth() / 2 - frame.getWidth() / 2, jTable.getParent().getHeight() / 2 - frame.getHeight() / 2);
		SwingUtilities.convertPointToScreen(mid, jTable.getParent());

		int maxX = owner.getX() + owner.getWidth() - window.getWidth() - 4;
		mid.x = Math.min(mid.x, maxX);
		int maxY = owner.getY() + owner.getHeight() - window.getHeight() - 4;
		mid.y = Math.min(mid.y, maxY);
		mid.x = Math.max(mid.x, owner.getX() + 4);
		mid.y = Math.max(mid.y, owner.getY() + 4);
		
		window.setLocation(mid);
		window.setSize(window.getWidth() + Math.max(0, Math.min(300, maxX - window.getX())), window.getHeight() + Math.max(0, Math.min(500, maxY - window.getY())));

		window.revalidate();
		int h = copyPanel.jScrollPane1.getHeight() - copyPanel.contentTable.getHeight() - 48;
		if (h > 0) {
			h = window.getHeight() - h;
			window.setSize(window.getWidth(), h);
			window.revalidate();
		}
		window.setVisible(true);
	}

    private void initContentTable(JTable jTable) {
    	// TODO
    	// TODO resp. column order: resp. order in Copy2CB too
    	// TODO resp. row order
    	Object[] colNames = new Object[jTable.getColumnCount()];
    	for (int i = 0; i < colNames.length; ++i) {
    		colNames[i] = jTable.getColumnName(i);
    	}
    	Object[][] data = new Object[jTable.getRowCount()][];
    	for (int row = 0; row < data.length; ++row) {
    		data[row] = new Object[colNames.length];
    		int ri = jTable.getRowSorter().convertRowIndexToModel(row);
    		for (int col = 0; col < colNames.length; ++col) {
    			data[row][col] = jTable.getModel().getValueAt(ri, col);
    		}
    	}
		DefaultTableModel dtm = new DefaultTableModel(data, colNames) {
			@Override
			public boolean isCellEditable(int row, int column) {
				return false;
			}
    	};
    	contentTable.setModel(dtm);
    	contentTable.setIntercellSpacing(new Dimension(0, 0));
    	contentTable.setAutoCreateRowSorter(true);
    	contentTable.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
    	
    	for (int i = 0; i < colNames.length; ++i) {
    		contentTable.getColumnModel().getColumn(i).setPreferredWidth(jTable.getColumnModel().getColumn(i).getWidth());
    		colNames[i] = jTable.getColumnName(i);
    	}
    	
    	jScrollPane1.getViewport().setViewPosition(((JViewport) jTable.getParent()).getViewPosition());
    	
    	contentTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    	contentTable.setRowSelectionAllowed(true);
    	contentTable.setColumnSelectionAllowed(true);
    	contentTable.setCellSelectionEnabled(true);
    	contentTable.setName("contentTable");
		contentTable.setDefaultRenderer(Object.class, new TableCellRenderer() {
			final Color BG = new Color(220, 230, 255);
			// TODO
			// TODO check
//			final Color BGSELECTED  = new Color(255, 230, 220);
			@Override
			public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected,
					boolean hasFocus, int row, int column) {
				int dmColumn = column;
//				if (dmColumn  > 0) {
//					--dmColumn;
//				}
				Component render = jTable.getCellRenderer(row, dmColumn).getTableCellRendererComponent(contentTable, value, isSelected, hasFocus, row, dmColumn);
				if (render instanceof JLabel) {
					if (UIUtil.TABLE_BACKGROUND_COLOR_1.equals(render.getBackground())) {
//						render.setBackground(BG);
						// TODO ? idea was: slight diff visually to indicate other dialog. But whot about wysiwyg?
						// TODO vorlaeufig: lass das so
						
						// TODO
						// TODO render selected cells w/o changing text position ("ruhiger machen")
					}
					
//					if (column == 0) {
//						((JLabel) render).setBackground(BGCOLUMNS);
//					}
//					if (column == 0) {
//						String text = ((JLabel) render).getText();
//						String tabName = useTableName? tableName.get(row) : null;
//						if (tabName != null) {
//							JLabel tab = new JLabel("<html>&nbsp;" + tabName + "&nbsp;</html>");
//							tab.setForeground(new Color(0, 0, 180));
//							tab.setBackground(render.getBackground());
//							tab.setOpaque(render.isOpaque());
//							JPanel panel = new JPanel(new GridBagLayout());
//							panel.setToolTipText(text);
//							GridBagConstraints gridBagConstraints;
//							gridBagConstraints = new java.awt.GridBagConstraints();
//					        gridBagConstraints.gridx = 1;
//					        gridBagConstraints.gridy = 1;
//					        gridBagConstraints.fill = GridBagConstraints.BOTH;
//					        gridBagConstraints.weightx = 1;
//					        gridBagConstraints.weighty = 1;
//					        gridBagConstraints.anchor = GridBagConstraints.WEST;
//					        panel.add(tab, gridBagConstraints);
//					        gridBagConstraints = new java.awt.GridBagConstraints();
//					        gridBagConstraints.gridx = 3;
//					        gridBagConstraints.gridy = 1;
//					        gridBagConstraints.anchor = GridBagConstraints.EAST;
//					        gridBagConstraints.fill = GridBagConstraints.BOTH;
//					        gridBagConstraints.weighty = 1;
//					        panel.add(render, gridBagConstraints);
//					        ((JLabel) render).setText(text + "  ");
//							return panel;
//						}
//						((JLabel) render).setText(" " + text);
//					}
					if ("found".equals(render.getName())) {
						Color background = render.getBackground();
						render.setBackground(
								new Color(
										Math.max((int)(background.getRed()), 0),
										Math.max((int)(background.getGreen() * 0.90), 0),
										Math.max((int)(background.getBlue() * 0.91), 0),
										background.getAlpha()));
					}
				}
				return render;
			}
		});
		try {
			((DefaultTableCellRenderer) contentTable.getTableHeader().getDefaultRenderer()).setHorizontalAlignment(JLabel.LEFT);
			for (int i = 0; i < jTable.getColumnModel().getColumnCount(); ++i) {
				TableColumn col = jTable.getColumnModel().getColumn(i);
				contentTable.getColumnModel().getColumn(i).setModelIndex(col.getModelIndex());
			}
			contentTable.getSelectionModel();
			for (int r: jTable.getSelectedRows()) {
				contentTable.addRowSelectionInterval(r, r);
			}
			contentTable.setColumnSelectionInterval(0, contentTable.getColumnCount() - 1);
		} catch (Exception e) {
			LogUtil.warn(e);
		}
		
		// TODO select.change.listener: events zusammenfassen (invokeLater)
		
	}

	private ExtendetCopyPanel() {
        initComponents();
        maximizeButton.setIcon(maximizeIcon);
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jPanel1 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        contentTable = new javax.swing.JTable();
        jLabel1 = new javax.swing.JLabel();
        jToolBar1 = new javax.swing.JToolBar();
        maximizeButton = new javax.swing.JToggleButton();
        jPanel3 = new javax.swing.JPanel();
        jScrollPane2 = new javax.swing.JScrollPane();
        jEditorPane1 = new javax.swing.JEditorPane();
        jPanel2 = new javax.swing.JPanel();
        copyCloseButton = new javax.swing.JButton();

        setLayout(new java.awt.GridBagLayout());

        jPanel1.setLayout(new java.awt.GridBagLayout());

        contentTable.setModel(new javax.swing.table.DefaultTableModel(
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
        jScrollPane1.setViewportView(contentTable);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel1.add(jScrollPane1, gridBagConstraints);

        jLabel1.setFont(jLabel1.getFont().deriveFont(jLabel1.getFont().getSize()-1f));
        jLabel1.setForeground(java.awt.Color.gray);
        jLabel1.setText("<html><i>Shift-Click:</i> Add row/column to selection. <i>Ctrl-Click:</i> Toggle row/column selection.</html>");
        jLabel1.setToolTipText("<html><i>Shift-Click:</i> Add row/column to selection. <br>\n<i>Ctrl-Click:</i> Toggle row/column selection.</html>");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 6, 0, 0);
        jPanel1.add(jLabel1, gridBagConstraints);

        jToolBar1.setFloatable(false);
        jToolBar1.setRollover(true);

        maximizeButton.setText("Maximize");
        maximizeButton.setToolTipText("Maximize size of dialog");
        maximizeButton.setHorizontalTextPosition(javax.swing.SwingConstants.LEFT);
        maximizeButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        maximizeButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                maximizeButtonActionPerformed(evt);
            }
        });
        jToolBar1.add(maximizeButton);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 2;
        jPanel1.add(jToolBar1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(jPanel1, gridBagConstraints);

        jPanel3.setBorder(javax.swing.BorderFactory.createTitledBorder("Preview"));
        jPanel3.setLayout(new java.awt.GridBagLayout());

        jScrollPane2.setViewportView(jEditorPane1);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel3.add(jScrollPane2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        add(jPanel3, gridBagConstraints);

        jPanel2.setLayout(new java.awt.GridBagLayout());

        copyCloseButton.setText("Copy and CLose");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        jPanel2.add(copyCloseButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        add(jPanel2, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    private Point oldLoc;
    private Dimension oldSize;

    private void maximizeButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_maximizeButtonActionPerformed
        Window dialog = SwingUtilities.getWindowAncestor(this);
        if (dialog != null) {
        	Window owner = dialog.getOwner();
        	if (owner != null) {
        		if (oldLoc != null) {
        			dialog.setLocation(oldLoc);
        			dialog.setSize(oldSize);
        			oldLoc = null;
        			oldSize = null;
        	        maximizeButton.setIcon(maximizeIcon);
        		} else {
        			oldLoc = dialog.getLocation();
        			oldSize = dialog.getSize();
 	        		Insets insets = owner.getInsets();
 	        		int b = 32;
 	        		insets.top += b;
 	        		insets.bottom += b;
 	        		insets.left += b;
 	        		insets.right += b;
					dialog.setLocation(owner.getLocation().x + insets.left, owner.getLocation().y + insets.top - 1);
	        		dialog.setSize(owner.getSize().width - insets.left - insets.right + 1, owner.getSize().height - insets.top - insets.bottom);
	                maximizeButton.setIcon(unmaximizeIcon);
        		}
        	}
        }
    }//GEN-LAST:event_maximizeButtonActionPerformed


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JTable contentTable;
    private javax.swing.JButton copyCloseButton;
    private javax.swing.JEditorPane jEditorPane1;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JToolBar jToolBar1;
    private javax.swing.JToggleButton maximizeButton;
    // End of variables declaration//GEN-END:variables
    
	private static ImageIcon maximizeIcon;
	private static ImageIcon unmaximizeIcon;
	static {
		// load images
		maximizeIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/maximizec.png"));
		unmaximizeIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/unmaximize.png"));
	}
	
}
