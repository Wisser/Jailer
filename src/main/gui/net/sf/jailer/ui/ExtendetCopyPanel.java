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

import java.awt.BasicStroke;
import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Window;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.JViewport;
import javax.swing.ListSelectionModel;
import javax.swing.RowSorter;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.WindowConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableColumnModelEvent;
import javax.swing.event.TableColumnModelListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableModel;

import net.sf.jailer.ui.databrowser.sqlconsole.TabContentPanel;
import net.sf.jailer.util.LogUtil;
import net.sf.jailer.util.Pair;

/**
 * Extendet Copy Panel.
 * 
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public class ExtendetCopyPanel extends javax.swing.JPanel {

	public static void openDialog(JTable jTable, boolean allColumnsSelected, String tableName, List<Integer> rowColumnTypes) {
		Window owner = SwingUtilities.getWindowAncestor(jTable);
		JDialog window = new JDialog(owner, "Extended Copy");
		window.setModal(false);
		
		window.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		ExtendetCopyPanel copyPanel = new ExtendetCopyPanel();
		copyPanel.initContentTable(jTable, allColumnsSelected);
		
		window.getContentPane().add(copyPanel);
		
		window.pack();
		window.setSize(Math.max(window.getWidth(), jTable.getParent().getWidth()), window.getHeight());
		copyPanel.previewPanel.setMinimumSize(new Dimension(1, 160));
		copyPanel.previewPanel.setPreferredSize(copyPanel.previewPanel.getMinimumSize());
		copyPanel.tabContentPanel = new TabContentPanel(new JLabel(""), new JLabel(""), tableName, 
				false,
				null,
				null,
				rowColumnTypes,
				allColumnsSelected);
		copyPanel.plainPanel.add(copyPanel.tabContentPanel.textTabPanel, java.awt.BorderLayout.CENTER);
		copyPanel.tabContentPanel.copyCBButton.setVisible(false);
		copyPanel.controlsPanel.add(copyPanel.tabContentPanel.headerCheckBox);
		copyPanel.controlsPanel.add(copyPanel.tabContentPanel.rotateCheckBox);
		copyPanel.tabContentPanel.textSortedStateLabel.setVisible(false);
		
		Point off = new Point();
		off = SwingUtilities.convertPoint(copyPanel, off, copyPanel.contentTable.getParent());

		Point mid = new Point(off.x + -window.getContentPane().getLocation().x - window.getInsets().left, off.y + -window.getContentPane().getLocation().y - window.getInsets().top); // jTable.getParent().getWidth() / 2 - frame.getWidth() / 2, jTable.getParent().getHeight() / 2 - frame.getHeight() / 2);
		SwingUtilities.convertPointToScreen(mid, jTable.getParent());

		shrink(window, copyPanel);

		int maxX = owner.getX() + owner.getWidth() - window.getWidth() - 4;
		mid.x = Math.min(mid.x, maxX);
		int maxY = owner.getY() + owner.getHeight() - window.getHeight() - 4;
		mid.y = Math.min(mid.y, maxY);
		mid.x = Math.max(mid.x, owner.getX() + 4);
		mid.y = Math.max(mid.y, owner.getY() + 4);
		
		window.setLocation(mid);
		window.setSize(window.getWidth() + Math.max(0, Math.min(300, maxX - window.getX())), window.getHeight() + Math.max(0, Math.min(500, maxY - window.getY())));

		shrink(window, copyPanel);
		
		copyPanel.updatePreview();
		Timer timer = new Timer(100, e -> {
			copyPanel.contentTable.repaint();
		});
		timer.setInitialDelay(100);
		timer.setRepeats(true);
		timer.start();
		window.addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosed(WindowEvent e) {
				timer.stop();
			}
		});
		window.setVisible(true);
	}

	private static void shrink(JDialog window, ExtendetCopyPanel copyPanel) {
		window.revalidate();
		int h = copyPanel.jScrollPane1.getHeight() - copyPanel.contentTable.getHeight() - 48;
		if (h > 0) {
			h = window.getHeight() - h;
			window.setSize(window.getWidth(), Math.max(h, 364));
			window.revalidate();
		}
	}

    private void initContentTable(JTable jTable, boolean allColumnsSelected) {

    	// TODO column-table in console: Ext.CopyPanel for rowsTable and col.Table for initial position + check "rotate" checkbox initially

    	// TODO
    	// TODO formatted (html): checkboxes: +- background-colors and +-alignment (left/right)
    	
    	// TODO silent mode for normal copy
    	
    	recreateContentTable();
    	
    	Object[] colNames = new Object[jTable.getColumnCount()];
    	for (int i = 0; i < colNames.length; ++i) {
    		colNames[i] = jTable.getColumnName(i);
    	}
    	RowSorter<? extends TableModel> rowSorter = jTable.getRowSorter();
    	Object[][] data = new Object[jTable.getRowCount()][];
    	for (int row = 0; row < data.length; ++row) {
    		data[row] = new Object[colNames.length];
    		int ri = rowSorter == null? row : rowSorter.convertRowIndexToModel(row);
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
			@Override
			public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected,
					boolean hasFocus, int row, int column) {
				int dmColumn = column;
				Component render = jTable.getCellRenderer(row, dmColumn).getTableCellRendererComponent(contentTable, value, false, hasFocus, row, dmColumn);
				if (render instanceof JLabel) {
					if (isSelected) {
						if (UIUtil.TABLE_BACKGROUND_COLOR_1.equals(render.getBackground())) {
							render.setBackground(UIUtil.TABLE_BG1SELECTED);
						} else {
							render.setBackground(UIUtil.TABLE_BG2SELECTED);
						}
					}
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
			if (allColumnsSelected) {
				contentTable.setColumnSelectionInterval(0, contentTable.getColumnCount() - 1);
			} else {
				for (int ci: jTable.getSelectedColumns()) {
					contentTable.addColumnSelectionInterval(ci, ci);
				}
			}
		} catch (Exception e) {
			LogUtil.warn(e);
		}
		
		contentTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			@Override
			public void valueChanged(ListSelectionEvent e) {
				updatePreview();
			}
		});
		contentTable.getColumnModel().addColumnModelListener(new TableColumnModelListener() {
			@Override
			public void columnSelectionChanged(ListSelectionEvent e) {
				updatePreview();
			}
			@Override
			public void columnRemoved(TableColumnModelEvent e) {
				updatePreview();
			}
			@Override
			public void columnMoved(TableColumnModelEvent e) {
				updatePreview();
			}
			@Override
			public void columnMarginChanged(ChangeEvent e) {
				updatePreview();
			}
			@Override
			public void columnAdded(TableColumnModelEvent e) {
				updatePreview();
			}
		});
	}

	private void recreateContentTable() {
		contentTable = new JTable() {
			@Override
			public void paint(Graphics graphics) {
				super.paint(graphics);
				if (!(graphics instanceof Graphics2D)) {
					return;
				}
				Rectangle visRect = getVisibleRect();

				Graphics2D g2d = (Graphics2D) graphics;
				g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
				g2d.setStroke(new BasicStroke(1));

				int left = -1;
				int last = -1;
				List<Pair<Integer, Integer>> intervall = new ArrayList<Pair<Integer, Integer>>();
				for (int si : getSelectedColumns()) {
					if (left < 0) {
						left = si;
					} else if (last < si - 1) {
						intervall.add(new Pair<Integer, Integer>(left, last));
						left = si;
					}
					last = si;
				}
				if (left >= 0) {
					intervall.add(new Pair<Integer, Integer>(left, last));
				}

				int top = -1;
				last = -1;
				List<Pair<Integer, Integer>> rowIntervall = new ArrayList<Pair<Integer, Integer>>();
				for (int si : getSelectedRows()) {
					if (top < 0) {
						top = si;
					} else if (last < si - 1) {
						rowIntervall.add(new Pair<Integer, Integer>(top, last));
						top = si;
					}
					last = si;
				}
				if (top >= 0) {
					rowIntervall.add(new Pair<Integer, Integer>(top, last));
				}
				int x[] = new int[2];
				int y[] = new int[2];
				for (Pair<Integer, Integer> iv : intervall) {
					for (Pair<Integer, Integer> rowIv : rowIntervall) {
						int[] selectedRows = getSelectedRows();
						if (selectedRows.length > 0) {
							x[0] = Integer.MAX_VALUE;
							y[0] = Integer.MAX_VALUE;
							x[1] = Integer.MIN_VALUE;
							y[1] = Integer.MIN_VALUE;
							Rectangle r = getCellRect(rowIv.a, iv.a, false);
							x[0] = Math.min((int) r.getMinX(), x[0]);
							y[0] = Math.min((int) r.getMinY(), y[0]);
							r = getCellRect(rowIv.b, iv.b, false);
							x[1] = Math.max((int) r.getMaxX(), x[1]);
							y[1] = Math.max((int) r.getMaxY(), y[1]);
							x[0] = (int) Math.max(visRect.getMinX(), x[0]) + 1;
							y[0] = (int) Math.max(visRect.getMinY(), y[0]);
							x[1] = (int) Math.min(visRect.getMaxX(), x[1]) - 2;
							y[1] = (int) Math.min(visRect.getMaxY() - 1, y[1]);
							if (x[0] < x[1] && y[0] < y[1]) {
								g2d.setColor(UIUtil.BG_FLATMOUSEOVER);
								BasicStroke stroke = new BasicStroke();
								g2d.setStroke(stroke);
								g2d.drawRoundRect(x[0], y[0], x[1] - x[0], y[1] - y[0], 8, 8);
								g2d.setColor(new Color(0, 0, 200, 100));
								g2d.setStroke(new BasicStroke(stroke.getLineWidth(), stroke.getEndCap(), stroke.getLineJoin(),
										stroke.getMiterLimit(), new float[] { 11f, 5f },
										(float) ((System.currentTimeMillis() / 50.0 * 1.1) % 16)));
								g2d.drawRoundRect(x[0], y[0], x[1] - x[0], y[1] - y[0], 8, 8);
							}
						}
					}
				}
			}
		};
		jScrollPane1.setViewportView(contentTable);
	}

	private void updatePreview() {
		if (!updatePending) {
			UIUtil.invokeLater(() -> {
				updatePending = false;
				if (!formattedCheckBox.isSelected()) {
					tabContentPanel.updateTextView(contentTable);
				}
				((CardLayout) previewPanel.getLayout()).show(previewPanel, formattedCheckBox.isSelected()? "formatted" : "plain");
			});
		}
	}

	private boolean updatePending = false;
	
    private TabContentPanel tabContentPanel;
    
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

        jSplitPane1 = new javax.swing.JSplitPane();
        jPanel1 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        contentTable = new javax.swing.JTable();
        jLabel1 = new javax.swing.JLabel();
        jToolBar1 = new javax.swing.JToolBar();
        maximizeButton = new javax.swing.JToggleButton();
        jPanel2 = new javax.swing.JPanel();
        previewPanel = new javax.swing.JPanel();
        plainPanel = new javax.swing.JPanel();
        formattedScrollPane = new javax.swing.JScrollPane();
        jEditorPane1 = new javax.swing.JEditorPane();
        panel = new javax.swing.JPanel();
        controlsPanel = new javax.swing.JPanel();
        formattedCheckBox = new javax.swing.JCheckBox();
        copyCloseButton = new javax.swing.JButton();

        setLayout(new java.awt.GridBagLayout());

        jSplitPane1.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        jSplitPane1.setResizeWeight(1.0);
        jSplitPane1.setOneTouchExpandable(true);

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
        jLabel1.setText("<html><i>Ctrl-Click:</i> Toggle row/column selection.<i> Ctrl-A:</i> Select All.</html>");
        jLabel1.setToolTipText("<html><i>Ctrl-Click: </i>Toggle row/column selection.<br>\n<i>Ctrl-A:</i> Select All</html>");
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

        jSplitPane1.setLeftComponent(jPanel1);

        jPanel2.setLayout(new java.awt.GridBagLayout());

        previewPanel.setBorder(javax.swing.BorderFactory.createTitledBorder("Preview"));
        previewPanel.setLayout(new java.awt.CardLayout());

        plainPanel.setLayout(new java.awt.BorderLayout());
        previewPanel.add(plainPanel, "plain");

        formattedScrollPane.setViewportView(jEditorPane1);

        previewPanel.add(formattedScrollPane, "formatted");

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel2.add(previewPanel, gridBagConstraints);

        jSplitPane1.setRightComponent(jPanel2);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(jSplitPane1, gridBagConstraints);

        panel.setLayout(new java.awt.GridBagLayout());

        controlsPanel.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 5, 0));

        formattedCheckBox.setSelected(true);
        formattedCheckBox.setText("Formatted");
        formattedCheckBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                formattedCheckBoxActionPerformed(evt);
            }
        });
        controlsPanel.add(formattedCheckBox);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        panel.add(controlsPanel, gridBagConstraints);

        copyCloseButton.setText("Copy and CLose");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 2);
        panel.add(copyCloseButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        add(panel, gridBagConstraints);
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

    private void formattedCheckBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_formattedCheckBoxActionPerformed
        updatePreview();
    }//GEN-LAST:event_formattedCheckBoxActionPerformed


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JTable contentTable;
    private javax.swing.JPanel controlsPanel;
    private javax.swing.JButton copyCloseButton;
    private javax.swing.JCheckBox formattedCheckBox;
    private javax.swing.JScrollPane formattedScrollPane;
    private javax.swing.JEditorPane jEditorPane1;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JSplitPane jSplitPane1;
    private javax.swing.JToolBar jToolBar1;
    private javax.swing.JToggleButton maximizeButton;
    private javax.swing.JPanel panel;
    private javax.swing.JPanel plainPanel;
    private javax.swing.JPanel previewPanel;
    // End of variables declaration//GEN-END:variables
    
	private static ImageIcon maximizeIcon;
	private static ImageIcon unmaximizeIcon;
	static {
		// load images
		maximizeIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/maximizec.png"));
		unmaximizeIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/unmaximize.png"));
	}
	
}
