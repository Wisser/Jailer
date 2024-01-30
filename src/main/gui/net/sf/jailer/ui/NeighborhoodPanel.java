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

import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.graphical_view.GraphicalDataModelView;
import net.sf.jailer.ui.util.UISettings;

/**
 * Neighborhood Panel.
 *
 * @author Ralf Wisser
 */
public class NeighborhoodPanel extends javax.swing.JPanel {

    private static final Color WHITE = new Color(255, 255, 255, 200);
    private static final Color SELECTED_COLOR = new Color(0, 255, 255, 150);
    
    private int length = 2;
    private int offset = 0;
    
    private final Font italicFont;

    private final Color depOnColor;
    private final Color hasDepColor;
    private final Color assocWithColor;
    private final Color ignoredColor;
    
    private final MouseWheelListener mouseWheelListener;
    
	/**
     * Creates new form NeighborhoodPanel
     * @param table 
	 * @param graphView 
	 * @param ignoredColor 
	 * @param assocWithColor 
	 * @param hasDepColor 
	 * @param depOnColor 
     */
    public NeighborhoodPanel(final DataModel dataModel, final Table table, final GraphicalDataModelView graphView, final boolean hideIgnored, final Color depOnColor, final Color hasDepColor, final Color assocWithColor, final Color ignoredColor) {
        this.depOnColor = depOnColor;
        this.hasDepColor = hasDepColor;
        this.assocWithColor = assocWithColor;
        this.ignoredColor = ignoredColor;
    	initComponents(); UIUtil.initComponents(this);
    	
    	upLabel.setIcon(upbuttonIcon);
    	downLabel.setIcon(dropDownIcon);
        
    	mouseWheelListener = new MouseWheelListener() {
    		@Override
    		public void mouseWheelMoved(MouseWheelEvent e) {
				int newOffset = Math.max(Math.min(offset + e.getWheelRotation(), sortedTableList.size() - length), 0);
				if (offset != newOffset) {
					offset = newOffset;
					createTableLinks(dataModel, table, graphView, hideIgnored);
				}
    		}
    	};
    	jPanel1.addMouseWheelListener(mouseWheelListener);
    	
        tableLabel.setText(dataModel.getDisplayName(table) + "  ");
        titlePanel.setBackground(WHITE);
        jPanel2.setBackground(WHITE);
        jPanel3.setBackground(WHITE);

        JLabel jLabel = new JLabel("normal");
		Font font = jLabel.getFont();
        Font boldFont = font.deriveFont(font.getStyle() | Font.BOLD, font.getSize());
        italicFont = font.deriveFont(font.getStyle() | Font.ITALIC, font.getSize());
		tableLabel.setFont(boldFont);

		length = graphView.getHeight() / (getFontMetrics(font).getHeight() + 2) - 12;
		if (length < 2) {
			length = 2;
		} else if (length > 20) { 
			length = 20;
		}

		initTableList(dataModel, table, hideIgnored, graphView);
		createTableLinks(dataModel, table, graphView, hideIgnored);
		
		upLabel.addMouseListener(new MouseEventHandler(jPanel2) {
			@Override
			protected void onRightClick(MouseEvent e) {
			}
			@Override
			protected void onLeftClick(MouseEvent e) {
				if (offset > 0) {
					offset = Math.max(offset - length + 1, 0);
					createTableLinks(dataModel, table, graphView, hideIgnored);
				}
			}
		});
		downLabel.addMouseListener(new MouseEventHandler(jPanel3) {
			@Override
			protected void onRightClick(MouseEvent e) {
			}
			@Override
			protected void onLeftClick(MouseEvent e) {
				if (offset < sortedTableList.size() - length) {
					offset = Math.min(offset + length - 1, sortedTableList.size() - length);
					createTableLinks(dataModel, table, graphView, hideIgnored);
				}
			}
		});
    }

	private Map<Table, Color> tabColor;
	private List<Table> sortedTableList;

	private void createTableLinks(final DataModel dataModel, final Table table, final GraphicalDataModelView graphView, final boolean hideIgnored) {
    	if (sortedTableList.isEmpty()) {
    		setVisible(false);
    	} else {
    		jPanel1.removeAll();
    		initUpAndDownButtons(sortedTableList);
    		int y = 0;
    		for (final Table t: sortedTableList) {
    			++y;
    			if (y < offset + 1 || y > length + offset) {
    				continue;
    			}
    			JLabel jLabel = new JLabel("  " + dataModel.getDisplayName(t) + "  ");
    			JPanel jPanel = new JPanel();
    			jLabel.addMouseWheelListener(mouseWheelListener);
    			jLabel.addMouseListener(new MouseEventHandler(graphView.isTableVisible(t)? null : jPanel) {
					@Override
					protected void onRightClick(MouseEvent e) {
						if (t != null) {
							JPopupMenu popup = graphView.createPopupMenu(t, null, true);
							popup.show(e.getComponent(), e.getX(), e.getY());
							popup.addPopupMenuListener(new PopupMenuListener() {
								@Override
								public void popupMenuWillBecomeVisible(PopupMenuEvent e) {
								}
								@Override
								public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
									UIUtil.invokeLater(new Runnable() {
										@Override
										public void run() {
											initTableList(dataModel, table, hideIgnored, graphView);
											createTableLinks(dataModel, table, graphView, hideIgnored);
										}
									});
								}
								@Override
								public void popupMenuCanceled(PopupMenuEvent e) {
								}
							});
						}
					}
					@Override
					protected void onLeftClick(MouseEvent e) {
						if (!graphView.isTableVisible(t)) {
							++UISettings.s9;
							graphView.showTable(table, t);
							initTableList(dataModel, table, hideIgnored, graphView);
							createTableLinks(dataModel, table, graphView, hideIgnored);
						}
						if (e.getClickCount() > 1) {
							graphView.selectTable(t);
						} else {
							graphView.startScrollTimer(t);
						}
					}
				});
    			jPanel.setLayout(new FlowLayout(0, 0, 0));
    			jPanel.add(jLabel);
    			jPanel.setBackground(WHITE);
    			jLabel.setForeground(tabColor.get(t));
    			if (graphView.isTableVisible(t)) {
    				jLabel.setFont(italicFont);
    				jLabel.setForeground(new Color(jLabel.getForeground().getRed(), jLabel.getForeground().getGreen(), jLabel.getForeground().getBlue(), 150));
    			}
    	        GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
    	        gridBagConstraints.gridx = 1;
    	        gridBagConstraints.gridy = y;
    	        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    	        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
    	        jPanel1.add(jPanel, gridBagConstraints);
    		}
    		jPanel1.revalidate();
    		jPanel1.repaint();
    	}
	}

	private void initTableList(final DataModel dataModel, Table table, boolean hideIgnored, GraphicalDataModelView graphView) {
		Set<Table> destinations = new HashSet<Table>();
    	tabColor = new HashMap<Table, Color>();
    	
    	for (Association a: table.associations) {
    		if (!a.destination.equals(table)) {
    			if (!a.isIgnored() || !hideIgnored || graphView.isTableVisible(a.destination)) {
    				destinations.add(a.destination);
    				Color c;
    				if (a.isIgnored()) {
    					c = ignoredColor;
    				} else if (a.isInsertDestinationBeforeSource()) {
    					c = depOnColor;
    				} else if (a.isInsertSourceBeforeDestination()) {
    					c = hasDepColor;
    				} else {
    					c = assocWithColor;
    				}
    				tabColor.put(a.destination, c);
    			}
    		}
    	}
    	sortedTableList = new ArrayList<Table>(destinations);
		Collections.sort(sortedTableList, new Comparator<Table>() {
			@Override
			public int compare(Table o1, Table o2) {
				return dataModel.getDisplayName(o1).compareTo(dataModel.getDisplayName(o2));
			}
		});
	}

	private void initUpAndDownButtons(List<Table> sorted) {
		upLabel.setVisible(offset > 0);
		downLabel.setVisible(sorted.size() > length + offset);
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

        titlePanel = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        tableLabel = new javax.swing.JLabel();
        jSeparator1 = new javax.swing.JSeparator();
        jPanel3 = new javax.swing.JPanel();
        jLabel3 = new javax.swing.JLabel();
        downLabel = new javax.swing.JLabel();
        jPanel1 = new javax.swing.JPanel();
        jPanel2 = new javax.swing.JPanel();
        jLabel2 = new javax.swing.JLabel();
        upLabel = new javax.swing.JLabel();
        jPanel4 = new javax.swing.JPanel();

        setOpaque(false);
        setLayout(new java.awt.GridBagLayout());

        titlePanel.setLayout(new java.awt.GridBagLayout());

        jLabel1.setText("Neighborhood of  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        titlePanel.add(jLabel1, gridBagConstraints);

        tableLabel.setText("TABLE");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        titlePanel.add(tableLabel, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        titlePanel.add(jSeparator1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 8);
        add(titlePanel, gridBagConstraints);

        jPanel3.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 0, 0));

        jLabel3.setText(" ");
        jPanel3.add(jLabel3);

        downLabel.setIcon(new javax.swing.ImageIcon(getClass().getResource("/net/sf/jailer/ui/resource/dropdown.png"))); // NOI18N
        downLabel.setText("more  ");
        jPanel3.add(downLabel);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 8);
        add(jPanel3, gridBagConstraints);

        jPanel1.setOpaque(false);
        jPanel1.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 8);
        add(jPanel1, gridBagConstraints);

        jPanel2.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 0, 0));

        jLabel2.setText(" ");
        jPanel2.add(jLabel2);

        upLabel.setIcon(new javax.swing.ImageIcon(getClass().getResource("/net/sf/jailer/ui/resource/upbutton.png"))); // NOI18N
        upLabel.setText("more  ");
        jPanel2.add(upLabel);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 8);
        add(jPanel2, gridBagConstraints);

        javax.swing.GroupLayout jPanel4Layout = new javax.swing.GroupLayout(jPanel4);
        jPanel4.setLayout(jPanel4Layout);
        jPanel4Layout.setHorizontalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 0, Short.MAX_VALUE)
        );
        jPanel4Layout.setVerticalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 0, Short.MAX_VALUE)
        );

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 0);
        add(jPanel4, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JLabel downLabel;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JLabel tableLabel;
    private javax.swing.JPanel titlePanel;
    private javax.swing.JLabel upLabel;
    // End of variables declaration//GEN-END:variables
    
    private abstract class MouseEventHandler implements MouseListener {

    	private final JPanel background;
    	
    	public MouseEventHandler(JPanel background) {
    		this.background = background;
    	}
    	
		@Override
		public void mouseClicked(MouseEvent e) {
			if (e.getButton() == MouseEvent.BUTTON1) {
				onLeftClick(e);
			} else if (e.getButton() == MouseEvent.BUTTON3) {
				onRightClick(e);
			}
		}

		@Override
		public void mousePressed(MouseEvent e) {
		}

		@Override
		public void mouseReleased(MouseEvent e) {
		}

		@Override
		public void mouseEntered(MouseEvent e) {
			if (background != null) {
				background.setBackground(SELECTED_COLOR);
			}
		}

		@Override
		public void mouseExited(MouseEvent e) {
			if (background != null) {
				background.setBackground(WHITE);
			}
		}
    	
		protected abstract void onLeftClick(MouseEvent e);
		
		protected abstract void onRightClick(MouseEvent e);
		
    }
    
    private Icon dropDownIcon;
	private ImageIcon upbuttonIcon;
	{
		// load images
		dropDownIcon = UIUtil.readImage("/dropdown.png");
		upbuttonIcon = UIUtil.readImage("/upbutton.png");
	}
	
}
