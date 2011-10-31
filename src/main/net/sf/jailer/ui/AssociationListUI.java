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
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.RenderingHints;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.SqlUtil;

/**
 * Lets user apply an action on a subset of a given {@link Association} set.
 *
 * @author Ralf Wisser
 */
public class AssociationListUI extends javax.swing.JPanel {

	/**
	 * The model.
	 */
	private List<Association> model = new ArrayList<Association>();
	
	/**
	 * Selected associations.
	 */
	private Set<Association> selection = new HashSet<Association>();
	
	/** Creates new form AssociationListUI */
    public AssociationListUI() {
        initComponents();
        groupByComboBox.setModel(new DefaultComboBoxModel(GroupByDefinition.values()));
        groupByComboBox.setSelectedIndex(0);
        groupByComboBox.addItemListener(new ItemListener() {
			@Override
			public void itemStateChanged(ItemEvent e) {
				updateModel();
				revalidate();
			}
		});
    }

    private Double pixelPerTableNameChar = null;
    
    /**
     * Sets the model.
     * 
     * @param model the model
     */
    public void setModel(Collection<Association> model) {
    	this.model = new ArrayList<Association>(model);
    	selection.retainAll(model);
    	if (pixelPerTableNameChar == null) {
	    	Iterator<Association> firstAssociationI = model.iterator();
	    	if (firstAssociationI.hasNext()) {
	    		Association firstAssociation = firstAssociationI.next();
	    		double size = 0.0;
	    		int l = 0;
				for (Table t: firstAssociation.getDataModel().getTables()) {
	    			String tName = firstAssociation.getDataModel().getDisplayName(t);
	    			size += new JLabel(tName).getMinimumSize().width;
	    			l += tName.length();
				}
				pixelPerTableNameChar = 8.0;
				if (l > 0) {
					pixelPerTableNameChar = size / l;
				}
				if (pixelPerTableNameChar < 5.0) {
					pixelPerTableNameChar = 5.0;
				}
	    	}
    	}
    	updateModel();
    }
    
    private interface ColumnContentGetter {
    	abstract String getContent(Association association);
		abstract String getDisplayName();
    };
    
    private static class TypeGetter implements ColumnContentGetter {
    	public String getContent(Association association) {
    		if (association.isInsertDestinationBeforeSource()) {
    			return "Parent";
    		}
    		return "Child";
    	}
		public String getDisplayName() {
			return "Type";
		}
    };
    
    private static class SourceGetter implements ColumnContentGetter {
    	public String getContent(Association association) {
    		return association.getDataModel().getDisplayName(association.source);
    	}
		public String getDisplayName() {
			return "Source";
		}
    };

    private static class DestinationGetter implements ColumnContentGetter {
    	public String getContent(Association association) {
    		return association.getDataModel().getDisplayName(association.destination);
    	}
		public String getDisplayName() {
			return "Destination";
		}
    };

    private static class AssociationNameGetter implements ColumnContentGetter {
    	public String getContent(Association association) {
    		return association.getName();
    	}
		public String getDisplayName() {
			return "Association";
		}
    };

    private class Node {
    	public final Collection<Association> associations;
    	public final ColumnContentGetter columnContentGetter;
    	public final List<Node> children = new ArrayList<AssociationListUI.Node>();
		public final String group;
    	
    	public Node(String group, Collection<Association> associations, ColumnContentGetter columnContentGetter) {
    		this.group = group;
    		this.associations = associations;
    		this.columnContentGetter = columnContentGetter;
    	}

		public boolean isLinear() {
			return children.size() == 0 || children.size() == 1 && children.get(0).isLinear();
		}

		public boolean isSelected() {
			return selection.containsAll(associations);
		}
    }

    private static ColumnContentGetter TYPE_CG = new TypeGetter();
    private static ColumnContentGetter SOURCE_CG = new SourceGetter();
    private static ColumnContentGetter DESTINATION_CG = new DestinationGetter();
    private static ColumnContentGetter ASSOCIATION_NAME_CG = new AssociationNameGetter();
    
    private enum GroupByDefinition {
    	TSD("Type, Source, Destination", new ColumnContentGetter[] { TYPE_CG, SOURCE_CG, DESTINATION_CG, ASSOCIATION_NAME_CG }),
    	TDS("Type, Destination, Source", new ColumnContentGetter[] { TYPE_CG, DESTINATION_CG, SOURCE_CG, ASSOCIATION_NAME_CG }),
    	STD("Source, Type, Destination", new ColumnContentGetter[] { SOURCE_CG, TYPE_CG, DESTINATION_CG, ASSOCIATION_NAME_CG }),
    	DTS("Destination, Type, Source", new ColumnContentGetter[] { DESTINATION_CG, TYPE_CG, SOURCE_CG, ASSOCIATION_NAME_CG }),
    	SDT("Source, Destination, Type", new ColumnContentGetter[] { SOURCE_CG, DESTINATION_CG, TYPE_CG, ASSOCIATION_NAME_CG }),
    	DST("Destination, Source, Type", new ColumnContentGetter[] { DESTINATION_CG, SOURCE_CG, TYPE_CG, ASSOCIATION_NAME_CG });
    	
    	public final ColumnContentGetter[] columnContentGetter;
    	public final String displayName;
    	
    	private GroupByDefinition(String displayName, ColumnContentGetter[] columnContentGetter) {
    		this.displayName = displayName;
    		this.columnContentGetter = columnContentGetter;
    	}
    	
    	public String toString() {
    		return displayName;
    	}
    };
    
    private List<MouseListener> allMouseListener = new ArrayList<MouseListener>();
    private List<Node> roots;
    
    /**
     * Updates the model UI.
     */
    private void updateModel() {
    	listPanel.removeAll();
    	allMouseListener.clear();
    	ColumnContentGetter[] columnContentGetter = ((GroupByDefinition) groupByComboBox.getSelectedItem()).columnContentGetter;
		roots = createHierarchy(columnContentGetter, 0, model);
    	for (int x = 0; x <= columnContentGetter.length; ++x) {
    		String text = x == 0? " " : columnContentGetter[x - 1].getDisplayName();
    		JLabel title = new JLabel(text + "  ");
    		title.setBackground(Color.WHITE);
    		title.setOpaque(true);
    		title.setFont(bold);
    		GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
            gridBagConstraints.gridx = x;
            gridBagConstraints.gridy = 0;
            gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
            listPanel.add(title, gridBagConstraints);
    	}
//		JPanel sep = new JPanel();
//		sep.setMinimumSize(new Dimension(10, 2));
//		sep.setBackground(Color.DARK_GRAY);
//		GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
//        gridBagConstraints.gridx = 0;
//        gridBagConstraints.gridy = 1;
//        gridBagConstraints.gridwidth = 4;
//        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
//        gridBagConstraints.weighty = 1.0;
//        listPanel.add(sep, gridBagConstraints);
    	
        int[] y = new int[] { 2 };
    	updateUI(roots, 0, y, new boolean[] { false }, new HashMap<Integer, Integer>());
    	JLabel l = new JLabel(" ");
    	l.setBackground(Color.WHITE);
    	l.setOpaque(true);
        GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = 5;
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = y[0];
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.weightx = 1.0;
        listPanel.add(l, gridBagConstraints);
        revalidate();
    }
    
    private void updateUI(List<Node> nodes, int level, int[] y, boolean[] groupStart, Map<Integer, Integer> minLevelPerY) {
    	boolean firstNode = true;
    	Node pred = null;
    	for (Node node: nodes) {
	    	GridBagConstraints gridBagConstraints;
    		Color bgColor;
    		if (y[0] % 2 == 0) {
    			bgColor = new java.awt.Color(240, 255, 255);
    		} else {
    			bgColor = Color.WHITE;
            }
    		if (!minLevelPerY.containsKey(y[0])) {
    			minLevelPerY.put(y[0], level);
    		}
    		if (!firstNode) {
    			for (int x = 0; x < level; ++x) {
    	    		JComponent l = createLabel(node, y[0], " ", null, false, false, bgColor, null, false);
    	    		gridBagConstraints = new java.awt.GridBagConstraints();
    	            gridBagConstraints.gridx = x + 1;
    	            gridBagConstraints.gridy = y[0];
    	            gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    	            listPanel.add(l, gridBagConstraints);
    			}
    		}
    		groupStart[0] = groupStart[0] || (y[0] > 0 && (!node.isLinear() || pred != null && !pred.isLinear())); // level < minLevelPerY.get(y[0] - 1));
    		Association a = node.associations.iterator().next();
			String joinCondition = a.getJoinCondition();
			if (a.reversed) {
				joinCondition = SqlUtil.reversRestrictionCondition(joinCondition);
			}
			JComponent l = createLabel(node, y[0], node.group, level >= 3? joinCondition : null, level < 3, groupStart[0], bgColor, level >= 3? Color.gray : null, node.isSelected());
    		gridBagConstraints = new java.awt.GridBagConstraints();
            gridBagConstraints.gridx = level + 1;
            gridBagConstraints.gridy = y[0];
            gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
            if (level == 3) {
            	gridBagConstraints.weightx = 1;
            }
            listPanel.add(l, gridBagConstraints);

            if (level >= 3) {
            	++y[0];
            	groupStart[0] = false;
            	JPanel p = new JPanel();
            	p.setLayout(new GridBagLayout());
        		gridBagConstraints = new java.awt.GridBagConstraints();
                gridBagConstraints.gridx = 0;
                gridBagConstraints.gridy = y[0] - 1;
                gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
                listPanel.add(p, gridBagConstraints);
                gridBagConstraints = new GridBagConstraints();
                gridBagConstraints.gridx = 1;
                gridBagConstraints.gridy = 1;
                gridBagConstraints.weightx = 1;
                gridBagConstraints.weighty = 1;
                gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
                gridBagConstraints.insets = new Insets(2, 0, 2, 0);
                final JCheckBox checkbox = new JCheckBox("  ");
                p.add(checkbox, gridBagConstraints);
        		p.setBackground(bgColor);
        		
        		if (node.associations.size() != 1) {
        			System.err.println("node.associations.size() != 1, " + node.associations.size());
        		} else {
        			final Association association = a;
                    checkbox.setSelected(selection.contains(association));
        			checkbox.addItemListener(new ItemListener() {
						@Override
						public void itemStateChanged(ItemEvent e) {
							if (checkbox.isSelected()) {
								selection.add(association);
							} else {
								selection.remove(association);
							}
							updateModel();
						}
					});
        		}
            } else {
            	updateUI(node.children, level + 1, y, groupStart, minLevelPerY);
            }
            firstNode = false;
            pred = node;
    	}
	}
    
    private final Color BGCOLOR_OF_SELECTED_ROW = Color.CYAN;
    private Font font = new JLabel("normal").getFont();
    private Font bold = new Font(font.getName(), font.getStyle() | Font.BOLD, font.getSize());
    private Font nonbold = new Font(font.getName(), font.getStyle() & ~Font.BOLD, font.getSize());
	
    private JComponent createLabel(final Node node, final int y, String text, String tooltip, boolean shorten, final boolean firstOfGroup, Color bgColor, Color fgColor, boolean selected) {
    	final JPanel panel = new JPanel() {
			@Override
    		public void paint(Graphics graphics) {
    			super.paint(graphics);
    			if (firstOfGroup) {
	    			if (!(graphics instanceof Graphics2D))
	    				return;
	    			Graphics2D g2d = (Graphics2D) graphics;
	    			Color color = Color.gray;
	    			g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
	    			g2d.setColor(color);
	    			g2d.setStroke(new BasicStroke());
	    			g2d.drawLine(0, 1, getSize().width - 1, 1);
    			}
    		}
			private static final long serialVersionUID = 5285941807747744395L;
    	};
		panel.setLayout(new GridBagLayout());
		final JLabel label = new JLabel();
		label.setOpaque(true);
		label.setFont(selected? bold : nonbold);
		if (fgColor != null) {
			label.setForeground(fgColor);
		}
		GridBagConstraints gridBagConstraints = new GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.weightx = 1;
        gridBagConstraints.weighty = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.insets = new Insets(2, 0, 2, 0);
        panel.add(label, gridBagConstraints);
		if (text.trim().length() > 0) {
			label.setToolTipText(text);
		}
		if (tooltip != null) {
			label.setToolTipText(tooltip);
		}
		if (shorten) {
			if (text.length() > 30) {
				text = text.substring(0, 30) + "...";
			}
		}
		label.setText(text + "  ");
		label.setBackground(bgColor);
		panel.setBackground(bgColor);
		
		if ("".equals(text.trim())) {
			return panel;
		}
		MouseListener l;
		label.addMouseListener(l = new MouseListener() {
			Color bgColor;
			@Override
			public void mouseReleased(MouseEvent e) {
			}
			@Override
			public void mousePressed(MouseEvent e) {
			}
			@Override
			public void mouseExited(MouseEvent e) {
				if (bgColor != null) {
					label.setBackground(bgColor);
				}
			}
			@Override
			public void mouseEntered(MouseEvent e) {
				for (MouseListener l: allMouseListener) {
					if (l != this) {
						l.mouseExited(e);
					}
				}
				if (bgColor == null) {
					bgColor = label.getBackground();
				}
				label.setBackground(BGCOLOR_OF_SELECTED_ROW);
			}
			@Override
			public void mouseClicked(MouseEvent e) {
				if (e.getButton() == MouseEvent.BUTTON1) {
					if (node.isSelected()) {
						selection.removeAll(node.associations);
					} else {
						selection.addAll(node.associations);
					}
					updateModel();
				}
				
			}
		});
		allMouseListener.add(l);
		return panel;
	}
    
    /**
     * Create hierarchy of associations.
     * 
     * @param columnContentGetter the {@link ColumnContentGetter}
     * @param i index of current {@link ColumnContentGetter}
     * @return list of nodes
     */
    private List<Node> createHierarchy(ColumnContentGetter[] columnContentGetter, int i, Collection<Association> associations) {
    	SortedMap<String, Collection<Association>> groups = new TreeMap<String, Collection<Association>>();
    	for (Association association: associations) {
    		String key = columnContentGetter[i].getContent(association);
    		if (!groups.containsKey(key)) {
    			groups.put(key, new ArrayList<Association>());
    		}
    		groups.get(key).add(association);
    	}
    	List<Node> result = new ArrayList<Node>();
    	for (String groupKey: groups.keySet()) {
    		Collection<Association> group = groups.get(groupKey);
    		Node node = new Node(groupKey, group, columnContentGetter[i]);
    		result.add(node);
    		if (i + 1 < columnContentGetter.length) {
    			node.children.addAll(createHierarchy(columnContentGetter, i + 1, group));
    		}
    	}
		return result;
	}

	/** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jPanel1 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        listPanel = new javax.swing.JPanel();
        jPanel2 = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        groupByComboBox = new javax.swing.JComboBox();

        setLayout(new java.awt.BorderLayout());

        jPanel1.setLayout(new java.awt.GridBagLayout());

        listPanel.setLayout(new java.awt.GridBagLayout());
        jScrollPane1.setViewportView(listPanel);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel1.add(jScrollPane1, gridBagConstraints);

        jPanel2.setLayout(new java.awt.GridBagLayout());

        jLabel1.setText("Group by ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        jPanel2.add(jLabel1, gridBagConstraints);

        groupByComboBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        jPanel2.add(groupByComboBox, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(jPanel2, gridBagConstraints);

        add(jPanel1, java.awt.BorderLayout.CENTER);
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JComboBox groupByComboBox;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JPanel listPanel;
    // End of variables declaration//GEN-END:variables

	private static final long serialVersionUID = -5302225732569622137L;

}
