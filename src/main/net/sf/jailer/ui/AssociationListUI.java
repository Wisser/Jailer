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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Cardinality;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.SqlUtil;

/**
 * Lets user apply an action on a subset of a given {@link Association} set.
 *
 * @author Ralf Wisser
 */
public class AssociationListUI extends javax.swing.JPanel {

	/**
	 * Association model for this UI.
	 */
	public interface AssociationModel {
		
		/**
	     * The source table.
	     */
	    String getSourceName();

	    /**
	     * The destination table.
	     */
	    String getDestinationName();

	    /**
	     * The join-condition for joining source with destination table.
	     */
	    String getJoinCondition();
	    
	    /**
	     * The cardinality.
	     */
	    Cardinality getCardinality();
	    
	    /**
	     * Whether or not to insert source-rows before destination rows
	     * in order to prevent foreign-key-constraint violation.
	     */
	    boolean isInsertSourceBeforeDestination();
	    
	    /**
	     * Whether or not to insert destination-rows before source-rows
	     * in order to prevent foreign-key-constraint violation.
	     */
	    boolean isInsertDestinationBeforeSource();
	    
	    /**
	     * <code>true</code> for reversed association.
	     */
	    boolean isReversed();

	    /**
	     * The name of the association.
	     */
	    String getName();

		DataModel getDataModel();
	    
	};
	
	/**
	 * Default implementation of an association model for this UI.
	 */
	public static class DefaultAssociationModel implements AssociationModel {
		
		protected final Association association;
		
		public DefaultAssociationModel(Association association) {
			this.association = association;
		}
		
		/**
	     * The source table.
	     */
	    public String getSourceName() {
	    	return association.getDataModel().getDisplayName(association.source);
	    }

	    /**
	     * The destination table.
	     */
	    public String getDestinationName() {
	    	return association.getDataModel().getDisplayName(association.destination);
	    }

	    /**
	     * The join-condition for joining source with destination table.
	     */
	    public String getJoinCondition() {
	    	return association.getJoinCondition();
	    }
	    
	    /**
	     * The cardinality.
	     */
	    public Cardinality getCardinality() {
	    	return association.getCardinality();
	    }
	    
	    /**
	     * Whether or not to insert source-rows before destination rows
	     * in order to prevent foreign-key-constraint violation.
	     */
	    public boolean isInsertSourceBeforeDestination() {
	    	return association.isInsertSourceBeforeDestination();
	    }
	    
	    /**
	     * Whether or not to insert destination-rows before source-rows
	     * in order to prevent foreign-key-constraint violation.
	     */
	    public boolean isInsertDestinationBeforeSource() {
	    	return association.isInsertDestinationBeforeSource();
	    }
	    
	    /**
	     * <code>true</code> for reversed association.
	     */
	    public boolean isReversed() {
	    	return association.reversed;
	    }

	    /**
	     * The name of the association.
	     */
	    public String getName() {
	    	return association.getName();
	    }

		public DataModel getDataModel() {
			return association.getDataModel();
		}
	    
	};
	
	/**
	 * The model.
	 */
	private List<AssociationModel> model = new ArrayList<AssociationModel>();
	
	/**
	 * Selected associations.
	 */
	private Set<AssociationModel> selection = new HashSet<AssociationModel>();
	
	/**
	 * Keep order of associations according to the source-table?
	 */
	private final boolean stableSourceOrder;
	
	/** Creates new form AssociationListUI */
    public AssociationListUI(boolean stableSourceOrder) {
    	this.stableSourceOrder = stableSourceOrder;
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
    public void setModel(Collection<AssociationModel> model) {
    	this.model = new ArrayList<AssociationModel>(model);
    	selection.retainAll(model);
    	if (pixelPerTableNameChar == null) {
	    	Iterator<AssociationModel> firstAssociationI = model.iterator();
	    	if (firstAssociationI.hasNext()) {
	    		AssociationModel firstAssociation = firstAssociationI.next();
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
    	abstract String getContent(AssociationModel association);
		abstract String getDisplayName();
		abstract Color getFgColor(String groupKey);
    };
    
    private static class TypeGetter implements ColumnContentGetter {
    	private static final String PARENT = "Parent";
    	private static final String CHILD = "Child";
    	public String getContent(AssociationModel association) {
    		if (association.isInsertDestinationBeforeSource()) {
    			return PARENT;
    		}
    		return "Child";
    	}
		public String getDisplayName() {
			return "Type";
		}
		public Color getFgColor(String groupKey) {
			if (PARENT.equals(groupKey)) {
				return Color.red;
			}
			if (CHILD.equals(groupKey)) {
				return new Color(0, 100, 0);
			}
			return null;
		}
    };
    
    private static class SourceGetter implements ColumnContentGetter {
    	public String getContent(AssociationModel association) {
    		return association.getSourceName();
    	}
		public String getDisplayName() {
			return "From";
		}
		public Color getFgColor(String groupKey) {
			return null;
		}
    };

    private static class DestinationGetter implements ColumnContentGetter {
    	public String getContent(AssociationModel association) {
    		return association.getDestinationName();
    	}
		public String getDisplayName() {
			return "To";
		}
		public Color getFgColor(String groupKey) {
			return null;
		}
    };

    private static class AssociationNameGetter implements ColumnContentGetter {
    	public String getContent(AssociationModel association) {
    		return association.getName();
    	}
		public String getDisplayName() {
			return "Association Name";
		}
		public Color getFgColor(String groupKey) {
			return null;
		}
    };

    private class Node {
    	public final Collection<AssociationModel> associations;
    	public final List<Node> children = new ArrayList<AssociationListUI.Node>();
		public final String group;
    	public final Color fgColor;
		
    	public Node(String group, Collection<AssociationModel> associations, Color fgColor) {
    		this.group = group;
    		this.associations = associations;
    		this.fgColor = fgColor;
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
    	updateUI(roots, 0, y, new boolean[] { false }, new HashMap<Integer, Integer>(), new Node[5]);
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
    
    private void updateUI(List<Node> nodes, int level, int[] y, boolean[] groupStart, Map<Integer, Integer> minLevelPerY, Node[] lastRowContent) {
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
    				String lastText = " ";
    				Color lastFgColor = null;
    				if (lastRowContent[x] != null) {
    					lastText = lastRowContent[x].group;
    					lastFgColor = lastRowContent[x].fgColor;
    					if (lastFgColor != null) {
    						lastFgColor = new Color(
    								lastFgColor.getRed() + 2 * (255 - lastFgColor.getRed()) / 3,
    								lastFgColor.getGreen() + 2 * (255 - lastFgColor.getGreen()) / 3,
    								lastFgColor.getBlue() + 2 * (255 - lastFgColor.getBlue()) / 3);
    					} else {
    						lastFgColor = Color.lightGray;
    					}
    				}
    	    		JComponent l = createLabel(node, y[0], lastText, null, false, false, bgColor, lastFgColor, false, false);
    	    		gridBagConstraints = new java.awt.GridBagConstraints();
    	            gridBagConstraints.gridx = x + 1;
    	            gridBagConstraints.gridy = y[0];
    	            gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    	            listPanel.add(l, gridBagConstraints);
    			}
    		}
    		groupStart[0] = groupStart[0] || (y[0] > 0 && (!node.isLinear() || pred != null && !pred.isLinear())); // level < minLevelPerY.get(y[0] - 1));
    		AssociationModel a = node.associations.iterator().next();
			String joinCondition = a.getJoinCondition();
			if (a.isReversed()) {
				joinCondition = SqlUtil.reversRestrictionCondition(joinCondition);
			}
			JComponent l = createLabel(node, y[0], node.group, level >= 3? joinCondition : null, level < 3, groupStart[0], bgColor, level >= 3? Color.gray : node.fgColor, node.isSelected(), true);
			lastRowContent[level] = node;
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
        			final AssociationModel association = a;
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
            	updateUI(node.children, level + 1, y, groupStart, minLevelPerY, lastRowContent);
            }
            firstNode = false;
            pred = node;
    	}
	}
    
    private final Color BGCOLOR_OF_SELECTED_ROW = Color.CYAN;
    private Font font = new JLabel("normal").getFont();
    private Font bold = new Font(font.getName(), font.getStyle() | Font.BOLD, font.getSize());
    private Font nonbold = new Font(font.getName(), font.getStyle() & ~Font.BOLD, font.getSize());
	
    private JComponent createLabel(final Node node, final int y, String text, String tooltip, boolean shorten, final boolean firstOfGroup, Color bgColor, Color fgColor, boolean selected, boolean addListener) {
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
		if (selected) {
			bgColor = new Color(220, 225, 255);
		}
    	panel.setLayout(new GridBagLayout());
		final JLabel label = new JLabel();
		label.setOpaque(true);
		label.setFont(nonbold);
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
			text = shorten(text);
		}
		label.setText(text + "  ");
		label.setBackground(bgColor);
		panel.setBackground(bgColor);
		
		if (!addListener) {
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
    
    private Map<String, String> shortForms = new HashMap<String, String>();
    
    private String shorten(String text) {
    	if (shortForms.containsKey(text)) {
    		return shortForms.get(text);
    	}
    	Pattern p = Pattern.compile("^(.*) (\\([0-9]*\\))$");
    	Matcher matcher = p.matcher(text);
    	String pre = text;
    	String suf = "";
    	if (matcher.matches()) {
    		pre = matcher.group(1);
    		suf = matcher.group(2);
    	}
    	
    	String shortText = text;
    	int maxWidth = (int) ((pixelPerTableNameChar == null? 8 : pixelPerTableNameChar) * 30.0);
    	
    	for (int i = pre.length() - 2; i > 3; --i) {
    		if (new JLabel(shortText).getMinimumSize().width <= maxWidth) {
    			break;
    		}
    		shortText = pre.substring(0, i) + "..." + suf;
		}
    	
    	shortForms.put(text, shortText);
		return shortText;
	}

	/**
     * Create hierarchy of associations.
     * 
     * @param columnContentGetter the {@link ColumnContentGetter}
     * @param i index of current {@link ColumnContentGetter}
     * @return list of nodes
     */
    private List<Node> createHierarchy(ColumnContentGetter[] columnContentGetter, int i, Collection<AssociationModel> associations) {
    	SortedMap<String, Collection<AssociationModel>> groups = new TreeMap<String, Collection<AssociationModel>>();
    	List<String> groupKeys = new ArrayList<String>();
    	for (AssociationModel association: associations) {
    		String key = columnContentGetter[i].getContent(association);
    		if (!groups.containsKey(key)) {
    			groups.put(key, new ArrayList<AssociationModel>());
    			groupKeys.add(key);
    		}
    		groups.get(key).add(association);
    	}
    	List<Node> result = new ArrayList<Node>();
    	Iterable<String> gk = groups.keySet();
    	if (stableSourceOrder && (columnContentGetter[i] instanceof SourceGetter)) {
    		gk = groupKeys;
    	}
    	for (String groupKey: gk) {
    		Collection<AssociationModel> group = groups.get(groupKey);
    		Node node = new Node(groupKey, group, columnContentGetter[i].getFgColor(groupKey));
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
