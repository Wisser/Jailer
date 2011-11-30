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
public abstract class AssociationListUI extends javax.swing.JPanel {

	/**
	 * Types of {@link Association}.
	 */
	private enum AssociationType {
		CHILD("1Child", GREEN),
		ASSOC("2Association", new Color(0, 0, 150)),
		PARENT("3Parent", RED);
		
		public final String name;
		public final Color color;
		
		private AssociationType(String name, Color color) {
			this.name = name;
			this.color = color;
		}
	};
	
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
	    
	    AssociationType getType();
	    
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
	    	return association.getUnrestrictedJoinCondition();
	    }
	    
	    /**
	     * The cardinality.
	     */
	    public Cardinality getCardinality() {
	    	return association.getCardinality();
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

		@Override
		public AssociationType getType() {
			if (association.isInsertSourceBeforeDestination()) {
				return AssociationType.CHILD;
			}
			if (association.isInsertDestinationBeforeSource()) {
				return AssociationType.PARENT;
			}
			return AssociationType.ASSOC;
		}
	    
		@Override
		public boolean equals(Object obj) {
			if (obj instanceof DefaultAssociationModel) {
				return association.equals(((DefaultAssociationModel) obj).association);
			}
			return false;
		}

		@Override
		public int hashCode() {
			return association.hashCode();
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
	 * Hidden model associations.
	 */
	private Set<AssociationModel> hidden = new HashSet<AssociationModel>();
	
	/**
	 * All hidden associations.
	 */
	private Set<AssociationModel> hiddenAll = new HashSet<AssociationModel>();
	
	/**
	 * Keep order of associations according to the source-table?
	 */
	private final boolean stableSourceOrder;
	
	/** Creates new form AssociationListUI */
    public AssociationListUI(String actionButtonText, String actionButtonToolTip, boolean stableSourceOrder) {
    	this.stableSourceOrder = stableSourceOrder;
        initComponents();
        doItButton.setText(actionButtonText);
        doItButton.setToolTipText(actionButtonToolTip);
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
    	hidden.clear();
    	hidden.addAll(hiddenAll);
    	hidden.retainAll(model);
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
    
    private static final Color GREEN = new Color(0, 130, 0);
	private static final Color RED = Color.red;
	
	private static abstract class ColumnContentGetter {
    	public abstract String getContent(AssociationModel association);
    	public abstract String getDisplayName();
    	public Color getFgColor(Collection<AssociationModel> group) {
			Color fg = null;
			for (AssociationModel associationModel: group) {
				Color c = associationModel.getType().color;
				if (fg == null || fg == c) {
					fg = c;
				} else {
					fg = null;
					break;
				}
			}
			return fg;
		}
    };
    
    private static class TypeGetter extends ColumnContentGetter {
    	public String getContent(AssociationModel association) {
    		return association.getType().name;
    	}
		public String getDisplayName() {
			return "Type";
		}
    };
    
    private static class SourceGetter extends ColumnContentGetter {
    	public String getContent(AssociationModel association) {
    		return association.getSourceName();
    	}
		public String getDisplayName() {
			return "From";
		}
		public Color getFgColor(Collection<AssociationModel> group) {
			return null;
		}
    };

    private static class DestinationGetter extends ColumnContentGetter {
    	public String getContent(AssociationModel association) {
    		return association.getDestinationName();
    	}
		public String getDisplayName() {
			return "To";
		}
    };

    private static class AssociationNameGetter extends ColumnContentGetter {
    	public String getContent(AssociationModel association) {
    		return association.getName();
    	}
		public String getDisplayName() {
			return "Association Name";
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

		public Collection<Node> getSubNodes() {
			Collection<Node> subNodes = new ArrayList<Node>();
			for (Node child: children) {
				subNodes.add(child);
				subNodes.addAll(child.getSubNodes());
			}
			return subNodes;
		}
    }

    private static ColumnContentGetter TYPE_CG = new TypeGetter();
    private static ColumnContentGetter SOURCE_CG = new SourceGetter();
    private static ColumnContentGetter DESTINATION_CG = new DestinationGetter();
    private static ColumnContentGetter ASSOCIATION_NAME_CG = new AssociationNameGetter();
    
    private enum GroupByDefinition {
    	TSD("Type, From, To", new ColumnContentGetter[] { TYPE_CG, SOURCE_CG, DESTINATION_CG, ASSOCIATION_NAME_CG }),
    	TDS("Type, To, From", new ColumnContentGetter[] { TYPE_CG, DESTINATION_CG, SOURCE_CG, ASSOCIATION_NAME_CG }),
    	STD("From, Type, To", new ColumnContentGetter[] { SOURCE_CG, TYPE_CG, DESTINATION_CG, ASSOCIATION_NAME_CG }),
    	DTS("To, Type, From", new ColumnContentGetter[] { DESTINATION_CG, TYPE_CG, SOURCE_CG, ASSOCIATION_NAME_CG }),
    	SDT("From, To, Type", new ColumnContentGetter[] { SOURCE_CG, DESTINATION_CG, TYPE_CG, ASSOCIATION_NAME_CG }),
    	DST("To, From, Type", new ColumnContentGetter[] { DESTINATION_CG, SOURCE_CG, TYPE_CG, ASSOCIATION_NAME_CG });
    	
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
    private Map<Node, List<MouseListener>> mouseListenerPerNode = new HashMap<Node, List<MouseListener>>();
    private List<Node> roots;
    
    /**
     * Hides selected associations.
     */
    private void hideSelection() {
    	hidden.addAll(selection);
    	hiddenAll.addAll(selection);
    	selection.clear();
    	updateModel();
    }

    /**
     * Unhides hidden associations.
     */
    private void unhideHidden() {
    	hidden.clear();
    	hiddenAll.clear();
    	updateModel();
    }

    /**
     * Performes action on selected associations.
     */
    protected abstract void applyAction(Collection<AssociationModel> selection);
    
    /**
     * Updates the model UI.
     */
    private void updateModel() {
    	listPanel.removeAll();
    	allMouseListener.clear();
    	mouseListenerPerNode.clear();
    	ColumnContentGetter[] columnContentGetter = ((GroupByDefinition) groupByComboBox.getSelectedItem()).columnContentGetter;
    	Collection<AssociationModel> unhidden = new ArrayList<AssociationListUI.AssociationModel>(model);
    	unhidden.removeAll(hidden);
		roots = createHierarchy(columnContentGetter, 0, unhidden);
    	if (!unhidden.isEmpty()) {
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
    	}
    	
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
        
        infoLabel.setText(selection.size() + " selected association" + (selection.size() == 1? "" : "s") + ", " + hidden.size() + " hidden ");
        hideButton.setEnabled(!selection.isEmpty());
        doItButton.setEnabled(!selection.isEmpty());
        unhideButton.setEnabled(!hidden.isEmpty());
        
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
    	    		JComponent l = createLabel(node, y[0], shorten(lastText), null, false, false, bgColor, lastFgColor, false, false);
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
                checkbox.setBackground(bgColor);
        		
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
								onSelect(association);
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
			Color bgColorL;
			Color bgColorP;
			@Override
			public void mouseReleased(MouseEvent e) {
			}
			@Override
			public void mousePressed(MouseEvent e) {
			}
			@Override
			public void mouseExited(MouseEvent e) {
				if (e != null) {
					for (Node subNode: node.getSubNodes()) {
						for (MouseListener l: mouseListenerPerNode.get(subNode)) {
							if (l != this) {
								l.mouseExited(null);
							}
						}
					}
				}
				if (bgColorL != null) {
					label.setBackground(bgColorL);
				}
				if (bgColorP != null) {
					panel.setBackground(bgColorP);
				}
			}
			@Override
			public void mouseEntered(MouseEvent e) {
				if (e != null) {
					for (MouseListener l: allMouseListener) {
						if (l != this) {
							l.mouseExited(null);
						}
					}
					for (Node subNode: node.getSubNodes()) {
						for (MouseListener l: mouseListenerPerNode.get(subNode)) {
							if (l != this) {
								l.mouseEntered(null);
							}
						}
					}
				}
				if (bgColorL == null) {
					bgColorL = label.getBackground();
				}
				if (bgColorP == null) {
					bgColorP = panel.getBackground();
				}
				panel.setBackground(BGCOLOR_OF_SELECTED_ROW);
				label.setBackground(BGCOLOR_OF_SELECTED_ROW);
			}
			@Override
			public void mouseClicked(MouseEvent e) {
				if (e.getButton() == MouseEvent.BUTTON1) {
					if (node.isSelected()) {
						selection.removeAll(node.associations);
					} else {
						selection.addAll(node.associations);
						Node leaf = node;
						while (leaf.children != null && leaf.children.size() > 0) {
							leaf = leaf.children.get(0);
						}
						if (leaf.associations.size() > 0) {
							onSelect(leaf.associations.iterator().next());
						}
					}
					updateModel();
				}
			}
		});
		allMouseListener.add(l);
		if (mouseListenerPerNode.get(node) == null) {
			mouseListenerPerNode.put(node, new ArrayList<MouseListener>());
		}
		mouseListenerPerNode.get(node).add(l);
		return panel;
	}
    
    protected void onSelect(AssociationModel association) {
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
    		String gName = groupKey;
    		if (columnContentGetter[i] instanceof TypeGetter) {
    			gName = gName.substring(1);
    		}
    		Node node = new Node(gName, group, columnContentGetter[i].getFgColor(group));
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
        groupByComboBox = new net.sf.jailer.ui.JComboBox();
        infoLabel = new javax.swing.JLabel();
        jPanel4 = new javax.swing.JPanel();
        hideButton = new javax.swing.JButton();
        unhideButton = new javax.swing.JButton();
        doItButton = new javax.swing.JButton();

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

        jLabel1.setText(" Sorted by ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        jPanel2.add(jLabel1, gridBagConstraints);

        groupByComboBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel2.add(groupByComboBox, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(jPanel2, gridBagConstraints);

        infoLabel.setText(" 12 selected, 0 hidden");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 4, 0);
        jPanel1.add(infoLabel, gridBagConstraints);

        jPanel4.setLayout(new java.awt.GridBagLayout());

        hideButton.setText("Hide");
        hideButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                hideButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        jPanel4.add(hideButton, gridBagConstraints);

        unhideButton.setText("Unhide");
        unhideButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                unhideButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        jPanel4.add(unhideButton, gridBagConstraints);

        doItButton.setText("doIt");
        doItButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                doItButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        jPanel4.add(doItButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        jPanel1.add(jPanel4, gridBagConstraints);

        add(jPanel1, java.awt.BorderLayout.CENTER);
    }// </editor-fold>//GEN-END:initComponents

    private void hideButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_hideButtonActionPerformed
        hideSelection();
    }//GEN-LAST:event_hideButtonActionPerformed

    private void unhideButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_unhideButtonActionPerformed
        unhideHidden();
    }//GEN-LAST:event_unhideButtonActionPerformed

    private void doItButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_doItButtonActionPerformed
        applyAction(new ArrayList<AssociationListUI.AssociationModel>(selection));
    }//GEN-LAST:event_doItButtonActionPerformed


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton doItButton;
    private javax.swing.JComboBox groupByComboBox;
    private javax.swing.JButton hideButton;
    private javax.swing.JLabel infoLabel;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JPanel listPanel;
    private javax.swing.JButton unhideButton;
    // End of variables declaration//GEN-END:variables

	private static final long serialVersionUID = -5302225732569622137L;

}
