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
package net.sf.jailer.ui.pathfinder;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.geom.Path2D;
import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.Stack;

import javax.swing.DefaultCellEditor;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JTable;
import javax.swing.JToggleButton;
import javax.swing.RowSorter.SortKey;
import javax.swing.SortOrder;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.pathfinder.PathGraph.Node;
import net.sf.jailer.ui.util.SmallButton;

/**
 * Finds path between two tables.
 * 
 * @author Ralf Wisser
 */
public abstract class PathFinderView extends javax.swing.JPanel {

	private final DataModel dataModel;
	private final Table source;
	private final Table destination;
	private final Map<Node, NodeRender> renderPerNode = new HashMap<Node, NodeRender>();
	private PathGraph pathGraph;
	private List<Table> result = new ArrayList<Table>();

	private List<Table> pathStations  = new ArrayList<Table>();
	private Set<Table> excludedTables = new HashSet<Table>();
	
	class State {
		final List<Table> pathStations;
		final Set<Table> excludedTables;

		State(List<Table> pathStations, Set<Table> excludedTables) {
			this.pathStations = new ArrayList<Table>(pathStations);
			this.excludedTables = new HashSet<Table>(excludedTables);
		}
	};
	
	private State lastState = null;
	private Stack<State> undoStack = new Stack<State>();
	private Stack<State> redoStack = new Stack<State>();

    /**
     * Creates new form PathFinderView
     * @param destination 
     * @param source 
     */
    public PathFinderView(DataModel dataModel, Table source, Table destination, boolean withExpandButton) {
    	this.dataModel = dataModel;
    	this.source = source;
    	this.destination = destination;
        initComponents();
        if (!withExpandButton) {
        	okExpandButton.setVisible(false);
        }
        scaledCancelIcon = UIUtil.scaleIcon(this, cancelIcon);
        undoButton.setText(null);
        undoButton.setToolTipText("Undo");
        undoButton.setIcon(UIUtil.scaleIcon(this, leftIcon));
        redoButton.setText(null);
        redoButton.setToolTipText("Redo");
        redoButton.setIcon(UIUtil.scaleIcon(this, rightIcon));
    }

    public void showGraph(boolean undoing) {
    	State state = new State(pathStations, excludedTables);
    	
    	if (!undoing) {
    		if (lastState != null) {
    			undoStack.push(lastState);
    			redoStack.clear();
    		}
    	}

    	this.pathGraph = new PathGraph(dataModel, source, destination, excludedTables, pathStations);
    	renderPerNode.clear();
    	Set<Integer> nonExcludablesColumns = new HashSet<Integer>();
    	JPanel panel = new JPanel(new GridBagLayout()) {
			private static final long serialVersionUID = 1L;
			@Override
			public void paint(Graphics graphics) {
				super.paint(graphics);
				if (graphics instanceof Graphics2D) {
					Graphics2D g2d = (Graphics2D) graphics;
					g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
					g2d.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
					for (Entry<Node, NodeRender> e: renderPerNode.entrySet()) {
						Node node = e.getKey();
						Point start = e.getValue().getLocation();
						start = SwingUtilities.convertPoint(e.getValue().getParent(), start, this);
						start.translate(0, e.getValue().getHeight() / 2);
						for (Node pre: node.prev) {
							NodeRender preRender = renderPerNode.get(pre);
							Point end = preRender.getLocation();
							end = SwingUtilities.convertPoint(preRender.getParent(), end, this);
							end.translate(preRender.getWidth(), preRender.getHeight() / 2);
							boolean selected = (relatedToSelectedNode.contains(pre) || selectedNode == pre) && (relatedToSelectedNode.contains(node) || selectedNode == node);
							Color color;
							PathGraph.EdgeType type = pathGraph.getEdgeType(pre, node);
							if (PathGraph.EdgeType.PARENT.equals(type)) {
								color = new Color(255, 0, 0);
							} else if (PathGraph.EdgeType.CHILD.equals(type)) {
								color = new Color(0, 200, 0);
							} else {
								color = new Color(0, 0, 255);
							}
							paintLink(start, end, color, selected, g2d);
						}
					}
				}
			}

			private void paintLink(Point2D start, Point2D end, Color color, boolean selected, Graphics2D g2d) {
				g2d.setColor(color);
				BasicStroke stroke = new BasicStroke(selected? 2 : 1);
				g2d.setStroke(stroke);
				
				Path2D.Double path = new Path2D.Double();
				double border = 0.25;
				double f = (1.0 - 2.0 * border) / 20;
				int midX = (int) (start.getX() + ((end.getX() - start.getX()) * (border + f)));
				f = 0.25 * f * (end.getY() - start.getY());
				path.moveTo(end.getX(), end.getY());
				path.curveTo(midX, end.getY(), midX, start.getY() + f, start.getX(), start.getY());
				g2d.draw(path);
			}
    	};
    	panel.setOpaque(false);

    	GridBagConstraints gridBagConstraints;

    	result.clear();
    	okButton.setEnabled(true);
    	okExpandButton.setEnabled(true);
    	List<Table> newPathStations = new ArrayList<Table>();
    	
    	List<JComponent> excludeButtons = new ArrayList<JComponent>();
    	int x = 0;
    	int maxY = 0;
    	int maxX = 0;
    	for (;;) {
    		final List<PathGraph.Node> nodes = pathGraph.getNodes(x);
    		if (nodes.isEmpty()) {
    			break;
    		}
    		if (nodes.size() == 1) {
    			result.add(nodes.get(0).table);
    		} else {
    			result.add(null);
    			okButton.setEnabled(false);
    			okExpandButton.setEnabled(false);
    		}
    		boolean showExcludeButton = true;
    		if (nodes.size() <= 1) {
    			showExcludeButton = false;
    		} else {
    			Node node = nodes.get(0);
	    		if (node.table.equals(source) || node.table.equals(destination)) {
	    			showExcludeButton = false;
	    		} else {
	    			Set<Table> excl = new HashSet<Table>(excludedTables);
	    			excl.add(node.table);
	    			if (new PathGraph(dataModel, source, destination, excl, pathStations).isEmpty()) {
	    				showExcludeButton = false;
	    			}
	    		}
    		}
    		if (!showExcludeButton) {
    			nonExcludablesColumns.add(x);
    		}
    		Collections.sort(nodes, new Comparator<PathGraph.Node>() {
				@Override
				public int compare(Node n1, Node n2) {
					return dataModel.getDisplayName(n1.table).compareTo(dataModel.getDisplayName(n2.table));
				}
			});
    		if (x > 0) {
	    		JButton excludeButton = new JButton(null, scaledCancelIcon);
	    		excludeButton.setToolTipText("Exclude all tables with distance " + x);
	    		excludeButtons.add(excludeButton);
	    		excludeButton.setVisible(showExcludeButton);
				final Set<Table> toExclude = new HashSet<Table>();
				for (Node node: nodes) {
					toExclude.add(node.table);
				}
				toExclude.remove(source);
				toExclude.remove(destination);
	    		if (showExcludeButton) {
	    			Set<Table> excl = new HashSet<Table>(excludedTables);
	    			excl.addAll(toExclude);
	    			if (new PathGraph(dataModel, source, destination, excl, pathStations).isEmpty()) {
	    				excludeButton.setVisible(false);
	    			}
	    		}
	    		excludeButton.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent arg0) {
						excludedTables.addAll(toExclude);
						showGraph(false);
					}
				});
    		}
    		int y = 4;
    		for (Node node: nodes) {
    			int pathStationIndex = newPathStations.size();
    			if (nodes.size() == 1 && pathStations.contains(node.table)) {
    				newPathStations.add(node.table);
    			}
				NodeRender nodeRender = new NodeRender(node, showExcludeButton, newPathStations.contains(node.table), pathStationIndex) {
					@Override
					protected void onClick() {
					}
					private static final long serialVersionUID = 1L;
				};
				renderPerNode.put(node, nodeRender);

		        gridBagConstraints = new java.awt.GridBagConstraints();
		        gridBagConstraints.gridx = x;
		        gridBagConstraints.gridy = y;
		        gridBagConstraints.fill = GridBagConstraints.BOTH;
		        gridBagConstraints.insets = new Insets(0, 0, 0, 24);
		        gridBagConstraints.anchor = GridBagConstraints.WEST;
		        panel.add(nodeRender, gridBagConstraints);
		        ++y;
    		}
    		maxY = Math.max(maxY, y);
    		maxX = Math.max(maxX, x);
    		++x;
    	}
    	if (!excludeButtons.isEmpty()) {
    		excludeButtons.remove(excludeButtons.size() - 1);
    	}
    	pathStations = newPathStations;
    	
    	x = 1;
    	for (x = 0; x <= maxX; ++x) {
    		JComponent excludeButton = null;
    		if (x > 0 && x < maxX) {
    			excludeButton = excludeButtons.get(x - 1);
    		}
    		if (excludeButton != null) {
	    		gridBagConstraints = new java.awt.GridBagConstraints();
	            gridBagConstraints.gridx = x;
	            gridBagConstraints.gridy = maxY + 1;
	            gridBagConstraints.anchor = GridBagConstraints.EAST;
	            gridBagConstraints.insets = new Insets(12, 0, 2, 26);
	    		panel.add(excludeButton, gridBagConstraints);
    		}

    		JLabel distanceLabel;
    		if (x == 0) {
    			distanceLabel = new JLabel("Source");
    		} else if (x == maxX) {
    			distanceLabel = new JLabel("Destination");
    		} else {
    			distanceLabel = new JLabel("" + x);
    		}
    		distanceLabel.setFont(distanceLabel.getFont().deriveFont(distanceLabel.getFont().getStyle() | Font.BOLD));
    		gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = x;
	        gridBagConstraints.gridy = 0;
	        gridBagConstraints.fill = GridBagConstraints.NONE;
	        gridBagConstraints.insets = new Insets(6, 16, 2, 8);
    		gridBagConstraints.anchor = GridBagConstraints.WEST;
	        panel.add(distanceLabel, gridBagConstraints);
    	}
    	
    	gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = maxX + 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.weightx = 1;
        panel.add(new JLabel(" "), gridBagConstraints);
        
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = maxY + 8;
        gridBagConstraints.weighty = 1;
        panel.add(new JLabel(" "), gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = maxX + 1;
        gridBagConstraints.fill = GridBagConstraints.BOTH;
        gridBagConstraints.insets = new Insets(4, 0, 4, 24);
        gridBagConstraints.anchor = GridBagConstraints.WEST;
        panel.add(new JSeparator(), gridBagConstraints);
        
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = maxY + 1;
        gridBagConstraints.gridwidth = maxX + 1;
        gridBagConstraints.fill = GridBagConstraints.BOTH;
        gridBagConstraints.insets = new Insets(4, 0, 4, 24);
        gridBagConstraints.anchor = GridBagConstraints.WEST;
        panel.add(new JSeparator(), gridBagConstraints);
        
    	pathContainerPanel.removeAll();
    	pathContainerPanel.add(panel, java.awt.BorderLayout.CENTER);

    	initExclusionTable(nonExcludablesColumns);

    	forceRepaint();
    	
    	lastState = state;
    	undoButton.setEnabled(!undoStack.isEmpty());
    	redoButton.setEnabled(!redoStack.isEmpty());
    }

    abstract class NodeRender extends JPanel {
		private static final long serialVersionUID = 1L;
		private int selectedCount = 0;
		
		public NodeRender(final Node node, boolean showExcludeButton, final boolean isSelected, final int pathStationIndex) {
			super(new GridBagLayout());
			final JComponent button;
    		if (node.table.equals(source) || node.table.equals(destination)) {
    			button = new JLabel(dataModel.getDisplayName(node.table));
    		} else {
	    		JToggleButton tButton = new JToggleButton(dataModel.getDisplayName(node.table));
	    		tButton.setSelected(isSelected);
	    		tButton.setForeground(Color.BLACK);
	    		tButton.addActionListener(new ActionListener() {
    				@Override
    				public void actionPerformed(ActionEvent e) {
    					if (isSelected) {
    						pathStations.remove(pathStationIndex);
    					} else {
    						pathStations.add(pathStationIndex, node.table);
    					}
    					showGraph(false);
    				}
				});
	    		button = tButton;
			}
    		setBackground(null);
    		setOpaque(false);
    		
    		GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
    		gridBagConstraints.gridx = 0;
    		gridBagConstraints.gridy = 0;
    		gridBagConstraints.fill = GridBagConstraints.BOTH;
    		gridBagConstraints.insets = new Insets(2, 16, 2, 8);
    		gridBagConstraints.weighty = 1;
    		gridBagConstraints.weightx = 1;
    		add(button, gridBagConstraints);
    		
    		MouseListener selectListener = new MouseListener() {
    			@Override
    			public void mouseReleased(MouseEvent e) {
    			}
    			@Override
    			public void mousePressed(MouseEvent e) {
    			}
				@Override
    			public void mouseClicked(MouseEvent e) {
    			}
    			@Override
    			public void mouseExited(MouseEvent e) {
    				--selectedCount;
    	    		forceRepaint();
    			}
    			@Override
    			public void mouseEntered(MouseEvent e) {
    				++selectedCount;
    	    		forceRepaint();
    			}
    			private void forceRepaint() {
    				if (selectedCount <= 0) {
    					setSelectedNode(null);
        	    		setBackground(null);
        	    		setOpaque(false);
    				} else {
    					setSelectedNode(node);
        	    		setBackground(SELECTED_BG);
        	    		setOpaque(true);
    				}
    				PathFinderView.this.forceRepaint();
    			}
    		};
    		addMouseListener(selectListener);
    		button.addMouseListener(selectListener);
    		
    		SmallButton excludeButton = new SmallButton(scaledCancelIcon) {
				@Override
				protected void onClick() {
					excludedTables.add(node.table);
					showGraph(false);
				}
    		};
    		excludeButton.setToolTipText("Exclude " + dataModel.getDisplayName(node.table) + " from Path");
    		excludeButton.addMouseListener(selectListener);
    		gridBagConstraints = new java.awt.GridBagConstraints();
    		gridBagConstraints.gridx = 1;
    		gridBagConstraints.gridy = 0;
    		gridBagConstraints.fill = GridBagConstraints.NONE;
    		gridBagConstraints.anchor = GridBagConstraints.CENTER;
    		gridBagConstraints.insets = new Insets(2, 0, 2, 16);
    		gridBagConstraints.weighty = 0;
    		gridBagConstraints.weightx = 0;
    		add(excludeButton, gridBagConstraints);

    		if (!showExcludeButton) {
    			excludeButton.setVisible(false);
    		}

    		addMouseListener(new MouseListener() {
    			@Override
    			public void mouseReleased(MouseEvent e) {
    				onClick();
    			}
    			@Override
    			public void mousePressed(MouseEvent e) {
    			}
    			@Override
    			public void mouseExited(MouseEvent e) {
    			}
    			@Override
    			public void mouseEntered(MouseEvent e) {
    			}
				@Override
    			public void mouseClicked(MouseEvent e) {
    				onClick();
    			}
    		});
    	}

		protected abstract void onClick();
    }

    private Node selectedNode = null;
    private Set<Node> relatedToSelectedNode = new HashSet<Node>();
    
	private void setSelectedNode(Node node) {
		for (Node n: relatedToSelectedNode) {
			NodeRender nodeRender = renderPerNode.get(n);
			if (nodeRender != null) {
				nodeRender.setBackground(null);
				nodeRender.setOpaque(false);
			}
		}
		relatedToSelectedNode.clear();
		selectedNode = node;
		if (selectedNode != null) {
			Set<Table> closure = new HashSet<Table>();
			selectedNode.collectPrevClosure(closure);
			closure.remove(selectedNode.table);
			selectedNode.collectNextClosure(closure);
			closure.remove(selectedNode.table);
			for (Table table: closure) {
				Node n = pathGraph.getNode(table);
				if (n != null) {
					relatedToSelectedNode.add(n);
					NodeRender nodeRender = renderPerNode.get(n);
					if (nodeRender != null) {
						nodeRender.setBackground(SELECTED_BG_RELATED);
						nodeRender.setOpaque(true);
					}
				}
			}
		}
	}

    protected void forceRepaint() {
    	int oldDLoc = jSplitPane1.getDividerLocation();
    	jSplitPane1.setDividerLocation(oldDLoc + 1);
    	jSplitPane1.setDividerLocation(oldDLoc);
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
        jSplitPane1 = new javax.swing.JSplitPane();
        jPanel2 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        pathContainerPanel = new javax.swing.JPanel();
        okButton = new javax.swing.JButton();
        okExpandButton = new javax.swing.JButton();
        jPanel3 = new javax.swing.JPanel();
        jScrollPane2 = new javax.swing.JScrollPane();
        exclusionTable = new javax.swing.JTable();
        jLabel1 = new javax.swing.JLabel();
        jPanel4 = new javax.swing.JPanel();
        undoButton = new javax.swing.JButton();
        redoButton = new javax.swing.JButton();
        jSeparator1 = new javax.swing.JSeparator();

        setLayout(new java.awt.GridBagLayout());

        jPanel1.setLayout(new java.awt.GridBagLayout());

        jSplitPane1.setResizeWeight(1.0);

        jPanel2.setBorder(javax.swing.BorderFactory.createTitledBorder("Path"));
        jPanel2.setForeground(java.awt.Color.white);
        jPanel2.setOpaque(false);
        jPanel2.setLayout(new java.awt.GridBagLayout());

        pathContainerPanel.setBackground(java.awt.Color.white);
        pathContainerPanel.setLayout(new java.awt.BorderLayout());
        jScrollPane1.setViewportView(pathContainerPanel);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel2.add(jScrollPane1, gridBagConstraints);

        okButton.setText("Show Path");
        okButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                okButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        jPanel2.add(okButton, gridBagConstraints);

        okExpandButton.setText("Show Path and open Tables");
        okExpandButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                okExpandButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        jPanel2.add(okExpandButton, gridBagConstraints);

        jSplitPane1.setLeftComponent(jPanel2);

        jPanel3.setBorder(javax.swing.BorderFactory.createTitledBorder("Excluded Tables"));
        jPanel3.setLayout(new java.awt.GridBagLayout());

        exclusionTable.setModel(new javax.swing.table.DefaultTableModel(
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
        jScrollPane2.setViewportView(exclusionTable);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel3.add(jScrollPane2, gridBagConstraints);

        jLabel1.setText("                                                                                                 ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        jPanel3.add(jLabel1, gridBagConstraints);

        jSplitPane1.setRightComponent(jPanel3);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel1.add(jSplitPane1, gridBagConstraints);

        jPanel4.setLayout(new java.awt.GridBagLayout());

        undoButton.setText("jButton1");
        undoButton.setFocusable(false);
        undoButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        undoButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        undoButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                undoButtonActionPerformed(evt);
            }
        });
        jPanel4.add(undoButton, new java.awt.GridBagConstraints());

        redoButton.setText("jButton1");
        redoButton.setFocusable(false);
        redoButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        redoButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        redoButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                redoButtonActionPerformed(evt);
            }
        });
        jPanel4.add(redoButton, new java.awt.GridBagConstraints());

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(jPanel4, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(jSeparator1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(jPanel1, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    private void okButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_okButtonActionPerformed
        applyPath(result, false);
    }//GEN-LAST:event_okButtonActionPerformed

    private void okExpandButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_okExpandButtonActionPerformed
        applyPath(result, true);
    }//GEN-LAST:event_okExpandButtonActionPerformed

    private void undoButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_undoButtonActionPerformed
        if (!undoStack.isEmpty()) {
        	State state = new State(pathStations, excludedTables);
        	redoStack.push(state);
        	state = undoStack.pop();
        	excludedTables = state.excludedTables;
        	pathStations = state.pathStations;
        	showGraph(true);
        }
    }//GEN-LAST:event_undoButtonActionPerformed

    private void redoButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_redoButtonActionPerformed
        if (!redoStack.isEmpty()) {
        	State state = new State(pathStations, excludedTables);
        	undoStack.push(state);
        	state = redoStack.pop();
        	excludedTables = state.excludedTables;
        	pathStations = state.pathStations;
        	showGraph(true);
        }
    }//GEN-LAST:event_redoButtonActionPerformed

    protected abstract void applyPath(List<Table> path, boolean expand);

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JTable exclusionTable;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JSplitPane jSplitPane1;
    private javax.swing.JButton okButton;
    private javax.swing.JButton okExpandButton;
    private javax.swing.JPanel pathContainerPanel;
    private javax.swing.JButton redoButton;
    private javax.swing.JButton undoButton;
    // End of variables declaration//GEN-END:variables

	private List<Table> exclusionCandidates = new ArrayList<Table>();

	Set<Table> getExcludedTables() {
		return excludedTables;
	}

	private final Color SELECTED_BG = new Color(190, 210, 255);
	private final Color SELECTED_BG_RELATED = new Color(228, 238, 255);

	static private ImageIcon cancelIcon;
	static private ImageIcon leftIcon;
	static private ImageIcon rightIcon;
    private ImageIcon scaledCancelIcon;
    private DefaultTableModel exclusionTableModel;
    
    private void initExclusionTable(Set<Integer> nonExcludablesColumns) {
		exclusionTable.setColumnSelectionAllowed(false);
		exclusionTable.setRowSelectionAllowed(false);
		exclusionTableModel = new DefaultTableModel(new String[] { "Excluded", "Table", "Neighbors" }, 0) {
			@Override
			public boolean isCellEditable(int row, int column) {
				return column == 0;
			}
			
			@Override
			public void setValueAt(Object aValue, int row, int column) {
				super.setValueAt(aValue, row, column);
				if (column == 0) {
					if (Boolean.TRUE.equals(aValue)) {
						excludedTables.add(exclusionCandidates.get(row));
					}
					if (Boolean.FALSE.equals(aValue)) {
						excludedTables.remove(exclusionCandidates.get(row));
					}
					showGraph(false);
				}
			}
			
			@Override
            public Class<?> getColumnClass(int columnIndex) {
				if(columnIndex == 0){
                    return Boolean.class;
                }
				if(columnIndex == 2){
                    return Integer.class;
                }
                return super.getColumnClass(columnIndex);
            }
		};
		final JCheckBox checkBox = new JCheckBox("  ");
		checkBox.setHorizontalAlignment(SwingConstants.RIGHT);
		DefaultCellEditor anEditor = new DefaultCellEditor(checkBox);
		anEditor.setClickCountToStart(1);
		exclusionTable.setDefaultEditor(Boolean.class, anEditor);
		
		exclusionTable.setModel(exclusionTableModel);
		
		final TableCellRenderer defaultTableCellRenderer = exclusionTable.getDefaultRenderer(String.class);
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
					((JLabel) render).setToolTipText(UIUtil.toHTML(String.valueOf(value), 200));
				}
				return render;
			}
		};
		exclusionTable.getColumnModel().getColumn(0).setCellRenderer(renderer);
		exclusionTable.setDefaultRenderer(Object.class, renderer);
		exclusionTable.setAutoCreateRowSorter(true);
		List<SortKey> keys = new ArrayList<SortKey>();
		keys.add(new SortKey(2, SortOrder.DESCENDING));
		exclusionTable.getRowSorter().setSortKeys(keys);
		
		Set<Table> closure = new HashSet<Table>(excludedTables);
		for (int col = 0; ; ++col) {
			List<Node> nodes = pathGraph.getNodes(col);
			if (nodes.isEmpty()) {
				break;
			}
			if (!nonExcludablesColumns.contains(col)) {
				for (Node node: nodes) {
					closure.add(node.table);
				}
			}
		}
		exclusionCandidates.clear();
		exclusionCandidates.addAll(closure);
		exclusionCandidates.remove(source);
		exclusionCandidates.remove(destination);
		updateExclusionTableModel();
    }

	private void updateExclusionTableModel() {
		Collections.sort(exclusionCandidates, new Comparator<Table>() {
			@Override
			public int compare(Table t1, Table t2) {
				return dataModel.getDisplayName(t1).compareTo(dataModel.getDisplayName(t2));
			}
		});
		while (exclusionTableModel.getRowCount() > 0) {
			exclusionTableModel.removeRow(0);
		}
		for (Table table: exclusionCandidates) {
			exclusionTableModel.addRow(new Object[] { excludedTables.contains(table), dataModel.getDisplayName(table), Integer.valueOf(table.associations.size()) });
		}
		
		adjustTableColumnsWidth(exclusionTable);
	}

    public void adjustTableColumnsWidth(JTable table) {
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
			if (i == 0) {
				column.setWidth(column.getPreferredWidth());
			}
		}
	}

    static {
        String dir = "/net/sf/jailer/ui/resource";
        
        // load images
        try {
            cancelIcon = new ImageIcon(PathFinderView.class.getResource(dir + "/Cancel2.png"));
            rightIcon = new ImageIcon(PathFinderView.class.getResource(dir + "/right.png"));
            leftIcon = new ImageIcon(PathFinderView.class.getResource(dir + "/left.png"));
        } catch (Exception e) {
            // ignore
        }
    }

    private static final long serialVersionUID = 1L;

}
