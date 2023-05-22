/*
 * Copyright 2007 - 2023 Ralf Wisser.
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
import java.awt.Polygon;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.geom.AffineTransform;
import java.awt.geom.Path2D;
import java.awt.geom.Point2D;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.Stack;
import java.util.TreeMap;

import javax.swing.BorderFactory;
import javax.swing.DefaultCellEditor;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.JTable;
import javax.swing.JToggleButton;
import javax.swing.RowSorter.SortKey;
import javax.swing.SortOrder;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;
import javax.swing.border.TitledBorder;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.Environment;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.pathfinder.PathGraph.EdgeType;
import net.sf.jailer.ui.pathfinder.PathGraph.Node;
import net.sf.jailer.ui.scrollmenu.JScrollPopupMenu;
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
    private boolean hasRestrictions;
    private final Set<Table> sourceClosure;
    
	private List<Table> pathStations  = new ArrayList<Table>();
	private Set<Table> excludedTables = new HashSet<Table>();
	
	class State {
		final List<Table> pathStations;
		final Set<Table> excludedTables;

		State(List<Table> pathStations, Set<Table> excludedTables) {
			this.pathStations = new ArrayList<Table>(pathStations);
			this.excludedTables = new HashSet<Table>(excludedTables);
		}
	}
	
	private State lastState = null;
	private Stack<State> undoStack = new Stack<State>();
	private Stack<State> redoStack = new Stack<State>();

    /**
     * Creates new form PathFinderView
     * @param destination 
     * @param source 
     * @param fromHistory 
     */
    public PathFinderView(DataModel dataModel, Table source, Table destination, boolean withExpandButton, boolean fromHistory) {
    	this.dataModel = dataModel;
    	this.source = source;
    	this.destination = destination;
    	this.sourceClosure = source.closure();
        initComponents();

        redDotIconScaled = UIUtil.scaleIcon(this, redDotIcon);
    	blueDotIconScaled = UIUtil.scaleIcon(this, blueDotIcon);
    	greenDotIconScaled = UIUtil.scaleIcon(this, greenDotIcon);
    	greyDotIconScaled = UIUtil.scaleIcon(this, greyDotIcon);

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
        
        if (jScrollPane1.getHorizontalScrollBar() != null) {
        	jScrollPane1.getHorizontalScrollBar().setUnitIncrement(16);
        }
        if (jScrollPane1.getVerticalScrollBar() != null) {
        	jScrollPane1.getVerticalScrollBar().setUnitIncrement(16);
        }
        restoreHistory();
		historyButton.setEnabled(false);
        if (getHistory(source, destination, dataModel) != null) {
        	historyButton.setEnabled(true);
        	if (fromHistory) {
        		restoreFromHistory(false);
        	}
        }
        hasRestrictions = false;
        for (Table table: dataModel.getTables()) {
        	for (Association a: table.associations) {
        		if (a.isIgnored()) {
        			hasRestrictions = true;
        			break;
        		}
        	}
        	if (hasRestrictions) {
        		break;
        	}
        }
        if (new PathGraph(dataModel, source, destination, excludedTables, pathStations, true).isEmpty()) {
        	considerRestrictionsCheckbox.setSelected(false);
        }
    }

    public void showGraph(boolean undoing) {
    	State state = new State(pathStations, excludedTables);
    	
    	if (!undoing) {
    		if (lastState != null) {
    			undoStack.push(lastState);
    			redoStack.clear();
    		}
    	}

    	this.pathGraph = new PathGraph(dataModel, source, destination, excludedTables, pathStations, considerRestrictionsCheckbox.isSelected());
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
							} else if (PathGraph.EdgeType.IGNORED.equals(type)) {
								color = new Color(100, 100, 100);
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
				double border = 0.4;
				double f = (1.0 - 2.0 * border) / 20;
				int midX = (int) (start.getX() + ((end.getX() - start.getX()) * (border + f)));
				f = 0.25 * f * (end.getY() - start.getY());
				path.moveTo(end.getX(), end.getY());
				path.curveTo(midX, end.getY(), midX, start.getY() + f, start.getX(), start.getY());
				g2d.draw(path);
				
				Polygon arrowHead = new Polygon();
				double ws = 0.4;
				double hs = 2.0 / 3.0;
				double w = 3, h = w;
				arrowHead.addPoint(0, 0);
				arrowHead.addPoint((int) (ws * -w), (int) (hs * (-h)));
				// m_arrowHead.addPoint(0, (int) (hs * (-2 * h)));
				arrowHead.addPoint((int) (ws * w), (int) (hs * (-h)));
				arrowHead.addPoint(0, 0);

				AffineTransform at = getArrowTrans(new Point2D.Double(midX, start.getY()), new Point2D.Double(start.getX() + 4, start.getY()), 6);
				Shape m_curArrow = at.createTransformedShape(arrowHead);

				g2d.fill(m_curArrow);
				g2d.draw(m_curArrow);
			}
			protected AffineTransform getArrowTrans(Point2D p1, Point2D p2, double width) {
				AffineTransform m_arrowTrans = new AffineTransform();
				int o = 1;
				m_arrowTrans.setToTranslation(p2.getX() + o, p2.getY());
				m_arrowTrans.rotate(-Math.PI / 2.0 + Math.atan2(p2.getY() - p1.getY(), p2.getX() + o - p1.getX()));
				if (width > 1) {
					double scalar = width / 2;
					m_arrowTrans.scale(scalar, scalar);
				}
				return m_arrowTrans;
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
    		if (nodes.size() == 1) {
    			Node node = nodes.get(0);
	    		if (node.table.equals(source) || node.table.equals(destination)) {
	    			showExcludeButton = false;
	    		} else {
	    			Set<Table> excl = new HashSet<Table>(excludedTables);
	    			excl.add(node.table);
	    			if (new PathGraph(dataModel, source, destination, excl, pathStations, considerRestrictionsCheckbox.isSelected(), true).isEmpty()) {
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
	    			if (new PathGraph(dataModel, source, destination, excl, pathStations, considerRestrictionsCheckbox.isSelected(), true).isEmpty()) {
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
				NodeRender nodeRender = new NodeRender(node, showExcludeButton, newPathStations.contains(node.table), pathStationIndex);
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
    	initMenu();
    	
    	forceRepaint();
    	
    	lastState = state;
    	undoButton.setEnabled(!undoStack.isEmpty());
    	redoButton.setEnabled(!redoStack.isEmpty());
    }

    private void initMenu() {
    	 resetButton.setEnabled(!pathStations.isEmpty() || !excludedTables.isEmpty());
    	 considerRestrictionsCheckbox.setEnabled(hasRestrictions 
        		&& (considerRestrictionsCheckbox.isSelected() || !new PathGraph(dataModel, source, destination, excludedTables, pathStations, true).isEmpty()));
	}

	class NodeRender extends JPanel {
		private static final long serialVersionUID = 1L;
		private int selectedCount = 0;
		
		public NodeRender(final Node node, boolean showExcludeButton, final boolean isSelected, final int pathStationIndex) {
			super(new GridBagLayout());
			final JComponent button;
    		if (node.table.equals(source) || node.table.equals(destination)) {
    			button = new JLabel(dataModel.getDisplayName(node.table));
	    		button.setForeground(sourceClosure.contains(node.table)? Color.BLACK : COLOR_NOT_IN_CLOSURE);
    		} else {
	    		JToggleButton tButton = new JToggleButton(dataModel.getDisplayName(node.table));
	    		tButton.setSelected(isSelected);
	    		tButton.setForeground(sourceClosure.contains(node.table)? Color.BLACK : COLOR_NOT_IN_CLOSURE);
	    		final ArrayList<Table> nPathStations = new ArrayList<Table>(pathStations);
	    		if (isSelected) {
					nPathStations.remove(pathStationIndex);
				} else {
					nPathStations.add(pathStationIndex, node.table);
				}
		        if (new PathGraph(dataModel, source, destination, excludedTables, nPathStations, considerRestrictionsCheckbox.isSelected()).isEmpty()) {
		        	tButton.setEnabled(false);
		        }
	    		tButton.addActionListener(new ActionListener() {
    				@Override
    				public void actionPerformed(ActionEvent e) {
    					pathStations = nPathStations;
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
				protected void onClick(MouseEvent e) {
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

    		MouseListener mouseListener = new MouseListener() {
    			@Override
    			public void mouseReleased(MouseEvent e) {
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
					if (!SwingUtilities.isRightMouseButton(e) && !node.table.equals(source) || node.table.equals(destination)) {
						return;
					}
					JPopupMenu popup;
					try {
						UIUtil.setWaitCursor(PathFinderView.this);
						popup = createPopupMenu();
					} finally {
						UIUtil.resetWaitCursor(PathFinderView.this);
					}
					if (popup != null) {
						Point p = SwingUtilities.convertPoint(e.getComponent(), e.getX(), e.getY(), button);
						if (e.getComponent() == null) {
							p = new Point(e.getX(), e.getY());
						}
						UIUtil.showPopup(button, (int) p.getX(), (int) p.getY(), popup);
					}
    			}
				private JPopupMenu createPopupMenu() {
					Map<String, PathGraph.EdgeType> following = new TreeMap<String, PathGraph.EdgeType>();
					Set<String> disablesTables = new HashSet<String>();
					for (Association a: node.table.associations) {
						EdgeType pType = following.get(dataModel.getDisplayName(a.destination));
						EdgeType type;
						if (a.isIgnored()) {
							type = EdgeType.IGNORED;
						} else if (a.isInsertDestinationBeforeSource()) {
							type = EdgeType.PARENT;
						} else if (a.isInsertSourceBeforeDestination()) {
							type = EdgeType.CHILD;
						} else {
							type = EdgeType.ASSOCIATION;
						}
						if (pType != null && pType != type) {
							type = EdgeType.ASSOCIATION;
						}
						following.put(dataModel.getDisplayName(a.destination), type);

				        if (source.equals(a.destination) || destination.equals(a.destination) /* || excludedTables.contains(a.destination) */
				        		|| pathStations.contains(a.destination)
				        		|| new PathGraph(dataModel, source, destination, nextExcludedTables(a.destination), nextPathStations(a.destination), considerRestrictionsCheckbox.isSelected(), true).getNode(a.destination) == null) {
				        	disablesTables.add(dataModel.getDisplayName(a.destination));
				        }
					}

					JPopupMenu popupMenu = new JScrollPopupMenu();
					Border titleUnderline = BorderFactory.createMatteBorder(0, 0, 0, 0, popupMenu.getForeground());
					TitledBorder labelBorder = BorderFactory.createTitledBorder(
							titleUnderline, "Successor of " + dataModel.getDisplayName(node.table), TitledBorder.LEFT, TitledBorder.ABOVE_TOP, popupMenu.getFont(), popupMenu.getForeground());
					popupMenu.setBorder(BorderFactory.createCompoundBorder(popupMenu.getBorder(), labelBorder));
					popupMenu.add(new JSeparator());
					
					for (Entry<String, EdgeType> e: following.entrySet()) {
						JMenuItem item = new JMenuItem(e.getKey() + "              ");
						final Table dest = dataModel.getTableByDisplayName(e.getKey());
						popupMenu.add(item);
						if (e.getValue() == EdgeType.PARENT) {
							item.setIcon(redDotIconScaled);
						} else if (e.getValue() == EdgeType.CHILD) {
							item.setIcon(greenDotIconScaled);
						} else if (e.getValue() == EdgeType.IGNORED) {
							item.setIcon(greyDotIconScaled);
						} else {
							item.setIcon(blueDotIconScaled);
						}
						if (disablesTables.contains(e.getKey())) {
							item.setEnabled(false);
						} else {
							item.addActionListener(new ActionListener() {
								@Override
								public void actionPerformed(ActionEvent e) {
									excludedTables = nextExcludedTables(dest);
									pathStations = nextPathStations(dest);
			    					showGraph(false);
								}
							});
						}
					}
					return popupMenu;
				}

				private List<Table> nextPathStations(Table destination) {
					List<Table> nPathStations = new ArrayList<Table>(pathStations);
					if (!nPathStations.contains(node.table)) {
						nPathStations.add(pathStationIndex, node.table);
					}
					nPathStations.remove(destination);
					int in = nPathStations.indexOf(node.table);
					if (in >= 0) {
						nPathStations.add(in + 1, destination);
					}
					return nPathStations;
				}

				private Set<Table> nextExcludedTables(Table dest) {
					Set<Table> nExpludedTables = new HashSet<Table>(excludedTables);
					nExpludedTables.remove(dest);
					return nExpludedTables;
				}
    		};

    		addMouseListener(mouseListener);
    		button.addMouseListener(mouseListener);
    		excludeButton.addMouseListener(mouseListener);
    	}
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
        okExpandButton = new javax.swing.JButton();
        okButton = new javax.swing.JButton();
        sepLabel = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        jPanel3 = new javax.swing.JPanel();
        jScrollPane2 = new javax.swing.JScrollPane();
        exclusionTable = new javax.swing.JTable();
        jLabel1 = new javax.swing.JLabel();
        jPanel4 = new javax.swing.JPanel();
        undoButton = new javax.swing.JButton();
        redoButton = new javax.swing.JButton();
        jSeparator2 = new javax.swing.JSeparator();
        historyButton = new javax.swing.JButton();
        jSeparator3 = new javax.swing.JSeparator();
        considerRestrictionsCheckbox = new javax.swing.JCheckBox();
        resetButton = new javax.swing.JButton();
        jPanel5 = new javax.swing.JPanel();

        setLayout(new java.awt.GridBagLayout());

        jPanel1.setLayout(new java.awt.GridBagLayout());

        jSplitPane1.setResizeWeight(1.0);
        jSplitPane1.setContinuousLayout(true);

        jPanel2.setBorder(javax.swing.BorderFactory.createTitledBorder("Union of all shortest paths without excluded tables."));
        jPanel2.setForeground(java.awt.Color.white);
        jPanel2.setOpaque(false);
        jPanel2.setLayout(new java.awt.GridBagLayout());

        pathContainerPanel.setBackground(java.awt.Color.white);
        pathContainerPanel.setLayout(new java.awt.BorderLayout());
        jScrollPane1.setViewportView(pathContainerPanel);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel2.add(jScrollPane1, gridBagConstraints);

        okExpandButton.setText("Show Path and open Tables");
        okExpandButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                okExpandButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        jPanel2.add(okExpandButton, gridBagConstraints);

        okButton.setText("Show Path");
        okButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                okButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 2);
        jPanel2.add(okButton, gridBagConstraints);

        sepLabel.setText("                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.gridwidth = 3;
        jPanel2.add(sepLabel, gridBagConstraints);

        jLabel2.setForeground(java.awt.Color.gray);
        jLabel2.setText("Open context menu (right mouse click on table) to define its direct successor.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel2.add(jLabel2, gridBagConstraints);

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

        jSeparator2.setOrientation(javax.swing.SwingConstants.VERTICAL);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.VERTICAL;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 8);
        jPanel4.add(jSeparator2, gridBagConstraints);

        historyButton.setText("From History");
        historyButton.setToolTipText("Take the previously determined path.");
        historyButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                historyButtonActionPerformed(evt);
            }
        });
        jPanel4.add(historyButton, new java.awt.GridBagConstraints());

        jSeparator3.setOrientation(javax.swing.SwingConstants.VERTICAL);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 5;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.VERTICAL;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 8);
        jPanel4.add(jSeparator3, gridBagConstraints);

        considerRestrictionsCheckbox.setSelected(true);
        considerRestrictionsCheckbox.setText("Consider restrictions");
        considerRestrictionsCheckbox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                considerRestrictionsCheckboxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 8);
        jPanel4.add(considerRestrictionsCheckbox, gridBagConstraints);

        resetButton.setText("Reset");
        resetButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                resetButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 6;
        gridBagConstraints.gridy = 0;
        jPanel4.add(resetButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(jPanel4, gridBagConstraints);

        jPanel5.setBorder(javax.swing.BorderFactory.createEtchedBorder());
        jPanel5.setPreferredSize(new java.awt.Dimension(1, 2));
        jPanel5.setLayout(null);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(jPanel5, gridBagConstraints);

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
    	putIntoHistory(source, destination, pathStations, result, excludedTables, true);
    	persistHistory();
    }//GEN-LAST:event_okButtonActionPerformed

    private void okExpandButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_okExpandButtonActionPerformed
        applyPath(result, true);
    	putIntoHistory(source, destination, pathStations, result, excludedTables, true);
    	persistHistory();
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

    private void historyButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_historyButtonActionPerformed
        restoreFromHistory(true);
    }//GEN-LAST:event_historyButtonActionPerformed

	private void restoreFromHistory(boolean show) {
		HistoryItem historyItem = getHistory(source, destination, dataModel);
        if (historyItem != null) {
        	List<Table> hPathStations = toTableList(historyItem.pathStations, dataModel);
        	List<Table> hPath = toTableList(historyItem.path, dataModel);
	        Set<Table> hExcludedTables = new HashSet<Table>(toTableList(historyItem.excludedTables, dataModel));

	        excludedTables = hExcludedTables;
	        if (new PathGraph(dataModel, source, destination, hExcludedTables, hPathStations, true).isUnique()) {
		        pathStations = hPathStations;
	        } else {
		        pathStations = hPath;
	        }

	        if (new PathGraph(dataModel, source, destination, excludedTables, pathStations, true).isEmpty()) {
	        	considerRestrictionsCheckbox.setSelected(false);
	        }

	        showGraph(false);
        }
        historyButton.setEnabled(false);
	}

    private void resetButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_resetButtonActionPerformed
    	 if (!pathStations.isEmpty() || !excludedTables.isEmpty()) {
    		 pathStations.clear();
    		 excludedTables.clear();
    		 if (new PathGraph(dataModel, source, destination, excludedTables, pathStations, true).isEmpty()) {
    			 considerRestrictionsCheckbox.setSelected(false);
    		 }
    		 showGraph(false);
    	 }
    }//GEN-LAST:event_resetButtonActionPerformed

    private void considerRestrictionsCheckboxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_considerRestrictionsCheckboxActionPerformed
    	showGraph(false);
    }//GEN-LAST:event_considerRestrictionsCheckboxActionPerformed

    protected abstract void applyPath(List<Table> path, boolean expand);

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JCheckBox considerRestrictionsCheckbox;
    private javax.swing.JTable exclusionTable;
    private javax.swing.JButton historyButton;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JSeparator jSeparator2;
    private javax.swing.JSeparator jSeparator3;
    private javax.swing.JSplitPane jSplitPane1;
    private javax.swing.JButton okButton;
    private javax.swing.JButton okExpandButton;
    private javax.swing.JPanel pathContainerPanel;
    private javax.swing.JButton redoButton;
    private javax.swing.JButton resetButton;
    javax.swing.JLabel sepLabel;
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
    	List<SortKey> keys = new ArrayList<SortKey>();
    	if (exclusionTable.getRowSorter() != null) {
    		keys.addAll(exclusionTable.getRowSorter().getSortKeys());
    	}
    	if (keys.isEmpty()) {
    		keys.add(new SortKey(2, SortOrder.DESCENDING));
    	}
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
			final Color BG1 = UIUtil.TABLE_BACKGROUND_COLOR_1;
			final Color BG2 = UIUtil.TABLE_BACKGROUND_COLOR_2;

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
		exclusionTable.getColumnModel().getColumn(2).setCellRenderer(renderer);
		exclusionTable.setDefaultRenderer(Object.class, renderer);
		exclusionTable.setAutoCreateRowSorter(true);
		exclusionTable.setShowHorizontalLines(false);
		exclusionTable.setShowVerticalLines(false);
		exclusionTable.getRowSorter().setSortKeys(keys);
		
		// Set<Table> closure = new HashSet<Table>(pathGraph.getVisitedExcludedTables());
		Set<Table> exCand = new HashSet<Table>(excludedTables);
		for (int col = 0; ; ++col) {
			List<Node> nodes = pathGraph.getNodes(col);
			if (nodes.isEmpty()) {
				break;
			}
			if (!nonExcludablesColumns.contains(col)) {
				for (Node node: nodes) {
					exCand.add(node.table);
				}
			}
		}
		exclusionCandidates.clear();
		exclusionCandidates.addAll(exCand);
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

	private static final String HISTORY_FILE = ".pathhistory";
	private static final int MAX_HISTORY_SIZE = 400;

	private static List<HistoryItem> history = new LinkedList<HistoryItem>();

	private static HistoryItem getHistory(Table source, Table destination, DataModel dataModel) {
    	for (HistoryItem item: history) {
    		if (source.getName().equals(item.source) && destination.getName().equals(item.destination)) {
    			if (item.isValid()) {
    				return item;
    			}
    		}
    	}
    	return null;
	}

	public static List<Table> getHistoricDestinations(Table source, DataModel dataModel) {
		restoreHistory();
		List<Table> result = new ArrayList<Table>();
    	for (HistoryItem item: history) {
    		if (item.isValid()) {
	    		if (source.getName().equals(item.source)) {
	    			Table table = dataModel.getTable(item.destination);
	    			if (table != null) {
	    				result.add(table);
	    			}
	    		}
    		}
    	}
    	return result;
	}

	private static List<Table> toTableList(Collection<String> list, DataModel dataModel) {
    	List<Table> result = new ArrayList<Table>();
    	for (String tName: list) {
    		Table table = dataModel.getTable(tName);
    		if (table != null) {
    			result.add(table);
    		}
    	}
    	return result;
    }

    private static void putIntoHistory(Table source, Table destination, List<Table> pathStations, List<Table> path, Set<Table> excl, boolean withReversal) {
    	for (Iterator<HistoryItem> i = history.iterator(); i.hasNext(); ) {
    		HistoryItem item = i.next();
    		if (source.getName().equals(item.source) && destination.getName().equals(item.destination)) {
        		if (!item.implicit && !withReversal) {
        			return;
        		}
        		i.remove();
    		}
    	}
    	HistoryItem historyItem = new HistoryItem();
    	historyItem.source = source.getName();
    	historyItem.destination = destination.getName();
    	historyItem.path = new ArrayList<String>();
    	historyItem.pathStations = new ArrayList<String>();
    	historyItem.excludedTables = new HashSet<String>();
    	for (Table table: path) {
    		historyItem.path.add(table.getName());
    	}
    	for (Table table: pathStations) {
    		historyItem.pathStations.add(table.getName());
    	}
    	for (Table table: excl) {
    		historyItem.excludedTables.add(table.getName());
    	}
    	history.add(0, historyItem);
    	if (history.size() > MAX_HISTORY_SIZE) {
    		history.remove(history.size() - 1);
    	}
    	
    	if (withReversal) {
    		List<Table> rPath = new ArrayList<Table>(path);
			List<Table> rPathStations = new ArrayList<Table>(pathStations);
			Collections.reverse(rPath);
			Collections.reverse(rPathStations);
			putIntoHistory(destination, source, rPathStations, rPath, excl, false);
    	}
    }

    private static void persistHistory() {
		try {
			File file = Environment.newFile(HISTORY_FILE);
			ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(file)); // lgtm [java/output-resource-leak]
			out.writeObject(history);
			out.close();
		} catch (Exception e) {
			e.printStackTrace();
		}

    }

    private static boolean historyRestored = false;

    private static void restoreHistory() {
    	if (!historyRestored) {
    		historyRestored = true;
    		try {
    			File file = Environment.newFile(HISTORY_FILE);
    			if (file.exists()) {
    				ObjectInputStream in = new ObjectInputStream(new FileInputStream(file)); // lgtm [java/input-resource-leak]
    				history = (List<HistoryItem>) in.readObject();
    				in.close();
    			}
    		} catch (Exception e) {
    			// ignore
    		}
    	}
    }

    public void initFocus() {
        if (okExpandButton.isVisible() && okExpandButton.isEnabled()) {
        	okExpandButton.grabFocus();
        }
    }

	private static final Color COLOR_NOT_IN_CLOSURE = new Color(255, 80, 80);

	static {
        // load images
        cancelIcon = UIUtil.readImage("/Cancel2.png");
        rightIcon = UIUtil.readImage("/right.png");
        leftIcon = UIUtil.readImage("/left.png");
    }

	private ImageIcon redDotIconScaled;
	private ImageIcon blueDotIconScaled;
	private ImageIcon greenDotIconScaled;
	private ImageIcon greyDotIconScaled;
	
	private static ImageIcon redDotIcon;
	private static ImageIcon blueDotIcon;
	private static ImageIcon greenDotIcon;
	private static ImageIcon greyDotIcon;
	static {
		// load images
		redDotIcon = UIUtil.readImage("/reddot.gif");
		blueDotIcon = UIUtil.readImage("/bluedot.gif");
		greenDotIcon = UIUtil.readImage("/greendot.gif");
		greyDotIcon = UIUtil.readImage("/greydot.gif");
	}

    private static final long serialVersionUID = 1L;

}

// TODO
// TODO fehler: pathfinder öffnet GeschPartner nicht, wenn schon sichtbar, aber anderer Pfad
