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
package net.sf.jailer.ui.graphical_view;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.SwingUtilities;

import net.sf.jailer.ScriptFormat;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.ExtractionModelEditor;
import prefuse.Display;
import prefuse.Visualization;
import prefuse.action.ActionList;
import prefuse.action.RepaintAction;
import prefuse.action.assignment.ColorAction;
import prefuse.action.filter.GraphDistanceFilter;
import prefuse.action.layout.graph.ForceDirectedLayout;
import prefuse.activity.Activity;
import prefuse.controls.Control;
import prefuse.controls.DragControl;
import prefuse.controls.FocusControl;
import prefuse.controls.PanControl;
import prefuse.controls.ToolTipControl;
import prefuse.controls.WheelZoomControl;
import prefuse.controls.ZoomToFitControl;
import prefuse.data.Edge;
import prefuse.data.Graph;
import prefuse.data.Node;
import prefuse.data.Schema;
import prefuse.data.Tuple;
import prefuse.data.event.TupleSetListener;
import prefuse.data.tuple.TupleSet;
import prefuse.render.Renderer;
import prefuse.render.RendererFactory;
import prefuse.render.ShapeRenderer;
import prefuse.util.ColorLib;
import prefuse.util.GraphicsLib;
import prefuse.util.display.DisplayLib;
import prefuse.util.force.DragForce;
import prefuse.util.force.Force;
import prefuse.util.force.ForceItem;
import prefuse.util.force.ForceSimulator;
import prefuse.util.force.Integrator;
import prefuse.util.force.NBodyForce;
import prefuse.util.ui.UILib;
import prefuse.visual.EdgeItem;
import prefuse.visual.NodeItem;
import prefuse.visual.VisualGraph;
import prefuse.visual.VisualItem;

/**
 * Graphical restriction model view and editor.
 * 
 * @author Ralf Wisser
 */
public class GraphicalDataModelView extends JPanel {

	/**
	 * Maximum number of tables to make visible during expansion.
	 */
	private final int EXPAND_LIMIT = 50;
	
	/**
	 * The selected association.
	 */
	Association selectedAssociation;

	/**
	 * Set of names of all tables on path from selected one to the root.
	 */
	Set<String> tablesOnPath = new HashSet<String>();
	
	// constants
    private static final String graph = "graph";
    private static final String nodes = "graph.nodes";
    private static final String edges = "graph.edges";

    // prefuse visualization
    private Visualization visualization;
    private VisualGraph visualGraph;
    public Display display;
    private boolean inInitialization = false;
    
    /**
     * The root table.
     */
    final Table root;
    
    /**
     * Table renderer.
     */
    private TableRenderer tableRenderer;
    
    /**
     * Association renderer.
     */
    private CompositeAssociationRenderer associationRenderer;

    /**
     * The enclosing model editor.
     */
    public final ExtractionModelEditor modelEditor;

    /**
     * Layouts the model graph.
     */
    private ForceDirectedLayout layout;
    
    /**
     * Zooms to fit on mouse click.
     */
    private ZoomToFitControlExtension zoomToFitControl;
    
    /**
     * Table details mode.
     */
    private boolean showTableDetails;

    private NBodyForce force;
    private DragForce dragForce;
    
    /**
     * Constructor.
     * 
     * @param model the restricted data model
     * @param modelEditor enclosing model editor
     * @param width
     * @param height initial size
     */
    public GraphicalDataModelView(final DataModel model, ExtractionModelEditor modelEditor, Table subject, int width, int height) {
    	super(new BorderLayout());
    	this.model = model;
    	this.modelEditor = modelEditor;
    	this.root = subject;
    	
        tableRenderer = new TableRenderer(model, this);
    	theGraph = getModelGraph(model);
    	
        // create a new, empty visualization for our data
        visualization = new Visualization();
        
        final ZoomBoxControl zoomBoxControl = new ZoomBoxControl();
		
        // --------------------------------------------------------------------
        // set up the renderers
        
        final ShapeRenderer sr = new ShapeRenderer() {
        	protected Shape getRawShape(VisualItem item) {
        		item.setFillColor(ColorLib.rgb(220,210,0));
                double x = item.getX();
                if ( Double.isNaN(x) || Double.isInfinite(x) )
                    x = 0;
                double y = item.getY();
                if ( Double.isNaN(y) || Double.isInfinite(y) )
                    y = 0;
                double width = 14 * item.getSize();
                
                // Center the shape around the specified x and y
                if ( width > 1 ) {
                    x = x-width/2;
                    y = y-width/2;
                }
                return ellipse((float) x, (float) y, (float) width, (float) width);
        	}
        };
        
        associationRenderer = new CompositeAssociationRenderer();
        
        tableRenderer.setRoundedCorner(3, 3);
        tableRenderer.setVerticalPadding(3);
        tableRenderer.setHorizontalPadding(3);
        visualization.setRendererFactory(new RendererFactory() {
			public Renderer getRenderer(VisualItem item) {
				if (zoomBoxControl.getRenderer().isBoxItem(item)) {
					return zoomBoxControl.getRenderer();
				}
				if (item instanceof EdgeItem) {
					return associationRenderer;
				}
				if (item.get("association") != null) {
					return sr;
				}
				return tableRenderer;
			}
        });

        // adds graph to visualization and sets renderer label field
        setGraph(theGraph);
        
        // fix selected focus nodes
        TupleSet focusGroup = visualization.getGroup(Visualization.FOCUS_ITEMS); 
        focusGroup.addTupleSetListener(new TupleSetListener() {
            public void tupleSetChanged(TupleSet ts, Tuple[] add, Tuple[] rem)
            {
                boolean draw = false;
                
                if (inInitialization) {
                	return;
                }
            	for (int i=0; i<add.length; ++i ) {
                	if (add[i] instanceof NodeItem) {
                		draw = true;
                		// expandTable(theGraph, model.getTable(add[i].getString("label")));
	    	            ((VisualItem)add[i]).setFixed(false);
	                    ((VisualItem)add[i]).setFixed(true);
                	}
                }
            	if (draw) {
            		//  m_vis.run("draw");
            	}
            }
        });
        
        // --------------------------------------------------------------------
        // create actions to process the visual data

        int hops = 30;
        final GraphDistanceFilter filter = new GraphDistanceFilter(graph, hops);

        ColorAction fill = new ColorAction(nodes, 
                VisualItem.FILLCOLOR, ColorLib.rgba(255,235,20,100));
        fill.add(VisualItem.FIXED, ColorLib.rgba(255,235,20,100));
        fill.add(VisualItem.HIGHLIGHT, ColorLib.rgba(160,160,0,120));
        
        ActionList draw = new ActionList();
        draw.add(filter);
        draw.add(fill);
        draw.add(new ColorAction(nodes, VisualItem.STROKECOLOR, 0));
        draw.add(new ColorAction(nodes, VisualItem.TEXTCOLOR, ColorLib.rgb(0,0,0)));
        draw.add(new ColorAction(edges, VisualItem.FILLCOLOR, ColorLib.gray(200)));
        draw.add(new ColorAction(edges, VisualItem.STROKECOLOR, ColorLib.gray(200)));
        
        ActionList animate = new ActionList(Activity.INFINITY);
        layout = new ForceDirectedLayout(graph) {
        	protected float getMassValue(VisualItem n) {
                return zoomBoxControl.getRenderer().isBoxItem(n)? 0.01f : showTableDetails? 2.0f : 1.0f;
            }
        };
        for (Force force: layout.getForceSimulator().getForces()) {
        	if (force instanceof DragForce) {
        		dragForce = (DragForce) force;
        	}
        	if (force instanceof NBodyForce) {
        		this.force = (NBodyForce) force;
        	}
        }
		if (force != null) {
			force.setParameter(NBodyForce.GRAVITATIONAL_CONST, -100.0f);
		}
        updateTableDetailsMode();
        layout.getForceSimulator().setIntegrator(new Integrator() {
        	public void integrate(ForceSimulator sim, long timestep) {                
        		float speedLimit = sim.getSpeedLimit();
                float vx, vy, v, coeff;
                float[][] k, l;
                
                Iterator iter = sim.getItems();
                while ( iter.hasNext() ) {
                    ForceItem item = (ForceItem)iter.next();
                    coeff = timestep / item.mass;
                    k = item.k;
                    l = item.l;
                    item.plocation[0] = item.location[0];
                    item.plocation[1] = item.location[1];
                    k[0][0] = timestep*item.velocity[0];
                    k[0][1] = timestep*item.velocity[1];
                    l[0][0] = coeff*item.force[0];
                    l[0][1] = coeff*item.force[1];
                
                    // Set the position to the new predicted position
                    item.location[0] += 0.5f*k[0][0];
                    item.location[1] += 0.5f*k[0][1];
                }
                
                // recalculate forces
                sim.accumulate();
                
                iter = sim.getItems();
                while ( iter.hasNext() ) {
                    ForceItem item = (ForceItem)iter.next();
                    coeff = timestep / item.mass;
                    k = item.k;
                    l = item.l;
                    vx = item.velocity[0] + .5f*l[0][0];
                    vy = item.velocity[1] + .5f*l[0][1];
                    v = (float)Math.sqrt(vx*vx+vy*vy);
                    if ( v > speedLimit ) {
                        vx = speedLimit * vx / v;
                        vy = speedLimit * vy / v;
                    }
                    k[1][0] = timestep*vx;
                    k[1][1] = timestep*vy;
                    l[1][0] = coeff*item.force[0];
                    l[1][1] = coeff*item.force[1];
                
                    // Set the position to the new predicted position
                    item.location[0] = item.plocation[0] + 0.5f*k[1][0];
                    item.location[1] = item.plocation[1] + 0.5f*k[1][1];
                }
                
                // recalculate forces
                sim.accumulate();
                
                iter = sim.getItems();
                while ( iter.hasNext() ) {
                    ForceItem item = (ForceItem)iter.next();
                    coeff = timestep / item.mass;
                    k = item.k;
                    l = item.l;
                    vx = item.velocity[0] + .5f*l[1][0];
                    vy = item.velocity[1] + .5f*l[1][1];
                    v = (float)Math.sqrt(vx*vx+vy*vy);
                    if ( v > speedLimit ) {
                        vx = speedLimit * vx / v;
                        vy = speedLimit * vy / v;
                    }
                    k[2][0] = timestep*vx;
                    k[2][1] = timestep*vy;
                    l[2][0] = coeff*item.force[0];
                    l[2][1] = coeff*item.force[1];
                
                    // Set the position to the new predicted position
                    item.location[0] = item.plocation[0] + 0.5f*k[2][0];
                    item.location[1] = item.plocation[1] + 0.5f*k[2][1];
                }
                
                // recalculate forces
                sim.accumulate();
                
                iter = sim.getItems();
                while ( iter.hasNext() ) {
                    ForceItem item = (ForceItem)iter.next();
                    coeff = timestep / item.mass;
                    k = item.k;
                    l = item.l;
                    float[] p = item.plocation;
                    vx = item.velocity[0] + l[2][0];
                    vy = item.velocity[1] + l[2][1];
                    v = (float)Math.sqrt(vx*vx+vy*vy);
                    if ( v > speedLimit ) {
                        vx = speedLimit * vx / v;
                        vy = speedLimit * vy / v;
                    }
                    k[3][0] = timestep*vx;
                    k[3][1] = timestep*vy;
                    l[3][0] = coeff*item.force[0];
                    l[3][1] = coeff*item.force[1];
                    float dx = (k[0][0]+k[3][0])/6.0f + (k[1][0]+k[2][0])/3.0f;
                    float dy = (k[0][1]+k[3][1])/6.0f + (k[1][1]+k[2][1])/3.0f;
                    if (dx*dx+dy*dy < 3) {
                    	dx = dy = 0;
                    }
					item.location[0] = p[0] + dx;
					item.location[1] = p[1] + dy;
                    
                    vx = (l[0][0]+l[3][0])/6.0f + (l[1][0]+l[2][0])/3.0f;
                    vy = (l[0][1]+l[3][1])/6.0f + (l[1][1]+l[2][1])/3.0f;
                    v = (float)Math.sqrt(vx*vx+vy*vy);
                    if ( v > speedLimit ) {
                        vx = speedLimit * vx / v;
                        vy = speedLimit * vy / v;
                    }
                    item.velocity[0] += vx;
                    item.velocity[1] += vy;
                }
            
        	}
        });
        layout.setVisualization(visualization);
        animate.add(layout);
        animate.add(fill);
        animate.add(new RepaintAction());
        
        if (modelEditor.extractionModelFrame.animationStepTime > 0) {
        	animate.setStepTime(modelEditor.extractionModelFrame.animationStepTime);
        }

        // finally, we register our ActionList with the Visualization.
        // we can later execute our Actions by invoking a method on our
        // Visualization, using the name we've chosen below.
        visualization.putAction("draw", draw);
        visualization.putAction("layout", animate);
        
        visualization.runAfter("draw", "layout");
        
        
        // --------------------------------------------------------------------
        // set up a display to show the visualization
        
        display = new Display(visualization);
        display.setSize(width, height);
        display.pan(width / 2, height / 2);
        display.setForeground(Color.GRAY);
        display.setBackground(Color.WHITE);
        
        // main display controls
        display.addControlListener(new FocusControl(1));
        display.addControlListener(new DragControl() {
			@Override
			public void itemClicked(VisualItem item, MouseEvent e) {
				Table table = model.getTable(item.getString("label"));
				if (SwingUtilities.isLeftMouseButton(e)) {
	            	if (table != null && e.getClickCount() == 1) {
	            		GraphicalDataModelView.this.modelEditor.select(table);
	            		Association sa = selectedAssociation;
	            		setSelection(null);
	            		setSelection(sa);
	            	}
                }
            	// context menu
                if (SwingUtilities.isRightMouseButton(e)) {
                	Association association = (Association) item.get("association");
                	if (association != null) {
                		if (Boolean.TRUE.equals(item.get("full"))) {
	            			associationRenderer.useAssociationRendererForLocation = true;
	            			VisualItem findItem = display.findItem(e.getPoint());
							if (findItem == null || !findItem.equals(item)) {
	            				association = association.reversalAssociation;
	            			}
	            			associationRenderer.useAssociationRendererForLocation = false;
	            		}
                		JPopupMenu popup = createPopupMenu(association);
						popup.show(e.getComponent(), e.getX(), e.getY());
                	}
                	if (table != null) {
						JPopupMenu popup = createPopupMenu(table);
						popup.show(e.getComponent(), e.getX(), e.getY());
                	}
                }
                super.itemClicked(item, e);
			}

			public void itemPressed(VisualItem item, MouseEvent e) {
				if (UILib.isButtonPressed(e, LEFT_MOUSE_BUTTON)) {
					Association association = (Association) item.get("association");
	            	if (association != null) {
	            		if (Boolean.TRUE.equals(item.get("full"))) {
	            			associationRenderer.useAssociationRendererForLocation = true;
	            			VisualItem findItem = display.findItem(e.getPoint());
							if (findItem == null || !findItem.equals(item)) {
	            				association = association.reversalAssociation;
	            			}
	            			associationRenderer.useAssociationRendererForLocation = false;
	            		}
	            		setSelection(association);
	            	}
	            	Table table = model.getTable(item.getString("label"));
	            	if (table != null && e.getClickCount() > 1) {
	            		if (expandedTables.contains(table)) {
	            			collapseTable(theGraph, table, false);
	            			display.pan(1, 0);
	            			display.pan(0, 1);
	            			Association sa = selectedAssociation;
		            		setSelection(null);
		            		setSelection(sa);
	            			visualization.invalidateAll();
	            			display.invalidate();
	            		} else {
		    	            expandTable(theGraph, table);
	            			visualization.invalidateAll();
	            			display.invalidate();
	            		}
		            }
				}
            	super.itemPressed(item, e);
			}
			
			public void itemReleased(VisualItem item, MouseEvent e) {
				// fix after drag
				super.itemReleased(item, e);
                if (!SwingUtilities.isLeftMouseButton(e)) return;
                if (item instanceof NodeItem) {
                	item.setFixed(true);
                }
        	}
        });
        display.addControlListener(new PanControl());
        display.addControlListener(zoomBoxControl);
        display.addControlListener(new WheelZoomControl(){
            /**
             * @see java.awt.event.MouseWheelListener#mouseWheelMoved(java.awt.event.MouseWheelEvent)
             */
            public void mouseWheelMoved(MouseWheelEvent e) {
                Display display = (Display)e.getComponent();
                Point p = e.getPoint();
                zoom(display, p,
                     1 + 0.1f * e.getWheelRotation(), false);
            }

        });
        zoomToFitControl = new ZoomToFitControlExtension(Visualization.ALL_ITEMS, 50, 800, Control.RIGHT_MOUSE_BUTTON,	model);
        display.addControlListener(zoomToFitControl);
        display.addControlListener(new ToolTipControl("tooltip"));

        display.setForeground(Color.GRAY);
        display.setBackground(Color.WHITE);
        
        // now we run our action list
        visualization.run("draw");
        layout.run();
        
		resetExpandedState();
		
        display.setHighQuality(true);
        
        add(display);
    }
    
    /**
     * Creates popup menu.
     *
     * @param table the table for which the menu pops up
     * @return the popup menu
     */
	public JPopupMenu createPopupMenu(final Table table) {
		JPopupMenu popup = new JPopupMenu();
		
//		JMenuItem select = new JMenuItem("Select " + table.getName());
//		select.addActionListener(new ActionListener () {
//			public void actionPerformed(ActionEvent e) {
//				modelEditor.select(table);
//			}
//		});
		JMenuItem selectAsRoot = new JMenuItem("Focus " + table.getName());
		selectAsRoot.addActionListener(new ActionListener () {
			public void actionPerformed(ActionEvent e) {
				modelEditor.setRootSelection(table);
			}
		});
		JMenuItem zoomToFit = new JMenuItem("Zoom to fit");
		zoomToFit.addActionListener(new ActionListener () {
			public void actionPerformed(ActionEvent e) {
				zoomToFit();
			}
		});
		JMenuItem hide = new JMenuItem("Hide " + table.getName());
		hide.addActionListener(new ActionListener () {
			public void actionPerformed(ActionEvent e) {
				hideTable(table);
				display.invalidate();
			}
		});
		if (table.equals(root)) {
			hide.setEnabled(false);
		}
		JMenuItem toggleDetails = new JMenuItem(showDetails(table)? "Hide details" : "Show details");
		toggleDetails.addActionListener(new ActionListener () {
			public void actionPerformed(ActionEvent e) {
				if (reversedShowDetailsTables.contains(table)) {
					reversedShowDetailsTables.remove(table);
				} else {
					reversedShowDetailsTables.add(table);
				}
				visualization.invalidateAll();
				display.invalidate();
			}
		});
		JMenuItem mapColumns = new JMenuItem("Map columns");
		mapColumns.addActionListener(new ActionListener () {
			public void actionPerformed(ActionEvent e) {
				modelEditor.openColumnMapper(table);
			}
		});
		mapColumns.setEnabled(ScriptFormat.XML.equals(modelEditor.scriptFormat));
		
		JMenuItem restrictAll = new JMenuItem("Disable associations");
		restrictAll.addActionListener(new ActionListener () {
			public void actionPerformed(ActionEvent e) {
				if (JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(modelEditor.extractionModelFrame, "Disable each association with '" + model.getDisplayName(table) + "'?\n(except dependencies)?", "Add restrictions", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE)) {
		    		modelEditor.ignoreAll(table);
		    	}
			}
		});
		restrictAll.setEnabled(modelEditor.isIgnoreAllApplicable(table));
		
		JMenuItem removeRestrictions = new JMenuItem("Remove restrictions");
		removeRestrictions.addActionListener(new ActionListener () {
			public void actionPerformed(ActionEvent e) {
				if (JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(modelEditor.extractionModelFrame, "Remove all restrictions from associations with '" + model.getDisplayName(table) + "'?", "Remove restrictions", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE)) {
		    		modelEditor.removeAllRestrictions(table);
		    	}
			}
		});
		JMenuItem htmlRender = new JMenuItem("Open HTML render");
		htmlRender.addActionListener(new ActionListener () {
			public void actionPerformed(ActionEvent e) {
				modelEditor.extractionModelFrame.openHTMLRender(table);
			}
		});
//		JMenuItem shortestPath = new JMenuItem("Show shortest path");
//		shortestPath.addActionListener(new ActionListener () {
//			public void actionPerformed(ActionEvent e) {
//				modelEditor.extractionModelFrame.showShortestPath(modelEditor.getSubject(), table);
//			}
//		});
		removeRestrictions.setEnabled(modelEditor.isRemovalOfAllRestrictionsApplicable(table));
		
		JMenuItem findTable = new JMenuItem("Open closure browser");
		findTable.addActionListener(new ActionListener () {
			public void actionPerformed(ActionEvent e) {
				modelEditor.extractionModelFrame.openClosureView(table);
			}
		});

		JMenuItem filterEditor= new JMenuItem("Edit Filters...");
		filterEditor.addActionListener(new ActionListener () {
			public void actionPerformed(ActionEvent e) {
				modelEditor.extractionModelFrame.openFilterEditor(table);
			}
		});

		popup.add(toggleDetails);
		popup.add(new JSeparator());
		popup.add(hide);
		popup.add(selectAsRoot);
		popup.add(findTable);
		popup.add(zoomToFit);
//		popup.add(select);
		popup.add(new JSeparator());
		popup.add(filterEditor);
		popup.add(restrictAll);
		popup.add(removeRestrictions);
		popup.add(new JSeparator());
		popup.add(mapColumns);
//		popup.add(shortestPath);
		popup.add(htmlRender);
		return popup;
	}

    /**
     * Creates popup menu.
     *
     * @param association the association for which the menu pops up
     * @return the popup menu
     */
	public JPopupMenu createPopupMenu(final Association association) {
		JPopupMenu popup = new JPopupMenu();
		
		JMenuItem disable = new JMenuItem("disable association");
		disable.addActionListener(new ActionListener () {
			public void actionPerformed(ActionEvent e) {
				setSelection(association);
				modelEditor.restrictionEditor.ignore.setSelected(true);
				modelEditor.onApply(false);
			}
		});
		JMenuItem enable = new JMenuItem("remove restriction");
		enable.addActionListener(new ActionListener () {
			public void actionPerformed(ActionEvent e) {
				setSelection(association);
				modelEditor.restrictionEditor.ignore.setSelected(false);
				modelEditor.onApply(false);
			}
		});
		JMenuItem zoomToFit = new JMenuItem("Zoom to fit");
		zoomToFit.addActionListener(new ActionListener () {
			public void actionPerformed(ActionEvent e) {
				zoomToFit();
			}
		});

		disable.setEnabled(!association.isIgnored());
		enable.setEnabled(association.isRestricted());
		
		popup.add(disable);
		popup.add(enable);
		popup.add(new JSeparator());
		popup.add(zoomToFit);

		return popup;
	}

	/**
	 * Hides a table.
	 * 
	 * @param table the table to hide
	 */
	protected void hideTable(Table table) {
		synchronized (visualization) {
			collapseTable(theGraph, table, true);
		}
	}

	/**
	 * Closes the view.
	 */
	public void close() {
		visualization.reset();
		layout.cancel();
	}

	/**
	 * Zooms to fit.
	 */
	public void zoomToFit() {
		zoomToFitControl.zoomToFit();
	}

	/**
	 * Sets visual graph.
	 * 
	 * @param g the (non-visual) model graph
	 */
    private void setGraph(Graph g) {
        // update graph
    	inInitialization = true;
        visualization.removeGroup(graph);
        visualGraph = visualization.addGraph(graph, g);
        if (visualGraph.getNodeCount() > 1) {
	        VisualItem f = (VisualItem) visualGraph.getNode(1);
		    visualization.getGroup(Visualization.FOCUS_ITEMS).setTuple(f);
	        f.setFixed(true);
	        ((VisualItem) visualGraph.getNode(0)).setFixed(true);
        }
        
        inInitialization = false;
    }
    
    /**
     * The model as graph.
     */
    private Graph theGraph;
    
    /**
     * Maps tables to their graph nodes.
     */
    private Map<net.sf.jailer.datamodel.Table, Node> tableNodes = new HashMap<net.sf.jailer.datamodel.Table, Node>();

    /**
     * Set of all tables which are currently expanded.
     */
    Set<net.sf.jailer.datamodel.Table> expandedTables = new HashSet<net.sf.jailer.datamodel.Table>();
    
    /**
     * Maps associations to their edges.
     */
    private Map<Association, Edge> renderedAssociations = new HashMap<Association, Edge>();
    private Map<Association, Node> renderedAssociationsAsNode = new HashMap<Association, Node>();

    /**
     * The data model.
     */
    private DataModel model = null;

    /**
     * Sets selected association.
     * 
     * @param association the association to select or <code>null</code> to deselect
     */
    public void setSelection(Association association) {
    	synchronized (visualization) {
	    	if (selectedAssociation == null || association == null || !selectedAssociation.equals(association)) {
	    		if (selectedAssociation != null || association != null) {
	    			selectedAssociation = association;
	    			modelEditor.select(association);
	    			tablesOnPath.clear();
					if (selectedAssociation != null) {
    					List<Association> path = getPathToRoot(selectedAssociation.destination, true);
    					if (path.isEmpty()) {
    						path = getPathToRoot(selectedAssociation.destination, false);
    					}
    					boolean highlightPath = true;
    					if (path.isEmpty()) {
    						highlightPath = false;
    						path = modelEditor.getPathToRoot(selectedAssociation);
    					}
    					for (int i = 0; i < path.size(); ++i) {
    						if (highlightPath) {
    							tablesOnPath.add(path.get(i).source.getName());
    							tablesOnPath.add(path.get(i).destination.getName());
	    					}
    						expandTable(theGraph, path.get(i).source, path.get(i));
    						expandTable(theGraph, path.get(i).destination, path.get(i));
    					}
	    			}
	    			invalidate();
	    		}
	    	}
    	}
    }
    
    /**
     * Gets shortest path from root to a given table.
     * 
     * @param destination the table
     * @param ignoreInvisibleAssociations if <code>true</code>, find a path over visible associations only
     * @return shortest path from root to a given table
     */
    private List<Association> getPathToRoot(Table destination, boolean ignoreInvisibleAssociations) {
    	List<Association> path = new ArrayList<Association>();
    	Map<Table, Table> successor = new HashMap<Table, Table>();
        Map<Table, Association> outgoingAssociation = new HashMap<Table, Association>();
        List<Table> agenda = new ArrayList<Table>();
        agenda.add(destination);
        
        while (!agenda.isEmpty()) {
            Table table = agenda.remove(0);
            for (Association association: incomingAssociations(table)) {
            	if (!ignoreInvisibleAssociations || renderedAssociations.containsKey(association)|| renderedAssociations.containsKey(association.reversalAssociation)) {
	                if (!successor.containsKey(association.source)) {
	                    successor.put(association.source, table);
	                    outgoingAssociation.put(association.source, association);
	                    agenda.add(association.source);
	                    if (association.source.equals(root)) {
	                        agenda.clear();
	                        break;
	                    }
	                }
                }
            }
        }
        if (successor.containsKey(root)) {
            for (Table table = root; !table.equals(destination); table = successor.get(table)) {
                Association association = outgoingAssociation.get(table);
                path.add(association);
            }
        }
        
        return path;
    }

    /**
     * Collects all non-disabled associations with a given table as destination.
     * 
     * @param table the table
     */
    private static Collection<Association> incomingAssociations(Table table) {
        Collection<Association> result = new ArrayList<Association>();
        for (Association association: table.associations) {
            if (association.reversalAssociation.getJoinCondition() != null) {
                result.add(association.reversalAssociation);
            }
        }
        return result;
    }

	/**
     * Gets model as graph.
     * 
     * @param model the data model
     * @return graph
     */
	private Graph getModelGraph(DataModel model) {
		tableNodes = new HashMap<net.sf.jailer.datamodel.Table, Node>();
        expandedTables = new HashSet<net.sf.jailer.datamodel.Table>();
        renderedAssociations = new HashMap<Association, Edge>();
        
		Graph g = new Graph(true);
		Schema s = new Schema();
		s.addColumn("label", String.class);
		s.addColumn("tooltip", String.class);
		s.addColumn("association", Association.class);
		s.addColumn("full", Boolean.class);
		g.addColumns(s);
		
		Node zoomBox = g.addNode();
		zoomBox.setString("label", ZoomBoxControl.BOX_ITEM_LABEL);
		
		Table table = root;
		showTable(g, table);
		
		int nAssociatedTables = 0;
		if (table != null) {
			for (Association a: table.associations) {
				if (isVisualizable(a)) {
					++nAssociatedTables;
				}
			}
		}
		
		if (nAssociatedTables <= 10) {
			expandTable(g, table);
		}
		
    	return g;
	}
	
	/**
	 * Creates visible node for given table.
	 * 
	 * @param graph the graph
	 * @param table the table to show
	 */
	private boolean showTable(Graph graph, Table table) {
		if (table != null && !tableNodes.containsKey(table)) {
			Node n = graph.addNode();
			n.setString("label", table.getName());
			String tooltip = tableRenderer.getToolTip(table);
			n.setString("tooltip", tooltip);
			tableNodes.put(table, n);
			return true;
		}
		return false;
	}
	
	/**
	 * Collapses a node representing a table.
	 * 
	 * @param g the graph
	 * @param table the table node
	 * @param hideTable if <code>true</code>, hide table too
	 */
	private void collapseTable(Graph g, Table table, boolean hideTable) {
		if (table == null || (expandedTables.contains(table) || hideTable)) {
			Set<Association> associationsToKeep = new HashSet<Association>();
			Set<Table> tablesToKeep = new HashSet<Table>();
			
			collect(root, table, associationsToKeep, tablesToKeep);
			if (hideTable && table != null) {
				for (Association a: table.associations) {
					associationsToKeep.remove(a);
					associationsToKeep.remove(a.reversalAssociation);
				}
			}
			for (Table t: model.getTables()) {
				for (Association a: t.associations) {
					if (!associationsToKeep.contains(a)) {
						Edge e = renderedAssociations.get(a);
						if (e != null) {
							g.removeEdge(e);
						}
						renderedAssociations.remove(a);
					}
				}
			}
			for (Table t: model.getTables()) {
				for (Association a: t.associations) {
					if (!associationsToKeep.contains(a)) {
						Node e = renderedAssociationsAsNode.get(a);
						if (e != null) {
							g.removeNode(e);
						}
						renderedAssociationsAsNode.remove(a);
					}
				}
			}
			for (Table t: model.getTables()) {
				if ((t != table || hideTable) && !tablesToKeep.contains(t)) {
					Node n = tableNodes.get(t);
					if (n != null) {
						g.removeNode(n);
					}
					tableNodes.remove(t);
					for (Association a: t.associations) {
						expandedTables.remove(a.source);
						expandedTables.remove(a.destination);
					}
				}
			}
			if (table != null) {
				expandedTables.remove(table);
				if (!hideTable) {
					tablesToKeep.add(table);
				}
			}
			checkForExpansion(theGraph, tablesToKeep);
		}
	}
	
	/**
	 * Collect all tables and associations in closure of root.
	 * 
	 * @param root the root
	 * @param ignore table to ignore
	 * @param associationsToKeep to collect associations
	 * @param tablesToKeep to collect tables
	 */
	private void collect(Table root, Table ignore,
			Set<Association> associationsToKeep, Set<Table> tablesToKeep) {
		if (root != ignore && !tablesToKeep.contains(root)) {
			tablesToKeep.add(root);
			for (Association a: root.associations) {
				if (isVisualizable(a) && (renderedAssociations.containsKey(a) || renderedAssociations.containsKey(a.reversalAssociation))) {
					associationsToKeep.add(a);
					associationsToKeep.add(a.reversalAssociation);
					collect(a.destination, ignore, associationsToKeep, tablesToKeep);
					collect(a.source, ignore, associationsToKeep, tablesToKeep);
				}
			}
		}
	}

	/**
	 * Expands a node representing a table.
	 * 
	 * @param g the graph
	 * @param table the table node
	 * 
	 * @return list of newly rendered tables
	 */
	private List<Table> expandTable(Graph g, net.sf.jailer.datamodel.Table table) {
		return expandTable(g, table, null);
	}

	/**
	 * Expands a node representing a table.
	 * 
	 * @param g the graph
	 * @param table the table node
	 * @param toRender if not null, the only association to make visible
	 *  
	 * @return list of newly rendered tables
	 */
	private List<Table> expandTable(Graph g, net.sf.jailer.datamodel.Table table, Association toRender) {
		List<Table> result = new ArrayList<Table>();
		if (table != null && (!expandedTables.contains(table) || toRender != null)) {
			List<Table> toCheck = new ArrayList<Table>();
			result = addEdges(g, table, toRender, toCheck, false);
			// expandedTables.add(table);
			checkForExpansion(g, toCheck);
		}
		return result;
	}

	/**
	 * Checks whether some tables are still expanded.
	 * 
	 * @param g the graph
	 * @param toCheck set of tables to check
	 */
	private void checkForExpansion(Graph g, java.util.Collection<Table> toCheck) {
		for (Table t: toCheck) {
			if (!expandedTables.contains(t)) {
				addEdges(g, t, null, new ArrayList<Table>(), true);
				boolean isExpanded = true;
				for (Association a: t.associations) {
					if (!isVisualizable(a)) {
						continue;
					}
					if (a.source != t && !tableNodes.containsKey(a.source) && !toCheck.contains(a.source)) {
						isExpanded = false;
						break;
					}
					if (a.destination != t && !tableNodes.containsKey(a.destination) && !toCheck.contains(a.destination)) {
						isExpanded = false;
						break;
					}
				}
				if (isExpanded) {
					expandedTables.add(t);
				}
			}
		}
	}

	/**
	 * Checks whether some tables are still collapsed.
	 * 
	 * @param g the graph
	 * @param toCheck set of tables to check
	 */
	private void checkForCollapsed(Graph g, java.util.Collection<Table> toCheck) {
		for (Table t: toCheck) {
			if (expandedTables.contains(t)) {
				boolean isExpanded = true;
				for (Association a: t.associations) {
					if (!isVisualizable(a)) {
						continue;
					}
					if (!renderedAssociations.containsKey(a) && !renderedAssociations.containsKey(a.reversalAssociation)) {
						isExpanded = false;
						break;
					}
				}
				if (!isExpanded) {
					expandedTables.remove(t);
				}
			}
		}
	}

	/**
	 * Adds edges for all associations of a table.
	 * 
	 * @param table the table
	 * @param toRender is not null, the only association to make visible
	 * 
	 * @return list of newly rendered tables
	 */
	private List<Table> addEdges(Graph g, Table table, Association toRender, List<Table> toCheck, boolean visibleDestinationRequired) {
		List<Table> result = new ArrayList<Table>();
		toCheck.add(table);
		for (Association a: table.associations) {
			if (toRender != null && toRender != a) {
				continue;
			}
			if (!isVisualizable(a) && (toRender == null || a != toRender)) {
				continue;
			}
			if (visibleDestinationRequired && !tableNodes.containsKey(a.destination)) {
				continue;
			}
			if (!renderedAssociations.containsKey(a) && !renderedAssociations.containsKey(a.reversalAssociation)) {
				toCheck.add(a.destination);
				toCheck.add(a.source);
				if (showTable(g, a.source)) {
					result.add(a.source);
				}
				if (showTable(g, a.destination)) {
					result.add(a.destination);
				}
				String tooltip = a.getJoinCondition();
				if (!associationIsUnique(a)) {
					Node an = g.addNode();
					an.set("association", a);
					an.setString("label", a.getName() + "#");
					an.setString("tooltip", tooltip);
					renderedAssociationsAsNode.put(a, an);
					Edge ae = g.addEdge(an, tableNodes.get(a.source));
					ae.set("association", a.reversalAssociation);
					ae.set("full", Boolean.FALSE);
					ae.setString("tooltip", tooltip);
					renderedAssociations.put(a.reversalAssociation, ae);
					Edge be = g.addEdge(an, tableNodes.get(a.destination));
					be.set("association", a);
					be.set("full", Boolean.FALSE);
					be.setString("tooltip", tooltip);
					renderedAssociations.put(a, be);
				} else {
					Edge e = g.addEdge(tableNodes.get(a.source), tableNodes.get(a.destination));
					e.set("association", a);
					e.set("full", Boolean.TRUE);
					e.setString("tooltip", tooltip);
					renderedAssociations.put(a, e);
				}
			}
		}
		return result;
	}

	/**
	 * Searches another association with same destination.
	 *  
	 * @param a an association
	 * @return <code>true</code> if no other association is found
	 */
	private boolean associationIsUnique(Association a) {
		if (a.source == a.destination) {
			return false;
		}
		List<Association> all = new ArrayList<Association>();
		all.addAll(a.source.associations);
		all.addAll(a.destination.associations);
		for (Association b: all) {
			if (a != b && a != b.reversalAssociation) {
				if (a.destination == b.destination && a.source == b.source) {
					return false;
				}
				if (a.destination == b.source && a.source == b.destination) {
					return false;
				}
			}
		}
		return true;
	}

	/**
	 * Extention of {@link ZoomToFitControl}.
	 */
	private final class ZoomToFitControlExtension extends ZoomToFitControl {
		private final DataModel model;
		private final long duration;

		private ZoomToFitControlExtension(String group, int margin,
				long duration, int button, DataModel model) {
			super(group, margin, duration, button);
			this.duration = duration;
			this.model = model;
		}

		@Override
		public void itemClicked(VisualItem item, MouseEvent e) {
			// click on table opens pop-up menu
			if (model.getTable(item.getString("label")) != null || item.get("association") != null) {
				return;
			}
			super.itemClicked(item, e);
		}

		/**
		 * Zooms to fit.
		 */
		public void zoomToFit() {
	        Visualization vis = display.getVisualization();
	        Rectangle2D bounds = vis.getBounds(Visualization.ALL_ITEMS);
	        GraphicsLib.expand(bounds, 50 + (int)(1/display.getScale()));
	        DisplayLib.fitViewToBounds(display, bounds, duration);
		}
		
	}

	/**
	 * Renderer for {@link Association}s.
	 */
	private final class CompositeAssociationRenderer implements Renderer {
		private AssociationRenderer associationRenderer = new AssociationRenderer(false);
		private AssociationRenderer reversedAssociationRenderer = new AssociationRenderer(true);
		private AssociationRenderer associationFullRenderer = new AssociationRenderer();
		public boolean useAssociationRendererForLocation = false;
		
		public boolean locatePointWithAssociationRenderer(Point2D p, VisualItem item) {
			return associationRenderer.locatePoint(p, item);
		}

		public boolean locatePoint(Point2D p, VisualItem item) {
			if (useAssociationRendererForLocation) {
				return locatePointWithAssociationRenderer(p, item);
			}
			return associationFullRenderer.locatePoint(p, item);
		}

		public void render(Graphics2D g, VisualItem item) {
			item.setInteractive(true);
			boolean isSelected = selectedAssociation != null && selectedAssociation.equals(item.get("association"));
			boolean isReversedSelected = selectedAssociation != null && selectedAssociation.reversalAssociation.equals(item.get("association"));
			associationFullRenderer.render(g, item, isSelected);
			associationRenderer.render(g, item, isSelected);
			reversedAssociationRenderer.render(g, item, isReversedSelected);
		}

		public void setBounds(VisualItem item) {
			associationFullRenderer.setBounds(item);
		}
	}
	
	/**
	 * Looks up "show disabled associations" setting and
	 * decides whether an association is visualizable.
	 * 
	 * @param association the association to check
	 * @return true if association is visualizable
	 */
	private boolean isVisualizable(Association association) {
		return modelEditor.extractionModelFrame.showDisabledAssociations()
			|| !association.isIgnored();
	}

	/**
	 * Expands all tables.
	 */
	public void expandAll(boolean expandOnlyVisibleTables) {
		boolean stop = false;
		List<Table> toExpand = new ArrayList<Table>();
		toExpand.addAll(tableNodes.keySet());
		while (!stop) {
			boolean askNow = false;
			synchronized (visualization) {
				boolean ask = tableNodes.size() <= EXPAND_LIMIT;
				while (!toExpand.isEmpty()) {
					Table table = toExpand.remove(0);
					List<Table> tables = expandTable(theGraph, table);
					if (!expandOnlyVisibleTables) {
						toExpand.addAll(tables);
					}
					if (ask && tableNodes.size() > EXPAND_LIMIT) {
						askNow = true;
						break;
					}
				}
			}
			if (askNow) {
				if (JOptionPane.NO_OPTION != JOptionPane.showConfirmDialog(modelEditor.extractionModelFrame, "More than " + EXPAND_LIMIT + " visible tables!\nStop expansion?", "", JOptionPane.YES_NO_OPTION, JOptionPane.INFORMATION_MESSAGE)) {
					stop = true;
				}
			} else {
				stop = true;
			}
		}
	}
	
	/**
	 * Resets expanded/collapsed status of each visible table.
	 */
	public void resetExpandedState() {
		synchronized (visualization) {
			hideTable(null);
			checkForCollapsed(theGraph, tableNodes.keySet());
			checkForExpansion(theGraph, tableNodes.keySet());
			visualization.invalidateAll();
		}
	}

	/**
	 * Sets fix property of all visual nodes.
	 * 
	 * @param fix the property value
	 */
	public void setFix(boolean fix) {
		synchronized (visualization) {
			for (int i = visualGraph.getNodeCount() - 1; i >= 0; --i) {
				VisualItem n = (VisualItem) visualGraph.getNode(i);
				n.setFixed(fix);
			}
		}
	}

	private Set<Table> reversedShowDetailsTables = new HashSet<Table>();
	
	/**
	 * Decides whether to show details of a table.
	 * 
	 * @param table the table
	 * @return <code>true</code> iff details are shown
	 */
	public boolean showDetails(Table table) {
		if (reversedShowDetailsTables.contains(table)) {
			return !showTableDetails;
		}
		return showTableDetails;
	}
	
	/**
	 * Updates table details mode.
	 */
	public void updateTableDetailsMode() {
		showTableDetails = modelEditor.extractionModelFrame.showTableDetails();
	//	dragForce.setParameter(DragForce.DRAG_COEFF, showTableDetails? 0.05f : 0.1f);
		reversedShowDetailsTables.clear();
		visualization.invalidateAll();
		visualization.repaint();
	}

	/**
	 * Gets all visible tables.
	 * 
	 * @return set of all tables which are currently visible
	 */
	public Set<Table> getVisibleTables() {
		return tableNodes.keySet();
	}

	private static DisplayExporter displayExporter = new DisplayExporter();
	public boolean inImageExport = false;
	
	public void exportDisplayToImage() throws Exception {
		Association oldAssociation = selectedAssociation;
		Set<String> oldTablesOnPath = tablesOnPath;
		try {
			synchronized (this) {
				selectedAssociation = null;
				tablesOnPath = new HashSet<String>();
				inImageExport = true;
			}
			displayExporter.export(display);
		} finally {
			synchronized (this) {
				inImageExport = false;
				selectedAssociation = oldAssociation;
				tablesOnPath = oldTablesOnPath;
			}
		}
	}
	
}
