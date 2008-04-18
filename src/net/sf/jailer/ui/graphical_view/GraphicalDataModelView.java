package net.sf.jailer.ui.graphical_view;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.event.MouseEvent;
import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.ExtractionModelEditor;
import prefuse.Display;
import prefuse.Visualization;
import prefuse.action.Action;
import prefuse.action.ActionList;
import prefuse.action.RepaintAction;
import prefuse.action.assignment.ColorAction;
import prefuse.action.filter.GraphDistanceFilter;
import prefuse.action.layout.graph.ForceDirectedLayout;
import prefuse.activity.Activity;
import prefuse.controls.DragControl;
import prefuse.controls.FocusControl;
import prefuse.controls.PanControl;
import prefuse.controls.ToolTipControl;
import prefuse.controls.WheelZoomControl;
import prefuse.controls.ZoomControl;
import prefuse.controls.ZoomToFitControl;
import prefuse.data.Edge;
import prefuse.data.Graph;
import prefuse.data.Node;
import prefuse.data.Schema;
import prefuse.data.Tuple;
import prefuse.data.event.TupleSetListener;
import prefuse.data.tuple.TupleSet;
import prefuse.render.LabelRenderer;
import prefuse.render.Renderer;
import prefuse.render.RendererFactory;
import prefuse.render.ShapeRenderer;
import prefuse.util.ColorLib;
import prefuse.util.FontLib;
import prefuse.util.force.Force;
import prefuse.util.force.ForceItem;
import prefuse.util.force.ForceSimulator;
import prefuse.util.force.Integrator;
import prefuse.util.force.NBodyForce;
import prefuse.visual.EdgeItem;
import prefuse.visual.NodeItem;
import prefuse.visual.VisualGraph;
import prefuse.visual.VisualItem;

/**
 * Graphical restriction model editor.
 * 
 * @author Wisser
 */
public class GraphicalDataModelView extends JPanel {

	/**
	 * The selected association.
	 */
	private Association selectedAssociation;

	// constants
    private static final String graph = "graph";
    private static final String nodes = "graph.nodes";
    private static final String edges = "graph.edges";

    // prefuse visualization
    private Visualization m_vis;
    private Display display;
    private boolean inInitialization = false;
    
    private final Table root;
    private Table tableToExpandNext;
    
    private CompositeAssociationRenderer associationRenderer;
    private final ExtractionModelEditor modelEditor;
    
    private ForceDirectedLayout layout;
    
//    private static Map<String, double[]> nodePositions = Collections.synchronizedMap(new HashMap<String, double[]>());
//    private Map<String, VisualItem> visualItems = Collections.synchronizedMap(new HashMap<String, VisualItem>());
    
    /**
     * Constructor.
     * 
     * @param model the restricted data model
     */
    public GraphicalDataModelView(final DataModel model, ExtractionModelEditor modelEditor, Table subject) {
    	super(new BorderLayout());
    	this.model = model;
    	this.modelEditor = modelEditor;
    	this.root = subject;
    	
    	theGraph = getModelGraph(model);
    	
        // create a new, empty visualization for our data
        m_vis = new Visualization();
        
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
        
        final LabelRenderer tr = new LabelRenderer() {
        	@Override
			public void render(Graphics2D g, VisualItem item) {
				// workaround for 'no-text-color' bug
        		item.setTextColor(ColorLib.rgb(0, 0, 0));
				super.render(g, item);
			}

			protected String getText(VisualItem item) {
                String s = super.getText(item);
                net.sf.jailer.datamodel.Table table = model.getTable(item.getString("label"));
                if (table != null && !expandedTables.contains(table)) {
                	s = "+ " + s;
                }
                return s + " ";
            }
        };
        associationRenderer = new CompositeAssociationRenderer();
        
        tr.setRoundedCorner(3, 3);
        tr.setVerticalPadding(3);
        tr.setHorizontalPadding(3);
        m_vis.setRendererFactory(new RendererFactory() {
			public Renderer getRenderer(VisualItem item) {
//				String label = item.getString("label");
//				if (label != null) {
//					visualItems.put(label, item);
//				}
				if (item instanceof EdgeItem) {
					return associationRenderer;
				}
				if (item.get("association") != null) {
					return sr;
				}
				return tr;
			}
        });

        // --------------------------------------------------------------------
        // register the data with a visualization
        
        // adds graph to visualization and sets renderer label field
        setGraph(theGraph, "label");
        
        // fix selected focus nodes
        TupleSet focusGroup = m_vis.getGroup(Visualization.FOCUS_ITEMS); 
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
                VisualItem.FILLCOLOR, ColorLib.rgba(210,190,0,120));
        fill.add(VisualItem.FIXED, ColorLib.rgba(170,150,0,120));
        fill.add(VisualItem.HIGHLIGHT, ColorLib.rgba(255,220,0,120));
        
        ActionList draw = new ActionList();
        draw.add(filter);
        draw.add(fill);
        draw.add(new ColorAction(nodes, VisualItem.STROKECOLOR, 0));
        draw.add(new ColorAction(nodes, VisualItem.TEXTCOLOR, ColorLib.rgb(0,0,0)));
        draw.add(new ColorAction(edges, VisualItem.FILLCOLOR, ColorLib.gray(200)));
        draw.add(new ColorAction(edges, VisualItem.STROKECOLOR, ColorLib.gray(200)));
        
        ActionList animate = new ActionList(Activity.INFINITY);
        layout = new ForceDirectedLayout(graph);
        for (Force force: layout.getForceSimulator().getForces()) {
        	if (force instanceof NBodyForce) {
        		((NBodyForce) force).setParameter(NBodyForce.GRAVITATIONAL_CONST, -40.0f);
        	}
        }
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
        layout.setVisualization(m_vis);
        animate.add(layout);
        animate.add(fill);
        animate.add(new RepaintAction());
        
        // finally, we register our ActionList with the Visualization.
        // we can later execute our Actions by invoking a method on our
        // Visualization, using the name we've chosen below.
        m_vis.putAction("draw", draw);
        m_vis.putAction("layout", animate);

        m_vis.putAction("expand", new Action() {
			@Override
			public void run(double frac) {
				expandTable(theGraph, tableToExpandNext);
			}
        });
        
        m_vis.runAfter("draw", "layout");
        
        
        // --------------------------------------------------------------------
        // set up a display to show the visualization
        
        display = new Display(m_vis);
        display.setSize(300,150);
        display.pan(300, 150);
        display.setForeground(Color.GRAY);
        display.setBackground(Color.WHITE);
        
        // main display controls
        display.addControlListener(new FocusControl(1));
        display.addControlListener(new DragControl() {
			public void itemPressed(VisualItem item, MouseEvent e) {
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
            			collapseTable(theGraph, table);
            			display.pan(1, 0);
            			display.pan(0, 1);
						display.repaint();
            		} else {
	    	            // expandTable(theGraph, table);
            			tableToExpandNext = table;
            			m_vis.run("expand");
            		}
	            }
            	super.itemPressed(item, e);
			}
			public void itemReleased(VisualItem item, MouseEvent e) {
        		super.itemReleased(item, e);
                if (!SwingUtilities.isLeftMouseButton(e)) return;
                if (item instanceof NodeItem) {
                	item.setFixed(true);
                }
        	}
        });
        display.addControlListener(new PanControl());
        display.addControlListener(new ZoomControl());
        display.addControlListener(new WheelZoomControl());
        display.addControlListener(new ZoomToFitControl());
        display.addControlListener(new ToolTipControl("tooltip"));
//        display.addControlListener(new NeighborHighlightControl());

        display.setForeground(Color.GRAY);
        display.setBackground(Color.WHITE);
       
        // now we run our action list
        m_vis.run("draw");
        layout.run();
        
        add(display);
    }
    
	public void close() {
		m_vis.reset();
		layout.cancel();
	}

    private void setGraph(Graph g, String label) {
        // update graph
    	inInitialization = true;
        m_vis.removeGroup(graph);
        VisualGraph vg = m_vis.addGraph(graph, g);
        VisualItem f = (VisualItem)vg.getNode(0);
		Font font = f.getFont();
	    f.setFont(FontLib.getFont(font.getName(), Font.BOLD, font.getSize()));
        m_vis.getGroup(Visualization.FOCUS_ITEMS).setTuple(f);
        f.setFixed(false);

        inInitialization = false;
    }
    
    /**
     * The model as graph.
     */
    private Graph theGraph;
    
    private Map<net.sf.jailer.datamodel.Table, Node> tableNodes = new HashMap<net.sf.jailer.datamodel.Table, Node>();
    private Set<net.sf.jailer.datamodel.Table> expandedTables = new HashSet<net.sf.jailer.datamodel.Table>();
    private Map<Association, Edge> renderedAssociations = new HashMap<Association, Edge>();
    private Map<Association, Node> renderedAssociationsAsNode = new HashMap<Association, Node>();
    private DataModel model = null;

    /**
     * Sets selected association.
     * 
     * @param association the association to select or <code>null</code> to deselect
     */
    public void setSelection(Association association) {
    	if (selectedAssociation == null || association == null || !selectedAssociation.equals(association)) {
    		if (selectedAssociation != null || association != null) {
    			selectedAssociation = association;
    			modelEditor.select(association);
    			if (selectedAssociation != null) {
    				if (!renderedAssociations.containsKey(selectedAssociation)) {
    					List<Association> path = modelEditor.getPathToRoot(selectedAssociation);
    					int lastVisible = -1;
    					for (int i = path.size() - 1; i >= 0; --i) {
    						if (renderedAssociations.containsKey(path.get(i))) {
    							lastVisible = i;
    							break;
    						}		
    					}
    					for (int i = lastVisible + 1; i < path.size(); ++i) {
    						expandTable(theGraph, path.get(i).source, path.get(i));
    						expandTable(theGraph, path.get(i).destination, path.get(i));
    					}
    				}
    			}
    			repaint();
    		}
    	}
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
		
		Table table = root;
		showTable(g, table);
		expandTable(g, table);
		
    	return g;
	}
	
	/**
	 * Creates visible node for given table.
	 * 
	 * @param graph the graph
	 * @param table the table to show
	 */
	private void showTable(Graph graph, Table table) {
		if (!tableNodes.containsKey(table)) {
			Node n = graph.addNode();
			n.setString("label", table.getName());
			String tooltip = table.getName() + " (" + table.primaryKey.toSQL(null, false) + ")";
			n.setString("tooltip", tooltip);
			tableNodes.put(table, n);
		}
	}
	
	/**
	 * Collapses a node representing a table.
	 * 
	 * @param g the graph
	 * @param table the table node
	 */
	private void collapseTable(Graph g, net.sf.jailer.datamodel.Table table) {
		if (table != null && expandedTables.contains(table)) {
			Set<Association> associationsToKeep = new HashSet<Association>();
			Set<Table> tablesToKeep = new HashSet<Table>();
			
			collect(root, table, associationsToKeep, tablesToKeep);
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
				if (t != table && !tablesToKeep.contains(t)) {
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
			expandedTables.remove(table);
			tablesToKeep.add(table);
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
				if (renderedAssociations.containsKey(a) || renderedAssociations.containsKey(a.reversalAssociation)) {
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
	 */
	private void expandTable(Graph g, net.sf.jailer.datamodel.Table table) {
		expandTable(g, table, null);
	}

	/**
	 * Expands a node representing a table.
	 * 
	 * @param g the graph
	 * @param table the table node
	 * @param toRender is not null, the only association to make visible 
	 */
	private void expandTable(Graph g, net.sf.jailer.datamodel.Table table, Association toRender) {
		if (table != null && (!expandedTables.contains(table) || toRender != null)) {
			List<Table> toCheck = new ArrayList<Table>();
			addEdges(g, table, toRender, toCheck);
			// expandedTables.add(table);
			checkForExpansion(g, toCheck);
		}
	}

	private void checkForExpansion(Graph g, java.util.Collection<Table> toCheck) {
		for (Table t: toCheck) {
			if (!expandedTables.contains(t)) {
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
					addEdges(g, t, null, new ArrayList<Table>());
				}
			}
		}
	}

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
	 * Removes a node representing a table from view.
	 * 
	 * @param g the graph
	 * @param table the table node
	 */
	private void hideTable(Graph g, net.sf.jailer.datamodel.Table table) {
		Node node = tableNodes.get(table);
		if (node != null) {
			List<Table> toCheck = new ArrayList<Table>();
			for (Association a: table.associations) {
				toCheck.add(a.source);
				toCheck.add(a.destination);
				Edge e = renderedAssociations.get(a);
				if (e != null) {
					g.removeEdge(e);
					renderedAssociations.remove(a);
				}
				e = renderedAssociations.get(a.reversalAssociation);
				if (e != null) {
					g.removeEdge(e);
					renderedAssociations.remove(a.reversalAssociation);
				}
				Node n = renderedAssociationsAsNode.get(a);
				if (n != null) {
					g.removeNode(n);
					renderedAssociationsAsNode.remove(a);
				}
				n = renderedAssociationsAsNode.get(a.reversalAssociation);
				if (n != null) {
					g.removeNode(n);
					renderedAssociationsAsNode.remove(a.reversalAssociation);
				}
			}

			g.removeNode(node);
			tableNodes.remove(table);
			expandedTables.remove(table);
			
			checkForCollapsed(g, toCheck);
		}
	}

	/**
	 * @param toRender is not null, the only association to make visible 
	 */
	private void addEdges(Graph g, net.sf.jailer.datamodel.Table table,
			Association toRender, List<Table> toCheck) {
		toCheck.add(table);
		for (Association a: table.associations) {
			if (toRender != null && toRender != a) {
				continue;
			}
			if (!isVisualizable(a) && (toRender == null || a != toRender)) {
				continue;
			}
			if (!renderedAssociations.containsKey(a) && !renderedAssociations.containsKey(a.reversalAssociation)) {
				toCheck.add(a.destination);
				toCheck.add(a.source);
				showTable(g, a.source);
				showTable(g, a.destination);
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
	public void expandAll() {
		if (modelEditor.extractionModelFrame.showDisabledAssociations()) {
			for (Table table: model.getTables()) {
				expandTable(theGraph, table);
			}
		} else {
			Set<Table> toExpand = new HashSet<Table>();
			for (Table table: tableNodes.keySet()) {
				toExpand.addAll(table.closure(true));
			}
			for (Table table: toExpand) {
				expandTable(theGraph, table);
			}
		}
	}
	
	/**
	 * Refresh. Removes all tables from view which are not in closure of root.
	 * 
	 * @param keepVisible set of tables to keep visible, hides all other tables
	 */
	public void refresh(Set<Table> keepVisible) {
		if (!modelEditor.extractionModelFrame.showDisabledAssociations()) {
			for (Table table: model.getTables()) {
				if (keepVisible.contains(table)) {
					expandTable(theGraph, table);
				}
			}
		}
	}
	
}
