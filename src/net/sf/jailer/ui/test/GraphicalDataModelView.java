package net.sf.jailer.ui.test;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.geom.Rectangle2D;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.KeyStroke;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import prefuse.Display;
import prefuse.Visualization;
import prefuse.action.ActionList;
import prefuse.action.RepaintAction;
import prefuse.action.assignment.ColorAction;
import prefuse.action.filter.GraphDistanceFilter;
import prefuse.action.layout.graph.ForceDirectedLayout;
import prefuse.activity.Activity;
import prefuse.controls.DragControl;
import prefuse.controls.FocusControl;
import prefuse.controls.NeighborHighlightControl;
import prefuse.controls.PanControl;
import prefuse.controls.WheelZoomControl;
import prefuse.controls.ZoomControl;
import prefuse.controls.ZoomToFitControl;
import prefuse.data.Edge;
import prefuse.data.Graph;
import prefuse.data.Node;
import prefuse.data.Schema;
import prefuse.data.Table;
import prefuse.data.Tuple;
import prefuse.data.event.TupleSetListener;
import prefuse.data.tuple.TupleSet;
import prefuse.render.EdgeRenderer;
import prefuse.render.LabelRenderer;
import prefuse.render.Renderer;
import prefuse.render.RendererFactory;
import prefuse.render.ShapeRenderer;
import prefuse.util.ColorLib;
import prefuse.util.GraphLib;
import prefuse.util.GraphicsLib;
import prefuse.util.display.DisplayLib;
import prefuse.util.display.ItemBoundsListener;
import prefuse.util.force.ForceItem;
import prefuse.util.force.ForceSimulator;
import prefuse.util.force.Integrator;
import prefuse.util.io.IOLib;
import prefuse.util.ui.JValueSlider;
import prefuse.util.ui.UILib;
import prefuse.visual.EdgeItem;
import prefuse.visual.NodeItem;
import prefuse.visual.VisualGraph;
import prefuse.visual.VisualItem;

/**
 * @author <a href="http://jheer.org">jeffrey heer</a>
 */
public class GraphicalDataModelView extends JPanel {

    private static final String graph = "graph";
    private static final String nodes = "graph.nodes";
    private static final String edges = "graph.edges";

    private Visualization m_vis;
    
    public GraphicalDataModelView(final DataModel model) {
    	super(new BorderLayout());
    	this.model = model;
    	
    	theGraph = getModelGraph(model);
    	
        // create a new, empty visualization for our data
        m_vis = new Visualization();
        
        // --------------------------------------------------------------------
        // set up the renderers
        
        final ShapeRenderer sr = new ShapeRenderer() {
        	protected Shape getRawShape(VisualItem item) {
                double x = item.getX();
                if ( Double.isNaN(x) || Double.isInfinite(x) )
                    x = 0;
                double y = item.getY();
                if ( Double.isNaN(y) || Double.isInfinite(y) )
                    y = 0;
                double width = 10 * item.getSize();
                
                // Center the shape around the specified x and y
                if ( width > 1 ) {
                    x = x-width/2;
                    y = y-width/2;
                }
                return diamond((float) x, (float) y, (float) width);
        	}
        };
        
        final LabelRenderer tr = new LabelRenderer() {
        	protected String getText(VisualItem item) {
                String s = super.getText(item);
                net.sf.jailer.datamodel.Table table = model.getTable(item.getString("label"));
                if (table != null && !expandedTables.contains(table)) {
                	s = "+ " + s;
                }
                return s + " ";
            }
        };
        final EdgeRenderer er = new EdgeRenderer(prefuse.Constants.EDGE_TYPE_CURVE);
        
        tr.setRoundedCorner(8, 8);
        m_vis.setRendererFactory(new RendererFactory() {
			public Renderer getRenderer(VisualItem item) {
				if (item instanceof EdgeItem) {
					return er;
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
                
            	for ( int i=0; i<add.length; ++i ) {
                	if (add[i] instanceof NodeItem) {
                		draw = true;
	    	            expandTable(theGraph, model.getTable(add[i].getString("label")));
	                    ((VisualItem)add[i]).setFixed(false);
	                    ((VisualItem)add[i]).setFixed(true);
                	}
                }
            	if (draw) {
            		m_vis.run("draw");
            	}
            }
        });
        
        // --------------------------------------------------------------------
        // create actions to process the visual data

        int hops = 30;
        final GraphDistanceFilter filter = new GraphDistanceFilter(graph, hops);

        ColorAction fill = new ColorAction(nodes, 
                VisualItem.FILLCOLOR, ColorLib.rgb(220,220,255));
        fill.add(VisualItem.FIXED, ColorLib.rgb(200,200,255));
        fill.add(VisualItem.HIGHLIGHT, ColorLib.rgb(255,200,125));
        
        ActionList draw = new ActionList();
        draw.add(filter);
        draw.add(fill);
        draw.add(new ColorAction(nodes, VisualItem.STROKECOLOR, 0));
        draw.add(new ColorAction(nodes, VisualItem.TEXTCOLOR, ColorLib.rgb(0,0,0)));
        draw.add(new ColorAction(edges, VisualItem.FILLCOLOR, ColorLib.gray(200)));
        draw.add(new ColorAction(edges, VisualItem.STROKECOLOR, ColorLib.gray(200)));
        
        ActionList animate = new ActionList(Activity.INFINITY);
        ForceDirectedLayout l = new ForceDirectedLayout(graph);
        l.getForceSimulator().setIntegrator(new Integrator() {
        	public void integrate(ForceSimulator sim, long timestep) {                float speedLimit = sim.getSpeedLimit();
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
        l.setVisualization(m_vis);
        animate.add(l);
        animate.add(fill);
        animate.add(new RepaintAction());
        
        // finally, we register our ActionList with the Visualization.
        // we can later execute our Actions by invoking a method on our
        // Visualization, using the name we've chosen below.
        m_vis.putAction("draw", draw);
        m_vis.putAction("layout", animate);

        m_vis.runAfter("draw", "layout");
        
        
        // --------------------------------------------------------------------
        // set up a display to show the visualization
        
        Display display = new Display(m_vis);
        display.setSize(700,700);
        display.pan(350, 350);
        display.setForeground(Color.GRAY);
        display.setBackground(Color.WHITE);
        
        // main display controls
        display.addControlListener(new FocusControl(1));
        display.addControlListener(new DragControl() {
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
        display.addControlListener(new NeighborHighlightControl());

        // overview display
//        Display overview = new Display(vis);
//        overview.setSize(290,290);
//        overview.addItemBoundsListener(new FitOverviewListener());
        
        display.setForeground(Color.GRAY);
        display.setBackground(Color.WHITE);
        
        // --------------------------------------------------------------------        
        // launch the visualization
        
        // create a panel for editing force values
       // ForceSimulator fsim = ((ForceDirectedLayout)animate.get(0)).getForceSimulator();
       // JForcePanel fpanel = new JForcePanel(fsim);
        
//        JPanel opanel = new JPanel();
//        opanel.setBorder(BorderFactory.createTitledBorder("Overview"));
//        opanel.setBackground(Color.WHITE);
//        opanel.add(overview);
        
        final JValueSlider slider = new JValueSlider("Distance", 0, hops, hops);
        slider.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                filter.setDistance(slider.getValue().intValue());
                m_vis.run("draw");
            }
        });
        slider.setBackground(Color.WHITE);
        slider.setPreferredSize(new Dimension(300,30));
        slider.setMaximumSize(new Dimension(300,30));
        
        Box cf = new Box(BoxLayout.Y_AXIS);
        cf.add(slider);
        cf.setBorder(BorderFactory.createTitledBorder("Connectivity Filter"));
  //      fpanel.add(cf);

        //fpanel.add(opanel);
        
  //      fpanel.add(Box.createVerticalGlue());
        
        // create a new JSplitPane to present the interface
        JSplitPane split = new JSplitPane();
        split.setLeftComponent(display);
  //      split.setRightComponent(fpanel);
        split.setOneTouchExpandable(true);
        split.setContinuousLayout(false);
        split.setDividerLocation(700);
        
        // now we run our action list
        m_vis.run("draw");
        l.run();
        
        add(split);
    }
    
    public void setGraph(Graph g, String label) {
        // update graph
        m_vis.removeGroup(graph);
        VisualGraph vg = m_vis.addGraph(graph, g);
        m_vis.setValue(edges, null, VisualItem.INTERACTIVE, Boolean.FALSE);
        VisualItem f = (VisualItem)vg.getNode(0);
        m_vis.getGroup(Visualization.FOCUS_ITEMS).setTuple(f);
        f.setFixed(false);
    }
    
    // ------------------------------------------------------------------------
    // Main and demo methods
    
    public static void main(String[] args) {
        UILib.setPlatformLookAndFeel();
        
        // create graphview
        String datafile = null;
        String label = "label";
        if ( args.length > 1 ) {
            datafile = args[0];
            label = args[1];
        }
        
        JFrame frame = demo();
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    }
    
    private Graph theGraph;
    
    private Map<net.sf.jailer.datamodel.Table, Node> tableNodes = new HashMap<net.sf.jailer.datamodel.Table, Node>();
    private Set<net.sf.jailer.datamodel.Table> expandedTables = new HashSet<net.sf.jailer.datamodel.Table>();
    private DataModel model = null;
	
	private Graph getModelGraph(DataModel model) {
		Graph g = new Graph(true);
		Schema s = new Schema();
		s.addColumn("label", String.class);
		s.addColumn("association", Association.class);
		g.addColumns(s);
		net.sf.jailer.datamodel.Table t = null;		
		
		for (net.sf.jailer.datamodel.Table table: model.getTables()) {
			Node n = g.addNode();
			n.setString("label", table.getName());
			System.out.println(table.getName());
			tableNodes.put(table, n);
			if (t==null) t = table;
		}
		
		expandTable(g, t);
		
    	return g;
	}

	private void expandTable(Graph g, net.sf.jailer.datamodel.Table table) {
		if (table != null && !expandedTables.contains(table)) {
			for (Association a: table.associations) {
				if (!a.reversed) {
					Node an = g.addNode();
					an.set("association", a);
					an.setString("label", "");
					Edge ae = g.addEdge(tableNodes.get(a.source), an);
					Edge be = g.addEdge(an, tableNodes.get(a.destination));
				}
			}
			expandedTables.add(table);
		}
	}

	public static JFrame demo() {
		try {
			final GraphicalDataModelView view = new GraphicalDataModelView(new DataModel());
        
	        // set up menu
	        JMenu dataMenu = new JMenu("Data");
	        dataMenu.add(new OpenGraphAction(view));
	        dataMenu.add(new GraphMenuAction("Grid","ctrl 1",view) {
	            protected Graph getGraph() {
	                return GraphLib.getGrid(15,15);
	            }
	        });
	        dataMenu.add(new GraphMenuAction("Clique","ctrl 2",view) {
	            protected Graph getGraph() {
	                return GraphLib.getClique(10);
	            }
	        });
	        dataMenu.add(new GraphMenuAction("Honeycomb","ctrl 3",view) {
	            protected Graph getGraph() {
	                return GraphLib.getHoneycomb(5);
	            }
	        });
	        dataMenu.add(new GraphMenuAction("Balanced Tree","ctrl 4",view) {
	            protected Graph getGraph() {
	                return GraphLib.getBalancedTree(3,5);
	            }
	        });
	        dataMenu.add(new GraphMenuAction("Diamond Tree","ctrl 5",view) {
	            protected Graph getGraph() {
	                return GraphLib.getDiamondTree(3,3,3);
	            }
	        });
	        JMenuBar menubar = new JMenuBar();
	        menubar.add(dataMenu);
	        
	        // launch window
	        JFrame frame = new JFrame("p r e f u s e  |  g r a p h v i e w");
	        frame.setJMenuBar(menubar);
	        frame.setContentPane(view);
	        frame.pack();
	        frame.setVisible(true);
	        
	        frame.addWindowListener(new WindowAdapter() {
	            public void windowActivated(WindowEvent e) {
	                view.m_vis.run("layout");
	            }
	            public void windowDeactivated(WindowEvent e) {
	                view.m_vis.cancel("layout");
	            }
	        });
	        return frame;
        
		} catch (Exception e1) {
			throw new RuntimeException(e1);
		}
    }
    
    
    // ------------------------------------------------------------------------
    
    /**
     * Swing menu action that loads a graph into the graph viewer.
     */
    public abstract static class GraphMenuAction extends AbstractAction {
        private GraphicalDataModelView m_view;
        public GraphMenuAction(String name, String accel, GraphicalDataModelView view) {
            m_view = view;
            this.putValue(AbstractAction.NAME, name);
            this.putValue(AbstractAction.ACCELERATOR_KEY,
                          KeyStroke.getKeyStroke(accel));
        }
        public void actionPerformed(ActionEvent e) {
            m_view.setGraph(getGraph(), "label");
        }
        protected abstract Graph getGraph();
    }
    
    public static class OpenGraphAction extends AbstractAction {
        private GraphicalDataModelView m_view;

        public OpenGraphAction(GraphicalDataModelView view) {
            m_view = view;
            this.putValue(AbstractAction.NAME, "Open File...");
            this.putValue(AbstractAction.ACCELERATOR_KEY,
                          KeyStroke.getKeyStroke("ctrl O"));
        }
        public void actionPerformed(ActionEvent e) {
            Graph g = IOLib.getGraphFile(m_view);
            if ( g == null ) return;
            String label = getLabel(m_view, g);
            if ( label != null ) {
                m_view.setGraph(g, label);
            }
        }
        public static String getLabel(Component c, Graph g) {
            // get the column names
            Table t = g.getNodeTable();
            int  cc = t.getColumnCount();
            String[] names = new String[cc];
            for ( int i=0; i<cc; ++i )
                names[i] = t.getColumnName(i);
            
            // where to store the result
            final String[] label = new String[1];

            // -- build the dialog -----
            // we need to get the enclosing frame first
            while ( c != null && !(c instanceof JFrame) ) {
                c = c.getParent();
            }
            final JDialog dialog = new JDialog(
                    (JFrame)c, "Choose Label Field", true);
            
            // create the ok/cancel buttons
            final JButton ok = new JButton("OK");
            ok.setEnabled(false);
            ok.addActionListener(new ActionListener() {
               public void actionPerformed(ActionEvent e) {
                   dialog.setVisible(false);
               }
            });
            JButton cancel = new JButton("Cancel");
            cancel.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    label[0] = null;
                    dialog.setVisible(false);
                }
            });
            
            // build the selection list
            final JList list = new JList(names);
            list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
            list.getSelectionModel().addListSelectionListener(
            new ListSelectionListener() {
                public void valueChanged(ListSelectionEvent e) {
                    int sel = list.getSelectedIndex(); 
                    if ( sel >= 0 ) {
                        ok.setEnabled(true);
                        label[0] = (String)list.getModel().getElementAt(sel);
                    } else {
                        ok.setEnabled(false);
                        label[0] = null;
                    }
                }
            });
            JScrollPane scrollList = new JScrollPane(list);
            
            JLabel title = new JLabel("Choose a field to use for node labels:");
            
            // layout the buttons
            Box bbox = new Box(BoxLayout.X_AXIS);
            bbox.add(Box.createHorizontalStrut(5));
            bbox.add(Box.createHorizontalGlue());
            bbox.add(ok);
            bbox.add(Box.createHorizontalStrut(5));
            bbox.add(cancel);
            bbox.add(Box.createHorizontalStrut(5));
            
            // put everything into a panel
            JPanel panel = new JPanel(new BorderLayout());
            panel.add(title, BorderLayout.NORTH);
            panel.add(scrollList, BorderLayout.CENTER);
            panel.add(bbox, BorderLayout.SOUTH);
            panel.setBorder(BorderFactory.createEmptyBorder(5,2,2,2));
            
            // show the dialog
            dialog.setContentPane(panel);
            dialog.pack();
            dialog.setLocationRelativeTo(c);
            dialog.setVisible(true);
            dialog.dispose();
            
            // return the label field selection
            return label[0];
        }
    }
    
    public static class FitOverviewListener implements ItemBoundsListener {
        private Rectangle2D m_bounds = new Rectangle2D.Double();
        private Rectangle2D m_temp = new Rectangle2D.Double();
        private double m_d = 15;
        public void itemBoundsChanged(Display d) {
            d.getItemBounds(m_temp);
            GraphicsLib.expand(m_temp, 25/d.getScale());
            
            double dd = m_d/d.getScale();
            double xd = Math.abs(m_temp.getMinX()-m_bounds.getMinX());
            double yd = Math.abs(m_temp.getMinY()-m_bounds.getMinY());
            double wd = Math.abs(m_temp.getWidth()-m_bounds.getWidth());
            double hd = Math.abs(m_temp.getHeight()-m_bounds.getHeight());
            if ( xd>dd || yd>dd || wd>dd || hd>dd ) {
                m_bounds.setFrame(m_temp);
                DisplayLib.fitViewToBounds(d, m_bounds, 0);
            }
        }
    }
    
} // end of class GraphView
