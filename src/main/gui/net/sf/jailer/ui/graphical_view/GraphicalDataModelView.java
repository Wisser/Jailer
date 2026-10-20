/*
 * Copyright 2007 - 2026 Ralf Wisser.
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
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.io.File;
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
import java.util.Set;

import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.JSeparator;
import javax.swing.SwingUtilities;
import javax.swing.Timer;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.datamodel.AggregationSchema;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.Colors;
import net.sf.jailer.ui.ExpansionLimitMessage;
import net.sf.jailer.ui.ExtractionModelEditor;
import net.sf.jailer.ui.QueryBuilderDialog;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.DataBrowser;
import net.sf.jailer.ui.databrowser.sqlconsole.SQLConsole;
import net.sf.jailer.ui.scrollmenu.JScrollMenu;
import net.sf.jailer.ui.scrollmenu.JScrollPopupMenu;
import net.sf.jailer.ui.undo.CompensationAction;
import net.sf.jailer.ui.util.UISettings;
import net.sf.jailer.util.SqlUtil;
import prefuse.Constants;
import prefuse.Display;
import prefuse.Visualization;
import prefuse.action.Action;
import prefuse.action.ActionList;
import prefuse.action.RepaintAction;
import prefuse.action.animate.LocationAnimator;
import prefuse.action.assignment.ColorAction;
import prefuse.action.filter.GraphDistanceFilter;
import prefuse.action.layout.graph.ForceDirectedLayout;
import prefuse.action.layout.graph.NodeLinkTreeLayout;
import prefuse.action.layout.graph.RadialTreeLayout;
import prefuse.action.layout.graph.TreeLayout;
import prefuse.activity.Activity;
import prefuse.activity.ActivityAdapter;
import prefuse.activity.SlowInSlowOutPacer;
import prefuse.controls.Control;
import prefuse.controls.FocusControl;
import prefuse.controls.ToolTipControl;
import prefuse.controls.WheelZoomControl;
import prefuse.controls.ZoomToFitControl;
import prefuse.data.Edge;
import prefuse.data.Graph;
import prefuse.data.Node;
import prefuse.data.Schema;
import prefuse.data.Tree;
import prefuse.data.Tuple;
import prefuse.data.event.TupleSetListener;
import prefuse.data.expression.AbstractPredicate;
import prefuse.data.expression.BooleanLiteral;
import prefuse.data.tuple.TupleSet;
import prefuse.render.Renderer;
import prefuse.render.RendererFactory;
import prefuse.render.ShapeRenderer;
import prefuse.util.GraphicsLib;
import prefuse.util.display.DisplayLib;
import prefuse.util.force.DragForce;
import prefuse.util.force.Force;
import prefuse.util.force.ForceItem;
import prefuse.util.force.ForceSimulator;
import prefuse.util.force.Integrator;
import prefuse.util.force.NBodyForce;
import prefuse.util.force.SpringForce;
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
	 * Maximum number of tables to make visible during expansion ("expand all").
	 */
	public static final int EXPAND_LIMIT = 10;

	/**
	 * Maximum number of tables to make visible during expansion ("expand single table").
	 */
	public static final int EXPAND_SINGLE_TABLE_LIMIT = 4;

	/**
	 * The selected association.
	 */
	public Association selectedAssociation;

	/**
	 * Set of names of all tables on path from selected one to the root. (mapped to the position)
	 */
	Map<String, Integer> tablesOnPath = new HashMap<String, Integer>();

	/**
	 * Path from selected one to the root.
	 */
	private List<Association> associationsOnPath = new ArrayList<Association>();

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

	/**
	 * Distance kept between the box of a table and the node of its reflexive association,
	 * on top of half the box diagonal.
	 */
	private static final double REFLEXIVE_NODE_MARGIN = 24;

	private volatile boolean layoutHasBeenSet = false;
	private final ExecutionContext executionContext;

	private final ActionList animate;

	private ImageIcon navigationIcon;
	private ImageIcon tableIcon;
	private ImageIcon focusIcon;
	private ImageIcon databaseIcon;
	private ImageIcon zoomIcon;
	private ImageIcon hideIcon;
	private ImageIcon detailsIcon;
	private ImageIcon columnMappingIcon;
	private ImageIcon disableIcon;
	private ImageIcon enableIcon;
	private ImageIcon htmlRenderIcon;
	private ImageIcon queryBuilderIcon;
	private ImageIcon filterIcon;
	private ImageIcon exportModeIcon;
	private ImageIcon insertIcon;
	private ImageIcon upsertIcon;
	private ImageIcon excludeMenuIcon;
	private ImageIcon yesIcon;
	private ImageIcon noIcon;
	{
		// load images
		navigationIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/navigation.png"));
		tableIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/table.png"));
		focusIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/pin_64.png"));
		databaseIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/database.png"));
		zoomIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/maximizec.png"));
		hideIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/close.png"));
		detailsIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/editdetails.png"));
		columnMappingIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/edit.png"));
		disableIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/Cancel2.png"));
		enableIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/connection.png"));
		htmlRenderIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/edit_32.png"));
		queryBuilderIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/querybuilder.png"));
		filterIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/filter.png"));
		exportModeIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/export.png"));
		insertIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/add.png"));
		upsertIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/upsert.png"));
		excludeMenuIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/delete.png"));
		yesIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/constraint_check.png"));
		noIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/Cancel.png"));
	}

	/**
	 * Constructor.
	 *
	 * @param model the restricted data model
	 * @param modelEditor enclosing model editor
	 * @param subject the subject (root) table
	 * @param expandSubject <code>true</code> to initially expand the subject table
	 * @param width initial width
	 * @param height initial height
	 * @param executionContext the execution context
	 */
	public GraphicalDataModelView(final DataModel model, ExtractionModelEditor modelEditor, Table subject, boolean expandSubject, int width, int height, ExecutionContext executionContext) {
		super(new BorderLayout());
		this.executionContext = executionContext;
		this.model = model;
		this.modelEditor = modelEditor;
		this.root = subject;
		
		UIUtil.invokeLater(() -> {
			if (subject != null && modelEditor.getCurrentSelection() == null) {
				modelEditor.select(subject);
			}
		});

		tableRenderer = new TableRenderer(model, this) {
			@Override
			protected void afterRendering(Table table, Rectangle2D bounds) {
			}
		};
		final Set<Table> initiallyVisibleTables = new HashSet<Table>();
		if (subject != null) {
			Map<String, double[]> positions = executionContext.getLayoutStorage().getPositions(subject.getName());
			if (positions != null) {
				for (String tn: positions.keySet()) {
					Table table = model.getTable(tn);
					if (table != null && !table.equals(subject)) {
						initiallyVisibleTables.add(table);
					}
				}
			}
		}
		theGraph = getModelGraph(model, initiallyVisibleTables, expandSubject);

		// create a new, empty visualization for our data
		visualization = new Visualization();

		final ZoomBoxControl zoomBoxControl = new ZoomBoxControl() {
			@Override
			public void mouseReleased(MouseEvent e) {
				super.mouseReleased(e);
				resetScrollTimer();
			}

		};

		// --------------------------------------------------------------------
		// set up the renderers

		final ShapeRenderer sr = new ShapeRenderer() {
			@Override
			protected Shape getRawShape(VisualItem item) {
				item.setFillColor(Colors.Color_220_210_0_100.getRGB());
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
				return ellipse((float) x, (float) y, (float) width, (float) width);
			}
		};

		associationRenderer = new CompositeAssociationRenderer(model);

		tableRenderer.setRoundedCorner(3, 3);
		tableRenderer.setVerticalPadding(3);
		tableRenderer.setHorizontalPadding(3);
		visualization.setRendererFactory(new RendererFactory() {
			@Override
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
			@Override
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
				VisualItem.FILLCOLOR, 0);
		fill.add(VisualItem.FIXED, 0);
//		fill.add(VisualItem.HIGHLIGHT, ColorLib.rgba(160,160,0,120));

		ActionList draw = new ActionList();
		draw.add(filter);
//		draw.add(fill);
//		draw.add(new ColorAction(nodes, VisualItem.STROKECOLOR, 0));
//		draw.add(new ColorAction(nodes, VisualItem.TEXTCOLOR, ColorLib.rgb(0,0,0)));
//		draw.add(new ColorAction(edges, VisualItem.FILLCOLOR, ColorLib.gray(200)));
//		draw.add(new ColorAction(edges, VisualItem.STROKECOLOR, ColorLib.gray(200)));

		animate = new ActionList(Activity.INFINITY);
//		animate.add(fill);
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
		display.setDamageRedraw(false);
		display.setSize(width, height);
		display.pan(width / 2, height / 2);
		display.zoom(new Point2D.Double(width / 2, height / 2), 1.3);
		display.setForeground(Colors.Color_128_128_128);

		// main display controls
		display.addControlListener(new FocusControl(1));
		display.addControlListener(new DragControl() {
			@Override
			public void itemClicked(VisualItem item, MouseEvent e) {
				resetScrollTimer();
				Table table = model.getTable(item.getString("label"));
				if (SwingUtilities.isLeftMouseButton(e)) {
					if (table != null && e.getClickCount() == 1) {
						selectTable(table);
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
						JPopupMenu popup = createPopupMenu(table, null, true);
						popup.show(e.getComponent(), e.getX(), e.getY());
					}
				}
				super.itemClicked(item, e);
			}

			@Override
			public void itemPressed(VisualItem item, MouseEvent e) {
				resetScrollTimer();
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
						GraphicalDataModelView.this.modelEditor.captureLayout();
						try {
							if (expandedTables.contains(table)) {
								++UISettings.s9;
								collapseTable(theGraph, table, false);
								display.pan(1, 0);
								display.pan(0, 1);
								Association sa = selectedAssociation;
								setSelection(null);
								setSelection(sa);
								visualization.invalidateAll();
								display.invalidate();
							} else {
								++UISettings.s9;
								expandTable(theGraph, table, null, true, null);
								visualization.invalidateAll();
								display.invalidate();
							}
						} finally {
							GraphicalDataModelView.this.modelEditor.checkLayoutStack();
						}
					}
				}
				super.itemPressed(item, e);
			}

			@Override
			public void itemReleased(VisualItem item, MouseEvent e) {
				resetScrollTimer();
				// fix after drag
				super.itemReleased(item, e);
				if (!SwingUtilities.isLeftMouseButton(e)) return;
				if (item instanceof NodeItem) {
					item.setFixed(true);
				}
			}
		});
		display.addControlListener(new PanControl() {
			@Override
			public void mouseDragged(MouseEvent e) {
				resetScrollTimer();
				super.mouseDragged(e);
			}
		});
		display.addControlListener(zoomBoxControl);
		display.addControlListener(new WheelZoomControl() {
			{
				setMinScale(0.05);
			}
			@Override
			public void mouseWheelMoved(MouseWheelEvent e) {
				resetScrollTimer();
				Display display = (Display)e.getComponent();
				Point p = e.getPoint();
				zoom(display, p,
					 1 - 0.1f * e.getWheelRotation(), false);
			}

		});
		zoomToFitControl = new ZoomToFitControlExtension(Visualization.ALL_ITEMS, 50, 800, Control.RIGHT_MOUSE_BUTTON,	model);
		display.addControlListener(zoomToFitControl);
		display.addControlListener(new ToolTipControl("tooltip"));

		display.setForeground(Colors.Color_128_128_128);
		display.setBackground(Colors.GraphicalDataViewBackground);

		// now we run our action list
		visualization.run("draw");

		resetExpandedState();

		display.setHighQuality(true);

		add(display);

		Rectangle2D bounds = null;
		if (root != null) {
			synchronized (visualization) {
				@SuppressWarnings("rawtypes")
				Iterator items = visualization.items(BooleanLiteral.TRUE);
				while (items.hasNext()) {
					VisualItem item = (VisualItem)items.next();
					if (item.canGetString("label") ) {
						String tableName = item.getString("label");
						double[] pos = executionContext.getLayoutStorage().getPosition(root.getName(), tableName);
						if (pos != null) {
							if (bounds == null) {
								bounds = new Rectangle2D.Double(pos[0], pos[1], 1, 1);
							} else {
								bounds.add(new Point2D.Double(pos[0], pos[1]));
							}
						}
					}
				}
			}
		}

		if (bounds != null) {
			display.panToAbs(new Point2D.Double(bounds.getCenterX(), bounds.getCenterY()));
		}

		layout = new ForceDirectedLayout(graph, createForceSimulator(), false) {
			@Override
			protected float getMassValue(VisualItem n) {
				if (zoomBoxControl.getRenderer().isBoxItem(n)) {
					return 0.01f;
				}
				Association association = (Association) n.get("association");
				if (association != null && association.source == association.destination) {
					// light, so that the repulsion from its own table doesn't push it away
					return 0.3f;
				}
				return showTableDetails? 2.0f : 1.0f;
			}
			@Override
			protected float getSpringLength(EdgeItem e) {
				Association association = (Association) e.get("association");
				if (association != null && association.source == association.destination) {
					// Both edges of a reflexive association pull its node towards the same table,
					// so with the default rest length the node settles inside the table box and
					// the association is drawn across its own columns. Keep it clear of the box.
					// Half the diagonal is the distance from the centre to a corner, so the node
					// clears the box in every direction, not just horizontally.
					double half = 0.5 * Math.max(diagonal(e.getSourceItem().getBounds()),
							diagonal(e.getTargetItem().getBounds()));
					if (half > 0) {
						return (float) (half + REFLEXIVE_NODE_MARGIN);
					}
				}
				return -1f;
			}
			private double diagonal(Rectangle2D b) {
				return b == null? 0 : Math.hypot(b.getWidth(), b.getHeight());
			}
		};
		for (Force force: layout.getForceSimulator().getForces()) {
			if (force instanceof NBodyForce) {
				this.force = (NBodyForce) force;
			}
		}
		if (force != null) {
			force.setParameter(NBodyForce.GRAVITATIONAL_CONST, -100.0f);
		}
		updateTableDetailsMode();
		layout.getForceSimulator().setIntegrator(new Integrator() {
			@Override
			public void integrate(ForceSimulator sim, long timestep) {
				// the forces have already been accumulated by the simulator
				limitForces(sim);
				float speedLimit = sim.getSpeedLimit();
				float vx, vy, v, coeff;
				float[][] k, l;

				@SuppressWarnings("rawtypes")
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
				limitForces(sim);

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
				limitForces(sim);

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
				limitForces(sim);

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
		layout.run();
		if (root != null) {
			final Map<String, double[]> posMap = executionContext.getLayoutStorage().getPositions(root.getName());
			Action a = new Action() {
				boolean done = false;
				@Override
				public void run(double frac) {
					if (!done) {
						synchronized (visualization) {
							if (root != null && !initiallyVisibleTables.isEmpty()) {
								@SuppressWarnings("rawtypes")
								Iterator items = visualization.items(BooleanLiteral.TRUE);
								while (items.hasNext()) {
									VisualItem item = (VisualItem)items.next();
									if (item.canGetString("label") ) {
										String tableName;
										tableName = item.getString("label");
										double[] pos = posMap.get(tableName);
										if (pos != null) {
											item.setX(pos[0]);
											item.setY(pos[1]);
											item.setEndX(pos[0]);
											item.setEndY(pos[1]);
											item.setFixed(pos[2] == 1.0);
										}
									}
								}
							}
						}
						layout.reset();
						visualization.invalidateAll();
						done = true;
						layoutHasBeenSet = true;
					}
				}
			};
			a.alwaysRunAfter(layout);
			animate.add(a);
		}
		animate.add(new Action() {
			// force redraw, work-around for a strange repaint bug
			long startTime = System.currentTimeMillis();
			boolean done = false;
			@Override
			public void run(double frac) {
				if (!done && System.currentTimeMillis() > startTime + 30) {
					display.pan(1, 1);
					display.pan(-1, -1);
					done = true;
				}
			}
		});
	}

	private static final float MAX_FORCE = 1e6f;

	/**
	 * Makes all forces finite. The n-body force is G*m1*m2/r^3, which overflows to infinity for items that are
	 * almost (but not exactly) at the same place. The speed limit of the integrator would turn that into NaN
	 * (infinity / infinity) instead of pushing the items apart.
	 */
	private static void limitForces(ForceSimulator sim) {
		@SuppressWarnings("rawtypes")
		Iterator items = sim.getItems();
		while (items.hasNext()) {
			float[] force = ((ForceItem) items.next()).force;
			for (int i = 0; i < force.length; ++i) {
				if (Float.isNaN(force[i])) {
					force[i] = 0;
				} else if (Math.abs(force[i]) > MAX_FORCE) {
					force[i] = Math.signum(force[i]) * MAX_FORCE;
				}
			}
		}
	}

	/**
	 * Creates the force simulator of the layout, with the same forces as prefuse's default one.
	 * The n-body force repairs items with a non-finite location before building its quadtree.
	 * prefuse's quadtree recurses endlessly on such an item (StackOverflowError, printed on every simulation step).
	 */
	private static ForceSimulator createForceSimulator() {
		ForceSimulator fsim = new ForceSimulator();
		fsim.addForce(new NBodyForce() {
			@Override
			public void init(ForceSimulator fsim) {
				@SuppressWarnings("rawtypes")
				Iterator items = fsim.getItems();
				while (items.hasNext()) {
					ForceItem item = (ForceItem) items.next();
					if (!isFinite(item.location)) {
						if (isFinite(item.plocation)) {
							item.location[0] = item.plocation[0];
							item.location[1] = item.plocation[1];
						} else {
							item.location[0] = item.location[1] = 0;
						}
						item.velocity[0] = item.velocity[1] = 0;
						item.force[0] = item.force[1] = 0;
					}
				}
				super.init(fsim);
			}
			private boolean isFinite(float[] p) {
				return Float.isFinite(p[0]) && Float.isFinite(p[1]);
			}
		});
		fsim.addForce(new SpringForce());
		fsim.addForce(new DragForce());
		return fsim;
	}

	/**
	 * Stores current positions of the tables.
	 */
	public void storeLayout() {
		if (root != null && layoutHasBeenSet) {
			synchronized (visualization) {
				executionContext.getLayoutStorage().removeAll(root.getName());
				@SuppressWarnings("rawtypes")
				Iterator items = visualization.items(BooleanLiteral.TRUE);
				while (items.hasNext()) {
					VisualItem item = (VisualItem)items.next();
					if (item.canGetString("label") ) {
						String tableName;
						tableName = item.getString("label");
						if (tableName != null) {
							executionContext.getLayoutStorage().setPosition(root.getName(), tableName, new double[] { item.getX(), item.getY(), item.isFixed()? 1.0:0.0 });
						}
					}
				}
				executionContext.getLayoutStorage().checkSignificance(root.getName());
			}
		}
	}

	/**
	 * Creates popup menu.
	 *
	 * @param table the table for which the menu pops up
	 * @param findPathMenuItem additional menu item
	 * @param withNavigation <code>true</code> to include the "Show Associated Table" navigation submenu
	 * @return the popup menu
	 */
	public JPopupMenu createPopupMenu(final Table table, JMenuItem findPathMenuItem, boolean withNavigation) {
		JPopupMenu popup = new JScrollPopupMenu(true);
		boolean withModifications = modelEditor.getAdditionalPopupMenuItems().isEmpty();

		JMenu navigateTo = null;

		if (withNavigation) {
			navigateTo = new JScrollMenu("Show Associated Table");
			navigateTo.setIcon(navigationIcon);
			List<Association> aList = new ArrayList<Association>();
			Set<Table> includedTables = new HashSet<Table>();
			Set<Association> visualizable = new HashSet<Association>();
			for (Association a: table.associations) {
				if (!includedTables.contains(a.destination)) {
					if (isVisualizable(a)) {
						visualizable.add(a);
					}
					aList.add(a);
					includedTables.add(a.destination);
				}
			}
			Collections.sort(aList, new Comparator<Association>() {
				@Override
				public int compare(Association o1, Association o2) {
					return o1.getDataModel().getDisplayName(o1.destination)
						   .compareTo(o2.getDataModel().getDisplayName(o2.destination));
				}
			});
			navigateTo.setEnabled(false);
			JMenu currentMenu = navigateTo;
//			int numItems = 0;
//			final int MAX_ITEMS = 30;
			Font italic = null;
			for (final Association a: aList) {
				String miText = a.getDataModel().getDisplayName(a.destination);
				JMenuItem mi = new JMenuItem();
				mi.setIcon(tableIcon);
				if (!visualizable.contains(a)) {
					if (italic == null) {
						Font font = new JLabel().getFont();
						italic = font.deriveFont(font.getStyle() | Font.ITALIC, font.getSize());
					}
					mi.setFont(italic);
				}
				final int MAX_LENGTH = 50;
				if (miText.length() < MAX_LENGTH) {
					mi.setText(miText);
				} else {
					mi.setText(miText.substring(0, MAX_LENGTH) + "...");
					mi.setToolTipText(miText);
				}
				mi.addActionListener(new ActionListener () {
					@Override
					public void actionPerformed(ActionEvent e) {
						GraphicalDataModelView.this.modelEditor.select(a);
					}
				});
				if (renderedAssociations.containsKey(a)|| renderedAssociations.containsKey(a.reversalAssociation)) {
					mi.setEnabled(false);
				} else {
					navigateTo.setEnabled(true);
				}
//				if (numItems++ > MAX_ITEMS) {
//					JMenu nextMenu = new JMenu("More");
//					currentMenu.add(nextMenu);
//					currentMenu = nextMenu;
//					numItems = 1;
//				}
				currentMenu.add(mi);
			}
		}

//		JMenuItem select = new JMenuItem("Select " + table.getName());
//		select.addActionListener(new ActionListener () {
//			public void actionPerformed(ActionEvent e) {
//				modelEditor.select(table);
//			}
//		});
		JMenuItem selectAsRoot = new JMenuItem("Focus on " + table.getName());
		selectAsRoot.setIcon(focusIcon);
		selectAsRoot.addActionListener(new ActionListener () {
			@Override
			public void actionPerformed(ActionEvent e) {
				GraphicalDataModelView.this.modelEditor.captureLayout();
				try {
					modelEditor.setRootSelection(table);
				} finally {
					GraphicalDataModelView.this.modelEditor.checkLayoutStack();
				}
			}
		});
		JMenuItem dataBrowser = new JMenuItem("Browse Data");
		dataBrowser.setIcon(databaseIcon);
		dataBrowser.addActionListener(new ActionListener () {
			@Override
			public void actionPerformed(ActionEvent e) {
				modelEditor.extractionModelFrame.openDataBrowser(table, "");
			}
		});
		JMenuItem zoomToFit = new JMenuItem("Zoom To Fit");
		zoomToFit.setIcon(zoomIcon);
		zoomToFit.addActionListener(new ActionListener () {
			@Override
			public void actionPerformed(ActionEvent e) {
				zoomToFit();
			}
		});
		JMenuItem hide = new JMenuItem("Hide " + table.getName());
		hide.setIcon(hideIcon);
		hide.addActionListener(new ActionListener () {
			@Override
			public void actionPerformed(ActionEvent e) {
				GraphicalDataModelView.this.modelEditor.captureLayout();
				hideTable(table);
				display.invalidate();
				GraphicalDataModelView.this.modelEditor.checkLayoutStack();
			}
		});
		if (table.equals(root)) {
			hide.setEnabled(false);
		}
		JMenuItem toggleDetails = new JMenuItem(showDetails(table)? "Hide Details" : "Show Details");
		toggleDetails.setIcon(detailsIcon);
		toggleDetails.addActionListener(new ActionListener () {
			@Override
			public void actionPerformed(ActionEvent e) {
				toggleShowDetails(table);
			}
		});
		JMenuItem mapColumns = null;
		if (modelEditor.scriptFormat.isObjectNotation()) {
			mapColumns = new JMenuItem(modelEditor.createXMappingButtonText(table, true));
			mapColumns.setIcon(columnMappingIcon);
			mapColumns.addActionListener(new ActionListener () {
				@Override
				public void actionPerformed(ActionEvent e) {
					modelEditor.openColumnMapper(table);
				}
			});
		}

		JMenuItem restrictAll = new JMenuItem("Disable All Associations");
		restrictAll.setToolTipText("<html>Disable all associations of this table except dependencies<br>"
				+ "(the parent rows needed for referential integrity).</html>");
		restrictAll.setIcon(disableIcon);
		restrictAll.addActionListener(new ActionListener () {
			@Override
			public void actionPerformed(ActionEvent e) {
				modelEditor.ignoreAll(table);
			}
		});
		restrictAll.setEnabled(modelEditor.isIgnoreAllApplicable(table));

		JMenuItem removeRestrictions = new JMenuItem("Remove All Restrictions");
		removeRestrictions.setToolTipText("Remove all restrictions of the associations of this table and enable the disabled ones.");
		removeRestrictions.setIcon(enableIcon);
		removeRestrictions.addActionListener(new ActionListener () {
			@Override
			public void actionPerformed(ActionEvent e) {
				modelEditor.removeAllRestrictions(table);
			}
		});
		JMenuItem htmlRender = new JMenuItem("Open HTML Render");
		htmlRender.setIcon(htmlRenderIcon);
		htmlRender.addActionListener(new ActionListener () {
			@Override
			public void actionPerformed(ActionEvent e) {
				modelEditor.extractionModelFrame.openHTMLRender(table);
			}
		});
		JMenuItem queryBuilder = new JMenuItem("Query Builder");
		queryBuilder.setIcon(queryBuilderIcon);
		queryBuilder.addActionListener(new ActionListener () {
			@Override
			public void actionPerformed(ActionEvent e) {
				openQueryBuilder(table, false);
			}
		});
//		JMenuItem shortestPath = new JMenuItem("Show shortest path");
//		shortestPath.addActionListener(new ActionListener () {
//			public void actionPerformed(ActionEvent e) {
//				modelEditor.extractionModelFrame.showShortestPath(modelEditor.getSubject(), table);
//			}
//		});
		removeRestrictions.setEnabled(modelEditor.isRemovalOfAllRestrictionsApplicable(table));

//		JMenuItem findTable = new JMenuItem("Browse Closure");
//		findTable.addActionListener(new ActionListener () {
//			public void actionPerformed(ActionEvent e) {
//				modelEditor.extractionModelFrame.openClosureView(table);
//			}
//		});

		JMenuItem filterEditor= new JMenuItem("Edit Filters...");
		filterEditor.setIcon(filterIcon);
		filterEditor.addActionListener(new ActionListener () {
			@Override
			public void actionPerformed(ActionEvent e) {
				modelEditor.extractionModelFrame.openFilterEditor(table);
			}
		});

		popup.add(toggleDetails);
		popup.add(new JSeparator());
		popup.add(hide);
		if (withModifications){
			popup.add(selectAsRoot);
		}
		if (navigateTo != null) {
			popup.add(navigateTo);
		}
		if (findPathMenuItem != null) {
			popup.add(findPathMenuItem);
		}
//		popup.add(findTable);
//		popup.add(select);
		if (withModifications){
			popup.add(new JSeparator());
			popup.add(restrictAll);
			popup.add(removeRestrictions);
			popup.add(filterEditor);
		}
		popup.add(new JSeparator());
//		popup.add(shortestPath);
		popup.add(zoomToFit);
		if (withModifications){
			popup.add(new JSeparator());
			popup.add(queryBuilder);
			popup.add(htmlRender);

			popup.add(new JSeparator());
			popup.add(dataBrowser);

			popup.add(new JSeparator());
			JMenu insertModeMenu = new JMenu("Export Mode");
			insertModeMenu.setIcon(exportModeIcon);
			popup.add(insertModeMenu);
			JRadioButtonMenuItem insert = new JRadioButtonMenuItem("Insert");
			insert.setIcon(insertIcon);
			insert.addActionListener(new ActionListener () {
				@Override
				public void actionPerformed(ActionEvent e) {
					if (!Boolean.FALSE.equals(table.upsert)) {
						changeExportMode(false, table);
					}
				}
			});
			insertModeMenu.add(insert);
			JRadioButtonMenuItem upsert = new JRadioButtonMenuItem("Upsert/Merge");
			upsert.setIcon(upsertIcon);
			insertModeMenu.add(upsert);
			upsert.addActionListener(new ActionListener () {
				@Override
				public void actionPerformed(ActionEvent e) {
					if (!Boolean.TRUE.equals(table.upsert)) {
						changeExportMode(true, table);
					}
				}
			});
			JRadioButtonMenuItem deflt = new JRadioButtonMenuItem("Data model default (" + ((table.defaultUpsert? "Upsert" : "Insert") + ")"));
			insertModeMenu.add(deflt);
			deflt.addActionListener(new ActionListener () {
				@Override
				public void actionPerformed(ActionEvent e) {
					if (table.upsert != null) {
						changeExportMode(null, table);
					}
				}
			});
			ButtonGroup bt = new ButtonGroup();
			bt.add(insert);
			bt.add(upsert);
			bt.add(deflt);

			if (table.upsert == null) {
				deflt.setSelected(true);
			}

			if (Boolean.TRUE.equals(table.upsert)) {
				upsert.setSelected(true);
			}

			if (Boolean.FALSE.equals(table.upsert)) {
				insert.setSelected(true);
			}

			JMenu excludeMenu = new JMenu("Exclude from Deletion");
			excludeMenu.setIcon(excludeMenuIcon);
			popup.add(excludeMenu);
			JRadioButtonMenuItem yes = new JRadioButtonMenuItem("Yes");
			yes.setIcon(yesIcon);
			yes.addActionListener(new ActionListener () {
				@Override
				public void actionPerformed(ActionEvent e) {
					if (!Boolean.TRUE.equals(table.excludeFromDeletion)) {
						changeExcludeFromDeletion(true, table);
					}
				}
			});
			excludeMenu.add(yes);
			JRadioButtonMenuItem no = new JRadioButtonMenuItem("No");
			no.setIcon(noIcon);
			excludeMenu.add(no);
			no.addActionListener(new ActionListener () {
				@Override
				public void actionPerformed(ActionEvent e) {
					if (!Boolean.FALSE.equals(table.excludeFromDeletion)) {
						changeExcludeFromDeletion(false, table);
					}
				}
			});
			deflt = new JRadioButtonMenuItem("Data model default (" + ((table.defaultExcludeFromDeletion? "Yes" : "No") + ")"));
			excludeMenu.add(deflt);
			deflt.addActionListener(new ActionListener () {
				@Override
				public void actionPerformed(ActionEvent e) {
					if (table.excludeFromDeletion != null) {
						changeExcludeFromDeletion(null, table);
					}
				}
			});
			bt = new ButtonGroup();
			bt.add(yes);
			bt.add(no);
			bt.add(deflt);

			if (table.excludeFromDeletion == null) {
				deflt.setSelected(true);
			}

			if (Boolean.TRUE.equals(table.excludeFromDeletion)) {
				yes.setSelected(true);
			}

			if (Boolean.FALSE.equals(table.excludeFromDeletion)) {
				no.setSelected(true);
			}
		}

		if (!modelEditor.getAdditionalPopupMenuItems().isEmpty()) {
			popup.add(new JSeparator());
			for (JMenuItem item: modelEditor.getAdditionalPopupMenuItems()) {
				popup.add(item);
			}
		}

		if (withModifications && mapColumns != null){
			popup.add(new JSeparator());
			popup.add(mapColumns);
		}
		
		return popup;
	}

	protected void changeExcludeFromDeletion(Boolean mode, final Table table) {
		final Boolean old = table.excludeFromDeletion;
		table.excludeFromDeletion = mode;
		synchronized (visualization) {
			visualization.invalidateAll();
			display.invalidate();
			modelEditor.markDirty();
		}
		modelEditor.getUndoManager().push(new CompensationAction(
				1,
				"changed exclude mode (" + (mode == null? "Default" : mode? "Yes" : "No") + ")",
				"changed exclude mode (" + (old == null? "Default" : old? "Yes" : "No") + ")",
				model.getDisplayName(table)) {
			@Override
			public void run() {
				changeExcludeFromDeletion(old, table);
			}
		});
	}

	private void changeExportMode(Boolean mode, final Table table) {
		final Boolean old = table.upsert;
		table.upsert = mode;
		synchronized (visualization) {
			visualization.invalidateAll();
			display.invalidate();
			modelEditor.markDirty();
		}
		modelEditor.getUndoManager().push(new CompensationAction(
				1,
				"changed export mode (" + (mode == null? "Default" : !mode? "Insert" : "Upsert") + ")",
				"changed export mode (" + (old == null? "Default" : !old? "Insert" : "Upsert") + ")",
				model.getDisplayName(table)) {
			@Override
			public void run() {
				changeExportMode(old, table);
			}
		});
	}

	/**
	 * Creates popup menu.
	 *
	 * @param association the association for which the menu pops up
	 * @return the popup menu
	 */
	public JPopupMenu createPopupMenu(final Association association) {
		JPopupMenu popup = new JPopupMenu();
		boolean withModifications = modelEditor.getAdditionalPopupMenuItems().isEmpty();

		JMenuItem disable = new JMenuItem("Disable Association");
		disable.setIcon(disableIcon);
		disable.addActionListener(new ActionListener () {
			@Override
			public void actionPerformed(ActionEvent e) {
				setRestriction(association, true);
			}
		});
		JMenuItem enable = new JMenuItem("Enable Association");
		enable.setIcon(enableIcon);
		enable.addActionListener(new ActionListener () {
			@Override
			public void actionPerformed(ActionEvent e) {
				setRestriction(association, false);
			}
		});
		JMenuItem zoomToFit = new JMenuItem("Zoom To Fit");
		zoomToFit.setIcon(zoomIcon);
		zoomToFit.addActionListener(new ActionListener () {
			@Override
			public void actionPerformed(ActionEvent e) {
				zoomToFit();
			}
		});

		disable.setEnabled(!association.isIgnored());
		enable.setEnabled(association.isRestricted());

		if (withModifications) {
			popup.add(disable);
			popup.add(enable);
			if (modelEditor.pendingDecisionsPanel != null && modelEditor.pendingDecisionsPanel.isActivated()) {
				popup.add(new JSeparator());
				JCheckBoxMenuItem check = new JCheckBoxMenuItem("Checked");
				check.setIcon(yesIcon);
				check.setSelected(modelEditor.pendingDecisionsPanel.isChecked(association));
				check.setEnabled(modelEditor.pendingDecisionsPanel.origDecisionPending.contains(association.reversalAssociation.getName()));
				check.addActionListener(new ActionListener () {
					@Override
					public void actionPerformed(ActionEvent e) {
						if (modelEditor.pendingDecisionsPanel.isChecked(association)) {
							modelEditor.pendingDecisionsPanel.decisionUndone(association);
						} else {
							modelEditor.pendingDecisionsPanel.decisionMade(association);
						}
					}
				});
				popup.add(check);
			}
			popup.add(new JSeparator());
		}
		if (modelEditor.scriptFormat != null && modelEditor.scriptFormat.isObjectNotation()) {
			if (!withModifications) {
				popup.add(new JSeparator());
			}
			JCheckBoxMenuItem check = new JCheckBoxMenuItem();
			check.setIcon(columnMappingIcon);
			check.setSelected(association.reversalAssociation.getAggregationSchema() != AggregationSchema.NONE);
			modelEditor.initXAggregationCheckbox(check, association, check.isSelected(), 70);
			check.addActionListener(e -> {
				modelEditor.onXAggregateCheckboxAction(association, check);
				modelEditor.updateView();
			});
			popup.add(check);
			JMenuItem aggregationMapping = new JMenuItem(modelEditor.createXMappingButtonText(association.destination, true));
			aggregationMapping.setIcon(columnMappingIcon);
			popup.add(aggregationMapping);
			aggregationMapping.addActionListener(e -> modelEditor.openColumnMapper(association.destination));
			popup.add(new JSeparator());
		}
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
	 * @param storeLayout <code>true</code> to save the current table layout before closing
	 * @param removeLayout <code>true</code> to delete the stored layout
	 */
	public void close(boolean storeLayout, boolean removeLayout) {
		visualization.cancel(ARRANGE_ACTION);
		if (storeLayout) {
			storeLayout();
		} else if (removeLayout) {
			if (root != null) {
				executionContext.getLayoutStorage().removeAll(root.getName());
			}
		}
		synchronized (visualization) {
			visualization.reset();
		}
		layout.cancel();
	}

	/**
	 * Zooms to fit.
	 */
	public void zoomToFit() {
		zoomToFitControl.zoomToFit();
	}

	/**
	 * Zooms to fit.
	 * @param duration the animation duration in milliseconds
	 */
	public void zoomToFit(long duration) {
		zoomToFitControl.zoomToFit(duration);
	}

	/**
	 * Creates the tree layout used by {@link GraphicalDataModelView#arrangeTree(TreeLayoutFactory)}.
	 */
	private interface TreeLayoutFactory {
		/**
		 * Creates the layout.
		 *
		 * @param group the group of the auxiliary graph
		 * @param spanningTree the spanning tree, rooted at the subject table
		 * @param sizes the size of the table box per node of the auxiliary graph
		 */
		TreeLayout create(String group, Tree spanningTree, Map<Tuple, Rectangle2D> sizes);

		/**
		 * Gets the schema of the parameter column the layout adds to the nodes.
		 */
		Schema paramsSchema();
	}

	/**
	 * Arranges the visible tables as a tree from left to right, rooted at the subject table, and fixes their positions.
	 * The tree is the breadth-first spanning tree of the tables. Association nodes are placed
	 * between their tables and remain movable.
	 */
	public void arrangeLayout() {
		arrangeTree(new NodeLinkTreeLayoutFactory(Constants.ORIENT_LEFT_RIGHT));
	}

	/**
	 * Arranges the visible tables as a tree from top to bottom, rooted at the subject table, and fixes their positions.
	 *
	 * @see #arrangeLayout()
	 */
	public void arrangeLayoutTopDown() {
		arrangeTree(new NodeLinkTreeLayoutFactory(Constants.ORIENT_TOP_BOTTOM));
	}

	/**
	 * Arranges the visible tables radially around the subject table, one ring per tree level, and fixes their positions.
	 *
	 * @see #arrangeLayout()
	 */
	public void arrangeRadial() {
		arrangeTree(new TreeLayoutFactory() {
			@Override
			public TreeLayout create(String group, Tree spanningTree, Map<Tuple, Rectangle2D> sizes) {
				final double gap = 20;
				double maxDiagonal = 0;
				Map<Integer, Double> diagonalSumPerDepth = new HashMap<Integer, Double>();
				Map<Integer, Integer> countPerDepth = new HashMap<Integer, Integer>();
				@SuppressWarnings("rawtypes")
				Iterator nodes = spanningTree.nodes();
				while (nodes.hasNext()) {
					Node node = (Node) nodes.next();
					if (node.getParent() == null) {
						// the root, or not reachable from it
						continue;
					}
					Rectangle2D size = sizes.get(((VisualItem) node).getSourceTuple());
					double diagonal = size == null? 0 : Math.sqrt(size.getWidth() * size.getWidth() + size.getHeight() * size.getHeight());
					maxDiagonal = Math.max(maxDiagonal, diagonal);
					int depth = spanningTree.getDepth(node.getRow());
					Double sum = diagonalSumPerDepth.get(depth);
					diagonalSumPerDepth.put(depth, (sum == null? 0 : sum) + diagonal);
					Integer count = countPerDepth.get(depth);
					countPerDepth.put(depth, (count == null? 0 : count) + 1);
				}
				// the rings must be far enough apart, and each ring long enough for the tables on it
				double radiusIncrement = maxDiagonal + 2 * gap;
				for (Map.Entry<Integer, Double> e: diagonalSumPerDepth.entrySet()) {
					double circumference = e.getValue() + countPerDepth.get(e.getKey()) * gap;
					radiusIncrement = Math.max(radiusIncrement, circumference / (2 * Math.PI * e.getKey()));
				}
				// with an explicit radius, the layout doesn't scale itself to the (here non-existent) display
				return new RadialTreeLayout(group, (int) Math.ceil(radiusIncrement));
			}

			@Override
			public Schema paramsSchema() {
				return RadialTreeLayout.PARAMS_SCHEMA;
			}
		});
	}

	private static class NodeLinkTreeLayoutFactory implements TreeLayoutFactory {
		private final int orientation;

		NodeLinkTreeLayoutFactory(int orientation) {
			this.orientation = orientation;
		}

		@Override
		public TreeLayout create(String group, Tree spanningTree, Map<Tuple, Rectangle2D> sizes) {
			return new NodeLinkTreeLayout(group, orientation, 80, 20, 40);
		}

		@Override
		public Schema paramsSchema() {
			return NodeLinkTreeLayout.PARAMS_SCHEMA;
		}
	}

	/**
	 * Arranges the visible tables as a tree, rooted at the subject table, and fixes their positions.
	 * The tree is the breadth-first spanning tree of the tables. Association nodes are placed
	 * between their tables and remain movable.
	 *
	 * @param factory creates the tree layout
	 */
	private void arrangeTree(TreeLayoutFactory factory) {
		if (root == null || tableNodes.get(root) == null) {
			return;
		}
		// outside of the synchronized block: locks the model editor first, then the visualization
		modelEditor.captureLayoutBeforeArrangement();
		synchronized (visualization) {
			List<NodeItem> associationItems = new ArrayList<NodeItem>();
			Map<Table, NodeItem> tableItems = collectTableItems(associationItems);

			// The tree is laid out on an auxiliary graph containing only the tables.
			// In the visual graph, association nodes would occupy a tree level of their own.
			Graph tableGraph = new Graph();
			Map<Table, Node> auxNodes = new HashMap<Table, Node>();
			final Map<Tuple, Rectangle2D> sizes = new HashMap<Tuple, Rectangle2D>();
			for (Map.Entry<Table, NodeItem> e: tableItems.entrySet()) {
				Node auxNode = tableGraph.addNode();
				auxNodes.put(e.getKey(), auxNode);
				sizes.put(auxNode, e.getValue().getBounds());
			}
			Node auxRoot = auxNodes.get(root);
			if (auxRoot == null) {
				return;
			}
			@SuppressWarnings("rawtypes")
			Iterator edges = visualGraph.edges();
			while (edges.hasNext()) {
				Object association = ((EdgeItem) edges.next()).get("association");
				if (association instanceof Association) {
					Node s = auxNodes.get(((Association) association).source);
					Node d = auxNodes.get(((Association) association).destination);
					if (s != null && d != null && s != d) {
						tableGraph.addEdge(s, d);
					}
				}
			}

			// the auxiliary items get the size of the table boxes, which the tree layout uses for spacing
			Visualization auxVisualization = new Visualization();
			final Renderer sizeRenderer = new Renderer() {
				@Override
				public void render(Graphics2D g, VisualItem item) {
				}
				@Override
				public boolean locatePoint(Point2D p, VisualItem item) {
					return false;
				}
				@Override
				public void setBounds(VisualItem item) {
					Rectangle2D size = sizes.get(item.getSourceTuple());
					double w = size == null? 0 : size.getWidth();
					double h = size == null? 0 : size.getHeight();
					item.setBounds(item.getX() - w / 2, item.getY() - h / 2, w, h);
				}
			};
			auxVisualization.setRendererFactory(item -> sizeRenderer);
			final String tables = "tables";
			VisualGraph auxGraph = auxVisualization.addGraph(tables, tableGraph);
			// tables not reachable from the root are not touched by the tree layout
			@SuppressWarnings("rawtypes")
			Iterator auxItems = auxGraph.nodes();
			while (auxItems.hasNext()) {
				((NodeItem) auxItems.next()).setX(Double.NaN);
			}
			NodeItem auxRootItem = (NodeItem) auxVisualization.getVisualItem(tables, auxRoot);
			// the tree layout adds its parameter column on run(), which would discard the spanning tree;
			// adding it beforehand keeps the tree rooted at the subject table
			auxGraph.getNodes().addColumns(factory.paramsSchema());
			// without an explicit root, prefuse would build the spanning tree from the first node
			Tree spanningTree = auxGraph.getSpanningTree(auxRootItem);

			NodeItem rootItem = tableItems.get(root);
			TreeLayout treeLayout = factory.create(tables, spanningTree, sizes);
			treeLayout.setLayoutRoot(auxRootItem);
			treeLayout.setLayoutAnchor(new Point2D.Double(rootItem.getX(), rootItem.getY()));
			treeLayout.setVisualization(auxVisualization);
			treeLayout.run(0);

			Map<VisualItem, Point2D> targets = new HashMap<VisualItem, Point2D>();
			for (Map.Entry<Table, NodeItem> e: tableItems.entrySet()) {
				VisualItem auxItem = auxVisualization.getVisualItem(tables, auxNodes.get(e.getKey()));
				if (!Double.isNaN(auxItem.getX())) {
					targets.put(e.getValue(), new Point2D.Double(auxItem.getX(), auxItem.getY()));
				}
			}
			arrangeTo(targets, tableItems, associationItems);
		}
	}

	/**
	 * Arranges the visible tables in layers, parent tables left, child tables right, and fixes their positions.
	 * The order within the layers is chosen so that only few associations cross.
	 * Association nodes are placed between their tables and remain movable.
	 *
	 * @see LayeredLayout
	 */
	public void arrangeLayered() {
		if (root == null || tableNodes.get(root) == null) {
			return;
		}
		// outside of the synchronized block: locks the model editor first, then the visualization
		modelEditor.captureLayoutBeforeArrangement();
		synchronized (visualization) {
			List<NodeItem> associationItems = new ArrayList<NodeItem>();
			Map<Table, NodeItem> tableItems = collectTableItems(associationItems);
			NodeItem rootItem = tableItems.get(root);
			if (rootItem == null) {
				return;
			}
			List<Table> tables = new ArrayList<Table>(tableItems.keySet());
			Collections.sort(tables);
			Map<Table, Rectangle2D> sizes = new HashMap<Table, Rectangle2D>();
			for (Map.Entry<Table, NodeItem> e: tableItems.entrySet()) {
				sizes.put(e.getKey(), e.getValue().getBounds());
			}
			LayeredLayout<Table> layeredLayout = new LayeredLayout<Table>(tables, sizes);
			@SuppressWarnings("rawtypes")
			Iterator edges = visualGraph.edges();
			while (edges.hasNext()) {
				Object association = ((EdgeItem) edges.next()).get("association");
				if (association instanceof Association) {
					Association a = (Association) association;
					if (a.isInsertDestinationBeforeSource()) {
						layeredLayout.addEdge(a.destination, a.source);
					} else if (a.isInsertSourceBeforeDestination()) {
						layeredLayout.addEdge(a.source, a.destination);
					}
				}
			}
			Map<Table, Point2D> positions = layeredLayout.layout();

			// the subject table stays where it is
			Point2D rootPosition = positions.get(root);
			double dx = rootItem.getX() - rootPosition.getX();
			double dy = rootItem.getY() - rootPosition.getY();
			Map<VisualItem, Point2D> targets = new HashMap<VisualItem, Point2D>();
			for (Map.Entry<Table, Point2D> e: positions.entrySet()) {
				targets.put(tableItems.get(e.getKey()), new Point2D.Double(e.getValue().getX() + dx, e.getValue().getY() + dy));
			}
			arrangeTo(targets, tableItems, associationItems);
		}
	}

	/**
	 * Arranges the visible tables in a circle and fixes their positions. The order is the breadth-first order
	 * starting at the subject table, so that associated tables are close to each other.
	 * The subject table stays where it is. Association nodes are placed between their tables and remain movable.
	 */
	public void arrangeCircle() {
		if (root == null || tableNodes.get(root) == null) {
			return;
		}
		// outside of the synchronized block: locks the model editor first, then the visualization
		modelEditor.captureLayoutBeforeArrangement();
		synchronized (visualization) {
			List<NodeItem> associationItems = new ArrayList<NodeItem>();
			Map<Table, NodeItem> tableItems = collectTableItems(associationItems);
			NodeItem rootItem = tableItems.get(root);
			if (rootItem == null) {
				return;
			}
			Map<Table, List<Table>> neighbors = new HashMap<Table, List<Table>>();
			@SuppressWarnings("rawtypes")
			Iterator edges = visualGraph.edges();
			while (edges.hasNext()) {
				Object association = ((EdgeItem) edges.next()).get("association");
				if (association instanceof Association) {
					Table s = ((Association) association).source;
					Table d = ((Association) association).destination;
					if (s != d && tableItems.containsKey(s) && tableItems.containsKey(d)) {
						neighbors.computeIfAbsent(s, t -> new ArrayList<Table>()).add(d);
						neighbors.computeIfAbsent(d, t -> new ArrayList<Table>()).add(s);
					}
				}
			}

			// breadth-first order, tables not reachable from the root at the end
			List<Table> order = new ArrayList<Table>();
			Set<Table> visited = new HashSet<Table>();
			order.add(root);
			visited.add(root);
			for (int i = 0; i < order.size(); ++i) {
				List<Table> next = neighbors.get(order.get(i));
				if (next != null) {
					Collections.sort(next);
					for (Table t: next) {
						if (visited.add(t)) {
							order.add(t);
						}
					}
				}
			}
			List<Table> unreachable = new ArrayList<Table>(tableItems.keySet());
			unreachable.removeAll(visited);
			Collections.sort(unreachable);
			order.addAll(unreachable);

			// each table gets an arc proportional to the diagonal of its box
			final double gap = 20;
			double[] arcs = new double[order.size()];
			double circumference = 0;
			for (int i = 0; i < arcs.length; ++i) {
				Rectangle2D bounds = tableItems.get(order.get(i)).getBounds();
				arcs[i] = Math.sqrt(bounds.getWidth() * bounds.getWidth() + bounds.getHeight() * bounds.getHeight()) + gap;
				circumference += arcs[i];
			}
			double radius = Math.max(100, circumference / (2 * Math.PI));

			// the root is at angle 0, the center is placed so that the root stays where it is
			double centerX = rootItem.getX() - radius;
			double centerY = rootItem.getY();
			Map<VisualItem, Point2D> targets = new HashMap<VisualItem, Point2D>();
			double arcPosition = 0;
			for (int i = 0; i < arcs.length; ++i) {
				double angle = 2 * Math.PI * (arcPosition + arcs[i] / 2 - arcs[0] / 2) / circumference;
				targets.put(tableItems.get(order.get(i)), new Point2D.Double(centerX + radius * Math.cos(angle), centerY + radius * Math.sin(angle)));
				arcPosition += arcs[i];
			}
			arrangeTo(targets, tableItems, associationItems);
		}
	}

	/**
	 * Pushes overlapping tables apart, keeping the arrangement otherwise, and fixes their positions.
	 * Only overlaps between tables are removed, association nodes and edges are not taken into account.
	 * The subject table stays where it is. Association nodes are placed between their tables and remain movable.
	 */
	public void removeTableOverlaps() {
		if (root == null || tableNodes.get(root) == null) {
			return;
		}
		// outside of the synchronized block: locks the model editor first, then the visualization
		modelEditor.captureLayoutBeforeArrangement();
		synchronized (visualization) {
			List<NodeItem> associationItems = new ArrayList<NodeItem>();
			Map<Table, NodeItem> tableItems = collectTableItems(associationItems);
			List<NodeItem> items = new ArrayList<NodeItem>(tableItems.values());
			NodeItem rootItem = tableItems.get(root);

			// the boxes, enlarged by the gap
			final double gap = 10;
			int n = items.size();
			Rectangle2D[] boxes = new Rectangle2D[n];
			for (int i = 0; i < n; ++i) {
				Rectangle2D bounds = items.get(i).getBounds();
				boxes[i] = new Rectangle2D.Double(bounds.getX() - gap / 2, bounds.getY() - gap / 2, bounds.getWidth() + gap, bounds.getHeight() + gap);
			}
			final int maxRounds = 100;
			boolean overlapping = true;
			for (int round = 0; round < maxRounds && overlapping; ++round) {
				overlapping = false;
				for (int i = 0; i < n; ++i) {
					for (int j = i + 1; j < n; ++j) {
						Rectangle2D a = boxes[i];
						Rectangle2D b = boxes[j];
						double overlapX = Math.min(a.getMaxX(), b.getMaxX()) - Math.max(a.getMinX(), b.getMinX());
						double overlapY = Math.min(a.getMaxY(), b.getMaxY()) - Math.max(a.getMinY(), b.getMinY());
						if (overlapX <= 0 || overlapY <= 0) {
							continue;
						}
						overlapping = true;
						// the subject table doesn't move, the other one takes the full distance
						double shareA = items.get(i) == rootItem? 0 : items.get(j) == rootItem? 1 : 0.5;
						double shareB = 1 - shareA;
						// pushed apart along the axis with the smaller overlap, j away from i
						if (overlapX < overlapY) {
							double dir = b.getCenterX() >= a.getCenterX()? 1 : -1;
							move(a, -dir * overlapX * shareA, 0);
							move(b, dir * overlapX * shareB, 0);
						} else {
							double dir = b.getCenterY() >= a.getCenterY()? 1 : -1;
							move(a, 0, -dir * overlapY * shareA);
							move(b, 0, dir * overlapY * shareB);
						}
					}
				}
			}

			Map<VisualItem, Point2D> targets = new HashMap<VisualItem, Point2D>();
			for (int i = 0; i < n; ++i) {
				NodeItem item = items.get(i);
				Rectangle2D bounds = item.getBounds();
				double dx = boxes[i].getX() + gap / 2 - bounds.getX();
				double dy = boxes[i].getY() + gap / 2 - bounds.getY();
				targets.put(item, new Point2D.Double(item.getX() + dx, item.getY() + dy));
			}
			arrangeTo(targets, tableItems, associationItems);
		}
	}

	private static void move(Rectangle2D r, double dx, double dy) {
		r.setRect(r.getX() + dx, r.getY() + dy, r.getWidth(), r.getHeight());
	}

	/**
	 * Collects the visible tables and association nodes.
	 * Must be called while holding the visualization lock.
	 *
	 * @param associationItems to put the association nodes into
	 * @return the node per table
	 */
	private Map<Table, NodeItem> collectTableItems(List<NodeItem> associationItems) {
		Map<Table, NodeItem> tableItems = new HashMap<Table, NodeItem>();
		@SuppressWarnings("rawtypes")
		Iterator nodes = visualGraph.nodes();
		while (nodes.hasNext()) {
			NodeItem item = (NodeItem) nodes.next();
			Table table = item.canGetString("label")? model.getTable(item.getString("label")) : null;
			if (table != null) {
				tableItems.put(table, item);
			} else if (item.get("association") instanceof Association) {
				associationItems.add(item);
			}
		}
		return tableItems;
	}

	/**
	 * Moves the tables to their new positions, animated, and fixes them there. Association nodes are placed
	 * between their tables and released to the force-directed layout afterwards.
	 * Must be called while holding the visualization lock.
	 *
	 * @param targets the new position per table node
	 * @param tableItems the node per table
	 * @param associationItems the association nodes
	 */
	private void arrangeTo(Map<VisualItem, Point2D> targets, Map<Table, NodeItem> tableItems, List<NodeItem> associationItems) {
		Map<VisualItem, Boolean> fixedAfterwards = new HashMap<VisualItem, Boolean>();
		for (VisualItem item: targets.keySet()) {
			fixedAfterwards.put(item, true);
		}
		for (NodeItem item: associationItems) {
			Association association = (Association) item.get("association");
			NodeItem s = tableItems.get(association.source);
			NodeItem d = tableItems.get(association.destination);
			if (s != null && d != null) {
				Point2D ps = target(targets, s);
				Point2D pd = target(targets, d);
				if (s == d) {
					targets.put(item, new Point2D.Double(ps.getX(), ps.getY() - s.getBounds().getHeight() / 2 - REFLEXIVE_NODE_MARGIN));
				} else {
					targets.put(item, new Point2D.Double((ps.getX() + pd.getX()) / 2, (ps.getY() + pd.getY()) / 2));
				}
				// released to the force-directed layout again
				fixedAfterwards.put(item, false);
			}
		}
		animateArrangement(targets, fixedAfterwards, true);
	}

	/**
	 * Moves the tables back to the given positions, animated. Only possible if the same tables are
	 * visible, i.e. only the positions differ.
	 *
	 * @param positions the positions (x, y, fixed) per item label, as stored by {@link #storeLayout()}
	 * @return <code>false</code> if the visible tables differ, nothing has been changed then
	 */
	public boolean animateToPositions(Map<String, double[]> positions) {
		if (!visibleItems().equals(positions.keySet())) {
			return false;
		}
		synchronized (visualization) {
			Map<VisualItem, Point2D> targets = new HashMap<VisualItem, Point2D>();
			Map<VisualItem, Boolean> fixedAfterwards = new HashMap<VisualItem, Boolean>();
			@SuppressWarnings("rawtypes")
			Iterator nodes = visualGraph.nodes();
			while (nodes.hasNext()) {
				NodeItem item = (NodeItem) nodes.next();
				double[] pos = item.canGetString("label")? positions.get(item.getString("label")) : null;
				if (pos != null) {
					targets.put(item, new Point2D.Double(pos[0], pos[1]));
					fixedAfterwards.put(item, pos[2] == 1.0);
				}
			}
			animateArrangement(targets, fixedAfterwards, false);
		}
		return true;
	}

	private static Point2D target(Map<VisualItem, Point2D> targets, VisualItem item) {
		Point2D target = targets.get(item);
		return target != null? target : new Point2D.Double(item.getX(), item.getY());
	}

	private static final String ARRANGE_ACTION = "arrange";
	private static final long ARRANGE_DURATION = 1000;

	/**
	 * The running arrangement transition, if any.
	 */
	private ActionList arrangeTransition;

	/**
	 * Moves the items from their current to the target positions, animated.
	 * Must be called while holding the visualization lock.
	 *
	 * @param targets the target position per item
	 * @param fixedAfterwards the "fixed" state per item once the transition is over
	 * @param zoomToFitAfterwards <code>true</code> to zoom to fit once the transition is over
	 */
	private void animateArrangement(Map<VisualItem, Point2D> targets, final Map<VisualItem, Boolean> fixedAfterwards, final boolean zoomToFitAfterwards) {
		for (Map.Entry<VisualItem, Point2D> e: targets.entrySet()) {
			VisualItem item = e.getKey();
			item.setStartX(item.getX());
			item.setStartY(item.getY());
			item.setEndX(e.getValue().getX());
			item.setEndY(e.getValue().getY());
			// fixed during the transition, so that the force-directed layout doesn't interfere
			item.setFixed(true);
		}
		final Set<VisualItem> items = new HashSet<VisualItem>(targets.keySet());
		LocationAnimator animator = new LocationAnimator(graph);
		animator.setFilterPredicate(new AbstractPredicate() {
			@Override
			public boolean getBoolean(Tuple t) {
				return items.contains(t);
			}
		});
		final ActionList transition = new ActionList(ARRANGE_DURATION);
		transition.setPacingFunction(new SlowInSlowOutPacer());
		transition.add(animator);
		transition.add(new RepaintAction());
		transition.addActivityListener(new ActivityAdapter() {
			@Override
			public void activityFinished(Activity a) {
				release();
				if (zoomToFitAfterwards) {
					SwingUtilities.invokeLater(() -> zoomToFit());
				}
			}
			@Override
			public void activityCancelled(Activity a) {
				release();
			}
			private void release() {
				synchronized (visualization) {
					if (arrangeTransition != transition) {
						// superseded by a newer arrangement, which releases the items itself
						return;
					}
					arrangeTransition = null;
					for (Map.Entry<VisualItem, Boolean> e: fixedAfterwards.entrySet()) {
						// the item may have been removed in the meantime (table collapsed or view closed)
						if (e.getKey().isValid()) {
							e.getKey().setFixed(e.getValue());
						}
					}
					layout.reset();
					visualization.invalidateAll();
				}
			}
		});
		arrangeTransition = transition;
		visualization.cancel(ARRANGE_ACTION);
		visualization.putAction(ARRANGE_ACTION, transition);
		visualization.run(ARRANGE_ACTION);
	}

	/**
	 * Sets visual graph.
	 *
	 * @param g the (non-visual) model graph
	 */
	private void setGraph(Graph g) {
		// update graph
		inInitialization = true;
		synchronized (visualization) {
			visualization.removeGroup(graph);
			visualGraph = visualization.addGraph(graph, g);
			if (visualGraph.getNodeCount() > 1) {
				try {
					VisualItem f = (VisualItem) visualGraph.getNode(1);
					visualization.getGroup(Visualization.FOCUS_ITEMS).setTuple(f);
					f.setFixed(true);
					((VisualItem) visualGraph.getNode(0)).setFixed(true);
				} catch (Exception e) {
					// ignore
				}
			}
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
	Map<net.sf.jailer.datamodel.Table, Node> tableNodes = Collections.synchronizedMap(new HashMap<net.sf.jailer.datamodel.Table, Node>());
	long tableNodesVersion = 1;

	/**
	 * Set of all tables which are currently expanded.
	 */
	Set<net.sf.jailer.datamodel.Table> expandedTables = Collections.synchronizedSet(new HashSet<net.sf.jailer.datamodel.Table>());

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
					Association newlySelectedAssociation = null;
					if (association != null) {
						if (!(renderedAssociations.containsKey(association) || renderedAssociations.containsKey(association.reversalAssociation))) {
							newlySelectedAssociation = association;
						}
					}
					selectedAssociation = association;
					modelEditor.select(association);
					if (association != null) {
						expandTable(theGraph, association.source, association, false, null);
						expandTable(theGraph, association.destination, association, false, null);
					}
					tablesOnPath.clear();
					associationsOnPath.clear();
					if (selectedAssociation != null) {
						List<Association> path = getPathToRoot(selectedAssociation.destination, true, newlySelectedAssociation);
						if (path.isEmpty()) {
							path = getPathToRoot(selectedAssociation.destination, false, newlySelectedAssociation);
						}
						boolean highlightPath = true;
						if (path.isEmpty()) {
							highlightPath = false;
							path = modelEditor.getPathToRoot(selectedAssociation);
						}
						for (int i = 0; i < path.size(); ++i) {
							if (highlightPath) {
								associationsOnPath.add(path.get(i));
								tablesOnPath.put(path.get(i).source.getName(), i);
								tablesOnPath.put(path.get(i).destination.getName(), i);
							}
							expandTable(theGraph, path.get(i).source, path.get(i), false, null);
							expandTable(theGraph, path.get(i).destination, path.get(i), false, null);
						}
						startScrollTimer(selectedAssociation.destination);
					}
					invalidate();
				}
			}
		}
	}

	private Table currentScrollDestination = null;
	private Object currentScrollBounds = null;

	private void resetScrollTimer() {
		synchronized (visualization) {
			currentScrollDestination = null;
			currentScrollBounds = null;
		}
	}

	/**
	 * Starts a timer that scrolls the view to make the given table visible.
	 * @param destination the table to scroll to
	 */
	public void startScrollTimer(final Table destination) {
		startScrollTimer(destination, 50, 10);
	}

	private void startScrollTimer(final Table destination, int delay, final int cd) {
		if (cd > 0) {
			currentScrollDestination = destination;
			Timer timer = new Timer(delay, new ActionListener() {
				@SuppressWarnings("rawtypes")
				@Override
				public void actionPerformed(ActionEvent e) {
					synchronized (visualization) {
						if (destination == currentScrollDestination) {
							Iterator items = visualization.items(BooleanLiteral.TRUE);
							while (items.hasNext()) {
								VisualItem item = (VisualItem)items.next();
								if (item.canGetString("label") ) {
									String tableName;
									tableName = item.getString("label");
									if (tableName != null) {
										if (tableName.equals(destination.getName())) {
											if (display.isTranformInProgress()) {
												startScrollTimer(destination, 50, cd);
												return;
											}
											Point2D tl = new Point2D.Double(0, 0);
											Point2D br = new Point2D.Double(display.getWidth(), display.getHeight());

											Rectangle2D iBounds = item.getBounds();
											Point2D d1 = display.getTransform().transform(new Point2D.Double(iBounds.getMinX(), iBounds.getMinY()), null);
											Point2D d2 = display.getTransform().transform(new Point2D.Double(iBounds.getMaxX(), iBounds.getMaxY()), null);
											int b = 40;
											Rectangle2D bounds = new Rectangle2D.Double(d1.getX() - b, d1.getY() - b, d2.getX() - d1.getX() + 2 * b, Math.min(20, d2.getY() - d1.getY()) + 2 * b);

											if (currentScrollBounds != null && currentScrollBounds.equals(iBounds)) {
												return;
											} else {
												currentScrollBounds = iBounds.clone();
											}

											double dx = 0;
											double dy = 0;

											double d = tl.getX() - bounds.getMinX();
											if (d > 0) {
												dx = d;
											} else {
												d = br.getX() - bounds.getMaxX();
												if (d < 0) {
													dx = d;
												}
											}
											d = tl.getY() - bounds.getMinY();
											if (d > 0) {
												dy = d;
											} else {
												d = br.getY() - bounds.getMaxY();
												if (d < 0) {
													dy = d;
												}
											}

											if (dx != 0 || dy != 0) {
												display.animatePan(dx, dy, 500);
											}
											startScrollTimer(destination, 510, cd - 1);
											break;
										}
									}
								}
							}
						}
					}
				}
			});
			timer.setRepeats(false);
			timer.start();
		}
	}

	/**
	 * Gets shortest path from root to a given table.
	 *
	 * @param destination the table
	 * @param ignoreInvisibleAssociations if <code>true</code>, find a path over visible associations only
	 * @return shortest path from root to a given table
	 */
	private List<Association> getPathToRoot(Table destination, boolean ignoreInvisibleAssociations, Association newlySelectedAssociation) {
		List<Association> path = new ArrayList<Association>();
		Map<Table, Table> successor = new HashMap<Table, Table>();
		Map<Table, Association> outgoingAssociation = new HashMap<Table, Association>();
		List<Table> agenda = new ArrayList<Table>();
		agenda.add(destination);

		while (!agenda.isEmpty()) {
			Table table = agenda.remove(0);
			for (Association association: incomingAssociations(table, ignoreInvisibleAssociations, newlySelectedAssociation)) {
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
	private Collection<Association> incomingAssociations(Table table, boolean ignoreInvisibleAssociations, Association newlySelectedAssociation) {
		Collection<Association> result = new ArrayList<Association>();
		for (Association association: table.associations) {
			if (association.reversalAssociation.getJoinCondition() != null || ((!ignoreInvisibleAssociations) && (association.reversalAssociation != newlySelectedAssociation)&& (renderedAssociations.containsKey(association) || renderedAssociations.containsKey(association.reversalAssociation)))) {
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
	private Graph getModelGraph(DataModel model, Set<Table> initiallyVisibleTables, boolean expandSubject) {
		tableNodes = new HashMap<net.sf.jailer.datamodel.Table, Node>();
		++tableNodesVersion;
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

		if (table != null) {
			if (!modelEditor.extractionModelFrame.showDisabledAssociations()) {
				initiallyVisibleTables.retainAll(table.closure());
			} else {
				initiallyVisibleTables.retainAll(table.unrestrictedClosure());
			}
		}

		for (Table t: initiallyVisibleTables) {
			showTable(g, t);
		}
		for (Table t: initiallyVisibleTables) {
			addEdges(g, t, null, new ArrayList<Table>(), true, null);
		}
		if (!initiallyVisibleTables.isEmpty()) {
			addEdges(g, table, null, new ArrayList<Table>(), true, null);
		}

		int nAssociatedTables = 0;
		if (table != null) {
			for (Association a: table.associations) {
				if (isVisualizable(a)) {
					++nAssociatedTables;
				}
			}
		}

		if (initiallyVisibleTables.isEmpty() && nAssociatedTables <= 10 && expandSubject) {
			expandTable(g, table, null, false, null);
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
			++tableNodesVersion;
			return true;
		}
		return false;
	}

	/**
	 * Creates a visible node for the given destination table and connects it to the source table.
	 * @param source the source table
	 * @param destination the table to make visible
	 */
	public void showTable(Table source, Table destination) {
		synchronized (visualization) {
			List<Table> toCheck = new ArrayList<Table>();
			toCheck.add(destination);
			addEdges(theGraph, source, null, toCheck, false, new HashSet<Table>(toCheck));
			checkForExpansion(theGraph, toCheck, false);
		}
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
					++tableNodesVersion;
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
			checkForExpansion(theGraph, tablesToKeep, false);
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
	 * @param toRender if not null, the only association to make visible
	 * @param allowedTables
	 *
	 * @return list of newly rendered tables
	 */
	private List<Table> expandTable(final Graph g, final net.sf.jailer.datamodel.Table table, final Association toRender, boolean withLimit, Set<Table> allowedTables) {
		List<Table> result = new ArrayList<Table>();
		if (table != null && (!expandedTables.contains(table) || toRender != null)) {
			List<Table> toCheck = new ArrayList<Table>();
			Set<Table> tables = null;
			if (withLimit) {
				tables = new HashSet<Table>();
				allowedTables = new HashSet<Table>();
				for (Association a: table.associations) {
					if (isVisualizable(a) && !tableNodes.containsKey(a.destination)) {
						tables.add(a.destination);
					}
				}
				allowedTables.addAll(tables);
				applyExpansionLimit(allowedTables, EXPAND_SINGLE_TABLE_LIMIT);
				if (allowedTables.size() == tables.size() - 1) {
					allowedTables.addAll(tables);
				}
			}
			result = addEdges(g, table, toRender, toCheck, false, allowedTables);
			// expandedTables.add(table);
			checkForExpansion(g, toCheck, false);

			if (withLimit) {
				checkForExpansion(g, model.getTables(), true);
				if (allowedTables.size() < tables.size()) {
					modelEditor.addMessageBox(new ExpansionLimitMessage(EXPAND_SINGLE_TABLE_LIMIT, tables.size() - allowedTables.size()) {

						@Override
						protected void showMore() {
							modelEditor.clearMessageBox();
							expandTable(g, table, toRender, true, null);
						}

						@Override
						protected void showAll() {
							modelEditor.clearMessageBox();
							expandTable(g, table, toRender, false, null);
						}
					});
				}
			}
		}

		return result;
	}

	private void applyExpansionLimit(Set<Table> tables, int limit) {
		List<Table> sorted = new ArrayList<Table>(tables);
		Collections.sort(sorted, new Comparator<Table>() {
			@Override
			public int compare(Table o1, Table o2) {
				int d = o2.associations.size() - o1.associations.size();
				if (d == 0) {
					return o2.getName().compareTo(o1.getName());
				} else {
					return d;
				}
			}
		});
		while (tables.size() > limit) {
			tables.remove(sorted.remove(0));
		}
	}

	/**
	 * Checks whether some tables are still expanded.
	 *
	 * @param g the graph
	 * @param toCheck set of tables to check
	 */
	private void checkForExpansion(Graph g, java.util.Collection<Table> toCheck, boolean dontAddEdges) {
		for (Table t: toCheck) {
			if (!expandedTables.contains(t)) {
				if (!dontAddEdges) {
					addEdges(g, t, null, new ArrayList<Table>(), true, null);
				}
				boolean isExpanded = true;
				for (Association a: t.associations) {
					if (!isVisualizable(a)) {
						continue;
					}
					if (a.source != t && !tableNodes.containsKey(a.source) && (dontAddEdges || !toCheck.contains(a.source))) {
						isExpanded = false;
						break;
					}
					if (a.destination != t && !tableNodes.containsKey(a.destination) && (dontAddEdges || !toCheck.contains(a.destination))) {
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
	private List<Table> addEdges(Graph g, Table table, Association toRender, List<Table> toCheck, boolean visibleDestinationRequired, Set<Table> allowedTables) {
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
			if (allowedTables != null && !allowedTables.contains(a.destination)) {
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
				String pending = "";
				if (a.getDataModel().decisionPending.contains(a.getName()) || a.getDataModel().decisionPending.contains(a.reversalAssociation.getName())) {
					pending = "This association was added after the last change to this extraction model. "
							+ "Use the Model Migration Tool to decide whether to accept it.<hr>";
				}
				String tooltip = "<html>" + pending + associationToolTip(a) + "</html>";
				if (!associationIsUnique(a)) {
					String reversalTooltip = "<html>" + pending + associationToolTip(a.reversalAssociation) + "</html>";
					Node an = g.addNode();
					an.set("association", a);
					an.setString("label", a.getName() + "#");
					an.setString("tooltip", tooltip);
					renderedAssociationsAsNode.put(a, an);
					Edge ae = g.addEdge(an, tableNodes.get(a.source));
					ae.set("association", a.reversalAssociation);
					ae.set("full", Boolean.FALSE);
					ae.setString("tooltip", reversalTooltip);
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
	 * Creates the tool tip for an association: the join condition with the aliases A and B
	 * resolved into the table names. For a reflexive association the aliases are kept (resolving
	 * them would render conditions like "B.tenant=A.tenant" as an apparent tautology) and a line
	 * explaining the direction of this association is added instead.
	 *
	 * @param a the association
	 * @return HTML fragment (without the enclosing html-tag)
	 */
	private String associationToolTip(Association a) {
		DataModel dataModel = a.getDataModel();
		String joinCondition = a.getUnrestrictedJoinCondition();
		if (a.reversed) {
			joinCondition = SqlUtil.reversRestrictionCondition(joinCondition);
		}
		String sourceName = dataModel.getDisplayName(a.source);
		String destinationName = dataModel.getDisplayName(a.destination);
		boolean reflexive = a.source == a.destination;
		if (!reflexive) {
			joinCondition = SqlUtil.replaceAliases(joinCondition, sourceName, destinationName);
		}
		StringBuilder tooltip = new StringBuilder(UIUtil.toHTMLFragment(joinCondition, 250) + "<br><hr>");
		if (reflexive) {
			String type = "associated with";
			if (a.isInsertDestinationBeforeSource()) {
				type = "depends on (has parent)";
			} else if (a.isInsertSourceBeforeDestination()) {
				type = "has dependent (has child)";
			}
			tooltip.append(UIUtil.toHTMLFragment(sourceName + " A " + type + " " + destinationName + " B", 250) + "<br>");
		}
		return tooltip.append("FK:&nbsp;<i><font color=" + Colors.HTMLColor_0000dd + ">"
				+ UIUtil.toHTMLFragment(a.reversed? a.reversalAssociation.getName() : a.getName(), 0)
				+ "</font></i>").toString();
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
		public void mousePressed(MouseEvent e) {
			UIUtil.invokeLater(new Runnable() {
				@Override
				public void run() {
					modelEditor.exportButton.grabFocus();
				}
			});
			super.mousePressed(e);
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
			zoomToFit(duration);
		}

		/**
		 * Zooms to fit.
		 */
		public void zoomToFit(long dur) {
			resetScrollTimer();
			Visualization vis = display.getVisualization();
			synchronized (visualization) {
				Rectangle2D bounds = vis.getBounds(Visualization.ALL_ITEMS);
				GraphicsLib.expand(bounds, 50 + (int)(1/display.getScale()));
				DisplayLib.fitViewToBounds(display, bounds, dur);
			}
		}
	}

	/**
	 * Renderer for {@link Association}s.
	 */
	private final class CompositeAssociationRenderer implements Renderer {
		public CompositeAssociationRenderer(DataModel dataModel) {
			associationRenderer = new AssociationRenderer(dataModel, false);
			reversedAssociationRenderer = new AssociationRenderer(dataModel, true);
			associationFullRenderer = new AssociationRenderer(dataModel);

		}
		private AssociationRenderer associationRenderer;
		private AssociationRenderer reversedAssociationRenderer;
		private AssociationRenderer associationFullRenderer;
		public boolean useAssociationRendererForLocation = false;

		public boolean locatePointWithAssociationRenderer(Point2D p, VisualItem item) {
			return associationRenderer.locatePoint(p, item);
		}

		@Override
		public boolean locatePoint(Point2D p, VisualItem item) {
			if (useAssociationRendererForLocation) {
				return locatePointWithAssociationRenderer(p, item);
			}
			return associationFullRenderer.locatePoint(p, item);
		}

		@Override
		public void render(Graphics2D g, VisualItem item) {
			item.setInteractive(true);
			boolean isSelected = selectedAssociation != null && selectedAssociation.equals(item.get("association"));
			boolean isReversedSelected = selectedAssociation != null && selectedAssociation.reversalAssociation.equals(item.get("association"));
			associationFullRenderer.render(g, item, isSelected);
			associationRenderer.render(g, item, isSelected);
			reversedAssociationRenderer.render(g, item, isReversedSelected);
		}

		@Override
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
	public boolean isVisualizable(Association association) {
		return modelEditor.extractionModelFrame.showDisabledAssociations()
			|| !association.isIgnored();
	}

	/**
	 * Expands all tables.
	 * @param expandOnlyVisibleTables <code>true</code> to expand only tables already visible in the graph
	 * @param withLimit <code>true</code> to stop expansion after reaching the expansion limit
	 */
	public void expandAll(final boolean expandOnlyVisibleTables, boolean withLimit) {
		modelEditor.captureLayout();
		try {
			List<Table> toExpand = new LinkedList<Table>();
			toExpand.addAll(tableNodes.keySet());
			Set<Table> allowedTables = null;
			int initialNumTabs = tableNodes.size();
			int initialNumAllowedWOLimit = 0;
			if (expandOnlyVisibleTables) {
				initialNumAllowedWOLimit = retrieveAllowedTables(toExpand, Integer.MAX_VALUE).size();
			}
			synchronized (visualization) {
				while (!toExpand.isEmpty()) {
					List<Table> toExpandNext = new ArrayList<Table>();
					if (withLimit) {
						int limit = EXPAND_LIMIT - (tableNodes.size() - initialNumTabs);
						if (limit <= 0) {
							break;
						}
						allowedTables = retrieveAllowedTables(toExpand, limit);
					}
					while (!toExpand.isEmpty()) {
						Table table = toExpand.remove(0);
						List<Table> tables = expandTable(theGraph, table, null, false, allowedTables);
						if (!expandOnlyVisibleTables) {
							toExpandNext.addAll(tables);
						}
						if (!withLimit) {
							if (tableNodes.size() - initialNumTabs > 100) {
								new Thread(new Runnable() {
									@Override
									public void run() {
										try {
											Thread.sleep(1000);
										} catch (InterruptedException e) {
											// ignore
										}
										UIUtil.invokeLater(new Runnable() {
											@Override
											public void run() {
												if (modelEditor.graphView == GraphicalDataModelView.this) {
													modelEditor.incCaptureLevel();
													try {
														expandAll(expandOnlyVisibleTables, false);
													} finally {
														modelEditor.decCaptureLevel();
													}
												}
											}
										});
									}
								}).start();
								toExpand.clear();
								toExpandNext.clear();
								break;
							}
						}
					}
					toExpand.addAll(toExpandNext);
				}
				if (withLimit) {
					int limit = EXPAND_LIMIT - (tableNodes.size() - initialNumTabs);
					if (limit <= 0) {
						int l = EXPAND_LIMIT;
						int size = model.getTables().size();
						if (root != null) {
							if (!modelEditor.extractionModelFrame.showDisabledAssociations()) {
								size = root.closure().size();
							} else {
								size = root.unrestrictedClosure().size();
							}
						}
						int numAll = size - tableNodes.size();
						if (expandOnlyVisibleTables) {
							numAll = initialNumAllowedWOLimit;
						}
						if (numAll > 0) {
							modelEditor.addMessageBox(new ExpansionLimitMessage(l, numAll) {

								@Override
								protected void showMore() {
									modelEditor.clearMessageBox();
									expandAll(expandOnlyVisibleTables, true);
								}

								@Override
								protected void showAll() {
									modelEditor.clearMessageBox();
									expandAll(expandOnlyVisibleTables, false);
								}
							});
						}
					}
				}
			}
		} finally {
			GraphicalDataModelView.this.modelEditor.checkLayoutStack();
		}
	}

	private Set<Table> retrieveAllowedTables(List<Table> toExpand, int limit) {
		Set<Table> allowedTables;
		HashSet<Table> tables = new HashSet<Table>();
		allowedTables = new HashSet<Table>();
		for (Table table: toExpand) {
			for (Association a: table.associations) {
				if (isVisualizable(a) && !tableNodes.containsKey(a.destination)) {
					tables.add(a.destination);
				}
			}
		}
		allowedTables.addAll(tables);
		applyExpansionLimit(allowedTables, limit);
		if (allowedTables.size() == tables.size() - 1) {
			allowedTables.addAll(tables);
		}
		return allowedTables;
	}

	/**
	 * Resets expanded/collapsed status of each visible table.
	 */
	public void resetExpandedState() {
		synchronized (visualization) {
			hideTable(null);
			checkForCollapsed(theGraph, tableNodes.keySet());
			checkForExpansion(theGraph, tableNodes.keySet(), false);
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
			try {
				for (int i = visualGraph.getNodeCount() - 1; i >= 0; --i) {
					VisualItem n = (VisualItem) visualGraph.getNode(i);
					n.setFixed(fix);
				}
			} catch (Exception e) {
				// ignore
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
		synchronized (visualization) {
			visualization.invalidateAll();
			visualization.repaint();
		}
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

	/**
	 * Exports the current display to image and HTML map files.
	 * @param overviewImg the target image file
	 * @param overviewHtml the target HTML map file, or <code>null</code>
	 * @throws Exception if the export fails
	 */
	public void exportDisplayToImage(File overviewImg, File overviewHtml) throws Exception {
		Association oldAssociation = selectedAssociation;
		Map<String, Integer> oldTablesOnPath = tablesOnPath;
		try {
			synchronized (this) {
				selectedAssociation = null;
				tablesOnPath = new HashMap<String, Integer>();
				inImageExport = true;
			}
			displayExporter.export(display, overviewImg, overviewHtml, model);
		} finally {
			synchronized (this) {
				inImageExport = false;
				selectedAssociation = oldAssociation;
				tablesOnPath = oldTablesOnPath;
			}
		}
	}

	/**
	 * Opens query builder dialog.
	 *
	 * @param table subject of query
	 * @param usePath if <code>true</code>, immediately build query based on selected path
	   */
	public void openQueryBuilder(Table table, boolean usePath) {
		final QueryBuilderDialog queryBuilderDialog = new QueryBuilderDialog(this.modelEditor.extractionModelFrame);
		queryBuilderDialog.sqlEditButton.setVisible(true);
		queryBuilderDialog.sqlEditButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				DataBrowser dataBrowser = modelEditor.extractionModelFrame.openDataBrowser(root, "");
				if (dataBrowser != null) {
					SQLConsole sqlConsole = dataBrowser.getSqlConsole(true);
					UIUtil.invokeLater(28, () -> {
						sqlConsole.appendStatement(queryBuilderDialog.getSQL() + UIUtil.LINE_SEPARATOR + ";", true, true);
					});
					queryBuilderDialog.setVisible(false);
					queryBuilderDialog.dispose();
					UIUtil.invokeLater(new Runnable() {
						@Override
						public void run() {
							dataBrowser.toFront();
						}
					});
				}
			}
		});
		queryBuilderDialog.buildQuery(table, usePath, true, associationsOnPath, null, model, null, null, false);
	}

	/**
	 * Gets visibility of a table.
	 *
	 * @param table the table
	 * @return <code>true</code> iff table is visible
	 */
	public boolean isTableVisible(Table table) {
		return tableNodes.containsKey(table);
	}

	/**
	 * Selects a table.
	 *
	 * @param table the table
	 */
	public void selectTable(Table table) {
		Association toS = null;
		for (Association a: table.associations) {
			if (renderedAssociations.containsKey(a) || renderedAssociations.containsKey(a.reversalAssociation)) {
				toS = a.reversalAssociation;
				break;
			}
		}
		if (toS == null) {
			GraphicalDataModelView.this.modelEditor.select(table);
		} else {
			GraphicalDataModelView.this.modelEditor.select(toS);
		}
		Association sa = selectedAssociation;
		setSelection(null);
		setSelection(sa);
	}

	public void setRestriction(final Association association, boolean ignore) {
		setSelection(association);
		if (ignore) {
			modelEditor.restrictionEditor.ignore.setSelected(true);
		} else {
			modelEditor.restrictionEditor.restricted.setSelected(true);
		}
		modelEditor.onApply(false);
	}

	public Set<String> visibleItems() {
		Set<String> result = new HashSet<String>();
		synchronized (visualization) {
			Iterator items = visualization.items(BooleanLiteral.TRUE);
			while (items.hasNext()) {
				VisualItem item = (VisualItem)items.next();
				if (item.canGetString("label") ) {
					String tableName;
					tableName = item.getString("label");
					if (tableName != null) {
						result.add(tableName);
					}
				}
			}
		}
		return result;
	}

	public void toggleShowDetails(final Table table) {
		if (reversedShowDetailsTables.contains(table)) {
			reversedShowDetailsTables.remove(table);
		} else {
			reversedShowDetailsTables.add(table);
		}
		synchronized (visualization) {
			visualization.invalidateAll();
			display.invalidate();
		}
	}

	public void setAnimationEnabled(boolean enabled) {
		synchronized (visualization) {
			animate.setEnabled(enabled);
		}
	}

	private static final long serialVersionUID = -5938101712807557555L;

}

