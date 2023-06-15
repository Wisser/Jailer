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
package net.sf.jailer.ui.graphical_view;

import java.awt.BorderLayout;
import java.awt.Color;
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
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.subsetting.ScriptFormat;
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
import prefuse.Display;
import prefuse.Visualization;
import prefuse.action.Action;
import prefuse.action.ActionList;
import prefuse.action.RepaintAction;
import prefuse.action.assignment.ColorAction;
import prefuse.action.filter.GraphDistanceFilter;
import prefuse.action.layout.graph.ForceDirectedLayout;
import prefuse.activity.Activity;
import prefuse.controls.Control;
import prefuse.controls.FocusControl;
import prefuse.controls.ToolTipControl;
import prefuse.controls.WheelZoomControl;
import prefuse.controls.ZoomToFitControl;
import prefuse.data.Edge;
import prefuse.data.Graph;
import prefuse.data.Node;
import prefuse.data.Schema;
import prefuse.data.Tuple;
import prefuse.data.event.TupleSetListener;
import prefuse.data.expression.BooleanLiteral;
import prefuse.data.tuple.TupleSet;
import prefuse.render.Renderer;
import prefuse.render.RendererFactory;
import prefuse.render.ShapeRenderer;
import prefuse.util.ColorLib;
import prefuse.util.GraphicsLib;
import prefuse.util.display.DisplayLib;
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
	private volatile boolean layoutHasBeenSet = false;
	private final ExecutionContext executionContext;

	private final ActionList animate;

	/**
	 * Constructor.
	 *
	 * @param model the restricted data model
	 * @param modelEditor enclosing model editor
	 * @param width
	 * @param height initial size
	 */
	public GraphicalDataModelView(final DataModel model, ExtractionModelEditor modelEditor, Table subject, boolean expandSubject, int width, int height, ExecutionContext executionContext) {
		super(new BorderLayout());
		this.executionContext = executionContext;
		this.model = model;
		this.modelEditor = modelEditor;
		this.root = subject;

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
				item.setFillColor(ColorLib.rgba(220,210,0,100));
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
				VisualItem.FILLCOLOR, ColorLib.rgba(255,235,20,75));
		fill.add(VisualItem.FIXED, ColorLib.rgba(255,235,20,75));
		fill.add(VisualItem.HIGHLIGHT, ColorLib.rgba(160,160,0,120));

		ActionList draw = new ActionList();
		draw.add(filter);
		draw.add(fill);
		draw.add(new ColorAction(nodes, VisualItem.STROKECOLOR, 0));
		draw.add(new ColorAction(nodes, VisualItem.TEXTCOLOR, ColorLib.rgb(0,0,0)));
		draw.add(new ColorAction(edges, VisualItem.FILLCOLOR, ColorLib.gray(200)));
		draw.add(new ColorAction(edges, VisualItem.STROKECOLOR, ColorLib.gray(200)));

		animate = new ActionList(Activity.INFINITY);
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
		display.setDamageRedraw(false);
		display.setSize(width, height);
		display.pan(width / 2, height / 2);
		display.setForeground(Color.GRAY);
		display.setBackground(Color.WHITE);

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

		display.setForeground(Color.GRAY);
		display.setBackground(Color.WHITE);

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

		layout = new ForceDirectedLayout(graph) {
			@Override
			protected float getMassValue(VisualItem n) {
				return zoomBoxControl.getRenderer().isBoxItem(n)? 0.01f : showTableDetails? 2.0f : 1.0f;
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
	 * @return the popup menu
	 */
	public JPopupMenu createPopupMenu(final Table table, JMenuItem findPathMenuItem, boolean withNavigation) {
		JPopupMenu popup = new JScrollPopupMenu(true);
		boolean withModifications = modelEditor.getAdditionalPopupMenuItems().isEmpty();

		JMenu navigateTo = null;

		if (withNavigation) {
			navigateTo = new JScrollMenu("Show Associated Table");
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
		dataBrowser.addActionListener(new ActionListener () {
			@Override
			public void actionPerformed(ActionEvent e) {
				modelEditor.extractionModelFrame.openDataBrowser(table, "");
			}
		});
		JMenuItem zoomToFit = new JMenuItem("Zoom To Fit");
		zoomToFit.addActionListener(new ActionListener () {
			@Override
			public void actionPerformed(ActionEvent e) {
				zoomToFit();
			}
		});
		JMenuItem hide = new JMenuItem("Hide " + table.getName());
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
		toggleDetails.addActionListener(new ActionListener () {
			@Override
			public void actionPerformed(ActionEvent e) {
				toggleShowDetails(table);
			}
		});
		JMenuItem mapColumns = new JMenuItem("XML Column Mapping");
		mapColumns.addActionListener(new ActionListener () {
			@Override
			public void actionPerformed(ActionEvent e) {
				modelEditor.openColumnMapper(table);
			}
		});
		mapColumns.setEnabled(modelEditor.scriptFormat.isObjectNotation());

		JMenuItem restrictAll = new JMenuItem("Disable Associations");
		restrictAll.addActionListener(new ActionListener () {
			@Override
			public void actionPerformed(ActionEvent e) {
				modelEditor.ignoreAll(table);
			}
		});
		restrictAll.setEnabled(modelEditor.isIgnoreAllApplicable(table));

		JMenuItem removeRestrictions = new JMenuItem("Remove Restrictions");
		removeRestrictions.addActionListener(new ActionListener () {
			@Override
			public void actionPerformed(ActionEvent e) {
				modelEditor.removeAllRestrictions(table);
			}
		});
		JMenuItem htmlRender = new JMenuItem("Open HTML Render");
		htmlRender.addActionListener(new ActionListener () {
			@Override
			public void actionPerformed(ActionEvent e) {
				modelEditor.extractionModelFrame.openHTMLRender(table);
			}
		});
		JMenuItem queryBuilder = new JMenuItem("Query Builder");
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
			popup.add(mapColumns);
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
			popup.add(insertModeMenu);
			JRadioButtonMenuItem insert = new JRadioButtonMenuItem("Insert");
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
			popup.add(excludeMenu);
			JRadioButtonMenuItem yes = new JRadioButtonMenuItem("Yes");
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
		disable.addActionListener(new ActionListener () {
			@Override
			public void actionPerformed(ActionEvent e) {
				setRestriction(association, true);
			}
		});
		JMenuItem enable = new JMenuItem("Enable Association");
		enable.addActionListener(new ActionListener () {
			@Override
			public void actionPerformed(ActionEvent e) {
				setRestriction(association, false);
			}
		});
		JMenuItem zoomToFit = new JMenuItem("Zoom To Fit");
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
	 * @param storeLayout
	 */
	public void close(boolean storeLayout, boolean removeLayout) {
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
	 */
	public void zoomToFit(long duration) {
		zoomToFitControl.zoomToFit(duration);
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
	 * Creates visible node for given table.
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
				String tooltip = a.getUnrestrictedJoinCondition();
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
						sqlConsole.appendStatement(queryBuilderDialog.getSQL() + UIUtil.LINE_SEPARATOR + ";", true);
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
