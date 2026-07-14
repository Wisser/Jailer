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
package net.sf.jailer.ui.databrowser.geo;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.Window;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowFocusListener;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Path2D;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Set;
import java.util.WeakHashMap;

import javax.swing.JComponent;
import javax.swing.JWindow;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

/**
 * Swing component that renders a 2D preview of a {@link Geometry} - points,
 * lines and (filled) polygons - on top of an OpenStreetMap tile background,
 * auto-fitted and centered within the panel. No JavaFX/WebView - plain
 * {@link Graphics2D} plus {@link OsmTileLayer} for the raster tiles.
 *
 * <p>Only WGS84 geometries (SRID 4326, or Oracle's geodetic 8307) are ever
 * handed to this panel - see {@link SpatialCellSupport#parse(Object)}, which
 * returns <code>null</code> (falling back to plain text) for anything else,
 * since there is no map to show a non-WGS84 geometry on. The background map
 * is always shown, not an opt-in toggle - the user asked for the preview to
 * only appear when a background map is actually available for it.
 *
 * @author Ralf Wisser
 */
public class GeometryPreviewPanel extends JComponent {

	private static final long serialVersionUID = 1L;

	private static final int PADDING = 10;
	private static final double POINT_RADIUS = 5.0;
	private static final float POINT_OUTLINE_WIDTH = 1.5f;
	private static final float LINE_WIDTH = 2.2f;
	private static final float LINE_HALO_WIDTH = LINE_WIDTH + 2.0f;

	// Tracks every panel currently part of a displayed component tree, so a tile-loaded
	// notification can be broadcast to whichever panel(s) are actually visible right now -
	// not just the one panel instance that happened to trigger the fetch. That instance can
	// otherwise already have been discarded (e.g. DetailsView.setCurrentRow(...) rebuilds its
	// whole content panel on most UI actions) by the time the async fetch completes, in which
	// case calling repaint() only on it would silently do nothing.
	private static final Set<GeometryPreviewPanel> livePanels = Collections.newSetFromMap(new WeakHashMap<GeometryPreviewPanel, Boolean>());

	private static final int DRAG_THRESHOLD_PX = 4;
	private static final double POPUP_SIZE_FRACTION = 0.9;

	private final OsmTileLayer tileLayer = new OsmTileLayer();

	private Geometry geometry;
	// null = no manual mouse-wheel zoom yet - use the auto-fit view.
	private Integer manualZoom;
	// null = no manual drag yet - use the geometry's own bbox center. Stored as normalized
	// Web Mercator world-fraction { x, y } (see WebMercator) rather than lon/lat so it stays
	// valid across zoom changes without needing re-projecting.
	private double[] manualCenterFraction;
	private final boolean popupEnabled;
	// Set only on the popup's own internal panel: a plain click dismisses the popup instead
	// of (the disabled) opening of another one.
	private Runnable dismissAction;
	private final Color lineColor = new Color(0, 90, 200);
	private final Color fillColor = new Color(0, 90, 200, 60);
	private final Color pointColor = new Color(200, 40, 40);

	public GeometryPreviewPanel() {
		this(true);
	}

	/**
	 * @param popupEnabled whether a plain click opens the enlarged popup preview - disabled for
	 *        the popup's own internal panel, so clicking inside it doesn't open another one
	 */
	GeometryPreviewPanel(boolean popupEnabled) {
		this.popupEnabled = popupEnabled;
		setOpaque(true);
		setPreferredSize(new Dimension(200, 150));
		addMouseWheelListener(this::onMouseWheel);
		PanAndClickHandler handler = new PanAndClickHandler();
		addMouseListener(handler);
		addMouseMotionListener(handler);
	}

	public GeometryPreviewPanel(Geometry geometry) {
		this();
		setGeometry(geometry);
	}

	GeometryPreviewPanel(Geometry geometry, boolean popupEnabled) {
		this(popupEnabled);
		setGeometry(geometry);
	}

	private void onMouseWheel(MouseWheelEvent e) {
		double[] bounds = activeBoundsOrNull();
		if (bounds == null) {
			return;
		}
		int newZoom = currentZoom(bounds) - e.getWheelRotation(); // scroll up/away = zoom in, the usual map-app convention
		manualZoom = Math.max(0, Math.min(OsmTileLayer.MAX_ZOOM, newZoom));
		// Center stays fixed - zoom is always centered on whatever the current center already is.
		e.consume(); // this panel sits inside DetailsView's JScrollPane - don't also scroll that
		repaint();
	}

	private double[] activeBoundsOrNull() {
		if (geometry == null || !Geometry.isWgs84(geometry.getSrid())) {
			return null;
		}
		return geometry.getBounds();
	}

	private int currentZoom(double[] bounds) {
		return manualZoom != null ? manualZoom : OsmTileLayer.pickBestFitZoom(bounds, getWidth(), getHeight(), PADDING);
	}

	private double[] currentCenterFraction(double[] bounds) {
		if (manualCenterFraction != null) {
			return manualCenterFraction;
		}
		double centerLon = (bounds[0] + bounds[2]) / 2;
		double centerLat = (bounds[1] + bounds[3]) / 2;
		return new double[] { WebMercator.lonToXFraction(centerLon), WebMercator.latToYFraction(centerLat) };
	}

	/**
	 * Handles both drag-to-pan and click-to-enlarge with a single press/drag/release
	 * sequence: a plain click (no significant movement) opens the enlarged popup; any
	 * movement past a small threshold is treated as a pan instead.
	 */
	private class PanAndClickHandler extends MouseAdapter {
		private Point pressPoint;
		private double[] pressCenterFraction;
		private boolean dragged;

		@Override
		public void mousePressed(MouseEvent e) {
			double[] bounds = activeBoundsOrNull();
			if (bounds == null) {
				pressPoint = null;
				return;
			}
			pressPoint = e.getPoint();
			pressCenterFraction = currentCenterFraction(bounds);
			dragged = false;
		}

		@Override
		public void mouseDragged(MouseEvent e) {
			if (pressPoint == null) {
				return;
			}
			double dx = e.getX() - pressPoint.x;
			double dy = e.getY() - pressPoint.y;
			if (!dragged && Math.hypot(dx, dy) < DRAG_THRESHOLD_PX) {
				return;
			}
			dragged = true;
			double[] bounds = activeBoundsOrNull();
			if (bounds == null) {
				return;
			}
			double worldSize = WebMercator.worldSize(currentZoom(bounds));
			manualCenterFraction = new double[] {
					pressCenterFraction[0] - dx / worldSize,
					pressCenterFraction[1] - dy / worldSize
			};
			repaint();
		}

		@Override
		public void mouseReleased(MouseEvent e) {
			if (pressPoint != null && !dragged) {
				if (dismissAction != null) {
					dismissAction.run();
				} else if (popupEnabled) {
					openLargePreviewWindow();
				}
			}
			pressPoint = null;
			pressCenterFraction = null;
		}
	}

	private void openLargePreviewWindow() {
		double[] bounds = activeBoundsOrNull();
		if (bounds == null) {
			return;
		}
		Window owner = SwingUtilities.getWindowAncestor(this);
		if (owner == null) {
			return;
		}

		GeometryPreviewPanel largePanel = new GeometryPreviewPanel(geometry, false);
		largePanel.manualZoom = currentZoom(bounds);
		largePanel.manualCenterFraction = currentCenterFraction(bounds);

		JWindow popup = new JWindow(owner);
		largePanel.dismissAction = popup::dispose; // a plain click on the popup closes it again
		popup.setLayout(new BorderLayout());
		popup.add(largePanel, BorderLayout.CENTER);
		resizePopupToOwner(popup, owner);

		// Keep the popup at 90% of the owner window's size and centered on it if the owner is
		// resized or moved while the popup is open - removed again once the popup closes, so
		// repeated open/close cycles don't pile up stale listeners on the owner.
		ComponentAdapter ownerResizeListener = new ComponentAdapter() {
			@Override
			public void componentResized(ComponentEvent e) {
				resizePopupToOwner(popup, owner);
			}
			@Override
			public void componentMoved(ComponentEvent e) {
				resizePopupToOwner(popup, owner);
			}
		};
		owner.addComponentListener(ownerResizeListener);
		popup.addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosed(WindowEvent e) {
				owner.removeComponentListener(ownerResizeListener);
			}
		});

		popup.addWindowFocusListener(new WindowFocusListener() {
			@Override
			public void windowGainedFocus(WindowEvent e) {
			}
			@Override
			public void windowLostFocus(WindowEvent e) {
				popup.dispose();
			}
		});
		popup.getRootPane().registerKeyboardAction(e -> popup.dispose(),
				KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), JComponent.WHEN_IN_FOCUSED_WINDOW);

		popup.setVisible(true);
		largePanel.requestFocusInWindow();
	}

	private static void resizePopupToOwner(JWindow popup, Window owner) {
		popup.setSize(Math.max(50, Math.round(owner.getWidth() * (float) POPUP_SIZE_FRACTION)),
				Math.max(50, Math.round(owner.getHeight() * (float) POPUP_SIZE_FRACTION)));
		popup.setLocationRelativeTo(owner);
	}

	public void setGeometry(Geometry geometry) {
		this.geometry = geometry;
		repaint();
	}

	public Geometry getGeometry() {
		return geometry;
	}

	@Override
	public void addNotify() {
		super.addNotify();
		livePanels.add(this);
	}

	@Override
	public void removeNotify() {
		livePanels.remove(this);
		super.removeNotify();
	}

	/** Repaints every panel currently part of a displayed component tree. */
	private static void repaintAllLivePanels() {
		for (GeometryPreviewPanel panel : new ArrayList<GeometryPreviewPanel>(livePanels)) {
			panel.repaint();
		}
	}

	@Override
	protected void paintComponent(Graphics g) {
		super.paintComponent(g);
		Graphics2D g2 = (Graphics2D) g.create();
		try {
			g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
			g2.setColor(getBackground());
			g2.fillRect(0, 0, getWidth(), getHeight());
			if (geometry == null || !Geometry.isWgs84(geometry.getSrid())) {
				return;
			}
			double[] bounds = geometry.getBounds();
			if (bounds == null) {
				return;
			}
			double[] centerFraction = currentCenterFraction(bounds);
			OsmTileLayer.MercatorView view = OsmTileLayer.computeViewAtZoomAndCenter(
					centerFraction[0], centerFraction[1], getWidth(), getHeight(), currentZoom(bounds), 1.0);
			tileLayer.paint(g2, view, getWidth(), getHeight(), GeometryPreviewPanel::repaintAllLivePanels);
			draw(g2, geometry, view);
			double centerLat = (bounds[1] + bounds[3]) / 2;
			ScaleBar.paint(g2, view, getWidth(), getHeight(), centerLat, OsmTileLayer.ATTRIBUTION_HEIGHT);
		} finally {
			g2.dispose();
		}
	}

	private void draw(Graphics2D g2, Geometry geo, CoordinateTransform t) {
		if (geo instanceof Geometry.Point) {
			Geometry.Point p = (Geometry.Point) geo;
			double[] sp = t.toScreen(p.x, p.y);
			Ellipse2D dot = new Ellipse2D.Double(sp[0] - POINT_RADIUS, sp[1] - POINT_RADIUS, POINT_RADIUS * 2, POINT_RADIUS * 2);
			g2.setColor(pointColor);
			g2.fill(dot);
			// White outline so the marker stays visually distinct on any tile color underneath
			// (e.g. OSM's own red road/POI markers), not just wherever it happens to contrast.
			java.awt.Stroke originalStroke = g2.getStroke();
			g2.setColor(Color.WHITE);
			g2.setStroke(new java.awt.BasicStroke(POINT_OUTLINE_WIDTH));
			g2.draw(dot);
			g2.setStroke(originalStroke);
		} else if (geo instanceof Geometry.LineString) {
			Path2D path = toPath(((Geometry.LineString) geo).points, t, false);
			drawHaloedLine(g2, path);
		} else if (geo instanceof Geometry.Polygon) {
			Geometry.Polygon poly = (Geometry.Polygon) geo;
			Path2D path = new Path2D.Double(Path2D.WIND_EVEN_ODD);
			for (double[][] ring : poly.rings) {
				path.append(toPath(ring, t, true), false);
			}
			g2.setColor(fillColor);
			g2.fill(path);
			drawHaloedLine(g2, path);
		} else if (geo instanceof Geometry.GeometryCollection) {
			for (Geometry sub : ((Geometry.GeometryCollection) geo).geometries) {
				draw(g2, sub, t);
			}
		}
	}

	/**
	 * Draws a line (or polygon outline) with a white "casing" halo underneath, the same
	 * contrast-over-arbitrary-tile-colors idea already used for the point marker/scale bar/
	 * attribution text - a plain 1px colored line can otherwise disappear into similarly
	 * colored tile content (e.g. OSM's own road rendering).
	 */
	private void drawHaloedLine(Graphics2D g2, Path2D path) {
		java.awt.Stroke originalStroke = g2.getStroke();
		g2.setColor(Color.WHITE);
		g2.setStroke(new java.awt.BasicStroke(LINE_HALO_WIDTH, java.awt.BasicStroke.CAP_ROUND, java.awt.BasicStroke.JOIN_ROUND));
		g2.draw(path);
		g2.setColor(lineColor);
		g2.setStroke(new java.awt.BasicStroke(LINE_WIDTH, java.awt.BasicStroke.CAP_ROUND, java.awt.BasicStroke.JOIN_ROUND));
		g2.draw(path);
		g2.setStroke(originalStroke);
	}

	private Path2D toPath(double[][] points, CoordinateTransform t, boolean closed) {
		Path2D path = new Path2D.Double();
		for (int i = 0; i < points.length; i++) {
			double[] sp = t.toScreen(points[i][0], points[i][1]);
			if (i == 0) {
				path.moveTo(sp[0], sp[1]);
			} else {
				path.lineTo(sp[0], sp[1]);
			}
		}
		if (closed) {
			path.closePath();
		}
		return path;
	}

}
