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

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.event.MouseWheelEvent;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Path2D;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Set;
import java.util.WeakHashMap;

import javax.swing.JComponent;

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
	private static final double POINT_RADIUS = 3.5;

	// Tracks every panel currently part of a displayed component tree, so a tile-loaded
	// notification can be broadcast to whichever panel(s) are actually visible right now -
	// not just the one panel instance that happened to trigger the fetch. That instance can
	// otherwise already have been discarded (e.g. DetailsView.setCurrentRow(...) rebuilds its
	// whole content panel on most UI actions) by the time the async fetch completes, in which
	// case calling repaint() only on it would silently do nothing.
	private static final Set<GeometryPreviewPanel> livePanels = Collections.newSetFromMap(new WeakHashMap<GeometryPreviewPanel, Boolean>());

	private final OsmTileLayer tileLayer = new OsmTileLayer();

	private Geometry geometry;
	// null = no manual mouse-wheel zoom yet - use the auto-fit view.
	private Integer manualZoom;
	private final Color lineColor = new Color(0, 90, 200);
	private final Color fillColor = new Color(0, 90, 200, 60);
	private final Color pointColor = new Color(200, 40, 40);

	public GeometryPreviewPanel() {
		setOpaque(true);
		setPreferredSize(new Dimension(200, 150));
		addMouseWheelListener(this::onMouseWheel);
	}

	private void onMouseWheel(MouseWheelEvent e) {
		if (geometry == null || !Geometry.isWgs84(geometry.getSrid())) {
			return;
		}
		double[] bounds = geometry.getBounds();
		if (bounds == null) {
			return;
		}
		int baseZoom = manualZoom != null ? manualZoom : OsmTileLayer.computeView(bounds, getWidth(), getHeight(), PADDING).getZoom();
		int newZoom = baseZoom - e.getWheelRotation(); // scroll up/away = zoom in, the usual map-app convention
		manualZoom = Math.max(0, Math.min(OsmTileLayer.MAX_ZOOM, newZoom));
		e.consume(); // this panel sits inside DetailsView's JScrollPane - don't also scroll that
		repaint();
	}

	public GeometryPreviewPanel(Geometry geometry) {
		this();
		setGeometry(geometry);
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
			OsmTileLayer.MercatorView view = manualZoom == null
					? OsmTileLayer.computeView(bounds, getWidth(), getHeight(), PADDING)
					: OsmTileLayer.computeViewAtZoom(bounds, getWidth(), getHeight(), manualZoom.intValue());
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
		} else if (geo instanceof Geometry.LineString) {
			Path2D path = toPath(((Geometry.LineString) geo).points, t, false);
			g2.setColor(lineColor);
			g2.draw(path);
		} else if (geo instanceof Geometry.Polygon) {
			Geometry.Polygon poly = (Geometry.Polygon) geo;
			Path2D path = new Path2D.Double(Path2D.WIND_EVEN_ODD);
			for (double[][] ring : poly.rings) {
				path.append(toPath(ring, t, true), false);
			}
			g2.setColor(fillColor);
			g2.fill(path);
			g2.setColor(lineColor);
			g2.draw(path);
		} else if (geo instanceof Geometry.GeometryCollection) {
			for (Geometry sub : ((Geometry.GeometryCollection) geo).geometries) {
				draw(g2, sub, t);
			}
		}
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
