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
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
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
import java.awt.geom.Line2D;
import java.awt.geom.Path2D;
import java.awt.geom.PathIterator;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.WeakHashMap;
import java.util.function.Function;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JWindow;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

import net.sf.jailer.ui.UIUtil;

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

	// Hover-highlight amplification and hit-test tolerance for the SQL Console's per-row tooltip
	// feature (see tooltipProvider) - a hovered leaf geometry gets a bigger/brighter marker or a
	// thicker/brighter outline, and hit-testing is forgiving by a few pixels so thin lines/points
	// remain easy to hit.
	private static final double POINT_RADIUS_HOVER_EXTRA = 3.0;
	private static final float LINE_WIDTH_HOVER_EXTRA = 1.5f;
	private static final double HOVER_HIT_TOLERANCE = 4.0;

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
	// Overrides the default click-to-enlarge-popup behavior with a custom action (e.g. the SQL
	// Console's corner/full-cover overlay toggle) - null (the default) leaves popup behavior unchanged.
	private Runnable clickAction;
	private final Color lineColor = new Color(0, 90, 200);
	private final Color fillColor = new Color(0, 90, 200, 60);
	private final Color pointColor = new Color(200, 40, 40);
	// Non-null only while a shift+left-mouse-button zoom-box drag is in progress, in panel-local
	// screen coordinates - painted on top of everything else, then consumed (zooms in) on release.
	private Rectangle zoomBoxRect;

	private static final int SETTINGS_BUTTON_SIZE = 32;
	private static final int SETTINGS_OVERLAY_MARGIN = 4;
	private static final int LEGEND_PANEL_WIDTH = 180;
	private static final int LEGEND_PANEL_MAX_HEIGHT = 160;
	private static final int LEGEND_SWATCH_SIZE = 12;
	// Small allowance for the legend JScrollPane's own LineBorder insets, so the content-height
	// calculation in layoutOverlayControls() doesn't clip the last row/its border by a pixel or two.
	private static final int LEGEND_SCROLLPANE_BORDER_ALLOWANCE = 4;
	// Translucent, not solid white - "Tabellenhintergrund mit transparenter Farbe, die etwas deckt" -
	// legible over arbitrary tile colors while still letting the map show through underneath.
	private static final Color LEGEND_BACKGROUND = new Color(255, 255, 255, 180);
	private static final int LEGEND_MOVE_RESIZE_BAR_HEIGHT = 14;
	private static final int LEGEND_RESIZE_GRIP_WIDTH = 14;
	private static final int LEGEND_MIN_WIDTH = 60;
	private static final int LEGEND_MIN_HEIGHT = 40;

	// Only enabled by the SQL Console's map overlay (setSettingsPanelEnabled(true)) - null for
	// every other consumer of this panel (e.g. DetailsView's cell preview/enlarge popup).
	private JButton settingsToggleButton;
	private JButton settingsSideButton;
	private JPanel settingsPanel;
	private JPanel settingsContentContainer;

	// Only set by the SQL Console's map overlay - null (the default) means every geo-object uses
	// the default lineColor/fillColor/pointColor, same as before this feature existed.
	private Function<Geometry, Color> geometryColorProvider;
	private List<LegendEntry> legendEntries;
	// legendContainer (added to `this`) wraps legendScrollPane (content) + a bottom move/resize bar -
	// moving/resizing it manipulates ITS OWN bounds within this panel directly (no top-level window
	// involved, unlike net.sf.jailer.ui.util.MovePanel/SizeGrip, which only move/resize their window
	// ancestor and so can't be reused here).
	private JPanel legendContainer;
	private JScrollPane legendScrollPane;
	private JPanel legendListPanel;
	// null = not yet customized by the user - use the existing default top-left/content-fit sizing;
	// non-null = the user's own drag/resize result, used verbatim (clamped to the panel's current
	// size in layoutOverlayControls(), in case the panel itself shrank since).
	private Rectangle legendManualBounds;
	// Non-null while the cursor is over a legend row - every geo-object assigned that exact color
	// (i.e. every row grouped under that legend entry) is highlighted, not just one.
	private Color hoveredLegendColor;

	/** One row of the map overlay's legend: a label (from the user-chosen column) and its swatch color. */
	public static final class LegendEntry {
		public final String label;
		public final Color color;

		public LegendEntry(String label, Color color) {
			this.label = label;
			this.color = color;
		}
	}

	private final JPopupMenu popupMenu;
	private final JMenuItem resetZoomMenuItem;

	// Only set by the SQL Console's map overlay - null (the default) fully disables the
	// hover-tooltip/highlight feature for every other consumer (e.g. DetailsView). Maps a specific
	// leaf geometry instance (identity, not equals/hashCode - Geometry has neither) to pre-built
	// tooltip HTML; the geo package itself never needs to know what a Row/TableModel is.
	private Function<Geometry, String> tooltipProvider;
	// The specific leaf Point/LineString/Polygon instance currently under the cursor, or null.
	private Geometry hoveredGeometry;

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
		setPreferredSize(new Dimension(500, 150));
		addMouseWheelListener(this::onMouseWheel);
		PanAndClickHandler handler = new PanAndClickHandler();
		addMouseListener(handler);
		addMouseMotionListener(handler);

		resetZoomMenuItem = new JMenuItem("Reset zoom");
		resetZoomMenuItem.addActionListener(e -> resetZoom());
		popupMenu = new JPopupMenu();
		popupMenu.add(resetZoomMenuItem);
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

	/** Clears any manual mouse-wheel zoom / drag-pan / zoom-box state, reverting to the auto-fit view. */
	private void resetZoom() {
		manualZoom = null;
		manualCenterFraction = null;
		repaint();
	}

	private void showPopupMenu(MouseEvent e) {
		resetZoomMenuItem.setEnabled(manualZoom != null || manualCenterFraction != null);
		popupMenu.show(this, e.getX(), e.getY());
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

	private OsmTileLayer.MercatorView currentView(double[] bounds) {
		double[] centerFraction = currentCenterFraction(bounds);
		return OsmTileLayer.computeViewAtZoomAndCenter(centerFraction[0], centerFraction[1], getWidth(), getHeight(), currentZoom(bounds), 1.0);
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
		// True only when the drag started as shift+left-mouse-button - draws/consumes a zoom-box
		// instead of panning for the rest of this press/drag/release gesture.
		private boolean zoomBoxMode;

		@Override
		public void mousePressed(MouseEvent e) {
			if (e.isPopupTrigger()) { // right-click on some platforms (e.g. Linux) fires it here
				showPopupMenu(e);
				pressPoint = null;
				return;
			}
			double[] bounds = activeBoundsOrNull();
			if (bounds == null) {
				pressPoint = null;
				return;
			}
			pressPoint = e.getPoint();
			pressCenterFraction = currentCenterFraction(bounds);
			dragged = false;
			zoomBoxMode = e.isShiftDown() && SwingUtilities.isLeftMouseButton(e);
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
			if (zoomBoxMode) {
				zoomBoxRect = new Rectangle(pressPoint);
				zoomBoxRect.add(e.getPoint());
				repaint();
				return;
			}
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
			if (e.isPopupTrigger()) { // right-click on most platforms (e.g. Windows) fires it here
				showPopupMenu(e);
				pressPoint = null;
				pressCenterFraction = null;
				zoomBoxMode = false;
				zoomBoxRect = null;
				repaint();
				return;
			}
			if (zoomBoxMode && dragged && zoomBoxRect != null) {
				applyZoomBox(zoomBoxRect);
			} else if (pressPoint != null && !dragged) {
				if (dismissAction != null) {
					dismissAction.run();
				} else if (clickAction != null) {
					clickAction.run();
				} else if (popupEnabled) {
					openLargePreviewWindow();
				}
			}
			pressPoint = null;
			pressCenterFraction = null;
			zoomBoxMode = false;
			zoomBoxRect = null;
			repaint();
		}

		@Override
		public void mouseMoved(MouseEvent e) {
			updateHover(tooltipProvider == null ? null : hitTestOrNull(e.getX(), e.getY()));
		}

		@Override
		public void mouseExited(MouseEvent e) {
			updateHover(null);
		}

		private Geometry hitTestOrNull(double mx, double my) {
			double[] bounds = activeBoundsOrNull();
			if (bounds == null) {
				return null;
			}
			return hitTest(geometry, currentView(bounds), mx, my);
		}

		private void updateHover(Geometry hit) {
			if (tooltipProvider == null && hoveredGeometry == null) {
				return;
			}
			if (hit == hoveredGeometry) {
				return;
			}
			hoveredGeometry = hit;
			setToolTipText(hit != null ? tooltipProvider.apply(hit) : null);
			repaint();
		}
	}

	/**
	 * Zooms in to the map area the user drew with a shift+left-mouse-button drag: translates the
	 * screen rectangle to world-fraction bounds via the current view, then picks the best-fit
	 * zoom/center for that box - the same auto-fit math used for the initial geometry bbox fit,
	 * just applied to an arbitrary user-drawn box instead.
	 */
	private void applyZoomBox(Rectangle rect) {
		double[] bounds = activeBoundsOrNull();
		if (bounds == null) {
			return;
		}
		OsmTileLayer.MercatorView view = currentView(bounds);
		double[] f0 = view.screenToFraction(rect.getMinX(), rect.getMinY());
		double[] f1 = view.screenToFraction(rect.getMaxX(), rect.getMaxY());
		double minXFraction = Math.min(f0[0], f1[0]);
		double maxXFraction = Math.max(f0[0], f1[0]);
		double minYFraction = Math.min(f0[1], f1[1]);
		double maxYFraction = Math.max(f0[1], f1[1]);

		manualZoom = OsmTileLayer.pickBestFitZoomForFraction(minXFraction, minYFraction, maxXFraction, maxYFraction, getWidth(), getHeight(), PADDING);
		manualCenterFraction = new double[] { (minXFraction + maxXFraction) / 2, (minYFraction + maxYFraction) / 2 };
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
		// New geometries are fresh instances anyway (hoveredGeometry couldn't match), but clear
		// explicitly so a stale tooltip doesn't linger if the mouse doesn't move again right away.
		hoveredGeometry = null;
		if (tooltipProvider != null) {
			setToolTipText(null);
		}
		repaint();
	}

	public void setClickAction(Runnable clickAction) {
		this.clickAction = clickAction;
	}

	/**
	 * Enables the hover-tooltip/highlight feature (SQL Console-specific - null, the default,
	 * leaves every other consumer of this panel, e.g. DetailsView, completely unaffected).
	 *
	 * @param tooltipProvider maps a specific leaf geometry instance (identity, not equals/hashCode)
	 *        to pre-built tooltip HTML, or {@code null} to disable the feature again
	 */
	public void setTooltipProvider(Function<Geometry, String> tooltipProvider) {
		this.tooltipProvider = tooltipProvider;
	}

	/**
	 * Adds (or removes) a small top-right toggle button that shows/hides an initially empty
	 * settings panel below it - SQL Console-specific, so this is opt-in rather than always-on
	 * for every consumer of this panel (e.g. DetailsView's cell preview/enlarge popup never
	 * enables this).
	 */
	public void setSettingsPanelEnabled(boolean enabled) {
		if (enabled == (settingsToggleButton != null)) {
			return;
		}
		if (!enabled) {
			remove(settingsToggleButton);
			remove(settingsPanel);
			if (settingsSideButton != null) {
				remove(settingsSideButton);
			}
			settingsToggleButton = null;
			settingsSideButton = null;
			settingsPanel = null;
			settingsContentContainer = null;
			revalidate();
			repaint();
			return;
		}
		settingsToggleButton = new JButton(UIUtil.scaleIcon(UIUtil.readImage("/menu.png"), 20, 18));
		settingsToggleButton.setToolTipText("Settings");
		settingsToggleButton.setFocusPainted(false);
		settingsToggleButton.setMargin(new Insets(0, 0, 0, 0));
		settingsToggleButton.addActionListener(e -> {
			settingsPanel.setVisible(!settingsPanel.isVisible());
			revalidate();
			repaint();
		});

		settingsPanel = new JPanel(new BorderLayout());
		settingsPanel.setOpaque(true);
		settingsPanel.setBackground(new Color(255, 255, 255, 230));
		settingsPanel.setBorder(BorderFactory.createLineBorder(Color.GRAY));
		settingsPanel.setVisible(false); // collapsed by default

		JButton settingsCloseButton = new JButton(UIUtil.readImage("/Close-16-1.png"));
		settingsCloseButton.setToolTipText("Close");
		settingsCloseButton.setFocusPainted(false);
		settingsCloseButton.setBorderPainted(false);
		settingsCloseButton.setContentAreaFilled(false);
		settingsCloseButton.setMargin(new Insets(0, 0, 0, 0));
		settingsCloseButton.addActionListener(e -> {
			settingsPanel.setVisible(false);
			revalidate();
			repaint();
		});
		JPanel settingsHeader = new JPanel(new FlowLayout(FlowLayout.RIGHT, 2, 2));
		settingsHeader.setOpaque(false);
		settingsHeader.add(settingsCloseButton);
		settingsPanel.add(settingsHeader, BorderLayout.NORTH);

		settingsContentContainer = new JPanel(new BorderLayout());
		settingsContentContainer.setOpaque(false);
		settingsPanel.add(settingsContentContainer, BorderLayout.CENTER);

		add(settingsToggleButton);
		add(settingsPanel);
		layoutOverlayControls();
		revalidate();
		repaint();
	}

	/**
	 * Installs {@code content} (e.g. the SQL Console's legend-column/palette comboboxes) as the
	 * settings panel's content - lets the caller build whatever controls it needs without this
	 * (geo) package needing to know what a {@code Row}/{@code TableModel} is. Requires
	 * {@link #setSettingsPanelEnabled(boolean)} to have been enabled first.
	 */
	public void setSettingsPanelContent(JComponent content) {
		settingsContentContainer.removeAll();
		settingsContentContainer.add(content, BorderLayout.CENTER);
		settingsContentContainer.revalidate();
		settingsContentContainer.repaint();
	}

	/**
	 * Installs a caller-supplied button immediately left of the settings toggle button, at the same
	 * height/size (e.g. the SQL Console map's "Save as..." export button). Requires
	 * {@link #setSettingsPanelEnabled(boolean)} to have been enabled first.
	 */
	public void setSettingsSideButton(JButton button) {
		if (settingsSideButton != null) {
			remove(settingsSideButton);
		}
		settingsSideButton = button;
		if (button != null) {
			add(button);
		}
		layoutOverlayControls();
		revalidate();
		repaint();
	}

	/**
	 * Sets (or clears) the per-row legend and per-geometry coloring - SQL Console-specific, opt-in
	 * like {@link #setTooltipProvider}. {@code entries}/{@code colorProvider} null or empty means
	 * "no legend" - every geo-object reverts to the default lineColor/fillColor/pointColor.
	 *
	 * @param entries one entry per geo-object, in the same order/identity as the rows they came from
	 * @param colorProvider maps a specific leaf geometry instance (identity, not equals/hashCode) to
	 *        its assigned color, or {@code null} to disable per-object coloring again
	 */
	public void setLegend(List<LegendEntry> entries, Function<Geometry, Color> colorProvider) {
		this.legendEntries = entries;
		this.geometryColorProvider = colorProvider;
		rebuildLegendUI();
		repaint();
	}

	private void rebuildLegendUI() {
		if (legendEntries == null || legendEntries.isEmpty()) {
			if (legendContainer != null) {
				legendContainer.setVisible(false);
			}
			return;
		}
		if (legendContainer == null) {
			// setOpaque(true) + a translucent background Color is unreliable in Swing - the
			// framework's own "opaque" background fill (done before paintComponent runs) doesn't
			// reliably alpha-blend on every L&F/rendering pipeline, and can render as fully opaque
			// instead ("es ist jetzt deckend weiss"). Painting the fill manually via Graphics2D with
			// setOpaque(false) blends correctly regardless of pipeline.
			legendListPanel = new JPanel() {
				@Override
				protected void paintComponent(Graphics g) {
					g.setColor(LEGEND_BACKGROUND);
					g.fillRect(0, 0, getWidth(), getHeight());
					super.paintComponent(g);
				}
			};
			legendListPanel.setLayout(new BoxLayout(legendListPanel, BoxLayout.Y_AXIS));
			legendListPanel.setOpaque(false);
			legendScrollPane = new JScrollPane(legendListPanel,
					JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
			legendScrollPane.setOpaque(false);
			legendScrollPane.getViewport().setOpaque(false);
			legendScrollPane.setBorder(BorderFactory.createLineBorder(Color.GRAY));
			// A click anywhere in the legend area toggles the corner/full-cover map presentation,
			// even where there's no row to click on (e.g. blank space below the last entry) -
			// attached to the container/scrollpane/viewport, not just individual rows.
			MouseAdapter legendAreaClickHandler = new MouseAdapter() {
				@Override
				public void mouseClicked(MouseEvent e) {
					if (clickAction != null) {
						clickAction.run();
					}
				}
			};
			legendListPanel.addMouseListener(legendAreaClickHandler);
			legendScrollPane.addMouseListener(legendAreaClickHandler);
			legendScrollPane.getViewport().addMouseListener(legendAreaClickHandler);

			// Move/resize handled directly here (manipulating legendContainer's own bounds within
			// this panel) rather than via net.sf.jailer.ui.util.MovePanel/SizeGrip, which only
			// move/resize their top-level Window ancestor and so can't be used for an embedded
			// component - no top-level window is involved at all.
			JPanel moveHandle = new JPanel();
			moveHandle.setOpaque(false);
			moveHandle.setCursor(Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR));
			LegendMoveHandler moveHandler = new LegendMoveHandler();
			moveHandle.addMouseListener(moveHandler);
			moveHandle.addMouseMotionListener(moveHandler);

			JPanel resizeGrip = new JPanel();
			resizeGrip.setOpaque(false);
			resizeGrip.setCursor(Cursor.getPredefinedCursor(Cursor.SE_RESIZE_CURSOR));
			resizeGrip.setPreferredSize(new Dimension(LEGEND_RESIZE_GRIP_WIDTH, LEGEND_MOVE_RESIZE_BAR_HEIGHT));
			LegendResizeHandler resizeHandler = new LegendResizeHandler();
			resizeGrip.addMouseListener(resizeHandler);
			resizeGrip.addMouseMotionListener(resizeHandler);

			JPanel moveResizeBar = new JPanel(new GridBagLayout());
			moveResizeBar.setOpaque(false);
			GridBagConstraints moveConstraints = new GridBagConstraints();
			moveConstraints.gridx = 0;
			moveConstraints.weightx = 1;
			moveConstraints.fill = GridBagConstraints.BOTH;
			moveResizeBar.add(moveHandle, moveConstraints);
			GridBagConstraints gripConstraints = new GridBagConstraints();
			gripConstraints.gridx = 1;
			gripConstraints.fill = GridBagConstraints.BOTH;
			moveResizeBar.add(resizeGrip, gripConstraints);

			legendContainer = new JPanel(new BorderLayout());
			legendContainer.setOpaque(false);
			legendContainer.add(legendScrollPane, BorderLayout.CENTER);
			legendContainer.add(moveResizeBar, BorderLayout.SOUTH);
			add(legendContainer);
		}
		legendListPanel.removeAll();
		for (LegendEntry entry : legendEntries) {
			JLabel row = new JLabel(entry.label == null || entry.label.isEmpty() ? " " : entry.label);
			row.setIcon(new SwatchIcon(entry.color));
			row.setToolTipText(entry.label);
			row.setOpaque(false);
			// Hovering a legend row highlights every geo-object sharing its color (i.e. every row
			// grouped under this same legend entry, not just one); a plain click toggles the
			// corner/full-cover map presentation, same as clicking the map itself.
			row.addMouseListener(new MouseAdapter() {
				@Override
				public void mouseEntered(MouseEvent e) {
					hoveredLegendColor = entry.color;
					repaint();
				}

				@Override
				public void mouseExited(MouseEvent e) {
					hoveredLegendColor = null;
					repaint();
				}

				@Override
				public void mouseClicked(MouseEvent e) {
					if (clickAction != null) {
						clickAction.run();
					}
				}
			});
			legendListPanel.add(row);
		}
		legendContainer.setVisible(true);
		legendContainer.revalidate();
		layoutOverlayControls();
	}

	/**
	 * Drags {@link #legendContainer} by manipulating its own bounds within this panel (no window
	 * involved) - mirrors {@code PanAndClickHandler}'s press/drag/release-with-threshold structure.
	 * A plain click (no significant drag) falls through to the existing "click legend area toggles
	 * corner/full-cover" behavior instead of moving it.
	 */
	private class LegendMoveHandler extends MouseAdapter {
		private Point pressPoint;
		private Rectangle startBounds;
		private boolean dragged;

		@Override
		public void mousePressed(MouseEvent e) {
			pressPoint = SwingUtilities.convertPoint(e.getComponent(), e.getPoint(), GeometryPreviewPanel.this);
			startBounds = legendContainer.getBounds();
			dragged = false;
		}

		@Override
		public void mouseDragged(MouseEvent e) {
			if (pressPoint == null) {
				return;
			}
			Point current = SwingUtilities.convertPoint(e.getComponent(), e.getPoint(), GeometryPreviewPanel.this);
			int dx = current.x - pressPoint.x;
			int dy = current.y - pressPoint.y;
			if (!dragged && Math.hypot(dx, dy) < DRAG_THRESHOLD_PX) {
				return;
			}
			dragged = true;
			int maxX = Math.max(0, getWidth() - startBounds.width);
			int maxY = Math.max(0, getHeight() - startBounds.height);
			int newX = Math.max(0, Math.min(startBounds.x + dx, maxX));
			int newY = Math.max(0, Math.min(startBounds.y + dy, maxY));
			legendManualBounds = new Rectangle(newX, newY, startBounds.width, startBounds.height);
			legendContainer.setBounds(legendManualBounds);
			repaint();
		}

		@Override
		public void mouseReleased(MouseEvent e) {
			if (!dragged && clickAction != null) {
				clickAction.run();
			}
			pressPoint = null;
			dragged = false;
		}
	}

	/**
	 * Resizes {@link #legendContainer} by manipulating its own bounds within this panel (no window
	 * involved) - never triggers {@link #clickAction} (a resize gesture is never a plain click).
	 */
	private class LegendResizeHandler extends MouseAdapter {
		private Point pressPoint;
		private Rectangle startBounds;

		@Override
		public void mousePressed(MouseEvent e) {
			pressPoint = SwingUtilities.convertPoint(e.getComponent(), e.getPoint(), GeometryPreviewPanel.this);
			startBounds = legendContainer.getBounds();
		}

		@Override
		public void mouseDragged(MouseEvent e) {
			if (pressPoint == null) {
				return;
			}
			Point current = SwingUtilities.convertPoint(e.getComponent(), e.getPoint(), GeometryPreviewPanel.this);
			int dx = current.x - pressPoint.x;
			int dy = current.y - pressPoint.y;
			int maxW = Math.max(LEGEND_MIN_WIDTH, getWidth() - startBounds.x);
			int maxH = Math.max(LEGEND_MIN_HEIGHT, getHeight() - startBounds.y);
			int newW = Math.max(LEGEND_MIN_WIDTH, Math.min(startBounds.width + dx, maxW));
			int newH = Math.max(LEGEND_MIN_HEIGHT, Math.min(startBounds.height + dy, maxH));
			legendManualBounds = new Rectangle(startBounds.x, startBounds.y, newW, newH);
			legendContainer.setBounds(legendManualBounds);
			repaint();
		}

		@Override
		public void mouseReleased(MouseEvent e) {
			pressPoint = null;
		}
	}

	/** A small colored square, used as a legend row's swatch icon. */
	private static final class SwatchIcon implements Icon {
		private final Color color;

		SwatchIcon(Color color) {
			this.color = color;
		}

		@Override
		public int getIconWidth() {
			return LEGEND_SWATCH_SIZE;
		}

		@Override
		public int getIconHeight() {
			return LEGEND_SWATCH_SIZE;
		}

		@Override
		public void paintIcon(Component c, Graphics g, int x, int y) {
			g.setColor(color);
			g.fillRect(x, y, LEGEND_SWATCH_SIZE, LEGEND_SWATCH_SIZE);
			g.setColor(Color.DARK_GRAY);
			g.drawRect(x, y, LEGEND_SWATCH_SIZE - 1, LEGEND_SWATCH_SIZE - 1);
		}
	}

	@Override
	public void doLayout() {
		super.doLayout();
		layoutOverlayControls();
	}

	private void layoutOverlayControls() {
		if (settingsToggleButton != null) {
			settingsToggleButton.setBounds(getWidth() - SETTINGS_BUTTON_SIZE - SETTINGS_OVERLAY_MARGIN, SETTINGS_OVERLAY_MARGIN,
					SETTINGS_BUTTON_SIZE, SETTINGS_BUTTON_SIZE);
			int panelY = SETTINGS_OVERLAY_MARGIN + SETTINGS_BUTTON_SIZE + 2;
			Dimension pref = settingsPanel.getPreferredSize();
			int panelWidth = Math.min(pref.width, Math.max(0, getWidth() - 2 * SETTINGS_OVERLAY_MARGIN));
			int panelHeight = Math.min(pref.height, Math.max(0, getHeight() - panelY - SETTINGS_OVERLAY_MARGIN));
			settingsPanel.setBounds(getWidth() - panelWidth - SETTINGS_OVERLAY_MARGIN, panelY, panelWidth, panelHeight);
			if (settingsSideButton != null) {
				int w = settingsSideButton.getPreferredSize().width;
				int toggleX = getWidth() - SETTINGS_BUTTON_SIZE - SETTINGS_OVERLAY_MARGIN;
				settingsSideButton.setBounds(toggleX - 2 - w, SETTINGS_OVERLAY_MARGIN, w, SETTINGS_BUTTON_SIZE);
			}
		}
		if (legendContainer != null) {
			if (legendManualBounds != null) {
				// The user has already dragged/resized it - use that verbatim, only clamped to the
				// panel's *current* size in case the panel itself shrank since (e.g. corner/expanded
				// map toggle), so it can't end up partly/fully off-screen.
				int w = Math.min(legendManualBounds.width, getWidth());
				int h = Math.min(legendManualBounds.height, getHeight());
				int x = Math.max(0, Math.min(legendManualBounds.x, getWidth() - w));
				int y = Math.max(0, Math.min(legendManualBounds.y, getHeight() - h));
				legendContainer.setBounds(x, y, w, h);
			} else {
				int w = Math.min(LEGEND_PANEL_WIDTH, Math.max(0, getWidth() - 2 * SETTINGS_OVERLAY_MARGIN));
				// Only as tall as the actual entries need (plus a small border allowance and the
				// move/resize bar), not always the full max height - "mache die Tabelle initial
				// nicht höher, als nötig". Still capped at LEGEND_PANEL_MAX_HEIGHT (and available
				// space) once there are enough entries to need scrolling, so long legends behave
				// exactly as before.
				int contentHeight = legendListPanel.getPreferredSize().height + LEGEND_SCROLLPANE_BORDER_ALLOWANCE + LEGEND_MOVE_RESIZE_BAR_HEIGHT;
				int h = Math.min(contentHeight, Math.min(LEGEND_PANEL_MAX_HEIGHT, Math.max(0, getHeight() - 2 * SETTINGS_OVERLAY_MARGIN)));
				legendContainer.setBounds(SETTINGS_OVERLAY_MARGIN, SETTINGS_OVERLAY_MARGIN, w, h);
			}
		}
	}

	public Geometry getGeometry() {
		return geometry;
	}

	/**
	 * Renders this panel's current map content (tiles/geometry/scale-bar) plus the legend if one is
	 * currently shown, to a new image - e.g. for the SQL Console's "Export" button. Deliberately
	 * excludes the settings toggle button and settings panel (calling {@link #paintComponent} rather
	 * than {@link #paint} excludes every child component by default; the legend is then explicitly
	 * re-painted on top, translated/clipped to its own bounds, the same technique
	 * {@code Container.paintChildren} uses internally) - only the map/legend are "content", the
	 * settings UI is chrome that doesn't belong in an exported image.
	 */
	public BufferedImage renderToImage() {
		int w = Math.max(1, getWidth());
		int h = Math.max(1, getHeight());
		BufferedImage img = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB);
		Graphics2D g2 = img.createGraphics();
		try {
			paintComponent(g2);
			if (legendContainer != null && legendContainer.isVisible()) {
				Graphics2D legendG2 = (Graphics2D) g2.create(
						legendContainer.getX(), legendContainer.getY(), legendContainer.getWidth(), legendContainer.getHeight());
				try {
					legendContainer.paint(legendG2);
				} finally {
					legendG2.dispose();
				}
			}
		} finally {
			g2.dispose();
		}
		return img;
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
			OsmTileLayer.MercatorView view = currentView(bounds);
			tileLayer.paint(g2, view, getWidth(), getHeight(), GeometryPreviewPanel::repaintAllLivePanels);
			draw(g2, geometry, view);
			double centerLat = (bounds[1] + bounds[3]) / 2;
			ScaleBar.paint(g2, view, getWidth(), getHeight(), centerLat, ScaleBar.MARGIN);
			if (zoomBoxRect != null) {
				drawZoomBox(g2, zoomBoxRect);
			}
		} finally {
			g2.dispose();
		}
	}

	private void drawZoomBox(Graphics2D g2, Rectangle rect) {
		g2.setColor(new Color(255, 255, 255, 80));
		g2.fill(rect);
		g2.setColor(Color.WHITE);
		g2.setStroke(new java.awt.BasicStroke(1f));
		g2.draw(rect);
	}

	/**
	 * Recursively finds the leaf {@link Geometry.Point}/{@link Geometry.LineString}/{@link Geometry.Polygon}
	 * (if any) under the given screen coordinates - mirrors {@link #draw(Graphics2D, Geometry, CoordinateTransform)}'s
	 * own recursion/leaf handling, using the same path-building helper ({@link #toPath}) so hit-testing
	 * and rendering never disagree about a geometry's screen shape.
	 */
	private Geometry hitTest(Geometry geo, CoordinateTransform t, double mx, double my) {
		if (geo instanceof Geometry.GeometryCollection) {
			for (Geometry sub : ((Geometry.GeometryCollection) geo).geometries) {
				Geometry hit = hitTest(sub, t, mx, my);
				if (hit != null) {
					return hit;
				}
			}
			return null;
		}
		if (geo instanceof Geometry.Point) {
			Geometry.Point p = (Geometry.Point) geo;
			double[] sp = t.toScreen(p.x, p.y);
			double dist = Math.hypot(sp[0] - mx, sp[1] - my);
			return dist <= POINT_RADIUS + HOVER_HIT_TOLERANCE ? geo : null;
		}
		if (geo instanceof Geometry.LineString) {
			Path2D path = toPath(((Geometry.LineString) geo).points, t, false);
			return hitTestPath(path, mx, my, LINE_WIDTH / 2 + HOVER_HIT_TOLERANCE) ? geo : null;
		}
		if (geo instanceof Geometry.Polygon) {
			Geometry.Polygon poly = (Geometry.Polygon) geo;
			Path2D path = new Path2D.Double(Path2D.WIND_EVEN_ODD);
			for (double[][] ring : poly.rings) {
				path.append(toPath(ring, t, true), false);
			}
			if (path.contains(mx, my) || hitTestPath(path, mx, my, LINE_WIDTH / 2 + HOVER_HIT_TOLERANCE)) {
				return geo;
			}
			return null;
		}
		return null;
	}

	/** @return whether (mx, my) is within {@code tolerance} pixels of any segment of {@code path} */
	private boolean hitTestPath(Path2D path, double mx, double my, double tolerance) {
		double[] coords = new double[6];
		double lastX = 0, lastY = 0, startX = 0, startY = 0;
		PathIterator pi = path.getPathIterator(null);
		while (!pi.isDone()) {
			int type = pi.currentSegment(coords);
			switch (type) {
				case PathIterator.SEG_MOVETO:
					lastX = startX = coords[0];
					lastY = startY = coords[1];
					break;
				case PathIterator.SEG_LINETO:
					if (Line2D.ptSegDist(lastX, lastY, coords[0], coords[1], mx, my) <= tolerance) {
						return true;
					}
					lastX = coords[0];
					lastY = coords[1];
					break;
				case PathIterator.SEG_CLOSE:
					if (Line2D.ptSegDist(lastX, lastY, startX, startY, mx, my) <= tolerance) {
						return true;
					}
					lastX = startX;
					lastY = startY;
					break;
				default:
					break;
			}
			pi.next();
		}
		return false;
	}

	private void draw(Graphics2D g2, Geometry geo, CoordinateTransform t) {
		Color assignedColor = geometryColorProvider != null ? geometryColorProvider.apply(geo) : null;
		boolean hovered = geo == hoveredGeometry || (hoveredLegendColor != null && hoveredLegendColor.equals(assignedColor));
		if (geo instanceof Geometry.Point) {
			Geometry.Point p = (Geometry.Point) geo;
			double[] sp = t.toScreen(p.x, p.y);
			double radius = hovered ? POINT_RADIUS + POINT_RADIUS_HOVER_EXTRA : POINT_RADIUS;
			Ellipse2D dot = new Ellipse2D.Double(sp[0] - radius, sp[1] - radius, radius * 2, radius * 2);
			Color fill = assignedColor != null ? assignedColor : pointColor;
			g2.setColor(hovered ? fill.brighter() : fill);
			g2.fill(dot);
			// White outline so the marker stays visually distinct on any tile color underneath
			// (e.g. OSM's own red road/POI markers), not just wherever it happens to contrast.
			java.awt.Stroke originalStroke = g2.getStroke();
			g2.setColor(Color.WHITE);
			g2.setStroke(new java.awt.BasicStroke(hovered ? POINT_OUTLINE_WIDTH + 1f : POINT_OUTLINE_WIDTH));
			g2.draw(dot);
			g2.setStroke(originalStroke);
		} else if (geo instanceof Geometry.LineString) {
			Path2D path = toPath(((Geometry.LineString) geo).points, t, false);
			drawHaloedLine(g2, path, hovered, assignedColor);
		} else if (geo instanceof Geometry.Polygon) {
			Geometry.Polygon poly = (Geometry.Polygon) geo;
			Path2D path = new Path2D.Double(Path2D.WIND_EVEN_ODD);
			for (double[][] ring : poly.rings) {
				path.append(toPath(ring, t, true), false);
			}
			g2.setColor(assignedColor != null ? withAlpha(assignedColor, fillColor.getAlpha()) : fillColor);
			g2.fill(path);
			drawHaloedLine(g2, path, hovered, assignedColor);
		} else if (geo instanceof Geometry.GeometryCollection) {
			for (Geometry sub : ((Geometry.GeometryCollection) geo).geometries) {
				draw(g2, sub, t);
			}
		}
	}

	private static Color withAlpha(Color color, int alpha) {
		return new Color(color.getRed(), color.getGreen(), color.getBlue(), alpha);
	}

	/**
	 * Draws a line (or polygon outline) with a white "casing" halo underneath, the same
	 * contrast-over-arbitrary-tile-colors idea already used for the point marker/scale bar/
	 * attribution text - a plain 1px colored line can otherwise disappear into similarly
	 * colored tile content (e.g. OSM's own road rendering). When {@code hovered}, both the halo
	 * and the line itself are thicker and the line color is brightened.
	 */
	private void drawHaloedLine(Graphics2D g2, Path2D path, boolean hovered, Color assignedColor) {
		float haloWidth = hovered ? LINE_HALO_WIDTH + LINE_WIDTH_HOVER_EXTRA : LINE_HALO_WIDTH;
		float lineWidth = hovered ? LINE_WIDTH + LINE_WIDTH_HOVER_EXTRA : LINE_WIDTH;
		Color base = assignedColor != null ? assignedColor : lineColor;
		java.awt.Stroke originalStroke = g2.getStroke();
		g2.setColor(Color.WHITE);
		g2.setStroke(new java.awt.BasicStroke(haloWidth, java.awt.BasicStroke.CAP_ROUND, java.awt.BasicStroke.JOIN_ROUND));
		g2.draw(path);
		g2.setColor(hovered ? base.brighter() : base);
		g2.setStroke(new java.awt.BasicStroke(lineWidth, java.awt.BasicStroke.CAP_ROUND, java.awt.BasicStroke.JOIN_ROUND));
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
