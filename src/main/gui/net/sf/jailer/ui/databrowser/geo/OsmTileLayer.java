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
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import javax.imageio.ImageIO;
import javax.swing.SwingWorker;

import net.sf.jailer.util.LogUtil;

/**
 * Optional OpenStreetMap raster tile background for {@link GeometryPreviewPanel},
 * shown only for WGS84 geometries (SRID 4326, or Oracle's geodetic 8307) and
 * only when the user opts in (see {@link GeometryPreviewPanel#setShowBackgroundMap(boolean)}).
 *
 * <p>Tiles are fetched via plain {@link HttpURLConnection} (not
 * {@code java.net.http.HttpClient}, which needs Java 11+ - this project
 * targets Java 8) on a background thread ({@link SwingWorker}) and cached
 * in-memory; a failed/missing tile is simply left blank rather than
 * blocking the UI or crashing. All cache/in-flight bookkeeping here is only
 * ever touched from the EDT (tile fetches are kicked off from
 * {@link #paint}, which runs during painting, and {@code SwingWorker.done()}
 * also runs on the EDT), so no synchronization is needed.
 *
 * @author Ralf Wisser
 */
public class OsmTileLayer {

	private static final int TILE_SIZE = 256;
	public static final int MAX_ZOOM = 18;
	// A Point geometry (by far the most common preview case) has a zero-size bounding box.
	// Flooring the span at something merely non-zero (e.g. 1e-9) makes the zoom-selection
	// loop below always pick MAX_ZOOM - there's no real span to ever exceed the available
	// pixel area - which then requires a huge extra scale to "fill" that near-zero box,
	// blowing up already-maximum-detail tiles into blurry/pixelated blocks. Flooring at a
	// geographically meaningful minimum instead (~400m at the equator) makes a point pick a
	// sensible "neighborhood" zoom, so the extra scale stays close to 1.
	private static final double MIN_SPAN_FRACTION = 1e-5;
	private static final String TILE_URL_TEMPLATE = "https://tile.openstreetmap.org/%d/%d/%d.png";
	private static final String USER_AGENT = "Jailer-DataBrowser-SpatialPreview/1.0 (+https://github.com/Wisser/Jailer)";

	/** Required by OpenStreetMap's tile usage policy. */
	public static final String ATTRIBUTION = "© OpenStreetMap contributors";
	/** Height in pixels of the attribution strip drawn at the bottom - so other overlays (e.g. a scale bar) can sit right above it. */
	public static final int ATTRIBUTION_HEIGHT = 15;

	private static final int CACHE_CAPACITY = 300;
	private static final Map<String, BufferedImage> TILE_CACHE = new LinkedHashMap<String, BufferedImage>(16, 0.75f, true) {
		private static final long serialVersionUID = 1L;
		@Override
		protected boolean removeEldestEntry(Map.Entry<String, BufferedImage> eldest) {
			return size() > CACHE_CAPACITY;
		}
	};
	private static final Set<String> inFlight = new HashSet<String>();
	private static final Set<String> failed = new HashSet<String>();

	/**
	 * Maps (lon, lat) to screen pixels for a chosen zoom level and auto-fit
	 * scale/offset - shared between the tile background and the geometry
	 * overlay so both align exactly.
	 */
	public static class MercatorView implements CoordinateTransform {
		final int zoom;
		final double scale;
		final double originPxX;
		final double originPxY;

		MercatorView(int zoom, double scale, double originPxX, double originPxY) {
			this.zoom = zoom;
			this.scale = scale;
			this.originPxX = originPxX;
			this.originPxY = originPxY;
		}

		@Override
		public double[] toScreen(double lon, double lat) {
			double worldSize = WebMercator.worldSize(zoom);
			double wx = WebMercator.lonToXFraction(lon) * worldSize;
			double wy = WebMercator.latToYFraction(lat) * worldSize;
			return new double[] { (wx - originPxX) * scale, (wy - originPxY) * scale };
		}

		public int getZoom() {
			return zoom;
		}

		public double getScale() {
			return scale;
		}
	}

	/**
	 * Computes the best-fit zoom level and auto-fit scale/origin so that the
	 * given lon/lat bounding box is centered within the available pixel area -
	 * the Mercator equivalent of {@code GeometryPreviewPanel}'s own linear
	 * auto-fit, so tiles and vector overlay line up. This is the default,
	 * un-zoomed view (before any manual mouse-wheel interaction).
	 *
	 * Always renders at native tile resolution (<code>scale == 1.0</code>) - the bbox may
	 * not fill the panel exactly (a blank margin can show around it), trading that off
	 * against never upscaling/softening the tiles.
	 *
	 * @param lonLatBounds {@code { minLon, minLat, maxLon, maxLat }}
	 */
	public static MercatorView computeView(double[] lonLatBounds, int availW, int availH, int padding) {
		double[] xyFrac = xyFraction(lonLatBounds);
		int zoom = pickZoom(xyFrac, availW, availH, padding);
		return centeredView(xyFrac, availW, availH, zoom, 1.0);
	}

	/**
	 * Computes the centered view for a fixed, caller-supplied zoom level at
	 * native tile resolution (<code>scale == 1.0</code> - no auto-fit stretch),
	 * still centered on the bounding box. Used for manual (mouse-wheel) zoom,
	 * where stretching would defeat the purpose of zooming in for more detail.
	 *
	 * @param lonLatBounds {@code { minLon, minLat, maxLon, maxLat }}
	 * @param zoom the fixed zoom level, typically clamped to {@code [0, MAX_ZOOM]} by the caller
	 */
	public static MercatorView computeViewAtZoom(double[] lonLatBounds, int availW, int availH, int zoom) {
		return centeredView(xyFraction(lonLatBounds), availW, availH, zoom, 1.0);
	}

	/**
	 * The best-fit integer zoom level for the given bbox/available area (see {@link #computeView}),
	 * without computing a full {@link MercatorView} - used to seed manual pan/zoom state.
	 *
	 * @param lonLatBounds {@code { minLon, minLat, maxLon, maxLat }}
	 */
	public static int pickBestFitZoom(double[] lonLatBounds, int availW, int availH, int padding) {
		return pickZoom(xyFraction(lonLatBounds), availW, availH, padding);
	}

	/**
	 * Computes the view for an explicit, caller-supplied center (as normalized Web Mercator
	 * world-fraction coordinates, see {@link WebMercator}) and zoom - used for manual panning,
	 * where the center is no longer necessarily the geometry's own bbox center.
	 */
	public static MercatorView computeViewAtZoomAndCenter(double centerXFraction, double centerYFraction, int availW, int availH, int zoom, double scale) {
		double worldSize = WebMercator.worldSize(zoom);
		double centerWx = centerXFraction * worldSize;
		double centerWy = centerYFraction * worldSize;
		double originPxX = centerWx - (availW / 2.0) / scale;
		double originPxY = centerWy - (availH / 2.0) / scale;
		return new MercatorView(zoom, scale, originPxX, originPxY);
	}

	/**
	 * @return {@code { minX, minY, maxX, maxY, spanX, spanY }} in normalized world-fraction
	 *         coordinates (see {@link WebMercator}), spans floored at {@link #MIN_SPAN_FRACTION}
	 */
	private static double[] xyFraction(double[] lonLatBounds) {
		double minX = WebMercator.lonToXFraction(lonLatBounds[0]);
		double maxX = WebMercator.lonToXFraction(lonLatBounds[2]);
		// A larger latitude maps to a *smaller* y-fraction, so min/max swap here.
		double yAtMinLat = WebMercator.latToYFraction(lonLatBounds[1]);
		double yAtMaxLat = WebMercator.latToYFraction(lonLatBounds[3]);
		double minY = Math.min(yAtMinLat, yAtMaxLat);
		double maxY = Math.max(yAtMinLat, yAtMaxLat);
		double spanX = Math.max(maxX - minX, MIN_SPAN_FRACTION);
		double spanY = Math.max(maxY - minY, MIN_SPAN_FRACTION);
		return new double[] { minX, minY, maxX, maxY, spanX, spanY };
	}

	private static int pickZoom(double[] xyFrac, int availW, int availH, int padding) {
		double availPxW = Math.max(availW - 2 * padding, 1);
		double availPxH = Math.max(availH - 2 * padding, 1);
		double spanX = xyFrac[4];
		double spanY = xyFrac[5];
		int zoom = MAX_ZOOM;
		for (int z = 0; z <= MAX_ZOOM; z++) {
			double worldSize = WebMercator.worldSize(z);
			if (spanX * worldSize > availPxW || spanY * worldSize > availPxH) {
				zoom = Math.max(0, z - 1);
				break;
			}
		}
		return zoom;
	}

	private static MercatorView centeredView(double[] xyFrac, int availW, int availH, int zoom, double scale) {
		double centerX = (xyFrac[0] + xyFrac[2]) / 2;
		double centerY = (xyFrac[1] + xyFrac[3]) / 2;
		return computeViewAtZoomAndCenter(centerX, centerY, availW, availH, zoom, scale);
	}

	/**
	 * Paints the tiles covering the visible area for the given view,
	 * fetching missing ones asynchronously (leaving them blank in the
	 * meantime) and drawing the mandatory attribution text.
	 *
	 * @param onTileLoaded invoked (on the EDT) once a fetched tile becomes available, so the caller can repaint
	 */
	public void paint(Graphics2D g2, MercatorView view, int width, int height, Runnable onTileLoaded) {
		int[] range = tileRange(view, width, height);
		for (int tx = range[0]; tx <= range[1]; tx++) {
			for (int ty = range[2]; ty <= range[3]; ty++) {
				String key = view.zoom + "/" + tx + "/" + ty;
				BufferedImage img = TILE_CACHE.get(key);
				double screenX = (tx * (double) TILE_SIZE - view.originPxX) * view.scale;
				double screenY = (ty * (double) TILE_SIZE - view.originPxY) * view.scale;
				double screenSize = TILE_SIZE * view.scale;
				if (img != null) {
					g2.drawImage(img, (int) Math.round(screenX), (int) Math.round(screenY),
							(int) Math.round(screenSize), (int) Math.round(screenSize), null);
				} else {
					fetchAsync(view.zoom, tx, ty, onTileLoaded);
				}
			}
		}

		g2.setColor(new Color(255, 255, 255, 190));
		g2.fillRect(0, height - ATTRIBUTION_HEIGHT, 160, ATTRIBUTION_HEIGHT);
		g2.setColor(Color.DARK_GRAY);
		g2.drawString(ATTRIBUTION, 3, height - 4);
	}

	/** @return {@code { tileMinX, tileMaxX, tileMinY, tileMaxY }} for the tiles visible in this view */
	private static int[] tileRange(MercatorView view, int width, int height) {
		double worldSize = WebMercator.worldSize(view.zoom);
		int maxTileIndex = (int) (worldSize / TILE_SIZE) - 1;

		double topLeftWx = view.originPxX;
		double topLeftWy = view.originPxY;
		double bottomRightWx = topLeftWx + width / view.scale;
		double bottomRightWy = topLeftWy + height / view.scale;

		int tileMinX = clamp((int) Math.floor(topLeftWx / TILE_SIZE), 0, maxTileIndex);
		int tileMaxX = clamp((int) Math.floor(bottomRightWx / TILE_SIZE), 0, maxTileIndex);
		int tileMinY = clamp((int) Math.floor(topLeftWy / TILE_SIZE), 0, maxTileIndex);
		int tileMaxY = clamp((int) Math.floor(bottomRightWy / TILE_SIZE), 0, maxTileIndex);
		return new int[] { tileMinX, tileMaxX, tileMinY, tileMaxY };
	}

	private static int clamp(int v, int min, int max) {
		return Math.max(min, Math.min(max, v));
	}

	private void fetchAsync(int z, int x, int y, Runnable onTileLoaded) {
		String key = z + "/" + x + "/" + y;
		if (inFlight.contains(key) || failed.contains(key)) {
			return;
		}
		inFlight.add(key);
		new SwingWorker<BufferedImage, Void>() {
			@Override
			protected BufferedImage doInBackground() {
				try {
					URL url = new URL(String.format(TILE_URL_TEMPLATE, z, x, y));
					HttpURLConnection conn = (HttpURLConnection) url.openConnection();
					conn.setRequestProperty("User-Agent", USER_AGENT);
					conn.setConnectTimeout(5000);
					conn.setReadTimeout(5000);
					conn.setInstanceFollowRedirects(true);
					if (conn.getResponseCode() != HttpURLConnection.HTTP_OK) {
						return null;
					}
					try (InputStream in = conn.getInputStream()) {
						return ImageIO.read(in);
					}
				} catch (IOException e) {
					LogUtil.warn(e);
					return null;
				}
			}
			@Override
			protected void done() {
				inFlight.remove(key);
				BufferedImage img = null;
				try {
					img = get();
				} catch (Exception e) {
					// treated as a failed fetch below
					LogUtil.warn(e);
				}
				if (img != null) {
					TILE_CACHE.put(key, img);
					if (onTileLoaded != null) {
						onTileLoaded.run();
					}
				} else {
					failed.add(key);
				}
			}
		}.execute();
	}

}
