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
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutionException;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.SwingWorker;

import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.util.LogUtil;

/**
 * Optional OpenStreetMap raster tile background for {@link GeometryPreviewPanel},
 * shown only for WGS84 geometries (SRID 4326, or Oracle's geodetic 8307) and
 * only when the user opts in (see {@link GeometryPreviewPanel#setShowBackgroundMap(boolean)}).
 *
 * <p>Tiles are fetched via plain {@link HttpURLConnection} (not
 * {@code java.net.http.HttpClient}, which needs Java 11+ - this project
 * targets Java 8) on a background thread ({@link SwingWorker}) and cached
 * in-memory; a tile that is still on its way is left blank, one whose fetch
 * failed gets a visible placeholder naming the reason instead (see
 * {@link #FAILED_TILES}) - either way the UI neither blocks nor crashes. All cache/in-flight/failure
 * bookkeeping here is only
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

	// A failed fetch has to trigger a repaint - otherwise its placeholder would never become
	// visible - and that repaint runs straight back into paint(), which would request the tile
	// again right away: repaint -> paint -> fetch -> fail -> repaint, spinning against the tile
	// server for as long as it stays unreachable. Hence the cool-down. A minute is the worst case
	// of a single attempt (connect + read timeout), so a tile can never have more than one request
	// per minute in flight, which keeps even a full screen of failing tiles well within
	// OpenStreetMap's tile usage policy - and it is short enough for the map to heal itself as soon
	// as connectivity returns, on the first repaint past the delay (hover, pan, zoom, resize).
	// Deliberately no timer: an idle window never polls in the background.
	private static final long FAILED_TILE_RETRY_DELAY = 60 * 1000L;
	private static final int FAILED_TILES_CAPACITY = 300;
	/**
	 * When a tile fetch failed, and why - the latter already broken into the parts the placeholder
	 * puts on separate lines (typically the exception's short class name and its message).
	 */
	private static class TileFailure {
		final long time;
		final List<String> reason;

		TileFailure(long time, List<String> reason) {
			this.time = time;
			this.reason = reason;
		}
	}

	/**
	 * Tiles whose last fetch failed, mapped to when that was and why.
	 * Tells "failed" apart from the two other reasons {@link #TILE_CACHE} can have no image for a
	 * tile - never requested, and still in flight - so only a genuinely failed tile gets a
	 * placeholder and a pending one still simply stays blank. Insertion-ordered (unlike
	 * {@link #TILE_CACHE}, which is access-ordered) and capped the same way so it cannot grow
	 * without bound; dropping the oldest entry only means that tile is retried earlier than the
	 * delay would have allowed, which is harmless.
	 */
	private static final Map<String, TileFailure> FAILED_TILES = new LinkedHashMap<String, TileFailure>(16, 0.75f, false) {
		private static final long serialVersionUID = 1L;
		@Override
		protected boolean removeEldestEntry(Map.Entry<String, TileFailure> eldest) {
			return size() > FAILED_TILES_CAPACITY;
		}
	};

	/** Placeholder colors - deliberately neutral, so an unreachable tile can't be mistaken for map content. */
	private static final Color PLACEHOLDER_BACKGROUND = new Color(245, 245, 245);
	private static final Color PLACEHOLDER_BORDER = new Color(210, 210, 210);
	/** The reason is subordinate to "no tile" itself, so it's drawn a shade lighter. */
	private static final Color PLACEHOLDER_REASON_FOREGROUND = Color.GRAY;
	private static final String PLACEHOLDER_TEXT = "no tile";
	private static final String PLACEHOLDER_ELLIPSIS = "...";
	private static final int PLACEHOLDER_ICON_TEXT_GAP = 4;
	private static final int PLACEHOLDER_TEXT_MARGIN = 4;
	private static final int PLACEHOLDER_MAX_REASON_LINES = 3;

	// The placeholder's warning icon, loaded lazily - there's no point paying for it unless a tile
	// actually fails - and only once either way, hence the separate "already tried" flag. Stays
	// null if the resource can't be read, in which case the placeholder falls back to text only.
	private static Image placeholderIcon;
	private static boolean placeholderIconLoaded;

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

		/**
		 * Inverse of {@link #toScreen} - maps a screen pixel back to normalized Web Mercator
		 * world-fraction coordinates {@code { xFraction, yFraction } } (see {@link WebMercator}).
		 * Used by the shift-drag zoom-box feature to translate the drawn screen rectangle back
		 * into map-space bounds.
		 */
		public double[] screenToFraction(double screenX, double screenY) {
			double worldSize = WebMercator.worldSize(zoom);
			double wx = screenX / scale + originPxX;
			double wy = screenY / scale + originPxY;
			return new double[] { wx / worldSize, wy / worldSize };
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
	 * Best-fit zoom level for an already-computed normalized world-fraction bounding box (see
	 * {@link WebMercator}), skipping the lon/lat conversion {@link #pickBestFitZoom} does - used
	 * by the shift-drag zoom-box feature, whose box is measured directly in fraction space via
	 * {@link MercatorView#screenToFraction}.
	 */
	public static int pickBestFitZoomForFraction(double minXFraction, double minYFraction, double maxXFraction, double maxYFraction, int availW, int availH, int padding) {
		double spanX = Math.max(maxXFraction - minXFraction, MIN_SPAN_FRACTION);
		double spanY = Math.max(maxYFraction - minYFraction, MIN_SPAN_FRACTION);
		return pickZoom(new double[] { minXFraction, minYFraction, maxXFraction, maxYFraction, spanX, spanY }, availW, availH, padding);
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
	 * fetching missing ones asynchronously (leaving them blank while the
	 * fetch is in flight, and drawing a placeholder for those whose fetch
	 * failed) and drawing the mandatory attribution text.
	 *
	 * @param onTileLoaded invoked (on the EDT) once a tile fetch has finished, successfully or not,
	 *                     so the caller can repaint - a failed tile has to be repainted too,
	 *                     otherwise its placeholder would never show up
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
					TileFailure failure = FAILED_TILES.get(key);
					if (failure != null) {
						// Drawn even while a retry is already in flight - dropping back to a blank
						// tile and only then re-filling it would just flicker.
						drawUnavailableTile(g2, (int) Math.round(screenX), (int) Math.round(screenY),
								(int) Math.round(screenSize), failure.reason);
					}
					if (failure == null || System.currentTimeMillis() - failure.time >= FAILED_TILE_RETRY_DELAY) {
						// fetchAsync's own in-flight guard collapses the repaints that happen while
						// a request is running into that single request.
						fetchAsync(view.zoom, tx, ty, onTileLoaded);
					}
				}
			}
		}

		g2.setColor(new Color(255, 255, 255, 190));
//		g2.fillRect(0, height - ATTRIBUTION_HEIGHT, 160, ATTRIBUTION_HEIGHT);
		g2.setColor(Color.DARK_GRAY);
//		g2.drawString(ATTRIBUTION, 3, height - 4);
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

	/** @return the placeholder's warning icon, or <code>null</code> if the resource can't be read */
	private static Image placeholderIcon() {
		if (!placeholderIconLoaded) {
			placeholderIconLoaded = true;
			try {
				// No error dialog - a missing icon must never pop one up out of a paint.
				ImageIcon icon = UIUtil.readImage("/wanr.png", false);
				placeholderIcon = icon != null? icon.getImage() : null;
			} catch (Throwable t) {
				// ignore - the placeholder falls back to text only
			}
		}
		return placeholderIcon;
	}

	/**
	 * Draws a placeholder for a tile that couldn't be fetched - a neutral, framed square with a
	 * warning icon, a short text, and below it the reason the fetch failed (typically the
	 * exception's short class name over its message) - so that an unreachable tile can be told
	 * apart from a genuinely empty stretch of map, which is exactly what a blank tile looks like,
	 * and so that an unresolvable host can be told apart from a server that turned us away.
	 * The icon carries the message on its own, so the lines below it are what gets dropped first
	 * (the reason before the text) when a tile is too small for all of them. Uses the ambient font
	 * and stroke and leaves both alone; only the color is left changed, which is harmless because
	 * every later drawing step ({@link #paint}'s attribution, the geometry overlay,
	 * {@link ScaleBar}) sets its own color first.
	 *
	 * @param reason the compact error message in its parts, one per line before wrapping, or
	 *               <code>null</code> if it isn't known
	 */
	private static void drawUnavailableTile(Graphics2D g2, int x, int y, int size, List<String> reason) {
		if (size <= 0) {
			return;
		}
		g2.setColor(PLACEHOLDER_BACKGROUND);
		g2.fillRect(x, y, size, size);
		g2.setColor(PLACEHOLDER_BORDER);
		// drawRect's width/height are the last pixel's offset, not a count, hence "size - 1".
		g2.drawRect(x, y, size - 1, size - 1);

		Image icon = placeholderIcon();
		int iconWidth = icon != null? icon.getWidth(null) : 0;
		int iconHeight = icon != null? icon.getHeight(null) : 0;
		boolean withIcon = iconWidth > 0 && iconHeight > 0 && iconWidth <= size && iconHeight <= size;

		FontMetrics fm = g2.getFontMetrics();
		int lineHeight = fm.getAscent() + fm.getDescent();
		int maxTextWidth = size - 2 * PLACEHOLDER_TEXT_MARGIN;

		List<String> reasonLines = new ArrayList<String>(PLACEHOLDER_MAX_REASON_LINES);
		if (reason != null) {
			for (String part : reason) {
				// Same list and cap for every part, so the wrapping accumulates across them and
				// stops once three lines are full - whichever part filled them.
				wrapToWidth(fm, part, maxTextWidth, PLACEHOLDER_MAX_REASON_LINES, reasonLines);
			}
		}
		List<String> lines = new ArrayList<String>(1 + reasonLines.size());
		if (fm.stringWidth(PLACEHOLDER_TEXT) <= maxTextWidth) {
			lines.add(PLACEHOLDER_TEXT);
		}
		int headlineCount = lines.size();
		lines.addAll(reasonLines);
		// Drop lines from the bottom up until what's left fits next to the icon.
		while (!lines.isEmpty()
				&& (withIcon? iconHeight + PLACEHOLDER_ICON_TEXT_GAP : 0) + lines.size() * lineHeight > size) {
			lines.remove(lines.size() - 1);
		}

		int gap = withIcon && !lines.isEmpty()? PLACEHOLDER_ICON_TEXT_GAP : 0;
		int top = y + (size - ((withIcon? iconHeight : 0) + gap + lines.size() * lineHeight)) / 2;
		if (withIcon) {
			g2.drawImage(icon, x + (size - iconWidth) / 2, top, null);
			top += iconHeight + gap;
		}
		for (int i = 0; i < lines.size(); i++) {
			String line = lines.get(i);
			g2.setColor(i < headlineCount? Color.DARK_GRAY : PLACEHOLDER_REASON_FOREGROUND);
			g2.drawString(line, x + (size - fm.stringWidth(line)) / 2, top + fm.getAscent());
			top += lineHeight;
		}
	}

	/**
	 * @return {@code text}, cut off with an ellipsis if it is wider than {@code maxWidth}, or
	 *         <code>null</code> if not even the ellipsis alone fits
	 */
	private static String shortenToWidth(FontMetrics fm, String text, int maxWidth) {
		if (fm.stringWidth(text) <= maxWidth) {
			return text;
		}
		if (fm.stringWidth(PLACEHOLDER_ELLIPSIS) > maxWidth) {
			return null;
		}
		StringBuilder shortened = new StringBuilder(text);
		while (shortened.length() > 0 && fm.stringWidth(shortened + PLACEHOLDER_ELLIPSIS) > maxWidth) {
			shortened.setLength(shortened.length() - 1);
		}
		return shortened.length() == 0? null : shortened + PLACEHOLDER_ELLIPSIS;
	}

	/**
	 * Breaks {@code text} into at most {@code maxLines} lines no wider than {@code maxWidth} and
	 * appends them to {@code lines}, which may already hold some - the cap counts all of them.
	 * Breaks at a space where that doesn't leave a stub of a line, and mid-word otherwise: a host
	 * name, the very thing worth showing here, has no space to break at at all. Whatever still
	 * doesn't fit is cut off with an ellipsis on the last line.
	 */
	private static void wrapToWidth(FontMetrics fm, String text, int maxWidth, int maxLines, List<String> lines) {
		String rest = text.trim();
		while (!rest.isEmpty() && lines.size() < maxLines) {
			if (fm.stringWidth(rest) <= maxWidth) {
				lines.add(rest);
				return;
			}
			if (lines.size() + 1 == maxLines) {
				String shortened = shortenToWidth(fm, rest, maxWidth);
				if (shortened != null) {
					lines.add(shortened);
				}
				return;
			}
			int end = rest.length();
			while (end > 1 && fm.stringWidth(rest.substring(0, end)) > maxWidth) {
				end--;
			}
			// Only break at a space if it isn't so far left that it leaves a stub behind.
			int space = rest.lastIndexOf(' ', end);
			int cut = space > end / 2? space : end;
			lines.add(rest.substring(0, cut).trim());
			rest = rest.substring(cut).trim();
		}
	}

	/**
	 * Turns a failed fetch into something that fits on a tile: the exception's short class name,
	 * and its message below it - for an {@code UnknownHostException} that message is exactly the
	 * host that couldn't be resolved, which is what tells a typo in the URL from a blocked host or
	 * a dead network. The package prefix is dropped; "java.net." on a 256 pixel tile buys nothing.
	 */
	private static List<String> failureReason(Throwable t) {
		// get() hands back whatever doInBackground threw wrapped in an ExecutionException - that
		// wrapper is noise, the cause is what's worth showing.
		Throwable cause = t instanceof ExecutionException && t.getCause() != null? t.getCause() : t;
		List<String> reason = new ArrayList<String>(2);
		reason.add(cause.getClass().getSimpleName());
		String message = cause.getMessage();
		if (message != null && !message.trim().isEmpty()) {
			reason.add(message.trim());
		}
		return reason;
	}

	/**
	 * Appends the tile's URL to a failure reason as its own line, so the placeholder shows exactly
	 * which request failed - useful once several tile servers/subdomains are in play, or simply to
	 * see the coordinates a tile request was actually made for.
	 *
	 * @param reason the reason so far, not modified
	 * @param url the tile's URL, or <code>null</code> if it wasn't built yet (e.g. a malformed URL)
	 * @return a new list with {@code url} appended, or {@code reason} unchanged if {@code url} is <code>null</code>
	 */
	private static List<String> withUrl(List<String> reason, URL url) {
		if (url == null) {
			return reason;
		}
		List<String> withUrl = new ArrayList<String>(reason);
		withUrl.add(url.toString());
		return withUrl;
	}

	private void fetchAsync(int z, int x, int y, Runnable onTileLoaded) {
		String key = z + "/" + x + "/" + y;
		if (inFlight.contains(key)) {
			return;
		}
		inFlight.add(key);
		new SwingWorker<BufferedImage, Void>() {
			/**
			 * Why the fetch failed, in the wording the placeholder shows - written in
			 * {@link #doInBackground()} and read in {@link #done()}, which is safe because the
			 * latter runs only once the former has completed.
			 */
			private List<String> reason;

			@Override
			protected BufferedImage doInBackground() {
				URL url = null;
				try {
					url = new URL(String.format(TILE_URL_TEMPLATE, z, x, y));
					HttpURLConnection conn = (HttpURLConnection) url.openConnection();
					conn.setRequestProperty("User-Agent", USER_AGENT);
					conn.setConnectTimeout(30000);
					conn.setReadTimeout(30000);
					conn.setInstanceFollowRedirects(true);
					int responseCode = conn.getResponseCode();
					if (responseCode != HttpURLConnection.HTTP_OK) {
						// Not logged - a whole screen of tiles failing would flood the log with
						// stack traces. The placeholder is the signal now, and it names the code
						// (403/429 mean OpenStreetMap itself is turning us away).
						String responseMessage = conn.getResponseMessage();
						reason = withUrl(Collections.singletonList("HTTP " + responseCode
								+ (responseMessage != null && !responseMessage.trim().isEmpty()? " " + responseMessage.trim() : "")), url);
						return null;
					}
					try (InputStream in = conn.getInputStream()) {
						BufferedImage image = ImageIO.read(in);
						if (image == null) {
							reason = withUrl(Collections.singletonList("invalid image"), url);
						}
						return image;
					}
				} catch (IOException e) {
					LogUtil.warn(e);
					reason = withUrl(failureReason(e), url);
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
					// img stays null - treated as a failed fetch (see below)
					LogUtil.warn(e);
					reason = failureReason(e);
				}
				if (img != null) {
					TILE_CACHE.put(key, img);
					// A tile that has come back must stop looking failed - and must keep doing so
					// after the LRU cache evicts its image again, which would otherwise leave the
					// stale failure entry as the only thing paint() finds for it.
					FAILED_TILES.remove(key);
				} else {
					// Recorded *before* notifying, so that the repaint that follows finds the tile
					// marked as failed and draws the placeholder instead of immediately requesting
					// it again. The timestamp lets paint() try once more after
					// FAILED_TILE_RETRY_DELAY, so nothing is blacklisted permanently.
					FAILED_TILES.put(key, new TileFailure(System.currentTimeMillis(), reason));
				}
				// Also on failure: the placeholder only becomes visible if something repaints.
				if (onTileLoaded != null) {
					onTileLoaded.run();
				}
			}
		}.execute();
	}
}

