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

/**
 * Draws a small metric + imperial ground-distance scale bar in the
 * bottom-right corner of {@link GeometryPreviewPanel}'s map background,
 * matching the current {@link OsmTileLayer.MercatorView} so it stays
 * accurate as the user zooms in/out. Web Mercator distorts ground distance
 * by latitude, so the view's center latitude is used to compute an accurate
 * meters-per-pixel value there.
 *
 * @author Ralf Wisser
 */
public class ScaleBar {

	private static final double EARTH_CIRCUMFERENCE_M = 40075016.686;
	private static final double METERS_PER_MILE = 1609.344;
	private static final int MAX_BAR_PIXELS = 70;
	private static final int MARGIN = 6;
	private static final int TICK_HEIGHT = 5;
	private static final int ROW_HEIGHT = 13;
	private static final int LABEL_BAR_GAP = 4;

	private ScaleBar() {
	}

	/**
	 * @param centerLat the view's center latitude in degrees (Web Mercator's distortion depends on it)
	 * @param bottomMargin pixels to leave free at the very bottom (e.g. for {@link OsmTileLayer}'s attribution strip)
	 */
	public static void paint(Graphics2D g2, OsmTileLayer.MercatorView view, int panelWidth, int panelHeight, double centerLat, int bottomMargin) {
		double metersPerScreenPixel = (EARTH_CIRCUMFERENCE_M * Math.cos(Math.toRadians(centerLat)))
				/ (WebMercator.worldSize(view.getZoom()) * view.getScale());
		if (!Double.isFinite(metersPerScreenPixel) || metersPerScreenPixel <= 0) {
			return;
		}

		int maxPixels = Math.min(MAX_BAR_PIXELS, Math.max(20, panelWidth / 3));

		double[] km = niceValueAndPixels(metersPerScreenPixel, maxPixels, 1000.0);
		double[] mi = niceValueAndPixels(metersPerScreenPixel, maxPixels, METERS_PER_MILE);

		int yMi = panelHeight - bottomMargin - ROW_HEIGHT / 2;
		int yKm = yMi - ROW_HEIGHT;

		drawRow(g2, panelWidth, yKm, (int) Math.round(km[1]), formatMetric(km[0]));
		drawRow(g2, panelWidth, yMi, (int) Math.round(mi[1]), formatImperial(mi[0]));
	}

	/**
	 * @return {@code { niceValue, pixelWidth }} - the largest "nice" (1/2/5 * 10^n) round
	 *         number of {@code unitToMeters}-sized units whose pixel width fits within {@code maxPixels}
	 */
	private static double[] niceValueAndPixels(double metersPerScreenPixel, int maxPixels, double unitToMeters) {
		double maxUnitDistance = maxPixels * metersPerScreenPixel / unitToMeters;
		double niceValue = niceRound(maxUnitDistance);
		double pixels = niceValue * unitToMeters / metersPerScreenPixel;
		return new double[] { niceValue, pixels };
	}

	/**
	 * Rounds <em>down</em> to the nearest "nice" 1/2/5 * 10^n number that is
	 * still <= {@code value} - the bar must never exceed its pixel budget, so
	 * this can't round to the nearest nice number the way axis tick labels
	 * usually do, it must always round down.
	 */
	private static double niceRound(double value) {
		if (value <= 0 || !Double.isFinite(value)) {
			return 0;
		}
		double exponent = Math.floor(Math.log10(value));
		double magnitude = Math.pow(10, exponent);
		double fraction = value / magnitude;
		double niceFraction;
		if (fraction >= 5) {
			niceFraction = 5;
		} else if (fraction >= 2) {
			niceFraction = 2;
		} else {
			niceFraction = 1;
		}
		return niceFraction * magnitude;
	}

	private static String formatMetric(double km) {
		if (km <= 0) {
			return "";
		}
		if (km < 1) {
			return Math.round(km * 1000) + " m";
		}
		return trimTrailingZero(km) + " km";
	}

	private static String formatImperial(double mi) {
		if (mi <= 0) {
			return "";
		}
		return trimTrailingZero(mi) + " mi";
	}

	private static String trimTrailingZero(double v) {
		if (v == Math.floor(v)) {
			return String.valueOf((long) v);
		}
		return String.valueOf(v);
	}

	private static void drawRow(Graphics2D g2, int panelWidth, int y, int barPixels, String label) {
		if (barPixels <= 0 || label.isEmpty()) {
			return;
		}
		FontMetrics fm = g2.getFontMetrics();
		int labelWidth = fm.stringWidth(label);

		int barEndX = panelWidth - MARGIN;
		int barStartX = barEndX - barPixels;
		int labelX = barStartX - LABEL_BAR_GAP - labelWidth;

		g2.setColor(new Color(255, 255, 255, 190));
		g2.fillRect(labelX - 2, y - fm.getAscent(), labelWidth + barPixels + LABEL_BAR_GAP + 4, fm.getAscent() + fm.getDescent());

		g2.setColor(Color.DARK_GRAY);
		g2.drawString(label, labelX, y);
		g2.drawLine(barStartX, y - 2, barEndX, y - 2);
		g2.drawLine(barStartX, y - 2 - TICK_HEIGHT / 2, barStartX, y - 2 + TICK_HEIGHT / 2);
		g2.drawLine(barEndX, y - 2 - TICK_HEIGHT / 2, barEndX, y - 2 + TICK_HEIGHT / 2);
	}

}
