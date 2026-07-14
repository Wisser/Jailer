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

/**
 * Standard "slippy map" Web Mercator (EPSG:3857) projection math, as used by
 * OpenStreetMap/Google/... XYZ tile schemes: longitude maps linearly, latitude
 * maps through the Mercator formula and is clamped to the scheme's usual
 * +/-85.0511 degree limit (where the projected square becomes exactly square).
 *
 * @author Ralf Wisser
 */
public final class WebMercator {

	/** The Web Mercator projection is only defined up to this absolute latitude. */
	public static final double MAX_LATITUDE = 85.0511287798;

	private WebMercator() {
	}

	/**
	 * @param lon longitude in degrees
	 * @return the normalized (0..1) world x-fraction
	 */
	public static double lonToXFraction(double lon) {
		return (lon + 180.0) / 360.0;
	}

	/**
	 * @param lat latitude in degrees (clamped to +/-{@link #MAX_LATITUDE})
	 * @return the normalized (0..1) world y-fraction (0 = north, 1 = south)
	 */
	public static double latToYFraction(double lat) {
		double clamped = Math.max(-MAX_LATITUDE, Math.min(MAX_LATITUDE, lat));
		double rad = Math.toRadians(clamped);
		return (1 - Math.log(Math.tan(rad) + 1 / Math.cos(rad)) / Math.PI) / 2;
	}

	/**
	 * @param zoom the tile zoom level
	 * @return the size, in pixels, of the whole projected world at this zoom level (256 * 2^zoom)
	 */
	public static double worldSize(int zoom) {
		return 256.0 * (1L << zoom);
	}

}
