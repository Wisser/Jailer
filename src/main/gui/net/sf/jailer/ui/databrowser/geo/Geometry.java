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
 * Minimal in-memory geometry model used to render a preview of spatial
 * (geometry/geography) column values in the DataBrowser. Not a general
 * purpose geometry library - just enough structure (coordinates + bounds)
 * to draw a 2D preview with {@link net.sf.jailer.ui.databrowser.geo.GeometryPreviewPanel}.
 *
 * @author Ralf Wisser
 */
public abstract class Geometry {

	/** WGS84 (the SRID used by MySQL/PostGIS/SQL Server geography for lon/lat data). */
	public static final int SRID_WGS84 = 4326;
	/** Oracle's own geodetic WGS84 SRID (lon/lat, same datum as {@link #SRID_WGS84}). */
	public static final int SRID_ORACLE_WGS84 = 8307;
	/** EPSG:3857 "Web Mercator" - the projection web maps/online portals (and the OSM tiles) use;
	 *  coordinates are in meters, not degrees, so they are reprojected via {@link #toWgs84()}. */
	public static final int SRID_WEB_MERCATOR = 3857;

	/** Earth radius (meters) of the EPSG:3857 spherical Mercator model. */
	private static final double WEB_MERCATOR_RADIUS = 6378137.0;

	private final int srid;

	protected Geometry(int srid) {
		this.srid = srid;
	}

	/**
	 * Returns the SRID (Spatial Reference System Identifier) of this geometry,
	 * or <code>0</code> if unknown/not georeferenced.
	 *
	 * @return the SRID
	 */
	public int getSrid() {
		return srid;
	}

	/**
	 * @param srid a SRID
	 * @return whether the SRID identifies (lon, lat) coordinates in the WGS84 datum
	 */
	public static boolean isWgs84(int srid) {
		return srid == SRID_WGS84 || srid == SRID_ORACLE_WGS84;
	}

	/**
	 * Returns an equivalent geometry with WGS84 (lon/lat) coordinates. EPSG:3857 (Web Mercator, used
	 * by web maps / online portals) geometries are reprojected from meters to lon/lat so they can be
	 * shown on the OSM map like native WGS84 data; every other geometry (including already-WGS84) is
	 * returned unchanged.
	 */
	public final Geometry toWgs84() {
		if (srid != SRID_WEB_MERCATOR) {
			return this;
		}
		return convertWebMercatorToWgs84();
	}

	/** Reprojects this geometry's EPSG:3857 (Web Mercator) meter coordinates to a new WGS84 geometry. */
	protected abstract Geometry convertWebMercatorToWgs84();

	/** Converts a single EPSG:3857 (x, y) in meters to WGS84 { lon, lat } in degrees. */
	protected static double[] webMercatorToWgs84(double x, double y) {
		double lon = Math.toDegrees(x / WEB_MERCATOR_RADIUS);
		double lat = Math.toDegrees(2 * Math.atan(Math.exp(y / WEB_MERCATOR_RADIUS)) - Math.PI / 2);
		return new double[] { lon, lat };
	}

	/**
	 * Returns the axis-aligned bounding box of this geometry as
	 * <code>{ minX, minY, maxX, maxY }</code>, or <code>null</code> if this
	 * geometry contains no coordinates at all.
	 *
	 * @return the bounding box, or <code>null</code>
	 */
	public abstract double[] getBounds();

	protected static double[] mergeBounds(double[] a, double[] b) {
		if (a == null) {
			return b;
		}
		if (b == null) {
			return a;
		}
		return new double[] { Math.min(a[0], b[0]), Math.min(a[1], b[1]), Math.max(a[2], b[2]), Math.max(a[3], b[3]) };
	}

	protected static double[] boundsOfPoints(double[][] points) {
		double[] bounds = null;
		for (double[] p : points) {
			double[] pb = new double[] { p[0], p[1], p[0], p[1] };
			bounds = mergeBounds(bounds, pb);
		}
		return bounds;
	}

	/**
	 * A single (x, y) coordinate. Any Z/M ordinates present in the source
	 * data are discarded - not needed for a 2D preview.
	 */
	public static class Point extends Geometry {
		public final double x;
		public final double y;

		public Point(int srid, double x, double y) {
			super(srid);
			this.x = x;
			this.y = y;
		}

		@Override
		public double[] getBounds() {
			return new double[] { x, y, x, y };
		}

		@Override
		protected Geometry convertWebMercatorToWgs84() {
			double[] ll = webMercatorToWgs84(x, y);
			return new Point(SRID_WGS84, ll[0], ll[1]);
		}
	}

	/**
	 * An ordered sequence of coordinates.
	 */
	public static class LineString extends Geometry {
		/** points[i] = { x, y } */
		public final double[][] points;

		public LineString(int srid, double[][] points) {
			super(srid);
			this.points = points;
		}

		@Override
		public double[] getBounds() {
			if (points.length == 0) {
				return null;
			}
			return boundsOfPoints(points);
		}

		@Override
		protected Geometry convertWebMercatorToWgs84() {
			double[][] converted = new double[points.length][];
			for (int i = 0; i < points.length; i++) {
				converted[i] = webMercatorToWgs84(points[i][0], points[i][1]);
			}
			return new LineString(SRID_WGS84, converted);
		}
	}

	/**
	 * A polygon with an exterior ring and zero or more interior (hole) rings.
	 * <code>rings[0]</code> is the exterior ring, <code>rings[1..]</code> are holes.
	 */
	public static class Polygon extends Geometry {
		/** rings[ring][point] = { x, y } */
		public final double[][][] rings;

		public Polygon(int srid, double[][][] rings) {
			super(srid);
			this.rings = rings;
		}

		@Override
		public double[] getBounds() {
			double[] bounds = null;
			for (double[][] ring : rings) {
				if (ring.length > 0) {
					bounds = mergeBounds(bounds, boundsOfPoints(ring));
				}
			}
			return bounds;
		}

		@Override
		protected Geometry convertWebMercatorToWgs84() {
			double[][][] converted = new double[rings.length][][];
			for (int r = 0; r < rings.length; r++) {
				double[][] ring = rings[r];
				double[][] convertedRing = new double[ring.length][];
				for (int i = 0; i < ring.length; i++) {
					convertedRing[i] = webMercatorToWgs84(ring[i][0], ring[i][1]);
				}
				converted[r] = convertedRing;
			}
			return new Polygon(SRID_WGS84, converted);
		}
	}

	/**
	 * A collection of geometries of a common or mixed kind (multi-point,
	 * multi-linestring, multi-polygon, or a generic geometry collection).
	 */
	public static class GeometryCollection extends Geometry {
		public final Geometry[] geometries;

		public GeometryCollection(int srid, Geometry[] geometries) {
			super(srid);
			this.geometries = geometries;
		}

		@Override
		public double[] getBounds() {
			double[] bounds = null;
			for (Geometry g : geometries) {
				bounds = mergeBounds(bounds, g.getBounds());
			}
			return bounds;
		}

		@Override
		protected Geometry convertWebMercatorToWgs84() {
			Geometry[] converted = new Geometry[geometries.length];
			for (int i = 0; i < geometries.length; i++) {
				converted[i] = geometries[i].convertWebMercatorToWgs84();
			}
			return new GeometryCollection(SRID_WGS84, converted);
		}
	}

}
