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
	}

}
