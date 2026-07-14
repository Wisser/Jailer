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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

/**
 * Decodes Oracle {@code MDSYS.SDO_GEOMETRY} values (as decomposed into
 * GTYPE/SRID/optional POINT/optional ELEM_INFO+ORDINATES by
 * {@code CellContentConverter.OracleSpatialValueWrapper}) into the
 * {@link Geometry} preview model, using the standard SDO element-info-triplet
 * decoding algorithm.
 *
 * <p>Curved elements (circular-arc line/ring segments - interpretation 2 -
 * and the rectangle/circle shorthand ring forms - interpretation 3/4) are
 * simply connected as straight lines between their defining points rather
 * than reconstructed geometrically - "good enough" for a preview, and avoids
 * a special-case code path for a rarely-used feature.
 *
 * @author Ralf Wisser
 */
public class SdoGeometryParser {

	private static final int ETYPE_POINT = 1;
	private static final int ETYPE_LINE = 2;
	private static final int ETYPE_COMPOUND_LINE = 4;
	private static final int ETYPE_EXTERIOR_RING = 1003;
	private static final int ETYPE_INTERIOR_RING = 2003;
	private static final int ETYPE_COMPOUND_EXTERIOR_RING = 1005;
	private static final int ETYPE_COMPOUND_INTERIOR_RING = 2005;

	private SdoGeometryParser() {
	}

	/**
	 * @param gtype the SDO_GTYPE value (e.g. "2001" = 2D point, "2003" = 2D polygon)
	 * @param srid the SDO_SRID value, or <code>null</code>
	 * @param point the SDO_POINT (x, y, z) optimized-storage values, or <code>null</code>
	 * @param elemInfo the flattened SDO_ELEM_INFO triplets, or <code>null</code>
	 * @param ordinates the flattened SDO_ORDINATES values, or <code>null</code>
	 * @return the decoded geometry
	 * @throws GeometryParseException if the data cannot be decoded
	 */
	public static Geometry parse(String gtype, String srid, String[] point, String[] elemInfo, String[] ordinates) throws GeometryParseException {
		if (gtype == null) {
			throw new GeometryParseException("Missing SDO_GTYPE");
		}
		int gtypeValue = parseInt(gtype);
		int dims = gtypeValue / 1000;
		if (dims < 2) {
			throw new GeometryParseException("Invalid SDO_GTYPE (dimensionality): " + gtype);
		}
		int sridValue = srid == null ? 0 : parseInt(srid);

		if (elemInfo == null || ordinates == null) {
			if (point == null) {
				throw new GeometryParseException("SDO_GEOMETRY has neither SDO_POINT nor SDO_ELEM_INFO/SDO_ORDINATES");
			}
			return new Geometry.Point(sridValue, parseDouble(point[0]), parseDouble(point[1]));
		}

		int[] triplets = parseInts(elemInfo);
		if (triplets.length % 3 != 0) {
			throw new GeometryParseException("SDO_ELEM_INFO length is not a multiple of 3: " + triplets.length);
		}
		double[] flatOrdinates = parseDoubles(ordinates);

		List<Geometry> shapes = new ArrayList<Geometry>();
		List<double[][]> pendingRings = new ArrayList<double[][]>();

		int elementCount = triplets.length / 3;
		int i = 0;
		while (i < elementCount) {
			int etype = triplets[i * 3 + 1];
			int interpretation = triplets[i * 3 + 2];

			switch (etype) {
				case ETYPE_POINT: {
					flushPolygon(shapes, pendingRings, sridValue);
					int numPoints = Math.max(interpretation, 1);
					double[][] pts = readPoints(triplets, i, numPoints, flatOrdinates, dims);
					for (double[] p : pts) {
						shapes.add(new Geometry.Point(sridValue, p[0], p[1]));
					}
					i++;
					break;
				}
				case ETYPE_LINE: {
					flushPolygon(shapes, pendingRings, sridValue);
					double[][] pts = readElement(triplets, i, elementCount, flatOrdinates, dims);
					shapes.add(new Geometry.LineString(sridValue, pts));
					i++;
					break;
				}
				case ETYPE_COMPOUND_LINE: {
					flushPolygon(shapes, pendingRings, sridValue);
					double[][] pts = readCompound(triplets, i, interpretation, elementCount, flatOrdinates, dims);
					shapes.add(new Geometry.LineString(sridValue, pts));
					i += 1 + interpretation;
					break;
				}
				case ETYPE_EXTERIOR_RING: {
					flushPolygon(shapes, pendingRings, sridValue);
					double[][] pts = readElement(triplets, i, elementCount, flatOrdinates, dims);
					pendingRings.add(pts);
					i++;
					break;
				}
				case ETYPE_INTERIOR_RING: {
					double[][] pts = readElement(triplets, i, elementCount, flatOrdinates, dims);
					pendingRings.add(pts);
					i++;
					break;
				}
				case ETYPE_COMPOUND_EXTERIOR_RING: {
					flushPolygon(shapes, pendingRings, sridValue);
					double[][] pts = readCompound(triplets, i, interpretation, elementCount, flatOrdinates, dims);
					pendingRings.add(pts);
					i += 1 + interpretation;
					break;
				}
				case ETYPE_COMPOUND_INTERIOR_RING: {
					double[][] pts = readCompound(triplets, i, interpretation, elementCount, flatOrdinates, dims);
					pendingRings.add(pts);
					i += 1 + interpretation;
					break;
				}
				default:
					// etype 0 (padding) or unrecognized - skip this single element
					i++;
					break;
			}
		}
		flushPolygon(shapes, pendingRings, sridValue);

		if (shapes.isEmpty()) {
			throw new GeometryParseException("SDO_GEOMETRY decoded to no elements");
		}
		if (shapes.size() == 1) {
			return shapes.get(0);
		}
		return new Geometry.GeometryCollection(sridValue, shapes.toArray(new Geometry[0]));
	}

	private static void flushPolygon(List<Geometry> shapes, List<double[][]> pendingRings, int srid) {
		if (!pendingRings.isEmpty()) {
			shapes.add(new Geometry.Polygon(srid, pendingRings.toArray(new double[0][][])));
			pendingRings.clear();
		}
	}

	/** Reads the ordinates belonging to a single (non-compound) element, spanning up to the next element's start offset. */
	private static double[][] readElement(int[] triplets, int index, int elementCount, double[] flatOrdinates, int dims) throws GeometryParseException {
		int startOffset = triplets[index * 3];
		int nextStartOffset = index + 1 < elementCount ? triplets[(index + 1) * 3] : flatOrdinates.length + 1;
		return readPointRange(startOffset, nextStartOffset, flatOrdinates, dims);
	}

	private static double[][] readPoints(int[] triplets, int index, int numPoints, double[] flatOrdinates, int dims) throws GeometryParseException {
		int startOffset = triplets[index * 3];
		return readPointRange(startOffset, startOffset + numPoints * dims, flatOrdinates, dims);
	}

	private static double[][] readPointRange(int startOffset, int endOffsetExclusive, double[] flatOrdinates, int dims) throws GeometryParseException {
		int startIdx = startOffset - 1;
		int numOrdinates = endOffsetExclusive - startOffset;
		if (startIdx < 0 || numOrdinates < 0 || startIdx + numOrdinates > flatOrdinates.length) {
			throw new GeometryParseException("SDO_ELEM_INFO offset out of range for SDO_ORDINATES");
		}
		int numPoints = numOrdinates / dims;
		double[][] pts = new double[numPoints][2];
		for (int k = 0; k < numPoints; k++) {
			int base = startIdx + k * dims;
			pts[k][0] = flatOrdinates[base];
			pts[k][1] = flatOrdinates[base + 1];
		}
		return pts;
	}

	/** Reads and concatenates the {@code n} child elements of a compound line/ring, dropping duplicated shared endpoints. */
	private static double[][] readCompound(int[] triplets, int index, int n, int elementCount, double[] flatOrdinates, int dims) throws GeometryParseException {
		List<double[]> combined = new ArrayList<double[]>();
		for (int j = 1; j <= n; j++) {
			int childIndex = index + j;
			if (childIndex >= elementCount) {
				throw new GeometryParseException("Compound element declares more children than available");
			}
			double[][] childPts = readElement(triplets, childIndex, elementCount, flatOrdinates, dims);
			int start = 0;
			if (!combined.isEmpty() && childPts.length > 0 && isSamePoint(combined.get(combined.size() - 1), childPts[0])) {
				start = 1;
			}
			for (int k = start; k < childPts.length; k++) {
				combined.add(childPts[k]);
			}
		}
		return combined.toArray(new double[0][]);
	}

	private static boolean isSamePoint(double[] a, double[] b) {
		return a[0] == b[0] && a[1] == b[1];
	}

	private static int parseInt(String s) {
		return new BigDecimal(s.trim()).intValue();
	}

	private static double parseDouble(String s) {
		return Double.parseDouble(s.trim());
	}

	private static int[] parseInts(String[] values) {
		int[] result = new int[values.length];
		for (int i = 0; i < values.length; i++) {
			result[i] = parseInt(values[i]);
		}
		return result;
	}

	private static double[] parseDoubles(String[] values) {
		double[] result = new double[values.length];
		for (int i = 0; i < values.length; i++) {
			result[i] = parseDouble(values[i]);
		}
		return result;
	}

}
