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

import java.util.ArrayList;
import java.util.List;

/**
 * Decodes SQL Server's proprietary native binary serialization format for
 * {@code geometry}/{@code geography} values (as read via
 * {@code CellContentConverter.MSSQLSpatialValueWrapper} - not OGC WKB, a
 * distinct format) into the {@link Geometry} preview model.
 *
 * <p><b>Scope limit:</b> only serialization "Version 1" (the common case -
 * no circular arcs/curves) is supported. Version 2 adds a trailing
 * segment-type array to support curves, whose exact byte layout isn't
 * something that could be confidently reconstructed from memory alone (unlike
 * approximating an already-decoded curve as straight line segments, guessing
 * wrong here would silently corrupt every subsequent byte read, not just
 * misdraw one element) - and there is no live SQL Server instance available
 * in this environment to verify against. Version 2 payloads, and the rarely
 * used "IsLargerThanStandard" flag, are reported as a {@link GeometryParseException}
 * (falling back to plain text) rather than guessed at.
 *
 * <p>The single-point fast path's header/flag layout was cross-checked
 * against a widely published reference value (SRID 4326, {@code POINT(3,4)}
 * serializes to hex {@code E6100000010C00000000000008400000000000001040}) -
 * the general Points/Figures/Shapes structure below it is reconstructed from
 * the documented format and open-source reference readers, but has not been
 * validated against real SQL Server output and may need correction.
 *
 * @author Ralf Wisser
 */
public class MssqlGeometryParser {

	private static final int FLAG_HAS_Z = 0x01;
	private static final int FLAG_HAS_M = 0x02;
	private static final int FLAG_IS_SINGLE_POINT = 0x08;
	private static final int FLAG_IS_SINGLE_LINE_SEGMENT = 0x10;
	private static final int FLAG_IS_LARGER_THAN_STANDARD = 0x20;

	private static final int OGC_POINT = 1;
	private static final int OGC_LINESTRING = 2;
	private static final int OGC_POLYGON = 3;

	private MssqlGeometryParser() {
	}

	/**
	 * @param hex the raw native-format bytes, hex-encoded
	 * @return the decoded geometry
	 * @throws GeometryParseException if the data cannot be decoded (including unsupported version/flags)
	 */
	public static Geometry parse(String hex) throws GeometryParseException {
		if (hex == null || hex.trim().isEmpty()) {
			throw new GeometryParseException("Empty SQL Server geometry data");
		}
		ByteReader r = new ByteReader(hexToBytes(hex.trim()));

		int rawSrid = r.readInt32LE();
		int srid = rawSrid == -1 ? 0 : rawSrid;
		int version = r.readUnsignedByte();
		int flags = r.readUnsignedByte();

		if ((flags & FLAG_IS_LARGER_THAN_STANDARD) != 0) {
			throw new GeometryParseException("SQL Server geometry 'IsLargerThanStandard' payloads are not supported");
		}
		if (version != 1) {
			throw new GeometryParseException("SQL Server geometry serialization version " + version + " is not supported (only version 1)");
		}

		boolean hasZ = (flags & FLAG_HAS_Z) != 0;
		boolean hasM = (flags & FLAG_HAS_M) != 0;
		int extraOrdinatesPerPoint = (hasZ ? 1 : 0) + (hasM ? 1 : 0);

		if ((flags & FLAG_IS_SINGLE_POINT) != 0) {
			double x = r.readDouble();
			double y = r.readDouble();
			return new Geometry.Point(srid, x, y);
		}
		if ((flags & FLAG_IS_SINGLE_LINE_SEGMENT) != 0) {
			double x1 = r.readDouble();
			double y1 = r.readDouble();
			double x2 = r.readDouble();
			double y2 = r.readDouble();
			return new Geometry.LineString(srid, new double[][] { { x1, y1 }, { x2, y2 } });
		}

		int numPoints = r.readInt32LE();
		double[][] points = new double[numPoints][2];
		for (int i = 0; i < numPoints; i++) {
			points[i][0] = r.readDouble();
			points[i][1] = r.readDouble();
		}
		for (int i = 0; i < numPoints * extraOrdinatesPerPoint; i++) {
			r.readDouble(); // discard Z/M - not needed for a 2D preview
		}

		int numFigures = r.readInt32LE();
		int[] figureAttribute = new int[numFigures];
		int[] figurePointOffset = new int[numFigures];
		for (int i = 0; i < numFigures; i++) {
			figureAttribute[i] = r.readUnsignedByte();
			figurePointOffset[i] = r.readInt32LE();
		}

		int numShapes = r.readInt32LE();
		int[] shapeParentOffset = new int[numShapes];
		int[] shapeFigureOffset = new int[numShapes];
		int[] shapeType = new int[numShapes];
		for (int i = 0; i < numShapes; i++) {
			shapeParentOffset[i] = r.readInt32LE();
			shapeFigureOffset[i] = r.readInt32LE();
			shapeType[i] = r.readUnsignedByte();
		}

		List<Geometry> leaves = new ArrayList<Geometry>();
		for (int s = 0; s < numShapes; s++) {
			int type = shapeType[s];
			if (type != OGC_POINT && type != OGC_LINESTRING && type != OGC_POLYGON) {
				continue; // container shape (Multi*/GeometryCollection) - no geometry of its own
			}
			if (shapeFigureOffset[s] < 0) {
				continue; // empty shape
			}
			int figureEnd = numFigures;
			for (int j = s + 1; j < numShapes; j++) {
				if (shapeFigureOffset[j] >= 0) {
					figureEnd = shapeFigureOffset[j];
					break;
				}
			}
			double[][][] rings = readFigureRanges(shapeFigureOffset[s], figureEnd, figurePointOffset, numPoints, points);
			if (type == OGC_POINT) {
				double[][] ring = rings[0];
				leaves.add(new Geometry.Point(srid, ring[0][0], ring[0][1]));
			} else if (type == OGC_LINESTRING) {
				leaves.add(new Geometry.LineString(srid, rings[0]));
			} else {
				leaves.add(new Geometry.Polygon(srid, rings));
			}
		}

		if (leaves.isEmpty()) {
			throw new GeometryParseException("SQL Server geometry decoded to no elements");
		}
		if (leaves.size() == 1) {
			return leaves.get(0);
		}
		return new Geometry.GeometryCollection(srid, leaves.toArray(new Geometry[0]));
	}

	private static double[][][] readFigureRanges(int figureStart, int figureEnd, int[] figurePointOffset, int numPoints, double[][] points) throws GeometryParseException {
		int count = figureEnd - figureStart;
		if (count <= 0) {
			throw new GeometryParseException("Shape references no figures");
		}
		double[][][] rings = new double[count][][];
		for (int f = figureStart; f < figureEnd; f++) {
			int start = figurePointOffset[f];
			int end = f + 1 < figurePointOffset.length ? figurePointOffset[f + 1] : numPoints;
			if (start < 0 || end > numPoints || end < start) {
				throw new GeometryParseException("Figure point offset out of range");
			}
			double[][] ring = new double[end - start][];
			System.arraycopy(points, start, ring, 0, end - start);
			rings[f - figureStart] = ring;
		}
		return rings;
	}

	private static byte[] hexToBytes(String hex) throws GeometryParseException {
		if (hex.length() % 2 != 0) {
			throw new GeometryParseException("Invalid SQL Server geometry hex length: " + hex.length());
		}
		byte[] out = new byte[hex.length() / 2];
		for (int i = 0; i < out.length; i++) {
			int hi = Character.digit(hex.charAt(2 * i), 16);
			int lo = Character.digit(hex.charAt(2 * i + 1), 16);
			if (hi < 0 || lo < 0) {
				throw new GeometryParseException("Invalid SQL Server geometry hex character at position " + (2 * i));
			}
			out[i] = (byte) ((hi << 4) | lo);
		}
		return out;
	}

	private static class ByteReader {
		private final byte[] data;
		private int pos;

		ByteReader(byte[] data) {
			this.data = data;
		}

		int readUnsignedByte() throws GeometryParseException {
			ensure(1);
			return data[pos++] & 0xFF;
		}

		int readInt32LE() throws GeometryParseException {
			ensure(4);
			int v = (data[pos] & 0xFF) | ((data[pos + 1] & 0xFF) << 8) | ((data[pos + 2] & 0xFF) << 16) | ((data[pos + 3] & 0xFF) << 24);
			pos += 4;
			return v;
		}

		double readDouble() throws GeometryParseException {
			ensure(8);
			long v = 0;
			for (int i = 7; i >= 0; i--) {
				v = (v << 8) | (data[pos + i] & 0xFFL);
			}
			pos += 8;
			return Double.longBitsToDouble(v);
		}

		private void ensure(int n) throws GeometryParseException {
			if (pos + n > data.length) {
				throw new GeometryParseException("Unexpected end of SQL Server geometry data (need " + n + " more bytes at position " + pos + ", have " + data.length + ")");
			}
		}
	}

}
