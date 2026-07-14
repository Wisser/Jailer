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
 * Decodes standard WKB (Well-Known Binary, used by MySQL/MariaDB's
 * {@code ST_AsBinary()}/internal geometry format) and PostGIS's Extended WKB
 * (EWKB, which additionally may carry Z/M ordinates and an embedded SRID
 * flag) into the minimal {@link Geometry} model.
 *
 * <p>Curved geometry types are not part of plain WKB/EWKB, so there is
 * nothing to approximate here (unlike the Oracle SDO and SQL Server native
 * format decoders).
 *
 * @author Ralf Wisser
 */
public class WkbGeometryParser {

	private static final int TYPE_POINT = 1;
	private static final int TYPE_LINESTRING = 2;
	private static final int TYPE_POLYGON = 3;
	// MultiPoint (4), MultiLineString (5), MultiPolygon (6) and GeometryCollection (7)
	// all share the same wire format: a count followed by that many full,
	// self-describing sub-geometries - so they share one code path.

	private WkbGeometryParser() {
	}

	/**
	 * Parses a standard WKB hex string (no embedded SRID) using the given SRID.
	 *
	 * @param wkbHex the WKB payload, hex-encoded
	 * @param srid the SRID to associate with the resulting geometry
	 * @return the parsed geometry
	 * @throws GeometryParseException if the data cannot be decoded
	 */
	public static Geometry parseWkb(String wkbHex, int srid) throws GeometryParseException {
		return parse(wkbHex, srid);
	}

	/**
	 * Parses an Extended WKB (EWKB) hex string, taking the SRID from the
	 * embedded SRID flag if present, defaulting to <code>0</code> otherwise.
	 *
	 * @param ewkbHex the EWKB payload, hex-encoded
	 * @return the parsed geometry
	 * @throws GeometryParseException if the data cannot be decoded
	 */
	public static Geometry parseEwkb(String ewkbHex) throws GeometryParseException {
		return parse(ewkbHex, null);
	}

	private static Geometry parse(String hex, Integer defaultSrid) throws GeometryParseException {
		if (hex == null || hex.trim().isEmpty()) {
			throw new GeometryParseException("Empty WKB/EWKB data");
		}
		ByteReader r = new ByteReader(hexToBytes(hex.trim()));
		return parseGeometry(r, defaultSrid);
	}

	private static Geometry parseGeometry(ByteReader r, Integer defaultSrid) throws GeometryParseException {
		int order = r.readUnsignedByte();
		boolean little = order == 1;
		long rawType = r.readUInt32(little);

		boolean hasZ = (rawType & 0x80000000L) != 0;
		boolean hasM = (rawType & 0x40000000L) != 0;
		boolean hasSrid = (rawType & 0x20000000L) != 0;
		int baseType = (int) (rawType & 0xFF);

		if (!hasZ && !hasM && !hasSrid && baseType > TYPE_POLYGON + 4) {
			// Not EWKB - could be ISO SQL/MM WKB, which encodes Z/M as +1000/+2000/+3000
			// on the base type instead of using high-bit flags.
			int isoBase = (int) (rawType % 1000);
			int dim = (int) (rawType / 1000);
			if (isoBase >= TYPE_POINT && isoBase <= 7) {
				baseType = isoBase;
				hasZ = dim == 1 || dim == 3;
				hasM = dim == 2 || dim == 3;
			}
		}

		int srid = defaultSrid != null ? defaultSrid : 0;
		if (hasSrid) {
			srid = (int) r.readUInt32(little);
		}

		int dims = 2 + (hasZ ? 1 : 0) + (hasM ? 1 : 0);

		switch (baseType) {
			case TYPE_POINT:
				return readPoint(r, little, dims, srid);
			case TYPE_LINESTRING:
				return readLineString(r, little, dims, srid);
			case TYPE_POLYGON:
				return readPolygon(r, little, dims, srid);
			case 4: // MultiPoint
			case 5: // MultiLineString
			case 6: // MultiPolygon
			case 7: // GeometryCollection
				return readCollection(r, little, srid);
			default:
				throw new GeometryParseException("Unsupported WKB geometry type: " + baseType);
		}
	}

	private static Geometry.Point readPoint(ByteReader r, boolean little, int dims, int srid) throws GeometryParseException {
		double[] c = readCoordinate(r, little, dims);
		return new Geometry.Point(srid, c[0], c[1]);
	}

	private static Geometry.LineString readLineString(ByteReader r, boolean little, int dims, int srid) throws GeometryParseException {
		int n = (int) r.readUInt32(little);
		double[][] points = new double[n][];
		for (int i = 0; i < n; i++) {
			points[i] = readCoordinate(r, little, dims);
		}
		return new Geometry.LineString(srid, points);
	}

	private static Geometry.Polygon readPolygon(ByteReader r, boolean little, int dims, int srid) throws GeometryParseException {
		int numRings = (int) r.readUInt32(little);
		double[][][] rings = new double[numRings][][];
		for (int ring = 0; ring < numRings; ring++) {
			int n = (int) r.readUInt32(little);
			double[][] points = new double[n][];
			for (int i = 0; i < n; i++) {
				points[i] = readCoordinate(r, little, dims);
			}
			rings[ring] = points;
		}
		return new Geometry.Polygon(srid, rings);
	}

	private static Geometry.GeometryCollection readCollection(ByteReader r, boolean little, int srid) throws GeometryParseException {
		int n = (int) r.readUInt32(little);
		Geometry[] geoms = new Geometry[n];
		for (int i = 0; i < n; i++) {
			geoms[i] = parseGeometry(r, srid);
		}
		return new Geometry.GeometryCollection(srid, geoms);
	}

	private static double[] readCoordinate(ByteReader r, boolean little, int dims) throws GeometryParseException {
		double x = r.readDouble(little);
		double y = r.readDouble(little);
		for (int i = 2; i < dims; i++) {
			r.readDouble(little); // discard Z/M - not needed for a 2D preview
		}
		return new double[] { x, y };
	}

	private static byte[] hexToBytes(String hex) throws GeometryParseException {
		if (hex.length() % 2 != 0) {
			throw new GeometryParseException("Invalid WKB hex length: " + hex.length());
		}
		byte[] out = new byte[hex.length() / 2];
		for (int i = 0; i < out.length; i++) {
			int hi = Character.digit(hex.charAt(2 * i), 16);
			int lo = Character.digit(hex.charAt(2 * i + 1), 16);
			if (hi < 0 || lo < 0) {
				throw new GeometryParseException("Invalid WKB hex character at position " + (2 * i));
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

		long readUInt32(boolean little) throws GeometryParseException {
			ensure(4);
			long b0 = data[pos] & 0xFFL;
			long b1 = data[pos + 1] & 0xFFL;
			long b2 = data[pos + 2] & 0xFFL;
			long b3 = data[pos + 3] & 0xFFL;
			pos += 4;
			return little ? (b0 | (b1 << 8) | (b2 << 16) | (b3 << 24)) : ((b0 << 24) | (b1 << 16) | (b2 << 8) | b3);
		}

		double readDouble(boolean little) throws GeometryParseException {
			ensure(8);
			long v = 0;
			if (little) {
				for (int i = 7; i >= 0; i--) {
					v = (v << 8) | (data[pos + i] & 0xFFL);
				}
			} else {
				for (int i = 0; i < 8; i++) {
					v = (v << 8) | (data[pos + i] & 0xFFL);
				}
			}
			pos += 8;
			return Double.longBitsToDouble(v);
		}

		private void ensure(int n) throws GeometryParseException {
			if (pos + n > data.length) {
				throw new GeometryParseException("Unexpected end of WKB data (need " + n + " more bytes at position " + pos + ", have " + data.length + ")");
			}
		}
	}

}
