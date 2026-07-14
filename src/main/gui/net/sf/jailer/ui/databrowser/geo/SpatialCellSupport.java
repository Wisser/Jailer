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

import java.lang.reflect.Method;

import net.sf.jailer.util.CellContentConverter;
import net.sf.jailer.util.LogUtil;

/**
 * Recognizes DataBrowser cell values that hold spatial (geometry/geography)
 * data and decodes them into the {@link Geometry} preview model, so callers
 * don't need to know about the various DBMS-specific wrapper types or binary
 * formats. Returns <code>null</code> for anything that isn't a recognized,
 * decodable, WGS84 (SRID 4326/8307) spatial value - callers should fall back
 * to the normal text rendering in that case. Non-WGS84 (e.g. local/planar,
 * SRID 0) geometries are deliberately not previewed either: the preview only
 * exists to show the geometry on its OpenStreetMap background, and there is
 * no map to show one on otherwise.
 *
 * <p>For PostgreSQL/PostGIS, {@code CellContentConverter} routes geometry and
 * geography columns through its generic {@code sqlExpressionRule} mechanism:
 * the cell value is a {@code SQLExpressionWrapper} whose wrapped value is the
 * driver's raw {@code org.postgresql.util.PGobject} (type name "geometry"/
 * "geography", string value the EWKB hex). That {@code PGobject} is accessed
 * via reflection on its simple class name, exactly like
 * {@code CellContentConverter} itself does, to avoid a hard compile-time
 * dependency on whichever PostgreSQL driver jar a connection happens to load
 * at runtime.
 *
 * @author Ralf Wisser
 */
public class SpatialCellSupport {

	private SpatialCellSupport() {
	}

	/**
	 * Tries to decode a DataBrowser cell's raw value into a {@link Geometry}.
	 *
	 * @param rawValue the cell's raw (un-formatted) value
	 * @return the decoded geometry, or <code>null</code> if the value isn't a
	 *         recognized/decodable spatial value
	 */
	public static Geometry parse(Object rawValue) {
		Geometry g = decode(rawValue);
		if (g == null || !Geometry.isWgs84(g.getSrid())) {
			return null;
		}
		return g;
	}

	private static Geometry decode(Object rawValue) {
		try {
			if (rawValue instanceof CellContentConverter.SpatialValueWrapper) {
				CellContentConverter.SpatialValueWrapper w = (CellContentConverter.SpatialValueWrapper) rawValue;
				if (w.getWkbHex() == null) {
					return null;
				}
				return WkbGeometryParser.parseWkb(w.getWkbHex(), w.getSrid());
			}
			if (rawValue instanceof CellContentConverter.SQLExpressionWrapper) {
				return parsePgObject(((CellContentConverter.SQLExpressionWrapper) rawValue).getValue());
			}
			if (rawValue instanceof CellContentConverter.PObjectWrapper) {
				CellContentConverter.PObjectWrapper w = (CellContentConverter.PObjectWrapper) rawValue;
				String type = w.getType();
				if (type == null || w.getValue() == null) {
					return null;
				}
				if (!"geometry".equalsIgnoreCase(type) && !"geography".equalsIgnoreCase(type)) {
					return null;
				}
				return WkbGeometryParser.parseEwkb(w.getValue());
			}
			if (rawValue instanceof CellContentConverter.OracleSpatialValueWrapper) {
				CellContentConverter.OracleSpatialValueWrapper w = (CellContentConverter.OracleSpatialValueWrapper) rawValue;
				return SdoGeometryParser.parse(w.getGtype(), w.getSrid(), w.getPoint(), w.getElemInfo(), w.getOrdinates());
			}
			if (rawValue instanceof CellContentConverter.MSSQLSpatialValueWrapper) {
				CellContentConverter.MSSQLSpatialValueWrapper w = (CellContentConverter.MSSQLSpatialValueWrapper) rawValue;
				return MssqlGeometryParser.parse(w.getHex());
			}
			// Not wrapped at all in some code paths (e.g. target DBMS != source DBMS) -
			// the raw PGobject may reach here directly.
			return parsePgObject(rawValue);
		} catch (GeometryParseException e) {
			LogUtil.warn(e);
			return null;
		} catch (RuntimeException e) {
			// malformed/unexpected data - fall back to the text rendering rather than crashing the DataBrowser
			LogUtil.warn(e);
			return null;
		}
	}

	/**
	 * Recognizes a PostgreSQL {@code org.postgresql.util.PGobject} (by simple
	 * class name, not by type, so it works regardless of which driver jar/
	 * classloader produced it) whose PG type is "geometry"/"geography", and
	 * decodes its EWKB hex value.
	 */
	private static Geometry parsePgObject(Object value) throws GeometryParseException {
		if (value == null || !"PGobject".equals(value.getClass().getSimpleName())) {
			return null;
		}
		String type = reflectInvokeString(value, "getType");
		if (type == null) {
			return null;
		}
		type = type.replace("\"", "").trim();
		if (!"geometry".equalsIgnoreCase(type) && !"geography".equalsIgnoreCase(type)) {
			return null;
		}
		String hex = reflectInvokeString(value, "getValue");
		if (hex == null) {
			return null;
		}
		return WkbGeometryParser.parseEwkb(hex);
	}

	private static String reflectInvokeString(Object target, String methodName) {
		try {
			Method m = target.getClass().getMethod(methodName);
			Object result = m.invoke(target);
			return result == null ? null : result.toString();
		} catch (ReflectiveOperationException e) {
			LogUtil.warn(e);
			return null;
		}
	}

}
