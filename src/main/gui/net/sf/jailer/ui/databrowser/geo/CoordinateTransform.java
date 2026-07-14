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
 * Maps a geometry (x, y) coordinate pair to a screen pixel coordinate.
 * Implemented once for the plain auto-fit Cartesian case
 * ({@code GeometryPreviewPanel}'s own transform) and once for the Web
 * Mercator projection ({@code OsmTileLayer.MercatorView}), so the drawing
 * code in {@code GeometryPreviewPanel} doesn't need to care which one is
 * active.
 *
 * @author Ralf Wisser
 */
public interface CoordinateTransform {

	/**
	 * @param x the geometry x ordinate (or longitude, in degrees)
	 * @param y the geometry y ordinate (or latitude, in degrees)
	 * @return the screen pixel coordinate as <code>{ x, y }</code>
	 */
	double[] toScreen(double x, double y);

}
