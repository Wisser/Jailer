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
package net.sf.jailer.ui.databrowser.sqlconsole;

import java.awt.Color;

/**
 * Named color palettes shared between the "Chart" tab ({@link SQLConsoleChartPanel}) and the map
 * overlay's per-row geo-object legend ({@link TabContentPanel}), so both offer/use exactly the
 * same color schemes from a single source of truth.
 *
 * @author Ralf Wisser
 */
final class ChartColorPalettes {

	private ChartColorPalettes() {
	}

	private static final Color[] PALETTE_PASTEL      = { new Color(174,199,232), new Color(255,187,120), new Color(152,223,138), new Color(255,152,150), new Color(197,176,213), new Color(196,156,148), new Color(255,247,153), new Color(162,228,226), new Color(250,210,210), new Color(235,210,165) };
	private static final Color[] PALETTE_EARTH       = { new Color(140, 86, 75), new Color(214,139, 74), new Color(188,189, 34), new Color( 23,190,207), new Color( 44,160, 44), new Color(127,127,127), new Color(222,184,135), new Color(143,161,114), new Color( 99, 67, 41), new Color(178,130, 55) };
	private static final Color[] PALETTE_COLORBLIND  = { new Color(  0,114,178), new Color(230,159,  0), new Color(  0,158,115), new Color(213, 94,  0), new Color( 86,180,233), new Color(204,121,167), new Color(0,0,0), new Color(240,228,66), new Color(238,136,102), new Color( 51, 34,136), new Color(136,204,170), new Color(136, 34, 85) };
	private static final Color[] PALETTE_MONOCHROME  = { new Color( 30, 30, 30), new Color( 80, 80, 80), new Color(130,130,130), new Color(180,180,180), new Color( 50, 50, 50), new Color(210,210,210), new Color(  5,  5,  5), new Color(105,105,105), new Color(158,158,158), new Color(235,235,235) };
	private static final Color[] PALETTE_OCEAN       = { new Color(  4, 90,141), new Color(  5,142,217), new Color(  2,194,197), new Color( 95,218,214), new Color(  0, 77, 64), new Color( 38,166,154), new Color(  7, 27, 76), new Color( 29,105,150), new Color(127,205,187), new Color(175,240,232) };
	private static final Color[] PALETTE_SUNSET      = { new Color(255, 87, 34), new Color(255,152,  0), new Color(255,193,  7), new Color(233, 30, 99), new Color(156, 39,176), new Color( 63, 81,181), new Color(255,111, 97), new Color(183,  0, 56), new Color(255,214, 90), new Color(106,  0,244) };
	private static final Color[] PALETTE_VIBRANT     = { new Color(220, 20, 60), new Color(  0,128,255), new Color(  0,200, 81), new Color(255,140,  0), new Color(148,  0,211), new Color(  0,210,210), new Color(255,220,  0), new Color(255,  0,153), new Color(100,255,  0), new Color( 65,  0,255) };
	private static final Color[] PALETTE_FOREST      = { new Color( 27, 94, 32), new Color( 85,139, 47), new Color(156,204,101), new Color(  0, 77, 64), new Color( 38,166,154), new Color(121, 85, 72), new Color( 72, 40, 20), new Color(163, 90, 33), new Color(200,160, 50), new Color(145,120,100) };
	private static final Color[] PALETTE_RETRO       = { new Color(188, 80, 80), new Color(210,140, 70), new Color(220,200,100), new Color(100,155,105), new Color( 80,110,160), new Color(140, 90,150), new Color( 80,140,140), new Color(155,130, 80), new Color( 70, 80,120), new Color(120,130, 70) };
	private static final Color[] PALETTE_NEON        = { new Color( 57,255, 20), new Color(  0,245,255), new Color(255,  7, 58), new Color(255,234,  0), new Color(185,  0,255), new Color(255,128,  0), new Color(255,  0,200), new Color(180,255,  0), new Color(  0, 80,255), new Color(  0,255,160) };
	private static final Color[] PALETTE_CORPORATE   = { new Color( 31, 73,125), new Color(112,173, 71), new Color(255,192,  0), new Color( 68,114,196), new Color(237,125, 49), new Color(112, 48,160), new Color(192, 80, 77), new Color( 75,172,198), new Color( 17, 46, 81), new Color(166,166,166) };
	private static final Color[] PALETTE_SPRING      = { new Color(255, 80,120), new Color( 60,180, 60), new Color(240,200,  0), new Color( 60,150,220), new Color(255, 90, 60), new Color(160, 60,200), new Color( 60,210,170), new Color(190,140,220), new Color(255,180,120), new Color(  0,190,230) };

	/** Names offered by the Chart tab's color-scheme combobox - includes {@code "Darkness"}, a JFreeChart built-in theme rather than a real palette (see {@link #getPalette}). */
	static final String[] CHART_PALETTE_NAMES = { "Corporate", "Pastel", "Earth", "Colorblind", "Monochrome", "Darkness", "Ocean", "Sunset", "Vibrant", "Forest", "Retro", "Neon", "Spring" };

	/** Names usable as an actual {@code Color[]} palette - {@link #CHART_PALETTE_NAMES} minus {@code "Darkness"}, which has no palette (a null palette makes no sense for coloring geo-objects). */
	static final String[] GEO_PALETTE_NAMES = { "Corporate", "Pastel", "Earth", "Colorblind", "Monochrome", "Ocean", "Sunset", "Vibrant", "Forest", "Retro", "Neon", "Spring" };

	/**
	 * @param scheme a name from {@link #CHART_PALETTE_NAMES} (or {@link #GEO_PALETTE_NAMES}), or {@code null}
	 * @return the corresponding palette, {@code null} for {@code "Darkness"} (not a real palette), or
	 *         {@link #PALETTE_CORPORATE} for an unrecognized name (matches the Chart tab's prior default)
	 */
	static Color[] getPalette(String scheme) {
		switch (scheme != null ? scheme : "Spring") {
			case "Pastel":     return PALETTE_PASTEL;
			case "Earth":      return PALETTE_EARTH;
			case "Colorblind": return PALETTE_COLORBLIND;
			case "Monochrome": return PALETTE_MONOCHROME;
			case "Ocean":      return PALETTE_OCEAN;
			case "Sunset":     return PALETTE_SUNSET;
			case "Vibrant":    return PALETTE_VIBRANT;
			case "Forest":     return PALETTE_FOREST;
			case "Retro":      return PALETTE_RETRO;
			case "Neon":       return PALETTE_NEON;
			case "Spring":     return PALETTE_SPRING;
			case "Darkness":   return null;
			default:           return PALETTE_CORPORATE;
		}
	}

}
