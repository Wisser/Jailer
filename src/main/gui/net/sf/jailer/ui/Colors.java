/*
 * Copyright 2007 - 2024 Ralf Wisser.
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
package net.sf.jailer.ui;

import java.awt.Color;
import java.awt.PaintContext;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Window;
import java.awt.color.ColorSpace;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.awt.image.ColorModel;
import java.util.ArrayList;
import java.util.List;

import javax.swing.Timer;

import net.sf.jailer.ui.UIUtil.PLAF;

/**
 * PLaF aware manager of UI colors.
 * 
 * @author Ralf Wisser
 */
public class Colors {
	
	private static void initColors() {
		colors.clear();
		
		GraphicalDataViewBackground.init(new Color(255, 255, 255), new Color(31, 39, 42));
		Color_170_0_0.init(new Color(170, 0, 0), new Color(255, 70, 50));
		Color_200_200_255.init(new Color(200, 200, 255), new Color(0, 0, 0));
		Color_0_100_255.init(new Color(0, 100, 255), new Color(0, 176, 255));
		Color_0_112_0.init(new Color(0, 112, 0), new Color(0, 215, 0));
		Color_255_255_255.init(Color.white, new Color(50, 53, 57));
		
		// TODO
		// TODO rsyntax: keywords blue, as in outline
		// TODO in outline: farben nach keywords am Anfang dezenter
		
		// TODO
		// desktop background darker? as in SQLConsole?
		
		HTMLColor_000000 = UIUtil.plaf != PLAF.FLATDARK? "\"#000000\"" : "\"#dddddd\"";
		HTMLColor_006600 = UIUtil.plaf != PLAF.FLATDARK? "\"#006600\"" : "\"#00c000\"";
		HTMLColor_0000B0 = UIUtil.plaf != PLAF.FLATDARK? "\"#0000B0\"" : "\"#00B0FF\"";
		HTMLColor_0000D0 = UIUtil.plaf != PLAF.FLATDARK? "\"#0000D0\"" : "\"#00B8FF\"";
		HTMLColor_0000cc = UIUtil.plaf != PLAF.FLATDARK? "\"#0000CC\"" : "\"#00B8FF\"";
		HTMLColor_0000dd = UIUtil.plaf != PLAF.FLATDARK? "\"#0000D0\"" : "\"#00B8FF\"";
		HTMLColor_0000ff = UIUtil.plaf != PLAF.FLATDARK? "\"#0000ff\"" : "\"#9999ff\"";
		HTMLColor_005500 = "\"#005500\"";
		HTMLColor_006000 = UIUtil.plaf != PLAF.FLATDARK? "\"#006000\"" : "\"#99ff99\"";
		HTMLColor_0066ff = "\"#0066ff\"";
		HTMLColor_008000 = "\"#008000\"";
		HTMLColor_050aff = "\"#050aff\"";
		HTMLColor_663300 = UIUtil.plaf != PLAF.FLATDARK? "\"#663300\"" : "\"#ff6600\"";
		HTMLColor_707080 = "\"#707080\"";
		HTMLColor_808080 = "\"#808080\"";
		HTMLColor_888888 = "\"#888888\"";
		HTMLColor_dd0000 = "\"#dd0000\"";
		HTMLColor_dd8888 = "\"#dd8888\"";
		HTMLColor_eeeeff = "\"#eeeeff\"";
		HTMLColor_eeffee = "\"#eeffee\"";
		HTMLColor_ff0000 = "\"#ff0000\"";
		HTMLColor_ff2222 = "\"#ff2222\"";
		HTMLColor_ff9999 = "\"#ff9999\"";

		Color_0_0_0.init(new Color(0, 0, 0));
		Color_0_0_0_0.init(new Color(0, 0, 0, 0));
		Color_0_0_0_60.init(new Color(0, 0, 0, 60));
		
		Color_0_0_1.init(new Color(0, 0, 1), Color_0_0_0.dark);
		
		Color_0_102_0.init(new Color(0, 102, 0));
		Color_0_0_145.init(new Color(0, 0, 145));
		Color_0_0_150.init(new Color(0, 0, 150));
		Color_0_0_180.init(new Color(0, 0, 180));
		Color_0_0_200.init(new Color(0, 0, 200));
		Color_0_0_200_100.init(new Color(0, 0, 200, 100));
		Color_0_0_200_60.init(new Color(0, 0, 200, 60));
		Color_0_0_205.init(new Color(0, 0, 205));
		Color_0_0_220.init(new Color(0, 0, 220));
		Color_0_0_225.init(new Color(0, 0, 225));
		Color_0_0_245_60.init(new Color(0, 0, 245, 60));
		Color_0_0_255.init(new Color(0, 0, 255));
		Color_0_0_255_80.init(new Color(0, 0, 255, 80));
		Color_0_0_62.init(new Color(0, 0, 62));
		Color_0_0_77.init(new Color(0, 0, 77));
		Color_0_100_0.init(new Color(0, 100, 0), Color_0_102_0);
		Color_0_100_200.init(new Color(0, 100, 200));
		
		Color_0_105_0.init(new Color(0, 105, 0), Color_0_102_0.dark);

		Color_0_130_0.init(new Color(0, 130, 0));
		Color_0_176_0.init(new Color(0, 176, 0));
		Color_0_200_0.init(new Color(0, 200, 0));
		Color_0_200_255.init(new Color(0, 200, 255));
		Color_0_230_0.init(new Color(0, 230, 0));
		Color_0_255_0_50.init(new Color(0, 255, 0, 50));
		Color_0_255_230_70.init(new Color(0, 255, 230, 70));
		Color_0_255_255_150.init(new Color(0, 255, 255, 150));
		Color_0_30_255.init(new Color(0, 30, 255));
		Color_0_40_255.init(new Color(0, 40, 255));
		Color_0_40_90.init(new Color(0, 40, 90));
		Color_0_55_0.init(new Color(0, 55, 0));
		Color_0_80_160.init(new Color(0, 80, 160));
		Color_0_80_200.init(new Color(0, 80, 200));
		Color_0_80_255_80.init(new Color(0, 80, 255, 80));
		Color_0_96_0.init(new Color(0, 96, 0));
		Color_100_0_0.init(new Color(100, 0, 0));
		Color_100_100_100.init(new Color(100, 100, 100));
		Color_100_110_210.init(new Color(100, 110, 210));
		Color_115_0_0.init(new Color(115, 0, 0));
		Color_115_217_255.init(new Color(115, 217, 255));
		Color_122_210_255_200.init(new Color(122, 210, 255, 200));
		Color_128_128_128.init(new Color(128, 128, 128));
		
		Color_130_130_130.init(new Color(130, 130, 130), Color_128_128_128.dark);
		
		Color_137_176_212.init(new Color(137, 176, 212));
		Color_140_0_0.init(new Color(140, 0, 0));
		Color_140_158_255.init(new Color(140, 158, 255));
		Color_141_16_16.init(new Color(141, 16, 16));
		Color_145_50_0.init(new Color(145, 50, 0));
		Color_150_0_0.init(new Color(150, 0, 0));
		Color_150_0_100.init(new Color(150, 0, 100));
		Color_150_150_150.init(new Color(150, 150, 150));
		Color_150_255_0_70.init(new Color(150, 255, 0, 70));
		
		Color_153_153_153.init(new Color(153, 153, 153), Color_150_150_150.dark);
		
		Color_155_0_0.init(new Color(155, 0, 0));
		Color_160_130_100.init(new Color(160, 130, 100));
		Color_160_200_255.init(new Color(160, 200, 255), new Color(160, 200, 255));
		Color_160_80_0.init(new Color(160, 80, 0));
		Color_170_200_0.init(new Color(170, 200, 0));
		Color_180_160_0.init(new Color(180, 160, 0));
		Color_180_255_220.init(new Color(180, 255, 220));
		Color_190_210_255.init(new Color(190, 210, 255));
		Color_190_255_180.init(new Color(190, 255, 180));
		Color_196_236_255.init(new Color(196, 236, 255));
		
		Color_1_0_0.init(new Color(1, 0, 0), Color_0_0_0.dark);
		
		Color_1_75_1.init(new Color(1, 75, 1));
		Color_200_100_200.init(new Color(200, 100, 200));
		Color_200_200_0.init(new Color(200, 200, 0));
		Color_200_200_200.init(new Color(200, 200, 200));
		Color_200_200_200_140.init(new Color(200, 200, 200, 140));
		Color_200_255_180.init(new Color(200, 255, 180));
		Color_204_204_204.init(new Color(204, 204, 204));
		Color_204_255_178.init(new Color(204, 255, 178));
		Color_205_255_205.init(new Color(205, 255, 205));
		Color_208_245_255.init(new Color(208, 245, 255));
		Color_220_20_20.init(new Color(220, 20, 20));
		Color_220_220_220.init(new Color(220, 220, 220));
		Color_220_220_255.init(new Color(220, 220, 255));
		Color_220_225_255.init(new Color(220, 225, 255));
		Color_220_255_220.init(new Color(220, 255, 220));
		Color_220_255_220_70.init(new Color(220, 255, 220, 70));
		Color_224_224_224.init(new Color(224, 224, 224));
		Color_224_240_255.init(new Color(224, 240, 255));
		Color_228_228_232.init(new Color(228, 228, 232));
		Color_228_238_255.init(new Color(228, 238, 255));
		Color_230_230_230.init(new Color(230, 230, 230));
		Color_230_80_50.init(new Color(230, 80, 50));
		Color_232_232_255.init(new Color(232, 232, 255));   // like Color_235_235_255 ?
		Color_235_235_255.init(new Color(235, 235, 255));
		Color_238_255_238.init(new Color(238, 255, 238));
		Color_240_240_255.init(new Color(240, 240, 255));    // like Color_242_242_255 ?
		Color_240_255_255.init(new Color(240, 255, 255));
		Color_242_242_242.init(new Color(242, 242, 242));
		Color_242_242_255.init(new Color(242, 242, 255));
		Color_245_0_0_60.init(new Color(245, 0, 0, 60));
		Color_245_90_60.init(new Color(245, 90, 60));
		Color_247_247_247.init(new Color(247, 247, 247));
		Color_248_252_255.init(new Color(248, 252, 255));   // like Color_250_250_255 ?
		Color_248_255_248.init(new Color(248, 255, 248));
		Color_250_250_255.init(new Color(250, 250, 255));
		Color_254_255_255.init(new Color(254, 255, 255));
		Color_255_0_0_0.init(new Color(255, 0, 0, 0));
		Color_255_0_0_10.init(new Color(255, 0, 0, 10));
		Color_255_0_0_120.init(new Color(255, 0, 0, 120));
		Color_255_0_0_150.init(new Color(255, 0, 0, 150));
		Color_255_0_0_20.init(new Color(255, 0, 0, 20));
		Color_255_0_0_50.init(new Color(255, 0, 0, 50));
		Color_255_0_0_60.init(new Color(255, 0, 0, 60));
		Color_255_0_51.init(new Color(255, 0, 51));
		Color_255_100_100.init(new Color(255, 100, 100));
		Color_255_150_140.init(new Color(255, 150, 140));
		Color_255_153_152.init(new Color(255, 153, 152));
		Color_255_200_200.init(new Color(255, 200, 200));
		Color_255_205_205.init(new Color(255, 205, 205));
		Color_255_206_206.init(new Color(255, 206, 206));
		Color_255_210_180.init(new Color(255, 210, 180));
		Color_255_210_210.init(new Color(255, 210, 210));
		Color_255_220_220.init(new Color(255, 220, 220));
		Color_255_230_200.init(new Color(255, 230, 200));
		Color_255_230_220.init(new Color(255, 230, 220));
		Color_255_230_230.init(new Color(255, 230, 230));
		Color_255_236_236.init(new Color(255, 236, 236));
		Color_255_240_240.init(new Color(255, 240, 240));  // like Color_255_242_240 ?
		Color_255_242_240.init(new Color(255, 242, 240));
		Color_255_243_218.init(new Color(255, 243, 218));
		Color_255_246_206.init(new Color(255, 246, 206));
		Color_255_249_200.init(new Color(255, 249, 200));
		Color_255_250_215.init(new Color(255, 250, 215));
		Color_255_255_0_128.init(new Color(255, 255, 0, 128));
		Color_255_255_170.init(new Color(255, 255, 170));
		Color_255_255_176.init(new Color(255, 255, 176));
		Color_255_255_204.init(new Color(255, 255, 204));
		Color_255_255_205.init(new Color(255, 255, 205));
		Color_255_255_210.init(new Color(255, 255, 210));
		Color_255_255_220.init(new Color(255, 255, 220));
		Color_255_255_230.init(new Color(255, 255, 230));
		Color_255_255_236.init(new Color(255, 255, 236));  // like Color_255_255_238 ?
		Color_255_255_238.init(new Color(255, 255, 238));  // like Color_255_255_240 ?
		Color_255_255_240.init(new Color(255, 255, 240));
		Color_255_255_250.init(new Color(255, 255, 250));
		Color_255_255_255_150.init(new Color(255,255,255,150));
		Color_255_255_255_200.init(new Color(255, 255, 255, 200));
		Color_255_255_255_70.init(new Color(255, 255, 255, 70));
		Color_255_40_0.init(new Color(255, 40, 0));
		Color_255_50_50.init(new Color(255, 50, 50));
		Color_255_80_80.init(new Color(255, 80, 80));
		Color_66_118_187.init(new Color(66, 118, 187));
		Color_70_255_70.init(new Color(70, 255, 70));
		Color_80_200_255_200.init(new Color(80, 200, 255, 200));
		Color_86_82_125.init(new Color(86, 82, 125));
		Color_95_0_0.init(new Color(95, 0, 0));
		Color_96_64_0.init(new Color(96, 64, 0));
		Color_0_255_255.init(new Color(0, 255, 255));
		Color_64_64_64.init(new Color(64, 64, 64));
		Color_0_255_0.init(new Color(0, 255, 0));
		Color_192_192_192.init(new Color(192, 192, 192));
		Color_255_0_0.init(new Color(255, 0, 0));
		Color_255_255_0.init(new Color(255, 255, 0));
	}

	//select x, y from (
	//Select
	//     'Color_' || a.r || '_' || a.g || '_' || a.b || (case when a.i = 255 then '' else '_' || a.i end) x,
	//     'Color_' || b.r || '_' || b.g || '_' || b.b || (case when b.i = 255 then '' else '_' || b.i end) y
	//From
	//     COLOR a,
	//     COLOR b
	//where
	//     abs(a.r - b.r) < 8
	//     and abs(a.g - b.g) < 8
	//     and abs(a.b - b.b) < 8
	//     and abs(a.i - b.i) < 8
	//	and (a.r!=b.r or a.g!=b.g or a.b!=b.b or a.i!=b.i)
	//)
	//where x < y
	
	public static LAFAwareColor GraphicalDataViewBackground = new LAFAwareColor();
	
	public static LAFAwareColor Color_0_0_0 = new LAFAwareColor();
	public static LAFAwareColor Color_0_0_0_0 = new LAFAwareColor();
	public static LAFAwareColor Color_0_0_0_60 = new LAFAwareColor();
	public static LAFAwareColor Color_0_0_1 = new LAFAwareColor();
	public static LAFAwareColor Color_0_0_145 = new LAFAwareColor();
	public static LAFAwareColor Color_0_0_150 = new LAFAwareColor();
	public static LAFAwareColor Color_0_0_180 = new LAFAwareColor();
	public static LAFAwareColor Color_0_0_200 = new LAFAwareColor();
	public static LAFAwareColor Color_0_0_200_100 = new LAFAwareColor();
	public static LAFAwareColor Color_0_0_200_60 = new LAFAwareColor();
	public static LAFAwareColor Color_0_0_205 = new LAFAwareColor();
	public static LAFAwareColor Color_0_0_220 = new LAFAwareColor();
	public static LAFAwareColor Color_0_0_225 = new LAFAwareColor();
	public static LAFAwareColor Color_0_0_245_60 = new LAFAwareColor();
	public static LAFAwareColor Color_0_0_255 = new LAFAwareColor();
	public static LAFAwareColor Color_0_0_255_80 = new LAFAwareColor();
	public static LAFAwareColor Color_0_0_62 = new LAFAwareColor();
	public static LAFAwareColor Color_0_0_77 = new LAFAwareColor();
	public static LAFAwareColor Color_0_100_0 = new LAFAwareColor();
	public static LAFAwareColor Color_0_100_200 = new LAFAwareColor();
	public static LAFAwareColor Color_0_100_255 = new LAFAwareColor();
	public static LAFAwareColor Color_0_102_0 = new LAFAwareColor();
	public static LAFAwareColor Color_0_105_0 = new LAFAwareColor();
	public static LAFAwareColor Color_0_112_0 = new LAFAwareColor();
	public static LAFAwareColor Color_0_130_0 = new LAFAwareColor();
	public static LAFAwareColor Color_0_176_0 = new LAFAwareColor();
	public static LAFAwareColor Color_0_200_0 = new LAFAwareColor();
	public static LAFAwareColor Color_0_200_255 = new LAFAwareColor();
	public static LAFAwareColor Color_0_230_0 = new LAFAwareColor();
	public static LAFAwareColor Color_0_255_0_50 = new LAFAwareColor();
	public static LAFAwareColor Color_0_255_230_70 = new LAFAwareColor();
	public static LAFAwareColor Color_0_255_255_150 = new LAFAwareColor();
	public static LAFAwareColor Color_0_30_255 = new LAFAwareColor();
	public static LAFAwareColor Color_0_40_255 = new LAFAwareColor();
	public static LAFAwareColor Color_0_40_90 = new LAFAwareColor();
	public static LAFAwareColor Color_0_55_0 = new LAFAwareColor();
	public static LAFAwareColor Color_0_80_160 = new LAFAwareColor();
	public static LAFAwareColor Color_0_80_200 = new LAFAwareColor();
	public static LAFAwareColor Color_0_80_255_80 = new LAFAwareColor();
	public static LAFAwareColor Color_0_96_0 = new LAFAwareColor();
	public static LAFAwareColor Color_100_0_0 = new LAFAwareColor();
	public static LAFAwareColor Color_100_100_100 = new LAFAwareColor();
	public static LAFAwareColor Color_100_110_210 = new LAFAwareColor();
	public static LAFAwareColor Color_115_0_0 = new LAFAwareColor();
	public static LAFAwareColor Color_115_217_255 = new LAFAwareColor();
	public static LAFAwareColor Color_122_210_255_200 = new LAFAwareColor();
	public static LAFAwareColor Color_128_128_128 = new LAFAwareColor();
	public static LAFAwareColor Color_130_130_130 = new LAFAwareColor();
	public static LAFAwareColor Color_137_176_212 = new LAFAwareColor();
	public static LAFAwareColor Color_140_0_0 = new LAFAwareColor();
	public static LAFAwareColor Color_140_158_255 = new LAFAwareColor();
	public static LAFAwareColor Color_141_16_16 = new LAFAwareColor();
	public static LAFAwareColor Color_145_50_0 = new LAFAwareColor();
	public static LAFAwareColor Color_150_0_0 = new LAFAwareColor();
	public static LAFAwareColor Color_150_0_100 = new LAFAwareColor();
	public static LAFAwareColor Color_150_150_150 = new LAFAwareColor();
	public static LAFAwareColor Color_150_255_0_70 = new LAFAwareColor();
	public static LAFAwareColor Color_153_153_153 = new LAFAwareColor();
	public static LAFAwareColor Color_155_0_0 = new LAFAwareColor();
	public static LAFAwareColor Color_160_130_100 = new LAFAwareColor();
	public static LAFAwareColor Color_160_200_255 = new LAFAwareColor();
	public static LAFAwareColor Color_160_80_0 = new LAFAwareColor();
	public static LAFAwareColor Color_170_0_0 = new LAFAwareColor();
	public static LAFAwareColor Color_170_200_0 = new LAFAwareColor();
	public static LAFAwareColor Color_180_160_0 = new LAFAwareColor();
	public static LAFAwareColor Color_180_255_220 = new LAFAwareColor();
	public static LAFAwareColor Color_190_210_255 = new LAFAwareColor();
	public static LAFAwareColor Color_190_255_180 = new LAFAwareColor();
	public static LAFAwareColor Color_196_236_255 = new LAFAwareColor();
	public static LAFAwareColor Color_1_0_0 = new LAFAwareColor();
	public static LAFAwareColor Color_1_75_1 = new LAFAwareColor();
	public static LAFAwareColor Color_200_100_200 = new LAFAwareColor();
	public static LAFAwareColor Color_200_200_0 = new LAFAwareColor();
	public static LAFAwareColor Color_200_200_200 = new LAFAwareColor();
	public static LAFAwareColor Color_200_200_200_140 = new LAFAwareColor();
	public static LAFAwareColor Color_200_200_255 = new LAFAwareColor();
	public static LAFAwareColor Color_200_255_180 = new LAFAwareColor();
	public static LAFAwareColor Color_204_204_204 = new LAFAwareColor();
	public static LAFAwareColor Color_204_255_178 = new LAFAwareColor();
	public static LAFAwareColor Color_205_255_205 = new LAFAwareColor();
	public static LAFAwareColor Color_208_245_255 = new LAFAwareColor();
	public static LAFAwareColor Color_220_20_20 = new LAFAwareColor();
	public static LAFAwareColor Color_220_220_220 = new LAFAwareColor();
	public static LAFAwareColor Color_220_220_255 = new LAFAwareColor();
	public static LAFAwareColor Color_220_225_255 = new LAFAwareColor();
	public static LAFAwareColor Color_220_255_220 = new LAFAwareColor();
	public static LAFAwareColor Color_220_255_220_70 = new LAFAwareColor();
	public static LAFAwareColor Color_224_224_224 = new LAFAwareColor();
	public static LAFAwareColor Color_224_240_255 = new LAFAwareColor();
	public static LAFAwareColor Color_228_228_232 = new LAFAwareColor();
	public static LAFAwareColor Color_228_238_255 = new LAFAwareColor();
	public static LAFAwareColor Color_230_230_230 = new LAFAwareColor();
	public static LAFAwareColor Color_230_80_50 = new LAFAwareColor();
	public static LAFAwareColor Color_232_232_255 = new LAFAwareColor();
	public static LAFAwareColor Color_235_235_255 = new LAFAwareColor();
	public static LAFAwareColor Color_238_255_238 = new LAFAwareColor();
	public static LAFAwareColor Color_240_240_255 = new LAFAwareColor();
	public static LAFAwareColor Color_240_255_255 = new LAFAwareColor();
	public static LAFAwareColor Color_242_242_242 = new LAFAwareColor();
	public static LAFAwareColor Color_242_242_255 = new LAFAwareColor();
	public static LAFAwareColor Color_245_0_0_60 = new LAFAwareColor();
	public static LAFAwareColor Color_245_90_60 = new LAFAwareColor();
	public static LAFAwareColor Color_247_247_247 = new LAFAwareColor();
	public static LAFAwareColor Color_248_252_255 = new LAFAwareColor();
	public static LAFAwareColor Color_248_255_248 = new LAFAwareColor();
	public static LAFAwareColor Color_250_250_255 = new LAFAwareColor();
	public static LAFAwareColor Color_254_255_255 = new LAFAwareColor();
	public static LAFAwareColor Color_255_0_0_0 = new LAFAwareColor();
	public static LAFAwareColor Color_255_0_0_10 = new LAFAwareColor();
	public static LAFAwareColor Color_255_0_0_120 = new LAFAwareColor();
	public static LAFAwareColor Color_255_0_0_150 = new LAFAwareColor();
	public static LAFAwareColor Color_255_0_0_20 = new LAFAwareColor();
	public static LAFAwareColor Color_255_0_0_50 = new LAFAwareColor();
	public static LAFAwareColor Color_255_0_0_60 = new LAFAwareColor();
	public static LAFAwareColor Color_255_0_51 = new LAFAwareColor();
	public static LAFAwareColor Color_255_100_100 = new LAFAwareColor();
	public static LAFAwareColor Color_255_150_140 = new LAFAwareColor();
	public static LAFAwareColor Color_255_153_152 = new LAFAwareColor();
	public static LAFAwareColor Color_255_200_200 = new LAFAwareColor();
	public static LAFAwareColor Color_255_205_205 = new LAFAwareColor();
	public static LAFAwareColor Color_255_206_206 = new LAFAwareColor();
	public static LAFAwareColor Color_255_210_180 = new LAFAwareColor();
	public static LAFAwareColor Color_255_210_210 = new LAFAwareColor();
	public static LAFAwareColor Color_255_220_220 = new LAFAwareColor();
	public static LAFAwareColor Color_255_230_200 = new LAFAwareColor();
	public static LAFAwareColor Color_255_230_220 = new LAFAwareColor();
	public static LAFAwareColor Color_255_230_230 = new LAFAwareColor();
	public static LAFAwareColor Color_255_236_236 = new LAFAwareColor();
	public static LAFAwareColor Color_255_240_240 = new LAFAwareColor();
	public static LAFAwareColor Color_255_242_240 = new LAFAwareColor();
	public static LAFAwareColor Color_255_243_218 = new LAFAwareColor();
	public static LAFAwareColor Color_255_246_206 = new LAFAwareColor();
	public static LAFAwareColor Color_255_249_200 = new LAFAwareColor();
	public static LAFAwareColor Color_255_250_215 = new LAFAwareColor();
	public static LAFAwareColor Color_255_255_0_128 = new LAFAwareColor();
	public static LAFAwareColor Color_255_255_170 = new LAFAwareColor();
	public static LAFAwareColor Color_255_255_176 = new LAFAwareColor();
	public static LAFAwareColor Color_255_255_204 = new LAFAwareColor();
	public static LAFAwareColor Color_255_255_205 = new LAFAwareColor();
	public static LAFAwareColor Color_255_255_210 = new LAFAwareColor();
	public static LAFAwareColor Color_255_255_220 = new LAFAwareColor();
	public static LAFAwareColor Color_255_255_230 = new LAFAwareColor();
	public static LAFAwareColor Color_255_255_236 = new LAFAwareColor();
	public static LAFAwareColor Color_255_255_238 = new LAFAwareColor();
	public static LAFAwareColor Color_255_255_240 = new LAFAwareColor();
	public static LAFAwareColor Color_255_255_250 = new LAFAwareColor();
	public static LAFAwareColor Color_255_255_255_150 = new LAFAwareColor();
	public static LAFAwareColor Color_255_255_255_200 = new LAFAwareColor();
	public static LAFAwareColor Color_255_255_255_70 = new LAFAwareColor();
	public static LAFAwareColor Color_255_40_0 = new LAFAwareColor();
	public static LAFAwareColor Color_255_50_50 = new LAFAwareColor();
	public static LAFAwareColor Color_255_80_80 = new LAFAwareColor();
	public static LAFAwareColor Color_66_118_187 = new LAFAwareColor();
	public static LAFAwareColor Color_70_255_70 = new LAFAwareColor();
	public static LAFAwareColor Color_80_200_255_200 = new LAFAwareColor();
	public static LAFAwareColor Color_86_82_125 = new LAFAwareColor();
	public static LAFAwareColor Color_95_0_0 = new LAFAwareColor();
	public static LAFAwareColor Color_96_64_0 = new LAFAwareColor();
	public static LAFAwareColor Color_0_255_255 = new LAFAwareColor();
	public static LAFAwareColor Color_64_64_64 = new LAFAwareColor();
	public static LAFAwareColor Color_0_255_0 = new LAFAwareColor();
	public static LAFAwareColor Color_192_192_192 = new LAFAwareColor();
	public static LAFAwareColor Color_255_0_0 = new LAFAwareColor();
	public static LAFAwareColor Color_255_255_255 = new LAFAwareColor();
	public static LAFAwareColor Color_255_255_0 = new LAFAwareColor();
	public static String HTMLColor_000000;
	public static String HTMLColor_0000B0;
	public static String HTMLColor_0000D0;
	public static String HTMLColor_0000cc;
	public static String HTMLColor_0000dd;
	public static String HTMLColor_0000ff;
	public static String HTMLColor_005500;
	public static String HTMLColor_006000;
	public static String HTMLColor_006600;
	public static String HTMLColor_0066ff;
	public static String HTMLColor_008000;
	public static String HTMLColor_050aff;
	public static String HTMLColor_663300;
	public static String HTMLColor_707080;
	public static String HTMLColor_808080;
	public static String HTMLColor_888888;
	public static String HTMLColor_dd0000;
	public static String HTMLColor_dd8888;
	public static String HTMLColor_eeeeff;
	public static String HTMLColor_eeffee;
	public static String HTMLColor_ff0000;
	public static String HTMLColor_ff2222;
	public static String HTMLColor_ff9999;
	
	
	private static boolean timerIsInitialized = false;
	
	public static void init() {
		if (!timerIsInitialized) {
			if ("true".equals(System.getProperty("darkLAF"))) {
				Timer timer = new Timer(1000, e -> init());
				timer.setInitialDelay(1000);
				timer.setRepeats(true);
				timer.start();
				
				timer = new Timer(10, e -> {
					boolean repaint = false;
					long t = System.currentTimeMillis() % 5000;
					for (LAFAwareColor lc: colors) {
						if (lc.blink) {
							Color newColor;
							if (t > 200) {
								newColor = lc.origColor;
							} else if (t > 100) {
								newColor = Color.black;
							} else {
								newColor = Color.white;
							}
							if (!newColor.equals(lc.color)) {
								lc.color = newColor;
								repaint = true;
							}
						}
						if (repaint) {
							for (Window w : Window.getWindows()) {
								w.repaint();
							}
						}
					}
				});
				timer.setInitialDelay(10);
				timer.setRepeats(true);
				timer.start();
			}
			timerIsInitialized = true;
		}
		
		initColors();
		for (Window w : Window.getWindows()) {
			w.repaint();
		}
	}
	
	private static List<LAFAwareColor> colors = new ArrayList<>();
	
	@SuppressWarnings("serial")
	public static class LAFAwareColor extends Color {
		
		private Color color;
		private Color origColor;
		private Color dark;
		private boolean blink;

		public LAFAwareColor init(Color light, Color dark) {
			this.color = UIUtil.plaf == PLAF.FLATDARK? dark : light;
			this.origColor = color;
			this.dark = dark;
			this.blink = false;
			if (dark == null) {
				throw new NullPointerException();
			}
			colors.add(this);
			return this;
		}
		
		public LAFAwareColor init(Color light) {
			init(light, light);
			return this;
		}
		
		public LAFAwareColor() {
			super(0);
		}
		
		public void blink() {
			blink = true;
		}

		@Override
		public int getRed() {
			return color.getRed();
		}

		@Override
		public int getGreen() {
			return color.getGreen();
		}

		@Override
		public int getBlue() {
			return color.getBlue();
		}

		@Override
		public int getAlpha() {
			return color.getAlpha();
		}

		@Override
		public int getRGB() {
			return color.getRGB();
		}

		@Override
		public Color brighter() {
			return color.brighter();
		}

		@Override
		public Color darker() {
			return color.darker();
		}

		@Override
		public int hashCode() {
			return color.hashCode();
		}

		@Override
		public boolean equals(Object obj) {
			return color.equals(obj);
		}

		@Override
		public String toString() {
			return color.toString();
		}

		@Override
		public float[] getRGBComponents(float[] compArray) {
			return color.getRGBComponents(compArray);
		}

		@Override
		public float[] getRGBColorComponents(float[] compArray) {
			return color.getRGBColorComponents(compArray);
		}

		@Override
		public float[] getComponents(float[] compArray) {
			return color.getComponents(compArray);
		}

		@Override
		public float[] getColorComponents(float[] compArray) {
			return color.getColorComponents(compArray);
		}

		@Override
		public float[] getComponents(ColorSpace cspace, float[] compArray) {
			return color.getComponents(cspace, compArray);
		}

		@Override
		public float[] getColorComponents(ColorSpace cspace, float[] compArray) {
			return color.getColorComponents(cspace, compArray);
		}

		@Override
		public ColorSpace getColorSpace() {
			return color.getColorSpace();
		}

		@Override
		public synchronized PaintContext createContext(ColorModel cm, Rectangle r, Rectangle2D r2d,
				AffineTransform xform, RenderingHints hints) {
			return color.createContext(cm, r, r2d, xform, hints);
		}

		@Override
		public int getTransparency() {
			return color.getTransparency();
		}
	}
}


// TODO
// TODO frame-color in darkFlaf for JScrollView(?) etc. is too bright
