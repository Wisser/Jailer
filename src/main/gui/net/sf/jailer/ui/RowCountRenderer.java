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
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.Toolkit;
import java.util.Map;

import javax.swing.JComponent;
import javax.swing.JLabel;

/**
 * Renders row counts.
 *
 * @author Ralf Wisser
 */
public class RowCountRenderer extends JComponent {
	
	private static final Color FG_COLOR = Colors.Color_0_80_200;
	
	private static JLabel lk;
	private static JLabel lm;
	private static JLabel lg;
	private static int w;
	private static Font font;
	private static boolean initialized = false;
	
	private JLabel suffix;
	private String count;
	private JLabel cl;
	private int clw;
	private int h;
	private int sep = 2;
	
	public static final String k = "k";
	public static final String m = "m";
	public static final String g = "g";

	public RowCountRenderer(String count, String suffix) {
		if (!initialized) {
			initialized = true;
			
			lk = new JLabel("k");
			lm = new JLabel("m");
			lg = new JLabel("g");
			
			font = lk.getFont().deriveFont(lk.getFont().getStyle() & ~Font.BOLD);
			
			lk.setSize(lk.getPreferredSize());
			lm.setSize(lm.getPreferredSize());
			lg.setSize(lg.getPreferredSize());
			w = Math.max(Math.max(lk.getPreferredSize().width, lm.getPreferredSize().width), lg.getPreferredSize().width);
			
			lk.setForeground(FG_COLOR);
			lm.setForeground(Colors.Color_96_64_0);
			lg.setForeground(Colors.Color_150_0_100);
			
			lk.setFont(font);
			lm.setFont(font);
			lg.setFont(font);
		}
		
		this.count = count;
		if (suffix == null) {
			this.suffix = null;
		} else if ("k".equals(suffix)) {
			this.suffix = lk;
		} else if ("m".equals(suffix)) {
			this.suffix = lm;
		} else if ("g".equals(suffix)) {
			this.suffix = lg;
		} else {
			throw new RuntimeException("unknown suffix: " + suffix);
		}

		cl = new JLabel(count);
		clw = cl.getPreferredSize().width;
		h = cl.getPreferredSize().height;
		
		setSize(clw + sep + w, h);
		setPreferredSize(getSize());
		setMaximumSize(getSize());
		setMinimumSize(getSize());
		setForeground(FG_COLOR);
	}
	
	@Override
	public void paint(Graphics g) {
		super.paint(g);
		doPaint(g);
	}

	public void doPaint(Graphics g) {
		int oy = UIUtil.plaf.isFlat ? 2 : 3;
		if (g instanceof Graphics2D) {
			Toolkit toolkit = Toolkit.getDefaultToolkit();
			Map map = (Map) (toolkit.getDesktopProperty("awt.font.desktophints"));

			if (map != null) {
				((Graphics2D) g).addRenderingHints(map);
			} else
				((Graphics2D) g).setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING,
						RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
		}
		g.setFont(font);
		g.setColor(this.getForeground());
		g.drawString(count, 0, h - oy);

		if (suffix != null) {
			g.setColor(suffix.getForeground());
			g.drawString(suffix.getText(), clw + sep, h - oy);
		}
	}
	
}
