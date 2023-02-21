/*
 * Copyright 2007 - 2023 Ralf Wisser.
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

import net.sf.jailer.ui.UIUtil.PLAF;

/**
 * Some utility methods.
 *
 * @author Ralf Wisser
 */
public class RowCountRenderingHelper {

	public final JLabel k = new JLabel("k");
	public final JLabel m = new JLabel("m");
	public final JLabel g = new JLabel("g");
	
	private int w;

	private final Font font = k.getFont().deriveFont(k.getFont().getStyle() & ~Font.BOLD);
	
	public RowCountRenderingHelper() {
		k.setSize(k.getPreferredSize());
		m.setSize(m.getPreferredSize());
		g.setSize(g.getPreferredSize());
		w = Math.max(Math.max(k.getPreferredSize().width, m.getPreferredSize().width), g.getPreferredSize().width);
		
		k.setForeground(new Color(0, 9 * 16 + 9, 0));
		m.setForeground(new Color(6 * 16 + 0, 4 * 16, 0));
		g.setForeground(new Color(9 * 16 + 6, 0, 6 * 16 + 4));
		
		k.setFont(font);
		m.setFont(font);
		g.setFont(font);
	}
	
	public JComponent createRowCountRender(String count, JLabel suffix) {
		JLabel cl = new JLabel(count);
		int clw = cl.getPreferredSize().width;
		int h = cl.getPreferredSize().height;
		int sep = 2;
		JComponent render = new JComponent() {
			@Override
			public void paint(Graphics g) {
				super.paint(g);
				
				int oy = PLAF.FLAT == UIUtil.plaf? 2 : 3;
				if (g instanceof Graphics2D) {
					Toolkit toolkit = Toolkit.getDefaultToolkit();
					Map map = (Map)(toolkit.getDesktopProperty("awt.font.desktophints"));

					if (map != null)
					{
						((Graphics2D) g).addRenderingHints(map);
					}
					else
						((Graphics2D) g).setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING,
							RenderingHints.VALUE_TEXT_ANTIALIAS_ON );
				}
				g.setFont(font);
				g.setColor(this.getForeground());
				g.drawString(count, 0, h - oy);
					
				if (suffix != null) {
					g.setColor(suffix.getForeground());
					g.drawString(suffix.getText(), clw + sep, h - oy);
				}
			}
		};
		render.setSize(clw + sep + w, h);
		render.setPreferredSize(render.getSize());
		render.setMaximumSize(render.getSize());
		render.setMinimumSize(render.getSize());
		render.setForeground(new Color(0, 80, 200));
		
		return render;
	}
	
	// TODO
	// TODO re-read rowcount heuristically after DML? (ins/upd/del)?
	// TODO re-read rowcount of 0 or < 1000(?) after reading stats-info-query-result?

}
