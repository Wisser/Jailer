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
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;

import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JLabel;

import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.UIUtil.PLAF;

/**
 * Icon of the pivot table: a small cross table with header row and column and a total row.
 * Drawn, so that it scales and has colors for the light and the dark theme.
 *
 * @author Ralf Wisser
 */
public class PivotIcon implements Icon {

	private final int size;

	/**
	 * Constructor.
	 *
	 * @param size width and height in pixels
	 */
	public PivotIcon(int size) {
		this.size = Math.max(8, size);
	}

	/**
	 * Creates the icon for a menu item or button, as high as a line of text (like the other icons scaled by {@link UIUtil#scaleIcon(JComponent, javax.swing.ImageIcon)}).
	 */
	public static PivotIcon forMenu(JComponent component) {
		return new PivotIcon(component.getFontMetrics(new JLabel("M").getFont()).getHeight());
	}

	/**
	 * Gets the icon as image (e.g. for a window).
	 *
	 * @param size width and height in pixels
	 */
	public static Image image(int size) {
		PivotIcon icon = new PivotIcon(size);
		BufferedImage image = new BufferedImage(icon.size, icon.size, BufferedImage.TYPE_INT_ARGB);
		Graphics g = image.getGraphics();
		icon.paintIcon(null, g, 0, 0);
		g.dispose();
		return image;
	}

	@Override
	public void paintIcon(Component c, Graphics g, int x, int y) {
		boolean dark = UIUtil.plaf == PLAF.FLATDARK;
		Color header = dark ? new Color(90, 150, 220) : new Color(70, 130, 200);
		Color cell = dark ? new Color(60, 63, 65) : Color.WHITE;
		Color total = dark ? new Color(210, 90, 20) : new Color(255, 160, 70);
		Color grid = dark ? new Color(150, 150, 150) : new Color(110, 110, 110);

		Graphics2D g2d = (Graphics2D) g.create();
		try {
			g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
			int margin = Math.max(1, size / 10);
			int n = 3; // cells per row and column
			int cellSize = (size - 2 * margin) / n;
			int x0 = x + (size - cellSize * n) / 2;
			int y0 = y + (size - cellSize * n) / 2;
			for (int row = 0; row < n; row++) {
				for (int column = 0; column < n; column++) {
					Color color = row == 0 || column == 0 ? header : row == n - 1 ? total : cell;
					g2d.setColor(color);
					g2d.fillRect(x0 + column * cellSize, y0 + row * cellSize, cellSize, cellSize);
				}
			}
			g2d.setColor(grid);
			for (int i = 0; i <= n; i++) {
				g2d.drawLine(x0 + i * cellSize, y0, x0 + i * cellSize, y0 + n * cellSize);
				g2d.drawLine(x0, y0 + i * cellSize, x0 + n * cellSize, y0 + i * cellSize);
			}
		} finally {
			g2d.dispose();
		}
	}

	@Override
	public int getIconWidth() {
		return size;
	}

	@Override
	public int getIconHeight() {
		return size;
	}

}
