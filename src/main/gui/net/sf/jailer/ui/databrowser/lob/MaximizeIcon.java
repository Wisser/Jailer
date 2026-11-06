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
package net.sf.jailer.ui.databrowser.lob;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Area;
import java.awt.geom.Rectangle2D;

import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.UIManager;

/**
 * Painted "Maximize" / "Restore" icon: one window, or two overlapping ones.
 * <p>
 * The symbol is centered in a square as high as the font, so that it lines up with a button text
 * (the bitmaps used elsewhere have their symbol in the upper part of the image). It is painted in
 * the foreground color of the component, which fits the light and the dark theme alike, and stays
 * sharp at any scaling.
 *
 * @author Ralf Wisser
 */
public class MaximizeIcon implements Icon {

	/**
	 * The symbols.
	 */
	public enum Kind {
		/** a single window */
		MAXIMIZE,
		/** two overlapping windows */
		RESTORE
	}

	/** Margin around the symbol, relative to the size. */
	private static final double MARGIN = 0.18;
	/** Height of a title bar, relative to the height of its window. */
	private static final double TITLE_BAR = 0.24;
	/** Offset of the two windows of {@link Kind#RESTORE}, relative to the size. */
	private static final double OFFSET = 0.17;

	private final Kind kind;
	private final int size;

	/**
	 * Creates an icon as high as the default font.
	 *
	 * @param kind the symbol
	 */
	public MaximizeIcon(Kind kind) {
		this(kind, fontHeight());
	}

	/**
	 * Creates an icon of a given size.
	 *
	 * @param kind the symbol
	 * @param size width and height in pixels
	 */
	public MaximizeIcon(Kind kind, int size) {
		this.kind = kind;
		this.size = size;
	}

	private static int fontHeight() {
		JLabel label = new JLabel("M");
		return label.getFontMetrics(label.getFont()).getHeight();
	}

	@Override
	public int getIconWidth() {
		return size;
	}

	@Override
	public int getIconHeight() {
		return size;
	}

	@Override
	public void paintIcon(Component c, Graphics graphics, int x, int y) {
		Graphics2D g = (Graphics2D) graphics.create();
		try {
			g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
			g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);
			g.translate(x, y);
			g.setColor(color(c));
			int lineWidth = Math.max(1, Math.round(size / 12f));
			g.setStroke(new BasicStroke(lineWidth));

			// the outer box of the symbol, whole pixels, so that the lines stay sharp
			int m = (int) Math.round(size * MARGIN);
			int box = size - 2 * m;
			if (kind == Kind.MAXIMIZE) {
				window(g, m, m, box, lineWidth);
			} else {
				int offset = (int) Math.round(size * OFFSET);
				int w = box - offset;
				// the window behind, top right; only where the front window does not cover it
				Area behind = new Area(new Rectangle2D.Double(0, 0, size, size));
				behind.subtract(new Area(new Rectangle2D.Double(m - lineWidth, m + offset - lineWidth,
						w + 2 * lineWidth, w + 2 * lineWidth)));
				Graphics2D gb = (Graphics2D) g.create();
				try {
					gb.clip(behind);
					window(gb, m + offset, m, w, lineWidth);
				} finally {
					gb.dispose();
				}
				// the window in front, bottom left
				window(g, m, m + offset, w, lineWidth);
			}
		} finally {
			g.dispose();
		}
	}

	/**
	 * Paints a window: an outline with a filled title bar.
	 */
	private static void window(Graphics2D g, int x, int y, int w, int lineWidth) {
		double h = lineWidth / 2.0;
		g.draw(new Rectangle2D.Double(x + h, y + h, w - lineWidth, w - lineWidth));
		int bar = Math.max(lineWidth + 1, (int) Math.round(w * TITLE_BAR));
		g.fill(new Rectangle2D.Double(x, y, w, bar));
	}

	private static Color color(Component c) {
		if (c != null && !c.isEnabled()) {
			Color disabled = UIManager.getColor("Label.disabledForeground");
			return disabled != null ? disabled : Color.GRAY;
		}
		if (c != null && c.getForeground() != null) {
			return c.getForeground();
		}
		Color fg = UIManager.getColor("Label.foreground");
		return fg != null ? fg : Color.BLACK;
	}
}
