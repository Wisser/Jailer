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
package net.sf.jailer.ui.graphical_view;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Line2D;
import java.awt.geom.Path2D;
import java.awt.geom.Rectangle2D;

import javax.swing.Icon;
import javax.swing.JLabel;

/**
 * Painted icon showing an arrangement of tables, for the {@link LayoutActions}.
 * Tables are drawn as small boxes, associations as lines. Being painted, it stays sharp at any scaling.
 *
 * @author Ralf Wisser
 */
public class LayoutIcon implements Icon {

	/**
	 * The arrangements.
	 */
	public enum Kind {
		/** tree from left to right */
		TREE,
		/** tree from top to bottom */
		TREE_TOP_DOWN,
		/** rings around a center */
		RADIAL,
		/** circle */
		CIRCLE,
		/** columns, parents left */
		LAYERS,
		/** overlapping tables pushed apart */
		SEPARATE,
		/** tables moved closer together */
		COMPACT,
		/** tables moved further apart */
		SPREAD
	}

	private static final Color BOX_FILL = new Color(200, 212, 255);
	private static final Color BOX_LINE = new Color(60, 80, 220);
	private static final Color EDGE = new Color(40, 140, 40);
	private static final Color RING = new Color(60, 80, 220, 110);
	private static final Color DISABLED_FILL = new Color(225, 225, 225);
	private static final Color DISABLED_LINE = new Color(150, 150, 150);

	private final Kind kind;
	private final int size;

	/**
	 * Creates an icon as high as the default font, like the other menu icons.
	 *
	 * @param kind the arrangement to show
	 */
	public LayoutIcon(Kind kind) {
		JLabel label = new JLabel("M");
		this.kind = kind;
		this.size = label.getFontMetrics(label.getFont()).getHeight();
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
			new Painter(g, size, c == null || c.isEnabled()).paint(kind);
		} finally {
			g.dispose();
		}
	}

	/**
	 * Paints in unit coordinates (0..1), scaled to the icon size.
	 */
	private static class Painter {
		private final Graphics2D g;
		private final double s;
		private final boolean enabled;
		private final BasicStroke stroke;

		Painter(Graphics2D g, int size, boolean enabled) {
			this.g = g;
			this.s = size;
			this.enabled = enabled;
			this.stroke = new BasicStroke((float) Math.max(1.0, size / 20.0));
		}

		void paint(Kind kind) {
			switch (kind) {
			case TREE:
				fan(0.2, 0.5, new double[][] { { 0.78, 0.18 }, { 0.78, 0.5 }, { 0.78, 0.82 } });
				break;
			case TREE_TOP_DOWN:
				fan(0.5, 0.2, new double[][] { { 0.18, 0.78 }, { 0.5, 0.78 }, { 0.82, 0.78 } });
				break;
			case RADIAL: {
				double r = 0.36;
				g.setStroke(stroke);
				g.setColor(enabled? RING : DISABLED_FILL);
				g.draw(new Ellipse2D.Double((0.5 - r) * s, (0.5 - r) * s, 2 * r * s, 2 * r * s));
				double[][] outer = onCircle(5, r, -Math.PI / 2);
				for (double[] p: outer) {
					edge(0.5, 0.5, p[0], p[1]);
				}
				smallBox(0.5, 0.5);
				for (double[] p: outer) {
					smallBox(p[0], p[1]);
				}
				break;
			}
			case CIRCLE: {
				double[][] points = onCircle(6, 0.35, -Math.PI / 2);
				for (int i = 0; i < points.length; ++i) {
					double[] a = points[i];
					double[] b = points[(i + 1) % points.length];
					edge(a[0], a[1], b[0], b[1]);
				}
				for (double[] p: points) {
					smallBox(p[0], p[1]);
				}
				break;
			}
			case LAYERS: {
				double[][] col1 = { { 0.14, 0.5 } };
				double[][] col2 = { { 0.5, 0.3 }, { 0.5, 0.7 } };
				double[][] col3 = { { 0.86, 0.16 }, { 0.86, 0.5 }, { 0.86, 0.84 } };
				edge(col1[0], col2[0]);
				edge(col1[0], col2[1]);
				edge(col2[0], col3[0]);
				edge(col2[0], col3[1]);
				edge(col2[1], col3[2]);
				for (double[][] col: new double[][][] { col1, col2, col3 }) {
					for (double[] p: col) {
						box(p[0], p[1]);
					}
				}
				break;
			}
			case SEPARATE:
				box(0.3, 0.38, 0.36, 0.3);
				box(0.7, 0.62, 0.36, 0.3);
				arrow(0.3, 0.84, 0.04, 0.84);
				arrow(0.7, 0.16, 0.96, 0.16);
				break;
			case COMPACT:
				// boxes in the corners, arrows to the middle
				for (double[] p: new double[][] { { 0.16, 0.14 }, { 0.84, 0.14 }, { 0.16, 0.86 }, { 0.84, 0.86 } }) {
					smallBox(p[0], p[1]);
					arrow(p[0] + (0.5 - p[0]) * 0.35, p[1] + (0.5 - p[1]) * 0.35, p[0] + (0.5 - p[0]) * 0.8, p[1] + (0.5 - p[1]) * 0.8);
				}
				break;
			case SPREAD:
				// box in the middle, arrows to the corners
				smallBox(0.5, 0.5);
				for (double[] p: new double[][] { { 0.06, 0.06 }, { 0.94, 0.06 }, { 0.06, 0.94 }, { 0.94, 0.94 } }) {
					arrow(0.5 + (p[0] - 0.5) * 0.4, 0.5 + (p[1] - 0.5) * 0.4, p[0], p[1]);
				}
				break;
			}
		}

		private void fan(double rx, double ry, double[][] children) {
			for (double[] c: children) {
				edge(rx, ry, c[0], c[1]);
			}
			box(rx, ry);
			for (double[] c: children) {
				box(c[0], c[1]);
			}
		}

		private double[][] onCircle(int n, double r, double start) {
			double[][] points = new double[n][];
			for (int i = 0; i < n; ++i) {
				double a = start + 2 * Math.PI * i / n;
				points[i] = new double[] { 0.5 + r * Math.cos(a), 0.5 + r * Math.sin(a) };
			}
			return points;
		}

		private void box(double cx, double cy) {
			box(cx, cy, 0.26, 0.2);
		}

		/**
		 * Box for the round arrangements, where the boxes are closer together.
		 */
		private void smallBox(double cx, double cy) {
			box(cx, cy, 0.2, 0.15);
		}

		private void box(double cx, double cy, double w, double h) {
			Rectangle2D rect = new Rectangle2D.Double((cx - w / 2) * s, (cy - h / 2) * s, w * s, h * s);
			g.setColor(enabled? BOX_FILL : DISABLED_FILL);
			g.fill(rect);
			g.setStroke(stroke);
			g.setColor(enabled? BOX_LINE : DISABLED_LINE);
			g.draw(rect);
		}

		private void edge(double[] a, double[] b) {
			edge(a[0], a[1], b[0], b[1]);
		}

		private void edge(double x1, double y1, double x2, double y2) {
			g.setStroke(stroke);
			g.setColor(enabled? EDGE : DISABLED_LINE);
			g.draw(new Line2D.Double(x1 * s, y1 * s, x2 * s, y2 * s));
		}

		private void arrow(double x1, double y1, double x2, double y2) {
			edge(x1, y1, x2, y2);
			double a = Math.atan2(y2 - y1, x2 - x1);
			double len = 0.14;
			Path2D head = new Path2D.Double();
			head.moveTo(x2 * s, y2 * s);
			head.lineTo((x2 - len * Math.cos(a - 0.6)) * s, (y2 - len * Math.sin(a - 0.6)) * s);
			head.lineTo((x2 - len * Math.cos(a + 0.6)) * s, (y2 - len * Math.sin(a + 0.6)) * s);
			head.closePath();
			g.setColor(enabled? EDGE : DISABLED_LINE);
			g.fill(head);
		}
	}

}
