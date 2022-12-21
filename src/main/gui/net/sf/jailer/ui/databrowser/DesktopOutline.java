/*
 * Copyright 2007 - 2022 Ralf Wisser.
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
package net.sf.jailer.ui.databrowser;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.geom.Rectangle2D;
import java.beans.PropertyVetoException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;

import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;

import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.UIUtil.PLAF;
import net.sf.jailer.ui.databrowser.Desktop.RowBrowser;

/**
 * Desktop outline.
 * 
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public class DesktopOutline extends JPanel {

	private final JComponent sameWidthFriend;
	private final JPanel controlPanel;
	private final Desktop desktop;
	private final JScrollPane scrollPane;
	private Point draggingStart = null;
	private Point draggingViewPosition = null;
	public Rectangle visibleRectInOutline = null;
	
	public DesktopOutline(JComponent sameWidthFriend, JPanel controlPanel, JScrollPane scrollPane, Desktop desktop, Consumer<RowBrowser> doubleClickAction) {
		this.sameWidthFriend = sameWidthFriend;
		this.controlPanel = controlPanel;
		this.scrollPane = scrollPane;
		this.desktop = desktop;
		setOpaque(false);
		addMouseMotionListener(new MouseMotionListener() {
			
			@Override
			public void mouseMoved(MouseEvent e) {
				stopDragging();
				RowBrowser browser = findBrowser(e);
				if (browser == null) {
					setToolTipText(null);
				} else {
					setToolTipText(browser.internalFrame.getTitle());
				}
			}
			
			@Override
			public void mouseDragged(MouseEvent e) {
				if (draggingStart == null) {
					startDragging(e);
				}
				setDragViewPosition(scrollPane, desktop, e);
			}
		});
		
		addMouseListener(new MouseListener() {
			@Override
			public void mouseReleased(MouseEvent e) {
				stopDragging();
				RowBrowser browser = findBrowser(e);
				if (e.getButton() == MouseEvent.BUTTON3) {
					if (browser != null) {
							try {
							browser.internalFrame.setSelected(true);
						} catch (PropertyVetoException e1) {
							// ignore
						}
					}
				    showPopupMenu(desktop, e, browser);
				}
			}
			
			@Override
			public void mousePressed(MouseEvent e) {
				RowBrowser browser = findBrowser(e);
				if (e.getButton() != MouseEvent.BUTTON3 || browser != null) {
					centeredDragging(e);
				}
				stopDragging();
			}
			
			@Override
			public void mouseExited(MouseEvent e) {
			}
			
			@Override
			public void mouseEntered(MouseEvent e) {
			}
			
			@Override
			public void mouseClicked(MouseEvent e) {
				RowBrowser browser = findBrowser(e);
				if (e.getButton() != MouseEvent.BUTTON3 || browser != null) {
					centeredDragging(e);
				}
				stopDragging();
				if (e.getButton() == MouseEvent.BUTTON1 || e.getButton() == MouseEvent.BUTTON3) {
					if (browser != null) {
						try {
							browser.internalFrame.setSelected(true);
						} catch (PropertyVetoException e1) {
							// ignore
						}
					}
					if (e.getButton() == MouseEvent.BUTTON3) {
	                    showPopupMenu(desktop, e, browser);
					} else if (e.getClickCount() > 1 && browser != null) {
						doubleClickAction.accept(browser);
					}
				}
			}

			private void showPopupMenu(Desktop desktop, MouseEvent e, RowBrowser browser) {
				if (browser == null) {
					desktop.openGlobalPopup(e);
					return;
				}
				JPopupMenu popup = browser.browserContentPane.createPopupMenu(null, -1, 0, 0, false, false, false);
				if (popup != null) {
				    JPopupMenu popup2 = browser.browserContentPane.createSqlPopupMenu(-1, 0, 0, true, DesktopOutline.this);
				    if (popup2.getComponentCount() > 0 && popup.getComponentCount() > 0) {
				         popup.add(new JSeparator());
				    }
				    for (Component c : popup2.getComponents()) {
				        popup.add(c);
				    }
				    UIUtil.fit(popup);
				    UIUtil.showPopup(e.getComponent(), e.getX(), e.getY(), popup);
				}
			}
		});
		
		addMouseWheelListener(new MouseWheelListener() {
			@Override
			public void mouseWheelMoved(MouseWheelEvent e) {
				if (scale >= 0.0) {
					long currentTime = System.currentTimeMillis();
					int x = (int) (((e.getX() - offX) / scale) + 0.5);
					int y = (int) (((e.getY() - offY) / scale) + 0.5);
					desktop.startRescaleMode(currentTime, x, y, desktop);
					desktop.onMouseWheelMoved(e, scrollPane, currentTime);
					desktop.onMouseWheelMoved(x, y, e.getWheelRotation(), desktop, currentTime, e);
				}
			}
		});
	}
	
	private void startDragging(MouseEvent e) {
		draggingStart = e.getPoint();
		draggingViewPosition = scrollPane.getViewport().getViewPosition();
		setCursor(Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR));
	}

	private void stopDragging() {
		draggingStart = null;
		draggingViewPosition = null;
		setCursor(null);
	}
	
	private void centeredDragging(MouseEvent e) {
		if (visibleRectInOutline != null) {
			double wb = Math.min(visibleRectInOutline.width / 2 - 2, Desktop.BROWSERTABLE_DEFAULT_WIDTH * desktop.layoutMode.factor * scale / 3);
			double wh = Math.min(visibleRectInOutline.height / 2 - 2, Desktop.BROWSERTABLE_DEFAULT_HEIGHT * desktop.layoutMode.factor * scale / 3);
			Rectangle r = new Rectangle(visibleRectInOutline.x + (int) wb, visibleRectInOutline.y + (int) wh, visibleRectInOutline.width - (int) (2 * wb), visibleRectInOutline.height - (int) (2 * wh));
			if (draggingStart == null && !r.contains(e.getPoint())) {
				startDragging(e);
				draggingStart = new Point((int) r.getCenterX(), (int) r.getCenterY());
				setDragViewPosition(scrollPane, desktop, e);
				stopDragging();
			}
		}
	}

	private double scale;
	private double offX;
	private double offY;

	@Override
	public void paint(Graphics g) {
		super.paint(g);
		if (g instanceof Graphics2D) {
			if (desktop.getWidth() == 0 || desktop.getHeight() == 0) {
				return;
			}
			Graphics2D g2d = (Graphics2D) g;
			FontMetrics fontMetrics = g2d.getFontMetrics();
			g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
			g2d.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
			double r = 6;
			scale = Math.min(((double) sameWidthFriend.getWidth() - r * 2.0) / (double) desktop.getWidth(), ((double) (getHeight() - r * 2.0) / (double) desktop.getHeight()));
			offX = r - 3;
			offY = r - 1; // Math.max(0, (outLinePanel.getHeight() - outlineSize.getHeight()) / 2);
			BasicStroke stroke = new BasicStroke();
			double border = 4 / scale;
			double x = -border;
			double y = -border;
			double width = desktop.getWidth() + 2 * border;
			double height = desktop.getHeight() + 2 * border;
			Color backgroundColor = new Color(232, 232, 255);
			g2d.setColor(backgroundColor);
			int gx = (int) (offX + scale * x + 0.5);
			int gy = (int)(offY + scale * y + 0.5);
			int gw = snap((int) (sameWidthFriend.getWidth() - (offX + scale * x + 0.5) - 1), (int)(scale * width + 0.5), 32);
			int gh = (int)(scale * height + 0.5);
			GradientPaint paint = new GradientPaint(
					0, 0, backgroundColor.brighter(),
					gw, gh, backgroundColor);
			g2d.setPaint(paint);
			g2d.fillRoundRect(gx, gy, gw, gh, 2, 2);
			Rectangle rectangle = desktop.getVisibleRect();
			int sx = (int)(offX + scale * (double) rectangle.x + 0.5);
			int sy = (int)(offY + scale * (double) rectangle.y + 0.5);
			int sw = (int)(scale * (double) (rectangle.width) + 0.5);
			int sh = (int)(scale * (double) rectangle.height + 0.5);
			visibleRectInOutline = new Rectangle(sx, sy, sw, sh);
			Color inDesktopColor = new Color(242, 242, 255);
			g2d.setColor(inDesktopColor);
			paint = new GradientPaint(
						0, 0, new Color(255, 255, 255),
						gw, gh, inDesktopColor);
			g2d.setPaint(paint);
			g2d.setStroke(stroke);
			g2d.fillRoundRect(sx, sy, sw, sh, 8, 8);
			g2d.setColor(UIUtil.plaf == PLAF.FLAT? UIUtil.FLAT_BORDER_COLOR : Color.LIGHT_GRAY);
			g2d.drawRoundRect(gx, gy, gw + 1, gh, 2, 2);
			
			g2d.setStroke(new BasicStroke(1));
			for (RowBrowser browser: getBrowsers()) {
				if (!browser.isHidden()) {
					RowBrowser parentBrowser = browser.parent;
					boolean hiddenParent = false;
					while (parentBrowser != null && parentBrowser.isHidden()) {
						parentBrowser = parentBrowser.parent;
						hiddenParent = true;
					}
					if (parentBrowser != null && !parentBrowser.isHidden()) {
						if (browser.association == null) {
							g2d.setColor(Color.GRAY);
						} else if (hiddenParent) {
							g2d.setColor(Color.yellow.darker());
						} else {
							g2d.setColor(desktop.getAssociationColor1(browser.association));
						}
						g2d.drawLine((int) (offX + scale * browser.internalFrame.getBounds().getCenterX() + 0.5), (int)(offY + scale * browser.internalFrame.getBounds().getCenterY() + 0.5), (int) (offX + scale * parentBrowser.internalFrame.getBounds().getCenterX() + 0.5), (int)(offY + scale * parentBrowser.internalFrame.getBounds().getCenterY() + 0.5));
					}
				}
			}
			Set<RowBrowser> onPath = new HashSet<RowBrowser>();
			calcOnPath(onPath, getBrowsers());
			for (RowBrowser browser: getBrowsers()) {
				if (!browser.isHidden()) {
					rectangle = subBorder(browser.internalFrame.getBounds());
					Color backgroundColor1 = new Color(160, 200, 255);
					if (onPath.contains(browser)) {
						backgroundColor1 = new Color(180, 255, 220);
					}
					sx = (int)(offX + scale * (double) rectangle.x + 0.5);
					sy = (int)(offY + scale * (double) rectangle.y + 0.5);
					sw = (int)(scale * (double) rectangle.width + 0.5);
					sh = (int)(scale * (double) rectangle.height + 0.5);
					if (backgroundColor1 != null) {
						g2d.setColor(backgroundColor1);
						paint = new GradientPaint(
								sx, sy, backgroundColor1.brighter(),
								sx + sw, sy + sh, backgroundColor1);
						g2d.setPaint(paint);
						g2d.fillRoundRect(sx, sy, sw, sh, 8, 8);
					}
					g2d.setColor(Color.black);
					Shape clip = g2d.getClip();
					g2d.clipRect(sx, sy, sw, sh);
					String title = browser.internalFrame.getTitle();
					Rectangle2D stringBounds = fontMetrics.getStringBounds(title, g2d);
					double hf = 1.2;
					if ((stringBounds.getHeight() * hf * hf + 0.5) >= sh - 2) {
						hf = 1.0;
					}
					if (stringBounds.getHeight() / 2 < sh - 1) {
						int linesAvailable = (int) (sh / Math.max(1, stringBounds.getHeight()) + 0.5);
						if (linesAvailable == 2) {
							linesAvailable = (int) (sh / Math.max(1, stringBounds.getHeight()));
						}
						int linesNeeded = 1;
						final int sb = 4;
						int maxWidth = sw - 2 * sb;
						if (linesAvailable > 1 && stringBounds.getWidth() > maxWidth) {
							String l = title;
							for (int i = 0; ; ++i) {
								int lWidth = (int) stringBounds.getWidth();
								int guess = Math.min(l.length(), (int) (l.length() * maxWidth / lWidth));
								String l1 = null;
							    while (guess > 0) {
							    	l1 = l.substring(0, guess);
							    	stringBounds = fontMetrics.getStringBounds(l1, g2d);
							    	if (stringBounds.getWidth() <= maxWidth) {
							    		break;
							    	}
							    	--guess;
							    }
							    if (l1 == null) {
							    	break;
							    }
								g2d.drawString(l1, sx + sb, (int)(sy + stringBounds.getHeight() * (hf - 1) + 3 - stringBounds.getY() + i * stringBounds.getHeight()));
							    l = l.substring(guess);
							    ++linesNeeded;
							    if (linesNeeded > linesAvailable) {
							    	break;
							    }
							}
						}
						if (linesNeeded <= 1) {
							g2d.drawString(title, (int)(sx + Math.max(0, (sw - stringBounds.getWidth()) / 2)), (int)(sy + stringBounds.getHeight() * (hf - 1) + 3 - stringBounds.getY()));
						}
					}
					g2d.setClip(clip);
					if (onPath.contains(browser)) {
						rectangle = subBorder(browser.internalFrame.getBounds());
						if (browser.internalFrame.isSelected()) {
							paintRect(g2d, rectangle.x, rectangle.y, rectangle.width, rectangle.height, new Color(0, 100, 255), null, new BasicStroke(2));
						} else {
							paintRect(g2d, rectangle.x, rectangle.y, rectangle.width, rectangle.height, new Color(0, 100, 255).brighter(), null, new BasicStroke(1));
						}
					}
				}
			}
			rectangle = desktop.getVisibleRect();
			sx = (int)(offX + scale * (double) rectangle.x + 0.5);
			sy = (int)(offY + scale * (double) rectangle.y + 0.5);
			sw = (int)(scale * (double) (rectangle.width) + 0.5);
			sh = (int)(scale * (double) rectangle.height + 0.5);
			visibleRectInOutline = new Rectangle(sx, sy, sw, sh);
			g2d.setColor(new Color(0, 0, 200));
			g2d.setStroke(new BasicStroke(stroke.getLineWidth(), stroke.getEndCap(), stroke.getLineJoin(), stroke.getMiterLimit(), new float[] { 11f, 5f }, (float) ((System.currentTimeMillis() / 50.0 * desktop.getAnimationFactor()) % 16)));
			g2d.drawRoundRect(sx, sy, sw, sh, 8, 8);
		}
	}

	private void calcOnPath(Set<RowBrowser> onPath, List<RowBrowser> browsers) {
		browsers.stream().filter(b -> b.internalFrame.isSelected()).findAny().ifPresent(
			b -> {
				while (b != null) {
					onPath.add(b);
					b = b.parent;
				}
			});
		
	}

	private List<RowBrowser> getBrowsers() {
		List<RowBrowser> browsers = desktop.getBrowsers();
		for (RowBrowser b: browsers) {
			if (!b.isHidden() && b.internalFrame.isSelected()) {
				if (b.internalFrame.isMaximum()) {
					browsers.clear();
				}
				browsers.remove(b);
				browsers.add(b);
				break;
			}
		}
		return browsers;
	}

	private int snap(int a, int b, int minDist) {
		if (Math.abs(a - b) < minDist) {
			return a;
		}
		return b;
	}

	@Override
	public Dimension getPreferredSize() {
		return getMinimumSize();
	}

	@Override
	public Dimension getMinimumSize() {
		int maxHeight = (int) ((Math.max(1.0, Math.min(1.5, (controlPanel.getHeight() - 750) / 500.0)) + 1) * 220.0);
		if (desktop.getWidth() == 0 || desktop.getHeight() == 0) {
			return new Dimension(1, maxHeight);
		}
		maxHeight = (int) Math.max(1.0, Math.min(maxHeight, sameWidthFriend.getHeight() - 124));
		double r = 6;
		return new Dimension(1, Math.max(1, (int) (Math.min(maxHeight, (((double) sameWidthFriend.getWidth() - r * 2.0) / (double) desktop.getWidth() * (double) desktop.getHeight())) + r * 2.0)));
	}
	
	public boolean draggingInProgress() {
		return draggingStart != null;
	}

	private Rectangle subBorder(Rectangle rect) {
		int border = (int) (4 / scale);
		return new Rectangle(rect.x + border, rect.y + border, rect.width - 2 * border, rect.height - 2 * border);
	}

	private void paintRect(Graphics2D g, double x, double y, double width, double height, Color borderColor, Color backgroundColor, BasicStroke stroke) {
		int sx = (int)(offX + scale * x + 0.5);
		int sy = (int)(offY + scale * y + 0.5);
		int sw = (int)(scale * width + 0.5);
		int sh = (int)(scale * height + 0.5);
		if (backgroundColor != null) {
			g.setColor(backgroundColor);
			GradientPaint paint = new GradientPaint(
					sx, sy, backgroundColor.brighter(),
					sx + sw, sy + sh, backgroundColor);
			g.setPaint(paint);
			g.fillRoundRect(sx, sy, sw, sh, 8, 8);
		}
		if (borderColor != null) {
			g.setColor(borderColor);
			g.setStroke(stroke);
			g.drawRoundRect(sx, sy, sw, sh, 8, 8);
		}
	}

	private RowBrowser findBrowser(MouseEvent e) {
		RowBrowser browser = null;
		for (RowBrowser b: getBrowsers()) {
			if (!b.isHidden()) {
				Rectangle rectangle = subBorder(b.internalFrame.getBounds());
				int sx = (int)(offX + scale * (double) rectangle.x + 0.5);
				int sy = (int)(offY + scale * (double) rectangle.y + 0.5);
				int sw = (int)(scale * (double) rectangle.width + 0.5);
				int sh = (int)(scale * (double) rectangle.height + 0.5);
				if (e.getPoint().getX() < sx + sw && e.getPoint().getX() >= sx) {
					if (e.getPoint().getY() < sy + sh && e.getPoint().getY() >= sy) {
						browser = b;
					}
				}
			}
		}
		return browser;
	}

	private void setDragViewPosition(JScrollPane scrollPane, Desktop desktop, MouseEvent e) {
		int minDist = 64;
		int minDistW = Math.max(0, Math.min(minDist, (desktop.getWidth() - desktop.getVisibleRect().width) / 2 - 1));
		int minDistH = Math.max(0, Math.min(minDist, (desktop.getHeight() - desktop.getVisibleRect().height) / 2 - 1));
		Point p = new Point(
				(int) Math.max(0, Math.min(desktop.getWidth() - desktop.getVisibleRect().width, draggingViewPosition.x + (e.getPoint().x - draggingStart.x) / scale)), 
				(int) Math.max(0, Math.min(desktop.getHeight() - desktop.getVisibleRect().height, draggingViewPosition.y + (e.getPoint().y - draggingStart.y) / scale)));
		scrollPane.getViewport().setViewPosition(
				new Point(
						snap(desktop.getWidth() - desktop.getVisibleRect().width, snap(0, p.x, minDistW), minDistW),
						snap(desktop.getHeight() - desktop.getVisibleRect().height, snap(0, p.y, minDistH), minDistH))
				);
	}

}
