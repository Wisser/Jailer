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

import java.awt.Component;
import java.awt.Point;
import java.awt.Rectangle;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import javax.swing.JInternalFrame;
import javax.swing.SwingUtilities;

import net.sf.jailer.ui.UIUtil;

/**
 * Animates layout changes of {@link Desktop}.
 * 
 * @author Ralf Wisser
 */
public class DesktopAnimation {
	
	private final double DURATION = 750;
	private final double FAST_LINEAR_DURATION = DURATION / 4;
	private final Desktop desktop;
	
	private static final boolean PRINT_FPS = false;

	/**
	 * Animation.
	 */
	abstract class Animation {
		private long startTime;
		public final boolean fastLinear;
		
		public Animation(boolean fastLinear) {
			this.fastLinear = fastLinear;
		}
		
		public void start() {
			startTime = System.currentTimeMillis();
			stopScrolling = false;
		}
		abstract boolean animate(double f);
	}

	/**
	 * Animation per subject (started).
	 */
	private Map<Object, Animation> animations = Collections.synchronizedMap(new LinkedHashMap<Object, Animation>());

	/**
	 * Animation per subject (waiting).
	 */
	private Map<Object, Animation> waiting = new LinkedHashMap<Object, Animation>();

	/**
	 * Constructor.
	 * 
	 * @param desktop the desktop
	 */
	public DesktopAnimation(Desktop desktop) {
		this.desktop = desktop;
	}
	
	/**
	 * Scrolls desktop to a given location.
	 */
	class ScrollTo extends Animation {
		private final Point scrollFrom;
		private final int initialWidth;
		private final int initialHeight;
		private final Rectangle scrollTo;

		public ScrollTo(Rectangle scrollTo, Point scrollFrom, int initialWidth, int initialHeight, boolean fastLinear) {
			super(fastLinear);
			this.scrollFrom = scrollFrom;
			this.initialWidth = initialWidth;
			this.initialHeight = initialHeight;
			this.scrollTo = scrollTo;
		}

		public boolean animate(double f) {
			if (stopScrolling) {
				return false;
			}
			if (scrollTo != null) {
				int w = wAvg(f, initialWidth, scrollTo.width);
				int h = wAvg(f, initialHeight, scrollTo.height);
				int x = (int) (scrollFrom.x + f * (scrollTo.x + scrollTo.width / 2 - scrollFrom.x)) - w / 2;
				int y = (int) (scrollFrom.y + f * (scrollTo.y + scrollTo.height / 2 - scrollFrom.y)) - h / 2;
				desktop.checkDesktopSize();
				desktop.scrollRectToVisible(new Rectangle(x, y, w, h));
			}
			return true;
		}
	}
 
	/**
	 * Moves an internal frame of the desktop.
	 */
	class MoveIFrame extends Animation {
		private final JInternalFrame iFrame;
		private final Rectangle moveTo;
		private Rectangle moveFrom;
		private final BrowserContentPane browserContentPane;
	
		public MoveIFrame(JInternalFrame iFrame, BrowserContentPane browserContentPane, Rectangle moveTo, boolean fastLinear) {
			super(fastLinear);
			this.iFrame = iFrame;
			this.browserContentPane = browserContentPane;
			this.moveTo = moveTo;
		}

		public boolean animate(double f) {
			if (moveFrom == null) {
				moveFrom = iFrame.getBounds();
				if (Math.abs(moveFrom.width - moveTo.width) > 2) {
					browserContentPane.adjustRowTableColumnsWidth();
				}
			}
			int wx = wAvg(f, moveFrom.x, moveTo.x);
			int wy = wAvg(f, moveFrom.y, moveTo.y);
			int ww = wAvg(f, moveFrom.width, moveTo.width);
			int wh = wAvg(f, moveFrom.height, moveTo.height);
			if (f < 1.0 && Math.abs(ww - iFrame.getWidth()) < 2 && Math.abs(wh - iFrame.getHeight()) < 2) {
				iFrame.setLocation(wx, wy);
			} else {
				iFrame.setBounds(wx, wy, ww, wh);
			}
			if (f == 1.0) {
				if (Math.abs(moveFrom.width - moveTo.width) > 2) {
					browserContentPane.adjustRowTableColumnsWidth();
				}
			}
			return !(wx == moveTo.x && wy == moveTo.y && ww == moveTo.width && wh == moveTo.height);
		}
	}

	private long startTime = 0;
	private long count = 0;
	private boolean lastResult;
    
	/**
	 * Performs an animation step for each animation. 
	 */
	public boolean animate() {
		final long currentTimeMillis = System.currentTimeMillis();
		boolean result = false;
		for (Iterator<Entry<Object, Animation>> i = animations.entrySet().iterator(); i.hasNext(); ) {
			Animation animation = i.next().getValue();
			double duration = animation.fastLinear? FAST_LINEAR_DURATION : DURATION;
			double f = (currentTimeMillis - animation.startTime) / duration;
			double fs;

			if (f >= 1.0) {
				f = 1.0;
				fs = 1.0;
			} else {
				final double M1 = -3;
				final double M2 = 5;
				fs = sig(f * (-M1 + M2) + M1);
			}

			if (!animation.animate(animation.fastLinear? f : fs)) {
				i.remove();
				continue;
			}
			result = true;

			if (f == 1.0) {
				i.remove();
			}
		}
		desktop.checkDesktopSize();
		if (PRINT_FPS) {
			++count;
			if (lastResult != result) {
				System.out.println(result + ": " + (1000.0 * count / (double) (System.currentTimeMillis() - startTime)) + " FPS ");
				startTime = System.currentTimeMillis();
				lastResult = result;
				count = 0;
			}
		}
		return result;
	}

	private double sig(double x) {
		return 1.0 / (1.0 + Math.exp(-x));
	}
	
	/**
	 * Scrolls desktop to a given location (animated).
	 * 
	 * @param vr the location
	 */
	public void scrollRectToVisible(Rectangle vr, boolean fastLinear) {
		Rectangle svr = desktop.getScrollPane().getViewport().getViewRect();
		if (!svr.contains(vr)) {
			int mx = vr.x + vr.width / 2;
			int my = vr.y + vr.height / 2;
			if (mx < svr.x) {
				mx = svr.x;
			}
			if (my < svr.y) {
				my = svr.y;
			}
			if (mx > svr.x + svr.width) {
				mx = svr.x + svr.width;
			}
			if (my > svr.y + svr.height) {
				my = svr.y + svr.height;
			}
			startAnimation(desktop, new ScrollTo(vr, new Point(mx, my), 2 * Math.min(mx - svr.x, svr.x + svr.width - mx), 2 * Math.min(my - svr.y, svr.y + svr.height - my), fastLinear));
		} else {
			waiting.remove(desktop);
			animations.remove(desktop);
		}
	}

	/**
	 * Scrolls desktop to a given location (immediately).
	 * 
	 * @param vr the location
	 */
	public void scrollRectToVisibleImmediately(Rectangle vr) {
		desktop.scrollRectToVisible(vr);
		waiting.remove(desktop);
		animations.remove(desktop);
	}

	/**
	 * Sets the bounds of an internal frame (animated).
	 * 
	 * @param iFrame the frame
	 * @param browserContentPane the content pane
	 * @param r new bounds
	 */
	public void setIFrameBounds(JInternalFrame iFrame, BrowserContentPane browserContentPane, Rectangle r, boolean rescale) {
		startAnimation(iFrame, new MoveIFrame(iFrame, browserContentPane, r, rescale));
	}

	/**
	 * Sets the bounds of an internal frame (immediately).
	 * 
	 * @param iFrame the frame
	 * @param browserContentPane the content pane
	 * @param r new bounds
	 */
	public void setIFrameBoundsImmediately(JInternalFrame internalFrame, BrowserContentPane browserContentPane, Rectangle newBounds) {
		internalFrame.setBounds(newBounds);
		browserContentPane.adjustRowTableColumnsWidth();
		waiting.remove(internalFrame);
		animations.remove(internalFrame);
	}

	public Rectangle getIFrameBounds(JInternalFrame iFrame) {
		Animation animation = waiting.get(iFrame);
		if (animation instanceof MoveIFrame) {
			return ((MoveIFrame) animation).moveTo;
		}
		animation = animations.get(iFrame);
		if (animation instanceof MoveIFrame) {
			return ((MoveIFrame) animation).moveTo;
		}
		return iFrame.getBounds();
	}

	private int wAvg(double f, int a, int b) {
		return (int) (a + f * (b - a));
	}
	
	private void startAnimation(Object key, Animation animation) {
		Component desktopAncestor = SwingUtilities.getWindowAncestor(desktop);
		if (desktopAncestor == null) {
			desktopAncestor = desktop;
		}
		final Component fDesktopAncestor = desktopAncestor;
		UIUtil.setWaitCursor(desktopAncestor);
		waiting.put(key, animation);
		UIUtil.invokeLater(4, new Runnable() {
			@Override
			public void run() {
				UIUtil.resetWaitCursor(fDesktopAncestor);
				for (Entry<Object, Animation> e: waiting.entrySet()) {
					e.getValue().start();
					animations.put(e.getKey(), e.getValue());
				}
				waiting.clear();
			}
		});
	}

	public static boolean stopScrolling = false;
	
	public boolean isActive() {
		return !animations.isEmpty();
	}

}
