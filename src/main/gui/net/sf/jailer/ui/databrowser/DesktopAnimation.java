/*
 * Copyright 2007 - 2018 the original author or authors.
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

import java.awt.Point;
import java.awt.Rectangle;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import javax.swing.JInternalFrame;

import net.sf.jailer.ui.UIUtil;

/**
 * Animates layout changes of {@link Desktop}.
 * 
 * @author Ralf Wisser
 */
public class DesktopAnimation {
	
	private final double DURATION = 1000;
	private final Desktop desktop;
	
	/**
	 * Animation.
	 */
	abstract class Animation {
		private long startTime;
		public void start() {
			startTime = System.currentTimeMillis();
		}
		abstract boolean animate(double f);
	};

	/**
	 * Animation per subject (started).
	 */
	private Map<Object, Animation> animations = new HashMap<Object, Animation>();

	/**
	 * Animation per subject (waiting).
	 */
	private Map<Object, Animation> waiting = new HashMap<Object, Animation>();

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
		private Point lastViewPosition;
	
		public ScrollTo(Rectangle scrollTo, Point scrollFrom, int initialWidth, int initialHeight) {
			this.scrollFrom = scrollFrom;
			this.initialWidth = initialWidth;
			this.initialHeight = initialHeight;
			this.scrollTo = scrollTo;
		}

		public boolean animate(double f) {
			if (lastViewPosition != null && !lastViewPosition.equals(desktop.getScrollPane().getViewport().getViewPosition())) {
				return false;
			}
			if (scrollTo != null) {
				int w = wAvg(f, initialWidth, scrollTo.width);
				int h = wAvg(f, initialHeight, scrollTo.height);
				int x = (int) (scrollFrom.x + f * (scrollTo.x + scrollTo.width / 2 - scrollFrom.x)) - w / 2;
				int y = (int) (scrollFrom.y + f * (scrollTo.y + scrollTo.height / 2 - scrollFrom.y)) - h / 2;
				desktop.scrollRectToVisible(new Rectangle(x, y, w, h));
			}
			lastViewPosition = desktop.getScrollPane().getViewport().getViewPosition();
			return true;
		}
	}
 
	/**
	 * Moves an internal frame of the desktop.
	 */
	class MoveIFrame extends Animation {
		private final JInternalFrame iFrame;
		private final Rectangle moveTo;
		private final Rectangle moveFrom;
		private final BrowserContentPane browserContentPane;
	
		public MoveIFrame(JInternalFrame iFrame, BrowserContentPane browserContentPane, Rectangle moveTo) {
			this.iFrame = iFrame;
			this.browserContentPane = browserContentPane;
			this.moveTo = moveTo;
			this.moveFrom = iFrame.getBounds();
		}

		public boolean animate(double f) {
			int wx = wAvg(f, moveFrom.x, moveTo.x);
			int wy = wAvg(f, moveFrom.y, moveTo.y);
			int ww = wAvg(f, moveFrom.width, moveTo.width);
			int wh = wAvg(f, moveFrom.height, moveTo.height);
			if (f < 1.0 && Math.abs(ww - iFrame.getWidth()) < 4 && Math.abs(wh - iFrame.getHeight()) < 4) {
				iFrame.setLocation(wx, wy);
			} else {
				iFrame.setBounds(wx, wy, ww, wh);
			}
			if (f == 1.0) {
				browserContentPane.adjustRowTableColumnsWidth();
			}
			return !(wx == moveTo.x && wy == moveTo.y && ww == moveTo.width && wh == moveTo.height);
		}
	}

	/**
	 * Performs an animation step for each animation. 
	 */
	public boolean animate() {
		boolean result = false;
		boolean wasActive = isActive();
		for (Iterator<Entry<Object, Animation>> i = animations.entrySet().iterator(); i.hasNext(); ) {
			Animation animation = i.next().getValue();
			double f = (System.currentTimeMillis() - animation.startTime) / DURATION;
			double fs;

			if (f > 1.0) {
				f = 1.0;
				fs = 1.0;
			} else {
				fs = Math.pow(f, 0.3);
			}

			if (!animation.animate(fs)) {
				i.remove();
				continue;
			}
			result = true;

			if (f == 1.0) {
				i.remove();
			}
		}
		if (wasActive && !isActive()) {
			desktop.checkDesktopSize();
		}
		return result;
	}

	/**
	 * Scrolls desktop to a given location (animated).
	 * 
	 * @param vr the location
	 */
	public void scrollRectToVisible(Rectangle vr) {
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
			startAnimation(desktop, new ScrollTo(vr, new Point(mx, my), 2 * Math.min(mx - svr.x, svr.x + svr.width - mx), 2 * Math.min(my - svr.y, svr.y + svr.height - my)));
		}
	}

	/**
	 * Scrolls desktop to a given location (immediately).
	 * 
	 * @param vr the location
	 */
	public void scrollRectToVisibleImmediately(Rectangle vr) {
		desktop.scrollRectToVisible(vr);
		animations.remove(desktop);
	}

	/**
	 * Sets the bounds of an internal frame (animated).
	 * 
	 * @param iFrame the frame
	 * @param browserContentPane the content pane
	 * @param r new bounds
	 */
	public void setIFrameBounds(JInternalFrame iFrame, BrowserContentPane browserContentPane, Rectangle r) {
		startAnimation(iFrame, new MoveIFrame(iFrame, browserContentPane, r));
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
		animations.remove(internalFrame);
	}

	public Rectangle getIFrameBounds(JInternalFrame iFrame) {
		Animation animation = animations.get(iFrame);
		if (animation instanceof MoveIFrame) {
			return ((MoveIFrame) animation).moveTo;
		}
		animation = waiting.get(iFrame);
		if (animation instanceof MoveIFrame) {
			return ((MoveIFrame) animation).moveTo;
		}
		return iFrame.getBounds();
	}

	private int wAvg(double f, int a, int b) {
		return (int) (a + f * (b - a));
	}
	
	private void startAnimation(final Object key, final Animation animation) {
		waiting.put(key, animation);
		UIUtil.invokeLater(12, new Runnable() {
			@Override
			public void run() {
				animation.start();
				waiting.remove(key);
				animations.put(key, animation);
			}
		});
	}

	public boolean isActive() {
		return !animations.isEmpty();
	}

}
