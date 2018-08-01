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

/**
 * Animates layout changes of {@link Desktop}.
 * 
 * @author Ralf Wisser
 */
public class DesktopAnimation {
	
	private final double DURATION = 750;
	private final Desktop desktop;
	
	/**
	 * Animation.
	 */
	abstract class Animation {
		private final long startTime;
		Animation() {
			startTime = System.currentTimeMillis();
		}
		abstract void animate(double f);
	};

	/**
	 * Animation per subject.
	 */
	private Map<Object, Animation> animations = new HashMap<Object, Animation>();
	
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
		private final Rectangle scrollTo;
	
		public ScrollTo(Rectangle scrollTo, Point scrollFrom) {
			this.scrollFrom = scrollFrom;
			this.scrollTo = scrollTo;
		}

		public void animate(double f) {
			if (scrollTo != null) {
				int w = (int) (f * scrollTo.width);
				int h = (int) (f * scrollTo.height);
				int x = (int) (scrollFrom.x + f * (scrollTo.x + scrollTo.width / 2 - scrollFrom.x)) - w / 2;
				int y = (int) (scrollFrom.y + f * (scrollTo.y + scrollTo.height / 2 - scrollFrom.y)) - h / 2;
				desktop.scrollRectToVisible(new Rectangle(x, y, w, h));
			}
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

		private int wAvg(double f, int a, int b) {
			return (int) (a + f * (b - a));
		}
		
		public void animate(double f) {
			iFrame.setBounds(
					wAvg(f, moveFrom.x, moveTo.x),
					wAvg(f, moveFrom.y, moveTo.y),
					wAvg(f, moveFrom.width, moveTo.width),
					wAvg(f, moveFrom.height, moveTo.height));
			if (f == 1.0) {
				browserContentPane.adjustRowTableColumnsWidth();
			}
		}
	}

	/**
	 * Performs an animation step for each animation. 
	 */
	public boolean animate() {
		boolean result = false;
		for (Iterator<Entry<Object, Animation>> i = animations.entrySet().iterator(); i.hasNext(); ) {
			Animation animation = i.next().getValue();
			double f = (System.currentTimeMillis() - animation.startTime) / DURATION;
			if (f > 1.0) {
				f = 1.0;
			}

			double fs = Math.pow(f, 0.4);

			animation.animate(fs);
			result = true;

			if (f == 1.0) {
				i.remove();
			}
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
		animations.put(desktop, new ScrollTo(vr, new Point(mx, my)));
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
		animations.put(iFrame, new MoveIFrame(iFrame, browserContentPane, r));
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
		return iFrame.getBounds();
	}

}
