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
package net.sf.jailer.ui.databrowser;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.geom.Path2D;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.swing.JInternalFrame;

import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.UIUtil.PLAF;

/**
 * Renders state changes of {@link JInternalFrame}s in {@link Desktop}.
 * 
 * @author Ralf Wisser
 */
public class DesktopIFrameStateChangeRenderer {
	
	private final double DURATION = 800.0;
	private List<JInternalFrame> atomicBlock = null;
	
	private class StateChange {
		JInternalFrame iFrame;
		long startTime;
		double factorOffset = 0;
	}
	
	private Map<JInternalFrame, StateChange> stateChanges = new LinkedHashMap<JInternalFrame, StateChange>();
	
	public void onNewIFrame(final JInternalFrame iFrame) {
		if (atomicBlock != null) {
			atomicBlock.add(iFrame);
		} else {
			UIUtil.invokeLater(12, new Runnable() {
				@Override
				public void run() {
					StateChange stateChange = new StateChange();
					stateChange.iFrame = iFrame;
					stateChange.startTime = System.currentTimeMillis();
					stateChanges.put(iFrame, stateChange);
				}
			});
		}
	}
 
	public void onIFrameSelected(JInternalFrame iFrame) {
		onIFrameSelected(iFrame, 0);
	}

	public void onIFrameSelected(final JInternalFrame iFrame, final double factorOffset) {
		if (atomicBlock == null) {
			UIUtil.invokeLater(12, new Runnable() {
				@Override
				public void run() {
					if (!stateChanges.containsKey(iFrame)) {
						StateChange stateChange = new StateChange();
						stateChange.iFrame = iFrame;
						stateChange.startTime = System.currentTimeMillis();
						stateChange.factorOffset = factorOffset;
						stateChanges.put(iFrame, stateChange);
					}
				}
			});
		}
	}

	public void render(Graphics2D g2d) {
		for (Iterator<Entry<JInternalFrame, StateChange>> iter = stateChanges.entrySet().iterator(); iter.hasNext(); ) {
			StateChange stateChange = iter.next().getValue();
			
			double factor = (System.currentTimeMillis() - stateChange.startTime) / DURATION;
			factor += stateChange.factorOffset;
			if (factor >= 1) {
				iter.remove();
				continue;
			}
			if (factor < 0) {
				factor = 0;
			}
			
			if (stateChange.iFrame.isVisible()) {
				Color color = UIUtil.plaf == PLAF.FLATDARK? new Color(200, 145, 0, (int) (170 * (1 - factor))) : new Color(255, 205, 0, (int) (170 * (1 - factor)));
				g2d.setColor(color);
				g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
				double width = stateChange.iFrame.getWidth() / 20 * ((factor - stateChange.factorOffset) / (1 - stateChange.factorOffset) + 0.1);
				BasicStroke stroke = new BasicStroke((float) Math.max(0.1, width), BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);
				g2d.setStroke(stroke);
				
				Rectangle rect = stateChange.iFrame.getBounds();
				int w = (int) (0.5 * width);
				rect = new Rectangle(rect.x + w / 2, rect.y + w / 2, rect.width - w, rect.height - w);
				
				Path2D.Double path = new Path2D.Double();
				path.moveTo(rect.getX(), rect.getY());
				path.lineTo(rect.getX() + rect.getWidth(), rect.getY());
				path.lineTo(rect.getX() + rect.getWidth(), rect.getY() + rect.getHeight());
				path.lineTo(rect.getX(), rect.getY() + rect.getHeight());
				path.lineTo(rect.getX(), rect.getY());
				
				g2d.draw(path);
			}
		}		
	}

	public void startAtomic() {
		atomicBlock = new ArrayList<JInternalFrame>();
	}

	public void endAtomic() {
		if (atomicBlock != null) {
			final List<JInternalFrame> iFrames = atomicBlock;
			atomicBlock = null;
			UIUtil.invokeLater(12, new Runnable() {
				@Override
				public void run() {
					long currentTimeMillis = System.currentTimeMillis();
					long offset = 0;
					for (int i = iFrames.size() - 1; i >= 0; --i) {
						JInternalFrame iFrame = iFrames.get(i);
						StateChange stateChange = new StateChange();
						stateChange.iFrame = iFrame;
						stateChange.startTime = currentTimeMillis - offset;
						stateChanges.put(iFrame, stateChange);
						offset += (long) (DURATION / 3);
					}
				}
			});
		}
	}

	public void rollbackAtomic() {
		atomicBlock = null;
	}

}
