/*
 * 12/23/2008
 *
 * SizeGrip.java - A size grip component that sits at the bottom of the window,
 * allowing the user to easily resize that window.
 * 
 * This library is distributed under a modified BSD license.  See the included
 * RSyntaxTextArea.License.txt file for details.
 */
package net.sf.jailer.ui.util;

import java.awt.BasicStroke;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.LayoutManager;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.Window;
import java.awt.event.MouseEvent;
import java.awt.geom.Path2D;

import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.event.MouseInputAdapter;

import net.sf.jailer.ui.Colors;

public class MovePanel extends JPanel {
		
		public MovePanel() {
	        MouseInputAdapter ml = new MouseInputAdapter() {
	    		private Point origPos;
	    		@Override
	    		public void mouseDragged(MouseEvent e) {
	    			if (origPos == null) {
	    				return;
	    			}
	    			Window window = SwingUtilities.getWindowAncestor(MovePanel.this);
	    			Point newPos = e.getPoint();
	    			SwingUtilities.convertPointToScreen(newPos, window);
	    			int xDelta = newPos.x - origPos.x;
	    			int yDelta = newPos.y - origPos.y;
	    			if (window!=null) { // Should always be true
	    				window.setLocation(window.getX() + xDelta, window.getY() + yDelta);
	    				window.invalidate();
	    				window.validate();
	    			}
	    			origPos.setLocation(newPos);
	    		}

	    		@Override
	    		public void mousePressed(MouseEvent e) {
	    			origPos = e.getPoint();
	    			SwingUtilities.convertPointToScreen(origPos, SwingUtilities.getWindowAncestor(MovePanel.this));
	    		}

	    		@Override
	    		public void mouseReleased(MouseEvent e) {
	    			origPos = null;
	    		}
	    	};
	    	addMouseListener(ml);
	    	addMouseMotionListener(ml);
	    	setCursor(Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR));
		}
		
		public MovePanel(LayoutManager layoutManager) {
			this();
			setLayout(layoutManager);
		}

		@Override
		protected void paintComponent(Graphics g) {
			super.paintComponent(g);
			Dimension dim = getSize();
			Graphics2D g2d = (Graphics2D) g;
			Shape clip = g2d.getClip();
			g2d.clipRect(0, 0, dim.width, dim.height);
			Stroke s = new BasicStroke();
			g2d.setStroke(s);
			g2d.setColor(Colors.Color_0_0_255_80);
			g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
		            RenderingHints.VALUE_ANTIALIAS_ON);
			g2d.setRenderingHint(RenderingHints.KEY_RENDERING,
		            RenderingHints.VALUE_RENDER_QUALITY);
			if (dim.width < dim.height) {
				for (int y = dim.width/ 16 + 3; y < dim.height - dim.width / 16 - 1; y += 5) {
					int h = dim.width / 2;
					Path2D.Double path = new Path2D.Double();
					path.moveTo(1, y);
					path.curveTo(h, y - h / 2, dim.width - h, y + h / 2,  dim.width, y);
					g2d.draw(path);
				}
			} else {
				for (int x = dim.height / 16 + 3; x < dim.width - dim.height / 16 - 1; x += 5) {
					int h = dim.height / 2;
					Path2D.Double path = new Path2D.Double();
					path.moveTo(x, 1);
					path.curveTo(x - h / 2, h, x + h / 2, dim.height - h, x, dim.height);
					g2d.draw(path);
				}
			}
			g2d.setClip(clip);
		}
	}
	
