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

import java.awt.Color;
import java.awt.ComponentOrientation;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Point;
import java.awt.Window;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.event.MouseInputAdapter;

import net.sf.jailer.ui.UIUtil;


/**
 * A component that allows its parent window to be resizable, similar to the
 * size grip seen on status bars.  This is essentially a copy of the class with
 * the same name in RSyntaxTextArea, but is duplicated to prevent a dependency
 * on that library.
 *
 * @author Robert Futrell
 * @version 1.0
 */
public class SizeGrip extends JPanel {

	/**
	 * The size grip to use if we're on OS X.
	 */
	private Image osxSizeGrip;


	public SizeGrip() {
		MouseHandler adapter = new MouseHandler();
		addMouseListener(adapter);
		addMouseMotionListener(adapter);
		setPreferredSize(new Dimension(16, 16));
		setCursor(Cursor.getPredefinedCursor(Cursor.SE_RESIZE_CURSOR));
	}


	/**
	 * Overridden to ensure that the cursor for this component is appropriate
	 * for the orientation.
	 *
	 * @param o The new orientation.
	 */
	@Override
	public void applyComponentOrientation(ComponentOrientation o) {
		possiblyFixCursor(o.isLeftToRight());
		super.applyComponentOrientation(o);
	}


	/**
	 * Creates and returns the OS X size grip image.
	 *
	 * @return The OS X size grip.
	 */
	private Image createOSXSizeGrip() {
		ClassLoader cl = getClass().getClassLoader();
		URL url = cl.getResource("org/fife/ui/autocomplete/osx_sizegrip.png");
		if (url==null) {
			// We're not running in a jar - we may be debugging in Eclipse,
			// for example
			File f = new File("../AutoComplete/src/org/fife/ui/autocomplete/osx_sizegrip.png");
			if (f.isFile()) {
				try {
					url = f.toURI().toURL();
				} catch (MalformedURLException mue) { // Never happens
					mue.printStackTrace();
					return null;
				}
			}
			else {
				return null; // Can't find resource or image file
			}
		}
		Image image = null;
		try {
			image = ImageIO.read(url);
			image = UIUtil.scaleIcon(new ImageIcon(image), 1.0).getImage();
		} catch (IOException ioe) { // Never happens
			ioe.printStackTrace();
		}
		return image;
	}


	/**
	 * Paints this panel.
	 *
	 * @param g The graphics context.
	 */
	@Override
	protected void paintComponent(Graphics g) {

		super.paintComponent(g);

		Dimension dim = getSize();

		if (osxSizeGrip!=null) {
			g.drawImage(osxSizeGrip, dim.width-16, dim.height-16, null);
			return;
		}

		Color c1 = UIManager.getColor("Label.disabledShadow");
		Color c2 = UIManager.getColor("Label.disabledForeground");
		ComponentOrientation orientation = getComponentOrientation();

		if (orientation.isLeftToRight()) {
			int width = dim.width  -= 3;
			int height = dim.height -= 3;
			g.setColor(c1);
			g.fillRect(width-9,height-1, 3,3);
			g.fillRect(width-5,height-1, 3,3);
			g.fillRect(width-1,height-1, 3,3);
			g.fillRect(width-5,height-5, 3,3);
			g.fillRect(width-1,height-5, 3,3);
			g.fillRect(width-1,height-9, 3,3);
			g.setColor(c2);
			g.fillRect(width-9,height-1, 2,2);
			g.fillRect(width-5,height-1, 2,2);
			g.fillRect(width-1,height-1, 2,2);
			g.fillRect(width-5,height-5, 2,2);
			g.fillRect(width-1,height-5, 2,2);
			g.fillRect(width-1,height-9, 2,2);
		}
		else {
			int height = dim.height -= 3;
			g.setColor(c1);
			g.fillRect(10,height-1, 3,3);
			g.fillRect(6,height-1, 3,3);
			g.fillRect(2,height-1, 3,3);
			g.fillRect(6,height-5, 3,3);
			g.fillRect(2,height-5, 3,3);
			g.fillRect(2,height-9, 3,3);
			g.setColor(c2);
			g.fillRect(10,height-1, 2,2);
			g.fillRect(6,height-1, 2,2);
			g.fillRect(2,height-1, 2,2);
			g.fillRect(6,height-5, 2,2);
			g.fillRect(2,height-5, 2,2);
			g.fillRect(2,height-9, 2,2);
		}

	}


	/**
	 * Ensures that the cursor for this component is appropriate for the
	 * orientation.
	 *
	 * @param ltr Whether the current component orientation is LTR.
	 */
	protected void possiblyFixCursor(boolean ltr) {
		int cursor = Cursor.NE_RESIZE_CURSOR;
		if (ltr) {
			cursor = Cursor.NW_RESIZE_CURSOR;
		}
		if (cursor!=getCursor().getType()) {
			setCursor(Cursor.getPredefinedCursor(cursor));
		}
	}


	@Override
	public void updateUI() {
		super.updateUI();
		if (System.getProperty("os.name").contains("OS X")) {
			if (osxSizeGrip==null) {
				osxSizeGrip = createOSXSizeGrip();
			}
		}
		else { // Clear memory in case of runtime LaF change.
			osxSizeGrip = null;
		}
	}


	/**
	 * Listens for mouse events on this panel and resizes the parent window
	 * appropriately.
	 */
	/*
	 * NOTE: We use SwingUtilities.convertPointToScreen() instead of just using
	 * the locations relative to the corner component because the latter proved
	 * buggy - stretch the window too wide and some kind of arithmetic error
	 * started happening somewhere - our window would grow way too large.
	 */
	private class MouseHandler extends MouseInputAdapter {

		private Point origPos;

		@Override
		public void mouseDragged(MouseEvent e) {
			if (origPos == null) {
				return;
			}
			Point newPos = e.getPoint();
			SwingUtilities.convertPointToScreen(newPos, SizeGrip.this);
			int xDelta = newPos.x - origPos.x;
			int yDelta = newPos.y - origPos.y;
			Window wind = SwingUtilities.getWindowAncestor(SizeGrip.this);
			if (wind!=null) { // Should always be true
				if (getComponentOrientation().isLeftToRight()) {
					int w = wind.getWidth();
					if (newPos.x>=wind.getX() + 100) {
						w += xDelta;
					}
					int h = wind.getHeight();
					if (newPos.y>=wind.getY() + 100) {
						h += yDelta;
					}
					wind.setSize(Math.max(w, 1), Math.max(h, 1));
				}
				else { // RTL
					int newW = Math.max(1, wind.getWidth()-xDelta);
					int newH = Math.max(1, wind.getHeight()+yDelta);
					wind.setBounds(newPos.x, wind.getY(), newW, newH);
				}
				// invalidate()/validate() needed pre-1.6.
				wind.invalidate();
				wind.validate();
			}
			origPos.setLocation(newPos);
		}

		@Override
		public void mousePressed(MouseEvent e) {
			origPos = e.getPoint();
			SwingUtilities.convertPointToScreen(origPos, SizeGrip.this);
		}

		@Override
		public void mouseReleased(MouseEvent e) {
			origPos = null;
		}

	}


}