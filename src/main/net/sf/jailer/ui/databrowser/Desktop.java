/*
 * Copyright 2007 the original author or authors.
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
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.beans.PropertyVetoException;
import java.lang.reflect.InvocationTargetException;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import javax.swing.DefaultDesktopManager;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JDesktopPane;
import javax.swing.JInternalFrame;
import javax.swing.JScrollPane;
import javax.swing.JViewport;
import javax.swing.SwingUtilities;
import javax.swing.event.InternalFrameEvent;
import javax.swing.event.InternalFrameListener;

import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.DbConnectionDialog.ConnectionInfo;
import prefuse.util.GraphicsLib;

/**
 * Desktop holding row-browsers as {@link JInternalFrame}s.
 * 
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public class Desktop extends JDesktopPane {

	/**
	 * The {@link DataModel}.
	 */
	private final DataModel datamodel;
	
	/**
	 * Icon for the row-browser frames.
	 */
	private final Icon jailerIcon;

	/**
	 * Default width of a row-browser frame.
	 */
	public static final int BROWSERTABLE_DEFAULT_WIDTH = 460;

	/**
	 * <code>true</code> while the desktop is visible.
	 */
	private boolean running;

	/**
	 * <code>false</code> if links must not be rendered (if a frame is maximized).
	 */
	private boolean renderLinks;

	/**
	 * DB session.
	 */
	private Session session;
	
	/**
	 * Constructor.
	 * 
	 * @param datamodel the {@link DataModel}
	 * @param jailerIcon icon for the frames
	 * @param connection DB-connection
	 */
	public Desktop(DataModel datamodel, Icon jailerIcon, ConnectionInfo connection, Frame parentFrame) {
		this.parentFrame = parentFrame;
		this.datamodel = datamodel;
		this.jailerIcon = jailerIcon;
		try {
			this.session = new Session(connection.driverClass, connection.url, connection.user, connection.password);
			setAutoscrolls(true);
			manager = new MDIDesktopManager(this);
			setDesktopManager(manager);
			synchronized (this) {
				running = true;
			}
			Thread updateUIThread = new Thread(new Runnable() {
				@Override
				public void run() {
					while (true) {
						synchronized (Desktop.this) {
							if (!running) {
								return;
							}
						}
						try {
							Thread.sleep(100);
							SwingUtilities.invokeAndWait(new Runnable() {
								@Override
								public void run() {
									if (calculateLinks()) {
										repaintDesktop();
									}
								}
							});
						} catch (InterruptedException e) {
							// ignore
						} catch (InvocationTargetException e) {
							// ignore
						}
					}
				}
			});
			updateUIThread.setDaemon(true);
			updateUIThread.start();
		} catch (Exception e) {
			UIUtil.showException(null, "Error", e);
		}
	}

	/**
	 * Renders a set of {@link Row}s.
	 */
	public class RowBrowser {
		
		/**
		 * Frame holding a {@link BrowserContentPane}.
		 */
		public JInternalFrame internalFrame;
		
		/**
		 * UI for row-browsing.
		 */
		public BrowserContentPane browserContentPane;
		
		/**
		 * Parent browser.
		 */
		public RowBrowser parent;
		
		/**
		 * Index of parent row in the parent's row browser.
		 */
		public int rowIndex;
		
		/**
		 * Coordinates of the link render.
		 */
		public int x1, y1, x2, y2;
		
		/**
		 * The link's color.
		 */
		public Color color;
	};

	/**
	 * All row-browsers.
	 */
	private List<RowBrowser> tableBrowsers = new ArrayList<RowBrowser>();

	/**
	 * Opens a new row-browser.
	 * 
	 * @param parent parent browser
	 * @param parentRowIndex index of parent row in the parent's row browser
	 * @param table to read rows from
	 * @param association to navigate, or <code>null</code>
	 * @return new row-browser
	 */
	public synchronized RowBrowser addTableBrowser(final RowBrowser parent, int parentRowIndex, final Table table, Association association) {
		final int MIN = 0, HEIGHT = 320, DISTANCE = 20;

		JInternalFrame jInternalFrame = new JInternalFrame(datamodel.getDisplayName(table));
		jInternalFrame.setClosable(true);
		jInternalFrame.setIconifiable(true);
		jInternalFrame.setMaximizable(true);
		jInternalFrame.setVisible(true);
		
		javax.swing.GroupLayout jInternalFrame1Layout = new javax.swing.GroupLayout(jInternalFrame.getContentPane());
		jInternalFrame.getContentPane().setLayout(jInternalFrame1Layout);
		jInternalFrame1Layout.setHorizontalGroup(jInternalFrame1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING).addGap(0, 162,
				Short.MAX_VALUE));
		jInternalFrame1Layout.setVerticalGroup(jInternalFrame1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING).addGap(0, 102,
				Short.MAX_VALUE));

		jInternalFrame.setResizable(true);
		if (jailerIcon != null) {
			jInternalFrame.setFrameIcon(jailerIcon);
		}
		add(jInternalFrame, javax.swing.JLayeredPane.DEFAULT_LAYER);

		jInternalFrame.getContentPane().setLayout(new BorderLayout());
 		
		jInternalFrame.addComponentListener(new ComponentListener() {

			@Override
			public void componentShown(ComponentEvent e) {
				repaintDesktop();
			}

			@Override
			public void componentResized(ComponentEvent e) {
				repaintDesktop();
			}

			@Override
			public void componentMoved(ComponentEvent e) {
				repaintDesktop();
			}

			@Override
			public void componentHidden(ComponentEvent e) {
				repaintDesktop();
			}
		});

		int x = MIN;
		if (parent != null) {
			x = parent.internalFrame.getX() + parent.internalFrame.getWidth() + DISTANCE;
		}
		int y = MIN;
		Rectangle r = new Rectangle(x, y, BROWSERTABLE_DEFAULT_WIDTH, HEIGHT);
		for (;;) {
			boolean ok = true;
			for (RowBrowser tb : tableBrowsers) {
				if (tb.internalFrame.getBounds().intersects(r)) {
					ok = false;
					break;
				}
			}
			r = new Rectangle(x, y, BROWSERTABLE_DEFAULT_WIDTH, HEIGHT);
			y += 8;
			if (ok) {
				break;
			}
		}

		jInternalFrame.setBounds(r);

		final RowBrowser tableBrowser = new RowBrowser();
		BrowserContentPane browserContentPane = new BrowserContentPane(table, session, parent == null? null : parent.browserContentPane.rows.get(parentRowIndex), association, parentFrame) {
			@Override
			protected void navigateTo(Association association, int rowIndex, Row row) {
				addTableBrowser(tableBrowser, rowIndex, association.destination, association);
			}
		};
		tableBrowser.internalFrame = jInternalFrame;
		tableBrowser.browserContentPane = browserContentPane;
		tableBrowser.rowIndex = parentRowIndex;
		tableBrowser.parent = parent;
		if (association != null) {
			tableBrowser.color = new java.awt.Color(0, 100, 255, 140);
			if (association.isInsertDestinationBeforeSource()) {
				tableBrowser.color = new java.awt.Color(170, 0, 0, 140);
			}
			if (association.isInsertSourceBeforeDestination()) {
				tableBrowser.color = new java.awt.Color(0, 112, 0, 140);
			}
			if (association.isIgnored()) {
				tableBrowser.color = new java.awt.Color(153, 153, 153, 140);
			}
		}
		tableBrowsers.add(tableBrowser);

		jInternalFrame.getContentPane().add(browserContentPane, java.awt.BorderLayout.CENTER);
		jInternalFrame.addInternalFrameListener(new InternalFrameListener() {
			@Override
			public void internalFrameOpened(InternalFrameEvent e) {
			}
			@Override
			public void internalFrameIconified(InternalFrameEvent e) {
				repaintDesktop();
			}
			@Override
			public void internalFrameDeiconified(InternalFrameEvent e) {
				repaintDesktop();
			}
			@Override
			public void internalFrameDeactivated(InternalFrameEvent e) {
			}
			@Override
			public void internalFrameClosing(InternalFrameEvent e) {
			}
			@Override
			public void internalFrameClosed(InternalFrameEvent e) {
				for (RowBrowser tb: tableBrowsers) {
					if (tb.parent == tableBrowser) {
						tb.parent = null;
					}
				}
				Desktop.this.remove(e.getInternalFrame());
				tableBrowsers.remove(tableBrowser);
				repaintDesktop();
			}
			@Override
			public void internalFrameActivated(InternalFrameEvent e) {
			}
		});

		checkDesktopSize();
		this.scrollRectToVisible(jInternalFrame.getBounds());
		
		return tableBrowser;
	}

	/**
	 * Repaints the desktop.
	 */
	private void repaintDesktop() {
		calculateLinks();
		JScrollPane scrollPane = getScrollPane();
		scrollPane.setSize(scrollPane.getWidth() + 1, scrollPane.getHeight() + 1);
		scrollPane.setSize(scrollPane.getWidth() - 1, scrollPane.getHeight() - 1);
		scrollPane.invalidate();
		scrollPane.validate();
	}

	/**
	 * Calculates coordinates of all link-renders.
	 * 
	 * @return <code>true</code> iff something has changed
	 */
	private synchronized boolean calculateLinks() {
		boolean changed = false;
		for (RowBrowser tableBrowser : tableBrowsers) {
			if (tableBrowser.internalFrame.isMaximum()) {
				changed = renderLinks;
				renderLinks = false;
				return changed;
			}
			if (tableBrowser.parent != null) {
				int x1 = tableBrowser.internalFrame.getX() + tableBrowser.internalFrame.getWidth() / 2;
				int y1 = tableBrowser.internalFrame.getY() + tableBrowser.internalFrame.getHeight() / 2;
				int midx = tableBrowser.parent.internalFrame.getX() + tableBrowser.parent.internalFrame.getWidth() / 2;

				int i = tableBrowser.parent.browserContentPane.rowsTable.getRowSorter().convertRowIndexToView(tableBrowser.rowIndex);

				Rectangle cellRect = tableBrowser.parent.browserContentPane.rowsTable.getCellRect(i, 0, true);
				int x2 = tableBrowser.parent.internalFrame.getX();
				int y = cellRect.y;
				y = cellRect.height * i;
				int y2 = tableBrowser.parent.internalFrame.getY() + y + cellRect.height / 2;
				if (midx < x1) {
					x2 += tableBrowser.parent.internalFrame.getWidth();
				}
				Container p = tableBrowser.parent.browserContentPane.rowsTable;
				while (p != tableBrowser.parent.internalFrame) {
					y2 += p.getY();
					p = p.getParent();
				}
				int min = tableBrowser.parent.internalFrame.getY() + cellRect.height;
				if (y2 < min) {
					y2 = min;
				}
				int max = tableBrowser.parent.internalFrame.getY() + tableBrowser.parent.internalFrame.getHeight();
				if (y2 > max) {
					y2 = max;
				}
				if (x1 != tableBrowser.x1 || y1 != tableBrowser.y1 || x2 != tableBrowser.x2 || y2 != tableBrowser.y2) {
					changed = true;
					tableBrowser.x1 = x1;
					tableBrowser.y1 = y1;
					tableBrowser.x2 = x2;
					tableBrowser.y2 = y2;
				}
			}
		}
		if (!renderLinks) {
			changed = true;
		}
		renderLinks = true;
		return changed;
	}

	/**
	 * Paints all link-renders.
	 */
	@Override
	public synchronized void paint(Graphics graphics) {
		super.paint(graphics);
		if (graphics instanceof Graphics2D) {
			Graphics2D g2d = (Graphics2D) graphics;
			if (renderLinks) {
				for (RowBrowser tableBrowser : tableBrowsers) {
					if (tableBrowser.parent != null) {
						if (tableBrowser.internalFrame.isIcon() || tableBrowser.parent.internalFrame.isIcon()) {
							continue;
						}
						Color color = tableBrowser.color;
						g2d.setColor(color);
						g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
						g2d.setStroke(new BasicStroke(3));

						Point2D start = new Point2D.Double(tableBrowser.x2, tableBrowser.y2);
						Point2D end = new Point2D.Double(tableBrowser.x1, tableBrowser.y1);

						AffineTransform t = new AffineTransform();
						t.setToRotation(Math.PI / 4);
						Point2D p = new Point2D.Double(), shift = new Point2D.Double();
						double d = start.distance(end) / 5.0;
						p.setLocation((end.getX() - start.getX()) / d, (end.getY() - start.getY()) / d);
						t.transform(p, shift);
						start.setLocation(start.getX() + shift.getX(), start.getY() + shift.getY());
						end.setLocation(end.getX() + shift.getX(), end.getY() + shift.getY());

						// compute the intersection with the target bounding box
						Point2D[] sect = new Point2D[10];
						int i = GraphicsLib.intersectLineRectangle(start, end, tableBrowser.internalFrame.getBounds(), sect);
						if (i == 0)
							continue;
						end = sect[0];
						if (start.distance(end) < 2)
							continue;

						// create the arrow head shape
						if (m_arrowHead == null) {
							m_arrowHead = new Polygon();
							double ws = 0.9;
							double hs = 2.0 / 3.0;
							double w = 3, h = 3;
							m_arrowHead.addPoint(0, 0);
							m_arrowHead.addPoint((int) (ws * -w), (int) (hs * (-h)));
							// m_arrowHead.addPoint(0, (int) (hs * (-2 * h)));
							m_arrowHead.addPoint((int) (ws * w), (int) (hs * (-h)));
							m_arrowHead.addPoint(0, 0);
						}

						AffineTransform at = getArrowTrans(start, end, 10);
						Shape m_curArrow = at.createTransformedShape(m_arrowHead);

						Point2D lineEnd = end;
						lineEnd.setLocation(0, -2);
						at.transform(lineEnd, lineEnd);

						g2d.drawLine((int) start.getX(), (int) start.getY(), (int) end.getX(), (int) end.getY());
						g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
						g2d.setStroke(new BasicStroke(1));
						g2d.fill(m_curArrow);
					}
				}
			}
		}
	}

	private Polygon m_arrowHead;

	/**
	 * Returns an affine transformation that maps the arrowhead shape to the
	 * position and orientation specified by the provided line segment end
	 * points.
	 */
	protected AffineTransform getArrowTrans(Point2D p1, Point2D p2, double width) {
		AffineTransform m_arrowTrans = new AffineTransform();
		m_arrowTrans.setToTranslation(p2.getX(), p2.getY());
		m_arrowTrans.rotate(-Math.PI / 2.0 + Math.atan2(p2.getY() - p1.getY(), p2.getX() - p1.getX()));
		if (width > 1) {
			double scalar = width / 2;
			m_arrowTrans.scale(scalar, scalar);
		}
		return m_arrowTrans;
	}

	private static int FRAME_OFFSET = 20;
	private MDIDesktopManager manager;

	public void setBounds(int x, int y, int w, int h) {
		super.setBounds(x, y, w, h);
		checkDesktopSize();
	}

	public Component add(JInternalFrame frame) {
		JInternalFrame[] array = getAllFrames();
		Point p;
		int w;
		int h;

		Component retval = super.add(frame);
		checkDesktopSize();
		if (array.length > 0) {
			p = array[0].getLocation();
			p.x = p.x + FRAME_OFFSET;
			p.y = p.y + FRAME_OFFSET;
		} else {
			p = new Point(0, 0);
		}
		frame.setLocation(p.x, p.y);
		if (frame.isResizable()) {
			w = getWidth() - (getWidth() / 3);
			h = getHeight() - (getHeight() / 3);
			if (w < frame.getMinimumSize().getWidth())
				w = (int) frame.getMinimumSize().getWidth();
			if (h < frame.getMinimumSize().getHeight())
				h = (int) frame.getMinimumSize().getHeight();
			frame.setSize(w, h);
		}
		moveToFront(frame);
		frame.setVisible(true);
		try {
			frame.setSelected(true);
		} catch (PropertyVetoException e) {
			frame.toBack();
		}
		
		return retval;
	}

	public void remove(Component c) {
		super.remove(c);
		checkDesktopSize();
	}

	/**
	 * Cascade all internal frames
	 */
	public void cascadeFrames() {
		int x = 0;
		int y = 0;
		JInternalFrame allFrames[] = getAllFrames();

		manager.setNormalSize();
		int frameHeight = (getBounds().height - 5) - allFrames.length * FRAME_OFFSET;
		int frameWidth = (getBounds().width - 5) - allFrames.length * FRAME_OFFSET;
		for (int i = allFrames.length - 1; i >= 0; i--) {
			allFrames[i].setSize(frameWidth, frameHeight);
			allFrames[i].setLocation(x, y);
			x = x + FRAME_OFFSET;
			y = y + FRAME_OFFSET;
		}
	}

	/**
	 * Tile all internal frames
	 */
	public void tileFrames() {
		java.awt.Component allFrames[] = getAllFrames();
		manager.setNormalSize();
		int frameHeight = getBounds().height / allFrames.length;
		int y = 0;
		for (int i = 0; i < allFrames.length; i++) {
			allFrames[i].setSize(getBounds().width, frameHeight);
			allFrames[i].setLocation(0, y);
			y = y + frameHeight;
		}
	}

	/**
	 * Sets all component size properties ( maximum, minimum, preferred) to the
	 * given dimension.
	 */
	public void setAllSize(Dimension d) {
		setMinimumSize(d);
		setMaximumSize(d);
		setPreferredSize(d);
	}

	/**
	 * Sets all component size properties ( maximum, minimum, preferred) to the
	 * given width and height.
	 */
	public void setAllSize(int width, int height) {
		setAllSize(new Dimension(width, height));
	}

	private void checkDesktopSize() {
		if (getParent() != null && isVisible())
			manager.resizeDesktop();
	}

	private JScrollPane getScrollPane() {
		if (getParent() instanceof JViewport) {
			JViewport viewPort = (JViewport) getParent();
			if (viewPort.getParent() instanceof JScrollPane)
				return (JScrollPane) viewPort.getParent();
		}
		return null;
	}

	/**
	 * Private class used to replace the standard DesktopManager for
	 * JDesktopPane. Used to provide scrollbar functionality.
	 */
	class MDIDesktopManager extends DefaultDesktopManager {
		private Desktop desktop;

		public MDIDesktopManager(Desktop desktop) {
			this.desktop = desktop;
		}

		public void endResizingFrame(JComponent f) {
			super.endResizingFrame(f);
			resizeDesktop();
		}

		public void endDraggingFrame(JComponent f) {
			super.endDraggingFrame(f);
			resizeDesktop();
		}

		public void setNormalSize() {
			JScrollPane scrollPane = getScrollPane();
			int x = 0;
			int y = 0;
			Insets scrollInsets = getScrollPaneInsets();

			if (scrollPane != null) {
				Dimension d = scrollPane.getVisibleRect().getSize();
				if (scrollPane.getBorder() != null) {
					d.setSize(d.getWidth() - scrollInsets.left - scrollInsets.right, d.getHeight() - scrollInsets.top - scrollInsets.bottom);
				}

				d.setSize(d.getWidth() - 20, d.getHeight() - 20);
				desktop.setAllSize(x, y);
				scrollPane.invalidate();
				scrollPane.validate();
			}
		}

		private Insets getScrollPaneInsets() {
			JScrollPane scrollPane = getScrollPane();
			if (scrollPane == null)
				return new Insets(0, 0, 0, 0);
			else
				return getScrollPane().getBorder().getBorderInsets(scrollPane);
		}

		private JScrollPane getScrollPane() {
			if (desktop.getParent() instanceof JViewport) {
				JViewport viewPort = (JViewport) desktop.getParent();
				if (viewPort.getParent() instanceof JScrollPane)
					return (JScrollPane) viewPort.getParent();
			}
			return null;
		}

		protected void resizeDesktop() {
			int x = 0;
			int y = 0;
			JScrollPane scrollPane = getScrollPane();
			Insets scrollInsets = getScrollPaneInsets();

			if (scrollPane != null) {
				JInternalFrame allFrames[] = desktop.getAllFrames();
				for (int i = 0; i < allFrames.length; i++) {
					if (allFrames[i].getX() + allFrames[i].getWidth() > x) {
						x = allFrames[i].getX() + allFrames[i].getWidth();
					}
					if (allFrames[i].getY() + allFrames[i].getHeight() > y) {
						y = allFrames[i].getY() + allFrames[i].getHeight();
					}
				}
				Dimension d = scrollPane.getVisibleRect().getSize();
				if (scrollPane.getBorder() != null) {
					d.setSize(d.getWidth() - scrollInsets.left - scrollInsets.right, d.getHeight() - scrollInsets.top - scrollInsets.bottom);
				}

				if (x <= d.getWidth())
					x = ((int) d.getWidth()) - 20;
				if (y <= d.getHeight())
					y = ((int) d.getHeight()) - 20;
				desktop.setAllSize(x, y);
				scrollPane.invalidate();
				scrollPane.validate();
			}
		}
	}

	public synchronized void stop() {
		running = false;
		try {
			session.shutDown();
		} catch (SQLException e) {
			// exception already has been logged
		}
	}

	private final Frame parentFrame;
	
}
