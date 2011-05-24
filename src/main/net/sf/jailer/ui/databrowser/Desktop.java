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
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyVetoException;
import java.lang.reflect.InvocationTargetException;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.swing.DefaultDesktopManager;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JDesktopPane;
import javax.swing.JFrame;
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
import net.sf.jailer.ui.ConditionEditor;
import net.sf.jailer.ui.QueryBuilderDialog;
import net.sf.jailer.ui.UIUtil;
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
	private final Reference<DataModel> datamodel;
	
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
	public Session session;
	
	private Set<Row> currentClosure = new HashSet<Row>();
	
	private final QueryBuilderDialog queryBuilderDialog;
		
	/**
	 * Constructor.
	 * 
	 * @param datamodel the {@link DataModel}
	 * @param jailerIcon icon for the frames
	 * @param session DB-session
	 */
	public Desktop(Reference<DataModel> datamodel, Icon jailerIcon, Session session, JFrame parentFrame) {
		this.parentFrame = parentFrame;
		this.datamodel = datamodel;
		this.jailerIcon = jailerIcon;
		this.queryBuilderDialog = new QueryBuilderDialog(parentFrame);

		try {
			this.session = session;
			// trigger sql dialect guessing
			datamodel.get().getUniversalPrimaryKey(session);
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

	public class RowToRowLink {
		
		/**
		 * The rows.
		 */
		public Row parentRow, childRow;
		
		/**
		 * Index of parent row in the parent's row browser.
		 */
		public int parentRowIndex = -1;
		
		/**
		 * Index of child row.
		 */
		public int childRowIndex = -1;
		
		/**
		 * Coordinates of the link render.
		 */
		public int x1 = -1, y1, x2, y2;
		
		/**
		 * The link's color.
		 */
		public Color color; 
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
		 * Association with parent.
		 */
		public Association association;
		
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
		
		/**
		 * Row-to-row links.
		 */
		public List<RowToRowLink> rowToRowLinks = new ArrayList<RowToRowLink>();
		
	};

	/**
	 * All row-browsers.
	 */
	private List<RowBrowser> tableBrowsers = new ArrayList<RowBrowser>();

	/**
	 * Opens a new row-browser.
	 * 
	 * @param parent parent browser
	 * @param parentRowIndex index of parent row in the parent's row browser, -1 for all rows
	 * @param table to read rows from
	 * @param association to navigate, or <code>null</code>
	 * @param condition 
	 * @return new row-browser
	 */
	public synchronized RowBrowser addTableBrowser(final RowBrowser parent, int parentRowIndex, final Table table, final Association association, String condition) {
		for (RowBrowser rb: tableBrowsers) {
			try {
				rb.internalFrame.setMaximum(false);
			} catch (PropertyVetoException e) {
				// ignore
			}
		}
		
		JInternalFrame jInternalFrame = new JInternalFrame(datamodel.get().getDisplayName(table));
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
 		
		jInternalFrame.addPropertyChangeListener(JInternalFrame.IS_MAXIMUM_PROPERTY, new PropertyChangeListener() {
			@Override
			public void propertyChange(PropertyChangeEvent evt) {
				manager.resizeDesktop();
			}
		});
		
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

		final RowBrowser tableBrowser = new RowBrowser();
		BrowserContentPane browserContentPane = new BrowserContentPane(datamodel.get(), table, condition, session, parent == null || parentRowIndex < 0? null : parent.browserContentPane.rows.get(parentRowIndex), parent == null || parentRowIndex >= 0? null : parent.browserContentPane.rows, association, parentFrame, currentClosure) {
			
			@Override
			protected QueryBuilderDialog getQueryBuilderDialog() {
				return queryBuilderDialog;
			}

			@Override
			protected void navigateTo(Association association, int rowIndex, Row row) {
				addTableBrowser(tableBrowser, rowIndex, association.destination, association, "");
			}

			@Override
			protected void onContentChange(List<Row> rows) {
				updateChildren(tableBrowser, rows);
			}

			@Override
			protected void onRedraw() {
				repaintDesktop();
			}

			@Override
			protected JFrame getOwner() {
				return parentFrame;
			}

			@Override
			protected void addRowToRowLink(Row parentRow, Row childRow) {
				synchronized (Desktop.this) {
					RowToRowLink rowToRowLink = new RowToRowLink();
					rowToRowLink.parentRow = parentRow;
					rowToRowLink.childRow = childRow;
					rowToRowLink.color = getAssociationColor(association);
					tableBrowser.rowToRowLinks.add(rowToRowLink);
				}
			}

			@Override
			protected void beforeReload() {
				synchronized (Desktop.this) {
					tableBrowser.rowToRowLinks.clear();
				}
			}

			@Override
			protected void findClosure(Row row) {
				if (!currentClosure.contains(row)) {
					currentClosure.add(row);
					for (RowBrowser child: tableBrowsers) {
						if (child.parent == tableBrowser) {
							for (RowToRowLink rowToRowLink: child.rowToRowLinks) {
								if (row.rowId.equals(rowToRowLink.parentRow.rowId)) {
									child.browserContentPane.findClosure(rowToRowLink.childRow);
								}
							}
						}
					}
				}
			}

			@Override
			protected void createAssociationList(List<Association> associations, List<String> whereClauses) {
				for (RowBrowser rb = tableBrowser; rb != null; rb = rb.parent) {
					String andC = ConditionEditor.toMultiLine(rb.browserContentPane.andCondition.getText().trim()).replaceAll("(\r|\n)+", " ");
					if (rb.association != null) {
						associations.add(rb.association.reversalAssociation);
					} else {
						whereClauses.add(andC.length() == 0? null : andC);
						break;
					}
					if (rb.rowIndex >= 0 && !(rb.rowIndex == 0 && rb.parent != null && rb.parent.browserContentPane != null && rb.parent.browserContentPane.rows != null && rb.parent.browserContentPane.rows.size() == 1)) {
						String w = rb.browserContentPane.parentRow.rowId;
						whereClauses.add(andC.length() == 0? w : "(" + w + ") and (" + andC + ")");
						break;
					}
					whereClauses.add(andC.length() == 0? null : andC);
				}
			}
		};
		
		Rectangle r = layout(parentRowIndex < 0, parent, association, browserContentPane, new ArrayList<RowBrowser>());

		jInternalFrame.setBounds(r);
		
		tableBrowser.internalFrame = jInternalFrame;
		tableBrowser.browserContentPane = browserContentPane;
		tableBrowser.rowIndex = parentRowIndex;
		tableBrowser.parent = parent;
		tableBrowser.association = association;
		if (association != null) {
			tableBrowser.color = getAssociationColor(association);
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
				close(tableBrowser);
			}
			@Override
			public void internalFrameActivated(InternalFrameEvent e) {
			}
		});

		checkDesktopSize();
		this.scrollRectToVisible(jInternalFrame.getBounds());
		jInternalFrame.toFront();
		browserContentPane.andCondition.grabFocus();
		return tableBrowser;
	}

	private Color getAssociationColor(Association association) {
		Color color = new java.awt.Color(0, 100, 255);
		if (association.isInsertDestinationBeforeSource()) {
			color = new java.awt.Color(170, 0, 0);
		}
		if (association.isInsertSourceBeforeDestination()) {
			color = new java.awt.Color(0, 112, 0);
		}
		if (association.isIgnored()) {
			color = new java.awt.Color(153, 153, 153);
		}
		return color;
	}

	private Rectangle layout(final boolean fullSize, final RowBrowser parent, Association association, BrowserContentPane browserContentPane, Collection<RowBrowser> ignore) {
		final int MIN = 0, HEIGHT = 460, MIN_HEIGHT = 80, DISTANCE = 32;

		int x = MIN;
		if (parent != null) {
			x = parent.internalFrame.getX() + parent.internalFrame.getWidth() + DISTANCE;
		}
		// int h = fullSize || association == null || (association.getCardinality() != Cardinality.MANY_TO_ONE && association.getCardinality() != Cardinality.ONE_TO_ONE)? HEIGHT : browserContentPane.getMinimumSize().height + MIN_HEIGHT; 
		int h = HEIGHT; 
		int y = MIN;
		Rectangle r = new Rectangle(x, y, BROWSERTABLE_DEFAULT_WIDTH, h);
		for (;;) {
			boolean ok = true;
			for (RowBrowser tb : tableBrowsers) {
				if (!ignore.contains(tb) && tb.internalFrame.getBounds().intersects(r)) {
					ok = false;
					break;
				}
			}
			r = new Rectangle(x, y, BROWSERTABLE_DEFAULT_WIDTH, h);
			y += 8;
			if (ok) {
				break;
			}
		}
		return r;
	}

	protected synchronized void updateChildren(RowBrowser tableBrowser, List<Row> rows) {
		boolean hasParent = false;
		tableBrowser.browserContentPane.highlightedRows.clear();
		for (RowBrowser rowBrowser: tableBrowsers) {
			if (rowBrowser == tableBrowser.parent) {
				hasParent = true;
			}
			if (rowBrowser.parent == tableBrowser) {
				rowBrowser.rowIndex = -1;
				for (int i = 0; i < rows.size(); ++i) {
					if (rowBrowser.browserContentPane.parentRow != null && rowBrowser.browserContentPane.parentRow.rowId.equals(rows.get(i).rowId)) {
						rowBrowser.rowIndex = i;
						tableBrowser.browserContentPane.highlightedRows.add(i);
						break;
					}
				}
			}
		}
		if (!hasParent) {
			tableBrowser.rowToRowLinks.clear();
		} else {
			for (RowToRowLink rowToRowLink: tableBrowser.rowToRowLinks) {
				rowToRowLink.childRowIndex = -1;
				for (int i = 0; i < rows.size(); ++i) {
					if (rowToRowLink.childRow.rowId.equals(rows.get(i).rowId)) {
						rowToRowLink.childRowIndex = i;
						break;
					}
				}
				rowToRowLink.parentRowIndex = -1;
				List<Row> parentRows = tableBrowser.parent.browserContentPane.rows;
				for (int i = 0; i < parentRows.size(); ++i) {
					if (rowToRowLink.parentRow.rowId.equals(parentRows.get(i).rowId)) {
						rowToRowLink.parentRowIndex = i;
						break;
					}
				}
			}
		}
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
				int BORDER = 12;
				int x1 = tableBrowser.internalFrame.getX() + tableBrowser.internalFrame.getWidth() / 2;
				int y1 = tableBrowser.internalFrame.getY() + tableBrowser.internalFrame.getHeight() / 2;
				int midx = tableBrowser.parent.internalFrame.getX() + tableBrowser.parent.internalFrame.getWidth() / 2;

				Rectangle cellRect = new Rectangle();
				int i = 0;
				if (tableBrowser.rowIndex >= 0) {
					i = tableBrowser.parent.browserContentPane.rowsTable.getRowSorter().convertRowIndexToView(tableBrowser.rowIndex);
					cellRect = tableBrowser.parent.browserContentPane.rowsTable.getCellRect(i, 0, true);
				}

				int x2 = tableBrowser.parent.internalFrame.getX();
				int y = cellRect.y;
				y = cellRect.height * i;
				int y2 = tableBrowser.parent.internalFrame.getY() + y + Math.min(cellRect.height / 2, 100);
				if (midx < x1) {
					x2 += tableBrowser.parent.internalFrame.getWidth() - BORDER;
				} else {
					x2 += BORDER;
				}
				Container p = tableBrowser.parent.browserContentPane.rowsTable;
				while (p != tableBrowser.parent.internalFrame) {
					y2 += p.getY();
					p = p.getParent();
				}
				int min = tableBrowser.parent.internalFrame.getY() + Math.min(cellRect.height, 20);
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
				
				for (RowToRowLink rowToRowLink: tableBrowser.rowToRowLinks) {
					x1 = y1 = x2 = y2 = -1;
					try {
						if (rowToRowLink.childRowIndex >= 0 && rowToRowLink.parentRowIndex >= 0) {
							cellRect = new Rectangle();
							i = 0;
							int dll = Math.abs(tableBrowser.parent.internalFrame.getX() - tableBrowser.internalFrame.getX());
							int dlr = Math.abs(tableBrowser.parent.internalFrame.getX() - (tableBrowser.internalFrame.getX() + tableBrowser.internalFrame.getWidth()));
							int drl = Math.abs((tableBrowser.parent.internalFrame.getX() + tableBrowser.parent.internalFrame.getWidth()) - tableBrowser.internalFrame.getX());
							int drr = Math.abs((tableBrowser.parent.internalFrame.getX() + tableBrowser.parent.internalFrame.getWidth()) - (tableBrowser.internalFrame.getX() + tableBrowser.internalFrame.getWidth()));
							
							boolean r1, r2;
							int dmin = Math.min(dll, Math.min(dlr, Math.min(drl, drr)));
							r2 = dmin == drl || dmin == drr;
							r1 = dmin == dlr || dmin == drr;
							
							if (rowToRowLink.childRowIndex >= 0) {
								i = tableBrowser.browserContentPane.rowsTable.getRowSorter().convertRowIndexToView(rowToRowLink.childRowIndex);
								cellRect = tableBrowser.browserContentPane.rowsTable.getCellRect(i, 0, true);
							}
		
							x1 = tableBrowser.internalFrame.getX();
							y = cellRect.height * i;
							y1 = tableBrowser.internalFrame.getY() + y + cellRect.height / 2;
							if (r1) {
								x1 += tableBrowser.internalFrame.getWidth()- BORDER;
							} else {
								x1 += BORDER;
							}
							p = tableBrowser.browserContentPane.rowsTable;
							while (p != tableBrowser.internalFrame) {
								y1 += p.getY();
								p = p.getParent();
							}
							min = tableBrowser.internalFrame.getY() + cellRect.height;
							if (y1 < min) {
								y1 = min;
							}
							max = tableBrowser.internalFrame.getY() + tableBrowser.internalFrame.getHeight();
							if (y1 > max) {
								y1 = max;
							}
							
							cellRect = new Rectangle();
							i = 0;
							if (rowToRowLink.parentRowIndex >= 0) {
								i = tableBrowser.parent.browserContentPane.rowsTable.getRowSorter().convertRowIndexToView(rowToRowLink.parentRowIndex);
								cellRect = tableBrowser.parent.browserContentPane.rowsTable.getCellRect(i, 0, true);
							}
		
							x2 = tableBrowser.parent.internalFrame.getX();
							y = cellRect.height * i;
							y2 = tableBrowser.parent.internalFrame.getY() + y + cellRect.height / 2;
							if (r2) {
								x2 += tableBrowser.parent.internalFrame.getWidth() - BORDER;
							} else {
								x2 += BORDER;
							}
							p = tableBrowser.parent.browserContentPane.rowsTable;
							while (p != tableBrowser.parent.internalFrame) {
								y2 += p.getY();
								p = p.getParent();
							}
							min = tableBrowser.parent.internalFrame.getY() + cellRect.height;
							if (y2 < min) {
								y2 = min;
							}
							max = tableBrowser.parent.internalFrame.getY() + tableBrowser.parent.internalFrame.getHeight();
							if (y2 > max) {
								y2 = max;
							}
						}
						if (x1 != rowToRowLink.x1 || y1 != rowToRowLink.y1 || x2 != rowToRowLink.x2 || y2 != rowToRowLink.y2) {
							changed = true;
							rowToRowLink.x1 = x1;
							rowToRowLink.y1 = y1;
							rowToRowLink.x2 = x2;
							rowToRowLink.y2 = y2;
						}
					} catch (Exception e) {
						// ignore
					}
				}
			}
		}
		if (!renderLinks) {
			changed = true;
		}
		renderLinks = true;
		if (lastPTS + 1000 < System.currentTimeMillis()) {
			changed = true;
		}
		if (changed) {
			lastPTS = System.currentTimeMillis();
		}
		return changed;
	}

	private long lastPTS = 0;
	
	/**
	 * Paints all link-renders.
	 */
	@Override
	public synchronized void paint(Graphics graphics) {
		super.paint(graphics);
		if (graphics instanceof Graphics2D) {
			Graphics2D g2d = (Graphics2D) graphics;
			if (renderLinks) {
				for (boolean pbg: new Boolean[] { true, false}) {
					for (RowBrowser tableBrowser : tableBrowsers) {
						if (!tableBrowser.internalFrame.isIcon() && (tableBrowser.parent == null || !tableBrowser.parent.internalFrame.isIcon())) {
							Color color = pbg? Color.white : tableBrowser.color;
							if (tableBrowser.parent != null && tableBrowser.rowIndex >= 0) {
								Point2D start = new Point2D.Double(tableBrowser.x2, tableBrowser.y2);
								Point2D end = new Point2D.Double(tableBrowser.x1, tableBrowser.y1);
								paintLink(start, end, color, g2d, tableBrowser, pbg, true);
							}
							for (RowToRowLink rowToRowLink: tableBrowser.rowToRowLinks) {
								if (rowToRowLink.x1 >= 0) {
									paintLink(new Point2D.Double(rowToRowLink.x2, rowToRowLink.y2), new Point2D.Double(rowToRowLink.x1, rowToRowLink.y1), color, g2d, tableBrowser, pbg, false);
								}
							}
						}
					}
				}
			}
		}
	}
	
	private void paintLink(Point2D start, Point2D end, Color color, Graphics2D g2d, RowBrowser tableBrowser, boolean pbg, boolean intersect) {
		g2d.setColor(color);
		g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
		g2d.setStroke(new BasicStroke(!intersect? (pbg? 3 : 1) : (pbg? 5 : 3)));

		AffineTransform t = new AffineTransform();
		t.setToRotation(Math.PI / 4);
		Point2D p = new Point2D.Double(), shift = new Point2D.Double();
		double d = start.distance(end) / 3.0;
		p.setLocation((end.getX() - start.getX()) / d, (end.getY() - start.getY()) / d);
		t.transform(p, shift);
		start.setLocation(start.getX() + shift.getX(), start.getY() + shift.getY());
		end.setLocation(end.getX() + shift.getX(), end.getY() + shift.getY());

		// compute the intersection with the target bounding box
		if (intersect) {
			Point2D[] sect = new Point2D[10];
			int i = GraphicsLib.intersectLineRectangle(start, end, tableBrowser.internalFrame.getBounds(), sect);
			if (i == 0)
				return;
			end = sect[0];
		}
		if (start.distance(end) < 2)
			return;

		// create the arrow head shape
		m_arrowHead = new Polygon();
		double ws = 0.5;
		double hs = 2.0 / 3.0;
		double w = !intersect? (pbg? 3 : 3) : (pbg? 3 : 3), h = w;
		m_arrowHead.addPoint(0, 0);
		m_arrowHead.addPoint((int) (ws * -w), (int) (hs * (-h)));
		// m_arrowHead.addPoint(0, (int) (hs * (-2 * h)));
		m_arrowHead.addPoint((int) (ws * w), (int) (hs * (-h)));
		m_arrowHead.addPoint(0, 0);

		AffineTransform at = getArrowTrans(start, end, 10);
		Shape m_curArrow = at.createTransformedShape(m_arrowHead);

		Point2D lineEnd = end;
		lineEnd.setLocation(0, -2);
		at.transform(lineEnd, lineEnd);

		g2d.drawLine((int) start.getX(), (int) start.getY(), (int) end.getX(), (int) end.getY());
		g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
		g2d.setStroke(new BasicStroke(1));
		g2d.fill(m_curArrow);
		if (pbg) {
			g2d.draw(m_curArrow);
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

		public void resizeDesktop() {
			int x = 0;
			int y = 0;
			JScrollPane scrollPane = getScrollPane();
			Insets scrollInsets = getScrollPaneInsets();

			if (scrollPane != null) {
				boolean isMaximized = false;
				JInternalFrame allFrames[] = desktop.getAllFrames();
				for (int i = 0; i < allFrames.length; i++) {
					if (allFrames[i].isMaximum()) {
						isMaximized = true;
					}
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

				if (x <= d.getWidth() || isMaximized)
					x = ((int) d.getWidth()) - 20;
				if (y <= d.getHeight() || isMaximized)
					y = ((int) d.getHeight()) - 20;
				desktop.setAllSize(x, y);
				scrollPane.invalidate();
				scrollPane.validate();
			}
		}
	}

	public synchronized void stop() {
		running = false;
		for (RowBrowser rb: tableBrowsers) {
			rb.browserContentPane.cancelLoadJob();
		}
		try {
			session.shutDown();
		} catch (SQLException e) {
			// exception already has been logged
		}
	}

	private final JFrame parentFrame;

	public void layoutBrowser() {
		List<RowBrowser> all = new ArrayList<RowBrowser>(tableBrowsers);
		List<RowBrowser> column = new ArrayList<RowBrowser>();
		for (RowBrowser rb: all) {
			if (rb.parent == null) {
				column.add(rb);
			}
		}
		while (!column.isEmpty()) {
			List<RowBrowser> nextColumn = new ArrayList<RowBrowser>();
			for (RowBrowser rb: column) {
				try {
					rb.internalFrame.setMaximum(false);
				} catch (PropertyVetoException e) {
					// ignore
				}
				rb.internalFrame.setBounds(layout(rb.rowIndex < 0, rb.parent, rb.association, rb.browserContentPane, all));
				all.remove(rb);
				for (RowBrowser rbc: all) {
					if (rbc.parent == rb) {
						nextColumn.add(rbc);
					}
				}
			}
			column = nextColumn;
		}
	}

	public void closeAll() {
		for (RowBrowser rb: new ArrayList<RowBrowser>(tableBrowsers)) {
			close(rb);
			getDesktopManager().closeFrame(rb.internalFrame);
		}
	}

	private void close(final RowBrowser tableBrowser) {
		for (RowBrowser tb: tableBrowsers) {
			if (tb.parent == tableBrowser) {
				tb.parent = null;
			}
		}
		tableBrowsers.remove(tableBrowser);
		tableBrowser.browserContentPane.cancelLoadJob();
		for (RowBrowser rb: tableBrowsers) {
			updateChildren(rb, rb.browserContentPane.rows);
		}
		repaintDesktop();
	}
	
}
