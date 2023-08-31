/*
 * Copyright 2007 - 2023 Ralf Wisser.
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
import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsEnvironment;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelEvent;
import java.awt.geom.AffineTransform;
import java.awt.geom.Path2D;
import java.awt.geom.Point2D;
import java.awt.image.BufferedImage;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyVetoException;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.swing.AbstractAction;
import javax.swing.ActionMap;
import javax.swing.DefaultDesktopManager;
import javax.swing.Icon;
import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JDesktopPane;
import javax.swing.JFrame;
import javax.swing.JInternalFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JTable;
import javax.swing.JViewport;
import javax.swing.KeyStroke;
import javax.swing.RowSorter.SortKey;
import javax.swing.SortOrder;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.WindowConstants;
import javax.swing.event.InternalFrameEvent;
import javax.swing.event.InternalFrameListener;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.modelbuilder.KnownIdentifierMap;
import net.sf.jailer.ui.DbConnectionDialog;
import net.sf.jailer.ui.Environment;
import net.sf.jailer.ui.QueryBuilderDialog;
import net.sf.jailer.ui.QueryBuilderDialog.Relationship;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.UIUtil.PLAF;
import net.sf.jailer.ui.databrowser.BrowserContentPane.RowsClosure;
import net.sf.jailer.ui.databrowser.BrowserContentPane.SqlStatementTable;
import net.sf.jailer.ui.databrowser.BrowserContentPane.UserAction;
import net.sf.jailer.ui.databrowser.TreeLayoutOptimizer.Node;
import net.sf.jailer.ui.databrowser.metadata.MDTable;
import net.sf.jailer.ui.databrowser.metadata.MetaDataSource;
import net.sf.jailer.ui.databrowser.sqlconsole.SQLConsole;
import net.sf.jailer.ui.databrowser.whereconditioneditor.WhereConditionEditorPanel;
import net.sf.jailer.ui.util.HSLColor;
import net.sf.jailer.ui.util.UISettings;
import net.sf.jailer.util.CancellationException;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.CsvFile.Line;
import net.sf.jailer.util.Pair;
import net.sf.jailer.util.SqlUtil;
import prefuse.util.GraphicsLib;

/**
 * Desktop holding row-browsers as {@link JInternalFrame}s.
 * 
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public abstract class Desktop extends JDesktopPane {

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
	private final int BROWSERTABLE_DEFAULT_MIN_X = 0;
	private final int BROWSERTABLE_DEFAULT_MIN_Y = 0;

	public static final int BROWSERTABLE_DEFAULT_HEIGHT = 460;
	public static final int BROWSERTABLE_DEFAULT_WIDTH = 476;
	
	final int BROWSERTABLE_DEFAULT_DISTANCE = 110;

	/**
	 * <code>true</code> while the desktop is visible.
	 */
	private boolean running;

	/**
	 * <code>false</code> if links must not be rendered (if a frame is
	 * maximized).
	 */
	private boolean renderLinks;

	/**
	 * Schema mapping.
	 */
	public final Map<String, String> schemaMapping;

	/**
	 * DB session.
	 */
	public Session session;
	DbConnectionDialog dbConnectionDialog;
	
	/**
	 * The execution context.
	 */
	private final ExecutionContext executionContext;
	
	private RowsClosure rowsClosure = new RowsClosure();

	final DesktopAnimation desktopAnimation;
	AtomicInteger animationStepTime = new AtomicInteger(0);
	
	double getAnimationFactor() {
		double h = 0.7;
		double l = 0.25;
		return Math.max(l, (h - (h - l) * animationStepTime.get() / 300.0));
	}
	
	private final QueryBuilderDialog queryBuilderDialog;
	private final DesktopIFrameStateChangeRenderer iFrameStateChangeRenderer = new DesktopIFrameStateChangeRenderer();
	private final DesktopAnchorManager anchorManager;
	
	private static final KeyStroke KS_SQLCONSOLE = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, InputEvent.CTRL_DOWN_MASK);
	
	public DesktopIFrameStateChangeRenderer getiFrameStateChangeRenderer() {
		return iFrameStateChangeRenderer;
	}

	private DesktopUndoManager desktopUndoManager;
	
	public void setUndoManager(DesktopUndoManager desktopUndoManager) {
		this.desktopUndoManager = desktopUndoManager;
	}

	public DesktopUndoManager getUndoManager() {
		return desktopUndoManager;
	}

	
	/**
	 * Constructor.
	 * 
	 * @param datamodel
	 *            the {@link DataModel}
	 * @param jailerIcon
	 *            icon for the frames
	 * @param session
	 *            DB-session
	 * @param anchorManager 
	 */
	public Desktop(Reference<DataModel> datamodel, Icon jailerIcon, Session session, DataBrowser parentFrame, DbConnectionDialog dbConnectionDialog, Map<String, String> schemaMapping, DesktopAnchorManager anchorManager, ExecutionContext executionContext) {
		this.executionContext = executionContext;
		this.anchorManager = anchorManager;
		this.parentFrame = parentFrame;
		this.datamodel = datamodel;
		this.jailerIcon = jailerIcon;
		this.queryBuilderDialog = new QueryBuilderDialog(parentFrame);
		this.dbConnectionDialog = dbConnectionDialog;
		this.schemaMapping = schemaMapping;

		this.desktopAnimation = new DesktopAnimation(this);
		
		this.queryBuilderDialog.sqlEditButton.setVisible(true);
		this.queryBuilderDialog.sqlEditButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				// addTableBrowser(null, null, 0, null, null, queryBuilderDialog.getSQL(), null, null, true);
				SQLConsole sqlConsole = getSqlConsole(true);
				UIUtil.invokeLater(16, () -> sqlConsole.appendStatement(queryBuilderDialog.getSQL() + LF + ";", true));
				queryBuilderDialog.setVisible(false);
			}
		});
		if (Toolkit.getDefaultToolkit().getScreenSize().height < 740) {
			layoutMode = LayoutMode.SMALL;
		}
		
		try {
			this.session = session;
			setAutoscrolls(true);
			manager = new MDIDesktopManager(this);
			setDesktopManager(manager);
			synchronized (this) {
				running = true;
			}
			Thread updateUIThread = new Thread(new Runnable() {
				@Override
				public void run() {
					final AtomicLong duration = new AtomicLong();
					final AtomicBoolean inProgress = new AtomicBoolean(false);
					Map<Long, Long> durations = new LinkedHashMap<Long, Long>();
					long lastDuration = 0;
					final long AVG_INTERVALL_SIZE = 4000;
					while (true) {
						synchronized (Desktop.this) {
							if (!running) {
								return;
							}
						}
						try {
							inProgress.set(false);
							if (!desktopAnimation.isActive()) {
								long now = System.currentTimeMillis();
								long d = lastDuration + paintDuration;
								if (d <= 0) {
									d = 1;
								}
								Iterator<Entry<Long, Long>> i = durations.entrySet().iterator();
								while (i.hasNext()) {
									if (i.next().getKey() < now - AVG_INTERVALL_SIZE) {
										i.remove();
									} else {
										break;
									}
								}
								durations.put(now, d);
								long dSum = 0;
								for (Entry<Long, Long> e: durations.entrySet()) {
									dSum += e.getValue();
								}
								long avgD = dSum / durations.size();
								if (UIUtil.isPopupActive()) {
									avgD *= 2;
								}

								Thread.sleep(Math.min(Math.max(STEP_DELAY, avgD), 500) + animationStepTime.get());
							}
							if (!inProgress.get()) {
								inProgress.set(true);
								duration.set(0);
								SwingUtilities.invokeAndWait(new Runnable() {
									@Override
									public void run() {
										long startTime = System.currentTimeMillis();
										try {
											checkAnchorRetension();
											if (isAnimationEnabled()) {
												repaintOutline();
												if (isDesktopVisible()) {
													suppressRepaintDesktop = true;
													desktopAnimation.animate();
													boolean cl = calculateLinks();
													if (cl) {
														if (!UIUtil.isDWActive()) {
															repaint();
														}
													}
												}
											}
										} finally {
											suppressRepaintDesktop = false;
											inProgress.set(false);
											duration.set(System.currentTimeMillis() - startTime);
										}
									}
								});
							}
						} catch (Throwable e) {
							// ignore
						}
						lastDuration = duration.get();
					}
				}
			}, "Databrowser-Animator");
			UIUtil.startDemon(updateUIThread);
			
			AbstractAction a = new AbstractAction() {
				@Override
				public void actionPerformed(ActionEvent e) {
					if (isDesktopVisible()) {
						for (final RowBrowser rb : tableBrowsers) {
							if (rb.internalFrame.isSelected()) {
								rb.browserContentPane.rowsTable.grabFocus();
								UIUtil.invokeLater(new Runnable() {
									@Override
									public void run() {
										rb.browserContentPane.openQueryBuilder(true);
									}
								});
								break;
							}
						}
					}
				}
			};
			Container parent = parentFrame.getContentPane();
			if (parent instanceof JComponent) {
				JComponent comp = (JComponent) parent;
				InputMap im = comp.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
				im.put(KS_SQLCONSOLE, a);
				ActionMap am = comp.getActionMap();
				am.put(a, a);
			}
		} catch (Exception e) {
			UIUtil.showException(null, "Error", e, session);
		}
		desktops.add(this);
		updateMenu();
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
		public Color color1;
		
		/**
		 * The link's alternating color.
		 */
		public Color color2;
		
		/**
		 * Is the link visible?
		 */
		public boolean visible = true;
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
		 * Coordinates of the link render.
		 */
		public int x1, y1, x2, y2;

		/**
		 * The link's color.
		 */
		public Color color1;
		
		/**
		 * The link's alternating color.
		 */
		public Color color2;

		/**
		 * Row-to-row links.
		 */
		public List<RowToRowLink> rowToRowLinks = new ArrayList<RowToRowLink>();

		public void convertToRoot() {
			association = null;
			parent = null;
			browserContentPane.convertToRoot();
		}

		/**
		 * Is this RowBrowser hidden?
		 */
		private boolean hidden;

		/**
		 * Hides/unhides RowBrowser.
		 */
		public void setHidden(boolean hidden) {
			if (hidden == this.hidden) {
				return;
			}
			rbSourceToLinks = null;
			if (hidden) {
				internalFrame.setVisible(false);
				JInternalFrame[] allFramesFromTableBrowsers = getAllFramesFromTableBrowsers();
				if (!Stream.of(allFramesFromTableBrowsers).anyMatch(i -> i.isVisible() && i.isSelected())) {
					Stream.of(allFramesFromTableBrowsers).filter(i -> i.isVisible()).findFirst().ifPresent(i -> {
						try {
							i.setSelected(true);
						} catch (PropertyVetoException e) {
							// ignore
						}
					});
				}
			} else {
				internalFrame.setVisible(true);
				Rectangle r = layout(parent, association, browserContentPane, new ArrayList<RowBrowser>(), 0, -1);
				internalFrame.setBounds(r);
				desktopAnimation.scrollRectToVisible(internalFrame.getBounds(), false);
				try {
					internalFrame.setSelected(true);
				} catch (PropertyVetoException e) {
					// ignore
				}
				internalFrame.grabFocus();
			}
			this.hidden = hidden;
			checkDesktopSize();
			updateMenu();
		}

		/**
		 * Is this RowBrowser hidden?
		 */
		public boolean isHidden() {
			return hidden;
		}

		private MDTable mdTable;
		
		public MDTable getMDTable() {
			return mdTable;
		}
		
		public void setMDTable(MDTable mdTable) {
			this.mdTable = mdTable;
		}

	}

	/**
	 * All row-browsers.
	 */
	List<RowBrowser> tableBrowsers = new ArrayList<RowBrowser>();

	/**
	 * Opens a new row-browser.
	 * 
	 * @param parent
	 *            parent browser
	 * @param origParent 
	 * @param table
	 *            to read rows from. Open SQL browser if table is
	 *            <code>null</code>.
	 * @param association
	 *            to navigate, or <code>null</code>
	 * @param condition
	 * @param selectDistinct
	 * @param title
	 * @return new row-browser
	 */
	public synchronized RowBrowser addTableBrowser(final RowBrowser parent, final RowBrowser origParent, final Table table, final Association association,
			String condition, Boolean selectDistinct, String title, boolean reload) {
		
		Set<String> titles = new HashSet<String>();
		for (RowBrowser rb : tableBrowsers) {
			titles.add(rb.internalFrame.getTitle());
		}
		demaximize();

		checkHAlignedPath();
		
		if (title == null) {
			if (table != null) {
				title = datamodel.get().getDisplayName(table);
				if (titles.contains(title)) {
					for (int i = 2;; ++i) {
						String titelPlusI = title + " (" + i + ")";
						if (!titles.contains(titelPlusI)) {
							title = titelPlusI;
							break;
						}
					}
				}
			}
		}
		if (title == null) {
			title = "?";
		}
		
		if (desktopUndoManager != null) {
			desktopUndoManager.beforeModification("Remove \"" + title.replaceFirst("\\s*\\(\\d+\\)$", "") + "\"", "Add \"" + title.replaceFirst("\\s*\\(\\d+\\)$", "") + "\"");
		}

		checkHAlignButtons();
		
		BufferedImage[] m_offscreen = new BufferedImage[1];
		final RowBrowser tableBrowser = new RowBrowser();
		final JInternalFrame jInternalFrame = new JInternalFrame(table == null ? "SQL" : title) {
			private Dimension bufferSize = null;
		    private AffineTransform originalTransform;
		    private int currentIFrameBufferGeneration;
		    
		    /**
		     * Creates a new buffered image to use as an offscreen buffer.
		     */
		    protected BufferedImage getNewOffscreenBuffer(int width, int height) {
		    	if (originalTransform != null) {
					width = (int) (width * originalTransform.getScaleX());
					height = (int) (height * originalTransform.getScaleY());
		    	}
		        BufferedImage img = null;
		        if ( !GraphicsEnvironment.isHeadless() ) {
		            try {
		                img = (BufferedImage)createImage(width, height);
		            } catch ( Exception e ) {
		                img = null;
		            }
		        }
		        if ( img == null ) {
		            return new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
		        }
		        return img;
		    }

		    @Override
			public void paint(Graphics g) {
				boolean useBuffer = (desktopAnimation != null && desktopAnimation.isActive()) || desktopOutlineDraggingInProgress();
				boolean updateBuffer = false;
				if (currentIFrameBufferGeneration != iFrameBufferGeneration || !useBuffer) {
					currentIFrameBufferGeneration = iFrameBufferGeneration;
					m_offscreen[0] = null;
					bufferSize = null;
				}

				Graphics2D g2D = (Graphics2D)g;
				AffineTransform at = g2D.getTransform();
		    	
		        boolean newBuffer = false;
		    	if (at != null) {
		    		if (at.getShearX() != 0.0 || at.getShearY() != 0.0 || at.getScaleX() <= 0.0 || at.getScaleY() <= 0.0) {
		    			super.paint(g);
		    			return;
		    		}
		    		AffineTransform scaleInstance = AffineTransform.getScaleInstance(at.getScaleX(), at.getScaleY());
		    		if (originalTransform != null && !originalTransform.equals(scaleInstance)) {
		    			originalTransform = null;
		    			newBuffer = true;
		    		}
			        if (at.getScaleX() > 1.0 || at.getScaleY() > 1.0) {
			        	if (at.getShearX() == 0.0 && at.getShearY() == 0.0) {
			        		originalTransform = scaleInstance;
			            }
			        }
		    	}
		    	if (bufferSize == null) {
		    		m_offscreen[0] = null;
		    	} else if (Math.abs(bufferSize.getWidth() - getSize().width) > 2 || Math.abs(bufferSize.getHeight() - getSize().height) > 2) {
		    		m_offscreen[0] = null;
		    		super.paint(g);
		    		return;
		    	}
	    		if (m_offscreen[0] == null || newBuffer) {
	    			if (useBuffer) {
	    				updateBuffer = true;
	    			}
	    		}

		    	if (!updateBuffer) {
					if (useBuffer) {
						Graphics2D buf_g2D = (Graphics2D) m_offscreen[0].getGraphics(); // lgtm [java/dereferenced-value-may-be-null]
				        
				        if (originalTransform != null) {
				        	g2D.scale(1.0 / at.getScaleX(), 1.0 / at.getScaleY()); // lgtm [java/dereferenced-value-may-be-null]
				        }
				        if (g2D.getTransform().getScaleX() != 1.0 || g2D.getTransform().getScaleY() != 1.0) {
							g2D.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
					                RenderingHints.VALUE_ANTIALIAS_ON);
							g2D.setRenderingHint(RenderingHints.KEY_RENDERING,
					                RenderingHints.VALUE_RENDER_QUALITY);
				        }
				        
				        // paint the visualization
				        if (originalTransform != null) {
				        	buf_g2D.setTransform(originalTransform);
				        }
				        g.drawImage(m_offscreen[0], 0, 0, null);
				        buf_g2D.dispose();
					} else {
						m_offscreen[0] = null;
						super.paint(g);
					}
					return;
				}
		    	
		        if (m_offscreen[0] == null || newBuffer) {
		            m_offscreen[0] = getNewOffscreenBuffer(getWidth(), getHeight());
		            bufferSize = getSize();
		        }
		        Graphics2D buf_g2D = (Graphics2D) m_offscreen[0].getGraphics();
		        
		        if (originalTransform != null) {
		        	g2D.scale(1.0 / at.getScaleX(), 1.0 / at.getScaleY()); // lgtm [java/dereferenced-value-may-be-null]
		        }
		        if (g2D.getTransform().getScaleX() != 1.0 || g2D.getTransform().getScaleY() != 1.0) {
					g2D.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
			                RenderingHints.VALUE_ANTIALIAS_ON);
					g2D.setRenderingHint(RenderingHints.KEY_RENDERING,
			                RenderingHints.VALUE_RENDER_QUALITY);
		        }
		        
		        // paint the visualization
		        if (originalTransform != null) {
		        	buf_g2D.setTransform(originalTransform);
		        }
		        super.paint(buf_g2D);
		        g.drawImage(m_offscreen[0], 0, 0, null);
		        buf_g2D.dispose();
		    }
		};

		jInternalFrame.setClosable(true);
		jInternalFrame.setIconifiable(true);
		jInternalFrame.setMaximizable(true);
		jInternalFrame.setVisible(true);
		jInternalFrame.addMouseWheelListener(new java.awt.event.MouseWheelListener() {
			@Override
			public void mouseWheelMoved(java.awt.event.MouseWheelEvent evt) {
				long currentTime = System.currentTimeMillis();
				startRescaleMode(currentTime, evt.getX(), evt.getY(), evt.getComponent());
				onMouseWheelMoved(evt.getX(), evt.getY(), evt.getWheelRotation(), evt.getComponent(), currentTime, evt);
				onMouseWheelMoved(evt, parentFrame.getDesktopScrollPane(), currentTime);
			}
		});
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

		jInternalFrame.addPropertyChangeListener(JInternalFrame.IS_MAXIMUM_PROPERTY, new PropertyChangeListener() {
			@Override
			public void propertyChange(PropertyChangeEvent evt) {
				checkStopTS = System.currentTimeMillis();
				manager.resizeDesktop();
				if (Boolean.FALSE.equals(evt.getNewValue())) {
					if (desktopViewPos != null) {
						getScrollPane().getViewport().setViewPosition(desktopViewPos);
					}
				}
			}
		});

		jInternalFrame.addPropertyChangeListener(JInternalFrame.IS_ICON_PROPERTY, new PropertyChangeListener() {
			@Override
			public void propertyChange(PropertyChangeEvent evt) {
				if (jInternalFrame.isIcon()) {
					demaximize();
					tableBrowser.setHidden(true);
					try {
						jInternalFrame.setIcon(false);
					} catch (PropertyVetoException e) {
						// ignore
					}
				}
			}
		});

		jInternalFrame.addPropertyChangeListener(JInternalFrame.IS_SELECTED_PROPERTY, new PropertyChangeListener() {
			@Override
			public void propertyChange(PropertyChangeEvent evt) {
				if (Boolean.TRUE.equals(evt.getNewValue())) {
					onBecomeSelected(tableBrowser, jInternalFrame);
				}
			}
		});
		
		if (!becomeSelectedPending) {
			becomeSelectedPending = true;
			UIUtil.invokeLater(22, () -> {
				becomeSelectedPending = false;
				tableBrowsers.forEach(tb -> {
					if (tb.internalFrame.isSelected()) {
						onBecomeSelected(tb, tb.internalFrame);
					}
				});
			});
		}
		
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

		if (reload) {
			UISettings.s5.incrementAndGet();
		}

		final BrowserContentPane browserContentPane = new BrowserContentPane(datamodel.get(), table, condition, session, parent == null ? null : parent.browserContentPane.rows,
				association, parentFrame, rowsClosure, selectDistinct, reload, executionContext) {
			{
				MouseAdapter ml = new MouseAdapter() {
					@Override
					public void mouseEntered(MouseEvent e) {
						updateRowView(e);
					}
					@Override
					public void mouseMoved(MouseEvent e) {
						updateRowView(e);
					}
					@Override
					public void mouseExited(MouseEvent e) {
						if (null != currentlyViewedRow) {
							onRowSelect(table, null);
							currentlyViewedRow = null;
						}
					}
				};
				additionalMouseAdapter = ml;
				singleRowViewScrollPane.addMouseListener(ml);
				singleRowViewScrollPane.addMouseMotionListener((MouseMotionListener) ml);
				singleRowViewContainterPanel.addMouseListener(ml);
				singleRowViewContainterPanel.addMouseMotionListener((MouseMotionListener) ml);
				rowsTable.addMouseListener(ml);
				rowsTable.addMouseMotionListener((MouseMotionListener) ml);
			}
			
			@Override
			public boolean copyAllColumns() {
				return true;
			}

			@Override
			protected List<net.sf.jailer.ui.databrowser.DBConditionEditor.RSyntaxTextArea> getEditorPanesCache() {
				return editorPanesCache;
			}
			
			private void updateRowView(MouseEvent e) {
				int ri;
				JComponent source = (JComponent) e.getSource();
				if (source == rowsTable) {
					ri = rowsTable.rowAtPoint(e.getPoint());
				} else if (source == inplaceEditorTextField && inplaceEditorcurrentRow >= 0 && inplaceEditorcurrentRow < rows.size()) {
					ri = rowsTable.getRowSorter().convertRowIndexToView(inplaceEditorcurrentRow);
				} else {
					ri = 0;
				}
				if (ri >= 0 && !rows.isEmpty() && rowsTable.getRowSorter().getViewRowCount() > 0) {
					int i = 0;
					if (source == rowsTable) {
						i = rowsTable.getRowSorter().convertRowIndexToModel(ri);
					} else if (source == inplaceEditorTextField) {
						i = rowsTable.getRowSorter().convertRowIndexToModel(ri);
					} else if (source == rowsTableScrollPane || source == singleRowViewContainterPanel) {
						if (rows.size() != 1 || getQueryBuilderDialog() == null /* SQL Console */) {
							return;
						}
						ri = 0;
						i = 0;
					}
					Row row = rows.get(i);
					if (row != currentlyViewedRow) {
						onRowSelect(table, row);
						currentlyViewedRow = row;
					}
				}
			}
			
			@Override
			protected void reloadDataModel() throws Exception {
				Desktop.this.reloadDataModel(schemaMapping);
			}
			
			@Override
			protected QueryBuilderDialog getQueryBuilderDialog() {
				return queryBuilderDialog;
			}

			@Override
			protected RowBrowser navigateTo(Association association, List<Row> pRows) {
				return addTableBrowser(tableBrowser, tableBrowser, association.destination, association, toCondition(pRows), null, null, true);
			}

			@Override
			protected void onContentChange(List<Row> rows, boolean reloadChildren) {
				updateChildren(tableBrowser, rows);
				for (RowBrowser rb : tableBrowsers) {
					if (rb.parent == tableBrowser) {
						updateChildren(rb, rb.browserContentPane.rows);
						if (reloadChildren /* && rb.browserContentPane.parentRow == null */) {
							rb.browserContentPane.reloadRows();
						}
					}
				}
			}

			@Override
			protected void onRedraw() {
				repaintDesktop();
			}

			@Override
			protected JFrame getOwner() {
				return parentFrame;
			}

			private Set<Pair<Row, Row>> addedRowPairs;

			@Override
			protected void addRowToRowLink(Row parentRow, Row childRow) {
				synchronized (Desktop.this) {
					Pair<Row, Row> pair = new Pair<Row, Row>(parentRow, childRow);
					if (addedRowPairs == null || !addedRowPairs.contains(pair)) {
						if (addedRowPairs != null) {
							addedRowPairs.add(pair);
						}
						RowToRowLink rowToRowLink = new RowToRowLink();
						rowToRowLink.parentRow = parentRow;
						rowToRowLink.childRow = childRow;
						rowToRowLink.color1 = getAssociationColor1(association);
						rowToRowLink.color2 = getAssociationColor2(association);
						tableBrowser.rowToRowLinks.add(rowToRowLink);
					}
				}
			}

			@Override
			protected void beforeReload() {
				synchronized (Desktop.this) {
					addedRowPairs = new HashSet<Pair<Row,Row>>();
					tableBrowser.rowToRowLinks.clear();
				}
			}

			@Override
			protected void afterReload() {
				synchronized (Desktop.this) {
					addedRowPairs = null;
					invalidateIFramesBuffers();
				}
			}
			@Override
			protected Object getMonitorForFindClosure() {
				return Desktop.this;
			}
			
			private boolean pendingUpdates = false;
			
			@Override
			protected void findClosure(Row row) {
				invalidateIFramesBuffers();
				Set<Pair<BrowserContentPane, Row>> rows = new HashSet<Pair<BrowserContentPane, Row>>();
				synchronized (Desktop.this) {
					FindClosureContext findClosureContext = new FindClosureContext();
					findClosure(row, rows, false, findClosureContext);
					rowsClosure.currentClosure.addAll(rows);
					rows = new HashSet<Pair<BrowserContentPane, Row>>();
					findClosure(row, rows, true, findClosureContext);
				}
				rowsClosure.currentClosure.addAll(rows);
				rowsClosure.parentPath.clear();
				rowsClosure.parentPath.add(this);
				for (RowBrowser p = parent; p != null; p = p.parent) {
					rowsClosure.parentPath.add(p.browserContentPane);
				}
				
				if (!pendingUpdates) {
					pendingUpdates = true;
					UIUtil.invokeLater(() -> {
						pendingUpdates = false;
						try {
							Set<BrowserContentPane> browserInClosure = new HashSet<BrowserContentPane>();
							for (Pair<BrowserContentPane, Row> rid: rowsClosure.currentClosure) {
								browserInClosure.add(rid.a);
							}
			
							for (RowBrowser rb: tableBrowsers) {
								rb.browserContentPane.updateRowsCountLabel(browserInClosure);
							}
						} catch (Exception e) {
							e.printStackTrace();
						}
					});
				}
			}

			@Override
			protected void findTempClosure(Row row) {
				invalidateIFramesBuffers();
				Set<Pair<BrowserContentPane, Row>> rows = new HashSet<Pair<BrowserContentPane, Row>>();
				Set<Pair<BrowserContentPane, Row>> closure = new HashSet<Pair<BrowserContentPane, Row>>();
				FindClosureContext findClosureContext = new FindClosureContext();
				synchronized (Desktop.this) {
					findClosure(row, rows, false, findClosureContext);
					closure.addAll(rows);
					rows = new HashSet<Pair<BrowserContentPane, Row>>();
					findClosure(row, rows, true, findClosureContext);
					closure.addAll(rows);
				}
				
				rowsClosure.tempClosure.clear();
				for (Pair<BrowserContentPane, Row> p: closure) {
					rowsClosure.tempClosure.add(p.b);
				}
			}

			@Override
			protected void findClosure(Row row, Set<Pair<BrowserContentPane, Row>> closure, boolean forward, FindClosureContext findClosureContext) {
				synchronized (Desktop.this) {
					Pair<BrowserContentPane, Row> thisRow = new Pair<BrowserContentPane, Row>(this, row);
					if (!closure.contains(thisRow)) {
						closure.add(thisRow);
						if (forward) {
							for (RowBrowser child : tableBrowsers) {
								if (child.parent == tableBrowser) {
									for (RowToRowLink rowToRowLink : findClosureContext.getParentPartition(child, row)) {
//									for (RowToRowLink rowToRowLink : child.rowToRowLinks) {
//										if (row.nonEmptyRowId.equals(rowToRowLink.parentRow.nonEmptyRowId)) {
											child.browserContentPane.findClosure(rowToRowLink.childRow, closure, forward, findClosureContext);
//										}
									}
								}
							}
						} else {
							if (tableBrowser.parent != null) {
								for (RowToRowLink rowToRowLink : findClosureContext.getChildPartition(tableBrowser, row)) {
//								for (RowToRowLink rowToRowLink : tableBrowser.rowToRowLinks) {
//									if (row.nonEmptyRowId.equals(rowToRowLink.childRow.nonEmptyRowId)) {
										tableBrowser.parent.browserContentPane.findClosure(rowToRowLink.parentRow, closure, forward, findClosureContext);
										for (RowBrowser sibling : tableBrowsers) {
											if (sibling.parent == tableBrowser.parent && sibling.browserContentPane != this) {
												for (RowToRowLink sRowToRowLink: findClosureContext.getParentPartition(sibling, rowToRowLink.parentRow)) {
//												for (RowToRowLink sRowToRowLink: sibling.rowToRowLinks) {
//													if (rowToRowLink.parentRow.nonEmptyRowId.equals(sRowToRowLink.parentRow.nonEmptyRowId)) {
														sibling.browserContentPane.findClosure(sRowToRowLink.childRow, closure, true, findClosureContext);
//													}
												}
											}
										}
//									}
								}
							}
						}
					}
				}
			}

			private void createAnchorSQL(RowBrowser rb, StringBuilder rowIds, boolean indent) {
				boolean f = true;
				for (Row row : rb.browserContentPane.rows) {
					if (!f) {
						rowIds.append(indent ? " or\n       " : " or\n");
					}
					f = false;
					rowIds.append(SqlUtil.replaceAliases(row.rowId, "A", "A"));
				}
				rowIds.append("");
			}

			@Override
			protected QueryBuilderDialog.Relationship createQBRelations(boolean withParents) {
				QueryBuilderDialog.Relationship root = new QueryBuilderDialog.Relationship();
				root.whereClause = (getAndConditionText().trim()); // .replaceAll("(\r|\n)+", " ");
				if (root.whereClause.length() == 0) {
					root.whereClause = null;
				}
				StringBuilder rowIds = new StringBuilder("");
				createAnchorSQL(tableBrowser, rowIds, withParents);
				root.anchorWhereClause = rowIds.length() == 0 ? null : rowIds.toString();

				root.children.addAll(createQBChildrenRelations(null, !withParents));

				Association a = association;

				QueryBuilderDialog.Relationship r = root;
				RowBrowser childRB = tableBrowser;
				for (RowBrowser rb = tableBrowser.parent; rb != null && a != null; rb = rb.parent) {
					if (!withParents) {
						root.needsAnchor = true;
						break;
					}
					QueryBuilderDialog.Relationship child = new QueryBuilderDialog.Relationship();
					child.children.addAll(rb.browserContentPane.createQBChildrenRelations(childRB, false));
					child.parent = r;
					r.children.add(0, child);
					child.whereClause = (rb.browserContentPane.getAndConditionText().trim()).replaceAll("(\r|\n)+", " ");
					if (child.whereClause.length() == 0) {
						child.whereClause = null;
					}
					child.association = a.reversalAssociation;
					r.anchor = child.association;
					a = rb.association;
					rowIds = new StringBuilder("");
					createAnchorSQL(rb, rowIds, true);
					child.anchorWhereClause = rowIds.length() == 0 ? null : rowIds.toString();

					r.originalParent = child;
					r = child;
					childRB = rb;
				}
				return root;
			}

			@Override
			protected List<Relationship> createQBChildrenRelations(RowBrowser tabu, boolean all) {
				List<QueryBuilderDialog.Relationship> result = new ArrayList<QueryBuilderDialog.Relationship>();
				for (RowBrowser rb : tableBrowsers) {
					if (rb.parent == tableBrowser && rb != tabu) {
						if (true) { // all || !singleRowParent) {
							QueryBuilderDialog.Relationship child = new QueryBuilderDialog.Relationship();
							child.whereClause = (rb.browserContentPane.getAndConditionText().trim()).replaceAll("(\r|\n)+", " ");
							child.joinOperator = QueryBuilderDialog.JoinOperator.LeftJoin;
							if (child.whereClause.length() == 0) {
								child.whereClause = null;
							}
							child.association = rb.association;
							if (child.association != null) {
								child.children.addAll(rb.browserContentPane.createQBChildrenRelations(tabu, all));
								result.add(child);
							}
						}
					}
				}
				return result;
			}

			@Override
			protected void openSchemaMappingDialog() {
				Desktop.this.openSchemaMappingDialog(false);
			}

			@Override
			protected void openSchemaAnalyzer() {
				Desktop.this.openSchemaAnalyzer();
			}

			@Override
			protected DbConnectionDialog getDbConnectionDialog() {
				return dbConnectionDialog;
			}

			@Override
			protected double getLayoutFactor() {
				return layoutMode.factor;
			}

			@Override
			protected List<RowBrowser> getChildBrowsers() {
				return Desktop.this.getChildBrowsers(tableBrowser, false);
			}

			@Override
			protected RowBrowser getParentBrowser() {
				return tableBrowser.parent;
			}
			
			@Override
			protected RowBrowser getRowBrowser() {
				return tableBrowser;
			}

			@Override
			protected List<RowBrowser> getTableBrowser() {
				return new ArrayList<Desktop.RowBrowser>(Desktop.this.tableBrowsers);
			}

			@Override
			protected void onHide() {
				demaximize();
				tableBrowser.setHidden(true);
			}

			@Override
			protected void unhide() {
				tableBrowser.setHidden(false);
			}

			@Override
			protected void adjustClosure(BrowserContentPane tabu, BrowserContentPane thisOne) {
				Desktop.this.adjustClosure(tabu, thisOne);
			}

			@Override
			protected void close() {
				closeAll(Collections.singleton(tableBrowser));
			}

			@Override
			protected void showInNewWindow() {
				Desktop.this.showInNewWindow(tableBrowser);
			}

			@Override
			protected void appendLayout() {
				Desktop.this.restoreSession(tableBrowser, null);
			}

			@Override
			protected PriorityBlockingQueue<RunnableWithPriority> getRunnableQueue() {
				return runnableQueue;
			}

			@Override
			protected void collectPositions(Map<String, Map<String, double[]>> positions) {
				Desktop.this.collectPositions(tableBrowser, positions);
			}

			@Override
			protected boolean renderRowAsPK(Row theRow) {
				return false;
			}

			@Override
			protected MetaDataSource getMetaDataSource() {
				return parentFrame.getMetaDataSource();
			}

			@Override
			protected SQLConsole getSqlConsole(boolean switchToConsole) {
				return Desktop.this.getSqlConsole(switchToConsole);
			}

			@Override
			protected void deselectChildrenIfNeededWithoutReload() {
				for (RowBrowser rb : tableBrowsers) {
					if (rb.parent == tableBrowser) {
						rb.browserContentPane.deselectIfNeededWithoutReload();
						rb.browserContentPane.deselectChildrenIfNeededWithoutReload();
					}
				}
			}
			
			@Override
			protected int getReloadLimit() {
				return Desktop.this.getRowLimit();
			}

			@Override
			protected void setReloadLimit(int limit) {
				Desktop.this.setRowLimit(limit);
			}

			@Override
			protected void changeColumnOrder(Table table) {
				Desktop.this.changeColumnOrder(table);
			}

			@Override
			protected void rebase() {
				Component parent = SwingUtilities.getWindowAncestor(this);
				if (parent == null) {
					parent = this;
				}
				UIUtil.setWaitCursor(parent);
				if (desktopUndoManager != null) {
					BrowserContentPane root = this;
					RowBrowser rootBrowser = null;
					while (root.getParentBrowser() != null) {
						rootBrowser = root.getParentBrowser();
						root = rootBrowser.browserContentPane;
					}
					String undoDescription = "Start Navigation at \"" + (rootBrowser != null? ((rootBrowser.internalFrame.getTitle().replaceFirst("\\s*\\(\\d+\\)$", "")) + "\" ") : "");
					String redoDescription = "Start Navigation at \"" + (tableBrowser.internalFrame.getTitle().replaceFirst("\\s*\\(\\d+\\)$", "")) + "\" ";
					desktopUndoManager.beforeModification(undoDescription, redoDescription);
				}
				try {
					Desktop.noArrangeLayoutOnNewTableBrowser = true;
					Desktop.noArrangeLayoutOnNewTableBrowserWithAnchor = true;
					
					RowBrowser newBrowser = copy(null, null, null, null, true);
					RowBrowser newChildBrowser = newBrowser;
					
					BrowserContentPane root;
					BrowserContentPane br = this;
					do {
						root = br;
						RowBrowser pb = br.getParentBrowser();
						if (pb != null) {
							if (br.association != null) {
								newChildBrowser = pb.browserContentPane.copy(newChildBrowser, br.association.reversalAssociation, null, br.getRowBrowser(), true);
							} else {
								closeSubTree(newBrowser.browserContentPane, true);
								return;
							}
							br = pb.browserContentPane;
						} else {
							br = null;
						}
					} while (br != null);
					newBrowser.browserContentPane.reloadRows();
					newBrowser.internalFrame.setSelected(true);
					UIUtil.invokeLater(2, new Runnable() {
						@Override
						public void run() {
							Desktop.this.onLayoutChanged(false, true);
						}
					});
					UISettings.s7.addAndGet(1000);
					closeSubTree(root, true);
				} catch (Throwable t) {
					UIUtil.showException(parent, "Error", t);
				} finally {
					Desktop.noArrangeLayoutOnNewTableBrowser = false;
					Desktop.noArrangeLayoutOnNewTableBrowserWithAnchor = false;
					UIUtil.resetWaitCursor(parent);
				}
			}

			@Override
			protected RowBrowser copy(RowBrowser parent, Association newAssociation, Row pRow, RowBrowser childToIgnore, boolean newParent) {
				String andConditionText = this.getAndConditionText();
				if (pRow != null && (andConditionText == null || andConditionText.trim().length() == 0)) {
					andConditionText = pRow.rowId;
				}
				if (newParent && andConditionText != null) {
					if (!andConditionText.equals(SqlUtil.replaceAliases(andConditionText, "A", ""))) {
						andConditionText = "";
					}
				}
				RowBrowser tb = addTableBrowser(parent, parent, table, newAssociation, andConditionText, null, tableBrowser.internalFrame.getTitle(), false);
				tb.internalFrame.setBounds(tableBrowser.internalFrame.getBounds());
				try {
					tb.internalFrame.setIcon(!tableBrowser.internalFrame.isVisible());
				} catch (PropertyVetoException e) {
					// ignore
				}
				for (RowBrowser child: getChildBrowsers()) {
					if (child != childToIgnore) {
						child.browserContentPane.copy(tb, child.association, null, null, false);
					}
				}
				tb.browserContentPane.ownLimit = ownLimit;
				return tb;
			}

			@Override
			protected boolean shouldShowLoadErrors() {
				return isDesktopVisible();
			}

			@Override
			protected void onLayoutChanged() {
				Desktop.this.onLayoutChanged(false, false);
			}

			@Override
			protected void onConditionChange(String cond) {
				Desktop.this.onConditionChange(this, cond);
			}

			@Override
			protected void onContentCellEditorCreated(BrowserContentCellEditor cellEditor) {
				Desktop.this.onContentCellEditorCreated(this, cellEditor);
			}

			@Override
			protected void openConditionEditor(Point location, int column, Runnable onClose) {
				Desktop.this.openConditionEditor(this, location, column, onClose);
			}

			@Override
			protected void loadScriptFile(String fileName) {
				Desktop.this.loadScriptFile(fileName);
			}

			@Override
			protected double getAnimationFactor() {
				if (tableBrowser.internalFrame.isMaximum()) {
					return 0;
				}
				return Desktop.this.getAnimationFactor();
			}

			@Override
			protected void forceRepaint() {
				Desktop.this.forceRepaint();
			}

			@Override
			protected void findPathTo(Table table) {
				Desktop.this.findPathTo(table);
			}

			@Override
			protected WhereConditionEditorPanel getWhereConditionEditorPanel(RowBrowser rowBrowser) {
				return Desktop.this.getWhereConditionEditorPanel(rowBrowser);
			}
		};
		browserContentPane.addUserAction(new UserAction(
				"Align Horizontally",
				"Align horizontally with predecessors",
				() -> anchorManager.isApplicable(tableBrowser),
				() -> anchorManager.layout(tableBrowser)
				));
		jInternalFrame.addPropertyChangeListener(JInternalFrame.IS_MAXIMUM_PROPERTY, new PropertyChangeListener() {
			@Override
			public void propertyChange(PropertyChangeEvent evt) {
				browserContentPane.onMaximumPropertyChange(jInternalFrame.isMaximum());
			}
		});

		Rectangle r = layout(parent, association, browserContentPane, new ArrayList<RowBrowser>(), 0, -1);
		java.awt.event.MouseWheelListener mouseWheelListener = new java.awt.event.MouseWheelListener() {
			@Override
			public void mouseWheelMoved(java.awt.event.MouseWheelEvent evt) {
				long currentTime = System.currentTimeMillis();
				checkRescaleMode(evt, currentTime);
				onMouseWheelMoved(evt.getX(), evt.getY(), evt.getWheelRotation(), evt.getComponent(), currentTime, evt);
				if (evt.getSource() instanceof JScrollPane) {
					onMouseWheelMoved(evt, (JScrollPane) evt.getSource(), currentTime);
				}
			}
		};
		browserContentPane.rowsTableScrollPane.addMouseWheelListener(mouseWheelListener);
		browserContentPane.singleRowViewScrollPane.addMouseWheelListener(mouseWheelListener);

		jInternalFrame.setBounds(r);

		tableBrowser.internalFrame = jInternalFrame;
		tableBrowser.browserContentPane = browserContentPane;
		tableBrowser.parent = parent;
		tableBrowser.association = association;
		if (association != null) {
			tableBrowser.color1 = getAssociationColor1(association);
			tableBrowser.color2 = getAssociationColor2(association);
		}
		tableBrowsers.add(tableBrowser);
		UISettings.s2.set(Math.max(tableBrowsers.size(), UISettings.s2.get()));

		initIFrame(jInternalFrame, browserContentPane);
		
		anchorManager.onNewTableBrowser(tableBrowser);
		
		jInternalFrame.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
		jInternalFrame.addInternalFrameListener(new InternalFrameListener() {
			@Override
			public void internalFrameOpened(InternalFrameEvent e) {
				onLayoutChanged(false, true);
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
				List<RowBrowser> sis = getChildBrowsers(parent, true);
				boolean sameY = parent != null && sis.size() == 1 && parent.internalFrame != null && sis.get(0).internalFrame != null && parent.internalFrame.getY() == sis.get(0).internalFrame.getY();
				if (tableBrowser.browserContentPane.closeWithChildren(jInternalFrame)) {
					onLayoutChanged(parent == null || sameY, false);
				}
			}

			@Override
			public void internalFrameClosed(InternalFrameEvent e) {
				m_offscreen[0] = null;
				close(tableBrowser, true);
				checkHAlignButtons();
			}

			@Override
			public void internalFrameActivated(InternalFrameEvent e) {
			}
		});

		checkDesktopSize();
		updateMenu();
		
		if (!noArrangeLayoutOnNewTableBrowser) {
			this.scrollToCenter(jInternalFrame);
			try {
				jInternalFrame.setSelected(true);
			} catch (PropertyVetoException e1) {
				// ignore
			}
			browserContentPane.andCondition.grabFocus();
			List<RowBrowser> sis = getChildBrowsers(parent, true);
			boolean sameY = parent != null && sis.size() == 1 && parent.internalFrame != null && sis.get(0).internalFrame != null && parent.internalFrame.getY() == sis.get(0).internalFrame.getY();
			onLayoutChanged(parent == null || sameY, true);
		} else {
			lastInternalFrame.set(jInternalFrame);
			lastBrowserContentPane.set(browserContentPane);
		}

		if (tableBrowsers.size() > 1) {
			iFrameStateChangeRenderer.onNewIFrame(jInternalFrame);
		}

		return tableBrowser;
	}

	protected abstract WhereConditionEditorPanel getWhereConditionEditorPanel(RowBrowser rowBrowser);

	protected abstract void findPathTo(Table table);

	protected abstract int getRowLimit();
	protected abstract void setRowLimit(int limit);
	protected abstract void forceRepaint();
	
	/**
	 * Demaximizes all internal frames.
	 */
	private void demaximize() {
		for (RowBrowser rb : tableBrowsers) {
			try {
				rb.internalFrame.setMaximum(false);
			} catch (PropertyVetoException e) {
				// ignore
			}
		}
	}

	private void initIFrame(final JInternalFrame jInternalFrame, final BrowserContentPane browserContentPane) {
		browserContentPane.thumbnail = new JPanel();
		final JPanel thumbnailInner = new JPanel();
		browserContentPane.thumbnail.setLayout(new GridBagLayout());
		GridBagConstraints gridBagConstraints = new GridBagConstraints();
		gridBagConstraints.gridx = 1;
		gridBagConstraints.gridy = 1;
		gridBagConstraints.gridwidth = 1;
		gridBagConstraints.gridheight = 1;
		gridBagConstraints.weightx = 1;
		gridBagConstraints.weighty = 1;
		gridBagConstraints.fill = GridBagConstraints.BOTH;
		gridBagConstraints.insets = new Insets(8, 8, 8, 8);
		browserContentPane.thumbnail.add(thumbnailInner, gridBagConstraints);

		thumbnailInner.setLayout(new FlowLayout(FlowLayout.CENTER, 0, 0));
		String title = jInternalFrame.getTitle();
		String suffix = null;
		Pattern tPat = Pattern.compile("^(.*)(\\([0-9]+\\))$");
		Matcher matcher = tPat.matcher(title);
		if (matcher.matches()) {
			title = matcher.group(1);
			suffix = matcher.group(2);
		}
		
//		boolean isEmpty = browserContentPane.get
		
		List<String> labels = new ArrayList<String>();
		final List<JLabel> jLabels = new ArrayList<JLabel>();
		
		for (int i = 0; i < title.length(); ++i) {
			labels.add(title.substring(i, i + 1));
		}
		if (suffix != null) {
			labels.add(suffix);
		}
		for (String l: labels) {
			JLabel jl = new JLabel(l);
			jl.setFont(jl.getFont().deriveFont(jl.getFont().getStyle() | Font.BOLD));
			jLabels.add(jl);
			thumbnailInner.add(jl);
		}
		
		browserContentPane.setOnReloadAction(new Runnable() {
			
			@Override
			public void run() {
				if (browserContentPane.rows != null) {
					if (browserContentPane.rows.size() == 0) {
						for (JLabel l: jLabels) {
							l.setForeground(Color.GRAY);
						}
					} else {
						for (JLabel l: jLabels) {
							l.setForeground(Color.BLUE);
						}
					}
				}
			}
		});
		
		jInternalFrame.getContentPane().setLayout(new CardLayout());

		jInternalFrame.getContentPane().add(browserContentPane, "C");
		jInternalFrame.getContentPane().add(browserContentPane.thumbnail, "T");

		browserContentPane.thumbnail.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				if (e.getButton() != MouseEvent.BUTTON1 && !(browserContentPane.table instanceof SqlStatementTable)) {
					JPopupMenu popup = browserContentPane.createPopupMenu(null, -1, 0, 0, false, false, false);
					JPopupMenu popup2 = browserContentPane.createSqlPopupMenu(-1, 0, 0, true, jInternalFrame);
					popup.add(new JSeparator());
					for (Component c : popup2.getComponents()) {
						popup.add(c);
					}
					UIUtil.fit(popup);
					UIUtil.showPopup(e.getComponent(), e.getX(), e.getY(), popup);
				}
			}
		});

		initIFrameContent(jInternalFrame, browserContentPane, browserContentPane.thumbnail);
		jInternalFrame.addComponentListener(new ComponentListener() {
			@Override
			public void componentHidden(ComponentEvent e) {
				onLayoutChanged(false, false);
			}

			@Override
			public void componentMoved(ComponentEvent e) {
//				onLayoutChanged(false);
				checkHAlignedPath();
			}

			@Override
			public void componentResized(ComponentEvent e) {
//				onLayoutChanged(jInternalFrame.isMaximum());
				checkHAlignedPath();
				initIFrameContent(jInternalFrame, browserContentPane, browserContentPane.thumbnail);
			}

			@Override
			public void componentShown(ComponentEvent e) {
				onLayoutChanged(false, true);
			}
		});
	}

	private void initIFrameContent(final JInternalFrame jInternalFrame, final BrowserContentPane browserContentPane, final JPanel thumbnail) {
		if (jInternalFrame.getWidth() < 150 || jInternalFrame.getHeight() < 150) {
			((CardLayout) jInternalFrame.getContentPane().getLayout()).show(jInternalFrame.getContentPane(), "T");
		} else {
			((CardLayout) jInternalFrame.getContentPane().getLayout()).show(jInternalFrame.getContentPane(), "C");
		}
	}

	protected Color getAssociationColor1(Association association) {
		Color color = new java.awt.Color(0, 40, 255);
		if (association.isIgnored()) {
			color = new java.awt.Color(130, 130, 130);
		} else if (association.isInsertDestinationBeforeSource()) {
			color = new java.awt.Color(230, 80, 50);
		} else if (association.isInsertSourceBeforeDestination()) {
			color = new java.awt.Color(0, 230, 0);
		}
		return color;
	}

	private Color getAssociationColor2(Association association) {
		Color color = new java.awt.Color(0, 30, 255);
		if (association.isIgnored()) {
			color = new java.awt.Color(150, 150, 150);
		} else if (association.isInsertSourceBeforeDestination()) {
			color = new java.awt.Color(70, 255, 70);
		} else if (association.isInsertDestinationBeforeSource()) {
			color = new java.awt.Color(245, 90, 60);
		}
		return color;
	}

	private Rectangle layout(RowBrowser parent, Association association, BrowserContentPane browserContentPane,
			Collection<RowBrowser> ignore, int maxH, int xPosition) {
		int x = (int) (BROWSERTABLE_DEFAULT_MIN_X * layoutMode.factor); // lgtm [java/evaluation-to-constant]
		int y = (int) (BROWSERTABLE_DEFAULT_MIN_Y * layoutMode.factor);

		while (parent != null && parent.isHidden()) {
			parent = parent.parent;
		}

		if (parent != null) {
			x = (int) (parent.internalFrame.getX() + parent.internalFrame.getWidth() + BROWSERTABLE_DEFAULT_DISTANCE * layoutMode.factor);
			y = parent.internalFrame.getY();
		}
		if (maxH > 0) {
			y = maxH;
		}
		if (xPosition >= 0) {
			x = (int) ((xPosition * (BROWSERTABLE_DEFAULT_WIDTH + BROWSERTABLE_DEFAULT_DISTANCE) * layoutMode.factor));
		}
		// int h = fullSize || association == null ||
		// (association.getCardinality() != Cardinality.MANY_TO_ONE &&
		// association.getCardinality() != Cardinality.ONE_TO_ONE)? HEIGHT :
		// browserContentPane.getMinimumSize().height + MIN_HEIGHT;
		int h = (int) (BROWSERTABLE_DEFAULT_HEIGHT * layoutMode.factor);
		Rectangle r = new Rectangle(x, y, (int) (BROWSERTABLE_DEFAULT_WIDTH * layoutMode.factor), h);
		for (;;) {
			boolean ok = true;
			for (RowBrowser tb : tableBrowsers) {
				if (!ignore.contains(tb) && !tb.isHidden() && tb.internalFrame.getBounds().intersects(r)) {
					ok = false;
					break;
				}
			}
			r = new Rectangle(x, y, (int) (BROWSERTABLE_DEFAULT_WIDTH * layoutMode.factor), h);
			y += (int) (8 * layoutMode.factor);
			if (ok) {
				break;
			}
		}
		return r;
	}

	protected synchronized void updateChildren(RowBrowser tableBrowser, List<Row> rows) {
		boolean hasParent = false;

		for (RowBrowser rowBrowser : tableBrowsers) {
			if (rowBrowser == tableBrowser.parent) {
				hasParent = true;
			}
		}

		if (!hasParent) {
			tableBrowser.rowToRowLinks.clear();
		} else {
			Map<Row, Integer> rowIndex = new IdentityHashMap<Row, Integer>();
			Map<String, Integer> rowIDIndex = new HashMap<String, Integer>();
			Map<Row, Integer> parentRowIndex = new IdentityHashMap<Row, Integer>();
			Map<String, Integer> parentRowIDIndex = new HashMap<String, Integer>();
			for (int i = 0; i < rows.size(); ++i) {
				Integer iI = i;
				Row r = rows.get(i);
				rowIndex.put(r, iI);
				rowIDIndex.put(r.nonEmptyRowId, iI);
			}
			List<Row> parentRows = tableBrowser.parent.browserContentPane.rows;
			for (int i = 0; i < parentRows.size(); ++i) {
				Integer iI = i;
				Row r = parentRows.get(i);
				parentRowIndex.put(r, iI);
				parentRowIDIndex.put(r.nonEmptyRowId, iI);
			}
			for (RowToRowLink rowToRowLink : tableBrowser.rowToRowLinks) {
				rowToRowLink.childRowIndex = -1;
				Integer i = rowIndex.get(rowToRowLink.childRow);
				if (i != null) {
					rowToRowLink.childRowIndex = i;
				}
				// for (int i = 0; i < rows.size(); ++i) {
				// if (rowToRowLink.childRow == rows.get(i)) {
				// rowToRowLink.childRowIndex = i;
				// break;
				// }
				// }
				if (rowToRowLink.childRowIndex < 0) {
					i = rowIDIndex.get(rowToRowLink.childRow.nonEmptyRowId);
					if (i != null) {
						rowToRowLink.childRowIndex = i;
					}
					// for (int i = 0; i < rows.size(); ++i) {
					// if
					// (rowToRowLink.childRow.rowId.equals(rows.get(i).rowId)) {
					// rowToRowLink.childRowIndex = i;
					// break;
					// }
					// }
				}
				rowToRowLink.parentRowIndex = -1;
				i = parentRowIndex.get(rowToRowLink.parentRow);
				if (i != null) {
					rowToRowLink.parentRowIndex = i;
				}
				// for (int i = 0; i < parentRows.size(); ++i) {
				// if (rowToRowLink.parentRow == parentRows.get(i)) {
				// rowToRowLink.parentRowIndex = i;
				// break;
				// }
				// }

				if (rowToRowLink.parentRowIndex < 0) {
					i = parentRowIDIndex.get(rowToRowLink.parentRow.nonEmptyRowId);
					if (i != null) {
						rowToRowLink.parentRowIndex = i;
					}
					// for (int i = 0; i < parentRows.size(); ++i) {
					// if
					// (rowToRowLink.parentRow.rowId.equals(parentRows.get(i).rowId))
					// {
					// rowToRowLink.parentRowIndex = i;
					// break;
					// }
					// }
				}
			}
		}
	}

	private boolean suppressRepaintDesktop = false;
	
	/**
	 * Repaints the desktop.
	 */
	private void repaintDesktop() {
		if (!suppressRepaintDesktop) {
			calculateLinks();
			repaint();
		}
	}

	/**
	 * Calculates coordinates of all link-renders.
	 * 
	 * @return <code>true</code> iff something has changed
	 */
	private synchronized boolean calculateLinks() {
		boolean changed = false;
		Set<Long> linesHash = new HashSet<Long>(20000);
		Map<JTable, Integer> yPerRowTable1 = new HashMap<JTable, Integer>();
		Map<JTable, Integer> yPerRowTable2 = new HashMap<JTable, Integer>();
		for (RowBrowser tableBrowser : tableBrowsers) {
			JInternalFrame internalFrame = tableBrowser.internalFrame;
			if (internalFrame.isMaximum()) {
				changed = renderLinks;
				renderLinks = false;
				if (changed) {
					rbSourceToLinks = null;
				}
				return changed;
			}
			if (tableBrowser.parent != null) {
				int BORDER = 3;
				int BOT_H = 32;
				int x1 = internalFrame.getX(); // + internalFrame.getWidth() / 2;
				int y1 = internalFrame.getY() + internalFrame.getHeight() / 2;

				RowBrowser visParent = tableBrowser.parent;
				while (visParent != null && visParent.isHidden()) {
					visParent = visParent.parent;
				}
				if (visParent == null) {
					visParent = tableBrowser.parent;
				}

				Rectangle cellRect = new Rectangle();
				boolean ignoreScrolling = false;
				int i = 0;

				int x2 = visParent.internalFrame.getX();
				int y = cellRect.y;
				y = cellRect.height * i;
				int y2 = visParent.internalFrame.getY() + y + Math.min(cellRect.height / 2, 100);
				// if (midx < x1) {
				x2 += visParent.internalFrame.getWidth() - BORDER;
				// } else {
				// x2 += BORDER;
				// }
				Container p = visParent.browserContentPane.rowsTable;
				if (ignoreScrolling) {
					p = p.getParent();
				}
				while (p != visParent.internalFrame) {
					y2 += p.getY();
					p = p.getParent();
				}
				int min = visParent.internalFrame.getY() + Math.min(cellRect.height, 20);
				if (y2 < min) {
					y2 = min;
				}
				int max = visParent.internalFrame.getY() + visParent.internalFrame.getHeight() - BOT_H;
				if (y2 > max) {
					y2 = max;
				}

				y2 = visParent.internalFrame.getY() + visParent.internalFrame.getHeight() / 2;

				if (x1 != tableBrowser.x1 || y1 != tableBrowser.y1 || x2 != tableBrowser.x2 || y2 != tableBrowser.y2) {
					changed = true;
					tableBrowser.x1 = x1;
					tableBrowser.y1 = y1;
					tableBrowser.x2 = x2;
					tableBrowser.y2 = y2;
				}

				Rectangle visibleRect = getScrollPane().getViewport().getViewRect();
				int linkAreaXMin = Math.min(visParent.internalFrame.getX() + visParent.internalFrame.getWidth(), internalFrame.getX());
				int linkAreaYMin = Math.min(visParent.internalFrame.getY(), internalFrame.getY());
				int linkAreaXMax = Math.max(visParent.internalFrame.getX() + visParent.internalFrame.getWidth(), internalFrame.getX());
				int linkAreaYMax = Math.max(visParent.internalFrame.getY() + visParent.internalFrame.getHeight(), internalFrame.getY() + internalFrame.getHeight());
				boolean allInvisible = false;
				if (!isIconOrHidden(visParent.internalFrame) && !isIconOrHidden(internalFrame)) {
					if (linkAreaXMin > visibleRect.getX() + visibleRect.getWidth()) {
						allInvisible = true;
					} else if (linkAreaYMin > visibleRect.getY() + visibleRect.getHeight()) {
						allInvisible = true;
					} else if (linkAreaXMax < visibleRect.getX()) {
						allInvisible = true;
					} else if (linkAreaYMax < visibleRect.getY()) {
						allInvisible = true;
					}
				}

				for (RowToRowLink rowToRowLink : tableBrowser.rowToRowLinks) {
					rowToRowLink.visible = !allInvisible;
					if (!rowToRowLink.visible) {
						continue;
					}
					x1 = y1 = x2 = y2 = -1;
					try {
						if (rowToRowLink.childRowIndex >= 0 && rowToRowLink.parentRowIndex >= 0) {
							cellRect = new Rectangle();
							i = 0;
							ignoreScrolling = false;
							i = tableBrowser.browserContentPane.rowsTable.getRowSorter().convertRowIndexToView(rowToRowLink.childRowIndex);
							if (i < 0) {
								rowToRowLink.visible = false;
								continue;
							}
							cellRect = tableBrowser.browserContentPane.rowsTable.getCellRect(i, 0, true);
							if (tableBrowser.browserContentPane.rows != null && tableBrowser.browserContentPane.rows.size() == 1) {
								cellRect.setBounds(cellRect.x, 0, cellRect.width, Math.min(cellRect.height, 20));
								ignoreScrolling = true;
							}

							x1 = internalFrame.getX();
							y = cellRect.height * i;
							// if (r1) {
							// x1 += internalFrame.getWidth()- BORDER;
							// } else {
							x1 += BORDER;
							// }

							p = tableBrowser.browserContentPane.rowsTable;
							Integer pY = yPerRowTable1.get(p);
							if (pY != null) {
								y1 = pY;
							} else {
								y1 = internalFrame.getY();
								if (ignoreScrolling) {
									p = p.getParent();
								}
								while (p != internalFrame) {
									y1 += p.getY();
									p = p.getParent();
								}
								yPerRowTable1.put(tableBrowser.browserContentPane.rowsTable, y1);
							}
							y1 += y + cellRect.height / 2;
							min = internalFrame.getY() + cellRect.height * 2;
							if (y1 < min) {
								y1 = min;
							}
							max = internalFrame.getY() + internalFrame.getHeight() - BOT_H;
							if (y1 > max) {
								y1 = max;
							}
							ignoreScrolling = false;
							cellRect = new Rectangle();
							i = 0;
							i = tableBrowser.parent.browserContentPane.rowsTable.getRowSorter().convertRowIndexToView(rowToRowLink.parentRowIndex);
							if (i < 0) {
								rowToRowLink.visible = false;
								continue;
							}
							cellRect = tableBrowser.parent.browserContentPane.rowsTable.getCellRect(i, 0, true);
							if (tableBrowser.parent.browserContentPane.rows != null && tableBrowser.parent.browserContentPane.rows.size() == 1) {
								cellRect.setBounds(cellRect.x, 0, cellRect.width, Math.min(cellRect.height, 20));
								ignoreScrolling = true;
							}

							x2 = visParent.internalFrame.getX();
							y = cellRect.height * i;
							// if (r2) {
							x2 += visParent.internalFrame.getWidth() - BORDER;
							// } else {
							// x2 += BORDER;
							// }

							p = visParent.browserContentPane.rowsTable;
							pY = yPerRowTable2.get(p);
							if (pY != null) {
								y2 = pY;
							} else {
								y2 = visParent.internalFrame.getY();
								if (ignoreScrolling) {
									p = p.getParent();
								}
								while (p != visParent.internalFrame) {
									y2 += p.getY();
									p = p.getParent();
								}
								yPerRowTable2.put(visParent.browserContentPane.rowsTable, y2);
							}
							y2 += y + cellRect.height / 2;
							min = visParent.internalFrame.getY() + cellRect.height;
							if (y2 < min) {
								y2 = min;
							}
							max = visParent.internalFrame.getY() + visParent.internalFrame.getHeight() - BOT_H;
							if (y2 > max) {
								y2 = max;
							}
						}

						if (tableBrowser.parent.internalFrame.isVisible() && tableBrowser.internalFrame.isVisible()) {
							long shift = 32768;
							long start = (long) x2 + shift * (long) y2;
							long end = (long) x1 + shift * (long) y1;
							long lineHash = start + shift * shift * end;
							if (linesHash.contains(lineHash)) {
								rowToRowLink.visible = false;
								continue;
							} else {
								linesHash.add(lineHash);
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
		long currentTimeMillis = System.currentTimeMillis();
		if (lastPTS + 100 < currentTimeMillis) {
			changed = true;
		}
		if (changed) {
			lastPTS = currentTimeMillis;
		}
		if (changed) {
			rbSourceToLinks = null;
		}
		
		animationStep = (currentTimeMillis / (double) STEP_DELAY) * getAnimationFactor();
		
		if (lastAnimationStepTime + STEP_DELAY < currentTimeMillis) {
			changed = true;
			lastAnimationStepTime = currentTimeMillis;
		}
		return changed;
	}

	private boolean isIconOrHidden(JInternalFrame internalFrame) {
		return internalFrame.isIcon() || !internalFrame.isVisible();
	}

	private long lastPTS = 0;

	private static class Link {
		public boolean visible = true;
		public final RowBrowser from, to;
		public final String sourceRowID, destRowID;
		public int x1, y1, x2, y2;
		public final Color color1;
		public final Color color2;
		public final boolean dotted, intersect;
		public final boolean inClosure;
		public final boolean inTempClosure;
		public final boolean notHAligned;
		public final boolean restricted;
		
		public Link(RowBrowser from, RowBrowser to, String sourceRowID, String destRowID, int x1, int y1, int x2, int y2, Color color1, Color color2, boolean dotted,
				boolean intersect, boolean inClosure, boolean inTempClosure, boolean notHAligned, boolean restricted) {
			this.from = from;
			this.to = to;
			this.sourceRowID = sourceRowID;
			this.destRowID = destRowID;
			this.x1 = x1;
			this.y1 = y1;
			this.x2 = x2;
			this.y2 = y2;
			this.color1 = restricted? color1.brighter() : color1;
			this.color2 = restricted? color2.brighter() : color2;
			this.dotted = dotted;
			this.intersect = intersect;
			this.inClosure = inClosure;
			this.inTempClosure = inTempClosure;
			this.notHAligned = notHAligned;
			this.restricted = restricted;
		}
	}

	private Map<RowBrowser, Map<String, List<Link>>> rbSourceToLinks = null;
	private long paintDuration = 0;
	
	/**
	 * Paints all link-renders.
	 */
	@Override
	public synchronized void paint(Graphics graphics) {
		long startTime = System.currentTimeMillis();
		super.paint(graphics);
		if (graphics instanceof Graphics2D) {
			final Graphics2D g2d = (Graphics2D) graphics;
			if (renderLinks) {
				g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
				g2d.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
				if (rbSourceToLinks == null) {
					rbSourceToLinks = new HashMap<RowBrowser, Map<String, List<Link>>>();
					final Object ALL = new String("-");

					for (RowBrowser tableBrowser : tableBrowsers) {
						Map<String, List<Link>> links = new TreeMap<String, List<Link>>();
						rbSourceToLinks.put(tableBrowser, links);

						Color color1 = tableBrowser.color1;
						Color color2 = tableBrowser.color2;
						boolean linkAdded = false;
						boolean restricted = tableBrowser.parent != null && tableBrowser.browserContentPane.loadedRowsAreRestricted;
						boolean notHAligned = false;
						if (tableBrowser.parent != null) {
							if (!rowsClosure.hAlignedPath.isEmpty() && !tableBrowser.isHidden()) {
								if (!rowsClosure.hAlignedPath.contains(tableBrowser.browserContentPane)) {
									RowBrowser visParent = tableBrowser.parent;
									while (visParent.isHidden() && visParent.parent != null) {
										visParent = visParent.parent;
									}
									if (rowsClosure.hAlignedPath.contains(visParent.browserContentPane)) {
										if (getChildBrowsers(visParent, true).stream().filter(c -> rowsClosure.hAlignedPath.contains(c.browserContentPane)).findAny().isPresent()) {
											if (visParent.browserContentPane.rows.size() > 1 || getChildBrowsers(visParent, false).stream().filter(c -> c.isHidden() || c.browserContentPane.rows.size() > 1).findAny().isPresent()) {
												notHAligned = true;
											}
										}
									}
								}
							}
						}
						for (RowToRowLink rowToRowLink : tableBrowser.rowToRowLinks) {
							if (rowToRowLink.x1 >= 0) {
								linkAdded = true;
								if (rowToRowLink.visible) {
									String sourceRowID = rowToRowLink.childRow.nonEmptyRowId;
									String destRowID = rowToRowLink.parentRow.nonEmptyRowId;
									boolean inClosure = false;
									boolean inTempClosure = false;
									
									if (tableBrowser.parent != null) {
										if (rowsClosure.currentClosure.contains(new Pair<BrowserContentPane, Row>(tableBrowser.browserContentPane, rowToRowLink.childRow))) {
											if (rowsClosure.currentClosure.contains(new Pair<BrowserContentPane, Row>(tableBrowser.parent.browserContentPane, rowToRowLink.parentRow))) {
												inClosure = true;
											}
										}
										if (rowsClosure.tempClosure.contains(rowToRowLink.childRow)) {
											if (rowsClosure.tempClosure.contains(rowToRowLink.parentRow)) {
												inTempClosure = true;
											}
										}
									}
	
									Link link = new Link(tableBrowser, tableBrowser.parent, sourceRowID, destRowID, rowToRowLink.x1, rowToRowLink.y1,
											rowToRowLink.x2, rowToRowLink.y2, color1, color2, false, false, inClosure, inTempClosure, notHAligned, restricted);
									List<Link> l = links.get(sourceRowID);
									if (l == null) {
										l = new ArrayList<Link>();
										links.put(sourceRowID, l);
									}
									l.add(link);
								}
							}
						}
						if (tableBrowser.parent != null && !linkAdded) {
							String sourceRowID = (String) ALL;
							String destRowID = (String) ALL;
							boolean inClosure = false;
							boolean inTempClosure = false;

							Link link = new Link(tableBrowser, tableBrowser.parent, sourceRowID, destRowID, tableBrowser.x1, tableBrowser.y1,
									tableBrowser.x2, tableBrowser.y2, color1, color2, true, true, inClosure, inTempClosure, notHAligned, restricted);
							List<Link> l = links.get(sourceRowID);
							if (l == null) {
								l = new ArrayList<Link>();
								links.put(sourceRowID, l);
							}
							l.add(link);
						}
					}

					// join links of hidden browser
					List<Link> toJoinList = new ArrayList<Link>();
					for (RowBrowser tableBrowser : tableBrowsers) {
						if (tableBrowser.parent != null && tableBrowser.parent.isHidden()) {
							List<Link> newLinks = new ArrayList<Link>();
							Map<String, List<Link>> links = rbSourceToLinks.get(tableBrowser);
							for (Map.Entry<String, List<Link>> e : links.entrySet()) {
								for (Link link : e.getValue()) {
									link.visible = false;

									List<Link> ll;
									if (link.destRowID == ALL) {
										ll = new ArrayList<Desktop.Link>();
										for (List<Link> values : rbSourceToLinks.get(link.to).values()) {
											for (Link l : values) {
												ll.add(l);
											}
										}
									} else {
										ll = rbSourceToLinks.get(link.to).get(link.destRowID);
									}

									toJoinList.clear();
									if (ll != null) {
										toJoinList.addAll(ll);
									}
									ll = rbSourceToLinks.get(link.to).get(ALL);
									if (ll != null) {
										toJoinList.addAll(ll);
									}

									for (Link toJoin : toJoinList) {
										toJoin.visible = false;
										boolean intersect = link.intersect;
										boolean dotted = link.dotted || toJoin.dotted;
										newLinks.add(new Link(link.from, toJoin.to, link.sourceRowID, toJoin.destRowID, link.x1, link.y1, toJoin.x2, toJoin.y2,
												UIUtil.plaf == PLAF.FLAT? new Color(170, 200, 0) : Color.yellow.darker().darker(), 
												UIUtil.plaf == PLAF.FLAT? new Color(170, 200, 0) : Color.yellow.darker(), 
												dotted, intersect, link.inClosure && toJoin.inClosure, link.inTempClosure && toJoin.inTempClosure, link.notHAligned, link.restricted || toJoin.restricted));
									}
								}
							}
							for (Link link : newLinks) {
								links.get(link.sourceRowID).add(link);
							}
						}
					}
					int i = 0;
				}

				Set<RowBrowser> pathToSelectedRowBrowser = new HashSet<RowBrowser>();
				for (RowBrowser rb: getBrowsers()) {
					if (rb.internalFrame.isSelected()) {
						for (RowBrowser parent = rb; parent != null; parent = parent.parent) {
							pathToSelectedRowBrowser.add(parent);
						}
						break;
					}
				}

				Set<Long> linesHash = new HashSet<Long>(20000);
				Map<RowBrowser, List<Link>> linksToRenderPerTableBrowser = new HashMap<RowBrowser, List<Link>>();
				Map<RowBrowser, Integer> dirPerTableBrowser = new HashMap<RowBrowser, Integer>();

				for (final RowBrowser tableBrowser : rbSourceToLinks.keySet()) {
					if (!tableBrowser.isHidden()) {
						Map<String, List<Link>> links = rbSourceToLinks.get(tableBrowser);
						final List<Link> linksToRender = new ArrayList<Link>(1000);
						int dir = 0;
						for (Map.Entry<String, List<Link>> e : links.entrySet()) {
							for (Link link : e.getValue()) {
								if (link.visible && !link.from.isHidden() && !link.to.isHidden()) {
									long shift = 32768;
									long start = (long) link.x2 + shift * (long) link.y2;
									long end = (long) link.x1 + shift * (long) link.y1;
									long lineHash = start + shift * shift * end;
									if (!linesHash.contains(lineHash)) {
										linksToRender.add(link);
										linesHash.add(lineHash);
										if (link.y1 < link.y2) {
											++dir;
										} else {
											--dir;
										}
									}
								}
							}
						}
						
						final boolean isToParentLink = tableBrowser.association != null && tableBrowser.association.isInsertDestinationBeforeSource();
						Collections.sort(linksToRender, new Comparator<Link>() {
							@Override
							public int compare(Link a, Link b) {
								if (isToParentLink) {
									if (a.y1 != b.y1) {
										return a.y1 - b.y1;
									} else {
										return a.y2 - b.y2;
									}
								} else {
									if (a.y2 != b.y2) {
										return a.y2 - b.y2;
									} else {
										return a.y1 - b.y1;
									}
								}
							}
						});
						linksToRenderPerTableBrowser.put(tableBrowser, linksToRender);
						dirPerTableBrowser.put(tableBrowser, dir);
					}
				}

				final int MAX_PRIO = 5;
				for (int prio = rowsClosure.hAlignedPath.isEmpty()? 2 : 0; prio <= MAX_PRIO; ++prio) {
					for (final boolean pbg : new Boolean[] { true, false }) {
						for (final RowBrowser tableBrowser : rbSourceToLinks.keySet()) {
							if (!tableBrowser.isHidden()) {
								final boolean inClosureRootPath = rowsClosure.parentPath.contains(tableBrowser.browserContentPane);
								boolean light = true;
								final Map<String, java.awt.geom.Point2D.Double> followMe;
								final boolean isToParentLink = tableBrowser.association != null && tableBrowser.association.isInsertDestinationBeforeSource();
								if (!isToParentLink) {
									followMe = new HashMap<String, java.awt.geom.Point2D.Double>();
								} else {
									followMe = null;
								}
								int lastY = -1;
								int lastLastY = -1;
								boolean lastInClosure = false;
								Map<String, List<Runnable>> renderTasks = new HashMap<String, List<Runnable>>();
								final List<Link> linksToRender = linksToRenderPerTableBrowser.get(tableBrowser);
								if (linksToRender == null) {
									continue;
								}
								Integer dv = dirPerTableBrowser.get(tableBrowser);
								int dir = dv == null? 0 : dv;
								for (int i = 0; i < linksToRender.size(); ++i) {
									final Link link = linksToRender.get(i);
									int y = isToParentLink? link.y1 : link.y2;
									if (lastInClosure != link.inClosure) {
										light = !light;
									} else if (lastY != y) {
										if (lastLastY == lastY) {
											light = !light;
										} else {
											if (i < linksToRender.size() - 1) {
												int nextY = isToParentLink? linksToRender.get(i + 1).y1 : linksToRender.get(i + 1).y2;
												if (nextY == y) {
													light = !light;
												}
											}
										}
									}
									lastLastY = lastY;
									lastY = y;
									lastInClosure = link.inClosure;
									Color cl = pbg ? Color.white : link.color1; // light? link.color1 : link.color2;
									if (UIUtil.plaf != PLAF.NIMBUS) {
										if (cl.getGreen() > cl.getBlue() && cl.getGreen() >= cl.getRed()) {
//											if (link.restricted) {
												cl = new Color(
														(int) (cl.getRed()),
														(int) (cl.getGreen() * 0.75f),
														(0));
//											}
										} else {
											double f = 1.1; // link.restricted? 1.7 : 1.1;
											cl = new Color(
													brighter(cl.getRed(), f),
													brighter(cl.getGreen(), f),
													brighter(cl.getBlue(), f));
										}
									}
									final Color color = link.restricted && UIUtil.plaf == PLAF.NIMBUS? cl.darker() : cl;
									final Point2D start = new Point2D.Double(link.x2, link.y2);
									final Point2D end = new Point2D.Double(link.x1, link.y1);
									final int ir = dir > 0? i : linksToRender.size() - 1 - i;
									final boolean finalLight = light;
									int linkPrio = 0;
									if (link.notHAligned) {
										if (link.inClosure) {
											linkPrio += 1;
										}
									} else {
										linkPrio = 2;
										if (pathToSelectedRowBrowser.contains(tableBrowser)) {
											linkPrio += 2;
										}
										if (link.inClosure) {
											linkPrio += 1;
										}
									}
									final boolean doPaint = linkPrio == prio;
									Runnable task = new Runnable() {
										@Override
										public void run() {
											paintLink(start, end, color, link.color2, g2d, tableBrowser, pbg, link.intersect,
													link.dotted, link.restricted,
												linksToRender.size() == 1 ? 0.5 : (ir + 1) * 1.0 / linksToRender.size(),
												finalLight, followMe,
												link.sourceRowID, link.inClosure, link.inTempClosure, link.notHAligned, inClosureRootPath,
												isToParentLink,
												doPaint);
										}
									};
									List<Runnable> tasks = renderTasks.get(link.sourceRowID);
									if (tasks == null) {
										tasks = new LinkedList<Runnable>();
										renderTasks.put(link.sourceRowID, tasks); 
									}
									tasks.add(task);
								}
								for (Entry<String, List<Runnable>> entry: renderTasks.entrySet()) {
									List<Runnable> tasks = entry.getValue();
									Runnable mid = tasks.get(tasks.size() / 2);
									mid.run();
									for (Runnable task: tasks) {
										if (task != mid) {
											task.run();
										}
									}
								}
							}
						}
					}
				}
				iFrameStateChangeRenderer.render(g2d);
			}
		}
		paintDuration = System.currentTimeMillis() - startTime;
		deferRescaleMode(startTime);
	}

	private int brighter(int col, double f) {
		if (col > 128) {
			return Math.min((int) (col * f), 255);
		} else {
			return Math.max(255 - (int) ((255 - col) / f), 0);
		}
	}

	private double animationStep = 0;
	long lastAnimationStepTime = 0;
	final long STEP_DELAY = 50;
	private Map<Color, Color> fgColorMap = new HashMap<Color, Color>();

	private void paintLink(Point2D start, Point2D end, Color color, Color fgColor, Graphics2D g2d, RowBrowser tableBrowser,
			boolean pbg, boolean intersect, boolean dotted, boolean restricted, double midPos, boolean light,
			Map<String, Point2D.Double> followMe, String sourceRowID, boolean inClosure, boolean inTempClosure, boolean notHAligned, boolean inClosureRootPath,
			boolean isToParentLink, boolean doPaint) {
		int so = 0;
		float sop = 0;
		if (doPaint) {
			if (UIUtil.plaf == PLAF.NIMBUS) {
				g2d.setColor(inTempClosure && !pbg? new Color(220, 220, 255) : color);
			} else if (UIUtil.plaf == PLAF.FLAT) {
				double f = 4;
				if (fgColor.getGreen() > fgColor.getRed() + fgColor.getBlue()) {
					f = 2.0;
				}
				g2d.setColor((inTempClosure) && pbg? new Color(
						brighter(fgColor.getRed(), f ),
						brighter(fgColor.getGreen(), f),
						brighter(fgColor.getBlue(), f)) : color);
				sop = 0.25f;
				so = pbg? 3 : 0;
				if (inTempClosure && pbg) {
					so = 6;
				} else if (inClosure && pbg) {
					so = 4;
				}
			} else {
				g2d.setColor(inTempClosure && pbg? new Color(200, 100, 200) : color);
			}
			BasicStroke stroke = new BasicStroke(sop + (so > 0? so : (!intersect ? (pbg ? inClosure? 3 : 2 : 1) : (pbg ? 3 : 2))));
			if (inClosure) {
				final int LENGTH = 16;
				g2d.setStroke(new BasicStroke(stroke.getLineWidth(), stroke.getEndCap(), stroke.getLineJoin(), stroke.getMiterLimit(), new float[] { 11f, 5f },
						(float) ((inClosureRootPath ^ isToParentLink)? animationStep % LENGTH : (LENGTH - animationStep % LENGTH))));
			} else {
				g2d.setStroke(dotted ? new BasicStroke(stroke.getLineWidth(), stroke.getEndCap(), stroke.getLineJoin(), stroke.getMiterLimit(), new float[] { 2f, 6f },
						1.0f) : restricted? new BasicStroke(stroke.getLineWidth(), stroke.getEndCap(), stroke.getLineJoin(), stroke.getMiterLimit(), new float[] { 22f, 2.5f },
								1.0f) 
						: stroke);
			}
		}

		if (notHAligned) {
			Color fg = g2d.getColor();
			if (!pbg) {
				Color mc = fgColorMap.get(fg);
				if (mc == null) {
					mc = new HSLColor(fg).adjustLuminance(fg.getBlue() > fg.getRed() && fg.getBlue() > fg.getGreen()? 80 : fg.getGreen() > 130 && fg.getRed() > 130 && fg.getBlue() < 8? 60 : fg.getGreen() > fg.getRed() && fg.getGreen() > fg.getBlue()? 50 : fg.getGreen() > 130 && fg.getBlue() > 130 && fg.getRed() < 30? 70 : 86);
					fgColorMap.put(fg, mc);
				}
				g2d.setColor(mc);
			}
		}
		
		// compute the intersection with the target bounding box
		if (intersect) {
			Point2D[] sect = new Point2D[10];
			int i = GraphicsLib.intersectLineRectangle(start, end, tableBrowser.internalFrame.getBounds(), sect);
			if (i > 0) {
				end = sect[0];
			}
		}
		if (start.distance(end) < 2)
			return;

		double border = 0.25;
		double f = midPos * (1.0 - 2.0 * border);
		int midX = (int) (start.getX() + ((end.getX() - start.getX()) * (border + f)));
		f = 0.25 * f * (end.getY() - start.getY());

		if (followMe != null) {
			java.awt.geom.Point2D.Double follow = followMe.get(sourceRowID);
			if (follow != null) {
				midX = (int) follow.getX();
				f = follow.getY();
			} else {
				followMe.put(sourceRowID, new Point2D.Double(midX, f));
			}
		}
		
		if (!doPaint) {
			return;
		}
		
		Path2D.Double path = new Path2D.Double();
		if (isToParentLink) {
			path.moveTo(end.getX() - 5, end.getY());
			path.curveTo(midX, end.getY(), midX, start.getY() + f, start.getX(), start.getY());
		} else {
			path.moveTo(start.getX(), start.getY());
			path.curveTo(midX, start.getY() + f, midX, end.getY(), end.getX() - 5, end.getY());
		}
		g2d.draw(path);
		
		// create the arrow head shape
		m_arrowHead = new Polygon();
		double ws = 0.4;
		double hs = 2.0 / 3.0;
		double w = 3, h = w;
		m_arrowHead.addPoint(0, 0);
		m_arrowHead.addPoint((int) (ws * -w), (int) (hs * (-h)));
		// m_arrowHead.addPoint(0, (int) (hs * (-2 * h)));
		m_arrowHead.addPoint((int) (ws * w), (int) (hs * (-h)));
		m_arrowHead.addPoint(0, 0);

		AffineTransform at = getArrowTrans(new Point2D.Double(midX, end.getY()), end, 9);
		Shape m_curArrow = at.createTransformedShape(m_arrowHead);

		g2d.setStroke(new BasicStroke(2));
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
		int o = 1;
		m_arrowTrans.setToTranslation(p2.getX() + o, p2.getY());
		m_arrowTrans.rotate(-Math.PI / 2.0 + Math.atan2(p2.getY() - p1.getY(), p2.getX() + o - p1.getX()));
		if (width > 1) {
			double scalar = width / 2;
			m_arrowTrans.scale(scalar, scalar);
		}
		return m_arrowTrans;
	}

	private static int FRAME_OFFSET = 20;
	private MDIDesktopManager manager;

	@Override
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

	@Override
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

	private Dimension currentDesktopnSize;
	private Dimension postAnimationDesktopSize;

	/**
	 * Sets all component size properties ( maximum, minimum, preferred) to the
	 * given dimension.
	 */
	public boolean setAllSize(Dimension d) {
		if (currentDesktopnSize != null && currentDesktopnSize.equals(d)) {
			return false;
		}
		currentDesktopnSize = d;
		setMinimumSize(d);
		setMaximumSize(d);
		setPreferredSize(d);
		revalidate();
		return true;
	}

	/**
	 * Sets all component size properties ( maximum, minimum, preferred) to the
	 * given width and height.
	 */
	public boolean setAllSize(int width, int height) {
		return setAllSize(new Dimension(width, height));
	}

	void checkDesktopSize() {
		if (getParent() != null && isVisible()) {
			manager.resizeDesktop();
		}
	}

	public JScrollPane getScrollPane() {
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

		@Override
		public void endResizingFrame(JComponent f) {
			super.endResizingFrame(f);
			resizeDesktop();
		}

		@Override
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
			int paX = 0;
			int paY = 0;
			JScrollPane scrollPane = getScrollPane();
			Insets scrollInsets = getScrollPaneInsets();

			if (scrollPane != null) {
				boolean isMaximized = false;
				JInternalFrame allFrames[] = desktop.getAllFrames();
				for (int i = 0; i < allFrames.length; i++) {
					if (allFrames[i].isVisible()) {
						if (allFrames[i].isMaximum()) {
							isMaximized = true;
						}
						Rectangle bounds = allFrames[i].getBounds();
						if (bounds.getX() + bounds.getWidth() > x) {
							x = (int) (bounds.getX() + bounds.getWidth());
						}
						if (bounds.getY() + bounds.getHeight() > y) {
							y = (int) (bounds.getY() + bounds.getHeight());
						}
						if (bounds.getX() + bounds.getWidth() > paX) {
							paX = (int) (bounds.getX() + bounds.getWidth());
						}
						if (bounds.getY() + bounds.getHeight() > paY) {
							paY = (int) (bounds.getY() + bounds.getHeight());
						}
						bounds = desktopAnimation.getIFrameBounds(allFrames[i]);
						if (bounds.getX() + bounds.getWidth() > paX) {
							paX = (int) (bounds.getX() + bounds.getWidth());
						}
						if (bounds.getY() + bounds.getHeight() > paY) {
							paY = (int) (bounds.getY() + bounds.getHeight());
						}
					}
				}
				Dimension d = scrollPane.getVisibleRect().getSize();
				if (scrollPane.getBorder() != null) {
					d.setSize(d.getWidth() - scrollInsets.left - scrollInsets.right, d.getHeight() - scrollInsets.top - scrollInsets.bottom);
				}

				minimumDesktopSize = new Dimension(Math.max(paX, x), Math.max(paY, y));
				if (minimumDesktopSize.getWidth() == 0 || minimumDesktopSize.getHeight() == 0) {
					minimumDesktopSize = null;
				}
				if (x <= d.getWidth() || isMaximized)
					x = ((int) d.getWidth()) - 20;
				if (y <= d.getHeight() || isMaximized)
					y = ((int) d.getHeight()) - 20;
				postAnimationDesktopSize = new Dimension(Math.max(paX, x), Math.max(paY, y));
				if (desktop.setAllSize(x, y) && !desktopAnimation.isActive()) {
					scrollPane.invalidate();
					scrollPane.validate();
				}
				if (!isMaximized) {
					UIUtil.invokeLater(() -> desktopViewPos = getScrollPane().getViewport().getViewPosition());
				}
			}
		}
	}

	private Dimension minimumDesktopSize;
	private Point desktopViewPos;
	
	public Dimension getMinimumDesktopSize() {
		return minimumDesktopSize != null? minimumDesktopSize : getSize();
	}

	public synchronized void stop() {
		running = false;
		desktops.remove(this);
		for (RowBrowser rb : tableBrowsers) {
			rb.browserContentPane.cancelLoadJob(false);
			rb.browserContentPane.destroy();
		}
		if (session != null) {
			Thread thread = new Thread(new Runnable() {
				@Override
				public void run() {
					session.shutDown();
				}
			});
			UIUtil.startDemon(thread);
		}
	}

	private final DataBrowser parentFrame;

	public static enum LayoutMode {
		THUMBNAIL(0.22, false),
		TINY(0.57, false), 
		L2(0.66, true),
		SMALL(0.75, false), 
		L3(0.87, true),
		MEDIUM(1.0, false), 
		L6(1.13, true),
		L7(1.26, true),
		LARGE(1.4, false);

		public final double factor;
		public final boolean anonym;

		private LayoutMode(double factor, boolean anonym) {
			this.factor = factor;
			this.anonym = anonym;
		}
	}

	LayoutMode layoutMode = LayoutMode.MEDIUM;

	private boolean layouting = false;
	
	public void layoutBrowser(JInternalFrame selectedFrame, boolean scrollToCenter, RowBrowser anchor) {
		if (layouting) {
			return;
		}
		
		try {
			layouting = true;
			if (selectedFrame == null) {
				selectedFrame = getSelectedFrame();
			}
			List<RowBrowser> all = new ArrayList<RowBrowser>(tableBrowsers);
			// layout(all, 0);
	
			optimizeLayout(anchor);
	
			all.clear();
			int maxH = 0;
			for (RowBrowser rb : tableBrowsers) {
				if (rb.browserContentPane.table instanceof BrowserContentPane.SqlStatementTable) {
					all.add(rb);
				} else {
					maxH = Math.max(maxH, rb.internalFrame.getBounds().y + rb.internalFrame.getBounds().height);
				}
			}
			layout(all, maxH + (int) (16 * layoutMode.factor));
	
			checkDesktopSize();
			if (selectedFrame != null) {
				try {
					selectedFrame.setSelected(true);
				} catch (PropertyVetoException e) {
					// ignore
				}
				if (scrollToCenter) {
					this.scrollToCenter(selectedFrame);
				}
			}
		} finally {
			layouting = false;
		}
	}
	
	private void layout(List<RowBrowser> toLayout, int maxH) {
		List<RowBrowser> roots = new ArrayList<RowBrowser>();
		for (RowBrowser rb : toLayout) {
			if (rb.parent == null) {
				roots.add(rb);
			}
		}
		while (!roots.isEmpty()) {
			List<RowBrowser> nextColumn = new ArrayList<RowBrowser>();
			int i = 0;
			for (RowBrowser rb : roots) {
				try {
					rb.internalFrame.setMaximum(false);
				} catch (PropertyVetoException e) {
					// ignore
				}
				int xPosition = -1;
				if (maxH > 0) {
					xPosition = i;
				}
				rb.internalFrame.setBounds(layout(rb.parent, rb.association, rb.browserContentPane, toLayout, maxH, xPosition));
				rb.browserContentPane.adjustRowTableColumnsWidth(null, null);
				toLayout.remove(rb);
				for (RowBrowser rbc : toLayout) {
					if (rbc.parent == rb) {
						nextColumn.add(rbc);
					}
				}
				++i;
			}
			roots = nextColumn;
		}
	}

	/**
	 * Experimental layout optimization.
	 * @param anchor 
	 */
	private void optimizeLayout(RowBrowser anchor) {
		Set<RowBrowser> anchors = new HashSet<RowBrowser>();
		while (anchor != null) {
			anchors.add(anchor);
			anchor = anchor.parent;
		}
		TreeLayoutOptimizer.Node<RowBrowser> root = new TreeLayoutOptimizer.Node<RowBrowser>(null, false);
		collectChildren(root, anchors);
		TreeLayoutOptimizer.optimizeTreeLayout(root);
		arrangeNodes(root);
	}

	private void collectChildren(Node<RowBrowser> root, Set<RowBrowser> anchors) {
		List<RowBrowser> children;
		if (root.getUserObject() == null) {
			children = getRootBrowsers(true);
		} else {
			children = getChildBrowsers(root.getUserObject(), true);
		}
		for (RowBrowser rb : children) {
			if (rb.browserContentPane.table instanceof BrowserContentPane.SqlStatementTable) {
				continue;
			}
			TreeLayoutOptimizer.Node<RowBrowser> childNode = new TreeLayoutOptimizer.Node<RowBrowser>(rb, anchors.contains(rb));
			root.addChild(childNode);
			collectChildren(childNode, anchors);
		}
	}

	private void arrangeNodes(Node<RowBrowser> root) {
		if (root.getUserObject() != null) {
			JInternalFrame iFrame = root.getUserObject().internalFrame;
			int x = (int) (BROWSERTABLE_DEFAULT_MIN_X * layoutMode.factor); // lgtm [java/evaluation-to-constant]
			int y = (int) (BROWSERTABLE_DEFAULT_MIN_Y * layoutMode.factor);
			x += (int) ((root.getLevel() - 1) * (int) ((BROWSERTABLE_DEFAULT_WIDTH + BROWSERTABLE_DEFAULT_DISTANCE) * layoutMode.factor));
			y += (int) (root.getPosition() * (BROWSERTABLE_DEFAULT_HEIGHT + 8) * layoutMode.factor);
			int h = (int) (BROWSERTABLE_DEFAULT_HEIGHT * layoutMode.factor);
			Rectangle r = new Rectangle(x, y, (int) (BROWSERTABLE_DEFAULT_WIDTH * layoutMode.factor), h);
			// iFrame.setBounds(r);
			desktopAnimation.setIFrameBounds(iFrame, root.getUserObject().browserContentPane, r, false);
		}
		for (Node<RowBrowser> child : root.getChildren()) {
			arrangeNodes(child);
		}
	}

	private Map<Rectangle, double[]> precBounds = new HashMap<Rectangle, double[]>();

	private static Collection<Desktop> desktops = new ArrayList<Desktop>();

	public void rescaleLayout(LayoutMode layoutMode, Point fixed) {
		double scale = layoutMode.factor / this.layoutMode.factor;

		if (fixed == null) {
			fixed = new Point(getVisibleRect().x + getVisibleRect().width / 2, getVisibleRect().y + getVisibleRect().height / 2);
		}

		try {
			UIUtil.setWaitCursor(this);
			this.layoutMode = layoutMode;
			Map<Rectangle, double[]> newPrecBounds = new HashMap<Rectangle, double[]>();
			for (RowBrowser rb : new ArrayList<RowBrowser>(tableBrowsers)) {
				if (rb.internalFrame.isMaximum()) {
					try {
						rb.internalFrame.setMaximum(false);
					} catch (PropertyVetoException e) {
						// ignore
					}
				}
				Rectangle bounds = desktopAnimation.getIFrameBounds(rb.internalFrame);
				Rectangle newBounds;
				double[] pBounds = precBounds.get(bounds);
				if (pBounds == null) {
					pBounds = new double[] { bounds.x * scale, bounds.y * scale, bounds.width * scale, bounds.height * scale };
				} else {
					pBounds = new double[] { pBounds[0] * scale, pBounds[1] * scale, pBounds[2] * scale, pBounds[3] * scale };
				}
				newBounds = new Rectangle((int) pBounds[0], (int) pBounds[1], (int) pBounds[2], (int) pBounds[3]);
				desktopAnimation.setIFrameBounds(rb.internalFrame, rb.browserContentPane, newBounds, true);
//				rb.browserContentPane.sortColumnsPanel.setVisible(LayoutMode.SMALL.factor <= layoutMode.factor);
				newPrecBounds.put(newBounds, pBounds);
			}
			precBounds = newPrecBounds;
			manager.resizeDesktop();

			Rectangle vr = new Rectangle(Math.max(0, (int) (fixed.x * scale - getVisibleRect().width / 2)), Math.max(0,
					(int) (fixed.y * scale - getVisibleRect().height / 2)), getVisibleRect().width, getVisibleRect().height);
			desktopAnimation.scrollRectToVisible(vr, true);
			updateMenu(layoutMode);
			adjustClosure(null, null);
		} finally {
			UIUtil.resetWaitCursor(this);
		}
	}

	private JScrollPane prevScrollPane = null;
	private long prevScrollTime = 0;
	
	void onMouseWheelMoved(java.awt.event.MouseWheelEvent e, JScrollPane scrollPane, long currentTime) {
		if (!inRescaleMode(currentTime, e)) {
			Container parentScrollPane = getParent();
			while (parentScrollPane != null && !(parentScrollPane instanceof JScrollPane)) {
				parentScrollPane = parentScrollPane.getParent();
			}
			if (prevScrollPane != null && prevScrollPane == parentScrollPane && currentTime - prevScrollTime < 350) {
				scrollPane = prevScrollPane;
			}
			if ((e.getScrollAmount() != 0) && (e.getScrollType() == MouseWheelEvent.WHEEL_UNIT_SCROLL)) {
				JScrollBar toScroll = scrollPane.getVerticalScrollBar();
				int direction = 0;

				// find which scrollbar to scroll, or return if none
				if ((e.getModifiers() & InputEvent.SHIFT_MASK) != 0) {
					toScroll = scrollPane.getHorizontalScrollBar();
				}
				if ((toScroll == null) || !toScroll.isVisible()) {
					return;
				}

				prevScrollPane = scrollPane;
				prevScrollTime = currentTime;

				if (e.getWheelRotation() != 0) {
					direction = (e.getWheelRotation() < 0) ? (-1) : 1;
				}
			
				double f = 1.0;
				
				double pwr = e.getPreciseWheelRotation();
				if (pwr != 0.0) {
					direction = pwr == 0? 0 : (pwr < 0) ? (-1) : 1;
					f = Math.abs(pwr);
				}

				if (direction != 0) {
					int oldValue = toScroll.getValue();
					int blockIncrement = toScroll.getUnitIncrement(direction);
					// allow for partial page overlapping
					// blockIncrement -= 10;
					int delta = (int) (f * blockIncrement * ((direction > 0) ? +1 : -1));
					int newValue = oldValue + delta;
	
					// Check for overflow.
					if ((delta > 0) && (newValue < oldValue)) {
						newValue = toScroll.getMaximum();
					} else if ((delta < 0) && (newValue > oldValue)) {
						newValue = toScroll.getMinimum();
					}
	
					toScroll.setValue(newValue);
				}
			}
		}
	}

	void onMouseWheelMoved(int x, int y, int wheelRotation, Component component, long currentTime, MouseWheelEvent evt) {
		if (inRescaleMode(currentTime, evt)) {
			int d = 0;
			if (wheelRotation < 0) {
				d = 1;
			}
			if (wheelRotation > 0) {
				d = -1;
			}
			if (d != 0) {
				for (RowBrowser rb : new ArrayList<RowBrowser>(tableBrowsers)) {
					if (rb.internalFrame.isMaximum()) {
						return;
					}
				}
				d += layoutMode.ordinal();
				if (d >= 0 && d < LayoutMode.values().length) {
					Point fixed = SwingUtilities.convertPoint(component, new Point(x, y), Desktop.this);
					rescaleLayout(LayoutMode.values()[d], fixed);
					rescaleFactorHasChanged = true;
				}
			}
		}
	}

	public void closeAll() {
		closeAll(new ArrayList<RowBrowser>(tableBrowsers));
	}

	public void closeAll(Collection<RowBrowser> toClose) {
		for (RowBrowser rb : toClose) {
			close(rb, toClose.size() == 1);
			if (rb.browserContentPane != null) {
				rb.browserContentPane.destroy();
			}
			rb.internalFrame.dispose();
		}
		updateMenu();
		checkHAlignedPath();
		JInternalFrame first = null;
		for (RowBrowser tb : tableBrowsers) {
			if (first == null) {
				first = tb.internalFrame;
			}
			if (tb.internalFrame != null && tb.internalFrame.isSelected()) {
				first = null;
				break;
			}
		}
		if (first != null) {
			try {
				first.setSelected(true);
			} catch (PropertyVetoException e) {
				// ignore
			}
		}
	}

	private void close(final RowBrowser tableBrowser, boolean convertChildrenToRoots) {
		if (desktopUndoManager != null && tableBrowsers.contains(tableBrowser)) {
			desktopUndoManager.beforeModification("Add \"" + tableBrowser.internalFrame.getTitle().replaceFirst("\\s*\\(\\d+\\)$", "") + "\"", "Remove \"" + tableBrowser.internalFrame.getTitle().replaceFirst("\\s*\\(\\d+\\)$", "") + "\"");
		}
		checkHAlignedPath();
		List<RowBrowser> children = new ArrayList<RowBrowser>();
		for (RowBrowser tb : tableBrowsers) {
			if (tb.parent == tableBrowser) {
				tb.parent = null;
				children.add(tb);
			}
		}
		tableBrowsers.remove(tableBrowser);
		tableBrowser.browserContentPane.cancelLoadJob(true);
		if (convertChildrenToRoots) {
			for (RowBrowser child : children) {
				child.convertToRoot();
			}
		}
		for (RowBrowser rb : tableBrowsers) {
			updateChildren(rb, rb.browserContentPane.rows);
		}
		repaintDesktop();
		updateMenu();
	}

	private long checkStopTS;
	
	void checkHAlignedPath() {
		if (checkStopTS == 0 || checkStopTS + 500 < System.currentTimeMillis()) {
			if (!desktopAnimation.isActive() && desktopAnimation.getInactiveTime() > 500) {
				rowsClosure.hAlignedPath.clear();
			}
		}
		checkHAlignButtons();
	}

	private boolean checkPending = false;
	
	private void checkHAlignButtons() {
		if (desktopAnimation.isActive()) {
			UIUtil.invokeLater(() -> {
				checkHAlignButtons();
			});
		} else {
			if (!checkPending) {
				checkPending = true;
				UIUtil.invokeLater(() -> {
					checkPending = false;
					this.tableBrowsers.forEach(tb -> tb.browserContentPane.setHAlignButtonEnabled(anchorManager.isApplicable(tb)));
				});
			}
		}
	}

	/**
	 * Reloads the data model and replaces the tables in all browser windows.
	 */
	public void reloadDataModel(Map<String, String> schemamapping) throws Exception {
		reloadDataModel(schemamapping, true, true, true);
	}

	/**
	 * Reloads the data model and replaces the tables in all browser windows.
	 * @param restoreSess 
	 * @param newSession 
	 */
	public void reloadDataModel(Map<String, String> schemamapping, boolean restoreSess, boolean forAll, boolean newSession) throws Exception {
		if (forAll) {
			for (Desktop desktop : desktops) {
				if (desktop != this) {
					desktop.reloadDataModel(desktop.schemaMapping, restoreSess, false, newSession);
				}
			}
		}

		try {
			removeMetaDataSource(session);
			Component pFrame = SwingUtilities.getWindowAncestor(this);
			if (pFrame == null) {
				pFrame = this;
			}
			String filename = Environment.newFile(".tempsession-" + System.currentTimeMillis()).getPath();
			if (restoreSess) {
				storeSession(filename);
			}
			
			DataModel newModel = new DataModel(schemamapping, executionContext, false);
			datamodel.set(newModel);
			UISettings.dmStats(newModel);

			if (newSession) {
				onNewDataModel();
			}
			
			if (restoreSess) {
				restoreSession(null, pFrame, filename);
				File file = new File(filename);
				file.delete();
			}
		} catch (Throwable e) {
			UIUtil.showException(this, "Error", e, session);
		}
	}

	/**
	 * Reloads the rows in all root-table-browsers.
	 */
	public void reloadRoots() throws Exception {
		for (RowBrowser rb : tableBrowsers) {
			if (rb.browserContentPane != null) {
				if (rb.parent == null) {
					rb.browserContentPane.reloadRows();
				}
			}
		}
	}

	private boolean loadSchemaMapping = true;

	public abstract void openSchemaAnalyzer();
	public abstract void onNewDataModel();
	public abstract void onLayoutChanged(boolean isLayouted, boolean scrollToCenter);
	public abstract void updateBookmarksMenu();
	protected abstract void repaintOutline();
	protected abstract boolean desktopOutlineDraggingInProgress();
	protected abstract void openGlobalPopup(MouseEvent e);
	protected abstract void openConditionEditor(BrowserContentPane browserContentPane, Point location, int column, Runnable onClose);
	
	public void openSchemaMappingDialog(boolean silent) {
		try {
			Map<String, String> mapping = schemaMapping;
			if (loadSchemaMapping || silent) {
				mapping = SchemaMappingDialog.restore(dbConnectionDialog);
				loadSchemaMapping = false;
			}
			if (!silent) {
				SchemaMappingDialog schemaMappingDialog = new SchemaMappingDialog(parentFrame, datamodel.get(), dbConnectionDialog, session, mapping, executionContext);
				mapping = schemaMappingDialog.getMapping();
			}
			if (mapping != null && !mapping.isEmpty()) {
				SchemaMappingDialog.store(mapping, dbConnectionDialog);
				schemaMapping.clear();
				schemaMapping.putAll(mapping);
				parentFrame.updateStatusBar();
				reloadDataModel(mapping, true, !silent, true);
				reloadRoots();
			}
		} catch (Exception e) {
			UIUtil.showException(this, "Error", e, session);
		}
	}

	/**
	 * Lets user chose a table browser and creates an extraction model for it.
	 */
	public void createExtractionModel(boolean doExport) {
		Set<String> titles = new TreeSet<String>();
		Map<String, RowBrowser> rowBrowserByTitle = new HashMap<String, Desktop.RowBrowser>();
		for (RowBrowser rb : tableBrowsers) {
			if (rb.browserContentPane.table != null && !(rb.browserContentPane.table instanceof BrowserContentPane.SqlStatementTable)) {
				titles.add(rb.internalFrame.getTitle());
				rowBrowserByTitle.put(rb.internalFrame.getTitle(), rb);
			}
		}
		String s = (String) JOptionPane.showInputDialog(this.parentFrame, "Select subject table", "Subject", JOptionPane.QUESTION_MESSAGE, null,
				titles.toArray(), null);
		if (s != null) {
			rowBrowserByTitle.get(s).browserContentPane.openExtractionModelEditor(doExport);
		}
	}

	private AtomicBoolean updateMenuPending = new AtomicBoolean(false);
	
	void updateMenu() {
		if (updateMenuPending.get()) {
			return;
		}
		UIUtil.invokeLater(4, new Runnable() {
			@Override
			public void run() {
				updateMenuPending.set(false);

				boolean hasTableBrowser = false;
				boolean hasIFrame = false;
		
				for (RowBrowser rb : tableBrowsers) {
					hasIFrame = true;
					if (!(rb.browserContentPane.table instanceof BrowserContentPane.SqlStatementTable)) {
						hasTableBrowser = true;
					}
				}
				updateMenu(hasTableBrowser, hasIFrame);
			}
		});
		updateMenuPending.set(true);
	}

	protected void updateMenu(boolean hasTableBrowser, boolean hasIFrame) {
		if (!hasIFrame) {
			if (!hasTableBrowser) {
				currentSessionFileName = null;
			}
		}
	}

	protected abstract void updateMenu(LayoutMode layoutMode);

	private final String LF = System.getProperty("line.separator", "\n");
	private String currentSessionFileName = null;

	/**
	 * Stores browser session.
	 */
	public void storeSession(BookmarksPanel bookmarksPanel) {
		String fnProp = null;
		Map<String, Integer> lOfProp = new HashMap<>();
		Set<String> props = new TreeSet<String>((a, b) -> {
			int d = lOfProp.getOrDefault(b, 0) - lOfProp.getOrDefault(a, 0);
			if (d == 0) {
				d = a.compareToIgnoreCase(b);
			}
			return d;
		});
		int propLen = 0;
		final String INVALID_CHARS = "[:;\\*\\?<>'`\"/\\\\\\~]+";
		for (RowBrowser rb : tableBrowsers) {
			if (rb.browserContentPane.table != null) {
				if (!(rb.browserContentPane.table instanceof BrowserContentPane.SqlStatementTable)) {
					int l = 1;
					RowBrowser parent;
					for (parent = rb; parent.parent != null; parent = parent.parent) {
						++l;
					}
					String prop = datamodel.get().getDisplayName(parent.browserContentPane.table).replaceAll(INVALID_CHARS, " ").trim();
					if (parent != rb) {
						prop += " - " + datamodel.get().getDisplayName(rb.browserContentPane.table).replaceAll(INVALID_CHARS, " ").trim();
						lOfProp.put(prop, l);
						props.add(prop);
					}
					if (l > propLen || fnProp == null || l == propLen && fnProp.compareTo(prop) < 0) {
						fnProp = prop;
						propLen = l;
					}
				}
			}
		}

		if (fnProp != null && bookmarksPanel == null) {
			fnProp += ".dbl";
		}

		if (bookmarksPanel == null) {
			if (currentSessionFileName != null) {
				fnProp = currentSessionFileName;
			}
		}

		String sFile;
		
		if (bookmarksPanel != null) {
			File startDir = BookmarksPanel.getBookmarksFolder(executionContext);
			sFile = bookmarksPanel.newBookmark(fnProp, props);
			if (sFile != null) {
				File f = new File(startDir, sFile + ".dbl");
				startDir.mkdirs();
				sFile = f.getAbsolutePath();
			}
		} else {
			File startDir = Environment.newFile("layout");
			Component pFrame = SwingUtilities.getWindowAncestor(this);
			if (pFrame == null) {
				pFrame = this;
			}
			sFile = UIUtil.choseFile(fnProp == null ? null : new File(startDir, fnProp), startDir.getPath(), "Store Layout", ".dbl", pFrame, true, false);
		}

		if (sFile != null) {
			try {
				storeSession(sFile);
			} catch (Throwable e) {
				UIUtil.showException(this, "Error", e, session);
			}
			if (bookmarksPanel == null) {
				currentSessionFileName = sFile;
			} else {
				if (new File(sFile).exists()) {
					new File(sFile).setLastModified(System.currentTimeMillis());
				}
				bookmarksPanel.updateBookmarksMenu();
				updateAllBookmarkMenues();
			}
		}
	}

	public void updateAllBookmarkMenues() {
		for (Desktop dTop: desktops) {
			dTop.updateBookmarksMenu();
		}
	}

	/**
	 * Stores browser session.
	 */
	void storeSession(String sFile) throws IOException {
		int i = 1;
		Map<RowBrowser, Integer> browserNumber = new HashMap<Desktop.RowBrowser, Integer>();
		for (RowBrowser rb : tableBrowsers) {
			browserNumber.put(rb, i++);
		}
		
		FileWriter out = new FileWriter(new File(sFile));

		out.write("Layout; " + layoutMode + "; " + parentFrame.autoLayoutMenuItem.isSelected() + LF);

		for (RowBrowser rb : tableBrowsers) {
			if (rb.parent == null) {
				storeSession(rb, browserNumber, out);
			}
		}
		out.close();
	}

	/**
	 * Recursively stores row-browser session.
	 */
	private void storeSession(RowBrowser rb, Map<RowBrowser, Integer> browserNumber, FileWriter out) throws IOException {
		if (rb.browserContentPane.table != null) {
			boolean maximum = rb.internalFrame.isMaximum();
			
			if (maximum) {
				try {
					rb.internalFrame.setMaximum(false);
				} catch (PropertyVetoException e) {
					// ignore
				}
			}
			
			String csv = browserNumber.get(rb) + "; " + (rb.parent == null ? "" : browserNumber.get(rb.parent)) + "; ";

			String where = rb.browserContentPane.getAndConditionText().trim();

			csv += CsvFile.encodeCell(where) + "; ";

			csv += rb.internalFrame.getLocation().x + "; " + rb.internalFrame.getLocation().y + "; ";
			csv += rb.internalFrame.getSize().width + "; " + rb.internalFrame.getSize().height + "; ";
			csv += (rb.browserContentPane.ownLimit == null? "" : rb.browserContentPane.ownLimit) + "; " + rb.browserContentPane.selectDistinctCheckBox.isSelected() + "; ";

			if (!(rb.browserContentPane.table instanceof BrowserContentPane.SqlStatementTable)) {
				csv += "T; " + CsvFile.encodeCell(rb.browserContentPane.table.getOriginalName()) + "; "
						+ (rb.association == null ? "" : CsvFile.encodeCell(rb.association.getName())) + "; ";
			}
			csv += rb.isHidden() + "; ";
			csv += rb.browserContentPane.ignoreSortKey? "" : serializedSortKey(rb.browserContentPane.rowsTable) + "; ";
			csv += maximum + ";";
			if (maximum) {
				try {
					rb.internalFrame.setMaximum(true);
				} catch (PropertyVetoException e) {
					// ignore
				}
			}
			csv += (rb.browserContentPane.filteredColumns == null? "" : rb.browserContentPane.filteredColumns.stream().map(c -> String.valueOf(c)).collect(Collectors.joining(","))) + ";";

			out.append(csv).append(LF);
			for (RowBrowser child : tableBrowsers) {
				if (child.parent == rb) {
					storeSession(child, browserNumber, out);
				}
			}
		}
	}

	private String serializedSortKey(JTable jTable) {
		if (jTable != null) {
			try {
				List<? extends SortKey> sortKeys = jTable.getRowSorter().getSortKeys();
				StringBuilder sb = new StringBuilder();
				for (SortKey sortKey: sortKeys) {
					if (sb.length() > 0) {
						sb.append(",");
					}
					sb.append(sortKey.getColumn() + "|" + sortKey.getSortOrder());
				}
				return sb.toString();
			} catch (Exception e) {
				return null; // sort keys are lost
			}
		}
		return null;
	}

	private void deserializedSortKey(JTable jTable, String externaliuedSortKey) {
		if (jTable != null && externaliuedSortKey != null && !externaliuedSortKey.trim().isEmpty()) {
			try {
				List<SortKey> sortKeys = new ArrayList<SortKey>();
				for (String externalizedKey: externaliuedSortKey.trim().split(",")) {
					String[] columnOrder = externalizedKey.trim().split("\\|");
					if (columnOrder.length == 2) {
						sortKeys.add(new SortKey(Integer.parseInt(columnOrder[0]), SortOrder.valueOf(columnOrder[1])));
					}
				}
				if (!sortKeys.isEmpty()) {
					jTable.getRowSorter().setSortKeys(sortKeys);
				}
			} catch (Exception e) {
				// ignore
			}
		}
	}

	/**
	 * Restores browser session.
	 * @param bookMarkFile 
	 */
	public void restoreSession(RowBrowser toBeAppended, File bookMarkFile) {
		File startDir = Environment.newFile("layout");
		Component pFrame = SwingUtilities.getWindowAncestor(this);
		if (pFrame == null) {
			pFrame = this;
		}
		String sFile = bookMarkFile != null? bookMarkFile.getAbsolutePath() : UIUtil.choseFile(null, startDir.getPath(), toBeAppended == null ? "Restore Layout" : "Append Layout", ".dbl", pFrame, true, true);
		
		if (sFile != null) {
			try {
				UIUtil.setWaitCursor(pFrame);
				noArrangeLayoutOnNewTableBrowser = true;
				restoreSession(toBeAppended, pFrame, sFile);
				if (toBeAppended == null) {
					currentSessionFileName = sFile;
				}
			} catch (Throwable e) {
				UIUtil.showException(this, "Error", e, session);
			} finally {
				noArrangeLayoutOnNewTableBrowser = false;
				UIUtil.resetWaitCursor(pFrame);
			}
		}
	}

	/**
	 * Restores browser session.
	 */
	private void restoreSession(RowBrowser toBeAppended, Component pFrame, String sFile) throws Exception {
		restoreSession(toBeAppended, pFrame, sFile, true);
	}
	
	/**
	 * Restores browser session.
	 */
	void restoreSession(RowBrowser toBeAppended, Component pFrame, String sFile, boolean restoreGlobalSettings) throws Exception {
		layouting = true;
		try {
			UIUtil.setWaitCursor(pFrame);
			editorPanesCache.clear();
			iFrameStateChangeRenderer.startAtomic();
			noArrangeLayoutOnNewTableBrowser = true;
			if (desktopUndoManager != null) {
				desktopUndoManager.beforeRestore();
			}
			
			String tbaPeerID = null;
			Map<String, RowBrowser> rbByID = new HashMap<String, Desktop.RowBrowser>();
			List<Line> lines = new CsvFile(new File(sFile)).getLines();
			if (toBeAppended == null) {
				closeAll();
			}
			Collection<RowBrowser> toBeLoaded = new ArrayList<Desktop.RowBrowser>();
			List<String> unknownTables = new ArrayList<String>();
			KnownIdentifierMap knownTablesMap = new KnownIdentifierMap();
			Map<String, Table> originalNameToTable = new HashMap<String, Table>();
			for (Table table: datamodel.get().getTables()) {
				knownTablesMap.putTableName(table.getOriginalName());
				originalNameToTable.put(table.getOriginalName(), table);
			}
			JInternalFrame toBeMaximized = null;
			for (CsvFile.Line l : lines) {
				if (l.cells.get(0).equals("Layout")) {
					try {
						if (toBeAppended == null) {
							layoutMode = LayoutMode.valueOf(l.cells.get(1));
							updateMenu(layoutMode);
						}
						if (restoreGlobalSettings) {
							parentFrame.autoLayoutMenuItem.setSelected(!"false".equalsIgnoreCase(l.cells.get(2)));
						}
					} catch (Exception e) {
						e.printStackTrace();
					}
					continue;
				}
	
				String id = l.cells.get(0);
				String parent = l.cells.get(1);
				String where = l.cells.get(2);
				Point loc = new Point(Integer.parseInt(l.cells.get(3)), Integer.parseInt(l.cells.get(4)));
				Dimension size = new Dimension(Integer.parseInt(l.cells.get(5)), Integer.parseInt(l.cells.get(6)));
				String ownLimitRaw = l.cells.get(7);
				if ("500".equals(ownLimitRaw)) {
					ownLimitRaw = ""; // legacy
				}
				Integer ownLimit = ownLimitRaw.isEmpty()? null : Integer.parseInt(ownLimitRaw);
				boolean selectDistinct = Boolean.parseBoolean(l.cells.get(8));
				RowBrowser rb = null;
				if ("T".equals(l.cells.get(9))) {
					Table table = originalNameToTable.get(l.cells.get(10));
					if (table == null) {
						String kt = knownTablesMap.getTableName(l.cells.get(10));
						if (kt != null) {
							table = originalNameToTable.get(kt);
						}
					}
					if (table == null) {
						unknownTables.add(l.cells.get(10));
					} else {
						Association association = datamodel.get().namedAssociations.get(l.cells.get(11));
						RowBrowser parentRB = rbByID.get(parent);
						if (association == null) {
							parentRB = null;
						}
						boolean add = true;
						if (toBeAppended != null) {
							if (tbaPeerID == null) {
								add = false;
								if (parent.trim().length() == 0 && table.equals(toBeAppended.browserContentPane.table)) {
									tbaPeerID = id;
								}
							} else {
								if (tbaPeerID.equals(parent)) {
									parentRB = toBeAppended;
								} else if (!rbByID.containsKey(parent)) {
									add = false;
								}
							}
						}
						if (add) {
							rb = addTableBrowser(parentRB, parentRB, table, parentRB != null ? association : null, where, selectDistinct, null, false);
							if (id.length() > 0) {
								rbByID.put(id, rb);
							}
							if (parentRB == null || parentRB == toBeAppended) {
								toBeLoaded.add(rb);
							}
						}
					}
				} else {
					if (toBeAppended == null) {
						rb = addTableBrowser(null, null, null, null, where, selectDistinct, null, false);
						toBeLoaded.add(rb);
					}
				}
				if (rb != null) {
					rb.browserContentPane.ownLimit = ownLimit;
					rb.setHidden(Boolean.parseBoolean(l.cells.get(12)));
					if (toBeAppended == null) {
						rb.internalFrame.setLocation(loc);
						rb.internalFrame.setSize(size);
						if (Boolean.parseBoolean(l.cells.get(14))) {
							toBeMaximized = rb.internalFrame;
						}
					}
					if (!l.cells.get(15).isEmpty()) {
						rb.browserContentPane.filteredColumns = new HashSet<>();
						for (String c: l.cells.get(15).split(",")) {
							try {
								rb.browserContentPane.filteredColumns.add(Integer.parseInt(c));
							} catch (Exception e) {
								// ignore
							}
						}
					}
					deserializedSortKey(rb.browserContentPane.rowsTable, l.cells.get(13));
					if (sFile.endsWith("INVENTORY - ACTOR.dbl")
							&& rb.browserContentPane.table != null
							&& "FILM".equals(rb.browserContentPane.table.getName())) { // Demo
						Timer timer = new Timer(1000, null);
						RowBrowser rowBrowser = rb;
						timer.addActionListener(e -> {
							if (rowBrowser.browserContentPane.rowsTable.getRowCount() > 0) {
								rowBrowser.browserContentPane.rowsTable.getSelectionModel().setSelectionInterval(0, 0);
								try {
									rowBrowser.internalFrame.setSelected(true);
								} catch (PropertyVetoException e1) {
									// ignore
								}
								timer.stop();
							}
						});
						timer.setDelay(100);
						timer.start();
					}
				}
			}
			checkDesktopSize();
			if (toBeMaximized != null) {
				JInternalFrame maximize = toBeMaximized;
				UIUtil.invokeLater(2, () -> {
					try {
						maximize.setMaximum(true);
					} catch (PropertyVetoException e) {
						e.printStackTrace();
						// ignore
					}
				});
			}
			makePrimaryRootVisible();

			for (RowBrowser rb : toBeLoaded) {
				rb.browserContentPane.reloadRows();
			}

			if (toBeAppended != null && toBeLoaded.isEmpty()) {
				JOptionPane.showMessageDialog(pFrame,
						"Layout doesn't contain table \"" + datamodel.get().getDisplayName(toBeAppended.browserContentPane.table) + "\" as root.");
			} else if (!unknownTables.isEmpty() && showMissingTablesOnRestoreSession) {
				String pList = "";
				for (String ut : unknownTables) {
					pList += ut + "\n";
				}
				JOptionPane.showMessageDialog(pFrame, "Unknown tables:\n\n" + pList + "\n");
			}
		} finally {
			if (desktopUndoManager != null) {
				desktopUndoManager.afterRestore();;
			}
			noArrangeLayoutOnNewTableBrowser = false;
			iFrameStateChangeRenderer.rollbackAtomic();
			UIUtil.invokeLater(() -> { layouting = false; });
	        UIUtil.resetWaitCursor(pFrame);
		}
	}

	boolean showMissingTablesOnRestoreSession = true;
	
	private void makePrimaryRootVisible() {
		RowBrowser root = null;
		for (RowBrowser rb : getRootBrowsers(true)) {
			if (rb.browserContentPane.table != null) {
				if (!(rb.browserContentPane.table instanceof BrowserContentPane.SqlStatementTable)) {
					if (root == null || root.internalFrame.getY() > rb.internalFrame.getY()) {
						root = rb;
					}
				}
			}
		}
		if (root != null) {
			try {
				root.internalFrame.setSelected(true);
			} catch (PropertyVetoException e) {
				// ignore
			}
			this.scrollToCenter(root.internalFrame);
		} else {
			this.desktopAnimation.scrollRectToVisible(new Rectangle(0, 0, 1, 1), false);
		}
	}

	public JInternalFrame[] getAllFramesFromTableBrowsers() {
		List<JInternalFrame> frames = new ArrayList<JInternalFrame>();
		for (RowBrowser rb : tableBrowsers) {
			frames.add(rb.internalFrame);
		}
		return frames.toArray(new JInternalFrame[frames.size()]);
	}

	public List<RowBrowser> getRootBrowsers(boolean ignoreHidden) {
		List<RowBrowser> roots = new ArrayList<Desktop.RowBrowser>();

		if (ignoreHidden) {
			for (RowBrowser rb : tableBrowsers) {
				if (!rb.isHidden()) {
					RowBrowser p = rb.parent;
					while (p != null && p.isHidden()) {
						p = p.parent;
					}
					if (p == null) {
						roots.add(rb);
					}
				}
			}
		} else {
			for (RowBrowser rb : tableBrowsers) {
				if (rb.parent == null) {
					roots.add(rb);
				}
			}
		}
		return roots;
	}

	public List<RowBrowser> getBrowsers() {
		return new ArrayList<Desktop.RowBrowser>(tableBrowsers);
	}

	public List<RowBrowser> getChildBrowsers(RowBrowser parent, boolean ignoreHidden) {
		List<RowBrowser> roots = new ArrayList<Desktop.RowBrowser>();

		if (ignoreHidden) {
			for (RowBrowser rb : tableBrowsers) {
				if (rb.parent == parent) {
					if (rb.isHidden()) {
						roots.addAll(getChildBrowsers(rb, true));
					} else {
						roots.add(rb);
					}
				}
			}
		} else {
			for (RowBrowser rb : tableBrowsers) {
				if (rb.parent == parent) {
					roots.add(rb);
				}
			}
		}
		return roots;
	}

	public static Boolean forceAdjustRows = false;

	/**
	 * Adjusts scroll-position of each table browser s.t. rows in closure are
	 * visible.
	 * 
	 * @param tabu don't adjust this one
	 * @param thisOne only adjust this one if it is not <code>null</code>
	 */
	protected synchronized void adjustClosure(BrowserContentPane tabu, BrowserContentPane thisOne) {
		if (forceAdjustRows == null) {
			return;
		}
		if (tabu != null && thisOne == null) {
			if (pendingTabu == null) {
				pendingTabu = tabu;
				UIUtil.invokeLater(() -> {
					doAdjustClosure(pendingTabu, null);
					pendingTabu = null;
				});
			} else if (pendingTabu != tabu) {
				doAdjustClosure(tabu, thisOne);
			}
		} else {
			doAdjustClosure(tabu, thisOne);
		}
	}
	
	private BrowserContentPane pendingTabu = null;
	
	private synchronized void doAdjustClosure(BrowserContentPane tabu, BrowserContentPane thisOne) {
		for (RowBrowser rb : tableBrowsers) {
			if (rb.browserContentPane == tabu) {
				continue;
			}
			if (thisOne != null && rb.browserContentPane != thisOne) {
				continue;
			}

			Set<Row> rowsOfParent = new HashSet<Row>();
			if (rb.parent != null) {
				for (Pair<BrowserContentPane, Row> r : rowsClosure.currentClosure) {
					if (r.a == rb.parent.browserContentPane) {
						rowsOfParent.add(r.b);
					}
				}
			}

			List<Row> rowsOfRB = new ArrayList<Row>();
			Set<Row> rowsOfRBSet = new LinkedHashSet<Row>();
			for (RowToRowLink link: rb.rowToRowLinks) {
				if (rowsOfParent.contains(link.parentRow)) {
					rowsOfRBSet.add(link.childRow);
				}
			}
			for (Pair<BrowserContentPane, Row> r : rowsClosure.currentClosure) {
				if (r.a == rb.browserContentPane) {
					if (rowsOfRBSet.isEmpty() || rowsOfRBSet.contains(r.b)) {
						rowsOfRB.add(r.b);
					}
				}
			}

			int topBorder = -1;
			if (rb.parent != null) {
				Rectangle firstParentRowPos = null;
				for (Row r : rowsOfParent) {
					int index = rb.parent.browserContentPane.rows.indexOf(r);
					if (index < 0) {
						for (int n = 0; n < rb.parent.browserContentPane.rows.size(); ++n) {
							if (r.nonEmptyRowId.equals(rb.parent.browserContentPane.rows.get(n).nonEmptyRowId)) {
								index = n;
								break;
							}
						}
					}

					if (index < 0) {
						// not visible due to distinct selection
						continue;
					}
					index = rb.parent.browserContentPane.rowsTable.getRowSorter().convertRowIndexToView(index);
					Rectangle pos = rb.parent.browserContentPane.rowsTable.getCellRect(index, 0, false);

					if (firstParentRowPos == null || firstParentRowPos.y > pos.y) {
						firstParentRowPos = pos;
					}
				}
				if (firstParentRowPos != null) {
					Rectangle visibleRect = rb.parent.browserContentPane.rowsTable.getVisibleRect();
					if (!(firstParentRowPos.y > visibleRect.getMaxY() || firstParentRowPos.y < visibleRect.getMinY())) {
						topBorder = (int) (firstParentRowPos.y - visibleRect.getMinY());
					}
				}
			}
			
			if (!rowsOfRB.isEmpty()) {
				Rectangle firstRowPos = null;
				Rectangle lastRowPos = null;
				Rectangle visibleRect = rb.browserContentPane.rowsTable.getVisibleRect();
				for (Row r : rowsOfRB) {
					int index = rb.browserContentPane.rows.indexOf(r);
					if (index < 0) {
						for (int n = 0; n < rb.browserContentPane.rows.size(); ++n) {
							if (r.nonEmptyRowId.equals(rb.browserContentPane.rows.get(n).nonEmptyRowId)) {
								index = n;
								break;
							}
						}
					}

					if (index < 0) {
						// not visible due to distinct selection
						continue;
					}
					index = rb.browserContentPane.rowsTable.getRowSorter().convertRowIndexToView(index);
					Rectangle pos = rb.browserContentPane.rowsTable.getCellRect(index, 0, false);

					if (!forceAdjustRows && pos.y >= visibleRect.y && pos.y + pos.height < visibleRect.y + visibleRect.height) {
						// already a visible row
						firstRowPos = null;
						lastRowPos = null;
						break;
					}
					if (firstRowPos == null || firstRowPos.y > pos.y) {
						firstRowPos = pos;
					}
					if (lastRowPos == null || lastRowPos.y < pos.y) {
						lastRowPos = pos;
					}
				}
				if (lastRowPos != null && firstRowPos != null && rb.browserContentPane.rowsTable.getRowCount() > 0) {
					lastRowPos = rb.browserContentPane.rowsTable.getCellRect(rb.browserContentPane.rowsTable.getRowCount(), 0, false);
				}
				if (lastRowPos != null) {
					rb.browserContentPane.rowsTable
							.scrollRectToVisible(new Rectangle(visibleRect.x, lastRowPos.y - lastRowPos.height, 1, 3 * lastRowPos.height));
				}
				if (firstRowPos != null) {
					if (topBorder >= 0 && topBorder < firstRowPos.y && topBorder + 2 * firstRowPos.height < visibleRect.height) {
						rb.browserContentPane.rowsTable.scrollRectToVisible(new Rectangle(visibleRect.x, firstRowPos.y - topBorder, 1,
								topBorder + firstRowPos.height));
					} else if (topBorder >= 0) {
						rb.browserContentPane.rowsTable.scrollRectToVisible(new Rectangle(visibleRect.x, 0, 1,
								firstRowPos.y + firstRowPos.height));
					} else {
						rb.browserContentPane.rowsTable.scrollRectToVisible(new Rectangle(visibleRect.x, firstRowPos.y - firstRowPos.height, 1,
								3 * firstRowPos.height));
					}
				}
			}
		}
		for (RowBrowser rb : tableBrowsers) {
			rb.browserContentPane.updateSingleRowDetailsView();
		}
		
		rbSourceToLinks = null;
		repaintDesktop();
	}

	/**
	 * Opens new Browser and adds complete sub-tree of {@link RowBrowser}.
	 * 
	 * @param tableBrowser
	 *            the root
	 */
	private void showInNewWindow(RowBrowser tableBrowser) {
		DataBrowser newDataBrowser = openNewDataBrowser();
		if (newDataBrowser != null) {
			newDataBrowser.desktop.layoutMode = layoutMode;
			newDataBrowser.desktop.updateMenu(layoutMode);

			StringBuilder cond = new StringBuilder();
			Set<String> known = new HashSet<String>();
			boolean needsParen = false;
			synchronized (this) {
				if (tableBrowser.browserContentPane.getParentBrowser() != null) {
					for (Row r : tableBrowser.browserContentPane.rows) {
						if (!known.contains(r.rowId)) {
							known.add(r.rowId);
							if (cond.length() > 0) {
								cond.append(" or \n");
								needsParen = true;
							}
							cond.append("(" + SqlUtil.replaceAliases(r.rowId, "A", "A") + ")");
						}
					}
				} else {
					cond.append(tableBrowser.browserContentPane.getAndConditionText());
				}
			}

			RowBrowser root = addTableBrowserSubTree(newDataBrowser, tableBrowser, null, null, cond.length() > 0? needsParen? "(" + cond + ")" : cond.toString() : null);
			root.browserContentPane.reloadRows();
			newDataBrowser.arrangeLayout(true);
			try {
				JInternalFrame iFrame = root.internalFrame;
				newDataBrowser.desktop.scrollToCenter(iFrame);
				iFrame.setSelected(true);
				iFrame.grabFocus();
			} catch (PropertyVetoException e1) {
				// ignore
			}
		}
	}

	private RowBrowser addTableBrowserSubTree(DataBrowser newDataBrowser, RowBrowser tableBrowser, RowBrowser parent, RowBrowser origParent, String rootCond) {
		RowBrowser rb;
		if (parent == null) {
			rb = newDataBrowser.desktop.addTableBrowser(null, null, tableBrowser.browserContentPane.table, null,
					rootCond == null ? tableBrowser.browserContentPane.getAndConditionText() : rootCond,
					tableBrowser.browserContentPane.selectDistinctCheckBox.isSelected(), null, false);
		} else {
			rb = newDataBrowser.desktop.addTableBrowser(parent, origParent, tableBrowser.browserContentPane.table,
					tableBrowser.browserContentPane.association, rootCond == null ? tableBrowser.browserContentPane.getAndConditionText() : rootCond,
					tableBrowser.browserContentPane.selectDistinctCheckBox.isSelected(), null, false);
		}
		rb.setHidden(tableBrowser.isHidden());

		for (RowBrowser child : getChildBrowsers(tableBrowser, false)) {
			addTableBrowserSubTree(newDataBrowser, child, rb, tableBrowser, null);
		}
		return rb;
	}

	protected abstract DataBrowser openNewDataBrowser();
	protected abstract SQLConsole getSqlConsole(boolean switchToConsole);
	protected abstract boolean isDesktopVisible();
	protected abstract void checkAnchorRetension();
	protected abstract void changeColumnOrder(Table table);
	protected abstract void onRowSelect(Table table, Row row);
	protected abstract void onConditionChange(BrowserContentPane browserContentPane, String cond);
	protected abstract void onContentCellEditorCreated(BrowserContentPane browserContentPane, BrowserContentCellEditor cellEditor);
	protected abstract void loadScriptFile(String fileName);
	
	/**
	 * Scrolls an iFrame to the center of the desktop.
	 */
	public void scrollToCenter(JInternalFrame iFrame) {
		demaximize();
		int w = getVisibleRect().width;
		int h = getVisibleRect().height;
		Rectangle bounds = desktopAnimation.getIFrameBounds(iFrame);
		int x = bounds.x + bounds.width / 2 - getVisibleRect().width / 2;
		int y = bounds.y + bounds.height / 2 - getVisibleRect().height / 2;
		if (x < 0) {
			w += x;
			x = 0;
		}
		if (y < 0) {
			h += y;
			y = 0;
		}
		Rectangle r = new Rectangle(x, y, Math.max(1, w), Math.max(1, h));
		Rectangle vr = new Rectangle(postAnimationDesktopSize != null? postAnimationDesktopSize : currentDesktopnSize == null? getScrollPane().getViewport().getPreferredSize() : currentDesktopnSize);
		desktopAnimation.scrollRectToVisible(r.intersection(vr), false);
	}

	/**
	 * Collect layout of tables in a extraction model.
	 * 
	 * @param positions
	 *            to put positions into
	 */
	private void collectPositions(RowBrowser root, Map<String, Map<String, double[]>> positions) {
		List<Pair<RowBrowser, Pair<Integer, Integer>>> toDo = new LinkedList<Pair<RowBrowser, Pair<Integer, Integer>>>();
		toDo.add(new Pair<RowBrowser, Pair<Integer, Integer>>(root, new Pair<Integer, Integer>(1, 1)));
		String subject = root.browserContentPane.table.getName(); // datamodel.get().getDisplayName(root.browserContentPane.table);
		double scaleX = 0.35 / layoutMode.factor;
		double scaleY = 0.3 / layoutMode.factor;
		double scher = 2;

		while (!toDo.isEmpty()) {
			Pair<RowBrowser, Pair<Integer, Integer>> rowBrowser = toDo.remove(0);
			int i = 1;
			for (RowBrowser child : getChildBrowsers(rowBrowser.a, true)) {
				toDo.add(new Pair<RowBrowser, Pair<Integer, Integer>>(child, new Pair<Integer, Integer>(rowBrowser.b.a + 1, i++)));
			}
			String table = rowBrowser.a.browserContentPane.table.getName(); // datamodel.get().getDisplayName(rowBrowser.a.browserContentPane.table);
			Map<String, double[]> tablePos = positions.get(subject);
			if (tablePos == null) {
				tablePos = new TreeMap<String, double[]>();
				positions.put(subject, tablePos);
			}
			if (!tablePos.containsKey(table)) {
				double x = rowBrowser.a.internalFrame.getX();
				double y = rowBrowser.a.internalFrame.getY();
				tablePos.put(table, new double[] { x * scaleX + scher * (2 * (rowBrowser.b.b % 2) - 1), y * scaleY + scher * (2 * (rowBrowser.b.a % 2) - 1),
						1.0 });
			}
		}
	}

	public interface RunnableWithPriority extends Runnable {
		int getPriority();
	}

	/**
	 * For concurrent reload of rows.
	 */
	public static final PriorityBlockingQueue<RunnableWithPriority> runnableQueue = new PriorityBlockingQueue<RunnableWithPriority>(100,
		new Comparator<RunnableWithPriority>() {

			@Override
			public int compare(RunnableWithPriority o1,	RunnableWithPriority o2) {
				return o2.getPriority() - o1.getPriority();
			}
		});

	static boolean noArrangeLayoutOnNewTableBrowser = false;
	static boolean noArrangeLayoutOnNewTableBrowserWithAnchor = false;
	private static final AtomicReference<JInternalFrame> lastInternalFrame = new AtomicReference<JInternalFrame>();
	private static final AtomicReference<BrowserContentPane> lastBrowserContentPane = new AtomicReference<BrowserContentPane>();
	public void catchUpLastArrangeLayoutOnNewTableBrowser() {
		if (lastInternalFrame.get() != null) {
			this.scrollToCenter(lastInternalFrame.get());
			try {
				lastInternalFrame.get().setSelected(true);
			} catch (PropertyVetoException e1) {
				// ignore
			}
			if (lastBrowserContentPane.get() != null) {
				lastBrowserContentPane.get().andCondition.grabFocus();
			}
			onLayoutChanged(false, true);
		}
		resetLastArrangeLayoutOnNewTableBrowser();
	}

	public static void resetLastArrangeLayoutOnNewTableBrowser() {
		lastInternalFrame.set(null);
		lastBrowserContentPane.set(null);
	}

	private final int RESCALE_DURATION = 500;
	private Long rescaleModeEnd;
	private Point rescaleStartPosition;
	private boolean rescaleFactorHasChanged = false;

	public void startRescaleMode(long currentTime, int x, int y, Component component) {
		rescaleModeEnd = currentTime + RESCALE_DURATION;
		rescaleStartPosition = new Point(x, y);
		SwingUtilities.convertPointToScreen(rescaleStartPosition, component);
	}
	
	public void checkRescaleMode(MouseWheelEvent evt, long currentTime) {
//		if (inRescaleMode(currentTime)) {
//			if (rescaleStartPosition != null) {
//				Point position = new Point(evt.getX(),  evt.getY());
//				SwingUtilities.convertPointToScreen(position, evt.getComponent());
//				if (position.distance(rescaleStartPosition) > 1000) {
//					rescaleModeEnd = null;
//				}
//			}
//		}
	}

	protected abstract boolean isZoomWithMouseWheel();
	
	private boolean inRescaleMode(long currentTime, MouseWheelEvent e) {
		return (isZoomWithMouseWheel() || e == null || (UIUtil.isMacOS()? e.isMetaDown() : e.isControlDown())) && rescaleModeEnd != null && currentTime < rescaleModeEnd;
	}

	private void deferRescaleMode(long startTime) {
		if (rescaleModeEnd != null && rescaleFactorHasChanged) {
			long duration = System.currentTimeMillis() - startTime;
			rescaleModeEnd += duration;
		}
		rescaleFactorHasChanged = false;
	}

	private boolean animationEnabled = true;
	
	public boolean isAnimationEnabled() {
		return animationEnabled;
	}

	public void setAnimationEnabled(boolean animationEnabled) {
		this.animationEnabled = animationEnabled;
	}

	boolean strictZoom = false;

	public void zoom(int dir) {
		int d = layoutMode.ordinal();
		do {
			d += dir;
		} while (strictZoom && d >= 0 && d < LayoutMode.values().length && LayoutMode.values()[d].anonym);
		if (d >= 0 && d < LayoutMode.values().length) {
			rescaleLayout(LayoutMode.values()[d], null);
		}
	}

	public String getRawSchemaMapping() {
		StringBuilder mapping = new StringBuilder();
		for (String schema: schemaMapping.keySet()) {
			String to = schemaMapping.get(schema).trim();
			if (!schema.equals(to)) {
				if (mapping.length() > 0) {
					mapping.append(",");
				}
				mapping.append(schema + "=" + to);
			}
		}
		return mapping.toString();
	}

	private Row currentlyViewedRow = null;
	
	public static class FindClosureContext {
		Map<RowBrowser, Map<String, Collection<RowToRowLink>>> childRowsPartitions = new HashMap<Desktop.RowBrowser, Map<String,Collection<RowToRowLink>>>();
		Map<RowBrowser, Map<String, Collection<RowToRowLink>>> parentRowsPartitions = new HashMap<Desktop.RowBrowser, Map<String,Collection<RowToRowLink>>>();
		
		Collection<RowToRowLink> getChildPartition(RowBrowser browser, Row row) {
			Map<String, Collection<RowToRowLink>> partitionPerBrowser = childRowsPartitions.get(browser);
			if (partitionPerBrowser == null) {
				partitionPerBrowser = new HashMap<String, Collection<RowToRowLink>>();
				childRowsPartitions.put(browser, partitionPerBrowser);
				for (RowToRowLink link: browser.rowToRowLinks) {
					Collection<RowToRowLink> partition = partitionPerBrowser.get(link.childRow.nonEmptyRowId);
					if (partition == null) {
						partition = new ArrayList<RowToRowLink>();
						partitionPerBrowser.put(link.childRow.nonEmptyRowId, partition);
					}
					partition.add(link);
				}
			}
			Collection<RowToRowLink> partition = partitionPerBrowser.get(row.nonEmptyRowId);
			if (partition == null) {
				partition = new ArrayList<RowToRowLink>();
				partitionPerBrowser.put(row.nonEmptyRowId, partition);
			}
			return partition;
		}
		
		Collection<RowToRowLink> getParentPartition(RowBrowser browser, Row row) {
			Map<String, Collection<RowToRowLink>> partitionPerBrowser = parentRowsPartitions.get(browser);
			if (partitionPerBrowser == null) {
				partitionPerBrowser = new HashMap<String, Collection<RowToRowLink>>();
				parentRowsPartitions.put(browser, partitionPerBrowser);
				for (RowToRowLink link: browser.rowToRowLinks) {
					Collection<RowToRowLink> partition = partitionPerBrowser.get(link.parentRow.nonEmptyRowId);
					if (partition == null) {
						partition = new ArrayList<RowToRowLink>();
						partitionPerBrowser.put(link.parentRow.nonEmptyRowId, partition);
					}
					partition.add(link);
				}
			}
			Collection<RowToRowLink> partition = partitionPerBrowser.get(row.nonEmptyRowId);
			if (partition == null) {
				partition = new ArrayList<RowToRowLink>();
				partitionPerBrowser.put(row.nonEmptyRowId, partition);
			}
			return partition;
		}

	}
	
    private void removeMetaDataSource(Session session) {
    	session.setSessionProperty(DataBrowser.class, "removeMetaDataSource", Boolean.TRUE);
    }

	private int iFrameBufferGeneration;
	
	private void invalidateIFramesBuffers() {
		++iFrameBufferGeneration;
	}
	
	private void onBecomeSelected(final RowBrowser tableBrowser, final JInternalFrame jInternalFrame) {
		Timer updateMenuPendingTimer = new Timer(120, null);
		updateMenuPendingTimer.addActionListener(e -> {
			updateMenu();
			iFrameStateChangeRenderer.onIFrameSelected(jInternalFrame, 0.6);
			updateMenuPendingTimer.stop();
		});
		updateMenuPendingTimer.setRepeats(false);
		updateMenuPendingTimer.start();
		
		if (!noArrangeLayoutOnNewTableBrowserWithAnchor) {
			if (anchorManager.isApplicable(tableBrowser) || rowsClosure.hAlignedPathOnSelection) {
				checkHAlignedPath();
			}
			if (rowsClosure.hAlignedPath.isEmpty() || rowsClosure.hAlignedPathOnSelection) {
					rowsClosure.hAlignedPath.clear();
					
					if (!desktopAnimation.isActive()) {
						RowBrowser ct = tableBrowser;
						while (ct != null) {
							int y = ct.internalFrame.getY();
							Optional<RowBrowser> optCB = getChildBrowsers(ct, false).stream().filter(cb -> cb.internalFrame.getY() == y || cb.isHidden()).findAny();
							if (optCB.isPresent()) {
								ct = optCB.get();
								if (!ct.isHidden()) {
									rowsClosure.hAlignedPath.add(ct.browserContentPane);	
								}
							} else {
								break;
							}
						}
					}
					if (tableBrowser.parent != null) {
						rowsClosure.hAlignedPath.add(tableBrowser.parent.browserContentPane);
						RowBrowser parent = tableBrowser.parent;
						while (parent != null && parent.parent != null && parent.isHidden()) {
							parent = parent.parent;
							rowsClosure.hAlignedPath.add(parent.browserContentPane);
						}
						for (RowBrowser ancBr = parent; ancBr != null; ancBr = ancBr.parent) {
//							if (ancBr.isHidden() || ancBr.internalFrame.getY() == tableBrowser.parent.internalFrame.getY()) {
								rowsClosure.hAlignedPath.add(ancBr.browserContentPane);
//							} else {
//								break;
//							}
						}
					}
					
					rowsClosure.hAlignedPath.add(tableBrowser.browserContentPane);
					rowsClosure.hAlignedPathOnSelection = true;
			}
		}
	}

	private List<net.sf.jailer.ui.databrowser.DBConditionEditor.RSyntaxTextArea> editorPanesCache = new LinkedList<DBConditionEditor.RSyntaxTextArea>();

	private boolean becomeSelectedPending = false;
	
	/**
	 * Maximum number of concurrent DB connections.
	 */
	private static final int MAX_CONCURRENT_CONNECTIONS = 6;
	static {
		// initialize listeners for #runnableQueue
		for (int i = 0; i < MAX_CONCURRENT_CONNECTIONS; ++i) {
			Thread t = new Thread(new Runnable() {
				@Override
				public void run() {
					for (;;) {
						RunnableWithPriority take = null;
						try {
							take = runnableQueue.take();
							take.run();
						} catch (InterruptedException e) {
							// ignore
						} catch (CancellationException e) {
							// ignore
						} catch (Throwable t) {
							t.printStackTrace();
						}
					}
				}
			}, "PQueue-Worker-" + (i + 1));
			t.setDaemon(true);
			t.start();
		}
	}
	
	// TODO display names for associations? (using unique fk-column list?)
	
}
