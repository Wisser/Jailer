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
import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyVetoException;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.DefaultDesktopManager;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JDesktopPane;
import javax.swing.JFrame;
import javax.swing.JInternalFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JViewport;
import javax.swing.SwingUtilities;
import javax.swing.event.InternalFrameEvent;
import javax.swing.event.InternalFrameListener;

import net.sf.jailer.CommandLineParser;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.ConditionEditor;
import net.sf.jailer.ui.DbConnectionDialog;
import net.sf.jailer.ui.QueryBuilderDialog;
import net.sf.jailer.ui.QueryBuilderDialog.Relationship;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.BrowserContentPane.LoadJob;
import net.sf.jailer.ui.databrowser.TreeLayoutOptimizer.Node;
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
	public static final int BROWSERTABLE_DEFAULT_WIDTH = 476;
	private final int BROWSERTABLE_DEFAULT_MIN = 0, BROWSERTABLE_DEFAULT_HEIGHT = 460, BROWSERTABLE_DEFAULT_DISTANCE = 64;

	/**
	 * <code>true</code> while the desktop is visible.
	 */
	private boolean running;

	/**
	 * <code>false</code> if links must not be rendered (if a frame is maximized).
	 */
	private boolean renderLinks;

	/**
	 * Schema mapping.
	 */
	public final Map<String, String> schemaMapping = new TreeMap<String, String>();
	
	/**
	 * DB session.
	 */
	public Session session;
	DbConnectionDialog dbConnectionDialog;
	
	private Set<Pair<BrowserContentPane, Row>> currentClosure = new HashSet<Pair<BrowserContentPane,Row>>();
	private Set<Pair<BrowserContentPane, String>> currentClosureRowIDs = new HashSet<Pair<BrowserContentPane,String>>();
	
	private final QueryBuilderDialog queryBuilderDialog;
	private final QueryBuilderPathSelector queryBuilderPathSelector;
	
	/**
	 * Constructor.
	 * 
	 * @param datamodel the {@link DataModel}
	 * @param jailerIcon icon for the frames
	 * @param session DB-session
	 */
	public Desktop(Reference<DataModel> datamodel, Icon jailerIcon, Session session, DataBrowser parentFrame, DbConnectionDialog dbConnectionDialog) {
		this.parentFrame = parentFrame;
		this.datamodel = datamodel;
		this.jailerIcon = jailerIcon;
		this.queryBuilderDialog = new QueryBuilderDialog(parentFrame);
		this.queryBuilderPathSelector = new QueryBuilderPathSelector(parentFrame, true);
		this.dbConnectionDialog = dbConnectionDialog;

		this.queryBuilderDialog.sqlEditButton.setVisible(true);
		this.queryBuilderDialog.sqlEditButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				addTableBrowser(null, 0, null, null, queryBuilderDialog.getSQL(), null, null, true);
				queryBuilderDialog.setVisible(false);
			}
		});
		
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
							Thread.sleep(300);
							SwingUtilities.invokeAndWait(new Runnable() {
								@Override
								public void run() {
									boolean cl = calculateLinks();
									if (cl) {
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

		public void convertToRoot() {
			rowIndex = -1;
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
			} else {
				internalFrame.setVisible(true);
				Rectangle r = layout(rowIndex < 0, parent, association, browserContentPane, new ArrayList<RowBrowser>(), 0, -1);
				internalFrame.setBounds(r);
				scrollRectToVisible(internalFrame.getBounds());
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
	 * @param table to read rows from. Open SQL browser if table is <code>null</code>.
	 * @param association to navigate, or <code>null</code>
	 * @param condition 
	 * @param selectDistinct 
	 * @param limit 
	 * @return new row-browser
	 */
	public synchronized RowBrowser addTableBrowser(final RowBrowser parent, int parentRowIndex, final Table table, final Association association, String condition, Integer limit, Boolean selectDistinct, boolean reload) {
		Set<String> titles = new HashSet<String>();
		for (RowBrowser rb: tableBrowsers) {
			titles.add(rb.internalFrame.getTitle());
		}
		demaximize();
		
		String title = null;
		if (table != null) {
			title = datamodel.get().getDisplayName(table);
			if (titles.contains(title)) {
				for (int i = 2; ; ++i) {
					String titelPlusI = title + " (" + i + ")";
					if (!titles.contains(titelPlusI)) {
						title = titelPlusI;
						break;
					}
				}
			}
		}
		
		final RowBrowser tableBrowser = new RowBrowser();
		final JInternalFrame jInternalFrame = new JInternalFrame(table == null? "SQL" : title);
		jInternalFrame.setClosable(true);
		jInternalFrame.setIconifiable(true);
		jInternalFrame.setMaximizable(true);
		jInternalFrame.setVisible(true);
		jInternalFrame.addMouseWheelListener(new java.awt.event.MouseWheelListener() {
            public void mouseWheelMoved(java.awt.event.MouseWheelEvent evt) {
                onMouseWheelMoved(evt);
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
				manager.resizeDesktop();
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
					updateMenu();
				}
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

		BrowserContentPane browserContentPane = new BrowserContentPane(datamodel.get(), table, condition, session, parent == null || parentRowIndex < 0? null : parent.browserContentPane.rows.get(parentRowIndex), parent == null || parentRowIndex >= 0? null : parent.browserContentPane.rows, association, parentFrame, currentClosure, currentClosureRowIDs, limit, selectDistinct, reload) {
			
			@Override
			protected QueryBuilderDialog getQueryBuilderDialog() {
				return queryBuilderDialog;
			}

			@Override
			protected QueryBuilderPathSelector getQueryBuilderPathSelector() {
				return queryBuilderPathSelector;
			}

			@Override
			protected void navigateTo(Association association, int rowIndex, Row row) {
				addTableBrowser(tableBrowser, rowIndex, association.destination, association, "", null, null, true);
			}

			@Override
			protected void onContentChange(List<Row> rows, boolean reloadChildren) {
				updateChildren(tableBrowser, rows);
				for (RowBrowser rb: tableBrowsers) {
					if (rb.parent == tableBrowser) {
						updateChildren(rb, rb.browserContentPane.rows);
						if (reloadChildren && rb.browserContentPane.parentRow == null) {
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
				Set<Pair<BrowserContentPane, Row>> rows = new HashSet<Pair<BrowserContentPane,Row>>();
				findClosure(row, rows, false);
				currentClosure.addAll(rows);
				rows = new HashSet<Pair<BrowserContentPane,Row>>();
				findClosure(row, rows, true);
				currentClosure.addAll(rows);
			}
				
			@Override
			protected void findClosure(Row row, Set<Pair<BrowserContentPane, Row>> closure, boolean forward) {
				Pair<BrowserContentPane, Row> thisRow = new Pair<BrowserContentPane, Row>(this, row);
				if (!closure.contains(thisRow)) {
					closure.add(thisRow);
					if (forward) {
						for (RowBrowser child: tableBrowsers) {
							if (child.parent == tableBrowser) {
								if (child.browserContentPane.parentRow != null) {
									if (row.rowId.equals(child.browserContentPane.parentRow.rowId)) {
										for (Row r: child.browserContentPane.rows) {
											child.browserContentPane.findClosure(r, closure, forward);
										}
									}
								}
								for (RowToRowLink rowToRowLink: child.rowToRowLinks) {
									if (row.rowId.equals(rowToRowLink.parentRow.rowId)) {
										child.browserContentPane.findClosure(rowToRowLink.childRow, closure, forward);
									}
								}
							}
						}
					} else {
						if (tableBrowser.parent != null) {
							if (tableBrowser.browserContentPane.parentRow != null) {
								tableBrowser.parent.browserContentPane.findClosure(tableBrowser.browserContentPane.parentRow, closure, forward);
							}
							for (RowToRowLink rowToRowLink: tableBrowser.rowToRowLinks) {
								if (row.rowId.equals(rowToRowLink.childRow.rowId)) {
									tableBrowser.parent.browserContentPane.findClosure(rowToRowLink.parentRow, closure, forward);
								}
							}
						}
					}
				}
			}

			private void createAnchorSQL(RowBrowser rb, StringBuilder rowIds, boolean indent) {
				boolean f = true;
				for (Row row: rb.browserContentPane.rows) {
					if (!f) {
						rowIds.append(indent? " or\n       " : " or\n");
					}
					f = false;
					rowIds.append(SqlUtil.replaceAliases(row.rowId, "A", "A"));
				}
				rowIds.append("");
			}

			@Override
			protected QueryBuilderDialog.Relationship createQBRelations(boolean withParents) {
				QueryBuilderDialog.Relationship root = new QueryBuilderDialog.Relationship();
				root.whereClause = ConditionEditor.toMultiLine(getAndConditionText().trim()).replaceAll("(\r|\n)+", " ");
				if (root.whereClause.length() == 0) {
					root.whereClause = null;
				}
				StringBuilder rowIds = new StringBuilder("");
				createAnchorSQL(tableBrowser, rowIds, withParents);
				root.anchorWhereClause = rowIds.length() == 0? null : rowIds.toString();
				
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
					child.whereClause = ConditionEditor.toMultiLine(rb.browserContentPane.getAndConditionText().trim()).replaceAll("(\r|\n)+", " ");
					if (child.whereClause.length() == 0) {
						child.whereClause = null;
					}
					child.association = a.reversalAssociation;
					r.anchor = child.association;
					a = rb.association;
					rowIds = new StringBuilder("");
					createAnchorSQL(rb, rowIds, true);
					child.anchorWhereClause = rowIds.length() == 0? null : rowIds.toString();

					r.originalParent = child;
					
					if (childRB.rowIndex >= 0 && !(childRB.rowIndex == 0 && childRB.parent != null && childRB.parent.browserContentPane != null && childRB.parent.browserContentPane.rows != null && childRB.parent.browserContentPane.rows.size() == 1)) {
						String w = childRB.browserContentPane.parentRow.rowId;
						child.whereClause = null;
						r.whereClause = w; // (r.whereClause == null || r.whereClause.length() == 0)? w : "(" + w + ") and (" + r.whereClause + ")";
						break;
					}
					
					r = child;
					childRB = rb;
				}
				return root;
			}

			@Override
			protected List<Relationship> createQBChildrenRelations(RowBrowser tabu, boolean all) {
				List<QueryBuilderDialog.Relationship> result = new ArrayList<QueryBuilderDialog.Relationship>();
				for (RowBrowser rb: tableBrowsers) {
					if (rb.parent == tableBrowser && rb != tabu) {
						boolean singleRowParent = rb.rowIndex >= 0 && !(rb.rowIndex == 0 && rb.parent != null && rb.parent.browserContentPane != null && rb.parent.browserContentPane.rows != null && rb.parent.browserContentPane.rows.size() == 1);
						if (true) { // all || !singleRowParent) {
							QueryBuilderDialog.Relationship child = new QueryBuilderDialog.Relationship();
							child.whereClause = ConditionEditor.toMultiLine(rb.browserContentPane.getAndConditionText().trim()).replaceAll("(\r|\n)+", " ");
							child.joinOperator = QueryBuilderDialog.JoinOperator.LeftJoin;
							if (child.whereClause.length() == 0) {
								child.whereClause = null;
							}
							if (singleRowParent) {
								String andIsParent = rb.browserContentPane.parentRow.rowId;
								if (child.whereClause == null) {
									child.whereClause = andIsParent;
								} else {
									child.whereClause = "(" + child.whereClause + ") and (" + andIsParent + ")";
								}
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
			protected void adjustClosure(BrowserContentPane tabu) {
				Desktop.this.adjustClosure(tabu);
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
				Desktop.this.restoreSession(tableBrowser);
			}

			@Override
			protected LinkedBlockingQueue<LoadJob> getRunnableQueue() {
				return runnableQueue;
			}

			@Override
			protected void collectPositions(Map<String, Map<String, double[]>> positions) {
				Desktop.this.collectPositions(tableBrowser, positions);
			}

		};
		
		Rectangle r = layout(parentRowIndex < 0, parent, association, browserContentPane, new ArrayList<RowBrowser>(), 0, -1);
		browserContentPane.rowsTableScrollPane.addMouseWheelListener(new java.awt.event.MouseWheelListener() {
            public void mouseWheelMoved(java.awt.event.MouseWheelEvent evt) {
                onMouseWheelMoved(evt);
            }
        });
		
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

		initIFrame(jInternalFrame, browserContentPane);
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
				close(tableBrowser, true);
			}
			@Override
			public void internalFrameActivated(InternalFrameEvent e) {
			}
		});

		checkDesktopSize();
		this.scrollToCenter(jInternalFrame);
		try {
			jInternalFrame.setSelected(true);
		} catch (PropertyVetoException e1) {
			// ignore
		}
		if (browserContentPane.sqlBrowserContentPane != null) {
			browserContentPane.sqlBrowserContentPane.sqlEditorPane.grabFocus();
		} else {
			browserContentPane.andCondition.grabFocus();
		}
		updateMenu();
		return tableBrowser;
	}

	/**
	 * Demaximizes all internal frames.
	 */
	private void demaximize() {
		for (RowBrowser rb: tableBrowsers) {
			try {
				rb.internalFrame.setMaximum(false);
			} catch (PropertyVetoException e) {
				// ignore
			}
		}
	}

	private void initIFrame(final JInternalFrame jInternalFrame,
			final BrowserContentPane browserContentPane) {
		final JPanel thumbnail = new JPanel();
		final JPanel thumbnailInner = new JPanel();
		thumbnail.setLayout(new GridBagLayout());
		GridBagConstraints gridBagConstraints = new GridBagConstraints();
		gridBagConstraints.gridx = 1;
		gridBagConstraints.gridy = 1;
		gridBagConstraints.gridwidth = 1;
		gridBagConstraints.gridheight = 1;
		gridBagConstraints.weightx = 1;
		gridBagConstraints.weighty = 1;
		gridBagConstraints.fill = GridBagConstraints.BOTH;
		gridBagConstraints.insets = new Insets(8, 8, 8, 8);
		thumbnail.add(thumbnailInner, gridBagConstraints);

		thumbnailInner.setLayout(new FlowLayout(FlowLayout.CENTER, 0, 0));
		String title = jInternalFrame.getTitle();
		String suffix = null;
		Pattern tPat = Pattern.compile("^(.*)(\\([0-9]+\\))$");
		Matcher matcher = tPat.matcher(title);
		if (matcher.matches()) {
			title = matcher.group(1);
			suffix = matcher.group(2);
		}
		for (int i = 0; i < title.length(); ++i) {
			thumbnailInner.add(new JLabel(title.substring(i, i + 1)));
		}
		if (suffix != null) {
			thumbnailInner.add(new JLabel(suffix));
		}
		jInternalFrame.getContentPane().setLayout(new CardLayout());
 		
		jInternalFrame.getContentPane().add(browserContentPane, "C");
		jInternalFrame.getContentPane().add(thumbnail, "T");
		
		thumbnail.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				JPopupMenu popup = browserContentPane.createPopupMenu(null, -1, 0, 0, false);
				JPopupMenu popup2 = browserContentPane.createSqlPopupMenu(null, -1, 0, 0, true);
				popup.add(new JSeparator());
				for (Component c: popup2.getComponents()) {
					popup.add(c);
				}
				popup.show(e.getComponent(), e.getX(), e.getY());
			}
		});
		
		initIFrameContent(jInternalFrame, browserContentPane, thumbnail);
		jInternalFrame.addComponentListener(new ComponentListener() {
			@Override
			public void componentHidden(ComponentEvent e) {
			}
			@Override
			public void componentMoved(ComponentEvent e) {
			}
			@Override
			public void componentResized(ComponentEvent e) {
				initIFrameContent(jInternalFrame, browserContentPane, thumbnail);
			}
			@Override
			public void componentShown(ComponentEvent e) {
			}
		});
	}

	private void initIFrameContent(final JInternalFrame jInternalFrame,
			final BrowserContentPane browserContentPane, final JPanel thumbnail) {
		if (jInternalFrame.getWidth() < 150 || jInternalFrame.getHeight() < 150) {
			((CardLayout) jInternalFrame.getContentPane().getLayout()).show(jInternalFrame.getContentPane(), "T");
		} else {
			((CardLayout) jInternalFrame.getContentPane().getLayout()).show(jInternalFrame.getContentPane(), "C");
		}
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

	private Rectangle layout(final boolean fullSize, RowBrowser parent, Association association, BrowserContentPane browserContentPane, Collection<RowBrowser> ignore, int maxH, int xPosition) {
		int x = (int) (BROWSERTABLE_DEFAULT_MIN * layoutMode.factor);
		int y = (int) (BROWSERTABLE_DEFAULT_MIN * layoutMode.factor);
		
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
			x = (int) (xPosition * (BROWSERTABLE_DEFAULT_WIDTH + BROWSERTABLE_DEFAULT_DISTANCE) * layoutMode.factor);
		}
		// int h = fullSize || association == null || (association.getCardinality() != Cardinality.MANY_TO_ONE && association.getCardinality() != Cardinality.ONE_TO_ONE)? HEIGHT : browserContentPane.getMinimumSize().height + MIN_HEIGHT; 
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
			y += 8 * layoutMode.factor;
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
			Map<Row, Integer> rowIndex = new IdentityHashMap<Row, Integer>();
			Map<String, Integer> rowIDIndex = new HashMap<String, Integer>();
			Map<Row, Integer> parentRowIndex = new IdentityHashMap<Row, Integer>();
			Map<String, Integer> parentRowIDIndex = new HashMap<String, Integer>();
			for (int i = 0; i < rows.size(); ++i) {
				Integer iI = i;
				Row r = rows.get(i);
				rowIndex.put(r, iI);
				rowIDIndex.put(r.rowId, iI);
			}
			List<Row> parentRows = tableBrowser.parent.browserContentPane.rows;
			for (int i = 0; i < parentRows.size(); ++i) {
				Integer iI = i;
				Row r = parentRows.get(i);
				parentRowIndex.put(r, iI);
				parentRowIDIndex.put(r.rowId, iI);
			}
			for (RowToRowLink rowToRowLink: tableBrowser.rowToRowLinks) {
				rowToRowLink.childRowIndex = -1;
				Integer i = rowIndex.get(rowToRowLink.childRow);
				if (i != null) {
					rowToRowLink.childRowIndex = i;
				}
//				for (int i = 0; i < rows.size(); ++i) {
//					if (rowToRowLink.childRow == rows.get(i)) {
//						rowToRowLink.childRowIndex = i;
//						break;
//					}
//				}
				if (rowToRowLink.childRowIndex < 0) {
					i = rowIDIndex.get(rowToRowLink.childRow.rowId);
					if (i != null) {
						rowToRowLink.childRowIndex = i;
					}
//					for (int i = 0; i < rows.size(); ++i) {
//						if (rowToRowLink.childRow.rowId.equals(rows.get(i).rowId)) {
//							rowToRowLink.childRowIndex = i;
//							break;
//						}
//					}
				}
				rowToRowLink.parentRowIndex = -1;
				i = parentRowIndex.get(rowToRowLink.parentRow);
				if (i != null) {
					rowToRowLink.parentRowIndex = i;
				}
//				for (int i = 0; i < parentRows.size(); ++i) {
//					if (rowToRowLink.parentRow == parentRows.get(i)) {
//						rowToRowLink.parentRowIndex = i;
//						break;
//					}
//				}
				
				if (rowToRowLink.parentRowIndex < 0) {
					i = parentRowIDIndex.get(rowToRowLink.parentRow.rowId);
					if (i != null) {
						rowToRowLink.parentRowIndex = i;
					}
//					for (int i = 0; i < parentRows.size(); ++i) {
//						if (rowToRowLink.parentRow.rowId.equals(parentRows.get(i).rowId)) {
//							rowToRowLink.parentRowIndex = i;
//							break;
//						}
//					}
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
				int BORDER = 6;
				int BOT_H = 32;
				int x1 = internalFrame.getX() + internalFrame.getWidth() / 2;
				int y1 = internalFrame.getY() + internalFrame.getHeight() / 2;
				
				RowBrowser visParent = tableBrowser.parent;
				while (visParent != null && visParent.isHidden()) {
					visParent = visParent.parent;
				}
				if (visParent == null) {
					visParent = tableBrowser.parent;
				}
				int midx = visParent.internalFrame.getX() + visParent.internalFrame.getWidth() / 2;
				
				Rectangle cellRect = new Rectangle();
				boolean ignoreScrolling = false;
				int i = 0;
				if (tableBrowser.rowIndex >= 0) {
					i = tableBrowser.parent.browserContentPane.rowsTable.getRowSorter().convertRowIndexToView(tableBrowser.rowIndex);
					cellRect = tableBrowser.parent.browserContentPane.rowsTable.getCellRect(i, 0, true);
					if (tableBrowser.parent.browserContentPane.rows != null && tableBrowser.parent.browserContentPane.rows.size() == 1) {
						cellRect.setBounds(cellRect.x, 0, cellRect.width, Math.min(cellRect.height, 20));
						ignoreScrolling = true;
					}
				}

				int x2 = visParent.internalFrame.getX();
				int y = cellRect.y;
				y = cellRect.height * i;
				int y2 = visParent.internalFrame.getY() + y + Math.min(cellRect.height / 2, 100);
//				if (midx < x1) {
					x2 += visParent.internalFrame.getWidth() - BORDER;
//				} else {
//					x2 += BORDER;
//				}
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
				
				if (tableBrowser.rowIndex < 0) {
					y2 = visParent.internalFrame.getY() + visParent.internalFrame.getHeight() / 2;
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
							visParent = tableBrowser.parent;
							while (visParent != null && visParent.isHidden()) {
								visParent = visParent.parent;
							}
							if (visParent == null) {
								visParent = tableBrowser.parent;
							}
							int dll = Math.abs(visParent.internalFrame.getX() - internalFrame.getX());
							int dlr = Math.abs(visParent.internalFrame.getX() - (internalFrame.getX() + internalFrame.getWidth()));
							int drl = Math.abs((visParent.internalFrame.getX() + visParent.internalFrame.getWidth()) - internalFrame.getX());
							int drr = Math.abs((visParent.internalFrame.getX() + visParent.internalFrame.getWidth()) - (internalFrame.getX() + internalFrame.getWidth()));
							
							boolean r1, r2;
							int dmin = Math.min(dll, Math.min(dlr, Math.min(drl, drr)));
							r2 = dmin == drl || dmin == drr;
							r1 = dmin == dlr || dmin == drr;
							ignoreScrolling = false;
							if (rowToRowLink.childRowIndex >= 0) {
								i = tableBrowser.browserContentPane.rowsTable.getRowSorter().convertRowIndexToView(rowToRowLink.childRowIndex);
								cellRect = tableBrowser.browserContentPane.rowsTable.getCellRect(i, 0, true);
								if (tableBrowser.browserContentPane.rows != null && tableBrowser.browserContentPane.rows.size() == 1) {
									cellRect.setBounds(cellRect.x, 0, cellRect.width, Math.min(cellRect.height, 20));
									ignoreScrolling = true;
								}
							}
		
							x1 = internalFrame.getX();
							y = cellRect.height * i;
							y1 = internalFrame.getY() + y + cellRect.height / 2;
//							if (r1) {
//								x1 += internalFrame.getWidth()- BORDER;
//							} else {
								x1 += BORDER;
//							}
							p = tableBrowser.browserContentPane.rowsTable;
							if (ignoreScrolling) {
								p = p.getParent();
							}
							while (p != internalFrame) {
								y1 += p.getY();
								p = p.getParent();
							}
							min = internalFrame.getY() + cellRect.height;
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
							if (rowToRowLink.parentRowIndex >= 0) {
								i = tableBrowser.parent.browserContentPane.rowsTable.getRowSorter().convertRowIndexToView(rowToRowLink.parentRowIndex);
								cellRect = tableBrowser.parent.browserContentPane.rowsTable.getCellRect(i, 0, true);
								if (tableBrowser.parent.browserContentPane.rows != null && tableBrowser.parent.browserContentPane.rows.size() == 1) {
									cellRect.setBounds(cellRect.x, 0, cellRect.width, Math.min(cellRect.height, 20));
									ignoreScrolling = true;
								}
							}
		
							x2 = visParent.internalFrame.getX();
							y = cellRect.height * i;
							y2 = visParent.internalFrame.getY() + y + cellRect.height / 2;
//							if (r2) {
								x2 += visParent.internalFrame.getWidth() - BORDER;
//							} else {
//								x2 += BORDER;
//							}
							p = visParent.browserContentPane.rowsTable;
							if (ignoreScrolling) {
								p = p.getParent();
							}
							while (p != visParent.internalFrame) {
								y2 += p.getY();
								p = p.getParent();
							}
							min = visParent.internalFrame.getY() + cellRect.height;
							if (y2 < min) {
								y2 = min;
							}
							max = visParent.internalFrame.getY() + visParent.internalFrame.getHeight() - BOT_H;
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
		if (changed) {
			rbSourceToLinks = null;
		}
		return changed;
	}

	private long lastPTS = 0;
	
	private static class Link {
		public boolean visible = true;
		public final RowBrowser from, to;
		public final String sourceRowID, destRowID;
		public int x1, y1, x2, y2;
		public final Color color;
		public final boolean dotted, intersect;
		
		public Link(RowBrowser from, RowBrowser to,
				String sourceRowID, String destRowID,
				int x1, int y1, int x2, int y2,
				Color color,
				boolean dotted, boolean intersect) {
			this.from = from;
			this.to = to;
			this.sourceRowID = sourceRowID;
			this.destRowID = destRowID;
			this.x1 = x1;
			this.y1 = y1;
			this.x2 = x2;
			this.y2 = y2;
			this.color = color;
			this.dotted = dotted;
			this.intersect = intersect;
		}
	};
	
	private Map<RowBrowser, Map<String, List<Link>>> rbSourceToLinks = null;
	
	/**
	 * Paints all link-renders.
	 */
	@Override
	public synchronized void paint(Graphics graphics) {
		super.paint(graphics);
		if (graphics instanceof Graphics2D) {
			Graphics2D g2d = (Graphics2D) graphics;
			if (renderLinks) {
				if (rbSourceToLinks == null) {
					rbSourceToLinks = new HashMap<RowBrowser, Map<String, List<Link>>>();
					final String ALL = "-";
					
					for (RowBrowser tableBrowser : tableBrowsers) {
						Map<String, List<Link>> links = new TreeMap<String, List<Link>>();
						rbSourceToLinks.put(tableBrowser, links);
						if (!tableBrowser.internalFrame.isIcon() && (tableBrowser.parent == null || !tableBrowser.parent.internalFrame.isIcon())) {
							Color color = tableBrowser.color;
							if (tableBrowser.parent != null && (tableBrowser.rowIndex >= 0 || tableBrowser.rowToRowLinks.isEmpty())) {
								String sourceRowID = ALL;
								String destRowID = ALL;
								if (tableBrowser.browserContentPane.parentRow != null) {
									destRowID = tableBrowser.browserContentPane.parentRow.rowId;
								}
								Link link = new Link(tableBrowser, tableBrowser.parent, sourceRowID, destRowID, tableBrowser.x1, tableBrowser.y1, tableBrowser.x2, tableBrowser.y2, color, tableBrowser.parent == null || tableBrowser.rowIndex < 0, true);
								List<Link> l = links.get(sourceRowID);
								if (l == null) {
									l = new ArrayList<Link>();
									links.put(sourceRowID, l);
								}
								l.add(link);
							}
							for (RowToRowLink rowToRowLink: tableBrowser.rowToRowLinks) {
								if (rowToRowLink.x1 >= 0) {
									String sourceRowID = rowToRowLink.childRow.rowId;
									String destRowID = rowToRowLink.parentRow.rowId;
									
									if (!tableBrowser.isHidden() && (tableBrowser.parent == null || !tableBrowser.parent.isHidden())) {
										// optimization
										sourceRowID = "";
									}
									
									Link link = new Link(tableBrowser, tableBrowser.parent, sourceRowID, destRowID, rowToRowLink.x1, rowToRowLink.y1, rowToRowLink.x2, rowToRowLink.y2, color, false, false);
									List<Link> l = links.get(sourceRowID);
									if (l == null) {
										l = new ArrayList<Link>();
										links.put(sourceRowID, l);
									}
									l.add(link);
								}
							}
						}
					}
					
					// join links of hidden browser
					List<Link> toJoinList = new ArrayList<Link>();
					for (RowBrowser tableBrowser : tableBrowsers) {
						if (tableBrowser.parent != null && tableBrowser.parent.isHidden()) {
							List<Link> newLinks = new ArrayList<Link>();
							Map<String, List<Link>> links = rbSourceToLinks.get(tableBrowser);
							for (Map.Entry<String, List<Link>> e: links.entrySet()) {
								for (Link link: e.getValue()) {
									link.visible = false;
									
									List<Link> ll;
									if (link.destRowID == ALL) {
										ll = new ArrayList<Desktop.Link>();
										for (List<Link> values: rbSourceToLinks.get(link.to).values()) {
											for (Link l: values) {
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
	
									for (Link toJoin: toJoinList) {
										toJoin.visible = false;
										Color color = /* link.color.equals(toJoin.color)? link.color : */ Color.black;
										boolean intersect = link.intersect;
										boolean dotted = link.dotted || toJoin.dotted;
										newLinks.add(new Link(link.from, toJoin.to, link.sourceRowID, toJoin.destRowID, link.x1, link.y1, toJoin.x2, toJoin.y2, color, dotted, intersect));
									}
								}
							}
							for (Link link: newLinks) {
								links.get(link.sourceRowID).add(link);
							}
						}
					}
				}
				
				for (boolean pbg: new Boolean[] { true, false}) {
					Set<Long> linesHash = new HashSet<Long>(200000);
					for (RowBrowser tableBrowser : rbSourceToLinks.keySet()) {
						if (!tableBrowser.isHidden()) {
							Map<String, List<Link>> links = rbSourceToLinks.get(tableBrowser);
							for (Map.Entry<String, List<Link>> e: links.entrySet()) {
								for (Link link: e.getValue()) {
									if (link.visible && !link.from.isHidden() && !link.to.isHidden()) {
										Color color = pbg? Color.white : link.color;
										Point2D start = new Point2D.Double(link.x2, link.y2);
										Point2D end = new Point2D.Double(link.x1, link.y1);
										paintLink(start, end, color, g2d, tableBrowser, pbg, link.intersect, linesHash, link.dotted);
									}
								}
							}
						}
						
//						if (!tableBrowser.internalFrame.isIcon() && (tableBrowser.parent == null || !tableBrowser.parent.internalFrame.isIcon())) {
//							Color color = pbg? Color.white : tableBrowser.color;
//							if (tableBrowser.parent != null && (tableBrowser.rowIndex >= 0 || tableBrowser.rowToRowLinks.isEmpty())) {
//								Point2D start = new Point2D.Double(tableBrowser.x2, tableBrowser.y2);
//								Point2D end = new Point2D.Double(tableBrowser.x1, tableBrowser.y1);
//								paintLink(start, end, color, g2d, tableBrowser, pbg, true, linesHash, tableBrowser.parent == null || tableBrowser.rowIndex < 0);
//							}
//							for (RowToRowLink rowToRowLink: tableBrowser.rowToRowLinks) {
//								if (rowToRowLink.x1 >= 0) {
//									paintLink(new Point2D.Double(rowToRowLink.x2, rowToRowLink.y2), new Point2D.Double(rowToRowLink.x1, rowToRowLink.y1), color, g2d, tableBrowser, pbg, false, linesHash, false);
//								}
//							}
//						}
					}
				}
			}
		}
	}
	
	private void paintLink(Point2D start, Point2D end, Color color, Graphics2D g2d, RowBrowser tableBrowser, boolean pbg, boolean intersect, Set<Long> lineHashes, boolean dotted) {
		g2d.setColor(color);
		g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
		BasicStroke stroke = new BasicStroke(!intersect? (pbg? 3 : 1) : (pbg? 5 : 3));
		g2d.setStroke(dotted? new BasicStroke(stroke.getLineWidth(), stroke.getEndCap(), stroke.getLineJoin(), stroke.getMiterLimit(), 
				new float[] { 2f, 6f }, 1.0f) : stroke);
		
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

		long lineHash = ((long) start.hashCode()) + (((long) Integer.MAX_VALUE) + 1) * ((long) end.hashCode());
		if (lineHashes.contains(lineHash)) {
			return;
		}
		lineHashes.add(lineHash);
		
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
					if (allFrames[i].isVisible()) {
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
		desktops.remove(this);
		for (RowBrowser rb: tableBrowsers) {
			rb.browserContentPane.cancelLoadJob();
		}
		if (session != null) {
			new Thread(new Runnable() {
				@Override
				public void run() {
					try {
						synchronized (session) {
							session.shutDown();
						}
					} catch (SQLException e) {
						// exception already has been logged
					}
				}
			}).start();
		}
	}

	private final DataBrowser parentFrame;
	
	public static enum LayoutMode {
		THUMBNAIL(0.22),
		TINY(0.569),
		SMALL(0.75),
		MEDIUM(1.0),
		LARGE(1.4);
		
		public final double factor;
		
		private LayoutMode(double factor) {
			this.factor = factor;
		}
	}
	
	LayoutMode layoutMode = LayoutMode.MEDIUM;
	
	public void layoutBrowser() {
		JInternalFrame selectedFrame = getSelectedFrame();
		List<RowBrowser> all = new ArrayList<RowBrowser>(tableBrowsers);
		layout(all, 0);
		
		optimizeLayout();

		all.clear();
		int maxH = 0;
		for (RowBrowser rb: tableBrowsers) {
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
			this.scrollToCenter(selectedFrame);
		}
	}

	private void layout(List<RowBrowser> toLayout, int maxH) {
		List<RowBrowser> roots = new ArrayList<RowBrowser>();
		for (RowBrowser rb: toLayout) {
			if (rb.parent == null) {
				roots.add(rb);
			}
		}
		while (!roots.isEmpty()) {
			List<RowBrowser> nextColumn = new ArrayList<RowBrowser>();
			int i = 0;
			for (RowBrowser rb: roots) {
				try {
					rb.internalFrame.setMaximum(false);
				} catch (PropertyVetoException e) {
					// ignore
				}
				int xPosition = -1;
				if (maxH > 0) {
					xPosition = i;
				}
				rb.internalFrame.setBounds(layout(rb.rowIndex < 0, rb.parent, rb.association, rb.browserContentPane, toLayout, maxH, xPosition));
				rb.browserContentPane.adjustRowTableColumnsWidth();
				toLayout.remove(rb);
				for (RowBrowser rbc: toLayout) {
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
	 */
	private void optimizeLayout() {
		TreeLayoutOptimizer.Node<RowBrowser> root = new TreeLayoutOptimizer.Node<RowBrowser>(null);
		collectChildren(root);
		TreeLayoutOptimizer.optimizeTreeLayout(root);
		arrangeNodes(root);
	}

	private void collectChildren(Node<RowBrowser> root) {
		List<RowBrowser> children;
		if (root.getUserObject() == null) {
			children = getRootBrowsers(true);
		} else {
			children = getChildBrowsers(root.getUserObject(), true);
		}
		for (RowBrowser rb: children) {
			if (rb.browserContentPane.table instanceof BrowserContentPane.SqlStatementTable) {
				continue;
			}
			TreeLayoutOptimizer.Node<RowBrowser> childNode = new TreeLayoutOptimizer.Node<RowBrowser>(rb);
			root.addChild(childNode);
			collectChildren(childNode);
		}
	}

	private void arrangeNodes(Node<RowBrowser> root) {
		if (root.getUserObject() != null) {
			JInternalFrame iFrame = root.getUserObject().internalFrame;
			int x = (int) (BROWSERTABLE_DEFAULT_MIN * layoutMode.factor);
			int y = (int) (BROWSERTABLE_DEFAULT_MIN * layoutMode.factor);
			x += (root.getLevel() - 1) * (int) ((BROWSERTABLE_DEFAULT_WIDTH + BROWSERTABLE_DEFAULT_DISTANCE) * layoutMode.factor);
			y += (int) (root.getPosition() * (BROWSERTABLE_DEFAULT_HEIGHT + 8) * layoutMode.factor);
			int h = (int) (BROWSERTABLE_DEFAULT_HEIGHT * layoutMode.factor); 
			Rectangle r = new Rectangle(x, y, (int) (BROWSERTABLE_DEFAULT_WIDTH * layoutMode.factor), h);
			iFrame.setBounds(r);
		}
		for (Node<RowBrowser> child: root.getChildren()) {
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
		
		this.layoutMode = layoutMode;
		Map<Rectangle, double[]> newPrecBounds = new HashMap<Rectangle, double[]>();
		for (RowBrowser rb: new ArrayList<RowBrowser>(tableBrowsers)) {
			if (rb.internalFrame.isMaximum()) {
				try {
					rb.internalFrame.setMaximum(false);
				} catch (PropertyVetoException e) {
					// ignore
				}
    		}
			Rectangle bounds = rb.internalFrame.getBounds();
			Rectangle newBounds;
			double[] pBounds = precBounds.get(bounds);
			if (pBounds == null) {
				pBounds = new double[] { bounds.x * scale, bounds.y * scale, bounds.width * scale, bounds.height * scale };
			} else {
				pBounds = new double[] { pBounds[0] * scale, pBounds[1] * scale, pBounds[2] * scale, pBounds[3] * scale };
			}
			newBounds = new Rectangle((int) pBounds[0], (int) pBounds[1], (int) pBounds[2], (int) pBounds[3]);
			rb.internalFrame.setBounds(newBounds);
			rb.browserContentPane.adjustRowTableColumnsWidth();
			newPrecBounds.put(newBounds, pBounds);
		}
		precBounds = newPrecBounds;
		manager.resizeDesktop();
		
		Rectangle vr = new Rectangle(
				Math.max(0, (int) (fixed.x * scale - getVisibleRect().width / 2)),
				Math.max(0, (int) (fixed.y * scale - getVisibleRect().height / 2)),
				getVisibleRect().width,
				getVisibleRect().height);
		scrollRectToVisible(vr);
		updateMenu(layoutMode);
		adjustClosure(null);
	}

	void onMouseWheelMoved(java.awt.event.MouseWheelEvent evt) {
		if (evt.isConsumed() && !evt.isControlDown()) return; // TODO
		int d = 0;
        if (evt.getWheelRotation() < 0) {
        	d = -1;
        }
        if (evt.getWheelRotation() > 0) {
        	d = 1;
        }
        if (d != 0) {
        	for (RowBrowser rb: new ArrayList<RowBrowser>(tableBrowsers)) {
    			if (rb.internalFrame.isMaximum()) {
    				return;
    			}
    		}
        	d += layoutMode.ordinal();
        	if (d >= 0 && d < LayoutMode.values().length) {
        		Point fixed = SwingUtilities.convertPoint(evt.getComponent(), evt.getPoint().x, evt.getPoint().y, Desktop.this);
        		rescaleLayout(LayoutMode.values()[d], fixed);
        	}
        }
	}

	public void closeAll() {
		closeAll(new ArrayList<RowBrowser>(tableBrowsers));
	}
	
	public void closeAll(Collection<RowBrowser> toClose) {
		for (RowBrowser rb: toClose) {
			close(rb, false);
			getDesktopManager().closeFrame(rb.internalFrame);
		}
		updateMenu();
	}

	private void close(final RowBrowser tableBrowser, boolean convertChildrenToRoots) {
		List<RowBrowser> children = new ArrayList<RowBrowser>();
		for (RowBrowser tb: tableBrowsers) {
			if (tb.parent == tableBrowser) {
				tb.parent = null;
				children.add(tb);
			}
		}
		tableBrowsers.remove(tableBrowser);
		tableBrowser.browserContentPane.cancelLoadJob();
		if (convertChildrenToRoots) {
			for (RowBrowser child: children) {
				child.convertToRoot();
			}
		}
		for (RowBrowser rb: tableBrowsers) {
			updateChildren(rb, rb.browserContentPane.rows);
		}
		repaintDesktop();
		updateMenu();
	}
	
	/**
	 * Reloads the data model and replaces the tables in all browser windows.
	 */
	public void reloadDataModel(Map<String, String> schemamapping) throws Exception {
		reloadDataModel(schemamapping, true);
	}
	
	/**
	 * Reloads the data model and replaces the tables in all browser windows.
	 */
	public void reloadDataModel(Map<String, String> schemamapping, boolean forAll) throws Exception {
		DataModel newModel = new DataModel(schemamapping);
		
		for (RowBrowser rb: tableBrowsers) {
			if (rb.browserContentPane != null) {
				rb.browserContentPane.dataModel = newModel;
				if (rb.browserContentPane.table != null && datamodel.get() != null) {
					Table oldTable = rb.browserContentPane.table;
					Table newTable = null;
					if (oldTable.getOriginalName() != null) {
						for (Table t: newModel.getTables()) {
							if (oldTable.getOriginalName().equals(t.getOriginalName())) {
								newTable = t;
								break;
							}
						}
					}
					if (newTable == null && oldTable.getName() != null) {
						newTable = newModel.getTableByDisplayName(datamodel.get().getDisplayName(oldTable));
					}
					if (newTable != null) {
						rb.browserContentPane.table = newTable;
					}
				}
			}
			updateChildren(rb, rb.browserContentPane.rows);
		}
		
		datamodel.set(newModel);
		
		if (forAll) {
			for (Desktop desktop: desktops ) {
				if (desktop != this) {
					desktop.reloadDataModel(desktop.schemaMapping, false);
				}
			}
		}
	}

	/**
	 * Reloads the rows in all root-table-browsers.
	 */
	public void reloadRoots() throws Exception {
		for (RowBrowser rb: tableBrowsers) {
			if (rb.browserContentPane != null) {
				if (rb.parent == null) {
					rb.browserContentPane.reloadRows();
				}
			}
		}
	}

	private boolean loadSchemaMapping = true;
	
	public abstract void openSchemaAnalyzer();
	
	public void openSchemaMappingDialog(boolean silent) {
		try {
			Map<String, String> mapping = schemaMapping;
			if (loadSchemaMapping || silent) {
				mapping = SchemaMappingDialog.restore(dbConnectionDialog);
				loadSchemaMapping = false;
			}
			if (!silent) {
				SchemaMappingDialog schemaMappingDialog = new SchemaMappingDialog(parentFrame, datamodel.get(), dbConnectionDialog, session, mapping);
				mapping = schemaMappingDialog.getMapping();
			}
			if (mapping != null) {
				SchemaMappingDialog.store(mapping, dbConnectionDialog);
				schemaMapping.clear();
				schemaMapping.putAll(mapping);
				parentFrame.updateStatusBar();
				reloadDataModel(mapping);
				reloadRoots();
			}
		} catch (Exception e) {
			UIUtil.showException(this, "Error", e);
		}
	}

	/**
	 * Lets user chose a table browser and creates an extraction model for it.
	 */
	public void createExtractionModel() {
		Set<String> titles = new TreeSet<String>();
		Map<String, RowBrowser> rowBrowserByTitle = new HashMap<String, Desktop.RowBrowser>();
		for (RowBrowser rb: tableBrowsers) {
			if (rb.browserContentPane.table != null && !(rb.browserContentPane.table instanceof BrowserContentPane.SqlStatementTable)) {
				titles.add(rb.internalFrame.getTitle());
				rowBrowserByTitle.put(rb.internalFrame.getTitle(), rb);
			}
		}
		String s = (String) JOptionPane.showInputDialog(this.parentFrame,
				"Select subject table", "Subject",
				JOptionPane.QUESTION_MESSAGE, null, titles.toArray(),
				null);
		if (s != null) {
			rowBrowserByTitle.get(s).browserContentPane.openExtractionModelEditor();
		}
	}

	void updateMenu() {
		boolean hasTableBrowser = false;
		boolean hasIFrame = false;
		
		for (RowBrowser rb: tableBrowsers) {
			hasIFrame = true;
			if (!(rb.browserContentPane.table instanceof BrowserContentPane.SqlStatementTable)) {
				hasTableBrowser = true;
			}
		}
		updateMenu(hasTableBrowser, hasIFrame);
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
	public void storeSession() {
		String fnProp = null;
		int i = 1;
		Map<RowBrowser, Integer> browserNumber = new HashMap<Desktop.RowBrowser, Integer>();
		for (RowBrowser rb: tableBrowsers) {
			browserNumber.put(rb, i++);
			if (fnProp == null && rb.parent == null && rb.browserContentPane.table != null) {
				if (!(rb.browserContentPane.table instanceof BrowserContentPane.SqlStatementTable)) {
					fnProp = datamodel.get().getDisplayName(rb.browserContentPane.table).replace(' ', '-').replace('\"', '-').replace('\'', '-').replace('(', '-').replace(')', '-').toLowerCase() + ".dbl";
				}
			}
		}

		if (currentSessionFileName != null) {
			fnProp = currentSessionFileName;
		}
		
		File startDir = CommandLineParser.getInstance().newFile("layout");
		Component pFrame = SwingUtilities.getWindowAncestor(this);
		if (pFrame == null) {
			pFrame = this;
		}
		String sFile = UIUtil.choseFile(fnProp == null? null : new File(startDir, fnProp), startDir.getPath(), "Store Layout", ".dbl", pFrame, true, false);
		
		if (sFile != null) {
			try {
				FileWriter out = new FileWriter(new File(sFile));

				out.write("Layout; " + layoutMode + LF);
				
				for (RowBrowser rb: tableBrowsers) {
					if (rb.parent == null) {
						storeSession(rb, browserNumber, out);
					}
				}
				out.close();
				currentSessionFileName = sFile;
			} catch (Throwable e) {
				UIUtil.showException(this, "Error", e);
			}
		}
	}

	/**
	 * Recursively stores row-browser session.
	 */
	private void storeSession(RowBrowser rb, Map<RowBrowser, Integer> browserNumber, FileWriter out) throws IOException {
		if (rb.browserContentPane.table != null) {
			String csv = browserNumber.get(rb) + "; " + (rb.parent == null? "" : browserNumber.get(rb.parent)) + "; ";
			
			String where = rb.browserContentPane.getAndConditionText().trim();
			
			if (rb.browserContentPane.parentRow != null) {
				if (where.length() > 0) {
					where = "(" + where + ") and (" + rb.browserContentPane.parentRow.rowId + ")";
				} else {
					where = rb.browserContentPane.parentRow.rowId;
				}
			}
			
			csv += where + "; ";
			
			csv += rb.internalFrame.getLocation().x + "; " + rb.internalFrame.getLocation().y + "; ";
			csv += rb.internalFrame.getSize().width + "; " + rb.internalFrame.getSize().height + "; ";
			csv += rb.browserContentPane.limitBox.getSelectedItem() + "; " + rb.browserContentPane.selectDistinctCheckBox.isSelected() + "; ";

			if (rb.browserContentPane.table instanceof BrowserContentPane.SqlStatementTable) {
				csv += "Q; " + CsvFile.encodeCell(rb.browserContentPane.sqlBrowserContentPane.sqlEditorPane.getText()) + "; ";
			} else {
				csv += "T; " + CsvFile.encodeCell(rb.browserContentPane.table.getName()) + "; "
					+ (rb.association == null? "" : CsvFile.encodeCell(rb.association.getName())) + "; ";
			}
			csv += rb.isHidden() + "; ";
			out.append(csv).append(LF);
			for (RowBrowser child: tableBrowsers) {
				if (child.parent == rb) {
					storeSession(child, browserNumber, out);
				}
			}
		}
	}

	/**
	 * Restores browser session.
	 */
	public void restoreSession(RowBrowser toBeAppended) {
		File startDir = CommandLineParser.getInstance().newFile("layout");
		Component pFrame = SwingUtilities.getWindowAncestor(this);
		if (pFrame == null) {
			pFrame = this;
		}
		String sFile = UIUtil.choseFile(null, startDir.getPath(), toBeAppended == null? "Restore Layout" : "Append Layout", ".dbl", pFrame, true, true);
		String tbaPeerID = null;
		
		if (sFile != null) {
			try {
				Map<String, RowBrowser> rbByID = new HashMap<String, Desktop.RowBrowser>();
				List<Line> lines = new CsvFile(new File(sFile)).getLines();
				if (toBeAppended == null) {
					closeAll();
				}
				Collection<RowBrowser> toBeLoaded = new ArrayList<Desktop.RowBrowser>();
				List<String> unknownTables = new ArrayList<String>();
				for (CsvFile.Line l: lines) {
					if (l.cells.get(0).equals("Layout")) {
						try {
							if (toBeAppended == null) {
								layoutMode = LayoutMode.valueOf(l.cells.get(1));
								updateMenu(layoutMode);
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
					int limit = Integer.parseInt(l.cells.get(7));
					boolean selectDistinct = Boolean.parseBoolean(l.cells.get(8));
					RowBrowser rb = null;
					if ("T".equals(l.cells.get(9))) {
						Table table = datamodel.get().getTable(l.cells.get(10));
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
								rb = addTableBrowser(parentRB, -1, table, parentRB != null? association : null, where, limit, selectDistinct, false);
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
							rb = addTableBrowser(null, 0, null, null, where, limit, selectDistinct, false);
							toBeLoaded.add(rb);
						}
					}
					if (rb != null) {
						rb.setHidden(Boolean.parseBoolean(l.cells.get(12)));
						if (toBeAppended == null) {
							rb.internalFrame.setLocation(loc);
							rb.internalFrame.setSize(size);
						}
					}
				}
				checkDesktopSize();
				makePrimaryRootVisible();
				
				for (RowBrowser rb: toBeLoaded) {
					rb.browserContentPane.reloadRows();
				}
				if (toBeAppended != null && toBeLoaded.isEmpty()) {
					JOptionPane.showMessageDialog(pFrame, "Layout doesn't contain table \"" + datamodel.get().getDisplayName(toBeAppended.browserContentPane.table) + "\" as root.");
				} else if (!unknownTables.isEmpty()) {
					String pList = "";
					for (String ut: unknownTables) {
						pList += ut + "\n";
					}
					JOptionPane.showMessageDialog(pFrame, "Unknown tables:\n\n" + pList + "\n");
				}
				if (toBeAppended == null) {
					currentSessionFileName = sFile;
				}
			} catch (Throwable e) {
				UIUtil.showException(this, "Error", e);
			}
		}
	}

	private void makePrimaryRootVisible() {
		RowBrowser root = null;
		for (RowBrowser rb: getRootBrowsers(true)) {
			if (rb.browserContentPane.table != null) {
				if (!(rb.browserContentPane.table instanceof BrowserContentPane.SqlStatementTable)) {
					root = rb;
					break;
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
			this.scrollRectToVisible(new Rectangle(0, 0, 1, 1));
		}
	}

	public JInternalFrame[] getAllFramesFromTableBrowsers() {
		List<JInternalFrame> frames = new ArrayList<JInternalFrame>();
		for (RowBrowser rb: tableBrowsers) {
			frames.add(rb.internalFrame);
		}
		return frames.toArray(new JInternalFrame[frames.size()]);
	}

	public List<RowBrowser> getRootBrowsers(boolean ignoreHidden) {
		List<RowBrowser> roots = new ArrayList<Desktop.RowBrowser>();
		
		if (ignoreHidden) {
			for (RowBrowser rb: tableBrowsers) {
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
			for (RowBrowser rb: tableBrowsers) {
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
			for (RowBrowser rb: tableBrowsers) {
				if (rb.parent == parent) {
					if (rb.isHidden()) {
						roots.addAll(getChildBrowsers(rb, true));
					} else {
						roots.add(rb);
					}
				}
			}
		} else {
			for (RowBrowser rb: tableBrowsers) {
				if (rb.parent == parent) {
					roots.add(rb);
				}
			}
		}
		return roots;
	}

	/**
	 * Adjusts scroll-position of each table browser s.t. rows in closure are visible.
	 * 
	 * @param tabu don't adjust this one
	 */
	protected synchronized void adjustClosure(BrowserContentPane tabu) {
		for (RowBrowser rb: tableBrowsers) {
			if (rb.browserContentPane == tabu) {
				continue;
			}
			List<Row> rowsOfRB = new ArrayList<Row>();
			for (Pair<BrowserContentPane, Row> r: currentClosure) {
				if (r.a == rb.browserContentPane) {
					rowsOfRB.add(r.b);
				}
			}
			if (!rowsOfRB.isEmpty()) {
				Rectangle firstRowPos = null;
				Rectangle lastRowPos = null;
				Rectangle visibleRect = rb.browserContentPane.rowsTable.getVisibleRect();
				for (Row r: rowsOfRB) {
					int index = rb.browserContentPane.rows.indexOf(r);
					if (index < 0) {
						for (int n = 0; n < rb.browserContentPane.rows.size(); ++n) {
							if (r.rowId.equals(rb.browserContentPane.rows.get(n).rowId)) {
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
					if (pos.y >= visibleRect.y && pos.y + pos.height < visibleRect.y + visibleRect.height) {
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
				if (lastRowPos != null) {
					rb.browserContentPane.rowsTable.scrollRectToVisible(new Rectangle(visibleRect.x, lastRowPos.y - lastRowPos.height, 1, 3 * lastRowPos.height));
				}
				if (firstRowPos != null) {
					rb.browserContentPane.rowsTable.scrollRectToVisible(new Rectangle(visibleRect.x, firstRowPos.y - firstRowPos.height, 1, 3 * firstRowPos.height));
				}
			}
		}
		
		repaintDesktop();
	}

	/**
	 * Opens new Browser and adds complete sub-tree of {@link RowBrowser}.
	 * 
	 * @param tableBrowser the root
	 */
	private void showInNewWindow(RowBrowser tableBrowser) {
		DataBrowser newDataBrowser = openNewDataBrowser();
		if (newDataBrowser != null) {
			newDataBrowser.desktop.layoutMode = layoutMode;
			newDataBrowser.desktop.updateMenu(layoutMode);

			StringBuilder cond = new StringBuilder();
			Set<String> known = new HashSet<String>();
			synchronized (this) {
				for (Row r: tableBrowser.browserContentPane.rows) {
					if (!known.contains(r.rowId)) {
						known.add(r.rowId);
						if (cond.length() > 0) {
							cond.append(" or \n");
						}
						cond.append("(" + SqlUtil.replaceAliases(r.rowId, "A", "A") + ")");
					}
				}
			}
			
			RowBrowser root = addTableBrowserSubTree(newDataBrowser, tableBrowser, null, cond.toString());
			root.browserContentPane.reloadRows();
			newDataBrowser.arrangeLayout();
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
	
	private RowBrowser addTableBrowserSubTree(DataBrowser newDataBrowser, RowBrowser tableBrowser, RowBrowser parent, String rootCond) {
		Object limitO = tableBrowser.browserContentPane.limitBox.getSelectedItem();
		Integer limit = null;
		if (limitO instanceof Integer) {
			limit = (Integer) limitO;
		}
		RowBrowser rb;
		if (parent == null) {
			rb = newDataBrowser.desktop.addTableBrowser(null, -1, tableBrowser.browserContentPane.table, null, rootCond == null? tableBrowser.browserContentPane.getAndConditionText() : rootCond, limit, tableBrowser.browserContentPane.selectDistinctCheckBox.isSelected(), false);
		} else {
			rb = newDataBrowser.desktop.addTableBrowser(parent, tableBrowser.rowIndex, tableBrowser.browserContentPane.table, tableBrowser.browserContentPane.association, rootCond == null? tableBrowser.browserContentPane.getAndConditionText() : rootCond, limit, tableBrowser.browserContentPane.selectDistinctCheckBox.isSelected(), false);
		}
		rb.setHidden(tableBrowser.isHidden());
		
		for (RowBrowser child: getChildBrowsers(tableBrowser, false)) {
			addTableBrowserSubTree(newDataBrowser, child, rb, null);
		}
		return rb;
	}

	protected abstract DataBrowser openNewDataBrowser();

	/**
	 * Scrolls an iFrame to the center of the desktop.
	 */
	public void scrollToCenter(JInternalFrame iFrame) {
		demaximize();
		int w = getVisibleRect().width;
		int h = getVisibleRect().height;
		int x = iFrame.getBounds().x + iFrame.getBounds().width / 2 - getVisibleRect().width / 2;
		int y = iFrame.getBounds().y + iFrame.getBounds().height / 2 - getVisibleRect().height / 2;
		if (x < 0) {
			w += x;
			x = 0;
		}
		if (y < 0) {
			h += y;
			y = 0;
		}
		Rectangle r = new Rectangle(x, y, Math.max(1, w), Math.max(1, h));
		scrollRectToVisible(r);
	}

	/**
	 * Collect layout of tables in a extraction model.
	 * 
	 * @param positions to put positions into
	 */
	private void collectPositions(RowBrowser root, Map<String, Map<String, double[]>> positions) {
		List<Pair<RowBrowser, Pair<Integer, Integer>>> toDo = new LinkedList<Pair<RowBrowser,Pair<Integer,Integer>>>();
		toDo.add(new Pair<RowBrowser, Pair<Integer, Integer>>(root, new Pair<Integer, Integer>(1, 1)));
		String subject = root.browserContentPane.table.getName(); // datamodel.get().getDisplayName(root.browserContentPane.table);
		double scaleX = 0.35 / layoutMode.factor;
		double scaleY = 0.3 / layoutMode.factor;
		double scher = 2;
		
		while (!toDo.isEmpty()) {
			Pair<RowBrowser, Pair<Integer, Integer>> rowBrowser = toDo.remove(0);
			int i = 1;
			for (RowBrowser child: getChildBrowsers(rowBrowser.a, true)) {
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
				tablePos.put(table, new double[] { x * scaleX + scher * (2 * (rowBrowser.b.b % 2) - 1), y * scaleY + scher * (2 * (rowBrowser.b.a % 2) - 1), 1.0 });
//			} else {
//				double[] pos = tablePos.get(table);
//				tablePos.put(table, new double[] { pos[0], pos[1], 0.0 });
			}
		}
	}

	/**
	 * For concurrent reload of rows.
	 */
	private final LinkedBlockingQueue<LoadJob> runnableQueue = new LinkedBlockingQueue<LoadJob>();

	/**
	 * Maximum number of concurrent DB connections.
	 */
	private static int MAX_CONCURRENT_CONNECTIONS = 4;
	private static int desktopNr = 1;
	{
		// initialize listeners for #runnableQueue
		for (int i = 0; i < MAX_CONCURRENT_CONNECTIONS; ++i) {
			Thread t = new Thread(new Runnable() {
				@Override
				public void run() {
					for (;;) {
						try {
							runnableQueue.take().run();
						} catch (InterruptedException e) {
							// ignore
						}
					}
				}
			}, "browser-" + desktopNr + "-" + i);
			t.setDaemon(true);
			t.start();
		}
		desktopNr++;
	}
	
}
