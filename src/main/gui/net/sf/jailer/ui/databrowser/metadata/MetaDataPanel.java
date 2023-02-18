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
package net.sf.jailer.ui.databrowser.metadata;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Frame;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.image.BufferedImage;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NavigableMap;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Vector;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Pattern;

import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListModel;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.JToggleButton;
import javax.swing.JTree;
import javax.swing.ListCellRenderer;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TreeExpansionEvent;
import javax.swing.event.TreeExpansionListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.event.TreeWillExpandListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.ExpandVetoException;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import org.fife.rsta.ui.EscapableDialog;
import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.configuration.DatabaseObjectRenderingDescription;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;
import net.sf.jailer.modelbuilder.MemorizedResultSet;
import net.sf.jailer.modelbuilder.ModelBuilder;
import net.sf.jailer.ui.AutoCompletion;
import net.sf.jailer.ui.JComboBox2;
import net.sf.jailer.ui.RowCountRenderingHelper;
import net.sf.jailer.ui.StringSearchPanel;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.UIUtil.IconWithText;
import net.sf.jailer.ui.UIUtil.PLAF;
import net.sf.jailer.ui.databrowser.Row;
import net.sf.jailer.util.Quoting;

/**
 * Meta Data UI.
 *
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public abstract class MetaDataPanel extends javax.swing.JPanel {

	/**
	 * The logger.
	 */
	private static final Logger logger = LoggerFactory.getLogger(MetaDataPanel.class);

    private final MetaDataSource metaDataSource;
    private final JComboBox2<String> tablesComboBox;
    private final DataModel dataModel;
    private final MetaDataDetailsPanel metaDataDetailsPanel;
    private final Frame parent;
    private final JToggleButton searchButton;
    private final ExecutionContext executionContext;

    private final Object CATEGORY_VIEWS = new String("Views");
    private final Object CATEGORY_TABLES = new String("Tables");
    private final Object CATEGORY_SYNONYMS = new String("Synonyms");

    /**
	 * Packages list view.
	 */
    private class MDPackages extends MDDescriptionBasedGeneric {

		private MDPackages(String name, MetaDataSource metaDataSource, final MDSchema schema, DataModel dataModel) {
			super(name, metaDataSource, schema, dataModel, new DatabaseObjectRenderingDescription() {
				{
					setIconURL("/net/sf/jailer/ui/resource/packages.png");
					DatabaseObjectRenderingDescription itemDescr = new DatabaseObjectRenderingDescription();
					itemDescr.setIconURL("/net/sf/jailer/ui/resource/package.png");
					itemDescr.setTextQuery(schema.getMetaDataSource().getSession().dbms.getProcedureSourceQuery());
					setItemDescription(itemDescr);
				}
			});
		}

		@Override
		public List<MDDescriptionBasedGeneric> getDetails() {
			ArrayList<MDDescriptionBasedGeneric> result = new ArrayList<MDDescriptionBasedGeneric>();
			try {
				MemorizedResultSet theList = retrieveList(getMetaDataSource().getSession());
				for (final Object[] row: theList.getRowList()) {
					MDProcedures procs = new MDProcedures(String.valueOf(row[0]), metaDataSource, schema, dataModel) {
						@Override
						protected boolean select(Object[] proc) {
							return proc[2] != null && proc[2].equals(row[0]);
						}
					};
					procs.databaseObjectRenderingDescription.setIconURL("/net/sf/jailer/ui/resource/package.png");
					procs.databaseObjectRenderingDescription.setTextQuery(schema.getMetaDataSource().getSession().dbms.getPackageSourceQuery());
					result.add(procs);
				}
			} catch (Throwable t) {
				logger.info("error", t);
			}
			return result;
		}

		@Override
		public MemorizedResultSet retrieveList(Session session, String query, String schema, String parentName) throws SQLException {
			Set<String> cats = null;
			if (session.dbms.getPackageNamesQuery() != null) {
				Statement cStmt = null;
		        try {
		            Connection connection = session.getConnection();
		            cStmt = connection.createStatement();
		            if (schema != null) {
		            	schema = Quoting.staticUnquote(schema);
		            }
		            ResultSet rs = cStmt.executeQuery(String.format(Locale.ENGLISH, session.dbms.getPackageNamesQuery(), schema));
		            MemorizedResultSet result = new MemorizedResultSet(rs, null, session, schema);
		            result.close();
		            rs.close();
		            cats = new TreeSet<String>();
		            for (Object[] cat: result.getRowList()) {
		            	cats.add((String) cat[0]);
		            }
		        } catch (Exception e) {
		        	logger.info("error", e);
		        } finally {
		            if (cStmt != null) {
		                try {
		                    cStmt.close();
		                } catch (SQLException e) {
		                }
		            }
		        }
			}
			if (cats == null) {
				cats = new TreeSet<String>();
				ResultSet rs = getProcedures(session, schema, "%");
				while (rs.next()) {
					String cat = rs.getString(1);
					if (cat != null) {
						cats.add(cat);
					}
				}
				rs.close();
			}
			List<Object[]> catList = new ArrayList<Object[]>();
			for (String cat: cats) {
				catList.add(new Object[] { cat });
			}
			MemorizedResultSet result = new MemorizedResultSet(catList, 1, new String[] { "Package", }, new int[] { Types.VARCHAR });
			return result;
		}
	}

	/**
	 * Procedures list view.
	 */
    private abstract class MDProcedures extends MDDescriptionBasedGeneric {
		private final MDSchema mdSchema;

		public MDProcedures(String name, MetaDataSource metaDataSource, final MDSchema schema, DataModel dataModel) {
			super(name, metaDataSource, schema, dataModel, new DatabaseObjectRenderingDescription() {
				{
					setIconURL("/net/sf/jailer/ui/resource/procedures.png");
					DatabaseObjectRenderingDescription itemDescr = new DatabaseObjectRenderingDescription();
					itemDescr.setIconURL("/net/sf/jailer/ui/resource/procedure.png");
					itemDescr.setTextQuery(schema.getMetaDataSource().getSession().dbms.getProcedureSourceQuery());
					setItemDescription(itemDescr);
				}
			});
			this.mdSchema = schema;
		}

		@Override
		public MemorizedResultSet retrieveList(Session session, String query, String schema, String parentName) throws SQLException {
			if (query != null) {
				return super.retrieveList(session, query, schema, parentName);
			}
			MemorizedResultSet procs = new MemorizedResultSet(getProcedures(session, schema, "%"),
					null, session, schema, new int[] { 3, 4, 1, 8, 9 }, new String[] { "Name", "Remarks", "Category", "Type", "SpecificName" });
			List<Object[]> catList = new ArrayList<Object[]>();
			for (Object[] cat: procs.getRowList()) {
				if (select(cat)) {
					catList.add(cat);
				}
			}
			procs.close();
			return new MemorizedResultSet(catList, procs.getMetaData());
		}

		@Override
		protected int getDetailIDIndex() {
			if (mdSchema.getMetaDataSource().getSession().dbms.isProcedureDetailNeedsSpecificName()) {
				return 4;
			}
			return 0;
		}

		protected abstract boolean select(Object[] proc);

		@Override
		protected DatabaseObjectRenderingDescription itemDescription(MemorizedResultSet item) {
			DatabaseObjectRenderingDescription desc = new DatabaseObjectRenderingDescription(databaseObjectRenderingDescription.getItemDescription());
			if (!item.getRowList().isEmpty() && String.valueOf(DatabaseMetaData.procedureReturnsResult).equals(String.valueOf(item.getRowList().get(0)[3]))) {
				desc.setIconURL("/net/sf/jailer/ui/resource/function.png");
				desc.setTextQuery(mdSchema.getMetaDataSource().getSession().dbms.getFunctionSourceQuery());
			}
			return desc;
		}
	}

	/**
	 * Constraints list view.
	 */
    private class MDConstraint extends MDDescriptionBasedGeneric {
		private final MDSchema mdSchema;

		private Map<String, String> constraintTypeNames = new LinkedHashMap<String, String>();
		{
			constraintTypeNames.put("PK", "Primary Keys");
			constraintTypeNames.put("Unique", "Unique Constraints");
			constraintTypeNames.put("Check", "Check Constraints");
		}

		public MDConstraint(String name, MetaDataSource metaDataSource, final MDSchema schema, DataModel dataModel) {
			super(name, metaDataSource, schema, dataModel, new DatabaseObjectRenderingDescription() {
				{
					setIconURL("/net/sf/jailer/ui/resource/constraints.png");
					DatabaseObjectRenderingDescription itemDescr = new DatabaseObjectRenderingDescription();
					itemDescr.setTextQuery(schema.getMetaDataSource().getSession().dbms.getProcedureSourceQuery());
					setItemDescription(itemDescr);
				}
			});
			this.databaseObjectRenderingDescription.setCheap(false);
			this.mdSchema = schema;
		}

		@Override
		public MemorizedResultSet retrieveList(Session session, String query, String schema, String parentName) throws SQLException {
			if (query != null) {
				return super.retrieveList(session, query, schema, parentName);
			}
			return mdSchema.getConstraints(null, MetaDataPanel.this.dataModel);
		}

		/**
		 * Gets a list of descriptions of the details.
		 *
		 * @return list of descriptions of the details
		 */
		@Override
		public List<MDDescriptionBasedGeneric> getDetails() {
			ArrayList<MDDescriptionBasedGeneric> result = new ArrayList<MDDescriptionBasedGeneric>();
			try {
				MemorizedResultSet theList = retrieveList(getMetaDataSource().getSession());
				Map<String, String> typeNames = new LinkedHashMap<String, String>(constraintTypeNames);
				for (final Object[] row: theList.getRowList()) {
					String type = String.valueOf(row[0]);
					if (!typeNames.containsKey(type)) {
						typeNames.put(type, type);
					}
				}
				for (Entry<String, String> e: typeNames.entrySet()) {
					List<Object[]> rowsPerType = new ArrayList<Object[]>();
					ArrayList<MDDescriptionBasedGeneric> descs = new ArrayList<MDDescriptionBasedGeneric>(); // lgtm [java/unused-container]
					for (final Object[] row: theList.getRowList()) {
						if (String.valueOf(row[0]).equals(e.getKey())) {
							rowsPerType.add(row);
						}
					}
					for (final Object[] row: theList.getRowList()) {
						MemorizedResultSet detailRS = new MemorizedResultSet(Collections.singletonList(row), theList.getMetaData());
						DatabaseObjectRenderingDescription detailDesc = itemDescription(detailRS);
						if (detailDesc != null) {
							MDDescriptionBasedGeneric mdDetails = createDetailDescription(row, detailDesc);
							mdDetails.detailName = String.valueOf(row[getDetailIDIndex()]);
							if (detailDesc.getListQuery() == null) {
								mdDetails.list = detailRS;
							}
							descs.add(mdDetails);
						}
					}
					DatabaseObjectRenderingDescription desc = new DatabaseObjectRenderingDescription();
					desc.setItemDescription(new DatabaseObjectRenderingDescription());
					final MemorizedResultSet listPerType = new MemorizedResultSet(rowsPerType, theList.getMetaData());
					final UIUtil.IconWithText label = MDSchema.getConstraintTypeIcon(e.getKey() + "s");
					result.add(new MDDescriptionBasedGeneric(e.getValue(), getMetaDataSource(), schema, dataModel, desc) {
						@Override
						protected MemorizedResultSet retrieveList(Session session) throws SQLException { // lgtm [java/non-sync-override]
							listPerType.reset();
							return listPerType;
						}

						@Override
						public List<MDDescriptionBasedGeneric> getDetails() {
							return super.getDetails();
						}

						@Override
						protected MDDescriptionBasedGeneric createDetailDescription(final Object[] row, DatabaseObjectRenderingDescription detailDesc) {
							final IconWithText label = (IconWithText) row[0];
							return new MDDescriptionBasedGeneric(row[1] + " on " + row[2] + (row[3] != null && row[3].toString().trim().length() > 0? "(" + row[3] + ")" : ""), getMetaDataSource(), schema, dataModel, detailDesc) {
								@Override
								public ImageIcon getIcon() {
									return label == null? null : label.icon;
								}
							};
						}

						@Override
						public ImageIcon getIcon() {
							return label == null? null : label.icon;
						}
					});
				}
			} catch (Throwable t) {
				logger.info("error", t);
			}
			return result;
		}
		@Override
		protected MemorizedResultSet distinct(MemorizedResultSet list) throws SQLException {
			return list;
		}

		@Override
		public boolean hasDetails() {
			return true;
		}
	}

    private List<MDDescriptionBasedGeneric> getGenericDatabaseObjects(final MDSchema mdSchema) {
		List<MDDescriptionBasedGeneric> genericDatabaseObjects = new ArrayList<MDDescriptionBasedGeneric>();
		genericDatabaseObjects.add(
				new MDProcedures("Procedures", metaDataSource, mdSchema, dataModel) {
					@Override
					protected boolean select(Object[] proc) {
						return proc[2] == null || !DBMS.ORACLE.equals(mdSchema.getMetaDataSource().getSession().dbms);
					}
				}
			);
		if (DBMS.ORACLE.equals(mdSchema.getMetaDataSource().getSession().dbms)) {
			genericDatabaseObjects.add(new MDPackages("Packages", metaDataSource, mdSchema, dataModel));
		}
		if (mdSchema.getMetaDataSource().getSession().dbms.getConstraintsQuery() != null) {
			genericDatabaseObjects.add(new MDConstraint("Constraints", metaDataSource, mdSchema, dataModel));
		}
		for (DatabaseObjectRenderingDescription desc: mdSchema.getMetaDataSource().getSession().dbms.getObjectRenderers()) {
			MDDescriptionBasedGeneric mdObjectRenderer
				= new MDDescriptionBasedGeneric(
						desc.getName(), metaDataSource, mdSchema, dataModel, desc);
			genericDatabaseObjects.add(mdObjectRenderer);
		}
		return genericDatabaseObjects;
	}

	private Map<String, MemorizedResultSet> proceduresPerSchema = Collections.synchronizedMap(new HashMap<String, MemorizedResultSet>());

	public ResultSet getProcedures(Session session, String schema, String context) throws SQLException {
		synchronized (proceduresPerSchema) {
			MemorizedResultSet rs = proceduresPerSchema.get(schema);
			if (rs == null) {
				if (schema != null) {
	            	schema = Quoting.staticUnquote(schema);
	            }
				rs = new MemorizedResultSet(JDBCMetaDataBasedModelElementFinder.getProcedures(session, Quoting.staticUnquote(schema), context), null, session, "");
				List<Object[]> result = new ArrayList<Object[]>();
				for (Object[] row: rs.getRowList()) {
//						1.TABLE_CAT String => table catalog (may be null) 
//						2.TABLE_SCHEM String => table schema (may be null)
					if (!Quoting.equalsWROSearchPattern(Quoting.staticUnquote(schema), String.valueOf(row[1 - 1]), String.valueOf(row[2 - 1]))) {
						continue;
					}
					result.add(row);
		        }
				rs.close();
				proceduresPerSchema.put(schema, new MemorizedResultSet(result, rs.getMetaData()));
			}
			rs.reset();
			return rs;
		}
	}

	private abstract class ExpandingMutableTreeNode extends DefaultMutableTreeNode {

        public ExpandingMutableTreeNode() {
            super("loading...");
        }

        protected abstract void expandImmediatelly();
        protected abstract void expand();
    }

    /**
     * Creates new form MetaDataPanel
     *
     * @param metaDataSource the meta data source
     * @param dataModel the data mmodel
     */
    @SuppressWarnings({ "rawtypes", "unchecked" })
    public MetaDataPanel(Frame parent, MetaDataSource metaDataSource, MetaDataDetailsPanel metaDataDetailsPanel, final DataModel dataModel, ExecutionContext executionContext) {
        this.metaDataSource = metaDataSource;
        this.dataModel = dataModel;
        this.metaDataDetailsPanel = metaDataDetailsPanel;
        this.parent = parent;
        this.executionContext = executionContext;
        initComponents();
        
        GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        JPanel p;
		jLayeredPane1.add(p = new JPanel(new BorderLayout()) {
        	{
        		setOpaque(false);
        	}
			@Override
			public void paint(Graphics g) {
				super.paint(g);
				if (g instanceof Graphics2D) {
					paintRowCounters((Graphics2D) g, getBounds());
				}
			}
        	
        }, gridBagConstraints);
		jLayeredPane1.setLayer(p, javax.swing.JLayeredPane.POPUP_LAYER);
        addComponentListener(new ComponentListener() {
        	int width = -1;
			@Override
			public void componentResized(ComponentEvent e) {
				if (getWidth() != width) {
					width = getWidth();
					updateRowCounters();
				}
			}
			@Override
			public void componentShown(ComponentEvent e) {
			}
			@Override
			public void componentMoved(ComponentEvent e) {
			}
			@Override
			public void componentHidden(ComponentEvent e) {
			}
		});
        
        metaDataTree.addTreeExpansionListener(new TreeExpansionListener() {
			@Override
			public void treeExpanded(TreeExpansionEvent event) {
				updateRowCounters();
			}
			@Override
			public void treeCollapsed(TreeExpansionEvent event) {
				updateRowCounters();
			}
		});
        
		if (UIUtil.plaf == PLAF.FLAT) {
			splitPane.setDividerSize(16);
		}
		
        cancelButton.setIcon(UIUtil.scaleIcon(cancelButton, cancelIcon));
		
        refreshButton.setText(null);
        refreshButton.setIcon(resetIcon);
        
        hideOutline();
        outlineScrollPane.setVisible(false);
        lastDividerLocation = -1;

        final ListCellRenderer olRenderer = outlineList.getCellRenderer();
        outlineList.setCellRenderer(new ListCellRenderer() {
            @Override
            public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected,
                    boolean cellHasFocus) {
            	String tooltip = null;
            	boolean withSeparator = false;
            	double level = 0;
                if (value instanceof OutlineInfo) {
                    tooltip = ((OutlineInfo) value).tooltip;
                    withSeparator = ((OutlineInfo) value).withSeparator;
                    level = ((OutlineInfo) value).level;
                    if (((OutlineInfo) value).isCTE) {
                    	level += 0.5;
                    }
                    value = outlineTableRender((OutlineInfo) value, isSelected);
                }
                Component render = olRenderer.getListCellRendererComponent(list, value, index, false, cellHasFocus);
                render.setBackground(isSelected? new Color(240, 240, 255) : index == indexOfInfoAtCaret? new Color(255, 255, 170) : Color.WHITE);
                if (render instanceof JLabel) {
                	((JLabel) render).setToolTipText(tooltip);

                	if (withSeparator) {
                		final Border border = ((JLabel) render).getBorder() == null? new EmptyBorder(0, 0, 0, 0) : ((JLabel) render).getBorder();
						final Color bg = ((JLabel) render).getBackground();
						final int ind = (int) (level * 22);
						final int SEP_LENGTH = 300;
                		((JLabel) render).setBorder(new Border() {
							@Override
							public void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {
								border.paintBorder(c, g, x, y, width, height);
								if (g instanceof Graphics2D) {
									Graphics2D g2d = (Graphics2D) g;
									Color color = new Color(140, 158, 255);
									int ofs = 50;
									GradientPaint paint = new GradientPaint(
										x + ind - 20, 0, bg==null ? Color.WHITE : bg,
										x + ind + ofs, 0, color);
									g2d.setPaint(paint);
									g2d.fillRect(x + ind - 20, 0, ofs + 20, 1);
									if (ind + ofs < SEP_LENGTH) {
										paint = new GradientPaint(
												x + ind + ofs, 0, color,
												x + SEP_LENGTH, 0, bg==null ? Color.WHITE : bg);
										g2d.setPaint(paint);
										g2d.fillRect(x + ind + ofs, 0, SEP_LENGTH, 1);
									}
								}
							}
							@Override
							public Insets getBorderInsets(Component c) {
								return border.getBorderInsets(c);
							}
							@Override
							public boolean isBorderOpaque() {
								return border.isBorderOpaque();
							}
                		});
                	}

                }
                return render;
            }
        });

        outlineList.addMouseListener(new MouseListener() {
			@Override
			public void mouseReleased(MouseEvent e) {
			}
			@Override
			public void mousePressed(MouseEvent e) {
			}
			@Override
			public void mouseExited(MouseEvent e) {
			}
			@Override
			public void mouseEntered(MouseEvent e) {
			}
			@Override
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() > 1) {
					int i = outlineList.locationToIndex(e.getPoint());
					if (i >= 0) {
						Object value = outlineList.getModel().getElementAt(i);
						if (value instanceof OutlineInfo) {
							setCaretPosition(((OutlineInfo) value).origPosition);
						}
					}
				}
			}
		});

        outlineList.addListSelectionListener(new ListSelectionListener() {
            @Override
            public void valueChanged(ListSelectionEvent e) {
                Object value = outlineList.getSelectedValue();
                if (!inSelectOutlineTable && value instanceof OutlineInfo) {
                    MDTable mdTable = ((OutlineInfo) value).mdTable;
                    if (mdTable != null) {
                        inSelectOutlineTable = true;
                        select(mdTable);
                        inSelectOutlineTable = false;
                    }
                }
            }
        });

        tablesComboBox = new JComboBox2<String>() {
            @Override
            public Dimension getMinimumSize() {
                return new Dimension(40, super.getMinimumSize().height);
            }
        };
        tablesComboBox.setMaximumRowCount(20);
        AutoCompletion.enable(tablesComboBox);

        tablesComboBox.grabFocus();

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1;
        add(tablesComboBox, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.NONE;
        gridBagConstraints.weightx = 0;
		final Window owner = parent;
		final MetaDataSource metaDataSource1 = metaDataSource;
        searchButton = StringSearchPanel.createSearchButton(owner, tablesComboBox, "Select Table", new Runnable() {
		    @Override
		    public void run() {
		        onSelectTable();
		    }
		}, new StringSearchPanel.Prepare() {
		    @Override
		    public void prepare(Set<MDSchema> selectedSchemas) {
		        updateTablesCombobox(selectedSchemas);
		    }
		}, metaDataSource1, dataModel, false, null, true, false, null, false, 
        		() -> {
        			Map<String, Integer> stringCount = new HashMap<String, Integer>();
        			if (metaDataSource != null) {
        		        for (int i = 0; i < tablesComboBox.getModel().getSize(); ++i) {
        		        	Object item = tablesComboBox.getModel().getElementAt(i);
        		        	MDTable mdTable = null;
        		        	if (item != null) {
        		                Table table = dataModel.getTableByDisplayName(item.toString());
        		                if (table != null) {
        		                    mdTable = metaDataSource.toMDTable(table);
        		                } else {
        		                	mdTable = tablesComboboxMDTablePerName.get(item);
        		                }
        		            }
        		        	if (mdTable != null) {
        		        		Long count = mdTable.getEstimatedRowCount();
        		        		if (count != null) {
        		        			stringCount.put((String) item, count.intValue());
        		        		}
         		        	}
        	        	}
        	        }
        			return stringCount;
        		}
        );
        jToolBar1.add(searchButton);

        tablesComboBox.setVisible(false);
        refreshButton1.setVisible(false);
        searchButton.setText("Open Table");
        searchButton.setIcon(tableIcon);

        metaDataTree.addKeyListener(new KeyListener() {
            @Override
            public void keyTyped(KeyEvent e) {
                if (e.getKeyChar() == '\n') {
                    if (metaDataTree.getSelectionPath() != null) {
                        Object last = metaDataTree.getSelectionPath().getLastPathComponent();
                        if (last instanceof DefaultMutableTreeNode) {
                            final Object uo = ((DefaultMutableTreeNode) last).getUserObject();
                            if (uo instanceof MDTable) {
                                openTable((MDTable) uo);
                            }
                        }
                    }
                }
            }

            @Override
            public void keyReleased(KeyEvent e) {
            }

            @Override
            public void keyPressed(KeyEvent e) {
            }
        });

        metaDataTree.addMouseListener(new MouseListener() {
            @Override
            public void mouseReleased(MouseEvent e) {
            }
            @Override
            public void mousePressed(MouseEvent e) {
            }
            @Override
            public void mouseExited(MouseEvent e) {
            }
            @Override
            public void mouseEntered(MouseEvent e) {
            }
            @Override
            public void mouseClicked(MouseEvent evt) {
            	 final MDTable mdTable = findTable(evt);
            	 TreePath node = findNode(evt);
            	 if (node != null) {
            		 metaDataTree.scrollRectToVisible(new Rectangle(0, (int) evt.getPoint().getY(), 1, 1));
            	 }
            	 final Set<MDTable> mdTables = new LinkedHashSet<MDTable>();
           		 findTables(mdTables);
            	 if (evt.getButton() == MouseEvent.BUTTON3) {
                    if (mdTable != null || !mdTables.isEmpty()) {
                    	int itemCount = 0;
                        JPopupMenu popup = new JPopupMenu();
                        if (mdTable != null) {
	                        JMenuItem open = new JMenuItem("Open");
	                        popup.add(open);
	                        ++itemCount;
	                        open.addActionListener(new ActionListener() {
	                            @Override
	                            public void actionPerformed(ActionEvent e) {
	                                openTable(mdTable);
	                            }
	                        });
                        }
                        if (mdTable != null && MetaDataPanel.this.metaDataSource.toTable(mdTable) == null) {
                            if (itemCount > 0) {
                            	popup.add(new JSeparator());
                            }
                            JMenuItem analyse = new JMenuItem("Analyse schema \""+ mdTable.getSchema().getUnquotedName() + "\"");
                            popup.add(analyse);
                            ++itemCount;
                            analyse.addActionListener(new ActionListener() {
                                @Override
                                public void actionPerformed(ActionEvent e) {
                                    analyseSchema(mdTable.getSchema().getName());
                                }
                            });
                        }
                        if (!mdTables.isEmpty()) {
                            if (itemCount > 0) {
                            	popup.add(new JSeparator());
                            }
                            JMenu menu = new JMenu("Create Script");
                            popup.add(menu);
                            ++itemCount;
                            Point pos = new Point(evt.getX(), evt.getY());
							SwingUtilities.convertPointToScreen(pos, evt.getComponent());
                            menu.add(createScriptMenuItem("\"Create Table\" Script", "DDL", "", mdTables, false, pos));
                            menu.add(createScriptMenuItem("\"Drop Table\" Script", "Drop %2$s %1$s;", "", mdTables, false, pos));
                            menu.add(new JSeparator());
                            menu.add(createScriptMenuItem("\"Delete\" Script", "Delete from %1$s;", "", mdTables, false, pos));
                            menu.add(createScriptMenuItem("\"Count Rows\" Script", "Select '%1$s' as Tab, count(*) as NumberOfRows From %1$s", " union all", mdTables, true, pos));
                        }
                        popup.show(evt.getComponent(), evt.getX(), evt.getY());
                    }
                }
                if (evt.getButton() == MouseEvent.BUTTON1) {
                    if (mdTable != null) {
                        if (evt.getClickCount() > 1) {
                        	if (node != null && metaDataTree.getSelectionModel().isPathSelected(node)) {
                        		openTable(mdTable);
                        	}
                        }
                    }
                }
            }
            private JMenuItem createScriptMenuItem(String title, final String template, final String separator, final Set<MDTable> mdTables, final boolean execute, final Point pos) {
                JMenuItem item = new JMenuItem(title);
                final AtomicBoolean cancelLoading = new AtomicBoolean(false);

                item.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(ActionEvent e) {
                    	final Window wa = SwingUtilities.getWindowAncestor(MetaDataPanel.this);
                    	final boolean withWaitDialog;
                    	if ("DDL".equals(template) && mdTables.size() > 1) {
                    		cancelLoading.set(false);
                    		withWaitDialog = true;
	                    	UIUtil.invokeLater(new Runnable() {
								@Override
								public void run() {
									JDialog dialog;
									dialog = new EscapableDialog((wa instanceof Frame)? (Frame) wa : null, "Script") {
									};
									dialog.setModal(true);
									waitDialog.set(dialog);
									dialog.getContentPane().add(waitingPanel);
									dialog.pack();
									dialog.setLocation(pos.x + dialog.getWidth() / 2 - dialog.getWidth() / 2, pos.y + dialog.getHeight() / 2 - dialog.getHeight() / 2);
									dialog.setVisible(true);
									waitDialog.set(null);
									cancelLoading.set(true);
								}
	                    	});
                    	} else {
                    		waitDialog.set(null);
                    		withWaitDialog = false;
                    	}
                    	MDSchema.loadMetaData(new Runnable() {
							@Override
							public void run() {
				        		if (withWaitDialog) {
			                    	for (MDTable mdTable: mdTables) {
			                    		mdTable.getDDL();
			                    		if (cancelLoading.get()) {
			                    			break;
			                    		}
			                    	}
				        		}
		                    	UIUtil.invokeLater(new Runnable() {
									@Override
									public void run() {
			                    		if (!cancelLoading.get()) {
					                    	StringBuilder script = new StringBuilder();
					                    	for (MDTable mdTable: mdTables) {
					                    		if (script.length() > 0) {
					                    			script.append(separator + UIUtil.LINE_SEPARATOR);
					                    		}
					                    		if ("DDL".equals(template)) {
						                    		String ddl = mdTable.getDDL().trim().replaceAll("\\nON ", " ON ");
													script.append(ddl);
													if (!ddl.endsWith(";")) {
														script.append(";");
													}
					                    		} else {
						                    		String tableName;
						                    		String tableType = "Table";
						                    		if (mdTable.isView()) {
						                    			tableType = "View";
						                    		}
						                    		if (mdTable.isSynonym()) {
						                    			tableType = "Synonym";
						                    		}
						                    		if (mdTable.getSchema().isDefaultSchema) {
						                    			tableName = mdTable.getName();
						                    		} else {
						                    			tableName = mdTable.getSchema() + "." + mdTable.getName();
						                    		}
						                    		script.append(String.format(Locale.ENGLISH, template, tableName, tableType));
					                    		}
					                    	}
					            			script.append("\n");
											appendScript(script.toString(), execute);
				                    	}
						        		if (waitDialog.get() != null) {
						        			waitDialog.get().dispose();
						        			waitDialog.set(null);
						        		}
									}
								});
							}
						}, 2);
                    }
                });
                return item;
			}
			private void findTables(Set<MDTable> mdTables) {
				TreePath[] paths = metaDataTree.getSelectionPaths();
				if (paths != null) {
					for (TreePath path: paths) {
						findTables(mdTables, path.getLastPathComponent());
					}
				}
			}
			private void findTables(Set<MDTable> mdTables, Object node) {
				if (node instanceof DefaultMutableTreeNode && node != metaDataTree.getModel().getRoot()) {
					DefaultMutableTreeNode defaultMutableTreeNode = (DefaultMutableTreeNode) node;
					Object selNode = defaultMutableTreeNode.getUserObject();
                    if (selNode instanceof MDTable) {
                        mdTables.add((MDTable) selNode);
                    }
                    for (int i = 0; i < defaultMutableTreeNode.getChildCount(); ++i) {
                    	findTables(mdTables, defaultMutableTreeNode.getChildAt(i));
                    }
				}
			}
			private MDTable findTable(MouseEvent evt) {
                MDTable mdTable = null;
                TreePath node = findNode(evt);
                if (node != null) {
                    Object sel = node.getLastPathComponent();
                    if (sel instanceof DefaultMutableTreeNode) {
                        Object selNode = ((DefaultMutableTreeNode) sel).getUserObject();
                        if (selNode instanceof MDTable) {
                            mdTable = (MDTable) selNode;
                        }
                    }
                }
                return mdTable;
            }
			private TreePath findNode(MouseEvent evt) {
				TreePath node = metaDataTree.getPathForLocation(evt.getX(), evt.getY());
                if (node == null) {
                    for (int x = metaDataTree.getWidth(); x > 0; x -= 32) {
                        node = metaDataTree.getPathForLocation(x, evt.getY());
                        if (node != null) {
                            break;
                        }
                    }
                }
				return node;
			}
        });

        metaDataTree.addTreeWillExpandListener(new TreeWillExpandListener() {
            @Override
            public void treeWillExpand(TreeExpansionEvent event) throws ExpandVetoException {
                Object node = event.getPath().getLastPathComponent();
                if (node instanceof DefaultMutableTreeNode) {
                    DefaultMutableTreeNode treeNode = (DefaultMutableTreeNode) node;
                    if (treeNode.getChildCount() == 1 && treeNode.getChildAt(0) instanceof ExpandingMutableTreeNode)
                    ((ExpandingMutableTreeNode) treeNode.getChildAt(0)).expand();
                }
            }

            @Override
            public void treeWillCollapse(TreeExpansionEvent event) throws ExpandVetoException {
            }
        });

        final ImageIcon finalScaledWarnIcon = getScaledIcon(this, warnIcon, true);

        DefaultTreeCellRenderer renderer = new DefaultTreeCellRenderer() {
        	Pattern htmlRE = Pattern.compile("\\s*<\\s*html\\s*>.*", Pattern.DOTALL|Pattern.CASE_INSENSITIVE);
            Map<MDTable, Boolean> dirtyTables = new HashMap<MDTable, Boolean>();
            @Override
            public Component getTreeCellRendererComponent(JTree tree, Object value, boolean sel, boolean expanded,
                    boolean leaf, int row, boolean hasFocus) {
                boolean unknownTable = false;
                boolean isJailerTable = false;
                boolean isView = false;
                boolean isSynonym = false;
                Boolean isDirty = false;
                ImageIcon image = null;
                if (value instanceof DefaultMutableTreeNode) {
                    Object uo = ((DefaultMutableTreeNode) value).getUserObject();
                    if (uo instanceof JLabel) {
                    	Icon icon = ((JLabel) uo).getIcon();
                    	if (icon instanceof ImageIcon) {
                    		image = (ImageIcon) ((JLabel) uo).getIcon();
                    	}
                    }
                    if (uo == CATEGORY_VIEWS) {
                    	image = viewsIcon;
                    }
                    if (uo == CATEGORY_SYNONYMS) {
                    	image = synonymsIcon;
                    }
                    if (uo == CATEGORY_TABLES) {
                    	image = tablesIcon;
                    }
                    if (uo instanceof MDDatabase) {
                        image = databaseIcon;
                    }
                    if (uo instanceof MDSchema) {
                    	image = schemaIcon;
                    }
                    if (uo instanceof MDDescriptionBasedGeneric) {
                    	image = ((MDDescriptionBasedGeneric) uo).getIcon();
                    }
                    if (uo instanceof MDTable) {
                        Table table = MetaDataPanel.this.metaDataSource.toTable((MDTable) uo);
                        if (table == null) {
                            unknownTable = true;
                        } else {
                            if (((MDTable) uo).isLoaded()) {
                                isDirty = dirtyTables.get(uo);
                                if (isDirty == null) {
                                    isDirty = !((MDTable) uo).isUptodate(table);
                                    dirtyTables.put(((MDTable) uo), isDirty);
                                }
                            }
                        }
                        isView = ((MDTable) uo).isView();
                        isSynonym = ((MDTable) uo).isSynonym();
                        if (isView) {
                        	image = viewIcon;
                        } else if (isSynonym) {
                        	image = synonymIcon;
                        } else {
                        	image = tableIcon;
                        }
                        if (ModelBuilder.isJailerTable(((MDTable) uo).getUnquotedName())) {
                            isJailerTable = true;
                        }
                    }
                }
                Component comp = super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);
            	if (comp instanceof JLabel) {
            		String text = ((JLabel) comp).getText();
            		if (htmlRE.matcher(text).matches()) {
            			((JLabel) comp).setText("<html>" + UIUtil.toHTMLFragment(text, 100) + "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</html>");
            		}
            	}
                if (isJailerTable && !sel) {
                	if (comp instanceof JLabel) {
                    	((JLabel) comp).setForeground(Color.gray);
                	}
                }
                if (image != null) {
                	if (comp instanceof JLabel) {
                    	((JLabel) comp).setIcon(image);
                	}
                }
                if ((unknownTable || isDirty) && !isJailerTable) {
                	if (comp instanceof JLabel) {
                		if (image != null) {
                			ImageIcon combinedIcon = combinedIcons.get(image);
                			if (combinedIcon == null) {
	                			Image im = image.getImage();
	                			// create the new image, canvas size is the max. of both image sizes
	                			int w = Math.max(image.getIconWidth(), finalScaledWarnIcon.getIconWidth());
	                			int h = Math.max(image.getIconHeight(), finalScaledWarnIcon.getIconHeight());
	                			BufferedImage combined = new BufferedImage(w * 2, h * 2, BufferedImage.TYPE_INT_ARGB);

	                			// paint both images, preserving the alpha channels
	                			Graphics2D g = (Graphics2D) combined.getGraphics();
	                			g.scale(2, 2);
	                			g.drawImage(im, 0, 0, null);
	                			g.translate(image.getIconWidth() / 4, image.getIconHeight() / 4);
	                			g.drawImage(finalScaledWarnIcon.getImage(), 0, 0, null);
	                			combinedIcon = UIUtil.scaleIcon(new ImageIcon(combined), 0.5);
	                			combinedIcons.put(image, combinedIcon);
                			}
							((JLabel) comp).setIcon(combinedIcon);
                		} else {
                			((JLabel) comp).setIcon(finalScaledWarnIcon);
                		}
                	}
                }
                if (UIUtil.plaf == PLAF.FLAT) {
					setTextSelectionColor(tree.hasFocus()? Color.white : null);
				}
                
                return comp;
            }
        };
        renderer.setOpenIcon(null);
        renderer.setLeafIcon(null);
        renderer.setClosedIcon(null);
        metaDataTree.setCellRenderer(renderer);
        metaDataTree.getSelectionModel().setSelectionMode(TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION);

        metaDataTree.addTreeSelectionListener(new TreeSelectionListener() {
            @Override
            public void valueChanged(TreeSelectionEvent e) {
                TreePath path = e.getNewLeadSelectionPath();
                if (path != null) {
                    final Object last = path.getLastPathComponent();
                    if (metaDataTree.getModel().getRoot() == last) {
//						 searchButton.doClick();
                    }
                    if (last instanceof DefaultMutableTreeNode) {
                        final Object uo = ((DefaultMutableTreeNode) last).getUserObject();
                        if (uo instanceof MDTable) {
                            selectOutlineTable((MDTable) uo);
                            Table table = MetaDataPanel.this.metaDataSource.toTable((MDTable) uo);
                            if (table != null) {
                                updateDataModelView(table);
                            }
                        }
                        UIUtil.invokeLater(new Runnable() {
                            @Override
                            public void run() {
                                UIUtil.invokeLater(new Runnable() {
                                    @Override
                                    public void run() {
                                        UIUtil.invokeLater(new Runnable() {
                                            @Override
                                            public void run() {
                                            	UIUtil.setWaitCursor(MetaDataPanel.this);
                                        	    try {
					                                if (metaDataTree.getSelectionPath() != null && metaDataTree.getSelectionPath().getLastPathComponent() == last) {
					                                    if (uo instanceof MDSchema) {
					                                        onSchemaSelect((MDSchema) uo);
					                                    } else if (uo instanceof MDTable) {
					                                        onTableSelect((MDTable) uo);
					                                    } else if (uo instanceof MDGeneric) {
					                                        onMDOtherSelect((MDGeneric) uo, MetaDataPanel.this.executionContext);
					                                    } else {
					                                        onMDOtherSelect(new MDGeneric("", MetaDataPanel.this.metaDataSource) {
																@Override
																public JComponent createRender(Session session,
																		ExecutionContext executionContext) throws Exception {
																	return new JLabel("");
																}
					                                        }, MetaDataPanel.this.executionContext);
					                                    }
					                                }
                                        	    } finally {
                                        	    	UIUtil.resetWaitCursor(MetaDataPanel.this);
                                        	    }
                                            }
                                        });
                                    }
                                });
                            }
                        });
                    }
                }
            }
        });

        updateTreeModel(metaDataSource);

        Font font = outlineLabel.getFont();
        if (font != null) {
            Font bold = font.deriveFont(font.getStyle() | Font.BOLD, font.getSize());
            outlineLabel.setFont(bold);
        }
    }

	private Map<String, MDTable> tablesComboboxMDTablePerName = new HashMap<String, MDTable>();

    private void updateTablesCombobox(Set<MDSchema> selectedSchemas) {
        Set<String> tableSet = new HashSet<String>();
        Set<MDSchema> toLoad = new HashSet<MDSchema>();

        for (Table table: dataModel.getTables()) {
        	MDSchema mdSchema = metaDataSource.getSchemaOfTable(table);
        	if (mdSchema != null && !mdSchema.isLoaded()) {
        		toLoad.add(mdSchema);
        	} else if (metaDataSource.toMDTable(table) == null) {
                String schemaName = table.getSchema("");
                MDSchema schema;
                if (schemaName.isEmpty()) {
                    schema = metaDataSource.getDefaultSchema();
                } else {
                    schema = metaDataSource.find(schemaName);
                }
                if (schema != null && selectedSchemas.contains(schema)) {
                    String displayName = dataModel.getDisplayName(table);
                    tableSet.add(displayName);
                }
            }
        }
        for (MDSchema schema: selectedSchemas) {
            if (!schema.isLoaded()) {
            	toLoad.add(schema);
            } else {
                for (MDTable table: schema.getTables()) {
                    if (!ModelBuilder.isJailerTable(table.getName())) {
                        String name;
                        if (!schema.isDefaultSchema) {
                            name = schema.getName() + "." + table.getName();
                        } else {
                            name = table.getName();
                        }
                        tableSet.add(name);
                        tablesComboboxMDTablePerName.put(name, table);
                    }
                }
            }
        }
        List<String> tables = new ArrayList<String>(tableSet);
        Collections.sort(tables, new Comparator<String>() {
            @Override
            public int compare(String o1, String o2) {
                return o1.compareToIgnoreCase(o2);
            }
        });
        ComboBoxModel model = new DefaultComboBoxModel(new Vector(tables));
        tablesComboBox.setModel(model);

        for (MDSchema schema: toLoad) {
        	schema.loadTables(true, null, null, null);
        }
    }

    protected void openTable(MDTable mdTable) {
        Table table = metaDataSource.toTable(mdTable);
        if (table != null) {
            open(table);
        } else {
            open(mdTable);
        }
    }

    private int inResetCount = 0;
	private static AtomicInteger pendingCount = new AtomicInteger();

    public void reset() {
    	if (pendingCount.get() > 0) {
    		return;
    	}
		inResetCount++;
    	refreshButton.setEnabled(false);
    	final AtomicBoolean waitStateIsResetted = new AtomicBoolean(false);
    	Timer timer = new Timer(100, new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (!waitStateIsResetted.get()) {
					setOrResetWaitState(true);
				}
			}
		});
		timer.setRepeats(false);
		timer.start();

    	JDBCMetaDataBasedModelElementFinder.resetCaches(metaDataSource.getSession());
        setOutline(new ArrayList<OutlineInfo>(), -1);
        // proceduresPerSchema.clear();
        proceduresPerSchema = Collections.synchronizedMap(new HashMap<String, MemorizedResultSet>()); // dont wait

        MDTable selectedTable = null;
        if (metaDataTree.getSelectionPath() != null) {
            Object last = metaDataTree.getSelectionPath().getLastPathComponent();
            if (last instanceof DefaultMutableTreeNode) {
                final Object uo = ((DefaultMutableTreeNode) last).getUserObject();
                if (uo instanceof MDTable) {
                    selectedTable = (MDTable) uo;
                }
            }
        }
        final MDTable finalSelectedTable = selectedTable;

		pendingCount.incrementAndGet();
		Thread thread = new Thread(new Runnable() {
			@Override
			public void run() {
				try {
					Session.setThreadSharesConnection();
					synchronized (metaDataSource) {
						pendingCount.getAndDecrement();
						metaDataSource.clear();
					}
				} finally {
					UIUtil.invokeLater(new Runnable() {
			            @Override
			            public void run() {
		                	if (--inResetCount <= 0) {
				            	try {
				                    metaDataDetailsPanel.reset();
				                    updateTreeModel(metaDataSource);
				                    if (finalSelectedTable != null) {
				                        final MDSchema schema = metaDataSource.find(finalSelectedTable.getSchema().getName());
				                        ActionListener al = new ActionListener() {
				                        	int cnt = 10;
				                			@Override
				                			public void actionPerformed(ActionEvent e) {
				                				if (schema != null) {
				                					if (!schema.isLoaded()) {
				                						if (--cnt > 0) {
				                							Timer timer = new Timer(200, this);
				    				                		timer.setRepeats(false);
				    				                		timer.start();
				                						}
			    				                		return;
				                					}
						                            MDTable table = schema.find(finalSelectedTable.getName());
						                            if (table != null) {
						                            	select(table);
						                            }
				                				}
				                			}
				                        };
				                        Timer timer = new Timer(100, al);
				                		timer.setRepeats(false);
				                		timer.start();
				                    }
				                } finally {
				                	refreshButton.setEnabled(true);
				                	setOrResetWaitState(false);
				                	waitStateIsResetted.set(true);
				                }
		                	}
			            }
			        });
				}
			}
		});
		thread.setDaemon(true);
		thread.start();
    }

    public void select(Table table) {
    	MDSchema mdSchema = metaDataSource.getSchemaOfTable(table);
    	if (mdSchema != null && !mdSchema.isLoaded()) {
    		mdSchema.loadTables(true, null, null, null);
    		return;
    	}
    	select(metaDataSource.toMDTable(table));
    }

    public void select(MDTable mdTable) {
        if (mdTable != null) {
            TreePath path = find(metaDataTree.getModel().getRoot(), mdTable);
            if (path != null) {
                selectSchema(mdTable.getSchema(), false);
                metaDataTree.expandPath(path);
                UIUtil.invokeLater(12,() -> updateRowCounters());
                metaDataTree.getSelectionModel().setSelectionPath(path);
                scrollToNode(path);
            } else {
                selectSchema(mdTable.getSchema());
            }
        }
    }

    private void scrollToNode(TreePath path) {
        Rectangle bounds = metaDataTree.getPathBounds(path);
        if (bounds != null) {
        	int b = 18;
        	bounds = new Rectangle(bounds.x, Math.max(bounds.y - b, 0), bounds.width, bounds.height + 2 * b);
            metaDataTree.scrollRectToVisible(new Rectangle(0, bounds.y, 1, bounds.height));
        }
    }

    private TreePath find(Object root, MDTable mdTable) {
        if (root instanceof DefaultMutableTreeNode) {
            Object userObject = ((DefaultMutableTreeNode) root).getUserObject();
            if (userObject instanceof MDSchema) {
                if (mdTable.getSchema().equals(userObject)) {
                	int cc = ((DefaultMutableTreeNode) root).getChildCount();
                    for (int i = 0; i < cc; ++i) {
                        TreeNode catNode = ((DefaultMutableTreeNode) root).getChildAt(i);
                        Object catUO = ((DefaultMutableTreeNode) catNode).getUserObject();
                        if (catNode.getChildCount() > 0 && (CATEGORY_TABLES == catUO || CATEGORY_VIEWS == catUO || CATEGORY_SYNONYMS == catUO)) {
                        	TreeNode firstChild = catNode.getChildAt(0);
                        	if (firstChild instanceof ExpandingMutableTreeNode) {
                        		((ExpandingMutableTreeNode) firstChild).expandImmediatelly();
                        	}
                        }
                    }
                }
            } else if (userObject instanceof MDTable) {
                if (((MDTable) userObject).getSchema() == mdTable.getSchema() && ((MDTable) userObject).getName().equals(mdTable.getName())) {
                    return new TreePath(((DefaultMutableTreeNode) root).getPath());
                }
            }
            int cc = ((DefaultMutableTreeNode) root).getChildCount();
            for (int i = 0; i < cc; ++i) {
                TreePath path = find(((DefaultMutableTreeNode) root).getChildAt(i), mdTable);
                if (path != null) {
                    return path;
                }
            }
        }
        return null;
    }

    private DefaultMutableTreeNode root;
    private Map<MDSchema, DefaultMutableTreeNode> treeNodePerSchema = new HashMap<MDSchema, DefaultMutableTreeNode>();

    private void updateTreeModel(final MetaDataSource metaDataSource) {
        root = new DefaultMutableTreeNode(new MDDatabase(metaDataSource.dataSourceName, metaDataSource, dataModel, executionContext));
        for (final MDSchema schema: metaDataSource.getSchemas()) {
            final DefaultMutableTreeNode schemaChild = new DefaultMutableTreeNode(schema);
            for (final MDDescriptionBasedGeneric desc: getGenericDatabaseObjects(schema)) {
                Iterable<Object> leafs = new Iterable<Object>() {
    				@Override
    				public Iterator<Object> iterator() {
    					List<Object> details = new ArrayList<Object>();
    		        	for (MDDescriptionBasedGeneric detail: desc.getDetails()) {
    		        		details.add(detail);
    		        	}
    		        	return details.iterator();
    				}
    			};
                createCategoryNode(schemaChild, leafs, desc, desc.hasDetails(), desc.isCheap()? 0 : 1);
            }
            Iterable<Object> leafs = new Iterable<Object>() {
				@Override
				public Iterator<Object> iterator() {
		        	List<Object> leafs = new ArrayList<Object>();
		            for (MDTable table: schema.getTables()) {
						if (table.isView()) {
							leafs.add(table);
						}
		            }
		            return leafs.iterator();
				}
			};
            createCategoryNode(schemaChild, leafs, CATEGORY_VIEWS, true, 0);
            leafs = new Iterable<Object>() {
				@Override
				public Iterator<Object> iterator() {
		        	List<Object> leafs = new ArrayList<Object>();
		            for (MDTable table: schema.getTables()) {
						if (table.isSynonym()) {
							leafs.add(table);
						}
		            }
		            return leafs.iterator();
				}
			};
            createCategoryNode(schemaChild, leafs, CATEGORY_SYNONYMS, true, 0);
            leafs = new Iterable<Object>() {
				@Override
				public Iterator<Object> iterator() {
					List<Object> leafs = new ArrayList<Object>();
		            for (MDTable table: schema.getTables(true, new Runnable() {
						@Override
						public void run() {
							refresh();
						}
					}, new Runnable() {
						@Override
						public void run() {
							refresh();
						}
					})) {
						if (!table.isView() && !table.isSynonym()) {
							leafs.add(table);
						}
		            }
		            return leafs.iterator();
				}
			};
            DefaultMutableTreeNode schemaTablesChild = createCategoryNode(schemaChild, leafs, CATEGORY_TABLES, true, 0);
            treeNodePerSchema.put(schema, schemaTablesChild);
        }
        DefaultTreeModel treeModel = new DefaultTreeModel(root);
        metaDataTree.setModel(treeModel);
		metaDataTree.scrollRectToVisible(new Rectangle(0, 0, 1, 1));
        MDSchema defaultSchema = metaDataSource.getDefaultSchema();
        final Map<String, String> schemaMapping = getSchemaMapping();
		if (schemaMapping != null) {
			String mappedName = schemaMapping.get("");
			if (mappedName == null && defaultSchema != null) {
				mappedName = schemaMapping.get(Quoting.staticUnquote(defaultSchema.getName()));
			}
			if (mappedName == null && defaultSchema != null) {
				try {
					mappedName = schemaMapping.get(Quoting.getQuoting(metaDataSource.getSession()).requote(defaultSchema.getName()));
				} catch (SQLException e) {
					mappedName = null;
				}
			}
			if (mappedName != null) {
				String fMappedName = mappedName;
				Optional<MDSchema> mappedSchema = metaDataSource.getSchemas().stream().filter(s -> Quoting.equalsIgnoreQuotingAndCase(fMappedName, s.getName())).findAny();
				if (mappedSchema.isPresent()) {
					defaultSchema = mappedSchema.get();
				}
			}
		}
		selectSchema(defaultSchema);
        UIUtil.invokeLater(12, () -> {
        	updateRowCounters();
            jScrollPane1.repaint();	
        });
    }

	public DefaultMutableTreeNode createCategoryNode(final DefaultMutableTreeNode schemaChild, final Iterable<Object> finalLeafs,
			Object category, boolean hasDetails, final int queueId) {
		final DefaultMutableTreeNode schemaViewsChild = new DefaultMutableTreeNode(category);
		schemaChild.add(schemaViewsChild);
		root.add(schemaChild);
		MutableTreeNode expandSchema;
		if (hasDetails) {
			synchronized (schemaChild) {
				expandSchema = new ExpandingMutableTreeNode() {
					private Iterable<Object> leafs = finalLeafs;
					private boolean expanded = false;
				    @Override
				    protected void expandImmediatelly() {
				        if (!expanded) {
				        	for (Object leaf: leafs) {
				                DefaultMutableTreeNode tableChild = new DefaultMutableTreeNode(leaf);
				                schemaViewsChild.add(tableChild);
				                if (leaf instanceof MDDescriptionBasedGeneric) {
				                	MDDescriptionBasedGeneric md = (MDDescriptionBasedGeneric) leaf;
				                	for (MDDescriptionBasedGeneric detail: md.getDetails()) {
				                		tableChild.add(new DefaultMutableTreeNode(detail));
				                	}
				                }
				            }
				            schemaViewsChild.remove(this);
				            TreeModel model = metaDataTree.getModel();
				            ((DefaultTreeModel) model).nodeStructureChanged(schemaViewsChild);
				        }
				        expanded = true;
				    }
				    @Override
				    protected void expand() {
				    	MDSchema.loadMetaData(new Runnable() {
							@Override
							public void run() {
								synchronized (schemaChild) {
									ArrayList<Object> leafList = new ArrayList<Object>();
									for (Object leaf: leafs) {
										leafList.add(leaf);
										 if (leaf instanceof MDDescriptionBasedGeneric) {
											 MDDescriptionBasedGeneric md = (MDDescriptionBasedGeneric) leaf;
											 md.getDetails();
										 }
									}
									leafs = leafList;
								}
						        UIUtil.invokeLater(new Runnable() {
						            @Override
						            public void run() {
								        UIUtil.invokeLater(new Runnable() {
								            @Override
								            public void run() {
								            	Window wa = SwingUtilities.getWindowAncestor(MetaDataPanel.this);
								                try {
								                	UIUtil.setWaitCursor(wa != null? wa : MetaDataPanel.this);
								                    expandImmediatelly();
								                } finally {
								                	UIUtil.resetWaitCursor(wa != null? wa : MetaDataPanel.this);
								                }
								            }
								        });
						            }
						        });

							}
						}, queueId);
				    }
				};
			}
			schemaViewsChild.add(expandSchema);
		}
		return schemaViewsChild;
	}

    public void selectSchema(MDSchema mdSchema) {
        selectSchema(mdSchema, true);
    }

    public void selectSchema(MDSchema mdSchema, boolean scrollToNode) {
        if (mdSchema != null) {
            DefaultMutableTreeNode node = treeNodePerSchema.get(mdSchema);
            if (node != null) {
            	final TreePath path = new TreePath(node.getPath());
                metaDataTree.expandPath(path);
                UIUtil.invokeLater(12,() -> updateRowCounters());
                metaDataTree.getSelectionModel().setSelectionPath(path);
                if (scrollToNode) {
                	UIUtil.invokeLater(new Runnable() {
						@Override
						public void run() {
							scrollToNode(path);
						}
                	});
                }
            }
        }
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        waitingPanel = new javax.swing.JPanel();
        jLabel5 = new javax.swing.JLabel();
        cancelButton = new javax.swing.JButton();
        splitPane = new javax.swing.JSplitPane();
        jPanel1 = new javax.swing.JPanel();
        refreshButton1 = new javax.swing.JButton();
        jToolBar1 = new javax.swing.JToolBar();
        jToolBar2 = new javax.swing.JToolBar();
        refreshButton = new javax.swing.JButton();
        jPanel3 = new javax.swing.JPanel();
        jLayeredPane1 = new javax.swing.JLayeredPane();
        jScrollPane1 = new javax.swing.JScrollPane();
        metaDataTree = new javax.swing.JTree();
        jPanel2 = new javax.swing.JPanel();
        outlineScrollPane = new javax.swing.JScrollPane();
        outlineList = new javax.swing.JList();
        outlineLabel = new javax.swing.JLabel();
        placeholderPanel = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();

        waitingPanel.setBackground(java.awt.Color.white);

        jLabel5.setForeground(java.awt.Color.red);
        jLabel5.setText("creating...");
        waitingPanel.add(jLabel5);

        cancelButton.setText("Cancel");
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });
        waitingPanel.add(cancelButton);

        setLayout(new java.awt.GridBagLayout());

        splitPane.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        splitPane.setResizeWeight(1.0);
        splitPane.setContinuousLayout(true);
        splitPane.setOneTouchExpandable(true);

        jPanel1.setLayout(new java.awt.GridBagLayout());

        refreshButton1.setText("Select");
        refreshButton1.setToolTipText("Choose the selecetd table in the tables tree");
        refreshButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                refreshButton1ActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 8);
        jPanel1.add(refreshButton1, gridBagConstraints);

        jToolBar1.setFloatable(false);
        jToolBar1.setRollover(true);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(jToolBar1, gridBagConstraints);

        jToolBar2.setFloatable(false);
        jToolBar2.setRollover(true);

        refreshButton.setText("Refresh");
        refreshButton.setToolTipText("Refresh Database Meta Data Cache");
        refreshButton.setFocusable(false);
        refreshButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        refreshButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        refreshButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                refreshButtonActionPerformed(evt);
            }
        });
        jToolBar2.add(refreshButton);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 5;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(jToolBar2, gridBagConstraints);

        jPanel3.setLayout(new java.awt.GridBagLayout());

        jLayeredPane1.setLayout(new java.awt.GridBagLayout());

        jScrollPane1.setViewportView(metaDataTree);

        jLayeredPane1.setLayer(jScrollPane1, javax.swing.JLayeredPane.PALETTE_LAYER);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jLayeredPane1.add(jScrollPane1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel3.add(jLayeredPane1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel1.add(jPanel3, gridBagConstraints);

        splitPane.setLeftComponent(jPanel1);

        jPanel2.setLayout(new java.awt.GridBagLayout());

        outlineList.setModel(new javax.swing.AbstractListModel() {
            String[] strings = { "Item 1", "Item 2", "Item 3", "Item 4", "Item 5" };
            public int getSize() { return strings.length; }
            public Object getElementAt(int i) { return strings[i]; }
        });
        outlineList.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
        outlineScrollPane.setViewportView(outlineList);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel2.add(outlineScrollPane, gridBagConstraints);

        outlineLabel.setText(" Outline");
        outlineLabel.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.VERTICAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        jPanel2.add(outlineLabel, gridBagConstraints);

        placeholderPanel.setLayout(new java.awt.GridBagLayout());

        jLabel1.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        placeholderPanel.add(jLabel1, gridBagConstraints);

        jLabel2.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        placeholderPanel.add(jLabel2, gridBagConstraints);

        jLabel3.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        placeholderPanel.add(jLabel3, gridBagConstraints);

        jLabel4.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 4;
        placeholderPanel.add(jLabel4, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        jPanel2.add(placeholderPanel, gridBagConstraints);

        splitPane.setRightComponent(jPanel2);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(splitPane, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    private void refreshButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_refreshButtonActionPerformed
        reset();
    }//GEN-LAST:event_refreshButtonActionPerformed

    private void refreshButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_refreshButton1ActionPerformed
        onSelectTable();
    }//GEN-LAST:event_refreshButton1ActionPerformed

    private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
        JDialog waitingDialog = waitDialog.get();
    	if (waitingDialog != null) {
    		waitingDialog.dispose();
    	}
    }//GEN-LAST:event_cancelButtonActionPerformed

    private List<OutlineInfo> outlineTables = new ArrayList<OutlineInfo>();
    private boolean inSelectOutlineTable = false;
	private boolean isOutlineVisible = true;
	private int lastDividerLocation;

    private void showOutline() {
    	if (!isOutlineVisible) {
    		outlineScrollPane.setVisible(true);
    		placeholderPanel.setVisible(true);
    		splitPane.setDividerLocation(Math.min(lastDividerLocation, splitPane.getHeight() - 80));
    		if (lastDividerLocation == -1) {
	        	final int PREFERRED_HEIGHT = 250;
	        	int prefHeight = Math.min(PREFERRED_HEIGHT, (int) (splitPane.getHeight() * 0.4));
	        	if (prefHeight > outlineScrollPane.getHeight()) {
	        		splitPane.setDividerLocation(splitPane.getHeight() - prefHeight);
	        	}
    		}
    		UIUtil.invokeLater(new Runnable() {
	            @Override
	            public void run() {
	                TreePath path = metaDataTree.getSelectionPath();
	                if (path != null) {
	                    Rectangle bounds = metaDataTree.getPathBounds(path);
	                    if (bounds != null) {
	                    	metaDataTree.scrollRectToVisible(new Rectangle(bounds.x, bounds.y, 1, bounds.height));
	                    }
	                }
	            }
	        });
    		jPanel2.repaint();
    		isOutlineVisible = true;
    	}
    }

    private void hideOutline() {
    	if (isOutlineVisible) {
    		isOutlineVisible = false;
//    		outlineScrollPane.setVisible(false);
    		placeholderPanel.setVisible(false);
    		lastDividerLocation = splitPane.getDividerLocation();
    		splitPane.setDividerLocation(splitPane.getHeight() - 48);
    	}
    }

	private final String fontName; {
		String name = null;
		try {
			name = new RSyntaxTextArea().getFont().getName();
		} catch (Throwable t) {
			// ignore
		}
		fontName = name;
	}

    private String outlineTableRender(OutlineInfo info, boolean selected) {
    	String KEYWORD_ATTRIBUTES = "color=\"#0000cc\"";
		String face = fontName != null? "face=\"" + fontName + "\" " : "";
        String render = "<font " + face + "color=\"#000000\">";
		String KEYWORD_PREFIX = (fontName != null? "<font " + face + "" + face + ">" : "") + "<b>";
		String KEYWORD_SUFFIX = "</b>" + (fontName != null? "</font>" : "");
		String scopeDescriptor = "selecT froM duaL".equalsIgnoreCase(info.scopeDescriptor) && info.scopeDescriptor.substring(7).equals("from duaL")? info.scopeDescriptor.substring(0, 6) : info.scopeDescriptor;
		if (scopeDescriptor != null) {
			scopeDescriptor = scopeDescriptor.replace(" ", "&nbsp;");
		}
		if (info.mdTable != null || info.isCTE) {
        	if (info.isCTE) {
        		render = scopeDescriptor;
        	} else {
	            if (info.mdTable.getSchema().isDefaultSchema) {
	                render += info.mdTable.getName();
	            } else {
	                render += info.mdTable.getSchema().getName() + "." + info.mdTable.getName();
	            }
	            if (scopeDescriptor != null) {
	                render = "<font " + face + "" + KEYWORD_ATTRIBUTES + ">" + KEYWORD_PREFIX  + "" + scopeDescriptor + "" + KEYWORD_SUFFIX + "&nbsp;" + render;
	            }
        	}
            String alias = info.alias;
            if (alias != null) {
            	if (!render.isEmpty()) {
            		render += "&nbsp;";
            	}
                render += "<font " + face + "" + KEYWORD_ATTRIBUTES + ">" + KEYWORD_PREFIX + "as" + KEYWORD_SUFFIX + "</font>&nbsp;" + alias;
            }
            render += "</font>";
        } else if (scopeDescriptor != null) {
            render = "<font " + face + "" + KEYWORD_ATTRIBUTES + ">" + KEYWORD_PREFIX + "" + scopeDescriptor + "" + KEYWORD_SUFFIX + "" + (info.rowCount > 1? "&nbsp;<i>(" + info.rowCount + "&nbsp;rows)</i>" : "") + "</font>";
        }

        String indent = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;";
        if (info.isCTE && "".equals(info.alias)) {
        	render = "&nbsp;&nbsp;&nbsp;" + render;
        } else if (scopeDescriptor == null || (info.isCTE && !"".equals(scopeDescriptor))) {
        	render = indent + render;
        }
        for (int i = 0; i < info.level; ++i) {
        	render = indent + render;
        }
        if (info.context != null) {
        	render += "<font " + face + "color=\"#dd8888\">&nbsp;" + info.context + "</font>";
        }
        render = "<html>" + render + "<html>";
        return render;
    }

    private int indexOfInfoAtCaret = -1;

    public void setOutline(List<OutlineInfo> outlineTables, int indexOfInfoAtCaret) {
        this.outlineTables = new ArrayList<OutlineInfo>(outlineTables);
        this.indexOfInfoAtCaret = indexOfInfoAtCaret;
        DefaultListModel model = new DefaultListModel();
        for (OutlineInfo info: outlineTables) {
            model.addElement(info);
        }
        outlineList.setModel(model);
        if (outlineTables.isEmpty()) {
            hideOutline();
        } else {
            showOutline();
            if (indexOfInfoAtCaret >= 0) {
            	Rectangle aRect = outlineList.getCellBounds(indexOfInfoAtCaret, indexOfInfoAtCaret);
            	if (aRect != null) {
            		outlineList.scrollRectToVisible(aRect);
            	}
            }
        }
    }

    private void selectOutlineTable(MDTable mdTable) {
        if (!inSelectOutlineTable) {
            inSelectOutlineTable = true;
            boolean found = false;
            for (OutlineInfo value: outlineTables) {
                if (mdTable.equals(value.mdTable)) {
                    outlineList.setSelectedValue(value, true);
                    found = true;
                    break;
                }
            }
            if (!found) {
                outlineList.clearSelection();
            }
            inSelectOutlineTable = false;
        }
    }


    public static class OutlineInfo {
		public final MDTable mdTable;
        public final String alias;
        public int level;
        public int position;
        public String scopeDescriptor;
        public boolean withContext = false;
        public int contextPosition;
        public String context;
        public String tooltip;
		public int contextEnd = 0;
		public boolean isCTE;
		public boolean isBegin;
		public boolean isEnd;
        public boolean withSeparator;
		public int rowCount;
		public int origPosition;

        public OutlineInfo(MDTable mdTable, String alias, int level, int position, String scopeDescriptor) {
            this.mdTable = mdTable;
            this.alias = alias;
            this.level = level;
            this.position = position;
            this.origPosition = position;
            this.scopeDescriptor = scopeDescriptor;
        }
    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton cancelButton;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLayeredPane jLayeredPane1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JToolBar jToolBar1;
    private javax.swing.JToolBar jToolBar2;
    private javax.swing.JTree metaDataTree;
    private javax.swing.JLabel outlineLabel;
    private javax.swing.JList outlineList;
    private javax.swing.JScrollPane outlineScrollPane;
    private javax.swing.JPanel placeholderPanel;
    private javax.swing.JButton refreshButton;
    private javax.swing.JButton refreshButton1;
    private javax.swing.JSplitPane splitPane;
    private javax.swing.JPanel waitingPanel;
    // End of variables declaration//GEN-END:variables

    protected abstract void open(Table table);
    protected abstract void open(MDTable mdTable);
    protected abstract void analyseSchema(String schemaName);
    protected abstract void onTableSelect(MDTable mdTable);
    protected abstract void onRowSelect(Table mdTable, Row row);
    protected abstract void onSchemaSelect(MDSchema mdSchema);
	protected abstract void onMDOtherSelect(MDGeneric mdOther, ExecutionContext executionContext);
    protected abstract void openNewTableBrowser();
    protected abstract void updateDataModelView(Table table);
    protected abstract void setCaretPosition(int position);
	protected abstract void appendScript(String script, boolean execute);
	protected abstract void setOrResetWaitState(boolean set);
	protected abstract Map<String, String> getSchemaMapping();


    public void onSelectTable() {
        Object item = tablesComboBox.getSelectedItem();
        if (item != null) {
            Table table = dataModel.getTableByDisplayName(item.toString());
            if (table != null) {
                MDTable mdTable = metaDataSource.toMDTable(table);
                if (mdTable != null) {
                    select(mdTable);
                } else {
                    JOptionPane.showMessageDialog(parent, "Table \"" + dataModel.getDisplayName(table) + "\" does not exist in the database");
                }
            } else {
                MDTable mdTable = tablesComboboxMDTablePerName.get(item);
                if (mdTable != null) {
                    select(mdTable);
                }
            }
        }
    }

    public void refresh() {
    	UIUtil.invokeLater(12, new Runnable() {
    		@Override
    		public void run() {
    			metaDataTree.repaint();
    			metaDataTree.setSize(metaDataTree.getWidth(), metaDataTree.getHeight() - 1);
    		}
    	});
    }
    
    private NavigableMap<Integer, MDTable> rowCounters = new TreeMap<Integer, MDTable>();
    private RowCountRenderingHelper rowCountRenderingHelper = new RowCountRenderingHelper();
	
    private void paintRowCounters(Graphics2D g, Rectangle bounds) {
		Rectangle visibleRect = metaDataTree.getVisibleRect();
		g.clipRect(1, 1, visibleRect.width + 1, visibleRect.height);
		g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);
		g.setRenderingHint(RenderingHints.KEY_RENDERING,
                RenderingHints.VALUE_RENDER_QUALITY);
		int oh = UIUtil.plaf == PLAF.NIMBUS? 3 : 2;
		int ow = UIUtil.plaf == PLAF.NIMBUS? 1 : 0;
		rowCounters.subMap(visibleRect.y - 16, visibleRect.y + visibleRect.height + 16).forEach((ry, mdTable) -> {
			Long rc = mdTable.getEstimatedRowCount();
			if (rc != null) {
				String value;
				Color fg = new Color(0, 80, 200);
				
				if (rc == 0) {
					value = " ";
				} else if (rc >= 1000000000) {
					value = String.format("%,1.1f<font color=\"#960064\">" + rowCountRenderingHelper.nonMGSuffixG + "g</font>", (double) rc / 1000000000.0);
//					fg = new Color(150, 0, 100);
				} else if (rc >= 1000000) {
					value = String.format("%,1.1f<font color=\"#604000\">" + rowCountRenderingHelper.nonMGSuffixM + "m</font>", (double) rc / 1000000.0);
//					fg = new Color(0, 0, 150);
				} else if (rc >= 1000) {
					value = String.format("%,1.1f<font color=\"#000000\">" + rowCountRenderingHelper.nonMGSuffixK + "k</font>", (double) rc / 1000.0);
//					fg = new Color(0, 0, 150);
	     		} else {
	     			value = String.format("%,1.0f" + rowCountRenderingHelper.nonMGSuffix, (double) rc);
	     		}
	     		htmlRender.setText("<html><nobr> " + value + "</html>");
				htmlRender.setSize(htmlRender.getPreferredSize());
				int x = visibleRect.width - htmlRender.getWidth() - 1;
				int y = ry - visibleRect.y + htmlRender.getHeight() - 1;
				g.setColor(new Color(255, 255, 255));
				g.fillRect(x - 8, y - htmlRender.getHeight() + 2, visibleRect.width - x + 9 + ow, htmlRender.getHeight() + oh);
				htmlRender.setForeground(fg);
				y -= htmlRender.getHeight() - oh - 1;
				g.translate(x, y);
				//the fontMetrics stringWidth and height can be replaced by
				//getLabel().getPreferredSize() if needed
				htmlRender.paint((Graphics) g);
				(g).translate(-x, -y);
			}
		});
	}

    private JLabel htmlRender = new JLabel();
    
    private void updateRowCounters() {
		DefaultTreeModel m = (DefaultTreeModel) metaDataTree.getModel();
		TreeNode root2 = (TreeNode) m.getRoot();
		rowCounters = new TreeMap<Integer, MDTable>();
		trav(m, root2, new TreePath(root2), rowCounters);
		rowCounters.size();
	}
	
	private void trav(DefaultTreeModel m, TreeNode n, TreePath path, Map<Integer, MDTable> rowCounters) {
		if (n.isLeaf()) {
			Rectangle b = metaDataTree.getPathBounds(path);
			if (b != null) {
				Object uo = ((DefaultMutableTreeNode) n).getUserObject();
				if (uo instanceof MDTable) {
					boolean isView = ((MDTable) uo).isView();
                    if (!isView) {
                    	rowCounters.put(b.y, (MDTable) uo);
                    }
        		}
			}
		}
		Enumeration<? extends TreeNode> e = n.children();
		while (e != null && e.hasMoreElements()) {
			TreeNode nextElement = e.nextElement();
			trav(m, nextElement, path.pathByAddingChild(nextElement), rowCounters);
		}
	}
	
	static ImageIcon warnIcon;
    static ImageIcon viewIcon;
    static ImageIcon viewsIcon;
    static ImageIcon tableIcon;
    static ImageIcon tablesIcon;
    static ImageIcon synonymIcon;
    static ImageIcon synonymsIcon;
    static ImageIcon databaseIcon;
    static ImageIcon schemaIcon;
	private static ImageIcon resetIcon;
    private static HashMap<ImageIcon, ImageIcon> combinedIcons = new HashMap<ImageIcon, ImageIcon>();

    private final AtomicReference<JDialog> waitDialog = new AtomicReference<JDialog>(null);

    static ImageIcon getScaledIcon(JComponent component, ImageIcon icon, boolean small) {
		if (icon != null) {
			ImageIcon scaledIcon = icon;
			int heigth = component.getFontMetrics(new JLabel("M").getFont()).getHeight();
			double s = heigth / (double) scaledIcon.getIconHeight();
			if (icon == viewIcon) {
				s *= 0.8;
			}
			if (small) {
				s *= 0.8;
			}
			try {
				return UIUtil.scaleIcon(scaledIcon, s);
			} catch (Exception e) {
				logger.info("error", e);
				return null;
			}
		}
        return null;
    }

    private static ImageIcon cancelIcon;
	
	static {
        // load images
        cancelIcon = UIUtil.readImage("/buttoncancel.png");
	    warnIcon = UIUtil.readImage("/wanr.png");
        viewIcon = UIUtil.readImage("/view.png");
        synonymIcon = UIUtil.readImage("/synonym.png");
        synonymsIcon = UIUtil.readImage("/synonyms.png");
        viewsIcon = UIUtil.readImage("/views.png");
        tablesIcon = UIUtil.readImage("/tables.png");
        tableIcon = UIUtil.readImage("/table.png");
        databaseIcon = UIUtil.readImage("/database.png");
        schemaIcon = UIUtil.readImage("/schema.png");
        resetIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/reset.png"));
    }

}
