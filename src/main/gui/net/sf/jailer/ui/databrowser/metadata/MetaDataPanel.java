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
package net.sf.jailer.ui.databrowser.metadata;

import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Frame;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;
import java.util.Vector;

import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListModel;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JTree;
import javax.swing.ListCellRenderer;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TreeExpansionEvent;
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

import org.apache.log4j.Logger;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.configuration.DatabaseObjectRenderingDescription;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.modelbuilder.MemorizedResultSet;
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;
import net.sf.jailer.modelbuilder.ModelBuilder;
import net.sf.jailer.ui.AutoCompletion;
import net.sf.jailer.ui.JComboBox;
import net.sf.jailer.ui.StringSearchPanel;
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
	private static final Logger logger = Logger.getLogger(MetaDataPanel.class);

    private final MetaDataSource metaDataSource;
    private final JComboBox<String> tablesComboBox;
    private final DataModel dataModel;
    private final MetaDataDetailsPanel metaDataDetailsPanel;
    private final Frame parent;
    private final JButton searchButton;
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
		            ResultSet rs = cStmt.executeQuery(String.format(session.dbms.getPackageNamesQuery(), schema));
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
				ResultSet rs = getProcedures(session, session.getMetaData(), schema, "%");
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
			MemorizedResultSet procs = new MemorizedResultSet(getProcedures(session, session.getMetaData(), schema, "%"),
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
			this.mdSchema = schema;
		}

		@Override
		public MemorizedResultSet retrieveList(Session session, String query, String schema, String parentName) throws SQLException {
			if (query != null) {
				return super.retrieveList(session, query, schema, parentName);
			}
			return mdSchema.getConstraints(null);
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
					ArrayList<MDDescriptionBasedGeneric> descs = new ArrayList<MDDescriptionBasedGeneric>();
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
					final JLabel label = MDSchema.getConstraintTypeIcon(e.getKey() + "s");
					result.add(new MDDescriptionBasedGeneric(e.getValue(), getMetaDataSource(), schema, dataModel, desc) {
						protected MemorizedResultSet retrieveList(Session session) throws SQLException {
							listPerType.reset();
							return listPerType;
						}
						
						@Override
						public List<MDDescriptionBasedGeneric> getDetails() {
							return super.getDetails();
						}
						
						@Override
						protected MDDescriptionBasedGeneric createDetailDescription(final Object[] row, DatabaseObjectRenderingDescription detailDesc) {
							final JLabel label = (JLabel) row[0];
							return new MDDescriptionBasedGeneric(row[1] + " on " + row[2] + (row[3] != null && row[3].toString().trim().length() > 0? "(" + row[3] + ")" : ""), getMetaDataSource(), schema, dataModel, detailDesc) {
								@Override
								public Icon getIcon() {
									return label == null? null : label.getIcon();
								}
							};
						}

						@Override
						public Icon getIcon() {
							return label == null? null : label.getIcon();
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

	private Map<String, MemorizedResultSet> proceduresPerSchema = new HashMap<String, MemorizedResultSet>();

	public synchronized ResultSet getProcedures(Session session, DatabaseMetaData metaData, String schema, String context) throws SQLException {
		MemorizedResultSet rs = proceduresPerSchema.get(schema);
		if (rs == null) {
			if (schema != null) {
            	schema = Quoting.staticUnquote(schema);
            }
			rs = new MemorizedResultSet(JDBCMetaDataBasedModelElementFinder.getProcedures(session, metaData, Quoting.staticUnquote(schema), context), null, session, "");
			proceduresPerSchema.put(schema, rs);
		}
		rs.reset();
		return rs;
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
        
        hideOutline();
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
									Color color = new Color(100, 100, 255);
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
							setCaretPosition(((OutlineInfo) value).position);
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
        
        tablesComboBox = new JComboBox<String>() {
            @Override
            public Dimension getMinimumSize() {
                return new Dimension(40, super.getMinimumSize().height);
            }
        };
        tablesComboBox.setMaximumRowCount(20);
        AutoCompletion.enable(tablesComboBox);
        
        tablesComboBox.grabFocus();
        
        GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
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
        searchButton = StringSearchPanel.createSearchButton(parent, tablesComboBox, "Select Table", new Runnable() {
            @Override
            public void run() {
                onSelectTable();
            }
        }, new StringSearchPanel.Prepare() {
            @Override
            public void prepare(Set<MDSchema> selectedSchemas) {
                updateTablesCombobox(selectedSchemas);
            }
        }, metaDataSource, dataModel);
        jPanel1.add(searchButton, gridBagConstraints);
        
        tablesComboBox.setVisible(false);
        refreshButton1.setVisible(false);
        searchButton.setText("Select Table");
        
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
                if (evt.getButton() == MouseEvent.BUTTON3) {
                    if (mdTable != null) {
                        JPopupMenu popup = new JPopupMenu();
                        JMenuItem open = new JMenuItem("Open");
                        popup.add(open);
                        open.addActionListener(new ActionListener() {
                            @Override
                            public void actionPerformed(ActionEvent e) {
                                openTable(mdTable);
                            }
                        });
                        if (MetaDataPanel.this.metaDataSource.toTable(mdTable) == null) {
                            popup.addSeparator();
                            JMenuItem analyse = new JMenuItem("Analyse schema \""+ mdTable.getSchema().getUnquotedName() + "\"");
                            popup.add(analyse);
                            analyse.addActionListener(new ActionListener() {
                                @Override
                                public void actionPerformed(ActionEvent e) {
                                    analyseSchema(mdTable.getSchema().getName());
                                }
                            });
                        }
                        popup.show(evt.getComponent(), evt.getX(), evt.getY());
                    }
                }
                if (evt.getButton() == MouseEvent.BUTTON1) {
                    if (mdTable != null) {
                        if (evt.getClickCount() > 1) {
                            openTable(mdTable);
                        }
                    }
                }
            }
            private MDTable findTable(MouseEvent evt) {
                MDTable mdTable = null;
                TreePath node = metaDataTree.getPathForLocation(evt.getX(), evt.getY());
                if (node == null) {
                    for (int x = metaDataTree.getWidth(); x > 0; x -= 32) {
                        node = metaDataTree.getPathForLocation(x, evt.getY());
                        if (node != null) {
                            break;
                        }
                    }
                }
                if (node != null) {
                    Object sel = node.getLastPathComponent();
                    if (sel instanceof DefaultMutableTreeNode) {
                        Object selNode = ((DefaultMutableTreeNode) sel).getUserObject();
                        if (selNode instanceof MDTable) {
                            mdTable = (MDTable) selNode;
                            metaDataTree.setSelectionPath(node);
                        }
                    }
                }
                return mdTable;
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
        
        final ImageIcon finalScaledWarnIcon = getScaledIcon(this, warnIcon); 
        
        DefaultTreeCellRenderer renderer = new DefaultTreeCellRenderer() {
            Map<MDTable, Boolean> dirtyTables = new HashMap<MDTable, Boolean>();
            @Override
            public Component getTreeCellRendererComponent(JTree tree, Object value, boolean sel, boolean expanded,
                    boolean leaf, int row, boolean hasFocus) {
                boolean unknownTable = false;
                boolean isJailerTable = false;
                boolean isView = false;
                boolean isSynonym = false;
                Boolean isDirty = false;
                Icon image = null;
                if (value instanceof DefaultMutableTreeNode) {
                    Object uo = ((DefaultMutableTreeNode) value).getUserObject();
                    if (uo instanceof JLabel) {
                    	image = ((JLabel) uo).getIcon();
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
                                isDirty = dirtyTables.get((MDTable) uo);
                                if (isDirty == null) {
                                    isDirty = !((MDTable) uo).isUptodate(table);
                                    dirtyTables.put(((MDTable) uo), isDirty);
                                }
                            }
                        }
                        if (ModelBuilder.isJailerTable(((MDTable) uo).getUnquotedName())) {
                            isJailerTable = true;
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
                    }
                }
                Component comp = super.getTreeCellRendererComponent(tree, value + (unknownTable? "" : (isDirty? " !" : "  ")), sel, expanded, leaf, row, hasFocus);
                String tooltip = null;
            	if (comp instanceof JLabel) {
            		String text = ((JLabel) comp).getText();
            		int maxLength = 40;
            		if (text != null && text.length() > maxLength) {
            			tooltip = text;
            			((JLabel) comp).setText(text.substring(0, maxLength) + "...");
            		}
            	}	
                Font font = comp.getFont();
                if (font != null) {
                    Font bold = new Font(font.getName(), unknownTable || isDirty? (font.getStyle() | Font.ITALIC) : (font.getStyle() & ~Font.ITALIC), font.getSize());
                    comp.setFont(bold);
                }
                if (isJailerTable) {
                    comp.setEnabled(false);
                }
                if (image != null) {
                    JPanel panel = new JPanel(new FlowLayout(0, 0, 0));
                    JLabel label = new JLabel(image);
                    label.setText(" ");
                    label.setOpaque(false);
                    panel.add(label);
                    panel.setOpaque(false);
                    panel.add(comp);
                    comp = panel;
                }
                JPanel panel = new JPanel(new FlowLayout(0, 0, 0));
                panel.add(comp);
                JLabel label = new JLabel("");
                if (unknownTable && !isJailerTable) {
                    label.setIcon(finalScaledWarnIcon);
                }
                label.setOpaque(false);
                panel.add(label);
                panel.setOpaque(false);
                panel.setToolTipText(tooltip);
                comp = panel;
                return comp;
            }
        };
        renderer.setOpenIcon(null);
        renderer.setLeafIcon(null);
        renderer.setClosedIcon(null);
        metaDataTree.setCellRenderer(renderer);
        metaDataTree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        
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
                        SwingUtilities.invokeLater(new Runnable() {
                            @Override
                            public void run() {
                                SwingUtilities.invokeLater(new Runnable() {
                                    @Override
                                    public void run() {
                                        SwingUtilities.invokeLater(new Runnable() {
                                            @Override
                                            public void run() {
                                            	setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
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
                                        	    	setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
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
            Font bold = new Font(font.getName(), font.getStyle() | Font.BOLD, font.getSize());
            outlineLabel.setFont(bold);
        }
    }
    
    private Map<String, MDTable> tablesComboboxMDTablePerName = new HashMap<String, MDTable>();
    
    private void updateTablesCombobox(Set<MDSchema> selectedSchemas) {
        Set<String> tableSet = new HashSet<String>();
        
        for (Table table: dataModel.getTables()) {
            if (metaDataSource.toMDTable(table) == null) {
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
            if (schema.isLoaded()) {
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
    }

    protected void openTable(MDTable mdTable) {
        Table table = metaDataSource.toTable(mdTable);
        if (table != null) {
            open(table);
        } else {
            open(mdTable);
        }
    }

    public void reset() {
        refreshButton.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        JDBCMetaDataBasedModelElementFinder.resetCaches(metaDataSource.getSession());
        setOutline(new ArrayList<OutlineInfo>(), -1);
        proceduresPerSchema.clear();
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                try {
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
                    metaDataSource.clear();
                    metaDataDetailsPanel.reset();
                    updateTreeModel(metaDataSource);
                    if (selectedTable != null) {
                        MDSchema schema = metaDataSource.find(selectedTable.getSchema().getName());
                        if (schema != null) {
                            MDTable table = schema.find(selectedTable.getName());
                            if (table != null) {
                                select(table);
                            }
                        }
                    }
                } finally {
                    refreshButton.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
                }
            }
        });
    }

    public void select(Table table) {
        select(metaDataSource.toMDTable(table));
    }
    
    public void select(MDTable mdTable) {
        if (mdTable != null) {
            TreePath path = find(metaDataTree.getModel().getRoot(), mdTable);
            if (path != null) {
                selectSchema(mdTable.getSchema(), false);
                metaDataTree.expandPath(path);
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
            metaDataTree.scrollRectToVisible(new Rectangle(bounds.x, bounds.y, 1, bounds.height));
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
                if (userObject == mdTable) {
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
		            for (MDTable table: schema.getTables()) {
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
        selectSchema(metaDataSource.getDefaultSchema());
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
						        SwingUtilities.invokeLater(new Runnable() {
						            @Override
						            public void run() {
								        SwingUtilities.invokeLater(new Runnable() {
								            @Override
								            public void run() {
								                try {
								                    setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
								                    expandImmediatelly();
								                } finally {
								                    setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
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
            	TreePath path = new TreePath(node.getPath());
                metaDataTree.expandPath(path);
                metaDataTree.getSelectionModel().setSelectionPath(path);
                if (scrollToNode) {
                    scrollToNode(path);
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

        splitPane = new javax.swing.JSplitPane();
        jPanel1 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        metaDataTree = new javax.swing.JTree();
        refreshButton = new javax.swing.JButton();
        refreshButton1 = new javax.swing.JButton();
        jPanel2 = new javax.swing.JPanel();
        outlineScrollPane = new javax.swing.JScrollPane();
        outlineList = new javax.swing.JList();
        outlineLabel = new javax.swing.JLabel();
        placeholderPanel = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();

        setLayout(new java.awt.GridBagLayout());

        splitPane.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        splitPane.setResizeWeight(1.0);
        splitPane.setOneTouchExpandable(true);

        jPanel1.setLayout(new java.awt.GridBagLayout());

        jScrollPane1.setViewportView(metaDataTree);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel1.add(jScrollPane1, gridBagConstraints);

        refreshButton.setText("Refresh");
        refreshButton.setToolTipText("Refresh Database Meta Data Cache");
        refreshButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                refreshButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 5;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        jPanel1.add(refreshButton, gridBagConstraints);

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
    		SwingUtilities.invokeLater(new Runnable() {
	            @Override
	            public void run() {
	                TreePath path = metaDataTree.getSelectionPath();
	                if (path != null) {
	                    Rectangle bounds = metaDataTree.getPathBounds(path);
	                    metaDataTree.scrollRectToVisible(new Rectangle(bounds.x, bounds.y, 1, bounds.height));
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
    		outlineScrollPane.setVisible(false);
    		placeholderPanel.setVisible(false);
    		lastDividerLocation = splitPane.getDividerLocation();
    		splitPane.setDividerLocation(splitPane.getHeight() - 32);
    	}
    }

    private String outlineTableRender(OutlineInfo info, boolean selected) {
        String render = "<font color=\"#000000\">";
        if (info.mdTable != null || info.isCTE) {
        	if (info.isCTE) {
        		render = info.scopeDescriptor;
        	} else {
	            if (info.mdTable.getSchema().isDefaultSchema) {
	                render += info.mdTable.getName();
	            } else {
	                render += info.mdTable.getSchema().getName() + "." + info.mdTable.getName();
	            }
        	}
            String alias = info.alias;
            if (alias != null) {
            	if (!render.isEmpty()) {
            		render += " ";
            	}
                render += "<font color=\"#0000dd\"><b>as</b></font> " + alias;
            }
            render += "</font>";
        } else if (info.scopeDescriptor != null) {
            render = "<font color=\"#0000dd\"><b>" + info.scopeDescriptor + "</b>" + (info.rowCount > 1? " <i>(" + info.rowCount + " rows)</i>" : "") + "</font>";
        }
        
        String indent = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;";
        if (info.isCTE && "".equals(info.alias)) {
        	render = "&nbsp;&nbsp;&nbsp;" + render;
        } else if (info.scopeDescriptor == null || (info.isCTE && !"".equals(info.scopeDescriptor))) {
        	render = indent + render;
        }
        for (int i = 0; i < info.level; ++i) {
        	render = indent + render;
        }
        if (info.context != null) {
        	render += "<font color=\"#dd9999\">&nbsp;&nbsp;" + info.context + "</font>";
        }
        render = "<html>" + render;
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
        public final int position;
        public final String scopeDescriptor;
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
        
        public OutlineInfo(MDTable mdTable, String alias, int level, int position, String scopeDescriptor) {
            this.mdTable = mdTable;
            this.alias = alias;
            this.level = level;
            this.position = position;
            this.scopeDescriptor = scopeDescriptor;
        }
    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTree metaDataTree;
    private javax.swing.JLabel outlineLabel;
    private javax.swing.JList outlineList;
    private javax.swing.JScrollPane outlineScrollPane;
    private javax.swing.JPanel placeholderPanel;
    private javax.swing.JButton refreshButton;
    private javax.swing.JButton refreshButton1;
    private javax.swing.JSplitPane splitPane;
    // End of variables declaration//GEN-END:variables

    protected abstract void open(Table table);
    protected abstract void open(MDTable mdTable);
    protected abstract void analyseSchema(String schemaName);
    protected abstract void onTableSelect(MDTable mdTable);
    protected abstract void onSchemaSelect(MDSchema mdSchema);
	protected abstract void onMDOtherSelect(MDGeneric mdOther, ExecutionContext executionContext);
    protected abstract void openNewTableBrowser();
    protected abstract void updateDataModelView(Table table);
    protected abstract void setCaretPosition(int position);

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

    static ImageIcon warnIcon;
    static ImageIcon viewIcon;
    static ImageIcon viewsIcon;
    static ImageIcon tableIcon;
    static ImageIcon tablesIcon;
    static ImageIcon synonymIcon;
    static ImageIcon synonymsIcon;
    static ImageIcon databaseIcon;
    static ImageIcon schemaIcon;
    
    static ImageIcon getScaledIcon(JComponent component, ImageIcon icon) {
        if (icon != null) {
            ImageIcon scaledIcon = icon;
            if (scaledIcon != null) {
                int heigth = component.getFontMetrics(new JLabel("M").getFont()).getHeight();
                double s = heigth / (double) scaledIcon.getIconHeight();
                if (icon == viewIcon) {
                    s *= 0.8;
                }
                try {
                    return new ImageIcon(scaledIcon.getImage().getScaledInstance((int)(scaledIcon.getIconWidth() * s), (int)(scaledIcon.getIconHeight() * s), Image.SCALE_SMOOTH));
                } catch (Exception e) {
                	logger.info("error", e);
                    return null;
                }
            }
        }
        return null;
    }
    static {
        String dir = "/net/sf/jailer/ui/resource";
        
        // load images
        try {
            warnIcon = new ImageIcon(MetaDataPanel.class.getResource(dir + "/wanr.png"));
            viewIcon = new ImageIcon(MetaDataPanel.class.getResource(dir + "/view.png"));
            synonymIcon = new ImageIcon(MetaDataPanel.class.getResource(dir + "/synonym.png"));
            synonymsIcon = new ImageIcon(MetaDataPanel.class.getResource(dir + "/synonyms.png"));
            viewsIcon = new ImageIcon(MetaDataPanel.class.getResource(dir + "/views.png"));
            tablesIcon = new ImageIcon(MetaDataPanel.class.getResource(dir + "/tables.png"));
            tableIcon = new ImageIcon(MetaDataPanel.class.getResource(dir + "/table.png"));
            databaseIcon = new ImageIcon(MetaDataPanel.class.getResource(dir + "/database.png"));
            schemaIcon = new ImageIcon(MetaDataPanel.class.getResource(dir + "/schema.png"));
        } catch (Exception e) {
        	logger.info("error", e);
            e.printStackTrace();
        }
    }

}
