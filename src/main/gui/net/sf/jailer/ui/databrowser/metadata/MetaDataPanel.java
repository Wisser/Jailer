/*
 * Copyright 2007 - 2017 the original author or authors.
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

import java.awt.Component;
import java.awt.Cursor;
import java.awt.Font;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.event.TreeExpansionEvent;
import javax.swing.event.TreeWillExpandListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.ExpandVetoException;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.util.Quoting;

/**
 * Meta Data UI.
 *
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public abstract class MetaDataPanel extends javax.swing.JPanel {

	private final MetaDataSource metaDataSource;
	private final Map<String, Table> tablePerUnquotedName = new HashMap<String, Table>();
	private final Map<String, Table> tablePerUnquotedNameUC = new HashMap<String, Table>();
	private final Map<MDTable, Table> mDTableToTable = new HashMap<MDTable, Table>();
	private final Map<Table, MDTable> tableToMDTable = new HashMap<Table, MDTable>();
	
	private abstract class ExpandingMutableTreeNode extends DefaultMutableTreeNode {
		
		public ExpandingMutableTreeNode() {
			super("loading...");
		}
		
		protected abstract void expand();
	}
	
    /**
     * Creates new form MetaDataPanel
     * 
     * @param metaDataSource the meta data source
     * @param dataModel the data mmodel
     */
    public MetaDataPanel(MetaDataSource metaDataSource, DataModel dataModel, ExecutionContext executionContext) {
    	this.metaDataSource = metaDataSource;
        initComponents();
        
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
				if (evt.getButton() == MouseEvent.BUTTON1) {
		            if (evt.getClickCount() > 1) {
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
		                        	open((MDTable) selNode);
		                        }
		                    }
		                }
		            }
				}
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
        
        DefaultTreeCellRenderer renderer = new DefaultTreeCellRenderer() {
			@Override
			public Component getTreeCellRendererComponent(JTree tree, Object value, boolean sel, boolean expanded,
					boolean leaf, int row, boolean hasFocus) {
				Component comp = super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);
				boolean italic = false;
				if (value instanceof DefaultMutableTreeNode) {
					Object uo = ((DefaultMutableTreeNode) value).getUserObject();
					if (uo instanceof MDTable) {
						if (toTable((MDTable) uo) == null) {
							italic = true;
						}
					}
				}
				Font font = comp.getFont();
				if (font != null) {
					Font bold = new Font(font.getName(), italic? (font.getStyle() | Font.ITALIC) : (font.getStyle() & ~Font.ITALIC), font.getSize());
					comp.setFont(bold);
				}
				return comp;
			}
        };
        renderer.setOpenIcon(null);
        renderer.setLeafIcon(null);
        renderer.setClosedIcon(null);
        metaDataTree.setCellRenderer(renderer);
        metaDataTree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        
        for (Table table: dataModel.getTables()) {
        	tablePerUnquotedName.put(Quoting.unquotedTableName(table, executionContext), table);
        	tablePerUnquotedNameUC.put(Quoting.unquotedTableName(table, executionContext).toUpperCase(Locale.ENGLISH), table);
        }

        updateTreeModel(metaDataSource);
    }
    
	protected void open(MDTable mdTable) {
		Table table = toTable(mdTable);
		if (table != null) {
			open(table);
		} else {
			open(mdTable.getSchema().isDefaultSchema? null : mdTable.getSchema().getName(), mdTable.getName());
		}
	}

	private void reset() {
		metaDataSource.clear();
    	updateTreeModel(metaDataSource);
    	mDTableToTable.clear();
	}

	public void select(Table table) {
		MDTable mdTable = tableToMDTable.get(table);
		if (mdTable != null) {
			TreePath path = find(metaDataTree.getModel().getRoot(), mdTable);
			if (path != null) {
				metaDataTree.expandPath(path);
				metaDataTree.getSelectionModel().setSelectionPath(path);
			}
		}
	}
	
	private TreePath find(Object root, MDTable mdTable) {
		if (root instanceof DefaultMutableTreeNode) {
			Object userObject = ((DefaultMutableTreeNode) root).getUserObject();
            if (userObject instanceof MDTable) {
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
	
    private Table toTable(MDTable mdTable) {
    	if (mDTableToTable.containsKey(mdTable)) {
    		return mDTableToTable.get(mdTable);
    	}
    	Table table = null;
    	if (mdTable.getSchema().isDefaultSchema) {
    		table = tablePerUnquotedName.get(mdTable.getName());
    	}
    	if (table == null) {
    		table = tablePerUnquotedName.get(mdTable.getSchema().getName() + "." + mdTable.getName());
    	}
    	if (table == null) {
        	if (mdTable.getSchema().isDefaultSchema) {
        		table = tablePerUnquotedNameUC.get(mdTable.getName().toUpperCase(Locale.ENGLISH));
        	}
        	if (table == null) {
        		table = tablePerUnquotedNameUC.get((mdTable.getSchema().getName() + "." + mdTable.getName()).toUpperCase(Locale.ENGLISH));
        	}
    	}
    	mDTableToTable.put(mdTable, table);
    	tableToMDTable.put(table, mdTable);
    	return table;
    }

	private void updateTreeModel(MetaDataSource metaDataSource) {
		DefaultMutableTreeNode root = new DefaultMutableTreeNode(metaDataSource.dataSourceName);
		DefaultMutableTreeNode defaultSchema = null;

        for (final MDSchema schema: metaDataSource.getSchemas()) {
        	final DefaultMutableTreeNode schemaChild = new DefaultMutableTreeNode(schema);
			root.add(schemaChild);
			if (schema.isDefaultSchema) {
				defaultSchema = schemaChild;
			}
			MutableTreeNode expandSchema = new ExpandingMutableTreeNode() {
				@Override
				protected void expand() {
					final ExpandingMutableTreeNode expandingMutableTreeNodeThis = this;
					SwingUtilities.invokeLater(new Runnable() {
						@Override
						public void run() {
							try {
								setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
								for (MDTable table: schema.getTables()) {
									DefaultMutableTreeNode tableChild = new DefaultMutableTreeNode(table);
									schemaChild.add(tableChild);
								}
							} catch (SQLException e) {
								UIUtil.showException(null, "Error", e);
				            } finally {
				                setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
				            }
							schemaChild.remove(expandingMutableTreeNodeThis);
							TreeModel model = metaDataTree.getModel();
							((DefaultTreeModel) model).nodeStructureChanged(schemaChild);
						}
					});
				}
			};
			schemaChild.add(expandSchema);
        }
        DefaultTreeModel treeModel = new DefaultTreeModel(root);
        metaDataTree.setModel(treeModel);
        if (defaultSchema != null) {
        	TreePath path = new TreePath(new Object[] { root, defaultSchema });
			metaDataTree.expandPath(path);
	        metaDataTree.getSelectionModel().setSelectionPath(path);
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

        jScrollPane1 = new javax.swing.JScrollPane();
        metaDataTree = new javax.swing.JTree();
        jLabel2 = new javax.swing.JLabel();
        refreshButton = new javax.swing.JButton();

        setLayout(new java.awt.GridBagLayout());

        jScrollPane1.setViewportView(metaDataTree);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(jScrollPane1, gridBagConstraints);

        jLabel2.setText(" Tables");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTH;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        add(jLabel2, gridBagConstraints);

        refreshButton.setText("Refresh");
        refreshButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                refreshButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        add(refreshButton, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    private void refreshButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_refreshButtonActionPerformed
    	reset();
    }//GEN-LAST:event_refreshButtonActionPerformed

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JLabel jLabel2;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTree metaDataTree;
    private javax.swing.JButton refreshButton;
    // End of variables declaration//GEN-END:variables

    protected abstract void open(Table table);
    protected abstract void open(String schemaName, String tableName);
    protected abstract void analyseSchema(String schemaName);

}
