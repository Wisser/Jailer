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
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
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

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;

/**
 * Meta Data UI.
 *
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public abstract class MetaDataPanel extends javax.swing.JPanel {

	private final MetaDataSource metaDataSource;
	
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
							JMenuItem analyse = new JMenuItem("Analyse schema \""+ mdTable.getSchema().getName() + "\"");
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
		            if (evt.getClickCount() > 1) {
		                if (mdTable != null) {
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
        
        final ImageIcon finalScaledWarnIcon = getWarnIcon(this); 
        
        DefaultTreeCellRenderer renderer = new DefaultTreeCellRenderer() {
			@Override
			public Component getTreeCellRendererComponent(JTree tree, Object value, boolean sel, boolean expanded,
					boolean leaf, int row, boolean hasFocus) {
				Component comp = super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);
				boolean unknownTable = false;
				if (value instanceof DefaultMutableTreeNode) {
					Object uo = ((DefaultMutableTreeNode) value).getUserObject();
					if (uo instanceof MDTable) {
						if (MetaDataPanel.this.metaDataSource.toTable((MDTable) uo) == null) {
							unknownTable = true;
						}
					}
				}
				Font font = comp.getFont();
				if (font != null) {
					Font bold = new Font(font.getName(), unknownTable? (font.getStyle() | Font.ITALIC) : (font.getStyle() & ~Font.ITALIC), font.getSize());
					comp.setFont(bold);
				}
				if (unknownTable) {
					JPanel panel = new JPanel(new FlowLayout(0, 0, 0));
					panel.add(comp);
					JLabel label = new JLabel(finalScaledWarnIcon);
					label.setOpaque(false);
					panel.add(label);
					panel.setOpaque(false);
					return panel;
				}
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
						 openNewTableBrowser();
					}
					if (last instanceof DefaultMutableTreeNode) {
						final Object uo = ((DefaultMutableTreeNode) last).getUserObject();
						if (uo instanceof MDTable) {
							SwingUtilities.invokeLater(new Runnable() {
								@Override
								public void run() {
									if (metaDataTree.getSelectionPath() != null && metaDataTree.getSelectionPath().getLastPathComponent() == last) {
										if (uo instanceof MDSchema) {
											onSchemaSelect((MDSchema) uo);
										} else if (uo instanceof MDTable) {
											onTableSelect((MDTable) uo);
										}
									}
								}
							});
						}
					}
				}
			}
		});
        
        updateTreeModel(metaDataSource);
    }
    
	protected void openTable(MDTable mdTable) {
		Table table = metaDataSource.toTable(mdTable);
		if (table != null) {
			open(table);
		} else {
			open(mdTable);
		}
	}

	private void reset() {
		metaDataSource.clear();
    	updateTreeModel(metaDataSource);
	}

	public void select(Table table) {
		MDTable mdTable = metaDataSource.toMDTable(table);
		if (mdTable != null) {
			TreePath path = find(metaDataTree.getModel().getRoot(), mdTable);
			if (path != null) {
				metaDataTree.expandPath(path);
				metaDataTree.getSelectionModel().setSelectionPath(path);
				scrollToNode(path);
			}
		}
	}
	
	public void select(MDTable mdTable) {
		if (mdTable != null) {
			TreePath path = find(metaDataTree.getModel().getRoot(), mdTable);
			if (path != null) {
				metaDataTree.expandPath(path);
				metaDataTree.getSelectionModel().setSelectionPath(path);
				scrollToNode(path);
			}
		}
	}

	private void scrollToNode(TreePath path) {
		Rectangle bounds = metaDataTree.getPathBounds(path);
		metaDataTree.scrollRectToVisible(new Rectangle(bounds.x, bounds.y, 1, bounds.height));
	}


	private TreePath find(Object root, MDTable mdTable) {
		if (root instanceof DefaultMutableTreeNode) {
			Object userObject = ((DefaultMutableTreeNode) root).getUserObject();
			if (userObject instanceof MDSchema) {
				if (mdTable.getSchema().equals(userObject)) {
					if (((DefaultMutableTreeNode) root).getChildCount() > 0) {
						TreeNode firstChild = ((DefaultMutableTreeNode) root).getFirstChild();
						if (firstChild instanceof ExpandingMutableTreeNode) {
							((ExpandingMutableTreeNode) firstChild).expandImmediatelly();
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
				private boolean expanded = false;
				@Override
				protected void expandImmediatelly() {
					if (!expanded) {
						for (MDTable table: schema.getTables()) {
							DefaultMutableTreeNode tableChild = new DefaultMutableTreeNode(table);
							schemaChild.add(tableChild);
						}
						schemaChild.remove(this);
						TreeModel model = metaDataTree.getModel();
						((DefaultTreeModel) model).nodeStructureChanged(schemaChild);
					}
					expanded = true;
				}
				@Override
				protected void expand() {
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
    protected abstract void open(MDTable mdTable);
    protected abstract void analyseSchema(String schemaName);
    protected abstract void onTableSelect(MDTable mdTable);
    protected abstract void onSchemaSelect(MDSchema mdSchema);
	protected abstract void openNewTableBrowser();
	
    static private ImageIcon warnIcon;
    static ImageIcon getWarnIcon(JComponent component) {
    	if (warnIcon != null) {
            ImageIcon scaledWarnIcon = warnIcon;
            if (scaledWarnIcon != null) {
            	int heigth = component.getFontMetrics(new JLabel("M").getFont()).getHeight();
            	double s = heigth / (double) scaledWarnIcon.getIconHeight();
            	try {
            		return new ImageIcon(scaledWarnIcon.getImage().getScaledInstance((int)(scaledWarnIcon.getIconWidth() * s), (int)(scaledWarnIcon.getIconHeight() * s), Image.SCALE_SMOOTH));
            	} catch (Exception e) {
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
			
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
}
