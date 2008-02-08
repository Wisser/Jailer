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
package net.sf.jailer.ui;

import java.awt.Color;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JTree;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreeCellRenderer;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.ModelElement;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.extractionmodel.ExtractionModel;
import net.sf.jailer.restrictionmodel.RestrictionModel;
import net.sf.jailer.util.SqlUtil;

/**
 * Editor for {@link RestrictionModel}s.
 * 
 * @author Wisser
 */
public class ExtractionModelEditor extends javax.swing.JPanel {

	/**
	 * Editor for single restriction.
	 */
	private RestrictionEditor restrictionEditor;
	
	/**
	 * Holds restrictions.
	 */
	private ExtractionModel extractionModel;
	
	/**
	 * The data model.
	 */
	DataModel dataModel;
	
	/**
	 * Subject table.
	 */
	private Table subject;
	
	/**
	 * Root of 'associations'-tree.
	 */
	private Table root;
	
	private Map<Association, Association> representant;
	private Map<ModelElement, DefaultMutableTreeNode> toNode;
	private DefaultTreeModel treeModel;
	private Collection<DefaultMutableTreeNode> treeNodes;
	private boolean suppressRestrictionSelection = false;
	boolean needsSave = false;
	ExtractionModelFrame extractionModelFrame;
	String extractionModelFile;
	
	/** 
	 * Creates new form ModelTree.
	 *  
	 * @param extractionModelFile file containing the model
     */
	public ExtractionModelEditor(String extractionModelFile, ExtractionModelFrame extractionModelFrame) {
		this.extractionModelFrame = extractionModelFrame;
		this.extractionModelFile = extractionModelFile;
		try {
			dataModel = new DataModel();
		} catch (Exception e) {
			UIUtil.showException(this, "Error in Data Model", e);
			return;
		}
		try {
			if (extractionModelFile == null || !new File(extractionModelFile).exists()) {
				needsSave = extractionModelFile != null;
				dataModel = new DataModel();
				extractionModel = new ExtractionModel(dataModel);
			} else {
				extractionModel = new ExtractionModel(extractionModelFile);
			}
			subject = extractionModel.getTasks().get(0).subject;
			dataModel = extractionModel.getTasks().get(0).dataModel;
		} catch (Exception e) {
			UIUtil.showException(this, extractionModelFile == null? "Error creating new Model" : "Error in " + new File(extractionModelFile).getName(), e);
			return;
		}

		boolean saveNeedsSave = needsSave;
		initComponents();
		tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
		tree.setExpandsSelectedPaths(true);
		restrictionEditor = new RestrictionEditor();
		restrictionsTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		restrictionsTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent e) {
				if (restrictionsTable.getSelectedRow() >= 0) {
					select(currentRestrictionDefinitions.get(restrictionsTable.getSelectedRow()));
				}
			}
		});
		inspectorHolder.add(restrictionEditor);
		inspectorHolder.setMinimumSize(inspectorHolder.getPreferredSize());
		restrictionEditor.apply.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				onApply(true);
			}
		});
		restrictionEditor.ignore.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				onApply(false);
			}
		});
		restrictionEditor.jump.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				onJump();
			}
		});
		initRestrictionEditor(null, null);
		if (extractionModel.getTasks().get(0).subject != null) {
			subjectTable.setSelectedItem(extractionModel.getTasks().get(0).subject.getName());
		}
		condition.setText(extractionModel.getTasks().get(0).condition);
		
		condition.addKeyListener(new KeyListener() {
			public void keyPressed(KeyEvent arg0) {
				needsSave = true;
				ExtractionModelEditor.this.extractionModelFrame.updateTitle(needsSave);
			}
			public void keyReleased(KeyEvent arg0) {
			}
			public void keyTyped(KeyEvent arg0) {
			}
		});
		
		exportButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				ExtractionModelEditor.this.extractionModelFrame.dataExportActionPerformed(e);
			}
    	});
		
		needsSave = saveNeedsSave;
		extractionModelFrame.updateTitle(needsSave);
	}

	/**
	 * This method is called from within the constructor to initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is always
	 * regenerated by the Form Editor.
	 */
	// <editor-fold defaultstate="collapsed" desc=" Erzeugter Quelltext
    // <editor-fold defaultstate="collapsed" desc=" Erzeugter Quelltext ">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        legende = new javax.swing.JPanel();
        dependsOn = new javax.swing.JLabel();
        hasDependent = new javax.swing.JLabel();
        associatedWith = new javax.swing.JLabel();
        ignored = new javax.swing.JLabel();
        inspectorHolder = new javax.swing.JPanel();
        jPanel2 = new javax.swing.JPanel();
        jPanel3 = new javax.swing.JPanel();
        jLabel6 = new javax.swing.JLabel();
        subjectTable = new javax.swing.JComboBox();
        jLabel7 = new javax.swing.JLabel();
        jScrollPane4 = new javax.swing.JScrollPane();
        restrictionsTable = new javax.swing.JTable();
        jLabel8 = new javax.swing.JLabel();
        condition = new javax.swing.JTextField();
        exportButton = new javax.swing.JButton();
        jPanel4 = new javax.swing.JPanel();
        rootTable = new javax.swing.JComboBox();
        jScrollPane1 = new javax.swing.JScrollPane();
        tree = new javax.swing.JTree();

        setLayout(new java.awt.GridBagLayout());

        legende.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 0, 0));

        dependsOn.setFont(new java.awt.Font("Dialog", 0, 12));
        dependsOn.setForeground(new java.awt.Color(153, 0, 0));
        dependsOn.setText("depends on");
        legende.add(dependsOn);

        hasDependent.setFont(new java.awt.Font("Dialog", 0, 12));
        hasDependent.setForeground(new java.awt.Color(0, 0, 204));
        hasDependent.setText("   has dependent");
        legende.add(hasDependent);

        associatedWith.setFont(new java.awt.Font("Dialog", 0, 12));
        associatedWith.setForeground(new java.awt.Color(0, 153, 51));
        associatedWith.setText("   associated with");
        legende.add(associatedWith);

        ignored.setFont(new java.awt.Font("Dialog", 0, 12));
        ignored.setForeground(new java.awt.Color(153, 153, 153));
        ignored.setText("   ignored");
        legende.add(ignored);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 6);
        add(legende, gridBagConstraints);

        inspectorHolder.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 0, 0));

        inspectorHolder.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "Restriction on association", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Dialog", 2, 12)));
        inspectorHolder.setMinimumSize(new java.awt.Dimension(100, 400));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        add(inspectorHolder, gridBagConstraints);

        jPanel2.setLayout(new java.awt.GridLayout(1, 4));

        jPanel3.setLayout(new java.awt.GridBagLayout());

        jPanel3.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "Subject", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Dialog", 2, 12)));
        jPanel3.setMaximumSize(new java.awt.Dimension(2147483647, 2147483647));
        jPanel3.setMinimumSize(new java.awt.Dimension(158, 122));
        jLabel6.setText("Export from ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel3.add(jLabel6, gridBagConstraints);

        subjectTable.setModel(subjectListModel());
        subjectTable.setMaximumRowCount(32);
        subjectTable.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                onNewSubject(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        jPanel3.add(subjectTable, gridBagConstraints);

        jLabel7.setText("Where");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        jPanel3.add(jLabel7, gridBagConstraints);

        jScrollPane4.setBorder(null);
        restrictionsTable.setModel(restrictionTableModel());
        jScrollPane4.setViewportView(restrictionsTable);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel3.add(jScrollPane4, gridBagConstraints);

        jLabel8.setFont(new java.awt.Font("Dialog", 2, 14));
        jLabel8.setText("Restrictions");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 4, 0);
        jPanel3.add(jLabel8, gridBagConstraints);

        condition.setText("jTextField1");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        jPanel3.add(condition, gridBagConstraints);

        exportButton.setText("Export Data");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 2);
        jPanel3.add(exportButton, gridBagConstraints);

        jPanel2.add(jPanel3);

        jPanel4.setLayout(new java.awt.BorderLayout(0, 4));

        jPanel4.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "Associations", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Dialog", 2, 12)));
        rootTable.setModel(getTableListModel());
        rootTable.setMaximumRowCount(32);
        rootTable.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                rootTableItemStateChanged(evt);
            }
        });

        jPanel4.add(rootTable, java.awt.BorderLayout.NORTH);

        jScrollPane1.setAutoscrolls(true);
        jScrollPane1.setBorder(null);
        tree.setAutoscrolls(true);
        tree.setCellRenderer(getTreeCellRenderer(tree.getCellRenderer()));
        tree.setModel(getModel());
        tree.addTreeSelectionListener(new javax.swing.event.TreeSelectionListener() {
            public void valueChanged(javax.swing.event.TreeSelectionEvent evt) {
                treeValueChanged(evt);
            }
        });

        jScrollPane1.setViewportView(tree);

        jPanel4.add(jScrollPane1, java.awt.BorderLayout.CENTER);

        jPanel2.add(jPanel4);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(jPanel2, gridBagConstraints);

    }// </editor-fold>//GEN-END:initComponents

    private void onNewSubject(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_onNewSubject
    	Object selectedItem = subjectTable.getSelectedItem();
		if (selectedItem instanceof String) {
			if (dataModel.getTable(selectedItem.toString()) != null) {
				subject = dataModel.getTable(selectedItem.toString());
			}
		}
    	rootTable.setModel(getTableListModel());
    	rootTable.setSelectedItem(null);
    	rootTable.setSelectedItem(selectedItem);
        if (!needsSave) {
			needsSave = true;
			extractionModelFrame.updateTitle(needsSave);
		}
    }//GEN-LAST:event_onNewSubject

    private void rootTableItemStateChanged(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_rootTableItemStateChanged
    	if (evt.getItem() != null) {
    		Table table = dataModel.getTable(evt.getItem().toString());
    		if (table != null) {
    			root = table;
    			tree.setModel(getModel());
    		}
    	}
    }//GEN-LAST:event_rootTableItemStateChanged

    /**
     * {@link RestrictionDefinition}s currently rendered in restrictions-table.
     */
    private List<RestrictionDefinition> currentRestrictionDefinitions;
    
    /**
     * Gets data model for the restrictions-table.
     * 
     * @return data model for the restrictions-table
     */
    private TableModel restrictionTableModel() {
    	currentRestrictionDefinitions = RestrictionDefinition.fromRestrictionModel(extractionModel);
    	Object[][] data = new Object[currentRestrictionDefinitions.size()][];
    	int i = 0;
    	for (RestrictionDefinition def: currentRestrictionDefinitions) {
    		data[i++] = new Object[] { def.from.getName(), def.to.getName(), def.name == null? "" : def.name, def.condition};
    	}
        return new DefaultTableModel(data, new Object[] { "From", "To", "Name", "Condition" });
    }
    
    private ComboBoxModel subjectListModel() {
    	Vector<String> tableNames = new Vector<String>();
    	for (Table table: dataModel.getTables()) {
    		tableNames.add(table.getName());
    	}
    	Collections.sort(tableNames);
    	DefaultComboBoxModel model = new DefaultComboBoxModel(tableNames);
        return model;
    }
    
    private ComboBoxModel getTableListModel() {
    	Vector<String> tableNames = new Vector<String>();
    	for (Table table: dataModel.getTables()) {
    		tableNames.add(table.getName());
    	}
    	Collections.sort(tableNames);
        if (subject != null) {
	    	tableNames.add(0, subject.getName());
	        tableNames.add(1, "---");
        }
    	DefaultComboBoxModel model = new DefaultComboBoxModel(tableNames);
        return model;
    }
    
    private void treeValueChanged(javax.swing.event.TreeSelectionEvent evt) {//GEN-FIRST:event_treeValueChanged
    	if (evt.getNewLeadSelectionPath() != null) {
    		DefaultMutableTreeNode node = ((DefaultMutableTreeNode) evt.getNewLeadSelectionPath().getLastPathComponent());
			Object selection = node.getUserObject();
	    	if (selection instanceof Association) {
	    		initRestrictionEditor((Association) selection, node);
	    	}
	    	if (selection instanceof Table) {
	    		initRestrictionEditor(null, null);
	    	}
    	} else {
    		initRestrictionEditor(null, null);
    	}
    }//GEN-LAST:event_treeValueChanged

    private Association currentAssociation;
    private DefaultMutableTreeNode currentNode;
    private String initialRestrictionCondition = null;
    
    /**
     * Initializes the restriction editor.
     * 
     * @param association the association on which a restriction can be edited
     * @param node selected tree node
     */
    private void initRestrictionEditor(Association association, DefaultMutableTreeNode node) {
    	currentAssociation = association;
    	currentNode = node;
    	initialRestrictionCondition = null;
		if (association == null) {
			int l = currentRestrictionDefinitions == null? -1 : currentRestrictionDefinitions.size();
			if (l > 0) {
				restrictionsTable.removeRowSelectionInterval(0, l - 1);
			}
			restrictionEditor.apply.setEnabled(false);
			restrictionEditor.cardinality.setText("");
			restrictionEditor.destination.setText("");
			restrictionEditor.ignore.getModel().setPressed(false);
			restrictionEditor.ignore.setEnabled(false);
			restrictionEditor.joinCondition.setText("");
			restrictionEditor.jump.setEnabled(false); 
			restrictionEditor.restriction.setText("");
			restrictionEditor.restriction.setEditable(false);
			restrictionEditor.source.setText("");
			restrictionEditor.type.setText("");
            restrictionEditor.aName.setText("");
		} else {
			if (currentRestrictionDefinitions != null) {
				int row = 0;
				boolean found = false;
				for (RestrictionDefinition rd: currentRestrictionDefinitions) {
					if (rd.from.equals(association.source) && rd.to.equals(association.destination)) {
						if (association.getName() == null || association.getName().equals("") || association.getName().equals(rd.name)) {
							restrictionsTable.addRowSelectionInterval(row, row);
							found = true;
							break;
						}
					}
					++row;
				}
				if (!found && row > 0) {
					restrictionsTable.removeRowSelectionInterval(0, row - 1);
				}
			}
			if (!association.destination.equals(root)) {
				restrictionEditor.jump.setEnabled(true);
			} else {
				restrictionEditor.jump.setEnabled(false);
			}

			String type = "Association ";
			boolean editable = true;
			if (association.isInsertDestinationBeforeSource()) {
				type = "Dependency ";
				editable = false;
			} else if (association.isInsertSourceBeforeDestination()) {
				type = "Inverse dependency ";
			}
            String shortendName = association.getName();
            if (shortendName != null && shortendName.length() > 25) {
            	restrictionEditor.aName.setToolTipText(shortendName);
            	shortendName = shortendName.substring(0, 25) + "...";
            } else {
            	restrictionEditor.aName.setToolTipText(null);
            }
			restrictionEditor.aName.setText(shortendName);
            restrictionEditor.aName.setForeground(restrictionEditor.type.getForeground());
            if (getAmbiguousNonames().contains(association)) {
                restrictionEditor.aName.setForeground(Color.RED);
                restrictionEditor.aName.setText("unnamed and ambiguous");
                editable = false;
            }
			restrictionEditor.restriction.setEditable(true);
			restrictionEditor.source.setText(association.source.getName());
			restrictionEditor.destination.setText(association.destination.getName());
			restrictionEditor.cardinality.setText(association.getCardinality() == null? "unknown" : association.getCardinality().toString());
			restrictionEditor.type.setText(type);
			restrictionEditor.joinCondition.setText(association.getUnrestrictedJoinCondition());
			String restrictionCondition = association.getRestrictionCondition();
			if (restrictionCondition != null && restrictionCondition.startsWith("(") && restrictionCondition.endsWith(")")) {
				restrictionCondition = restrictionCondition.substring(1, restrictionCondition.length() - 1);
			}
			initialRestrictionCondition = association.isIgnored()? null : restrictionCondition;
			restrictionEditor.restriction.setText(restrictionCondition);
			restrictionEditor.ignore.getModel().setSelected(association.isIgnored());
			restrictionEditor.restriction.setEditable(editable && !association.isIgnored());
			restrictionEditor.ignore.setEnabled(editable);
			restrictionEditor.apply.setEnabled(editable);
			String joinCondition = association.getUnrestrictedJoinCondition();
			if (association.reversed) {
				joinCondition = SqlUtil.reversRestrictionCondition(joinCondition);
			}
			boolean singleCondition = true;
			for (Association represented: representant.keySet()) {
				if (association.equals(representant.get(represented))) {
					String jc = represented.getUnrestrictedJoinCondition();
					if (represented.reversed) {
						jc = SqlUtil.reversRestrictionCondition(jc);
					}
					if (singleCondition) {
						joinCondition = "(" + joinCondition + ")";
					}
					singleCondition = false;
					joinCondition += "\nor\n(" + jc + ")";
				}
			}
			restrictionEditor.joinCondition.setText(joinCondition);
			tree.grabFocus();
		}
	}

    /**
     * Jump-button clicked.
     */
	private void onJump() {
    	if (tree.getLeadSelectionPath() != null) {
    		Object selection = ((DefaultMutableTreeNode) tree.getLeadSelectionPath().getLastPathComponent()).getUserObject();
	    	if (selection instanceof Association) {
	    		jumpTo(((Association) selection).destination);
	    	}
	    	if (selection instanceof Table) {
	    		jumpTo(subject);
	    	}
    	}
    }

	/**
	 * Reacts on 'Apply'-button pressed.
	 */
    private void onApply(boolean applyButtonKlicked) {
    	if (currentAssociation != null) {
    		if (!needsSave) {
    			needsSave = true;
    			extractionModelFrame.updateTitle(needsSave);
    		}
    		String condition;
    		if (restrictionEditor.ignore.getModel().isSelected()) {
    			condition = "ignore";
    		} else {
    			if (!applyButtonKlicked) {
    				if (initialRestrictionCondition != null) {
    					condition = initialRestrictionCondition;
    				} else {
    					condition = "";
    				}
    			} else {
    				condition = restrictionEditor.restriction.getText().trim();
    			}
    		}
			dataModel.getRestrictionModel().addRestriction(currentAssociation.source, currentAssociation, condition, "GUI", true);
    		tree.repaint();
			restrictionsTable.setModel(restrictionTableModel());
			String saveInitialRestrictionCondition = initialRestrictionCondition;
			initRestrictionEditor(currentAssociation, currentNode);
			initialRestrictionCondition = saveInitialRestrictionCondition;
    	}
    }
    
    /**
     * Makes a table the root of the associations-tree.
     */
	private void jumpTo(Table table) {
	    rootTable.setSelectedItem(table.getName());
    }

    /**
     * Gets model of the associations-tree.
     */
	private TreeModel getModel() {
		representant = new HashMap<Association, Association>();
		toNode = new HashMap<ModelElement, DefaultMutableTreeNode>();
		treeNodes = new ArrayList<DefaultMutableTreeNode>();

		if (this.root == null) {
			this.root = subject;
		}
		
		DefaultMutableTreeNode root = new DefaultMutableTreeNode(this.root);
		Map<Association, DefaultMutableTreeNode> parent = new HashMap<Association, DefaultMutableTreeNode>();
		List<Association> agenda = new LinkedList<Association>();

		toNode.put(this.root, root);
		if (this.root != null) {
			for (Association a: this.root.associations) {
				agenda.add(a);
				parent.put(a, root);
			}
		}
		while (!agenda.isEmpty()) {
			Association a = agenda.get(0);
			agenda.remove(0);
			if (extractionModelFrame.hideIgnored() && a.isIgnored()) {
				continue;
			}
			if (toNode.get(a) == null) {
				Association rep = null;
				for (ModelElement cand: toNode.keySet()) {
					if (cand instanceof Association) {
						Association candRep = (Association) cand;
						if ((a.getName() == null || candRep.getName() == null)
						 && a.source.equals(candRep.source) && a.destination.equals(candRep.destination)) {
							rep = candRep;
							break;
						}
					}
				}
				if (rep != null) {
					representant.put(a, rep);
				} else {
					DefaultMutableTreeNode node = new DefaultMutableTreeNode(a);
					treeNodes.add(node);
					parent.get(a).add(node);
					sort(parent.get(a));
					toNode.put(a, node);
					for (Association nextA: a.destination.associations) {
						if (!parent.containsKey(nextA)) {
							agenda.add(nextA);
							parent.put(nextA, node);
						}
					}
				}
			}
		}
		if (treeModel != null) {
			treeModel.setRoot(root);
		} else {
			treeModel = new DefaultTreeModel(root);
		}
		return treeModel;
	}

	/**
	 * Sorts children of a tree node.
	 * 
	 * @param node the node
	 */
	private void sort(DefaultMutableTreeNode node) {
		List<DefaultMutableTreeNode> children = new ArrayList<DefaultMutableTreeNode>();
		for (int i = 0; i < node.getChildCount(); ++i) {
			children.add((DefaultMutableTreeNode) node.getChildAt(i));
		}
		Collections.sort(children, new Comparator<DefaultMutableTreeNode>() {
			private int cat(Association a) {
				if (a.isIgnored()) {
					return 4;
				}
				if (a.isInsertDestinationBeforeSource()) {
					return 1;
				}
				if (a.isInsertSourceBeforeDestination()) {
					return 2;
				}
				return 3;
			}
			
			public int compare(DefaultMutableTreeNode o1,
					DefaultMutableTreeNode o2) {
				Association a1 = (Association) o1.getUserObject();
				Association a2 = (Association) o2.getUserObject();
				int cat1 = cat(a1);
				int cat2 = cat(a2);
				if (cat1 == cat2) {
					return a1.destination.getName().compareTo(a2.destination.getName());
				}
				return cat1 - cat2;
			}
		});
		node.removeAllChildren();
		for (MutableTreeNode child: children) {
			node.add(child);
		}
	}

	private TreeCellRenderer getTreeCellRenderer(TreeCellRenderer treeCellRenderer) {
		return new DefaultTreeCellRenderer() {
			public Component getTreeCellRendererComponent(JTree tree,
					Object value, boolean selected, boolean expanded,
					boolean leaf, int row, boolean hasFocus) {
				String text = "";
				if (value instanceof DefaultMutableTreeNode) {
					DefaultMutableTreeNode node = (DefaultMutableTreeNode) value;
					if (node.getUserObject() instanceof Table) {
						text = ((Table) node.getUserObject()).getName();
						setTextSelectionColor(Color.BLACK);
						setTextNonSelectionColor(Color.BLACK);
					} else if (node.getUserObject() instanceof Association) {
						Association association = (Association) node.getUserObject();
						text = association.destination.getName();
						if (!association.isIgnored() && association.isRestricted()) {
							text += "*";
						} else {
							text += "  ";
						}
						if (association.isIgnored()) {
							setTextSelectionColor(ignored.getForeground());
							setTextNonSelectionColor(ignored.getForeground());
						} else if (association.isInsertDestinationBeforeSource()) {
							setTextSelectionColor(dependsOn.getForeground());
							setTextNonSelectionColor(dependsOn.getForeground());
						} else if (association.isInsertSourceBeforeDestination()) {
							setTextSelectionColor(hasDependent.getForeground());
							setTextNonSelectionColor(hasDependent.getForeground());
						} else {
							setTextSelectionColor(associatedWith.getForeground());
							setTextNonSelectionColor(associatedWith.getForeground());
						}
					}
				}
				return super.getTreeCellRendererComponent(tree, text, selected, expanded, leaf, row, hasFocus);
			}
		};
	}

	/**
     * Set of all association which are not editable due to ambiguity.
     */
    private Set<Association> ambiguousNonames = null;

    /**
     * Selects a restriction.
     * 
     * @param restrictionDefinition the restriction to select
     */
	private void select(RestrictionDefinition restrictionDefinition) {
		if (extractionModelFrame.hideIgnored() && restrictionDefinition.isIgnored) {
			extractionModelFrame.setHideIgnored(false);
		}
		if (!suppressRestrictionSelection) {
			suppressRestrictionSelection = true;
			try {
				DefaultMutableTreeNode toSelect = null;
				for (int i = 0; i < 2; ++i) {
					for (DefaultMutableTreeNode node: treeNodes) {
						if (node.getUserObject() instanceof Association) {
							Association a = (Association) node.getUserObject();
							if (a.destination.equals(restrictionDefinition.to) && a.source.equals(restrictionDefinition.from)) {
								if (restrictionDefinition.name == null || restrictionDefinition.name.length() == 0 || restrictionDefinition.name.equals(a.getName())) {
									if (toSelect == null || toSelect.isLeaf()) {
										toSelect = node;
									}
								}
							}
						}
					}
					if (toSelect != null) {
						tree.setSelectionPath(new TreePath(toSelect.getPath()));
						break;
					}
					jumpTo(restrictionDefinition.from);
				}
			} finally {
				suppressRestrictionSelection = false;
			}
		}
	}

	/**
	 * Saves the model.
	 * 
	 * @param selectNewFile <code>true</code> for 'save as...'
	 */
	public boolean save(boolean selectNewFile, String reason) {
		if (currentRestrictionDefinitions == null) {
			return false;
		}
		if (selectNewFile || extractionModelFile == null) {
			String startDir = "extractionmodel";
			if (extractionModelFile != null) {
				startDir = new File(extractionModelFile).getParent();
				if (startDir == null) {
					startDir = "extractionmodel";
				}
			}
			String newFile = UIUtil.choseFile(extractionModelFile == null? null : new File(extractionModelFile), startDir, (reason == null? "" : (reason + " - ")) + "Save Extraction Model", ".csv", this, true, false);
			if (newFile == null) {
				return false;
			}
			extractionModelFile = newFile;
		}
		File extractionModel = new File(extractionModelFile);
		try {
			PrintWriter out = new PrintWriter(extractionModel);
			out.println("# subject; condition;  limit; restrictions");
			out.println(subjectTable.getSelectedItem() + "; " + condition.getText() + "; ; " + RestrictionModel.EMBEDDED);
			saveRestrictions(out);
			out.close();
			needsSave = false;
			extractionModelFrame.updateTitle(needsSave);
		} catch (IOException e) {
			UIUtil.showException(this, "Could not save " + new File(extractionModelFile).getName(), e);
			return false;
		}
		return true;
	}

	/**
	 * Saves restrictions only.
	 * 
	 * @param out to save restrictions into
	 */
	private void saveRestrictions(PrintWriter out) {
		out.println();
		out.println("# from A (or association name); to B; restriction-condition");
		for (RestrictionDefinition rd: currentRestrictionDefinitions) {
			String condition = rd.isIgnored? "ignore" : rd.condition;
			if (rd.name == null || rd.name.trim().length() == 0) {
				out.println(rd.from.getName() + "; " + rd.to.getName() + "; " + condition);
			} else {
				out.println(rd.name + "; ; " + condition);
			}
		}
	}

	/**
	 * Saves restrictions only.
	 * 
	 * @param file to save restrictions into
	 */
	public void saveRestrictions(File file) throws Exception {
		PrintWriter out = new PrintWriter(file);
		saveRestrictions(out);
		out.close();
	}

	/**
     * Gets set of all association which are not editable due to ambiguity.
     */
    private Set<Association> getAmbiguousNonames() {
        if (ambiguousNonames == null) {
            ambiguousNonames = new HashSet<Association>();
            for (Table table: dataModel.getTables()) {
                for (Association a: table.associations) {
                    if (a.getName() == null) {
                        for (Association b: table.associations) {
                            if (!a.equals(b)) {
                                if (a.destination.equals(b.destination)) {
                                    ambiguousNonames.add(a);
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }
        return ambiguousNonames;
    }
        
    /**
     * Refreshed associations tree.
     */
	public void refresh() {
		tree.setModel(getModel());
	}
	
	/**
	 * Expands all node in associations tree.
	 */
	public void expand() {
		for (DefaultMutableTreeNode node: treeNodes) {
			tree.expandPath(new TreePath(node.getPath()));
		}
	}

	/**
	 * Removes all restrictions from extraction model.
	 */
	public void removeAllRestrictions() {
		for (Table table: dataModel.getTables()) {
			for (Association association: table.associations) {
				if (!association.isInsertDestinationBeforeSource()) {
					dataModel.getRestrictionModel().addRestriction(table, association, "", "GUI", true);
				}
			}
		}
		if (!needsSave) {
			needsSave = true;
			extractionModelFrame.updateTitle(needsSave);
		}
		tree.repaint();
		restrictionsTable.setModel(restrictionTableModel());
		initRestrictionEditor(currentAssociation, currentNode);
	}
	
	/**
	 * Add restriction for each non-dependency.
	 */
	public void ignoreAll() {
		for (Table table: dataModel.getTables()) {
			for (Association association: table.associations) {
				if (!association.isInsertDestinationBeforeSource()) {
					if (association.getName() != null && !"".equals(association.getName().trim())) {
						dataModel.getRestrictionModel().addRestriction(table, association, "false", "GUI", true);
					}
				}
			}
		}
		if (!needsSave) {
			needsSave = true;
			extractionModelFrame.updateTitle(needsSave);
		}
		tree.repaint();
		restrictionsTable.setModel(restrictionTableModel());
		initRestrictionEditor(currentAssociation, currentNode);
	}

    // Variablendeklaration - nicht modifizieren//GEN-BEGIN:variables
    private javax.swing.JLabel associatedWith;
    private javax.swing.JTextField condition;
    private javax.swing.JLabel dependsOn;
    public javax.swing.JButton exportButton;
    private javax.swing.JLabel hasDependent;
    private javax.swing.JLabel ignored;
    private javax.swing.JPanel inspectorHolder;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane4;
    private javax.swing.JPanel legende;
    private javax.swing.JTable restrictionsTable;
    private javax.swing.JComboBox rootTable;
    private javax.swing.JComboBox subjectTable;
    private javax.swing.JTree tree;
    // Ende der Variablendeklaration//GEN-END:variables

}
