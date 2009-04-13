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

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
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
import javax.swing.DefaultListCellRenderer;
import javax.swing.JInternalFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.JTree;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableModel;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreeCellRenderer;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import net.sf.jailer.Jailer;
import net.sf.jailer.ScriptFormat;
import net.sf.jailer.datamodel.AggregationSchema;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.ModelElement;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.extractionmodel.ExtractionModel;
import net.sf.jailer.restrictionmodel.RestrictionModel;
import net.sf.jailer.ui.graphical_view.AssociationRenderer;
import net.sf.jailer.ui.graphical_view.GraphicalDataModelView;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.SqlUtil;
import sdoc.SyntaxSupport;

/**
 * Editor for {@link ExtractionModel}s.
 * 
 * @author Ralf Wisser
 */
public class ExtractionModelEditor extends javax.swing.JPanel {

	/**
	 * Editor for single restriction.
	 */
	public RestrictionEditor restrictionEditor;
	
	/**
	 * Holds restrictions.
	 */
	private ExtractionModel extractionModel;
	
	/**
	 * The restricted data model.
	 */
	DataModel dataModel;
	
	/**
	 * Closure of current {@link #subjectTable}.
	 */
	private Set<Table> currentSubjectClosure = null;
	
	/**
	 * Version of data model from which {@link #currentSubjectClosure} is build.
	 */
	private long closureVersion = -1;
	
	/**
	 * Subject table.
	 */
	Table subject;
	
	/**
	 * Root of 'associations'-tree.
	 */
	Table root;

	/**
	 * Maps unnamed {@link Association}s to one {@link Association} having same source and destination.
	 */
	private Map<Association, Association> representant;
	
	/**
	 * Maps {@link Association}s and Root-table to tree-nodes.
	 */
	private Map<ModelElement, DefaultMutableTreeNode> toNode;
	
	/**
	 * Model of tree-view.
	 */
	private DefaultTreeModel treeModel;
	
	/**
	 * Collection of all nodes of treeModel.
	 */
	private Collection<DefaultMutableTreeNode> treeNodes;
	
	/**
	 * For making {@link #select(Association)} re-entrant.
	 */
	private boolean suppressRestrictionSelection = false;
	
	/**
	 * <code>true</code> iff model is modified after last saving.
	 */
	boolean needsSave = false;
	
	/**
	 * The enclosing frame.
	 */
	public ExtractionModelFrame extractionModelFrame;

	/**
	 * Dialog for XML column mapping.
	 */
    private final ColumnMapperDialog columnMapperDialog;
    
	/**
	 * Name of file containing the currently edited model.
	 */
	String extractionModelFile;
	
	/**
	 * The graphical model view.
	 */
	GraphicalDataModelView graphView; 
	
	public ScriptFormat scriptFormat = ScriptFormat.SQL;

	private boolean isHorizontalLayout = false;
	
	/** 
	 * Creates new form ModelTree.
	 *  
	 * @param extractionModelFile file containing the model
	 * @param extractionModelFrame the enclosing frame
     */
	public ExtractionModelEditor(String extractionModelFile, ExtractionModelFrame extractionModelFrame, boolean horizontalLayout) {
		this.extractionModelFrame = extractionModelFrame;
		this.extractionModelFile = extractionModelFile;
		columnMapperDialog = new ColumnMapperDialog(extractionModelFrame);
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
			if (subject == null && dataModel != null && !dataModel.getTables().isEmpty()) {
				subject = dataModel.getTables().iterator().next();
				needsSave = true;
			}
		} catch (Exception e) {
			UIUtil.showException(this, extractionModelFile == null? "Error creating new Model" : "Error in " + new File(extractionModelFile).getName(), e);
			return;
		}

		boolean saveNeedsSave = needsSave;
		initComponents();
		tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
		tree.setExpandsSelectedPaths(true);
		restrictionEditor = new RestrictionEditor();
		
		if (isHorizontalLayout ) {
			graphView = new GraphicalDataModelView(dataModel, this, subject, 948, 379);
		} else {
			graphView = new GraphicalDataModelView(dataModel, this, subject, 648, 579);
		}
		graphContainer.add(graphView);
		
		AssociationRenderer.COLOR_ASSOCIATION = associatedWith.getForeground();
		AssociationRenderer.COLOR_DEPENDENCY = dependsOn.getForeground();
		AssociationRenderer.COLOR_IGNORED = ignored.getForeground();
		AssociationRenderer.COLOR_REVERSE_DEPENDENCY = hasDependent.getForeground();
		
		restrictionsTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		restrictionsTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent e) {
				if (restrictionsTable.getSelectedRow() >= 0) {
					select(currentRestrictionDefinitions.get(restrictionsTable.getSelectedRow()));
				}
			}
		});
		final TableCellRenderer defaultTableCellRenderer = restrictionsTable.getDefaultRenderer(String.class);
		final Color BG1 = new Color(255, 255, 255);
		final Color BG2 = new Color(230, 255, 255);
		restrictionsTable.setDefaultRenderer(Object.class, new TableCellRenderer() {
			public Component getTableCellRendererComponent(JTable table,
					Object value, boolean isSelected, boolean hasFocus,
					int row, int column) {
				Component render = defaultTableCellRenderer.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
				if (render instanceof JLabel && !isSelected) {
					((JLabel) render).setBackground(row % 2 == 0? BG1 : BG2);
				}
				return render;
			}
		});
		
		restrictionsTable.setShowGrid(false);
		
		java.awt.GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        inspectorHolder.add(restrictionEditor, gridBagConstraints);
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
//		restrictionEditor.jump.addActionListener(new ActionListener() {
//			public void actionPerformed(ActionEvent e) {
//				onJump();
//			}
//		});
		initRestrictionEditor(null, null);
		if (extractionModel.getTasks().get(0).subject != null) {
			subjectTable.setSelectedItem(dataModel.getDisplayName(extractionModel.getTasks().get(0).subject));
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
		
		try {
			ScriptFormat f = ScriptFormat.valueOf(dataModel.getExportModus());
			if (f != null) {
				scriptFormat = f;
			}
		} catch (Exception e) {
			// ignore
		}
		
		exportFormat.setModel(new DefaultComboBoxModel(ScriptFormat.values()));
		exportFormat.setRenderer(new DefaultListCellRenderer() {
			@Override
			public Component getListCellRendererComponent(JList list,
					Object value, int index, boolean isSelected,
					boolean cellHasFocus) {
				return super.getListCellRendererComponent(list, ((ScriptFormat) value).getDisplayName(), index, isSelected,
						cellHasFocus);
			}
		});
		exportFormat.setSelectedItem(scriptFormat);
    	openXmlSettings.setVisible(ScriptFormat.XML.equals(scriptFormat));
		onExportModusChanged(null);
        SyntaxSupport instance = SyntaxSupport.getInstance();
        instance.highlightCurrent(false);
		instance.addSupport(SyntaxSupport.XML_LEXER, xmlSketch);
		setOrientation(horizontalLayout);
	}

	void setOrientation(boolean horizontal) {
		isHorizontalLayout = horizontal;
		
		if (isHorizontalLayout) {
			jPanel1.removeAll();
			GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 0;
	        gridBagConstraints.gridy = 1;
	        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
	        gridBagConstraints.weightx = 1.0;
	        gridBagConstraints.weighty = 1.0;
	        jPanel1.add(editorPanel, gridBagConstraints);

			jSplitPane1.setOrientation(JSplitPane.VERTICAL_SPLIT);
			jSplitPane1.setLeftComponent(jPanel1);
			
			editorPanel.removeAll();
			editorPanel.add(jPanel3);
			editorPanel.add(jPanel4);
			editorPanel.add(inspectorHolder);
			((GridLayout) editorPanel.getLayout()).setVgap(0);
			((GridLayout) editorPanel.getLayout()).setRows(1);
			if (ScriptFormat.XML.equals(scriptFormat)) {
				editorPanel.add(xmlMappingPanel);
				((GridLayout) editorPanel.getLayout()).setColumns(4);
			} else {
				((GridLayout) editorPanel.getLayout()).setColumns(3);
			}
		} else {
			jSplitPane1.setOrientation(JSplitPane.HORIZONTAL_SPLIT);
			
			final JScrollPane scrollPanel = new JScrollPane() {
				@Override
				public Dimension getMinimumSize() {
					return editorPanel.getMinimumSize();
				}
			};
			scrollPanel.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
			JPanel panel = new JPanel() {
				@Override
				public Dimension getMaximumSize() {
					Dimension s = super.getMaximumSize();
					return new Dimension(scrollPanel.getWidth() - 20, Math.max((int) (650.0 * (scriptFormat.equals(ScriptFormat.XML)? 1.33 : 1.0)), scrollPanel.getHeight() - 10));
				}
				@Override
				public Dimension getPreferredSize() {
					return getMaximumSize();
				}
			};
			
			java.awt.GridBagConstraints gridBagConstraints;

			panel.setLayout(new GridLayout(1, 1, 0, 0));
			panel.add(editorPanel);
			scrollPanel.setViewportView(panel);
			jSplitPane1.setLeftComponent(scrollPanel);
			
			JPanel panel2 = new JPanel();
			panel2.setLayout(new GridBagLayout());
			gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 0;
	        gridBagConstraints.gridy = 0;
	        gridBagConstraints.weightx = 1;
	        gridBagConstraints.weighty = 1;
	        gridBagConstraints.fill = GridBagConstraints.BOTH;
	        
			panel2.add(jPanel3, gridBagConstraints);
			gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 0;
	        gridBagConstraints.gridy = 1;
	        gridBagConstraints.weightx = 1;
	        gridBagConstraints.weighty = 1;
	        gridBagConstraints.fill = GridBagConstraints.BOTH;
	        gridBagConstraints.insets = new Insets(6, 0, 0, 0);
			panel2.add(jPanel4, gridBagConstraints);
			gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 0;
	        gridBagConstraints.gridy = 2;
	        gridBagConstraints.weightx = 1;
	        gridBagConstraints.weighty = 0;
	        gridBagConstraints.fill = GridBagConstraints.BOTH;
	        gridBagConstraints.insets = new Insets(6, 0, 0, 0);
			panel2.add(inspectorHolder, gridBagConstraints);
			
			editorPanel.removeAll();
			editorPanel.add(panel2);
			((GridLayout) editorPanel.getLayout()).setVgap(1);
			((GridLayout) editorPanel.getLayout()).setColumns(1);
			if (ScriptFormat.XML.equals(scriptFormat)) {
				gridBagConstraints = new java.awt.GridBagConstraints();
		        gridBagConstraints.gridx = 0;
		        gridBagConstraints.gridy = 3;
		        gridBagConstraints.weightx = 1;
		        gridBagConstraints.weighty = 1;
		        gridBagConstraints.fill = GridBagConstraints.BOTH;
		        gridBagConstraints.insets = new Insets(6, 0, 0, 0);
				panel2.add(xmlMappingPanel, gridBagConstraints);
//				editorPanel.add(xmlMappingPanel);
				((GridLayout) editorPanel.getLayout()).setRows(1);
			} else {
				((GridLayout) editorPanel.getLayout()).setRows(1);
			}
		}
	}
	
	/**
	 * Resets the graphical editor.
	 */
	public void resetGraphEditor(boolean full) {
		if (full) {
			graphView.close();
			graphContainer.remove(graphView);
			graphView = new GraphicalDataModelView(dataModel, this, root, graphView.display.getWidth(), graphView.display.getHeight());
			graphContainer.add(graphView);
		} else {
			graphView.resetExpandedState();
		}
		validate();
	}
	
	/**
	 * Gets current subject table.
	 * 
	 * @return current subject table
	 */
	public Table getSubject() {
		return subject;
	}
	
	/**
	 * Gets current subject condition.
	 * 
	 * @return current subject condition
	 */
	public String getSubjectCondition() {
		return condition.getText();
	}
	
	/**
	 * This method is called from within the constructor to initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is always
	 * regenerated by the Form Editor.
	 */
	// <editor-fold defaultstate="collapsed" desc=" Erzeugter Quelltext
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jScrollPane2 = new javax.swing.JScrollPane();
        buttonGroup1 = new javax.swing.ButtonGroup();
        jSplitPane1 = new javax.swing.JSplitPane();
        graphContainer = new javax.swing.JPanel();
        jPanel1 = new javax.swing.JPanel();
        editorPanel = new javax.swing.JPanel();
        jPanel3 = new javax.swing.JPanel();
        jLabel6 = new javax.swing.JLabel();
        subjectTable = new javax.swing.JComboBox();
        jLabel7 = new javax.swing.JLabel();
        jScrollPane4 = new javax.swing.JScrollPane();
        restrictionsTable = new javax.swing.JTable();
        jLabel8 = new javax.swing.JLabel();
        condition = new javax.swing.JTextField();
        jLabel4 = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        jPanel2 = new javax.swing.JPanel();
        openXmlSettings = new javax.swing.JButton();
        exportButton = new javax.swing.JButton();
        exportFormat = new javax.swing.JComboBox();
        jPanel4 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        tree = new javax.swing.JTree();
        jPanel6 = new javax.swing.JPanel();
        jLabel9 = new javax.swing.JLabel();
        rootTable = new javax.swing.JComboBox();
        openClosureView = new javax.swing.JButton();
        inspectorHolder = new javax.swing.JPanel();
        xmlMappingPanel = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        aggregationCombobox = new javax.swing.JComboBox();
        tagField = new javax.swing.JTextField();
        jScrollPane3 = new javax.swing.JScrollPane();
        xmlSketch = new javax.swing.JTextArea();
        jPanel5 = new javax.swing.JPanel();
        xmlTagApply = new javax.swing.JButton();
        mapColumns = new javax.swing.JButton();
        legende = new javax.swing.JPanel();
        dependsOn = new javax.swing.JLabel();
        hasDependent = new javax.swing.JLabel();
        associatedWith = new javax.swing.JLabel();
        ignored = new javax.swing.JLabel();

        setLayout(new java.awt.GridBagLayout());

        jSplitPane1.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        jSplitPane1.setContinuousLayout(true);
        jSplitPane1.setOneTouchExpandable(true);

        graphContainer.setLayout(new java.awt.BorderLayout());
        jSplitPane1.setBottomComponent(graphContainer);

        jPanel1.setLayout(new java.awt.GridBagLayout());

        editorPanel.setLayout(new java.awt.GridLayout(1, 4));

        jPanel3.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "Subject", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Dialog", 2, 12)));
        jPanel3.setMaximumSize(new java.awt.Dimension(2147483647, 2147483647));
        jPanel3.setMinimumSize(new java.awt.Dimension(158, 122));
        jPanel3.setLayout(new java.awt.GridBagLayout());

        jLabel6.setText("Export from ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel3.add(jLabel6, gridBagConstraints);

        subjectTable.setMaximumRowCount(24);
        subjectTable.setModel(subjectListModel());
        subjectTable.setMinimumSize(new java.awt.Dimension(150, 19));
        subjectTable.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                onNewSubject(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel3.add(subjectTable, gridBagConstraints);

        jLabel7.setText("Where");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel3.add(jLabel7, gridBagConstraints);

        restrictionsTable.setModel(restrictionTableModel());
        jScrollPane4.setViewportView(restrictionsTable);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 14;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel3.add(jScrollPane4, gridBagConstraints);

        jLabel8.setFont(new java.awt.Font("Dialog", 2, 14));
        jLabel8.setText("Restrictions");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 13;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 4, 0);
        jPanel3.add(jLabel8, gridBagConstraints);

        condition.setText("jTextField1");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel3.add(condition, gridBagConstraints);

        jLabel4.setText("To");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel3.add(jLabel4, gridBagConstraints);

        jLabel5.setText(" as T ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        jPanel3.add(jLabel5, gridBagConstraints);

        jPanel2.setLayout(new java.awt.GridBagLayout());

        openXmlSettings.setText("XML Settings");
        openXmlSettings.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                openXmlSettingsActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 4);
        jPanel2.add(openXmlSettings, gridBagConstraints);

        exportButton.setText("Export Data");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 2, 2);
        jPanel2.add(exportButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 11;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        jPanel3.add(jPanel2, gridBagConstraints);

        exportFormat.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        exportFormat.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                onExportModusChanged(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel3.add(exportFormat, gridBagConstraints);

        editorPanel.add(jPanel3);

        jPanel4.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "Association", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Dialog", 2, 12)));
        jPanel4.setLayout(new java.awt.BorderLayout(0, 4));

        jScrollPane1.setBorder(null);
        jScrollPane1.setAutoscrolls(true);

        tree.setAutoscrolls(true);
        tree.setCellRenderer(getTreeCellRenderer(tree.getCellRenderer()));
        tree.setModel(getModel());
        tree.addTreeSelectionListener(new javax.swing.event.TreeSelectionListener() {
            public void valueChanged(javax.swing.event.TreeSelectionEvent evt) {
                treeValueChanged(evt);
            }
        });
        tree.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                treeMouseClicked(evt);
            }
        });
        jScrollPane1.setViewportView(tree);

        jPanel4.add(jScrollPane1, java.awt.BorderLayout.CENTER);

        jPanel6.setLayout(new java.awt.GridBagLayout());

        jLabel9.setText(" Focus ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        jPanel6.add(jLabel9, gridBagConstraints);

        rootTable.setMaximumRowCount(24);
        rootTable.setModel(getTableListModel());
        rootTable.setMinimumSize(new java.awt.Dimension(150, 19));
        rootTable.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                rootTableItemStateChanged(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel6.add(rootTable, gridBagConstraints);

        openClosureView.setText("Closure...");
        openClosureView.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                openClosureViewActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 0);
        jPanel6.add(openClosureView, gridBagConstraints);

        jPanel4.add(jPanel6, java.awt.BorderLayout.NORTH);

        editorPanel.add(jPanel4);

        inspectorHolder.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "Restriction", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Dialog", 2, 12)));
        inspectorHolder.setMinimumSize(new java.awt.Dimension(100, 400));
        inspectorHolder.setLayout(new java.awt.GridBagLayout());
        editorPanel.add(inspectorHolder);

        xmlMappingPanel.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "XML Mapping", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Dialog", 2, 12)));
        xmlMappingPanel.setLayout(new java.awt.GridBagLayout());

        jLabel1.setText(" Aggregation ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        xmlMappingPanel.add(jLabel1, gridBagConstraints);

        jLabel2.setText(" Tag ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        xmlMappingPanel.add(jLabel2, gridBagConstraints);

        jLabel3.setText(" Sketch ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        xmlMappingPanel.add(jLabel3, gridBagConstraints);

        aggregationCombobox.setModel(getAggregationModel());
        aggregationCombobox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                aggregationComboboxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 4);
        xmlMappingPanel.add(aggregationCombobox, gridBagConstraints);

        tagField.setMinimumSize(new java.awt.Dimension(250, 19));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 4);
        xmlMappingPanel.add(tagField, gridBagConstraints);

        xmlSketch.setColumns(10);
        xmlSketch.setEditable(false);
        xmlSketch.setRows(5);
        xmlSketch.setOpaque(false);
        jScrollPane3.setViewportView(xmlSketch);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 11;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 20, 0, 4);
        xmlMappingPanel.add(jScrollPane3, gridBagConstraints);

        xmlTagApply.setText("apply");
        xmlTagApply.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                xmlTagApplyActionPerformed(evt);
            }
        });
        jPanel5.add(xmlTagApply);

        mapColumns.setText("map columns");
        mapColumns.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                mapColumnsActionPerformed(evt);
            }
        });
        jPanel5.add(mapColumns);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        xmlMappingPanel.add(jPanel5, gridBagConstraints);

        editorPanel.add(xmlMappingPanel);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel1.add(editorPanel, gridBagConstraints);

        jSplitPane1.setTopComponent(jPanel1);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(jSplitPane1, gridBagConstraints);

        legende.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 0, 0));

        dependsOn.setFont(new java.awt.Font("Dialog", 0, 12));
        dependsOn.setForeground(new java.awt.Color(170, 0, 0));
        dependsOn.setText("depends on");
        legende.add(dependsOn);

        hasDependent.setFont(new java.awt.Font("Dialog", 0, 12));
        hasDependent.setForeground(new java.awt.Color(0, 112, 0));
        hasDependent.setText("   has dependent");
        legende.add(hasDependent);

        associatedWith.setFont(new java.awt.Font("Dialog", 0, 12));
        associatedWith.setForeground(new java.awt.Color(0, 100, 255));
        associatedWith.setText("   associated with");
        legende.add(associatedWith);

        ignored.setFont(new java.awt.Font("Dialog", 0, 12));
        ignored.setForeground(new java.awt.Color(153, 153, 153));
        ignored.setText("   disabled");
        legende.add(ignored);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 6);
        add(legende, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    private void openClosureViewActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_openClosureViewActionPerformed
    	extractionModelFrame.openClosureView(root);
    }//GEN-LAST:event_openClosureViewActionPerformed

    private void treeMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_treeMouseClicked
    	if (evt.getButton() == MouseEvent.BUTTON3) {
            if (evt.getClickCount() == 1) {
                TreePath node = tree.getPathForLocation(evt.getX(), evt.getY());
                if (node != null) {
                    Object sel = node.getLastPathComponent();
                    if (sel instanceof DefaultMutableTreeNode) {
                        Object selNode = ((DefaultMutableTreeNode) sel).getUserObject();
                        Table table = null;
                        if (selNode instanceof Table) {
                            table = (Table) selNode;
                        }
                        if (selNode instanceof Association) {
                            table = ((Association) selNode).destination;
                        }
                        JPopupMenu popup = graphView.createPopupMenu(table);
				        popup.show(evt.getComponent(), evt.getX(), evt.getY());
                    }
                }
            }
        }
    }//GEN-LAST:event_treeMouseClicked

    private void mapColumnsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_mapColumnsActionPerformed
    	applyXmlMapping();
    	Table table = root;
    	if (currentAssociation != null) {
    		table = currentAssociation.source;
    	}
    	openColumnMapper(table);
    }//GEN-LAST:event_mapColumnsActionPerformed

    private void xmlTagApplyActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_xmlTagApplyActionPerformed
    	applyXmlMapping();
    }//GEN-LAST:event_xmlTagApplyActionPerformed

    private XmlSettingsDialog xmlSettingsDialog;
    
    private void openXmlSettingsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_openXmlSettingsActionPerformed
    	if (xmlSettingsDialog == null) {
    		 xmlSettingsDialog = new XmlSettingsDialog(extractionModelFrame);
    	}
    	if (xmlSettingsDialog.edit(dataModel)) {
    		needsSave = true;
			ExtractionModelEditor.this.extractionModelFrame.updateTitle(needsSave);
    	}
    }//GEN-LAST:event_openXmlSettingsActionPerformed

    private void aggregationComboboxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_aggregationComboboxActionPerformed
    	tagField.setEnabled(AggregationSchema.NONE != aggregationCombobox.getSelectedItem() && AggregationSchema.FLAT != aggregationCombobox.getSelectedItem());
    	tagField.setEditable(AggregationSchema.NONE != aggregationCombobox.getSelectedItem() && AggregationSchema.FLAT != aggregationCombobox.getSelectedItem());
		if (currentAssociation != null) {
    		if (currentAssociation.getAggregationSchema() != aggregationCombobox.getSelectedItem()) {
    			currentAssociation.setAggregationSchema((AggregationSchema) aggregationCombobox.getSelectedItem());
    			needsSave = true;
				ExtractionModelEditor.this.extractionModelFrame.updateTitle(needsSave);
				updateSketch();
			}
    	}
    }//GEN-LAST:event_aggregationComboboxActionPerformed

    private void onNewSubject(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_onNewSubject
    	Object selectedItem = subjectTable.getSelectedItem();
		if (selectedItem instanceof String) {
			if (dataModel.getTableByDisplayName(selectedItem.toString()) != null) {
				currentSubjectClosure = null; // force re-calculation
				subject = dataModel.getTableByDisplayName(selectedItem.toString());
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
    		Table table = dataModel.getTableByDisplayName(evt.getItem().toString());
    		setRoot(table);
    	}
    }//GEN-LAST:event_rootTableItemStateChanged

    /**
     * Sets the root table
     * 
     * @param table the new root
     */
	public void setRoot(Table table) {
		if (table != null) {
			root = table;
			tree.setModel(getModel());
			resetGraphEditor(true);
		}
	}
	
	/**
     * Sets root table selection of select-box.
     * 
     * @param table the new root
     */
	public void setRootSelection(Table table) {
		if (table != null) {
			rootTable.setSelectedItem(dataModel.getDisplayName(table));
		}
	}

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
    		data[i++] = new Object[] { dataModel.getDisplayName(def.from), dataModel.getDisplayName(def.to), def.name == null? "" : def.name, def.condition };
    	}
        return new DefaultTableModel(data, new Object[] { "From", "To", "Name", "Condition" });
    }
    
    /**
     * Gets list model for the subject-combobox.
     * 
     * @return list model for the subject-combobox
     */
    private ComboBoxModel subjectListModel() {
    	Vector<String> tableNames = new Vector<String>();
    	for (Table table: dataModel.getTables()) {
    		tableNames.add(dataModel.getDisplayName(table));
    	}
    	Collections.sort(tableNames);
    	DefaultComboBoxModel model = new DefaultComboBoxModel(tableNames);
        return model;
    }

    /**
     * Gets list model for the root-combobox.
     * 
     * @return list model for the root-combobox
     */
    private ComboBoxModel getTableListModel() {
    	Vector<String> tableNames = new Vector<String>();
    	for (Table table: dataModel.getTables()) {
    		tableNames.add(dataModel.getDisplayName(table));
    	}
    	Collections.sort(tableNames);
        if (subject != null) {
	    	tableNames.add(0, dataModel.getDisplayName(subject));
	        tableNames.add(1, "---");
        }
    	DefaultComboBoxModel model = new DefaultComboBoxModel(tableNames);
        return model;
    }

    /**
     * Reacts on changes of selection of tree-view.
     */
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

    Association currentAssociation;
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
    	initXmlMappingEditor(association);
    	initRestrictedDependencyWarningField();
    	if (association == null) {
			int l = currentRestrictionDefinitions == null? -1 : currentRestrictionDefinitions.size();
			if (l > 0) {
				restrictionsTable.removeRowSelectionInterval(0, l - 1);
			}
			String noVal = "n/a";
			restrictionEditor.apply.setEnabled(false);
			restrictionEditor.cardinality.setText("");
			restrictionEditor.destination.setText(noVal);
			restrictionEditor.ignore.getModel().setPressed(false);
			restrictionEditor.ignore.setEnabled(false);
			restrictionEditor.joinCondition.setText(noVal);
			restrictionEditor.joinCondition.setToolTipText(noVal);
//			restrictionEditor.jump.setEnabled(false); 
			restrictionEditor.restriction.setText("");
			restrictionEditor.restriction.setEditable(false);
			restrictionEditor.source.setText(noVal);
			restrictionEditor.type.setText(noVal);
            restrictionEditor.aName.setText(noVal);
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
//			if (!association.destination.equals(root)) {
//				restrictionEditor.jump.setEnabled(true);
//			} else {
//				restrictionEditor.jump.setEnabled(false);
//			}

			String type = "associates  ";
			Color typeColor = associatedWith.getForeground();
			boolean editable = true;
			if (association.isInsertDestinationBeforeSource()) {
				type = "depends on  ";
				typeColor = dependsOn.getForeground();
				// editable = false;
			} else if (association.isInsertSourceBeforeDestination()) {
				type = "has dependency  ";
				typeColor = hasDependent.getForeground();
			}
            String shortendName = association.getName();
            if (shortendName != null && shortendName.length() > 30) {
            	restrictionEditor.aName.setToolTipText(shortendName);
            	shortendName = shortendName.substring(0, 30) + "...";
            } else {
            	restrictionEditor.aName.setToolTipText(null);
            }
			restrictionEditor.aName.setText(shortendName);
//            restrictionEditor.aName.setForeground(restrictionEditor.type.getForeground());
            if (getAmbiguousNonames().contains(association)) {
//                restrictionEditor.aName.setForeground(Color.RED);
                restrictionEditor.aName.setText("unnamed and ambiguous");
                editable = false;
            }
			restrictionEditor.restriction.setEditable(true);
			restrictionEditor.source.setText(dataModel.getDisplayName(association.source));
			restrictionEditor.destination.setText(dataModel.getDisplayName(association.destination));
			restrictionEditor.cardinality.setText(association.getCardinality() == null? "" : ("(" + association.getCardinality().toString() + ")"));
			restrictionEditor.type.setText(type);
			restrictionEditor.type.setForeground(typeColor);
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
			restrictionEditor.joinCondition.setToolTipText(joinCondition);
		}
		graphView.setSelection(association);
	}

    private void initRestrictedDependencyWarningField() {
		restrictionEditor.restrictedDependencyWarning.setVisible(currentAssociation != null && !ScriptFormat.XML.equals(scriptFormat) && currentAssociation.isInsertDestinationBeforeSource() && currentAssociation.isRestricted());
	}

	/**
     * Initializes the XML mapping editor.
     * 
     * @param association the association on which mapping can be edited
     */
    private void initXmlMappingEditor(Association association) {
    	if (association == null) {
    		aggregationCombobox.setSelectedItem(AggregationSchema.NONE);
    		tagField.setText("");
    		aggregationCombobox.setEnabled(false);
    		tagField.setEnabled(false);
    		tagField.setEditable(false);
    		xmlTagApply.setEnabled(false);
    	} else {
    		aggregationCombobox.setEditable(false); // association.reversalAssociation.getAggregationSchema() == AggregationSchema.NONE);
    		aggregationCombobox.setEnabled(association.reversalAssociation.getAggregationSchema() == AggregationSchema.NONE);
    		aggregationCombobox.setSelectedItem(association.getAggregationSchema());
    		tagField.setText(association.getAggregationTagName());
    		tagField.setEnabled(AggregationSchema.NONE != association.getAggregationSchema() && AggregationSchema.FLAT != association.getAggregationSchema());
    		tagField.setEditable(AggregationSchema.NONE != association.getAggregationSchema() && AggregationSchema.FLAT != association.getAggregationSchema());
    		xmlTagApply.setEnabled(true);
    	}
    	updateSketch();
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
    public void onApply(boolean applyButtonKlicked) {
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
    		graphView.display.invalidate();
			restrictionsTable.setModel(restrictionTableModel());
			String saveInitialRestrictionCondition = initialRestrictionCondition;
			initRestrictionEditor(currentAssociation, currentNode);
			initialRestrictionCondition = saveInitialRestrictionCondition;
			extractionModelFrame.closureView.refresh();
			tree.grabFocus();
    	}
    }
    
    /**
     * Makes a table the root of the associations-tree.
     */
	private void jumpTo(Table table) {
	    rootTable.setSelectedItem(dataModel.getDisplayName(table));
    }

	/**
     * Gets model of the associations-tree.
     */
	private TreeModel getModel() {
		return getModel(null);
	}
	
    /**
     * Gets model of the associations-tree.
     * 
     * @param dontExclude don't exclude this association
     */
	private TreeModel getModel(Association dontExclude) {
		representant = new HashMap<Association, Association>();
		toNode = new HashMap<ModelElement, DefaultMutableTreeNode>();
		treeNodes = new ArrayList<DefaultMutableTreeNode>();

		if (this.root == null) {
			this.root = subject;
		}
		
		DefaultMutableTreeNode root = new DefaultMutableTreeNode(this.root);
		treeNodes.add(root);
		
		Map<Association, DefaultMutableTreeNode> parent = new HashMap<Association, DefaultMutableTreeNode>();
		List<Association> agenda = new LinkedList<Association>();

		toNode.put(this.root, root);
		if (this.root != null) {
			for (Association a: this.root.associations) {
				agenda.add(a);
				parent.put(a, root);
			}
		}
		
		DefaultMutableTreeNode dontExcludeNode = null;
		
		while (!agenda.isEmpty()) {
			Association a = agenda.get(0);
			agenda.remove(0);
			// since 2.4.1 disabled associations are only hidden in graphical view, not in the tree
//			if (dontExclude == null && extractionModelFrame.hideIgnored() && a.isIgnored()) {
//				continue;
//			}
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
					if (dontExclude == a) {
						dontExcludeNode = node;
					}
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
	
		// since 2.4.1 disabled associations are only hidden in graphical view, not in the tree
/*		if (dontExclude != null && dontExcludeNode != null && extractionModelFrame.hideIgnored()) {
			// remove ignored associations except those on path to dontExclude
			TreeNode[] path = dontExcludeNode.getPath();
			treeNodes.clear();
			removeIgnoredAssociations(root, path);
		}
*/
		if (treeModel != null) {
			treeModel.setRoot(root);
		} else {
			treeModel = new DefaultTreeModel(root);
		}
		return treeModel;
	}

	/**
	 * Removes all disabled associations from tree.
	 * 
	 * @param root the root of the tree
	 * @param exceptions don't remove these nodes
	 */
	private void removeIgnoredAssociations(DefaultMutableTreeNode root,
			TreeNode[] exceptions) {
		if (root.getUserObject() instanceof Association) {
			Association a = (Association) root.getUserObject();
			if (a.isIgnored()) {
				boolean isException = false;
				for (TreeNode ex: exceptions) {
					if (ex == root) {
						isException = true;
						break;
					}
				}
				if (!isException) {
					root.removeFromParent();
					return;
				}
			}
		}
		treeNodes.add(root);
		List<DefaultMutableTreeNode> children = new ArrayList<DefaultMutableTreeNode>();
		for (int i = 0; i < root.getChildCount(); ++i) {
			children.add((DefaultMutableTreeNode) root.getChildAt(i));
		}
		for (DefaultMutableTreeNode n: children) {
			removeIgnoredAssociations(n, exceptions);
		}
	}

	/**
	 * Updates the XML sketch component.
	 */
	private void updateSketch() {
		try {
			xmlSketch.setText(XmlSketchBuilder.buildSketch(currentAssociation == null? null : currentAssociation.source, 0));
			xmlSketch.setCaretPosition(0);
		} catch (Exception e) {
			e.printStackTrace();
		}
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
					return dataModel.getDisplayName(a1.destination).compareTo(dataModel.getDisplayName(a2.destination));
				}
				return cat1 - cat2;
			}
		});
		node.removeAllChildren();
		for (MutableTreeNode child: children) {
			node.add(child);
		}
	}
	
	private void applyXmlMapping() {//GEN-FIRST:event_xmlMappingApplyButtonActionPerformed
		if (currentAssociation != null) {
			String tag = tagField.getText().trim();
		  	if (tag.length() == 0) {
		  		tag = null;
		  	}
		  	if (!currentAssociation.getAggregationTagName().equals(tag)) {
			  	currentAssociation.setAggregationTagName(tag);
			  	tagField.setText(currentAssociation.getAggregationTagName());
			  	needsSave = true;
			  	ExtractionModelEditor.this.extractionModelFrame.updateTitle(needsSave);
		  	}
		  	updateSketch();
	  	}
  	}//GEN-LAST:event_xmlMappingApplyButtonActionPerformed

    private void onExportModusChanged(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_onExportModusChanged
        ScriptFormat f = (ScriptFormat) exportFormat.getSelectedItem();
        if (f != null) {
        	scriptFormat = f;
        	dataModel.setExportModus(scriptFormat.toString());
        }
        setOrientation(isHorizontalLayout);
//    	if (ScriptFormat.XML.equals(scriptFormat)) {
//    		editorPanel.add(xmlMappingPanel);
//  		} else {
//  			editorPanel.remove(xmlMappingPanel);
//  		}
		validate();
    }//GEN-LAST:event_onExportModusChanged
		  
	/**
	 * Renderer for the tree-view.
	 */
	private TreeCellRenderer getTreeCellRenderer(TreeCellRenderer treeCellRenderer) {
		return new DefaultTreeCellRenderer() {
			public Component getTreeCellRendererComponent(JTree tree,
					Object value, boolean selected, boolean expanded,
					boolean leaf, int row, boolean hasFocus) {
				String text = "";
				if (value instanceof DefaultMutableTreeNode) {
					DefaultMutableTreeNode node = (DefaultMutableTreeNode) value;
					if (node.getUserObject() instanceof Table) {
						text = dataModel.getDisplayName(((Table) node.getUserObject()));
						setTextSelectionColor(Color.BLACK);
						setTextNonSelectionColor(Color.BLACK);
					} else if (node.getUserObject() instanceof Association) {
						Association association = (Association) node.getUserObject();
						text = dataModel.getDisplayName(association.destination);
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
						return wrapTreeNode(super.getTreeCellRendererComponent(tree, text, selected, expanded, leaf, row, hasFocus), association);
					}
				}
				return super.getTreeCellRendererComponent(tree, text, selected, expanded, leaf, row, hasFocus);
			}
		};
	}

	/**
	 * Adds an arrow that indicates the type of the reversal association to the render.
	 */
	private Component wrapTreeNode(Component component, final Association association) {
		java.awt.GridBagConstraints gridBagConstraints;

		if (!(component instanceof JLabel)) {
			return component;
		}
		
        JPanel panel = new javax.swing.JPanel();
        panel.setOpaque(false);
        final JLabel reversal = new javax.swing.JLabel();
        reversal.setText("re");

        panel.setLayout(new java.awt.GridBagLayout());

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        panel.add(component, gridBagConstraints);

        reversal.setFont(((JLabel) component).getFont());
        reversal.setBackground(((JLabel) component).getBackground());

        JPanel revArrow = new JPanel() {
        	
        	public void paintComponent(Graphics g) {
        		if (association.reversalAssociation.isIgnored()) {
        			return;
        		}
        		Color color;
        		if (association.reversalAssociation.isIgnored()) {
					color = ignored.getForeground();
				} else if (association.reversalAssociation.isInsertDestinationBeforeSource()) {
					color = dependsOn.getForeground();
				} else if (association.reversalAssociation.isInsertSourceBeforeDestination()) {
					color = hasDependent.getForeground();
				} else {
					color = associatedWith.getForeground();
				}
        	    int width = getWidth();
        	    int height = getHeight();
        	    if (g instanceof Graphics2D) {
        	    	Graphics2D g2d = (Graphics2D) g;
        	    	color = new Color(color.getRed(), color.getGreen(), color.getBlue(), 100);
        	    	g2d.setColor(color);
        	    	g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        	    	g2d.setStroke(new BasicStroke(2));
	        	    int a = height / 4;
	        	    g2d.drawLine(0, height / 2, width, height / 2);
	        	    g2d.drawLine(1, height / 2 - 1, a, height / 2 - a);
	        	    g2d.drawLine(1, height / 2 + 1, a, height / 2 + a);
        	    } else {
	        	    g.setColor(color);
	        	    int a = height / 4;
	        	    g.drawLine(0, height / 2, width, height / 2);
	        	    g.drawLine(0, height / 2, a, height / 2 - a);
	        	    g.drawLine(0, height / 2, a, height / 2 + a);
        	    }
        	}
        	
			@Override
			public Dimension getMinimumSize() {
				return getPreferredSize();
			}
        	
			@Override
			public Dimension getPreferredSize() {
				return reversal.getPreferredSize();
			}
        };
        
        revArrow.setOpaque(false);
        
        panel.add(revArrow, new java.awt.GridBagConstraints());

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        return panel;
	}
	
	/**
     * Set of all association which are not editable due to ambiguity.
     */
    private Set<Association> ambiguousNonames = null;

    /**
     * Selects a table in tree view.
     * 
     * @param table the table to select
     * @return <code>true</code> if table was selected
     */
	public boolean select(Table table) {
		if (root != null) {
			if (root.equals(table)) {
				Object r = tree.getModel().getRoot();
				if (r != null && r instanceof DefaultMutableTreeNode) {
					TreePath treePath = new TreePath(((DefaultMutableTreeNode) r).getPath());
					tree.setSelectionPath(treePath);
					tree.scrollPathToVisible(treePath);
					return true;
				}
			}
		}
		for (DefaultMutableTreeNode node: treeNodes) {
			if (node.getChildCount() > 0) {
				Table t = null;
				Association a = null;
				if (node.getUserObject() instanceof Association) {
					a = (Association) node.getUserObject();
					t = a.destination;
				}
				if (t != null && a != null && table.equals(t)) {
					select(a);
					TreePath treePath = new TreePath(node.getPath());
					tree.expandPath(treePath);
					for (int i = 0; i < node.getChildCount(); ++i) {
						DefaultMutableTreeNode c = (DefaultMutableTreeNode) node.getChildAt(i);
						tree.collapsePath(new TreePath(c.getPath()));
					}
					tree.scrollPathToVisible(treePath);
					return true;
				}
			}
		}
		Association first = null;
		for (Association a: table.associations) {
			if (first == null || dataModel.getDisplayName(first.destination).compareTo(dataModel.getDisplayName(a.destination)) < 0) {
				first = a;
			}
		}
		if (first != null) {
			return select(first);
		}
		return false;
	}
	
    /**
     * Selects a restriction.
     * 
     * @param restrictionDefinition the restriction to select
     */
	private void select(RestrictionDefinition restrictionDefinition) {
		if (!suppressRestrictionSelection) {
			suppressRestrictionSelection = true;
			try {
				DefaultMutableTreeNode toSelect = null;
				for (int i = 0; i < 3; ++i) {
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
						TreePath treePath = new TreePath(toSelect.getPath());
						tree.setSelectionPath(treePath);
						tree.scrollPathToVisible(treePath);
						break;
					}
					
					if (i == 2) {
						setRoot(restrictionDefinition.from);
						break;
					}
					// make association part of tree
					for (Table t: dataModel.getTables()) {
						for (Association a: t.associations) {
							if (a.destination.equals(restrictionDefinition.to) && a.source.equals(restrictionDefinition.from)) {
								if (restrictionDefinition.name == null || restrictionDefinition.name.length() == 0 || restrictionDefinition.name.equals(a.getName())) {
									tree.setModel(getModel(a));
									break;
								}
							}
						}
					}
				}
			} finally {
				suppressRestrictionSelection = false;
			}
		}
	}

    /**
     * Selects an association.
     * 
     * @param association the association to select
     * @return <code>true</code> if table was selected
     */
	public boolean select(Association association) {
		if (!suppressRestrictionSelection) {
			suppressRestrictionSelection = true;
			try {
				DefaultMutableTreeNode toSelect = null;
				for (int i = 0; i < 2; ++i) {
					for (DefaultMutableTreeNode node: treeNodes) {
						if (node.getUserObject() instanceof Association) {
							Association a = (Association) node.getUserObject();
							if (a.equals(association)) {
								if (toSelect == null || toSelect.isLeaf()) {
									toSelect = node;
								}
							}
						}
					}
					if (toSelect != null) {
						TreePath treePath = new TreePath(toSelect.getPath());
						tree.setSelectionPath(treePath);
						tree.scrollPathToVisible(treePath);
						return true;
					}
					tree.setModel(getModel(association));
				}
			} finally {
				suppressRestrictionSelection = false;
			}
		}
		return false;
	}

	/**
	 * Gets path from given association to root of association-tree.
	 * 
	 * @param association to start with
	 * @return path from given association to root of association-tree
	 */
	public List<Association> getPathToRoot(Association association) {
		List<Association> path = new ArrayList<Association>();

		for (DefaultMutableTreeNode node: treeNodes) {
			if (node.getUserObject() instanceof Association) {
				Association a = (Association) node.getUserObject();
				if (a.equals(association)) {
					for (TreeNode n: node.getPath()) {
						if (((DefaultMutableTreeNode) n).getUserObject() instanceof Association) {
							path.add((Association) ((DefaultMutableTreeNode) n).getUserObject());
						}			
					}
					break;
				}
			}
		}
		return path;
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
			// TODO: refactoring, move save-functionality into Model-class
			PrintWriter out = new PrintWriter(extractionModel);
			out.println("# subject; condition;  limit; restrictions");
			Table stable = dataModel.getTableByDisplayName((String) subjectTable.getSelectedItem());
			out.println(CsvFile.encodeCell("" + stable.getName()) + "; " + CsvFile.encodeCell(condition.getText()) + "; ; " + RestrictionModel.EMBEDDED);
			saveRestrictions(out);
			saveXmlMapping(out);
			out.println();
			out.println(CsvFile.BLOCK_INDICATOR + "export modus");
			out.println(scriptFormat);
			out.println();
			out.println(CsvFile.BLOCK_INDICATOR + "xml settings");
			out.println(CsvFile.encodeCell(dataModel.getXmlSettings().datePattern) + ";" + 
				    CsvFile.encodeCell(dataModel.getXmlSettings().timestampPattern) + ";" +
				    CsvFile.encodeCell(dataModel.getXmlSettings().rootTag));
			out.println(CsvFile.BLOCK_INDICATOR + "xml column mapping");
			for (Table table: dataModel.getTables()) {
				String xmlMapping = table.getXmlTemplate();
				if (xmlMapping != null) {
					out.println(CsvFile.encodeCell(table.getName()) + "; " + CsvFile.encodeCell(xmlMapping));
				}
			}
			saveFilters(out);
			out.println();
			out.println(CsvFile.BLOCK_INDICATOR + "version");
			out.println(Jailer.VERSION);
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
	 * Saves xml mappings.
	 * 
	 * @param out to save xml mappings into
	 */
	private void saveXmlMapping(PrintWriter out) {
		out.println();
		out.println(CsvFile.BLOCK_INDICATOR + "xml-mapping");
		for (Table table: dataModel.getTables()) {
			for (Association a: table.associations) {
				String name = a.getName();
				String tag = a.getAggregationTagName();
				String aggregation = a.getAggregationSchema().name();
				out.println(CsvFile.encodeCell(name) + ";" + CsvFile.encodeCell(tag) + ";" + CsvFile.encodeCell(aggregation));
			}
		}
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
				out.println(CsvFile.encodeCell(rd.from.getName()) + "; " + CsvFile.encodeCell(rd.to.getName()) + "; " + CsvFile.encodeCell(condition));
			} else {
				out.println(CsvFile.encodeCell(rd.name) + "; ; " + CsvFile.encodeCell(condition));
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
	 * Saves filters.
	 * 
	 * @param out to save filters into
	 */
	private void saveFilters(PrintWriter out) {
		out.println();
		out.println(CsvFile.BLOCK_INDICATOR + "filters");
		for (Table table: dataModel.getTables()) {
			for (Column c: table.getColumns()) {
				if (c.getFilterExpression() != null) {
					out.println(CsvFile.encodeCell(table.getName()) + ";" + CsvFile.encodeCell(c.name) + ";" + CsvFile.encodeCell(c.getFilterExpression()));
				}
			}
		}
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
     * Refreshes associations tree.
     */
	public void refresh(boolean restoreSelection, boolean fullGraphModelReset) {
		Association association = currentAssociation;
		tree.setModel(getModel());
		resetGraphEditor(fullGraphModelReset);
		if (restoreSelection) {
			select(association);
		}
	}
	
	/**
	 * Expands all node in associations tree.
	 */
	public void expand() {
		graphView.expandAll(false);
		expandPathsToVisibleTables();
	}

	/**
	 * Expands all associations with visible tables in associations tree.
	 */
	public void expandAllVisibleTables() {
		graphView.expandAll(true);
		expandPathsToVisibleTables();
	}
	
	/**
	 * Expands all associations with visible tables in associations tree.
	 */
	private void expandPathsToVisibleTables() {
		Set<Table> visibleTables = graphView.getVisibleTables();
		for (DefaultMutableTreeNode node: treeNodes) {
			if (node.getUserObject() instanceof Association) {
				if (!visibleTables.contains((((Association) node.getUserObject()).destination))) {
					continue;
				}
			}
			TreePath treePath = new TreePath(node.getPath());
			tree.expandPath(treePath);
		}
	}

	/**
	 * Removes all restrictions from extraction model.
	 */
	public void removeAllRestrictions(Table context) {
		for (Table table: dataModel.getTables()) {
			for (Association association: table.associations) {
				if (!association.isInsertDestinationBeforeSource()) {
					if (context == null || association.source.equals(context) || association.destination.equals(context)) {
						dataModel.getRestrictionModel().addRestriction(table, association, "", "GUI", true);
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
		graphView.resetExpandedState();
		extractionModelFrame.closureView.refresh();
	}
	
	/**
	 * Checks if {@link #removeAllRestrictions(Table)} is applicable.
	 */
	public boolean isRemovalOfAllRestrictionsApplicable(Table context) {
		for (Table table: dataModel.getTables()) {
			for (Association association: table.associations) {
				if (!association.isInsertDestinationBeforeSource()) {
					if (context == null || association.source.equals(context) || association.destination.equals(context)) {
						if (association.isRestricted()) {
							return true;
						}
					}
				}
			}
		}
		return false;
	}
	
	/**
	 * Add restriction for each non-dependency.
	 */
	public void ignoreAll(Table context) {
		for (Table table: dataModel.getTables()) {
			for (Association association: table.associations) {
				if (!association.isInsertDestinationBeforeSource()) {
					if (association.getName() != null && !"".equals(association.getName().trim())) {
						if (context == null || association.source.equals(context) || association.destination.equals(context)) {
							dataModel.getRestrictionModel().addRestriction(table, association, "false", "GUI", true);
						}
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
		graphView.resetExpandedState();
		extractionModelFrame.closureView.refresh();
	}

	/**
	 * Checks if {@link #ignoreAll(Table)} is applicable.
	 */
	public boolean isIgnoreAllApplicable(Table context) {
		for (Table table: dataModel.getTables()) {
			for (Association association: table.associations) {
				if (!association.isInsertDestinationBeforeSource()) {
					if (association.getName() != null && !"".equals(association.getName().trim())) {
						if (context == null || association.source.equals(context) || association.destination.equals(context)) {
							if (!association.isIgnored()) {
								return true;
							}
						}
					}
				}
			}
		}
		return false;
	}
	
	/**
	 * Zooms graphical view to fit.
	 */
	public void zoomToFit() {
		graphView.zoomToFit();
	}

    /**
     * Model for aggregation combobox.
     */
    private ComboBoxModel getAggregationModel() {
        DefaultComboBoxModel model = new DefaultComboBoxModel();
        for (AggregationSchema aggregation: AggregationSchema.values()) {
        	model.addElement(aggregation);
        }
        return model;
    }
    
    /**
     * Opens column mapper dialog for given table.
     * 
     * @param table the table
     */
	public void openColumnMapper(Table table) {
		if (columnMapperDialog.edit(dataModel, table)) {
			updateSketch();
			markDirty();
		}
	}

	/**
     * Marks the model as dirty (needs save)
     */
	public void markDirty() {
		if (!needsSave) {
			needsSave = true;
			extractionModelFrame.updateTitle(needsSave);
		}
	}

	/**
	 * Gets closure of current subject table.
	 * 
	 * @return closure of current subject table
	 */
	public Set<Table> getCurrentSubjectClosure() {
		if (dataModel == null || subject == null) {
			return Collections.emptySet();
		}
		if (currentSubjectClosure == null || dataModel.getVersion() != closureVersion) {
			currentSubjectClosure = subject.closure(true);
			closureVersion = dataModel.getVersion(); 
		}
		return currentSubjectClosure;
	}
	
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JComboBox aggregationCombobox;
    private javax.swing.JLabel associatedWith;
    private javax.swing.ButtonGroup buttonGroup1;
    private javax.swing.JTextField condition;
    private javax.swing.JLabel dependsOn;
    private javax.swing.JPanel editorPanel;
    public javax.swing.JButton exportButton;
    private javax.swing.JComboBox exportFormat;
    private javax.swing.JPanel graphContainer;
    private javax.swing.JLabel hasDependent;
    private javax.swing.JLabel ignored;
    private javax.swing.JPanel inspectorHolder;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JScrollPane jScrollPane3;
    private javax.swing.JScrollPane jScrollPane4;
    private javax.swing.JSplitPane jSplitPane1;
    private javax.swing.JPanel legende;
    private javax.swing.JButton mapColumns;
    private javax.swing.JButton openClosureView;
    private javax.swing.JButton openXmlSettings;
    private javax.swing.JTable restrictionsTable;
    private javax.swing.JComboBox rootTable;
    private javax.swing.JComboBox subjectTable;
    private javax.swing.JTextField tagField;
    private javax.swing.JTree tree;
    private javax.swing.JPanel xmlMappingPanel;
    private javax.swing.JTextArea xmlSketch;
    private javax.swing.JButton xmlTagApply;
    // End of variables declaration//GEN-END:variables

}
