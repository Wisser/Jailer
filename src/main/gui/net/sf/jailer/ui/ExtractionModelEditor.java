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
package net.sf.jailer.ui;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Cursor;
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
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.geom.Rectangle2D;
import java.io.File;
import java.io.PrintWriter;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Deque;
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
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.JTree;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
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

import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.fife.ui.rsyntaxtextarea.SyntaxConstants;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.datamodel.AggregationSchema;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.ModelElement;
import net.sf.jailer.datamodel.RestrictionDefinition;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.extractionmodel.ExtractionModel;
import net.sf.jailer.extractionmodel.ExtractionModel.AdditionalSubject;
import net.sf.jailer.subsetting.ScriptFormat;
import net.sf.jailer.ui.graphical_view.AssociationRenderer;
import net.sf.jailer.ui.graphical_view.GraphicalDataModelView;
import net.sf.jailer.ui.scrollmenu.JScrollPopupMenu;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.SqlUtil;

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
	ExtractionModel extractionModel;
	
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
	public GraphicalDataModelView graphView; 
	
	public ScriptFormat scriptFormat = ScriptFormat.SQL;

	private boolean isHorizontalLayout = false;
	
	/**
	 * The border browser.
	 */
	ClosureBorderDialog closureBorderView;

	/**
	 * Multi-line editor for subject condition.
	 */
	private ConditionEditor subjectConditionEditor;

	/**
	 * Multi-line editor for restriction condition.
	 */
	private ConditionEditor restrictionConditionEditor;
	
	/**
	 * The execution context.
	 */
	private final ExecutionContext executionContext;
	
	/** 
	 * Creates new form ModelTree.
	 *  
	 * @param extractionModelFile file containing the model
	 * @param extractionModelFrame the enclosing frame
	 */
	public ExtractionModelEditor(String extractionModelFile, ExtractionModelFrame extractionModelFrame, boolean horizontalLayout, String connectionState, String connectionStateToolTip, ExecutionContext executionContext) {
		this.executionContext = executionContext;
		this.extractionModelFrame = extractionModelFrame;
		this.extractionModelFile = extractionModelFile;
		extractionModelFrame.reload.setEnabled(false);
		ParameterSelector.ParametersGetter parametersGetter = new ParameterSelector.ParametersGetter() {
			@Override
			public Set<String> getParameters() {
				if (dataModel == null) {
					return new HashSet<String>();
				}
				return dataModel.getParameters(condition.getText(), extractionModel.additionalSubjects);
			}
		};
		columnMapperDialog = new ColumnMapperDialog(extractionModelFrame, parametersGetter);
		try {
			dataModel = new DataModel(executionContext);
		} catch (Exception e) {
			UIUtil.showException(this, "Error in Data Model", e);
			return;
		}
		try {
			if (extractionModelFile == null || !new File(extractionModelFile).exists()) {
				needsSave = extractionModelFile != null;
				dataModel = new DataModel(executionContext);
				extractionModel = new ExtractionModel(dataModel, executionContext);
				executionContext.getLayoutStorage().removeAll();
			} else {
				extractionModel = new ExtractionModel(extractionModelFile, new HashMap<String, String>(), new HashMap<String, String>(), executionContext);
				executionContext.getLayoutStorage().restore(extractionModelFile);
			}
			subject = extractionModel.subject;
			dataModel = extractionModel.dataModel;
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
		
		AutoCompletion.enable(rootTable);
		AutoCompletion.enable(subjectTable);
        GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        jPanel3.add(StringSearchPanel.createSearchButton(extractionModelFrame, subjectTable, "Find Table", null), gridBagConstraints);
		
		ItemListener aListener = new ItemListener() {
			@Override
			public void itemStateChanged(ItemEvent arg0) {
				Object o1 = rootTable.getSelectedItem();
				Object o2 = subjectTable.getSelectedItem();
				resetFocus.setEnabled(!(o1 == null && o2 == null || o1 != null && o1.equals(o2)));
			}
		};
				
		rootTable.addItemListener(aListener);
		subjectTable.addItemListener(aListener);
		
		closureBorderView = new ClosureBorderDialog(this.extractionModelFrame, true) {
			private static final long serialVersionUID = -7426280043553389753L;
			@Override
			protected Table getRoot() {
				return root;
			}
			@Override
			protected DataModel getDataModel() {
				return dataModel;
			}
			@Override
			protected void removeRestrictions(Collection<Association> associations) {
				ExtractionModelEditor.this.removeRestrictions(associations);
			}
			@Override
			protected void onSelect(Association association) {
				select(association);
			}
		};

		closureView = new ClosureView(this, rootTable) {
			@Override
			protected void repaintClosureView() {
				jSplitPane2.repaint();
				jSplitPane2.setDividerLocation(jSplitPane2.getDividerLocation() - 1);
				jSplitPane2.setDividerLocation(jSplitPane2.getDividerLocation() + 1);
			}
		};

		closureView.addTabComponent("Closure Border", closureBorderView.getContentPane());
		closureBorderView.dispose();
		
		Container cVContentPane = closureView.getContentPane();
		closureView.dispose();
		toolPanel.add(cVContentPane, java.awt.BorderLayout.CENTER);

		validate();
		
		tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
		tree.setExpandsSelectedPaths(true);
		restrictionEditor = new RestrictionEditor();
		
		if (isHorizontalLayout ) {
			graphView = new GraphicalDataModelView(dataModel, this, subject, true, 948, 379, executionContext);
		} else {
			graphView = new GraphicalDataModelView(dataModel, this, subject, true, 648, 579, executionContext);
		}
		graphContainer.add(graphView);

		layeredPane.removeAll();
		layeredPane.setLayer(graphContainer, 1);
		layeredPane.setLayer(toolBarPanel, 2);
		layeredPane.setLayer(messagePanel, 2);
		layeredPane.setLayout(new GridBagLayout());
		gridBagConstraints = new GridBagConstraints();
		gridBagConstraints.gridx = 0;
		gridBagConstraints.gridy = 0;
		gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
		gridBagConstraints.weightx = 1.0;
		gridBagConstraints.weighty = 1.0;
		layeredPane.add(graphContainer, gridBagConstraints);
		gridBagConstraints = new GridBagConstraints();
		gridBagConstraints.gridx = 0;
		gridBagConstraints.gridy = 0;
		gridBagConstraints.fill = java.awt.GridBagConstraints.NONE;
		gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
		gridBagConstraints.weightx = 0;
		gridBagConstraints.weighty = 0;
		layeredPane.add(toolBarPanel, gridBagConstraints);
		gridBagConstraints = new GridBagConstraints();
		gridBagConstraints.gridx = 0;
		gridBagConstraints.gridy = 0;
		gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
		gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHWEST;
		gridBagConstraints.weightx = 0;
		gridBagConstraints.weighty = 0;
		layeredPane.add(inspectorHolder, gridBagConstraints);
		gridBagConstraints = new GridBagConstraints();
		gridBagConstraints.gridx = 0;
		gridBagConstraints.gridy = 0;
		gridBagConstraints.fill = java.awt.GridBagConstraints.NONE;
		gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
		gridBagConstraints.weightx = 0;
		gridBagConstraints.weighty = 0;
		gridBagConstraints.insets = new Insets(2, 0, 0, 0);
		layeredPane.add(focusPanel, gridBagConstraints);
		gridBagConstraints = new GridBagConstraints();
		gridBagConstraints.gridx = 0;
		gridBagConstraints.gridy = 0;
		gridBagConstraints.fill = java.awt.GridBagConstraints.VERTICAL;
		gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
		gridBagConstraints.weightx = 0;
		gridBagConstraints.weighty = 0;
		layeredPane.add(rightBorderPanel, gridBagConstraints);
		
		focusLabelPanel.setBackground(new Color(255, 255, 255, 200));
		
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
		
		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 0;
		gridBagConstraints.gridy = 0;
		gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
		gridBagConstraints.weightx = 1.0;
		gridBagConstraints.weighty = 1.0;
		inspectorHolder.add(restrictionEditor, gridBagConstraints);
		// inspectorHolder.setMinimumSize(inspectorHolder.getPreferredSize());
		restrictionEditor.restriction.getDocument().addDocumentListener(new DocumentListener() {

			@Override
			public void changedUpdate(DocumentEvent e) {
				restrictionEditor.apply.setEnabled(true);
			}

			@Override
			public void insertUpdate(DocumentEvent e) {
				restrictionEditor.apply.setEnabled(true);
			}

			@Override
			public void removeUpdate(DocumentEvent e) {
				restrictionEditor.apply.setEnabled(true);
			}
			
		});
		tagField.getDocument().addDocumentListener(new DocumentListener() {

			@Override
			public void changedUpdate(DocumentEvent e) {
				xmlTagApply.setEnabled(true);
			}

			@Override
			public void insertUpdate(DocumentEvent e) {
				xmlTagApply.setEnabled(true);
			}

			@Override
			public void removeUpdate(DocumentEvent e) {
				xmlTagApply.setEnabled(true);
			}
			
		});
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
		restrictionEditor.restricted.addActionListener(new ActionListener() {
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
		if (extractionModel.subject != null) {
			subjectTable.setSelectedItem(null);
			subjectTable.setSelectedItem(dataModel.getDisplayName(extractionModel.subject));
		}
		String cond = extractionModel.getCondition();
		if (cond.equals("1=1")) {
			cond = "";
		}
		condition.setText(ConditionEditor.toSingleLine(cond));
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
		
		DefaultComboBoxModel formatComboBoxModel = new DefaultComboBoxModel(ScriptFormat.values());
		
		exportFormat.setModel(formatComboBoxModel);
		exportFormat.setRenderer(new DefaultListCellRenderer() {
			@Override
			public Component getListCellRendererComponent(JList list,
					Object value, int index, boolean isSelected,
					boolean cellHasFocus) {
				return super.getListCellRendererComponent(list, ((ScriptFormat) value).getDisplayName(), index, isSelected,
						cellHasFocus);
			}
			private static final long serialVersionUID = 2393022320508863837L;
		});
		exportFormat.setSelectedItem(scriptFormat);
		
		openXmlSettings.setVisible(ScriptFormat.XML.equals(scriptFormat));
		onExportModusChanged(null);
		setOrientation(horizontalLayout);
		connectivityState.setText(connectionState);
		connectivityState.setToolTipText(connectionStateToolTip);
		
		String modelname = "Data Model \"" + dataModel.getName() + "\"";
		String lastMod = dataModel.getLastModifiedAsString();
		if (lastMod.length() > 0) {
			lastMod = " (" + lastMod + ")";
		}
		modelName.setText(modelname);
		modelName.setToolTipText(modelname + lastMod);
		
		String modelpath = executionContext.getQualifiedDatamodelFolder();
		try {
			modelpath = new File(modelpath).getAbsolutePath();
		} catch (Throwable t) {
			// use default modelpath
		}
		modelpath += File.separator;
		modelPath.setToolTipText(modelpath);
		final int MAX_LENGTH = 50;
		if (modelpath.length() > MAX_LENGTH + 4) {
			modelpath = modelpath.substring(0, MAX_LENGTH/2) + "..." + modelpath.substring(modelpath.length() - MAX_LENGTH/2);
		}
		modelPath.setText(modelpath);
		
		subjectConditionEditor = new ConditionEditor(extractionModelFrame, parametersGetter, dataModel);
		subjectConditionEditor.setTitle("Subject condition");
		restrictionConditionEditor = new ConditionEditor(extractionModelFrame, parametersGetter, dataModel);
		restrictionConditionEditor.setTitle("Restriction");
		openSubjectConditionEditor.setIcon(conditionEditorIcon);
		openSubjectConditionEditor.setText(null);
		restrictionEditor.openRestrictionConditionEditor.setIcon(conditionEditorIcon);
		restrictionEditor.openRestrictionConditionEditor.setText(null);
		restrictionEditor.openRestrictionConditionEditor.addMouseListener(new java.awt.event.MouseAdapter() {
			@Override
			public void mouseReleased(MouseEvent e) {
				mouseClicked(e);
			}
			@Override
			public void mouseClicked(MouseEvent e) {
				if (currentAssociation != null && restrictionEditor.restriction.isEditable()) {
					String cond = restrictionConditionEditor.edit(restrictionEditor.restriction.getText(), "Table A", "A", currentAssociation.source, "Table B", "B", currentAssociation.destination, true);
					if (cond != null) {
						if (!restrictionEditor.restriction.getText().equals(ConditionEditor.toSingleLine(cond))) {
							restrictionEditor.restriction.setText(ConditionEditor.toSingleLine(cond));
							onApply(true);
						}
					}
					restrictionEditor.openRestrictionConditionEditor.setIcon(conditionEditorSelectedIcon);
				}
			}
			
			public void mouseEntered(java.awt.event.MouseEvent evt) {
				if (currentAssociation != null) {
					restrictionEditor.openRestrictionConditionEditor.setIcon(conditionEditorSelectedIcon);
				}
			}
			public void mouseExited(java.awt.event.MouseEvent evt) {
				restrictionEditor.openRestrictionConditionEditor.setIcon(conditionEditorIcon);
		   }
		});
		openSubjectConditionEditor.addMouseListener(new java.awt.event.MouseAdapter() {
			@Override
			public void mouseReleased(MouseEvent e) {
				mouseClicked(e);
			}
			public void mouseClicked(java.awt.event.MouseEvent evt) {
				String cond = subjectConditionEditor.edit(condition.getText(), "Subject", "T", subject, null, null, null, false);
				if (cond != null) {
					if (!condition.getText().equals(ConditionEditor.toSingleLine(cond))) {
						condition.setText(ConditionEditor.toSingleLine(cond));
						needsSave = true;
						ExtractionModelEditor.this.extractionModelFrame.updateTitle(needsSave);
					}
					openSubjectConditionEditor.setIcon(conditionEditorSelectedIcon);
				}
			}
			
			public void mouseEntered(java.awt.event.MouseEvent evt) {
				openSubjectConditionEditor.setIcon(conditionEditorSelectedIcon);
			}
			public void mouseExited(java.awt.event.MouseEvent evt) {
				openSubjectConditionEditor.setIcon(conditionEditorIcon);
		   }
		});
		
		leftButton.setIcon(leftIcon);
		leftButton.setPressedIcon(leftIconP);
		
		leftButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				undo();
			}
		});
		
		updateLeftButton();
		updateAdditionalSubjectsButton();
	}

	private void updateAdditionalSubjectsButton() {
		int n = extractionModel.additionalSubjects.size();
		additionalSubjectsButton.setText("Additional Subjects" + (n > 0? " (" + n + ")" : ""));
	}

	/**
	 * The "closure view" component.
	 */
	private ClosureView closureView;

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
//			editorPanel.add(inspectorHolder);
			((GridLayout) editorPanel.getLayout()).setVgap(0);
			((GridLayout) editorPanel.getLayout()).setRows(1);
			if (ScriptFormat.XML.equals(scriptFormat)) {
				editorPanel.add(xmlMappingPanel);
				((GridLayout) editorPanel.getLayout()).setColumns(3);
			} else {
				((GridLayout) editorPanel.getLayout()).setColumns(2);
			}
		} else {
			jSplitPane1.setOrientation(JSplitPane.HORIZONTAL_SPLIT);
			
			final JScrollPane scrollPanel = new JScrollPane() {
				@Override
				public Dimension getMinimumSize() {
					return new Dimension(Math.max(editorPanel.getMinimumSize().width, 380), editorPanel.getMinimumSize().height);
					// return editorPanel.getMinimumSize();
				}
				private static final long serialVersionUID = -947582621664272477L;
			};
			scrollPanel.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
			JPanel panel = new JPanel() {
				@Override
				public Dimension getMaximumSize() {
					return new Dimension(scrollPanel.getWidth() - 20, Math.max((int) (650.0 * (scriptFormat.equals(ScriptFormat.XML)? 1.33 : 1.0)), scrollPanel.getHeight() - 10));
				}
				@Override
				public Dimension getPreferredSize() {
					return getMaximumSize();
				}
				private static final long serialVersionUID = -5175513783835424396L;
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
			gridBagConstraints.weighty = 0;
			gridBagConstraints.fill = GridBagConstraints.BOTH;
			
			panel2.add(jPanel3, gridBagConstraints);
			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.gridx = 0;
			gridBagConstraints.gridy = 1;
			gridBagConstraints.weightx = 1;
			gridBagConstraints.weighty = 1;
			gridBagConstraints.fill = GridBagConstraints.BOTH;
			gridBagConstraints.insets = new Insets(4, 0, 0, 0);
			panel2.add(jPanel4, gridBagConstraints);
			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.gridx = 0;
			gridBagConstraints.gridy = 2;
			gridBagConstraints.weightx = 1;
			gridBagConstraints.weighty = 0;
			gridBagConstraints.fill = GridBagConstraints.BOTH;
			gridBagConstraints.insets = new Insets(4, 0, 0, 0);
//			panel2.add(inspectorHolder, gridBagConstraints);
			
			editorPanel.removeAll();
			editorPanel.add(panel2);
			((GridLayout) editorPanel.getLayout()).setVgap(1);
			((GridLayout) editorPanel.getLayout()).setColumns(1);
			if (ScriptFormat.XML.equals(scriptFormat)) {
				gridBagConstraints = new java.awt.GridBagConstraints();
				gridBagConstraints.gridx = 0;
				gridBagConstraints.gridy = 3;
				gridBagConstraints.weightx = 1;
				gridBagConstraints.weighty = 0.6;
				gridBagConstraints.fill = GridBagConstraints.BOTH;
				gridBagConstraints.insets = new Insets(4, 0, 0, 0);
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
	public void resetGraphEditor(boolean full, boolean storeLayout, boolean removeLayout, boolean expandRoot) {
		if (full) {
			graphView.close(storeLayout, removeLayout);
			graphContainer.remove(graphView);
			graphView = new GraphicalDataModelView(dataModel, this, root, expandRoot, graphView.display.getWidth(), graphView.display.getHeight(), executionContext);
			graphContainer.add(graphView);
		} else {
			graphView.resetExpandedState();
		}
		clearMessageBox();
		updateNeighborHolderPanel(null);
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
		return ConditionEditor.toMultiLine(condition.getText());
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

        jScrollPane4 = new javax.swing.JScrollPane();
        restrictionsTable = new javax.swing.JTable();
        jSplitPane1 = new javax.swing.JSplitPane();
        jpanel = new javax.swing.JPanel();
        jPanel12 = new javax.swing.JPanel();
        jSplitPane2 = new javax.swing.JSplitPane();
        layeredPane = new javax.swing.JLayeredPane();
        toolBarPanel = new javax.swing.JPanel();
        graphContainer = new javax.swing.JPanel();
        inspectorHolder = new javax.swing.JPanel();
        focusPanel = new javax.swing.JPanel();
        focusLabelPanel = new javax.swing.JPanel();
        jLabel9 = new javax.swing.JLabel();
        rootTable = new JComboBox();
        resetFocus = new javax.swing.JButton();
        leftButton = new javax.swing.JButton();
        rightBorderPanel = new javax.swing.JPanel();
        messagePanel = new javax.swing.JPanel();
        neighborHolderPanel = new javax.swing.JPanel();
        toolPanel = new javax.swing.JPanel();
        jPanel1 = new javax.swing.JPanel();
        editorPanel = new javax.swing.JPanel();
        jPanel3 = new javax.swing.JPanel();
        jLabel8 = new javax.swing.JLabel();
        jLabel6 = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        jPanel2 = new javax.swing.JPanel();
        additionalSubjectsButton = new javax.swing.JButton();
        jPanel7 = new javax.swing.JPanel();
        condition = new javax.swing.JTextField();
        jPanel8 = new javax.swing.JPanel();
        jLabel7 = new javax.swing.JLabel();
        openSubjectConditionEditor = new javax.swing.JLabel();
        subjectTable = new JComboBox();
        jPanel10 = new javax.swing.JPanel();
        exportFormat = new JComboBox();
        exportButton = new javax.swing.JButton();
        openXmlSettings = new javax.swing.JButton();
        jPanel4 = new javax.swing.JPanel();
        jPanel14 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        tree = new javax.swing.JTree();
        xmlMappingPanel = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        aggregationCombobox = new JComboBox();
        tagField = new javax.swing.JTextField();
        jPanel5 = new javax.swing.JPanel();
        xmlTagApply = new javax.swing.JButton();
        mapColumns = new javax.swing.JButton();
        sketchTabbedPane = new javax.swing.JTabbedPane();
        jPanel11 = new javax.swing.JPanel();
        legende1 = new javax.swing.JPanel();
        modelName = new javax.swing.JLabel();
        modelPath = new javax.swing.JLabel();
        legende = new javax.swing.JPanel();
        dependsOn = new javax.swing.JLabel();
        hasDependent = new javax.swing.JLabel();
        associatedWith = new javax.swing.JLabel();
        ignored = new javax.swing.JLabel();
        legende2 = new javax.swing.JPanel();
        connectivityState = new javax.swing.JLabel();

        restrictionsTable.setModel(restrictionTableModel());
        jScrollPane4.setViewportView(restrictionsTable);

        setLayout(new java.awt.GridBagLayout());

        jSplitPane1.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        jSplitPane1.setContinuousLayout(true);
        jSplitPane1.setOneTouchExpandable(true);

        jpanel.setLayout(new java.awt.BorderLayout());

        jPanel12.setLayout(new java.awt.BorderLayout());

        jSplitPane2.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        jSplitPane2.setResizeWeight(1.0);
        jSplitPane2.setOneTouchExpandable(true);

        toolBarPanel.setBackground(new java.awt.Color(255, 255, 255));
        toolBarPanel.setOpaque(false);
        toolBarPanel.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.LEFT, 0, 0));
        layeredPane.setLayer(toolBarPanel, javax.swing.JLayeredPane.PALETTE_LAYER);
        layeredPane.add(toolBarPanel);
        toolBarPanel.setBounds(0, 0, 0, 0);

        graphContainer.setLayout(new java.awt.BorderLayout());
        layeredPane.add(graphContainer);
        graphContainer.setBounds(0, 0, 0, 0);

        inspectorHolder.setOpaque(false);
        inspectorHolder.setLayout(new java.awt.GridBagLayout());
        layeredPane.setLayer(inspectorHolder, javax.swing.JLayeredPane.PALETTE_LAYER);
        layeredPane.add(inspectorHolder);
        inspectorHolder.setBounds(0, 0, 0, 0);

        focusPanel.setForeground(new java.awt.Color(86, 82, 125));
        focusPanel.setOpaque(false);
        focusPanel.setLayout(new java.awt.GridBagLayout());

        jLabel9.setText("   Focus on ");
        focusLabelPanel.add(jLabel9);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        focusPanel.add(focusLabelPanel, gridBagConstraints);

        rootTable.setMaximumRowCount(18);
        rootTable.setModel(getTableListModel());
        rootTable.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                rootTableItemStateChanged(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        focusPanel.add(rootTable, gridBagConstraints);

        resetFocus.setText("Reset");
        resetFocus.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                resetFocusActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        focusPanel.add(resetFocus, gridBagConstraints);

        leftButton.setToolTipText("back");
        leftButton.setBorderPainted(false);
        leftButton.setContentAreaFilled(false);
        leftButton.setHorizontalAlignment(javax.swing.SwingConstants.LEFT);
        leftButton.setIconTextGap(0);
        leftButton.setMargin(new java.awt.Insets(0, 0, 0, 0));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        focusPanel.add(leftButton, gridBagConstraints);

        layeredPane.setLayer(focusPanel, javax.swing.JLayeredPane.PALETTE_LAYER);
        layeredPane.add(focusPanel);
        focusPanel.setBounds(0, 0, 154, 29);

        rightBorderPanel.setOpaque(false);
        rightBorderPanel.setLayout(new java.awt.GridBagLayout());

        messagePanel.setOpaque(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        rightBorderPanel.add(messagePanel, gridBagConstraints);

        neighborHolderPanel.setOpaque(false);
        neighborHolderPanel.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 0, 0));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHEAST;
        gridBagConstraints.weighty = 1.0;
        rightBorderPanel.add(neighborHolderPanel, gridBagConstraints);

        layeredPane.setLayer(rightBorderPanel, javax.swing.JLayeredPane.PALETTE_LAYER);
        layeredPane.add(rightBorderPanel);
        rightBorderPanel.setBounds(0, 0, 10, 10);

        jSplitPane2.setTopComponent(layeredPane);

        toolPanel.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "Closure", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Tahoma", 0, 11), new java.awt.Color(82, 86, 125))); // NOI18N
        toolPanel.setLayout(new java.awt.BorderLayout());
        jSplitPane2.setBottomComponent(toolPanel);

        jPanel12.add(jSplitPane2, java.awt.BorderLayout.CENTER);

        jpanel.add(jPanel12, java.awt.BorderLayout.CENTER);

        jSplitPane1.setBottomComponent(jpanel);

        jPanel1.setLayout(new java.awt.GridBagLayout());

        editorPanel.setLayout(new java.awt.GridLayout(1, 4));

        jPanel3.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "Subject ", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Dialog", 1, 12), new java.awt.Color(86, 82, 125))); // NOI18N
        jPanel3.setLayout(new java.awt.GridBagLayout());

        jLabel8.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 9;
        gridBagConstraints.weighty = 1.0;
        jPanel3.add(jLabel8, gridBagConstraints);

        jLabel6.setText(" Export from ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel3.add(jLabel6, gridBagConstraints);

        jLabel4.setText(" To");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel3.add(jLabel4, gridBagConstraints);

        jLabel5.setText(" as T ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 0;
        jPanel3.add(jLabel5, gridBagConstraints);

        jPanel2.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 11;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        jPanel3.add(jPanel2, gridBagConstraints);

        additionalSubjectsButton.setText("Additional Subjects");
        additionalSubjectsButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                additionalSubjectsButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 7;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel3.add(additionalSubjectsButton, gridBagConstraints);

        jPanel7.setLayout(new java.awt.BorderLayout());

        condition.setText("jTextField1");
        condition.setToolTipText("SQL expression. Keep empty if you want to export all rows.");
        jPanel7.add(condition, java.awt.BorderLayout.CENTER);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel3.add(jPanel7, gridBagConstraints);

        jPanel8.setLayout(new java.awt.BorderLayout());

        jLabel7.setText(" Where ");
        jPanel8.add(jLabel7, java.awt.BorderLayout.WEST);

        openSubjectConditionEditor.setText("jLabel10");
        openSubjectConditionEditor.setToolTipText("open editor");
        jPanel8.add(openSubjectConditionEditor, java.awt.BorderLayout.EAST);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel3.add(jPanel8, gridBagConstraints);

        subjectTable.setMaximumRowCount(18);
        subjectTable.setModel(subjectListModel());
        subjectTable.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                subjectTableItemStateChanged(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel3.add(subjectTable, gridBagConstraints);

        jPanel10.setLayout(new java.awt.GridBagLayout());

        exportFormat.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        exportFormat.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                onExportModusChanged(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel10.add(exportFormat, gridBagConstraints);

        exportButton.setText("Export");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        jPanel10.add(exportButton, gridBagConstraints);

        openXmlSettings.setText("Setting");
        openXmlSettings.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                openXmlSettingsActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        jPanel10.add(openXmlSettings, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
        jPanel3.add(jPanel10, gridBagConstraints);

        editorPanel.add(jPanel3);

        jPanel4.setLayout(new java.awt.BorderLayout(0, 4));

        jPanel14.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "Association", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Dialog", 1, 12), new java.awt.Color(82, 86, 125))); // NOI18N
        jPanel14.setLayout(new java.awt.BorderLayout());

        jScrollPane1.setAutoscrolls(true);

        tree.setModel(getModel());
        tree.setAutoscrolls(true);
        tree.setCellRenderer(getTreeCellRenderer(tree.getCellRenderer()));
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

        jPanel14.add(jScrollPane1, java.awt.BorderLayout.CENTER);

        jPanel4.add(jPanel14, java.awt.BorderLayout.CENTER);

        editorPanel.add(jPanel4);

        xmlMappingPanel.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "XML Mapping ", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Dialog", 1, 12), new java.awt.Color(86, 82, 125))); // NOI18N
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
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 4);
        xmlMappingPanel.add(tagField, gridBagConstraints);

        jPanel5.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 5, 0));

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
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        xmlMappingPanel.add(jPanel5, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 11;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        xmlMappingPanel.add(sketchTabbedPane, gridBagConstraints);

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
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(jSplitPane1, gridBagConstraints);

        jPanel11.setLayout(new java.awt.GridBagLayout());

        legende1.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
        legende1.setLayout(new java.awt.GridBagLayout());

        modelName.setFont(new java.awt.Font("Dialog", 0, 12)); // NOI18N
        modelName.setText("Data Model \"Demo\"");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 12);
        legende1.add(modelName, gridBagConstraints);

        modelPath.setFont(new java.awt.Font("Dialog", 0, 12)); // NOI18N
        modelPath.setForeground(java.awt.Color.gray);
        modelPath.setText("/home/jailer/datamodel/");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        legende1.add(modelPath, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 2);
        jPanel11.add(legende1, gridBagConstraints);

        legende.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
        legende.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 0, 0));

        dependsOn.setFont(new java.awt.Font("Dialog", 0, 12)); // NOI18N
        dependsOn.setForeground(new java.awt.Color(170, 0, 0));
        dependsOn.setText(" depends on (has parent) ");
        legende.add(dependsOn);

        hasDependent.setFont(new java.awt.Font("Dialog", 0, 12)); // NOI18N
        hasDependent.setForeground(new java.awt.Color(0, 112, 0));
        hasDependent.setText("   has dependent (has child) ");
        legende.add(hasDependent);

        associatedWith.setFont(new java.awt.Font("Dialog", 0, 12)); // NOI18N
        associatedWith.setForeground(new java.awt.Color(0, 100, 255));
        associatedWith.setText("   associated with");
        legende.add(associatedWith);

        ignored.setFont(new java.awt.Font("Dialog", 0, 12)); // NOI18N
        ignored.setForeground(new java.awt.Color(153, 153, 153));
        ignored.setText("   disabled ");
        legende.add(ignored);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 2);
        jPanel11.add(legende, gridBagConstraints);

        legende2.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
        legende2.setLayout(new java.awt.GridBagLayout());

        connectivityState.setFont(new java.awt.Font("Dialog", 0, 12)); // NOI18N
        connectivityState.setText("offline");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 0);
        legende2.add(connectivityState, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 4);
        jPanel11.add(legende2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        add(jPanel11, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

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
						JPopupMenu popup = graphView.createPopupMenu(table, false);
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

	private boolean rootTableItemStateChangedSetRoot = true;
	private void rootTableItemStateChanged(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_rootTableItemStateChanged
		if (evt.getItem() != null) {
			if (evt.getStateChange() == ItemEvent.SELECTED) {
				Table table = dataModel.getTableByDisplayName(evt.getItem().toString());
				if (rootTableItemStateChangedSetRoot) {
					setRoot(table, true);
				}
			}
		}
	}//GEN-LAST:event_rootTableItemStateChanged

	/**
	 * Sets the root table
	 * 
	 * @param table the new root
	 */
	public void setRoot(Table table, boolean storeLayout) {
		if (table != null) {
			root = table;
			tree.setModel(getModel());
			resetGraphEditor(true, storeLayout, false, true);
			if (extractionModelFrame != null && closureBorderView != null) {
				closureBorderView.refresh();
			}
			if (extractionModelFrame != null && extractionModelFrame.restrictedDependenciesView != null) {
				extractionModelFrame.restrictedDependenciesView.refresh();
			}
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
			data[i++] = new Object[] { dataModel.getDisplayName(def.from), dataModel.getDisplayName(def.to), def.condition };
		}
		return new DefaultTableModel(data, new Object[] { "From", "To", "Condition" }) {
			public boolean isCellEditable(int row, int column) {
				return false;
			}
			private static final long serialVersionUID = 7309234765268573389L;
		};
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
//        if (subject != null) {
//	    	tableNames.add(0, dataModel.getDisplayName(subject));
//	        tableNames.add(1, "---");
//        }
		DefaultComboBoxModel model = new DefaultComboBoxModel(tableNames);
		return model;
	}

	/**
	 * Reacts on changes of selection of tree-view.
	 */
	private void treeValueChanged(javax.swing.event.TreeSelectionEvent evt) {//GEN-FIRST:event_treeValueChanged
		suppressRestrictionSelection = true;
		try {
			captureLayout();
			if (evt.getNewLeadSelectionPath() != null) {
				DefaultMutableTreeNode node = ((DefaultMutableTreeNode) evt.getNewLeadSelectionPath().getLastPathComponent());
				Object selection = node.getUserObject();
				if (selection instanceof Association) {
					initRestrictionEditor((Association) selection, node);
					updateNeighborHolderPanel(((Association) selection).destination);
				}
				if (selection instanceof Table) {
					initRestrictionEditor(null, null);
					updateNeighborHolderPanel((Table) selection);
				}
			} else {
				initRestrictionEditor(null, null);
				updateNeighborHolderPanel(null);
			}
		} finally {
			suppressRestrictionSelection = false;
			checkLayoutStack();
		}
		if (evt.getPath() != null && evt.getPath().getLastPathComponent() != null) {
			Object node = evt.getPath().getLastPathComponent();
			if (node instanceof DefaultMutableTreeNode) {
				DefaultMutableTreeNode n = (DefaultMutableTreeNode) node;
				if (n.getChildCount() == 0 && n.getUserObject() instanceof Association) {
					for (Association a: ((Association) n.getUserObject()).destination.associations) {
						n.add(new DefaultMutableTreeNode(a));
					}
					sort(n);
				}
			}
		}
	}//GEN-LAST:event_treeValueChanged

	Association currentAssociation;
	private DefaultMutableTreeNode currentNode;
	private String initialRestrictionCondition = null;

	/**
	 * Opens a drop-down box which allows the user to select columns for restriction definitions.
	 */
	private void openColumnDropDownBox(JLabel label, String alias, Table table) {
		JPopupMenu popup = new JScrollPopupMenu();
		List<String> columns = new ArrayList<String>();
		
		for (Column c: table.getColumns()) {
			columns.add(alias + "." + c.name);
		}
		columns.add("");
		columns.add(alias + ".$IS_SUBJECT");
		columns.add(alias + ".$DISTANCE");
		columns.add("$IN_DELETE_MODE");
		columns.add("NOT $IN_DELETE_MODE");
		
		for (final String c: columns) {
			if (c.equals("")) {
				popup.add(new JSeparator());
				continue;
			}
			JMenuItem m = new JMenuItem(c);
			m.addActionListener(new ActionListener () {
				public void actionPerformed(ActionEvent e) {
					if (restrictionEditor.restriction.isEnabled()) {
						if (restrictionEditor.restriction.isEditable()) {
							restrictionEditor.restriction.replaceSelection(c);
						}
					}
				}
			});
			popup.add(m);
		}
		UIUtil.fit(popup);
		popup.show(label, 0, label.getHeight());
	}

	private MouseAdapter maA, maB;
	
	/**
	 * Initializes the restriction editor.
	 * 
	 * @param association the association on which a restriction can be edited
	 * @param node selected tree node
	 */
	private void initRestrictionEditor(final Association association, DefaultMutableTreeNode node) {
		currentAssociation = association;
		currentNode = node;
		initialRestrictionCondition = null;
		initXmlMappingEditor(association);
		initRestrictedDependencyWarningField();
		restrictionEditor.columnsA.setText(null);
		restrictionEditor.columnsB.setText(null);
		restrictionEditor.columnsA.setIcon(dropDownIcon);
		restrictionEditor.columnsB.setIcon(dropDownIcon);
		restrictionEditor.columnsA.setEnabled(true);
		restrictionEditor.columnsB.setEnabled(true);
		restrictionEditor.openRestrictionConditionEditor.setEnabled(true);
		if (maA != null) {
			restrictionEditor.columnsA.removeMouseListener(maA);
		}
		if (maB != null) {
			restrictionEditor.columnsB.removeMouseListener(maB);
		}
		maA = maB = null;
		if (association != null) {
			restrictionEditor.columnsB.addMouseListener(maB = new java.awt.event.MouseAdapter() {
				public void mousePressed(java.awt.event.MouseEvent evt) {
					openColumnDropDownBox(restrictionEditor.columnsB, "B", association.destination);
				}
				
				public void mouseEntered(java.awt.event.MouseEvent evt) {
					restrictionEditor.columnsB.setEnabled(false);
				}
				public void mouseExited(java.awt.event.MouseEvent evt) {
					restrictionEditor.columnsB.setEnabled(true);
			   }
			});
			restrictionEditor.columnsA.addMouseListener(maA = new java.awt.event.MouseAdapter() {
				public void mousePressed(java.awt.event.MouseEvent evt) {
					openColumnDropDownBox(restrictionEditor.columnsA, "A", association.source);
				}
				
				public void mouseEntered(java.awt.event.MouseEvent evt) {
					restrictionEditor.columnsA.setEnabled(false);
				}
				public void mouseExited(java.awt.event.MouseEvent evt) {
					restrictionEditor.columnsA.setEnabled(true);
			   }
			});
		}
		if (association == null) {
			int l = currentRestrictionDefinitions == null? -1 : currentRestrictionDefinitions.size();
			if (l > 0) {
				restrictionsTable.removeRowSelectionInterval(0, l - 1);
			}
			restrictionEditor.setVisible(false);
		} else {
			restrictionEditor.setVisible(true);
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

			String type = "associated with";
			Color typeColor = associatedWith.getForeground();
			boolean editable = true;
			if (association.isInsertDestinationBeforeSource()) {
				type = "depends on (has parent)";
				typeColor = dependsOn.getForeground();
				// editable = false;
			} else if (association.isInsertSourceBeforeDestination()) {
				type = "has dependent (has child)";
				typeColor = hasDependent.getForeground();
			}
			String shortendName = association.getName();
			restrictionEditor.type.setToolTipText(shortendName);
			restrictionEditor.cardinality.setToolTipText(shortendName);
			restrictionEditor.restriction.setEditable(true);
			restrictionEditor.source.setText(dataModel.getDisplayName(association.source));
			restrictionEditor.destination.setText(dataModel.getDisplayName(association.destination));
			restrictionEditor.cardinality.setText(association.getCardinality() == null? "" : (association.getCardinality().toString()));
			restrictionEditor.type.setText(type);
			restrictionEditor.type.setForeground(typeColor);
			restrictionEditor.joinCondition.setText(association.getUnrestrictedJoinCondition());
			String restrictionCondition = association.getRestrictionCondition();
			if (restrictionCondition != null && restrictionCondition.startsWith("(") && restrictionCondition.endsWith(")")) {
				restrictionCondition = restrictionCondition.substring(1, restrictionCondition.length() - 1);
			}
			initialRestrictionCondition = association.isIgnored()? null : restrictionCondition;
			restrictionEditor.restriction.setText(restrictionCondition == null? "" : ConditionEditor.toSingleLine(restrictionCondition));
			if (association.isIgnored()) {
				restrictionEditor.ignore.getModel().setSelected(true);
			} else {
				restrictionEditor.restricted.getModel().setSelected(true);
			}
			restrictionEditor.restriction.setEditable(editable && !association.isIgnored());
			restrictionEditor.restriction.setEnabled(editable && !association.isIgnored());
			// restrictionEditor.openRestrictionConditionEditor.setVisible(editable && !association.isIgnored());
			restrictionEditor.ignore.setEnabled(editable);
			// restrictionEditor.apply.setEnabled(editable);
			String joinCondition = association.getUnrestrictedJoinCondition();
			if (association.reversed) {
				joinCondition = SqlUtil.reversRestrictionCondition(joinCondition);
			}
			boolean singleCondition = true;
			String shortJC = joinCondition;
			if (shortJC.length() > 50) {
				shortJC = shortJC.substring(0, 50) + "...";
			}
			restrictionEditor.joinCondition.setText(shortJC);
			restrictionEditor.joinCondition.setToolTipText(joinCondition);
		}
		graphView.setSelection(association);
		restrictionEditor.apply.setEnabled(false);
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
		xmlTagApply.setEnabled(false);
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
					condition = ConditionEditor.toMultiLine(restrictionEditor.restriction.getText()).trim();
				}
			}
			dataModel.getRestrictionModel().addRestriction(currentAssociation.source, currentAssociation, condition, "GUI", true, new HashMap<String, String>());
			tree.repaint();
			graphView.display.invalidate();
			restrictionsTable.setModel(restrictionTableModel());
			String saveInitialRestrictionCondition = initialRestrictionCondition;
			initRestrictionEditor(currentAssociation, currentNode);
			initialRestrictionCondition = saveInitialRestrictionCondition;
			closureView.refresh();
			closureBorderView.refresh();
			extractionModelFrame.restrictedDependenciesView.refresh();
			tree.grabFocus();
		}
	}
	
	public void removeRestrictions(Collection<Association> associations) {
		for (Association association: associations) {
			if (!needsSave) {
				needsSave = true;
				extractionModelFrame.updateTitle(needsSave);
			}
			String condition = "";
			dataModel.getRestrictionModel().addRestriction(association.source, association, condition, "GUI", true, new HashMap<String, String>());
			graphView.setSelection(association);
		}
		tree.repaint();
		graphView.display.invalidate();
		restrictionsTable.setModel(restrictionTableModel());
		closureView.refresh();
		closureBorderView.refresh();
		extractionModelFrame.restrictedDependenciesView.refresh();
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

		while (!agenda.isEmpty()) {
			Association a = agenda.remove(0);
			if (toNode.get(a) == null) {
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

		if (treeModel != null) {
			treeModel.setRoot(root);
		} else {
			treeModel = new DefaultTreeModel(root);
		}
		return treeModel;
	}

	/**
	 * Updates the XML sketch component.
	 */
	private void updateSketch() {
		try {
			sketchTabbedPane.removeAll();

			if (currentAssociation != null) {
				addSketchTab(currentAssociation.source);
				if (currentAssociation.source != currentAssociation.destination) {
					addSketchTab(currentAssociation.destination);
				}
				sketchTabbedPane.setSelectedIndex(0);
			} else {
				if (root != null) {
					addSketchTab(root);
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Adds a tab to the sketch-tabbedpane for a given table. 
	 * 
	 * @param table the table
	 */
	private void addSketchTab(Table table) throws Exception {
		JScrollPane tab = new JScrollPane();
		RSyntaxTextArea xmlSketch = new RSyntaxTextArea();
		
		xmlSketch.setEditable(false);
		tab.setViewportView(xmlSketch);
		xmlSketch.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_SQL);
		xmlSketch.setCodeFoldingEnabled(true);

		String tabName = null;
		String sketch = "";
		tabName = dataModel.getDisplayName(table);
		sketch = XmlSketchBuilder.buildSketch(table, 1);
		xmlSketch.setText(sketch);
		xmlSketch.setCaretPosition(0);
		sketchTabbedPane.addTab(tabName, tab);
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
			  xmlTagApply.setEnabled(false);
		  }
	  }//GEN-LAST:event_xmlMappingApplyButtonActionPerformed

	private void onExportModusChanged(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_onExportModusChanged
		ScriptFormat f = (ScriptFormat) exportFormat.getSelectedItem();
		if (f != null) {
			scriptFormat = f;
			dataModel.setExportModus(scriptFormat.toString());
		}
		setOrientation(isHorizontalLayout);
		openXmlSettings.setVisible(ScriptFormat.XML.equals(scriptFormat));
		validate();
	}//GEN-LAST:event_onExportModusChanged

	private void subjectTableItemStateChanged(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_subjectTableItemStateChanged
		Object selectedItem = subjectTable.getSelectedItem();
		if (selectedItem instanceof String) {
			if (dataModel.getTableByDisplayName(selectedItem.toString()) != null) {
				currentSubjectClosure = null; // force re-calculation
				subject = dataModel.getTableByDisplayName(selectedItem
						.toString());
			}
		}
		rootTable.setModel(getTableListModel());
		rootTable.setSelectedItem(null);
		rootTable.setSelectedItem(selectedItem);
		if (!needsSave) {
			needsSave = true;
			extractionModelFrame.updateTitle(needsSave);
		}
	}//GEN-LAST:event_subjectTableItemStateChanged

	private void resetFocusActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_resetFocusActionPerformed
		rootTable.setSelectedItem(subjectTable.getSelectedItem());
	}//GEN-LAST:event_resetFocusActionPerformed

	private void additionalSubjectsButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_additionalSubjectsButtonActionPerformed
		AdditionalSubjectsDialog additionalSubjectsDialog;
		try {
			setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
			additionalSubjectsDialog = new AdditionalSubjectsDialog(extractionModelFrame, extractionModel, subject, condition.getText());
		} finally {
			setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		}
		if (additionalSubjectsDialog.edit()) {
			needsSave = true;
			updateAdditionalSubjectsButton();
			extractionModelFrame.updateTitle(needsSave);
		}
	}//GEN-LAST:event_additionalSubjectsButtonActionPerformed

	/**
	 * Renderer for the tree-view.
	 */
	private TreeCellRenderer getTreeCellRenderer(TreeCellRenderer treeCellRenderer) {
		DefaultTreeCellRenderer r = new DefaultTreeCellRenderer() {
			public Component getTreeCellRendererComponent(JTree tree,
					Object value, boolean selected, boolean expanded,
					boolean leaf, int row, boolean hasFocus) {
				String text = "";
				if (value instanceof DefaultMutableTreeNode) {
					DefaultMutableTreeNode node = (DefaultMutableTreeNode) value;
					if (node.getUserObject() instanceof Table) {
						text = dataModel.getDisplayName(((Table) node.getUserObject()));
//						setTextSelectionColor(Color.BLACK);
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
//							setTextSelectionColor(ignored.getForeground());
							setTextNonSelectionColor(ignored.getForeground());
						} else if (association.isInsertDestinationBeforeSource()) {
//							setTextSelectionColor(dependsOn.getForeground());
							setTextNonSelectionColor(dependsOn.getForeground());
						} else if (association.isInsertSourceBeforeDestination()) {
//							setTextSelectionColor(hasDependent.getForeground());
							setTextNonSelectionColor(hasDependent.getForeground());
						} else {
//							setTextSelectionColor(associatedWith.getForeground());
							setTextNonSelectionColor(associatedWith.getForeground());
						}
						return wrapTreeNode(super.getTreeCellRendererComponent(tree, text, selected, expanded, leaf, row, hasFocus), selected, association);
					}
				}
				return super.getTreeCellRendererComponent(tree, text, selected, expanded, leaf, row, hasFocus);
			}
			private static final long serialVersionUID = 2657584557290860355L;
		};
		r.setOpenIcon(null);
		r.setLeafIcon(null);
		r.setClosedIcon(null);
		return r;
	}

	/**
	 * Adds an arrow that indicates the type of the reversal association to the render.
	 */
	private Component wrapTreeNode(Component component, boolean selected, final Association association) {
		java.awt.GridBagConstraints gridBagConstraints;

		if (!(component instanceof JLabel)) {
			return component;
		}
		
		JPanel panel = new javax.swing.JPanel();
		panel.setOpaque(false);
//        ((JLabel) component).setOpaque(false);
//        ((JLabel) component).setBackground(selected? Color.BLUE : null);
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

			private static final long serialVersionUID = -8515778652672162975L;
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
		try {
			clearMessageBox();
			captureLayout();
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
		} finally {
			checkLayoutStack();
		}
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
				captureLayout();
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
						setRoot(restrictionDefinition.from, true);
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
				checkLayoutStack();
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
			captureLayout();
			try {
				clearMessageBox();
				updateNeighborHolderPanel(association == null? null : association.destination);
				suppressRestrictionSelection = true;
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
				checkLayoutStack();
			}
		}
		return false;
	}
	
	private void updateNeighborHolderPanel(Table table) {
		neighborHolderPanel.removeAll();
		if (table != null) {
			NeighborhoodPanel neighborhoodPanel = new NeighborhoodPanel(dataModel, table, graphView, extractionModelFrame.hideIgnored(), dependsOn.getForeground(), hasDependent.getForeground(), associatedWith.getForeground(), ignored.getForeground());
			neighborHolderPanel.add(neighborhoodPanel);
		}
		layeredPane.validate();
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
			String newFile = UIUtil.choseFile(extractionModelFile == null? null : new File(extractionModelFile), startDir, (reason == null? "" : (reason + " - ")) + "Save Extraction Model", ".jm", this, true, false, false);
			if (newFile == null) {
				return false;
			}
			extractionModelFile = newFile;
			extractionModelFrame.reload.setEnabled(extractionModelFile != null && needsSave);
		}
		String file = extractionModelFile;
		graphView.storeLayout();
		if (save(file)) {
			needsSave = false;
			extractionModelFrame.updateTitle(needsSave);
			return true;
		}
		return false;
	}

	public boolean save(String fileName) {
		try {
			Table stable = dataModel.getTableByDisplayName((String) subjectTable.getSelectedItem());
			if (stable == null) {
				return true;
			}
			String currentModelSubfolder = DataModelManager.getCurrentModelSubfolder(executionContext);
			dataModel.save(fileName, stable, ConditionEditor.toMultiLine(condition.getText()), scriptFormat, currentRestrictionDefinitions, null, extractionModel.additionalSubjects, currentModelSubfolder);
		} catch (Exception e) {
			UIUtil.showException(this, "Could not save " + new File(fileName).getName(), e);
			return false;
		}
		return true;
	}

	/**
	 * Saves restrictions only.
	 * 
	 * @param out to save restrictions into
	 * @param restrictionDefinitions 
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
	public void refresh(boolean restoreSelection, boolean fullGraphModelReset, boolean storeLayout, boolean removeLayout) {
		Association association = currentAssociation;
		tree.setModel(getModel());
		resetGraphEditor(fullGraphModelReset, storeLayout, removeLayout, true);
		if (restoreSelection) {
			select(association);
		}
	}
	
	/**
	 * Expands all node in associations tree.
	 */
	public void expand() {
		graphView.expandAll(false, true);
		expandPathsToVisibleTables();
	}

	/**
	 * Expands all associations with visible tables in associations tree.
	 */
	public void expandAllVisibleTables() {
		graphView.expandAll(true, true);
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
//				if (!association.isInsertDestinationBeforeSource()) {
					if (context == null || association.source.equals(context) || association.destination.equals(context)) {
						dataModel.getRestrictionModel().addRestriction(table, association, "", "GUI", true, new HashMap<String, String>());
					}
//				}
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
		closureView.refresh();
		closureBorderView.refresh();
		extractionModelFrame.restrictedDependenciesView.refresh();
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
							dataModel.getRestrictionModel().addRestriction(table, association, "false", "GUI", true, new HashMap<String, String>());
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
		closureView.refresh();
		closureBorderView.refresh();
		extractionModelFrame.restrictedDependenciesView.refresh();
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
	 * Add restriction.
	 */
	public void ignorAssociation(Association association) {
		dataModel.getRestrictionModel().addRestriction(association.source, association, "false", "GUI", true, new HashMap<String, String>());
		if (!needsSave) {
			needsSave = true;
			extractionModelFrame.updateTitle(needsSave);
		}
		tree.repaint();
		restrictionsTable.setModel(restrictionTableModel());
		initRestrictionEditor(currentAssociation, currentNode);
		graphView.resetExpandedState();
		closureView.refresh();
		closureBorderView.refresh();
		extractionModelFrame.restrictedDependenciesView.refresh();
	}

	/**
	 * Removes restrictions.
	 */
	public void removeRestriction(Association association) {
		dataModel.getRestrictionModel().addRestriction(association.source, association, "", "GUI", true, new HashMap<String, String>());
		if (!needsSave) {
			needsSave = true;
			extractionModelFrame.updateTitle(needsSave);
		}
		tree.repaint();
		restrictionsTable.setModel(restrictionTableModel());
		initRestrictionEditor(currentAssociation, currentNode);
		graphView.resetExpandedState();
		closureView.refresh();
		closureBorderView.refresh();
		extractionModelFrame.restrictedDependenciesView.refresh();
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

	
	private static final int MAX_UNDOSTACKSIZE = 20;

	private static class Layout {
		Table root;
		Rectangle2D bounds;
		Map<String, double[]> positions = new HashMap<String,double[]>();
	};
	
	private Deque<Layout> undoStack = new ArrayDeque<Layout>();
	
	private int captureLevel = 0;
	
	public synchronized void incCaptureLevel() {
		++captureLevel;
	}
		
	public synchronized void decCaptureLevel() {
		--captureLevel;
	}
		
	public synchronized void captureLayout() {
		try {
			if (graphView != null && captureLevel == 0) {
				Layout layout = new Layout();
				layout.root = root;
//				layout.bounds = graphView.getDisplayBounds();
				executionContext.getLayoutStorage().setTempStorage(layout.positions);
				graphView.storeLayout();
				
				if (!undoStack.isEmpty()) {
					if (undoStack.peek().positions.keySet().equals(layout.positions.keySet())) {
						return;
					}
				}
				
				undoStack.push(layout);
				if (undoStack.size() > MAX_UNDOSTACKSIZE) {
					undoStack.removeLast();
				}
				updateLeftButton();
			}
		} finally {
			if (captureLevel == 0) {
				executionContext.getLayoutStorage().setTempStorage(null);
			}
			++captureLevel;
		}
	}
	
	public synchronized void checkLayoutStack() {
		if (captureLevel > 0) {
			--captureLevel;
		}
		if (captureLevel == 0) {
			if (!undoStack.isEmpty()) {
				if (undoStack.peek().positions.size() > 1400 || undoStack.peek().positions.keySet().equals(graphView.visibleItems())) {
					undoStack.pop();
					updateLeftButton();
				}
			}
		}
	}
	
	public void undo() {
		if (!undoStack.isEmpty()) {
			Layout layout = undoStack.pop();
			try {
				++captureLevel;
				executionContext.getLayoutStorage().setTempStorage(layout.positions);
				Table table = layout.root;
				if (table != null) {
					root = table;
					tree.setModel(getModel());
					resetGraphEditor(true, false, false, false);
					if (extractionModelFrame != null && closureBorderView != null) {
						closureBorderView.refresh();
					}
					if (extractionModelFrame != null && extractionModelFrame.restrictedDependenciesView != null) {
						extractionModelFrame.restrictedDependenciesView.refresh();
					}
				}
				rootTableItemStateChangedSetRoot = false;
				rootTable.setSelectedItem(dataModel.getDisplayName(layout.root));
			} finally {
				--captureLevel;
				rootTableItemStateChangedSetRoot = true;
				executionContext.getLayoutStorage().setTempStorage(null);
			}
		}
		updateLeftButton();
	}

	private void updateLeftButton() {
		leftButton.setVisible(!undoStack.isEmpty());
	}

	/**
	 * @param table a table
	 * @return <code>true</code> if table is an additional subject
	 */
	public boolean isAdditionalSubject(Table table) {
		for (AdditionalSubject as: extractionModel.additionalSubjects) {
			if (as.getSubject().equals(table)) {
				return true;
			}
		}
		return false;
	}

	private JComponent messageBox;
	
	public void addMessageBox(JComponent messageBox) {
		clearMessageBox();
		this.messageBox = messageBox;
		messagePanel.add(this.messageBox);
		messagePanel.revalidate();
	}

	public void clearMessageBox() {
		if (messageBox != null) {
			messagePanel.remove(messageBox);
			messageBox = null;
		}
	}
	
	private List<JMenuItem> additionalPopupMenuItems = new ArrayList<JMenuItem>();

	/**
	 * Adds additional popup menu items.
	 * 
	 * @param items additional popup menu items
	 */
	public void addAdditionalPopupMenuItems(List<JMenuItem> items) {
		additionalPopupMenuItems.addAll(items);
	}
	
	/**
	 * Gets additional popup menu items.
	 * 
	 * @return additional popup menu items
	 */
	public List<JMenuItem> getAdditionalPopupMenuItems() {
		return additionalPopupMenuItems;
	}

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton additionalSubjectsButton;
    private JComboBox aggregationCombobox;
    private javax.swing.JLabel associatedWith;
    javax.swing.JTextField condition;
    public javax.swing.JLabel connectivityState;
    private javax.swing.JLabel dependsOn;
    private javax.swing.JPanel editorPanel;
    public javax.swing.JButton exportButton;
    private JComboBox exportFormat;
    private javax.swing.JPanel focusLabelPanel;
    javax.swing.JPanel focusPanel;
    private javax.swing.JPanel graphContainer;
    private javax.swing.JLabel hasDependent;
    private javax.swing.JLabel ignored;
    javax.swing.JPanel inspectorHolder;
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
    private javax.swing.JPanel jPanel10;
    private javax.swing.JPanel jPanel11;
    private javax.swing.JPanel jPanel12;
    private javax.swing.JPanel jPanel14;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanel7;
    private javax.swing.JPanel jPanel8;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane4;
    private javax.swing.JSplitPane jSplitPane1;
    private javax.swing.JSplitPane jSplitPane2;
    private javax.swing.JPanel jpanel;
    javax.swing.JLayeredPane layeredPane;
    private javax.swing.JButton leftButton;
    private javax.swing.JPanel legende;
    private javax.swing.JPanel legende1;
    private javax.swing.JPanel legende2;
    private javax.swing.JButton mapColumns;
    private javax.swing.JPanel messagePanel;
    private javax.swing.JLabel modelName;
    private javax.swing.JLabel modelPath;
    public javax.swing.JPanel neighborHolderPanel;
    private javax.swing.JLabel openSubjectConditionEditor;
    private javax.swing.JButton openXmlSettings;
    private javax.swing.JButton resetFocus;
    private javax.swing.JTable restrictionsTable;
    private javax.swing.JPanel rightBorderPanel;
    JComboBox rootTable;
    private javax.swing.JTabbedPane sketchTabbedPane;
    JComboBox subjectTable;
    private javax.swing.JTextField tagField;
    javax.swing.JPanel toolBarPanel;
    private javax.swing.JPanel toolPanel;
    private javax.swing.JTree tree;
    private javax.swing.JPanel xmlMappingPanel;
    private javax.swing.JButton xmlTagApply;
    // End of variables declaration//GEN-END:variables
	
	private Icon dropDownIcon;
	private Icon conditionEditorIcon;
	private Icon conditionEditorSelectedIcon;
	private Icon leftIcon;
	private Icon leftIconP;
	{
		String dir = "/net/sf/jailer/ui/resource";
		
		// load images
		try {
			dropDownIcon = new ImageIcon(getClass().getResource(dir + "/dropdown.png"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		try {
			conditionEditorIcon = new ImageIcon(getClass().getResource(dir + "/edit.png"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		try {
			conditionEditorSelectedIcon = new ImageIcon(getClass().getResource(dir + "/edit_s.png"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		try {
			leftIconP = new ImageIcon(getClass().getResource(dir + "/leftp.png"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		try {
			leftIcon = new ImageIcon(getClass().getResource(dir + "/left.png"));
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private static final long serialVersionUID = -5640822484296649670L;

}
