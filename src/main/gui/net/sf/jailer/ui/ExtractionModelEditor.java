/*
 * Copyright 2007 - 2022 Ralf Wisser.
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
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowFocusListener;
import java.awt.event.WindowListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.IOException;
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
import java.util.function.Consumer;
import java.util.function.Supplier;

import javax.swing.BorderFactory;
import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListCellRenderer;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.JToggleButton;
import javax.swing.JTree;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;
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
import net.sf.jailer.database.BasicDataSource;
import net.sf.jailer.datamodel.AggregationSchema;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.ModelElement;
import net.sf.jailer.datamodel.PrimaryKey;
import net.sf.jailer.datamodel.PrimaryKeyFactory;
import net.sf.jailer.datamodel.RestrictionDefinition;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.extractionmodel.ExtractionModel;
import net.sf.jailer.extractionmodel.ExtractionModel.AdditionalSubject;
import net.sf.jailer.extractionmodel.SubjectLimitDefinition;
import net.sf.jailer.restrictionmodel.RestrictionModel;
import net.sf.jailer.subsetting.ScriptFormat;
import net.sf.jailer.ui.StringSearchPanel.StringSearchDialog;
import net.sf.jailer.ui.UIUtil.PLAF;
import net.sf.jailer.ui.commandline.CommandLineInstance;
import net.sf.jailer.ui.databrowser.BrowserContentCellEditor;
import net.sf.jailer.ui.databrowser.whereconditioneditor.WhereConditionEditorPanel;
import net.sf.jailer.ui.graphical_view.AssociationRenderer;
import net.sf.jailer.ui.graphical_view.GraphicalDataModelView;
import net.sf.jailer.ui.scrollmenu.JScrollPopupMenu;
import net.sf.jailer.ui.syntaxtextarea.DataModelBasedSQLCompletionProvider;
import net.sf.jailer.ui.syntaxtextarea.RSyntaxTextAreaWithSQLSyntaxStyle;
import net.sf.jailer.ui.syntaxtextarea.SQLCompletionProvider;
import net.sf.jailer.ui.undo.CompensationAction;
import net.sf.jailer.ui.undo.UndoManager;
import net.sf.jailer.ui.util.LightBorderSmallButton;
import net.sf.jailer.ui.util.SmallButton;
import net.sf.jailer.ui.util.UISettings;
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
	 * The undo mananger.
	 */
	private final UndoManager undoManager;

	/**
	 * The border browser.
	 */
	ClosureBorderDialog closureBorderView;

	/**
	 * Pending Decisions Panel.
	 */
	PendingDecisionsPanel pendingDecisionsPanel;

	/**
	 * Multi-line editor for subject condition.
	 */
	private NonModalConditionEditor subjectConditionEditor;

	/**
	 * Multi-line editor for restriction condition.
	 */
	private NonModalConditionEditor restrictionConditionEditor;

	/**
	 * The execution context.
	 */
	private final ExecutionContext executionContext;

	/**
	 * Creates new form ModelTree.
	 *
	 * @param extractionModelFile file containing the model
	 * @param extractionModelFrame the enclosing frame
	 * @param dbmsLogo 
	 */
	@SuppressWarnings("unchecked")
	public ExtractionModelEditor(String extractionModelFile, final ExtractionModelFrame extractionModelFrame, boolean horizontalLayout, String connectionState, String connectionStateToolTip, ImageIcon dbmsLogo, ExecutionContext executionContext) throws IOException {
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
		boolean isNew;
		if (extractionModelFile == null || !new File(extractionModelFile).exists()) {
			needsSave = extractionModelFile != null;
			dataModel = new DataModel(null, null, new HashMap<String, String>(), null, new PrimaryKeyFactory(executionContext), executionContext, CommandLineInstance.getInstance().datamodelFolder != null, null);
			extractionModel = new ExtractionModel(dataModel, executionContext);
			executionContext.getLayoutStorage().removeAll();
			isNew = true;
		} else {
			extractionModel = new ExtractionModel(extractionModelFile, new HashMap<String, String>(), new HashMap<String, String>(), executionContext);
			executionContext.getLayoutStorage().restore(extractionModelFile);
			isNew = false;
			UISettings.addRecentFile(new File(extractionModelFile));
		}
		subject = extractionModel.subject;
		dataModel = extractionModel.dataModel;
		if (dataModel != null) {
			UISettings.dmStats(dataModel);
		}
		if (subject == null && dataModel != null && !dataModel.getTables().isEmpty()) {
			subject = dataModel.getTables().iterator().next();
			needsSave = true;
			if (!isNew && extractionModel.subject == null && extractionModel.getSubjectTableName() != null) {
				UIUtil.invokeLater(new Runnable() {
					@Override
					public void run() {
						JOptionPane.showMessageDialog(extractionModelFrame,
								"Subject table \"" + extractionModel.getSubjectTableName() + "\" does not exist.\n");
					}
				});
			}
		}

		boolean saveNeedsSave = needsSave;
		initComponents();
		subjectTable = new JComboBox2() {
			@Override
			public Dimension getPreferredSize() {
				Dimension size = super.getPreferredSize();
				return limitedSize(size);
			}
			@Override
			public Dimension getMinimumSize() {
				Dimension size = super.getMinimumSize();
				return limitedSize(size);
			}
			private Dimension limitedSize(Dimension size) {
				return new Dimension(Math.min(size.width, 100), size.height);
			}
		};
        subjectTable.setMaximumRowCount(18);
        subjectTable.setModel(subjectListModel());
        subjectTable.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
            	Object selectedItem = subjectTable.getSelectedItem();
        		Table newSubject = null;
        		if (selectedItem instanceof String) {
        			if (dataModel.getTableByDisplayName(selectedItem.toString()) != null) {
        				newSubject = dataModel.getTableByDisplayName(selectedItem
        						.toString());
        			}
        		}
        		changeSubject(newSubject);
            }
        });
        GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        jPanel3.add(subjectTable, gridBagConstraints);

		connectivityState.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				extractionModelFrame.reconnectToDB(null);
			}
		});
		
		assocStatsLabel.setText("");
		
		gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.NONE;
        JButton button = createWhereConEditorButton(() -> getSubject(), () -> condition.getText(), s -> {
        	condition.setText(UIUtil.toSingleLineSQL(s));
        }, true, "T", null);
		jPanel3.add(button, gridBagConstraints);
		
		UIUtil.setLeadingComponent(condition, button);
		
		button.setEnabled(subjectTable.getSelectedIndex() >= 0);
		subjectTable.getModel().addListDataListener(new ListDataListener() {
			@Override
			public void intervalRemoved(ListDataEvent arg0) {
				button.setEnabled(subjectTable.getSelectedIndex() >= 0);
				}
			@Override
			public void intervalAdded(ListDataEvent arg0) {
				button.setEnabled(subjectTable.getSelectedIndex() >= 0);
			}
			@Override
			public void contentsChanged(ListDataEvent arg0) {
				button.setEnabled(subjectTable.getSelectedIndex() >= 0);
			}
		});
		subjectTable.addPropertyChangeListener("model", new PropertyChangeListener() {
			@Override
			public void propertyChange(PropertyChangeEvent arg0) {
				button.setEnabled(subjectTable.getSelectedIndex() >= 0);
			}
		});

		exportButton.setIcon(runIcon);
		limitLabel.setText(SubjectLimitEditor.subjectLimitDefinitionRender(extractionModel.subjectLimitDefinition));

		undoManager = new UndoManager(extractionModelFrame.undoMenuItem, extractionModelFrame.redoMenuItem, undoViewHolder) {
			@Override
			public void undo() {
				super.undo();
				updateView();
				clearMessageBox();
			}

			@Override
			public void redo() {
				super.redo();
				updateView();
				clearMessageBox();
			}
		};

		AutoCompletion.enable(rootTable);
		AutoCompletion.enable(subjectTable);

		gridBagConstraints = new GridBagConstraints();
		gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        JToggleButton sb;
		jPanel3.add(sb = StringSearchPanel.createSearchButton(extractionModelFrame, subjectTable, "Find Table", null), gridBagConstraints);
		UIUtil.setLeadingComponent(subjectTable, sb);
		
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        focusPanel.add(sb = StringSearchPanel.createSearchButton(extractionModelFrame, rootTable, "Focus on Table", null), gridBagConstraints);
        UIUtil.setLeadingComponent(rootTable, sb);
        resetFocus.setIcon(UIUtil.scaleIcon(resetFocus, resetIcon));
        UIUtil.setTrailingComponent(rootTable, resetFocus);
        rootTable.setPreferredSize(new Dimension(rootTable.getPreferredSize().width + 40, rootTable.getPreferredSize().height));
        rootTable.setMinimumSize(rootTable.getPreferredSize());
        
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
			protected List<Table> getRoots() {
				ArrayList<Table> roots = new ArrayList<Table>();
				if (subject != null) {
					roots.add(subject);
				}
				for (AdditionalSubject as: extractionModel.additionalSubjects) {
					if (as.getSubject() != subject) {
						if (as.getSubject() != null) {
							roots.add(as.getSubject());
						}
					}
				}
				return roots;
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

		restrDepsView = extractionModelFrame.restrictedDependenciesView.getContentPane();
		closureView.addTabComponent("Restricted Dependencies", restrDepsView);
		extractionModelFrame.restrictedDependenciesView.dispose();

		activateDesictionPendingButton.setVisible(false);

		if (extractionModelFrame.pendingDecisionsDialog != null) {
			extractionModelFrame.pendingDecisionsDialog.dispose();
		}
		extractionModelFrame.pendingDecisionsDialog = new JDialog(extractionModelFrame, "Model Migration Tool");
		extractionModelFrame.pendingDecisionsDialog.setModal(false);
		extractionModelFrame.pendingDecisionsDialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
		extractionModelFrame.pendingDecisionsDialog.setLocation(30, 130);
		UIUtil.setDialogSize(extractionModelFrame.pendingDecisionsDialog, 860, 600);
		extractionModelFrame.addWindowListener(new WindowListener() {
			@Override
			public void windowClosed(WindowEvent e) {
				if (extractionModelFrame.pendingDecisionsDialog != null) {
					extractionModelFrame.pendingDecisionsDialog.dispose();
				}
			}
			@Override
			public void windowOpened(WindowEvent e) {
			}
			@Override
			public void windowIconified(WindowEvent e) {
			}
			@Override
			public void windowDeiconified(WindowEvent e) {
			}
			@Override
			public void windowDeactivated(WindowEvent e) {
			}
			@Override
			public void windowClosing(WindowEvent e) {
			}
			@Override
			public void windowActivated(WindowEvent e) {
			}
		});

		pendingDecisionsPanel = new PendingDecisionsPanel(dataModel, this) {
			Boolean docked = null;
			boolean observed = false;
			boolean first = true;
			boolean locate = true;
			@Override
			protected void toggleDockingState() {
				if (!Boolean.FALSE.equals(docked)) {
					ExtractionModelEditor.this.extractionModelFrame.pendingDecisionsDialog.getContentPane().removeAll();
					ExtractionModelEditor.this.extractionModelFrame.pendingDecisionsDialog.getContentPane().add(pendingDecisionsPanel);
					if (locate) {
						UIUtil.setInitialWindowLocation(ExtractionModelEditor.this.extractionModelFrame.pendingDecisionsDialog, ExtractionModelEditor.this.extractionModelFrame, 30, 130);
						locate = false;
					}
					ExtractionModelEditor.this.extractionModelFrame.pendingDecisionsDialog.setVisible(true);
					if (!observed) {
						ExtractionModelEditor.this.extractionModelFrame.pendingDecisionsDialog.addWindowListener(new WindowListener() {
							@Override
							public void windowClosed(WindowEvent e) {
							}
							@Override
							public void windowOpened(WindowEvent e) {
							}
							@Override
							public void windowIconified(WindowEvent e) {
							}
							@Override
							public void windowDeiconified(WindowEvent e) {
							}
							@Override
							public void windowDeactivated(WindowEvent e) {
							}
							@Override
							public void windowClosing(WindowEvent e) {
								toggleDockingState();
							}
							@Override
							public void windowActivated(WindowEvent e) {
							}
						});
						observed = true;
					}
					dockButton.setText("Dock");
					infoPanel.setVisible(true);
					docked = false;
					closureView.selectTabIndex(0);
				} else {
					ExtractionModelEditor.this.extractionModelFrame.pendingDecisionsDialog.setVisible(false);
					closureView.addTabComponent("Migration pending", pendingDecisionsPanel);
					closureView.selectTabComponent(pendingDecisionsPanel);
					dockButton.setText("Undock");
					infoPanel.setVisible(false);
					docked = true;
				}
				ExtractionModelEditor.this.extractionModelFrame.modelMigrationMenuItem.setEnabled(!Boolean.FALSE.equals(docked));
				activateDesictionPendingButton.setEnabled(docked);
			}
			@Override
			protected void activate() {
				activateDesictionPendingButton.setVisible(true);
				ExtractionModelEditor.this.extractionModelFrame.modelMigrationMenuItem.setEnabled(true);
				if (first) {
					first = false;
					if (!decisionMade) {
						toggleDockingState();
					}
				}
			}
			@Override
			protected void deactivate() {
				activateDesictionPendingButton.setVisible(false);
				ExtractionModelEditor.this.extractionModelFrame.modelMigrationMenuItem.setEnabled(false);
			}
			@Override
			protected void markDirty() {
				ExtractionModelEditor.this.markDirty();
			}
		};

		Container cVContentPane = closureView.getContentPane();
		closureView.dispose();
		toolPanel.add(cVContentPane, java.awt.BorderLayout.CENTER);

		validate();

		tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
		tree.setExpandsSelectedPaths(true);
		restrictionEditor = new RestrictionEditor();
		gridBagConstraints = new GridBagConstraints();
		gridBagConstraints.gridx = 5;
		gridBagConstraints.gridy = 0;
		gridBagConstraints.gridheight = 10;
		gridBagConstraints.anchor = GridBagConstraints.NORTH;
		JButton whereConEditorButton = createWhereConEditorButton(
				() -> {
					List<Column> pks = new ArrayList<Column>();
					List<Column> columns = new ArrayList<Column>();
					if (currentAssociation.source.primaryKey.getColumns() != null) {
						currentAssociation.source.primaryKey.getColumns().forEach(c -> {
							Column newColumn = new Column("A." + c.name, c.type,  c.length, c.precision);
							newColumn.isNullable = c.isNullable;
							pks.add(newColumn);
						});
					}
					if (currentAssociation.source.getColumns() != null) {
						currentAssociation.source.getColumns().forEach(c -> {
							Column newColumn = new Column("A." + c.name, c.type,  c.length, c.precision);
							newColumn.isNullable = c.isNullable;
							columns.add(newColumn);
						});
					}
					if (currentAssociation.destination.primaryKey.getColumns() != null) {
						currentAssociation.destination.primaryKey.getColumns().forEach(c -> {
							Column newColumn = new Column("B." + c.name, c.type,  c.length, c.precision);
							newColumn.isNullable = c.isNullable;
							pks.add(newColumn);
						});
					}
					if (currentAssociation.destination.getColumns() != null) {
						currentAssociation.destination.getColumns().forEach(c -> {
							Column newColumn = new Column("B." + c.name, c.type,  c.length, c.precision);
							newColumn.isNullable = c.isNullable;
							columns.add(newColumn);
						});
					}
					PrimaryKey pk = new PrimaryKey(pks, false);
					String unrestrictedJoinCondition = currentAssociation.getUnrestrictedJoinCondition();
					if (currentAssociation.reversed) {
						unrestrictedJoinCondition = SqlUtil.reversRestrictionCondition(unrestrictedJoinCondition);
					}
					Table table = new Table(currentAssociation.source.getName() + " A join " + currentAssociation.destination.getName() + " B on (" + unrestrictedJoinCondition + ")", pk, false, false);
					table.setColumns(columns);
					return table;
		}, () -> restrictionEditor.restriction.getText(), 
				s -> {
					restrictionEditor.restriction.setText(s);
					onApply(true);
				}, false, null, provider -> {
					if (provider != null) {
						provider.removeAliases();
						provider.addAlias("A", currentAssociation.source);
						provider.addAlias("B", currentAssociation.destination);
					}
				});
		SmallButton sm = new LightBorderSmallButton(whereConEditorButton.getIcon()) {
			@Override
			protected void onClick(MouseEvent e) {
				whereConEditorButton.doClick();
			}
		};
		whereConEditorButton.setVisible(false);
		restrictionEditor.jPanel11.add(whereConEditorButton, gridBagConstraints);
		restrictionEditor.jPanel11.add(sm, gridBagConstraints);
		whereConEditorButton.setEnabled(restrictionEditor.restriction.isEnabled());
		sm.setEnabled(restrictionEditor.restriction.isEnabled());
		restrictionEditor.restriction.addPropertyChangeListener("enabled", e -> whereConEditorButton.setEnabled(restrictionEditor.restriction.isEnabled()));
		if (UIUtil.plaf != PLAF.FLAT) {
			restrictionEditor.restriction.addPropertyChangeListener("enabled", e -> sm.setVisible(restrictionEditor.restriction.isEnabled()));
		}
		// condition.setFont(UIUtil.getSQLEditorFont());
		restrictionEditor.restriction.setFont(UIUtil.getSQLEditorFont());
		
		if (UIUtil.plaf == PLAF.FLAT) {
			sm.setVisible(false);
			whereConEditorButton.setVisible(true);
			UIUtil.setLeadingComponent(restrictionEditor.restriction, whereConEditorButton);
		}
		
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
		layeredPane.setLayer(undoViewHolder, 110);
		layeredPane.setLayout(new GridBagLayout());
		gridBagConstraints = new GridBagConstraints();
		gridBagConstraints.gridx = 0;
		gridBagConstraints.gridy = 0;
		gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHEAST;
		gridBagConstraints.weightx = 0;
		gridBagConstraints.weighty = 0;
		gridBagConstraints.insets = new Insets(16, 0, 0, 4);
		layeredPane.add(undoViewHolder, gridBagConstraints);
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
			@Override
			public void valueChanged(ListSelectionEvent e) {
				if (restrictionsTable.getSelectedRow() >= 0) {
					select(currentRestrictionDefinitions.get(restrictionsTable.getSelectedRow()));
				}
			}
		});
		final TableCellRenderer defaultTableCellRenderer = restrictionsTable.getDefaultRenderer(String.class);
		final Color BG1 = UIUtil.TABLE_BACKGROUND_COLOR_1;
		final Color BG2 = UIUtil.TABLE_BACKGROUND_COLOR_2;
		restrictionsTable.setDefaultRenderer(Object.class, new TableCellRenderer() {
			@Override
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
			@Override
			public void actionPerformed(ActionEvent e) {
				onApply(true);
			}
		});
		restrictionEditor.ignore.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				onApply(false);
			}
		});
		restrictionEditor.restricted.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				onApply(false);
			}
		});
		restrictionEditor.fkToNullCheckBox.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (currentAssociation != null) {
					setOrResetFKNullFilter(currentAssociation, restrictionEditor.fkToNullCheckBox.isSelected());
				}
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
			if (dataModel != null) {
				subjectTable.setSelectedItem(dataModel.getDisplayName(extractionModel.subject));
			}
		}
		String cond = extractionModel.getCondition();
		if (cond.equals(SqlUtil.SQL_TRUE)) {
			cond = "";
		}
		condition.setText((cond));
		condition.addKeyListener(new KeyListener() {
			@Override
			public void keyPressed(KeyEvent arg0) {
				needsSave = true;
				ExtractionModelEditor.this.extractionModelFrame.updateTitle(needsSave);
			}
			@Override
			public void keyReleased(KeyEvent arg0) {
			}
			@Override
			public void keyTyped(KeyEvent arg0) {
			}
		});

		exportButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				ExtractionModelEditor.this.extractionModelFrame.dataExportActionPerformed(e);
			}
		});

		needsSave = saveNeedsSave;
		extractionModelFrame.updateTitle(needsSave);

		try {
			if (dataModel != null) {
				ScriptFormat f = ScriptFormat.valueOf(dataModel.getExportModus());
				if (f != null) {
					scriptFormat = f;
				}
			}
		} catch (Exception e) {
			// ignore
		}

		DefaultComboBoxModel formatComboBoxModel = new DefaultComboBoxModel(ScriptFormat.values());

		exportFormat.setModel(formatComboBoxModel);
		exportFormat.setRenderer(new DefaultListCellRenderer() {
			ListCellRenderer renderer = exportFormat.getRenderer();
			@Override
			public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
				return renderer.getListCellRendererComponent(list, ((ScriptFormat) value).getDisplayName(), index, isSelected, cellHasFocus);
			}
			private static final long serialVersionUID = 2393022320508863837L;
		});
		exportFormat.setSelectedItem(scriptFormat);

		openXmlSettings.setVisible(ScriptFormat.XML.equals(scriptFormat));
		onExportModusChanged(null);
		setOrientation(horizontalLayout);
		connectivityState.setText(connectionState);
		connectivityState.setToolTipText(connectionStateToolTip);
		connectivityState.setIcon(dbmsLogo);

		if (dataModel != null) {
			String modelname = "Data Model \"" + dataModel.getName() + "\"";
			String lastMod = dataModel.getLastModifiedAsString();
			if (lastMod.length() > 0) {
				lastMod = " (" + lastMod + ")";
			}
			modelName.setText(modelname);
			modelName.setToolTipText(modelname + lastMod);
		}

		String modelpath = executionContext.getQualifiedDatamodelFolder();
		try {
			modelpath = new File(modelpath).getAbsolutePath();
		} catch (Throwable t) {
			// use default modelpath
		}
		modelpath += File.separator;
		modelPath.setToolTipText(modelpath);
		final int MAX_LENGTH = 40;
		if (modelpath.length() > MAX_LENGTH + 4) {
			modelpath = modelpath.substring(0, MAX_LENGTH/2) + "..." + modelpath.substring(modelpath.length() - MAX_LENGTH/2);
		}
		modelPath.setText(modelpath);

		subjectConditionEditor = new NonModalConditionEditor(extractionModelFrame, parametersGetter, false, dataModel) {
			@Override
			protected void consume(String cond) {
				if (cond != null) {
					if (!condition.getText().equals((cond))) {
						condition.setText((cond));
						try {
							condition.setCaretPosition(0);
						} catch (Exception e) {
							// ignore
						}
						needsSave = true;
						ExtractionModelEditor.this.extractionModelFrame.updateTitle(needsSave);
					}
				}
			}
		};
		subjectConditionEditor.setTitle("Subject condition");
		restrictionConditionEditor = new NonModalConditionEditor(extractionModelFrame, parametersGetter, true, dataModel) {
			@Override
			protected void consume(String cond) {
				if (cond != null) {
					if (!restrictionEditor.restriction.getText().equals((cond))) {
						restrictionEditor.restriction.setText((cond));
						onApply(true);
						try {
							restrictionEditor.restriction.setCaretPosition(0);
						} catch (Exception e) {
							// ignore
						}
					}
				}
			}
		};
		gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        restrictionConditionEditor.addOnPanel.add(restrictionEditor.columnsA, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        restrictionConditionEditor.addOnPanel.add(restrictionEditor.columnsB, gridBagConstraints);
		restrictionConditionEditor.setTitle("Restriction");
		openSubjectConditionEditor.setIcon(conditionEditorIcon);
		openSubjectConditionEditor.setText(null);
		restrictionConditionEditor.observe(restrictionEditor.restriction, new Consumer<String>() {
			@Override
			public void accept(String text) {
				if (currentAssociation != null && restrictionEditor.restriction.isEditable()) {
					restrictionConditionEditor.edit(restrictionEditor.restriction, text, "Table A", "A", currentAssociation.source, "Table B", "B", currentAssociation.destination, true, false);
				}
			}
		});
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
					restrictionConditionEditor.edit(restrictionEditor.restriction, restrictionEditor.restriction.getText(), "Table A", "A", currentAssociation.source, "Table B", "B", currentAssociation.destination, true, false);
				}
			}

			@Override
			public void mouseEntered(java.awt.event.MouseEvent evt) {
				if (currentAssociation != null) {
					restrictionEditor.openRestrictionConditionEditor.setIcon(conditionEditorSelectedIcon);
				}
			}
			@Override
			public void mouseExited(java.awt.event.MouseEvent evt) {
				restrictionEditor.openRestrictionConditionEditor.setIcon(conditionEditorIcon);
		   }
		});
		openSubjectConditionEditor.addMouseListener(new java.awt.event.MouseAdapter() {
			@Override
			public void mouseReleased(MouseEvent e) {
				mouseClicked(e);
			}
			@Override
			public void mouseClicked(java.awt.event.MouseEvent evt) {
				subjectConditionEditor.edit(condition, condition.getText(), "", "T", subject, null, null, null, false, true);
			}

			@Override
			public void mouseEntered(java.awt.event.MouseEvent evt) {
				openSubjectConditionEditor.setIcon(conditionEditorSelectedIcon);
			}
			@Override
			public void mouseExited(java.awt.event.MouseEvent evt) {
				openSubjectConditionEditor.setIcon(conditionEditorIcon);
		   }
		});
		subjectConditionEditor.observe(condition, new Consumer<String>() {
			@Override
			public void accept(String text) {
				subjectConditionEditor.edit(condition, text, "Subject", "T", subject, null, null, null, false, true);
			}
		});

		leftButton.setIcon(UIUtil.scaleIcon(leftIcon, (48 * 48) / 64, (48 * 48) / 64));
		leftButton.setPressedIcon(UIUtil.scaleIcon(leftIconP, (48 * 48) / 64, (48 * 48) / 64));

		leftButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				navigateBack();
			}
		});

		updateLeftButton();
		updateAdditionalSubjectsButton();
	}

	private WhereConditionEditorPanel whereConditionEditorPanel;
	RSyntaxTextAreaWithSQLSyntaxStyle whereConditionEditorEditor;
	
	JButton createWhereConEditorButton(Supplier<Table> getSubject, Supplier<String> initialText, Consumer<String> consumer, boolean locateUnderButton, String tableAlias, Consumer<SQLCompletionProvider> providerConsumer) {
		whereConditionEditorEditor = new RSyntaxTextAreaWithSQLSyntaxStyle(false, false) {
			@Override
			protected void runBlock() {
				super.runBlock();
				if (whereConditionEditorPanel != null) {
					whereConditionEditorPanel.parseCondition();
				}
			}
			@Override
			protected boolean withFindAndReplace() {
				return false;
			}
		};
		whereConditionEditorEditor.setEnabled(true);
		whereConditionEditorEditor.setMarkOccurrences(false);
		whereConditionEditorEditor.grabFocus();
		
		JButton button = new JButton(null, UIUtil.scaleIcon(this, searchIcon));
		button.addActionListener(e -> {
			if (getSubject.get() != null && (extractionModelFrame.theSession != null || extractionModelFrame.connectToDBIfNeeded("Open Condition Editor"))) {
				Window windowAncestor = SwingUtilities.getWindowAncestor(button);
				try {
					UIUtil.setWaitCursor(windowAncestor);
					JDialog dialog = new JDialog(windowAncestor);
					Runnable close = () -> {
						dialog.setVisible(false);
						dialog.dispose();
					};
					if (extractionModelFrame.theSession == null) {
						BasicDataSource dataSource = new BasicDataSource(extractionModelFrame.dbConnectionDialog.currentConnection.driverClass, extractionModelFrame.dbConnectionDialog.currentConnection.url, extractionModelFrame.dbConnectionDialog.currentConnection.user, extractionModelFrame.dbConnectionDialog.getPassword(), 0, extractionModelFrame.dbConnectionDialog.currentJarURLs());
						extractionModelFrame.theSession = SessionForUI.createSession(dataSource, dataSource.dbms, executionContext.getIsolationLevel(), true, false, windowAncestor);
						if (extractionModelFrame.theSession == null) {
							return;
						}
					}
					
					String initialCondition = initialText.get();
					WhereConditionEditorPanel wcep = new WhereConditionEditorPanel(windowAncestor,
						dataModel, getSubject.get(), BrowserContentCellEditor.forTable(getSubject.get(), extractionModelFrame.theSession),
						false,
						whereConditionEditorPanel, whereConditionEditorEditor,
						null, true, -1, locateUnderButton,
						extractionModelFrame.theSession, new DataModelBasedSQLCompletionProvider(extractionModelFrame.theSession, dataModel), executionContext) {
					
						@Override
						protected void onEscape() {
							consumer.accept(initialCondition);
							close.run();
						}
						
						@Override
						protected void consume(String condition, Set<Integer> involvedColumns) {
							consumer.accept(condition);
						}
					};
					
					whereConditionEditorPanel = wcep;
					whereConditionEditorPanel.setTableAlias(tableAlias);
					if (providerConsumer != null) {
						providerConsumer.accept(whereConditionEditorPanel.provider);
					}
					
					whereConditionEditorPanel.parseCondition(initialCondition);
					dialog.setModal(false);
					dialog.setUndecorated(true);
					dialog.addWindowFocusListener(new WindowFocusListener() {
						@Override
						public void windowLostFocus(WindowEvent e) {
							if (!(e.getOppositeWindow() instanceof StringSearchDialog)) {
								close.run();
							}
						}
						@Override
						public void windowGainedFocus(WindowEvent e) {
						}
					});
					
					Component comp = button.isVisible()? button : button.getParent();
					Point location = new Point(0, comp.getHeight());
					SwingUtilities.convertPointToScreen(location, comp);
							
					int x = location.x;
					int y = location.y;
					
					if (!wcep.isVisible()) {
						dialog.getContentPane().add(new JLabel("Condition editor not supported for DBMS \"" + (extractionModelFrame.theSession.dbms == null? null : extractionModelFrame.theSession.dbms.getId()) + "\"\n") {
							{
								setBorder(BorderFactory.createLineBorder(Color.red));
							}
						});
						whereConditionEditorPanel = null;
					} else {
						dialog.getContentPane().add(whereConditionEditorPanel);
					}
					
					dialog.pack();
					dialog.setLocation(x, y);
					int minWidth = 660;
					int wid = Math.max(minWidth, dialog.getWidth());
					int maxX = getX() + getWidth() - wid - 8;;
					if (whereConditionEditorPanel != null) {
						dialog.setSize(wid, Math.min(Math.max(dialog.getHeight(), 260), 600));
					}
					dialog.setLocation(Math.max(0, Math.min(maxX, dialog.getX())), dialog.getY());
					int maxY = getY() + getHeight() - dialog.getHeight() - 8;
					if (maxY < dialog.getY()) {
						int deltaH = Math.min(dialog.getY() - maxY, (int) (0.30 * dialog.getHeight()));
						maxY += deltaH;
						if (whereConditionEditorPanel != null) {
							dialog.setSize(dialog.getWidth(), dialog.getHeight() - deltaH);
						}
						dialog.setLocation(dialog.getX(), Math.max(0, maxY));
					}
					UIUtil.invokeLater(() -> {
						UIUtil.invokeLater(() -> dialog.requestFocus());
						dialog.setVisible(true);
					});
				} catch (Exception e1) {
					UIUtil.showException(windowAncestor, "Error", e1);
				} finally {
					UIUtil.resetWaitCursor(windowAncestor);
				}
				
			}	
		});
		return button;
	}

	public void undoChange() {
		undoManager.undo();
	}

	public void redoChange() {
		undoManager.redo();
	}

	public void openPendingDecisionsEditor() {
		pendingDecisionsPanel.toggleDockingState();
	}

	private void updateAdditionalSubjectsButton() {
		int n = extractionModel.additionalSubjects.size();
		additionalSubjectsButton.setText("Add Subject" + (n > 0? " (" + n + ")" : ""));
		if (extractionModelFrame != null && closureBorderView != null) {
			closureBorderView.refresh();
		}
	}

	/**
	 * The "closure view" component.
	 */
	ClosureView closureView;

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
			jSplitPane1.setLeftComponent(panel);

			JPanel panel2 = new JPanel();
			panel2.setLayout(new GridBagLayout());
			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.gridx = 0;
			gridBagConstraints.gridy = 0;
			gridBagConstraints.weightx = 1;
			gridBagConstraints.weighty = 0;
			gridBagConstraints.insets = new Insets(2, 0, 0, 0);
			gridBagConstraints.fill = GridBagConstraints.BOTH;

			panel2.add(jPanel3, gridBagConstraints);
			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.gridx = 0;
			gridBagConstraints.gridy = 1;
			gridBagConstraints.weightx = 1;
			gridBagConstraints.weighty = 1;
			gridBagConstraints.fill = GridBagConstraints.BOTH;
			gridBagConstraints.insets = new Insets(0, 0, 0, 0);
			panel2.add(jPanel4, gridBagConstraints);
			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.gridx = 0;
			gridBagConstraints.gridy = 2;
			gridBagConstraints.weightx = 1;
			gridBagConstraints.weighty = 0;
			gridBagConstraints.fill = GridBagConstraints.BOTH;
			gridBagConstraints.insets = new Insets(0, 0, 0, 0);
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
				gridBagConstraints.insets = new Insets(0, 0, 0, 0);
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
		return (condition.getText());
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
        openSubjectConditionEditor = new javax.swing.JLabel();
        jSplitPane1 = new javax.swing.JSplitPane();
        jpanel = new javax.swing.JPanel();
        jPanel12 = new javax.swing.JPanel();
        jSplitPane2 = new javax.swing.JSplitPane();
        layeredPane = new javax.swing.JLayeredPane();
        toolBarPanel = new javax.swing.JPanel();
        graphContainer = new javax.swing.JPanel();
        undoViewHolder = new javax.swing.JPanel();
        inspectorHolder = new javax.swing.JPanel();
        focusPanel = new javax.swing.JPanel();
        focusLabelPanel = new javax.swing.JPanel();
        jLabel9 = new javax.swing.JLabel();
        rootTable = new JComboBox2();
        resetFocus = new javax.swing.JButton();
        leftButton = new javax.swing.JButton();
        activateDesictionPendingButton = new javax.swing.JButton();
        jPanel19 = new javax.swing.JPanel();
        rightBorderPanel = new javax.swing.JPanel();
        messagePanel = new javax.swing.JPanel();
        neighborHolderPanel = new javax.swing.JPanel();
        jPanel6 = new javax.swing.JPanel();
        toolPanel = new javax.swing.JPanel();
        jPanel1 = new javax.swing.JPanel();
        editorPanel = new javax.swing.JPanel();
        jPanel3 = new javax.swing.JPanel();
        jPanel9 = new javax.swing.JPanel();
        jLabel6 = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        jPanel15 = new javax.swing.JPanel();
        jPanel2 = new javax.swing.JPanel();
        jPanel7 = new javax.swing.JPanel();
        condition = new javax.swing.JTextField();
        jPanel8 = new javax.swing.JPanel();
        jLabel7 = new javax.swing.JLabel();
        jPanel10 = new javax.swing.JPanel();
        exportFormat = new JComboBox2();
        exportButton = new javax.swing.JButton();
        openXmlSettings = new javax.swing.JButton();
        jLabel10 = new javax.swing.JLabel();
        jPanel13 = new javax.swing.JPanel();
        limitLabel = new javax.swing.JLabel();
        limitButton = new javax.swing.JToggleButton();
        jPanel16 = new javax.swing.JPanel();
        additionalSubjectsButton = new javax.swing.JButton();
        jPanel17 = new javax.swing.JPanel();
        jPanel18 = new javax.swing.JPanel();
        jPanel20 = new javax.swing.JPanel();
        jPanel4 = new javax.swing.JPanel();
        jPanel14 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        tree = new javax.swing.JTree();
        assocStatsLabel = new javax.swing.JLabel();
        jLabel11 = new javax.swing.JLabel();
        xmlMappingPanel = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        aggregationCombobox = new JComboBox2();
        tagField = new javax.swing.JTextField();
        jPanel5 = new javax.swing.JPanel();
        xmlTagApply = new javax.swing.JButton();
        mapColumns = new javax.swing.JButton();
        sketchTabbedPane = new javax.swing.JTabbedPane();
        jLabel12 = new javax.swing.JLabel();
        jPanel11 = new javax.swing.JPanel();
        legende = new javax.swing.JPanel();
        dependsOn = new javax.swing.JLabel();
        hasDependent = new javax.swing.JLabel();
        associatedWith = new javax.swing.JLabel();
        ignored = new javax.swing.JLabel();
        jSeparator1 = new javax.swing.JSeparator();
        legende1 = new javax.swing.JPanel();
        modelName = new javax.swing.JLabel();
        modelPath = new javax.swing.JLabel();
        legende2 = new javax.swing.JPanel();
        connectivityState = new javax.swing.JLabel();

        restrictionsTable.setModel(restrictionTableModel());
        jScrollPane4.setViewportView(restrictionsTable);

        openSubjectConditionEditor.setText("jLabel10");
        openSubjectConditionEditor.setToolTipText("open SQL editor");

        setLayout(new java.awt.GridBagLayout());

        jSplitPane1.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        jSplitPane1.setContinuousLayout(true);
        jSplitPane1.setOneTouchExpandable(true);

        jpanel.setLayout(new java.awt.BorderLayout());

        jPanel12.setLayout(new java.awt.BorderLayout());

        jSplitPane2.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        jSplitPane2.setResizeWeight(1.0);
        jSplitPane2.setContinuousLayout(true);
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

        undoViewHolder.setOpaque(false);
        undoViewHolder.setLayout(new java.awt.GridBagLayout());
        layeredPane.setLayer(undoViewHolder, javax.swing.JLayeredPane.PALETTE_LAYER);
        layeredPane.add(undoViewHolder);
        undoViewHolder.setBounds(0, 0, 0, 0);

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
        gridBagConstraints.gridx = 0;
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
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        focusPanel.add(leftButton, gridBagConstraints);

        activateDesictionPendingButton.setBackground(new java.awt.Color(255, 153, 152));
        activateDesictionPendingButton.setForeground(new java.awt.Color(0, 0, 1));
        activateDesictionPendingButton.setText("Migration pending");
        activateDesictionPendingButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                activateDesictionPendingButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(0, 16, 0, 0);
        focusPanel.add(activateDesictionPendingButton, gridBagConstraints);

        jPanel19.setLayout(null);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        focusPanel.add(jPanel19, gridBagConstraints);

        layeredPane.setLayer(focusPanel, javax.swing.JLayeredPane.PALETTE_LAYER);
        layeredPane.add(focusPanel);
        focusPanel.setBounds(0, 0, 289, 33);

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

        jPanel6.setLayout(new java.awt.GridBagLayout());

        toolPanel.setLayout(new java.awt.BorderLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel6.add(toolPanel, gridBagConstraints);

        jSplitPane2.setBottomComponent(jPanel6);

        jPanel12.add(jSplitPane2, java.awt.BorderLayout.CENTER);

        jpanel.add(jPanel12, java.awt.BorderLayout.CENTER);

        jSplitPane1.setBottomComponent(jpanel);

        jPanel1.setLayout(new java.awt.GridBagLayout());

        editorPanel.setLayout(new java.awt.GridLayout(1, 4));

        jPanel3.setToolTipText("Specify here where to start collecting the rows to be exported.");
        jPanel3.setLayout(new java.awt.GridBagLayout());

        jPanel9.setLayout(null);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel3.add(jPanel9, gridBagConstraints);

        jLabel6.setText("  Export from  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel3.add(jLabel6, gridBagConstraints);

        jLabel4.setText("  To");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel3.add(jLabel4, gridBagConstraints);

        jLabel5.setText(" as T ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 12);
        jPanel3.add(jLabel5, gridBagConstraints);

        jPanel15.setLayout(null);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        jPanel3.add(jPanel15, gridBagConstraints);

        jPanel2.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 11;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        jPanel3.add(jPanel2, gridBagConstraints);

        jPanel7.setLayout(new java.awt.GridBagLayout());

        condition.setText("jTextField1");
        condition.setToolTipText("SQL expression. Keep empty if you want to export all rows.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 12);
        jPanel7.add(condition, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.gridwidth = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        jPanel3.add(jPanel7, gridBagConstraints);

        jPanel8.setLayout(new java.awt.BorderLayout());

        jLabel7.setText("  Where ");
        jPanel8.add(jLabel7, java.awt.BorderLayout.WEST);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel3.add(jPanel8, gridBagConstraints);

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
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHEAST;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 12);
        jPanel10.add(exportButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.gridwidth = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel3.add(jPanel10, gridBagConstraints);

        openXmlSettings.setText("XML Settings");
        openXmlSettings.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                openXmlSettingsActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel3.add(openXmlSettings, gridBagConstraints);

        jLabel10.setText("  Limit");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 4, 0);
        jPanel3.add(jLabel10, gridBagConstraints);

        jPanel13.setLayout(new java.awt.GridBagLayout());

        limitLabel.setText("<html><i>no limit</i></html>");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 0);
        jPanel13.add(limitLabel, gridBagConstraints);

        limitButton.setText("Edit Limit");
        limitButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                limitButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 12);
        jPanel13.add(limitButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.gridwidth = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 4, 0);
        jPanel3.add(jPanel13, gridBagConstraints);

        jPanel16.setLayout(new java.awt.GridBagLayout());

        additionalSubjectsButton.setText("Add  Subject");
        additionalSubjectsButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                additionalSubjectsButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        jPanel16.add(additionalSubjectsButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 7;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 4, 0);
        jPanel3.add(jPanel16, gridBagConstraints);

        jPanel17.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 5, 8));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 12;
        gridBagConstraints.gridwidth = 4;
        jPanel3.add(jPanel17, gridBagConstraints);

        jPanel18.setLayout(null);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridwidth = 5;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        jPanel3.add(jPanel18, gridBagConstraints);

        jPanel20.setLayout(null);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 11;
        gridBagConstraints.weighty = 1.0;
        jPanel3.add(jPanel20, gridBagConstraints);

        editorPanel.add(jPanel3);

        jPanel4.setLayout(new java.awt.GridBagLayout());

        jPanel14.setLayout(new java.awt.GridBagLayout());

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

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel14.add(jScrollPane1, gridBagConstraints);

        assocStatsLabel.setText("jLabel8");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 0);
        jPanel14.add(assocStatsLabel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 0);
        jPanel4.add(jPanel14, gridBagConstraints);

        jLabel11.setFont(jLabel11.getFont().deriveFont(jLabel11.getFont().getStyle() | java.awt.Font.BOLD));
        jLabel11.setText(" Association");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        jPanel4.add(jLabel11, gridBagConstraints);

        editorPanel.add(jPanel4);

        xmlMappingPanel.setLayout(new java.awt.GridBagLayout());

        jLabel1.setText(" Aggregation ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 0);
        xmlMappingPanel.add(jLabel1, gridBagConstraints);

        jLabel2.setText(" Tag ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 8, 0, 0);
        xmlMappingPanel.add(jLabel2, gridBagConstraints);

        jLabel3.setText(" Sketch ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 0);
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
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 0);
        xmlMappingPanel.add(sketchTabbedPane, gridBagConstraints);

        jLabel12.setFont(jLabel12.getFont().deriveFont(jLabel12.getFont().getStyle() | java.awt.Font.BOLD));
        jLabel12.setText(" XML Mapping");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridwidth = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(8, 0, 4, 0);
        xmlMappingPanel.add(jLabel12, gridBagConstraints);

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

        legende.setLayout(new java.awt.GridBagLayout());

        dependsOn.setFont(dependsOn.getFont().deriveFont(dependsOn.getFont().getSize()+1f));
        dependsOn.setForeground(new java.awt.Color(170, 0, 0));
        dependsOn.setText(" Parent (depends on) ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        legende.add(dependsOn, gridBagConstraints);

        hasDependent.setFont(hasDependent.getFont().deriveFont(hasDependent.getFont().getSize()+1f));
        hasDependent.setForeground(new java.awt.Color(0, 112, 0));
        hasDependent.setText("   Child (has dependent) ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        legende.add(hasDependent, gridBagConstraints);

        associatedWith.setFont(associatedWith.getFont().deriveFont(associatedWith.getFont().getSize()+1f));
        associatedWith.setForeground(new java.awt.Color(0, 100, 255));
        associatedWith.setText("   associated with");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        legende.add(associatedWith, gridBagConstraints);

        ignored.setFont(ignored.getFont().deriveFont(ignored.getFont().getSize()+1f));
        ignored.setForeground(new java.awt.Color(153, 153, 153));
        ignored.setText("   disabled ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        legende.add(ignored, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.insets = new java.awt.Insets(3, 0, 3, 0);
        jPanel11.add(legende, gridBagConstraints);

        jSeparator1.setOrientation(javax.swing.SwingConstants.VERTICAL);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        jPanel11.add(jSeparator1, gridBagConstraints);

        legende1.setLayout(new java.awt.GridBagLayout());

        modelName.setFont(modelName.getFont().deriveFont(modelName.getFont().getSize()+1f));
        modelName.setText("Data Model \"Demo\"");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 12);
        legende1.add(modelName, gridBagConstraints);

        modelPath.setFont(modelPath.getFont().deriveFont(modelPath.getFont().getSize()+1f));
        modelPath.setForeground(java.awt.Color.gray);
        modelPath.setText("/home/jailer/datamodel/");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        legende1.add(modelPath, gridBagConstraints);

        jPanel11.add(legende1, new java.awt.GridBagConstraints());

        legende2.setLayout(new java.awt.GridBagLayout());

        connectivityState.setFont(connectivityState.getFont().deriveFont(connectivityState.getFont().getSize()+1f));
        connectivityState.setText("offline");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 0);
        legende2.add(connectivityState, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
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
						JPopupMenu popup = graphView.createPopupMenu(table, null, false);
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
			markDirty();
		}
	}//GEN-LAST:event_openXmlSettingsActionPerformed

	private void aggregationComboboxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_aggregationComboboxActionPerformed
		tagField.setEnabled(AggregationSchema.NONE != aggregationCombobox.getSelectedItem() && AggregationSchema.FLAT != aggregationCombobox.getSelectedItem());
		tagField.setEditable(AggregationSchema.NONE != aggregationCombobox.getSelectedItem() && AggregationSchema.FLAT != aggregationCombobox.getSelectedItem());
		if (currentAssociation != null) {
			if (currentAssociation.getAggregationSchema() != aggregationCombobox.getSelectedItem()) {
				final AggregationSchema selectedItem = (AggregationSchema) aggregationCombobox.getSelectedItem();
				setAggregationSchema(currentAssociation, selectedItem);
			}
		}
	}//GEN-LAST:event_aggregationComboboxActionPerformed

	private void setAggregationSchema(final Association association, AggregationSchema aggregationSchema) {
		final AggregationSchema old = currentAssociation.getAggregationSchema();
		currentAssociation.setAggregationSchema(aggregationSchema);
		markDirty();
		updateSketch();
		undoManager.push(new CompensationAction(1, "changed aggregation", "changed aggregation", dataModel.getDisplayName(association.destination)) {
			@Override
			public void run() {
				setAggregationSchema(association, old);
			}
		});
	}

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
			@Override
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
		Collections.sort(tableNames, String::compareToIgnoreCase);
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
		Collections.sort(tableNames, String::compareToIgnoreCase);
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
				@Override
				public void actionPerformed(ActionEvent e) {
					restrictionConditionEditor.editorPane.replaceSelection(c);
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
		if (association != null) {
			restrictionEditor.columnsA.setText(dataModel.getDisplayName(association.source));
			restrictionEditor.columnsB.setText(dataModel.getDisplayName(association.destination));
		}
		restrictionEditor.columnsA.setIcon(dropDownIcon);
		restrictionEditor.columnsB.setIcon(dropDownIcon);
		restrictionEditor.columnsA.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
		restrictionEditor.columnsB.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
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
				@Override
				public void mousePressed(java.awt.event.MouseEvent evt) {
					openColumnDropDownBox(restrictionEditor.columnsB, "B", association.destination);
				}

				@Override
				public void mouseEntered(java.awt.event.MouseEvent evt) {
					restrictionEditor.columnsB.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.LOWERED));
				}
				@Override
				public void mouseExited(java.awt.event.MouseEvent evt) {
					restrictionEditor.columnsB.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
			   }
			});
			restrictionEditor.columnsA.addMouseListener(maA = new java.awt.event.MouseAdapter() {
				@Override
				public void mousePressed(java.awt.event.MouseEvent evt) {
					openColumnDropDownBox(restrictionEditor.columnsA, "A", association.source);
				}

				@Override
				public void mouseEntered(java.awt.event.MouseEvent evt) {
					restrictionEditor.columnsA.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.LOWERED));
				}
				@Override
				public void mouseExited(java.awt.event.MouseEvent evt) {
					restrictionEditor.columnsA.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
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
			initialRestrictionCondition = association.isIgnored()? null : restrictionCondition;
			restrictionEditor.restriction.setText(restrictionCondition == null? "" : (restrictionCondition));
			try {
				restrictionEditor.restriction.setCaretPosition(0);
			} catch (Exception e) {
				// ignore
			}
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
			restrictionEditor.resetBGColor();
		}
		graphView.setSelection(association);
		restrictionEditor.apply.setEnabled(false);
	}

	private void initRestrictedDependencyWarningField() {
		boolean restrictedDep = currentAssociation != null && !ScriptFormat.XML.equals(scriptFormat) && currentAssociation.isInsertDestinationBeforeSource() && currentAssociation.getRestrictionCondition() != null;

		restrictionEditor.fkToNullCheckBox.setVisible(restrictedDep); //  && (RestrictionModel.IGNORE.equals(currentAssociation.getRestrictionCondition()) || "false".equals(currentAssociation.getRestrictionCondition())));
		restrictionEditor.fkToNullCheckBox.setEnabled(restrictedDep && currentAssociation.hasNullableFK() && !currentAssociation.fkHasExcludeFilter());
		restrictionEditor.fkToNullCheckBox.setSelected(restrictedDep && currentAssociation.fkHasNullFilter());
		restrictionEditor.restrictedDependencyWarning.setVisible(restrictedDep && !currentAssociation.fkHasNullFilter());

		if (currentAssociation != null) {
			restrictionEditor.fk20DisabledHintLabel.setText("(Foreign key is not nullable)");
			Map<Column, Column> sdMap = currentAssociation.createSourceToDestinationKeyMapping();
			StringBuilder sb = new StringBuilder();
			boolean isFiltered = false;
			boolean isPk = false;
			for (Column c: sdMap.keySet()) {
				if (sb.length() > 0) {
					sb.append(", ");
				}
				sb.append(c.name);
				if (c.getFilter() != null) {
					isFiltered = true;
				}
				for (Column pk: currentAssociation.source.primaryKey.getColumns()) {
					if (c.name.equals(pk.name)) {
						isPk = true;
					}
				}
			}
			if (isPk) {
				restrictionEditor.fk20DisabledHintLabel.setText("(Foreign key is also primary key)");
			} else {
				restrictionEditor.fk20DisabledHintLabel.setText("(Foreign key has filter)");
			}
			restrictionEditor.fkToNullCheckBox.setToolTipText("<html><i>in export-script (SQL)</i>: set all foreign keys to null to which the row with the corresponding primary key is not exported <br><hr>\n<i>in delete-script</i>: set all foreign keys in the rows that cannot be deleted to null when the row with the corresponding primary key is deleted "
					+ "<br><br>Foreign key: <b>" + dataModel.getDisplayName(currentAssociation.source) + "(" + sb + ")"
					+ "</html>");
			if (sb.length() > 32) {
				sb.setLength(32);
				sb.append("...");
			}
			if (currentAssociation.fkHasExcludeFilter()) {
				restrictionEditor.fkToNullCheckBox.setText("Foreign key is excluded (" + sb + ")");
				restrictionEditor.fkToNullCheckBox.setEnabled(false);
			} else {
				restrictionEditor.fkToNullCheckBox.setText("Set foreign key to null (" + sb + ")");
			}
		}

		restrictionEditor.updateHint();
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
		restrictionEditor.resetBGColor();
		if (currentAssociation != null) {

			if (restrictionEditor.restricted.isSelected() && restrictionEditor.restriction.getText().trim().length() == 0 && currentAssociation.hasNullableFK() && currentAssociation.fkHasNullFilter()) {
				setOrResetFKNullFilter(currentAssociation, false);
				markDirty();
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
					condition = (restrictionEditor.restriction.getText()).trim();
					initialRestrictionCondition = condition;
				}
			}
			if (addRestriction(currentAssociation, condition, true)) {
				markDirty();
			}
			updateView();
		}
	}

	private void updateView() {
		tree.repaint();
		graphView.display.invalidate();
		restrictionsTable.setModel(restrictionTableModel());
		String saveInitialRestrictionCondition = initialRestrictionCondition;
		initRestrictionEditor(currentAssociation, currentNode);
		initialRestrictionCondition = saveInitialRestrictionCondition;
		closureView.refresh();
		closureBorderView.refresh();
		extractionModelFrame.restrictedDependenciesView.refresh();
		if (extractionModelFrame.hasFocus()) {
			tree.grabFocus();
		}
		
		updateAssocStats();
	}

	private void updateAssocStats() {
		int total = 0;
		int restricted = 0;
		int ignored = 0;
		
		for (Table table: getCurrentSubjectClosure()) {
			for (Association association: table.associations) {
				++total;
				if (association.isIgnored()) {
					++ignored;
				} else if (association.isRestricted()) {
					++restricted;
				}
			}
		}
		String iCol = "<font color=\"#0000B0\">";
		String rCol = "<font color=\"#006000\">";
		String ef = "</font>";
//		assocStatsLabel.setForeground(Color.gray);
		assocStatsLabel.setText("<html><nobr>" + iCol + ignored + ef + (restricted != 0? " + " + rCol + restricted + ef : "") + " / " + total +"<html>");
		assocStatsLabel.setToolTipText("<html>" + iCol + ignored + ef +" disabled and " + rCol + restricted + ef +" restricted associations out of a total of " + total +" associations in the closure.<html>");
	}

	public void removeRestrictions(Collection<Association> associations) {
		for (Association association: associations) {
			markDirty();
			String condition = "";
			addRestriction(association, condition, false);
			graphView.setSelection(association);
		}
		afterAddRestriction();
	}

	public void afterAddRestriction() {
		updateAssocStats();
		markDirty();
		initRestrictionEditor(currentAssociation, currentNode);
		graphView.resetExpandedState();
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
		xmlSketch.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_XML);
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

			@Override
			public int compare(DefaultMutableTreeNode o1,
					DefaultMutableTreeNode o2) {
				Association a1 = (Association) o1.getUserObject();
				Association a2 = (Association) o2.getUserObject();
				int cat1 = cat(a1);
				int cat2 = cat(a2);
				if (cat1 == cat2) {
					return dataModel.getDisplayName(a1.destination).compareToIgnoreCase(dataModel.getDisplayName(a2.destination));
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
				  setAggregationTagName(currentAssociation, tag);
			  }
		  }
	  }//GEN-LAST:event_xmlMappingApplyButtonActionPerformed

	private void setAggregationTagName(final Association association, String tag) {
		final String old = association.getAggregationTagName();
		association.setAggregationTagName(tag);
		tagField.setText(currentAssociation.getAggregationTagName());
		markDirty();
		updateSketch();
		xmlTagApply.setEnabled(false);
		undoManager.push(new CompensationAction(1, "changed tag", "changed tag", dataModel.getDisplayName(association.destination)) {
			@Override
			public void run() {
				setAggregationTagName(association, old);
			}
		});
	}

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

	private void changeSubject(Table newSubject) {
		if (newSubject != null && newSubject != subject) {
			final Table oldSubject = subject;
			final SubjectLimitDefinition oldSubjectLimitDefinition = extractionModel.subjectLimitDefinition;
			undoManager.push(new CompensationAction(1, "changed subject", "changed subject", dataModel.getDisplayName(oldSubject)) {
				@Override
				public void run() {
					subjectTable.setSelectedItem(dataModel.getDisplayName(oldSubject));
					extractionModel.subjectLimitDefinition = oldSubjectLimitDefinition;
					limitLabel.setText(SubjectLimitEditor.subjectLimitDefinitionRender(oldSubjectLimitDefinition));
					markDirty();
				}
			});
			synchronized (currentSubjectClosureLock) {
				currentSubjectClosure = null; // force re-calculation
			}
			subject = newSubject;
			pendingDecisionsPanel.updateView();
			extractionModel.subjectLimitDefinition = new SubjectLimitDefinition(null, null);
			limitLabel.setText(SubjectLimitEditor.subjectLimitDefinitionRender(extractionModel.subjectLimitDefinition));
			markDirty();
		}
		rootTable.setModel(getTableListModel());
		rootTable.setSelectedItem(null);
		rootTable.setSelectedItem(subjectTable.getSelectedItem());
		rootTable.setPreferredSize(new Dimension(Math.min(rootTable.getPreferredSize().width, 300), rootTable.getPreferredSize().height));
		updateAssocStats();
	}

	private void changeSubjectLimitDefinition(SubjectLimitDefinition newSubjectLimitDefinition) {
		if (subject != null && newSubjectLimitDefinition != null && !newSubjectLimitDefinition.equals(extractionModel.subjectLimitDefinition)) {
			final SubjectLimitDefinition oldSubjectLimitDefinition = extractionModel.subjectLimitDefinition;
			undoManager.push(new CompensationAction(1, "changed limit", "changed limit", dataModel.getDisplayName(subject)) {
				@Override
				public void run() {
					changeSubjectLimitDefinition(oldSubjectLimitDefinition);
				}
			});
			extractionModel.subjectLimitDefinition = newSubjectLimitDefinition;
			limitLabel.setText(SubjectLimitEditor.subjectLimitDefinitionRender(newSubjectLimitDefinition));
			markDirty();
		}
	}

	private SubjectLimitEditor subjectLimitEditor;

    @SuppressWarnings("serial")
	private void limitButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_limitButtonActionPerformed
        if (limitButton.isSelected()) {
			if (subjectLimitEditor == null) {
				subjectLimitEditor = new SubjectLimitEditor(extractionModelFrame, dataModel) {
					@Override
					protected void consume(SubjectLimitDefinition subjectLimitDefinition) {
						if (subjectLimitDefinition != null) {
							changeSubjectLimitDefinition(subjectLimitDefinition);
						}
						limitButton.setSelected(false);
					}
				};
			}
			subjectLimitEditor.edit(limitLabel, subject, extractionModel.subjectLimitDefinition);
        }
    }//GEN-LAST:event_limitButtonActionPerformed

	private void resetFocusActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_resetFocusActionPerformed
		rootTable.setSelectedItem(subjectTable.getSelectedItem());
	}//GEN-LAST:event_resetFocusActionPerformed

	private void additionalSubjectsButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_additionalSubjectsButtonActionPerformed
		AdditionalSubjectsDialog additionalSubjectsDialog;
		try {
			UIUtil.setWaitCursor(this);
			additionalSubjectsDialog = new AdditionalSubjectsDialog(extractionModelFrame, this, extractionModel, subject, condition.getText());
		} finally {
			UIUtil.resetWaitCursor(this);
		}
		ArrayList<AdditionalSubject> oldSubjects = new ArrayList<ExtractionModel.AdditionalSubject>(extractionModel.additionalSubjects);
		if (additionalSubjectsDialog.edit()) {
			ArrayList<AdditionalSubject> newSubjects = new ArrayList<ExtractionModel.AdditionalSubject>(extractionModel.additionalSubjects);
			extractionModel.additionalSubjects = oldSubjects;
			changeAdditionalSubjects(newSubjects);
		}
	}//GEN-LAST:event_additionalSubjectsButtonActionPerformed

    private void changeAdditionalSubjects(List<AdditionalSubject> newSubjects) {
    	final ArrayList<AdditionalSubject> oldSubjects = new ArrayList<ExtractionModel.AdditionalSubject>(extractionModel.additionalSubjects);
		extractionModel.additionalSubjects = new ArrayList<>(newSubjects);
		synchronized (currentSubjectClosureLock) {
			currentSubjectClosure = null; // force re-calculation
		}
		markDirty();
		updateAdditionalSubjectsButton();
		pendingDecisionsPanel.updateView();
		undoManager.push(new CompensationAction(1, "changed additional subjects", "changed additional subjects", null) {
			@Override
			public void run() {
				changeAdditionalSubjects(oldSubjects);
			}
		});
	}

	private void activateDesictionPendingButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_activateDesictionPendingButtonActionPerformed
    	pendingDecisionsPanel.toggleDockingState();
    }//GEN-LAST:event_activateDesictionPendingButtonActionPerformed

	/**
	 * Renderer for the tree-view.
	 */
	private TreeCellRenderer getTreeCellRenderer(TreeCellRenderer treeCellRenderer) {
		DefaultTreeCellRenderer r = new DefaultTreeCellRenderer() {
			@Override
			public Component getTreeCellRendererComponent(JTree tree,
					Object value, boolean selected, boolean expanded,
					boolean leaf, int row, boolean hasFocus) {
				String text = "";
				if (value instanceof DefaultMutableTreeNode) {
					DefaultMutableTreeNode node = (DefaultMutableTreeNode) value;
					if (node.getUserObject() instanceof Table) {
						text = dataModel.getDisplayName(((Table) node.getUserObject()));
						setTextColor(tree, Color.BLACK);
					} else if (node.getUserObject() instanceof Association) {
						Association association = (Association) node.getUserObject();
						text = dataModel.getDisplayName(association.destination);
						if (!association.isIgnored() && association.isRestricted()) {
							text += "*";
						} else {
							text += "  ";
						}
						if (association.isIgnored()) {
							setTextColor(tree, ignored.getForeground());
						} else if (association.isInsertDestinationBeforeSource()) {
							setTextColor(tree, dependsOn.getForeground());
						} else if (association.isInsertSourceBeforeDestination()) {
							setTextColor(tree, hasDependent.getForeground());
						} else {
							setTextColor(tree, associatedWith.getForeground());
						}
						return wrapTreeNode(super.getTreeCellRendererComponent(tree, text, selected, expanded, leaf, row, hasFocus), selected, association);
					}
				}
				return super.getTreeCellRendererComponent(tree, text, selected, expanded, leaf, row, hasFocus);
			}
			private void setTextColor(JTree tree, Color color) {
				setTextNonSelectionColor(color);
				if (UIUtil.plaf == PLAF.FLAT) {
					setTextSelectionColor(tree.hasFocus()? Color.white : color);
				}
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
			@Override
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

		switch (UIUtil.plaf) {
			case NATIVE:
				// nothing to do
				break;
			case FLAT:
				// nothing to do
				break;
			case NIMBUS:
				if (((JLabel) component).getText() != null) {
					((JLabel) component).setText(UIUtil.toHTML(((JLabel) component).getText(), 100));
				}
				break;
		}
		
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
			for (boolean checkVisibility: new boolean [] { true, false }) {
				for (DefaultMutableTreeNode node: treeNodes) {
					if (node.getChildCount() > 0) {
						Table t = null;
						Association a = null;
						if (node.getUserObject() instanceof Association) {
							a = (Association) node.getUserObject();
							t = a.destination;
						}
						if (t != null && a != null && table.equals(t)) {
							if (!checkVisibility || (graphView.isTableVisible(a.source) && graphView.isTableVisible(a.destination))) {
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
				}
			}
			Association first = null;
			for (Association a: table.associations) {
				if (first == null || dataModel.getDisplayName(first.destination).compareToIgnoreCase(dataModel.getDisplayName(a.destination)) < 0) {
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
				for (int i = 0; i < 3; ++i) { // lgtm [java/constant-comparison]
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
				pendingDecisionsPanel.selectAssociation(association);
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
						if (association != null) {
							graphView.startScrollTimer(association.destination);
						}
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
			extractionModelFrame.reload.setEnabled(needsSave);
		}
		String file = extractionModelFile;
		graphView.storeLayout();
		if (save(file)) {
			needsSave = false;
			extractionModelFrame.updateTitle(needsSave);
			UISettings.addRecentFile(new File(file));
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
			dataModel.save(fileName, stable, extractionModel.subjectLimitDefinition, condition.getText(), scriptFormat, currentRestrictionDefinitions, null, extractionModel.additionalSubjects, currentModelSubfolder);
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
		++UISettings.s9;
		expandPathsToVisibleTables();
	}

	/**
	 * Expands all associations with visible tables in associations tree.
	 */
	public void expandAllVisibleTables() {
		graphView.expandAll(true, true);
		++UISettings.s9;
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
						addRestriction(association, "", context != null);
					}
//				}
			}
		}
		afterAddRestriction();
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
							addRestriction(association, "false", context != null);
						}
					}
				}
			}
		}
		afterAddRestriction();
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
		addRestriction(association, "false", false);
		afterAddRestriction();
	}

	/**
	 * Removes restrictions.
	 */
	public void removeRestriction(Association association) {
		addRestriction(association, "", true);
		afterAddRestriction();
	}

	/**
	 * Zooms graphical view to fit.
	 */
	public void zoomToFit() {
		graphView.zoomToFit();
	}

	private boolean afterAddRestrictionCallPending = false;

	/**
	 * Adds a restriction to a association.
	 *
	 * @param association the association
	 * @param condition the restriction-condition
	 */
	private boolean addRestriction(final Association association, String condition, final boolean withWhere) {
		return addRestriction(association, condition, withWhere, true);
	}

	/**
	 * Adds a restriction to a association.
	 *
	 * @param association the association
	 * @param condition the restriction-condition
	 */
	private boolean addRestriction(final Association association, String condition, final boolean withWhere, boolean modifyFilter) {
		final String oldRestriction;
		String oc = association.getRestrictionCondition();

		if (oc == null) {
			oc = "";
		} else if ("false".equals(oc)) {
			oc = RestrictionModel.IGNORE;
		}
		oldRestriction = oc;

		if (oldRestriction.equals(condition)) {
			return false;
		}

		dataModel.getRestrictionModel().addRestriction(association, condition, "GUI", true, new HashMap<String, String>());

		if (modifyFilter) {
			if (association.getRestrictionCondition() == null) {
				if (association.hasNullableFK() && association.fkHasNullFilter() && !association.fkHasExcludeFilter()) {
					setOrResetFKNullFilter(association, false);
				}
			}
		}

		pendingDecisionsPanel.decisionMade(association);
		if (withWhere) {
			++UISettings.s4;
		}

		undoManager.push(new CompensationAction(
				100,
				"".equals(condition)? "removed restriction" : ("true".equals(condition) || "ignore".equals(condition))? "disabled assocation" : "added restriction",
				"".equals(oc)? "removed restriction" : ("true".equals(oc) || "ignore".equals(oc))? "disabled assocation" : "added restriction",
				withWhere? dataModel.getDisplayName(association.destination) : null) {
			@Override
			public void run() {
				addRestriction(association, oldRestriction, withWhere, false);
				if (!afterAddRestrictionCallPending) {
					UIUtil.invokeLater(new Runnable() {
						@Override
						public void run() {
							afterAddRestrictionCallPending = false;
							afterAddRestriction();
						}
					});
					afterAddRestrictionCallPending = true;
				}
			}
		});

		return true;
	}

	/**
	 * Model for aggregation combobox.
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
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
		String old = table.getXmlTemplate();
		if (columnMapperDialog.edit(dataModel, table)) {
			String template = table.getXmlTemplate();
			table.setXmlTemplate(old);
			changeXmlTemplate(table, template);
		}
	}

	private void changeXmlTemplate(final Table table, String template) {
		final String old = table.getXmlTemplate();
		table.setXmlTemplate(template);
		updateSketch();
		markDirty();
		undoManager.push(new CompensationAction(1, "changed XML template", "changed XML template", dataModel.getDisplayName(table)) {
			@Override
			public void run() {
				changeXmlTemplate(table, old);
			}
		});
	}

	public void resetUndoStack() {
		undoManager.reset();
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
	
	private Object currentSubjectClosureLock = new Object();

	/**
	 * Gets closure of current subject table.
	 *
	 * @return closure of current subject table
	 */
	public Set<Table> getCurrentSubjectClosure() {
		synchronized (currentSubjectClosureLock) {
			if (dataModel == null || subject == null) {
				return Collections.emptySet();
			}
			if (currentSubjectClosure == null || dataModel.getVersion() != closureVersion) {
				Set<Table> subjects = new HashSet<Table>();
				if (extractionModel.additionalSubjects != null) {
					for (AdditionalSubject as: extractionModel.additionalSubjects) {
						subjects.add(as.getSubject());
					}
				}
				subjects.add(subject);
	
				currentSubjectClosure = new HashSet<Table>();
				for (Table subject: subjects) {
					for (Table table: subject.closure(currentSubjectClosure)) {
						currentSubjectClosure.add(table);
					}
				}
				closureVersion = dataModel.getVersion();
			}
			return currentSubjectClosure;
		}
	}

	private static final int MAX_NAVIGATIONSTACKSIZE = 20;

	private static class Layout {
		Table root;
		Map<String, double[]> positions = new HashMap<String,double[]>();
	}

	private Deque<Layout> navigationStack = new ArrayDeque<Layout>();

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

				if (!navigationStack.isEmpty()) {
					if (navigationStack.peek().positions.keySet().equals(layout.positions.keySet())) {
						return;
					}
				}

				navigationStack.push(layout);
				if (navigationStack.size() > MAX_NAVIGATIONSTACKSIZE) {
					navigationStack.removeLast();
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
			if (!navigationStack.isEmpty()) {
				if (navigationStack.peek().positions.size() > 1400 || navigationStack.peek().positions.keySet().equals(graphView.visibleItems())) {
					navigationStack.pop();
					updateLeftButton();
				}
			}
		}
	}

	public void navigateBack() {
		if (!navigationStack.isEmpty()) {
			Layout layout = navigationStack.pop();
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
					rootTable.setSelectedItem(dataModel.getDisplayName(layout.root));
				}
				rootTableItemStateChangedSetRoot = false;
			} finally {
				--captureLevel;
				rootTableItemStateChangedSetRoot = true;
				executionContext.getLayoutStorage().setTempStorage(null);
			}
		}
		updateLeftButton();
	}

	private void updateLeftButton() {
		leftButton.setVisible(!navigationStack.isEmpty());
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
		undoManager.hideView();
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

    private void setOrResetFKNullFilter(final Association association, final boolean set) {
    	if (association.setOrResetFKNullFilter(set)) {
    		markDirty();
    		undoManager.push(new CompensationAction(1, set? "set filter" : "removed filter", !set? "set filter" : "removed filter", dataModel.getDisplayName(association.source)) {
    			@Override
    			public void run() {
    				setOrResetFKNullFilter(association, !set);
    			}
    		});
    	}
	}

	/**
	 * Gets the undo mananger.
	 *
	 * @return the undo mananger
	 */
	public UndoManager getUndoManager() {
		return undoManager;
	}

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton activateDesictionPendingButton;
    private javax.swing.JButton additionalSubjectsButton;
    private JComboBox2 aggregationCombobox;
    private javax.swing.JLabel assocStatsLabel;
    private javax.swing.JLabel associatedWith;
    javax.swing.JTextField condition;
    public javax.swing.JLabel connectivityState;
    private javax.swing.JLabel dependsOn;
    private javax.swing.JPanel editorPanel;
    public javax.swing.JButton exportButton;
    private JComboBox2 exportFormat;
    private javax.swing.JPanel focusLabelPanel;
    javax.swing.JPanel focusPanel;
    private javax.swing.JPanel graphContainer;
    private javax.swing.JLabel hasDependent;
    private javax.swing.JLabel ignored;
    javax.swing.JPanel inspectorHolder;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel10;
    private javax.swing.JLabel jLabel11;
    private javax.swing.JLabel jLabel12;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel10;
    private javax.swing.JPanel jPanel11;
    private javax.swing.JPanel jPanel12;
    private javax.swing.JPanel jPanel13;
    private javax.swing.JPanel jPanel14;
    private javax.swing.JPanel jPanel15;
    private javax.swing.JPanel jPanel16;
    private javax.swing.JPanel jPanel17;
    private javax.swing.JPanel jPanel18;
    private javax.swing.JPanel jPanel19;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel20;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JPanel jPanel7;
    private javax.swing.JPanel jPanel8;
    private javax.swing.JPanel jPanel9;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane4;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JSplitPane jSplitPane1;
    private javax.swing.JSplitPane jSplitPane2;
    private javax.swing.JPanel jpanel;
    javax.swing.JLayeredPane layeredPane;
    private javax.swing.JButton leftButton;
    private javax.swing.JPanel legende;
    private javax.swing.JPanel legende1;
    private javax.swing.JPanel legende2;
    private javax.swing.JToggleButton limitButton;
    private javax.swing.JLabel limitLabel;
    private javax.swing.JButton mapColumns;
    private javax.swing.JPanel messagePanel;
    javax.swing.JLabel modelName;
    javax.swing.JLabel modelPath;
    public javax.swing.JPanel neighborHolderPanel;
    private javax.swing.JLabel openSubjectConditionEditor;
    private javax.swing.JButton openXmlSettings;
    private javax.swing.JButton resetFocus;
    private javax.swing.JTable restrictionsTable;
    private javax.swing.JPanel rightBorderPanel;
    JComboBox2 rootTable;
    private javax.swing.JTabbedPane sketchTabbedPane;
    private javax.swing.JTextField tagField;
    javax.swing.JPanel toolBarPanel;
    private javax.swing.JPanel toolPanel;
    private javax.swing.JTree tree;
    javax.swing.JPanel undoViewHolder;
    private javax.swing.JPanel xmlMappingPanel;
    private javax.swing.JButton xmlTagApply;
    // End of variables declaration//GEN-END:variables

    JComboBox2 subjectTable;

    Container restrDepsView;

	private Icon dropDownIcon;
	private Icon conditionEditorIcon;
	private ImageIcon resetIcon;
	private Icon conditionEditorSelectedIcon;
	private ImageIcon leftIcon;
	private ImageIcon leftIconP;
	private ImageIcon searchIcon;
	private Icon runIcon;
	{
		// load images
		dropDownIcon = UIUtil.readImage("/dropdown.png");
		resetIcon = UIUtil.readImage("/reset.png");
		conditionEditorIcon = UIUtil.readImage("/edit.png");
		conditionEditorSelectedIcon = UIUtil.readImage("/edit_s.png");
		leftIconP = UIUtil.readImage("/leftp.png");
		leftIcon = UIUtil.readImage("/left.png");
        runIcon = UIUtil.readImage("/run.png");
        searchIcon = UIUtil.readImage("/search.png");
	}

	private static final long serialVersionUID = -5640822484296649670L;

}
