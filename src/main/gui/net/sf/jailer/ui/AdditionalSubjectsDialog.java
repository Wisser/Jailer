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
package net.sf.jailer.ui;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;

import net.sf.jailer.datamodel.Table;
import net.sf.jailer.extractionmodel.ExtractionModel;
import net.sf.jailer.extractionmodel.ExtractionModel.AdditionalSubject;
import net.sf.jailer.extractionmodel.SubjectLimitDefinition;

/**
 * Dialog for additional subjects.
 * 
 * @author Ralf Wisser
 */
public class AdditionalSubjectsDialog extends javax.swing.JDialog {
	
	private static final long serialVersionUID = -4301921544746428522L;

	private final ExtractionModel extractionModel;
	private final List<AdditionalSubject> subjects;
	private final java.awt.Frame parent;
	private final String subjectCond;
	private final Table subject;
	private Set<Table> remaining = new HashSet<Table>();
	private Set<Table> remainingIsolated = new HashSet<Table>();
	private Set<Table> isolated = new HashSet<Table>();
	private SubjectLimitDefinition currentLimitDefinition;

	@SuppressWarnings("serial")
	private class AdditionalSubjectListEditor extends ListEditor<AdditionalSubject> {

		public AdditionalSubjectListEditor() {
			super(new String[] { "Table", "Condition", "Limit" }, "Additional Subject", true, false, false);
			hideUpAndDownButton();
		}

		@Override
		protected String getDisplayName(AdditionalSubject element) {
			return extractionModel.dataModel.getDisplayName(element.getSubject());
		}

		@Override
		protected AdditionalSubject copy(AdditionalSubject element) {
			AdditionalSubject ld = new AdditionalSubject(element.getSubject(), element.getCondition(), element.getSubjectLimitDefinition());
			return ld;
		}

		@Override
		protected AdditionalSubject createNew() {
			return new AdditionalSubject(null, "", new SubjectLimitDefinition(null, null));
		}

		@Override
		protected JComponent createDetailsView(AdditionalSubject element) {
			detailsComboBox.setModel(subjectListModel(true));
			if (element.getSubject() != null) {
				detailsComboBox.setSelectedItem(extractionModel.dataModel.getDisplayName(detailsTable = element.getSubject()));
			} else {
				detailsTable = null;
				detailsComboBox.setSelectedItem("");
			}
			whereConEditorButton.setEnabled(detailsTable != null);
			detailsCondtition.setText(element.getCondition());
			currentLimitDefinition = element.getSubjectLimitDefinition();
        	limitLabel.setText(SubjectLimitEditor.subjectLimitDefinitionRender(currentLimitDefinition));
			return detailsPanel;
		}

		@Override
		protected void updateFromDetailsView(AdditionalSubject element,
				JComponent detailsView, List<AdditionalSubject> model,
				StringBuilder errorMessage) {
			
			String cond = detailsCondtition.getText().trim();
			
			Table table = null;
			Object item = detailsComboBox.getSelectedItem();
			if (item != null) {
				table = extractionModel.dataModel.getTableByDisplayName(item.toString());
			}

			if (table == null) {
				errorMessage.append("Subject missing");
				return;
			}
			element.setSubject(table);
			element.setCondition(cond);
			element.setSubjectLimitDefinition(currentLimitDefinition);
		}

		@Override
		protected Object[] toColumnList(AdditionalSubject element, int index) {
			return new String[] { 
					getDisplayName(element),
					element.getCondition().length() > 0 ? ("Where " + element.getCondition()) : "all rows",
					element.getSubjectLimitDefinition().limit == null? "" : (element.getSubjectLimitDefinition().limit + " rows" + (element.getSubjectLimitDefinition().orderBy != null ? " (ordered)" : ""))
				};
		}

		@Override
		protected Color getForegroundColor(AdditionalSubject element, int column) {
			return null;
		}

		/* (non-Javadoc)
		 * @see net.sf.jailer.ui.ListEditor#detailsViewMinSize()
		 */
		@Override
		protected Dimension detailsViewMinSize() {
			return new Dimension(500, 10);
		}

		/* (non-Javadoc)
		 * @see net.sf.jailer.ui.ListEditor#onModelUpdate()
		 */
		@Override
		protected void onModelUpdate() {
			collectRemaining();
		}
		
	}

	private AdditionalSubjectListEditor additionalSubjectListEditor;
	private Table detailsTable;
	private JButton whereConEditorButton;
	
	/**
	 * Creates new form AdditionalSubjectsDialog
	 * @param extractionModelEditor 
	 */
	public AdditionalSubjectsDialog(Frame parent, ExtractionModelEditor extractionModelEditor, final ExtractionModel extractionModel, Table subject, String subjectCond) {
		super(parent, true);
		this.parent = parent;
		this.extractionModel = extractionModel;
		this.subjectCond = subjectCond;
		this.subject = subject;
		subjects = new ArrayList<ExtractionModel.AdditionalSubject>(this.extractionModel.additionalSubjects);
		additionalSubjectListEditor = new AdditionalSubjectListEditor();
		additionalSubjectListEditor.setModel(subjects);
		initComponents();
		
		okButton.setIcon(UIUtil.scaleIcon(okButton, okIcon));
		cancelButton.setIcon(UIUtil.scaleIcon(cancelButton, cancelIcon));
		
		AutoCompletion.enable(detailsComboBox);
		GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        detailsPanel.add(StringSearchPanel.createSearchButton(null, detailsComboBox, "Find Table", null), gridBagConstraints);
        
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        whereConEditorButton = extractionModelEditor.createWhereConEditorButton(() -> detailsTable, () -> detailsCondtition.getText(), s -> {
        	detailsCondtition.setText(UIUtil.toSingleLineSQL(s));
        }, true, "T", null);
		whereConEditorButton.setEnabled(detailsTable != null);
		detailsPanel.add(whereConEditorButton, gridBagConstraints);

		detailsComboBox.addItemListener(e -> {
        	detailsTable = null;
    		Object item = detailsComboBox.getSelectedItem();
    		if (item != null) {
    			detailsTable = extractionModel.dataModel.getTableByDisplayName(item.toString());
    		}
    		whereConEditorButton.setEnabled(detailsTable != null);
        });

		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 1;
		gridBagConstraints.gridy = 1;
		gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
		gridBagConstraints.weightx = 1.0;
		gridBagConstraints.weighty = 1.0;
		subjectsPanel.add(additionalSubjectListEditor, gridBagConstraints);
		
		detailsButton.setIcon(conditionEditorIcon);
		detailsButton.setText(null);
		detailsButton.setToolTipText("open SQL editor");
		final ParameterSelector.ParametersGetter parametersGetter = new ParameterSelector.ParametersGetter() {
			@Override
			public Set<String> getParameters() {
				return AdditionalSubjectsDialog.this.extractionModel.dataModel.getParameters(AdditionalSubjectsDialog.this.subjectCond, subjects);
			}
		};
		detailsButton.addActionListener(e -> {
			ConditionEditor conditionEditor = new ConditionEditor(detailsButton, AdditionalSubjectsDialog.this.parent,
					parametersGetter, AdditionalSubjectsDialog.this.extractionModel.dataModel, null);
			Object item = detailsComboBox.getSelectedItem();
			conditionEditor.setTitle("");
			Table table = null;
			if (item != null) {
				table = extractionModel.dataModel.getTableByDisplayName(item.toString());
				conditionEditor.setTitle(item.toString());
			}
			String cond = conditionEditor.edit(detailsCondtition, detailsCondtition.getText(), "Subject", "T", table, null, null, null,
					false, true);
			if (cond != null) {
				if (!detailsCondtition.getText().equals((cond))) {
					detailsCondtition.setText((cond));
				}
				detailsButton.setIcon(conditionEditorIcon);
			}
		});
		
		collectRemaining();
		
		pack();
		setSize(Math.max(700, getWidth()), 500);
		setLocation(parent.getLocation().x + parent.getSize().width / 3 - getWidth() / 2,
				parent.getLocation().y + parent.getSize().height / 3 - getHeight() / 2);
		UIUtil.fit(this);
	}

	public boolean edit() {
		ok = false;
		setVisible(true);
		synchronized (this) {
			return ok;	
		}
	}

	/**
	 * Gets list model for the subject-combobox.
	 * 
	 * @return list model for the subject-combobox
	 */
	private ComboBoxModel<String> subjectListModel(boolean withNull) {
		Vector<String> tableNames = new Vector<String>();
		for (Table table: extractionModel.dataModel.getTables()) {
			tableNames.add(extractionModel.dataModel.getDisplayName(table));
		}
		Collections.sort(tableNames, String::compareToIgnoreCase);
		if (withNull) {
			tableNames.add(0, "");
		}
		DefaultComboBoxModel<String> model = new DefaultComboBoxModel<String>(tableNames);
		return model;
	}

	private void collectRemaining() {
		isolated.clear();
		remaining.clear();
		remaining.addAll(extractionModel.dataModel.getTables());
		if (subject != null) {
			remaining.remove(subject);
		}
		
		for (int i = 0; i < subjects.size(); ++i) {
			Table sub = subjects.get(i).getSubject();
			remaining.remove(sub);
			isolated.add(sub);
		}

		Set<Table> closure;
		if (subject != null) {
			closure = subject.closure();
		} else {
			closure = new HashSet<Table>();
		}

		remainingIsolated.clear();
		remainingIsolated.addAll(remaining);
		remainingIsolated.removeAll(closure);
		isolated.removeAll(closure);
		
		addAllButton.setEnabled(!remaining.isEmpty());
		addIsolatedlButton.setEnabled(!remainingIsolated.isEmpty());
		removeAllButton.setEnabled(!subjects.isEmpty());
		removeIsolatedlButton.setEnabled(!isolated.isEmpty());
	}

	/**
	 * This method is called from within the constructor to initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is always
	 * regenerated by the Form Editor.
	 */
	@SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        detailsPanel = new javax.swing.JPanel();
        detailsComboBox = new JComboBox2();
        detailsCondtition = new javax.swing.JTextField();
        detailsWhere = new javax.swing.JLabel();
        jPanel13 = new javax.swing.JPanel();
        limitLabel = new javax.swing.JLabel();
        limitButton = new javax.swing.JToggleButton();
        detailsWhere1 = new javax.swing.JLabel();
        detailsButton = new javax.swing.JButton();
        jPanel3 = new javax.swing.JPanel();
        okButton = new javax.swing.JButton();
        cancelButton = new javax.swing.JButton();
        addAllButton = new javax.swing.JButton();
        removeAllButton = new javax.swing.JButton();
        addIsolatedlButton = new javax.swing.JButton();
        removeIsolatedlButton = new javax.swing.JButton();
        jScrollPane1 = new javax.swing.JScrollPane();
        jPanel1 = new javax.swing.JPanel();
        subjectsPanel = new javax.swing.JPanel();

        detailsPanel.setLayout(new java.awt.GridBagLayout());

        detailsComboBox.setMaximumRowCount(20);
        detailsComboBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 8);
        detailsPanel.add(detailsComboBox, gridBagConstraints);

        detailsCondtition.setText("jTextField1");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 5;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        detailsPanel.add(detailsCondtition, gridBagConstraints);

        detailsWhere.setText("Where ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        detailsPanel.add(detailsWhere, gridBagConstraints);

        jPanel13.setLayout(new java.awt.GridBagLayout());

        limitLabel.setText("<html><i>no limit</i></html>");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 0);
        jPanel13.add(limitLabel, gridBagConstraints);

        limitButton.setText("Edit Limit");
        limitButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                limitButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        jPanel13.add(limitButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
        detailsPanel.add(jPanel13, gridBagConstraints);

        detailsWhere1.setText("Limit ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        detailsPanel.add(detailsWhere1, gridBagConstraints);

        detailsButton.setText("jButton1");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 1;
        detailsPanel.add(detailsButton, gridBagConstraints);

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Additional Subjects");
        getContentPane().setLayout(new java.awt.GridBagLayout());

        jPanel3.setLayout(new java.awt.GridBagLayout());

        okButton.setText("Ok");
        okButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                okButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 2);
        jPanel3.add(okButton, gridBagConstraints);

        cancelButton.setText("Cancel");
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 5;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 2);
        jPanel3.add(cancelButton, gridBagConstraints);

        addAllButton.setText("Add all");
        addAllButton.setToolTipText("Add all remaining tables.");
        addAllButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                addAllButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 2, 2);
        jPanel3.add(addAllButton, gridBagConstraints);

        removeAllButton.setText("Remove all");
        removeAllButton.setToolTipText("Remove all tables.");
        removeAllButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                removeAllButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 2);
        jPanel3.add(removeAllButton, gridBagConstraints);

        addIsolatedlButton.setText("Add isolated tables");
        addIsolatedlButton.setToolTipText("Add all remaining tables that are not in the closure of the primary subject.");
        addIsolatedlButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                addIsolatedlButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 2, 2);
        jPanel3.add(addIsolatedlButton, gridBagConstraints);

        removeIsolatedlButton.setText("Remove isolated tables");
        removeIsolatedlButton.setToolTipText("Remove all tables that are not in the closure of the primary subject.");
        removeIsolatedlButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                removeIsolatedlButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 2);
        jPanel3.add(removeIsolatedlButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 4, 4);
        getContentPane().add(jPanel3, gridBagConstraints);

        jScrollPane1.setHorizontalScrollBarPolicy(javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);

        jPanel1.setLayout(new java.awt.GridBagLayout());

        subjectsPanel.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel1.add(subjectsPanel, gridBagConstraints);

        jScrollPane1.setViewportView(jPanel1);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 0);
        getContentPane().add(jScrollPane1, gridBagConstraints);

        pack();
    }// </editor-fold>//GEN-END:initComponents

	private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
		dispose();
	}//GEN-LAST:event_cancelButtonActionPerformed

	private void okButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_okButtonActionPerformed
		sortSubjects();
		synchronized (this) {
			ok = !extractionModel.additionalSubjects.equals(subjects);
		}
		extractionModel.additionalSubjects = new ArrayList<ExtractionModel.AdditionalSubject>(subjects);
		dispose();
	}//GEN-LAST:event_okButtonActionPerformed

	void sortSubjects() {
		Collections.sort(subjects, new Comparator<ExtractionModel.AdditionalSubject>() {
			@Override
			public int compare(AdditionalSubject o1, AdditionalSubject o2) {
				return extractionModel.dataModel.getDisplayName(o1.getSubject()).compareTo(extractionModel.dataModel.getDisplayName(o2.getSubject()));
			}
		});
	}

	private void addAllButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_addAllButtonActionPerformed
		try {
			UIUtil.setWaitCursor(this);
			
			List<Table> tables = new ArrayList<Table>(remaining);
			Collections.sort(tables);
			
			for (Table table: tables) {
				subjects.add(new AdditionalSubject(table, "", new SubjectLimitDefinition(null, null)));
			}
			sortSubjects();
			
			additionalSubjectListEditor.setModel(subjects);
			collectRemaining();
		} finally {
			UIUtil.resetWaitCursor(this);
		}
	}//GEN-LAST:event_addAllButtonActionPerformed

	private void removeAllButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_removeAllButtonActionPerformed
		subjects.clear();
		additionalSubjectListEditor.setModel(subjects);
		collectRemaining();
	}//GEN-LAST:event_removeAllButtonActionPerformed

    private void addIsolatedlButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_addIsolatedlButtonActionPerformed
    	try {
			UIUtil.setWaitCursor(this);
			
			List<Table> tables = new ArrayList<Table>(remainingIsolated);
			Collections.sort(tables);
			
			for (Table table: tables) {
				subjects.add(new AdditionalSubject(table, "", new SubjectLimitDefinition(null, null)));
			}
			sortSubjects();
			
			additionalSubjectListEditor.setModel(subjects);
			collectRemaining();
		} finally {
			UIUtil.resetWaitCursor(this);
		}
    }//GEN-LAST:event_addIsolatedlButtonActionPerformed

    private void removeIsolatedlButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_removeIsolatedlButtonActionPerformed
    	for (Iterator<AdditionalSubject> si = subjects.iterator(); si.hasNext(); ) {
    		AdditionalSubject sub = si.next();
    		if (isolated.contains(sub.getSubject())) {
    			si.remove();
    		}
    	}
		additionalSubjectListEditor.setModel(subjects);
		collectRemaining();
    }//GEN-LAST:event_removeIsolatedlButtonActionPerformed

    private SubjectLimitEditor subjectLimitEditor;

    @SuppressWarnings("serial")
	private void limitButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_limitButtonActionPerformed
        if (limitButton.isSelected()) {
            if (subjectLimitEditor == null) {
                subjectLimitEditor = new SubjectLimitEditor(this, extractionModel.dataModel, true) {
                    @Override
                    protected void consume(SubjectLimitDefinition subjectLimitDefinition) {
                        if (subjectLimitDefinition != null) {
                        	currentLimitDefinition = subjectLimitDefinition;
                        	limitLabel.setText(SubjectLimitEditor.subjectLimitDefinitionRender(subjectLimitDefinition));
                        }
                        limitButton.setSelected(false);
                    }
                };
            }
            limitLabel.setText(SubjectLimitEditor.subjectLimitDefinitionRender(currentLimitDefinition));
            Table table = null;
			Object item = detailsComboBox.getSelectedItem();
			if (item != null) {
				table = extractionModel.dataModel.getTableByDisplayName(item.toString());
			}
			subjectLimitEditor.edit(limitLabel, table, currentLimitDefinition);
			limitButton.setSelected(false);
        }
    }//GEN-LAST:event_limitButtonActionPerformed

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton addAllButton;
    private javax.swing.JButton addIsolatedlButton;
    private javax.swing.JButton cancelButton;
    private javax.swing.JButton detailsButton;
    private JComboBox2 detailsComboBox;
    private javax.swing.JTextField detailsCondtition;
    private javax.swing.JPanel detailsPanel;
    private javax.swing.JLabel detailsWhere;
    private javax.swing.JLabel detailsWhere1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel13;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JToggleButton limitButton;
    private javax.swing.JLabel limitLabel;
    private javax.swing.JButton okButton;
    private javax.swing.JButton removeAllButton;
    private javax.swing.JButton removeIsolatedlButton;
    private javax.swing.JPanel subjectsPanel;
    // End of variables declaration//GEN-END:variables

	private ImageIcon conditionEditorIcon = null;

	private boolean ok;
	private static ImageIcon okIcon;
	private static ImageIcon cancelIcon;
	
	static {
        // load images
        okIcon = UIUtil.readImage("/buttonok.png");
        cancelIcon = UIUtil.readImage("/buttoncancel.png");
	}
	
	{
		// load image
		conditionEditorIcon = UIUtil.readImage("/edit.png");
	}
}
