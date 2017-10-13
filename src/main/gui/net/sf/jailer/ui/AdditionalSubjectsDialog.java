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

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.Image;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.ImageIcon;
import javax.swing.JComponent;

import net.sf.jailer.datamodel.Table;
import net.sf.jailer.extractionmodel.ExtractionModel;
import net.sf.jailer.extractionmodel.ExtractionModel.AdditionalSubject;

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
	
	@SuppressWarnings("serial")
	private class AdditionalSubjectListEditor extends ListEditor<AdditionalSubject> {

		public AdditionalSubjectListEditor() {
			super(new String[] { "Table", "Condition" }, "Additional Subject", true, false, false);
			hideUpAndDownButton();
		}

		@Override
		protected String getDisplayName(AdditionalSubject element) {
			return extractionModel.dataModel.getDisplayName(element.getSubject());
		}

		@Override
		protected AdditionalSubject copy(AdditionalSubject element) {
			return new AdditionalSubject(element.getSubject(), element.getCondition());
		}

		@Override
		protected AdditionalSubject createNew() {
			return new AdditionalSubject(null, "");
		}

		@Override
		protected JComponent createDetailsView(AdditionalSubject element) {
			detailsComboBox.setModel(subjectListModel(true));
			if (element.getSubject() != null) {
				detailsComboBox.setSelectedItem(extractionModel.dataModel.getDisplayName(element.getSubject()));
			} else {
				detailsComboBox.setSelectedItem("");
			}
			detailsCondtition.setText(element.getCondition());
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
		}

		@Override
		protected Object[] toColumnList(AdditionalSubject element, int index) {
			return new String[] { getDisplayName(element), element.getCondition().length() > 0? ("Where " + element.getCondition()) : "all rows" };
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
		
	};
	
	private AdditionalSubjectListEditor additionalSubjectListEditor;
	
	
	/**
	 * Creates new form AdditionalSubjectsDialog
	 */
	public AdditionalSubjectsDialog(java.awt.Frame parent, final ExtractionModel extractionModel, Table subject, String subjectCond) {
		super(parent, true);
		this.parent = parent;
		this.extractionModel = extractionModel;
		this.subjectCond = subjectCond;
		this.subject = subject;
		subjects = new ArrayList<ExtractionModel.AdditionalSubject>(this.extractionModel.additionalSubjects);
		additionalSubjectListEditor = new AdditionalSubjectListEditor();
		additionalSubjectListEditor.setModel(subjects);
		initComponents();
		
		AutoCompletion.enable(detailsComboBox);
		GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        detailsPanel.add(StringSearchPanel.createSearchButton(parent, detailsComboBox, "Find Table", null), gridBagConstraints);

		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 1;
		gridBagConstraints.gridy = 1;
		gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
		gridBagConstraints.weightx = 1.0;
		gridBagConstraints.weighty = 1.0;
		subjectsPanel.add(additionalSubjectListEditor, gridBagConstraints);
		
		detailsLabel.setIcon(conditionEditorIcon);
		detailsLabel.setText(null);
		detailsLabel.setToolTipText("open editor");
		final ParameterSelector.ParametersGetter parametersGetter = new ParameterSelector.ParametersGetter() {
			@Override
			public Set<String> getParameters() {
				return AdditionalSubjectsDialog.this.extractionModel.dataModel.getParameters(AdditionalSubjectsDialog.this.subjectCond, subjects);
			}
		};
		detailsLabel.addMouseListener(new java.awt.event.MouseAdapter() {
			@Override
			public void mouseReleased(MouseEvent e) {
				mouseClicked(e);
			}
			public void mouseClicked(java.awt.event.MouseEvent evt) {
				ConditionEditor conditionEditor = new ConditionEditor(AdditionalSubjectsDialog.this.parent, parametersGetter, AdditionalSubjectsDialog.this.extractionModel.dataModel);
				Object item = detailsComboBox.getSelectedItem();
				conditionEditor.setTitle("");
				Table table = null;
				if (item != null) {
					table = extractionModel.dataModel.getTableByDisplayName(item.toString());
					conditionEditor.setTitle(item.toString());
				}
				String cond = conditionEditor.edit(detailsCondtition.getText(), "Subject", "T", table, null, null, null, false);
				if (cond != null) {
					if (!detailsCondtition.getText().equals(ConditionEditor.toSingleLine(cond))) {
						detailsCondtition.setText(ConditionEditor.toSingleLine(cond));
					}
					detailsLabel.setIcon(conditionEditorIcon);
				}
			}
			
			public void mouseEntered(java.awt.event.MouseEvent evt) {
				detailsLabel.setIcon(conditionEditorSelectedIcon);
			}
			public void mouseExited(java.awt.event.MouseEvent evt) {
				detailsLabel.setIcon(conditionEditorIcon);
		   }
		});
		
		collectRemaining();
		
		pack();
		setSize(Math.max(700, getWidth()), 500);
		setLocation(parent.getLocation().x + parent.getSize().width / 3 - getWidth() / 2,
				parent.getLocation().y + parent.getSize().height / 3 - getHeight() / 2);
		UIUtil.fit(this);
		UIUtil.initPeer();
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
		Collections.sort(tableNames);
		if (withNull) {
			tableNames.add(0, "");
		}
		DefaultComboBoxModel<String> model = new DefaultComboBoxModel<String>(tableNames);
		return model;
	}

	private void collectRemaining() {
		remaining.clear();
		remaining.addAll(extractionModel.dataModel.getTables());
		if (subject != null) {
			remaining.remove(subject);
		}
		
		for (int i = 0; i < subjects.size(); ++i) {
			remaining.remove(subjects.get(i).getSubject());
		}
				
		addAllButton.setEnabled(!remaining.isEmpty());
		removeAllButton.setEnabled(!subjects.isEmpty());
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
        detailsComboBox = new javax.swing.JComboBox();
        detailsLabel = new javax.swing.JLabel();
        detailsCondtition = new javax.swing.JTextField();
        detailsWhere = new javax.swing.JLabel();
        jPanel3 = new javax.swing.JPanel();
        okButton = new javax.swing.JButton();
        cancelButton = new javax.swing.JButton();
        addAllButton = new javax.swing.JButton();
        removeAllButton = new javax.swing.JButton();
        jScrollPane1 = new javax.swing.JScrollPane();
        jPanel1 = new javax.swing.JPanel();
        subjectsPanel = new javax.swing.JPanel();

        detailsPanel.setLayout(new java.awt.GridBagLayout());

        detailsComboBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        detailsPanel.add(detailsComboBox, gridBagConstraints);

        detailsLabel.setText("jLabel1");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        detailsPanel.add(detailsLabel, gridBagConstraints);

        detailsCondtition.setText("jTextField1");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        detailsPanel.add(detailsCondtition, gridBagConstraints);

        detailsWhere.setText("   Where ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        detailsPanel.add(detailsWhere, gridBagConstraints);

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Additional Subjects");
        getContentPane().setLayout(new java.awt.GridBagLayout());

        jPanel3.setLayout(new java.awt.GridBagLayout());

        okButton.setText(" Ok ");
        okButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                okButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 2, 2);
        jPanel3.add(okButton, gridBagConstraints);

        cancelButton.setText("Cancel");
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 2, 2);
        jPanel3.add(cancelButton, gridBagConstraints);

        addAllButton.setText("Add remaining tables");
        addAllButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                addAllButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 2, 2);
        jPanel3.add(addAllButton, gridBagConstraints);

        removeAllButton.setText("Delete all");
        removeAllButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                removeAllButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 2, 2);
        jPanel3.add(removeAllButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
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
			setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
			
			List<Table> tables = new ArrayList<Table>(remaining);
			Collections.sort(tables);
			
			for (Table table: tables) {
				subjects.add(new AdditionalSubject(table, ""));
			}
			sortSubjects();
			
			additionalSubjectListEditor.setModel(subjects);
			collectRemaining();
		} finally {
			setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		}
	}//GEN-LAST:event_addAllButtonActionPerformed

	private void removeAllButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_removeAllButtonActionPerformed
		subjects.clear();
		additionalSubjectListEditor.setModel(subjects);
		collectRemaining();
	}//GEN-LAST:event_removeAllButtonActionPerformed

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton addAllButton;
    private javax.swing.JButton cancelButton;
    private javax.swing.JComboBox detailsComboBox;
    private javax.swing.JTextField detailsCondtition;
    private javax.swing.JLabel detailsLabel;
    private javax.swing.JPanel detailsPanel;
    private javax.swing.JLabel detailsWhere;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JButton okButton;
    private javax.swing.JButton removeAllButton;
    private javax.swing.JPanel subjectsPanel;
    // End of variables declaration//GEN-END:variables

	private ImageIcon minusImage = null;
	private ImageIcon conditionEditorIcon = null;
	private ImageIcon conditionEditorSelectedIcon = null;

	private boolean ok;

	{
		String dir = "/net/sf/jailer/ui/resource";

		// load image
		try {
			conditionEditorSelectedIcon = new ImageIcon(getClass().getResource(dir + "/edit_s.png"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		try {
			conditionEditorIcon = new ImageIcon(getClass().getResource(dir + "/edit.png"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		try {
			minusImage = new ImageIcon(new ImageIcon(getClass().getResource(
					dir + "/minus.png")).getImage().getScaledInstance(22, 18,
					Image.SCALE_SMOOTH));
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
