/*
 * Copyright 2007 - 2016 the original author or authors.
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

import java.awt.Cursor;
import java.awt.GridBagConstraints;
import java.awt.Image;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
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
import javax.swing.JLabel;
import javax.swing.JTextField;

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
	private final List<ExtractionModel.AdditionalSubject> subjects;
	private final java.awt.Frame parent;
	private final String subjectCond;
	private final Table subject;
	private Set<Table> remaining = new HashSet<Table>();
	
	/**
     * Creates new form AdditionalSubjectsDialog
     */
    public AdditionalSubjectsDialog(java.awt.Frame parent, ExtractionModel extractionModel, Table subject, String subjectCond) {
        super(parent, true);
        this.parent = parent;
        this.extractionModel = extractionModel;
        this.subjectCond = subjectCond;
        this.subject = subject;
        subjects = new ArrayList<ExtractionModel.AdditionalSubject>(this.extractionModel.additionalSubjects);
        initComponents();
        initSubjectsPanel();
		
		pack();
		setSize(Math.max(700, getWidth()), 500);
		setLocation(parent.getLocation().x + parent.getSize().width / 3 - getWidth() / 2,
    			parent.getLocation().y + parent.getSize().height / 3 - getHeight() / 2);
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

    private List<JTextField> conditionFields = new ArrayList<JTextField>();

	private void readConditions() {
		for (int i = 0; i < conditionFields.size(); ++i) {
			subjects.set(i, new AdditionalSubject(subjects.get(i).subject, conditionFields.get(i).getText().trim()));
		}
	}

	private void initSubjectsPanel() {
		subjectsPanel.removeAll();
		conditionFields.clear();
		
        JLabel jLabel = new JLabel(" ");
        GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 5;
        gridBagConstraints.gridy = 10000;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        subjectsPanel.add(jLabel, gridBagConstraints);
        
        remaining.clear();
        remaining.addAll(extractionModel.dataModel.getTables());
        if (subject != null) {
        	remaining.remove(subject);
        }
        
		for (int i = 0; i < subjects.size(); ++i) {
			final int finalI = i;
			
			remaining.remove(subjects.get(i).subject);
			
			JComboBox jComboBox = new JComboBox();
			jComboBox.setMaximumRowCount(18);
			jComboBox.setModel(subjectListModel(false));
			jComboBox.setSelectedItem(extractionModel.dataModel.getDisplayName(subjects.get(i).subject));

			jComboBox.addItemListener(new ItemListener() {
				@Override
				public void itemStateChanged(ItemEvent e) {
					String item = (String) e.getItem();
					if (item != null) {
						Table table = extractionModel.dataModel.getTableByDisplayName(item);
						if (table != null) {
							readConditions();
							subjects.set(finalI, new AdditionalSubject(table, ""));
						}
					}
				}
			});
			
	        gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 1;
	        gridBagConstraints.gridy = i;
	        gridBagConstraints.insets = new java.awt.Insets(0, 2, 0, 2);
	        subjectsPanel.add(jComboBox, gridBagConstraints);
	        
			final JLabel minusLabel = new javax.swing.JLabel();
			minusLabel.setText(null);
			minusLabel.setIcon(minusImage);
			minusLabel.setToolTipText("delete subject");
	
			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.gridx = 2;
			gridBagConstraints.gridy = i;
			gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
			gridBagConstraints.weightx = 0.0;
			gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
			gridBagConstraints.insets = new Insets(0, 4, 0, 0);
	
			minusLabel.setEnabled(false);
			minusLabel.addMouseListener(new java.awt.event.MouseAdapter() {
				public void mouseEntered(java.awt.event.MouseEvent evt) {
					minusLabel.setEnabled(true);
				}
	
				public void mouseExited(java.awt.event.MouseEvent evt) {
					minusLabel.setEnabled(false);
				}
	
				public void mouseClicked(java.awt.event.MouseEvent evt) {
					readConditions();
					subjects.remove(finalI);
					initSubjectsPanel();
				}
			});
	
			subjectsPanel.add(minusLabel, gridBagConstraints);

			jLabel = new JLabel();
	        jLabel.setText("Where");
	        gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 3;
	        gridBagConstraints.gridy = i;
	        subjectsPanel.add(jLabel, gridBagConstraints);

			final JLabel conditionLabel = new javax.swing.JLabel();
			conditionLabel.setIcon(conditionEditorIcon);
			conditionLabel.setText(null);
			conditionLabel.setToolTipText("open editor");
			conditionLabel.addMouseListener(new java.awt.event.MouseAdapter() {
				@Override
				public void mouseReleased(MouseEvent e) {
					mouseClicked(e);
				}
				@Override
				public void mouseClicked(MouseEvent e) {
					readConditions();
					ParameterSelector.ParametersGetter parametersGetter = new ParameterSelector.ParametersGetter() {
						@Override
						public Set<String> getParameters() {
							return extractionModel.dataModel.getParameters(subjectCond, subjects);
						}
					};
					String cond = new ConditionEditor(parent, parametersGetter).edit(subjects.get(finalI).condition, "Additional Subject", "T", subjects.get(finalI).subject, null, null, null, false);
					if (cond != null) {
						subjects.set(finalI, new AdditionalSubject(subjects.get(finalI).subject, ConditionEditor.toSingleLine(cond)));
						initSubjectsPanel();
					}
					conditionLabel.setIcon(conditionEditorSelectedIcon);
				}
				
				public void mouseEntered(java.awt.event.MouseEvent evt) {
					conditionLabel.setIcon(conditionEditorSelectedIcon);
	            }
	            public void mouseExited(java.awt.event.MouseEvent evt) {
	            	conditionLabel.setIcon(conditionEditorIcon);
	           }
	        });
			
			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.gridx = 4;
			gridBagConstraints.gridy = i;
			gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
			gridBagConstraints.weightx = 0.0;
			gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
			gridBagConstraints.insets = new Insets(0, 4, 0, 0);
	
			subjectsPanel.add(conditionLabel, gridBagConstraints);

			JTextField jTextField = new JTextField();
			conditionFields.add(jTextField);
			jTextField.setToolTipText("SQL expression. Keep empty if you want to export all rows.");
			jTextField.setText(subjects.get(i).condition);
	        gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 5;
	        gridBagConstraints.gridy = i;
	        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
	        gridBagConstraints.weightx = 1.0;
	        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 2);
	        subjectsPanel.add(jTextField, gridBagConstraints);
		}
		JComboBox jComboBox = new JComboBox();
		jComboBox.setMaximumRowCount(18);
		jComboBox.setModel(subjectListModel(true));
		jComboBox.setSelectedItem("");

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = subjects.size() + 1;
        gridBagConstraints.insets = new java.awt.Insets(0, 2, 0, 2);
        subjectsPanel.add(jComboBox, gridBagConstraints);
        
		jComboBox.addItemListener(new ItemListener() {
			@Override
			public void itemStateChanged(ItemEvent e) {
				String item = (String) e.getItem();
				if (item != null) {
					Table table = extractionModel.dataModel.getTableByDisplayName(item);
					if (table != null) {
						readConditions();
						subjects.add(new AdditionalSubject(table, ""));
						initSubjectsPanel();
					}
				}
			}
		});
		jScrollPane1.setViewportView(jPanel1);
		
		addAllButton.setEnabled(!remaining.isEmpty());
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

        jPanel3 = new javax.swing.JPanel();
        okButton = new javax.swing.JButton();
        cancelButton = new javax.swing.JButton();
        addAllButton = new javax.swing.JButton();
        jScrollPane1 = new javax.swing.JScrollPane();
        jPanel1 = new javax.swing.JPanel();
        subjectsPanel = new javax.swing.JPanel();

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

        addAllButton.setText("Add all tables");
        addAllButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                addAllButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 2, 2);
        jPanel3.add(addAllButton, gridBagConstraints);

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
    	readConditions();
    	Collections.sort(subjects, new Comparator<ExtractionModel.AdditionalSubject>() {
			@Override
			public int compare(AdditionalSubject o1, AdditionalSubject o2) {
				return extractionModel.dataModel.getDisplayName(o1.subject).compareTo(extractionModel.dataModel.getDisplayName(o2.subject));
			}
		});
    	synchronized (this) {
    		ok = !extractionModel.additionalSubjects.equals(subjects);
		}
    	extractionModel.additionalSubjects = new ArrayList<ExtractionModel.AdditionalSubject>(subjects);
    	dispose();
    }//GEN-LAST:event_okButtonActionPerformed

    private void addAllButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_addAllButtonActionPerformed
    	try {
    		setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
    		readConditions();
    	
			List<Table> tables = new ArrayList<Table>(remaining);
			Collections.sort(tables);
			
			for (Table table: tables) {
				subjects.add(new AdditionalSubject(table, ""));
	    	}
	    	
			initSubjectsPanel();
		} finally {
			setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		}
    }//GEN-LAST:event_addAllButtonActionPerformed

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton addAllButton;
    private javax.swing.JButton cancelButton;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JButton okButton;
    private javax.swing.JPanel subjectsPanel;
    // End of variables declaration//GEN-END:variables

    private ImageIcon minusImage = null;
    private ImageIcon conditionEditorIcon = null;
    private ImageIcon conditionEditorSelectedIcon = null;

	private boolean ok;

	{
		String dir = "/net/sf/jailer/resource";

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
