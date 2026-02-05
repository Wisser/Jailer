/*
 * Copyright 2007 - 2026 Ralf Wisser.
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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

import javax.swing.ButtonGroup;
import javax.swing.DefaultComboBoxModel;
import javax.swing.ImageIcon;

import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.subsetting.ScriptFormat;

/**
 * Dialog for XML settings.
 * 
 * @author Ralf Wisser
 */
public class XmlSettingsDialog extends javax.swing.JDialog {
	
	/** Creates new form XmlSettingsDialog */
	public XmlSettingsDialog(java.awt.Frame parent) {
		super(parent, true);
		initComponents(); UIUtil.initComponents(this);
		setModal(true);
		
		ButtonGroup buttonGroup = new ButtonGroup();
		buttonGroup.add(multipleRoots);
		buttonGroup.add(singleRoot);

		buttonGroup = new ButtonGroup(); 
		buttonGroup.add(include);
		buttonGroup.add(ignore);
		buttonGroup.add(disallow);
		
		UIUtil.setInitialWindowLocation(this, parent, 100, 150);
		Ok.setIcon(UIUtil.scaleIcon(Ok, okIcon));
		cancelButton.setIcon(UIUtil.scaleIcon(cancelButton, cancelIcon));
		datePattern.getEditor().addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				updateExamples();
			}
		});
		timestampPattern.getEditor().addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				updateExamples();
			}
		});
		datePattern.setModel(new DefaultComboBoxModel(datePatternOptions));
		timestampPattern.setModel(new DefaultComboBoxModel(timePatternOptions));
		timestampPattern.getEditor().setItem("yyyy.MM.dd.-H.mm.ss   ");
		timestampExample.setText("yyyy.MM.dd.-H.mm.ss   ");
		pack();
	}
	
	private boolean okClicked;
	
	/**
	 * Edits the XML settings of a data model.
	 * 
	 * @param dataModel the data model
	 * @param scriptFormat 
	 * @return <code>true</code> iff settings are changed
	 */
	public boolean edit(DataModel dataModel, ScriptFormat scriptFormat) {
		DataModel.XmlSettings xmlSettings = dataModel.getXmlSettings();
		
		datePattern.getEditor().setItem(xmlSettings.datePattern);
		timestampPattern.getEditor().setItem(xmlSettings.timestampPattern);
		rootTag.setText(xmlSettings.rootTag);
		
		rootTag.setVisible(scriptFormat == ScriptFormat.XML);
		xmlRootTagLabel.setVisible(scriptFormat == ScriptFormat.XML);
		
		unformattedCheckbox.setSelected(xmlSettings.unformatted);
		unformattedCheckbox.setVisible(scriptFormat != ScriptFormat.YAML);
		
		singleRoot.setSelected(xmlSettings.singleRoot);
		multipleRoots.setSelected(!xmlSettings.singleRoot);
		ignore.setSelected(xmlSettings.ignoreNonAggregated);
		include.setSelected(xmlSettings.includeNonAggregated);
		disallow.setSelected(xmlSettings.disallowNonAggregated);
		rootTag.setEditable(!xmlSettings.singleRoot);
		updateExamples();
		
		okClicked = false;
		setVisible(true);
		if (okClicked) {
			boolean change = false;
			String newDatePattern = datePattern.getEditor().getItem().toString();
			String newTimestampPattern = timestampPattern.getEditor().getItem().toString();
			String newRootTag = rootTag.getText().trim();
			if (newRootTag.isEmpty()) {
				newRootTag = "root";
			}
			if (!newDatePattern.equals(xmlSettings.datePattern)) {
				xmlSettings.datePattern = newDatePattern;
				change = true;
			}
			if (!newTimestampPattern.equals(xmlSettings.timestampPattern)) {
				xmlSettings.timestampPattern = newTimestampPattern;
				change = true;
			}
			if (!newRootTag.equals(xmlSettings.rootTag)) {
				xmlSettings.rootTag = newRootTag;
				change = true;
			}
			if (singleRoot.isSelected() != xmlSettings.singleRoot) {
				xmlSettings.singleRoot = singleRoot.isSelected();
				change = true;
			}
			if (ignore.isSelected() != xmlSettings.ignoreNonAggregated) {
				xmlSettings.ignoreNonAggregated = ignore.isSelected();
				change = true;
			}
			if (include.isSelected() != xmlSettings.includeNonAggregated) {
				xmlSettings.includeNonAggregated = include.isSelected();
				change = true;
			}
			if (disallow.isSelected() != xmlSettings.disallowNonAggregated) {
				xmlSettings.disallowNonAggregated = disallow.isSelected();
				change = true;
			}
			if (unformattedCheckbox.isSelected() != xmlSettings.unformatted) {
				xmlSettings.unformatted = unformattedCheckbox.isSelected();
				change = true;
			}
			return change;
		}
		return false;
	}

	private void updateExamples() {
		Date now = new Date();
		String dateExampleText = "- invalid pattern -";
		String timestampExampleText = "- invalid pattern -";
		try {
			dateExampleText = new SimpleDateFormat(datePattern.getEditor().getItem().toString(), Locale.ENGLISH).format(now);
		} catch (Exception e) {
			// ignore
		}
		try {
			timestampExampleText = new SimpleDateFormat(timestampPattern.getEditor().getItem().toString(), Locale.ENGLISH).format(now);
		} catch (Exception e) {
			// ignore
		}
		dateExample.setText(dateExampleText);
		timestampExample.setText(timestampExampleText);
	}
	
	private static String[] datePatternOptions = new String[] {
		"dd-MM-yyyy",
		"dd/MM/yyyy", "dd.MM.yyyy", "dd.MMM.yyyy", "dd MMMM yyyy",
		"MM-dd-yyyy", "MM/dd/yyyy", "MMM d, yyyy", "yyyy.d.M",
		"dd MMM yyyy", "dd-MMM-yyyy", "dd.MM.yyyy.", "d MMM yyyy",
		"d MMM, yyyy", "d-MMM-yyyy", "d/MMM/yyyy", "d/MM/yyyy",
		"d.MM.yyyy", "d.M.yyyy", "Gy.MM.dd", "yyyy-M-d", "yyyy/M/d",
		"yyyy. M. d", "yyyy.M.d", "yyyy'?'M'?'d'?'", "yyyy-MM-dd",
		"yyyy/MM/dd", "yyyy.MM.dd", "yyyy.MM.dd.", "yyyy MMM d",
		"yyyy-MMM-dd", "dd MMM yy", "dd-MMM-yy", "MM d, yy" };

	private static String[] timePatternOptions = new String[] {
		"yyyy-MM-dd'T'HH:mm:ss",
		"yyyy-MM-dd'T'HH:mm:ss.SSS",
		"dd/MM/yyyy HH:mm:ss", "dd.MM.yyyy HH:mm:ss",
		"dd.MM.yyyy. HH.mm.ss", 
		"dd.MM.yyyy H:mm:ss",
		"yyyy-MM-dd HH:mm:ss",
		"yyyy.MM.dd HH:mm:ss", "yyyy/MM/dd H:mm:ss", "yyyy.MM.dd. H:mm:ss",
		"yyyy-MMM-dd HH:mm:ss",

		"dd/MM/yyyy HH.mm.ss", "dd.MM.yyyy HH.mm.ss",
		"dd.MM.yyyy. HH.mm.ss", "dd.MM.yyyy H.mm.ss",
		"yyyy-MM-dd HH.mm.ss",
		"yyyy.MM.dd HH.mm.ss", "yyyy/MM/dd H.mm.ss", "yyyy.MM.dd. H.mm.ss",
		"yyyy-MMM-dd HH.mm.ss",

		"dd/MM/yyyy-HH.mm.ss", "dd.MM.yyyy-HH.mm.ss",
		"dd.MM.yyyy.-HH.mm.ss", 
		"dd.MM.yyyy-H.mm.ss",
		"yyyy-MM-dd-HH.mm.ss",
		"yyyy.MM.dd-HH.mm.ss", "yyyy/MM/dd-H.mm.ss", "yyyy.MM.dd.-H.mm.ss",
		"yyyy-MMM-dd-HH.mm.ss",

		"dd MMM yy H:mm:ss",
		"dd MMM yyyy HH:mm:ss", "dd-MMM-yyyy HH:mm:ss",
		"dd.MMM.yyyy HH:mm:ss", "dd-MMM-yyyy H:mm:ss",
		"dd-MM-yyyy HH:mm:ss",
		"d MMM yyyy HH:mm:ss", "d-MMM-yyyy HH:mm:ss", "d MMM yyyy H:mm:ss",
		"d MMM yyyy, H:mm:ss", "d-MMM-yyyy H:mm:ss", "d-MMM-yyyy H.mm.ss",
		"d/MMM/yyyy H:mm:ss",
		"d/MM/yyyy HH:mm:ss", "d.MM.yyyy H:mm:ss",
		"d.M.yyyy HH:mm:", "d.M.yyyy HH:mm:ss", "d.M.yyyy H:mm:ss",
		"d.M.yyyy H.mm.ss", "Gy.MM.dd H:mm:ss", "HH:mm:ss dd-MM-yyyy",
		"HH:mm:ss dd/MM/yyyy",
		"yyyy.d.M HH:mm:ss",
		"yyyy.M.d HH.mm.ss",
		"yyyy MMM d HH:mm:ss" };

	/** This method is called from within the constructor to
	 * initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is
	 * always regenerated by the Form Editor.
	 */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jPanel3 = new javax.swing.JPanel();
        jPanel1 = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        xmlRootTagLabel = new javax.swing.JLabel();
        datePattern = new javax.swing.JComboBox();
        timestampPattern = new javax.swing.JComboBox();
        rootTag = new javax.swing.JTextField();
        dateExample = new javax.swing.JLabel();
        timestampExample = new javax.swing.JLabel();
        singleRoot = new javax.swing.JCheckBox();
        jPanel2 = new javax.swing.JPanel();
        Ok = new javax.swing.JButton();
        cancelButton = new javax.swing.JButton();
        xmlRootTagLabel1 = new javax.swing.JLabel();
        multipleRoots = new javax.swing.JCheckBox();
        include = new javax.swing.JCheckBox();
        xmlRootTagLabel2 = new javax.swing.JLabel();
        xmlRootTagLabel3 = new javax.swing.JLabel();
        ignore = new javax.swing.JCheckBox();
        disallow = new javax.swing.JCheckBox();
        unformattedCheckbox = new javax.swing.JCheckBox();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("XML Settings");
        getContentPane().setLayout(new java.awt.GridBagLayout());

        jPanel3.setLayout(new java.awt.GridBagLayout());

        jPanel1.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        jPanel3.add(jPanel1, gridBagConstraints);

        jLabel1.setText("Date pattern ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 5;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        jPanel3.add(jLabel1, gridBagConstraints);

        jLabel2.setText("Timestamp pattern  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 5;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        jPanel3.add(jLabel2, gridBagConstraints);

        xmlRootTagLabel.setText("Root tag (XML)");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 5;
        gridBagConstraints.gridy = 15;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(8, 0, 0, 0);
        jPanel3.add(xmlRootTagLabel, gridBagConstraints);

        datePattern.setEditable(true);
        datePattern.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Eintrag 1", "Eintrag 2", "Eintrag 3", "Eintrag 4" }));
        datePattern.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                datePatternActionPerformed(evt);
            }
        });
        datePattern.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyTyped(java.awt.event.KeyEvent evt) {
                datePatternKeyTyped(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        jPanel3.add(datePattern, gridBagConstraints);

        timestampPattern.setEditable(true);
        timestampPattern.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Eintrag 1", "Eintrag 2", "Eintrag 3", "Eintrag 4" }));
        timestampPattern.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                timestampPatternActionPerformed(evt);
            }
        });
        timestampPattern.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyTyped(java.awt.event.KeyEvent evt) {
                timestampPatternKeyTyped(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        jPanel3.add(timestampPattern, gridBagConstraints);

        rootTag.setText("jTextField1");
        rootTag.setToolTipText("Name of the top-level element in the XML export file. Only relevant if \"Multiple Objects (Array)\" is selected.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 15;
        gridBagConstraints.gridwidth = 20;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(8, 0, 0, 0);
        jPanel3.add(rootTag, gridBagConstraints);

        dateExample.setText("jLabel4");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 20;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 8);
        jPanel3.add(dateExample, gridBagConstraints);

        timestampExample.setText("jLabel5");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 20;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 8);
        jPanel3.add(timestampExample, gridBagConstraints);

        singleRoot.setText("Single Object");
        singleRoot.setToolTipText("<html>Writes a single root/subject object with all its aggregated sub-objects. <br>The export process fails if more than one such object exists.</html>");
        singleRoot.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                singleRootItemStateChanged(evt);
            }
        });
        singleRoot.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                singleRootActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(8, 0, 0, 0);
        jPanel3.add(singleRoot, gridBagConstraints);

        jPanel2.setLayout(new java.awt.GridBagLayout());

        Ok.setText("OK");
        Ok.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                OkActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHEAST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 2);
        jPanel2.add(Ok, gridBagConstraints);

        cancelButton.setText("Cancel");
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHEAST;
        jPanel2.add(cancelButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 100;
        gridBagConstraints.gridwidth = 20;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 2, 0);
        jPanel3.add(jPanel2, gridBagConstraints);

        xmlRootTagLabel1.setText("Root content");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 5;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(8, 0, 0, 0);
        jPanel3.add(xmlRootTagLabel1, gridBagConstraints);

        multipleRoots.setText("Multiple Objects (Array)");
        multipleRoots.setToolTipText("<html>Writes an array/collection of all root/subject objects with all their aggregated sub-objects.</html>");
        multipleRoots.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                multipleRootsItemStateChanged(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 22;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        jPanel3.add(multipleRoots, gridBagConstraints);

        include.setText("Include");
        include.setToolTipText("Write out all objects that are not aggregated in any other object at root level.");
        include.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                includeItemStateChanged(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 24;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(8, 0, 0, 0);
        jPanel3.add(include, gridBagConstraints);

        xmlRootTagLabel2.setText("objects");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 5;
        gridBagConstraints.gridy = 25;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel3.add(xmlRootTagLabel2, gridBagConstraints);

        xmlRootTagLabel3.setText("Non-aggregated ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 5;
        gridBagConstraints.gridy = 24;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(8, 0, 0, 0);
        jPanel3.add(xmlRootTagLabel3, gridBagConstraints);

        ignore.setText("Ignore");
        ignore.setToolTipText("Ignore all objects that are not aggregated in any other object.");
        ignore.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                ignoreItemStateChanged(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 25;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        jPanel3.add(ignore, gridBagConstraints);

        disallow.setText("Disallow");
        disallow.setToolTipText("Raise an error if there are objects that are not aggregated into any other object.");
        disallow.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                disallowItemStateChanged(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 26;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        jPanel3.add(disallow, gridBagConstraints);

        unformattedCheckbox.setText("Unformatted");
        unformattedCheckbox.setToolTipText("If selected, output the result without formatting/indentation (compact).");
        unformattedCheckbox.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                unformattedCheckboxItemStateChanged(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 18;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(8, 0, 0, 0);
        jPanel3.add(unformattedCheckbox, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 4, 2, 4);
        getContentPane().add(jPanel3, gridBagConstraints);

        pack();
    }// </editor-fold>//GEN-END:initComponents

	private void timestampPatternKeyTyped(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_timestampPatternKeyTyped
		updateExamples();
	}//GEN-LAST:event_timestampPatternKeyTyped

	private void timestampPatternActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_timestampPatternActionPerformed
		updateExamples();
	}//GEN-LAST:event_timestampPatternActionPerformed

	private void datePatternKeyTyped(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_datePatternKeyTyped
		updateExamples();
	}//GEN-LAST:event_datePatternKeyTyped

	private void datePatternActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_datePatternActionPerformed
		updateExamples();
	}//GEN-LAST:event_datePatternActionPerformed

	private void OkActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_OkActionPerformed
		okClicked = true;
		setVisible(false);
	}//GEN-LAST:event_OkActionPerformed

	private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
		dispose();
	}//GEN-LAST:event_cancelButtonActionPerformed

    private void singleRootItemStateChanged(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_singleRootItemStateChanged
        rootTag.setEditable(!multipleRoots.isSelected());
    }//GEN-LAST:event_singleRootItemStateChanged

    private void multipleRootsItemStateChanged(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_multipleRootsItemStateChanged
    	 rootTag.setEditable(!multipleRoots.isSelected());
    }//GEN-LAST:event_multipleRootsItemStateChanged

    private void includeItemStateChanged(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_includeItemStateChanged
        // TODO add your handling code here:
    }//GEN-LAST:event_includeItemStateChanged

    private void ignoreItemStateChanged(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_ignoreItemStateChanged
        // TODO add your handling code here:
    }//GEN-LAST:event_ignoreItemStateChanged

    private void disallowItemStateChanged(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_disallowItemStateChanged
        // TODO add your handling code here:
    }//GEN-LAST:event_disallowItemStateChanged

    private void singleRootActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_singleRootActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_singleRootActionPerformed

    private void unformattedCheckboxItemStateChanged(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_unformattedCheckboxItemStateChanged
        // TODO add your handling code here:
    }//GEN-LAST:event_unformattedCheckboxItemStateChanged
	
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton Ok;
    private javax.swing.JButton cancelButton;
    private javax.swing.JLabel dateExample;
    private javax.swing.JComboBox datePattern;
    private javax.swing.JCheckBox disallow;
    private javax.swing.JCheckBox ignore;
    private javax.swing.JCheckBox include;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JCheckBox multipleRoots;
    private javax.swing.JTextField rootTag;
    private javax.swing.JCheckBox singleRoot;
    private javax.swing.JLabel timestampExample;
    private javax.swing.JComboBox timestampPattern;
    private javax.swing.JCheckBox unformattedCheckbox;
    private javax.swing.JLabel xmlRootTagLabel;
    private javax.swing.JLabel xmlRootTagLabel1;
    private javax.swing.JLabel xmlRootTagLabel2;
    private javax.swing.JLabel xmlRootTagLabel3;
    // End of variables declaration//GEN-END:variables
	
	private static ImageIcon okIcon;
	private static ImageIcon cancelIcon;
	
	static {
        // load images
        okIcon = UIUtil.readImage("/buttonok.png");
        cancelIcon = UIUtil.readImage("/buttoncancel.png");
	}

	private static final long serialVersionUID = -2752715206964965549L;
}

