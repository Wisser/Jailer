/*
 * Copyright 2007 - 2025 Ralf Wisser.
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
import java.awt.GridBagConstraints;
import java.awt.Point;
import java.awt.event.ActionListener;
import java.net.URI;
import java.util.Collections;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.DefaultComboBoxModel;
import javax.swing.ImageIcon;
import javax.swing.JScrollPane;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.fife.ui.rsyntaxtextarea.SyntaxConstants;
import org.w3c.dom.Document;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.subsetting.ScriptFormat;
import net.sf.jailer.ui.syntaxtextarea.RSyntaxTextAreaWithTheme;
import net.sf.jailer.xml.XmlUtil;

/**
 * XML Column Mapping Dialog.
 *
 * @author Ralf Wisser
 */
public class ColumnMapperDialog extends javax.swing.JDialog {
	
	private final java.awt.Frame parent;
	private Table table;
	private DataModel dataModel;
	private ScriptFormat scriptFormat;
	private boolean ok;
	private ParameterSelector parameterSelector;
	private String initialTemplate = "";
	private ExecutionContext executionContext;
	
	/** Creates new form ColumnMapperDialog */
	public ColumnMapperDialog(java.awt.Frame parent, ParameterSelector.ParametersGetter parametersGetter, ExecutionContext executionContext) {
		super(parent, true);
		this.executionContext = executionContext;
		this.parent = parent;
		this.mappingField = new RSyntaxTextAreaWithTheme();
		mappingField.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_XML);
		mappingField.setCodeFoldingEnabled(true);

		initComponents(); UIUtil.initComponents(this);

		okButton.setIcon(UIUtil.scaleIcon(okButton, okIcon));
		cancelButton.setIcon(UIUtil.scaleIcon(cancelButton, cancelIcon));
		helpButton.setIcon(UIUtil.scaleIcon(helpButton, helpIcon));
		
		JScrollPane jScrollPane2 = new JScrollPane();
		jScrollPane2.setViewportView(mappingField);
		GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 1;
		gridBagConstraints.gridy = 20;
		gridBagConstraints.gridwidth = 2;
		gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
		gridBagConstraints.weightx = 1.0;
		gridBagConstraints.weighty = 1.0;
		gridBagConstraints.insets = new java.awt.Insets(4, 4, 0, 4);
		jPanel4.add(jScrollPane2, gridBagConstraints);
		jScrollPane2.setViewportView(mappingField);
		
		xmlSketchScrollPane = new JScrollPane();
		xmlSketch = new RSyntaxTextAreaWithTheme();
		
		xmlSketch.setEditable(false);
		xmlSketchScrollPane.setViewportView(xmlSketch);
		xmlSketch.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_XML);
		xmlSketch.setCodeFoldingEnabled(true);

		xmlSketch.setText("");
		xmlSketch.setCaretPosition(0);
		gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.weightx = 1;
        gridBagConstraints.weighty = 1;
        gridBagConstraints.fill = GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        jLayeredPane1.add(xmlSketchScrollPane, gridBagConstraints);
        
        AutoCompletion.enable(tableCombobox);
		
		paramPanel.add(parameterSelector = new ParameterSelector(this, mappingField, parametersGetter));
		tableCombobox.addActionListener(new ActionListener() {
			 @Override
			public void actionPerformed(java.awt.event.ActionEvent e) {
				 try {
					 Table t = dataModel.getTableByDisplayName((String) tableCombobox.getSelectedItem());
					 if (t != null) {
						 table = t;
						 setMappingFieldText(XmlUtil.build(table.getXmlTemplateAsDocument(null)));
						 mappingField.discardAllEdits();
						 updateSketch(table);
						 updateSketchUpdateButtons();
					 }
				 } catch (Exception ex) {
					UIUtil.showException(ColumnMapperDialog.this.parent, "Error", ex);
				 }
			 }
		});
		
		mappingField.getDocument().addDocumentListener(new DocumentListener() {
			@Override
			public void removeUpdate(DocumentEvent e) {
				update();
			}
			@Override
			public void insertUpdate(DocumentEvent e) {
				update();
			}
			@Override
			public void changedUpdate(DocumentEvent e) {
				update();
			}
			private void update() {
				if (sketchUpdateCheckbox.isSelected()) {
					updateSketch(table);
				} else {
					stale = false;
					updateStaleRender();
				}
			}
		});
	}

	/**
	 * Edits column mapping for a given table.
	 * 
	 * @param dataModel the data model
	 * @param table the table
	 */
	public boolean edit(DataModel dataModel, Table table, ScriptFormat scriptFormat) {
		this.table = table;
		this.dataModel = dataModel;
		this.scriptFormat = scriptFormat;
		parameterSelector.updateParameters();
		
		stdBg = new RSyntaxTextAreaWithTheme().getBackground();
		
		Vector<String> tableNames = new Vector<String>();
		for (Table t: dataModel.getTables()) {
			tableNames.add(dataModel.getDisplayName(t));
		}
		Collections.sort(tableNames);
		tableCombobox.setModel(new DefaultComboBoxModel(tableNames));
		tableCombobox.setMaximumRowCount(40);
		tableCombobox.setSelectedItem(dataModel.getDisplayName(table));
		tableCombobox.setVisible(false);
		jLabel1.setVisible(false);
		tableLabel.setText(dataModel.getDisplayName(table));
		int w = 1200, h = 600;
		setSize(w, h);
		setLocation(parent.getX() + parent.getWidth() / 2 - w / 2,
					parent.getY() + parent.getHeight() / 2 - h / 2);
		jSplitPane1.setDividerLocation((int) (w * 0.66));
		invalidate();
		try {
			setMappingFieldText(XmlUtil.build(table.getXmlTemplateAsDocument(null)));
			initialTemplate = mappingField.getText();
			updateSketch(table);
		} catch (Exception e) {
			try {
				// try again with default template,
				// there was a bug in Jailer 3.0 which causes corruption of XML templates
				// on windows platform
				setMappingFieldText(XmlUtil.build(table.getDefaultXmlTemplate(null)));
			} catch (Exception e2) {
				UIUtil.showException(parent, "Error", e);
				return false;
			}
		}
		
		updateSketchUpdateButtons();
		
		mappingField.discardAllEdits();
		ok = false;
		setVisible(true);
		return ok;
	}

	private void updateSketchUpdateButtons() {
		if (mappingField.getText().length() < 40000) {
			sketchUpdateCheckbox.setSelected(true);
			sketchUpdateButton.setEnabled(false);
		} else {
			sketchUpdateCheckbox.setSelected(false);
			sketchUpdateButton.setEnabled(true);
		}
	}
	
	private JScrollPane xmlSketchScrollPane;
	private RSyntaxTextArea xmlSketch;
	
	/**
	 * Adds a tab to the sketch-tabbedpane for a given table.
	 *
	 * @param table the table
	 */
	private void updateSketch(Table table) {
		if (mappingField.getText().isEmpty()) {
			return;
		}
		String sketch = "";
		String oldTemplate = table.getXmlTemplate();
		try {
			table.setXmlTemplateInternal(mappingField.getText());
			sketch = XmlSketchBuilder.buildSketch(table, 1, scriptFormat, executionContext);
			if (scriptFormat != ScriptFormat.XML) {
				Pattern pattern = Pattern.compile("\"?j\\:comment\"?\\s*\\:\\s*\"(.*?)/j\\:comment\"(?:\\,)?(?:\\:)?");
				Matcher matcher = pattern.matcher(sketch);
				boolean result = matcher.find();
				StringBuffer sb = new StringBuffer();
				if (result) {
					do {
						String comment = matcher.group(1);
						matcher.appendReplacement(sb, (scriptFormat == ScriptFormat.YAML? "# " : "// ") + Matcher.quoteReplacement(comment.replace("\\\"", "\f").replace("\"", "").replace("\f", "\"")));
						result = matcher.find();
					} while (result);
				}
				matcher.appendTail(sb);
				sketch = sb.toString();
			}
			stale = false;
		} catch (Exception e) {
			stale = true;
		} finally {
			table.setXmlTemplateInternal(oldTemplate);
		}
		if (scriptFormat == ScriptFormat.XML) {
			xmlSketch.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_XML);
		}
		if (scriptFormat == ScriptFormat.JSON) {
			xmlSketch.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_JSON_WITH_COMMENTS);
		}
		if (scriptFormat == ScriptFormat.YAML) {
			xmlSketch.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_YAML);
		}
		
		xmlSketch.setCurrentLineHighlightColor(new Color(0, 0, 0, 0));
		if (stale) {
			updateStaleRender();
		} else {
			Point pos = xmlSketchScrollPane.getViewport().getViewPosition();
			xmlSketch.setBackground(stdBg);
			xmlSketch.setText(sketch);
			xmlSketch.setEnabled(true);
			UIUtil.invokeLater(() -> xmlSketchScrollPane.getViewport().setViewPosition(pos));
		}
	}
	
	private void updateStaleRender() {
		xmlSketch.setBackground(Colors.Color_224_224_224);
	}
	
	private boolean stale = false;
	private Color stdBg;
	
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
        tableCombobox = new JComboBox2();
        jSplitPane1 = new javax.swing.JSplitPane();
        jPanel4 = new javax.swing.JPanel();
        jLabel2 = new javax.swing.JLabel();
        paramPanel = new javax.swing.JPanel();
        jPanel2 = new javax.swing.JPanel();
        formatButton = new javax.swing.JButton();
        resetButton = new javax.swing.JButton();
        helpButton = new javax.swing.JButton();
        okButton = new javax.swing.JButton();
        cancelButton = new javax.swing.JButton();
        jPanel5 = new javax.swing.JPanel();
        jLabel3 = new javax.swing.JLabel();
        jPanel6 = new javax.swing.JPanel();
        sketchUpdateButton = new javax.swing.JButton();
        sketchUpdateCheckbox = new javax.swing.JCheckBox();
        jPanel7 = new javax.swing.JPanel();
        jLayeredPane1 = new javax.swing.JLayeredPane();
        tableLabel = new javax.swing.JLabel();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("XML Column Mapping");
        getContentPane().setLayout(new java.awt.GridBagLayout());

        jPanel3.setLayout(new java.awt.GridBagLayout());

        jPanel1.setLayout(new java.awt.GridBagLayout());

        jLabel1.setText(" Table ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 8, 0);
        jPanel1.add(jLabel1, gridBagConstraints);

        tableCombobox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Eintrag 1", "Eintrag 2", "Eintrag 3", "Eintrag 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 8, 0);
        jPanel1.add(tableCombobox, gridBagConstraints);

        jSplitPane1.setDividerLocation(300);

        jPanel4.setLayout(new java.awt.GridBagLayout());

        jLabel2.setText(" Mapping  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel4.add(jLabel2, gridBagConstraints);

        paramPanel.setMinimumSize(new java.awt.Dimension(150, 0));
        paramPanel.setLayout(new javax.swing.BoxLayout(paramPanel, javax.swing.BoxLayout.LINE_AXIS));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        jPanel4.add(paramPanel, gridBagConstraints);

        jPanel2.setLayout(new java.awt.GridBagLayout());

        formatButton.setText("Format");
        formatButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                formatButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 2);
        jPanel2.add(formatButton, gridBagConstraints);

        resetButton.setText("Reset");
        resetButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                resetButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 2);
        jPanel2.add(resetButton, gridBagConstraints);

        helpButton.setText("Help");
        helpButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                helpButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 2);
        jPanel2.add(helpButton, gridBagConstraints);

        okButton.setText("Ok");
        okButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                okButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 2);
        jPanel2.add(okButton, gridBagConstraints);

        cancelButton.setText("Cancel");
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });
        jPanel2.add(cancelButton, new java.awt.GridBagConstraints());

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 30;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
        jPanel4.add(jPanel2, gridBagConstraints);

        jSplitPane1.setLeftComponent(jPanel4);

        jPanel5.setLayout(new java.awt.GridBagLayout());

        jLabel3.setText("Sketch");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 2, 0, 0);
        jPanel5.add(jLabel3, gridBagConstraints);

        jPanel6.setLayout(new java.awt.GridBagLayout());

        sketchUpdateButton.setText("Update");
        sketchUpdateButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                sketchUpdateButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 2);
        jPanel6.add(sketchUpdateButton, gridBagConstraints);

        sketchUpdateCheckbox.setText("Automatic Update");
        sketchUpdateCheckbox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                sketchUpdateCheckboxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        jPanel6.add(sketchUpdateCheckbox, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 30;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
        jPanel5.add(jPanel6, gridBagConstraints);

        jPanel7.setLayout(new java.awt.GridBagLayout());

        jLayeredPane1.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel7.add(jLayeredPane1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        jPanel5.add(jPanel7, gridBagConstraints);

        jSplitPane1.setRightComponent(jPanel5);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel1.add(jSplitPane1, gridBagConstraints);

        tableLabel.setFont(tableLabel.getFont().deriveFont(tableLabel.getFont().getStyle() | java.awt.Font.BOLD));
        tableLabel.setText(" Table ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 8, 0);
        jPanel1.add(tableLabel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel3.add(jPanel1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        getContentPane().add(jPanel3, gridBagConstraints);

        pack();
    }// </editor-fold>//GEN-END:initComponents

	private void resetButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_resetButtonActionPerformed
		try {
			mappingField.beginAtomicEdit();
			setMappingFieldText(XmlUtil.build(table.getXmlTemplateAsDocument(XmlUtil.build(table.getDefaultXmlTemplate(null)), null)));
			mappingField.endAtomicEdit();
			mappingField.grabFocus();
		} catch (Exception e) {
			UIUtil.showException(parent, "Syntax Error", e);
		}
	}//GEN-LAST:event_resetButtonActionPerformed

	private void formatButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_formatButtonActionPerformed
		try {
			Document doc = XmlUtil.parse(mappingField.getText());
			mappingField.beginAtomicEdit();
			setMappingFieldText(XmlUtil.build(doc));
			mappingField.endAtomicEdit();
			mappingField.grabFocus();
		} catch (Exception e) {
			UIUtil.showException(parent, "Syntax Error", e, UIUtil.EXCEPTION_CONTEXT_USER_ERROR);
		}
	}//GEN-LAST:event_formatButtonActionPerformed

	private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
		ok = false;
		setVisible(false);
	}//GEN-LAST:event_cancelButtonActionPerformed

	private void okButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_okButtonActionPerformed
		try {
			Document doc = XmlUtil.parse(mappingField.getText());
			setMappingFieldText(XmlUtil.build(doc));
			if (!initialTemplate.equals(mappingField.getText())) {
				table.setXmlTemplate(mappingField.getText());
				ok = true;
			} else {
				ok = false;
			}
			setVisible(false);
		} catch (Exception e) {
			UIUtil.showException(parent, "Syntax Error", e, UIUtil.EXCEPTION_CONTEXT_USER_ERROR);
		}
	}//GEN-LAST:event_okButtonActionPerformed

    private void sketchUpdateButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_sketchUpdateButtonActionPerformed
    	updateSketch(table);
    }//GEN-LAST:event_sketchUpdateButtonActionPerformed

    private void sketchUpdateCheckboxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_sketchUpdateCheckboxActionPerformed
    	updateSketch(table);
    	sketchUpdateButton.setEnabled(!sketchUpdateCheckbox.isSelected());
    }//GEN-LAST:event_sketchUpdateCheckboxActionPerformed

    private void helpButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_helpButtonActionPerformed
    	try {
			BrowserLauncher.openURL(new URI("https://wisser.github.io/Jailer/exporting-xml.html"), this);
		} catch (Exception e) {
			UIUtil.showException(this, "Error", e);
		}
    }//GEN-LAST:event_helpButtonActionPerformed
	
    private void setMappingFieldText(String text) {
		mappingField.setText(text.replace("\r", ""));
		mappingField.setCaretPosition(0);
	}

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton cancelButton;
    private javax.swing.JButton formatButton;
    private javax.swing.JButton helpButton;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLayeredPane jLayeredPane1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JPanel jPanel7;
    private javax.swing.JSplitPane jSplitPane1;
    private javax.swing.JButton okButton;
    private javax.swing.JPanel paramPanel;
    private javax.swing.JButton resetButton;
    private javax.swing.JButton sketchUpdateButton;
    private javax.swing.JCheckBox sketchUpdateCheckbox;
    private JComboBox2 tableCombobox;
    private javax.swing.JLabel tableLabel;
    // End of variables declaration//GEN-END:variables

    private final RSyntaxTextArea mappingField;
    
    private static ImageIcon okIcon;
	private static ImageIcon cancelIcon;
	private static ImageIcon helpIcon;
	
    static {
        // load images
        okIcon = UIUtil.readImage("/buttonok.png");
        cancelIcon = UIUtil.readImage("/buttoncancel.png");
        helpIcon = UIUtil.readImage("/help.png");
	}
    
	private static final long serialVersionUID = -5437578641818236294L;
}
