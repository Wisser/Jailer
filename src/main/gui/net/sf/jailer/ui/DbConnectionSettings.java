/*
/*
 * Copyright 2007 - 2019 Ralf Wisser.
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

import java.awt.Component;
import java.awt.Dialog;
import java.awt.Font;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListCellRenderer;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JTextField;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import org.fife.rsta.ui.EscapableDialog;

import net.sf.jailer.util.CsvFile.Line;
import net.sf.jailer.util.Pair;

/**
 * Connection Setting Dialog.
 * 
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public class DbConnectionSettings extends javax.swing.JPanel {

	private final Component owner;
	private JDialog dialog;
	private List<String> names = new ArrayList<String>();
	private List<String> defValues = new ArrayList<String>();
	private List<Boolean> isOptional = new ArrayList<Boolean>();
	private Map<Integer, Map<String, String>> oldContent = new HashMap<Integer, Map<String, String>>();
	private JLabel pLabel[];
	private JTextField pTextField[];
	private JButton pButton[];
	private String url;

	/**
     * Creates new form DBConnectionSetings
     */
    public DbConnectionSettings(Component root) {
    	this.owner = root;
        initComponents();
    	pLabel = new JLabel[] { paramLabel1, paramLabel2, paramLabel3, paramLabel4 };
    	pTextField = new JTextField[] { paramField1, paramField2, paramField3, paramField4 };
    	pButton = new JButton[] { paramButton1, paramButton2, paramButton3, paramButton4 };
    	for (int i = 0; i < pLabel.length; ++i) {
    		final int finalI = i;
    		pTextField[i].addFocusListener(new FocusListener() {
				@Override
				public void focusLost(FocusEvent e) {
				}
				@Override
				public void focusGained(FocusEvent e) {
					if (pTextField[finalI].getText().length() == 0 || pTextField[finalI].getText().startsWith("<") && pTextField[finalI].getText().endsWith(">")) {
						String def = defValues.get(finalI);
						pTextField[finalI].setText(def == null? "" : def);
						pTextField[finalI].selectAll();
					}
				}
			});
			pTextField[i].getDocument().addDocumentListener(new DocumentListener() {
				@Override
				public void removeUpdate(DocumentEvent e) {
					updateUrl();
				}
				@Override
				public void insertUpdate(DocumentEvent e) {
					updateUrl();
				}
				@Override
				public void changedUpdate(DocumentEvent e) {
					updateUrl();
				}
			});
			pButton[i].setText(null);
			pButton[i].addActionListener(new java.awt.event.ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					String fn = UIUtil.choseFile(null, new File(".").getAbsolutePath(), pLabel[finalI].getText(), "", dialog, false, true, false);
					if (fn != null) {
						pTextField[finalI].setText(new File(fn).getAbsolutePath());
					}
				}
			});
    	}
    }

	public Pair<String, String> edit(List<Line> lines) {
		DefaultComboBoxModel<Line> dbmses = new DefaultComboBoxModel<Line>();
		for (Line line: lines) {
			if (line.cells.get(0).length() > 0) {
				dbmses.addElement(line);
			}
		}
		url = "";
		dbmsComboBox.setModel(dbmses);
		dbmsComboBox.setRenderer(new DefaultListCellRenderer() {
			@Override
			public Component getListCellRendererComponent(JList<?> list, Object value, int index, boolean isSelected,
					boolean cellHasFocus) {
				Line line = null;
				if (value instanceof Line) {
					line = (Line) value;
					value = line.cells.get(0);
				}
				Component r = super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
				if (line != null && r instanceof JLabel) {
					if (line.cells.get(1) != null && line.cells.get(1).length() == 0) {
						((JLabel) r).setFont(((JLabel) r).getFont().deriveFont((((JLabel) r).getFont().getStyle() | Font.ITALIC)));
					}
				}
				return r;
			}
		});
		dbmsComboBox.setSelectedIndex(0);
		String titel = "Connection Settings";
		if (owner instanceof Frame) {
			dialog = new EscapableDialog((Frame) owner, titel, true) {
			};
		} else if (owner instanceof Dialog) {
			dialog = new EscapableDialog((Dialog) owner, titel, true) {
			};
		} else {
			dialog = new EscapableDialog((Dialog) null, titel, true) {
			};
		}

		dialog.getContentPane().add(this);
		for (int i = 0; i < pLabel.length; ++i) {
			pLabel[i].setVisible(true);
			pTextField[i].setVisible(true);
			pButton[i].setVisible(true);
		}
		dialog.pack();
		dialog.setSize(Math.max(dialog.getWidth(), 540), dialog.getHeight());
		dialog.setLocation(
				owner.getLocation().x + owner.getWidth() /2 - dialog.getWidth() /2,
				owner.getLocation().y + owner.getHeight() /2 - dialog.getHeight() /2);
		dialog.setModal(true);
		updateFields();
		dbmsComboBox.grabFocus();

		ok = false;
		dialog.setVisible(true);
		if (ok && dbmses.getSelectedItem() instanceof Line) {
			return new Pair<String, String>(((Line) dbmses.getSelectedItem()).cells.get(0), urlLabel.getText());
		} else {
			return null;
		}
	}

	private int selIndex = -1;

	private void updateFields() {
		if (dbmsComboBox.getSelectedItem() instanceof Line) {
			url = ((Line) dbmsComboBox.getSelectedItem()).cells.get(1);
		}

		if (!oldContent.containsKey(selIndex)) {
			oldContent.put(selIndex, new HashMap<String, String>());
		}
		
		for (int i = 0; i < pTextField.length; ++i) {
			if (pTextField[i].isVisible()) {
				String content = pTextField[i].getText().trim();
				if (content.length() > 0 && !content.startsWith("<") && !content.endsWith(">")) {
					if (i < names.size()) {
						oldContent.get(selIndex).put(names.get(i), content);
					}
				}
			}
		}

		selIndex = dbmsComboBox.getSelectedIndex();
		if (!oldContent.containsKey(selIndex)) {
			oldContent.put(selIndex, new HashMap<String, String>());
		}

		names = new ArrayList<String>();
		defValues= new ArrayList<String>();
		isOptional = new ArrayList<Boolean>();

		Pattern pattern = Pattern.compile("(\\[(?:[^\\[\\]]*))?<(\\w+)(?:\\(((?:\\w|\\d)+)\\))?>((?:[^\\[\\]]*)\\])?", Pattern.DOTALL);
		Matcher matcher = pattern.matcher(url);
		for (int i = 0; i < pLabel.length; ++i) {
			pLabel[i].setVisible(false);
			pTextField[i].setVisible(false);
			pButton[i].setVisible(false);
		}
		int i = 0;
		while (matcher.find()) {
			String param = matcher.group(2);
			String defValue = matcher.group(3);
			
			if (param != null && param.length() > 0) {
				boolean optional = matcher.group(1) != null && matcher.group(4) != null;
				pLabel[i].setText((param.length() <= 3? param.toUpperCase() : (param.substring(0, 1).toUpperCase() + (param.substring(1).toLowerCase()))) + ":");
				pLabel[i].setVisible(true);
				pTextField[i].setVisible(true);
				if (oldContent.get(selIndex).containsKey(param)) {
					pTextField[i].setText(oldContent.get(selIndex).get(param));
				} else {
					pTextField[i].setText(optional? "<" + param + ">" : "");
				}
				if (param.toLowerCase().endsWith("file")) {
					pButton[i].setVisible(true);
					pButton[i].setIcon(loadIcon);
				}
				names.add(param);
				defValues.add(defValue);
				isOptional.add(optional);

				i++;
				if (i >= pLabel.length) {
					break;
				}
			}			
		};

		updateUrl();
	}

	private void updateUrl() {
		String newUrl = url;
		for (int i = 0; i < names.size(); ++i) {
			String value = pTextField[i].getText().trim();
			if (isOptional.get(i)) {
				if (!(value.startsWith("<") && value.endsWith(">"))) {
					newUrl = newUrl.replaceAll("(?:\\[([^\\[\\]]*))<(?:" + Pattern.quote(names.get(i)) + ")(?:\\((?:(?:\\w|\\d)+)\\))?>(?:([^\\[\\]]*)\\])", value.length() > 0? "$1" + Matcher.quoteReplacement(value) + "$2" : "");
				}
			} else if (value.length() > 0) {
				newUrl = newUrl.replaceAll("<(" + Pattern.quote(names.get(i)) + ")(?:\\(((?:\\w|\\d)+)\\))?>", Matcher.quoteReplacement(value));
			}
		}
		urlLabel.setText(newUrl);
		urlLabel.setToolTipText(newUrl);
	}

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jPanel1 = new javax.swing.JPanel();
        okButton = new javax.swing.JButton();
        cancelButton = new javax.swing.JButton();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        jSeparator1 = new javax.swing.JSeparator();
        paramLabel1 = new javax.swing.JLabel();
        paramLabel2 = new javax.swing.JLabel();
        paramLabel3 = new javax.swing.JLabel();
        paramLabel4 = new javax.swing.JLabel();
        paramField1 = new javax.swing.JTextField();
        paramField2 = new javax.swing.JTextField();
        paramField3 = new javax.swing.JTextField();
        paramField4 = new javax.swing.JTextField();
        paramButton1 = new javax.swing.JButton();
        paramButton2 = new javax.swing.JButton();
        paramButton3 = new javax.swing.JButton();
        paramButton4 = new javax.swing.JButton();
        urlLabel = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        dbmsComboBox = new javax.swing.JComboBox<>();

        setLayout(new java.awt.GridBagLayout());

        jPanel1.setLayout(new java.awt.GridBagLayout());

        okButton.setText(" Ok ");
        okButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                okButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 2);
        jPanel1.add(okButton, gridBagConstraints);

        cancelButton.setText("Cancel");
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 2);
        jPanel1.add(cancelButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 100;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHEAST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(8, 0, 0, 0);
        add(jPanel1, gridBagConstraints);

        jLabel1.setText("  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        add(jLabel1, gridBagConstraints);

        jLabel2.setText("URL");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 98;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 40);
        add(jLabel2, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 96;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTH;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(12, 0, 12, 0);
        add(jSeparator1, gridBagConstraints);

        paramLabel1.setText("jLabel3");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(8, 0, 0, 4);
        add(paramLabel1, gridBagConstraints);

        paramLabel2.setText("jLabel3");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 4);
        add(paramLabel2, gridBagConstraints);

        paramLabel3.setText("jLabel3");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 4);
        add(paramLabel3, gridBagConstraints);

        paramLabel4.setText("jLabel3");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 4);
        add(paramLabel4, gridBagConstraints);

        paramField1.setText("jTextField2");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(8, 0, 0, 0);
        add(paramField1, gridBagConstraints);

        paramField2.setText("jTextField2");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        add(paramField2, gridBagConstraints);

        paramField3.setText("jTextField2");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        add(paramField3, gridBagConstraints);

        paramField4.setText("jTextField2");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        add(paramField4, gridBagConstraints);

        paramButton1.setText("jButton1");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(8, 0, 0, 2);
        add(paramButton1, gridBagConstraints);

        paramButton2.setText("jButton1");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 2);
        add(paramButton2, gridBagConstraints);

        paramButton3.setText("jButton1");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 2);
        add(paramButton3, gridBagConstraints);

        paramButton4.setText("jButton1");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 2);
        add(paramButton4, gridBagConstraints);

        urlLabel.setFont(urlLabel.getFont().deriveFont(urlLabel.getFont().getStyle() | java.awt.Font.BOLD));
        urlLabel.setText("URL");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 98;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 4);
        add(urlLabel, gridBagConstraints);

        jLabel5.setText("DBMS");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(8, 0, 0, 4);
        add(jLabel5, gridBagConstraints);

        dbmsComboBox.setMaximumRowCount(24);
        dbmsComboBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                dbmsComboBoxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(8, 0, 0, 0);
        add(dbmsComboBox, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    private void okButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_okButtonActionPerformed
        ok = true;
        dialog.setVisible(false);
    }//GEN-LAST:event_okButtonActionPerformed

    private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
        ok = false;
        dialog.setVisible(false);
    }//GEN-LAST:event_cancelButtonActionPerformed

    private void dbmsComboBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_dbmsComboBoxActionPerformed
        updateFields();
    }//GEN-LAST:event_dbmsComboBoxActionPerformed

    private boolean ok;

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton cancelButton;
    private javax.swing.JComboBox<Line> dbmsComboBox;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JButton okButton;
    private javax.swing.JButton paramButton1;
    private javax.swing.JButton paramButton2;
    private javax.swing.JButton paramButton3;
    private javax.swing.JButton paramButton4;
    private javax.swing.JTextField paramField1;
    private javax.swing.JTextField paramField2;
    private javax.swing.JTextField paramField3;
    private javax.swing.JTextField paramField4;
    private javax.swing.JLabel paramLabel1;
    private javax.swing.JLabel paramLabel2;
    private javax.swing.JLabel paramLabel3;
    private javax.swing.JLabel paramLabel4;
    private javax.swing.JLabel urlLabel;
    // End of variables declaration//GEN-END:variables
    
	private Icon loadIcon;
	{
		// load images
		loadIcon = UIUtil.readImage("/load.png");
	}

}
