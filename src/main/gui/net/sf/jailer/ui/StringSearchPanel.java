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

import java.awt.Component;
import java.awt.Cursor;
import java.awt.Frame;
import java.awt.Image;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.Locale;

import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListCellRenderer;
import javax.swing.DefaultListModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.SwingUtilities;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import net.sf.jailer.ui.databrowser.metadata.MetaDataPanel;

/**
 * Substring search for combo boxes.
 * 
 * @author Ralf Wisser
 */
public class StringSearchPanel extends javax.swing.JPanel {

	private JDialog dialog;
	private String result;
	
	public static JButton createSearchButton(final Frame owner, final javax.swing.JComboBox comboBox, final String titel, final Runnable onSuccess) {
		final JButton button = new JButton();
		button.setIcon(getScaledIcon(button, icon));
		button.setToolTipText("Find Table");
		button.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				owner.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
				SwingUtilities.invokeLater(new Runnable() {
					@Override
					public void run() {
				        try {
							Point location = button.getLocationOnScreen();
							StringSearchPanel searchPanel = new StringSearchPanel((DefaultComboBoxModel<String>) comboBox.getModel());
							String result = searchPanel.find(owner, titel, location.x, location.y);
							if (result != null) {
								comboBox.setSelectedItem(result);
								if (onSuccess != null) {
									onSuccess.run();
								}
							}
				        } finally {
				            owner.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
				        }
					}
				});
			}
		});
		return button;
	}
	
	public String find(Frame owner, String titel, int x, int y) {
		dialog = new JDialog(owner, titel, true);
		dialog.getContentPane().add(this);
		dialog.pack();
		dialog.setLocation(x, y);
		dialog.setSize(300, Math.max(dialog.getHeight() + 20, 440));
		int h = dialog.getHeight();
		UIUtil.fit(dialog);
		if (h > dialog.getHeight()) {
			dialog.setLocation(x, y - (h - dialog.getHeight()));
			dialog.setSize(400, Math.max(dialog.getHeight() + 20, 400));
			UIUtil.fit(dialog);
		}
		
		result = null;
		dialog.setVisible(true);
		
		return result;
	}
	
	private void updateList() {
		DefaultListModel<String> matches = new DefaultListModel<String>();
		String searchText = searchTextField.getText().trim().toUpperCase(Locale.ENGLISH);
		int size = model.getSize();
		for (int i = 0; i < size; ++i) {
			String item = model.getElementAt(i);
			if (!item.isEmpty()) {
				if (searchText.isEmpty() || item.toUpperCase(Locale.ENGLISH).contains(searchText)) {
					matches.addElement(item);
				}
			}
		}
		searchList.setModel(matches);
		if (!matches.isEmpty()) {
			searchList.setSelectedIndex(0);
		}
	}

	private final DefaultComboBoxModel<String> model;
	
    /**
     * Creates new form StringSearchPanel
     */
    public StringSearchPanel(DefaultComboBoxModel<String> model) {
    	this.model = model;
        initComponents();
		KeyListener keyListener = new KeyListener() {
			@Override
			public void keyTyped(KeyEvent e) {
				if (e.getKeyChar() == KeyEvent.VK_ESCAPE) {
					dialog.setVisible(false);
				} else if (e.getKeyChar() == KeyEvent.VK_DOWN) {
					searchList.grabFocus();
				} else if (e.getKeyChar() == '\n') {
					result  = searchList.getSelectedValue();
					dialog.setVisible(false);
				}
			}
			@Override
			public void keyReleased(KeyEvent e) {
			}
			@Override
			public void keyPressed(KeyEvent e) {
				int kc = e.getKeyCode();
				if (kc == KeyEvent.VK_DOWN) {
					searchList.grabFocus();
				}
			}
		};
		searchTextField.addKeyListener(keyListener);
		searchList.addKeyListener(keyListener);
		okButton.addKeyListener(keyListener);
		
		searchTextField.getDocument().addDocumentListener(new DocumentListener() {
			@Override
			public void removeUpdate(DocumentEvent e) {
				updateList();
			}
			@Override
			public void insertUpdate(DocumentEvent e) {
				updateList();
			}
			@Override
			public void changedUpdate(DocumentEvent e) {
				updateList();
			}
		});
		
		searchList.addMouseListener(new MouseListener() {
			@Override
			public void mouseReleased(MouseEvent e) {
			}
			@Override
			public void mousePressed(MouseEvent e) {
			}
			@Override
			public void mouseExited(MouseEvent e) {
			}
			@Override
			public void mouseEntered(MouseEvent e) {
			}
			@Override
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() >1) {
					result  = searchList.getSelectedValue();
					dialog.setVisible(false);
				}
			}
		});
		searchList.setCellRenderer(new DefaultListCellRenderer() {
			@Override
			public Component getListCellRendererComponent(JList<?> list, Object value, int index, boolean isSelected,
					boolean cellHasFocus) {
				String search = searchTextField.getText().trim().toUpperCase(Locale.ENGLISH);
				int i = value.toString().trim().toUpperCase(Locale.ENGLISH).indexOf(search);
				String item = value.toString();
				if (i >= 0) {
					item = item.substring(0, i) + "<b><i>" + item.substring(i, i + search.length()) + "</i></b>" + item.substring(i + search.length());
				}
				String html = "<html>" + item;
				return super.getListCellRendererComponent(list, html, index, isSelected, cellHasFocus);
			}
			
		});

		searchTextField.setText("");
		updateList();
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

        searchTextField = new javax.swing.JTextField();
        jScrollPane1 = new javax.swing.JScrollPane();
        searchList = new javax.swing.JList<>();
        okButton = new javax.swing.JButton();
        cancelButton = new javax.swing.JButton();

        setLayout(new java.awt.GridBagLayout());

        searchTextField.setText("jTextField1");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        add(searchTextField, gridBagConstraints);

        searchList.setModel(new javax.swing.AbstractListModel<String>() {
            String[] strings = { "Item 1", "Item 2", "Item 3", "Item 4", "Item 5" };
            public int getSize() { return strings.length; }
            public String getElementAt(int i) { return strings[i]; }
        });
        jScrollPane1.setViewportView(searchList);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(jScrollPane1, gridBagConstraints);

        okButton.setText(" Ok ");
        okButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                okButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        add(okButton, gridBagConstraints);

        cancelButton.setText("Cancel");
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 3;
        add(cancelButton, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    private void okButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_okButtonActionPerformed
    	result  = searchList.getSelectedValue();
		dialog.setVisible(false);
    }//GEN-LAST:event_okButtonActionPerformed

    private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
    	dialog.setVisible(false);
	}//GEN-LAST:event_cancelButtonActionPerformed


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton cancelButton;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JButton okButton;
    private javax.swing.JList<String> searchList;
    private javax.swing.JTextField searchTextField;
    // End of variables declaration//GEN-END:variables
    
    static private ImageIcon icon;
    static ImageIcon getScaledIcon(JComponent component, ImageIcon scaledIcon) {
    	if (scaledIcon != null) {
            if (scaledIcon != null) {
            	int heigth = component.getFontMetrics(new JLabel("M").getFont()).getHeight();
            	double s = heigth / (double) scaledIcon.getIconHeight();
            	try {
            		return new ImageIcon(scaledIcon.getImage().getScaledInstance((int)(scaledIcon.getIconWidth() * s), (int)(scaledIcon.getIconHeight() * s), Image.SCALE_SMOOTH));
            	} catch (Exception e) {
            		return null;
            	}
            }
    	}
    	return null;
    }
    static {
		String dir = "/net/sf/jailer/ui/resource";
		
		// load images
		try {
			icon = new ImageIcon(MetaDataPanel.class.getResource(dir + "/search.png"));
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
