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
package net.sf.jailer.ui.databrowser;

import java.awt.Color;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.swing.JComponent;
import javax.swing.JMenu;
import javax.swing.JMenuItem;

import org.fife.rsta.ui.EscapableDialog;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.ui.Environment;
import net.sf.jailer.ui.ListEditor;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.util.UISettings;

/**
 * Bookmarks panel.
 * 
 * @author Ralf Wisser
 */
public class BookmarksPanel extends javax.swing.JPanel {

	private final Frame owner;
	private final Desktop desktop;
	private EscapableDialog dialog;
	private boolean isOk;
	private final ExecutionContext executionContext;
	private final JMenu bookmarksMenu;
	
	/**
	 * Creates new form BookmarksPanel
	 * 
	 * @param dataBrowser
	 */
	public BookmarksPanel(Frame owner, JMenu bookmarksMenu, Desktop desktop, ExecutionContext executionContext) {
		this.owner = owner;
		this.bookmarksMenu = bookmarksMenu;
		this.desktop = desktop;
		this.executionContext = executionContext;
		initComponents();
	}

	public String newBookmark(String defaultName) {
    	dialog = new EscapableDialog(owner, "New Bookmark") {
        };
        dialog.setModal(true);
 		dialog.getContentPane().add(this);
 		dialog.pack();
 		dialog.setSize(340, 440);
 		dialog.setLocation(owner.getX() + (owner.getWidth() - dialog.getWidth()) / 2, Math.max(0, owner.getY() + (owner.getHeight() - dialog.getHeight()) / 2));
 		UIUtil.fit(dialog);

 		ListEditor<StringBuilder> tableEditor = createListEditor(false);
		tableEditor.hideAllButtons();
		List<StringBuilder> bookmarks = loadBookmarks();
		tableEditor.setModel(new ArrayList<StringBuilder>(bookmarks));
		jPanel1.add(tableEditor);
		nameTextField.setText(defaultName);
 		okButton.grabFocus();
 		isOk = false;
 		closeButton.setVisible(false);
 		dialog.setVisible(true);
 		
 		String name = toValidFileName(nameTextField.getText());

 		if (!isOk || name.isEmpty()) {
 			return null;
 		}

 		UISettings.s6 += 1000000;

 		return name;
    }

	private String toValidFileName(String text) {
		return text.replaceAll("['`\"/\\\\\\~]+", " ").trim();
	}

	public void editBookmarks() {
		dialog = new EscapableDialog(owner, "Edit Bookmarks") {
        };
        dialog.setModal(true);
 		dialog.getContentPane().add(this);
 		dialog.pack();
 		dialog.setSize(340, 440);
 		dialog.setLocation(owner.getX() + (owner.getWidth() - dialog.getWidth()) / 2, Math.max(0, owner.getY() + (owner.getHeight() - dialog.getHeight()) / 2));
 		UIUtil.fit(dialog);

 		nameTextField.setVisible(false);
 		jLabel1.setVisible(false);
 		okButton.setVisible(false);
 		closeButton.setText(" Close ");
 		
 		ListEditor<StringBuilder> tableEditor = createListEditor(true);
		tableEditor.forUpdateAndDeleteOnly();
		List<StringBuilder> bookmarks = loadBookmarks();
		tableEditor.setModel(bookmarks);
		jPanel1.add(tableEditor);
		
 		isOk = false;
 		dialog.setVisible(true);
 		
		Set<String> bookmarksSet = new HashSet<String>();
		for (StringBuilder sb: bookmarks) {
			bookmarksSet.add(sb.toString());
		}
		
		for (StringBuilder bm: loadBookmarks()) {
			if (!bookmarksSet.contains(bm.toString())) {
				new File(getBookmarksFolder(), bm.toString()).delete();
			}
		}
	}

	private ListEditor<StringBuilder> createListEditor(final boolean forRenaming) {
		return new ListEditor<StringBuilder>(new String[] { "Name" }, "Bookmark", false, false, true) {

			@Override
			protected String getDisplayName(StringBuilder element) {
				return element.toString().replaceAll("\\.dbl$", "");
			}

			@Override
			protected StringBuilder copy(StringBuilder element) {
				return new StringBuilder(element.toString());
			}

			@Override
			protected StringBuilder createNew() {
				return new StringBuilder();
			}

			@Override
			protected JComponent createDetailsView(StringBuilder element) {
				if (forRenaming) {
					renameTextField.setText(getDisplayName(element));
					return renamePanel;
				}
				return null;
			}

			@Override
			protected void updateFromDetailsView(StringBuilder element, JComponent detailsView, List<StringBuilder> model,
					StringBuilder errorMessage) {
				if (forRenaming) {
					String dest = toValidFileName(renameTextField.getText()) + ".dbl";
					StringBuilder source = element;
					if (dest.length() > 0 && !dest.equals(source.toString())) {
						new File(getBookmarksFolder(), dest).delete();
						new File(getBookmarksFolder(), source.toString()).renameTo(new File(getBookmarksFolder(), dest));
						source.setLength(0);
						source.append(dest.toString());
					}
				}
			}

			@Override
			protected Object[] toColumnList(StringBuilder element, int index) {
				return new String[] { getDisplayName(element) };
			}

			@Override
			protected Color getForegroundColor(StringBuilder element, int column) {
				return null;
			}

			protected void onElementClicked(StringBuilder element) {
				nameTextField.setText(getDisplayName(element));
			}
		};
	}

	/**
	 * This method is called from within the constructor to initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is always
	 * regenerated by the Form Editor.
	 */
	// <editor-fold defaultstate="collapsed" desc="Generated
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        renamePanel = new javax.swing.JPanel();
        jLabel2 = new javax.swing.JLabel();
        renameTextField = new javax.swing.JTextField();
        jLabel3 = new javax.swing.JLabel();
        jLabel1 = new javax.swing.JLabel();
        nameTextField = new javax.swing.JTextField();
        jPanel1 = new javax.swing.JPanel();
        okButton = new javax.swing.JButton();
        closeButton = new javax.swing.JButton();

        renamePanel.setLayout(new java.awt.GridBagLayout());

        jLabel2.setText("  New name ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(8, 4, 4, 8);
        renamePanel.add(jLabel2, gridBagConstraints);

        renameTextField.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                renameTextFieldActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(8, 0, 4, 8);
        renamePanel.add(renameTextField, gridBagConstraints);

        jLabel3.setText("                                                                             ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        renamePanel.add(jLabel3, gridBagConstraints);

        setLayout(new java.awt.GridBagLayout());

        jLabel1.setText("  Name ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(8, 4, 8, 2);
        add(jLabel1, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(8, 2, 8, 2);
        add(nameTextField, gridBagConstraints);

        jPanel1.setBorder(javax.swing.BorderFactory.createTitledBorder("Bookmarks"));
        jPanel1.setLayout(new javax.swing.BoxLayout(jPanel1, javax.swing.BoxLayout.LINE_AXIS));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        add(jPanel1, gridBagConstraints);

        okButton.setText(" Ok ");
        okButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                okButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(8, 2, 8, 2);
        add(okButton, gridBagConstraints);

        closeButton.setText(" Cancel ");
        closeButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                closeButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        add(closeButton, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    private void closeButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_closeButtonActionPerformed
    	isOk = false;
		dialog.setVisible(false);
    }//GEN-LAST:event_closeButtonActionPerformed

    private void renameTextFieldActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_renameTextFieldActionPerformed
    }//GEN-LAST:event_renameTextFieldActionPerformed

	private void okButtonActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_okButtonActionPerformed
		isOk = true;
		dialog.setVisible(false);
	}// GEN-LAST:event_okButtonActionPerformed

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton closeButton;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JTextField nameTextField;
    private javax.swing.JButton okButton;
    private javax.swing.JPanel renamePanel;
    private javax.swing.JTextField renameTextField;
    // End of variables declaration//GEN-END:variables

	public File getBookmarksFolder() {
		String currentModelSubfolder = executionContext.getCurrentModelSubfolder();
		if (currentModelSubfolder == null) {
			currentModelSubfolder = "default";
		}
		return Environment.newFile("bookmark" + File.separator + new File(currentModelSubfolder).getName());
	}

	public void updateBookmarksMenu() {
		List<StringBuilder> bookmarks = loadBookmarks();
		while (bookmarksMenu.getItemCount() > 3) {
			bookmarksMenu.remove(bookmarksMenu.getItemCount() - 1);
		}
		bookmarksMenu.getItem(1).setEnabled(!bookmarks.isEmpty());
		if (bookmarks.isEmpty()) {
			JMenuItem nb = new JMenuItem("No Bookmarks");
			nb.setEnabled(false);
			bookmarksMenu.add(nb);
		} else {
			int count = 0;
			for (StringBuilder nbSb: bookmarks) {
				final String nb = nbSb.toString();
				JMenuItem b = new JMenuItem(nb.replaceAll("\\.dbl$", ""));
				b.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						UISettings.s6 += 1000;
						desktop.restoreSession(null, new File(getBookmarksFolder(), nb));
						new File(getBookmarksFolder(), nb).setLastModified(System.currentTimeMillis());
						updateBookmarksMenu();
					}
				});
				bookmarksMenu.add(b);
				if (++count > 40) {
					break;
				}
			}
		}
	}

	private List<StringBuilder> loadBookmarks() {
		try {
			final File bookmarksFolder = getBookmarksFolder();
			ArrayList<String> result = new ArrayList<String>(Arrays.asList(bookmarksFolder.list(new FilenameFilter() {
				@Override
				public boolean accept(File dir, String name) {
					return name.endsWith(".dbl");
				}
			})));
			Collections.sort(result, new Comparator<String>() {
				@Override
				public int compare(String o1, String o2) {
					long l1 = new File(bookmarksFolder, o1).lastModified();
					long l2 = new File(bookmarksFolder, o2).lastModified();
					if (l1 > l2) {
						return -1;
					} else if (l1 < l2) {
						return 1;
					} else {
						return 0;
					}
				}
			});
			List<StringBuilder> resultAsSBList = new ArrayList<StringBuilder>();
			for (String bm: result) {
				resultAsSBList.add(new StringBuilder(bm));
			}
			return resultAsSBList;
		} catch (Throwable t) {
			return Collections.emptyList();
		}
	}

}
