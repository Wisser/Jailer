/*
 * Copyright 2007 - 2021 Ralf Wisser.
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
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.FilenameFilter;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
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
@SuppressWarnings("serial")
public class BookmarksPanel extends javax.swing.JPanel {

	public static final String BOOKMARKFILE_EXTENSION = ".dbl";
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
		
		nameTextField.addKeyListener(new KeyAdapter() {
			@Override
			public void keyTyped(KeyEvent e) {
				char c = e.getKeyChar();
				if (c == '\n') {
					okButton.doClick();
				}
			}
		});
	}

	public String newBookmark(String defaultName) {
    	dialog = new EscapableDialog(owner, "New Bookmark") {
        };
        dialog.addWindowListener(new WindowAdapter() {
			@Override
			public void windowActivated(WindowEvent e) {
				super.windowActivated(e);
				nameTextField.grabFocus();
			}
		});
        dialog.setModal(true);
 		dialog.getContentPane().add(this);
 		dialog.pack();
 		dialog.setSize(440, 440);
 		dialog.setLocation(owner.getX() + (owner.getWidth() - dialog.getWidth()) / 2, Math.max(0, owner.getY() + (owner.getHeight() - dialog.getHeight()) / 2));
 		UIUtil.fit(dialog);

 		ListEditor<StringBuilder> tableEditor = createListEditor(false);
		tableEditor.hideAllButtons();
		List<StringBuilder> bookmarks = loadBookmarks(executionContext);
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

 		UISettings.s6 += 100000;

 		UISettings.addRecentBookmarks(new BookmarkId(name, executionContext.getCurrentModelSubfolder(), executionContext.getCurrentConnectionAlias(), desktop.getRawSchemaMapping()));
 		setLastUsedBookmark(name, executionContext);
 		return name;
    }

	public void editBookmarks() {
		dialog = new EscapableDialog(owner, "Edit Bookmarks") {
        };
        dialog.setModal(true);
 		dialog.getContentPane().add(this);
 		dialog.pack();
 		dialog.setSize(440, 440);
 		dialog.setLocation(owner.getX() + (owner.getWidth() - dialog.getWidth()) / 2, Math.max(0, owner.getY() + (owner.getHeight() - dialog.getHeight()) / 2));
 		UIUtil.fit(dialog);

 		nameTextField.setVisible(false);
 		jLabel1.setVisible(false);
 		okButton.setVisible(false);
 		closeButton.setText(" Close ");
 		
 		ListEditor<StringBuilder> tableEditor = createListEditor(true);
		tableEditor.forUpdateAndDeleteOnly();
		List<StringBuilder> bookmarks = loadBookmarks(executionContext);
		tableEditor.setModel(bookmarks);
		jPanel1.add(tableEditor);
		
 		isOk = false;
 		dialog.setVisible(true);
 		
		Set<String> bookmarksSet = new HashSet<String>();
		for (StringBuilder sb: bookmarks) {
			bookmarksSet.add(sb.toString());
		}
		
		for (StringBuilder bm: loadBookmarks(executionContext)) {
			if (!bookmarksSet.contains(bm.toString())) {
				new File(getBookmarksFolder(executionContext), toValidFileName(bm.toString())).delete();
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
					String dest = toValidFileName(renameTextField.getText()) + BOOKMARKFILE_EXTENSION;
					StringBuilder source = element;
					if (dest.length() > 0 && !dest.equals(source.toString())) {
						new File(getBookmarksFolder(executionContext), dest).delete();
						new File(getBookmarksFolder(executionContext), toValidFileName(source.toString())).renameTo(new File(getBookmarksFolder(executionContext), dest));
						source.setLength(0);
						source.append(renameTextField.getText() + BOOKMARKFILE_EXTENSION);
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
			
			protected void onDoubleClick(StringBuilder element) {
				if (!forRenaming) {
					nameTextField.setText(getDisplayName(element));
					okButtonActionPerformed(null);
				}
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

    /**
     * @return folder containing bookmarks for current data model
     */
	public static File getBookmarksFolder(ExecutionContext executionContext) {
		return getBookmarksFolder(executionContext.getCurrentModelSubfolder());
	}


	private static File getBookmarksFolder(String currentModelSubfolder) {
		if (currentModelSubfolder == null) {
			currentModelSubfolder = "default";
		}
		return Environment.newFile("bookmark" + File.separator + new File(currentModelSubfolder).getName());
	}

	/**
     * @param bookmark bookmark name
     * @return file containing bookmark with given name or <code>null</code>, if no such bookmark exists
     */
	public static File getBookmarksFile(String bookmark, ExecutionContext executionContext) {
		if (bookmark == null) {
			return null;
		}
		File bmFile = new File(getBookmarksFolder(executionContext), bookmark + BOOKMARKFILE_EXTENSION);
		try {
			if (bmFile.exists()) {
				return bmFile;
			}
		} catch (Exception e) {
			// ignore
		}
		bmFile = new File(getBookmarksFolder(executionContext), toValidFileName(bookmark) + BOOKMARKFILE_EXTENSION);
		if (bmFile.exists()) {
			return bmFile;
		} else {
			return null;
		}
	}

	public void updateBookmarksMenu() {
		List<StringBuilder> bookmarks = loadBookmarks(executionContext);
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
				final String bmName = nb.replaceAll("\\.dbl$", "");
				JMenuItem b = new JMenuItem(bmName);
				b.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						UISettings.s6 += 1000;
						File bookMarkFile = new File(getBookmarksFolder(executionContext), toValidFileName(nb));
						desktop.restoreSession(null, bookMarkFile);
						if (bookMarkFile.exists()) {
							bookMarkFile.setLastModified(System.currentTimeMillis());
						}
						new File(getBookmarksFolder(executionContext), nb).setLastModified(System.currentTimeMillis());
						updateBookmarksMenu();
				 		UISettings.addRecentBookmarks(new BookmarkId(bmName, executionContext.getCurrentModelSubfolder(), executionContext.getCurrentConnectionAlias(), desktop.getRawSchemaMapping()));
				 		setLastUsedBookmark(nb, executionContext);
					}
				});
				bookmarksMenu.add(b);
				if (++count > 40) {
					break;
				}
			}
		}
	}

	public static List<StringBuilder> loadBookmarks(ExecutionContext executionContext) {
		File bookmarksFolder = getBookmarksFolder(executionContext);
		return loadBookmarks(bookmarksFolder, executionContext);
	}

	public static List<StringBuilder> loadBookmarks(final File bookmarksFolder, ExecutionContext executionContext) {
		try {
			String[] fileList = bookmarksFolder.list(new FilenameFilter() {
				@Override
				public boolean accept(File dir, String name) {
					return name.endsWith(BOOKMARKFILE_EXTENSION);
				}
			});
			ArrayList<String> result = new ArrayList<String>();
			if (fileList != null) {
				for (String vf: fileList) {
					result.add(fromValidFileName(vf));
				}
			}
			Collections.sort(result, new Comparator<String>() {
				@Override
				public int compare(String o1, String o2) {
					long l1 = new File(bookmarksFolder, toValidFileName(o1)).lastModified();
					long l2 = new File(bookmarksFolder, toValidFileName(o2)).lastModified();
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

	private static Map<File, String> lastUsedBM = new HashMap<File, String>();

	public static String getLastUsedBookmark(ExecutionContext executionContext) {
		return lastUsedBM.get(getBookmarksFolder(executionContext));
	}

	public static String setLastUsedBookmark(String lastUsedBookmark, ExecutionContext executionContext) {
		return lastUsedBM.put(getBookmarksFolder(executionContext), lastUsedBookmark);
	}

	/**
	 * Unique bookmark identifier.
	 */
	public static class BookmarkId implements Serializable {
		public final String bookmark;
		public final String datamodelFolder;
		public final String connectionAlias;
		public final String rawSchemaMapping;
		public final Date date;
		
		public BookmarkId(String bookmark, String datamodelFolder, String connectionAlias, String rawSchemaMapping) {
			this.bookmark = bookmark;
			this.datamodelFolder = datamodelFolder;
			this.connectionAlias = connectionAlias;
			this.rawSchemaMapping = rawSchemaMapping;
			this.date = new Date();
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + ((bookmark == null) ? 0 : bookmark.hashCode());
			result = prime * result + ((connectionAlias == null) ? 0 : connectionAlias.hashCode());
			result = prime * result + ((datamodelFolder == null) ? 0 : datamodelFolder.hashCode());
			return result;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			BookmarkId other = (BookmarkId) obj;
			if (bookmark == null) {
				if (other.bookmark != null)
					return false;
			} else if (!bookmark.equals(other.bookmark))
				return false;
			if (connectionAlias == null) {
				if (other.connectionAlias != null)
					return false;
			} else if (!connectionAlias.equals(other.connectionAlias))
				return false;
			if (datamodelFolder == null) {
				if (other.datamodelFolder != null)
					return false;
			} else if (!datamodelFolder.equals(other.datamodelFolder))
				return false;
			return true;
		}

		private static final long serialVersionUID = -7491145126834345194L;
	}

	private static final String[] INVALID_FILENAME_CHARACTERS = new String[] {"\\", "/", ":", ";", "*", "?", "\"", "<", ">", "|"};
	private static final String[][] INVALID_FILENAME_CHARACTERS_TO_REPLACEMENT = new String[INVALID_FILENAME_CHARACTERS.length][];
	static {
    	for (int i = 0; i < INVALID_FILENAME_CHARACTERS.length; ++i) {
    		INVALID_FILENAME_CHARACTERS_TO_REPLACEMENT[i] 
    				= new String[] { 
    						new String(INVALID_FILENAME_CHARACTERS[i]), 
    						String.format("%%%02X", 0xFF & (int) (INVALID_FILENAME_CHARACTERS[i].charAt(0)))
    				};
    	}
    }

	private static String toValidFileName(String text) {
		for (String[] cp: INVALID_FILENAME_CHARACTERS_TO_REPLACEMENT) {
			text = text.replace(cp[0], cp[1]);
		}
		return text.trim();
	}

	private static String fromValidFileName(String text) {
		for (String[] cp: INVALID_FILENAME_CHARACTERS_TO_REPLACEMENT) {
			text = text.replace(cp[1], cp[0]);
		}
		return text.trim();
	}

	/**
	 * Gets all bookmarks in a given data model.
	 * 
	 * @param model the data model
	 * @return all bookmarks in a given data model folder
	 */
	public static List<String> getAllBookmarks(String model, ExecutionContext executionContext) {
		List<String> result = new ArrayList<String>();
		for (StringBuilder bm: loadBookmarks(getBookmarksFolder(model), executionContext)) {
			result.add(bm.toString().replaceAll("\\.dbl$", ""));
		}
		return result;
	}

}
