/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/GUIForms/JPanel.java to edit this template
 */
package net.sf.jailer.ui;

import java.awt.Color;
import java.awt.Dialog;
import java.awt.Font;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.Window;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.WindowConstants;

import org.fife.rsta.ui.EscapableDialog;

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
import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.ui.DbConnectionDialog.ConnectionInfo;
import net.sf.jailer.ui.util.CompoundIcon;
import net.sf.jailer.util.LogUtil;
import net.sf.jailer.util.Pair;

/**
 * UI for exporting data models and database connections.
 *
 * @author Ralf Wisser
 */
public class ExportPanel extends javax.swing.JPanel {

    private static final String EXPORT_MARKER = "JailerModelAndConnectionArchiv";
    private static final String EXPORT_FILE_EXTENSION = ".jmarc.zip";

	/**
     * Creates new form ExportPanel
     */
    public ExportPanel() {
        initComponents();
        okButton.setIcon(UIUtil.scaleIcon(okButton, okIcon));
		cancelButton.setIcon(UIUtil.scaleIcon(cancelButton, cancelIcon));
		if (UIUtil.plaf.isFlat) {
			selectionPanel.setBackground(Colors.Color_255_255_255);
			jPanel3.setBackground(Colors.Color_255_255_255);
		}
    }
    
    private JDialog dialog;
    private boolean ok = false;
    
    private Set<String> selectedModels = new TreeSet<>(String::compareToIgnoreCase);
    private Set<ConnectionInfo> selectedConnections = new TreeSet<>((a, b) -> a.alias.compareToIgnoreCase(b.alias));
    private ExecutionContext executionContext;
    
    public boolean openExportDialog(Window owner, Object initiallySelected, DbConnectionDialog connectionDialog) {
    	List<ConnectionInfo> connectionList = connectionDialog.getConnectionList();
    	return openDialog(owner, initiallySelected, connectionList, new HashSet<>(), "Export data models and connections", connectionDialog);
    }
    
    private boolean openDialog(Window owner, Object initiallySelected, List<ConnectionInfo> connectionList, Set<String> tabuModels, String title, DbConnectionDialog connectionDialog) {
    	executionContext = connectionDialog.executionContext;
    	dialog = owner instanceof Dialog? new EscapableDialog((Dialog) owner) {
		} : new EscapableDialog((Frame) owner) {
		};
		dialog.getContentPane().add(this);
		dialog.setModal(true);
		dialog.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		dialog.setTitle(title);
		dialog.pack();
		dialog.setSize(Math.max(500, dialog.getWidth()), Math.min(700, Math.max(500, dialog.getHeight()))); 

		UIUtil.fit(dialog);
		dialog.setLocation(owner.getX() + owner.getWidth() / 2 - dialog.getWidth() / 2, owner.getY() + owner.getHeight() / 2 - dialog.getHeight() / 2);
		
		String cmsf = DataModelManager.getCurrentModelSubfolder(connectionDialog.executionContext);
		try {
			DataModelManager.setCurrentModelSubfolder(null, connectionDialog.executionContext);
			TreeMap<String, String> modelNames = new TreeMap<>(String::compareToIgnoreCase);
			Set<String> seen = new HashSet<>();
			Set<String> dup = new HashSet<>();
			for (String model : DataModelManager.getModelFolderNames(connectionDialog.executionContext)) {
				String displayName = DataModelManager.getModelDetails(model, connectionDialog.executionContext).a;
				if (!seen.add(displayName)) {
					dup.add(displayName);
				}
			}
			for (String model : DataModelManager.getModelFolderNames(connectionDialog.executionContext)) {
				if (model != null) {
					String displayName = DataModelManager.getModelDetails(model, connectionDialog.executionContext).a;
					modelNames.put(model, displayName + (dup.contains(displayName) ? " (" + model + ")" : ""));
				}
			}
			
			int y = 1;
			List<ConnectionInfo> connections = connectionList == null? new ArrayList<>() : new ArrayList<>(connectionList);
			connections.sort((a, b) -> a.alias.compareToIgnoreCase(b.alias));
			List<ConnectionInfo> rest = new ArrayList<>(connections);
			List<Entry<String, String>> entryList = new ArrayList<>(modelNames.entrySet());
			entryList.sort((a, b) -> a.getValue().compareToIgnoreCase(b.getValue()));
			entryList.add(null);
	        Font nonBold = jLabel1.getFont().deriveFont(jLabel1.getFont().getStyle() & ~Font.BOLD);
	        Font bold = jLabel1.getFont().deriveFont(jLabel1.getFont().getStyle() | Font.BOLD);
			String orphaned = "<html><i>orphaned</i></html>";
			int b = 0;
			for (Map.Entry<String, String> entry: entryList) {
				if ( entry == null) {
					if (rest.isEmpty()) {
						continue;
					}
				}
				String key = entry == null? orphaned : entry.getKey();
				String value = entry == null? orphaned : entry.getValue();
				JCheckBox jCheckBox = new JCheckBox();
				int sep = y > 1? 8 : 0;
				if (entry != null) {
					if (!tabuModels.contains(key)) {
						checkBoxes.add(jCheckBox);
				        if (key.equals(initiallySelected) || Boolean.TRUE.equals(initiallySelected)) {
				        	selectAndScrollTo(jCheckBox);
				        }
						jCheckBox.addItemListener(new ItemListener() {
							@Override
							public void itemStateChanged(ItemEvent e) {
								if (jCheckBox.isSelected()) {
									selectedModels.add(key);
								} else {
									selectedModels.remove(key);
								}
								updateStatus();
							}
						});
					} else {
						jCheckBox.setEnabled(false);
					}
					jCheckBox.setText(" ");
					jCheckBox.setFocusable(false);
					GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
					gridBagConstraints.gridx = 1;
					gridBagConstraints.gridy = y;
					gridBagConstraints.insets = new Insets(b + sep, 0, b, 0);
					selectionPanel.add(jCheckBox, gridBagConstraints);
		        } else {
		        	jCheckBox.setText(" ");
		        	jCheckBox.setFocusable(false);
		        	jCheckBox.setEnabled(false);
					GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
					gridBagConstraints.gridx = 1;
					gridBagConstraints.gridy = y;
					gridBagConstraints.insets = new Insets(b + sep, 0, b, 0);
					selectionPanel.add(jCheckBox, gridBagConstraints);
		        }

		        JLabel label = new JLabel();
		        label.setText(value);
		        label.setFont(nonBold);
		        jCheckBox.addItemListener(new ItemListener() {
					@Override
					public void itemStateChanged(ItemEvent e) {
						label.setFont(jCheckBox.isSelected()? bold : nonBold);
					}
				});
		        if (entry != null) {
		        	label.setIcon(modelIcon);
		        }
		        label.addMouseListener(new MouseAdapter() {
					@Override
					public void mouseClicked(MouseEvent e) {
						jCheckBox.setSelected(!jCheckBox.isSelected());
					}
				});
		        GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
		        gridBagConstraints.gridx = 2;
		        gridBagConstraints.gridy = y;
		        gridBagConstraints.insets = new Insets(b + sep, 0, b, 0);
				gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
		        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
		        gridBagConstraints.weightx = 1.0;
		        selectionPanel.add(label, gridBagConstraints);
		        ++y;
		        
		        for (ConnectionInfo ci: connections) {
		        	if (entry != null && key.equals(ci.dataModelFolder) || entry == null && rest.contains(ci)) {
		        		rest.remove(ci);
		        		JCheckBox jCheckBox2 = new JCheckBox();
		        		checkBoxes.add(jCheckBox2);
						jCheckBox2.setFocusable(false);
						jCheckBox2.setText(" ");
						jCheckBox2.addItemListener(new ItemListener() {
							@Override
							public void itemStateChanged(ItemEvent e) {
								if (jCheckBox2.isSelected()) {
									selectedConnections.add(ci);
								} else {
									selectedConnections.remove(ci);
								}
								updateStatus();
							}
						});
				        if (ci.equals(initiallySelected) || Boolean.TRUE.equals(initiallySelected)) {
				        	selectAndScrollTo(jCheckBox2);
				        }
				        gridBagConstraints = new java.awt.GridBagConstraints();
				        gridBagConstraints.gridx = 1;
				        gridBagConstraints.gridy = y;
				        gridBagConstraints.insets = new Insets(b, 20, b, 0);
				        selectionPanel.add(jCheckBox2, gridBagConstraints);

				        JLabel label2 = new JLabel();
				        label2.setText(ci.alias);
				        label2.setFont(nonBold);
				        jCheckBox2.addItemListener(new ItemListener() {
							@Override
							public void itemStateChanged(ItemEvent e) {
								label2.setFont(jCheckBox2.isSelected()? bold : nonBold);
							}
						});
						String dbmsLogoURL = UIUtil.getDBMSLogoURL(ci.url);
						Color bg = ci.getConnectionType().getBackground();
						if (bg != null) {
							label2.setBackground(bg);
							label2.setOpaque(true);
						}
						if (dbmsLogoURL != null) {
							Icon icon = UIUtil.scaleIcon(label2, UIUtil.readImage(dbmsLogoURL, false), 1.2);
							icon = new CompoundIcon(icon) {
								@Override
							    public int getIconWidth() {
									return 32;
								}
							};
							label2.setIcon(icon);
						}
				        label2.addMouseListener(new MouseAdapter() {
							@Override
							public void mouseClicked(MouseEvent e) {
								jCheckBox2.setSelected(!jCheckBox2.isSelected());
							}
						});
				        label2.setToolTipText(
								"<html><b>" + UIUtil.toHTMLFragment(ci.alias, 0) + "</b><br>" +
								(ci.user != null && ci.user.length() > 0? UIUtil.toHTMLFragment(ci.user, 0) + "<br>" : "") +
								UIUtil.toHTMLFragment(ci.url, 0) +
								(ci.getConnectionType().getBackground() != null? "<br>" + ci.getConnectionType().displayName : "")
								);
				        gridBagConstraints = new java.awt.GridBagConstraints();
				        gridBagConstraints.gridx = 2;
				        gridBagConstraints.gridy = y;
				        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
				        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
				        gridBagConstraints.weightx = 1.0;
				        gridBagConstraints.insets = new Insets(0, 20, 0, 0);
				        selectionPanel.add(label2, gridBagConstraints);
				        ++y;
		        	}
		        }
			}
			updateStatus();
			UIUtil.initComponents(this);
		} finally {
			DataModelManager.setCurrentModelSubfolder(cmsf, connectionDialog.executionContext);
		}
		
		dialog.setVisible(true);
		return ok;
    }
    
    private void selectAndScrollTo(JCheckBox jCheckBox) {
		UIUtil.invokeLater(() -> {
			jCheckBox.setSelected(true);
			jCheckBox.scrollRectToVisible(new Rectangle(0, -2 * jCheckBox.getHeight(), 1, selectionPanel.getHeight() - 64));
		});
	}

	private String okMessage;
    
	private void updateStatus() {
		String info = "";
		if (selectedModels.size() > 0) {
			info += selectedModels.size() + " data model" + (selectedModels.size() == 1? "" : "s");
		}
		if (selectedConnections.size() > 0) {
			if (!info.isEmpty()) {
				info += " and ";
			}
			info += selectedConnections.size() + " connection" + (selectedConnections.size() == 1? "" : "s");
		}
		okMessage = info + " exported.";
		if (!info.isEmpty()) {
			info += " selected.";
			selectAllButton.setText("Deselect All");
		} else {
			selectAllButton.setText("Select All");
		}
		infoLabel.setText(info.isEmpty()? " " : info);
		
		okButton.setEnabled(!selectedConnections.isEmpty() || !selectedModels.isEmpty());
	}

	private List<JCheckBox> checkBoxes = new ArrayList<>();

	/**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jLabel1 = new javax.swing.JLabel();
        jPanel1 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        jPanel2 = new javax.swing.JPanel();
        selectionPanel = new javax.swing.JPanel();
        jPanel3 = new javax.swing.JPanel();
        jPanel5 = new javax.swing.JPanel();
        okButton = new javax.swing.JButton();
        cancelButton = new javax.swing.JButton();
        infoLabel = new javax.swing.JLabel();
        selectAllButton = new javax.swing.JButton();

        setLayout(new java.awt.GridBagLayout());

        jLabel1.setText("Select data models and connections to be exported.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(6, 6, 6, 6);
        add(jLabel1, gridBagConstraints);

        jPanel1.setLayout(new javax.swing.BoxLayout(jPanel1, javax.swing.BoxLayout.LINE_AXIS));

        jPanel2.setLayout(new java.awt.GridBagLayout());

        selectionPanel.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.weightx = 1.0;
        jPanel2.add(selectionPanel, gridBagConstraints);

        jPanel3.setLayout(new java.awt.GridBagLayout());

        jPanel5.setLayout(null);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.insets = new java.awt.Insets(6, 0, 0, 0);
        jPanel3.add(jPanel5, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel2.add(jPanel3, gridBagConstraints);

        jScrollPane1.setViewportView(jPanel2);

        jPanel1.add(jScrollPane1);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
        add(jPanel1, gridBagConstraints);

        okButton.setText("  Export  ");
        okButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                okButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 2, 4, 2);
        add(okButton, gridBagConstraints);

        cancelButton.setText(" Cancel ");
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 4);
        add(cancelButton, gridBagConstraints);

        infoLabel.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(6, 6, 6, 6);
        add(infoLabel, gridBagConstraints);

        selectAllButton.setText("Select All");
        selectAllButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                selectAllButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 2, 4, 2);
        add(selectAllButton, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    private void okButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_okButtonActionPerformed
    	if (!selectedConnections.isEmpty() || !selectedModels.isEmpty()) {
    		doExport();
    	}
        ok = true;
    	dialog.setVisible(false);
        dialog.dispose();
    }//GEN-LAST:event_okButtonActionPerformed

    protected void doExport() {
		String cmsf = DataModelManager.getCurrentModelSubfolder(executionContext);
		try {
			DataModelManager.setCurrentModelSubfolder(null, executionContext);
			File proposal = null;
			if (selectedModels.size() == 1) {
				proposal = new File(selectedModels.iterator().next() + EXPORT_FILE_EXTENSION);
			} else if (selectedModels.isEmpty() && selectedConnections.size() == 1) {
				proposal = new File(selectedConnections.iterator().next().alias.replaceAll("[\\\\/:;*?\"\\<\\>\\|]+","_") + EXPORT_FILE_EXTENSION);
			}
			String file = UIUtil.choseFile(proposal, ".", dialog.getTitle(), EXPORT_FILE_EXTENSION, ExportPanel.this, true, false, false);
			UIUtil.setWaitCursor(this);
			if (file != null) {
				try {
					File cFile = null;
					if (!selectedConnections.isEmpty()) {
						cFile = Configuration.getInstance().createTempFile();
						DbConnectionDialog.saveAsCSV(selectedConnections, cFile);
					}
					zip(file, cFile);
					if (cFile != null) {
						cFile.delete();
					}
			    	JOptionPane.showMessageDialog(this, okMessage);
				} catch (IOException e) {
					UIUtil.showException(this, "Error", e);
				}
			}
		} finally {
			DataModelManager.setCurrentModelSubfolder(cmsf, executionContext);
			UIUtil.resetWaitCursor(this);
		}
	}
    
    public static Pair<Boolean, Pair<Integer, List<ConnectionInfo>>> openImportDialog(Window owner, DbConnectionDialog connectionDialog) {
    	ExecutionContext executionContext = connectionDialog.executionContext;
		
    	String oldDatamodelFolder = executionContext.getDatamodelFolder();
    	File tmpFile = null;
		try {
    		tmpFile = Configuration.getInstance().createTempFile();
    		executionContext.setDatamodelFolder(tmpFile.getPath());
    		Path targetPath = tmpFile.toPath();
     		
    		String file = UIUtil.choseFile(null, ".", "Import data models and connections", EXPORT_FILE_EXTENSION, owner, false, true, false);
    		if (file != null) {
				Pair<Boolean, Pair<Integer, List<ConnectionInfo>>> importTmp = doImport(owner, file, true, null, null, connectionDialog);
	    		Set<String> tabuModels = new HashSet<>();
	    		if (importTmp.a) {
	    			List<ConnectionInfo> connectionList = importTmp.b.b;
					Path start = Paths.get(oldDatamodelFolder);
					Files.walkFileTree(start, new SimpleFileVisitor<Path>() {
	    	    		private Path sourcePath = null;
	
	    	    		@Override
	    	    		public FileVisitResult preVisitDirectory(final Path dir, final BasicFileAttributes attrs) throws IOException {
	    	    			if (!dir.equals(start)) {
	    	    				if (!connectionList.stream().anyMatch(ci -> dir.endsWith(ci.dataModelFolder))) {
	    	    					return FileVisitResult.SKIP_SUBTREE;
	    	    				}
	    	    			}
	    	    			if (sourcePath == null) {
	    	    				sourcePath = dir;
	    	    			}
	    	    			Path targetDir = targetPath.resolve(sourcePath.relativize(dir));
	    	    			if (targetDir.toFile().exists()) {
	    	    				return FileVisitResult.SKIP_SUBTREE;
	    	    			} else {
	    	    				tabuModels.add(dir.toFile().getName());
	    	    			}
							Files.createDirectories(targetDir);
	    	    			return FileVisitResult.CONTINUE;
	    	    		}
	    	
	    	    		@Override
	    	    		public FileVisitResult visitFile(final Path file, final BasicFileAttributes attrs) throws IOException {
	    	    			try {
	    	    				Path target = sourcePath == null ? targetPath : targetPath.resolve(sourcePath.relativize(file));
	    	    				if (!target.toFile().exists()) {
	    	    					Files.copy(file, target);
	    	    				}
	    	    			} catch (Exception e) {
	    	    				// ignore
	    	    			}
	    	    			return FileVisitResult.CONTINUE;
	    	    		}
	    	    	});
	    			ExportPanel exportPanel = new ExportPanel() {
	    				@Override
	    				protected void doExport() {
	    				}
	    			};
	    			exportPanel.okButton.setText("  Import  ");
					if (exportPanel.openDialog(owner, true, connectionList, tabuModels, "Import data models and connections", connectionDialog)) {
			    		executionContext.setDatamodelFolder(oldDatamodelFolder);
						return doImport(owner, file, false, exportPanel.selectedConnections, exportPanel.selectedModels, connectionDialog);
					}
	    		}
    		}
    	} catch (Exception e) {
			UIUtil.showException(owner, "Error", e);
		} finally {
    		executionContext.setDatamodelFolder(oldDatamodelFolder);
    		if (tmpFile != null) {
        		deleteDir(tmpFile);
    		}
    	}
		return new Pair<Boolean, Pair<Integer, List<ConnectionInfo>>>(false, new Pair<Integer, List<ConnectionInfo>>(0, null));
    }
    
    private static void deleteDir(File file) {
    	try {
	        File[] contents = file.listFiles();
	        if (contents != null) {
	            for (File f : contents) {
	                deleteDir(f);
	            }
	        }
	        file.delete();
    	} catch (Throwable t) {
    		LogUtil.warn(t);
    	}
    }

    private static Pair<Boolean, Pair<Integer, List<ConnectionInfo>>> doImport(Window owner, String file, boolean silent, Set<ConnectionInfo> selectedConnections, Set<String> selectedModels, DbConnectionDialog connectionDialog) {
    	ExecutionContext executionContext = connectionDialog.executionContext;
		String cmsf = DataModelManager.getCurrentModelSubfolder(executionContext);
		int numModels = 0;
		List<ConnectionInfo> cList = null;
		try {
			DataModelManager.setCurrentModelSubfolder(null, executionContext);
			UIUtil.setWaitCursor(owner);
			if (file != null) {
				try {
					ZipInputStream zipIn = new ZipInputStream(new FileInputStream(file));

					ZipEntry entry = zipIn.getNextEntry();
					boolean ok = false;
					while (entry != null) {
						String name2 = entry.getName();
						if (name2.endsWith(EXPORT_MARKER)) {
							ok = true;
						}
						zipIn.closeEntry();
						entry = zipIn.getNextEntry();
					}
					zipIn.close();
					
					if (!ok) {
						JOptionPane.showMessageDialog(owner, "\"" + file + "\" is not a valid Jailer export file.", "Error", JOptionPane.ERROR_MESSAGE);
						return new Pair<Boolean, Pair<Integer, List<ConnectionInfo>>>(false, new Pair<Integer, List<ConnectionInfo>>(0, null));
					}
					File destDir = new File(executionContext.getDatamodelFolder());
					if (!destDir.exists()) {
						destDir.mkdir();
					}
					zipIn = new ZipInputStream(new FileInputStream(file));

					entry = zipIn.getNextEntry();
					Set<String> models = new HashSet<>();
					Set<String> seenTopLevelDirs = new HashSet<>();
					// iterates over entries in the zip file
					while (entry != null) {
						String name = entry.getName();
						if (!name.endsWith(EXPORT_MARKER) && (selectedModels == null || selectedModels.stream().anyMatch(m -> name.startsWith(m + "/") || name.startsWith(m + "\\")))) {
							if (name.equals(DbConnectionDialog.CONNECTIONS_FILE)) {
								File cFile = Configuration.getInstance().createTempFile();
								BufferedOutputStream bos = new BufferedOutputStream(new FileOutputStream(cFile));
								byte[] bytesIn = new byte[4096];
								int read = 0;
								while ((read = zipIn.read(bytesIn)) != -1) {
									bos.write(bytesIn, 0, read);
								}
								bos.close();
								cList = DbConnectionDialog.loadAsCSV(cFile);
								cFile.delete();
							} else {
								String filePath = executionContext.getDatamodelFolder() + File.separator + name;
								String topLevelDir = name.replaceFirst("[/\\\\].*", "");
								if (!topLevelDir.equals(name)) {
									if (seenTopLevelDirs.add(topLevelDir)) {
										deleteDir(new File(executionContext.getDatamodelFolder() + File.separator + topLevelDir));
									}
								}
								if (!entry.isDirectory()) {
									// if the entry is a file, extracts it
									models.add(new File(filePath).getParentFile().getName());
									new File(filePath).getParentFile().mkdirs();
									BufferedOutputStream bos = new BufferedOutputStream(new FileOutputStream(filePath));
									byte[] bytesIn = new byte[4096];
									int read = 0;
									while ((read = zipIn.read(bytesIn)) != -1) {
										bos.write(bytesIn, 0, read);
									}
									bos.close();
								} else {
									// if the entry is a directory, make the directory
									File dir = new File(filePath);
									dir.mkdirs();
								}
							}
						}
						zipIn.closeEntry();
						entry = zipIn.getNextEntry();
					}
					zipIn.close();
					
					if (selectedConnections != null) {
						cList = new ArrayList<>(selectedConnections);
					}
					
					numModels = models.size();
					int numConnections = cList == null? 0 : cList.size();
					
					String info = "";
					if (numModels > 0) {
						info += numModels + " data model" + (numModels == 1? "" : "s");
					}
					if (numConnections > 0) {
						if (!info.isEmpty()) {
							info += " and ";
						}
						info += numConnections + " connection" + (numConnections == 1? "" : "s");
					}
					info = info + " imported.";
					if (!silent) {
						String message = info;
						UIUtil.invokeLater(() -> JOptionPane.showMessageDialog(owner, message));
					}
				} catch (Exception e) {
					UIUtil.showException(owner, "Error", e);
					return new Pair<Boolean, Pair<Integer, List<ConnectionInfo>>>(false, new Pair<Integer, List<ConnectionInfo>>(1, cList));
				}
			} else {
				return new Pair<Boolean, Pair<Integer, List<ConnectionInfo>>>(false, new Pair<Integer, List<ConnectionInfo>>(0, null));
			}
		} finally {
			DataModelManager.setCurrentModelSubfolder(cmsf, executionContext);
			UIUtil.resetWaitCursor(owner);
		}
		return new Pair<Boolean, Pair<Integer, List<ConnectionInfo>>>(true, new Pair<Integer, List<ConnectionInfo>>(numModels, cList));
    }
  
    private void zip(String zipFilePath, File cFile) throws IOException {
		Path p = Paths.get(zipFilePath);
		Path dmPath = Paths.get(executionContext.getDatamodelFolder());
		try (ZipOutputStream zs = new ZipOutputStream(Files.newOutputStream(p))) {
			for (String dm: selectedModels) {
				Files.walk(dmPath.resolve(dm)).filter(path -> !Files.isDirectory(path)).forEach(path -> {
					ZipEntry zipEntry = new ZipEntry(dmPath.relativize(path).toString().replace(File.separatorChar, '/'));
					try {
						zs.putNextEntry(zipEntry);
						Files.copy(path, zs);
						zs.closeEntry();
					} catch (IOException e) {
						throw new RuntimeException(e.getMessage(), e);
					}
				});
			}
			if (cFile != null) {
				zs.putNextEntry(new ZipEntry(DbConnectionDialog.CONNECTIONS_FILE));
				Files.copy(cFile.toPath(), zs);
			}
			zs.putNextEntry(new ZipEntry(EXPORT_MARKER));
		}
	}

    private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
        ok = false;
        dialog.setVisible(false);
        dialog.dispose();
    }//GEN-LAST:event_cancelButtonActionPerformed

    private void selectAllButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_selectAllButtonActionPerformed
    	if (checkBoxes.stream().filter(cb -> cb.isSelected()).findAny().isPresent()) {
    		checkBoxes.forEach(cb -> cb.setSelected(false));
    	} else {
    		checkBoxes.forEach(cb -> cb.setSelected(true));
    	}
    }//GEN-LAST:event_selectAllButtonActionPerformed

	private static ImageIcon okIcon;
	private static ImageIcon cancelIcon;
	private static ImageIcon modelIcon;
	
    static {
        // load images
    	okIcon = UIUtil.readImage("/buttonok.png");
        cancelIcon = UIUtil.readImage("/buttoncancel.png");
        modelIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/model.png"));
    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton cancelButton;
    private javax.swing.JLabel infoLabel;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JButton okButton;
    private javax.swing.JButton selectAllButton;
    private javax.swing.JPanel selectionPanel;
    // End of variables declaration//GEN-END:variables
}
