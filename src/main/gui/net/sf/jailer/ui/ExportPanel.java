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
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
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
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.WindowConstants;

import org.fife.rsta.ui.EscapableDialog;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.ui.DbConnectionDialog.ConnectionInfo;
import net.sf.jailer.ui.UIUtil.PLAF;
import net.sf.jailer.ui.util.CompoundIcon;
import net.sf.jailer.util.Pair;

/**
 *
 * @author RalfW
 */
public class ExportPanel extends javax.swing.JPanel {

    /**
     * Creates new form ExportPanel
     */
    public ExportPanel() {
        initComponents();
        okButton.setIcon(UIUtil.scaleIcon(okButton, okIcon));
		cancelButton.setIcon(UIUtil.scaleIcon(cancelButton, cancelIcon));
		if (UIUtil.plaf == PLAF.FLAT) {
			selectionPanel.setBackground(Color.white);
			jPanel3.setBackground(Color.white);
		}
    }
    
    private static final String EXPORT_MARKER = "JailerModelExportArchiv";

    private JDialog dialog;
    private boolean ok = false;
    
    private Set<String> selectedModels = new TreeSet<>(String::compareToIgnoreCase);
    private Set<ConnectionInfo> selectedConnections = new TreeSet<>((a, b) -> a.alias.compareToIgnoreCase(b.alias));
    private ExecutionContext executionContext;
    
    public boolean openExportDialog(Window owner, Object initiallySelected, DbConnectionDialog connectionDialog) {
    	executionContext = connectionDialog.executionContext;
    	dialog = owner instanceof Dialog? new EscapableDialog((Dialog) owner) {
		} : new EscapableDialog((Frame) owner) {
		};
		dialog.getContentPane().add(this);
		dialog.setModal(true);
		dialog.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		dialog.setTitle("Export data models and connections");
		dialog.pack();
		dialog.setSize(Math.max(500, dialog.getWidth()), Math.min(700, Math.max(500, dialog.getHeight()))); 
		// TODO
		// TODO check size bounds
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
				String displayName = DataModelManager.getModelDetails(model, connectionDialog.executionContext).a;
				modelNames.put(model, displayName + (dup.contains(displayName) ? " (" + model + ")" : ""));
			}
			
			int y = 1;
			List<ConnectionInfo> connections = new ArrayList<>(connectionDialog.getConnectionList());
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
				if (entry != null) {
					 checkBoxes.add(jCheckBox);
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
					jCheckBox.setText(" ");
					jCheckBox.setFocusable(false);
			        GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
			        gridBagConstraints.gridx = 1;
			        gridBagConstraints.gridy = y;
			        gridBagConstraints.insets = new Insets(b, 0, b, 0);
			        selectionPanel.add(jCheckBox, gridBagConstraints);
			        if (key.equals(initiallySelected)) {
			        	selectAndScrollTo(jCheckBox);
			        }
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
				        if (ci.equals(initiallySelected)) {
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

    private void doExport() {
		String cmsf = DataModelManager.getCurrentModelSubfolder(executionContext);
		try {
			DataModelManager.setCurrentModelSubfolder(null, executionContext);
			String file = UIUtil.choseFile(null, ".", dialog.getTitle(), ".zip", ExportPanel.this, true, false);
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
    
    public static Pair<Boolean, Pair<Integer, List<ConnectionInfo>>> doImport(Window owner, DbConnectionDialog connectionDialog) {
    	ExecutionContext executionContext = connectionDialog.executionContext;
		String cmsf = DataModelManager.getCurrentModelSubfolder(executionContext);
		int numModels = 0;
		List<ConnectionInfo> cList = null;
		try {
			DataModelManager.setCurrentModelSubfolder(null, executionContext);
			String file = UIUtil.choseFile(null, ".", "Import data models and connections", ".zip", owner, true, true);
			UIUtil.setWaitCursor(owner);
			if (file != null) {
				try {
					ZipInputStream zipIn = new ZipInputStream(new FileInputStream(file));

					ZipEntry entry = zipIn.getNextEntry();
					boolean ok = false;
					while (entry != null) {
						if (entry.getName().endsWith(EXPORT_MARKER)) {
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
					// iterates over entries in the zip file
					while (entry != null) {
						if (!entry.getName().endsWith(EXPORT_MARKER)) {
							if (entry.getName().equals(DbConnectionDialog.CONNECTIONS_FILE)) {
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
								String filePath = executionContext.getDatamodelFolder() + File.separator + entry.getName();
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
					JOptionPane.showMessageDialog(owner, info);
				} catch (Exception e) {
					UIUtil.showException(owner, "Error", e);
					return new Pair<Boolean, Pair<Integer, List<ConnectionInfo>>>(true, new Pair<Integer, List<ConnectionInfo>>(1, cList));
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

    // TODO
    // TODO test with different base dir
  
    private void zip(String zipFilePath, File cFile) throws IOException {
		Path p = Paths.get(zipFilePath);
		Path dmPath = Paths.get(executionContext.getDatamodelFolder());
		try (ZipOutputStream zs = new ZipOutputStream(Files.newOutputStream(p))) {
			for (String dm: selectedModels) {
				Files.walk(dmPath.resolve(dm)).filter(path -> !Files.isDirectory(path)).forEach(path -> {
					ZipEntry zipEntry = new ZipEntry(dmPath.relativize(path).toString());
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
