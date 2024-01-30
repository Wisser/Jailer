/*
 * Copyright 2007 - 2024 Ralf Wisser.
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
import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowFocusListener;
import java.io.File;
import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListCellRenderer;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.ListCellRenderer;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.border.Border;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.ui.DbConnectionDialog.ConnectionInfo;
import net.sf.jailer.ui.DbConnectionDialog.ConnectionType;
import net.sf.jailer.ui.UIUtil.PLAF;
import net.sf.jailer.ui.util.ConcurrentTaskControl;
import net.sf.jailer.ui.util.HttpDownload;
import net.sf.jailer.ui.util.LightBorderSmallButton;
import net.sf.jailer.ui.util.SmallButton;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.CsvFile.Line;
import net.sf.jailer.util.Pair;

/**
 * "Connect with DB" dialog.
 *
 * @author Ralf Wisser
 */
public class DbConnectionDetailsEditor extends javax.swing.JDialog {

	/**
	 * <code>true</code> if valid connection is available.
	 */
	private boolean isOk = false;
	
	/**
	 * The connection to edit.
	 */
	private ConnectionInfo ci;

	/**
	 * <code>true</code> if connection must be tested on OK.
	 */
	private final boolean needsTest;
	
	/**
	 * The {@link ExecutionContext}.
	 */
	private ExecutionContext executionContext;
	
	/**
	 * Opens detail editor for a connection.
	 * 
	 * @param ci the connection
	 * @param connectionList 
	 * @param executionContext 
	 * @return <code>true</code> if connection has been edited
	 */
	public boolean edit(ConnectionInfo ci, List<ConnectionInfo> connectionList, ExecutionContext executionContext) {
		this.connectionList = connectionList;
		this.executionContext = executionContext;
		setDetails(ci);
		loadModelList(executionContext);
		dataModelComboBox.setSelectedItem(ci.dataModelFolder == null? "" : ci.dataModelFolder);
		firePending = false;
		setVisible(true);
		if (firePending) {
			UIUtil.invokeLater(4, () -> {
				listener.forEach(l -> {
					try {
						l.run();
					} catch (Throwable t) {
						// ignore
					}
				});
			});
		}
		return isOk;
	}
	
	private List<ConnectionInfo> connectionList;
	
	/**
	 * List of available models.
	 */
	private List<String> modelList;

	/**
	 * Model details as pair of folder-name and last-modified timestamp.
	 */
	private Map<String, Pair<String, Long>> modelDetails;

	private void loadModelList(ExecutionContext executionContext) {
		String cmsf = DataModelManager.getCurrentModelSubfolder(executionContext);
		try {
			DataModelManager.setCurrentModelSubfolder(null, executionContext);
	
			modelList = new ArrayList<String>();
			modelDetails = new HashMap<String, Pair<String,Long>>();
			for (String mf: DataModelManager.getModelFolderNames(executionContext)) {
				String modelFolder = mf == null? "" : mf;
				modelList.add(modelFolder);
				modelDetails.put(modelFolder, DataModelManager.getModelDetails(mf, executionContext));
			}
			String dm = ci.dataModelFolder == null? "" : ci.dataModelFolder;
			if (!modelList.contains(dm)) {
				modelList.add(dm);
				modelDetails.put(dm, DataModelManager.getModelDetails(dm, executionContext));
			}
			Collections.sort(modelList, new Comparator<String>() {
				@Override
				public int compare(String o1, String o2) {
					return modelDetails.get(o1).a.compareToIgnoreCase(modelDetails.get(o2).a);
				}
			});
			if (!dataModelAware && !modelList.contains("")) {
				modelList.add(0, "");
			}
			dataModelComboBox.setModel(new DefaultComboBoxModel<String>(modelList.toArray(new String[0])));
			ListCellRenderer<? super String> renderer = dataModelComboBox.getRenderer();
			dataModelComboBox.setRenderer(new DefaultListCellRenderer() {
				@Override
				public Component getListCellRendererComponent(JList<?> list, Object value, int index, boolean isSelected,
						boolean cellHasFocus) {
					final Pair<String, Long> pair = modelDetails.get((String) value);
					return renderer.getListCellRendererComponent((JList<String>) list, "".equals(value)? "" : pair == null? (String) value : pair.a, index, isSelected, cellHasFocus);
				}
			});
			Color fg = jLabel5.getForeground();
			Color bg = dataModelComboBox.getBackground();
			ItemListener l;
			dataModelComboBox.addItemListener(l = new ItemListener() {
				@Override
				public void itemStateChanged(ItemEvent e) {
					if (!dataModelAware) {
						if ("".equals(dataModelComboBox.getSelectedItem())) {
							dataModelComboBox.setBackground(new Color(255, 200, 200));
							jLabel5.setForeground(Color.red);
						} else {
							jLabel5.setForeground(fg);
							dataModelComboBox.setBackground(bg);
						}
					}
				}
			});
			l.itemStateChanged(null);
		} finally {
			DataModelManager.setCurrentModelSubfolder(cmsf, executionContext);
		}
	}

	/**
	 * Sets details.
	 * 
	 * @param ci the connection
	 * @return <code>true</code> if connection has been edited
	 */
	public void setDetails(ConnectionInfo ci) {
		this.ci = ci;
		nameContent = "@";
		alias.setText(ci.alias);
		dbUrl.setText(ci.url);
		updateDbURLStyle();
        user.setText(ci.user);
		password.setText(ci.password);
		driverClass.setText(ci.driverClass);
		typeComboBox.setSelectedItem(ci.getConnectionType());
		jar1.setText(ci.jar1);
		jar2.setText(ci.jar2);
		jar3.setText(ci.jar3);
		jar4.setText(ci.jar4);
		try {
			dbUrl.setCaretPosition(0);
			driverClass.setCaretPosition(0);
		} catch (Exception e) {
			// ignore
		}
		String newName = createNewName(ci.user, ci.url);
		if (newName.replaceFirst("\\s*\\(\\d+\\)$", "").equals(ci.alias.replaceFirst("\\s*\\(\\d+\\)$", ""))) {
			nameContent = ci.alias;
		}
	}

	private void updateDbURLStyle() {
		try {
			String tr = dbUrl.getText().replaceAll("\\n|\\t", "");
			if (!tr.equals(dbUrl.getText())) {
				dbUrl.setText(tr);
			}
			SimpleAttributeSet set = new SimpleAttributeSet();
			for (int i = 0; i < dbUrl.getDocument().getLength(); ++i) {
				final String c0 = dbUrl.getDocument().getText(i, 1);
				if (!c0.equals("[") && !c0.equals("]") && !c0.equals("<") && !c0.equals(">")) {
					((DefaultStyledDocument) dbUrl.getDocument()).setCharacterAttributes(i, 1, set, true);
				}
			}
			Color color = Color.RED;
			Pattern pattern = Pattern.compile("\\<([^\\>\\<]*)\\>");
			Matcher matcher = pattern.matcher(dbUrl.getText());
			while (matcher.find()) {
				if (acceptParameter(matcher.group(1))) {
					set = new SimpleAttributeSet();
					StyleConstants.setForeground(set, color);
					((DefaultStyledDocument) dbUrl.getDocument()).setCharacterAttributes(matcher.start(),
							matcher.end() - matcher.start(), set, true);
				}
			}
			pattern = Pattern.compile("\\[[^\\[\\]]*\\<([^\\>\\<]*)\\>[^\\]\\[]*\\]");
			matcher = pattern.matcher(dbUrl.getText());
			while (matcher.find()) {
				if (acceptParameter(matcher.group(1))) {
					set = new SimpleAttributeSet();
					StyleConstants.setForeground(set, color);
					((DefaultStyledDocument) dbUrl.getDocument()).setCharacterAttributes(matcher.start(), 1, set, true);
					((DefaultStyledDocument) dbUrl.getDocument()).setCharacterAttributes(matcher.end() - 1, 1, set, true);
				}
			}
		} catch (Exception e) {
			// ignore
		}
	}

	/** Creates new form DbConnectionDialog 
	 * @param forNew 
	 * @param dataModelAware */
	public DbConnectionDetailsEditor(Window parent, final String jdbcHelpURL, boolean forNew, boolean forClone, boolean dataModelAware) {
		this(parent, jdbcHelpURL, forNew, forClone, null, false, dataModelAware);
	}

	private final Window parent;
	private List<Line> driverlist;
	private String nameContent = "";
	
	private void updateNameField() {
		String usr = user.getText().trim();
		String url = dbUrl.getText();
		String newName = createNewName(usr, url);
		if (nameContent.equals(alias.getText()) || alias.getText().trim().isEmpty()) {
			alias.setText(newName);
			nameContent = newName;
		}
	}

	private List<Pattern> pattern = new ArrayList<Pattern>();
	private final boolean dataModelAware;
	
	/** Creates new form DbConnectionDialog
	 */
	public DbConnectionDetailsEditor(Window parent, final String jdbcHelpURL, boolean forNew, boolean forClone, InfoBar infoBar, boolean needsTest, boolean dataModelAware) {
		super(parent);
		setModal(true);
		this.parent = parent;
		this.needsTest = needsTest;
		this.dataModelAware = dataModelAware;
		initComponents(); UIUtil.initComponents(this);
		
		typeComboBox.setModel(new DefaultComboBoxModel(DbConnectionDialog.ConnectionType.values()));
		Color dbg = typeComboBox.getBackground();
		typeComboBox.setRenderer(new DefaultListCellRenderer() {
			@Override
			public Component getListCellRendererComponent(JList<?> list, Object value, int index, boolean isSelected,
					boolean cellHasFocus) {
				Component render = super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
				if (value instanceof ConnectionType && render instanceof JLabel) {
					if (!isSelected) {
						Color b1 = ((ConnectionType) value).getBackground();
						render.setBackground(b1 == null? dbg : b1);
						render.setForeground(Color.black);
					}
				}
				return render;
			}
		});
		typeComboBox.addItemListener(e -> {
			if (typeComboBox.getSelectedItem() != null) {
				Color b1 = ((ConnectionType) typeComboBox.getSelectedItem()).getBackground();
				typeComboBox.setBackground(b1 == null? dbg : b1);
			}
		});
		
		newDataModelButton.setVisible(!dataModelAware);
		ImageIcon scaledWarnIcon = UIUtil.scaleIcon(jtdsWarnLabel, warnIcon, 1);
		jtdsWarnLabel.setIcon(scaledWarnIcon);
		jtdsWarnLabel.setVisible(false);
		jSeparator2.setVisible(false);
		okButton.setIcon(UIUtil.scaleIcon(okButton, okIcon));
		cancelButton.setIcon(UIUtil.scaleIcon(cancelButton, cancelIcon));
		renameButton.setIcon(resetIcon);
		renameButton.setText(null);
		feedbackTimer = new Timer(4000, e -> {
			feedbackLabel.setText(" ");
		});
		feedbackTimer.setRepeats(false);
		textFields = new JTextComponent[] {
	    		alias,
	    		jar1,
	    		jar2,
	    		jar3,
	    		jar4,
	    		driverClass,
	    		dbUrl,
	    		user
	    };
		
		
		DefaultStyledDocument doc = new DefaultStyledDocument();
		dbUrl.setDocument(doc);
		Border nfBorder = user.getBorder();
		Border fBorder = BorderFactory.createLineBorder(new Color(137, 176, 212));
		jScrollPane1.setBorder(nfBorder);
		dbUrl.addKeyListener(new KeyListener() {
			@Override
			public void keyTyped(KeyEvent e) {
				if (e.getKeyChar() == '\t' || e.getKeyChar() == '\n') {
					user.grabFocus();
				}
			}
			@Override
			public void keyReleased(KeyEvent e) {
			}
			@Override
			public void keyPressed(KeyEvent e) {
			}
		});
		dbUrl.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() > 1) {
					UIUtil.invokeLater(2, () -> {
						Pattern pattern = Pattern.compile("\\[?\\<([^\\>\\<]*)\\>\\]?");
						Matcher matcher = pattern.matcher(dbUrl.getText());
						while (matcher.find()) {
							if (acceptParameter(matcher.group(1))) {
								if (matcher.start() <= dbUrl.getCaretPosition() && dbUrl.getCaretPosition() <= matcher.end()) {
									dbUrl.select(matcher.start(), matcher.end());
									break;
								}
							}
						}
					});
				}
			}
		});
		Border errFBorder = BorderFactory.createLineBorder(new Color(255, 100, 100));
		Border errBorder = BorderFactory.createLineBorder(new Color(255, 50, 50));
		Runnable updateBorder = () -> {
			if (UIUtil.plaf == PLAF.FLAT) {
				boolean ok = true;
				try {
					for (int i = 0; i < dbUrl.getStyledDocument().getLength(); ++i) {
						Element el = dbUrl.getStyledDocument().getCharacterElement(i);
						if (el.getAttributes().getAttributeCount() > 0) {
							ok = false;
						}
					}
				} catch (Throwable t) {
					ok = true;
				}
				jScrollPane1.setBorder(dbUrl.hasFocus() ? ok ? fBorder : errFBorder : ok ? nfBorder : errBorder);
			}
		};
		dbUrl.addFocusListener(new FocusListener() {
			@Override
			public void focusLost(FocusEvent e) {
				updateBorder.run();
			}
			@Override
			public void focusGained(FocusEvent e) {
				updateBorder.run();
			}
		});

		doc.addDocumentListener(new DocumentListener() {
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
			}
			private void update() {
				UIUtil.invokeLater(() -> { updateBorder.run(); updateDbURLStyle(); });
			}
		});
		
		try {
			CsvFile drivers = new CsvFile(Environment.newWorkingFolderFile("driverlist.csv"));
			driverlist = new ArrayList<Line>(drivers.getLines());
		} catch (Throwable t) {
			driverlist = null;
		}
		
		if (driverlist != null) {
			try {
				driverlist.forEach(line -> {
					String res = line.cells.get(5).trim();
					if (res.startsWith("\"") && res.endsWith("\"")) {
						res = res.substring(1, res.length() - 2).trim();
					}
					for (String re: res.split(" +")) {
						try {
							pattern.add(Pattern.compile(re, Pattern.CASE_INSENSITIVE));
						} catch (Throwable t) {
							t.printStackTrace();
						}
					}
				});
			} catch (Throwable t) {
				// ignore
			}
		}
		
		alias.addFocusListener(new FocusListener() {
			@Override
			public void focusLost(FocusEvent e) {
				updateNameField();
			}
			@Override
			public void focusGained(FocusEvent e) {
			}
		});
		DocumentListener docListener = new DocumentListener() {
			@Override
			public void removeUpdate(DocumentEvent e) {
				updateNameField();
			}
			@Override
			public void insertUpdate(DocumentEvent e) {
				updateNameField();
			}
			@Override
			public void changedUpdate(DocumentEvent e) {
//				updateNameField();
			}
		};
		dbUrl.getDocument().addDocumentListener(docListener);
		user.getDocument().addDocumentListener(docListener);
		
		dbUrl.getDocument().addDocumentListener(new DocumentListener() {
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
//				update();
			}
			private String getDBMSDriver(String url) {
				if (!url.matches("jdbc:.+")) {
					return null;
				}
				Optional<Line> result = driverlist.stream().filter(line -> {
					String prefix = line.cells.get(1).replaceFirst("[</\\[@].*$", "");
					return !prefix.isEmpty() && url.startsWith(prefix);
				}).findAny();
				if (result.isPresent()) {
					return result.get().cells.get(2);
				}
				return null;
			}
			private void update() {
				String driver = driverClass.getText().trim();
				if (driver.isEmpty() || driverlist.stream().filter(line -> line.cells.get(2).equals(driver)).findFirst().isPresent()) {
					String fromUrl = getDBMSDriver(dbUrl.getText().trim());
					if (fromUrl != null && !fromUrl.isEmpty()) {
						driverClass.setText(fromUrl);
					}
				}
				String dbmsLogoURL = UIUtil.getDBMSLogoURL(dbUrl.getText().trim());
		        if (dbmsLogoURL == null || dbmsLogoURL.contains("other_small")) {
		        	logoLabel.setIcon(null);
		        } else {
					logoLabel.setIcon(UIUtil.scaleIcon(logoLabel, UIUtil.readImage(dbmsLogoURL, false), 2));
		        }
		        DbConnectionSettings settingsDialog = createSettingsDialog(false);
				editButton.setEnabled(settingsDialog != null);
			}
		});
		
		driverClass.getDocument().addDocumentListener(new DocumentListener() {
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
//				update();
			}
			private void update() {
				jtdsWarnLabel.setVisible(driverClass.getText().equals("net.sourceforge.jtds.jdbc.Driver"));
				jSeparator2.setVisible(false);
			}
		});
		DocumentListener renameListener = new DocumentListener() {
			private void check() {
				String usr = user.getText().trim();
				String url = dbUrl.getText();
				String newName = createNewName(usr, url);
				renameButton.setEnabled(!newName.equals(alias.getText()));
			}
			@Override
			public void removeUpdate(DocumentEvent e) {
				check();
			}
			@Override
			public void insertUpdate(DocumentEvent e) {
				check();
			}
			@Override
			public void changedUpdate(DocumentEvent e) {
//				check();
			}
		};
		dbUrl.getDocument().addDocumentListener(renameListener);
		user.getDocument().addDocumentListener(renameListener);
		alias.getDocument().addDocumentListener(renameListener);
		
		renameButton.addActionListener(e -> {
			String usr = user.getText().trim();
			String url = dbUrl.getText();
			alias.setText(nameContent = createNewName(usr, url));
			alias.grabFocus();
		});
		
		UIUtil.setTrailingComponent(alias, renameButton);
		
		Arrays.asList(jar1, jar2, jar3, jar4, dbUrl).forEach(f -> f.getDocument().addDocumentListener(new DocumentListener() {
			@Override
			public void removeUpdate(DocumentEvent e) {
				check();
			}
			@Override
			public void insertUpdate(DocumentEvent e) {
				check();
			}
			@Override
			public void changedUpdate(DocumentEvent e) {
//				check();
			}
			private void check() {
				List<String> driverURLs = retrieveDriverURLs(driverlist);
				Set<String> files = driverURLs == null? new HashSet<String>() : driverURLs.stream().map(url -> {
					try {
						return toRelFileName(new File(Environment.newFile(HttpDownload.DOWNLOADFOLDER), HttpDownload.toFileName(new URL(url))).getAbsolutePath());
					} catch (Exception e) {
						return toRelFileName(url.trim());
					}
				}).collect(Collectors.toSet());
				Set<String> jarsContent = Arrays.asList(jar1, jar2, jar3, jar4).stream()
						.map(f -> f.getText().trim())
						.filter(v -> !v.isEmpty())
						.filter(v -> !v.contains(HttpDownload.DOWNLOADFOLDER) || new File(v).exists())
						.collect(Collectors.toSet());
				downloadButton.setEnabled(driverURLs != null
						&& !jarsContent.containsAll(files));
				downloadButton.setToolTipText(driverURLs == null? null :
					driverURLs.stream().collect(Collectors.joining("<br>", "<html>", "</html>"))
					);
			}
		}));
		if (needsTest) {
			testConnectionButton.setVisible(false);
		} else {
			selectConnectionButton.setVisible(false);
		}
		InfoBar theInfoBar = null;
		if (infoBar != null) {
			UIUtil.replace(infoBarLabel, theInfoBar = infoBar);
		} else if (forNew) {
			UIUtil.replace(infoBarLabel, theInfoBar = new InfoBar("New Connection",
					"Enter connection credentials for the database.\n" +
					"Replace placeholders (\"<...>\") with appropriate URL parameters.", null));
		} else if (forClone) {
			UIUtil.replace(infoBarLabel, theInfoBar = new InfoBar("Clone Connection",
					"Edit cloned connection credentials for the database.", null));
		} else {
			UIUtil.replace(infoBarLabel, theInfoBar = new InfoBar("Edit Connection",
					"Edit connection credentials for the database.", null));
		}
		if (theInfoBar != null) {
			theInfoBar.shrink();
		}
		loadButton1.addActionListener(new java.awt.event.ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				String fn = UIUtil.choseFile(null, Environment.isJPacked()? Environment.newFile("lib").getAbsolutePath() : Environment.newFile(".").getAbsolutePath(), "Jdbc Driver", ".jar", DbConnectionDetailsEditor.this, true, true, false);
				if (fn != null) {
					jar1.setText(fn);
				}
			}
		});
		loadButton2.addActionListener(new java.awt.event.ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				String fn = UIUtil.choseFile(null, Environment.isJPacked()? Environment.newFile("lib").getAbsolutePath() : Environment.newFile(".").getAbsolutePath(), "Jdbc Driver", ".jar", DbConnectionDetailsEditor.this, true, true, false);
				if (fn != null) {
					jar2.setText(fn);
				}
			}
		});
		loadButton3.addActionListener(new java.awt.event.ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				String fn = UIUtil.choseFile(null, Environment.isJPacked()? Environment.newFile("lib").getAbsolutePath() : Environment.newFile(".").getAbsolutePath(), "Jdbc Driver", ".jar", DbConnectionDetailsEditor.this, true, true, false);
				if (fn != null) {
					jar3.setText(fn);
				}
			}
		});
		loadButton4.addActionListener(new java.awt.event.ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				String fn = UIUtil.choseFile(null, Environment.isJPacked()? Environment.newFile("lib").getAbsolutePath() : Environment.newFile(".").getAbsolutePath(), "Jdbc Driver", ".jar", DbConnectionDetailsEditor.this, true, true, false);
				if (fn != null) {
					jar4.setText(fn);
				}
			}
		});
		helpjdbc.addMouseListener(new java.awt.event.MouseAdapter() {
			@Override
			public void mouseEntered(java.awt.event.MouseEvent evt) {
				helpjdbc.setEnabled(false);
			}
			@Override
			public void mouseExited(java.awt.event.MouseEvent evt) {
				helpjdbc.setEnabled(true);
		   }
		});
		helpjdbc.addMouseListener(new java.awt.event.MouseAdapter() {
			@Override
			public void mouseClicked(java.awt.event.MouseEvent evt) {
				try {
					BrowserLauncher.openURL(new URI(jdbcHelpURL), DbConnectionDetailsEditor.this);
				} catch (Exception e) {
					UIUtil.showException(DbConnectionDetailsEditor.this, "Error", e);
				}
			}
		});
		helpjdbc.setIcon(helpIcon);
		helpjdbc.setText(null);
		loadButton1.setText(null);
		loadButton2.setText(null);
		loadButton3.setText(null);
		loadButton4.setText(null);
		loadButton1.setIcon(loadIcon);
		loadButton2.setIcon(loadIcon);
		loadButton3.setIcon(loadIcon);
		loadButton4.setIcon(loadIcon);

		UIUtil.setTrailingComponent(jar1, loadButton1);
		UIUtil.setTrailingComponent(jar2, loadButton2);
		UIUtil.setTrailingComponent(jar3, loadButton3);
		UIUtil.setTrailingComponent(jar4, loadButton4);

		editButton.setIcon(UIUtil.scaleIcon(editButton, editorIcon));
		
		SmallButton clear = new LightBorderSmallButton(UIUtil.scaleIcon(editButton,clearIcon)) {
			@Override
			protected void onClick(MouseEvent e) {
				if (lastKnownUrlPattern != null) {
					dbUrl.setText(lastKnownUrlPattern);
					UIUtil.invokeLater(4, () -> editButton.doClick());
				}
			}
		};
		clear.setToolTipText("Reset");
		DocumentListener cDl = new DocumentListener() {
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
				final String text = dbUrl.getText().trim();
				if (lastKnownUrlPattern == null) {
					lastKnownUrlPattern = UIUtil.getDBMSURLPattern(text);
				}
				clear.setEnabled(lastKnownUrlPattern != null && !lastKnownUrlPattern.equals(text));
			}
		};
		
		dbUrl.getDocument().addDocumentListener(cDl);
		cDl.changedUpdate(null);
		
		clear.setEnabled(false);
		
		GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 1;
		gridBagConstraints.gridy = 0;
		gridBagConstraints.fill = GridBagConstraints.BOTH;
		jPanel9.add(clear, gridBagConstraints);

		addWindowListener(new WindowAdapter() {
			@Override
			public void windowOpened(WindowEvent evt) {
				user.grabFocus();
				UIUtil.invokeLater(4, () -> {
					List<String> driverURLs = retrieveDriverURLs(driverlist);
					if (forNew && driverURLs != null && downloadButton.isEnabled() && driverURLs.stream().allMatch(
							url -> {
								try {
									return new File(Environment.newFile(HttpDownload.DOWNLOADFOLDER), HttpDownload.toFileName(new URL(url))).exists();
								} catch (Exception e) {
									return false;
								}
							})) {
						UIUtil.invokeLater(() -> {
							List<String> files = driverURLs.stream().map(url -> {
								try {
									return new File(Environment.newFile(HttpDownload.DOWNLOADFOLDER), HttpDownload.toFileName(new URL(url))).getAbsolutePath();
								} catch (Exception e) {
									return "";
								}
							}).collect(Collectors.toList());
							if (files.size() > 0) {
								jar1.setText(toRelFileName(files.get(0)));
							}
							if (files.size() > 1) {
								jar2.setText(toRelFileName(files.get(1)));
							}
							if (files.size() > 2) {
								jar3.setText(toRelFileName(files.get(2)));
							}
							if (files.size() > 3) {
								jar4.setText(toRelFileName(files.get(3)));
							}
						});
					}
				});
			}
			@Override
			public void windowClosed(WindowEvent e) {
				feedbackTimer.stop();
			}
		});
		
		if (forNew) {
			UIUtil.invokeLater(2, () -> {
				if (editButton.isEnabled()) {
					editButton.doClick();
				}
			});
		}
		
		pack();
		setSize(Math.max(570, getWidth()), getHeight() + 32);
		if (parent != null) {
			setLocation(parent.getX() + (parent.getWidth() - getWidth()) / 2, Math.max(0, parent.getY() + (parent.getHeight() - getHeight()) / 2));			
		} else {
			setLocation(120, 170);
		}
	}
	
	private String lastKnownUrlPattern;

	private boolean firePending;

	protected List<String> retrieveDriverURLs(List<Line> driverlist) {
		if (driverlist != null) {
			String url = dbUrl.getText().trim().replaceAll("^(\\w+:\\w+:).*", "$1");
			Line line = driverlist.stream().filter(l -> l.cells.get(1).startsWith(url)).findFirst().orElse(null);
			if (line != null) {
				if (!line.cells.get(4).isEmpty()) {
					return new ArrayList<String>(Arrays.asList(line.cells.get(4).split("\\s+")));
				}
				if (!line.cells.get(3).isEmpty()) {
					return Arrays.stream(line.cells.get(3).split("\\s+")).map(fileName -> " " + fileName).collect(Collectors.toList());
				}
			}
		}
		return null;
	}

	/** This method is called from within the constructor to
	 * initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is
	 * always regenerated by the Form Editor.
	 */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        helpjdbc = new javax.swing.JLabel();
        jPanel1 = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();
        jLabel6 = new javax.swing.JLabel();
        jLabel8 = new javax.swing.JLabel();
        jLabel9 = new javax.swing.JLabel();
        jar1 = new javax.swing.JTextField();
        jar2 = new javax.swing.JTextField();
        driverClass = new javax.swing.JTextField();
        user = new javax.swing.JTextField();
        jPanel2 = new javax.swing.JPanel();
        okButton = new javax.swing.JButton();
        cancelButton = new javax.swing.JButton();
        testConnectionButton = new javax.swing.JButton();
        selectConnectionButton = new javax.swing.JButton();
        jPanel8 = new javax.swing.JPanel();
        jLabel11 = new javax.swing.JLabel();
        password = new javax.swing.JPasswordField();
        loadButton1 = new javax.swing.JButton();
        loadButton2 = new javax.swing.JButton();
        jar3 = new javax.swing.JTextField();
        jar4 = new javax.swing.JTextField();
        jLabel10 = new javax.swing.JLabel();
        jLabel12 = new javax.swing.JLabel();
        loadButton3 = new javax.swing.JButton();
        loadButton4 = new javax.swing.JButton();
        jPanel4 = new javax.swing.JPanel();
        infoBarLabel = new javax.swing.JLabel();
        jPanel3 = new javax.swing.JPanel();
        jPanel5 = new javax.swing.JPanel();
        exportCBButton = new javax.swing.JButton();
        importCBButton = new javax.swing.JButton();
        feedbackLabel = new javax.swing.JLabel();
        downloadButton = new javax.swing.JButton();
        jSeparator2 = new javax.swing.JSeparator();
        alias = new javax.swing.JTextField();
        renameButton = new javax.swing.JButton();
        jtdsWarnLabel = new javax.swing.JLabel();
        logoLabel = new javax.swing.JLabel();
        jPanel7 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        jPanel6 = new javax.swing.JPanel();
        dbUrl = new javax.swing.JTextPane();
        editButton = new javax.swing.JToggleButton();
        jPanel9 = new javax.swing.JPanel();
        jLabel7 = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        jPanel10 = new javax.swing.JPanel();
        dataModelComboBox = new javax.swing.JComboBox<>();
        newDataModelButton = new javax.swing.JButton();
        jPanel11 = new javax.swing.JPanel();
        jLabel13 = new javax.swing.JLabel();
        typeComboBox = new javax.swing.JComboBox<>();

        helpjdbc.setText("help");

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Database Connection");
        getContentPane().setLayout(new java.awt.GridBagLayout());

        jPanel1.setLayout(new java.awt.GridBagLayout());

        jLabel1.setText(" Name ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel1.add(jLabel1, gridBagConstraints);

        jLabel2.setText(" JDBC Driver JAR ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 4);
        jPanel1.add(jLabel2, gridBagConstraints);

        jLabel3.setText(" 3. additional JAR ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 42;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 4);
        jPanel1.add(jLabel3, gridBagConstraints);

        jLabel4.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridy = 15;
        jPanel1.add(jLabel4, gridBagConstraints);

        jLabel6.setText(" Driver-Class");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 90;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(16, 0, 0, 0);
        jPanel1.add(jLabel6, gridBagConstraints);

        jLabel8.setText(" User");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 70;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel1.add(jLabel8, gridBagConstraints);

        jLabel9.setText(" Password");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 80;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel1.add(jLabel9, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel1.add(jar1, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 40;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel1.add(jar2, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 90;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(16, 0, 0, 0);
        jPanel1.add(driverClass, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 70;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel1.add(user, gridBagConstraints);

        jPanel2.setLayout(new java.awt.GridBagLayout());

        okButton.setText("Ok");
        okButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                okButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHEAST;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 0, 4);
        jPanel2.add(okButton, gridBagConstraints);

        cancelButton.setText("Cancel");
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHEAST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        jPanel2.add(cancelButton, gridBagConstraints);

        testConnectionButton.setText(" Test Connection ");
        testConnectionButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                testConnectionButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHEAST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 0, 12);
        jPanel2.add(testConnectionButton, gridBagConstraints);

        selectConnectionButton.setText("Select Connection ");
        selectConnectionButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                selectConnectionButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHEAST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 0, 12);
        jPanel2.add(selectConnectionButton, gridBagConstraints);

        jPanel8.setLayout(null);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.weighty = 1.0;
        jPanel2.add(jPanel8, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 100;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        jPanel1.add(jPanel2, gridBagConstraints);

        jLabel11.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 10;
        jPanel1.add(jLabel11, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 80;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel1.add(password, gridBagConstraints);

        loadButton1.setText(" Browse..");
        loadButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                loadButton1ActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel1.add(loadButton1, gridBagConstraints);

        loadButton2.setText(" Browse..");
        loadButton2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                loadButton2ActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 40;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel1.add(loadButton2, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 41;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel1.add(jar3, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 42;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel1.add(jar4, gridBagConstraints);

        jLabel10.setText(" 1. additional JAR ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 40;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 4);
        jPanel1.add(jLabel10, gridBagConstraints);

        jLabel12.setText(" 2. additional JAR ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 41;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 4);
        jPanel1.add(jLabel12, gridBagConstraints);

        loadButton3.setText(" Browse..");
        loadButton3.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                loadButton3ActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 41;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel1.add(loadButton3, gridBagConstraints);

        loadButton4.setText(" Browse..");
        loadButton4.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                loadButton4ActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 42;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel1.add(loadButton4, gridBagConstraints);

        jPanel4.setLayout(new java.awt.GridBagLayout());

        infoBarLabel.setText("info bar");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 12);
        jPanel4.add(infoBarLabel, gridBagConstraints);

        jPanel3.setLayout(new java.awt.GridBagLayout());

        jPanel5.setLayout(new java.awt.GridBagLayout());

        exportCBButton.setText("Copy Credentials");
        exportCBButton.setToolTipText("Copy credentials to clipboard");
        exportCBButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                exportCBButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 7;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel5.add(exportCBButton, gridBagConstraints);

        importCBButton.setText("Paste Credentials");
        importCBButton.setToolTipText("Paste credentials from clipboard");
        importCBButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                importCBButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel5.add(importCBButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        jPanel3.add(jPanel5, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHEAST;
        gridBagConstraints.insets = new java.awt.Insets(6, 0, 0, 4);
        jPanel4.add(jPanel3, gridBagConstraints);

        feedbackLabel.setFont(feedbackLabel.getFont().deriveFont((feedbackLabel.getFont().getStyle() | java.awt.Font.ITALIC)));
        feedbackLabel.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 8);
        jPanel4.add(feedbackLabel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 11;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 16, 0);
        jPanel1.add(jPanel4, gridBagConstraints);

        downloadButton.setText("Download Driver");
        downloadButton.setEnabled(false);
        downloadButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                downloadButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 44;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 16, 0);
        jPanel1.add(downloadButton, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 46;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(8, 0, 8, 0);
        jPanel1.add(jSeparator2, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel1.add(alias, gridBagConstraints);

        renameButton.setText("Rename");
        renameButton.setToolTipText("Reset Name");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel1.add(renameButton, gridBagConstraints);

        jtdsWarnLabel.setForeground(java.awt.Color.red);
        jtdsWarnLabel.setText("<html>Due to known problems with the jTDS JDBC driver, it is strongly recommended to use the original driver for SQL Server or Sybase.<html>");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 94;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(jtdsWarnLabel, gridBagConstraints);

        logoLabel.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 44;
        gridBagConstraints.gridheight = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 0);
        jPanel1.add(logoLabel, gridBagConstraints);

        jPanel7.setLayout(new java.awt.GridBagLayout());

        jScrollPane1.setHorizontalScrollBarPolicy(javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        jScrollPane1.setVerticalScrollBarPolicy(javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER);

        jPanel6.setLayout(new java.awt.GridBagLayout());

        dbUrl.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel6.add(dbUrl, gridBagConstraints);

        jScrollPane1.setViewportView(jPanel6);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 60;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        jPanel7.add(jScrollPane1, gridBagConstraints);

        editButton.setText("Edit");
        editButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                editButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 60;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        jPanel7.add(editButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 60;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(jPanel7, gridBagConstraints);

        jPanel9.setLayout(new java.awt.GridBagLayout());

        jLabel7.setText(" DB-URL");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel9.add(jLabel7, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 60;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel1.add(jPanel9, gridBagConstraints);

        jLabel5.setText(" Data Model");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 11;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel1.add(jLabel5, gridBagConstraints);

        jPanel10.setLayout(new java.awt.GridBagLayout());

        dataModelComboBox.setModel(new javax.swing.DefaultComboBoxModel<>(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        dataModelComboBox.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                dataModelComboBoxItemStateChanged(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel10.add(dataModelComboBox, gridBagConstraints);

        newDataModelButton.setText("Create New Data Model");
        newDataModelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                newDataModelButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        jPanel10.add(newDataModelButton, gridBagConstraints);

        jPanel11.setLayout(null);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.weightx = 1.0;
        jPanel10.add(jPanel11, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 11;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel1.add(jPanel10, gridBagConstraints);

        jLabel13.setText(" Type");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 81;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel1.add(jLabel13, gridBagConstraints);

        typeComboBox.setModel(new javax.swing.DefaultComboBoxModel<>(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 81;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel1.add(typeComboBox, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 0);
        getContentPane().add(jPanel1, gridBagConstraints);

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void okButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_okButtonActionPerformed
    	ConnectionInfo oldCi = new ConnectionInfo();
		oldCi.assign(ci);
		boolean ok = fillConnectionInfo();
		if (!dataModelAware && "".equals(dataModelComboBox.getSelectedItem())) {
			JOptionPane.showMessageDialog(isVisible()? this : parent, "Please specify the Data Model or create a new one.", "No Data Model", JOptionPane.ERROR_MESSAGE);
			dataModelComboBox.grabFocus();
			ci.assign(oldCi);
			ok = false;
		}
		if (ok) {
    		if (needsTest) {
    			if (!DbConnectionDialog.testConnection(isVisible()? this : parent, ci, createDownloadButton())) {
    				return;
    			}
    		}
		   isOk = true;
		   onClose(isOk, ci);
		}
	}//GEN-LAST:event_okButtonActionPerformed

	protected void onClose(boolean ok, ConnectionInfo info) {
		setVisible(false);
	}

    @SuppressWarnings("deprecation")
	private boolean fillConnectionInfo() {
		boolean ok = true;
		Color red = new Color(255, 200, 180);
		if (alias.getText().trim().length() == 0) {
			alias.setBackground(red);
			ok = false;
		}
		if (driverClass.getText().trim().length() == 0) {
			driverClass.setBackground(red);
			ok = false;
		}
		if (dbUrl.getText().trim().length() == 0) {
			dbUrl.setBackground(red);
			ok = false;
		}
		if (ok) {
			ci.alias = alias.getText().trim();
			ci.driverClass = driverClass.getText().trim();
			ci.jar1 = jar1.getText().trim();
			ci.jar2 = jar2.getText().trim();
			ci.jar3 = jar3.getText().trim();
			ci.jar4 = jar4.getText().trim();
			ci.url = dbUrl.getText().trim();
			ci.user = user.getText().trim();
			ci.password = password.getText().trim();
	       	ci.dataModelFolder = (String) dataModelComboBox.getSelectedItem();
	       	ci.setConnectionType((ConnectionType) typeComboBox.getSelectedItem());
		}
		return ok;
	}

	private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
		onClose(false, ci);
	}//GEN-LAST:event_cancelButtonActionPerformed

	private void testConnectionButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_testConnectionButtonActionPerformed
		ConnectionInfo oldCi = new ConnectionInfo();
		oldCi.assign(ci);
		if (fillConnectionInfo()) {
			if (DbConnectionDialog.testConnection(isVisible()? this : parent, ci, createDownloadButton())) {
				JOptionPane.showMessageDialog(isVisible()? this : parent, "Successfully established connection.", "Connected", JOptionPane.INFORMATION_MESSAGE);
			}
			ci.assign(oldCi);
		}
	}//GEN-LAST:event_testConnectionButtonActionPerformed

	private JButton createDownloadButton() {
		JButton button = null;
		if (downloadButton.isEnabled()) {
			JButton finalButton = new JButton("Download Driver");
			button = finalButton;
			button.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					Window window = SwingUtilities.getWindowAncestor(finalButton);
					if (window != null) {
						window.setVisible(false);
						window.dispose();
					}
					downloadButton.doClick();
				}
			});
		}
		return button;
	}

	private void loadButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_loadButton1ActionPerformed
	}//GEN-LAST:event_loadButton1ActionPerformed

	private void loadButton2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_loadButton2ActionPerformed
	}//GEN-LAST:event_loadButton2ActionPerformed

    private void loadButton3ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_loadButton3ActionPerformed
    }//GEN-LAST:event_loadButton3ActionPerformed

    private void loadButton4ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_loadButton4ActionPerformed
    }//GEN-LAST:event_loadButton4ActionPerformed

    private void selectConnectionButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_selectConnectionButtonActionPerformed
        onSelect();
    }//GEN-LAST:event_selectConnectionButtonActionPerformed

    private final static String[] SEPARATORS = new String[] { "|", ";", ",", "#", "*", "~", "!", "-" };
	
    private void exportCBButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_exportCBButtonActionPerformed
    	try {
        	String content;
        	String rawContent = Arrays.stream(textFields).map(field -> field.getText()).collect(Collectors.joining());
        	String separator = Arrays.stream(SEPARATORS).filter(sep -> !rawContent.contains(sep)).findAny().orElseGet(() -> null);
        	if (separator != null) {
        		content = separator + Arrays.stream(textFields).map(field -> field.getText()).collect(Collectors.joining(separator)) + separator;
        		content += typeComboBox.getSelectedItem() + separator;
        		StringSelection contents = new StringSelection(content);
        		UIUtil.setClipboardContent(contents);
        		showFeedback("credentials copied to clipboard");
        		return;
        	}
    	} catch (Exception e) {
    		e.printStackTrace();
    	}
		Toolkit.getDefaultToolkit().beep();
    }//GEN-LAST:event_exportCBButtonActionPerformed

    private void importCBButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_importCBButtonActionPerformed
    	String content = null;
    	try {
    		Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
    		Transferable contents = clipboard.getContents(this);
    		if (contents != null) {
    			content = contents.getTransferData(DataFlavor.stringFlavor).toString().trim();
    			if (content.length() > 1
    					&& content.substring(0, 1).equals(content.substring(content.length() - 1))
    					&& Arrays.asList(SEPARATORS).contains(content.substring(0, 1))) {
    				nameContent = "@";
    				String separator = content.substring(0, 1);
    				content = content.substring(1);
    				String[] fields = content.split(Pattern.quote(separator));
    				int i;
    				for (i = 0; i < textFields.length && i < fields.length; ++i) {
    					textFields[i].setText(fields[i]);
    				}
    				if (i >= 0 && i < fields.length) {
    					String tField = fields[i].trim();
						Arrays.stream(ConnectionType.values())
							.filter(t -> t.displayName.equals(tField)).findAny()
							.ifPresent(t -> typeComboBox.setSelectedItem(t));
					}
    				String newName = createNewName(user.getText(), dbUrl.getText());
    				if (newName.replaceFirst("\\s*\\(\\d+\\)$", "").equals(alias.getText().replaceFirst("\\s*\\(\\d+\\)$", ""))) {
    					nameContent = alias.getText();
    				}
    				showFeedback("credentials taken from clipboard");
            		return;
    			}
    		}
    	} catch (Exception e) {
    		// ignore
    	}
    	Toolkit.getDefaultToolkit().beep();
    }//GEN-LAST:event_importCBButtonActionPerformed

	private void downloadButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_downloadButtonActionPerformed
    	List<String> driverURLs = retrieveDriverURLs(driverlist);
		if (driverURLs != null) {
			final List<String> files;
			final Object LOCK = new Object();
			synchronized (LOCK) {
				files = new ArrayList<String>();
			}
			
			AtomicBoolean ok = new AtomicBoolean(true);
			Function<Long, String> updateInfo = total -> {
				String text = "<html><b>Downloading... " + (total > 0? "(" + total / 1024 + "K)" : "(1.1%)") + "</b><br>"
						+ driverURLs.stream().collect(Collectors.joining("<br>")) + "</html>";
				return text;
			};
			
			@SuppressWarnings("serial")
			final ConcurrentTaskControl concurrentTaskControl = new ConcurrentTaskControl(
					this, updateInfo.apply(0L)) {

				@Override
				protected void onError(Throwable error) {
					UIUtil.showException(this, "Error", error);
					ok.set(false);
					closeWindow();
				}

				@Override
				protected void onCancellation() {
					ok.set(false);
					closeWindow();
				}
			};
			
			ConcurrentTaskControl.openInModalDialog(this, concurrentTaskControl, 
					new ConcurrentTaskControl.Task() {
				@Override
				public void run() throws Throwable {
					long[] total = { 0 };
					driverURLs.forEach(url -> {
						try {
							if (url.startsWith(" ")) {
								synchronized (LOCK) {
									files.add(url.substring(1));
								}
							} else {
								String result = HttpDownload.get(url, vol -> {
									String text = updateInfo.apply(total[0] += vol);
									UIUtil.invokeLater(() -> {
										concurrentTaskControl.master.infoLabel.setText(text);
									});
								});
								if (result.length() == 0) {
									throw new RuntimeException("cannot download \"" + url + "\"");
								}
								synchronized (LOCK) {
									files.add(result.toString());
								}
							}
						} catch (Throwable t) {
							throw new RuntimeException("cannot download \"" + url + "\"", t);
						}
					});

					UIUtil.invokeLater(new Runnable() {
						@Override
						public void run() {
							if (ok.get()) {
								synchronized (LOCK) {
									jar1.setText("");
									jar2.setText("");
									jar3.setText("");
									jar4.setText("");
									if (files.size() > 0) {
										jar1.setText(toRelFileName(files.get(0)));
									}
									if (files.size() > 1) {
										jar2.setText(toRelFileName(files.get(1)));
									}
									if (files.size() > 2) {
										jar3.setText(toRelFileName(files.get(2)));
									}
									if (files.size() > 3) {
										jar4.setText(toRelFileName(files.get(3)));
									}
								}
							}
							concurrentTaskControl.closeWindow();
						}
					});
				}
			}, "Downloading Driver");
		}
    }//GEN-LAST:event_downloadButtonActionPerformed

    private void editButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_editButtonActionPerformed
    	DbConnectionSettings settingsDialog = createSettingsDialog(true);
		if (settingsDialog == null) {
			return;
		}
		
		JDialog dialog = new JDialog(this);
		dialog.setModal(false);
		dialog.setUndecorated(true);
		dialog.addWindowFocusListener(new WindowFocusListener() {
			@Override
			public void windowLostFocus(WindowEvent e) {
				editButton.setSelected(false);
				dialog.setVisible(false);
				dialog.dispose();
			}
			@Override
			public void windowGainedFocus(WindowEvent e) {
			}
		});
		
		JPanel content = new JPanel(new GridBagLayout());
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.insets = new Insets(4, 4, 4, 4);
		gbc.fill = GridBagConstraints.BOTH;
		gbc.weightx = 1;
		content.add(settingsDialog.getDetailsPanel(), gbc);
		content.setBorder(BorderFactory.createLineBorder(Color.GRAY));
		
		dialog.getContentPane().add(content);
		
		Component comp = editButton;
		Point location = new Point(0, comp.getHeight());
		SwingUtilities.convertPointToScreen(location, comp);
		
		dialog.pack();
		int minWidth = 420;
		int wid = Math.max(minWidth, dialog.getWidth());
		dialog.setSize(wid, dialog.getHeight());
		int x = location.x + editButton.getWidth() - minWidth;
		int y = location.y;
		dialog.setLocation(x, y);
		
		editButton.setSelected(true);
		UIUtil.invokeLater(() -> dialog.requestFocus());
		dialog.setVisible(true);
    }//GEN-LAST:event_editButtonActionPerformed

    private void dataModelComboBoxItemStateChanged(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_dataModelComboBoxItemStateChanged
    }//GEN-LAST:event_dataModelComboBoxItemStateChanged

    private void newDataModelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_newDataModelButtonActionPerformed
    	String cmsf = DataModelManager.getCurrentModelSubfolder(executionContext);
		try {
			DataModelManager.setCurrentModelSubfolder(null, executionContext);

			NewDataModelDialog newDataModelDialog = new NewDataModelDialog(this, modelList);
	
			String newName = newDataModelDialog.getNameEntered();
			if (newName != null) {
				DataModelManager.createNewModel(newName, newDataModelDialog.getFolderName(), executionContext);
				loadModelList(executionContext);
				UIUtil.invokeLater(() -> {
					dataModelComboBox.setSelectedItem(newDataModelDialog.getFolderName());
					firePending = true;
				});
			}
		} catch (Exception e) {
			UIUtil.showException(this, "Error", e, UIUtil.EXCEPTION_CONTEXT_USER_ERROR);
		} finally {
			DataModelManager.setCurrentModelSubfolder(cmsf, executionContext);
		}
    }//GEN-LAST:event_newDataModelButtonActionPerformed

    private boolean acceptParameter(String name) {
    	String urlPattern = UIUtil.getDBMSURLPattern(dbUrl.getText().trim());
		if (urlPattern == null) {
			return true;
		}
		return urlPattern.matches(".*\\<" + Pattern.quote(name) + "(\\([^\\)]+\\))?" + "\\>.*");
	}
    
    private DbConnectionSettings createSettingsDialog(boolean active) {
		DbConnectionSettings settingsDialog = new DbConnectionSettings(this) {
			@Override
			protected boolean acceptParameter(String name) {
				return DbConnectionDetailsEditor.this.acceptParameter(name);
			}
			@Override
			protected void consumeURL(String url) {
				if (active) {
					dbUrl.setText(url);
					updateDbURLStyle();
				}
			}
			@Override
			protected JDialog getFileDialogParent() {
				return DbConnectionDetailsEditor.this;
			}
		};
		if (settingsDialog.updateFields(dbUrl.getText())) {
			return settingsDialog;
		} else {
			return null;
		}
	}

	private String toRelFileName(final String fileName) {
		try {
            File f = new File(fileName);
            String work = Environment.newFile(".").getCanonicalPath();
            if (f.getCanonicalPath().startsWith(work)) {
                String fn = f.getName();
                f = f.getParentFile();
                while (f != null && !f.getCanonicalPath().equals(work)) {
                    fn = f.getName() + File.separator + fn;
                    f = f.getParentFile();
                }
                return fn;
            } else {
                return fileName;
            }
		} catch (Throwable t) {
			return fileName;
		}
	}

	protected void onSelect() {
	}

    // Variables declaration - do not modify//GEN-BEGIN:variables
    public javax.swing.JTextField alias;
    private javax.swing.JButton cancelButton;
    private javax.swing.JComboBox<String> dataModelComboBox;
    private javax.swing.JTextPane dbUrl;
    private javax.swing.JButton downloadButton;
    public javax.swing.JTextField driverClass;
    private javax.swing.JToggleButton editButton;
    private javax.swing.JButton exportCBButton;
    private javax.swing.JLabel feedbackLabel;
    private javax.swing.JLabel helpjdbc;
    private javax.swing.JButton importCBButton;
    private javax.swing.JLabel infoBarLabel;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel10;
    private javax.swing.JLabel jLabel11;
    private javax.swing.JLabel jLabel12;
    private javax.swing.JLabel jLabel13;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel10;
    private javax.swing.JPanel jPanel11;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JPanel jPanel7;
    private javax.swing.JPanel jPanel8;
    private javax.swing.JPanel jPanel9;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JSeparator jSeparator2;
    private javax.swing.JTextField jar1;
    private javax.swing.JTextField jar2;
    private javax.swing.JTextField jar3;
    private javax.swing.JTextField jar4;
    private javax.swing.JLabel jtdsWarnLabel;
    private javax.swing.JButton loadButton1;
    private javax.swing.JButton loadButton2;
    private javax.swing.JButton loadButton3;
    private javax.swing.JButton loadButton4;
    private javax.swing.JLabel logoLabel;
    private javax.swing.JButton newDataModelButton;
    private javax.swing.JButton okButton;
    javax.swing.JPasswordField password;
    private javax.swing.JButton renameButton;
    private javax.swing.JButton selectConnectionButton;
    private javax.swing.JButton testConnectionButton;
    private javax.swing.JComboBox<String> typeComboBox;
    public javax.swing.JTextField user;
    // End of variables declaration//GEN-END:variables

    private final Timer feedbackTimer;
    
    private void showFeedback(String message) {
		feedbackLabel.setText(message);
		feedbackTimer.restart();
	}

    private String createNewName(String usr, String url) {
    	final int MAX_LENGTH = 64;
    	String simplifiedUrl = url.replaceAll("[\\[\\]<>]", "");
		Optional<String> descOpt = pattern.stream().map(p -> {
			Matcher m = p.matcher(simplifiedUrl);
			String d = null;
			if (m.matches() && m.groupCount() > 0) {
				d = m.group(1);
				if (d.length() > MAX_LENGTH) {
					d = d.substring(0, MAX_LENGTH);
				}
			}
			return d;
		}).filter(d -> d != null).findFirst();
		String desc = descOpt.isPresent()? descOpt.get().trim() : "";
		String newName;
		if (usr.isEmpty() && desc.isEmpty()) {
			newName = "Connection";
		} else if (usr.isEmpty()) {
			newName = desc;
		} else if (desc.isEmpty()) {
			newName = usr;
		} else {
			newName = usr + "@" + desc;
		}
		for (int nr = 1; ; ++nr) {
			String newAlias = newName + (nr > 1? " (" + nr + ")" : "");
			boolean found = false;
			for (ConnectionInfo ci: connectionList) {
				if (ci != this.ci && ci.alias.equals(newAlias)) {
					found = true;
					break;
				}
			}
			if (!found) {
				return newAlias;
			}
		}
	}

	private JTextComponent[] textFields;
	
	private static List<Runnable> listener = new ArrayList<Runnable>();
	
	public static void addNewDatamodelListener(Runnable onChange) {
		listener.add(onChange);
	}

	private Icon helpIcon;
	private Icon loadIcon;
    private Icon resetIcon;
    private ImageIcon warnIcon;
    private ImageIcon editorIcon;
    {
		// load images
		helpIcon = UIUtil.readImage("/help.png");
		loadIcon = UIUtil.readImage("/load2.png");
		resetIcon = UIUtil.readImage("/reset.png");
		warnIcon = UIUtil.readImage("/wanr.png");
		editorIcon = UIUtil.readImage("/edit.png");
    }

	private static ImageIcon okIcon;
	private static ImageIcon cancelIcon;
	private static ImageIcon clearIcon;
	
    static {
        // load images
    	okIcon = UIUtil.readImage("/buttonok.png");
        cancelIcon = UIUtil.readImage("/buttoncancel.png");
        clearIcon = UIUtil.readImage("/reset.png");
	}

	private static final long serialVersionUID = -492511696901313920L;

}
