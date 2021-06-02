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
package net.sf.jailer.ui;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridLayout;
import java.awt.Point;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowFocusListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;

import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListCellRenderer;
import javax.swing.DefaultListModel;
import javax.swing.ImageIcon;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.SwingUtilities;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.fife.rsta.ui.EscapableDialog;

import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.databrowser.metadata.MDSchema;
import net.sf.jailer.ui.databrowser.metadata.MetaDataSource;

/**
 * Substring search for combo boxes.
 * 
 * @author Ralf Wisser
 */
public class StringSearchPanel extends javax.swing.JPanel {

	private JDialog dialog;
	private String result;

	public interface Prepare {
		void prepare(Set<MDSchema> selectedSchemas);
	}

	public interface AdditionalComponentFactory {
		JComponent create(StringSearchPanel searchPanel);
	}

	public static JToggleButton createSearchButton(final Window owner, final javax.swing.JComboBox comboBox, final Object titel, final Runnable onSuccess) {
		return createSearchButton(owner, comboBox, titel, onSuccess, false);
	}

	public static JToggleButton createSearchButton(final Window owner, final javax.swing.JComboBox comboBox, final Object titel, final Runnable onSuccess, boolean alternativeIcon) {
		return createSearchButton(owner, comboBox, titel, onSuccess, null, alternativeIcon);
	}
	
	public static JToggleButton createSearchButton(final Window owner, final javax.swing.JComboBox comboBox, final Object titel, final Runnable onSuccess, final Prepare prepare, boolean alternativeIcon) {
		return createSearchButton(owner, comboBox, titel, onSuccess, null, null, null, alternativeIcon, null, true);
	}
	
	public static JToggleButton createSearchButton(final Window owner, final javax.swing.JComboBox comboBox, final Object titel, final Runnable onSuccess, final Prepare prepare, final MetaDataSource metaDataSource, final DataModel dataModel) {
		return createSearchButton(owner, comboBox, titel, onSuccess, prepare, metaDataSource, dataModel, false, null, true);
	}
	
	public static JToggleButton createSearchButton(final Window owner, final javax.swing.JComboBox comboBox, final Object titel, final Runnable onSuccess, final Prepare prepare, final MetaDataSource metaDataSource, final DataModel dataModel, boolean alternativeIcon, final AdditionalComponentFactory additionalComponentFactory, final boolean locateUnderButton) {
		return createSearchButton(owner, comboBox, titel, onSuccess, prepare, metaDataSource, dataModel, alternativeIcon, additionalComponentFactory, locateUnderButton, false, null);
	}

	public static JToggleButton createSearchButton(final Window owner, final javax.swing.JComboBox comboBox, final Object titel, final Runnable onSuccess, final Prepare prepare, final MetaDataSource metaDataSource, final DataModel dataModel, boolean alternativeIcon, final AdditionalComponentFactory additionalComponentFactory, final boolean locateUnderButton, final boolean keepSearchText, final Map<String, Consumer<JLabel>> renderConsumer) {
		final JToggleButton button = new JToggleButton();
		button.setIcon(getSearchIcon(alternativeIcon, button));
		button.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				UIUtil.setWaitCursor(button);
				UIUtil.invokeLater(4, new Runnable() {
					@Override
					public void run() {
				        try {
				        	if (button.isShowing()) {
					        	Point buttonLocation = button.getLocationOnScreen();
					        	Point location;
					        	if (locateUnderButton) {
					        		location = new Point(buttonLocation.x + button.getWidth(), buttonLocation.y);
					        	} else {
					        		location = buttonLocation;
					        	}
								StringSearchPanel searchPanel = new StringSearchPanel(button, comboBox, metaDataSource, dataModel, prepare, onSuccess, renderConsumer);
								searchPanel.keepSearchText = keepSearchText;
								if (additionalComponentFactory != null) {
									searchPanel.plugInPanel.add(additionalComponentFactory.create(searchPanel), java.awt.BorderLayout.CENTER);
									searchPanel.plugInPanel.setVisible(true);
								}
								Window ownerWindow = owner;
								if (ownerWindow == null) {
									ownerWindow = SwingUtilities.getWindowAncestor(comboBox);
								}
								searchPanel.find(ownerWindow, titel, location.x, location.y, locateUnderButton);
				        	}
				        } finally {
				        	UIUtil.resetWaitCursor(button);
				        }
					}
				});
			}
		});
		if (prepare == null) {
			updateEnabledState(button, comboBox);
			comboBox.getModel().addListDataListener(new ListDataListener() {
				@Override
				public void intervalRemoved(ListDataEvent arg0) {
					updateEnabledState(button, comboBox);
				}
				@Override
				public void intervalAdded(ListDataEvent arg0) {
					updateEnabledState(button, comboBox);
				}
				@Override
				public void contentsChanged(ListDataEvent arg0) {
					updateEnabledState(button, comboBox);
				}
			});
			comboBox.addPropertyChangeListener("model", new PropertyChangeListener() {
				@Override
				public void propertyChange(PropertyChangeEvent arg0) {
					updateEnabledState(button, comboBox);
				}
			});
		}
		return button;
	}

	private static ImageIcon sIcon = null;
	private static ImageIcon sIcon2 = null;
	
	public static ImageIcon getSearchIcon(boolean alternativeIcon, final JComponent button) {
		if (alternativeIcon) {
			if (sIcon2 == null) {
				sIcon2 = UIUtil.scaleIcon(button, icon2);
			}
			return sIcon2;
		} else {
			if (sIcon == null) {
				sIcon = UIUtil.scaleIcon(button, icon);
			}
			return sIcon;
		}
	}

	private static void updateEnabledState(JToggleButton button, JComboBox comboBox) {
		button.setEnabled(comboBox.getModel().getSize() > 1 || comboBox.getModel().getSize() == 1 && !"".equals(comboBox.getModel().getElementAt(0)));
	}

	private void consumeResult() {
		if (acceptAll || result != null && !result.equals(showAllLabel)) {
			combobox.setSelectedItem(result);
			if (onSuccess != null) {
				onSuccess.run();
			}
		}
		result = null;
		if (button != null) {
			button.setSelected(false);
		}
	}
	
	public void setOnClose(Runnable onClose) {
		this.onClose = onClose;
	}

	public void find(Window owner, Object titel, int x, int y, boolean locateUnderButton) {
		dialog = owner instanceof Dialog? new EscapableDialog((Dialog) owner, String.valueOf(titel), false) {
		} : new EscapableDialog((Frame) owner, String.valueOf(titel), false) {
		};
		dialog.setUndecorated(true);
		dialog.addWindowFocusListener(new WindowFocusListener() {
			@Override
			public void windowLostFocus(WindowEvent e) {
				if (!loadingDialogisVisible.get()) {
					dialog.dispose();
					consumeResult();
				}
			}
			@Override
			public void windowGainedFocus(WindowEvent e) {
			}
		});

		addComponentListener(new ComponentListener() {
			@Override
			public void componentShown(ComponentEvent e) {
			}
			@Override
			public void componentResized(ComponentEvent e) {
			}
			@Override
			public void componentMoved(ComponentEvent e) {
			}
			@Override
			public void componentHidden(ComponentEvent e) {
				consumeResult();
			}
		});
		
		dialog.getContentPane().add(this);
		
		boolean pv = plugInPanel.isVisible();
		plugInPanel.setVisible(false);
		
		dialog.pack();
		double mh = 440;
		double f = searchList.getModel().getSize() / 18.0;
		if (f < 1) {
			mh = Math.max(240, mh * f);
		}
		int height = Math.max(dialog.getHeight(), (int) mh);
		if (!locateUnderButton) {
			y = Math.max(1, y - height);
		}
		dialog.setLocation(x, y);
		int minWidth = metaDataSource == null? 300 : 500;
		if (pv) {
			minWidth *= 2;
		}
		Integer prefWidth = preferredWidth();
		Integer maxX = maxX();
		dialog.setSize(prefWidth != null ? prefWidth : Math.max(minWidth, dialog.getWidth()), Math.min(height, 600));
		if (maxX != null) {
			dialog.setLocation(Math.max(0, Math.min(maxX, dialog.getX())), dialog.getY());
		}
		Integer maxY = maxY(dialog.getHeight());
		if (maxY != null && maxY < dialog.getY()) {
			int deltaH = Math.min(dialog.getY() - maxY, (int) (0.30 * dialog.getHeight()));
			maxY += deltaH;
			dialog.setSize(dialog.getWidth(), dialog.getHeight() - deltaH);
			dialog.setLocation(dialog.getX(), Math.max(0, maxY));
		}
		plugInPanel.setVisible(pv);

		result = null;
		if (button != null) {
			button.setSelected(true);
		}
		if (combobox != null && keepSearchText) {
			try {
				JTextField c;
				c = (JTextField) combobox.getEditor().getEditorComponent();
				searchTextField.setText(c.getText());
			} catch (ClassCastException e) {
				// ignore
			}
		}
		dialog.addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosed(WindowEvent e) {
				if (onClose != null) {
					onClose.run();
				}
			}
		});
		plainIsValid = false;
		dialog.setVisible(true);
	}

	protected Integer preferredWidth() {
		return null;
	}

	protected Integer maxX() {
		return null;
	}

	protected Integer maxY(int height) {
		return null;
	}

	private final int MAX_LIST_LENGTH = 80;
	private boolean showAll = false;
	private String showAllLabel;
	private boolean keepSearchText = false;
	
	public void updateList() {
		updateList(true);
	}

	public void updateList(boolean filter) {
		DefaultListModel<String> matches = new DefaultListModel<String>();
		String text = searchTextField.getText();
		boolean withPrefix = !text.startsWith(" ");
		boolean withSuffix = !text.endsWith(" ");
		String searchText = text.trim().toUpperCase(Locale.ENGLISH);
		DefaultComboBoxModel<String> model = (DefaultComboBoxModel) combobox.getModel();
		int size = model.getSize();
		for (int i = 0; i < size; ++i) {
			String item = model.getElementAt(i);
			if (!item.isEmpty()) {
				if (!filter
						|| searchText.isEmpty() 
						|| withPrefix && withSuffix && item.toUpperCase(Locale.ENGLISH).contains(searchText)
						|| !withPrefix && withSuffix && item.toUpperCase(Locale.ENGLISH).startsWith(searchText)
						|| withPrefix && !withSuffix && item.toUpperCase(Locale.ENGLISH).endsWith(searchText)
						|| !withPrefix && !withSuffix && item.toUpperCase(Locale.ENGLISH).equals(searchText)
						) {
					matches.addElement(item);
					if (!showAll && matches.getSize() > MAX_LIST_LENGTH) {
						showAllLabel = "show all ...";
						matches.addElement(showAllLabel);
						break;
					}
				}
			}
		}
		searchList.setModel(matches);
		if (!matches.isEmpty()) {
			searchList.setSelectedIndex(0);
		}
	}

	private final MetaDataSource metaDataSource;
	private final DataModel dataModel;
	private final javax.swing.JComboBox combobox;
	private final Runnable onSuccess;
	private Runnable onClose;
	private final JToggleButton button;
	private final Map<String, Consumer<JLabel>> renderConsumer;
	
    /**
     * Creates new form StringSearchPanel
     * @param button 
     * @param dataModel 
     * @param metaDataSource 
     * @param prepare 
     * @param renderConsumer 
     */
    public StringSearchPanel(JToggleButton button, javax.swing.JComboBox combobox, MetaDataSource metaDataSource, DataModel dataModel, Prepare prepare, final Runnable onSuccess, Map<String, Consumer<JLabel>> renderConsumer) {
    	this.button = button;
    	this.combobox = combobox;
    	this.dataModel = dataModel;
    	this.metaDataSource = metaDataSource;
    	this.prepare = prepare;
    	this.onSuccess = onSuccess;
    	this.renderConsumer = renderConsumer;
        initComponents();
        
        plugInPanel.setVisible(false);

        if (metaDataSource != null) {
        	List<MDSchema> vis = new ArrayList<MDSchema>();
        	Set<MDSchema> visAsSet = new HashSet<MDSchema>();
	        for (Table table: dataModel.getTables()) {
	        	String schema = table.getSchema("");
	        	MDSchema mdSchema = metaDataSource.find(schema);
	        	if (mdSchema != null && !mdSchema.isDefaultSchema) {
	        		if (!visAsSet.contains(mdSchema)) {
	        			visAsSet.add(mdSchema);
	        			vis.add(mdSchema);
	        		}
	        	}
	        }
	        Collections.sort(vis, new Comparator<MDSchema>() {
				@Override
				public int compare(MDSchema o1, MDSchema o2) {
					return o1.getName().compareTo(o2.getName());
				}
			});
	        MDSchema defaultSchema = metaDataSource.getDefaultSchema();
	        if (defaultSchema != null) {
	        	vis.add(0, defaultSchema);
	        }
	        createSchemaSelectionList(visPanel, vis);
        }
		
		KeyListener keyListener = new KeyListener() {
			@Override
			public void keyTyped(KeyEvent e) {
				if (e.getKeyChar() == KeyEvent.VK_ESCAPE) {
					close();
				} else if (e.getKeyChar() == KeyEvent.VK_DOWN) {
					searchList.grabFocus();
				} else if (e.getKeyChar() == '\n') {
					result = searchList.getSelectedValue();
			    	plainIsValid = e.getSource() == searchTextField;
					close();
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
		
		searchList.addListSelectionListener(new ListSelectionListener() {
			@Override
			public void valueChanged(ListSelectionEvent e) {
				if (showAllLabel != null && showAllLabel.equals(searchList.getSelectedValue())) {
					showAll = true;
					updateList();
				}
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
				if (e.getClickCount() > 1) {
					result = searchList.getSelectedValue();
					close();
				}
			}
		});
		searchList.setCellRenderer(new DefaultListCellRenderer() {
			@Override
			public Component getListCellRendererComponent(JList<?> list, Object value, int index, boolean isSelected,
					boolean cellHasFocus) {
				if (value.equals(showAllLabel)) {
					return super.getListCellRendererComponent(list, "<html><font color=\"#ff0000\">" + value + "</font>", index, isSelected, cellHasFocus);
				}
				Color bgColor = Color.WHITE;
				Color fgColor = new Color(0, 0, 0);
				String hlColor = "#050aff";
				if (isSelected) {
					bgColor = new Color(0, 0, 145);
					fgColor = Color.WHITE;
					hlColor = "#ff9999";
				}
				String search = searchTextField.getText().trim().toUpperCase(Locale.ENGLISH);
				int i = value.toString().trim().toUpperCase(Locale.ENGLISH).indexOf(search);
				String item = value.toString();
				if (i >= 0) {
					item = item.substring(0, i) + "<b><font color=\"" + hlColor + "\">" + item.substring(i, i + search.length()) + "</font></b>" + item.substring(i + search.length());
				}
				if (stringCount != null) {
					Integer count = stringCount.get(value.toString());
					if (count != null && count > 1) {
						item += "&nbsp;<font color=" + (isSelected? "#66ff66" : "#006600") + ">&nbsp;&nbsp;(" + count + ")</font>";
					}
				}
				String html = "<html>" + item;
				Component render = super.getListCellRendererComponent(list, html, index, false, cellHasFocus);
				render.setBackground(bgColor);
				render.setForeground(fgColor);
				if (renderConsumer != null) {
					Consumer<JLabel> cons = renderConsumer.get(value);
					if (cons != null && render instanceof JLabel) {
						cons.accept((JLabel) render);
					}
				}
				if (isSelected) {
					render.setBackground(bgColor);
					render.setForeground(fgColor);
				}
				return render;
			}
			
		});

		if (metaDataSource == null) {
			schemaPanel.setVisible(false);
		}
		
		searchTextField.setText("");
		updateList();
    }

    public void setInitialValue(String value) {
		searchTextField.setText(value);
		searchTextField.selectAll();
		updateList(false);
		acceptAll = true;
	}

    private boolean acceptAll = false;
    private boolean plainIsValid = false;
    
    public String getPlainValue() {
	    return plainIsValid? searchTextField.getText() : combobox.getSelectedItem() instanceof String? (String) combobox.getSelectedItem() : searchTextField.getText();
    }
    
	private void createSchemaSelectionList(JPanel container, final List<MDSchema> vis) {
    	container.setLayout(new GridLayout(metaDataSource.getSchemas().size() + 1, 1));
    	Set<MDSchema> selectedSchemas = new HashSet<MDSchema>();
    	boolean loadVis = false;
    	for (MDSchema dmSchema: vis) {
    		boolean isLoaded = dmSchema.isLoaded();
			JCheckBox checkBox = createSchemaCheckbox(dmSchema, isLoaded);
    		if (isLoaded) {
    			selectedSchemas.add(dmSchema);
    		} else {
    			loadVis = true;
    		}
			container.add(checkBox);
    	}
    	prepare.prepare(selectedSchemas);
    	
    	JPanel panel = new JPanel();
    	panel.setLayout(new java.awt.GridBagLayout());
    	panel.setOpaque(false);
        GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        JPanel sepPanel = new JPanel(null);
        sepPanel.setPreferredSize(new Dimension(1, 2));
        sepPanel.setBorder(javax.swing.BorderFactory.createEtchedBorder());
        panel.add(sepPanel, gridBagConstraints);
        container.add(panel);
        
		for (MDSchema dmSchema: metaDataSource.getSchemas()) {
			if (!vis.contains(dmSchema)) {
				JCheckBox checkBox = createSchemaCheckbox(dmSchema, false);
				container.add(checkBox);
			}
    	}
		
		if (loadVis) {
			UIUtil.invokeLater(new Runnable() {
				@Override
				public void run() {
					selectSchemas(vis);
				}
			});
		}
	}

    private final Map<MDSchema, JCheckBox> checkboxPerSchema = Collections.synchronizedMap(new HashMap<MDSchema, JCheckBox>());
    private final List<MDSchema> schemas = Collections.synchronizedList(new ArrayList<MDSchema>());
    private final AtomicBoolean stateChangeMode = new AtomicBoolean(false);
	private final Prepare prepare;
    
	private JCheckBox createSchemaCheckbox(final MDSchema mdSchema, boolean selected) {
		final JCheckBox checkBox = new JCheckBox(mdSchema.getName());
		checkBox.setSelected(selected);
		checkboxPerSchema.put(mdSchema, checkBox);
		schemas.add(mdSchema);
		checkBox.addItemListener(new ItemListener() {
			@Override
			public void itemStateChanged(ItemEvent e) {
				if (!stateChangeMode.get()) {
					if (checkBox.isSelected()) {
						selectSchemas(Collections.singletonList(mdSchema));
					} else {
						updateTableList();
					}
				}
			}
		});
		return checkBox;
	}

	private void updateTableList() {
		Set<MDSchema> selectedSchemas = new HashSet<MDSchema>();
    	for (Entry<MDSchema, JCheckBox> e: checkboxPerSchema.entrySet()) {
    		if (e.getValue().isSelected()) {
    			selectedSchemas.add(e.getKey());
    		}
    	}
    	prepare.prepare(selectedSchemas);
    	updateList();
	}

	private AtomicBoolean cancelLoading = new AtomicBoolean(false);
	private AtomicBoolean loadingDialogisVisible = new AtomicBoolean(false);

	private void selectSchemas(final List<MDSchema> schemas) {
		UIUtil.invokeLater(new Runnable() {
			@Override
			public void run() {
				final List<MDSchema> toLoad = new ArrayList<MDSchema>();
				for (MDSchema schema: schemas) {
					if (schema.isLoaded()) {
						stateChangeMode.set(true);
						checkboxPerSchema.get(schema).setSelected(true);
						stateChangeMode.set(false);
					} else {
						toLoad.add(schema);
					}
				}
				
				if (!toLoad.isEmpty()) {
					final JDialog loadingDialog = new JDialog(dialog, "Loading", true);
					Thread thread = new Thread(new Runnable() {
						@Override
						public void run() {
							cancelLoading.set(false);
							for (MDSchema schema: toLoad) {
								schema.loadTables(false, null, null, null);
								setCheckboxState(checkboxPerSchema.get(schema), schema, true, false);
								while (!schema.isLoaded() && !cancelLoading.get()) {
									try {
										Thread.sleep(10);
									} catch (InterruptedException e) {
										// ignore
									}
								}
								if (cancelLoading.get()) {
									setCheckboxState(checkboxPerSchema.get(schema), schema, false, false);						
									break;
								}
								setCheckboxState(checkboxPerSchema.get(schema), schema, false, true);
							}
							UIUtil.invokeLater(new Runnable() {
								@Override
								public void run() {
									loadingDialog.setVisible(false);
								}
							});
						}
		
						private void setCheckboxState(final JCheckBox checkBox, final MDSchema schema, final boolean waitingState, final boolean selected) {
							UIUtil.invokeLater(new Runnable() {
								@Override
								public void run() {
									checkBox.setForeground(waitingState? Color.red : null);
									stateChangeMode.set(true);
									checkboxPerSchema.get(schema).setSelected(selected);
									stateChangeMode.set(false);
									updateTableList();
								}
							});
						}
					});
					thread.setDaemon(true);
					thread.start();
					loadingDialog.getContentPane().add(loadingPanel);
					loadingDialog.pack();
					Point los = dialog.getLocationOnScreen();
					loadingDialog.setLocation(los.x + dialog.getWidth() / 2 - loadingDialog.getWidth() / 2, los.y + dialog.getHeight() / 2 - loadingDialog.getHeight() / 2);
					loadingDialogisVisible.set(true);
					loadingDialog.setVisible(true);
					loadingDialog.dispose();
					loadingDialogisVisible.set(false);
				}
				
				updateTableList();
			}
		});
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

        loadingPanel = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        cancelLoadiingButton = new javax.swing.JButton();
        jPanel1 = new javax.swing.JPanel();
        cancelButton = new javax.swing.JButton();
        jPanel4 = new javax.swing.JPanel();
        searchTextField = new javax.swing.JTextField();
        okButton = new javax.swing.JButton();
        jScrollPane1 = new javax.swing.JScrollPane();
        searchList = new javax.swing.JList<>();
        schemaPanel = new javax.swing.JPanel();
        jPanel2 = new javax.swing.JPanel();
        jScrollPane2 = new javax.swing.JScrollPane();
        jPanel3 = new javax.swing.JPanel();
        jLabel2 = new javax.swing.JLabel();
        visPanel = new javax.swing.JPanel();
        selectAllButton = new javax.swing.JButton();
        plugInPanel = new javax.swing.JPanel();

        loadingPanel.setBackground(java.awt.Color.white);

        jLabel1.setForeground(java.awt.Color.red);
        jLabel1.setText("loading...");
        loadingPanel.add(jLabel1);

        cancelLoadiingButton.setText("Cancel");
        cancelLoadiingButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelLoadiingButtonActionPerformed(evt);
            }
        });
        loadingPanel.add(cancelLoadiingButton);

        jPanel1.setLayout(new java.awt.GridBagLayout());

        cancelButton.setText("Cancel");
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 3;
        jPanel1.add(cancelButton, gridBagConstraints);

        setLayout(new java.awt.GridBagLayout());

        jPanel4.setLayout(new java.awt.GridBagLayout());

        searchTextField.setText("jTextField1");
        searchTextField.setToolTipText("<html>Search criteria.<br><br>\nSearch for items that contain the search criteria as:<br>\n<table>\n<tr><td><b>Prefix</b></td><td>if it starts with a space</td></tr>\n<tr><td><b>Suffix</b></td><td>if it ends with a space</td></tr>\n<tr><td><b>Substring</b></td><td>else</td></tr>\n</table>\n</html>\n\n");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        jPanel4.add(searchTextField, gridBagConstraints);

        okButton.setText("  Ok  ");
        okButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                okButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        jPanel4.add(okButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        add(jPanel4, gridBagConstraints);

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

        schemaPanel.setLayout(new java.awt.GridBagLayout());

        jPanel2.setBorder(javax.swing.BorderFactory.createTitledBorder("Schema"));
        jPanel2.setLayout(new java.awt.GridBagLayout());

        jPanel3.setLayout(new java.awt.GridBagLayout());

        jLabel2.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.weighty = 1.0;
        jPanel3.add(jLabel2, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
        gridBagConstraints.weightx = 1.0;
        jPanel3.add(visPanel, gridBagConstraints);

        jScrollPane2.setViewportView(jPanel3);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel2.add(jScrollPane2, gridBagConstraints);

        selectAllButton.setText("Select all");
        selectAllButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                selectAllButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel2.add(selectAllButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        schemaPanel.add(jPanel2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridheight = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 0.5;
        gridBagConstraints.weighty = 1.0;
        add(schemaPanel, gridBagConstraints);

        plugInPanel.setLayout(new java.awt.BorderLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridheight = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 0.1;
        add(plugInPanel, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    private void okButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_okButtonActionPerformed
    	result = searchList.getSelectedValue();
    	plainIsValid = true;
		close();
    }//GEN-LAST:event_okButtonActionPerformed

    private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
    	close();
	}//GEN-LAST:event_cancelButtonActionPerformed

	public void close() {
		dialog.setVisible(false);
	}

    private void cancelLoadiingButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelLoadiingButtonActionPerformed
        cancelLoading.set(true);
    }//GEN-LAST:event_cancelLoadiingButtonActionPerformed

    private void selectAllButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_selectAllButtonActionPerformed
        selectSchemas(schemas);
    }//GEN-LAST:event_selectAllButtonActionPerformed

    Map<String, Integer> stringCount = null;

	public void setStringCount(Map<String, Integer> stringCount) {
		this.stringCount = stringCount;
	}

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton cancelButton;
    private javax.swing.JButton cancelLoadiingButton;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JPanel loadingPanel;
    private javax.swing.JButton okButton;
    private javax.swing.JPanel plugInPanel;
    private javax.swing.JPanel schemaPanel;
    private javax.swing.JList<String> searchList;
    private javax.swing.JTextField searchTextField;
    private javax.swing.JButton selectAllButton;
    private javax.swing.JPanel visPanel;
    // End of variables declaration//GEN-END:variables
    
    static private ImageIcon icon;
    static private ImageIcon icon2;
    static {
		// load images
		icon = UIUtil.readImage("/search.png");
		icon2 = UIUtil.readImage("/search2.png");
	}
}
