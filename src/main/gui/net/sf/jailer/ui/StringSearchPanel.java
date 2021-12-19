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
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
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
import java.awt.geom.Rectangle2D;
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
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
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

import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.databrowser.metadata.MDSchema;
import net.sf.jailer.ui.databrowser.metadata.MetaDataSource;
import net.sf.jailer.ui.util.MovePanel;
import net.sf.jailer.ui.util.SizeGrip;

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
		return createSearchButton(owner, comboBox, titel, onSuccess, prepare, metaDataSource, dataModel, alternativeIcon, additionalComponentFactory, locateUnderButton, false, null, false);
	}

	public static JToggleButton createSearchButton(final Window owner, final javax.swing.JComboBox comboBox, final Object titel, final Runnable onSuccess, final Prepare prepare, final MetaDataSource metaDataSource, final DataModel dataModel, boolean alternativeIcon, final AdditionalComponentFactory additionalComponentFactory, final boolean locateUnderButton, final boolean keepSearchText, final Map<String, Consumer<JLabel>> renderConsumer, boolean closeOwner) {
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
								searchPanel.setCloseOwner(closeOwner);
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

	private boolean isEscaped = false;

	private void consumeResult() {
		boolean success = false;
		if (acceptAll || result != null && !result.equals(showAllLabel)) {
			combobox.setSelectedItem(result);
			if (!isEscaped && onSuccess != null) {
				onSuccess.run();
				success = true;
			}
		}
		result = null;
		if (button != null) {
			button.setSelected(false);
		}
		if (!success) {
			onAbort();
		}
	}
	
	public void setOnClose(Runnable onClose) {
		this.onClose = onClose;
	}

	private Integer estimatedItemsCount = null;
	private Integer cellHeight;
	
	public void setEstimatedItemsCount(Integer estimatedItemsCount) {
		this.estimatedItemsCount = estimatedItemsCount;
	}
	
	public class StringSearchDialog extends JDialog {

		public StringSearchDialog(Window owner, String titel) {
			super(owner, titel);
		}
		
	};

	public void find(Window owner, Object titel, int x, int y, boolean locateUnderButton) {
		isEscaped = false;
		explictlyClosed = false;
		dialog = new StringSearchDialog(owner, String.valueOf(titel));
		dialog.setModal(false);
		dialog.setUndecorated(true);
		dialog.addWindowFocusListener(new WindowFocusListener() {
			long t = System.currentTimeMillis();
			@Override
			public void windowLostFocus(WindowEvent e) {
				if (!loadingDialogisVisible.get()) {
					if (System.currentTimeMillis() < t + 200) {
						dialog.requestFocus();
						return;
					}
					if (owner != null && isCloseOwner() && e.getOppositeWindow() != owner) {
						if (!(owner instanceof JFrame)) {
							owner.dispose();
							owner.setVisible(false);
						}
					}
					onClosing();
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
		oHeight = dialog.getHeight();
		double mh = 440;
		int size = searchList.getModel().getSize();
		double f = size / 18.0;
		if (f < 1) {
			mh = Math.max(240, mh * f + 16);
		}
		int height = Math.max(dialog.getHeight(), (int) mh);
		Rectangle2D screenBounds = UIUtil.getScreenBounds();
		if (!locateUnderButton) {
			y = Math.max((int) screenBounds.getY(), y - height);
		}
		dialog.setLocation(x, y);
		int minWidth = metaDataSource == null? 300 : 500;
		if (pv) {
			minWidth *= 2;
		}
		Integer prefWidth = preferredWidth();
		Integer maxX = maxX();
		if (estimatedItemsCount != null && estimatedItemsCount > 0) {
			height = (int) ((estimatedItemsCount) * Math.max(16, cellHeight) + oHeight);
		}
		dialog.setSize(prefWidth != null ? prefWidth : Math.max(minWidth, dialog.getWidth()), Math.min(height, 300));
		if (!locateUnderButton) {
			dialog.setLocation(dialog.getX(), dialog.getY() + height - dialog.getHeight());
		}
		if (maxX != null) {
			dialog.setLocation(Math.max((int) screenBounds.getX(), Math.min(maxX, dialog.getX())), dialog.getY());
		}
		Integer maxY = maxY(dialog.getHeight());
		if (maxY != null && maxY < dialog.getY()) {
			int deltaH = Math.min(dialog.getY() - maxY, (int) (0.30 * dialog.getHeight()));
			maxY += deltaH;
			dialog.setSize(dialog.getWidth(), dialog.getHeight() - deltaH);
			dialog.setLocation(dialog.getX(), Math.max((int) screenBounds.getY(), maxY));
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
				UIUtil.setPopupActive(false);
				if (onClose != null) {
					onClose.run();
				}
			}
		});
		plainIsValid = false;
		UIUtil.setPopupActive(true);
		UIUtil.addDW(dialog);
		dialog.setVisible(true);
	}

	protected void onClosing() {
	}

	private int oHeight;
	
	public void resetHeight() {
		Integer maxX = maxX();
		int y = dialog.getY();
		Rectangle2D screenBounds = UIUtil.getScreenBounds();
		if (estimatedItemsCount != null && estimatedItemsCount >= 0) {
			int height = (int) ((Math.max(2, estimatedItemsCount)) * Math.max(16, cellHeight) + oHeight);
			dialog.setSize(dialog.getWidth(), Math.min(height, 600));
			if (maxX != null) {
				dialog.setLocation(Math.max((int) screenBounds.getX(), Math.min(maxX, dialog.getX())), dialog.getY());
			}
			Integer maxY = maxY(dialog.getHeight());
			if (maxY != null && maxY < dialog.getY()) {
				int deltaH = Math.min(dialog.getY() - maxY, (int) (0.30 * dialog.getHeight()));
				maxY += deltaH;
				dialog.setSize(dialog.getWidth(), dialog.getHeight() - deltaH);
				dialog.setLocation(dialog.getX(), Math.max((int) screenBounds.getY(), maxY));
			}
			if (dialog.getY() < y) {
				int diff = y - dialog.getY();
				if (dialog.getHeight() - diff > 280) {
					dialog.setLocation(dialog.getX(), dialog.getY() + diff);
					dialog.setSize(dialog.getWidth(), dialog.getHeight() - diff);
				}
			}
		}
	}

	protected Integer preferredWidth() {
		return null;
	}

	protected Integer maxX() {
		return null;
	}

	protected Integer maxY(int height) {
		Window window = SwingUtilities.getWindowAncestor(this);
		if (window != null) {
			while (window.getOwner() != null) {
				window = window.getOwner();
			}
			return window.getY() + Math.max(0, window.getHeight() - height - 8);
		}
		return null;
	}

	private int maxListLength = 80;
	private String showAllLabel;
	private boolean keepSearchText = false;
	private boolean isFiltered = true;
	
	public void updateList() {
		updateList(true, false);
	}
	
	public void updateList(boolean filter) {
		updateList(filter, false);
	}

	public void updateList(boolean filter, boolean allowDuplicates) {
		DefaultListModel<String> matches = new DefaultListModel<String>();
		Set<String> seen = new HashSet<String>();
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
					if (!searchText.isEmpty() && !allowDuplicates) {
						if (seen.contains(item)) {
							matches.removeElement(item);
						}
					}
					matches.addElement(item);
					seen.add(item);
					if (matches.getSize() > maxListLength) {
						showAllLabel = "more...";
						matches.addElement(showAllLabel);
						break;
					}
				}
			}
		}
		searchList.setModel(matches);
		if (!acceptAll && !matches.isEmpty()) {
			searchList.setSelectedIndex(0);
		}
		isFiltered = filter;
	}

	private final MetaDataSource metaDataSource;
	private final DataModel dataModel;
	private final javax.swing.JComboBox combobox;
	private final Runnable onSuccess;
	private Runnable onClose;
	private final JToggleButton button;
	public final Map<String, Consumer<JLabel>> renderConsumer;
	
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
            
        MovePanel comp = new MovePanel();
        GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        sizeGripPanel.add(comp, gridBagConstraints);
        
        bottomPanel.setVisible(false);
        setStatus(null, null);
        plugInPanel.setVisible(false);

        DefaultListModel<String> model = new DefaultListModel<String>();
		searchList.setModel(model);
		cellHeight = searchList.getMinimumSize().height;
		model.addElement("1");
		model.addElement("2");
		model.addElement("3");
		model.addElement("4");
		cellHeight = (searchList.getMinimumSize().height - cellHeight) / 4;
		
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
					isEscaped = true;
					close(true);
				} else if (e.getKeyChar() == KeyEvent.VK_DOWN) {
					searchList.grabFocus();
					searchList.setSelectedIndex(0);
				} else if (e.getKeyChar() == '\n') {
					result = searchList.getSelectedValue();
			    	plainIsValid = e.getSource() == searchTextField;
					close(true);
				}
			}
			@Override
			public void keyReleased(KeyEvent e) {
			}
			@Override
			public void keyPressed(KeyEvent e) {
				if (e.getComponent() == searchTextField) {
					int kc = e.getKeyCode();
					if (kc == KeyEvent.VK_DOWN) {
						searchList.grabFocus();
						searchList.setSelectedIndex(0);
					}
				}
			}
		};
		searchTextField.addKeyListener(keyListener);
		searchList.addKeyListener(keyListener);
		okButton.addKeyListener(keyListener);
		okButton.setIcon(UIUtil.scaleIcon(okButton, okIcon));
		
		UIUtil.setTrailingComponent(searchTextField, okButton);
		
		searchTextField.getDocument().addDocumentListener(new DocumentListener() {
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
			protected void update() {
				updateList();
				infoLabel.setVisible(searchTextField.getText().isEmpty());
			}
		});
		
		searchList.addListSelectionListener(new ListSelectionListener() {
			@Override
			public void valueChanged(ListSelectionEvent e) {
				if (showAllLabel != null && showAllLabel.equals(searchList.getSelectedValue())) {
					maxListLength += 5000;
					try {
						UIUtil.setWaitCursor(StringSearchPanel.this);
						updateList(isFiltered);
					} finally {
						UIUtil.resetWaitCursor(StringSearchPanel.this);
					}
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
					close(true);
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
					item = UIUtil.toHTMLFragment(item.substring(0, i), 0) + "<b><font color=\"" + hlColor + "\">" + UIUtil.toHTMLFragment(item.substring(i, i + search.length()), 0) + "</font></b>" + UIUtil.toHTMLFragment(item.substring(i + search.length()), 0);
				} else {
					item = UIUtil.toHTMLFragment(item, 0);
				}
//				if (stringCount != null) {
//					Integer count = stringCount.get(value.toString());
//					if (count != null && count > 1) {
//						item += "&nbsp;<font color=" + (isSelected? "#66ff66" : "#006600") + ">&nbsp;&nbsp;(" + count + ")</font>";
//					}
//				}
				String html = "<html><nobr>" + item.replaceAll("<br>", "") + "</html>";
				Component render = super.getListCellRendererComponent(list, html, index, false, cellHasFocus);
				render.setBackground(bgColor);
				render.setForeground(fgColor);
				if (renderConsumer != null) {
					Consumer<JLabel> cons = renderConsumer.get(value);
					if (cons != null && render instanceof JLabel) {
						cons.accept((JLabel) render);
					}
				}
				if (render instanceof JLabel) {
					String text = ((JLabel) render).getText();
					((JLabel) render).setToolTipText(text);
				}
				if (isSelected) {
					render.setBackground(bgColor);
					render.setForeground(fgColor);
				}
				if (stringCount != null && stringCountLeftPad != null) {
					Integer count = stringCount.get(value.toString());
					if (count != null) {
						JPanel panel = new JPanel(new GridBagLayout());
						GridBagConstraints gbc = new GridBagConstraints();
						gbc.gridx = 1;
						gbc.gridy = 1;
						gbc.weightx = 1;
						gbc.fill = GridBagConstraints.HORIZONTAL;
						gbc.anchor = GridBagConstraints.WEST;
						panel.add(render, gbc);
						gbc = new GridBagConstraints();
						gbc.gridx = 2;
						gbc.gridy = 1;
						gbc.weightx = 0;
						gbc.fill = GridBagConstraints.NONE;
						gbc.anchor = GridBagConstraints.EAST;
						JLabel cl = new JLabel(stringCountLeftPad + " ");
						cl.setForeground(new Color(255, 255, 255, 0));
						panel.add(cl, gbc);
						if (count > 1 || count < 0) {
							gbc = new GridBagConstraints();
							gbc.gridx = 2;
							gbc.gridy = 1;
							gbc.weightx = 0;
							gbc.fill = GridBagConstraints.NONE;
							gbc.anchor = GridBagConstraints.EAST;
							cl = new JLabel(" " + Math.abs(count) + "  ");
							cl.setForeground(lightCounters? new Color(160, 130, 100) : new Color(0, 130, 0));
							panel.add(cl, gbc);
						}
						panel.setOpaque(false);
						render.setMinimumSize(new Dimension(1, render.getMinimumSize().height));
						render.setPreferredSize(new Dimension(1, render.getPreferredSize().height));
						if (render instanceof JLabel) {
							String text = ((JLabel) render).getText();
							panel.setToolTipText(text);
						}
						return panel;
					}
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

    /**
     * Closes panel.
     */
	public void abort() {
		isEscaped = true;
		close(true);
	}

	/**
     * Sets status text and icon.
     */
    public void setStatus(String text, Icon icon) {
		statusLabel.setText(text);
		statusLabel.setIcon(icon);
		statusPanel.setVisible(text != null || icon != null);
		bottomPanel.setVisible(text != null || icon != null || withSG);
    }

	public void setInitialValue(String value) {
		searchTextField.setText(value);
		searchTextField.selectAll();
		updateList(false, true);
		acceptAll = true;
	}
	
	public void withSizeGrip() {
		JPanel corner = new SizeGrip();
        GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        sizeGripPanel.add(corner, gridBagConstraints);
		withSG = true;
		bottomPanel.setVisible(true);
	}

	private boolean withSG = false;
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
        jLayeredPane1 = new javax.swing.JLayeredPane();
        searchTextField = new javax.swing.JTextField();
        okButton = new javax.swing.JButton();
        infoLabel = new javax.swing.JLabel();
        schemaPanel = new javax.swing.JPanel();
        jPanel2 = new javax.swing.JPanel();
        jScrollPane2 = new javax.swing.JScrollPane();
        jPanel3 = new javax.swing.JPanel();
        jLabel2 = new javax.swing.JLabel();
        visPanel = new javax.swing.JPanel();
        selectAllButton = new javax.swing.JButton();
        plugInPanel = new javax.swing.JPanel();
        jPanel5 = new javax.swing.JPanel();
        bottomPanel = new javax.swing.JPanel();
        statusPanel = new javax.swing.JPanel();
        statusLabel = new javax.swing.JLabel();
        sizeGripPanel = new javax.swing.JPanel();
        bottomComponentsPanel = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        searchList = new javax.swing.JList<>();

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

        jLayeredPane1.setLayout(new java.awt.GridBagLayout());

        searchTextField.setToolTipText("<html>Search criteria.<br><br>\nSearch for items that contain the search criteria as:<br>\n<table>\n<tr><td><b>Prefix</b></td><td>if it starts with a space</td></tr>\n<tr><td><b>Suffix</b></td><td>if it ends with a space</td></tr>\n<tr><td><b>Substring</b></td><td>else</td></tr>\n</table>\n</html>\n\n");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        jLayeredPane1.add(searchTextField, gridBagConstraints);

        okButton.setText("Ok");
        okButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                okButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 0);
        jLayeredPane1.add(okButton, gridBagConstraints);

        infoLabel.setFont(infoLabel.getFont().deriveFont((infoLabel.getFont().getStyle() | java.awt.Font.ITALIC)));
        infoLabel.setForeground(java.awt.Color.lightGray);
        infoLabel.setText("  Type partial value to search");
        jLayeredPane1.setLayer(infoLabel, javax.swing.JLayeredPane.MODAL_LAYER);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        jLayeredPane1.add(infoLabel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel4.add(jLayeredPane1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        add(jPanel4, gridBagConstraints);

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
        gridBagConstraints.gridheight = 3;
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

        jPanel5.setBorder(new javax.swing.border.LineBorder(java.awt.Color.lightGray, 1, true));
        jPanel5.setLayout(new java.awt.GridBagLayout());

        bottomPanel.setLayout(new java.awt.GridBagLayout());

        statusPanel.setBackground(new java.awt.Color(255, 255, 204));
        statusPanel.setLayout(new java.awt.GridBagLayout());

        statusLabel.setForeground(new java.awt.Color(0, 0, 77));
        statusLabel.setText("jLabel3");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 16, 0, 0);
        statusPanel.add(statusLabel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        bottomPanel.add(statusPanel, gridBagConstraints);

        sizeGripPanel.setOpaque(false);
        sizeGripPanel.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.gridheight = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        bottomPanel.add(sizeGripPanel, gridBagConstraints);

        bottomComponentsPanel.setLayout(new java.awt.BorderLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        bottomPanel.add(bottomComponentsPanel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        jPanel5.add(bottomPanel, gridBagConstraints);

        searchList.setVisibleRowCount(1);
        jScrollPane1.setViewportView(searchList);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel5.add(jScrollPane1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(jPanel5, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    private void okButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_okButtonActionPerformed
    	result = searchList.getSelectedValue();
    	plainIsValid = true;
		close(true);
    }//GEN-LAST:event_okButtonActionPerformed

    private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
    	close(true);
	}//GEN-LAST:event_cancelButtonActionPerformed

    private boolean explictlyClosed = false;
    
	public boolean isExplictlyClosed() {
		return explictlyClosed;
	}

	public void close(boolean explictlyClosed) {
		this.explictlyClosed = explictlyClosed;
		onClose(searchTextField.getText());
		dialog.setVisible(false);
	}

	protected void onClose(String text) {
	}

	protected void onAbort() {
	}

    private void cancelLoadiingButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelLoadiingButtonActionPerformed
        cancelLoading.set(true);
    }//GEN-LAST:event_cancelLoadiingButtonActionPerformed

    private void selectAllButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_selectAllButtonActionPerformed
        selectSchemas(schemas);
    }//GEN-LAST:event_selectAllButtonActionPerformed

    Map<String, Integer> stringCount = null;
    private String stringCountLeftPad = null;
    private boolean lightCounters = false;
    
	public void setLightCounters(boolean lightCounters) {
		this.lightCounters = lightCounters;
	}

	public void setStringCount(Map<String, Integer> stringCount) {
		this.stringCount = stringCount;
		this.stringCountLeftPad = null;
		stringCount.values().stream().map(Math::abs).max(Integer::compare).ifPresent(max -> {
			if (max > 1) {
				stringCountLeftPad = " ";
				for (int i = String.valueOf(max).length(); i > 0; --i) {
					stringCountLeftPad += "0";
				}
			}
		});
	}

	private boolean closeOwner;
	
    public boolean isCloseOwner() {
		return closeOwner;
	}

	public void setCloseOwner(boolean closeOwner) {
		this.closeOwner = closeOwner;
	}
	
	public void addBottomcomponent(JComponent component) {
		bottomComponentsPanel.add(component, java.awt.BorderLayout.CENTER);
	}

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JPanel bottomComponentsPanel;
    private javax.swing.JPanel bottomPanel;
    private javax.swing.JButton cancelButton;
    private javax.swing.JButton cancelLoadiingButton;
    private javax.swing.JLabel infoLabel;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLayeredPane jLayeredPane1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JPanel loadingPanel;
    private javax.swing.JButton okButton;
    private javax.swing.JPanel plugInPanel;
    private javax.swing.JPanel schemaPanel;
    private javax.swing.JList<String> searchList;
    private javax.swing.JTextField searchTextField;
    private javax.swing.JButton selectAllButton;
    private javax.swing.JPanel sizeGripPanel;
    private javax.swing.JLabel statusLabel;
    private javax.swing.JPanel statusPanel;
    private javax.swing.JPanel visPanel;
    // End of variables declaration//GEN-END:variables
    
    static private ImageIcon icon;
    static private ImageIcon icon2;
    private static ImageIcon okIcon;
    
	static {
		// load images
		icon = UIUtil.readImage("/search.png");
		icon2 = UIUtil.readImage("/search2.png");
		okIcon = UIUtil.readImage("/buttonok.png");
 	}
}

