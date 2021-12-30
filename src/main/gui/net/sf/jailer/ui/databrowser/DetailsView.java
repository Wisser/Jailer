/*
 * Copyright 2007 - 2022 Ralf Wisser.
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
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Point;
import java.awt.Window;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.RowSorter;
import javax.swing.ScrollPaneConstants;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.event.AncestorEvent;
import javax.swing.event.AncestorListener;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.table.TableModel;
import javax.swing.text.JTextComponent;

import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.RowIdSupport;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.BrowserContentPane.TableModelItem;
import net.sf.jailer.ui.util.MovePanel;
import net.sf.jailer.ui.util.SizeGrip;
import net.sf.jailer.ui.util.SmallButton;
import net.sf.jailer.util.CellContentConverter.PObjectWrapper;
import net.sf.jailer.util.Quoting;

/**
 * Row Details View.
 * 
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public abstract class DetailsView extends javax.swing.JPanel {

	private final Table table;
	protected final List<Row> rows;
	private final RowSorter<? extends TableModel> rowSorter;
	private final RowIdSupport rowIdSupport;
	private final boolean showSpinner;
	private final Session session;
	private final String[] alternativeColumnLabels;
	private final String[] alternativeColumnLabelsFull;
	private final BrowserContentCellEditor browserContentCellEditor;
	private final TableModel tableModel;
	private boolean initialized = false;
	private Consumer<Boolean> pendingUpdate;

	/** Creates new form DetailsView 
	 * @param rowSorter 
	 * @param showSelectButton 
	 * @param deselect 
	 * @param alternativeColumnLabelsFull 
	*/
	public DetailsView(List<Row> rows, int size, DataModel dataModel, Table table, int rowIndex, RowSorter<? extends TableModel> rowSorter, boolean showSpinner, boolean showSelectButton, RowIdSupport rowIdSupport, boolean deselect, String[] alternativeColumnLabels, String[] alternativeColumnLabelsFull, Session session, BrowserContentCellEditor browserContentCellEditor, TableModel tableModel) {
		this.table = table;
		this.rows = rows;
		this.rowSorter = rowSorter;
		this.rowIdSupport = rowIdSupport;
		this.showSpinner = showSpinner;
		this.alternativeColumnLabels = alternativeColumnLabels;
		this.alternativeColumnLabelsFull = alternativeColumnLabelsFull;
		this.session = session;
		this.browserContentCellEditor = browserContentCellEditor;
		this.tableModel = tableModel;
		initComponents();
		browserContentCellEditor.setLoading(false);
		editModeToggleButton.setBackground(new Color(255, 206, 206));
		editModeToggleButton.setForeground(Color.black);
		editModeToggleButton.setIcon(editdetails);
		pinToggleButton.setIcon(pinIcon);
		pinToggleButton.setText(null);
		pinToggleButton.setToolTipText("Pin Window");
		FocusListener editCancelListener = new FocusListener() {
			@Override
			public void focusLost(FocusEvent e) {
			}
			@Override
			public void focusGained(FocusEvent e) {
				cancelEdit();
			}
		};
		rowSpinner.addFocusListener(editCancelListener);
		JComponent editor = rowSpinner.getEditor();
		if (editor != null) {
			editor.addFocusListener(editCancelListener);
			if (editor instanceof JSpinner.DefaultEditor) {
				if (((JSpinner.DefaultEditor) editor).getTextField() != null) {
					((JSpinner.DefaultEditor) editor).getTextField().addFocusListener(editCancelListener);
				}
			}
		}
		sortCheckBox.addFocusListener(editCancelListener);
		editModeToggleButton.addFocusListener(editCancelListener);
		closeButton.addFocusListener(editCancelListener);
		selectButton.addFocusListener(editCancelListener);
		if (deselect) {
			selectButton.setText("Deselect Row");
		}
		if (rowSorter != null) {
			rowIndex = rowSorter.convertRowIndexToView(rowIndex);
		}
		final SpinnerNumberModel model = new SpinnerNumberModel(rowIndex + 1, 1, size, -1);
		rowSpinner.setModel(model);
		rowSpinner.addChangeListener(new ChangeListener() {
			@Override
			public void stateChanged(ChangeEvent e) {
				int row = (Integer) model.getValue() - 1;
				if (row >= 0) {
					setCurrentRow(row, true);
				}
			}
		});
		if (!showSelectButton) {
			selectButton.setVisible(false);
		}
		if (!showSpinner) {
			jLabel1.setVisible(false);
			rowSpinner.setVisible(false);
			sortCheckBox.setVisible(false);
			closeButton.setVisible(false);
			selectButton.setVisible(false);
			jScrollPane1.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER);
			jScrollPane1.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
		} else {
			addAncestorListener(new AncestorListener() {
				@Override
				public void ancestorRemoved(AncestorEvent event) {
				}
				@Override
				public void ancestorMoved(AncestorEvent event) {
				}
				@Override
				public void ancestorAdded(AncestorEvent event) {
					closeButton.grabFocus();
					resetScrollPane();
				}
			});
		}
        if (jScrollPane1.getHorizontalScrollBar() != null) {
        	jScrollPane1.getHorizontalScrollBar().setUnitIncrement(16);
        }
        if (jScrollPane1.getVerticalScrollBar() != null) {
        	jScrollPane1.getVerticalScrollBar().setUnitIncrement(16);
        }
        setCurrentRow(rowIndex, showSpinner);
        initialized = true;
	}

	/**
	 * Default constructor.
	 */
	protected DetailsView() {
		this.table = null;
		this.showSpinner = false;
		this.session = null;
		this.rows = null;
		this.rowSorter = null;
		this.rowIdSupport = null;
		this.alternativeColumnLabels = null;
		this.alternativeColumnLabelsFull = null;
		this.browserContentCellEditor = null;
		this.tableModel = null;
	}

	private static final Font font = new JLabel().getFont();
	private static final Font nonbold = new Font(font.getName(), font.getStyle() & ~Font.BOLD, font.getSize()); 
	private static final Font italic = new Font(font.getName(), font.getStyle() & ~Font.BOLD | Font.ITALIC, font.getSize()); 
	public static final Color BG1 = UIUtil.TABLE_BACKGROUND_COLOR_1;
	public static final Color BG2 = UIUtil.TABLE_BACKGROUND_COLOR_2;
	public static final Color BG3 = blend(new Color(196, 234, 255), BG1);
	public static final Color BG3_2 = blend(new Color(184, 226, 255), BG2);
	public static final Color FG1 = new Color(155, 0, 0);
	private List<JLabel> labels = new ArrayList<JLabel>();
	private List<Color> labelColors = new ArrayList<Color>();
	
	public void setBorderColor(Color color) {
		jScrollPane1.setBorder(BorderFactory.createEtchedBorder(color, Color.GRAY));
	}
	
	private static Color blend(Color a, Color b) {
		final double f = 0.6;
		return new Color(
				(int)(a.getRed() * f + b.getRed() * (1 - f)),
				(int)(a.getGreen() * f + b.getGreen() * (1 - f)),
				(int)(a.getBlue() * f + b.getBlue() * (1 - f)));
	}

	protected int currentRow = -1;
	private boolean sortColumns;
	private JPanel content;
	private boolean isPacked = false;

	protected void setCurrentRow(int row, boolean selectableFields) {
		try {
			if (row >= rows.size()) {
				row = rows.size() - 1;
				if (row < 0) {
					rowSpinner.setValue(0);
					return;
				}
			}
			browserContentCellEditor.setInDetailsView(true);
			boolean changed = currentRow != row;
			currentRow = row;
			cancelEdit();
			
			java.awt.GridBagConstraints gridBagConstraints;
			
			labels.clear();
			labelColors.clear();
			
			final JPanel oldContent = content;
			content = new JPanel(new GridBagLayout());
			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.weightx = 1;
			gridBagConstraints.weighty = 1;
			gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
			gridBagConstraints.gridx = 0;
			gridBagConstraints.gridy = 0;
			
			final List<Column> columns = rowIdSupport.getColumns(table, session);
			List<Integer> columnIndex = new ArrayList<Integer>();
			for (int j = 0; j < columns.size(); ++j) {
				columnIndex.add(j);
			}
			if (sortColumns) {
				Collections.sort(columnIndex, new Comparator<Integer>() {
					@Override
					public int compare(Integer o1, Integer o2) {
						String o1Name = columns.get(o1).name;
						if (o1Name == null && alternativeColumnLabels != null && alternativeColumnLabels.length > o1) {
							o1Name = alternativeColumnLabels[o1];
						}
						String o2Name = columns.get(o2).name;
						if (o2Name == null && alternativeColumnLabels != null && alternativeColumnLabels.length > o2) {
							o2Name = alternativeColumnLabels[o2];
						}
						if (o1Name == null || o2Name == null) {
							if (o1Name == null && o2Name == null) {
								return 0;
							}
							if (o1Name == null) {
								return 1;
							} else {
								return -1;
							}
						}
						return Quoting.staticUnquote(o1Name).compareToIgnoreCase(Quoting.staticUnquote(o2Name));
					}
				});
			}
			boolean hasEditableColumn = false;
			List<Runnable> removeTableNames = new ArrayList<Runnable>();
			List<JLabel> tableNames = new ArrayList<JLabel>();
			Component nextFocusComponentForLastField = editModeToggleButton;
			int i = 0;
			while (i < columns.size()) {
				Integer columnIndexAtI = columnIndex.get(i);
				Column c = columns.get(columnIndexAtI);
				JLabel l = new JLabel();
				l.setVerticalAlignment(SwingConstants.TOP);
				JComponent lCont = l;
				JLabel tab = null;
				JPanel panel2 = null;
				if (alternativeColumnLabels != null && alternativeColumnLabels.length > columnIndexAtI) {
					String altName = alternativeColumnLabels[columnIndexAtI];
					String text = altName;
					String[] ntPair = altName.replaceAll("<.?html>", "").replaceAll("<br>", "\t").split("\t");
					if (ntPair.length == 2) {
						text = ntPair[0];
					} else if (ntPair.length == 3) {
						text = "<html>" + ntPair[1] + "</html>\t<html>" + ntPair[0] + "</html>";
					}
					l.setText(text);
					if (alternativeColumnLabelsFull != null && alternativeColumnLabelsFull.length > columnIndexAtI) {
						l.setToolTipText(alternativeColumnLabelsFull[columnIndexAtI]);
					} else {
						l.setToolTipText(altName);
					}
					if (!text.equals(altName)) {
						int pos = text.indexOf('\t');
						if (pos >= 0) {
							JPanel panel = new JPanel(new GridBagLayout());
							panel.setToolTipText(l.getToolTipText());
							tab = new JLabel(text.substring(pos + 1));
							tab.setToolTipText(l.getToolTipText());
							tab.setOpaque(true);
							JLabel sep = new JLabel("  ");
							sep.setToolTipText(l.getToolTipText());
							tableNames.add(tab);
							JLabel fTab = tab;
							removeTableNames.add(() -> {
								fTab.setVisible(false);
								sep.setVisible(false);
								l.setText(l.getText().replaceAll("<[^>]+>", ""));
								GridBagConstraints gbc = new java.awt.GridBagConstraints();
								gbc.gridx = 4;
								gbc.gridy = 1;
								gbc.weightx = 1;
								gbc.fill = GridBagConstraints.HORIZONTAL;
								gbc.anchor = GridBagConstraints.NORTHWEST;
						        panel.add(new JLabel(""), gbc);
						    });
							String cName = text.substring(0, pos);
							l.setText(shortText(cName));
							gridBagConstraints = new java.awt.GridBagConstraints();
					        gridBagConstraints.gridx = 2;
					        gridBagConstraints.gridy = 1;
					        gridBagConstraints.fill = GridBagConstraints.BOTH;
					        gridBagConstraints.anchor = GridBagConstraints.NORTHWEST;
					        gridBagConstraints.weighty = 1;
					        panel.add(sep, gridBagConstraints);
					        gridBagConstraints = new java.awt.GridBagConstraints();
					        gridBagConstraints.gridx = 3;
					        gridBagConstraints.gridy = 1;
					        gridBagConstraints.anchor = GridBagConstraints.NORTHWEST;
					        gridBagConstraints.fill = GridBagConstraints.BOTH;
					        gridBagConstraints.weightx = 1;
					        gridBagConstraints.weighty = 1;
					        panel.add(l, gridBagConstraints);
					        gridBagConstraints = new java.awt.GridBagConstraints();
					        gridBagConstraints.gridx = 1;
					        gridBagConstraints.gridy = 1;
					        gridBagConstraints.anchor = GridBagConstraints.NORTHWEST;
					        panel2 = new JPanel(new FlowLayout(0, 0, 0));
					        panel2.setOpaque(true);
					        panel2.setBackground(Color.green);
					        panel2.add(tab);
					        gridBagConstraints = new java.awt.GridBagConstraints();
					        gridBagConstraints.gridx = 1;
					        gridBagConstraints.gridy = i;
					        gridBagConstraints.anchor = GridBagConstraints.NORTHWEST;
					        gridBagConstraints.fill = GridBagConstraints.BOTH;
					        content.add(panel2, gridBagConstraints);
					        lCont = panel;
						} else {
							l.setText(shortText(text.replaceAll("<[^>]+>", "")));
						}
					}
				} else {
					l.setText(shortText(c.name != null? c.name : ""));
				}
				l.setFont(nonbold);
				gridBagConstraints = new java.awt.GridBagConstraints();
				gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
				gridBagConstraints.weightx = 0;
				gridBagConstraints.weighty = 0;
				gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
				gridBagConstraints.gridx = 2;
				gridBagConstraints.gridy = i;
				if (!selectableFields) {
					l.setVerticalAlignment(SwingConstants.TOP);
				}
				content.add(lCont, gridBagConstraints);
	
				gridBagConstraints = new java.awt.GridBagConstraints();
				gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
				gridBagConstraints.weighty = 0;
				gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
				gridBagConstraints.weightx = 1;
				gridBagConstraints.gridx = 6;
				gridBagConstraints.gridy = i;
				int rowIndex = rowSorter != null? rowSorter.convertRowIndexToModel(row) : row;
				Object v = rows.get(rowIndex).values[columnIndexAtI];
				hasEditableColumn = hasEditableColumn || tableModel.isCellEditable(rowIndex, columnIndexAtI);
				boolean isEditable = editModeToggleButton.isSelected() && tableModel.isCellEditable(rowIndex, columnIndexAtI);
				if (v instanceof PObjectWrapper) {
					v = ((PObjectWrapper) v).getValue();
				}
				Object vOrig = v;
				if (v instanceof Double) {
					v = UIUtil.format((Double) v);
				} else if (v instanceof Float) {
					v = UIUtil.format((double) (Float) v);
				} else if (v instanceof Long) {
					v = UIUtil.format((long) (Long) v);
				} else if (v instanceof Integer) {
					v = UIUtil.format((long) (Integer) v);
				} else if (v instanceof Short) {
					v = UIUtil.format((long) (Short) v);
				} else if (v instanceof BigDecimal && ((BigDecimal) v).scale() >= 0) {
					try {
						v = UIUtil.format((BigDecimal) v);
					} catch (Exception e) {
						// ignore
					}
				} else if (v instanceof BigDecimal || v instanceof BigInteger) {
					try {
						v = NumberFormat.getInstance().format(v);
					} catch (Exception e) {
						// ignore
					}
				}
				
				Object finalV = v;
				if (selectableFields) {
					JTextComponent f;
					if (v != null && v.toString().contains("\n")) {
						f = new JTextArea();
					} else {
						f = new JTextField();
					}
					if (i == columns.size() - 1) {
						f.setNextFocusableComponent(nextFocusComponentForLastField);
					}
					if (i == 0) {
						nextFocusComponentForLastField = f;
					}
					switch (UIUtil.plaf) {
						case NATIVE:
							// nothing to do
							break;
						case FLAT:
						case NIMBUS:
							f.setBorder(BorderFactory.createLineBorder(isEditable? new Color(255, 242, 240) : Color.white));
							break;
					}
					f.addKeyListener(new KeyListener() {
						@Override
						public void keyTyped(KeyEvent e) {
							if (e.getKeyChar() == KeyEvent.VK_ESCAPE || e.getKeyChar() == KeyEvent.VK_CANCEL) {
								if (pendingUpdate != null) {
									pendingUpdate.accept(false);
								}
								pendingUpdate = null;
								editModeToggleButton.grabFocus();
							}
							if (e.getKeyChar() == KeyEvent.VK_ENTER && !(f instanceof JTextArea)) {
								if (pendingUpdate != null) {
									pendingUpdate.accept(true);
								}
								pendingUpdate = null;
								editModeToggleButton.grabFocus();
							}
						}
			
						@Override
						public void keyReleased(KeyEvent arg0) {
						}
			
						@Override
						public void keyPressed(KeyEvent arg0) {
						}
					});
					f.addFocusListener(new FocusListener() {
						Color origColor;
						String origText;
						@Override
						public void focusLost(FocusEvent e) {
						}
						
						@Override
						public void focusGained(FocusEvent e) {
							if (pendingUpdate != null) {
								pendingUpdate.accept(true);
							}
							pendingUpdate = ok -> {
								if (isEditable) {
									f.setBackground(origColor);
								}
								Point vPos = jScrollPane1.getViewport().getViewPosition();
								if (isEditable && ok && origText != null
										&& !origText.equals(f.getText())) {
									tableModel.setValueAt(f.getText(), rowIndex, columnIndexAtI);
									if (browserContentCellEditor.isLoading()) {
										disableAll(DetailsView.this);
										waitLoading();
										return;
									}
									Object v = tableModel.getValueAt(rowIndex, columnIndexAtI);
									if (v instanceof TableModelItem) {
										v = ((TableModelItem) v).value;
									}
									f.setText(v == null ? "null" : v.toString());
								} else {
									if (finalV instanceof UIUtil.IconWithText) {
										f.setText(((UIUtil.IconWithText) finalV).text);
									} else {
										f.setText(finalV == null ? "null" : finalV.toString());
									}
								}
								origText = null;
								UIUtil.invokeLater(() -> jScrollPane1.getViewport().setViewPosition(vPos));
							};
							origColor = f.getBackground();
							if (isEditable) {
								f.setBackground(null);
							}
							Point vPos = jScrollPane1.getViewport().getViewPosition();
							if (isEditable) {
								origText = browserContentCellEditor.cellContentToText(columnIndexAtI, vOrig);
								f.setText(origText);
							} else {
								if (vOrig instanceof UIUtil.IconWithText) {
									f.setText(((UIUtil.IconWithText) vOrig).text);
								} else {
									f.setText(vOrig == null? "null" : vOrig.toString());
								}
							}
							if (vOrig != null && !isEditable) {
								f.selectAll();
							}
							UIUtil.invokeLater(() -> jScrollPane1.getViewport().setViewPosition(vPos));
						}
					});
					if (v instanceof UIUtil.IconWithText) {
						f.setText(((UIUtil.IconWithText) v).text);
					} else {
						f.setText(v == null? "null" : v.toString());
					}
	//				f.setEnabled(v != null);
					if (v == null) {
						f.setForeground(Color.GRAY);
						f.setFont(italic);
					}
					f.setEditable(isEditable);
					f.setBackground(i % 2 == 0? BG1 : BG2);
					l.setBackground(i % 2 == 0? BG1 : BG2);
					f.setOpaque(true);
					l.setOpaque(true);
					content.add(f, gridBagConstraints);
					JLabel l1 = new JLabel(" ");
					JLabel l2 = new JLabel("    ");
					l1.setOpaque(true);
					l2.setOpaque(true);
					gridBagConstraints = new java.awt.GridBagConstraints();
					gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
					gridBagConstraints.weightx = 0;
					gridBagConstraints.weighty = 0;
					gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
					gridBagConstraints.gridx = 0;
					gridBagConstraints.gridy = i;
					content.add(l1, gridBagConstraints);
					gridBagConstraints = new java.awt.GridBagConstraints();
					gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
					gridBagConstraints.weightx = 0;
					gridBagConstraints.weighty = 0;
					gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
					gridBagConstraints.gridx = 4;
					gridBagConstraints.gridy = i;
					content.add(l2, gridBagConstraints);
					f.setBackground(i % 2 == 0? BG1 : BG2);
					l.setBackground(i % 2 == 0? BG1 : BG2);
					l1.setBackground(i % 2 == 0? BG1 : BG2);
					l2.setBackground(i % 2 == 0? BG1 : BG2);
					lCont.setBackground(i % 2 == 0? BG1 : BG2);
					if (panel2 != null) {
						panel2.setBackground(i % 2 == 0? BG1 : BG2);
					}
					if (tab != null) {
						tab.setBackground(i % 2 == 0? BG1 : BG2);
					}
					if (rowIdSupport.getPrimaryKey(table) != null && rowIdSupport.getPrimaryKey(table).getColumns().contains(c)) {
						lCont.setForeground(FG1);
						l.setForeground(FG1);
						l1.setForeground(FG1);
						l2.setForeground(FG1);
					} else {
						lCont.setForeground(Color.BLUE);
						l.setForeground(Color.BLUE);
						l1.setForeground(Color.BLUE);
						l2.setForeground(Color.BLUE);
					}
					MouseListener ml = new MouseAdapter() {
						@Override
						public void mousePressed(MouseEvent e) {
							f.grabFocus();
						}
					};
					lCont.addMouseListener(ml);
					l.addMouseListener(ml);
					l1.addMouseListener(ml);
					l2.addMouseListener(ml);
					if (panel2 != null) {
						panel2.addMouseListener(ml);
					}
					if (tab != null) {
						tab.addMouseListener(ml);
					}
					labelColors.add(l.getBackground());
					labels.add(l);
					if (f.getText().trim().length() > 0) {
						f.setToolTipText(UIUtil.toHTML(f.getText(), 200));
					}
					if (isEditable) {
						f.setBackground(i % 2 == 0? new Color(255, 242, 240) : new Color(255, 236, 236));
					}
				} else {
					JLabel f = new JLabel();
					String text;
					if (v instanceof UIUtil.IconWithText) {
						text = ((UIUtil.IconWithText) v).text + "    ";
						f.setIcon(((UIUtil.IconWithText) v).icon);
					} else {
						text = (v == null? "null" : v.toString()) + "    ";
					}
					f.setText(text.indexOf('\n') >= 0? UIUtil.toHTML(text, 0) : text);
					f.setFont(v == null? italic : nonbold);
					if (v == null) {
						f.setForeground(Color.GRAY);
					}
					content.add(f, gridBagConstraints);
					l.setText(" " + l.getText() + "  ");
					f.setOpaque(true);
					l.setOpaque(true);
					f.setBackground(i % 2 == 0? BG1 : BG2);
					l.setBackground(i % 2 == 0? BG1 : BG2);
					if (rowIdSupport.getPrimaryKey(table) != null && rowIdSupport.getPrimaryKey(table).getColumns().contains(c)) {
						l.setForeground(FG1);
					} else {
						l.setForeground(Color.BLUE);
					}
					labelColors.add(f.getBackground());
					labels.add(f);
				}
				++i;
			}
			editModeToggleButton.setVisible(hasEditableColumn);
			JPanel p = new JPanel();
			Dimension preferredSize = new Dimension(1, 1);
			p.setPreferredSize(preferredSize);
			p.setOpaque(true);
			p.setBackground(i % 2 == 0? BG1 : BG2);
			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.weightx = 1;
			gridBagConstraints.weighty = 1;
			gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
			gridBagConstraints.gridx = 0;
			gridBagConstraints.gridy = i;
			gridBagConstraints.gridwidth = 10;
			gridBagConstraints.weightx = 1;
			gridBagConstraints.weighty = 1;
			content.add(p, gridBagConstraints);
			Set<String> un = new HashSet<String>();
			tableNames.forEach(s -> {
				if (!s.getText().matches("^((\\s*)|(<[^>]+>)*((&nbsp;)*)(<[^>]+>))*$")) {
					un.add(s.getText());
				}
			});
			if (un.size() <= 1) {
				removeTableNames.forEach(s -> {
					s.run();
				});
			}
			
			Runnable update = new Runnable() {
				@Override
				public void run() {
					if (oldContent != null) {
						jPanel1.remove(oldContent);
					}
					GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
					gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
					gridBagConstraints.weightx = 1;
					gridBagConstraints.weighty = 1;
					gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
					gridBagConstraints.gridx = 1;
					gridBagConstraints.gridy = 1;
					jPanel1.add(content, gridBagConstraints);
					jPanel1.revalidate();
					jPanel1.repaint();
				}
			};
			if (isPacked) {
				UIUtil.invokeLater(2, update);
			} else {
				update.run();
				isPacked = true;
			}
			if (changed && initialized) {
				onRowChanged(row);
			}
		} finally {
			browserContentCellEditor.setInDetailsView(false);
		}
	}

	protected abstract void waitLoading();

	private void disableAll(Component root) {
		root.setEnabled(false);
		if (root instanceof JComponent)
		for (Component c: ((JComponent)root).getComponents()) {
			disableAll(c);
		}
	}

	protected abstract void onRowChanged(int row);
	protected abstract void onClose();
	protected abstract void onSelectRow(Row row);

	/** This method is called from within the constructor to
	 * initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is
	 * always regenerated by the Form Editor.
	 */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jLabel1 = new javax.swing.JLabel();
        rowSpinner = new javax.swing.JSpinner();
        jScrollPane1 = new javax.swing.JScrollPane();
        jPanel1 = new javax.swing.JPanel();
        closeButton = new javax.swing.JButton();
        selectButton = new javax.swing.JButton();
        jToolBar1 = new javax.swing.JToolBar();
        sortCheckBox = new javax.swing.JCheckBox();
        editModeToggleButton = new javax.swing.JToggleButton();
        pinToggleButton = new javax.swing.JToggleButton();

        setLayout(new java.awt.GridBagLayout());

        jLabel1.setText(" Row ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridheight = 2;
        gridBagConstraints.insets = new java.awt.Insets(0, 2, 0, 0);
        add(jLabel1, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 4);
        add(rowSpinner, gridBagConstraints);

        jPanel1.setLayout(new java.awt.GridBagLayout());
        jScrollPane1.setViewportView(jPanel1);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.gridwidth = 12;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(jScrollPane1, gridBagConstraints);

        closeButton.setText("CLose");
        closeButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                closeButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 5;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.gridwidth = 12;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        add(closeButton, gridBagConstraints);

        selectButton.setText("Select Row");
        selectButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                selectButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 4);
        add(selectButton, gridBagConstraints);

        jToolBar1.setFloatable(false);
        jToolBar1.setRollover(true);

        sortCheckBox.setText("Sort Columns ");
        sortCheckBox.setFocusable(false);
        sortCheckBox.setHorizontalTextPosition(javax.swing.SwingConstants.RIGHT);
        sortCheckBox.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        sortCheckBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                sortCheckBoxActionPerformed(evt);
            }
        });
        jToolBar1.add(sortCheckBox);

        editModeToggleButton.setText(" Edit Mode ");
        editModeToggleButton.setFocusable(false);
        editModeToggleButton.setHorizontalTextPosition(javax.swing.SwingConstants.RIGHT);
        editModeToggleButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        editModeToggleButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                editModeToggleButtonActionPerformed(evt);
            }
        });
        jToolBar1.add(editModeToggleButton);

        pinToggleButton.setText("jToggleButton1");
        pinToggleButton.setFocusable(false);
        pinToggleButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        pinToggleButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        pinToggleButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                pinToggleButtonActionPerformed(evt);
            }
        });
        jToolBar1.add(pinToggleButton);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 5;
        gridBagConstraints.gridy = 1;
        add(jToolBar1, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    private boolean programChangedSortCheckBox = false;
    
    private void sortCheckBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_sortCheckBoxActionPerformed
    	if (!programChangedSortCheckBox) {
    		sortColumns = !sortColumns;
	    	setCurrentRow(currentRow, showSpinner);
	    	UIUtil.invokeLater(4, new Runnable() {
				@Override
				public void run() {
					resetScrollPane();
				}
			});
    	}
    }//GEN-LAST:event_sortCheckBoxActionPerformed

    private void closeButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_closeButtonActionPerformed
        onClose();
    }//GEN-LAST:event_closeButtonActionPerformed

    private void selectButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_selectButtonActionPerformed
        onSelectRow(rows.get(rowSorter != null? rowSorter.convertRowIndexToModel(currentRow) : currentRow));
    }//GEN-LAST:event_selectButtonActionPerformed

    private void editModeToggleButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_editModeToggleButtonActionPerformed
        setCurrentRow(currentRow, showSpinner);
    }//GEN-LAST:event_editModeToggleButtonActionPerformed

    private void pinToggleButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_pinToggleButtonActionPerformed
    	if (editModeToggleButton.isSelected()) {
    		editModeToggleButton.doClick();
    	}
        editModeToggleButton.setSelected(false);
        editModeToggleButton.setEnabled(false);
        selectButton.setEnabled(false);
    }//GEN-LAST:event_pinToggleButtonActionPerformed

    public boolean isPinned() {
    	return pinToggleButton.isSelected();
    }
    
	public void updateInClosureState(boolean inClosure) {
		if (inClosure) {
			int r = 0;
			for (JLabel label: labels) {
				label.setBackground(r++ % 2 == 0? BG3 : BG3_2);
			}
		} else {
			for (int i = 0; i < labels.size(); ++i) {
				labels.get(i).setBackground(labelColors.get(i));
			}
		}
	}
    
    public JPanel getDetailsPanel() {
    	return jPanel1;
    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton closeButton;
    public javax.swing.JToggleButton editModeToggleButton;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JToolBar jToolBar1;
    private javax.swing.JToggleButton pinToggleButton;
    protected javax.swing.JSpinner rowSpinner;
    private javax.swing.JButton selectButton;
    private javax.swing.JCheckBox sortCheckBox;
    // End of variables declaration//GEN-END:variables

	public void setSortColumns(boolean selected) {
		sortColumns = selected;
    	setCurrentRow(currentRow, showSpinner);
    	try {
    		programChangedSortCheckBox = true;
    		sortCheckBox.setSelected(selected);
    	} finally {
    		programChangedSortCheckBox = false;
    	}
	}

	public void prepareForNonModalUsage() {
		closeButton.setVisible(false);
		setBorder(BorderFactory.createLineBorder(Color.black));
		SizeGrip corner = new SizeGrip();
		boolean isLeftToRight = getComponentOrientation().isLeftToRight();
        String str = isLeftToRight ? JScrollPane.LOWER_RIGHT_CORNER :
                                        JScrollPane.LOWER_LEFT_CORNER;
        jScrollPane1.setCorner(str, corner);
        jScrollPane1.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
        jScrollPane1.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
        
        JPanel movePanel = new MovePanel();
        
        GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.gridheight = 2;
        gridBagConstraints.weightx = 1.0;
        add(movePanel, gridBagConstraints);
        
        SmallButton closeButton = new SmallButton(closeIcon, closeOverIcon, false) {
			@Override
			protected void onClick(MouseEvent e) {
				Window windowAncestor = SwingUtilities.getWindowAncestor(DetailsView.this);
				windowAncestor.setVisible(false);
				windowAncestor.dispose();
			}
		};
		gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridheight = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.gridheight = 2;
        add(closeButton, gridBagConstraints);
 	}
 
	private void resetScrollPane() {
		jScrollPane1.getVerticalScrollBar().setValue(jScrollPane1.getVerticalScrollBar().getMinimum());					
		jScrollPane1.getHorizontalScrollBar().setValue(jScrollPane1.getHorizontalScrollBar().getMinimum());
	}
	
	private void cancelEdit() {
		if (pendingUpdate != null) {
			pendingUpdate.accept(false);
		}
		pendingUpdate = null;
	}
	
	private String shortText(String text) {
		return text.replaceFirst("^(<[^>]+>)*([^<]{32}).+$", "$2...");
	}
	
	public void restoreFromOld(DetailsView oldDv) {
		if (oldDv != null) {
			setSortColumns(oldDv.sortColumns);
			UIUtil.invokeLater(() -> jScrollPane1.getViewport().setViewPosition(oldDv.jScrollPane1.getViewport().getViewPosition()));
		}
	}

	private static ImageIcon editdetails;
	private static ImageIcon pinIcon;
	private static ImageIcon closeIcon;
	private static ImageIcon closeOverIcon;
	static {
		// load images
		editdetails = UIUtil.readImage("/editdetails.png");
		pinIcon = UIUtil.readImage("/pin.png");
        closeIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/close.png"), 1.4);
        closeOverIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/close_over.png"), 1.4);
	}
}
