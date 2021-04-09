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
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.RowSorter;
import javax.swing.ScrollPaneConstants;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingConstants;
import javax.swing.event.AncestorEvent;
import javax.swing.event.AncestorListener;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.table.TableModel;

import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.RowIdSupport;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.util.Quoting;

/**
 * Row Details View.
 * 
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public abstract class DetailsView extends javax.swing.JPanel {

	private final Table table;
	private final List<Row> rows;
	private final RowSorter<? extends TableModel> rowSorter;
	private final RowIdSupport rowIdSupport;
	private final boolean showSpinner;
	private final Session session;
	private final String[] alternativeColumnLabels;

	/** Creates new form DetailsView 
	 * @param rowSorter 
	 * @param showSelectButton 
	 * @param deselect 
	*/
	public DetailsView(List<Row> rows, int size, DataModel dataModel, Table table, int rowIndex, RowSorter<? extends TableModel> rowSorter, boolean showSpinner, boolean showSelectButton, RowIdSupport rowIdSupport, boolean deselect, String[] alternativeColumnLabels, Session session) {
		this.table = table;
		this.rows = rows;
		this.rowSorter = rowSorter;
		this.rowIdSupport = rowIdSupport;
		this.showSpinner = showSpinner;
		this.alternativeColumnLabels = alternativeColumnLabels;
		this.session = session;
		initComponents();
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
				setCurrentRow((Integer) model.getValue() - 1, true);
			}
		});
		KeyListener keyListener = new KeyListener() {
			@Override
			public void keyTyped(KeyEvent e) {
				if (e.getKeyChar() == KeyEvent.VK_ESCAPE) {
					onClose();
				}
			}

			@Override
			public void keyReleased(KeyEvent arg0) {
			}

			@Override
			public void keyPressed(KeyEvent arg0) {
			}
		};
		rowSpinner.addKeyListener(keyListener);
		sortCheckBox.addKeyListener(keyListener);
		closeButton.addKeyListener(keyListener);
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

	private int currentRow;
	private boolean sortColumns;
	private JPanel content;
	private boolean isPacked = false;

	private void setCurrentRow(int row, boolean selectableFields) {
		currentRow = row;

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
		
		int i = 0;
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
		List<Runnable> removeTableNames = new ArrayList<Runnable>();
		List<JLabel> tableNames = new ArrayList<JLabel>();
		while (i < columns.size()) {
			Column c = columns.get(columnIndex.get(i));
			JLabel l = new JLabel();
			JComponent lCont = l;
			if (alternativeColumnLabels != null && alternativeColumnLabels.length > columnIndex.get(i)) {
				String altName = alternativeColumnLabels[columnIndex.get(i)];
				String text = altName;
				String[] ntPair = altName.replaceAll("<br>|<.html>", "\t").split("\t");
				if (ntPair.length == 3) {
					text = "<html>" + ntPair[1] + "</html>\t<html>" + ntPair[0] + "</html>";
				}
				l.setText(shortText(text));
				l.setToolTipText(text);
				if (!text.equals(altName)) {
					int pos = text.indexOf('\t');
					if (i >= 0) {
						JPanel panel = new JPanel(new GridBagLayout());
						JLabel tab = new JLabel(text.substring(pos + 1));
						JLabel sep = new JLabel(" ");
						tableNames.add(tab);
						removeTableNames.add(() -> {
							tab.setVisible(false);
							sep.setVisible(false);
							GridBagConstraints gbc = new java.awt.GridBagConstraints();
							gbc.gridx = 4;
							gbc.gridy = 1;
							gbc.weightx = 1;
							gbc.fill = GridBagConstraints.HORIZONTAL;
							gbc.anchor = GridBagConstraints.EAST;
					        panel.add(new JLabel(""), gbc);
					    });
						String cName = text.substring(0, pos);
						l.setText(shortText(cName));
						l.setToolTipText(cName);
						gridBagConstraints = new java.awt.GridBagConstraints();
				        gridBagConstraints.gridx = 2;
				        gridBagConstraints.gridy = 1;
				        gridBagConstraints.weightx = 1;
				        gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
				        gridBagConstraints.anchor = GridBagConstraints.EAST;
				        panel.add(sep, gridBagConstraints);
				        gridBagConstraints = new java.awt.GridBagConstraints();
				        gridBagConstraints.gridx = 3;
				        gridBagConstraints.gridy = 1;
				        gridBagConstraints.anchor = GridBagConstraints.EAST;
				        panel.add(l, gridBagConstraints);
				        gridBagConstraints = new java.awt.GridBagConstraints();
				        gridBagConstraints.gridx = 1;
				        gridBagConstraints.gridy = 1;
				        gridBagConstraints.anchor = GridBagConstraints.WEST;
				        panel.add(tab, gridBagConstraints);
				        lCont = panel;
					} else {
						l.setText(altName.replaceFirst("^(<html>)(.*)<br>(.*)(</html>)$", "$1$3&nbsp;&nbsp;$2$4"));
					}
				}
			} else {
				l.setText((c.name != null? c.name : ""));
			}
			l.setFont(nonbold);
			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
			gridBagConstraints.weightx = 0;
			gridBagConstraints.weighty = 0;
			gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
			gridBagConstraints.gridx = 0;
			gridBagConstraints.gridy = i;
			if (!selectableFields) {
				l.setText(" " + l.getText() + "    ");
				l.setVerticalAlignment(SwingConstants.TOP);
			} else {
				gridBagConstraints.insets = new Insets(0, 4, 0, 8);
			}
			content.add(lCont, gridBagConstraints);

			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
			gridBagConstraints.weighty = 0;
			gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
			gridBagConstraints.weightx = 1;
			gridBagConstraints.gridx = 1;
			gridBagConstraints.gridy = i;
			Object v = rows.get(rowSorter != null? rowSorter.convertRowIndexToModel(row) : row).values[columnIndex.get(i)];
//			if (v instanceof BigDecimal) {
//				v = SqlUtil.toString((BigDecimal) v);
//			} else if (v instanceof Double) {
//				v = SqlUtil.toString((Double) v);
//			}

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
				JTextArea f = new JTextArea();
				f.addFocusListener(new FocusListener() {
					@Override
					public void focusLost(FocusEvent e) {
						if (finalV instanceof UIUtil.IconWithText) {
							f.setText(((UIUtil.IconWithText) finalV).text);
						} else {
							f.setText(finalV == null? "" : finalV.toString());
						}
					}
					
					@Override
					public void focusGained(FocusEvent e) {
						if (vOrig instanceof UIUtil.IconWithText) {
							f.setText(((UIUtil.IconWithText) vOrig).text);
						} else {
							f.setText(vOrig == null? "" : vOrig.toString());
						}
						if (vOrig != null) {
							f.selectAll();
						}
					}
				});
				if (v instanceof UIUtil.IconWithText) {
					f.setText(((UIUtil.IconWithText) v).text);
				} else {
					f.setText(v == null? "" : v.toString());
				}
//				f.setEnabled(v != null);
				if (v == null) {
					f.setBackground(new Color(238, 238, 238));
				}
				f.setEditable(false);
				content.add(f, gridBagConstraints);
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
				f.setBackground(i % 2 == 0? BG1 : BG2);
				l.setBackground(i % 2 == 0? BG1 : BG2);
				f.setOpaque(true);
				l.setOpaque(true);
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
		
		if (selectableFields) {
			JLabel l = new JLabel();
			l.setText(" ");
			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
			gridBagConstraints.weightx = 1;
			gridBagConstraints.weighty = 1;
			gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
			gridBagConstraints.gridx = 1;
			gridBagConstraints.gridy = i;
			content.add(l, gridBagConstraints);
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
		onRowChanged(row);
	}

	private String shortText(String text) {
		return text.replaceFirst("^(.{32}).+$", "$1...");
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
        sortCheckBox = new javax.swing.JCheckBox();
        closeButton = new javax.swing.JButton();
        selectButton = new javax.swing.JButton();

        setLayout(new java.awt.GridBagLayout());

        jLabel1.setText(" Row ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(0, 2, 0, 0);
        add(jLabel1, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        add(rowSpinner, gridBagConstraints);

        jPanel1.setLayout(new java.awt.GridBagLayout());
        jScrollPane1.setViewportView(jPanel1);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(jScrollPane1, gridBagConstraints);

        sortCheckBox.setText("Sort Columns");
        sortCheckBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                sortCheckBoxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 4);
        add(sortCheckBox, gridBagConstraints);

        closeButton.setText("CLose");
        closeButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                closeButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 3;
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
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 0);
        add(selectButton, gridBagConstraints);
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
    private javax.swing.JLabel jLabel1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JSpinner rowSpinner;
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

	private void resetScrollPane() {
		jScrollPane1.getVerticalScrollBar().setValue(jScrollPane1.getVerticalScrollBar().getMinimum());					
		jScrollPane1.getHorizontalScrollBar().setValue(jScrollPane1.getHorizontalScrollBar().getMinimum());
	}

}
