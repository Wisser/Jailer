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

import java.awt.BasicStroke;
import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Window;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.ClipboardOwner;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.DefaultComboBoxModel;
import javax.swing.ImageIcon;
import javax.swing.InputMap;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.JViewport;
import javax.swing.KeyStroke;
import javax.swing.ListSelectionModel;
import javax.swing.RowSorter;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.WindowConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableColumnModelEvent;
import javax.swing.event.TableColumnModelListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableModel;

import net.sf.jailer.ui.databrowser.sqlconsole.TabContentPanel;
import net.sf.jailer.util.LogUtil;
import net.sf.jailer.util.Pair;

/**
 * Extendet Copy Panel.
 * 
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public class ExtendetCopyPanel extends javax.swing.JPanel {

	private static final KeyStroke KS_COPY_TO_CLIPBOARD = KeyStroke.getKeyStroke(KeyEvent.VK_C, InputEvent.CTRL_DOWN_MASK);

	private static Boolean lastColored;
	private static Boolean lastAligned;
	private static Boolean lastFormatted;
	private static Object lastCellpadding;
	
	public static void openDialog(JTable jTable, boolean allColumnsSelected, String tableName, List<Integer> rowColumnTypes, boolean columnNamesInFirstRow, boolean silent) {
		Window owner = SwingUtilities.getWindowAncestor(jTable);
		JDialog window = new JDialog(owner, "Extended Copy");
		window.setModal(false);
		
		window.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		ExtendetCopyPanel copyPanel = new ExtendetCopyPanel(columnNamesInFirstRow);
		copyPanel.initContentTable(jTable, allColumnsSelected, columnNamesInFirstRow);
		
		window.getContentPane().add(copyPanel);
		
		window.pack();
		window.setSize(Math.max(window.getWidth(), jTable.getParent().getWidth()), window.getHeight());
		copyPanel.previewPanel.setMinimumSize(new Dimension(1, 180));
		copyPanel.previewPanel.setPreferredSize(copyPanel.previewPanel.getMinimumSize());
		copyPanel.tabContentPanel = new TabContentPanel(null, new JLabel(""), null, new JLabel(""), tableName, 
				false,
				null,
				null,
				rowColumnTypes,
				true, silent);
		copyPanel.tabContentPanel.setRowAndColumnsLimit(500);
		copyPanel.tabContentPanel.setColumnNamesInFirstRow(columnNamesInFirstRow);
		copyPanel.plainPanel.add(copyPanel.tabContentPanel.textTabPanel, java.awt.BorderLayout.CENTER);
		copyPanel.tabContentPanel.copyCBButton.setVisible(false);
		GridBagConstraints g = new java.awt.GridBagConstraints();
		g.insets = new Insets(0, 0, 0, 8);
		copyPanel.controlsPanel.add(copyPanel.tabContentPanel.headerCheckBox, g);
		g = new java.awt.GridBagConstraints();
		g.insets = new Insets(0, 0, 0, 8);
		copyPanel.controlsPanel.add(copyPanel.tabContentPanel.rotateCheckBox, g);
		g = new java.awt.GridBagConstraints();
		g.insets = new Insets(0, 0, 0, 8);
		copyPanel.controlsPanel2.add(copyPanel.tabContentPanel.columnSeparatorLabel, g);
		g = new java.awt.GridBagConstraints();
		g.insets = new Insets(0, 0, 0, 8);
		copyPanel.controlsPanel2.add(copyPanel.tabContentPanel.columnSeparatorComboBox, g);
		if (!silent) {
			if (lastAligned != null) {
				copyPanel.alignedCheckBox.setSelected(lastAligned);
			}
			if (lastColored != null) {
				copyPanel.coloredCheckBox.setSelected(lastColored);
			}
			if (lastFormatted != null) {
				copyPanel.formattedCheckBox.setSelected(lastFormatted);
			}
			if (lastCellpadding != null) {
				copyPanel.cellpaddingComboBox.setSelectedItem(lastCellpadding);
			}
		} else {
			if (columnNamesInFirstRow && jTable.getSelectedRowCount() <= 1) {
				copyPanel.tabContentPanel.headerCheckBox.setSelected(false);
			}
		}
		ItemListener l;
		copyPanel.formattedCheckBox.addItemListener(l = e -> {
			copyPanel.tabContentPanel.columnSeparatorLabel.setVisible(!copyPanel.formattedCheckBox.isSelected());
			copyPanel.tabContentPanel.columnSeparatorComboBox.setVisible(!copyPanel.formattedCheckBox.isSelected());
			copyPanel.coloredCheckBox.setVisible(copyPanel.formattedCheckBox.isSelected());
			copyPanel.alignedCheckBox.setVisible(copyPanel.formattedCheckBox.isSelected());
			copyPanel.tabContentPanel.columnSeparatorComboBox.setVisible(!copyPanel.formattedCheckBox.isSelected());
			copyPanel.cellpaddingLabel.setVisible(copyPanel.formattedCheckBox.isSelected());
			copyPanel.cellpaddingComboBox.setVisible(copyPanel.formattedCheckBox.isSelected());
		});
		l.itemStateChanged(null);
		copyPanel.tabContentPanel.textSortedStateLabel.setVisible(false);
		copyPanel.tabContentPanel.headerCheckBox.addItemListener(e -> copyPanel.updatePreview());
		copyPanel.tabContentPanel.rotateCheckBox.addItemListener(e -> copyPanel.updatePreview());
		copyPanel.cellpaddingComboBox.addItemListener(e -> copyPanel.updatePreview());

		Point off = new Point();
		off = SwingUtilities.convertPoint(copyPanel, off, copyPanel.contentTable.getParent());

		Point mid = new Point(off.x + -window.getContentPane().getLocation().x - window.getInsets().left, off.y + -window.getContentPane().getLocation().y - window.getInsets().top); // jTable.getParent().getWidth() / 2 - frame.getWidth() / 2, jTable.getParent().getHeight() / 2 - frame.getHeight() / 2);
		SwingUtilities.convertPointToScreen(mid, jTable.getParent());

		shrink(window, copyPanel);

		int maxX = owner.getX() + owner.getWidth() - window.getWidth() - 4;
		mid.x = Math.min(mid.x, maxX);
		int maxY = owner.getY() + owner.getHeight() - window.getHeight() - 4;
		mid.y = Math.min(mid.y, maxY);
		mid.x = Math.max(mid.x, owner.getX() + 4);
		mid.y = Math.max(mid.y, owner.getY() + 4);
		
		window.setLocation(mid);
		window.setSize(window.getWidth() + Math.max(0, Math.min(300, maxX - window.getX())), window.getHeight() + Math.max(0, Math.min(500, maxY - window.getY())));

		shrink(window, copyPanel);
		
		copyPanel.updatePreview();
		Timer timer = new Timer(100, e -> {
			copyPanel.contentTable.repaint();
		});
		timer.setInitialDelay(100);
		timer.setRepeats(true);
		timer.start();
		window.addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosed(WindowEvent e) {
				timer.stop();
				if (!silent) {
					lastAligned = copyPanel.alignedCheckBox.isSelected();
					lastColored = copyPanel.coloredCheckBox.isSelected();
					lastFormatted = copyPanel.formattedCheckBox.isSelected();
					lastCellpadding = copyPanel.cellpaddingComboBox.getSelectedItem();
				}
			}
		});
		Dimension size = window.getSize();
		window.pack();
		window.revalidate();
		window.setSize(Math.max(size.width, window.getWidth()), size.height);
		if (!silent) {
			window.setVisible(true);
		} else {
			window.dispose();
			copyPanel.copyToClipboard(jTable);
		}
	}

	private static void shrink(JDialog window, ExtendetCopyPanel copyPanel) {
		window.revalidate();
		int h = copyPanel.jScrollPane1.getHeight() - copyPanel.contentTable.getHeight() - 48;
		if (h > 0) {
			h = window.getHeight() - h;
			window.setSize(window.getWidth(), Math.max(h, 364));
			window.revalidate();
		}
		int w = copyPanel.jScrollPane1.getWidth() - copyPanel.contentTable.getWidth() - 48;
		if (w > 0) {
			w = window.getWidth() - w;
			window.setSize(Math.max(w, 100), window.getHeight());
			window.revalidate();
		}
	}

    private void initContentTable(JTable jTable, boolean allColumnsSelected, boolean columnNamesInFirstRow) {
    	recreateContentTable();
    	
    	Object[] colNames = new Object[jTable.getColumnCount()];
    	for (int i = 0; i < colNames.length; ++i) {
    		colNames[i] = jTable.getColumnName(i);
    	}
    	RowSorter<? extends TableModel> rowSorter = jTable.getRowSorter();
    	Object[][] data = new Object[jTable.getRowCount()][];
    	for (int row = 0; row < data.length; ++row) {
    		data[row] = new Object[colNames.length];
    		int ri = rowSorter == null? row : rowSorter.convertRowIndexToModel(row);
    		for (int col = 0; col < colNames.length; ++col) {
    			data[row][col] = jTable.getModel().getValueAt(ri, col);
    		}
    	}
		DefaultTableModel dtm = new DefaultTableModel(data, colNames) {
			@Override
			public boolean isCellEditable(int row, int column) {
				return false;
			}
    	};
    	contentTable.setModel(dtm);
    	contentTable.setIntercellSpacing(new Dimension(0, 0));
    	contentTable.setAutoCreateRowSorter(true);
    	
    	contentTable.setAutoCreateColumnsFromModel(false);
    	contentTable.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
    	
    	for (int i = 0; i < colNames.length; ++i) {
    		contentTable.getColumnModel().getColumn(i).setPreferredWidth(jTable.getColumnModel().getColumn(i).getWidth());
    		colNames[i] = jTable.getColumnName(i);
    	}
    	
    	jScrollPane1.getViewport().setViewPosition(((JViewport) jTable.getParent()).getViewPosition());
    	
    	contentTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    	contentTable.setRowSelectionAllowed(true);
    	contentTable.setColumnSelectionAllowed(true);
    	contentTable.setCellSelectionEnabled(true);
    	contentTable.setName("contentTable");
		contentTable.setDefaultRenderer(Object.class, new TableCellRenderer() {
			@Override
			public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected,
					boolean hasFocus, int row, int column) {
				int dmColumn = column;
				try {
					Component render = jTable.getCellRenderer(row, dmColumn).getTableCellRendererComponent(contentTable,
							value, false, hasFocus, row, dmColumn);
					if (render instanceof JLabel) {
						if (isSelected) {
							if (UIUtil.TABLE_BACKGROUND_COLOR_1.equals(render.getBackground())) {
								render.setBackground(UIUtil.TABLE_BG1SELECTED);
							} else {
								render.setBackground(UIUtil.TABLE_BG2SELECTED);
							}
						}
						if ("found".equals(render.getName())) {
							Color background = render.getBackground();
							render.setBackground(new Color(Math.max((int) (background.getRed()), 0),
									Math.max((int) (background.getGreen() * 0.90), 0),
									Math.max((int) (background.getBlue() * 0.91), 0), background.getAlpha()));
						}
					}
					return render;
				} catch (Exception e) {
					LogUtil.warn(e);
					return new JLabel();
				}

			}
		});
		try {
			((DefaultTableCellRenderer) contentTable.getTableHeader().getDefaultRenderer()).setHorizontalAlignment(JLabel.LEFT);
			for (int i = 0; i < jTable.getColumnModel().getColumnCount(); ++i) {
				TableColumn col = jTable.getColumnModel().getColumn(i);
				contentTable.getColumnModel().getColumn(i).setModelIndex(col.getModelIndex());
			}
			Map<Object, Integer> mIndex = new HashMap<>();
			for (int i = 0; i < colNames.length; ++i) {
				mIndex.put(colNames[i], contentTable.getColumnModel().getColumn(i).getModelIndex());	
			}
			Arrays.sort(colNames, (a, b) -> mIndex.get(a) - mIndex.get(b));
			dtm.setColumnIdentifiers(colNames);
			contentTable.getSelectionModel();
			for (int r: jTable.getSelectedRows()) {
				contentTable.addRowSelectionInterval(r, r);
			}
			if (allColumnsSelected) {
				contentTable.setColumnSelectionInterval(0, contentTable.getColumnCount() - 1);
			} else {
				for (int ci: jTable.getSelectedColumns()) {
					contentTable.addColumnSelectionInterval(ci, ci);
				}
			}
		} catch (Exception e) {
			LogUtil.warn(e);
		}
		
		contentTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			@Override
			public void valueChanged(ListSelectionEvent e) {
				updatePreview();
			}
		});
		contentTable.getColumnModel().addColumnModelListener(new TableColumnModelListener() {
			@Override
			public void columnSelectionChanged(ListSelectionEvent e) {
				updatePreview();
			}
			@Override
			public void columnRemoved(TableColumnModelEvent e) {
				updatePreview();
			}
			@Override
			public void columnMoved(TableColumnModelEvent e) {
				updatePreview();
			}
			@Override
			public void columnMarginChanged(ChangeEvent e) {
				updatePreview();
			}
			@Override
			public void columnAdded(TableColumnModelEvent e) {
				updatePreview();
			}
		});
		if (columnNamesInFirstRow) {
			contentTable.getColumnModel().removeColumn(contentTable.getColumnModel().getColumn(0));
		}
		
		formattedScrollPane.getHorizontalScrollBar().setUnitIncrement(32);
		formattedScrollPane.getVerticalScrollBar().setUnitIncrement(32);
	}

	private void recreateContentTable() {
		contentTable = new JTable() {
			@Override
			public void paint(Graphics graphics) {
				super.paint(graphics);
				if (!(graphics instanceof Graphics2D)) {
					return;
				}
				Rectangle visRect = getVisibleRect();

				Graphics2D g2d = (Graphics2D) graphics;
				g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
				g2d.setStroke(new BasicStroke(1));

				int left = -1;
				int last = -1;
				List<Pair<Integer, Integer>> intervall = new ArrayList<Pair<Integer, Integer>>();
				for (int si : getSelectedColumns()) {
					if (left < 0) {
						left = si;
					} else if (last < si - 1) {
						intervall.add(new Pair<Integer, Integer>(left, last));
						left = si;
					}
					last = si;
				}
				if (left >= 0) {
					intervall.add(new Pair<Integer, Integer>(left, last));
				}

				int top = -1;
				last = -1;
				List<Pair<Integer, Integer>> rowIntervall = new ArrayList<Pair<Integer, Integer>>();
				for (int si : getSelectedRows()) {
					if (top < 0) {
						top = si;
					} else if (last < si - 1) {
						rowIntervall.add(new Pair<Integer, Integer>(top, last));
						top = si;
					}
					last = si;
				}
				if (top >= 0) {
					rowIntervall.add(new Pair<Integer, Integer>(top, last));
				}
				int x[] = new int[2];
				int y[] = new int[2];
				for (Pair<Integer, Integer> iv : intervall) {
					for (Pair<Integer, Integer> rowIv : rowIntervall) {
						int[] selectedRows = getSelectedRows();
						if (selectedRows.length > 0) {
							x[0] = Integer.MAX_VALUE;
							y[0] = Integer.MAX_VALUE;
							x[1] = Integer.MIN_VALUE;
							y[1] = Integer.MIN_VALUE;
							Rectangle r = getCellRect(rowIv.a, iv.a, false);
							x[0] = Math.min((int) r.getMinX(), x[0]);
							y[0] = Math.min((int) r.getMinY(), y[0]);
							r = getCellRect(rowIv.b, iv.b, false);
							x[1] = Math.max((int) r.getMaxX(), x[1]);
							y[1] = Math.max((int) r.getMaxY(), y[1]);
							x[0] = (int) Math.max(visRect.getMinX(), x[0]) + 1;
							y[0] = (int) Math.max(visRect.getMinY(), y[0]);
							x[1] = (int) Math.min(visRect.getMaxX(), x[1]) - 2;
							y[1] = (int) Math.min(visRect.getMaxY() - 1, y[1]);
							if (x[0] < x[1] && y[0] < y[1]) {
								g2d.setColor(UIUtil.BG_FLATMOUSEOVER);
								BasicStroke stroke = new BasicStroke();
								g2d.setStroke(stroke);
								g2d.drawRoundRect(x[0], y[0], x[1] - x[0], y[1] - y[0], 8, 8);
								g2d.setColor(new Color(0, 0, 200, 100));
								g2d.setStroke(new BasicStroke(stroke.getLineWidth(), stroke.getEndCap(), stroke.getLineJoin(),
										stroke.getMiterLimit(), new float[] { 11f, 5f },
										(float) ((System.currentTimeMillis() / 50.0 * 1.1) % 16)));
								g2d.drawRoundRect(x[0], y[0], x[1] - x[0], y[1] - y[0], 8, 8);
							}
						}
					}
				}
			}
		};
		jScrollPane1.setViewportView(contentTable);
		InputMap im = contentTable.getInputMap();
		Object key = "copyClipboard";
		im.put(KS_COPY_TO_CLIPBOARD, key);
		ActionMap am = contentTable.getActionMap();
		Action a = new AbstractAction() {
			@Override
			public void actionPerformed(ActionEvent e) {
				copyToClipboard(ExtendetCopyPanel.this);
			}
		};
		am.put(key, a);
	}

	private void updatePreview() {
		if (!updatePending) {
			UIUtil.invokeLater(() -> {
				updatePending = false;
				if (!formattedCheckBox.isSelected()) {
					tabContentPanel.updateTextView(contentTable);
				} else {
					int cols = 1 + (tabContentPanel.rotateCheckBox.isSelected()? contentTable.getSelectedRows().length : contentTable.getSelectedColumnCount());
					int maxColumns = 50;
					int maxRows = 30;
					if (maxColumns > cols) {
						maxRows = (maxColumns * maxRows) / cols;
						maxColumns = cols;
					}
					Integer cellpadding = null;
					Object si = cellpaddingComboBox.getSelectedItem();
					if (si instanceof Number) {
						cellpadding = ((Number) si).intValue();
					}
					boolean[] stopped = new boolean[2];
					formattedContentLabel.setText(tabContentPanel.getHTMLContent(contentTable, alignedCheckBox.isSelected(), coloredCheckBox.isSelected(), cellpadding, maxColumns, maxRows, stopped).replaceFirst("<table", "<table cellspacing=\"0\""));
			        stopRowLabel.setVisible(stopped[1]);
			        stopColumnLabel.setVisible(stopped[0]);
			        stopBothLabel.setVisible(stopped[0] || stopped[1]);
				}
				((CardLayout) previewPanel.getLayout()).show(previewPanel, formattedCheckBox.isSelected()? "formatted" : "plain");
			});
		}
	}

	private boolean updatePending = false;
	
    private TabContentPanel tabContentPanel;
    
	private ExtendetCopyPanel(boolean columnNamesInFirstRow) {
        initComponents(); UIUtil.initComponents(this);
        jToolBar1.setFloatable(false);
        maximizeButton.setIcon(maximizeIcon);
        selectAllButton.setIcon(selectIcon);
        copyButton.setIcon(copyIcon);
        closeCloseButton.setIcon(cancelIcon);
        copyCloseButton.setIcon(copyCloseIcon);
        
        stopRowLabel.setVisible(false);
        stopColumnLabel.setVisible(false);
        stopBothLabel.setVisible(false);
        
        DefaultComboBoxModel<Object> comboBoxModel = new DefaultComboBoxModel<Object>(new Object[] { "None", 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 });
		cellpaddingComboBox.setModel(comboBoxModel);
		cellpaddingComboBox.setSelectedItem(3);
	}

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jSplitPane1 = new javax.swing.JSplitPane();
        jPanel1 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        contentTable = new javax.swing.JTable();
        jLabel1 = new javax.swing.JLabel();
        jToolBar1 = new javax.swing.JToolBar();
        selectAllButton = new javax.swing.JButton();
        maximizeButton = new javax.swing.JToggleButton();
        jPanel2 = new javax.swing.JPanel();
        previewPanel = new javax.swing.JPanel();
        plainPanel = new javax.swing.JPanel();
        formattedScrollPane = new javax.swing.JScrollPane();
        jPanel3 = new javax.swing.JPanel();
        formattedContentLabel = new javax.swing.JLabel();
        jPanel4 = new javax.swing.JPanel();
        stopRowLabel = new javax.swing.JLabel();
        stopColumnLabel = new javax.swing.JLabel();
        stopBothLabel = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        panel = new javax.swing.JPanel();
        copyCloseButton = new javax.swing.JButton();
        closeCloseButton = new javax.swing.JButton();
        copyButton = new javax.swing.JButton();
        jPanel5 = new javax.swing.JPanel();
        controlsPanel = new javax.swing.JPanel();
        formattedCheckBox = new javax.swing.JCheckBox();
        controlsPanel2 = new javax.swing.JPanel();
        alignedCheckBox = new javax.swing.JCheckBox();
        coloredCheckBox = new javax.swing.JCheckBox();
        cellpaddingLabel = new javax.swing.JLabel();
        cellpaddingComboBox = new javax.swing.JComboBox<>();

        setLayout(new java.awt.GridBagLayout());

        jSplitPane1.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        jSplitPane1.setResizeWeight(1.0);
        jSplitPane1.setOneTouchExpandable(true);

        jPanel1.setLayout(new java.awt.GridBagLayout());

        contentTable.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null}
            },
            new String [] {
                "Title 1", "Title 2", "Title 3", "Title 4"
            }
        ));
        jScrollPane1.setViewportView(contentTable);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel1.add(jScrollPane1, gridBagConstraints);

        jLabel1.setFont(jLabel1.getFont().deriveFont(jLabel1.getFont().getSize()-1f));
        jLabel1.setForeground(java.awt.Color.gray);
        jLabel1.setText("<html><i>Ctrl-Click:</i> Toggle row/column selection.<i> Ctrl-A:</i> Select All.</html>");
        jLabel1.setToolTipText("<html><i>Ctrl-Click: </i>Toggle row/column selection.<br>\n<i>Ctrl-A:</i> Select All</html>");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 6, 0, 0);
        jPanel1.add(jLabel1, gridBagConstraints);

        jToolBar1.setRollover(true);

        selectAllButton.setText("Select all");
        selectAllButton.setFocusable(false);
        selectAllButton.setHorizontalTextPosition(javax.swing.SwingConstants.RIGHT);
        selectAllButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        selectAllButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                selectAllButtonActionPerformed(evt);
            }
        });
        jToolBar1.add(selectAllButton);

        maximizeButton.setText("Maximize");
        maximizeButton.setToolTipText("Maximize size of dialog");
        maximizeButton.setHorizontalTextPosition(javax.swing.SwingConstants.LEFT);
        maximizeButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        maximizeButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                maximizeButtonActionPerformed(evt);
            }
        });
        jToolBar1.add(maximizeButton);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 2;
        jPanel1.add(jToolBar1, gridBagConstraints);

        jSplitPane1.setLeftComponent(jPanel1);

        jPanel2.setLayout(new java.awt.GridBagLayout());

        previewPanel.setLayout(new java.awt.CardLayout());

        plainPanel.setLayout(new java.awt.BorderLayout());
        previewPanel.add(plainPanel, "plain");

        jPanel3.setBackground(java.awt.Color.white);
        jPanel3.setLayout(new java.awt.GridBagLayout());

        formattedContentLabel.setText("jLabel2");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel3.add(formattedContentLabel, gridBagConstraints);

        jPanel4.setLayout(null);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel3.add(jPanel4, gridBagConstraints);

        stopRowLabel.setFont(stopRowLabel.getFont().deriveFont((stopRowLabel.getFont().getStyle() | java.awt.Font.ITALIC)));
        stopRowLabel.setForeground(java.awt.Color.blue);
        stopRowLabel.setText("Preview ends here...");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        jPanel3.add(stopRowLabel, gridBagConstraints);

        stopColumnLabel.setFont(stopColumnLabel.getFont().deriveFont((stopColumnLabel.getFont().getStyle() | java.awt.Font.ITALIC)));
        stopColumnLabel.setForeground(java.awt.Color.blue);
        stopColumnLabel.setText("Preview ends here...");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        jPanel3.add(stopColumnLabel, gridBagConstraints);

        stopBothLabel.setFont(stopBothLabel.getFont().deriveFont((stopBothLabel.getFont().getStyle() | java.awt.Font.ITALIC)));
        stopBothLabel.setForeground(java.awt.Color.blue);
        stopBothLabel.setText("Preview ends here...");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 2;
        jPanel3.add(stopBothLabel, gridBagConstraints);

        formattedScrollPane.setViewportView(jPanel3);

        previewPanel.add(formattedScrollPane, "formatted");

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel2.add(previewPanel, gridBagConstraints);

        jLabel2.setFont(jLabel2.getFont().deriveFont(jLabel2.getFont().getSize()+4f));
        jLabel2.setForeground(java.awt.Color.blue);
        jLabel2.setText("Preview");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.insets = new java.awt.Insets(2, 4, 4, 0);
        jPanel2.add(jLabel2, gridBagConstraints);

        jSplitPane1.setRightComponent(jPanel2);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(jSplitPane1, gridBagConstraints);

        panel.setLayout(new java.awt.GridBagLayout());

        copyCloseButton.setText("Copy and CLose");
        copyCloseButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                copyCloseButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 5;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 4);
        panel.add(copyCloseButton, gridBagConstraints);

        closeCloseButton.setText("CLose");
        closeCloseButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                closeCloseButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 6;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 4);
        panel.add(closeCloseButton, gridBagConstraints);

        copyButton.setText("Copy to Clipboard");
        copyButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                copyButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHEAST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 4);
        panel.add(copyButton, gridBagConstraints);

        jPanel5.setLayout(new java.awt.GridBagLayout());

        controlsPanel.setLayout(new java.awt.GridBagLayout());

        formattedCheckBox.setSelected(true);
        formattedCheckBox.setText("Formatted");
        formattedCheckBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                formattedCheckBoxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 8);
        controlsPanel.add(formattedCheckBox, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        jPanel5.add(controlsPanel, gridBagConstraints);

        controlsPanel2.setLayout(new java.awt.GridBagLayout());

        alignedCheckBox.setSelected(true);
        alignedCheckBox.setText("Aligned");
        alignedCheckBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                alignedCheckBoxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 8);
        controlsPanel2.add(alignedCheckBox, gridBagConstraints);

        coloredCheckBox.setSelected(true);
        coloredCheckBox.setText("Colored");
        coloredCheckBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                coloredCheckBoxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 8);
        controlsPanel2.add(coloredCheckBox, gridBagConstraints);

        cellpaddingLabel.setText("  Cellpadding ");
        controlsPanel2.add(cellpaddingLabel, new java.awt.GridBagConstraints());

        cellpaddingComboBox.setModel(new javax.swing.DefaultComboBoxModel<>(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 8);
        controlsPanel2.add(cellpaddingComboBox, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        jPanel5.add(controlsPanel2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 8;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 16);
        panel.add(jPanel5, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 1, 0);
        add(panel, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    private Point oldLoc;
    private Dimension oldSize;

    private void maximizeButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_maximizeButtonActionPerformed
        Window dialog = SwingUtilities.getWindowAncestor(this);
        if (dialog != null) {
        	Window owner = dialog.getOwner();
        	if (owner != null) {
        		if (oldLoc != null) {
        			dialog.setLocation(oldLoc);
        			dialog.setSize(oldSize);
        			oldLoc = null;
        			oldSize = null;
        	        maximizeButton.setIcon(maximizeIcon);
        		} else {
        			oldLoc = dialog.getLocation();
        			oldSize = dialog.getSize();
 	        		Insets insets = owner.getInsets();
 	        		int b = 32;
 	        		insets.top += b;
 	        		insets.bottom += b;
 	        		insets.left += b;
 	        		insets.right += b;
					dialog.setLocation(owner.getLocation().x + insets.left, owner.getLocation().y + insets.top - 1);
	        		dialog.setSize(owner.getSize().width - insets.left - insets.right + 1, owner.getSize().height - insets.top - insets.bottom);
	                maximizeButton.setIcon(unmaximizeIcon);
        		}
        	}
        }
    }//GEN-LAST:event_maximizeButtonActionPerformed

    private void formattedCheckBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_formattedCheckBoxActionPerformed
        updatePreview();
    }//GEN-LAST:event_formattedCheckBoxActionPerformed

    private void alignedCheckBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_alignedCheckBoxActionPerformed
    	updatePreview();
    }//GEN-LAST:event_alignedCheckBoxActionPerformed

    private void coloredCheckBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_coloredCheckBoxActionPerformed
    	updatePreview();
    }//GEN-LAST:event_coloredCheckBoxActionPerformed

    private void selectAllButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_selectAllButtonActionPerformed
        try {
        	UIUtil.setWaitCursor(this);
        	contentTable.selectAll();
        } finally {
        	UIUtil.resetWaitCursor(this);
        }
    }//GEN-LAST:event_selectAllButtonActionPerformed

    private void copyButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_copyButtonActionPerformed
    	copyToClipboard(this);
    }//GEN-LAST:event_copyButtonActionPerformed

    private void copyCloseButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_copyCloseButtonActionPerformed
    	copyToClipboard(this);
    	Window window = SwingUtilities.getWindowAncestor(this);
    	window.setVisible(false);
    	window.dispose();
    }//GEN-LAST:event_copyCloseButtonActionPerformed

    private void closeCloseButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_closeCloseButtonActionPerformed
    	Window window = SwingUtilities.getWindowAncestor(this);
    	window.setVisible(false);
    	window.dispose();
    }//GEN-LAST:event_closeCloseButtonActionPerformed

    private void copyToClipboard(Component cursorSubject) {
    	try {
    		UIUtil.setWaitCursor(cursorSubject);
    		String html = null;
			boolean[] stopped = new boolean[2];
			if (formattedCheckBox.isSelected()) {
				Integer cellpadding = null;
				Object si = cellpaddingComboBox.getSelectedItem();
				if (si instanceof Number) {
					cellpadding = ((Number) si).intValue();
				}
				html = tabContentPanel.getHTMLContent(contentTable, alignedCheckBox.isSelected(), coloredCheckBox.isSelected(), cellpadding, Integer.MAX_VALUE, Integer.MAX_VALUE, stopped);
				html = html.replaceFirst("<html>", "<!DOCTYPE html>\n"
					+ "<html>\n"
					+ "<head>\n"
					+ "<meta charset=\"UTF-8\"/>\n"
					+ "</head>\n");
    		}
    		HtmlSelection htmlSelection = new HtmlSelection(html, tabContentPanel.getPlainContent(contentTable, alignedCheckBox.isSelected(), coloredCheckBox.isSelected(), Integer.MAX_VALUE, Integer.MAX_VALUE, stopped));
		    UIUtil.setClipboardContent(htmlSelection);
    	} finally {
    		UIUtil.resetWaitCursor(cursorSubject);
    	}
	}

    private static class HtmlSelection implements Transferable, ClipboardOwner {
	
	    private List<DataFlavor> htmlFlavors = new ArrayList<>(3);
	    private DataFlavor htmlDataFlavor;
	    
	    private String html;
	    private String plainText;
	
	    public HtmlSelection(String html, String plainText) {
	        this.html = html;
	        this.plainText = plainText;
	        try {
	        	if (plainText != null) {
	        		htmlFlavors.add(DataFlavor.stringFlavor);
	        	}
	        	if (html != null) {
		        	htmlFlavors.add(htmlDataFlavor = new DataFlavor("text/html;charset=unicode;class=java.lang.String"));
	        	}
			} catch (ClassNotFoundException e) {
				LogUtil.warn(e);
			}
	    }
	
	    public DataFlavor[] getTransferDataFlavors() {
	        return (DataFlavor[]) htmlFlavors.toArray(new DataFlavor[htmlFlavors.size()]);
	    }
	
	    public boolean isDataFlavorSupported(DataFlavor flavor) {
	        return htmlFlavors.contains(flavor);
	    }
	
	    public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException {
	
	        String toBeExported = plainText;
	        if (flavor == htmlDataFlavor) {
	            toBeExported = html;
	        }
	
	        if (String.class.equals(flavor.getRepresentationClass())) {
	            return toBeExported;
	        }
	        throw new UnsupportedFlavorException(flavor);
	    }
	
	    public void lostOwnership(Clipboard clipboard, Transferable contents) {
	    }
	}

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JCheckBox alignedCheckBox;
    private javax.swing.JComboBox<Object> cellpaddingComboBox;
    private javax.swing.JLabel cellpaddingLabel;
    private javax.swing.JButton closeCloseButton;
    private javax.swing.JCheckBox coloredCheckBox;
    private javax.swing.JTable contentTable;
    private javax.swing.JPanel controlsPanel;
    private javax.swing.JPanel controlsPanel2;
    private javax.swing.JButton copyButton;
    private javax.swing.JButton copyCloseButton;
    private javax.swing.JCheckBox formattedCheckBox;
    private javax.swing.JLabel formattedContentLabel;
    private javax.swing.JScrollPane formattedScrollPane;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JSplitPane jSplitPane1;
    private javax.swing.JToolBar jToolBar1;
    private javax.swing.JToggleButton maximizeButton;
    private javax.swing.JPanel panel;
    private javax.swing.JPanel plainPanel;
    private javax.swing.JPanel previewPanel;
    private javax.swing.JButton selectAllButton;
    private javax.swing.JLabel stopBothLabel;
    private javax.swing.JLabel stopColumnLabel;
    private javax.swing.JLabel stopRowLabel;
    // End of variables declaration//GEN-END:variables
    
	private static ImageIcon maximizeIcon;
	private static ImageIcon unmaximizeIcon;
	private static ImageIcon selectIcon;
	private static ImageIcon copyIcon;
	private static ImageIcon cancelIcon;
	private static ImageIcon copyCloseIcon;
	
	static {
		// load images
		maximizeIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/maximizec.png"));
		unmaximizeIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/unmaximize.png"));
		selectIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/select.png"));
        copyIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/copy.png"));
        copyCloseIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/copyclose.png"));
        cancelIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/buttoncancel.png"));
	}
	
	// TODO offer other flavors: JSON, XML, ...
	
}
