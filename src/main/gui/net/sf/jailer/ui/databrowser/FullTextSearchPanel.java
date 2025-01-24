/*
 * Copyright 2007 - 2025 Ralf Wisser.
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
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.NavigableMap;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.border.Border;
import javax.swing.border.LineBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.RowSorterEvent;
import javax.swing.event.RowSorterListener;
import javax.swing.event.TableColumnModelEvent;
import javax.swing.event.TableColumnModelListener;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableModel;

import net.sf.jailer.ui.Colors;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.sqlconsole.ColumnsTable;
import net.sf.jailer.util.LogUtil;


/**
 * Full Text Search Panel.
 * 
 * @author Ralf Wisser
 */
public class FullTextSearchPanel extends javax.swing.JPanel {

	private final static int MAX_OCCURRENCES = 100_000;

	private final JTable table;
	
    /**
     * Creates new form FullTextSearchPanel
     * @param table 
     */
    public FullTextSearchPanel(JTable table) {
    	this.table = table;
    	
        initComponents(); UIUtil.initComponents(this);
        jToolBar1.setFloatable(false);

        setVisible(false);

        origBackground = searchField.getBackground();
		
        closeButton.setText(null);
        closeButton.setIcon(UIUtil.scaleIcon(UIUtil.scaleIcon(closeButton, closeIcon), 0.8));
        prevButton.setText(null);
        prevButton.setIcon(UIUtil.scaleIcon(prevButton, prevIcon));
        nextButton.setText(null);
        nextButton.setIcon(UIUtil.scaleIcon(nextButton, nextIcon));
        
        warnLabel.setText(null);
        warnLabel.setIcon(UIUtil.scaleIcon(warnLabel, warnIcon));
        warnLabel.setVisible(false);
        UIUtil.setTrailingComponent(searchField, warnLabel);
        
        searchField.setMinimumSize(new Dimension(200, searchField.getMinimumSize().height));
        searchField.setPreferredSize(new Dimension(200, searchField.getMinimumSize().height));
        
        prevButton.addActionListener(e -> prev());
        nextButton.addActionListener(e -> next());
        
        KeyListener keyListener = new KeyListener() {
			@Override
			public void keyTyped(KeyEvent e) {
				if (e.getKeyChar() == KeyEvent.VK_ESCAPE) {
					close();
				} else if (e.getKeyChar() == KeyEvent.VK_ENTER) {
					next();
				}
			}
			@Override
			public void keyReleased(KeyEvent e) {
			}
			@Override
			public void keyPressed(KeyEvent e) {
			}
		};
		searchField.addKeyListener(keyListener);
		nextButton.addKeyListener(keyListener);
		prevButton.addKeyListener(keyListener);
		closeButton.addKeyListener(keyListener);
		
		searchField.getDocument().addDocumentListener(new DocumentListener() {
			long lastFTSearchDuration = -1;
			long lastFTSearchTimeEnd = -1;
			boolean slowMode;
			long numCells;
			Timer timer = null;
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
				int numC = table.getColumnCount() * table.getModel().getRowCount();
				if (numC != numCells) {
					numCells = numC;
					slowMode = numCells > 500 * 80;
				}
				if (lastFTSearchDuration >=500) {
					slowMode = true;
				}
				updateLater(slowMode? 500 : 200);
			}
			private void updateLater(int delay) {
				if (timer != null) {
					timer.stop();
					timer = null;
				}
				timer = new Timer(delay, e -> updateFullTextSearchPanel());
				timer.setRepeats(false);
				timer.start();
			}
			
			private void updateFullTextSearchPanel() {
				long lastFTSearchTimeStart = System.currentTimeMillis();
				FullTextSearchPanel.this.update(searchField.getText(), true);
				scrollToCurrentPosition();
				lastFTSearchTimeEnd = System.currentTimeMillis();
				lastFTSearchDuration = lastFTSearchTimeEnd - lastFTSearchTimeStart;
			}
		});
		
		table.addPropertyChangeListener("model", new PropertyChangeListener() {
			@Override
			public void propertyChange(PropertyChangeEvent evt) {
				if (!updatePending) {
					updatePending = true;
					UIUtil.invokeLater(12, () -> {
						updatePending = false;
						if (isVisible()) {
							update(searchField.getText(), false);
						}
					});
				}
			}
		});
		
		if (table.getRowSorter() != null) {
			table.getRowSorter().addRowSorterListener(new RowSorterListener() {
				@Override
				public void sorterChanged(RowSorterEvent e) {
					if (isVisible()) {
						update(searchField.getText(), false);
						updateCurrentPositionFromVisibleRect();
					}
				}
			});
		}
		
		table.getColumnModel().addColumnModelListener(new TableColumnModelListener() {
			@Override
			public void columnSelectionChanged(ListSelectionEvent e) {
				modelChanged();
			}
			@Override
			public void columnRemoved(TableColumnModelEvent e) {
				modelChanged();
			}
			@Override
			public void columnMoved(TableColumnModelEvent e) {
				modelChanged();
			}
			@Override
			public void columnMarginChanged(ChangeEvent e) {
				modelChanged();
			}
			@Override
			public void columnAdded(TableColumnModelEvent e) {
				modelChanged();
			}
			void modelChanged() {
				if (!updatePending) {
					updatePending = true;
					UIUtil.invokeLater(12, () -> {
						updatePending = false;
						if (isVisible()) {
							update(searchField.getText(), false);
							updateCurrentPositionFromVisibleRect();
						}
					});
				}
			}
		});
		
		table.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			boolean pending = false;
			@Override
			public void valueChanged(ListSelectionEvent e) {
				if (!pending) {
					pending = true;
					UIUtil.invokeLater(() -> {
						pending = false;
						if (isVisible()) {
							updateCurrentPositionFromVisibleRect();
						}
					});
				}
			}
		});
		
		if (UIUtil.plaf.isFlat) {
			searchField.putClientProperty("JTextField.placeholderText", "Full Text Search");
		}
    }

    private boolean updatePending = false;

	public boolean openSilently() {
		if (!isVisible()) {
			setVisible(true);
			searchField.setText("");
			update(searchField.getText(), false);
			searchField.grabFocus();
			return true;
		} else {
			return false;
		}
	}
	
	public boolean isSilent() {
		return searchField.getText().isEmpty();
	}
		
	public void open() {
		if (!isVisible()) {
			setVisible(true);
			update(searchField.getText(), false);
		} else {
			searchField.selectAll();
		}
		searchField.grabFocus();
	}

	public void openPermanently() {
		if (!isVisible()) {
			setVisible(true);
			update(searchField.getText(), false);
		}
		closeButton.setVisible(false);
	}

	public void close() {
		if (closeButton.isVisible()) {
			setVisible(false);
			update();
		} else {
			searchField.setText("");
		}
	}

	public void updateFromPredecessor(FullTextSearchPanel predecessor) {
		try {
			inUpdate = true;
			searchField.setText(predecessor.searchField.getText());
		} finally {
			inUpdate = false;
		}
		currentPosition = predecessor.currentPosition;
		searchField.setBackground(predecessor.searchField.getBackground());
		prevButton.setEnabled(predecessor.prevButton.isEnabled());
		nextButton.setEnabled(predecessor.nextButton.isEnabled());
		warnLabel.setVisible(predecessor.warnLabel.isVisible());

		if (predecessor.isVisible()) {
			setVisible(true);
			update(searchField.getText(), false);
		}
		updateErrorState();
	}

	private Pattern whitSpacePattern = Pattern.compile("\\s|\\h");
	private String lastText = null;
	private String lastTextReplaced = null;
	private Pattern extSTPattern = null;
	private String extSTText = null;
	
	private String extendedSearchText(String text, String item) {
		if (text == lastText && text != null) {
			text = lastTextReplaced;
		} else {
			lastText = text;
			text = whitSpacePattern.matcher(text).replaceAll(" ");
			lastTextReplaced = text;
		}
		String searchText = text.toUpperCase(Locale.ENGLISH);
		if (!searchText.contains("*") && !searchText.contains("?")) {
			return searchText.trim();
		}
		
		if (!text.equals(extSTText)) {
			boolean withPrefix = !text.startsWith(" ");
			boolean withSuffix = !text.endsWith(" ");
			String reg = (withPrefix? ".*?" : "") + "(\\Q" +
						text.trim().replace("?", "\\E.\\Q").replace("*", "\\E.*\\Q") +
						"\\E)" +
						(withSuffix? ".*?" : "");
			
			extSTText = reg;
			extSTPattern = Pattern.compile(reg, Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE);
		}
		
		Matcher matcher = extSTPattern.matcher(item);
		
		if (matcher.matches()) {
			return matcher.group(1);
		} else {
			return searchText.trim();
		}
	}
	
	private void update(String searchText, boolean setCurrentPosition) {
		if (inUpdate) {
			return;
		}
		Component root = SwingUtilities.getWindowAncestor(this);
		if (root == null) {
			root = this;
		}
		try {
			UIUtil.setWaitCursor(root);
			inUpdate = true;
			
			boolean wasOk = !markedValues.isEmpty() || lastSearchText == null || lastSearchText.trim().isEmpty();
			lastSearchText = searchText;
			
			markedValues.clear();
			tooltips.clear();
			markedValuePerPosition.clear();
			ordinalPerViewPosition.clear();
			viewToModelPosition.clear();
			
			String searchTextTrim = searchText.trim();
			if (!searchTextTrim.isEmpty()) {
				TableModel dm = table.getModel();
				TableCellRenderer renderer = table.getDefaultRenderer(Object.class);
				
				String searchTextUC = searchText.toUpperCase(Locale.ENGLISH).trim();
				
				int cc = dm.getColumnCount();
				int rc = dm.getRowCount();
				
				Map<Integer, Integer> viewColumn = new HashMap<Integer, Integer>();
				for (int i = 0; i < cc; ++i) {
					viewColumn.put(table.getColumnModel().getColumn(i).getModelIndex(), i);
				}

				boolean stop = false;
				for (int y = 0; y < rc; ++y) {
					for (int x = 0; x < cc; ++x) {
						Object v = dm.getValueAt(y, x);
						if (v != null) {
							searchTextUC = extendedSearchText(searchText, v.toString().trim()).toUpperCase(Locale.ENGLISH);
						}
						if (v != null && !v.toString().toUpperCase(Locale.ENGLISH).contains(searchTextUC)) {
							continue;
						}
						Component comp = renderer.getTableCellRendererComponent(table, dm.getValueAt(y, x), false, false, y, x);
						if (comp instanceof JLabel) {
							String value = ((JLabel) comp).getText();
							if (value != null && value.toUpperCase(Locale.ENGLISH).contains(searchTextUC)) {
								String markedValue = null;
								int i;
								int offset = 0;
								while (offset < value.length() && value.charAt(offset) == ' ') {
									offset += 1;
								}
								String core = value.trim();
								if (searchText.startsWith(" ") && !core.toUpperCase(Locale.ENGLISH).startsWith(searchTextUC)) {
									continue;
								}
								if (searchText.endsWith(" ") && !core.toUpperCase(Locale.ENGLISH).endsWith(searchTextUC)) {
									continue;
								}
								if (searchText.startsWith(" ") && searchText.endsWith(" ") && !core.toUpperCase(Locale.ENGLISH).equals(searchTextUC)) {
									continue;
								}
								i = searchText.endsWith(" ")? core.toUpperCase(Locale.ENGLISH).lastIndexOf(searchTextUC) : core.toUpperCase(Locale.ENGLISH).indexOf(searchTextUC);
								if (i < 0) {
									continue;
								}
								i += offset;
								i = Math.min(i, value.length());
								if (i + searchTextUC.length() <= value.length()) {
									markedValue = UIUtil.toHTMLFragment(value.substring(0, i), 0, false) + "<b><u><font color=" + Colors.HTMLColor_0000D0 + ">" + UIUtil.toHTMLFragment(value.substring(i, i + searchTextUC.length()), 0, false) + "</font></u></b>" + UIUtil.toHTMLFragment(value.substring(i + searchTextUC.length()), 0, false);
								}
								if (markedValue == null) {
									markedValue = "<b><u><font color=" + Colors.HTMLColor_0000D0 + ">" + UIUtil.toHTMLFragment(value, 0, false) + "</font></u></b>";
								}
								markedValue = "<html>" + markedValue + "</html>";
								markedValues.put(value, markedValue);
								Integer vx = viewColumn.get(x);
								if (vx == null) {
									continue;
								}
								int vy = table.getRowSorter() != null? table.getRowSorter().convertRowIndexToView(y) : y;
								if (vy < 0) {
									continue;
								}

								Integer viewPosition = vx + vy * cc;
								Integer modelPosition = x + y * cc;
								markedValuePerPosition.put(viewPosition, markedValue);
								viewToModelPosition.put(viewPosition, modelPosition);
								
								if (markedValuePerPosition.size() > MAX_OCCURRENCES) {
									stop = true;
									break;
								}
							}
						}
					}
					if (stop) {
						break;
					}
				}
				int ord = 1;
				for (Map.Entry<Integer, String> e: markedValuePerPosition.entrySet()) {
					ordinalPerViewPosition.put(e.getKey(), ord++);
				}
			}
			updateErrorState();
			if (setCurrentPosition) {
				if (!searchTextTrim.isEmpty() && markedValuePerPosition.isEmpty() && wasOk) {
					beep();
				}
				
				if (currentPosition == null || !markedValuePerPosition.containsKey(currentPosition)) {
					currentPosition = null;
					if (!markedValuePerPosition.isEmpty()) {
						currentPosition = markedValuePerPosition.firstKey();
					}
				}
			}
		} catch (Throwable t) {
			LogUtil.warn(t);
		} finally {
			inUpdate = false;
			UIUtil.resetWaitCursor(root);
		}
		update();
	}

	private void updateErrorState() {
		String searchText = searchField.getText();
		if (searchText.trim().isEmpty() || !markedValuePerPosition.isEmpty()) {
			searchField.setBackground(origBackground);
			nextButton.setEnabled(true);
			prevButton.setEnabled(true);
		} else {
			searchField.setBackground(Colors.Color_255_220_220);
			nextButton.setEnabled(false);
			prevButton.setEnabled(false);
		}
	}

	private void update() {
		if (markedValuePerPosition.isEmpty()) {
			counterLabel.setText("");
		} else {
			String size = ordinalPerViewPosition.size() > MAX_OCCURRENCES? "more than " + MAX_OCCURRENCES : String.valueOf(ordinalPerViewPosition.size());
			if (currentPosition != null && ordinalPerViewPosition.containsKey(currentPosition)) {
				counterLabel.setText(ordinalPerViewPosition.get(currentPosition) + " / " + size);
			} else {
				counterLabel.setText("" + size);
			}
		}
		table.repaint();
	}
	
	public Component markOccurrence(JLabel render, int x, int y) {
		if (inUpdate || !isVisible()) {
			return render;
		}
		
		String marked = markedValues.get(render.getText());
		if (marked != null) {
			render.setText(marked);
			String toolTipText = tooltips.get(marked);
			if (toolTipText == null) {
				toolTipText = render.getToolTipText();
				if (toolTipText == null || toolTipText.length() < 5000) {
					toolTipText = marked.replace("" + (char) 182, "<br>").replaceFirst("^<html>&nbsp;", "<html>");
					tooltips.put(marked, toolTipText);
				}
			}
			render.setToolTipText(toolTipText);
			render.setBackground(Colors.Color_190_255_180);
			
			if (currentPosition != null && currentPosition == x + y * table.getColumnCount()) {
				JPanel panel = new JPanel(null) {
					Border border = new LineBorder(Colors.Color_0_176_0, 1, true);
					
					@Override
					public void paint(Graphics graphics) {
						super.paint(graphics);
						border.paintBorder(this, graphics, 0, 0, getWidth(), getHeight());
					}
				};
				panel.setToolTipText(toolTipText);
				panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
				panel.add(render);
				return panel;
			}
		}
		
		return render;
	}
	
	private void prev() {
		if (!markedValuePerPosition.isEmpty()) {
			if (currentPosition != null && markedValuePerPosition.containsKey(currentPosition)) {
				currentPosition = markedValuePerPosition.floorKey(currentPosition - 1);
				if (currentPosition == null) {
					beep();
					currentPosition = markedValuePerPosition.lastKey();
				}
			} else {
				currentPosition = markedValuePerPosition.lastKey();
			}
			scrollToCurrentPosition();
		}
	}

	private void next() {
		if (!markedValuePerPosition.isEmpty()) {
			if (currentPosition != null && markedValuePerPosition.containsKey(currentPosition)) {
				currentPosition = markedValuePerPosition.ceilingKey(currentPosition + 1);
				if (currentPosition == null) {
					beep();
					currentPosition = markedValuePerPosition.firstKey();
				}
			} else {
				currentPosition = markedValuePerPosition.firstKey();
			}
		scrollToCurrentPosition();
		}
	}

	private void scrollToCurrentPosition() {
		if (currentPosition != null && table.getColumnCount() > 0) {
			int y = currentPosition / table.getColumnCount();
			int x = currentPosition % table.getColumnCount();
			Rectangle cr = table.getCellRect(y, x, true);
			if (cr != null) {
				int border = table.getRowHeight();
				table.scrollRectToVisible(new Rectangle(cr.x - border , cr.y - border, cr.width + 2 * border, cr.height + 3 * border));
			}
			if (transposedTable != null) {
				cr = transposedTable.getCellRect(x, y + 1, true);
				if (cr != null) {
					int border = transposedTable.getRowHeight();
					transposedTable.scrollRectToVisible(new Rectangle(cr.x - border , cr.y - border, cr.width + 2 * border, cr.height + 3 * border));
				}
			}
		}
		update();
	}

	private void updateCurrentPositionFromVisibleRect() {
		if (!markedValuePerPosition.isEmpty()) {
			Rectangle visRect = table.getVisibleRect();
			if (visRect == null) {
				return;
			}
			int minY = table.rowAtPoint(new Point(visRect.x, visRect.y + table.getRowHeight() / 2));
			if (minY < 0) {
				minY = 0;
			}
			int maxY = table.rowAtPoint(new Point(visRect.x, visRect.y + visRect.height - table.getRowHeight() / 2));
			if (maxY < 0) {
				maxY = table.getRowCount() + 1;
			}
			int cc = table.getColumnCount();
			Integer pos = markedValuePerPosition.ceilingKey(minY * cc);
			if (pos == null || pos > maxY * cc) {
				pos = markedValuePerPosition.floorKey(minY * cc);
			}
			if (cc > 0 && currentPosition != null && currentPosition / cc >= minY && currentPosition / cc <= maxY) {
				return;
			}
			if (pos != null) {
				currentPosition = pos;
				update();
			}
		}
	}

	private void beep() {
		Toolkit.getDefaultToolkit().beep();
	}

	private boolean inUpdate = false;
	
	private Color origBackground;
	private Map<String, String> markedValues = new HashMap<String, String>();
	private Map<String, String> tooltips = new HashMap<String, String>();
	private NavigableMap<Integer, String> markedValuePerPosition = new TreeMap<Integer, String>();
	private NavigableMap<Integer, Integer> ordinalPerViewPosition = new TreeMap<Integer, Integer>();
	private Map<Integer, Integer> viewToModelPosition = new TreeMap<Integer, Integer>();
	private String lastSearchText = null;
	private Integer currentPosition = null;

	/**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jToolBar1 = new javax.swing.JToolBar();
        searchField = new javax.swing.JTextField();
        jLabel1 = new javax.swing.JLabel();
        prevButton = new javax.swing.JButton();
        nextButton = new javax.swing.JButton();
        closeButton = new javax.swing.JButton();
        jLabel3 = new javax.swing.JLabel();
        warnLabel = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        counterLabel = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();

        setLayout(new java.awt.GridBagLayout());

        jToolBar1.setRollover(true);

        searchField.setBackground(Colors.Color_255_255_255);
        searchField.setToolTipText("<html>Search criteria.<br><br>\nSearch for items that contain the search criteria as:<br>\n<table>\n<tr><td><b>Prefix</b></td><td>if it starts with a space</td></tr>\n<tr><td><b>Suffix</b></td><td>if it ends with a space</td></tr>\n<tr><td><b>Substring</b></td><td>else</td></tr>\n</table>\n<br>\n(<b>*</b> = any string, <b>?</b> = any character)\n</html>");
        jToolBar1.add(searchField);

        jLabel1.setText(" ");
        jToolBar1.add(jLabel1);

        prevButton.setText("Previous");
        prevButton.setToolTipText("Find  previous occurrence");
        prevButton.setFocusable(false);
        prevButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        prevButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar1.add(prevButton);

        nextButton.setText("Next");
        nextButton.setToolTipText("Find next occurrence");
        nextButton.setFocusable(false);
        nextButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        nextButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar1.add(nextButton);

        closeButton.setText("Close");
        closeButton.setFocusable(false);
        closeButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        closeButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        closeButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                closeButtonActionPerformed(evt);
            }
        });
        jToolBar1.add(closeButton);

        jLabel3.setText("  ");
        jToolBar1.add(jLabel3);

        warnLabel.setText("warn");
        warnLabel.setToolTipText("<html>The row limit has been exceeded. <br>The search result is potentially incomplete.</html>");
        jToolBar1.add(warnLabel);

        jLabel5.setText("  ");
        jToolBar1.add(jLabel5);

        counterLabel.setText("counter");
        jToolBar1.add(counterLabel);

        add(jToolBar1, new java.awt.GridBagConstraints());

        jLabel2.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.weightx = 1.0;
        add(jLabel2, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    private void closeButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_closeButtonActionPerformed
        setVisible(false);
        update();
    }//GEN-LAST:event_closeButtonActionPerformed

	private JTable transposedTable;
	
	public void setTransposedTable(ColumnsTable transposedTable) {
		this.transposedTable = transposedTable;
	}
	
	public void setLimitExceeded(boolean limitExceeded) {
		warnLabel.setVisible(limitExceeded);
	}

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton closeButton;
    private javax.swing.JLabel counterLabel;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JToolBar jToolBar1;
    private javax.swing.JButton nextButton;
    private javax.swing.JButton prevButton;
    private javax.swing.JTextField searchField;
    private javax.swing.JLabel warnLabel;
    // End of variables declaration//GEN-END:variables
    
    private static final long serialVersionUID = 2251485971289829275L;
	
	private ImageIcon closeIcon;
	private ImageIcon prevIcon;
	private ImageIcon nextIcon;
	private ImageIcon warnIcon;
	{
		// load images
		closeIcon = UIUtil.readImage("/buttoncancel.png");
		prevIcon = UIUtil.readImage("/prev.png");
		nextIcon = UIUtil.readImage("/next.png");
		warnIcon = UIUtil.readImage("/wanr.png");
	}
	
}
