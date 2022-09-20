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

import java.awt.AlphaComposite;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.Stack;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.event.InternalFrameEvent;
import javax.swing.event.InternalFrameListener;
import javax.swing.plaf.basic.BasicInternalFrameUI;

import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.Desktop.RowBrowser;

/**
 * Desktop Anchor Manager.
 * 
 * @author Ralf Wisser
 */
public abstract class DesktopAnchorManager {

	private final JPanel anchorPanel;
	private final JPanel topLayerPanel;
	private final JButton anchorButton;
	private Long disabledUntil;
	private Long showedAt;
	private RowBrowser currentBrowser;
	private RowBrowser newestBrowser;
	private int height = 5 * 18;
	private boolean useBuffer = true;
	private final static int MAX_RETENDION = 800;

	@SuppressWarnings("serial")
	public DesktopAnchorManager(JPanel topLayerPanel) {
		this.anchorPanel = new JPanel(null) {
			@Override
			public void paint(Graphics g) {
				if (useBuffer) {
					Graphics2D g2 = (Graphics2D) g.create();
					g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.5f));
					super.paint(g2);
					g2.dispose();
				} else {
					super.paint(g);
				}
			}
		};
		anchorPanel.setOpaque(false);
		topLayerPanel.add(anchorPanel);

		this.topLayerPanel = topLayerPanel;
		this.anchorButton = new JButton(anchorIcon);
		
		this.anchorPanel.setVisible(false);
		this.topLayerPanel.setVisible(false);
		
		anchorPanel.add(anchorButton);
		JComponent comp = new JSeparator(SwingConstants.HORIZONTAL);
		anchorPanel.add(comp);
		comp.setLocation(1, 32);
		comp.setSize(32, 32);
		anchorPanel.add(comp);
		comp = new JButton("Test");
		anchorPanel.add(comp);
		comp.setLocation(1, 64);
		comp.setSize(32, 32);
		
		anchorButton.setToolTipText("Align horizontally with predecessors");
		anchorButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (currentBrowser != null) {
					reset(1000);
					layout(currentBrowser);
				}
			}
		});
		MouseListener ml;
		anchorButton.addMouseListener(ml = new MouseListener() {
			@Override
			public void mouseReleased(MouseEvent e) {
			}
			@Override
			public void mousePressed(MouseEvent e) {
			}
			@Override
			public void mouseExited(MouseEvent e) {
				showedAt = System.currentTimeMillis();
			}
			@Override
			public void mouseEntered(MouseEvent e) {
				showedAt = null;
				useBuffer = false;
				anchorPanel.repaint();
			}
			@Override
			public void mouseClicked(MouseEvent e) {
			}
		});
		anchorPanel.addMouseListener(ml);
	}
	
	protected abstract void addAdditionalActions(RowBrowser tableBrowser);

	protected void addAdditionalAction(ImageIcon icon, String tooltip, Runnable action, boolean enabled) {
		if (icon == null) {
			anchorPanel.setSize(anchorPanel.getWidth(), anchorPanel.getHeight() + 6);
			return;
		}
		JButton button = new JButton();
		button.setEnabled(enabled);
		button.setText(null);
		button.setIcon(UIUtil.scaleIcon(icon, anchorIcon.getIconHeight() / (double) icon.getIconHeight()));
		button.setToolTipText(tooltip);
		button.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (currentBrowser != null) {
					reset(1000);
					action.run();
				}
			}
		});
		button.addMouseListener(new MouseListener() {
			@Override
			public void mouseReleased(MouseEvent e) {
			}
			@Override
			public void mousePressed(MouseEvent e) {
			}
			@Override
			public void mouseExited(MouseEvent e) {
				showedAt = System.currentTimeMillis();
			}
			@Override
			public void mouseEntered(MouseEvent e) {
				showedAt = null;
				useBuffer = false;
				anchorPanel.repaint();
			}
			@Override
			public void mouseClicked(MouseEvent e) {
			}
		});
		anchorPanel.add(button);
		button.setSize(anchorButton.getSize());
		button.setLocation(anchorButton.getX(), anchorPanel.getHeight());
		anchorPanel.setSize(anchorPanel.getWidth(), button.getY() + button.getHeight());
	}
	
	public void onTableBrowserNeared(RowBrowser tableBrowser) {
		anchorButton.setEnabled(isApplicable(tableBrowser));
		reset();
		showButton(tableBrowser);
	}

	public void onNewTableBrowser(final RowBrowser tableBrowser) {
		newestBrowser = tableBrowser;
		tableBrowser.internalFrame.addInternalFrameListener(new InternalFrameListener() {
			@Override
			public void internalFrameOpened(InternalFrameEvent e) {
			}

			@Override
			public void internalFrameIconified(InternalFrameEvent e) {
				reset();
			}

			@Override
			public void internalFrameDeiconified(InternalFrameEvent e) {
				reset();
			}

			@Override
			public void internalFrameDeactivated(InternalFrameEvent e) {
			}

			@Override	
			public void internalFrameClosing(InternalFrameEvent e) {
			}

			@Override
			public void internalFrameClosed(InternalFrameEvent e) {
				reset();
				newestBrowser = null;
			}

			@Override
			public void internalFrameActivated(InternalFrameEvent e) {
			}
		});
		MouseListener showButton = new MouseListener() {
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
				anchorButton.setEnabled(isApplicable(tableBrowser));
				reset();
				showButton(tableBrowser);
			}
			@Override
			public void mouseClicked(MouseEvent e) {
			}
		};
//		tableBrowser.browserContentPane.menuPanel.addMouseListener(showButton);
		
		Stack<JPanel> panels = new Stack<JPanel>();
		panels.add(tableBrowser.browserContentPane.menuPanel);
		while (!panels.isEmpty()) {
			JPanel panel = panels.pop();
			synchronized (panel.getTreeLock()) {
				for (Component comp: panel.getComponents()) {
					if (comp instanceof JPanel) {
						if (comp != tableBrowser.browserContentPane.sqlPanel) {
							if (comp != tableBrowser.browserContentPane.relatedRowsPanel) {
								panels.push((JPanel) comp);
							}
						}
					} else if (comp instanceof JLabel) {
						comp.addMouseListener(showButton);
					}
				}
			}
		}

//		tableBrowser.browserContentPane.addMouseListener(showButton);
//		tableBrowser.browserContentPane.sqlPanel.addMouseListener(showButton);
//		tableBrowser.browserContentPane.relatedRowsPanel.addMouseListener(showButton);
//		tableBrowser.browserContentPane.loadButton.addMouseListener(showButton);
//		tableBrowser.browserContentPane.rowsTable.addMouseListener(showButton);
		if (tableBrowser.browserContentPane.thumbnail != null) {
			tableBrowser.browserContentPane.thumbnail.addMouseListener(showButton);
		}
//		if (tableBrowser.browserContentPane.andCondition.getEditor() != null) {
//			if (tableBrowser.browserContentPane.andCondition.getEditor().getEditorComponent() != null) {
//				tableBrowser.browserContentPane.andCondition.getEditor().getEditorComponent().addMouseListener(showButton);
//			}
//		}
		Object bi = tableBrowser.internalFrame.getUI();
		if (bi instanceof BasicInternalFrameUI) {
			JComponent northPane = ((BasicInternalFrameUI) bi).getNorthPane();
			if (northPane != null) {
				northPane.addMouseListener(showButton);
			}
		}
		tableBrowser.internalFrame.addComponentListener(new ComponentListener() {
			@Override
			public void componentShown(ComponentEvent e) {
			}
			@Override
			public void componentResized(ComponentEvent e) {
				reset(100);
			}
			@Override
			public void componentMoved(ComponentEvent e) {
				reset(100);
			}
			@Override
			public void componentHidden(ComponentEvent e) {
				reset();
			}
		});
		reset();
	}

	protected void showButton(RowBrowser tableBrowser) {
		if (disabledUntil != null && disabledUntil > System.currentTimeMillis()) {
			return;
		}
		useBuffer = true;
		anchorPanel.removeAll();
		anchorPanel.add(anchorButton);
		currentBrowser = tableBrowser;
		anchorButton.setSize(anchorButton.getPreferredSize());
		anchorPanel.setSize(anchorButton.getPreferredSize());
		addAdditionalActions(tableBrowser);
		Point loc;
		loc = tableBrowser.internalFrame.getLocation();
		loc.translate(-anchorButton.getWidth(), 0);
		anchorPanel.setLocation(0, 0);
		loc = SwingUtilities.convertPoint(tableBrowser.internalFrame.getParent(), loc, anchorPanel);
		Point locO = new Point();
		locO = SwingUtilities.convertPoint(tableBrowser.internalFrame.getParent().getParent(), locO, anchorPanel);
		loc = new Point(Math.max(locO.x - anchorPanel.getWidth(), loc.x), Math.max(locO.y, loc.y + 1));
		anchorPanel.setLocation(loc);
		anchorButton.setVisible(true);
		anchorPanel.setVisible(true);
		topLayerPanel.setVisible(true);
		height = anchorPanel.getHeight();
		showedAt = System.currentTimeMillis();
	}

	void reset() {
		reset(0);
	}

	void reset(int delay) {
		anchorButton.setVisible(false);
		anchorPanel.setVisible(false);
		topLayerPanel.setVisible(false);
		if (delay > 0) {
			disabledUntil = System.currentTimeMillis() + delay;
		}
	}

	public int getHeight() {
		return height;
	}
	
	public RowBrowser getNewestBrowser() {
		return newestBrowser;
	}

	public void setNewestBrowser(RowBrowser browser) {
		newestBrowser = browser;
	}

	public void checkRetention() {
		long now = System.currentTimeMillis();
		if (showedAt != null && showedAt + MAX_RETENDION < now) {
			reset();
		}
	}

	public int getButtonWidth() {
		return anchorButton.getWidth();
	}

	protected abstract void layout(RowBrowser anchor);
	protected abstract boolean isApplicable(RowBrowser tableBrowser);

	private ImageIcon anchorIcon;
	{
        // load images
		anchorIcon = UIUtil.scaleIcon(UIUtil.readImage("/anchor_32.png"), 0.75);
    }

}
