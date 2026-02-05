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
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.util.Stack;

import javax.swing.AbstractButton;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JToggleButton;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.event.InternalFrameEvent;
import javax.swing.event.InternalFrameListener;
import javax.swing.plaf.basic.BasicInternalFrameUI;

import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.Desktop.RowBrowser;
import net.sf.jailer.ui.util.UISettings;

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
	private boolean fadeDown;
	private float lastAlpha;
	private long fadeDelay;
	private Long initFadeStartedAt = null;
	private RowBrowser currentBrowser;
	private RowBrowser newestBrowser;
	private int height = 5 * 18;
	private final static int MAX_RETENDION = 1000;

	@SuppressWarnings("serial")
	public DesktopAnchorManager(JPanel topLayerPanel) {
		this.anchorPanel = new JPanel(null) {
			RowBrowser initFadeBrowser = null;
			@Override
			public void paint(Graphics g) {
				Graphics2D g2 = (Graphics2D) g.create();
				if (ft) {
					g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, lastAlpha = 0));
					super.paint(g2);
					return;
				}
				lastAlpha = 0;
				try {
					if (initFadeStartedAt != null) {
						double r1 = 0.65f;
						double t1 = System.currentTimeMillis() - initFadeStartedAt - MAX_RETENDION * (1 - r1);
						if (t1 < 0 && initFadeBrowser == currentBrowser) {
							g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, lastAlpha = (float) Math.min(1f, (t1 / (-MAX_RETENDION * (1 - r1))) / 2f + 0.5f)));
							super.paint(g2);
							return;
						} else {
							initFadeStartedAt = null;
							initFadeBrowser = null;
							fadeDown = false;
							fadeDelay = 0;
							showedAt = System.currentTimeMillis();
						}
					}
					if (showedAt != null) {
						if (fadeDown && initFadeStartedAt == null) {
							initFadeStartedAt = System.currentTimeMillis() + fadeDelay;
							initFadeBrowser = currentBrowser;
							super.paint(g2);
							return;
						} else {
							double r = 0.75f;
							double t = System.currentTimeMillis() - showedAt - MAX_RETENDION * r;
							g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, lastAlpha = (float) ((1 - Math.max(0f, Math.min(1f, ((t / (MAX_RETENDION * (1 - r))))))) * 0.5f)));
						}
						super.paint(g2);
						return;
					} else {
						super.paint(g);
					}
				} finally {
					g2.dispose();
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
		UIUtil.initToolTip(anchorButton);
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
				initShowedAt(true);
			}
			@Override
			public void mouseEntered(MouseEvent e) {
				showedAt = null;
				fadeDown = false;
				initFadeStartedAt = null;
				anchorPanel.repaint();
			}
			@Override
			public void mouseClicked(MouseEvent e) {
			}
		});
		anchorPanel.addMouseListener(ml);
	}
	
	protected abstract void addAdditionalActions(RowBrowser tableBrowser);

	protected void addAdditionalAction(ImageIcon icon, String tooltip, Runnable action, boolean isToggleButton, boolean isSelected, boolean enabled) {
		if (icon == null) {
			anchorPanel.setSize(anchorPanel.getWidth(), anchorPanel.getHeight() + 6);
			return;
		}
		AbstractButton button = isToggleButton? new JToggleButton((String) null, isSelected) : new JButton();
		button.setEnabled(enabled);
		button.setText(null);
		button.setIcon(UIUtil.scaleIcon(icon, anchorIcon.getIconHeight() / (double) icon.getIconHeight()));
		button.setToolTipText(tooltip);
		UIUtil.initToolTip(button);
		int componentCount = anchorPanel.getComponentCount();
		button.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (currentBrowser != null) {
					if (!isToggleButton) {
						reset(1000);
					}
					action.run();
					if (UISettings.s13 % 100 != 99) {
						UISettings.s13++;
						switch (componentCount) {
						case 1:	UISettings.s13 += 100; break;
						case 2:	UISettings.s13 += 10000; break;
						case 3:	UISettings.s13 += 1000000; break;
						}
					}
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
				initShowedAt(true);
			}
			@Override
			public void mouseEntered(MouseEvent e) {
				showedAt = null;
				fadeDown = false;
				initFadeStartedAt = null;
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

	private void reorgPanel() {
		anchorPanel.setSize((anchorButton.getWidth()) * 2, anchorButton.getHeight());
		final Component[] components = anchorPanel.getComponents();
		for (int i = 0; i < components.length; i += 1) {
			Component comp = components[i <= 1? 1 - i : i];
			comp.setLocation(new Point((i % 2) == 0? 1 : (1 + anchorButton.getWidth()), (i / 2) * anchorButton.getHeight()));
			anchorPanel.setSize(anchorPanel.getWidth(), comp.getY() + comp.getHeight() + 1);
		}
	}

	public void onTableBrowserNeared(RowBrowser tableBrowser) {
		if (isAvailable()) {
			anchorButton.setEnabled(isApplicable(tableBrowser));
			boolean wasVisible = anchorPanel.isVisible();
			reset();
			showButton(tableBrowser, wasVisible);
		}
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
		MouseMotionListener showButton = new MouseAdapter() {
			@Override
			public void mouseMoved(MouseEvent e) {
				if (isAvailable()) {
					double dx = SwingUtilities.convertPoint(e.getComponent(), e.getX(), e.getY(), tableBrowser.internalFrame).getX();
					if (dx <= 180) {
						anchorButton.setEnabled(isApplicable(tableBrowser));
						boolean wasVisible = anchorPanel.isVisible();
						reset();
						showButton(tableBrowser, wasVisible);
					}
				}
			}
		};
		
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
						comp.addMouseMotionListener(showButton);
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
			tableBrowser.browserContentPane.thumbnail.addMouseMotionListener(showButton);
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
				northPane.addMouseMotionListener(showButton);
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
	
	private boolean ft = true;
	private void showButton(RowBrowser tableBrowser, boolean wasVisible) {
		showButton0(tableBrowser, wasVisible);
		if (ft) {
			UIUtil.invokeLater(2, () -> {
				ft = false;
				showButton0(tableBrowser, wasVisible);
			});
		}
	}

	private void showButton0(RowBrowser tableBrowser, boolean wasVisible) {
		if (disabledUntil != null && disabledUntil > System.currentTimeMillis()) {
			return;
		}
		anchorPanel.removeAll();
		anchorPanel.add(anchorButton);
		
		boolean nBrChg = currentBrowser == tableBrowser;
		boolean fadeDown = this.fadeDown && nBrChg;
		currentBrowser = tableBrowser;
		anchorButton.setSize(anchorButton.getPreferredSize());
		anchorPanel.setSize(anchorButton.getPreferredSize());
		addAdditionalActions(tableBrowser);
		reorgPanel();
		Point loc;
		loc = tableBrowser.internalFrame.getLocation();
		loc.translate(-anchorPanel.getWidth(), 0);
		anchorPanel.setLocation(0, 0);
		loc = SwingUtilities.convertPoint(tableBrowser.internalFrame.getParent(), loc, anchorPanel);
		Point locO = new Point();
		locO = SwingUtilities.convertPoint(tableBrowser.internalFrame.getParent().getParent(), locO, anchorPanel);
		loc = new Point(Math.max(locO.x - anchorPanel.getWidth(), loc.x) - 1, Math.max(locO.y, loc.y + 1));

		if (anchorPanel.getComponentCount() > 1 && !anchorPanel.getComponents()[0].isEnabled() && !anchorPanel.getComponents()[1].isEnabled()) {
			loc = new Point(loc.x, loc.y + anchorPanel.getComponents()[0].getHeight());
			anchorPanel.setSize(anchorPanel.getWidth(), anchorPanel.getHeight() - anchorPanel.getComponents()[0].getHeight());
			for (Component c: anchorPanel.getComponents()) {
				c.setLocation(c.getX(), c.getY() - anchorPanel.getComponents()[0].getHeight());
			}
			anchorPanel.getComponents()[0].setVisible(false);
			anchorPanel.getComponents()[1].setVisible(false);
		}
		anchorPanel.setLocation(loc);
		anchorButton.setVisible(true);
		anchorPanel.setVisible(true);
		topLayerPanel.setVisible(true);
		height = anchorPanel.getHeight();
		if (!wasVisible || !nBrChg || lastAlpha >= 0.8f) {
			initFadeStartedAt = null;
			fadeDelay = 150;
			initShowedAt(true);
		} else {
			initShowedAt(fadeDown);
		}
	}

	private void initShowedAt(boolean fadeDownP) {
		this.fadeDown = fadeDownP;
		showedAt = System.currentTimeMillis() + fadeDelay;
		int delay = 30;
		Timer timer = new Timer(delay, null);
		timer.addActionListener(e -> {
			if (showedAt != null && anchorPanel.isVisible()) {
				anchorPanel.paintImmediately(0, 0, anchorPanel.getWidth(), anchorPanel.getHeight());
			} else {
				timer.stop();
			}
		});
		timer.setRepeats(true);
		timer.setInitialDelay(delay);
		timer.start();
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
		if (anchorPanel.isVisible() && showedAt != null && showedAt + MAX_RETENDION < now) {
			reset();
		}
	}

	public int getButtonWidth() {
		return anchorButton.getWidth();
	}

	protected abstract void layout(RowBrowser anchor);
	protected abstract boolean isAvailable();
	protected abstract boolean isApplicable(RowBrowser tableBrowser);

	private ImageIcon anchorIcon;
	{
        // load images
		anchorIcon = UIUtil.scaleIcon(UIUtil.readImage("/anchor_32.png"), 0.75);
    }

}
