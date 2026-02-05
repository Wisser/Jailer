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
package net.sf.jailer.ui.util;

import java.awt.Color;
import java.awt.Toolkit;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.SwingUtilities;

import net.sf.jailer.ui.Colors;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.UIUtil.PLAF;
import net.sf.jailer.ui.UIUtil.PlafAware;

public abstract class SmallButton extends JLabel implements PlafAware {

	private final boolean borderStyle;
	private static final Color BORDER_LIGHT = Colors.Color_192_192_192;
	private static final Color BORDER_SHADOW = Colors.Color_128_128_128;
	private final Icon icon;
	private final Icon iconOver;
	protected boolean entered = false;
	protected boolean silent = false;
	protected MouseListener mouseListener;
	
	public SmallButton(Icon icon) {
		this(icon, false);
	}
	
	public SmallButton(Icon icon, boolean borderStyle) {
		this(icon, null, borderStyle, false);
	}
	
	public SmallButton(Icon icon, Icon iconOver, boolean borderStyle, boolean reactImmediately) {
		super(icon);
		this.icon = icon;
		this.iconOver = iconOver;
		this.borderStyle = borderStyle;
		onMouseExited();
		addMouseListener(mouseListener = new MouseListener() {
			@Override
			public void mouseReleased(MouseEvent e) {
				onMouseReleased(e);
			}
			@Override
			public void mousePressed(MouseEvent e) {
				if (reactImmediately) {
					mouseClicked(e);
				}
			}
			@Override
			public void mouseExited(MouseEvent e) {
				onMouseExited();
			}
			@Override
			public void mouseEntered(MouseEvent e) {
				onMouseEntered();
			}
			@Override
			public void mouseClicked(MouseEvent e) {
				if (SwingUtilities.isLeftMouseButton(e) && isReady()) {
					doClick(e);
				}
			}
		});
	}

	protected boolean isReady() {
		return true;
	}

	@Override
	public void onNewPlaf() {
		onMouseExited();
	}
	
	boolean sent = false;
	private void doClick(MouseEvent e) {
		if (!sent && isEnabled()) {
			sent = true;
			UIUtil.invokeLater(() -> {
				sent = false;
				onClick(e);
			});
		}
	}

	public void doClick() {
		if (isReady()) {
			doClick(null);
		}
	}
	
	protected abstract void onClick(MouseEvent e);

	public void onMouseExited() {
		entered = false;
		Icon theIcon = lafIcon();
		setIcon(theIcon);
		if (silent) {
			return;
		}
		if (iconOver != null) {
			setIcon(theIcon);
		} else if (borderStyle) {
			setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED, BORDER_LIGHT, BORDER_SHADOW));
		} else {
			setEnabled(false);
		}
	}

	private Icon lafIcon() {
		if (icon == closeIcon && UIUtil.plaf == PLAF.FLATDARK) {
			return closeDarkIcon;
		}
		if (icon == close16Icon && UIUtil.plaf == PLAF.FLATDARK) {
			return close16darkIcon;
		}
		if (icon == closeDarkIcon && UIUtil.plaf != PLAF.FLATDARK) {
			return closeIcon;
		}
		if (icon == close16darkIcon && UIUtil.plaf != PLAF.FLATDARK) {
			return close16Icon;
		}
		return icon;
	}

	public void onMouseEntered() {
		entered = true;
		if (silent) {
			return;
		}
		if (iconOver != null) {
			setIcon(iconOver);
		} else if (borderStyle) {
			setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.LOWERED, BORDER_LIGHT, BORDER_SHADOW));
		} else {
			setEnabled(true);
		}
	}
	
	public void onMouseReleased(MouseEvent e) {
		if (entered && isReady() && SwingUtilities.isLeftMouseButton(e)) {
			doClick(e);
		}
	}

	private Icon closeIcon;
	private Icon close16Icon;
	private Icon close16darkIcon;
	private Icon closeDarkIcon;
	private Icon closeOverIcon;
	
	{
		closeIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/close.png"), 1.4);
		closeDarkIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/closedark.png"), 1.4);
    	close16Icon = UIUtil.readImage("/Close-16-1.png");
    	close16darkIcon = UIUtil.readImage("/Close-16-1dark.png");
		closeOverIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/close_over.png"), 1.4);
	}

}
