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
package net.sf.jailer.ui.util;

import java.awt.Color;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.SwingUtilities;

import net.sf.jailer.ui.UIUtil;

public abstract class SmallButton extends JLabel {

	private final boolean borderStyle;
	private static final Color BORDER_LIGHT = Color.lightGray;
	private static final Color BORDER_SHADOW = Color.gray;
	private final Icon icon;
	private final Icon iconOver;
	protected boolean entered = false;
	protected boolean silent = false;
	protected MouseListener mouseListener;
	
	public SmallButton(Icon icon) {
		this(icon, false);
	}
	
	public SmallButton(Icon icon, boolean borderStyle) {
		this(icon, null, borderStyle);
	}
	
	public SmallButton(Icon icon, Icon iconOver, boolean borderStyle) {
		super(icon);
		this.icon = icon;
		this.iconOver = iconOver;
		this.borderStyle = borderStyle;
		onMouseExited();
		addMouseListener(mouseListener = new MouseListener() {
			@Override
			public void mouseReleased(MouseEvent e) {
				if (entered && SwingUtilities.isLeftMouseButton(e)) {
					doClick(e);
				}
			}
			@Override
			public void mousePressed(MouseEvent e) {
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
				if (SwingUtilities.isLeftMouseButton(e)) {
					doClick(e);
				}
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
		});
	}

	protected abstract void onClick(MouseEvent e);

	protected void onMouseExited() {
		entered = false;
		if (silent) {
			return;
		}
		if (iconOver != null) {
			setIcon(icon);
		} else if (borderStyle) {
			setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED, BORDER_LIGHT, BORDER_SHADOW));
		} else {
			setEnabled(false);
		}
	}

	protected void onMouseEntered() {
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
	
}
