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
package net.sf.jailer.ui.util;

import java.awt.Color;

import javax.swing.Icon;
import javax.swing.JComponent;

import net.sf.jailer.ui.Colors;
import net.sf.jailer.ui.UIUtil;

public abstract class LightBorderSmallButton extends SmallButton {
	
	private static final Color INVISIBLE = Colors.Color_0_0_0_0;
	protected boolean freezed = false;
	
	public LightBorderSmallButton(Icon icon) {
		super(icon, true);
	}
	
	protected JComponent getFrame() {
		return this;
	}
	
	protected void onMouseExited() {
		if (!UIUtil.plaf.isFlat) {
			super.onMouseExited();
			getFrame().setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED,
					INVISIBLE, INVISIBLE));
		} else if (!freezed) {
			entered = false;
			getFrame().setOpaque(false);
			getFrame().setBackground(null);
			getFrame().setBorder(new javax.swing.border.LineBorder(INVISIBLE, 2, true));
		}
	}

	protected void onMouseEntered() {
		if (isEnabled()) {
			if (!UIUtil.plaf.isFlat) {
				super.onMouseEntered();
				getFrame().setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.LOWERED, Colors.Color_light_gray, Colors.Color_gray));
			} else if (!freezed) {
				entered = true;
				getFrame().setOpaque(true);
				getFrame().setBackground(getSelectedBackgroundColor());
				getFrame().setBorder(new javax.swing.border.LineBorder(getSelectedBackgroundColor(), 2, true));
			}
		}
	}

	protected Color getSelectedBackgroundColor() {
		return UIUtil.BG_FLATMOUSEOVER;
	}
	
};

