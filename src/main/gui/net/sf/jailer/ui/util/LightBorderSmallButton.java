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

import javax.swing.Icon;

import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.UIUtil.PLAF;

public abstract class LightBorderSmallButton extends SmallButton {
	
	private static final Color INVISIBLE = new Color(0, 0, 0, 0);
	protected boolean freezed = false;
	
	public LightBorderSmallButton(Icon icon) {
		super(icon, true);
	}
	
	protected void onMouseExited() {
		if (UIUtil.plaf != PLAF.FLAT) {
			super.onMouseExited();
			setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED, INVISIBLE, INVISIBLE));
		} else if (!freezed) {
			entered = false;
			setOpaque(false);
			setBackground(null);
			setBorder(new javax.swing.border.LineBorder(INVISIBLE, 2, true));
		}
	}

	protected void onMouseEntered() {
		if (UIUtil.plaf != PLAF.FLAT) {
			super.onMouseEntered();
			setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.LOWERED, Color.LIGHT_GRAY, Color.GRAY));
		} else if (!freezed) {
			entered = true;
			setOpaque(true);
			setBackground(UIUtil.BG_FLATMOUSEOVER);
			setBorder(new javax.swing.border.LineBorder(UIUtil.BG_FLATMOUSEOVER, 2, true));
		}
	}
	
};

