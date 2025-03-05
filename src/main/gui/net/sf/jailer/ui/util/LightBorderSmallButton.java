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
package net.sf.jailer.ui.util;

import java.awt.Color;

import javax.swing.Icon;
import javax.swing.JComponent;

import net.sf.jailer.ui.Colors;
import net.sf.jailer.ui.UIUtil;

public abstract class LightBorderSmallButton extends SmallButton {
	
	private static final Color INVISIBLE = Colors.Color_0_0_0_0;
	protected boolean freezed = false;
	private final boolean reactImmediately;
	private final int borderThickness;
	
	public LightBorderSmallButton(Icon icon) {
		this(icon, false, 2);
	}
	
	public LightBorderSmallButton(Icon icon, boolean reactImmediately) {
		this(icon, reactImmediately, 2);
	}
	
	public LightBorderSmallButton(Icon icon, boolean reactImmediately, int borderThickness) {
		super(icon, null, true, reactImmediately);
		this.reactImmediately = reactImmediately;
		this.borderThickness = borderThickness;
		onMouseExited();
	}
	
	protected JComponent getFrame() {
		return this;
	}
	
	protected JComponent getAdditionalFrame() {
		return null;
	}
	
	protected Color getSelectedBackgroundColor() {
		return UIUtil.BG_FLATMOUSEOVER;
	}
	
	@Override
	protected boolean isReady() {
		return !reactImmediately || !freezed;
	}
	
	@Override
	public void onMouseExited() {
		JComponent frame = getFrame();
		if (!UIUtil.plaf.isFlat) {
			super.onMouseExited();
			frame.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED,
					INVISIBLE, INVISIBLE));
		} else if (!freezed) {
			entered = false;
			frame.setOpaque(false);
			frame.setBackground(null);
			if (borderThickness > 0) {
				frame.setBorder(new javax.swing.border.LineBorder(INVISIBLE, borderThickness, true));
			}
			JComponent additionalFrame = getAdditionalFrame();
			if (additionalFrame != null) {
				additionalFrame.setOpaque(false);
				additionalFrame.setBackground(null);
				if (borderThickness > 0) {
					additionalFrame.setBorder(new javax.swing.border.LineBorder(INVISIBLE, borderThickness, true));
				}
			}
		}
	}

	@Override
	public void onMouseEntered() {
		if (isEnabled()) {
			JComponent frame = getFrame();
			if (!UIUtil.plaf.isFlat) {
				super.onMouseEntered();
				frame.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.LOWERED, Colors.Color_192_192_192, Colors.Color_128_128_128));
			} else if (!freezed) {
				entered = true;
				frame.setOpaque(true);
				frame.setBackground(getSelectedBackgroundColor());
				if (borderThickness > 0) {
					frame.setBorder(new javax.swing.border.LineBorder(getSelectedBackgroundColor(), borderThickness, true));
				}
				JComponent additionalFrame = getAdditionalFrame();
				if (additionalFrame != null) {
					additionalFrame.setOpaque(true);
					additionalFrame.setBackground(getSelectedBackgroundColor());
					if (borderThickness > 0) {
						additionalFrame.setBorder(new javax.swing.border.LineBorder(getSelectedBackgroundColor(), borderThickness, true));
					}
				}
			}
		}
	}
};

