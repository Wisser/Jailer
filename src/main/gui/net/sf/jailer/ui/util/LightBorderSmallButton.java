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
package net.sf.jailer.ui.util;

import java.awt.Color;

import javax.swing.Icon;

public abstract class LightBorderSmallButton extends SmallButton {
	
	public LightBorderSmallButton(Icon icon) {
		super(icon, true);
	}
	
	protected void onMouseExited() {
		super.onMouseExited();
		Color borderColor = new Color(0, 0, 0, 0);
		setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED, borderColor, borderColor));
	}

	protected void onMouseEntered() {
		super.onMouseEntered();
		setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.LOWERED, Color.LIGHT_GRAY, Color.GRAY));
	}
	
};

