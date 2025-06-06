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
package net.sf.jailer.ui.syntaxtextarea;

import java.awt.Color;
import java.io.IOException;

import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.fife.ui.rsyntaxtextarea.Theme;
import org.fife.ui.rtextarea.Gutter;

import net.sf.jailer.ui.Colors;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.UIUtil.PLAF;
import net.sf.jailer.util.LogUtil;

/**
 * Text area with theme.
 * 
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public class RSyntaxTextAreaWithTheme extends RSyntaxTextArea {
	
	private Color currentLineHighlightColor;

	@Override
	public void setCurrentLineHighlightColor(Color color) {
		currentLineHighlightColor = color;
		super.setCurrentLineHighlightColor(color);
	}

	public RSyntaxTextAreaWithTheme() {
		initTheme();
	}

	public RSyntaxTextAreaWithTheme(String text) {
		super(text);
	}

	protected Gutter gutter;

	public void setGutter(Gutter gutter) {
		this.gutter = gutter;
		Color clhc = currentLineHighlightColor;
		boolean flhc = getFadeCurrentLineHighlight();
		initTheme();
		if (flhc) {
			setCurrentLineHighlightColor(clhc);
			setFadeCurrentLineHighlight(flhc);
		}
	}

	public void initTheme() {
		try {
			Color clhc = currentLineHighlightColor;
			boolean flhc = getFadeCurrentLineHighlight();
			new Theme(this);
			Theme t = Theme.load(getClass().getResourceAsStream(UIUtil.plaf == PLAF.FLATDARK?
					"/net/sf/jailer/ui/resource/dark.xml" : "/org/fife/ui/rsyntaxtextarea/themes/default.xml"));
			t.apply(this);
			if (UIUtil.plaf == PLAF.FLATDARK) {
				setBackground(Colors.GraphicalDataViewBackground);
			}
			if (flhc) {
				setCurrentLineHighlightColor(clhc);
				setFadeCurrentLineHighlight(flhc);
			}
		} catch (IOException e1) {
			LogUtil.warn(e1);
		}
	}
}
