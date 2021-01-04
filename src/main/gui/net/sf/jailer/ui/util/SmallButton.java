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

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.SwingUtilities;

public abstract class SmallButton extends JLabel {

	public SmallButton(Icon icon) {
		super(icon);
		setEnabled(false);
		addMouseListener(new MouseListener() {
			@Override
			public void mouseReleased(MouseEvent e) {
				if (SwingUtilities.isLeftMouseButton(e)) {
					onClick();
				}
			}
			@Override
			public void mousePressed(MouseEvent e) {
			}
			@Override
			public void mouseExited(MouseEvent e) {
				setEnabled(false);
			}
			@Override
			public void mouseEntered(MouseEvent e) {
				setEnabled(true);
			}
			@Override
			public void mouseClicked(MouseEvent e) {
				if (SwingUtilities.isLeftMouseButton(e)) {
					onClick();
				}
			}
		});
	}

	protected abstract void onClick();
	
}
