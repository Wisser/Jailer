/*
 * Copyright 2007 - 2019 Ralf Wisser.
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

import java.awt.Window;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.util.HashMap;
import java.util.Map;

/**
 * Ensures that only the focused top level window performs expensive animations.
 * 
 * @author Ralf Wisser
 */
public class AnimationController {

	public interface AnimationControl {
		void setEnabled(boolean enabled);
	}

	private static Map<Window, Boolean> windowIsActive = new HashMap<Window, Boolean>();
	private static Map<Window, AnimationControl> windowControl = new HashMap<Window, AnimationControl>();

	/**
	 * Registers a new top level window.
	 */
	public static void registerWindow(final Window window, AnimationControl animationControl) {
		windowIsActive.put(window, false);
		windowControl.put(window, animationControl);
		window.addWindowListener(new WindowListener() {

			private void controlAnimation() {
				Window theActiveWindow = null;
				for (Window window: windowIsActive.keySet()) {
					if (windowIsActive.get(window)) {
						if (theActiveWindow == null) {
							theActiveWindow = window;
						} else {
							theActiveWindow = null;
							break;
						}
					}
				}
				for (Window window: windowControl.keySet()) {
					windowControl.get(window).setEnabled(theActiveWindow == null || window.equals(theActiveWindow));
				}
			}

			@Override
			public void windowDeactivated(WindowEvent e) {
				windowIsActive.put(window, false);
				controlAnimation();
			}
			@Override
			public void windowActivated(WindowEvent e) {
				windowIsActive.put(window, true);
				controlAnimation();
			}
			@Override
			public void windowClosed(WindowEvent e) {
				windowControl.remove(window);
				windowIsActive.remove(window);
				controlAnimation();
			}
			
			@Override
			public void windowOpened(WindowEvent e) {
			}
			@Override
			public void windowIconified(WindowEvent e) {
			}
			@Override
			public void windowDeiconified(WindowEvent e) {
			}
			@Override
			public void windowClosing(WindowEvent e) {
			}			
		});
	}

}