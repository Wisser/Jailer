/*
 * Copyright 2007 - 2023 Ralf Wisser.
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
package net.sf.jailer.ui;

import java.awt.Component;
import java.awt.Desktop;
import java.net.URI;

import javax.swing.JOptionPane;

/**
 * Browser launcher.
 */
public class BrowserLauncher {

	/**
	 * Opens URL in browser.
	 * 
	 * @param uri the uri to open
	 * @param parent parent component
	 */
	public static void openURL(URI uri, Component parent) {
		try {
			Desktop.getDesktop().browse(uri);
		} catch (Throwable t) {
			String message = "The default browser for displaying\n" + (uri.toString()) + "\ncan not be started.";
			JOptionPane.showMessageDialog(parent, message, "Warning", JOptionPane.WARNING_MESSAGE);
		}
	}

}
