/*
 * Copyright 2007 - 2012 the original author or authors.
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

import java.io.File;
import java.lang.reflect.Method;
import java.net.URI;

/**
 * Browser launcher.
 */
public class BrowserLauncher {

	/**
	 * Opens URL in browser.
	 * 
	 * @param uri the uri to open
	 */
	public static void openURL(URI uri) throws Exception {
		Class<?> desktop = Class.forName("java.awt.Desktop");
		if (desktop != null) {
			Method getDesktop = desktop.getMethod("getDesktop");
			Method browse = desktop.getMethod("browse", URI.class);
			if (getDesktop != null && browse != null) {
				browse.invoke(getDesktop.invoke(null), uri);
				return;
			}
		}
	}

	/**
	 * Opens URL in browser.
	 * 
	 * @param url the url to open
	 */
	public static void openURL(String url) {
		try {
			Class<?> desktop = Class.forName("java.awt.Desktop");
			if (desktop != null) {
				Method getDesktop = desktop.getMethod("getDesktop");
				Method browse = desktop.getMethod("browse", URI.class);
				if (getDesktop != null && browse != null) {
					browse.invoke(getDesktop.invoke(null), new File(url)
							.toURI());
					return;
				}
			}
		} catch (Exception e) {
		}
	}

}
