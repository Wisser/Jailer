/*
 * Copyright 2007 the original author or authors.
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

import net.sf.jailer.extractionmodel.ExtractionModel;

import org.apache.log4j.Logger;

/**
 * Browser launcher.
 */
public class BrowserLauncher {

	/**
     * The logger.
     */
    private static final Logger _log = Logger.getLogger(BrowserLauncher.class);
 
	/**
	 * Opens URL in browser.
	 * 
	 * @param url the url to open
	 */
	public static void openURL(String url) {
		try {
			Class desktop = Class.forName("java.awt.Desktop");
			if (desktop != null) {
				Method getDesktop = desktop.getMethod("getDesktop");
				Method browse = desktop.getMethod("browse", URI.class);
				if (getDesktop != null && browse != null) {
					browse.invoke(getDesktop.invoke(null), new File(url)
							.toURI());
					return;
				}
			}
			// fall-back for JDK 1.5
			openURL1_5(url);
		} catch (Exception e) {
			// fall-back for JDK 1.5
			openURL1_5(url);
		}
	}

	/**
	 * Opens URL in browser. Fall-back for JRE 5.
	 * 
	 * @param url the url to open
	 */
	private static void openURL1_5(String url) {
		_log.info("open browser fall-back for " + url);
		String osName = System.getProperty("os.name");
		try {
			if (osName.startsWith("Mac OS")) {
				Class fileMgr = Class.forName("com.apple.eio.FileManager");
				Method openURL = fileMgr.getDeclaredMethod("openURL",
						new Class[] { String.class });
				openURL.invoke(null, new Object[] { url });
			} else if (osName.startsWith("Windows"))
				Runtime.getRuntime().exec(
						"rundll32 url.dll,FileProtocolHandler " + url);
			else { // assume Unix or Linux
				String[] browsers = { "firefox", "opera", "konqueror",
						"epiphany", "mozilla", "netscape" };
				String browser = null;
				for (int count = 0; count < browsers.length && browser == null; count++)
					if (Runtime.getRuntime().exec(
							new String[] { "which", browsers[count] })
							.waitFor() == 0)
						browser = browsers[count];
				if (browser == null)
					throw new Exception("Could not find web browser");
				else
					Runtime.getRuntime().exec(new String[] { browser, url });
			}
		} catch (Exception e) {
			UIUtil.showException(null,
					"Error attempting to launch web browser", e);
		}
	}
	
}
