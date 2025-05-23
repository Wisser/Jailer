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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;

import net.sf.jailer.ui.Environment;

public class HttpUtil {
	
	public static String get(final String url) {
		final StringBuilder result = new StringBuilder();
		try {
			int rc = get(url, result);
			if (rc == 200) {
				return result.toString();
			}
			if (rc == 403) {
				return "403";
			}
		} catch (Throwable e) {
			// fall through
		}
	    try {
	        Process p = Runtime.getRuntime().exec("java -classpath " + Environment.newWorkingFolderFile("jailer.jar").getPath() + " -Djava.net.useSystemProxies=true " + HttpUtil.class.getName() + " " + url); // lgtm [java/concatenated-command-line]
        	BufferedReader input = new BufferedReader(new InputStreamReader(p.getInputStream()));
            String line;

            while ((line = input.readLine()) != null) {
                result.append(line + "\n");
            }
            
            input.close();
	    } catch (Exception err) {
	       // ignore
	    }
		return result.toString();
	}

	private static int get(final String url, final StringBuilder result) throws MalformedURLException, IOException {
		URL theUrl;
		theUrl = new URL(url);
		URLConnection con = theUrl.openConnection();
		int rc = ((HttpURLConnection) con).getResponseCode();
		if (rc == 403) {
			return rc;
		}
		String redirect = con.getHeaderField("Location");
		int MAX_REDIRECTS = 8;
		for (int i = 0; i < MAX_REDIRECTS  ; i++) {
            if (redirect != null) {
            	con = new URL(redirect).openConnection();
                redirect = con.getHeaderField("Location");
            } else {
                break;
            }
        }
		InputStreamReader in = new InputStreamReader(con.getInputStream()); // lgtm [java/input-resource-leak] 
		int c;
		while ((c = in.read()) != -1) {
			result.append((char) c);
		}
		return rc;
	}

	public static void main(String args[]) {
		final StringBuilder result = new StringBuilder();
		try {
			get(args[0] + "&withproxy=1", result);
			System.out.println(result);
		} catch (Throwable e) {
			// ignore
		}
	}

}
