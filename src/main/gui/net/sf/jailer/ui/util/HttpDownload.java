/*
 * Copyright 2007 - 2020 Ralf Wisser.
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
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.function.Consumer;

import net.sf.jailer.ui.Environment;

public class HttpDownload {
	
	public static final String DOWNLOADFOLDER = "download";
	
	public static String get(final String url, Consumer<Long> volConsumer) throws Throwable {
		Throwable t = null;
		StringBuilder result = new StringBuilder();
		try {
			if (get(url, result, volConsumer) == 200) {
				return result.toString();
			}
		} catch (Throwable err) {
			t = err;
		}
		try {
			result = new StringBuilder();
			Process p = Runtime.getRuntime().exec("java -classpath " + Environment.newWorkingFolderFile("jailer.jar").getPath() + " -Djava.net.useSystemProxies=true " + HttpDownload.class.getName() + " " + url);
			BufferedReader input = new BufferedReader(new InputStreamReader(p.getInputStream()));
			String line;
	
			while ((line = input.readLine()) != null) {
				result.append(line);
			}
	            
			input.close();
		} catch (Throwable err) {
			err.printStackTrace();
			if (t != null) {
				throw t;
			}
		}
		return result.toString();
	}

	public static int get(final String url, final StringBuilder result, Consumer<Long> volConsumer) throws MalformedURLException, IOException {
		URL theUrl;
		theUrl = new URL(url);
		long t0 = System.currentTimeMillis();
		URLConnection con = theUrl.openConnection();
		((HttpURLConnection) con).setInstanceFollowRedirects(true);
		InputStream in = con.getInputStream();
		int rc = ((HttpURLConnection) con).getResponseCode();
		if (rc == 200) {
			File dir = Environment.newFile(DOWNLOADFOLDER);
			dir.mkdir();
			String name = toFileName(theUrl);
			File tmpFile = new File(dir, name + "." + System.currentTimeMillis());
			File file = new File(dir, name);
			if (!file.exists()) {
				OutputStream out = new FileOutputStream(tmpFile);
				int c;
				long total = 0;
				long vol = 0;
				while ((c = in.read()) != -1) {
					out.write((char) c);
					++total;
					++vol;
					long t1 = System.currentTimeMillis();
					if (t1 - t0 > 1000) {
						t0 = t1;
						if (volConsumer != null) {
							volConsumer.accept(vol);
						}
						vol = 0;
					}
				}
				out.close();
				if (!tmpFile.renameTo(file)) {
					throw new RuntimeException("can't rename \"" + tmpFile.getAbsolutePath() + "\"");
				};
				if (total < 1024 * 100L) {
					return 0;
				}
			}
			in.close();
			result.append(new File(DOWNLOADFOLDER, name).getAbsolutePath());
		}
		return rc;
	}

	public static String toFileName(URL theUrl) {
		String name = theUrl.getPath().replaceAll("^.*/([^/]+)$", "$1");
		return name;
	}

	public static void main(String args[]) {
		final StringBuilder result = new StringBuilder();
		try {
			get(args[0], result, null);
			System.out.println(result);
		} catch (Throwable e) {
			e.printStackTrace();
		}
	}

}
