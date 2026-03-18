/*
 * Copyright 2007 - 2026 Ralf Wisser.
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

import java.io.BufferedOutputStream;
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

/**
 * Utility class for downloading files over HTTP.
 */
public class HttpDownload {

	public static final String DOWNLOADFOLDER = "downloads";

	/**
	 * Downloads the resource at the given URL and returns the local file path.
	 *
	 * @param url the URL to download
	 * @param volConsumer optional consumer called periodically with the number of bytes received since the last call, or {@code null}
	 * @return the absolute path of the downloaded file
	 * @throws Throwable if the download fails
	 */
	public static String get(final String url, Consumer<Long> volConsumer) throws Throwable {
		Throwable t = null;
		StringBuilder result = new StringBuilder();
		try {
			get(url, result, volConsumer);
			return result.toString();
		} catch (Throwable err) {
			t = err;
		}
		try {
			result = new StringBuilder();
			Process p = Runtime.getRuntime().exec("java -classpath " + Environment.newWorkingFolderFile("jailer.jar").getPath() + " -Djava.net.useSystemProxies=true " + HttpDownload.class.getName() + " " + url); // lgtm [java/concatenated-command-line]
			BufferedReader input = new BufferedReader(new InputStreamReader(p.getInputStream()));
			String line;
	
			while ((line = input.readLine()) != null) {
				result.append(line);
			}
	            
			input.close();
		} catch (Throwable err) {
			err.printStackTrace();
			throw t;
		}
		if (result.length() == 0) {
			throw t;
		}
		return result.toString();
	}

	/**
	 * Downloads the resource at the given URL and appends the local file path to {@code result}.
	 *
	 * @param url the URL to download
	 * @param result the builder to which the local file path is appended
	 * @param volConsumer optional consumer called periodically with the number of bytes received since the last call, or {@code null}
	 * @throws MalformedURLException if the URL is malformed
	 * @throws IOException if an I/O error occurs during the download
	 */
	public static void get(final String url, final StringBuilder result, Consumer<Long> volConsumer) throws MalformedURLException, IOException {
		URL theUrl;
		theUrl = new URL(url);
		long t0 = System.currentTimeMillis();
		String name = toFileName(theUrl);
		final File dir = Environment.newFile(DOWNLOADFOLDER);
		dir.mkdir();
		File file = new File(dir, name);
		int rc = 200;
		if (!file.exists()) {
			URLConnection con = theUrl.openConnection();
			((HttpURLConnection) con).setInstanceFollowRedirects(true);
			InputStream in = con.getInputStream();
			rc = ((HttpURLConnection) con).getResponseCode();
			if (rc == 200) {
				File tmpFile = new File(dir, name + "." + System.currentTimeMillis());
				OutputStream out = new BufferedOutputStream(new FileOutputStream(tmpFile));
				int c;
				long total = 0;
				long vol = 0;
				while ((c = in.read()) != -1) {
					out.write((char) c);
					++total;
					++vol;
					long t1 = System.currentTimeMillis();
					if (t1 - t0 > 100) {
						t0 = t1;
						if (volConsumer != null) {
							volConsumer.accept(vol);
						}
						vol = 0;
					}
				}
				out.close();
				if (!tmpFile.renameTo(file)) {
					throw new HttpException("can't rename \"" + tmpFile.getAbsolutePath() + "\"");
				};
				if (total < 1024 * 100L) {
					throw new HttpException("download failed");
				}
			}
			in.close();
		}
		result.append(new File(dir, name).getAbsolutePath());
		if (rc != 200) {
			throw new HttpException("Response code " + rc + " received");
		}
	}

	/**
	 * Derives a local file name from the path component of the given URL.
	 *
	 * @param theUrl the URL whose path is used to derive the file name
	 * @return the file name
	 */
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
			// ignore
		}
	}

	@SuppressWarnings("serial")
	public static class HttpException extends RuntimeException {
		public HttpException(String message) {
			super(message);
		}
	}
}
