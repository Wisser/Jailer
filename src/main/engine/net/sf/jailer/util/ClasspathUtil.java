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
package net.sf.jailer.util;

import java.io.File;
import java.io.FileNotFoundException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 * Loads jar files dynamically.
 * 
 * @author Ralf Wisser
 */
public class ClasspathUtil {
	
	public static URL[] toURLArray(String jarName1, String jarName2, String jarName3, String jarName4) throws FileNotFoundException {
		if (jarName1 != null && jarName1.trim().length() == 0) {
			jarName1 = null;
		}
		if (jarName2 != null && jarName2.trim().length() == 0) {
			jarName2 = null;
		}
		if (jarName3 != null && jarName3.trim().length() == 0) {
			jarName3 = null;
		}
		if (jarName4 != null && jarName4.trim().length() == 0) {
			jarName4 = null;
		}
		
		Set<String> jars = new LinkedHashSet<String>();
		if (jarName1 != null) {
			jars.add(jarName1);
		}
		if (jarName2 != null) {
			jars.add(jarName2);
		}
		if (jarName3 != null) {
			jars.add(jarName3);
		}
		if (jarName4 != null) {
			jars.add(jarName4);
		}

		List<URL> urlList = new ArrayList<URL>();

		for (String fn: jars) {
			File file = new File(fn);
			if (!file.exists() && !file.isAbsolute()) {
				File home = new File(System.getProperty("user.home"), ".jailer");
				File uFile = new File(home, fn);
				if (uFile.exists()) {
					file = uFile;
				}
			}
			if (!file.exists()) {
				throw new FileNotFoundException("Driver jar file not found: '" + fn + "'");
			}
			try {
				urlList.add(file.toURI().toURL());
			} catch (MalformedURLException e) {
				throw new RuntimeException(e);
			}
		}
		
		return urlList.toArray(new URL[0]);
	}

}
