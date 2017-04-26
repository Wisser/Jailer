/*
 * Copyright 2007 - 2017 the original author or authors.
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
import java.net.URL;

/**
 * Loads jar files dynamically.
 * 
 * @author Ralf Wisser
 */
public class ClasspathUtil {
	
	public static URL[] toURLArray(String jarName1, String jarName2)
			throws Exception {
		URL[] urls;
		if (jarName1 != null && jarName1.trim().length() == 0) {
			jarName1 = null;
		}
		if (jarName2 != null && jarName2.trim().length() == 0) {
			jarName2 = null;
		}
		if (jarName1 == null) {
			if (jarName2 == null) {
				return new URL[0];
			}
			jarName1 = jarName2;
			jarName2 = null;
		}
		File file = new File(jarName1);
		if (!file.exists()) {
			throw new FileNotFoundException("Jar-file not found: '" + jarName1 + "'");
		}
		if (jarName2 == null) {
			urls = new URL[] { file.toURI().toURL() };
		} else {
			File file2 = new File(jarName2);
			if (!file2.exists()) {
				throw new FileNotFoundException("Jar-file not found: '" + jarName2 + "'");
			}
			urls = new URL[] { file.toURI().toURL(),
					file2.toURI().toURL() };
		}
		return urls;
	}

}
