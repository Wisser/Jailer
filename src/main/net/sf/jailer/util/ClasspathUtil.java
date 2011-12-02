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
package net.sf.jailer.util;

import java.io.File;
import java.io.FileNotFoundException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.HashMap;
import java.util.Map;

import org.apache.log4j.Logger;

/**
 * Loads jar files dynamically.
 * 
 * @author Ralf Wisser
 */
public class ClasspathUtil {
	
	/**
     * The logger.
     */
    private static final Logger _log = Logger.getLogger(ClasspathUtil.class);
    
    /**
	 * Holds all class-loader in order to prevent loading a jar twice.
	 */
	private static Map<String, URLClassLoader> classloaders = new HashMap<String, URLClassLoader>();

	/**
	 * Adds one or two jars to classpath.
	 * 
	 * @param jarName1
	 *            filename of jar 1
	 * @param jarName2
	 *            filename of jar 2
	 */
	public static synchronized URLClassLoader addJarToClasspath(String jarName1, String jarName2)
			throws Exception {
		String mapKey = jarName1 + "," + jarName2;
		if (classloaders.containsKey(mapKey)) {
			return classloaders.get(mapKey);
		}
		URL[] urls;
		if (jarName1 == null) {
			if (jarName2 == null) {
				return null;
			}
			jarName1 = jarName2;
			jarName2 = null;
		}
		File file = new File(jarName1);
		if (!file.exists()) {
			throw new FileNotFoundException("Jar-file not found: '" + jarName1 + "'");
		}
		_log.info("added '" + jarName1 + "' to classpath");
		if (jarName2 == null) {
			urls = new URL[] { file.toURI().toURL() };
		} else {
			File file2 = new File(jarName2);
			if (!file2.exists()) {
				throw new FileNotFoundException("Jar-file not found: '" + jarName2 + "'");
			}
			_log.info("added '" + jarName2 + "' to classpath");
			urls = new URL[] { file.toURI().toURL(),
					file2.toURI().toURL() };
		}
		URLClassLoader urlLoader = new URLClassLoader(urls);
		classloaders.put(mapKey, urlLoader);
		return urlLoader;
	}

}
