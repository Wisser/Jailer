/*
 * Copyright 2007 - 2019 the original author or authors.
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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.HashMap;
import java.util.Map;

import net.sf.jailer.ui.Environment;

/**
 * Persists UI settings.
 * 
 * @author Ralf Wisser
 */
public class UISettings  {

	/**
	 * Name of property (boolean) holding the PLAF setting.
	 */
	public static final String USE_NATIVE_PLAF = "USE_NATIVE_PLAF";

	/**
	 * Persistent properties.
	 */
	private static Map<String, Object> properties;

	/**
	 * The name of the file holding the settings.
	 */
	private static final String FILENAME = ".uisettings";

	@SuppressWarnings("unchecked")
	private static void loadUISettings() {
		if (properties == null) {
			properties = new HashMap<String, Object>();
			File file = Environment.newFile(FILENAME);
			if (file.exists()) {
				try {
					ObjectInputStream in = new ObjectInputStream(new FileInputStream(file));
					properties = (Map<String, Object>) in.readObject();
					in.close();
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		}
	}

	/**
	 * Saves a property.
	 * 
	 * @param name the name of the property
	 * @param value value to store
	 */
	public static synchronized void store(String name, Object value) {
		loadUISettings();
		properties.put(name, value);
		File file = Environment.newFile(FILENAME);
		if (file.exists()) {
			try {
				file.delete();
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		try {
			ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(file));
			out.writeObject(properties);
			out.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Restores a property.
	 * 
	 * @param name the name of the property
	 */
	public static synchronized Object restore(String name) {
		loadUISettings();
		return properties.get(name);
	}

	public static int s1, s2, s3, s4, s5, s6, s7, s8, s9;
	
	public synchronized static void storeStats() {
		int i = 1;
		StringBuilder sb = new StringBuilder();
		for (int s: new int[] { s1, s2, s3, s4, s5, s6, s7, s8, s9 }) {
			if (s != 0) {
				sb.append("&s" + i + "=" + s);
			}
			++i;
		}
		store("stats", sb.toString());
	}
	
	public synchronized static String restoreStats() {
		Object stats = restore("stats");
		if (stats != null) {
			return stats.toString();
		}
		return "";
	}

}
