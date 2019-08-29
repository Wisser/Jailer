/*
 * Copyright 2007 - 2019 Ralf Wisser.
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
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.Environment;
import net.sf.jailer.ui.databrowser.BookmarksPanel;
import net.sf.jailer.ui.databrowser.BookmarksPanel.BookmarkId;

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
	 * Name of property holding the "recent files".
	 */
	public static final String RECENT_FILES = "RECENT_FILES";

	/**
	 * Name of property holding the "recent connection aliases".
	 */
	public static final String RECENT_ALIASES = "RECENT_ALIASES";

	/**
	 * Name of property holding the "recent bookmarks".
	 */
	public static final String RECENT_BOOKMARKS = "RECENT_BOOKMARKS";
	
	/**
	 * Maximum size of any "recent" list.
	 */
	private final static int MAX_RECENT_LIST_SIZE = 12;

	/**
	 * Persistent properties.
	 */
	private static Map<String, Object> properties;

	/**
	 * The name of the file holding the settings.
	 */
	private static final String FILENAME = ".uisettings";

	@SuppressWarnings("unchecked")
	private static synchronized void loadUISettings() {
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
		for (int retry = 0; retry < 4; ++retry) {
			try {
				ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(file));
				out.writeObject(properties);
				out.close();
				return;
			} catch (Exception e) {
				e.printStackTrace();
			}
			try {
				Thread.sleep(100);
			} catch (InterruptedException e) {
				// ignore
			}
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
	public static String s10;
	
	public synchronized static void storeStats() {
		int i = 1;
		StringBuilder sb = new StringBuilder();
		for (int s: new int[] { s1, s2, s3, s4, s5, s6, s7, s8, s9 }) {
			if (s != 0) {
				sb.append("&s" + i + "=" + s);
			}
			++i;
		}
		if (s10 != null) {
			sb.append("&s10=" + s10);
		}
		store("stats", sb.toString());
	}

	public synchronized static String restoreStats() {
		Object stats = restore("stats");
		if (stats != null) {
			store("stats", null);
			return stats.toString();
		}
		return "";
	}

	public static void dmStats(DataModel dataModel) {
		if (dataModel != null) {
			s1 = Math.max(UISettings.s1, dataModel.getTables().size());
			ArrayList<Integer> nc = new ArrayList<Integer>();
			int numA = 0;
			for (Table table: dataModel.getTables()) {
				nc.add(table.getColumns().size());
				if (table.associations != null) {
					numA += table.associations.size();
				}
			}
			if (!nc.isEmpty()) {
				Collections.sort(nc);
				int mid = Math.min(Math.max(nc.size() / 2, 0), nc.size() - 1);
				s8 = Math.min(nc.get(mid), 999) + 1000 * nc.get(nc.size() - 1);
				s5 = (s5 % 1000) + 1000 * (numA / 2);
			}
		}
	}

	private static boolean isSbeModel(File file) {
		try {
			File pFile = file.getParentFile();
			return pFile != null && pFile.getName() != null && "by-example".equals(pFile.getName());
		} catch (Throwable t) {
			return false;
		}
	}

	@SuppressWarnings("unchecked")
	public static List<File> loadRecentFiles() {
		Object files = restore(RECENT_FILES);
		List<File> result = new ArrayList<File>();
		if (files instanceof List) {
			for (File file: (List<File>) files) {
				if (!isSbeModel(file) && !Configuration.getInstance().isTempFile(file)) {
					result.add(file);
				}
			}
		}
		return result;
	}

	public static void addRecentFile(File file) {
		if (!isSbeModel(file) && !Configuration.getInstance().isTempFile(file)) {
			List<File> files = loadRecentFiles();
			files.remove(file);
			files.add(0, file);
			if (MAX_RECENT_LIST_SIZE < files.size()) {
				files.remove(files.size() - 1);
			}
			store(RECENT_FILES, files);
		}
	}

	@SuppressWarnings("unchecked")
	public static List<String> loadRecentConnectionAliases() {
		Object files = restore(RECENT_ALIASES);
		List<String> result = new ArrayList<String>();
		if (files instanceof List) {
			for (String alias: (List<String>) files) {
				result.add(alias);
			}
		}
		return result;
	}
	
	public static void addRecentConnectionAliases(String alias) {
		List<String> aliases = loadRecentConnectionAliases();
		aliases.remove(alias);
		aliases.add(0, alias);
		if (MAX_RECENT_LIST_SIZE < aliases.size()) {
			aliases.remove(aliases.size() - 1);
		}
		store(RECENT_ALIASES, aliases);
	}


	@SuppressWarnings("unchecked")
	public static List<BookmarkId> loadRecentBookmarks() {
		Object bookmarks = restore(RECENT_BOOKMARKS);
		List<BookmarkId> result = new ArrayList<BookmarkId>();
		if (bookmarks instanceof List) {
			for (BookmarkId bm: (List<BookmarkId>) bookmarks) {
				result.add(bm);
			}
		}
		return result;
	}
	
	public static void addRecentBookmarks(BookmarksPanel.BookmarkId bookmark) {
		if (bookmark.connectionAlias != null && bookmark.datamodelFolder != null) {
			List<BookmarkId> bookmarks = loadRecentBookmarks();
			bookmarks.remove(bookmark);
			bookmarks.add(0, bookmark);
			if (MAX_RECENT_LIST_SIZE < bookmarks.size()) {
				bookmarks.remove(bookmarks.size() - 1);
			}
			store(RECENT_BOOKMARKS, bookmarks);
		}
	}

}
