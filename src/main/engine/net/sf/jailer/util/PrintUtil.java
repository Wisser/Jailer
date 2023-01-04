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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.GZIPInputStream;
import java.util.zip.ZipInputStream;

import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.datamodel.Table;

/**
 * Utility for printing.
 * 
 * @author Ralf Wisser
 */
public class PrintUtil {
	
	public static final String LINE_SEPARATOR = System.getProperty("line.separator", "\n");

	/**
	 * Converts a set of tables into a string.
	 * 
	 * @param tables the set
	 */
	public String tableSetAsString(Set<Table> tables) {
		return tableSetAsString(tables, "      ");
	}
	
	/**
	 * Converts a set of tables into a string.
	 * 
	 * @param tables the set
	 */
	public String tableSetAsString(Set<Table> tables, String linePrefix) {
		List<String> tableNames = new ArrayList<String>();
		for (Table table: tables) {
			tableNames.add(table.getName());
		}
		Collections.sort(tableNames);
		StringBuffer str = new StringBuffer();
		str.append(" { ");
		int i = 1;
		for (String tableName: tableNames) {
			if (i > 1) {
				str.append(", ");
			}
			if (i % 5 == 0 && linePrefix != null) {
				str.append("\n" + linePrefix);
			}
			str.append(tableName);
			++i;
		}
		str.append(" }");
		return str.toString();
	}

	/**
	 * Cache for {@link #applyTemplate(String, Object[])}.
	 */
	private Map<String, String> templateCache = new HashMap<String, String>();

	/**
	 * Loads a file.
	 * 
	 * @param name the file to load
	 * @return content of file
	 * @throws FileNotFoundException
	 * @throws IOException
	 */
	public String loadResource(String name) throws FileNotFoundException, IOException {
		StringBuffer sb;
		sb = new StringBuffer();
		File newFile;
		if (Configuration.applicationBase == null) {
			newFile = new File(name);
		} else {
			newFile = new File(Configuration.applicationBase, name);
		}
		BufferedReader reader;
		if (newFile.exists()) {
			reader = new BufferedReader(new FileReader(newFile));
		} else {
			InputStream in = getClass().getResourceAsStream("/net/sf/jailer/" + name.replace('\\', '/'));
			if (in == null) {
				throw new FileNotFoundException(newFile.getPath());
			}
			reader = new BufferedReader(new InputStreamReader(in));
		}
		String line = null;
		while ((line = reader.readLine()) != null) {
			sb.append(line + "\n");
		}
		reader.close();
		return sb.toString();
	}

	/**
	 * Applies arguments to template.
	 * 
	 * @param template file name of template
	 * @param arguments the arguments
	 * 
	 * @return template with arguments filled in
	 */
	public String applyTemplate(String template, Object[] arguments) throws FileNotFoundException, IOException {
		String sb = templateCache.get(template);
		if (sb == null) {
			sb = loadResource(template);
			templateCache.put(template, sb);
		}
		
		return MessageFormat.format(sb, arguments);
	}

	/**
	 * Applies arguments to template.
	 * 
	 * @param template file name of template
	 * @param arguments the arguments (named values)
	 * 
	 * @return template with arguments filled in
	 */
	public String applyTemplate(String template, Map<String, String> arguments, Map<String, List<String>> listArguments) throws FileNotFoundException, IOException {
		String sb = templateCache.get(template);
		if (sb == null) {
			sb = loadResource(template);
			templateCache.put(template, sb);
		}
		
		for (Map.Entry<String, String> e: arguments.entrySet()) {
			sb = sb.replaceAll(Pattern.quote("${" + e.getKey() + "}"), Matcher.quoteReplacement(e.getValue())); 
		}
		
		for (;;) {
			int begin = sb.indexOf("${for-each:");
			int end = sb.indexOf("${end}");
			
			if (begin >= 0 && end >= 0) {
				String pre = sb.substring(0, begin);
				String woPre = sb.substring(begin + 11);
				int i = woPre.indexOf('}');
				String content = woPre.substring(i + 1, end - begin - 11);
				String suf = sb.substring(end + 6);
				String cContent = "";
				int index = 1;
				if (listArguments != null) {
					for (String var : listArguments.get(woPre.substring(0, i))) {
						cContent += content.replaceAll("\\$i", "" + (index++)).replaceAll("\\$",
								Matcher.quoteReplacement(var));
					}
				}
				sb = pre + cContent + suf;
			} else {
				break;
			}
		}
		
		return sb;
	}

	/**
	 * Loads table list file and fill a list.
	 * 
	 * @param list
	 *            to fill
	 * @param fileName
	 *            name of file
	 */
	public static void loadTableList(List<String> list, String fileName)
			throws IOException {
		File file = new File(fileName);
		if (file.exists()) {
			BufferedReader in = new BufferedReader(new FileReader(file));
			String line;
			while ((line = in.readLine()) != null) {
				line = line.trim();
				if (line.length() > 0) {
					list.add(line);
				}
			}
			in.close();
		}
	}

	/**
	 * Loads table list file and fill a list.
	 * 
	 * @param list
	 *            to fill
	 * @param in to read from
	 */
	public static void loadTableList(List<String> list, InputStream in)
			throws IOException {
		if (in != null) {
			BufferedReader ir = new BufferedReader(new InputStreamReader(in));
			String line;
			while ((line = ir.readLine()) != null) {
				line = line.trim();
				if (line.length() > 0) {
					list.add(line);
				}
			}
			ir.close();
		}
	}

	/**
	 * Loads a file.
	 * 
	 * @param file the file to load
	 * @return content of file
	 * @throws FileNotFoundException
	 * @throws IOException
	 */
	public String loadFile(String file) throws FileNotFoundException, IOException {
		return loadFile(file, false);
	}
	
	/**
	 * Loads a file.
	 * 
	 * @param file the file to load
	 * @return content of file
	 * @throws FileNotFoundException
	 * @throws IOException
	 */
	public String loadFile(String file, boolean ignoreComments) throws FileNotFoundException, IOException {
		StringBuilder sb;
		sb = new StringBuilder(65536);
		Charset encoding = SqlUtil.retrieveEncoding(file);
		InputStream inputStream = new FileInputStream(file);
		BufferedReader reader;
		if (file.toLowerCase(Locale.ENGLISH).endsWith(".gz")) {
			reader = new BufferedReader(new InputStreamReader(new GZIPInputStream(inputStream), encoding));
		} else if (file.toLowerCase(Locale.ENGLISH).endsWith(".zip")){
			ZipInputStream zis = new ZipInputStream(inputStream); // lgtm [java/input-resource-leak]
			zis.getNextEntry();
			reader = new BufferedReader(new InputStreamReader(zis, encoding));
		} else {
			reader = new BufferedReader(new InputStreamReader(inputStream, encoding));
		}
		String line = null;
		while ((line = reader.readLine()) != null) {
			if (!ignoreComments || (line.trim().length() > 0 && !line.startsWith("#"))) {
				sb.append(line + "\n");
			}
		}
		reader.close();
		return sb.toString();
	}

	public static String formatVitalTime(long time) {
		long et = time / 100;
		long hs = et % 10;
		long sec = (et / 10) % 60;
		long min = (et / 600) % 60;
		long h = et / 36000;
		return (h<10? "0" : "") + h + ":" + (min<10? "0" : "") + min + ":" + (sec<10? "0" : "") + sec + "." + hs;
	}

}
