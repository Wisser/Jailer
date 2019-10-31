/*
 * Copyright 2007 - 2016 Ralf Wisser.
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
package net.sf.jailer.ui.databrowser;

import java.io.File;
import java.io.FileReader;
import java.text.SimpleDateFormat;
import java.util.Locale;
import java.util.Map;

import javax.script.Bindings;
import javax.script.Compilable;
import javax.script.CompiledScript;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.SimpleBindings;

/**
 * Filters the content of the browser-table cells.
 * Invokes script "TableContentViewFilter.js" if it exists. Otherwise the content remains unchanged.
 * 
 * @author Ralf Wisser
 */
public class TableContentViewFilter {
	
	private static CompiledScript script;
	private ScriptEngine engine; 
	private SimpleDateFormat df = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss", Locale.ENGLISH);

	/**
	 * Create a filter.
	 * 
	 * @return new filter or <code>null</code>, if the script doesn't exist
	 */
	public static TableContentViewFilter create() {
		File jsFile = new File("TableContentViewFilter.js");
		if (jsFile.exists()) {
			try {
				System.setProperty("rhino.opt.level", "9");
			} catch (Exception e) {
				// ignore
			}
			TableContentViewFilter filter = new TableContentViewFilter();
			filter.engine = new ScriptEngineManager().getEngineByMimeType("text/javascript");
			if (filter.engine == null) {
				filter.engine = new ScriptEngineManager().getEngineByMimeType("application/javascript");
			}
			if (filter.engine != null) {
				try {
					Compilable compilable = (Compilable) filter.engine;
					script = compilable.compile(new FileReader(jsFile));
					// script.eval();
				} catch (Exception e) {
					e.printStackTrace();
					return null;
				}
				return filter;
			}
		}
		return null;
	}

	/**
	 * Filters content
	 * 
	 * @param object cell content
	 * @param name names of the column
	 * @return filtered content
	 */
	public Object filter(Object[] object, Map<String, Integer> name) {
		try {
			Bindings bindings = new SimpleBindings();
			bindings.put("object", object);
			bindings.put("name", name);
			bindings.put("df", df);
			script.eval(bindings);
		} catch (Exception e) {
			e.printStackTrace();
			
		}
		return object;
	}

}
