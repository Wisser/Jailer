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
package net.sf.jailer;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.HashSet;

import org.kohsuke.args4j.Option;

/**
 * Execution context of import-/export commands.
 * 
 * @author Ralf Wisser
 */
public class ExecutionContext {

	public static void main(String args[]) throws Exception {
		HashSet<String> blacklist = new HashSet<String>(Arrays.asList("uTF8", "format", "targetDBMS", "_asXml",
				"xmlRootTag", "xmlDatePattern", "xmlTimePattern", "xmlTimeStampPattern", "withClosures", "explain",
				"undirected", "maxNumberOfEntities", "exportScriptFileName", "schema", "qualifyNames", "analyseAlias",
				"analyseSynonym", "analyseView", "deleteScriptFileName", "tabuFileName", "where", "rawschemamapping",
				"rawsourceschemamapping", "parameters", "numberOfThreads", "numberOfEntities", "upsertOnly", "scope",
				"workingTableSchema", "datamodelFolder", "workingFolder", "jdbcjar", "jdbcjar2", "noSorting",
				"transactional", "noRowid", "importFilterMappingTableSchema", "scriptEnhancer"));

		for (Field f : CommandLine.class.getDeclaredFields()) {
			if (!blacklist.contains(f.getName())) {
				Option option = f.getAnnotation(Option.class);
				if (option != null) {
					String name = f.getName().substring(0, 1).toUpperCase() + f.getName().substring(1);
					String usage = option.usage();
					System.out.println("// " + usage);
					Object value = f.get(new CommandLine());
					if (f.getType() == String.class) {
						value = "\"" + value + "\"";
					}

					System.out.println("/**");
					if (f.getType() == boolean.class) {
						System.out.println(
								" * If <code>true</code>, " + usage.substring(0, 1).toUpperCase() + usage.substring(1));
					} else {
						System.out.println(" * Gets " + usage);
					}
					System.out.println(" *");
					if (f.getType() == boolean.class) {
						System.out.println(" * @return <code>true</code> if " + usage.substring(0, 1).toUpperCase()
								+ usage.substring(1));
					} else {
						System.out.println(" * @return " + usage);
					}
					System.out.println(" */");
					System.out.println("public " + f.getType().getSimpleName() + " get" + name + "() {");
					System.out.println("return " + f.getName() + ";");
					System.out.println("}");
					System.out.println("");

					System.out.println("/**");
					if (f.getType() == boolean.class) {
						System.out.println(
								" * If <code>true</code>, " + usage.substring(0, 1).toUpperCase() + usage.substring(1));
					} else {
						System.out.println(" * Sets " + usage);
					}
					System.out.println(" *");
					if (f.getType() == boolean.class) {
						System.out.println(" * @param " + f.getName() + " <code>true</code> if "
								+ usage.substring(0, 1).toUpperCase() + usage.substring(1));
					} else {
						System.out.println(" * @param " + f.getName() + " " + usage);
					}
					System.out.println(" */");
					System.out.println(
							"public void set" + name + "(" + f.getType().getSimpleName() + " " + f.getName() + ") {");
					System.out.println("this." + f.getName() + " = " + f.getName() + ";");
					System.out.println("}");
					System.out.println("");
				}
			}
		}
		for (Field f : CommandLine.class.getDeclaredFields()) {
			if (!blacklist.contains(f.getName())) {
				Option option = f.getAnnotation(Option.class);
				if (option != null) {
					String usage = option.usage();
					System.out.println("// " + usage);
					Object value = f.get(new CommandLine());
					if (f.getType() == String.class) {
						value = "\"" + value + "\"";
					}
					System.out.println(
							"private " + f.getType().getSimpleName() + " " + f.getName() + " = " + value + ";");
					System.out.println();
				}
			}
		}
		System.out.println("private void copyCommandLineFields(CommandLine commandLine) {");
		for (Field f : CommandLine.class.getDeclaredFields()) {
			if (!blacklist.contains(f.getName())) {
				Option option = f.getAnnotation(Option.class);
				if (option != null) {
					System.out.println(f.getName() + " = commandLine." + f.getName() + ";");
				}
			}
		}
		System.out.println("}");
		System.out.println("");
	}

}