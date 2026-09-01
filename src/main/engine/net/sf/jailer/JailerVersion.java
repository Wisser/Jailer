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
package net.sf.jailer;

/**
 * The Jailer Version.
 *
 * @author Ralf Wisser
 */
public class JailerVersion {
	
	/**
	 * The Jailer version.
	 */
	public static final String VERSION = "17.2.2.1";

	/**
	 * The Jailer working tables version.
	 */
	public static final int WORKING_TABLE_VERSION = 4;

	/**
	 * The Jailer application name.
	 */
	public static final String APPLICATION_NAME = "Jailer";

	/**
	 * Prints version.
	 *
	 * @param args command-line arguments (unused)
	 */
	public static void main(String[] args) {
		System.out.print(VERSION);
	}

}

// TODO
// TODO project_subset_insight.md, Ideen für Aufräumen (Entity-Graph weg)
// TODO discover: 100% bei "trusted" rules

// TODO
// TODO project_subset_insight.md:
// TODO tables: adjust width
// TODO statt "possible predecessors" tablelle den "show zcollected rows" dialog hier einbetten
// TODO Tabellen mit full text search und limitierung
// TODO ohne "keep" checkbox: im Analyse-Tab erwähnen, dass man die "keep" checkbox aktivieren muss
// TODO test: $-Expressions (parameter, $IS_SUBJECT)
// TODO Analyse-Tab: auch Tabellen (neben Associations) anzeigen, und deren Rows dann darunter
// TODO im "Export"-Tab (Fortschrittstabelle): popup menu mit "open databrowser (und doppelklick). Im Analyse-Tab ist es drin.
// TODO im Analyse-Tab: erklärtext, was das kann und wie man es benutzt
// TODO dokumentieren
// TODO aufräumen alter Entity-Graphen ermöglichen. Vielleicht Timestamp in JAILER_PROPERTY?
// TODO ExportDialog: layout im Buttonbereich unten verbessern
// TODO "Export"-Tab: Doppelklick und popup für "open in DataBrowser", Kette zum Subjekt öffnen
// TODO "Analye"-Tab: Doppelklick -> "open in DataBrowser"
// TODO kein öffnen von datenbrowser wenn nicht "keep" gesetzt ist
// TODO neue button im hover over menü oft disabled

// TODO
// TODO ExtractionModelEditor - graphview: associationrendering verbessern

// TODO export im databrowser zb. kann entitygrahtabellen droppen. dann sind retained-graphen ungültig

