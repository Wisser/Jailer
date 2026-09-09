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
// TODO test: $-Expressions (parameter, $IS_SUBJECT)

// TODO dokumentieren
// TODO ExportDialog: layout im Buttonbereich unten verbessern
// TODO test: JAILER_ENTITY in eigenem Schema

// TODO
// TODO wenn entitygrahtabellen droppen sind alle EntityGraph-Instanzen, die damit verbunden sind, ungültig. insbes. müssen dann alle DataBrowser, die damit verbunden sind, geschlossen werden

//TODO export im databrowser: "Select Association" sinnvoll?
//TODO databrowser für originAnalysis: wenn entitygrahtabellen droppen oder der mit dem DBrowser verbundene gelöscht wird, soll der DBrowser geschlossen werden

// TODO 
// TODO next big thing: testen, ob retained Entitigraphen nach neustart gelöscht werden und wie (silent?)


// TODO Analyse-Datenbrowser: bei Schliessen nicht layout persistieren


// TODO wenn EntityGraph gelöscht wird (auch bei drop der Tabellen), dann müssen alle DataBrowser, die damit verbunden sind. geschlossen werden

// TODO ExportDialog: "mandatory field empty" warning. Grundsätzlich verbessern. ggf. erwähnen, dass man die "keep" checkbox aktiv sein sollte

// TODO BrCoPa context menu "open path to subject": Icon - nimm grünen Pfeil nach unten. Auch für AnchorControl.

// TODO silent delete EntityGraph nach schliessen ProgressView, dann direkt ExportDialog öffnen: wenn Löschen lange dauert, verzögert sich Laden der Schemainfos im ExportDialog, da selbe Connection und Löschen in einem einzigen DELETE
// TODO Laden Schemainfo ist abbrechbar. Wird es abgebrochen und Schemainfo vom Letzten Mal ist noch da, dann wird es nicht neu geladen. Dann werden die genommen mit JOptionPane, der das erklärt.

// TODO discover assocs: status line: also shown number of already known associations discovered


// TODO extrmodeleditor: titel: "Subject"

