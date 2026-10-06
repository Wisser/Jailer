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
	public static final String VERSION = "17.2.5.1";

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
//Verbesserungsvorschläge Jailer (Engine + GUI)
//
//Context
//
//Ralf bat um allgemeine Verbesserungsvorschläge für Jailer. Test/Doku/Build wurden auf Wunsch ausgeklammert.
//Ergebnis ist eine priorisierte Liste; die Umsetzung erfolgt erst nach Auswahl einzelner Punkte.
//Geprüft habe ich selbst: Punkte E2, E3 und E4 (Quelltext gelesen). Die übrigen stammen aus der Analyse und sind mit Datei:Zeile belegt.
//
//Engine
//
//A - Bugs / Sicherheit
//
//- E1 Alte JDBC-Treiber mit CVEs in lib/: postgresql 42.2.16/42.5.1, sqlite-jdbc 3.28.0 (Default in driverlist.csv), mysql-connector 8.0.21, mysql 5.1.5, mariadb 2.7.3. Zusätzlich liegen Duplikate auf dem
//  Compile-Classpath (build.xml:22-26: zwei Versionen h2, zwei mssql, drei postgresql). CVE-Nummern stammen aus dem Gedächtnis des Agenten und sind vor dem Upgrade gegen eine Advisory-DB zu prüfen.
//- E2 MySQL-Treiberklasse (geprüft): driverlist.csv nutzt com.mysql.jdbc.Driver mit Connector/J 8, richtig ist com.mysql.cj.jdbc.Driver. Einzeilige Änderung.
//- E3 Kollision der Entity-Graph-IDs (geprüft): EntityGraph.java:445-449 startet bei currentTimeMillis() % 30000 und zählt hoch. Zwei JVMs auf denselben globalen Working Tables können überlappende IDs
//  bekommen. Der TODO in DDLCreator.java:103 beschreibt genau das. Billiger Fix: zufälliger Startwert über den gesamten Bereich (z. B. SecureRandom bis Integer.MAX_VALUE/4) statt 0..30000. Ein
//  Heartbeat/Registry wäre die Vollversion.
//- E4 Script-Import zerlegt mehrzeilige String-Literale (Parser geprüft): SqlScriptExecutor.java:282 trimmt jede Zeile, :289 behandelt ---Zeilenanfänge als Kommentar, :320 beendet ein Statement bei ; am
//  Zeilenende. Das betrifft DBMS ohne Newline-Escape in jailer.json (SQLite, Derby, HSQL, Firebird, Informix, Sybase, ClickHouse). Noch per Roundtrip-Test zu bestätigen.
//- E5 XXE in XmlUtil.parse (xml/XmlUtil.java:482-486): DTD und External Entities abschalten.
//- E6 ShellScriptBasedStatisticRenovator (:65-72): Runtime.exec(String), waitFor() vor dem Lesen von stdout (möglicher Deadlock), stderr wird nie gelesen, Exit-Code ignoriert. Umbau auf ProcessBuilder mit
//  redirectErrorStream (betrifft DB2).
//
//B - Billige Gewinne in jailer.json
//
//- E7 -limit-transaction-size wirkungslos bei PG, H2, SQLite, DB2, HSQL (DBMS.java:1859, LimitTransactionSizeInfo.java:55): statementSuffix ergänzen und warnen, wenn die Option nicht greift.
//- E8 Multi-Row-Inserts für SQLite (ab 3.7.11), HSQL und Derby aktivieren.
//- E9 Fehlende Queries pro DBMS: identityColumnsQuery für MSSQL, MySQL und H2; statisticRenovator (ANALYZE) für H2 und SQLite.
//
//C - Funktionale Erweiterungen
//
//- E10 Native Upserts: MERGE für MSSQL, ON CONFLICT für PG, ON DUPLICATE KEY für MySQL (UPSERT_MODE, DMLTransformer.java:504,614).
//- E11 Tabellentypen konfigurierbar (TODO in JDBCMetaDataBasedModelElementFinder.java:879): Materialized Views für Oracle und DB2.
//- E12 CLI: mehrere -schema bei build-model (ModelBuilder.java:599), beliebig viele jdbcjar-Optionen statt vier, JDBC-Properties, Credentials aus Umgebungsvariablen.
//
//D - Aufräumen
//
//- E13 Verschluckte Exceptions: XmlUtil.java:168,212, SubsettingEngine.java:1928, Session.java:486-514,577,721. Mindestens loggen.
//- E14 Weitere Aufräumpunkte: commons-lang3 auf 3.18+ (CVE-2025-48924), das deprecated newInstance() in BasicDataSource.java:239.
//
//GUI
//
//A - Datenschutz / Bugs (AI-Assistent)
//
//- G1 AI-Fehler senden Schema und Prompt an sourceforge (geprüft): AIQueryAssistant.java:832 hängt Headers (Key maskiert) und den kompletten Request-Body an die IOException. Die Dialoge
//  (AIQueryDialog.java:837, AIExtractionModelDialog.java:387, AIProviderPanel.java:216/243) rufen UIUtil.showException ohne User-Error-Context auf. Damit läuft der Fehler über sendIssue("internal", ...)
//  (UIUtil.java:955) nach issueReport.php: Schema, Tabellennamen und die Frage des Users gehen nach außen, schon bei einem simplen 401 oder 429. Fix: Body aus der Exception-Message entfernen (nur ins
//  Debug-Log schreiben) und HTTP-Fehler als EXCEPTION_CONTEXT_USER_ERROR melden.
//- G2 curl-Fallback bei jedem IOException (geprüft, AIQueryAssistant.java:728-735): Auch HTTP-4xx/5xx und Timeouts lösen einen zweiten Versand aus. Das kostet doppelt und dauert doppelt so lange. Fix: eigene
//  Exception-Klasse für HTTP-Status, kein Fallback bei Status >= 400.
//- G3 curl-Fallback: Der API-Key steht in der Kommandozeile (:905/908) und ist damit in der Prozessliste sichtbar; besser per -H @- oder --config über stdin übergeben. waitFor(60s) steht erst nach dem
//  vollständigen Lesen von stdout (:933-934) und greift deshalb nie; stderr wird nicht parallel gelesen.
//- G4 Read-Timeout 60 s ohne Streaming (:768, :663): Reasoning-Modelle laufen in den Timeout, und der triggert dann G2. Timeout konfigurierbar machen bzw. auf etwa 300 s anheben.
//- G5 NUL-Bytes im Quelltext (geprüft, 2 Stück) in AIQueryDialog.java:1269: Git und grep behandeln die Datei deshalb als binär. Durch \u0000 ersetzen.
//
//B - UX
//
//- G6 Fenstergröße und -position werden nicht gespeichert: kein UISettings.store für Bounds. Ein zentraler Helper neben UIUtil.setDialogSize (UIUtil.java:2641) könnte das für alle Dialoge lösen.
//- G7 Escape schließt Dialoge nicht einheitlich: Nur 4 von rund 38 Dialogen schließen mit Escape, darunter fehlen CompareDialog, RowOriginDialog und die AI-Dialoge. Ein gemeinsames Escape-Binding über einen
//  Helper nachrüsten (nicht in initComponents).
//- G8 Compare Rows synchronisiert LOBs stillschweigend nicht (CompareWithConnection.java:550): Mindestens eine Warnung im UI anzeigen.
//- G9 Data Model Editor ohne Undo und Shortcuts (DataModelEditor.java, TableEditor.java).
//- G10 SQL-Console-History: maximal 100 Einträge, keine Suche (TODO SQLConsole.java:5510).
//- G11 Kopierformate JSON, CSV, Markdown (TODO ExtendetCopyPanel.java:1047).
//- G12 „Open all" auf 12 Tabellen begrenzt: Auswahldialog statt Ja/Nein (TODO BrowserContentPane.java:3154).
//- G13 Save As liegt auf Ctrl+A (ExtractionModelFrame.java:913): Kollidiert mit „Alles markieren". Ctrl+Shift+S wäre der Standard. Die Zeile steht in initComponents, also nur über die .form-Datei in NetBeans
//  änderbar.
//- G14 Kleinkram: hart codierte Farben in AIProviderPanel.java:204/212 und AIQueryDialog.java:296 statt Colors; nackte new JFileChooser() statt UIUtil.choseFile in SQLConsoleChartPanel.java:1228/1251 und
//  TabContentPanel.java:297; TODO in dark.xml:20.
//
//C - Wartbarkeit
//
//- G15 Doppelter Code mit DB-Query auf dem EDT: showEntityGraphsDump ist doppelt vorhanden (DataBrowser.java:7682, ExtractionModelFrame.java:544) und fragt die DB synchron auf dem EDT ab.
//- G16 BrowserContentPane.java hat 11.300 Zeilen: Row-Origin- und Subset-Insight-Code (:5843-6460) ins progress/-Package auslagern.
//- G17 Offene Ideen (Bestand): Credentials nur obfuskiert, kein SSH-Tunnel, kein JDBC-Properties-Editor.
//
//Empfohlene Reihenfolge
//
//0. G1 zuerst: Datenabfluss an einen externen Dienst. Danach G2 und G5.
//1. E2, E3, E5 (klein, risikoarm, echte Fehler)
//2. E4 per Roundtrip bestätigen, dann den Parser fixen
//3. E7, E8, E9 (nur jailer.json)
//4. E1 mit Treiber-Upgrade und Bereinigung der Duplikate
//
//Verifikation
//
//- Nur Compile-Check mit JDK 8 nach dem Rezept in der Memory, kein Build und kein Start durch mich.
//- E3: zwei parallele Exporte mit globalen Working Tables; die IDs dürfen sich nicht überlappen.
//- E4: Export nach SQLite mit Werten, die \n--x und abc;\n enthalten, dann Import und Vergleich.
//- E7/E8: Export gegen PG, H2 und SQLite; das erzeugte Script prüfen.
//- Ralf baut und testet die Anwendung selbst.