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
package net.sf.jailer.ui.databrowser;

import net.sf.jailer.JailerVersion;

/**
 * Holds information about the context, in which the data browser is used.
 * 
 * @author Ralf Wisser
 */
public class DataBrowserContext {

	public static String getAppName(boolean shortName) {
		return JailerVersion.APPLICATION_NAME + " " + JailerVersion.VERSION + (shortName? "" : " - Relational Data Browser");
	}
	
	public static String getAppName() {
		return JailerVersion.APPLICATION_NAME;
	}
	
}
