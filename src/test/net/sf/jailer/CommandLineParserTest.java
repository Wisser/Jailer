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

import junit.framework.TestCase;

/**
 * Tests {@link CommandLineParser}
 *
 * @author Ralf Wisser
 */
public class CommandLineParserTest extends TestCase {

	public void testIsHelpRequest() throws Exception {
		assertTrue(CommandLineParser.isHelpRequest(new String[] { "-h" }));
		assertTrue(CommandLineParser.isHelpRequest(new String[] { "-help" }));
		assertTrue(CommandLineParser.isHelpRequest(new String[] { "--help" }));

		assertFalse(CommandLineParser.isHelpRequest(new String[0]));
		assertFalse(CommandLineParser.isHelpRequest(new String[] { "export" }));
		assertFalse(CommandLineParser.isHelpRequest(new String[] { "-e", "-h" }));
	}

	public void testIsVersionRequest() throws Exception {
		assertTrue(CommandLineParser.isVersionRequest(new String[] { "-v" }));
		assertTrue(CommandLineParser.isVersionRequest(new String[] { "-version" }));
		assertTrue(CommandLineParser.isVersionRequest(new String[] { "--version" }));

		assertFalse(CommandLineParser.isVersionRequest(new String[0]));
		assertFalse(CommandLineParser.isVersionRequest(new String[] { "export" }));
		assertFalse(CommandLineParser.isVersionRequest(new String[] { "-e", "-v" }));
	}

}
