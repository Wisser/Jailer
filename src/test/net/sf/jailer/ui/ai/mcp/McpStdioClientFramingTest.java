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
package net.sf.jailer.ui.ai.mcp;

import java.io.IOException;

import junit.framework.TestCase;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

/**
 * Tests {@link McpStdioClient}'s message framing (newline-delimited, compact JSON;
 * no process/IO involved).
 */
public class McpStdioClientFramingTest extends TestCase {

	private final ObjectMapper mapper = new ObjectMapper();

	public void testEncodeMessageIsSingleNewlineTerminatedLine() throws Exception {
		ObjectNode msg = mapper.createObjectNode();
		msg.put("jsonrpc", "2.0");
		msg.put("id", 1);
		msg.put("method", "tools/list");

		String encoded = McpStdioClient.encodeMessage(msg, mapper);

		assertTrue(encoded.endsWith("\n"));
		String withoutTerminator = encoded.substring(0, encoded.length() - 1);
		assertFalse("must not contain an embedded newline", withoutTerminator.contains("\n"));
		assertFalse("must not contain an embedded carriage return", withoutTerminator.contains("\r"));
	}

	public void testEncodeThenDecodeRoundTrips() throws Exception {
		ObjectNode msg = mapper.createObjectNode();
		msg.put("jsonrpc", "2.0");
		msg.put("id", 42);
		msg.put("method", "tools/call");
		ObjectNode params = msg.putObject("params");
		params.put("name", "read_file");
		params.putObject("arguments").put("path", "schema.sql");

		String encoded = McpStdioClient.encodeMessage(msg, mapper);
		String line = encoded.substring(0, encoded.length() - 1); // strip the line terminator, as a reader would
		JsonNode decoded = McpStdioClient.decodeLine(line, mapper);

		assertEquals("2.0", decoded.path("jsonrpc").asText());
		assertEquals(42, decoded.path("id").asInt());
		assertEquals("tools/call", decoded.path("method").asText());
		assertEquals("read_file", decoded.path("params").path("name").asText());
		assertEquals("schema.sql", decoded.path("params").path("arguments").path("path").asText());
	}

	public void testDecodeMalformedLineThrowsIOException() {
		try {
			McpStdioClient.decodeLine("not json at all {", mapper);
			fail("expected an IOException for malformed input");
		} catch (IOException expected) {
			// expected: a clear exception, not a NullPointerException further downstream
		}
	}
}
