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

import com.fasterxml.jackson.databind.JsonNode;

/**
 * One tool offered by an MCP server, as reported by "tools/list".
 * {@link #inputSchema} is the JSON Schema for the tool's arguments, kept verbatim
 * since it is directly reusable (only re-wrapped) for both OpenAI and Anthropic tool calling.
 */
public class McpToolDescriptor {

    public final String name;
    public final String description;
    public final JsonNode inputSchema;

    public McpToolDescriptor(String name, String description, JsonNode inputSchema) {
        this.name = name;
        this.description = description != null ? description : "";
        this.inputSchema = inputSchema;
    }
}
