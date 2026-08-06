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

/**
 * Result of an MCP "tools/call" invocation: the concatenated text content
 * (non-text content blocks are replaced with a visible placeholder, see {@code McpStdioClient#callTool})
 * and whether the server flagged the call as an error.
 */
public class McpToolResult {

    public final String text;
    public final boolean isError;

    public McpToolResult(String text, boolean isError) {
        this.text = text != null ? text : "";
        this.isError = isError;
    }
}
