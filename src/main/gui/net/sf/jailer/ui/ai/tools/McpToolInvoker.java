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
package net.sf.jailer.ui.ai.tools;

import java.io.IOException;
import java.util.concurrent.TimeoutException;

import com.fasterxml.jackson.databind.node.ObjectNode;

import net.sf.jailer.ui.ai.mcp.McpStdioClient;
import net.sf.jailer.ui.ai.mcp.McpToolResult;

/** Wraps one {@link McpStdioClient}, dispatching tool calls to it by their bare (unprefixed) name. */
public class McpToolInvoker implements ToolInvoker {

    private final McpStdioClient client;

    public McpToolInvoker(McpStdioClient client) {
        this.client = client;
    }

    @Override
    public ToolResult invoke(String toolName, ObjectNode arguments, long timeoutMillis) throws IOException {
        try {
            McpToolResult result = client.callTool(toolName, arguments, timeoutMillis);
            return new ToolResult(result.text, result.isError);
        } catch (TimeoutException e) {
            throw new IOException("MCP tool '" + toolName + "' timed out after " + timeoutMillis + " ms", e);
        }
    }
}
