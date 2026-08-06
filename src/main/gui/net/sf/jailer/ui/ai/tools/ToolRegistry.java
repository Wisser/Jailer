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
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeoutException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.sf.jailer.ui.ai.mcp.McpServerConfig;
import net.sf.jailer.ui.ai.mcp.McpServerRegistry;
import net.sf.jailer.ui.ai.mcp.McpStdioClient;
import net.sf.jailer.ui.ai.mcp.McpToolDescriptor;

/**
 * Aggregates the tools currently offered by all enabled MCP servers into one provider-agnostic
 * list, prefixing tool names with the server id ("serverId__toolName") only when more than one
 * server is active, to avoid name collisions. Must be built off the EDT (starts subprocesses).
 */
public class ToolRegistry {

    private static final Logger _log = LoggerFactory.getLogger("ai_mcp");
    private static final long LIST_TOOLS_TIMEOUT_MILLIS = 15000;
    private static final String NAME_SEPARATOR = "__";

    private final Map<String, ToolDescriptor> byName = new LinkedHashMap<>();

    private ToolRegistry() {
    }

    /**
     * Starts (if needed) every enabled server and lists its tools. A server that fails to start
     * or list its tools is skipped with a warning rather than failing the whole registry.
     */
    public static ToolRegistry buildActive(List<McpServerConfig> enabledServers) {
        ToolRegistry registry = new ToolRegistry();
        boolean multipleServers = enabledServers.size() > 1;
        for (McpServerConfig serverConfig : enabledServers) {
            try {
                McpStdioClient client = McpServerRegistry.getInstance().getOrStart(serverConfig);
                McpToolInvoker invoker = new McpToolInvoker(client);
                for (McpToolDescriptor toolDescriptor : client.listTools(LIST_TOOLS_TIMEOUT_MILLIS)) {
                    String providerFacingName = multipleServers
                            ? serverConfig.id + NAME_SEPARATOR + toolDescriptor.name
                            : toolDescriptor.name;
                    registry.byName.put(providerFacingName, new ToolDescriptor(
                            providerFacingName, toolDescriptor.name, toolDescriptor.description,
                            toolDescriptor.inputSchema, invoker));
                }
            } catch (IOException | TimeoutException e) {
                _log.warn("MCP server '{}' unavailable, its tools are omitted: {}", serverConfig.name, e.getMessage());
            }
        }
        return registry;
    }

    public boolean isEmpty() {
        return byName.isEmpty();
    }

    public List<ToolDescriptor> getToolDescriptors() {
        return new ArrayList<>(byName.values());
    }

    public ToolDescriptor resolve(String toolName) {
        return byName.get(toolName);
    }
}
