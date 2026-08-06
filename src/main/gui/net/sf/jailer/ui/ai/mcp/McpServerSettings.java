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
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import net.sf.jailer.ui.util.StringObfuscator;
import net.sf.jailer.ui.util.UISettings;

/**
 * Loads and persists the list of configured MCP servers as a single JSON string in {@link UISettings},
 * matching the convention used elsewhere for structured-but-simple AI settings (see {@code AIProviderPanel}).
 * Environment variable values are obfuscated individually since they may hold secrets (e.g. API tokens).
 */
public class McpServerSettings {

    private static final String SETTING_KEY = "AiMcpServers";
    private static final ObjectMapper MAPPER = new ObjectMapper();
    private static final StringObfuscator STRING_OBFUSCATOR = new StringObfuscator();

    private McpServerSettings() {
    }

    public static List<McpServerConfig> load() {
        Object stored = UISettings.restore(SETTING_KEY);
        List<McpServerConfig> result = new ArrayList<>();
        if (!(stored instanceof String) || ((String) stored).isEmpty()) {
            return result;
        }
        try {
            JsonNode arr = MAPPER.readTree((String) stored);
            for (JsonNode node : arr) {
                Map<String, String> env = new LinkedHashMap<>();
                Iterator<Map.Entry<String, JsonNode>> fields = node.path("env").fields();
                while (fields.hasNext()) {
                    Map.Entry<String, JsonNode> e = fields.next();
                    env.put(e.getKey(), STRING_OBFUSCATOR.decrypt(e.getValue().asText("")));
                }
                List<String> args = new ArrayList<>();
                for (JsonNode a : node.path("args")) {
                    args.add(a.asText(""));
                }
                result.add(new McpServerConfig(
                        node.path("id").asText(""),
                        node.path("name").asText(""),
                        node.path("command").asText(""),
                        args, env,
                        node.path("enabled").asBoolean(false)));
            }
        } catch (IOException e) {
            // Corrupted settings; start fresh rather than failing.
        }
        return result;
    }

    public static void save(List<McpServerConfig> configs) {
        ArrayNode arr = MAPPER.createArrayNode();
        for (McpServerConfig config : configs) {
            ObjectNode node = arr.addObject();
            node.put("id", config.id);
            node.put("name", config.name);
            node.put("command", config.command);
            ArrayNode argsNode = node.putArray("args");
            for (String a : config.args) {
                argsNode.add(a);
            }
            ObjectNode envNode = node.putObject("env");
            for (Map.Entry<String, String> e : config.env.entrySet()) {
                envNode.put(e.getKey(), STRING_OBFUSCATOR.encrypt(e.getValue()));
            }
            node.put("enabled", config.enabled);
        }
        UISettings.store(SETTING_KEY, arr.toString());
    }

    /** Convenience: the subset of {@link #load()} that is currently enabled. */
    public static List<McpServerConfig> loadEnabled() {
        List<McpServerConfig> enabled = new ArrayList<>();
        for (McpServerConfig config : load()) {
            if (config.enabled) {
                enabled.add(config);
            }
        }
        return enabled;
    }
}
