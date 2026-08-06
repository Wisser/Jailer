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

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Immutable configuration for one MCP server reachable via the stdio transport
 * (a command, its arguments, and environment variables to launch it with).
 */
public class McpServerConfig {

    public final String id;
    public final String name;
    public final String command;
    public final List<String> args;
    public final Map<String, String> env;
    public final boolean enabled;

    public McpServerConfig(String id, String name, String command, List<String> args,
            Map<String, String> env, boolean enabled) {
        this.id = id;
        this.name = name;
        this.command = command;
        this.args = args != null ? Collections.unmodifiableList(new ArrayList<>(args)) : Collections.<String>emptyList();
        this.env = env != null ? Collections.unmodifiableMap(new LinkedHashMap<>(env)) : Collections.<String, String>emptyMap();
        this.enabled = enabled;
    }
}
