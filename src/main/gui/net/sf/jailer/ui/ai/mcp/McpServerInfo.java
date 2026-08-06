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
 * Server-reported information from the MCP "initialize" handshake.
 */
public class McpServerInfo {

    public final String name;
    public final String version;
    public final String protocolVersion;
    public final String instructions;

    public McpServerInfo(String name, String version, String protocolVersion, String instructions) {
        this.name = name;
        this.version = version;
        this.protocolVersion = protocolVersion;
        this.instructions = instructions;
    }
}
