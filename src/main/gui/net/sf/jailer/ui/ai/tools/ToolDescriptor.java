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

import com.fasterxml.jackson.databind.JsonNode;

/**
 * Provider-agnostic tool descriptor. {@link #name} is what is sent to (and comes back from) the
 * LLM provider — possibly prefixed with a server id to avoid collisions when several MCP servers
 * are active (see {@code ToolRegistry}) — while {@link #invokeName} is the bare name to pass to
 * {@link #invoker} when actually dispatching the call.
 */
public class ToolDescriptor {

    public final String name;
    public final String invokeName;
    public final String description;
    public final JsonNode inputSchema;
    public final ToolInvoker invoker;

    public ToolDescriptor(String name, String invokeName, String description, JsonNode inputSchema, ToolInvoker invoker) {
        this.name = name;
        this.invokeName = invokeName;
        this.description = description != null ? description : "";
        this.inputSchema = inputSchema;
        this.invoker = invoker;
    }
}
