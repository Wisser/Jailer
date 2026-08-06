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

import com.fasterxml.jackson.databind.node.ObjectNode;

/**
 * One tool call requested by the model in a single provider response
 * (Anthropic "tool_use" block, OpenAI Chat "tool_calls" entry, or OpenAI Responses "function_call" item).
 */
public class PendingToolCall {

    public final String id;
    public final String name;
    public final ObjectNode arguments;

    public PendingToolCall(String id, String name, ObjectNode arguments) {
        this.id = id;
        this.name = name;
        this.arguments = arguments;
    }
}
