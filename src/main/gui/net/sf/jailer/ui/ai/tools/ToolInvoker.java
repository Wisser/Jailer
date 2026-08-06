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

import com.fasterxml.jackson.databind.node.ObjectNode;

/**
 * Dispatches a tool call to whatever backs it (an MCP server today; potentially an in-process
 * local tool in the future). Implementations must not be called on the EDT.
 */
public interface ToolInvoker {

    ToolResult invoke(String toolName, ObjectNode arguments, long timeoutMillis) throws IOException;
}
