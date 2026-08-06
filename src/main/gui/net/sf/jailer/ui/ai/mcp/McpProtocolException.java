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

/**
 * Wraps a JSON-RPC 2.0 "error" object returned by an MCP server in response to a request.
 */
public class McpProtocolException extends IOException {

    private static final long serialVersionUID = 1L;

    public final int code;
    public final String data;

    public McpProtocolException(int code, String message, String data) {
        super(message);
        this.code = code;
        this.data = data;
    }
}
