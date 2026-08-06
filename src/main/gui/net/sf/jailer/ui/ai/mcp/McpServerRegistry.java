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
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.TimeoutException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Owns one {@link McpStdioClient} per enabled, configured MCP server. Clients are started lazily
 * on first use and kept alive for the lifetime of the JVM (no restart per dialog open); a shutdown
 * hook makes sure no MCP subprocess survives Jailer exiting.
 */
public class McpServerRegistry {

    private static final Logger _log = LoggerFactory.getLogger("ai_mcp");
    private static final McpServerRegistry INSTANCE = new McpServerRegistry();
    private static final long HANDSHAKE_TIMEOUT_MILLIS = 15000;

    private final Map<String, McpStdioClient> clients = new LinkedHashMap<>();
    private boolean shutdownHookRegistered;

    private McpServerRegistry() {
    }

    public static McpServerRegistry getInstance() {
        return INSTANCE;
    }

    /**
     * Returns the running, initialized client for the given enabled server config, starting it
     * on first call. Must not be called on the EDT.
     */
    public synchronized McpStdioClient getOrStart(McpServerConfig config) throws IOException, TimeoutException {
        McpStdioClient existing = clients.get(config.id);
        if (existing != null) {
            if (existing.isAlive()) {
                return existing;
            }
            clients.remove(config.id);
        }
        McpStdioClient client = new McpStdioClient(config);
        client.start();
        client.initialize(HANDSHAKE_TIMEOUT_MILLIS);
        clients.put(config.id, client);
        ensureShutdownHook();
        return client;
    }

    /** Starts and initializes a transient client for connection testing; the caller must {@code close()} it. */
    public static McpStdioClient startTransient(McpServerConfig config, long timeoutMillis) throws IOException, TimeoutException {
        McpStdioClient client = new McpStdioClient(config);
        client.start();
        client.initialize(timeoutMillis);
        return client;
    }

    public synchronized void stopAll() {
        for (McpStdioClient client : clients.values()) {
            try {
                client.close();
            } catch (Exception e) {
                _log.debug("Error closing MCP client '{}': {}", client.getConfig().name, e.getMessage());
            }
        }
        clients.clear();
    }

    public synchronized void stop(String serverId) {
        McpStdioClient client = clients.remove(serverId);
        if (client != null) {
            client.close();
        }
    }

    private void ensureShutdownHook() {
        if (shutdownHookRegistered) {
            return;
        }
        Runtime.getRuntime().addShutdownHook(new Thread(this::stopAll, "mcp-shutdown"));
        shutdownHookRegistered = true;
    }
}
