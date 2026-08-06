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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicLong;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

import net.sf.jailer.JailerVersion;

/**
 * MCP client speaking JSON-RPC 2.0 over a subprocess's stdin/stdout (the MCP "stdio" transport).
 * Each message is a single line of compact (no embedded newline) UTF-8 JSON; the subprocess's
 * stderr is diagnostic-only and is never parsed as protocol traffic.
 *
 * <p>All blocking methods ({@link #initialize}, {@link #listTools}, {@link #callTool}) must not
 * be called on the EDT.
 */
public class McpStdioClient implements AutoCloseable {

    private static final Logger _log = LoggerFactory.getLogger("ai_mcp");
    private static final ObjectMapper MAPPER = new ObjectMapper();

    /** MCP protocol version this client speaks; the newline-delimited stdio framing is stable across versions. */
    private static final String PROTOCOL_VERSION = "2025-06-18";
    private static final int STDERR_TAIL_MAX = 50;

    public enum State { NOT_STARTED, STARTING, READY, CLOSED, FAILED }

    private final McpServerConfig config;
    private volatile Process process;
    private volatile OutputStream stdin;
    private Thread stdoutReaderThread;
    private Thread stderrReaderThread;
    private final AtomicLong idGenerator = new AtomicLong(1);
    private final ConcurrentHashMap<Long, CompletableFuture<JsonNode>> pending = new ConcurrentHashMap<>();
    private final Object writeLock = new Object();
    private volatile State state = State.NOT_STARTED;
    private volatile McpServerInfo serverInfo;
    private final List<String> stderrTail = new CopyOnWriteArrayList<>();

    public McpStdioClient(McpServerConfig config) {
        this.config = config;
    }

    public McpServerConfig getConfig() {
        return config;
    }

    public State getState() {
        return state;
    }

    public boolean isAlive() {
        Process p = process;
        return p != null && p.isAlive() && state != State.CLOSED && state != State.FAILED;
    }

    public McpServerInfo getServerInfo() {
        return serverInfo;
    }

    /** Returns the last few lines the server wrote to stderr, for diagnostics after a failure. */
    public List<String> getStderrTail() {
        return new ArrayList<>(stderrTail);
    }

    /**
     * Spawns the server subprocess and starts the background reader threads.
     * Does not perform the MCP handshake; call {@link #initialize} afterwards.
     */
    public synchronized void start() throws IOException {
        if (state != State.NOT_STARTED) {
            throw new IllegalStateException("start() already called (state=" + state + ")");
        }
        state = State.STARTING;
        List<String> cmd = new ArrayList<>();
        cmd.add(config.command);
        cmd.addAll(config.args);
        ProcessBuilder pb = new ProcessBuilder(cmd);
        pb.redirectErrorStream(false);
        pb.environment().putAll(config.env);
        try {
            process = pb.start();
        } catch (IOException e) {
            state = State.FAILED;
            throw new IOException("Could not start MCP server '" + config.name + "' (" + config.command + "): " + e.getMessage(), e);
        }
        stdin = process.getOutputStream();

        stdoutReaderThread = new Thread(this::readLoop, "mcp-stdout-" + config.id);
        stdoutReaderThread.setDaemon(true);
        stdoutReaderThread.start();

        stderrReaderThread = new Thread(this::stderrLoop, "mcp-stderr-" + config.id);
        stderrReaderThread.setDaemon(true);
        stderrReaderThread.start();

        state = State.READY;
    }

    /**
     * Performs the MCP "initialize" handshake followed by the "notifications/initialized" notification.
     */
    public McpServerInfo initialize(long timeoutMillis) throws IOException, TimeoutException {
        ObjectNode params = MAPPER.createObjectNode();
        params.put("protocolVersion", PROTOCOL_VERSION);
        params.putObject("capabilities");
        ObjectNode clientInfo = params.putObject("clientInfo");
        clientInfo.put("name", "Jailer");
        clientInfo.put("version", JailerVersion.VERSION);

        JsonNode result = request("initialize", params, timeoutMillis);
        sendNotification("notifications/initialized", null);

        JsonNode serverInfoNode = result.path("serverInfo");
        serverInfo = new McpServerInfo(
                serverInfoNode.path("name").asText(""),
                serverInfoNode.path("version").asText(""),
                result.path("protocolVersion").asText(""),
                result.hasNonNull("instructions") ? result.path("instructions").asText() : null);
        return serverInfo;
    }

    /** Lists all tools the server offers, following pagination via "nextCursor" if present. */
    public List<McpToolDescriptor> listTools(long timeoutMillis) throws IOException, TimeoutException {
        List<McpToolDescriptor> result = new ArrayList<>();
        String cursor = null;
        do {
            ObjectNode params = MAPPER.createObjectNode();
            if (cursor != null) {
                params.put("cursor", cursor);
            }
            JsonNode response = request("tools/list", params, timeoutMillis);
            for (JsonNode toolNode : response.path("tools")) {
                result.add(new McpToolDescriptor(
                        toolNode.path("name").asText(""),
                        toolNode.path("description").asText(""),
                        toolNode.has("inputSchema") ? toolNode.get("inputSchema") : MAPPER.createObjectNode()));
            }
            JsonNode nextCursor = response.path("nextCursor");
            cursor = (nextCursor.isTextual() && !nextCursor.asText().isEmpty()) ? nextCursor.asText() : null;
        } while (cursor != null);
        return result;
    }

    /**
     * Invokes one tool by its bare (server-local, unprefixed) name.
     * Non-text content blocks in the result are replaced with a visible placeholder rather than silently dropped.
     */
    public McpToolResult callTool(String name, ObjectNode arguments, long timeoutMillis) throws IOException, TimeoutException {
        ObjectNode params = MAPPER.createObjectNode();
        params.put("name", name);
        params.set("arguments", arguments != null ? arguments : MAPPER.createObjectNode());
        JsonNode response = request("tools/call", params, timeoutMillis);

        StringBuilder text = new StringBuilder();
        for (JsonNode block : response.path("content")) {
            if (text.length() > 0) {
                text.append("\n");
            }
            String type = block.path("type").asText("");
            if ("text".equals(type)) {
                text.append(block.path("text").asText(""));
            } else {
                text.append("(unsupported content type '").append(type).append("' omitted)");
            }
        }
        boolean isError = response.path("isError").asBoolean(false);
        return new McpToolResult(text.toString(), isError);
    }

    private JsonNode request(String method, ObjectNode params, long timeoutMillis) throws IOException, TimeoutException {
        if (state == State.CLOSED || state == State.FAILED) {
            throw new IOException("MCP server '" + config.name + "' is not running (state=" + state + ")");
        }
        long id = idGenerator.getAndIncrement();
        CompletableFuture<JsonNode> future = new CompletableFuture<>();
        pending.put(id, future);
        ObjectNode msg = MAPPER.createObjectNode();
        msg.put("jsonrpc", "2.0");
        msg.put("id", id);
        msg.put("method", method);
        if (params != null) {
            msg.set("params", params);
        }
        try {
            writeMessage(msg);
        } catch (IOException e) {
            pending.remove(id);
            throw e;
        }
        try {
            return future.get(timeoutMillis, TimeUnit.MILLISECONDS);
        } catch (ExecutionException e) {
            Throwable cause = e.getCause();
            if (cause instanceof IOException) {
                throw (IOException) cause;
            }
            throw new IOException("MCP request '" + method + "' to '" + config.name + "' failed", cause);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new IOException("MCP request '" + method + "' interrupted");
        } finally {
            pending.remove(id);
        }
    }

    private void sendNotification(String method, ObjectNode params) throws IOException {
        ObjectNode msg = MAPPER.createObjectNode();
        msg.put("jsonrpc", "2.0");
        msg.put("method", method);
        if (params != null) {
            msg.set("params", params);
        }
        writeMessage(msg);
    }

    private void writeMessage(ObjectNode node) throws IOException {
        String line = encodeMessage(node, MAPPER);
        synchronized (writeLock) {
            OutputStream out = stdin;
            if (out == null) {
                throw new IOException("MCP server '" + config.name + "' is not started");
            }
            out.write(line.getBytes(StandardCharsets.UTF_8));
            out.flush();
        }
    }

    private void readLoop() {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream(), StandardCharsets.UTF_8))) {
            String line;
            while ((line = reader.readLine()) != null) {
                if (line.trim().isEmpty()) {
                    continue;
                }
                JsonNode node;
                try {
                    node = decodeLine(line, MAPPER);
                } catch (IOException e) {
                    _log.warn("MCP server '{}': could not parse line as JSON, ignoring: {}", config.name, line);
                    continue;
                }
                handleIncoming(node);
            }
        } catch (IOException e) {
            _log.debug("MCP server '{}': stdout reader stopped: {}", config.name, e.getMessage());
        } finally {
            onProcessGone();
        }
    }

    private void handleIncoming(JsonNode node) {
        boolean hasId = node.hasNonNull("id");
        boolean hasMethod = node.has("method");
        if (hasId && !hasMethod) {
            // A response to one of our own requests.
            long id;
            try {
                id = node.get("id").asLong();
            } catch (Exception e) {
                return;
            }
            CompletableFuture<JsonNode> future = pending.remove(id);
            if (future == null) {
                return;
            }
            JsonNode error = node.get("error");
            if (error != null && !error.isNull()) {
                future.completeExceptionally(new McpProtocolException(
                        error.path("code").asInt(-32000),
                        error.path("message").asText("MCP error"),
                        error.has("data") ? error.get("data").toString() : null));
            } else {
                future.complete(node.get("result"));
            }
        } else if (hasMethod && !hasId) {
            // A notification from the server; v1 only logs it.
            _log.debug("MCP server '{}': notification {}", config.name, node.path("method").asText());
        } else if (hasMethod && hasId) {
            // A server-to-client request (e.g. sampling/createMessage, roots/list); not supported in v1.
            respondMethodNotFound(node.get("id"));
        }
    }

    private void respondMethodNotFound(JsonNode idNode) {
        ObjectNode resp = MAPPER.createObjectNode();
        resp.put("jsonrpc", "2.0");
        resp.set("id", idNode.deepCopy());
        ObjectNode error = resp.putObject("error");
        error.put("code", -32601);
        error.put("message", "Method not found");
        try {
            writeMessage(resp);
        } catch (IOException e) {
            _log.debug("MCP server '{}': failed to send Method-not-found reply: {}", config.name, e.getMessage());
        }
    }

    private void onProcessGone() {
        state = State.CLOSED;
        Process p = process;
        String exitInfo = "";
        if (p != null) {
            try {
                if (p.waitFor(200, TimeUnit.MILLISECONDS)) {
                    exitInfo = " (exit code " + p.exitValue() + ")";
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
        IOException cause = new IOException("MCP server '" + config.name + "' process exited unexpectedly" + exitInfo + stderrDiagnostics());
        for (Long id : new ArrayList<>(pending.keySet())) {
            CompletableFuture<JsonNode> future = pending.remove(id);
            if (future != null) {
                future.completeExceptionally(cause);
            }
        }
    }

    private String stderrDiagnostics() {
        if (stderrTail.isEmpty()) {
            return "";
        }
        return ". Last output:\n" + String.join("\n", stderrTail);
    }

    private void stderrLoop() {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(process.getErrorStream(), StandardCharsets.UTF_8))) {
            String line;
            while ((line = reader.readLine()) != null) {
                _log.debug("MCP server '{}' (stderr): {}", config.name, line);
                stderrTail.add(line);
                while (stderrTail.size() > STDERR_TAIL_MAX) {
                    stderrTail.remove(0);
                }
            }
        } catch (IOException e) {
            // Process gone; readLoop's onProcessGone() handles the state transition.
        }
    }

    @Override
    public synchronized void close() {
        if (state == State.CLOSED) {
            return;
        }
        boolean wasNotStarted = state == State.NOT_STARTED;
        state = State.CLOSED;
        if (wasNotStarted) {
            return;
        }
        OutputStream out = stdin;
        if (out != null) {
            try {
                out.close();
            } catch (IOException ignored) {
            }
        }
        joinQuietly(stdoutReaderThread, 2000);
        Process p = process;
        if (p != null && p.isAlive()) {
            try {
                if (!p.waitFor(2, TimeUnit.SECONDS)) {
                    p.destroyForcibly();
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                p.destroyForcibly();
            }
        }
        joinQuietly(stderrReaderThread, 500);
        IOException cause = new IOException("MCP client for '" + config.name + "' was closed");
        for (Long id : new ArrayList<>(pending.keySet())) {
            CompletableFuture<JsonNode> future = pending.remove(id);
            if (future != null) {
                future.completeExceptionally(cause);
            }
        }
    }

    private static void joinQuietly(Thread t, long millis) {
        if (t == null) {
            return;
        }
        try {
            t.join(millis);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

    /**
     * Encodes one JSON-RPC message as a single newline-terminated line of compact JSON,
     * refusing to produce a line with an embedded newline (which would break the framing).
     */
    static String encodeMessage(JsonNode node, ObjectMapper mapper) throws IOException {
        String json = mapper.writeValueAsString(node);
        if (json.indexOf('\n') >= 0 || json.indexOf('\r') >= 0) {
            throw new IOException("Refusing to send MCP message containing an embedded newline");
        }
        return json + "\n";
    }

    static JsonNode decodeLine(String line, ObjectMapper mapper) throws IOException {
        return mapper.readTree(line);
    }
}
