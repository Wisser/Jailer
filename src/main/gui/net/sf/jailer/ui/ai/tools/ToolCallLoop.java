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
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import net.sf.jailer.ui.ai.AIProviderConfig;
import net.sf.jailer.ui.ai.AIQueryAssistant;

/**
 * Drives one AI request through zero or more rounds of tool-calling: sends the request, checks
 * whether the model asked to call a tool, dispatches it via the {@link ToolRegistry}, feeds the
 * result back, and repeats until the model returns a final text answer or {@link #maxIterations}
 * is reached. Must not be called on the EDT (it performs blocking network/tool I/O).
 */
public class ToolCallLoop {

    public interface Listener {
        void onToolCallStart(String toolName, ObjectNode arguments);
        void onToolCallResult(String toolName, ToolResult result);
    }

    private static final int DEFAULT_MAX_ITERATIONS = 8;
    private static final long DEFAULT_TOOL_TIMEOUT_MILLIS = 60000;

    private final AIProviderConfig config;
    private final boolean isAnthropic;
    private final boolean isResponsesApi;
    private final ToolRegistry registry;
    private final AtomicReference<Runnable> abortRef;
    private final Listener listener;
    private final int maxIterations;
    private final long toolTimeoutMillis;

    public ToolCallLoop(AIProviderConfig config, boolean isAnthropic, boolean isResponsesApi,
            ToolRegistry registry, AtomicReference<Runnable> abortRef, Listener listener) {
        this(config, isAnthropic, isResponsesApi, registry, abortRef, listener,
                DEFAULT_MAX_ITERATIONS, DEFAULT_TOOL_TIMEOUT_MILLIS);
    }

    public ToolCallLoop(AIProviderConfig config, boolean isAnthropic, boolean isResponsesApi,
            ToolRegistry registry, AtomicReference<Runnable> abortRef, Listener listener,
            int maxIterations, long toolTimeoutMillis) {
        this.config = config;
        this.isAnthropic = isAnthropic;
        this.isResponsesApi = isResponsesApi;
        this.registry = registry;
        this.abortRef = abortRef;
        this.listener = listener;
        this.maxIterations = maxIterations;
        this.toolTimeoutMillis = toolTimeoutMillis;
    }

    /**
     * Runs {@code body} (already containing "messages"/"input", with the "tools" array already
     * added) through the provider, dispatching any tool calls and re-sending until a final text
     * answer is produced.
     */
    public String run(ObjectNode body) throws IOException {
        for (int iteration = 0; iteration < maxIterations; iteration++) {
            if (Thread.currentThread().isInterrupted()) {
                throw new IOException("Request cancelled");
            }
            JsonNode response = AIQueryAssistant.post(config, body, abortRef);
            List<PendingToolCall> calls = AIQueryAssistant.extractToolCalls(response, isAnthropic, isResponsesApi);
            if (calls.isEmpty()) {
                return AIQueryAssistant.extractText(response, isAnthropic);
            }
            appendAssistantTurn(body, response);
            List<ToolResult> results = new ArrayList<>(calls.size());
            for (PendingToolCall call : calls) {
                if (Thread.currentThread().isInterrupted()) {
                    throw new IOException("Request cancelled");
                }
                if (listener != null) {
                    listener.onToolCallStart(call.name, call.arguments);
                }
                ToolResult result = dispatch(call);
                if (listener != null) {
                    listener.onToolCallResult(call.name, result);
                }
                results.add(result);
            }
            appendToolResults(body, calls, results);
        }
        throw new IOException("Tool-calling loop exceeded " + maxIterations + " iterations without a final answer");
    }

    private ToolResult dispatch(PendingToolCall call) {
        ToolDescriptor descriptor = registry.resolve(call.name);
        if (descriptor == null) {
            return new ToolResult("Error: unknown tool '" + call.name + "'", true);
        }
        try {
            return descriptor.invoker.invoke(descriptor.invokeName, call.arguments, toolTimeoutMillis);
        } catch (IOException e) {
            return new ToolResult("Error calling tool '" + call.name + "': " + e.getMessage(), true);
        }
    }

    private ArrayNode messagesArray(ObjectNode body) {
        String key = isResponsesApi ? "input" : "messages";
        JsonNode existing = body.get(key);
        if (existing instanceof ArrayNode) {
            return (ArrayNode) existing;
        }
        return body.putArray(key);
    }

    private void appendAssistantTurn(ObjectNode body, JsonNode response) {
        ArrayNode messages = messagesArray(body);
        if (isAnthropic) {
            ObjectNode assistantMsg = messages.addObject();
            assistantMsg.put("role", "assistant");
            assistantMsg.set("content", response.path("content").deepCopy());
        } else if (isResponsesApi) {
            for (JsonNode item : response.path("output")) {
                messages.add(item.deepCopy());
            }
        } else {
            JsonNode messageNode = response.path("choices").path(0).path("message");
            if (messageNode.isObject()) {
                messages.add(messageNode.deepCopy());
            } else {
                ObjectNode fallback = messages.addObject();
                fallback.put("role", "assistant");
                fallback.put("content", "");
            }
        }
    }

    private void appendToolResults(ObjectNode body, List<PendingToolCall> calls, List<ToolResult> results) {
        ArrayNode messages = messagesArray(body);
        if (isAnthropic) {
            ObjectNode toolResultMsg = messages.addObject();
            toolResultMsg.put("role", "user");
            ArrayNode content = toolResultMsg.putArray("content");
            for (int i = 0; i < calls.size(); i++) {
                PendingToolCall call = calls.get(i);
                ToolResult result = results.get(i);
                ObjectNode block = content.addObject();
                block.put("type", "tool_result");
                block.put("tool_use_id", call.id);
                block.put("content", result.text);
                if (result.isError) {
                    block.put("is_error", true);
                }
            }
        } else if (isResponsesApi) {
            for (int i = 0; i < calls.size(); i++) {
                PendingToolCall call = calls.get(i);
                ToolResult result = results.get(i);
                ObjectNode item = messages.addObject();
                item.put("type", "function_call_output");
                item.put("call_id", call.id);
                item.put("output", result.text);
            }
        } else {
            for (int i = 0; i < calls.size(); i++) {
                PendingToolCall call = calls.get(i);
                ToolResult result = results.get(i);
                ObjectNode toolMsg = messages.addObject();
                toolMsg.put("role", "tool");
                toolMsg.put("tool_call_id", call.id);
                toolMsg.put("content", result.text);
            }
        }
    }
}
