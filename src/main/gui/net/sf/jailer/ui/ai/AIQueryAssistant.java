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
package net.sf.jailer.ui.ai;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BooleanSupplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.ai.AIProviderConfig.ProviderType;
import net.sf.jailer.ui.syntaxtextarea.BasicFormatterImpl;
import net.sf.jailer.ui.util.UISettings;
import net.sf.jailer.util.SqlUtil;

/**
 * Sends a natural-language question together with the current data-model schema
 * to an AI API and returns the generated SQL.
 * Supports Anthropic and OpenAI-compatible endpoints (OpenAI, Azure, Groq, Ollama, OpenRouter, ...).
 * Uses HttpURLConnection with a curl subprocess fallback to handle proxies that strip
 * the Authorization header.
 */
public class AIQueryAssistant {

    private static final Logger _log = LoggerFactory.getLogger("ai_api");
    private static final ObjectMapper MAPPER = new ObjectMapper();
    private static final Pattern AI_COMMENT_PATTERN = Pattern.compile(
            "\\A\\s*/\\*\\s*AI:\\s*(.*?)\\s*\\*/",
            Pattern.DOTALL | Pattern.CASE_INSENSITIVE);

    public static String generateSQL(String question, List<ConversationMessage> history,
            DataModel dataModel, String dbmsName, AIProviderConfig config) throws IOException {
        return generateSQL(question, history, dataModel, dbmsName, config, null);
    }

    public static String generateSQL(String question, List<ConversationMessage> history,
            DataModel dataModel, String dbmsName, AIProviderConfig config,
            String systemPromptTemplate) throws IOException {
        return generateSQL(question, history, dataModel, dbmsName, config, systemPromptTemplate, false, false);
    }

    public static String generateSQL(String question, List<ConversationMessage> history,
            DataModel dataModel, String dbmsName, AIProviderConfig config,
            String systemPromptTemplate, boolean smartSelection) throws IOException {
        return generateSQL(question, history, dataModel, dbmsName, config, systemPromptTemplate, smartSelection, false);
    }

    public static String generateSQL(String question, List<ConversationMessage> history,
            DataModel dataModel, String dbmsName, AIProviderConfig config,
            String systemPromptTemplate, boolean smartSelection, boolean omitColumnTypes) throws IOException {
        return generateSQL(question, history, dataModel, dbmsName, config, systemPromptTemplate, smartSelection, omitColumnTypes, null);
    }

    public static String generateSQL(String question, List<ConversationMessage> history,
            DataModel dataModel, String dbmsName, AIProviderConfig config,
            String systemPromptTemplate, boolean smartSelection, boolean omitColumnTypes,
            AtomicReference<Runnable> abortRef) throws IOException {
        return generateSQL(question, history, dataModel, dbmsName, config, systemPromptTemplate,
                smartSelection, omitColumnTypes, abortRef, null, null);
    }

    public static String generateSQL(String question, List<ConversationMessage> history,
            DataModel dataModel, String dbmsName, AIProviderConfig config,
            ExecutionContext executionContext, AtomicReference<Runnable> abortRef) throws IOException {
        return generateSQL(question, history, dataModel, dbmsName, config, executionContext, abortRef, null);
    }

    public static String generateSQL(String question, List<ConversationMessage> history,
            DataModel dataModel, String dbmsName, AIProviderConfig config,
            ExecutionContext executionContext, AtomicReference<Runnable> abortRef,
            BooleanSupplier confirmFullSchema) throws IOException {
        validateConfig(config);
        boolean omitColumnTypes = loadOmitColumnTypes(config, executionContext);
        boolean smartSelection  = loadSmartSelection(config, executionContext, dataModel.getSortedTables().size());
        return generateSQL(question, history, dataModel, dbmsName, config,
                loadSystemPromptTemplate(), smartSelection, omitColumnTypes, abortRef, confirmFullSchema,
                loadFirstPassSystemPromptTemplate());
    }

    public static void validateConfig(AIProviderConfig config) throws IOException {
        if (config.apiKey.isEmpty() && config.providerType.requiresApiKey) {
            throw new IOException("No API key configured. Please enter an API key in the AI Assistant settings.");
        }
    }

    public static String generateSQL(String question, List<ConversationMessage> history,
            DataModel dataModel, String dbmsName, AIProviderConfig config,
            String systemPromptTemplate, boolean smartSelection, boolean omitColumnTypes,
            AtomicReference<Runnable> abortRef, BooleanSupplier confirmFullSchema) throws IOException {
        return generateSQL(question, history, dataModel, dbmsName, config, systemPromptTemplate,
                smartSelection, omitColumnTypes, abortRef, confirmFullSchema, null);
    }

    public static String generateSQL(String question, List<ConversationMessage> history,
            DataModel dataModel, String dbmsName, AIProviderConfig config,
            String systemPromptTemplate, boolean smartSelection, boolean omitColumnTypes,
            AtomicReference<Runnable> abortRef, BooleanSupplier confirmFullSchema,
            String firstPassSystemPromptTemplate) throws IOException {
        return generateSQL(question, history, dataModel, dbmsName, config, systemPromptTemplate,
                smartSelection, omitColumnTypes, abortRef, confirmFullSchema,
                firstPassSystemPromptTemplate, null);
    }

    public static String generateSQL(String question, List<ConversationMessage> history,
            DataModel dataModel, String dbmsName, AIProviderConfig config,
            String systemPromptTemplate, boolean smartSelection, boolean omitColumnTypes,
            AtomicReference<Runnable> abortRef, BooleanSupplier confirmFullSchema,
            String firstPassSystemPromptTemplate, AtomicReference<String> rawResponseRef) throws IOException {
        Set<String> relevantTables = null;
        if (smartSelection) {
            try {
                relevantTables = selectRelevantTables(question, history, dataModel, dbmsName, config, abortRef,
                        firstPassSystemPromptTemplate);
            } catch (IOException e) {
                if (Thread.currentThread().isInterrupted()) {
                    return "";
                }
                throw e;
            }
            if (relevantTables == null && !Thread.currentThread().isInterrupted()) {
                if (confirmFullSchema != null && !confirmFullSchema.getAsBoolean()) {
                    return "";
                }
            }
        }
        String schema = buildSchemaDescription(dataModel, relevantTables, omitColumnTypes);
        boolean isAnthropic = config.providerType == ProviderType.ANTHROPIC;
        ObjectNode body = buildRequestBody(question, history, schema, dbmsName, config, isAnthropic, systemPromptTemplate);
        JsonNode response = post(config, body, abortRef);
        String rawText = extractText(response, isAnthropic).trim();
        if (rawResponseRef != null) rawResponseRef.set(rawText);
        String result = stripMarkdownCodeFence(rawText);
        if (result.endsWith(";")) {
			result = result.substring(0, result.length() - 1).trim();
		}
        result = new BasicFormatterImpl().format(result);
        UISettings.s21 = (omitColumnTypes ? 1L : 0L) | (smartSelection ? 2L : 0L);
        return result;
    }

    private static String extractText(JsonNode response, boolean isAnthropic) throws IOException {
        if (isAnthropic) {
            JsonNode contentNode = response.path("content");
            if (contentNode.isArray() && contentNode.size() > 0) {
                return contentNode.get(0).path("text").asText("").trim();
            }
            throw new IOException("Unexpected response format: missing 'content' array. Response: " + response.toString());
        }
        // OpenAI-compatible: choices[0].message.content
        JsonNode choicesNode = response.path("choices");
        if (choicesNode.isArray() && choicesNode.size() > 0) {
            String content = choicesNode.get(0).path("message").path("content").asText("");
            if (!content.isEmpty()) {
                return content.trim();
            }
        }
        // Ollama-compatible: message.content
        JsonNode messageNode = response.path("message");
        if (!messageNode.isMissingNode() && !messageNode.isNull()) {
            String content = messageNode.path("content").asText("");
            if (!content.isEmpty()) {
                return content.trim();
            }
        }
        throw new IOException("Unexpected response format: missing 'choices' or 'message'. Response: " + response.toString());
    }

    private static Set<String> selectRelevantTables(String question, List<ConversationMessage> history,
            DataModel dataModel, String dbmsName, AIProviderConfig config,
            AtomicReference<Runnable> abortRef, String firstPassSystemPromptTemplate) throws IOException {
        List<Table> allTables = new ArrayList<>(dataModel.getSortedTables());
        StringBuilder tableList = new StringBuilder();
        for (Table t : allTables) {
            tableList.append(t.getName()).append("\n");
        }

        String template = (firstPassSystemPromptTemplate != null && !firstPassSystemPromptTemplate.isEmpty())
                ? firstPassSystemPromptTemplate
                : loadFirstPassSystemPromptTemplate();
        if (template == null) {
            template = SystemPromptPanel.DEFAULT_FIRST_PASS_TEMPLATE;
        }
        String systemPrompt = template.replace("{dbmsName}", dbmsName);

        boolean isAnthropic = config.providerType == ProviderType.ANTHROPIC;
        ObjectNode body = MAPPER.createObjectNode();
        body.put("model", config.model);
        body.put("max_tokens", 512);
        body.put("stream", false);

        ArrayNode messages = body.putArray("messages");
        if (isAnthropic) {
            body.put("system", systemPrompt);
        } else {
            ObjectNode sysMsg = messages.addObject();
            sysMsg.put("role", "system");
            sysMsg.put("content", systemPrompt);
        }
        for (ConversationMessage msg : history) {
            ObjectNode m = messages.addObject();
            m.put("role", msg.role);
            m.put("content", msg.content);
        }
        ObjectNode userMsg = messages.addObject();
        userMsg.put("role", "user");
        userMsg.put("content", "Tables:\n" + tableList + "\nQuestion: " + question);

        JsonNode response = post(config, body, abortRef);
        String responseText = extractText(response, isAnthropic);

        Map<String, String> lowerToActual = new HashMap<>();
        for (Table t : allTables) {
            lowerToActual.put(t.getName().toLowerCase(Locale.ROOT), t.getName());
        }

        Set<String> selected = new LinkedHashSet<>();
        for (String line : responseText.split("\\r?\\n")) {
            String name = line.trim();
            if (name.isEmpty()) continue;
            String actual = lowerToActual.get(name.toLowerCase(Locale.ROOT));
            if (actual != null) {
                selected.add(actual);
            }
        }

        if (selected.isEmpty()) {
            _log.warn("Smart table selection returned no matching tables, falling back to full schema");
            return null;
        }
        return expandWithFkNeighbors(dataModel, selected);
    }

    private static Set<String> expandWithFkNeighbors(DataModel dataModel, Set<String> tableNames) {
        Set<String> expanded = new LinkedHashSet<>(tableNames);
        for (Table table : dataModel.getSortedTables()) {
            if (tableNames.contains(table.getName())) {
                for (Association assoc : table.associations) {
                    expanded.add(assoc.destination.getName());
                }
            }
        }
        return expanded;
    }

    public static String generateSQL(String question, DataModel dataModel, String dbmsName, AIProviderConfig config) throws IOException {
        return generateSQL(question, Collections.emptyList(), dataModel, dbmsName, config, null);
    }

    public static void testConnection(AIProviderConfig config, AtomicReference<Runnable> abortRef) throws IOException {
        boolean isAnthropic = config.providerType == ProviderType.ANTHROPIC;
        ObjectNode body = MAPPER.createObjectNode();
        body.put("model", config.model);
        body.put("max_tokens", 16);
        body.put("stream", false);
        ArrayNode messages = body.putArray("messages");
        if (isAnthropic) {
            body.put("system", "You are a helpful assistant.");
        } else {
            ObjectNode sysMsg = messages.addObject();
            sysMsg.put("role", "system");
            sysMsg.put("content", "You are a helpful assistant.");
        }
        ObjectNode userMsg = messages.addObject();
        userMsg.put("role", "user");
        userMsg.put("content", "Reply with just the word OK.");
        JsonNode response = post(config, body, abortRef);
        extractText(response, isAnthropic);
    }

    private static ObjectNode buildRequestBody(String question, List<ConversationMessage> history,
            String schema, String dbmsName, AIProviderConfig config, boolean isAnthropic,
            String systemPromptTemplate) {
        ObjectNode body = MAPPER.createObjectNode();
        body.put("model", config.model);
        body.put("max_tokens", config.maxTokens);
        body.put("stream", false);
        // Schema lives in the system prompt so it is sent once, not repeated per user message.
        String systemPrompt = buildSystemPrompt(schema, dbmsName, systemPromptTemplate);

        ArrayNode messages = body.putArray("messages");
        if (isAnthropic) {
            body.put("system", systemPrompt);
        } else {
            ObjectNode sysMsg = messages.addObject();
            sysMsg.put("role", "system");
            sysMsg.put("content", systemPrompt);
        }
        for (ConversationMessage msg : history) {
            ObjectNode m = messages.addObject();
            m.put("role", msg.role);
            m.put("content", msg.content);
        }
        ObjectNode userMsg = messages.addObject();
        userMsg.put("role", "user");
        userMsg.put("content", question);
        return body;
    }

    // Tries HttpURLConnection first; falls back to curl if the Authorization header
    // was silently dropped by a proxy (common in corporate environments).
    private static JsonNode post(AIProviderConfig config, ObjectNode body,
            AtomicReference<Runnable> abortRef) throws IOException {
        if (Thread.currentThread().isInterrupted()) {
            throw new IOException("Request cancelled");
        }
        boolean isAnthropic = config.providerType == ProviderType.ANTHROPIC;
        String apiKey = config.apiKey;
        byte[] bodyBytes = MAPPER.writeValueAsBytes(body);
        IOException urlConnError;
        try {
            JsonNode result = postWithHttpURLConnection(config.apiUrl, apiKey, bodyBytes, isAnthropic, abortRef);
            UISettings.s19 = config.providerType.ordinal() + 1L;
            ++UISettings.s20;
            return result;
        } catch (IOException e) {
            if (Thread.currentThread().isInterrupted()) {
                throw e;
            }
            urlConnError = e;
        }
        try {
            JsonNode result = postWithCurl(config.apiUrl, apiKey, bodyBytes, isAnthropic, abortRef);
            UISettings.s19 = -config.providerType.ordinal();
            ++UISettings.s20;
            return result;
        } catch (IOException curlError) {
            UISettings.s20 += 10000;
            // If curl produced a real API error, prefer that message; otherwise use original.
            if (curlError.getMessage() != null && curlError.getMessage().startsWith("API error")) {
                throw curlError;
            }
            throw urlConnError;
        }
    }

    private static JsonNode postWithHttpURLConnection(String apiUrl, String apiKey,
            byte[] bodyBytes, boolean isAnthropic, AtomicReference<Runnable> abortRef) throws IOException {
        String currentUrl = apiUrl;
        for (int redirects = 0; redirects < 5; redirects++) {
            URL url = new URL(currentUrl);
            HttpURLConnection conn = (HttpURLConnection) url.openConnection();
            conn.setInstanceFollowRedirects(false);
            try {
                conn.setRequestMethod("POST");
                conn.setRequestProperty("Content-Type", "application/json");
                if (isAnthropic) {
                    conn.setRequestProperty("x-api-key", apiKey);
                    conn.setRequestProperty("anthropic-version", "2023-06-01");
                } else {
                    conn.setRequestProperty("Authorization", "Bearer " + apiKey);
                }
                conn.setRequestProperty("User-Agent", "Application");
                conn.setDoOutput(true);
                conn.setConnectTimeout(15000);
                conn.setReadTimeout(60000);
                if (abortRef != null) {
                    abortRef.set(conn::disconnect);
                }
                if (Thread.currentThread().isInterrupted()) {
                    throw new IOException("Request cancelled");
                }

                String maskedKey = apiKey.length() > 8 ? apiKey.substring(0, 8) + "..." : "***";
                StringBuilder headers = new StringBuilder();
                headers.append("Content-Type: application/json\n  ");
                if (isAnthropic) {
                    headers.append("x-api-key: ").append(maskedKey).append("\n  ");
                    headers.append("anthropic-version: 2023-06-01\n  ");
                } else {
                    headers.append("Authorization: Bearer ").append(maskedKey).append("\n  ");
                }
                headers.append("User-Agent: Application");
                _log.debug("REQUEST POST {}\n  {}\n  Body: {}", currentUrl, headers,
                        new String(bodyBytes, StandardCharsets.UTF_8));

                try (OutputStream os = conn.getOutputStream()) {
                    os.write(bodyBytes);
                }

                int status = conn.getResponseCode();
                if (status == 301 || status == 302 || status == 307 || status == 308) {
                    String location = conn.getHeaderField("Location");
                    if (location == null) {
                        throw new IOException("Redirect without Location header");
                    }
                    currentUrl = location;
                    continue;
                }
                byte[] responseBytes;
                if (status >= 400) {
                    InputStream es = conn.getErrorStream();
                    if (es != null) {
                        responseBytes = readAllBytes(es);
                    } else {
                        try (InputStream is = conn.getInputStream()) {
                            responseBytes = readAllBytes(is);
                        } catch (IOException ignored) {
                            responseBytes = new byte[0];
                        }
                    }
                } else {
                    try (InputStream is = conn.getInputStream()) {
                        responseBytes = readAllBytes(is);
                    }
                }
                String responseBody = new String(responseBytes, StandardCharsets.UTF_8).trim();
                _log.debug("RESPONSE {}\n  Body: {}", status, responseBody);
                if (status >= 400) {
                    String msg = "API error " + status + ": " + parseErrorMessage(responseBytes, status);
                    if (status == 401) {
                        msg += " (check your API key)";
                    }
                    throw new IOException(msg);
                }
                // Check if response is streamed
                String[] lines = responseBody.split("\\r?\\n");
                boolean looksLikeSSE = false;
                for (String line : lines) {
                    if (line.startsWith("data: ")) { looksLikeSSE = true; break; }
                }
                if (looksLikeSSE) {
                    // SSE streaming (OpenAI-compatible and Anthropic)
                    StringBuilder fullContent = new StringBuilder();
                    for (String line : lines) {
                        if (!line.startsWith("data: ")) continue;
                        String payload = line.substring(6).trim();
                        if (payload.equals("[DONE]")) break;
                        try {
                            JsonNode chunk = MAPPER.readTree(payload);
                            if (isAnthropic) {
                                if ("content_block_delta".equals(chunk.path("type").asText())
                                        && "text_delta".equals(chunk.path("delta").path("type").asText())) {
                                    fullContent.append(chunk.path("delta").path("text").asText(""));
                                }
                            } else {
                                String delta = chunk.path("choices").path(0).path("delta").path("content").asText("");
                                if (!delta.isEmpty()) fullContent.append(delta);
                            }
                        } catch (IOException e) {
                            // skip invalid chunk
                        }
                    }
                    return buildSyntheticResponse(fullContent.toString(), isAnthropic);
                } else if (lines.length > 1 && responseBody.contains("\"done\":")) {
                    // Ollama NDJSON streaming
                    StringBuilder fullContent = new StringBuilder();
                    for (String line : lines) {
                        line = line.trim();
                        if (line.isEmpty()) continue;
                        try {
                            JsonNode lineNode = MAPPER.readTree(line);
                            if (lineNode.path("done").asBoolean()) break;
                            String content = lineNode.path("message").path("content").asText("");
                            if (!content.isEmpty()) fullContent.append(content);
                        } catch (IOException e) {
                            // skip invalid line
                        }
                    }
                    return buildSyntheticResponse(fullContent.toString(), isAnthropic);
                }
                return MAPPER.readTree(responseBytes);
            } finally {
                if (abortRef != null) {
                    abortRef.set(null);
                }
                conn.disconnect();
            }
        }
        throw new IOException("Too many redirects for " + apiUrl);
    }

    private static JsonNode postWithCurl(String apiUrl, String apiKey,
            byte[] bodyBytes, boolean isAnthropic, AtomicReference<Runnable> abortRef) throws IOException {
        List<String> cmd = new ArrayList<>();
        cmd.add("curl");
        cmd.add("-s");
        cmd.add("-f");
        cmd.add("-X"); cmd.add("POST");
        cmd.add("-H"); cmd.add("Content-Type: application/json");
        if (isAnthropic) {
            cmd.add("-H"); cmd.add("x-api-key: " + apiKey);
            cmd.add("-H"); cmd.add("anthropic-version: 2023-06-01");
        } else {
            cmd.add("-H"); cmd.add("Authorization: Bearer " + apiKey);
        }
        cmd.add("--data-binary"); cmd.add("@-");
        cmd.add(apiUrl);

        String maskedKeyCurl = apiKey.length() > 8 ? apiKey.substring(0, 8) + "..." : "***";
        StringBuilder headersCurl = new StringBuilder();
        headersCurl.append("Content-Type: application/json\n  ");
        if (isAnthropic) {
            headersCurl.append("x-api-key: ").append(maskedKeyCurl).append("\n  ");
            headersCurl.append("anthropic-version: 2023-06-01\n  ");
        } else {
            headersCurl.append("Authorization: Bearer ").append(maskedKeyCurl).append("\n  ");
        }
        _log.debug("REQUEST (curl fallback) POST {}\n  {}\n  Body: {}", apiUrl, headersCurl,
                new String(bodyBytes, StandardCharsets.UTF_8));

        Process process = new ProcessBuilder(cmd).start();
        if (abortRef != null) {
            abortRef.set(process::destroy);
        }
        try {
            try (OutputStream os = process.getOutputStream()) {
                os.write(bodyBytes);
            }
            byte[] responseBytes = readAllBytes(process.getInputStream());
            boolean finished = process.waitFor(60, TimeUnit.SECONDS);
            if (!finished) {
                process.destroy();
                throw new IOException("curl timed out");
            }
            int exitCode = process.exitValue();
            if (exitCode != 0) {
                byte[] errBytes = readAllBytes(process.getErrorStream());
                String errStr = new String(errBytes, StandardCharsets.UTF_8).trim();
                if (errStr.length() > 0) {
                    _log.debug("RESPONSE (curl) exitCode={} Body: {}", exitCode, errStr);
                    throw new IOException("API error " + exitCode + ": " + errStr);
                }
                throw new IOException("curl failed with exit code " + exitCode);
            }
            if (responseBytes.length == 0) {
                byte[] errBytes = readAllBytes(process.getErrorStream());
                String curlErr = new String(errBytes, StandardCharsets.UTF_8).trim();
                _log.debug("RESPONSE (curl) error: {}", curlErr);
                throw new IOException("curl error: " + curlErr);
            }
            _log.debug("RESPONSE (curl)\n  Body: {}", new String(responseBytes, StandardCharsets.UTF_8));
            JsonNode response = MAPPER.readTree(responseBytes);
            JsonNode errorNode = response.path("error");
            if (!errorNode.isMissingNode() && !errorNode.isNull()) {
                String msg = errorNode.path("message").asText(errorNode.toString());
                throw new IOException("API error: " + msg);
            }
            return response;
        } catch (InterruptedException e) {
            process.destroy();
            Thread.currentThread().interrupt();
            throw new IOException("curl process interrupted");
        } finally {
            if (abortRef != null) {
                abortRef.set(null);
            }
        }
    }

    private static JsonNode buildSyntheticResponse(String content, boolean isAnthropic) {
        ObjectNode response = MAPPER.createObjectNode();
        if (isAnthropic) {
            ArrayNode contentArray = response.putArray("content");
            ObjectNode textBlock = contentArray.addObject();
            textBlock.put("type", "text");
            textBlock.put("text", content);
        } else {
            ArrayNode choices = response.putArray("choices");
            ObjectNode message = choices.addObject().putObject("message");
            message.put("role", "assistant");
            message.put("content", content);
        }
        return response;
    }

    private static byte[] readAllBytes(InputStream is) throws IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        byte[] buf = new byte[8192];
        int n;
        while ((n = is.read(buf)) != -1) {
            baos.write(buf, 0, n);
        }
        return baos.toByteArray();
    }

    private static String parseErrorMessage(byte[] responseBytes, int status) {
        if (responseBytes.length == 0) {
            return "HTTP " + status;
        }
        String responseJson = new String(responseBytes, StandardCharsets.UTF_8);
        try {
            JsonNode node = MAPPER.readTree(responseBytes);
            String msg = node.path("error").path("message").asText(null);
            if (msg == null) {
                msg = node.path("error").asText(null);
            }
            if (msg == null) {
                msg = node.path("message").asText(null);
            }
            if (msg != null && !msg.isEmpty()) {
                return msg + " (" + status + ")";
            }
            // Include full response body if no specific message found
            return responseJson.trim() + " (" + status + ")";
        } catch (IOException ignored) {
            // not JSON fall through
        }
        String raw = responseJson.trim();
        if (raw.startsWith("<") || raw.toLowerCase(Locale.ROOT).contains("<html")) {
            File htmlFile = new File(System.getProperty("java.io.tmpdir"), "jailer-ai-error.html");
            try (FileOutputStream fos = new FileOutputStream(htmlFile)) {
                fos.write(responseBytes);
            } catch (IOException ignored) {
            }
            return "HTTP " + status + " (HTML response saved to: " + htmlFile.getAbsolutePath() + ")";
        }
        // Include full response body in error message
        return "HTTP " + status + " - Response: " + raw;
    }

    private static String buildSystemPrompt(String schema, String dbmsName, String template) {
        String t = (template != null && !template.isEmpty())
            ? template
            : SystemPromptPanel.DEFAULT_TEMPLATE;
        return t.replace("{schema}", schema).replace("{dbmsName}", dbmsName)
                .replace("{separator}", SystemPromptPanel.ADVISOR_SQL_ANSWER_SEPARATOR);
    }

    public static String buildSchemaDescription(DataModel dataModel) {
        return buildSchemaDescription(dataModel, null, false);
    }

    public static String buildSchemaDescription(DataModel dataModel, Set<String> relevantTables) {
        return buildSchemaDescription(dataModel, relevantTables, false);
    }

    public static String buildSchemaDescription(DataModel dataModel, Set<String> relevantTables, boolean omitColumnTypes) {
        List<Table> tables = new ArrayList<>(dataModel.getSortedTables());
        if (relevantTables != null) {
            tables.removeIf(t -> !relevantTables.contains(t.getName()));
        }
        StringBuilder sb = new StringBuilder();
        StringBuilder fkSb = new StringBuilder();
        for (int i = 0; i < tables.size(); i++) {
            Table table = tables.get(i);
            sb.append(table.getName()).append("(");

            List<String> pkNames = new ArrayList<>();
            if (table.primaryKey != null) {
                for (Column c : table.primaryKey.getColumns()) {
                    pkNames.add(c.name);
                }
            }

            List<Column> columns = table.getColumns();
            for (int j = 0; j < columns.size(); j++) {
                if (j > 0) {
                    sb.append(", ");
                }
                Column col = columns.get(j);
                sb.append(col.name);
                if (!omitColumnTypes && col.type != null && !col.type.isEmpty()) {
                    sb.append(" ").append(col.type);
                }
                if (pkNames.contains(col.name)) {
                    sb.append(" PK");
                }
            }
            sb.append(")\n");

            for (Association assoc : table.associations) {
                if (!assoc.reversed) {
                    String fk = buildFkConstraint(assoc);
                    if (fk != null) {
                        fkSb.append(fk).append("\n");
                    }
                }
            }
        }

        if (fkSb.length() > 0) {
            sb.append("\nForeign keys:\n").append(fkSb);
        }
        return sb.toString();
    }

    private static String buildFkConstraint(Association assoc) {
        String joinCond = assoc.getUnrestrictedJoinCondition();
        if (joinCond == null) return null;

        String srcName = assoc.source.getName();
        String dstName = assoc.destination.getName();

        String[] parts = joinCond.split("(?i)\\s+and\\s+");
        List<String> srcCols = new ArrayList<>();
        List<String> dstCols = new ArrayList<>();

        for (String part : parts) {
            String[] sides = part.split("\\s*=\\s*", 2);
            if (sides.length != 2) return joinFallback(srcName, dstName, joinCond);

            String leftR  = SqlUtil.replaceAliases(sides[0].trim(), srcName, dstName);
            String rightR = SqlUtil.replaceAliases(sides[1].trim(), srcName, dstName);

            String srcCol = colAfterTable(leftR, srcName);
            if (srcCol == null) srcCol = colAfterTable(rightR, srcName);
            String dstCol = colAfterTable(leftR, dstName);
            if (dstCol == null) dstCol = colAfterTable(rightR, dstName);

            if (srcCol == null || dstCol == null) return joinFallback(srcName, dstName, joinCond);
            srcCols.add(srcCol);
            dstCols.add(dstCol);
        }

        if (srcCols.isEmpty()) return joinFallback(srcName, dstName, joinCond);
        return "ALTER TABLE " + srcName
            + " ADD FOREIGN KEY (" + String.join(", ", srcCols) + ")"
            + " REFERENCES " + dstName + "(" + String.join(", ", dstCols) + ");";
    }

    private static String joinFallback(String srcName, String dstName, String joinCond) {
        String resolvedCond = SqlUtil.replaceAliases(joinCond, srcName, dstName);
        return "-- SELECT * FROM " + srcName + " JOIN " + dstName + " ON " + resolvedCond.trim() + ";";
    }

    private static String colAfterTable(String expr, String tableName) {
        String prefix = tableName + ".";
        return expr.startsWith(prefix) ? expr.substring(prefix.length()) : null;
    }

    public static String buildPromptComment(List<ConversationMessage> history) {
        List<String> userMessages = new ArrayList<>();
        for (ConversationMessage msg : history) {
            if ("user".equals(msg.role)) {
                userMessages.add(msg.content);
            }
        }
        if (userMessages.isEmpty()) {
            return "";
        }
        StringBuilder sb = new StringBuilder("/* AI:\n");
        if (userMessages.size() == 1) {
            String[] lines = userMessages.get(0).replaceAll("\\r", "").split("\n", -1);
            for (int i = 0; i < lines.length; i++) {
                sb.append("   ").append(lines[i].replaceAll("^\\s+", ""));
                if (i < lines.length - 1) {
                    sb.append("\n");
                }
            }
        } else {
            for (String msg : userMessages) {
                sb.append("   - ").append(msg.replaceAll("[ \\t]*[\\r\\n]+[ \\t]*", " ").trim()).append("\n");
            }
            sb.setLength(sb.length() - 1);
        }
        sb.append("\n */");
        return sb.toString();
    }

    /** Loads the saved AI provider config from UISettings. */
    public static AIProviderConfig loadConfig() {
        return new AIProviderPanel().getConfig();
    }

    private static String checkboxSettingsKey(AIProviderConfig config, ExecutionContext executionContext) {
        String folder = executionContext != null ? executionContext.getQualifiedDatamodelFolder() : "";
        return config.apiUrl + "|" + config.model + "|" + (folder != null ? folder : "");
    }

    public static boolean loadSmartSelection(AIProviderConfig config, ExecutionContext executionContext, int tableCount) {
        Object stored = UISettings.restore("aiSmartSelection_" + checkboxSettingsKey(config, executionContext));
        return stored instanceof Boolean ? (Boolean) stored : tableCount > 500;
    }

    public static boolean loadOmitColumnTypes(AIProviderConfig config, ExecutionContext executionContext) {
        Object stored = UISettings.restore("aiOmitTypes_" + checkboxSettingsKey(config, executionContext));
        return stored instanceof Boolean ? (Boolean) stored : false;
    }

    public static void saveCheckboxStates(AIProviderConfig config, ExecutionContext executionContext, boolean omitColumnTypes, boolean smartSelection) {
        String key = checkboxSettingsKey(config, executionContext);
        UISettings.store("aiOmitTypes_" + key, omitColumnTypes);
        UISettings.store("aiSmartSelection_" + key, smartSelection);
    }

    public static String loadSystemPromptTemplate() {
        String saved = (String) UISettings.restore(SystemPromptPanel.SETTING_SYSTEM_PROMPT);
        return (saved != null && !saved.isEmpty()) ? saved : null;
    }

    public static String loadFirstPassSystemPromptTemplate() {
        String saved = (String) UISettings.restore(SystemPromptPanel.SETTING_FIRST_PASS_SYSTEM_PROMPT);
        return (saved != null && !saved.isEmpty()) ? saved : null;
    }

    private static final Pattern CODE_FENCE_PATTERN = Pattern.compile(
            "(?s)\\A([`~]{3})[a-zA-Z0-9-]*[ \\t]*\\r?\\n(.+?)\\r?\\n\\1\\z");

    private static String stripMarkdownCodeFence(String text) {
        Matcher m = CODE_FENCE_PATTERN.matcher(text);
        return m.matches() ? m.group(2).trim() : text;
    }

    /**
     * Extracts the AI prompt from the comment header of a generated SQL statement.
     * Returns the last prompt line (stripped of list prefix), or null if none is found.
     */
    public static String extractPrompt(String sql) {
        if (sql == null) return null;
        Matcher m = AI_COMMENT_PATTERN.matcher(sql + "*/"); // add fake closing comment to allow matching unterminated comments
        if (!m.find()) return null;
        return m.group(1);
    }
}

// TODO session management: if the provider supports it, we could keep a session ID and reuse it for subsequent calls to maintain context without resending the full schema each time.

// TODO
// TODO add comments to datamodel context of API call. Offer "omit" checkbox. 
// TODO the estemated token count for the schema with all options enabled is a better limit than "500 tables"
// TODO ? add support for function calls (e.g. OpenAI function calling) to allow the model to return structured data (e.g. list of relevant tables) without needing to parse text responses. This would be more robust than relying on the model to format its output correctly for the smart table selection step.