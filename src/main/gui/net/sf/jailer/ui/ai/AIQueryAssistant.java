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
import java.util.Locale;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.TimeUnit;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.ai.AIProviderConfig.ProviderType;

/**
 * Sends a natural-language question together with the current data-model schema
 * to an AI API and returns the generated SQL.
 * Supports Anthropic and OpenAI-compatible endpoints (OpenAI, Azure, Groq, Ollama, OpenRouter, ...).
 * Uses HttpURLConnection with a curl subprocess fallback to handle proxies that strip
 * the Authorization header.
 */
public class AIQueryAssistant {

    private static final Logger _log = LoggerFactory.getLogger("ai_api");
    private static final int MAX_TABLES = 120;
    private static final ObjectMapper MAPPER = new ObjectMapper();

    public static String generateSQL(String question, List<ConversationMessage> history,
            DataModel dataModel, String dbmsName, AIProviderConfig config) throws IOException {
        String schema = buildSchemaDescription(dataModel);
        boolean isAnthropic = config.providerType == ProviderType.ANTHROPIC;
        ObjectNode body = buildRequestBody(question, history, schema, dbmsName, config, isAnthropic);
        JsonNode response = post(config.apiUrl, config.apiKey, body, isAnthropic);
        if (isAnthropic) {
            return response.path("content").get(0).path("text").asText("").trim();
        } else {
            return response.path("choices").get(0).path("message").path("content").asText("").trim();
        }
    }

    public static String generateSQL(String question, DataModel dataModel, String dbmsName, AIProviderConfig config) throws IOException {
        return generateSQL(question, Collections.emptyList(), dataModel, dbmsName, config);
    }

    private static ObjectNode buildRequestBody(String question, List<ConversationMessage> history,
            String schema, String dbmsName, AIProviderConfig config, boolean isAnthropic) {
        ObjectNode body = MAPPER.createObjectNode();
        body.put("model", config.model);
        body.put("max_tokens", 1024);
        // Schema lives in the system prompt so it is sent once, not repeated per user message.
        String systemPrompt = buildSystemPrompt(schema, dbmsName);

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
    private static JsonNode post(String apiUrl, String apiKey, ObjectNode body, boolean isAnthropic) throws IOException {
        byte[] bodyBytes = MAPPER.writeValueAsBytes(body);
        IOException urlConnError;
        try {
            return postWithHttpURLConnection(apiUrl, apiKey, bodyBytes, isAnthropic);
        } catch (IOException e) {
            urlConnError = e;
        }
        try {
            return postWithCurl(apiUrl, apiKey, bodyBytes, isAnthropic);
        } catch (IOException curlError) {
            // If curl produced a real API error, prefer that message; otherwise use original.
            if (curlError.getMessage() != null && curlError.getMessage().startsWith("API error")) {
                throw curlError;
            }
            throw urlConnError;
        }
    }

    private static JsonNode postWithHttpURLConnection(String apiUrl, String apiKey,
            byte[] bodyBytes, boolean isAnthropic) throws IOException {
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

                String maskedKey = apiKey.length() > 8 ? apiKey.substring(0, 8) + "..." : "***";
                String authHeader = isAnthropic ? "x-api-key: " + maskedKey : "Authorization: Bearer " + maskedKey;
                _log.debug("REQUEST POST {}\n  {}\n  Body: {}", currentUrl, authHeader,
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
                    responseBytes = (es != null) ? readAllBytes(es) : new byte[0];
                } else {
                    try (InputStream is = conn.getInputStream()) {
                        responseBytes = readAllBytes(is);
                    }
                }
                _log.debug("RESPONSE {}\n  Body: {}", status, new String(responseBytes, StandardCharsets.UTF_8).trim());
                if (status >= 400) {
                    throw new IOException("API error " + status + ": " + parseErrorMessage(responseBytes, status));
                }
                return MAPPER.readTree(responseBytes);
            } finally {
                conn.disconnect();
            }
        }
        throw new IOException("Too many redirects for " + apiUrl);
    }

    private static JsonNode postWithCurl(String apiUrl, String apiKey,
            byte[] bodyBytes, boolean isAnthropic) throws IOException {
        List<String> cmd = new ArrayList<>();
        cmd.add("curl");
        cmd.add("-s");
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

        _log.debug("REQUEST (curl fallback) POST {}\n  Body: {}", apiUrl,
                new String(bodyBytes, StandardCharsets.UTF_8));

        Process process = new ProcessBuilder(cmd).start();
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
            Thread.currentThread().interrupt();
            throw new IOException("curl process interrupted");
        }
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
        try {
            JsonNode node = MAPPER.readTree(responseBytes);
            String msg = node.path("error").path("message").asText(null);
            if (msg == null) {
                msg = node.path("message").asText(null);
            }
            if (msg != null && !msg.isEmpty()) {
                return msg;
            }
        } catch (IOException ignored) {
            // not JSON — fall through
        }
        String raw = new String(responseBytes, StandardCharsets.UTF_8).trim();
        if (raw.startsWith("<") || raw.toLowerCase(Locale.ROOT).contains("<html")) {
            File htmlFile = new File(System.getProperty("java.io.tmpdir"), "jailer-ai-error.html");
            try (FileOutputStream fos = new FileOutputStream(htmlFile)) {
                fos.write(responseBytes);
            } catch (IOException ignored) {
            }
            return "HTTP " + status + " (HTML response saved to: " + htmlFile.getAbsolutePath() + ")";
        }
        return raw.length() > 300 ? raw.substring(0, 300) + "..." : raw;
    }

    private static String buildSystemPrompt(String schema, String dbmsName) {
        return "You are a SQL expert for " + dbmsName + ".\n"
            + "Database schema:\n" + schema + "\n"
            + "Return ONLY raw SQL - no explanation, no code fences, no trailing semicolon. "
            + "Use only tables and columns from the schema above.";
    }

    static String buildSchemaDescription(DataModel dataModel) {
        List<Table> tables = new ArrayList<>(dataModel.getSortedTables());
        StringBuilder sb = new StringBuilder();
        int count = Math.min(tables.size(), MAX_TABLES);
        for (int i = 0; i < count; i++) {
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
                if (col.type != null && !col.type.isEmpty()) {
                    sb.append(" ").append(col.type);
                }
                if (pkNames.contains(col.name)) {
                    sb.append(" PK");
                }
            }
            sb.append(")");

            for (Association assoc : table.associations) {
                if (!assoc.reversed) {
                    String joinCond = assoc.getUnrestrictedJoinCondition();
                    if (joinCond != null) {
                        String resolved = joinCond
                            .replace("A.", assoc.source.getName() + ".")
                            .replace("B.", assoc.destination.getName() + ".");
                        sb.append(" -- FK: ").append(resolved.trim());
                    }
                }
            }
            sb.append("\n");
        }
        if (tables.size() > MAX_TABLES) {
            sb.append("... and ").append(tables.size() - MAX_TABLES).append(" more tables\n");
        }
        return sb.toString();
    }
}

// TODO 
// TODO put comments into context
