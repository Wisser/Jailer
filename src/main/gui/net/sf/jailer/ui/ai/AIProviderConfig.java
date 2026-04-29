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

/**
 * Immutable configuration for an AI provider (endpoint, credentials, model).
 */
public class AIProviderConfig {

    public enum ProviderType {
        ANTHROPIC(
            "Anthropic",
            "https://api.anthropic.com/v1/messages",
            "claude-haiku-4-5-20251001"),
        OPENAI_COMPATIBLE(
            "OpenAI-compatible",
            "https://api.openai.com/v1/chat/completions",
            "gpt-4o-mini"),
        OPENROUTER(
            "OpenRouter",
            "https://openrouter.ai/api/v1/chat/completions",
            "meta-llama/llama-3.1-8b-instruct:free");

        public final String displayName;
        public final String defaultApiUrl;
        public final String defaultModel;

        ProviderType(String displayName, String defaultApiUrl, String defaultModel) {
            this.displayName = displayName;
            this.defaultApiUrl = defaultApiUrl;
            this.defaultModel = defaultModel;
        }

        @Override
        public String toString() {
            return displayName;
        }
    }

    public final ProviderType providerType;
    public final String apiUrl;
    public final String apiKey;
    public final String model;

    public AIProviderConfig(ProviderType providerType, String apiUrl, String apiKey, String model) {
        this.providerType = providerType;
        this.apiUrl = (apiUrl != null && !apiUrl.isEmpty()) ? apiUrl : providerType.defaultApiUrl;
        this.apiKey = apiKey != null ? apiKey : "";
        this.model = (model != null && !model.isEmpty()) ? model : providerType.defaultModel;
    }
}
