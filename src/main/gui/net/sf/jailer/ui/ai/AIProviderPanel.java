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

import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ItemEvent;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JTextField;

import net.sf.jailer.ui.ai.AIProviderConfig.ProviderType;
import net.sf.jailer.ui.util.UISettings;

/**
 * Reusable Swing panel for configuring an AI provider (endpoint, API key, model).
 * Loads from and saves to {@link UISettings}.
 */
public class AIProviderPanel extends JPanel {

    private static final long serialVersionUID = 1L;

    static final String SETTING_PROVIDER       = "aiProviderType";
    static final String SETTING_API_URL        = "aiApiUrl";
    static final String SETTING_MODEL          = "aiModel";
    static final String SETTING_MAX_TOKENS     = "aiMaxTokens";
    static final String SETTING_API_KEY_PREFIX = "aiApiKey_";

    private final JComboBox<ProviderType> providerCombo;
    private final JTextField urlField;
    private final JTextField modelField;
    private final JTextField maxTokensField;
    private final JPasswordField apiKeyField;

    public AIProviderPanel() {
        super(new GridBagLayout());

        ProviderType savedProvider = loadProviderType();
        String savedUrl   = (String) UISettings.restore(SETTING_API_URL);
        String savedModel = (String) UISettings.restore(SETTING_MODEL);
        String savedKey   = loadApiKey(savedProvider);

        providerCombo  = new JComboBox<>(ProviderType.values());
        providerCombo.setSelectedItem(savedProvider);

        urlField       = new JTextField(savedUrl   != null ? savedUrl   : savedProvider.defaultApiUrl, 36);
        modelField     = new JTextField(savedModel != null ? savedModel : savedProvider.defaultModel,   18);
        maxTokensField = new JTextField((String) UISettings.restore(SETTING_MAX_TOKENS), 6);
        if (maxTokensField.getText().isEmpty()) {
            maxTokensField.setText("1024");
        }
        apiKeyField = new JPasswordField(36);
        if (savedKey != null) {
            apiKeyField.setText(savedKey);
        }

        GridBagConstraints lc = new GridBagConstraints();
        lc.anchor = GridBagConstraints.WEST;
        lc.insets = new Insets(2, 4, 2, 4);
        GridBagConstraints fc = new GridBagConstraints();
        fc.fill = GridBagConstraints.HORIZONTAL;
        fc.insets = new Insets(2, 0, 2, 8);

        lc.gridx = 0; lc.gridy = 0; add(new JLabel("Provider:"), lc);
        fc.gridx = 1; fc.gridy = 0; add(providerCombo, fc);
        lc.gridx = 2; lc.gridy = 0; add(new JLabel("URL:"), lc);
        fc.gridx = 3; fc.gridy = 0; fc.weightx = 1.0; add(urlField, fc); fc.weightx = 0;
        lc.gridx = 4; lc.gridy = 0; add(new JLabel("Model:"), lc);
        fc.gridx = 5; fc.gridy = 0; add(modelField, fc);

        lc.gridx = 0; lc.gridy = 1; add(new JLabel("API Key:"), lc);
        fc.gridx = 1; fc.gridy = 1; fc.gridwidth = 5; fc.weightx = 1.0; add(apiKeyField, fc);
        fc.gridwidth = 1; fc.weightx = 0;

        JButton resetButton = new JButton("Reset to Default");
        resetButton.addActionListener(e -> {
            int choice = JOptionPane.showConfirmDialog(
                this,
                "Reset all fields to their defaults for the selected provider?",
                "Reset to Default",
                JOptionPane.YES_NO_OPTION);
            if (choice != JOptionPane.YES_OPTION) {
                return;
            }
            ProviderType current = (ProviderType) providerCombo.getSelectedItem();
            urlField.setText(current.defaultApiUrl);
            modelField.setText(current.defaultModel);
            maxTokensField.setText("1024");
            apiKeyField.setText("");
        });

        lc.gridx = 0; lc.gridy = 2; add(new JLabel("Max Tokens:"), lc);
        fc.gridx = 1; fc.gridy = 2; add(maxTokensField, fc);

        GridBagConstraints rc = new GridBagConstraints();
        rc.gridx = 5; rc.gridy = 2;
        rc.anchor = GridBagConstraints.EAST;
        rc.insets = new Insets(2, 0, 2, 0);
        add(resetButton, rc);

        ProviderType[] prev = { savedProvider };
        providerCombo.addItemListener(e -> {
            if (e.getStateChange() != ItemEvent.SELECTED) {
                return;
            }
            ProviderType next = (ProviderType) providerCombo.getSelectedItem();
            if (urlField.getText().trim().equals(prev[0].defaultApiUrl)) {
                urlField.setText(next.defaultApiUrl);
            }
            if (modelField.getText().trim().equals(prev[0].defaultModel)) {
                modelField.setText(next.defaultModel);
            }
            String key = loadApiKey(next);
            apiKeyField.setText(key != null ? key : "");
            prev[0] = next;
        });
    }

    /** Returns the API key currently entered (trimmed). */
    public String getApiKey() {
        return new String(apiKeyField.getPassword()).trim();
    }

    /** Returns the API key field so callers can request focus on validation failure. */
    public Component getApiKeyComponent() {
        return apiKeyField;
    }

    /** Builds an {@link AIProviderConfig} from the current field values. */
    public AIProviderConfig getConfig() {
        int maxTokens = 1024;
        try {
            maxTokens = Integer.parseInt(maxTokensField.getText().trim());
            if (maxTokens <= 0) {
                maxTokens = 1024;
            }
        } catch (NumberFormatException ignored) {
        }
        return new AIProviderConfig(
            (ProviderType) providerCombo.getSelectedItem(),
            urlField.getText().trim(),
            getApiKey(),
            modelField.getText().trim(),
            maxTokens
        );
    }

    /** Persists the current settings to {@link UISettings}. */
    public void saveSettings() {
        AIProviderConfig config = getConfig();
        UISettings.store(SETTING_PROVIDER,   config.providerType.name());
        UISettings.store(SETTING_API_URL,    config.apiUrl);
        UISettings.store(SETTING_MODEL,      config.model);
        UISettings.store(SETTING_MAX_TOKENS, String.valueOf(config.maxTokens));
        UISettings.store(SETTING_API_KEY_PREFIX + config.providerType.name(), config.apiKey);
    }

    private ProviderType loadProviderType() {
        Object stored = UISettings.restore(SETTING_PROVIDER);
        if (stored instanceof String) {
            try {
                return ProviderType.valueOf((String) stored);
            } catch (IllegalArgumentException ignored) {
            }
        }
        return ProviderType.ANTHROPIC;
    }

    private String loadApiKey(ProviderType providerType) {
        Object perProvider = UISettings.restore(SETTING_API_KEY_PREFIX + providerType.name());
        if (perProvider instanceof String && !((String) perProvider).isEmpty()) {
            return (String) perProvider;
        }
        Object legacy = UISettings.restore("aiApiKey");
        return legacy instanceof String ? (String) legacy : null;
    }
}


// TODO
// TODO check: is GitHub-Copilot-Chat an OpenAI-compatible provider? If yes, add it to the enum with its defaults. If no, consider how to support it (custom provider type with some special handling?).

// TODO
// TODO menu item in "Tools": "AI-Assistant". Switches to default console.

// TODO
// TODO make existing SQL-Statements available as context for the AI assistant for improvement suggestions.
