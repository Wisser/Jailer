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

import java.awt.Color;
import java.awt.Component;
import java.util.concurrent.atomic.AtomicReference;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ItemEvent;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingWorker;
import javax.swing.UIManager;

import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.ai.AIProviderConfig.ProviderType;
import net.sf.jailer.ui.util.UISettings;

/**
 * Reusable Swing panel for configuring an AI provider (endpoint, API key, model).
 * Loads from and saves to {@link UISettings}.
 */
public class AIProviderPanel extends JPanel {

    private static final long serialVersionUID = 1L;

    static final String SETTING_PROVIDER        = "aiProviderType";
    static final String SETTING_API_URL         = "aiApiUrl";
    static final String SETTING_MODEL           = "aiModel";
    static final String SETTING_API_KEY_PREFIX  = "aiApiKey_";
    static final String SETTING_MAX_TOKENS      = "aiMaxTokens";

    private final JComboBox<ProviderType> providerCombo;
    private final JTextField urlField;
    private final JTextField modelField;
    private final JPasswordField apiKeyField;
    private final JSpinner maxTokensSpinner;
    private final JLabel apiKeyLabel;

    public AIProviderPanel() {
        super(new GridBagLayout());

        ProviderType savedProvider = loadProviderType();
        String savedUrl       = (String) UISettings.restore(SETTING_API_URL);
        String savedModel     = (String) UISettings.restore(SETTING_MODEL);
        String savedKey       = loadApiKey(savedProvider);
        int    savedMaxTokens = loadMaxTokens();

        providerCombo  = new JComboBox<>(ProviderType.values());
        providerCombo.setSelectedItem(savedProvider);

        urlField    = new JTextField(savedUrl   != null ? savedUrl   : savedProvider.defaultApiUrl, 36);
        modelField  = new JTextField(savedModel != null ? savedModel : savedProvider.defaultModel,   20);
        apiKeyField = new JPasswordField(36);
        if (savedKey != null) {
            apiKeyField.setText(savedKey);
        }
        maxTokensSpinner = new JSpinner(new SpinnerNumberModel(savedMaxTokens, 256, 32768, 256));
        ((JSpinner.NumberEditor) maxTokensSpinner.getEditor()).getTextField().setColumns(5);

        GridBagConstraints lc = new GridBagConstraints();
        lc.anchor = GridBagConstraints.WEST;
        lc.insets = new Insets(2, 4, 2, 4);
        GridBagConstraints fc = new GridBagConstraints();
        fc.fill = GridBagConstraints.HORIZONTAL;
        fc.insets = new Insets(2, 0, 2, 8);

        lc.gridx = 0; lc.gridy = 0; add(new JLabel("Provider"), lc);
        fc.gridx = 1; fc.gridy = 0; add(providerCombo, fc);
        lc.gridx = 2; lc.gridy = 0; add(new JLabel("URL"), lc);
        fc.gridx = 3; fc.gridy = 0; fc.weightx = 1.0; add(urlField, fc); fc.weightx = 0;
        lc.gridx = 4; lc.gridy = 0; add(new JLabel("Model"), lc);
        fc.gridx = 5; fc.gridy = 0; fc.weightx = 0.3; add(modelField, fc); fc.weightx = 0;
        lc.gridx = 6; lc.gridy = 0; add(new JLabel("Max. response tokens"), lc);
        GridBagConstraints sc = new GridBagConstraints();
        sc.gridx = 7; sc.gridy = 0; sc.insets = new Insets(2, 0, 2, 8); sc.anchor = GridBagConstraints.WEST;
        add(maxTokensSpinner, sc);

        apiKeyLabel = new JLabel(apiKeyLabelText(savedProvider));
        lc.gridx = 0; lc.gridy = 1; add(apiKeyLabel, lc);
        fc.gridx = 1; fc.gridy = 1; fc.gridwidth = 6; fc.weightx = 1.0; add(apiKeyField, fc);
        fc.gridwidth = 1; fc.weightx = 0;

        JButton resetButton = new JButton("Reset to Default");
        ImageIcon resetIcon = UIUtil.readImage("/reset_64.png");
        if (resetIcon != null) {
            resetButton.setIcon(UIUtil.scaleIcon(resetButton, resetIcon));
        }
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
            apiKeyField.setText("");
            maxTokensSpinner.setValue(AIProviderConfig.DEFAULT_MAX_TOKENS);
        });

        GridBagConstraints rc = new GridBagConstraints();
        rc.gridx = 7; rc.gridy = 1;
        rc.anchor = GridBagConstraints.EAST;
        rc.weightx = 1.0;
        rc.insets = new Insets(2, 8, 2, 8);
        add(resetButton, rc);

        JButton testButton = new JButton("Test Connection");
        JButton cancelTestButton = new JButton("Cancel");
        cancelTestButton.setVisible(false);
        JLabel testStatusLabel = new JLabel();

        AtomicReference<Runnable> abortRef = new AtomicReference<>();
        SwingWorker<?,?>[] workerHolder = { null };

        cancelTestButton.addActionListener(e -> {
            Runnable abort = abortRef.get();
            if (abort != null) abort.run();
            if (workerHolder[0] != null) workerHolder[0].cancel(true);
        });

        testButton.addActionListener(e -> {
            testButton.setEnabled(false);
            cancelTestButton.setVisible(true);
            testStatusLabel.setForeground(UIManager.getColor("Label.foreground"));
            testStatusLabel.setText("Testing...");
            AIProviderConfig cfg = getConfig();
            SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {
                @Override
                protected Void doInBackground() throws Exception {
                    AIQueryAssistant.testConnection(cfg, abortRef);
                    return null;
                }
                @Override
                protected void done() {
                    testButton.setEnabled(true);
                    cancelTestButton.setVisible(false);
                    try {
                        get();
                        testStatusLabel.setForeground(new Color(0, 140, 0));
                        testStatusLabel.setText("Connection successful");
                    } catch (java.util.concurrent.CancellationException ex) {
                        testStatusLabel.setForeground(UIManager.getColor("Label.foreground"));
                        testStatusLabel.setText("Cancelled");
                    } catch (Exception ex) {
                        testStatusLabel.setForeground(Color.RED);
                        testStatusLabel.setText("Connection failed");
                        Throwable cause = ex.getCause() != null ? ex.getCause() : ex;
                        UIUtil.showException(AIProviderPanel.this, "Test Connection", cause);
                    }
                }
            };
            workerHolder[0] = worker;
            worker.execute();
        });

        JPanel testRow = new JPanel(new java.awt.FlowLayout(java.awt.FlowLayout.LEFT, 4, 0));
        testRow.setOpaque(false);
        testRow.add(testButton);
        testRow.add(cancelTestButton);
        testRow.add(testStatusLabel);

        GridBagConstraints trc = new GridBagConstraints();
        trc.gridx = 0; trc.gridy = 2; trc.gridwidth = 8;
        trc.fill = GridBagConstraints.HORIZONTAL; trc.weightx = 1.0;
        trc.insets = new Insets(4, 0, 2, 0);
        add(testRow, trc);

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
            apiKeyLabel.setText(apiKeyLabelText(next));
            String key = loadApiKey(next);
            apiKeyField.setText(key != null ? key : "");
            prev[0] = next;
        });
    }

    private static String apiKeyLabelText(ProviderType type) {
        return "API Key";
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
        return new AIProviderConfig(
            (ProviderType) providerCombo.getSelectedItem(),
            urlField.getText().trim(),
            getApiKey(),
            modelField.getText().trim(),
            (Integer) maxTokensSpinner.getValue()
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

    private int loadMaxTokens() {
        Object stored = UISettings.restore(SETTING_MAX_TOKENS);
        if (stored instanceof String) {
            try {
                int v = Integer.parseInt((String) stored);
                if (v >= 256 && v <= 32768) return v;
            } catch (NumberFormatException ignored) {
            }
        }
        return AIProviderConfig.DEFAULT_MAX_TOKENS;
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
// TODO make existing SQL-Statements available as context for the AI assistant for improvement suggestions.
