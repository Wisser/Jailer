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
import net.sf.jailer.ui.util.StringObfuscator;
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

    private static final StringObfuscator STRING_OBFUSCATOR = new StringObfuscator();

    private final JComboBox<ProviderType> providerCombo;
    private final JTextField urlField;
    private final JTextField modelField;
    private final JPasswordField apiKeyField;
    private final JSpinner maxTokensSpinner;
    private final JLabel apiKeyLabel;
    private final JButton saveButton;
    private final JButton testButton;
    private boolean connectionVerified = true;

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

        // 5-column grid:
        //  row 0: Provider [combo]  URL [field, wide x2]
        //  row 1: Model [field]     Max tokens [spinner]  Reset
        //  row 2: API Key [field, spans cols 1-4]
        //  row 3: test row (spans all)

        GridBagConstraints lc = new GridBagConstraints();
        lc.anchor = GridBagConstraints.WEST;
        lc.insets = new Insets(2, 4, 2, 4);
        GridBagConstraints fc = new GridBagConstraints();
        fc.fill = GridBagConstraints.HORIZONTAL;
        fc.insets = new Insets(2, 0, 2, 8);

        // row 0
        lc.gridx = 0; lc.gridy = 0; add(new JLabel("Provider"), lc);
        fc.gridx = 1; fc.gridy = 0; add(providerCombo, fc);
        lc.gridx = 2; lc.gridy = 0; add(new JLabel("URL"), lc);
        fc.gridx = 3; fc.gridy = 0; fc.gridwidth = 2; fc.weightx = 1.0; add(urlField, fc);
        fc.gridwidth = 1; fc.weightx = 0;

        // row 1
        lc.gridx = 0; lc.gridy = 1; add(new JLabel("Model"), lc);
        fc.gridx = 1; fc.gridy = 1; add(modelField, fc);
        lc.gridx = 2; lc.gridy = 1; add(new JLabel("Max. response tokens"), lc);
        GridBagConstraints sc = new GridBagConstraints();
        sc.gridx = 3; sc.gridy = 1; sc.insets = new Insets(2, 0, 2, 8); sc.anchor = GridBagConstraints.WEST;
        add(maxTokensSpinner, sc);

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
        rc.gridx = 4; rc.gridy = 1;
        rc.anchor = GridBagConstraints.EAST;
        rc.insets = new Insets(2, 8, 2, 8);
        add(resetButton, rc);

        // row 2
        apiKeyLabel = new JLabel(apiKeyLabelText(savedProvider));
        lc.gridx = 0; lc.gridy = 2; add(apiKeyLabel, lc);
        fc.gridx = 1; fc.gridy = 2; fc.gridwidth = 4; fc.weightx = 1.0; add(apiKeyField, fc);
        fc.gridwidth = 1; fc.weightx = 0;

        testButton = new JButton("Test Connection");
        ImageIcon testIcon = UIUtil.readImage("/sync.png");
        if (testIcon != null) {
            testButton.setIcon(UIUtil.scaleIcon(testButton, testIcon));
        }
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
                    cancelTestButton.setVisible(false);
                    try {
                        get();
                        testStatusLabel.setForeground(new Color(0, 140, 0));
                        testStatusLabel.setText("Connection successful");
                        markConnectionVerified();
                    } catch (java.util.concurrent.CancellationException ex) {
                        testStatusLabel.setForeground(UIManager.getColor("Label.foreground"));
                        testStatusLabel.setText("Cancelled");
                        markConnectionFailed();
                    } catch (Exception ex) {
                        testStatusLabel.setForeground(Color.RED);
                        testStatusLabel.setText("Connection failed");
                        markConnectionFailed();
                        Throwable cause = ex.getCause() != null ? ex.getCause() : ex;
                        UIUtil.showException(AIProviderPanel.this, "Test Connection", cause);
                    }
                }
            };
            workerHolder[0] = worker;
            worker.execute();
        });

        saveButton = new JButton("Save");
        ImageIcon saveIcon = UIUtil.readImage("/buttonok.png");
        if (saveIcon != null) {
            saveButton.setIcon(UIUtil.scaleIcon(saveButton, saveIcon));
        }
        saveButton.addActionListener(e -> {
            saveSettings();
        });

        JPanel testRow = new JPanel(new java.awt.FlowLayout(java.awt.FlowLayout.LEFT, 4, 0));
        testRow.setOpaque(false);
        testRow.add(saveButton);
        testRow.add(testButton);
        testRow.add(cancelTestButton);
        testRow.add(testStatusLabel);

        GridBagConstraints trc = new GridBagConstraints();
        trc.gridx = 0; trc.gridy = 3; trc.gridwidth = 5;
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
            updateSaveButton();
            markConnectionFailed();
        });

        javax.swing.event.DocumentListener dl = new javax.swing.event.DocumentListener() {
            public void insertUpdate(javax.swing.event.DocumentEvent e) { updateSaveButton(); markConnectionFailed(); }
            public void removeUpdate(javax.swing.event.DocumentEvent e) { updateSaveButton(); markConnectionFailed(); }
            public void changedUpdate(javax.swing.event.DocumentEvent e) { updateSaveButton(); markConnectionFailed(); }
        };
        urlField.getDocument().addDocumentListener(dl);
        modelField.getDocument().addDocumentListener(dl);
        apiKeyField.getDocument().addDocumentListener(dl);
        maxTokensSpinner.addChangeListener(e -> { updateSaveButton(); markConnectionFailed(); });
        updateSaveButton();
        updateTestButton();
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
        UISettings.store(SETTING_API_KEY_PREFIX + config.providerType.name(),
                STRING_OBFUSCATOR.encrypt(config.apiKey));
        updateSaveButton();
    }

    private void updateSaveButton() {
        saveButton.setEnabled(!isSaved());
    }

    private void updateTestButton() {
        testButton.setEnabled(!connectionVerified);
    }

    public void markConnectionVerified() {
        connectionVerified = true;
        updateTestButton();
        saveSettings();
    }

    public void markConnectionFailed() {
        connectionVerified = false;
        updateTestButton();
    }

    private boolean isSaved() {
        ProviderType pt = (ProviderType) providerCombo.getSelectedItem();
        if (pt != loadProviderType()) return false;
        Object storedUrl = UISettings.restore(SETTING_API_URL);
        String expectedUrl = storedUrl instanceof String ? (String) storedUrl : pt.defaultApiUrl;
        if (!urlField.getText().trim().equals(expectedUrl)) return false;
        Object storedModel = UISettings.restore(SETTING_MODEL);
        String expectedModel = storedModel instanceof String ? (String) storedModel : pt.defaultModel;
        if (!modelField.getText().trim().equals(expectedModel)) return false;
        if ((Integer) maxTokensSpinner.getValue() != loadMaxTokens()) return false;
        String storedKey = loadApiKey(pt);
        if (!getApiKey().equals(storedKey != null ? storedKey : "")) return false;
        return true;
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
            return deobfuscate((String) perProvider);
        }
        Object legacy = UISettings.restore("aiApiKey");
        return legacy instanceof String ? deobfuscate((String) legacy) : null;
    }

    private static String deobfuscate(String value) {
        return STRING_OBFUSCATOR.decrypt(value);
    }
}

// TODO
// TODO make existing SQL-Statements available as context for the AI assistant for improvement suggestions.
// TODO support multiple "profiles" for use cases, allowing users to switch between them easily.
// TODO add option to save multiple provider configurations and switch between them (e.g. for different projects or use cases).
