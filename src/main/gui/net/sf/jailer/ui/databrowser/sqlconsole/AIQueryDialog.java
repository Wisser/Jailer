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
package net.sf.jailer.ui.databrowser.sqlconsole;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Window;
import java.awt.event.ItemEvent;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.function.Consumer;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingWorker;

import org.fife.ui.rtextarea.RTextScrollPane;

import net.sf.jailer.ui.syntaxtextarea.RSyntaxTextAreaWithSQLSyntaxStyle;

import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.ui.ai.AIProviderConfig;
import net.sf.jailer.ui.ai.AIProviderConfig.ProviderType;
import net.sf.jailer.ui.ai.AIQueryAssistant;
import net.sf.jailer.ui.ai.ConversationMessage;
import net.sf.jailer.ui.util.UISettings;

/**
 * Modal dialog that lets the user have a multi-turn conversation with an AI
 * to generate SQL queries. Each exchange is added to the conversation history
 * and sent as context with the next request.
 */
public class AIQueryDialog extends JDialog {

    private static final long serialVersionUID = 1L;

    private static final String SETTING_PROVIDER        = "aiProviderType";
    private static final String SETTING_API_URL         = "aiApiUrl";
    private static final String SETTING_MODEL           = "aiModel";
    private static final String SETTING_API_KEY_PREFIX  = "aiApiKey_";

    private final DataModel dataModel;
    private final String dbmsName;
    private final Consumer<String> sqlConsumer;

    private final List<ConversationMessage> conversationHistory = new ArrayList<>();

    private JTextArea historyArea;
    private JScrollPane historyScrollPane;
    private JTextArea questionArea;
    private RSyntaxTextAreaWithSQLSyntaxStyle sqlArea;
    private JButton generateButton;
    private JButton insertButton;
    private JButton newConversationButton;
    private JLabel statusLabel;
    private JComboBox<ProviderType> providerCombo;
    private JTextField urlField;
    private JTextField modelField;
    private JPasswordField apiKeyField;
    private JCheckBox saveBox;

    public AIQueryDialog(Window owner, DataModel dataModel, String dbmsName, Consumer<String> sqlConsumer) {
        super(owner, "Ask AI - Natural Language to SQL", ModalityType.APPLICATION_MODAL);
        this.dataModel = dataModel;
        this.dbmsName = dbmsName;
        this.sqlConsumer = sqlConsumer;
        initUI();
        pack();
        setLocationRelativeTo(owner);
    }

    private void initUI() {
        ((JComponent) getContentPane()).setBorder(BorderFactory.createEmptyBorder(12, 12, 8, 12));
        setLayout(new BorderLayout(8, 8));

        // Conversation history (hidden until first exchange)
        historyArea = new JTextArea();
        historyArea.setEditable(false);
        historyArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 11));
        historyScrollPane = new JScrollPane(historyArea);
        historyScrollPane.setPreferredSize(new Dimension(700, 160));
        historyScrollPane.setBorder(BorderFactory.createTitledBorder("Conversation"));
        historyScrollPane.setVisible(false);

        // Question area
        JPanel questionPanel = new JPanel(new BorderLayout(4, 4));
        questionPanel.add(new JLabel("Describe the query in plain language:"), BorderLayout.NORTH);
        questionArea = new JTextArea(3, 60);
        questionArea.setLineWrap(true);
        questionArea.setWrapStyleWord(true);
        questionPanel.add(new JScrollPane(questionArea), BorderLayout.CENTER);

        generateButton = new JButton("Generate SQL");
        statusLabel = new JLabel(" ");
        generateButton.addActionListener(e -> onGenerate());
        JPanel genRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        genRow.add(generateButton);
        genRow.add(statusLabel);
        questionPanel.add(genRow, BorderLayout.SOUTH);

        // SQL result area
        JPanel resultPanel = new JPanel(new BorderLayout(4, 4));
        resultPanel.add(new JLabel("Generated SQL:"), BorderLayout.NORTH);
        sqlArea = new RSyntaxTextAreaWithSQLSyntaxStyle(false, false);
        sqlArea.setEditable(false);
        sqlArea.setRows(8);
        sqlArea.setColumns(60);
        RTextScrollPane sqlScrollPane = new RTextScrollPane();
        sqlScrollPane.setViewportView(sqlArea);
        resultPanel.add(sqlScrollPane, BorderLayout.CENTER);

        JPanel questionResultPanel = new JPanel(new BorderLayout(4, 8));
        questionResultPanel.add(questionPanel, BorderLayout.NORTH);
        questionResultPanel.add(resultPanel, BorderLayout.CENTER);

        JPanel centerPanel = new JPanel(new BorderLayout(4, 8));
        centerPanel.add(historyScrollPane, BorderLayout.NORTH);
        centerPanel.add(questionResultPanel, BorderLayout.CENTER);

        // Settings section
        JPanel settingsPanel = buildSettingsPanel();
        settingsPanel.setBorder(BorderFactory.createTitledBorder("AI Provider"));

        // Buttons
        newConversationButton = new JButton("New Conversation");
        newConversationButton.setToolTipText("Clear history and start a new conversation");
        newConversationButton.setEnabled(false);
        newConversationButton.addActionListener(e -> clearHistory());

        insertButton = new JButton("Insert into Editor");
        insertButton.setEnabled(false);
        insertButton.addActionListener(e -> {
            String sql = sqlArea.getText().trim();
            if (!sql.isEmpty()) {
                sqlConsumer.accept(sql);
                dispose();
            }
        });
        JButton closeButton = new JButton("Close");
        closeButton.addActionListener(e -> dispose());

        JPanel buttonRow = new JPanel(new FlowLayout(FlowLayout.RIGHT, 6, 4));
        buttonRow.add(newConversationButton);
        buttonRow.add(insertButton);
        buttonRow.add(closeButton);

        JPanel southPanel = new JPanel(new BorderLayout(4, 4));
        southPanel.add(settingsPanel, BorderLayout.CENTER);
        southPanel.add(buttonRow, BorderLayout.SOUTH);

        add(centerPanel, BorderLayout.CENTER);
        add(southPanel, BorderLayout.SOUTH);
    }

    private JPanel buildSettingsPanel() {
        JPanel panel = new JPanel(new GridBagLayout());
        GridBagConstraints lc = new GridBagConstraints();
        lc.anchor = GridBagConstraints.WEST;
        lc.insets = new Insets(2, 4, 2, 4);
        GridBagConstraints fc = new GridBagConstraints();
        fc.fill = GridBagConstraints.HORIZONTAL;
        fc.insets = new Insets(2, 0, 2, 8);

        ProviderType savedProvider = loadProviderType();
        String savedUrl   = (String) UISettings.restore(SETTING_API_URL);
        String savedModel = (String) UISettings.restore(SETTING_MODEL);
        String savedKey   = loadApiKey(savedProvider);

        providerCombo = new JComboBox<>(ProviderType.values());
        providerCombo.setSelectedItem(savedProvider);

        urlField   = new JTextField(savedUrl   != null ? savedUrl   : savedProvider.defaultApiUrl, 36);
        modelField = new JTextField(savedModel != null ? savedModel : savedProvider.defaultModel,   18);
        apiKeyField = new JPasswordField(36);
        if (savedKey != null) {
            apiKeyField.setText(savedKey);
        }
        saveBox = new JCheckBox("Save", savedKey != null && !savedKey.isEmpty());

        lc.gridx = 0; lc.gridy = 0; panel.add(new JLabel("Provider:"), lc);
        fc.gridx = 1; fc.gridy = 0; panel.add(providerCombo, fc);
        lc.gridx = 2; lc.gridy = 0; panel.add(new JLabel("URL:"), lc);
        fc.gridx = 3; fc.gridy = 0; fc.weightx = 1.0; panel.add(urlField, fc); fc.weightx = 0;
        lc.gridx = 4; lc.gridy = 0; panel.add(new JLabel("Model:"), lc);
        fc.gridx = 5; fc.gridy = 0; panel.add(modelField, fc);

        lc.gridx = 0; lc.gridy = 1; panel.add(new JLabel("API Key:"), lc);
        fc.gridx = 1; fc.gridy = 1; fc.gridwidth = 4; fc.weightx = 1.0; panel.add(apiKeyField, fc);
        fc.gridwidth = 1; fc.weightx = 0;
        fc.gridx = 5; fc.gridy = 1; panel.add(saveBox, fc);

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

        return panel;
    }

    private void onGenerate() {
        String question = questionArea.getText().trim();
        if (question.isEmpty()) {
            JOptionPane.showMessageDialog(this, "Please describe the query.", "Input Required", JOptionPane.WARNING_MESSAGE);
            return;
        }
        String apiKey = new String(apiKeyField.getPassword()).trim();
        if (apiKey.isEmpty()) {
            JOptionPane.showMessageDialog(this, "Please enter an API key.", "API Key Required", JOptionPane.WARNING_MESSAGE);
            apiKeyField.requestFocusInWindow();
            return;
        }

        AIProviderConfig config = new AIProviderConfig(
            (ProviderType) providerCombo.getSelectedItem(),
            urlField.getText().trim(),
            apiKey,
            modelField.getText().trim()
        );

        if (saveBox.isSelected()) {
            UISettings.store(SETTING_PROVIDER, config.providerType.name());
            UISettings.store(SETTING_API_URL,  config.apiUrl);
            UISettings.store(SETTING_MODEL,    config.model);
            UISettings.store(SETTING_API_KEY_PREFIX + config.providerType.name(), config.apiKey);
        }

        generateButton.setEnabled(false);
        insertButton.setEnabled(false);
        sqlArea.setText("");
        statusLabel.setText("Generating...");

        List<ConversationMessage> historySnapshot = new ArrayList<>(conversationHistory);

        new SwingWorker<String, Void>() {
            @Override
            protected String doInBackground() throws Exception {
                return AIQueryAssistant.generateSQL(question, historySnapshot, dataModel, dbmsName, config);
            }

            @Override
            protected void done() {
                generateButton.setEnabled(true);
                statusLabel.setText(" ");
                try {
                    String sql = get();
                    sqlArea.setText(sql);
                    sqlArea.setCaretPosition(0);
                    insertButton.setEnabled(!sql.isEmpty());
                    if (!sql.isEmpty()) {
                        conversationHistory.add(new ConversationMessage("user", question));
                        conversationHistory.add(new ConversationMessage("assistant", sql));
                        questionArea.setText("");
                        updateHistoryDisplay();
                    }
                } catch (ExecutionException ex) {
                    String msg = ex.getCause() != null ? ex.getCause().getMessage() : ex.getMessage();
                    sqlArea.setText("Error: " + msg);
                } catch (InterruptedException ex) {
                    Thread.currentThread().interrupt();
                    sqlArea.setText("Request interrupted.");
                }
            }
        }.execute();
    }

    private void updateHistoryDisplay() {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i + 1 < conversationHistory.size(); i += 2) {
            if (i > 0) {
                sb.append("\n");
            }
            sb.append("You: ").append(conversationHistory.get(i).content).append("\n");
            sb.append("----\n");
            sb.append(conversationHistory.get(i + 1).content).append("\n");
        }
        historyArea.setText(sb.toString());
        historyArea.setCaretPosition(historyArea.getDocument().getLength());

        if (!historyScrollPane.isVisible()) {
            historyScrollPane.setVisible(true);
            newConversationButton.setEnabled(true);
            pack();
        }
    }

    private void clearHistory() {
        conversationHistory.clear();
        historyArea.setText("");
        historyScrollPane.setVisible(false);
        newConversationButton.setEnabled(false);
        sqlArea.setText("");
        insertButton.setEnabled(false);
        pack();
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
