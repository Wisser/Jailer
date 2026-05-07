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
import java.awt.Window;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.function.Consumer;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingWorker;

import org.fife.ui.rtextarea.RTextScrollPane;

import net.sf.jailer.ui.syntaxtextarea.RSyntaxTextAreaWithSQLSyntaxStyle;

import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.ai.AIProviderConfig;
import net.sf.jailer.ui.ai.AIProviderPanel;
import net.sf.jailer.ui.ai.AIQueryAssistant;
import net.sf.jailer.ui.ai.ConversationMessage;
import net.sf.jailer.ui.ai.SystemPromptPanel;

/**
 * Modal dialog that lets the user have a multi-turn conversation with an AI
 * to generate SQL queries. Each exchange is added to the conversation history
 * and sent as context with the next request.
 */
public class AIQueryDialog extends JDialog {

    private static final long serialVersionUID = 1L;

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
    private AIProviderPanel providerPanel;
    private SystemPromptPanel systemPromptPanel;

    public AIQueryDialog(Window owner, DataModel dataModel, String dbmsName, Consumer<String> sqlConsumer) {
        super(owner, "AI Assistant - Natural Language to SQL", ModalityType.APPLICATION_MODAL);
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
        questionArea = new JTextArea(6, 60);
        questionArea.setLineWrap(true);
        questionArea.setWrapStyleWord(true);
        questionPanel.add(new JScrollPane(questionArea), BorderLayout.CENTER);

        generateButton = new JButton("Generate SQL");
        ImageIcon aiIcon = UIUtil.scaleIcon(generateButton, UIUtil.readImage("/ask_ai.png"));
        if (aiIcon != null) {
            generateButton.setIcon(UIUtil.scaleIcon(generateButton, aiIcon));
        }
        generateButton.setEnabled(false);
        statusLabel = new JLabel(" ");
        generateButton.addActionListener(e -> onGenerate());
        JPanel genRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        genRow.add(generateButton);
        genRow.add(statusLabel);
        questionPanel.add(genRow, BorderLayout.SOUTH);

        questionArea.getDocument().addDocumentListener(new javax.swing.event.DocumentListener() {
            @Override
            public void insertUpdate(javax.swing.event.DocumentEvent e) {
                updateGenerateButton();
            }
            @Override
            public void removeUpdate(javax.swing.event.DocumentEvent e) {
                updateGenerateButton();
            }
            @Override
            public void changedUpdate(javax.swing.event.DocumentEvent e) {
                updateGenerateButton();
            }
            private void updateGenerateButton() {
                generateButton.setEnabled(!questionArea.getText().trim().isEmpty());
            }
        });

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

        systemPromptPanel = new SystemPromptPanel();

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
                String comment = buildCommentForHistory();
                String combined = comment + "\n" + sql;
                sqlConsumer.accept(combined);
                dispose();
            }
        });
        JButton closeButton = new JButton("Close");
        closeButton.addActionListener(e -> dispose());

        JButton systemPromptButton = new JButton("System Prompt...");
        systemPromptButton.addActionListener(e -> openSystemPromptDialog());

        JPanel leftButtons = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 4));
        leftButtons.add(systemPromptButton);

        JPanel rightButtons = new JPanel(new FlowLayout(FlowLayout.RIGHT, 6, 4));
        rightButtons.add(newConversationButton);
        rightButtons.add(insertButton);
        rightButtons.add(closeButton);

        JPanel buttonRow = new JPanel(new BorderLayout());
        buttonRow.add(leftButtons, BorderLayout.WEST);
        buttonRow.add(rightButtons, BorderLayout.EAST);

        JPanel southPanel = new JPanel(new BorderLayout(4, 4));
        southPanel.add(settingsPanel, BorderLayout.CENTER);
        southPanel.add(buttonRow, BorderLayout.SOUTH);

        add(centerPanel, BorderLayout.CENTER);
        add(southPanel, BorderLayout.SOUTH);
    }

    private JPanel buildSettingsPanel() {
        providerPanel = new AIProviderPanel();
        return providerPanel;
    }

    private void openSystemPromptDialog() {
        JDialog d = new JDialog(this, "System Prompt", true);
        d.getContentPane().add(systemPromptPanel, BorderLayout.CENTER);

        JButton okButton = new JButton("OK");
        okButton.addActionListener(e -> {
            systemPromptPanel.saveSettings();
            d.dispose();
        });
        JPanel bottom = new JPanel(new FlowLayout(FlowLayout.RIGHT, 6, 4));
        bottom.add(okButton);
        d.getContentPane().add(bottom, BorderLayout.SOUTH);

        d.pack();
        d.setSize(d.getWidth() + 120, d.getHeight() + 100);
        d.setLocationRelativeTo(this);
        d.setVisible(true);
    }

    private void onGenerate() {
        String question = questionArea.getText().trim();
        if (question.isEmpty()) {
            JOptionPane.showMessageDialog(this, "Please describe the query.", "Input Required", JOptionPane.WARNING_MESSAGE);
            return;
        }
        String apiKey = providerPanel.getApiKey();
        if (apiKey.isEmpty()) {
            JOptionPane.showMessageDialog(this, "Please enter an API key.", "API Key Required", JOptionPane.WARNING_MESSAGE);
            providerPanel.getApiKeyComponent().requestFocusInWindow();
            return;
        }

        AIProviderConfig config = providerPanel.getConfig();
        providerPanel.saveSettings();

        generateButton.setEnabled(false);
        insertButton.setEnabled(false);
        sqlArea.setText("");
        statusLabel.setText("Generating...");

        List<ConversationMessage> historySnapshot = new ArrayList<>(conversationHistory);

        new SwingWorker<String, Void>() {
            @Override
            protected String doInBackground() throws Exception {
                return AIQueryAssistant.generateSQL(question, historySnapshot, dataModel, dbmsName, config,
                        systemPromptPanel.getTemplate());
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
                    UIUtil.showException(AIQueryDialog.this, "SQL Generation Error", ex);
                } catch (InterruptedException ex) {
                    Thread.currentThread().interrupt();
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

    private String buildCommentForHistory() {
        // Collect only user messages
        List<String> userMessages = new ArrayList<>();
        for (ConversationMessage msg : conversationHistory) {
            if ("user".equals(msg.role)) {
                // Replace newlines with spaces to keep each message on one line in the comment
                String cleanedContent = msg.content.replaceAll("[\\r\\n]+", " ");
                userMessages.add(cleanedContent);
            }
        }
        if (userMessages.isEmpty()) {
            return "";
        }

        StringBuilder sb = new StringBuilder();
        sb.append("/* AI:\n");
        if (userMessages.size() == 1) {
            sb.append("   " + userMessages.get(0));
        } else {
            for (String msg : userMessages) {
                sb.append("   - ").append(msg).append("\n");
            }
            // Remove trailing newline
            sb.setLength(sb.length() - 1);
        }
        sb.append("\n */");
        return sb.toString();
    }
}
