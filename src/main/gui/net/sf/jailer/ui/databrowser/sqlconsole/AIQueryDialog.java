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
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Insets;
import java.awt.Window;
import java.awt.event.ItemListener;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;

import org.fife.ui.rtextarea.RTextScrollPane;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.ai.AIProviderConfig;
import net.sf.jailer.ui.ai.AIProviderPanel;
import net.sf.jailer.ui.ai.AIQueryAssistant;
import net.sf.jailer.ui.ai.ConversationMessage;
import net.sf.jailer.ui.ai.SystemPromptPanel;
import net.sf.jailer.ui.syntaxtextarea.RSyntaxTextAreaWithSQLSyntaxStyle;

/**
 * Modal dialog that lets the user have a multi-turn conversation with an AI
 * to generate SQL queries. Each exchange is added to the conversation history
 * and sent as context with the next request.
 */
public class AIQueryDialog extends JDialog {

    private static final long serialVersionUID = 1L;
    private static final Logger _log = LoggerFactory.getLogger(AIQueryDialog.class);

    private final DataModel dataModel;
    private final String dbmsName;
    private final Consumer<String> insertAction;
    private final ExecutionContext executionContext;

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
    private JCheckBox smartSelectionBox;
    private JCheckBox omitColumnTypesBox;
    private JLabel contextEstimateLabel;
    private JButton cancelButton;
    private JButton closeButton;
    private JButton systemPromptButton;
    private SwingWorker<String, Void> currentWorker;
    private final AtomicReference<Runnable> abortRef = new AtomicReference<>();

    public AIQueryDialog(Window owner, DataModel dataModel, String dbmsName, Consumer<String> insertAction, ExecutionContext executionContext, String initialPrompt, boolean silent, String initialSql) {
        super(owner, "AI Assistant - Natural Language to SQL", ModalityType.APPLICATION_MODAL);
        this.dataModel = dataModel;
        this.dbmsName = dbmsName;
        this.insertAction = insertAction;
        this.executionContext = executionContext;
        initUI();
        if (initialPrompt != null) {
            String displayPrompt = initialPrompt;
            int nlIdx = displayPrompt.indexOf('\n');
            if (nlIdx >= 0) {
                String rest = displayPrompt.substring(nlIdx + 1);
                int indent = 0;
                while (indent < rest.length() && rest.charAt(indent) == ' ') indent++;
                if (indent > 0) {
                    StringBuilder prefix = new StringBuilder();
                    for (int i = 0; i < indent; i++) prefix.append(' ');
                    displayPrompt = prefix.toString() + displayPrompt;
                }
            }
            questionArea.setText(displayPrompt);
            if (silent) {
                historyArea.setText(initialPrompt);
                if (generateButton.isEnabled()) {
                    UIUtil.invokeLater(() -> onGenerate());
                }
            }
        }
        if (initialSql != null) {
            String sql = initialSql.replaceFirst("(?s)\\A\\s*/\\*.*?\\*/\\s*", "");
            sqlArea.setText(sql);
            sqlArea.setCaretPosition(0);
        }
        getRootPane().setDefaultButton(insertButton);
        UIUtil.initComponents(this);
        pack();
        setSize(getWidth() + 120, getHeight());
        setLocationRelativeTo(owner);
    }

    private void initUI() {
        ((JComponent) getContentPane()).setBorder(BorderFactory.createEmptyBorder(12, 12, 8, 12));
        setLayout(new BorderLayout(8, 8));

        historyArea = new JTextArea();
        historyArea.setEditable(false);
        historyArea.setEnabled(false);
        historyArea.setText("No conversation yet");
        historyArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 11));
        historyScrollPane = new JScrollPane(historyArea);
        historyScrollPane.setPreferredSize(new Dimension(700, 120));
        historyScrollPane.setBorder(BorderFactory.createTitledBorder("Conversation"));

        // Question area
        JPanel questionPanel = new JPanel(new BorderLayout(4, 4));
        questionPanel.add(new JLabel("Describe the query in plain language"), BorderLayout.NORTH);
        questionArea = new JTextArea(8, 60);
        questionArea.setLineWrap(true);
        questionArea.setWrapStyleWord(true);
        KeyStroke ctrlEnter = KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ENTER, java.awt.event.InputEvent.CTRL_DOWN_MASK);
        questionArea.getInputMap(JComponent.WHEN_FOCUSED).put(ctrlEnter, "generateSQL");
        questionArea.getActionMap().put("generateSQL", new AbstractAction() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent e) {
                generateButton.doClick();
            }
        });
        questionPanel.add(new JScrollPane(questionArea), BorderLayout.CENTER);

        generateButton = new JButton("Generate SQL");
        ImageIcon aiIcon = UIUtil.scaleIcon(generateButton, UIUtil.readImage("/ask_ai.png"));
        if (aiIcon != null) {
            generateButton.setIcon(UIUtil.scaleIcon(generateButton, aiIcon));
        }
        generateButton.setEnabled(false);
        generateButton.setToolTipText("Generate SQL (Ctrl+Enter)");
        statusLabel = new JLabel(" ");
        generateButton.addActionListener(e -> onGenerate());

        cancelButton = new JButton("Cancel");
        ImageIcon cancelIcon = UIUtil.readImage("/Cancel.png");
        if (cancelIcon != null) {
            cancelButton.setIcon(UIUtil.scaleIcon(cancelButton, cancelIcon));
        }
        cancelButton.setEnabled(false);
        cancelButton.addActionListener(e -> {
            Runnable abort = abortRef.get();
            if (abort != null) {
                abort.run();
            }
            if (currentWorker != null) {
                currentWorker.cancel(true);
            }
        });

        boolean manyTables = dataModel.getSortedTables().size() > 500;

        smartSelectionBox = new JCheckBox("Relevant tables only (reduces context)");
        smartSelectionBox.setSelected(manyTables);
        smartSelectionBox.setToolTipText("<html>Reduces the AI context size for large schemas.<br>"
            + "A first AI call selects only the tables relevant to your question;<br>"
            + "a second call then generates the SQL using only those tables.<br>"
            + "Adds one extra API call per query.</html>");

        omitColumnTypesBox = new JCheckBox("Omit column types");
        omitColumnTypesBox.setToolTipText("<html>Reduces the AI context size by omitting column type information<br>"
            + "from the schema description sent to the AI.<br>"
            + "Table and column names, primary keys and foreign keys are still included.</html>");

        contextEstimateLabel = new JLabel();
        contextEstimateLabel.setFont(contextEstimateLabel.getFont().deriveFont(
                contextEstimateLabel.getFont().getSize2D() - 1f));
        contextEstimateLabel.setForeground(java.awt.Color.GRAY);

        ItemListener contextUpdater = e -> updateContextEstimate();
        omitColumnTypesBox.addItemListener(contextUpdater);
        smartSelectionBox.addItemListener(contextUpdater);
        updateContextEstimate();

        newConversationButton = new JButton("New Conversation");
        ImageIcon clearIcon = UIUtil.readImage("/clear.png");
        if (clearIcon != null) {
            newConversationButton.setIcon(UIUtil.scaleIcon(newConversationButton, clearIcon));
        }
        newConversationButton.setToolTipText("Clear history and start a new conversation");
        newConversationButton.setEnabled(false);
        newConversationButton.addActionListener(e -> clearHistory());

        insertButton = new JButton("Insert into SQL Console");
        insertButton.setMargin(new Insets(4, 10, 4, 10));
        ImageIcon insertIcon = UIUtil.readImage("/runall.png");
        if (insertIcon != null) {
            insertButton.setIcon(UIUtil.scaleIcon(insertButton, insertIcon));
        }
        insertButton.setEnabled(false);
        insertButton.addActionListener(e -> {
            String sql = sqlArea.getText().trim();
            if (!sql.isEmpty()) {
                AIProviderConfig cfg = providerPanel.getConfig();
                AIQueryAssistant.saveCheckboxStates(cfg, executionContext, omitColumnTypesBox.isSelected(), smartSelectionBox.isSelected());
                String comment = buildCommentForHistory();
                String combined = comment.isEmpty() ? sql : comment + "\n" + sql;
                insertAction.accept(combined);
                dispose();
            }
        });

        JPanel genLeft = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        genLeft.add(generateButton);
        genLeft.add(cancelButton);
        genLeft.add(newConversationButton);
        genLeft.add(statusLabel);
        JPanel checkboxRow = new JPanel(new FlowLayout(FlowLayout.RIGHT, 10, 0));
        checkboxRow.add(omitColumnTypesBox);
        checkboxRow.add(smartSelectionBox);
        JPanel estimateRow = new JPanel(new FlowLayout(FlowLayout.RIGHT, 10, 2));
        estimateRow.add(contextEstimateLabel);
        JPanel genRight = new JPanel(new BorderLayout());
        genRight.add(checkboxRow, BorderLayout.NORTH);
        genRight.add(estimateRow, BorderLayout.SOUTH);
        JPanel genRow = new JPanel(new BorderLayout());
        genRow.add(genLeft, BorderLayout.WEST);
        genRow.add(genRight, BorderLayout.EAST);
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
        resultPanel.add(new JLabel("Generated SQL"), BorderLayout.NORTH);
        sqlArea = new RSyntaxTextAreaWithSQLSyntaxStyle(false, false);
        sqlArea.setRows(5);
        sqlArea.setColumns(60);
        RTextScrollPane sqlScrollPane = new RTextScrollPane();
        sqlScrollPane.setViewportView(sqlArea);
        resultPanel.add(sqlScrollPane, BorderLayout.CENTER);
        JPanel insertRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 2));
        insertRow.add(insertButton);
        resultPanel.add(insertRow, BorderLayout.SOUTH);

        JPanel questionResultPanel = new JPanel(new BorderLayout(4, 8));
        questionResultPanel.add(questionPanel, BorderLayout.NORTH);
        questionResultPanel.add(resultPanel, BorderLayout.CENTER);

        JPanel centerPanel = new JPanel(new BorderLayout(4, 8));
        centerPanel.add(historyScrollPane, BorderLayout.NORTH);
        centerPanel.add(questionResultPanel, BorderLayout.CENTER);

        // Settings section
        JPanel settingsPanel = buildSettingsPanel();
        settingsPanel.setBorder(BorderFactory.createEmptyBorder(8, 0, 2, 0));
        AIProviderConfig initialConfig = providerPanel.getConfig();
        omitColumnTypesBox.setSelected(AIQueryAssistant.loadOmitColumnTypes(initialConfig, executionContext));
        smartSelectionBox.setSelected(AIQueryAssistant.loadSmartSelection(initialConfig, executionContext, dataModel.getSortedTables().size()));

        systemPromptPanel = new SystemPromptPanel();

        // Buttons
        closeButton = new JButton("Close");
        ImageIcon closeIcon = UIUtil.readImage("/buttoncancel.png");
        if (closeIcon != null) {
            closeButton.setIcon(UIUtil.scaleIcon(closeButton, closeIcon));
        }
        closeButton.addActionListener(e -> dispose());

        systemPromptButton = new JButton("System Prompt...");
        ImageIcon editIcon = UIUtil.readImage("/ieditdetails_64.png");
        if (editIcon != null) {
            systemPromptButton.setIcon(UIUtil.scaleIcon(systemPromptButton, editIcon));
        }
        systemPromptButton.addActionListener(e -> openSystemPromptDialog());

        JPanel leftButtons = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 4));
        leftButtons.add(systemPromptButton);

        JPanel rightButtons = new JPanel(new FlowLayout(FlowLayout.RIGHT, 6, 4));
        rightButtons.add(closeButton);

        JPanel buttonRow = new JPanel(new BorderLayout());
        buttonRow.add(leftButtons, BorderLayout.WEST);
        buttonRow.add(rightButtons, BorderLayout.EAST);

        JPanel southPanel = new JPanel(new BorderLayout(0, 4));
        southPanel.add(settingsPanel, BorderLayout.CENTER);
        southPanel.add(buttonRow, BorderLayout.SOUTH);

        add(centerPanel, BorderLayout.CENTER);
        add(southPanel, BorderLayout.SOUTH);
    }

    private JPanel buildSettingsPanel() {
        providerPanel = new AIProviderPanel();
        return providerPanel;
    }

    private void updateContextEstimate() {
        boolean omit  = omitColumnTypesBox.isSelected();
        boolean smart = smartSelectionBox.isSelected();
        String schema = AIQueryAssistant.buildSchemaDescription(dataModel, null, omit);
        int totalTokens = schema.length() / 4;
        String text;
        if (smart) {
            int tableCount = dataModel.getSortedTables().size();
            int perTable = tableCount > 0 ? totalTokens / tableCount : 0;
            text = String.format("Estimated context size: ~%,d tokens per relevant table", perTable);
        } else {
            text = String.format("Estimated context size: ~%,d tokens", totalTokens);
        }
        contextEstimateLabel.setText(text);
    }

    private void openSystemPromptDialog() {
        JDialog d = new JDialog(this, "System Prompt", true);
        d.getContentPane().add(systemPromptPanel, BorderLayout.CENTER);

        JButton okButton = new JButton("OK");
        ImageIcon okIcon = UIUtil.readImage("/buttonok.png");
        if (okIcon != null) {
            okButton.setIcon(UIUtil.scaleIcon(okButton, okIcon));
        }
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

    private void setGenerating(boolean generating) {
        generateButton.setEnabled(!generating);
        insertButton.setEnabled(!generating);
        newConversationButton.setEnabled(!generating && !conversationHistory.isEmpty());
        questionArea.setEnabled(!generating);
        providerPanel.setEnabled(!generating);
        smartSelectionBox.setEnabled(!generating);
        omitColumnTypesBox.setEnabled(!generating);
        systemPromptButton.setEnabled(!generating);
        cancelButton.setEnabled(generating);
        setCursor(generating ? Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR) : Cursor.getDefaultCursor());
    }

    private void onGenerate() {
        String question = questionArea.getText().trim();
        if (question.isEmpty()) {
            JOptionPane.showMessageDialog(this, "Please describe the query.", "Input Required", JOptionPane.WARNING_MESSAGE);
            return;
        }
        if (question.startsWith("!echo ")) {
            String sql = question.substring("!echo ".length());
            sqlArea.setText(sql);
            sqlArea.setCaretPosition(0);
            insertButton.setEnabled(!sql.isEmpty());
            conversationHistory.add(new ConversationMessage("user", question));
            conversationHistory.add(new ConversationMessage("assistant", sql));
            questionArea.setText("");
            updateHistoryDisplay();
            return;
        }
        AIProviderConfig config = providerPanel.getConfig();
        String apiKey = providerPanel.getApiKey();
        if (apiKey.isEmpty() && config.providerType.requiresApiKey) {
            JOptionPane.showMessageDialog(this, "Please enter an API key.", "API Key Required", JOptionPane.WARNING_MESSAGE);
            providerPanel.getApiKeyComponent().requestFocusInWindow();
            return;
        }
        setGenerating(true);
        sqlArea.setText("");
        statusLabel.setText("Generating...");

        List<ConversationMessage> historySnapshot = new ArrayList<>(conversationHistory);
        boolean smartSelection = smartSelectionBox.isSelected();
        boolean omitColumnTypes = omitColumnTypesBox.isSelected();

        currentWorker = new SwingWorker<String, Void>() {
            @Override
            protected String doInBackground() throws Exception {
                return AIQueryAssistant.generateSQL(question, historySnapshot, dataModel, dbmsName, config,
                        systemPromptPanel.getTemplate(), smartSelection, omitColumnTypes, abortRef, () -> {
                    boolean[] result = { false };
                    try {
                        SwingUtilities.invokeAndWait(() -> {
                            int choice = JOptionPane.showConfirmDialog(
                                AIQueryDialog.this,
                                "<html>No relevant tables could be identified for your question.<br>"
                                + "(It may help to rephrase your question using table names and be more specific.)<br><br>"
                                + "Proceed with the full schema?</html>",
                                "No Relevant Tables Found",
                                JOptionPane.YES_NO_OPTION,
                                JOptionPane.WARNING_MESSAGE);
                            result[0] = choice == JOptionPane.YES_OPTION;
                        });
                    } catch (InterruptedException ex) {
                        Thread.currentThread().interrupt();
                    } catch (java.lang.reflect.InvocationTargetException ex) {
                        _log.warn("Confirmation dialog failed", ex);
                    }
                    return result[0];
                }, systemPromptPanel.getFirstPassTemplate());
            }

            @Override
            protected void done() {
                currentWorker = null;
                setGenerating(false);
                statusLabel.setText(" ");
                if (isCancelled()) {
                    return;
                }
                try {
                    String sql = get();
                    sqlArea.setText(sql);
                    sqlArea.setCaretPosition(0);
                    insertButton.setEnabled(!sql.isEmpty());
                    if (!sql.isEmpty()) {
                        providerPanel.markConnectionVerified();
                        AIQueryAssistant.saveCheckboxStates(config, executionContext, omitColumnTypes, smartSelection);
                        conversationHistory.add(new ConversationMessage("user", question));
                        conversationHistory.add(new ConversationMessage("assistant", sql));
                        questionArea.setText("");
                        updateHistoryDisplay();
                        insertButton.requestFocusInWindow();
                    }
                } catch (ExecutionException ex) {
                    providerPanel.markConnectionFailed();
                    UIUtil.showException(AIQueryDialog.this, "SQL Generation Error", ex);
                } catch (InterruptedException ex) {
                    Thread.currentThread().interrupt();
                }
            }
        };
        currentWorker.execute();
    }

    private void updateHistoryDisplay() {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i + 1 < conversationHistory.size(); i += 2) {
            if (i > 0) {
                sb.append("\n");
            }
            sb.append("You:\n").append(conversationHistory.get(i).content).append("\n");
            sb.append("----\n");
            sb.append(conversationHistory.get(i + 1).content).append("\n");
        }
        historyArea.setEnabled(true);
        historyArea.setText(sb.toString());
        historyArea.setCaretPosition(historyArea.getDocument().getLength());
        newConversationButton.setEnabled(true);
    }

    private void clearHistory() {
        conversationHistory.clear();
        historyArea.setEnabled(false);
        historyArea.setText("No conversation yet");
        newConversationButton.setEnabled(false);
        sqlArea.setText("");
        insertButton.setEnabled(false);
    }

    private String buildCommentForHistory() {
        return AIQueryAssistant.buildPromptComment(conversationHistory);
    }

}

