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
import java.util.function.Consumer;
import java.util.concurrent.atomic.AtomicReference;

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
import net.sf.jailer.ui.util.UISettings;

/**
 * Modal dialog that lets the user have a multi-turn conversation with an AI
 * to generate SQL queries.
 */
public class AIQueryDialog extends javax.swing.JDialog {

    private static final long serialVersionUID = 1L;
    private static final Logger _log = LoggerFactory.getLogger(AIQueryDialog.class);

    private final DataModel dataModel;
    private final String dbmsName;
    private final Consumer<String> insertAction;
    private final ExecutionContext executionContext;

    private final List<ConversationMessage> conversationHistory = new ArrayList<>();
    private SwingWorker<String, Void> currentWorker;
    private final AtomicReference<Runnable> abortRef = new AtomicReference<>();

    public AIQueryDialog(Window owner, DataModel dataModel, String dbmsName, Consumer<String> insertAction, ExecutionContext executionContext, String initialPrompt, boolean silent, String initialSql) {
        super(owner, "AI Assistant - Natural Language to SQL", ModalityType.APPLICATION_MODAL);
        this.dataModel = dataModel;
        this.dbmsName = dbmsName;
        this.insertAction = insertAction;
        this.executionContext = executionContext;

        initComponents();
        UIUtil.initComponents(this);

        // Icons
        ImageIcon aiIcon = UIUtil.scaleIcon(generateButton, UIUtil.readImage("/ask_ai.png"));
        if (aiIcon != null) {
            generateButton.setIcon(aiIcon);
        }
        ImageIcon cancelIcon = UIUtil.readImage("/Cancel.png");
        if (cancelIcon != null) {
            cancelButton.setIcon(UIUtil.scaleIcon(cancelButton, cancelIcon));
        }
        ImageIcon clearIcon = UIUtil.readImage("/clear.png");
        if (clearIcon != null) {
            newConversationButton.setIcon(UIUtil.scaleIcon(newConversationButton, clearIcon));
        }
        ImageIcon insertIcon = UIUtil.readImage("/runall.png");
        if (insertIcon != null) {
            insertButton.setIcon(UIUtil.scaleIcon(insertButton, insertIcon));
        }
        ImageIcon closeIcon = UIUtil.readImage("/buttoncancel.png");
        if (closeIcon != null) {
            closeButton.setIcon(UIUtil.scaleIcon(closeButton, closeIcon));
        }
        ImageIcon editIcon = UIUtil.readImage("/ieditdetails_64.png");
        if (editIcon != null) {
            systemPromptButton.setIcon(UIUtil.scaleIcon(systemPromptButton, editIcon));
        }

        // Tooltip text (too long for form)
        generateButton.setToolTipText("Generate SQL (Ctrl+Enter)");
        newConversationButton.setToolTipText("Clear history and start a new conversation");
        smartSelectionBox.setToolTipText("<html>Reduces the AI context size for large schemas.<br>"
            + "A first AI call selects only the tables relevant to your question;<br>"
            + "a second call then generates the SQL using only those tables.<br>"
            + "Adds one extra API call per query.</html>");
        omitColumnTypesBox.setToolTipText("<html>Reduces the AI context size by omitting column type information<br>"
            + "from the schema description sent to the AI.<br>"
            + "Table and column names, primary keys and foreign keys are still included.</html>");

        // Insert button margin
        insertButton.setMargin(new Insets(4, 10, 4, 10));

        // Smart-selection default based on schema size
        boolean manyTables = dataModel.getSortedTables().size() > 500;
        smartSelectionBox.setSelected(manyTables);

        // Context estimate label styling
        contextEstimateLabel.setFont(contextEstimateLabel.getFont().deriveFont(
                contextEstimateLabel.getFont().getSize2D() - 1f));
        contextEstimateLabel.setForeground(java.awt.Color.GRAY);

        // Checkbox item listeners
        ItemListener contextUpdater = e -> updateContextEstimate();
        omitColumnTypesBox.addItemListener(contextUpdater);
        smartSelectionBox.addItemListener(contextUpdater);
        updateContextEstimate();

        // Document listener to enable/disable Generate button
        questionArea.getDocument().addDocumentListener(new javax.swing.event.DocumentListener() {
            @Override
            public void insertUpdate(javax.swing.event.DocumentEvent e) { updateGenerateButton(); }
            @Override
            public void removeUpdate(javax.swing.event.DocumentEvent e) { updateGenerateButton(); }
            @Override
            public void changedUpdate(javax.swing.event.DocumentEvent e) { updateGenerateButton(); }
            private void updateGenerateButton() {
                generateButton.setEnabled(!questionArea.getText().trim().isEmpty());
            }
        });

        // Ctrl+Enter fires Generate
        KeyStroke ctrlEnter = KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ENTER, java.awt.event.InputEvent.CTRL_DOWN_MASK);
        questionArea.getInputMap(JComponent.WHEN_FOCUSED).put(ctrlEnter, "generateSQL");
        questionArea.getActionMap().put("generateSQL", new AbstractAction() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent e) {
                generateButton.doClick();
            }
        });

        restoreCheckboxStates(providerPanel.getConfig());

        if (initialPrompt != null) {
            questionArea.setText(initialPrompt);
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
        setSize(getWidth() + 120, getHeight() + 40);
        setLocationRelativeTo(owner);
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        centerPanel = new javax.swing.JPanel();
        historyScrollPane = new javax.swing.JScrollPane();
        historyArea = new javax.swing.JTextArea();
        questionResultPanel = new javax.swing.JPanel();
        questionPanel = new javax.swing.JPanel();
        questionLabel = new javax.swing.JLabel();
        questionScrollPane = new javax.swing.JScrollPane();
        questionArea = new javax.swing.JTextArea();
        genRowPanel = new javax.swing.JPanel();
        genLeftPanel = new javax.swing.JPanel();
        generateButton = new javax.swing.JButton();
        cancelButton = new javax.swing.JButton();
        newConversationButton = new javax.swing.JButton();
        statusLabel = new javax.swing.JLabel();
        genRightPanel = new javax.swing.JPanel();
        checkboxRowPanel = new javax.swing.JPanel();
        omitColumnTypesBox = new javax.swing.JCheckBox();
        smartSelectionBox = new javax.swing.JCheckBox();
        estimateRowPanel = new javax.swing.JPanel();
        contextEstimateLabel = new javax.swing.JLabel();
        resultPanel = new javax.swing.JPanel();
        sqlLabel = new javax.swing.JLabel();
        sqlScrollPane = new org.fife.ui.rtextarea.RTextScrollPane();
        sqlArea = new net.sf.jailer.ui.syntaxtextarea.RSyntaxTextAreaWithSQLSyntaxStyle(false, false);
        insertRowPanel = new javax.swing.JPanel();
        insertButton = new javax.swing.JButton();
        southPanel = new javax.swing.JPanel();
        providerPanel = new net.sf.jailer.ui.ai.AIProviderPanel();
        buttonRowPanel = new javax.swing.JPanel();
        leftButtonsPanel = new javax.swing.JPanel();
        systemPromptButton = new javax.swing.JButton();
        rightButtonsPanel = new javax.swing.JPanel();
        closeButton = new javax.swing.JButton();
        systemPromptPanel = new net.sf.jailer.ui.ai.SystemPromptPanel();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("AI Assistant - Natural Language to SQL");
        ((javax.swing.JComponent) getContentPane()).setBorder(javax.swing.BorderFactory.createEmptyBorder(12, 12, 8, 12));
        getContentPane().setLayout(new java.awt.GridBagLayout());

        // ----- historyArea -----
        historyArea.setEditable(false);
        historyArea.setEnabled(false);
        historyArea.setFont(new java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 11));
        historyArea.setText("No conversation yet");
        historyScrollPane.setBorder(javax.swing.BorderFactory.createTitledBorder("Conversation"));
        historyScrollPane.setPreferredSize(new java.awt.Dimension(700, 120));
        historyScrollPane.setViewportView(historyArea);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 8, 0);
        centerPanel.add(historyScrollPane, gridBagConstraints);

        // ----- questionPanel -----
        questionLabel.setText("Describe the query in plain language");
        questionArea.setColumns(60);
        questionArea.setLineWrap(true);
        questionArea.setRows(8);
        questionArea.setWrapStyleWord(true);
        questionScrollPane.setViewportView(questionArea);

        questionPanel.setLayout(new java.awt.GridBagLayout());

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        questionPanel.add(questionLabel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        questionPanel.add(questionScrollPane, gridBagConstraints);

        // ----- genRow -----
        generateButton.setText("Generate SQL");
        generateButton.setEnabled(false);
        generateButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                generateButtonActionPerformed(evt);
            }
        });
        cancelButton.setText("Cancel");
        cancelButton.setEnabled(false);
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });
        newConversationButton.setText("New Conversation");
        newConversationButton.setEnabled(false);
        newConversationButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                newConversationButtonActionPerformed(evt);
            }
        });
        statusLabel.setText(" ");

        genLeftPanel.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.LEFT, 6, 0));
        genLeftPanel.add(generateButton);
        genLeftPanel.add(cancelButton);
        genLeftPanel.add(newConversationButton);
        genLeftPanel.add(statusLabel);

        omitColumnTypesBox.setText("Omit column types");
        smartSelectionBox.setText("Relevant tables only (reduces context)");
        contextEstimateLabel.setText(" ");

        checkboxRowPanel.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.RIGHT, 10, 0));
        checkboxRowPanel.add(omitColumnTypesBox);
        checkboxRowPanel.add(smartSelectionBox);

        estimateRowPanel.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.RIGHT, 10, 2));
        estimateRowPanel.add(contextEstimateLabel);

        genRightPanel.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        genRightPanel.add(checkboxRowPanel, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        genRightPanel.add(estimateRowPanel, gridBagConstraints);

        genRowPanel.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        genRowPanel.add(genLeftPanel, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        genRowPanel.add(genRightPanel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        questionPanel.add(genRowPanel, gridBagConstraints);

        // ----- resultPanel -----
        sqlLabel.setText("Generated SQL");
        sqlArea.setEditable(false);
        sqlArea.setColumns(60);
        sqlArea.setRows(8);
        sqlScrollPane.setViewportView(sqlArea);

        insertButton.setText("Insert into SQL Console");
        insertButton.setEnabled(false);
        insertButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                insertButtonActionPerformed(evt);
            }
        });

        insertRowPanel.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.LEFT, 6, 2));
        insertRowPanel.add(insertButton);

        resultPanel.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 4, 0);
        resultPanel.add(sqlLabel, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        resultPanel.add(sqlScrollPane, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        resultPanel.add(insertRowPanel, gridBagConstraints);

        // ----- questionResultPanel -----
        questionResultPanel.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 8, 0);
        questionResultPanel.add(questionPanel, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        questionResultPanel.add(resultPanel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        centerPanel.add(questionResultPanel, gridBagConstraints);

        // ----- centerPanel -> content pane -----
        centerPanel.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        getContentPane().add(centerPanel, gridBagConstraints);

        // ----- southPanel -----
        systemPromptButton.setText("System Prompt...");
        systemPromptButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                systemPromptButtonActionPerformed(evt);
            }
        });
        closeButton.setText("Close");
        closeButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                closeButtonActionPerformed(evt);
            }
        });

        leftButtonsPanel.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.LEFT, 6, 4));
        leftButtonsPanel.add(systemPromptButton);

        rightButtonsPanel.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.RIGHT, 6, 4));
        rightButtonsPanel.add(closeButton);

        buttonRowPanel.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        buttonRowPanel.add(leftButtonsPanel, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        buttonRowPanel.add(rightButtonsPanel, gridBagConstraints);

        southPanel.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 0, 2, 0));
        southPanel.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        southPanel.add(providerPanel, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        southPanel.add(buttonRowPanel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        getContentPane().add(southPanel, gridBagConstraints);

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void generateButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_generateButtonActionPerformed
        onGenerate();
    }//GEN-LAST:event_generateButtonActionPerformed

    private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
        Runnable abort = abortRef.get();
        if (abort != null) {
            abort.run();
        }
        if (currentWorker != null) {
            currentWorker.cancel(true);
        }
    }//GEN-LAST:event_cancelButtonActionPerformed

    private void newConversationButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_newConversationButtonActionPerformed
        clearHistory();
    }//GEN-LAST:event_newConversationButtonActionPerformed

    private void insertButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_insertButtonActionPerformed
        String sql = sqlArea.getText().trim();
        if (!sql.isEmpty()) {
            saveCheckboxStates(providerPanel.getConfig());
            String comment = buildCommentForHistory();
            String combined = comment.isEmpty() ? sql : comment + "\n" + sql;
            insertAction.accept(combined);
            dispose();
        }
    }//GEN-LAST:event_insertButtonActionPerformed

    private void systemPromptButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_systemPromptButtonActionPerformed
        openSystemPromptDialog();
    }//GEN-LAST:event_systemPromptButtonActionPerformed

    private void closeButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_closeButtonActionPerformed
        dispose();
    }//GEN-LAST:event_closeButtonActionPerformed

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JPanel buttonRowPanel;
    private javax.swing.JButton cancelButton;
    private javax.swing.JPanel centerPanel;
    private javax.swing.JPanel checkboxRowPanel;
    private javax.swing.JButton closeButton;
    private javax.swing.JLabel contextEstimateLabel;
    private javax.swing.JPanel estimateRowPanel;
    private javax.swing.JPanel genLeftPanel;
    private javax.swing.JPanel genRightPanel;
    private javax.swing.JPanel genRowPanel;
    private javax.swing.JButton generateButton;
    private javax.swing.JTextArea historyArea;
    private javax.swing.JScrollPane historyScrollPane;
    private javax.swing.JButton insertButton;
    private javax.swing.JPanel insertRowPanel;
    private javax.swing.JPanel leftButtonsPanel;
    private javax.swing.JButton newConversationButton;
    private javax.swing.JCheckBox omitColumnTypesBox;
    private net.sf.jailer.ui.ai.AIProviderPanel providerPanel;
    private javax.swing.JLabel questionLabel;
    private javax.swing.JPanel questionPanel;
    private javax.swing.JTextArea questionArea;
    private javax.swing.JScrollPane questionScrollPane;
    private javax.swing.JPanel questionResultPanel;
    private javax.swing.JPanel resultPanel;
    private javax.swing.JPanel rightButtonsPanel;
    private javax.swing.JCheckBox smartSelectionBox;
    private javax.swing.JLabel sqlLabel;
    private org.fife.ui.rtextarea.RTextScrollPane sqlScrollPane;
    private javax.swing.JLabel statusLabel;
    private javax.swing.JPanel southPanel;
    private net.sf.jailer.ui.ai.SystemPromptPanel systemPromptPanel;
    private javax.swing.JButton systemPromptButton;
    // End of variables declaration//GEN-END:variables

    private net.sf.jailer.ui.syntaxtextarea.RSyntaxTextAreaWithSQLSyntaxStyle sqlArea;

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
        if (question.startsWith("echo ")) {
            String sql = question.substring("echo ".length());
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
        if (apiKey.isEmpty() && config.providerType != AIProviderConfig.ProviderType.OLLAMA) {
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
                });
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
                        saveCheckboxStates(config);
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

    private void openSystemPromptDialog() {
        JDialog d = new JDialog(this, "System Prompt", true);
        d.getContentPane().add(systemPromptPanel, BorderLayout.CENTER);

        JButton okButton = new JButton("OK");
        okButton.addActionListener(e -> d.dispose());
        JPanel bottom = new JPanel(new FlowLayout(FlowLayout.RIGHT, 6, 4));
        bottom.add(okButton);
        d.getContentPane().add(bottom, BorderLayout.SOUTH);

        d.pack();
        d.setSize(d.getWidth() + 120, d.getHeight() + 100);
        d.setLocationRelativeTo(this);
        d.setVisible(true);
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

    private String buildCommentForHistory() {
        List<String> userMessages = new ArrayList<>();
        for (ConversationMessage msg : conversationHistory) {
            if ("user".equals(msg.role)) {
                userMessages.add(msg.content.replaceAll("[\\r\\n]+", " "));
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
            sb.setLength(sb.length() - 1);
        }
        sb.append("\n */");
        return sb.toString();
    }

    private String checkboxSettingsKey(AIProviderConfig config) {
        String folder = executionContext != null ? executionContext.getQualifiedDatamodelFolder() : "";
        return config.apiUrl + "|" + config.model + "|" + (folder != null ? folder : "");
    }

    private void saveCheckboxStates(AIProviderConfig config) {
        String key = checkboxSettingsKey(config);
        UISettings.store("aiOmitTypes_" + key, omitColumnTypesBox.isSelected());
        UISettings.store("aiSmartSelection_" + key, smartSelectionBox.isSelected());
    }

    private void restoreCheckboxStates(AIProviderConfig config) {
        String key = checkboxSettingsKey(config);
        Object omit  = UISettings.restore("aiOmitTypes_" + key);
        Object smart = UISettings.restore("aiSmartSelection_" + key);
        if (omit  instanceof Boolean) omitColumnTypesBox.setSelected((Boolean) omit);
        if (smart instanceof Boolean) smartSelectionBox.setSelected((Boolean) smart);
    }
}
