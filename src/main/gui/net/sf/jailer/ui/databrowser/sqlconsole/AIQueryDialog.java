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
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
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
import javax.swing.JLayeredPane;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.KeyStroke;
import javax.swing.SwingConstants;
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
import net.sf.jailer.ui.syntaxtextarea.RSyntaxTextAreaWithTheme;

/**
 * Modal dialog with two tabs: "Generate SQL" (natural language to SQL) and
 * "SQL Advisor" (analyze and refactor existing queries). Both tabs share the
 * same AI provider settings and system prompt.
 */
public class AIQueryDialog extends JDialog {

    private static final long serialVersionUID = 1L;
    private static final Logger _log = LoggerFactory.getLogger(AIQueryDialog.class);

    private final DataModel dataModel;
    private final String dbmsName;
    private final Consumer<String> insertAction;
    private final ExecutionContext executionContext;

    private AIProviderPanel providerPanel;
    private SystemPromptPanel systemPromptPanel;
    private JButton closeButton;
    private JButton systemPromptButton;

    private ConversationTab generateTab;
    private ConversationTab advisorTab;
    private JTabbedPane tabbedPane;

    private static int lastSelectedTab = 0;

    // -------------------------------------------------------------------------
    // Inner class: one self-contained conversation tab
    // -------------------------------------------------------------------------

    private class ConversationTab {

        final List<ConversationMessage> conversationHistory = new ArrayList<>();

        JTextArea historyArea;
        JScrollPane historyScrollPane;
        JTextArea questionArea;
        RSyntaxTextAreaWithSQLSyntaxStyle sqlArea;
        JButton generateButton;
        JButton insertButton;
        JButton newConversationButton;
        JLabel statusLabel;
        JCheckBox smartSelectionBox;
        JCheckBox omitColumnTypesBox;
        JLabel contextEstimateLabel;
        JButton cancelButton;
        SwingWorker<String, Void> currentWorker;
        final AtomicReference<Runnable> abortRef = new AtomicReference<>();

        boolean isAdvisor;
        RSyntaxTextAreaWithTheme answerArea;
        JLabel placeholderLabel;

        JPanel buildPanel(boolean isAdvisor) {
            this.isAdvisor = isAdvisor;
            historyArea = new JTextArea();
            historyArea.setEditable(false);
            historyArea.setEnabled(false);
            historyArea.setText("No conversation yet");
            historyArea.setCaretPosition(0);
            historyArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 11));
            historyArea.setLineWrap(true);
            historyArea.setWrapStyleWord(true);
            historyScrollPane = new JScrollPane(historyArea);
            historyScrollPane.setPreferredSize(new Dimension(700, 200));

            // Question area
            JPanel questionPanel = new JPanel(new GridBagLayout());
            JLabel questionTitleLabel = new JLabel(isAdvisor ? "What would you like to know or change?" : "Describe the query in plain language");
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
            JScrollPane questionScrollPane = new JScrollPane(questionArea);
            placeholderLabel = new JLabel();
            placeholderLabel.setEnabled(false);
            placeholderLabel.setFont(questionArea.getFont());
            placeholderLabel.setVerticalAlignment(SwingConstants.TOP);
            placeholderLabel.setVisible(false);
            JLayeredPane questionLayered = new JLayeredPane() {
                @Override
                public void doLayout() {
                    questionScrollPane.setBounds(0, 0, getWidth(), getHeight());
                    java.awt.Insets spIns = questionScrollPane.getInsets();
                    java.awt.Insets vpIns = questionScrollPane.getViewport().getInsets();
                    java.awt.Insets taIns = questionArea.getInsets();
                    int x = spIns.left + vpIns.left + taIns.left;
                    int y = spIns.top  + vpIns.top  + taIns.top;
                    placeholderLabel.setBounds(x, y,
                            getWidth()  - x - spIns.right  - vpIns.right  - taIns.right,
                            getHeight() - y - spIns.bottom - vpIns.bottom - taIns.bottom);
                }
                @Override
                public Dimension getPreferredSize() {
                    return questionScrollPane.getPreferredSize();
                }
            };
            questionLayered.add(questionScrollPane, JLayeredPane.DEFAULT_LAYER);
            questionLayered.add(placeholderLabel, JLayeredPane.PALETTE_LAYER);

            // Label: row 0, col 0
            GridBagConstraints gbc = new GridBagConstraints();
            gbc.gridx = 0; gbc.gridy = 0;
            gbc.weightx = 1; gbc.weighty = 0;
            gbc.fill = GridBagConstraints.HORIZONTAL;
            gbc.insets = new Insets(0, 0, 4, 0);
            gbc.anchor = GridBagConstraints.SOUTHWEST;
            questionPanel.add(questionTitleLabel, gbc);

            // Question area: row 1, cols 0+1
            gbc = new GridBagConstraints();
            gbc.gridx = 0; gbc.gridy = 1;
            gbc.gridwidth = 2;
            gbc.weightx = 1; gbc.weighty = 1;
            gbc.fill = GridBagConstraints.BOTH;
            gbc.insets = new Insets(2, 0, 0, 0);
            questionPanel.add(questionLayered, gbc);

            // Combobox (advisor only): col 1, spans rows 0+1 → does not affect row 0 height
            if (isAdvisor) {
                javax.swing.JComboBox<String> suggestionsBox = new javax.swing.JComboBox<>(new String[]{
                    "Suggestions…",
                    "Explain this query",
                    "Find potential performance issues",
                    "Optimize this query for performance",
                    "Rewrite using CTEs",
                    "Convert subqueries to joins",
                    "Make this query more readable",
                    "Add comments to explain the query",
                    "What indexes would help this query?"
                });
                suggestionsBox.addActionListener(e -> {
                    int idx = suggestionsBox.getSelectedIndex();
                    if (idx > 0) {
                        questionArea.setText((String) suggestionsBox.getSelectedItem());
                        questionArea.setCaretPosition(0);
                        suggestionsBox.setSelectedIndex(0);
                        questionArea.requestFocusInWindow();
                        generateButton.doClick();
                    }
                });
                gbc = new GridBagConstraints();
                gbc.gridx = 1; gbc.gridy = 0;
                gbc.gridheight = 1;
                gbc.weightx = 0; gbc.weighty = 0;
                gbc.fill = GridBagConstraints.NONE;
                gbc.anchor = GridBagConstraints.NORTHEAST;
                questionPanel.add(suggestionsBox, gbc);
            }

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

            JPanel genLeft = new JPanel(new GridBagLayout());
            {
                GridBagConstraints gbcL = new GridBagConstraints();
                gbcL.gridy = 0; gbcL.anchor = GridBagConstraints.WEST;
                gbcL.insets = new Insets(0, 0, 0, 6);
                gbcL.gridx = 0; genLeft.add(generateButton, gbcL);
                gbcL.gridx = 1; genLeft.add(cancelButton, gbcL);
                gbcL.gridx = 2; genLeft.add(newConversationButton, gbcL);
                gbcL.gridx = 3; gbcL.insets = new Insets(0, 0, 0, 0); genLeft.add(statusLabel, gbcL);
            }
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
            GridBagConstraints gbcGenRow = new GridBagConstraints();
            gbcGenRow.gridx = 0; gbcGenRow.gridy = 2;
            gbcGenRow.gridwidth = 2;
            gbcGenRow.weightx = 1; gbcGenRow.weighty = 0;
            gbcGenRow.fill = GridBagConstraints.HORIZONTAL;
            questionPanel.add(genRow, gbcGenRow);

            questionArea.addFocusListener(new java.awt.event.FocusAdapter() {
                @Override
                public void focusGained(java.awt.event.FocusEvent e) {
                    if (placeholderLabel != null) placeholderLabel.setVisible(false);
                }
            });
            questionArea.getDocument().addDocumentListener(new javax.swing.event.DocumentListener() {
                @Override
                public void insertUpdate(javax.swing.event.DocumentEvent e) {
                    if (placeholderLabel != null) placeholderLabel.setVisible(false);
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
            sqlArea = new RSyntaxTextAreaWithSQLSyntaxStyle(false, false);
            sqlArea.setRows(5);
            sqlArea.setColumns(60);
            RTextScrollPane sqlScrollPane = new RTextScrollPane();
            sqlScrollPane.setViewportView(sqlArea);
            JPanel insertRow = new JPanel(new GridBagLayout());
            {
                GridBagConstraints gbcI = new GridBagConstraints();
                gbcI.gridx = 0; gbcI.gridy = 0;
                gbcI.anchor = GridBagConstraints.WEST;
                gbcI.insets = new Insets(2, 0, 2, 0);
                insertRow.add(insertButton, gbcI);
                gbcI.gridx = 1; gbcI.weightx = 1; gbcI.fill = GridBagConstraints.HORIZONTAL;
                insertRow.add(new JLabel(), gbcI);
            }
            JPanel resultPanel = new JPanel(new BorderLayout(4, 4));
            if (isAdvisor) {
                JPanel sqlPanel = new JPanel(new BorderLayout(4, 4));
                sqlPanel.add(new JLabel("SQL"), BorderLayout.NORTH);
                sqlPanel.add(sqlScrollPane, BorderLayout.CENTER);
                answerArea = new RSyntaxTextAreaWithTheme();
                answerArea.setEditable(false);
                answerArea.setLineWrap(true);
                answerArea.setWrapStyleWord(true);
                RTextScrollPane answerScrollPane = new RTextScrollPane();
                answerScrollPane.setViewportView(answerArea);
                JPanel answerPanel = new JPanel(new BorderLayout(4, 4));
                answerPanel.add(new JLabel("Answer"), BorderLayout.NORTH);
                answerPanel.add(answerScrollPane, BorderLayout.CENTER);
                JSplitPane resultSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, sqlPanel, answerPanel);
                resultSplitPane.setResizeWeight(0.5);
                resultSplitPane.addHierarchyListener(new java.awt.event.HierarchyListener() {
                    public void hierarchyChanged(java.awt.event.HierarchyEvent e) {
                        if ((e.getChangeFlags() & java.awt.event.HierarchyEvent.SHOWING_CHANGED) != 0
                                && resultSplitPane.isShowing()) {
                            resultSplitPane.setDividerLocation(0.5);
                            resultSplitPane.removeHierarchyListener(this);
                        }
                    }
                });
                resultPanel.add(resultSplitPane, BorderLayout.CENTER);
            } else {
                resultPanel.add(new JLabel("Generated SQL"), BorderLayout.NORTH);
                resultPanel.add(sqlScrollPane, BorderLayout.CENTER);
            }
            resultPanel.add(insertRow, BorderLayout.SOUTH);

            JPanel questionResultPanel = new JPanel(new BorderLayout(4, 8));
            questionResultPanel.add(questionPanel, BorderLayout.NORTH);
            questionResultPanel.add(resultPanel, BorderLayout.CENTER);

            JButton historyToggleButton = new JButton("\u25BC Conversation");
            historyToggleButton.setBorderPainted(false);
            historyToggleButton.setContentAreaFilled(false);
            historyToggleButton.setFocusPainted(false);
            historyToggleButton.setHorizontalAlignment(SwingConstants.LEFT);
            historyToggleButton.setMargin(new Insets(2, 0, 2, 0));
            historyToggleButton.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
            historyToggleButton.setFont(historyToggleButton.getFont().deriveFont(Font.BOLD));
            historyToggleButton.setToolTipText("Click to collapse or expand the conversation history");
            JPanel conversationPanel = new JPanel(new BorderLayout());
            conversationPanel.setBorder(BorderFactory.createEtchedBorder());
            conversationPanel.add(historyScrollPane, BorderLayout.CENTER);
            conversationPanel.setVisible(false);

            historyToggleButton.addActionListener(e -> {
                boolean show = !conversationPanel.isVisible();
                conversationPanel.setVisible(show);
                historyToggleButton.setText((show ? "\u25B2" : "\u25BC") + " Conversation");
            });

            JPanel conversationWrapper = new JPanel(new BorderLayout(0, 2));
            conversationWrapper.add(historyToggleButton, BorderLayout.NORTH);
            conversationWrapper.add(conversationPanel, BorderLayout.CENTER);

            JPanel centerPanel = new JPanel(new BorderLayout(4, 8));
            centerPanel.add(conversationWrapper, BorderLayout.NORTH);
            centerPanel.add(questionResultPanel, BorderLayout.CENTER);

            return centerPanel;
        }

        void loadCheckboxStates(AIProviderConfig config) {
            omitColumnTypesBox.setSelected(AIQueryAssistant.loadOmitColumnTypes(config, executionContext));
            smartSelectionBox.setSelected(AIQueryAssistant.loadSmartSelection(config, executionContext, dataModel.getSortedTables().size()));
        }

        void updateContextEstimate() {
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

        void setGenerating(boolean generating) {
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

        void onGenerate() {
            String question = questionArea.getText().trim();
            if (question.isEmpty()) {
                JOptionPane.showMessageDialog(AIQueryDialog.this, "Please describe the query.", "Input Required", JOptionPane.WARNING_MESSAGE);
                return;
            }
            if (question.startsWith("!echo ")) {
                String sql = question.substring("!echo ".length());
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
                JOptionPane.showMessageDialog(AIQueryDialog.this, "Please enter an API key.", "API Key Required", JOptionPane.WARNING_MESSAGE);
                providerPanel.getApiKeyComponent().requestFocusInWindow();
                return;
            }
            final String sqlContent = isAdvisor ? sqlArea.getText().trim() : null;
            setGenerating(true);
            statusLabel.setText("Generating...");

            List<ConversationMessage> historySnapshot = new ArrayList<>(conversationHistory);
            boolean smartSelection = smartSelectionBox.isSelected();
            boolean omitColumnTypes = omitColumnTypesBox.isSelected();

            final AtomicReference<String> rawResponseRef = isAdvisor ? new AtomicReference<>("") : null;
            currentWorker = new SwingWorker<String, Void>() {
                @Override
                protected String doInBackground() throws Exception {
                    String template = isAdvisor ? systemPromptPanel.getAdvisorTemplate() : systemPromptPanel.getTemplate();
                    if (isAdvisor && sqlContent != null && template != null) {
                        template = template.replace("{SQL}", sqlContent);
                    }
                    return AIQueryAssistant.generateSQL(question, historySnapshot, dataModel, dbmsName, config,
                            template, smartSelection, omitColumnTypes, abortRef, () -> {
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
                    }, systemPromptPanel.getFirstPassTemplate(), rawResponseRef);
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
                        String answerText = null;
                        if (isAdvisor) {
                            String bare   = SystemPromptPanel.ADVISOR_SQL_ANSWER_SEPARATOR;
                            String quoted = "\"" + bare + "\"";
                            int sep    = sql.indexOf(quoted);
                            int sepLen = quoted.length();
                            if (sep < 0) {
                                sep    = sql.indexOf(bare);
                                sepLen = bare.length();
                            }
                            if (sep >= 0) {
                                answerText = sql.substring(sep + sepLen).trim();
                                sql = AIQueryAssistant.stripMarkdownCodeFence(sql.substring(0, sep).trim());
                                if (sql.endsWith(";")) {
                                	sql = sql.substring(0, sql.length() - 1).trim();
                        		}
                            } else {
                                answerText = rawResponseRef != null ? rawResponseRef.get() : sql;
                                sql = sqlContent != null ? sqlContent : "";
                            }
                        }
                        if (!sqlArea.getText().equals(sql) && !sql.isEmpty()) {
    						sqlArea.setText(sql);
    						sqlArea.setCaretPosition(0);
                        }
                        if (answerArea != null && answerText != null) {
                            answerArea.setText(answerText);
                            answerArea.setCaretPosition(0);
                        }
                        insertButton.setEnabled(!sql.isEmpty());
                        if (!sql.isEmpty()) {
                            providerPanel.markConnectionVerified();
                            AIQueryAssistant.saveCheckboxStates(config, executionContext, omitColumnTypes, smartSelection);
                            String rawForHistory = (rawResponseRef != null && rawResponseRef.get() != null && !rawResponseRef.get().isEmpty())
                                    ? rawResponseRef.get() : sql;
                            conversationHistory.add(new ConversationMessage("user", question));
                            conversationHistory.add(new ConversationMessage("assistant", rawForHistory));
                            questionArea.setText("");
                            questionArea.setCaretPosition(0);
                            String escaped = question.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\n", "<br>");
                            placeholderLabel.setText("<html>" + escaped + "</html>");
                            placeholderLabel.setVisible(true);
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

        void updateHistoryDisplay() {
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

        void clearHistory() {
            conversationHistory.clear();
            historyArea.setEnabled(false);
            historyArea.setText("No conversation yet");
            historyArea.setCaretPosition(0);
            newConversationButton.setEnabled(false);
            sqlArea.setText("");
            sqlArea.setCaretPosition(0);
            if (answerArea != null) { answerArea.setText(""); answerArea.setCaretPosition(0); }
            insertButton.setEnabled(false);
            statusLabel.setText(" ");
            placeholderLabel.setVisible(false);
        }

        String buildCommentForHistory() {
            return AIQueryAssistant.buildPromptComment(conversationHistory);
        }
    }

    // -------------------------------------------------------------------------
    // Dialog
    // -------------------------------------------------------------------------

    public AIQueryDialog(Window owner, DataModel dataModel, String dbmsName, Consumer<String> insertAction, ExecutionContext executionContext, String initialPrompt, boolean silent, String initialSql, String sql) {
        super(owner, "AI Assistant", ModalityType.APPLICATION_MODAL);
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
            generateTab.questionArea.setText(displayPrompt);
            generateTab.questionArea.setCaretPosition(0);
            if (silent) {
                generateTab.historyArea.setText(initialPrompt);
                generateTab.historyArea.setCaretPosition(0);
                if (generateTab.generateButton.isEnabled()) {
                    UIUtil.invokeLater(() -> generateTab.onGenerate());
                }
            }
        }
        if (initialSql != null) {
            String strippedSql = initialSql.replaceFirst("(?s)\\A\\s*/\\*.*?\\*/\\s*", "");
            generateTab.sqlArea.setText(strippedSql);
            generateTab.sqlArea.setCaretPosition(0);
        }
        if (sql != null) {
            advisorTab.sqlArea.setText(sql);
            advisorTab.sqlArea.setCaretPosition(0);
        }
        boolean hasPrompt = initialPrompt != null;
        boolean hasSql    = sql != null;
        tabbedPane.setEnabledAt(1, hasSql);
        if (hasPrompt != hasSql) {
            tabbedPane.setEnabledAt(0, hasPrompt);
            tabbedPane.setSelectedIndex(hasSql ? 1 : 0);
        } else {
            tabbedPane.setSelectedIndex(!hasSql && lastSelectedTab == 1 ? 0 : lastSelectedTab);
            tabbedPane.addChangeListener(e -> lastSelectedTab = tabbedPane.getSelectedIndex());
        }
        UIUtil.invokeLater(2, () -> { (tabbedPane.getSelectedIndex() != 0? advisorTab : generateTab).questionArea.grabFocus(); });
        UIUtil.initComponents(this);
        pack();
        setSize(getWidth() + 320, getHeight() + 200);
        setLocationRelativeTo(owner);
    }

    private void initUI() {
        ((JComponent) getContentPane()).setBorder(BorderFactory.createEmptyBorder(0, 12, 8, 12));
        setLayout(new BorderLayout(8, 8));

        generateTab = new ConversationTab();
        advisorTab  = new ConversationTab();

        JPanel generatePanel = generateTab.buildPanel(false);
        JPanel advisorPanel  = advisorTab.buildPanel(true);

        tabbedPane = new JTabbedPane();
        tabbedPane.addTab("Generate SQL", generatePanel);
        tabbedPane.setToolTipTextAt(0, "<html>Generate SQL from a plain-language description.<br>"
                + "Describe what you need and let the AI write the query for you.</html>");
        tabbedPane.addTab("SQL Advisor", advisorPanel);
        tabbedPane.setToolTipTextAt(1, "<html>Analyze, explain, and refactor existing SQL queries.<br>"
                + "Paste a query and ask the AI to explain, optimize, or rewrite it.</html>");
        tabbedPane.addChangeListener(e -> UIUtil.invokeLater(
                () -> (tabbedPane.getSelectedIndex() != 0 ? advisorTab : generateTab).questionArea.grabFocus()));

        // Shared settings
        JPanel settingsPanel = buildSettingsPanel();
        settingsPanel.setBorder(BorderFactory.createEmptyBorder(8, 0, 2, 0));
        AIProviderConfig initialConfig = providerPanel.getConfig();
        generateTab.loadCheckboxStates(initialConfig);
        advisorTab.loadCheckboxStates(initialConfig);

        systemPromptPanel = new SystemPromptPanel();

        // Buttons
        closeButton = new JButton("Close");
        ImageIcon closeIcon = UIUtil.readImage("/buttoncancel.png");
        if (closeIcon != null) {
            closeButton.setIcon(UIUtil.scaleIcon(closeButton, closeIcon));
        }
        closeButton.addActionListener(e -> dispose());

        systemPromptButton = new JButton("System Prompts...");
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

        add(tabbedPane, BorderLayout.CENTER);
        add(southPanel, BorderLayout.SOUTH);
    }

    private JPanel buildSettingsPanel() {
        providerPanel = new AIProviderPanel();
        return providerPanel;
    }

    private void openSystemPromptDialog() {
        JDialog d = new JDialog(this, "System Prompts", true);
        ((javax.swing.JComponent) d.getContentPane()).setBorder(BorderFactory.createEmptyBorder(8, 0, 0, 0));
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

        UIUtil.initComponents(d);
        d.pack();
        d.setSize(d.getWidth() + 120, Math.min(d.getHeight() + 100, 800));
        d.setLocationRelativeTo(this);
        d.setVisible(true);
    }

}

// TODO
// TODO suggest box 4px higher
// TODO more suggestions
// TODO keep orig prompt comment
// TODO suggestionbox disabled during call
// TODO adv: initially empty quest area

// TODO 
// TODO  handle markdown: *   **Indexes (Highest Priority):**  Create indexes on  `PROJECT_PARTICIPATION.EMPNO` and `PROJECT_PARTICIPATION.ROLE_ID`.
// TODO 	  *   **ANALYZE TABLE:** Run `ANALYZE TABLE EMPLOYEE; ANALYZE TABLE PROJECT_PARTICIPATION; ANALYZE TABLE ROLE;` to update table statistics.  Run this after large data modifications.
// TODO 	  *   **Data Size:** If possible, reduce the size of the `PROJECT_PARTICIPATION` table by archiving or " what about the stars?

