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
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Window;
import java.awt.event.ItemListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
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
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JLayeredPane;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.JToggleButton;
import javax.swing.KeyStroke;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;

import org.fife.ui.rsyntaxtextarea.SyntaxConstants;
import org.fife.ui.rtextarea.RTextScrollPane;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.database.Session;
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
    private final Session session;
    private AIProviderPanel providerPanel;
    private SystemPromptPanel systemPromptPanel;
    private JButton closeButton;
    private JButton systemPromptButton;

    private ConversationTab generateTab;
    private ConversationTab advisorTab;
    private JTabbedPane tabbedPane;

    private static int lastSelectedTab = 0;
    private boolean diffShownPreference = false;

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
        JComboBox<String> suggestionsBox;
        JToggleButton diffToggleButton;
        RSyntaxTextAreaWithTheme diffArea;
        JScrollPane diffScrollPane;
        JLayeredPane sqlLayeredPane;
        String lastOriginalSql;
        JEditorPane answerArea;
        String answerRawText;
        JButton copyAnswerButton;
        JLabel placeholderLabel;
        JPanel questionResultPanel;
        JPanel conversationWrapper;

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
            gbc.insets = new Insets(4, 0, 2, 0);
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
                suggestionsBox = new JComboBox<>(new String[]{
                    "Suggestions…",
                    "Explain this query",
                    "Add comments to explain the query",
                    "Make this query more readable",
                    "Identify columns that could be renamed for clarity",
                    "Find potential performance issues",
                    "Optimize this query for performance",
                    "What indexes would help this query?",
                    "Rewrite using CTEs",
                    "Extract repeated expressions as CTEs",
                    "Convert subqueries to joins",
                    "Rewrite using window functions",
                    "Convert IN to EXISTS",
                    "Simplify redundant conditions",
                    "Extract repeated expressions as CTEs",
                    "Check for NULL handling issues"
                });
                suggestionsBox.setMaximumRowCount(16);
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

            generateButton = new JButton(isAdvisor ? "Ask AI" : "Generate SQL");
            ImageIcon aiIcon = UIUtil.scaleIcon(generateButton, UIUtil.readImage("/ask_ai.png"));
            if (aiIcon != null) {
                generateButton.setIcon(UIUtil.scaleIcon(generateButton, aiIcon));
            }
            generateButton.setEnabled(false);
            generateButton.setToolTipText(isAdvisor ? "Ask AI (Ctrl+Enter)" : "Generate SQL (Ctrl+Enter)");
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
                    String combined;
                    if (isAdvisor) {
                        combined = sql;
                    } else {
                        String comment = buildCommentForHistory();
                        combined = comment.isEmpty() ? sql : comment + "\n" + sql;
                    }
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
            diffToggleButton = new JToggleButton("Show Diff");
            diffToggleButton.setEnabled(false);
            diffToggleButton.setSelected(diffShownPreference);
            diffToggleButton.setToolTipText("Toggle inline diff view: original vs. AI-modified SQL");
            diffToggleButton.setIcon(UIUtil.scaleIcon(diffToggleButton, UIUtil.readImage("/diff.png")));
            diffToggleButton.addActionListener(e -> {
                diffShownPreference = diffToggleButton.isSelected();
                boolean show = diffShownPreference && lastOriginalSql != null;
                if (show) updateDiffArea();
                diffScrollPane.setVisible(show);
                sqlLayeredPane.revalidate();
                sqlLayeredPane.repaint();
            });
            JPanel insertRow = new JPanel(new GridBagLayout());
            {
                GridBagConstraints gbcI = new GridBagConstraints();
                gbcI.gridx = 0; gbcI.gridy = 0;
                gbcI.anchor = GridBagConstraints.WEST;
                gbcI.insets = new Insets(2, 0, 2, 0);
                insertRow.add(insertButton, gbcI);
                gbcI.gridx = 1; gbcI.weightx = 0; gbcI.fill = GridBagConstraints.NONE;
                gbcI.insets = new Insets(2, 6, 2, 0);
                insertRow.add(diffToggleButton, gbcI);
                gbcI.gridx = 2; gbcI.insets = new Insets(2, 0, 2, 0);
                gbcI.weightx = 1; gbcI.fill = GridBagConstraints.HORIZONTAL;
                insertRow.add(new JLabel(), gbcI);
            }
            diffArea = new RSyntaxTextAreaWithTheme();
            diffArea.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_SQL);
            diffArea.setBracketMatchingEnabled(false);
            diffArea.setHighlightCurrentLine(false);
            diffArea.setEditable(false);
            diffScrollPane = new JScrollPane(diffArea);
            diffScrollPane.setVisible(false);
            final RTextScrollPane sqlScrollPaneFinal = sqlScrollPane;
            sqlLayeredPane = new JLayeredPane() {
                @Override
                public void doLayout() {
                    for (java.awt.Component c : getComponents()) {
                        c.setBounds(0, 0, getWidth(), getHeight());
                    }
                }
                @Override
                public Dimension getPreferredSize() {
                    return sqlScrollPaneFinal.getPreferredSize();
                }
                @Override
                public Dimension getMinimumSize() {
                    return sqlScrollPaneFinal.getMinimumSize();
                }
            };
            sqlLayeredPane.add(sqlScrollPane, JLayeredPane.DEFAULT_LAYER);
            sqlLayeredPane.add(diffScrollPane, JLayeredPane.PALETTE_LAYER);
            JPanel resultPanel = new JPanel(new BorderLayout(4, 4));
            if (isAdvisor) {
                JPanel sqlButtonRow = new JPanel(new GridBagLayout());
                {
                    GridBagConstraints gbcS = new GridBagConstraints();
                    gbcS.gridx = 0; gbcS.gridy = 0;
                    gbcS.anchor = GridBagConstraints.WEST;
                    gbcS.insets = new Insets(2, 0, 2, 0);
                    sqlButtonRow.add(insertButton, gbcS);
                    gbcS.gridx = 1; gbcS.insets = new Insets(2, 6, 2, 0);
                    sqlButtonRow.add(diffToggleButton, gbcS);
                    gbcS.gridx = 2; gbcS.weightx = 1; gbcS.fill = GridBagConstraints.HORIZONTAL;
                    gbcS.insets = new Insets(0, 0, 0, 0);
                    sqlButtonRow.add(new JLabel(), gbcS);
                }
                JPanel sqlPanel = new JPanel(new BorderLayout(4, 4));
                sqlPanel.add(new JLabel("SQL"), BorderLayout.NORTH);
                sqlPanel.add(sqlLayeredPane, BorderLayout.CENTER);
                sqlPanel.add(sqlButtonRow, BorderLayout.SOUTH);
                answerArea = new JEditorPane("text/html", "");
                answerArea.setEditable(false);
                answerArea.putClientProperty(JEditorPane.HONOR_DISPLAY_PROPERTIES, Boolean.TRUE);
                JScrollPane answerScrollPane = new JScrollPane(answerArea);
                copyAnswerButton = new JButton("Copy");
                ImageIcon copyIcon = UIUtil.readImage("/copy.png");
                if (copyIcon != null) copyAnswerButton.setIcon(UIUtil.scaleIcon(copyAnswerButton, copyIcon));
                copyAnswerButton.setEnabled(false);
                copyAnswerButton.setToolTipText("Copy answer to clipboard (HTML and plain text)");
                copyAnswerButton.addActionListener(e -> copyAnswerToClipboard());
                JPanel answerButtonRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 2));
                answerButtonRow.add(copyAnswerButton);
                boolean dark = UIUtil.plaf == UIUtil.PLAF.FLATDARK;
                ImageIcon maximizeIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/maximize.png"));
                ImageIcon unmaximizeIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/unmaximize.png"));
                ImageIcon maximizeDarkIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/maximize_dark.png"));
                ImageIcon unmaximizeDarkIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/unmaximize_dark.png"));
                ImageIcon maxIcon = dark ? maximizeDarkIcon : maximizeIcon;
                ImageIcon unmaxIcon = dark ? unmaximizeDarkIcon : unmaximizeIcon;
                JToggleButton maximizeAnswerButton = new JToggleButton(maxIcon);
                maximizeAnswerButton.setMargin(new Insets(1, 2, 1, 2));
                maximizeAnswerButton.setBorderPainted(false);
                maximizeAnswerButton.setContentAreaFilled(false);
                maximizeAnswerButton.setFocusPainted(false);
                maximizeAnswerButton.setToolTipText("Maximize answer area");
                JPanel answerHeader = new JPanel(new BorderLayout());
                answerHeader.add(new JLabel("Answer"), BorderLayout.WEST);
                answerHeader.add(maximizeAnswerButton, BorderLayout.EAST);
                JPanel answerPanel = new JPanel(new BorderLayout(4, 4));
                answerPanel.add(answerHeader, BorderLayout.NORTH);
                answerPanel.add(answerScrollPane, BorderLayout.CENTER);
                answerPanel.add(answerButtonRow, BorderLayout.SOUTH);
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
                maximizeAnswerButton.addActionListener(e -> {
                    if (maximizeAnswerButton.isSelected()) {
                        if (conversationWrapper != null) conversationWrapper.setVisible(false);
                        questionPanel.setVisible(false);
                        sqlPanel.setMinimumSize(new Dimension(0, 0));
                        resultSplitPane.setDividerLocation(0);
                        maximizeAnswerButton.setIcon(unmaxIcon);
                        maximizeAnswerButton.setToolTipText("Restore split view");
                    } else {
                        if (conversationWrapper != null) conversationWrapper.setVisible(true);
                        questionPanel.setVisible(true);
                        resultSplitPane.setDividerLocation(0.5);
                        maximizeAnswerButton.setIcon(maxIcon);
                        maximizeAnswerButton.setToolTipText("Maximize answer area");
                    }
                });
                resultPanel.add(resultSplitPane, BorderLayout.CENTER);
            } else {
                resultPanel.add(new JLabel("Generated SQL"), BorderLayout.NORTH);
                resultPanel.add(sqlLayeredPane, BorderLayout.CENTER);
                resultPanel.add(insertRow, BorderLayout.SOUTH);
            }

            JPanel questionResultPanel = new JPanel(new BorderLayout(4, 8));
            questionResultPanel.add(questionPanel, BorderLayout.NORTH);
            questionResultPanel.add(resultPanel, BorderLayout.CENTER);
            this.questionResultPanel = questionResultPanel;

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
            this.conversationWrapper = conversationWrapper;

            JPanel centerPanel = new JPanel(new BorderLayout(4, 8));
            centerPanel.add(conversationWrapper, BorderLayout.NORTH);
            centerPanel.add(questionResultPanel, BorderLayout.CENTER);

            return centerPanel;
        }

        void loadCheckboxStates(AIProviderConfig config) {
            omitColumnTypesBox.setSelected(AIQueryAssistant.loadOmitColumnTypes(config, executionContext));
            smartSelectionBox.setSelected(AIQueryAssistant.loadSmartSelection(config, executionContext));
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
            if (suggestionsBox != null) suggestionsBox.setEnabled(!generating);
            if (diffToggleButton != null) diffToggleButton.setEnabled(!generating && lastOriginalSql != null && !lastOriginalSql.equals(sqlArea.getText().trim()));
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
            final String sqlContent = sqlArea.getText().trim();
            java.util.regex.Matcher _cm = java.util.regex.Pattern
                    .compile("(?s)\\A\\s*(/\\*\\s*AI:.*?\\*/)")
                    .matcher(isAdvisor ? sqlArea.getText() : "");
            final String existingAiComment = _cm.find() ? _cm.group(1) : "";
            setGenerating(true);
            statusLabel.setText("Generating...");

            List<ConversationMessage> historySnapshot = new ArrayList<>(conversationHistory);
            boolean smartSelection = smartSelectionBox.isSelected();
            boolean omitColumnTypes = omitColumnTypesBox.isSelected();

            final AtomicReference<String> rawResponseRef = isAdvisor ? new AtomicReference<>("") : null;
            final AtomicReference<Boolean> smartSelectionFallbackRef = new AtomicReference<>(Boolean.FALSE);
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
                    }, systemPromptPanel.getFirstPassTemplate(), rawResponseRef, session, smartSelectionFallbackRef);
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
                            // Point 5: mask code-block contents so separator search ignores them
                            java.util.regex.Matcher maskM = java.util.regex.Pattern.compile("(?s)```.*?```").matcher(sql);
                            StringBuffer maskedBuf = new StringBuffer(sql.length());
                            int maskPos = 0;
                            while (maskM.find()) {
                                maskedBuf.append(sql, maskPos, maskM.start());
                                for (int i = maskM.start(); i < maskM.end(); i++) maskedBuf.append('\0');
                                maskPos = maskM.end();
                            }
                            maskedBuf.append(sql, maskPos, sql.length());
                            String masked = maskedBuf.toString();

                            // Point 1: fuzzy separator matching (--ENDOFSQL / -- END OF SQL / "..." variants)
                            java.util.regex.Matcher sepM = java.util.regex.Pattern
                                    .compile("(?i)\"?--\\s*end\\s*(?:of\\s*)?sql\"?")
                                    .matcher(masked);
                            int sep = -1, sepLen = 0;
                            if (sepM.find()) { sep = sepM.start(); sepLen = sepM.end() - sepM.start(); }

                            if (sep >= 0) {
                                answerText = sql.substring(sep + sepLen).trim();
                                sql = AIQueryAssistant.stripMarkdownCodeFence(sql.substring(0, sep).trim());
                                if (sql.endsWith(";")) sql = sql.substring(0, sql.length() - 1).trim();
                            } else {
                                // Heuristic fallback: extract last markdown code block as SQL
                                String raw = rawResponseRef != null ? rawResponseRef.get() : "";
                                java.util.regex.Matcher cm2 = java.util.regex.Pattern
                                        .compile("(?s)```(?:sql|SQL)?\\s*\\n(.*?)```")
                                        .matcher(raw);
                                String lastBlock = null; int lastStart = -1, lastEnd = -1;
                                while (cm2.find()) { lastBlock = cm2.group(1).trim(); lastStart = cm2.start(); lastEnd = cm2.end(); }
                                if (lastBlock != null && !lastBlock.isEmpty()) {
                                    sql = lastBlock.endsWith(";") ? lastBlock.substring(0, lastBlock.length() - 1).trim() : lastBlock;
                                    String before = raw.substring(0, lastStart).trim();
                                    String after  = raw.substring(lastEnd).trim();
                                    answerText = before.isEmpty() ? after : after.isEmpty() ? before : before + "\n" + after;
                                } else {
                                    // Point 2: bare SQL (no code fence) — response starts with SQL keyword
                                    String rawTrimmed = raw.trim();
                                    java.util.regex.Matcher sqlKw = java.util.regex.Pattern
                                            .compile("(?si)^\\s*(?:(?:--[^\\n]*|/\\*.*?\\*/)\\s*)*(SELECT|WITH|UPDATE|INSERT|DELETE|CREATE|ALTER|DROP|MERGE)\\b")
                                            .matcher(rawTrimmed);
                                    if (sqlKw.find()) {
                                        java.util.regex.Matcher blankM = java.util.regex.Pattern
                                                .compile("(\r?\n){2,}").matcher(rawTrimmed);
                                        if (blankM.find()) {
                                            sql = rawTrimmed.substring(0, blankM.start()).trim();
                                            answerText = rawTrimmed.substring(blankM.end()).trim();
                                            if (sql.endsWith(";")) sql = sql.substring(0, sql.length() - 1).trim();
                                        } else {
                                            java.util.regex.Matcher semiM = java.util.regex.Pattern
                                                    .compile(";[ \\t]*(\\r?\\n)").matcher(rawTrimmed);
                                            int lastSemiPos = -1, lastSemiEnd = -1;
                                            while (semiM.find()) { lastSemiPos = semiM.start(); lastSemiEnd = semiM.end(); }
                                            if (lastSemiEnd > 0) {
                                                sql = rawTrimmed.substring(0, lastSemiPos).trim();
                                                answerText = rawTrimmed.substring(lastSemiEnd).trim();
                                                if (answerText.isEmpty()) answerText = null;
                                            } else {
                                                sql = rawTrimmed;
                                                if (sql.endsWith(";")) sql = sql.substring(0, sql.length() - 1).trim();
                                                answerText = null;
                                            }
                                        }
                                    } else {
                                        answerText = raw.isEmpty() ? sql : raw;
                                        sql = sqlContent != null ? sqlContent : "";
                                    }
                                }
                            }
                        }
                        String displaySql = (isAdvisor && !existingAiComment.isEmpty()
                                && AIQueryAssistant.extractPrompt(sql) == null)
                                ? existingAiComment + "\n" + sql : sql;
                        if (!sqlArea.getText().equals(displaySql) && !displaySql.isEmpty()) {
                            sqlArea.beginAtomicEdit();
                            try {
                                sqlArea.setText(displaySql);
                            } finally {
                                sqlArea.endAtomicEdit();
                            }
                            sqlArea.setCaretPosition(0);
                        }
                        if (answerArea != null && answerText != null) {
                            java.util.regex.Matcher outerFence = java.util.regex.Pattern
                                    .compile("(?s)\\A```[a-zA-Z]*\\r?\\n(.*)\\r?\\n```\\z")
                                    .matcher(answerText.trim());
                            if (outerFence.matches()) answerText = outerFence.group(1);
                            answerRawText = answerText;
                            answerArea.setText(markdownToHtml(answerText));
                            answerArea.setCaretPosition(0);
                            if (copyAnswerButton != null) copyAnswerButton.setEnabled(true);
                        }
                        insertButton.setEnabled(!sql.isEmpty());
                        if (diffToggleButton != null && !sql.isEmpty()) {
                            lastOriginalSql = sqlContent;
                            boolean hasDiff = !lastOriginalSql.equals(sqlArea.getText().trim());
                            diffToggleButton.setEnabled(hasDiff);
                            if (hasDiff && diffToggleButton.isSelected()) {
                                updateDiffArea();
                                diffScrollPane.setVisible(true);
                            } else if (!hasDiff) {
                                diffScrollPane.setVisible(false);
                            }
                            sqlLayeredPane.revalidate();
                            sqlLayeredPane.repaint();
                        }
                        if (!sql.isEmpty()) {
                            providerPanel.markConnectionVerified();
                            if (Boolean.TRUE.equals(smartSelectionFallbackRef.get())) {
                                smartSelectionBox.setSelected(true);
                                AIQueryAssistant.saveCheckboxStates(config, executionContext, omitColumnTypes, true);
                                JOptionPane.showMessageDialog(AIQueryDialog.this,
                                    "<html>The full schema was too large for the AI model.<br>"
                                    + "The query was generated using <b>smart table selection</b>.<br>"
                                    + "The option has been enabled for future requests.</html>",
                                    "Smart Table Selection activated",
                                    JOptionPane.INFORMATION_MESSAGE);
                            } else {
                                AIQueryAssistant.saveCheckboxStates(config, executionContext, omitColumnTypes, smartSelection);
                            }
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
                            if (!isAdvisor) {
                                advisorTab.clearHistory();
                                String aiComment = buildCommentForHistory();
                                String sqlWithComment = aiComment.isEmpty() ? sql : aiComment + "\n" + sql;
                                advisorTab.sqlArea.setText(sqlWithComment);
                                advisorTab.sqlArea.setCaretPosition(0);
                                advisorTab.insertButton.setEnabled(true);
                                advisorTab.conversationHistory.add(new ConversationMessage("user", question));
                                advisorTab.conversationHistory.add(new ConversationMessage("assistant", sql));
                                advisorTab.updateHistoryDisplay();
                                tabbedPane.setEnabledAt(1, true);
                            }
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
            if (answerArea != null) { answerArea.setText("<html><body></body></html>"); answerArea.setCaretPosition(0); }
            answerRawText = null;
            if (copyAnswerButton != null) copyAnswerButton.setEnabled(false);
            if (diffToggleButton != null) {
                diffToggleButton.setEnabled(false);
                lastOriginalSql = null;
                diffScrollPane.setVisible(false);
                sqlLayeredPane.revalidate();
                sqlLayeredPane.repaint();
            }
            insertButton.setEnabled(false);
            statusLabel.setText(" ");
            placeholderLabel.setVisible(false);
        }

        void copyAnswerToClipboard() {
            if (answerArea == null) return;
            String html = answerArea.getText();
            String plain = answerRawText != null ? answerRawText : "";
            java.awt.datatransfer.DataFlavor htmlFlavorTmp;
            try {
                htmlFlavorTmp = new java.awt.datatransfer.DataFlavor("text/html;class=java.lang.String");
            } catch (ClassNotFoundException ex) {
                htmlFlavorTmp = null;
            }
            final java.awt.datatransfer.DataFlavor htmlFlavor = htmlFlavorTmp;
            final String htmlContent = html;
            final String plainContent = plain;
            java.awt.datatransfer.Transferable t = new java.awt.datatransfer.Transferable() {
                @Override
                public java.awt.datatransfer.DataFlavor[] getTransferDataFlavors() {
                    if (htmlFlavor != null) {
                        return new java.awt.datatransfer.DataFlavor[]{htmlFlavor, java.awt.datatransfer.DataFlavor.stringFlavor};
                    }
                    return new java.awt.datatransfer.DataFlavor[]{java.awt.datatransfer.DataFlavor.stringFlavor};
                }
                @Override
                public boolean isDataFlavorSupported(java.awt.datatransfer.DataFlavor flavor) {
                    if (htmlFlavor != null && htmlFlavor.equals(flavor)) return true;
                    return java.awt.datatransfer.DataFlavor.stringFlavor.equals(flavor);
                }
                @Override
                public Object getTransferData(java.awt.datatransfer.DataFlavor flavor)
                        throws java.awt.datatransfer.UnsupportedFlavorException, java.io.IOException {
                    if (htmlFlavor != null && htmlFlavor.equals(flavor)) return htmlContent;
                    if (java.awt.datatransfer.DataFlavor.stringFlavor.equals(flavor)) return plainContent;
                    throw new java.awt.datatransfer.UnsupportedFlavorException(flavor);
                }
            };
            java.awt.Toolkit.getDefaultToolkit().getSystemClipboard().setContents(t, null);
        }

        void updateDiffArea() {
            if (lastOriginalSql == null) return;
            boolean dark = UIUtil.plaf == UIUtil.PLAF.FLATDARK;
            Color delColor = dark ? new Color(0x60, 0x20, 0x20) : new Color(0xFF, 0xD0, 0xD0);
            Color addColor = dark ? new Color(0x1a, 0x4a, 0x1a) : new Color(0xD0, 0xFF, 0xD0);
            String[] orig = lastOriginalSql.replace("\r\n", "\n").split("\n", -1);
            String[] mod  = sqlArea.getText().trim().replace("\r\n", "\n").split("\n", -1);
            List<String> diff = computeDiff(orig, mod);
            StringBuilder sb = new StringBuilder();
            for (String entry : diff) {
                char type = entry.charAt(0);
                sb.append(type == '-' ? "- " : type == '+' ? "+ " : "  ").append(entry.substring(1)).append("\n");
            }
            diffArea.setText(sb.toString());
            diffArea.setCaretPosition(0);
            diffArea.removeAllLineHighlights();
            int lineIdx = 0;
            for (String entry : diff) {
                char type = entry.charAt(0);
                if (type != '=') {
                    try { diffArea.addLineHighlight(lineIdx, type == '-' ? delColor : addColor); }
                    catch (javax.swing.text.BadLocationException ex) { _log.warn("diff highlight", ex); }
                }
                lineIdx++;
            }
        }

        String buildCommentForHistory() {
            return AIQueryAssistant.buildPromptComment(conversationHistory);
        }
    }

    // -------------------------------------------------------------------------
    // Dialog
    // -------------------------------------------------------------------------

    public AIQueryDialog(Window owner, DataModel dataModel, String dbmsName, Consumer<String> insertAction, ExecutionContext executionContext, String initialPrompt, boolean silent, String initialSql, String sql, Session session) {
        super(owner, "AI Assistant", ModalityType.APPLICATION_MODAL);
        this.dataModel = dataModel;
        this.dbmsName = dbmsName;
        this.insertAction = insertAction;
        this.session = session;
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

    private static List<String> computeDiff(String[] orig, String[] mod) {
        int m = Math.min(orig.length, 800), n = Math.min(mod.length, 800);
        int[][] dp = new int[m + 1][n + 1];
        for (int i = m - 1; i >= 0; i--)
            for (int j = n - 1; j >= 0; j--)
                dp[i][j] = orig[i].equals(mod[j]) ? dp[i+1][j+1] + 1 : Math.max(dp[i+1][j], dp[i][j+1]);
        List<String> result = new ArrayList<>();
        int i = 0, j = 0;
        while (i < m || j < n) {
            if (i < m && j < n && orig[i].equals(mod[j])) {
                result.add("=" + orig[i]); i++; j++;
            } else if (j < n && (i >= m || dp[i][j+1] >= dp[i+1][j])) {
                result.add("+" + mod[j]); j++;
            } else {
                result.add("-" + orig[i]); i++;
            }
        }
        while (i < orig.length) { result.add("-" + orig[i]); i++; }
        while (j < mod.length)  { result.add("+" + mod[j]);  j++; }
        return result;
    }

    private static String markdownToHtml(String text) {
        if (text == null || text.isEmpty()) return "<html><body></body></html>";
        boolean dark = UIUtil.plaf == UIUtil.PLAF.FLATDARK;
        String codeBg  = dark ? "#1e1e1e" : "#f5f5f5";
        String codeBd  = dark ? "#555"    : "#ccc";
        String qtColor = dark ? "#aaa"    : "#555";
        String qtBd    = dark ? "#666"    : "#aaa";
        StringBuilder sb = new StringBuilder();
        sb.append("<html><head><style>");
        sb.append("body{margin:4px}");
        sb.append("pre{background:").append(codeBg).append(";border:1px solid ").append(codeBd).append(";padding:6px;white-space:pre-wrap}");
        sb.append("code,tt{font-family:monospace}");
        sb.append("blockquote{border-left:3px solid ").append(qtBd).append(";margin-left:4px;padding-left:8px;color:").append(qtColor).append("}");
        sb.append("h1{font-size:1.3em;margin:6px 0}h2{font-size:1.15em;margin:5px 0}h3{font-size:1.05em;margin:4px 0}");
        sb.append("</style></head><body>");
        boolean inCode = false, inUl = false, inOl = false, inBq = false;
        boolean inNestedUl = false, inOlLi = false;
        java.util.regex.Pattern olPat = java.util.regex.Pattern.compile("^\\d+\\.\\s+(.+)");
        java.util.regex.Pattern nestedBulletPat = java.util.regex.Pattern.compile("^\\s+[-*]\\s+(.+)");
        for (String line : text.split("\n", -1)) {
            if (line.trim().startsWith("```")) {
                if (!inCode) {
                    if (inNestedUl) { sb.append("</ul>"); inNestedUl = false; }
                    if (inOlLi) { sb.append("</li>"); inOlLi = false; }
                    if (inUl) { sb.append("</ul>"); inUl = false; }
                    if (inOl) { sb.append("</ol>"); inOl = false; }
                    if (inBq) { sb.append("</blockquote>"); inBq = false; }
                    sb.append("<pre><code>"); inCode = true;
                } else {
                    sb.append("</code></pre>"); inCode = false;
                }
                continue;
            }
            if (inCode) { sb.append(mdEscape(line)).append("\n"); continue; }
            if (line.trim().isEmpty()) {
                if (inNestedUl) { sb.append("</ul>"); inNestedUl = false; }
                if (inOlLi) { sb.append("</li>"); inOlLi = false; }
                if (inUl) { sb.append("</ul>"); inUl = false; }
                if (inOl) { sb.append("</ol>"); inOl = false; }
                if (inBq) { sb.append("</blockquote>"); inBq = false; }
                sb.append("<br>"); continue;
            }
            if (line.matches("[-*_]{3,}\\s*")) {
                if (inNestedUl) { sb.append("</ul>"); inNestedUl = false; }
                if (inOlLi) { sb.append("</li>"); inOlLi = false; }
                if (inUl) { sb.append("</ul>"); inUl = false; }
                if (inOl) { sb.append("</ol>"); inOl = false; }
                if (inBq) { sb.append("</blockquote>"); inBq = false; }
                sb.append("<hr>"); continue;
            }
            if (line.startsWith("### ")) {
                if (inNestedUl) { sb.append("</ul>"); inNestedUl = false; }
                if (inOlLi) { sb.append("</li>"); inOlLi = false; }
                if (inUl) { sb.append("</ul>"); inUl = false; } if (inOl) { sb.append("</ol>"); inOl = false; } if (inBq) { sb.append("</blockquote>"); inBq = false; }
                sb.append("<h3>").append(mdInline(line.substring(4))).append("</h3>"); continue;
            }
            if (line.startsWith("## ")) {
                if (inNestedUl) { sb.append("</ul>"); inNestedUl = false; }
                if (inOlLi) { sb.append("</li>"); inOlLi = false; }
                if (inUl) { sb.append("</ul>"); inUl = false; } if (inOl) { sb.append("</ol>"); inOl = false; } if (inBq) { sb.append("</blockquote>"); inBq = false; }
                sb.append("<h2>").append(mdInline(line.substring(3))).append("</h2>"); continue;
            }
            if (line.startsWith("# ")) {
                if (inNestedUl) { sb.append("</ul>"); inNestedUl = false; }
                if (inOlLi) { sb.append("</li>"); inOlLi = false; }
                if (inUl) { sb.append("</ul>"); inUl = false; } if (inOl) { sb.append("</ol>"); inOl = false; } if (inBq) { sb.append("</blockquote>"); inBq = false; }
                sb.append("<h1>").append(mdInline(line.substring(2))).append("</h1>"); continue;
            }
            if (line.startsWith("> ")) {
                if (inNestedUl) { sb.append("</ul>"); inNestedUl = false; }
                if (inOlLi) { sb.append("</li>"); inOlLi = false; }
                if (inUl) { sb.append("</ul>"); inUl = false; } if (inOl) { sb.append("</ol>"); inOl = false; }
                if (!inBq) { sb.append("<blockquote>"); inBq = true; }
                sb.append(mdInline(line.substring(2))).append("<br>"); continue;
            }
            java.util.regex.Matcher nm = olPat.matcher(line);
            if (nm.matches()) {
                if (inBq) { sb.append("</blockquote>"); inBq = false; } if (inUl) { sb.append("</ul>"); inUl = false; }
                if (inNestedUl) { sb.append("</ul>"); inNestedUl = false; }
                if (inOlLi) { sb.append("</li>"); inOlLi = false; }
                if (!inOl) { sb.append("<ol>"); inOl = true; }
                sb.append("<li>").append(mdInline(nm.group(1)));
                inOlLi = true;
                continue;
            }
            java.util.regex.Matcher nbm = nestedBulletPat.matcher(line);
            if (inOl && nbm.matches()) {
                if (!inNestedUl) { sb.append("<ul>"); inNestedUl = true; }
                sb.append("<li>").append(mdInline(nbm.group(1))).append("</li>"); continue;
            }
            if (line.startsWith("- ") || line.startsWith("* ")) {
                if (inBq) { sb.append("</blockquote>"); inBq = false; }
                if (inNestedUl) { sb.append("</ul>"); inNestedUl = false; }
                if (inOlLi) { sb.append("</li>"); inOlLi = false; }
                if (inOl) { sb.append("</ol>"); inOl = false; }
                if (!inUl) { sb.append("<ul>"); inUl = true; }
                sb.append("<li>").append(mdInline(line.substring(2))).append("</li>"); continue;
            }
            if (inBq) { sb.append("</blockquote>"); inBq = false; }
            if (inNestedUl) { sb.append("</ul>"); inNestedUl = false; }
            if (inOlLi) { sb.append("</li>"); inOlLi = false; }
            if (inUl) { sb.append("</ul>"); inUl = false; }
            if (inOl) { sb.append("</ol>"); inOl = false; }
            sb.append(mdInline(line)).append("<br>");
        }
        if (inNestedUl) sb.append("</ul>");
        if (inOlLi) sb.append("</li>");
        if (inUl) sb.append("</ul>");
        if (inOl) sb.append("</ol>");
        if (inBq) sb.append("</blockquote>");
        if (inCode) sb.append("</code></pre>");
        sb.append("</body></html>");
        return sb.toString();
    }

    private static String mdEscape(String s) {
        return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;");
    }

    private static String mdInline(String text) {
        String s = mdEscape(text);
        s = s.replaceAll("\\*\\*([^*\n]+?)\\*\\*", "<b>$1</b>");
        s = s.replaceAll("~~([^\n]+?)~~", "<s>$1</s>");
        s = s.replaceAll("\\*([^*\n]+?)\\*", "<i>$1</i>");
        s = s.replaceAll("(?<![\\w_])_([^_\n]+?)_(?![\\w_])", "<i>$1</i>");
        s = s.replaceAll("`([^`\n]+?)`", "<tt>$1</tt>");
        return s;
    }

    private void openSystemPromptDialog() {
        JDialog d = new JDialog(this, "System Prompts", true);
        ((javax.swing.JComponent) d.getContentPane()).setBorder(BorderFactory.createEmptyBorder(8, 0, 0, 0));
        d.getContentPane().add(systemPromptPanel, BorderLayout.CENTER);

        d.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                systemPromptPanel.discardSettings();
            }
        });

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
// TODO test with various models
// TODO test first pass strategy, esp.: no unnecessary quoting (but quoting of keyword) of simple table names, no lU changes
// TODO how about data model > 500 tables? relevant-tables-strategy might be unnecessary, or even counterproductive in that case.
