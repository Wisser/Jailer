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
package net.sf.jailer.ui;

import java.awt.BorderLayout;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.IOException;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.atomic.AtomicReference;

import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.KeyStroke;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.ui.Colors;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.ai.AIProviderConfig;
import net.sf.jailer.ui.ai.AIProviderPanel;
import net.sf.jailer.ui.ai.AIQueryAssistant;
import net.sf.jailer.ui.util.UISettings;

/**
 * Dialog for AI-assisted creation of an extraction model.
 * The user describes in natural language what data to extract,
 * and the AI suggests the subject table, WHERE condition and
 * association restrictions.
 */
public class AIExtractionModelDialog extends JDialog {

    private static final long serialVersionUID = 1L;

    private final ExtractionModelEditor editor;
    private final DataModel dataModel;
    private final ExecutionContext executionContext;
    private AIProviderPanel providerPanel;
    private JTextArea questionArea;
    private JButton generateButton;
    private JButton cancelButton;
    private JButton applyButton;
    private JButton closeButton;
    private JTextArea previewArea;
    private JLabel statusLabel;

    private String resultSubject;
    private String resultCondition;
    private List<String[]> resultRestrictions;

    private JCheckBox reducedSchemaBox;
    private JTextField maxTablesField;
    private JCheckBox omitColumnTypesBox;

    private static final String SETTING_REDUCED_SCHEMA = "aiExtractionReducedSchema";
    private static final String SETTING_MAX_TABLES     = "aiExtractionMaxTables";

    private JTextArea systemPromptArea;
    private static final String SYSTEM_PROMPT_SETTING = "aiExtractionModelSystemPrompt";
    private JTextArea subjectPromptArea;
    private static final String SETTING_SUBJECT_PROMPT = "aiExtractionSubjectTablePrompt";

    private SwingWorker<String, Void> currentWorker;
    private final AtomicReference<Runnable> abortRef = new AtomicReference<>();
    private static final ObjectMapper MAPPER = new ObjectMapper();

    public AIExtractionModelDialog(Window owner, ExtractionModelEditor editor) {
        super(owner, "AI Subsetting Assistant", ModalityType.APPLICATION_MODAL);
        this.editor = editor;
        this.dataModel = editor.dataModel;
        this.executionContext = editor.getExecutionContext();
        initUI();
        UIUtil.initComponents(this);
        pack();
        setSize(Math.max(getWidth(), 720), Math.max(getHeight(), 580));
        setLocationRelativeTo(owner);
        setVisible(true);
    }

    private void initUI() {
        setLayout(new BorderLayout(8, 8));
        ((JComponent) getContentPane()).setBorder(BorderFactory.createEmptyBorder(8, 12, 8, 12));

        // Question panel
        JPanel questionPanel = new JPanel(new BorderLayout(4, 4));
        questionPanel.add(new JLabel("Describe what data you want to extract:"), BorderLayout.NORTH);
        questionArea = new JTextArea(6, 60);
        questionArea.setLineWrap(true);
        questionArea.setWrapStyleWord(true);
        questionArea.getDocument().addDocumentListener(new DocumentListener() {
            public void insertUpdate(DocumentEvent e) { updateGenerateButton(); }
            public void removeUpdate(DocumentEvent e) { updateGenerateButton(); }
            public void changedUpdate(DocumentEvent e) {}
        });
        questionPanel.add(new JScrollPane(questionArea), BorderLayout.CENTER);

        generateButton = new JButton("Generate Extraction Model");
        generateButton.setHorizontalAlignment(SwingConstants.LEFT);
        ImageIcon aiIcon = UIUtil.readImage("/ask_ai.png");
        if (aiIcon != null) {
            generateButton.setIcon(UIUtil.scaleIcon(generateButton, aiIcon));
        }
        generateButton.setEnabled(false);
        generateButton.setToolTipText("Generate an extraction model from your description (Ctrl+Enter)");
        generateButton.addActionListener(e -> onGenerate());

        cancelButton = new JButton("Cancel");
        ImageIcon cancelIcon = UIUtil.readImage("/Cancel.png");
        if (cancelIcon != null) {
            cancelButton.setIcon(UIUtil.scaleIcon(cancelButton, cancelIcon));
        }
        cancelButton.setEnabled(false);
        cancelButton.setToolTipText("Cancel the running AI request");
        cancelButton.addActionListener(e -> {
            Runnable abort = abortRef.get();
            if (abort != null) abort.run();
            if (currentWorker != null) currentWorker.cancel(true);
        });

        statusLabel = new JLabel(" ");

        reducedSchemaBox = new JCheckBox("Reduced schema");
        reducedSchemaBox.setToolTipText("<html>Reduces the schema sent to the AI:<br>"
                + "First asks the AI for the best subject table, then traverses<br>"
                + "connected tables in breadth-first order up to the specified count.</html>");
        maxTablesField = new JTextField(String.valueOf(dataModel.getSortedTables().size()), 4);
        maxTablesField.setEnabled(false);
        maxTablesField.setToolTipText("Maximum number of tables to include in the reduced schema");
        maxTablesField.getDocument().addDocumentListener(new DocumentListener() {
            public void insertUpdate(DocumentEvent e) { updateContextEstimate(); }
            public void removeUpdate(DocumentEvent e) { updateContextEstimate(); }
            public void changedUpdate(DocumentEvent e) {}
        });
        reducedSchemaBox.addItemListener(e -> {
            maxTablesField.setEnabled(reducedSchemaBox.isSelected());
            updateContextEstimate();
        });
        omitColumnTypesBox = new JCheckBox("Omit column types");
        omitColumnTypesBox.setToolTipText("<html>Reduces the AI context size by omitting column type information<br>"
                + "from the schema description sent to the AI.<br>"
                + "Table and column names, primary keys and foreign keys are still included.</html>");

        omitColumnTypesBox.addItemListener(e -> updateContextEstimate());

        JPanel genLeft = new JPanel(new java.awt.GridBagLayout());
        {
            java.awt.GridBagConstraints gbcL = new java.awt.GridBagConstraints();
            gbcL.gridy = 0; gbcL.anchor = java.awt.GridBagConstraints.WEST;
            gbcL.insets = new Insets(0, 0, 0, 6);
            gbcL.gridx = 0; genLeft.add(generateButton, gbcL);
            gbcL.gridx = 1; genLeft.add(cancelButton, gbcL);
            gbcL.gridx = 2; gbcL.insets = new Insets(0, 0, 0, 0); genLeft.add(statusLabel, gbcL);
        }
        JPanel reducedSchemaPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 4, 0));
        reducedSchemaPanel.add(reducedSchemaBox);
        reducedSchemaPanel.add(maxTablesField);
        JPanel checkboxRow = new JPanel(new FlowLayout(FlowLayout.RIGHT, 10, 0));
        checkboxRow.add(omitColumnTypesBox);
        checkboxRow.add(reducedSchemaPanel);
        JPanel genRow = new JPanel(new BorderLayout());
        genRow.setBorder(BorderFactory.createEmptyBorder(0, 0, 4, 0));
        genRow.add(genLeft, BorderLayout.WEST);
        genRow.add(checkboxRow, BorderLayout.EAST);
        questionPanel.add(genRow, BorderLayout.SOUTH);
        
        // Proposal panel
        previewArea = new JTextArea();
        previewArea.setEditable(false);
        previewArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 12));
        previewArea.setText("Generate an extraction model to see the proposal here.");
        JScrollPane previewScroll = new JScrollPane(previewArea);
        previewScroll.setBorder(BorderFactory.createEtchedBorder());
        previewScroll.setPreferredSize(new Dimension(600, 200));

        applyButton = new JButton("Apply to Editor");
        ImageIcon okIcon = UIUtil.readImage("/buttonok.png");
        if (okIcon != null) {
            applyButton.setIcon(UIUtil.scaleIcon(applyButton, okIcon));
        }
        applyButton.setEnabled(false);
        applyButton.setToolTipText("Apply suggestion to the editor (Ctrl+Enter, undoable with Ctrl+Z)");
        applyButton.addActionListener(e -> onApply());

        JPanel applyRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
        applyRow.add(applyButton);

        JPanel previewPanel = new JPanel(new BorderLayout(4, 4));
        previewPanel.add(new JLabel("Proposal"), BorderLayout.NORTH);
        previewPanel.add(previewScroll, BorderLayout.CENTER);
        previewPanel.add(applyRow, BorderLayout.SOUTH);

        JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, questionPanel, previewPanel);
        splitPane.setResizeWeight(0.4);

        List<String> warnings = new ArrayList<>();
        if (!editor.extractionModel.additionalSubjects.isEmpty()) {
            warnings.add("⚠ Additional subjects are set — the AI assistant works with the primary subject only.");
        }
        if (editor.scriptFormat != net.sf.jailer.subsetting.ScriptFormat.SQL) {
            warnings.add("⚠ Export format is not SQL — the AI assistant is optimized for SQL.");
        }

        JPanel centerPanel = new JPanel(new BorderLayout(4, 8));
        if (!warnings.isEmpty()) {
            JLabel warningLabel = new JLabel("<html>" + String.join("<br>", warnings) + "</html>");
            warningLabel.setBorder(BorderFactory.createCompoundBorder(
                    BorderFactory.createLineBorder(Colors.Color_255_255_0.darker(), 1),
                    BorderFactory.createEmptyBorder(4, 8, 4, 8)));
            warningLabel.setBackground(Colors.Color_255_255_0);
            warningLabel.setOpaque(true);
            JPanel warningPanel = new JPanel(new BorderLayout());
            warningPanel.add(warningLabel, BorderLayout.CENTER);
            JPanel splitWithWarning = new JPanel(new BorderLayout(0, 4));
            splitWithWarning.add(warningPanel, BorderLayout.NORTH);
            splitWithWarning.add(splitPane, BorderLayout.CENTER);
            centerPanel.add(splitWithWarning, BorderLayout.CENTER);
        } else {
            centerPanel.add(splitPane, BorderLayout.CENTER);
        }
        add(centerPanel, BorderLayout.CENTER);

        // South: provider settings + buttons
        providerPanel = new AIProviderPanel();
        providerPanel.setDefaultModelOverrides(Map.of(AIProviderConfig.ProviderType.OPENROUTER, "deepseek/deepseek-r1"));
        AIProviderConfig initConfig = providerPanel.getConfig();
        omitColumnTypesBox.setSelected(AIQueryAssistant.loadOmitColumnTypes(initConfig, executionContext));
        Object savedReduced = UISettings.restore(dmKey(SETTING_REDUCED_SCHEMA));
        reducedSchemaBox.setSelected(Boolean.TRUE.equals(savedReduced));
        maxTablesField.setEnabled(reducedSchemaBox.isSelected());
        Object savedMaxTables = UISettings.restore(dmKey(SETTING_MAX_TABLES));
        if (savedMaxTables instanceof Integer) maxTablesField.setText(String.valueOf(savedMaxTables));
        updateContextEstimate();

        String savedPrompt = (String) UISettings.restore(SYSTEM_PROMPT_SETTING);
        systemPromptArea = new JTextArea(savedPrompt != null && !savedPrompt.isEmpty() ? savedPrompt : buildDefaultSystemPrompt(), 10, 60);
        systemPromptArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 11));
        systemPromptArea.setLineWrap(true);
        systemPromptArea.setWrapStyleWord(true);
        systemPromptArea.setCaretPosition(0);

        String savedSubjectPrompt = (String) UISettings.restore(SETTING_SUBJECT_PROMPT);
        subjectPromptArea = new JTextArea(savedSubjectPrompt != null && !savedSubjectPrompt.isEmpty() ? savedSubjectPrompt : buildDefaultSubjectPrompt(), 4, 60);
        subjectPromptArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 11));
        subjectPromptArea.setLineWrap(true);
        subjectPromptArea.setWrapStyleWord(true);
        subjectPromptArea.setCaretPosition(0);

        closeButton = new JButton("Close");
        ImageIcon closeIcon = UIUtil.readImage("/buttoncancel.png");
        if (closeIcon != null) {
            closeButton.setIcon(UIUtil.scaleIcon(closeButton, closeIcon));
        }
        closeButton.setToolTipText("Close this dialog");
        closeButton.addActionListener(e -> dispose());

        JButton systemPromptButton = new JButton("System Prompts...");
        ImageIcon editIcon = UIUtil.readImage("/ieditdetails_64.png");
        if (editIcon != null) {
            systemPromptButton.setIcon(UIUtil.scaleIcon(systemPromptButton, editIcon));
        }
        systemPromptButton.setToolTipText("Edit the system prompt sent to the AI");
        systemPromptButton.addActionListener(e -> openSystemPromptDialog());

        JPanel leftButtons = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 4));
        leftButtons.add(systemPromptButton);

        JPanel rightButtons = new JPanel(new FlowLayout(FlowLayout.RIGHT, 6, 4));
        rightButtons.add(closeButton);

        JPanel buttonRow = new JPanel(new BorderLayout());
        buttonRow.add(leftButtons, BorderLayout.WEST);
        buttonRow.add(rightButtons, BorderLayout.EAST);

        JPanel southPanel = new JPanel(new BorderLayout(0, 4));
        southPanel.add(providerPanel, BorderLayout.CENTER);
        southPanel.add(buttonRow, BorderLayout.SOUTH);
        add(southPanel, BorderLayout.SOUTH);

        KeyStroke ctrlEnter = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, InputEvent.CTRL_DOWN_MASK);
        getRootPane().getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(ctrlEnter, "applyModel");
        getRootPane().getActionMap().put("applyModel", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (applyButton.isEnabled()) applyButton.doClick();
                else if (generateButton.isEnabled()) generateButton.doClick();
            }
        });
    }

    private void updateGenerateButton() {
        generateButton.setEnabled(!questionArea.getText().trim().isEmpty());
    }

    private void onGenerate() {
        String question = questionArea.getText().trim();
        if (question.isEmpty()) return;

        AIProviderConfig config = providerPanel.getConfig();
        if (config.apiKey.isEmpty() && config.providerType.requiresApiKey) {
            JOptionPane.showMessageDialog(this, "Please enter an API key.", "API Key Required",
                    JOptionPane.WARNING_MESSAGE);
            return;
        }

        setGenerating(true);
        statusLabel.setText("Generating...");
        previewArea.setText("Generating...");
        applyButton.setEnabled(false);
        resultSubject = null;

        final String questionCopy = question;
        final AIProviderConfig configCopy = config;
        final boolean reduced = reducedSchemaBox.isSelected();
        final int maxTablesVal = parseMaxTables();
        final boolean omitTypes = omitColumnTypesBox.isSelected();

        currentWorker = new SwingWorker<String, Void>() {
            @Override
            protected String doInBackground() throws Exception {
                return callAI(questionCopy, configCopy, reduced, maxTablesVal, omitTypes);
            }

            @Override
            protected void done() {
                currentWorker = null;
                setGenerating(false);
                statusLabel.setText(" ");
                if (isCancelled()) return;
                try {
                    String result = get();
                    parseAndShowResult(result);
                } catch (ExecutionException ex) {
                    UIUtil.showException(AIExtractionModelDialog.this, "AI Error", ex);
                } catch (InterruptedException ex) {
                    Thread.currentThread().interrupt();
                }
            }
        };
        currentWorker.execute();
    }

    private String callAI(String question, AIProviderConfig config, boolean reducedSchema, int maxTables, boolean omitColumnTypes) throws IOException, Exception {
        String currentState = buildCurrentState();
        String systemPrompt = buildSystemPrompt();
        UISettings.store(SYSTEM_PROMPT_SETTING, systemPromptArea.getText().trim());
        Set<Table> relevantTables = null;
        if (reducedSchema && maxTables > 0) {
            SwingUtilities.invokeLater(() -> statusLabel.setText("Determining subject table..."));
            try {
                Table subjectTable = AIQueryAssistant.selectSubjectTable(question, dataModel, config, abortRef, subjectPromptArea.getText().trim());
                if (subjectTable != null && !Thread.currentThread().isInterrupted()) {
                    SwingUtilities.invokeLater(() -> statusLabel.setText("Building reduced schema..."));
                    relevantTables = selectTablesByBFS(subjectTable, maxTables);
                }
            } catch (Exception e) {
                // Fall back to full schema on error
            }
        }
        String userMessage = currentState + "Schema:\n" + buildSchemaWithAssociations(relevantTables, omitColumnTypes) + "\n\nRequest: " + question;
        int est = userMessage.length() / 4;
        Integer contextWindow = AIQueryAssistant.fetchContextWindowTokens(config);
        boolean overload = contextWindow != null && est > contextWindow * 0.6;
        String tokenInfo = est >= 1000 ? "~" + (est / 1000) + "k tokens" : "~" + est + " tokens";
        SwingUtilities.invokeLater(() -> statusLabel.setText(
                overload ? "<html><font color='orange'>⚠ " + tokenInfo + " — context window may be exceeded</font></html>"
                         : tokenInfo));
        AIQueryAssistant.saveCheckboxStates(config, executionContext, omitColumnTypes, false);
        UISettings.store(dmKey(SETTING_REDUCED_SCHEMA), reducedSchema);
        UISettings.store(dmKey(SETTING_MAX_TABLES), maxTables);
        String response = AIQueryAssistant.generateSQL(userMessage, Collections.emptyList(), dataModel,
                "SQL", config, systemPrompt, false, false, abortRef, null);
        UISettings.s19 += UISettings.s19 > 0 ? 100 : -100;
        UISettings.store("aiExtractionModelUsed", "true");
        return response;
    }

    private Set<Table> selectTablesByBFS(Table startTable, int maxTables) {
        Map<Table, Set<Table>> neighbors = new HashMap<>();
        for (Association a : dataModel.namedAssociations.values()) {
            neighbors.computeIfAbsent(a.source, k -> new LinkedHashSet<>()).add(a.destination);
            neighbors.computeIfAbsent(a.destination, k -> new LinkedHashSet<>()).add(a.source);
        }
        Set<Table> result = new LinkedHashSet<>();
        Deque<Table> queue = new ArrayDeque<>();
        result.add(startTable);
        queue.add(startTable);
        while (!queue.isEmpty() && result.size() < maxTables) {
            Table current = queue.poll();
            for (Table neighbor : neighbors.getOrDefault(current, Collections.emptySet())) {
                if (result.add(neighbor)) {
                    queue.add(neighbor);
                    if (result.size() >= maxTables) break;
                }
            }
        }
        return result;
    }

    private String buildCurrentState() {
        StringBuilder sb = new StringBuilder();
        sb.append("Current extraction model state:\n");
        String subjectName = editor.subject != null ? editor.subject.getName() : "(none)";
        sb.append("  Subject: ").append(subjectName).append("\n");
        String condition = editor.condition.getText().trim();
        sb.append("  Condition: ").append(condition.isEmpty() ? "(none)" : condition).append("\n");
        sb.append("  Active restrictions:\n");
        boolean hasRestrictions = false;
        for (Association a : dataModel.namedAssociations.values()) {
            String r = dataModel.getRestrictionModel().getRestriction(a);
            if (r != null) {
                hasRestrictions = true;
                String display = (r == net.sf.jailer.restrictionmodel.RestrictionModel.IGNORE)
                        ? "false" : r;
                sb.append("    - ").append(a.getName()).append(": ").append(display).append("\n");
            }
        }
        if (!hasRestrictions) {
            sb.append("    (none)\n");
        }
        sb.append("\n");
        return sb.toString();
    }

    private String buildSchemaWithAssociations(Set<Table> relevantTables, boolean omitColumnTypes) {
        StringBuilder sb = new StringBuilder();
        sb.append(AIQueryAssistant.buildSchemaDescription(dataModel, relevantTables, omitColumnTypes, false));
        sb.append("\nNamed associations (use exact names in the restrictions list):\n");
        for (Association a : dataModel.namedAssociations.values()) {
            if (relevantTables == null || relevantTables.contains(a.source) || relevantTables.contains(a.destination)) {
                sb.append("  - ").append(a.getName());
                sb.append(": ").append(a.source.getName());
                sb.append(" -> ").append(a.destination.getName());
                sb.append("\n");
            }
        }
        return sb.toString();
    }

    private String buildSystemPrompt() {
        String text = systemPromptArea.getText().trim();
        return text.isEmpty() ? buildDefaultSystemPrompt() : text;
    }

    private String buildDefaultSystemPrompt() {
        return "You are a Jailer extraction model assistant.\n\n"
             + "HOW JAILER EXTRACTION WORKS:\n"
             + "Jailer starts with a \"subject\" table and a WHERE condition that selects the initial rows.\n"
             + "It then automatically follows all foreign-key relationships (called \"associations\") from\n"
             + "those rows: for each association Source -> Destination, it finds all Destination rows\n"
             + "linked to already-collected Source rows via an association-specific condition, and adds those rows to\n"
             + "the result. This is repeated recursively for every newly collected row, until nothing new\n"
             + "is reachable. The result is a complete, consistent snapshot of all data connected to the\n"
             + "starting rows.\n\n"
             + "Because Jailer follows ALL associations by default, you must explicitly stop it from\n"
             + "following associations that lead to tables the user did NOT request. Use \"false\" to\n"
             + "exclude an association. If you leave an association unrestricted, Jailer will follow it\n"
             + "and pull in those rows even if the user did not ask for them.\n\n"
             + "ASSOCIATION FORMAT (in the schema below):\n"
             + "Each named association is listed as:\n"
             + "  name: Source -> Destination\n"
             + "where Source and Destination are table names. Source is alias A and Destination is alias B\n"
             + "in SQL restriction conditions.\n\n"
             + "RESTRICTIONS control which associations are followed:\n"
             + "  \"false\"  - Jailer does NOT follow this association; the connected table is excluded\n"
             + "             (unless it is also reachable via a different unrestricted association)\n"
             + "  \"<sql>\"  - Jailer follows this association but only includes rows where the SQL\n"
             + "             predicate is true (A and B are single-letter aliases matching the association\n"
             + "             direction: A is the left-hand table (before \"->\"), B is the right-hand table (after \"->\"))\n"
             + "  \"\"       - Jailer follows this association and includes all rows (same as omitting it)\n\n"
             + "WHERE TO PUT CONDITIONS:\n"
             + "  - Conditions that filter the SUBJECT TABLE (Alias \"T\") rows go into \"condition\"\n"
             + "    (e.g., \"T.CUSTOMER_ID = 42\" selects only that customer's orders)\n"
             + "  - Conditions that filter ROWS IN RELATED TABLES go into \"restrictions\" as SQL predicates\n"
             + "    (use A for the source table row, B for the destination table row)\n\n"
             + "EXAMPLE:\n"
             + "Request: \"All orders for customer 42, with order items and products, but without payment history\"\n"
             + "Response:\n"
             + "{\n"
             + "  \"subject\": \"ORDER\",\n"
             + "  \"condition\": \"T.CUSTOMER_ID = 42\",\n"
             + "  \"restrictions\": [\n"
             + "    {\"association\": \"payments\",    \"condition\": \"false\"},\n"
             + "    {\"association\": \"order_items\", \"condition\": \"\"},\n"
             + "    {\"association\": \"products\",    \"condition\": \"\"}\n"
             + "  ],\n"
             + "  \"explanation\": \"Extracts orders of customer 42. Order items and products are included. Payment history is explicitly excluded.\"\n"
             + "}\n\n"
             + "CURRENT STATE:\n"
             + "You will receive the current extraction model state before the schema.\n"
             + "Use it as context when deciding which associations to restrict.\n\n"
             + "OUTPUT: Respond with a single JSON object containing:\n"
             + "  \"subject\"      - the subject table name\n"
             + "  \"condition\"    - WHERE condition for the subject table (empty string if none)\n"
             + "  \"restrictions\" - ALL named associations from the schema, each with an\n"
             + "                   \"association\" name and a \"condition\" (\"false\", SQL predicate, or \"\")\n"
             + "  \"explanation\"  - brief description of what is extracted and what is excluded\n"
             + "Use exact association names from the schema. No markdown fences, no other text.";
    }

    private String buildDefaultSubjectPrompt() {
        return "You are a database expert. Given a list of database table names and a data extraction request, "
             + "identify the single best subject table — the main entity the user wants to extract data about. "
             + "Choose ONLY from the table names in the provided list. "
             + "Reply with ONLY that exact table name — no explanation, no schema prefix, no other text.";
    }

    private String dmKey(String base) {
        String folder = executionContext != null ? executionContext.getQualifiedDatamodelFolder() : null;
        return base + "|" + (folder != null ? folder : "");
    }

    @Override
    public void dispose() {
        AIProviderConfig config = providerPanel.getConfig();
        AIQueryAssistant.saveCheckboxStates(config, executionContext, omitColumnTypesBox.isSelected(), false);
        UISettings.store(dmKey(SETTING_REDUCED_SCHEMA), reducedSchemaBox.isSelected());
        UISettings.store(dmKey(SETTING_MAX_TABLES), parseMaxTables());
        UISettings.store(SETTING_SUBJECT_PROMPT, subjectPromptArea.getText().trim());
        super.dispose();
    }

    private void parseAndShowResult(String response) {
        response = AIQueryAssistant.stripMarkdownCodeFence(response.trim());
        int start = response.indexOf('{');
        int end = response.lastIndexOf('}');
        if (start >= 0 && end > start) {
            response = response.substring(start, end + 1);
        }
        try {
            JsonNode root = MAPPER.readTree(response);
            resultSubject = root.path("subject").asText("").trim();
            if (resultSubject.isEmpty() && editor.subject != null) {
                resultSubject = editor.subject.getName();
            }
            if (root.has("condition")) {
                resultCondition = root.path("condition").asText("").trim();
            } else {
                resultCondition = editor.condition.getText().trim();
            }
            String explanation = root.path("explanation").asText("").trim();
            resultRestrictions = new ArrayList<>();

            JsonNode restr = root.path("restrictions");
            if (restr.isArray()) {
                for (JsonNode r : restr) {
                    String name = r.path("association").asText("").trim();
                    String cond = r.path("condition").asText("").trim();
                    if (!name.isEmpty()) {
                        resultRestrictions.add(new String[]{name, cond});
                    }
                }
            }

            // Remove restrictions on dep associations (isInsertDestinationBeforeSource == true):
            // restricting these would cut off required parent rows.
            resultRestrictions.removeIf(r -> {
                Association a = dataModel.namedAssociations.get(r[0]);
                return a != null && a.isInsertDestinationBeforeSource();
            });

            // Remove restrictions where neither source nor destination is reachable
            // from the proposed subject table (such restrictions are irrelevant).
            Table subjectTable = findTable(resultSubject);
            Set<Table> closure = subjectTable != null ? subjectTable.closure() : Collections.<Table>emptySet();
            if (!closure.isEmpty()) {
                resultRestrictions.removeIf(r -> {
                    Association a = dataModel.namedAssociations.get(r[0]);
                    return a != null && !closure.contains(a.source) && !closure.contains(a.destination);
                });
            }

            StringBuilder preview = new StringBuilder();
            if (!explanation.isEmpty()) {
                preview.append(explanation).append("\n\n");
            }
            preview.append("Subject table: ").append(resultSubject).append("\n");
            preview.append("Condition:     ").append(resultCondition.isEmpty() ? "(none)" : resultCondition).append("\n");
            preview.append("\nAssociation restrictions:\n");
            if (resultRestrictions.isEmpty()) {
                preview.append("  (none - all associations use default behaviour)\n");
            } else {
                for (String[] r : resultRestrictions) {
                    String status = "false".equals(r[1]) ? "exclude" : ("remove".equals(r[1]) || r[1].isEmpty()) ? "remove restriction" : "restrict: " + r[1];
                    preview.append("  ").append(r[0]).append(": ").append(status).append("\n");
                }
            }

            previewArea.setText(preview.toString());
            previewArea.setCaretPosition(0);

            if (findTable(resultSubject) == null) {
                previewArea.append("\nWARNING: Table '" + resultSubject + "' not found in the data model!");
                applyButton.setEnabled(false);
            } else {
                applyButton.setEnabled(true);
            }
        } catch (Exception ex) {
            previewArea.setText("Could not parse AI response:\n\n" + response + "\n\nError: " + ex.getMessage());
            applyButton.setEnabled(false);
        }
    }

    private Table findTable(String name) {
        if (name == null || name.isEmpty()) return null;
        Table t = dataModel.getTable(name);
        if (t == null) t = dataModel.getTableByDisplayName(name);
        return t;
    }

    private void onApply() {
        Table subject = findTable(resultSubject);
        if (subject == null) {
            JOptionPane.showMessageDialog(this, "Table '" + resultSubject + "' not found in data model.",
                    "Error", JOptionPane.ERROR_MESSAGE);
            return;
        }

        editor.beginAIOperation();

        // Set subject table via the combo box (triggers the item listener -> changeSubject)
        editor.subjectTable.setSelectedItem(dataModel.getDisplayName(subject));

        // Set WHERE condition
        editor.setSubjectCondition(resultCondition);

        // Apply association restrictions (delta: only changed entries, rest keeps current state)
        for (String[] r : resultRestrictions) {
            Association assoc = dataModel.namedAssociations.get(r[0]);
            if (assoc != null) {
                if ("false".equals(r[1])) {
                    editor.ignorAssociation(assoc);
                } else if (r[1].isEmpty() || "remove".equals(r[1])) {
                    editor.removeRestriction(assoc);
                } else {
                    dataModel.getRestrictionModel().addRestriction(assoc, r[1], "AI", true, new HashMap<>());
                    editor.afterAddRestriction();
                }
            }
        }

        editor.markDirty();
        dispose();
    }

    private void openSystemPromptDialog() {
        JDialog d = new JDialog(this, "System Prompts", true);
        ((JComponent) d.getContentPane()).setBorder(BorderFactory.createEmptyBorder(8, 8, 0, 8));
        d.getContentPane().setLayout(new BorderLayout(0, 4));

        final String originalText = systemPromptArea.getText();
        final String originalSubjectText = subjectPromptArea.getText();
        d.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                systemPromptArea.setText(originalText);
                systemPromptArea.setCaretPosition(0);
                subjectPromptArea.setText(originalSubjectText);
                subjectPromptArea.setCaretPosition(0);
            }
        });

        JButton okButton = new JButton("OK");
        ImageIcon okIcon = UIUtil.readImage("/buttonok.png");
        if (okIcon != null) {
            okButton.setIcon(UIUtil.scaleIcon(okButton, okIcon));
        }
        okButton.addActionListener(e -> {
            UISettings.store(SYSTEM_PROMPT_SETTING, systemPromptArea.getText().trim());
            UISettings.store(SETTING_SUBJECT_PROMPT, subjectPromptArea.getText().trim());
            d.dispose();
        });

        JPanel bottom = new JPanel(new FlowLayout(FlowLayout.RIGHT, 6, 4));
        bottom.add(okButton);

        // Main system prompt section
        JLabel mainLabel = new JLabel("Extraction Model Prompt:");
        mainLabel.setFont(mainLabel.getFont().deriveFont(java.awt.Font.BOLD));
        JButton resetMain = new JButton("Reset to Default");
        ImageIcon resetIcon = UIUtil.readImage("/reset_64.png");
        if (resetIcon != null) {
            resetMain.setIcon(UIUtil.scaleIcon(resetMain, resetIcon));
        }
        resetMain.addActionListener(e -> { systemPromptArea.setText(buildDefaultSystemPrompt()); systemPromptArea.setCaretPosition(0); });
        JPanel mainHeader = new JPanel(new BorderLayout(8, 0));
        mainHeader.add(mainLabel, BorderLayout.WEST);
        mainHeader.add(resetMain, BorderLayout.EAST);

        JScrollPane mainScroll = new JScrollPane(systemPromptArea);
        mainScroll.setPreferredSize(new Dimension(700, 300));

        // Subject table detection prompt section
        JLabel subjectLabel = new JLabel("Subject Table Detection (1st pass):");
        subjectLabel.setFont(subjectLabel.getFont().deriveFont(java.awt.Font.BOLD));
        JButton resetSubject = new JButton("Reset to Default");
        if (resetIcon != null) {
            resetSubject.setIcon(UIUtil.scaleIcon(resetSubject, resetIcon));
        }
        resetSubject.addActionListener(e -> { subjectPromptArea.setText(buildDefaultSubjectPrompt()); subjectPromptArea.setCaretPosition(0); });
        JPanel subjectHeader = new JPanel(new BorderLayout(8, 0));
        subjectHeader.add(subjectLabel, BorderLayout.WEST);
        subjectHeader.add(resetSubject, BorderLayout.EAST);

        JScrollPane subjectScroll = new JScrollPane(subjectPromptArea);
        subjectScroll.setPreferredSize(new Dimension(700, 100));

        JPanel center = new JPanel(new BorderLayout(0, 6));
        JPanel mainSection = new JPanel(new BorderLayout(0, 2));
        mainSection.add(mainHeader, BorderLayout.NORTH);
        mainSection.add(mainScroll, BorderLayout.CENTER);
        JPanel subjectSection = new JPanel(new BorderLayout(0, 2));
        subjectSection.add(subjectHeader, BorderLayout.NORTH);
        subjectSection.add(subjectScroll, BorderLayout.CENTER);
        center.add(mainSection, BorderLayout.CENTER);
        center.add(subjectSection, BorderLayout.SOUTH);

        d.getContentPane().add(center, BorderLayout.CENTER);
        d.getContentPane().add(bottom, BorderLayout.SOUTH);
        d.pack();
        d.setLocationRelativeTo(this);
        d.setVisible(true);
    }

    private void setGenerating(boolean generating) {
        generateButton.setEnabled(!generating && !questionArea.getText().trim().isEmpty());
        cancelButton.setEnabled(generating);
        providerPanel.setEnabled(!generating);
        questionArea.setEnabled(!generating);
        reducedSchemaBox.setEnabled(!generating);
        maxTablesField.setEnabled(!generating && reducedSchemaBox.isSelected());
        omitColumnTypesBox.setEnabled(!generating);
        setCursor(generating ? Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR) : Cursor.getDefaultCursor());
    }

    private void updateContextEstimate() {
        if (reducedSchemaBox.isSelected()) {
            int tableCount = dataModel.getSortedTables().size();
            try {
                int raw = Integer.parseInt(maxTablesField.getText().trim());
                if (raw > tableCount) {
                    SwingUtilities.invokeLater(() -> maxTablesField.setText(String.valueOf(tableCount)));
                }
            } catch (NumberFormatException ignored) {}
        }
    }

    private int parseMaxTables() {
        try {
            int v = Integer.parseInt(maxTablesField.getText().trim());
            return Math.min(v, dataModel.getSortedTables().size());
        } catch (NumberFormatException e) {
            return 0;
        }
    }
}
