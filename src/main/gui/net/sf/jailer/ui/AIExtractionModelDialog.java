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
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.atomic.AtomicReference;

import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.KeyStroke;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.SwingWorker;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.ai.AIProviderConfig;
import net.sf.jailer.ui.ai.AIProviderPanel;
import net.sf.jailer.ui.ai.AIQueryAssistant;
import net.sf.jailer.ui.ai.ConversationMessage;
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

    private JTextArea systemPromptArea;
    private static final String SYSTEM_PROMPT_SETTING = "aiExtractionModelSystemPrompt";

    private final List<ConversationMessage> conversationHistory = new ArrayList<>();

    private SwingWorker<String, Void> currentWorker;
    private final AtomicReference<Runnable> abortRef = new AtomicReference<>();
    private static final ObjectMapper MAPPER = new ObjectMapper();

    public AIExtractionModelDialog(Window owner, ExtractionModelEditor editor) {
        super(owner, "AI Extraction Model Assistant", ModalityType.APPLICATION_MODAL);
        this.editor = editor;
        this.dataModel = editor.dataModel;
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
        ImageIcon aiIcon = UIUtil.readImage("/ask_ai.png");
        if (aiIcon != null) {
            generateButton.setIcon(UIUtil.scaleIcon(generateButton, aiIcon));
        }
        generateButton.setEnabled(false);
        generateButton.setToolTipText("Generate extraction model (Ctrl+Enter)");
        generateButton.addActionListener(e -> onGenerate());

        cancelButton = new JButton("Cancel");
        ImageIcon cancelIcon = UIUtil.readImage("/Cancel.png");
        if (cancelIcon != null) {
            cancelButton.setIcon(UIUtil.scaleIcon(cancelButton, cancelIcon));
        }
        cancelButton.setEnabled(false);
        cancelButton.addActionListener(e -> {
            Runnable abort = abortRef.get();
            if (abort != null) abort.run();
            if (currentWorker != null) currentWorker.cancel(true);
        });

        statusLabel = new JLabel(" ");
        JPanel genRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        genRow.add(generateButton);
        genRow.add(cancelButton);
        genRow.add(statusLabel);
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

        JPanel applyRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 4));
        applyRow.add(applyButton);

        JPanel previewPanel = new JPanel(new BorderLayout(4, 4));
        previewPanel.add(new JLabel("Proposal"), BorderLayout.NORTH);
        previewPanel.add(previewScroll, BorderLayout.CENTER);
        previewPanel.add(applyRow, BorderLayout.SOUTH);

        JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, questionPanel, previewPanel);
        splitPane.setResizeWeight(0.4);
        add(splitPane, BorderLayout.CENTER);

        // South: provider settings + buttons
        providerPanel = new AIProviderPanel();

        String savedPrompt = (String) UISettings.restore(SYSTEM_PROMPT_SETTING);
        systemPromptArea = new JTextArea(savedPrompt != null && !savedPrompt.isEmpty() ? savedPrompt : buildDefaultSystemPrompt(), 10, 60);
        systemPromptArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 11));
        systemPromptArea.setLineWrap(true);
        systemPromptArea.setWrapStyleWord(true);

        closeButton = new JButton("Close");
        ImageIcon closeIcon = UIUtil.readImage("/buttoncancel.png");
        if (closeIcon != null) {
            closeButton.setIcon(UIUtil.scaleIcon(closeButton, closeIcon));
        }
        closeButton.addActionListener(e -> dispose());

        JButton systemPromptButton = new JButton("System Prompt...");
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
        applyButton.setEnabled(false);
        resultSubject = null;

        final String questionCopy = question;
        final AIProviderConfig configCopy = config;

        currentWorker = new SwingWorker<String, Void>() {
            @Override
            protected String doInBackground() throws Exception {
                return callAI(questionCopy, configCopy);
            }

            @Override
            protected void done() {
                currentWorker = null;
                setGenerating(false);
                statusLabel.setText(" ");
                if (isCancelled()) return;
                try {
                    String result = get();
                    questionArea.setText("");
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

    private String callAI(String question, AIProviderConfig config) throws IOException, Exception {
        String currentState = buildCurrentState();
        String systemPrompt = buildSystemPrompt();
        UISettings.store(SYSTEM_PROMPT_SETTING, systemPromptArea.getText().trim());
        String userMessage = conversationHistory.isEmpty()
                ? currentState + "Schema:\n" + buildSchemaWithAssociations() + "\n\nRequest: " + question
                : currentState + "Request: " + question;
        String response = AIQueryAssistant.generateSQL(userMessage, conversationHistory, dataModel,
                "SQL", config, systemPrompt, false, true, abortRef, null);
        conversationHistory.add(new ConversationMessage("user", userMessage));
        conversationHistory.add(new ConversationMessage("assistant", response));
        return response;
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

    private String buildSchemaWithAssociations() {
        StringBuilder sb = new StringBuilder();
        sb.append(AIQueryAssistant.buildSchemaDescription(dataModel, null, false, false));
        sb.append("\nNamed associations (use exact names in the restrictions list):\n");
        for (Association a : dataModel.namedAssociations.values()) {
            sb.append("  - ").append(a.getName());
            sb.append(": ").append(a.source.getName());
            sb.append(" -> ").append(a.destination.getName());
            sb.append("\n");
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
             + "The database schema is a directed graph G where each table is a node and each\n"
             + "association (foreign-key link) is a directed edge. Given a subject node s and\n"
             + "a WHERE condition, Jailer collects all rows in the subgraph reachable from s\n"
             + "by following active edges transitively.\n\n"
             + "RESTRICTIONS control which edges are active:\n"
             + "  \"false\"  - deactivates this edge; a destination node becomes unreachable only\n"
             + "             if ALL paths from s to it pass through this edge\n"
             + "  \"<sql>\"  - edge stays active, but only rows matching the SQL predicate are followed\n"
             + "  omitted  - edge active, no filter (default)\n"
             + "  \"\"       - edge explicitly active (same as omitted, used to override a delta)\n\n"
             + "EXAMPLE:\n"
             + "Request: \"All orders for customer 42, with order items and products, but without payment history\"\n"
             + "{\n"
             + "  \"subject\": \"ORDER\",\n"
             + "  \"condition\": \"T.CUSTOMER_ID = 42\",\n"
             + "  \"restrictions\": [\n"
             + "    {\"association\": \"payments\", \"condition\": \"false\"},\n"
             + "    {\"association\": \"order_items\", \"condition\": \"\"},\n"
             + "    {\"association\": \"products\", \"condition\": \"\"}\n"
             + "  ],\n"
             + "  \"explanation\": \"Extracts orders of customer 42 with items and products, payment history excluded.\"\n"
             + "}\n\n"
             + "CURRENT STATE:\n"
             + "You will receive the current extraction model state before the schema.\n"
             + "Use it as context when deciding which edges to restrict.\n\n"
             + "OUTPUT: Return ALL named associations from the schema in the restrictions list.\n"
             + "For each association set condition:\n"
             + "  \"false\"  - to deactivate the edge\n"
             + "  \"<sql>\"  - to filter rows (SQL predicate on the destination table)\n"
             + "  \"\"       - to leave the edge active (default, no restriction)\n\n"
             + "Respond ONLY with a single JSON object in the same format, no markdown fences, no other text.\n"
             + "Use exact association names from the schema.";
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
        JDialog d = new JDialog(this, "System Prompt", true);
        ((JComponent) d.getContentPane()).setBorder(BorderFactory.createEmptyBorder(8, 8, 0, 8));
        d.getContentPane().setLayout(new BorderLayout(0, 4));

        JButton resetButton = new JButton("Reset to Default");
        ImageIcon resetIcon = UIUtil.readImage("/reset_64.png");
        if (resetIcon != null) {
            resetButton.setIcon(UIUtil.scaleIcon(resetButton, resetIcon));
        }
        resetButton.addActionListener(e -> systemPromptArea.setText(buildDefaultSystemPrompt()));

        JButton okButton = new JButton("OK");
        ImageIcon okIcon = UIUtil.readImage("/buttonok.png");
        if (okIcon != null) {
            okButton.setIcon(UIUtil.scaleIcon(okButton, okIcon));
        }
        okButton.addActionListener(e -> {
            UISettings.store(SYSTEM_PROMPT_SETTING, systemPromptArea.getText().trim());
            d.dispose();
        });

        JPanel bottom = new JPanel(new BorderLayout());
        JPanel leftBottom = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 4));
        leftBottom.add(resetButton);
        JPanel rightBottom = new JPanel(new FlowLayout(FlowLayout.RIGHT, 6, 4));
        rightBottom.add(okButton);
        bottom.add(leftBottom, BorderLayout.WEST);
        bottom.add(rightBottom, BorderLayout.EAST);

        JScrollPane scroll = new JScrollPane(systemPromptArea);
        scroll.setPreferredSize(new Dimension(700, 360));
        d.getContentPane().add(scroll, BorderLayout.CENTER);
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
        setCursor(generating ? Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR) : Cursor.getDefaultCursor());
    }
}

// TODO
// TODO Benchmark: kunden mit ihren ausleihen, die in Boston wohnen
// TODO limitierung: pass 1: nach subjekten fragen, pass 2: bis zu einem vom Nutzer vorgegebenen Anzahl Tabellen von subjekten ausgehen in Breitensuche sammeln
// TODO System prompt: 
// erkl�ren, wann einschr�nkungen auf association notwendig sind und wie man sie formuliert
// erkl�ren, dass Bedingungen, die direkt auf das Subjekt gehen, in "condition" kommen, alle anderen Einschr�nkungen auf Assoziationen
// fordern, dass alle Tabellen, ausgeschlossen werden sollen, die nicht explizit angefordert werden und auch nicht auf dem Pfad zu den angeforderten Tabellen liegen, mit "false" eingeschr�nkt werden m�ssen, damit sie nicht automatisch mitgenommen werden (z.B. payment history im Beispiel)


// TODO ? im systemprompt fordern, dass m�glichst vieles "false" ist?

// TODO warnung wenn bei �ffnen des Dialogs schon ein additional Subjekt gesetzt ist
// TODO warnung wenn format != SQL

// TODO wenn API key vorhanden, AI Assistant in wizzard anbieten.

// TODO
// TODO UNDO: bessere Bezeichnung der Compensating Action

// TODO
//Aktuell sendet der Dialog nur eine einzige user-Message pro Request (keine History) → Reihenfolge noch irrelevant.
//
//Wenn Conversation-History ergänzt wird (geplanter "stateful"-Ausbau), muss die Reihenfolge korrekt aufgebaut werden — analog zu AIQueryDialog, das List<ConversationMessage> in der richtigen Reihenfolge an
//AIQueryAssistant.generateSQL() übergibt.
