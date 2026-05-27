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

import java.awt.BorderLayout;
import java.awt.Font;

import javax.swing.BorderFactory;
import javax.swing.border.TitledBorder;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.util.UISettings;

/**
 * Reusable Swing panel for editing the AI system prompt templates.
 * The main template may contain the placeholders {@code {schema}} and {@code {dbmsName}};
 * the first-pass (table-selection) template supports {@code {dbmsName}}.
 * Both templates are loaded from and saved to {@link UISettings}.
 */
public class SystemPromptPanel extends JPanel {

    private static final long serialVersionUID = 1L;

    static final String SETTING_SYSTEM_PROMPT = "aiSystemPrompt";
    static final String SETTING_FIRST_PASS_SYSTEM_PROMPT = "aiFirstPassSystemPrompt";
    static final String SETTING_ADVISOR_SYSTEM_PROMPT = "aiAdvisorSystemPrompt";

    /** Separator line the advisor AI must emit between the SQL result and the explanation. */
    public static final String ADVISOR_SQL_ANSWER_SEPARATOR = "-- END OF SQL";

    /** Default template for SQL generation (second AI call). */
    public static final String DEFAULT_TEMPLATE =
        "You are a SQL expert for {dbmsName}.\n"
        + "Database schema: {schema}\n"
        + "Return ONLY raw SQL - no explanation, no code fences, no trailing semicolon. "
        + "Use only tables and columns from the schema above.\n"
        + "\n"
        + "Use aliases for complex expressions in select clauses.\n"
        + "Aliases will use the same uppercase convention as all other identifiers.\n";

    /** Default template for the SQL Advisor (analysis / optimization). */
    public static final String DEFAULT_ADVISOR_TEMPLATE =
        "You are a SQL expert for {dbmsName}.\n"
        + "Database schema: {schema}\n"
        + "SQL to analyze or optimize:\n{SQL}\n"
        + "Analyze, explain, or optimize the SQL query provided by the user.\n"
        + "First output only the resulting SQL - no code fences, no trailing semicolon.\n"
        + "Then output a line containing exactly: \"{separator}\"\n"
        + "Then provide a plain-text explanation of the changes or analysis.\n"
        + "Use only tables and columns from the schema above.\n"
        + "\n"
        + "Use aliases for complex expressions in select clauses.\n"
        + "Aliases will use the same uppercase convention as all other identifiers.";

    /** Default template for table selection (first AI call / smart selection). */
    public static final String DEFAULT_FIRST_PASS_TEMPLATE =
        "You are a SQL expert for {dbmsName}.\n"
        + "Given a list of database table names and a natural-language question, "
        + "respond with ONLY the table names needed to answer the question. "
        + "One table name per line. No explanation, no other text.";

    private final JTextArea promptArea;
    private final JTextArea firstPassPromptArea;
    private final JTextArea advisorPromptArea;

    public SystemPromptPanel() {
        super(new BorderLayout(4, 4));

        String saved = (String) UISettings.restore(SETTING_SYSTEM_PROMPT);
        String initial = (saved != null && !saved.isEmpty()) ? saved : DEFAULT_TEMPLATE;

        String savedFP = (String) UISettings.restore(SETTING_FIRST_PASS_SYSTEM_PROMPT);
        String initialFP = (savedFP != null && !savedFP.isEmpty()) ? savedFP : DEFAULT_FIRST_PASS_TEMPLATE;

        String savedAdv = (String) UISettings.restore(SETTING_ADVISOR_SYSTEM_PROMPT);
        String initialAdv = (savedAdv != null && !savedAdv.isEmpty()) ? savedAdv : DEFAULT_ADVISOR_TEMPLATE;

        promptArea = new JTextArea(initial, 5, 60);
        promptArea.setLineWrap(true);
        promptArea.setWrapStyleWord(true);
        promptArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 11));

        firstPassPromptArea = new JTextArea(initialFP, 4, 60);
        firstPassPromptArea.setLineWrap(true);
        firstPassPromptArea.setWrapStyleWord(true);
        firstPassPromptArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 11));

        advisorPromptArea = new JTextArea(initialAdv, 5, 60);
        advisorPromptArea.setLineWrap(true);
        advisorPromptArea.setWrapStyleWord(true);
        advisorPromptArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 11));

        JPanel mainSection = buildSection(
            "SQL Generation",
            promptArea,
            "Placeholders: {schema}, {dbmsName}",
            DEFAULT_TEMPLATE);

        JPanel advisorSection = buildSection(
            "SQL Advisor",
            advisorPromptArea,
            "Placeholders: {schema}, {dbmsName}, {separator}, {SQL}",
            DEFAULT_ADVISOR_TEMPLATE);

        JPanel firstPassSection = buildSection(
            "Table Selection (Relevant Tables Only)",
            firstPassPromptArea,
            "Placeholder: {dbmsName}",
            DEFAULT_FIRST_PASS_TEMPLATE);

        advisorSection.setBorder(BorderFactory.createCompoundBorder(
            BorderFactory.createEmptyBorder(20, 0, 0, 0),
            advisorSection.getBorder()));

        firstPassSection.setBorder(BorderFactory.createCompoundBorder(
            BorderFactory.createEmptyBorder(20, 0, 0, 0),
            firstPassSection.getBorder()));

        JPanel both = new JPanel();
        both.setLayout(new BoxLayout(both, BoxLayout.Y_AXIS));
        both.add(mainSection);
        both.add(advisorSection);
        both.add(firstPassSection);

        add(both, BorderLayout.CENTER);
    }

    private JPanel buildSection(String title, JTextArea area, String hintText, String defaultText) {
        JPanel section = new JPanel(new BorderLayout(4, 4));
        TitledBorder tb = BorderFactory.createTitledBorder(title);
        tb.setTitleFont(section.getFont().deriveFont(Font.BOLD));
        section.setBorder(tb);

        JLabel hint = new JLabel(hintText);
        hint.setFont(hint.getFont().deriveFont(hint.getFont().getSize2D() - 1f));

        JButton resetButton = new JButton("Reset to Default");
        ImageIcon resetIcon = UIUtil.readImage("/reset_64.png");
        if (resetIcon != null) {
            resetButton.setIcon(UIUtil.scaleIcon(resetButton, resetIcon));
        }
        resetButton.addActionListener(e -> area.setText(defaultText));

        JPanel bottomRow = new JPanel(new BorderLayout(6, 0));
        bottomRow.add(hint, BorderLayout.WEST);
        bottomRow.add(resetButton, BorderLayout.EAST);

        section.add(new JScrollPane(area), BorderLayout.CENTER);
        section.add(bottomRow, BorderLayout.SOUTH);
        return section;
    }

    /**
     * Returns the current SQL-generation template, or {@code null} if empty.
     */
    public String getTemplate() {
        String text = promptArea.getText().trim();
        return text.isEmpty() ? null : text;
    }

    /**
     * Returns the current SQL Advisor template, or {@code null} if empty.
     */
    public String getAdvisorTemplate() {
        String text = advisorPromptArea.getText().trim();
        return text.isEmpty() ? null : text;
    }

    /**
     * Returns the current table-selection (first-pass) template, or {@code null} if empty.
     */
    public String getFirstPassTemplate() {
        String text = firstPassPromptArea.getText().trim();
        return text.isEmpty() ? null : text;
    }

    /** Persists all templates to {@link UISettings}. */
    public void saveSettings() {
        UISettings.store(SETTING_SYSTEM_PROMPT, promptArea.getText().trim());
        UISettings.store(SETTING_ADVISOR_SYSTEM_PROMPT, advisorPromptArea.getText().trim());
        UISettings.store(SETTING_FIRST_PASS_SYSTEM_PROMPT, firstPassPromptArea.getText().trim());
    }
}
