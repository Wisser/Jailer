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
import java.awt.FlowLayout;
import java.awt.Font;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.util.UISettings;

/**
 * Reusable Swing panel for editing the AI system prompt template.
 * The template may contain the placeholders {@code {schema}} and {@code {dbmsName}},
 * which are replaced at request time. Loads from and saves to {@link UISettings}.
 */
public class SystemPromptPanel extends JPanel {

    private static final long serialVersionUID = 1L;

    static final String SETTING_SYSTEM_PROMPT = "aiSystemPrompt";

    /** Default template that mirrors the hard-coded prompt used before this panel existed. */
    public static final String DEFAULT_TEMPLATE =
        "You are a SQL expert for {dbmsName}.\n"
        + "Database schema: {schema}\n"
        + "Return ONLY raw SQL - no explanation, no code fences, no trailing semicolon. "
        + "Use only tables and columns from the schema above.";

    private final JTextArea promptArea;

    public SystemPromptPanel() {
        super(new BorderLayout(4, 4));

        String saved = (String) UISettings.restore(SETTING_SYSTEM_PROMPT);
        String initial = (saved != null && !saved.isEmpty()) ? saved : DEFAULT_TEMPLATE;

        promptArea = new JTextArea(initial, 5, 60);
        promptArea.setLineWrap(true);
        promptArea.setWrapStyleWord(true);
        promptArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 11));

        JLabel hint = new JLabel("Placeholders: {schema}, {dbmsName}");
        hint.setFont(hint.getFont().deriveFont(hint.getFont().getSize2D() - 1f));

        JButton resetButton = new JButton("Reset to Default");
        ImageIcon resetIcon = UIUtil.readImage("/reset_64.png");
        if (resetIcon != null) {
            resetButton.setIcon(UIUtil.scaleIcon(resetButton, resetIcon));
        }
        resetButton.addActionListener(e -> promptArea.setText(DEFAULT_TEMPLATE));

        JPanel bottomRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        bottomRow.add(hint);
        bottomRow.add(resetButton);

        add(new JScrollPane(promptArea), BorderLayout.CENTER);
        add(bottomRow, BorderLayout.SOUTH);
    }

    /**
     * Returns the current template text, or {@code null} if it equals the default
     * (so callers can skip passing it and rely on the built-in default).
     */
    public String getTemplate() {
        String text = promptArea.getText().trim();
        return text.isEmpty() ? null : text;
    }

    /** Persists the template to {@link UISettings}. */
    public void saveSettings() {
        UISettings.store(SETTING_SYSTEM_PROMPT, promptArea.getText().trim());
    }
}
