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
package net.sf.jailer.ui.ai.mcp;

import java.awt.BorderLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Window;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;

/**
 * Modal editor for a single {@link McpServerConfig} entry, used by {@link McpServerConfigDialog}.
 */
class ServerEditorDialog extends JDialog {

    private static final long serialVersionUID = 1L;

    private final JTextField nameField = new JTextField(24);
    private final JTextField commandField = new JTextField(24);
    private final JTextField argsField = new JTextField(24);
    private final JTextArea envArea = new JTextArea(4, 24);
    private final JCheckBox enabledBox = new JCheckBox("Enabled");
    private McpServerConfig result;

    private ServerEditorDialog(Window owner, McpServerConfig existing) {
        super(owner, existing == null ? "Add MCP Server" : "Edit MCP Server", ModalityType.APPLICATION_MODAL);
        JPanel form = new JPanel(new GridBagLayout());
        GridBagConstraints lc = new GridBagConstraints();
        lc.anchor = GridBagConstraints.WEST;
        lc.insets = new Insets(4, 4, 4, 4);
        GridBagConstraints fc = new GridBagConstraints();
        fc.fill = GridBagConstraints.HORIZONTAL;
        fc.weightx = 1.0;
        fc.insets = new Insets(4, 4, 4, 4);

        lc.gridx = 0; lc.gridy = 0; form.add(new JLabel("Name"), lc);
        fc.gridx = 1; fc.gridy = 0; form.add(nameField, fc);

        lc.gridx = 0; lc.gridy = 1; form.add(new JLabel("Command"), lc);
        fc.gridx = 1; fc.gridy = 1; form.add(commandField, fc);
        commandField.setToolTipText("Executable to launch, e.g. \"npx\" or \"python\"");

        lc.gridx = 0; lc.gridy = 2; form.add(new JLabel("Arguments"), lc);
        fc.gridx = 1; fc.gridy = 2; form.add(argsField, fc);
        argsField.setToolTipText("<html>Space-separated arguments, e.g.<br>"
                + "-y @modelcontextprotocol/server-filesystem /path/to/dir</html>");

        lc.gridx = 0; lc.gridy = 3; lc.anchor = GridBagConstraints.NORTHWEST; form.add(new JLabel("Environment"), lc);
        fc.gridx = 1; fc.gridy = 3; fc.fill = GridBagConstraints.BOTH; fc.weighty = 1.0;
        envArea.setToolTipText("One KEY=VALUE pair per line");
        form.add(new JScrollPane(envArea), fc);

        lc.gridx = 0; lc.gridy = 4; lc.anchor = GridBagConstraints.WEST; form.add(new JLabel(""), lc);
        fc.gridx = 1; fc.gridy = 4; fc.fill = GridBagConstraints.NONE; fc.weighty = 0; form.add(enabledBox, fc);

        if (existing != null) {
            nameField.setText(existing.name);
            commandField.setText(existing.command);
            argsField.setText(String.join(" ", existing.args));
            StringBuilder envText = new StringBuilder();
            for (Map.Entry<String, String> e : existing.env.entrySet()) {
                envText.append(e.getKey()).append("=").append(e.getValue()).append("\n");
            }
            envArea.setText(envText.toString());
            enabledBox.setSelected(existing.enabled);
        } else {
            enabledBox.setSelected(true);
        }

        JButton okButton = new JButton("OK");
        okButton.addActionListener(e -> onOk(existing));
        JButton cancelButton = new JButton("Cancel");
        cancelButton.addActionListener(e -> dispose());
        JPanel buttons = new JPanel();
        buttons.add(okButton);
        buttons.add(cancelButton);

        setLayout(new BorderLayout(8, 8));
        add(form, BorderLayout.CENTER);
        add(buttons, BorderLayout.SOUTH);
        pack();
        setLocationRelativeTo(owner);
    }

    private void onOk(McpServerConfig existing) {
        String name = nameField.getText().trim();
        String command = commandField.getText().trim();
        if (name.isEmpty() || command.isEmpty()) {
            JOptionPane.showMessageDialog(this, "Name and Command are required.", "Input Required", JOptionPane.WARNING_MESSAGE);
            return;
        }
        List<String> args = new ArrayList<>();
        for (String token : argsField.getText().trim().split("\\s+")) {
            if (!token.isEmpty()) {
                args.add(token);
            }
        }
        Map<String, String> env = new LinkedHashMap<>();
        for (String line : envArea.getText().split("\\r?\\n")) {
            int eq = line.indexOf('=');
            if (eq > 0) {
                env.put(line.substring(0, eq).trim(), line.substring(eq + 1).trim());
            }
        }
        String id = existing != null ? existing.id : UUID.randomUUID().toString();
        result = new McpServerConfig(id, name, command, args, env, enabledBox.isSelected());
        dispose();
    }

    /** Shows the editor modally and returns the created/edited config, or {@code null} if cancelled. */
    static McpServerConfig showDialog(Window owner, McpServerConfig existing) {
        ServerEditorDialog dialog = new ServerEditorDialog(owner, existing);
        dialog.setVisible(true);
        return dialog.result;
    }
}
