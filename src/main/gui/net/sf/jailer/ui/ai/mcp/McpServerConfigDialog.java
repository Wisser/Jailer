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
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Window;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.SwingWorker;
import javax.swing.UIManager;
import javax.swing.table.AbstractTableModel;

/**
 * Settings dialog for configuring the external MCP (Model Context Protocol) stdio servers the AI
 * Assistant may call tools on. Loads from and saves to {@link McpServerSettings}.
 */
public class McpServerConfigDialog extends JDialog {

    private static final long serialVersionUID = 1L;
    private static final long TEST_TIMEOUT_MILLIS = 15000;

    private final List<McpServerConfig> servers;
    private final ServerTableModel tableModel;
    private final JTable table;
    private final JLabel testStatusLabel = new JLabel(" ");

    private static class ServerTableModel extends AbstractTableModel {
        private static final long serialVersionUID = 1L;
        private static final String[] COLUMNS = { "Enabled", "Name", "Command", "Arguments" };
        final List<McpServerConfig> rows;

        ServerTableModel(List<McpServerConfig> rows) {
            this.rows = rows;
        }

        @Override
        public int getRowCount() {
            return rows.size();
        }

        @Override
        public int getColumnCount() {
            return COLUMNS.length;
        }

        @Override
        public String getColumnName(int col) {
            return COLUMNS[col];
        }

        @Override
        public Class<?> getColumnClass(int col) {
            return col == 0 ? Boolean.class : String.class;
        }

        @Override
        public boolean isCellEditable(int row, int col) {
            return col == 0;
        }

        @Override
        public Object getValueAt(int row, int col) {
            McpServerConfig c = rows.get(row);
            switch (col) {
                case 0: return c.enabled;
                case 1: return c.name;
                case 2: return c.command;
                case 3: return String.join(" ", c.args);
                default: return "";
            }
        }

        @Override
        public void setValueAt(Object value, int row, int col) {
            if (col == 0) {
                McpServerConfig c = rows.get(row);
                rows.set(row, new McpServerConfig(c.id, c.name, c.command, c.args, c.env, (Boolean) value));
                fireTableRowsUpdated(row, row);
            }
        }
    }

    public McpServerConfigDialog(Window owner) {
        super(owner, "MCP Servers", ModalityType.APPLICATION_MODAL);
        this.servers = new ArrayList<>(McpServerSettings.load());
        this.tableModel = new ServerTableModel(servers);
        this.table = new JTable(tableModel);
        table.setRowHeight(22);
        table.getColumnModel().getColumn(0).setMaxWidth(60);

        setLayout(new BorderLayout(8, 8));
        ((JComponent) getContentPane()).setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));

        JLabel introLabel = new JLabel("<html>Configure external MCP servers the AI Assistant may call tools on.<br>"
                + "Enabled servers are started (as a local subprocess) on first use.</html>");
        add(introLabel, BorderLayout.NORTH);
        add(new JScrollPane(table), BorderLayout.CENTER);

        JButton addButton = new JButton("Add...");
        JButton editButton = new JButton("Edit...");
        JButton removeButton = new JButton("Remove");
        JButton testButton = new JButton("Test");
        addButton.addActionListener(e -> onAdd());
        editButton.addActionListener(e -> onEdit());
        removeButton.addActionListener(e -> onRemove());
        testButton.addActionListener(e -> onTest());

        JPanel leftButtons = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 4));
        leftButtons.add(addButton);
        leftButtons.add(editButton);
        leftButtons.add(removeButton);
        leftButtons.add(testButton);
        leftButtons.add(testStatusLabel);

        JButton closeButton = new JButton("Close");
        closeButton.addActionListener(e -> {
            McpServerSettings.save(servers);
            dispose();
        });
        JPanel rightButtons = new JPanel(new FlowLayout(FlowLayout.RIGHT, 6, 4));
        rightButtons.add(closeButton);

        JPanel buttonRow = new JPanel(new BorderLayout());
        buttonRow.add(leftButtons, BorderLayout.WEST);
        buttonRow.add(rightButtons, BorderLayout.EAST);
        add(buttonRow, BorderLayout.SOUTH);

        setPreferredSize(new Dimension(680, 360));
        pack();
        setLocationRelativeTo(owner);
    }

    private void onAdd() {
        McpServerConfig created = ServerEditorDialog.showDialog(this, null);
        if (created != null) {
            servers.add(created);
            tableModel.fireTableDataChanged();
        }
    }

    private void onEdit() {
        int row = table.getSelectedRow();
        if (row < 0) {
            return;
        }
        McpServerConfig edited = ServerEditorDialog.showDialog(this, servers.get(row));
        if (edited != null) {
            servers.set(row, edited);
            tableModel.fireTableDataChanged();
        }
    }

    private void onRemove() {
        int row = table.getSelectedRow();
        if (row < 0) {
            return;
        }
        int choice = JOptionPane.showConfirmDialog(this, "Remove server '" + servers.get(row).name + "'?",
                "Remove", JOptionPane.YES_NO_OPTION);
        if (choice == JOptionPane.YES_OPTION) {
            servers.remove(row);
            tableModel.fireTableDataChanged();
        }
    }

    private void onTest() {
        int row = table.getSelectedRow();
        if (row < 0) {
            return;
        }
        McpServerConfig config = servers.get(row);
        testStatusLabel.setForeground(UIManager.getColor("Label.foreground"));
        testStatusLabel.setText("Testing...");
        SwingWorker<List<McpToolDescriptor>, Void> worker = new SwingWorker<List<McpToolDescriptor>, Void>() {
            McpStdioClient client;

            @Override
            protected List<McpToolDescriptor> doInBackground() throws Exception {
                client = McpServerRegistry.startTransient(config, TEST_TIMEOUT_MILLIS);
                return client.listTools(TEST_TIMEOUT_MILLIS);
            }

            @Override
            protected void done() {
                if (client != null) {
                    client.close();
                }
                try {
                    List<McpToolDescriptor> tools = get();
                    StringBuilder names = new StringBuilder();
                    for (McpToolDescriptor t : tools) {
                        if (names.length() > 0) {
                            names.append(", ");
                        }
                        names.append(t.name);
                    }
                    testStatusLabel.setForeground(new Color(0, 140, 0));
                    testStatusLabel.setText("Connected - " + tools.size() + " tool(s): " + names);
                } catch (Exception ex) {
                    testStatusLabel.setForeground(Color.RED);
                    testStatusLabel.setText("Connection failed");
                    Throwable cause = ex.getCause() != null ? ex.getCause() : ex;
                    JOptionPane.showMessageDialog(McpServerConfigDialog.this,
                            cause.getMessage(), "Test Failed", JOptionPane.ERROR_MESSAGE);
                }
            }
        };
        worker.execute();
    }
}
