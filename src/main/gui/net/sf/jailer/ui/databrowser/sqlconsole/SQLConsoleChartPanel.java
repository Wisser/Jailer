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
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.sql.Types;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.Box;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.RowSorter;
import javax.swing.SwingConstants;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.data.category.DefaultCategoryDataset;
import org.jfree.data.general.DefaultPieDataset;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.BrowserContentPane.TableModelItem;

import javax.swing.JTable;

/**
 * Chart panel for the SQL Console result view.
 * Renders JFreeChart diagrams from query result data.
 */
public class SQLConsoleChartPanel extends JPanel {

    private static final String CHART_BAR  = "Bar";
    private static final String CHART_LINE = "Line";
    private static final String CHART_PIE  = "Pie";
    private static final String CHART_XY   = "XY (Scatter)";

    private final JComboBox<String> chartTypeCombo = new JComboBox<>(new String[]{CHART_BAR, CHART_LINE, CHART_PIE, CHART_XY});
    private final JComboBox<String> xColumnCombo   = new JComboBox<>();
    private final JComboBox<String> yColumnCombo   = new JComboBox<>();
    private final JPanel chartContainer = new JPanel(new BorderLayout());

    private JTable currentTable;
    private final List<Integer> columnTypes;
    private boolean suppressUpdate = false;

    private static final Pattern BOLD_PATTERN = Pattern.compile("<b>(.*?)</b>");

    public SQLConsoleChartPanel(List<Integer> columnTypes) {
        this.columnTypes = columnTypes;
        setLayout(new BorderLayout());

        JPanel toolbar = new JPanel(new FlowLayout(FlowLayout.LEFT, 4, 2));
        toolbar.add(new JLabel("Chart type:"));
        toolbar.add(chartTypeCombo);
        toolbar.add(Box.createHorizontalStrut(8));
        toolbar.add(new JLabel("X:"));
        xColumnCombo.setPreferredSize(new Dimension(140, xColumnCombo.getPreferredSize().height));
        toolbar.add(xColumnCombo);
        toolbar.add(new JLabel("Y:"));
        yColumnCombo.setPreferredSize(new Dimension(140, yColumnCombo.getPreferredSize().height));
        toolbar.add(yColumnCombo);

        add(toolbar, BorderLayout.NORTH);
        add(chartContainer, BorderLayout.CENTER);
        chartContainer.add(new JLabel("Select columns above to create a chart.", SwingConstants.CENTER));

        java.awt.event.ActionListener redraw = e -> { if (!suppressUpdate) updateChart(); };
        chartTypeCombo.addActionListener(redraw);
        xColumnCombo.addActionListener(redraw);
        yColumnCombo.addActionListener(redraw);
    }

    public void setTable(JTable table) {
        this.currentTable = table;
        refreshColumnCombos();
        updateChart();
    }

    private void refreshColumnCombos() {
        if (currentTable == null) return;
        TableModel model = currentTable.getModel();
        TableColumnModel cm = currentTable.getColumnModel();
        int colCount = cm.getColumnCount();

        String xSel = (String) xColumnCombo.getSelectedItem();
        String ySel = (String) yColumnCombo.getSelectedItem();

        suppressUpdate = true;
        try {
            xColumnCombo.removeAllItems();
            yColumnCombo.removeAllItems();

            for (int i = 0; i < colCount; i++) {
                int modelIdx = cm.getColumn(i).getModelIndex();
                String name = stripHtml(model.getColumnName(modelIdx));
                xColumnCombo.addItem(name);
                yColumnCombo.addItem(name);
            }

            if (xSel != null) xColumnCombo.setSelectedItem(xSel);
            if (ySel != null) yColumnCombo.setSelectedItem(ySel);

            if (xColumnCombo.getSelectedIndex() < 0 && colCount > 0) {
                xColumnCombo.setSelectedIndex(0);
            }
            if (yColumnCombo.getSelectedIndex() < 0 && colCount > 0) {
                int defY = findFirstNumericViewCol();
                yColumnCombo.setSelectedIndex(defY >= 0 ? defY : Math.min(1, colCount - 1));
            }
        } finally {
            suppressUpdate = false;
        }
    }

    private int findFirstNumericViewCol() {
        if (currentTable == null) return -1;
        TableColumnModel cm = currentTable.getColumnModel();
        for (int i = 0; i < cm.getColumnCount(); i++) {
            if (isNumeric(cm.getColumn(i).getModelIndex())) return i;
        }
        return -1;
    }

    private boolean isNumeric(int modelCol) {
        if (columnTypes != null && modelCol < columnTypes.size()) {
            switch (columnTypes.get(modelCol)) {
                case Types.BIGINT:
                case Types.DECIMAL:
                case Types.DOUBLE:
                case Types.FLOAT:
                case Types.INTEGER:
                case Types.NUMERIC:
                case Types.REAL:
                case Types.SMALLINT:
                case Types.TINYINT:
                    return true;
                default:
                    break;
            }
        }
        return false;
    }

    private void updateChart() {
        if (currentTable == null) return;
        int xView = xColumnCombo.getSelectedIndex();
        int yView = yColumnCombo.getSelectedIndex();
        if (xView < 0 || yView < 0) return;

        TableColumnModel cm = currentTable.getColumnModel();
        RowSorter<? extends TableModel> sorter = currentTable.getRowSorter();
        TableModel model = currentTable.getModel();

        if (cm.getColumnCount() == 0) return;

        int xModelCol = cm.getColumn(xView).getModelIndex();
        int yModelCol = cm.getColumn(yView).getModelIndex();
        int rowCount = sorter != null ? sorter.getViewRowCount() : model.getRowCount();
        String xLabel = stripHtml(model.getColumnName(xModelCol));
        String yLabel = stripHtml(model.getColumnName(yModelCol));
        String type = (String) chartTypeCombo.getSelectedItem();

        JFreeChart chart;
        try {
            if (CHART_PIE.equals(type)) {
                chart = buildPieChart(model, sorter, rowCount, xModelCol, yModelCol);
            } else if (CHART_XY.equals(type)) {
                chart = buildXYChart(model, sorter, rowCount, xModelCol, yModelCol, xLabel, yLabel);
            } else {
                chart = buildCategoryChart(type, model, sorter, rowCount, xModelCol, yModelCol, xLabel, yLabel);
            }
        } catch (Exception ex) {
            showError(ex.getMessage());
            return;
        }

        ChartPanel cp = new ChartPanel(chart);
        cp.setMouseWheelEnabled(true);
        chartContainer.removeAll();
        chartContainer.add(cp, BorderLayout.CENTER);
        chartContainer.revalidate();
        chartContainer.repaint();
    }

    private JFreeChart buildPieChart(TableModel model, RowSorter<? extends TableModel> sorter, int rowCount, int xCol, int yCol) {
        DefaultPieDataset dataset = new DefaultPieDataset();
        for (int i = 0; i < rowCount; i++) {
            int row = sorter != null ? sorter.convertRowIndexToModel(i) : i;
            String key = cellString(model.getValueAt(row, xCol));
            double val = cellDouble(model.getValueAt(row, yCol));
            if (!Double.isNaN(val)) {
                Number existing = dataset.getValue(key);
                dataset.setValue(key, existing != null ? existing.doubleValue() + val : val);
            }
        }
        return ChartFactory.createPieChart(null, dataset, true, true, false);
    }

    private JFreeChart buildXYChart(TableModel model, RowSorter<? extends TableModel> sorter, int rowCount, int xCol, int yCol, String xLabel, String yLabel) {
        XYSeriesCollection dataset = new XYSeriesCollection();
        XYSeries series = new XYSeries("Data");
        for (int i = 0; i < rowCount; i++) {
            int row = sorter != null ? sorter.convertRowIndexToModel(i) : i;
            double x = cellDouble(model.getValueAt(row, xCol));
            double y = cellDouble(model.getValueAt(row, yCol));
            if (!Double.isNaN(x) && !Double.isNaN(y)) series.add(x, y);
        }
        dataset.addSeries(series);
        return ChartFactory.createScatterPlot(null, xLabel, yLabel, dataset);
    }

    private JFreeChart buildCategoryChart(String type, TableModel model, RowSorter<? extends TableModel> sorter, int rowCount, int xCol, int yCol, String xLabel, String yLabel) {
        DefaultCategoryDataset dataset = new DefaultCategoryDataset();
        for (int i = 0; i < rowCount; i++) {
            int row = sorter != null ? sorter.convertRowIndexToModel(i) : i;
            String cat = cellString(model.getValueAt(row, xCol));
            double val = cellDouble(model.getValueAt(row, yCol));
            if (!Double.isNaN(val)) dataset.addValue(val, yLabel, cat);
        }
        if (CHART_BAR.equals(type)) {
            return ChartFactory.createBarChart(null, xLabel, yLabel, dataset, PlotOrientation.VERTICAL, false, true, false);
        }
        return ChartFactory.createLineChart(null, xLabel, yLabel, dataset, PlotOrientation.VERTICAL, false, true, false);
    }

    private void showError(String msg) {
        chartContainer.removeAll();
        chartContainer.add(new JLabel("<html><center>Error: " + UIUtil.toHTMLFragment(msg != null ? msg : "unknown error", 80) + "</center></html>", SwingConstants.CENTER));
        chartContainer.revalidate();
        chartContainer.repaint();
    }

    private String cellString(Object value) {
        if (value instanceof TableModelItem) value = ((TableModelItem) value).value;
        if (value == null || value == UIUtil.NULL) return "(null)";
        return value.toString();
    }

    private double cellDouble(Object value) {
        if (value instanceof TableModelItem) value = ((TableModelItem) value).value;
        if (value == null || value == UIUtil.NULL) return Double.NaN;
        if (value instanceof Number) return ((Number) value).doubleValue();
        try {
            return Double.parseDouble(value.toString().trim().replace(",", "."));
        } catch (NumberFormatException e) {
            return Double.NaN;
        }
    }

    private String stripHtml(String html) {
        if (html == null) return "";
        if (html.startsWith("<html>")) {
            Matcher m = BOLD_PATTERN.matcher(html);
            if (m.find()) return m.group(1);
            return html.replaceAll("<[^>]*>", " ").replaceAll("\\s+", " ").trim();
        }
        return html;
    }
}
