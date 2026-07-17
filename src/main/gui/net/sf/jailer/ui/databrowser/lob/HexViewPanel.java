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
package net.sf.jailer.ui.databrowser.lob;

import java.awt.BorderLayout;
import java.awt.Font;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

/**
 * Read-only hexadecimal dump of a byte buffer: 16 bytes per row, rendered as
 * {@code offset  hex-octets  |ascii|}. Used as the fallback view for content
 * whose type could not be recognized.
 *
 * @author Ralf Wisser
 */
public class HexViewPanel extends JPanel {

	private static final long serialVersionUID = 1L;

	private static final char[] HEX = "0123456789ABCDEF".toCharArray();

	/**
	 * @param data        the (possibly truncated) bytes to render
	 * @param totalLength the total content length, to note truncation
	 */
	public HexViewPanel(byte[] data, long totalLength) {
		super(new BorderLayout());
		JTextArea area = new JTextArea();
		area.setEditable(false);
		area.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 12));
		area.setText(toHex(data, totalLength));
		area.setCaretPosition(0);
		JScrollPane scrollPane = new JScrollPane(area);
		scrollPane.getVerticalScrollBar().setUnitIncrement(16);
		scrollPane.getHorizontalScrollBar().setUnitIncrement(16);
		add(scrollPane, BorderLayout.CENTER);
	}

	private static String toHex(byte[] data, long totalLength) {
		if (data == null) {
			data = new byte[0];
		}
		StringBuilder sb = new StringBuilder();
		StringBuilder ascii = new StringBuilder();
		for (int i = 0; i < data.length; i += 16) {
			appendHex8(sb, i);
			sb.append("  ");
			ascii.setLength(0);
			for (int j = 0; j < 16; j++) {
				if (i + j < data.length) {
					int b = data[i + j] & 0xFF;
					sb.append(HEX[(b >> 4) & 0xF]).append(HEX[b & 0xF]).append(' ');
					ascii.append(b >= 0x20 && b < 0x7F ? (char) b : '.');
				} else {
					sb.append("   ");
				}
				if (j == 7) {
					sb.append(' ');
				}
			}
			sb.append(' ').append('|').append(ascii).append('|').append('\n');
		}
		if (data.length < totalLength) {
			sb.append("\n... ").append(totalLength).append(" bytes total, showing the first ").append(data.length).append('.');
		}
		return sb.toString();
	}

	private static void appendHex8(StringBuilder sb, int value) {
		for (int shift = 28; shift >= 0; shift -= 4) {
			sb.append(HEX[(value >> shift) & 0xF]);
		}
	}
}
