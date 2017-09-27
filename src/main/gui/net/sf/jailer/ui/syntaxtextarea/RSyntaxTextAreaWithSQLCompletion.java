/*
 * Copyright 2007 - 2017 the original author or authors.
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
package net.sf.jailer.ui.syntaxtextarea;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.InputMap;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.KeyStroke;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.Segment;

import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.fife.ui.rsyntaxtextarea.SyntaxConstants;

import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.util.Pair;

/**
 * Text area with code completion based on {@link DataModel} information.
 * 
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public class RSyntaxTextAreaWithSQLCompletion extends RSyntaxTextArea {
	
	private int end;

	public RSyntaxTextAreaWithSQLCompletion() {
		setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_SQL);
		
		Action action = new AbstractAction() {
			@Override
			public void actionPerformed(ActionEvent e) {
				RSyntaxTextAreaWithSQLCompletion.this.actionPerformed();
			}
		};
		
		KeyStroke ks = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, InputEvent.CTRL_DOWN_MASK);
		InputMap im = getInputMap();
		im.put(ks, action);
		ActionMap am = getActionMap();
		am.put(action, action);
		
		setMarkOccurrences(true);
		
		addCaretListener(new CaretListener() {
			@Override
			public void caretUpdate(CaretEvent e) {
				try {
					removeAllLineHighlights();
					Pair<Integer, Integer> loc = getCurrentStatementLocation(false);
					for (int l = loc.a; l < loc.b; ++l) {
						addLineHighlight(l, new Color(240, 250, 255));
					}
				} catch (BadLocationException e1) {
					e1.printStackTrace();
				}	
			}
		});
	}

	/**
	 * Overridden to add menu items related to formatting
	 *
	 * @return the popup menu
	 */
	@Override
	protected JPopupMenu createPopupMenu() {
		JPopupMenu menu = super.createPopupMenu();

		JMenuItem item = new JMenuItem("Format SQL");
		item.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				String currentStatement = getCurrentStatement(true);
				Pattern pattern = Pattern.compile("(.*?)(;\\s*(\\n\\r?|$))", Pattern.DOTALL);
				Matcher matcher = pattern.matcher(currentStatement);
		        boolean result = matcher.find();
		        if (result) {
		            StringBuffer sb = new StringBuffer();
		            do {
		                matcher.appendReplacement(sb, Matcher.quoteReplacement(new BasicFormatterImpl().format(matcher.group(1))) + matcher.group(2));
		                result = matcher.find();
		            } while (result);
		            matcher.appendTail(sb);
					replaceCurrentStatement(sb.toString(), true);
		        }
			}
		});
		menu.add(item, 0);
		menu.add(new JSeparator(), 1);
		return menu;
	}
	
	/**
	 * Gets statement(s) at caret position.
	 *
	 * @param replacement the replacement
	 * @param singleStatement <code>true</code> to replace only one statement
	 */
	public void replaceCurrentStatement(String replacement, boolean singleStatement) {
		Pair<Integer, Integer> loc = getCurrentStatementLocation(singleStatement);
		if (loc != null) {
			try {
				int from = loc.a;
				int to = loc.b;
				if (to >= getLineCount()) {
					to = getLineCount() - 1;
				}
				int start = getLineStartOffset(from);
				int end = getLineEndOffset(to);
				replaceRange(replacement, start, end);
			} catch (BadLocationException e) {
				e.printStackTrace();
			}
		}
	}

	/**
	 * Gets statement(s) at caret position.
	 * 
	 * @param singleStatement <code>true</code> to get only one statement
	 * @return pair of start and end line number
	 */
	public String getCurrentStatement(boolean singleStatement) {
		Pair<Integer, Integer> loc = getCurrentStatementLocation(singleStatement);
		if (loc != null) {
			return getText(loc.a, loc.b, true);
		}
		return "";
	}

	/**
	 * Gets text between two lines.
	 * 
	 * @param complete if <code>false</code>, return text from start line to current caret position
	 */
	public String getText(int from, int to, boolean complete) {
		Segment txt = new Segment();
		try {
			if (to >= getLineCount()) {
				to = getLineCount() - 1;
			}
			int off = getLineStartOffset(from);
			getDocument().getText(off, (complete? getLineEndOffset(to) : getCaretPosition()) - off, txt);
		} catch (BadLocationException e) {
			return "";
		}
		return txt.toString();
	}
	
	/**
	 * Gets start- and end-line number of statement(s) at caret position.
	 * 
	 * @param singleStatement <code>true</code> to get only one statement
	 * @return pair of start and end line number
	 */
	public Pair<Integer, Integer> getCurrentStatementLocation(boolean singleStatement) {
		try {
			int y = getLineOfOffset(Math.min(getCaret().getDot(), getCaret().getMark()));
			int start = y;
//			if (getText(start, start, true).trim().length() > 0) {
				while (start > 0) {
					int startM1Off = getLineStartOffset(start - 1);
					Segment txt = new Segment();
					getDocument().getText(startM1Off, getLineEndOffset(start - 1) - startM1Off, txt);
					String sLine = txt.toString().trim();
					if (sLine.length() == 0 || (singleStatement && sLine.endsWith(";"))) {
						break;
					}
					--start;
				}
//			}
			int end = getLineOfOffset(Math.max(getCaret().getDot(), getCaret().getMark()));
			int lineCount = getLineCount();
			while (end < lineCount) {
				int endOff = getLineStartOffset(end);
				Segment txt = new Segment();
				getDocument().getText(endOff, getLineEndOffset(end) - endOff, txt);
				String sLine = txt.toString().trim();
				if (sLine.length() == 0 || (singleStatement && sLine.endsWith(";"))) {
					break;
				}
				++end;
			}
			return new Pair<Integer, Integer>(start, end);
		} catch (BadLocationException e) {
			e.printStackTrace();
			return null;
		}
	}
	
	protected void actionPerformed() {
	}
	
}
