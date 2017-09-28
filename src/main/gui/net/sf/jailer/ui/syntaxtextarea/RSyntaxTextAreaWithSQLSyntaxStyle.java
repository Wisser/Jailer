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
import java.awt.Dialog;
import java.awt.Frame;
import java.awt.Window;
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
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.Segment;

import org.fife.rsta.ui.search.FindDialog;
import org.fife.rsta.ui.search.ReplaceDialog;
import org.fife.rsta.ui.search.SearchEvent;
import org.fife.rsta.ui.search.SearchListener;
import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.fife.ui.rsyntaxtextarea.SyntaxConstants;
import org.fife.ui.rtextarea.SearchContext;
import org.fife.ui.rtextarea.SearchEngine;
import org.fife.ui.rtextarea.SearchResult;

import net.sf.jailer.util.Pair;

/**
 * Text area for SQL documents.
 * 
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public class RSyntaxTextAreaWithSQLSyntaxStyle extends RSyntaxTextArea implements SearchListener {

	private FindDialog findDialog;
	private ReplaceDialog replaceDialog;

	public RSyntaxTextAreaWithSQLSyntaxStyle() {
		setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_SQL);
		Action action = new AbstractAction() {
			@Override
			public void actionPerformed(ActionEvent e) {
				RSyntaxTextAreaWithSQLSyntaxStyle.this.actionPerformed();
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
		
		createPopupMenu();
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
						matcher.appendReplacement(sb,
								Matcher.quoteReplacement(new BasicFormatterImpl().format(matcher.group(1)))
										+ matcher.group(2));
						result = matcher.find();
					} while (result);
					matcher.appendTail(sb);
					replaceCurrentStatement(sb.toString(), true);
				}
			}
		});
		menu.add(item, 0);
		menu.add(new JSeparator(), 1);

		menu.add(new JMenuItem(new ShowFindDialogAction()), 0);
		menu.add(new JMenuItem(new ShowReplaceDialogAction()), 1);
		menu.add(new JSeparator(), 2);

		return menu;
	}

	/**
	 * Gets statement(s) at caret position.
	 *
	 * @param replacement
	 *            the replacement
	 * @param singleStatement
	 *            <code>true</code> to replace only one statement
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
	 * @param singleStatement
	 *            <code>true</code> to get only one statement
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
	 * @param complete
	 *            if <code>false</code>, return text from start line to current
	 *            caret position
	 */
	public String getText(int from, int to, boolean complete) {
		Segment txt = new Segment();
		try {
			if (to >= getLineCount()) {
				to = getLineCount() - 1;
			}
			int off = getLineStartOffset(from);
			getDocument().getText(off, (complete ? getLineEndOffset(to) : getCaretPosition()) - off, txt);
		} catch (BadLocationException e) {
			return "";
		}
		return txt.toString();
	}

	/**
	 * Gets start- and end-line number of statement(s) at caret position.
	 * 
	 * @param singleStatement
	 *            <code>true</code> to get only one statement
	 * @return pair of start and end line number
	 */
	public Pair<Integer, Integer> getCurrentStatementLocation(boolean singleStatement) {
		try {
			int y = getLineOfOffset(Math.min(getCaret().getDot(), getCaret().getMark()));
			int start = y;
			// if (getText(start, start, true).trim().length() > 0) {
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
			// }
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

	/**
	 * Listens for events from our search dialogs and actually does the dirty
	 * work.
	 */
	@Override
	public void searchEvent(SearchEvent e) {

		SearchEvent.Type type = e.getType();
		SearchContext context = e.getSearchContext();
		SearchResult result = null;

		switch (type) {
		default: // Prevent FindBugs warning later
		case MARK_ALL:
			result = SearchEngine.markAll(this, context);
			break;
		case FIND:
			result = SearchEngine.find(this, context);
			if (!result.wasFound()) {
				UIManager.getLookAndFeel().provideErrorFeedback(this);
			}
			break;
		case REPLACE:
			result = SearchEngine.replace(this, context);
			if (!result.wasFound()) {
				UIManager.getLookAndFeel().provideErrorFeedback(this);
			}
			break;
		case REPLACE_ALL:
			result = SearchEngine.replaceAll(this, context);
			JOptionPane.showMessageDialog(null, result.getCount() + " occurrences replaced.");
			break;
		}

		String text = null;
		if (result.wasFound()) {
			text = "Text found; occurrences marked: " + result.getMarkedCount();
		} else if (type == SearchEvent.Type.MARK_ALL) {
			if (result.getMarkedCount() > 0) {
				text = "Occurrences marked: " + result.getMarkedCount();
			} else {
				text = "";
			}
		} else {
			text = "Text not found";
			JOptionPane.showMessageDialog(null, text);
		}
	}

	private class ShowFindDialogAction extends AbstractAction {

		public ShowFindDialogAction() {
			super("Find...");
			int c = getToolkit().getMenuShortcutKeyMask();
			KeyStroke keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_F, c);
			putValue(ACCELERATOR_KEY, keyStroke);
			InputMap im = getInputMap();
			im.put(keyStroke, this);
			ActionMap am = getActionMap();
			am.put(this, this);
		}

		@Override
		public void actionPerformed(ActionEvent e) {
			initDialogs();
			if (replaceDialog.isVisible()) {
				replaceDialog.setVisible(false);
			}
			findDialog.setVisible(true);
		}
	}

	private class ShowReplaceDialogAction extends AbstractAction {

		public ShowReplaceDialogAction() {
			super("Replace...");
			int c = getToolkit().getMenuShortcutKeyMask();
			KeyStroke keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_H, c);
			putValue(ACCELERATOR_KEY, keyStroke);
			InputMap im = getInputMap();
			im.put(keyStroke, this);
			ActionMap am = getActionMap();
			am.put(this, this);
		}

		@Override
		public void actionPerformed(ActionEvent e) {
			initDialogs();
			if (findDialog.isVisible()) {
				findDialog.setVisible(false);
			}
			replaceDialog.setVisible(true);
		}
	}

	protected void actionPerformed() {
	}

	private void initDialogs() {
		if (findDialog == null) {
			Window owner = SwingUtilities.getWindowAncestor(this);
			if (owner instanceof Dialog) {
				findDialog = new FindDialog((Dialog) owner, this);
				replaceDialog = new ReplaceDialog((Dialog) owner, this);
			} else if (owner instanceof Frame) {
				findDialog = new FindDialog((Frame) owner, this);
				replaceDialog = new ReplaceDialog((Frame) owner, this);
			} else {
				findDialog = new FindDialog((Frame) null, this);
				replaceDialog = new ReplaceDialog((Frame) null, this);
			}
		
			// This ties the properties of the two dialogs together (match case,
			// regex, etc.).
			SearchContext context = findDialog.getSearchContext();
			replaceDialog.setSearchContext(context);
			}
	}
}
