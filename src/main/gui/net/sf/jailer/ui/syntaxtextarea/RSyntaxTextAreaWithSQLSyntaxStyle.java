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

import java.awt.Dialog;
import java.awt.Frame;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.ImageIcon;
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
import org.fife.ui.rtextarea.Gutter;
import org.fife.ui.rtextarea.SearchContext;
import org.fife.ui.rtextarea.SearchEngine;
import org.fife.ui.rtextarea.SearchResult;

import net.sf.jailer.ui.databrowser.metadata.MetaDataPanel;
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
	private final boolean withExecuteActions;

	/**
	 * Key stokes.
	 */
	public static KeyStroke KS_RUN_BLOCK = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, InputEvent.CTRL_DOWN_MASK);
	public static KeyStroke KS_RUN_ALL = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, InputEvent.ALT_DOWN_MASK);
	public static KeyStroke KS_FORMAT = KeyStroke.getKeyStroke(KeyEvent.VK_F, InputEvent.SHIFT_DOWN_MASK|InputEvent.CTRL_DOWN_MASK);

	/**
	 * Actions.
	 */
	public final Action runBlock;
	public final Action runAll;
	public final Action formatSQL;

	public RSyntaxTextAreaWithSQLSyntaxStyle(boolean withExecuteActions) {
		this.withExecuteActions = withExecuteActions;
		setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_SQL);
		setAutoIndentEnabled(true);
		
		formatSQL = new AbstractAction("Format SQL") {
			{
				putValue(ACCELERATOR_KEY, KS_FORMAT);
				InputMap im = getInputMap();
				im.put(KS_FORMAT, this);
				ActionMap am = getActionMap();
				am.put(this, this);
			}

			@Override
			public void actionPerformed(ActionEvent e) {
				formatSQL();
			}
		};

		runBlock = new AbstractAction("Run selected SQL") {
			{
				putValue(ACCELERATOR_KEY, KS_RUN_BLOCK);
				InputMap im = getInputMap();
				im.put(KS_RUN_BLOCK, this);
				ActionMap am = getActionMap();
				am.put(this, this);
			}

			@Override
			public void actionPerformed(ActionEvent e) {
				RSyntaxTextAreaWithSQLSyntaxStyle.this.runBlock();
			}
		};

		runAll = new AbstractAction("Run all SQL") {
			{
				putValue(ACCELERATOR_KEY, KS_RUN_ALL);
				InputMap im = getInputMap();
				im.put(KS_RUN_ALL, this);
				ActionMap am = getActionMap();
				am.put(this, this);
			}

			@Override
			public void actionPerformed(ActionEvent e) {
				RSyntaxTextAreaWithSQLSyntaxStyle.this.runAll();
			}
		};

		InputMap im = getInputMap();
		im.put(KS_RUN_BLOCK, runBlock);
		ActionMap am = getActionMap();
		am.put(runBlock, runBlock);

		im = getInputMap();
		im.put(KS_RUN_ALL, runAll);
		am = getActionMap();
		am.put(runAll, runAll);

		setMarkOccurrences(true);

		addCaretListener(new CaretListener() {
			@Override
			public void caretUpdate(CaretEvent e) {
				updateMenuItemState();
			}
		});

		setHighlightCurrentLine(true);
		setFadeCurrentLineHighlight(true);
		
		createPopupMenu();
		updateMenuItemState();
	}

	/**
	 * Overridden to add menu items related to formatting.
	 *
	 * @return the popup menu
	 */
	@Override
	protected JPopupMenu createPopupMenu() {
		JPopupMenu menu = super.createPopupMenu();

		JMenuItem item = new JMenuItem(formatSQL);
		item.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				formatSQL();
			}
		});
		menu.add(item, 0);
		menu.add(new JSeparator(), 1);

		menu.add(new JMenuItem(new ShowFindDialogAction()), 0);
		menu.add(new JMenuItem(new ShowReplaceDialogAction()), 1);
		menu.add(new JSeparator(), 2);

		if (withExecuteActions) {
			item = new JMenuItem(runBlock);
			menu.add(item, 0);
			item = new JMenuItem(runAll);
			menu.add(item, 1);
			menu.add(new JSeparator(), 2);
		}

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
		Pair<Integer, Integer> loc = getCurrentStatementLocation(singleStatement, false);
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
		Pair<Integer, Integer> loc = getCurrentStatementLocation(singleStatement, false);
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
	 * Does the text have any non-WS characters?
	 */
	public boolean isTextEmpty(int from, int to) {
		Segment txt = new Segment();
		try {
			if (to >= getLineCount()) {
				to = getLineCount() - 1;
			}
			for (int i = from; i <= to; ++i) {
				int off = getLineStartOffset(i);
				getDocument().getText(off, getLineEndOffset(to) - off, txt);
				if (txt.toString().trim().length() > 0) {
					return false;
				}
			}
		} catch (BadLocationException e) {
			return false;
		}
		return true;
	}

	/**
	 * Gets start- and end-line number of statement(s) at caret position.
	 * 
	 * @param singleStatement
	 *            <code>true</code> to get only one statement
	 * @return pair of start and end line number
	 */
	public Pair<Integer, Integer> getCurrentStatementLocation() {
		return getCurrentStatementLocation(getCaret().getDot() != getCaret().getMark(), false);
	}

	/**
	 * Gets start- and end-line number of statement(s) at caret position.
	 * 
	 * @param singleStatement
	 *            <code>true</code> to get only one statement
	 * @return pair of start and end line number
	 */
	public Pair<Integer, Integer> getCurrentStatementLocation(boolean singleStatement, boolean currentLineMayBeEmpty) {
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
				if (sLine.length() == 0 && !(currentLineMayBeEmpty && end == y)) {
					if (end > start) {
						--end;
					}
					break;
				}
				if (singleStatement && sLine.endsWith(";")) {
					break;
				}
				++end;
			}
			if (end == lineCount && end > 0) {
				--end;
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

	protected void runBlock() {
	}

	protected void runAll() {
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

	public void updateMenuItemState() {
		updateMenuItemState(true, true);
	}

	public void updateMenuItemState(boolean allowRun, boolean setLineHighlights) {
		Pair<Integer, Integer> loc = getCurrentStatementLocation();
		runBlock.setEnabled(allowRun && loc != null && !isTextEmpty(loc.a, loc.b));
		runAll.setEnabled(allowRun && RSyntaxTextAreaWithSQLSyntaxStyle.this.getDocument().getLength() > 0);
		if (allowRun && setLineHighlights) {
			if (!pending.get()) {
				removeAllLineHighlights();
				setHighlightCurrentLine(true);
				if (gutter != null) {
					gutter.removeAllTrackingIcons();
					try {
						if (loc.a != loc.b || !getText(loc.a, loc.b, true).trim().isEmpty()) {
							for (int l = loc.a; l <= loc.b; ++l) {
								gutter.addLineTrackingIcon(l, icon);
							}
						}
					} catch (BadLocationException e) {
					}
				}
//				for (int l = loc.a; l <= loc.b; ++l) {
//					try {
//						addLineHighlight(l, new Color(235, 255, 245));
//					} catch (BadLocationException e) {
//						e.printStackTrace();
//					}
//				}
				if (loc.b - loc.a > 10000) {
					stopped.set(false);
					pending.set(true);
					new Thread(new Runnable() {
						@Override
						public void run() {
							try {
								Thread.sleep(2000);
							} catch (InterruptedException e) {
							}
							pending.set(false);
							if (!stopped.get()) {
								SwingUtilities.invokeLater(new Runnable() {
									@Override
									public void run() {
										updateMenuItemState(true, true);
									}
								});
							}
						}
					}).start();
				} else {
					stopped.set(true);
				}
			}
		}
	}

	/**
	 * Formats SQL statement at caret position.
	 */
	private void formatSQL() {
		String currentStatement = getCurrentStatement(true);
		Pattern pattern = Pattern.compile("(.*?)(;\\s*(\\n\\r?|$))", Pattern.DOTALL);
		Matcher matcher = pattern.matcher(currentStatement + ";");
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
			if (sb.length() > 0) {
				sb.setLength(sb.length() - 1);
			}
			pattern = Pattern.compile(".*?[^;](\\s*)$", Pattern.DOTALL);
			matcher = pattern.matcher(currentStatement);
			String tail = matcher.matches() ? matcher.group(1) : "";
			replaceCurrentStatement(sb.toString() + tail, true);
		}
	}

	private final AtomicBoolean stopped = new AtomicBoolean(false);
	private final AtomicBoolean pending = new AtomicBoolean(false);
	private Gutter gutter;

	public void setGutter(Gutter gutter) {
		this.gutter = gutter;
	}

	private static ImageIcon icon;
    static {
		String dir = "/net/sf/jailer/ui/resource";
		
		// load images
		try {
			icon = new ImageIcon(MetaDataPanel.class.getResource(dir + "/sqlconsole.png"));
		} catch (Exception e) {
			e.printStackTrace();
		}
    }

}
