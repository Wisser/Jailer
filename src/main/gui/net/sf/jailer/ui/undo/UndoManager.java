/*
 * Copyright 2007 - 2025 Ralf Wisser.
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
package net.sf.jailer.ui.undo;

import java.awt.Color;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.Timer;

import net.sf.jailer.ui.Colors;
import net.sf.jailer.ui.ExtractionModelEditor;
import net.sf.jailer.ui.UIUtil;

/**
 * Undo/Redo support for {@link ExtractionModelEditor}.
 * 
 * @author Ralf Wisser
 */
public class UndoManager {

	private final LinkedList<CompensationAction> undoStack = new LinkedList<>();
	private final LinkedList<CompensationAction> redoStack = new LinkedList<>();

	private final JMenuItem undoMenuItem;
	private final JMenuItem redoMenuItem;
	private final JPanel undoViewHolder;
	
	/**
	 * Constructor.
	 */
	public UndoManager(JMenuItem undoMenuItem, JMenuItem redoMenuItem, JPanel undoViewHolder) {
		this.undoMenuItem = undoMenuItem;
		this.redoMenuItem = redoMenuItem;
		this.undoViewHolder = undoViewHolder;
		
		int mask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
		if (!System.getProperty("os.name", "").toLowerCase(Locale.ENGLISH).startsWith("mac")) {
			undoMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Z, mask));
			redoMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Y, mask));
		} else {
			undoMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Z, mask));
			redoMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Z, InputEvent.SHIFT_MASK | mask));
		}
		updateView();
	}

	private boolean undoing = false;
	private boolean redoing = false;
	private boolean isOpen = false;

	private void open() {
		if (!isOpen) {
			isOpen = true;
			final boolean protokoll = !undoing && !redoing;
			UIUtil.invokeLater(new Runnable() {
				@Override
				public void run() {
					isOpen = false;
					if (protokoll && !undoStack.isEmpty()) {
						toProtokoll(undoStack.getLast(), false, ProtokollItemType.DONE);
						updateView();
					}
				}
			});
		}
	}

	private static class CompositeAction extends CompensationAction {
		
		public CompositeAction() {
			super(0, null, null, null);
		}

		List<CompensationAction> actions = new ArrayList<>();
		
		@Override
		public void run() {
			for (int i = actions.size() - 1; i >= 0; --i) {
				actions.get(i).run();
			}
			actions.clear();
		}
	}

	/**
	 * Pushes a {@link CompensationAction} to the undo/redo stack.
	 * 
	 * @param compensationAction the action
	 */
	public void push(CompensationAction compensationAction) {
		if (undoing) {
			if (isOpen && !redoStack.isEmpty()) {
				CompensationAction action = redoStack.removeLast();
				if (action instanceof CompositeAction) {
					((CompositeAction) action).actions.add(compensationAction);
					redoStack.add(action);
				} else {
					CompositeAction compositeAction = new CompositeAction();
					compositeAction.actions.add(action);
					compositeAction.actions.add(compensationAction);
					redoStack.add(compositeAction);
				}
			} else {
				redoStack.add(compensationAction);
			}
		} else {
			if (isOpen && !undoStack.isEmpty()) {
				CompensationAction action = undoStack.removeLast();
				if (action instanceof CompositeAction) {
					((CompositeAction) action).actions.add(compensationAction);
					undoStack.add(action);
				} else {
					CompositeAction compositeAction = new CompositeAction();
					compositeAction.actions.add(action);
					compositeAction.actions.add(compensationAction);
					undoStack.add(compositeAction);
				}
			} else {
				undoStack.add(compensationAction);
			}
			if (!redoing) {
				redoStack.clear();
			}
		}
		
		open();
		
		updateMenuItems();
	}

	private final int MAX_PROTOCOL_SIZE = 10;
	private final LinkedList<Object[]> protocol = new LinkedList<>();
	
	private void updateView() {
		updateMenuItems();
		updateUndoView();			
	}

	private void updateMenuItems() {
		undoMenuItem.setEnabled(!undoStack.isEmpty());
		redoMenuItem.setEnabled(!redoStack.isEmpty());
	}

	private void updateUndoView() {
		undoViewHolder.removeAll();
		undoViewHolder.setVisible(!protocol.isEmpty());
		if (!protocol.isEmpty()) {
			Font font = new JLabel("normal").getFont();
			font = font.deriveFont(font.getSize() * 1.1f);
		    Font normal = font.deriveFont(font.getStyle() & ~Font.BOLD, font.getSize());
		    Font bold = font.deriveFont(font.getStyle() | Font.BOLD, font.getSize());
		    Font italic = font.deriveFont(font.getStyle() | Font.ITALIC, font.getSize());

			int y = 1;

			Color fg = Colors.Color_0_0_0;
			Color bg = Colors.Color_255_255_255_70;
			for (Object[] line: protocol) {
				JPanel panel = new JPanel(new GridBagLayout());
				panel.setOpaque(false);
				
				GridBagConstraints gridBagConstraints = new GridBagConstraints();
				gridBagConstraints.gridx = 0;
				gridBagConstraints.gridy = 0;
				gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
				gridBagConstraints.fill = java.awt.GridBagConstraints.NONE;
				gridBagConstraints.weightx = 0;
				gridBagConstraints.weighty = 0;
				JLabel label = new JLabel(String.valueOf(line[0]) + " ");
				label.setFont(italic);
				label.setBackground(bg);
				label.setOpaque(true);
				label.setForeground(fg);
				panel.add(label, gridBagConstraints);
			
				gridBagConstraints = new GridBagConstraints();
				gridBagConstraints.gridx = 1;
				gridBagConstraints.gridy = 0;
				gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
				gridBagConstraints.fill = java.awt.GridBagConstraints.NONE;
				gridBagConstraints.weightx = 0;
				gridBagConstraints.weighty = 0;
				label = new JLabel(" " + (line[1] == null? "" : String.valueOf(line[1])) + "   ");
				label.setFont(bold);
				label.setBackground(bg);
				label.setOpaque(true);
				label.setForeground(fg);
				panel.add(label, gridBagConstraints);
			
				gridBagConstraints = new GridBagConstraints();
				gridBagConstraints.gridx = 1;
				gridBagConstraints.gridy = y;
				gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
				gridBagConstraints.fill = java.awt.GridBagConstraints.NONE;
				gridBagConstraints.weightx = 0;
				gridBagConstraints.weighty = 0;
				undoViewHolder.add(panel, gridBagConstraints);
				
				gridBagConstraints = new GridBagConstraints();
				gridBagConstraints.gridx = 2;
				gridBagConstraints.gridy = y;
				gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
				gridBagConstraints.weightx = 1;
				gridBagConstraints.weighty = 0;
				label = new JLabel(" " + (((ProtokollItemType) line[2]).name) + "  ");
				label.setFont(font);
				label.setBackground(((ProtokollItemType) line[2]).color);
				label.setForeground(fg);
				label.setOpaque(true);
				label.setBorder(BorderFactory.createEtchedBorder());
				undoViewHolder.add(label, gridBagConstraints);

				gridBagConstraints = new GridBagConstraints();
				gridBagConstraints.gridx = 3;
				gridBagConstraints.gridy = y;
				gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
				gridBagConstraints.fill = java.awt.GridBagConstraints.NONE;
				gridBagConstraints.weightx = 0;
				gridBagConstraints.weighty = 0;
				label = new JLabel(line[3] == null? "" : String.valueOf(line[3]));
				label.setFont(normal);
				label.setBackground(bg);
				if (line[3] != null) {
					label.setOpaque(true);
					label.setForeground(fg);
				}
				undoViewHolder.add(label, gridBagConstraints);
				
				++y;
			}
			undoViewHolder.revalidate();
		}
	}

	private final int RETENTION_TIME = 5000;
	
	private Long retentionStartTime;
	
	private enum ProtokollItemType {
		UNDO(Colors.Color_150_255_0_70, "Undo"),
		REDO(Colors.Color_0_255_230_70, "Redo"),
		DONE(Colors.Color_220_255_220_70, "Done");
		
		public final Color color;
		public final String name;
		
		ProtokollItemType(Color color, String name) {
			this.color = color;
			this.name = name;
		}
	}
	
	private void toProtokoll(CompensationAction action, ProtokollItemType type) {
		toProtokoll(action, true, type);
	}

	private void toProtokoll(CompensationAction action, boolean positive, ProtokollItemType type) {
		String count = null;
		String what;
		String where;
		if (action instanceof CompositeAction) {
			final List<CompensationAction> actions = ((CompositeAction) action).actions;
			int l = 0;
			int prio = -1;
			for (CompensationAction a: actions) {
				if (a.prio > prio) {
					prio = a.prio;
				}
			}
			what = null;
			where = null;
			Map<String, Integer> whereCount = new HashMap<>();
			for (CompensationAction a: actions) {
				if (a.prio == prio) {
					++l;
					what = positive? a.whatToCompensate : a.whatHasDone;
					if (a.where != null) {
						Integer c = whereCount.get(a.where);
						if (c == null) {
							c = 1;
						} else {
							c = c + 1;
						}
						whereCount.put(a.where, c);
					}
				}
			}
			int maxC = -1;
			for (Entry<String, Integer> e: whereCount.entrySet()) {
				if (maxC < e.getValue()) {
					maxC = e.getValue();
					where = e.getKey();
				}
			}
			if (l > 1) {
				count = "  x " + l;
			}
		} else {
			what = positive? action.whatToCompensate : action.whatHasDone;
			where = action.where;
		}
		protocol.add(new Object[] { what == null? "" : what, where, type, count } );
		if (protocol.size() > MAX_PROTOCOL_SIZE) {
			protocol.removeFirst();
		}
		startRetentionTimer(RETENTION_TIME); 
	}

	private void startRetentionTimer(int time) {
		retentionStartTime = System.currentTimeMillis();
		final long myRetentionStartTime = retentionStartTime; 
		Timer timer = new Timer(time, new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (retentionStartTime != null && myRetentionStartTime == retentionStartTime) {
					retentionStartTime = null;
					if (!protocol.isEmpty()) {
						protocol.removeFirst();
					}
					updateUndoView();
					if (!protocol.isEmpty()) {
						startRetentionTimer(RETENTION_TIME / 40);
					}
				}
			}
		});
		timer.setRepeats(false);
		timer.start();
	}

	public void undo() {
		undoing = true;
		isOpen = true;
		try {
			redoStack.add(new CompositeAction());
			if (!undoStack.isEmpty()) {
				CompensationAction action = undoStack.removeLast();
				toProtokoll(action, ProtokollItemType.UNDO);
				action.run();
			}
			cleanupStack(redoStack);
		} catch (Throwable t) {
			UIUtil.showException(null, "Error", t);
		} finally {
			undoing = false;
			isOpen = false;
		}
		updateView();
	}

	public void redo() {
		redoing = true;
		isOpen = true;
		try {
			undoStack.add(new CompositeAction());
			if (!redoStack.isEmpty()) {
				CompensationAction action = redoStack.removeLast();
				toProtokoll(action, ProtokollItemType.REDO);
				action.run();
			}
			cleanupStack(undoStack);
		} catch (Throwable t) {
			UIUtil.showException(null, "Error", t);
		} finally {
			redoing = false;
			isOpen = false;
		}
		updateView();
	}

	private void cleanupStack(LinkedList<CompensationAction> stack) {
		if (!stack.isEmpty()) {
			if (stack.getLast() instanceof CompositeAction) {
				if (((CompositeAction) stack.getLast()).actions.isEmpty()) {
					stack.removeLast();
				}
			}
		}
	}

	public void reset() {
		undoStack.clear();
		redoStack.clear();
		updateView();
	}

	public void hideView() {
		protocol.clear();
		updateView();
	}

}
