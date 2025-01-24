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
package net.sf.jailer.ui.databrowser;

import java.awt.Component;
import java.awt.Point;
import java.beans.PropertyVetoException;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Optional;
import java.util.Stack;
import java.util.function.Function;

import javax.swing.JButton;
import javax.swing.JMenuItem;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.ui.Colors;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.Desktop.LayoutMode;
import net.sf.jailer.ui.databrowser.Desktop.RowBrowser;
import net.sf.jailer.ui.util.UISettings;
import net.sf.jailer.util.LogUtil;

/**
 * Desktop undo/redo manager.
 * 
 * @author Ralf Wisser
 */
public class DesktopUndoManager {

	private final JButton undoButton;
	private final JButton redoButton;
	private final JMenuItem undoMenuItem;
	private final JMenuItem redoMenuItem;
	private final DataBrowser dataBrowser;
	private final JScrollPane scrollPane;
	private boolean restoring = false;
	private boolean enabled = true;

	private class State {
		final Point position;
		final File layout;
		final String fileContent;
		final String undoDescription;
		final String redoDescription;
		final String selectedTable;
		int numMods = 1;
		
		State (Point position, File layout, String fileContent, String undoDescription, String redoDescription, String selectedTable) {
			this.position = position;
			this.layout = layout;
			this.fileContent = fileContent;
			this.undoDescription = undoDescription;
			this.redoDescription = redoDescription;
			this.selectedTable = selectedTable;
		}
	}

	private Stack<State> undoStack = new Stack<State>();
	private Stack<State> redoStack = new Stack<State>();

	public DesktopUndoManager(JButton undoButton, JButton redoButton, JMenuItem undoMenuItem, JMenuItem redoMenuItem, DataBrowser dataBrowser, JScrollPane scrollPane) {
		this.undoButton = undoButton;
		this.redoButton = redoButton;
		this.undoMenuItem = undoMenuItem;
		this.redoMenuItem = redoMenuItem;
		this.dataBrowser = dataBrowser;
		this.scrollPane = scrollPane;
		undoButton.addActionListener(e -> undo());
		undoMenuItem.addActionListener(e -> undo());
		redoButton.addActionListener(e -> redo());
		redoMenuItem.addActionListener(e -> redo());
		updateUI();
	}

	private void undo() {
		if (!undoStack.isEmpty()) {
			State state = undoStack.pop();
			try {
				State currentState = captureState(state.undoDescription, state.redoDescription);
				currentState.numMods = state.numMods;
				redoStack.push(currentState);
			} catch (Throwable t) {
				LogUtil.warn(t);
				undoStack.clear();
				redoStack.clear();
				return;
			}
			Component pFrame = SwingUtilities.getWindowAncestor(dataBrowser.desktop);
			if (pFrame == null) {
				pFrame = dataBrowser;
			}
			String sFile = state.layout.getAbsolutePath();
			try {
				BufferedWriter out = new BufferedWriter(new FileWriter(state.layout));
				out.append(state.fileContent);
				out.close();
				
				restoring = true;
				LayoutMode layoutMode = dataBrowser.desktop.layoutMode;
				dataBrowser.desktop.restoreSession(null, pFrame, sFile, false);
				
				if (!state.layout.delete()) {
					state.layout.deleteOnExit();
				}
				
				if (state.selectedTable != null) {
					Optional<RowBrowser> opt = dataBrowser.desktop.tableBrowsers.stream().filter(b -> b.internalFrame != null && state.selectedTable.equals(b.getTitle())).findAny();
					if (opt.isPresent()) {
						try {
							opt.get().internalFrame.setSelected(true);
						} catch (PropertyVetoException e) {
							// ignore
						}
					}
				}
				if (layoutMode == dataBrowser.desktop.layoutMode) {
				 	scrollPane.getViewport().setViewPosition(state.position);
		        }
				UISettings.s7.addAndGet(100000);
				UIUtil.invokeLater(8, () -> {
					if (layoutMode != dataBrowser.desktop.layoutMode) {
						dataBrowser.desktop.rescaleLayout(layoutMode, null);
					} else {
			        	scrollPane.getViewport().setViewPosition(state.position);
			        }
				});
			} catch (Throwable e) {
				UIUtil.showException(pFrame, "Error", e);
			} finally {
				restoring = false;
			}
			updateUI();
		}
	}
	
	private void redo() {
		Runnable swapStacks = () -> {
			Stack<State> h;
			h = redoStack;
			redoStack = undoStack;
			undoStack = h;
		};
		swapStacks.run();
		undo();
		swapStacks.run();
		updateUI();
	}

	void updateUI() {
		undoButton.setEnabled(!undoStack.isEmpty());
		undoMenuItem.setEnabled(!undoStack.isEmpty());
		undoButton.setToolTipText("<html>Go Back" + toHTMLList(undoStack, e -> e.undoDescription) + "</html>");
		redoButton.setEnabled(!redoStack.isEmpty());
		redoMenuItem.setEnabled(!redoStack.isEmpty());
		redoButton.setToolTipText("<html>Go Forward" + toHTMLList(redoStack, e -> e.redoDescription) + "</html>");
	}

	private String toHTMLList(Stack<State> stack, Function<State, String> descriptionSupplier) {
		StringBuilder sb = new StringBuilder();
		for (int i = 1; i <= stack.size(); ++i) { 
			State e = stack.get(stack.size() - i);
			if (i == 1) {
				sb.append("<hr>");
			} else {
				sb.append("<font color=" + Colors.HTMLColor_707080 + ">");
			}
			int rest = stack.size() - i + 1;
			if (i > 8 && rest > 2) {
				sb.append(rest + " more");
				break;
			}
			String desc = descriptionSupplier.apply(e);
			sb.append(desc + (e.numMods > 1 && !desc.endsWith(" ")? " + " + (e.numMods - 1) + " more" : "") + "<br>");
			if (i == 1) {
				sb.append("");
			} else {
				sb.append("</font>");
			}
		}
		return sb.toString();
	}

	private int numPending = 0;
	
	public void beforeModification(String undoDescription, String redoDescription) {
		if (!restoring && enabled) {
			if (numPending > 0) {
				++numPending;
			} else {
				try {
					numPending = 1;
					State state = captureState(undoDescription, redoDescription);
					UIUtil.invokeLater(32, () -> {
						state.numMods = numPending;
						undoStack.push(state);
						redoStack.clear();
						numPending = 0;
						updateUI();
					});
				} catch (Throwable t) {
					LogUtil.warn(t);
					undoStack.clear();
					redoStack.clear();
				}
			}
		}
	}

	private State captureState(String undoDescription, String redoDescription) throws IOException {
		File file = Configuration.getInstance().createTempFile();
		dataBrowser.desktop.storeSession(file.getPath());
		BufferedReader in = new BufferedReader(new FileReader(file));
		int c;
		StringBuilder sb = new StringBuilder();
		while ((c = in.read()) != -1) {
			sb.append((char) c);
		}
		in.close();
		if (!file.delete()) {
			file.deleteOnExit();
		}
		String selectedTable = null;
		Optional<RowBrowser> opt = dataBrowser.desktop.tableBrowsers.stream().filter(b -> b.internalFrame != null && b.internalFrame.isSelected()).findAny();
		if (opt.isPresent()) {
			selectedTable = opt.get().getTitle();
		}
		State state = new State(scrollPane.getViewport().getViewPosition(), file, sb.toString(), undoDescription, redoDescription, selectedTable);
		return state;
	}

	public void beforeRestore() {
		if (!restoring) {
			enabled = false;
			undoStack.clear();
			redoStack.clear();
			updateUI();
		}
	}

	public void afterRestore() {
		if (!restoring) {
			enabled = true;
		}
	}

	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
		if (!enabled) {
			undoStack.clear();
			redoStack.clear();
			updateUI();
		}
	}

}
