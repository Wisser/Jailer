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
package net.sf.jailer.ui.databrowser;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dialog;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.WindowConstants;

import net.sf.jailer.ui.UIUtil;

/**
 * Opens a number of table browsers one per event of the AWT event queue instead of all in one go.
 * <p>
 * Opening many browsers in a single event blocks the user interface until the last one is there,
 * with no way to stop it. Here the event queue gets a turn after each browser, so the desktop stays
 * operable, and a progress window (shown only if it takes a while) lets the user cancel. The browsers
 * opened up to then stay and are arranged.
 *
 * @author Ralf Wisser
 */
class StepwiseBrowserOpener {

	/** Delay after which the progress window appears, in milliseconds. */
	private static final int PROGRESS_DELAY = 500;

	/** <code>true</code> while a run is in progress, so that two runs don't mix. */
	private static boolean running = false;

	private final List<ActionListener> actions;
	private final List<String> names;
	private final ActionEvent event;
	private final Component parent;
	private final Runnable onCancel;
	private final Runnable onFinish;

	private int next = 0;
	private boolean cancelled = false;
	private Timer progressTimer;
	private JDialog progressDialog;
	private JLabel progressLabel;
	private JProgressBar progressBar;

	/**
	 * @param actions opens one table browser each
	 * @param names names of the tables, parallel to <code>actions</code> (for the progress window)
	 * @param event the event to pass to the actions
	 * @param parent component of the window the progress window belongs to
	 * @param onCancel called after a cancellation, once the browsers opened so far are there
	 * @param onFinish called at the end, after a cancellation too (after <code>onCancel</code>)
	 */
	StepwiseBrowserOpener(List<ActionListener> actions, List<String> names, ActionEvent event, Component parent,
			Runnable onCancel, Runnable onFinish) {
		this.actions = actions;
		this.names = names;
		this.event = event;
		this.parent = parent;
		this.onCancel = onCancel;
		this.onFinish = onFinish;
	}

	/**
	 * @return <code>true</code> while table browsers are being opened
	 */
	static boolean isRunning() {
		return running;
	}

	/**
	 * Starts opening. Returns at once, the browsers are opened by subsequent events.
	 *
	 * @return <code>false</code> if nothing has been started (another run is in progress, or nothing to open);
	 *         then neither <code>onCancel</code> nor <code>onFinish</code> will be called
	 */
	boolean start() {
		if (running || actions.isEmpty()) {
			return false;
		}
		running = true;
		UIUtil.setWaitCursor(parent);
		Desktop.resetLastArrangeLayoutOnNewTableBrowser();
		progressTimer = new Timer(PROGRESS_DELAY, e -> showProgress());
		progressTimer.setRepeats(false);
		progressTimer.start();
		UIUtil.invokeLater(this::step);
		return true;
	}

	private void step() {
		if (cancelled) {
			finish();
			return;
		}
		int i = next++;
		updateProgress(i);
		boolean last = i == actions.size() - 1;
		try {
			// only the last browser arranges the desktop; the flags are reset right away, so that
			// the events processed between two steps see the normal state
			Desktop.noArrangeLayoutOnNewTableBrowser = !last;
			Desktop.noArrangeLayoutOnNewTableBrowserWithAnchor = actions.size() > 1;
			actions.get(i).actionPerformed(event);
		} catch (RuntimeException e) {
			cancelled = true;
			finish();
			throw e;
		} finally {
			Desktop.noArrangeLayoutOnNewTableBrowser = false;
			Desktop.noArrangeLayoutOnNewTableBrowserWithAnchor = false;
		}
		if (last) {
			finish();
		} else {
			UIUtil.invokeLater(this::step);
		}
	}

	private void finish() {
		if (!running) {
			return;
		}
		running = false;
		if (progressTimer != null) {
			progressTimer.stop();
		}
		if (progressDialog != null) {
			progressDialog.dispose();
			progressDialog = null;
		}
		UIUtil.resetWaitCursor(parent);
		if (cancelled && next > 0 && onCancel != null) {
			onCancel.run();
		}
		if (onFinish != null) {
			onFinish.run();
		}
	}

	private void cancel() {
		cancelled = true;
		if (progressLabel != null) {
			progressLabel.setText("Cancelling...");
		}
	}

	private void showProgress() {
		if (!running || cancelled) {
			return;
		}
		Window owner = parent == null ? null
				: parent instanceof Window ? (Window) parent : SwingUtilities.getWindowAncestor(parent);
		if (owner instanceof Frame) {
			progressDialog = new JDialog((Frame) owner, "Opening table browsers", false);
		} else if (owner instanceof Dialog) {
			progressDialog = new JDialog((Dialog) owner, "Opening table browsers", false);
		} else {
			progressDialog = new JDialog((Frame) null, "Opening table browsers", false);
		}
		progressLabel = new JLabel();
		progressBar = new JProgressBar(0, actions.size());
		JButton cancelButton = new JButton("Cancel");
		cancelButton.setIcon(UIUtil.scaleIcon(cancelButton, UIUtil.readImage("/buttoncancel.png")));
		cancelButton.addActionListener(e -> cancel());

		JPanel content = new JPanel(new BorderLayout(0, 8));
		content.setBorder(BorderFactory.createEmptyBorder(12, 12, 8, 12));
		content.add(progressLabel, BorderLayout.NORTH);
		content.add(progressBar, BorderLayout.CENTER);
		JPanel buttons = new JPanel(new FlowLayout(FlowLayout.RIGHT, 0, 0));
		buttons.add(cancelButton);
		content.add(buttons, BorderLayout.SOUTH);
		progressDialog.setContentPane(content);
		progressDialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
		progressDialog.addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosing(WindowEvent e) {
				cancel();
			}
		});
		progressDialog.getRootPane().registerKeyboardAction(e -> cancel(),
				KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), JComponent.WHEN_IN_FOCUSED_WINDOW);
		progressDialog.getRootPane().setDefaultButton(cancelButton);
		updateProgress(Math.max(0, next - 1));
		progressDialog.pack();
		progressDialog.setSize(Math.max(progressDialog.getWidth(), 420), progressDialog.getHeight());
		progressDialog.setLocationRelativeTo(owner);
		progressDialog.setVisible(true);
	}

	private void updateProgress(int i) {
		if (progressLabel == null || cancelled) {
			return;
		}
		String name = i < names.size() ? names.get(i) : "";
		progressLabel.setText("Opening " + (i + 1) + " of " + actions.size() + (name.isEmpty() ? "" : ": " + name));
		progressBar.setValue(i);
	}
}
