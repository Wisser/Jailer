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
package net.sf.jailer.ui.progress;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.lang.ref.WeakReference;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.concurrent.Callable;
import java.util.function.Consumer;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.WindowConstants;

import net.sf.jailer.datamodel.Table;
import net.sf.jailer.entitygraph.RowOriginStep;
import net.sf.jailer.ui.UIUtil;

/**
 * Shows the origin of a single row which is already known, in a window of its own.
 * <p>
 * The counterpart to {@link RowOriginDialog}: there a row is picked from the rows collected
 * through an association, here the row comes from wherever the question has been asked - from
 * the Data Browser, for instance. The window is not modal and there is at most one per owner,
 * so that one can keep browsing and ask again for the next row.
 * <p>
 * As long as such a window is open, the retained rows are needed. It therefore announces itself
 * to {@link RetainedEntityGraphs} and gives them up again when it is closed.
 *
 * @author Ralf Wisser
 */
public class RowOriginWindow extends JDialog {

	/**
	 * At most one window per owner. Weak, so that a closed owner does not keep its window alive.
	 */
	private static final Map<Window, RowOriginWindow> windows = new WeakHashMap<Window, RowOriginWindow>();

	/**
	 * The key of this window in {@link #windows}, weakly, so that a value cannot keep its own
	 * key alive.
	 */
	private final WeakReference<Window> ownerRef;

	private final RowOriginContext context;
	private final RowOriginPanel originPanel;

	private boolean released = false;

	/**
	 * Shows the origin of a row, in a new window or in the one already open for that owner.
	 *
	 * @param owner the owner window
	 * @param context the context to ask
	 * @param table the table of the row, as of the data model of the run
	 * @param primaryKeySupplier delivers the primary key values of the row, off the event
	 *        dispatch thread
	 * @param rowTitle names the row, for the title of the window
	 * @param pathOpener lays the chain out in a Data Browser, or <code>null</code>
	 */
	public static void open(Window owner, RowOriginContext context, Table table,
			Callable<Object[]> primaryKeySupplier, String rowTitle, Consumer<List<RowOriginStep>> pathOpener) {
		RowOriginWindow window = windows.get(owner);
		if (window != null && window.context != context) {
			window.releaseAndDispose();
			window = null;
		}
		if (window == null) {
			window = new RowOriginWindow(owner, context);
			windows.put(owner, window);
		}
		// the window is reused, so the opener is set anew each time
		window.originPanel.setPathOpener(pathOpener);
		window.showRow(table, primaryKeySupplier, rowTitle);
	}

	private RowOriginWindow(Window owner, RowOriginContext context) {
		super(owner, ModalityType.MODELESS);
		this.ownerRef = new WeakReference<Window>(owner);
		this.context = context;
		this.originPanel = new RowOriginPanel(context);

		JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		JButton closeButton = new JButton(" Close ");
		closeButton.setIcon(UIUtil.scaleIcon(closeButton, UIUtil.readImage("/buttoncancel.png")));
		closeButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				setVisible(false);
				dispose();
			}
		});
		buttonPanel.add(closeButton);

		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(originPanel, BorderLayout.CENTER);
		getContentPane().add(buttonPanel, BorderLayout.SOUTH);

		setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		UIUtil.setDialogSize(this, 700, 400);
		setLocationRelativeTo(owner);

		// the retained rows are needed for as long as this window is open
		RetainedEntityGraphs.addUser();
		addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosed(WindowEvent e) {
				release();
			}
		});
	}

	private void showRow(Table table, Callable<Object[]> primaryKeySupplier, String rowTitle) {
		setTitle("Origin of " + rowTitle);
		if (!isVisible()) {
			setVisible(true);
		}
		toFront();
		originPanel.showOrigin(table, primaryKeySupplier);
	}

	/**
	 * Gives up the retained rows, at most once per window. If nobody else needs them any more
	 * and a discard has been asked for, this is where it happens.
	 */
	private void release() {
		boolean release;
		synchronized (this) {
			release = !released;
			released = true;
		}
		if (release) {
			Window owner = ownerRef.get();
			if (owner != null && windows.get(owner) == this) {
				windows.remove(owner);
			}
			RetainedEntityGraphs.removeUser();
		}
	}

	private void releaseAndDispose() {
		release();
		setVisible(false);
		dispose();
	}

}
