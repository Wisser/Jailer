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
package net.sf.jailer.ui.util;

import java.awt.Window;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.util.concurrent.Callable;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.SwingUtilities;
import javax.swing.Timer;

import net.sf.jailer.ui.Colors;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.util.CancellationException;

/**
 * Controls a concurrent task.
 *
 * @author Wisser
 */
@SuppressWarnings("serial")
public abstract class ConcurrentTaskControl extends javax.swing.JPanel {

    /**
     * Creates new form ConcurrentTaskControl
     * @param parent
     */
    public ConcurrentTaskControl(Window parent, String info) {
        initComponents(); UIUtil.initComponents(this);
        cancelButton.setIcon(UIUtil.scaleIcon(cancelButton, cancelIcon));
		
        infoLabel.setText(info);
        if (parent != null) {
        	parent.addWindowListener(new WindowListener() {
				@Override
				public void windowClosed(WindowEvent e) {
					cancel();
				}
				@Override
				public void windowOpened(WindowEvent e) {
				}
				@Override
				public void windowIconified(WindowEvent e) {
				}
				@Override
				public void windowDeiconified(WindowEvent e) {
				}
				@Override
				public void windowDeactivated(WindowEvent e) {
				}
				@Override
				public void windowClosing(WindowEvent e) {
				}
				@Override
				public void windowActivated(WindowEvent e) {
				}
			});
        }
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        infoLabel = new javax.swing.JLabel();
        cancelButton = new javax.swing.JButton();

        setLayout(new java.awt.GridBagLayout());

        infoLabel.setText("<html>1<br>2");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(8, 8, 8, 8);
        add(infoLabel, gridBagConstraints);

        cancelButton.setText("Cancel");
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 4);
        add(cancelButton, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
        cancel();
    }//GEN-LAST:event_cancelButtonActionPerformed

    private boolean cancelled;
    
    private void cancel() {
    	if (!cancelled) {
			cancelled = true;
			onCancellation();
    	}
	}

	/**
     * A task.
     */
	public interface Task {

		/**
		 * Runs the task.
		 */
		void run() throws Throwable;

	}

	/**
	 * Starts the task.
	 * 
	 * @param task the task
	 */
	public void start(final Task task) {
		cancelled = false;
		Thread thread = new Thread(new Runnable() {
			@Override
			public void run() {
				try {
					task.run();
					cancelled = true;
				} catch (final Throwable e) {
					if (!cancelled) {
						UIUtil.invokeLater(new Runnable() {
							@Override
							public void run() {
								onError(e);
							}
						});
					}
				}
			}
		});
		thread.setDaemon(true);
		thread.start();
	}

	/**
	 * Reacts on errors (called in AWT-Thread).
	 * 
	 * @param error the error
	 */
	protected abstract void onError(Throwable error);

	/**
	 * Reacts on cancellation (called in AWT-Thread).
	 */
	protected abstract void onCancellation();

    // Variables declaration - do not modify//GEN-BEGIN:variables
    public javax.swing.JButton cancelButton;
    public javax.swing.JLabel infoLabel;
    // End of variables declaration//GEN-END:variables

    public ConcurrentTaskControl master;
  
    private static Long fadeStart;
	private static Timer fadeTimer;
	
	public static void openInModalDialog(Window windowAncestor, final ConcurrentTaskControl concurrentTaskControl, final Task task, String title) {
		openInModalDialog(windowAncestor, concurrentTaskControl, task, title, null);
	}
	
	public static void openInModalDialog(Window windowAncestor, final ConcurrentTaskControl concurrentTaskControl, final Task task, String title, Consumer<JLabel> initInfoLabel) {
		final JDialog dialog = new JDialog(windowAncestor);
		dialog.setUndecorated(true);
		dialog.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
		dialog.setModal(true);
		dialog.setTitle(title);
		
		ConcurrentTaskControl control = new ConcurrentTaskControl(dialog, concurrentTaskControl.infoLabel.getText()) {
			@Override
			protected void onError(Throwable error) {
				concurrentTaskControl.onError(error);
			}
			@Override
			protected void onCancellation() {
				concurrentTaskControl.onCancellation();
			}
		};
		concurrentTaskControl.master = control;
		if (initInfoLabel != null) {
			initInfoLabel.accept(control.infoLabel);
		}

		control.setBorder(BorderFactory.createLineBorder(Colors.Color_128_128_128));
		dialog.getContentPane().add(control);
		dialog.pack();
 		dialog.setLocation(windowAncestor.getX() + (windowAncestor.getWidth() - dialog.getWidth()) / 2, Math.max(0, windowAncestor.getY() + (windowAncestor.getHeight() - dialog.getHeight()) / 2));
 		UIUtil.fit(dialog);
 		
 		control.start(new Task() {
			@Override
			public void run() throws Throwable {
				task.run();
			}
 		});
 		
 		fadeStart = System.currentTimeMillis();
		int fadeTime = 800;
		if (fadeTimer != null) {
			fadeTimer.stop();
		}
		fadeTimer = new Timer(50, e -> {
			if (fadeStart != null) {
				long dif = System.currentTimeMillis() - fadeStart;
				float h = Math.max(0f, Math.min(1f, -0.5f + dif / (float) fadeTime));
				UIUtil.setOpacity(dialog, h);
				if (h < 1) {
					return;
				}
				fadeTimer.stop();
			}
		});
		fadeTimer.setInitialDelay(1);
		fadeTimer.setRepeats(true);
		fadeTimer.start();
		
		UIUtil.setOpacity(dialog, 0f);
		dialog.setVisible(true);
	}

	/**
	 * Closes the window containing this component.
	 */
	public void closeWindow() {
		Window window = SwingUtilities.getWindowAncestor(master != null? master : this);
		if (window != null) {
			window.setVisible(false);
			window.dispose();
		}
	}

	/**
	 * Calls a {@link Callable} in a separate thread while showing a modal dialog.
	 */
	public static <T> T call(Window window, final Callable<T> call, String info, Consumer<JLabel> initInfoLabel) throws Exception {
		final AtomicReference<Exception> exception = new AtomicReference<Exception>();
		final AtomicReference<T> result = new AtomicReference<T>();
		final AtomicBoolean done = new AtomicBoolean(false);
		final ConcurrentTaskControl concurrentTaskControl = new ConcurrentTaskControl(window, info) {
			@Override
			protected void onError(Throwable error) {
				if (error instanceof Exception) {
					exception.set((Exception) error);
				} else {
					exception.set(new RuntimeException(error));
				}
				closeWindow();
			}
			@Override
			protected void onCancellation() {
				exception.set(new CancellationException());
				closeWindow();
			}
		};
		
		ConcurrentTaskControl.openInModalDialog(window, concurrentTaskControl, new ConcurrentTaskControl.Task() {
			@Override
			public void run() throws Throwable {
				try {
					result.set(call.call());
					done.set(true);
				} finally {
					UIUtil.invokeLater(100, new Runnable() {
						@Override
						public void run() {
							concurrentTaskControl.closeWindow();
						}
					});
				}
			}
		}, info, initInfoLabel);
		
		if (exception.get() != null) {
			throw exception.get();
		}
		if (!done.get()) {
			throw new CancellationException();
		}
		return result.get();
	}
	
	private static ImageIcon cancelIcon;
	
	static {
        // load images
        cancelIcon = UIUtil.readImage("/buttoncancel.png");
	}
}
