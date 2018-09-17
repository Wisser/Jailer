package net.sf.jailer.ui.util;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.SwingUtilities;

public abstract class SmallButton extends JLabel {

	public SmallButton(Icon icon) {
		super(icon);
		setEnabled(false);
		addMouseListener(new MouseListener() {
			@Override
			public void mouseReleased(MouseEvent e) {
				if (SwingUtilities.isLeftMouseButton(e)) {
					onClick();
				}
			}
			@Override
			public void mousePressed(MouseEvent e) {
			}
			@Override
			public void mouseExited(MouseEvent e) {
				setEnabled(false);
			}
			@Override
			public void mouseEntered(MouseEvent e) {
				setEnabled(true);
			}
			@Override
			public void mouseClicked(MouseEvent e) {
				if (SwingUtilities.isLeftMouseButton(e)) {
					onClick();
				}
			}
		});
	}

	protected abstract void onClick();
	
}
