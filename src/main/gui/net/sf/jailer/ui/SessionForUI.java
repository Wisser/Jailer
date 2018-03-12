/*
 * Copyright 2007 - 2018 the original author or authors.
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
package net.sf.jailer.ui;

import java.awt.Point;
import java.awt.Window;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import javax.sql.DataSource;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.Session;

/**
 * Specialized {@link Session} for the UI.
 * 
 * @author Ralf Wisser
 */
public class SessionForUI extends Session {

	/**
	 * Creates a new session.
	 * 
	 * @param dataSource the data source
	 * @param dbms the DBMS
	 */
	public static SessionForUI createSession(DataSource dataSource, DBMS dbms, final Window w) throws SQLException {
		final SessionForUI session = new SessionForUI(dataSource, dbms);
		final AtomicReference<Connection> con = new AtomicReference<Connection>();
		final AtomicReference<Throwable> exception = new AtomicReference<Throwable>();
		session.connectionDialog = new JDialog(w, "Connecting");
		session.connectionDialog.setModal(true);
		Thread thread = new Thread(new Runnable() {
			@Override
			public void run() {
				try {
					Connection newCon = session.connectionFactory.getConnection();
					if (session.cancelled.get()) {
						try {
							newCon.close();
						} catch (SQLException e) {
							// ignore
						}
					} else {
						con.set(newCon);
					}
				} catch (Throwable e) {
					exception.set(e);
				}
				SwingUtilities.invokeLater(new Runnable() {
					@Override
					public void run() {
						session.connectionDialog.setVisible(false);
					}
				});
			}
		});
		thread.setDaemon(true);
		thread.start();
		Point p;
		if (w.isShowing()) {
			p = w.getLocationOnScreen();
		} else {
			p = new Point(0, 0);
		}
		final Point los = p;
		session.connectionDialog.getContentPane().add(session.connectingPanel);
		session.connectionDialog.pack();
		session.connectionDialog.setLocation(los.x + w.getWidth() / 2 - session.connectionDialog.getWidth() / 2, los.y + w.getHeight() / 2 - session.connectionDialog.getHeight() / 2);
		session.connectionDialog.setVisible(true);
		session.connectionDialog.dispose();
		Throwable throwable = exception.get();
		if (throwable  != null) {
			if (throwable instanceof SQLException) {
				throw (SQLException) throwable;
			}
			if (throwable instanceof RuntimeException) {
				throw (RuntimeException) throwable;
			}
			throw new RuntimeException(throwable);
		}
		if (con.get() != null) {
			session.connection.set(con.get());
			return session;
		}
		return null;
	}

	private JPanel connectingPanel = new JPanel();
	private JLabel jLabel1 = new JLabel();
	private JButton cancelConnectingButton = new JButton();
	private AtomicBoolean cancelled = new AtomicBoolean(false);
	private JDialog connectionDialog;

	/**
	 * Constructor.
	 */
	private SessionForUI(DataSource dataSource, DBMS dbms) throws SQLException {
		super(dataSource, dbms);
		connectingPanel.setBackground(java.awt.Color.white);

        jLabel1.setForeground(java.awt.Color.red);
        jLabel1.setText("connecting...");
        connectingPanel.add(jLabel1);

        cancelConnectingButton .setText("Cancel");
        cancelConnectingButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
            	cancelled.set(true);
            	connectionDialog.setVisible(false);
            }
        });
        connectingPanel.add(cancelConnectingButton);
	}

	protected void init() throws SQLException {
	}
	
	/**
	 * Closes all connections asynchronously.
	 */
	@Override
	public void shutDown() throws SQLException {
		Thread thread = new Thread(new Runnable() {
			@Override
			public void run() {
				try {
					SessionForUI.super.shutDown();
				} catch (SQLException e) {
					e.printStackTrace();
				}
			}
		});
		thread.setDaemon(true);
		thread.start();
	}

}
