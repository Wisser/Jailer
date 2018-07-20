package net.sf.jailer.ui.util;

import java.awt.Desktop;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.URI;
import java.net.URLConnection;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.SwingUtilities;

import net.sf.jailer.JailerVersion;
import net.sf.jailer.ui.Environment;

public class UpdateInfoManager {

	private static final String versionURL = "http://jailer.sourceforge.net/currentVersion";
	private static final String downloadURL = "https://sourceforge.net/projects/jailer/files/";
	private static final long CHECK_INTERVALL = 1000L * 60 * 60 * 46;
	private static final long DELAY = 1000L * 10;
	private static final String LAST_TS_FILE = ".lastcuats";
	private static boolean checked = false;

	public static void checkUpdateAvailability(final JComponent ui, final JLabel infoLabel) {
		ui.setVisible(false);

		if (checked) {
			return;
		}
		checked = true;
		
		Runnable check = new Runnable() {
			@Override
			public void run() {
				try {
					File lastTSFile = Environment.newFile(LAST_TS_FILE);
					if (lastTSFile.exists()) {
						ObjectInputStream in = new ObjectInputStream(new FileInputStream(lastTSFile));
						long lastTS = in.readLong();
						in.close();
						if (System.currentTimeMillis() < lastTS + CHECK_INTERVALL) {
							return;
						}
					}
					
					URI uri = new URI(versionURL);
					URLConnection connection = uri.toURL().openConnection();
			        BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
			        String inputLine = in.readLine();
			        in.close();
			        
			        if (inputLine != null && !inputLine.trim().isEmpty()) {
			        	final String[] versions = inputLine.trim().split(",");
			        	for (String version: versions) {
			        		if (version.trim().equals(JailerVersion.VERSION)) {
			        			return;
			        		}
			        	}
			        	if (isValid(versions[0].trim())) {
			        		Thread.sleep(DELAY);
							ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(lastTSFile));
							out.writeLong(System.currentTimeMillis());
							out.close();
							SwingUtilities.invokeLater(new Runnable() {
								@Override
								public void run() {
									infoLabel.setText("Release " + versions[0].trim() + " available");
									ui.setVisible(true);
								}
							});
			        	}
			        }
				} catch (Throwable t) {
					// ignore
				}
			}
		};
		Thread checkThread = new Thread(check);
		checkThread.setDaemon(true);
		checkThread.start();
	}

	private static boolean isValid(String version) {
		return version.matches("\\d+\\.\\d+(\\.\\d+)?");
	}

	public static void download() {
		try {
			URI uri = new URI(downloadURL);
			Desktop.getDesktop().browse(uri);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

}
