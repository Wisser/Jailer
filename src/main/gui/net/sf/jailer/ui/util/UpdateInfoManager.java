package net.sf.jailer.ui.util;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.StringReader;
import java.net.URI;
import java.net.URLEncoder;
import java.util.Date;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.Timer;

import net.sf.jailer.JailerVersion;
import net.sf.jailer.ui.BrowserLauncher;
import net.sf.jailer.ui.DataModelManagerDialog;
import net.sf.jailer.ui.Environment;
import net.sf.jailer.ui.UIUtil;

public class UpdateInfoManager {

	private static final String versionURL = "http://jailer.sourceforge.net/currentVersion.php";
	private static final String downloadURL = "https://sourceforge.net/projects/jailer/files/";
	private static final long CHECK_INTERVALL = 1000L * 60 * 60 * 46;
	private static final long DELAY = 1000L * 10;
	private static final String LAST_TS_FILE = ".lastcuats";
	private static boolean checked = false;

	public static void checkUpdateAvailability(final JComponent ui, final JLabel infoLabel, final JMenuItem downloadMenuItem, final String modul) {
		ui.setVisible(false);

		if (checked) {
			return;
		}
		checked = true;
		
		Runnable check = new Runnable() {
			@Override
			public void run() {
				try {
					boolean inIntervall = true;
					File lastTSFile = Environment.newFile(LAST_TS_FILE);
					if (lastTSFile.exists()) {
						ObjectInputStream in = new ObjectInputStream(new FileInputStream(lastTSFile)); // lgtm [java/input-resource-leak]
						long lastTS = in.readLong();
						in.close();
						if (System.currentTimeMillis() < lastTS + CHECK_INTERVALL) {
							inIntervall = false;
						}
					}
					
					Object uuid = UISettings.restore("uuid");
					boolean isNew = false;
					if (uuid == null) {
						uuid = String.valueOf(System.currentTimeMillis() % 1000000);
						UISettings.store("uuid", uuid);
						UISettings.store("buser", true);
						isNew = true;
					}
					Object stat0 = UISettings.restore("stat0");
					UISettings.store("stat0", null);
					Object uimCnt = UISettings.restore("UIM-Cnt");
					if (uimCnt instanceof Long) {
						uimCnt = Long.valueOf(((Long) uimCnt) + 1);
					} else {
						uimCnt = Long.valueOf(isNew? 1 : 10000);
					}
					UISettings.store("UIM-Cnt", uimCnt);
					startShutdownHook();
					String content = HttpUtil.get(versionURL
							+ "?jversion=" + URLEncoder.encode(System.getProperty("java.version") + "/" + System.getProperty("java.vm.vendor") + "/" + System.getProperty("java.vm.name") + "/" + System.getProperty("os.name"), "UTF-8") + "/(" + Environment.state + ")"
							+ "&modul=" + URLEncoder.encode((DataModelManagerDialog.lastSessionRestored? modul.toLowerCase() : modul) + (UIUtil.subModule != 0? UIUtil.subModule : ""), "UTF-8")
							+ "&ts=" + URLEncoder.encode(new Date().toString(), "UTF-8")
							+ "&uuid=" + URLEncoder.encode(uuid.toString(), "UTF-8")
							+ "&version=" + URLEncoder.encode(JailerVersion.VERSION, "UTF-8")
							+ (stat0 != null? "&s=" + stat0 : "")
							+ ("&c=" + uimCnt)
							+ UISettings.restoreStats());
					BufferedReader in = new BufferedReader(new StringReader(content));
			        String inputLine = in.readLine();
			        in.close();
			        
			        if (inputLine != null && !inputLine.trim().isEmpty()) {
			        	final String[] versions = inputLine.trim().split(",");
			        	String currentVersion = JailerVersion.VERSION.replaceFirst("(\\d+\\.\\d+\\.\\d+)(\\.\\d+$)", "$1").replaceFirst("\\.0$", "");
			        	for (String version: versions) {
			        		if (version.trim().equals(currentVersion) || version.trim().equals(currentVersion  + ".0")) {
			        			return;
			        		}
			        	}
			        	if (isValid(versions[0].trim())) {
			        		currentDownloadableRelease = versions[0].trim();
			        		UIUtil.invokeLater(new Runnable() {
								@Override
								public void run() {
									if (downloadMenuItem != null) {
										downloadMenuItem.setText("Download Release " + versions[0].trim());
									}
								}
							});
			        		if (inIntervall) {
				        		Thread.sleep(DELAY);
								ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(lastTSFile)); // lgtm [java/output-resource-leak] 
								out.writeLong(System.currentTimeMillis());
								out.close();
								UIUtil.invokeLater(new Runnable() {
									@Override
									public void run() {
										infoLabel.setText("Release " + versions[0].trim() + " available");
										ui.setVisible(true);
										startAnimationTimer(ui);
									}

									int t;
									Color startColor = ui.getBackground();

									private void startAnimationTimer(final JComponent ui) {
										Timer timer = new Timer(50, new ActionListener() {
											@Override
											public void actionPerformed(ActionEvent e) {
												++t;
												Color destColor = new Color(255, 210, 180);
												double f = 0.5 * (1 + Math.sin(2 * Math.PI * t / 20.0));
												ui.setBackground(new Color(mid(f, startColor.getRed(), destColor.getRed()), mid(f, startColor.getGreen(), destColor.getGreen()), mid(f, startColor.getBlue(), destColor.getBlue())));
												
												if (ui.isVisible()) {
													startAnimationTimer(ui);
												}
											}
											private int mid(double f, int s, int d) {
												return (int) (s + f * (d - s));
											}
										});
										timer.setRepeats(false);
										timer.start();
									}
								});
			        		}
			        	}
			        }
				} catch (Throwable t) {
					// ignore
				}
			}

			private void startShutdownHook() {
				Runtime.getRuntime().addShutdownHook(new Thread("cleanup") {
					long startTime = System.currentTimeMillis();
					
					@Override
					public void run() {
						if (startTime != 0) {
							UISettings.store("stat0", (System.currentTimeMillis() - startTime) / 1000 / 60);
							UISettings.storeStats();
						}
						startTime = 0;
					}
				});
			}
		};
		Thread checkThread = new Thread(check);
		checkThread.setDaemon(true);
		checkThread.start();
	}

	private static boolean isValid(String version) {
		return version.matches("\\d+\\.\\d+(\\.\\d+)?");
	}

	public static String currentDownloadableRelease = null;

	public static void download() {
		try {
			URI uri = new URI(downloadURL);
			BrowserLauncher.openURL(uri, null);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

}

