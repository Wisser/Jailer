package net.sf.jailer.util;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.util.Enumeration;
import java.util.Properties;

import org.apache.log4j.PropertyConfigurator;

public class LogUtil {

	public static void reloadLog4jConfig(File baseDir) {
		Properties props = new Properties();
		PrintStream oldErr = System.err;
		System.setErr(new PrintStream(new ByteArrayOutputStream()));
		try {
			props.load(LogUtil.class.getResourceAsStream("/log4j.properties"));
			Enumeration<?> e = props.propertyNames();
			while (e.hasMoreElements()) {
				String key = (String) e.nextElement();
				if (key.endsWith(".File")) {
					String p = props.getProperty(key);
					if (p != null) {
						String logfileName = p.trim();
						if (logfileName.length() > 0) {
							File logfile = new File(logfileName);
							if (!logfile.isAbsolute()) {
								props.setProperty(key, new File(baseDir, logfileName).getPath());
		                    }
						}
					}
				}
			}
			PropertyConfigurator.configure(props);
		} catch (Exception e) {
			System.setErr(oldErr);
			e.printStackTrace();
		} finally {
			System.setErr(oldErr);
		}
	}

	public static boolean testCreateTempFile() {
		try {
			File tempFile = new File("tmp", "tempfiletest"); // configuration.createTempFile();
			FileOutputStream out = new FileOutputStream(tempFile);
			out.write(0);
			out.close();
			tempFile.delete();
		} catch (Exception e) {
			return false;
		}
		return true;
	}

}
