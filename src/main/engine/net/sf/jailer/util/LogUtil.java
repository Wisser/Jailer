package net.sf.jailer.util;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.PrintStream;
import java.util.Enumeration;
import java.util.Properties;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.core.Appender;
import org.apache.logging.log4j.core.LoggerContext;
import org.apache.logging.log4j.core.config.Configuration;

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
			
			LoggerContext context = (org.apache.logging.log4j.core.LoggerContext) LogManager.getContext(false);
			File file = new File("path/to/a/different/log4j2.xml");

			// this will force a reconfiguration
//			Configuration c = context.getConfiguration();
//			c.getAppenders().values().forEach(a -> {
//				if (a instanceof Appender)
//			});
			
//			PropertyConfigurator.configure(props);
		} catch (Exception e) {
			System.setErr(oldErr);
			e.printStackTrace();
		} finally {
			System.setErr(oldErr);
		}
	}

	public interface Warn {
		void warn(Throwable t);
	}
	
	private static Warn warn = null;
	
	public static synchronized void setWarn(Warn warn) {
		LogUtil.warn = warn;
	}

	public static synchronized void warn(Throwable t) {
		t.printStackTrace();
		if (warn != null) {
			warn.warn(t);
		}
	}

}
