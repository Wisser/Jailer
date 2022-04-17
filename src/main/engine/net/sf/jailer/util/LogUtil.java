package net.sf.jailer.util;

import java.io.File;

public class LogUtil {

	public static void initLog4jConfig(File baseDir) {
		System.setProperty("logdir", baseDir != null? baseDir.getAbsolutePath() + File.separator : "");
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
