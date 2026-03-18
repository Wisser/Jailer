package net.sf.jailer.util;

import java.io.File;

/**
 * Logging utilities.
 *
 * @author Ralf Wisser
 */
public class LogUtil {

	/**
	 * Initializes the log4j configuration with the given base directory.
	 *
	 * @param baseDir the base directory to use as the log directory, or <code>null</code> for the current directory
	 */
	public static void initLog4jConfig(File baseDir) {
		System.setProperty("logdir", baseDir != null? baseDir.getAbsolutePath() + File.separator : "");
	}

	/**
	 * Callback interface for warning notifications.
	 */
	public interface Warn {
		/**
		 * Called when a warning occurs.
		 *
		 * @param t the throwable that triggered the warning
		 */
		void warn(Throwable t);
	}

	private static Warn warn = null;

	/**
	 * Sets the warning callback.
	 *
	 * @param warn the warning callback, or <code>null</code> to clear it
	 */
	public static synchronized void setWarn(Warn warn) {
		LogUtil.warn = warn;
	}

	/**
	 * Issues a warning for the given throwable.
	 * Prints the stack trace and notifies the registered {@link Warn} callback.
	 *
	 * @param t the throwable to warn about
	 */
	public static synchronized void warn(Throwable t) {
		t.printStackTrace();
		if (warn != null) {
			warn.warn(t);
		}
	}

}
