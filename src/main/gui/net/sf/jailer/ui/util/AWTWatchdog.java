package net.sf.jailer.ui.util;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.management.LockInfo;
import java.lang.management.ManagementFactory;
import java.lang.management.MonitorInfo;
import java.lang.management.ThreadInfo;
import java.lang.management.ThreadMXBean;
import java.util.regex.Pattern;

import javax.swing.SwingUtilities;

import net.sf.jailer.JailerVersion;
import net.sf.jailer.database.Session;
import net.sf.jailer.ui.UIUtil;

public class AWTWatchdog {

	private static final long MAX_DELAY = 8000;

	public static void start() {
		try {
			Thread watchdog = new Thread(new Runnable() {
				@Override
				public void run() {
					boolean issueSent = false;
					for (;;) {
						try {
							long t = System.currentTimeMillis();
							Thread.sleep(1000);
							if (t + 5000 < System.currentTimeMillis()) {
								setStarttime(0);
								continue;
							}
						} catch (InterruptedException e) {
							// ignore
						}
						long st = getStarttime();
						if (st != 0 && st + MAX_DELAY < System.currentTimeMillis()) {
							if (!issueSent) {
								String dump;
								try {
									dump = sendThreadDump();
								} catch (Throwable t) {
									StringWriter sw = new StringWriter();
							        PrintWriter pw = new PrintWriter(sw);
							        t.printStackTrace(pw);
							        dump = sw.toString();
								}
								dump = JailerVersion.VERSION + " " + dump;
								Session._log.error(dump);
					            String iMsg = dump;
								UIUtil.sendIssue("AWTHanging", iMsg);
								issueSent = true;
							}
						} else {
							if (issueSent) {
								String iMsg = "AWT-Thread working";
								Session._log.info(iMsg);
								UIUtil.sendIssue("AWTHanging", iMsg);
								return;
							}
						}
						if (st == 0) {
							setStarttime(System.currentTimeMillis());
							SwingUtilities.invokeLater(new Runnable() {
								@Override
								public void run() {
									setStarttime(0);
								}
							});
						}
						
					}
				}
			}, "JAWT-WD");
			watchdog.setDaemon(true);
			watchdog.start();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	protected static String sendThreadDump() {
		ThreadMXBean threadMxBean = ManagementFactory.getThreadMXBean();
		for (ThreadInfo ti : threadMxBean.dumpAllThreads(true, true)) {
			if (ti.getThreadName() != null && ti.getThreadName().toLowerCase().startsWith("awt-event")) {

				StringBuilder sb = new StringBuilder(
						"\"" + ti.getThreadName() + "\"" + " Id=" + ti.getThreadId() + " " + ti.getThreadState());
				if (ti.getLockName() != null) {
					sb.append(" on " + ti.getLockName());
				}
				if (ti.getLockOwnerName() != null) {
					sb.append(" owned by \"" + ti.getLockOwnerName() + "\" Id=" + ti.getLockOwnerId());
				}
				if (ti.isSuspended()) {
					sb.append(" (suspended)");
				}
				if (ti.isInNative()) {
					sb.append(" (in native)");
				}
				sb.append('\n');
				final String pck = "net.sf.jailer";
				final String pckPtrn = "net\\.sf\\.jailer";
				final String mrk = "nonjlrmrk";
				int i = 0;
				for (; i < ti.getStackTrace().length; i++) {
					StackTraceElement ste = ti.getStackTrace()[i];
					
					String s = ste.getClassName() + "." + ste.getMethodName() + "(" +
			             (ste.isNativeMethod() ? "Native Method)" :
			              (ste.getFileName() != null &&ste.getLineNumber() >= 0 ?
			            		  ste.getFileName() + ":" + ste.getLineNumber() + ")" :
			                (ste.getFileName() != null ?  "" + ste.getFileName() + ")" : "Unknown Source)")));
					
					sb.append(" at " + s);
					sb.append('\n');
					if (i == 0 && ti.getLockInfo() != null) {
						Thread.State ts = ti.getThreadState();
						switch (ts) {
						case BLOCKED:
							sb.append("\t-  blocked on " + ti.getLockInfo());
							sb.append('\n');
							break;
						case WAITING:
							sb.append("\t-  waiting on " + ti.getLockInfo());
							sb.append('\n');
							break;
						case TIMED_WAITING:
							sb.append("\t-  waiting on " + ti.getLockInfo());
							sb.append('\n');
							break;
						default:
						}
					}

					for (MonitorInfo mi : ti.getLockedMonitors()) {
						if (mi.getLockedStackDepth() == i) {
							sb.append("\t-  locked " + mi);
							sb.append('\n');
						}
					}
					if (i == 0 && !ste.toString().contains(pck)) {
						sb.append(mrk);
					}
				}
				if (i < ti.getStackTrace().length) {
					sb.append("\t...");
					sb.append('\n');
				}

				LockInfo[] locks = ti.getLockedSynchronizers();
				if (locks.length > 0) {
					sb.append("\n\tNumber of locked synchronizers = " + locks.length);
					sb.append('\n');
					for (LockInfo li : locks) {
						sb.append("\t- " + li);
						sb.append('\n');
					}
				}
				sb.append('\n');

				String dump = Pattern.compile(mrk + "(?d)(.*?)\\b" + pckPtrn, Pattern.DOTALL).matcher(sb.toString()).replaceFirst(".. at " + pck);
				System.err.print("Error: AWT-Thread hanging: " + dump);
				return dump;
			}
		}
		return "no awt-thread?";
	}

	private static long starttime = 0;

	private synchronized static long getStarttime() {
		return starttime;
	}

	private synchronized static void setStarttime(long starttime) {
		AWTWatchdog.starttime = starttime;
	}

}
