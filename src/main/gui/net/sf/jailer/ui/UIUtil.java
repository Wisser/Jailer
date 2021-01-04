/*
 * Copyright 2007 - 2021 Ralf Wisser.
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

import java.awt.Component;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.FileDialog;
import java.awt.Font;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Constructor;
import java.net.URL;
import java.net.URLEncoder;
import java.text.DateFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.WeakHashMap;
import java.util.concurrent.Callable;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JTable;
import javax.swing.RowSorter;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;

import org.apache.log4j.Logger;
import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.Jailer;
import net.sf.jailer.JailerVersion;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.BasicDataSource;
import net.sf.jailer.database.PrimaryKeyValidator;
import net.sf.jailer.database.Session;
import net.sf.jailer.database.SqlException;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ddl.DDLCreator;
import net.sf.jailer.extractionmodel.ExtractionModel.IncompatibleModelException;
import net.sf.jailer.progress.ProgressListener;
import net.sf.jailer.subsetting.RowLimitExceededException;
import net.sf.jailer.ui.databrowser.BrowserContentPane.TableModelItem;
import net.sf.jailer.ui.databrowser.DetailsView;
import net.sf.jailer.ui.databrowser.Row;
import net.sf.jailer.ui.scrollmenu.JScrollC2PopupMenu;
import net.sf.jailer.ui.scrollmenu.JScrollPopupMenu;
import net.sf.jailer.ui.syntaxtextarea.RSyntaxTextAreaWithSQLSyntaxStyle;
import net.sf.jailer.ui.util.ConcurrentTaskControl;
import net.sf.jailer.ui.util.HttpUtil;
import net.sf.jailer.ui.util.UISettings;
import net.sf.jailer.util.CancellationException;
import net.sf.jailer.util.CancellationHandler;
import net.sf.jailer.util.CycleFinder;
import net.sf.jailer.util.JobManager;
import net.sf.jsqlparser.parser.CCJSqlParser;

/**
 * Some utility methods.
 *
 * @author Ralf Wisser
 */
public class UIUtil {

    /**
     * The logger.
     */
    private static final Logger _log = Logger.getLogger(UIUtil.class);

    /**
     * Opens file chooser.
     *
     * @param selectedFile
     *            if not <code>null</code> this file will be selected initially
     * @param startDir
     *            directory to start with
     * @param description
     *            description of file to chose
     */
    public static String choseFile(File selectedFile, String startDir, final String description, final String extension,
            Component parent, boolean addExtension, boolean forLoad) {
        return choseFile(selectedFile, startDir, description, extension, parent, addExtension, forLoad, true);
    }

    /**
     * Opens file chooser.
     *
     * @param selectedFile
     *            if not <code>null</code> this file will be selected initially
     * @param startDir
     *            directory to start with
     * @param description
     *            description of file to chose
     */
    public static String choseFile(File selectedFile, String startDir, final String description, final String extension,
            Component parent, boolean addExtension, boolean forLoad, final boolean allowZip) {
        String newStartDir = restoreCurrentDir(extension);
        if (newStartDir != null) {
            startDir = newStartDir;
            try {
                if (!new File(startDir).isDirectory()) {
                    startDir = new File(startDir).getParent();
                }
            } catch (Exception e) {
                // ignore
            }
        } else {
            if (!new File(startDir).isAbsolute()) {
                startDir = Environment.newFile(startDir).getPath();
            }
        }

        Window ancestor = SwingUtilities.getWindowAncestor(parent);
        if (ancestor != null) {
        	parent = ancestor;
        }
        FileDialog fileChooser;
        if (parent instanceof Dialog) {
            fileChooser = new FileDialog((Dialog) parent);
        } else if (parent instanceof Frame) {
            fileChooser = new FileDialog((Frame) parent);
        } else {
            fileChooser = new FileDialog(new Frame());
        }

        fileChooser.setDirectory(startDir);
        fileChooser.setTitle(description);
        if (selectedFile != null) {
            fileChooser.setFile(selectedFile.getName());
        } else if (extension != null && extension.length() > 0) {
            if (System.getProperty("os.name", "").toLowerCase(Locale.ENGLISH).startsWith("windows")) {
            	fileChooser.setFile("*" + extension + (allowZip ? ";*" + ".zip;*" + ".gz" : ""));
            }
        }
        FilenameFilter filter = new FilenameFilter() {
            @Override
            public boolean accept(File dir, String name) {
                return name.toLowerCase(Locale.ENGLISH).endsWith(extension)
                        || allowZip && name.toLowerCase(Locale.ENGLISH).endsWith(extension + ".gz")
                        || allowZip && name.toLowerCase(Locale.ENGLISH).endsWith(extension + ".zip");
            }
        };
        fileChooser.setFilenameFilter(filter);
        fileChooser.setMode(forLoad ? FileDialog.LOAD : FileDialog.SAVE);
        fileChooser.setVisible(true);
        String fn = fileChooser.getFile();
        if (fn != null) {
            File selFile = new File(fileChooser.getDirectory(), fn);
            try {
                File f = selFile;
                String work = new File(".").getCanonicalPath();
                if (f.getCanonicalPath().startsWith(work)) {
                    fn = f.getName();
                    f = f.getParentFile();
                    while (f != null && !f.getCanonicalPath().equals(work)) {
                        fn = f.getName() + File.separator + fn;
                        f = f.getParentFile();
                    }
                } else {
                    fn = f.getCanonicalPath();
                }
                if (addExtension && !(fn.endsWith(extension)
                        || (allowZip && (fn.endsWith(extension + ".zip") || fn.endsWith(extension + ".gz"))))) {
                    fn += extension;
                } else {
                	if (forLoad && !fn.endsWith(extension) && !new File(fn).exists()) {
                        fn += extension;
                	}
                }
                try {
                    storeCurrentDir(extension, selFile.getParent());
                } catch (Exception e) {
                    // ignore
                }
                return fn;
            } catch (IOException e1) {
                try {
                    fn = selFile.getCanonicalPath();
                    if (addExtension && !(fn.endsWith(extension)
                            || (allowZip && (fn.endsWith(extension + ".zip") || fn.endsWith(extension + ".gz"))))) {
                        fn += extension;
                    }
                    return fn;
                } catch (IOException e) {
                    throw new RuntimeException(e.getMessage(), e);
                }
            }
        }
        return null;
    }

    /**
     * File to store current directory of file chooser.
     */
    private final static String cdSettingsName = ".cdsettings";

    /**
     * Stores current directory of file chooser.
     *
     * @param key
     *            the key under which to store current directory
     * @param currentDir
     *            the current directory
     */
    @SuppressWarnings("unchecked")
    private static void storeCurrentDir(String key, String currentDir) {
        try {
            File cdSettings = Environment.newFile(cdSettingsName);
            Map<String, String> cd = new HashMap<String, String>();
            if (cdSettings.exists()) {
                try {
                    ObjectInputStream in = new ObjectInputStream(new FileInputStream(cdSettings));
                    cd = (Map<String, String>) in.readObject();
                    in.close();
                } catch (Exception e) {
                    // ignore
                }
            }
            cdSettings.delete();
            cd.put(key, currentDir);
            ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(cdSettings));
            out.writeObject(cd);
            out.close();
        } catch (Exception e) {
            // ignore
        }
    }

    /**
     * Restores current directory of file chooser.
     *
     * @param key
     *            the key of the current directory to restore
     * @return the current directory, or <code>null</code> if no directory has
     *         been stored under the key
     */
    @SuppressWarnings("unchecked")
    private static String restoreCurrentDir(String key) {
        File cdSettings = Environment.newFile(cdSettingsName);
        if (cdSettings.exists()) {
            try {
                ObjectInputStream in = new ObjectInputStream(new FileInputStream(cdSettings));
                Map<String, String> map = ((Map<String, String>) in.readObject());
                String cd = map.get(key);
                if (cd == null && ".jm".equals(key)) {
                    cd = map.get(".csv");
                }
                in.close();
                return cd;
            } catch (Exception e) {
                // ignore
            }
        }
        return null;
    }

    /**
     * Calls the Jailer export engine via CLI.
     *
     * @param ownerOfConsole
     *            owner component of jailer console
     * @param cliArgs
     *            CLI arguments
     * @param showLogfileButton
     *            console property
     * @param printCommandLine
     *            if true, print CLI command line
     * @param showExplainLogButton
     *            console property
     * @param closeOutputWindow
     *            if <code>true</code>, close console immediately after call
     * @param continueOnErrorQuestion
     *            to ask when call fails
     * @param password
     *            CLI argument to print as "*****"
     * @return <code>true</code> iff call succeeded
     */
    public static boolean runJailer(Window ownerOfConsole, List<String> cliArgs, boolean showLogfileButton,
            final boolean printCommandLine, final boolean closeOutputWindow,
            String continueOnErrorQuestion, String user, String password, final ProgressListener progressListener,
            final ProgressPanel progressPanel, final boolean showExeptions, boolean fullSize,
            boolean returnFalseOnError, ExecutionContext executionContext) {
        return runJailer(ownerOfConsole, cliArgs, showLogfileButton, printCommandLine,
                closeOutputWindow, continueOnErrorQuestion, user, password, progressListener, progressPanel, showExeptions,
                fullSize, false, returnFalseOnError, false, null, executionContext);
    }

	public static boolean canRunJailer() {
		if (currentConsoleFrame != null) {
			currentConsoleFrame.toFront();
			JOptionPane.showMessageDialog(currentConsoleFrame, "Please complete this process before starting another one.", "Engine already running...",
                    JOptionPane.INFORMATION_MESSAGE);
			return false;
		}
		return true;
	}

	private static JFrame currentConsoleFrame = null;

	public interface ResultConsumer {
    	public void consume(boolean result, Throwable t);
    	public void cleanUp();
    }

    /**
     * Calls the Jailer export engine via CLI.
     *
     * @param ownerOfConsole
     *            owner component of jailer console
     * @param cliArgs
     *            CLI arguments
     * @param showLogfileButton
     *            console property
     * @param printCommandLine
     *            if true, print CLI command line
     * @param showExplainLogButton
     *            console property
     * @param closeOutputWindow
     *            if <code>true</code>, close console immediately after call
     * @param continueOnErrorQuestion
     *            to ask when call fails
     * @param password
     *            CLI argument to print as "*****"
     * @return <code>true</code> iff call succeeded
     */
    public static boolean runJailer(Window ownerOfConsole, List<String> cliArgs, boolean showLogfileButton,
            final boolean printCommandLine, final boolean closeOutputWindow,
            final String continueOnErrorQuestion, String user, String password, final ProgressListener progressListener,
            final ProgressPanel progressPanel, final boolean showExeptions, boolean fullSize,
            final boolean closeOutputWindowOnError, final boolean returnFalseOnError, boolean throwException, final ResultConsumer resultConsumer, ExecutionContext executionContext) {
		if (!UIUtil.canRunJailer()) {
			return false;
		}
		Window dialog;
        if (resultConsumer == null) {
        	dialog = new JDialog(ownerOfConsole);
        } else {
        	dialog = currentConsoleFrame = new JFrame();
        	currentConsoleFrame.addWindowListener(new WindowAdapter() {
				@Override
				public void windowClosed(WindowEvent e) {
					currentConsoleFrame = null;
					checkTermination();
				}
			});
    		try {
    			dialog.setIconImage(UIUtil.readImage("/jailerlight.png").getImage());
    		} catch (Throwable t) {
    			// ignore
    		}
        }
        List<String> args = new ArrayList<String>(cliArgs);
        final StringBuffer arglist = createCLIArgumentString(user, password, args, executionContext);
        final List<String> argslist = new ArrayList<String>();
        boolean esc = true;
        for (String arg : args) {
        	if (esc && arg.startsWith("-")) {
        		if (arg.equals(user) || arg.equals(password)) {
        			argslist.add("-");
        		} else {
        			esc = false;
        		}
        	}
            argslist.add(arg);
        }
        final String[] argsarray = new String[argslist.size()];
        int i = 0;
        for (String arg : argslist) {
        	argsarray[i++] = arg;
        }
        final JailerConsole outputView = new JailerConsole(ownerOfConsole, dialog, showLogfileButton, progressPanel, fullSize);
        final PrintStream originalOut = System.out;
        final boolean[] ready = new boolean[] { true };
        System.setOut(new PrintStream(new OutputStream() {
            private int lineNr = 0;
            StringBuffer buffer = new StringBuffer();

            @Override
			public synchronized void write(byte[] arg0, int arg1, int arg2) throws IOException {
                super.write(arg0, arg1, arg2);
            }

            @Override
			public void write(int b) throws IOException {
                if (b != '@') {
                    originalOut.write(b);
                }
                boolean wasReady;
                synchronized (buffer) {
                    wasReady = ready[0];
                    if (b != '@') {
                        buffer.append((char) b);
                    }
                }
                if ((char) b == '\n') {
                    ++lineNr;
                }
                if ((char) b == '\n' && lineNr % 60 == 0 || (char) b == '@') {
                    if (wasReady) {
                        synchronized (buffer) {
                            ready[0] = false;
                        }
                        UIUtil.invokeLater(new Runnable() {
                            @Override
							public void run() {
                                synchronized (buffer) {
                                    if (buffer.length() > 0) {
                                        outputView.appendText(buffer.toString());
                                        buffer.setLength(0);
                                    }
                                }
                                ready[0] = true;
                            }
                        });
                    }
                }
            }
        }));
        final boolean[] exceptionShown = new boolean[1];
        try {
            try {
                File exportLog = Environment.newFile("export.log");
                File sqlLog = Environment.newFile("sql.log");
                if (exportLog.exists()) {
                    FileOutputStream out = new FileOutputStream(exportLog);
                    out.close();
                }
                if (sqlLog.exists()) {
                    FileOutputStream out = new FileOutputStream(sqlLog);
                    out.close();
                }
            } catch (Exception e) {
//				UIUtil.showException(null, "Error", e, arglist);
            }
            final boolean[] result = new boolean[] { false };
            final Throwable[] exp = new Throwable[1];
            final StringBuffer warnings = new StringBuffer();
            final boolean[] fin = new boolean[] { false };
            outputView.dialog.addWindowListener(new WindowAdapter() {
                boolean cancelled = false;

                @Override
                public void windowClosing(WindowEvent e) {
                    boolean f;
                    synchronized (UIUtil.class) {
                        f = exp[0] == null;
                    }
                    try {
                    	CancellationHandler.checkForCancellation(null);
                    } catch (CancellationException ce) {
                    	cancelled = true;
                    }
                    if (cancelled && f) {
                        cancel();
                        JOptionPane.showMessageDialog(outputView.dialog, "Cancellation in progress...", "Cancellation",
                                JOptionPane.INFORMATION_MESSAGE);
                    } else if (exp[0] == null && !fin[0] && !cancelled) {
                        if (JOptionPane.showConfirmDialog(outputView.dialog, "Cancel operation?", "Cancellation",
                                JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE) == JOptionPane.YES_OPTION) {
                            if (!outputView.hasFinished) {
                                cancel();
        						if (outputView.dialog instanceof JDialog) {
        							((JDialog) outputView.dialog).setTitle("Jailer Console - cancelling...");
        						} else {
        							((JFrame) outputView.dialog).setTitle("Jailer Console - cancelling...");
        						}

                                if (progressListener != null) {
                                    progressListener.newStage("cancelling...", true, true);
                                }
                                outputView.getCancelButton().setEnabled(false);
                                cancelled = true;
                            }
                        }
                    }
                }
				private void cancel() {
					Thread thread = new Thread(new Runnable() {
					    @Override
					    public void run() {
					        CancellationHandler.cancel(null);
					    }
					});
					thread.setDaemon(true);
					thread.start();
				}
            });
            new Thread(new Runnable() {
                @Override
				public void run() {
                    for (int i = 0;; ++i) {
                        try {
                            Thread.sleep(i == 0 ? 500 : 1000);
                        } catch (InterruptedException e) {
                        }
                        synchronized (fin) {
                            if (fin[0]) {
                                break;
                            }
                            System.out.print("@");
                        }
                    }
                }
            }).start();

            new Thread(new Runnable() {
                @Override
				public void run() {
                    try {
                        if (printCommandLine) {
                            _log.info("arguments: " + arglist.toString().trim());
                        }
                        result[0] = Jailer.jailerMain(argsarray, disableWarnings ? new StringBuffer() : warnings,
                                progressListener, false);
                    } catch (Throwable t) {
                        synchronized (UIUtil.class) {
                            exp[0] = t;
                        }
                    } finally {
                        // flush
                        System.out.println("@");
                        synchronized (UIUtil.class) {
                            fin[0] = true;
                        }
                    }
                    UIUtil.invokeLater(new Runnable() {
                        @Override
						public void run() {
                            synchronized (UIUtil.class) {
                            	outputView.dialog.toFront();
                            	currentConsoleFrame = null;
                            	System.setOut(originalOut);
                            	if (resultConsumer != null) { // non-modal
                            		resultConsumer.cleanUp();
                            	}

                            	if (outputView.dialog instanceof JDialog) {
                            		((JDialog) outputView.dialog).setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
                            	} else {
                            		((JFrame) outputView.dialog).setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
                            	}
                                if (progressListener != null) {
                                    progressListener.newStage(outputView.hasCancelled ? "cancelled"
                                            : exp[0] == null ? "finished" : "failed", exp[0] != null, true);
                                }
                                if ((exp[0] instanceof CancellationException) || closeOutputWindowOnError
                                        || (closeOutputWindow && result[0] && exp[0] == null
                                                && warnings.length() == 0)) {
                                    if (!outputView.hasCancelled) {
                                        closeDialog();
                                    }
                                } else {
                                    outputView.finish(result[0] && exp[0] == null);
                                    if (result[0] && warnings.length() > 0) {
                                        JOptionPane.showMessageDialog(outputView.dialog,
                                                warnings.length() > 800 ? warnings.substring(0, 800) + "..."
                                                        : warnings.toString(),
                                                "Warning", JOptionPane.INFORMATION_MESSAGE);
                                        closeDialog();
                                    } else if (showExeptions && exp[0] != null
                                            && !(exp[0] instanceof CancellationException)) {
                                        UIUtil.showException(outputView.dialog, "Error", exp[0], arglist);
                                        exceptionShown[0] = true;
                                    }
                                    if (result[0] && progressPanel != null) {
                                        progressPanel.confirm();
                                    }
                                }
                            }
                        }

						private void closeDialog() {
							outputView.dialog.setVisible(false);
						}
                    });
                }
            }, "jailer-main").start();
            if (resultConsumer != null) { // non-modal
                outputView.dialog.addWindowListener(new WindowAdapter() {
                    public void windowClosed(WindowEvent ev) {
		            	System.setOut(originalOut);
		            	Throwable e = null;
						synchronized (UIUtil.class) {
			                if (exp[0] != null) {
			                    if (returnFalseOnError) {
			                        result[0] = false;
			                    } else {
			                        e = exp[0];
			                    }
			                }
			            }
			            if (e != null && !result[0] && continueOnErrorQuestion != null) {
			                result[0] = JOptionPane.showConfirmDialog(outputView.dialog, continueOnErrorQuestion, "Error",
			                        JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION;
			            }
			            resultConsumer.consume(result[0], e);
                    }
                });
            }
            outputView.dialog.setVisible(true);
            if (resultConsumer == null) { // modal
	            synchronized (UIUtil.class) {
	                if (exp[0] != null) {
	                    if (returnFalseOnError) {
	                        result[0] = false;
	                    } else {
	                        throw exp[0];
	                    }
	                }
	            }
	            if (!result[0] && continueOnErrorQuestion != null) {
	                result[0] = JOptionPane.showConfirmDialog(outputView.dialog, continueOnErrorQuestion, "Error",
	                        JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION;
	            }
	            return result[0];
            } else {
            	return true;
            }
        } catch (Throwable t) {
            if (t instanceof CancellationException) {
                CancellationHandler.reset(null);
            } else {
                boolean shown = false;
                synchronized (UIUtil.class) {
                    shown = exceptionShown[0];
                }
                if (throwException) {
                    throw new RuntimeException(t);
                }
                if (!shown) {
                    UIUtil.showException(null, "Error", t, arglist);
                }
            }
            return false;
        } finally {
            if (resultConsumer == null) { // modal
            	System.setOut(originalOut);
            }
        }
    }

    public static StringBuffer createCLIArgumentString(String user, String password, List<String> args, ExecutionContext executionContext) {
        args.add("-datamodel");
        args.add(executionContext.getQualifiedDatamodelFolder());
        return createPlainCLIArguments(user, password, args, true);
    }

    public static StringBuffer createPlainCLIArguments(String user, String password, List<String> args, boolean escMinus) {
        final StringBuffer arglist = new StringBuffer();
        int pwi = -1;
        for (int i = args.size() - 1; i >= 0; --i) {
            if (args.get(i) != null && password != null && args.get(i) == password && password.length() > 0) {
                pwi = i;
                break;
            }
        }
        if (pwi < 0) {
	        for (int i = args.size() - 1; i >= 0; --i) {
	            if (args.get(i) != null && password != null && args.get(i).equals(password) && password.length() > 0) {
	                pwi = i;
	                break;
	            }
	        }
        }
        char q =  System.getProperty("os.name", "").toLowerCase(Locale.ENGLISH).startsWith("windows") ? '"' : '\'';
        for (int i = 0; i < args.size(); ++i) {
            String arg = args.get(i);
            if (i == pwi) {
                arglist.append((escMinus? " -" : "") + " " + q + "<password>" + q);
            } else {
                if (escMinus && arg.startsWith("-") && user != null && arg.equals(user)) {
                	arglist.append(" -");
                }
                if ("".equals(arg) || arg.contains(" ") || arg.contains("<") || arg.contains(">") || arg.contains("*")
                        || arg.contains("?") || arg.contains("|") || arg.contains("$") || arg.contains("\"")
                        || arg.contains("'") || arg.contains("\\") || arg.contains(";") || arg.contains("&") || arg.contains("~")) {
                    arglist.append(" " + q);
                    for (int j = 0; j < arg.length(); ++j) {
                        char c = arg.charAt(j);
                        if (c == '\"' || c == '\'' || c == '$') {
                            arglist.append("\\");
                        }
                        arglist.append(c);
                    }
                    arglist.append(q);
                } else {
                    arglist.append(" " + arg);
                }
            }
        }
        return arglist;
    }

    public static Object EXCEPTION_CONTEXT_USER_ERROR = new Object();
    public static Object EXCEPTION_CONTEXT_MB_USER_ERROR = new Object();
    public static Object EXCEPTION_CONTEXT_USER_WARNING = new Object();

    /**
     * Shows an exception.
     *
     * @param parent
     *            parent component of option pane
     * @param title
     *            title of option pane
     * @param t
     *            the exception
     */
    public static void showException(Component parent, String title, Throwable t) {
        showException(parent, title, t, null);
    }

    /**
     * Shows an exception.
     *
     * @param parent
     *            parent component of option pane
     * @param title
     *            title of option pane
     * @param context
     *            optional context object. String or Session is supported.
     * @param t
     *            the exception
     */
    public static void showException(Component parent, String title, Throwable t, Object context) {
    	showException(parent, title, t, context, null);
    }

    /**
     * Shows an exception.
     *
     * @param parent
     *            parent component of option pane
     * @param title
     *            title of option pane
     * @param context
     *            optional context object. String or Session is supported.
     * @param t
     *            the exception
     */
    public static void showException(Component parent, String title, Throwable t, Object context, JComponent additionalControl) {
    	Throwable original = t;

    	// TODO exception stack trace stripping ala AWTWatchdog#sendThreadDump

    	if (context == EXCEPTION_CONTEXT_USER_ERROR || context == EXCEPTION_CONTEXT_MB_USER_ERROR) {
        	if (t instanceof IndexOutOfBoundsException
        			|| t instanceof NullPointerException
        			|| t instanceof ClassCastException
        			|| t instanceof IllegalStateException
        			|| t instanceof IllegalArgumentException) {
        		context = null;
        	}
        }
    	if (t instanceof DataModel.NoPrimaryKeyException || t instanceof CycleFinder.CycleFoundException || t instanceof IncompatibleModelException) {
            context = EXCEPTION_CONTEXT_USER_ERROR;
        }
    	if (!(t instanceof CancellationException)) {
            t.printStackTrace();
    	}
        if (!(t instanceof ClassNotFoundException)) {
            while (t.getCause() != null && t != t.getCause() && !(t instanceof SqlException)) {
                t = t.getCause();
            }
        }
    	if (t instanceof DataModel.NoPrimaryKeyException || t instanceof CycleFinder.CycleFoundException || t instanceof IncompatibleModelException) {
            context = EXCEPTION_CONTEXT_USER_ERROR;
        }
    	if (t instanceof RowLimitExceededException) {
            context = EXCEPTION_CONTEXT_USER_WARNING;
        }
    	if (t instanceof SqlException) {
            String message = ((SqlException) t).getMessage();
            String sql = ((SqlException) t).sqlStatement;
            String errorDialogTitle = ((SqlException) t).errorDialogTitle;
			if (message != null) {
				if (sql != null) {
		            String iMsg = message.toString() + "\n" + JailerVersion.VERSION + "\n" + sql;
					sendIssue("internalSQL", iMsg);
				}
			}
			new SqlErrorDialog(parent == null ? null : parent instanceof Window? (Window) parent : SwingUtilities.getWindowAncestor(parent),
					((SqlException) t).isFormatted()? message : lineWrap(message, 120).toString(), sql, ((SqlException) t).isFormatted(), true, errorDialogTitle, false, additionalControl);
            return;
        }
        if (t instanceof CancellationException) {
        	return;
        }
        String message;
        if (context == EXCEPTION_CONTEXT_USER_ERROR || context == EXCEPTION_CONTEXT_MB_USER_ERROR) {
        	message = original.getClass().getSimpleName();
	        if (original.getMessage() != null && !"".equals(original.getMessage().trim())) {
	            message += ": " + original.getMessage();
	        }
	        Throwable cause = original.getCause();
	        while (cause != null && cause != cause.getCause()) {
	        	if (cause.getMessage() != null && !"".equals(cause.getMessage().trim())) {
			        message += "\n \nCaused by: " + cause.getClass().getSimpleName();
		            message += ": " + cause.getMessage();
		        }
	        	cause = cause.getCause();
            }
        } else {
	        message = t.getMessage();
	        if (message == null || "".equals(message.trim())) {
	            message = t.getClass().getName();
	        }
        }

        StringBuilder msg = lineWrap(message, 80);

        String contextDesc = "";
        if (context != null) {
            if (context instanceof Session) {
                Session session = (Session) context;
                contextDesc = session.dbUrl + " (" + session.dbms + ")";
            } else if (context != EXCEPTION_CONTEXT_USER_ERROR && context != EXCEPTION_CONTEXT_MB_USER_ERROR) {
                contextDesc = lineWrap(context.toString(), 80).toString();
            }
        }
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        t.printStackTrace(pw);
        if (context != EXCEPTION_CONTEXT_USER_ERROR) {
        	if (context != EXCEPTION_CONTEXT_MB_USER_ERROR) {
        		contextDesc += "\nHelp Desk: https://sourceforge.net/p/jailer/discussion/";
        		contextDesc += "\nMail: rwisser@users.sourceforge.net\n";
        		contextDesc += "\n" + JailerVersion.APPLICATION_NAME + " " + JailerVersion.VERSION + "\n\n" + sw.toString();
        	}

            String iMsg = (context != null && "AWT".equals(context)? context : "") +
            		msg.toString() + "\n" + JailerVersion.APPLICATION_NAME + " " + JailerVersion.VERSION + "\n\n" + sw.toString();

            boolean silent = "AWT".equals(context);

            iMsg = iMsg
            		.replaceAll("\\bat [^\\n]*/", "at ")
            		.replaceAll("\\bat java.", "atj..")
					.replaceAll("\\bat javax.swing.", "atjs..")
					.replaceAll("\\bat net.sf.jailer.", "atn..")
					.replaceAll("\\s*(\\n)\\s*", "$1");
            sendIssue("internal", (silent? "S" : "") + iMsg);

            if (silent) {
            	return;
            }
        }

        new SqlErrorDialog(parent == null ? null : parent instanceof Window? (Window) parent : SwingUtilities.getWindowAncestor(parent), msg.toString(),
                contextDesc, false, false, context == EXCEPTION_CONTEXT_USER_ERROR || context == EXCEPTION_CONTEXT_MB_USER_ERROR || context == EXCEPTION_CONTEXT_USER_WARNING? title : null, context == EXCEPTION_CONTEXT_USER_WARNING, additionalControl);
    }

    public static StringBuilder lineWrap(String message, int maxwidth) {
        StringBuilder msg = new StringBuilder();
        Pattern wrapRE = Pattern.compile("(\\S\\S{" + maxwidth + ",}|.{1," + maxwidth + "})(\\s+|$)");
        Matcher m = wrapRE.matcher(message == null ? "" : message);
        while (m.find()) {
            String line = m.group();
            while (line.length() > maxwidth + 10) {
                msg.append(line.substring(0, maxwidth) + "\n");
                line = line.substring(maxwidth);
            }
            msg.append(line + (line.contains("\n") ? "" : "\n"));
        }
        return msg;
    }

    /**
     * Initializes peer of newly created window.
     *
     * Should not be neccassary, but there is a strange bug in AWT of jre 6 on
     * multi-core/processor systems. Sleeping a little after creating peer and
     * before making peer visible seems to help.
     */
    public static void initPeer() {
        try {
            Thread.sleep(100);
        } catch (InterruptedException e) {
        }
    }

    public static void fit(Window d) {
        try {
            // Get the size of the screen
            Dimension dim = Toolkit.getDefaultToolkit().getScreenSize();
            int hd = d.getY() + d.getHeight() - (dim.height - 60);
            if (hd > 0) {
                d.setSize(d.getWidth(), Math.max(d.getHeight() - hd, 150));
            }
            d.setLocation(Math.max(0, d.getX()), Math.max(0, d.getY()));
        } catch (Throwable t) {
            // ignore
        }
    }

    public static boolean disableWarnings = false;

    /**
     * Tries to get as much memory as possible and shows it's size.
     */
    public static void showMaxMemory() {
        long memSize = 0;
        try {
            final int MB = 1024 * 1024;
            List<byte[]> mem = new ArrayList<byte[]>();
            while (true) {
                mem.add(new byte[MB]);
                memSize += MB;
            }
        } catch (OutOfMemoryError e) {
            JOptionPane.showConfirmDialog(null, "MaxMem=" + memSize, "", JOptionPane.INFORMATION_MESSAGE);
        }
    }

    public static void replace(JComponent component, JComponent replacement) {
        Container parent = component.getParent();
        GridBagConstraints c = ((GridBagLayout) parent.getLayout()).getConstraints(component);
        parent.remove(component);
        if (replacement != null) {
            parent.add(replacement, c);
        }
    }

    public static void fit(JPopupMenu popup) {
        if (!(popup instanceof JScrollPopupMenu || popup instanceof JScrollC2PopupMenu)) {
            final int MAX_ITEMS = 40;
            Component[] comps = popup.getComponents();
            popup.removeAll();
            JMenu current = null;
            int ci = 1;
            for (int i = 0; i < comps.length; ++i) {
                if (ci > MAX_ITEMS && i < comps.length - 5) {
                    ci = 1;
                    JMenu newMenu = new JMenu("more...");
                    if (current == null) {
                        popup.add(newMenu);
                    } else {
                        current.add(newMenu);
                    }
                    current = newMenu;
                }
                if (current == null) {
                    popup.add(comps[i]);
                } else {
                    current.add(comps[i]);
                }
                ++ci;
            }
        }
    }

    public static void checkTermination() {
        for (Window w : Window.getWindows()) {
            if (w.isShowing()) {
                return;
            }
        }
        System.exit(0);
    }

    private static int issueCount = 0;
    private static long lastIssueTS = 0;

    public static void sendIssue(final String type, final String issue) {
		new Thread(new Runnable() {
			@Override
			public void run() {
		    	try {
					final int MAX_CL = 1300;
					int maxEIssueLength = 4 * MAX_CL;
					String ipf = ++issueCount + "z" + (lastIssueTS != 0? (System.currentTimeMillis() - lastIssueTS) / 1000 + "s." : "");
					lastIssueTS = System.currentTimeMillis();
					String url;
					do {
						String eIssue = URLEncoder.encode(ipf + issue, "UTF-8");
			            if (eIssue.length() > maxEIssueLength) {
			            	eIssue = eIssue.substring(0, maxEIssueLength);
			            }
						url = "http://jailer.sf.net/issueReport.php?type=" + URLEncoder.encode(type, "UTF-8") + "&" + "issue=" + eIssue
							+ "&uuid=" + URLEncoder.encode(String.valueOf(UISettings.restore("uuid")), "UTF-8")
							+ "&ts=" + URLEncoder.encode(new Date().toString(), "UTF-8")
							+ "&jversion=" + URLEncoder.encode(System.getProperty("java.version") + "/" + System.getProperty("java.vm.vendor") + "/" + System.getProperty("java.vm.name") + "/" + System.getProperty("os.name"), "UTF-8") + "/(" + Environment.state + ")";
						maxEIssueLength -= 10;
					} while (url.length() > MAX_CL && maxEIssueLength > 100);
					HttpUtil.get(url);
				} catch (UnsupportedEncodingException e) {
					// ignore
				}
			}
		}).start();
    }

    public static String toHTML(String plainText, int maxLineLength) {
    	return "<html>" + toHTMLFragment(plainText, maxLineLength);
    }

    public static String toHTMLFragment(String plainText, int maxLineLength) {
    	        plainText = plainText.trim();
        if (maxLineLength > 0) {
            StringBuilder sb = new StringBuilder();
            int MAXLINES = 50;
            int lineNr = 0;
            for (String line: plainText.split("\n")) {
                if (line.length() > maxLineLength) {
                    sb.append(line.substring(0, maxLineLength) + "...");
                } else {
                    sb.append(line);
                }
                if (++lineNr > MAXLINES) {
                	sb.append("...");
                	break;
                }
                sb.append("\n");
            }
            plainText = sb.toString();
        }
        return plainText
                .replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;")
                .replace("\n", "<br>")
                .replace(" ", "&nbsp;")
                .replace("\t","&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
    }

    public static ImageIcon scaleIcon(JComponent component, ImageIcon icon) {
    	return scaleIcon(component, icon, 1);
    }

    public static ImageIcon scaleIcon(JComponent component, ImageIcon icon, double scaleFactor) {
        if (icon != null) {
        	int heigth = component.getFontMetrics(new JLabel("M").getFont()).getHeight();
        	double s = heigth * scaleFactor / (double) icon.getIconHeight();
        	return scaleIcon(icon, s);
        }
    	return null;
    }

	public static ImageIcon scaleIcon(ImageIcon icon, double factor) {
		if (icon != null) {
	        return scaleIcon(icon, (int)(icon.getIconWidth() * factor), (int)(icon.getIconHeight() * factor));
		}
		return null;
	}

	public static synchronized ImageIcon scaleIcon(ImageIcon icon, int w, int h) {
		if (icon != null) {
			try {
				Image scaled = icon.getImage().getScaledInstance(w, h, Image.SCALE_SMOOTH);
				if (!baseMultiResolutionImageClassExists || (icon.getIconWidth() <= w && icon.getIconHeight() <= h)) {
					return new ImageIcon(scaled);
				}
				Image[] imageList = new Image[] {
						scaled,
						icon.getImage()
						};
				try {
					if (baseMultiResolutionImageClassConstructor == null) {
						baseMultiResolutionImageClassConstructor = Class.forName("java.awt.image.BaseMultiResolutionImage").getConstructor(imageList.getClass());
					}
					Object baseMultiResolutionImage = baseMultiResolutionImageClassConstructor.newInstance((Object) imageList);
					return new ImageIcon((Image) baseMultiResolutionImage);
				} catch (Throwable t) {
					baseMultiResolutionImageClassExists = false;
					return new ImageIcon(scaled);
				}
			} catch (Exception e) {
				return null;
			}
		}
		return null;
	}

	private static Constructor<?> baseMultiResolutionImageClassConstructor = null;
	private static boolean baseMultiResolutionImageClassExists = true;

    /**
     * Represents "null"-value in rows tables.
     */
	public static final String NULL = "null";

	public static String LINE_SEPARATOR = System.getProperty("line.separator", "\n");

	/**
	 * Copies selected cells of a rows table into the clipboard.
	 *
	 * @param table the table
	 */
	public static void copyToClipboard(JTable table, boolean lineOnSingeCell) {
		String nl = System.getProperty("line.separator", "\n");
		StringBuilder sb = new StringBuilder();
		boolean prepNL = table.getSelectedRows().length > 1;
		int[] selectedColumns = table.getSelectedColumns();
		if (table.getSelectedColumnCount() == 1 && table.getSelectedRowCount() == 1 && lineOnSingeCell) {
			prepNL = true;
			selectedColumns = new int[table.getColumnCount()];
			for (int i = 0; i < selectedColumns.length; ++i) {
				selectedColumns[i] = i;
			}
		}
		RowSorter<? extends TableModel> rowSorter = table.getRowSorter();
		TableColumnModel columnModel = table.getColumnModel();
		boolean firstLine = true;
		for (int row: table.getSelectedRows()) {
			if (!firstLine) {
				sb.append(nl);
			}
			boolean f = true;
			for (int col: selectedColumns) {
				Object value = table.getModel().getValueAt(
						rowSorter == null? row : rowSorter.convertRowIndexToModel(row),
						columnModel.getColumn(col).getModelIndex());
				if (value instanceof Row) {
					Object[] values = ((Row) value).values;
					for (Object v: values) {
						if (v instanceof TableModelItem) {
							v = ((TableModelItem) v).value;
						}
						if (!f) {
							sb.append("\t");
						}
						f = false;
						sb.append(v == NULL || v == null? "" : v);
					}
				} else {
					if (value instanceof TableModelItem) {
						value = ((TableModelItem) value).value;
					}
					if (!f) {
						sb.append("\t");
					}
					f = false;
					sb.append(value == NULL || value == null? "" : value);
				}
			}
			firstLine = false;
		}
		if (prepNL) {
			sb.append(nl);
		}
		StringSelection selection = new StringSelection(sb.toString());
	    Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
	    clipboard.setContents(selection, selection);
	}

	/**
	 * Pair of Icon and Text.
	 */
	public static class IconWithText implements Comparable<IconWithText> {
		public final String text;
		public final ImageIcon icon;

		public IconWithText(String text, ImageIcon icon) {
			this.text = text;
			this.icon = icon;
		}

		@Override
		public String toString() {
			return text;
		}

		@Override
		public int compareTo(IconWithText o) {
			return text.compareTo(o.text);
		}
	}

	private static boolean isPopupActive = false;

	public static synchronized boolean isPopupActive() {
		return isPopupActive;
	}

	private static synchronized void setPopupActive(boolean b) {
		isPopupActive = b;
	}

    public static void showPopup(final Component invoker, final int x, final int y, final JPopupMenu popup) {
    	popup.addPropertyChangeListener("visible", new PropertyChangeListener() {
    		@Override
			public void propertyChange(PropertyChangeEvent evt) {
    			if (Boolean.FALSE.equals(evt.getNewValue())) {
    				setPopupActive(false);
    			}
    		}
		});
    	UIUtil.invokeLater(new Runnable() {
			@Override
			public void run() {
				if (invoker.isShowing()) {
					setPopupActive(true);
					popup.show(invoker, x, y);
				}
			}
		});
	}

    public static void invokeLater(final Runnable runnable) {
    	SwingUtilities.invokeLater(runnable);
    }

    public static void invokeLater(final int ticks, final Runnable runnable) {
		SwingUtilities.invokeLater(new Runnable() {
			int count = ticks;
			@Override
			public void run() {
				--count;
				if (count <= 0) {
					runnable.run();
				} else {
					SwingUtilities.invokeLater(this);
				}
			}
		});
	}

    private static Map<Component, Integer> waitLevel = new WeakHashMap<Component, Integer>();

	public static void setWaitCursor(Component component) {
		if (component != null) {
			Integer level = waitLevel.get(component);
			if (level != null) {
				waitLevel.put(component, level + 1);
			} else {
				waitLevel.put(component, 1);
				component.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
			}
		}
	}

	public static void resetWaitCursor(Component component) {
		if (component != null) {
			Integer level = waitLevel.get(component);
			if (level != null) {
				if (level == 1) {
					component.setCursor(null);
					waitLevel.remove(component);
				} else {
					waitLevel.put(component, level - 1);
				}
			}
		}
	}

	private static boolean isInitialized = false;

	/**
	 * Triggers expensive UI initializations.
	 */
	@SuppressWarnings("serial")
	public static void prepareUI() {
		if (!isInitialized) {
			isInitialized = true;
			try {
				new RSyntaxTextAreaWithSQLSyntaxStyle(false, false);
				new DetailsView() {
					@Override
					protected void onSelectRow(Row row) {
					}
					@Override
					protected void onRowChanged(int row) {
					}
					@Override
					protected void onClose() {
					}
				};
				new RSyntaxTextArea();
			} catch (Throwable t) {
				// ignore
			}
			try {
				new CCJSqlParser("Select 1");
			} catch (Throwable t) {
				UIUtil.showException(null, "Error", t);
			}
		}
	}

	/**
	 * Calls the {@link PrimaryKeyValidator}.
	 * @param i
	 */
	@SuppressWarnings("serial")
	public static void validatePrimaryKeys(final Window windowAncestor, final BasicDataSource basicDataSource, final Set<Table> tables) {
		final Object cancellationContext = new Object();

		final String infoPrefix = "<html>"
		+ "Checking the primary key definitions in the data model <br>for uniqueness...<br><br><br>".replace(" ", "&nbsp;");
		final ConcurrentTaskControl concurrentTaskControl = new ConcurrentTaskControl(null,
				infoPrefix + "<br><br><br>") {
				@Override
				protected void onError(Throwable error) {
					UIUtil.showException(windowAncestor, "Error", error);
					closeWindow();
				}
				@Override
				protected void onCancellation() {
					master.cancelButton.setText("Stopping...");
					master.cancelButton.setEnabled(false);
					CancellationHandler.cancel(cancellationContext);
				}
			};
		ConcurrentTaskControl.openInModalDialog(windowAncestor, concurrentTaskControl,
			new ConcurrentTaskControl.Task() {
				@Override
				public void run() throws Throwable {
					Session session = new Session(basicDataSource, basicDataSource.dbms, null);
					JobManager jobManager = new JobManager(tables.size() > 1? 4 : 1) {
						@Override
						protected void onException(Throwable t) {
							if (tables.size() > 1) {
								session.killRunningStatements();
							}
						}
					};
					try {
						new PrimaryKeyValidator(cancellationContext) {
							boolean initialized = false;
							@Override
							protected void updateProgressBar() {
								invokeLater(new Runnable() {
									@Override
									public void run() {
										String info;
										int total = numTotal.get();
										if (total == 0) {
											total = 1;
										}
										info = (numDone.get() * 100 / total) + "%";
										int errors = numErrors.get();
										if (errors > 0) {
											info += "&nbsp;<font color=\"#ff2222\">" + errors + "&nbsp;Error" + (errors == 1? "" : "s") + "</font>";
										}
										if (!initialized) {
											concurrentTaskControl.master.cancelButton.setText("Stop");
											initialized = true;
										}
										concurrentTaskControl.master.infoLabel.setText(infoPrefix + "<font size=\"+1\">" + info + "</font></html>");
									}
								});
							}
						}.validatePrimaryKey(session, tables, jobManager);
					} catch (final Throwable t) {
						invokeLater(new Runnable() {
							@Override
							public void run() {
								if (concurrentTaskControl.master.isShowing()) {
									UIUtil.showException(windowAncestor, "Error", t);
									concurrentTaskControl.master.closeWindow();
								}
							}
						});
						return;
					} finally {
						session.shutDown();
						jobManager.shutdown();
					}
					invokeLater(new Runnable() {
						@Override
						public void run() {
							if (concurrentTaskControl.master.isShowing()) {
								JOptionPane.showMessageDialog(windowAncestor, tables.size() == 1? "The primary key definition is valid." : "All primary key definitions are valid.");
								concurrentTaskControl.closeWindow();
							}
						}
					});
				}
		}, "Checking Primary Keys...");
		invokeLater(4, new Runnable() {
			@Override
			public void run() {
				CancellationHandler.reset(cancellationContext);
			}
		});
	}

	/**
	 * Initializes the "Native L&F" menu items.
	 *
	 * @param nativeLAFCheckBoxMenuItem the menu item
	 */
	public static void initPLAFMenuItem(final JCheckBoxMenuItem nativeLAFCheckBoxMenuItem, final Component parentComponent) {
		nativeLAFCheckBoxMenuItem.setSelected(Boolean.TRUE.equals(UISettings.restore(UISettings.USE_NATIVE_PLAF)));
		nativeLAFCheckBoxMenuItem.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				boolean nativeLF = nativeLAFCheckBoxMenuItem.isSelected();
				UISettings.store(UISettings.USE_NATIVE_PLAF, nativeLF);
				JOptionPane.showMessageDialog(parentComponent, "The look and feel has been changed.\n(Will be effective after restart)", "Look&Feel", JOptionPane.INFORMATION_MESSAGE);
			}
		});
	}

	public static String format(long number) {
		return NumberFormat.getInstance().format(number);
	}

	public static String correctFileSeparator(String fileName) {
		try {
			if (fileName == null || new File(fileName).exists()) {
				return fileName;
			}
			if (File.separatorChar == '/') {
				return fileName.replace('\\', File.separatorChar);
			} else {
				return fileName;
			}
		} catch (Exception e) {
			return fileName;
		}
	}

	public static String toDateAsString(Long time) {
		if (time == null) {
			return "";
		}
		return DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.SHORT).format(new Date(time));
	}

	public static Object toDateAsString(Date date) {
		if (date != null) {
			return toDateAsString(date.getTime());
		} else {
			return null;
		}
	}

	private static Font defaultFont = null;

	public static Font defaultTitleFont() {
		if (defaultFont == null) {
			defaultFont = new JLabel().getFont();
			defaultFont = defaultFont.deriveFont(defaultFont.getStyle() | Font.BOLD);
		}
		return defaultFont;
	}

	public static String getDDLTableInConflict(final DDLCreator ddlCreator, Window window, final BasicDataSource dataSource, final DBMS dbms) throws Exception {
		return ConcurrentTaskControl.call(window, new Callable<String>() {
			@Override
			public String call() throws Exception {
				return ddlCreator.getTableInConflict(dataSource, dbms);
			}
		}, "validate working tables...");
	}

	public static boolean isDDLUptodate(final DDLCreator ddlCreator, Window window, final BasicDataSource dataSource,
			final DBMS dbms, final boolean useRowId, final boolean useRowIdsOnlyForTablesWithoutPK, final String workingTableSchema) throws Exception {
		return ConcurrentTaskControl.call(window, new Callable<Boolean>() {
			@Override
			public Boolean call() throws Exception {
				return ddlCreator.isUptodate(dataSource, dbms, useRowId, useRowIdsOnlyForTablesWithoutPK, workingTableSchema);
			}
		}, "validate working tables...");
	}

	public static BasicDataSource createBasicDataSource(Window window, final String driverClassName, final String dbUrl, final String dbUser, final String dbPassword, final int maxPoolSize, final URL... jdbcDriverURL) throws Exception {
		if (!BasicDataSource.findDBMSNeedsConnection(dbUrl)) {
			return new BasicDataSource(driverClassName, dbUrl, dbUser, dbPassword, maxPoolSize, jdbcDriverURL);
		}
		return ConcurrentTaskControl.call(window, new Callable<BasicDataSource>() {
			@Override
			public BasicDataSource call() throws Exception {
				return new BasicDataSource(driverClassName, dbUrl, dbUser, dbPassword, maxPoolSize, jdbcDriverURL);
			}
		}, "connecting...");
	}

	public static boolean checkFileExistsAndWarn(String file, Component parent) {
		if (file != null && !new File(file).exists()) {
            JOptionPane.showMessageDialog(parent, "File \"" + file + "\" not found.", "File not found",
                    JOptionPane.WARNING_MESSAGE);
			return false;
		}
		return true;
	}

	public static String removesuperfluousSpaces(String text) {
		return text.trim()
				.replaceAll(" *\\) +or +\\( *", ") or (")
				.replaceAll("\\( *", "(")
				.replaceAll(" *\\)", ")")
				.replaceAll(" *;$", ";");
	}

	/**
	 * Removes single line comments.
	 *
	 * @param statement
	 *            the statement
	 *
	 * @return statement the statement without comments and literals
	 */
	private static String removeSingleLineComments(String statement) {
		Pattern pattern = Pattern.compile("('(?:[^']*'))|(/\\*.*?\\*/)|(\\-\\-.*?(?=\n|$))", Pattern.DOTALL);
		Matcher matcher = pattern.matcher(statement);
		boolean result = matcher.find();
		StringBuffer sb = new StringBuffer();
		if (result) {
			do {
				if (matcher.group(3) == null) {
					matcher.appendReplacement(sb, "$0");
					result = matcher.find();
					continue;
				}
				int l = matcher.group(0).length();
				matcher.appendReplacement(sb, "");
				if (matcher.group(1) != null) {
					l -= 2;
					sb.append("'");
				}
				while (l > 0) {
					--l;
					sb.append(' ');
				}
				if (matcher.group(1) != null) {
					sb.append("'");
				}
				result = matcher.find();
			} while (result);
		}
		matcher.appendTail(sb);
		return sb.toString();
	}

	public static String toSingleLineSQL(String text) {
		return UIUtil.removesuperfluousSpaces(
				removeSingleLineComments(text).replaceAll("\\s*\\n\\s*", " "));
	}

	public static void startDemon(Thread thread) {
		thread.setDaemon(true);
		thread.start();
	}

	private static Map<String, ImageIcon> images = new HashMap<String, ImageIcon>();
	private static boolean errorSeen = false;

	public static ImageIcon readImage(String resource) {
		ImageIcon result = images.get(resource);
		if (result == null) {
			String name = "/net/sf/jailer/ui/resource" + resource;
			try {
				result = new ImageIcon(ImageIO.read(UIUtil.class.getResource(name)));
			} catch (Throwable t) {
				if (!errorSeen) {
					errorSeen = true;
					invokeLater(8, () -> showException(null, "Error", new IOException("unable to load image " + name + ": " + t.getMessage(), t), EXCEPTION_CONTEXT_MB_USER_ERROR));
				}
				result = null;
			}
			images.put(resource, result);
		}
		return result;
	}

}
