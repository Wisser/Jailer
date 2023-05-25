/*
 * Copyright 2007 - 2023 Ralf Wisser.
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

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.FileDialog;
import java.awt.Font;
import java.awt.Frame;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.ClipboardOwner;
import java.awt.datatransfer.Transferable;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
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
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.net.URL;
import java.net.URLEncoder;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.Stack;
import java.util.WeakHashMap;
import java.util.concurrent.Callable;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.imageio.ImageIO;
import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.ComboBoxEditor;
import javax.swing.ImageIcon;
import javax.swing.InputMap;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.JSeparator;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;
import javax.swing.WindowConstants;
import javax.swing.border.Border;
import javax.swing.plaf.BorderUIResource;
import javax.swing.text.DefaultEditorKit;

import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.formdev.flatlaf.FlatLightLaf;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.Jailer;
import net.sf.jailer.JailerVersion;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.BasicDataSource;
import net.sf.jailer.database.PrimaryKeyValidator;
import net.sf.jailer.database.Session;
import net.sf.jailer.database.SqlException;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ddl.DDLCreator;
import net.sf.jailer.progress.ProgressListener;
import net.sf.jailer.subsetting.RowLimitExceededException;
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
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.CsvFile.Line;
import net.sf.jailer.util.CycleFinder;
import net.sf.jailer.util.JobManager;
import net.sf.jailer.util.LogUtil;
import net.sf.jailer.util.Pair;
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
    private static final Logger _log = LoggerFactory.getLogger(UIUtil.class);

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
                    ObjectInputStream in = new ObjectInputStream(new FileInputStream(cdSettings)); // lgtm [java/input-resource-leak]
                    cd = (Map<String, String>) in.readObject();
                    in.close();
                } catch (Exception e) {
                    // ignore
                }
            }
            cdSettings.delete();
            cd.put(key, currentDir);
            ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(cdSettings)); // lgtm [java/output-resource-leak]
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
                ObjectInputStream in = new ObjectInputStream(new FileInputStream(cdSettings)); // lgtm [java/input-resource-leak]
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
     * @param closeOutputWindow
     *            if <code>true</code>, close console immediately after call
     * @param continueOnErrorQuestion
     *            to ask when call fails
     * @param password
     *            CLI argument to print as "*****"
     * @param openResult 
     * @return <code>true</code> iff call succeeded
     */
    public static boolean runJailer(Window ownerOfConsole, List<String> cliArgs, boolean showLogfileButton,
            final boolean printCommandLine, final boolean closeOutputWindow,
            String continueOnErrorQuestion, String user, String password, final ProgressListener progressListener,
            final ProgressPanel progressPanel, final boolean showExeptions, boolean fullSize,
            boolean returnFalseOnError, Consumer<Window> openResult, ExecutionContext executionContext) {
        return runJailer(ownerOfConsole, cliArgs, showLogfileButton, printCommandLine,
                closeOutputWindow, continueOnErrorQuestion, user, password, progressListener, progressPanel, showExeptions,
                fullSize, false, returnFalseOnError, false, null, openResult, false, executionContext);
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
     * @param closeOutputWindow
     *            if <code>true</code>, close console immediately after call
     * @param continueOnErrorQuestion
     *            to ask when call fails
     * @param password
     *            CLI argument to print as "*****"
     * @param openResult 
     * @return <code>true</code> iff call succeeded
     */
    public static boolean runJailer(Window ownerOfConsole, List<String> cliArgs, boolean showLogfileButton,
            final boolean printCommandLine, final boolean closeOutputWindow,
            final String continueOnErrorQuestion, String user, String password, final ProgressListener progressListener,
            final ProgressPanel progressPanel, final boolean showExeptions, boolean fullSize,
            final boolean closeOutputWindowOnError, final boolean returnFalseOnError, boolean throwException, final ResultConsumer resultConsumer, Consumer<Window> openResult, boolean closeAfterExceptionShown, ExecutionContext executionContext) {
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
    			dialog.setIconImage(UIUtil.readImage("/jailer.png").getImage());
    		} catch (Throwable t) {
    			// ignore
    		}
        }
        if (openResult != null) {
        	JailerConsole.openResultActions.put(dialog, openResult);
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
//    			File exportLog = Environment.newFile("export.log");
                File sqlLog = Environment.newFile("sql.log");
//                if (exportLog.exists()) {
//                    FileOutputStream out = new FileOutputStream(exportLog);
//                    out.close();
//                }
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
                                if ((exp[0] instanceof CancellationException) || (exp[0] != null && closeOutputWindowOnError)
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
                                        if (closeAfterExceptionShown) {
                                            closeDialog();
                                        }
                                    }
                                    if (result[0] && progressPanel != null) {
                                        progressPanel.confirm();
                                    }
                                }
                            }
                        }

						private void closeDialog() {
							outputView.dialog.setVisible(false);
							outputView.dialog.dispose();
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
			            } else if (e != null) {
			            	resultConsumer.consume(result[0], e);
			            }
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
        return createPlainCLIArguments(user, password == null? "" : password, args, true);
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
    	if (context == EXCEPTION_CONTEXT_USER_ERROR || context == EXCEPTION_CONTEXT_MB_USER_ERROR) {
        	if (t instanceof IndexOutOfBoundsException
        			|| t instanceof NullPointerException
        			|| t instanceof ClassCastException
        			|| t instanceof IllegalStateException
        			|| t instanceof IllegalArgumentException) {
        		context = null;
        	}
        }
    	if (t instanceof DataModel.NoPrimaryKeyException || t instanceof CycleFinder.CycleFoundException) {
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
    	if (t instanceof DataModel.NoPrimaryKeyException || t instanceof CycleFinder.CycleFoundException) {
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
            		(msg.toString().startsWith("<html>")? msg.toString().replaceAll("\\</?[^>]+\\>",  "") : msg.toString()) + "\n" + JailerVersion.APPLICATION_NAME + " " + JailerVersion.VERSION + "\n\n" + sw.toString();

            boolean silent = "AWT".equals(context);

            sendIssue("internal", (silent? "S" : "") + iMsg);

            if (silent) {
            	return;
            }
        }

        new SqlErrorDialog(parent == null ? null : parent instanceof Window? (Window) parent : SwingUtilities.getWindowAncestor(parent), msg.toString(),
                contextDesc, false, false, context == EXCEPTION_CONTEXT_USER_ERROR || context == EXCEPTION_CONTEXT_MB_USER_ERROR || context == EXCEPTION_CONTEXT_USER_WARNING? title : null, context == EXCEPTION_CONTEXT_USER_WARNING, additionalControl);
    }

    public static StringBuilder lineWrap(String message, int maxwidth) {
    	if (message.startsWith("<html>")) {
    		return new StringBuilder(message);
    	}
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

    public static void fit(Window d) {
    	fit(d, false);
    }
    
    public static void fit(Window d, boolean full) {
    	try {
            // Get the size of the screen
            Rectangle2D dim = getScreenBounds();
            
            int hd = (int) (d.getY() + d.getHeight() - (dim.getHeight() - 60));
            if (hd > 0) {
                d.setSize(d.getWidth(), Math.max(d.getHeight() - hd, 150));
            }
            if (full) {
	            int wd = (int) (d.getX() + d.getWidth() - (dim.getWidth() - 4));
	            if (wd > 0) {
	                d.setSize(Math.max(d.getWidth() - wd, 150), d.getHeight());
	            }
            }
            d.setLocation((int) Math.min(d.getX(), dim.getX() + dim.getWidth() - 100), (int) Math.min(d.getY(), dim.getY() + dim.getHeight() - 150));
            d.setLocation((int) Math.max(dim.getX(), d.getX()), (int) Math.max(dim.getY(), d.getY()));
        } catch (Throwable t) {
            // ignore
        }
    }

    public static void setInitialWindowLocation(Window window, Window owner, int x, int y) {
		Point p = new Point(x, y);
		if (owner != null) {
			SwingUtilities.convertPointToScreen(p, owner);
		}
		window.setLocation(p);
		fit(window);
	}

	public static Rectangle2D getScreenBounds() {
		Rectangle2D dim = new Rectangle2D.Double();
		try {
		    GraphicsEnvironment localGE = GraphicsEnvironment.getLocalGraphicsEnvironment();
		    for (GraphicsDevice gd : localGE.getScreenDevices()) {
		      for (GraphicsConfiguration graphicsConfiguration : gd.getConfigurations()) {
		        Rectangle2D.union(dim, graphicsConfiguration.getBounds(), dim);
		      }
		    }
		} catch (Throwable t) {
			LogUtil.warn(t);
			dim = new Rectangle2D.Double();
		}
		if (dim.getWidth() == 0.0) {
			Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
			dim = new Rectangle2D.Double(0, 0, screenSize.getWidth(), screenSize.getHeight());
		}
		return dim;
	}

    public static boolean disableWarnings = false;

    /**
     * Tries to get as much memory as possible and shows it's size.
     */
    public static void showMaxMemory() {
        long memSize = 0;
        try {
            final int MB = 1024 * 1024;
            List<byte[]> mem = new ArrayList<byte[]>(); // lgtm [java/unused-container]
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

    public static void sendIssue(final String type,  String theIssue) {
    	String threadName = Thread.currentThread().getName();
		new Thread(new Runnable() {
			@Override
			public void run() {
		    	try {
					final int MAX_CL = 3900;
					int maxEIssueLength = MAX_CL + 10;
					String ipf = ++issueCount + "z" + (lastIssueTS != 0? (System.currentTimeMillis() - lastIssueTS) / 1000 + "s. " : "");
					ipf += threadName;
					lastIssueTS = System.currentTimeMillis();
					String url;
					int i = 0;
					String issue = theIssue;
					do {
						if (i == 1) {
							issue = issue
									.replaceAll("(?is)(\\n) +", "$1")
									.replaceAll("(?is)\\s*(\\n)\\s*", "$1")
									.replaceAll("(?is), birthday, type\\) Select distinct ", "~btsD")
									.replaceAll("(?is), birthday, type\\) Select ", "~btS")
									.replaceAll("(?is)select distinct ", "~d")
									.replaceAll("(?is)select ", "~s")
									.replaceAll("(?is)from ", "~f")
									.replaceAll("(?is)from dual union all ", "~fD")
									.replaceAll("(?is)union all ", "~u")
									.replaceAll("(?is)where ", "~w")
									.replaceAll("(?is)Insert into ", "~i")
									.replaceAll("(?is)JAILER_ENTITY", "~e")
									.replaceAll("(?is)r_entitygraph", "~g")
									.replaceAll("(?is)Duplicate", "~dU")
									.replaceAll("(?is)char", "c~har")
									;
							StringBuilder sb = new StringBuilder();
							String lastLine = null;
							for (String line: issue.split("\\n")) {
								if (lastLine == null) {
									sb.append(line);
								} else {
									int l = Math.min(lastLine.length(), line.length());
									int j = 0;
									for (; j < l; ++j) {
										if (line.charAt(j) != lastLine.charAt(j)) {
											break;
										}
									}
									if (j > 4) {
										sb.append("~" + j + ".");
										sb.append(line.substring(j));
									} else {
										sb.append(line);
									}
								}
								sb.append("\n");
								lastLine = line;
							}
							issue = sb.toString();
						}
						String eIssue = URLEncoder.encode(ipf + issue, "UTF-8");
			            if (eIssue.length() > maxEIssueLength) {
			            	eIssue = eIssue.substring(0, maxEIssueLength);
			            }
						url = "?type=" + URLEncoder.encode(type, "UTF-8") + "&" + "issue=" + eIssue
							+ "&uuid=" + URLEncoder.encode(String.valueOf(UISettings.restore("uuid")), "UTF-8")
							+ "&ts=" + URLEncoder.encode(new Date().toString(), "UTF-8")
							+ "&jversion=" + URLEncoder.encode(System.getProperty("java.version") + "/" + System.getProperty("java.vm.vendor") + "/" + System.getProperty("java.vm.name") + "/" + System.getProperty("os.name"), "UTF-8") + "/(" + Environment.state + ")";
						maxEIssueLength -= 10;
						++i;
					} while (url.length() > MAX_CL && maxEIssueLength > 100);
					for (String res: new String[] {
							"http://jailer.sourceforge.net/issueReport.php",
							"https://jailer.sourceforge.net/issueReport.php",
							"https://jailer.sourceforge.io/issueReport.php"
					}) {
						try {
							String result = HttpUtil.get(res + url);
							if (result != null && result.equals("403")) {
								if (!type.startsWith("!")) {
									sendIssue("!" + type, theIssue.replaceAll("(.{3})", "$1_"));
								}
								return;
							}
							if (result != null && !result.trim().isEmpty()) {
								break;
							}
						} catch (Throwable t) {
							// ignore
						}
					}
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
    	return toHTMLFragment(plainText, maxLineLength, true, true);
    }
    
    public static String toHTMLFragment(String plainText, int maxLineLength, boolean trim) {
    	return toHTMLFragment(plainText, maxLineLength, trim, true);
    }
    
    public static String toHTMLFragment(String plainText, int maxLineLength, boolean trim, boolean withSpaces) {
    	if (trim) {
    		plainText = plainText.trim();
    	}
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
            if (sb.length() > 0 && sb.charAt(sb.length() - 1) == '\n') {
            	sb.setLength(sb.length() - 1);
            }
            plainText = sb.toString();
        }
        plainText = plainText
                .replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;")
                .replace("\n", "<br>");
        if (withSpaces) {
        	plainText = plainText.replace(" ", "&nbsp;")
                .replace("\t","&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
        }
        return plainText;
    }

    public static String fromHTMLFragment(String htmlText) {
    	return htmlText
                .replace("&amp;", "&")
                .replace("&lt;", "<")
                .replace("&gt;", ">")
                .replace("<br>", "\n")
                .replace("&nbsp;", " ");
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

    private static Map<Pair<String, Pair<Integer, Integer>>, ImageIcon> scaledIconPerWH = new HashMap<Pair<String, Pair<Integer, Integer>>, ImageIcon>();
    
	public static ImageIcon scaleIcon(ImageIcon icon, double factor) {
		if (icon != null) {
			return scaleIcon(icon, (int)(icon.getIconWidth() * factor), (int)(icon.getIconHeight() * factor));
		}
		return null;
	}

	public static synchronized ImageIcon scaleIcon(ImageIcon icon, int w, int h) {
		if (icon != null) {
			String name = imageNames.get(icon);
			Pair<String, Pair<Integer, Integer>> key = name != null? new Pair<String, Pair<Integer, Integer>>(name, new Pair<Integer, Integer>(w, h)) : null;
			ImageIcon result = scaledIconPerWH.get(key);
			if (result == null) {
	        	result = doScaleIcon(icon, w, h);
	        	if (key != null && result.getIconWidth() * result.getIconHeight() * 3 < 500_000) {
	        		scaledIconPerWH.put(key, result);
	        	}
			}
			return result;
		}
		return null;
	}
	
	private static synchronized ImageIcon doScaleIcon(ImageIcon icon, int w, int h) {
		try {
			if (w <= 0) {
				w = 1;
			}
			if (h <= 0) {
				h = 1;
			}
			Image scaled = scaledInstance(icon.getImage(), w, h);
			if (!baseMultiResolutionImageClassExists || (icon.getIconWidth() <= w && icon.getIconHeight() <= h)) {
				return new ImageIcon(scaled);
			}

			List<Image> imageListArr = new ArrayList<Image>();
			for (int p = 1125; p <= 9000; p += (p > 4000 ? 500 : (p > 2000 ? 250 : 125))) {
				double pSqrt = Math.sqrt(p / 1000.0);
				int wp = (int) (w * pSqrt + 0.5);
				int hp = (int) (h * pSqrt + 0.5);
				Image scaledP = scaledInstance(icon.getImage(), wp, hp);
				imageListArr.add(scaledP);
			}
			imageListArr.add(0, scaled);
			Image[] imageList = imageListArr.toArray(new Image[0]);
			try {
				if (baseMultiResolutionImageClassConstructor == null) {
					baseMultiResolutionImageClassConstructor = Class.forName("java.awt.image.BaseMultiResolutionImage")
							.getConstructor(imageList.getClass());
				}
				Object baseMultiResolutionImage = baseMultiResolutionImageClassConstructor
						.newInstance((Object) imageList);
				return new ImageIcon((Image) baseMultiResolutionImage);
			} catch (Throwable t) {
				baseMultiResolutionImageClassExists = false;
				return new ImageIcon(scaled);
			}
		} catch (Exception e) {
			e.printStackTrace();
			return icon;
		}
	}

	private static Image scaledInstance(Image image, int w, int h) {
		BufferedImage bufferedImage = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB);
		Graphics2D g = (Graphics2D) bufferedImage.getGraphics();
		g.drawImage(image.getScaledInstance(w, h, Image.SCALE_SMOOTH), 0, 0, null);
		return bufferedImage;
	}

	private static Constructor<?> baseMultiResolutionImageClassConstructor = null;
	private static boolean baseMultiResolutionImageClassExists = true;

    /**
     * Represents "null"-value in rows tables.
     */
	public static final String NULL = "null";
	
	/**
	 * Background color of even table rows.
	 */
	public static final Color TABLE_BACKGROUND_COLOR_1 = new Color(238, 255, 238);

	/**
	 * Background color of odd table rows.
	 */
	public static final Color TABLE_BACKGROUND_COLOR_2 = new Color(255, 255, 255);

	/**
	 * Background color of selected even table rows.
	 */
	public static final Color TABLE_BG1SELECTED = new Color(255, 246, 206);
	
	/**
	 * Background color of selected odd table rows.
	 */
	public static final Color TABLE_BG2SELECTED  = new Color(255, 250, 215);
				
	/**
	 * Background color of even table rows in closure.
	 */
	public static final Color TABLE_BACKGROUND_COLOR_1_INCLOSURE = new Color(196, 236, 255);

	/**
	 * Background color of odd table rows in closure.
	 */
	public static final Color TABLE_BACKGROUND_COLOR_2_INCLOSURE = new Color(208, 245, 255);

	/**
	 * Color of FLAT borders.
	 */
	public static final Color FLAT_BORDER_COLOR =  new Color(220, 220, 220);
	
	public static String LINE_SEPARATOR = System.getProperty("line.separator", "\n");

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

	public static synchronized void setPopupActive(boolean b) {
		isPopupActive = b;
	}

    public static void showPopup(final Component invoker, final int x, final int y, final JPopupMenu popup) {
    	while (popup.getComponentCount() > 0 && popup.getComponent(0) instanceof JSeparator) {
    		popup.remove(0);
    	}
    	while (popup.getComponentCount() > 0 && popup.getComponent(popup.getComponentCount() - 1) instanceof JSeparator) {
    		popup.remove(popup.getComponentCount() - 1);
    	}
    	for (int i = 1; i < popup.getComponentCount();) {
    		if (popup.getComponent(i) instanceof JSeparator && popup.getComponent(i - 1) instanceof JSeparator) {
    			popup.remove(i - 1);
    		} else {
    			++i;
    		}
    	}
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
					@Override
					protected void waitLoading() {
					}
				};
				new RSyntaxTextArea();
				getSQLEditorFont();
				new RowCountRenderer("", null);
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
										info = String.format("%1.1f", numDone.get() * 100d / total) + "%";
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
								Object message = tables.size() == 1? "The primary key definition is valid." : "All primary key definitions are valid.";
								JOptionPane.showMessageDialog(windowAncestor, message);
								concurrentTaskControl.closeWindow();
								if (validatePrimaryKeysPending) {
									validatePrimaryKeysPending = false;
									LogUtil.warn(new RuntimeException("Checking PK (>= 12.3): " +  message));
								}
								
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
	
	public static boolean validatePrimaryKeysPending = false;

	/**
	 * Initializes the "Native L&F" menu items.
	 *
	 * @param plafMenu the menu
	 */
	public static void initPLAFMenuItem(final JMenu plafMenu, final Component parentComponent) {
		ButtonGroup buttonGroup = new ButtonGroup();
		for (PLAF p: PLAF.values()) {
			if (p == PLAF.NIMBUS) {
				continue;
			}
			JRadioButtonMenuItem item = new JRadioButtonMenuItem();
			buttonGroup.add(item);
			if (p == plaf) {
				item.setSelected(true);
			}
			item.setText(p.description);
			item.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					UISettings.store(UISettings.USE_NATIVE_PLAF, p.name());
					JOptionPane.showMessageDialog(parentComponent, "The \"look and feel\" has been changed.\n(Will be effective after restart)", "Look&Feel", JOptionPane.INFORMATION_MESSAGE);
				}
			});
			plafMenu.add(item);
		}
	}

	public static String format(long number) {
		return NumberFormat.getInstance().format(number);
	}

	public static String format(double number) {
		return NumberFormat.getInstance().format(number);
	}

	public static String format(BigDecimal number) {
		NumberFormat instance = new DecimalFormat("");
		int scale = number.scale();
		int maxScale = 40;
		instance.setMinimumFractionDigits(scale);
		instance.setMaximumFractionDigits(scale);
		instance.setMinimumIntegerDigits(1);
		String result = instance.format(number);
		if (scale > maxScale && !result.replaceAll("\\d", "").trim().isEmpty()) {
			result = result.replaceFirst("0{1," + (scale - maxScale) + "}$", "");
		}
		return result;
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
				removeSingleLineComments(text).replaceAll("\\s*\\n\\s*", " ").replaceAll(";\\s*$", ""));
	}

	public static void startDemon(Thread thread) {
		thread.setDaemon(true);
		thread.start();
	}

	private static Map<String, ImageIcon> images = new HashMap<String, ImageIcon>();
	private static Map<ImageIcon, String> imageNames = new HashMap<ImageIcon, String>();
	private static boolean errorSeen = false;

	public static ImageIcon readImage(String resource) {
		return readImage(resource, true);
	}

	public static ImageIcon readImage(String resource, boolean showError) {
		ImageIcon result = images.get(resource);
		if (result == null) {
			String name = "/net/sf/jailer/ui/resource" + resource;
			try {
				URL resource64 = UIUtil.class.getResource(name.replaceFirst("(\\.\\w+)$", "_64$1"));
				if (resource64 != null) {
					result = new ImageIcon(ImageIO.read(resource64));
					result = scaleIcon(result, result.getIconWidth() / 4, result.getIconHeight() / 4);
				} else {
					URL resource32 = UIUtil.class.getResource(name.replaceFirst("(\\.\\w+)$", "_32$1"));
					if (resource32 != null) {
						result = new ImageIcon(ImageIO.read(resource32));
						result = scaleIcon(result, result.getIconWidth() / 2, result.getIconHeight() / 2);
					} else {
						URL resource16 = UIUtil.class.getResource(name);
						result = new ImageIcon(ImageIO.read(resource16));
						result = scaleIcon(result, result.getIconWidth(), result.getIconHeight());
					}
				}
			} catch (Throwable t) {
				if (!errorSeen && showError) {
					errorSeen = true;
					invokeLater(8, () -> showException(null, "Error", new IOException("unable to load image " + name + ": " + t.getMessage(), t)));
				}
				result = null;
			}
			if (result != null) {
				if (result.getIconWidth() * result.getIconHeight() * 3 < 500_000) {
					images.put(resource, result);
					imageNames.put(result, resource);
				}
			}
		}
		return result;
	}

	public static final Color FG_PK = new Color(170, 0, 0);
	public static final Color FG_FK = new Color(0, 0, 220);
	public static final Color BG_FLATMOUSEOVER = new Color(224, 224, 224);
	public static final Color BG_FLATSELECTED = new Color(204, 204, 204);
	
	private static final String[] INVALID_FILENAME_CHARACTERS = new String[] {"\\", "/", ":", ";", "*", "?", "\"", "<", ">", "|"};
	private static final String[][] INVALID_FILENAME_CHARACTERS_TO_REPLACEMENT = new String[INVALID_FILENAME_CHARACTERS.length][];
	static {
    	for (int i = 0; i < INVALID_FILENAME_CHARACTERS.length; ++i) {
    		INVALID_FILENAME_CHARACTERS_TO_REPLACEMENT[i] 
    				= new String[] { 
    						new String(INVALID_FILENAME_CHARACTERS[i]), 
    						String.format("%%%02X", 0xFF & (int) (INVALID_FILENAME_CHARACTERS[i].charAt(0)))
    				};
    	}
    }

	public static String toValidFileName(String text) {
		for (String[] cp: INVALID_FILENAME_CHARACTERS_TO_REPLACEMENT) {
			text = text.replace(cp[0], cp[1]);
		}
		return text.trim();
	}

	public static String fromValidFileName(String text) {
		for (String[] cp: INVALID_FILENAME_CHARACTERS_TO_REPLACEMENT) {
			text = text.replace(cp[1], cp[0]);
		}
		return text.trim();
	}


	public static ImageIcon jailerLogo;
	public static ImageIcon jailerLogo16;
    
    static {
		String name = "/net/sf/jailer/ui/resource" + "/jailer.png";
		try {
			jailerLogo = new ImageIcon(ImageIO.read(UIUtil.class.getResource(name)));
			jailerLogo16 = new ImageIcon(ImageIO.read(UIUtil.class.getResource(name)));
			jailerLogo = UIUtil.scaleIcon(jailerLogo, 1.0 / 3.0);
			jailerLogo16 = UIUtil.scaleIcon(jailerLogo16, 16, 16);
		} catch (Throwable t1) {
			UIUtil.invokeLater(8, () -> UIUtil.showException(null, "Error", new IOException("unable to load image " + name + ": " + t1.getMessage(), t1)));
			jailerLogo = null;
		}
    }

    private static Font sqlEditorFont = null;
	
    public static String suspectQuery;
	public static int subModule;
	
	public static Font getSQLEditorFont() {
		if (sqlEditorFont == null) {
			sqlEditorFont = new RSyntaxTextArea().getFont();
		}
		return sqlEditorFont;
	}

	private static List<Line> lines;

	public static synchronized List<Line> loadDriverList(Window parent) {
		if (lines != null) {
			return lines;
		}
		lines = new ArrayList<Line>();
		
		final String DRIVERLIST_FILE = "driverlist.csv";
		File csvFile = Environment.newWorkingFolderFile(DRIVERLIST_FILE);

		try {
			// check existence of "driverlist.csv"
			FileInputStream is = new FileInputStream(csvFile);
			is.close();
			
			CsvFile drivers = new CsvFile(csvFile);
			lines.addAll(drivers.getLines());
		} catch (FileNotFoundException e) {
			StringBuilder info = new StringBuilder();
			List<String> fileList = new ArrayList<String>();
			try {
				info.append(csvFile.getAbsolutePath() + ": ");
				File[] files = csvFile.getAbsoluteFile().getParentFile().listFiles();
				if (files == null) {
					info.append("null");
				} else {
					for (File file: files) {
						String ord;
						String name = file.getName();
						if (!file.exists()) {
							ord = "1";
						} else {
							ord = "2";
						}
						int state = 0;
						state += file.exists()? 1 : 0;
						state += file.isFile()? 2 : 0;
						state += file.isDirectory()? 4 : 0;
						fileList.add(ord + name + "/" + Integer.toHexString(state));
						if (DRIVERLIST_FILE.equalsIgnoreCase(name)) {
							ord = "0";
							fileList.add(ord + "!" + name + "/" + Integer.toHexString(state));
						}
					}
				}
				Collections.sort(fileList);
				for (int i = 0; i < fileList.size(); ++i) {
					fileList.set(i, fileList.get(i).substring(1));
				}
			} catch (Throwable t) {
				info.append(" err: " + t.getMessage() + ": ");
			}
			if (parent != null) {
				UIUtil.showException(parent, "Error", new FileNotFoundException(e.getMessage() + " / (" + info + fileList + ")"));
				return lines;
			}
			lines = null;
			return new ArrayList<CsvFile.Line>();
		} catch (Exception e) {
			if (parent != null) {
				UIUtil.showException(parent, "Error", e);
				return lines;
			}
			lines = null;
			return new ArrayList<CsvFile.Line>();
		}
		return lines;
	}

	public static String getDBMSLogoURL(String theUrl) {
		String url = theUrl.toLowerCase();
		if (!url.matches("jdbc:.+")) {
			return null;
		}
		Optional<Line> result = loadDriverList(null).stream().filter(line -> {
			String prefix = line.cells.get(1).replaceFirst("[</\\[@].*$", "");
			return !prefix.isEmpty() && url.startsWith(prefix);
		}).findFirst();
		if (result.isPresent()) {
			String logoUrl = "/dbmslogo/" + result.get().cells.get(6);
			String smallLogoUrl = logoUrl.replaceFirst("^(.*)(\\.([^\\.]+))$", "$1_small$2");
			
			if (readImage(smallLogoUrl, false) != null) {
				return smallLogoUrl;
			}
			return logoUrl;
		}
		return null;
	}

	public static String getDBMSURLPattern(String theUrl) {
		String url = theUrl.toLowerCase();
		if (!url.matches("jdbc:.+")) {
			return null;
		}
		Optional<Line> result = loadDriverList(null).stream().filter(line -> {
			String prefix = line.cells.get(1).replaceFirst("[</\\[@].*$", "");
			return !prefix.isEmpty() && url.startsWith(prefix);
		}).findFirst();
		if (result.isPresent()) {
			if (result.get().cells.get(0).equals("<other>")) {
				return null;
			}
			return result.get().cells.get(1);
		}
		return null;
	}

	public static void setOpacity(Window w, float opacity) {
    	try {
    		if (!opacityfailed) {
    			w.setOpacity(opacity);
    			opacityfailed = false;
    		}
		} catch (Exception e) {
			opacityfailed = true;
		}
	}

    public static boolean opacityfailed = false;
	private static Map<Window, Float> hiddenWindows = new LinkedHashMap<>();
	private static Timer dwTimer = null;
	
	public static void startDW() {
		if (dwTimer == null) {
			dwTimer = new Timer(120, e -> {
				stopDW();
			});
			dwTimer.setRepeats(false);
			dwTimer.start();
		}
	}
	
	public static boolean isDWActive() {
		return dwTimer != null;
	}
	
	public static void addDW(Window w) {
		if (dwTimer != null && w != null) {
			hiddenWindows.put(w, w.getOpacity());
			setOpacity(w, 0f);
		}
	}

	public static void addDW(Window w, float o) {
		if (dwTimer != null && w != null) {
			hiddenWindows.put(w, o);
			setOpacity(w, 0f);
		} else if (w != null) {
			setOpacity(w, o);
		}
	}
	
	public static void stopDW() {
		if (dwTimer != null) {
			dwTimer.stop();
		}
		dwTimer = null;
		Stack<Window> wStack = new Stack<>();
		hiddenWindows.keySet().forEach(w -> wStack.push(w));
		while (!wStack.isEmpty()) {
			Window w = wStack.pop();
			setOpacity(w, hiddenWindows.get(w));
		}
		hiddenWindows.clear();
	}
	
	public static enum PLAF {
		FLAT("Flat"), NIMBUS("Nimbus"), NATIVE("Native");
		
		PLAF(String description) {
			this.description = description;
		}
		public final String description;
	};
	
	public static PLAF plaf = PLAF.NATIVE;
	
	public static void checkPLAF(Component parentComponent) {
		Object oldPlafSetting = UISettings.restore("OLD_PLAF");
		if (plaf != PLAF.FLAT && plaf != PLAF.NIMBUS && oldPlafSetting != null && !oldPlafSetting.equals(plaf.name())) {
			if (JOptionPane.showOptionDialog(parentComponent, 
					"The \"look and feel\" has been changed.\nDo you want to keep the change?", 
					"Look&Feel", JOptionPane.DEFAULT_OPTION, JOptionPane.INFORMATION_MESSAGE,
					null, new Object[] { "Keep it", "No, back to the old one (forces restart)" }, null) != 0) {
				UISettings.store(UISettings.USE_NATIVE_PLAF, oldPlafSetting);
				System.exit(0);
			}
		}
		UISettings.store("OLD_PLAF", plaf.name());
	}
	
	public static void initPLAF() {
		Object plafSetting = UISettings.restore(UISettings.USE_NATIVE_PLAF);
		plaf = PLAF.FLAT;
		if (Boolean.FALSE.equals(plafSetting)) {
			plaf = PLAF.FLAT;
			UISettings.store(UISettings.USE_NATIVE_PLAF, plaf.name());
		} else if (Boolean.TRUE.equals(plafSetting)) {
			plaf = PLAF.NATIVE;
			UISettings.store(UISettings.USE_NATIVE_PLAF, plaf.name());
		} else if (plafSetting instanceof PLAF) {
			plaf = (PLAF) plafSetting;
			UISettings.store(UISettings.USE_NATIVE_PLAF, plaf.name());
		} else if (plafSetting instanceof String) {
			try {
				plaf = PLAF.valueOf((String) plafSetting);
			} catch (Exception e) {
				// ignore
			}
		}
		
		if (plaf == PLAF.NIMBUS) {
			plaf = PLAF.FLAT;
		}

		switch (plaf) {
			case NATIVE:
				// nothing to do
				break;
			case FLAT:
				try {
					UIManager.put("ScrollPane.border", BorderFactory.createLineBorder(FLAT_BORDER_COLOR));
                    // UIManager.put("Component.arrowType", "triangle");
                    UIManager.put("SplitPane.dividerSize", 8);
                    UIManager.put("SplitPaneDivider.gripDotSize", 6);
//                    UIManager.put("SplitPane.oneTouchButtonOffset", 32);
                    UIManager.put("SplitPane.oneTouchButtonSize", 5);
                    UIManager.put("TitledBorder.border", "");
                    UIManager.put( "Component.arrowType", "triangle" );
                    UIManager.put( "ScrollBar.showButtons", true );
                    UIManager.put( "ScrollBar.width", 14 );
                    UIManager.put( "InternalFrame.borderMargins", new Insets(2,2,1,1));
                    UIManager.put( "TableHeader.separatorColor", Color.lightGray);
                    UIManager.put( "PasswordField.showRevealButton", true);
                    
                    UIManager.put( "Button.arc", 8 );
                    UIManager.put( "Component.arc", 8 );
                    UIManager.put( "CheckBox.arc", 8 );
                    UIManager.put( "ProgressBar.arc", 8 );
                    
//                    UIManager.put( "TextArea.background", Color.white);
                    
                    FlatLightLaf.setup();
					break;
				} catch (Exception x) {
					UIUtil.showException(null, "Error", x);
					break;
				} catch (/*NoClassDefFound*/ Error e) {
					LogUtil.warn(e);
					// if this happens, then FlatLaf jar is not there. Fall back to Nimbus then.
				}
			case NIMBUS:
				try {
					for (LookAndFeelInfo info : UIManager.getInstalledLookAndFeels()) {
						if ("Nimbus".equals(info.getName())) {
							UIManager.put("nimbusBase", new Color(66, 118, 187)); // orig. color: 51, 98, 140
							UIManager.setLookAndFeel(info.getClassName());
							break;
						}
					}
					initMacKeyStrokes();
	
					((InputMap) UIManager.get("Button.focusInputMap")).put(KeyStroke.getKeyStroke("pressed ENTER"),
							"pressed");
					((InputMap) UIManager.get("Button.focusInputMap")).put(KeyStroke.getKeyStroke("released ENTER"),
							"released");
					Object dSize = UIManager.get("SplitPane.dividerSize");
					if (Integer.valueOf(10).equals(dSize)) {
						UIManager.put("SplitPane.dividerSize", Integer.valueOf(14));
					}
	
					if (UIManager.get("InternalFrame:InternalFrameTitlePane[Enabled].textForeground") instanceof Color) {
						UIManager.put("InternalFrame:InternalFrameTitlePane[Enabled].textForeground", Color.BLUE);
					}
					UIManager.put("TitledBorder.border", new BorderUIResource((Border) UIManager.get("TitledBorder.border")) {
						public Insets getBorderInsets(Component c)       {
							return new Insets(4, 4, 6, 4);
						}
					});
				} catch (Exception x) {
					UIUtil.showException(null, "Error", x);
				}
				break;
		}
	}

	private static boolean isMacOS = System.getProperty("os.name", "").toLowerCase(Locale.ENGLISH).startsWith("mac");
	
	public static boolean isMacOS() {
		return isMacOS;
	}

	public static void initMacKeyStrokes() {
		try {
			if (isMacOS()) {
				addOSXKeyStrokes((InputMap) UIManager.get("EditorPane.focusInputMap"));
				addOSXKeyStrokes((InputMap) UIManager.get("FormattedTextField.focusInputMap"));
				addOSXKeyStrokes((InputMap) UIManager.get("PasswordField.focusInputMap"));
				addOSXKeyStrokes((InputMap) UIManager.get("TextField.focusInputMap"));
				addOSXKeyStrokes((InputMap) UIManager.get("TextPane.focusInputMap"));
				addOSXKeyStrokes((InputMap) UIManager.get("TextArea.focusInputMap"));
				addOSXKeyStrokesList((InputMap) UIManager.get("Table.ancestorInputMap"));
				addOSXKeyStrokesList((InputMap) UIManager.get("Tree.focusInputMap"));
			}
		} catch (Throwable t) {
			// ignore
		}
	}

	private static void addOSXKeyStrokes(InputMap inputMap) {
		inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_C, KeyEvent.META_DOWN_MASK),
				DefaultEditorKit.copyAction);
		inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_X, KeyEvent.META_DOWN_MASK),
				DefaultEditorKit.cutAction);
		inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_V, KeyEvent.META_DOWN_MASK),
				DefaultEditorKit.pasteAction);
		inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_A, KeyEvent.META_DOWN_MASK),
				DefaultEditorKit.selectAllAction);
	}

	private static void addOSXKeyStrokesList(InputMap inputMap) {
		inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_C, KeyEvent.META_DOWN_MASK), "copy");
		inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_A, KeyEvent.META_DOWN_MASK), "selectAll");
	}
	
	public static void setLeadingComponent(JComponent component, JComponent leadingComponent) {
		setLeadingOrTrailingComponent("JTextField.leadingComponent", component, leadingComponent);
	}
	
	public static void setTrailingComponent(JComponent component, JComponent trailingComponent) {
		setLeadingOrTrailingComponent("JTextField.trailingComponent", component, trailingComponent);
	}

	private static void setLeadingOrTrailingComponent(String propertyName, JComponent component, JComponent leadingComponent) {
		if (plaf == PLAF.FLAT && component != null) {
			if (leadingComponent instanceof JButton) {
				((JButton) leadingComponent).setText(null);
			}
			if (component instanceof JComboBox) {
				ComboBoxEditor editor = ((JComboBox) component).getEditor();
				if (editor != null) {
					Component editorComponent = editor.getEditorComponent();
					if (editorComponent instanceof JComponent) {
						((JComponent) editorComponent).putClientProperty(propertyName, leadingComponent);
					}
				}
			} else {
				component.putClientProperty(propertyName, leadingComponent);
			}
		}
	}

	public static void setDialogSize(JDialog dialog, int w, int h) {
		dialog.pack();
		dialog.setSize(Math.max(w, dialog.getWidth()), Math.max(h, dialog.getHeight()));
	}

	public static String getToolTip(Table table, DataModel model) {
		String comment = model.getComment(table, null);
		StringBuilder colComments = new StringBuilder();
		table.getColumns().forEach(c -> {
			String cc = model.getComment(table, c);
			if (cc != null) {
				if (colComments.length() == 0) {
					colComments.append("<hr><table>");
				}
				colComments.append("<tr><td>" + toHTMLFragment(c.name, 200) + "</td><td><i>" + toHTMLFragment(cc, 200) + "</i></td></tr>");
			}
		});
		if (colComments.length() == 0) {
			colComments.append("</table>");
		}
		return "<html>" +UIUtil.toHTMLFragment(table.getName() + " (" + table.primaryKey.toSQL(null, false) + ")", 250, true, true) +
				(comment != null? "<br><hr><i>" + UIUtil.toHTMLFragment(comment, 250, true, true) + "</i>": "") + colComments +
				"</html>";
	}
	
	private static Pattern trailingWSPattern = Pattern.compile("^(\\s*)(.*?)()$");
	private static Pattern leadingTrailingWSPattern = Pattern.compile("^(\\s*)(.*?)(\\s*)$");
	private static Pattern charTypePattern = Pattern.compile("^(char|character|nchar|ncharacter)(\\s*)(\\(?)(.*)$", Pattern.CASE_INSENSITIVE);
	public static char spaceIndicator = '\u23B5';
	public static char tabIndicator = '\u21E5';
	
	public static String indicateLeadingAndTrailingSpaces(String item, boolean valueIsCHAR) {
		item = item.replace('\t', tabIndicator);
		Matcher m = valueIsCHAR ? trailingWSPattern.matcher(item) : leadingTrailingWSPattern.matcher(item);
		if (m.matches()) {
			String leading = m.group(1);
			String trainling = m.group(3);
			if (leading.length() > 0 || trainling.length() > 0) {
				StringBuilder sb = new StringBuilder();
				for (int i = leading.length(); i > 0; --i) {
					sb.append(spaceIndicator);
				}
				sb.append(m.group(2));
				for (int i = trainling.length(); i > 0; --i) {
					sb.append(spaceIndicator);
				}
				item = sb.toString();
			}
		}
		return item;
	}

	public static String indicateLeadingAndTrailingSpaces(String item, Column c) {		
		return indicateLeadingAndTrailingSpaces(item, isCHARType(c));
	}

	public static boolean isCHARType(Column c) {
		return c != null && c.type != null? charTypePattern.matcher(c.type).matches() : false;
	}
	
	private static Pattern rTrimPattern = Pattern.compile("\\s+$");
	
	public static String rtrim(String value ) {
		return rTrimPattern.matcher(value).replaceAll("");
	}

	public static void setClipboardContent(Transferable text) {
		Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
		try {
			clipboard.setContents(text, (ClipboardOwner) text);
		} catch (Exception ex) {
			// https://stackoverflow.com/questions/51797673/in-java-why-do-i-get-java-lang-illegalstateexception-cannot-open-system-clipboa
			LogUtil.warn(ex);
			try {
				Thread.sleep(500);
			} catch(Exception ie) {
				// ignore
			}
			clipboard.setContents(text, (ClipboardOwner) text);
		}
	}
	
	public static <T> void traverse(Component component, T parentValue, Function<Component, T> componentValue, BiFunction<T, T, T> composedValue, BiConsumer<T, Component> consumer) {
		T value = composedValue.apply(parentValue, componentValue.apply(component));
		consumer.accept(value, component);
		if (component instanceof Container) {
			for (Component child: ((Container) component).getComponents()) {
				traverse(child, value, componentValue, composedValue, consumer);
			}
		}
	}
	
	public static void createComponentNameTooltips(Component component) {
		Map<Object, String> names = new HashMap<>();
		Class<?> clazz = component.getClass();
		while (clazz != null) {
			for (Field field: clazz.getDeclaredFields()) {
				field.setAccessible(true);
				Object value;
				try {
					value = field.get(component);
				} catch (IllegalArgumentException | IllegalAccessException e) {
					throw new RuntimeException(e);
				}
				if (value != null) {
					names.put(value, field.getName());
				}
			}
			clazz = clazz.getSuperclass();
		}
		traverse(component, new ArrayList<String>(),
			c -> {
				List<String> result = names.containsKey(c)? Collections.singletonList(names.get(c)) : Collections.emptyList();
				return result;
			},
			(pv, v) -> {
				List<String> result = new ArrayList<String>(pv);
				result.addAll(v);
				return result;
			},
			(value, c) -> {
				if (c instanceof JComponent) {
					((JComponent) c).setToolTipText(value.toString() + ": " + c.getClass().getSimpleName());
					try {
						((JComponent) c).setBorder(BorderFactory.createLineBorder(Color.gray));
					} catch (Exception e) {
						// ignore
					}
				}
			});
	}

}
