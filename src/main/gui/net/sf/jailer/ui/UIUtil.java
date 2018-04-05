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

import java.awt.Component;
import java.awt.Container;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.FileDialog;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
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

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.Jailer;
import net.sf.jailer.JailerVersion;
import net.sf.jailer.database.Session;
import net.sf.jailer.database.SqlException;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.progress.ProgressListener;
import net.sf.jailer.ui.databrowser.BrowserContentPane.TableModelItem;
import net.sf.jailer.ui.databrowser.Row;
import net.sf.jailer.ui.scrollmenu.JScrollC2PopupMenu;
import net.sf.jailer.ui.scrollmenu.JScrollPopupMenu;
import net.sf.jailer.util.CancellationException;
import net.sf.jailer.util.CancellationHandler;
import net.sf.jailer.util.CycleFinder;

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
        final String extensionAlias;
        if (".jm".equalsIgnoreCase(extension)) {
            extensionAlias = ".csv";
        } else {
            extensionAlias = null;
        }
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

        parent = SwingUtilities.getWindowAncestor(parent);
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
            if (System.getProperty("os.name", "").toLowerCase().startsWith("windows")) {
                // workaround: http://bugs.java.com/view_bug.do?bug_id=8021943
                boolean set = true;
                Matcher m = Pattern.compile("1.7.0_(\\d+).*").matcher(System.getProperty("java.version", ""));
                if (m.matches()) {
                    try {
                        set = Integer.parseInt(m.group(1)) > 40;
                    } catch (NumberFormatException e) {
                        // ignore;
                    }
                }
                if (set) {
                    fileChooser.setFile("*" + extension + (extensionAlias != null? ";*" + extensionAlias : "") + (allowZip ? ";*" + ".zip;*" + ".gz" : ""));
                }
            }
        }
        FilenameFilter filter = new FilenameFilter() {
            @Override
            public boolean accept(File dir, String name) {
                return name.toLowerCase().endsWith(extension)
                        || (extensionAlias != null && name.toLowerCase().endsWith(extensionAlias))
                        || allowZip && name.toLowerCase().endsWith(extension + ".gz")
                        || allowZip && name.toLowerCase().endsWith(extension + ".zip");
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
                        || (extensionAlias != null && fn.endsWith(extensionAlias))
                        || (allowZip && (fn.endsWith(extension + ".zip") || fn.endsWith(extension + ".gz"))))) {
                    fn += extension;
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
                            || (extensionAlias != null && fn.endsWith(extensionAlias))
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
            final boolean printCommandLine, boolean showExplainLogButton, final boolean closeOutputWindow,
            String continueOnErrorQuestion, String user, String password, final ProgressListener progressListener,
            final ProgressPanel progressPanel, final boolean showExeptions, boolean fullSize,
            boolean returnFalseOnError, ExecutionContext executionContext) {
        return runJailer(ownerOfConsole, cliArgs, showLogfileButton, printCommandLine, showExplainLogButton,
                closeOutputWindow, continueOnErrorQuestion, user, password, progressListener, progressPanel, showExeptions,
                fullSize, false, returnFalseOnError, false, executionContext);
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
            final boolean printCommandLine, boolean showExplainLogButton, final boolean closeOutputWindow,
            String continueOnErrorQuestion, String user, String password, final ProgressListener progressListener,
            final ProgressPanel progressPanel, final boolean showExeptions, boolean fullSize,
            final boolean closeOutputWindowOnError, boolean returnFalseOnError, boolean throwException, ExecutionContext executionContext) {
        JDialog dialog = new JDialog(ownerOfConsole);
        List<String> args = new ArrayList<String>(cliArgs);
        final StringBuffer arglist = createCLIArgumentString(user, password, args, executionContext);
        final String[] argsarray = new String[args.size()];
        int i = 0;
        for (String arg : args) {
            argsarray[i++] = arg.trim();
        }
        final JailerConsole outputView = new JailerConsole(ownerOfConsole, dialog, showLogfileButton,
                showExplainLogButton, progressPanel, fullSize);
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
                        SwingUtilities.invokeLater(new Runnable() {
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
                    if (cancelled && f) {
                        JOptionPane.showMessageDialog(outputView.dialog, "Cancellation in progress...", "Cancellation",
                                JOptionPane.INFORMATION_MESSAGE);
                    }
                    if (exp[0] == null && !fin[0] && !cancelled) {
                        if (JOptionPane.showConfirmDialog(outputView.dialog, "Cancel operation?", "Cancellation",
                                JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE) == JOptionPane.YES_OPTION) {
                            if (!outputView.hasFinished) {
                                new Thread(new Runnable() {
                                    @Override
                                    public void run() {
                                        CancellationHandler.cancel(null);
                                    }
                                }).start();
                                outputView.dialog.setTitle("Jailer Console - cancelling...");
                                if (progressListener != null) {
                                    progressListener.newStage("cancelling", true, true);
                                }
                                cancelled = true;
                            }
                        }
                    }
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
                                progressListener);
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
                    SwingUtilities.invokeLater(new Runnable() {
                        @Override
						public void run() {
                            synchronized (UIUtil.class) {
                                outputView.dialog.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
                                if (progressListener != null) {
                                    progressListener.newStage(outputView.hasCancelled ? "cancelled"
                                            : exp[0] == null ? "finished" : "failed", exp[0] != null, true);
                                }
                                if ((exp[0] instanceof CancellationException) || closeOutputWindowOnError
                                        || (closeOutputWindow && result[0] && exp[0] == null
                                                && warnings.length() == 0)) {
                                    if (!outputView.hasCancelled) {
                                        outputView.dialog.setVisible(false);
                                    }
                                } else {
                                    outputView.finish(result[0] && exp[0] == null);
                                    if (result[0] && warnings.length() > 0) {
                                        JOptionPane.showMessageDialog(outputView.dialog,
                                                warnings.length() > 800 ? warnings.substring(0, 800) + "..."
                                                        : warnings.toString(),
                                                "Warning", JOptionPane.INFORMATION_MESSAGE);
                                        outputView.dialog.setVisible(false);
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
                    });
                }
            }, "jailer-main").start();
            outputView.dialog.setVisible(true);
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
            System.setOut(originalOut);
        }
    }

    public static StringBuffer createCLIArgumentString(String user, String password, List<String> args, ExecutionContext executionContext) {
        args.add("-datamodel");
        args.add(executionContext.getQualifiedDatamodelFolder());
        return createPlainCLIArguments(user, password, args);
    }

    public static StringBuffer createPlainCLIArguments(String user, String password, List<String> args) {
        final StringBuffer arglist = new StringBuffer();
        int pwi = -1;
        for (int i = args.size() - 1; i >= 0; --i) {
            if (args.get(i) != null && args.get(i).equals(password) && password.length() > 0) {
                pwi = i;
                break;
            }
        }
        for (int i = 0; i < args.size(); ++i) {
            String arg = args.get(i);
            if (i == pwi) {
                arglist.append(" \"<password>\"");
            } else {
                if ("".equals(arg) || arg.contains(" ") || arg.contains("<") || arg.contains(">") || arg.contains("*")
                        || arg.contains("?") || arg.contains("|") || arg.contains("$") || arg.contains("\"")
                        || arg.contains("'") || arg.contains("\\") || arg.contains(";") || arg.contains("&")) {
                    arglist.append(" \"");
                    for (int j = 0; j < arg.length(); ++j) {
                        char c = arg.charAt(j);
                        if (c == '\"' || c == '$') {
                            arglist.append("\\");
                        }
                        arglist.append(c);
                    }
                    arglist.append("\"");
                } else {
                    arglist.append(" " + arg);
                }
            }
        }
        return arglist;
    }

    public static Object EXCEPTION_CONTEXT_USER_ERROR = new Object();

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
        if (t instanceof DataModel.NoPrimaryKeyException || t instanceof CycleFinder.CycleFoundException) {
            context = EXCEPTION_CONTEXT_USER_ERROR;
        }
        t.printStackTrace();
        if (!(t instanceof ClassNotFoundException)) {
            while (t.getCause() != null && t != t.getCause() && !(t instanceof SqlException)) {
                t = t.getCause();
            }
        }
        if (t instanceof SqlException) {
            String message = ((SqlException) t).message;
            String sql = ((SqlException) t).sqlStatement;
            new SqlErrorDialog(parent == null ? null : SwingUtilities.getWindowAncestor(parent),
                    lineWrap(message, 120).toString(), sql, true, null);
            return;
        }
        String message = t.getMessage();
        if (message == null || "".equals(message.trim())) {
            message = t.getClass().getName();
        }
        StringBuilder msg = lineWrap(message, 80);

        String contextDesc = "";
        if (context != null) {
            if (context instanceof Session) {
                Session session = (Session) context;
                contextDesc = session.dbUrl + " (" + session.dbms + ")";
            } else if (context != EXCEPTION_CONTEXT_USER_ERROR) {
                contextDesc = lineWrap(context.toString(), 80).toString();
            }
        }
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        t.printStackTrace(pw);
        if (context != EXCEPTION_CONTEXT_USER_ERROR) {
            contextDesc += "\nHelp Desk: https://sourceforge.net/forum/?group_id=197260";
            contextDesc += "\nMail: rwisser@users.sourceforge.net\n";
            contextDesc += "\n" + JailerVersion.APPLICATION_NAME + " " + JailerVersion.VERSION + "\n\n" + sw.toString();
            
            sendIssue("internal", msg.toString() + "\n" + contextDesc);
        }

        new SqlErrorDialog(parent == null ? null : SwingUtilities.getWindowAncestor(parent), msg.toString(),
                contextDesc, false, context == EXCEPTION_CONTEXT_USER_ERROR ? title : null);
    }

    private static StringBuilder lineWrap(String message, int maxwidth) {
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
            Thread.sleep(200);
        } catch (InterruptedException e) {
        }
    }

    public static void fit(Window d) {
        try {
            // Get the size of the screen
            Dimension dim = Toolkit.getDefaultToolkit().getScreenSize();
            int hd = d.getY() + d.getHeight() - (dim.height - 80);
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

    public static void wireComponentWithButton(JComponent component, final JButton button) {
        component.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                if (e.getButton() == MouseEvent.BUTTON1 && e.getClickCount() > 1) {
                    if (button.isEnabled()) {
                        button.doClick();
                    }
                }
            }
        });
        component.addKeyListener(new KeyListener() {
            @Override
            public void keyTyped(KeyEvent e) {
                if (e.getKeyChar() == '\n') {
                    if (button.isEnabled()) {
                        button.doClick();
                    }
                }
            }

            @Override
            public void keyReleased(KeyEvent arg0) {
            }

            @Override
            public void keyPressed(KeyEvent arg0) {
            }
        });
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

    private static void sendIssue(final String type, final String issue) {
    }

    public static String toHTML(String plainText, int maxLineLength) {
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
        return "<html>" + 
                plainText
                .replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;")
                .replace("\n", "<br>")
                .replace(" ", "&nbsp;")
                .replace("\t","&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
    }
    
    public static ImageIcon scaleIcon(JComponent component, ImageIcon icon) {
        if (icon != null) {
        	int heigth = component.getFontMetrics(new JLabel("M").getFont()).getHeight();
        	double s = heigth / (double) icon.getIconHeight();
        	try {
        		return new ImageIcon(icon.getImage().getScaledInstance((int)(icon.getIconWidth() * s), (int)(icon.getIconHeight() * s), Image.SCALE_SMOOTH));
        	} catch (Exception e) {
        		return null;
        	}
        }
    	return null;
    }

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

}
