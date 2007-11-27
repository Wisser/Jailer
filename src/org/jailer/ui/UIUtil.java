/*
 * Copyright 2007 the original author or authors.
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
package org.jailer.ui;

import java.awt.Component;
import java.awt.Frame;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.StringWriter;
import java.util.List;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;

import org.jailer.Jailer;

/**
 * Some utility methods.
 * 
 * @author Wisser
 */
public class UIUtil {

    /**
     * File chooser.
     */
    public static String choseFile(File selectedFile, String startDir, final String description, final String extension, Component parent, boolean addExtension, boolean forLoad) {
        JFileChooser fileChooser = new JFileChooser(startDir);
        javax.swing.filechooser.FileFilter filter = new javax.swing.filechooser.FileFilter() {
            public boolean accept(File pathname) {
                return pathname.isDirectory() || pathname.getName().toLowerCase().endsWith(extension);
            }
            public String getDescription() {
                return "*" + extension;
            }
        };
        fileChooser.setFileFilter(filter);
        fileChooser.setDialogTitle(description);
        if (selectedFile != null) {
        	fileChooser.setSelectedFile(selectedFile);
        }
        fileChooser.setDialogType(forLoad? JFileChooser.OPEN_DIALOG : JFileChooser.SAVE_DIALOG);
        int returnVal = forLoad? fileChooser.showOpenDialog(parent) : fileChooser.showSaveDialog(parent);
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            String fn = "";
            try {
                File f = fileChooser.getSelectedFile();
                String work = new File(".").getCanonicalPath();
                fn = f.getName();
                f = f.getParentFile();
                while (f != null && !f.getCanonicalPath().equals(work)) {
                    fn = f.getName() + File.separator + fn;
                    f = f.getParentFile();
                }
                if (addExtension && !fn.endsWith(extension)) {
                	fn += extension;
                }
                return fn;
            } catch (IOException e1) {
                try {
					return fileChooser.getSelectedFile().getCanonicalPath();
				} catch (IOException e) {
					e.printStackTrace();
				}
            }
        }
        return null;
    }

    /**
     * Runs Jailer.
     * 
     * @param parent parent of OutputView.
     * @param args jailer arguments
     */
    public static boolean runJailer(Frame ownerOfConsole, List<String> args, boolean showLogfileButton, boolean printCommandLine, boolean showExplainLogButton, final boolean closeOutputWindow, String continueOnErrorQuestion) {
        StringBuffer arglist = new StringBuffer();
        final String[] argsarray = new String[args.size()];
        int i = 0;
        for (String arg: args) {
            arglist.append(" " + arg);
            argsarray[i++] = arg.trim();
        }
        final JailerConsole outputView = new JailerConsole(ownerOfConsole, showLogfileButton, showExplainLogButton);
        final PrintStream originalOut = System.out;
        System.setOut(new PrintStream(new OutputStream() {
        	StringBuffer buffer = new StringBuffer();
            public void write(int b) throws IOException {
                originalOut.write(b);
                synchronized (buffer) {
                	buffer.append((char) b);
                }
                if ((char) b == '\n') {
                	SwingUtilities.invokeLater(new Runnable() {
		            	public void run() {
		            		synchronized (buffer) {
		                        outputView.appendText(buffer.toString());
		                        buffer.setLength(0);
		            		}
		            	}
                	});
                }
            }
        }));
        if (printCommandLine) {
            System.out.println("$ jailer" + arglist);
        }
        try {
            try {
                File exportLog = new File("export.log");
                File sqlLog = new File("sql.log");
                if (exportLog.exists()) {
                    FileOutputStream out = new FileOutputStream(exportLog);
                    out.close();
                }
                if (sqlLog.exists()) {
                    FileOutputStream out = new FileOutputStream(sqlLog);
                    out.close();
                }
            } catch (Exception e) {
                UIUtil.showException(null, "Error", e);
            }
            final boolean[] result = new boolean[] { false };
            final Throwable[] exp = new Throwable[1];
            new Thread(new Runnable() {
				public void run() {
		            try {
		            	result[0] = Jailer.jailerMain(argsarray);
		            } catch (Throwable t) {
		            	exp[0] = t;
		            }
		            SwingUtilities.invokeLater(new Runnable() {
		            	public void run() {
		            		outputView.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		                    if (closeOutputWindow && result[0] && exp[0] == null) {
		                    	outputView.setVisible(false);
		                    } else {
		                    	outputView.finish(result[0] && exp[0] == null);
		                    }
		            	}
		            });
				}
            }, "jailer-main").start();
            outputView.setVisible(true);
            if (exp[0] != null) {
            	throw exp[0];
            }
            if (!result[0] && continueOnErrorQuestion != null) {
            	result[0] = JOptionPane.showConfirmDialog(outputView, continueOnErrorQuestion, "Error", JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION;
            }
            return result[0];
        } catch (Throwable t) {
            UIUtil.showException(null, "Error", t);
            return false;
        } finally {
            System.setOut(originalOut);
        }
    }

    /**
     * Shows an exception.
     * 
     * @param e the exception.
     */
	public static void showException(Component parent, String title, Throwable t) {
		t.printStackTrace();
		while (t.getCause() != null && t != t.getCause()) {
			t = t.getCause();
		}
		JOptionPane.showMessageDialog(parent, t.getClass().getName() + "\n" + t.getMessage(), title, JOptionPane.ERROR_MESSAGE);
	}

}
