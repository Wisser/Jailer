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
package net.sf.jailer.ui;

import java.awt.Component;
import java.awt.Frame;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.lang.reflect.InvocationTargetException;
import java.util.List;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;

import net.sf.jailer.Jailer;

import org.apache.log4j.Logger;

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
     * @param selectedFile if not <code>null</code> this file will be selected initially
     * @param startDir directory to start with
     * @param description description of file to chose
     */
    public static String choseFile(File selectedFile, String startDir, final String description, final String extension, Component parent, boolean addExtension, boolean forLoad) {
        JFileChooser fileChooser = new JFileChooser(startDir);
        javax.swing.filechooser.FileFilter filter = new javax.swing.filechooser.FileFilter() {
            public boolean accept(File pathname) {
                return pathname.isDirectory() || pathname.getName().toLowerCase().endsWith(extension)
                	|| pathname.getName().toLowerCase().endsWith(extension + ".gz")
                	|| pathname.getName().toLowerCase().endsWith(extension + ".zip");
            }
            public String getDescription() {
            	if (extension.endsWith(".sql") || extension.endsWith(".xml")) {
            		return "*" + extension + " *" + extension + ".zip";
            	}
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
     * Calls the Jailer export engine via CLI.
     * 
     * @param ownerOfConsole owner component of jailer console
     * @param args CLI arguments
     * @param showLogfileButton console property
     * @param printCommandLine if true, print CLI command line
     * @param showExplainLogButton console property
     * @param closeOutputWindow if <code>true</code>, close console immediately after call
     * @param continueOnErrorQuestion to ask when call fails
     * @param password CLI argument to print as "*****"
     * @return <code>true</code> iff call succeeded
     */
    public static boolean runJailer(Frame ownerOfConsole, List<String> args, boolean showLogfileButton, final boolean printCommandLine, boolean showExplainLogButton, final boolean closeOutputWindow, String continueOnErrorQuestion, String password) {
        final StringBuffer arglist = new StringBuffer();
        final String[] argsarray = new String[args.size()];
        int i = 0;
        for (String arg: args) {
        	if (arg != null && arg.equals(password)) {
        		arglist.append(" *****");
        	} else {
        		if ("".equals(arg) || arg.contains(" ") || arg.contains("<") || arg.contains(">") || arg.contains("*") || arg.contains("?") || arg.contains("|") || arg.contains("$") || arg.contains("\"") || arg.contains("'") || arg.contains("\\")) {
        			arglist.append(" \"");
        			for (int j = 0; j < arg.length(); ++j) {
        				char c = arg.charAt(j);
        				if (c == '\"') {
        					arglist.append("\\");
        				}
        				arglist.append(c);
        			}
        			arglist.append("\"");
        		} else {
        			arglist.append(" " + arg);
        		}
        	}
            argsarray[i++] = arg.trim();
        }
        final JailerConsole outputView = new JailerConsole(ownerOfConsole, showLogfileButton, showExplainLogButton);
        final PrintStream originalOut = System.out;
        final boolean[] ready = new boolean[] { true };
        System.setOut(new PrintStream(new OutputStream() {
        	private int lineNr = 0;
        	StringBuffer buffer = new StringBuffer();
        	
        	public synchronized void write(byte[] arg0, int arg1, int arg2)
					throws IOException {
				super.write(arg0, arg1, arg2);
			}
			
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
	                	try {
							SwingUtilities.invokeAndWait(new Runnable() {
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
						} catch (InterruptedException e) {
							e.printStackTrace();
						} catch (InvocationTargetException e) {
							e.printStackTrace();
						}
                	}
                }
            }
        }));
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
            final StringBuffer warnings = new StringBuffer();
            final boolean[] fin = new boolean[] { false };
            
            new Thread(new Runnable() {
				public void run() {
					for (int i = 0; ; ++i) {
						try {
							Thread.sleep(i == 0? 500 : 1000);
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
				public void run() {
		            try {
		                if (printCommandLine) {
		                    _log.info("arguments: " + arglist.toString().trim());
		                }
		            	result[0] = Jailer.jailerMain(argsarray, warnings);
		            } catch (Throwable t) {
		            	exp[0] = t;
		            } finally {
		            	// flush
		            	System.out.println("@");
		            	synchronized (fin) {
		            		fin[0] = true;
		            	}
		            }
		            SwingUtilities.invokeLater(new Runnable() {
		            	public void run() {
		            		outputView.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		                    if (closeOutputWindow && result[0] && exp[0] == null && warnings.length() == 0) {
		                    	outputView.setVisible(false);
		                    } else {
		                    	outputView.finish(result[0] && exp[0] == null);
		                        if (result[0] && warnings.length() > 0) {
		                        	JOptionPane.showMessageDialog(outputView, warnings.length() > 800? warnings.substring(0, 800) + "..." : warnings.toString(), "Warning", JOptionPane.INFORMATION_MESSAGE);
		                        }
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
     * @param parent parent component of option pane
     * @param title title of option pane
     * @param t the exception
     */
	public static void showException(Component parent, String title, Throwable t) {
		t.printStackTrace();
		while (t.getCause() != null && t != t.getCause()) {
			t = t.getCause();
		}
		JOptionPane.showMessageDialog(parent, t.getMessage() + "\n(" + t.getClass().getSimpleName() + ")", title + " - " + t.getClass().getName(), JOptionPane.ERROR_MESSAGE);
	}

	/**
     * Loads table list file and fill a list.
     * 
     * @param list to fill
     * @param fileName name of file
     */
    public static void loadTableList(List<String> list, String fileName) throws IOException {
    	File file = new File(fileName);
    	if (file.exists()) {
    		BufferedReader in = new BufferedReader(new FileReader(file));
    		String line;
    		while ((line = in.readLine()) != null) {
    			line = line.trim();
    			if (line.length() > 0) {
    				list.add(line);
    			}
    		}
    		in.close();
    	}
	}

    /**
     * Initializes peer of newly created window.
     * 
     * Should not be neccassary, but there is a strange bug in
     * AWT of jre 6 on multi-core/processor systems. Sleeping a
     * little after creating peer and before making peer visible
     * seems to help. 
     */
    public static void initPeer() {
    	try {
			Thread.sleep(200);
		} catch (InterruptedException e) {
		}
    }
    
}
