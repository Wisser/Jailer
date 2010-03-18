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
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;

import net.sf.jailer.CommandLineParser;
import net.sf.jailer.Jailer;
import net.sf.jailer.progress.ProgressListener;
import net.sf.jailer.util.CancellationException;
import net.sf.jailer.util.CancellationHandler;

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
    	String newStartDir = restoreCurrentDir(extension);
    	if (newStartDir != null) {
    		startDir = newStartDir;
    	}
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
                try {
                	storeCurrentDir(extension, fileChooser.getSelectedFile().getParent());
                } catch (Exception e) {
                	// ignore
                }
                return fn;
            } catch (IOException e1) {
                try {
					fn = fileChooser.getSelectedFile().getCanonicalPath();
                    if (addExtension && !fn.endsWith(extension)) {
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
    private final static File cdSettings = new File(".cdsettings");
    
    /**
     * Stores current directory of file chooser.
     * 
     * @param key the key under which to store current directory
     * @param currentDir the current directory
     */
    @SuppressWarnings("unchecked")
	private static void storeCurrentDir(String key, String currentDir) {
		try {
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
     * @param key the key of the current directory to restore
     * @return the current directory, or <code>null</code> if no directory has been stored under the key
     */
	@SuppressWarnings("unchecked")
	private static String restoreCurrentDir(String key) {
		if (cdSettings.exists()) {
			try {
				ObjectInputStream in = new ObjectInputStream(new FileInputStream(cdSettings));
				String cd = ((Map<String, String>) in.readObject()).get(key);
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
     * @param ownerOfConsole owner component of jailer console
     * @param cliArgs CLI arguments
     * @param showLogfileButton console property
     * @param printCommandLine if true, print CLI command line
     * @param showExplainLogButton console property
     * @param closeOutputWindow if <code>true</code>, close console immediately after call
     * @param continueOnErrorQuestion to ask when call fails
     * @param password CLI argument to print as "*****"
     * @return <code>true</code> iff call succeeded
     */
    public static boolean runJailer(Frame ownerOfConsole, List<String> cliArgs, 
    		boolean showLogfileButton, final boolean printCommandLine, boolean showExplainLogButton, 
    		final boolean closeOutputWindow, String continueOnErrorQuestion, String password,
    		final ProgressListener progressListener, final ProgressPanel progressPanel, final boolean showExeptions, boolean fullSize) {
    	JDialog dialog = new JDialog(ownerOfConsole);
        List<String> args = new ArrayList<String>(cliArgs);
        args.add("-datamodel");
        args.add(CommandLineParser.getInstance().datamodelFolder);
        args.add("-script-enhancer");
        args.add(CommandLineParser.getInstance().enhancerFolder);
    	final StringBuffer arglist = new StringBuffer();
        final String[] argsarray = new String[args.size()];
        int i = 0;
        for (String arg: args) {
        	if (arg != null && arg.equals(password)) {
        		arglist.append(" \"<password>\"");
        	} else {
        		if ("".equals(arg) || arg.contains(" ") || arg.contains("<") || arg.contains(">") || arg.contains("*") || arg.contains("?") || arg.contains("|") || arg.contains("$") || arg.contains("\"") || arg.contains("'") || arg.contains("\\")) {
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
            argsarray[i++] = arg.trim();
        }
        final JailerConsole outputView = new JailerConsole(ownerOfConsole, dialog, showLogfileButton, showExplainLogButton, progressPanel, fullSize);
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
        final boolean[] exceptionShown = new boolean[1];
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
            outputView.dialog.addWindowListener(new WindowAdapter() {
            	boolean cancelled = false;

            	@Override
    			public void windowClosing(WindowEvent e) {
            		boolean f;
            		synchronized (UIUtil.class) {
	            		f = exp[0] == null;
	            	}
            		if (cancelled && f) {
            			JOptionPane.showMessageDialog(outputView.dialog, "Cancellation in progress...", "Cancellation", JOptionPane.INFORMATION_MESSAGE);
            		}
    				if (exp[0] == null && !fin[0] && !cancelled) {
	    				if (JOptionPane.showConfirmDialog(outputView.dialog, "Cancel operation?", "Cancellation", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE) == JOptionPane.YES_OPTION) {
	    					new Thread(new Runnable() {
	    						@Override
	    						public void run() {
    								CancellationHandler.cancel();
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
            });
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
		            	result[0] = Jailer.jailerMain(argsarray, warnings, progressListener);
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
		            	public void run() {
		            		synchronized (UIUtil.class) {
			            		outputView.dialog.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		    					if (progressListener != null) {
		    						progressListener.newStage(exp[0] == null? "finished" : "failed", exp[0] != null, true);
		    					}
		    					if (closeOutputWindow && result[0] && exp[0] == null && warnings.length() == 0) {
		                    		outputView.dialog.setVisible(false);
			                    } else {
			                    	outputView.finish(result[0] && exp[0] == null);
			                        if (result[0] && warnings.length() > 0) {
			                        	JOptionPane.showMessageDialog(outputView.dialog, warnings.length() > 800? warnings.substring(0, 800) + "..." : warnings.toString(), "Warning", JOptionPane.INFORMATION_MESSAGE);
			                        } else if (showExeptions && exp[0] != null && !(exp[0] instanceof CancellationException)) {
			                        	UIUtil.showException(outputView.dialog, "Error", exp[0]);
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
	            	throw exp[0];
	            }
            }
            if (!result[0] && continueOnErrorQuestion != null) {
            	result[0] = JOptionPane.showConfirmDialog(outputView.dialog, continueOnErrorQuestion, "Error", JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION;
            }
            return result[0];
        } catch (Throwable t) {
        	if (t instanceof CancellationException) {
        		CancellationHandler.reset();
        	} else {
        		boolean shown = false;
        		synchronized (UIUtil.class) {
        			shown = exceptionShown[0];
        		}
        		if (!shown) {
        			UIUtil.showException(null, "Error", t);
        		}
        	}
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
