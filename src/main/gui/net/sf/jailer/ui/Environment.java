/*
 * Copyright 2007 - 2020 Ralf Wisser.
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

import java.awt.AWTEvent;
import java.awt.EventQueue;
import java.awt.Toolkit;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Locale;
import java.util.Random;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.render.HtmlDataModelRenderer;
import net.sf.jailer.ui.util.AWTWatchdog;
import net.sf.jailer.util.LogUtil;

/**
 * Sets up environment.
 * 
 * @author Ralf Wisser
 */
public class Environment {

	private static File home = null;

	public static void init() {
		initUI();
		String osName = System.getProperty("os.name");
		if (osName != null) {
			if (osName.toLowerCase(Locale.ENGLISH).contains("mac os")) {
				// https://github.com/AdoptOpenJDK/openjdk-jdk11/issues/10
				// https://bugs.java.com/bugdatabase/view_bug.do?bug_id=8215200
				// https://bugs.openjdk.java.net/browse/JDK-8215200
				System.setProperty("java.util.Arrays.useLegacyMergeSort", "true");
			}
		}
		if (new File(".singleuser").exists() // legacy
				|| new File(".multiuser").exists()
				|| isJPacked()) {
			home = new File(System.getProperty("user.home"), ".jailer");
			home.mkdirs();
			LogUtil.reloadLog4jConfig(home);
			Configuration configuration = Configuration.getInstance();
			try {
				copyIfNotExists("datamodel");
				copyIfNotExists("bookmark");
				copyIfNotExists("extractionmodel");
				copyIfNotExists("layout");
				copyIfNotExists("demo-scott-1.4.mv.db");
				copyIfNotExists("demo-sakila-1.4.mv.db");
				copyIfNotExists("demo-scott-subset-1.4.mv.db");
				copyIfNotExists("example");
				copyIfNotExists("render");

				configuration.setTempFileFolder(newFile("tmp").getPath());
				HtmlDataModelRenderer renderer = configuration.getRenderer();
				if (renderer != null) {
					renderer.setOutputFolder(newFile(renderer.getOutputFolder()).getAbsolutePath());
				}
				ExecutionContext.defaultDatamodelFolder = newFile(ExecutionContext.defaultDatamodelFolder).getAbsolutePath();
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		} else {
			if (!testCreateTempFile()) {
				UIUtil.showException(null, "Error", new IllegalStateException("No write permission on "
						+ new File(".").getAbsolutePath() + " \n"
						+ "To setup multi-user mode, create a (empty) file named \".multiuser\" in this folder. "
						+ "All model and settings files are then stored in a folder named \".jailer\" in the user's home directory."));
				System.exit(-1);
			}
		}
		int stateOffset = 100;
		state = (new File(".singleuser").exists() ? 1 : 0) // legacy
				+ (new File(".multiuser").exists() ? 2 : 0) + (new File("..", "dbeauty").exists() ? 4 : 0)
				+ (isJPacked() ? 1000 : 0)
				+ (!testCreateTempFile() ? 8 : 0)
				+ stateOffset;
		AWTWatchdog.start();
		LogUtil.setWarn(new LogUtil.Warn() {
			@Override
			public void warn(Exception e) {
				StringWriter sw = new StringWriter();
		        PrintWriter pw = new PrintWriter(sw);
		        e.printStackTrace(pw);
		        UIUtil.sendIssue("warn", sw.toString().replaceAll("at (.*)?\\.((\\w|\\$)+\\.(\\w|\\$)+\\()", "$2"));
		        LogUtil.setWarn(null);
			}
		});
	}

	// TODO move to engine
	private static File applicationBase = null;

	// TODO init, lib/ in app/ or null
	private static File libFolder = null;

	// TODO init applicationBase
	// TODO use newWorkingFolderFile for jailer.xml, driver.csv, template, script, render, local db jdbc jar
	// TODO HttpUtil, FileBasedScriptEnhancer

	// TODO jdbc jars in lib: rename prior packing, copy into user dir, adjust file name in connection configs (!exists(f) && exists(~/lib/f) -> f =~ /lib/f)
	// TODO default dir for *.jar in FileCh. is ~/lib
	
	// TODO config: test, does this work in DBrowser? -> LogUtil.class.getResourceAsStream("/log4j.properties")
	
	// TODO no bat/exe/sh in app/

	public static File newWorkingFolderFile(String name) {
		if (applicationBase == null || new File(name).isAbsolute()) {
			return new File(name);
		}
		return new File(applicationBase, name);
	}

	private static boolean isJPacked() {
		return applicationBase != null;
	}

	private static void initUI() {
		try {
			EventQueue queue = Toolkit.getDefaultToolkit().getSystemEventQueue();
			queue.push(new JEventQueue());
		} catch (Throwable t) {
			// ignore
		}
	}

	public static class JEventQueue extends EventQueue {
		int activeCD = 5;

		@Override
		protected void dispatchEvent(AWTEvent newEvent) {
		    try {
		        super.dispatchEvent(newEvent);
		    } catch (Throwable t) {
		        if (activeCD > 0) {
		        	--activeCD;
		        	try {
		        		UIUtil.showException(null, "Error", t, "AWT");
				    } catch (Throwable t2) {
				    	UIUtil.showException(null, "Error", t2, "AWT2");
				    }
		        } else {
		        	throw t;
		        }
		    }
		}
	}

	private static boolean copyIfNotExists(String f) throws IOException {
		File sFile = new File(f);
		File dFile = new File(home, f);

		if (dFile.exists() || !sFile.exists()) {
			return false;
		}

		Path sourcePath = sFile.toPath();
		Path targetPath = dFile.toPath();
		Files.walkFileTree(sourcePath, new CopyFileVisitor(targetPath));
		return true;
	}

	static class CopyFileVisitor extends SimpleFileVisitor<Path> {
		private final Path targetPath;
		private Path sourcePath = null;

		public CopyFileVisitor(Path targetPath) {
			this.targetPath = targetPath;
		}

		@Override
		public FileVisitResult preVisitDirectory(final Path dir, final BasicFileAttributes attrs) throws IOException {
			if (sourcePath == null) {
				sourcePath = dir;
			}
			Files.createDirectories(targetPath.resolve(sourcePath.relativize(dir)));
			return FileVisitResult.CONTINUE;
		}

		@Override
		public FileVisitResult visitFile(final Path file, final BasicFileAttributes attrs) throws IOException {
			try {
				Files.copy(file, sourcePath == null ? targetPath : targetPath.resolve(sourcePath.relativize(file)));
			} catch (Exception e) {
				// ignore
			}
			return FileVisitResult.CONTINUE;
		}
	}

	public static File newFile(String name) {
		if (home == null || new File(name).isAbsolute()) {
			return new File(name);
		}
		return new File(home, name);
	}

	public static boolean testCreateTempFile() {
		try {
			File tempFile = new File("tp" + new Random().nextInt(100000));
			FileOutputStream out = new FileOutputStream(tempFile);
			out.write(0);
			out.close();
			tempFile.delete();
		} catch (Exception e) {
			return false;
		}
		return true;
	}

	public static int state;

	public static boolean nimbus = false;

}
