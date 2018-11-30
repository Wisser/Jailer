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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Enumeration;
import java.util.Locale;

import org.apache.log4j.Appender;
import org.apache.log4j.FileAppender;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.render.HtmlDataModelRenderer;

/**
 * @author Ralf Wisser
 */
public class Environment {

	private static File home = null;
	public static Locale initialLocal = Locale.ENGLISH;
	
	public static void init() {
		initialLocal = Locale.getDefault();
		Locale.setDefault(Locale.ENGLISH);
		Configuration configuration = Configuration.getInstance();
		if (new File(".singleuser").exists() || unableToCreateTempFile(configuration)) {
			home = new File(System.getProperty("user.home"), ".jailer");
			home.mkdirs();
			try {
				copyIfNotExists("datamodel");
				copyIfNotExists("extractionmodel");
				copyIfNotExists("layout");
				copyIfNotExists(".cdsettings");
				copyIfNotExists(".exportdata.ui");
				copyIfNotExists(".selecteddatamodel");
				copyIfNotExists("demo-scott.h2.db");
				copyIfNotExists("demo-sakila.h2.db");

				configuration.setTempFileFolder(newFile("tmp").getPath());
				HtmlDataModelRenderer renderer = configuration.getRenderer();
				if (renderer != null) {
					renderer.setOutputFolder(newFile(renderer.getOutputFolder()).getAbsolutePath());
				}
				
				Enumeration e = LogManager.getCurrentLoggers();
				while (e.hasMoreElements()) {
					Logger logger = (Logger) e.nextElement();
					Enumeration a = logger.getAllAppenders();
					while (a.hasMoreElements()) {
						Appender appender = (Appender) a.nextElement();
						if (appender instanceof FileAppender) {
		                    FileAppender fileAppender = (FileAppender) appender;

		                    String logfileName = fileAppender.getFile();

		                    if (logfileName != null && logfileName.length() > 0) {
		                        File logfile = new File(logfileName);
		                        if (logfile.isAbsolute() == false) {
		                            fileAppender.setFile(Environment.newFile(logfileName).getPath());
		                            fileAppender.activateOptions();
		                        }
		                    }
						}
					}
				}
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
	}

	private static boolean unableToCreateTempFile(Configuration configuration) {
		try {
			File tempFile = configuration.createTempFile();
			FileOutputStream out = new FileOutputStream(tempFile);
			out.write(0);
			out.close();
			tempFile.delete();
		} catch (FileNotFoundException e) {
			return true;
		} catch (IOException e) {
			return true;
		}
		return false;
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
	    public FileVisitResult preVisitDirectory(final Path dir,
	    final BasicFileAttributes attrs) throws IOException {
	        if (sourcePath == null) {
	            sourcePath = dir;
	        }
	        Files.createDirectories(targetPath.resolve(sourcePath
	                    .relativize(dir)));
	        return FileVisitResult.CONTINUE;
	    }

	    @Override
	    public FileVisitResult visitFile(final Path file,
	    final BasicFileAttributes attrs) throws IOException {
	    Files.copy(file,
	    		sourcePath == null? targetPath : targetPath.resolve(sourcePath.relativize(file)));
	    return FileVisitResult.CONTINUE;
	    }
	}

	public static File newFile(String name) {
		if (home == null || new File(name).isAbsolute()) {
			return new File(name);
		}
		return new File(home, name);
	}

}
