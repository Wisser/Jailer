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
package net.sf.jailer.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.Charset;
import java.sql.SQLException;
import java.util.Locale;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.Semaphore;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.Pattern;
import java.util.zip.GZIPInputStream;
import java.util.zip.ZipInputStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.database.Session;
import net.sf.jailer.database.SqlException;

/**
 * Reads in and executes SQL-scripts.
 * 
 * @author Ralf Wisser
 */
public class SqlScriptExecutor {

	/**
	 * Comment prefix for multi-line comments.
	 */
	public static final String UNFINISHED_MULTILINE_COMMENT = "--+";

	/**
	 * Comment prefix for last line of a multi-line comment.
	 */
	public static final String FINISHED_MULTILINE_COMMENT = "--.";
	
	/**
	 * The logger.
	 */
	private static final Logger _log = LoggerFactory.getLogger(SqlScriptExecutor.class);

	/**
	 * The session.
	 */
	private final Session session;
	
	/**
	 * Executes the statements.
	 */
	private BoundedExecutor executor;
	
	/**
	 * Threads number of threads to use.
	 */
	private final int threads;
	
	/**
	 * Log statements?
	 */
	private final boolean logStatements;

	private RuntimeException exception;
	
	/**
	 * Constructor.
	 * 
	 * @param session for execution of statements
	 * @param threads number of threads to use
	 * @param logStatements log statements?
	 */
	public SqlScriptExecutor(Session session, int threads, boolean logStatements) {
		this.session = session;
		this.threads = threads;
		this.logStatements = logStatements;
	}

	/**
	 * Constructor.
	 * 
	 * @param session for execution of statements
	 * @param threads number of threads to use
	 */
	public SqlScriptExecutor(Session session, int threads) {
		this(session, threads, true);
	}

	/**
	 * Reads in and executes a SQL-script.
	 * 
	 * @param scriptFileName the name of the script-file
	 * 
	 * @return Pair(statementCount, rowCount)
	 */
	public Pair<Integer, Long> executeScript(String scriptFileName, boolean transactional) throws IOException, SQLException {
		if (!transactional) {
			return executeScript(scriptFileName);
		}
		try {
			Pair<Integer, Long> r = executeScript(scriptFileName);
			session.commitAll();
			return r;
		} catch (IOException e) {
			session.rollbackAll();
			throw e;
		} catch (SQLException e) {
			session.rollbackAll();
			throw e;
		} catch (Throwable e) {
			session.rollbackAll();
			throw new RuntimeException(e);
		}
	}
	
	private static class BoundedExecutor {
		private final ExecutorService exec;
		private final Semaphore semaphore;

		public BoundedExecutor(ExecutorService exec, int bound) {
			this.exec = exec;
			this.semaphore = new Semaphore(bound);
		}

		public void submitTask(final Runnable command)
				throws RejectedExecutionException {
			try {
				semaphore.acquire();
			} catch (InterruptedException e) {
				throw new RuntimeException(e);
			}
			try {
				exec.execute(new Runnable() {
					@Override
					public void run() {
						try {
							command.run();
						} finally {
							semaphore.release();
						}
					}
				});
			} catch (RejectedExecutionException e) {
				semaphore.release();
				throw e;
			}
		}

		public void shutdown() {
			exec.shutdown();
			try {
				exec.awaitTermination(1, TimeUnit.MINUTES);
			} catch (InterruptedException e) {
				// ignore
			}
		}
	}
	
	private long submittedTasks;
	private AtomicLong executedTasks;
	
	/**
	 * Reads in and executes a SQL-script.
	 * 
	 * @param scriptFileName the name of the script-file
	 * 
	 * @return Pair(statementCount, rowCount)
	 */
	public Pair<Integer, Long> executeScript(String scriptFileName) throws IOException, SQLException {
		_log.info("reading file '" + scriptFileName + "'");
		BufferedReader bufferedReader;
		long fileSize = 0;
		final long[] bytesRead = new long[1];
		Charset encoding = SqlUtil.retrieveEncoding(scriptFileName);
		
		File file = new File(scriptFileName);
		InputStream inputStream = new FileInputStream(file);
		inputStream = new FilterInputStream(inputStream) { // lgtm [java/input-resource-leak]
			@Override
			public int read() throws IOException {
				int result = in.read();
				if (result != -1) {
					bytesRead[0]++;
				}
				return result;
			}
			@Override
			public int read(byte[] b, int off, int len) throws IOException {
				int result = in.read(b, off, len);
				if (result != -1) {
					bytesRead[0] += result;
				}
				return result;
			}
		};
		bytesRead[0] = 0;
		fileSize = file.length();
		if (scriptFileName.toLowerCase(Locale.ENGLISH).endsWith(".gz")) {
			bufferedReader = new BufferedReader(new InputStreamReader(new GZIPInputStream(inputStream), encoding));
		} else if (scriptFileName.toLowerCase(Locale.ENGLISH).endsWith(".zip")){
			ZipInputStream zis = new ZipInputStream(inputStream); // lgtm [java/input-resource-leak]
			zis.getNextEntry();
			bufferedReader = new BufferedReader(new InputStreamReader(zis, encoding));
		} else {
			bufferedReader = new BufferedReader(new InputStreamReader(inputStream, encoding));
		}
		
		String line = null;
		StringBuffer currentStatement = new StringBuffer();
		final AtomicLong linesRead = new AtomicLong(0);
		final AtomicLong totalRowCount = new AtomicLong(0);
		final AtomicLong t = new AtomicLong(System.currentTimeMillis());
		final AtomicInteger count = new AtomicInteger(0);
		submittedTasks = 0;
		executedTasks = new AtomicLong(0);
		final long finalFileSize = fileSize;
		LineReader lineReader = new LineReader(bufferedReader);
		boolean inSync = false;
		synchronized (this) {
			exception = null;
		}
		CancellationHandler.reset(null);
		
		Runnable logProgress = new Runnable() {
			@Override
			public void run() {
				if (System.currentTimeMillis() > t.get() + 1000) {
					t.set(System.currentTimeMillis());
					long p = -1;
					if (finalFileSize > 0) {
						p = (1000 * bytesRead[0]) / finalFileSize;
						if (p > 999) {
							p = 999;
						}
					}
					_log.info(linesRead + " statements" + (p >= 0? " (" + String.format(Locale.ENGLISH, "%1.1f", p / 10.0) + "%)" : ""));
				}
			}
		};

		executor = threads > 1? new BoundedExecutor(
				new ThreadPoolExecutor(threads, threads, 0, TimeUnit.MINUTES, new LinkedBlockingQueue<Runnable>()), threads + 3) : null; 
		try {
			final Pattern IDENTITY_INSERT = Pattern.compile(".*SET\\s+IDENTITY_INSERT.*", Pattern.CASE_INSENSITIVE);
			boolean tryMode = false;
			int lineNumber = 0;
			int currentStatementlineNumber = 1;
			while ((line = lineReader.readLine()) != null) {
				++lineNumber;
				line = line.trim();
				if (line.length() == 0) {
					if (currentStatement.length() == 0) {
						++currentStatementlineNumber;
					}
					continue;
				}
				if (line.startsWith("--")) {
					if (currentStatement.length() == 0) {
						++currentStatementlineNumber;
					}
					final String TRY = "try:";
					String uncommentedLine = line.substring(2).trim();
					if (uncommentedLine.startsWith(TRY)) {
						line = uncommentedLine.substring(TRY.length()).trim();
						tryMode = true;
					} else {
						if (line.startsWith(UNFINISHED_MULTILINE_COMMENT)) {
							String cmd = line.substring(UNFINISHED_MULTILINE_COMMENT.length());
							if (cmd.startsWith("XML")) {
								importSQLXML(cmd.substring(3).trim(), lineReader);
							}
							if (cmd.startsWith("CLOB")) {
								importCLob(cmd.substring(4).trim(), lineReader);
							}
							if (cmd.startsWith("BLOB")) {
								importBLob(cmd.substring(4).trim(), lineReader);
							}
						} else if (uncommentedLine.equals("sync")) {
							inSync = true;
							sync();
						} else if (uncommentedLine.equals("epilog")) {
							inSync = false;
							sync();
						}
						continue;
					}
				}
				if (line.endsWith(";")) {
					currentStatement.append(line.substring(0, line.length() - 1));
					if (IDENTITY_INSERT.matcher(currentStatement).matches()) {
						sync();
						if (executor != null) {
							executor.shutdown();
							executor = null;
						}
					}
					final String stmt = currentStatement.toString();
					final boolean finalTryMode = tryMode;
					final int finalCurrentStatementlineNumber = currentStatementlineNumber;
					execute(new Runnable() {
						@Override
						public void run() {
							boolean startsWithDrop = stmt.trim().toLowerCase(Locale.ENGLISH).startsWith("drop");
							boolean silent = session.getSilent();
							session.setSilent(silent || finalTryMode || startsWithDrop);
							boolean oldLogStatements = session.getLogStatements();
							session.setLogStatements(logStatements);
							try {
								if (stmt.trim().length() > 0) {
									boolean done = false;
									long rc = 0;
									if (startsWithDrop) {
										// [bugs:#37] PostreSQL: transactional execution
										String withExists = stmt.replaceFirst("(?is)(DROP\\s+TABLE\\s+)", "$1IF EXISTS ");
										if (!withExists.equals(stmt)) {
											try {
												rc = session.execute(withExists);
											} catch (SQLException e) {
												rc = session.execute(stmt);
											}
											done = true;
										}
									}
									if (!done) {
										rc = session.execute(stmt, null, true);
									}
									totalRowCount.addAndGet(rc);
									linesRead.getAndIncrement();
									if (!startsWithDrop) {
										count.getAndIncrement();
									}
								}
							} catch (SQLException e) {
								try {
									// [bugs:#37] PostreSQL: transactional execution
									session.getConnection().rollback();
								} catch (SQLException e1) {
									// ignore
								}
								// drop may fail
								if (!finalTryMode && !startsWithDrop) {
									// fix for bug [2946477]
									if (!stmt.trim().toUpperCase(Locale.ENGLISH).contains("DROP TABLE JAILER_DUAL")) {
										Session._log.warn(stmt, e);
										if (e instanceof SqlException) {
											((SqlException) e).setInsufficientPrivileges(count.get() == 0);
										}
										if (e instanceof SqlException) {
											String message = (scriptFileName.matches("(tmp|temp).*")? "" : "Error in file: \"" + scriptFileName + "\" line: " + finalCurrentStatementlineNumber + "\n") + ((SqlException) e).getMessage();
											e = new SqlException(message, ((SqlException) e).sqlStatement, e);
										}
										throw new RuntimeException("Error in file: \"" + scriptFileName + "\" line: " + finalCurrentStatementlineNumber, e);
									}
								}
							} finally {
								session.setSilent(silent);
								session.setLogStatements(oldLogStatements);
							}
						}
					}, inSync);
					currentStatement.setLength(0);
					currentStatementlineNumber = lineNumber + 1;
					logProgress.run();
					tryMode = false;
				} else {
					currentStatement.append(line + "\n");
				}
				CancellationHandler.checkForCancellation(null);
				synchronized (this) {
					if (exception != null) {
						if (exception.getCause() instanceof SQLException) {
							throw (SQLException) exception.getCause();
						}
						throw exception;
					}
				}
			}
			bufferedReader.close();
			sync();
			_log.info(linesRead + " statements (100%)");
			_log.info("successfully read file '" + scriptFileName + "'");
			Pair<Integer, Long> r = new Pair<Integer, Long>(count.get(), totalRowCount.get());
			synchronized (SqlScriptExecutor.class) {
				lastRowCount = r;
			}
			return r;
		} catch (Exception e) {
			if (e.getCause() instanceof SQLException) {
				throw (SQLException) e.getCause();
			}
			if (e instanceof RuntimeException) {
				throw (RuntimeException) e;
			}
			throw new RuntimeException(e);
		} finally {
			if (executor != null) {
				executor.shutdown();
			}
			synchronized (this) {
				if (exception != null) {
					if (exception.getCause() instanceof SQLException) {
						throw (SQLException) exception.getCause();
					}
					throw exception;
				}
			}
		}
	}

	private void execute(final Runnable task, boolean inSync) {
		if (!inSync || executor == null) {
			task.run();
		} else {
			++submittedTasks;
			executor.submitTask(new Runnable() {
				@Override
				public void run() {
					try {
						task.run();
					} catch (RuntimeException e) {
						storeException(e);
					} catch (Throwable e) {
						storeException(new RuntimeException(e));
					} finally {
						executedTasks.incrementAndGet();
					}
				}

				private void storeException(RuntimeException runtimeException) {
					synchronized (SqlScriptExecutor.this) {
						if (exception == null) {
							exception = runtimeException;
						}
					}
					CancellationHandler.cancel(null);
				}
			});
		}
	}

	private void sync() {
		if (executor != null) {
			while (submittedTasks > executedTasks.get()) {
				try {
					Thread.sleep(100);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			}
		}
	}

	private static class LineReader {

		private final BufferedReader reader;
		private boolean eofRead = false;
		
		public LineReader(BufferedReader reader) {
			this.reader = reader;
		}

		public String readLine() throws IOException {
			String line = reader.readLine();
			if (line == null && !eofRead) {
				eofRead = true;
				return ";";
			}
			return line;
		}
	}
	
	/**
	 * Imports clob from sql-script.
	 * 
	 * @param clobLocator locates the clob
	 * @param lineReader for reading content
	 */
	private void importCLob(final String clobLocator, final LineReader lineReader) throws IOException, SQLException {
		int c1 = clobLocator.indexOf(',');
		int c2 = clobLocator.indexOf(',', c1 + 1);
		final String table = clobLocator.substring(0, c1).trim();
		final String column = clobLocator.substring(c1 + 1, c2).trim();
		final String where = clobLocator.substring(c2 + 1).trim();
		String line;
		final File lobFile = Configuration.getInstance().createTempFile(); // new File("lob." + System.currentTimeMillis());
		Writer out = new OutputStreamWriter(new FileOutputStream(lobFile), "UTF-8"); // lgtm [java/output-resource-leak] 
		long length = 0;
		while ((line = lineReader.readLine()) != null) {
			if (line.startsWith(UNFINISHED_MULTILINE_COMMENT)) {
				String content = line.substring(UNFINISHED_MULTILINE_COMMENT.length());
				int l = content.length();
				boolean inEscape = false;
				for (int i = 0; i < l; ++i) {
					char c = content.charAt(i);
					if (c == '\\') {
						if (inEscape) {
							inEscape = false;
						} else {
							inEscape = true;
							continue;
						}
					} else {
						if (inEscape) {
							if (c == 'n') {
								c = '\n';
							} else if (c == 'r') {
								c = '\r';
							}
							inEscape = false;
						}
					}
					out.write(c);
					++length;
				}
			} else {
				break;
			}
		}
		out.close();
		sync();
		final long finalLength = length;
		session.insertClob(table, column, where, lobFile, finalLength);
		lobFile.delete();
	}
	
	/**
	 * Imports SQL-XML from sql-script.
	 * 
	 * @param xmlLocator locates the XML column
	 * @param lineReader for reading content
	 */
	private void importSQLXML(final String xmlLocator, final LineReader lineReader) throws IOException, SQLException {
		int c1 = xmlLocator.indexOf(',');
		int c2 = xmlLocator.indexOf(',', c1 + 1);
		final String table = xmlLocator.substring(0, c1).trim();
		final String column = xmlLocator.substring(c1 + 1, c2).trim();
		final String where = xmlLocator.substring(c2 + 1).trim();
		String line;
		final File lobFile = Configuration.getInstance().createTempFile(); // new File("lob." + System.currentTimeMillis());
		Writer out = new OutputStreamWriter(new FileOutputStream(lobFile), "UTF-8"); // lgtm [java/output-resource-leak] 
		long length = 0;
		while ((line = lineReader.readLine()) != null) {
			// line = line.trim();
			if (line.startsWith(UNFINISHED_MULTILINE_COMMENT)) {
				String content = line.substring(UNFINISHED_MULTILINE_COMMENT.length());
				int l = content.length();
				boolean inEscape = false;
				for (int i = 0; i < l; ++i) {
					char c = content.charAt(i);
					if (c == '\\') {
						if (inEscape) {
							inEscape = false;
						} else {
							inEscape = true;
							continue;
						}
					} else {
						if (inEscape) {
							if (c == 'n') {
								c = '\n';
							} else if (c == 'r') {
								c = '\r';
							}
							inEscape = false;
						}
					}
					out.write(c);
					++length;
				}
			} else {
				break;
			}
		}
		out.close();
		final long finalLength = length;
		sync();
		session.insertSQLXML(table, column, where, lobFile, finalLength);
		lobFile.delete();
	}
	
	/**
	 * Imports blob from sql-script.
	 * 
	 * @param clobLocator locates the clob
	 * @param lineReader for reading content
	 */
	private void importBLob(final String clobLocator, final LineReader lineReader) throws IOException, SQLException {
		int c1 = clobLocator.indexOf(',');
		int c2 = clobLocator.indexOf(',', c1 + 1);
		final String table = clobLocator.substring(0, c1).trim();
		final String column = clobLocator.substring(c1 + 1, c2).trim();
		final String where = clobLocator.substring(c2 + 1).trim();
		String line;
		final File lobFile = Configuration.getInstance().createTempFile(); // new File("lob." + System.currentTimeMillis());
		OutputStream out = new FileOutputStream(lobFile);
		while ((line = lineReader.readLine()) != null) {
			line = line.trim();
			if (line.startsWith(UNFINISHED_MULTILINE_COMMENT)) {
				String content = line.substring(UNFINISHED_MULTILINE_COMMENT.length());
				out.write(Base64.decode(content));
			} else {
				break;
			}
		}
		out.close();
		sync();
		session.insertBlob(table, column, where, lobFile);
		lobFile.delete();
	}

	private static Pair<Integer, Long> lastRowCount = null;
	
	public static synchronized Pair<Integer, Long> getLastStatementCount() {
		if (lastRowCount != null) {
			return lastRowCount;
		}
		return new Pair<Integer, Long>(0, 0L);
	}
	
}
