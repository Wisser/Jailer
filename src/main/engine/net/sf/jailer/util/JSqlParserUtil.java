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

import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Supplier;

import net.sf.jsqlparser.JSQLParserException;
import net.sf.jsqlparser.parser.CCJSqlParser;
import net.sf.jsqlparser.parser.ParseException;
import net.sf.jsqlparser.parser.StringProvider;
import net.sf.jsqlparser.statement.Statement;

/**
 * SQL parsing.
 *
 * @see https://github.com/JSQLParser/JSqlParser
 */
public final class JSqlParserUtil {

	/**
	 * Parses a SQL statement.
	 *
	 * @param sql the statement
	 * @param timeoutSec timeout in seconds
	 * @return parsed statement
	 *
	 * @throws JSQLParserException if JSQLParser is not able to parse the statement
	 */
    public static Statement parse(String sql, int timeoutSec) throws JSQLParserException {
    	CCJSqlParser parser = new CCJSqlParser(new StringProvider(sql)).withSquareBracketQuotation(false);
		try {
		    return parse(parser, sql, timeoutSec);
		} catch (Exception e) {
			if (sql.contains("[")) {
				parser = new CCJSqlParser(new StringProvider(sql)).withSquareBracketQuotation(true);
				try {
				    return parse(parser, sql, timeoutSec);
				} catch (Exception e2) {
				    // ignore
				}
			}
		    throw new JSQLParserException(e);
		} catch (Throwable t) {
			if (t instanceof StackOverflowError) {
				LogUtil.warn(new StackOverflowError(JSqlParserUtil.class.getName() + ": " + sql.length()));
			} else {
				LogUtil.warn(t);
			}
			throw new RuntimeException("ParseErr:" + sql + " " + t.getMessage(), t);
		}
    }

    private static final int MAX_ENTRIES = 1000;
    @SuppressWarnings("serial")
	private static Map<String, String> timedOut = new LinkedHashMap<String, String>(MAX_ENTRIES + 1, .75F, true) {
        // This method is called just after a new entry has been added
        public boolean removeEldestEntry(Map.Entry<String, String> eldest) {
            return size() > MAX_ENTRIES;
        }
    };
    private static Method stop;
    static {
    	try {
			stop = Thread.class.getMethod("stop");
		} catch (NoSuchMethodException | SecurityException e) {
			stop = null;
		}
    }
    private static LinkedBlockingQueue<CCJSqlParser> statementQueue;
    private static Thread parseThread;
    private static AtomicReference<Object> result;
    private static int n = 1;
    private static CCJSqlParser nullParser = new CCJSqlParser(new StringProvider(""));
    private static Supplier<Long> currentTime = System::currentTimeMillis;
    static {
    	try {
	    	ThreadMXBean threadMXBean = ManagementFactory.getThreadMXBean();
	    	if (threadMXBean != null && threadMXBean.isThreadCpuTimeSupported()) {
	    		if (threadMXBean.isThreadCpuTimeEnabled()) {
	    			currentTime = () -> {
	    				long cpuTime = threadMXBean.getThreadCpuTime(parseThread.getId());
	    				if (cpuTime < 0) {
	    					return System.currentTimeMillis();
	    				}
	    				return cpuTime / 1_000_000;
	    			};
	    		}
	    	}
    	} catch (Throwable t) {
    		// ignore
    	}
    }

    private static final int MAX_CACHEDENTRIES = 20;
    @SuppressWarnings("serial")
	private static Map<String, Statement> cache = new LinkedHashMap<String, Statement>(MAX_CACHEDENTRIES + 1, .75F, true) {
        // This method is called just after a new entry has been added
        public boolean removeEldestEntry(Map.Entry<String, Statement> eldest) {
            return size() > MAX_CACHEDENTRIES;
        }
    };
    
    /**
     * Workaround for https://github.com/JSQLParser/JSqlParser/issues/1013
     *
	 * @see https://github.com/JSQLParser/JSqlParser/issues/1013
     */
    private static synchronized Statement parse(CCJSqlParser parser, String sql, int timeoutSec) throws ParseException {
    	Statement cachedStatement = cache.get(sql);
    	if (cachedStatement != null) {
    		return cachedStatement;
    	}
		if (stop == null) {
			return parser.Statement();
		}
		if (timedOut.containsKey(sql)) {
			timedOut.put(sql, sql);
			ParseException timeoutException = new ParseException("Timeout (" + timeoutSec + "): " + sql);
			throw timeoutException;
		}
		if (parseThread == null) {
			LinkedBlockingQueue<CCJSqlParser> queue = statementQueue = new LinkedBlockingQueue<CCJSqlParser>();
			AtomicReference<Object> res = result = new AtomicReference<Object>();
			parseThread = new Thread(() -> {
				for (;;) {
					try {
						CCJSqlParser p = queue.take();
						if (p == nullParser) {
							break;
						}
						res.set(p.Statement());
					} catch (Throwable t) {
						res.set(t);
					}
				}
			}, "SQLParser-" + (n++));
			parseThread.setDaemon(true);
			parseThread.start();
		}
		result.set(null);
		statementQueue.add(parser);
		long time = currentTime.get();
		Object r = null;
		for (int i = 0; ; ++i) {
			r = result.get();
			if (r != null) {
				result.set(null);
				break;
			}
			long dt = currentTime.get() - time;
			if (dt > timeoutSec * 1000) {
				break;
			}
			try {
				Thread.sleep(i < 10? 1 : 10);
			} catch (InterruptedException e) {
				// ignore
			}
		}
		if (r instanceof ParseException) {
			throw (ParseException) r;
		} else if (r instanceof Throwable) {
			throw new RuntimeException((Throwable) r);
		} else if (r instanceof Statement) {
			cache.put(sql, (Statement) r);
			return (Statement) r;
		} else {
			statementQueue.add(nullParser);
			try {
				stop.invoke(parseThread);
			} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
				// ignore
			}
			statementQueue = null;
			parseThread = null;
			result = null;
			ParseException timeoutException = new ParseException("Timeout (" + timeoutSec + " sec): " + sql);
			LogUtil.warn(timeoutException);
			timedOut.put(sql, sql);
			throw timeoutException;
		}
	}
}
