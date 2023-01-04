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

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Executes a job-list in a concurrent way.
 *  
 * @author Ralf Wisser
 */
public abstract class JobManager {

	/**
	 * A job to be managed by a {@link JobManager}.
	 */
	public interface Job {
		
		/**
		 * Runs the job.
		 */
		void run() throws SQLException, CancellationException;
	}

	/**
	 * The logger.
	 */
	private static final Logger _log = LoggerFactory.getLogger(JobManager.class);

	/**
	 * The runners list.
	 */
	private final List<JobRunner> runnersList;
	
	/**
	 * Maximum number of threads.
	 */
	private final int threads;
	
	/**
	 * Thread for executing jobs.
	 */
	private class JobRunner implements Runnable {

		/**
		 * For shutting down the runner.
		 */
		private boolean isRunning = true;
		
		/**
		 * Shuts the runner down.
		 */
		public synchronized void shutdown() {
			isRunning = false;
		}

		/**
		 * Still running?
		 */
		public synchronized boolean isRunning() {
			return isRunning;
		}
		
		/**
		 * Executes jobs.
		 */
		@Override
		public void run() {
			_log.debug("start up");
			while (isRunning()) {
				Job job = nextJob();
				if (job == null) {
					try {
						Thread.sleep(100);
					} catch (InterruptedException e) {
					}
				} else {
					try {
						incrementJobsInExecutionCounter();
						job.run();
						incrementJobsDoneCounter();
					} catch (Throwable e) {
						setException(e);
					} finally {
						decrementJobsInExecutionCounter();
					}
				}
			}
			_log.debug("shutting down");
		}
	}
	
	/**
	 * Constructor.
	 * 
	 * @param threads number of threads
	 */
	public JobManager(int threads) {
		this.threads = threads;
		runnersList = new ArrayList<JobRunner>(threads);
	}
	
	private void ensureThreadCapacity(int capacity) {
		if (threads > 1) {
			while (runnersList.size() < Math.min(capacity, threads)) {
				JobRunner jobRunner = new JobRunner();
				runnersList.add(jobRunner);
				String threadName = "job- " + runnersList.size();
				_log.debug("starting " + threadName);
				Thread thread = new Thread(jobRunner, threadName);
				thread.setDaemon(true);
				thread.start();
			}
		}
	}

	/**
	 * Executes a list of jobs.
	 * 
	 * @param jobs the job-list
	 */
	public void executeJobs(Collection<Job> jobs) throws CancellationException, SQLException {
		int jobCount = jobs.size();
		ensureThreadCapacity(jobCount);
		_log.info("starting " + jobCount + " jobs");
		if (runnersList.isEmpty()) {
			for (Job job: jobs) {
				job.run();
			}
		} else {
			setJobs(new LinkedList<Job>(jobs));
			while (getJobsDoneCounter() < jobCount || getJobsInExecutionCounter() > 0) {
				try {
					Thread.sleep(100);
				} catch (InterruptedException e1) {
					throw new RuntimeException(e1);
				}
				Exception e = getException();
				if (e != null) {
					if (!(e instanceof CancellationException)) {
						_log.error("Job-error", e);
					}
					// wait for other jobs
					while (getJobsInExecutionCounter() > 0) {
						try {
							Thread.sleep(50);
						} catch (InterruptedException e1) {
							throw new RuntimeException(e1);
						}
					}
					if (e instanceof CancellationException) {
						throw (CancellationException) e;
					}
					if (e instanceof SQLException) {
						throw (SQLException) e;
					}
					throw new RuntimeException(e);
				}
			}
		}
		_log.info("executed " + jobCount + " jobs");
	}
	
	/**
	 * Shuts the manager down.
	 */
	public void shutdown() {
		for (int i = 0; i < runnersList.size(); ++i) {
			String threadName = "job- " + (i + 1);
			_log.debug("shutting down " + threadName);
			runnersList.get(i).shutdown();
		}
	}
	
	/**
	 * Number of executed jobs.
	 */
	private int jobsDoneCounter;
	
	/**
	 * Number of jobs waiting for primary cause.
	 */
	private int jobsWaitingForPrimaryCauseCounter;
	
	/**
	 * Number of jobs currently executed.
	 */
	private int jobsInExecutionCounter;
	
	/**
	 * The job-list.
	 */
	private List<Job> jobs;

	/**
	 * Exception during job-execution.
	 */
	private Exception exception;

	/**
	 * Sets the job-list.
	 * Resets the job-counter to 0.
	 */
	private synchronized void setJobs(List<Job> jobs) {
		jobsDoneCounter = 0;
		jobsWaitingForPrimaryCauseCounter = 0;
		jobsInExecutionCounter = 0;
		this.jobs = jobs;
		exception = null;
	}

	/**
	 * Gets the Number of executed jobs.
	 */
	private synchronized int getJobsDoneCounter() {
		return jobsDoneCounter;
	}

	/**
	 * Gets the Number of jobs waiting for primary cause.
	 */
	private synchronized int getJobsWaitingForPrimaryCauseCounter() {
		return jobsWaitingForPrimaryCauseCounter;
	}

	/**
	 * Increments the Number of executed jobs.
	 */
	private synchronized void incrementJobsDoneCounter() {
		++jobsDoneCounter;
	}

	/**
	 * Increments the Number of jobs Waiting for primary cause.
	 */
	private synchronized void incrementJobsWaitingForPrimaryCauseCounter() {
		++jobsWaitingForPrimaryCauseCounter;
	}

	/**
	 * Decrements the Number of jobs Waiting for primary cause.
	 */
	private synchronized void decrementJobsWaitingForPrimaryCauseCounter() {
		--jobsWaitingForPrimaryCauseCounter;
	}

	/**
	 * Gets the Number of jobs currently executed.
	 */
	private synchronized int getJobsInExecutionCounter() {
		return jobsInExecutionCounter;
	}

	/**
	 * Increments the Number of jobs currently executed.
	 */
	private synchronized void incrementJobsInExecutionCounter() {
		++jobsInExecutionCounter;
	}

	/**
	 * Decrements the Number of jobs currently executed.
	 */
	private synchronized void decrementJobsInExecutionCounter() {
		--jobsInExecutionCounter;
	}

	/**
	 * Gets the exception.
	 */
	private synchronized Exception getException() {
		return exception;
	}
	
	/**
	 * Gets next job.
	 */
	private synchronized Job nextJob() {
		if (jobs != null && !jobs.isEmpty()) {
			return jobs.remove(0);
		}
		return null;
	}
	
	/**
	 * Sets an exception.
	 */
	private void setException(Throwable e) {
		if (isPotentiallyConsequentialError(e)) {
			synchronized (this) {
				jobs = null;
			}
			// wait for primary error
			// wait for other jobs
			incrementJobsWaitingForPrimaryCauseCounter();
			int i = 0;
			while (getJobsInExecutionCounter() - getJobsWaitingForPrimaryCauseCounter() > 0) {
				try {
					Thread.sleep(50);
				} catch (InterruptedException e1) {
					throw new RuntimeException(e1);
				}
				++i;
				if (getException() != null) {
					break;
				}
				if (i > (1000 * 20) / 50 /* 20 sec */) {
					LogUtil.warn(new RuntimeException("No prim. cause. " + getJobsInExecutionCounter() + " " + getJobsWaitingForPrimaryCauseCounter()));
					break;
				}
			}
			decrementJobsWaitingForPrimaryCauseCounter();
		}
		synchronized (this) {
			if (exception == null) {
				exception = e == null? null : (e instanceof CancellationException || e instanceof SQLException)? (Exception) e 
					: new RuntimeException(Thread.currentThread().getName() + " failed", e);
				if (e != null && !(e instanceof CancellationException)) {
					onException(e);
				}
			}
			jobs = null;
		}
	}

	private synchronized boolean isPotentiallyConsequentialError(Throwable e) {
		if (exception == null) {
			if (e instanceof SQLException) {
				if ("25P02".equals(((SQLException)e).getSQLState())) {
					return true;
				}
			}
		}
		return false;
	}

	protected abstract void onException(Throwable t);

}
