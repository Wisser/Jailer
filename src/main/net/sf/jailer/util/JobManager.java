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
package net.sf.jailer.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import org.apache.log4j.Logger;

/**
 * Executes a job-list in a concurrent way.
 *  
 * @author wisser
 */
public class JobManager {

    /**
     * A job to be managed by a {@link JobManager}.
     */
    public interface Job {
        
        /**
         * Runs the job.
         */
        void run() throws Exception;
    };

    /**
     * The logger.
     */
    private static final Logger _log = Logger.getLogger(JobManager.class);

    /**
     * The runners list.
     */
    private final List<JobRunner> runnersList;
    
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
        public void run() {
            _log.debug("start up");
            while (isRunning()) {
                Job job = nextJob();
                if (job == null) {
                    try {
                        Thread.sleep(500);
                    } catch (InterruptedException e) {
                    }
                } else {
                    try {
                        incrementJobsInExecutionCounter();
                        job.run();
                        incrementJobsDoneCounter();
                    } catch (Exception e) {
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
        runnersList = new ArrayList<JobRunner>(threads);
        for (int i = 0; i < threads; ++i) {
            JobRunner jobRunner = new JobRunner();
            runnersList.add(jobRunner);
            String threadName = "job-runner " + (i + 1);
            _log.debug("starting " + threadName);
            Thread thread = new Thread(jobRunner, threadName);
            thread.setDaemon(true);
            thread.start();
        }
    }

    /**
     * Executes a list of jobs.
     * 
     * @param jobs the job-list
     * @throws Exception exception thrown during job-execution
     */
    public void executeJobs(Collection<Job> jobs) throws Exception {
        int jobCount = jobs.size();
        _log.info("starting " + jobCount + " jobs");
        if (runnersList.size() == 1) {
        	for (Job job: jobs) {
        		job.run();
        	}
        } else {
	        setJobs(new LinkedList<Job>(jobs));
	        while (getJobsDoneCounter() < jobCount || getJobsInExecutionCounter() > 0) {
	            Thread.sleep(100);
	            Exception e = getException();
	            if (e != null) {
	                _log.error("Job-error", e);
	                // wait for other jobs
	                while (getJobsInExecutionCounter() > 0) {
	                    Thread.sleep(500);
	                }
	                throw e;
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
            String threadName = "job-runner " + (i + 1);
            _log.debug("shutting down " + threadName);
            runnersList.get(i).shutdown();
        }
    }
    
    /**
     * Number of executed jobs.
     */
    private int jobsDoneCounter;
    
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
     * Increments the Number of executed jobs.
     */
    private synchronized void incrementJobsDoneCounter() {
        ++jobsDoneCounter;
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

    /**index
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
    private synchronized void setException(Exception e) {
        exception = e == null? null : new RuntimeException(Thread.currentThread().getName() + " failed", e);
        jobs = null;
    }
    
}
