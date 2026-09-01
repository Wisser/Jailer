/*
 * Copyright 2007 - 2026 Ralf Wisser.
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
package net.sf.jailer.ui.progress;

import java.util.concurrent.Callable;


import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.BasicDataSource;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.RowIdSupport;
import net.sf.jailer.entitygraph.RowOriginFinder;
import net.sf.jailer.entitygraph.remote.RemoteEntityGraph;
import net.sf.jailer.util.LogUtil;

/**
 * Everything that is needed to ask where a row of the subset comes from: the connection, the
 * data model and the ID of the entity-graph that has been kept after the run.
 * <p>
 * Deliberately independent of the progress window, so that the same analysis can later be
 * started from elsewhere, for instance from the Data Browser. A row is always addressed by its
 * primary key values, never by a position in a result.
 *
 * @author Ralf Wisser
 */
public class RowOriginContext {

	private final DataModel dataModel;
	private final ExecutionContext executionContext;
	private final Callable<BasicDataSource> dataSourceFactory;
	private final DBMS dbms;
	private final String dbUrl;

	private int graphId = -1;
	private Session session;
	private RemoteEntityGraph entityGraph;
	private boolean discarded = false;

	/**
	 * Constructor.
	 *
	 * @param dataModel the data model the graph has been collected with
	 * @param executionContext holds the working table scope, the working table schema and the
	 *        row identification settings of the run
	 * @param dataSourceFactory creates a data source for the database of the run, on demand
	 * @param dbms the DBMS of that database, or <code>null</code> to take it from the data source
	 * @param dbUrl URL of that database, used to remember the retained graph
	 */
	public RowOriginContext(DataModel dataModel, ExecutionContext executionContext, Callable<BasicDataSource> dataSourceFactory, DBMS dbms, String dbUrl) {
		this.dataModel = dataModel;
		this.executionContext = executionContext;
		this.dataSourceFactory = dataSourceFactory;
		this.dbms = dbms;
		this.dbUrl = dbUrl;
	}

	/**
	 * Gets the URL of the database the graph lives in.
	 *
	 * @return the URL
	 */
	public String getDbUrl() {
		return dbUrl;
	}

	/**
	 * Sets the ID of the entity-graph that has been kept. Before that, nothing can be analyzed.
	 *
	 * @param graphId the ID of the retained entity-graph
	 */
	public synchronized void setGraphId(int graphId) {
		if (this.graphId != graphId) {
			this.graphId = graphId;
			// the attached graph belongs to the id it was attached with, see getEntityGraph.
			// The session is kept: it does not depend on the graph, the next attach reuses it.
			entityGraph = null;
		}
	}

	/**
	 * Gets the ID of the retained entity-graph.
	 *
	 * @return the ID, or -1 if no graph has been retained
	 */
	public synchronized int getGraphId() {
		return graphId;
	}

	/**
	 * Returns whether an analysis is possible, that is whether a graph has been retained and
	 * has not been discarded yet.
	 *
	 * @return <code>true</code> if the origin of a row can be analyzed
	 */
	public synchronized boolean isAvailable() {
		return graphId >= 0 && !discarded;
	}

	/**
	 * Gets the data model.
	 *
	 * @return the data model
	 */
	public DataModel getDataModel() {
		return dataModel;
	}

	/**
	 * Gets the retained entity-graph, connecting to the database on first use.
	 *
	 * @return the entity-graph
	 */
	public synchronized RemoteEntityGraph getEntityGraph() throws Exception {
		if (!isAvailable()) {
			throw new IllegalStateException("no entity-graph has been retained");
		}
		if (entityGraph == null) {
			BasicDataSource dataSource = dataSourceFactory.call();
			DBMS theDbms = dbms != null? dbms : dataSource.dbms;
			session = new Session(dataSource, theDbms, executionContext.getIsolationLevel());
			entityGraph = RemoteEntityGraph.attach(dataModel, graphId, session,
					new RowIdSupport(dataModel, theDbms, executionContext).getUniversalPrimaryKey(session), executionContext);
		}
		return entityGraph;
	}

	/**
	 * Creates a finder for the retained entity-graph.
	 *
	 * @return the finder
	 */
	public RowOriginFinder createFinder() throws Exception {
		return new RowOriginFinder(getEntityGraph(), dataModel);
	}

	/**
	 * Gets the row identification of the run, which tells what the primary key of a table is.
	 *
	 * @return the row identification
	 */
	public synchronized RowIdSupport getRowIdSupport() {
		if (rowIdSupport == null) {
			rowIdSupport = new RowIdSupport(dataModel, dbms, executionContext);
		}
		return rowIdSupport;
	}

	private RowIdSupport rowIdSupport;

	/**
	 * Deletes the retained entity-graph and closes the connection. Does nothing if there is
	 * nothing to discard. May take a while, so it is not to be called on the event dispatch
	 * thread.
	 */
	public synchronized void discard() throws Exception {
		if (graphId < 0 || discarded) {
			closeSession();
			return;
		}
		try {
			getEntityGraph().delete();
			session.commitAll();
		} finally {
			discarded = true;
			closeSession();
		}
	}

	/**
	 * Closes the connection, but keeps the graph.
	 */
	public synchronized void closeSession() {
		entityGraph = null;
		if (session != null) {
			try {
				session.shutDown();
			} catch (Exception e) {
				LogUtil.warn(e);
			}
			session = null;
		}
	}

}
